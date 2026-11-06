#!/usr/bin/env Rscript
# CI guard: the Posit Connect manifest at the repository root can deploy the
# app in this repository.
#
# Connect deploys the repository ROOT. The root app.R installs DTAtools from
# that same checkout and serves the app bundled inside it, so the app and the
# package always come from one commit. For that to work the manifest must:
#
#   - NOT list DTAtools. It used to, as a GitHub dependency pinned by SHA, and
#     that pin always named a different commit than the app's own code -- the
#     problem the root deploy exists to remove. A re-added entry would bring
#     back a second, competing copy of the package.
#   - list every package DESCRIPTION Depends on or Imports. R CMD INSTALL runs
#     on Connect at every worker start; a missing dependency fails it there and
#     nowhere earlier. (This replaces the hand-kept DESCRIPTION snapshot the old
#     DTAtools entry carried.)
#   - list every package the app code loads (`pkg::`, library(), require()).
#     The app's Suggests are not installed by R CMD INSTALL.
#   - carry a live checksum for each file in its `files` block (only app.R --
#     see app_bundle_files() in bump_version.R for why).
#
# What it deliberately does NOT check: the `packages` block's versions. It is a
# frozen snapshot of one developer's renv library, and regenerating it with
# rsconnect::writeManifest() would rewrite all of it to match whatever machine
# ran the command. A missing entry breaks the deploy; a stale version does not.
#
# Assumes the repository root as the working directory, like every other script
# in this directory.

MANIFEST <- "manifest.json"
APP_DIR <- "inst/shiny/dta_app"

problems <- character(0)
note <- function(...) problems <<- c(problems, sprintf(...))

if (!file.exists(MANIFEST)) {
  cat("check_manifest.R: cannot find", MANIFEST, "\n")
  cat("Run this from the repository root.\n")
  quit(status = 1)
}

manifest <- tryCatch(
  jsonlite::fromJSON(MANIFEST, simplifyVector = FALSE),
  error = function(e) {
    cat("check_manifest.R:", MANIFEST, "is not valid JSON:\n  ", conditionMessage(e), "\n")
    quit(status = 1)
  }
)

for (key in c("files", "packages", "metadata")) {
  if (is.null(manifest[[key]])) note("the manifest has no top-level \"%s\" key.", key)
}
if (length(problems) > 0) {
  cat("check_manifest.R: the manifest's top-level structure is wrong:\n\n")
  for (p in problems) cat("  - ", p, "\n", sep = "")
  quit(status = 1)
}

# --- the files block ---------------------------------------------------------

recorded <- vapply(
  manifest$files,
  function(entry) if (is.null(entry$checksum)) NA_character_ else entry$checksum,
  character(1)
)
if (!identical(names(recorded), "app.R")) {
  note(
    "the files block lists %s; it must list exactly app.R.",
    if (length(recorded) == 0L) "nothing" else paste(names(recorded), collapse = ", ")
  )
}
for (f in names(recorded)) {
  actual <- unname(tools::md5sum(f))
  if (is.na(actual)) {
    note("the manifest lists %s, which does not exist.", f)
  } else if (!identical(recorded[[f]], actual)) {
    note("%s: manifest records %s, file on disk is %s.", f, recorded[[f]], actual)
  }
}

# --- DTAtools must not be a dependency ---------------------------------------

if (!is.null(manifest$packages$DTAtools)) {
  note(paste0(
    "the packages block has a DTAtools entry. Remove it: app.R installs DTAtools ",
    "from the deployed checkout, so a manifest copy would be a second, different version."
  ))
}

base_pkgs <- rownames(installed.packages(priority = "base"))
listed <- c(names(manifest$packages), base_pkgs)

# --- every DESCRIPTION dependency is present ---------------------------------

dcf <- read.dcf("DESCRIPTION", fields = c("Depends", "Imports"))
deps <- unlist(strsplit(paste(stats::na.omit(dcf[1, ]), collapse = ","), ","))
deps <- trimws(sub("\\(.*$", "", deps))
deps <- setdiff(deps[nzchar(deps)], "R")
for (p in sort(setdiff(deps, listed), method = "radix")) {
  note("DESCRIPTION depends on %s, but the manifest's packages block has no entry for it.", p)
}

# --- every package the app loads is present ----------------------------------
#
# getParseData() rather than a text regex, for the same reason
# check_deps_in_desc.R uses it: `pkg::fun` inside a comment or a string literal
# is not a real dependency, and R's own tokenizer is the only thing that knows
# the difference.
#
# `library()`/`require()` arguments are picked up as the first SYMBOL or
# STR_CONST token following the call, skipping SYMBOL_SUB/EQ_SUB so the
# named-argument form `library(package = "shiny")` is caught too.
app_r_files <- c(
  "app.R",
  list.files(APP_DIR, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
)
loaded <- character(0)

for (f in app_r_files) {
  parsed <- tryCatch(parse(f, keep.source = TRUE), error = function(e) NULL)
  if (is.null(parsed)) {
    note("%s does not parse; the manifest cannot be verified against it.", f)
    next
  }
  pd <- utils::getParseData(parsed)
  pd <- pd[pd$terminal, ]
  pd <- pd[order(pd$line1, pd$col1), ]

  loaded <- c(loaded, pd$text[pd$token == "SYMBOL_PACKAGE"])

  attach_calls <- which(
    pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% c("library", "require")
  )
  for (i in attach_calls) {
    following <- pd[seq(i + 1L, min(i + 8L, nrow(pd))), , drop = FALSE]
    # Stop at the closing paren so a bare `library()` cannot reach forward and
    # adopt the next statement's first symbol as its argument.
    close_paren <- which(following$token == "')'")
    if (length(close_paren) > 0) {
      following <- following[seq_len(close_paren[[1L]]), , drop = FALSE]
    }
    arg <- following$text[following$token %in% c("SYMBOL", "STR_CONST")]
    if (length(arg) > 0) loaded <- c(loaded, gsub('^"|"$', "", arg[[1L]]))
  }
}

# DTAtools itself is installed by app.R, not by the manifest.
missing_pkgs <- setdiff(unique(loaded), c(listed, "DTAtools"))
for (p in sort(missing_pkgs, method = "radix")) {
  note("the app loads %s, but the manifest's packages block has no entry for it.", p)
}

# --- report ------------------------------------------------------------------

if (length(problems) > 0) {
  cat("check_manifest.R:", MANIFEST, "cannot deploy the app in this repository.\n\n")
  for (p in problems) cat("  - ", p, "\n", sep = "")
  cat("\nA stale checksum is repaired by\n")
  cat("  Rscript .github/scripts/bump_version.R --sync-manifest\n")
  cat("Everything else needs fixing by hand.\n")
  quit(status = 1)
}

cat(sprintf(
  "check_manifest.R: OK -- %d packages, every dependency present, app.R checksum live.\n",
  length(manifest$packages)
))
