#!/usr/bin/env Rscript
# CI guard: the Shiny app's rsconnect manifest describes the app that is
# actually in the repository.
#
# Why this exists. bump_version.R keeps the manifest's VERSION-bearing lines in
# step with DESCRIPTION, and that is all it can do -- it is a line patcher. The
# rest of the file was maintained by hand, and the history shows the hand
# maintenance failing every release:
#
#   - release 0.16.0 shipped with app.R recorded as df2b7079... while the file
#     on disk was 9d798811.... Six of the eight `files` checksums were checked
#     by nothing at all, and app.R is the most-edited file in the bundle.
#   - the file shipped 0.17.3 with `RemoteRef` still reading v0.17.2, then
#     shipped 0.18.0 with hand-added `GithubSHA1`/`RemoteSha` pointing at the
#     *0.17.3* release commit.
#
# So this script checks the parts a version patcher structurally cannot:
# whether the manifest still describes the right set of files, whether the
# checksums are live, and whether the fields that can never be correct have
# stayed deleted.
#
# What it deliberately does NOT check:
#   - the `packages` block's contents. It is a frozen snapshot of one
#     developer's renv library (87 entries, `Built:` spanning three R patch
#     releases, `locale: de_DE`) and regenerating it with
#     rsconnect::writeManifest() would rewrite all of it to match whatever
#     machine ran the command. Only "is every package the app loads present at
#     all" is verified -- a missing entry breaks the deploy, a stale version
#     within an entry does not.
#   - the version strings themselves, including RemoteRef/GithubRef. Those are
#     sites in .github/scripts/bump_version.R and are covered by
#     check_version_sync.R; duplicating them here would create exactly the
#     two-implementations-that-can-disagree problem that file exists to avoid.
#   - JSON validity, which the `check-json` pre-commit hook covers on every
#     commit (and which fromJSON() below would surface anyway).
#
# Like every other script in this directory, it assumes the repository root as
# the working directory -- that is how the r-style workflow sources it.

APP_DIR <- "inst/shiny/dta_app"
MANIFEST <- file.path(APP_DIR, "manifest.json")

# Fields removed from the manifest's DTAtools entry, which must stay removed. A
# bump commit cannot know the SHA of the release commit that will eventually
# contain it, and it cannot know when that release will be built -- so there is
# no value these could ever be checked against, and every past attempt to
# maintain them by hand recorded the *previous* release's commit.
FORBIDDEN_DTATOOLS_FIELDS <- c("GithubSHA1", "RemoteSha", "Packaged", "Built")

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

# --- the files block matches the app directory -------------------------------

recorded <- vapply(
  manifest$files,
  function(entry) if (is.null(entry$checksum)) NA_character_ else entry$checksum,
  character(1)
)
on_disk <- setdiff(
  list.files(APP_DIR, recursive = TRUE, all.files = TRUE, no.. = TRUE),
  "manifest.json"
)

for (f in setdiff(on_disk, names(recorded))) {
  note("%s is in the app directory but has no entry in the manifest (it would not be deployed).", f)
}
for (f in setdiff(names(recorded), on_disk)) {
  note("the manifest lists %s, which does not exist in the app directory.", f)
}

for (f in intersect(names(recorded), on_disk)) {
  actual <- unname(tools::md5sum(file.path(APP_DIR, f)))
  if (is.na(recorded[[f]])) {
    note("the manifest's entry for %s has no checksum.", f)
  } else if (!identical(recorded[[f]], actual)) {
    note("%s: manifest records %s, file on disk is %s.", f, recorded[[f]], actual)
  }
}

# --- fields that must stay deleted -------------------------------------------

dtatools <- manifest$packages$DTAtools$description
if (is.null(dtatools)) {
  note("the manifest's packages block has no DTAtools entry.")
} else {
  for (field in intersect(FORBIDDEN_DTATOOLS_FIELDS, names(dtatools))) {
    note(
      "the DTAtools entry has a \"%s\" field. Remove it -- it cannot be correct at commit time.",
      field
    )
  }
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
# named-argument form `library(package = "shiny")` is caught too -- that form
# pushes the package name past a naive fixed-width window, and being silently
# blind to it would mean a package could be absent from the manifest and still
# be reported OK.
app_r_files <- list.files(APP_DIR, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
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

# Base packages ship with every R install and rsconnect never lists them.
base_pkgs <- rownames(installed.packages(priority = "base"))
missing_pkgs <- setdiff(unique(loaded), c(names(manifest$packages), base_pkgs))
for (p in sort(missing_pkgs, method = "radix")) {
  note("the app loads %s, but the manifest's packages block has no entry for it.", p)
}

# --- report ------------------------------------------------------------------

if (length(problems) > 0) {
  cat("check_manifest.R:", MANIFEST, "does not describe the app in this repository.\n\n")
  for (p in problems) cat("  - ", p, "\n", sep = "")
  cat("\nMost of this is repaired mechanically by\n")
  cat("  Rscript .github/scripts/bump_version.R --sync-manifest\n")
  cat("which rebuilds the file list and every checksum from the app directory.\n")
  cat("(A missing packages entry and a forbidden field are NOT repaired by it --\n")
  cat("fix those by hand.)\n")
  quit(status = 1)
}

cat(sprintf(
  "check_manifest.R: OK -- %d files and %d packages, every checksum live.\n",
  length(recorded), length(manifest$packages)
))
