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
#     *0.17.3* release commit -- and 0.18.1 shipped with no SHA fields at all,
#     because an earlier version of this script simply forbade their presence
#     rather than checking their correctness. Posit Connect needs a resolvable
#     SHA to build a github-sourced package's archive URL; without one it
#     crashes with "argument is of length zero" on every deploy, not just a
#     stale one. See .github/workflows/manifest-release-sha.yml, which is what
#     populates these fields now, and bump_version.R's manifest_ref_site()
#     comment for why they can't be bump-version sites.
#
# So this script checks the parts a version patcher structurally cannot:
# whether the manifest still describes the right set of files, whether the
# checksums are live, and whether GithubSHA1/RemoteSha -- when present -- name
# the commit their ref actually resolves to.
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
# the working directory -- that is how the r-style workflow sources it. Unlike
# the others, it also needs `git rev-parse` to see the release tags, which
# means the checkout step that runs it must NOT be a shallow, tag-less clone
# (see the fetch-depth note in r-style.yaml).

APP_DIR <- "inst/shiny/dta_app"
MANIFEST <- file.path(APP_DIR, "manifest.json")

# Fields that describe when/where the package was locally built, which has no
# correct value to check against -- it names a developer's machine, not this
# release. These must stay removed, full stop; GithubSHA1/RemoteSha are handled
# separately below, because unlike a build timestamp they DO have a correct
# value once the release tag exists.
FORBIDDEN_DTATOOLS_FIELDS <- c("Packaged", "Built")

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

# --- release SHA fields, when present, must be correct ------------------------
#
# GithubSHA1/RemoteSha are legitimately ABSENT for most of a release cycle --
# see bump_version.R's manifest_ref_site() -- so their absence is not checked
# here at all. What's checked is the thing "forbidden" never could: when one IS
# present, does it actually name the commit its ref (GithubRef/RemoteRef)
# resolves to? A stale SHA is worse than a missing one -- Connect would
# silently deploy old code under a new version number instead of failing loudly
# -- and "forbidden" cannot distinguish stale from correct, only present from
# absent.
SHA_FIELDS <- list(RemoteRef = "RemoteSha", GithubRef = "GithubSHA1")

# NA if `ref` does not resolve to a commit at all (unknown tag, or a checkout
# too shallow to have fetched it) -- `--verify --quiet` fails silently instead
# of printing to stderr, which system2() would otherwise interleave into
# `out`.
git_commit_for <- function(ref) {
  out <- suppressWarnings(system2(
    "git", c("rev-parse", "--verify", "--quiet", paste0(ref, "^{commit}")),
    stdout = TRUE, stderr = FALSE
  ))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) return(NA_character_)
  if (length(out) != 1L) return(NA_character_)
  out
}

if (!is.null(dtatools)) {
  for (ref_field in names(SHA_FIELDS)) {
    sha_field <- SHA_FIELDS[[ref_field]]
    recorded_sha <- dtatools[[sha_field]]
    if (is.null(recorded_sha)) next # absent is expected; nothing to verify

    ref <- dtatools[[ref_field]]
    resolved <- git_commit_for(ref)
    if (is.na(resolved)) {
      note(
        paste0(
          "the DTAtools entry's %s is \"%s\", but git cannot resolve %s to a commit ",
          "(unknown tag, or the checkout didn't fetch it). A %s should never be ",
          "recorded for a ref that doesn't exist."
        ),
        sha_field, recorded_sha, ref, sha_field
      )
    } else if (!identical(resolved, recorded_sha)) {
      note(
        "the DTAtools entry's %s is \"%s\", but %s resolves to \"%s\".",
        sha_field, recorded_sha, ref, resolved
      )
    }
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
  cat("(A missing packages entry, a forbidden field, and a wrong/unresolvable\n")
  cat("RemoteSha or GithubSHA1 are NOT repaired by it. The SHA fields are set by\n")
  cat("  Rscript .github/scripts/bump_version.R --set-release-sha <sha>\n")
  cat("everything else needs fixing by hand.)\n")
  quit(status = 1)
}

cat(sprintf(
  "check_manifest.R: OK -- %d files and %d packages, every checksum live.\n",
  length(recorded), length(manifest$packages)
))
