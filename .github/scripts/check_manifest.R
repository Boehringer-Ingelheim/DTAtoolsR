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
# checksums are live, and whether GithubSHA1/RemoteSha are present at all and
# name the commit their ref actually resolves to.
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

# --- deploy SHA fields must be present and correct ----------------------------
#
# GithubSHA1/RemoteSha used to be treated as legitimately ABSENT for most of a
# release cycle, on the theory that only tagged releases are ever deployed.
# That theory was wrong: `master` and `dev` are each deployed to their own
# Connect instance, and Connect builds the package's archive URL from this SHA.
# Without one, `archiveUrl` is empty and every deploy dies on
# `if (!grepl("^http", archiveUrl))` -- "argument is of length zero".
#
# Both branches shipped in exactly that state while this script reported OK,
# because "absent" was the one case it declined to look at. So absence is now a
# FAILURE. The correctness check remains: does the SHA actually name the commit
# its ref resolves to? A stale SHA is worse than a missing one -- Connect would
# silently deploy old code under a new version number rather than failing
# loudly.
SHA_FIELDS <- list(RemoteRef = "RemoteSha", GithubRef = "GithubSHA1")

# Tag-shaped refs pin a release; anything else is a branch. The shape decides
# both what bump_version.R writes and what this script demands, so the predicate
# is SHARED with it rather than restated here -- two copies of this rule are two
# things that can drift, and the cost of drift is precisely the release/branch
# conflation this whole check exists to catch.
REF_SHAPE <- ".github/scripts/ref_shape.R"
if (!file.exists(REF_SHAPE)) {
  cat("check_manifest.R: cannot find", REF_SHAPE, "\n")
  cat("Run this from the repository root.\n")
  quit(status = 1)
}
source(REF_SHAPE)

# NA if `ref` does not resolve to a commit at all (unknown tag, or a checkout
# too shallow to have fetched it) -- `--verify --quiet` fails silently instead
# of printing to stderr, which system2() would otherwise interleave into
# `out`.
#
# origin/<ref> is tried FIRST, and the bare ref only as a fallback. Two reasons,
# and the order matters in opposite directions in the two places this runs:
#
#   in CI   a branch name like `dev` has no LOCAL ref at all. actions/checkout
#           builds a detached merge commit for a pull_request and creates only
#           the branch it was told to, so the bare lookup finds nothing.
#   locally a local branch of that name usually DOES exist, and is routinely
#           stale -- a checked-out `dev` sitting several commits behind
#           origin/dev is the normal state of a worktree. Trusting it inverts
#           the containment test: a freshly pushed tip reads as "not in dev's
#           history" because it is a DESCENDANT of the stale local branch
#           rather than an ancestor. That is a false failure, and the
#           mirror-image false PASS is available too.
#
# The remote-tracking ref is what the deployed branch actually is, which is the
# thing the pin has to be consistent with. A tag has no origin/ form, so it
# simply falls through to the bare name.
git_commit_for <- function(ref) {
  candidates <- c(paste0("origin/", ref), paste0("refs/remotes/origin/", ref), ref)
  for (candidate in candidates) {
    out <- suppressWarnings(system2(
      "git", c("rev-parse", "--verify", "--quiet", paste0(candidate, "^{commit}")),
      stdout = TRUE, stderr = FALSE
    ))
    status <- attr(out, "status")
    if (!is.null(status) && status != 0L) next
    if (length(out) != 1L) next
    return(out)
  }
  NA_character_
}

# TRUE when `ancestor` is contained in `ref`'s history. FALSE when it is not, and
# also when it is not a commit this checkout knows about -- both are equally a
# reason to reject the SHA, so they collapse to one answer here.
git_contains <- function(ancestor, ref) {
  status <- suppressWarnings(system2(
    "git", c("merge-base", "--is-ancestor", ancestor, ref),
    stdout = FALSE, stderr = FALSE
  ))
  identical(as.integer(status), 0L)
}

# How many commits `ref` has moved on since `sha`. NA if that cannot be counted.
git_commits_behind <- function(sha, ref) {
  out <- suppressWarnings(system2(
    "git", c("rev-list", "--count", paste0(sha, "..", ref)),
    stdout = TRUE, stderr = FALSE
  ))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) return(NA_integer_)
  if (length(out) != 1L) return(NA_integer_)
  suppressWarnings(as.integer(out))
}

# Reported, never enforced. Being ON the branch is the correctness property and
# is checked below; being CURRENT is a matter of degree, and the honest bound on
# it is the manifest-sync.yml workflow that re-pins on every PR -- not a
# threshold here. A stricter rule would have to pick a number of commits to
# tolerate and would go red for the whole window between a push landing and that
# workflow finishing: a false alarm, not a defect.
#
# READ THE NUMBER CAREFULLY. Where this script actually runs -- r-style.yaml, on
# pull_request -- the count is mostly the AGE OF THE BRANCH, not a problem: a PR
# opened before ten commits landed on dev trails by ten through no fault of its
# own, and merging fixes it. It is worth attention only when it is large on
# `dev` itself, which means the pinning workflow has stopped running. The
# printed line says so, because a number that looks like a warning and usually
# is not is how people learn to stop reading warnings.
#
# What the ancestor test does NOT catch: a SHA genuinely on the branch but very
# old still passes. Connect would install that old DTAtools under the current
# app bundle. This note is the only thing that would show it.
staleness_notes <- character(0)

if (!is.null(dtatools)) {
  for (ref_field in names(SHA_FIELDS)) {
    sha_field <- SHA_FIELDS[[ref_field]]
    recorded_sha <- dtatools[[sha_field]]
    ref <- dtatools[[ref_field]]

    if (is.null(recorded_sha)) {
      note(
        paste0(
          "the DTAtools entry has no \"%s\". Connect builds the package's archive URL from it ",
          "and fails with \"argument is of length zero\" without one, so the app cannot be ",
          "deployed at all. Pin it with `bump_version.R --set-deploy-sha <sha>`."
        ),
        sha_field
      )
      next
    }

    resolved <- git_commit_for(ref)
    if (is.na(resolved)) {
      note(
        paste0(
          "the DTAtools entry's %s is \"%s\", but git cannot resolve %s to a commit ",
          "(unknown tag or branch, or the checkout didn't fetch it). A %s should never be ",
          "recorded for a ref that doesn't exist."
        ),
        sha_field, recorded_sha, ref, sha_field
      )
      next
    }

    # A release ref names one immutable commit, so the SHA must equal it. A
    # branch ref moves: the pin necessarily trails the tip by at least the
    # commit that recorded it, so demanding equality would fail on every push.
    # What must hold is that the SHA is genuinely ON that branch.
    if (is_release_ref(ref)) {
      if (!identical(resolved, recorded_sha)) {
        note(
          "the DTAtools entry's %s is \"%s\", but %s resolves to \"%s\".",
          sha_field, recorded_sha, ref, resolved
        )
      }
    } else if (!git_contains(recorded_sha, resolved)) {
      note(
        paste0(
          "the DTAtools entry's %s is \"%s\", which is not in %s's history. A branch ref may ",
          "trail its tip, but the SHA must name a commit actually on the branch."
        ),
        sha_field, recorded_sha, ref
      )
    } else {
      behind <- git_commits_behind(recorded_sha, resolved)
      if (!is.na(behind) && behind > 0L) {
        staleness_notes <- c(staleness_notes, sprintf(
          "%s trails %s by %d commit%s", sha_field, ref, behind, if (behind == 1L) "" else "s"
        ))
      }
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
  cat("(A missing packages entry, a forbidden field, and a missing, wrong or\n")
  cat("unresolvable RemoteSha or GithubSHA1 are NOT repaired by it. The SHA fields\n")
  cat("are set by\n")
  cat("  Rscript .github/scripts/bump_version.R --set-deploy-sha <sha>\n")
  cat("and the ref they hang off by\n")
  cat("  Rscript .github/scripts/bump_version.R --set-deploy-ref <ref>\n")
  cat("everything else needs fixing by hand.)\n")
  quit(status = 1)
}

cat(sprintf(
  "check_manifest.R: OK -- %d files and %d packages, every checksum live.\n",
  length(recorded), length(manifest$packages)
))
for (s in staleness_notes) {
  cat("  note: ", s, ".\n", sep = "")
}
if (length(staleness_notes) > 0) {
  cat("        Expected on a branch opened before those commits landed -- merging clears it.\n")
  cat("        Only a concern if it is large on dev itself, which would mean\n")
  cat("        manifest-sync.yml has stopped re-pinning.\n")
}
