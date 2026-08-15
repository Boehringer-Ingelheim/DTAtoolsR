#!/usr/bin/env Rscript

# CI guard: every file that records the package version must agree with
# DESCRIPTION. Replaces check_shiny_version_file.R, which covered only
# inst/shiny/dta_app/VERSION -- the two sites it did not cover, manifest.json and
# docs/index.html, had both drifted behind by the time this was written.
#
# The comparison itself lives in bump_version.R's `--check` mode, so the checker
# and the writer cannot disagree about what the sites are or what counts as
# drift. This wrapper exists so the workflow step reads as a check rather than as
# a bump invoked with a flag.
#
# Like every other script in this directory, it assumes the repository root as
# the working directory -- that is how the r-style workflow sources it.

bump <- ".github/scripts/bump_version.R"

if (!file.exists(bump)) {
  cat("check_version_sync.R: cannot find", bump, "\n")
  cat("Run this from the repository root.\n")
  quit(status = 1)
}

# system2() rather than source(): bump_version.R reads commandArgs() and would
# otherwise see this script's arguments instead of "--check".
status <- system2(
  file.path(R.home("bin"), "Rscript"),
  c(shQuote(bump), "--check")
)

quit(status = status)
