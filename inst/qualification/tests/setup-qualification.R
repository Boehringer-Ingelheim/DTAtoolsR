# Sourced once per stage, before any test file.
#
# testthat sets the working directory to this directory, which on a target
# system lives inside the package library and is normally read-only. Nothing
# here or in any test file may write relative to it: `qa_tempdir()` and
# `qa_artifact_dir()` exist so that no test has to think about where it is.

# Recorded rather than forced. Forcing the locale would make the run say
# nothing about the system it is qualifying, which is the one thing it is for.
qual_log(
  "stage_setup",
  paste0("locale=", Sys.getlocale("LC_COLLATE")),
  paste0("tz=", Sys.timezone()),
  paste0("wd=", getwd())
)

# The seed is fixed per run so that a randomised case which fails can be
# reproduced from the report alone.
set.seed(qa_seed())
