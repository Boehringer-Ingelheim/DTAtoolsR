# Regression cover for the "generic already exists" guards in R/.
#
# Several class files register S7 methods against a generic that is created
# once, behind an `if (!exists(...))` guard. Those guards used to search the
# whole SEARCH PATH, so any attached package exporting a plain function of the
# same name made the guard skip creating the generic -- and every later
# method() registration then failed against the stranger's function.
#
# That is not hypothetical: `devtools::load_all()` failed with
#   `generic` is a function, but not an S3 generic function:
#   function (pkg = ".", document = NULL, build_args = NULL, ...)
# because devtools exports a plain `check()`, while `pkgload::load_all()` in a
# clean session succeeded. The two differ only in whether devtools is attached.

test_that("check is a real S7 generic in the package namespace", {
  # Cheap invariant. It cannot catch the bug on its own -- in a clean test
  # session the guard behaves either way -- but it fails loudly if someone
  # replaces the generic with a plain function.
  g <- get("check", envir = asNamespace("DTAtools"))
  expect_s3_class(g, "S7_generic")
})

test_that("the package still loads when a non-generic `check` is attached", {
  # THE test for this bug. It has to run in a subprocess: the failure only
  # happens while the package is being LOADED, so it cannot be observed from
  # inside a session where it is already loaded.
  skip_on_cran()
  skip_if_not_installed("pkgload")

  pkg <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "not running from a package source tree (R CMD check runs from the install)"
  )

  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
  skip_if(!file.exists(rscript), "Rscript not found")

  script <- withr::local_tempfile(fileext = ".R")
  writeLines(
    c(
      # Stand in for devtools without needing it installed: the only thing
      # that ever mattered is a plain function named `check` on the search
      # path. The signature mirrors devtools::check so the failure is the
      # same one users reported.
      "e <- new.env()",
      'e$check <- function(pkg = ".", document = NULL, build_args = NULL, ...) NULL',
      'attach(e, name = "fake_devtools", warn.conflicts = FALSE)',
      sprintf("pkgload::load_all(%s, quiet = TRUE)", shQuote(pkg)),
      'g <- get("check", envir = asNamespace("DTAtools"))',
      'stopifnot(inherits(g, "S7_generic"))',
      'cat("LOADED_OK\\n")'
    ),
    script
  )

  out <- suppressWarnings(
    system2(rscript, c("--vanilla", shQuote(script)), stdout = TRUE, stderr = TRUE)
  )

  expect_true(
    any(grepl("^LOADED_OK$", out)),
    info = paste(c("subprocess output:", out), collapse = "\n")
  )
})

test_that("every generic-existence guard in R/ is scoped correctly", {
  # Whole-package invariant, generalising the two hand-picked spot checks this
  # test used to be. `names`, `print` and `labels` deliberately resolve to
  # base R's S3 generics (confirmed empirically: in a clean `--vanilla`
  # session `exists("print", mode = "function")` is TRUE), so their guards
  # register methods against the base generic and must stay unscoped --
  # scoping them would make DTAtools create its own generic that shadows
  # `base::print`/`base::names`/`base::labels` for every user of the package.
  # Every other guard name does NOT resolve to anything in a clean session, so
  # it must carry `inherits = FALSE`: without it, an attached package
  # exporting a plain function of the same name (e.g. devtools::check) makes
  # the guard skip creating the generic. See R/00_helpers.R for the full
  # account.
  unscoped_names <- c("names", "print", "labels")

  r_dir <- testthat::test_path("..", "..", "R")
  testthat::skip_if_not(dir.exists(r_dir), "package source not available")

  r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  expect_gt(length(r_files), 0)

  guard_pattern <- 'exists\\("([a-zA-Z_.]+)",\\s*mode\\s*=\\s*"function"'

  offenders <- character(0)
  total_hits <- 0

  for (f in r_files) {
    lines <- readLines(f, warn = FALSE)
    hits <- grep(guard_pattern, lines)
    total_hits <- total_hits + length(hits)

    for (i in hits) {
      line <- lines[i]
      name <- regmatches(line, regexec(guard_pattern, line))[[1]][2]
      is_scoped <- grepl("inherits = FALSE", line, fixed = TRUE)
      must_stay_unscoped <- name %in% unscoped_names

      ok <- if (must_stay_unscoped) !is_scoped else is_scoped

      if (!ok) {
        offenders <- c(
          offenders,
          sprintf(
            "%s:%d: exists(\"%s\", ...) is %s but must be %s",
            basename(f),
            i,
            name,
            if (is_scoped) "scoped (inherits = FALSE)" else "unscoped",
            if (must_stay_unscoped) "unscoped" else "scoped (inherits = FALSE)"
          )
        )
      }
    }
  }

  # The scan is line-based: a styler reflow that folds the pattern or a
  # matching line across lines would silently leave `hits` empty in every
  # file without failing anything above. Pin a nonzero total so that
  # happens loudly instead.
  expect_gt(total_hits, 0)

  expect_true(
    length(offenders) == 0,
    info = paste(c("Incorrectly scoped generic guard(s):", offenders), collapse = "\n")
  )
})

test_that("the package still loads when a non-generic `read_file` is attached", {
  # read_file is the most exposed remaining unscoped-by-default name --
  # readr::read_file is a plain function, not a generic -- so it is the guard
  # most likely to be hit by an attached package in practice. Same pattern as
  # the `check` subprocess test above: the failure only happens while the
  # package is being LOADED, so it must be observed from a fresh session.
  skip_on_cran()
  skip_if_not_installed("pkgload")

  pkg <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "not running from a package source tree (R CMD check runs from the install)"
  )

  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
  skip_if(!file.exists(rscript), "Rscript not found")

  script <- withr::local_tempfile(fileext = ".R")
  writeLines(
    c(
      # Stand in for a package exporting a plain `read_file()`, mirroring
      # readr::read_file's signature closely enough to be the same shape of
      # collision.
      "e <- new.env()",
      "e$read_file <- function(file, ...) NULL",
      'attach(e, name = "fake_readr", warn.conflicts = FALSE)',
      sprintf("pkgload::load_all(%s, quiet = TRUE)", shQuote(pkg)),
      'g <- get("read_file", envir = asNamespace("DTAtools"))',
      'stopifnot(inherits(g, "S7_generic"))',
      'cat("LOADED_OK\\n")'
    ),
    script
  )

  out <- suppressWarnings(
    system2(rscript, c("--vanilla", shQuote(script)), stdout = TRUE, stderr = TRUE)
  )

  expect_true(
    any(grepl("^LOADED_OK$", out)),
    info = paste(c("subprocess output:", out), collapse = "\n")
  )
})

test_that("the skip_on_cran() tests above are not silently skipped on CI", {
  # COMPANION GUARD to the two subprocess tests in this file. They are the only
  # way to observe a package-LOAD failure -- the failure happens while DTAtools
  # is being loaded, so it cannot be seen from a session where it already is --
  # and both are gated on skip_on_cran(), which skips unless NOT_CRAN is set.
  #
  # `R CMD check` does not set it. These tests used to run under it only because
  # fourteen test files each called Sys.setenv(NOT_CRAN = "true") at file level,
  # which leaked into every file sourced after them; when that leak was cleaned
  # up the gated tests went quiet without failing anything, which is the whole
  # problem -- a permanently skipped test is invisible dead coverage. CI now
  # declares NOT_CRAN once, in .github/workflows/R-CMD-check.yaml, and this
  # asserts the declaration is still there and still reaching the tests.
  #
  # Asserted from the environment rather than by reading the workflow file,
  # because `.Rbuildignore` keeps .github out of the built tarball: under the
  # very check this is defending, the file is not there to read. This test
  # itself never skips -- both branches assert.
  if (identical(Sys.getenv("CI"), "true")) {
    expect_identical(
      Sys.getenv("NOT_CRAN"), "true",
      info = paste(
        "CI must set NOT_CRAN (see the job-level env: in R-CMD-check.yaml);",
        "without it every skip_on_cran() test in this suite silently skips."
      )
    )
  } else {
    # Off CI, whether it is set is the runner's business -- devtools::test()
    # sets it, a bare R CMD check does not. What is pinned is the spelling:
    # skip_on_cran() tests for the exact string "true", so a well-meant "TRUE"
    # would skip everything while looking set.
    expect_true(Sys.getenv("NOT_CRAN") %in% c("", "true"))
  }
})
