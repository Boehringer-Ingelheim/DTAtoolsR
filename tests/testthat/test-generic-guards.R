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
  r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)

  guard_pattern <- 'exists\\("([a-zA-Z_.]+)",\\s*mode\\s*=\\s*"function"'

  offenders <- character(0)

  for (f in r_files) {
    lines <- readLines(f, warn = FALSE)
    hits <- grep(guard_pattern, lines)

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
