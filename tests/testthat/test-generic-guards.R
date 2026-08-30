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
      'e <- new.env()',
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

test_that("the generic guards that must stay unscoped are still unscoped", {
  # names/print/labels deliberately DO resolve to base's S3 generics, so their
  # methods register against those rather than shadowing them with a new S7
  # generic. Scoping those guards would silently change dispatch for every
  # user of the package, so this pins the distinction rather than leaving the
  # next person to rediscover it by breaking it.
  helpers <- readLines(testthat::test_path("..", "..", "R", "DTAColumnSpecStructure-class.R"))
  print_guard <- grep('exists\\("print", mode = "function"', helpers, value = TRUE)
  expect_length(print_guard, 1)
  expect_false(grepl("inherits", print_guard))

  # ...and the ones that must stay scoped, are.
  core <- readLines(testthat::test_path("..", "..", "R", "00_helpers.R"))
  check_guard <- grep('exists\\("check", mode = "function"', core, value = TRUE)
  expect_length(check_guard, 1)
  expect_match(check_guard, "inherits = FALSE", fixed = TRUE)
})
