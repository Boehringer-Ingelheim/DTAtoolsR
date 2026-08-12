# Shared access to the Shiny app's helper code.
#
# The app lives in inst/shiny/dta_app/. Its helper files (R/utils_dta.R,
# R/utils_export.R, R/template_core.R, R/theme.R) are auto-sourced by Shiny at
# launch, which means they are NOT part of the package namespace and are
# invisible to the test suite. These helpers source them into a private
# environment so their functions can be unit tested directly, without starting
# a Shiny server.
#
# testthat sources helper-*.R before any test file, so these are available
# everywhere in the suite.

# Directory of the bundled Shiny app.
#
# Resolved via system.file() so it works both under devtools::test() (pkgload
# shims system.file() onto inst/) and under R CMD check against the installed
# package. The app is a guaranteed package asset — a missing app directory is
# a failure, not a skip.
.shiny_app_dir <- function() {
  dir <- system.file("shiny", "dta_app", package = "DTAtools")
  expect_true(nzchar(dir), info = "inst/shiny/dta_app missing from the package")
  dir
}

# The app's helper .R files, in the order Shiny would source them.
.shiny_app_helper_files <- function() {
  dir <- file.path(.shiny_app_dir(), "R")
  expect_true(dir.exists(dir), info = "inst/shiny/dta_app/R missing from the package")
  files <- sort(list.files(dir, pattern = "[.][Rr]$", full.names = TRUE))
  expect_gt(length(files), 0)
  files
}

# Cache, so the ~2,900 lines of helper code are parsed once per test run rather
# than once per test_that() block.
.shiny_env_cache <- new.env(parent = emptyenv())

# Source the app's helper files into a fresh environment and return it.
#
# The environment's parent is shiny's namespace, which reproduces the scope the
# app itself runs under: app.R starts with library(shiny), and the helper files
# call a handful of shiny/htmltools UI functions unqualified (div(), column(),
# fluidRow(), tagList(), ...). Everything else they use is already namespaced
# (DTAtools::, bslib::, yaml::, ...) and resolves through shiny's own parent
# chain down to the search path.
#
# Returns the same environment on repeated calls. Tests must therefore treat it
# as read-only: assign nothing into it, and mock via local_mocked_bindings()
# rather than by overwriting a binding in place.
app_env <- function() {
  skip_if_not_installed("shiny")
  if (!is.null(.shiny_env_cache$env)) {
    return(.shiny_env_cache$env)
  }
  env <- new.env(parent = asNamespace("shiny"))

  # Several app helpers locate bundled assets with an unqualified
  # system.file(package = "DTAtools"). Under devtools::test() the package is
  # loaded, not installed, so only pkgload's shimmed system.file() knows how to
  # resolve inst/ — and that shim lives on the search path, which the parent
  # chain above deliberately bypasses. Bind whichever system.file() is correct
  # here (the shim in dev mode, base::system.file() under R CMD check against
  # the installed package) so those helpers resolve their assets in both.
  env$system.file <- system.file

  for (f in .shiny_app_helper_files()) {
    sys.source(f, envir = env, keep.source = FALSE)
  }
  .shiny_env_cache$env <- env
  env
}

# Fetch one function from the app helper environment by name.
#
# Fails loudly when the function does not exist, so that renaming a helper in
# the app turns into a clear test failure instead of a confusing
# "attempt to apply non-function" further down.
app_fn <- function(name) {
  fn <- get0(name, envir = app_env(), inherits = FALSE)
  if (!is.function(fn)) {
    stop(
      sprintf("app helper `%s()` is not defined in inst/shiny/dta_app/R", name),
      call. = FALSE
    )
  }
  fn
}

# ---- DTA fixtures for app-helper tests -------------------------------------

# A DTA read from the bundled clinical spec, with no data bound to it.
app_fixture_dta <- function() {
  spec_path <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  expect_true(nzchar(spec_path), info = "clinical_dta.yaml missing from extdata")
  read_dta_from_yaml(spec_path)
}

# Path to a bundled extdata fixture, asserted to exist.
app_fixture_path <- function(filename) {
  path <- system.file("extdata", filename, package = "DTAtools")
  expect_true(nzchar(path), info = paste(filename, "missing from extdata"))
  path
}

# A DTA with `filename` bound to its first dataset, optionally validated.
#
# `checked = FALSE` yields the "data bound but never validated" state the app
# reports as "pending"; `checked = TRUE` yields pass/fail depending on the
# fixture used.
app_fixture_dta_with_data <- function(filename = "clinical_data.csv", checked = FALSE) {
  dta <- load_file(app_fixture_dta(), 1, file = app_fixture_path(filename))
  if (checked) {
    dta <- check(dta, persist = FALSE, quiet = TRUE)
  }
  dta
}
