# Shared access to the Shiny app's helper code.
#
# The app lives in inst/shiny/dta_app/. Its helper files (R/utils_dta.R,
# R/utils_export.R, R/template_core.R, R/theme.R) are auto-sourced by Shiny at
# launch, which means they are NOT part of the package namespace and are
# invisible to the test suite. These helpers source them into a private
# environment so their functions can be unit tested directly, without starting
# a Shiny server.
#
# This is the ONLY harness for the app's test files (test-shinyapp-*.R). It
# used to coexist with a second, independently grown harness
# (helper-shiny-app.R, dta_app_*() names) that solved the same problem; the two
# were merged here, keeping app_env()'s wider file coverage (all four helper
# files, not just utils_dta.R and theme.R) and dta_app_dir()'s more defensive
# handling of the app-directory lookup trap described below.
#
# testthat sources helper-*.R before any test file, so these are available
# everywhere in the suite.

# Directory of the bundled Shiny app.
#
# LOCATING THE APP IS THE TRAP: system.file() returns "" (not an error) when
# it misses, and a "" path silently degrades to the working directory rather
# than blowing up. system.file() alone is normally enough here -- pkgload
# shims it onto inst/ under devtools::test(), and base::system.file() finds it
# directly once the package is installed for R CMD check -- but a fallback
# candidate list plus a hard existence check closes the gap for any
# invocation where the shim isn't in play. Every candidate is tried, the
# first one that actually contains app.R wins, and a total miss is a hard
# error, never a silently empty or wrong directory. The app is a guaranteed
# package asset — a missing app directory is a failure, not a skip.
.shiny_app_dir <- function() {
  candidates <- c(
    system.file("shiny", "dta_app", package = "DTAtools"),
    testthat::test_path("..", "..", "inst", "shiny", "dta_app"),
    tryCatch(
      file.path(find.package("DTAtools"), "inst", "shiny", "dta_app"),
      error = function(e) ""
    ),
    tryCatch(
      file.path(find.package("DTAtools"), "shiny", "dta_app"),
      error = function(e) ""
    )
  )
  candidates <- candidates[nzchar(candidates)]
  hit <- candidates[file.exists(file.path(candidates, "app.R"))]
  if (length(hit) == 0) {
    cli::cli_abort(
      "Could not locate the Shiny app directory (no {.file app.R} in any candidate)."
    )
  }
  normalizePath(hit[[1]], winslash = "/", mustWork = TRUE)
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

# A two-column spec collection: one character column and one numeric column, so
# a non-numeric value in VAL is an import error and nothing else.
app_fixture_char_num_specs <- function() {
  cols <- list(
    DTAtools::DTAColumnSpec(id = "SUBJID", type = "SAS Char", nullable = FALSE),
    DTAtools::DTAColumnSpec(id = "VAL", type = "SAS Num", nullable = TRUE)
  )
  DTAtools::DTAColumnSpecCollection(
    columns = stats::setNames(cols, vapply(cols, function(x) x@id, character(1))),
    rules = list()
  )
}

# ---- Unlocking a loaded document for editing --------------------------------

# Drive the app's "Create new version" flow, which is what unlocks editing for
# a document LOADED from an existing one (an upload, a bundled example, or a
# restored session). Such a document arrives read-only by design -- see the
# WHY comment on editing() in app.R -- and this confirm handler is one of the
# paths that flips rv$editing on, so every test that loads a fixture and then
# edits it needs this in between.
unlock_editing <- function(session, version = "9.9") {
  session$setInputs(create_new_version = 1)
  session$setInputs(new_version_value = version)
  session$setInputs(new_version_confirm = 1)
}

# Enter editing via the app's "Enable edit mode" control, without creating a
# new version. Works whether or not a version entry is already open -- it
# resumes an open one rather than starting anything.
#
# Wrapping the input id rather than inlining setInputs() at every call site
# means the id belongs to the app, not to the test: if the app ever renames
# enable_edit_mode, only this line changes.
enter_edit_mode <- function(session) session$setInputs(enable_edit_mode = 1)

# Leave editing via the app's "Stop editing" control.
#
# Same reasoning as enter_edit_mode() above -- one seam for the input id
# rather than setInputs(stop_editing = ...) scattered across every test.
leave_edit_mode <- function(session) session$setInputs(stop_editing = 1)

# ---- Static-source assertions for app-wiring tests --------------------------

# The full text of one app source file, for the few assertions that are about
# the app's static wiring (a CSS class, a UI branch) rather than a function.
app_source <- function(file) {
  paste(
    readLines(file.path(.shiny_app_dir(), file), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
}
