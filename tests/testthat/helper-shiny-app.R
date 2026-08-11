# Test harness for the Shiny app shipped under inst/shiny/dta_app.
#
# The app is not part of the package namespace: Shiny sources its R/*.R at
# runtime. To unit-test those helpers we locate the app directory and source the
# pure helper files into a dedicated environment.
#
# LOCATING THE APP IS THE TRAP. Under pkgload/devtools the "package directory"
# is the repo root, where the app lives at inst/shiny/dta_app; in an INSTALLED
# package inst/ has been stripped and the same app lives at shiny/dta_app.
# Neither path works in both situations, and the usual lookups fail QUIETLY:
# system.file() returns "" (not an error) when it misses, and a "" path silently
# degrades to the working directory rather than blowing up. So every candidate
# is tried, the first one that actually contains app.R wins, and a total miss is
# a hard error rather than an empty directory.
dta_app_dir <- function() {
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

# Source the app's pure helper files into a fresh environment and return it.
# Only files that define functions are sourced: app.R itself calls Shiny at
# top level and cannot be evaluated outside a running app.
.dta_app_env_cache <- new.env(parent = emptyenv())

dta_app_env <- function() {
  if (!is.null(.dta_app_env_cache$env)) {
    return(.dta_app_env_cache$env)
  }
  dir <- dta_app_dir()
  env <- new.env(parent = globalenv())
  for (f in c("R/utils_dta.R", "R/theme.R")) {
    source(file.path(dir, f), local = env, keep.source = FALSE)
  }
  .dta_app_env_cache$env <- env
  env
}

# Fetch one app helper by name.
dta_app_fn <- function(name) {
  get(name, envir = dta_app_env(), inherits = FALSE)
}

# The full text of one app source file, for the few assertions that are about
# the app's static wiring (a CSS class, a UI branch) rather than a function.
dta_app_source <- function(file) {
  paste(
    readLines(file.path(dta_app_dir(), file), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
}

# A two-column spec collection: one character column and one numeric column, so
# a non-numeric value in VAL is an import error and nothing else.
dta_app_char_num_specs <- function() {
  cols <- list(
    DTAtools::DTAColumnSpec(id = "SUBJID", type = "SAS Char", nullable = FALSE),
    DTAtools::DTAColumnSpec(id = "VAL", type = "SAS Num", nullable = TRUE)
  )
  DTAtools::DTAColumnSpecCollection(
    columns = stats::setNames(cols, vapply(cols, function(x) x@id, character(1))),
    rules = list()
  )
}
