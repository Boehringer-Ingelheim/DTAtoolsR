# Access to the bundled Shiny app's helper code, for the qualification suite.
#
# Ported from tests/testthat/helper-shinyapp.R. That file is the harness the
# package's own development suite uses for the app's helper files -- it is
# not installed with the package, so it cannot be sourced here, and this is a
# freestanding copy of the same approach. Every name below is prefixed
# qapp_ so nothing here can collide with a helper from another qualification
# area, or with a symbol the app's own sourced files put into the search
# path this environment's parent chain reaches.
#
# LOAD-TIME SAFETY: nothing below calls qa_tempdir(), qa_requires() or any
# other qa_*/testthat helper at the top level of this file. testthat sources
# every helper-*.R file alphabetically before any test runs, and a load-time
# call that skips or aborts would take every stage of the suite down with it
# (see helper-00-qa.R's own header for the incident this rule comes from).
# Every function here does its work when CALLED from inside a test, not when
# this file is sourced.

# The app's dependencies. Callers still route through qa_requires() /
# qa_skip_unless_app_installed() themselves so the skip is recorded against
# the calling test, not swallowed here.
qapp_required_packages <- function() c("shiny", "bslib", "DT")

# Directory of the bundled Shiny app, or "" if this copy of the package was
# built or installed without inst/ (a stripped build). Never aborts --
# callers decide what an empty result means.
qapp_app_dir <- function() {
  dir <- tryCatch(
    system.file("shiny", "dta_app", package = "DTAtools"),
    error = function(e) ""
  )
  if (nzchar(dir) && file.exists(file.path(dir, "app.R"))) dir else ""
}

# Skip the calling test, with a real reason, unless the app and every
# package it needs are both available. Returns the app directory invisibly
# so a caller can chain straight into shiny::testServer(qapp_skip_unless_app_installed(), ...).
qapp_skip_unless_app_installed <- function() {
  qa_requires(qapp_required_packages())
  dir <- qapp_app_dir()
  if (!nzchar(dir)) {
    testthat::skip("the application is not installed with this copy of the package")
  }
  invisible(dir)
}

# The app's helper .R files, in the order Shiny would source them.
qapp_helper_files <- function(dir = qapp_app_dir()) {
  sort(list.files(file.path(dir, "R"), pattern = "[.][Rr]$", full.names = TRUE))
}

# Cache, so the app's ~19,000 lines of helper code are parsed once per R
# session rather than once per test_that() block. Mirrors
# tests/testthat/helper-shinyapp.R's .shiny_env_cache exactly, as an
# independent binding -- this file cannot share that one across install
# boundaries.
.qapp_env_cache <- new.env(parent = emptyenv())

# Source the app's helper files into a fresh environment and return it.
#
# The environment's parent is shiny's namespace, reproducing the scope the
# app itself runs under: app.R starts with library(shiny), and the helper
# files call a handful of shiny/htmltools UI functions unqualified. See
# tests/testthat/helper-shinyapp.R's app_env() for the fuller rationale this
# is a direct port of.
#
# Returns the same environment on repeated calls within one R session.
# Treat it as read-only.
qapp_env <- function() {
  if (!is.null(.qapp_env_cache$env)) {
    return(.qapp_env_cache$env)
  }
  dir <- qapp_app_dir()
  if (!nzchar(dir)) {
    cli::cli_abort("Could not locate the Shiny app directory; call qapp_skip_unless_app_installed() first.")
  }
  env <- new.env(parent = asNamespace("shiny"))
  # Several app helpers locate bundled assets with an unqualified
  # system.file(package = "DTAtools"). Bind whichever system.file() is
  # correct in this session (pkgload's shim under devtools::test(), base's
  # own under R CMD check / an installed copy) so those helpers resolve
  # their assets either way.
  env$system.file <- system.file
  for (f in qapp_helper_files(dir)) {
    sys.source(f, envir = env, keep.source = FALSE)
  }
  .qapp_env_cache$env <- env
  env
}

# Fetch one function from the app helper environment by name. Fails loudly
# when the function does not exist, so a renamed app helper is a clear
# failure here rather than a confusing "attempt to apply non-function"
# further down in a test.
qapp_fn <- function(name) {
  fn <- get0(name, envir = qapp_env(), inherits = FALSE)
  if (!is.function(fn)) {
    cli::cli_abort("app helper {.fn {name}} is not defined under {.path inst/shiny/dta_app/R}.")
  }
  fn
}

# A shiny fileInput value for a bundled extdata fixture. fileInput() hands
# the server a one-row data.frame; the upload observers read `datapath` and
# `name` off it, so a fixture offered to session$setInputs() has to arrive
# the same way. Used for both data uploads (up_<i>_<j>) and specification
# uploads (dta_file) -- both observers read only name/datapath.
qapp_file_input <- function(filename) {
  path <- system.file("extdata", filename, package = "DTAtools")
  if (!nzchar(path)) {
    cli::cli_abort("{.file {filename}} is missing from inst/extdata.")
  }
  qapp_upload_value(path, filename)
}

.qapp_upload_counter <- new.env(parent = emptyenv())

# Stage file(s) as Shiny would have received them, and return the fileInput
# value (a data frame: name, size, type, datapath). The app acts on a file
# input only when every datapath is an existing file, not a directory, at
# <tempdir()>/<24 lowercase hex>/<index><.ext>, the layout of Shiny's own upload
# context (REQ-APP-050), so a fixture path handed over directly is a value the
# application correctly refuses.
#
# WHY THIS WRITES UNDER tempdir() AND NOT qa_tempdir(): qa_tempdir() is a
# "qa-*" folder below tempdir(), so a file in it is two levels down and is not
# an upload location. The upload folder has to be a direct child of tempdir(),
# exactly where Shiny puts it on a target system; the R session's temporary
# directory is writable there and is removed with the session. The folder name
# comes from a counter, not the random stream, because the suite seeds that
# stream deliberately and a drawn name would repeat across stages. The counter
# advances until dir.create() succeeds, so a helper file sourced twice in one R
# session (which resets the counter) never reuses or trips over an existing
# folder. `name` sets the extension (as the client's file name does in Shiny).
# Vectorised over `path`/`name`, order preserved.
qapp_upload_value <- function(path, name = basename(path)) {
  repeat {
    n <- (if (is.null(.qapp_upload_counter$n)) 0L else .qapp_upload_counter$n) + 1L
    .qapp_upload_counter$n <- n
    dir <- file.path(tempdir(), sprintf("%024x", n))
    if (dir.create(dir, showWarnings = FALSE)) break
  }
  ext <- tools::file_ext(name)
  ext <- ifelse(ext == "", "", paste0(".", ext))
  staged <- file.path(dir, paste0(seq_along(path) - 1L, ext))
  stopifnot(all(file.copy(path, staged)))
  data.frame(
    name = name, size = file.size(staged),
    type = "", datapath = staged, stringsAsFactors = FALSE
  )
}

# The rendered HTML of a renderUI output, as one string ("" for nothing).
qapp_ui_text <- function(out) {
  if (is.null(out) || is.null(out$html)) "" else paste(as.character(out$html), collapse = "")
}

# What a browser reports for the controls of rendered HTML before the user
# touches any of them, as a named list for session$setInputs(). testServer()
# has no browser, so an input a test sets is whatever the test says; a test
# that names an operator itself can never see the operator the browser would
# have reported. A <select> reports the option marked `selected`, or its FIRST
# option when none is marked (what it does for a `selected` value that is not
# among its options); a multiple <select> reports its selected options; a text
# <input> reports its `value` attribute, "" when it has none.
qapp_browser_inputs <- function(html) {
  qa_requires("xml2")
  doc <- xml2::read_html(paste0("<body>", html, "</body>"))
  out <- list()
  for (sel in xml2::xml_find_all(doc, "//select")) {
    id <- xml2::xml_attr(sel, "id")
    if (is.na(id)) next
    opts <- xml2::xml_find_all(sel, "./option")
    chosen <- opts[!is.na(xml2::xml_attr(opts, "selected"))]
    if (!is.na(xml2::xml_attr(sel, "multiple"))) {
      out[[id]] <- as.character(xml2::xml_attr(chosen, "value"))
      next
    }
    pick <- if (length(chosen) > 0) chosen[[1]] else opts[[1]]
    out[[id]] <- xml2::xml_attr(pick, "value")
  }
  for (inp in xml2::xml_find_all(doc, "//input[@type='text']")) {
    id <- xml2::xml_attr(inp, "id")
    if (is.na(id)) next
    value <- xml2::xml_attr(inp, "value")
    out[[id]] <- if (is.na(value)) "" else value
  }
  out[lengths(out) > 0L]
}

# The app autosaves to a path in tempdir() keyed to a client-reported
# browser id. A file left by an earlier test would put the next server under
# test into "previous session available" state; clear before, not after, so
# isolation holds regardless of run order. Ported from clean_session_file()
# in helper-shinyapp.R.
qapp_clean_session_files <- function() {
  f <- list.files(tempdir(), pattern = "^dtatools_app_session.*\\.rds$", full.names = TRUE)
  unlink(f, force = TRUE)
  invisible(f)
}

# Make the calling test's next servers read the app configuration in `lines`
# (the dta-app.yaml of the only template library), or none at all when `lines`
# is NULL. The configuration is read when a server STARTS, so this has to run
# before shiny::testServer(); shiny::testServer() sources the application
# directory afresh, so its configuration cache starts empty each time and the
# new value is seen. The library, the environment variables and the option are
# all undone when the calling test ends.
#
# Mirrors the development suite's with_app_library(), which is not installed
# with the package. DTATOOLS_TEMPLATE_INCLUDE_BUILTIN is false so that the
# packaged templates do not join the library; the remaining variables are
# cleared so that a machine that exports them does not redirect the run.
qapp_with_library <- function(lines = NULL, env = parent.frame()) {
  lib <- qa_tempdir(env)
  if (!is.null(lines)) {
    writeLines(c("kind: dta_app_config", lines), file.path(lib, "dta-app.yaml"))
  }
  withr::local_envvar(
    c(
      DTATOOLS_TEMPLATE_SOURCES = paste0("qualification=dir:", lib),
      DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = "false",
      DTATOOLS_TEMPLATE_REFRESH_SECONDS = NA,
      DTATOOLS_TEMPLATE_CACHE_DIR = NA
    ),
    .local_envir = env
  )
  withr::local_options(list(DTAtools.template_dir = NULL), .local_envir = env)
  qapp_clean_session_files()
  invisible(lib)
}

# Drive the app's "Create new version" flow -- the route that unlocks
# editing for a document LOADED from an existing one (an upload or a bundled
# example arrives read-only; see the WHY comment on editing() in app.R).
qapp_unlock_editing <- function(session, version = "9.9") {
  session$setInputs(create_new_version = 1)
  session$setInputs(new_version_value = version)
  session$setInputs(new_version_confirm = 1)
}

# Enter editing via "Enable edit mode", without creating a version entry.
qapp_enter_edit_mode <- function(session) session$setInputs(enable_edit_mode = 1)

# Leave editing via "Stop editing".
qapp_leave_edit_mode <- function(session) session$setInputs(stop_editing = 1)

# Temporarily make base::requireNamespace() report `pkgs` as unavailable,
# for exactly the duration of `code`, restoring the original binding no
# matter how `code` exits (normal return or condition).
#
# WHY THIS AND NOT testthat::local_mocked_bindings(): that function rebinds
# a name inside the TARGET package's own namespace/import table, which works
# fine for shiny::runApp() directly in a test (DTAtools calls it namespaced,
# so local_mocked_bindings(runApp = ..., .package = "shiny") is enough) but
# not for requireNamespace() -- a base function called unqualified from
# inside run_dta_app(). Verified on this build:
# local_mocked_bindings(requireNamespace = ..., .package = "DTAtools")
# aborts with "Can't find binding for `requireNamespace`", because base's
# exports are not tracked as an ordinary package import. Rebinding the name
# directly in the base namespace is the only way found to exercise
# run_dta_app()'s missing-package branch honestly -- without uninstalling a
# suggested package from the machine this suite runs on, which this
# function must never do.
#
# The substitute delegates to the REAL requireNamespace() for every package
# not named in `pkgs`, so nothing else evaluated during the window (cli's
# own formatting, shiny's own startup checks) sees a different answer than
# it would have gotten anyway.
qapp_with_packages_unavailable <- function(pkgs, code) {
  ns <- asNamespace("base")
  orig <- base::requireNamespace
  unlocked <- tryCatch(
    {
      unlockBinding("requireNamespace", ns)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!unlocked) {
    testthat::skip("base::requireNamespace is locked on this build and cannot be substituted")
  }
  withr::defer({
    assign("requireNamespace", orig, envir = ns)
    lockBinding("requireNamespace", ns)
  })
  assign(
    "requireNamespace",
    function(package, ...) {
      if (identical(as.character(package)[1], NA_character_)) {
        return(orig(package, ...))
      }
      if (as.character(package)[1] %in% pkgs) FALSE else orig(package, ...)
    },
    envir = ns
  )
  force(code)
}

# The vocabulary selections a browser would have supplied.
#
# A creation template may declare vocabulary slots with a `min:`, and the app
# reads each slot's chosen terms from an input the browser creates when the
# options dialog renders. `shiny::testServer()` has no browser, so those
# inputs never exist -- and an ABSENT input is not the same answer as an
# unopened slot: the app reads it as "deliberately none" (see
# vocabulary_slot_values(), which distinguishes NULL from character(0) so that
# a slot WITH a default can still be emptied on purpose). A slot with min >= 1
# then refuses, and the document is never created.
#
# Setting each slot to its own declared default is what the rendered dialog
# does, so this reproduces the browser rather than working around it. Reading
# the slots from the session means it stays correct for whatever template the
# caller picked, rather than hard-coding one template's slot names.
qapp_fill_template_vocab <- function(session, rv) {
  slots <- rv$template_vocab_specs$slots %||% list()
  for (rec in slots) {
    if (!is.null(rec$error)) {
      next
    }
    default <- as.character(rec$slot$default %||% character(0))
    if (length(default) == 0) {
      next
    }
    args <- stats::setNames(list(default), paste0("tmpl_vocab_", rec$slot$id))
    do.call(session$setInputs, args)
  }
  invisible(length(slots))
}
