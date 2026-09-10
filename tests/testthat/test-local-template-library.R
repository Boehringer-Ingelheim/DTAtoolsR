# Opt-in coverage for a template library kept OUTSIDE this repository, on the
# developer's own machine. Everything else in the suite tests the template
# engine against fixtures it writes itself; this file tests it against a real,
# hand-maintained directory of templates, which is the arrangement
# vignette("private-templates") actually describes and the one that breaks in
# ways a temp-directory fixture cannot reproduce (a template edited months ago
# against an older engine, a party profile whose slot was later renamed).
#
# Inert unless DTATOOLS_LOCAL_LIBRARY names an existing directory, so CI and
# every other developer skip it. Set it in an untracked `.Renviron` at the
# repository root:
#
#     DTATOOLS_LOCAL_LIBRARY=C:/path/to/your/template-library
#
# Deliberately NOT DTATOOLS_TEMPLATE_SOURCES. That variable is live -- exported
# from `.Renviron` it would flip the whole session into private-only mode
# (dta_template_source_roots()'s "private replaces public" contract), dropping
# the packaged directory that test-shinyapp-template.R and
# test-bundled-templates.R are about. This file sets the live variable itself,
# scoped to the one test that needs it.

Sys.setenv(NOT_CRAN = "true")

# The configured library, or "" when the developer has not set one up.
local_template_library <- function() {
  lib <- trimws(Sys.getenv("DTATOOLS_LOCAL_LIBRARY", unset = ""))
  if (nzchar(lib) && dir.exists(lib)) lib else ""
}

skip_without_local_library <- function() {
  skip_if(
    !nzchar(local_template_library()),
    "DTATOOLS_LOCAL_LIBRARY is unset or does not name an existing directory"
  )
}

# ---- the templates themselves ----------------------------------------------

test_that("every template in the local library lints and instantiates", {
  skip_without_local_library()
  lib <- local_template_library()

  # What makes this worth running is that validate_template() does not merely
  # parse the YAML: it resolves `extends:`, imports dataset templates, expands
  # vocabularies and runs a real create_dta_from_template() dry-run
  # (R/validateTemplate.R). A zero-error result means the templates BUILD, not
  # just that they are well-formed.
  #
  # strict = FALSE deliberately, and it weakens nothing: `strict` gates only
  # the closing .dta_template_strict_check() abort (validateTemplate.R), never
  # which checks run, so the rows are identical either way. With strict = TRUE
  # the function cli_abort()s on the first error instead of returning, which
  # would make the assertion below unreachable on exactly the path it exists
  # for -- and its message, which names the offending templates, dead code.
  result <- validate_template(lib, strict = FALSE)
  errors <- result[result$severity == "error", c("file", "code", "message"), drop = FALSE]

  # Name the offending templates in the failure message -- "0 is not 3" would
  # otherwise send the reader back to the command line to find out which file
  # broke.
  expect_equal(
    nrow(errors), 0L,
    info = paste0(
      "validate_template('", lib, "') reported errors:\n",
      paste(
        sprintf("  %s [%s] %s", basename(errors$file), errors$code, errors$message),
        collapse = "\n"
      )
    )
  )
})

# ---- the integration -------------------------------------------------------

test_that("the app's index discovers the local library through a dir: source", {
  skip_without_local_library()
  lib <- local_template_library()

  invalidate <- app_fn("dta_template_index_invalidate")

  # Invalidate on BOTH sides. Before, so no earlier test's index is reused;
  # after, so this library cannot leak into a later test file through the
  # 900-second TTL cache -- dta_template_index_cached() re-reads only when the
  # TTL expires, and a whole suite run finishes well inside it.
  invalidate()
  withr::defer(invalidate())
  withr::local_envvar(c(
    DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", lib),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = NA
  ))

  index <- app_fn("build_template_index")()

  expect_gt(nrow(index), 0L)

  # A warning here is the interesting failure: build_template_index() collects
  # unreadable files and kind/filename mismatches into an attribute rather than
  # erroring, so a library with one broken file still indexes -- silently,
  # unless something asserts on this.
  expect_identical(attr(index, "warnings"), character(0))

  # Every kind the library does contain is one the engine recognises -- a
  # subset, not expect_setequal(): this runs against whatever library the
  # developer configured, and one that happens to hold no party profile yet is
  # perfectly valid, not a defect worth failing over.
  expect_true(all(unique(index$kind) %in% app_fn("dta_template_all_kinds")()))

  # Creation templates specifically must be there, or the picker assertion
  # below passes for the wrong reason.
  expect_true("dta_creation_template" %in% index$kind)

  # The picker is the app's actual consumer: a template that indexes but never
  # reaches the picker (wrong kind, abstract, unresolvable lineage) is a bug
  # this would otherwise miss.
  entries <- app_fn("template_picker_entries")(index)
  expect_gt(nrow(entries), 0L)
})
