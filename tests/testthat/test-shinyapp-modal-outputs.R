# modal_unsuspend_js (app.R) -- the one-line-of-DOM fix that keeps an output
# inside a modalDialog() alive.
#
# THE FAILURE THIS FILE PINS: Shiny suspends an output whose element looked
# hidden at the moment the client bound it, and only re-checks that when
# something fires an event it listens for (shown.bs.tab for a nav_panel,
# shown.bs.collapse for an accordion). Nothing re-checks a modal. Bootstrap 5
# then guarantees the bad snapshot -- Modal.show() hands _showElement() to the
# backdrop as a completion callback, so .modal() returns with the dialog still
# display:none and Shiny binds the whole thing inside that gap. Every output in
# the dialog is recorded hidden and suspended for good.
#
# Concretely: the "Create new from template" picker opened with an EMPTY body,
# because output$template_picker_ui -- the uiOutput holding the picker body,
# at the time two dropdowns, today the search box and the nested list and
# detail outputs -- was suspended and its HTML never sent. "Next" then found
# no selection and reported it. Nothing on the server was wrong, which is
# exactly why the whole existing testServer() suite passed throughout.
#
# These are SOURCE-LEVEL assertions, which this suite otherwise avoids, for the
# same reason test-shinyapp-click-guard.R gives: the behaviour under test is
# JavaScript against a DOM, and the package's dependency set carries no JS
# engine (no V8, no chromote, no shinytest2), so the snippet cannot be executed
# here at all. What is pinned instead is each structural property the fix rests
# on -- a regression in any one of them puts the modals back to blank, silently
# and with every other test still green.

Sys.setenv(NOT_CRAN = "true")

skip_if_not_installed("shiny")

# app.R is not part of app_env() (which sources only inst/shiny/dta_app/R), so
# the string is lifted out of the parsed source rather than fetched by name.
# Returns NA when the binding is gone, which every test below asserts on: a
# renamed or deleted snippet must fail loudly, not pass vacuously.
app_r_binding <- function(name) {
  exprs <- parse(text = app_source("app.R"), keep.source = FALSE)
  for (e in exprs) {
    if (is.call(e) && identical(as.character(e[[1]]), "<-") &&
      identical(as.character(e[[2]]), name)) {
      return(eval(e[[3]]))
    }
  }
  NA_character_
}

modal_js <- function() app_r_binding("modal_unsuspend_js")

# Every `uiOutput("id")` / `textOutput("id")` written directly into a
# modalDialog() call in app.R, found by walking the parsed source rather than
# by grepping text. A LOWER bound on what the fix has to rescue: a modal whose
# body is assembled into a variable first (the export modal does this) is not
# reachable this way, and outputs nested inside a modal body's own render are
# not visible in the source at all.
modal_output_ids <- function() {
  ids <- character(0)
  collect_outputs <- function(e) {
    if (!is.call(e)) {
      return(invisible(NULL))
    }
    head <- as.character(e[[1]])[[1]]
    if (head %in% c("uiOutput", "textOutput") && length(e) >= 2 && is.character(e[[2]])) {
      ids <<- c(ids, e[[2]])
    }
    for (i in seq_along(e)[-1]) {
      if (!identical(e[[i]], quote(expr = ))) collect_outputs(e[[i]])
    }
    invisible(NULL)
  }
  walk <- function(e) {
    if (!is.call(e)) {
      return(invisible(NULL))
    }
    if (identical(as.character(e[[1]])[[1]], "modalDialog")) {
      collect_outputs(e)
      return(invisible(NULL))
    }
    for (i in seq_along(e)) {
      if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
    }
    invisible(NULL)
  }
  for (e in parse(text = app_source("app.R"), keep.source = FALSE)) walk(e)
  unique(ids)
}

test_that("the fix is load-bearing: app.R still puts outputs straight into modalDialog()", {
  # Not a test of the snippet but of its REASON to exist. Every id listed here
  # renders blank without it. If a future refactor moves all of these out of
  # modals, this test is the signal that modal_unsuspend_js can go too --
  # rather than it lingering as a snippet nobody dares delete.
  ids <- modal_output_ids()

  expect_true("template_picker_ui" %in% ids)
  expect_true("create_new_body" %in% ids)
  expect_true("col_modal_body" %in% ids)
  expect_gte(length(ids), 5)
})

test_that("the snippet is registered in the app's head", {
  # A snippet that is never rendered into the page fixes nothing, and the
  # symptom is identical to not having written it.
  src <- app_source("app.R")
  head_block <- sub("(?s)^.*?tags\\$head\\((.*?)\\n  \\),.*$", "\\1", src, perl = TRUE)

  expect_false(identical(head_block, src)) # the head block was actually found
  expect_match(head_block, "modal_unsuspend_js", fixed = TRUE)
})

test_that("it hooks the one event that fires after the dialog is really on screen", {
  js <- modal_js()
  expect_type(js, "character")

  # shown.bs.modal, not show.bs.modal: the latter fires BEFORE the dialog is
  # displayed, which is the very state that produced the bad snapshot -- so
  # dropping the "n" would leave every modal exactly as broken as before while
  # looking correct.
  expect_match(js, "'shown.bs.modal'", fixed = TRUE)
  expect_no_match(js, "'show.bs.modal'", fixed = TRUE)

  # Delegated from `document`, because the modal is inserted into the page
  # long after this script runs -- a listener attached to a specific dialog
  # element at load time would have nothing to attach to.
  expect_match(js, "document.addEventListener('shown.bs.modal'", fixed = TRUE)
})

test_that("it writes the exact clientdata input Shiny's suspendWhenHidden reads", {
  js <- modal_js()

  # This name is Shiny's own protocol, not ours: outputs are suspended on
  # `.clientdata_output_<id>_hidden`. A typo in it is silent -- an unknown
  # input is simply stored and never consulted, so the modal stays blank and
  # nothing anywhere reports an error.
  expect_match(js, "'.clientdata_output_' + el.id + '_hidden'", fixed = TRUE)
})

test_that("it re-measures visibility rather than asserting it", {
  js <- modal_js()

  # The value sent is computed, not the literal `false`. Hard-coding "not
  # hidden" would work on every modal in the app TODAY and quietly break the
  # first one that hides an output on purpose -- a conditionalPanel, or a
  # second step of a two-step body -- by rendering something meant to stay
  # suspended. This is the assertion that keeps the sweep a corrected
  # snapshot instead of an override.
  expect_match(js, "_hidden', hidden(el))", fixed = TRUE)
  expect_no_match(js, "_hidden', false)", fixed = TRUE)

  # And it is measured the way Shiny measures it: display:none anywhere up the
  # ancestor chain. Not offsetParent (null for any position:fixed element) and
  # not a size test (a zero-width container is still "shown").
  expect_match(js, "window.getComputedStyle(n).display === 'none'", fixed = TRUE)
  expect_match(js, "n = n.parentElement", fixed = TRUE)
})

test_that("it sweeps every bound output in the dialog, not one known id", {
  js <- modal_js()

  # The dialog carries more than its body: the inline *_msg outputs, and
  # whatever the body's own render inserted before the fade finished. Scoped
  # to ev.target so a modal's sweep never touches outputs elsewhere on the
  # page -- re-measuring a nav_panel tab nobody has opened is wasted work at
  # best, and the snapshot this listener exists to distrust at worst.
  expect_match(js, "ev.target.querySelectorAll('.shiny-bound-output')", fixed = TRUE)
})

test_that("the snippet survives being embedded in the page", {
  js <- modal_js()

  # An unescaped "</script>" would close the tag early and spill the rest into
  # the document as text -- silently, since the page still renders.
  expect_no_match(js, "</script", fixed = TRUE)
  # The listener is installed from tags$head(), so it can fire before Shiny's
  # own JS has initialised; touching Shiny then would throw and kill the
  # handler for the rest of the session.
  expect_match(js, "typeof Shiny === 'undefined'", fixed = TRUE)
})
