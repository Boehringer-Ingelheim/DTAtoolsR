# modal_reattach_js (app.R) -- never puts a replaced dialog back on the page.
#
# THE FAILURE THIS FILE PINS: Bootstrap 5 shows a dialog only once its
# backdrop has faded in, from a callback (_backdrop.show(() =>
# _showElement())), and _showElement() appends the dialog to <body> if it is
# no longer in the page. Shiny's showModal() over a dialog that is still
# opening swaps it out of #shiny-modal-wrapper without hiding it, so the
# replaced dialog came back as a second copy on top of the real one, its
# inputs unbound: a Load or Create button that did nothing.
#
# Reproduced on 2026-11-19 in a real browser (headless Edge, real mouse
# events) twice: with two requests for the example dialog 100 ms apart, and
# with the page's outgoing messages held for seven seconds while Load example
# was clicked twice -- the click guard let the button go after five seconds,
# the second click went through, and the server answered both at once. Both
# left two #shiny-modal in the page, the top one directly under <body>, and a
# Load that did nothing. With the snippet: one dialog, and Load loads.
#
# These are SOURCE-LEVEL assertions, for the reason test-shinyapp-modal-outputs.R
# gives: nothing in this suite starts a browser. The last test is a canary on
# the Bootstrap that bslib bundles: the snippet rests on a private method, and
# an upgrade that dropped it would turn the fix into a silent no-op.

skip_if_not_installed("shiny")

reattach_js <- function() app_r_binding("modal_reattach_js")

test_that("the snippet is registered in the app's head", {
  # A snippet that is never rendered into the page fixes nothing.
  src <- app_source("app.R")
  # Anchored on the ui_page() definition: a bare `tags$head(` also appears in
  # comments earlier in the file.
  head_block <- sub(
    "(?s)^.*?\nui_page <- function.*?tags\\$head\\((.*?)\\n *\\),.*$",
    "\\1", src,
    perl = TRUE
  )

  expect_false(identical(head_block, src)) # the head block was actually found
  expect_match(head_block, "tags$script(shiny::HTML(modal_reattach_js))", fixed = TRUE)
})

test_that("it takes hold of each Shiny dialog as it starts to show", {
  js <- reattach_js()
  expect_type(js, "character")

  # Delegated from document through jQuery: show.bs.modal only reaches
  # document for a dialog that is in the page, which is what keeps
  # Bootstrap's own not-yet-inserted dialogs out of this.
  expect_match(js, "$(document).on('show.bs.modal', '#shiny-modal'", fixed = TRUE)
  expect_match(js, "bootstrap.Modal", fixed = TRUE)
  expect_match(js, ".getInstance(", fixed = TRUE)
})

test_that("a dialog no longer in the page is left alone, not re-attached", {
  js <- reattach_js()

  # The whole fix: a dialog that has left the page by the time its backdrop
  # has faded in was replaced, and must not come back.
  expect_match(js, "if (!document.body.contains(this._element)) return;", fixed = TRUE)
  # Every other dialog is shown exactly as Bootstrap would show it.
  expect_match(js, "showElement.apply(this, arguments)", fixed = TRUE)
})

test_that("a renamed Bootstrap method degrades to a no-op, and nothing is wrapped twice", {
  js <- reattach_js()

  expect_match(js, "typeof inst._showElement !== 'function'", fixed = TRUE)
  expect_match(js, "inst.dtaNoReattach", fixed = TRUE)
})

test_that("the snippet survives being embedded in the page", {
  # An unescaped "</script>" would close the tag early and spill the rest into
  # the document as text -- silently, since the page still renders.
  expect_no_match(reattach_js(), "</script", fixed = TRUE)
})

test_that("the bundled Bootstrap still shows a dialog the way the snippet assumes", {
  # If this fails, Bootstrap changed how a dialog is shown: check in a browser
  # whether a dialog requested twice in quick succession still leaves a second
  # copy on top, then update modal_reattach_js or delete it.
  skip_on_cran()
  bundle <- system.file("lib/bs5/dist/js/bootstrap.bundle.min.js", package = "bslib")
  skip_if(!nzchar(bundle), "bslib's Bootstrap 5 bundle was not found")
  js <- paste(readLines(bundle, warn = FALSE), collapse = "\n")

  expect_match(js, "this._showElement(", fixed = TRUE)
  expect_match(js, "document.body.contains(this._element)", fixed = TRUE)
  expect_match(js, "document.body.append(this._element)", fixed = TRUE)
})
