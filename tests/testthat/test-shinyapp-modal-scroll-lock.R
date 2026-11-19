# modal_scroll_lock_js (app.R) -- gives the page its scrolling back once the last
# dialog has closed.
#
# THE FAILURE THIS FILE PINS: Bootstrap 5 locks the page while a modal is open
# (inline `overflow: hidden` and `padding-right` on <body>, plus `modal-open`)
# and unlocks it by restoring what it SAW when the modal opened. Shiny's
# showModal() over a dialog that is still on screen replaces the dialog without
# hiding the first, so the second one sees the first one's lock, files it away
# as the page's own style and restores it on close. What the user gets is a
# page with no scrollbar and a dead mouse wheel, taller than the window, until
# a reload -- "no vertical scrolling after the YAML is loaded". The natural
# way in is the "Could not load" dialog: a corrected YAML that still fails to
# load shows the dialog again over itself (finish_file_load), and the next
# correction, which loads, closes it. Any of the dozens of other showModal()
# calls can do it too, and it stays invisible until a loaded document is taller
# than the window.
#
# Reproduced in a real browser (headless Edge, real mouse events) twice: with
# a second showModal() while the example dialog was open and then "Load", and
# with the load-failure retry above. Both left `overflow: hidden` on <body>
# with no modal in the page, and (in the first, 985px of content in a 650px
# window) a wheel that moved nothing. With the snippet the same sequences end
# with an empty body style and a wheel that scrolls.
# On 2026-11-19 the same lock followed Load example, Create new and Create
# from template whenever the dialog was requested a second time while open;
# with the snippet each scrolled to the bottom.
#
# These are SOURCE-LEVEL assertions, for the reason test-shinyapp-modal-outputs.R
# gives: the behaviour is JavaScript against a DOM, and nothing in this test
# suite starts a browser (chromote and shinytest2 are used by the qualification
# suite only), so the snippet cannot run here. What is pinned is each property
# the fix rests on; a regression in any one leaves the page locked again,
# silently, with every other test still green.

skip_if_not_installed("shiny")

lock_js <- function() app_r_binding("modal_scroll_lock_js")

test_that("the snippet is registered in the app's head", {
  # A snippet that is never rendered into the page fixes nothing, and the
  # symptom is identical to not having written it.
  src <- app_source("app.R")
  # Anchored on the ui_page() definition: a bare `tags$head(` also appears in
  # comments earlier in the file.
  head_block <- sub(
    "(?s)^.*?\nui_page <- function.*?tags\\$head\\((.*?)\\n *\\),.*$",
    "\\1", src,
    perl = TRUE
  )

  expect_false(identical(head_block, src)) # the head block was actually found
  expect_match(head_block, "tags$script(shiny::HTML(modal_scroll_lock_js))", fixed = TRUE)
})

test_that("it listens through jQuery, because the native 'hidden' never reaches document", {
  js <- lock_js()
  expect_type(js, "character")

  # Shiny removes the dialog from the page inside its own jQuery 'hidden'
  # handler, and Bootstrap dispatches the native event only afterwards, from a
  # node that is no longer in the document: a document.addEventListener()
  # handler never fires, and the page stays locked while looking fixed. Seen
  # happen -- the first version of this snippet did exactly that.
  expect_match(js, "$(document).on('hidden.bs.modal'", fixed = TRUE)
  expect_no_match(js, "addEventListener('hidden.bs.modal'", fixed = TRUE)
})

test_that("it waits for Bootstrap's own restore before clearing the lock", {
  js <- lock_js()

  # Bootstrap restores the saved (wrong) style BEFORE it fires 'hidden', so the
  # clear lands after it either way. The deferral gives the page one tick to
  # settle -- other 'hidden' listeners have run and a dialog the server opened
  # in the meantime has put its backdrop up -- before the guard below looks.
  expect_match(js, "setTimeout(function () {", fixed = TRUE)
})

test_that("it unlocks only when no dialog is left, so an open dialog stays locked", {
  js <- lock_js()

  # A dialog the server opens just after another one closes (the template
  # picker's removeModal(); showModal() hand-over does this on purpose) can have
  # its backdrop up by the time this runs. Unlocking then would let the page
  # scroll behind an open dialog. Both halves of the selector matter:
  # `.modal.show` alone misses a dialog still fading in, `.modal-backdrop`
  # alone misses one whose backdrop is disabled.
  expect_match(js, "document.querySelector('.modal.show, .modal-backdrop')", fixed = TRUE)
  expect_match(js, "if (document.querySelector('.modal.show, .modal-backdrop')) return;", fixed = TRUE)
})

test_that("it clears every trace of the lock, not just the visible style", {
  js <- lock_js()

  # The class and the two inline properties are what hide the scrollbar. The
  # two data attributes are Bootstrap's saved "original" values; leaving them
  # behind plants the same wrong style in the next dialog's restore.
  expect_match(js, "classList.remove('modal-open')", fixed = TRUE)
  expect_match(js, "style.removeProperty('overflow')", fixed = TRUE)
  expect_match(js, "style.removeProperty('padding-right')", fixed = TRUE)
  expect_match(js, "removeAttribute('data-bs-overflow')", fixed = TRUE)
  expect_match(js, "removeAttribute('data-bs-padding-right')", fixed = TRUE)
})

test_that("the snippet survives being embedded in the page", {
  js <- lock_js()

  # An unescaped "</script>" would close the tag early and spill the rest into
  # the document as text -- silently, since the page still renders.
  expect_no_match(js, "</script", fixed = TRUE)
})
