# modal_scroll_lock_js and modal_reattach_js (app.R), run in a real browser.
#
# test-shinyapp-modal-scroll-lock.R and test-shinyapp-modal-reattach.R pin the
# SOURCE of the two snippets. A string match cannot tell a snippet that works
# from one that only reads right, and this bug has already had a fix that read
# right and did nothing (a native 'hidden.bs.modal' listener, which never
# fires). It was also reported three times, twice after it had been "fixed" on
# a branch. So this file runs the snippets exactly as app.R defines them
# against the real thing -- Shiny's own showModal() on a Bootstrap 5 page, in
# headless Chrome -- and reads the one property that decides whether the user
# can scroll: the page's computed `overflow-y` once the dialog has gone.
# (Scrolling the window from script would prove nothing: that still works on
# a page the user cannot scroll.)
#
# THE FAILURE: Bootstrap 5 locks the page while a dialog is open and unlocks
# it by restoring what it SAW when the dialog opened. Shiny's showModal() over
# a dialog that is still on screen replaces it without closing it, so the
# second dialog sees the first one's lock, takes it for the page's own style
# and restores it on close: no scrollbar, a dead mouse wheel, content below
# the fold out of reach until a reload. The way in is a dialog requested twice
# -- a landing button clicked again because a busy server had not answered
# yet (the click guard lets a button go after five seconds), or any handler
# that opens a dialog over an open one.
#
# The page under test is a stand-in rather than the app: one tall div and a
# server that opens a numbered dialog whenever it is asked, which is all the
# failure needs. That the app really carries the snippets in its <head> is
# pinned by the two source-level files; the same sequences were also
# reproduced against the app itself when this file was written.

skip_on_cran()
skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("shinytest2")
skip_if_not_installed("chromote")
skip_if(
  is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)),
  "no Chrome to run the dialogs in"
)

# A Shiny app in a temporary directory, with `snippets` in its <head>, and a
# browser on it. The app is a directory rather than an object so that nothing
# of this test's environment (and through it the package under test) has to
# be rebuilt in the background process that serves it.
#
# A page that fails to start is a FAILURE here, not a skip: AppDriver skips by
# itself when there is no browser to drive, and anything else that stops the
# page from coming up is exactly what this file is for.
modal_page <- function(snippets, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  saveRDS(snippets, file.path(dir, "snippets.rds"))
  writeLines(c(
    "snippets <- readRDS('snippets.rds')",
    "shiny::shinyApp(",
    "  ui = bslib::page_fluid(",
    "    theme = bslib::bs_theme(version = 5),",
    "    shiny::tags$head(lapply(snippets, function(js) shiny::tags$script(shiny::HTML(js)))),",
    "    shiny::div(style = 'height: 3000px;')",
    "  ),",
    "  server = function(input, output, session) {",
    "    shiny::observeEvent(input$open, {",
    "      shiny::showModal(shiny::modalDialog(",
    "        paste('dialog', input$open),",
    "        footer = shiny::modalButton('Cancel')",
    "      ))",
    "    })",
    "  }",
    ")"
  ), file.path(dir, "app.R"))

  app <- suppressMessages(shinytest2::AppDriver$new(
    dir,
    width = 900, height = 600, load_timeout = 30000
  ))
  withr::defer(app$stop(), envir = env)

  # What the steps below wait on, instead of sleeping and hoping. A dialog is
  # marked once it is fully on screen (Bootstrap ignores a Cancel that arrives
  # mid-fade). 'hidden' is counted a moment late on purpose: the snippet clears
  # the lock from a zero-delay timer of its own, and the count must not get
  # ahead of it.
  app$run_js("
    window.dtaHidden = 0;
    $(document).on('shown.bs.modal', function (e) {
      e.target.setAttribute('data-dta-shown', '1');
    });
    $(document).on('hidden.bs.modal', function () {
      setTimeout(function () { window.dtaHidden++; }, 100);
    });
  ")
  app
}

request_dialog <- function(app, n) {
  app$run_js(sprintf("Shiny.setInputValue('open', %d, {priority: 'event'});", n))
}

# Dialog number `n` is the one in Shiny's wrapper and has finished opening.
wait_for_dialog <- function(app, n) {
  app$wait_for_js(
    sprintf(
      "(function () {
         var m = document.querySelector('#shiny-modal-wrapper .modal[data-dta-shown]');
         return !!m && m.textContent.indexOf('dialog %d') > -1;
       })()",
      n
    ),
    timeout = 15000
  )
}

cancel_dialog <- function(app) {
  hidden <- app$get_js("window.dtaHidden")
  app$run_js("document.querySelector('#shiny-modal-wrapper [data-bs-dismiss]').click();")
  app$wait_for_js(
    sprintf(
      "window.dtaHidden > %d && !document.querySelector('.modal.show, .modal-backdrop')",
      hidden
    ),
    timeout = 15000
  )
}

# The dialog, then a second one while the first is fully on screen, then
# Cancel.
open_over_open_then_cancel <- function(app) {
  request_dialog(app, 1)
  wait_for_dialog(app, 1)
  request_dialog(app, 2)
  wait_for_dialog(app, 2)
  cancel_dialog(app)
}

page_overflow <- function(app) app$get_js("getComputedStyle(document.body).overflowY")

test_that("the steps really do lock a page that has no snippets", {
  # The control. Without it the test below could pass because the steps had
  # stopped reproducing anything -- a Shiny or Bootstrap release that closes
  # the replaced dialog properly would do that. If THIS starts failing, that
  # has happened: confirm in a browser, then modal_scroll_lock_js can go.
  app <- modal_page(list())

  expect_identical(page_overflow(app), "visible")
  open_over_open_then_cancel(app)

  expect_identical(app$get_js("document.querySelectorAll('.modal, .modal-backdrop').length"), 0L)
  expect_identical(page_overflow(app), "hidden")
})

test_that("with the snippets the page scrolls again once the dialog has gone", {
  lock_js <- app_r_binding("modal_scroll_lock_js")
  reattach_js <- app_r_binding("modal_reattach_js")
  # A binding app.R no longer defines comes back as NA, and a page built from
  # NA would only be the control again.
  expect_match(lock_js, "hidden.bs.modal", fixed = TRUE)
  expect_match(reattach_js, "_showElement", fixed = TRUE)
  app <- modal_page(list(lock_js, reattach_js))

  open_over_open_then_cancel(app)

  expect_identical(page_overflow(app), "visible")
  # Every trace of the lock, not just the one that hides the scrollbar: a
  # leftover padding shifts the page, and a leftover data attribute plants the
  # wrong style in the next dialog's restore.
  expect_identical(
    app$get_js("[document.body.style.overflow, document.body.style.paddingRight,
                 document.body.getAttribute('data-bs-overflow'),
                 document.body.getAttribute('data-bs-padding-right'),
                 document.body.classList.contains('modal-open')]"),
    list("", "", NULL, NULL, FALSE)
  )

  # Two requests answered in the same instant: the second dialog arrives while
  # the first is still fading in. Bootstrap would put the replaced one back on
  # the page as a dead copy (modal_reattach_js), and the lock the replaced one
  # took must go with the survivor. The replaced dialog would come back when
  # its backdrop has faded in, which is before the survivor has finished
  # opening, so the count below is taken late enough. No control for this
  # half: whether the two answers land inside one 150 ms fade is up to the
  # machine, and where they do not, the steps are the ones above.
  app$run_js("
    Shiny.setInputValue('open', 3, {priority: 'event'});
    Shiny.setInputValue('open', 4, {priority: 'event'});
  ")
  wait_for_dialog(app, 4)
  expect_identical(app$get_js("document.querySelectorAll('.modal').length"), 1L)

  cancel_dialog(app)
  expect_identical(page_overflow(app), "visible")
})
