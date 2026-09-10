# The contact editor's repeat-request guard (edit_contact_flow(), app.R).
#
# A double-click is normally swallowed in the browser by click_guard_script()
# (ui_components.R), but that guard releases a link if the server has not
# even acknowledged the click within its hold -- which a CPU-starved server
# can fail to do. Every further click a waiting user makes then reaches the
# server in one burst, and each showModal() in that burst would tear the
# dialog down and put it back. The server therefore keeps its own record of
# which contact's dialog is open (rv$editing_contact) and drops a request for
# the one already showing. That record is cleared by Save and by the
# browser's modal-state report (dta_modal_state, sent by modal_state_js for
# every shown/hidden Bootstrap modal event), so a dialog the user closed, or
# one another dialog replaced, can be opened again.

skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

# Same file-local server helpers as test-shinyapp-edit-mode.R: each testServer
# file carries its own copies rather than sharing them through a helper file.
app_server_dir <- function() .shiny_app_dir()

app_file_input <- function(filename) {
  path <- app_fixture_path(filename)
  data.frame(
    name = filename, size = file.size(path), type = "",
    datapath = path, stringsAsFactors = FALSE
  )
}

clean_session_file <- function() {
  f <- list.files(tempdir(),
    pattern = "^dtatools_app_session.*\\.rds$", full.names = TRUE
  )
  unlink(f, force = TRUE)
  invisible(f)
}

load_fixture <- function(session) {
  session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
}

test_that("a repeat request for the open contact dialog is dropped, and a closed one can be reopened", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)

    shown <- 0L
    last_ui <- NULL
    testthat::local_mocked_bindings(
      showModal = function(ui, session = NULL) {
        shown <<- shown + 1L
        last_ui <<- ui
        invisible(NULL)
      },
      .package = "shiny"
    )

    # First click: the dialog opens, stamped so the browser's report can
    # tell it from any other dialog.
    session$setInputs(editc_receiver_1 = 1)
    expect_equal(shown, 1L)
    expect_equal(rv$editing_contact, list(side = "receiver", index = 1L))
    expect_match(
      paste(as.character(last_ui), collapse = "\n"),
      'data-dta-modal="edit_contact"',
      fixed = TRUE
    )

    # A repeat click while it is open: dropped, not re-shown.
    session$setInputs(editc_receiver_1 = 2)
    expect_equal(shown, 1L)

    # The browser reports the dialog closed (Cancel, Esc, backdrop) ...
    session$setInputs(dta_modal_state = list(event = "hidden", kind = "edit_contact", at = 1))
    expect_null(rv$editing_contact)
    # ... so the next click opens it again.
    session$setInputs(editc_receiver_1 = 3)
    expect_equal(shown, 2L)
    expect_equal(rv$editing_contact, list(side = "receiver", index = 1L))

    # The dialog's own 'shown' report keeps the record ...
    session$setInputs(dta_modal_state = list(event = "shown", kind = "edit_contact", at = 2))
    expect_equal(rv$editing_contact, list(side = "receiver", index = 1L))
    # ... and any other dialog taking its place clears it.
    session$setInputs(dta_modal_state = list(event = "shown", kind = "", at = 3))
    expect_null(rv$editing_contact)
    session$setInputs(editc_receiver_1 = 4)
    expect_equal(shown, 3L)
  })
})

test_that("a request for a different contact replaces the open dialog rather than being dropped", {
  # Only the SAME contact is a repeat. clinical_dta.yaml's receiver has more
  # than one contact, so the second dialog is a real, different request.
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)
    expect_gt(length(app_fn("dta_contacts")(rv$dta, "receiver")), 1)

    shown <- 0L
    testthat::local_mocked_bindings(
      showModal = function(ui, session = NULL) {
        shown <<- shown + 1L
        invisible(NULL)
      },
      .package = "shiny"
    )

    session$setInputs(editc_receiver_1 = 1)
    session$setInputs(editc_receiver_2 = 1)
    expect_equal(shown, 2L)
    expect_equal(rv$editing_contact, list(side = "receiver", index = 2L))
  })
})

test_that("a successful Save clears the open-dialog record", {
  # Save is the one close the server performs itself, so it must not rely on
  # the browser's report to make the contact editable again.
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)

    shown <- 0L
    testthat::local_mocked_bindings(
      showModal = function(ui, session = NULL) {
        shown <<- shown + 1L
        invisible(NULL)
      },
      .package = "shiny"
    )

    session$setInputs(editc_receiver_1 = 1)
    session$setInputs(
      edit_contact_name = "Alice Smith-Jones", edit_contact_roles = "Lead Data Manager",
      edit_contact_email = "", edit_contact_department = "",
      edit_contact_phone = "", edit_contact_address = ""
    )
    session$setInputs(confirm_edit_contact = 1)
    expect_null(rv$editing_contact)
    expect_equal(app_fn("dta_contacts")(rv$dta, "receiver")[[1]]$name, "Alice Smith-Jones")

    session$setInputs(editc_receiver_1 = 2)
    expect_equal(shown, 2L)
  })
})

test_that("the browser reports both modal events and the UI loads the reporter", {
  src <- app_source("app.R")
  expect_match(src, "$(document).on('shown.bs.modal hidden.bs.modal', '#shiny-modal'", fixed = TRUE)
  expect_match(src, "Shiny.setInputValue('dta_modal_state'", fixed = TRUE)
  expect_match(src, "tags$script(shiny::HTML(modal_state_js))", fixed = TRUE)
})
