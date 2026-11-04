# The "Authorized for corrections" picker in the Metadata tab: contacts from
# both sides plus free-text names, saved as the whole selection.

skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

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

test_that("picking contacts and typing a name saves the whole selection; emptying unsets it", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)
    contact <- app_fn("dta_contacts")(rv$dta, "receiver")[[1]]$name
    expect_true(nzchar(contact))

    session$setInputs(md_authorized = c(contact, "Manual Person"))
    expect_identical(
      DTAtools::metadata(rv$dta)@authorized_for_corrections,
      c(contact, "Manual Person")
    )

    session$setInputs(md_authorized = NULL)
    expect_null(DTAtools::metadata(rv$dta)@authorized_for_corrections)
  })
})

test_that("the picker reporting the stored names in another order is not an edit", {
  # selectize reports its initial selection in option order (contacts first),
  # not in the stored order; opening the editor must not rewrite the document.
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)
    contact <- app_fn("dta_contacts")(rv$dta, "receiver")[[1]]$name
    session$setInputs(md_authorized = c("Manual Person", contact))
    stored <- DTAtools::metadata(rv$dta)@authorized_for_corrections
    expect_identical(stored, c("Manual Person", contact))

    session$setInputs(md_authorized = c(contact, "Manual Person"))
    expect_identical(DTAtools::metadata(rv$dta)@authorized_for_corrections, stored)
  })
})

test_that("adding a contact refreshes the picker's options and keeps the stored selection", {
  clean_session_file()
  # app.R itself runs library(shiny), so its unqualified updateSelectizeInput()
  # resolves through package:shiny; attaching it first lets the mock reach that
  # binding as well as the namespace one.
  suppressPackageStartupMessages(library(shiny))
  calls <- list()
  local_mocked_bindings(
    updateSelectizeInput = function(session, inputId, ...) {
      calls[[length(calls) + 1L]] <<- c(list(inputId = inputId), list(...))
      invisible()
    },
    .package = "shiny"
  )
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)
    session$setInputs(md_authorized = "Manual Person")
    calls <<- list()
    session$setInputs(new_contact_name = "Zed Newcomer", new_contact_roles = "")
    session$setInputs(confirm_add_receiver = 1)
  })
  hits <- Filter(function(x) identical(x$inputId, "md_authorized"), calls)
  expect_length(hits, 1L)
  expect_true("Zed Newcomer" %in% hits[[1]]$choices$Receiver)
  expect_identical(
    hits[[1]]$choices[["Entered manually"]],
    c("Manual Person" = "Manual Person")
  )
  expect_identical(hits[[1]]$selected, "Manual Person")
})

test_that("the editor offers contact names as options and a multi-select", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)
    contact <- app_fn("dta_contacts")(rv$dta, "receiver")[[1]]$name
    html <- as.character(output$metadata_editor$html)
    expect_match(html, 'id="md_authorized"', fixed = TRUE)
    expect_match(html, "multiple", fixed = TRUE)
    expect_match(html, contact, fixed = TRUE)
  })
})

test_that("read-only shows every value comma-joined and renders no picker", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)
    session$setInputs(md_authorized = c("Ann Example", "Bob Example"))
    leave_edit_mode(session)
    html <- as.character(output$metadata_editor$html)
    expect_match(html, "Ann Example, Bob Example", fixed = TRUE)
    expect_no_match(html, 'id="md_authorized"', fixed = TRUE)
  })
})

test_that("an input arriving while read-only writes nothing", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    expect_false(editing())
    before <- DTAtools::metadata(rv$dta)@authorized_for_corrections
    session$setInputs(md_authorized = c("Intruder"))
    expect_identical(DTAtools::metadata(rv$dta)@authorized_for_corrections, before)
  })
})
