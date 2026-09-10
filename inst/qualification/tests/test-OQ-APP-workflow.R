# The longer journeys through the application. REQ-APP-014 .. REQ-APP-018.
#
# A user does not exercise one behaviour at a time. They load a specification,
# bind a delivery, check it and export a report; or they build a specification
# from a template, pick a party profile, choose permitted values from a
# vocabulary, and cut a version. Each case here follows one of those journeys
# end to end through the real server function.

qw_bundled <- function(name) {
  path <- system.file("extdata", name, package = "DTAtools")
  if (!nzchar(path)) {
    testthat::skip(paste0("bundled example not installed: ", name))
  }
  path
}

test_that("OQ-APP-020 | the report is refused until something has been checked | REQ-APP-014", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()
  dir <- qa_tempdir()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("m", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = qapp_file_input("clinical_data_error_all.csv"))

    # Nothing has been checked yet. A report produced now would carry an
    # overview with no verdict in it, which reads as a delivery nobody found
    # anything wrong with.
    unchecked <- tryCatch(
      {
        output$dl_msgs_html
        "produced"
      },
      error = function(e) "refused"
    )
    qa_step("a report before any check is refused", "refused", unchecked)

    session$setInputs(check_all = 1)

    produced <- output$dl_msgs_html
    written <- file.path(dir, "report.html")
    file.copy(produced, written, overwrite = TRUE)
    qa_check("after a check the report is produced", file.exists(written))

    parsed <- qa_html_report(written)
    qa_step(
      "it is well-formed HTML naming the dataset that was checked",
      TRUE, grepl("clinical", paste(readLines(written, warn = FALSE), collapse = " "), fixed = TRUE)
    )
    qa_step(
      "and it is standalone, fetching nothing over the network",
      character(0), parsed$external_refs
    )
    qa_check("while reporting the findings the check made", parsed$n_messages > 0)
  })
})

test_that("OQ-APP-021 | a document built from a template takes the template's metadata | REQ-APP-015", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("n", 32))
    session$setInputs(create_from_template = 1)

    # The bundled templates are what a first-time user is offered. If none is
    # installed there is nothing to qualify here.
    index <- tryCatch(qapp_fn("dta_template_index_cached")(), error = function(e) NULL)
    if (is.null(index) || !is.data.frame(index) || nrow(index) == 0) {
      testthat::skip("no templates are installed with this copy of the package")
    }
    creation <- index[index$kind == "dta_creation_template", , drop = FALSE]
    if (nrow(creation) == 0) {
      testthat::skip("no creation template is installed with this copy of the package")
    }

    invisible(output$template_picker_ui)
    session$setInputs(
      template_select_name = creation$id[[1]],
      template_select_version = creation$version[[1]]
    )
    session$setInputs(template_select_next = 1)
    qapp_fill_template_vocab(session, rv)
    session$setInputs(tmpl_carry_source = "none")
    session$setInputs(template_create_confirm = 1)

    qa_check("a document is created", !is.null(rv$dta))
    # The point of a template is that the agreed boilerplate arrives already
    # filled in. A document that came back empty would leave the user typing
    # what the template exists to supply.
    meta <- metadata(rv$dta)
    qa_check(
      "carrying metadata from the template rather than nothing",
      nzchar(meta@title %||% "") || length(meta@supplier %||% list()) > 0
    )
  })
})

test_that("OQ-APP-022 | a vocabulary supplies a column's permitted values | REQ-APP-017", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("o", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    qapp_enter_edit_mode(session)

    # A vocabulary reference that resolves to nothing must report rather than
    # raise: an editing session that died on a missing vocabulary would lose
    # whatever else the user had entered.
    session$setInputs(col_vocab_id = "no_such_vocabulary")
    outcome <- tryCatch(
      {
        session$setInputs(col_vocab_apply = 1)
        "reported"
      },
      error = function(e) "raised"
    )
    qa_step(
      "an unresolvable vocabulary reports rather than raising",
      "reported", outcome
    )
    qa_check("and the document survives it", !is.null(rv$dta))
  })
})

test_that("OQ-APP-023 | creating a version appends exactly one history entry | REQ-APP-018", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("p", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))

    before <- nrow(get_version_history_df(metadata(rv$dta)))
    qapp_unlock_editing(session, version = "9.9")

    after <- get_version_history_df(metadata(rv$dta))
    qa_step(
      "exactly one entry is appended",
      before + 1L, nrow(after)
    )
    qa_step(
      "and the document records the new version",
      "9.9", as.character(metadata(rv$dta)@version)
    )
    qa_step(
      "which is the version the new entry names",
      "9.9", as.character(after$version[[nrow(after)]])
    )
  })
})

test_that("OQ-APP-024 | a blank version is refused and changes nothing | REQ-APP-018", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("q", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))

    version_before <- as.character(metadata(rv$dta)@version)
    history_before <- nrow(get_version_history_df(metadata(rv$dta)))

    session$setInputs(create_new_version = 1)
    session$setInputs(new_version_value = "   ")
    session$setInputs(new_version_confirm = 1)

    # A blank version accepted would put an unnamed release into the document's
    # history, and the history is what a reader uses to tell one agreed
    # specification from another.
    qa_step(
      "the version is unchanged",
      version_before, as.character(metadata(rv$dta)@version)
    )
    qa_step(
      "no history entry is appended",
      history_before, nrow(get_version_history_df(metadata(rv$dta)))
    )
    qa_check(
      "and the refusal is reported to the user",
      isFALSE(rv$new_version_msg$ok)
    )
  })
})

test_that("OQ-APP-025 | a template that extends another carries the parent forward | REQ-APP-016", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("r", 32))
    session$setInputs(create_from_template = 1)

    index <- tryCatch(qapp_fn("dta_template_index_cached")(), error = function(e) NULL)
    if (is.null(index) || !is.data.frame(index) || nrow(index) == 0) {
      testthat::skip("no templates are installed with this copy of the package")
    }
    extending <- index[
      index$kind == "dta_creation_template" &
        !is.na(index$extends) & nzchar(as.character(index$extends)), ,
      drop = FALSE
    ]
    if (nrow(extending) == 0) {
      testthat::skip("no installed creation template extends another")
    }

    invisible(output$template_picker_ui)
    session$setInputs(
      template_select_name = extending$id[[1]],
      template_select_version = extending$version[[1]]
    )
    session$setInputs(template_select_next = 1)
    qapp_fill_template_vocab(session, rv)
    session$setInputs(tmpl_carry_source = "none")
    session$setInputs(template_create_confirm = 1)

    qa_check("a document is created from the extending template", !is.null(rv$dta))

    # A child template states only what it changes. If the parent's content did
    # not come through, every template would have to be a full copy of the one
    # it extends, and the inheritance would be decorative.
    meta <- metadata(rv$dta)
    carried <- nzchar(meta@title %||% "") ||
      length(meta@supplier %||% list()) > 0 ||
      length(meta@receiver %||% list()) > 0
    qa_check("carrying metadata inherited through the chain", carried)
  })
})
