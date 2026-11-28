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

test_that("OQ-APP-031 | template suggestions preserve editable input | REQ-APP-024", {
  qapp_skip_unless_app_installed()
  html <- paste(as.character(qapp_fn("render_template_option_input")(list(
    id = "scope", label = "Delivery scope", type = "textarea",
    allow_custom = TRUE,
    choices = c("Complete data", "Incremental data"),
    default = "Complete data"
  ))), collapse = "")

  # A one-line text option that permits custom values has the same shape, with
  # a text input in place of the textarea.
  text_html <- paste(as.character(qapp_fn("render_template_option_input")(list(
    id = "freq", label = "Transfer frequency", type = "text",
    choices = c("weekly", "monthly"),
    default = "weekly"
  ))), collapse = "")

  qa_step(
    "suggested one-line controls retain the original editable text input and map blank to empty",
    c(
      text_input = TRUE, suggestions = TRUE, blank = TRUE,
      bubbling_input = TRUE, sentinel_not_stored = TRUE
    ),
    c(
      text_input = grepl('<input id="tmpl_opt_freq" type="text"', text_html, fixed = TRUE) &&
        grepl('value="weekly"', text_html, fixed = TRUE),
      suggestions = grepl('id="tmpl_opt_freq_suggestion"', text_html, fixed = TRUE) &&
        grepl("weekly", text_html, fixed = TRUE) &&
        grepl("monthly", text_html, fixed = TRUE),
      blank = grepl("(leave blank)", text_html, fixed = TRUE),
      bubbling_input = grepl("dispatchEvent(new Event('input', { bubbles: true }))", text_html, fixed = TRUE),
      sentinel_not_stored = grepl("__blank__", text_html, fixed = TRUE) &&
        grepl("value ===", text_html, fixed = TRUE) &&
        grepl("? '' :", text_html, fixed = TRUE)
    )
  )

  qa_step(
    "suggested narrative controls retain the original editable multiline input and map blank to empty",
    c(
      textarea = TRUE, suggestions = TRUE, blank = TRUE,
      bubbling_input = TRUE, custom_text = TRUE, sentinel_not_stored = TRUE
    ),
    c(
      textarea = grepl('id="tmpl_opt_scope"', html, fixed = TRUE) &&
        grepl('rows="3"', html, fixed = TRUE),
      suggestions = grepl('id="tmpl_opt_scope_suggestion"', html, fixed = TRUE) &&
        grepl("Complete data", html, fixed = TRUE) &&
        grepl("Incremental data", html, fixed = TRUE),
      blank = grepl("(leave blank)", html, fixed = TRUE),
      bubbling_input = grepl("dispatchEvent(new Event('input', { bubbles: true }))", html, fixed = TRUE),
      custom_text = grepl("<textarea", html, fixed = TRUE),
      sentinel_not_stored = grepl("__blank__", html, fixed = TRUE) &&
        grepl("value ===", html, fixed = TRUE) &&
        grepl("? '' :", html, fixed = TRUE)
    )
  )
})

test_that("OQ-APP-085 | the Word export through a template tells the user which placeholder stayed in the document, and still delivers it | REQ-APP-066", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()
  lib <- qapp_with_library(c(
    "exports:", "  bundled_word_templates: false", "  document: [word_template]"
  ))
  dir.create(file.path(lib, "export-templates"))
  # One placeholder the exporter can fill, one block placeholder that names a
  # dataset the document does not contain, so it stays as written.
  template <- officer::read_docx()
  template <- officer::body_add_par(template, "Agreement {DTA_TITLE}", style = "Normal")
  template <- officer::body_add_par(template, "{COLUMN_SPECS:NOPE}", style = "Normal")
  template <- officer::body_add_par(template, "Hello {NOT_A_FIELD}", style = "Normal")
  print(template, target = file.path(lib, "export-templates", "house.docx"))
  # And one the exporter fills completely.
  clean <- officer::read_docx()
  clean <- officer::body_add_par(clean, "Agreement {DTA_TITLE}", style = "Normal")
  clean <- officer::body_add_par(clean, "{COLUMN_SPECS}", style = "Normal")
  print(clean, target = file.path(lib, "export-templates", "clean.docx"))

  notices <- list()
  testthat::local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      notices[[length(notices) + 1L]] <<- list(text = paste(as.character(ui), collapse = ""), type = type)
      invisible(NULL)
    },
    .package = "shiny"
  )

  delivered <- NA
  delivered_clean <- NA
  warned_house <- list()
  warned_clean <- list()
  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("r", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    session$setInputs(
      export_format = "word", export_word_mode = "custom",
      export_template_select = "library:house.docx", export_include_yaml_word = FALSE
    )
    session$setInputs(export_do = 1)
    delivered <<- !is.null(export_state$file_path) && file.exists(export_state$file_path)
    warned_house <<- Filter(function(n) identical(n$type, "warning"), notices)

    # The same session, a template that fills completely: no warning.
    notices <<- list()
    session$setInputs(export_template_select = "library:clean.docx")
    session$setInputs(export_do = 2)
    delivered_clean <<- !is.null(export_state$file_path) && file.exists(export_state$file_path)
    warned_clean <<- Filter(function(n) identical(n$type, "warning"), notices)
  })

  qa_step("a block placeholder and an inline one each raise a warning notification", 2L, length(warned_house))
  texts <- paste(vapply(warned_house, function(n) n$text, ""), collapse = " ")
  qa_check(
    "the notifications name both placeholders that stayed in the document",
    grepl("{COLUMN_SPECS:NOPE}", texts, fixed = TRUE) && grepl("{NOT_A_FIELD}", texts, fixed = TRUE)
  )
  qa_step("and the document is still delivered", TRUE, delivered)
  qa_step("a template that fills completely shows no warning", 0L, length(warned_clean))
  qa_step("and its document is delivered", TRUE, delivered_clean)
})
