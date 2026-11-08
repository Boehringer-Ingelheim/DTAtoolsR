test_that("OQ-APP-029 | agreement metadata edits preserve the document and respect read-only mode | REQ-APP-022", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = qapp_file_input("clinical_data.csv"))
    session$setInputs(check_all = 1)
    qapp_enter_edit_mode(session)

    # The extension is authored test input, not an expected value harvested
    # from a saved document. Editing a known field must leave it alone.
    rv$dta <- qapp_fn("dta_set_transmission_field")(
      rv$dta, "vendor_extension", list(route = "retain")
    )$value
    saved_datasets <- DTAtools::datasets(rv$dta)
    saved_status <- rv$status
    session$setInputs(
      md_header = "Qualification header",
      md_study_id = "QUAL-STUDY-001", md_protocol_id = "QUAL-PROTOCOL-001",
      tr_extent = "All scheduled visits", tr_scope = "Laboratory data",
      tr_packaging = "One archive per transfer", tr_security = "Encrypted transfer",
      tr_checksum = "SHA-256", tr_test_requirements = "Send test.\n\nObtain approval.\n",
      tr_blinding_requirements = "Keep treatment allocation concealed.",
      tr_test_upload = "no", tr_blinded = "yes"
    )
    session$elapse(1000)
    md <- DTAtools::metadata(rv$dta)
    qa_step(
      "study and protocol identity are saved separately from the presentation header",
      list(id = "QUAL-STUDY-001", protocol_id = "QUAL-PROTOCOL-001"),
      md@study
    )
    qa_step("the header remains a presentation field", "Qualification header", md@header)
    fields <- c("extent", "scope", "packaging", "security", "checksum", "test_requirements", "blinding_requirements")
    qa_step(
      "all additional transmission fields retain the supplied text and paragraphs",
      list(
        extent = "All scheduled visits", scope = "Laboratory data",
        packaging = "One archive per transfer", security = "Encrypted transfer",
        checksum = "SHA-256", test_requirements = "Send test.\n\nObtain approval.\n",
        blinding_requirements = "Keep treatment allocation concealed."
      ),
      md@transmission[fields]
    )
    qa_step(
      "custom transmission extensions remain intact",
      list(route = "retain"), md@transmission[["vendor_extension"]]
    )
    qa_step(
      "existing yes and no choices remain logical flags",
      list(test_upload = FALSE, blinded_transfer = TRUE),
      md@transmission[c("test_upload", "blinded_transfer")]
    )
    qa_step(
      "the uploaded first subject is still present",
      "SUBJ0001",
      as.data.frame(DTAtools::datasets(rv$dta)[["clinical_data"]]@tables[[1]])$SUBJECT_ID[[1]]
    )
    qa_check(
      "metadata edits change neither the bound datasets nor their validation state",
      identical(saved_datasets, DTAtools::datasets(rv$dta)) && identical(saved_status, rv$status)
    )

    session$setInputs(md_study_id = "", tr_scope = "  \n ")
    session$elapse(1000)
    qa_step(
      "clearing one study field removes it while keeping its sibling",
      list(protocol_id = "QUAL-PROTOCOL-001"), DTAtools::metadata(rv$dta)@study
    )
    qa_step(
      "blank transmission input removes the field",
      FALSE, "scope" %in% names(DTAtools::metadata(rv$dta)@transmission)
    )

    qapp_leave_edit_mode(session)
    saved_document <- rv$dta
    session$setInputs(
      md_study_id = "Blocked", md_protocol_id = "Blocked",
      tr_extent = "Blocked", tr_scope = "Blocked", tr_packaging = "Blocked",
      tr_security = "Blocked", tr_checksum = "Blocked",
      tr_test_requirements = "Blocked", tr_blinding_requirements = "Blocked"
    )
    session$elapse(1000)
    qa_check("read-only server handlers reject metadata writes", identical(saved_document, rv$dta))
    html <- as.character(output$metadata_editor$html)
    qa_step(
      "read-only metadata shows the retained values without study or transmission controls",
      c(protocol = TRUE, security = TRUE, study_input = FALSE, transmission_input = FALSE),
      c(
        protocol = grepl("QUAL-PROTOCOL-001", html, fixed = TRUE),
        security = grepl("Encrypted transfer", html, fixed = TRUE),
        study_input = grepl('id="md_study_id"', html, fixed = TRUE),
        transmission_input = grepl('id="tr_security"', html, fixed = TRUE)
      )
    )
  })
})

test_that("OQ-APP-030 | ordered agreement section editing is safe and renders plain text | REQ-APP-023", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    qapp_enter_edit_mode(session)
    session$setInputs(agreement_add = 1)
    session$setInputs(agreement_id = "scope", agreement_title = "Scope", agreement_text = "First.\n\nSecond.\n")
    session$setInputs(agreement_save = 1)
    session$setInputs(agreement_add = 2)
    session$setInputs(agreement_id = "security", agreement_title = "Security", agreement_text = "")
    session$setInputs(agreement_save = 2)
    qa_step(
      "new sections append in order and blank draft text is allowed",
      list(
        scope = list(title = "Scope", text = "First.\n\nSecond.\n"),
        security = list(title = "Security", text = "")
      ),
      DTAtools::metadata(rv$dta)@sections
    )
    session$setInputs(agreement_up_security = 1)
    qa_step(
      "moving a section up changes only the declared order",
      c("security", "scope"), names(DTAtools::metadata(rv$dta)@sections)
    )
    session$setInputs(agreement_edit_scope = 1)
    session$setInputs(
      agreement_id = "scope", agreement_title = "<em>Scope</em>",
      agreement_text = "<script>unsafe()</script>\n\nAfter.\n"
    )
    session$setInputs(agreement_save = 3)
    qa_step(
      "editing after a move still targets the stable identifier and retains its sibling",
      list(
        security = list(title = "Security", text = ""),
        scope = list(title = "<em>Scope</em>", text = "<script>unsafe()</script>\n\nAfter.\n")
      ),
      DTAtools::metadata(rv$dta)@sections
    )
    html <- as.character(output$agreement_sections$html)
    qa_step(
      "section title and text are escaped, with paragraph-preserving display",
      c(title = TRUE, text = TRUE, executable = FALSE, paragraphs = TRUE),
      c(
        title = grepl("&lt;em&gt;Scope&lt;/em&gt;", html, fixed = TRUE),
        text = grepl("&lt;script&gt;unsafe()&lt;/script&gt;\n\nAfter.\n", html, fixed = TRUE),
        executable = grepl("<script>unsafe()", html, fixed = TRUE),
        paragraphs = grepl("white-space:pre-wrap", html, fixed = TRUE)
      )
    )

    errors <- character()
    testthat::local_mocked_bindings(
      showNotification = function(ui, type = "default", ...) {
        if (identical(type, "error")) errors <<- c(errors, as.character(ui))
        invisible(NULL)
      },
      .package = "shiny"
    )
    saved_document <- rv$dta
    session$setInputs(agreement_add = 3)
    session$setInputs(agreement_id = "invalid id", agreement_title = "Title", agreement_text = "Draft")
    session$setInputs(agreement_save = 4)
    qa_check("an invalid identifier leaves the document unchanged", identical(saved_document, rv$dta))
    session$setInputs(agreement_id = "scope")
    session$setInputs(agreement_save = 5)
    qa_check("a duplicate identifier does not overwrite an existing section", identical(saved_document, rv$dta))
    session$setInputs(agreement_id = "new_valid", agreement_title = " ")
    session$setInputs(agreement_save = 6)
    qa_check("a blank title leaves the document unchanged", identical(saved_document, rv$dta))
    qa_step("each invalid save reports an error", 3L, length(errors))

    session$setInputs(agreement_edit_scope = 2)
    session$setInputs(agreement_remove_scope = 1)
    qapp_leave_edit_mode(session)
    session$setInputs(agreement_id = "scope", agreement_title = "Blocked", agreement_text = "Blocked")
    session$setInputs(
      agreement_save = 7, agreement_remove_confirm = 1, agreement_add = 4,
      agreement_edit_scope = 3, agreement_remove_scope = 2,
      agreement_up_security = 2, agreement_down_security = 1
    )
    qa_check("read-only mode blocks every section mutation handler", identical(saved_document, rv$dta))
    html <- as.character(output$agreement_sections$html)
    qa_step(
      "read-only sections display retained text without mutation buttons",
      c(content = TRUE, buttons = FALSE),
      c(
        content = grepl("&lt;script&gt;unsafe()&lt;/script&gt;", html, fixed = TRUE),
        buttons = grepl("<button", html, fixed = TRUE)
      )
    )

    qapp_enter_edit_mode(session)
    session$setInputs(agreement_down_security = 2)
    qa_step(
      "moving down restores the declared order",
      c("scope", "security"), names(DTAtools::metadata(rv$dta)@sections)
    )
    session$setInputs(agreement_remove_scope = 3)
    qa_step(
      "requesting deletion does not remove anything before confirmation",
      c("scope", "security"), names(DTAtools::metadata(rv$dta)@sections)
    )
    session$setInputs(agreement_remove_confirm = 2)
    qa_step(
      "confirmation removes only the named section",
      list(security = list(title = "Security", text = "")),
      DTAtools::metadata(rv$dta)@sections
    )
  })
})
