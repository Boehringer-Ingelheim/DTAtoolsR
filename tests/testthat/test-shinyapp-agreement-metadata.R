agreement_sections <- function(dta) S7::prop(DTAtools::metadata(dta), "sections")

agreement_fixture <- function() {
  dta <- app_fixture_dta_with_data(checked = TRUE)
  md <- DTAtools::metadata(dta)
  md@study <- list(id = "STUDY-1", protocol_id = "PROTO-1")
  md@transmission <- list(type = "SFTP", test_upload = FALSE, vendor_extension = list(keep = "yes"))
  md@sections <- list(
    scope = list(title = "Scope", text = "First paragraph.\n\nSecond paragraph.\n"),
    security = list(title = "Security", text = "Keep secure.")
  )
  dta@metadata <- md
  dta
}

test_that("agreement section helpers preserve order, paragraph breaks and all other data", {
  dta <- agreement_fixture()
  original <- dta
  save <- app_fn("dta_save_agreement_section")
  move <- app_fn("dta_move_agreement_section")
  remove <- app_fn("dta_remove_agreement_section")

  added <- save(dta, "delivery", "Delivery", "")
  expect_true(added$ok)
  expect_identical(names(agreement_sections(added$value)), c("scope", "security", "delivery"))
  edited <- save(added$value, "scope", "Revised scope", " A paragraph.\n\nAnother.\n", previous_id = "scope")
  expect_true(edited$ok)
  expect_identical(agreement_sections(edited$value)$scope$text, " A paragraph.\n\nAnother.\n")
  expect_identical(agreement_sections(edited$value)$security, agreement_sections(dta)$security)

  moved <- move(edited$value, "delivery", "up")
  expect_true(moved$ok)
  expect_identical(names(agreement_sections(moved$value)), c("scope", "delivery", "security"))
  back <- move(moved$value, "delivery", "down")
  expect_identical(names(agreement_sections(back$value)), c("scope", "security", "delivery"))
  edge <- move(back$value, "scope", "up")
  expect_true(edge$ok)
  expect_identical(edge$value, back$value)
  renamed <- save(back$value, "delivery_notes", "Delivery", "", previous_id = "delivery")
  expect_true(renamed$ok)
  expect_identical(names(agreement_sections(renamed$value)), c("scope", "security", "delivery_notes"))
  removed <- remove(renamed$value, "security")
  expect_true(removed$ok)
  expect_identical(names(agreement_sections(removed$value)), c("scope", "delivery_notes"))
  expect_identical(DTAtools::datasets(removed$value), DTAtools::datasets(original))
  expect_identical(DTAtools::metadata(removed$value)@study, DTAtools::metadata(original)@study)
  expect_identical(DTAtools::metadata(removed$value)@transmission, DTAtools::metadata(original)@transmission)
  expect_identical(dta, original)
})

test_that("invalid section edits and stale identifiers fail without changing the DTA", {
  dta <- agreement_fixture()
  original <- dta
  save <- app_fn("dta_save_agreement_section")
  for (id in c("", "1bad", "two words", "scope")) {
    result <- save(dta, id, "Title", "Text")
    expect_false(result$ok, info = id)
    expect_true(nzchar(result$error), info = id)
    expect_null(result$value)
  }
  expect_false(save(dta, "valid", " ", "Text")$ok)
  expect_false(save(dta, "valid", "Title", NA_character_)$ok)
  expect_false(save(dta, "scope", "Title", "Text", previous_id = "security")$ok)
  expect_false(save(dta, "valid", "Title", "Text", previous_id = "missing")$ok)
  expect_false(app_fn("dta_remove_agreement_section")(dta, "missing")$ok)
  expect_false(app_fn("dta_move_agreement_section")(dta, "missing", "up")$ok)
  expect_false(app_fn("dta_move_agreement_section")(dta, "scope", "sideways")$ok)
  expect_identical(dta, original)
})

test_that("study and transmission edits preserve siblings and remove blank optional fields", {
  dta <- agreement_fixture()
  study <- app_fn("dta_set_study_field")
  tr <- app_fn("dta_set_transmission_field")
  result <- study(dta, "id", "STUDY-2")
  expect_true(result$ok)
  expect_identical(DTAtools::metadata(result$value)@study, list(id = "STUDY-2", protocol_id = "PROTO-1"))
  expect_false(study(dta, "unknown", "bad")$ok)
  expect_false(study(dta, "id", c("A", "B"))$ok)
  result <- study(result$value, "id", "  ")
  expect_true(result$ok)
  expect_identical(DTAtools::metadata(result$value)@study, list(protocol_id = "PROTO-1"))

  for (field in c("extent", "scope", "packaging", "security", "checksum", "test_requirements", "blinding_requirements")) {
    result <- tr(result$value, field, "Paragraph one.\n\nParagraph two.\n")
    expect_true(result$ok)
    expect_identical(DTAtools::metadata(result$value)@transmission[[field]], "Paragraph one.\n\nParagraph two.\n")
  }
  result <- tr(result$value, "scope", "  \n ")
  expect_true(result$ok)
  expect_null(DTAtools::metadata(result$value)@transmission[["scope"]])
  expect_identical(DTAtools::metadata(result$value)@transmission$vendor_extension, list(keep = "yes"))
  expect_identical(DTAtools::metadata(result$value)@transmission$test_upload, FALSE)
  expect_identical(DTAtools::datasets(result$value), DTAtools::datasets(dta))
})

test_that("agreement edits survive raw YAML and existing template metadata paths", {
  dta <- agreement_fixture()
  edited <- app_fn("dta_save_agreement_section")(dta, "scope", "Scope", "One.\n\nTwo.\n", previous_id = "scope")$value
  edited <- app_fn("apply_template_metadata_path")(edited, "metadata.sections.scope.text", "Override.\n\nRetained.\n")
  edited <- app_fn("apply_template_metadata_path")(edited, "metadata.study.id", "STUDY-3")
  yaml <- app_fn("dta_to_yaml_text")(edited)
  expect_true(yaml$ok, info = yaml$error)
  parsed <- app_fn("dta_read_yaml_text")(yaml$value)
  expect_true(parsed$ok, info = parsed$error)
  expect_identical(agreement_sections(parsed$value), agreement_sections(edited))
  expect_identical(DTAtools::metadata(parsed$value)@study$id, "STUDY-3")
  expect_identical(DTAtools::metadata(parsed$value)@transmission$vendor_extension, list(keep = "yes"))
  expect_identical(DTAtools::datasets(edited), DTAtools::datasets(dta))
})

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

agreement_file_input <- function(filename) {
  path <- app_fixture_path(filename)
  data.frame(name = filename, size = file.size(path), type = "", datapath = path, stringsAsFactors = FALSE)
}

test_that("the real section editor adds, edits, reorders and confirms deletion by stable ID", {
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = agreement_file_input("clinical_dta.yaml"))
    enter_edit_mode(session)
    session$setInputs(up_1_1 = agreement_file_input("clinical_data.csv"))
    session$setInputs(check_all = 1)
    original_datasets <- DTAtools::datasets(rv$dta)
    original_status <- rv$status
    original_uploads <- rv$uploads
    session$setInputs(agreement_add = 1)
    session$setInputs(agreement_id = "scope", agreement_title = "Scope", agreement_text = "One.\n\nTwo.\n")
    session$setInputs(agreement_save = 1)
    expect_identical(agreement_sections(rv$dta)$scope$text, "One.\n\nTwo.\n")
    session$setInputs(agreement_add = 2)
    session$setInputs(agreement_id = "security", agreement_title = "Security", agreement_text = "")
    session$setInputs(agreement_save = 2)
    expect_identical(names(agreement_sections(rv$dta)), c("scope", "security"))
    session$setInputs(agreement_up_security = 1)
    expect_identical(names(agreement_sections(rv$dta)), c("security", "scope"))
    session$setInputs(agreement_edit_scope = 1)
    session$setInputs(agreement_id = "scope", agreement_title = "<script>Title</script>", agreement_text = "<b>Text</b>\n\nAfter.")
    session$setInputs(agreement_save = 3)
    expect_identical(agreement_sections(rv$dta)$scope$title, "<script>Title</script>")
    html <- as.character(output$agreement_sections$html)
    expect_match(html, "&lt;script&gt;Title&lt;/script&gt;", fixed = TRUE)
    expect_match(html, "&lt;b&gt;Text&lt;/b&gt;\n\nAfter.", fixed = TRUE)
    expect_match(html, "white-space:pre-wrap", fixed = TRUE)
    session$setInputs(raw_yaml_editor = gsub("<b>Text</b>", "Edited in YAML", rv$yaml_text, fixed = TRUE))
    session$setInputs(apply_yaml = 1)
    expect_true(rv$yaml_msg$ok, info = rv$yaml_msg$error)
    expect_identical(agreement_sections(rv$dta)$scope$text, "Edited in YAML\n\nAfter.")
    session$setInputs(agreement_down_security = 1)
    expect_identical(names(agreement_sections(rv$dta)), c("scope", "security"))
    session$setInputs(agreement_remove_scope = 1)
    expect_identical(names(agreement_sections(rv$dta)), c("scope", "security"))
    session$setInputs(agreement_remove_confirm = 1)
    expect_identical(names(agreement_sections(rv$dta)), "security")
    expect_identical(DTAtools::datasets(rv$dta), original_datasets)
    expect_identical(rv$status, original_status)
    expect_identical(rv$uploads, original_uploads)
    expect_identical(yaml::yaml.load(rv$yaml_text)$metadata$sections$security$text, "")
  })
})

test_that("invalid modal saves notify and preserve both document and entered values", {
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = agreement_file_input("clinical_dta.yaml"))
    enter_edit_mode(session)
    notices <- character()
    testthat::local_mocked_bindings(
      showNotification = function(ui, ...) {
        notices <<- c(notices, as.character(ui))
        invisible(NULL)
      },
      .package = "shiny"
    )
    before <- rv$dta
    session$setInputs(agreement_add = 1)
    session$setInputs(agreement_id = "not valid", agreement_title = "Title", agreement_text = "Draft")
    session$setInputs(agreement_save = 1)
    expect_identical(rv$dta, before)
    expect_true(any(nzchar(notices)))
    expect_identical(input$agreement_text, "Draft")
    session$setInputs(agreement_id = "scope", agreement_title = " ")
    session$setInputs(agreement_save = 2)
    expect_identical(rv$dta, before)
    expect_gte(length(notices), 2)
    session$setInputs(agreement_title = "Scope")
    session$setInputs(agreement_save = 3)
    before <- rv$dta
    session$setInputs(agreement_add = 2)
    session$setInputs(agreement_id = "scope", agreement_title = "Duplicate", agreement_text = "Must not replace")
    session$setInputs(agreement_save = 4)
    expect_identical(rv$dta, before)
    expect_gte(length(notices), 3)
    session$setInputs(md_study_id = c("invalid", "vector"))
    session$elapse(1000)
    expect_identical(rv$dta, before)
    expect_gte(length(notices), 4)
  })
})

test_that("study and expanded transmission autosave without dropping dataset data", {
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = agreement_file_input("clinical_dta.yaml"))
    enter_edit_mode(session)
    session$setInputs(up_1_1 = agreement_file_input("clinical_data.csv"))
    before <- DTAtools::datasets(rv$dta)
    session$setInputs(md_study_id = "STUDY-X", md_protocol_id = "PROTO-X")
    session$setInputs(
      tr_extent = "All visits", tr_scope = "Lab data",
      tr_packaging = "One archive", tr_security = "Encrypted",
      tr_checksum = "SHA-256", tr_test_requirements = "Test.\n\nApprove.\n",
      tr_blinding_requirements = "Keep blinded."
    )
    session$elapse(1000)
    expect_identical(DTAtools::metadata(rv$dta)@study, list(id = "STUDY-X", protocol_id = "PROTO-X"))
    tr <- DTAtools::metadata(rv$dta)@transmission
    expect_identical(tr$extent, "All visits")
    expect_identical(tr$scope, "Lab data")
    expect_identical(tr$packaging, "One archive")
    expect_identical(tr$security, "Encrypted")
    expect_identical(tr$checksum, "SHA-256")
    expect_identical(tr$test_requirements, "Test.\n\nApprove.\n")
    expect_identical(tr$blinding_requirements, "Keep blinded.")
    session$setInputs(md_study_id = "", md_protocol_id = "  ", tr_test_requirements = "")
    session$elapse(1000)
    expect_length(DTAtools::metadata(rv$dta)@study, 0)
    expect_null(DTAtools::metadata(rv$dta)@transmission[["test_requirements"]])
    expect_identical(DTAtools::datasets(rv$dta), before)
  })
})

test_that("read-only mode displays agreement content and gates all mutation handlers", {
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = agreement_file_input("clinical_dta.yaml"))
    enter_edit_mode(session)
    session$setInputs(agreement_add = 1)
    session$setInputs(agreement_id = "scope", agreement_title = "Scope", agreement_text = "Visible.\n\nText.")
    session$setInputs(agreement_save = 1)
    session$setInputs(md_study_id = "Visible study", tr_scope = "Visible scope")
    session$elapse(1000)
    session$setInputs(agreement_edit_scope = 1)
    session$setInputs(agreement_remove_scope = 1)
    leave_edit_mode(session)
    before <- rv$dta
    session$setInputs(agreement_id = "changed", agreement_title = "Changed", agreement_text = "Changed")
    session$setInputs(
      agreement_save = 2, agreement_remove_confirm = 1, agreement_add = 2,
      agreement_edit_scope = 2, agreement_remove_scope = 2,
      agreement_up_scope = 1, agreement_down_scope = 1,
      md_study_id = "Changed", md_protocol_id = "Changed", tr_scope = "Changed"
    )
    session$elapse(1000)
    expect_identical(rv$dta, before)
    html <- as.character(output$agreement_sections$html)
    expect_match(html, "Visible.\n\nText.", fixed = TRUE)
    expect_no_match(html, "<button", fixed = TRUE)
    expect_no_match(as.character(output$metadata_editor$html), 'id="agreement_add"', fixed = TRUE)
    expect_no_match(as.character(output$metadata_editor$html), 'id="md_study_id"', fixed = TRUE)
    expect_match(as.character(output$metadata_editor$html), "Visible study", fixed = TRUE)
    expect_match(as.character(output$metadata_editor$html), "Visible scope", fixed = TRUE)
    expect_no_match(as.character(output$metadata_editor$html), 'id="tr_scope"', fixed = TRUE)
  })
})

test_that("template carry-over labels make section replacement explicit and opt-in", {
  html <- as.character(app_fn("template_options_modal_body")(
    def = list(id = "agreement", version = "1", base = list(metadata = list())),
    loaded = list(value = list()), index = NULL, vocab_specs = list(),
    has_open_doc = TRUE
  ))
  expect_match(html, "Study identity", fixed = TRUE)
  expect_match(html, "Agreement sections (replace template clauses)", fixed = TRUE)
  expect_match(html, 'value="study" checked="checked"', fixed = TRUE)
  expect_no_match(html, 'value="sections" checked="checked"', fixed = TRUE)
})
