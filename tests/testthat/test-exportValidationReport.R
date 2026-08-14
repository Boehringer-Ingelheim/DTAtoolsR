skip_if_not_installed("xml2")

test_that("write_validation_report validates input", {
  expect_error(
    write_validation_report(list(), file = tempfile(fileext = ".html")),
    "must be a DTA"
  )

  existing <- tempfile(fileext = ".html")
  file.create(existing)
  on.exit(unlink(existing, force = TRUE), add = TRUE)
  expect_error(
    write_validation_report(
      create_example_DTA(),
      file = existing,
      overwrite = FALSE
    ),
    "already exists"
  )
})

test_that("write_validation_report happy path with capping", {
  # Create a DTA with exactly 7 messages using the fixture from test-validationReporting.R:73-90
  ds <- create_example_DTADataSetTabular(2)
  ds <- check(ds, tab = "tab1", force = TRUE, persist = FALSE, quiet = TRUE)
  dta <- DTA(
    datasets = list(clinical_data = ds),
    metadata = DTAMetaData(title = "Test DTA")
  )

  out <- tempfile(fileext = ".html")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  write_validation_report(dta, out, max_repeats = 2, overwrite = TRUE, quiet = TRUE)

  expect_true(file.exists(out))

  doc <- xml2::read_html(out)

  # Assert title in page
  h1 <- xml2::xml_find_first(doc, "//h1")
  expect_equal(xml2::xml_text(h1), "Test DTA")

  # Assert 1 summary row with fail status
  summary_rows <- xml2::xml_find_all(doc, "//tr[@data-status]")
  expect_length(summary_rows, 1)
  expect_equal(xml2::xml_attr(summary_rows, "data-status"), "fail")

  # Assert 7 msg-row elements total
  msg_rows <- xml2::xml_find_all(
    doc,
    "//tr[contains(concat(' ', normalize-space(@class), ' '), ' msg-row ')]"
  )
  expect_length(msg_rows, 7)

  # Assert 2 msg-row-extra (capped rows from repeated message groups)
  msg_rows_extra <- xml2::xml_find_all(
    doc,
    "//tr[contains(concat(' ', normalize-space(@class), ' '), ' msg-row-extra ')]"
  )
  expect_length(msg_rows_extra, 2)

  # STUDYID (3 occurrences) and VISIT (3 occurrences) each exceed
  # max_repeats = 2, so each gets its own "show more" row; SUBJID (1
  # occurrence) does not.
  more_rows <- xml2::xml_find_all(doc, "//tr[@class='msg-more-row']")
  expect_length(more_rows, 2)
  buttons <- xml2::xml_find_all(more_rows, ".//button[@class='show-more-btn']")
  expect_equal(xml2::xml_text(buttons), c("Show 1 more like this", "Show 1 more like this"))

  # Assert 7 inspect panels (one per message)
  inspect_panels <- xml2::xml_find_all(doc, "//div[contains(@class,'inspect-panel')]")
  expect_length(inspect_panels, 7)
})

test_that("write_validation_report capping disabled with max_repeats = NULL", {
  ds <- create_example_DTADataSetTabular(2)
  ds <- check(ds, tab = "tab1", force = TRUE, persist = FALSE, quiet = TRUE)
  dta <- DTA(
    datasets = list(clinical_data = ds),
    metadata = DTAMetaData(title = "Test DTA")
  )

  out <- tempfile(fileext = ".html")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  write_validation_report(dta, out, max_repeats = NULL, overwrite = TRUE, quiet = TRUE)

  doc <- xml2::read_html(out)

  # No msg-row-extra elements
  msg_rows_extra <- xml2::xml_find_all(
    doc,
    "//tr[contains(concat(' ', normalize-space(@class), ' '), ' msg-row-extra ')]"
  )
  expect_length(msg_rows_extra, 0)

  # No msg-more-row elements
  more_rows <- xml2::xml_find_all(doc, "//tr[@class='msg-more-row']")
  expect_length(more_rows, 0)

  # Still 7 msg-row elements total
  msg_rows <- xml2::xml_find_all(
    doc,
    "//tr[contains(concat(' ', normalize-space(@class), ' '), ' msg-row ')]"
  )
  expect_length(msg_rows, 7)
})

test_that("write_validation_report capping disabled with max_repeats = Inf", {
  ds <- create_example_DTADataSetTabular(2)
  ds <- check(ds, tab = "tab1", force = TRUE, persist = FALSE, quiet = TRUE)
  dta <- DTA(
    datasets = list(clinical_data = ds),
    metadata = DTAMetaData(title = "Test DTA")
  )

  out <- tempfile(fileext = ".html")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  write_validation_report(dta, out, max_repeats = Inf, overwrite = TRUE, quiet = TRUE)

  doc <- xml2::read_html(out)

  # No msg-row-extra elements
  msg_rows_extra <- xml2::xml_find_all(
    doc,
    "//tr[contains(concat(' ', normalize-space(@class), ' '), ' msg-row-extra ')]"
  )
  expect_length(msg_rows_extra, 0)

  # No msg-more-row elements
  more_rows <- xml2::xml_find_all(doc, "//tr[@class='msg-more-row']")
  expect_length(more_rows, 0)

  # Still 7 msg-row elements total
  msg_rows <- xml2::xml_find_all(
    doc,
    "//tr[contains(concat(' ', normalize-space(@class), ' '), ' msg-row ')]"
  )
  expect_length(msg_rows, 7)
})

test_that(".report_html_escape escapes HTML special characters", {
  result <- .report_html_escape(c("<b>&\"'", NA))
  expect_equal(result, c("&lt;b&gt;&amp;&quot;&#39;", ""))
})

test_that("write_validation_report escapes HTML in metadata title", {
  # Use a validated DTA with valid data to test escaping in title
  dta <- app_fixture_dta_with_data("clinical_data.csv", checked = TRUE)
  # Override the title with HTML-special characters
  dta@metadata@title <- "A & B <Co>"

  out <- tempfile(fileext = ".html")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  write_validation_report(dta, out, overwrite = TRUE, quiet = TRUE)

  doc <- xml2::read_html(out)

  # Parse HTML and verify the title round-trips correctly
  # xml2::xml_text() will un-escape the entities back to the original text
  h1 <- xml2::xml_find_first(doc, "//h1")
  expect_equal(xml2::xml_text(h1), "A & B <Co>")
})

test_that("write_validation_report handles zero messages (all-pass)", {
  # Create a DTA with valid data that passes validation (0 error messages)
  dta <- app_fixture_dta_with_data("clinical_data.csv", checked = TRUE)

  out <- tempfile(fileext = ".html")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  write_validation_report(dta, out, overwrite = TRUE, quiet = TRUE)

  expect_true(file.exists(out))
  doc <- xml2::read_html(out)

  # Messages table tbody has 0 msg-row elements
  msg_rows <- xml2::xml_find_all(
    doc,
    "//tr[contains(concat(' ', normalize-space(@class), ' '), ' msg-row ')]"
  )
  expect_length(msg_rows, 0)

  # Summary section still present
  summary_sections <- xml2::xml_find_all(doc, "//section[@class='report-summary']")
  expect_length(summary_sections, 1)
})

test_that("write_validation_report overwrite = TRUE allows replacing existing file", {
  # Create a DTA with valid data that passes validation
  dta <- app_fixture_dta_with_data("clinical_data.csv", checked = TRUE)

  out <- tempfile(fileext = ".html")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  # Write once
  write_validation_report(dta, out, overwrite = TRUE, quiet = TRUE)
  expect_true(file.exists(out))

  # Write again with overwrite = TRUE
  expect_no_error(
    write_validation_report(dta, out, overwrite = TRUE, quiet = TRUE)
  )
  expect_true(file.exists(out))

  # File is still valid HTML
  doc <- xml2::read_html(out)
  expect_true(!is.na(doc))
})

test_that("write_validation_report validates title and falls back on NA", {
  dta <- create_example_DTA()

  expect_error(
    write_validation_report(dta, tempfile(fileext = ".html"), title = c("A", "B")),
    "single character string"
  )

  # nzchar(NA) is TRUE by default -- a title = NA_character_ caller must
  # still get the metadata/default fallback, not an empty <title>/<h1>.
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  write_validation_report(dta, out, title = NA_character_, overwrite = TRUE, quiet = TRUE)
  doc <- xml2::read_html(out)
  h1_text <- xml2::xml_text(xml2::xml_find_first(doc, "//h1"))
  expect_true(nzchar(h1_text))
})

test_that(".report_messages_table_html rejects a non-whole-number or non-positive max_repeats", {
  msgs <- data.frame(
    id = 1L, dataset = "d", target = "t", severity = "error", source = "columnspec",
    rule_id = NA_character_, row = 1, column = "X", keyword = "required",
    message = "must have required property 'X'", stringsAsFactors = FALSE
  )
  # A non-whole-number max_repeats would cap rows via `>` but never emit a
  # "show more" toggle (which fires on exact equality), permanently hiding
  # them -- regression test for the bug caught in review.
  expect_error(.report_messages_table_html(msgs, max_repeats = 2.5), "positive whole number")
  expect_error(.report_messages_table_html(msgs, max_repeats = 0), "positive whole number")
  expect_error(.report_messages_table_html(msgs, max_repeats = -1), "positive whole number")

  # Validation must fire even when there is no data to render -- it must not
  # depend on nrow(messages_df) > 0.
  expect_error(
    .report_messages_table_html(msgs[0, , drop = FALSE], max_repeats = 0),
    "positive whole number"
  )
})

test_that(".report_inspect_panel_html escapes a rule id containing HTML-special characters", {
  # DTARule@id only forbids whitespace (R/DTARule-class.R), not HTML-special
  # characters, so a spec author's id can legitimately contain them.
  rule_def <- DTARuleColRange(id = "a&b<c", columns = "AGE", min = 0, max = 100)
  inspect_row <- data.frame(
    id = 1L, type = "rule", message = "range violated", headline = "range violated",
    rule_id = "a&b<c", stringsAsFactors = FALSE
  )

  html <- .report_inspect_panel_html(inspect_row, rule_def = rule_def)

  expect_false(grepl("a&b<c", html, fixed = TRUE))
  expect_true(grepl("a&amp;b&lt;c", html, fixed = TRUE))
})

test_that(".report_inspect_panel_html escapes a failing-row column name containing HTML-special characters", {
  inspect_row <- data.frame(
    id = 1L, type = "rule", message = "range violated", headline = "range violated",
    rule_id = NA_character_, stringsAsFactors = FALSE, check.names = FALSE
  )
  inspect_row[["failing_A&B"]] <- "200"

  html <- .report_inspect_panel_html(inspect_row, rule_def = NULL)

  expect_false(grepl("A&B:", html, fixed = TRUE))
  expect_true(grepl("A&amp;B:", html, fixed = TRUE))
})
