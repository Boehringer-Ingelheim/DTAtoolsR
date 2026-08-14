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
  # characters, so a spec author's id can legitimately contain them. The rule
  # id is shown in the "What this rule checks" explain box.
  rule_def <- DTARuleColRange(id = "a&b<c", columns = "AGE", min = 0, max = 100)
  inspect_rows <- data.frame(
    id = 1L, type = "rule", message = "range violated", headline = "range violated",
    rule_id = "a&b<c", stringsAsFactors = FALSE
  )

  html <- .report_inspect_panel_html(inspect_rows, rule_def = rule_def)

  expect_false(grepl("a&b<c", html, fixed = TRUE))
  expect_true(grepl("a&amp;b&lt;c", html, fixed = TRUE))
})

test_that(".report_inspect_panel_html escapes a failing-row column name containing HTML-special characters", {
  inspect_rows <- data.frame(
    id = 1L, type = "rule", message = "range violated", headline = "range violated",
    rule_id = NA_character_, stringsAsFactors = FALSE, check.names = FALSE
  )
  inspect_rows[["failing_A&B"]] <- "200"

  html <- .report_inspect_panel_html(inspect_rows, rule_def = NULL)

  # Failing-row columns are now rendered as a <th> table header, not "col: val".
  expect_false(grepl(">A&B<", html, fixed = TRUE))
  expect_true(grepl(">A&amp;B<", html, fixed = TRUE))
})

test_that(".report_columnspec_expected_text shows the actual allowed values for an enum violation", {
  row1 <- data.frame(
    id = 1L, type = "columnspec", message = "must be equal to one of the allowed values",
    columnspec_keyword = "enum", columnspec_column = "STATUS",
    columnspec_columnspec = "COMPLETED; DROPPED; SCREEN_FAIL",
    columnspec_data = "IN_PROGRESS",
    stringsAsFactors = FALSE
  )

  text <- .report_columnspec_expected_text(row1)

  expect_true(grepl("COMPLETED, DROPPED, SCREEN_FAIL", text, fixed = TRUE))
  expect_false(grepl("one of the allowed values", text, fixed = TRUE))
  expect_equal(.report_columnspec_actual_text(row1), "IN_PROGRESS")
})

test_that(".report_columnspec_expected_text covers every columnspec keyword the validator emits", {
  # Ground truth: R/columnSpecChecks.R's dta_columnspec_error_rows() call sites.
  row_for <- function(keyword, columnspec) {
    data.frame(
      columnspec_keyword = keyword, columnspec_columnspec = columnspec,
      columnspec_message = "generic validator message", stringsAsFactors = FALSE
    )
  }

  expect_match(.report_columnspec_expected_text(row_for("required", NA_character_)), "must be present")
  expect_match(.report_columnspec_expected_text(row_for("type", "string,number")), "type: string, number")
  expect_match(.report_columnspec_expected_text(row_for("maxLength", "10")), "at most 10 character")
  expect_match(.report_columnspec_expected_text(row_for("const", "FIXED")), "exactly: FIXED")
  expect_match(.report_columnspec_expected_text(row_for("pattern", "^[A-Z]+$")), "pattern: \\^\\[A-Z\\]\\+\\$")
  # An unrecognized keyword falls back to the validator's own message rather
  # than erroring.
  expect_equal(
    .report_columnspec_expected_text(row_for("some_future_keyword", "x")),
    "generic validator message"
  )
})

test_that(".report_import_expected_text/.report_import_actual_text show the declared type and raw value", {
  row1 <- data.frame(
    import_column = "BMI", import_raw = "unknown", import_declared_type = "number",
    import_reason = "cannot be coerced", stringsAsFactors = FALSE
  )

  expect_match(.report_import_expected_text(row1), "declared type: number")
  expect_equal(.report_import_actual_text(row1), "unknown")
})

test_that(".report_rule_expected_text builds a plain-language description per rule type", {
  range_text <- .report_rule_expected_text(DTARuleColRange(id = "r1", columns = "AGE", min = 18, max = 65))
  expect_match(range_text, "AGE must be between 18 and 65")

  unique_text <- .report_rule_expected_text(DTARuleColUnique(id = "r2", columns = c("SUBJECT_ID", "VISIT")))
  expect_match(unique_text, "SUBJECT_ID, VISIT must be unique")

  # The "unequal" (not_equal) comparator specifically -- called out by name
  # in the request that prompted this rework.
  cond_text <- .report_rule_expected_text(DTARuleColCondition(
    id = "r3",
    condition = list(VISIT = list(equals = "V03")),
    then = list(STATUS = list(not_equal = "DROPPED"))
  ))
  expect_match(cond_text, "IF VISIT = V03 THEN STATUS")
  expect_true(grepl("&#8800; DROPPED", cond_text, fixed = TRUE))

  expect_match(.report_rule_expected_text(NULL), "not available")
})

test_that("group_condition rules get an elaborate explanation of conditions and constraints", {
  rule_def <- DTARuleGroupCondition(
    id = "gc1",
    description = "Every visit needs a status.",
    group_by = "SUBJECT_ID",
    conditions = list(
      c_has_visit = list(VISIT = list(empty = FALSE)),
      c_has_status = list(STATUS = list(empty = FALSE))
    ),
    constraints = list(
      list(
        id = "visit_requires_status", type = "requires",
        `if` = "c_has_visit", `then` = "c_has_status",
        if_scope = "any", then_scope = "all"
      )
    )
  )

  explain <- .report_rule_explain_html(rule_def)

  expect_match(explain, "Grouped by")
  expect_match(explain, "SUBJECT_ID")
  expect_match(explain, "c_has_visit")
  expect_match(explain, "VISIT is not empty")
  expect_match(explain, "visit_requires_status")
  expect_match(explain, "requires")
  expect_match(explain, "Every visit needs a status\\.")

  # failing_* columns: dta_inspect_tabular_message()'s group_condition branch
  # (R/validationReporting.R) previews group_by + every condition's columns,
  # so the affected rows' actual VALUES must appear, not just row numbers.
  inspect_rows <- data.frame(
    id = 1L, type = "rule", message = "group condition violated", headline = "group condition violated",
    rule_id = "gc1",
    group_violation_group = "SUBJECT_ID=5", group_violation_constraint = "visit_requires_status",
    group_violation_message = "In group [SUBJECT_ID=5]: ...", group_violation_rows = "3, 4",
    `failing_.row` = c(3, 4), failing_SUBJECT_ID = c("5", "5"),
    failing_VISIT = c("V03", ""), failing_STATUS = c("", ""),
    stringsAsFactors = FALSE, check.names = FALSE
  )

  html <- .report_inspect_panel_html(inspect_rows, rule_def = rule_def)
  expect_match(html, "SUBJECT_ID=5")
  expect_match(html, "visit_requires_status")
  expect_match(html, "Affected rows")
  # The affected rows' actual column values, not merely the row numbers.
  expect_match(html, "V03")
  expect_false(grepl("see technical detail below", html, fixed = TRUE))
})

test_that(".report_condition_to_text renders EVERY operator on a column, not just the first", {
  # evaluate_condition() (R/evaluateRules.R) ANDs every operator supplied for
  # one column -- e.g. a two-sided numeric band -- so the text must too.
  text <- .report_condition_to_text(list(WEIGHT = list(min = 1, max = 2)))
  expect_true(grepl("&gt;= 1", text, fixed = TRUE))
  expect_true(grepl("&lt;= 2", text, fixed = TRUE))
})

test_that("group_condition constraint scopes are rendered, with the runtime's actual defaults", {
  # normalize_scope() (R/DTARuleGroupCondition-class.R) defaults EVERY scope
  # field to "any", not "all" -- constraint 2 omits then_scope/right_scope to
  # pin that the fallback text matches, not just the explicit case.
  rule_def <- DTARuleGroupCondition(
    id = "gc2",
    group_by = "SUBJECT_ID",
    conditions = list(
      c1 = list(A = list(empty = FALSE)),
      c2 = list(B = list(empty = FALSE))
    ),
    constraints = list(
      list(
        id = "me1", type = "mutually_exclusive",
        left = "c1", right = "c2", left_scope = "all", right_scope = "any"
      ),
      list(id = "req1", type = "requires", `if` = "c1", `then` = "c2")
    )
  )

  explain <- .report_rule_explain_html(rule_def)

  expect_match(explain, "\"c1\" \\(all row")
  expect_match(explain, "\"c2\" \\(any row")
  expect_match(explain, "holds for any row")
  expect_match(explain, "must hold for any row")
})

test_that("the inspect panel drops the bracketed '[dataset/target]' headline from its summary", {
  # The raw `headline` field legitimately still appears in the "Full
  # technical detail" dump at the bottom (that table is deliberately
  # exhaustive) -- what must NOT happen is the bracketed text appearing in
  # the summary message shown at the top of the panel.
  inspect_rows <- data.frame(
    id = 1L, type = "columnspec", message = "must be equal to one of the allowed values",
    headline = "[clinical_data/clinical_data_error_all] must be equal to one of the allowed values",
    columnspec_keyword = "enum", columnspec_columnspec = "A; B", columnspec_data = "C",
    stringsAsFactors = FALSE
  )

  html <- .report_inspect_panel_html(inspect_rows, rule_def = NULL)

  # The <div class="inspect-msg"> ... </div> line is where the summary
  # message is shown; it's single-line in the template, so a plain (non
  # dotall) regex is enough to isolate it from the technical-detail dump
  # further down the panel.
  msg_line <- regmatches(html, regexpr('<div class="inspect-msg">.*?</div>', html, perl = TRUE))
  expect_length(msg_line, 1)
  expect_false(grepl("[clinical_data/clinical_data_error_all]", msg_line, fixed = TRUE))
  expect_match(msg_line, "must be equal to one of the allowed values", fixed = TRUE)

  # The full raw field is still available in the technical-detail table.
  expect_match(html, "clinical_data_error_all", fixed = TRUE)
})

test_that("no inspect panel branch ever falls back to the generic '(see technical detail below)' text", {
  # The rework's whole point: columnspec/import/rule should always compute a
  # real should-be/actual, never punt to the technical-detail fallback.
  columnspec_rows <- data.frame(
    id = 1L, type = "columnspec", message = "must be equal to one of the allowed values",
    columnspec_keyword = "enum", columnspec_columnspec = "A; B", columnspec_data = "C",
    stringsAsFactors = FALSE
  )
  import_rows <- data.frame(
    id = 2L, type = "import", message = "bad value",
    import_raw = "unknown", import_declared_type = "number", stringsAsFactors = FALSE
  )
  rule_rows <- data.frame(
    id = 3L, type = "rule", message = "range violated",
    failing_AGE = "200", `failing_.row` = 5, stringsAsFactors = FALSE, check.names = FALSE
  )
  rule_def <- DTARuleColRange(id = "r1", columns = "AGE", min = 0, max = 100)

  for (html in list(
    .report_inspect_panel_html(columnspec_rows, rule_def = NULL),
    .report_inspect_panel_html(import_rows, rule_def = NULL),
    .report_inspect_panel_html(rule_rows, rule_def = rule_def)
  )) {
    expect_false(grepl("see technical detail below", html, fixed = TRUE))
  }
})
