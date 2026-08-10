test_that("results() and messages() work for empty or unvalidated objects", {
  empty_dta <- DTA(datasets = list(), metadata = DTAMetaData(title = "Empty"))
  expect_error(results(empty_dta), "no datasets")
  expect_error(messages(empty_dta), "no datasets")

  dta <- create_example_DTA()
  res <- results(dta)
  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 2)
  expect_true(all(res$status == "not_validated"))
  expect_true(all(is.na(res$run_id)))
  expect_true(all(is.na(res$validation_run)))

  msgs <- messages(dta, as_tibble = FALSE)
  expect_s3_class(msgs, "data.frame")
  expect_equal(nrow(msgs), 0)
})

test_that("message helpers produce empty and populated tables consistently", {
  empty_df <- dta_empty_messages()
  expect_s3_class(empty_df, "data.frame")
  expect_equal(nrow(empty_df), 0)
  expect_named(
    empty_df,
    c("id", "dataset", "target", "severity", "source", "rule_id", "row", "column", "keyword", "message")
  )

  populated_df <- data.frame(
    id = 1L,
    message = "rule failure",
    stringsAsFactors = FALSE
  )

  # as_tibble = TRUE must actually convert; as_tibble = FALSE must be a no-op.
  expect_s3_class(dta_to_tibble_if_available(populated_df, TRUE), "tbl_df")
  expect_identical(dta_to_tibble_if_available(populated_df, FALSE), populated_df)
})

test_that("schema and rule message converters preserve expected columns", {
  details <- list(
    schema_errors = list(
      full_error = data.frame(
        row = c(0, 1),
        column = c("AGE", "WEIGHT"),
        keyword = c("maximum", "required"),
        message = c("too large", "missing"),
        stringsAsFactors = FALSE
      )
    ),
    rule_errors = list(list(id = "r1", message = "rule violated"))
  )

  schema_msgs <- dta_schema_messages_to_df("ds", "tab", details)
  rule_msgs <- dta_rule_messages_to_df("ds", "tab", details)

  expect_equal(nrow(schema_msgs), 2)
  expect_true(all(c("dataset", "target", "message") %in% names(schema_msgs)))
  expect_equal(schema_msgs$row, c(0, 1))
  expect_equal(schema_msgs$column, c("AGE", "WEIGHT"))
  expect_equal(schema_msgs$keyword, c("maximum", "required"))
  expect_equal(schema_msgs$message, c("too large", "missing"))
  expect_equal(unique(schema_msgs$source), "schema")
  expect_equal(unique(schema_msgs$severity), "error")
  expect_equal(unique(schema_msgs$dataset), "ds")
  expect_equal(unique(schema_msgs$target), "tab")
  expect_true(all(is.na(schema_msgs$rule_id)))

  expect_true(nrow(rule_msgs) == 1)
  expect_equal(rule_msgs$rule_id, "r1")
  expect_equal(rule_msgs$message, "rule violated")
})

test_that("message collection handles dataset-level aggregation and ordering", {
  ds <- create_example_DTADataSetTabular(2)
  ds <- check(ds, tab = "tab1", force = TRUE, persist = FALSE, quiet = TRUE)

  msgs <- dta_collect_messages_for_dataset(ds, tables = "tab1", source = "memory")
  expect_true(is.data.frame(msgs))
  expect_equal(nrow(msgs), 7)
  expect_true(all(c("dataset", "target", "message") %in% names(msgs)))
  expect_equal(msgs$id, seq_len(nrow(msgs)))
  expect_equal(unique(msgs$source), "schema")
  expect_equal(msgs$row, c(1, 1, 2, 2, 3, 3, 3))
  expect_equal(
    msgs$column,
    c("STUDYID", "VISIT", "STUDYID", "VISIT", "STUDYID", "VISIT", "SUBJID")
  )

  dta <- DTA(datasets = list(clinical_data = ds), metadata = DTAMetaData(title = "Test DTA"))
  dta_msgs <- messages(dta, datasets = "clinical_data", as_tibble = FALSE)
  expect_true(is.data.frame(dta_msgs))
  expect_equal(nrow(dta_msgs), 7)
  expect_true(all(c("dataset", "target", "message") %in% names(dta_msgs)))
  expect_equal(dta_msgs$id, seq_len(nrow(dta_msgs)))
  expect_equal(unique(dta_msgs$dataset), "clinical_data")
})

test_that("validation summaries report target type and validation run metadata", {
  ds <- create_example_DTADataSetTabular(2)
  ds <- check(ds, tables = "tab1", force = TRUE, persist = FALSE, quiet = TRUE)

  status <- validation_status(ds, tables = "tab1")
  expect_equal(status$target_type, "table")
  expect_equal(status$status, "validated")
  expect_false(is.na(status$validation_run))
  # The example dataset carries 7 schema errors and no rule errors.
  expect_false(status$ok)
  expect_equal(status$n_schema_errors, 7)
  expect_equal(status$n_rule_errors, 0)

  path <- tempfile(fileext = ".txt")
  writeLines("hello world", path)
  file_ds <- DTADataSetFile(name = "notes", paths = path)
  file_ds <- check(file_ds, quiet = TRUE)

  file_status <- validation_status(file_ds)
  expect_equal(file_status$target_type, "file")
  expect_equal(file_status$status, "validated")
  expect_false(is.na(file_status$validation_run))
})

test_that("validation_run groups items checked together", {
  ds <- create_example_DTADataSetTabular(2)
  ds@tables[["tab2"]] <- ds@tables[["tab1"]]

  ds <- check(ds, tables = c("tab1", "tab2"), force = TRUE, persist = FALSE, quiet = TRUE)

  status <- validation_status(ds, tables = c("tab1", "tab2"))
  expect_equal(length(unique(status$validation_run)), 1)
  expect_false(any(is.na(status$run_id)))

  first_run <- unique(status$validation_run)
  ds <- check(ds, tables = c("tab1", "tab2"), force = FALSE, persist = FALSE, quiet = TRUE)
  second_status <- validation_status(ds, tables = c("tab1", "tab2"))

  expect_equal(length(unique(second_status$validation_run)), 1)
  expect_false(identical(unique(second_status$validation_run), first_run))
})

test_that("dta_rule_failure_row_indices reports failing rows for a check_range rule", {
  df <- data.frame(age = c(10, 20, 70, 50, NA), stringsAsFactors = FALSE)

  # min/max constructor
  rule_min_max <- DTARuleColRange(id = "r_range", columns = "age", min = 18, max = 65)
  expect_identical(dta_rule_failure_row_indices(rule_min_max, df), c(1L, 3L))

  # legacy range= constructor (stored internally as min/max)
  rule_range <- DTARuleColRange(id = "r_range2", columns = "age", range = c(18, 65))
  expect_identical(dta_rule_failure_row_indices(rule_range, df), c(1L, 3L))

  # missing column yields no failing rows rather than an error
  expect_identical(dta_rule_failure_row_indices(rule_min_max, data.frame(x = 1:3)), integer(0))

  # bounds are inclusive: only the value below the minimum is reported
  expect_identical(
    dta_rule_failure_row_indices(rule_min_max, data.frame(age = c(18, 65, 17.999))),
    3L
  )
})

test_that("dta_rule_failure_row_indices reports failing rows for a unique rule", {
  rule <- DTARuleColUnique(id = "r_unique", columns = "ID")
  df <- data.frame(ID = c("A", "A", "A", "B"), stringsAsFactors = FALSE)

  # Unlike the violation count, the row indices cover every row in the
  # duplicated group, including the first occurrence.
  expect_identical(dta_rule_failure_row_indices(rule, df), c(1L, 2L, 3L))

  unique_df <- data.frame(ID = c("A", "B", "C"), stringsAsFactors = FALSE)
  expect_identical(dta_rule_failure_row_indices(rule, unique_df), integer(0))

  # a missing column yields no failing rows rather than an error
  multi_rule <- DTARuleColUnique(id = "r_unique_multi", columns = c("ID", "MISSING"))
  expect_identical(dta_rule_failure_row_indices(multi_rule, df), integer(0))
})

test_that("dta_rule_failure_row_indices reports failing rows for a condition rule", {
  rule <- DTARuleColCondition(
    id = "r_condition",
    condition = list(VISIT = list(equals = "V03")),
    then = list(STATUS = list(equals = "COMPLETED"))
  )
  df <- data.frame(
    VISIT = c("V03", "EOT", "V03"),
    STATUS = c("X", "X", "COMPLETED"),
    stringsAsFactors = FALSE
  )

  # Row 1 meets the IF and fails the THEN; row 2 never meets the IF and row 3
  # satisfies both.
  expect_identical(dta_rule_failure_row_indices(rule, df), 1L)

  passing_df <- data.frame(
    VISIT = c("V03", "EOT"),
    STATUS = c("COMPLETED", "X"),
    stringsAsFactors = FALSE
  )
  expect_identical(dta_rule_failure_row_indices(rule, passing_df), integer(0))
})

test_that("dta_rule_failure_row_indices returns no rows for unsupported rule objects", {
  expect_identical(
    dta_rule_failure_row_indices(list(id = "not_a_rule"), data.frame(age = 1:3)),
    integer(0)
  )
})
