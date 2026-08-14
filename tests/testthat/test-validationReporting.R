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

test_that("column spec and rule message converters preserve expected columns", {
  details <- list(
    columnspec_errors = list(
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

  schema_msgs <- dta_columnspec_messages_to_df("ds", "tab", details)
  rule_msgs <- dta_rule_messages_to_df("ds", "tab", details)

  expect_equal(nrow(schema_msgs), 2)
  expect_true(all(c("dataset", "target", "message") %in% names(schema_msgs)))
  expect_equal(schema_msgs$row, c(0, 1))
  expect_equal(schema_msgs$column, c("AGE", "WEIGHT"))
  expect_equal(schema_msgs$keyword, c("maximum", "required"))
  expect_equal(schema_msgs$message, c("too large", "missing"))
  expect_equal(unique(schema_msgs$source), "columnspec")
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
  expect_equal(unique(msgs$source), "columnspec")
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
  # The example dataset carries 7 column spec errors and no rule errors.
  expect_false(status$ok)
  expect_equal(status$n_columnspec_errors, 7)
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

# The two-sites regression: rule_check_range() (messages) and
# dta_rule_failure_row_indices() (inspect) each carried the same numeric
# coercion defect, independently. They must agree row for row, or messages()
# reports N violated rows while inspect() shows failing_row_count = 0.

mixed_numeric_rule_dataset <- function() {
  specs <- DTAColumnSpecCollection(
    columns = list(
      SUBJECT_ID = DTAColumnSpec(
        id = "SUBJECT_ID", type = "SAS Char", length = 10, nullable = FALSE
      ),
      AGE = DTAColumnSpec(id = "AGE", type = "SAS Char", length = 10, nullable = TRUE)
    ),
    rules = list(DTARuleColRange(id = "age_range", columns = "AGE", min = 18, max = 65))
  )

  table <- arrow::arrow_table(data.frame(
    SUBJECT_ID = c("S1", "S2", "S3", "S4", "S5"),
    # convertible+in range / unconvertible / convertible+out of range /
    # genuinely missing / convertible+in range
    AGE = c("30", "ninety", "700", NA, "50"),
    stringsAsFactors = FALSE
  ))

  DTADataSetTabular(name = "mixed", specs = specs, tables = list(tab = table))
}

# The count a rule message states, read back out of the message text. This is a
# package-generated sprintf() string, not a translated one.
stated_violation_count <- function(message) {
  if (is.null(message)) {
    return(0L)
  }

  hit <- regmatches(message, regexpr("violated: [0-9]+ rows", message))
  if (length(hit) == 0) {
    return(NA_integer_)
  }

  as.integer(regmatches(hit, regexpr("[0-9]+", hit)))
}

test_that("dta_rule_failure_row_indices matches rule_check_range row for row", {
  rule <- DTARuleColRange(id = "age_range", columns = "AGE", min = 18, max = 65)

  cases <- list(
    data.frame(AGE = c("30", "ninety", "700", NA, "50"), stringsAsFactors = FALSE),
    data.frame(AGE = factor(c("500", "600", "700"))),
    data.frame(AGE = c("ninety", "N/A", ">65"), stringsAsFactors = FALSE),
    data.frame(AGE = c(NA, NA), stringsAsFactors = FALSE),
    data.frame(AGE = c(20, 70, NA), stringsAsFactors = FALSE)
  )

  for (df in cases) {
    reported <- rule_check_range(rule, df)
    counted <- length(dta_rule_failure_row_indices(rule, df))

    stated <- stated_violation_count(reported$message)

    expect_identical(counted, stated)
  }

  # Pin the values, not only the agreement.
  expect_identical(
    dta_rule_failure_row_indices(
      rule,
      data.frame(AGE = c("30", "ninety", "700", NA, "50"), stringsAsFactors = FALSE)
    ),
    c(2L, 3L)
  )
  expect_identical(
    dta_rule_failure_row_indices(rule, data.frame(AGE = factor(c("500", "600", "700")))),
    c(1L, 2L, 3L)
  )
})

test_that("messages() violation count equals inspect() failing_row_count", {
  ds <- check(mixed_numeric_rule_dataset(), persist = FALSE, quiet = TRUE)

  msgs <- messages(ds, as_tibble = FALSE)
  rule_msgs <- msgs[msgs$source == "rule", , drop = FALSE]
  expect_equal(nrow(rule_msgs), 1)

  stated <- stated_violation_count(rule_msgs$message[[1]])
  expect_equal(stated, 2L)

  info <- inspect(ds, id = rule_msgs$id[[1]], as_tibble = FALSE)
  expect_true(all(info$type == "rule"))
  expect_equal(unique(info$failing_row_count), stated)

  # The previewed rows are the ones the message counted.
  expect_equal(sort(unique(info$failing_.row)), c(2, 3))
})

test_that("the row lookup finds the rows a grouped rule failed on", {
  df <- data.frame(
    SUBJECT_ID = c("S1", "S1"),
    STATUS = c("FAILED", "FAILED"),
    RESULT = c(NA, 12)
  )
  rule <- DTARuleGroupCondition(
    id = "group_example",
    group_by = "SUBJECT_ID",
    conditions = list(
      c_failed = list(STATUS = list(equals = "FAILED")),
      c_reported = list(RESULT = list(empty = FALSE))
    ),
    constraints = list(list(
      type = "mutually_exclusive", left = "c_failed", right = "c_reported"
    ))
  )

  expect_false(rule_check_group_condition(rule, df)$valid)
  # Reporting no rows here made inspect() show failing_row_count = 0 for a rule
  # that unambiguously failed, because the lookup had no branch for this class.
  expect_equal(dta_rule_failure_row_indices(rule, df), c(1L, 2L))
})
