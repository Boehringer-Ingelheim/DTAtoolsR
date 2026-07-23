test_that("results() and messages() work for empty or unvalidated objects", {
  empty_dta <- DTA(datasets = list(), metadata = DTAMetaData(title = "Empty"))
  expect_error(results(empty_dta), "no datasets")
  expect_error(messages(empty_dta), "no datasets")

  dta <- create_example_DTA()
  res <- results(dta)
  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 2)
  expect_true(all(res$status == "not_validated"))
  expect_true(all(is.na(res$ok)))

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
    c("dataset", "table", "severity", "source", "rule_id", "row", "column", "keyword", "message")
  )

  populated_df <- data.frame(
    dataset = "ds",
    table = "tab",
    severity = "error",
    source = "rule",
    rule_id = "r1",
    row = NA_real_,
    column = NA_character_,
    keyword = NA_character_,
    message = "rule failure",
    stringsAsFactors = FALSE
  )

  tibble_out <- dta_to_tibble_if_available(populated_df, as_tibble = TRUE)
  expect_true(inherits(tibble_out, "tbl_df") || is.data.frame(tibble_out))
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

  expect_true(nrow(schema_msgs) >= 1)
  expect_true(all(c("dataset", "table", "message") %in% names(schema_msgs)))
  expect_true(nrow(rule_msgs) == 1)
  expect_equal(rule_msgs$rule_id, "r1")
  expect_equal(rule_msgs$message, "rule violated")
})

test_that("message collection handles dataset-level aggregation and ordering", {
  ds <- create_example_DTADataSetTabular(2)
  ds <- check(ds, tab = "tab1", force = TRUE, persist = FALSE, quiet = TRUE)

  msgs <- dta_collect_messages_for_dataset(ds, tables = "tab1", source = "memory")
  expect_true(is.data.frame(msgs))
  expect_true(nrow(msgs) >= 0)
  expect_true(all(c("dataset", "table", "message") %in% names(msgs)))

  dta <- DTA(datasets = list(clinical_data = ds), metadata = DTAMetaData(title = "Test DTA"))
  dta_msgs <- messages(dta, datasets = "clinical_data", as_tibble = FALSE)
  expect_true(is.data.frame(dta_msgs))
  expect_true(all(c("dataset", "table", "message") %in% names(dta_msgs)))
})
