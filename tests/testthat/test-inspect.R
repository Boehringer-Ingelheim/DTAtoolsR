load_clinical_fixture_for_inspect <- function(filename) {
  spec_path <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  fixture_path <- system.file("extdata", filename, package = "DTAtools")
  skip_if_not(nzchar(spec_path))
  skip_if_not(nzchar(fixture_path))

  dta <- read_dta_from_yaml(spec_path)
  dta <- load_file(dta, 1, file = fixture_path)
  check(dta, persist = FALSE, quiet = TRUE)
}

test_that("messages() provides sequential numeric ids", {
  dta <- load_clinical_fixture_for_inspect("clinical_data_error_all.csv")

  msgs <- messages(dta, as_tibble = FALSE)
  expect_true(is.data.frame(msgs))
  expect_gt(nrow(msgs), 0)
  expect_true(is.numeric(msgs$id))
  expect_equal(msgs$id, seq_len(nrow(msgs)))
})

test_that("inspect() gives detailed schema context", {
  dta <- load_clinical_fixture_for_inspect("clinical_data_error_schema.csv")

  msgs <- messages(dta, as_tibble = FALSE)
  schema_id <- msgs$id[msgs$source == "schema"][1]
  expect_false(is.na(schema_id))

  info <- inspect(dta, id = schema_id)

  expect_true(is.list(info))
  expect_equal(info$id, schema_id)
  expect_equal(info$type, "schema")
  expect_true(is.character(info$headline) && nzchar(info$headline))
  expect_true(is.character(info$why) && nzchar(info$why))
  expect_true(is.data.frame(info$schema_matches) || is.null(info$schema_matches))
  expect_true(is.data.frame(info$row_context) || is.null(info$row_context))
})

test_that("inspect() gives detailed rule context with failing rows", {
  dta <- load_clinical_fixture_for_inspect("clinical_data_error_rules.csv")

  msgs <- messages(dta, as_tibble = FALSE)
  rule_id <- msgs$id[msgs$source == "rule"][1]
  expect_false(is.na(rule_id))

  info <- inspect(dta, id = rule_id)

  expect_true(is.list(info))
  expect_equal(info$id, rule_id)
  expect_equal(info$type, "rule")
  expect_true(is.character(info$rule_id) && nzchar(info$rule_id))
  expect_true(is.numeric(info$failing_row_count))
  expect_gte(info$failing_row_count, 0)
  expect_true(is.data.frame(info$failing_rows_preview) || is.null(info$failing_rows_preview))
})

test_that("inspect() supports DTADataSetFile messages", {
  path <- tempfile(fileext = ".txt")
  if (file.exists(path)) {
    unlink(path)
  }

  ds <- DTADataSetFile(name = "missing_file", paths = path)
  ds <- check(ds, quiet = TRUE)

  msgs <- messages(ds, as_tibble = FALSE)
  expect_equal(nrow(msgs), 1)

  info <- inspect(ds, id = msgs$id[[1]])

  expect_true(is.list(info))
  expect_equal(info$type, "rule")
  expect_equal(info$rule_id, "file_presence")
  expect_true(grepl("not found|readable|empty", info$message, ignore.case = TRUE))
})

test_that("inspect() errors on unknown id", {
  dta <- load_clinical_fixture_for_inspect("clinical_data_error_all.csv")
  msgs <- messages(dta, as_tibble = FALSE)

  expect_error(inspect(dta, id = max(msgs$id) + 1), "not found")
  expect_error(inspect(dta, id = 0), "positive")
})
