load_clinical_fixture_for_inspect <- function(filename) {
  spec_path <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  fixture_path <- system.file("extdata", filename, package = "DTAtools")
  # These are guaranteed package assets, not optional dependencies. A bare
  # skip_if_not() here meant a broken install reported "skipped" and left CI
  # green with zero coverage of validation, inspection and reporting.
  expect_true(nzchar(spec_path), info = "clinical_dta.yaml missing from extdata")
  expect_true(nzchar(fixture_path), info = paste(filename, "missing from extdata"))

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

  info <- inspect(dta, id = schema_id, as_tibble = FALSE)

  expect_true(is.data.frame(info))
  expect_gt(nrow(info), 0)
  expect_true(all(info$id == schema_id))
  expect_true(all(info$type == "schema"))
  expect_true(is.character(info$headline[[1]]) && nzchar(info$headline[[1]]))
  expect_true(is.character(info$why[[1]]) && nzchar(info$why[[1]]))
  expect_true("schema_keyword" %in% names(info))
  expect_true("schema_message" %in% names(info))
  expect_true(any(grepl("required|type|length|range|pattern", info$schema_keyword, ignore.case = TRUE), na.rm = TRUE))
})

test_that("inspect() schema matches stay specific to required HEIGHT message", {
  dta <- load_clinical_fixture_for_inspect("clinical_data_error_schema.csv")

  msgs <- messages(dta, as_tibble = FALSE)
  target <- msgs[
    msgs$source == "schema" &
      msgs$keyword == "required" &
      grepl("required property 'HEIGHT'", msgs$message, fixed = TRUE), ,
    drop = FALSE
  ]
  expect_gt(nrow(target), 0)

  info <- inspect(dta, id = target$id[[1]], as_tibble = FALSE)

  expect_true(is.data.frame(info))
  expect_gt(nrow(info), 0)
  expect_true(all(info$schema_keyword == "required"))
  expect_true(all(grepl("required property 'HEIGHT'", info$schema_message, fixed = TRUE)))
  expect_false(any(info$schema_column %in% c("BMI", "GENDER"), na.rm = TRUE))
})

test_that("inspect() gives detailed rule context with failing rows", {
  dta <- load_clinical_fixture_for_inspect("clinical_data_error_rules.csv")

  msgs <- messages(dta, as_tibble = FALSE)
  rule_id <- msgs$id[msgs$source == "rule"][1]
  expect_false(is.na(rule_id))

  info <- inspect(dta, id = rule_id, as_tibble = FALSE)

  expect_true(is.data.frame(info))
  expect_gt(nrow(info), 0)
  expect_true(all(info$id == rule_id))
  expect_true(all(info$type == "rule"))
  expect_true(is.character(info$rule_id[[1]]) && nzchar(info$rule_id[[1]]))
  expect_true(is.numeric(info$failing_row_count[[1]]))
  expect_gte(info$failing_row_count[[1]], 0)
  expect_true(any(grepl("^failing_", names(info))))
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

  info <- inspect(ds, id = msgs$id[[1]], as_tibble = FALSE)

  expect_true(is.data.frame(info))
  expect_gt(nrow(info), 0)
  expect_equal(info$type[[1]], "rule")
  expect_equal(info$rule_id[[1]], "file_presence")
  expect_true(grepl("not found|readable|empty", info$message[[1]], ignore.case = TRUE))
})

test_that("inspect() supports multiple ids and tibble/data.frame output", {
  dta <- load_clinical_fixture_for_inspect("clinical_data_error_all.csv")
  msgs <- messages(dta, as_tibble = FALSE)
  ids <- c(1, 2)

  info_df <- inspect(dta, id = ids, as_tibble = FALSE)
  expect_true(is.data.frame(info_df))
  expect_false(inherits(info_df, "tbl_df"))
  expect_equal(sort(unique(info_df$id)), ids)

  info_tbl <- inspect(dta, id = ids, as_tibble = TRUE)
  if (requireNamespace("tibble", quietly = TRUE)) {
    expect_true(inherits(info_tbl, "tbl_df"))
  } else {
    expect_true(is.data.frame(info_tbl))
  }
  expect_equal(sort(unique(info_tbl$id)), ids)
})

test_that("inspect() without id returns all messages", {
  dta <- load_clinical_fixture_for_inspect("clinical_data_error_all.csv")
  msgs <- messages(dta, as_tibble = FALSE)

  info_df <- inspect(dta, as_tibble = FALSE)
  expect_true(is.data.frame(info_df))
  expect_equal(sort(unique(info_df$id)), msgs$id)

  info_tbl <- inspect(dta)
  if (requireNamespace("tibble", quietly = TRUE)) {
    expect_true(inherits(info_tbl, "tbl_df"))
  } else {
    expect_true(is.data.frame(info_tbl))
  }
  expect_equal(sort(unique(info_tbl$id)), msgs$id)
})

test_that("inspect() errors on unknown id", {
  dta <- load_clinical_fixture_for_inspect("clinical_data_error_all.csv")
  msgs <- messages(dta, as_tibble = FALSE)

  expect_error(inspect(dta, id = max(msgs$id) + 1), "not found")
  expect_error(inspect(dta, id = 0), "positive")
})
