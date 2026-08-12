load_clinical_fixture_dta <- function(filename) {
  spec_path <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  fixture_path <- system.file("extdata", filename, package = "DTAtools")
  # Guaranteed package assets — a missing fixture is a failure, not a skip.
  expect_true(nzchar(spec_path), info = "clinical_dta.yaml missing from extdata")
  expect_true(nzchar(fixture_path), info = paste(filename, "missing from extdata"))

  dta <- read_dta_from_yaml(spec_path)
  dta <- load_file(dta, 1, file = fixture_path)
  check(dta, persist = FALSE, quiet = TRUE)
}

test_that("clinical_data_error_schema.csv triggers schema failures and one import failure", {
  dta <- load_clinical_fixture_dta("clinical_data_error_schema.csv")

  res <- results(dta)
  table_name <- res$target[1]
  details <- validation_errors(dta[["clinical_data"]], table = table_name, source = "memory")
  schema_full <- as.data.frame(details$schema_errors$full_error)
  msgs <- messages(dta, as_tibble = FALSE)

  expect_equal(nrow(res), 1)
  expect_equal(res$dataset, "clinical_data")
  expect_equal(res$target, "clinical_data_error_schema")
  expect_equal(res$status, "failed")
  expect_gt(res$n_schema_errors, 0)
  expect_equal(res$n_rule_errors, 0)
  expect_equal(res$n_import_errors, 1L)

  expect_false(details$schema_valid)
  expect_true(details$rules_valid)
  expect_false(details$import_valid)
  # The table is invalid on the three-axis verdict, not merely on the schema
  # axis: the schema count is now lower than it was, and `ok` must not follow it.
  expect_false(details$ok)
  expect_equal(sort(unique(schema_full$keyword)), c("const", "enum", "maxLength", "required", "type"))
  expect_equal(sort(unique(schema_full$column)), c("BMI", "GENDER", "STUDYID", "VISIT"))

  # BMI is declared `SAS Num` and holds one "heavy" among 500 numbers. Before the
  # typed import choke point, Arrow read the whole column as text and every row
  # failed the `type` check. Now the column is a number, only the one cell that
  # could not be represented is NA, and that cell is reported once on each axis:
  # `type` (null against nullable: false) and `not_convertible` on the import
  # axis. The schema count falls by ~500; `ok` is unmoved because the import axis
  # carries the failure.
  bmi_schema <- schema_full[schema_full$column %in% "BMI", , drop = FALSE]
  expect_equal(nrow(bmi_schema), 1)
  expect_equal(bmi_schema$keyword, "type")

  import_errors <- details$import_errors
  expect_true(is.data.frame(import_errors))
  expect_equal(nrow(import_errors), 1)
  expect_equal(import_errors$column, "BMI")
  expect_equal(import_errors$raw, "heavy")
  expect_equal(import_errors$declared_type, "SAS Num")
  expect_equal(import_errors$reason, "not_convertible")

  expect_true(is.data.frame(msgs))
  expect_gt(nrow(msgs), 0)
  expect_equal(sort(unique(msgs$source)), c("import", "schema"))
  expect_true(all(msgs$target == table_name))
})

test_that("clinical_data_error_rules.csv triggers representative rule failures only", {
  dta <- load_clinical_fixture_dta("clinical_data_error_rules.csv")

  res <- results(dta)
  table_name <- res$target[1]
  details <- validation_errors(dta[["clinical_data"]], table = table_name, source = "memory")
  rule_ids <- vapply(details$rule_errors, function(x) x$id, character(1))
  msgs <- messages(dta, as_tibble = FALSE)

  expect_equal(nrow(res), 1)
  expect_equal(res$status, "failed")
  expect_equal(res$n_schema_errors, 0)
  expect_equal(res$n_rule_errors, 6)
  # Every value in this fixture is representable in its declared type; the
  # failures are all on the rule axis. The typed import must not add to them.
  expect_equal(res$n_import_errors, 0L)

  expect_true(details$schema_valid)
  expect_false(details$rules_valid)
  expect_true(details$import_valid)
  expect_false(details$ok)
  expect_equal(
    sort(rule_ids),
    sort(c(
      "check_col_condition_example",
      "rule_dependency_example",
      "rule_equal_example",
      "rule_range_example",
      "rule_unequal_example",
      "rule_unique_example"
    ))
  )

  expect_true(is.data.frame(msgs))
  expect_gt(nrow(msgs), 0)
  expect_true(all(msgs$source == "rule"))
  expect_equal(sort(unique(msgs$rule_id)), sort(rule_ids))
})

test_that("clinical_data_error_all.csv combines schema and rule failures", {
  dta <- load_clinical_fixture_dta("clinical_data_error_all.csv")

  res <- results(dta)
  table_name <- res$target[1]
  details <- validation_errors(dta[["clinical_data"]], table = table_name, source = "memory")
  schema_full <- as.data.frame(details$schema_errors$full_error)
  rule_ids <- vapply(details$rule_errors, function(x) x$id, character(1))
  msgs <- messages(dta, as_tibble = FALSE)

  expect_equal(nrow(res), 1)
  expect_equal(res$status, "failed")
  expect_gt(res$n_schema_errors, 0)
  expect_gt(res$n_rule_errors, 0)
  expect_equal(res$n_import_errors, 0L)

  expect_false(details$schema_valid)
  expect_false(details$rules_valid)
  expect_true(details$import_valid)
  expect_false(details$ok)
  expect_equal(sort(unique(schema_full$keyword)), c("const", "enum", "maxLength"))
  expect_equal(sort(unique(schema_full$column)), c("INCLUDE", "STUDYID", "VISIT"))
  expect_equal(
    sort(rule_ids),
    sort(c(
      "check_col_condition_example",
      "rule_dependency_example",
      "rule_equal_example",
      "rule_unequal_example",
      "rule_unique_example"
    ))
  )

  expect_true(is.data.frame(msgs))
  expect_gt(nrow(msgs), 0)
  expect_equal(sort(unique(msgs$source)), c("rule", "schema"))
})