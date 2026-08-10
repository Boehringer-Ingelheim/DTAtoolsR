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

test_that("clinical_data_error_schema.csv triggers diverse schema failures only", {
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

  expect_false(details$schema_valid)
  expect_true(details$rules_valid)
  expect_equal(sort(unique(schema_full$keyword)), c("const", "enum", "maxLength", "required", "type"))
  expect_equal(sort(unique(schema_full$column)), c("BMI", "GENDER", "STUDYID", "VISIT"))

  expect_true(is.data.frame(msgs))
  expect_gt(nrow(msgs), 0)
  expect_true(all(msgs$source == "schema"))
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

  expect_true(details$schema_valid)
  expect_false(details$rules_valid)
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

  expect_false(details$schema_valid)
  expect_false(details$rules_valid)
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