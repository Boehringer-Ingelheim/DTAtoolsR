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

test_that("clinical_data_error_all.csv combines schema, rule and import failures", {
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
  # This fixture now also carries 4 import errors (SUBJ0009 BMI "unknown",
  # SUBJ0012 HEIGHT ">190", SUBJ0013 AGE "ninety", SUBJ0018 WEIGHT ">300"),
  # added on rows the pre-existing schema/rule edits did not touch.
  expect_equal(res$n_import_errors, 4L)

  expect_false(details$schema_valid)
  expect_false(details$rules_valid)
  expect_false(details$import_valid)
  expect_false(details$ok)
  # AGE, BMI, HEIGHT and WEIGHT are all declared `nullable: false`, so each of
  # the 4 unconvertible cells becoming NA is also a schema `type` violation --
  # plus one more `type` violation for a genuinely *missing* BMI cell
  # (SUBJ0011, blank, not unconvertible) that contributes 0 import errors.
  # That fifth cell is the unavoidable schema-axis fallout: every numeric
  # column in this spec is non-nullable, so a missing value there can never be
  # schema-clean, even though it is import-clean.
  expect_equal(sort(unique(schema_full$keyword)), c("const", "enum", "maxLength", "type"))
  expect_equal(
    sort(unique(schema_full$column)),
    c("AGE", "BMI", "HEIGHT", "INCLUDE", "STUDYID", "VISIT", "WEIGHT")
  )
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

  import_errors <- details$import_errors
  expect_true(is.data.frame(import_errors))
  expect_equal(nrow(import_errors), 4)
  expect_equal(sort(import_errors$column), c("AGE", "BMI", "HEIGHT", "WEIGHT"))
  expect_equal(sort(import_errors$row), c(9, 12, 13, 18))
  expect_equal(import_errors$declared_type, rep("SAS Num", 4))
  expect_equal(import_errors$reason, rep("not_convertible", 4))
  expect_true(all(c(">190", "ninety", "unknown", ">300") %in% import_errors$raw))
  # The missing (blank) BMI cell must not appear as an import error: missing
  # and unconvertible are different defects.
  expect_false(11 %in% import_errors$row)

  expect_true(is.data.frame(msgs))
  expect_gt(nrow(msgs), 0)
  expect_equal(sort(unique(msgs$source)), c("import", "rule", "schema"))
})

test_that("clinical_data_error_import.csv triggers import failures, isolated as far as the spec allows", {
  dta <- load_clinical_fixture_dta("clinical_data_error_import.csv")

  res <- results(dta)
  table_name <- res$target[1]
  details <- validation_errors(dta[["clinical_data"]], table = table_name, source = "memory")
  schema_full <- as.data.frame(details$schema_errors$full_error)
  rule_ids <- vapply(details$rule_errors, function(x) x$id, character(1))
  msgs <- messages(dta, as_tibble = FALSE)

  expect_equal(nrow(res), 1)
  expect_equal(res$dataset, "clinical_data")
  expect_equal(res$target, "clinical_data_error_import")
  expect_equal(res$status, "failed")
  expect_equal(res$n_import_errors, 4L)
  expect_equal(res$n_rule_errors, 0)
  # AGE, BMI, HEIGHT and WEIGHT are all declared `nullable: false`, so each of
  # the 4 unconvertible cells (HEIGHT ">190", AGE "ninety", BMI "unknown",
  # WEIGHT ">300") becomes NA and is *also* a schema `type` violation -- plus
  # one more `type` violation for a genuinely missing (blank) BMI cell that
  # contributes 0 import errors. This is the unavoidable schema-axis fallout
  # called out in the task: the import axis cannot be isolated perfectly here
  # because no numeric column in this spec is nullable. No rule references
  # these 4 columns' comparison operators in a way this fixture triggers, so
  # the rule axis stays clean.
  expect_equal(res$n_schema_errors, 5)

  expect_false(details$schema_valid)
  expect_true(details$rules_valid)
  expect_false(details$import_valid)
  expect_false(details$ok)
  expect_equal(length(rule_ids), 0)

  expect_equal(sort(unique(schema_full$keyword)), "type")
  expect_equal(sort(unique(schema_full$column)), c("AGE", "BMI", "HEIGHT", "WEIGHT"))

  import_errors <- details$import_errors
  expect_true(is.data.frame(import_errors))
  expect_equal(nrow(import_errors), 4)
  expect_equal(sort(import_errors$column), c("AGE", "BMI", "HEIGHT", "WEIGHT"))
  # Rows 5, 6, 9 and 18 are the unconvertible cells; row 3 (SUBJ0002's blank
  # BMI cell) is a genuinely missing value and must NOT appear here -- proving
  # missing and unconvertible are reported as different defects.
  expect_equal(sort(import_errors$row), c(5, 6, 9, 18))
  expect_false(3 %in% import_errors$row)
  expect_equal(import_errors$declared_type, rep("SAS Num", 4))
  expect_equal(import_errors$reason, rep("not_convertible", 4))
  # The raw source text is retained verbatim, not merely the fact that
  # something failed to convert.
  expect_true(all(c(">190", "ninety", "unknown", ">300") %in% import_errors$raw))

  expect_true(is.data.frame(msgs))
  expect_equal(sort(unique(msgs$source)), c("import", "schema"))
  expect_true(all(msgs$target == table_name))
  import_msgs <- msgs[msgs$source == "import", ]
  expect_equal(nrow(import_msgs), 4)
  # The raw text is recoverable from the message, not just from the
  # structured import_errors frame.
  expect_true(any(grepl("ninety", import_msgs$message, fixed = TRUE)))
  expect_true(any(grepl(">190", import_msgs$message, fixed = TRUE)))

  # ... and from inspect(), the third way a caller can recover the raw value.
  insp <- inspect(dta, source = "memory", as_tibble = FALSE)
  import_insp <- insp[insp$type == "import", ]
  expect_equal(nrow(import_insp), 4)
  expect_true(all(c(">190", "ninety", "unknown", ">300") %in% import_insp$import_raw))
})