test_that("DTADataSetTabular stores validation state per table", {
  ds <- create_example_DTADataSetTabular(2)

  ds <- validate_dataset(ds, tables = "tab1", force = TRUE, persist = FALSE)

  status <- validation_status(ds, tables = "tab1")
  expect_equal(nrow(status), 1)
  expect_equal(status$table, "tab1")
  expect_equal(status$status, "validated")
  expect_true(is.logical(status$ok))

  details <- validation_errors(ds, table = "tab1", source = "memory")
  expect_true(is.list(details))
  expect_true(all(c(
    "ok",
    "schema_valid",
    "rules_valid",
    "n_schema_errors",
    "n_rule_errors",
    "schema_errors",
    "rule_results",
    "rule_errors"
  ) %in% names(details)))
})


test_that("DTADataSetTabular can skip unchanged table validation", {
  ds <- create_example_DTADataSetTabular(2)

  ds <- validate_dataset(ds, tables = "tab1", force = TRUE, persist = FALSE)
  first_summary <- attr(ds, "last_validation_summary")

  ds <- validate_dataset(ds, tables = "tab1", force = FALSE, persist = FALSE)
  second_summary <- attr(ds, "last_validation_summary")

  expect_equal(first_summary$status[[1]], "validated")
  expect_equal(second_summary$status[[1]], "skipped")
})


test_that("DTADataSetTabular can persist and reload validation artifacts", {
  ds <- create_example_DTADataSetTabular(2)
  artifact_dir <- file.path(tempdir(), "dtatools-validation-artifacts-test")
  unlink(artifact_dir, recursive = TRUE, force = TRUE)

  ds <- validate_dataset(
    ds,
    tables = "tab1",
    force = TRUE,
    persist = TRUE,
    artifact_dir = artifact_dir
  )

  index_entry <- ds@validation_index[["tab1"]]
  expect_true(file.exists(index_entry$artifact_path))

  details <- validation_errors(ds, table = "tab1", source = "artifact")
  expect_true(is.list(details))

  ds <- clear_validation(ds, tables = "tab1", remove_artifacts = TRUE)
  expect_true(is.null(ds@validation_index[["tab1"]]))
  expect_true(is.null(ds@validation_store[["tab1"]]))
})
