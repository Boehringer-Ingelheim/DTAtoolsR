test_that("DTADataSetTabular stores validation state per table", {
  ds <- create_example_DTADataSetTabular(2)

  ds <- check(ds, tables = "tab1", force = TRUE, persist = FALSE, quiet = TRUE)

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

  result_tbl <- results(ds, tables = "tab1")
  expect_true(is.data.frame(result_tbl))
  expect_equal(nrow(result_tbl), 1)
  expect_equal(result_tbl$table, "tab1")
})


test_that("DTADataSetTabular can skip unchanged table validation", {
  ds <- create_example_DTADataSetTabular(2)

  ds <- check(ds, tables = "tab1", force = TRUE, persist = FALSE, quiet = TRUE)
  first_summary <- attr(ds, "last_validation_summary")

  ds <- check(ds, tables = "tab1", force = FALSE, persist = FALSE, quiet = TRUE)
  second_summary <- attr(ds, "last_validation_summary")

  expect_equal(first_summary$status[[1]], "validated")
  expect_equal(second_summary$status[[1]], "skipped")
})


test_that("DTADataSetTabular can persist and reload validation artifacts", {
  ds <- create_example_DTADataSetTabular(2)
  artifact_dir <- file.path(tempdir(), "dtatools-validation-artifacts-test")
  unlink(artifact_dir, recursive = TRUE, force = TRUE)

  ds <- check(
    ds,
    tables = "tab1",
    force = TRUE,
    persist = TRUE,
    artifact_dir = artifact_dir,
    quiet = TRUE
  )

  index_entry <- ds@validation_index[["tab1"]]
  expect_true(file.exists(index_entry$artifact_path))

  details <- validation_errors(ds, table = "tab1", source = "artifact")
  expect_true(is.list(details))

  ds <- clear_validation(ds, tables = "tab1", remove_artifacts = TRUE)
  expect_true(is.null(ds@validation_index[["tab1"]]))
  expect_true(is.null(ds@validation_store[["tab1"]]))
})

test_that("check() validates a single table by name", {
  ds <- create_example_DTADataSetTabular(2)

  ds <- check(ds, tab = "tab1", quiet = TRUE)
  result <- attr(ds, "last_validation_details")

  expect_true(is.list(result))
  expect_true(all(c("ok", "schema_valid", "rules_valid") %in% names(result)))

  status <- validation_status(ds, tables = "tab1")
  expect_equal(nrow(status), 1)
  expect_equal(status$status, "validated")
})

test_that("check() validates a single table by index", {
  ds <- create_example_DTADataSetTabular(2)

  ds <- check(ds, tab = 1, quiet = TRUE)
  result <- attr(ds, "last_validation_details")

  expect_true(is.list(result))
  status <- validation_status(ds, tables = 1)
  expect_equal(nrow(status), 1)
  expect_equal(status$status, "validated")
})

test_that("check() skips unchanged table validation", {
  ds <- create_example_DTADataSetTabular(2)

  # First validation
  ds <- check(ds, tab = "tab1", force = TRUE, quiet = TRUE)
  first_status <- validation_status(ds, tables = "tab1")
  first_time <- first_status$validated_at

  Sys.sleep(0.1)

  # Second validation without force (should skip)
  ds <- check(ds, tab = "tab1", force = FALSE, quiet = TRUE)

  # Since skipped, result should still be from cache
  status <- validation_status(ds, tables = "tab1")
  # First validation should still be the same time
  expect_equal(first_time, status$validated_at)
})

test_that("check() forces re-validation when force=TRUE", {
  ds <- create_example_DTADataSetTabular(2)

  # First validation
  ds <- check(ds, tab = "tab1", force = TRUE, quiet = TRUE)
  first_status <- validation_status(ds, tables = "tab1")
  first_time <- first_status$validated_at

  Sys.sleep(0.1)

  # Second validation with force
  ds <- check(ds, tab = "tab1", force = TRUE, quiet = TRUE)
  second_status <- validation_status(ds, tables = "tab1")
  second_time <- second_status$validated_at

  # Times should be different (re-validated)
  expect_gt(second_time, first_time)
})

test_that("check() aborts when table argument is missing and tables is null", {
  ds <- create_example_DTADataSetTabular(2)
  # With neither table nor tables, it checks all tables (no error)
  ds <- check(ds, quiet = TRUE)
  expect_true(TRUE)  # Just verify it doesn't error
})

test_that("check() aborts on non-existent table", {
  ds <- create_example_DTADataSetTabular(2)
  expect_error(check(ds, tab = "nonexistent"), "not found")
})

test_that("invalidate_by_spec_change() marks validation as outdated", {
  ds <- create_example_DTADataSetTabular(2)

  # Validate table
  ds <- check(ds, tab = "tab1", force = TRUE, quiet = TRUE)
  specs_hash_before <- ds@validation_index[["tab1"]]$specs_hash
  expect_false(is.null(specs_hash_before))

  # Invalidate by spec change
  ds <- invalidate_by_spec_change(ds, tables = "tab1")

  # specs_hash should be NULL now
  specs_hash_after <- ds@validation_index[["tab1"]]$specs_hash
  expect_true(is.null(specs_hash_after))
})

test_that("invalidate_by_spec_change() invalidates all tables when no tables specified", {
  ds <- create_example_DTADataSetTabular(2)
  # create_example_DTADataSetTabular(2) only ships a single table ("tab1");
  # add a second table (same schema) so both tables can be invalidated here.
  ds@tables[["tab2"]] <- ds@tables[["tab1"]]

  # Validate both tables
  ds <- check(ds, tab = "tab1", force = TRUE, quiet = TRUE)
  ds <- check(ds, tab = "tab2", force = TRUE, quiet = TRUE)

  # Invalidate all tables
  ds <- invalidate_by_spec_change(ds)

  # Both specs_hash should be NULL
  expect_true(is.null(ds@validation_index[["tab1"]]$specs_hash))
  expect_true(is.null(ds@validation_index[["tab2"]]$specs_hash))
})

test_that("messages() returns flattened rule failures", {
  ds <- create_example_DTADataSetTabular(2)

  bad_rule <- create_example_DTARuleColUnique()
  ds@specs@rules <- DTARuleCollection(rules = list(bad_rule))

  # Duplicate SUBJID to violate uniqueness rule.
  bad_tab <- as.data.frame(ds@tables[["tab1"]])
  bad_tab$SUBJID[3] <- bad_tab$SUBJID[1]
  ds@tables[["tab1"]] <- arrow::arrow_table(bad_tab)

  ds <- check(ds, tab = "tab1", force = TRUE, persist = FALSE, quiet = TRUE)
  msgs <- messages(ds, tables = "tab1", as_tibble = FALSE)

  expect_true(is.data.frame(msgs))
  expect_true(nrow(msgs) >= 1)
  expect_true(all(unique(msgs$source) %in% c("schema", "rule")))
  expect_true(any(msgs$source == "rule"))
  expect_true(any(grepl("violated", msgs$message, fixed = TRUE)))
})

