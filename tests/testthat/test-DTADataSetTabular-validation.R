test_that("DTADataSetTabular stores validation state per table", {
  ds <- create_example_DTADataSetTabular(2)

  ds <- check(ds, tables = "tab1", force = TRUE, persist = FALSE, quiet = TRUE)

  status <- validation_status(ds, tables = "tab1")
  expect_equal(nrow(status), 1)
  expect_equal(status$table, "tab1")
  # `status` records that a run happened, NOT that the data passed. The example
  # fixture deliberately carries 7 column spec errors, so `ok` is FALSE while
  # `status` is "validated" — assert both so the two can never be conflated.
  expect_equal(status$status, "validated")
  expect_false(status$ok)
  expect_equal(status$n_columnspec_errors, 7)
  expect_equal(status$n_rule_errors, 0)

  details <- validation_errors(ds, table = "tab1", source = "memory")
  expect_true(is.list(details))
  expect_true(all(c(
    "ok",
    "columnspec_valid",
    "rules_valid",
    "n_columnspec_errors",
    "n_rule_errors",
    "columnspec_errors",
    "rule_results",
    "rule_errors"
  ) %in% names(details)))

  result_tbl <- results(ds, tables = "tab1")
  expect_true(is.data.frame(result_tbl))
  expect_equal(nrow(result_tbl), 1)
  expect_equal(result_tbl$target, "tab1")
  # Was `if (status$ok) "validated" else "failed"`, which derived the expected
  # value from the object under test and therefore could never fail.
  expect_equal(result_tbl$status, "failed")
  expect_equal(result_tbl$dataset, "demographics")
  expect_equal(result_tbl$n_targets, 1)
  expect_equal(result_tbl$n_validated, 1)
})


test_that("validation_errors() output coerces to a data frame", {
  # Previously the returned list mixed a 5-row summary with a 7-row full error
  # table inside `columnspec_errors`, so as.data.frame() died with
  # "arguments imply differing number of rows: 5, 7".
  ds <- check(
    create_example_DTADataSetTabular(2),
    tables = "tab1",
    persist = FALSE,
    quiet = TRUE
  )
  details <- validation_errors(ds, table = "tab1", source = "memory")

  errors_df <- as.data.frame(details)
  expect_s3_class(errors_df, "data.frame")
  expect_equal(nrow(errors_df), details$n_columnspec_errors)
  expect_true(all(
    c("source", "rule_id", "row", "column", "keyword", "message") %in%
      names(errors_df)
  ))
  expect_true(all(errors_df$source == "columnspec"))
  expect_false(any(is.na(errors_df$message)))

  # The list interface callers already rely on is untouched.
  expect_true(is.list(details))
  expect_true(all(c(
    "ok",
    "columnspec_valid",
    "rules_valid",
    "n_columnspec_errors",
    "n_rule_errors",
    "columnspec_errors",
    "rule_results",
    "rule_errors"
  ) %in% names(details)))
  expect_equal(nrow(as.data.frame(details$columnspec_errors$full_error)), 7)
})

test_that("validation_errors() data frame carries rule failures alongside column spec ones", {
  ds <- create_example_DTADataSetTabular(2)
  ds@specs@rules <- list(create_example_DTARuleColUnique())

  bad_tab <- as.data.frame(ds@tables[["tab1"]])
  bad_tab$SUBJID[3] <- bad_tab$SUBJID[1]
  ds@tables[["tab1"]] <- arrow::arrow_table(bad_tab)

  ds <- check(ds, tables = "tab1", force = TRUE, persist = FALSE, quiet = TRUE)
  details <- validation_errors(ds, table = "tab1", source = "memory")

  errors_df <- as.data.frame(details)
  expect_equal(
    nrow(errors_df),
    details$n_columnspec_errors + details$n_rule_errors
  )

  rule_rows <- errors_df[errors_df$source == "rule", ]
  expect_equal(nrow(rule_rows), 1)
  expect_equal(rule_rows$rule_id, "rule_unique1")
  expect_match(rule_rows$message, "violated")
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

  # `readRDS` of a list is trivially a list, so is.list() proved nothing about
  # the artifact. Compare the round-tripped artifact against memory instead --
  # that is what catches a stale or corrupt file.
  expect_identical(
    validation_errors(ds, table = "tab1", source = "artifact"),
    validation_errors(ds, table = "tab1", source = "memory")
  )

  artifact_path <- index_entry$artifact_path
  ds <- clear_validation(ds, tables = "tab1", remove_artifacts = TRUE)
  expect_true(is.null(ds@validation_index[["tab1"]]))
  expect_true(is.null(ds@validation_store[["tab1"]]))
  # remove_artifacts = TRUE must also delete the file it wrote; without this
  # the branch could be deleted and the suite would stay green.
  expect_false(file.exists(artifact_path))
})

test_that("check() validates a single table by name", {
  ds <- create_example_DTADataSetTabular(2)

  ds <- check(ds, tab = "tab1", quiet = TRUE)
  result <- attr(ds, "last_validation_details")

  expect_true(is.list(result))
  expect_true(all(c("ok", "columnspec_valid", "rules_valid") %in% names(result)))

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

test_that("check() with neither `tab` nor `tables` validates every table", {
  # Was named "check() aborts ...", asserted the opposite of its own title, and
  # the only assertion was expect_true(TRUE) — it passed no matter what check()
  # returned. Assert the real all-tables contract instead.
  ds <- create_example_DTADataSetTabular(2)
  ds@tables[["tab2"]] <- ds@tables[["tab1"]]

  ds <- check(ds, persist = FALSE, quiet = TRUE)

  status <- validation_status(ds)
  expect_setequal(status$table, c("tab1", "tab2"))
  expect_true(all(status$status == "validated"))
  expect_equal(nrow(status), 2)
})

test_that("check() rejects `tab` and `tables` given together", {
  ds <- create_example_DTADataSetTabular(2)
  expect_error(check(ds, tab = "tab1", tables = "tab1"), "Cannot specify both")
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

  # `NULL$specs_hash` is also NULL, so asserting only that the hash is gone
  # would still pass if the whole index entry had been dropped. Pin both.
  entry <- ds@validation_index[["tab1"]]
  expect_false(is.null(entry))
  expect_true(is.null(entry$specs_hash))
  expect_false(is.null(entry$table_hash))

  # The documented point of invalidating is that the next check() re-runs
  # rather than reporting "skipped".
  ds <- check(ds, tab = "tab1", force = FALSE, persist = FALSE, quiet = TRUE)
  expect_equal(attr(ds, "last_validation_summary")$status[[1]], "validated")
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
  ds@specs@rules <- list(bad_rule)

  # Duplicate SUBJID to violate uniqueness rule.
  bad_tab <- as.data.frame(ds@tables[["tab1"]])
  bad_tab$SUBJID[3] <- bad_tab$SUBJID[1]
  ds@tables[["tab1"]] <- arrow::arrow_table(bad_tab)

  ds <- check(ds, tab = "tab1", force = TRUE, persist = FALSE, quiet = TRUE)
  msgs <- messages(ds, tables = "tab1", as_tibble = FALSE)

  # A duplicated SUBJID was injected at row 3, so the unique rule must fire
  # exactly once. `nrow(msgs) >= 1` would also pass if the rule never ran and
  # only pre-existing column spec errors were reported.
  expect_s3_class(msgs, "data.frame")
  rule_msgs <- msgs[msgs$source == "rule", ]
  expect_equal(nrow(rule_msgs), 1)
  expect_match(rule_msgs$message, "violated")
  expect_true(all(unique(msgs$source) %in% c("columnspec", "rule")))
})

test_that("manually added table can be validated without errors", {
  ds <- create_example_DTADataSetTabular(2)

  manual_df <- as.data.frame(ds@tables[["tab1"]])
  manual_df$SUBJID <- paste0(manual_df$SUBJID, "_MANUAL")
  ds@tables[["manual_tab"]] <- arrow::arrow_table(manual_df)

  ds <- check(ds, tables = "manual_tab", force = TRUE, persist = FALSE, quiet = TRUE)

  status <- validation_status(ds, tables = "manual_tab")
  expect_equal(nrow(status), 1)
  expect_equal(status$table, "manual_tab")
  expect_equal(status$status, "validated")

  # The old test was named "without errors" and used expect_no_error() as its
  # only assertion, which said nothing about what validation actually found.
  # The manual table copies tab1 (7 column spec errors) and suffixes SUBJID with
  # "_MANUAL", which adds 2 more SUBJID violations -- 9 in total.
  expect_false(status$ok)
  expect_equal(status$n_columnspec_errors, 9)
  expect_equal(status$n_rule_errors, 0)

  msgs <- messages(ds, tables = "manual_tab", as_tibble = FALSE)
  expect_s3_class(msgs, "data.frame")
  expect_equal(nrow(msgs), 9)

  info <- inspect(ds, as_tibble = FALSE)
  expect_s3_class(info, "data.frame")
  expect_equal(nrow(info), nrow(messages(ds, as_tibble = FALSE)))
})

test_that("multiple manually added tables can be validated without errors", {
  ds <- create_example_DTADataSetTabular(2)

  base_df <- as.data.frame(ds@tables[["tab1"]])
  ds@tables[["manual_tab_a"]] <- arrow::arrow_table(base_df)

  base_df_b <- base_df
  base_df_b$VISIT <- as.character(base_df_b$VISIT)
  ds@tables[["manual_tab_b"]] <- arrow::arrow_table(base_df_b)

  expect_no_error({
    ds <- check(
      ds,
      tables = c("tab1", "manual_tab_a", "manual_tab_b"),
      force = TRUE,
      persist = FALSE,
      quiet = TRUE
    )
  })

  status <- validation_status(ds, tables = c("tab1", "manual_tab_a", "manual_tab_b"))
  expect_equal(nrow(status), 3)
  expect_true(all(status$status == "validated"))

  expect_no_error({
    res <- results(ds, tables = c("tab1", "manual_tab_a", "manual_tab_b"))
    expect_equal(nrow(res), 3)
  })
})
