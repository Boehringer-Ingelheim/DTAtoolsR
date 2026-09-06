# Tests for the STATEFUL / MUTATING wrappers in inst/shiny/dta_app/R/utils_dta.R
# -- the functions through which the Shiny app is allowed to touch a DTA
# object. Pure conversion/formatting helpers (handler_*, dta_to_list(),
# dta_*_signature(), dta_build_validation_report(), etc.) are covered
# elsewhere; this file exercises status classification, load/check, the
# loaded-file lifecycle, session dump/restore, the column and rule editors,
# and the metadata/contact editors.
#
# Every mutator here wraps its body in dta_try() and returns
# list(ok, value, error) instead of throwing. Because that shape degrades
# gracefully to ok = FALSE / value = NULL on almost any internal failure, every
# test asserts on the actual VALUES produced (column ids, rule ids, table
# names, status labels) rather than merely on the list's type.

# ---- Status maps ------------------------------------------------------------

test_that("dta_status_map and dta_table_status_map classify a dataset with no bound data as nodata", {
  status_fn <- app_fn("dta_status_map")
  table_status_fn <- app_fn("dta_table_status_map")
  dta <- app_fixture_dta()

  expect_equal(unname(status_fn(dta)["clinical_data"]), "nodata")
  expect_length(table_status_fn(dta, "clinical_data"), 0)
})

test_that("dta_status_map and dta_table_status_map classify bound-but-unchecked data as pending", {
  status_fn <- app_fn("dta_status_map")
  table_status_fn <- app_fn("dta_table_status_map")
  dta <- app_fixture_dta_with_data(checked = FALSE)

  expect_equal(unname(status_fn(dta)["clinical_data"]), "pending")
  tstat <- table_status_fn(dta, "clinical_data")
  expect_equal(unname(tstat["clinical_data"]), "pending")
})

test_that("dta_status_map and dta_table_status_map classify a clean validated dataset as pass", {
  status_fn <- app_fn("dta_status_map")
  table_status_fn <- app_fn("dta_table_status_map")
  dta <- app_fixture_dta_with_data(checked = TRUE)

  expect_equal(unname(status_fn(dta)["clinical_data"]), "pass")
  tstat <- table_status_fn(dta, "clinical_data")
  expect_equal(unname(tstat["clinical_data"]), "pass")
})

test_that("dta_status_map and dta_table_status_map classify a failing validated dataset as fail", {
  status_fn <- app_fn("dta_status_map")
  table_status_fn <- app_fn("dta_table_status_map")
  dta <- app_fixture_dta_with_data("clinical_data_error_all.csv", checked = TRUE)

  expect_equal(unname(status_fn(dta)["clinical_data"]), "fail")
  tstat <- table_status_fn(dta, "clinical_data")
  expect_equal(unname(tstat["clinical_data_error_all"]), "fail")
})

# ---- Dataset readiness -------------------------------------------------------

test_that("dta_dataset_readiness flips from not-ready to ready once data is bound", {
  fn <- app_fn("dta_dataset_readiness")

  r0 <- fn(app_fixture_dta(), "clinical_data")
  expect_equal(r0$count, 0L)
  expect_equal(r0$min, 1)
  expect_false(r0$has_data)
  expect_false(r0$ready)

  r1 <- fn(app_fixture_dta_with_data(checked = FALSE), "clinical_data")
  expect_equal(r1$count, 1L)
  expect_equal(r1$min, 1)
  expect_true(r1$has_data)
  expect_true(r1$ready)
})

# ---- Loading + checking -------------------------------------------------------

test_that("dta_load_file returns ok=TRUE and a DTA with the table bound", {
  fn <- app_fn("dta_load_file")
  res <- fn(
    app_fixture_dta(),
    dataset = "clinical_data",
    file = app_fixture_path("clinical_data.csv"),
    handler_index = 1
  )

  expect_true(res$ok)
  expect_null(res$error)
  ds <- datasets(res$value, "clinical_data")
  expect_true("clinical_data" %in% names(ds@tables))
})

test_that("dta_check marks a cleanly loaded dataset as passing", {
  load_fn <- app_fn("dta_load_file")
  check_fn <- app_fn("dta_check")
  status_fn <- app_fn("dta_status_map")

  loaded <- load_fn(
    app_fixture_dta(),
    dataset = "clinical_data",
    file = app_fixture_path("clinical_data.csv"),
    handler_index = 1
  )
  checked <- check_fn(loaded$value, dataset = "clinical_data")

  expect_true(checked$ok)
  expect_null(checked$error)
  expect_s3_class(checked$value, "DTAtools::DTA")
  expect_equal(unname(status_fn(checked$value)["clinical_data"]), "pass")
})

test_that("dta_check marks a dataset with column spec/rule errors as failing", {
  load_fn <- app_fn("dta_load_file")
  check_fn <- app_fn("dta_check")
  status_fn <- app_fn("dta_status_map")

  loaded <- load_fn(
    app_fixture_dta(),
    dataset = "clinical_data",
    file = app_fixture_path("clinical_data_error_all.csv"),
    handler_index = 1
  )
  checked <- check_fn(loaded$value, dataset = "clinical_data")

  expect_true(checked$ok)
  expect_equal(unname(status_fn(checked$value)["clinical_data"]), "fail")
})

test_that("dta_load_file returns ok=FALSE without throwing for a nonexistent file path", {
  fn <- app_fn("dta_load_file")
  bad_path <- file.path(tempdir(), "does_not_exist_dta_test_12345.csv")

  res <- fn(app_fixture_dta(), dataset = "clinical_data", file = bad_path, handler_index = 1)

  expect_false(res$ok)
  expect_null(res$value)
  expect_true(nzchar(res$error))
})

test_that("dta_load_file returns ok=FALSE without throwing for a nonexistent dataset name", {
  fn <- app_fn("dta_load_file")

  res <- fn(
    app_fixture_dta(),
    dataset = "no_such_dataset",
    file = app_fixture_path("clinical_data.csv"),
    handler_index = 1
  )

  expect_false(res$ok)
  expect_null(res$value)
  expect_true(nzchar(res$error))
})

test_that("dta_check returns ok=FALSE without throwing for a nonexistent dataset name", {
  fn <- app_fn("dta_check")

  res <- fn(app_fixture_dta_with_data(checked = FALSE), dataset = "no_such_dataset")

  expect_false(res$ok)
  expect_true(nzchar(res$error))
})

# ---- Per-dataset messages -----------------------------------------------------

test_that("dta_dataset_messages is empty before checking and populated with expected columns after", {
  fn <- app_fn("dta_dataset_messages")

  before <- fn(app_fixture_dta_with_data(checked = FALSE), "clinical_data")
  expect_equal(nrow(before), 0)

  after <- fn(app_fixture_dta_with_data("clinical_data_error_all.csv", checked = TRUE), "clinical_data")
  expect_gt(nrow(after), 0)
  expect_true(all(c("source", "column", "message", "severity") %in% names(after)))
  # clinical_data_error_all.csv now also carries import errors (see
  # test-clinical-error-fixtures.R), so "import" joins the source set.
  expect_setequal(unique(after$source), c("columnspec", "rule", "import"))
})

test_that("dta_dataset_messages returns an empty data.frame for a nonexistent dataset", {
  fn <- app_fn("dta_dataset_messages")

  msgs <- fn(app_fixture_dta(), "no_such_dataset")

  expect_equal(nrow(msgs), 0)
})

# ---- Unloading tables and clearing validation ---------------------------------

test_that("dta_unload_table removes both the table and its validation state", {
  fn <- app_fn("dta_unload_table")
  dta <- app_fixture_dta_with_data(checked = TRUE)
  ds_before <- datasets(dta, "clinical_data")
  expect_true("clinical_data" %in% names(ds_before@tables))
  expect_true("clinical_data" %in% names(ds_before@validation_index))

  res <- fn(dta, "clinical_data", "clinical_data")

  expect_true(res$ok)
  ds_after <- datasets(res$value, "clinical_data")
  expect_false("clinical_data" %in% names(ds_after@tables))
  expect_false("clinical_data" %in% names(ds_after@validation_index))
  expect_false("clinical_data" %in% names(ds_after@validation_store))
})

test_that("dta_unload_table returns ok=FALSE without throwing for a nonexistent dataset", {
  fn <- app_fn("dta_unload_table")

  res <- fn(app_fixture_dta_with_data(checked = TRUE), "no_such_dataset", "clinical_data")

  expect_false(res$ok)
  expect_true(nzchar(res$error))
})

test_that("dta_unload_all clears every table and all validation state", {
  fn <- app_fn("dta_unload_all")

  res <- fn(app_fixture_dta_with_data(checked = TRUE), "clinical_data")

  expect_true(res$ok)
  ds <- datasets(res$value, "clinical_data")
  expect_length(ds@tables, 0)
  expect_length(ds@validation_index, 0)
  expect_length(ds@validation_store, 0)
})

test_that("dta_unload_all returns ok=FALSE without throwing for a nonexistent dataset", {
  fn <- app_fn("dta_unload_all")

  res <- fn(app_fixture_dta_with_data(checked = TRUE), "no_such_dataset")

  expect_false(res$ok)
  expect_true(nzchar(res$error))
})

test_that("dta_clear_validation resets status to pending while leaving the table bound", {
  clear_fn <- app_fn("dta_clear_validation")
  status_fn <- app_fn("dta_table_status_map")
  dta <- app_fixture_dta_with_data(checked = TRUE)
  expect_equal(unname(status_fn(dta, "clinical_data")["clinical_data"]), "pass")

  res <- clear_fn(dta, "clinical_data")

  expect_true(res$ok)
  ds <- datasets(res$value, "clinical_data")
  expect_true("clinical_data" %in% names(ds@tables))
  after <- status_fn(res$value, "clinical_data")
  expect_equal(unname(after["clinical_data"]), "pending")
})

test_that("dta_clear_validation returns ok=FALSE without throwing for a nonexistent dataset", {
  fn <- app_fn("dta_clear_validation")

  res <- fn(app_fixture_dta_with_data(checked = TRUE), "no_such_dataset")

  expect_false(res$ok)
  expect_true(nzchar(res$error))
})

# ---- Session dump / restore ---------------------------------------------------

test_that("dta_dump_session -> dta_restore_session preserves datasets, tables, and validation state", {
  dump_fn <- app_fn("dta_dump_session")
  restore_fn <- app_fn("dta_restore_session")
  names_fn <- app_fn("dta_dataset_names")
  table_names_fn <- app_fn("dta_dataset_table_names")
  status_fn <- app_fn("dta_status_map")

  dta <- app_fixture_dta_with_data(checked = TRUE)
  orig_ds <- datasets(dta, "clinical_data")
  orig_nrow <- nrow(as.data.frame(orig_ds@tables[["clinical_data"]]))

  dump <- dump_fn(dta)
  restored <- restore_fn(dump)

  expect_equal(names_fn(restored), names_fn(dta))
  restored_ds <- datasets(restored, "clinical_data")
  expect_equal(table_names_fn(restored_ds), table_names_fn(orig_ds))
  expect_equal(nrow(as.data.frame(restored_ds@tables[["clinical_data"]])), orig_nrow)
  expect_equal(unname(status_fn(restored)["clinical_data"]), "pass")
})

# ---- Transferring bound data across a spec re-parse ----------------------------

test_that("dta_transfer_bound_data with keep_validation=TRUE carries the table and its validation state", {
  fn <- app_fn("dta_transfer_bound_data")
  old_ds <- datasets(app_fixture_dta_with_data(checked = TRUE), "clinical_data")

  res <- fn(app_fixture_dta(), "clinical_data", old_ds, keep_validation = TRUE)

  expect_true(res$ok)
  new_ds <- datasets(res$value, "clinical_data")
  expect_true("clinical_data" %in% names(new_ds@tables))
  expect_true("clinical_data" %in% names(new_ds@validation_index))
  expect_true("clinical_data" %in% names(new_ds@validation_store))
})

test_that("dta_transfer_bound_data with keep_validation=FALSE carries the table but drops validation state", {
  fn <- app_fn("dta_transfer_bound_data")
  old_ds <- datasets(app_fixture_dta_with_data(checked = TRUE), "clinical_data")

  res <- fn(app_fixture_dta(), "clinical_data", old_ds, keep_validation = FALSE)

  expect_true(res$ok)
  new_ds <- datasets(res$value, "clinical_data")
  expect_true("clinical_data" %in% names(new_ds@tables))
  expect_length(new_ds@validation_index, 0)
  expect_length(new_ds@validation_store, 0)
})

test_that("dta_transfer_bound_data returns ok=FALSE without throwing for a nonexistent target dataset", {
  fn <- app_fn("dta_transfer_bound_data")
  old_ds <- datasets(app_fixture_dta_with_data(checked = TRUE), "clinical_data")

  res <- fn(app_fixture_dta(), "no_such_dataset", old_ds, keep_validation = TRUE)

  expect_false(res$ok)
  expect_true(nzchar(res$error))
})

# ---- Column editor -------------------------------------------------------------

test_that("dta_set_column adds a new column at the end of the id order", {
  fn <- app_fn("dta_set_column")
  ids_fn <- app_fn("dta_column_ids")
  dta <- app_fixture_dta()
  before <- ids_fn(dta, "clinical_data")

  res <- fn(
    dta, "clinical_data",
    id = "NEWCOL", label = "New Column", backend = "SAS",
    type = "Char", length = "20", nullable = TRUE, description = "a new column"
  )

  expect_true(res$ok)
  expect_equal(ids_fn(res$value, "clinical_data"), c(before, "NEWCOL"))
  col <- datasets(res$value, "clinical_data")@specs@columns[["NEWCOL"]]
  expect_equal(col@label, "New Column")
  expect_equal(col@structure@backend, "SAS")
  expect_equal(col@structure@type, "Char")
  expect_equal(as.numeric(col@structure@length), 20)
  expect_true(col@nullable)
})

test_that("dta_set_column modifies an existing column in place without changing the id order", {
  fn <- app_fn("dta_set_column")
  ids_fn <- app_fn("dta_column_ids")
  dta <- app_fixture_dta()
  before <- ids_fn(dta, "clinical_data")

  res <- fn(
    dta, "clinical_data",
    id = "STUDYID", label = "Updated Label", backend = "SAS",
    type = "Char", length = "15", nullable = FALSE, description = "updated"
  )

  expect_true(res$ok)
  expect_equal(ids_fn(res$value, "clinical_data"), before)
  col <- datasets(res$value, "clinical_data")@specs@columns[["STUDYID"]]
  expect_equal(col@label, "Updated Label")
  expect_equal(as.numeric(col@structure@length), 15)
  expect_false(col@nullable)
})

test_that("dta_set_column parses backend/type/format/length out of their string inputs", {
  fn <- app_fn("dta_set_column")
  dta <- app_fixture_dta()

  res <- fn(dta, "clinical_data", id = "SCORE", backend = "SAS", type = "Num", format = "8.2", length = "8")

  expect_true(res$ok)
  st <- datasets(res$value, "clinical_data")@specs@columns[["SCORE"]]@structure
  expect_equal(st@backend, "SAS")
  expect_equal(st@type, "Num")
  expect_equal(st@format, "8.2")
  expect_equal(as.numeric(st@length), 8)
})

test_that("dta_set_column silently drops a non-numeric length instead of storing it", {
  fn <- app_fn("dta_set_column")
  dta <- app_fixture_dta()

  res <- fn(dta, "clinical_data", id = "BADLEN", type = "Char", length = "not-a-number")

  expect_true(res$ok)
  st <- datasets(res$value, "clinical_data")@specs@columns[["BADLEN"]]@structure
  expect_null(st@length)
})

test_that("dta_set_column returns ok=FALSE without throwing for an id containing whitespace", {
  fn <- app_fn("dta_set_column")

  res <- fn(app_fixture_dta(), "clinical_data", id = "bad id")

  expect_false(res$ok)
  expect_true(nzchar(res$error))
})

test_that("dta_remove_column drops the column from the id order", {
  fn <- app_fn("dta_remove_column")
  ids_fn <- app_fn("dta_column_ids")
  dta <- app_fixture_dta()
  before <- ids_fn(dta, "clinical_data")

  res <- fn(dta, "clinical_data", "AGE")

  expect_true(res$ok)
  expect_equal(ids_fn(res$value, "clinical_data"), setdiff(before, "AGE"))
})

test_that("dta_remove_column returns ok=FALSE without throwing for a nonexistent dataset", {
  fn <- app_fn("dta_remove_column")

  res <- fn(app_fixture_dta(), "no_such_dataset", "AGE")

  expect_false(res$ok)
  expect_true(nzchar(res$error))
})

test_that("dta_move_column swaps a column with its neighbour, up and down", {
  fn <- app_fn("dta_move_column")
  ids_fn <- app_fn("dta_column_ids")
  dta <- app_fixture_dta()
  ids0 <- ids_fn(dta, "clinical_data")

  res_up <- fn(dta, "clinical_data", ids0[2], "up")
  expect_true(res_up$ok)
  ids_up <- ids_fn(res_up$value, "clinical_data")
  expect_equal(ids_up[1], ids0[2])
  expect_equal(ids_up[2], ids0[1])
  expect_equal(ids_up[-(1:2)], ids0[-(1:2)])

  res_down <- fn(dta, "clinical_data", ids0[1], "down")
  expect_true(res_down$ok)
  ids_down <- ids_fn(res_down$value, "clinical_data")
  expect_equal(ids_down[1], ids0[2])
  expect_equal(ids_down[2], ids0[1])
})

test_that("dta_move_column at the boundary is a no-op", {
  fn <- app_fn("dta_move_column")
  ids_fn <- app_fn("dta_column_ids")
  dta <- app_fixture_dta()
  ids0 <- ids_fn(dta, "clinical_data")

  res_first_up <- fn(dta, "clinical_data", ids0[1], "up")
  expect_true(res_first_up$ok)
  expect_equal(ids_fn(res_first_up$value, "clinical_data"), ids0)

  res_last_down <- fn(dta, "clinical_data", ids0[length(ids0)], "down")
  expect_true(res_last_down$ok)
  expect_equal(ids_fn(res_last_down$value, "clinical_data"), ids0)
})

test_that("dta_move_column returns ok=FALSE without throwing for an unknown column id", {
  fn <- app_fn("dta_move_column")

  res <- fn(app_fixture_dta(), "clinical_data", "NO_SUCH_COLUMN", "up")

  expect_false(res$ok)
  expect_true(nzchar(res$error))
})

# ---- Rule editor -----------------------------------------------------------------

test_that("dta_build_rule dispatches to DTARuleColCondition for a col_condition rule", {
  fn <- app_fn("dta_build_rule")

  rule <- fn(
    id = "r_cond", type = "col_condition",
    condition = list(VISIT = list(equals = "V03")),
    then = list(STATUS = list(equals = "COMPLETED"))
  )

  expect_s3_class(rule, "DTAtools::DTARuleColCondition")
  expect_equal(rule@id, "r_cond")
  expect_equal(rule@condition, list(VISIT = list(equals = "V03")))
  expect_equal(rule@then, list(STATUS = list(equals = "COMPLETED")))
})

test_that("dta_build_rule dispatches to DTARuleColRange for a col_range rule", {
  fn <- app_fn("dta_build_rule")

  rule <- fn(id = "r_range", type = "col_range", columns = "AGE", min = 18, max = 65)

  expect_s3_class(rule, "DTAtools::DTARuleColRange")
  expect_equal(rule@columns, "AGE")
  expect_equal(rule@min, 18)
  expect_equal(rule@max, 65)
})

test_that("dta_build_rule dispatches to DTARuleColUnique for a col_unique rule", {
  fn <- app_fn("dta_build_rule")

  rule <- fn(id = "r_unique", type = "col_unique", columns = c("SUBJECT_ID", "VISIT"))

  expect_s3_class(rule, "DTAtools::DTARuleColUnique")
  expect_equal(rule@columns, c("SUBJECT_ID", "VISIT"))
})

test_that("dta_build_rule dispatches to DTARuleGroupCondition for a group_condition rule", {
  fn <- app_fn("dta_build_rule")

  rule <- fn(
    id = "r_group",
    type = "group_condition",
    group_by = c("SUBJECT_ID", "VISIT"),
    conditions = list(
      c1 = list(STATUS = list(equals = "FAILED")),
      c2 = list(RESULT = list(empty = FALSE))
    ),
    constraints = list(list(type = "requires", `if` = "c1", then = "c2"))
  )

  expect_s3_class(rule, "DTAtools::DTARuleGroupCondition")
  expect_equal(rule@group_by, c("SUBJECT_ID", "VISIT"))
  expect_equal(names(rule@conditions), c("c1", "c2"))
  expect_equal(rule@constraints[[1]]$type, "requires")
})

test_that("dta_build_rule errors for an unknown rule type", {
  fn <- app_fn("dta_build_rule")

  expect_error(fn(id = "r_bad", type = "not_a_rule_type"), "Unknown rule type", fixed = TRUE)
})

test_that("dta_set_rule appends a new rule at the end of the rule order", {
  fn <- app_fn("dta_set_rule")
  dta <- app_fixture_dta()
  before_ids <- vapply(datasets(dta, "clinical_data")@specs@rules, function(r) r@id, character(1))

  res <- fn(dta, "clinical_data", id = "new_rule", type = "col_unique", columns = "STUDYID")

  expect_true(res$ok)
  after_ids <- vapply(datasets(res$value, "clinical_data")@specs@rules, function(r) r@id, character(1))
  expect_equal(after_ids, c(before_ids, "new_rule"))
})

test_that("dta_set_rule replaces a rule at a given index without touching the others", {
  fn <- app_fn("dta_set_rule")
  dta <- app_fixture_dta()
  before_ids <- vapply(datasets(dta, "clinical_data")@specs@rules, function(r) r@id, character(1))

  res <- fn(dta, "clinical_data",
    index = 3, id = "rule_range_example", type = "col_range",
    columns = "AGE", min = 21, max = 60
  )

  expect_true(res$ok)
  rules <- datasets(res$value, "clinical_data")@specs@rules
  after_ids <- vapply(rules, function(r) r@id, character(1))
  expect_length(rules, length(before_ids))
  expect_equal(after_ids[-3], before_ids[-3])
  expect_equal(after_ids[3], "rule_range_example")
  expect_equal(rules[[3]]@min, 21)
  expect_equal(rules[[3]]@max, 60)
})

test_that("dta_set_rule returns ok=FALSE without throwing for an unknown rule type", {
  fn <- app_fn("dta_set_rule")

  res <- fn(app_fixture_dta(), "clinical_data", id = "bad", type = "not_a_rule_type")

  expect_false(res$ok)
  expect_true(nzchar(res$error))
})

test_that("dta_remove_rule drops a rule by index and shifts the remainder up", {
  fn <- app_fn("dta_remove_rule")
  dta <- app_fixture_dta()
  before_ids <- vapply(datasets(dta, "clinical_data")@specs@rules, function(r) r@id, character(1))

  res <- fn(dta, "clinical_data", 1)

  expect_true(res$ok)
  after_ids <- vapply(datasets(res$value, "clinical_data")@specs@rules, function(r) r@id, character(1))
  expect_equal(after_ids, before_ids[-1])
})

test_that("dta_remove_rule returns ok=FALSE without throwing for a nonexistent dataset", {
  fn <- app_fn("dta_remove_rule")

  res <- fn(app_fixture_dta(), "no_such_dataset", 1)

  expect_false(res$ok)
  expect_true(nzchar(res$error))
})

test_that("dta_move_rule swaps two adjacent rules", {
  fn <- app_fn("dta_move_rule")
  dta <- app_fixture_dta()
  before_ids <- vapply(datasets(dta, "clinical_data")@specs@rules, function(r) r@id, character(1))

  res <- fn(dta, "clinical_data", 2, "up")

  expect_true(res$ok)
  after_ids <- vapply(datasets(res$value, "clinical_data")@specs@rules, function(r) r@id, character(1))
  expect_equal(after_ids[1], before_ids[2])
  expect_equal(after_ids[2], before_ids[1])
  expect_equal(after_ids[-(1:2)], before_ids[-(1:2)])
})

test_that("dta_move_rule at the boundary is a no-op", {
  fn <- app_fn("dta_move_rule")
  dta <- app_fixture_dta()
  before_ids <- vapply(datasets(dta, "clinical_data")@specs@rules, function(r) r@id, character(1))

  res <- fn(dta, "clinical_data", 1, "up")

  expect_true(res$ok)
  after_ids <- vapply(datasets(res$value, "clinical_data")@specs@rules, function(r) r@id, character(1))
  expect_equal(after_ids, before_ids)
})

test_that("dta_move_rule returns ok=FALSE without throwing for a nonexistent dataset", {
  fn <- app_fn("dta_move_rule")

  res <- fn(app_fixture_dta(), "no_such_dataset", 1, "up")

  expect_false(res$ok)
  expect_true(nzchar(res$error))
})

# ---- Metadata: scalar fields ------------------------------------------------------

test_that("dta_set_metadata_field sets and reads back a scalar field", {
  fn <- app_fn("dta_set_metadata_field")

  res <- fn(app_fixture_dta(), "title", "New Title")

  expect_true(res$ok)
  expect_equal(metadata(res$value)@title, "New Title")
})

test_that("dta_set_metadata_field unsets an optional nullable field on blank input", {
  fn <- app_fn("dta_set_metadata_field")

  res <- fn(app_fixture_dta(), "date", "")

  expect_true(res$ok)
  expect_null(metadata(res$value)@date)
})

test_that("dta_set_metadata_field stores a scalar field trimmed", {
  # The function's own blank test treats surrounding whitespace as absence of
  # content, so storing it would contradict a rule it has already applied. It
  # matters most for the two fields that identify the document: an untrimmed
  # title reaches the exported document and the version-history diff, where it
  # reads as a change to a version that renders identically to the last one.
  fn <- app_fn("dta_set_metadata_field")

  res <- fn(app_fixture_dta(), "title", "  Padded Title  ")
  expect_true(res$ok)
  expect_equal(metadata(res$value)@title, "Padded Title")

  res_v <- fn(app_fixture_dta(), "version", "\t2.0\n")
  expect_true(res_v$ok)
  expect_equal(metadata(res_v$value)@version, "2.0")
})

test_that("dta_set_metadata_field leaves a non-character field alone when trimming", {
  # authorized_for_corrections is character OR list; trimws() on a list would
  # corrupt it, which is why the trim is guarded on is.character(). A list
  # value has to survive the round trip untouched.
  fn <- app_fn("dta_set_metadata_field")

  res <- fn(app_fixture_dta(), "authorized_for_corrections", list("  Alice  ", "Bob"))

  expect_true(res$ok)
  expect_equal(metadata(res$value)@authorized_for_corrections, list("  Alice  ", "Bob"))
})

test_that("dta_set_metadata_field returns ok=FALSE without throwing for an unknown field", {
  fn <- app_fn("dta_set_metadata_field")

  res <- fn(app_fixture_dta(), "not_a_real_field", "x")

  expect_false(res$ok)
  expect_true(nzchar(res$error))
})

# ---- Metadata: contacts --------------------------------------------------------------

test_that("dta_add_contact appends a new contact to the given side", {
  add_fn <- app_fn("dta_add_contact")
  contacts_fn <- app_fn("dta_contacts")
  dta <- app_fixture_dta()
  before <- contacts_fn(dta, "receiver")

  res <- add_fn(dta, "receiver", name = "New Person", roles = "Tester", email = "new@example.com")

  expect_true(res$ok)
  after <- contacts_fn(res$value, "receiver")
  expect_length(after, length(before) + 1)
  expect_equal(after[[length(after)]]$name, "New Person")
  expect_equal(after[[length(after)]]$email, "new@example.com")
  expect_equal(after[[length(after)]]$role, "Tester")
})

test_that("dta_add_contact returns ok=FALSE without throwing for an invalid side", {
  fn <- app_fn("dta_add_contact")

  res <- fn(app_fixture_dta(), "not_a_side", name = "X")

  expect_false(res$ok)
  expect_true(nzchar(res$error))
})

test_that("dta_remove_contact removes a contact by index and shifts the remainder up", {
  remove_fn <- app_fn("dta_remove_contact")
  contacts_fn <- app_fn("dta_contacts")
  dta <- app_fixture_dta()
  before <- contacts_fn(dta, "receiver")

  res <- remove_fn(dta, "receiver", 1)

  expect_true(res$ok)
  after <- contacts_fn(res$value, "receiver")
  expect_length(after, length(before) - 1)
  expect_equal(after[[1]]$name, before[[2]]$name)
})

test_that("dta_remove_contact returns ok=FALSE without throwing for an invalid side", {
  fn <- app_fn("dta_remove_contact")

  res <- fn(app_fixture_dta(), "not_a_side", 1)

  expect_false(res$ok)
  expect_true(nzchar(res$error))
})

test_that("dta_update_contact updates the given fields and preserves the rest", {
  update_fn <- app_fn("dta_update_contact")
  contact_at_fn <- app_fn("dta_contact_at")
  dta <- app_fixture_dta()
  original_name <- contact_at_fn(dta, "receiver", 1)$name

  res <- update_fn(dta, "receiver", 1, list(email = "updated@example.com"))

  expect_true(res$ok)
  updated <- contact_at_fn(res$value, "receiver", 1)
  expect_equal(updated$email, "updated@example.com")
  expect_equal(updated$name, original_name)
})

test_that("dta_update_contact returns ok=FALSE without throwing for an out-of-range index", {
  fn <- app_fn("dta_update_contact")

  res <- fn(app_fixture_dta(), "receiver", 99, list(email = "x@example.com"))

  expect_false(res$ok)
  expect_true(nzchar(res$error))
})

# ---- Metadata: affiliation ------------------------------------------------------------

test_that("dta_set_affiliation sets and reads back affiliation fields", {
  fn <- app_fn("dta_set_affiliation")
  aff_fn <- app_fn("dta_affiliation")

  res <- fn(app_fixture_dta(), "receiver", name = "New Org", country = "Testland")

  expect_true(res$ok)
  aff <- aff_fn(res$value, "receiver")
  expect_equal(aff$name, "New Org")
  expect_equal(aff$country, "Testland")
})

test_that("dta_set_affiliation returns ok=FALSE without throwing for an invalid side", {
  fn <- app_fn("dta_set_affiliation")

  res <- fn(app_fixture_dta(), "not_a_side", name = "X")

  expect_false(res$ok)
  expect_true(nzchar(res$error))
})

# ---- Metadata: transmission -----------------------------------------------------------

test_that("dta_set_transmission_field sets and reads back a value", {
  fn <- app_fn("dta_set_transmission_field")
  tr_fn <- app_fn("dta_transmission")

  res <- fn(app_fixture_dta(), "frequency", "recurring")

  expect_true(res$ok)
  expect_equal(tr_fn(res$value)$frequency, "recurring")
})

test_that("dta_set_transmission_field drops the field when the value is blank", {
  fn <- app_fn("dta_set_transmission_field")
  tr_fn <- app_fn("dta_transmission")

  res <- fn(app_fixture_dta(), "frequency", "")

  expect_true(res$ok)
  expect_null(tr_fn(res$value)$frequency)
})
