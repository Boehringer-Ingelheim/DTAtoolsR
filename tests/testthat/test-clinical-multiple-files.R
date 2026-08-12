load_clinical_multi_dta <- function(filenames) {
  spec_path <- system.file("extdata", "clinical_dta_multiple_files.yaml", package = "DTAtools")
  # Guaranteed package assets — a missing fixture is a failure, not a skip.
  expect_true(
    nzchar(spec_path),
    info = "clinical_dta_multiple_files.yaml missing from extdata"
  )

  dta <- read_dta_from_yaml(spec_path)

  for (filename in filenames) {
    fixture_path <- system.file("extdata", filename, package = "DTAtools")
    expect_true(nzchar(fixture_path), info = paste(filename, "missing from extdata"))
    dta <- load_file(dta, 1, file = fixture_path)
  }

  check(dta, persist = FALSE, quiet = TRUE)
}

test_that("clinical multi-file clean fixtures validate without schema or rule errors", {
  dta <- load_clinical_multi_dta(c("clinical_data.csv", "clinical_data2.csv"))

  res <- results(dta)
  expect_true(is.data.frame(res))
  expect_equal(sort(res$target), c("clinical_data", "clinical_data2"))
  expect_true(all(res$status == "validated"))
  expect_true(all(res$n_schema_errors == 0L))
  expect_true(all(res$n_rule_errors == 0L))

  ds <- dta[["clinical_data"]]
  status <- validation_status(ds)
  expect_equal(nrow(status), 2)
  expect_equal(sort(status$table), c("clinical_data", "clinical_data2"))
  expect_true(all(status$status == "validated"))
  expect_true(all(status$ok %in% TRUE))

  for (table_name in c("clinical_data", "clinical_data2")) {
    details <- validation_errors(ds, table = table_name, source = "memory")
    expect_true(is.list(details))
    expect_true(isTRUE(details$ok))
    expect_true(isTRUE(details$schema_valid))
    expect_true(isTRUE(details$rules_valid))
    expect_equal(details$n_schema_errors, 0L)
    expect_equal(details$n_rule_errors, 0L)
  }

  msgs <- messages(dta, as_tibble = FALSE)
  expect_true(is.data.frame(msgs))
  expect_equal(nrow(msgs), 0)
  expect_true(all(c("id", "dataset", "target", "source", "message") %in% names(msgs)))

  info_all_dta <- inspect(dta, as_tibble = FALSE)
  expect_true(is.data.frame(info_all_dta))
  expect_equal(nrow(info_all_dta), 0)

  info_all_ds <- inspect(ds, as_tibble = FALSE)
  expect_true(is.data.frame(info_all_ds))
  expect_equal(nrow(info_all_ds), 0)
})

test_that("clinical multi-file mixed error fixtures support reporting and inspect end-to-end", {
  dta <- load_clinical_multi_dta(c("clinical_data_error_all.csv", "clinical_data2_error_schema.csv"))

  res <- results(dta)
  expect_true(is.data.frame(res))
  expect_equal(sort(res$target), c("clinical_data2_error_schema", "clinical_data_error_all"))
  expect_true(all(res$status == "failed"))

  row_all <- res[res$target == "clinical_data_error_all", , drop = FALSE]
  row_schema2 <- res[res$target == "clinical_data2_error_schema", , drop = FALSE]
  expect_gt(row_all$n_schema_errors[[1]], 0)
  expect_gt(row_all$n_rule_errors[[1]], 0)
  expect_gt(row_schema2$n_schema_errors[[1]], 0)
  expect_equal(row_schema2$n_rule_errors[[1]], 0)

  ds <- dta[["clinical_data"]]
  status <- validation_status(ds)
  expect_equal(nrow(status), 2)
  expect_true(all(status$status == "validated"))
  expect_true(all(status$ok %in% FALSE))

  details_all <- validation_errors(ds, table = "clinical_data_error_all", source = "memory")
  expect_false(details_all$schema_valid)
  expect_false(details_all$rules_valid)
  expect_gt(details_all$n_schema_errors, 0)
  expect_gt(details_all$n_rule_errors, 0)

  details_schema2 <- validation_errors(ds, table = "clinical_data2_error_schema", source = "memory")
  expect_false(details_schema2$schema_valid)
  expect_true(details_schema2$rules_valid)
  expect_gt(details_schema2$n_schema_errors, 0)
  expect_equal(details_schema2$n_rule_errors, 0)

  msgs <- messages(dta, as_tibble = FALSE)
  expect_true(is.data.frame(msgs))
  expect_gt(nrow(msgs), 0)
  expect_equal(msgs$id, seq_len(nrow(msgs)))
  expect_equal(sort(unique(msgs$target)), c("clinical_data2_error_schema", "clinical_data_error_all"))
  expect_equal(sort(unique(msgs$source)), c("rule", "schema"))

  schema_msgs_target2 <- msgs[msgs$target == "clinical_data2_error_schema", , drop = FALSE]
  expect_gt(nrow(schema_msgs_target2), 0)
  expect_true(all(schema_msgs_target2$source == "schema"))

  schema_id_target2 <- schema_msgs_target2$id[[1]]
  info_schema <- inspect(dta, id = schema_id_target2, as_tibble = FALSE)
  expect_true(is.data.frame(info_schema))
  expect_gt(nrow(info_schema), 0)
  expect_true(all(info_schema$id == schema_id_target2))
  expect_true(all(info_schema$target == "clinical_data2_error_schema"))
  expect_true(all(info_schema$type == "schema"))
  expect_true(any(grepl("^schema_", names(info_schema))))

  id_pair <- unique(c(msgs$id[[1]], schema_id_target2))
  info_pair <- inspect(dta, id = id_pair, as_tibble = FALSE)
  expect_true(is.data.frame(info_pair))
  expect_equal(sort(unique(info_pair$id)), sort(id_pair))

  info_all <- inspect(dta, as_tibble = FALSE)
  expect_true(is.data.frame(info_all))
  expect_equal(sort(unique(info_all$id)), msgs$id)

  info_ds_all <- inspect(ds, as_tibble = FALSE)
  expect_true(is.data.frame(info_ds_all))
  expect_equal(sort(unique(info_ds_all$id)), msgs$id)

  info_tbl <- inspect(dta, as_tibble = TRUE)
  if (requireNamespace("tibble", quietly = TRUE)) {
    expect_true(inherits(info_tbl, "tbl_df"))
  } else {
    expect_true(is.data.frame(info_tbl))
  }
})
