test_that("DTADataSetFile validates readable, non-empty files", {
  path <- tempfile(fileext = ".txt")
  writeLines("hello world", path)

  ds <- DTADataSetFile(name = "notes", paths = path)
  ds <- check(ds, quiet = TRUE)

  status <- validation_status(ds)
  expect_equal(status$table, basename(path))
  expect_equal(status$status, "validated")
  expect_true(status$ok)

  res <- results(ds)
  expect_equal(nrow(res), 1)
  expect_equal(res$target, basename(path))
  expect_equal(res$status, "validated")

  msgs <- messages(ds, as_tibble = FALSE)
  expect_equal(nrow(msgs), 0)
})

test_that("DTADataSetFile reports missing or unreadable files", {
  path <- tempfile(fileext = ".txt")
  if (file.exists(path)) {
    unlink(path)
  }

  ds <- DTADataSetFile(name = "missing_file", paths = path)
  ds <- check(ds, quiet = TRUE)

  status <- validation_status(ds)
  expect_equal(status$status, "validated")
  expect_false(status$ok)

  res <- results(ds)
  expect_equal(res$n_rule_errors, 1)
  expect_equal(res$status, "failed")

  msgs <- messages(ds, as_tibble = FALSE)
  expect_equal(nrow(msgs), 1)
  expect_equal(msgs$source, "rule")
  expect_true(grepl("not found|readable|empty", msgs$message, ignore.case = TRUE))
})

test_that("DTA results and messages combine tabular and file datasets", {
  path <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  dta <- read_dta_from_yaml(path)
  dta <- load_file(
    dta,
    1,
    file = system.file("extdata", "clinical_data.csv", package = "DTAtools")
  )

  missing_path <- tempfile(fileext = ".txt")
  if (file.exists(missing_path)) {
    unlink(missing_path)
  }

  file_ds <- DTADataSetFile(name = "attachment", paths = missing_path)
  dta@datasets[["attachment"]] <- file_ds

  dta <- check(dta, persist = FALSE, quiet = TRUE)

  res <- results(dta)
  expect_true(all(c("clinical_data", "attachment") %in% res$dataset))
  expect_equal(nrow(res), 2)
  expect_equal(length(unique(res$validation_run)), 1)
  expect_equal(res$status[res$dataset == "clinical_data"], "validated")
  expect_equal(res$status[res$dataset == "attachment"], "failed")
  expect_false(any(is.na(res$run_id)))

  msgs <- messages(dta, as_tibble = FALSE)
  expect_true(any(msgs$dataset == "attachment"))
  expect_false(any(msgs$dataset == "clinical_data"))
})
