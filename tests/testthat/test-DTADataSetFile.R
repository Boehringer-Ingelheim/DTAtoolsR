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
  # The old alternation "not found|readable|empty" could not tell the three
  # distinct failure reasons apart. This scenario is specifically a missing
  # file, so pin that reason.
  expect_match(msgs$message, "not found")
})

test_that("DTADataSetFile flags an existing but empty file", {
  path <- tempfile(fileext = ".txt")
  file.create(path)
  on.exit(unlink(path), add = TRUE)

  ds <- check(DTADataSetFile(name = "empty_file", paths = path), quiet = TRUE)

  status <- validation_status(ds)
  expect_false(status$ok)

  msgs <- messages(ds, as_tibble = FALSE)
  expect_equal(nrow(msgs), 1)
  expect_match(msgs$message, "empty")
})

test_that("DTADataSetFile keeps two paths that share a basename apart", {
  dir_a <- file.path(tempdir(), "dta-basename-a")
  dir_b <- file.path(tempdir(), "dta-basename-b")
  dir.create(dir_a, showWarnings = FALSE)
  dir.create(dir_b, showWarnings = FALSE)
  on.exit(unlink(c(dir_a, dir_b), recursive = TRUE), add = TRUE)

  present <- file.path(dir_a, "same.txt")
  writeLines("content", present)
  absent <- file.path(dir_b, "same.txt")

  ds <- check(
    DTADataSetFile(name = "collision", paths = c(present, absent)),
    quiet = TRUE
  )

  status <- validation_status(ds)

  expect_equal(nrow(status), 2)
  expect_setequal(status$ok, c(TRUE, FALSE))

  # Exactly one message, for the absent file, and inspect() must resolve it
  # back to the full path it came from.
  msgs <- messages(ds, as_tibble = FALSE)
  expect_equal(nrow(msgs), 1)
  expect_match(msgs$message, "not found")

  details <- inspect(ds, as_tibble = FALSE)
  expect_equal(nrow(details), 1)
  expect_equal(details$file_path, absent)
})

test_that("dta_file_target_keys disambiguates only where it has to", {
  expect_equal(
    dta_file_target_keys(c("a/one.txt", "b/two.txt")),
    c("one.txt", "two.txt")
  )
  expect_equal(
    dta_file_target_keys(c("a/same.txt", "b/same.txt")),
    c("a/same.txt", "b/same.txt")
  )
  # Identical paths cannot be told apart by path either, so fall back to a
  # suffix rather than silently dropping one.
  expect_equal(length(unique(dta_file_target_keys(c("a/x.txt", "a/x.txt")))), 2)
  expect_equal(dta_file_target_keys(character()), character())
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
