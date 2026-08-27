# The bundled example that pairs a never-parsed file dataset with a tabular one
# (inst/extdata/clinical_dta_with_file_dataset.yaml). It is the only shipped
# specification exercising DTADataSetFile / DTAFileAny, so these tests are what
# stop the worked example for that feature from silently rotting.

load_clinical_file_dataset_dta <- function() {
  spec_path <- system.file(
    "extdata", "clinical_dta_with_file_dataset.yaml",
    package = "DTAtools"
  )
  # Guaranteed package assets — a missing fixture is a failure, not a skip.
  expect_true(
    nzchar(spec_path),
    info = "clinical_dta_with_file_dataset.yaml missing from extdata"
  )

  read_dta_from_yaml(spec_path)
}

example_fixture_path <- function(filename) {
  path <- system.file("extdata", filename, package = "DTAtools")
  expect_true(nzchar(path), info = paste(filename, "missing from extdata"))
  path
}

test_that("the example declares a tabular dataset and a file dataset side by side", {
  dta <- load_clinical_file_dataset_dta()

  expect_length(dta@datasets, 2)
  expect_equal(
    unname(vapply(dta@datasets, function(ds) ds@name, character(1))),
    c("clinical_data", "raw_export")
  )

  tabular <- dta[["clinical_data"]]
  file_ds <- dta[["raw_export"]]

  expect_s3_class(tabular, "DTAtools::DTADataSetTabular")
  expect_s3_class(file_ds, "DTAtools::DTADataSetFile")
  expect_equal(file_ds@type, "file")

  # `type: any` must build the reader-less handler: a csv/tsv handler here
  # would claim the deliverable gets parsed, which a file dataset never does.
  expect_length(file_ds@files, 1)
  expect_s3_class(file_ds@files[[1]], "DTAtools::DTAFileAny")
  expect_equal(file_ds@files[[1]]@filename, "clinical_data2.csv.gz")
})

test_that("the example's file dataset reports the deliverable as missing before it arrives", {
  file_ds <- check(
    load_clinical_file_dataset_dta()[["raw_export"]],
    persist = FALSE, quiet = TRUE
  )

  res <- results(file_ds)
  expect_equal(nrow(res), 1)
  expect_equal(res$target, "clinical_data2.csv.gz")
  expect_equal(res$target_type, "file")
  expect_equal(res$status, "failed")
  expect_equal(res$n_rule_errors, 1L)
  expect_equal(res$n_columnspec_errors, 0L)

  msgs <- messages(file_ds, as_tibble = FALSE)
  expect_equal(nrow(msgs), 1)
  expect_equal(msgs$rule_id, "file_presence")
  expect_match(msgs$message, "clinical_data2\\.csv\\.gz", fixed = FALSE)
  # "not delivered", NOT "not found": a file declared in the spec that never
  # arrived is a different condition from a delivered path that is missing
  # from disk ("File '<path>' not found.", validate_file_dataset_entry()).
  # This target has no path to look at at all, and the check deliberately
  # never stats one for it.
  expect_match(msgs$message, "was not delivered")
})

test_that("both example datasets validate clean once their deliverables are bound", {
  dta <- load_clinical_file_dataset_dta()
  dta <- load_file(dta, "clinical_data", file = example_fixture_path("clinical_data.csv"))
  dta <- load_file(dta, "raw_export", file = example_fixture_path("clinical_data2.csv.gz"))
  dta <- check(dta, persist = FALSE, quiet = TRUE)

  res <- results(dta)
  expect_equal(nrow(res), 2)
  expect_equal(res$dataset, c("clinical_data", "raw_export"))
  expect_equal(res$target, c("clinical_data", "clinical_data2.csv.gz"))
  expect_equal(res$target_type, c("table", "file"))
  expect_true(all(res$status == "validated"))
  expect_true(all(res$n_columnspec_errors == 0L))
  expect_true(all(res$n_rule_errors == 0L))
  expect_true(all(res$n_import_errors == 0L))

  # Nothing left to report on either axis, for either dataset.
  msgs <- messages(dta, as_tibble = FALSE)
  expect_true(is.data.frame(msgs))
  expect_equal(nrow(msgs), 0)

  file_status <- validation_status(dta[["raw_export"]])
  expect_equal(nrow(file_status), 1)
  expect_true(isTRUE(file_status$ok[[1]]))

  details <- validation_errors(
    dta[["raw_export"]],
    table = "clinical_data2.csv.gz", source = "memory"
  )
  expect_true(isTRUE(details$ok))
  expect_true(isTRUE(details$rules_valid))
  expect_equal(details$n_rule_errors, 0L)
})

test_that("the example's file handler refuses a file delivered into the wrong slot", {
  file_ds <- load_clinical_file_dataset_dta()[["raw_export"]]

  expect_error(
    DTAtools:::load_file(
      file_ds,
      file = example_fixture_path("clinical_data.csv"),
      handler_index = 1
    ),
    regexp = "does not match the filename or pattern"
  )
})
