test_that("DTA object is constructed correctly from example", {
  dta <- create_example_DTA()

  # Check class
  expect_equal(class(dta), c("DTAtools::DTA", "S7_object"))

  # Check metadata
  expect_equal(class(dta@metadata), c("DTAtools::DTAMetaData", "S7_object"))
  expect_equal(class(metadata(dta)), c("DTAtools::DTAMetaData", "S7_object"))
  expect_equal(dta@metadata, metadata(dta))
  expect_equal(dta@metadata@title, "Example DTA")

  # Check container retrieval
  expect_type(dta@datasets, "list")
  expect_type(datasets(dta), "list")
  expect_equal(dta@datasets, datasets(dta))
  expect_length(dta@datasets, 2)
  expect_named(dta@datasets, c("demographics", "vitals"))

  # Retrieve by name
  expect_equal(datasets(dta, "demographics"), dta@datasets[["demographics"]])
  expect_s3_class(datasets(dta, "demographics"), "DTAtools::DTADataSet")

  # Retrieve by index
  expect_equal(datasets(dta, 2), dta@datasets[["vitals"]])

  # Error on missing container
  expect_error(datasets(dta, "missing"), "not found")

  # Index bounds are checked
  expect_error(datasets(dta, 0), "out of bounds")
  expect_error(datasets(dta, 99), "out of bounds")
})

test_that("datasets() does not enforce a single 'name' argument (documented gap)", {
  # The guard in method(datasets, DTA) reads
  #   !is.null(name) && !is.character(name) && !is.numeric(name) && length(name) != 1
  # so the length check is ANDed with the two type checks. Any character or any
  # numeric input short-circuits the condition to FALSE, which means the
  # "must be a single ..." abort is unreachable for exactly the inputs it was
  # written to catch. The `&&` should be `||` around the length test.
  #
  # Consequences pinned below; both are gaps, not intended behaviour:
  dta <- create_example_DTA()

  # (1) A length-2 character vector passes the guard, passes the setdiff()
  #     membership check (both names exist), and then dies inside `[[` with
  #     R's own subscript error instead of the intended cli_abort. The message
  #     is localised, so pin the condition class rather than the text.
  expect_error(
    datasets(dta, c("demographics", "vitals")),
    class = "subscriptOutOfBoundsError"
  )
  expect_error(
    datasets(dta, c(1, 2)),
    class = "subscriptOutOfBoundsError"
  )

  # (2) A non-integer numeric index is silently truncated by `[[` rather than
  #     rejected, so a caller asking for dataset 1.9 quietly receives dataset 1.
  expect_identical(datasets(dta, 1.9)@name, "demographics")

  # Deferred: asserting a "single value" error for all four calls above
  # requires fixing the guard in R/DTA-class.R.
})


test_that("DTA() names datasets from the DTADataSet it is given", {
  ds <- DTADataSetTabular(
    name = "demographics",
    specs = create_example_DTAColumnSpecCollection(1),
    tables = list(t1 = data.frame(STUDYID = "1234", VISIT = "V03"))
  )

  # A bare DTADataSet is wrapped in a list and named from its @name slot.
  dta_bare <- DTA(datasets = ds, metadata = create_example_DTAMetaData())
  expect_named(dta_bare@datasets, "demographics")
  expect_length(dta_bare@datasets, 1)
  expect_equal(dta_bare@datasets[["demographics"]], ds)

  # An unnamed list takes the same path via vapply() over @name.
  dta_list <- DTA(datasets = list(ds), metadata = create_example_DTAMetaData())
  expect_named(dta_list@datasets, "demographics")

  # Explicit names are kept as given, even when they differ from @name.
  dta_named <- DTA(datasets = list(other = ds), metadata = create_example_DTAMetaData())
  expect_named(dta_named@datasets, "other")
})

test_that("DTA() builds metadata from ... when metadata is not supplied", {
  dta <- DTA(datasets = list(), title = "Constructed From Dots", version = "1.0")

  expect_s3_class(dta@metadata, "DTAtools::DTAMetaData")
  expect_equal(dta@metadata@title, "Constructed From Dots")
  expect_equal(dta@metadata@version, "1.0")
  expect_length(dta@datasets, 0)

  # Deferred (implementation gap): DTA(metadata = DTAMetaData(title = "t")) with
  # no `datasets` argument fails S7 property validation because @datasets is
  # declared class_list but defaults to NULL -- the constructor never coerces
  # the default to list(). Asserting that this builds an empty DTA needs a fix
  # in R/DTA-class.R, so it is not asserted here.
})

test_that("DTA object is constructed correctly from reading YAML DTA", {
  path <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  dta <- read_dta_from_yaml(path)

  # Check class
  expect_equal(class(dta), c("DTAtools::DTA", "S7_object"))

  # Check metadata
  expect_equal(class(dta@metadata), c("DTAtools::DTAMetaData", "S7_object"))
  expect_equal(dta@metadata@title, "Clinical Data Specification")

  # Check container retrieval
  expect_type(dta@datasets, "list")
  expect_length(dta@datasets, 1)

  expect_s3_class(datasets(dta, 1), "DTAtools::DTADataSetTabular")
  expect_true(is.list(datasets(dta)))

  expect_named(dta@datasets, c("clinical_data"))
})

test_that("[[ extracts a single dataset by index or name", {
  dta <- create_example_DTA()

  # By numeric index
  result <- dta[[1]]
  expect_s3_class(result, "DTAtools::DTADataSet")
  expect_equal(result, dta@datasets[[1]])

  result2 <- dta[[2]]
  expect_s3_class(result2, "DTAtools::DTADataSet")
  expect_equal(result2, dta@datasets[[2]])

  # By character name
  result_name <- dta[["demographics"]]
  expect_s3_class(result_name, "DTAtools::DTADataSet")
  expect_equal(result_name, dta@datasets[["demographics"]])

  # Error on length > 1
  expect_error(dta[[c(1, 2)]], "single value")

  # Error on missing name
  expect_error(dta[["nonexistent"]], "not found")

  # Error on out-of-bounds index
  expect_error(dta[[99]], "out of bounds")
})

test_that("[ extracts multiple datasets as a named list", {
  dta <- create_example_DTA()

  # By numeric vector
  result <- dta[c(1, 2)]
  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("demographics", "vitals"))

  # Single index still returns a list
  result_single <- dta[1]
  expect_type(result_single, "list")
  expect_length(result_single, 1)

  # By character vector
  result_names <- dta[c("demographics", "vitals")]
  expect_type(result_names, "list")
  expect_length(result_names, 2)
  expect_named(result_names, c("demographics", "vitals"))

  # Error on missing name
  expect_error(dta[c("demographics", "missing")], "not found")

  # Error on out-of-bounds index
  expect_error(dta[c(1, 99)], "out of bounds")
})


test_that("Load data from file via file handler from DTA object", {
  # read in a dta from yaml file
  path <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  dta <- read_dta_from_yaml(path)

  # Check class
  expect_equal(class(dta), c("DTAtools::DTA", "S7_object"))

  dta1 <- load_file(dta, "clinical_data", file = system.file("extdata", "clinical_data.csv", package = "DTAtools"))

  dta2 <- load_file(dta, 1, file = system.file("extdata", "clinical_data.csv", package = "DTAtools"))

  expect_type(dta1[["clinical_data"]]@tables, "list")
  expect_equal(1, length(dta1[["clinical_data"]]@tables))
  expect_equal(500, nrow(dta1[["clinical_data"]]@tables[[1]]))
  expect_equal(14, ncol(dta1[["clinical_data"]]@tables[[1]]))
  expect_s3_class(dta1[["clinical_data"]]@tables[[1]], "Table")
  expect_s3_class(dta1[["clinical_data"]]@tables[[1]], "ArrowTabular")

  expect_type(dta2[["clinical_data"]]@tables, "list")
  expect_equal(1, length(dta2[["clinical_data"]]@tables))
  expect_equal(500, nrow(dta2[["clinical_data"]]@tables[[1]]))
  expect_equal(14, ncol(dta2[["clinical_data"]]@tables[[1]]))
  expect_s3_class(dta2[["clinical_data"]]@tables[[1]], "Table")
  expect_s3_class(dta2[["clinical_data"]]@tables[[1]], "ArrowTabular")
})

test_that("check() method validates all datasets in DTA", {
  dta <- read_dta_from_yaml(
    system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  )
  dta <- load_file(
    dta, 1,
    file = system.file("extdata", "clinical_data.csv", package = "DTAtools")
  )

  # Check all datasets
  dta <- check(dta, persist = FALSE, quiet = TRUE)
  result <- results(dta)

  # Check summary return value is a data.frame via results()
  expect_true(is.data.frame(result))
  expect_true(all(c(
    "dataset",
    "target",
    "status",
    "validated_at",
    "run_id",
    "validation_run",
    "n_columnspec_errors",
    "n_rule_errors",
    "n_targets",
    "n_validated",
    "n_valid",
    "n_invalid",
    "n_skipped",
    "n_not_validated"
  ) %in% names(result)))

  # Check that clinical_data was validated
  expect_equal(nrow(result), 1)
  expect_equal(result$dataset, "clinical_data")
  expect_equal(result$target, "clinical_data")
  expect_true(result$status %in% c("validated", "failed"))
  expect_equal(result$n_targets, 1)
  expect_equal(result$n_validated, 1)
})

test_that("check() method validates specific dataset by name", {
  dta <- read_dta_from_yaml(
    system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  )
  dta <- load_file(
    dta, 1,
    file = system.file("extdata", "clinical_data.csv", package = "DTAtools")
  )

  # Check specific dataset
  dta <- check(dta, datasets = "clinical_data", persist = FALSE, quiet = TRUE)
  result <- results(dta, datasets = "clinical_data")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
  expect_equal(result$dataset, "clinical_data")
  expect_equal(result$target, "clinical_data")
})

test_that("check() method validates by dataset index", {
  dta <- read_dta_from_yaml(
    system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  )
  dta <- load_file(
    dta, 1,
    file = system.file("extdata", "clinical_data.csv", package = "DTAtools")
  )

  # Check by index
  dta <- check(dta, datasets = 1, persist = FALSE, quiet = TRUE)
  result <- results(dta, datasets = 1)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
  expect_equal(result$dataset, "clinical_data")
  expect_equal(result$target, "clinical_data")
})

test_that("results() returns not_validated state before checks", {
  dta <- create_example_DTA()

  result <- results(dta)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true(all(result$n_not_validated >= 1))
})

test_that("messages() returns human-readable messages for a checked DTA", {
  dta <- read_dta_from_yaml(
    system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  )
  dta <- load_file(
    dta, 1,
    file = system.file("extdata", "clinical_data.csv", package = "DTAtools")
  )

  dta <- check(dta, persist = FALSE, quiet = TRUE)
  msgs <- messages(dta, as_tibble = FALSE)

  expect_true(is.data.frame(msgs))
  expect_named(
    msgs,
    c("id", "dataset", "target", "severity", "source", "rule_id", "row", "column", "keyword", "message")
  )
})

helper_dta_with_metadata <- function(metadata) {
  dta <- read_dta_from_yaml(
    system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  )
  dta <- load_file(
    dta, 1,
    file = system.file("extdata", "clinical_data.csv", package = "DTAtools")
  )
  dta@metadata <- metadata
  dta
}

test_that("check() reports a clean metadata axis for valid metadata", {
  dta <- helper_dta_with_metadata(
    DTAMetaData(
      title = "Clean Metadata",
      version = "1.0",
      date = "2026-01-15",
      transmission = list(date_last_transfer = "after approval")
    )
  )

  out <- capture.output(dta <- check(dta, persist = FALSE), type = "message")

  metadata_summary <- attr(dta, "last_metadata_summary")
  expect_true(is.data.frame(metadata_summary))
  expect_identical(metadata_summary$scope, "metadata")
  expect_identical(metadata_summary$status, "validated")
  expect_true(metadata_summary$ok)
  expect_true(metadata_summary$import_valid)
  expect_identical(metadata_summary$n_import_errors, 0L)

  expect_true(attr(dta, "last_validation_ok"))
  expect_true(any(grepl("PASSED", out, fixed = TRUE)))
  expect_false(any(grepl("Metadata", out, fixed = TRUE)))

  # The per-dataset summary keeps its own columns; metadata is not a row in it.
  summary_df <- attr(dta, "last_validation_summary")
  expect_identical(summary_df$dataset, "clinical_data")
  expect_false("metadata" %in% summary_df$dataset)
})

test_that("check() FAILS when metadata carries an import error", {
  # Before the metadata axis existed, check(DTA) iterated x@datasets only, so a
  # DTA whose transfer date had silently lost its qualification still printed
  # "Validation PASSED: All datasets are valid".
  dta <- helper_dta_with_metadata(
    DTAMetaData(
      title = "Qualified Metadata",
      version = "1.0",
      transmission = list(date_last_transfer = "2026-12-31 at the earliest")
    )
  )

  out <- capture.output(dta <- check(dta, persist = FALSE), type = "message")

  # Every dataset is still valid: the failure comes from metadata alone.
  summary_df <- attr(dta, "last_validation_summary")
  expect_equal(sum(summary_df$n_invalid), 0)
  expect_equal(sum(summary_df$n_import_errors), 0)

  metadata_summary <- attr(dta, "last_metadata_summary")
  expect_identical(metadata_summary$status, "failed")
  expect_false(metadata_summary$ok)
  expect_false(metadata_summary$import_valid)
  expect_identical(metadata_summary$n_import_errors, 1L)
  expect_identical(metadata_summary$fields, "transmission$date_last_transfer")

  expect_false(attr(dta, "last_validation_ok"))

  # The banner must not claim a pass, and must name the offending value.
  expect_false(any(grepl("PASSED", out, fixed = TRUE)))
  expect_true(any(grepl("FAILED", out, fixed = TRUE)))
  expect_true(any(grepl("2026-12-31 at the earliest", out, fixed = TRUE)))

  # The same failure is queryable rather than only printed.
  msgs <- messages(metadata(dta), as_tibble = FALSE)
  expect_equal(nrow(msgs), 1)
  expect_identical(msgs$source, "import")
  expect_identical(msgs$target, "metadata")
})

test_that("check() metadata axis is silent under quiet = TRUE but still fails", {
  dta <- helper_dta_with_metadata(
    DTAMetaData(
      title = "Qualified Metadata",
      version = "1.0",
      date = "2026-07-24 provisional"
    )
  )

  out <- capture.output(dta <- check(dta, persist = FALSE, quiet = TRUE), type = "message")

  expect_length(out, 0)
  expect_false(attr(dta, "last_validation_ok"))
  expect_identical(attr(dta, "last_metadata_summary")$fields, "date")
})

test_that("check() aborts on empty DTA", {
  dta <- DTA(datasets = list(), metadata = DTAMetaData(title = "Empty DTA"))
  expect_error(check(dta, quiet = TRUE), "no datasets")
})

test_that("check() aborts on invalid dataset index", {
  dta <- create_example_DTA()
  expect_error(check(dta, datasets = 99, quiet = TRUE), "out of bounds")
})

test_that("check() aborts on missing dataset name", {
  dta <- create_example_DTA()
  expect_error(check(dta, datasets = "nonexistent", quiet = TRUE), "not found")
})

test_that("read_dta_from_yaml aborts for non-existent yaml file", {
  expect_error(
    read_dta_from_yaml(file.path(tempdir(), "does-not-exist-dta.yaml")),
    "does not exist"
  )
})

test_that("load_file() aborts for missing dataset name", {
  dta <- read_dta_from_yaml(
    system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  )

  expect_error(
    load_file(
      dta,
      "missing_dataset",
      file = system.file("extdata", "clinical_data.csv", package = "DTAtools")
    ),
    "not found"
  )
})

test_that("messages(dta) folds in metadata import errors", {
  md <- DTAMetaData(
    title = "Qualified",
    version = "1.0",
    transmission = list(date_last_transfer = "2026-12-31 at the earliest")
  )
  dta <- DTA(datasets = list(create_example_DTADataSetTabular(2)), metadata = md)
  dta <- check(dta, persist = FALSE, quiet = TRUE)

  msgs <- messages(dta, as_tibble = FALSE)

  # The metadata frame must carry the same 10 columns in the same order as the
  # per-table frames; two populated frames of differing width would make the
  # rbind error rather than forgive.
  expect_identical(names(msgs), names(dta_empty_messages()))
  expect_identical(msgs$id, seq_len(nrow(msgs)))

  meta_rows <- msgs[msgs$target %in% "metadata", ]
  expect_equal(nrow(meta_rows), 1)
  expect_equal(meta_rows$source, "import")
  expect_match(meta_rows$message, "at the earliest", fixed = TRUE)
  expect_match(meta_rows$message, "2026-12-31", fixed = TRUE)
})

test_that("messages(dta) is unchanged when metadata has no import errors", {
  dta <- check(create_example_DTA(), persist = FALSE, quiet = TRUE)

  msgs <- messages(dta, as_tibble = FALSE)

  expect_false(any(msgs$target %in% "metadata"))
  expect_false(any(msgs$source == "import"))
  expect_identical(msgs$id, seq_len(nrow(msgs)))
})

# ---------------------------------------------------------------------------
# dta_from_list() with no datasets -- section P12 of the code review fixes
# ---------------------------------------------------------------------------
# dta_dataset_from_list() tells "one dataset" from "several" apart by
# inspecting `x[[1]]$name`, and `x[[1]]` on a zero-length list used to abort
# with a raw base "subscript out of bounds" instead of the intended "no
# datasets" outcome. This is exactly the document produced by removing a
# DTA's last dataset and serialising it back to YAML.

test_that("dta_from_list() with no 'datasets' key builds a DTA with zero datasets", {
  dta <- dta_from_list(list(metadata = list(title = "Round trip", version = "1.0")))

  expect_s3_class(dta, "DTAtools::DTA")
  expect_length(datasets(dta), 0)
})

test_that("dta_from_list() with datasets = NULL builds a DTA with zero datasets", {
  dta <- dta_from_list(list(
    metadata = list(title = "Round trip", version = "1.0"),
    datasets = NULL
  ))

  expect_s3_class(dta, "DTAtools::DTA")
  expect_length(datasets(dta), 0)
})

test_that("dta_from_list() with datasets = list() builds a DTA with zero datasets", {
  dta <- dta_from_list(list(
    metadata = list(title = "Round trip", version = "1.0"),
    datasets = list()
  ))

  expect_s3_class(dta, "DTAtools::DTA")
  expect_length(datasets(dta), 0)
})

test_that("check() reports INCOMPLETE rather than certifying unchecked targets", {
  # An ok = NA target (here: a table checked against zero column specs) used
  # to vanish from the rollup entirely -- every sum ran na.rm = TRUE -- so an
  # all-unspecified DTA printed "Validation PASSED: All datasets are valid"
  # with last_validation_ok = TRUE: a certificate covering zero checks.
  ds <- DTADataSetTabular(
    name = "d",
    specs = specs_from_list(NULL),
    files = list(DTAFileCSV(filename = "clinical_data.csv"))
  )
  ds <- load_file(
    ds,
    file = system.file("extdata", "clinical_data.csv", package = "DTAtools"),
    handler_index = 1
  )
  dta <- DTA(datasets = list(ds), metadata = create_example_DTAMetaData())

  msgs <- capture_messages(dta <- check(dta, persist = FALSE))
  expect_true(any(grepl("Validation INCOMPLETE", msgs)))
  expect_false(any(grepl("Validation PASSED", msgs)))

  summary_df <- attr(dta, "last_validation_summary")
  expect_true("n_unchecked" %in% names(summary_df))
  expect_equal(summary_df$n_unchecked, 1)
  expect_false(attr(dta, "last_validation_ok"))
})


# ---------------------------------------------------------------------------
# A dataset with nothing bound: reported, never certified
# ---------------------------------------------------------------------------

# Console output of one expression, whitespace-normalised: cli wraps to the
# terminal width, so raw output can break in the middle of a matched phrase.
dta_console <- function(expr) {
  gsub("[[:space:]]+", " ", paste(testthat::capture_messages(expr), collapse = " "))
}

test_that("a dataset with no tables makes the DTA INCOMPLETE, never PASSED", {
  # Checking a DTA whose data has not been delivered used to abort outright
  # ("No tables found in dataset."), taking every other dataset's verdict with
  # it. Reporting it is only half the fix: a dataset with zero targets also has
  # zero UNCHECKED targets, so without counting the dataset itself the run
  # would print "Validation PASSED: All datasets are valid" over a check that
  # read no file at all.
  dta <- DTA(
    datasets = list(empty = create_example_DTADataSetTabular(1)),
    metadata = create_example_DTAMetaData()
  )

  out <- dta_console(dta <- check(dta, persist = FALSE))

  expect_match(out, "Validation INCOMPLETE", fixed = TRUE)
  expect_match(out, "no tables loaded", fixed = TRUE)
  expect_false(grepl("Validation PASSED", out, fixed = TRUE))
  expect_false(attr(dta, "last_validation_ok"))

  summary_df <- attr(dta, "last_validation_summary")
  expect_equal(summary_df$n_targets, 0)
  expect_equal(summary_df$n_undelivered, 1L)

  # The read-only reports answer instead of aborting, too.
  expect_equal(nrow(results(dta)), 0)
  expect_equal(nrow(messages(dta, as_tibble = FALSE)), 0)
})

test_that("an undelivered dataset does not hide a delivered one's result", {
  delivered <- DTADataSetTabular(
    name = "delivered",
    specs = DTAColumnSpecCollection(columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
    )),
    tables = list(tab = data.frame(ID = "A001", stringsAsFactors = FALSE))
  )
  dta <- DTA(
    datasets = list(delivered = delivered, empty = create_example_DTADataSetTabular(1)),
    metadata = create_example_DTAMetaData()
  )

  out <- dta_console(dta <- check(dta, persist = FALSE))

  expect_match(out, "Validation INCOMPLETE", fixed = TRUE)
  expect_false(attr(dta, "last_validation_ok"))

  res <- results(dta)
  expect_equal(nrow(res), 1)
  expect_equal(res$dataset, "delivered")
  expect_equal(res$status, "validated")
})


# ---------------------------------------------------------------------------
# check(DTA) forwards the scan controls
# ---------------------------------------------------------------------------

test_that("check(DTA) forwards on_missing_column to its datasets", {
  # `on_missing_column`, `fail_fast` and `use_threads` existed only on the
  # dataset method, so the only way to reach them was to take a dataset out of
  # its DTA and check it alone -- and passing one here died on R's own
  # "unused argument" before any dataset was touched.
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("ID", "A001"), path)

  ds <- DTADataSetTabular(
    name = "m",
    specs = DTAColumnSpecCollection(columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
      ABSENT = DTAColumnSpec(id = "ABSENT", type = "SAS Char", length = 4, nullable = FALSE)
    )),
    files = list(DTAFileCSV(filename = basename(path)))
  )
  ds <- load_file(ds, file = path, handler_index = 1)
  dta <- DTA(datasets = list(m = ds), metadata = create_example_DTAMetaData())

  table_name <- tools::file_path_sans_ext(basename(path))

  stopped <- check(dta, on_missing_column = "stop", persist = FALSE, quiet = TRUE)
  details <- validation_errors(datasets(stopped, "m"), table = table_name)
  # The result says, in the object itself, that no row was read -- so nothing
  # can mistake it for a verdict on the data.
  expect_true(isTRUE(attr(details, "structural_only")))

  scanned <- check(dta, persist = FALSE, quiet = TRUE)
  scanned_details <- validation_errors(datasets(scanned, "m"), table = table_name)
  expect_false(isTRUE(attr(scanned_details, "structural_only")))

  # Both agree that the declared column is absent.
  expect_false(validation_status(datasets(stopped, "m"))$ok)
  expect_false(validation_status(datasets(scanned, "m"))$ok)
})

test_that("check(DTA) accepts fail_fast and use_threads for every dataset type", {
  path <- withr::local_tempfile(fileext = ".txt")
  writeLines("content", path)

  dta <- DTA(
    datasets = list(
      tab = create_example_DTADataSetTabular(2),
      files = DTADataSetFile(name = "files", paths = path)
    ),
    metadata = create_example_DTAMetaData()
  )

  # A file dataset reads no rows, so it accepts and ignores all three; the call
  # must not have to branch on the dataset's class.
  dta <- check(
    dta,
    fail_fast = TRUE, use_threads = FALSE, on_missing_column = "scan",
    persist = FALSE, quiet = TRUE
  )

  expect_true(validation_status(datasets(dta, "files"))$ok)
  expect_equal(nrow(validation_status(datasets(dta, "tab"))), 1)
})

test_that("check(DTA) rejects an unknown on_missing_column before checking anything", {
  dta <- create_example_DTA()

  # match.arg()'s message comes from base R and is rendered in the system
  # language, so the condition class is what can be asserted about it.
  expect_error(
    check(dta, on_missing_column = "maybe", quiet = TRUE),
    class = "simpleError"
  )

  # And it is rejected before the first dataset is touched: a run that had
  # started would have printed its "Validating N Datasets" line by now.
  expect_silent(
    try(check(dta, on_missing_column = "maybe", quiet = FALSE), silent = TRUE)
  )
})


# ---------------------------------------------------------------------------
# A non-dataset entry is an error, quiet or not
# ---------------------------------------------------------------------------

test_that("check() aborts on a non-DTADataSet entry regardless of quiet", {
  # `quiet` governs how much is PRINTED, never what is checked. Skipping the
  # entry under quiet = TRUE left it out of the rollup entirely, so the run
  # reported last_validation_ok = TRUE: a pass over an object nothing had
  # looked at.
  dta <- DTA(datasets = list(bogus = "x"), metadata = create_example_DTAMetaData())

  expect_error(check(dta, quiet = TRUE, persist = FALSE), "is not a DTADataSet")
  expect_error(check(dta, quiet = FALSE, persist = FALSE), "is not a DTADataSet")
})


# ---------------------------------------------------------------------------
# A dataset name containing braces is data, not cli syntax
# ---------------------------------------------------------------------------

test_that("print(DTA) survives braces in a dataset name", {
  # print(DTA) used to paste every dataset name into `{.field ...}` markup
  # with paste0() and hand the assembled string to cli_alert_info(), so a
  # dataset called `a{b}` took it down with "Could not evaluate cli `{}`
  # expression" -- the same defect already fixed for print(DTADataSetTabular).
  ds <- DTADataSet(
    name = "a{b}",
    type = "file",
    files = list(create_example_DTAFileCSV())
  )
  dta <- DTA(datasets = ds, metadata = create_example_DTAMetaData())

  out <- dta_console(expect_invisible(print(dta)))

  expect_match(out, "Datasets (1): a{b}", fixed = TRUE)
})
