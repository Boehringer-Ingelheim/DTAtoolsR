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
    "n_schema_errors",
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
