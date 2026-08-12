test_that("Validation of single tabular dataset", {
  # read in a dta from yaml file
  path <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  # Guaranteed package assets — a missing fixture is a failure, not a skip.
  expect_true(nzchar(path), info = "clinical_dta.yaml missing from extdata")

  dta <- read_dta_from_yaml(path)

  # Check class
  expect_equal(class(dta), c("DTAtools::DTA", "S7_object"))

  data_path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
  expect_true(nzchar(data_path), info = "clinical_data.csv missing from extdata")

  dta <- load_file(dta, "clinical_data", file = data_path)

  # persist = FALSE: the default wrote .rds artifacts into tempdir() for the
  # rest of the session, undeclared state this test never examined.
  dta <- check(dta, persist = FALSE, quiet = TRUE)
  expect_true(inherits(dta, "DTAtools::DTA"))
  expect_false(is.null(dta[[1]]@validation_index))
  expect_gt(length(dta[[1]]@validation_index), 0)
  expect_false(is.null(dta[[1]]@validation_store))
  expect_gt(length(dta[[1]]@validation_store), 0)

  res <- results(dta)
  expect_equal(nrow(res), 1)
  expect_equal(res$target, "clinical_data")
  expect_equal(res$status, "validated")

  rule_errors <- dta[[1]]@validation_store[[1]]$rule_errors
  expect_equal(length(rule_errors), res$n_rule_errors)
})

test_that("Examples can be built with current constructors", {
  build_dataset_from_example <- function(data_filename, data_sep, params_filename) {
    # Guaranteed package assets — a missing fixture is a failure, not a skip.
    expect_true(file.exists(data_filename), info = data_filename)
    expect_true(file.exists(params_filename), info = params_filename)

    table <- read.table(data_filename, sep = data_sep, header = TRUE)
    yaml_obj <- yaml::read_yaml(params_filename)
    if (!is.null(yaml_obj$columns)) {
      specs <- import_specs_from_yaml(params_filename)
    } else {
      dta_from_yaml <- read_dta_from_yaml(params_filename)
      specs <- dta_from_yaml[[1]]@specs
    }

    ds <- DTADataSetTabular(
      name = "example",
      specs = specs,
      tables = list(DTA = table)
    )

    expect_true(inherits(ds, "DTAtools::DTADataSetTabular"))

    # expect_no_error() alone would pass for a validate_table() that reported
    # success unconditionally. Inspect the result.
    res <- validate_table_detailed(
      specs = specs,
      table = table,
      verbose = FALSE
    )
    expect_true(res$ok)
    expect_equal(res$n_schema_errors, 0)
    expect_equal(res$n_rule_errors, 0)
  }

  build_dataset_from_example(
    system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools"),
    "\t",
    system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
  )
  build_dataset_from_example(
    system.file("extdata", "clinical_data.csv", package = "DTAtools"),
    ",",
    system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  )
})
