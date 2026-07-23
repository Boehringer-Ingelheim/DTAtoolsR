test_that("Validation of single tabular dataset", {
  # read in a dta from yaml file
  path <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  skip_if_not(nzchar(path))

  dta <- read_dta_from_yaml(path)

  # Check class
  expect_equal(class(dta), c("DTAtools::DTA", "S7_object"))

  data_path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
  skip_if_not(nzchar(data_path))

  dta <- load_file(dta, "clinical_data", file = data_path)

  expect_no_error(check(dta))
  expect_no_error(check(dta[[1]]))
})

test_that("Examples can be built with current constructors", {
  build_dataset_from_example <- function(data_filename, data_sep, params_filename) {
    skip_if_not(file.exists(data_filename))
    skip_if_not(file.exists(params_filename))

    table <- read.table(data_filename, sep = data_sep, header = TRUE)
    specs <- import_specs_from_yaml(params_filename)

    ds <- DTADataSetTabular(
      name = "example",
      specs = specs,
      tables = list(DTA = table)
    )

    expect_true(inherits(ds, "DTAtools::DTADataSetTabular"))
    expect_no_error(validate_table(specs = specs, table = table, verbose = FALSE))
  }

  build_dataset_from_example(
    system.file("extdata", "gf_data_small.tsv", package = "DTAtools"),
    "\t",
    system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
  )
  build_dataset_from_example(
    system.file("extdata", "clinical_data.csv", package = "DTAtools"),
    ",",
    system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  )
})
