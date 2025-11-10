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

  # Error on missing container
  expect_error(datasets(dta, "missing"), "not found")
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
