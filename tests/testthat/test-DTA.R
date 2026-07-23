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
  result <- check(dta, persist = FALSE, quiet = TRUE)

  # Check return value is a data.frame
  expect_true(is.data.frame(result))
  expect_named(result, c("dataset", "n_tables", "n_validated", "n_valid", "n_invalid", "n_skipped"))

  # Check that clinical_data was validated
  expect_equal(nrow(result), 1)
  expect_equal(result$dataset, "clinical_data")
  expect_equal(result$n_tables, 1)
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
  result <- check(dta, datasets = "clinical_data", persist = FALSE, quiet = TRUE)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
  expect_equal(result$dataset, "clinical_data")
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
  result <- check(dta, datasets = 1, persist = FALSE, quiet = TRUE)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
  expect_equal(result$dataset, "clinical_data")
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
