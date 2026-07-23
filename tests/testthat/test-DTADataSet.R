test_that("DTADataSet object is created from examples", {
  ds1 <- create_example_DTADataSetTabular(1)
  expect_s3_class(ds1, "DTAtools::DTADataSet")

  ds2 <- create_example_DTADataSetTabular(2)
  expect_s3_class(ds2, "DTAtools::DTADataSet")

  ds3 <- create_example_DTADataSetTabular(3)
  expect_s3_class(ds3, "DTAtools::DTADataSet")
})

test_that("DTADataSet object is loaded from yaml", {
  path <- system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
  ds <- read_dataset_from_yaml(path)
  expect_s3_class(ds, "DTAtools::DTADataSet")
  expect_s3_class(ds@files[[1]], "DTAtools::DTAFileTSV") 
})


test_that("DTADataSet object is created and table can be loaded", {
  path <- system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
  ds <- read_dataset_from_yaml(path)

  table_path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

  expect_equal(max_number_of_files(ds), 1)
  expect_equal(min_number_of_files(ds), 1)

  expect_s3_class(ds, "DTAtools::DTADataSet")

  tab <- read_file(ds@files[[1]], table_path)

  expect_error(read_file(ds@files[[1]], "blala.tsv"),  "does not match the filename")
  
  expect_s3_class(tab, c("R6", "Table", "ArrowTabular", "ArrowObject"))
  expect_equal(nrow(tab), 20940)
  expect_equal(ncol(tab), 33)

  expect_true(is.list(files(ds)))
  expect_s3_class(files(ds)[[1]], "DTAtools::DTAFileTSV")

  expect_s3_class(specs(ds), "DTAtools::DTAColumnSpecCollection")

  expect_true(is.list(tables(ds)))    

  expect_s3_class(colspec(ds, 1), "DTAtools::DTAColumnSpec")
  expect_s3_class(colspec(ds, "STUDYID"), "DTAtools::DTAColumnSpec")
})

