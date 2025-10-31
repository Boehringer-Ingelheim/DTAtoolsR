test_that("DTAFileCSV object is created from reading in csv and table is accessible", {
  path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")

  file_info <- DTAFileCSV("clinical_data.csv")

  expect_s3_class(file_info, "DTAtools::DTAFile")
  expect_s3_class(file_info, "DTAtools::DTAFileCSV")

  expect_true(matches_filename(file_info, basename(path)))

  x <- read_file(file_info, path)

  expect_true(inherits(x, "Table"))
})


test_that("DTAFileTSV object is created from reading in tsv and table is accessible", {

  path <- system.file("extdata", "gf_data_small.tsv", package = "DTAtools")

  file_info <- DTAFileTSV("gf_data_small.tsv")

  expect_s3_class(file_info, "DTAtools::DTAFile")
  expect_s3_class(file_info, "DTAtools::DTAFileTSV")

  expect_true(matches_filename(file_info, basename(path)))

  x <- read_file(file_info, path)

  expect_true(inherits(x, "Table"))
})


test_that("Testing pattern with DTAFileTSV", {

  path <- system.file("extdata", "gf_data_small.tsv", package = "DTAtools")

  file_info <- DTAFileTSV("data_gf_.*\\.tsv", pattern = TRUE, number_of_files = 1)

  expect_s3_class(file_info, "DTAtools::DTAFile")
  expect_s3_class(file_info, "DTAtools::DTAFileTSV")

  expect_true(matches_filename(file_info, basename(path)))

  x <- read_file(file_info, path)

  expect_true(inherits(x, "Table"))
})


test_that("Testing wrong pattern with DTAFileTSV", {
  path <- system.file("extdata", "gf_data_small.tsv", package = "DTAtools")

  file_info <- DTAFileTSV("data_gf_dfjlkadwefwfew.*\\.tsv", pattern = TRUE, number_of_files = 1)

  expect_false(matches_filename(file_info, basename(path)))
})

test_that("DTAFile Creation", {
  path <- system.file("extdata", "gf_data_small.tsv", package = "DTAtools")

  file_info <- DTAFile("gf_data_small.tsv")

  expect_s3_class(file_info, "DTAtools::DTAFile")

  expect_true(matches_filename(file_info, basename(path)))
})

