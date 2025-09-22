test_that("DTAFileInfoCSV object is created from reading in csv and table is accessible", {

  path <- system.file("extdata", "data_spec.csv", package = "DTAtools")

  file_info <- DTAFileInfoCSV("data_spec.csv")

  expect_s3_class(file_info, "DTAtools::DTAFileInfo")
  expect_s3_class(file_info, "DTAtools::DTAFileInfoCSV")

  expect_true(matches_filename(file_info, basename(path)))

  x <- read_file(file_info, path)

  expect_true(inherits(x, "Table"))
})


test_that("DTAFileInfoTSV object is created from reading in tsv and table is accessible", {

  path <- system.file("extdata", "data_gf_small.tsv", package = "DTAtools")

  file_info <- DTAFileInfoTSV("data_gf_small.tsv")

  expect_s3_class(file_info, "DTAtools::DTAFileInfo")
  expect_s3_class(file_info, "DTAtools::DTAFileInfoTSV")

  expect_true(matches_filename(file_info, basename(path)))

  x <- read_file(file_info, path)

  expect_true(inherits(x, "Table"))
})


test_that("Testing pattern with DTAFileInfoTSV", {

  path <- system.file("extdata", "data_gf_small.tsv", package = "DTAtools")

  file_info <- DTAFileInfoTSV("data_gf_.*\\.tsv", pattern = TRUE, number_of_files = 1)

  expect_s3_class(file_info, "DTAtools::DTAFileInfo")
  expect_s3_class(file_info, "DTAtools::DTAFileInfoTSV")

  expect_true(matches_filename(file_info, basename(path)))

  x <- read_file(file_info, path)

  expect_true(inherits(x, "Table"))
})


test_that("Testing wrong pattern with DTAFileInfoTSV", {
  path <- system.file("extdata", "data_gf_small.tsv", package = "DTAtools")

  file_info <- DTAFileInfoTSV("data_gf_dfjlkadwefwfew.*\\.tsv", pattern = TRUE, number_of_files = 1)

  expect_false(matches_filename(file_info, basename(path)))
})

test_that("DTAFileInfo Creation", {
  path <- system.file("extdata", "data_gf_small.tsv", package = "DTAtools")

  file_info <- DTAFileInfo("data_gf_small.tsv")

  expect_s3_class(file_info, "DTAtools::DTAFileInfo")

  expect_true(matches_filename(file_info, basename(path)))
})

