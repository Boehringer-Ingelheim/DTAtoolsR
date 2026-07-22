test_that("DTAFileCSV object is created from reading in csv and table is accessible", {
  path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")

  file_info <- DTAFileCSV("clinical_data.csv")

  expect_s3_class(file_info, "DTAtools::DTAFile")
  expect_s3_class(file_info, "DTAtools::DTAFileCSV")

  expect_true(matches_filename(file_info, basename(path)))

  x <- read_file(file_info, path)

  expect_true(inherits(x, "Table"))
  expect_s3_class(x, c("R6", "Table", "ArrowTabular", "ArrowObject"))
})


test_that("matches_filename accepts full file paths", {
  path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
  file_info <- DTAFileCSV("clinical_data.csv")

  expect_true(matches_filename(file_info, path))
})


test_that("DTAFileTSV object is created from reading in tsv and table is accessible", {

  path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

  file_info <- DTAFileTSV("gf_data_small_smirna.tsv")

  expect_s3_class(file_info, "DTAtools::DTAFile")
  expect_s3_class(file_info, "DTAtools::DTAFileTSV")

  expect_true(matches_filename(file_info, basename(path)))

  x <- read_file(file_info, path)

  expect_s3_class(x, c("R6", "Table", "ArrowTabular", "ArrowObject"))
})


test_that("Testing pattern with DTAFileTSV", {

  path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

  file_info <- DTAFileTSV("gf_data_.*\\.tsv", pattern = TRUE, number_of_files = 1)

  expect_s3_class(file_info, "DTAtools::DTAFile")
  expect_s3_class(file_info, "DTAtools::DTAFileTSV")

  expect_true(matches_filename(file_info, basename(path)))

  x <- read_file(file_info, path)

  expect_s3_class(x, c("R6", "Table", "ArrowTabular", "ArrowObject"))
})


test_that("Testing wrong pattern with DTAFileTSV", {
  path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

  file_info <- DTAFileTSV("data_gf_dfjlkadwefwfew.*\\.tsv", pattern = TRUE, number_of_files = 1)

  expect_false(matches_filename(file_info, basename(path)))
})

test_that("DTAFile Creation", {
  path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

  file_info <- DTAFile("gf_data_small_smirna.tsv")

  expect_s3_class(file_info, "DTAtools::DTAFile")

  expect_true(matches_filename(file_info, basename(path)))
})

