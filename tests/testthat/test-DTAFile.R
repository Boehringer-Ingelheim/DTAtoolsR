test_that("DTAFileCSV object is created from reading in csv and table is accessible", {
  path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")

  file_info <- DTAFileCSV("clinical_data.csv")

  expect_s3_class(file_info, "DTAtools::DTAFile")
  expect_s3_class(file_info, "DTAtools::DTAFileCSV")

  expect_true(matches_filename(file_info, basename(path)))

  x <- read_file(file_info, path)

  # `expect_s3_class()` with a character vector is an ANY-match, so it passes
  # on a mis-parsed table. Assert every class and the parsed shape instead.
  expect_true(all(c("Table", "ArrowTabular") %in% class(x)))
  expect_equal(ncol(x), 14)
  expect_true(all(c("STUDYID", "AGE", "GENDER", "SUBJECT_ID") %in% names(x)))
})


test_that("matches_filename accepts full file paths", {
  path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
  file_info <- DTAFileCSV("clinical_data.csv")

  expect_true(matches_filename(file_info, path))
})

test_that("CSV headers are normalized before validation", {
  path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
  file_info <- DTAFileCSV("clinical_data.csv")

  x <- read_file(file_info, path)

  expect_true("AGE" %in% names(x))
  expect_false("\"AGE\"" %in% names(x))
  expect_false("AGE " %in% names(x))
})

test_that("dta_normalize_column_names strips quotes and surrounding whitespace", {
  # The fixture above reaches this helper already clean (arrow removes the
  # quotes), so the integration test passes even if the helper is deleted.
  # Exercise the helper directly on input that genuinely needs cleaning.
  tbl <- arrow::arrow_table(data.frame(a = 1, b = 2, c = 3))
  names(tbl) <- c('"AGE"', " SEX ", '  "RACE"  ')

  expect_equal(names(dta_normalize_column_names(tbl)), c("AGE", "SEX", "RACE"))
})

test_that("dta_normalize_column_names leaves already-clean names untouched", {
  tbl <- arrow::arrow_table(data.frame(AGE = 1, SEX = 2))

  expect_equal(names(dta_normalize_column_names(tbl)), c("AGE", "SEX"))
})


test_that("DTAFileTSV object is created from reading in tsv and table is accessible", {

  path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

  file_info <- DTAFileTSV("gf_data_small_smirna.tsv")

  expect_s3_class(file_info, "DTAtools::DTAFile")
  expect_s3_class(file_info, "DTAtools::DTAFileTSV")

  expect_true(matches_filename(file_info, basename(path)))

  x <- read_file(file_info, path)

  expect_true(all(c("Table", "ArrowTabular") %in% class(x)))
  # A tab file parsed with the wrong delimiter collapses to a single column,
  # which the previous ANY-match class assertion happily accepted.
  expect_gt(ncol(x), 1)
  expect_true(all(c("STUDYID", "DOMAIN") %in% names(x)))
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

test_that("DTAFile rejects a fixed filename combined with number_of_files > 1", {
  expect_error(
    DTAFile("gf_data_small_smirna.tsv", number_of_files = 2),
    "number_of_files"
  )
})

test_that("DTAFile pattern accessors report the configured bounds", {
  file_info <- DTAFile(
    "gf_data_.*\\.tsv",
    pattern = TRUE,
    min_number_of_files = 1,
    max_number_of_files = 3
  )

  expect_equal(min_number_of_files(file_info), 1)
  expect_equal(max_number_of_files(file_info), 3)
})

test_that("read_file refuses a path whose basename does not match", {
  other <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
  file_info <- DTAFileTSV("gf_data_small_smirna.tsv")

  # Bare expect_error() would also pass on a typo in the call itself.
  expect_error(read_file(file_info, other), "match")
})

test_that("read_file with namecheck = FALSE reads a non-matching path", {
  path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")
  file_info <- DTAFileTSV("some_other_name.tsv")

  x <- read_file(file_info, path, namecheck = FALSE)

  expect_true(all(c("Table", "ArrowTabular") %in% class(x)))
  expect_gt(ncol(x), 1)
})

test_that("DTAFileDelim reads tab-delimited files", {
  path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

  file_info <- DTAFileDelim("gf_data_small_smirna.tsv")

  expect_s3_class(file_info, "DTAtools::DTAFile")
  expect_s3_class(file_info, "DTAtools::DTAFileDelim")
  expect_true(matches_filename(file_info, basename(path)))

  x <- read_file(file_info, path)
  expect_s3_class(x, c("R6", "Table", "ArrowTabular", "ArrowObject"))
})

