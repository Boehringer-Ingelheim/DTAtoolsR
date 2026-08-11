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


# ---------------------------------------------------------------------------
# DTAFileTabular class registration
# ---------------------------------------------------------------------------

test_that("DTAFileTabular is registered under its own class name", {
  # DTAFileTabular used to be created with S7::new_class("DTAFile", ...), so it
  # shadowed its own parent: the class vector listed "DTAtools::DTAFile" twice
  # and every DTAFileTabular method overwrote the DTAFile method of the same
  # name.
  tabular <- DTAFileTabular("a.txt")

  expect_s3_class(tabular, "DTAtools::DTAFileTabular")
  expect_equal(
    class(DTAFileCSV("a.csv")),
    c(
      "DTAtools::DTAFileCSV",
      "DTAtools::DTAFileTabular",
      "DTAtools::DTAFile",
      "S7_object"
    )
  )
})

test_that("print() works on a plain DTAFile", {
  # While DTAFileTabular was registered as "DTAFile" its print_info method
  # replaced the DTAFile one, so printing a plain DTAFile looked for @sep.
  out <- cli::cli_fmt(print(DTAFile("a.txt")))

  expect_true(any(grepl("a.txt", out, fixed = TRUE)))
  expect_true(any(grepl("Number of files", out, fixed = TRUE)))
})

test_that("read_file_execution on a plain DTAFile hits the base abort", {
  err <- expect_error(
    read_file_execution(DTAFile("a.txt"), "x"),
    class = "rlang_error"
  )

  expect_match(conditionMessage(err), "not implemented")
  # The DTAFileTabular abort ("derived from DTAFileTabular class") used to be
  # reached instead, because it was registered against DTAFile.
  expect_false(grepl("DTAFileTabular", conditionMessage(err), fixed = TRUE))
})

test_that("read_file_execution on a DTAFileTabular hits the subclass abort", {
  err <- expect_error(
    read_file_execution(DTAFileTabular("a.txt"), "x"),
    class = "rlang_error"
  )

  expect_match(conditionMessage(err), "DTAFileTabular")
})


# ---------------------------------------------------------------------------
# DTAFileTabular validator
# ---------------------------------------------------------------------------

test_that("DTAFileTabular rejects a multi-character separator", {
  # Was inert: the message was computed by a bare `if` and thrown away, so the
  # object constructed successfully.
  expect_error(DTAFileTabular("a.txt", sep = "||"), "'sep' must be a single")
})

test_that("DTAFileTabular rejects a non-scalar has_header", {
  expect_error(
    DTAFileTabular("a.txt", has_header = c(TRUE, FALSE)),
    "'has_header' must be a single"
  )
})

test_that("DTAFileTabular rejects a multi-character quote", {
  expect_error(DTAFileTabular("a.txt", quote = "ab"), "'quote' must be a single")
})

test_that("DTAFileTabular reports every validator violation at once", {
  err <- expect_error(
    DTAFileTabular(
      "a.txt",
      sep = "||",
      has_header = c(TRUE, FALSE),
      quote = "ab"
    )
  )

  msg <- conditionMessage(err)
  expect_match(msg, "'sep' must be a single character.", fixed = TRUE)
  expect_match(msg, "'has_header' must be a single logical value.", fixed = TRUE)
  expect_match(msg, "'quote' must be a single character.", fixed = TRUE)
})

test_that("DTAFileTabular accepts a valid configuration", {
  tabular <- DTAFileTabular("a.txt", sep = ";", has_header = FALSE, quote = "'")

  expect_equal(tabular@sep, ";")
  expect_false(tabular@has_header)
  expect_equal(tabular@quote, "'")
})


# ---------------------------------------------------------------------------
# Separator handling
# ---------------------------------------------------------------------------

test_that("DTAFileDelim parses with its own separator, not a hardcoded comma", {
  path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

  x <- read_file(DTAFileDelim("gf_data_small_smirna.tsv"), path)

  # read_delim_arrow() was called without `delim`, so it fell back to a comma
  # and collapsed the tab file into a single column.
  expect_equal(ncol(x), 33)
  expect_equal(nrow(x), 20940)
  expect_true(all(c("STUDYID", "DOMAIN") %in% names(x)))
})

test_that("DTAFileDelim honours a non-tab separator", {
  path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")

  x <- read_file(DTAFileDelim("clinical_data.csv", sep = ","), path)

  expect_equal(ncol(x), 14)
  expect_true(all(c("STUDYID", "AGE") %in% names(x)))
})


# ---------------------------------------------------------------------------
# has_header handling
# ---------------------------------------------------------------------------

test_that("DTAFileTSV with has_header = FALSE keeps the first row as data", {
  path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

  x <- read_file(DTAFileTSV("gf_data_small_smirna.tsv", has_header = FALSE), path)

  # has_header = FALSE used to be implemented as skip = 1, which discarded the
  # first *data* row and promoted the second one to the header (20939 rows).
  expect_equal(nrow(x), 20941)
  expect_equal(ncol(x), 33)
  expect_false("STUDYID" %in% names(x))
  expect_false("1234-5678" %in% names(x))
})

test_that("DTAFileDelim with has_header = FALSE keeps the first row as data", {
  path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

  x <- read_file(DTAFileDelim("gf_data_small_smirna.tsv", has_header = FALSE), path)

  expect_equal(nrow(x), 20941)
  expect_equal(ncol(x), 33)
  expect_false("STUDYID" %in% names(x))
})

test_that("DTAFileCSV with has_header = FALSE keeps the first row as data", {
  path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")

  with_header <- read_file(DTAFileCSV("clinical_data.csv"), path)
  # has_header was ignored outright for CSV (the skip= line was commented out).
  without_header <- read_file(
    DTAFileCSV("clinical_data.csv", has_header = FALSE),
    path
  )

  expect_equal(nrow(without_header), nrow(with_header) + 1)
  expect_equal(ncol(without_header), ncol(with_header))
  expect_false("STUDYID" %in% names(without_header))
})

