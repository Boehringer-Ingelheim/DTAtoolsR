# Gzipped input.
#
# Arrow decompresses `.gz` transparently on read, so the reader was never the
# problem. What was missing is that a specification declaring `data.csv` did not
# recognise `data.csv.gz` as the file it was asking for -- compression is a
# transport detail, not part of the data's identity. These tests pin both
# halves: that the handler matches, and that compressing a file changes nothing
# about the verdict it produces.

gz_extdata <- function(name) {
  path <- system.file("extdata", name, package = "DTAtools")
  # Guaranteed package assets -- a missing fixture is a failure, not a skip.
  expect_true(nzchar(path), info = paste(name, "missing from extdata"))
  path
}

# Compress `from` to `to`, byte for byte.
gz_compress <- function(from, to) {
  bytes <- readBin(from, "raw", n = file.size(from))
  con <- gzfile(to, "wb")
  on.exit(close(con), add = TRUE)
  writeBin(bytes, con)
  invisible(to)
}

test_that("a declared filename also matches its gzipped form", {
  handler <- DTAFileCSV("clinical_data2.csv")

  expect_true(matches_filename(handler, "clinical_data2.csv"))
  expect_true(matches_filename(handler, "clinical_data2.csv.gz"))

  # Stripping the suffix must not turn an unrelated file into a match, and only
  # a known compression suffix is stripped -- not any second extension.
  expect_false(matches_filename(handler, "somethingelse.csv.gz"))
  expect_false(matches_filename(handler, "clinical_data2.csv.bak"))
})

test_that("an anchored filename pattern still matches a gzipped file", {
  handler <- DTAFileCSV("^clinical_data2\\.csv$", pattern = TRUE)

  expect_true(matches_filename(handler, "clinical_data2.csv"))
  expect_true(matches_filename(handler, "clinical_data2.csv.gz"))
  expect_false(matches_filename(handler, "clinical_data9.csv.gz"))
})

test_that("the bundled gzipped example reads back identical to the plain csv", {
  handler <- DTAFileCSV("clinical_data2.csv")

  plain <- as.data.frame(read_file(handler, gz_extdata("clinical_data2.csv")))
  gzipped <- as.data.frame(read_file(handler, gz_extdata("clinical_data2.csv.gz")))

  expect_gt(nrow(plain), 0)
  expect_equal(gzipped, plain)
})

test_that("streaming validation of a gzipped file matches the uncompressed file", {
  dta <- read_dta_from_yaml(gz_extdata("clinical_dta.yaml"))
  specs <- dta[["clinical_data"]]@specs

  plain <- gz_extdata("clinical_data_error_columnspec.csv")
  gz <- file.path(tempdir(), "clinical_data_error_columnspec.csv.gz")
  on.exit(unlink(gz), add = TRUE)
  gz_compress(plain, gz)

  expected <- validate_file_stream(specs, plain, verbose = FALSE)
  actual <- validate_file_stream(specs, gz, verbose = FALSE)

  # A fixture that actually fails, so this compares real error detail rather
  # than two empty frames.
  expect_gt(expected$n_columnspec_errors, 0)

  expect_equal(actual$ok, expected$ok)
  expect_equal(actual$n_columnspec_errors, expected$n_columnspec_errors)
  expect_equal(actual$n_rule_errors, expected$n_rule_errors)
  expect_equal(actual$n_import_errors, expected$n_import_errors)
  expect_equal(
    actual$columnspec_errors$full_error,
    expected$columnspec_errors$full_error
  )
})

test_that("a gzipped file is still read in batches, with file row numbers", {
  dta <- read_dta_from_yaml(gz_extdata("clinical_dta.yaml"))
  specs <- dta[["clinical_data"]]@specs

  plain <- gz_extdata("clinical_data_error_columnspec.csv")
  gz <- file.path(tempdir(), "clinical_data_batched.csv.gz")
  on.exit(unlink(gz), add = TRUE)
  gz_compress(plain, gz)

  expected <- validate_file_stream(specs, plain, verbose = FALSE)
  # Small enough to force many batches out of the compressed stream; the row
  # numbers reported must still be positions in the file, not in a batch.
  batched <- validate_file_stream(specs, gz, batch_rows = 64L, verbose = FALSE)

  expect_equal(
    batched$columnspec_errors$full_error,
    expected$columnspec_errors$full_error
  )
})

test_that("a gzipped file can be loaded and checked through the DTA entry points", {
  plain_path <- gz_extdata("clinical_data.csv")
  gz <- file.path(tempdir(), "clinical_data.csv.gz")
  on.exit(unlink(gz), add = TRUE)
  gz_compress(plain_path, gz)

  from_plain <- results(check(
    load_file(read_dta_from_yaml(gz_extdata("clinical_dta.yaml")), 1, file = plain_path),
    persist = FALSE, quiet = TRUE
  ))
  from_gz <- results(check(
    load_file(read_dta_from_yaml(gz_extdata("clinical_dta.yaml")), 1, file = gz),
    persist = FALSE, quiet = TRUE
  ))

  expect_equal(nrow(from_gz), 1)
  expect_equal(from_gz$status, from_plain$status)
  expect_equal(from_gz$n_columnspec_errors, from_plain$n_columnspec_errors)
  expect_equal(from_gz$n_rule_errors, from_plain$n_rule_errors)
  expect_equal(from_gz$n_import_errors, from_plain$n_import_errors)
})
