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

test_that("a gzipped latin1 file streams, and agrees with every other route", {
  # Two transport details at once: the bytes are compressed AND they are not
  # UTF-8. The lazy reader has to decompress through gzfile() to convert, then
  # hand the scanner plain UTF-8 text -- so this is the case where a converter
  # that expanded the archive to disk first, or that read the compressed bytes
  # as if they were text, would show up.
  plain <- file.path(tempdir(), "gz_latin1.csv")
  gz <- paste0(plain, ".gz")
  on.exit(unlink(c(plain, gz)), add = TRUE)

  bytes <- c(
    charToRaw("NAME,V\n"),
    as.raw(c(0x4a, 0xfc, 0x72, 0x67, 0x65, 0x6e)), charToRaw(",1\n"),
    as.raw(c(0x4d, 0xf6, 0x6c, 0x6c, 0x65, 0x72)), charToRaw(",2\n")
  )
  con <- file(plain, "wb")
  writeBin(bytes, con)
  close(con)
  gz_compress(plain, gz)

  handler <- DTAFileCSV(filename = "gz_latin1.csv", encoding = "latin1")
  expected <- c("Jürgen", "Möller")

  expect_identical(as.data.frame(read_file(handler, plain))$NAME, expected)
  expect_identical(as.data.frame(read_file(handler, gz))$NAME, expected)
  expect_identical(as.data.frame(open_file(handler, plain))$NAME, expected)
  expect_identical(as.data.frame(open_file(handler, gz))$NAME, expected)

  # The scan reads a converted copy, and the copy is plain text under
  # tempdir() -- not a second archive left next to the delivery.
  scanned <- normalizePath(open_file(handler, gz)$files[[1]], winslash = "/")
  expect_true(startsWith(scanned, normalizePath(tempdir(), winslash = "/")))
  expect_false(identical(scanned, normalizePath(gz, winslash = "/")))
  expect_identical(readBin(scanned, "raw", n = 2L), as.raw(c(0x4e, 0x41)))
})

test_that("a gzipped latin1 file gets the same verdict on all four routes", {
  # Reading the same values is necessary but not sufficient: what a user acts
  # on is the verdict. Compression and encoding are both transport details, so
  # all four combinations of {plain, gzipped} x {read into memory, streamed}
  # must produce one answer -- counts, statuses and error detail alike.
  plain <- file.path(tempdir(), "gz_latin1_verdict.csv")
  gz <- paste0(plain, ".gz")
  on.exit(unlink(c(plain, gz)), add = TRUE)

  con <- file(plain, "wb")
  writeBin(
    c(
      charToRaw("NAME,V\n"),
      as.raw(c(0x4a, 0xfc, 0x72, 0x67, 0x65, 0x6e)), charToRaw(",1\n"),
      as.raw(c(0x4d, 0xf6, 0x6c, 0x6c, 0x65, 0x72)), charToRaw(",2\n"),
      # An unconvertible number and a duplicated key, so this compares real
      # error detail on all three axes rather than four clean verdicts.
      charToRaw("Ann,notanumber\n"),
      charToRaw("Ann,4\n")
    ),
    con
  )
  close(con)
  gz_compress(plain, gz)

  specs <- DTAColumnSpecCollection(
    columns = list(
      NAME = DTAColumnSpec(id = "NAME", type = "SAS Char", length = 6, nullable = FALSE),
      V = DTAColumnSpec(id = "V", type = "SAS Num", nullable = FALSE)
    ),
    rules = list(DTARuleColUnique(id = "name_unique", columns = "NAME"))
  )

  # Two routes may legitimately report the same errors in a different order,
  # and an order difference is not a disagreement.
  sorted <- function(errors) {
    if (nrow(errors) == 0) {
      return(errors)
    }
    out <- errors[do.call(order, lapply(errors, as.character)), , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  route <- function(path, stream) {
    ds <- DTADataSetTabular(
      name = "enc",
      specs = specs,
      files = list(DTAFileCSV(filename = "gz_latin1_verdict.csv", encoding = "latin1"))
    )
    checked <- check(
      load_file(ds, file = path, handler_index = 1, stream = stream),
      quiet = TRUE, persist = FALSE
    )
    status <- validation_status(checked)
    list(
      status = status[
        , setdiff(names(status), c("validated_at", "run_id", "validation_run")),
        drop = FALSE
      ],
      errors = sorted(as.data.frame(validation_errors(checked, "gz_latin1_verdict"))),
      n_import_errors = checked@validation_store[["gz_latin1_verdict"]]$n_import_errors,
      values = as.data.frame(tables(checked)[["gz_latin1_verdict"]])$NAME
    )
  }

  reference <- route(plain, "never")

  # A fixture that passed everything would prove nothing about the error paths.
  expect_false(reference$status$ok)
  expect_gt(nrow(reference$errors), 0)
  expect_gt(reference$n_import_errors, 0)
  expect_identical(reference$values, c("Jürgen", "Möller", "Ann", "Ann"))

  for (route_name in c("plain streamed", "gz in memory", "gz streamed")) {
    got <- switch(route_name,
      "plain streamed" = route(plain, "always"),
      "gz in memory" = route(gz, "never"),
      "gz streamed" = route(gz, "always")
    )
    expect_identical(got$status, reference$status, info = route_name)
    expect_identical(got$errors, reference$errors, info = route_name)
    expect_identical(got$n_import_errors, reference$n_import_errors, info = route_name)
    expect_identical(got$values, reference$values, info = route_name)
  }
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
