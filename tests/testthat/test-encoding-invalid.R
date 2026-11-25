# Bytes that are not valid in a file's declared encoding are refused with the
# package's own condition class, `dta_encoding_invalid`, wherever they are met.
#
# For UTF-8, the default, the refusal is Arrow's. It reaches the caller with no
# class that says so: an `rlang_error` out of the in-memory reader and the
# lazy opener, a `simpleError` out of a scan, and a row of the wrong width or a
# truncated archive arrive as exactly the same kinds of condition. Each route
# that reads a delimited file is therefore pinned separately, on a real file, so
# that a route that stops being translated shows up as its own failure -- and
# the files that are NOT an encoding problem are pinned too, because a matcher
# that is too broad would pass every positive case here.
#
# The refusals are matched on their class. Their message is Arrow's, in Arrow's
# words, and a base-R message would be in the system language.

enc_specs <- function() {
  DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
      AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    )
  )
}

enc_handler <- function(path, encoding = NULL) {
  if (is.null(encoding)) {
    DTAFileCSV(filename = basename(path))
  } else {
    DTAFileCSV(filename = basename(path), encoding = encoding)
  }
}

enc_load <- function(path, stream, encoding = NULL) {
  dataset <- DTADataSetTabular(
    id = "enc", specs = enc_specs(), files = list(enc_handler(path, encoding))
  )
  load_file(dataset, file = path, handler_index = 1, stream = stream)
}

# What a caller meets, whichever of load_file() and check() it is that refuses:
# the condition, or NULL when the delivery was read and validated.
enc_refusal <- function(path, stream) {
  tryCatch(
    {
      check(enc_load(path, stream), persist = FALSE, quiet = TRUE)
      NULL
    },
    error = function(e) e
  )
}

# Bytes FF FE occur in no UTF-8 text.
enc_bad_id <- c(charToRaw("A00000"), as.raw(c(0xff, 0xfe)))
enc_valid_id <- c(charToRaw("A00000"), as.raw(c(0xc3, 0xa9)))

enc_write <- function(dir, name, ...) {
  path <- file.path(dir, name)
  writeBin(do.call(c, lapply(list(...), function(piece) if (is.raw(piece)) piece else charToRaw(piece))), path)
  path
}

# 3000 data rows, each 11 bytes, so that with 1 KiB read blocks a row after them
# is far behind the first block the lazy opener reads.
enc_valid_rows <- function() paste(sprintf("A%07d,30\n", seq_len(3000L)), collapse = "")

test_that("bytes that are not valid UTF-8 are refused with dta_encoding_invalid, read in memory or opened lazily", {
  dir <- withr::local_tempdir()
  only_row <- enc_write(dir, "only-row.csv", "ID,AGE\n", enc_bad_id, ",30\n")
  withr::local_options(DTAtools.stream_block_size = 1024L)
  late_row <- enc_write(
    dir, "late-row.csv", "ID,AGE\n", enc_valid_rows(), enc_bad_id, ",30\n"
  )

  # Eagerly the whole file is read at load time wherever the bytes are; lazily
  # only the first block is, so a bad row in it is met when the dataset is
  # opened.
  for (case in list(
    list(stream = "never", path = only_row),
    list(stream = "never", path = late_row),
    list(stream = "always", path = only_row)
  )) {
    err <- tryCatch(enc_load(case$path, case$stream), error = function(e) e)
    label <- paste(case$stream, basename(case$path))

    expect_s3_class(err, "dta_encoding_invalid")
    expect_s3_class(err, "rlang_error")
    # The file is named, and Arrow's own message -- which locates the bytes --
    # is kept as the condition's parent rather than thrown away.
    expect_match(conditionMessage(err), basename(case$path), fixed = TRUE, info = label)
    expect_true(dta_is_arrow_encoding_refusal(err$parent), info = label)
  }
})

test_that("a lazy table's later bad bytes are refused by check(), with the same class", {
  dir <- withr::local_tempdir()
  withr::local_options(DTAtools.stream_block_size = 1024L)
  late_row <- enc_write(
    dir, "late-row.csv", "ID,AGE\n", enc_valid_rows(), enc_bad_id, ",30\n"
  )

  # The first block is valid, so opening the dataset succeeds: nothing has read
  # the bad row yet.
  loaded <- enc_load(late_row, "always")
  expect_true(inherits(loaded, "DTAtools::DTADataSetTabular"))

  err <- tryCatch(check(loaded, persist = FALSE, quiet = TRUE), error = function(e) e)
  expect_s3_class(err, "dta_encoding_invalid")
  expect_true(dta_is_arrow_encoding_refusal(err$parent))
  # check() stops for the whole call, so a reader of it has to be told which
  # file it was.
  expect_match(conditionMessage(err), "late-row.csv", fixed = TRUE)

  # validate_file_stream() scans the same way and gets the same class.
  direct <- tryCatch(
    validate_file_stream(enc_specs(), late_row, verbose = FALSE),
    error = function(e) e
  )
  expect_s3_class(direct, "dta_encoding_invalid")
  expect_match(conditionMessage(direct), "late-row.csv", fixed = TRUE)
})

test_that("a header that is not valid UTF-8 is refused with dta_encoding_invalid", {
  dir <- withr::local_tempdir()
  bad_header <- enc_write(
    dir, "bad-header.csv", "ID", as.raw(c(0xff, 0xfe)), ",AGE\nA0000001,30\n"
  )

  for (stream in c("never", "always")) {
    err <- tryCatch(enc_load(bad_header, stream), error = function(e) e)
    expect_s3_class(err, "dta_encoding_invalid")
    expect_s3_class(err, "rlang_error")
    expect_match(conditionMessage(err), "bad-header.csv", fixed = TRUE, info = stream)
  }
})

test_that("a matrix dataset's header that is not valid UTF-8 is refused with dta_encoding_invalid", {
  dir <- withr::local_tempdir()
  header <- enc_write(dir, "m.csv", "a,b", as.raw(c(0xff, 0xfe)), "\n1,2,3\n")
  ok <- enc_write(dir, "ok.csv", "a,b", as.raw(c(0xc3, 0xa9)), "\n1,2,3\n")

  err <- tryCatch(dta_matrix_read_head(DTAFileCSV(filename = "m.csv"), header), error = function(e) e)
  expect_s3_class(err, "dta_encoding_invalid")
  expect_match(conditionMessage(err), "m.csv", fixed = TRUE)

  # The same header with valid bytes reads, so the refusal is about the bytes.
  expect_identical(
    dta_matrix_read_head(DTAFileCSV(filename = "ok.csv"), ok)$fields,
    c("a", "bé")
  )
})

test_that("bytes that are not valid in a declared non-UTF-8 encoding are refused with the same class by both readers", {
  dir <- withr::local_tempdir()
  path <- enc_write(dir, "sjis.csv", "ID,AGE\n", as.raw(c(0x41, 0x81, 0xff, 0xfe)), ",30\n")

  # The in-memory reader has Arrow re-encode, the lazy one converts the file
  # first; the same bytes must meet the same class on both. Windows' iconv is
  # lenient about several code pages, so the assertions run only where the
  # platform refuses these bytes -- and then both readers have to, a reader that
  # accepted them would be the failure this test is for.
  skip_if_not(
    is.na(iconv(rawToChar(as.raw(c(0x41, 0x81, 0xff, 0xfe))), from = "SHIFT-JIS", to = "UTF-8")),
    "this platform's iconv accepts the bytes this test needs rejected"
  )

  for (stream in c("never", "always")) {
    err <- tryCatch(enc_load(path, stream, encoding = "SHIFT-JIS"), error = function(e) e)
    expect_s3_class(err, "dta_encoding_invalid")
    expect_match(conditionMessage(err), "SHIFT-JIS", fixed = TRUE, info = stream)
  }
})

test_that("a read that is not an encoding failure is not given the class", {
  dir <- withr::local_tempdir()
  withr::local_options(DTAtools.stream_block_size = 1024L)

  # A row with a field too many, far down: Arrow refuses it in the same style
  # as it refuses bytes, and a classifier that keyed on that style would
  # classify it.
  ragged <- enc_write(
    dir, "ragged.csv", "ID,AGE\n", enc_valid_rows(), "A9999999,30,EXTRA\n"
  )
  # The same row, and Arrow quotes it in its message together with its own
  # invalid bytes. Looking for the refusal in that message must neither warn
  # nor mistake it for one.
  ragged_bytes <- enc_write(
    dir, "ragged-bytes.csv", "ID,AGE\n", enc_valid_rows(), enc_bad_id, ",30,EXTRA\n"
  )
  # A gzip archive cut in half: Arrow's IOError.
  whole <- file.path(dir, "whole.csv.gz")
  con <- gzfile(whole, "wb")
  writeLines(c("ID,AGE", sprintf("A%07d,%d", seq_len(20000L), seq_len(20000L) %% 100L)), con)
  close(con)
  archive <- readBin(whole, "raw", n = file.size(whole))
  truncated <- file.path(dir, "truncated.csv.gz")
  writeBin(archive[seq_len(floor(length(archive) / 2))], truncated)

  for (case in list(
    list(what = "a ragged row", path = ragged),
    list(what = "a ragged row holding invalid bytes", path = ragged_bytes),
    list(what = "a truncated archive", path = truncated)
  )) {
    for (stream in c("never", "always")) {
      label <- paste(case$what, stream)
      # Arrow's own error handling greps the message it is quoting the bad
      # bytes in and warns about it ("input string 1 is invalid in this
      # locale"); that is not ours to assert on, and the matcher is shown not to
      # add one of its own below.
      err <- suppressWarnings(enc_refusal(case$path, stream))
      expect_s3_class(err, "error")
      expect_false(inherits(err, "dta_encoding_invalid"), info = label)
    }
  }
})

test_that("a declared encoding iconv does not know is refused by name, on both readers, and not blamed on the bytes", {
  dir <- withr::local_tempdir()
  # `latin-1` is not a name iconv() accepts (`latin1` is). A header byte that
  # is valid text in the encoding that was meant, and a header with none, so
  # that neither can be what the refusal is about: what is wrong is the
  # declaration.
  non_ascii <- enc_write(dir, "non-ascii.csv", "ID", as.raw(0xe9), ",AGE\nA0000001,30\n")
  ascii <- enc_write(dir, "ascii.csv", "ID,AGE\nA0000001,30\n")

  for (path in c(non_ascii, ascii)) {
    for (stream in c("never", "always")) {
      err <- tryCatch(enc_load(path, stream, encoding = "latin-1"), error = function(e) e)
      label <- paste(basename(path), stream)
      expect_s3_class(err, "dta_encoding_unsupported")
      expect_false(inherits(err, "dta_encoding_invalid"), info = label)
      # The file and the encoding are named, and not by a base-R message.
      expect_match(conditionMessage(err), "latin-1", fixed = TRUE, info = label)
    }
  }
})

test_that("the same files with valid bytes are read and checked", {
  dir <- withr::local_tempdir()
  withr::local_options(DTAtools.stream_block_size = 1024L)
  only_row <- enc_write(dir, "only-row.csv", "ID,AGE\n", enc_valid_id, ",30\n")
  late_row <- enc_write(
    dir, "late-row.csv", "ID,AGE\n", enc_valid_rows(), enc_valid_id, ",30\n"
  )
  header <- enc_write(dir, "header.csv", "ID", as.raw(c(0xc3, 0xa9)), ",AGE\nA0000001,30\n")

  for (path in c(only_row, late_row, header)) {
    for (stream in c("never", "always")) {
      expect_null(enc_refusal(path, stream), info = paste(basename(path), stream))
    }
  }
})

test_that("dta_with_encoding_refusal() translates only Arrow's refusal of bad bytes and leaves everything else alone", {
  # A value passes through.
  expect_identical(dta_with_encoding_refusal(42L), 42L)

  # An error that is not that refusal is the same error: same message, no
  # class added, nothing swallowed.
  other <- tryCatch(dta_with_encoding_refusal(stop("boom", call. = FALSE)), error = function(e) e)
  expect_identical(conditionMessage(other), "boom")
  expect_false(inherits(other, "dta_encoding_invalid"))

  # The words Arrow uses, in a condition of the class a scan raises it in. Real
  # Arrow output is exercised above; this pins what the matcher keys on.
  arrow_words <- "Invalid: In CSV column #0: Row #2: CSV conversion error to string: invalid UTF8 data"
  translated <- tryCatch(
    dta_with_encoding_refusal(stop(simpleError(arrow_words)), path = "delivery.csv"),
    error = function(e) e
  )
  expect_s3_class(translated, "dta_encoding_invalid")
  expect_identical(conditionMessage(translated$parent), arrow_words)
  expect_match(conditionMessage(translated), "delivery.csv", fixed = TRUE)

  # Without a path the message still reads.
  expect_s3_class(
    tryCatch(dta_with_encoding_refusal(stop(simpleError(arrow_words))), error = function(e) e),
    "dta_encoding_invalid"
  )

  # An error that merely has the refusal among its causes is not the refusal:
  # its own message is what is read, so it is not reclassified.
  wrapping <- tryCatch(
    dta_with_encoding_refusal(
      cli::cli_abort("The reader failed.", class = "dta_other", parent = simpleError(arrow_words))
    ),
    error = function(e) e
  )
  expect_s3_class(wrapping, "dta_other")
  expect_false(inherits(wrapping, "dta_encoding_invalid"))

  # A message that quotes the delivery's own bytes, which are not valid text,
  # is read without a warning and without being taken for the refusal.
  quoting <- simpleError(paste0(
    "Invalid: CSV parse error: Row #3002: Expected 2 columns, got 3: A00000",
    rawToChar(as.raw(c(0xff, 0xfe))), ",30,EXTRA"
  ))
  expect_no_warning(verdict <- dta_is_arrow_encoding_refusal(quoting))
  expect_false(verdict)

  # Other Arrow refusals share the "Invalid:" prefix and the "CSV" wording.
  for (words in c(
    "Invalid: CSV parse error: Row #3002: Expected 2 columns, got 3: a,b,c",
    "Invalid: CSV conversion error to int64: invalid value 'x'",
    "IOError: Truncated compressed stream"
  )) {
    expect_false(dta_is_arrow_encoding_refusal(simpleError(words)), info = words)
  }
})
