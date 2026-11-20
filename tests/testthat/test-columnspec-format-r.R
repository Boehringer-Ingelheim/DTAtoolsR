# The R backend end to end: a column declared `R <type>` is judged by the same
# evaluator that judges `SAS <type>`, and an R date/time FORMAT (`%Y-%m-%d`, ...)
# is enforced on text values.
#
# Every value case runs through BOTH engines -- the materialising
# validate_table_detailed() and the streaming batch loop -- because both are
# fed by the one evaluator, and the property worth pinning is that they agree.
# Expectations are derived by hand from the format definitions (a fixed-width
# field per directive, a day within its month, the exact text and nothing
# around it), never taken from the code under test, and cases are matched on
# the constraint keyword or condition class, not on translated text.

rf_col <- function(...) {
  DTAColumnSpecCollection(columns = list(X = DTAColumnSpec(id = "X", ...)))
}

# The errors each engine reports for one single-column table, as
# "<row> <keyword>" pairs (and the offending text, for the cases that need it).
rf_errors <- function(specs, values) {
  tab <- data.frame(X = values, stringsAsFactors = FALSE)
  eager <- validate_table_detailed(specs, tab, verbose = FALSE)
  streamed <- dta_validate_table_stream(
    specs,
    dta_as_batch_reader(arrow::as_arrow_table(tab), batch_rows = 2L),
    verbose = FALSE, coerce = FALSE
  )
  list(
    eager = eager$columnspec_errors$full_error,
    streamed = streamed$columnspec_errors$full_error
  )
}

rf_pairs <- function(err) {
  if (is.null(err)) character(0) else paste(err$row, err$keyword)
}

rf_expect <- function(specs, values, expected) {
  found <- rf_errors(specs, values)
  expect_setequal(rf_pairs(found$eager), expected)
  expect_setequal(rf_pairs(found$streamed), expected)
}


# ---- one test per R type -------------------------------------------------------

test_that("R character is checked for its length, in characters", {
  specs <- rf_col(type = "R character", length = 3, nullable = FALSE)
  # "ééé" is three characters and six bytes: it fits.
  rf_expect(specs, c("ab", "abc", "abcd", "ééé"), "3 maxLength")
})

test_that("R numeric takes numbers, and nothing else", {
  specs <- rf_col(type = "R numeric", nullable = FALSE)
  rf_expect(specs, c(1.5, NA, 3, -0.25), "2 type")
  # Text where a number is declared is a type finding on every row (a raw table
  # is never coerced).
  rf_expect(specs, c("1", "2"), c("1 type", "2 type"))
  rf_expect(rf_col(type = "R numeric", nullable = TRUE), c(1.5, NA), character(0))
})

test_that("R integer takes whole numbers within the 32-bit range and rejects a fraction", {
  specs <- rf_col(type = "R integer", nullable = FALSE)
  # 2.5 has a fraction; 5e9 is whole but beyond +-(2^31 - 1); 7 is whole.
  rf_expect(specs, c(1, 2.5, 7, 5e9, -5e9), c("2 type", "4 type", "5 type"))
  rf_expect(specs, c(1L, NA, 3L), "2 type")
})

test_that("R logical takes logical values, and a missing one only when nullable", {
  rf_expect(rf_col(type = "R logical", nullable = TRUE), c(TRUE, FALSE, NA), character(0))
  rf_expect(rf_col(type = "R logical", nullable = FALSE), c(TRUE, FALSE, NA), "3 type")
  # Text that merely looks logical is not a logical value in a raw table.
  rf_expect(rf_col(type = "R logical", nullable = FALSE), c("TRUE", "F"), c("1 type", "2 type"))
})

test_that("R Date, POSIXct and hms take text in their stored default format", {
  # The default is stored by the constructor, so the format is in force with
  # none declared.
  expect_identical(DTAColumnSpec(id = "X", type = "R Date")@structure@format, "%Y-%m-%d")

  rf_expect(
    rf_col(type = "R Date", nullable = TRUE),
    c(
      "2024-02-29", "2023-02-29", "2024-04-31", "2024-1-5", "2024-13-01",
      "20240105", "2024-01-05T10:30:00", "0000-02-29", NA, "9999-12-31"
    ),
    # Not a leap year, 31 April, one-digit fields, month 13, no separators and
    # a time suffix all fail; year 0000 is leap and the last day of 9999 is fine.
    c("2 format", "3 format", "4 format", "5 format", "6 format", "7 format")
  )
  rf_expect(
    rf_col(type = "R POSIXct", nullable = TRUE),
    c(
      "2024-01-05 10:30:00", "2024-01-05T10:30:00", "2024-01-05 10:30",
      "2024-02-30 10:30:00", "2024-01-05 23:59:60", "2024-01-05 24:00:00",
      "2024-01-05 00:00:00"
    ),
    c("2 format", "3 format", "4 format", "5 format", "6 format")
  )
  # `hms` is a time of day: 24 hours and more are not one, nor are short fields.
  rf_expect(
    rf_col(type = "R hms", nullable = TRUE),
    c("00:00:00", "23:59:59", "24:00:00", "7:05:00", "12:60:00", "100:00:00"),
    c("3 format", "4 format", "5 format", "6 format")
  )
})

test_that("a custom R format replaces the default", {
  # Day first, with slashes.
  rf_expect(
    rf_col(type = "R Date", format = "R %d/%m/%Y", nullable = TRUE),
    c("05/01/2024", "29/02/2024", "29/02/2023", "2024-01-05", "5/1/2024"),
    c("3 format", "4 format", "5 format")
  )
  # No separators at all: the fixed widths keep it unambiguous.
  rf_expect(
    rf_col(type = "R Date", format = "R %Y%m%d", nullable = TRUE),
    c("20240105", "20240230", "2024-01-05"),
    c("2 format", "3 format")
  )
  # ISO 8601 with a literal T and Z: the letters are matched exactly, so a
  # lower-case one, a missing one or a space instead of the T fails.
  rf_expect(
    rf_col(type = "R POSIXct", format = "R %Y-%m-%dT%H:%M:%SZ", nullable = TRUE),
    c(
      "2024-01-05T10:30:00Z", "2024-01-05t10:30:00Z", "2024-01-05T10:30:00z",
      "2024-01-05T10:30:00", "2024-01-05 10:30:00Z", "2024-01-05T24:00:00Z",
      "2024-01-05T23:59:59Z"
    ),
    c("2 format", "3 format", "4 format", "5 format", "6 format")
  )
  rf_expect(
    rf_col(type = "R hms", format = "R %H:%M", nullable = TRUE),
    c("07:05", "23:59", "24:00", "07:05:00"),
    c("3 format", "4 format")
  )
  # %OS3: whole seconds, or a fraction of one to three digits, and no more.
  rf_expect(
    rf_col(type = "R hms", format = "R %H:%M:%OS3", nullable = TRUE),
    c("10:30:00", "10:30:00.5", "10:30:00.123", "10:30:00.1234", "10:30:00.", "10:30:60.0"),
    c("4 format", "5 format", "6 format")
  )
})


# ---- missing values, padding, typed vectors -------------------------------------

test_that("a missing value is never a format finding, only a type finding when not nullable", {
  rf_expect(rf_col(type = "R Date", nullable = TRUE), c("2024-01-05", NA), character(0))
  # One finding, and it is the type: "must be string", not "must fit format".
  rf_expect(rf_col(type = "R Date", nullable = FALSE), c("2024-01-05", NA), "2 type")
})

test_that("a padded value is rejected, where a SAS display value is trimmed", {
  rf_expect(
    rf_col(type = "R Date", nullable = FALSE),
    c(" 2024-01-05", "2024-01-05 ", "2024-01-05"),
    c("1 format", "2 format")
  )
  # A blank is not a date either (the CSV reader makes it missing before this).
  rf_expect(rf_col(type = "R Date", nullable = FALSE), c("", "2024-01-05"), "1 format")
  # The SAS side trims and still accepts the same padding.
  rf_expect(
    rf_col(type = "SAS Date", format = "SAS DATE9.", nullable = FALSE),
    c(" 15AUG2025", "15AUG2025 "),
    character(0)
  )
})

test_that("numbers in a column declared R Date are a type finding, not a silent pass", {
  specs <- rf_col(type = "R Date", nullable = FALSE)
  rf_expect(specs, c(20000, 20001), c("1 type", "2 type"))
  rf_expect(specs, c(20000L, 20001L), c("1 type", "2 type"))
  rf_expect(specs, c(TRUE, FALSE), c("1 type", "2 type"))
})

test_that("a typed Date or POSIXct vector is not matched against an R format", {
  # Neither carries display text, so the format check skips them. A format they
  # could never match makes the point: no finding.
  odd <- rf_col(type = "R Date", format = "R %d/%m/%Y", nullable = TRUE)
  rf_expect(odd, as.Date(c("2024-01-05", NA)), character(0))
  rf_expect(
    rf_col(type = "R POSIXct", format = "R %Y%m%d %H%M", nullable = TRUE),
    as.POSIXct(c("2024-01-05 10:30:00", NA), tz = "UTC"),
    character(0)
  )
})

test_that("a typed hms or difftime vector is skipped by an R format and checked by a SAS one", {
  schema_r <- list(type = c("string", "null"), format = "%H:%M:%S")
  hms_like <- structure(c(52200, NA), class = c("hms", "difftime"), units = "secs")
  plain <- structure(c(52200, NA), class = "difftime", units = "secs")
  expect_null(dta_check_column_spec("X", schema_r, hms_like))
  expect_null(dta_check_column_spec("X", schema_r, plain))

  # The SAS branch is unchanged: it does not skip a difftime, and a plain one
  # reads as the text "52200", which has no colon and so is not a TIME8. value.
  # KNOWN DEFECT, pinned rather than endorsed: a typed Date or POSIXct is skipped
  # there and a difftime is not. When the SAS branch skips difftime as well,
  # switch the two finding assertions below to expect_null(found).
  schema_sas <- list(type = c("string", "null"), format = "TIME8.")
  found <- dta_check_column_spec("X", schema_sas, plain)
  expect_identical(found$row, 1L)
  expect_identical(found$keyword, "format")
  # ... while a typed Date or POSIXct stays skipped there, as it always was.
  expect_null(dta_check_column_spec(
    "X", list(type = "string", format = "DATE9."), as.Date("2024-01-05")
  ))
  expect_null(dta_check_column_spec(
    "X", list(type = "string", format = "DATETIME20."),
    as.POSIXct("2024-01-05 10:30:00", tz = "UTC")
  ))
})

test_that("a format that is neither SAS nor R is still not enforced", {
  # No `%`, not a SAS format: nothing to evaluate, and no error either.
  expect_null(dta_check_column_spec("X", list(type = "string", format = "yyyy-mm-dd"), "anything"))
})


# ---- values and examples against type and format --------------------------------

test_that("values and examples must fit an R temporal format at construction", {
  # The check that would catch these runs inside a tryCatch for the schema; a
  # bad entry must still be refused, not dropped.
  expect_error(
    DTAColumnSpec(id = "X", type = "R Date", examples = "2024-02-30"),
    class = "dta_spec_value_mismatch"
  )
  expect_error(
    DTAColumnSpec(id = "X", type = "R Date", values = c("2024-01-05", "2024-1-5")),
    class = "dta_spec_value_mismatch"
  )
  expect_error(
    DTAColumnSpec(
      id = "X", type = "R POSIXct", format = "R %Y-%m-%dT%H:%M:%SZ",
      examples = "2024-01-05T10:30:00"
    ),
    class = "dta_spec_value_mismatch"
  )
  expect_error(
    DTAColumnSpec(id = "X", type = "R hms", format = "R %H:%M", examples = "24:00"),
    class = "dta_spec_value_mismatch"
  )
  e <- expect_error(
    DTAColumnSpec(id = "X", type = "R Date", examples = "2024-02-30"),
    class = "dta_spec_value_mismatch"
  )
  expect_match(conditionMessage(e), "2024-02-30", fixed = TRUE)

  expect_no_error(
    DTAColumnSpec(id = "X", type = "R Date", examples = c("2024-02-29", "0000-02-29"))
  )
  expect_no_error(DTAColumnSpec(id = "X", type = "R Date", values = "2024-01-05"))
  expect_no_error(
    DTAColumnSpec(
      id = "X", type = "R POSIXct", format = "R %Y-%m-%dT%H:%M:%SZ",
      examples = "2024-01-05T10:30:00Z"
    )
  )
  expect_no_error(
    DTAColumnSpec(id = "X", type = "R hms", format = "R %H:%M:%OS3", examples = "10:30:00.5")
  )
  # A missing example is not a value that fails.
  expect_no_error(
    DTAColumnSpec(id = "X", type = "R Date", nullable = TRUE, examples = c("2024-01-05", NA))
  )
})

test_that("declared values restrict an R Date column as well as its format", {
  specs <- rf_col(type = "R Date", values = c("2024-01-05", "2024-02-29"), nullable = FALSE)
  rf_expect(
    specs,
    c("2024-01-05", "2024-02-29", "2024-03-01", "2024-02-30"),
    # 3 is a real date outside the list; 4 breaks the list and the format.
    c("3 enum", "4 enum", "4 format")
  )
})

test_that("an R logical spec accepts its tokens as values and examples", {
  expect_no_error(DTAColumnSpec(id = "X", type = "R logical", values = c("TRUE", "FALSE")))
  expect_no_error(DTAColumnSpec(id = "X", type = "R logical", examples = "T"))
  expect_no_error(
    DTAColumnSpec(id = "X", type = "R logical", nullable = TRUE, values = c("TRUE", "FALSE"))
  )
  expect_no_error(
    DTAColumnSpec(id = "X", type = "R logical", nullable = TRUE, examples = c("true", "F", NA))
  )
  expect_no_error(DTAColumnSpec(id = "X", type = "R logical", values = "TRUE"))

  # `yes`, `1` and padded text are not tokens.
  expect_error(
    DTAColumnSpec(id = "X", type = "R logical", examples = "yes"),
    class = "dta_spec_value_mismatch"
  )
  expect_error(
    DTAColumnSpec(id = "X", type = "R logical", values = c("TRUE", "maybe")),
    class = "dta_spec_value_mismatch"
  )
  expect_error(
    DTAColumnSpec(id = "X", type = "R logical", examples = "1"),
    class = "dta_spec_value_mismatch"
  )
  expect_error(
    DTAColumnSpec(id = "X", type = "R logical", examples = " TRUE"),
    class = "dta_spec_value_mismatch"
  )
  # YAML's unquoted true/false are a logical vector: still refused, with the
  # quoting hint.
  expect_error(
    DTAColumnSpec(id = "X", type = "R logical", values = c(TRUE, FALSE)),
    class = "dta_spec_logical_values"
  )
  # The evaluator would skip a pattern on a boolean, so it is refused outright.
  expect_error(
    DTAColumnSpec(id = "X", type = "R logical", pattern = "^T"),
    class = "dta_spec_logical_pattern"
  )
})

test_that("declared values restrict an R logical column", {
  # A nullable column's single value becomes the list {TRUE, missing}.
  specs <- rf_col(type = "R logical", values = "TRUE", nullable = TRUE)
  rf_expect(specs, c(TRUE, FALSE, NA), "2 enum")
  rf_expect(rf_col(type = "R logical", values = "TRUE", nullable = FALSE), c(TRUE, FALSE), "2 const")

  both <- rf_col(type = "R logical", values = c("TRUE", "FALSE"), nullable = TRUE)
  rf_expect(both, c(TRUE, FALSE, NA), character(0))
})

test_that("R numeric and R integer values are read as numbers", {
  expect_no_error(DTAColumnSpec(id = "X", type = "R numeric", values = c(1, 2.5, 1e10)))
  expect_no_error(DTAColumnSpec(id = "X", type = "R integer", examples = c(1, 2)))
  expect_error(
    DTAColumnSpec(id = "X", type = "R integer", values = "1.5"),
    class = "dta_spec_value_mismatch"
  )
  expect_error(
    DTAColumnSpec(id = "X", type = "R numeric", examples = "abc"),
    class = "dta_spec_value_mismatch"
  )
})


# ---- what a finding says ----------------------------------------------------------

test_that("a format finding names the keyword, the format and the offending text", {
  details <- validate_table_detailed(
    rf_col(type = "R Date", nullable = FALSE),
    data.frame(X = c("2024-01-05", "2024-02-30")),
    verbose = FALSE
  )
  err <- details$columnspec_errors$full_error
  expect_identical(nrow(err), 1L)
  expect_identical(err$row, 2L)
  expect_identical(err$column, "X")
  expect_identical(err$keyword, "format")
  expect_identical(err$message, "must fit format \"%Y-%m-%d\"")
  expect_identical(err$columnspec, "%Y-%m-%d")
  expect_identical(err$data, "2024-02-30")
  expect_false(details$columnspec_valid)
})

test_that("the per-check summary counts the stored default format as declared", {
  clean <- validate_table_detailed(
    rf_col(type = "R Date", nullable = FALSE),
    data.frame(X = "2024-01-05"),
    verbose = FALSE
  )$columnspec_checks
  expect_equal(clean$status[clean$keyword == "format"], "passed")

  bad <- validate_table_detailed(
    rf_col(type = "R Date", nullable = FALSE),
    data.frame(X = "2024-1-5"),
    verbose = FALSE
  )$columnspec_checks
  expect_equal(bad$status[bad$keyword == "format"], "failed")
  expect_equal(bad$status[bad$keyword == "type"], "passed")

  # R character has no format, so nothing declares one.
  plain <- validate_table_detailed(
    rf_col(type = "R character", nullable = FALSE),
    data.frame(X = "a"),
    verbose = FALSE
  )$columnspec_checks
  expect_equal(plain$status[plain$keyword == "format"], "not_applicable")
})

test_that("only a temporal R type carries a format into the schema", {
  expect_identical(
    as_json_schema(DTAColumnSpec(id = "X", type = "R Date"))$format, "%Y-%m-%d"
  )
  expect_identical(
    as_json_schema(DTAColumnSpec(id = "X", type = "R POSIXct"))$format, "%Y-%m-%d %H:%M:%S"
  )
  expect_identical(
    as_json_schema(DTAColumnSpec(id = "X", type = "R hms", format = "R %H:%M"))$format, "%H:%M"
  )
  for (type in c("R character", "R numeric", "R integer", "R logical")) {
    expect_null(as_json_schema(DTAColumnSpec(id = "X", type = type))$format, info = type)
  }
})


# ---- one verdict from every engine ----------------------------------------------

test_that("an R Date column is judged alike in memory and over several batches", {
  dates <- c(
    "2024-01-05", "2024-02-30", "2024-02-29", "2023-02-29", NA,
    "2024-1-5", "2024-12-31", " 2024-01-05", "2024-01-05", "20240105"
  )
  specs <- rf_col(type = "R Date", nullable = TRUE)
  tab <- data.frame(X = dates, stringsAsFactors = FALSE)
  eager <- validate_table_detailed(specs, tab, verbose = FALSE)$columnspec_errors$full_error

  # By hand: 2024-02-30 (2), 2023-02-29 (4), 2024-1-5 (6), the padded value (8)
  # and the compact one (10) fail; NA is nullable.
  expect_identical(eager$row, c(2L, 4L, 6L, 8L, 10L))
  expect_true(all(eager$keyword == "format"))

  # Batches of 1, 3 and 4 put findings on a batch boundary (rows 3/4, 6/7, 8/9)
  # and in a batch of their own; 100 is one batch.
  for (batch_rows in c(1L, 3L, 4L, 100L)) {
    streamed <- dta_validate_table_stream(
      specs,
      dta_as_batch_reader(arrow::as_arrow_table(tab), batch_rows = batch_rows),
      verbose = FALSE, coerce = FALSE
    )$columnspec_errors$full_error
    expect_equal(streamed, eager, info = paste("batch_rows =", batch_rows))
  }
})

test_that("a file read in batches gives the in-memory verdict for an R Date column", {
  dates <- c(
    "2024-01-05", "2024-02-30", "2024-02-29", "2023-02-29", NA,
    "2024-1-5", "2024-12-31", "2024-01-05", "20240105"
  )
  specs <- rf_col(type = "R Date", nullable = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(data.frame(X = dates), path, row.names = FALSE)

  # By hand: rows 2, 4, 6 and 9 fail; the NA (row 5) is a missing value.
  streamed <- validate_file_stream(specs, path, batch_rows = 3L, verbose = FALSE)
  expect_false(streamed$columnspec_valid)
  expect_identical(streamed$columnspec_errors$full_error$row, c(2L, 4L, 6L, 9L))
  expect_true(all(streamed$columnspec_errors$full_error$keyword == "format"))
})

test_that("a delimited file is judged against the declared R format, not read back as dates", {
  # Every value is a well-formed ISO date or time, so a reader that inferred
  # dates from the text would hand typed vectors to a check that skips them and
  # pass columns the declared formats reject.
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(
    c(
      "D,T,H",
      "2024-01-05,2024-01-05 10:30:00,10:30:00",
      "2024-02-06,2024-01-05 11:30:00,11:30:00"
    ),
    path
  )
  specs <- DTAColumnSpecCollection(columns = list(
    D = DTAColumnSpec(id = "D", type = "R Date", format = "R %d/%m/%Y", nullable = FALSE),
    T = DTAColumnSpec(id = "T", type = "R POSIXct", format = "R %Y%m%d %H%M", nullable = FALSE),
    H = DTAColumnSpec(id = "H", type = "R hms", format = "R %H.%M", nullable = FALSE)
  ))

  ds <- DTADataSetTabular(
    id = "d", specs = specs, files = list(DTAFileCSV(filename = basename(path)))
  )
  ds <- load_file(ds, file = path, handler_index = 1, stream = "never")
  loaded <- as.data.frame(tables(ds)[[1]])
  expect_true(all(vapply(loaded, is.character, logical(1))))
  # Two rows, three columns, each value a format finding.
  expect_identical(
    validation_status(check(ds, quiet = TRUE, persist = FALSE))$n_columnspec_errors, 6L
  )

  streamed <- validate_file_stream(specs, path, batch_rows = 1L, verbose = FALSE)
  expect_identical(nrow(streamed$columnspec_errors$full_error), 6L)
  expect_true(all(streamed$columnspec_errors$full_error$keyword == "format"))
})
