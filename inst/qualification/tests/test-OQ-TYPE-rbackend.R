# The R type vocabulary. REQ-TYPE-017 .. REQ-TYPE-027, and REQ-TYPE-010 for the
# one typed-column defect on record (DEV-018).
#
# A column declared `R Date`, `R POSIXct` or `R hms` carries a format built from
# a closed set of fixed-width fields; a column declared `R logical` is read from
# eight spellings; `R numeric`, `R integer` and `R character` are typed as
# `SAS Num`, `SAS Int` and `SAS Char` are. Every expected value in this file is
# written down from the requirement text: the width and range of each field, the
# Gregorian calendar, the eight logical spellings, the type table. None of it
# was captured from a run. Where an independent oracle exists it is used as one:
# the calendar cases are also compared with a round trip through base R's
# as.Date() and format() over a seeded random sample.
#
# The ids continue the area's own sequence: OQ-TYPE-027 .. OQ-TYPE-029 are not
# in use, and OQ-TYPE-030 .. OQ-TYPE-035 are the strict numeric conversion
# cases of test-OQ-TYPE-numeric.R.

# ---- fixtures ---------------------------------------------------------------

# One column X of the declared type, in a collection of one.
rb_specs <- function(...) {
  DTAColumnSpecCollection(columns = list(X = DTAColumnSpec(id = "X", ...)))
}

# What validate_table() reports for a one-column table of `values`, as
# "<row> <keyword>" pairs. A table that is valid comes back as a data frame, so
# it reports nothing. The table is handed over as it is: nothing is read from a
# file, so no reader can trim or drop a value before the check sees it.
rb_found <- function(specs, values) {
  out <- validate_table(
    specs, data.frame(X = values, stringsAsFactors = FALSE),
    verbose = FALSE
  )
  if (is.data.frame(out)) {
    return(character(0))
  }
  paste(out$full_error$row, out$full_error$keyword)
}

# The "<row> format" pairs expected of a named logical vector whose names are
# the values and whose elements say whether the value fits.
rb_unfit <- function(fits) {
  rows <- which(!fits)
  if (length(rows) == 0L) character(0) else sprintf("%d format", rows)
}

# Check a table of values against a spec: `fits` is named by value, TRUE where
# the requirement says the value fits its format.
rb_expect_fit <- function(description, specs, fits) {
  qa_step(description, rb_unfit(fits), rb_found(specs, names(fits)))
}

# Writes `lines` as a CSV and returns the dataset loaded from it, held in
# memory ("never") or scanned in batches ("always"), unchecked.
rb_dataset <- function(lines, specs, dir, stream = "never") {
  path <- file.path(dir, paste0("rb", as.integer(stats::runif(1, 1, 1e9)), ".csv"))
  writeLines(lines, path)
  ds <- DTADataSetTabular(
    id = "rb", specs = specs,
    files = list(DTAFileCSV(filename = basename(path)))
  )
  load_file(ds, file = path, handler_index = 1, stream = stream)
}

# The findings of a checked dataset as source, row, column and keyword, in a
# fixed order; the message text is prose and is not compared.
rb_messages <- function(ds) {
  msgs <- as.data.frame(messages(ds))
  out <- data.frame(
    source = as.character(msgs$source),
    row = as.integer(msgs$row),
    column = as.character(msgs$column),
    keyword = as.character(msgs$keyword),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$source, out$row, out$column, out$keyword, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

rb_expect_messages <- function(source = character(0), row = integer(0),
                               column = character(0), keyword = character(0)) {
  out <- data.frame(
    source = as.character(source), row = as.integer(row),
    column = as.character(column), keyword = as.character(keyword),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$source, out$row, out$column, out$keyword, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

rb_axis <- function(ds) {
  st <- validation_status(ds)
  c(
    columnspec = as.integer(st$n_columnspec_errors),
    rule = as.integer(st$n_rule_errors),
    import = as.integer(st$n_import_errors)
  )
}

# Does building the column raise a condition?
rb_refused <- function(...) {
  inherits(tryCatch(
    {
      DTAColumnSpec(id = "X", ...)
      NULL
    },
    error = function(e) e
  ), "condition")
}

# The class of the condition building the column raises, or "" when it builds.
rb_refusal_class <- function(...) {
  e <- tryCatch(
    {
      DTAColumnSpec(id = "X", ...)
      NULL
    },
    error = function(e) e
  )
  if (is.null(e)) "" else class(e)[[1L]]
}

# ---- the type table ---------------------------------------------------------

test_that("OQ-TYPE-036 | the seven R types map to their JSON type, R storage type and Arrow type | REQ-TYPE-024", {
  types <- c("character", "numeric", "integer", "logical", "Date", "POSIXct", "hms")
  specs <- lapply(types, function(t) DTAColumnSpec(id = "X", type = paste("R", t)))

  qa_step(
    "as_json_schema_type(): text and the three date and time types are string",
    c("string", "number", "integer", "boolean", "string", "string", "string"),
    vapply(specs, function(s) as_json_schema_type(s@structure), character(1))
  )
  qa_step(
    "as_r_type(): the date and time types are stored as the text that was read",
    c("character", "double", "integer", "logical", "character", "character", "character"),
    vapply(specs, function(s) as_r_type(s@structure), character(1))
  )
  qa_step(
    "get_arrow_type(): the three date and time types have no Arrow type",
    c("utf8", "double", "int32", "bool", NA_character_, NA_character_, NA_character_),
    vapply(specs, get_arrow_type, character(1))
  )

  unset <- DTAColumnSpecStructureR()
  qa_check("a structure with no type carries none", is.null(unset@type))
  qa_step("as_r_type() defaults an unset R type to character", "character", as_r_type(unset))
})

# ---- values against a format --------------------------------------------------

test_that("OQ-TYPE-037 | an R Date value fits only as four, two and two digits in range, with nothing around it | REQ-TYPE-017", {
  specs <- rb_specs(type = "R Date", nullable = TRUE)

  # Each value is named, with what the requirement says: %Y four digits, %m 01
  # to 12, %d 01 to 31, the literal hyphens as written, nothing before or after.
  fits <- c(
    "2024-01-05" = TRUE,
    "0000-01-01" = TRUE,
    "9999-12-31" = TRUE,
    "2024-1-5" = FALSE,
    "2024-01-5" = FALSE,
    "24-01-05" = FALSE,
    "02024-01-05" = FALSE,
    "2024-13-01" = FALSE,
    "2024-00-10" = FALSE,
    "2024-12-00" = FALSE,
    "2024-12-32" = FALSE,
    "2024/01/05" = FALSE,
    "20240105" = FALSE,
    "2024-01-05T10:30" = FALSE,
    " 2024-01-05" = FALSE,
    "2024-01-05 " = FALSE,
    "2024-01-05\n" = FALSE,
    "\t2024-01-05" = FALSE,
    "+2024-01-05" = FALSE
  )
  rb_expect_fit("exactly the values that fill the fixed-width fields fit", specs, fits)
  # An empty text is no date text either. A delimited-text reader makes an empty
  # field a missing value before this check sees it (REQ-FILE-004); handed over
  # as it is, the text is checked.
  qa_step("an empty text does not fit", "1 format", rb_found(specs, ""))

  qa_step(
    "a missing value is not reported for its format in a nullable column",
    character(0), rb_found(specs, c("2024-01-05", NA))
  )
  qa_step(
    "in a column that is not nullable it is a type finding and not a format finding",
    "2 type",
    rb_found(rb_specs(type = "R Date", nullable = FALSE), c("2024-01-05", NA))
  )
  qa_step(
    "a number is no date text: one type finding per value, and no format finding",
    c("1 type", "2 type"),
    rb_found(rb_specs(type = "R Date", nullable = FALSE), c(20000, 20001))
  )
})

test_that("OQ-TYPE-038 | an R Date value must name a day of the Gregorian calendar, leap years included | REQ-TYPE-018", {
  specs <- rb_specs(type = "R Date", nullable = TRUE)

  fits <- c(
    "2024-02-29" = TRUE, # divisible by 4
    "2000-02-29" = TRUE, # divisible by 400
    "2400-02-29" = TRUE,
    "2096-02-29" = TRUE,
    "0000-02-29" = TRUE, # year 0000 is divisible by 400
    "2024-02-28" = TRUE,
    "2023-03-31" = TRUE,
    "2024-04-30" = TRUE,
    "2023-12-31" = TRUE,
    "2023-02-29" = FALSE,
    "1900-02-29" = FALSE, # divisible by 100, not by 400
    "2100-02-29" = FALSE,
    "0100-02-29" = FALSE,
    "2024-02-30" = FALSE,
    "2023-02-31" = FALSE,
    "2024-04-31" = FALSE,
    "2024-06-31" = FALSE,
    "2024-09-31" = FALSE,
    "2024-11-31" = FALSE
  )
  rb_expect_fit("the calendar decides which days exist", specs, fits)

  # The same rule applies when the date is part of a datetime.
  rb_expect_fit(
    "a datetime names a day that exists too",
    rb_specs(type = "R POSIXct", nullable = TRUE),
    c(
      "2024-02-29 10:00:00" = TRUE, "2023-02-29 10:00:00" = FALSE,
      "2024-04-31 00:00:00" = FALSE
    )
  )

  # An oracle that is not this package: a date text fits exactly when base R
  # reads it as a date and writes the same text back. Years below 1000 are left
  # out, because %Y is not zero-padded there on every platform; the hand-written
  # cases above cover them.
  dates <- withr::with_seed(qa_seed(), {
    n <- 400L
    year <- sample(1000:9999, 2L * n, replace = TRUE)
    month <- c(sample(1:12, n, replace = TRUE), rep(2L, n))
    day <- c(sample(1:31, n, replace = TRUE), sample(26:31, n, replace = TRUE))
    sprintf("%04d-%02d-%02d", year, month, day)
  })
  as_date <- suppressWarnings(as.Date(dates, format = "%Y-%m-%d"))
  fits_oracle <- !is.na(as_date) & format(as_date, "%Y-%m-%d") == dates
  qa_check(
    "the sample holds both outcomes, so agreeing is not agreeing on nothing",
    any(fits_oracle) && any(!fits_oracle)
  )
  qa_step(
    "the dates reported are the ones base R does not read as that same date, over 800 random dates",
    rb_unfit(stats::setNames(fits_oracle, dates)),
    rb_found(specs, dates)
  )
})

test_that("OQ-TYPE-039 | R POSIXct and R hms values fit fixed-width time fields, an hms is a time of day, and %OSn takes up to n fraction digits | REQ-TYPE-017 REQ-TYPE-019", {
  rb_expect_fit(
    "a datetime fits %Y-%m-%d %H:%M:%S, the default of R POSIXct",
    rb_specs(type = "R POSIXct", nullable = TRUE),
    c(
      "2024-01-05 10:30:00" = TRUE,
      "2024-01-05 00:00:00" = TRUE,
      "2024-01-05 23:59:59" = TRUE,
      "2024-01-05T10:30:00" = FALSE,
      "2024-01-05 10:30" = FALSE,
      "2024-01-05 24:00:00" = FALSE,
      "2024-01-05 10:60:00" = FALSE,
      "2024-01-05 10:30:60" = FALSE,
      "2024-01-05 10:30:00.5" = FALSE,
      "2024-01-05 1:30:00" = FALSE,
      "2024-01-05  10:30:00" = FALSE
    )
  )

  # LIM-030: an hms is a time of day, not a duration; LIM-029: no leap second.
  rb_expect_fit(
    "an hms is a time of day: hours 00 to 23, minutes and seconds 00 to 59",
    rb_specs(type = "R hms", nullable = TRUE),
    c(
      "00:00:00" = TRUE,
      "12:00:00" = TRUE,
      "23:59:59" = TRUE,
      "24:00:00" = FALSE,
      "100:00:00" = FALSE,
      "23:59:60" = FALSE,
      "12:60:00" = FALSE,
      "7:05:00" = FALSE,
      "12:00" = FALSE,
      "12:00:00.0" = FALSE
    )
  )

  rb_expect_fit(
    "%OS3 takes whole seconds or a fraction of one to three digits",
    rb_specs(type = "R hms", format = "R %H:%M:%OS3", nullable = TRUE),
    c(
      "10:30:00" = TRUE,
      "10:30:00.5" = TRUE,
      "10:30:00.12" = TRUE,
      "10:30:00.123" = TRUE,
      "10:30:00.1234" = FALSE,
      "10:30:00." = FALSE,
      "10:30:00,5" = FALSE,
      "10:30:60.0" = FALSE,
      "10:30:0.5" = FALSE
    )
  )
  rb_expect_fit(
    "%OS1 takes one digit of fraction and no more",
    rb_specs(type = "R hms", format = "R %H:%M:%OS1", nullable = TRUE),
    c("10:30:00" = TRUE, "10:30:00.9" = TRUE, "10:30:00.12" = FALSE)
  )
  rb_expect_fit(
    "%OS6 takes up to six digits",
    rb_specs(type = "R hms", format = "R %H:%M:%OS6", nullable = TRUE),
    c("10:30:00.123456" = TRUE, "10:30:00.1234567" = FALSE)
  )
})

test_that("OQ-TYPE-040 | a declared format replaces the default, its literals must be written as they are, and no value is trimmed or case-folded | REQ-TYPE-017", {
  rb_expect_fit(
    "a day-first date with slashes",
    rb_specs(type = "R Date", format = "R %d/%m/%Y", nullable = TRUE),
    c(
      "05/01/2024" = TRUE, "29/02/2024" = TRUE, "29/02/2023" = FALSE,
      "2024-01-05" = FALSE, "5/1/2024" = FALSE, "05.01.2024" = FALSE
    )
  )
  rb_expect_fit(
    "fields with no separator at all stay unambiguous by their widths",
    rb_specs(type = "R Date", format = "R %Y%m%d", nullable = TRUE),
    c("20240105" = TRUE, "20240230" = FALSE, "2024-01-05" = FALSE, "2024015" = FALSE)
  )
  rb_expect_fit(
    "a dot, an underscore, a comma and a semicolon stand for themselves",
    rb_specs(type = "R Date", format = "R %d.%m.%Y", nullable = TRUE),
    c("05.01.2024" = TRUE, "05/01/2024" = FALSE, "05x01x2024" = FALSE)
  )
  rb_expect_fit(
    "an underscore",
    rb_specs(type = "R Date", format = "R %Y_%m_%d", nullable = TRUE),
    c("2024_01_05" = TRUE, "2024-01-05" = FALSE)
  )

  # The letters T and Z are matched exactly, in capitals only.
  rb_expect_fit(
    "ISO 8601 with a literal T and Z",
    rb_specs(type = "R POSIXct", format = "R %Y-%m-%dT%H:%M:%SZ", nullable = TRUE),
    c(
      "2024-01-05T10:30:00Z" = TRUE,
      "2024-01-05t10:30:00Z" = FALSE,
      "2024-01-05T10:30:00z" = FALSE,
      "2024-01-05T10:30:00" = FALSE,
      "2024-01-05 10:30:00Z" = FALSE,
      "2024-01-05T10:30:00ZZ" = FALSE,
      "2024-01-05T24:00:00Z" = FALSE
    )
  )
  rb_expect_fit(
    "%F and %T stand for %Y-%m-%d and %H:%M:%S",
    rb_specs(type = "R POSIXct", format = "R %F %T", nullable = TRUE),
    c("2024-01-05 10:30:00" = TRUE, "2024-02-30 10:30:00" = FALSE, "2024-01-05 10:30" = FALSE)
  )
  rb_expect_fit(
    "an hour and a minute only",
    rb_specs(type = "R hms", format = "R %H:%M", nullable = TRUE),
    c("07:05" = TRUE, "23:59" = TRUE, "24:00" = FALSE, "07:05:00" = FALSE, "7:05" = FALSE)
  )
  rb_expect_fit(
    "a datetime without seconds",
    rb_specs(type = "R POSIXct", format = "R %Y-%m-%d %H:%M", nullable = TRUE),
    c("2024-01-05 10:30" = TRUE, "2024-01-05 10:30:00" = FALSE)
  )
  rb_expect_fit(
    "a colon followed by a space in a format is two literals, matched as written",
    rb_specs(type = "R hms", format = "R %H: %M", nullable = TRUE),
    c("10: 30" = TRUE, "10:30" = FALSE, "10:  30" = FALSE)
  )
})

test_that("OQ-TYPE-041 | a typed Date, POSIXct, difftime or hms vector is not matched against an R format, nor its class against the declared type | REQ-TYPE-017", {
  # LIM-027. A typed vector holds no text to fit, so none of these is a finding,
  # whatever the format says or the class is; the same value written as text is.
  day <- as.Date("2024-01-05")
  stamp <- as.POSIXct("2024-01-05 10:30:00", tz = "UTC")
  tod <- structure(c(52200, NA), class = c("hms", "difftime"), units = "secs")

  qa_step(
    "a Date column under a format it could never match",
    character(0),
    rb_found(rb_specs(type = "R Date", format = "R %d/%m/%Y", nullable = TRUE), c(day, NA))
  )
  qa_step(
    "a POSIXct column under a format it could never match",
    character(0),
    rb_found(rb_specs(type = "R POSIXct", format = "R %Y%m%d %H%M", nullable = TRUE), c(stamp, NA))
  )
  qa_step(
    "an hms column under a format it could never match",
    character(0),
    rb_found(rb_specs(type = "R hms", format = "R %H.%M", nullable = TRUE), tod)
  )
  qa_step(
    "a Date vector in a column declared R POSIXct is not a class finding",
    character(0),
    rb_found(rb_specs(type = "R POSIXct", nullable = TRUE), c(day, NA))
  )
  qa_step(
    "a POSIXct vector in a column declared R hms is not a class finding",
    character(0),
    rb_found(rb_specs(type = "R hms", nullable = TRUE), c(stamp, NA))
  )
  qa_step(
    "the same date written as text under the same format is a finding",
    "1 format",
    rb_found(rb_specs(type = "R Date", format = "R %d/%m/%Y", nullable = TRUE), "2024-01-05")
  )
})

test_that("OQ-TYPE-042 | a difftime held in R is reported against a SAS time format where the same time as text, a Date and a POSIXct are not | REQ-TYPE-010 | tags: white-box", {
  time8 <- rb_specs(type = "SAS Time", format = "SAS TIME8.", nullable = TRUE)
  plain <- structure(c(52200, NA), class = "difftime", units = "secs")
  hms <- structure(c(52200, NA), class = c("hms", "difftime"), units = "secs")

  # What the requirement describes: the equivalents of 14:30:00 all fit TIME8.
  qa_step("the text 14:30:00 fits TIME8.", character(0), rb_found(time8, c("14:30:00", NA)))
  qa_step("a time of day of class hms is the equivalent and fits", character(0), rb_found(time8, hms))
  qa_step(
    "a typed Date fits DATE9., as its text does",
    character(0),
    rb_found(rb_specs(type = "SAS Date", format = "SAS DATE9.", nullable = TRUE), c(as.Date("2024-01-05"), NA))
  )
  qa_step(
    "a typed POSIXct fits DATETIME20., as its text does",
    character(0),
    rb_found(
      rb_specs(type = "SAS DateTime", format = "SAS DATETIME20.", nullable = TRUE),
      c(as.POSIXct("2024-01-05 10:30:00", tz = "UTC"), NA)
    )
  )

  # DEV-018: the plain difftime is the one that is reported, as the text 52200.
  qa_known_deviation("DEV-018", identical(rb_found(time8, plain), "1 format"))
})

# ---- the formats that are refused ---------------------------------------------

test_that("OQ-TYPE-043 | a format outside the supported pieces, or not making the declared type, is refused when the specification is built | REQ-TYPE-020", {
  # Accepted: each supported directive and literal, with the type it describes.
  accepted <- list(
    c("R Date", "R %Y-%m-%d"), c("R Date", "R %F"), c("R Date", "R %d/%m/%Y"),
    c("R Date", "R %Y%m%d"), c("R Date", "R %d.%m.%Y"), c("R Date", "R %Y_%m_%d"),
    c("R Date", "R %d,%m,%Y"), c("R POSIXct", "R %Y-%m-%dT%H:%M:%SZ"),
    c("R POSIXct", "R %F %T"), c("R POSIXct", "R %Y-%m-%d %H:%M:%OS3"),
    c("R POSIXct", "R %Y-%m-%d %H:%M"), c("R hms", "R %H:%M"), c("R hms", "R %T"),
    c("R hms", "R %H:%M:%OS6"), c("R hms", "R %H;%M")
  )
  built <- vapply(
    accepted, function(a) !rb_refused(type = a[[1L]], format = a[[2L]]), logical(1)
  )
  qa_step(
    "every supported directive and literal builds",
    stats::setNames(rep(TRUE, length(accepted)), vapply(accepted, paste, character(1), collapse = " | ")),
    stats::setNames(built, vapply(accepted, paste, character(1), collapse = " | "))
  )

  # Refused: each reason the requirement lists, at least once.
  refused <- list(
    # a directive that is not supported
    c("R Date", "R %d %b %Y"), c("R Date", "R %d %B %Y"), c("R Date", "R %a %Y-%m-%d"),
    c("R Date", "R %A %Y-%m-%d"), c("R POSIXct", "R %Y-%m-%d %I:%M %p"),
    c("R Date", "R %y-%m-%d"), c("R Date", "R %Y-%j"), c("R POSIXct", "R %Y-%m-%dT%H:%M:%S%z"),
    c("R POSIXct", "R %Y-%m-%d %H:%M:%S %Z"), c("R Date", "R %e/%m/%Y"),
    c("R Date", "R %%Y-%m-%d"), c("R Date", "R %Y-%m-%d%"),
    c("R POSIXct", "R %Y-%m-%d %H:%M:%OS"), c("R POSIXct", "R %Y-%m-%d %H:%M:%OS0"),
    c("R POSIXct", "R %Y-%m-%d %H:%M:%OS7"),
    # any other character
    c("R Date", "R %Y-%m-%d 12"), c("R Date", "R %Y-%m-%d x"), c("R Date", "R %Y-%m-%d'"),
    c("R Date", "R %Y-%m-%d{x}"), c("R Date", "R %Y-%m-%d\\"), c("R Date", "R %Y-%m-%d\""),
    c("R Date", "R %Y-%m-%dt"),
    # a field written twice
    c("R Date", "R %F %Y"), c("R Date", "R %Y %Y-%m-%d"), c("R POSIXct", "R %F %T %H"),
    c("R hms", "R %T %OS3"), c("R hms", "R %H:%M:%S %S"),
    # a decimal point or a digit directly after %OSn
    c("R hms", "R %H:%M:%OS3."), c("R hms", "R %H:%M:%OS3.5"),
    # an empty or blank format
    c("R Date", "R "), c("R Date", "R    "),
    # fields that do not make the type
    c("R Date", "R %H:%M"), c("R Date", "R %Y-%m-%d %H:%M:%S"), c("R Date", "R %Y-%m"),
    c("R POSIXct", "R %Y-%m-%d"), c("R POSIXct", "R %Y-%m-%d %H"),
    c("R hms", "R %Y-%m-%d %H:%M"), c("R hms", "R %H")
  )
  was_refused <- vapply(
    refused, function(a) rb_refused(type = a[[1L]], format = a[[2L]]), logical(1)
  )
  labels <- vapply(refused, paste, character(1), collapse = " | ")
  qa_step(
    "each unsupported directive, character, repetition, blank format and mismatched field set is refused",
    stats::setNames(rep(TRUE, length(refused)), labels),
    stats::setNames(was_refused, labels)
  )

  # No declared type: the format names it.
  inferred <- vapply(
    c("R %Y-%m-%d", "R %Y-%m-%d %H:%M:%S", "R %H:%M"),
    function(f) DTAColumnSpec(id = "X", format = f)@structure@type,
    character(1),
    USE.NAMES = FALSE
  )
  qa_step(
    "with no type declared, the type is the one the format describes",
    c("Date", "POSIXct", "hms"), inferred
  )

  # LIM-029: nothing of the time zone, month name or AM/PM kind exists to ask for.
  # A format holding braces is plain text in the refusal, not a template.
  err <- tryCatch(
    DTAColumnSpec(id = "X", type = "R Date", format = "R %Y-%m-%d{x}"),
    error = function(e) e
  )
  qa_check(
    "a format with braces is refused as an ordinary error that quotes it unevaluated",
    inherits(err, "error") && grepl("{x}", conditionMessage(err), fixed = TRUE)
  )
})

# ---- logical values at import ---------------------------------------------------

test_that("OQ-TYPE-044 | an R logical column reads eight exact spellings, takes a blank as missing, and reports every other text once | REQ-TYPE-021", {
  dir <- qa_tempdir()
  specs <- DTAColumnSpecCollection(columns = list(
    ID = DTAColumnSpec(id = "ID", type = "R character", nullable = FALSE),
    FLAG = DTAColumnSpec(id = "FLAG", type = "R logical", nullable = TRUE)
  ))
  # Quoted fields keep the blanks a value is written with.
  lines <- c(
    "ID,FLAG",
    "a1,T", "a2,TRUE", "a3,true", "a4,True",
    "a5,F", "a6,FALSE", "a7,false", "a8,False",
    "a9,yes", "b1,1", "b2,0", "b3,tRuE", "b4,\" TRUE\"", "b5,\"T \"",
    "b6,\"\"", "b7,\"   \"", "b8,TRUE"
  )
  expected_values <- c(
    TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
    NA, NA, NA, NA, NA, NA, NA, NA, TRUE
  )
  # Rows 9 to 14 are text that is no logical spelling; 15 and 16 are blank.
  expected_messages <- rb_expect_messages(
    source = rep("import", 6L), row = 9:14, column = "FLAG", keyword = "not_convertible"
  )

  for (stream in c("never", "always")) {
    ds <- check(rb_dataset(lines, specs, dir, stream), persist = FALSE, quiet = TRUE)
    qa_step(
      sprintf("the six texts that are not a spelling are import findings, stream = %s", stream),
      expected_messages, rb_messages(ds)
    )
    qa_step(
      sprintf("and nothing else is reported on any axis, stream = %s", stream),
      c(columnspec = 0L, rule = 0L, import = 6L), rb_axis(ds)
    )
  }

  ds <- rb_dataset(lines, specs, dir, "never")
  col <- as.data.frame(tables(ds)[[1]])$FLAG
  qa_step("the column is stored as R logical", "logical", class(col))
  qa_step("each spelling is its value and every other text is missing", expected_values, col)
})

test_that("OQ-TYPE-045 | the text NA is missing in a delimited file and not a logical spelling in a SAS dataset | REQ-TYPE-022", {
  # LIM-028. A delimited-text reader removes NA before the typing sees it.
  dir <- qa_tempdir()
  csv_specs <- DTAColumnSpecCollection(columns = list(
    ID = DTAColumnSpec(id = "ID", type = "R character", nullable = FALSE),
    FLAG = DTAColumnSpec(id = "FLAG", type = "R logical", nullable = TRUE)
  ))
  ds <- check(
    rb_dataset(c("ID,FLAG", "a,T", "b,NA", "c,F"), csv_specs, dir),
    persist = FALSE, quiet = TRUE
  )
  qa_step(
    "a CSV field NA is missing: no finding on any axis",
    c(columnspec = 0L, rule = 0L, import = 0L), rb_axis(ds)
  )
  qa_step(
    "...and the column reads TRUE, missing, FALSE",
    c(TRUE, NA, FALSE), as.data.frame(tables(ds)[[1]])$FLAG
  )

  # delivery.sas7bdat holds, in SUBJID, the six values 007, "  lead", trail, a
  # missing one, NA and Muller; none is a logical spelling, and the fourth is
  # missing (REQ-FILE-036). Every other variable is declared so that it reads as
  # text or as a number that fits.
  sas_specs <- DTAColumnSpecCollection(columns = list(
    SUBJID = DTAColumnSpec(id = "SUBJID", type = "R logical", nullable = TRUE),
    AGE = DTAColumnSpec(id = "AGE", type = "R numeric", nullable = TRUE),
    VISDT = DTAColumnSpec(id = "VISDT", type = "R Date", nullable = TRUE),
    VISDTM = DTAColumnSpec(id = "VISDTM", type = "R POSIXct", nullable = TRUE),
    VISTM = DTAColumnSpec(id = "VISTM", type = "R hms", nullable = TRUE)
  ))
  path <- file.path(qa_suite_root(), "fx", "sas", "delivery.sas7bdat")
  sas <- DTADataSetTabular(
    id = "sas", specs = sas_specs,
    files = list(DTAFileSAS7BDAT(filename = basename(path)))
  )
  sas <- check(
    load_file(sas, file = path, handler_index = 1, stream = "never"),
    persist = FALSE, quiet = TRUE
  )
  qa_step(
    "read from a SAS dataset, NA and the other four texts are import findings, the missing value is not",
    rb_expect_messages(
      source = rep("import", 5L), row = c(1L, 2L, 3L, 5L, 6L),
      column = "SUBJID", keyword = "not_convertible"
    ),
    rb_messages(sas)
  )
})

# ---- numbers and text as their SAS counterparts -------------------------------

test_that("OQ-TYPE-046 | R numeric, R integer and R character are typed as SAS Num, SAS Int and SAS Char are | REQ-TYPE-023", {
  dir <- qa_tempdir()
  lines <- c(
    "N,I,C",
    "007,007,007",
    "1e3,12,x",
    "0x1F,1.5,y",
    "abc,2147483648,z",
    ",,q",
    "2.5,abc,w"
  )
  specs_for <- function(num, int, chr) {
    DTAColumnSpecCollection(columns = list(
      N = DTAColumnSpec(id = "N", type = num, nullable = TRUE),
      I = DTAColumnSpec(id = "I", type = int, nullable = TRUE),
      C = DTAColumnSpec(id = "C", type = chr, nullable = TRUE)
    ))
  }

  expected_numbers <- c(7, 1000, 31, NA, NA, 2.5)
  expected_integers <- c(7L, 12L, NA, NA, NA, NA)
  expected_text <- c("007", "x", "y", "z", "q", "w")
  # Rows are those of the data lines: the numeric column fails at row 4, the
  # integer column at rows 3 (a fraction), 4 (out of range) and 6 (text).
  expected_messages <- rb_expect_messages(
    source = rep("import", 4L), row = c(4L, 3L, 4L, 6L),
    column = c("N", "I", "I", "I"),
    keyword = c("not_convertible", "not_whole", "out_of_range", "not_convertible")
  )

  vocabularies <- list(
    R = c("R numeric", "R integer", "R character"),
    SAS = c("SAS Num", "SAS Int", "SAS Char")
  )
  for (label in names(vocabularies)) {
    vocab <- vocabularies[[label]]
    specs <- specs_for(vocab[[1]], vocab[[2]], vocab[[3]])
    ds <- check(rb_dataset(lines, specs, dir), persist = FALSE, quiet = TRUE)
    tab <- as.data.frame(tables(ds)[[1]])
    qa_step(
      paste(label, "numeric column: 007, 1e3 and 0x1F are 7, 1000 and 31"),
      expected_numbers, tab$N
    )
    qa_step(paste(label, "numeric column is stored as double"), "numeric", class(tab$N))
    qa_step(paste(label, "integer column: 007 and 12 are read, the rest is missing"), expected_integers, tab$I)
    qa_step(paste(label, "integer column is stored as integer"), "integer", class(tab$I))
    qa_step(paste(label, "text column keeps 007 and every other text"), expected_text, tab$C)
    qa_step(
      paste(label, "findings: abc, 1.5, 2147483648 and abc again, and no finding for the empty fields"),
      expected_messages, rb_messages(ds)
    )
  }
})

test_that("OQ-TYPE-047 | an R logical specification takes logical spellings as text, refuses the rest by class, and a one-spelling list reports the other value | REQ-TYPE-025", {
  qa_check(
    "a codelist of both spellings builds",
    !rb_refused(type = "R logical", values = c("TRUE", "FALSE"))
  )
  qa_check("an example that is a spelling builds", !rb_refused(type = "R logical", examples = "T"))
  qa_step(
    "an example that is not a spelling (yes) is a value mismatch",
    "dta_spec_value_mismatch", rb_refusal_class(type = "R logical", examples = "yes")
  )
  qa_step(
    "so is a codelist with a text that is not a spelling",
    "dta_spec_value_mismatch", rb_refusal_class(type = "R logical", values = c("TRUE", "maybe"))
  )
  qa_step(
    "so is the number 1 written as text",
    "dta_spec_value_mismatch", rb_refusal_class(type = "R logical", examples = "1")
  )
  qa_step(
    "an unquoted YAML true and false are logical values, not text",
    "dta_spec_logical_values", rb_refusal_class(type = "R logical", values = c(TRUE, FALSE))
  )
  qa_step(
    "a pattern is refused",
    "dta_spec_logical_pattern", rb_refusal_class(type = "R logical", pattern = "^T")
  )

  one <- rb_specs(type = "R logical", values = "TRUE", nullable = FALSE)
  qa_step(
    "a list of exactly one spelling reports the other value with the keyword const",
    "2 const", rb_found(one, c(TRUE, FALSE))
  )
  qa_step("...and not the value it lists", character(0), rb_found(one, c(TRUE, TRUE)))
})

# ---- the round trip through a file ----------------------------------------------

test_that("OQ-TYPE-048 | the type and format of every R column survive a YAML round trip, the stored default format and a colon-space format included | REQ-TYPE-026", {
  col <- function(id, ...) DTAColumnSpec(id = id, ...)
  specs <- DTAColumnSpecCollection(columns = list(
    C = col("C", type = "R character", length = 8, nullable = TRUE),
    N = col("N", type = "R numeric", nullable = TRUE),
    I = col("I", type = "R integer", nullable = TRUE),
    L = col("L", type = "R logical", values = c("TRUE", "FALSE"), nullable = FALSE),
    D = col("D", type = "R Date", nullable = TRUE),
    P = col("P", type = "R POSIXct", format = "R %Y-%m-%dT%H:%M:%SZ", nullable = TRUE),
    H = col("H", type = "R hms", format = "R %H: %M", nullable = TRUE)
  ))
  path <- file.path(qa_tempdir(), "r_columns.yaml")
  write_columns_to_yaml(specs, path)

  written <- qa_read_yaml(path)$columns
  qa_step(
    "the file names every type with its R prefix",
    c(
      C = "R character", N = "R numeric", I = "R integer", L = "R logical",
      D = "R Date", P = "R POSIXct", H = "R hms"
    ),
    vapply(written, function(w) w$type, character(1))
  )
  qa_step(
    "...and every format with its R prefix, the default of the Date included",
    c(D = "R %Y-%m-%d", P = "R %Y-%m-%dT%H:%M:%SZ", H = "R %H: %M"),
    vapply(written[c("D", "P", "H")], function(w) w$format, character(1))
  )

  back <- import_specs_from_yaml(path)
  structure_of <- function(id) as.list(back@columns[[id]]@structure)
  qa_step("R character keeps its type and length", list(type = "R character", length = 8), structure_of("C"), tolerance = 0)
  qa_step("R numeric", list(type = "R numeric"), structure_of("N"))
  qa_step("R integer", list(type = "R integer"), structure_of("I"))
  qa_step("R logical", list(type = "R logical"), structure_of("L"))
  qa_step("R Date comes back with its stored default format", list(type = "R Date", format = "R %Y-%m-%d"), structure_of("D"))
  qa_step(
    "R POSIXct keeps its declared format",
    list(type = "R POSIXct", format = "R %Y-%m-%dT%H:%M:%SZ"), structure_of("P")
  )
  qa_step(
    "R hms keeps a format that holds a colon followed by a space",
    list(type = "R hms", format = "R %H: %M"), structure_of("H")
  )
  qa_step(
    "the codelist of the logical column comes back as the same text",
    c("TRUE", "FALSE"), back@columns[["L"]]@values
  )
})

# ---- one answer from both engines -----------------------------------------------

test_that("OQ-TYPE-049 | a declared R logical, R numeric and R integer column is typed alike whole and in batches | REQ-TYPE-027 | tags: white-box", {
  # dta_coerce_table_to_specs() types a column once over the whole table for the
  # whole-column engine and once per batch for the streaming scan. Each cell is
  # judged alone, so where a batch boundary falls cannot change the result.
  specs <- DTAColumnSpecCollection(columns = list(
    L = DTAColumnSpec(id = "L", type = "R logical", nullable = TRUE),
    N = DTAColumnSpec(id = "N", type = "R numeric", nullable = TRUE),
    I = DTAColumnSpec(id = "I", type = "R integer", nullable = TRUE)
  ))
  rows <- data.frame(
    L = c("T", "TRUE", "F", "false", "True", "yes", "F", "True", "FALSE", "F"),
    N = c("1", "2", "3", "4", "5", "6", "7", "abc", "9", "10"),
    I = c("1", "2", "3", "4", "5", "6", "7", "8.5", "9", "10"),
    stringsAsFactors = FALSE
  )
  batches <- list(rows[1:5, ], rows[6:10, ])

  whole <- dta_coerce_table_to_specs(rows, specs)
  parts <- lapply(batches, dta_coerce_table_to_specs, specs = specs)
  storage <- function(r) vapply(as.data.frame(r$table)[c("L", "N", "I")], function(x) class(x)[[1L]], character(1))

  qa_step(
    "the whole table: logical, numeric and integer",
    c(L = "logical", N = "numeric", I = "integer"), storage(whole)
  )
  qa_step("the first batch types them the same", c(L = "logical", N = "numeric", I = "integer"), storage(parts[[1]]))
  qa_step("the second batch types them the same", c(L = "logical", N = "numeric", I = "integer"), storage(parts[[2]]))
  qa_step(
    "the whole table reports the three unreadable cells: yes, abc and 8.5",
    3L, nrow(whole$issues)
  )
  qa_step(
    "the batches report no finding in the first and the same three in the second",
    c(0L, 3L), vapply(parts, function(r) nrow(r$issues), integer(1))
  )
  qa_step(
    "the logical values of the batches are those of the whole table",
    c(TRUE, TRUE, FALSE, FALSE, TRUE, NA, FALSE, TRUE, FALSE, FALSE),
    unlist(lapply(parts, function(r) as.data.frame(r$table)$L), use.names = FALSE)
  )
})

test_that("OQ-TYPE-050 | a file with R logical, R integer and R Date columns gets the same findings held in memory and scanned in batches of any size | REQ-TYPE-027", {
  dir <- qa_tempdir()
  specs <- DTAColumnSpecCollection(columns = list(
    ID = DTAColumnSpec(id = "ID", type = "R character", nullable = FALSE),
    FLAG = DTAColumnSpec(id = "FLAG", type = "R logical", nullable = TRUE),
    N = DTAColumnSpec(id = "N", type = "R integer", nullable = TRUE),
    D = DTAColumnSpec(id = "D", type = "R Date", nullable = TRUE)
  ))
  lines <- c(
    "ID,FLAG,N,D",
    "a,T,1,2024-01-05",
    "b,yes,2,2024-02-30",
    "c,,x,2024-1-5",
    "d,false,2147483648,2024-02-29",
    "e,True,5,",
    "f,F,6.5,2023-02-29"
  )
  # By hand, from the data lines. Import: FLAG yes (row 2); N x (3), 2147483648
  # (4) and 6.5 (6). Column specification: the dates 2024-02-30 (2), 2024-1-5
  # (3) and 2023-02-29 (6); the empty date in row 5 is missing, and a nullable
  # column does not report missing.
  expected <- rb_expect_messages(
    source = c("import", "import", "import", "import", "columnspec", "columnspec", "columnspec"),
    row = c(2L, 3L, 4L, 6L, 2L, 3L, 6L),
    column = c("FLAG", "N", "N", "N", "D", "D", "D"),
    keyword = c(
      "not_convertible", "not_convertible", "out_of_range", "not_whole",
      "format", "format", "format"
    )
  )

  memory <- check(rb_dataset(lines, specs, dir, "never"), persist = FALSE, quiet = TRUE)
  qa_step("held in memory, the findings are those worked out by hand", expected, rb_messages(memory))
  qa_step(
    "...and the three axis counts follow from them",
    c(columnspec = 3L, rule = 0L, import = 4L), rb_axis(memory)
  )

  for (rows in c(1L, 2L, 4L, 100L)) {
    scanned <- check(
      rb_dataset(lines, specs, dir, "always"),
      persist = FALSE, quiet = TRUE, batch_rows = rows
    )
    qa_step(
      sprintf("scanned in batches of %d rows, the same findings", rows),
      expected, rb_messages(scanned)
    )
    qa_step(
      sprintf("...and the same axis counts, batches of %d rows", rows),
      c(columnspec = 3L, rule = 0L, import = 4L), rb_axis(scanned)
    )
  }
})
