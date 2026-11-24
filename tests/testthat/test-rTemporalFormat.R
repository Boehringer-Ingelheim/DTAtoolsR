# The R backend's date/time formats: one tokeniser, a regex plus an arithmetic
# calendar check, and a renderer driven by the same tokens. Every expected value
# below is written out by hand from the grammar in R/rTemporalFormat.R; none is
# copied from what the code returned.

parse_r_format <- function(format) dta_r_format_parse(format)

# ---- the tokeniser: what it accepts -------------------------------------------

test_that("the tokeniser accepts each supported format and names its family", {
  accepted <- list(
    list(format = "%Y-%m-%d", family = "Date"),
    list(format = "%F", family = "Date"),
    list(format = "%d/%m/%Y", family = "Date"),
    list(format = "%Y%m%d", family = "Date"),
    list(format = "%d.%m.%Y", family = "Date"),
    list(format = "%Y-%m-%d %H:%M:%S", family = "POSIXct"),
    list(format = "%F %T", family = "POSIXct"),
    list(format = "%Y-%m-%dT%H:%M:%SZ", family = "POSIXct"),
    list(format = "%Y-%m-%d %H:%M", family = "POSIXct"),
    list(format = "%Y-%m-%d %H:%M:%OS3", family = "POSIXct"),
    list(format = "%H:%M:%S", family = "hms"),
    list(format = "%T", family = "hms"),
    list(format = "%H:%M", family = "hms"),
    list(format = "%H:%M:%OS6", family = "hms"),
    list(format = "%H;%M_%S", family = "hms")
  )
  for (case in accepted) {
    parsed <- parse_r_format(case$format)
    expect_true(parsed$ok, info = case$format)
    expect_null(parsed$problem, info = case$format)
    expect_identical(parsed$family, case$family, info = case$format)
  }
})

test_that("%F and %T are expanded inside the tokeniser and count toward the fields", {
  parsed <- parse_r_format("%F %T")
  kinds <- vapply(parsed$tokens, function(t) t$kind, character(1))
  expect_identical(
    kinds,
    c("Y", "lit", "m", "lit", "d", "lit", "H", "lit", "M", "lit", "S")
  )
  expect_identical(parsed$fields, c("Y", "m", "d", "H", "M", "S"))
  expect_identical(parse_r_format("%T")$fields, c("H", "M", "S"))
})

test_that("%OSn records its digit count as frac and counts as the seconds field", {
  parsed <- parse_r_format("%H:%M:%OS4")
  expect_identical(parsed$frac, 4L)
  expect_identical(parsed$fields, c("H", "M", "S"))
  expect_identical(parse_r_format("%H:%M:%S")$frac, NA_integer_)
  expect_identical(parse_r_format("%H:%M:%OS1")$frac, 1L)
  expect_identical(parse_r_format("%H:%M:%OS6")$frac, 6L)
})

test_that("a literal stands for itself and a surrounding space is kept", {
  parsed <- parse_r_format(" %H:%M ")
  expect_true(parsed$ok)
  texts <- vapply(parsed$tokens, function(t) t$text, character(1))
  expect_identical(texts[c(1, 5)], c(" ", " "))
})

# ---- the tokeniser: what it refuses -------------------------------------------

test_that("the tokeniser refuses each unsupported or malformed format with a plain message", {
  refused <- list(
    # directives outside the whitelist
    list(format = "%b", regexp = "directive '%b' is not supported"),
    list(format = "%d %B %Y", regexp = "directive '%B' is not supported"),
    list(format = "%a", regexp = "directive '%a' is not supported"),
    list(format = "%A", regexp = "directive '%A' is not supported"),
    list(format = "%p", regexp = "directive '%p' is not supported"),
    list(format = "%I", regexp = "directive '%I' is not supported"),
    list(format = "%y-%m-%d", regexp = "directive '%y' is not supported"),
    list(format = "%j", regexp = "directive '%j' is not supported"),
    list(format = "%z", regexp = "directive '%z' is not supported"),
    list(format = "%Z", regexp = "directive '%Z' is not supported"),
    list(format = "%e", regexp = "directive '%e' is not supported"),
    list(format = "%Y%%", regexp = "directive '%%' is not supported"),
    # %OS needs exactly one digit from 1 to 6
    list(format = "%H:%M:%OS", regexp = "not supported on its own"),
    list(format = "%H:%M:%OS0", regexp = "not supported on its own"),
    list(format = "%H:%M:%OS7", regexp = "not supported on its own"),
    list(format = "%H:%M:%O", regexp = "not supported on its own"),
    # nothing may follow %OSn that would read as part of the fraction
    list(format = "%H:%M:%OS35", regexp = "cannot directly follow %OSn"),
    list(format = "%H:%M:%OS3.", regexp = "cannot directly follow %OSn"),
    # a lone trailing percent sign
    list(format = "%Y-%m-%d%", regexp = "ends with a lone '%'"),
    # literals outside the whitelist
    list(format = "%Y-%m-%d5", regexp = "character '5' is not allowed"),
    list(format = "%Y{x}", regexp = "character '\\{' is not allowed"),
    list(format = "%Y-%m-%d'", regexp = "character ''' is not allowed"),
    list(format = "%Y-%m-%d\"", regexp = "character '\"' is not allowed"),
    list(format = "%Y\\%m", regexp = "character '\\\\' is not allowed"),
    list(format = "%Y-%m-%d a", regexp = "character 'a' is not allowed"),
    list(format = "%Y-%m-%d t", regexp = "character 't' is not allowed"),
    list(format = "%Y-%m-%dé", regexp = "is not allowed"),
    # a repeated directive, also through %F and %T
    list(format = "%Y-%Y-%m-%d", regexp = "'%Y' appears more than once"),
    list(format = "%d-%d-%Y", regexp = "'%d' appears more than once"),
    list(format = "%H:%H", regexp = "'%H' appears more than once"),
    list(format = "%M:%M", regexp = "'%M' appears more than once"),
    list(format = "%F %Y", regexp = "'%Y' appears more than once"),
    list(format = "%F %m", regexp = "'%m' appears more than once"),
    list(format = "%T %H", regexp = "'%H' appears more than once"),
    # seconds twice, in any combination
    list(format = "%H:%M:%S:%S", regexp = "Seconds appear more than once"),
    list(format = "%T %OS3", regexp = "Seconds appear more than once"),
    list(format = "%H:%M:%OS1%OS2", regexp = "Seconds appear more than once"),
    # a format that is not a family
    list(format = "%Y", regexp = "does not describe a Date, POSIXct or hms"),
    list(format = "%Y-%m", regexp = "does not describe a Date, POSIXct or hms"),
    list(format = "%Y-%m-%d %H", regexp = "does not describe a Date, POSIXct or hms"),
    list(format = "%Y-%m-%d %M:%S", regexp = "does not describe a Date, POSIXct or hms"),
    list(format = "%m/%d %H:%M", regexp = "does not describe a Date, POSIXct or hms"),
    list(format = "%S", regexp = "does not describe a Date, POSIXct or hms"),
    list(format = "---", regexp = "does not describe a Date, POSIXct or hms")
  )
  for (case in refused) {
    parsed <- parse_r_format(case$format)
    expect_false(parsed$ok, info = case$format)
    expect_match(parsed$problem, case$regexp, info = case$format)
    expect_true(is.na(parsed$family), info = case$format)
  }
})

test_that("the tokeniser refuses a format that is not one non-empty piece of text", {
  for (bad_format in list("", NA_character_, NULL, 5, c("%Y", "%m"), character(0), TRUE)) {
    parsed <- parse_r_format(bad_format)
    expect_false(parsed$ok)
    expect_match(parsed$problem, "one non-empty piece of text")
  }
})

test_that("a refusal quotes the offending text as a value, never as a template", {
  parsed <- parse_r_format("%Y-{x}")
  expect_match(parsed$problem, "'{'", fixed = TRUE)
  # The message is a plain string: a brace is not evaluated and a percent sign
  # in the offending directive is not a conversion.
  expect_match(parse_r_format("%Y %s")$problem, "'%s'", fixed = TRUE)
  expect_type(parsed$problem, "character")
  expect_length(parsed$problem, 1L)
})

test_that("dta_r_format_problem reports a family that does not fit the declared type", {
  expect_null(dta_r_format_problem("%Y-%m-%d", "Date"))
  expect_null(dta_r_format_problem("%Y-%m-%d"))
  expect_match(dta_r_format_problem("%H:%M", "Date"), "describes a hms")
  expect_match(dta_r_format_problem("%Y-%m-%d", "POSIXct"), "describes a Date")
  expect_match(dta_r_format_problem("%b", "Date"), "not supported")
})

test_that("each type has its default format", {
  expect_identical(dta_r_default_format("Date"), "%Y-%m-%d")
  expect_identical(dta_r_default_format("POSIXct"), "%Y-%m-%d %H:%M:%S")
  expect_identical(dta_r_default_format("hms"), "%H:%M:%S")
  expect_null(dta_r_default_format("numeric"))
  expect_null(dta_r_default_format(NULL))
})

test_that("every default format is itself a valid format of its own family", {
  for (type in c("Date", "POSIXct", "hms")) {
    parsed <- parse_r_format(dta_r_default_format(type))
    expect_true(parsed$ok, info = type)
    expect_identical(parsed$family, type, info = type)
  }
})

# ---- the regex builder --------------------------------------------------------

test_that("capture groups are recorded for year, month and day in format order only", {
  expect_identical(
    dta_r_temporal_regex(parse_r_format("%Y-%m-%d"))$groups,
    c(Y = 1L, m = 2L, d = 3L)
  )
  expect_identical(
    dta_r_temporal_regex(parse_r_format("%d/%m/%Y %H:%M:%S"))$groups,
    c(d = 1L, m = 2L, Y = 3L)
  )
  expect_length(dta_r_temporal_regex(parse_r_format("%H:%M:%OS3"))$groups, 0L)
})

test_that("the regex is anchored at both ends and a literal dot is not a wildcard", {
  built <- dta_r_temporal_regex(parse_r_format("%d.%m.%Y"))
  expect_true(startsWith(built$regex, "^"))
  expect_true(grepl(built$regex, "05.01.2024", perl = TRUE))
  expect_false(grepl(built$regex, "05x01x2024", perl = TRUE))
  expect_false(grepl(built$regex, "05.01.2024 ", perl = TRUE))
  expect_false(grepl(built$regex, "x05.01.2024", perl = TRUE))
})

test_that("a trailing line break does not slip past the end anchor", {
  built <- dta_r_temporal_regex(parse_r_format("%Y-%m-%d"))
  expect_true(grepl(built$regex, "2024-01-05", perl = TRUE))
  expect_false(grepl(built$regex, "2024-01-05\n", perl = TRUE))
})

# ---- the checker: one directive at a time ------------------------------------

test_that("%Y takes exactly four digits, 0000 to 9999", {
  expect_identical(
    dta_r_temporal_bad(c("0000-01-01", "0001-01-01", "1999-12-31", "9999-12-31"), "%Y-%m-%d"),
    integer(0)
  )
  expect_identical(
    dta_r_temporal_bad(c("999-01-01", "10000-01-01", "20a4-01-01", "-024-01-01"), "%Y-%m-%d"),
    1:4
  )
})

test_that("%m takes 01 to 12", {
  good <- sprintf("2024-%02d-15", 1:12)
  expect_identical(dta_r_temporal_bad(good, "%Y-%m-%d"), integer(0))
  expect_identical(
    dta_r_temporal_bad(c("2024-00-15", "2024-13-15", "2024-1-15", "2024-0-15"), "%Y-%m-%d"),
    1:4
  )
})

test_that("%d takes 01 to 31", {
  good <- sprintf("2024-01-%02d", 1:31)
  expect_identical(dta_r_temporal_bad(good, "%Y-%m-%d"), integer(0))
  expect_identical(
    dta_r_temporal_bad(c("2024-01-00", "2024-01-32", "2024-01-5", "2024-01-99"), "%Y-%m-%d"),
    1:4
  )
})

test_that("%H takes 00 to 23 and %M takes 00 to 59", {
  expect_identical(dta_r_temporal_bad(c("00:00", "12:30", "23:59"), "%H:%M"), integer(0))
  expect_identical(dta_r_temporal_bad(c("24:00", "99:00", "7:30", "12:60", "12:5"), "%H:%M"), 1:5)
})

test_that("%S takes 00 to 59: there are no leap seconds", {
  expect_identical(dta_r_temporal_bad(c("12:00:00", "12:00:59"), "%H:%M:%S"), integer(0))
  expect_identical(dta_r_temporal_bad(c("12:00:60", "12:00:5", "12:00:61"), "%H:%M:%S"), 1:3)
})

test_that("%OS3 takes the seconds and an optional fraction of one to three digits", {
  fmt <- "%H:%M:%OS3"
  expect_identical(
    dta_r_temporal_bad(c("12:00:05", "12:00:05.1", "12:00:05.12", "12:00:05.123"), fmt),
    integer(0)
  )
  expect_identical(
    dta_r_temporal_bad(c("12:00:05.1234", "12:00:5.1", "12:00:05.", "12:00:60.1", "12:00:05,1"), fmt),
    1:5
  )
})

test_that("%OS1 and %OS6 bound the fraction at their own digit count", {
  expect_identical(dta_r_temporal_bad(c("12:00:05", "12:00:05.1"), "%H:%M:%OS1"), integer(0))
  expect_identical(dta_r_temporal_bad("12:00:05.12", "%H:%M:%OS1"), 1L)
  expect_identical(dta_r_temporal_bad("12:00:05.123456", "%H:%M:%OS6"), integer(0))
  expect_identical(dta_r_temporal_bad("12:00:05.1234567", "%H:%M:%OS6"), 1L)
})

test_that("a literal must match exactly and case-sensitively", {
  fmt <- "%Y-%m-%dT%H:%M:%SZ"
  expect_identical(dta_r_temporal_bad("2024-02-29T23:59:59Z", fmt), integer(0))
  expect_identical(
    dta_r_temporal_bad(
      c(
        "2024-02-29 23:59:59Z", "2024-02-29T23:59:59", "2024-02-29t23:59:59Z",
        "2024-02-29T23:59:59z", "2024-02-29T24:00:00Z", "2024-02-30T23:59:59Z"
      ),
      fmt
    ),
    1:6
  )
})

# ---- the checker: the calendar ------------------------------------------------

test_that("the Gregorian leap rule decides 29 February", {
  ok <- c("2024-02-29", "2000-02-29", "0000-02-29", "1600-02-29", "2400-02-29")
  bad <- c("2023-02-29", "1900-02-29", "0100-02-29", "2100-02-29", "1700-02-29")
  expect_identical(dta_r_temporal_bad(ok, "%Y-%m-%d"), integer(0))
  expect_identical(dta_r_temporal_bad(bad, "%Y-%m-%d"), 1:5)
})

test_that("a day past the end of its month is bad in every month", {
  expect_identical(
    dta_r_temporal_bad(
      c(
        "2024-01-31", "2024-03-31", "2024-05-31", "2024-07-31", "2024-08-31",
        "2024-10-31", "2024-12-31", "2024-04-30", "2024-06-30", "2024-09-30",
        "2024-11-30", "2023-02-28", "2024-02-29"
      ),
      "%Y-%m-%d"
    ),
    integer(0)
  )
  expect_identical(
    dta_r_temporal_bad(
      c(
        "2024-04-31", "2024-06-31", "2024-09-31", "2024-11-31", "2024-02-30",
        "2023-02-30", "2024-02-31"
      ),
      "%Y-%m-%d"
    ),
    1:7
  )
})

test_that("the calendar check follows the format's field order", {
  expect_identical(dta_r_temporal_bad(c("29/02/2024", "31/12/2023"), "%d/%m/%Y"), integer(0))
  expect_identical(dta_r_temporal_bad(c("29/02/2023", "31/04/2024", "00/01/2024"), "%d/%m/%Y"), 1:3)
})

test_that("a date-time is checked for the calendar too", {
  fmt <- "%Y-%m-%d %H:%M:%S"
  expect_identical(dta_r_temporal_bad("2024-02-29 23:59:59", fmt), integer(0))
  expect_identical(dta_r_temporal_bad("2023-02-29 23:59:59", fmt), 1L)
})

test_that("%Y%m%d is unambiguous because every field has a fixed width", {
  expect_identical(dta_r_temporal_bad(c("20240229", "19000101", "00000229"), "%Y%m%d"), integer(0))
  expect_identical(
    dta_r_temporal_bad(c("20230229", "20241301", "2024101", "202401011", "20240100", "2024-01-01"), "%Y%m%d"),
    1:6
  )
})

test_that("%H:%M is a time of day: 23:59 fits and 24:00 does not", {
  expect_identical(dta_r_temporal_bad("23:59", "%H:%M"), integer(0))
  expect_identical(dta_r_temporal_bad("24:00", "%H:%M"), 1L)
})

test_that("a value is never trimmed or padded to fit", {
  fmt <- "%Y-%m-%d"
  expect_identical(
    dta_r_temporal_bad(c("2024-1-5", " 2024-01-05", "2024-01-05 ", "2024-01-05\t", "2024-01-05\n", "2024-01-05x"), fmt),
    1:6
  )
})

test_that("a value outside ASCII cannot fit and does not stop the check", {
  # Full-width digits and a trailing accented letter.
  expect_identical(
    dta_r_temporal_bad(c("2024-01-05", "２０２４-01-05", "2024-01-05é"), "%Y-%m-%d"),
    2:3
  )
})

test_that("missing values are never reported, and positions refer to the whole input", {
  fmt <- "%Y-%m-%d"
  expect_identical(dta_r_temporal_bad(c(NA, "2024-01-05", NA_character_), fmt), integer(0))
  expect_identical(dta_r_temporal_bad(c("nope", NA, "2024-01-05", "worse"), fmt), c(1L, 4L))
  expect_identical(dta_r_temporal_bad(character(0), fmt), integer(0))
  expect_identical(dta_r_temporal_bad(NA_character_, fmt), integer(0))
})

test_that("the positions are integers and repeated values are all reported", {
  bad <- dta_r_temporal_bad(c("x", "2024-01-05", "x", "x"), "%Y-%m-%d")
  expect_type(bad, "integer")
  expect_identical(bad, c(1L, 3L, 4L))
})

test_that("a non-character input is checked through its text", {
  expect_identical(dta_r_temporal_bad(c(20240105, 20240230), "%Y%m%d"), 2L)
  expect_identical(dta_r_temporal_bad(factor(c("2024-01-05", "no")), "%Y-%m-%d"), 2L)
})

test_that("an unusable format aborts instead of passing every value", {
  expect_error(dta_r_temporal_bad("2024-01-05", "%b"), "unusable format")
  expect_error(dta_r_temporal_bad("2024-01-05", ""), "unusable format")
})

# ---- the checker against base R, on generated text ----------------------------

test_that("the checker agrees with a base R round trip on generated and mutated dates", {
  set.seed(20260101)
  n <- 2400L

  # Years from 1000 on: below that the platform's strftime decides whether a
  # year is zero-padded, which would make the oracle disagree with itself across
  # systems; 0000 and 0100 are covered by hand-written cases above.
  year <- sample(1000:9999, n, replace = TRUE)
  edge_years <- rep(c(1000L, 1600L, 1700L, 1900L, 2000L, 2023L, 2024L, 2100L, 2400L, 9999L), 24)
  year[seq_along(edge_years)] <- edge_years
  month <- sample(c(1:12, 0L, 13L), n, replace = TRUE, prob = c(rep(1, 12), 0.15, 0.15))
  day <- sample(c(1:31, 0L, 32L), n, replace = TRUE, prob = c(rep(1, 31), 0.15, 0.15))
  # Many February 27th to 31st, in leap, century and non-leap years: the leap
  # rule is where an implementation errs.
  feb <- seq(241L, 840L)
  month[feb] <- 2L
  day[feb] <- sample(27:31, length(feb), replace = TRUE)
  year[feb] <- sample(
    c(1600L, 1700L, 1900L, 2000L, 2100L, 2400L, 2023L, 2024L, 1999L, 2020L),
    length(feb),
    replace = TRUE
  )

  mutate <- function(x, kind) {
    switch(kind,
      sub(x, pattern = "([^0-9])0", replacement = "\\1"),
      paste0(x, " "),
      paste0(" ", x),
      paste0(x, "x"),
      substr(x, 1L, nchar(x) - 1L),
      paste0(x, "\n"),
      paste0(substr(x, 1L, 3L), "a", substr(x, 5L, nchar(x))),
      chartr("-/", "/-", x),
      sub("([-/])", "\\1\\1", x)
    )
  }

  formats <- list(
    list(fmt = "%Y-%m-%d", write = function(y, m, d) sprintf("%04d-%02d-%02d", y, m, d)),
    list(fmt = "%Y%m%d", write = function(y, m, d) sprintf("%04d%02d%02d", y, m, d)),
    list(fmt = "%d/%m/%Y", write = function(y, m, d) sprintf("%02d/%02d/%04d", d, m, y))
  )
  for (case in formats) {
    x <- case$write(year, month, day)
    picked <- which(runif(n) < 0.4)
    kinds <- sample(9L, length(picked), replace = TRUE)
    for (k in seq_along(picked)) {
      x[picked[k]] <- mutate(x[picked[k]], kinds[k])
    }

    parsed <- as.Date(x, format = case$fmt)
    # glibc writes %Y without zero padding (year 202 comes out as "202"), while
    # Windows and macOS pad to four digits. The truncation mutation produces such
    # three-digit years, so the oracle writes the year itself, padded: the checker
    # requires four digits, and the oracle must mean the same on every platform.
    year_text <- sprintf("%04d", as.integer(format(parsed, "%Y")))
    rendered <- unname(mapply(
      sub, "@YEAR@", year_text,
      format(parsed, gsub("%Y", "@YEAR@", case$fmt, fixed = TRUE)),
      MoreArgs = list(fixed = TRUE), USE.NAMES = FALSE
    ))
    valid <- !is.na(parsed) & !is.na(rendered) & rendered == x

    # The test must have power: plenty of both kinds.
    expect_gt(sum(valid), 500L, label = paste("valid strings for", case$fmt))
    expect_gt(sum(!valid), 500L, label = paste("invalid strings for", case$fmt))
    expect_identical(
      dta_r_temporal_bad(x, case$fmt),
      which(!valid),
      info = case$fmt
    )
  }
})

# ---- the renderer -------------------------------------------------------------

test_that("a Date is rendered field by field, zero-padded", {
  d <- as.Date("2024-02-29")
  expect_identical(dta_r_render_temporal(d, "%Y-%m-%d"), "2024-02-29")
  expect_identical(dta_r_render_temporal(d, "%d/%m/%Y"), "29/02/2024")
  expect_identical(dta_r_render_temporal(d, "%Y%m%d"), "20240229")
  expect_identical(dta_r_render_temporal(as.Date("2024-01-05"), "%d.%m.%Y"), "05.01.2024")
})

test_that("the year is always written with four digits", {
  # 719162 days before 1970-01-01 is 0001-01-01.
  early <- structure(-719162, class = "Date")
  expect_identical(dta_r_render_temporal(early, "%Y-%m-%d"), "0001-01-01")
  expect_identical(dta_r_render_temporal(structure(-719162 + 59, class = "Date"), "%Y-%m-%d"), "0001-03-01")
})

test_that("a missing or non-finite value stays missing", {
  d <- structure(c(19782, NA, Inf), class = "Date")
  expect_identical(dta_r_render_temporal(d, "%Y-%m-%d"), c("2024-02-29", NA, NA))
  expect_identical(dta_r_render_temporal(as.Date(character(0)), "%Y-%m-%d"), character(0))
  expect_identical(dta_r_render_temporal(as.Date(NA), "%Y-%m-%d"), NA_character_)
})

test_that("a POSIXct is rendered in UTC whatever its own time zone", {
  utc <- as.POSIXct("2024-02-29 13:05:09", tz = "UTC")
  expect_identical(dta_r_render_temporal(utc, "%Y-%m-%d %H:%M:%S"), "2024-02-29 13:05:09")
  expect_identical(dta_r_render_temporal(utc, "%Y-%m-%dT%H:%M:%SZ"), "2024-02-29T13:05:09Z")
  expect_identical(dta_r_render_temporal(utc, "%Y-%m-%d %H:%M"), "2024-02-29 13:05")

  # 23:30 in New York on 29 February is 04:30 UTC on 1 March.
  local <- as.POSIXct("2024-02-29 23:30:00", tz = "America/New_York")
  expect_identical(dta_r_render_temporal(local, "%Y-%m-%d %H:%M:%S"), "2024-03-01 04:30:00")
})

test_that("%OSn writes the first n digits of the fraction, truncated, never rounded up", {
  half <- structure(0.3, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  # format(x, "%OS3") prints .299 for this value; the arithmetic must not.
  expect_identical(dta_r_render_temporal(half, "%Y-%m-%d %H:%M:%OS3"), "1970-01-01 00:00:00.300")
  expect_identical(dta_r_render_temporal(half, "%Y-%m-%d %H:%M:%OS1"), "1970-01-01 00:00:00.3")
  expect_identical(dta_r_render_temporal(half, "%Y-%m-%d %H:%M:%OS6"), "1970-01-01 00:00:00.300000")

  value <- structure(1700000000.123, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  expect_identical(dta_r_render_temporal(value, "%Y-%m-%d %H:%M:%OS3"), "2023-11-14 22:13:20.123")
  expect_identical(dta_r_render_temporal(value, "%Y-%m-%d %H:%M:%OS2"), "2023-11-14 22:13:20.12")
  # %S shows no fraction at all.
  expect_identical(dta_r_render_temporal(value, "%Y-%m-%d %H:%M:%S"), "2023-11-14 22:13:20")
})

test_that("a value is rounded to whole microseconds before the digits are cut", {
  # 0.9999996 s rounds up to the next whole second, with the carry.
  carry <- structure(59.9999996, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  expect_identical(dta_r_render_temporal(carry, "%Y-%m-%d %H:%M:%OS3"), "1970-01-01 00:01:00.000")
  # 0.9999994 s rounds down to .999999 and is then cut to three digits.
  stay <- structure(59.9999994, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  expect_identical(dta_r_render_temporal(stay, "%Y-%m-%d %H:%M:%OS3"), "1970-01-01 00:00:59.999")
})

test_that("an hms is rendered from its seconds, and hours may pass 23", {
  hms <- function(secs) structure(secs, class = c("hms", "difftime"), units = "secs")
  expect_identical(dta_r_render_temporal(hms(52200), "%H:%M:%S"), "14:30:00")
  expect_identical(dta_r_render_temporal(hms(52200), "%H:%M"), "14:30")
  expect_identical(dta_r_render_temporal(hms(0), "%T"), "00:00:00")
  expect_identical(dta_r_render_temporal(hms(86399), "%T"), "23:59:59")
  expect_identical(dta_r_render_temporal(hms(90000), "%H:%M:%S"), "25:00:00")
  expect_identical(dta_r_render_temporal(hms(360000), "%H:%M:%S"), "100:00:00")
  expect_identical(dta_r_render_temporal(hms(c(52200, NA)), "%H:%M:%S"), c("14:30:00", NA))
  # Truncated, not rounded: 3661.257 s is 01:01:01 and .25.
  expect_identical(dta_r_render_temporal(hms(3661.257), "%H:%M:%OS2"), "01:01:01.25")
})

test_that("a difftime in other units is converted to seconds first", {
  expect_identical(dta_r_render_temporal(as.difftime(90, units = "mins"), "%H:%M:%S"), "01:30:00")
  expect_identical(dta_r_render_temporal(as.difftime(2, units = "hours"), "%H:%M"), "02:00")
})

test_that("the renderer refuses what it cannot render", {
  hms <- structure(100, class = c("hms", "difftime"), units = "secs")
  expect_error(dta_r_render_temporal(hms, "%Y-%m-%d"), "no date parts")
  expect_error(dta_r_render_temporal(1, "%Y-%m-%d"), "must be a Date, POSIXct, hms or difftime")
  expect_error(dta_r_render_temporal("2024-01-05", "%Y-%m-%d"), "must be a Date, POSIXct, hms or difftime")
  expect_error(dta_r_render_temporal(as.Date("2024-01-05"), "%b"), "unusable format")
})

test_that("what the renderer writes always fits the format it was driven by", {
  dates <- seq(as.Date("0100-01-01"), by = "97 days", length.out = 900)
  dates <- c(dates, as.Date(c("2024-02-29", "1900-03-01", "9999-12-31")))
  for (fmt in c("%Y-%m-%d", "%d/%m/%Y", "%Y%m%d")) {
    expect_identical(
      dta_r_temporal_bad(dta_r_render_temporal(dates, fmt), fmt),
      integer(0),
      info = fmt
    )
  }

  stamps <- as.POSIXct("2001-01-01 00:00:00", tz = "UTC") + seq(0, by = 86413.337, length.out = 700)
  for (fmt in c("%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%OS3Z", "%d/%m/%Y %H:%M")) {
    expect_identical(
      dta_r_temporal_bad(dta_r_render_temporal(stamps, fmt), fmt),
      integer(0),
      info = fmt
    )
  }

  clock <- structure(seq(0, 86399, by = 977), class = c("hms", "difftime"), units = "secs")
  for (fmt in c("%H:%M:%S", "%H:%M", "%H:%M:%OS6")) {
    expect_identical(
      dta_r_temporal_bad(dta_r_render_temporal(clock, fmt), fmt),
      integer(0),
      info = fmt
    )
  }
})

test_that("an hms past 24 hours renders to text the checker rejects", {
  clock <- structure(c(86399, 86400, 90000), class = c("hms", "difftime"), units = "secs")
  txt <- dta_r_render_temporal(clock, "%H:%M:%S")
  expect_identical(txt, c("23:59:59", "24:00:00", "25:00:00"))
  expect_identical(dta_r_temporal_bad(txt, "%H:%M:%S"), 2:3)
})

# ---- the units of a difftime ------------------------------------------------------
# as.numeric(x, units = "secs") ignores the `units` attribute of an hms-classed
# vector once the hms namespace is loaded, so the renderer reads the number and
# the attribute itself.

test_that("a plain difftime is rendered in whatever units it carries", {
  render <- function(x, units, format = "%H:%M:%S") {
    dta_r_render_temporal(as.difftime(x, units = units), format)
  }
  expect_identical(render(5400, "secs"), "01:30:00")
  expect_identical(render(90, "mins"), "01:30:00")
  expect_identical(render(1.5, "hours"), "01:30:00")
  expect_identical(render(1.5, "hours", "%H:%M"), "01:30")
  expect_identical(render(0.5, "days"), "12:00:00")
  expect_identical(render(c(1.5, 2, NA), "hours", "%H:%M"), c("01:30", "02:00", NA))
  # a fraction survives the conversion
  expect_identical(render(1.5, "mins", "%H:%M:%OS1"), "00:01:30.0")
})

test_that("an hms-classed vector is rendered by its units attribute, in seconds when it has none", {
  hms <- function(x, units = NULL) {
    out <- structure(x, class = c("hms", "difftime"))
    if (!is.null(units)) attr(out, "units") <- units
    out
  }
  expect_identical(dta_r_render_temporal(hms(1.5, "hours"), "%H:%M"), "01:30")
  expect_identical(dta_r_render_temporal(hms(90, "mins"), "%H:%M"), "01:30")
  expect_identical(dta_r_render_temporal(hms(5400, "secs"), "%H:%M"), "01:30")
  expect_identical(dta_r_render_temporal(hms(5400), "%H:%M"), "01:30")
  expect_identical(dta_r_render_temporal(hms(c(1.5, NA), "hours"), "%T"), c("01:30:00", NA))
  # the hours of a value past a day are still the real hour count
  expect_identical(dta_r_render_temporal(hms(25, "hours"), "%H:%M:%S"), "25:00:00")
})

test_that("the hms namespace being loaded changes nothing", {
  skip_if_not_installed("hms")
  # getExportedValue(), not `hms::hms`: hms is not a declared dependency, and
  # R CMD check flags a `::` call on an undeclared package even behind a skip.
  value <- getExportedValue("hms", "hms")(hours = 1, minutes = 30)
  expect_identical(dta_r_render_temporal(value, "%H:%M:%S"), "01:30:00")
  hours <- structure(1.5, class = c("hms", "difftime"), units = "hours")
  expect_identical(dta_r_render_temporal(hours, "%H:%M"), "01:30")
})

# ---- control characters in a refused format ---------------------------------------

test_that("a control character in a refused format is shown as its escape, not printed raw", {
  problem <- function(format) dta_r_format_parse(format)$problem
  expect_match(problem("%H:%M\t"), "The character '\\t' is not allowed", fixed = TRUE)
  expect_match(problem("%H:%M\n"), "The character '\\n' is not allowed", fixed = TRUE)
  expect_match(problem("%H:%M\001"), "The character '\\001' is not allowed", fixed = TRUE)
  # after %OSn, and as a directive
  expect_match(problem("%H:%M:%OS3\t1"), "'\\t'", fixed = TRUE)
  expect_match(problem("%\t"), "The directive '%\\t' is not supported", fixed = TRUE)
  expect_match(problem("%O\t"), "'%O\\t'", fixed = TRUE)
  for (format in c("%H:%M\t", "%H:%M\n", "%H:%M\001", "%\t", "%O\t", "%H:%M:%OS3\t1")) {
    expect_false(grepl("[[:cntrl:]]", problem(format)), info = encodeString(format))
  }
  # ordinary refusals are worded as before, a printable backslash as typed
  expect_match(problem("%H{M"), "The character '{' is not allowed", fixed = TRUE)
  expect_match(problem("%H\\M"), "The character '\\' is not allowed", fixed = TRUE)
})
