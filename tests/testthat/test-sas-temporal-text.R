# The display text of SAS dates, datetimes and times.
#
# Every expected string below is written out from the SAS definitions of the
# formats (DATEw., DATETIMEw., TIMEw.d) and the SAS epoch, 1960-01-01, never
# taken from the function under test. SAS day 23968 is 3653 + 20315: the 3653
# days from 1960-01-01 to 1970-01-01, plus the 20315 days from 1970-01-01 to
# 2025-08-15.

rt <- dta_sas_render_temporal

test_that("a DATEw. format renders the SAS display text at each width", {
  expect_identical(rt(23968, "Date", "DATE9."), "15AUG2025")
  expect_identical(rt(23968, "Date", "DATE10."), "15AUG2025")
  expect_identical(rt(23968, "Date", "DATE7."), "15AUG25")
  expect_identical(rt(23968, "Date", "DATE8."), "15AUG25")
  expect_identical(rt(23968, "Date", "DATE5."), "15AUG")
  expect_identical(rt(23968, "Date", "DATE6."), "15AUG")
  expect_identical(rt(23968, "Date", "DATE11."), "15-AUG-2025")
})

test_that("the SAS epoch, leap days and dates before 1960 are exact", {
  expect_identical(rt(0, "Date", "DATE9."), "01JAN1960")
  expect_identical(rt(-1, "Date", "DATE9."), "31DEC1959")
  expect_identical(rt(-1, "Date", "DATE7."), "31DEC59")
  expect_identical(rt(14669, "Date", "DATE9."), "29FEB2000")
  expect_identical(rt(3653, "Date", "DATE9."), "01JAN1970")
  expect_identical(rt(19068, "Date", "DATE9."), "16MAR2012")
  # 1950-01-01 is 10 years back, two of them with a leap day (1952, 1956).
  expect_identical(rt(-3652, "Date", "DATE9."), "01JAN1950")
  # 9999-12-31 is 2932896 days after 1970-01-01.
  expect_identical(rt(2932896 + 3653, "Date", "DATE9."), "31DEC9999")
})

test_that("every month renders its own three-letter abbreviation", {
  # SAS day numbers of the first of each month of 2025, counted by hand:
  # 1960-01-01 to 2025-01-01 is 65 years of 365 days plus the 17 leap days of
  # 1960, 1964, ..., 2024 (2000 included) = 23742; the months then start 0, 31,
  # 59, 90, 120, 151, 181, 212, 243, 273, 304 and 334 days into a non-leap year.
  first_of_month <- 23742 + c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)
  expect_identical(
    rt(first_of_month, "Date", "DATE9."),
    c(
      "01JAN2025", "01FEB2025", "01MAR2025", "01APR2025", "01MAY2025", "01JUN2025",
      "01JUL2025", "01AUG2025", "01SEP2025", "01OCT2025", "01NOV2025", "01DEC2025"
    )
  )
  expect_identical(rt(first_of_month[[12]], "Date", "DATE11."), "01-DEC-2025")
})

test_that("the first Gregorian year renders as text and stops at 1582", {
  # 1582-10-14 to 1583-01-01 is 79 days; the 377 years 1583-1959 hold 91 leap
  # days; so 1582-10-14 is 377 * 365 + 91 + 79 = 137775 days before 1960-01-01.
  expect_identical(rt(-137775, "Date", "DATE9."), "14OCT1582")
  expect_identical(rt(-137775, "Date"), "1582-10-14")
})

test_that("a DATETIMEw. format renders the SAS display text at each width", {
  expect_identical(rt(2070887400, "DateTime", "DATETIME20."), "15AUG2025:14:30:00")
  expect_identical(rt(2070887400, "DateTime", "DATETIME19."), "15AUG2025:14:30:00")
  expect_identical(rt(2070887400, "DateTime", "DATETIME16."), "15AUG25:14:30:00")
  expect_identical(rt(2070887400, "DateTime", "DATETIME18."), "15AUG25:14:30:00")
  expect_identical(rt(2070887400, "DateTime", "DATETIME13."), "15AUG25:14:30")
  expect_identical(rt(2070887400, "DateTime", "DATETIME15."), "15AUG25:14:30")
  expect_identical(rt(1267444800, "DateTime", "DATETIME20."), "29FEB2000:12:00:00")
  expect_identical(rt(315619200, "DateTime", "DATETIME20."), "01JAN1970:00:00:00")
  expect_identical(rt(0, "DateTime", "DATETIME20."), "01JAN1960:00:00:00")
  expect_identical(rt(-1, "DateTime", "DATETIME20."), "31DEC1959:23:59:59")
  expect_identical(rt(1447213759, "DateTime", "DATETIME19."), "10NOV2005:03:49:19")
  expect_identical(rt(1447213759, "DateTime", "DATETIME18."), "10NOV05:03:49:19")
})

test_that("a TIMEw.d format renders h:mm[:ss] with an unpadded hour", {
  expect_identical(rt(52200, "Time", "TIME8."), "14:30:00")
  expect_identical(rt(52200, "Time", "TIME5."), "14:30")
  expect_identical(rt(52200, "Time", "TIME7."), "14:30")
  expect_identical(rt(0, "Time", "TIME8."), "0:00:00")
  expect_identical(rt(0, "Time", "TIME5."), "0:00")
  expect_identical(rt(3600, "Time", "TIME8."), "1:00:00")
  expect_identical(rt(86399, "Time", "TIME8."), "23:59:59")
})

test_that("a time is a duration: it runs past 24 hours and past 100", {
  expect_identical(rt(90000, "Time", "TIME8."), "25:00:00")
  expect_identical(rt(360000, "Time", "TIME10."), "100:00:00")
  expect_identical(rt(360000, "Time", NULL), "100:00:00")
})

test_that("a negative time carries a leading minus", {
  expect_identical(rt(-1, "Time", "TIME8."), "-0:00:01")
  expect_identical(rt(-3661, "Time", "TIME8."), "-1:01:01")
  expect_identical(rt(-1, "Time", NULL), "-00:00:01")
})

test_that("TIMEw.d appends only as many fraction digits as fit in w", {
  expect_identical(rt(52200.25, "Time", "TIME11.2"), "14:30:00.25")
  expect_identical(rt(52200, "Time", "TIME11.2"), "14:30:00.00")
  # d digits are dropped, then the point, until the text fits the width.
  expect_identical(rt(52200.25, "Time", "TIME11.3"), "14:30:00.25")
  expect_identical(rt(52200.25, "Time", "TIME9.2"), "14:30:00")
  expect_identical(rt(52200.25, "Time", "TIME8.2"), "14:30:00")
  # Truncation, not rounding, of a fraction the width cannot show: an
  # assumption, not verified against SAS.
  expect_identical(rt(52200.25, "Time", "TIME10.2"), "14:30:00.2")
  expect_identical(rt(52200.75, "Time", "TIME8."), "14:30:00")
  expect_identical(rt(86399.999, "Time", "TIME8."), "23:59:59")
})

test_that("without a format the text is ISO 8601", {
  expect_identical(rt(23968, "Date"), "2025-08-15")
  expect_identical(rt(23968, "Date", NULL), "2025-08-15")
  expect_identical(rt(2070887400, "DateTime"), "2025-08-15T14:30:00")
  expect_identical(rt(52200, "Time"), "14:30:00")
  expect_identical(rt(0, "Date"), "1960-01-01")
  expect_identical(rt(-1, "DateTime"), "1959-12-31T23:59:59")
  expect_identical(rt(3600, "Time"), "01:00:00")
})

test_that("a format of another kind, or without text at that width, falls back to ISO", {
  expect_identical(rt(23968, "Date", "DATE12."), "2025-08-15")
  expect_identical(rt(23968, "Date", "DATE4."), "2025-08-15")
  expect_identical(rt(2070887400, "DateTime", "DATETIME12."), "2025-08-15T14:30:00")
  expect_identical(rt(2070887400, "DateTime", "DATETIME41."), "2025-08-15T14:30:00")
  expect_identical(rt(52200, "Time", "TIME4."), "14:30:00")
  expect_identical(rt(23968, "Date", "TIME8."), "2025-08-15")
  expect_identical(rt(52200, "Time", "DATE9."), "14:30:00")
  expect_identical(rt(23968, "Date", "not a format"), "2025-08-15")
})

test_that("years outside 1582-9999 are ISO whatever the format", {
  # 9999-12-31 is 2932896 days after 1970-01-01; the next day is year 10000.
  expect_identical(rt(2932896 + 3653 + 1, "Date", "DATE9."), "10000-01-01")
  expect_identical(rt(2932896 + 3653 + 1, "Date"), "10000-01-01")
  # 1412 is before the Gregorian calendar: the expectation comes from base R.
  d <- -200000
  expect_identical(
    rt(d, "Date", "DATE9."),
    format(as.Date(d - 3653, origin = "1970-01-01"), "%Y-%m-%d")
  )
  expect_identical(
    rt((2932896 + 3653 + 1) * 86400, "DateTime", "DATETIME20."),
    "10000-01-01T00:00:00"
  )
})

test_that("missing and non-finite numbers stay NA", {
  expect_identical(rt(NA_real_, "Date", "DATE9."), NA_character_)
  expect_identical(rt(c(NA, Inf, -Inf, NaN), "Time", "TIME8."), rep(NA_character_, 4))
  expect_identical(rt(numeric(0), "Date"), character(0))
  expect_identical(rt(NA, "DateTime"), NA_character_)
})

test_that("a vector mixes values and missings in order", {
  expect_identical(
    rt(c(0, 23968, NA, -1, 14669, 3653), "Date", "DATE9."),
    c("01JAN1960", "15AUG2025", NA, "31DEC1959", "29FEB2000", "01JAN1970")
  )
  expect_identical(
    rt(c(0, 2070887400, NA, -1, 1267444800, 315619200), "DateTime", "DATETIME20."),
    c(
      "01JAN1960:00:00:00", "15AUG2025:14:30:00", NA,
      "31DEC1959:23:59:59", "29FEB2000:12:00:00", "01JAN1970:00:00:00"
    )
  )
  expect_identical(
    rt(c(0, 52200, NA, 86399, 43200, 3600), "Time", "TIME8."),
    c("0:00:00", "14:30:00", NA, "23:59:59", "12:00:00", "1:00:00")
  )
  expect_identical(rt(c(NA, NA), "Date"), c(NA_character_, NA_character_))
})

test_that("the civil-date arithmetic agrees with base R over a wide range", {
  # An independent oracle: base R's proleptic Gregorian calendar, formatted with
  # the locale-free %Y-%m-%d.
  days <- seq(-100000, 100000, by = 997)
  expect_identical(
    rt(days, "Date"),
    format(as.Date(days - 3653, origin = "1970-01-01"), "%Y-%m-%d")
  )
  secs <- days * 86400 + (days %% 86400)
  expect_identical(
    rt(secs, "DateTime"),
    format(
      as.POSIXct(secs - 3653 * 86400, origin = "1970-01-01", tz = "UTC"),
      "%Y-%m-%dT%H:%M:%S",
      tz = "UTC"
    )
  )
  # Every leap-day boundary: the day before and after 29 February.
  leap <- c(14668, 14669, 14670, 14610 + 59, 14610 + 60)
  expect_identical(
    rt(leap, "Date"),
    format(as.Date(leap - 3653, origin = "1970-01-01"), "%Y-%m-%d")
  )
})

# ---- consistency with the package's own format checker -------------------------
# Not an oracle: it pins that the text this function writes is text the format
# check accepts. The expected strings above are what pin the text itself.

st_format_errors <- function(text, type, format) {
  specs <- DTAColumnSpecCollection(columns = list(
    X = DTAColumnSpec(id = "X", type = type, format = format, nullable = TRUE)
  ))
  details <- validate_table_detailed(
    specs, data.frame(X = text, stringsAsFactors = FALSE),
    verbose = FALSE
  )
  err <- details$columnspec_errors$full_error
  if (is.null(err)) 0L else sum(err$keyword == "format")
}

test_that("every non-fallback text is accepted by the format check", {
  dates <- c(0, 23968, -1, 14669, 3653, 19068, 2932896 + 3653)
  for (w in c(5, 6, 7, 8, 9, 10, 11)) {
    fmt <- paste0("DATE", w, ".")
    expect_identical(
      st_format_errors(rt(dates, "Date", fmt), "SAS Date", paste("SAS", fmt)), 0L,
      info = fmt
    )
  }

  stamps <- c(0, 2070887400, -1, 1267444800, 315619200, 1447213759)
  for (w in c(13, 14, 15, 16, 17, 18, 19, 20, 24, 40)) {
    fmt <- paste0("DATETIME", w, ".")
    expect_identical(
      st_format_errors(rt(stamps, "DateTime", fmt), "SAS DateTime", paste("SAS", fmt)), 0L,
      info = fmt
    )
  }

  times <- c(0, 52200, 3600, 86399, 43200)
  for (fmt in c("TIME5.", "TIME7.", "TIME8.", "TIME10.", "TIME12.")) {
    expect_identical(
      st_format_errors(rt(times, "Time", fmt), "SAS Time", paste("SAS", fmt)), 0L,
      info = fmt
    )
  }
  expect_identical(
    st_format_errors(rt(c(52200.25, 0), "Time", "TIME11.2"), "SAS Time", "SAS TIME11.2"), 0L
  )
  expect_identical(
    st_format_errors(rt(c(52200.25, 0), "Time", "TIME8.2"), "SAS Time", "SAS TIME8.2"), 0L
  )
})

test_that("the checker does reject text the function would never write", {
  # Guards the consistency test above against a checker that accepts anything.
  expect_identical(st_format_errors("2025-08-15", "SAS Date", "SAS DATE9."), 1L)
  expect_identical(st_format_errors("15AUG25", "SAS DateTime", "SAS DATETIME20."), 1L)
})
