# Strict numeric conversion. REQ-TYPE-012 .. REQ-TYPE-015.
#
# `dta_as_numeric_strict()` is what every numeric rule reads a column
# through, and it draws a three-way distinction a bare `as.numeric()`
# collapses into one NA: missing (absent in the source), unconvertible
# (present but not representable as a number) and converted. This file pins
# that distinction for a fixed set of inputs chosen for exactly how easy they
# would be to get wrong -- a hex literal, a factor, a hidden decimal --
# rather than exercising it incidentally through a rule.
#
# The 13-case matrix is copied verbatim from the value/missing/unconvertible
# table this package's own unit suite carries at
# tests/testthat/helper-validation-corpus.R:304-347 (`vc_numeric_edges()`).
# It is copied rather than sourced: these files are installed with the
# package and run standalone, and the developer test tree is not part of
# that installation.

# ---- fixtures ---------------------------------------------------------------

tn_matrix <- function() {
  data.frame(
    input = c(
      "42", " 42 ", "-3.5", "1e3", "0x10", "Inf", "NaN",
      "", "   ", NA_character_, "abc", "1,5", "TRUE"
    ),
    # Expected value after conversion (NA where not representable).
    value = c(42, 42, -3.5, 1000, 16, Inf, NaN, NA, NA, NA, NA, NA, NA),
    missing = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      TRUE, TRUE, TRUE,
      FALSE, FALSE, FALSE
    ),
    unconvertible = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      # "NaN" parses to the double NaN, and is.na(NaN) is TRUE, so it is
      # classified as unconvertible rather than missing -- pinned separately
      # as OQ-TYPE-035 (REQ-TYPE-015, LIM-002).
      TRUE,
      FALSE, FALSE, FALSE,
      TRUE, TRUE, TRUE
    ),
    stringsAsFactors = FALSE
  )
}

# A readable label for a qa_step() description, so a failure names the input
# rather than an index into the matrix.
tn_label <- function(x) {
  if (is.na(x)) {
    return("NA")
  }
  if (identical(x, "")) {
    return("'' (empty string)")
  }
  if (nzchar(x) && identical(trimws(x), "")) {
    return(sprintf("'%s' (whitespace only)", x))
  }
  sprintf("'%s'", x)
}

# ---- the semantics matrix -----------------------------------------------------

test_that("OQ-TYPE-030 | strict numeric conversion matches its documented value, missing flag and unconvertible flag for 13 input forms | REQ-TYPE-012", {
  m <- tn_matrix()
  for (i in seq_len(nrow(m))) {
    result <- dta_as_numeric_strict(m$input[i])
    qa_step(
      sprintf("input %s", tn_label(m$input[i])),
      list(value = m$value[i], missing = m$missing[i], unconvertible = m$unconvertible[i]),
      list(value = result$values, missing = result$missing, unconvertible = result$unconvertible)
    )
  }
})

test_that("OQ-TYPE-031 | missing and unconvertible never both hold for the same value | REQ-TYPE-013", {
  m <- tn_matrix()
  # Run over the whole matrix at once, exercising the vectorised form rather
  # than the scalar-at-a-time calls OQ-TYPE-030 makes.
  result <- dta_as_numeric_strict(m$input)

  qa_check(
    "no value across the matrix is both missing and unconvertible",
    !any(result$missing & result$unconvertible)
  )
  # And every value lands in exactly one of the three categories: missing,
  # unconvertible, or a successful conversion.
  category_count <- as.integer(result$missing) + as.integer(result$unconvertible) +
    as.integer(!result$missing & !result$unconvertible)
  qa_step(
    "every value falls into exactly one of missing, unconvertible or converted",
    rep(1L, nrow(m)), category_count
  )
})

# ---- typed inputs take an early return, never unconvertible -----------------

test_that("OQ-TYPE-032 | a Date or POSIXct input takes the early-return branch and is never unconvertible | REQ-TYPE-014", {
  # Dates and date-times carry their own numeric representation and are
  # converted through it directly (as.numeric()), never through text parsing
  # -- so there is no text to fail to parse, and no such value can ever be
  # unconvertible.
  d <- as.Date(c("2020-01-01", NA))
  result_d <- dta_as_numeric_strict(d)
  qa_step("a Date converts to its numeric (day-count) representation", as.numeric(d), result_d$values)
  qa_step("a missing Date is missing, not unconvertible", c(FALSE, TRUE), result_d$missing)
  qa_check("no Date value is ever unconvertible", !any(result_d$unconvertible))

  ts <- as.POSIXct(c("2020-01-01 10:00:00", NA), tz = "UTC")
  result_ts <- dta_as_numeric_strict(ts)
  qa_step("a POSIXct converts to its numeric (seconds-since-epoch) representation", as.numeric(ts), result_ts$values)
  qa_step("a missing POSIXct is missing, not unconvertible", c(FALSE, TRUE), result_ts$missing)
  qa_check("no POSIXct value is ever unconvertible", !any(result_ts$unconvertible))
})

test_that("OQ-TYPE-033 | a numeric or logical input takes the early-return branch and is never unconvertible | REQ-TYPE-014", {
  n <- c(1.5, NA, -2)
  result_n <- dta_as_numeric_strict(n)
  qa_step("an already-numeric column passes through unchanged", n, result_n$values)
  qa_check("no numeric value is ever unconvertible", !any(result_n$unconvertible))

  l <- c(TRUE, FALSE, NA)
  result_l <- dta_as_numeric_strict(l)
  qa_step("a logical column converts to 1/0 directly, not by parsing 'TRUE'/'FALSE' text", c(1, 0, NA), result_l$values)
  qa_check("no logical value is ever unconvertible", !any(result_l$unconvertible))
})

test_that("OQ-TYPE-034 | a factor of digits converts through its printed labels, not its integer level codes | REQ-TYPE-014", {
  f <- factor(c("500", "600", "700"))
  result <- dta_as_numeric_strict(f)

  qa_step("the factor's labels are converted, not its level codes", c(500, 600, 700), result$values)
  # For contrast: as.numeric() on a bare factor reads its integer level
  # codes, small enough to sail through any range rule that admits them --
  # exactly the mistake dta_as_numeric_strict() converts through
  # as.character() first to avoid.
  qa_check(
    "as.numeric() on the bare factor would have given the wrong answer, for contrast",
    identical(as.numeric(f), c(1, 2, 3))
  )
})

# ---- a pinned limitation ------------------------------------------------------

test_that("OQ-TYPE-035 | the text 'NaN' is classified as unconvertible rather than missing | REQ-TYPE-015", {
  result <- dta_as_numeric_strict("NaN")
  qa_check("'NaN' parses to the double NaN", is.nan(result$values))
  # Arguable, but it is the behaviour on record: registered as LIM-002 rather
  # than silently left to drift.
  qa_step(
    "it is classified as unconvertible, not missing",
    list(missing = FALSE, unconvertible = TRUE),
    list(missing = result$missing, unconvertible = result$unconvertible)
  )
})
