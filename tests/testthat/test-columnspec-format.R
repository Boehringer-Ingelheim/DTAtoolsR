# Value-versus-spec conformance: SAS formats, patterns on numeric columns,
# Int range, and a spec that is checked against itself.
#
# Every value case runs through BOTH engines -- the materialising
# validate_table_detailed() and the streaming batch loop -- because both are
# fed by the one evaluator, and the property worth pinning is that they agree.
# Expectations are derived by hand from the format definitions, and cases are
# matched on the constraint keyword or condition class, never on message text.

cf_col <- function(...) {
  DTAColumnSpecCollection(columns = list(X = DTAColumnSpec(id = "X", ...)))
}

# The (row, keyword) pairs each engine reports for one single-column table.
cf_found <- function(specs, values) {
  tab <- data.frame(X = values, stringsAsFactors = FALSE)
  eager <- validate_table_detailed(specs, tab, verbose = FALSE)
  streamed <- dta_validate_table_stream(
    specs,
    dta_as_batch_reader(arrow::as_arrow_table(tab), batch_rows = 2L),
    verbose = FALSE, coerce = FALSE
  )
  pairs <- function(details) {
    err <- details$columnspec_errors$full_error
    if (is.null(err)) {
      return(character(0))
    }
    paste(err$row, err$keyword)
  }
  list(eager = pairs(eager), streamed = pairs(streamed))
}

cf_expect <- function(specs, values, expected) {
  found <- cf_found(specs, values)
  expect_setequal(found$eager, expected)
  expect_setequal(found$streamed, expected)
}


test_that("an Int value beyond the int32 range is a type violation", {
  specs <- cf_col(type = "SAS Int", nullable = FALSE)
  cf_expect(specs, c(1, 2147483647, 5e9, -5e9), c("3 type", "4 type"))
})

test_that("a pattern is applied to numeric columns through plain decimal text", {
  specs <- cf_col(type = "SAS Num", nullable = FALSE, pattern = "^[0-9]{10}$")
  # 1e9 prints as "1e+09" under as.character(); it is ten digits all the same.
  cf_expect(specs, c(123, 1000000000, 9999999999), "1 pattern")
})

test_that("a pattern sees an integer column's digits, and a double at 15 significant digits", {
  cf_expect(
    cf_col(type = "SAS Int", nullable = FALSE, pattern = "^[0-9]{1,3}$"),
    c(7, 999, 1000), "3 pattern"
  )
  cf_expect(
    cf_col(type = "SAS Num", nullable = FALSE, pattern = "^[0-9]+(\\.[0-9]{1,2})?$"),
    c(1.5, 12.25, 0.125), "3 pattern"
  )
})

test_that("a `$w.` format fails values longer than w, unless the length already says so", {
  cf_expect(
    cf_col(type = "SAS Char", format = "SAS $5.", nullable = FALSE),
    c("ABCDE", "ABCDEF"), "2 format"
  )
  # With a length at or below w, maxLength covers it and no format is emitted.
  covered <- cf_col(type = "SAS Char", format = "SAS $5.", length = 3, nullable = FALSE)
  cf_expect(covered, c("ABC", "ABCDEF"), "2 maxLength")
  expect_null(as_json_schema(covered@columns$X)$format)
})

test_that("a `w.d` format fails more decimals than d and more width than w", {
  specs <- cf_col(type = "SAS Num", format = "SAS 8.2", nullable = FALSE)
  # 12345.67 is exactly 8 characters wide; 123456.7 is 9; 1.234 has 3 decimals.
  cf_expect(specs, c(12345.67, 1.234, 123456.7, -1234.5), c("2 format", "3 format"))
})

test_that("a `w.` format fails any fraction and counts the sign in the width", {
  cf_expect(
    cf_col(type = "SAS Num", format = "SAS 5.", nullable = FALSE),
    c(12345, 1.5, 123456), c("2 format", "3 format")
  )
  cf_expect(
    cf_col(type = "SAS Int", format = "SAS 5.", nullable = FALSE),
    c(-1234, -12345), "2 format"
  )
})

test_that("a DATEw. format wants SAS display text, not ISO, and ignores missing values", {
  specs <- cf_col(type = "SAS Date", format = "SAS DATE9.", nullable = TRUE)
  cf_expect(
    specs,
    c("15AUG2025", "2025-08-15", NA, "15aug2025", "15AUG25"),
    "2 format"
  )
})

test_that("TIMEw.d and DATETIMEw. check shape and width", {
  cf_expect(
    cf_col(format = "SAS TIME12.2", nullable = FALSE),
    c("1:02:03", "01:01:01.50", "01:01:01.555", "01:01:01 PM"),
    c("3 format", "4 format")
  )
  cf_expect(
    cf_col(format = "SAS TIME8.", nullable = FALSE),
    c("01:01:01", "01:01:01.5"), "2 format"
  )
  cf_expect(
    cf_col(format = "SAS DATETIME18.", nullable = FALSE),
    c("15AUG2025:14:30:00", "15AUG2025 14:30", "2025-08-15T14:30:00"),
    c("2 format", "3 format")
  )
})

test_that("a BESTw. format constrains nothing and emits no schema keyword", {
  col <- DTAColumnSpec(id = "X", type = "SAS Num", format = "SAS BEST12.", nullable = FALSE)
  expect_null(as_json_schema(col)$format)
  cf_expect(cf_col(type = "SAS Num", format = "SAS BEST12.", nullable = FALSE), c(1, 1e15), character(0))
})

test_that("the per-check summary has a format row, not applicable when nothing declares one", {
  plain <- validate_table_detailed(
    cf_col(type = "SAS Num", nullable = FALSE),
    data.frame(X = 1),
    verbose = FALSE
  )$columnspec_checks
  expect_equal(plain$status[plain$keyword == "format"], "not_applicable")

  declared <- validate_table_detailed(
    cf_col(type = "SAS Num", format = "SAS 8.2", nullable = FALSE),
    data.frame(X = c(1.25, 1.234)),
    verbose = FALSE
  )$columnspec_checks
  expect_equal(declared$status[declared$keyword == "format"], "failed")
  expect_equal(declared$status[declared$keyword == "type"], "passed")
})

test_that("a column declaring both type and format is counted once in the report", {
  out <- testthat::capture_messages(
    validate_table_detailed(
      cf_col(type = "SAS Num", format = "SAS 8.2", nullable = FALSE),
      data.frame(X = 1.25),
      verbose = TRUE
    )
  )
  # One declared column; the folded Format line must not say two.
  expect_true(any(grepl("Format check passed (1 column)", gsub("\\s+", " ", out), fixed = TRUE)))
})

test_that("the report names the SAS format a value failed", {
  details <- validate_table_detailed(
    cf_col(type = "SAS Num", format = "SAS 8.2", nullable = FALSE),
    data.frame(X = 1.234),
    verbose = FALSE
  )
  row <- details$columnspec_errors$full_error
  row1 <- data.frame(
    columnspec_keyword = row$keyword[[1]],
    columnspec_columnspec = row$columnspec[[1]]
  )
  txt <- .report_columnspec_expected_text(row1, html = FALSE)
  expect_match(txt, "8.2", fixed = TRUE)
  expect_match(txt, "SAS format", fixed = TRUE)
})


# ---- a spec checked against itself -------------------------------------------

test_that("construction rejects a value that its own format, length or type rejects", {
  expect_error(
    DTAColumnSpec(id = "X", type = "SAS Char", format = "SAS $3.", values = "ABCD"),
    class = "dta_spec_value_mismatch"
  )
  expect_error(
    DTAColumnSpec(id = "X", type = "SAS Int", values = "abc"),
    class = "dta_spec_value_mismatch"
  )
  expect_error(
    DTAColumnSpec(id = "X", type = "SAS Int", values = "1.5"),
    class = "dta_spec_value_mismatch"
  )
  expect_error(
    DTAColumnSpec(id = "X", type = "SAS Char", length = 2, examples = "abc"),
    class = "dta_spec_value_mismatch"
  )
  expect_error(
    DTAColumnSpec(id = "X", type = "SAS Num", format = "SAS 8.2", values = c(1.5, 1.234)),
    class = "dta_spec_value_mismatch"
  )
  expect_error(
    DTAColumnSpec(id = "X", type = "SAS Date", format = "SAS DATE9.", values = "2025-08-15"),
    class = "dta_spec_value_mismatch"
  )
})

test_that("construction accepts values and examples that fit", {
  expect_no_error(
    DTAColumnSpec(id = "X", type = "SAS Char", format = "SAS $3.", values = c("AB", "ABC"))
  )
  expect_no_error(DTAColumnSpec(id = "X", type = "SAS Int", values = c(1, 2, 3)))
  expect_no_error(
    DTAColumnSpec(id = "X", type = "SAS Num", format = "SAS 8.2", examples = c(1.5, 12345.67))
  )
  expect_no_error(
    DTAColumnSpec(id = "X", type = "SAS Date", format = "SAS DATE9.", values = "15AUG2025")
  )
  # A missing example is not a value that fails.
  expect_no_error(
    DTAColumnSpec(id = "X", type = "SAS Char", length = 3, nullable = TRUE, examples = c("AB", NA))
  )
})

test_that("a values brace in a mismatch message does not break the abort", {
  expect_error(
    DTAColumnSpec(id = "X", type = "SAS Char", length = 2, values = "{abc}"),
    class = "dta_spec_value_mismatch"
  )
})

test_that("construction rejects a pattern that is not a valid regular expression", {
  expect_error(
    DTAColumnSpec(id = "X", type = "SAS Char", pattern = "(["),
    class = "dta_spec_invalid_pattern"
  )
  expect_no_error(DTAColumnSpec(id = "X", type = "SAS Char", pattern = "^[A-Z]{3}$"))
})

test_that("examples are matched against the pattern with PCRE, as the data is", {
  # A lookahead exists in PCRE only: TRE would reject the pattern outright.
  expect_no_error(
    DTAColumnSpec(
      id = "X", type = "SAS Char", pattern = "^(?=[A-Z])[A-Z0-9]{3}$", examples = "A12"
    )
  )
  expect_error(
    DTAColumnSpec(
      id = "X", type = "SAS Char", pattern = "^(?=[A-Z])[A-Z0-9]{3}$", examples = "112"
    ),
    "must conform"
  )
})


# ---- review fixes: cost, DATE11., tight widths, padding, double reports -------

test_that("a numeric pattern converts and tests each distinct value once", {
  seen <- integer(0)
  real <- dta_numeric_text
  testthat::local_mocked_bindings(
    dta_numeric_text = function(x) {
      seen <<- c(seen, length(x))
      real(x)
    }
  )
  # 3 distinct values over 12 rows, one of them missing.
  x <- rep(c(1000000000, 123, NA, 7), 3)
  errs <- dta_check_column_spec(
    "X", list(type = c("number", "null"), pattern = "^[0-9]{10}$"), x
  )
  expect_equal(seen, 3L)
  # 123 and 7 break the pattern; the missing value is not reported; each bad
  # value is reported at every row it occupies.
  expect_equal(errs$row, c(2L, 4L, 6L, 8L, 10L, 12L))
  expect_true(all(errs$keyword == "pattern"))
  expect_equal(errs$data, rep(c("123", "7"), 3))
})

test_that("a string pattern is mapped back to the right rows", {
  x <- c("AB1", NA, "ab1", "AB1", "xyz", "ab1")
  errs <- dta_check_column_spec(
    "X", list(type = c("string", "null"), pattern = "^[A-Z]{2}[0-9]$"), x
  )
  expect_equal(errs$row, c(3L, 5L, 6L))
})

test_that("dta_numeric_text writes whole numbers and exponent forms in full", {
  expect_equal(
    dta_numeric_text(c(1e5, 1e9, 0, -0, -3, 123456789012345, 0.5, 1.5e-5, 1e16, NA)),
    c(
      "100000", "1000000000", "0", "0", "-3", "123456789012345", "0.5", "0.000015",
      "10000000000000000", NA
    )
  )
  expect_equal(dta_numeric_text(c(7L, NA, -12L)), c("7", NA, "-12"))
  expect_equal(dta_numeric_text(c(Inf, -Inf)), c("Inf", "-Inf"))
})

test_that("DATE11. display text with hyphens fits, and the day is 01-31", {
  specs <- cf_col(type = "SAS Date", format = "SAS DATE11.", nullable = FALSE)
  cf_expect(
    specs,
    c("15-AUG-2025", "15AUG2025", "01-jan-25", "31-DEC-2025", "99-FEB-2025", "00JAN2025", "32JAN2025"),
    c("5 format", "6 format", "7 format")
  )
  # Without hyphens the day bound holds under DATE9. as well.
  cf_expect(
    cf_col(type = "SAS Date", format = "SAS DATE9.", nullable = FALSE),
    c("99FEB2025", "28FEB2025"), "1 format"
  )
})

test_that("a tight width drops the leading zero of a value below one", {
  # 0.5 shows as ".50" (3 characters) under 3.2; -0.5 as "-.50" under 4.2.
  cf_expect(
    cf_col(type = "SAS Num", format = "SAS 3.2", nullable = FALSE),
    # 1.5 needs 4; 0.5 needs 4 but loses its zero; 0.999 rounds to 1.00 (4).
    c(0.5, 1.5, 0.999), c("2 format", "3 format")
  )
  cf_expect(
    cf_col(type = "SAS Num", format = "SAS 4.2", nullable = FALSE),
    # -0.5 is "-.50" (4); 12.5 needs 5; 0.5 is 4 with its zero.
    c(-0.5, 12.5, 0.5), "2 format"
  )
  # One character short even without the zero: ".50" is 3, the width is 2.
  cf_expect(cf_col(type = "SAS Num", format = "SAS 2.2", nullable = FALSE), 0.5, "1 format")
  # No decimals, no dropped zero.
  cf_expect(cf_col(type = "SAS Num", format = "SAS 1.", nullable = FALSE), c(0, 12), "2 format")
})

test_that("surrounding spaces and long hours do not make display text invalid", {
  cf_expect(
    cf_col(type = "SAS Date", format = "SAS DATE9.", nullable = FALSE),
    c(" 15AUG2025", "15AUG2025  ", "  15AUG2025  X"), "3 format"
  )
  # The width applies to the trimmed text: nine characters do not fit TIME8.
  cf_expect(
    cf_col(type = "SAS Time", format = "SAS TIME10.", nullable = FALSE),
    c("100:00:00", " 9:05:00", "12345678:00:00"), "3 format"
  )
  cf_expect(
    cf_col(type = "SAS Time", format = "SAS TIME8.", nullable = FALSE),
    c("100:00:00", "99:00:00"), "1 format"
  )
})

test_that("a fraction in an integer-only column is reported once, as a type", {
  cf_expect(
    cf_col(type = "SAS Int", format = "SAS 5.", nullable = FALSE),
    c(1, 1.5, 123456), c("2 type", "3 format")
  )
  # The width test still applies to a value that is whole.
  cf_expect(
    cf_col(type = "SAS Int", format = "SAS 3.", nullable = FALSE),
    c(12, 1234), "2 format"
  )
})
