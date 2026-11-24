# Date/time masks (`<<yyyyMMdd>>`) inside regex-mode fields. Expected values
# come from the token table in the plan, not from observed output.

expand <- dta_expand_date_pattern_tokens
# TRUE where the whole value matches the mask, in both engines in play.
full_match <- function(mask, values) {
  rx <- expand(paste0("^<<", mask, ">>$"))
  pcre <- grepl(rx, values, perl = TRUE)
  icu <- stringr::str_detect(values, rx)
  expect_identical(pcre, icu)
  pcre
}

test_that("a pattern without a mask is returned unchanged", {
  x <- c("^ae_[0-9]{12}\\.csv$", "a<b>c", "", "\\<\\<x\\>\\>")
  expect_identical(expand(x), x)
  expect_identical(expand(character(0)), character(0))
  expect_null(expand(NULL))
})

test_that("NA passes through and the input is expanded element by element", {
  out <- expand(c("<<yyyy>>", NA, "x", "a<<MM>>b"))
  expect_identical(out, c("20[0-9]{2}", NA, "x", "a(?:0[1-9]|1[0-2])b"))
  expect_identical(expand(NA_character_), NA_character_)
})

test_that("each token compiles to its fragment", {
  expect_identical(expand("<<yyyy>>"), "20[0-9]{2}")
  expect_identical(expand("<<yy>>"), "[0-9]{2}")
  expect_identical(expand("<<MM>>"), "(?:0[1-9]|1[0-2])")
  expect_identical(expand("<<dd>>"), "(?:0[1-9]|[12][0-9]|3[01])")
  expect_identical(expand("<<d>>"), "(?:[1-9]|[12][0-9]|3[01])")
  expect_identical(expand("<<HH>>"), "(?:[01][0-9]|2[0-3])")
  expect_identical(expand("<<H>>"), "(?:[0-9]|1[0-9]|2[0-3])")
  expect_identical(expand("<<mm>>"), "[0-5][0-9]")
  expect_identical(expand("<<ss>>"), "[0-5][0-9]")
})

test_that("expansion is idempotent: the output never holds a mask", {
  x <- c("^s_<<yyyyMMddHHmm>>\\.csv$", "<<yyyy-MM-ddTHH:mm:ss>>")
  expect_identical(expand(expand(x)), expand(x))
  expect_false(any(grepl("<<", expand(x), fixed = TRUE)))
})

test_that("token boundary values are accepted and out-of-range ones rejected", {
  expect_identical(full_match("yyyy", c("2000", "2099", "1999", "2100", "202")), c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(full_match("yy", c("00", "99", "1", "100")), c(TRUE, TRUE, FALSE, FALSE))
  expect_identical(full_match("MM", c("01", "12", "00", "13", "1")), c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(full_match("dd", c("01", "31", "00", "32", "1")), c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(full_match("d", c("1", "9", "31", "0", "01", "32")), c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(full_match("HH", c("00", "23", "24", "0")), c(TRUE, TRUE, FALSE, FALSE))
  expect_identical(full_match("H", c("0", "9", "23", "24", "00")), c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(full_match("mm", c("00", "59", "60", "5")), c(TRUE, TRUE, FALSE, FALSE))
  expect_identical(full_match("ss", c("00", "59", "60")), c(TRUE, TRUE, FALSE))
})

test_that("month names are title case only", {
  expect_identical(full_match("MMM", c("Jan", "Dec", "JAN", "jan", "January")), c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(full_match("MMMM", c("January", "September", "JANUARY", "Jan")), c(TRUE, TRUE, FALSE, FALSE))
})

test_that("the no-separator timestamps match real values and reject malformed ones", {
  expect_identical(
    full_match("yyyyMMdd", c("20240131", "20241231", "20241301", "20240132", "19991231", "2024013")),
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  expect_identical(
    full_match("yyyyMMddHHmm", c("202401311459", "202412312359", "202401312400", "202401311460", "999999999999")),
    c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  expect_identical(
    full_match("yyyyMMddHHmmss", c("20240131145959", "20240131145960", "2024013114595")),
    c(TRUE, FALSE, FALSE)
  )
})

test_that("characters other than the six token letters are literal and escaped", {
  expect_identical(
    full_match("yyyy-MM-dd", c("2024-01-31", "2024x01x31", "20240131")),
    c(TRUE, FALSE, FALSE)
  )
  expect_identical(
    full_match("yyyy-MM-ddTHH:mm:ss", c("2024-01-31T14:59:59", "2024-01-31 14:59:59", "2024-01-31T24:00:00")),
    c(TRUE, FALSE, FALSE)
  )
  # A regex metacharacter inside the mask is literal, not "any character".
  expect_identical(full_match("dd.MM.yyyy", c("31.01.2024", "31x01x2024")), c(TRUE, FALSE))
  expect_identical(expand("<<yyyy.MM>>"), "20[0-9]{2}\\.(?:0[1-9]|1[0-2])")
})

test_that("a mask sits inside an ordinary regex next to brace quantifiers", {
  rx <- expand("^s123_[0-9]{3}_<<yyyyMMddHHmm>>\\.csv$")
  expect_true(grepl(rx, "s123_001_202401311459.csv", perl = TRUE))
  expect_false(grepl(rx, "s123_001_202413311459.csv", perl = TRUE))
})

test_that("a run of a token letter that is no token is refused, naming the run", {
  for (bad in c("yyy", "y", "yyyyy", "M", "MMMMM", "ddd", "HHH", "m", "mmm", "s", "sss")) {
    expect_error(expand(paste0("x<<", bad, ">>")), class = "dta_date_pattern_invalid", info = bad)
  }
  expect_error(expand("<<yyyyMMddHHH>>"), "\"HHH\"", class = "dta_date_pattern_invalid")
})

test_that("the wrong case of a single-case token letter is refused, naming the right one", {
  cases <- list(
    YYYYMMDD = c("\"YYYY\"", "`yyyy` (year)"),
    yyyyMMDD = c("\"DD\"", "`d` (day)"),
    HHmmSS = c("\"SS\"", "`ss` (second)"),
    hhmmss = c("\"hh\"", "`H` (hour)")
  )
  for (m in names(cases)) {
    mask <- paste0("^a_<<", m, ">>\\.csv$")
    expect_error(expand(mask), class = "dta_date_pattern_invalid", info = m)
    problem <- dta_date_pattern_problem(mask)
    for (part in cases[[m]]) expect_match(problem, part, fixed = TRUE, info = m)
  }
  # Outside a mask the letters stay ordinary text.
  expect_identical(expand("^YYYY_DD_hh_SS\\.csv$"), "^YYYY_DD_hh_SS\\.csv$")
})

test_that("an empty or unclosed mask is refused", {
  expect_error(expand("a<<>>b"), class = "dta_date_pattern_invalid")
  expect_match(dta_date_pattern_problem("a<<>>b"), "is empty", fixed = TRUE)
  expect_error(expand("a<<yyyy"), class = "dta_date_pattern_invalid")
  expect_match(dta_date_pattern_problem("a<<yyyy"), "never closed", fixed = TRUE)
})

test_that("a minute token among dates is refused as a mistyped month", {
  for (bad in c("yyyymmdd", "yyyy-mm-dd", "mm/dd/yyyy", "yyyymm")) {
    expect_error(expand(paste0("<<", bad, ">>")), class = "dta_date_pattern_invalid", info = bad)
    # cli wraps the raw message; the problem text is unwrapped.
    expect_match(dta_date_pattern_problem(paste0("<<", bad, ">>")), "mistyped `MM` (month)", fixed = TRUE, info = bad)
  }
})

test_that("a month token among times is refused as mistyped minutes", {
  for (bad in c("yyyyMMddHHMM", "HHMM", "HH:MM:ss", "MMss")) {
    expect_error(expand(paste0("<<", bad, ">>")), class = "dta_date_pattern_invalid", info = bad)
    expect_match(dta_date_pattern_problem(paste0("<<", bad, ">>")), "mistyped `mm` (minutes)", fixed = TRUE, info = bad)
  }
})

test_that("correctly cased date-and-time masks are not caught by the case guard", {
  ok <- c(
    "yyyyMMddHHmm", "yyyyMMddHHmmss", "yyyy-MM-ddTHH:mm:ss", "HH:mm dd.MM.yyyy", "ddMMHHmm", "MMyyyy", "mmss",
    # MMM/MMMM have no minute-token counterpart (different run length), so a
    # month-name token next to a time-only token is never ambiguous.
    "MMMM_HHmm", "MMM-HH", "HHmm_MMMM"
  )
  for (m in ok) {
    expect_no_error(expand(paste0("<<", m, ">>")))
  }
})

test_that("dta_date_pattern_problem() is NULL for a good mask and the message for a bad one", {
  expect_null(dta_date_pattern_problem("^a_<<yyyyMMdd>>\\.csv$"))
  expect_null(dta_date_pattern_problem("no mask here"))
  expect_match(dta_date_pattern_problem(c("ok", "<<mmm>>")), "mmm", fixed = TRUE)
})
