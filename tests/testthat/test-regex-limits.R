# A pattern or glob a specification supplies is matched by ICU, which has no
# time or stack limit unless asked for. The limits are on in dta_str_detect();
# these tests use tiny inputs and a mocked-down limit, never a real
# catastrophic run: `^(a+)+$` against n 'a's and a '!' takes ~2^n steps, so
# n = 18 is a few hundredths of a second even when nothing stops it.

test_that("a file-name pattern that hits the match limit matches nothing and warns", {
  testthat::local_mocked_bindings(dta_regex_time_limit = 10L)
  h <- DTAFileCSV("^(a+)+$", pattern = TRUE)
  expect_warning(
    res <- matches_filename(h, paste0(strrep("a", 18), "!")),
    class = "dta_regex_limit"
  )
  expect_false(any(res))
})

test_that("an ordinary pattern is unaffected by the limit", {
  h <- DTAFileCSV("^lab_[0-9]+[.]csv$", pattern = TRUE)
  expect_no_warning(res <- matches_filename(h, "lab_12.csv"))
  expect_true(any(res))
  expect_false(any(matches_filename(h, "lab_x.csv")))
  # one logical per declared pattern is kept
  h2 <- DTAFileCSV(c("^a[.]csv$", "^b[.]csv$"), pattern = TRUE)
  expect_identical(as.logical(matches_filename(h2, "b.csv")), c(FALSE, TRUE))
})

test_that("an invalid pattern still errors instead of being swallowed as a limit", {
  expect_error(dta_str_detect("a", "("), regexp = "U_REGEX_MISMATCHED_PAREN", fixed = TRUE)
})

test_that("glob overlap against a long literal returns instead of backtracking", {
  testthat::local_mocked_bindings(dta_regex_time_limit = 10L)
  expect_warning(
    hit <- dta_globs_overlap(paste0(strrep("*a", 5), "*b.zip"), paste0(strrep("a", 80), ".zip")),
    class = "dta_regex_limit"
  )
  expect_false(hit)
})

test_that("a container glob with more than three wildcards is refused", {
  expect_error(
    DTAFileCSV("x.csv", container = "*a*a*a*a*.zip"),
    class = "dta_container_invalid"
  )
  expect_identical(DTAFileCSV("x.csv", container = "*a*a*.zip")@container, "*a*a*.zip")
})

test_that("one name that trips the limit does not hide the others in the same call", {
  testthat::local_mocked_bindings(dta_regex_time_limit = 10L)
  nm <- c("fine", paste0(strrep("a", 18), "!"), "also fine")
  expect_warning(res <- dta_str_detect(nm, "^(a+)+$|fine"), class = "dta_regex_limit")
  expect_identical(res, c(TRUE, FALSE, TRUE))
})

test_that("a vector of hostile names is cut off after a few trips, with one warning for the call", {
  testthat::local_mocked_bindings(dta_regex_time_limit = 10L, dta_regex_max_trips = 2L)
  bad <- paste0(strrep("a", 18), "!")
  nm <- c("fine", bad, bad, "not tried fine", bad)
  n_warn <- 0L
  res <- withCallingHandlers(
    dta_str_detect(nm, "^(a+)+$|fine"),
    dta_regex_limit = function(w) {
      n_warn <<- n_warn + 1L
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(n_warn, 1L)
  # the benign name tried before the cap still matches; those after it stay FALSE
  expect_identical(res, c(TRUE, FALSE, FALSE, FALSE, FALSE))
})
