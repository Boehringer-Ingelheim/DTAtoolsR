# Expected values come from the example/spec definitions, not from output:
# create_example_DTAColumnSpec declares STUDYID values "1234" and "5678" and
# create_example_DTADataSetTabular(2) holds "STUDY001" in STUDYID.

explain_capture <- function(...) {
  out <- NULL
  msgs <- testthat::capture_messages(out <- explain_messages(...))
  list(result = out, text = paste(msgs, collapse = ""))
}

em_specs <- function() {
  DTAColumnSpecCollection(
    columns = list(
      SEX = DTAColumnSpec(
        id = "SEX", type = "SAS Char", length = 20, nullable = FALSE,
        values = c("M", "F")
      ),
      AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    ),
    rules = list(
      DTARuleColRange(
        id = "age_range", columns = "AGE", range = c(18, 70),
        description = "Adults only"
      ),
      DTARuleColCondition(
        id = "f_not_12",
        condition = list(SEX = list(equals = "F")),
        then = list(AGE = list(not_equal = 12))
      )
    )
  )
}

em_dataset <- function(sex = c("M", "F"), age = c("30", "12"), extra = NULL) {
  path <- tempfile(fileext = ".csv")
  withr::defer(unlink(path), envir = parent.frame())
  df <- data.frame(SEX = sex, AGE = age, stringsAsFactors = FALSE)
  if (!is.null(extra)) df$EXTRA <- extra
  utils::write.csv(
    df,
    path,
    row.names = FALSE, na = ""
  )
  ds <- DTADataSetTabular(
    name = "em", specs = em_specs(),
    files = list(DTAFileCSV(filename = basename(path)))
  )
  ds <- load_file(ds, file = path, handler_index = 1, stream = "never")
  check(ds, persist = FALSE, quiet = TRUE)
}

test_that("an enum violation states the allowed values and the offending one", {
  ds <- check(create_example_DTADataSetTabular(2), quiet = TRUE)
  res <- explain_capture(ds)$result

  hit <- res[res$row == 1 & res$column == "STUDYID", ]
  expect_equal(nrow(hit), 1L)
  expect_equal(hit$should_be, "Must be one of: 1234, 5678.")
  expect_equal(hit$actual, "STUDY001")
})

test_that("an undeclared column is named as such, not as a missing value", {
  ds <- em_dataset(sex = c("M", "M"), age = c("30", "40"), extra = c("a", "b"))
  res <- explain_capture(ds)$result

  hit <- res[res$column %in% "EXTRA", ]
  expect_equal(nrow(hit), 1L)
  expect_equal(hit$should_be, "The specs declare no such column; it must not be present.")
  expect_equal(hit$actual, "(an undeclared column)")
})

test_that("rule messages use plain-text operators and show offending rows", {
  ds <- em_dataset()
  res_txt <- explain_capture(ds)
  res <- res_txt$result

  range_row <- res[res$source == "rule" & grepl("AGE", res$should_be) &
    grepl("between", res$should_be), ]
  expect_equal(range_row$should_be[[1]], "Values in AGE must be between 18 and 70 (inclusive).")
  # AGE 12 is the only value outside 18-70 in the data
  expect_match(range_row$actual[[1]], "12", fixed = TRUE)
  expect_match(res_txt$text, "Adults only", fixed = TRUE)

  cond_row <- res[res$source == "rule" & grepl("IF", res$should_be, fixed = TRUE), ]
  expect_equal(cond_row$should_be[[1]], "IF SEX = F THEN AGE != 12 must hold.")
  expect_false(any(grepl("&#|&gt;|&lt;", c(res$should_be, res$actual))))
})

test_that("data values are shown literally: no escaping and no cli evaluation", {
  ds <- em_dataset(sex = c("{Sys.time()}", "<b>&"), age = c("30", "40"))
  out <- explain_capture(ds)

  expect_true("{Sys.time()}" %in% out$result$actual)
  expect_true("<b>&" %in% out$result$actual)
  expect_match(out$text, "{Sys.time()}", fixed = TRUE)
  expect_match(out$text, "<b>&", fixed = TRUE)
})

test_that("id filters, n caps with a hint, and an unknown id errors", {
  ds <- check(create_example_DTADataSetTabular(2), quiet = TRUE)

  one <- explain_capture(ds, id = 2)
  expect_equal(one$result$id, 2L)

  capped <- explain_capture(ds, n = 2)
  expect_equal(nrow(capped$result), 2L)
  total <- nrow(messages(ds, as_tibble = FALSE))
  expect_match(capped$text, paste0("and ", total - 2, " more message"), fixed = TRUE)
  expect_match(capped$text, "n = Inf", fixed = TRUE)

  expect_error(explain_messages(ds, id = 9999), "not found")
  expect_error(explain_messages(ds, n = 0), "'n' must be")
})

test_that("a clean dataset reports that there is nothing to explain", {
  ds <- em_dataset(sex = c("M", "F"), age = c("30", "40"))
  out <- explain_capture(ds)
  expect_match(out$text, "No validation messages.", fixed = TRUE)
  expect_equal(nrow(out$result), 0L)
})

test_that("check() points to explain_messages() on failure, not on success", {
  bad <- NULL
  msgs_bad <- testthat::capture_messages(
    bad <- check(create_example_DTADataSetTabular(2))
  )
  expect_match(paste(msgs_bad, collapse = ""), "explain_messages", fixed = TRUE)

  msgs_dta <- testthat::capture_messages(check(create_example_DTA()))
  expect_match(paste(msgs_dta, collapse = ""), "explain_messages", fixed = TRUE)

  msgs_ok <- testthat::capture_messages(
    check(em_dataset(sex = c("M", "F"), age = c("30", "40")))
  )
  expect_false(grepl("explain_messages", paste(msgs_ok, collapse = ""), fixed = TRUE))
})

test_that("dta_find_rule_def resolves a rule on a dataset, and returns NULL for an unknown id", {
  ds <- em_dataset()
  expect_equal(dta_find_rule_def(ds, "em", "age_range")@id, "age_range")
  expect_null(dta_find_rule_def(ds, "em", "nope"))
})
