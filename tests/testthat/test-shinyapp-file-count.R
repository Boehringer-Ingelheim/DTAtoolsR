# The app must not read a DTA-level file-count failure as a pass.
#
# check(<DTA>) fails a run whose tabular dataset holds a table count outside its
# declared bounds. It signals the classed warning `dta_file_count_mismatch` and
# records the finding on the returned object (attr `last_file_count_findings`,
# `last_validation_ok`). The per-table results() are untouched, so a dataset whose
# tables are all valid still looks clean there. These tests pin that the app
# folds the finding into its status map, report and run_check.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

# Fixtures: same construction as test-DTADataSetTabularFileCount.R. `n` valid
# tables behind one pattern handler with the given bounds.
fcs_specs <- function() {
  DTAtools::DTAColumnSpecCollection(columns = list(
    ID = DTAtools::DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE)
  ))
}

fcs_dataset <- function(id, n, ...) {
  tables <- stats::setNames(
    lapply(seq_len(n), function(i) {
      data.frame(ID = paste0("T", i, c("1", "2")), stringsAsFactors = FALSE)
    }),
    paste0("t", seq_len(n))
  )
  DTAtools::DTADataSetTabular(
    id = id,
    specs = fcs_specs(),
    tables = tables,
    files = list(DTAtools::DTAFileCSV(filename = "x_.*[.]csv$", pattern = TRUE, ...))
  )
}

fcs_dta <- function(...) {
  sets <- list(...)
  DTAtools::DTA(
    datasets = stats::setNames(sets, vapply(sets, function(d) d@id, character(1))),
    metadata = DTAtools::create_example_DTAMetaData()
  )
}

# One dataset needing exactly 2 tables but holding 3 (all valid).
fcs_bad_dta <- function() fcs_dta(fcs_dataset("d_bad", 3, number_of_files = 2))

# Same shape, within bounds.
fcs_ok_dta <- function() fcs_dta(fcs_dataset("d_ok", 2, number_of_files = 2))

# Two datasets: d_bad violates its bounds, d_ok does not.
fcs_two_dta <- function() {
  fcs_dta(
    fcs_dataset("d_bad", 3, number_of_files = 2),
    fcs_dataset("d_ok", 2, number_of_files = 2)
  )
}

test_that("dta_check keeps a count violation as a failed status instead of a pass", {
  dta_check <- app_fn("dta_check")
  dta_status_map <- app_fn("dta_status_map")
  dta_file_count_messages <- app_fn("dta_file_count_messages")

  # Neither the warning nor an error may escape: the run completes.
  expect_no_warning(res <- dta_check(fcs_bad_dta()))
  expect_true(res$ok)

  expect_gt(length(dta_file_count_messages(res$value)), 0)
  expect_match(dta_file_count_messages(res$value), "d_bad", fixed = TRUE, all = FALSE)

  status <- dta_status_map(res$value)
  expect_false(identical(unname(status[["d_bad"]]), "pass"))
  expect_identical(unname(status[["d_bad"]]), "fail")
})

test_that("a DTA within its file-count bounds still passes with no messages", {
  dta_check <- app_fn("dta_check")

  res <- dta_check(fcs_ok_dta())
  expect_true(res$ok)
  expect_identical(unname(app_fn("dta_status_map")(res$value)[["d_ok"]]), "pass")
  expect_identical(app_fn("dta_file_count_messages")(res$value), character(0))
})

test_that("a per-dataset re-check does not clear another dataset's count failure", {
  dta_check <- app_fn("dta_check")
  dta_status_map <- app_fn("dta_status_map")

  a <- dta_check(fcs_two_dta(), "d_bad")
  expect_true(a$ok)
  expect_identical(unname(dta_status_map(a$value)[["d_bad"]]), "fail")

  b <- dta_check(a$value, "d_ok")
  expect_true(b$ok)
  status <- dta_status_map(b$value)
  expect_identical(unname(status[["d_bad"]]), "fail")
  expect_identical(unname(status[["d_ok"]]), "pass")
  expect_match(
    app_fn("dta_file_count_messages")(b$value), "d_bad",
    fixed = TRUE, all = FALSE
  )
})

test_that("a finding for a dataset that no longer exists is not reported", {
  stale <- attr(app_fn("dta_check")(fcs_bad_dta())$value, "last_file_count_findings")
  dta <- app_fn("dta_check")(fcs_ok_dta())$value
  attr(dta, "last_file_count_findings") <- stale
  expect_null(app_fn("dta_file_count_findings")(dta))
  expect_identical(unname(app_fn("dta_status_map")(dta)[["d_ok"]]), "pass")
})

test_that("messages can be limited to the datasets of the current run", {
  dta <- app_fn("dta_check")(fcs_two_dta(), "d_bad")$value
  msgs <- app_fn("dta_file_count_messages")
  expect_length(msgs(dta, datasets = "d_bad"), 1)
  expect_identical(msgs(dta, datasets = "d_ok"), character(0))
})

test_that("dta_check only swallows the file-count warning; others propagate", {
  skip_if_not_installed("testthat", "3.2.0")
  dta <- fcs_ok_dta()

  testthat::local_mocked_bindings(
    check = function(...) {
      warning(structure(
        class = c("my_other_warning", "warning", "condition"),
        list(message = "unrelated", call = NULL)
      ))
      dta
    },
    .package = "DTAtools"
  )

  dta_check <- app_fn("dta_check")
  expect_warning(dta_check(dta), class = "my_other_warning")
})

test_that("the validation report fails and names a file-count failure", {
  dta_check <- app_fn("dta_check")
  build <- app_fn("dta_build_validation_report")

  dta <- dta_check(fcs_bad_dta())$value
  html <- build(dta, status = app_fn("dta_status_map")(dta))

  expect_match(html, "VALIDATION FAILED", fixed = TRUE)
  expect_false(grepl("VALIDATION PASSED", html, fixed = TRUE))
  expect_match(html, "File count", fixed = TRUE)
  expect_match(html, "d_bad", fixed = TRUE)
})

test_that("a file-count message is HTML-escaped in the report", {
  dta <- app_fn("dta_check")(fcs_bad_dta())$value
  attr(dta, "last_file_count_findings") <- data.frame(
    dataset = "d_bad", n_supplied = 3, min = 2, max = 2,
    message = "a <b>x</b> & y",
    stringsAsFactors = FALSE
  )

  html <- app_fn("dta_build_validation_report")(dta, status = app_fn("dta_status_map")(dta))
  expect_match(html, "&lt;b&gt;x&lt;/b&gt;", fixed = TRUE)
  expect_false(grepl("<b>x</b>", html, fixed = TRUE))
})

test_that("the report banner never reads PASSED over a recorded count failure", {
  dta <- app_fn("dta_check")(fcs_ok_dta())$value
  attr(dta, "last_validation_ok") <- FALSE
  attr(dta, "last_file_count_findings") <- data.frame(
    dataset = "d_ok", n_supplied = 1, min = 2, max = 2,
    message = "d_ok expects 2 files, got 1",
    stringsAsFactors = FALSE
  )

  # A status map that has drifted back to the old, blind behaviour.
  html <- app_fn("dta_build_validation_report")(dta, status = list(d_ok = "pass"))
  expect_false(grepl("VALIDATION PASSED", html, fixed = TRUE))
})

test_that("check_all marks a dataset with a count violation as fail", {
  # Status only: the completion toast is not observable through testServer.
  shiny::testServer(.shiny_app_dir(), {
    rv$dta <- fcs_bad_dta()
    session$setInputs(check_all = 1)

    expect_false(identical(unname(rv$status[["d_bad"]]), "pass"))
    expect_identical(unname(rv$status[["d_bad"]]), "fail")
  })
})

# Regression (round-3 review): the app merged the prior findings in itself
# after check() had started keeping them, so every per-dataset Validate added
# another copy of the other datasets' rows.
test_that("repeated per-dataset checks keep one finding per failing dataset", {
  dta_check <- app_fn("dta_check")
  three <- fcs_dta(
    fcs_dataset("d_a", 3, number_of_files = 2),
    fcs_dataset("d_b", 3, number_of_files = 2),
    fcs_dataset("d_c", 3, number_of_files = 2)
  )
  res <- dta_check(three)
  for (i in 1:3) res <- dta_check(res$value, "d_a")
  findings <- attr(res$value, "last_file_count_findings", exact = TRUE)
  expect_setequal(findings$dataset, c("d_a", "d_b", "d_c"))
  expect_identical(nrow(findings), 3L)
})
