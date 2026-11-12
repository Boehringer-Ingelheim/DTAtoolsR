# The number of files a tabular dataset holds against the bounds its handlers
# declare. A tabular dataset keeps no file list of its own: each load_file()
# binds one table, so the supplied count is the number of tables. The bounds
# are the sum of the handlers' own (min_number_of_files / max_number_of_files).
# Only a DTA-level check() judges the count; a dataset checked alone does not.

fc_specs <- function() {
  DTAColumnSpecCollection(columns = list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE)
  ))
}

fc_frame <- function(prefix) {
  data.frame(ID = paste0(prefix, c("1", "2")), stringsAsFactors = FALSE)
}

# `n` tables named t1..tn behind one pattern handler with the given bounds.
fc_dataset <- function(id, n, ...) {
  tables <- stats::setNames(
    lapply(seq_len(n), function(i) fc_frame(paste0("T", i))),
    paste0("t", seq_len(n))
  )
  DTADataSetTabular(
    id = id,
    specs = fc_specs(),
    tables = tables,
    files = list(DTAFileCSV(filename = "x_.*[.]csv$", pattern = TRUE, ...))
  )
}

fc_dta <- function(...) {
  sets <- list(...)
  DTA(
    datasets = stats::setNames(sets, vapply(sets, function(d) d@id, character(1))),
    metadata = create_example_DTAMetaData()
  )
}

# check() with the file-count warnings collected rather than printed. Every
# other condition is left alone.
fc_check <- function(x, ...) {
  caught <- character(0)
  res <- withCallingHandlers(
    check(x, persist = FALSE, quiet = TRUE, ...),
    dta_file_count_mismatch = function(w) {
      caught <<- c(caught, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(res = res, warnings = caught)
}

test_that("an exact file count is not a finding", {
  expect_null(dta_tabular_file_count_finding(fc_dataset("d", 2, number_of_files = 2)))
})

test_that("too few files is a finding naming the dataset and both counts", {
  f <- dta_tabular_file_count_finding(fc_dataset("d", 1, number_of_files = 2))

  expect_identical(f$dataset, "d")
  expect_equal(f$n_supplied, 1)
  expect_equal(f$min, 2)
  expect_equal(f$max, 2)
  expect_match(f$message, "expects 2", fixed = TRUE)
  expect_match(f$message, "got 1", fixed = TRUE)
})

test_that("too many files is a finding naming the dataset and both counts", {
  f <- dta_tabular_file_count_finding(fc_dataset("d", 3, number_of_files = 2))

  expect_identical(f$dataset, "d")
  expect_equal(f$n_supplied, 3)
  expect_equal(f$min, 2)
  expect_equal(f$max, 2)
  expect_match(f$message, "expects 2", fixed = TRUE)
  expect_match(f$message, "got 3", fixed = TRUE)
})

test_that("a min/max range accepts its bounds and rejects what lies outside", {
  mk <- function(n) {
    fc_dataset("d", n, min_number_of_files = 2, max_number_of_files = 3)
  }

  expect_null(dta_tabular_file_count_finding(mk(2)))
  expect_null(dta_tabular_file_count_finding(mk(3)))

  below <- dta_tabular_file_count_finding(mk(1))
  expect_equal(c(below$n_supplied, below$min, below$max), c(1, 2, 3))
  expect_match(below$message, "between 2 and 3", fixed = TRUE)
  expect_match(below$message, "got 1", fixed = TRUE)

  above <- dta_tabular_file_count_finding(mk(4))
  expect_equal(c(above$n_supplied, above$min, above$max), c(4, 2, 3))
  expect_match(above$message, "got 4", fixed = TRUE)
})

test_that("an unbounded maximum reads 'at least' and never rejects a larger count", {
  mk <- function(n) fc_dataset("d", n, min_number_of_files = 3)

  expect_null(dta_tabular_file_count_finding(mk(3)))
  expect_null(dta_tabular_file_count_finding(mk(5)))

  f <- dta_tabular_file_count_finding(mk(2))
  expect_identical(f$max, Inf)
  expect_equal(f$min, 3)
  expect_match(f$message, "at least 3", fixed = TRUE)
  expect_match(f$message, "got 2", fixed = TRUE)
})

test_that("a dataset with no handlers, no tables or no tabular shape is not judged", {
  no_handlers <- DTADataSetTabular(
    id = "d", specs = fc_specs(),
    tables = list(t1 = fc_frame("A"), t2 = fc_frame("B"))
  )
  expect_null(dta_tabular_file_count_finding(no_handlers))

  no_tables <- DTADataSetTabular(
    id = "d", specs = fc_specs(),
    files = list(DTAFileCSV(filename = "x_.*[.]csv$", pattern = TRUE, number_of_files = 2))
  )
  expect_null(dta_tabular_file_count_finding(no_tables))

  path <- withr::local_tempfile(fileext = ".txt")
  writeLines("hello", path)
  expect_null(dta_tabular_file_count_finding(DTADataSetFile(id = "f", paths = path)))
})

test_that("a DTA check with a wrong count warns, fails and reports the dataset", {
  dta <- fc_dta(
    fc_dataset("d_bad", 3, number_of_files = 2),
    fc_dataset("d_ok", 2, number_of_files = 2)
  )

  out <- fc_check(dta)
  expect_length(out$warnings, 1)
  expect_match(out$warnings, "d_bad", fixed = TRUE)

  # The classed warning itself, as a caller would catch it.
  expect_warning(
    check(dta, persist = FALSE, quiet = TRUE),
    class = "dta_file_count_mismatch"
  )

  expect_false(attr(out$res, "last_validation_ok"))

  s <- attr(out$res, "last_validation_summary")
  expect_identical(
    names(s),
    c(
      "dataset", "n_targets", "n_validated", "n_valid", "n_invalid",
      "n_skipped", "n_unchecked", "n_undelivered", "n_file_count_errors",
      "n_import_errors"
    )
  )
  expect_equal(s$n_file_count_errors[s$dataset == "d_bad"], 1L)
  expect_equal(s$n_file_count_errors[s$dataset == "d_ok"], 0L)

  findings <- attr(out$res, "last_file_count_findings")
  expect_equal(nrow(findings), 1)
  expect_identical(findings$dataset, "d_bad")
  expect_equal(findings$n_supplied, 3)
  expect_equal(findings$min, 2)
  expect_equal(findings$max, 2)
})

test_that("a DTA check with correct counts is clean and its findings are empty", {
  dta <- fc_dta(
    fc_dataset("d_a", 2, number_of_files = 2),
    fc_dataset("d_b", 1, number_of_files = 1)
  )

  out <- fc_check(dta)
  expect_length(out$warnings, 0)
  expect_no_warning(check(dta, persist = FALSE, quiet = TRUE))
  expect_true(attr(out$res, "last_validation_ok"))
  expect_true(all(attr(out$res, "last_validation_summary")$n_file_count_errors == 0L))

  findings <- attr(out$res, "last_file_count_findings")
  expect_equal(nrow(findings), 0)
  expect_identical(
    names(findings),
    c("dataset", "n_supplied", "min", "max", "message")
  )
})

test_that("a dataset checked on its own is not judged on its file count", {
  ds <- fc_dataset("d_bad", 3, number_of_files = 2)

  caught <- character(0)
  res <- withCallingHandlers(
    check(ds, persist = FALSE, quiet = TRUE),
    dta_file_count_mismatch = function(w) {
      caught <<- c(caught, "x")
      invokeRestart("muffleWarning")
    }
  )

  expect_length(caught, 0)
  status <- validation_status(res)
  expect_equal(nrow(status), 3)
  expect_true(all(status$ok))
})

test_that("checking one dataset by name does not judge the others", {
  dta <- fc_dta(
    fc_dataset("d_bad", 3, number_of_files = 2),
    fc_dataset("d_ok", 2, number_of_files = 2)
  )

  out <- fc_check(dta, datasets = "d_ok")
  expect_length(out$warnings, 0)
  expect_equal(nrow(attr(out$res, "last_file_count_findings")), 0)
  expect_true(attr(out$res, "last_validation_ok"))
})

test_that("checking one dataset by name keeps another dataset's file-count failure", {
  dta <- fc_dta(
    fc_dataset("d_bad", 3, number_of_files = 2),
    fc_dataset("d_ok", 2, number_of_files = 2)
  )

  full <- fc_check(dta)$res
  expect_false(attr(full, "last_validation_ok"))

  sub <- fc_check(full, datasets = "d_ok")
  expect_length(sub$warnings, 0)
  findings <- attr(sub$res, "last_file_count_findings")
  expect_identical(findings$dataset, "d_bad")
  expect_false(attr(sub$res, "last_validation_ok"))

  # The report reads the same attribute, so its verdict must agree.
  html <- tempfile(fileext = ".html")
  write_validation_report(sub$res, html, overwrite = TRUE, quiet = TRUE)
  expect_true(any(grepl("got 3", readLines(html, warn = FALSE), fixed = TRUE)))

  # Re-checking the failing dataset replaces its row: fix it, and it clears.
  fixed <- full
  fixed@datasets[["d_bad"]] <- fc_dataset("d_bad", 2, number_of_files = 2)
  again <- fc_check(fixed, datasets = "d_bad")
  expect_equal(nrow(attr(again$res, "last_file_count_findings")), 0)
  expect_true(attr(again$res, "last_validation_ok"))

  # Fixed but NOT re-checked: the kept finding is judged again, not copied, so
  # a check of the other dataset does not report a count that no longer holds.
  other <- fc_check(fixed, datasets = "d_ok")
  expect_equal(nrow(attr(other$res, "last_file_count_findings")), 0)
})

test_that("eager and streamed loading report the same file-count findings", {
  dir <- withr::local_tempdir()
  for (nm in c("x_a", "x_b", "x_c")) {
    utils::write.csv(fc_frame(nm), file.path(dir, paste0(nm, ".csv")), row.names = FALSE)
  }

  # Too many (3 for 2) and too few (3 for 4) alike.
  for (bound in c(2, 4)) {
    loaded <- function(stream) {
      ds <- DTADataSetTabular(
        id = "d", specs = fc_specs(),
        files = list(DTAFileCSV(filename = "x_.*[.]csv$", pattern = TRUE, number_of_files = bound))
      )
      dta <- fc_dta(ds)
      for (nm in c("x_a", "x_b", "x_c")) {
        dta <- load_file(
          dta,
          dataset = "d", file = file.path(dir, paste0(nm, ".csv")),
          name = nm, stream = stream
        )
      }
      dta
    }

    eager_dta <- loaded("never")
    streamed_dta <- loaded("always")

    # The two holdings differ in kind, which is what makes the parity worth
    # asserting: the count must not depend on it.
    expect_true(all(vapply(
      eager_dta@datasets$d@tables, function(t) !inherits(t, "Dataset"), logical(1)
    )))
    expect_true(all(vapply(
      streamed_dta@datasets$d@tables, function(t) inherits(t, "Dataset"), logical(1)
    )))

    eager <- attr(fc_check(eager_dta)$res, "last_file_count_findings")
    streamed <- attr(fc_check(streamed_dta)$res, "last_file_count_findings")

    expect_equal(nrow(eager), 1)
    expect_equal(eager$n_supplied, 3)
    expect_equal(eager$min, bound)
    expect_identical(eager, streamed)
  }
})

test_that("the bound is the SUM over all handlers, not a per-handler limit", {
  mk <- function(n) {
    tables <- stats::setNames(
      lapply(seq_len(n), function(i) fc_frame(paste0("T", i))),
      paste0("t", seq_len(n))
    )
    DTADataSetTabular(
      id = "d", specs = fc_specs(), tables = tables,
      files = list(
        DTAFileCSV(filename = "a_.*[.]csv$", pattern = TRUE, number_of_files = 1),
        DTAFileCSV(filename = "b_.*[.]csv$", pattern = TRUE, number_of_files = 1)
      )
    )
  }

  f <- dta_tabular_file_count_finding(mk(3))
  expect_equal(c(f$n_supplied, f$min, f$max), c(3, 2, 2))
  expect_null(dta_tabular_file_count_finding(mk(2)))

  expect_equal(nrow(attr(fc_check(fc_dta(mk(3)))$res, "last_file_count_findings")), 1)
  expect_equal(nrow(attr(fc_check(fc_dta(mk(2)))$res, "last_file_count_findings")), 0)
})

test_that("an NA bound never reaches check(): the handler refuses it", {
  # An NA bound used to be accepted and silently meant "no bound". "No bound"
  # is NULL; NA is now refused where the handler is built.
  expect_error(
    DTAFileCSV(
      filename = "x_.*[.]csv$", pattern = TRUE,
      min_number_of_files = NA_real_, max_number_of_files = NA_real_
    ),
    class = "dta_file_count_bound_invalid"
  )
})

test_that("the report verdict and message table carry a file-count failure", {
  bad <- fc_check(fc_dta(fc_dataset("d_bad", 3, number_of_files = 2)))$res
  ok <- fc_check(fc_dta(fc_dataset("d_ok", 2, number_of_files = 2)))$res
  expect_false(attr(bad, "last_validation_ok"))

  render <- function(x) {
    out <- withr::local_tempfile(fileext = ".html", .local_envir = parent.frame())
    write_validation_report(x, out, overwrite = TRUE, quiet = TRUE)
    paste(readLines(out, warn = FALSE), collapse = "\n")
  }

  html <- render(bad)
  # Overview row: failed, and counted on the Failed card.
  expect_match(
    html, 'data-dataset="d_bad" data-target="file count" data-status="fail"',
    fixed = TRUE
  )
  expect_match(
    html, '<span class="summary-count">1</span><span class="summary-label">Failed</span>',
    fixed = TRUE
  )
  # Messages table: the finding's own text.
  expect_match(html, 'data-source="file_count"', fixed = TRUE)
  expect_match(html, "got 3", fixed = TRUE)

  clean <- render(ok)
  expect_false(grepl("file count", clean, fixed = TRUE))
})

test_that("the report helpers leave a clean or unchecked DTA untouched", {
  dta <- fc_dta(fc_dataset("d_ok", 2, number_of_files = 2))
  res <- data.frame(
    dataset = "d_ok", target = "t1", target_type = "table", status = "validated",
    n_columnspec_errors = 0L, n_rule_errors = 0L, n_import_errors = 0L,
    stringsAsFactors = FALSE
  )
  expect_identical(.report_with_file_count_rows(dta, res), res)
  expect_identical(.report_with_file_count_messages(dta, dta_empty_messages()), dta_empty_messages())
})

test_that("the dataset summary line never reads all-valid over a count failure", {
  clean <- dta_dataset_summary_message(2, 2, 2, 0, 0)
  expect_identical(clean$severity, "success")

  count_only <- dta_dataset_summary_message(2, 2, 2, 0, 0, n_count_errors = 1L)
  expect_identical(count_only$severity, "danger")
  expect_match(count_only$text, "file count outside the declared bounds", fixed = TRUE)

  alongside <- dta_dataset_summary_message(3, 3, 2, 1, 0, n_count_errors = 1L)
  expect_identical(alongside$severity, "danger")
  expect_match(alongside$text, "1 INVALID", fixed = TRUE)
  expect_match(alongside$text, "file count outside the declared bounds", fixed = TRUE)

  unchecked <- dta_dataset_summary_message(3, 2, 2, 0, 1, n_count_errors = 1L)
  expect_identical(unchecked$severity, "warning")
  expect_match(unchecked$text, "not checked", fixed = TRUE)
  expect_match(unchecked$text, "file count outside the declared bounds", fixed = TRUE)
})

test_that("the overall line reports a count failure alongside other failures", {
  with_invalid <- dta_overall_summary_message(2, 0, 0, 0, total_count_errors = 1L)
  expect_identical(with_invalid$severity, "danger")
  expect_match(with_invalid$text, "2 tables with validation errors", fixed = TRUE)
  expect_match(with_invalid$text, "1 dataset with a file count", fixed = TRUE)

  with_import <- dta_overall_summary_message(0, 3, 0, 0, total_count_errors = 2L)
  expect_match(with_import$text, "could not be imported", fixed = TRUE)
  expect_match(with_import$text, "2 datasets with a file count", fixed = TRUE)

  no_count <- dta_overall_summary_message(2, 0, 0, 0)
  expect_false(grepl("file count", no_count$text, fixed = TRUE))
})

test_that("a file-count failure alone makes the overall message a failure", {
  msg <- dta_overall_summary_message(
    total_invalid = 0,
    total_import_errors = 0,
    total_unchecked = 0,
    total_undelivered = 0,
    total_count_errors = 1L
  )

  expect_identical(msg$severity, "danger")
  expect_match(msg$text, "file count", fixed = TRUE)
  expect_match(msg$text, "FAILED", fixed = TRUE)
  expect_false(grepl("PASSED", msg$text, fixed = TRUE))
})
