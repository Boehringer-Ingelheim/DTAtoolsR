# File count against the handlers' bounds. REQ-VAL-033 .. REQ-VAL-037.
#
# Every expected number below is derived from the specification sentence: the
# bounds are the dataset-wide SUM of the handlers' minima and maxima, a missing
# bound is no bound, and only a DTA-level check judges the count.

vf_handler <- function(...) DTAFileCSV(filename = "x_.*[.]csv$", pattern = TRUE, ...)

vf_dataset <- function(id, n, handlers) {
  tables <- stats::setNames(
    lapply(seq_len(n), function(i) {
      data.frame(ID = paste0("T", i, c("1", "2")), stringsAsFactors = FALSE)
    }),
    paste0("t", seq_len(n))
  )
  specs <- DTAColumnSpecCollection(columns = list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE)
  ))
  DTADataSetTabular(id = id, specs = specs, files = handlers, tables = tables)
}

# check() of a DTA of one dataset: the result's relevant attributes and the
# number of dta_file_count_mismatch warnings raised.
vf_check <- function(ds) {
  n_warn <- 0L
  dta <- DTA(
    datasets = stats::setNames(list(ds), ds@id),
    metadata = DTAMetaData(title = "File count", version = "1.0")
  )
  res <- withCallingHandlers(
    check(dta, persist = FALSE, quiet = TRUE),
    dta_file_count_mismatch = function(w) {
      n_warn <<- n_warn + 1L
      invokeRestart("muffleWarning")
    }
  )
  list(
    n_warn = n_warn,
    ok = attr(res, "last_validation_ok"),
    n_err = attr(res, "last_validation_summary")$n_file_count_errors,
    found = attr(res, "last_file_count_findings")
  )
}

test_that("OQ-VAL-046 | too few and too many tables are each reported with the supplied count and the bounds | REQ-VAL-033", {
  handlers <- list(vf_handler(min_number_of_files = 2, max_number_of_files = 3))

  few <- vf_check(vf_dataset("few", 1, handlers))
  qa_step("one table against 2..3 raises one dta_file_count_mismatch warning", 1L, few$n_warn)
  qa_step("the run is not valid", FALSE, few$ok)
  qa_step("the summary counts one file-count error", 1L, few$n_err)
  qa_step("the finding lists the dataset", "few", few$found$dataset)
  qa_step(
    "with supplied count 1 and bounds 2..3", c(1, 2, 3),
    c(few$found$n_supplied, few$found$min, few$found$max)
  )

  many <- vf_check(vf_dataset("many", 4, handlers))
  qa_step("four tables against 2..3 raise one warning", 1L, many$n_warn)
  qa_step("that run is not valid either", FALSE, many$ok)
  qa_step("the summary counts one file-count error", 1L, many$n_err)
  qa_step("the finding lists the dataset", "many", many$found$dataset)
  qa_step(
    "with supplied count 4 and bounds 2..3", c(4, 2, 3),
    c(many$found$n_supplied, many$found$min, many$found$max)
  )
})

test_that("OQ-VAL-047 | a table count equal to either bound, or between them, is not a violation | REQ-VAL-034", {
  handlers <- list(vf_handler(min_number_of_files = 2, max_number_of_files = 4))

  for (n in 2:4) {
    r <- vf_check(vf_dataset("d", n, handlers))
    lab <- sprintf("%d tables against 2..4", n)
    qa_step(paste(lab, "raise no warning"), 0L, r$n_warn)
    qa_step(paste(lab, "count no file-count error"), 0L, r$n_err)
    qa_step(paste(lab, "leave the findings empty"), 0L, nrow(r$found))
    qa_check(paste(lab, "leave the run valid"), isTRUE(r$ok))
  }

  # Just outside on each side, so the boundary is shown to lie exactly there.
  qa_step("one table (min - 1) is a violation", 1L, vf_check(vf_dataset("d", 1, handlers))$n_warn)
  qa_step("five tables (max + 1) are a violation", 1L, vf_check(vf_dataset("d", 5, handlers))$n_warn)
})

test_that("OQ-VAL-048 | a handler without a maximum is unbounded above and one without a minimum is unbounded below | REQ-VAL-035", {
  only_min <- list(vf_handler(min_number_of_files = 2))
  big <- vf_check(vf_dataset("d", 6, only_min))
  qa_step("six tables against a minimum of 2 and no maximum raise no warning", 0L, big$n_warn)
  qa_check("and the run is valid", isTRUE(big$ok))
  small <- vf_check(vf_dataset("d", 1, only_min))
  qa_step("one table is still below the minimum", 1L, small$n_warn)
  qa_step("with supplied count 1 and minimum 2", c(1, 2), c(small$found$n_supplied, small$found$min))
  qa_check("and the unbounded maximum is reported as infinite", is.infinite(small$found$max))

  only_max <- list(vf_handler(max_number_of_files = 3))
  one <- vf_check(vf_dataset("d", 1, only_max))
  qa_step("one table against a maximum of 3 and no minimum raises no warning", 0L, one$n_warn)
  qa_check("and the run is valid", isTRUE(one$ok))
  qa_step("four tables still exceed the maximum", 1L, vf_check(vf_dataset("d", 4, only_max))$n_warn)
})

test_that("OQ-VAL-049 | the bounds are summed over all handlers, not applied per handler | REQ-VAL-036", {
  two <- list(
    vf_handler(min_number_of_files = 1, max_number_of_files = 1),
    vf_handler(min_number_of_files = 1, max_number_of_files = 1)
  )
  within <- vf_check(vf_dataset("d", 2, two))
  qa_step("two tables against two handlers of exactly one are within bounds", 0L, within$n_warn)
  qa_check("and the run is valid", isTRUE(within$ok))
  over <- vf_check(vf_dataset("d", 3, two))
  qa_step("three tables exceed the summed maximum of 2", 1L, over$n_warn)
  qa_step(
    "reported with supplied count 3 and summed bounds 2..2", c(3, 2, 2),
    c(over$found$n_supplied, over$found$min, over$found$max)
  )
  under <- vf_check(vf_dataset("d", 1, two))
  qa_step(
    "one table is below the summed minimum of 2", c(1, 2, 2),
    c(under$found$n_supplied, under$found$min, under$found$max)
  )

  open <- list(
    vf_handler(min_number_of_files = 1, max_number_of_files = 1),
    vf_handler(min_number_of_files = 1)
  )
  qa_step(
    "a handler without a maximum makes the summed maximum unbounded",
    0L, vf_check(vf_dataset("d", 7, open))$n_warn
  )
  r <- vf_check(vf_dataset("d", 1, open))
  qa_step(
    "while the summed minimum of 2 still applies", c(1, 2),
    c(r$found$n_supplied, r$found$min)
  )
})

test_that("OQ-VAL-050 | a dataset checked on its own is not judged on its table count | REQ-VAL-037", {
  ds <- vf_dataset("d", 3, list(vf_handler(number_of_files = 2)))

  # Control: inside a DTA the same dataset is a violation.
  qa_step("inside a DTA the dataset fails on its count", 1L, vf_check(ds)$n_warn)

  n_warn <- 0L
  res <- withCallingHandlers(
    check(ds, persist = FALSE, quiet = TRUE),
    dta_file_count_mismatch = function(w) {
      n_warn <<- n_warn + 1L
      invokeRestart("muffleWarning")
    }
  )
  qa_step("checked alone it raises no file-count warning", 0L, n_warn)
  status <- validation_status(res)
  qa_step("and all three tables are validated", 3L, nrow(status))
  qa_check("with every table valid", all(status$ok))
})
