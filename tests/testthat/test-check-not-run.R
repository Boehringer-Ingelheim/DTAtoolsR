# A check that cannot run is an error, never a pass.
#
# The defect these pin: a column whose schema could not be built lost EVERY one
# of its value checks behind a single warning, and the table was reported valid
# on data nobody had looked at. The verdict was computed from the findings that
# were produced, so a check that produced none because it never ran looked
# exactly like a check that ran and found nothing.
#
# Every test here asserts the VERDICT and the finding, never only the warning:
# deleting the `cli_warn()` must not be able to save any of them. The warning
# stays as a hint at the cause and has one test of its own, last.
#
# Expected values are derived from the specification by hand, not read off the
# functions under test. A column that cannot be compiled is reachable only by
# putting a bare base `DTAColumnSpecStructure` on a column, which no public
# constructor does, so the tests assign one directly (the `@<-` route is
# deliberately left open: it is the honest stand-in for "some schema method
# fails").

# A well-formed column and one whose structure cannot say how it validates.
nr_specs <- function() {
  bad <- DTAColumnSpec(id = "BAD", type = "SAS Char")
  bad@structure <- DTAColumnSpecStructure(backend = "SAS")
  DTAColumnSpecCollection(columns = list(
    SEX = DTAColumnSpec(
      id = "SEX", type = "SAS Char", length = 1, nullable = FALSE, values = c("M", "F")
    ),
    BAD = bad
  ))
}

# SEX holds one value outside its list (row 2); BAD holds values that a working
# check would have examined.
nr_table <- function() {
  data.frame(
    SEX = c("M", "X"),
    BAD = c("anything", NA),
    stringsAsFactors = FALSE
  )
}

nr_keywords <- function(details) {
  as.character(details$columnspec_errors$full_error$keyword)
}

test_that("a column whose schema cannot be built makes the table invalid, not valid", {
  details <- suppressWarnings(
    validate_table_detailed(nr_specs(), nr_table(), verbose = FALSE)
  )

  expect_false(details$ok)
  expect_false(details$columnspec_valid)
  # One finding for the column that was not checked, one for SEX's value outside
  # its list: nothing is hidden behind the other.
  expect_identical(details$n_columnspec_errors, 2L)
  expect_identical(nr_keywords(details), c("dta_check_not_run", "enum"))

  not_run <- details$columnspec_errors$full_error[1, ]
  expect_identical(not_run$column, "BAD")
  expect_true(is.na(not_run$row))
})

test_that("a column that cannot be checked is invalid even when it is the only column", {
  bad <- DTAColumnSpec(id = "BAD", type = "SAS Char")
  bad@structure <- DTAColumnSpecStructure(backend = "SAS")
  specs <- DTAColumnSpecCollection(columns = list(BAD = bad))

  details <- suppressWarnings(validate_table_detailed(
    specs, data.frame(BAD = c("a", "b"), stringsAsFactors = FALSE),
    verbose = FALSE
  ))

  expect_false(details$ok)
  expect_identical(details$n_columnspec_errors, 1L)
  expect_identical(nr_keywords(details), "dta_check_not_run")
})

test_that("the per-check report lists the check that did not run as failed", {
  details <- suppressWarnings(
    validate_table_detailed(nr_specs(), nr_table(), verbose = FALSE)
  )
  checks <- details$columnspec_checks

  expect_identical(checks$status[match("dta_check_not_run", checks$keyword)], "failed")
  expect_identical(checks$failed_columns[match("dta_check_not_run", checks$keyword)], "BAD")
  expect_identical(checks$n_errors[match("dta_check_not_run", checks$keyword)], 1L)
})

test_that("the console never says the column spec checks passed when one did not run", {
  # SEX holds only values from its list here, so the checks that DID run have
  # nothing to fail and the closing line would read "All column spec checks
  # passed" if the column that was not checked were left out of the report.
  clean <- data.frame(SEX = c("M", "F"), BAD = c("a", NA), stringsAsFactors = FALSE)
  out <- character(0)
  suppressWarnings(out <- gsub(
    "\\s+", " ",
    paste(testthat::capture_messages(
      validate_table_detailed(nr_specs(), clean, verbose = TRUE)
    ), collapse = " ")
  ))

  expect_false(grepl("All column spec checks passed", out, fixed = TRUE))
  expect_match(out, "Checks not run", fixed = TRUE)
  # Applicable checks, by hand: presence, extra columns, format (SEX declares a
  # type), length, values and the one that was not run; the pattern check is not
  # declared by any column. One of the six failed.
  expect_match(out, "1 of 6 column spec checks failed", fixed = TRUE)
})

test_that("a table with a column that was not checked is never reported as valid by validate_table()", {
  # validate_table() returns the table itself only when everything passed; with
  # a column spec error it returns the error list, which carries the finding.
  res <- suppressWarnings(validate_table(nr_specs(), nr_table(), verbose = FALSE))

  expect_false(is.data.frame(res))
  expect_true("dta_check_not_run" %in% as.character(res$full_error$keyword))
})

test_that("the streaming engine reaches the same verdict, once, at any batch size", {
  expected <- suppressWarnings(
    validate_table_detailed(nr_specs(), nr_table(), verbose = FALSE)
  )

  for (batch_rows in c(1L, 2L, 100L)) {
    reader <- dta_as_batch_reader(arrow::as_arrow_table(nr_table()), batch_rows = batch_rows)
    streamed <- suppressWarnings(dta_validate_table_stream(
      nr_specs(), reader,
      verbose = FALSE, coerce = FALSE, known_columns = names(nr_table())
    ))

    info <- paste0("batch_rows = ", batch_rows)
    expect_false(streamed$ok, info = info)
    # Once for the scan, not once per batch.
    expect_identical(streamed$n_columnspec_errors, 2L, info = info)
    expect_identical(
      as.character(streamed$columnspec_errors$full_error$keyword),
      c("dta_check_not_run", "enum"),
      info = info
    )
    expect_equal(
      streamed$columnspec_errors$full_error,
      expected$columnspec_errors$full_error,
      info = info
    )
  }
})

test_that("the finding survives the retention cap on both paths", {
  # The cap bounds what is KEPT; the not-run finding is a fact about the
  # specification and must be among the rows that are kept.
  many <- data.frame(
    SEX = rep("X", 50),
    BAD = rep("a", 50),
    stringsAsFactors = FALSE
  )
  eager <- suppressWarnings(
    validate_table_detailed(nr_specs(), many, verbose = FALSE, max_errors = 3)
  )
  streamed <- suppressWarnings(dta_validate_table_stream(
    nr_specs(),
    dta_as_batch_reader(arrow::as_arrow_table(many), batch_rows = 7L),
    verbose = FALSE, coerce = FALSE, known_columns = names(many), max_errors = 3
  ))

  for (details in list(eager, streamed)) {
    expect_false(details$ok)
    expect_identical(details$columnspec_errors$full_error$keyword[[1]], "dta_check_not_run")
    # Exact count: 50 enum violations plus the one finding.
    expect_identical(details$n_columnspec_errors, 51L)
  }
})

test_that("a missing required column does not hide the column that was not checked", {
  details <- suppressWarnings(validate_table_detailed(
    nr_specs(), data.frame(BAD = "a", stringsAsFactors = FALSE),
    verbose = FALSE
  ))

  expect_false(details$ok)
  summary_keywords <- as.character(details$columnspec_errors$summarised_error$keyword)
  expect_true("required" %in% summary_keywords)
  expect_true("dta_check_not_run" %in% summary_keywords)
})

test_that("check() fails the table and says why in the readers", {
  ds <- DTADataSetTabular(id = "nr", specs = nr_specs(), tables = list(tab = nr_table()))
  ds <- suppressWarnings(suppressMessages(check(ds, persist = FALSE, quiet = TRUE)))

  status <- validation_status(ds)
  expect_identical(status$ok, FALSE)
  expect_identical(status$n_columnspec_errors, 2L)

  msgs <- messages(ds, as_tibble = FALSE)
  expect_true("dta_check_not_run" %in% as.character(msgs$keyword))

  # explain_messages() and inspect() have to cope with the new keyword: a reader
  # that aborts on it would hide the finding exactly where a person looks.
  explained <- suppressMessages(explain_messages(ds, n = Inf))
  row <- explained[explained$column == "BAD" & !is.na(explained$column), , drop = FALSE]
  expect_identical(nrow(row), 1L)
  expect_match(row$actual, "no check was run", fixed = TRUE)
  expect_false(grepl("missing / not present", row$actual, fixed = TRUE))

  id <- msgs$id[as.character(msgs$keyword) == "dta_check_not_run"]
  detail <- inspect(ds, id = id, as_tibble = FALSE)
  expect_s3_class(detail, "data.frame")
  # One finding is one record; it must not vanish or fan out.
  expect_identical(nrow(detail), 1L)
})

test_that("the HTML report carries the finding instead of dropping it", {
  ds <- DTADataSetTabular(id = "nr", specs = nr_specs(), tables = list(tab = nr_table()))
  ds <- suppressWarnings(suppressMessages(check(ds, persist = FALSE, quiet = TRUE)))
  dta <- DTA(datasets = list(ds))
  path <- withr::local_tempfile(fileext = ".html")

  suppressWarnings(suppressMessages(write_validation_report(dta, path)))
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")

  # The phrase is the report's own wording for this finding and appears nowhere
  # else, so its presence is the finding being carried (the column name alone
  # would be matched by the specification table).
  expect_match(html, "no check was run on this column", fixed = TRUE)
})

test_that("a bare backend is refused where the column is built, not skipped at check time", {
  # `type: SAS` names a backend and no type. It used to build, lint clean and
  # then lose every value check of the column at check time.
  expect_error(
    DTAColumnSpec(id = "ARM", type = "SAS", nullable = FALSE, values = c("A", "B")),
    class = "dta_spec_bare_backend"
  )
  # A length alone does not make it a type.
  expect_error(
    DTAColumnSpec(id = "ARM", type = "SAS", length = 3),
    class = "dta_spec_bare_backend"
  )

  # A type, or a format that implies one, is still all it takes.
  expect_identical(DTAColumnSpec(id = "A", type = "SAS Char")@structure@type, "Char")
  expect_identical(DTAColumnSpec(id = "A", format = "SAS $5.")@structure@type, "Char")
})

test_that("lint_dta() reports the bare backend as an error, naming the column", {
  out <- lint_dta(list(columns = list(
    list(id = "ARM", type = "SAS", nullable = FALSE, values = c("A", "B")),
    list(id = "SEX", type = "SAS Char", length = 1)
  )))

  expect_identical(nrow(out), 1L)
  expect_identical(out$severity, "error")
  expect_identical(out$code, "column_spec_invalid")
  expect_identical(out$id, "ARM")
})

test_that("a bare backend in a YAML specification does not load", {
  # The bundled specification with ONE column's type cut down to the backend.
  lines <- readLines(
    system.file("extdata", "clinical_dta.yaml", package = "DTAtools"),
    warn = FALSE
  )
  at <- grep("^\\s+type: SAS Char\\s*$", lines)[[1]]
  lines[[at]] <- sub("SAS Char", "SAS", lines[[at]], fixed = TRUE)
  path <- withr::local_tempfile(fileext = ".yaml")
  writeLines(lines, path)

  err <- tryCatch(read_dta_from_yaml(path), error = function(e) e)

  expect_s3_class(err, "dta_lint_error")
  expect_identical(err$findings$code, "column_spec_invalid")
  expect_identical(err$findings$severity, "error")
})

test_that("the warning that names the cause is still raised, once per scan", {
  # Only a hint: the verdict tests above hold with the warning suppressed.
  count_warnings <- function(expr) {
    n <- 0L
    withCallingHandlers(
      expr,
      dta_columnspec_schema_failed = function(w) {
        n <<- n + 1L
        invokeRestart("muffleWarning")
      }
    )
    n
  }

  expect_identical(
    count_warnings(validate_table_detailed(nr_specs(), nr_table(), verbose = FALSE)),
    1L
  )
  # A streamed scan of two batches compiles the schemas once, not once per batch.
  expect_identical(
    count_warnings(dta_validate_table_stream(
      nr_specs(),
      dta_as_batch_reader(arrow::as_arrow_table(nr_table()), batch_rows = 1L),
      verbose = FALSE, coerce = FALSE, known_columns = names(nr_table())
    )),
    1L
  )
})


# ---- a table with a header and no rows ---------------------------------------
#
# Its verdict is a deliberate one and is kept: an empty table is a legitimate
# result -- an analysis that yielded nothing -- so `ok` stays TRUE. What changes
# is what the console says. "1 table passed validation" certified a table whose
# every value check was `not_checked`; the line now says the table had 0 rows
# and that no value check was run (LIM-037).

nr_id_specs <- function() {
  DTAColumnSpecCollection(columns = list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
}

nr_console <- function(expr) {
  gsub("[[:space:]]+", " ", paste(testthat::capture_messages(expr), collapse = " "))
}

nr_ds <- function(dir, files, ..., stream = FALSE) {
  ds <- DTADataSetTabular(
    id = "d", specs = nr_id_specs(),
    files = lapply(names(files), function(f) DTAFileCSV(filename = f))
  )
  for (i in seq_along(files)) {
    path <- file.path(dir, names(files)[[i]])
    writeLines(files[[i]], path)
    ds <- load_file(ds, file = path, handler_index = i, name = names(files)[[i]], stream = stream)
  }
  ds
}

test_that("a header-only table keeps ok = TRUE but is not announced as having passed", {
  dir <- withr::local_tempdir()

  for (stream in c(FALSE, TRUE)) {
    info <- paste0("stream = ", stream)
    ds <- nr_ds(dir, list(t.csv = "ID"), stream = stream)
    out <- nr_console(ds <- check(ds, persist = FALSE))

    # The verdict is unchanged: this is the documented limitation, not a fix.
    expect_identical(validation_status(ds)$ok, TRUE, info = info)
    expect_identical(results(ds)$n_valid, 1L, info = info)
    expect_identical(results(ds)$n_invalid, 0L, info = info)

    expect_match(out, "0 rows", fixed = TRUE, info = info)
    expect_match(out, "no value check was run", fixed = TRUE, info = info)
    expect_false(grepl("passed validation", out, fixed = TRUE), info = info)
  }
})

test_that("a table with rows is still announced as having passed", {
  dir <- withr::local_tempdir()

  for (stream in c(FALSE, TRUE)) {
    ds <- nr_ds(dir, list(t.csv = c("ID", "A001")), stream = stream)
    out <- nr_console(ds <- check(ds, persist = FALSE))

    expect_identical(validation_status(ds)$ok, TRUE)
    expect_match(out, "1 table passed validation", fixed = TRUE)
    expect_false(grepl("0 rows", out, fixed = TRUE))
  }
})

test_that("with one empty table among two, the line counts both kinds", {
  dir <- withr::local_tempdir()
  ds <- nr_ds(dir, list(a.csv = c("ID", "A001"), b.csv = "ID"))
  out <- nr_console(ds <- check(ds, persist = FALSE))

  expect_identical(validation_status(ds)$ok, c(TRUE, TRUE))
  expect_match(out, "1 of 2 tables passed validation", fixed = TRUE)
  expect_match(out, "1 had 0 rows", fixed = TRUE)
  expect_match(out, "no value check was run", fixed = TRUE)
})

test_that("a skipped re-check of an empty table still says it had no rows", {
  dir <- withr::local_tempdir()
  ds <- nr_ds(dir, list(t.csv = "ID"))
  ds <- suppressMessages(check(ds, persist = FALSE, quiet = TRUE))

  # Unchanged since the first check, so the second reuses the recorded verdict;
  # the row count has to travel with it.
  out <- nr_console(ds <- check(ds, persist = FALSE))
  # Reused, not rescanned: otherwise the row count would simply be recomputed
  # and this would not test that it travels with the recorded verdict.
  expect_identical(attr(ds, "last_validation_summary")$status, "skipped")
  expect_identical(validation_status(ds)$ok, TRUE)
  expect_match(out, "0 rows", fixed = TRUE)
  expect_false(grepl("passed validation", out, fixed = TRUE))
})

test_that("the summary of a whole DTA does not certify a header-only table either", {
  dir <- withr::local_tempdir()
  ds <- nr_ds(dir, list(t.csv = "ID"))
  dta <- DTA(datasets = list(ds))

  out <- nr_console(dta <- suppressWarnings(check(dta, persist = FALSE)))

  # The verdict is the documented one, not a failure ...
  expect_true(isTRUE(attr(dta, "last_validation_ok")))
  # ... but the line that certifies it has to say what was not looked at.
  expect_match(out, "Validation PASSED", fixed = TRUE)
  expect_match(out, "1 table with 0 rows", fixed = TRUE)
  expect_match(out, "no value check was run", fixed = TRUE)

  # And a delivery with rows reads exactly as before.
  ds2 <- nr_ds(dir, list(u.csv = c("ID", "A001")))
  out2 <- nr_console(suppressWarnings(check(DTA(datasets = list(ds2)), persist = FALSE)))
  expect_match(out2, "Validation PASSED: All datasets are valid", fixed = TRUE)
  expect_false(grepl("0 rows", out2, fixed = TRUE))
})

# ---- review follow-ups ---------------------------------------------------------

test_that("a run that stops at a missing column still reports the column that cannot be checked", {
  # on_missing_column = "stop" returns before any row is read, through its own
  # builder. The verdict was already failed (the column is missing), but the
  # finding about the specification was dropped there, so a reader fixing the
  # missing column met it only afterwards.
  only_bad <- data.frame(BAD = "a", stringsAsFactors = FALSE)

  details <- suppressWarnings(dta_validate_any_table(
    nr_specs(), only_bad,
    verbose = FALSE, on_missing_column = "stop"
  ))

  expect_false(details$ok)
  expect_identical(
    as.character(details$columnspec_errors$full_error$keyword),
    c("dta_check_not_run", "required")
  )
  expect_identical(details$n_columnspec_errors, 2L)
  checks <- details$columnspec_checks
  expect_identical(checks$status[match("dta_check_not_run", checks$keyword)], "failed")
})

test_that("the DTA summary counts the empty tables of the datasets it checked, not those of others", {
  dir <- withr::local_tempdir()
  empty <- nr_ds(dir, list(e.csv = "ID"))
  empty@id <- "empty_ds"
  full <- nr_ds(dir, list(f.csv = c("ID", "A001")))
  full@id <- "full_ds"
  dta <- DTA(datasets = list(empty_ds = empty, full_ds = full))
  dta <- suppressWarnings(suppressMessages(check(dta, persist = FALSE, quiet = TRUE)))

  # Checking only the dataset with rows is a delivery without empty tables.
  out <- nr_console(suppressWarnings(check(dta, datasets = "full_ds", persist = FALSE)))
  expect_match(out, "Validation PASSED", fixed = TRUE)
  expect_false(grepl("0 rows", out, fixed = TRUE))

  # Checking the empty one says so.
  out_empty <- nr_console(suppressWarnings(check(dta, datasets = "empty_ds", persist = FALSE)))
  expect_match(out_empty, "1 table with 0 rows", fixed = TRUE)
})

test_that("the line of a dataset inside check(DTA) does not certify an empty table as all valid", {
  dir <- withr::local_tempdir()
  out <- nr_console(suppressWarnings(
    check(DTA(datasets = list(nr_ds(dir, list(t.csv = "ID")))), persist = FALSE)
  ))
  expect_match(out, "1 table validated: all valid; 1 with 0 rows, on which no value check was run", fixed = TRUE)

  out_full <- nr_console(suppressWarnings(
    check(DTA(datasets = list(nr_ds(dir, list(u.csv = c("ID", "A001"))))), persist = FALSE)
  ))
  expect_match(out_full, "1 table validated: all valid", fixed = TRUE)
  expect_false(grepl("0 rows", out_full, fixed = TRUE))
})
