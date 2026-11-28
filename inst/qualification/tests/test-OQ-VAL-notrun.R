# A check that cannot run is never a pass. REQ-VAL-039 .. REQ-VAL-041.
#
# A verdict derived from the findings that were produced reads a check that
# never ran as a check that ran and found nothing. Every expected value below
# is derived from the requirement sentence by hand: the table is built so that
# exactly one value breaks exactly one declared constraint, and the column whose
# checks cannot run holds values a working check would have examined.

# ---- fixtures ---------------------------------------------------------------

# SEX is an ordinary column with a codelist; BAD carries a bare base structure,
# which cannot say how it validates, so none of its checks can run. No factory
# builds one, so it is assigned directly (hence the white-box tag).
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

# Row 2 of SEX is outside its codelist, and nothing else is wrong with a column
# that is being checked.
nr_frame <- function() {
  data.frame(SEX = c("M", "X"), BAD = c("a", NA), stringsAsFactors = FALSE)
}

nr_id_specs <- function() {
  DTAColumnSpecCollection(columns = list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE)
  ))
}

# One dataset over the CSV files written from `files` (a named list of lines),
# read in memory ("never") or streamed ("always").
nr_dataset <- function(dir, specs, files, stream) {
  ds <- DTADataSetTabular(
    id = "qual", specs = specs,
    files = lapply(names(files), function(f) DTAFileCSV(filename = f))
  )
  for (i in seq_along(files)) {
    path <- file.path(dir, names(files)[[i]])
    writeLines(files[[i]], path)
    ds <- load_file(ds, file = path, handler_index = i, name = names(files)[[i]], stream = stream)
  }
  ds
}

nr_console <- function(expr) {
  gsub("\\s+", " ", paste(testthat::capture_messages(expr), collapse = " "))
}

# What messages() reports, reduced to which constraint, which column, which row.
nr_found <- function(ds) {
  msgs <- as.data.frame(messages(ds))
  out <- data.frame(
    keyword = as.character(msgs$keyword),
    column = as.character(msgs$column),
    row = as.integer(msgs$row),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$keyword, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# ---- REQ-VAL-039 ------------------------------------------------------------

test_that("OQ-VAL-055 | a column whose specification cannot be turned into a check fails the table, in memory and streamed, once and ahead of the other findings whatever the batch size | REQ-VAL-039 | tags: white-box", {
  # The streamed scan is run with the default batch and with one row per batch,
  # which would repeat a finding made per batch.
  variants <- list(
    memory = list(stream = "never", batch_rows = NULL),
    streaming = list(stream = "always", batch_rows = NULL),
    `streaming, one row per batch` = list(stream = "always", batch_rows = 1L)
  )
  for (label in names(variants)) {
    engine <- variants[[label]]$stream
    dir <- qa_tempdir()
    csv <- c("SEX,BAD", "M,a", "X,")
    ds <- nr_dataset(dir, nr_specs(), list(t.csv = csv), stream = engine)
    ds <- suppressWarnings(suppressMessages(
      if (is.null(variants[[label]]$batch_rows)) {
        check(ds, persist = FALSE, quiet = TRUE)
      } else {
        check(ds, persist = FALSE, quiet = TRUE, batch_rows = variants[[label]]$batch_rows)
      }
    ))
    status <- validation_status(ds)

    qa_step(paste0(label, ": the table is not valid"), FALSE, status$ok)
    # One finding for the column that was not checked and one for the value of
    # SEX outside its list; neither hides the other.
    qa_step(paste0(label, ": two column specification findings"), 2L, as.integer(status$n_columnspec_errors))
    qa_step(
      paste0(label, ": the unchecked column and the failing value are both reported"),
      data.frame(
        keyword = c("dta_check_not_run", "enum"),
        column = c("BAD", "SEX"),
        row = c(NA_integer_, 2L),
        stringsAsFactors = FALSE
      ),
      nr_found(ds)
    )
    # In the order the findings are recorded: the specification first. (messages()
    # lists them by row, so the recorded frame is read.)
    recorded <- validation_errors(ds, names(tables(ds))[[1]])$columnspec_errors$full_error
    qa_step(
      paste0(label, ": the finding about the specification is recorded first"),
      c("dta_check_not_run", "enum"),
      as.character(recorded$keyword)
    )
  }
})

test_that("OQ-VAL-056 | the per-check report lists the checks that were not run as failed and the console never says the column specification checks passed | REQ-VAL-039 | tags: white-box", {
  res <- suppressWarnings(validate_table(nr_specs(), nr_frame(), verbose = FALSE))

  qa_check("a table with a column that was not checked is not returned as valid", !is.data.frame(res))
  qa_step(
    "the finding of the unchecked column comes ahead of the findings of the others",
    c("dta_check_not_run", "enum"),
    as.character(res$full_error$keyword)
  )
  checks <- res$columnspec_checks
  at <- match("dta_check_not_run", checks$keyword)
  qa_step("the checks not run are listed as failed", "failed", checks$status[[at]])
  qa_step("and name the column", "BAD", checks$failed_columns[[at]])

  # Every value of SEX is from its list here, so the checks that did run have
  # nothing to fail, and the closing line would read "All column spec checks
  # passed" if the column that was not checked were left out of the report.
  clean <- data.frame(SEX = c("M", "F"), BAD = c("a", NA), stringsAsFactors = FALSE)
  out <- nr_console(suppressWarnings(validate_table(nr_specs(), clean, verbose = TRUE)))
  qa_check("the console does not say that every column specification check passed", !grepl("All column spec checks passed", out, fixed = TRUE))
  qa_check("it says which checks were not run", grepl("Checks not run", out, fixed = TRUE))

  # A missing column is a finding of its own and must not hide this one.
  alone <- suppressWarnings(validate_table(
    nr_specs(), data.frame(BAD = c("a", "b"), stringsAsFactors = FALSE),
    verbose = FALSE
  ))
  qa_step(
    "a missing column and an unchecked column are both in the summary",
    c("dta_check_not_run", "required"),
    sort(as.character(alone$summarised_error$keyword), method = "radix")
  )

  # Nor does a run that stops at the missing column without reading a row.
  stopped <- suppressWarnings(suppressMessages(check(
    nr_dataset(qa_tempdir(), nr_specs(), list(t.csv = c("BAD", "a", "b")), stream = "always"),
    persist = FALSE, quiet = TRUE, on_missing_column = "stop"
  )))
  qa_step(
    "a run that stops at the missing column reports both findings, the specification first",
    c("dta_check_not_run", "required"),
    as.character(validation_errors(stopped, names(tables(stopped))[[1]])$columnspec_errors$full_error$keyword)
  )
})

# ---- REQ-VAL-040 ------------------------------------------------------------

test_that("OQ-VAL-057 | a column declared with the SAS backend and neither a type nor a format is refused where it is built, linted as an error and not loaded from YAML | REQ-VAL-040", {
  refused <- function(expr) inherits(tryCatch(expr, error = function(e) e), "dta_spec_bare_backend")

  qa_check("type: SAS is refused", refused(DTAColumnSpec(id = "ARM", type = "SAS", nullable = FALSE, values = c("A", "B"))))
  qa_check("a length does not make it a type", refused(DTAColumnSpec(id = "ARM", type = "SAS", length = 3)))
  qa_step("a type after the backend builds", "Char", DTAColumnSpec(id = "ARM", type = "SAS Char")@structure@type)
  qa_step("so does a format that implies one", "Char", DTAColumnSpec(id = "ARM", format = "SAS $5.")@structure@type)

  found <- lint_dta(list(columns = list(
    list(id = "ARM", type = "SAS", nullable = FALSE, values = c("A", "B")),
    list(id = "SEX", type = "SAS Char", length = 1)
  )))
  qa_step(
    "the linter reports the one column, as an error",
    data.frame(id = "ARM", severity = "error", code = "column_spec_invalid", stringsAsFactors = FALSE),
    found[, c("id", "severity", "code")]
  )

  # The bundled specification with ONE column's type cut down to the backend.
  lines <- readLines(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"), warn = FALSE)
  at <- grep("^\\s+type: SAS Char\\s*$", lines)[[1]]
  lines[[at]] <- sub("SAS Char", "SAS", lines[[at]], fixed = TRUE)
  path <- file.path(qa_tempdir(), "bare.yaml")
  writeLines(lines, path)
  err <- tryCatch(read_dta_from_yaml(path), error = function(e) e)
  qa_check("the YAML does not load", inherits(err, "dta_lint_error"))
  qa_step("and the finding is the column", "column_spec_invalid", err$findings$code)
})

# ---- REQ-VAL-041 / LIM-037 ----------------------------------------------------

test_that("OQ-VAL-058 | a table with a header and no rows is valid, and check() says it had 0 rows and that no value check was run instead of announcing a pass | REQ-VAL-041", {
  for (label in c("memory", "streaming")) {
    engine <- c(memory = "never", streaming = "always")[[label]]
    dir <- qa_tempdir()

    empty <- nr_dataset(dir, nr_id_specs(), list(t.csv = "ID"), stream = engine)
    out <- nr_console(empty <- check(empty, persist = FALSE))
    # LIM-037: the verdict is the documented one. Pinned, not endorsed.
    qa_step(paste0(label, ": the empty table is valid"), TRUE, validation_status(empty)$ok)
    qa_check(paste0(label, ": the line says 0 rows"), grepl("0 rows", out, fixed = TRUE))
    qa_check(paste0(label, ": and that no value check was run"), grepl("no value check was run", out, fixed = TRUE))
    qa_check(paste0(label, ": and does not say it passed validation"), !grepl("passed validation", out, fixed = TRUE))

    full <- nr_dataset(dir, nr_id_specs(), list(u.csv = c("ID", "A001")), stream = engine)
    out_full <- nr_console(full <- check(full, persist = FALSE))
    qa_check(paste0(label, ": a table with rows still passed validation"), grepl("1 table passed validation", out_full, fixed = TRUE))

    mixed <- nr_dataset(dir, nr_id_specs(), list(a.csv = c("ID", "A001"), b.csv = "ID"), stream = engine)
    out_mixed <- nr_console(check(mixed, persist = FALSE))
    qa_check(
      paste0(label, ": with one of two tables empty the line counts both"),
      grepl("1 of 2 tables passed validation; 1 had 0 rows", out_mixed, fixed = TRUE)
    )
  }

  # The summary of a whole DTA keeps its verdict and says what was not looked at.
  dir <- qa_tempdir()
  dta <- DTA(datasets = list(nr_dataset(dir, nr_id_specs(), list(t.csv = "ID"), stream = "never")))
  out_dta <- nr_console(dta <- suppressWarnings(check(dta, persist = FALSE)))
  qa_check("the DTA still passes", isTRUE(attr(dta, "last_validation_ok")))
  qa_check(
    "the line of its dataset does not certify the empty table as all valid",
    grepl("1 table validated: all valid; 1 with 0 rows, on which no value check was run", out_dta, fixed = TRUE)
  )
  qa_check("its summary names the table with 0 rows", grepl("Validation PASSED: All datasets are valid; 1 table with 0 rows, on which no value check was run", out_dta, fixed = TRUE))
})
