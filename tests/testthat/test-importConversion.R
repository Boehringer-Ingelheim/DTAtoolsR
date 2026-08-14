make_specs <- function(...) {
  cols <- list(...)
  DTAColumnSpecCollection(
    columns = setNames(cols, vapply(cols, function(x) x@id, character(1))),
    rules = list()
  )
}

char_num_specs <- function() {
  make_specs(
    DTAColumnSpec(id = "SUBJID", type = "SAS Char", nullable = FALSE),
    DTAColumnSpec(id = "VAL", type = "SAS Num", nullable = TRUE)
  )
}


test_that("as_r_type maps every declared SAS type to its R storage type", {
  r_type <- function(type) as_r_type(DTAColumnSpecStructureSAS(type = type))

  expect_equal(r_type("Char"), "character")
  expect_equal(r_type("Num"), "double")
  expect_equal(r_type("Int"), "integer")

  # Date/Time/DateTime are validated as strings by their pattern and format --
  # as_json_schema_type() maps all three to "string" -- so they are stored as
  # the text that was read. Parsing them would re-render the value and validate
  # something other than what the file contained.
  expect_equal(r_type("Date"), "character")
  expect_equal(r_type("Time"), "character")
  expect_equal(r_type("DateTime"), "character")

  # An unset type carries no instruction to convert, so it falls back to the
  # type the coercion layer leaves alone.
  expect_equal(as_r_type(DTAColumnSpecStructureSAS(length = 8)), "character")
})


test_that("a declared Num column is typed and its unrepresentable value becomes NA", {
  df <- data.frame(
    SUBJID = c("a", "b", "c"),
    VAL = c("30.9", "heavy", "28.5"),
    stringsAsFactors = FALSE
  )

  result <- dta_coerce_table_to_specs(df, char_num_specs())

  expect_true(is.numeric(result$table$VAL))
  expect_equal(result$table$VAL, c(30.9, NA, 28.5))

  expect_equal(nrow(result$issues), 1)
  expect_equal(result$issues$row, 2L)
  expect_equal(result$issues$column, "VAL")
  # The original value is retained verbatim, which is the whole point of making
  # the cell NA rather than dropping it.
  expect_equal(result$issues$raw, "heavy")
  expect_equal(result$issues$declared_type, "SAS Num")
  expect_equal(result$issues$reason, "not_convertible")
})


test_that("a Char column is never coerced", {
  df <- data.frame(
    SUBJID = c("007", "008"),
    VAL = c("1", "2"),
    stringsAsFactors = FALSE
  )

  result <- dta_coerce_table_to_specs(df, char_num_specs())

  # The guard that keeps a SUBJECT_ID intact: any numeric round trip would
  # silently make "007" into 7.
  expect_true(is.character(result$table$SUBJID))
  expect_identical(result$table$SUBJID, c("007", "008"))
  expect_equal(nrow(result$issues), 0)
})


test_that("only unrecoverable values are import errors", {
  df <- data.frame(
    SUBJID = c("a", "b", "c"),
    VAL = c("007", "1.50", "+3e2"),
    stringsAsFactors = FALSE
  )

  result <- dta_coerce_table_to_specs(df, char_num_specs())

  # A value that converts but changes representation is a clean conversion.
  # There is no "lossy" reason.
  expect_equal(result$table$VAL, c(7, 1.5, 300))
  expect_equal(nrow(result$issues), 0)
})


test_that("missing values stay missing and are not import errors", {
  df <- data.frame(
    SUBJID = c("a", "b", "c", "d"),
    VAL = c(NA, "", "   ", "5"),
    stringsAsFactors = FALSE
  )

  result <- dta_coerce_table_to_specs(df, char_num_specs())

  expect_equal(result$table$VAL, c(NA, NA, NA, 5))
  expect_equal(nrow(result$issues), 0)
})


test_that("a column absent from the specs is left untouched, not dropped", {
  df <- data.frame(
    SUBJID = c("a", "b"),
    VAL = c("1", "2"),
    EXTRA = c("x1", "x2"),
    stringsAsFactors = FALSE
  )

  result <- dta_coerce_table_to_specs(df, char_num_specs())

  expect_named(result$table, c("SUBJID", "VAL", "EXTRA"))
  expect_identical(result$table$EXTRA, c("x1", "x2"))
})


test_that("an already numeric column is left alone", {
  df <- data.frame(SUBJID = c("a", "b"), VAL = c(1.5, 2.5), stringsAsFactors = FALSE)

  result <- dta_coerce_table_to_specs(df, char_num_specs())

  # Nothing to parse means no value can fail to parse: this is what stops the
  # import axis from inventing errors on a clean file.
  expect_identical(result$table$VAL, c(1.5, 2.5))
  expect_equal(nrow(result$issues), 0)
  expect_identical(result$table, df)
})


test_that("an Int column narrows to integer only when that is lossless", {
  specs <- make_specs(DTAColumnSpec(id = "N", type = "SAS Int", nullable = TRUE))

  whole <- dta_coerce_table_to_specs(
    data.frame(N = c("1", "2", NA), stringsAsFactors = FALSE),
    specs
  )
  expect_true(is.integer(whole$table$N))
  expect_equal(whole$table$N, c(1L, 2L, NA))

  # Rounding a fractional value into an integer column would discard the
  # fraction and hide the `type: integer` column spec error that reports it, so the
  # value stays a double and the column spec axis keeps its job.
  fractional <- dta_coerce_table_to_specs(
    data.frame(N = c("1", "2.5"), stringsAsFactors = FALSE),
    specs
  )
  expect_false(is.integer(fractional$table$N))
  expect_equal(fractional$table$N, c(1, 2.5))
  expect_equal(nrow(fractional$issues), 0)
})


test_that("import issues are carried on the table and survive the Arrow round trip", {
  df <- data.frame(
    SUBJID = c("a", "b"),
    VAL = c("1", "heavy"),
    stringsAsFactors = FALSE
  )

  result <- dta_coerce_table_to_specs(df, char_num_specs())
  round_tripped <- as.data.frame(arrow::as_arrow_table(result$table))
  carried <- dta_carried_import_issues(round_tripped)

  expect_true(is.data.frame(carried))
  expect_equal(as.data.frame(carried), as.data.frame(result$issues))
  expect_equal(dta_import_error_count(carried), 1L)
})


test_that("carried import issues change the table hash", {
  df <- data.frame(
    SUBJID = c("a", "b"),
    VAL = c("1", "heavy"),
    stringsAsFactors = FALSE
  )

  with_issues <- as.data.frame(
    arrow::as_arrow_table(dta_coerce_table_to_specs(df, char_num_specs())$table)
  )

  without <- with_issues
  attr(without, "dta_import_issues") <- NULL

  altered <- with_issues
  altered_issues <- dta_carried_import_issues(with_issues)
  altered_issues$raw <- "different"
  attr(altered, "dta_import_issues") <- altered_issues

  # check() skips revalidation when the table hash and the specs hash are both
  # unchanged. Issues living only in the dataset property would not be hashed,
  # so a table whose import issues had changed could be skipped while still
  # reporting a stale ok = TRUE.
  expect_false(identical(dta_hash_object(with_issues), dta_hash_object(without)))
  expect_false(identical(dta_hash_object(with_issues), dta_hash_object(altered)))
})


test_that("validate_table_detailed reads the carried import issues", {
  df <- data.frame(
    SUBJID = c("a", "b"),
    VAL = c("1", "heavy"),
    stringsAsFactors = FALSE
  )

  specs <- char_num_specs()
  typed <- as.data.frame(
    arrow::as_arrow_table(dta_coerce_table_to_specs(df, specs)$table)
  )

  details <- validate_table_detailed(specs, typed, verbose = FALSE)

  expect_false(details$import_valid)
  expect_equal(details$n_import_errors, 1L)
  expect_equal(details$import_errors$column, "VAL")
  expect_equal(details$import_errors$raw, "heavy")
  # The import axis fails the table on its own.
  expect_false(details$ok)
})


test_that("retained issues are capped but the error count stays exact", {
  specs <- char_num_specs()
  n <- dta_import_max_rows_per_column + 2000L

  wide <- data.frame(
    SUBJID = as.character(seq_len(n)),
    VAL = rep("nope", n),
    stringsAsFactors = FALSE
  )

  result <- dta_coerce_table_to_specs(wide, specs)

  expect_equal(nrow(result$issues), dta_import_max_rows_per_column)
  expect_equal(dta_import_error_count(result$issues), n)

  # The cap bounds memory; it must never be able to turn a failing table into a
  # passing one.
  details <- validate_table_detailed(specs, result$table, verbose = FALSE)
  expect_equal(details$n_import_errors, n)
  expect_equal(nrow(details$import_errors), dta_import_max_rows_per_column)
  expect_false(details$ok)
})


test_that("a retained raw value is truncated but reported in full count", {
  result <- dta_coerce_table_to_specs(
    data.frame(
      SUBJID = "a",
      VAL = strrep("z", dta_import_raw_max_chars + 300L),
      stringsAsFactors = FALSE
    ),
    char_num_specs()
  )

  expect_equal(nchar(result$issues$raw), dta_import_raw_max_chars)
  expect_equal(dta_import_error_count(result$issues), 1L)
})


test_that("the DTADataSetTabular constructor types its tables", {
  ds <- DTADataSetTabular(
    name = "ctor",
    specs = char_num_specs(),
    tables = list(
      t = data.frame(
        SUBJID = c("007", "008"),
        VAL = c("1.50", "heavy"),
        stringsAsFactors = FALSE
      )
    )
  )

  typed <- as.data.frame(ds@tables[["t"]])

  expect_identical(typed$SUBJID, c("007", "008"))
  expect_true(is.numeric(typed$VAL))
  expect_equal(typed$VAL, c(1.5, NA))

  expect_named(ds@import_issues, "t")
  expect_equal(ds@import_issues[["t"]]$raw, "heavy")
})


test_that("a clean table leaves import_issues an empty list", {
  ds <- DTADataSetTabular(
    name = "clean",
    specs = char_num_specs(),
    tables = list(t = data.frame(SUBJID = "a", VAL = "1", stringsAsFactors = FALSE))
  )

  # A named empty list is not identical() to list(); "no import issues" must be
  # one value, not two.
  expect_identical(ds@import_issues, list())
})


test_that("load_file types the table and records its import issues", {
  dir <- file.path(tempdir(), "dta-typed-import")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  csv <- file.path(dir, "typed_import.csv")
  writeLines(
    c('"SUBJID","VAL"', '"007A","1.50"', '"008B","heavy"', '"009C",""'),
    csv
  )

  handler <- DTAFileCSV(filename = "typed_import.csv")
  ds <- DTADataSetTabular(
    name = "typed",
    specs = char_num_specs(),
    files = list(handler)
  )

  ds <- DTAtools:::load_file(ds, file = csv, handler_index = 1)
  typed <- as.data.frame(ds@tables[["typed_import"]])

  # The reader would have made VAL a string column because of "heavy"; the
  # declared type decides instead.
  expect_true(is.numeric(typed$VAL))
  expect_equal(typed$VAL, c(1.5, NA, NA))
  expect_identical(typed$SUBJID, c("007A", "008B", "009C"))

  expect_named(ds@import_issues, "typed_import")
  issues <- ds@import_issues[["typed_import"]]
  expect_equal(nrow(issues), 1)
  expect_equal(issues$row, 2L)
  expect_equal(issues$raw, "heavy")

  # The blank in row 3 is missing, not an import error.
  expect_false(3L %in% issues$row)

  # And the same frame rides on the table.
  expect_equal(
    as.data.frame(dta_carried_import_issues(typed)),
    as.data.frame(issues)
  )
})


# ---------------------------------------------------------------------------
# dta_reader_col_types(): the schema handed to the reader
# ---------------------------------------------------------------------------

id_specs <- function() {
  make_specs(
    DTAColumnSpec(id = "SUBJID", type = "SAS Char", format = "SAS $8."),
    DTAColumnSpec(id = "AGE", type = "SAS Int", format = "SAS 8.")
  )
}

test_that("dta_reader_col_types pins only the declared character columns", {
  schema <- dta_reader_col_types(id_specs())

  # AGE is declared Int and is deliberately left to inference: telling arrow a
  # column is int64 makes it abort the entire read on the first cell it cannot
  # parse, which would turn one reportable bad cell into a file that will not
  # load at all.
  expect_equal(schema$names, "SUBJID")
  expect_true(schema$GetFieldByName("SUBJID")$type == arrow::utf8())
})

test_that("dta_reader_col_types yields NULL when there is nothing to pin", {
  expect_null(dta_reader_col_types(NULL))
  expect_null(dta_reader_col_types(make_specs(
    DTAColumnSpec(id = "AGE", type = "SAS Int", format = "SAS 8.")
  )))

  # Without a header arrow generates positional names that cannot correspond to
  # spec ids, so no schema can be built.
  expect_null(dta_reader_col_types(id_specs(), has_header = FALSE))
})


# ---------------------------------------------------------------------------
# A declared Char column survives the whole load_file path
# ---------------------------------------------------------------------------

# Before the specs reached the reader, arrow inferred a column of quoted
# subject ids as int64 and "007" arrived in R as 7. The leading zeros were
# already gone by the time dta_coerce_table_to_specs() ran, so its "never
# coerce a Char column" guard had nothing left to protect.
dta_load_id_fixture <- function(handler, name, sep) {
  dir <- file.path(tempdir(), "dta-char-ids")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, name)
  row <- function(...) paste(c(...), collapse = sep)
  writeLines(
    c(
      row('"SUBJID"', '"AGE"', '"EXTRA"'),
      row('"007"', "30", "10"),
      row('"008"', "41", "20")
    ),
    path
  )

  ds <- DTADataSetTabular(
    name = "ids",
    specs = id_specs(),
    files = list(handler)
  )

  ds <- DTAtools:::load_file(ds, file = path, handler_index = 1)
  unlink(path)
  list(
    table = as.data.frame(ds@tables[[tools::file_path_sans_ext(name)]]),
    issues = ds@import_issues
  )
}

test_that("a declared Char id keeps its leading zeros through load_file (CSV)", {
  out <- dta_load_id_fixture(DTAFileCSV("char_ids.csv"), "char_ids.csv", ",")

  expect_identical(out$table$SUBJID, c("007", "008"))
  # A declared numeric column is still a number, so range rules and column specs
  # validation see what they saw before.
  expect_true(is.numeric(out$table$AGE))
  expect_equal(out$table$AGE, c(30L, 41L))
  # No new import errors on a clean file.
  expect_identical(out$issues, list())
})

test_that("a declared Char id keeps its leading zeros through load_file (TSV)", {
  out <- dta_load_id_fixture(DTAFileTSV("char_ids.tsv"), "char_ids.tsv", "\t")

  expect_identical(out$table$SUBJID, c("007", "008"))
  expect_true(is.numeric(out$table$AGE))
  expect_identical(out$issues, list())
})

test_that("a declared Char id keeps its leading zeros through load_file (Delim)", {
  out <- dta_load_id_fixture(
    DTAFileDelim("char_ids.psv", sep = "|"),
    "char_ids.psv",
    "|"
  )

  expect_identical(out$table$SUBJID, c("007", "008"))
  expect_true(is.numeric(out$table$AGE))
  expect_identical(out$issues, list())
})

test_that("a column absent from the specs is inferred, not dropped", {
  out <- dta_load_id_fixture(DTAFileCSV("char_ids.csv"), "char_ids.csv", ",")

  # EXTRA is in neither the schema handed to the reader nor the coercion loop,
  # so it keeps exactly the type arrow would have given it with no specs at
  # all -- an integer here -- and it is still present.
  expect_true("EXTRA" %in% names(out$table))
  expect_true(is.numeric(out$table$EXTRA))
  expect_equal(as.numeric(out$table$EXTRA), c(10, 20))
})
