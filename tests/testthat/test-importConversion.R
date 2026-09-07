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
# A declared Char column survives the whole load_file path
# ---------------------------------------------------------------------------

id_specs <- function() {
  make_specs(
    DTAColumnSpec(id = "SUBJID", type = "SAS Char", format = "SAS $8."),
    DTAColumnSpec(id = "AGE", type = "SAS Int", format = "SAS 8.")
  )
}

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

test_that("a column absent from the specs is read as text, not dropped", {
  out <- dta_load_id_fixture(DTAFileCSV("char_ids.csv"), "char_ids.csv", ",")

  # EXTRA is described by no spec, so nothing in the coercion loop touches it
  # and it is still present. Its TYPE is no longer arrow's guess, though: when
  # specs are supplied every column is read as text on both the eager and the
  # lazy reader, because an undeclared column that arrow inferred in memory and
  # read as text when streamed made the same file reach two different rule
  # verdicts depending on how it was loaded.
  expect_true("EXTRA" %in% names(out$table))
  expect_type(out$table$EXTRA, "character")
  expect_identical(out$table$EXTRA, c("10", "20"))
})


# ---------------------------------------------------------------------------
# The content stamp an Arrow table carries out of the import choke point
# ---------------------------------------------------------------------------

test_that("a coerced Arrow table carries a content stamp and the caller's does not", {
  specs <- char_num_specs()
  original <- arrow::as_arrow_table(
    data.frame(SUBJID = c("a", "b"), VAL = c("1", "2"), stringsAsFactors = FALSE)
  )

  stamped <- dta_coerce_table_to_specs(original, specs)$table

  expect_type(dta_table_hash_stamp(stamped), "character")
  # VAL had to be typed, so the returned table is a NEW one built from the typed
  # frame. The caller's object is a different table holding different values, and
  # a stamp claiming otherwise would be a lie about its contents.
  expect_null(dta_table_hash_stamp(original))
  # And the data itself is untouched by the stamping.
  expect_identical(
    as.data.frame(stamped)$SUBJID,
    c("a", "b")
  )
})


test_that("a table needing no typing at all is still stamped", {
  # The early return hands back the ORIGINAL object rather than rebuilding it.
  # Without a stamp on that branch, a clean table -- the common case -- would
  # keep paying the full hash on every check().
  specs <- make_specs(DTAColumnSpec(id = "SUBJID", type = "SAS Char", nullable = FALSE))
  table <- arrow::as_arrow_table(
    data.frame(SUBJID = c("x", "y"), stringsAsFactors = FALSE)
  )

  stamped <- dta_coerce_table_to_specs(table, specs)$table

  expect_type(dta_table_hash_stamp(stamped), "character")
  # Nothing was rebuilt, so the returned table IS the caller's -- an arrow table
  # is an R6 object, i.e. an environment, and the stamp rides on the object. The
  # caller's reference sees it, which is correct: it is the same contents.
  expect_identical(dta_table_hash_stamp(table), dta_table_hash_stamp(stamped))
})


test_that("a table Arrow builds anew carries no stamp", {
  # The stamp used to live in the table's SCHEMA METADATA, which every Arrow
  # operation carries forward: a table concatenated with itself kept the stamp
  # of the table it came from, so check() skipped it and reported the old
  # verdict over data that had doubled. The stamp identifies an OBJECT's
  # contents, so it must not survive an operation that produces different ones.
  specs <- char_num_specs()
  table <- dta_coerce_table_to_specs(
    arrow::as_arrow_table(
      data.frame(SUBJID = c("a", "b"), VAL = c("1", "2"), stringsAsFactors = FALSE)
    ),
    specs
  )$table
  expect_type(dta_table_hash_stamp(table), "character")

  rebuilt <- list(
    concatenated = arrow::concat_tables(table, table),
    sliced = table$Slice(0, 1),
    subset = table[, "SUBJID", drop = FALSE],
    computed = arrow::as_arrow_table(dplyr::compute(dplyr::filter(table, VAL > 1)))
  )

  for (how in names(rebuilt)) {
    expect_null(dta_table_hash_stamp(rebuilt[[how]]), info = how)
    # And the change signal falls back to hashing them, so each is identified by
    # what it actually holds rather than by what it was derived from. (Each of
    # these four holds something different from the original; an operation that
    # happened to reproduce the original's contents exactly -- a subset naming
    # every column -- would rightly hash to the same value.)
    expect_false(
      identical(dta_table_change_signal(rebuilt[[how]]), dta_table_change_signal(table)),
      info = how
    )
  }
})


test_that("check() rescans a table Arrow rebuilt and skips the same object", {
  specs <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
      VAL = DTAColumnSpec(id = "VAL", type = "SAS Num", nullable = TRUE)
    ),
    rules = list(DTARuleColUnique(id = "uid", columns = "ID"))
  )
  dir <- file.path(tempdir(), "dta-stamp-identity")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  csv <- file.path(dir, "ids.csv")
  writeLines(c("ID,VAL", "a,1", "b,2", "c,3"), csv)

  ds <- DTADataSetTabular(
    name = "d", specs = specs, files = list(DTAFileCSV(filename = "ids.csv"))
  )
  ds <- load_file(ds, file = csv, handler_index = 1, stream = "never")
  ds <- check(ds, persist = FALSE, quiet = TRUE)
  summary_of <- function(x) attr(x, "last_validation_summary")

  expect_identical(summary_of(ds)$status, "validated")
  expect_true(summary_of(ds)$ok)

  # The same object handed back: nothing changed, so nothing is rescanned.
  same <- ds
  same@tables[[1]] <- ds@tables[[1]]
  same <- check(same, persist = FALSE, quiet = TRUE)
  expect_identical(summary_of(same)$status, "skipped")

  # Concatenated with itself: every id is now a duplicate, and the uniqueness
  # rule must say so. Reported "skipped"/ok while the stamp rode on the schema.
  doubled <- ds
  doubled@tables[[1]] <- arrow::concat_tables(ds@tables[[1]], ds@tables[[1]])
  doubled <- check(doubled, persist = FALSE, quiet = TRUE)
  expect_identical(summary_of(doubled)$status, "validated")
  expect_false(summary_of(doubled)$ok)
  expect_identical(summary_of(doubled)$n_rule_errors, 1L)

  # An unstamped table of DIFFERENT contents is identified by hashing it, and
  # that fallback is stable: revalidated once, skipped from then on.
  plain <- ds
  plain@tables[[1]] <- arrow::as_arrow_table(
    data.frame(ID = c("a", "b", "c", "d"), VAL = c(1, 2, 3, 4), stringsAsFactors = FALSE)
  )
  expect_null(dta_table_hash_stamp(plain@tables[[1]]))
  plain <- check(plain, persist = FALSE, quiet = TRUE)
  expect_identical(summary_of(plain)$status, "validated")
  plain <- check(plain, persist = FALSE, quiet = TRUE)
  expect_identical(summary_of(plain)$status, "skipped")

  # And an unstamped table rebuilt from a stamped one's OWN contents hashes to
  # the stamp, so it is recognised rather than rescanned on every check().
  rebuilt <- ds
  rebuilt@tables[[1]] <- arrow::as_arrow_table(as.data.frame(ds@tables[[1]]))
  expect_null(dta_table_hash_stamp(rebuilt@tables[[1]]))
  rebuilt <- check(rebuilt, persist = FALSE, quiet = TRUE)
  expect_identical(summary_of(rebuilt)$status, "skipped")

  # Clearing the verdicts does not touch the data, so the stamp stays put.
  cleared <- clear_validation(ds)
  expect_identical(
    dta_table_hash_stamp(cleared@tables[[1]]),
    dta_table_hash_stamp(ds@tables[[1]])
  )
})


test_that("the stamp is what dta_table_change_signal() reports, and it tracks content", {
  specs <- char_num_specs()
  frame <- function(vals) {
    arrow::as_arrow_table(
      data.frame(SUBJID = c("a", "b"), VAL = vals, stringsAsFactors = FALSE)
    )
  }

  first <- dta_coerce_table_to_specs(frame(c("1", "2")), specs)$table
  same <- dta_coerce_table_to_specs(frame(c("1", "2")), specs)$table
  altered <- dta_coerce_table_to_specs(frame(c("1", "3")), specs)$table

  expect_identical(dta_table_change_signal(first), dta_table_hash_stamp(first))
  expect_identical(dta_table_change_signal(first), dta_table_change_signal(same))
  expect_false(identical(dta_table_change_signal(first), dta_table_change_signal(altered)))

  # A table that never passed the choke point has no stamp, and is identified by
  # hashing it -- to the same value, so a lost stamp cannot fake a change.
  unstamped <- arrow::as_arrow_table(as.data.frame(first))
  expect_null(dta_table_hash_stamp(unstamped))
  expect_identical(dta_table_change_signal(unstamped), dta_table_change_signal(first))
})


test_that("carried import issues are inside the stamp", {
  # The reason the stamp is taken AFTER the issues are attached: check() skips
  # revalidation on an unchanged signal, so issues outside the signal could
  # change while a stale ok = TRUE stood.
  specs <- char_num_specs()
  base <- data.frame(SUBJID = c("a", "b"), VAL = c("1", "heavy"), stringsAsFactors = FALSE)

  dirty <- dta_coerce_table_to_specs(arrow::as_arrow_table(base), specs)$table
  expect_type(dta_table_hash_stamp(dirty), "character")

  clean <- base
  clean$VAL <- c("1", "2")
  clean_table <- dta_coerce_table_to_specs(arrow::as_arrow_table(clean), specs)$table

  expect_false(
    identical(dta_table_change_signal(dirty), dta_table_change_signal(clean_table))
  )

  # Same typed values, different retained issue text: still a different signal.
  round_tripped <- as.data.frame(dirty)
  issues <- dta_carried_import_issues(round_tripped)
  issues$raw <- "different"
  attr(round_tripped, "dta_import_issues") <- issues
  expect_false(
    identical(
      dta_table_change_signal(dirty),
      dta_table_change_signal(arrow::as_arrow_table(round_tripped))
    )
  )
})


test_that("the stamp and the unstamped fallback are the same hash", {
  # `rlang::hash()` of a data frame digests its attributes IN ORDER, and the
  # Arrow round trip returns `dta_import_issues` and `class` in the opposite
  # order to the frame the stamp was taken from. Every table carrying import
  # issues therefore hashed one way at the choke point and another when read
  # back, so a rebuilt table was rescanned on every check() -- the exact cost
  # the stamp exists to avoid. Both sides now go through
  # dta_table_content_hash(), which does not see attribute order at all.
  specs <- char_num_specs()
  dirty <- dta_coerce_table_to_specs(
    arrow::as_arrow_table(
      data.frame(SUBJID = c("a", "b"), VAL = c("1", "zzz"), stringsAsFactors = FALSE)
    ),
    specs
  )$table

  # The issues really are riding on the table; without them the two hashes agree
  # trivially and this test proves nothing.
  expect_equal(nrow(dta_carried_import_issues(as.data.frame(dirty))), 1L)

  expect_identical(
    dta_table_hash_stamp(dirty),
    dta_table_content_hash(as.data.frame(dirty))
  )
  expect_identical(
    dta_table_change_signal(dirty),
    dta_table_change_signal(arrow::as_arrow_table(as.data.frame(dirty)))
  )

  # And it is still a hash OF THE CONTENTS: the same table with that one cell
  # fixed hashes differently.
  fixed <- dta_coerce_table_to_specs(
    arrow::as_arrow_table(
      data.frame(SUBJID = c("a", "b"), VAL = c("1", "2"), stringsAsFactors = FALSE)
    ),
    specs
  )$table
  expect_false(identical(dta_table_hash_stamp(dirty), dta_table_hash_stamp(fixed)))
})


test_that("load_file and the DTADataSetTabular constructor both stamp their tables", {
  specs <- char_num_specs()

  ds <- DTADataSetTabular(
    name = "stamped",
    specs = specs,
    tables = list(
      t = data.frame(SUBJID = c("a", "b"), VAL = c("1", "heavy"), stringsAsFactors = FALSE)
    )
  )
  expect_type(dta_table_hash_stamp(ds@tables[["t"]]), "character")

  dir <- file.path(tempdir(), "dta-stamped-load")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  csv <- file.path(dir, "stamped.csv")
  writeLines(c('"SUBJID","VAL"', '"a","1"', '"b","heavy"'), csv)

  loaded <- DTADataSetTabular(
    name = "stamped_file",
    specs = specs,
    files = list(DTAFileCSV(filename = "stamped.csv"))
  )
  loaded <- DTAtools:::load_file(loaded, file = csv, handler_index = 1, stream = "never")

  expect_type(dta_table_hash_stamp(loaded@tables[["stamped"]]), "character")
})


# ---------------------------------------------------------------------------
# Regression: float value in a declared Int column must not abort the read
# ---------------------------------------------------------------------------

test_that("a float in a declared Int column is an import error, not a read abort", {
  # Before the fix, Arrow inferred the column as int64 (because early rows look
  # like integers) and then aborted with
  #   "CSV conversion error to int64: invalid value '0.01'"
  # when it encountered the fractional value.  The column is now pinned to
  # utf8 at read time so Arrow never attempts the int64 conversion.
  # dta_coerce_column() intentionally does NOT flag 0.01 as an import error --
  # a fractional value in an Int column is left as a double and reported as a
  # *schema* (column spec) error, not an import error.  What matters here is
  # that the file loads without aborting.
  dir <- file.path(tempdir(), "dta-float-in-int")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  csv <- file.path(dir, "float_in_int.csv")
  writeLines(
    c(
      '"SUBJID","DOSE"',
      paste(paste0('"S', seq_len(100), '"'), "10", sep = ","),
      '"S101",0.01'
    ),
    csv
  )

  specs <- make_specs(
    DTAColumnSpec(id = "SUBJID", type = "SAS Char", nullable = FALSE),
    DTAColumnSpec(id = "DOSE", type = "SAS Int", nullable = FALSE)
  )

  handler <- DTAFileCSV(filename = "float_in_int.csv")
  ds <- DTADataSetTabular(name = "fi", specs = specs, files = list(handler))

  # Must not abort -- this was the bug.
  expect_no_error(ds <- DTAtools:::load_file(ds, file = csv, handler_index = 1))

  typed <- as.data.frame(ds@tables[["float_in_int"]])

  # The fractional value is readable; it stays as a double so the schema axis
  # can report it as a type violation.
  expect_true(is.numeric(typed$DOSE))
  expect_equal(typed$DOSE[[101]], 0.01)
})


test_that("streaming validation does not abort on a float in a declared Int column", {
  dir <- file.path(tempdir(), "dta-float-in-int-stream")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  csv <- file.path(dir, "stream_float.csv")
  writeLines(
    c(
      '"SUBJID","DOSE"',
      paste(paste0('"S', seq_len(100), '"'), "10", sep = ","),
      '"S101",0.01'
    ),
    csv
  )

  specs <- make_specs(
    DTAColumnSpec(id = "SUBJID", type = "SAS Char", nullable = FALSE),
    DTAColumnSpec(id = "DOSE", type = "SAS Int", nullable = FALSE)
  )

  # Must not abort -- this was the bug.
  expect_no_error(
    details <- validate_file_stream(specs, csv, verbose = FALSE)
  )

  # 0.01 in an Int column is a schema (column spec) violation, so the table
  # fails even though there are no *import* errors.
  expect_false(details$ok)
})


test_that("an import error count above the integer limit survives being read back", {
  issues <- dta_empty_import_errors()
  attr(issues, "n_import_errors") <- 3e9

  # The count used to be round-tripped through `as.integer()`, which turns 3e9
  # into `NA` -- and an `NA` attribute is read here as "no attribute at all",
  # falling back to the capped `nrow()`. The cap exists precisely for frames
  # this large, so the fallback under-reported by nearly the whole count.
  expect_no_warning(n <- dta_import_error_count(issues))
  expect_equal(n, 3e9)
})


test_that("merging import errors totals past the integer limit", {
  carried <- dta_empty_import_errors()
  attr(carried, "n_import_errors") <- 2.5e9

  rule <- data.frame(
    row = 1L,
    column = "VAL",
    raw = "nope",
    declared_type = "SAS Num",
    reason = "not_convertible",
    stringsAsFactors = FALSE
  )

  expect_no_warning(merged <- dta_merge_import_errors(carried, rule))
  expect_equal(dta_import_error_count(merged), 2.5e9 + 1)
})


# ---------------------------------------------------------------------------
# dta_frame_is_arrow_stable(): which frames survive the Arrow round trip
# ---------------------------------------------------------------------------

# The DTADataSetTabular() constructor stamps a data-frame table with the hash
# of the frame it coerced, instead of paying for an extra Arrow -> R -> Arrow
# detour just to hash the round-tripped copy. That is the same answer only
# while the round trip is lossless, and this predicate is what decides. The
# tests below do not restate the whitelist -- they perform the round trip and
# check that the predicate agreed with what actually happened, so a change in
# Arrow's R type mapping shows up here rather than as tables that are silently
# rescanned on every check().

dta_round_trip <- function(df) as.data.frame(arrow::as_arrow_table(df))

test_that("every type the predicate admits really does survive the round trip", {
  admitted <- list(
    character = data.frame(a = c("x", NA_character_), stringsAsFactors = FALSE),
    numeric = data.frame(a = c(1.5, NA_real_)),
    integer = data.frame(a = c(1L, NA_integer_)),
    logical = data.frame(a = c(TRUE, FALSE, NA)),
    Date = data.frame(a = as.Date(c("2026-01-01", NA))),
    factor = data.frame(a = factor(c("x", "y"), levels = c("y", "x", "z"))),
    ordered = data.frame(a = factor(c("x", "y"), ordered = TRUE)),
    POSIXct_named_tz = data.frame(
      a = as.POSIXct(c("2026-01-01 10:00:00", NA), tz = "UTC")
    ),
    several = data.frame(
      a = c("x", "y"), b = c(1.5, 2.5), c = c(TRUE, NA),
      stringsAsFactors = FALSE
    )
  )

  for (label in names(admitted)) {
    df <- admitted[[label]]
    expect_true(dta_frame_is_arrow_stable(df), info = label)
    # The claim the constructor relies on: a stamp taken from `df` is also the
    # digest dta_table_change_signal() derives from the stored Table.
    expect_identical(
      dta_table_content_hash(df),
      dta_table_content_hash(dta_round_trip(df)),
      info = label
    )
  }

  # A frame with no columns has nothing that could change.
  expect_true(dta_frame_is_arrow_stable(data.frame()))
})

test_that("the types the predicate rejects really do change across the round trip", {
  rejected <- list(
    # Arrow returns seconds whatever units went in.
    difftime = data.frame(a = as.difftime(c(1, 2), units = "hours")),
    # `tzone = ""` -- what as.POSIXct() leaves when no timezone is named --
    # comes back with no tzone at all.
    POSIXct_unnamed_tz = data.frame(a = as.POSIXct("2026-01-01 10:00:00"))
  )

  if (requireNamespace("bit64", quietly = TRUE)) {
    # int64_downcast: an integer64 whose values fit in 32 bits comes back as
    # integer.
    rejected$integer64 <- data.frame(a = bit64::as.integer64(c(1, 2)))
  }

  for (label in names(rejected)) {
    df <- rejected[[label]]
    expect_false(dta_frame_is_arrow_stable(df), info = label)
    expect_false(
      identical(
        dta_table_content_hash(df),
        dta_table_content_hash(dta_round_trip(df))
      ),
      info = label
    )
  }

  # Not a data frame at all: an Arrow input is not this predicate's business,
  # and takes the route on which the coercion does the stamping.
  expect_false(dta_frame_is_arrow_stable(arrow::arrow_table(a = 1)))
  expect_false(dta_frame_is_arrow_stable(NULL))
})

test_that("a time column is admitted on its storage, not on its class alone", {
  # Arrow returns a date32 and a timestamp as DOUBLES whichever storage went
  # in. `as.Date()` and `as.POSIXct()` produce doubles, but the class can also
  # be put on an integer vector -- by `structure()`, by an older serialisation,
  # or by arithmetic that stayed integral -- and such a column comes back a
  # different vector carrying the same instants. Admitted on its class alone it
  # was stamped from the frame that went in, so check() skipped a table whose
  # stored digest it would never derive again.
  # The integer twin is derived from the double one rather than written out, so
  # that the two carry the same instants by construction and the test cannot
  # drift into comparing two different dates.
  as_integer_storage <- function(x) {
    structure(
      as.integer(unclass(x)),
      class = class(x),
      tzone = attr(x, "tzone", exact = TRUE)
    )
  }

  storages <- list(
    Date = list(double = as.Date(c("2026-01-01", "2026-06-15"))),
    POSIXct = list(
      double = as.POSIXct(c("2026-01-01 10:00:00", "2026-06-15 23:59:59"), tz = "UTC")
    )
  )
  storages <- lapply(storages, function(pair) {
    pair$integer <- as_integer_storage(pair$double)
    pair
  })

  for (label in names(storages)) {
    pair <- storages[[label]]

    # The premise: same class, same values, different storage.
    expect_identical(class(pair$double), class(pair$integer), info = label)
    expect_true(is.double(pair$double), info = label)
    expect_true(is.integer(pair$integer), info = label)
    expect_equal(as.numeric(pair$double), as.numeric(pair$integer), info = label)

    expect_true(dta_frame_is_arrow_stable(data.frame(a = pair$double)), info = label)
    expect_false(dta_frame_is_arrow_stable(data.frame(a = pair$integer)), info = label)

    # And the reason: only the double storage survives the round trip.
    stable <- data.frame(a = pair$double)
    expect_identical(
      dta_table_content_hash(stable),
      dta_table_content_hash(dta_round_trip(stable)),
      info = label
    )
    unstable <- data.frame(a = pair$integer)
    expect_false(
      identical(
        dta_table_content_hash(unstable),
        dta_table_content_hash(dta_round_trip(unstable))
      ),
      info = label
    )
  }
})

test_that("the coercion cannot turn an admitted frame into an unstable one", {
  # The predicate is applied to the frame handed IN, while the stamp is taken
  # from the frame the coercion hands back -- so the two would disagree if
  # coercion could produce a type the predicate rejects. It cannot:
  # dta_coerce_column() only ever yields `double` or `integer`, both admitted.
  specs <- DTAColumnSpecCollection(columns = list(
    NUMS = DTAColumnSpec(id = "NUMS", type = "SAS Num", nullable = TRUE),
    INTS = DTAColumnSpec(id = "INTS", type = "SAS Int", nullable = TRUE),
    KEEP = DTAColumnSpec(id = "KEEP", type = "SAS Char", length = 8, nullable = TRUE)
  ))
  df <- data.frame(
    NUMS = c("1.5", "abc"),
    INTS = c("7", "8"),
    KEEP = c("007", "008"),
    stringsAsFactors = FALSE
  )

  expect_true(dta_frame_is_arrow_stable(df))
  coerced <- dta_coerce_table_to_specs(df, specs)$table
  expect_true(dta_frame_is_arrow_stable(coerced))
  expect_identical(vapply(coerced, function(x) class(x)[[1]], character(1)), c(
    NUMS = "numeric", INTS = "integer", KEEP = "character"
  ))
  # Including the issues attribute the coercion attached, which the stamp
  # covers and the round trip must therefore preserve.
  expect_false(is.null(attr(coerced, "dta_import_issues", exact = TRUE)))
  expect_identical(
    dta_table_content_hash(coerced),
    dta_table_content_hash(dta_round_trip(coerced))
  )
})


# ---- parsing declared-numeric text in Arrow ----------------------------------

# Digit strings of a fixed width. Built by matrix rather than by loop because
# the differential test below wants 50,000 of them and a `vapply()` over that
# costs more than the comparison it feeds.
ic_digits <- function(width, n) {
  if (width == 0L) {
    return(rep("", n))
  }
  m <- matrix(sample(0:9, width * n, replace = TRUE), nrow = n)
  do.call(paste0, split(m, col(m)))
}

ic_sign <- function(n) sample(c("", "", "", "+", "-"), n, replace = TRUE)

# Literals drawn from exactly the shapes `DTA_ARROW_DOUBLE_PATTERN` admits:
# every integer width from 1 to 15, crossed with every fraction width from 0 to
# 3 that keeps the total at 15 digits or fewer, plus the bare-point forms at
# each end (`123.` and `.45`), each with an optional sign.
ic_double_literals <- function(per = 900L) {
  blocks <- list()
  for (int_width in 1:12) {
    for (frac_width in 0:3) {
      body <- ic_digits(int_width, per)
      if (frac_width > 0L) {
        body <- paste0(body, ".", ic_digits(frac_width, per))
      }
      blocks[[length(blocks) + 1L]] <- paste0(ic_sign(per), body)
    }
  }
  for (int_width in 13:15) {
    blocks[[length(blocks) + 1L]] <- paste0(ic_sign(per), ic_digits(int_width, per))
    blocks[[length(blocks) + 1L]] <- paste0(ic_sign(per), ic_digits(int_width, per), ".")
  }
  for (frac_width in 1:3) {
    blocks[[length(blocks) + 1L]] <- paste0(ic_sign(per), ".", ic_digits(frac_width, per))
  }
  c(
    unlist(blocks, use.names = FALSE),
    # The boundary literals the pattern was designed around, so that a change
    # to it that keeps the random shapes passing still fails here.
    "1.", ".5", "+4", "+.5", "-.5", "-0", "-0.0", "007", "0", "0.", ".0",
    "999999999999999", "999999999999999.", "-999999999999999",
    "123456789012.123", ".999", "0.001", "1.5", "000000000000001"
  )
}

# Literals drawn from exactly the shapes `DTA_ARROW_INTEGER_PATTERN` admits.
ic_integer_literals <- function(per = 900L) {
  blocks <- lapply(1:9, function(width) {
    paste0(sample(c("", "", "", "-"), per, replace = TRUE), ic_digits(width, per))
  })
  c(
    unlist(blocks, use.names = FALSE),
    "0", "-0", "007", "999999999", "-999999999", "000000000"
  )
}

# The fast path declines a batch below `DTAtools.stream_arrow_numeric_min_rows`
# (20,000 by default, because a small batch is cheaper to type in R). The
# batches built here are a few values long, so the threshold is lowered to 0
# for the rest of this file to exercise the path itself; its own test below
# pins the default.
withr::local_options(
  list(DTAtools.stream_arrow_numeric_min_rows = 0),
  .local_envir = teardown_env()
)

ic_num_specs <- function(type) {
  make_specs(DTAColumnSpec(id = "VAL", type = type, nullable = TRUE))
}

ic_parse_in_arrow <- function(literals, type) {
  batch <- arrow::record_batch(VAL = literals)
  state <- dta_arrow_numeric_state(dta_compile_spec_types(ic_num_specs(type)))
  as.data.frame(dta_arrow_parse_numeric_batch(batch, state))$VAL
}

ic_matches <- function(literals, pattern) {
  as.vector(arrow::call_function(
    "match_substring_regex",
    arrow::Array$create(literals, type = arrow::utf8()),
    options = list(pattern = pattern)
  ))
}


test_that("every accepted double literal parses to the same double in Arrow and in R", {
  # The claim the fast path rests on, tested against the R parse it replaces
  # rather than against a hand-written expectation. Seeded, so a failure is
  # reproducible and a pass is not luck of the draw.
  set.seed(20260906)
  literals <- ic_double_literals()
  expect_gte(length(literals), 50000L)

  in_arrow <- ic_parse_in_arrow(literals, "SAS Num")
  in_r <- dta_coerce_column(literals, "double")$values

  # `num.eq = FALSE` compares the bit patterns rather than with `==`. That is
  # the whole point: `identical(0, -0)` is TRUE by default, and two engines
  # disagreeing on the sign of a zero -- or on the last bit of a mantissa --
  # is exactly what `dta_row_key()` would go on to report as two distinct
  # values in a uniqueness rule.
  expect_true(identical(in_arrow, in_r, num.eq = FALSE))
  expect_type(in_arrow, "double")
})

test_that("every accepted integer literal parses to the same integer in Arrow and in R", {
  set.seed(20260906)
  literals <- ic_integer_literals()

  in_arrow <- ic_parse_in_arrow(literals, "SAS Int")
  # The R path parses to double and then narrows, and the narrowing is what
  # the 9-digit cap on the pattern exists to reproduce: every accepted literal
  # is whole and inside `.Machine$integer.max`, so the column comes back as R
  # `integer` on both routes rather than as a double on one of them.
  in_r <- dta_coerce_column(literals, "integer")$values

  expect_type(in_r, "integer")
  expect_identical(in_arrow, in_r)
})

test_that("a signed zero survives the Arrow route with its sign", {
  parsed <- ic_parse_in_arrow(c("-0", "0", "1"), "SAS Num")
  expect_identical(1 / parsed, c(-Inf, Inf, 1))
})

test_that("the patterns exclude every form the two parsers may disagree on", {
  excluded_double <- c(
    # An exponent sends R's parser through a scaling step, which is where its
    # double rounding happens; Arrow rounds once.
    "1e5", "1E5", "1e-400", "1e400", "1.5e10", "1e0",
    # Past 15 digits R's accumulator stops being exact.
    "12345678901234567890", "1234567890123456",
    # A fourth decimal puts the division past the point where rounding twice
    # provably matches rounding once.
    "1.2345", "0.1000000000000000",
    # Forms one engine accepts and the other does not, or neither does.
    " 1", "1 ", "1\t", "", "NA", "NaN", "Inf", "-inf", "0x1F", "1,5",
    ".", "+", "-", "+.", "1.2.3", "--1", "1_0", "1e", "1 000"
  )
  expect_equal(
    ic_matches(excluded_double, DTA_ARROW_DOUBLE_PATTERN),
    rep(FALSE, length(excluded_double))
  )

  excluded_integer <- c(
    # Arrow's integer parser refuses a leading `+`, so the pattern must too.
    "+4",
    # Past 9 digits a value can exceed `.Machine$integer.max`, where the R path
    # would keep the column a double instead of narrowing it.
    "1234567890", "2147483648", "-2147483648",
    "1.0", "1e5", "", " 4", "NA"
  )
  expect_equal(
    ic_matches(excluded_integer, DTA_ARROW_INTEGER_PATTERN),
    rep(FALSE, length(excluded_integer))
  )
})

test_that("one value outside the accepted forms keeps the whole column in R", {
  specs <- ic_num_specs("SAS Num")
  state <- dta_arrow_numeric_state(dta_compile_spec_types(specs))

  clean <- arrow::record_batch(VAL = c("1.5", "2.5", "3.5"))
  expect_identical(
    class(dta_arrow_parse_numeric_batch(clean, state)$column(0L)$type)[[1]],
    "Float64"
  )

  # `1e5` parses in both engines, and to the same double here -- but not
  # provably so for every literal of its shape, which is why the pattern
  # rejects it and one of them is enough to hold the column back.
  for (bad in c("1e5", "abc", "", " 2.5", "1.23456")) {
    batch <- arrow::record_batch(VAL = c("1.5", bad, "3.5"))
    fresh <- dta_arrow_numeric_state(dta_compile_spec_types(specs))
    expect_identical(
      class(dta_arrow_parse_numeric_batch(batch, fresh)$column(0L)$type)[[1]],
      "Utf8",
      info = bad
    )
  }
})

test_that("nulls alone do not hold a column back, and an all-null column is left as text", {
  specs <- ic_num_specs("SAS Num")

  mixed <- arrow::record_batch(VAL = c("1.5", NA, "3.5"))
  parsed <- dta_arrow_parse_numeric_batch(
    mixed, dta_arrow_numeric_state(dta_compile_spec_types(specs))
  )
  expect_identical(as.data.frame(parsed)$VAL, c(1.5, NA, 3.5))

  # An all-missing column is left exactly as the R path leaves it: text. A
  # column of NA doubles is a different column to the column spec axis, and
  # the two paths have to agree on it.
  empty <- arrow::record_batch(VAL = rep(NA_character_, 3))
  untouched <- dta_arrow_parse_numeric_batch(
    empty, dta_arrow_numeric_state(dta_compile_spec_types(specs))
  )
  expect_identical(class(untouched$column(0L)$type)[[1]], "Utf8")
})

test_that("a column that fails is not retried on the very next batch", {
  # The backoff is a scheduling decision, not a semantic one -- whichever way
  # it goes the column is typed in R and its bad values reported -- but it is
  # what keeps a file that is dirty throughout from paying for a test that can
  # never succeed on it, once per column per batch.
  specs <- ic_num_specs("SAS Num")
  state <- dta_arrow_numeric_state(dta_compile_spec_types(specs))
  column_type <- function(batch) class(batch$column(0L)$type)[[1]]

  dirty <- arrow::record_batch(VAL = c("1.5", "abc"))
  clean <- arrow::record_batch(VAL = c("1.5", "2.5"))

  expect_identical(column_type(dta_arrow_parse_numeric_batch(dirty, state)), "Utf8")
  # Skipped, though this batch would have qualified.
  expect_identical(column_type(dta_arrow_parse_numeric_batch(clean, state)), "Utf8")
  # And picked up again on the batch after it.
  expect_identical(column_type(dta_arrow_parse_numeric_batch(clean, state)), "Float64")
})

test_that("a batch that qualifies spends the column's history of failing", {
  # Without the reset the wait only ever doubles, so a column with one bad
  # value every few thousand batches -- which is what a real dirty file looks
  # like -- ends the scan being skipped 1,024 batches at a time and pays the R
  # path for almost all of them. The backoff exists to bound wasted work on a
  # file that is dirty EVERYWHERE, and such a file never qualifies at all.
  specs <- ic_num_specs("SAS Num")
  state <- dta_arrow_numeric_state(dta_compile_spec_types(specs))
  column_type <- function(batch) class(batch$column(0L)$type)[[1]]
  feed <- function(batch) column_type(dta_arrow_parse_numeric_batch(batch, state))

  dirty <- arrow::record_batch(VAL = c("1.5", "abc"))
  clean <- arrow::record_batch(VAL = c("1.5", "2.5"))

  expect_identical(feed(dirty), "Utf8")
  expect_identical(feed(clean), "Utf8") # serving the wait of 1
  expect_identical(feed(clean), "Float64") # qualifies, and resets
  expect_identical(state$backoff[["VAL"]], 1)

  # The discriminating sequence: a second dirty patch. Reset, the column waits
  # one batch again; unreset, it would have waited the two its doubled backoff
  # had reached and this last batch would still be text.
  expect_identical(feed(dirty), "Utf8")
  expect_identical(feed(clean), "Utf8")
  expect_identical(feed(clean), "Float64")

  # A column that never recovers still backs off, which is the whole point.
  always <- dta_arrow_numeric_state(dta_compile_spec_types(specs))
  for (i in 1:4) {
    expect_identical(
      class(dta_arrow_parse_numeric_batch(dirty, always)$column(0L)$type)[[1]],
      "Utf8"
    )
  }
  expect_gt(always$backoff[["VAL"]], 1)
})

test_that("the fast path leaves alone what is not its business", {
  specs <- make_specs(
    DTAColumnSpec(id = "VAL", type = "SAS Num", nullable = TRUE),
    DTAColumnSpec(id = "TXT", type = "SAS Char", length = 4, nullable = TRUE)
  )
  type_map <- dta_compile_spec_types(specs)

  # A declared-Char column, an undeclared column, and a column that is already
  # numeric are all untouched.
  batch <- arrow::record_batch(
    VAL = c(1.5, 2.5), TXT = c("0012", "0034"), OTHER = c("7", "8")
  )
  out <- dta_arrow_parse_numeric_batch(batch, dta_arrow_numeric_state(type_map))
  expect_identical(
    vapply(seq_len(3L), function(i) class(out$column(i - 1L)$type)[[1]], character(1)),
    c("Float64", "Utf8", "Utf8")
  )

  # Anything that is not an Arrow batch, and a state of NULL, are no-ops.
  df <- data.frame(VAL = c("1", "2"), stringsAsFactors = FALSE)
  expect_identical(dta_arrow_parse_numeric_batch(df, dta_arrow_numeric_state(type_map)), df)
  expect_identical(dta_arrow_parse_numeric_batch(batch, NULL), batch)
})

test_that("the fast path is switched off entirely by its diagnostic option", {
  specs <- ic_num_specs("SAS Num")
  type_map <- dta_compile_spec_types(specs)

  withr::local_options(DTAtools.stream_arrow_numeric = FALSE)
  expect_null(dta_arrow_numeric_state(type_map))

  # And no state means no work: the batch comes back byte for byte as it went
  # in, so the R path sees exactly what it saw before this existed.
  batch <- arrow::record_batch(VAL = c("1.5", "2.5"))
  expect_identical(
    class(
      dta_arrow_parse_numeric_batch(batch, dta_arrow_numeric_state(type_map))$column(0L)$type
    )[[1]],
    "Utf8"
  )
})

test_that("a specification with no numeric column builds no state at all", {
  # The per-batch cost of the fast path where it can do nothing is then one
  # `is.null()`, not a walk over every column of every batch.
  specs <- make_specs(DTAColumnSpec(id = "TXT", type = "SAS Char", length = 4))
  expect_null(dta_arrow_numeric_state(dta_compile_spec_types(specs)))
  expect_null(dta_arrow_numeric_state(character(0)))
})

test_that("the Arrow numeric path declines a batch below the row threshold", {
  specs <- make_specs(DTAColumnSpec(id = "VAL", type = "SAS Num", nullable = TRUE))
  batch <- arrow::record_batch(VAL = sprintf("%.2f", seq_len(50) / 7))
  column_type <- function(b) class(b$column(0L)$type)[[1]]

  # The default threshold (20,000 rows) leaves a 50-row batch to R: every
  # Arrow call costs the same whatever the batch holds, and at that size the
  # R parse is the cheaper one.
  withr::with_options(list(DTAtools.stream_arrow_numeric_min_rows = NULL), {
    state <- dta_arrow_numeric_state(dta_compile_spec_types(specs))
    expect_identical(state$min_rows, 20000)
    expect_identical(column_type(dta_arrow_parse_numeric_batch(batch, state)), "Utf8")
  })

  # At or above the threshold the same batch is cast.
  withr::with_options(list(DTAtools.stream_arrow_numeric_min_rows = 50), {
    state <- dta_arrow_numeric_state(dta_compile_spec_types(specs))
    expect_identical(column_type(dta_arrow_parse_numeric_batch(batch, state)), "Float64")
  })

  # A threshold that is not a single non-negative whole number is refused.
  for (bad in list(NA, -1, 1.5, c(1, 2), "20000")) {
    withr::with_options(list(DTAtools.stream_arrow_numeric_min_rows = bad), {
      expect_error(
        dta_arrow_numeric_state(dta_compile_spec_types(specs)),
        regexp = "stream_arrow_numeric_min_rows",
        class = "rlang_error"
      )
    })
  }
})
