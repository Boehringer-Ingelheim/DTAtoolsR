test_that("DTAFileCSV object is created from reading in csv and table is accessible", {
  path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")

  file_info <- DTAFileCSV("clinical_data.csv")

  expect_s3_class(file_info, "DTAtools::DTAFile")
  expect_s3_class(file_info, "DTAtools::DTAFileCSV")

  expect_true(matches_filename(file_info, basename(path)))

  x <- read_file(file_info, path)

  # `expect_s3_class()` with a character vector is an ANY-match, so it passes
  # on a mis-parsed table. Assert every class and the parsed shape instead.
  expect_true(all(c("Table", "ArrowTabular") %in% class(x)))
  expect_equal(ncol(x), 14)
  expect_true(all(c("STUDYID", "AGE", "GENDER", "SUBJECT_ID") %in% names(x)))
})


test_that("matches_filename accepts full file paths", {
  path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
  file_info <- DTAFileCSV("clinical_data.csv")

  expect_true(matches_filename(file_info, path))
})

test_that("CSV headers are normalized before validation", {
  path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
  file_info <- DTAFileCSV("clinical_data.csv")

  x <- read_file(file_info, path)

  expect_true("AGE" %in% names(x))
  expect_false("\"AGE\"" %in% names(x))
  expect_false("AGE " %in% names(x))
})

test_that("dta_normalize_column_names strips quotes and surrounding whitespace", {
  # The fixture above reaches this helper already clean (arrow removes the
  # quotes), so the integration test passes even if the helper is deleted.
  # Exercise the helper directly on input that genuinely needs cleaning.
  tbl <- arrow::arrow_table(data.frame(a = 1, b = 2, c = 3))
  names(tbl) <- c('"AGE"', " SEX ", '  "RACE"  ')

  expect_equal(names(dta_normalize_column_names(tbl)), c("AGE", "SEX", "RACE"))
})

test_that("dta_normalize_column_names leaves already-clean names untouched", {
  tbl <- arrow::arrow_table(data.frame(AGE = 1, SEX = 2))

  expect_equal(names(dta_normalize_column_names(tbl)), c("AGE", "SEX"))
})


test_that("DTAFileTSV object is created from reading in tsv and table is accessible", {
  path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

  file_info <- DTAFileTSV("gf_data_small_smirna.tsv")

  expect_s3_class(file_info, "DTAtools::DTAFile")
  expect_s3_class(file_info, "DTAtools::DTAFileTSV")

  expect_true(matches_filename(file_info, basename(path)))

  x <- read_file(file_info, path)

  expect_true(all(c("Table", "ArrowTabular") %in% class(x)))
  # A tab file parsed with the wrong delimiter collapses to a single column,
  # which the previous ANY-match class assertion happily accepted.
  expect_gt(ncol(x), 1)
  expect_true(all(c("STUDYID", "DOMAIN") %in% names(x)))
})


test_that("Testing pattern with DTAFileTSV", {
  path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

  file_info <- DTAFileTSV("gf_data_.*\\.tsv", pattern = TRUE, number_of_files = 1)

  expect_s3_class(file_info, "DTAtools::DTAFile")
  expect_s3_class(file_info, "DTAtools::DTAFileTSV")

  expect_true(matches_filename(file_info, basename(path)))

  x <- read_file(file_info, path)

  expect_s3_class(x, c("R6", "Table", "ArrowTabular", "ArrowObject"))
})


test_that("Testing wrong pattern with DTAFileTSV", {
  path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

  file_info <- DTAFileTSV("data_gf_dfjlkadwefwfew.*\\.tsv", pattern = TRUE, number_of_files = 1)

  expect_false(matches_filename(file_info, basename(path)))
})

test_that("DTAFile Creation", {
  path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

  file_info <- DTAFile("gf_data_small_smirna.tsv")

  expect_s3_class(file_info, "DTAtools::DTAFile")

  expect_true(matches_filename(file_info, basename(path)))
})

test_that("DTAFile rejects a fixed filename combined with number_of_files > 1", {
  expect_error(
    DTAFile("gf_data_small_smirna.tsv", number_of_files = 2),
    "number_of_files"
  )
})

test_that("DTAFile pattern accessors report the configured bounds", {
  file_info <- DTAFile(
    "gf_data_.*\\.tsv",
    pattern = TRUE,
    min_number_of_files = 1,
    max_number_of_files = 3
  )

  expect_equal(min_number_of_files(file_info), 1)
  expect_equal(max_number_of_files(file_info), 3)
})

test_that("read_file refuses a path whose basename does not match", {
  other <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
  file_info <- DTAFileTSV("gf_data_small_smirna.tsv")

  # Bare expect_error() would also pass on a typo in the call itself.
  expect_error(read_file(file_info, other), "match")
})

test_that("read_file with namecheck = FALSE reads a non-matching path", {
  path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")
  file_info <- DTAFileTSV("some_other_name.tsv")

  x <- read_file(file_info, path, namecheck = FALSE)

  expect_true(all(c("Table", "ArrowTabular") %in% class(x)))
  expect_gt(ncol(x), 1)
})

test_that("DTAFileDelim reads tab-delimited files", {
  path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

  file_info <- DTAFileDelim("gf_data_small_smirna.tsv")

  expect_s3_class(file_info, "DTAtools::DTAFile")
  expect_s3_class(file_info, "DTAtools::DTAFileDelim")
  expect_true(matches_filename(file_info, basename(path)))

  x <- read_file(file_info, path)
  expect_s3_class(x, c("R6", "Table", "ArrowTabular", "ArrowObject"))
})


# ---------------------------------------------------------------------------
# DTAFileTabular class registration
# ---------------------------------------------------------------------------

test_that("DTAFileTabular is registered under its own class name", {
  # DTAFileTabular used to be created with S7::new_class("DTAFile", ...), so it
  # shadowed its own parent: the class vector listed "DTAtools::DTAFile" twice
  # and every DTAFileTabular method overwrote the DTAFile method of the same
  # name.
  tabular <- DTAFileTabular("a.txt")

  expect_s3_class(tabular, "DTAtools::DTAFileTabular")
  expect_equal(
    class(DTAFileCSV("a.csv")),
    c(
      "DTAtools::DTAFileCSV",
      "DTAtools::DTAFileTabular",
      "DTAtools::DTAFile",
      "S7_object"
    )
  )
})

test_that("print() works on a plain DTAFile", {
  # While DTAFileTabular was registered as "DTAFile" its print_info method
  # replaced the DTAFile one, so printing a plain DTAFile looked for @sep.
  out <- cli::cli_fmt(print(DTAFile("a.txt")))

  expect_true(any(grepl("a.txt", out, fixed = TRUE)))
  expect_true(any(grepl("Number of files", out, fixed = TRUE)))
})

test_that("read_file_execution on a plain DTAFile hits the base abort", {
  err <- expect_error(
    read_file_execution(DTAFile("a.txt"), "x"),
    class = "rlang_error"
  )

  expect_match(conditionMessage(err), "not implemented")
  # The DTAFileTabular abort ("derived from DTAFileTabular class") used to be
  # reached instead, because it was registered against DTAFile.
  expect_false(grepl("DTAFileTabular", conditionMessage(err), fixed = TRUE))
})

test_that("read_file_execution on a DTAFileTabular hits the subclass abort", {
  err <- expect_error(
    read_file_execution(DTAFileTabular("a.txt"), "x"),
    class = "rlang_error"
  )

  expect_match(conditionMessage(err), "DTAFileTabular")
})


# ---------------------------------------------------------------------------
# DTAFileTabular validator
# ---------------------------------------------------------------------------

test_that("DTAFileTabular rejects a multi-character separator", {
  # Was inert: the message was computed by a bare `if` and thrown away, so the
  # object constructed successfully.
  expect_error(DTAFileTabular("a.txt", sep = "||"), "'sep' must be a single")
})

test_that("DTAFileTabular rejects a non-scalar has_header", {
  expect_error(
    DTAFileTabular("a.txt", has_header = c(TRUE, FALSE)),
    "'has_header' must be a single"
  )
})

test_that("DTAFileTabular rejects a multi-character quote", {
  expect_error(DTAFileTabular("a.txt", quote = "ab"), "'quote' must be a single")
})

test_that("DTAFileTabular reports every validator violation at once", {
  err <- expect_error(
    DTAFileTabular(
      "a.txt",
      sep = "||",
      has_header = c(TRUE, FALSE),
      quote = "ab"
    )
  )

  msg <- conditionMessage(err)
  expect_match(msg, "'sep' must be a single character.", fixed = TRUE)
  expect_match(msg, "'has_header' must be a single logical value.", fixed = TRUE)
  expect_match(msg, "'quote' must be a single character.", fixed = TRUE)
})

test_that("DTAFileTabular accepts a valid configuration", {
  tabular <- DTAFileTabular("a.txt", sep = ";", has_header = FALSE, quote = "'")

  expect_equal(tabular@sep, ";")
  expect_false(tabular@has_header)
  expect_equal(tabular@quote, "'")
})


# ---------------------------------------------------------------------------
# Separator handling
# ---------------------------------------------------------------------------

test_that("DTAFileDelim parses with its own separator, not a hardcoded comma", {
  path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

  x <- read_file(DTAFileDelim("gf_data_small_smirna.tsv"), path)

  # read_delim_arrow() was called without `delim`, so it fell back to a comma
  # and collapsed the tab file into a single column.
  expect_equal(ncol(x), 33)
  expect_equal(nrow(x), 490)
  expect_true(all(c("STUDYID", "DOMAIN") %in% names(x)))
})

test_that("DTAFileDelim honours a non-tab separator", {
  path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")

  x <- read_file(DTAFileDelim("clinical_data.csv", sep = ","), path)

  expect_equal(ncol(x), 14)
  expect_true(all(c("STUDYID", "AGE") %in% names(x)))
})


# ---------------------------------------------------------------------------
# has_header handling
# ---------------------------------------------------------------------------

test_that("DTAFileTSV with has_header = FALSE keeps the first row as data", {
  path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

  x <- read_file(DTAFileTSV("gf_data_small_smirna.tsv", has_header = FALSE), path)

  # has_header = FALSE used to be implemented as skip = 1, which discarded the
  # first *data* row and promoted the second one to the header (489 rows).
  expect_equal(nrow(x), 491)
  expect_equal(ncol(x), 33)
  expect_false("STUDYID" %in% names(x))
  expect_false("1234-5678" %in% names(x))
})

test_that("DTAFileDelim with has_header = FALSE keeps the first row as data", {
  path <- system.file("extdata", "gf_data_small_smirna.tsv", package = "DTAtools")

  x <- read_file(DTAFileDelim("gf_data_small_smirna.tsv", has_header = FALSE), path)

  expect_equal(nrow(x), 491)
  expect_equal(ncol(x), 33)
  expect_false("STUDYID" %in% names(x))
})

test_that("DTAFileCSV with has_header = FALSE keeps the first row as data", {
  path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")

  with_header <- read_file(DTAFileCSV("clinical_data.csv"), path)
  # has_header was ignored outright for CSV (the skip= line was commented out).
  without_header <- read_file(
    DTAFileCSV("clinical_data.csv", has_header = FALSE),
    path
  )

  expect_equal(nrow(without_header), nrow(with_header) + 1)
  expect_equal(ncol(without_header), ncol(with_header))
  expect_false("STUDYID" %in% names(without_header))
})


# ---------------------------------------------------------------------------
# Spec-driven column types at read time
# ---------------------------------------------------------------------------

# Arrow infers a column's type from its contents, and that inference runs
# *before* any code in this package sees the data: a column of quoted subject
# ids reads as int64 and arrives in R as 7 and 8, with the leading zeros
# already destroyed. The coercion choke point cannot repair that, because the
# damage precedes it. The specs are therefore offered to the reader, so a
# column the specification declares as text is read as text.

# Three columns: one declared Char whose text must survive, one declared Int
# that must still parse as a number, and one the specs say nothing about.
dta_write_id_fixture <- function(name, sep) {
  path <- file.path(tempdir(), name)
  row <- function(...) paste(c(...), collapse = sep)
  writeLines(
    c(
      row('"SUBJID"', '"AGE"', '"EXTRA"'),
      row('"007"', "30", "10"),
      row('"008"', "41", "20")
    ),
    path
  )
  path
}

dta_id_specs <- function() {
  DTAColumnSpecCollection(
    columns = list(
      DTAColumnSpec(id = "SUBJID", type = "SAS Char", format = "SAS $8."),
      DTAColumnSpec(id = "AGE", type = "SAS Int", format = "SAS 8.")
    )
  )
}

test_that("read_file keeps a declared Char column as text for CSV", {
  path <- dta_write_id_fixture("dta_ids_read.csv", ",")
  on.exit(unlink(path), add = TRUE)

  x <- read_file(DTAFileCSV(basename(path)), path, specs = dta_id_specs())
  df <- as.data.frame(x)

  expect_type(df$SUBJID, "character")
  expect_equal(df$SUBJID, c("007", "008"))
})

test_that("read_file keeps a declared Char column as text for TSV", {
  path <- dta_write_id_fixture("dta_ids_read.tsv", "\t")
  on.exit(unlink(path), add = TRUE)

  x <- read_file(DTAFileTSV(basename(path)), path, specs = dta_id_specs())
  df <- as.data.frame(x)

  expect_type(df$SUBJID, "character")
  expect_equal(df$SUBJID, c("007", "008"))
})

test_that("read_file keeps a declared Char column as text for Delim", {
  path <- dta_write_id_fixture("dta_ids_read.psv", "|")
  on.exit(unlink(path), add = TRUE)

  x <- read_file(DTAFileDelim(basename(path), sep = "|"), path, specs = dta_id_specs())
  df <- as.data.frame(x)

  expect_type(df$SUBJID, "character")
  expect_equal(df$SUBJID, c("007", "008"))
})

test_that("read_file reads a column the specs do not mention as text", {
  path <- dta_write_id_fixture("dta_ids_extra.csv", ",")
  on.exit(unlink(path), add = TRUE)

  x <- read_file(DTAFileCSV(basename(path)), path, specs = dta_id_specs())
  df <- as.data.frame(x)

  # EXTRA has no spec, and is still not dropped -- but once ANY specs are
  # supplied it is read as text rather than inferred. The lazy path has always
  # had to pin every column (a dataset locks in a type from its first block),
  # so leaving this one to inference here made the SAME file present EXTRA as a
  # double in memory and as a string when streamed; a uniqueness rule over it
  # then counted "1.5" and "1.50" as one key on one path and two on the other.
  expect_true("EXTRA" %in% names(df))
  expect_type(df$EXTRA, "character")
  expect_equal(df$EXTRA, c("10", "20"))
})

test_that("both readers type an undeclared column identically", {
  # The generalisation of the case above: whatever the reader decides, it has
  # to decide it once. Compared as data rather than as a class, because
  # as.data.frame() on a Dataset returns a tibble and on a Table a data.frame.
  path <- dta_write_id_fixture("dta_ids_bothpaths.csv", ",")
  on.exit(unlink(path), add = TRUE)

  handler <- DTAFileCSV(basename(path))
  eager <- as.data.frame(read_file(handler, path, specs = dta_id_specs()))
  lazy <- as.data.frame(open_file(handler, path, specs = dta_id_specs()))

  expect_identical(names(eager), names(lazy))
  for (column in names(eager)) {
    expect_identical(eager[[column]], lazy[[column]], info = column)
  }
})

test_that("read_file without specs still infers every column as before", {
  path <- dta_write_id_fixture("dta_ids_nospec.csv", ",")
  on.exit(unlink(path), add = TRUE)

  bare <- read_file(DTAFileCSV(basename(path)), path)

  # The standalone no-specs call is unchanged: arrow still guesses, and still
  # guesses wrong for "007". This is the documented behaviour of read_file()
  # on a bare DTAFile, and the fix deliberately does not alter it.
  expect_true(all(c("Table", "ArrowTabular") %in% class(bare)))
  expect_equal(ncol(bare), 3)
  expect_equal(names(bare), c("SUBJID", "AGE", "EXTRA"))
  expect_equal(as.numeric(as.data.frame(bare)$SUBJID), c(7, 8))

  # Passing specs = NULL explicitly must be the same call.
  explicit <- read_file(DTAFileCSV(basename(path)), path, specs = NULL)
  expect_equal(as.data.frame(explicit), as.data.frame(bare))
})

test_that("dta_reader_args finds the file whatever order it was given in", {
  specs <- dta_id_specs()

  # read_file_execution() dispatches on `x` alone and takes the rest through
  # `...`, so a `list(...)[[1]]` file would silently become the specs as soon
  # as a caller named its arguments in the other order.
  expect_equal(dta_reader_args("f.csv")$file, "f.csv")
  expect_equal(dta_reader_args("f.csv", specs = specs)$file, "f.csv")
  expect_equal(dta_reader_args(specs = specs, "f.csv")$file, "f.csv")
  expect_equal(dta_reader_args(file = "f.csv", specs = specs)$file, "f.csv")

  expect_identical(dta_reader_args("f.csv", specs = specs)$specs, specs)
  expect_null(dta_reader_args("f.csv")$specs)
})

test_that("dta_reader_args rejects a call with no file", {
  expect_error(dta_reader_args(specs = dta_id_specs()), "file")
})

test_that("specs cannot name a column when the file has no header", {
  path <- dta_write_id_fixture("dta_ids_noheader.csv", ",")
  on.exit(unlink(path), add = TRUE)

  # Arrow generates positional names (f0, f1, ...) that cannot correspond to
  # spec ids, so nothing the specs declare can be matched -- and the first line
  # is data, not a header, so all three lines are rows.
  x <- read_file(
    DTAFileCSV(basename(path), has_header = FALSE),
    path,
    specs = dta_id_specs()
  )

  expect_equal(nrow(x), 3)
  expect_equal(ncol(x), 3)
  expect_false("SUBJID" %in% names(x))
})


# ---------------------------------------------------------------------------
# One reader plan for both paths
# ---------------------------------------------------------------------------
# The eager and the lazy reader are two calls into arrow with the same
# configuration, derived once by dta_delim_reader_plan(). What follows pins
# that configuration itself, because the failures it prevents (a header parsed
# as a data row, a first data row silently dropped) are invisible in a verdict
# and only show up as an off-by-one row count.

# A header that needs cleaning and one that does not, with the SAME two data
# rows, so the row count answers one question only: was the right line skipped?
dta_write_header_fixture <- function(name, header) {
  path <- file.path(tempdir(), name)
  writeLines(c(header, "42,M", "51,F"), path)
  path
}

test_that("the header line is skipped and no data row is lost, on both readers", {
  cases <- list(
    clean = "AGE,GENDER",
    padded = '" AGE ","GENDER "'
  )

  for (label in names(cases)) {
    path <- dta_write_header_fixture(paste0("dta_hdr_", label, ".csv"), cases[[label]])
    on.exit(unlink(path), add = TRUE)

    handler <- DTAFileCSV(basename(path))
    eager <- as.data.frame(read_file(handler, path))
    lazy <- as.data.frame(open_file(handler, path))

    for (got in list(eager = eager, lazy = lazy)) {
      expect_identical(names(got), c("AGE", "GENDER"), info = label)
      expect_equal(nrow(got), 2, info = label)
      # The first data row is the one a wrong `skip` eats; the header is what a
      # missing `skip` adds back as a row of the literal words.
      expect_identical(as.character(got$AGE), c("42", "51"), info = label)
      expect_identical(as.character(got$GENDER), c("M", "F"), info = label)
    }
  }
})

test_that("with no header every line is data, on both readers", {
  path <- dta_write_header_fixture("dta_hdr_none.csv", "AGE,GENDER")
  on.exit(unlink(path), add = TRUE)

  handler <- DTAFileCSV(basename(path), has_header = FALSE)
  eager <- as.data.frame(read_file(handler, path))
  lazy <- as.data.frame(open_file(handler, path))

  for (got in list(eager = eager, lazy = lazy)) {
    expect_equal(nrow(got), 3)
    expect_identical(names(got), c("f0", "f1"))
    expect_identical(as.character(got$f0), c("AGE", "42", "51"))
  }
})

test_that("the reader plan derives one configuration for both paths", {
  path <- dta_write_header_fixture("dta_plan_padded.csv", '" AGE ","GENDER "')
  on.exit(unlink(path), add = TRUE)

  plan <- dta_delim_reader_plan(path, delim = ",", quote = '"', has_header = TRUE)

  expect_identical(plan$column_names, c("AGE", "GENDER"))
  expect_identical(plan$skip, 1L)
  # No specs: inference is left alone, exactly as a bare read_file() has always
  # behaved.
  expect_null(plan$col_types)

  with_specs <- dta_delim_reader_plan(
    path,
    specs = dta_id_specs(), delim = ",", quote = '"', has_header = TRUE
  )
  # EVERY column, not only the declared ones -- neither AGE nor GENDER is in
  # dta_id_specs()'s SUBJID/AGE pair by name and type both, but both are pinned.
  expect_identical(names(with_specs$col_types), c("AGE", "GENDER"))
  expect_true(all(vapply(
    with_specs$col_types$fields,
    function(f) f$type$Equals(arrow::utf8()),
    logical(1)
  )))

  no_header <- dta_delim_reader_plan(path, delim = ",", quote = '"', has_header = FALSE)
  expect_identical(no_header$skip, 0L)
  expect_identical(no_header$column_names, c("f0", "f1"))
})

test_that("a header arrow cannot use is refused naming the file", {
  path <- file.path(tempdir(), "dta_dup_header.csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("A,A,B", "1,2,3"), path)

  # Arrow reports this as "Could not read schema ... Is this a 'csv' file?",
  # which sends the reader looking at the file format rather than at its first
  # line. Both paths now say what is actually wrong, and say it identically.
  expect_error(read_file(DTAFileCSV(basename(path)), path), "column names")
  expect_error(open_file(DTAFileCSV(basename(path)), path), "column names")
})

test_that("names that collide only after cleaning are refused", {
  path <- file.path(tempdir(), "dta_collide_header.csv")
  on.exit(unlink(path), add = TRUE)
  # Two distinct names to arrow, one name after the quotes and spaces go.
  writeLines(c('"A"," A ",B', "1,2,3"), path)

  expect_error(read_file(DTAFileCSV(basename(path)), path), "repeated column names")
  expect_error(open_file(DTAFileCSV(basename(path)), path), "repeated column names")
})


# ---------------------------------------------------------------------------
# Real-world parse settings on the handler
# ---------------------------------------------------------------------------

test_that("a tabular handler carries the two parse settings with harmless defaults", {
  for (ctor in list(DTAFileCSV, DTAFileTSV, DTAFileDelim, DTAFileTabular)) {
    h <- ctor(filename = "a.csv")
    expect_false(h@newlines_in_values)
    expect_equal(h@encoding, "UTF-8")
  }

  declared <- DTAFileCSV(
    filename = "a.csv",
    newlines_in_values = TRUE,
    encoding = "latin1"
  )
  expect_true(declared@newlines_in_values)
  expect_equal(declared@encoding, "latin1")
})

test_that("the two parse settings are validated", {
  expect_error(
    DTAFileCSV(filename = "a.csv", newlines_in_values = NA),
    "newlines_in_values"
  )
  expect_error(
    DTAFileCSV(filename = "a.csv", newlines_in_values = c(TRUE, TRUE)),
    "newlines_in_values"
  )
  expect_error(DTAFileCSV(filename = "a.csv", encoding = ""), "encoding")
  expect_error(DTAFileCSV(filename = "a.csv", encoding = NA_character_), "encoding")
  expect_error(
    DTAFileCSV(filename = "a.csv", encoding = c("UTF-8", "latin1")),
    "encoding"
  )
})

test_that("the two parse settings survive the factory and a YAML round trip", {
  from_factory <- DTAFileFactory(
    type = "csv", filename = "a.csv",
    newlines_in_values = TRUE, encoding = "latin1"
  )
  expect_true(from_factory@newlines_in_values)
  expect_equal(from_factory@encoding, "latin1")

  # The route a specification document actually takes: YAML text -> list ->
  # DTAFileFactory. A key that never reaches the constructor is a key a user
  # can write and never see honoured.
  parsed <- yaml::yaml.load(paste(
    "name: encoded",
    "type: tabular",
    "files:",
    "  type: tsv",
    "  filename: a.tsv",
    "  newlines_in_values: true",
    "  encoding: latin1",
    "columns:",
    "  - id: STUDYID",
    "    type: SAS Char",
    sep = "\n"
  ))
  ds <- dta_dataset_from_list(parsed)

  expect_true(ds@files[[1]]@newlines_in_values)
  expect_equal(ds@files[[1]]@encoding, "latin1")
})

test_that("print_info reports the two parse settings", {
  out <- paste(
    capture_messages(print_info(DTAFileCSV("a.csv", encoding = "latin1"))),
    collapse = ""
  )

  expect_match(out, "Newlines in values")
  expect_match(out, "Encoding")
  expect_match(out, "latin1")
})


# ---------------------------------------------------------------------------
# Converting a file that is not UTF-8
# ---------------------------------------------------------------------------
# Arrow's dataset scanner has no re-encoding step, so the lazy reader converts
# the file once into a UTF-8 copy and scans that. These pin the converter
# itself; test-load-file-streaming.R pins what the two readers then agree on.

# `bytes` is written verbatim, so a fixture can declare exactly which encoding
# it is in rather than depending on the session's.
transcode_fixture <- function(name, bytes, gz = FALSE) {
  path <- file.path(tempdir(), name)
  con <- if (gz) gzfile(path, "wb") else file(path, "wb")
  on.exit(close(con), add = TRUE)
  writeBin(bytes, con)
  path
}

# "Jürgen,1" and "Möller,2" under a two-column header, in latin1.
latin1_body <- function(terminator = "\n") {
  c(
    charToRaw(paste0("NAME,V", terminator)),
    as.raw(c(0x4a, 0xfc, 0x72, 0x67, 0x65, 0x6e)), charToRaw(paste0(",1", terminator)),
    as.raw(c(0x4d, 0xf6, 0x6c, 0x6c, 0x65, 0x72)), charToRaw(paste0(",2", terminator))
  )
}

test_that("only UTF-8 counts as UTF-8, and only the wide encodings are wide", {
  expect_true(dta_encoding_is_utf8("UTF-8"))
  expect_true(dta_encoding_is_utf8("utf-8"))
  expect_false(dta_encoding_is_utf8("UTF8"))
  expect_false(dta_encoding_is_utf8("latin1"))

  # A newline byte is part of an ordinary character in these, so the line-based
  # converter cannot be used on them.
  for (wide in c("UTF-16", "utf16le", "UTF-16BE", "UTF-32", "UCS-2", "ucs-4")) {
    expect_true(dta_encoding_is_wide(wide), info = wide)
  }
  for (narrow in c("UTF-8", "latin1", "ISO-8859-15", "windows-1252", "SHIFT-JIS", "CP1251")) {
    expect_false(dta_encoding_is_wide(narrow), info = narrow)
  }
})

test_that("the transcoding block is an option, validated like the read block", {
  expect_equal(dta_transcode_block_bytes(), 4194304L)

  withr::local_options(DTAtools.transcode_block_bytes = 128)
  expect_identical(dta_transcode_block_bytes(), 128L)

  withr::local_options(DTAtools.transcode_block_bytes = 0)
  expect_error(dta_transcode_block_bytes(), "between 1 and")
  withr::local_options(DTAtools.transcode_block_bytes = c(1, 2))
  expect_error(dta_transcode_block_bytes(), "between 1 and")
})

test_that("the copy is the delivered bytes re-encoded and nothing else", {
  # CRLF input, so the claim being made is the one that matters: the copy is
  # what `iconv()` of the whole file would produce, line endings included. A
  # line-based converter folded CRLF to LF here, which is invisible in an
  # unquoted field and one character short inside a quoted one.
  path <- transcode_fixture("dta_transcode_crlf.csv", latin1_body("\r\n"))
  on.exit(unlink(path), add = TRUE)

  copy <- dta_transcode_to_utf8(path, "latin1")
  bytes <- readBin(copy, "raw", n = file.size(copy))

  expect_identical(
    bytes,
    charToRaw(iconv(
      rawToChar(readBin(path, "raw", n = file.size(path))),
      from = "latin1", to = "UTF-8"
    ))
  )

  # "Jürgen" is 0x4a 0xfc ... in latin1 and 0x4a 0xc3 0xbc ... in UTF-8.
  expect_true(grepl("\xc3\xbc", rawToChar(bytes), fixed = TRUE, useBytes = TRUE))
  # Every CR the delivery carried is still there, and the file still ends the
  # way it was delivered.
  expect_identical(sum(bytes == as.raw(0x0d)), 3L)
  expect_identical(bytes[(length(bytes) - 1L):length(bytes)], as.raw(c(0x0d, 0x0a)))

  lines <- readLines(copy, encoding = "UTF-8")
  expect_identical(lines[[1]], "NAME,V")
  expect_length(lines, 3L)
})

test_that("a CR that is not a line ending survives the conversion", {
  # A lone CR inside a quoted value, and a CRLF inside another. Neither is a
  # line break -- they are data -- and `readLines()` turns both into LF, which
  # made the streamed value one character shorter than the in-memory one.
  body <- c(
    charToRaw("NAME,V\n"),
    charToRaw("\"a\rb\","), as.raw(0xfc), charToRaw("\n"),
    charToRaw("\"c\r\nd\",x\n")
  )
  path <- transcode_fixture("dta_transcode_cr.csv", body)
  on.exit(unlink(path), add = TRUE)

  copy <- dta_transcode_to_utf8(path, "latin1")
  bytes <- readBin(copy, "raw", n = file.size(copy))

  # The only byte that changed is the latin1 "ü", which became two UTF-8 bytes.
  expect_identical(
    bytes,
    c(
      charToRaw("NAME,V\n"),
      charToRaw("\"a\rb\","), as.raw(c(0xc3, 0xbc)), charToRaw("\n"),
      charToRaw("\"c\r\nd\",x\n")
    )
  )
})

test_that("the copy does not depend on how many bytes a block holds", {
  # A body long enough that a 16-byte block cuts it in many places, with a
  # multi-byte character straddling several of them and one line longer than
  # the block itself.
  body <- c(
    charToRaw("NAME,V\n"),
    unlist(lapply(1:20, function(i) {
      c(as.raw(c(0x4a, 0xfc, 0x72)), charToRaw(paste0(i, ",", i, "\n")))
    })),
    charToRaw(paste0(strrep("x", 100), ",1\n"))
  )

  path <- transcode_fixture("dta_transcode_blocks.csv", body)
  on.exit(unlink(path), add = TRUE)

  # A block far smaller than one line against the whole file at once: the block
  # bounds memory, it must not touch the answer. A block that ends mid-line, or
  # mid-character, is exactly what the carry exists for.
  withr::local_options(DTAtools.transcode_block_bytes = 16)
  by_block <- readBin(
    copy_one <- dta_transcode_to_utf8(path, "latin1"),
    "raw",
    n = file.size(copy_one)
  )

  # A fresh source, because the first copy is now cached against this one.
  other <- transcode_fixture("dta_transcode_blocks2.csv", body)
  on.exit(unlink(other), add = TRUE)
  withr::local_options(DTAtools.transcode_block_bytes = 4194304L)
  at_once <- readBin(
    copy_all <- dta_transcode_to_utf8(other, "latin1"),
    "raw",
    n = file.size(copy_all)
  )

  expect_identical(by_block, at_once)
  expect_identical(
    by_block,
    charToRaw(iconv(rawToChar(body), from = "latin1", to = "UTF-8"))
  )
})

test_that("the pieces a block is converted in cover it exactly, cut on newlines", {
  # The pieces exist so that iconv() is handed short strings, which it converts
  # about three times faster than one long one. They must therefore be a fact
  # about speed alone: they cover the block once, in order, and every cut but
  # the last falls on a newline byte -- which no ASCII-compatible encoding puts
  # inside a character, so the conversions rejoin to the conversion of the whole.
  block <- charToRaw(paste0(
    paste(vapply(1:5000, function(i) paste0("row", i, ",value"), character(1)),
      collapse = "\n"
    ),
    "\n"
  ))

  spans <- dta_transcode_spans(block)

  expect_gt(length(spans$starts), 1L)
  expect_length(spans$ends, length(spans$starts))
  expect_identical(spans$starts[[1]], 1L)
  expect_identical(spans$ends[[length(spans$ends)]], length(block))
  # Contiguous and non-overlapping.
  expect_identical(
    spans$starts[-1],
    spans$ends[-length(spans$ends)] + 1L
  )
  # Every cut but the last is a newline.
  expect_true(all(block[head(spans$ends, -1L)] == as.raw(0x0a)))

  # A block with no newline at all is one span rather than a cut line.
  one_line <- charToRaw(strrep("x", 200000))
  expect_identical(dta_transcode_spans(one_line), list(starts = 1L, ends = 200000L))
})

test_that("a last line with no terminator gets none added", {
  path <- transcode_fixture(
    "dta_transcode_unterminated.csv",
    c(charToRaw("NAME,V\n"), as.raw(c(0x4a, 0xfc)), charToRaw(",1"))
  )
  on.exit(unlink(path), add = TRUE)

  copy <- dta_transcode_to_utf8(path, "latin1")
  bytes <- readBin(copy, "raw", n = file.size(copy))

  expect_identical(bytes[[length(bytes)]], charToRaw("1"))
  expect_identical(
    bytes,
    c(charToRaw("NAME,V\n"), as.raw(c(0x4a, 0xc3, 0xbc)), charToRaw(",1"))
  )
})

test_that("a gzip-compressed source is converted without being expanded first", {
  path <- transcode_fixture("dta_transcode_gz.csv.gz", latin1_body(), gz = TRUE)
  on.exit(unlink(path), add = TRUE)

  copy <- dta_transcode_to_utf8(path, "latin1")

  expect_identical(readLines(copy, encoding = "UTF-8"), c("NAME,V", "Jürgen,1", "Möller,2"))
  # The copy is plain text: it is what a scanner reads, not a second archive.
  expect_gt(file.size(copy), 0)
  expect_identical(readBin(copy, "raw", n = 2L), as.raw(c(0x4e, 0x41)))
})

test_that("the copy is cached on the source's identity and its encoding", {
  path <- transcode_fixture("dta_transcode_cache.csv", latin1_body())
  on.exit(unlink(path), add = TRUE)

  first <- dta_transcode_to_utf8(path, "latin1")
  expect_identical(dta_transcode_to_utf8(path, "latin1"), first)

  # A different declared encoding is a different conversion of the same bytes.
  # It is also a re-conversion of the same delivery, so it supersedes the copy
  # above rather than joining it -- see the eviction test below.
  expect_false(identical(dta_transcode_to_utf8(path, "CP1251"), first))

  # tempdir() is the session's, but nothing stops something else clearing it:
  # a cache entry whose copy is gone must reconvert, not hand back a path that
  # is not there. A fresh source, so that what is being tested is the vanished
  # copy and not the eviction the line above performed.
  cleared <- transcode_fixture("dta_transcode_cache_cleared.csv", latin1_body())
  on.exit(unlink(cleared), add = TRUE)

  before <- dta_transcode_to_utf8(cleared, "latin1")
  unlink(before)
  again <- dta_transcode_to_utf8(cleared, "latin1")
  expect_false(identical(again, before))
  expect_true(file.exists(again))
})

test_that("a wide encoding is refused before anything is read", {
  path <- transcode_fixture("dta_transcode_wide.csv", latin1_body())
  on.exit(unlink(path), add = TRUE)

  expect_error(
    dta_transcode_to_utf8(path, "UTF-16LE"),
    "cannot be converted block by block"
  )
  expect_error(dta_transcode_to_utf8(path, "UTF-16LE"), "stream = \"never\"", fixed = TRUE)
})

test_that("an encoding name this platform cannot use is refused by name", {
  path <- transcode_fixture("dta_transcode_badname.csv", latin1_body())
  on.exit(unlink(path), add = TRUE)

  # `latin1` and `cp1252` are real names; the hyphenated spellings are not, and
  # used to surface as iconv()'s own error -- in the system language, naming
  # neither the file nor the encoding that was declared for it. Matched on the
  # condition class rather than on a base-R message for the same reason.
  for (name in c("latin-1", "cp-1252", "not an encoding")) {
    expect_error(
      dta_transcode_to_utf8(path, name),
      class = "rlang_error",
      info = name
    )
    expect_error(dta_transcode_to_utf8(path, name), name, fixed = TRUE, info = name)
    expect_error(dta_transcode_to_utf8(path, name), "iconvlist", info = name)
  }

  # The refusal happens before the file is opened, so it does not depend on the
  # delivery being there -- and it costs no I/O to establish.
  expect_error(
    dta_check_encoding_supported("latin-1", "C:/nowhere/absent.csv"),
    "not an encoding this platform can convert from"
  )
  expect_true(dta_check_encoding_supported("latin1", path))
  expect_true(dta_check_encoding_supported("UTF-8", path))
})

test_that("a half-written copy does not survive the failure that produced it", {
  path <- transcode_fixture(
    "dta_transcode_bad.csv",
    c(charToRaw("NAME,V\n"), as.raw(c(0x41, 0x81, 0xff, 0xfe)), charToRaw(",1\n"))
  )
  on.exit(unlink(path), add = TRUE)

  # Windows' iconv is lenient about several single-byte code pages, so the
  # assertion runs only where the conversion really does fail.
  probe <- iconv(rawToChar(as.raw(c(0x41, 0x81, 0xff, 0xfe))), from = "SHIFT-JIS", to = "UTF-8")
  skip_if_not(is.na(probe), "this platform's iconv accepts the bytes this test needs rejected")

  before <- list.files(tempdir(), pattern = "^file.*\\.csv$")
  expect_error(dta_transcode_to_utf8(path, "SHIFT-JIS"), "cannot be decoded as")
  # The offset is named, because "somewhere in a 60 GB file" is not actionable.
  # A byte offset rather than a line number: the file is cut into blocks, not
  # lines, and the second line of this fixture starts at byte 8.
  expect_error(dta_transcode_to_utf8(path, "SHIFT-JIS"), "offset 8")

  # No copy left behind, and nothing cached that a later call could return.
  expect_identical(list.files(tempdir(), pattern = "^file.*\\.csv$"), before)
})

test_that("a NUL byte is named rather than left to rawToChar()", {
  # rawToChar() cannot hold a NUL, and its own error is a base-R message about
  # a "raw vector" in the system language -- which says nothing about the
  # delivery. A NUL is also what a UTF-16 file declared as latin1 looks like.
  path <- transcode_fixture(
    "dta_transcode_nul.csv",
    c(charToRaw("NAME,V\n"), charToRaw("A"), as.raw(0x00), charToRaw(",1\n"))
  )
  on.exit(unlink(path), add = TRUE)

  expect_error(dta_transcode_to_utf8(path, "latin1"), class = "rlang_error")
  expect_error(dta_transcode_to_utf8(path, "latin1"), "0x00")
  expect_error(dta_transcode_to_utf8(path, "latin1"), "byte 9")
})

test_that("a re-delivery replaces its copy instead of adding to it", {
  path <- transcode_fixture("dta_transcode_evict.csv", latin1_body())
  on.exit(unlink(path), add = TRUE)

  first <- dta_transcode_to_utf8(path, "latin1")
  expect_true(file.exists(first))

  # A new size and modification time is a new cache key, so the entry for the
  # old one is unreachable -- and, until it was evicted, still a full-size copy
  # of the file under tempdir() for the rest of the session. A 60 GB delivery
  # received ten times left ten of them.
  Sys.sleep(0.01)
  con <- file(path, "ab")
  writeBin(charToRaw("Zoe,3\n"), con)
  close(con)

  second <- dta_transcode_to_utf8(path, "latin1")

  expect_false(identical(second, first))
  expect_false(file.exists(first))
  expect_true(file.exists(second))

  # And the entry went with the file: nothing in the cache still points at it.
  entries <- mget(
    ls(envir = `__DTAtools_transcode_cache__`, all.names = TRUE),
    envir = `__DTAtools_transcode_cache__`
  )
  copies <- vapply(entries, function(e) e$copy, character(1))
  expect_false(first %in% copies)
  expect_true(second %in% copies)

  # A copy of a DIFFERENT delivery is not touched by any of this.
  other <- transcode_fixture("dta_transcode_evict_other.csv", latin1_body())
  on.exit(unlink(other), add = TRUE)
  kept <- dta_transcode_to_utf8(other, "latin1")
  dta_transcode_to_utf8(path, "CP1251")
  expect_true(file.exists(kept))
})

test_that("a dataset can be told which file it should be identified by", {
  path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
  dataset <- open_file(DTAFileCSV("clinical_data.csv"), path)

  # Unstamped, a dataset answers with the files Arrow opened.
  expect_identical(dta_dataset_source_files(dataset), dataset$files)

  stamped <- dta_stamp_dataset_source(dataset, "C:/deliveries/original.csv")
  expect_identical(dta_dataset_source_files(stamped), "C:/deliveries/original.csv")
  # An arrow object is an environment, so the stamp is shared by every
  # reference to it rather than copied onto one of them.
  expect_identical(dta_dataset_source_files(dataset), "C:/deliveries/original.csv")

  # Nothing that is not a dataset, and no empty stamp, is accepted.
  expect_identical(dta_stamp_dataset_source("not a dataset", "x"), "not a dataset")
  fresh <- open_file(DTAFileCSV("clinical_data.csv"), path)
  expect_identical(dta_dataset_source_files(dta_stamp_dataset_source(fresh, "")), fresh$files)
})

test_that("the reader plan's encoding argument overrides the handler's", {
  path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
  handler <- DTAFileCSV("clinical_data.csv", encoding = "latin1")

  expect_identical(dta_delim_reader_plan(path, handler = handler)$encoding, "latin1")
  # What the lazy opener passes once it has converted the file: the handler
  # still declares latin1, and honouring that would decode the copy twice.
  expect_identical(
    dta_delim_reader_plan(path, handler = handler, encoding = "UTF-8")$encoding,
    "UTF-8"
  )
  # The override touches only the encoding: everything else the plan decides is
  # still the handler's. `parse_options` is excluded because it is a fresh
  # arrow object on every call and so is never `identical()` to another one --
  # what it was built from is covered by the settings tests above.
  comparable <- function(plan) plan[setdiff(names(plan), c("parse_options", "encoding"))]
  expect_identical(
    comparable(dta_delim_reader_plan(path, handler = handler, encoding = "UTF-8")),
    comparable(dta_delim_reader_plan(path, handler = handler))
  )
})


# ---- pattern_description reaches the concrete subclasses ---------------------
# DTAFile has carried a `pattern_description` property from the start, and the
# app serialises it into `files:`, but none of the concrete constructors took
# one: DTAFileFactory(type = "csv", pattern_description = ...) failed on an
# unused argument. A specification that described its own pattern in words could
# therefore be written and never read back.

test_that("a csv handler keeps the pattern description it was given", {
  f <- DTAFileCSV(
    filename = "clinical_data.*[.]csv$",
    pattern = TRUE,
    number_of_files = 2,
    pattern_description = "one file per site"
  )

  expect_equal(f@pattern_description, "one file per site")
})

test_that("every tabular file type accepts a pattern description", {
  for (ctor in list(DTAFileCSV, DTAFileTSV, DTAFileDelim, DTAFileTabular)) {
    f <- ctor(
      filename = "a.*[.]txt$", pattern = TRUE, number_of_files = 1,
      pattern_description = "described"
    )
    expect_equal(f@pattern_description, "described")
  }
})

test_that("a pattern description survives the factory and a YAML round trip", {
  f <- DTAFileFactory(
    type = "tsv", filename = "gf_.*[.]tsv$", pattern = TRUE,
    min_number_of_files = 1, max_number_of_files = 4,
    pattern_description = "one file per batch"
  )
  expect_equal(f@pattern_description, "one file per batch")

  ds <- dta_dataset_from_list(list(
    name = "described",
    type = "tabular",
    files = list(
      type = "tsv", filename = "gf_.*[.]tsv$", pattern = TRUE,
      min_number_of_files = 1, max_number_of_files = 4,
      pattern_description = "one file per batch"
    ),
    columns = list(list(id = "STUDYID", type = "SAS Char"))
  ))


  # ---------------------------------------------------------------------------
  # DTAFileAny class
  # ---------------------------------------------------------------------------

  test_that("DTAFileAny constructs and carries its properties", {
    h <- DTAFileAny(filename = "study_report.pdf")

    expect_s3_class(h, "DTAtools::DTAFile")
    expect_s3_class(h, "DTAtools::DTAFileAny")
    # DTAFileAny is NOT a DTAFileTabular
    expect_false(inherits(h, "DTAtools::DTAFileTabular"))
    expect_equal(h@filename, "study_report.pdf")
    expect_null(h@extensions)
  })

  test_that("DTAFileAny normalises extensions on construction", {
    # Leading dot stripped, lower-cased, de-duplicated
    h <- DTAFileAny(filename = "report.pdf", extensions = c(".PDF", "pdf"))
    expect_equal(h@extensions, "pdf")
  })

  test_that("DTAFileAny normalises upper-case extension", {
    h <- DTAFileAny(filename = "report.pdf", extensions = "PDF")
    expect_equal(h@extensions, "pdf")
  })

  test_that("DTAFileAny flattens a YAML-style list of extensions", {
    # YAML sequences parse to lists; the normaliser must unlist them
    h <- DTAFileAny(filename = "x", extensions = list("PDF", "zip"))
    expect_equal(h@extensions, c("pdf", "zip"))
  })

  test_that("DTAFileAny collapses all-blank extensions to NULL", {
    h1 <- DTAFileAny(filename = "x", extensions = c("", " "))
    expect_null(h1@extensions)

    h2 <- DTAFileAny(filename = "x", extensions = NULL)
    expect_null(h2@extensions)

    h3 <- DTAFileAny(filename = "x", extensions = character(0))
    expect_null(h3@extensions)
  })

  test_that("matches_filename(DTAFileAny) accepts any ending when extensions is NULL", {
    # Use a pattern so various endings can match the same base pattern
    h <- DTAFileAny(filename = "^report\\..*", pattern = TRUE)

    expect_true(any(matches_filename(h, "report.pdf")))
    expect_true(any(matches_filename(h, "report.csv")))
    expect_true(any(matches_filename(h, "report.xpt")))
  })

  test_that("matches_filename(DTAFileAny) accepts only listed extensions", {
    h <- DTAFileAny(filename = "report.pdf", extensions = c("pdf", "zip"))

    expect_true(matches_filename(h, "report.pdf"))
    expect_false(matches_filename(h, "report.csv"))
    expect_false(matches_filename(h, "report.xpt"))
  })

  test_that("matches_filename(DTAFileAny) satisfies extension via compressed basename", {
    # report.pdf.gz carries the pdf ending underneath the .gz wrapper
    h <- DTAFileAny(filename = "report.pdf", extensions = "pdf")

    expect_true(matches_filename(h, "report.pdf.gz"))
    # but .csv.gz does NOT satisfy extensions = "pdf"
    expect_false(matches_filename(h, "report.csv.gz"))
  })

  test_that("matches_filename(DTAFileAny) returns a vector with one element per name/pattern", {
    # A handler carrying TWO patterns must return length 2, not a scalar TRUE/FALSE.
    # This is load-bearing: the Shiny app reduces with any(), not isTRUE().
    h <- DTAFileAny(
      filename = c("report_a.pdf", "report_b.pdf"),
      pattern = TRUE
    )
    result <- matches_filename(h, "report_a.pdf")

    expect_length(result, 2)
    expect_true(result[[1]])
    expect_false(result[[2]])
  })

  test_that("DTAFileFactory(type='any') returns a DTAFileAny", {
    h <- DTAFileFactory(type = "any", filename = "audit.log")

    expect_s3_class(h, "DTAtools::DTAFileAny")
    expect_equal(h@filename, "audit.log")
  })

  test_that("DTAFileFactory rejects an unsupported type", {
    expect_error(DTAFileFactory(type = "xls", filename = "x.xls"), "supported")
  })

  test_that("DTAFileAny is NOT a DTAFileTabular", {
    h <- DTAFileAny(filename = "raw.zip")
    expect_false(inherits(h, "DTAtools::DTAFileTabular"))
  })

  expect_equal(ds@files[[1]]@pattern_description, "one file per batch")
})

test_that("a handler without a pattern description still has none", {
  f <- DTAFileCSV(filename = "clinical_data.csv")

  expect_null(f@pattern_description)
})


# ---- Several file names on one handler --------------------------------------
# `filename` is documented as a character VECTOR and matches_filename() has a
# `%in%` branch for exactly that case, but the validator tested
# `self@filename == ""` -- a length-1 test. Two names made the `if` condition
# length 2, which R rejects outright, so the documented case could never be
# built. A YAML `filename:` sequence hit it from the other side: it parses to a
# list, which the character property refused.

test_that("a handler can carry several file names", {
  f <- DTAFileCSV(
    filename = c("site_a.csv", "site_b.csv"),
    pattern = TRUE, number_of_files = 2
  )

  expect_equal(f@filename, c("site_a.csv", "site_b.csv"))
})

test_that("a filename sequence from YAML becomes a character vector", {
  ds <- dta_dataset_from_list(list(
    name = "multi_name",
    type = "tabular",
    files = list(
      type = "csv", filename = list("site_a.csv", "site_b.csv"),
      pattern = TRUE, number_of_files = 2
    ),
    columns = list(list(id = "STUDYID", type = "SAS Char"))
  ))

  expect_equal(ds@files[[1]]@filename, c("site_a.csv", "site_b.csv"))
})

test_that("matches_filename accepts any of a handler's names and nothing else", {
  f <- DTAFileCSV(
    filename = c("site_a.csv", "site_b.csv"),
    pattern = TRUE, number_of_files = 2
  )

  # A non-pattern check over several names is the `%in%` branch; with pattern
  # TRUE each name is a regex, so both still match themselves and a stranger
  # matches neither.
  expect_true(any(matches_filename(f, "site_a.csv")))
  expect_true(any(matches_filename(f, "site_b.csv")))
  expect_false(any(matches_filename(f, "site_c.csv")))
})

test_that("an empty name is still rejected, in any position", {
  expect_error(
    DTAFileCSV(filename = ""),
    "must be a non-empty character vector"
  )
  expect_error(
    DTAFileCSV(filename = c("a.csv", ""), pattern = TRUE, number_of_files = 2),
    "must be a non-empty character vector"
  )
  expect_error(
    DTAFileCSV(filename = character(0)),
    "must be a non-empty character vector"
  )
})


# ---- A non-pattern handler expects exactly one file -------------------------
# The guard used to test `number_of_files != 1` only. A handler declaring its
# count as a min/max pair was never checked, and with only a min/max set
# `number_of_files` is NULL, so the comparison ran on a zero-length value: the
# object either failed with a message about the wrong thing or -- worse -- was
# built inconsistent and crashed later, in print_info(), where min and max are
# compared directly.

test_that("a non-pattern handler rejects a count other than 1, however it is declared", {
  expect_error(
    DTAFile("file.txt", pattern = FALSE, number_of_files = 2),
    "number_of_files must be 1"
  )
  expect_error(
    DTAFile("file.txt", pattern = FALSE, min_number_of_files = 2),
    "number_of_files must be 1"
  )
  expect_error(
    DTAFile("file.txt", pattern = FALSE, max_number_of_files = 3),
    "number_of_files must be 1"
  )
  expect_error(
    DTAFile("file.txt", pattern = FALSE, min_number_of_files = 1, max_number_of_files = 2),
    "number_of_files must be 1"
  )
})

test_that("a non-pattern handler accepts the counts that do mean one file", {
  # Whatever is accepted must be complete enough to print: min and max are
  # compared to each other there, so a half-built object surfaces as a crash.
  implicit <- DTAFile("file.txt", pattern = FALSE)
  expect_equal(min_number_of_files(implicit), 1)
  expect_equal(max_number_of_files(implicit), 1)
  # print_info() reports through cli, i.e. on the message stream. It compares
  # min to max directly, so a half-built object surfaces here as an error.
  expect_match(
    paste(capture_messages(print_info(implicit)), collapse = ""),
    "Number of files"
  )

  explicit <- DTAFile("file.txt", pattern = FALSE, number_of_files = 1)
  expect_equal(min_number_of_files(explicit), 1)
  expect_equal(max_number_of_files(explicit), 1)

  as_range <- DTAFile(
    "file.txt",
    pattern = FALSE, min_number_of_files = 1, max_number_of_files = 1
  )
  expect_equal(min_number_of_files(as_range), 1)
  expect_equal(max_number_of_files(as_range), 1)
  expect_match(
    paste(capture_messages(print_info(as_range)), collapse = ""),
    "Number of files"
  )
})

test_that("a pattern handler may still declare a range", {
  ranged <- DTAFile(
    "data.*[.]csv$",
    pattern = TRUE, min_number_of_files = 1, max_number_of_files = 4
  )

  expect_equal(min_number_of_files(ranged), 1)
  expect_equal(max_number_of_files(ranged), 4)
  # A genuine range reports both bounds, not the single-count line.
  expect_match(
    paste(capture_messages(print_info(ranged)), collapse = ""),
    "Min number of files"
  )
})


# ---------------------------------------------------------------------------
# read_file()'s namecheck reduces a multi-pattern match with any() -- section
# P11 of the code review fixes
# ---------------------------------------------------------------------------
# matches_filename() returns one logical PER declared name or pattern.
# read_file()'s namecheck used to feed that vector straight into `if
# (!matches_filename(...))`, which dies with R's own "the condition has
# length > 1" the moment a handler declares more than one name/pattern and the
# file matches only one of them.

test_that("read_file() reads a file matching only the second of two patterns", {
  path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
  file_info <- DTAFileCSV(
    filename = c("^nonexistent_pattern$", "^clinical_data[.]csv$"),
    pattern = TRUE,
    number_of_files = 2
  )

  x <- read_file(file_info, path)

  expect_true(all(c("Table", "ArrowTabular") %in% class(x)))
  expect_equal(ncol(x), 14)
})

test_that("read_file() still aborts with 'does not match' when neither pattern matches", {
  path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
  file_info <- DTAFileCSV(
    filename = c("^nope_a$", "^nope_b$"),
    pattern = TRUE,
    number_of_files = 2
  )

  expect_error(read_file(file_info, path), "does not match")
})


# ---------------------------------------------------------------------------
# A brace in a file name must not be read as cli markup
# ---------------------------------------------------------------------------
# Both guards used to render the path with str_glue() and hand the RESULT to
# cli, which then read any brace the path contained as an expression of its
# own. `data{1}.csv` -- an ordinary name -- aborted with cli's own parse error
# instead of the intended message, so the user was told nothing about their
# file. Interpolating the variable lets cli escape the braces itself.

test_that("a file name containing braces still reports the real problem", {
  braced <- file.path(tempdir(), "dta_brace{1}.csv")
  on.exit(unlink(braced), add = TRUE)
  writeLines(c("A", "1"), braced)

  expect_error(
    read_file(DTAFileCSV("something_else.csv"), braced),
    regexp = "does not match"
  )
  expect_error(
    open_file(DTAFileCSV("something_else.csv"), braced),
    regexp = "does not match"
  )

  absent <- file.path(tempdir(), "dta_absent{2}.csv")
  expect_false(file.exists(absent))
  expect_error(
    read_file(DTAFileCSV("dta_absent{2}.csv"), absent),
    regexp = "cannot be found"
  )
  expect_error(
    open_file(DTAFileCSV("dta_absent{2}.csv"), absent),
    regexp = "cannot be found"
  )
})

test_that("a file name containing cli markup is not rendered as markup", {
  # `{.field x}` is a style, not a variable, so it would not have aborted --
  # it would have printed the name in colour with the markup silently removed,
  # naming a file the user does not have. cli wraps at the console width, so
  # widen it: a line break landing inside the name would fail this for the
  # wrong reason.
  withr::local_options(cli.width = 1000)

  styled <- file.path(tempdir(), "dta_{.field x}.csv")
  expect_false(file.exists(styled))

  expect_error(
    read_file(DTAFileCSV("dta_{.field x}.csv"), styled),
    regexp = "{.field x}",
    fixed = TRUE
  )
})
