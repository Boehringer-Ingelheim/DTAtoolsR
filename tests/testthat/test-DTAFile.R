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

test_that("read_file leaves a column the specs do not mention to inference", {
  path <- dta_write_id_fixture("dta_ids_extra.csv", ",")
  on.exit(unlink(path), add = TRUE)

  x <- read_file(DTAFileCSV(basename(path)), path, specs = dta_id_specs())
  df <- as.data.frame(x)

  # EXTRA has no spec, so it is neither pinned nor dropped: arrow types it
  # exactly as it does without any specs at all.
  expect_true("EXTRA" %in% names(df))
  expect_equal(as.numeric(df$EXTRA), c(10, 20))
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

test_that("specs are ignored when the file has no header", {
  path <- dta_write_id_fixture("dta_ids_noheader.csv", ",")
  on.exit(unlink(path), add = TRUE)

  # Arrow generates positional names (f0, f1, ...) that cannot correspond to
  # spec ids, so no column spec is built and the header line becomes data.
  x <- read_file(
    DTAFileCSV(basename(path), has_header = FALSE),
    path,
    specs = dta_id_specs()
  )

  expect_equal(nrow(x), 3)
  expect_equal(ncol(x), 3)
  expect_false("SUBJID" %in% names(x))
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
