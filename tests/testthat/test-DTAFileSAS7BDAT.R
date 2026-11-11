# Reading a SAS dataset (.sas7bdat) as input data.
#
# Three fixtures under inst/qualification/fx/sas/ carry the evidence:
#
# * iris.sas7bdat, written by SAS 9.04.01M2 (the copy haven ships, MIT): the four
#   numeric columns are bit-identical to datasets::iris and Species is stored
#   $6, so "versicolor" is "versic" and "virginica" is "virgin".
# * delivery.sas7bdat and empty.sas7bdat, written by haven/ReadStat (no
#   SAS-written file with these shapes is at hand); see
#   .github/scripts/build_sas7bdat_fixtures.R.
#
# Every expected value is written out from SAS semantics -- the epoch is
# 1960-01-01, so SAS day 23968 is 3653 + 20315, i.e. 2025-08-15 -- never taken
# from the reader.

sas_fx <- function(name) {
  path <- system.file("qualification", "fx", "sas", name, package = "DTAtools")
  # A guaranteed package asset: missing is a failure, not a skip.
  expect_true(nzchar(path), info = paste(name, "missing from the qualification fixtures"))
  path
}

sas_read_df <- function(name, specs = NULL) {
  handler <- DTAFileSAS7BDAT(filename = name)
  as.data.frame(read_file(handler, sas_fx(name), specs = specs))
}

# One column spec per column of delivery.sas7bdat. `temporal = "text"` declares
# the three temporal columns as SAS dates/datetimes/times under their display
# format; "num" declares them as plain numbers.
delivery_specs <- function(temporal = c("text", "num")) {
  temporal <- match.arg(temporal)
  cols <- list(
    SUBJID = DTAColumnSpec(id = "SUBJID", type = "SAS Char", nullable = TRUE),
    AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
  )
  cols <- c(cols, if (temporal == "text") {
    list(
      VISDT = DTAColumnSpec(id = "VISDT", type = "SAS Date", format = "SAS DATE9.", nullable = TRUE),
      VISDTM = DTAColumnSpec(id = "VISDTM", type = "SAS DateTime", format = "SAS DATETIME20.", nullable = TRUE),
      VISTM = DTAColumnSpec(id = "VISTM", type = "SAS Time", format = "SAS TIME8.", nullable = TRUE)
    )
  } else {
    list(
      VISDT = DTAColumnSpec(id = "VISDT", type = "SAS Num", nullable = TRUE),
      VISDTM = DTAColumnSpec(id = "VISDTM", type = "SAS Num", nullable = TRUE),
      VISTM = DTAColumnSpec(id = "VISTM", type = "SAS Num", nullable = TRUE)
    )
  })
  DTAColumnSpecCollection(columns = cols)
}

iris_specs <- function(sepal_length_type = "SAS Num") {
  DTAColumnSpecCollection(columns = list(
    Sepal_Length = DTAColumnSpec(id = "Sepal_Length", type = sepal_length_type, nullable = FALSE),
    Sepal_Width = DTAColumnSpec(id = "Sepal_Width", type = "SAS Num", nullable = FALSE),
    Petal_Length = DTAColumnSpec(id = "Petal_Length", type = "SAS Num", nullable = FALSE),
    Petal_Width = DTAColumnSpec(id = "Petal_Width", type = "SAS Num", nullable = FALSE),
    Species = DTAColumnSpec(id = "Species", type = "SAS Char", nullable = FALSE)
  ))
}

sas_dataset <- function(file, specs, stream = "never") {
  ds <- DTADataSetTabular(
    id = "sas",
    specs = specs,
    files = list(DTAFileSAS7BDAT(filename = file))
  )
  load_file(ds, file = sas_fx(file), handler_index = 1, stream = stream)
}

# The three axis counts after a check().
sas_counts <- function(ds) {
  status <- validation_status(check(ds, quiet = TRUE, persist = FALSE))
  c(
    columnspec = status$n_columnspec_errors,
    rule = status$n_rule_errors,
    import = status$n_import_errors
  )
}

# A condition's message with its line wrapping flattened, for matching
# package-authored text only.
flat_message <- function(cnd) gsub("\\s+", " ", conditionMessage(cnd))

# ---- the handler -------------------------------------------------------------

test_that("the factory and a YAML list build a SAS handler that is not a DTAFileTabular", {
  from_factory <- DTAFileFactory(type = "sas7bdat", filename = "ae.sas7bdat")
  from_yaml <- dta_file_handlers_from_list(
    list(type = "sas7bdat", filename = "ae.sas7bdat")
  )[[1]]

  for (h in list(from_factory, from_yaml)) {
    expect_true(inherits(h, "DTAtools::DTAFileSAS7BDAT"))
    expect_true(inherits(h, "DTAtools::DTAFile"))
    expect_false(inherits(h, "DTAtools::DTAFileTabular"))
    expect_identical(h@filename, "ae.sas7bdat")
  }
})

test_that("a tabular dataset accepts a SAS handler and still refuses a reader-less one", {
  specs <- create_example_DTAColumnSpecCollection(1)

  expect_no_error(
    DTADataSetTabular(
      id = "ae",
      specs = specs,
      files = list(DTAFileSAS7BDAT(filename = "ae.sas7bdat"))
    )
  )

  bogus <- tryCatch(
    DTADataSetTabular(
      id = "ae",
      specs = specs,
      files = list(DTAFileAny(filename = "ae.pdf"))
    ),
    error = function(e) e
  )
  expect_s3_class(bogus, "error")
  expect_match(flat_message(bogus), "not tabular", fixed = TRUE)
  # The hint now names the SAS handler too.
  expect_match(flat_message(bogus), "SAS7BDAT", fixed = TRUE)
})

test_that("a SAS handler matches the compressed name of the file it declares", {
  handler <- DTAFileSAS7BDAT(filename = "ae.sas7bdat")
  expect_true(matches_filename(handler, "ae.sas7bdat"))
  expect_true(matches_filename(handler, "ae.sas7bdat.gz"))
  expect_false(matches_filename(handler, "dm.sas7bdat"))
})

test_that("the SAS handler type is listed by the factory error", {
  err <- tryCatch(DTAFileFactory(type = "zip", filename = "a.zip"), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(flat_message(err), "sas7bdat", fixed = TRUE)
})

test_that("a sas7bdat handler inside a file dataset is coerced to a reader-less one", {
  ds <- DTADataSetFile(
    id = "raw",
    files = list(DTAFileSAS7BDAT(filename = "ae.sas7bdat"))
  )
  handler <- files(ds, 1)
  expect_true(inherits(handler, "DTAtools::DTAFileAny"))
  expect_false(inherits(handler, "DTAtools::DTAFileSAS7BDAT"))
  expect_identical(handler@filename, "ae.sas7bdat")
})

test_that("the template guide names a SAS dataset as the exported document does", {
  expect_identical(
    .tgc_file_type(DTAFileSAS7BDAT(filename = "ae.sas7bdat")),
    .file_format_label(DTAFileSAS7BDAT(filename = "ae.sas7bdat"))
  )
  expect_identical(.tgc_file_type(DTAFileCSV(filename = "a.csv")), "CSV")
})

test_that(".file_format_label names a SAS dataset", {
  expect_identical(
    .file_format_label(DTAFileSAS7BDAT(filename = "ae.sas7bdat")),
    "SAS dataset (.sas7bdat)"
  )
})

# ---- reading: the SAS-written iris file --------------------------------------

test_that("iris.sas7bdat reads back as the SAS-written values", {
  table <- read_file(DTAFileSAS7BDAT(filename = "iris.sas7bdat"), sas_fx("iris.sas7bdat"))
  expect_s3_class(table, "Table")

  df <- as.data.frame(table)
  expect_identical(
    names(df),
    c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width", "Species")
  )
  expect_identical(nrow(df), 150L)

  # The four numeric columns are bit-identical to datasets::iris.
  expect_identical(df$Sepal_Length, datasets::iris$Sepal.Length)
  expect_identical(df$Sepal_Width, datasets::iris$Sepal.Width)
  expect_identical(df$Petal_Length, datasets::iris$Petal.Length)
  expect_identical(df$Petal_Width, datasets::iris$Petal.Width)

  # Species is stored $6: the SAS value is cut at six characters.
  expect_identical(df$Species, substr(as.character(datasets::iris$Species), 1, 6))
  expect_identical(unique(df$Species), c("setosa", "versic", "virgin"))
})

test_that("iris.sas7bdat validates clean against its correct specification", {
  counts <- sas_counts(sas_dataset("iris.sas7bdat", iris_specs()))
  expect_identical(unname(counts), c(0L, 0L, 0L))
})

test_that("a SAS number declared as text is a column-specification error, not an import error", {
  ds <- sas_dataset("iris.sas7bdat", iris_specs(sepal_length_type = "SAS Char"))
  counts <- sas_counts(ds)
  # One violation per row of the column; reading numbers is not an import problem.
  expect_identical(unname(counts[["columnspec"]]), 150L)
  expect_identical(unname(counts[["import"]]), 0L)
})

# ---- reading: values as stored -----------------------------------------------

test_that("character values are kept as stored and a blank is missing", {
  df <- sas_read_df("delivery.sas7bdat")

  # "007" keeps its zeros, "  lead" its leading blanks, "trail   " loses the
  # blanks SAS pads with, "" is missing, "NA" is a value (unlike in a CSV file).
  expect_identical(df$SUBJID, c("007", "  lead", "trail", NA, "NA", "M\u00fcller"))
  expect_identical(df$AGE, c(30, 41.5, NA, 0, -2, 1e10))
})

test_that("without a specification a date, datetime and time are ISO 8601 text", {
  df <- sas_read_df("delivery.sas7bdat")

  expect_identical(
    df$VISDT,
    c("1960-01-01", "2025-08-15", NA, "1959-12-31", "2000-02-29", "1970-01-01")
  )
  expect_identical(
    df$VISDTM,
    c(
      "1960-01-01T00:00:00", "2025-08-15T14:30:00", NA,
      "1959-12-31T23:59:59", "2000-02-29T12:00:00", "1970-01-01T00:00:00"
    )
  )
  expect_identical(
    df$VISTM,
    c("00:00:00", "14:30:00", NA, "23:59:59", "12:00:00", "01:00:00")
  )
})

test_that("declared with their SAS formats, temporal columns are the format's display text", {
  ds <- sas_dataset("delivery.sas7bdat", delivery_specs("text"))
  df <- as.data.frame(tables(ds)[["delivery"]])

  expect_identical(
    df$VISDT,
    c("01JAN1960", "15AUG2025", NA, "31DEC1959", "29FEB2000", "01JAN1970")
  )
  expect_identical(
    df$VISDTM,
    c(
      "01JAN1960:00:00:00", "15AUG2025:14:30:00", NA,
      "31DEC1959:23:59:59", "29FEB2000:12:00:00", "01JAN1970:00:00:00"
    )
  )
  expect_identical(
    df$VISTM,
    c("0:00:00", "14:30:00", NA, "23:59:59", "12:00:00", "1:00:00")
  )

  expect_identical(unname(sas_counts(ds)), c(0L, 0L, 0L))
})

test_that("declared Num, temporal columns stay the SAS number counted from 1960-01-01", {
  ds <- sas_dataset("delivery.sas7bdat", delivery_specs("num"))
  df <- as.data.frame(tables(ds)[["delivery"]])

  expect_identical(df$VISDT, c(0, 23968, NA, -1, 14669, 3653))
  expect_identical(df$VISDTM, c(0, 2070887400, NA, -1, 1267444800, 315619200))
  expect_identical(df$VISTM, c(0, 52200, NA, 86399, 43200, 3600))

  expect_identical(unname(sas_counts(ds)), c(0L, 0L, 0L))
})

test_that("a SAS date declared as text turns the numbers into column-specification errors", {
  specs <- delivery_specs("num")
  specs@columns$VISDT <- DTAColumnSpec(id = "VISDT", type = "SAS Char", nullable = TRUE)

  counts <- sas_counts(sas_dataset("delivery.sas7bdat", specs))
  # VISDT holds five dates and one missing value (see the fixture builder); the
  # other four columns are declared to match, so only those five are reported.
  expect_identical(unname(counts[["columnspec"]]), 5L)
  expect_identical(unname(counts[["import"]]), 0L)
})

test_that("text declared as a number is parsed: what is not a number is an import error", {
  # SUBJID in the fixture builder: "007", "  lead", "trail", "" (missing), "NA"
  # and a surname with an umlaut. Only "007" is a number (7); "" stays missing;
  # the other four are import errors and nothing is a column-spec violation.
  specs <- delivery_specs("num")
  specs@columns$SUBJID <- DTAColumnSpec(id = "SUBJID", type = "SAS Num", nullable = TRUE)

  ds <- sas_dataset("delivery.sas7bdat", specs)
  expect_identical(
    as.data.frame(tables(ds)[["delivery"]])$SUBJID,
    c(7, NA, NA, NA, NA, NA)
  )
  expect_identical(unname(sas_counts(ds)), c(0L, 0L, 4L))
})

# ---- the normaliser, on in-memory frames -------------------------------------

test_that("the normaliser renders by declaration: text, number or ISO", {
  frame <- function() {
    data.frame(
      D = as.Date("2025-08-15"),
      DT = as.POSIXct("2025-08-15 14:30:00", tz = "UTC"),
      TM = structure(52200, class = c("hms", "difftime"), units = "secs")
    )
  }
  declared <- function(...) {
    DTAColumnSpecCollection(columns = list(...))
  }
  # A structure with a length and no type (DTAColumnSpec() cannot build one:
  # its factory needs a backend prefix from a type or a format).
  length_only <- DTAColumnSpec(id = "DT", nullable = TRUE)
  length_only@structure <- DTAColumnSpecStructureSAS(length = 8)
  expect_null(length_only@structure@type)

  # Declared as the same kind, with a format: display text.
  out <- dta_sas7bdat_normalise(
    frame(),
    declared(
      D = DTAColumnSpec(id = "D", type = "SAS Date", format = "SAS DATE9."),
      DT = DTAColumnSpec(id = "DT", type = "SAS DateTime", format = "SAS DATETIME20."),
      TM = DTAColumnSpec(id = "TM", type = "SAS Time", format = "SAS TIME8.")
    )
  )
  expect_identical(out$D, "15AUG2025")
  expect_identical(out$DT, "15AUG2025:14:30:00")
  expect_identical(out$TM, "14:30:00")

  # Declared as a non-temporal type: the SAS number.
  out <- dta_sas7bdat_normalise(
    frame(),
    declared(
      D = DTAColumnSpec(id = "D", type = "SAS Num"),
      DT = DTAColumnSpec(id = "DT", type = "SAS Int"),
      TM = DTAColumnSpec(id = "TM", type = "SAS Char")
    )
  )
  expect_identical(out$D, 23968)
  expect_identical(out$DT, 2070887400)
  expect_identical(out$TM, 52200)

  iso <- list(D = "2025-08-15", DT = "2025-08-15T14:30:00", TM = "14:30:00")

  # No specs, an undeclared column, a spec without a structure, a spec with a
  # length only (no type), and a declared type of another temporal kind.
  expect_identical(as.list(dta_sas7bdat_normalise(frame(), NULL)), iso)
  expect_identical(as.list(dta_sas7bdat_normalise(frame(), declared())), iso)
  expect_identical(
    as.list(dta_sas7bdat_normalise(
      frame(),
      declared(
        D = DTAColumnSpec(id = "D", nullable = TRUE),
        DT = length_only,
        TM = DTAColumnSpec(id = "TM", type = "SAS Date", format = "SAS DATE9.")
      )
    )),
    iso
  )
  # Declared as the same kind but with no format: ISO as well.
  expect_identical(
    dta_sas7bdat_normalise(
      frame(),
      declared(D = DTAColumnSpec(id = "D", type = "SAS Date"))
    )$D,
    "2025-08-15"
  )
})

test_that("a time held in minutes or hours is converted to seconds before it is rendered", {
  # 870 minutes = 14.5 hours = 52200 seconds = 14:30:00. as.numeric(x, units =)
  # is not unit-aware for an hms, so the normaliser converts from the units
  # attribute itself.
  time_spec <- DTAColumnSpecCollection(columns = list(
    TM = DTAColumnSpec(id = "TM", type = "SAS Time", format = "SAS TIME8.")
  ))
  num_spec <- DTAColumnSpecCollection(columns = list(
    TM = DTAColumnSpec(id = "TM", type = "SAS Num")
  ))
  held <- list(
    mins = structure(870, class = c("hms", "difftime"), units = "mins"),
    hours = structure(14.5, class = "difftime", units = "hours"),
    secs = structure(52200, class = c("hms", "difftime"), units = "secs")
  )

  for (value in held) {
    out <- dta_sas7bdat_normalise(data.frame(TM = value), time_spec)
    expect_identical(out$TM, "14:30:00")

    out <- dta_sas7bdat_normalise(data.frame(TM = value), num_spec)
    expect_identical(out$TM, 52200)
  }
})

test_that("the normaliser blanks empty text and strips every attribute but the structural ones", {
  x <- data.frame(S = c("a", "", NA), N = c(1.5, NA, 3), stringsAsFactors = FALSE)
  attr(x$S, "label") <- "Subject"
  attr(x$S, "format.sas") <- "$3."
  attr(x$N, "label") <- "Number"
  attr(x$N, "format.sas") <- "BEST12."
  attr(x$N, "display_width") <- 12L

  out <- dta_sas7bdat_normalise(x, NULL)

  expect_identical(out$S, c("a", NA, NA))
  expect_identical(out$N, c(1.5, NA, 3))
  expect_null(attributes(out$S))
  expect_null(attributes(out$N))
})

# ---- an empty dataset --------------------------------------------------------

test_that("a dataset with no observations reads as zero rows and keeps its variables", {
  df <- sas_read_df("empty.sas7bdat")
  expect_identical(nrow(df), 0L)
  expect_identical(names(df), c("SUBJID", "AGE", "VISDT", "VISDTM", "VISTM"))

  ds <- sas_dataset("empty.sas7bdat", delivery_specs("text"))
  expect_identical(unname(sas_counts(ds)), c(0L, 0L, 0L))
})

# ---- files that cannot be read -----------------------------------------------

test_that("an empty or non-SAS file is refused with a condition carrying its path", {
  tmp <- withr::local_tempdir()
  handler <- DTAFileSAS7BDAT(filename = "ae.sas7bdat")

  empty <- file.path(tmp, "ae.sas7bdat")
  file.create(empty)
  text <- file.path(tmp, "other", "ae.sas7bdat")
  dir.create(dirname(text))
  writeLines("this is not a SAS dataset", text)

  for (path in c(empty, text)) {
    expect_error(read_file(handler, path), class = "dta_sas7bdat_read_error")

    cnd <- tryCatch(
      read_file(handler, path),
      dta_sas7bdat_read_error = function(e) e
    )
    expect_s3_class(cnd, "dta_sas7bdat_read_error")
    expect_identical(cnd$path, path)
  }
})

test_that("the reader refuses duplicate names and keeps ReadStat's console output to itself", {
  seen <- NULL
  testthat::local_mocked_bindings(
    read_sas = function(data_file, ...) {
      seen <<- list(file = data_file, args = list(...))
      cat("ReadStat chatter on standard output\n")
      data.frame(A = c(1, 2), B = c("x", ""))
    },
    .package = "haven"
  )

  # read_file() insists the path exists; the mocked reader never opens it.
  path <- file.path(withr::local_tempdir(), "any.sas7bdat")
  file.create(path)
  handler <- DTAFileSAS7BDAT(filename = "any.sas7bdat")
  expect_silent(table <- read_file(handler, path))

  expect_identical(seen$file, path)
  expect_identical(seen$args[[".name_repair"]], "check_unique")
  # The mocked frame still went through the normaliser: "" is missing.
  expect_identical(as.data.frame(table)$B, c("x", NA))
})

# ---- compression and paths ---------------------------------------------------

test_that("a gzipped SAS dataset reads identically and the archive is left alone", {
  tmp <- withr::local_tempdir()
  gz <- file.path(tmp, "iris.sas7bdat.gz")
  bytes <- readBin(sas_fx("iris.sas7bdat"), "raw", n = file.size(sas_fx("iris.sas7bdat")))
  con <- gzfile(gz, "wb")
  writeBin(bytes, con)
  close(con)
  before <- unname(tools::md5sum(gz))

  handler <- DTAFileSAS7BDAT(filename = "iris.sas7bdat")
  plain <- as.data.frame(read_file(handler, sas_fx("iris.sas7bdat")))
  zipped <- as.data.frame(read_file(handler, gz))

  expect_identical(zipped, plain)
  expect_identical(unname(tools::md5sum(gz)), before)

  # The table is named after the file with the compression suffix and extension
  # removed, as for a CSV file.
  ds <- DTADataSetTabular(id = "sas", specs = iris_specs(), files = list(handler))
  ds <- load_file(ds, file = gz, handler_index = 1, stream = "never")
  expect_identical(names(tables(ds)), "iris")
})

test_that("a path with non-ASCII characters is readable", {
  tmp <- withr::local_tempdir()
  dir <- file.path(tmp, "d\u00e9j\u00e0 vu")
  dir.create(dir)
  copy <- file.path(dir, "iris.sas7bdat")
  file.copy(sas_fx("iris.sas7bdat"), copy)

  df <- as.data.frame(read_file(DTAFileSAS7BDAT(filename = "iris.sas7bdat"), copy))
  expect_identical(nrow(df), 150L)
})

# ---- memory: no lazy reader --------------------------------------------------

test_that("stream = 'auto' reads a SAS dataset into memory and says so", {
  withr::local_options(DTAtools.stream_threshold = 1)
  ds <- DTADataSetTabular(
    id = "sas",
    specs = iris_specs(),
    files = list(DTAFileSAS7BDAT(filename = "iris.sas7bdat"))
  )

  expect_message(
    load_file(ds, file = sas_fx("iris.sas7bdat"), handler_index = 1, stream = "auto"),
    class = "dta_sas7bdat_in_memory"
  )
  loaded <- suppressMessages(
    load_file(ds, file = sas_fx("iris.sas7bdat"), handler_index = 1, stream = "auto")
  )

  held <- tables(loaded)[["iris"]]
  expect_true(inherits(held, "Table"))
  expect_false(inherits(held, "Dataset"))
  expect_identical(nrow(held), 150L)
})

test_that("a SAS dataset under the size threshold is read in memory without the message", {
  ds <- DTADataSetTabular(
    id = "sas",
    specs = iris_specs(),
    files = list(DTAFileSAS7BDAT(filename = "iris.sas7bdat"))
  )

  seen <- character()
  withCallingHandlers(
    load_file(ds, file = sas_fx("iris.sas7bdat"), handler_index = 1, stream = "auto"),
    message = function(m) {
      seen <<- c(seen, class(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_false("dta_sas7bdat_in_memory" %in% seen)
})

test_that("stream = 'always' and open_file() refuse a SAS dataset and name the way out", {
  withr::local_options(DTAtools.stream_threshold = 1)
  handler <- DTAFileSAS7BDAT(filename = "iris.sas7bdat")
  ds <- DTADataSetTabular(id = "sas", specs = iris_specs(), files = list(handler))

  for (stream in list("always", TRUE)) {
    cnd <- tryCatch(
      load_file(ds, file = sas_fx("iris.sas7bdat"), handler_index = 1, stream = stream),
      error = function(e) e
    )
    expect_s3_class(cnd, "error")
    expect_match(flat_message(cnd), "Streaming is not supported", fixed = TRUE)
    expect_match(flat_message(cnd), "stream = \"never\"", fixed = TRUE)
  }

  cnd <- tryCatch(open_file(handler, sas_fx("iris.sas7bdat")), error = function(e) e)
  expect_s3_class(cnd, "error")
  expect_match(flat_message(cnd), "Streaming is not supported", fixed = TRUE)
})

test_that("validate_file_stream() and cache_as_parquet() refuse a SAS dataset and say why", {
  path <- sas_fx("iris.sas7bdat")
  specs <- iris_specs()
  cache <- file.path(withr::local_tempdir(), "cache")

  for (call in list(
    quote(validate_file_stream(specs, path, verbose = FALSE)),
    quote(cache_as_parquet(specs, path, cache_path = cache))
  )) {
    cnd <- tryCatch(eval(call), error = function(e) e)
    expect_s3_class(cnd, "dta_sas7bdat_not_delimited")
    # Package-authored wording only: it names the cause and the way out, not
    # Arrow's "Is this a 'csv' file?".
    expect_match(flat_message(cnd), "SAS dataset", fixed = TRUE)
    expect_match(flat_message(cnd), "load_file", fixed = TRUE)
    expect_false(grepl("csv", flat_message(cnd), fixed = TRUE))
  }
  expect_false(dir.exists(cache))
})

test_that("only a SAS handler gets the 'cannot be streamed' message", {
  withr::local_options(DTAtools.stream_threshold = 1)
  csv <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
  skip_if(!nzchar(csv), "bundled clinical_data.csv not found")

  ds <- DTADataSetTabular(
    id = "csv",
    specs = create_example_DTAColumnSpecCollection(1),
    files = list(DTAFileCSV(filename = "clinical_data.csv"))
  )
  seen <- character()
  withCallingHandlers(
    suppressWarnings(load_file(ds, file = csv, handler_index = 1, stream = "auto")),
    message = function(m) {
      seen <<- c(seen, class(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_false("dta_sas7bdat_in_memory" %in% seen)
})

# ---- two pinned limitations --------------------------------------------------

test_that("LIM-011: a number under a w.d format is checked as stored, not as displayed", {
  # KNOWN DEFECT, pinned rather than endorsed. SAS would display 1/3 under 8.2
  # as 0.33, which fits; the value is checked as stored, so it is a format
  # error. If rounding is ever added, switch this to expect 0 errors.
  specs <- DTAColumnSpecCollection(columns = list(
    X = DTAColumnSpec(id = "X", type = "SAS Num", format = "SAS 8.2", nullable = FALSE)
  ))
  details <- validate_table_detailed(specs, data.frame(X = 1 / 3), verbose = FALSE)

  errors <- details$columnspec_errors$full_error
  expect_identical(nrow(errors), 1L)
  expect_identical(errors$keyword, "format")
})

test_that("LIM-012: a declared length on a SAS date is checked against the rendered text", {
  # KNOWN DEFECT, pinned rather than endorsed. A spec taken from PROC CONTENTS
  # carries length 8, the bytes SAS stores a date in; the check sees the 9
  # characters of "15AUG2025" instead, so every row fails. If the length of a
  # temporal column stops being checked against the text, switch this to expect
  # 0 errors.
  specs <- DTAColumnSpecCollection(columns = list(
    VISDT = DTAColumnSpec(
      id = "VISDT", type = "SAS Date", format = "SAS DATE9.",
      length = 8, nullable = TRUE
    )
  ))
  ds <- DTADataSetTabular(
    id = "sas",
    specs = specs,
    files = list(DTAFileSAS7BDAT(filename = "delivery.sas7bdat"))
  )
  ds <- load_file(ds, file = sas_fx("delivery.sas7bdat"), handler_index = 1, stream = "never")

  details <- validate_table_detailed(
    specs, as.data.frame(tables(ds)[["delivery"]])["VISDT"],
    verbose = FALSE
  )
  errors <- details$columnspec_errors$full_error
  # Five of the six rows hold a date; the sixth is missing.
  expect_identical(nrow(errors), 5L)
  expect_true(all(errors$keyword == "maxLength"))
})
