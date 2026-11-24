# The SAS dataset (.sas7bdat) reader. REQ-FILE-034 .. REQ-FILE-040 and
# REQ-FILE-047, with REQ-FILE-011, REQ-FILE-012 and REQ-VAL-031 touched on the
# way. The last two cases declare the same variables in the R vocabulary.
#
# Every expected value in this file is written down from SAS semantics or taken
# from a source that is not the reader: the iris data set, literal values, and
# the bytes of the fixture files read back with base R. None of it was captured
# from a run. The fixtures live in fx/sas/:
#
#   iris.sas7bdat      written by SAS 9.04.01M2; the example file shipped with
#                      the haven package (MIT, see NOTICE-haven-iris.txt there).
#                      Its four numeric variables are bit-identical to the first
#                      four columns of datasets::iris and Species is stored $6.
#   delivery.sas7bdat  six observations written by haven, because no SAS-written
#                      file with these shapes is available. Built by
#                      .github/scripts/build_sas7bdat_fixtures.R, which also
#                      checks the SAS numbers it holds at byte level.
#   empty.sas7bdat     the same five variables and no observations.
#
# The SAS epoch is 1960-01-01 and R's is 1970-01-01, 3653 days apart, so
# 2025-08-15 is 20315 + 3653 = 23968 SAS days, and the SAS datetime
# 2025-08-15T14:30:00 is 2070887400 SAS seconds.

# ---- fixtures ---------------------------------------------------------------

fs_fx <- function(name) file.path(qa_suite_root(), "fx", "sas", name)

fs_handler <- function(filename) DTAFileSAS7BDAT(filename = filename)

fs_specs <- function(cols) {
  DTAColumnSpecCollection(
    columns = stats::setNames(cols, vapply(cols, function(x) x@id, character(1)))
  )
}

# The delivery's variables, declared the way SAS would describe them. "display"
# declares the dates and times with their SAS formats; "number" declares them as
# plain numbers. `date_length` adds a declared length to VISDT only.
fs_delivery_specs <- function(as = c("display", "number"), date_length = NULL, subjid = "SAS Char") {
  as <- match.arg(as)
  temporal <- if (identical(as, "display")) {
    list(
      DTAColumnSpec(
        id = "VISDT", type = "SAS Date", format = "SAS DATE9.",
        length = date_length, nullable = TRUE
      ),
      DTAColumnSpec(id = "VISDTM", type = "SAS DateTime", format = "SAS DATETIME20.", nullable = TRUE),
      DTAColumnSpec(id = "VISTM", type = "SAS Time", format = "SAS TIME8.", nullable = TRUE)
    )
  } else {
    lapply(c("VISDT", "VISDTM", "VISTM"), function(id) {
      DTAColumnSpec(id = id, type = "SAS Num", nullable = TRUE)
    })
  }
  fs_specs(c(
    list(
      DTAColumnSpec(id = "SUBJID", type = subjid, nullable = TRUE),
      DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    ),
    temporal
  ))
}

# Four numeric variables and the species. `sepal_length` and `species_values`
# are the two things the cases below vary.
fs_iris_specs <- function(sepal_length = "SAS Num", species_values = c("setosa", "versic", "virgin")) {
  fs_specs(list(
    DTAColumnSpec(id = "Sepal_Length", type = sepal_length, nullable = FALSE),
    DTAColumnSpec(id = "Sepal_Width", type = "SAS Num", nullable = FALSE),
    DTAColumnSpec(id = "Petal_Length", type = "SAS Num", nullable = FALSE),
    DTAColumnSpec(id = "Petal_Width", type = "SAS Num", nullable = FALSE),
    DTAColumnSpec(id = "Species", type = "SAS Char", values = species_values, nullable = FALSE)
  ))
}

fs_load <- function(path, specs, stream = "never") {
  ds <- DTADataSetTabular(
    id = "sas", specs = specs,
    files = list(fs_handler(basename(path)))
  )
  load_file(ds, file = path, handler_index = 1, stream = stream)
}

fs_check <- function(ds) check(ds, persist = FALSE, quiet = TRUE)

fs_verdict <- function(ds) {
  s <- validation_status(ds)
  list(
    ok = isTRUE(s$ok[[1]]),
    columnspec = as.integer(s$n_columnspec_errors[[1]]),
    rule = as.integer(s$n_rule_errors[[1]]),
    import = as.integer(s$n_import_errors[[1]])
  )
}

fs_clean <- list(ok = TRUE, columnspec = 0L, rule = 0L, import = 0L)

# The one table a single-file dataset holds, as a data frame.
fs_table <- function(ds) as.data.frame(tables(ds)[[1]])

fs_bytes <- function(path) readBin(path, "raw", n = file.size(path))

# How many times a double sits in the file, as the eight little-endian bytes a
# SAS numeric variable is stored as.
fs_n_double <- function(bytes, value) {
  length(grepRaw(writeBin(value, raw(), endian = "little"), bytes, fixed = TRUE, all = TRUE))
}

fs_n_text <- function(bytes, text) {
  length(grepRaw(charToRaw(text), bytes, fixed = TRUE, all = TRUE))
}

# ---- the handler --------------------------------------------------------------

test_that("OQ-FILE-057 | a SAS dataset handler is built three ways, is not a tabular handler, and a tabular dataset accepts it | REQ-FILE-034", {
  direct <- DTAFileSAS7BDAT(filename = "ae.sas7bdat")
  via_factory <- DTAFileFactory(type = "sas7bdat", filename = "ae.sas7bdat")
  via_yaml <- dta_file_handlers_from_list(list(type = "sas7bdat", filename = "ae.sas7bdat"))

  qa_step("the YAML declaration yields exactly one handler", 1L, length(via_yaml))

  built <- list(direct = direct, factory = via_factory, yaml = via_yaml[[1]])
  for (how in names(built)) {
    h <- built[[how]]
    qa_step(
      sprintf("built by %s: a SAS dataset handler for the declared file name", how),
      list(sas = TRUE, tabular = FALSE, filename = "ae.sas7bdat"),
      list(
        sas = S7::S7_inherits(h, DTAFileSAS7BDAT),
        tabular = S7::S7_inherits(h, DTAFileTabular),
        filename = h@filename
      )
    )
  }

  ds <- DTADataSetTabular(
    id = "ae", specs = DTAColumnSpecCollection(columns = list()),
    files = list(direct)
  )
  qa_check("a tabular dataset accepts a SAS dataset handler", S7::S7_inherits(ds, DTADataSetTabular))

  # The acceptance above is not vacuous: the same dataset still refuses a
  # handler that is neither tabular nor a SAS dataset reader.
  refused <- tryCatch(
    DTADataSetTabular(
      id = "ae", specs = DTAColumnSpecCollection(columns = list()),
      files = list(DTAFileAny(filename = "ae.xpt"))
    ),
    error = function(e) e
  )
  # The refusal is the package's own text: it names the handler that is not
  # tabular and the three handlers that are accepted.
  refusal <- if (inherits(refused, "error")) gsub("\\s+", " ", conditionMessage(refused)) else ""
  qa_step(
    "a tabular dataset still refuses an unrelated handler, naming it and the readable handlers",
    c(refused = TRUE, names_handler = TRUE, names_sas = TRUE),
    c(
      refused = grepl("not tabular", refusal, fixed = TRUE),
      names_handler = grepl("ae.xpt", refusal, fixed = TRUE),
      names_sas = grepl("DTAFileSAS7BDAT", refusal, fixed = TRUE)
    )
  )
})

# ---- values as stored -----------------------------------------------------------

test_that("OQ-FILE-058 | a SAS-written dataset reads back every observation and variable, names and values as stored | REQ-FILE-035", {
  path <- fs_fx("iris.sas7bdat")
  df <- as.data.frame(read_file(fs_handler("iris.sas7bdat"), path))

  qa_step(
    "every variable, named exactly as stored, case included",
    c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width", "Species"),
    names(df)
  )
  qa_step("every observation", 150L, nrow(df))

  # datasets::iris is the oracle: the four numeric variables in the SAS file are
  # bit-identical to its first four columns.
  oracle <- datasets::iris
  numeric_names <- c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width")
  for (i in 1:4) {
    qa_step(
      sprintf("%s holds exactly the stored doubles", numeric_names[[i]]),
      as.numeric(oracle[[i]]),
      as.numeric(df[[numeric_names[[i]]]])
    )
  }
  # Species is stored as $6, so SAS holds the first six characters of each name.
  qa_step(
    "Species holds the stored text, cut to its six-character width by SAS",
    substr(as.character(oracle$Species), 1, 6),
    as.character(df$Species)
  )

  # The bytes of the file say the same thing without any decoder: 50 of each
  # six-character name, and the full name never stored.
  bytes <- fs_bytes(path)
  qa_step(
    "the file holds each six-character species name 50 times and no full name",
    c(setosa = 50L, versic = 50L, virgin = 50L, versicolor = 0L, virginica = 0L),
    c(
      setosa = fs_n_text(bytes, "setosa"),
      versic = fs_n_text(bytes, "versic"),
      virgin = fs_n_text(bytes, "virgin"),
      versicolor = fs_n_text(bytes, "versicolor"),
      virginica = fs_n_text(bytes, "virginica")
    )
  )
})

test_that("OQ-FILE-059 | blank is missing, trailing blanks are not part of a value, and leading blanks, zeros, non-ASCII text and 'NA' are kept | REQ-FILE-036", {
  df <- as.data.frame(read_file(fs_handler("delivery.sas7bdat"), fs_fx("delivery.sas7bdat")))

  qa_step(
    "the five variables, named as stored",
    c("SUBJID", "AGE", "VISDT", "VISDTM", "VISTM"),
    names(df)
  )
  # Row 1 keeps its leading zeros, row 2 its leading blanks, row 3 loses the
  # blanks written after it, row 4 was blank and is missing, row 5 is the
  # two-letter value NA and row 6 has a non-ASCII letter.
  qa_step(
    "the six character values as SAS defines them",
    c("007", "  lead", "trail", NA, "NA", "M\u00fcller"),
    as.character(df$SUBJID)
  )
  qa_step(
    "a numeric variable keeps its stored values, a missing number stays missing",
    c(30, 41.5, NA, 0, -2, 1e10),
    as.numeric(df$AGE)
  )
})

# ---- dates, datetimes and times --------------------------------------------------

test_that("OQ-FILE-060 | SAS dates, datetimes and times read as display text under their format, as SAS numbers under a numeric type, and as ISO text otherwise | REQ-FILE-037", {
  path <- fs_fx("delivery.sas7bdat")

  # Nothing declared: ISO 8601, with the missing row still missing.
  iso <- as.data.frame(read_file(fs_handler("delivery.sas7bdat"), path))
  qa_step(
    "dates with no declaration are ISO text",
    c("1960-01-01", "2025-08-15", NA, "1959-12-31", "2000-02-29", "1970-01-01"),
    as.character(iso$VISDT)
  )
  qa_step(
    "datetimes with no declaration are ISO text",
    c(
      "1960-01-01T00:00:00", "2025-08-15T14:30:00", NA, "1959-12-31T23:59:59",
      "2000-02-29T12:00:00", "1970-01-01T00:00:00"
    ),
    as.character(iso$VISDTM)
  )
  qa_step(
    "times with no declaration are ISO text",
    c("00:00:00", "14:30:00", NA, "23:59:59", "12:00:00", "01:00:00"),
    as.character(iso$VISTM)
  )

  # Declared as dates, datetimes and times with their SAS formats: the text SAS
  # itself would print, and a delivery that checks clean.
  shown <- fs_check(fs_load(path, fs_delivery_specs("display")))
  shown_df <- fs_table(shown)
  qa_step(
    "dates under DATE9. read as SAS prints them",
    c("01JAN1960", "15AUG2025", NA, "31DEC1959", "29FEB2000", "01JAN1970"),
    as.character(shown_df$VISDT)
  )
  qa_step(
    "datetimes under DATETIME20. read as SAS prints them",
    c(
      "01JAN1960:00:00:00", "15AUG2025:14:30:00", NA, "31DEC1959:23:59:59",
      "29FEB2000:12:00:00", "01JAN1970:00:00:00"
    ),
    as.character(shown_df$VISDTM)
  )
  qa_step(
    "times under TIME8. read as SAS prints them, the hour unpadded",
    c("0:00:00", "14:30:00", NA, "23:59:59", "12:00:00", "1:00:00"),
    as.character(shown_df$VISTM)
  )
  qa_step("the display text satisfies its own declared formats", fs_clean, fs_verdict(shown))

  # Declared as plain numbers: the SAS number itself, days since 1960-01-01 for
  # a date and seconds for a datetime and a time.
  numbers <- fs_check(fs_load(path, fs_delivery_specs("number")))
  numbers_df <- fs_table(numbers)
  qa_step(
    "dates under SAS Num are days since 1960-01-01",
    c(0, 23968, NA, -1, 14669, 3653),
    as.numeric(numbers_df$VISDT)
  )
  qa_step(
    "datetimes under SAS Num are seconds since 1960-01-01 00:00:00",
    c(0, 2070887400, NA, -1, 1267444800, 315619200),
    as.numeric(numbers_df$VISDTM)
  )
  qa_step(
    "times under SAS Num are seconds since midnight",
    c(0, 52200, NA, 86399, 43200, 3600),
    as.numeric(numbers_df$VISTM)
  )
  qa_step("the numbers satisfy a numeric declaration", fs_clean, fs_verdict(numbers))

  # The same numbers, found in the file without any decoder. Each is present once
  # as a little-endian double. 20315 is what 2025-08-15 would be had the
  # writer counted from 1970 instead of 1960, and it must not be there.
  bytes <- fs_bytes(path)
  expected_hits <- c(
    `23968` = 1L, `2070887400` = 1L, `14669` = 1L, `1267444800` = 1L,
    `315619200` = 1L, `52200` = 1L, `3653` = 1L, `20315` = 0L
  )
  observed_hits <- vapply(
    as.numeric(names(expected_hits)),
    function(v) fs_n_double(bytes, v),
    integer(1)
  )
  names(observed_hits) <- names(expected_hits)
  qa_step(
    "the file holds the SAS day and second counts, and not the 1970-based day count",
    expected_hits, observed_hits
  )
})

# ---- type mismatch, and two pinned limitations -------------------------------------

test_that("OQ-FILE-061 | a SAS dataset checks clean under its own description, a mistyped variable is a column-specification violation, and two limitations are pinned | REQ-FILE-035 REQ-FILE-038 REQ-VAL-031", {
  path <- fs_fx("iris.sas7bdat")

  qa_step(
    "iris under a specification that describes it checks clean on all three axes",
    fs_clean,
    fs_verdict(fs_check(fs_load(path, fs_iris_specs())))
  )

  # Numbers under SAS Char: one column-specification violation per observation,
  # and nothing on the import axis, because nothing was parsed from text.
  as_char <- fs_verdict(fs_check(fs_load(path, fs_iris_specs(sepal_length = "SAS Char"))))
  qa_step(
    "Sepal_Length declared SAS Char: 150 type violations, no import errors",
    list(ok = FALSE, columnspec = 150L, import = 0L),
    list(ok = as_char$ok, columnspec = as_char$columnspec, import = as_char$import)
  )

  # A fraction under SAS Int. The count is the number of non-whole lengths in
  # the iris data set itself.
  n_fractional <- sum(datasets::iris$Sepal.Length %% 1 != 0)
  as_int <- fs_verdict(fs_check(fs_load(path, fs_iris_specs(sepal_length = "SAS Int"))))
  qa_step(
    "Sepal_Length declared SAS Int: one type violation per fractional value, no import errors",
    list(columnspec = as.integer(n_fractional), import = 0L),
    list(columnspec = as_int$columnspec, import = as_int$import)
  )

  # Only setosa permitted: the 50 versic and 50 virgin observations break it.
  narrow <- fs_verdict(fs_check(fs_load(path, fs_iris_specs(species_values = "setosa"))))
  qa_step(
    "Species restricted to setosa: the other 100 observations are reported, no import errors",
    list(ok = FALSE, columnspec = 100L, import = 0L),
    list(ok = narrow$ok, columnspec = narrow$columnspec, import = narrow$import)
  )

  # The exception to the rule above: TEXT under SAS Num is parsed, as it is from
  # a delimited file, so a value that does not parse is an import error and
  # nothing is reported on the column-specification axis. The text written into
  # SUBJID by .github/scripts/build_sas7bdat_fixtures.R is "007", "  lead",
  # "trail" (after the padding is dropped), "" (missing), "NA" and a surname
  # with an umlaut: "007" parses to 7, "" stays missing, and the other four are
  # not numbers.
  as_num <- fs_check(fs_load(
    fs_fx("delivery.sas7bdat"),
    fs_delivery_specs("display", subjid = "SAS Num")
  ))
  qa_step(
    "SUBJID text declared SAS Num: four import errors, no column-specification violation",
    list(columnspec = 0L, rule = 0L, import = 4L),
    {
      v <- fs_verdict(as_num)
      list(columnspec = v$columnspec, rule = v$rule, import = v$import)
    }
  )
  qa_step(
    "and the column holds 7 for '007' and missing for the empty text and the four that do not parse",
    c(7, NA, NA, NA, NA, NA),
    fs_table(as_num)$SUBJID
  )

  # LIM-011: a number is checked as stored, not as displayed. SAS would print
  # one third as 0.33 under 8.2, but 0.3333333333333333 has more decimals than
  # the format shows and is reported. Pinned, not endorsed: if the check starts
  # rounding to the displayed value this becomes zero errors.
  lim011 <- validate_table(
    specs = fs_specs(list(DTAColumnSpec(id = "X", type = "SAS Num", format = "SAS 8.2", nullable = FALSE))),
    table = data.frame(X = 1 / 3),
    verbose = FALSE
  )
  qa_step(
    "LIM-011: one third under 8.2 is one format violation, as stored",
    list(rows = 1L, keyword = "format"),
    list(
      rows = if (is.null(lim011$full_error)) 0L else nrow(lim011$full_error),
      keyword = if (is.null(lim011$full_error)) NA_character_ else as.character(lim011$full_error$keyword[[1]])
    )
  )

  # LIM-012: a length declared on a SAS date is compared with the display text,
  # not with the eight-byte storage PROC CONTENTS reports. 01JAN1960 and the
  # four other non-missing dates are nine characters, which exceeds 8.
  # Pinned, not endorsed.
  with_length <- fs_check(fs_load(fs_fx("delivery.sas7bdat"), fs_delivery_specs("display", date_length = 8)))
  msgs <- as.data.frame(messages(with_length))
  qa_step(
    "LIM-012: every non-missing date text exceeds a declared length of 8",
    list(max_length_on_date = 5L, columnspec = 5L),
    list(
      max_length_on_date = sum(as.character(msgs$column) == "VISDT" & as.character(msgs$keyword) == "maxLength"),
      columnspec = fs_verdict(with_length)$columnspec
    )
  )
})

# ---- memory only --------------------------------------------------------------------

test_that("OQ-FILE-062 | a SAS dataset is held in memory under auto, and always and open_file are refused naming stream = never | REQ-FILE-039", {
  path <- fs_fx("iris.sas7bdat")
  # LIM-010: with a one-byte threshold every file is "large", which is the case
  # where a delimited file would be streamed and a SAS dataset cannot be.
  withr::local_options(DTAtools.stream_threshold = 1)

  seen <- character(0)
  auto <- withCallingHandlers(
    fs_load(path, fs_iris_specs(), stream = "auto"),
    message = function(m) {
      seen <<- c(seen, class(m))
      invokeRestart("muffleMessage")
    }
  )
  held <- tables(auto)[[1]]
  qa_step(
    "stream = 'auto' above the threshold holds an in-memory table of every row and says so",
    list(message = TRUE, in_memory_table = TRUE, lazy_dataset = FALSE, rows = 150L),
    list(
      message = "dta_sas7bdat_in_memory" %in% seen,
      in_memory_table = inherits(held, "Table"),
      lazy_dataset = inherits(held, "Dataset"),
      rows = nrow(held)
    )
  )

  never <- fs_check(fs_load(path, fs_iris_specs(), stream = "never"))
  qa_step(
    "and reaches the verdict that stream = 'never' reaches",
    fs_verdict(never),
    fs_verdict(fs_check(auto))
  )

  refusal <- function(expr) {
    e <- tryCatch(expr, error = function(e) e)
    if (!inherits(e, "error")) {
      return(NA_character_)
    }
    gsub("\\s+", " ", conditionMessage(e))
  }
  refused <- list(
    always = refusal(fs_load(path, fs_iris_specs(), stream = "always")),
    open_file = refusal(open_file(fs_handler("iris.sas7bdat"), path))
  )
  for (how in names(refused)) {
    qa_step(
      sprintf("%s is refused, says streaming is not supported and names stream = \"never\"", how),
      c(not_supported = TRUE, names_never = TRUE),
      c(
        not_supported = isTRUE(grepl("Streaming is not supported", refused[[how]], fixed = TRUE)),
        names_never = isTRUE(grepl("stream = \"never\"", refused[[how]], fixed = TRUE))
      )
    )
  }
})

# ---- files that are not datasets --------------------------------------------------------

test_that("OQ-FILE-063 | a zero-byte or non-SAS file is refused with the path, and a dataset without observations reads as zero rows | REQ-FILE-040 REQ-FILE-012", {
  dir <- qa_tempdir()
  h <- fs_handler("ae.sas7bdat")

  # Two bad files with the same name in two directories, so the handler's file
  # name matches both.
  dir.create(file.path(dir, "zero"))
  dir.create(file.path(dir, "text"))
  zero <- file.path(dir, "zero", "ae.sas7bdat")
  file.create(zero)
  not_sas <- file.path(dir, "text", "ae.sas7bdat")
  writeLines("these bytes are text, not a SAS dataset", not_sas)

  for (what in c("zero", "not_sas")) {
    p <- get(what)
    cnd <- tryCatch(read_file(h, p), error = function(e) e)
    qa_step(
      sprintf("%s: refused with a dta_sas7bdat_read_error that carries the path", what),
      list(class = TRUE, path = normalizePath(p, winslash = "/")),
      list(
        class = inherits(cnd, "dta_sas7bdat_read_error"),
        path = if (is.null(cnd$path)) NA_character_ else normalizePath(cnd$path, winslash = "/", mustWork = FALSE)
      )
    )
  }
  # A text value that is not valid in the encoding the file declares (haven
  # writes UTF-8; the byte 0xDC of a Latin-1 umlaut is patched into the second
  # value) is refused, naming the column and the first row it is in.
  enc <- file.path(dir, "enc.sas7bdat")
  suppressWarnings(haven::write_sas(data.frame(NAME = c("ABC", "MXLLER"), stringsAsFactors = FALSE), enc))
  b <- readBin(enc, "raw", file.size(enc))
  at <- grepRaw("MXLLER", b, fixed = TRUE)
  qa_check("the written value is stored once, verbatim", length(at) == 1L, length(at))
  b[at + 1L] <- as.raw(0xDC)
  writeBin(b, enc)
  cnd <- tryCatch(read_file(fs_handler("enc.sas7bdat"), enc), error = function(e) e)
  msg <- if (inherits(cnd, "condition")) gsub("[[:space:]]+", " ", conditionMessage(cnd)) else ""
  qa_step(
    "text that is not valid in the declared encoding is refused, naming column NAME and row 2",
    list(class = TRUE, column = TRUE, row = TRUE),
    list(
      class = inherits(cnd, "dta_sas7bdat_read_error"),
      column = grepl("NAME", msg, fixed = TRUE),
      row = grepl("row 2", msg, fixed = TRUE)
    )
  )

  # No observations is a dataset, not an error.
  empty <- read_file(fs_handler("empty.sas7bdat"), fs_fx("empty.sas7bdat"))
  qa_step(
    "a dataset with no observations reads as zero rows with its variables",
    list(rows = 0L, names = c("SUBJID", "AGE", "VISDT", "VISDTM", "VISTM")),
    list(rows = nrow(empty), names = names(empty))
  )
  loaded <- fs_check(fs_load(fs_fx("empty.sas7bdat"), fs_delivery_specs("display")))
  qa_step("and loads and checks clean under a specification", fs_clean, fs_verdict(loaded))
})

# ---- compression ----------------------------------------------------------------------------

test_that("OQ-FILE-064 | a gzip-compressed SAS dataset matches the plain declaration and reads to the same table and verdict | REQ-FILE-011", {
  dir <- qa_tempdir()
  plain <- fs_fx("iris.sas7bdat")
  gz <- file.path(dir, "iris.sas7bdat.gz")
  con <- gzfile(gz, "wb")
  writeBin(fs_bytes(plain), con)
  close(con)
  md5_before <- unname(tools::md5sum(gz))

  h <- fs_handler("iris.sas7bdat")
  qa_step(
    "a declaration that says nothing about compression matches the .gz delivery",
    TRUE,
    isTRUE(matches_filename(h, "iris.sas7bdat.gz"))
  )
  qa_step(
    "the compressed delivery reads to the identical table",
    as.data.frame(read_file(h, plain)),
    as.data.frame(read_file(h, gz))
  )

  from_plain <- fs_check(fs_load(plain, fs_iris_specs()))
  from_gz <- fs_check(fs_load(gz, fs_iris_specs()))
  qa_step("and to the same, clean, verdict", fs_verdict(from_plain), fs_verdict(from_gz))
  qa_step(
    "the table is named after the file without either ending",
    "iris",
    names(tables(from_gz))[[1]]
  )
  qa_step(
    "reading left the compressed file as it was",
    md5_before,
    unname(tools::md5sum(gz))
  )
})

# ---- the same variables declared in the R vocabulary -------------------------------------
#
# REQ-FILE-037 (the R half) and REQ-FILE-047. The epoch arithmetic is the one
# stated at the top of this file: SAS day 0 is 1960-01-01, 23968 is 2025-08-15,
# -1 is 1959-12-31, 14669 is 2000-02-29 and 3653 is 1970-01-01; the datetime
# seconds are those days counted in seconds, plus the time of day.

# delivery.sas7bdat with every variable declared as an R type. A NULL format
# stores the default of the type.
fs_delivery_r_specs <- function(date = "R Date", datetime = "R POSIXct", time = "R hms",
                                date_format = NULL, datetime_format = NULL, time_format = NULL) {
  fs_specs(list(
    DTAColumnSpec(id = "SUBJID", type = "R character", nullable = TRUE),
    DTAColumnSpec(id = "AGE", type = "R numeric", nullable = TRUE),
    DTAColumnSpec(id = "VISDT", type = date, format = date_format, nullable = TRUE),
    DTAColumnSpec(id = "VISDTM", type = datetime, format = datetime_format, nullable = TRUE),
    DTAColumnSpec(id = "VISTM", type = time, format = time_format, nullable = TRUE)
  ))
}

test_that("OQ-FILE-072 | SAS dates, datetimes and times declared R Date, R POSIXct and R hms read as text in the column's format, and as the SAS number under any other R type | REQ-FILE-037", {
  path <- fs_fx("delivery.sas7bdat")

  # No format named: the default of each type.
  default <- fs_check(fs_load(path, fs_delivery_r_specs()))
  default_df <- fs_table(default)
  qa_step(
    "dates under R Date read as %Y-%m-%d",
    c("1960-01-01", "2025-08-15", NA, "1959-12-31", "2000-02-29", "1970-01-01"),
    as.character(default_df$VISDT)
  )
  qa_step(
    "datetimes under R POSIXct read as %Y-%m-%d %H:%M:%S",
    c(
      "1960-01-01 00:00:00", "2025-08-15 14:30:00", NA, "1959-12-31 23:59:59",
      "2000-02-29 12:00:00", "1970-01-01 00:00:00"
    ),
    as.character(default_df$VISDTM)
  )
  qa_step(
    "times under R hms read as %H:%M:%S, the hour padded to two digits",
    c("00:00:00", "14:30:00", NA, "23:59:59", "12:00:00", "01:00:00"),
    as.character(default_df$VISTM)
  )
  qa_step("the text satisfies the formats it was written under", fs_clean, fs_verdict(default))

  # A format named on the column is the one the text is written in.
  named <- fs_check(fs_load(path, fs_delivery_r_specs(
    date_format = "R %d/%m/%Y", datetime_format = "R %Y-%m-%dT%H:%M:%SZ", time_format = "R %H:%M"
  )))
  named_df <- fs_table(named)
  qa_step(
    "dates under R %d/%m/%Y",
    c("01/01/1960", "15/08/2025", NA, "31/12/1959", "29/02/2000", "01/01/1970"),
    as.character(named_df$VISDT)
  )
  qa_step(
    "datetimes under R %Y-%m-%dT%H:%M:%SZ",
    c(
      "1960-01-01T00:00:00Z", "2025-08-15T14:30:00Z", NA, "1959-12-31T23:59:59Z",
      "2000-02-29T12:00:00Z", "1970-01-01T00:00:00Z"
    ),
    as.character(named_df$VISDTM)
  )
  qa_step(
    "times under R %H:%M",
    c("00:00", "14:30", NA, "23:59", "12:00", "01:00"),
    as.character(named_df$VISTM)
  )
  qa_step("and the verdict is clean", fs_clean, fs_verdict(named))

  # Any other R type is the SAS number, days or seconds from 1960-01-01.
  numbers <- fs_check(fs_load(path, fs_delivery_r_specs(date = "R numeric", datetime = "R numeric", time = "R integer")))
  numbers_df <- fs_table(numbers)
  qa_step(
    "dates under R numeric are days since 1960-01-01",
    c(0, 23968, NA, -1, 14669, 3653), as.numeric(numbers_df$VISDT)
  )
  qa_step(
    "datetimes under R numeric are seconds since 1960-01-01 00:00:00",
    c(0, 2070887400, NA, -1, 1267444800, 315619200), as.numeric(numbers_df$VISDTM)
  )
  qa_step(
    "times under R integer are seconds since midnight",
    c(0, 52200, NA, 86399, 43200, 3600), as.numeric(numbers_df$VISTM)
  )
  qa_step("the numbers satisfy a numeric declaration", fs_clean, fs_verdict(numbers))

  for (type in c("R character", "R logical")) {
    other <- fs_table(fs_load(path, fs_delivery_r_specs(date = type)))
    qa_step(
      paste("dates under", type, "are still the SAS number"),
      c(0, 23968, NA, -1, 14669, 3653), as.numeric(other$VISDT)
    )
  }
})

test_that("OQ-FILE-073 | an R date or time is written in UTC and to the fraction the format shows, cut and never rounded, and a mismatched kind shows ISO text the format reports | REQ-FILE-047 | tags: white-box", {
  normalise <- function(x, ...) DTAtools:::dta_sas7bdat_normalise(data.frame(X = x), fs_specs(list(DTAColumnSpec(id = "X", ...))))$X

  base <- as.POSIXct("2025-08-15 14:30:00", tz = "UTC")
  qa_step(
    "a datetime held in another time zone is written as the UTC time of the same instant",
    "2025-08-15 14:30:00",
    normalise(structure(base, tzone = "Asia/Tokyo"), type = "R POSIXct")
  )

  # 0.3 seconds is .300, which format(x, "%OS3") would print as .299.
  qa_step(
    "%OS3 shows the stored fraction to three digits: 0.3, 0.5 and 0.123456 seconds",
    c("2025-08-15 14:30:00.300", "2025-08-15 14:30:00.500", "2025-08-15 14:30:00.123"),
    normalise(base + c(0.3, 0.5, 0.123456), type = "R POSIXct", format = "R %Y-%m-%d %H:%M:%OS3")
  )
  qa_step(
    "%OS1 cuts 0.19 seconds to .1 and 0.96 seconds to .9, never rounding up",
    c("2025-08-15 14:30:00.1", "2025-08-15 14:30:00.9"),
    normalise(base + c(0.19, 0.96), type = "R POSIXct", format = "R %Y-%m-%d %H:%M:%OS1")
  )
  tod <- function(seconds) structure(seconds, class = c("hms", "difftime"), units = "secs")
  qa_step(
    "a time of 52200.5 and 52200.123 seconds under %H:%M:%OS3",
    c("14:30:00.500", "14:30:00.123"),
    normalise(tod(c(52200.5, 52200.123)), type = "R hms", format = "R %H:%M:%OS3")
  )

  # A time past a day keeps its real hour count, and the R hms format reports it.
  long <- normalise(tod(c(90000, 86399)), type = "R hms")
  qa_step("25 hours is written 25:00:00", c("25:00:00", "23:59:59"), long)
  report <- validate_table(
    fs_specs(list(DTAColumnSpec(id = "X", type = "R hms", nullable = TRUE))),
    data.frame(X = long),
    verbose = FALSE
  )
  qa_step(
    "...and only that one value fails the time-of-day format",
    "1 format", paste(report$full_error$row, report$full_error$keyword)
  )

  # LIM-035: what the format does not show is cut, so a typed value finer than
  # its format is written as the format shows it and then passes the format
  # check; matching is exact for text sources only. 52245 seconds is 14:30:45
  # and 52245.1234 is 14:30:45.1234, stated from the clock, not from the renderer.
  fine <- normalise(tod(c(52245, 52245.1234)), type = "R hms", format = "R %H:%M")
  qa_step("LIM-035: 14:30:45 under %H:%M is written 14:30, and so is 14:30:45.1234", c("14:30", "14:30"), fine)
  finer <- normalise(tod(52245.1234), type = "R hms", format = "R %H:%M:%OS3")
  qa_step("LIM-035: 14:30:45.1234 under %H:%M:%OS3 is written 14:30:45.123", "14:30:45.123", finer)
  # The same for a SAS time: TIME5. is hh:mm, so 14:30:45 is written 14:30.
  qa_step(
    "LIM-035: 14:30:45 declared SAS Time with TIME5. is written 14:30, as under R %H:%M",
    "14:30", normalise(tod(52245), type = "SAS Time", format = "SAS TIME5.")
  )
  qa_step(
    "LIM-035: the cut text passes the format check, whatever was cut",
    0L,
    length(validate_table(
      fs_specs(list(DTAColumnSpec(id = "X", type = "R hms", format = "R %H:%M", nullable = TRUE))),
      data.frame(X = fine),
      verbose = FALSE
    )$full_error$row)
  )

  # A declaration that is not the kind of variable SAS stored.
  day <- as.Date("2025-08-15")
  qa_step("a date declared R POSIXct is shown as ISO text", "2025-08-15", normalise(day, type = "R POSIXct"))
  qa_step("a date declared R hms is shown as ISO text", "2025-08-15", normalise(day, type = "R hms"))
  qa_step("a datetime declared R Date is shown as ISO text", "2025-08-15T14:30:00", normalise(base, type = "R Date"))
  qa_step("a time declared R Date is shown as ISO text", "14:30:00", normalise(tod(52200), type = "R Date"))

  # In a file, the five dates of the delivery then fail the datetime format.
  mismatch <- fs_check(fs_load(fs_fx("delivery.sas7bdat"), fs_delivery_r_specs(date = "R POSIXct")))
  qa_step(
    "the five dates that are not missing are five format findings, and nothing else is reported",
    list(ok = FALSE, columnspec = 5L, rule = 0L, import = 0L), fs_verdict(mismatch)
  )
})
