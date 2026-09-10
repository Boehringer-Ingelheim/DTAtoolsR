# Compression, edge-case files, and the stream/handler-routing mechanics of
# reading. REQ-FILE-010 .. REQ-FILE-024.

# ---- fixtures ---------------------------------------------------------------

# Writes `lines` as-is, in binary mode: several cases below depend on the
# exact bytes on disk (a truly empty line, CRLF, a raw quoted newline) and
# writeLines()'s own platform line-ending translation would erase the very
# thing being tested.
fr_write_raw <- function(lines, path, sep = "\n") {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeChar(paste0(paste(lines, collapse = sep), sep), con, eos = NULL, useBytes = TRUE)
  path
}

# Passed as `specs` to a bare read_file() wherever a case compares exact
# values: supplying ANY DTAColumnSpecCollection, even one declaring no
# columns, pins every column of the read result to text (see
# dta_delim_reader_plan()), so a numeric-looking value ("30") is read back as
# the string it is in the file rather than inferred as a number -- which is
# not what these particular cases are about.
fr_text_specs <- DTAColumnSpecCollection(columns = list())

# A small two-column specification used wherever a case needs a validation
# VERDICT rather than a bare read: REQ-FILE-011 and REQ-FILE-022 promise "the
# same verdict", which a raw table comparison cannot stand in for.
fr_specs <- function() {
  DTAColumnSpecCollection(columns = list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
    AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
  ))
}

fr_verdict <- function(path, filename = basename(path), specs = fr_specs(), stream = "never") {
  ds <- DTADataSetTabular(
    name = "r", specs = specs,
    files = list(DTAFileCSV(filename = filename))
  )
  ds <- load_file(ds, file = path, handler_index = 1, stream = stream)
  ds <- check(ds, persist = FALSE, quiet = TRUE)
  validation_status(ds)
}

fr_verdict_fields <- function(status) {
  list(
    ok = status$ok,
    columnspec = as.integer(status$n_columnspec_errors),
    import = as.integer(status$n_import_errors)
  )
}

# ---- the .zip guard -----------------------------------------------------------------

test_that("OQ-FILE-020 | a .zip file is refused immediately with an actionable message | REQ-FILE-010", {
  dir <- qa_tempdir()
  p <- file.path(dir, "delivery.csv.zip")
  # The content does not matter -- the extension alone must be enough to
  # refuse the file before a single byte of it is parsed.
  writeLines("not really a zip, and it must not matter", p)

  err_read <- tryCatch(read_file(DTAFileCSV(filename = "delivery.csv.zip"), p), error = function(e) e)
  qa_check("read_file() raises a condition for a .zip file", inherits(err_read, "condition"))
  qa_step(
    "the message names the reason and a supported alternative, not an opaque parse failure",
    TRUE,
    grepl(".zip archives are not supported.", conditionMessage(err_read), fixed = TRUE)
  )

  err_open <- tryCatch(open_file(DTAFileCSV(filename = "delivery.csv.zip"), p), error = function(e) e)
  qa_check("open_file() refuses the same file before opening it", inherits(err_open, "condition"))
  qa_step(
    "with the identical message as read_file(), since both share one guard",
    conditionMessage(err_read),
    conditionMessage(err_open)
  )
})

# ---- compression --------------------------------------------------------------------

test_that("OQ-FILE-021 | every extension dta_compression_extensions() advertises reads to the same verdict as uncompressed | REQ-FILE-011", {
  dir <- qa_tempdir()
  exts <- dta_compression_extensions()

  # Asserted explicitly, not just looped over: if this package ever starts
  # advertising a second extension, this comparison fails loudly and says so,
  # rather than the loop below silently exercising only the ones it already
  # knew how to write.
  qa_step("the currently advertised set of auto-detected compression extensions", "gz", exts)

  frame <- data.frame(ID = c("A0000001", "A0000002"), AGE = c("30", "40"), stringsAsFactors = FALSE)
  p_plain <- file.path(dir, "plain.csv")
  qa_write_csv(frame, p_plain)
  plain_verdict <- fr_verdict_fields(fr_verdict(p_plain))

  for (ext in exts) {
    p_comp <- file.path(dir, paste0("comp.csv.", ext))
    # "gz" is the only member of `exts` today; gzip is the only compression
    # qa_write_csv() knows how to produce, which is what this end-to-end case
    # actually needs.
    qa_write_csv(frame, p_comp, gzip = identical(ext, "gz"))
    comp_verdict <- fr_verdict_fields(fr_verdict(p_comp, filename = basename(p_comp)))

    qa_step(
      sprintf("a '.%s'-compressed file reaches the same verdict as its uncompressed twin", ext),
      plain_verdict,
      comp_verdict
    )
  }
})

# ---- edge-case files ------------------------------------------------------------------

test_that("OQ-FILE-022 | an empty file is refused rather than read as zero rows | REQ-FILE-012", {
  dir <- qa_tempdir()
  p <- file.path(dir, "empty.csv")
  file.create(p)

  err_read <- tryCatch(read_file(DTAFileCSV(filename = "empty.csv"), p), error = function(e) e)
  qa_check("read_file() raises a condition for a zero-byte file", inherits(err_read, "condition"))

  err_open <- tryCatch(open_file(DTAFileCSV(filename = "empty.csv"), p), error = function(e) e)
  qa_check("open_file() raises a condition for the same file", inherits(err_open, "condition"))
})

test_that("OQ-FILE-023 | a header-only file reads as a valid table of zero rows | REQ-FILE-013", {
  dir <- qa_tempdir()
  p <- file.path(dir, "headeronly.csv")
  fr_write_raw(c("ID,VAL"), p)

  t <- read_file(DTAFileCSV(filename = "headeronly.csv"), p)

  qa_step("the declared columns are exposed", c("ID", "VAL"), names(t))
  qa_step("and there are no rows", 0L, nrow(t))
})

test_that("OQ-FILE-024 | a line with no content at all is dropped as a record separator | REQ-FILE-014", {
  dir <- qa_tempdir()
  p <- file.path(dir, "blankline.csv")
  # The middle line is genuinely empty -- zero characters between the two
  # newlines -- which is what LIM-001 is about. A row of comma-separated
  # empty fields ("," for two columns) is a different, NOT-dropped case,
  # covered by OQ-FILE-025.
  fr_write_raw(c("ID,AGE", "A001,30", "", "B002,50"), p)

  t <- read_file(DTAFileCSV(filename = "blankline.csv"), p, specs = fr_text_specs)

  qa_step("the blank line contributes no row: two rows remain, not three", 2L, nrow(t))
  qa_step(
    "the two surviving rows are the two non-blank lines",
    data.frame(ID = c("A001", "B002"), AGE = c("30", "50"), stringsAsFactors = FALSE),
    as.data.frame(t)
  )
})

test_that("OQ-FILE-025 | a row that is only partially empty survives intact | REQ-FILE-014", {
  dir <- qa_tempdir()
  p <- file.path(dir, "partial.csv")
  fr_write_raw(c("ID,AGE", "A001,30", ",40", "B002,"), p)

  t <- read_file(DTAFileCSV(filename = "partial.csv"), p, specs = fr_text_specs)

  qa_step("all three rows survive: a merely partial emptiness never drops a row", 3L, nrow(t))
  qa_step(
    "a leading empty field and a trailing empty field both become missing, not dropped",
    data.frame(ID = c("A001", NA, "B002"), AGE = c("30", "40", NA), stringsAsFactors = FALSE),
    as.data.frame(t)
  )
})

test_that("OQ-FILE-026 | an embedded newline in a quoted field is preserved | REQ-FILE-015", {
  dir <- qa_tempdir()
  p <- file.path(dir, "embedded_nl.csv")
  fr_write_raw(c("ID,NOTE", 'A001,"line1\nline2"', "A002,plain"), p)

  t <- read_file(DTAFileCSV(filename = "embedded_nl.csv"), p)

  qa_step(
    "the quoted value keeps its internal line break, and the row count is unaffected",
    data.frame(ID = c("A001", "A002"), NOTE = c("line1\nline2", "plain"), stringsAsFactors = FALSE),
    as.data.frame(t)
  )
})

test_that("OQ-FILE-027 | CRLF line endings read identically to LF | REQ-FILE-016", {
  dir <- qa_tempdir()
  lines <- c("ID,VAL", "A001,10", "A002,20")

  p_lf <- file.path(dir, "lf.csv")
  fr_write_raw(lines, p_lf, sep = "\n")
  p_crlf <- file.path(dir, "crlf.csv")
  fr_write_raw(lines, p_crlf, sep = "\r\n")

  t_lf <- as.data.frame(read_file(DTAFileCSV(filename = "lf.csv"), p_lf))
  t_crlf <- as.data.frame(read_file(DTAFileCSV(filename = "crlf.csv"), p_crlf))

  qa_step("CRLF and LF produce the identical table", t_lf, t_crlf)
})

test_that("OQ-FILE-028 | a data row with fewer fields than the header aborts the read | REQ-FILE-017", {
  dir <- qa_tempdir()
  p <- file.path(dir, "toofew.csv")
  fr_write_raw(c("A,B,C", "1,2,3", "4,5"), p)

  err_read <- tryCatch(read_file(DTAFileCSV(filename = "toofew.csv"), p), error = function(e) e)
  qa_check("the eager reader aborts rather than padding the short row", inherits(err_read, "condition"))

  # open_file() is meant to be schema-only and lazy, but a row-width problem
  # is caught at open time too -- pinned in REQ-FILE-017's notes as a
  # surprising edge, not smoothed away here.
  err_open <- tryCatch(open_file(DTAFileCSV(filename = "toofew.csv"), p), error = function(e) e)
  qa_check("the lazy opener aborts on the same file as well", inherits(err_open, "condition"))
})

test_that("OQ-FILE-029 | a data row with more fields than the header aborts the read | REQ-FILE-017", {
  dir <- qa_tempdir()
  p <- file.path(dir, "toomany.csv")
  fr_write_raw(c("A,B,C", "1,2,3", "6,7,8,9"), p)

  err <- tryCatch(read_file(DTAFileCSV(filename = "toomany.csv"), p), error = function(e) e)
  qa_check("the reader aborts rather than truncating the long row", inherits(err, "condition"))
})

test_that("OQ-FILE-030 | plain, quoted and padded headers all clean to the same column names | REQ-FILE-018", {
  dir <- qa_tempdir()
  frame <- data.frame(ID = "A1", VAL = "9", stringsAsFactors = FALSE)

  for (style in c("plain", "quoted", "padded")) {
    p <- file.path(dir, paste0("style_", style, ".csv"))
    qa_write_csv(frame, p, header_style = style)
    t <- read_file(DTAFileCSV(filename = basename(p)), p)
    qa_step(sprintf("header style '%s' cleans to the declared names", style), c("ID", "VAL"), names(t))
  }
})

test_that("OQ-FILE-031 | header names that collide only after cleaning abort with an identifying message | REQ-FILE-019", {
  dir <- qa_tempdir()
  p <- file.path(dir, "dup.csv")
  # "ID" and '" ID "' are two names to Arrow and one after trimming quotes and
  # surrounding whitespace -- the collision this case is about.
  fr_write_raw(c('ID," ID ",VAL', "1,2,3"), p)

  err <- tryCatch(read_file(DTAFileCSV(filename = "dup.csv"), p), error = function(e) e)

  qa_check("the read aborts rather than silently keeping one of the two columns", inherits(err, "condition"))
  qa_step(
    "the message identifies cleaning as the cause and names the colliding column",
    TRUE,
    grepl("left repeated column names", conditionMessage(err), fixed = TRUE) &&
      grepl("\"ID\"", conditionMessage(err), fixed = TRUE)
  )
})

test_that("OQ-FILE-032 | a path with spaces and non-ASCII characters reads correctly | REQ-FILE-020", {
  dir <- qa_tempdir()
  subdir <- file.path(dir, "sub dir éäß")
  dir.create(subdir)
  p <- file.path(subdir, "my file ü.csv")
  fr_write_raw(c("ID,VAL", "A001,10"), p)

  t <- read_file(DTAFileCSV(filename = basename(p)), p, specs = fr_text_specs)

  qa_step(
    "the file is read exactly as the same content would be under an all-ASCII path",
    data.frame(ID = "A001", VAL = "10", stringsAsFactors = FALSE),
    as.data.frame(t)
  )
})

test_that("OQ-FILE-033 | a file with 2000 columns reads correctly within seconds | REQ-FILE-021", {
  dir <- qa_tempdir()
  n_cols <- 2000L
  p <- file.path(dir, "wide.csv")
  fr_write_raw(c(
    paste0("C", seq_len(n_cols), collapse = ","),
    paste(seq_len(n_cols), collapse = ",")
  ), p)

  elapsed <- system.time(t <- read_file(DTAFileCSV(filename = "wide.csv"), p))[["elapsed"]]

  qa_step("every declared column is present", n_cols, ncol(t))
  qa_step("the one data row is read intact", as.character(seq_len(n_cols)), as.character(unlist(as.data.frame(t)[1, ])))
  qa_check(sprintf("2000 columns read within a generous bound (%.2fs elapsed, limit 30s)", elapsed), elapsed < 30)
})

test_that("OQ-FILE-034 | a single field of about 1 MB reads intact within seconds | REQ-FILE-021", {
  dir <- qa_tempdir()
  big_value <- paste(rep("x", 1e6), collapse = "")
  p <- file.path(dir, "bigfield.csv")
  fr_write_raw(c("A,B", paste0("1,\"", big_value, "\"")), p)

  elapsed <- system.time(t <- read_file(DTAFileCSV(filename = "bigfield.csv"), p))[["elapsed"]]

  qa_step("the megabyte-scale field is read back at its full length, unmangled", 1000000L, nchar(as.data.frame(t)$B[[1]]))
  qa_check(sprintf("a ~1MB field reads within a generous bound (%.2fs elapsed, limit 30s)", elapsed), elapsed < 30)
})

# ---- stream modes, handler routing, and lazy opening -----------------------------------

test_that("OQ-FILE-035 | stream = 'never', 'auto' and 'always' reach the identical verdict | REQ-FILE-022", {
  dir <- qa_tempdir()
  p <- file.path(dir, "verdict.csv")
  # AGE is unparseable in row 2, which gives the three modes something
  # non-trivial to agree on: an import error, not just a clean pass.
  fr_write_raw(c("ID,AGE", "A0000001,30", "A0000002,abc"), p)

  expected <- list(ok = FALSE, columnspec = 0L, import = 1L)

  for (mode in c("never", "auto", "always")) {
    actual <- fr_verdict_fields(fr_verdict(p, stream = mode))
    qa_step(sprintf("stream = '%s' reaches the expected verdict", mode), expected, actual)
  }
})

test_that("OQ-FILE-036 | handler_index routes a delivered file to the handler it names | REQ-FILE-023", {
  dir <- qa_tempdir()
  p_csv <- file.path(dir, "a.csv")
  fr_write_raw(c("ID,AGE", "A0000001,30"), p_csv)
  p_tsv <- file.path(dir, "b.tsv")
  fr_write_raw(c("ID\tAGE", "A0000002\t40"), p_tsv, sep = "\n")

  ds <- DTADataSetTabular(
    name = "multi", specs = fr_specs(),
    files = list(DTAFileCSV(filename = "a.csv"), DTAFileTSV(filename = "b.tsv"))
  )
  ds <- load_file(ds, file = p_csv, handler_index = 1, stream = "never")
  ds <- load_file(ds, file = p_tsv, handler_index = 2, stream = "never")

  qa_step("each delivered file lands under its own table name", c("a", "b"), sort(names(tables(ds))))
  # AGE comes back numeric, not text: load_file() (unlike a bare read_file())
  # coerces every declared column to its specified type, and fr_specs()
  # declares AGE as "SAS Num" -- this is real typing per the specification,
  # not the incidental inference a bare read without specs would apply.
  qa_step(
    "handler_index 1's table holds the CSV's content, AGE coerced to the declared numeric type",
    data.frame(ID = "A0000001", AGE = 30, stringsAsFactors = FALSE),
    as.data.frame(tables(ds)[["a"]])
  )
  qa_step(
    "handler_index 2's table holds the TSV's content, coerced the same way",
    data.frame(ID = "A0000002", AGE = 40, stringsAsFactors = FALSE),
    as.data.frame(tables(ds)[["b"]])
  )
})

test_that("OQ-FILE-037 | open_file returns a lazy dataset, never a materialised table | REQ-FILE-024", {
  dir <- qa_tempdir()
  p <- file.path(dir, "lazy.csv")
  fr_write_raw(c("A,B,C", "1,2,3"), p)

  ds <- open_file(DTAFileCSV(filename = "lazy.csv"), p)
  t <- read_file(DTAFileCSV(filename = "lazy.csv"), p)

  qa_step("the column names are known from the header alone", c("A", "B", "C"), names(ds))
  qa_check("open_file() returns a lazy Dataset object", inherits(ds, "Dataset"))
  qa_check("...unlike read_file(), which returns an already-materialised table", !inherits(t, "Dataset"))
})
