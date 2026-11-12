# File handler classes and the properties they apply at read time.
# REQ-FILE-001 .. REQ-FILE-010.
#
# Every case here calls a handler's read_file()/matches_filename() directly,
# with no DTADataSetTabular and no specification in between -- the point is to
# pin what a single handler property does to a file on its own, before the
# validation engine (covered by area VAL) ever sees the result.

# ---- fixtures ---------------------------------------------------------------

# Writes `lines` as-is, in binary mode so no platform line-ending translation
# is inserted between what this file asks for and what lands on disk -- several
# cases below depend on the exact bytes written (a BOM, a CRLF, a bare blank
# line).
fh_write_raw <- function(lines, path, sep = "\n") {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeChar(paste0(paste(lines, collapse = sep), sep), con, eos = NULL, useBytes = TRUE)
  path
}

fh_path <- function(dir, name) file.path(dir, name)

# Passed as `specs` wherever a case compares exact values rather than just
# names or row counts. Supplying ANY DTAColumnSpecCollection -- even, as here,
# one declaring no columns at all -- pins every column of the read result to
# text (see dta_delim_reader_plan()): without it, a value that looks numeric
# ("10", "007") is inferred as a number and can lose information (a leading
# zero) that these cases are not about and must not be tripped up by. What
# specs does to a column it DOES declare (real typing, real coercion) is area
# VAL and TYPE territory, not this one.
fh_text_specs <- DTAColumnSpecCollection(columns = list())

# ---- delimiter ---------------------------------------------------------------

test_that("OQ-FILE-001 | DTAFileCSV reads a comma-separated file | REQ-FILE-001", {
  dir <- qa_tempdir()
  p <- fh_path(dir, "a.csv")
  fh_write_raw(c("ID,VAL", "A001,10", "A002,20"), p)

  t <- read_file(DTAFileCSV(filename = "a.csv"), p, specs = fh_text_specs)

  qa_step("column names", c("ID", "VAL"), names(t))
  qa_step("values", data.frame(ID = c("A001", "A002"), VAL = c("10", "20"), stringsAsFactors = FALSE), as.data.frame(t))
})

test_that("OQ-FILE-002 | DTAFileTSV reads a tab-separated file | REQ-FILE-001", {
  dir <- qa_tempdir()
  p <- fh_path(dir, "a.tsv")
  fh_write_raw(c("ID\tVAL", "A001\t10", "A002\t20"), p)

  t <- read_file(DTAFileTSV(filename = "a.tsv"), p, specs = fh_text_specs)

  qa_step("column names", c("ID", "VAL"), names(t))
  qa_step("values", data.frame(ID = c("A001", "A002"), VAL = c("10", "20"), stringsAsFactors = FALSE), as.data.frame(t))
})

test_that("OQ-FILE-003 | DTAFileDelim honours a caller-supplied single-character separator | REQ-FILE-001", {
  dir <- qa_tempdir()

  # Two separators in one case: the behaviour under test is "the declared
  # character is used, whatever it is", which one separator alone cannot
  # distinguish from "semicolons happen to work".
  sep_files <- c(";" = "semi.txt", "|" = "pipe.txt")
  for (sep in names(sep_files)) {
    p <- fh_path(dir, sep_files[[sep]])
    fh_write_raw(c(paste("ID", "VAL", sep = sep), paste("A001", "10", sep = sep)), p)

    t <- read_file(DTAFileDelim(filename = basename(p), sep = sep), p, specs = fh_text_specs)

    qa_step(sprintf("separator '%s': column names", sep), c("ID", "VAL"), names(t))
    qa_step(
      sprintf("separator '%s': the one data row", sep),
      data.frame(ID = "A001", VAL = "10", stringsAsFactors = FALSE),
      as.data.frame(t)
    )
  }
})

# ---- quote --------------------------------------------------------------------

test_that("OQ-FILE-004 | the quote character protects an embedded delimiter and an escaped quote | REQ-FILE-002", {
  dir <- qa_tempdir()
  p <- fh_path(dir, "quoted.csv")
  fh_write_raw(c(
    "A,B",
    '"has, comma",plain',
    '"has ""quote""",x2'
  ), p)

  t <- read_file(DTAFileCSV(filename = "quoted.csv"), p)

  qa_step(
    "a comma inside quotes does not split the field, and a doubled quote is one literal quote",
    data.frame(A = c("has, comma", 'has "quote"'), B = c("plain", "x2"), stringsAsFactors = FALSE),
    as.data.frame(t)
  )
})

# ---- header ---------------------------------------------------------------------

test_that("OQ-FILE-005 | has_header selects whether the first line is names or data | REQ-FILE-003", {
  dir <- qa_tempdir()

  p_h <- fh_path(dir, "header.csv")
  fh_write_raw(c("ID,VAL", "A001,10"), p_h)
  t_h <- read_file(DTAFileCSV(filename = "header.csv", has_header = TRUE), p_h)
  qa_step("has_header = TRUE: the first line names the columns", c("ID", "VAL"), names(t_h))
  qa_step("has_header = TRUE: one data row remains", 1L, nrow(t_h))

  p_nh <- fh_path(dir, "noheader.csv")
  fh_write_raw(c("A001,10", "A002,20"), p_nh)
  t_nh <- read_file(DTAFileCSV(filename = "noheader.csv", has_header = FALSE), p_nh)
  qa_step("has_header = FALSE: column names are generated, not read from the file", c("f0", "f1"), names(t_nh))
  qa_step("has_header = FALSE: the first line is a data row, so both rows remain", 2L, nrow(t_nh))
})

# ---- missing values ---------------------------------------------------------------

test_that("OQ-FILE-006 | declared missing_values add to, rather than replace, the empty string | REQ-FILE-004", {
  dir <- qa_tempdir()
  p <- fh_path(dir, "na_custom.csv")
  fh_write_raw(c("ID,VAL", "A001,.", "A002,MISSING", "A003,", "A004,5"), p)

  t <- read_file(DTAFileCSV(filename = "na_custom.csv", missing_values = c(".", "MISSING")), p, specs = fh_text_specs)

  qa_step(
    "both declared tokens and the always-missing empty string become NA; a real value does not",
    c(NA, NA, NA, "5"),
    as.data.frame(t)$VAL
  )
})

test_that("OQ-FILE-007 | an undeclared literal 'NA' token is missing by the reader's own default | REQ-FILE-004", {
  dir <- qa_tempdir()
  p <- fh_path(dir, "na_default.csv")
  # missing_values is left at its default (""), which declares nothing extra
  # and keeps the reader's own default missing set.
  fh_write_raw(c("ID,NOTE", "A001,present", "A002,NA", "A003,"), p)

  t <- read_file(DTAFileCSV(filename = "na_default.csv"), p)

  qa_step(
    "a literal NA and an empty cell are both missing with no missing_values declared",
    c("present", NA, NA),
    as.data.frame(t)$NOTE
  )
})

test_that("OQ-FILE-053 | a declared missing token and the reader's own literal 'NA' default are both missing in the same file | REQ-FILE-004", {
  dir <- qa_tempdir()
  p <- fh_path(dir, "na_combined.csv")
  # Neither OQ-FILE-006 nor OQ-FILE-007 puts a declared token and a literal
  # "NA" in the same file: 006 declares tokens but never writes "NA" itself,
  # and 007 writes "NA" but declares nothing. This pins that a declared token
  # ADDS to the default set rather than replacing it, in one file where both
  # kinds of missing marker are present together.
  fh_write_raw(c("ID,VAL", "A001,.", "A002,NA", "A003,present"), p)

  t <- read_file(DTAFileCSV(filename = "na_combined.csv", missing_values = "."), p, specs = fh_text_specs)

  qa_step(
    "the declared token and the reader's own 'NA' default are both missing; a real value is not",
    c(NA, NA, "present"),
    as.data.frame(t)$VAL
  )
})

# ---- encoding ---------------------------------------------------------------------

test_that("OQ-FILE-008 | UTF-8, a UTF-8 byte-order mark, and transcoded latin1 all decode to the same text | REQ-FILE-005", {
  dir <- qa_tempdir()

  p_utf8 <- fh_path(dir, "utf8.csv")
  fh_write_raw(c("ID,NAME", "A001,Müller"), p_utf8)
  t_utf8 <- read_file(DTAFileCSV(filename = "utf8.csv", encoding = "UTF-8"), p_utf8)
  qa_step("a plain UTF-8 file needs no transcoding", "Müller", as.data.frame(t_utf8)$NAME[[1]])

  p_bom <- fh_path(dir, "bom.csv")
  con <- file(p_bom, "wb")
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  writeChar("ID,NAME\r\nA001,Alice\r\n", con, eos = NULL, useBytes = TRUE)
  close(con)
  t_bom <- read_file(DTAFileCSV(filename = "bom.csv"), p_bom)
  qa_step("a leading byte-order mark is not part of the first column's name", "ID", names(t_bom)[[1]])
  qa_step("nor of the first value", "Alice", as.data.frame(t_bom)$NAME[[1]])

  p_l1 <- fh_path(dir, "latin1.csv")
  con <- file(p_l1, "wb")
  writeBin(charToRaw("ID,NAME\n"), con)
  # "M" + latin1 0xFC ("u"-umlaut) + "ller", written as raw bytes so the
  # source encoding is exactly latin1 regardless of this session's own locale.
  writeBin(c(charToRaw("A001,M"), as.raw(0xFC), charToRaw("ller\n")), con)
  close(con)
  t_l1 <- read_file(DTAFileCSV(filename = "latin1.csv", encoding = "latin1"), p_l1)
  qa_step(
    "a latin1 byte is transcoded to the correct UTF-8 representation",
    "Müller",
    as.data.frame(t_l1)$NAME[[1]]
  )
})

# ---- filename / pattern matching ---------------------------------------------------

test_that("OQ-FILE-009 | matches_filename is an exact, case-sensitive comparison when pattern is FALSE | REQ-FILE-006", {
  h <- DTAFileCSV(filename = "data.csv")

  qa_step("the declared name matches itself", TRUE, isTRUE(matches_filename(h, "data.csv")))
  qa_step("a name that merely contains the declared name does not match", FALSE, isTRUE(matches_filename(h, "xdata.csv")))
  qa_step("a name the declared name merely starts does not match", FALSE, isTRUE(matches_filename(h, "data.csv.bak")))
  qa_step("the comparison is case-sensitive", FALSE, isTRUE(matches_filename(h, "DATA.CSV")))
})

test_that("OQ-FILE-010 | matches_filename is an unanchored regular-expression search when pattern is TRUE | REQ-FILE-006", {
  # The declared string is deliberately not anchored, to pin that the package
  # does not anchor it either: it is a SEARCH, not a full match.
  h <- DTAFileCSV(filename = "data", pattern = TRUE)

  qa_step("the pattern matches a name it appears in as a prefix", TRUE, isTRUE(matches_filename(h, "data.csv")))
  qa_step("...or in the middle", TRUE, isTRUE(matches_filename(h, "mydata2.csv")))
  qa_step("...or with no separator at all around it", TRUE, isTRUE(matches_filename(h, "xdatacsv")))
  qa_step("a name that does not contain the pattern at all does not match", FALSE, isTRUE(matches_filename(h, "nomatchhere.csv")))
})

# ---- file counting ------------------------------------------------------------------

test_that("OQ-FILE-011 | min_number_of_files and max_number_of_files sum across a dataset's handlers | REQ-FILE-007", {
  f_exact <- DTAFileCSV(filename = "x.csv", number_of_files = 1)
  qa_step("a handler declaring number_of_files = 1 has min = max = 1", list(min = 1, max = 1), list(min = min_number_of_files(f_exact), max = max_number_of_files(f_exact)))

  f_range <- DTAFileAny(filename = "^scan_.*", pattern = TRUE, min_number_of_files = 2, max_number_of_files = 4)
  qa_step("a handler's own bounds are returned unchanged", list(min = 2, max = 4), list(min = min_number_of_files(f_range), max = max_number_of_files(f_range)))

  ds <- DTADataSetFile(id = "d", files = list(f_exact, f_range))
  qa_step(
    "a dataset's bounds are the sum of every handler's own bounds (1+2, 1+4)",
    list(min = 3, max = 5),
    list(min = min_number_of_files(ds), max = max_number_of_files(ds))
  )
})

# ---- DTAFileAny: non-tabular deliverables --------------------------------------------

test_that("OQ-FILE-012 | DTAFileAny restricts endings by suffix, including multi-part endings | REQ-FILE-008", {
  # A single, ordinary ending. The name pattern is satisfied by every
  # candidate here, so what varies between them is only the ending -- which
  # isolates the extension check from the name/pattern check.
  ha <- DTAFileAny(filename = "^report", pattern = TRUE, extensions = "pdf")
  qa_step("the declared ending is accepted", TRUE, isTRUE(matches_filename(ha, "report.pdf")))
  qa_step("...and so is the same ending under a compressed delivery", TRUE, isTRUE(matches_filename(ha, "report.pdf.gz")))
  qa_step("an ending not on the list is refused even though the name matches", FALSE, isTRUE(matches_filename(ha, "report.docx")))

  # Extensions are normalised to lower-case with no leading dot at
  # construction, which is what makes the comparison case-insensitive.
  ha_norm <- DTAFileAny(filename = "x", extensions = c(".PDF", "Zip"))
  qa_step("extensions are normalised at construction: lower-case, no leading dot", c("pdf", "zip"), ha_norm@extensions)

  # A multi-part ending: tools::file_ext() would see only "gz" for either of
  # these, which is exactly the shortcut this package's suffix match avoids.
  ha_multi <- DTAFileAny(filename = "^scan_.*", pattern = TRUE, extensions = c("tar.gz", "nii.gz"))
  qa_step("a declared multi-part ending matches the whole suffix", TRUE, isTRUE(matches_filename(ha_multi, "scan_1.tar.gz")))
  qa_step("...and a different declared multi-part ending, distinctly", TRUE, isTRUE(matches_filename(ha_multi, "scan_1.nii.gz")))
  qa_step("a bare compression suffix alone does not satisfy a multi-part ending", FALSE, isTRUE(matches_filename(ha_multi, "scan_1.gz")))
  qa_step("nor does the first part of the ending on its own", FALSE, isTRUE(matches_filename(ha_multi, "scan_1.tar")))
})

# ---- compression is transparent to filename matching ---------------------------------

test_that("OQ-FILE-013 | a declared filename also matches its gzip-compressed delivery | REQ-FILE-009", {
  h <- DTAFileCSV(filename = "data.csv")

  qa_step("the gzip-compressed name satisfies a declaration that says nothing about compression", TRUE, isTRUE(matches_filename(h, "data.csv.gz")))
  qa_step("a compression this package does not advertise does not get the same treatment", FALSE, isTRUE(matches_filename(h, "data.csv.bz2")))
})

# ---- per-pattern missing reporting -----------------------------------------------

test_that("OQ-FILE-054 | check() reports every declared pattern with no delivered match, not just the whole handler | REQ-FILE-031", {
  dir <- qa_tempdir()
  path_a <- fh_path(dir, "a_2026.pdf")
  fh_write_raw("content", path_a)

  # One handler declaring two patterns. Only the "a_" pattern is delivered;
  # the "b_" pattern never arrives.
  h <- DTAFileAny(filename = c("^a_.*\\.pdf$", "^b_.*\\.pdf$"), pattern = TRUE, number_of_files = 2)
  ds <- DTADataSetFile(id = "d", files = list(h))
  ds <- load_file(ds, file = path_a, handler_index = 1)
  ds <- check(ds, quiet = TRUE, persist = FALSE)

  status <- validation_status(ds)
  qa_step("both the delivered file and the still-missing pattern are reported as their own target", 2L, nrow(status))
  qa_step(
    # The delivered target is keyed by the file that satisfied it; only the
    # UNMATCHED pattern is keyed by its own declared name -- that name is
    # exactly what a delivery report has no other way to mention.
    "the delivered file is ok and the pattern with no match is not, each under its own key",
    list(delivered = TRUE, undelivered = FALSE),
    list(
      delivered = isTRUE(status$ok[status$table == "a_2026.pdf"]),
      undelivered = isTRUE(status$ok[status$table == "^b_.*\\.pdf$"])
    )
  )

  msgs <- messages(ds, as_tibble = FALSE)
  qa_step("exactly one message is recorded, for the pattern that never arrived", 1L, nrow(msgs))
  qa_step("its text names that pattern", TRUE, grepl("^b_.*\\.pdf$", msgs$message[[1]], fixed = TRUE))
})

# ---- file counts of a file dataset ------------------------------------------------------

# The check() status of a file dataset: one row per target (a delivered file,
# or an entry whose count is wrong or that was not delivered).
fh_status <- function(ds) validation_status(check(ds, quiet = TRUE, persist = FALSE))

# A file dataset `d` in a DTA, with the files delivered by load_delivery(), and
# its status. The files are written under qa_tempdir() by the caller.
fh_delivered_status <- function(ds, files) {
  r <- cont_run(cont_of(ds), files, exdir = cont_exdir("OQ-FILE-065"))
  stopifnot(is.null(r$error))
  fh_status(DTAtools::datasets(r$result, "d"))
}

test_that("OQ-FILE-065 | each entry of a file dataset counts the files routed to it, and an entry with a minimum of zero is optional | REQ-FILE-041", {
  pdf <- function(...) DTAFileAny("[.]pdf$", pattern = TRUE, ...)

  # A literal entry beside a pattern entry. The pattern also matches x.pdf, but
  # x.pdf is named by the literal entry, so it is that entry's file: the pattern
  # entry (1 to 1 files) counts y.pdf alone, then y.pdf and z.pdf.
  ds <- DTADataSetFile(id = "d", files = list(
    DTAFileAny("x.pdf", number_of_files = 1), pdf(min_number_of_files = 1, max_number_of_files = 1)
  ))
  s1 <- fh_delivered_status(ds, c(cont_write("x.pdf", "a"), cont_write("y.pdf", "b")))
  qa_step("x.pdf (literal) and y.pdf (pattern, 1 of 1) are each fine", c(TRUE, TRUE), s1$ok)
  s2 <- fh_delivered_status(ds, c(cont_write("x.pdf", "a"), cont_write("y.pdf", "b"), cont_write("z.pdf", "c")))
  qa_step("with z.pdf as well the pattern entry holds 2 files against a maximum of 1: it alone fails", "[.]pdf$", s2$table[!s2$ok])

  # Two entries that differ only in their container each count their own files:
  # one pdf in a.zip and one in b.zip, one file each.
  per_container <- DTADataSetFile(id = "d", files = list(
    pdf(container = "a.zip", number_of_files = 1), pdf(container = "b.zip", number_of_files = 1)
  ))
  archives <- c(cont_zip(list(p.pdf = "a"), name = "a.zip"), cont_zip(list(q.pdf = "b"), name = "b.zip"))
  spc <- fh_delivered_status(per_container, archives)
  qa_step("each archive's entry holds its one file, so both files are fine and nothing else is reported", c("p.pdf", "q.pdf"), spc$table[spc$ok])
  qa_step("with no failure", 0L, sum(!spc$ok))

  # A minimum of zero makes the entry optional; a maximum still applies.
  optional <- DTADataSetFile(id = "d", files = list(pdf(min_number_of_files = 0, max_number_of_files = 1)))
  qa_step("an entry with a minimum of 0 and nothing delivered reports nothing", 0L, nrow(fh_status(optional)))
  required <- DTADataSetFile(id = "d", files = list(pdf(min_number_of_files = 1, max_number_of_files = 1)))
  sr <- fh_status(required)
  qa_step("while the same entry with a minimum of 1 reports the missing file", list("[.]pdf$", FALSE), list(sr$table, sr$ok))
  so <- fh_delivered_status(optional, c(cont_write("p.pdf", "a"), cont_write("q.pdf", "b")))
  qa_step("and an optional entry given two files against a maximum of 1 fails", "[.]pdf$", so$table[!so$ok])
})

test_that("OQ-FILE-066 | a file count bound must be a single non-negative whole number, with the minimum no greater than the maximum | REQ-FILE-042", {
  csv <- function(...) DTAFileCSV(filename = "x_.*[.]csv$", pattern = TRUE, ...)
  refused <- function(label, expr) {
    e <- cont_catch(expr)
    qa_step(sprintf("%s is refused as an invalid file count bound", label), TRUE, cont_is(e, "dta_file_count_bound_invalid"))
  }
  refused("a minimum of 3 above a maximum of 2", csv(min_number_of_files = 3, max_number_of_files = 2))
  refused("a minimum of -1", csv(min_number_of_files = -1))
  refused("a maximum of -1", csv(max_number_of_files = -1))
  refused("a minimum of 1.5", csv(min_number_of_files = 1.5))
  refused("a maximum of 1.5", csv(max_number_of_files = 1.5))
  refused("a minimum of NA", csv(min_number_of_files = NA_real_))
  refused("a maximum of NA", csv(max_number_of_files = NA_real_))
  refused("a minimum of Inf", csv(min_number_of_files = Inf))
  refused("a maximum of Inf", csv(max_number_of_files = Inf))
  refused("a minimum of two numbers", csv(min_number_of_files = c(1, 2)))
  refused("number_of_files of -1", csv(number_of_files = -1))
  refused("number_of_files of 1.5", csv(number_of_files = 1.5))
  refused("number_of_files of NA", csv(number_of_files = NA_real_))
  refused("number_of_files of Inf", csv(number_of_files = Inf))
  h <- csv(max_number_of_files = 2)
  refused("a minimum of 5 assigned to a handler with a maximum of 2", {
    h@min_number_of_files <- 5
  })

  # Valid forms are accepted with the bounds as given (NULL for a bound left out).
  accepted <- function(label, h, min, max) {
    qa_step(sprintf("%s is accepted, with the bounds as given", label), list(min, max), list(h@min_number_of_files, h@max_number_of_files))
  }
  accepted("a minimum of 0", csv(min_number_of_files = 0), 0, NULL)
  accepted("0 to 0", csv(min_number_of_files = 0, max_number_of_files = 0), 0, 0)
  accepted("number_of_files of 0", csv(number_of_files = 0), 0, 0)
  accepted("2 to 2", csv(min_number_of_files = 2, max_number_of_files = 2), 2, 2)
  accepted("a minimum of 2 alone", csv(min_number_of_files = 2), 2, NULL)
  accepted("a maximum of 4 alone", csv(max_number_of_files = 4), NULL, 4)
  accepted("number_of_files of 3", csv(number_of_files = 3), 3, 3)
})
