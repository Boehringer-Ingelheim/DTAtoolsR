# Reader layer for matrix datasets: head records, verbatim header tokenising and
# caller-supplied column names. Fixtures are written as bytes so that line ends,
# BOMs and encodings are exactly what each test says.

mx_file <- function(x, name = "m.csv", env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  path <- file.path(dir, name)
  writeBin(if (is.raw(x)) x else charToRaw(x), path)
  path
}

mx_rec <- function(h) vapply(h$records, rawToChar, "")

# ---- dta_head_records --------------------------------------------------------

test_that("dta_head_records drops a BOM and returns raw records without terminators", {
  path <- mx_file(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw("a,b\n1,2\n3,4\n")))
  h <- dta_head_records(path, 2L)
  expect_identical(mx_rec(h), c("a,b", "1,2"))
  expect_identical(h$blank, 0L)
  expect_identical(h$multiline, c(FALSE, FALSE))
})

test_that("dta_head_records skips LF, CRLF and lone-CR blank lines and counts the leading ones", {
  h <- dta_head_records(mx_file("\n\r\n\ra,b\n\n\r\n1,2\r\n"), 2L)
  expect_identical(mx_rec(h), c("a,b", "1,2"))
  expect_identical(h$blank, 3L)
})

test_that("dta_head_records ends records at a lone CR", {
  h <- dta_head_records(mx_file("a,b\r1,2\r3,4\r"), 2L)
  expect_identical(mx_rec(h), c("a,b", "1,2"))
})

test_that("dta_head_records treats a doubled quote as an escape and a mid-field quote as literal", {
  h <- dta_head_records(mx_file("h\n\"a\"\"\nb\",5\" pipe\n1,2\n"), 3L, newlines_in_values = TRUE)
  expect_identical(mx_rec(h), c("h", "\"a\"\"\nb\",5\" pipe", "1,2"))
  expect_identical(h$multiline, c(FALSE, TRUE, FALSE))
})

test_that("dta_head_records keeps a quoted line break inside a later record when allowed", {
  h <- dta_head_records(mx_file("h\n\"x\ny\",b\n1,2\n"), 3L, newlines_in_values = TRUE)
  expect_identical(mx_rec(h), c("h", "\"x\ny\",b", "1,2"))
  expect_identical(h$multiline, c(FALSE, TRUE, FALSE))
})

test_that("dta_head_records cuts the first record at a quoted line break", {
  h <- dta_head_records(mx_file("\"x\ny\",b\n1,2\n"), 2L, newlines_in_values = TRUE)
  expect_identical(mx_rec(h), "\"x")
  expect_identical(h$multiline, TRUE)
})

test_that("dta_head_records copes with a header-only file and fewer records than asked", {
  h <- dta_head_records(mx_file("a,b\n"), 2L)
  expect_identical(mx_rec(h), "a,b")
  expect_identical(dta_head_records(mx_file("a,b"), 2L)$multiline, FALSE)
  expect_length(dta_head_records(mx_file("\n\n"), 2L)$records, 0L)
  expect_identical(dta_head_records(mx_file("\"a,b\n"), 2L)$multiline, TRUE)
})

test_that("dta_head_records reads gzip files", {
  path <- file.path(withr::local_tempdir(), "m.csv.gz")
  con <- gzfile(path, "wb")
  writeBin(charToRaw("a,b\n1,2\n"), con)
  close(con)
  expect_identical(mx_rec(dta_head_records(path, 2L)), c("a,b", "1,2"))
})

test_that("dta_head_records with quote = \"\" ignores quotes", {
  h <- dta_head_records(mx_file("\"a\n\"b\n"), 2L, quote = "")
  expect_identical(mx_rec(h), c("\"a", "\"b"))
  expect_identical(h$multiline, c(FALSE, FALSE))
})

test_that("a quoted header over 1 MiB is not a quoted line break (tabular too)", {
  labels <- paste0("\"label number ", seq_len(60000L), "\"", collapse = ",")
  path <- mx_file(paste0(labels, "\n", strrep("1,", 59999L), "1\n"))
  expect_gt(file.size(path), 1048576)
  expect_false(dta_header_layout(path)$multiline)
  handler <- DTAFileCSV(filename = "m.csv")
  plan <- dta_delim_reader_plan(path, handler = handler)
  expect_length(plan$column_names, 60000L)
  expect_identical(plan$column_names[[60000L]], "label number 60000")
})

# ---- dta_matrix_read_head ----------------------------------------------------

test_that("dta_matrix_read_head returns header labels verbatim", {
  handler <- DTAFileCSV(filename = "m.csv", missing_values = ".")
  path <- mx_file("007,1e3,NA,\"\",.,S 1,a{b},dup,dup,\"say \"\"hi\"\"\"\n1,2,3,4,5,6,7,8,9,10,11\n")
  h <- dta_matrix_read_head(handler, path)
  expect_identical(
    h$fields,
    # Literal on purpose: `"say ""hi"""` loses its closing quote to the cleaning rule.
    c("007", "1e3", "NA", "", ".", "S 1", "a{b}", "dup", "dup", "say \"hi")
  )
  expect_identical(h$n_header, 10L)
  expect_identical(h$n_first_row, 11L)
  expect_null(h$hint)
})

test_that("dta_matrix_read_head reads a latin1 label through the handler's encoding", {
  handler <- DTAFileCSV(filename = "m.csv", encoding = "latin1")
  path <- mx_file(c(charToRaw("a,Pr"), as.raw(0xfc), charToRaw("f\n1,2\n")))
  h <- dta_matrix_read_head(handler, path)
  expect_identical(h$fields, c("a", "Prüf"))
  expect_identical(Encoding(h$fields[[2L]]), "UTF-8")
})

test_that("dta_matrix_read_head drops a BOM and reports field counts", {
  handler <- DTAFileCSV(filename = "m.csv")
  bom <- as.raw(c(0xef, 0xbb, 0xbf))
  h <- dta_matrix_read_head(handler, mx_file(c(bom, charToRaw("a,b\n1,2,3\n"))))
  expect_identical(h$fields, c("a", "b"))
  expect_identical(h$n_first_row, 3L)

  short <- dta_matrix_read_head(handler, mx_file("a,b\n1,2,3\n"))
  expect_identical(short$n_first_row, short$n_header + 1L)

  only <- dta_matrix_read_head(handler, mx_file("a,b\n"))
  expect_identical(only$n_first_row, NA_integer_)
})

test_that("dta_matrix_read_head hints at a preamble and at a wrong delimiter", {
  handler <- DTAFileCSV(filename = "m.csv")
  expect_identical(dta_matrix_read_head(handler, mx_file("# Program: x\na,b\n1,2\n"))$hint, "preamble")
  semi <- dta_matrix_read_head(handler, mx_file("a;b;c\n1;2;3\n"))
  expect_identical(semi$hint, "delimiter")
  expect_identical(semi$n_header, 1L)
})

test_that("dta_matrix_read_head refuses a header with a quoted line break", {
  handler <- DTAFileCSV(filename = "m.csv")
  expect_error(
    dta_matrix_read_head(handler, mx_file("\"a\nb\",c\n1,2\n")),
    "quoted line break",
    fixed = TRUE
  )
})

test_that("dta_matrix_read_head tokenises a wide header", {
  handler <- DTAFileCSV(filename = "m.csv")
  labels <- paste0("c", seq_len(20000L))
  h <- dta_matrix_read_head(handler, mx_file(paste0(paste(labels, collapse = ","), "\n1\n")))
  expect_identical(h$n_header, 20000L)
  expect_identical(h$n_first_row, 1L)
})

# ---- column_names threading ---------------------------------------------------

test_that("column_names replace the header verbatim on both readers, CSV and TSV", {
  cases <- list(
    list(handler = DTAFileCSV(filename = "m.csv"), name = "m.csv", sep = ","),
    list(handler = DTAFileTSV(filename = "m.tsv"), name = "m.tsv", sep = "\t")
  )
  wanted <- c("x", "y", " \"\" z", "w")
  for (case in cases) {
    sep <- case$sep
    # duplicate labels and two blank fields: a header Arrow itself would refuse
    path <- mx_file(
      paste0(
        paste(c("a", "a", "", ""), collapse = sep), "\n",
        paste(1:4, collapse = sep), "\n",
        paste(5:8, collapse = sep), "\n"
      ),
      case$name
    )
    specs <- DTAColumnSpecCollection(columns = list(DTAColumnSpec(id = "x", type = "SAS Char", format = "SAS $8.")))

    eager <- as.data.frame(read_file(case$handler, path, specs = specs, column_names = wanted))
    expect_identical(names(eager), wanted, info = case$name)
    expect_identical(eager[[1]], c("1", "5"), info = case$name)
    expect_identical(eager[[4]], c("4", "8"), info = case$name)

    lazy <- dplyr::collect(open_file(case$handler, path, specs = specs, column_names = wanted))
    expect_identical(names(lazy), wanted, info = case$name)
    expect_identical(lazy[[2]], c("2", "6"), info = case$name)
  }
})

test_that("column_names skip leading blank lines and are used without further cleaning", {
  path <- mx_file("\n\"\"\"\" a,b\n1,2\n")
  handler <- DTAFileCSV(filename = "m.csv")
  wanted <- c("\"\" a", "b")
  eager <- as.data.frame(read_file(handler, path, column_names = wanted))
  expect_identical(names(eager), wanted)
  expect_identical(nrow(eager), 1L)
})

test_that("invalid column_names are refused by the plan", {
  path <- mx_file("a,b\n1,2\n")
  handler <- DTAFileCSV(filename = "m.csv")
  for (bad in list(c("a", "a"), c("a", NA), character(0), 1:2)) {
    expect_error(
      dta_delim_reader_plan(path, handler = handler, column_names = bad),
      class = "dta_column_names_invalid"
    )
  }
  expect_error(
    dta_delim_reader_plan(path, handler = handler, has_header = FALSE, column_names = c("a", "b")),
    class = "dta_column_names_invalid"
  )
})

test_that("SAS and transport readers refuse column_names instead of ignoring them", {
  for (cls in list(DTAFileSAS7BDAT, DTAFileXPT)) {
    expect_error(
      read_file_execution(cls(filename = "x"), file = "unused", column_names = "a"),
      class = "dta_column_names_unsupported"
    )
  }
})

test_that("a transcoded lazy dataset is refreshed with the same column_names", {
  handler <- DTAFileCSV(filename = "m.csv", encoding = "latin1")
  path <- mx_file(c(charToRaw("Pr"), as.raw(0xfc), charToRaw("f,x\n1,2\n")))
  wanted <- c("n1", "n2")
  ds <- open_file(handler, path, column_names = wanted)
  expect_identical(names(ds), wanted)

  unlink(ds$files)
  refreshed <- dta_refresh_transcoded_dataset(ds)
  expect_identical(names(refreshed), wanted)
  expect_identical(nrow(dplyr::collect(refreshed)), 1L)
})

# ---- bounded, linear head scan ------------------------------------------------

test_that("an unterminated header quote is cut at its first line end, not read to the end", {
  # Bigger than one read chunk, with a closing quote only at the very end: a
  # scan to the end of the file would return a multi-MiB record.
  rows <- strrep("1,2\n", 800000L)
  path <- mx_file(paste0("\"id,name\n", rows, "\"\n"))
  expect_gt(file.size(path), 3 * 1048576)
  elapsed <- system.time(h <- dta_head_records(path, 2L))[["elapsed"]]
  expect_identical(mx_rec(h), "\"id,name")
  expect_identical(h$multiline, TRUE)
  expect_lt(elapsed, 5)
  expect_error(
    dta_matrix_read_head(DTAFileCSV(filename = "m.csv"), path),
    "quoted line break",
    fixed = TRUE
  )
})

test_that("a second record cut at a quoted line break has an unknown width", {
  path <- mx_file("a,b\n1,\"x\ny\"\n")
  h <- dta_head_records(path, 2L)
  expect_identical(mx_rec(h), c("a,b", "1,\"x"))
  expect_identical(h$multiline, c(FALSE, TRUE))
  expect_identical(dta_matrix_read_head(DTAFileCSV(filename = "m.csv"), path)$n_first_row, NA_integer_)

  # Where values may span lines the record is read whole and counted.
  allowed <- DTAFileCSV(filename = "m.csv", newlines_in_values = TRUE)
  h2 <- dta_head_records(path, 2L, newlines_in_values = TRUE)
  expect_identical(mx_rec(h2), c("a,b", "1,\"x\ny\""))
  expect_identical(h2$multiline, c(FALSE, TRUE))
  expect_identical(dta_matrix_read_head(allowed, path)$n_first_row, 2L)
})

test_that("a later record still open after max_scan bytes is given up with multiline NA", {
  path <- mx_file(paste0("a,b\n1,\"", strrep("x\n", 500L)))
  h <- dta_head_records(path, 2L, newlines_in_values = TRUE, max_scan = 100)
  expect_length(h$records, 2L)
  expect_identical(h$multiline, c(FALSE, NA))
})

test_that("chunk edges do not change how records are cut, plain or gzip", {
  M <- 1048576L
  r <- charToRaw
  env <- environment()
  put <- function(bytes, gz) {
    if (!gz) {
      return(mx_file(bytes, env = env))
    }
    path <- file.path(withr::local_tempdir(.local_envir = env), "m.csv.gz")
    con <- gzfile(path, "wb")
    writeBin(bytes, con)
    close(con)
    path
  }
  for (gz in c(FALSE, TRUE)) {
    # A doubled quote split by the edge, then a quoted line break.
    b <- c(r("\""), rep(r("x"), M - 2L), r("\"\""), r("\nmore\",b\n1,2\n"))
    expect_identical(b[M], r("\""))
    expect_identical(b[M + 1L], r("\""))
    h <- dta_head_records(put(b, gz), 2L, newlines_in_values = TRUE)
    expect_identical(h$multiline, TRUE)
    expect_length(h$records[[1L]], M + 1L)

    # CRLF pairs of leading blank lines split by the edge.
    b <- c(r("\n"), rep(r("\r\n"), M / 2L), r("a,b\n1,2\n"))
    expect_identical(b[M], as.raw(0x0d))
    h <- dta_head_records(put(b, gz), 2L)
    expect_identical(h$blank, as.integer(1L + M / 2L))
    expect_identical(mx_rec(h), c("a,b", "1,2"))

    # The CRLF ending the header split by the edge.
    b <- c(rep(r("a"), M - 1L), r("\r\n1,2\r\n"))
    expect_identical(b[M], as.raw(0x0d))
    h <- dta_head_records(put(b, gz), 2L)
    expect_length(h$records[[1L]], M - 1L)
    expect_identical(mx_rec(h)[[2L]], "1,2")

    # The closing quote is the last byte of the first chunk.
    b <- c(r("\""), rep(r("x"), M - 2L), r("\""), r(",b\n1,2\n"))
    h <- dta_head_records(put(b, gz), 2L)
    expect_identical(h$multiline, c(FALSE, FALSE))
    expect_length(h$records[[1L]], M + 2L)
    expect_identical(mx_rec(h)[[2L]], "1,2")

    # The opening quote is the last byte of the first chunk; the record that
    # follows it spans a line.
    b <- c(r("h\n"), rep(r("x"), M - 4L), r(",\""), r("a\nb\",c\n1,2\n"))
    expect_identical(b[M], r("\""))
    h <- dta_head_records(put(b, gz), 3L, newlines_in_values = TRUE)
    expect_identical(h$multiline, c(FALSE, TRUE, FALSE))
    expect_identical(mx_rec(h)[[3L]], "1,2")
  }
})

test_that("a 60,000-label quoted header (over 1 MiB) is read whole by the matrix head reader", {
  k <- 60000L
  labels <- paste0("\"label number ", seq_len(k), "\"", collapse = ",")
  path <- mx_file(paste0(labels, "\n", strrep("1,", k), "1\n"))
  expect_gt(file.size(path), 1048576)
  elapsed <- system.time(h <- dta_matrix_read_head(DTAFileCSV(filename = "m.csv"), path))[["elapsed"]]
  expect_identical(h$n_header, k)
  expect_identical(h$fields[[k]], "label number 60000")
  expect_identical(h$fields[[1L]], "label number 1")
  expect_identical(h$n_first_row, k + 1L)
  expect_lt(elapsed, 10)
})

test_that("dta_matrix_read_head names an unsupported encoding itself", {
  handler <- DTAFileCSV(filename = "m.csv", encoding = "no-such-encoding")
  expect_error(
    dta_matrix_read_head(handler, mx_file("a,b\n1,2\n")),
    class = "dta_encoding_unsupported"
  )
})
