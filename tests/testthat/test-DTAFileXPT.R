# Reading a SAS transport (XPORT) file as input data: the `xpt` file type.
#
# Fixtures under inst/qualification/fx/container/ carry the evidence (built by
# .github/scripts/build_container_fixtures.R from the literal data it lists):
#
# * adverse.xpt, a one-dataset file: AE with USUBJID, AETERM, AESEQ and two
#   records (S-001/HEADACHE/1, S-002/NAUSEA/2).
# * sdtm.xpt, a library of three datasets: AE, DM and VS.
# * cport.xpt (a CPORT file) and not_transport.xpt (not SAS at all).
#
# Every expected value is written out from those data and from SAS semantics --
# the epoch is 1960-01-01, so SAS day 23968 is 3653 + 20315, i.e. 2025-08-15 --
# never taken from the reader.

# A gzip-compressed copy of a fixture, called `name`, in a fresh temp directory.
xpt_gzip <- function(fixture, name, env = parent.frame()) {
  path <- file.path(withr::local_tempdir(.local_envir = env), name)
  con <- gzfile(path, "wb")
  writeBin(readBin(cont_fx(fixture), "raw", file.size(cont_fx(fixture))), con)
  close(con)
  path
}

test_that("the xpt type builds a DTAFileXPT, which is a DTAFileSAS7BDAT", {
  h <- DTAFileFactory(type = "xpt", filename = "ae.xpt")
  expect_identical(class(h)[[1]], "DTAtools::DTAFileXPT")
  expect_true(inherits(h, "DTAtools::DTAFileSAS7BDAT"))
  expect_false(inherits(h, "DTAtools::DTAFileTabular"))

  from_list <- dta_file_handlers_from_list(list(type = "xpt", filename = "ae.xpt"))[[1]]
  expect_identical(class(from_list)[[1]], "DTAtools::DTAFileXPT")
})

test_that("a tabular dataset accepts an xpt entry", {
  ds <- DTADataSetTabular(
    id = "ae",
    specs = cont_specs("ae"),
    files = list(DTAFileXPT("ae.xpt"))
  )
  expect_true(inherits(ds, "DTAtools::DTADataSetTabular"))
  expect_true(inherits(ds@files[[1]], "DTAtools::DTAFileXPT"))
})

test_that("a single-member file is read with every observation and variable", {
  t <- as.data.frame(read_file(
    DTAFileXPT("adverse.xpt"),
    cont_fx("adverse.xpt"),
    specs = cont_specs("ae")
  ))
  expect_identical(names(t), c("USUBJID", "AETERM", "AESEQ"))
  expect_identical(t$USUBJID, c("S-001", "S-002"))
  expect_identical(t$AETERM, c("HEADACHE", "NAUSEA"))
  expect_equal(t$AESEQ, c(1, 2))
})

test_that("a library is refused, naming every member", {
  e <- expect_error(
    read_file(DTAFileXPT("sdtm.xpt"), cont_fx("sdtm.xpt")),
    class = "dta_xpt_multiple_members"
  )
  expect_identical(e$members, c("AE", "DM", "VS"))
})

test_that("a gzip-compressed single-member file is read", {
  gz <- xpt_gzip("adverse.xpt", "adverse.xpt.gz")
  expect_equal(nrow(read_file(DTAFileXPT("adverse.xpt"), gz)), 2)
})

test_that("a file that is not SAS transport is refused before haven sees it", {
  expect_error(
    read_file(DTAFileXPT("not_transport.xpt"), cont_fx("not_transport.xpt")),
    class = "dta_xpt_not_transport"
  )

  # The 240 bytes of the library header and nothing after: no member at all.
  expect_error(
    read_file(DTAFileXPT("empty.xpt"), cont_cut("sdtm.xpt", 240, "empty.xpt")),
    class = "dta_xpt_read_error"
  )
})

test_that("temporal columns are normalised as for sas7bdat", {
  members <- dta_xpt_members(cont_fx("sdtm.xpt"))
  dest <- file.path(withr::local_tempdir(), "lib")
  parts <- dta_xpt_split(cont_fx("sdtm.xpt"), members, dest)
  vs <- parts[members$name == "vs.xpt"]
  expect_length(vs, 1)

  # VSDT declared a SAS date under DATE9.: read as the text SAS would display.
  t <- as.data.frame(read_file(DTAFileXPT("vs.xpt"), vs, specs = cont_specs("vs")))
  expect_identical(t$VSDT, c("15AUG2025", "15AUG2025", "16AUG2025"))

  # Declared SAS Num instead: the SAS number, days since 1960-01-01.
  num_specs <- DTAColumnSpecCollection(
    columns = list(
      USUBJID = DTAColumnSpec(id = "USUBJID", type = "SAS Char", nullable = FALSE),
      VSTESTCD = DTAColumnSpec(id = "VSTESTCD", type = "SAS Char", nullable = FALSE),
      VSSTRESN = DTAColumnSpec(id = "VSSTRESN", type = "SAS Num", nullable = FALSE),
      VSDT = DTAColumnSpec(id = "VSDT", type = "SAS Num", nullable = FALSE)
    ),
    rules = list()
  )
  t_num <- as.data.frame(read_file(DTAFileXPT("vs.xpt"), vs, specs = num_specs))
  expect_identical(t_num$VSDT, c(23968, 23968, 23969))
})

test_that("an xpt is held in memory and says so when its size would stream", {
  withr::local_options(DTAtools.stream_threshold = 1)
  ds <- DTADataSetTabular(
    id = "adverse",
    specs = cont_specs("ae"),
    files = list(DTAFileXPT("adverse.xpt"))
  )
  expect_message(
    ds <- load_file(ds, file = cont_fx("adverse.xpt")),
    class = "dta_sas7bdat_in_memory"
  )
  expect_true(inherits(tables(ds)[["adverse"]], "Table"))
})

test_that("open_file refuses an xpt", {
  expect_error(
    open_file(DTAFileXPT("adverse.xpt"), cont_fx("adverse.xpt")),
    regexp = "Streaming is not supported"
  )
})

test_that("the delimited entry points refuse an xpt", {
  path <- cont_fx("adverse.xpt")
  specs <- cont_specs("ae")
  cache <- file.path(withr::local_tempdir(), "cache")

  for (call in list(
    quote(validate_file_stream(specs, path, verbose = FALSE)),
    quote(cache_as_parquet(specs, path, cache_path = cache))
  )) {
    cnd <- tryCatch(eval(call), error = function(e) e)
    expect_s3_class(cnd, "dta_sas7bdat_not_delimited")
    # Package-authored wording only (cli wraps lines, so flatten white space):
    # it names the cause and the way out.
    msg <- gsub("\\s+", " ", conditionMessage(cnd))
    expect_match(msg, "transport file", fixed = TRUE)
    expect_match(msg, "DTAFileXPT", fixed = TRUE)
  }
  expect_false(dir.exists(cache))
})

test_that("a gzip transport file is judged by its inflated member size", {
  gz <- xpt_gzip("adverse.xpt", "adverse.xpt.gz")

  # The gzip copy is 248 bytes on disk; its member is 1,280 bytes inflated, and
  # the cap is judged against the second number.
  withr::with_options(
    list(DTAtools.container_max_member_bytes = 1000),
    {
      e <- expect_error(
        read_file(DTAFileXPT("adverse.xpt"), gz),
        class = "dta_container_too_large"
      )
      expect_equal(e$value, 1280)
      expect_identical(e$limit, "DTAtools.container_max_member_bytes")
    }
  )

  withr::with_options(
    list(DTAtools.container_max_member_bytes = 2000),
    expect_equal(nrow(read_file(DTAFileXPT("adverse.xpt"), gz)), 2)
  )
})

test_that("the transport reader shows file names literally", {
  cport <- file.path(withr::local_tempdir(), "{1+1}.xpt")
  expect_true(file.copy(cont_fx("cport.xpt"), cport))
  lib <- file.path(withr::local_tempdir(), "{1+1}l.xpt")
  expect_true(file.copy(cont_fx("sdtm.xpt"), lib))

  e1 <- expect_error(
    read_file(DTAFileXPT("x.xpt"), cport, namecheck = FALSE),
    class = "dta_xpt_not_transport"
  )
  expect_match(conditionMessage(e1), "{1+1}", fixed = TRUE)

  e2 <- expect_error(
    read_file(DTAFileXPT("x.xpt"), lib, namecheck = FALSE),
    class = "dta_xpt_multiple_members"
  )
  expect_match(conditionMessage(e2), "{1+1}", fixed = TRUE)
})

test_that("a haven failure is reported once, without parent or server path", {
  p <- file.path(withr::local_tempdir(), "adverse.xpt")
  expect_true(file.copy(cont_fx("adverse.xpt"), p))
  testthat::local_mocked_bindings(
    read_xpt = function(file, ...) stop(paste0("Failed to parse ", file, ": boom")),
    .package = "haven"
  )

  e <- expect_error(
    read_file(DTAFileXPT("adverse.xpt"), p),
    class = "dta_xpt_read_error"
  )
  msg <- conditionMessage(e)
  expect_true(is.null(e$parent))
  expect_identical(lengths(regmatches(msg, gregexpr("boom", msg, fixed = TRUE))), 1L)
  expect_false(grepl(dirname(p), msg, fixed = TRUE))
})

xpt_flat <- function(e) gsub("[[:space:]]+", " ", conditionMessage(e))

# ---- text encoding ---------------------------------------------------------------

# A V5 transport file holding `df` (text only), written by haven, in a temp file.
xpt_write <- function(df, version = 5, name = "AE", env = parent.frame()) {
  p <- file.path(withr::local_tempdir(.local_envir = env), "t.xpt")
  haven::write_xpt(df, p, version = version, name = name)
  p
}

test_that("text that is not valid UTF-8 is refused, naming the column and row but not the bytes", {
  # A transport file records no encoding; a Latin-1 SAS session writes the byte
  # 0xDC for the umlaut. ASCII placeholders are written, then patched.
  p <- xpt_write(data.frame(
    SUBJ = c("S1", "S2"), NAME = c("ABC", "MXLLER"),
    stringsAsFactors = FALSE
  ))
  b <- readBin(p, "raw", file.size(p))
  at <- grepRaw("MXLLER", b, fixed = TRUE)
  b[at + 1L] <- as.raw(0xDC)
  writeBin(b, p)

  e <- expect_error(read_file(DTAFileXPT("t.xpt"), p), class = "dta_xpt_read_error")
  expect_identical(e$path, p)
  m <- xpt_flat(e)
  expect_match(m, "NAME", fixed = TRUE)
  expect_match(m, "row 2", fixed = TRUE)
  expect_match(m, "UTF-8", fixed = TRUE)
  expect_false(grepl(dirname(p), m, fixed = TRUE))
})

test_that("a non-ASCII UTF-8 value still reads", {
  p <- xpt_write(data.frame(NAME = c("M\u00fcller", "Abc"), stringsAsFactors = FALSE))
  t <- as.data.frame(read_file(DTAFileXPT("t.xpt"), p))
  expect_identical(t$NAME, c("M\u00fcller", "Abc"))
})

# ---- trailing blank rows ---------------------------------------------------------

# ReadStat takes trailing all-blank observations of a transport file for the
# padding that fills the last 80-byte record, so haven drops them. The reader
# gets the true count from the file and restores them as blank values.
blank_tail <- function(width) {
  data.frame(
    SUBJID = c("S1", "S2", "", ""), TERM = c("A", strrep("x", width), "", ""),
    stringsAsFactors = FALSE
  )
}
blank_specs <- function() {
  DTAColumnSpecCollection(columns = list(
    SUBJID = DTAColumnSpec(id = "SUBJID", type = "SAS Char", nullable = FALSE),
    TERM = DTAColumnSpec(id = "TERM", type = "SAS Char", nullable = FALSE)
  ))
}
n_errors <- function(ds, file) {
  v <- check(
    load_file(ds, file = file, handler_index = 1, stream = "never"),
    quiet = TRUE, persist = FALSE
  )
  validation_status(v)[["n_columnspec_errors"]]
}

test_that("trailing blank rows are restored when the row is 80 bytes or longer (V5) and in V8", {
  for (case in list(list(v = 5, w = 100), list(v = 8, w = 100), list(v = 8, w = 5))) {
    d <- blank_tail(case$w)
    p <- xpt_write(d, version = case$v)
    expect_lt(nrow(haven::read_xpt(p)), 4) # the defect haven alone has

    t <- as.data.frame(read_file(DTAFileXPT("t.xpt"), p))
    expect_identical(nrow(t), 4L, info = paste("version", case$v, "width", case$w))
    expect_identical(t$SUBJID, c("S1", "S2", NA, NA))
    expect_identical(t$TERM[3:4], c(NA_character_, NA_character_))

    # The blank rows violate nullable: false, exactly as the same data in a CSV.
    ds <- DTADataSetTabular(
      id = "ae", specs = blank_specs(), files = list(DTAFileXPT(filename = "t.xpt"))
    )
    csv <- file.path(withr::local_tempdir(), "t.csv")
    writeLines(c("SUBJID,TERM", paste0("S1,A"), paste0("S2,", d$TERM[2]), ",", ","), csv)
    dsc <- DTADataSetTabular(
      id = "ae", specs = blank_specs(), files = list(DTAFileCSV(filename = "t.csv"))
    )
    expect_gt(n_errors(ds, p), 0)
    expect_identical(n_errors(ds, p), n_errors(dsc, csv))
  }
})

test_that("a short V5 row cannot tell blank rows from padding (limitation)", {
  # 4 bytes per row: the padding that follows the data (under 80 bytes) could
  # hold twenty rows, so the file does not say how many were written and the
  # trailing blank rows stay lost. Pinned as the documented limitation.
  p <- xpt_write(blank_tail(2), version = 5)
  expect_true(is.na(dta_xpt_expected_rows(p, 240, file.size(p), 5L)))
  expect_identical(nrow(read_file(DTAFileXPT("t.xpt"), p)), 2L)
})

test_that("dta_xpt_expected_rows reads the count from the layout", {
  p <- xpt_write(data.frame(A = c("x", "y", "z"), stringsAsFactors = FALSE), version = 8)
  m <- dta_xpt_members(p)
  expect_identical(dta_xpt_expected_rows(p, m$start, m$end, 8L), 3)
  p5 <- xpt_write(data.frame(A = strrep("x", 90), stringsAsFactors = FALSE), version = 5)
  m5 <- dta_xpt_members(p5)
  expect_identical(dta_xpt_expected_rows(p5, m5$start, m5$end, 5L), 1)
})

test_that("zero-row and zero-variable members still read", {
  p <- xpt_write(data.frame(A = character(), stringsAsFactors = FALSE))
  expect_identical(nrow(read_file(DTAFileXPT("t.xpt"), p)), 0L)
  p8 <- xpt_write(data.frame(A = character(), stringsAsFactors = FALSE), version = 8)
  expect_identical(nrow(read_file(DTAFileXPT("t.xpt"), p8)), 0L)
})

# Regression (round-2 review): a V8 count was believed whatever the bytes
# said, so a forged header invented rows, or asked for billions (memory), and
# a writer's placeholder 0 refused a sound file.
test_that("a V8 row count the file's bytes cannot hold is not believed", {
  for (cnt in c("100", "30000000", "3000000000", "0")) {
    p <- xpt_write(data.frame(A = c("x", "y", "z"), stringsAsFactors = FALSE), version = 8)
    b <- readBin(p, "raw", file.size(p))
    at <- grepRaw("OBSV8   HEADER RECORD!!!!!!!", b, fixed = TRUE)
    # Bytes 49-64 of the OBSV8 record (`at` is its byte 21) hold the count.
    b[(at + 28L):(at + 43L)] <- charToRaw(formatC(cnt, width = 16))
    writeBin(b, p)
    t <- as.data.frame(read_file(DTAFileXPT("t.xpt"), p))
    expect_identical(t$A, c("x", "y", "z"), info = cnt)
  }
})

# Regression (round-3 review): "1e7 " in the NAMESTR header's variable count
# passed as.numeric() and asked readBin() for 1.4 GB.
test_that("a variable count that is not plain digits is not believed", {
  p <- xpt_write(data.frame(A = c("x", "y"), stringsAsFactors = FALSE), version = 8)
  m <- dta_xpt_members(p)
  b <- readBin(p, "raw", file.size(p))
  at <- m$start + 320 + 55 # bytes 55-58 of the NAMESTR header record, 1-based
  b[at:(at + 3L)] <- charToRaw("1e7 ")
  writeBin(b, p)
  testthat::local_mocked_bindings(
    dta_read_stream = function(con, n) {
      if (n > 1e6) stop("asked for ", n, " bytes")
      readBin(con, "raw", n)
    }
  )
  expect_true(is.na(dta_xpt_expected_rows(p, m$start, m$end, 8L)))
})

test_that("a header that disagrees with the rows haven read is refused", {
  p <- xpt_write(data.frame(A = c(1, 2)), version = 8)
  # Fewer rows than announced, in a numeric column: nothing to restore them as.
  testthat::local_mocked_bindings(
    read_xpt = function(...) tibble::tibble(A = 1),
    .package = "haven"
  )
  e <- expect_error(read_file(DTAFileXPT("t.xpt"), p), class = "dta_xpt_read_error")
  expect_match(xpt_flat(e), "announces 2", fixed = TRUE)
  # More rows than announced.
  testthat::local_mocked_bindings(
    read_xpt = function(...) tibble::tibble(A = c(1, 2, 3)),
    .package = "haven"
  )
  e <- expect_error(read_file(DTAFileXPT("t.xpt"), p), class = "dta_xpt_read_error")
  expect_match(xpt_flat(e), "3", fixed = TRUE)
})
