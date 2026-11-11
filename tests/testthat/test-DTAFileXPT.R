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
