# load_delivery(): routing of loose files, zip archives and SAS transport
# libraries to the file entries of a specification. Every expected value is the
# literal data the fixtures were built from (helper-containers.R, the fixture
# builder) or the specification, never the code's own output.

# --- Local helpers ------------------------------------------------------------
# (ld, plan_of, rows_of, tab_ds, file_ds, dta_of live in helper-containers.R.)

# The bytes of a committed fixture.
cont_bytes <- function(fixture) readBin(cont_fx(fixture), "raw", file.size(cont_fx(fixture)))

empty_specs <- function() DTAColumnSpecCollection(columns = list())

n_tables <- function(d, id) length(tables(datasets(d, id)))

# A copy of a zip with every occurrence of the bytes of each `from` replaced by
# the same number of bytes of `to`: entry names that the file system of a test
# machine could not create (and that zip::zip() would not write) are built from
# a stand-in name of the same length.
swap_bytes <- function(path, from, to, env = parent.frame()) {
  expect_equal(nchar(to, type = "bytes"), nchar(from, type = "bytes"))
  b <- readBin(path, "raw", file.size(path))
  for (i in seq_along(from)) {
    hits <- grepRaw(charToRaw(from[[i]]), b, fixed = TRUE, all = TRUE)
    expect_gt(length(hits), 0)
    for (h in hits) b[h + seq_len(nchar(from[[i]], type = "bytes")) - 1L] <- charToRaw(to[[i]])
  }
  cont_write(basename(path), b, env = env)
}

# CONT_AE_CSV gzip-compressed into a temp file called `name`.
gz_text <- function(text, name, env = parent.frame()) {
  p <- file.path(withr::local_tempdir(.local_envir = env), name)
  con <- gzfile(p, "wb")
  writeBin(charToRaw(text), con)
  close(con)
  p
}

# The reason a deliverable .zip or .xpt carries when it was kept as one file.
KEPT_SENTENCE <- "kept as one file: a type: any entry takes it as a deliverable; to have its files checked, name it as the container of their entries"

# The nine-value AE file of the container-layer tests: values that look like
# header records of a second member.
forged_ae_xpt <- function(env = parent.frame()) {
  hdr <- "HEADER RECORD*******MEMBER  HEADER RECORD!!!!!!!000000000000000001600000000140  "
  dsc <- "HEADER RECORD*******DSCRPTR HEADER RECORD!!!!!!!000000000000000000000000000000  "
  mrec <- sprintf("%-80s", "SAS     ZZ      SASDATA 9.4     X64_10HO                        01JAN26:00:00:00")
  p <- file.path(withr::local_tempdir(.local_envir = env), "ae.xpt")
  haven::write_xpt(
    data.frame(X = c(sprintf("%-80s", paste0("ROW", 1:3)), hdr, dsc, mrec, sprintf("%-80s", paste0("ROW", 7:9)))),
    p,
    version = 5, name = "AE"
  )
  p
}

# --- Zips ---------------------------------------------------------------------

# 1.
test_that("a zip of CSVs fills every dataset whose entry matches a member", {
  ex <- withr::local_tempdir()
  d <- ld(cont_dta("csv"), cont_fx("sdtm_csv.zip"), exdir = ex)
  plan <- plan_of(d)
  expect_identical(names(tables(datasets(d, "ae"))), "ae")
  expect_identical(names(tables(datasets(d, "dm"))), "dm")
  expect_identical(n_tables(d, "vs"), 0L)
  expect_identical(cont_table(d, "ae", "ae")$AETERM, c("HEADACHE", "NAUSEA"))

  loaded <- rows_of(plan, "loaded")
  expect_identical(loaded$name, c("ae.csv", "dm.csv"))
  expect_identical(loaded$dataset, c("ae", "dm"))
  expect_identical(loaded$item, c("ae", "dm"))
  expect_identical(loaded$container, c("sdtm_csv.zip", "sdtm_csv.zip"))

  unused <- rows_of(plan, "unused")
  expect_setequal(unused$name, c("define.xml", "readme.txt"))
  expect_identical(unused$member[unused$name == "readme.txt"], "docs/readme.txt")
  expect_setequal(rows_of(plan, "ignored")$name, c("._ae.csv", ".DS_Store"))

  opened <- rows_of(plan, "opened")
  expect_identical(nrow(opened), 1L)
  expect_identical(opened$name, "sdtm_csv.zip")
  expect_true(is.na(opened$container))

  ae_path <- loaded$path[loaded$name == "ae.csv"]
  expect_identical(basename(ae_path), "ae.csv")
  expect_identical(basename(dirname(dirname(ae_path))), "1-sdtm_csv.zip")
  expect_identical(basename(dirname(ae_path)), "e00001")
  on_disk <- list.files(ex, recursive = TRUE)
  expect_length(on_disk, 2)
  expect_setequal(basename(on_disk), c("ae.csv", "dm.csv"))

  # Extracted clinical data is readable by its owner only.
  if (.Platform$OS.type != "windows") {
    run_dir <- dirname(dirname(dirname(ae_path)))
    expect_identical(format(file.info(run_dir)$mode), "700")
  }
})

# 2.
test_that("the summary is one classed message", {
  expect_message(
    load_delivery(cont_dta("csv"), cont_fx("sdtm_csv.zip")),
    class = "dta_delivery_summary"
  )
})

# 3.
test_that("a member validates exactly like the same file delivered loose", {
  loose <- c(cont_write("ae.csv", CONT_AE_CSV), cont_write("dm.csv", CONT_DM_CSV))
  d_loose <- check(ld(cont_dta("csv"), loose), quiet = TRUE, persist = FALSE)
  d_zip <- check(ld(cont_dta("csv"), cont_fx("sdtm_csv.zip")), quiet = TRUE, persist = FALSE)
  for (d in list(d_loose, d_zip)) {
    expect_true(validation_status(datasets(d, "ae"))$ok)
    expect_true(validation_status(datasets(d, "dm"))$ok)
  }

  # One value, `x`, cannot be read as the declared SAS Num.
  bad <- paste0(CONT_AE_CSV, "S-003,VOMITING,x\n")
  d_loose <- check(ld(cont_dta("csv"), cont_write("ae.csv", bad)), quiet = TRUE, persist = FALSE)
  d_zip <- check(ld(cont_dta("csv"), cont_zip(list(ae.csv = bad))), quiet = TRUE, persist = FALSE)
  expect_identical(validation_status(datasets(d_loose, "ae"))$n_import_errors, 1L)
  expect_identical(validation_status(datasets(d_zip, "ae"))$n_import_errors, 1L)
})

# 4.
test_that("folders inside a zip are matched by base name and a gzip member is read", {
  d <- ld(cont_dta("csv"), cont_fx("sdtm_dirs.zip"))
  expect_equal(cont_table(d, "ae", "ae")$AESEQ, c(1, 2))
  expect_equal(cont_table(d, "dm", "dm")$AGE, c(34, 51))
  loaded <- rows_of(plan_of(d), "loaded")
  dm <- loaded[loaded$dataset == "dm", ]
  expect_identical(dm$member, "sdtm/dm.csv.gz")
  expect_identical(dm$name, "dm.csv.gz")
  expect_identical(dm$item, "dm")
})

# 5.
test_that("backslash folders from Windows tools are folders", {
  d <- ld(cont_dta("csv"), cont_fx("backslash_sep.zip"))
  loaded <- rows_of(plan_of(d), "loaded")
  expect_setequal(loaded$dataset, c("ae", "dm"))
  ae <- loaded[loaded$dataset == "ae", ]
  expect_identical(ae$member, "sdtm\\ae.csv")
  expect_identical(ae$name, "ae.csv")
})

# 6.
test_that("a zip of SAS datasets", {
  f <- system.file("qualification", "fx", "sas", "iris.sas7bdat", package = "DTAtools")
  expect_true(nzchar(f))
  z <- cont_zip(list(iris.sas7bdat = readBin(f, "raw", file.size(f))))
  dta <- DTA(datasets = list(DTADataSetTabular(
    id = "iris", specs = DTAColumnSpecCollection(columns = list()),
    files = list(DTAFileSAS7BDAT("iris.sas7bdat"))
  )))
  d <- ld(dta, z)
  tab <- cont_table(d, "iris", "iris")
  expect_identical(nrow(tab), 150L)
  expect_equal(tab[[1]], datasets::iris$Sepal.Length)
})

# 7.
test_that("a zip of single-dataset transport files", {
  d <- ld(cont_dta("xpt"), cont_fx("sdtm_xpt.zip"))
  expect_identical(n_tables(d, "ae"), 1L)
  expect_identical(n_tables(d, "dm"), 1L)
  expect_identical(n_tables(d, "vs"), 0L)
  expect_equal(cont_table(d, "dm", "dm")$AGE, c(34, 51))
})

# 8.
test_that("a zip inside a zip is not opened", {
  e <- expect_error(
    ld(cont_dta("csv"), cont_fx("nested.zip")),
    class = "dta_delivery_nothing_loaded"
  )
  inner <- e$plan[e$plan$name == "inner.zip", ]
  expect_identical(nrow(inner), 1L)
  expect_identical(inner$status, "unused")
  expect_identical(inner$reason, "is a .zip inside a .zip, which is not opened")
})

# 9.
test_that("an irrelevant bad .xpt in a zip is neither extracted nor fatal", {
  ex <- withr::local_tempdir()
  z <- cont_zip(list(ae.xpt = cont_bytes("adverse.xpt"), cport.xpt = cont_bytes("cport.xpt")))
  dta <- dta_of(tab_ds("ae", list(DTAFileXPT("ae.xpt"))))
  d <- ld(dta, z, exdir = ex)
  expect_identical(nrow(cont_table(d, "ae", "ae")), 2L)
  plan <- plan_of(d)
  expect_identical(plan$status[plan$name == "cport.xpt"], "unused")
  expect_identical(basename(list.files(ex, recursive = TRUE)), "ae.xpt")
})

# --- Transport libraries ------------------------------------------------------

# 10.
test_that("a transport library is split into one file per member", {
  d <- ld(cont_dta("xpt"), cont_fx("sdtm.xpt"))
  loaded <- rows_of(plan_of(d), "loaded")
  expect_identical(loaded$name, c("ae.xpt", "dm.xpt", "vs.xpt"))
  expect_identical(loaded$member, c("AE", "DM", "VS"))
  expect_identical(loaded$container, rep("sdtm.xpt", 3))
  expect_identical(loaded$dataset, c("ae", "dm", "vs"))
  expect_equal(cont_table(d, "vs", "vs")$VSSTRESN, c(120, 80, 135))
  expect_identical(cont_table(d, "vs", "vs")$VSDT, c("15AUG2025", "15AUG2025", "16AUG2025"))
})

# 11.
test_that("a V8 library with a long member name", {
  dta <- dta_of(tab_ds("ae", list(DTAFileXPT("adverse_events.xpt")), cont_specs("ae")))
  d <- ld(dta, cont_fx("sdtm_v8.xpt"))
  expect_identical(nrow(cont_table(d, "ae", "adverse_events")), 2L)
  expect_identical(rows_of(plan_of(d), "unused")$name, "dm.xpt")
})

# 12.
test_that("a single-dataset file named as declared is read whole", {
  dta <- dta_of(tab_ds("adverse", list(DTAFileXPT("adverse.xpt")), empty_specs()))
  d <- ld(dta, cont_fx("adverse.xpt"))
  plan <- plan_of(d)
  expect_identical(nrow(rows_of(plan, "opened")), 0L)
  loaded <- rows_of(plan, "loaded")
  expect_identical(nrow(loaded), 1L)
  expect_true(is.na(loaded$container))
  expect_identical(loaded$path, cont_fx("adverse.xpt"))
})

# 13.
test_that("a single-dataset file named otherwise is not used", {
  e <- expect_error(
    ld(cont_dta("xpt"), cont_fx("adverse.xpt")),
    class = "dta_delivery_nothing_loaded"
  )
  expect_identical(
    e$plan$reason[e$plan$name == "adverse.xpt"],
    "matches no file entry of the specification"
  )
  # A declaration opens it, and member AE fills ae.
  d <- ld(cont_dta("xpt", container = "adverse.xpt"), cont_fx("adverse.xpt"))
  expect_identical(names(tables(datasets(d, "ae"))), "ae")
})

# 14.
test_that("a member unused only because of letter case says so", {
  dta <- dta_of(tab_ds("ae", list(DTAFileXPT("AE.xpt"))))
  e <- expect_error(ld(dta, cont_fx("sdtm.xpt")), class = "dta_delivery_nothing_loaded")
  expect_true("member AE is offered as ae.xpt; the specification declares AE.xpt" %in% e$plan$reason)
})

# 15.
test_that("a library named like its entry is refused, not split", {
  two <- cont_cut("sdtm.xpt", 2160, "ae.xpt") # members AE and DM
  e <- expect_error(ld(cont_dta("xpt"), two), class = "dta_delivery_load_error")
  expect_true(inherits(e$parent, "dta_xpt_multiple_members"))

  e <- expect_error(ld(cont_dta("xpt"), two, on_error = "skip"), class = "dta_delivery_nothing_loaded")
  expect_identical(e$plan$status[e$plan$name == "ae.xpt"], "refused")
})

# 16.
test_that("forged header values cannot cut a file the specification names", {
  d <- ld(cont_dta("xpt"), forged_ae_xpt())
  expect_identical(nrow(cont_table(d, "ae", "ae")), 9L)
})

# 17.
test_that("a library inside a zip is split too, and only its members stay on disk", {
  ex <- withr::local_tempdir()
  d <- ld(cont_dta("xpt"), cont_fx("lib_in_zip.zip"), exdir = ex)
  plan <- plan_of(d)
  loaded <- rows_of(plan, "loaded")
  expect_identical(nrow(loaded), 3L)
  expect_identical(loaded$container, rep("lib_in_zip.zip > sdtm.xpt", 3))
  expect_identical(rows_of(plan, "unused")$name, "define.xml")
  expect_setequal(basename(list.files(ex, recursive = TRUE)), c("ae.xpt", "dm.xpt", "vs.xpt"))
  expect_identical(unique(basename(dirname(loaded$path))), "2-sdtm.xpt")
})

# --- Routing ------------------------------------------------------------------

# 18.
test_that("a file outside its declared container is loaded and judged not conforming", {
  declared <- cont_dta("csv", container = "sdtm_csv.zip")
  loose_ae <- cont_write("ae.csv", CONT_AE_CSV)

  # D4: content and packaging are separate verdicts; a loose copy still loads.
  d <- ld(declared, loose_ae)
  loaded <- rows_of(plan_of(d), "loaded")
  expect_identical(loaded$dataset, "ae")
  expect_identical(loaded$conformant, FALSE)
  expect_match(loaded$reason, "requires it inside 'sdtm_csv.zip'", fixed = TRUE)
  expect_identical(cont_table(d, "ae", "ae")$AETERM, c("HEADACHE", "NAUSEA"))
  expect_identical(
    dta_provenance(datasets(d, "ae"))$ae,
    list(file = "ae.csv", container = character(), via = "delivery")
  )

  d <- ld(declared, cont_fx("sdtm_csv.zip"))
  expect_setequal(rows_of(plan_of(d), "loaded")$dataset, c("ae", "dm"))

  # A conforming copy beats a misfit one, in either order: the loose ae.csv is
  # unused and names where the other copy came from.
  for (files in list(c(loose_ae, cont_fx("sdtm_csv.zip")), c(cont_fx("sdtm_csv.zip"), loose_ae))) {
    d <- ld(declared, files)
    plan <- plan_of(d)
    loaded <- rows_of(plan, "loaded")
    expect_setequal(loaded$dataset, c("ae", "dm"))
    expect_identical(loaded$container, rep("sdtm_csv.zip", 2))
    expect_identical(loaded$conformant, c(TRUE, TRUE))
    loose_row <- plan[is.na(plan$container) & plan$name == "ae.csv", ]
    expect_identical(nrow(loose_row), 1L)
    expect_identical(loose_row$status, "unused")
    expect_identical(loose_row$reason, "another copy was loaded from sdtm_csv.zip > ae.csv")
  }
})

# 19.
test_that("a member of another container is loaded as a misfit, with the container it came in", {
  d <- ld(cont_dta("csv", container = "other.zip"), cont_fx("sdtm_csv.zip"))
  loaded <- rows_of(plan_of(d), "loaded")
  expect_setequal(loaded$dataset, c("ae", "dm"))
  expect_identical(loaded$conformant, c(FALSE, FALSE))
  expect_match(loaded$reason, "it arrived inside 'sdtm_csv.zip' and the agreement requires it inside 'other.zip'", fixed = TRUE)
  expect_false(any(grepl("letter case", loaded$reason, fixed = TRUE)))

  # The case hint moves into the misfit reason (D8).
  shouting <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "SDTM_CSV.ZIP")
  d <- ld(cont_dta("csv", container = "sdtm_csv.zip"), shouting)
  reason <- rows_of(plan_of(d), "loaded")$reason
  expect_identical(length(reason), 1L)
  expect_true(endsWith(reason, "(letter case must match)"))
})

# 20.
test_that("a wildcard container matches dated archive names", {
  d <- ld(cont_dta("csv", container = "sdtm_*.zip"), cont_fx("sdtm_csv.zip"))
  loaded <- rows_of(plan_of(d), "loaded")
  expect_setequal(loaded$dataset, c("ae", "dm"))
  expect_identical(loaded$conformant, c(TRUE, TRUE))

  # `?` is one character: "csv" is three, so the members fit nothing.
  d <- ld(cont_dta("csv", container = "sdtm_????.zip"), cont_fx("sdtm_csv.zip"))
  loaded <- rows_of(plan_of(d), "loaded")
  expect_setequal(loaded$dataset, c("ae", "dm"))
  expect_identical(loaded$conformant, c(FALSE, FALSE))
})

# 21.
test_that("the summary names what a declared container lacked", {
  one <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "sdtm_csv.zip")
  m <- expect_message(
    load_delivery(cont_dta("csv", container = "sdtm_csv.zip"), one),
    class = "dta_delivery_summary"
  )
  expect_match(conditionMessage(m), "Not in the delivery", fixed = TRUE)
  expect_match(conditionMessage(m), "dm.csv", fixed = TRUE)
  expect_true(setequal(m$missing, c("dm.csv", "vs.csv")))
  expect_identical(m$unused, character(0))

  # Only containers this call opened count.
  dta <- dta_of(
    tab_ds("ae", list(DTAFileCSV("ae.csv", container = "sdtm_csv.zip"))),
    tab_ds("dm", list(DTAFileCSV("dm.csv", container = "sdtm_csv.zip"))),
    tab_ds("lb", list(DTAFileCSV("lb.csv", container = "other.zip")), empty_specs())
  )
  m <- expect_message(load_delivery(dta, one), class = "dta_delivery_summary")
  expect_identical(m$missing, "dm.csv")

  # A pattern entry is named by its description.
  dta <- dta_of(
    tab_ds("ae", list(DTAFileCSV(
      "^ae_.*\\.csv$",
      pattern = TRUE, pattern_description = "ae_<date>.csv", container = "sdtm_csv.zip"
    ))),
    tab_ds("dm", list(DTAFileCSV("dm.csv")))
  )
  m <- expect_message(load_delivery(dta, cont_fx("sdtm_csv.zip")), class = "dta_delivery_summary")
  expect_identical(m$missing, "ae_<date>.csv")
})

# 22.
test_that("loose files are routed the same way", {
  files <- c(cont_write("ae.csv", CONT_AE_CSV), cont_write("dm.csv", CONT_DM_CSV))
  d <- ld(cont_dta("csv"), files)
  loaded <- rows_of(plan_of(d), "loaded")
  expect_setequal(loaded$dataset, c("ae", "dm"))
  expect_true(all(is.na(loaded$container)))
})

# 23.
test_that("a literal entry wins over another dataset's pattern", {
  dta <- suppressWarnings(read_dta_from_yaml(
    system.file("extdata", "clinical_dta_with_file_dataset.yaml", package = "DTAtools")
  ))
  files <- system.file("extdata", c("clinical_data.csv", "clinical_data2.csv.gz"), package = "DTAtools")
  d <- ld(dta, files)
  expect_identical(names(tables(datasets(d, "clinical_data"))), "clinical_data")
  expect_identical(basename(datasets(d, "raw_export")@file_paths), "clinical_data2.csv.gz")
})

# 24.
test_that("a literal entry wins over a pattern entry of the same dataset", {
  dta <- dta_of(tab_ds("ae", list(DTAFileCSV("^a.*\\.csv$", pattern = TRUE), DTAFileCSV("ae.csv"))))
  d <- ld(dta, cont_write("ae.csv", CONT_AE_CSV))
  loaded <- rows_of(plan_of(d), "loaded")
  expect_identical(loaded$handler, 2L)
})

# 25.
test_that("a container listed as a deliverable is also opened", {
  dta <- dta_of(
    file_ds("archive", DTAFileAny("sdtm_csv.zip")),
    file_ds("define", DTAFileAny("define.xml", container = "sdtm_csv.zip"))
  )
  d <- ld(dta, cont_fx("sdtm_csv.zip"))
  plan <- plan_of(d)
  expect_setequal(plan$status[plan$name == "sdtm_csv.zip"], c("loaded", "opened"))
  expect_identical(basename(datasets(d, "define")@file_paths), "define.xml")
})

# 26.
test_that("a pattern that takes everything does not swallow the archive", {
  dta <- dta_of(
    tab_ds("ae", list(DTAFileCSV("ae.csv"))),
    file_ds("docs", DTAFileAny(".*", pattern = TRUE))
  )
  d <- ld(dta, cont_fx("sdtm_csv.zip"))
  plan <- plan_of(d)
  zip_rows <- plan[plan$name == "sdtm_csv.zip", ]
  expect_setequal(zip_rows$status, c("loaded", "opened"))
  expect_identical(zip_rows$dataset[zip_rows$status == "loaded"], "docs")
  loaded <- rows_of(plan, "loaded")
  expect_identical(loaded$dataset[loaded$name == "ae.csv"], "ae")
  expect_identical(loaded$dataset[loaded$name == "define.xml"], "docs")
  expect_identical(loaded$dataset[loaded$name == "readme.txt"], "docs")
})

# 27.
test_that("two files that would be the same table are refused", {
  z <- cont_zip(list("a/ae.csv" = CONT_AE_CSV, "b/ae.csv" = CONT_AE_CSV))
  e <- expect_error(ld(cont_dta("csv"), z), class = "dta_delivery_duplicate_name")
  expect_identical(e$dataset, "ae")
  expect_identical(e$item, "ae")

  loose_ae <- cont_write("ae.csv", CONT_AE_CSV)
  expect_error(
    ld(cont_dta("csv"), c(loose_ae, cont_fx("sdtm_csv.zip"))),
    class = "dta_delivery_duplicate_name"
  )
})

# 28.
test_that("in skip mode only the clashing files are refused", {
  z <- cont_zip(list("a/ae.csv" = CONT_AE_CSV, "b/ae.csv" = CONT_AE_CSV))
  files <- c(z, cont_write("dm.csv", CONT_DM_CSV))
  expect_warning(
    d <- ld(cont_dta("csv"), files, on_error = "skip"),
    class = "dta_delivery_refused"
  )
  expect_identical(n_tables(d, "dm"), 1L)
  refused <- rows_of(plan_of(d), "refused")
  expect_identical(nrow(refused), 2L)
  expect_identical(refused$name, c("ae.csv", "ae.csv"))
  expect_identical(
    refused$reason,
    rep("is one of several files of the delivery that would be loaded as 'ae' into dataset 'ae'", 2)
  )
})

# 29.
test_that("a member two datasets need is extracted once", {
  ex <- withr::local_tempdir()
  dta <- dta_of(
    tab_ds("ae1", list(DTAFileCSV("ae.csv")), cont_specs("ae")),
    tab_ds("ae2", list(DTAFileCSV("ae.csv")), cont_specs("ae"))
  )
  d <- ld(dta, cont_fx("sdtm_csv.zip"), exdir = ex)
  expect_identical(names(tables(datasets(d, "ae1"))), "ae")
  expect_identical(names(tables(datasets(d, "ae2"))), "ae")
  loaded <- rows_of(plan_of(d), "loaded")
  expect_identical(nrow(loaded), 2L)
  expect_identical(loaded$path[[1]], loaded$path[[2]])
  expect_identical(sum(basename(list.files(ex, recursive = TRUE)) == "ae.csv"), 1L)
})

# --- Failure handling ---------------------------------------------------------

sas_and_csv_dta <- function() {
  DTA(datasets = list(
    DTADataSetTabular(
      id = "ae", specs = DTAColumnSpecCollection(columns = list()),
      files = list(DTAFileSAS7BDAT("ae.sas7bdat"))
    ),
    DTADataSetTabular(id = "dm", specs = cont_specs("dm"), files = list(DTAFileCSV("dm.csv")))
  ))
}

# 30.
test_that("by default an unreadable member aborts and leaves no trace", {
  ex <- withr::local_tempdir()
  z <- cont_zip(list(ae.sas7bdat = "not a SAS dataset\n", dm.csv = CONT_DM_CSV))
  dta0 <- sas_and_csv_dta()
  err <- tryCatch(load_delivery(dta0, z, exdir = ex), error = identity)
  expect_s3_class(err, "dta_delivery_load_error")
  expect_identical(err$dataset, "ae")
  expect_true(inherits(err$parent, "dta_sas7bdat_read_error"))
  expect_identical(length(tables(datasets(dta0, "dm"))), 0L)
  expect_identical(list.files(ex, recursive = TRUE), character(0))
})

# 31.
test_that("with on_error = \"skip\" the rest loads and the failure is listed without a server path", {
  z <- cont_zip(list(ae.sas7bdat = "not a SAS dataset\n", dm.csv = CONT_DM_CSV))
  dta0 <- sas_and_csv_dta()
  expect_warning(d <- ld(dta0, z, on_error = "skip"), class = "dta_delivery_refused")
  expect_equal(cont_table(d, "dm", "dm")$AGE, c(34, 51))
  row <- plan_of(d)[plan_of(d)$name == "ae.sas7bdat", ]
  expect_identical(row$status, "refused")
  for (p in c(
    tempdir(),
    normalizePath(tempdir(), winslash = "/"),
    normalizePath(tempdir(), winslash = "\\")
  )) {
    expect_false(grepl(p, row$reason, fixed = TRUE), info = p)
  }
})

# 32.
test_that("a delivery from which nothing loads is an error", {
  e <- expect_error(
    ld(cont_dta("csv"), cont_write("unrelated.txt", "x")),
    class = "dta_delivery_nothing_loaded"
  )
  expect_identical(e$plan$reason, "matches no file entry of the specification")
})

# 33.
test_that("bad input is refused first", {
  csv <- cont_write("ae.csv", CONT_AE_CSV)
  expect_error(ld(cont_dta("csv"), character(0)), class = "dta_delivery_bad_input")
  expect_error(ld(cont_dta("csv"), NA_character_), class = "dta_delivery_bad_input")
  expect_error(
    ld(cont_dta("csv"), file.path(withr::local_tempdir(), "nothing.csv")),
    class = "dta_delivery_bad_input"
  )
  expect_error(ld(cont_dta("csv"), withr::local_tempdir()), class = "dta_delivery_bad_input")
  expect_error(ld("a", csv), class = "dta_delivery_bad_input")

  # A missing file is named literally, never as cli markup.
  e <- expect_error(
    ld(cont_dta("csv"), file.path(withr::local_tempdir(), "{1+1}.csv")),
    class = "dta_delivery_bad_input"
  )
  expect_match(conditionMessage(e), "{1+1}", fixed = TRUE)
})

# 34.
test_that("the archive that crashed zip::unzip is refused and nothing is written", {
  p <- withr::local_tempdir()
  ex2 <- file.path(p, "x")
  expect_error(
    ld(cont_dta("csv"), cont_fx("unsafe_dotdot_mid.zip"), exdir = ex2),
    class = "dta_container_unsafe_member"
  )
  expect_identical(list.files(p, recursive = TRUE, all.files = TRUE), character(0))
})

# 35.
test_that("skip mode refuses a hostile archive and keeps going", {
  files <- c(cont_fx("unsafe_dotdot.zip"), cont_write("ae.csv", CONT_AE_CSV))
  expect_warning(
    d <- ld(cont_dta("csv"), files, on_error = "skip"),
    class = "dta_delivery_refused"
  )
  expect_identical(n_tables(d, "ae"), 1L)
  plan <- plan_of(d)
  expect_identical(plan$status[plan$name == "unsafe_dotdot.zip"], "refused")
})

# 36.
test_that("names with braces are data, never cli markup", {
  withr::local_envvar(DTA_PWNED = "")
  nm <- "{Sys.setenv(DTA_PWNED = '1')}.csv"
  zipped <- cont_zip(stats::setNames(list(CONT_AE_CSV), nm))
  loose <- cont_write(nm, CONT_AE_CSV)

  # Against a specification that does not name it: nothing loads, either mode.
  for (f in list(zipped, loose)) {
    for (mode in c("abort", "skip")) {
      expect_error(ld(cont_dta("csv"), f, on_error = mode), class = "dta_delivery_nothing_loaded")
    }
  }

  # Against one that declares it literally: it loads, and is then printed and
  # checked.
  dta_x <- dta_of(tab_ds("x", list(DTAFileCSV(nm)), cont_specs("ae")))
  for (f in list(zipped, loose)) {
    d <- ld(dta_x, f)
    expect_identical(n_tables(d, "x"), 1L)
    utils::capture.output(suppressMessages(print_info(datasets(d, "x"))))
    suppressWarnings(suppressMessages(check(d, quiet = TRUE, persist = FALSE)))
  }
  expect_identical(Sys.getenv("DTA_PWNED"), "")

  # {1+1} stays {1+1}.
  z <- cont_zip(list("{1+1}.csv" = CONT_AE_CSV, "dm.csv" = CONT_DM_CSV))
  m <- expect_message(load_delivery(cont_dta("csv"), z), class = "dta_delivery_summary")
  expect_match(conditionMessage(m), "{1+1}", fixed = TRUE)

  z2 <- cont_zip(list("a/{1+1}.csv" = CONT_AE_CSV, "b/{1+1}.csv" = CONT_AE_CSV))
  dta_b <- dta_of(tab_ds("x", list(DTAFileCSV("{1+1}.csv")), cont_specs("ae")))
  e <- expect_error(ld(dta_b, z2), class = "dta_delivery_duplicate_name")
  expect_match(conditionMessage(e), "{1+1}", fixed = TRUE)
  # The header only: the item {1+1} would make the whole message match, but
  # only the sources hold the folder.
  header <- cli::ansi_strip(rlang::cnd_header(e))
  expect_match(header, "a/{1+1}.csv", fixed = TRUE)
  expect_match(header, "b/{1+1}.csv", fixed = TRUE)
})

# --- Limits, streaming, shape -------------------------------------------------

# 37.
test_that("the byte budget covers what is extracted", {
  withr::local_options(DTAtools.container_max_bytes = 60)
  e <- expect_error(
    ld(cont_dta("csv"), cont_fx("sdtm_csv.zip")),
    class = "dta_container_too_large"
  )
  expect_identical(e$limit, "DTAtools.container_max_bytes")
  expect_equal(e$value, 83) # 53 + 30: the two claimed entries; nothing else is extracted
  expect_match(conditionMessage(e), "83 bytes against a limit of 60 bytes", fixed = TRUE)

  # In skip mode the error of "nothing loaded" carries the refusals.
  expect_no_warning(
    e <- expect_error(
      ld(cont_dta("csv"), cont_fx("sdtm_csv.zip"), on_error = "skip"),
      class = "dta_delivery_nothing_loaded"
    ),
    class = "dta_delivery_refused"
  )
  expect_setequal(rows_of(e$plan, "ignored")$name, c("._ae.csv", ".DS_Store"))
  expect_match(conditionMessage(e), "sdtm_csv.zip: ", fixed = TRUE)
  expect_false(grepl("._ae.csv", conditionMessage(e), fixed = TRUE))
})

# 38.
test_that("the files a call creates count against the member cap", {
  withr::local_options(DTAtools.container_max_members = 2)
  files <- c(
    cont_zip(list(ae.csv = CONT_AE_CSV), "z1.zip"),
    cont_zip(list(dm.csv = CONT_DM_CSV), "z2.zip"),
    cont_zip(list(vs.csv = CONT_AE_CSV), "z3.zip")
  )
  e <- expect_error(ld(cont_dta("csv"), files), class = "dta_container_too_large")
  expect_identical(e$limit, "DTAtools.container_max_members")
  expect_equal(e$value, 3)
  expect_match(conditionMessage(e), "3 entries or files against a limit of 2 entries or files", fixed = TRUE)
})

# 39.
test_that("the per-member cap refuses a large member", {
  withr::local_options(DTAtools.container_max_member_bytes = 40)
  e <- expect_error(
    ld(cont_dta("csv"), cont_fx("sdtm_csv.zip")),
    class = "dta_container_too_large"
  )
  expect_identical(e$limit, "DTAtools.container_max_member_bytes")
  expect_equal(e$value, 53)

  expect_warning(
    d <- ld(cont_dta("csv"), cont_fx("sdtm_csv.zip"), on_error = "skip"),
    class = "dta_delivery_refused"
  )
  plan <- plan_of(d)
  expect_identical(rows_of(plan, "loaded")$name, "dm.csv") # 30 bytes
  expect_identical(
    plan$reason[plan$name == "ae.csv" & plan$status == "refused"],
    "is larger (53 bytes) than DTAtools.container_max_member_bytes (40)"
  )

  # A library: the members are 1280, 1120 and 1360 bytes.
  withr::local_options(DTAtools.container_max_member_bytes = 1000)
  e <- expect_error(
    ld(cont_dta("xpt"), cont_fx("sdtm.xpt"), on_error = "skip"),
    class = "dta_delivery_nothing_loaded"
  )
  expect_identical(nrow(rows_of(e$plan, "refused")), 3L)
})

# 40.
test_that("stream = \"auto\" decides on the member's size", {
  withr::local_options(DTAtools.stream_threshold = 40)
  d <- ld(cont_dta("csv"), cont_fx("sdtm_csv.zip"))
  expect_true(inherits(tables(datasets(d, "ae"))[["ae"]], "Dataset")) # 53 bytes > 40
  expect_true(inherits(tables(datasets(d, "dm"))[["dm"]], "Table")) # 30 bytes <= 40
  checked <- check(d, quiet = TRUE, persist = FALSE)
  expect_true(validation_status(datasets(checked, "ae"))$ok)
})

# 41.
test_that("a single dataset takes a delivery", {
  d <- ld(datasets(cont_dta("csv"), "ae"), cont_fx("sdtm_csv.zip"))
  expect_true(inherits(d, "DTAtools::DTADataSetTabular"))
  expect_identical(names(tables(d)), "ae")
  plan <- plan_of(d)
  expect_identical(plan$status[plan$name == "dm.csv"], "unused")
})

# 42.
test_that("last_delivery has the documented columns", {
  plan <- plan_of(ld(cont_dta("csv"), cont_fx("sdtm_csv.zip")))
  expect_identical(
    names(plan),
    c(
      "delivered", "container", "member", "name", "dataset", "handler", "item", "status", "reason",
      "conformant", "candidates", "path"
    )
  )
  expect_true(is.integer(plan$handler))
  expect_true(is.list(plan$candidates))
  expect_true(is.logical(plan$conformant))
})

# 43.
test_that("a container name of 255 characters still loads", {
  long <- file.path(withr::local_tempdir(), paste0(strrep("x", 251), ".zip"))
  skip_if_not(
    isTRUE(suppressWarnings(file.copy(cont_fx("sdtm_csv.zip"), long))),
    "the temp directory cannot hold a 255-character name"
  )
  d <- ld(cont_dta("csv"), long)
  loaded <- rows_of(plan_of(d), "loaded")
  expect_setequal(loaded$dataset, c("ae", "dm"))
  ae_path <- loaded$path[loaded$dataset == "ae"]
  expect_lte(nchar(basename(dirname(dirname(ae_path)))), 104)
})

# --- Added with the routing, cleanup and size fixes ---------------------------

# 44.
test_that("an outer container declaration reaches a library's members", {
  d <- ld(cont_dta("xpt", container = "lib_in_zip.zip"), cont_fx("lib_in_zip.zip"))
  expect_setequal(rows_of(plan_of(d), "loaded")$name, c("ae.xpt", "dm.xpt", "vs.xpt"))
})

# 45.
test_that("the first matching pattern entry wins", {
  dta <- dta_of(tab_ds("ae", list(
    DTAFileCSV("^a.*\\.csv$", pattern = TRUE),
    DTAFileCSV("^ae.*\\.csv$", pattern = TRUE)
  )))
  d <- ld(dta, cont_write("ae.csv", CONT_AE_CSV))
  expect_identical(rows_of(plan_of(d), "loaded")$handler, 1L)
})

# 46.
test_that("a declared domain the delivery lacks does not make other .xpt entries extracted", {
  dd <- withr::local_tempdir()
  zip::unzip(cont_fx("sdtm_xpt.zip"), files = "dm.xpt", exdir = dd)
  dm_bytes <- readBin(file.path(dd, "dm.xpt"), "raw", file.size(file.path(dd, "dm.xpt")))
  z <- cont_zip(list(
    ae.xpt = cont_bytes("adverse.xpt"), dm.xpt = dm_bytes, lb.xpt = cont_bytes("adverse.xpt")
  ))

  ex <- withr::local_tempdir()
  d <- ld(cont_dta("xpt"), z, exdir = ex)
  plan <- plan_of(d)
  expect_setequal(rows_of(plan, "loaded")$dataset, c("ae", "dm"))
  lb <- plan[plan$name == "lb.xpt", ]
  expect_identical(lb$status, "unused")
  expect_true(is.na(lb$path))
  expect_setequal(basename(list.files(ex, recursive = TRUE)), c("ae.xpt", "dm.xpt"))

  # ae 1280 + dm 1120 fit, lb's 1280 more would not.
  withr::local_options(DTAtools.container_max_bytes = 3000)
  ex2 <- withr::local_tempdir()
  d2 <- ld(cont_dta("xpt"), z, exdir = ex2)
  expect_setequal(rows_of(plan_of(d2), "loaded")$dataset, c("ae", "dm"))
})

# 47.
test_that("a refused container gives back its charge and leaves nothing", {
  withr::local_options(DTAtools.container_max_bytes = 85)
  ex <- withr::local_tempdir()
  files <- c(cont_fx("lying_size.zip"), cont_fx("sdtm_csv.zip"))
  expect_warning(
    d <- ld(cont_dta("csv"), files, on_error = "skip", exdir = ex),
    class = "dta_delivery_refused"
  )
  plan <- plan_of(d)
  expect_identical(plan$status[plan$name == "lying_size.zip"], "refused")
  expect_setequal(rows_of(plan, "loaded")$dataset, c("ae", "dm"))
  on_disk <- list.files(ex, recursive = TRUE)
  expect_length(on_disk, 2)
  expect_setequal(basename(on_disk), c("ae.csv", "dm.csv"))
})

# 48.
test_that("a gzip file read into memory is judged by what it inflates to", {
  big <- paste0("USUBJID,AETERM,AESEQ\n", strrep("S-001,HEADACHE,1\n", 200)) # 3,421 bytes
  expect_identical(nchar(big), 3421L)
  gz <- gz_text(big, "ae.csv.gz")
  withr::local_options(DTAtools.container_max_member_bytes = 1000)

  # As a zip member its declared size is the compressed one.
  gz_bytes <- readBin(gz, "raw", file.size(gz))
  e <- expect_error(
    ld(cont_dta("csv"), cont_zip(list(ae.csv.gz = gz_bytes))),
    class = "dta_delivery_load_error"
  )
  expect_s3_class(e$parent, "dta_container_too_large")
  expect_identical(e$parent$limit, "DTAtools.container_max_member_bytes")

  # Loose, in skip mode: the rest loads.
  files <- c(gz, cont_write("dm.csv", CONT_DM_CSV))
  expect_warning(
    d <- ld(cont_dta("csv"), files, on_error = "skip"),
    class = "dta_delivery_refused"
  )
  plan <- plan_of(d)
  expect_identical(plan$status[plan$name == "dm.csv"], "loaded")
  expect_identical(plan$status[plan$name == "ae.csv.gz"], "refused")

  # A streamed file is not counted.
  d <- ld(cont_dta("csv"), gz, stream = "always")
  expect_identical(plan_of(d)$status[plan_of(d)$name == "ae.csv.gz"], "loaded")
})

# 49.
test_that("the same file given twice is one file", {
  d <- ld(cont_dta("csv"), c(cont_fx("sdtm_csv.zip"), cont_fx("sdtm_csv.zip")))
  expect_setequal(rows_of(plan_of(d), "loaded")$dataset, c("ae", "dm"))

  # Another spelling of the same path is the same file.
  z <- cont_fx("sdtm_csv.zip")
  d <- ld(cont_dta("csv"), c(z, file.path(dirname(z), ".", basename(z))))
  plan <- plan_of(d)
  expect_setequal(rows_of(plan, "loaded")$dataset, c("ae", "dm"))
  expect_identical(nrow(rows_of(plan, "opened")), 1L)
})

# --- Deliverables, bookkeeping and cost ---------------------------------------

# 50.
test_that("a zip a type: any entry takes as a deliverable stays closed", {
  reports <- dta_of(file_ds(
    "reports",
    DTAFileAny("^report_.*", pattern = TRUE, number_of_files = 1, extensions = c("pdf", "zip"))
  ))
  z <- cont_zip(list(a.pdf = "%PDF-1\n", b.pdf = "%PDF-2\n"), name = "report_2024.zip")
  d <- ld(reports, z)
  expect_identical(plan_of(d)$status, "loaded")
  checked <- check(d, quiet = TRUE, persist = FALSE)
  expect_true(all(validation_status(datasets(checked, "reports"))$ok))

  # Never inspected, so never refused.
  enc <- file.path(withr::local_tempdir(), "report_enc.zip")
  expect_true(file.copy(cont_fx("encrypted.zip"), enc))
  d <- ld(reports, enc)
  expect_identical(plan_of(d)$status, "loaded")

  # The transfer archive itself is listed next to the domains.
  dta <- dta_of(
    file_ds("docs", DTAFileAny("sdtm_csv.zip")),
    tab_ds("ae", list(DTAFileCSV("ae.csv")))
  )
  d <- ld(dta, cont_fx("sdtm_csv.zip"))
  expect_identical(plan_of(d)$status, "loaded")
  expect_identical(n_tables(d, "ae"), 0L)

  # Naming it as the container of the domain entry opens it.
  dta <- dta_of(
    file_ds("docs", DTAFileAny("sdtm_csv.zip")),
    tab_ds("ae", list(DTAFileCSV("ae.csv", container = "sdtm_csv.zip")))
  )
  d <- ld(dta, cont_fx("sdtm_csv.zip"))
  plan <- plan_of(d)
  expect_setequal(plan$status[plan$name == "sdtm_csv.zip"], c("loaded", "opened"))
  expect_identical(names(tables(datasets(d, "ae"))), "ae")
})

# 51.
test_that("a literal any entry takes a .xpt whole, unscanned", {
  # A scan would refuse these bytes as not transport.
  dta <- dta_of(file_ds("docs", DTAFileAny("ae.xpt")))
  d <- ld(dta, cont_cut("cport.xpt", 160, "ae.xpt"))
  expect_identical(plan_of(d)$status, "loaded")
  # Kept as one file is said on its row, with the remedy.
  expect_identical(plan_of(d)$reason, KEPT_SENTENCE)
})

# 52.
test_that("a pattern any entry listing xpt keeps a library whole", {
  dta <- dta_of(file_ds(
    "docs",
    DTAFileAny("\\.xpt$", pattern = TRUE, number_of_files = 1, extensions = "xpt")
  ))
  d <- ld(dta, cont_fx("sdtm.xpt"))
  expect_identical(plan_of(d)$status, "loaded")
  expect_identical(plan_of(d)$reason, KEPT_SENTENCE)
  checked <- check(d, quiet = TRUE, persist = FALSE)
  expect_true(all(validation_status(datasets(checked, "docs"))$ok))
})

# 53.
test_that("an archive bound as a file that cannot be opened is not refused", {
  dta <- dta_of(
    file_ds("docs", DTAFileAny(".*", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 10)),
    tab_ds("ae", list(DTAFileCSV("ae.csv")))
  )
  expect_no_warning(d <- ld(dta, cont_fx("encrypted.zip"), on_error = "skip"))
  plan <- plan_of(d)
  expect_setequal(plan$status, c("loaded", "opened"))
  expect_false("refused" %in% plan$status)
  expect_true(startsWith(plan$reason[plan$status == "opened"], "could not be opened: "))
})

# 54.
test_that("a refused container leaves nothing 'not in the delivery'", {
  dta <- dta_of(
    tab_ds("ae", list(DTAFileCSV("ae.csv", container = "encrypted.zip"))),
    tab_ds("dm", list(DTAFileCSV("dm.csv")))
  )
  files <- c(cont_fx("encrypted.zip"), cont_zip(list(dm.csv = CONT_DM_CSV), name = "other.zip"))
  m <- expect_message(
    suppressWarnings(load_delivery(dta, files, on_error = "skip")),
    class = "dta_delivery_summary"
  )
  expect_length(m$missing, 0)
})

# 55.
test_that("routing an archive is linear in its entries", {
  ids <- sprintf("d%02d", 1:60)
  dta <- DTA(datasets = lapply(ids, function(id) {
    tab_ds(id, list(DTAFileCSV(paste0(id, ".csv"))), empty_specs())
  }))
  entries <- c(
    stats::setNames(rep(list("A,B\n1,2\n"), 60), paste0(ids, ".csv")),
    stats::setNames(rep(list("x"), 2000), sprintf("x%04d.pdf", 1:2000))
  )
  z <- cont_zip(entries)
  orig <- DTAtools::matches_filename
  n <- 0
  local_mocked_bindings(matches_filename = function(x, ...) {
    n <<- n + 1
    orig(x, ...)
  })
  d <- ld(dta, z)
  expect_identical(nrow(rows_of(plan_of(d), "loaded")), 60L)
  # One claim per CSV against the 60 entries, one load_file() each (3,660 were
  # measured); without the name prefilter it is 123,720. The bound is on
  # calls, not on time.
  expect_lte(n, 60 * 60 + 60)
})

# 56.
test_that("refused and unreadable names with braces are data", {
  withr::local_envvar(DTA_PWNED = "")
  nm <- "{Sys.setenv(DTA_PWNED = '1')}.sas7bdat"
  dta <- dta_of(
    tab_ds("x", list(DTAFileSAS7BDAT(nm)), empty_specs()),
    tab_ds("dm", list(DTAFileCSV("dm.csv")))
  )
  z <- cont_zip(stats::setNames(list("not SAS\n", CONT_DM_CSV), c(nm, "dm.csv")))
  w <- expect_warning(ld(dta, z, on_error = "skip"), class = "dta_delivery_refused")
  expect_identical(Sys.getenv("DTA_PWNED"), "")
  expect_match(conditionMessage(w), "{Sys.setenv", fixed = TRUE)

  # Abort mode. The header only: the reader's own text, appended as "Caused by",
  # would carry the name whatever the header did with it.
  z2 <- cont_zip(list("{1+1}.sas7bdat" = "not SAS\n"))
  dta2 <- dta_of(tab_ds("x", list(DTAFileSAS7BDAT("{1+1}.sas7bdat")), empty_specs()))
  e <- expect_error(ld(dta2, z2), class = "dta_delivery_load_error")
  expect_match(conditionMessage(e), "{1+1}", fixed = TRUE)
  expect_match(cli::ansi_strip(rlang::cnd_header(e)), "{1+1}.sas7bdat", fixed = TRUE)

  dta3 <- dta_of(tab_ds("x", list(DTAFileSAS7BDAT(nm)), empty_specs()))
  e <- expect_error(
    ld(dta3, cont_zip(stats::setNames(list("not SAS\n"), nm))),
    class = "dta_delivery_load_error"
  )
  expect_identical(Sys.getenv("DTA_PWNED"), "")
  expect_match(cli::ansi_strip(rlang::cnd_header(e)), nm, fixed = TRUE)
})

# 57.
test_that("a catch-all tabular pattern never binds an archive or a library as a table", {
  dta <- dta_of(tab_ds(
    "all",
    list(DTAFileCSV(".*", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 100)),
    empty_specs()
  ))
  d <- ld(dta, cont_fx("sdtm_csv.zip"))
  plan <- plan_of(d)
  expect_identical(plan$status[plan$name == "sdtm_csv.zip"], "opened")
  expect_true(all(c("ae", "dm") %in% names(tables(datasets(d, "all")))))

  e <- expect_error(ld(dta, cont_fx("sdtm.xpt")), class = "dta_delivery_nothing_loaded")
  expect_identical(e$plan$status[e$plan$name == "sdtm.xpt"], "opened")
})

# 58.
test_that("an opened library is never also read whole", {
  dta <- dta_of(tab_ds(
    "x",
    list(DTAFileXPT("^.*\\.xpt$", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 10)),
    empty_specs()
  ))
  d <- ld(dta, cont_fx("sdtm.xpt"))
  plan <- plan_of(d)
  expect_setequal(rows_of(plan, "loaded")$name, c("ae.xpt", "dm.xpt", "vs.xpt"))
  expect_identical(plan$status[plan$name == "sdtm.xpt"], "opened")
})

# 59.
test_that("a gzip SAS dataset is judged by what it inflates to", {
  f <- system.file("qualification", "fx", "sas", "iris.sas7bdat", package = "DTAtools")
  expect_true(nzchar(f))
  expect_identical(file.size(f), 131072)
  gz <- file.path(withr::local_tempdir(), "iris.sas7bdat.gz")
  con <- gzfile(gz, "wb")
  writeBin(readBin(f, "raw", file.size(f)), con)
  close(con)
  dta <- dta_of(tab_ds("iris", list(DTAFileSAS7BDAT("iris.sas7bdat")), empty_specs()))
  withr::local_options(DTAtools.container_max_member_bytes = 1e5)
  e <- expect_error(ld(dta, gz), class = "dta_delivery_load_error")
  expect_s3_class(e$parent, "dta_container_too_large")
})

# 60.
test_that("a zip inside a zip is bound, never opened, even when an entry takes it", {
  dta <- dta_of(
    file_ds(
      "bundle",
      DTAFileAny(".*\\.zip$", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 10)
    ),
    tab_ds("ae", list(DTAFileCSV("ae.csv")))
  )
  d <- ld(dta, cont_fx("nested.zip"))
  plan <- plan_of(d)
  expect_identical(plan$status[plan$name == "inner.zip"], "loaded")
  expect_identical(n_tables(d, "ae"), 0L)
})

# 61.
test_that("an undeclared .xpt whose scan fails is unused, not fatal", {
  e <- expect_error(
    ld(cont_dta("xpt"), cont_cut("sdtm.xpt", 1300)),
    class = "dta_delivery_nothing_loaded"
  )
  row <- e$plan[e$plan$name == "sdtm.xpt", ]
  expect_identical(row$status, "unused")
  expect_match(row$reason, "truncated", fixed = TRUE)
})

# 62.
test_that("a library member over the cap is refused before it is split", {
  withr::local_options(DTAtools.container_max_member_bytes = 1000)
  ex <- withr::local_tempdir()
  e <- expect_error(
    suppressWarnings(ld(cont_dta("xpt"), cont_fx("sdtm.xpt"), on_error = "skip", exdir = ex)),
    class = "dta_delivery_nothing_loaded"
  )
  refused <- rows_of(e$plan, "refused")
  expect_identical(
    refused$reason[refused$name == "ae.xpt"],
    "is larger (1280 bytes) than DTAtools.container_max_member_bytes (1000)"
  )
  expect_true(all(is.na(refused$path)))
  expect_identical(
    list.files(ex, recursive = TRUE, all.files = TRUE, include.dirs = TRUE),
    character(0)
  )
})

# 63.
test_that("archive names with braces are data in every refusal", {
  src <- withr::local_tempdir()
  # The condition each archive is refused with (abort mode); in skip mode the
  # refusal is a row and nothing loads.
  refusals <- c(
    encrypted.zip = "dta_container_encrypted",
    dup_case.zip = "dta_container_duplicate_member",
    not_a_zip.zip = "dta_container_unreadable",
    truncated.zip = "dta_container_unreadable",
    bomb_ratio.zip = "dta_container_too_large",
    lying_size.zip = "dta_container_corrupt",
    unsafe_dotdot.zip = "dta_container_unsafe_member"
  )
  for (fx in names(refusals)) {
    braced <- file.path(src, paste0("{1+1}", fx))
    expect_true(file.copy(cont_fx(fx), braced), info = fx)
    e <- expect_error(ld(cont_dta("csv"), braced), class = refusals[[fx]], info = fx)
    expect_match(conditionMessage(e), "{1+1}", fixed = TRUE, info = fx)
    e <- expect_error(
      ld(cont_dta("csv"), braced, on_error = "skip"),
      class = "dta_delivery_nothing_loaded", info = fx
    )
    expect_match(conditionMessage(e), "{1+1}", fixed = TRUE, info = fx)
  }

  withr::local_options(DTAtools.container_max_bytes = 60)
  big <- file.path(src, "{1+1}s.zip")
  expect_true(file.copy(cont_fx("sdtm_csv.zip"), big))
  e <- expect_error(ld(cont_dta("csv"), big), class = "dta_container_too_large")
  expect_match(conditionMessage(e), "{1+1}", fixed = TRUE)
  expect_match(conditionMessage(e), "83 bytes against a limit of 60 bytes", fixed = TRUE)
})

# 64.
test_that("a pattern entry takes a gzip file its pattern matches without .gz, loose or inside a zip", {
  dta <- dta_of(tab_ds("ae", list(DTAFileCSV("^ae_.*\\.csv$", pattern = TRUE)), empty_specs()))
  g <- gz_text(CONT_AE_CSV, "ae_2025.csv.gz")

  loaded <- rows_of(plan_of(ld(dta, g)), "loaded")
  expect_identical(nrow(loaded), 1L)
  expect_identical(loaded$name, "ae_2025.csv.gz")
  expect_identical(loaded$item, "ae_2025")

  z <- cont_zip(list(ae_2025.csv.gz = readBin(g, "raw", file.size(g))))
  loaded <- rows_of(plan_of(ld(dta, z)), "loaded")
  expect_identical(nrow(loaded), 1L)
  expect_identical(loaded$name, "ae_2025.csv.gz")
  expect_identical(loaded$item, "ae_2025")
  expect_identical(loaded$container, "test.zip")
})

# --- Names that cannot be extracted here, unprobed libraries, kept deliverables

# 65.
test_that("a name that cannot be extracted here is skipped when nothing claims it", {
  phrases <- c(
    unsafe_colon.zip = "contains a colon",
    unsafe_reserved.zip = "uses a reserved Windows device name",
    unsafe_trailing_dot.zip = "has a path segment ending in a dot or a space",
    unsafe_control.zip = "contains a control character"
  )
  for (fx in names(phrases)) {
    ex <- withr::local_tempdir()
    e <- expect_error(
      ld(cont_dta("csv"), cont_fx(fx), exdir = file.path(ex, "x")),
      class = "dta_delivery_nothing_loaded",
      info = fx
    )
    unused <- rows_of(e$plan, "unused")
    expect_identical(nrow(unused), 1L, info = fx)
    expect_true(grepl(phrases[[fx]], unused$reason, fixed = TRUE), info = fx)
    expect_identical(list.files(ex, recursive = TRUE, all.files = TRUE), character(0), info = fx)
  }

  # A control character in a name prints as an escape, never raw.
  z <- swap_bytes(
    cont_zip(list("sdtm/ae.csv" = CONT_AE_CSV, "sdtm/a_b.txt" = "")),
    "a_b.txt", "a\tb.txt"
  )
  m <- expect_message(load_delivery(cont_dta("csv"), z), class = "dta_delivery_summary")
  plan <- attr(
    suppressMessages(load_delivery(cont_dta("csv"), z)),
    "last_delivery"
  )
  expect_identical(plan$status[plan$name == "ae.csv"], "loaded")
  tab <- plan[plan$name == "a\tb.txt", ]
  expect_identical(tab$status, "unused")
  expect_match(tab$reason, "contains a control character", fixed = TRUE)
  expect_match(conditionMessage(m), "a\\tb.txt", fixed = TRUE)
  expect_false(grepl("\t", conditionMessage(m), fixed = TRUE))

  # A Finder-style zip: the Finder's Icon file and the resource fork are system
  # files, whatever their names would need on disk.
  finder <- swap_bytes(
    cont_zip(list(
      "sdtm/ae.csv" = CONT_AE_CSV, "sdtm/define.xml" = "<define/>\n",
      "sdtm/IconX" = "", "__MACOSX/sdtm/._IconX" = ""
    )),
    "IconX", "Icon\r"
  )
  m <- expect_message(load_delivery(cont_dta("csv"), finder), class = "dta_delivery_summary")
  expect_false(grepl("\r", conditionMessage(m), fixed = TRUE))
  plan <- plan_of(ld(cont_dta("csv"), finder))
  expect_identical(plan$status[plan$name == "ae.csv"], "loaded")
  for (nm in c("Icon\r", "._Icon\r")) {
    row <- plan[plan$name == nm, ]
    expect_identical(nrow(row), 1L, info = encodeString(nm))
    expect_identical(row$status, "ignored", info = encodeString(nm))
    expect_identical(row$reason, "is a system file", info = encodeString(nm))
  }

  # A catch-all never claims a system file, so the archive is not refused for it.
  dta <- dta_of(
    tab_ds("ae", list(DTAFileCSV("ae.csv"))),
    file_ds("docs", DTAFileAny(".*", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 10))
  )
  expect_no_condition(d <- ld(dta, finder))
  plan <- plan_of(d)
  loaded <- rows_of(plan, "loaded")
  expect_identical(loaded$dataset[loaded$name == "ae.csv"], "ae")
  expect_identical(loaded$dataset[loaded$name == "define.xml"], "docs")
  expect_identical(plan$status[plan$name == "Icon\r"], "ignored")

  # A character Windows does not allow in file names.
  q <- swap_bytes(
    cont_zip(list("sdtm/ae.csv" = CONT_AE_CSV, "sdtm/Questions_.txt" = "")),
    "Questions_.txt", "Questions?.txt"
  )
  plan <- plan_of(ld(cont_dta("csv"), q))
  expect_identical(plan$status[plan$name == "ae.csv"], "loaded")
  note <- plan[plan$name == "Questions?.txt", ]
  expect_identical(note$status, "unused")
  expect_match(note$reason, "contains a character Windows does not allow in file names", fixed = TRUE)

  # An entry that cannot be extracted is never probed as a library, even when
  # a type: xpt entry found nothing.
  lib <- swap_bytes(
    cont_zip(list(ae.csv = CONT_AE_CSV, lib_x.xpt = cont_bytes("sdtm.xpt"))),
    "lib_x.xpt", "lib:x.xpt"
  )
  dta <- dta_of(
    tab_ds("ae", list(DTAFileCSV("ae.csv"))),
    tab_ds("dm", list(DTAFileXPT("dm.xpt")))
  )
  plan <- plan_of(ld(dta, lib))
  expect_identical(plan$status[plan$name == "ae.csv"], "loaded")
  colon <- plan[plan$name == "lib:x.xpt", ]
  expect_identical(colon$status, "unused")
  expect_match(colon$reason, "contains a colon", fixed = TRUE)
})

# 66.
test_that("a name that cannot be extracted here refuses the archive when claimed", {
  ex <- withr::local_tempdir()
  docs <- dta_of(file_ds(
    "docs",
    DTAFileAny(".*", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 10)
  ))
  e <- expect_error(
    ld(docs, cont_fx("unsafe_colon.zip"), exdir = file.path(ex, "x")),
    class = "dta_container_unsafe_member"
  )
  expect_identical(e$problems, "contains a colon")
  msg <- gsub("\\s+", " ", conditionMessage(e))
  expect_match(msg, "cannot write", fixed = TRUE)
  expect_false(grepl("would be unsafe", msg, fixed = TRUE))
  expect_identical(list.files(ex, recursive = TRUE, all.files = TRUE), character(0))

  q <- swap_bytes(
    cont_zip(list("sdtm/ae.csv" = CONT_AE_CSV, "sdtm/Questions_.txt" = "")),
    "Questions_.txt", "Questions?.txt"
  )
  dta <- dta_of(
    tab_ds("ae", list(DTAFileCSV("ae.csv"))),
    file_ds("docs", DTAFileAny("^Questions", pattern = TRUE, number_of_files = 1))
  )
  e <- expect_error(ld(dta, q), class = "dta_container_unsafe_member")
  expect_identical(e$members, "sdtm/Questions?.txt")
})

# 67.
test_that("a library beside named single-dataset files says why it was not opened", {
  z <- cont_zip(
    list(ae.xpt = cont_bytes("adverse.xpt"), supp.xpt = cont_bytes("sdtm_v8.xpt")),
    name = "p2.zip"
  )
  dta <- dta_of(
    tab_ds("ae", list(DTAFileXPT("ae.xpt")), empty_specs()),
    tab_ds("dm", list(DTAFileXPT("dm.xpt")), empty_specs())
  )
  plan <- plan_of(ld(dta, z))
  expect_identical(plan$status[plan$name == "ae.xpt"], "loaded")
  supp <- plan[plan$name == "supp.xpt", ]
  expect_identical(supp$status, "unused")
  expect_true(startsWith(
    supp$reason,
    "is a .xpt that was not opened: other .xpt files of p2.zip are named by the specification"
  ))
  expect_match(supp$reason, "(container: supp.xpt)", fixed = TRUE)

  # Declaring the library as the container of dm opens it.
  dta_decl <- dta_of(
    tab_ds("ae", list(DTAFileXPT("ae.xpt")), empty_specs()),
    tab_ds("dm", list(DTAFileXPT("dm.xpt", container = "supp.xpt")), empty_specs())
  )
  plan <- plan_of(ld(dta_decl, z))
  dm <- plan[plan$name == "dm.xpt", ]
  expect_identical(dm$status, "loaded")
  expect_identical(dm$container, "p2.zip > supp.xpt")

  # Every type: xpt entry satisfied: nothing to say about the library.
  dta_ae <- dta_of(tab_ds("ae", list(DTAFileXPT("ae.xpt")), empty_specs()))
  plan <- plan_of(ld(dta_ae, z))
  expect_identical(plan$reason[plan$name == "supp.xpt"], "matches no file entry of the specification")
})

# 68.
test_that("a deliverable kept closed says so", {
  dta <- dta_of(
    file_ds("archive", DTAFileAny("sdtm_csv.zip")),
    tab_ds("ae", list(DTAFileCSV("ae.csv")))
  )
  m <- expect_message(load_delivery(dta, cont_fx("sdtm_csv.zip")), class = "dta_delivery_summary")
  plan <- plan_of(ld(dta, cont_fx("sdtm_csv.zip")))
  expect_identical(rows_of(plan, "loaded")$reason, KEPT_SENTENCE)
  expect_identical(m$kept, "sdtm_csv.zip")
  expect_match(conditionMessage(m), "kept as one file", fixed = TRUE)

  # Declared as a container, the archive is opened and nothing is kept.
  dta_decl <- dta_of(
    file_ds("archive", DTAFileAny("sdtm_csv.zip")),
    tab_ds("ae", list(DTAFileCSV("ae.csv", container = "sdtm_csv.zip")))
  )
  plan <- plan_of(ld(dta_decl, cont_fx("sdtm_csv.zip")))
  expect_setequal(plan$status[plan$name == "sdtm_csv.zip"], c("loaded", "opened"))
  zip_loaded <- plan[plan$name == "sdtm_csv.zip" & plan$status == "loaded", ]
  expect_true(is.na(zip_loaded$reason))

  # A catch-all makes no deliverable: the archive is opened.
  dta_all <- dta_of(
    file_ds(
      "archive",
      DTAFileAny(".*", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 10)
    ),
    tab_ds("ae", list(DTAFileCSV("ae.csv")))
  )
  plan <- plan_of(ld(dta_all, cont_fx("sdtm_csv.zip")))
  expect_true("opened" %in% plan$status[plan$name == "sdtm_csv.zip"])
  expect_false(KEPT_SENTENCE %in% plan$reason)
})

# 69.
test_that("container directory names are cut by bytes", {
  cjk <- file.path(withr::local_tempdir(), paste0(strrep("\u4e2d", 83), "x.zip"))
  skip_if_not(
    isTRUE(suppressWarnings(file.copy(cont_fx("sdtm_csv.zip"), cjk))),
    "the temp directory cannot hold this name"
  )
  expect_identical(nchar(basename(cjk), type = "bytes"), 254L)
  d <- ld(cont_dta("csv"), cjk)
  loaded <- rows_of(plan_of(d), "loaded")
  expect_setequal(loaded$dataset, c("ae", "dm"))
  ae_path <- loaded$path[loaded$dataset == "ae"]
  expect_lte(nchar(basename(dirname(dirname(ae_path))), type = "bytes"), 104)
})

# --- Added after review -------------------------------------------------------

# 70.
test_that("a server path never reaches a refused reason, also when it holds spaces", {
  dta0 <- sas_and_csv_dta()
  root <- withr::local_tempdir()
  # cli wraps a message at its spaces, so a path with spaces comes back in
  # pieces. testthat switches the wrapping off (cli.condition_width = Inf).
  withr::local_options(cli.condition_width = 60)

  z <- cont_zip(list(ae.sas7bdat = "not a SAS dataset\n", dm.csv = CONT_DM_CSV))
  spaced <- file.path(root, "a dir with several spaces in its name to force cli to wrap it", "x")
  expect_warning(
    d <- ld(dta0, z, on_error = "skip", exdir = spaced),
    class = "dta_delivery_refused"
  )
  row <- plan_of(d)[plan_of(d)$name == "ae.sas7bdat", ]
  expect_identical(row$status, "refused")
  expect_false(grepl("several spaces", row$reason, fixed = TRUE))
  expect_false(grepl(basename(root), row$reason, fixed = TRUE))
  expect_true(grepl("test.zip > ae.sas7bdat", row$reason, fixed = TRUE))

  # A loose file in a directory with spaces.
  loose_dir <- file.path(root, "another dir with quite a few spaces in its name to force wrapping")
  dir.create(loose_dir)
  bad <- file.path(loose_dir, "ae.sas7bdat")
  writeBin(charToRaw("not a SAS dataset\n"), bad)
  files <- c(bad, cont_write("dm.csv", CONT_DM_CSV))
  expect_warning(d <- ld(dta0, files, on_error = "skip"), class = "dta_delivery_refused")
  row <- plan_of(d)[plan_of(d)$name == "ae.sas7bdat", ]
  expect_identical(row$status, "refused")
  expect_false(grepl("quite a few", row$reason, fixed = TRUE))
  expect_false(grepl(basename(root), row$reason, fixed = TRUE))
})

# 71.
test_that("a relative exdir leaves no working directory in a refused reason", {
  dta0 <- sas_and_csv_dta()
  z <- cont_zip(list(ae.sas7bdat = "not a SAS dataset\n", dm.csv = CONT_DM_CSV))
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  withr::local_options(cli.condition_width = 60)
  expect_warning(
    d <- ld(dta0, z, on_error = "skip", exdir = "rel"),
    class = "dta_delivery_refused"
  )
  expect_true(dir.exists("rel"))
  row <- plan_of(d)[plan_of(d)$name == "ae.sas7bdat", ]
  expect_identical(row$status, "refused")
  expect_false(grepl(normalizePath(tmp, winslash = "/"), row$reason, fixed = TRUE))
  expect_false(grepl(normalizePath(tmp, winslash = "\\"), row$reason, fixed = TRUE))
  expect_true(grepl("test.zip > ae.sas7bdat", row$reason, fixed = TRUE))
})

# 72.
test_that("an exdir that is not one directory path is refused before anything is created", {
  z <- cont_fx("sdtm_csv.zip")
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  for (bad in list(NA_character_, NA, 5, character(0), c("a", "b"), "")) {
    expect_error(
      load_delivery(cont_dta("csv"), z, exdir = bad),
      class = "dta_delivery_bad_input", info = deparse(bad)
    )
  }
  expect_error(
    load_delivery(datasets(cont_dta("csv"), "ae"), z, exdir = NA_character_),
    class = "dta_delivery_bad_input"
  )
  expect_identical(
    list.files(tmp, all.files = TRUE, recursive = TRUE, include.dirs = TRUE, no.. = TRUE),
    character(0)
  )
})

# 73.
test_that("a delivery of only system files gives no empty bullet", {
  z <- cont_zip(list(.DS_Store = "x"), name = "junk.zip")
  e <- expect_error(ld(cont_dta("csv"), z), class = "dta_delivery_nothing_loaded")
  expect_identical(rows_of(e$plan, "ignored")$name, ".DS_Store")
  expect_false(grepl(": *$", conditionMessage(e)))
})

# 74.
test_that("an unreadable .xpt that a catch-all entry takes is bound, and only its opening failed", {
  docs <- dta_of(file_ds(
    "docs",
    DTAFileAny(".*", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 10)
  ))
  notes <- cont_cut("cport.xpt", 160, "notes.xpt")

  # Skip mode: like an unreadable .zip, the file stays bound by its entry.
  expect_no_warning(d <- ld(docs, notes, on_error = "skip"))
  plan <- plan_of(d)
  expect_setequal(plan$status, c("loaded", "opened"))
  expect_identical(plan$dataset[plan$status == "loaded"], "docs")
  expect_true(startsWith(plan$reason[plan$status == "opened"], "could not be opened: "))
  expect_identical(basename(datasets(d, "docs")@file_paths), "notes.xpt")

  # Abort mode: the container's own condition.
  expect_error(ld(docs, notes), class = "dta_xpt_not_transport")
})

# 75.
test_that("a scan failure is the file's own problem when something wants the file", {
  cut <- cont_cut("sdtm.xpt", 1300)

  # An entry declares it as its container.
  declared <- cont_dta("xpt", container = "sdtm.xpt")
  expect_error(ld(declared, cut), class = "dta_container_corrupt")
  e <- expect_error(ld(declared, cut, on_error = "skip"), class = "dta_delivery_nothing_loaded")
  expect_identical(e$plan$status[e$plan$name == "sdtm.xpt"], "refused")

  # A pattern entry of type xpt claims it (a literal one would not scan it).
  claimed <- dta_of(tab_ds(
    "x",
    list(DTAFileXPT("^.*\\.xpt$", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 10)),
    empty_specs()
  ))
  expect_error(ld(claimed, cut), class = "dta_container_corrupt")
  e <- expect_error(ld(claimed, cut, on_error = "skip"), class = "dta_delivery_nothing_loaded")
  expect_identical(e$plan$status[e$plan$name == "sdtm.xpt"], "refused")
})

# 76.
test_that("a split is charged to the call by its bytes", {
  withr::local_options(DTAtools.container_max_bytes = 3000)
  e <- expect_error(ld(cont_dta("xpt"), cont_fx("sdtm.xpt")), class = "dta_container_too_large")
  expect_identical(e$limit, "DTAtools.container_max_bytes")
  expect_equal(e$value, 3760) # 1280 + 1120 + 1360
})

# 77.
test_that("a split is charged to the call by its files", {
  # lib_in_zip.zip: 1 file extracted, then 3 split; the library itself holds 3
  # members, so its own cap passes.
  withr::local_options(DTAtools.container_max_members = 3)
  e <- expect_error(ld(cont_dta("xpt"), cont_fx("lib_in_zip.zip")), class = "dta_container_too_large")
  expect_identical(e$limit, "DTAtools.container_max_members")
  expect_equal(e$value, 4)
})

# 78.
test_that("a refused container gives back the files it was charged", {
  withr::local_options(DTAtools.container_max_members = 2)
  files <- c(
    cont_fx("lying_size.zip"),
    cont_zip(list(ae.csv = CONT_AE_CSV), "z1.zip"),
    cont_zip(list(dm.csv = CONT_DM_CSV), "z2.zip")
  )
  expect_warning(
    d <- ld(cont_dta("csv"), files, on_error = "skip"),
    class = "dta_delivery_refused"
  )
  plan <- plan_of(d)
  expect_identical(plan$status[plan$name == "lying_size.zip"], "refused")
  expect_setequal(rows_of(plan, "loaded")$dataset, c("ae", "dm"))
})

# 79.
test_that("a library a limit stopped is refused when the specification has a type: xpt entry", {
  withr::local_options(DTAtools.container_max_members = 2) # sdtm.xpt holds 3 members
  e <- expect_error(ld(cont_dta("xpt"), cont_fx("sdtm.xpt")), class = "dta_container_too_large")
  expect_identical(e$limit, "DTAtools.container_max_members")
  e <- expect_error(
    ld(cont_dta("xpt"), cont_fx("sdtm.xpt"), on_error = "skip"),
    class = "dta_delivery_nothing_loaded"
  )
  expect_identical(e$plan$status[e$plan$name == "sdtm.xpt"], "refused")
})

# 80.
test_that("a library a limit stopped is only unused when no entry could want a library", {
  withr::local_options(DTAtools.container_max_members = 2)
  for (mode in c("abort", "skip")) {
    e <- expect_error(
      ld(cont_dta("csv"), cont_fx("sdtm.xpt"), on_error = mode),
      class = "dta_delivery_nothing_loaded", info = mode
    )
    row <- e$plan[e$plan$name == "sdtm.xpt", ]
    expect_identical(row$status, "unused", info = mode)
    expect_match(row$reason, "DTAtools.container_max_members", fixed = TRUE, info = mode)
  }
})

# 81.
test_that("names with control characters print as escapes in the messages", {
  # The entry of unsafe_control.zip is "ae<tab>x.csv". cli turns a raw tab into
  # a space, so the escape itself is what shows that the name was encoded.
  e <- expect_error(
    ld(cont_dta("csv"), cont_fx("unsafe_control.zip")),
    class = "dta_delivery_nothing_loaded"
  )
  expect_identical(e$plan$name[e$plan$status == "unused"], "ae\tx.csv")
  expect_match(conditionMessage(e), "ae\\tx.csv", fixed = TRUE)
  expect_false(grepl("[[:cntrl:]]", gsub("\n", "", conditionMessage(e))))

  # A specification name that the delivery lacks.
  dta <- dta_of(
    tab_ds("ae", list(DTAFileCSV("a\tb.csv", container = "sdtm_csv.zip")), empty_specs()),
    tab_ds("dm", list(DTAFileCSV("dm.csv")))
  )
  m <- expect_message(load_delivery(dta, cont_fx("sdtm_csv.zip")), class = "dta_delivery_summary")
  expect_identical(m$missing, "a\tb.csv")
  expect_match(conditionMessage(m), "a\\tb.csv", fixed = TRUE)
  expect_false(grepl("\t", conditionMessage(m), fixed = TRUE))

  # The sources of a duplicate.
  st <- new.env(parent = emptyenv())
  entry <- function(p) {
    list(
      cand = list(path = p, chain = character(), member = NA_character_),
      dataset = "ae", item = "ae"
    )
  }
  st$bind <- list(entry("x/a\rb.csv"), entry("y/a\rb.csv"))
  e <- expect_error(
    dta_delivery_resolve_duplicates(st, "abort", character()),
    class = "dta_delivery_duplicate_name"
  )
  expect_false(grepl("[[:cntrl:]]", gsub("\n", "", conditionMessage(e))))
})

# --- Review fixes (0.27.0) ------------------------------------------------------

test_that("a literal entry whose container the file missed still beats a free pattern elsewhere", {
  mk <- function() {
    dta_of(
      tab_ds("ae", list(DTAFileCSV("ae.csv", container = "sdtm.zip")), empty_specs()),
      tab_ds("listings", list(DTAFileCSV("[.]csv$", pattern = TRUE)), empty_specs())
    )
  }
  ae <- cont_write("ae.csv", CONT_AE_CSV)
  lb <- cont_write("lb.csv", "USUBJID,LBTEST
S-001,ALT
")
  d <- ld(mk(), c(ae, lb))
  plan <- plan_of(d)
  row <- plan[plan$name == "ae.csv", ]
  # Tier 3 (literal misfit) outranks tier 4 (pattern, free): ae takes it, loaded
  # and not conforming; listings gets neither the file nor a row.
  expect_identical(nrow(row), 1L)
  expect_identical(row$status, "loaded")
  expect_identical(row$dataset, "ae")
  expect_identical(row$conformant, FALSE)
  expect_identical(names(tables(datasets(d, "listings"))), "lb")
  expect_identical(names(tables(datasets(d, "ae"))), "ae")

  # The same name inside the container still goes to its dataset.
  z <- cont_zip(list("ae.csv" = CONT_AE_CSV), name = "sdtm.zip")
  d <- ld(mk(), c(z, lb))
  loaded <- rows_of(plan_of(d), "loaded")
  expect_identical(loaded$dataset[loaded$name == "ae.csv"], "ae")
  expect_identical(names(tables(datasets(d, "listings"))), "lb")
})

# Regression (round-2 review): holding a file back for a literal entry's
# container must not take it from a pattern entry whose OWN container holds it.
test_that("a literal entry blocked by its container leaves a pattern entry of the right container its file", {
  mk <- function() {
    dta_of(
      file_ds("sdtm_docs", DTAFileAny("define.xml", container = "sdtm.zip")),
      file_ds("adam_docs", DTAFileAny("[.]xml$", pattern = TRUE, container = "adam.zip"))
    )
  }
  sdtm <- cont_zip(list("define.xml" = "<sdtm/>"), name = "sdtm.zip")
  adam <- cont_zip(list("define.xml" = "<adam/>"), name = "adam.zip")
  d <- ld(mk(), c(sdtm, adam))
  loaded <- rows_of(plan_of(d), "loaded")
  expect_setequal(loaded$dataset[loaded$name == "define.xml"], c("sdtm_docs", "adam_docs"))
  expect_identical(nrow(rows_of(plan_of(d), "unused")), 0L)
  expect_true(attr(check(d, quiet = TRUE), "last_validation_ok"))

  # A loose copy is a misfit of the literal entry (tier 3) and the member of
  # sdtm.zip fits it: the conforming copy is kept, the loose one says so.
  loose <- cont_write("define.xml", "<loose/>")
  d <- ld(mk(), c(sdtm, adam, loose))
  plan <- plan_of(d)
  row <- plan[plan$name == "define.xml" & plan$status == "unused", ]
  expect_identical(nrow(row), 1L)
  expect_identical(row$reason, "another copy was loaded from sdtm.zip > define.xml")
})

# Regression (round-3 review): the dataset's first matching entry, without a
# container, hid a later entry of the same dataset whose container holds it.
test_that("the entry whose container holds the file takes it, whatever its position", {
  y <- cont_zip(list("ae.csv" = CONT_AE_CSV), name = "y.zip")
  for (order in list(c(1, 2), c(2, 1))) {
    pats <- list(
      DTAFileCSV("[.]csv$", pattern = TRUE),
      DTAFileCSV("[.]csv$", pattern = TRUE, container = "y.zip")
    )[order]
    dta <- dta_of(
      tab_ds("ae", list(DTAFileCSV("ae.csv", container = "x.zip")), empty_specs()),
      tab_ds("listings", pats, empty_specs())
    )
    loaded <- rows_of(plan_of(ld(dta, y)), "loaded")
    expect_identical(loaded$dataset, "listings", info = paste(order, collapse = ","))
    expect_identical(loaded$handler, as.integer(which(order == 2)), info = paste(order, collapse = ","))
  }
})

test_that("a library probed or declared inside a zip is held to the per-member cap", {
  lib <- cont_fx("sdtm.xpt")
  z <- cont_zip(list("lib.xpt" = readBin(lib, "raw", file.size(lib))))
  withr::local_options(DTAtools.container_max_member_bytes = 2000)
  for (container in list(NULL, "lib.xpt")) {
    dta <- dta_of(
      tab_ds("ae", list(DTAFileFactory(type = "xpt", filename = "ae.xpt", container = container))),
      tab_ds("dm", list(DTAFileFactory(type = "xpt", filename = "dm.xpt", container = container)))
    )
    e <- expect_error(ld(dta, z, exdir = withr::local_tempdir()), class = "dta_container_too_large")
    expect_identical(e$limit, "DTAtools.container_max_member_bytes")

    e <- expect_error(
      suppressWarnings(ld(dta, z, on_error = "skip", exdir = withr::local_tempdir())),
      class = "dta_delivery_nothing_loaded"
    )
    refused <- rows_of(e$plan, "refused")
    expect_identical(refused$name, "lib.xpt")
    expect_true(grepl("container_max_member_bytes", refused$reason, fixed = TRUE))
  }
})

test_that("a refused file is not reported as missing and keeps its dataset and entry", {
  dta <- dta_of(
    tab_ds("ae", list(DTAFileSAS7BDAT("ae.sas7bdat", container = "sdtm.zip")), empty_specs()),
    tab_ds("dm", list(DTAFileCSV("dm.csv", container = "sdtm.zip")), empty_specs())
  )
  z <- cont_zip(list("ae.sas7bdat" = "not a SAS dataset", "dm.csv" = CONT_DM_CSV), name = "sdtm.zip")
  msg <- NULL
  d <- withCallingHandlers(
    suppressWarnings(load_delivery(dta, z, on_error = "skip")),
    message = function(m) {
      if (inherits(m, "dta_delivery_summary")) msg <<- m
      invokeRestart("muffleMessage")
    }
  )
  expect_false("ae.sas7bdat" %in% msg$missing)
  r <- rows_of(plan_of(d), "refused")
  expect_identical(r$name, "ae.sas7bdat")
  expect_identical(r$dataset, "ae")
  expect_identical(r$handler, 1L)

  # A duplicate refusal names its dataset too.
  dup <- dta_of(tab_ds("ae", list(DTAFileCSV("[.]csv$", pattern = TRUE)), empty_specs()))
  dd <- suppressWarnings(ld(
    dup, c(cont_write("x.csv", CONT_AE_CSV), cont_write("x.csv", CONT_AE_CSV), cont_write("y.csv", CONT_AE_CSV)),
    on_error = "skip"
  ))
  dup_rows <- rows_of(plan_of(dd), "refused")
  expect_identical(nrow(dup_rows), 2L)
  expect_true(all(dup_rows$dataset == "ae" & dup_rows$handler == 1L))
})

test_that("a container name cut to a trailing dot or space still extracts", {
  mk <- function() dta_of(tab_ds("ae", list(DTAFileCSV("ae.csv")), empty_specs()))
  for (nm in c(paste0(strrep("x", 97), "   y.zip"), paste0(strrep("x", 97), "...y.zip"))) {
    z <- cont_zip(list("ae.csv" = CONT_AE_CSV), name = nm)
    d <- ld(mk(), z, exdir = withr::local_tempdir())
    loaded <- rows_of(plan_of(d), "loaded")
    expect_identical(nrow(loaded), 1L)
    expect_true(file.exists(loaded$path))
    expect_false(grepl("[. ]$", basename(dirname(dirname(loaded$path)))))
  }
})

# --- Containers as a ranking, ambiguity, route and datasets (container manager) ---

# The default container of the document: every entry without a reference of its
# own must arrive inside delivery.zip.
delivery_default <- function() DTAContainer("main", "delivery.zip", default = TRUE)

test_that("a default container is the requirement of every entry without its own", {
  d0 <- cont_dta("csv", containers = list(delivery_default()))
  loose <- cont_write("ae.csv", CONT_AE_CSV)
  row <- rows_of(plan_of(ld(d0, loose)), "loaded")
  expect_identical(row$conformant, FALSE)
  expect_match(row$reason, "requires it inside 'delivery.zip'", fixed = TRUE)

  inside <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "delivery.zip")
  row <- rows_of(plan_of(ld(d0, inside)), "loaded")
  expect_identical(row$conformant, TRUE)

  # `none` opts an entry out: no requirement, conformant is NA.
  none <- dta_of(tab_ds("ae", list(DTAFileCSV("ae.csv", container = "none"))))
  dd <- DTA(datasets = none@datasets, containers = list(delivery_default()))
  expect_identical(rows_of(plan_of(ld(dd, loose)), "loaded")$conformant, NA)
})

test_that("a type: any entry that names the default container takes the archive as conforming", {
  dta <- DTA(
    datasets = list(tab_ds("ae", list(DTAFileCSV("ae.csv"))), file_ds("pkg", DTAFileAny("delivery.zip"))),
    containers = list(delivery_default())
  )
  inside <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "delivery.zip")
  plan <- plan_of(ld(dta, inside))
  zip_row <- plan[plan$name == "delivery.zip" & plan$status == "loaded", ]
  expect_identical(zip_row$dataset, "pkg")
  expect_identical(zip_row$conformant, TRUE)
})

test_that("an archive is never a misfit of a type: any entry (default container plus an ADaM container)", {
  dta <- DTA(
    datasets = list(
      tab_ds("ae", list(DTAFileCSV("ae.csv"))),
      tab_ds("adsl", list(DTAFileCSV("adsl.csv", container = "adam.zip")), empty_specs()),
      file_ds("docs", DTAFileAny("[.]pdf$|[.]zip$", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 10))
    ),
    containers = list(delivery_default())
  )
  main <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "delivery.zip")
  adam <- cont_zip(list(adsl.csv = "USUBJID\nS-001\n"), name = "adam.zip")
  plan <- plan_of(ld(dta, c(main, adam)))
  expect_false(any(plan$conformant %in% FALSE))
  expect_setequal(rows_of(plan, "loaded")$dataset, c("ae", "adsl"))
})

# An old-shaped object: every DTAtools class without the newer properties.
ld_old_class <- function(cls, drop) {
  parent <- cls@parent
  parent_old <- if (inherits(parent, "S7_class") && !identical(parent@name, "S7_object")) {
    ld_old_class(parent, drop)
  } else {
    S7::S7_object
  }
  own <- setdiff(names(cls@properties), names(parent@properties))
  S7::new_class(cls@name,
    package = "DTAtools", parent = parent_old,
    properties = cls@properties[setdiff(own, drop)], validator = function(self) NULL
  )
}
ld_age <- function(obj, drop = c("containers", "provenance", "file_handlers")) {
  if (is.environment(obj) || is.data.frame(obj)) {
    return(obj)
  }
  if (inherits(obj, "S7_object")) {
    cur <- attr(obj, "S7_class", exact = TRUE)
    if (!identical(cur@package, "DTAtools")) {
      return(obj)
    }
    for (p in setdiff(names(attributes(obj)), c("class", "S7_class"))) {
      attr(obj, p) <- ld_age(attr(obj, p), drop)
    }
    for (p in intersect(drop, names(cur@properties))) attr(obj, p) <- NULL
    attr(obj, "S7_class") <- ld_old_class(cur, drop)
    return(obj)
  }
  if (is.list(obj)) {
    for (i in seq_along(obj)) if (!is.null(obj[[i]])) obj[[i]] <- ld_age(obj[[i]], drop)
  }
  obj
}
ld_roundtrip <- function(x) {
  f <- withr::local_tempfile(fileext = ".rds", .local_envir = parent.frame())
  saveRDS(x, f)
  readRDS(f)
}

test_that("a stale (old-shaped) DTA is upgraded by load_delivery()", {
  old <- ld_roundtrip(ld_age(cont_dta("csv")))
  expect_true(dta_is_stale(old))
  d <- ld(old, cont_fx("sdtm_csv.zip"))
  expect_setequal(rows_of(plan_of(d), "loaded")$dataset, c("ae", "dm"))
  expect_identical(dta_provenance(datasets(d, "ae"))$ae$container, "sdtm_csv.zip")
})

# --- Tiers ---------------------------------------------------------------------

test_that("tier 1 (literal, free) beats tier 2 (pattern, fits) in another dataset", {
  dta <- dta_of(
    tab_ds("ae", list(DTAFileCSV("ae.csv"))),
    tab_ds("dm", list(DTAFileCSV("[.]csv$", pattern = TRUE, container = "sdtm_csv.zip")))
  )
  loaded <- rows_of(plan_of(ld(dta, cont_fx("sdtm_csv.zip"))), "loaded")
  expect_identical(loaded$dataset[loaded$name == "ae.csv"], "ae")
  expect_identical(loaded$dataset[loaded$name == "dm.csv"], "dm")
})

test_that("tier 1 beats tier 3 across datasets: the misfit dataset gets neither file nor row", {
  dta <- dta_of(
    tab_ds("ae", list(DTAFileCSV("ae.csv"))),
    tab_ds("dm", list(DTAFileCSV("ae.csv", container = "other.zip")))
  )
  d <- ld(dta, cont_write("ae.csv", CONT_AE_CSV))
  plan <- plan_of(d)
  expect_identical(plan$dataset, "ae")
  expect_identical(n_tables(d, "dm"), 0L)
})

test_that("literal entries of the same tier in two datasets both take the file", {
  dta <- dta_of(
    tab_ds("ae", list(DTAFileCSV("ae.csv", container = "other.zip"))),
    tab_ds("dm", list(DTAFileCSV("ae.csv", container = "other.zip")), cont_specs("ae"))
  )
  loaded <- rows_of(plan_of(ld(dta, cont_write("ae.csv", CONT_AE_CSV))), "loaded")
  expect_setequal(loaded$dataset, c("ae", "dm"))
})

test_that("a pattern that fits beats a free pattern elsewhere (S3): not ambiguous", {
  dta <- dta_of(
    tab_ds("ae", list(DTAFileCSV("^ae.*[.]csv$", pattern = TRUE, container = "sdtm_csv.zip"))),
    tab_ds("dm", list(DTAFileCSV("^[a-d].*[.]csv$", pattern = TRUE)))
  )
  plan <- plan_of(ld(dta, cont_fx("sdtm_csv.zip")))
  expect_identical(nrow(rows_of(plan, "ambiguous")), 0L)
  loaded <- rows_of(plan, "loaded")
  expect_identical(loaded$dataset[loaded$name == "ae.csv"], "ae")
  expect_identical(loaded$dataset[loaded$name == "dm.csv"], "dm")
})

test_that("within one dataset a fitting literal entry beats a free one (Q1)", {
  dta <- dta_of(tab_ds("ae", list(
    DTAFileCSV("ae.csv"),
    DTAFileCSV("ae.csv", container = "sdtm_csv.zip")
  )))
  loaded <- rows_of(plan_of(ld(dta, cont_fx("sdtm_csv.zip"))), "loaded")
  expect_identical(loaded$handler, 2L)
  expect_identical(loaded$conformant, TRUE)
})

# --- Misfit duplicates ----------------------------------------------------------

test_that("several copies that all arrived outside the container are not loaded", {
  declared <- cont_dta("csv", container = "sdtm_csv.zip")
  loose <- cont_write("ae.csv", CONT_AE_CSV)
  other <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "other.zip")
  # Abort mode (the default): neither an abort nor a refusal.
  d <- ld(declared, c(loose, other))
  plan <- plan_of(d)
  ae_rows <- plan[plan$name == "ae.csv", ]
  expect_identical(ae_rows$status, c("unused", "unused"))
  expect_identical(
    ae_rows$reason,
    rep("several copies arrived outside 'sdtm_csv.zip'; none was loaded", 2)
  )
  expect_identical(n_tables(d, "ae"), 0L)
  expect_identical(rows_of(plan, "loaded")$dataset, "dm")
})

# --- Ambiguity ------------------------------------------------------------------

# ae takes `^a.*[.]csv$`, dm takes `^.*e[.]csv$`: ae.csv is taken by both, de.csv
# by dm only.
tie_dta <- function() {
  dta_of(
    tab_ds("ae", list(DTAFileCSV("^a.*[.]csv$", pattern = TRUE))),
    tab_ds("dm", list(DTAFileCSV("^.*e[.]csv$", pattern = TRUE)), cont_specs("ae"))
  )
}
de_csv <- function(env = parent.frame()) cont_write("de.csv", CONT_AE_CSV, env = env)

test_that("a pattern tie is ambiguous: not loaded, candidates listed, no path", {
  e <- expect_error(ld(tie_dta(), cont_write("ae.csv", CONT_AE_CSV)), class = "dta_delivery_nothing_loaded")
  row <- e$plan
  expect_identical(row$status, "ambiguous")
  expect_identical(row$candidates[[1]], c("ae", "dm"))
  expect_identical(row$path, NA_character_)
  expect_identical(
    row$reason,
    "matches the file entries of datasets 'ae' and 'dm'; pass route = list(\"ae.csv\" = \"<dataset>\") to choose"
  )
})

test_that("ambiguity never aborts a delivery that loads something else", {
  d <- ld(tie_dta(), c(cont_write("ae.csv", CONT_AE_CSV), de_csv()))
  plan <- plan_of(d)
  expect_identical(plan$status[plan$name == "ae.csv"], "ambiguous")
  expect_identical(n_tables(d, "ae"), 0L)
  expect_identical(names(tables(datasets(d, "dm"))), "de")
  expect_identical(plan$candidates[plan$name == "de.csv"][[1]], character(0))
})

test_that("the summary names ambiguous and outside files", {
  m <- expect_message(
    load_delivery(tie_dta(), c(cont_write("ae.csv", CONT_AE_CSV), de_csv())),
    class = "dta_delivery_summary"
  )
  expect_identical(m$ambiguous, "ae.csv")
  expect_match(conditionMessage(m), "Matches several datasets, not loaded: ae.csv", fixed = TRUE)

  m <- expect_message(
    load_delivery(cont_dta("csv", container = "sdtm_csv.zip"), cont_write("ae.csv", CONT_AE_CSV)),
    class = "dta_delivery_summary"
  )
  expect_identical(m$outside, "ae.csv")
  expect_match(conditionMessage(m), "Not inside its required container: ae.csv (sdtm_csv.zip)", fixed = TRUE)
})

# --- route ----------------------------------------------------------------------

test_that("route chooses among the ambiguous datasets", {
  files <- c(cont_write("ae.csv", CONT_AE_CSV), de_csv())
  d <- ld(tie_dta(), files, route = list("ae.csv" = "dm"))
  expect_identical(n_tables(d, "ae"), 0L)
  expect_setequal(names(tables(datasets(d, "dm"))), c("ae", "de"))
  expect_identical(nrow(rows_of(plan_of(d), "ambiguous")), 0L)

  # A named character vector is the same thing.
  d <- ld(tie_dta(), files, route = c("ae.csv" = "ae"))
  expect_identical(names(tables(datasets(d, "ae"))), "ae")
})

three_way_dta <- function() {
  dta_of(
    tab_ds("ae", list(DTAFileCSV("^a.*[.]csv$", pattern = TRUE))),
    tab_ds("dm", list(DTAFileCSV("^.*e[.]csv$", pattern = TRUE)), cont_specs("ae")),
    tab_ds("vs", list(DTAFileCSV("^v.*[.]csv$", pattern = TRUE)))
  )
}

test_that("a route to a dataset the file is not ambiguous between refuses the whole file", {
  files <- c(cont_write("ae.csv", CONT_AE_CSV), de_csv())
  for (route in list(list("ae.csv" = "vs"), list("ae.csv" = c("dm", "vs")))) {
    d <- suppressWarnings(ld(three_way_dta(), files, route = route, on_error = "skip"))
    rows <- plan_of(d)[plan_of(d)$name == "ae.csv", ]
    expect_identical(nrow(rows), 1L)
    expect_identical(rows$status, "refused")
    expect_match(rows$reason, "route sends it to dataset 'vs', which is not one of the datasets it is ambiguous between ('ae' and 'dm')", fixed = TRUE)
    expect_identical(n_tables(d, "ae"), 0L)
    expect_false("ae" %in% names(tables(datasets(d, "dm"))))
    expect_error(ld(three_way_dta(), files, route = route), class = "dta_delivery_bad_input")
  }
})

test_that("a route to no dataset leaves the file unused", {
  d <- ld(tie_dta(), c(cont_write("ae.csv", CONT_AE_CSV), de_csv()), route = list("ae.csv" = character(0)))
  plan <- plan_of(d)
  rows <- plan[plan$name == "ae.csv", ]
  expect_identical(rows$status, "unused")
  expect_identical(rows$reason, "route sends it to no dataset")
  expect_identical(n_tables(d, "ae"), 0L)
})

test_that("an ambiguous archive routed to no dataset is not opened", {
  dta <- dta_of(
    file_ds("docs1", DTAFileAny("[.]zip$", pattern = TRUE, extensions = "zip", number_of_files = 1)),
    file_ds("docs2", DTAFileAny("[.]zip$", pattern = TRUE, extensions = "zip", number_of_files = 1)),
    tab_ds("ae", list(DTAFileCSV("ae.csv")))
  )
  z <- cont_fx("sdtm_csv.zip")
  ae <- cont_write("ae.csv", CONT_AE_CSV)
  expect_identical(plan_of(ld(dta, c(z, ae)))$status[1], "ambiguous")
  plan <- plan_of(ld(dta, c(z, ae), route = list("sdtm_csv.zip" = character(0))))
  expect_identical(plan$status[plan$name == "sdtm_csv.zip"], "unused")
  expect_false("opened" %in% plan$status)
})

test_that("a route for a file that was not ambiguous changes nothing and is listed", {
  m <- expect_message(
    d <- load_delivery(cont_dta("csv"), cont_write("ae.csv", CONT_AE_CSV), route = list("ae.csv" = "dm")),
    class = "dta_delivery_summary"
  )
  expect_identical(names(tables(datasets(d, "ae"))), "ae")
  expect_identical(n_tables(d, "dm"), 0L)
  expect_match(conditionMessage(m), "route names files that were not ambiguous: ae.csv", fixed = TRUE)
})

test_that("a malformed route or datasets argument is refused before anything is read", {
  f <- cont_write("ae.csv", CONT_AE_CSV)
  expect_error(ld(cont_dta("csv"), f, route = list("dm")), class = "dta_delivery_bad_input")
  expect_error(ld(cont_dta("csv"), f, route = list(a = "ae", a = "dm")), class = "dta_delivery_bad_input")
  expect_error(ld(cont_dta("csv"), f, route = list("ae.csv" = "nope")), class = "dta_delivery_bad_input")
  expect_error(ld(cont_dta("csv"), f, datasets = "nope"), class = "dta_delivery_bad_input")
})

test_that("a routed library is opened and its members are routed (S5r)", {
  dta <- dta_of(
    tab_ds("ae", list(DTAFileXPT("\\.xpt$", pattern = TRUE))),
    file_ds("docs", DTAFileAny("\\.xpt$", pattern = TRUE, number_of_files = 1, extensions = "xpt"))
  )
  lib <- cont_fx("sdtm.xpt")
  e <- expect_error(ld(dta, lib), class = "dta_delivery_nothing_loaded")
  expect_identical(e$plan$status, "ambiguous")

  # Nothing loads, so the members are reported through the error.
  e <- expect_error(ld(dta, lib, route = list("sdtm.xpt" = "ae")), class = "dta_delivery_nothing_loaded")
  plan <- e$plan
  expect_identical(plan$status[plan$name == "sdtm.xpt"], "opened")
  members <- rows_of(plan, "ambiguous")
  expect_identical(members$name, c("ae.xpt", "dm.xpt", "vs.xpt"))
  expect_identical(members$candidates[[1]], c("ae", "docs"))

  # Routing the members as well loads them.
  d <- ld(dta, lib, route = list("sdtm.xpt" = "ae", "ae.xpt" = "ae", "dm.xpt" = "ae", "vs.xpt" = "ae"))
  expect_setequal(names(tables(datasets(d, "ae"))), c("ae", "dm", "vs"))
})

# --- datasets -------------------------------------------------------------------

test_that("datasets limits what a call loads and extracts", {
  ex <- withr::local_tempdir()
  d <- ld(cont_dta("csv"), cont_fx("sdtm_csv.zip"), datasets = "ae", exdir = ex)
  expect_identical(n_tables(d, "ae"), 1L)
  expect_identical(n_tables(d, "dm"), 0L)
  plan <- plan_of(d)
  expect_identical(plan$reason[plan$name == "dm.csv"], "is for dataset 'dm', which this call does not load")
  expect_identical(plan$status[plan$name == "dm.csv"], "unused")
  expect_identical(basename(list.files(ex, recursive = TRUE)), "ae.csv")
})

test_that("a zip a type: any entry takes as a deliverable is still opened for the chosen datasets", {
  dta <- dta_of(
    tab_ds("ae", list(DTAFileCSV("ae.csv"))),
    file_ds("docs", DTAFileAny("sdtm_csv.zip"))
  )
  d <- ld(dta, cont_fx("sdtm_csv.zip"), datasets = "ae")
  expect_identical(plan_of(d)$status[plan_of(d)$name == "sdtm_csv.zip"], "opened")
  expect_identical(names(tables(datasets(d, "ae"))), "ae")
})

test_that("datasets reduces an ambiguous file to the one dataset left, and a route outside it is unused", {
  files <- c(cont_write("ae.csv", CONT_AE_CSV), de_csv())
  d <- ld(tie_dta(), files, datasets = "ae")
  expect_identical(names(tables(datasets(d, "ae"))), "ae")
  expect_identical(nrow(rows_of(plan_of(d), "ambiguous")), 0L)

  e <- expect_error(
    ld(tie_dta(), files, datasets = "ae", route = list("ae.csv" = "dm")),
    class = "dta_delivery_nothing_loaded"
  )
  row <- e$plan[e$plan$name == "ae.csv", ]
  expect_identical(row$status, "unused")
  expect_identical(row$reason, "is for dataset 'dm', which this call does not load")
})

test_that("of three ambiguous datasets, datasets keeps the chosen ones as candidates", {
  dta <- dta_of(
    tab_ds("ae", list(DTAFileCSV("^a.*[.]csv$", pattern = TRUE))),
    tab_ds("dm", list(DTAFileCSV("^.*e[.]csv$", pattern = TRUE)), cont_specs("ae")),
    tab_ds("vs", list(DTAFileCSV("^[a-z]+[.]csv$", pattern = TRUE)), cont_specs("ae"))
  )
  e <- expect_error(
    ld(dta, cont_write("ae.csv", CONT_AE_CSV), datasets = c("dm", "vs")),
    class = "dta_delivery_nothing_loaded"
  )
  row <- e$plan
  expect_identical(row$status, "ambiguous")
  expect_identical(row$candidates[[1]], c("dm", "vs"))
})

# --- Provenance -----------------------------------------------------------------

test_that("a delivery records how each item arrived", {
  d <- ld(cont_dta("csv"), cont_fx("sdtm_csv.zip"))
  expect_identical(
    dta_provenance(datasets(d, "ae"))$ae,
    list(file = "ae.csv", container = "sdtm_csv.zip", via = "delivery")
  )
  d <- ld(cont_dta("xpt"), cont_fx("lib_in_zip.zip"))
  expect_identical(dta_provenance(datasets(d, "ae"))$ae$container, c("lib_in_zip.zip", "sdtm.xpt"))
  expect_identical(dta_provenance(datasets(d, "ae"))$ae$file, "ae.xpt")

  # A file dataset is keyed by the path it was bound under.
  dta <- dta_of(file_ds("docs", DTAFileAny("define.xml")))
  d <- ld(dta, cont_fx("sdtm_csv.zip"))
  docs <- datasets(d, "docs")
  expect_identical(
    dta_provenance(docs)[[docs@file_paths]],
    list(file = "define.xml", container = "sdtm_csv.zip", via = "delivery")
  )
})

# --- Archives under a default container (WP3 review) ---------------------------

# N23: a loose deliverable archive of a type: any entry is a file, never a
# container, when no entry declares it as one.
n23_dta <- function() {
  DTA(
    datasets = list(file_ds("docs", DTAFileAny("report.zip")), tab_ds("ae", list(DTAFileCSV("ae.csv")))),
    containers = list(delivery_default())
  )
}

test_that("a loose deliverable archive stays closed under a default container (N23)", {
  report <- cont_zip(list(ae.csv = CONT_AE_CSV, x.pdf = "%PDF"), name = "report.zip")
  d <- ld(n23_dta(), report)
  plan <- plan_of(d)
  expect_false("opened" %in% plan$status)
  loaded <- rows_of(plan, "loaded")
  expect_identical(loaded$name, "report.zip")
  expect_identical(loaded$dataset, "docs")
  expect_identical(loaded$conformant, FALSE)
  # The misfit sentence, then the kept sentence after "; ".
  expect_match(loaded$reason, "requires it inside 'delivery.zip'", fixed = TRUE)
  expect_true(endsWith(loaded$reason, paste0("; ", KEPT_SENTENCE)))
  expect_identical(n_tables(d, "ae"), 0L)
  expect_false("ae.csv" %in% plan$name)
})

test_that("a deliverable matching a wildcard default glob is closed when its entry says none (N3)", {
  dta <- DTA(
    datasets = list(
      file_ds("docs", DTAFileAny("STUDY_define.zip", container = "none")),
      tab_ds("ae", list(DTAFileCSV("ae.csv")))
    ),
    containers = list(DTAContainer("study", "STUDY_*.zip", default = TRUE))
  )
  define <- cont_zip(list(ae.csv = CONT_AE_CSV, d.pdf = "%PDF"), name = "STUDY_define.zip")
  sdtm <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "STUDY_sdtm.zip")
  d <- ld(dta, c(define, sdtm))
  plan <- plan_of(d)
  expect_identical(plan$status[plan$name == "STUDY_define.zip"], "loaded")
  expect_identical(plan$dataset[plan$name == "STUDY_define.zip"], "docs")
  expect_identical(plan$status[plan$name == "STUDY_sdtm.zip"], "opened")
  ae <- plan[plan$name == "ae.csv", ]
  expect_identical(ae$status, "loaded")
  expect_identical(ae$container, "STUDY_sdtm.zip")
  expect_identical(n_tables(d, "ae"), 1L)
})

test_that("a type: any entry that names the default container itself still opens it", {
  dta <- DTA(
    datasets = list(tab_ds("ae", list(DTAFileCSV("ae.csv"))), file_ds("pkg", DTAFileAny("delivery.zip"))),
    containers = list(delivery_default())
  )
  plan <- plan_of(ld(dta, cont_zip(list(ae.csv = CONT_AE_CSV), name = "delivery.zip")))
  expect_true("opened" %in% plan$status[plan$name == "delivery.zip"])
  expect_identical(plan$container[plan$name == "ae.csv" & plan$status == "loaded"], "delivery.zip")
})

test_that("an optional entry is never reported as not in the delivery (N8)", {
  mk <- function(min) {
    DTA(
      datasets = list(
        tab_ds("ae", list(DTAFileCSV("ae.csv"))),
        file_ds("extra", DTAFileAny("^extra_.*[.]pdf$", pattern = TRUE, min_number_of_files = min, max_number_of_files = 5))
      ),
      containers = list(delivery_default())
    )
  }
  z <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "delivery.zip")
  m <- expect_message(load_delivery(mk(0), z), class = "dta_delivery_summary")
  expect_identical(m$missing, character(0))
  # A required entry of the same kind is still reported.
  m <- expect_message(load_delivery(mk(1), z), class = "dta_delivery_summary")
  expect_length(m$missing, 1L)
})

test_that("when the copy that fits fails to load, the best misfit copy is loaded (DUP)", {
  dta <- dta_of(
    tab_ds("ae", list(DTAFileSAS7BDAT("ae.sas7bdat", container = "sdtm.zip")), empty_specs()),
    tab_ds("dm", list(DTAFileCSV("dm.csv")), empty_specs())
  )
  sas <- readBin(
    system.file("qualification", "fx", "sas", "iris.sas7bdat", package = "DTAtools"), "raw", 131072
  )
  expect_gt(length(sas), 0L)
  good <- cont_write("ae.sas7bdat", sas)
  bad_zip <- cont_zip(list(ae.sas7bdat = "not SAS", dm.csv = CONT_DM_CSV), name = "sdtm.zip")
  d <- suppressWarnings(ld(dta, c(good, bad_zip), on_error = "skip"))
  plan <- plan_of(d)
  ae <- plan[plan$name == "ae.sas7bdat", ]
  expect_identical(ae$status[is.na(ae$container)], "loaded")
  expect_identical(ae$conformant[is.na(ae$container)], FALSE)
  expect_identical(ae$status[!is.na(ae$container)], "refused")
  expect_identical(n_tables(d, "ae"), 1L)
  # No row claims another copy was loaded from the zip that failed.
  expect_false(any(grepl("another copy was loaded from sdtm.zip", plan$reason, fixed = TRUE)))

  # When the copy that fits loads, the misfit copy names it.
  ok_zip <- cont_zip(list(ae.sas7bdat = sas, dm.csv = CONT_DM_CSV), name = "sdtm.zip")
  plan <- plan_of(ld(dta, c(good, ok_zip), on_error = "skip"))
  row <- plan[plan$name == "ae.sas7bdat" & plan$status == "unused", ]
  expect_identical(row$reason, "another copy was loaded from sdtm.zip > ae.sas7bdat")
})

test_that("a refused route keeps the opening decision of the un-routed delivery (N21b)", {
  dta <- dta_of(
    file_ds("d1", DTAFileAny(".*", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 10)),
    file_ds("d2", DTAFileAny("[.]zip$", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 10)),
    tab_ds("ae", list(DTAFileCSV("ae.csv")))
  )
  z <- cont_fx("sdtm_csv.zip")
  base <- plan_of(ld(dta, z, on_error = "skip"))
  expect_true("opened" %in% base$status) # ambiguous, and opened all the same
  plan <- plan_of(suppressWarnings(ld(dta, z, route = list("sdtm_csv.zip" = "ae"), on_error = "skip")))
  expect_true("refused" %in% plan$status[plan$name == "sdtm_csv.zip"])
  expect_true("opened" %in% plan$status[plan$name == "sdtm_csv.zip"])
  expect_true("ae.csv" %in% plan$name)
})

test_that("an empty datasets argument is refused", {
  expect_error(
    ld(cont_dta("csv"), cont_fx("sdtm_csv.zip"), datasets = character(0)),
    class = "dta_delivery_bad_input"
  )
})

# --- Cost -----------------------------------------------------------------------

test_that("container globs are compiled once per glob, not once per entry", {
  glob <- paste0("perf_", basename(tempfile("g")), ".zip") # a glob no earlier test cached
  ids <- sprintf("ds%02d", 1:60)
  dta <- DTA(datasets = lapply(ids, function(id) {
    tab_ds(id, list(DTAFileCSV(paste0(id, ".csv"), container = glob)), empty_specs())
  }))
  entries <- stats::setNames(
    rep(list("USUBJID\nS-001\n"), 2000),
    c(paste0(ids, ".csv"), sprintf("other%04d.txt", seq_len(1940)))
  )
  z <- cont_zip(entries, name = glob)
  real_match <- dta_glob_matches
  real_regex <- dta_container_regex
  n_match <- 0L
  n_regex <- 0L
  testthat::local_mocked_bindings(
    dta_glob_matches = function(...) {
      n_match <<- n_match + 1L
      real_match(...)
    },
    dta_container_regex = function(glob) {
      n_regex <<- n_regex + 1L
      real_regex(glob)
    }
  )
  d <- ld(dta, z)
  expect_identical(n_tables(d, "ds01"), 1L)
  expect_identical(n_tables(d, "ds60"), 1L)
  # About two tests per claimed name (its own and its chain), one per .xpt name
  # (none here) and one per slot; testing every entry against every slot would
  # be 2,000 x 60.
  expect_gt(n_match, 0L)
  expect_lte(n_match, 2L * 60L + 0L + 60L + 20L)
  # One distinct glob; a few more builds are tolerated, 2,000 entries are not.
  expect_gte(n_regex, 1L)
  expect_lte(n_regex, 1L + 3L)
})
