# The standalone HTML validation report. REQ-REPORT-001 .. REQ-REPORT-014.
#
# This document is meant to be opened on a validated system with no internet
# access, by someone who was not in the room when check() ran. That means two
# things have to be true at once: nothing in it may be fetched at render time,
# and it has to say what check() actually found. A report that silently drops
# a finding is worse than no report, because it reads as a clean bill of
# health.

# ---- fixtures -----------------------------------------------------------------

qrpt_specs <- function() {
  DTAColumnSpecCollection(columns = list(
    ID  = DTAColumnSpec(id = "ID", type = "SAS Char", length = 4, nullable = FALSE),
    SEX = DTAColumnSpec(id = "SEX", type = "SAS Char", length = 1, nullable = FALSE, values = c("M", "F"))
  ))
}

# A checked dataset with exactly `n_bad` rows, each failing the same SEX
# check, so the number of identical repeated messages is chosen by the
# caller rather than discovered after the fact.
qrpt_dataset <- function(dir, name, n_bad) {
  df <- data.frame(ID = sprintf("A%03d", seq_len(n_bad)), SEX = rep("X", n_bad), stringsAsFactors = FALSE)
  path <- file.path(dir, paste0(name, ".csv"))
  utils::write.csv(df, path, row.names = FALSE, na = "")
  ds <- DTADataSetTabular(
    name = name, specs = qrpt_specs(), files = list(DTAFileCSV(filename = paste0(name, ".csv")))
  )
  ds <- load_file(ds, file = path, handler_index = 1, stream = "never")
  check(ds, persist = FALSE, quiet = TRUE)
}

qrpt_clean_dataset <- function(dir, name) {
  path <- file.path(dir, paste0(name, ".csv"))
  utils::write.csv(
    data.frame(ID = c("A001", "A002"), SEX = c("M", "F"), stringsAsFactors = FALSE),
    path,
    row.names = FALSE, na = ""
  )
  ds <- DTADataSetTabular(
    name = name, specs = qrpt_specs(), files = list(DTAFileCSV(filename = paste0(name, ".csv")))
  )
  ds <- load_file(ds, file = path, handler_index = 1, stream = "never")
  check(ds, persist = FALSE, quiet = TRUE)
}

qrpt_unchecked_dataset <- function(dir, name) {
  path <- file.path(dir, paste0(name, ".csv"))
  utils::write.csv(
    data.frame(ID = c("A001", "A002"), SEX = c("M", "F"), stringsAsFactors = FALSE),
    path,
    row.names = FALSE, na = ""
  )
  ds <- DTADataSetTabular(
    name = name, specs = qrpt_specs(), files = list(DTAFileCSV(filename = paste0(name, ".csv")))
  )
  load_file(ds, file = path, handler_index = 1, stream = "never") # deliberately never check()ed
}

qrpt_raw <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

qrpt_tag_body <- function(html_text, tag) {
  pattern <- sprintf("(?s)<%s>(.*?)</%s>", tag, tag)
  m <- regmatches(html_text, regexpr(pattern, html_text, perl = TRUE))
  if (length(m) == 0) "" else sub(pattern, "\\1", m, perl = TRUE)
}

qrpt_doc_title <- function(path) {
  xml2::xml_text(xml2::xml_find_first(xml2::read_html(path), "//title"))
}

# The badge on one message's inspect panel: "columnspec"/"rule"/"import" for a
# properly resolved message, "unknown" for the generic degraded fallback.
qrpt_panel_badge <- function(html_text, id) {
  pat <- sprintf('id="inspect-panel-%d"', as.integer(id))
  start <- regexpr(pat, html_text, fixed = TRUE)
  if (start < 0) {
    return(NA_character_)
  }
  chunk <- substr(html_text, start, start + 3000)
  m <- regmatches(chunk, regexpr("inspect-badge-[a-z]+", chunk))
  if (length(m) == 0) NA_character_ else m
}

# ---- standalone ---------------------------------------------------------------

test_that("OQ-REPORT-001 | the report is fully self-contained, with no script, stylesheet or image fetched over the network | REQ-REPORT-001", {
  dir <- qa_tempdir()
  ds <- qrpt_clean_dataset(dir, "ok")
  dta <- DTA(datasets = list(ok = ds), metadata = create_example_DTAMetaData())
  out <- file.path(dir, "report.html")
  write_validation_report(dta, out, quiet = TRUE)

  info <- qa_html_report(out)
  qa_step("no script, link or image points at a URL", character(0), info$external_refs)

  raw <- qrpt_raw(out)
  qa_check("and nothing in the document reaches for http(s) at all", !grepl("https?://", raw))

  qa_check("the stylesheet is embedded inline, not linked", nchar(qrpt_tag_body(raw, "style")) > 100)
  qa_check("the script is embedded inline, not linked", nchar(qrpt_tag_body(raw, "script")) > 100)
})

# ---- the overview ---------------------------------------------------------------

test_that("OQ-REPORT-002 | the overview counts agree with results() | REQ-REPORT-002", {
  dir <- qa_tempdir()
  ds_ok <- qrpt_clean_dataset(dir, "ok")
  ds_bad <- qrpt_dataset(dir, "bad", n_bad = 1L)
  dta <- DTA(datasets = list(ok = ds_ok, bad = ds_bad), metadata = create_example_DTAMetaData())

  out <- file.path(dir, "report.html")
  write_validation_report(dta, out, quiet = TRUE)
  info <- qa_html_report(out)

  qa_step(
    "one dataset passed and one failed, exactly as the fixture was built",
    c(pass = 1L, fail = 1L, pending = 0L),
    info$counts[c("pass", "fail", "pending")]
  )

  res <- as.data.frame(results(dta))
  agree <- c(
    pass = sum(res$status == "validated"),
    fail = sum(res$status == "failed"),
    pending = sum(!res$status %in% c("validated", "failed"))
  )
  qa_step(
    "and those are exactly the counts results() itself reports",
    as.integer(agree),
    as.integer(info$counts[c("pass", "fail", "pending")])
  )
})

test_that("OQ-REPORT-003 | a metadata-only failure is visible in the overview, not just in the messages | REQ-REPORT-003", {
  dir <- qa_tempdir()
  ds <- qrpt_clean_dataset(dir, "clean")
  md <- DTAMetaData(title = "dirty meta", transmission = list(date_last_transfer = "2026-12-31 at the earliest"))
  dta <- DTA(datasets = list(clean = ds), metadata = md)

  qa_step("the fixture's metadata really does carry one import error", 1L, nrow(metadata_import_errors(metadata(dta))))
  qa_step("and its only dataset is clean", 0L, nrow(messages(datasets(dta)[["clean"]], as_tibble = FALSE)))

  out <- file.path(dir, "report.html")
  write_validation_report(dta, out, quiet = TRUE)
  info <- qa_html_report(out)

  # The overview is the first thing a reviewer reads and for many it is the
  # only thing. A headline of "everything passed" over a document whose
  # message table says otherwise invites exactly the wrong conclusion from a
  # report a quality unit signs. This was DEV-011 until the metadata axis was
  # given a row of its own.
  qa_step(
    "the dataset passed and the metadata failure is counted",
    c(pass = 1L, fail = 1L, pending = 0L),
    info$counts[c("pass", "fail", "pending")]
  )
  qa_step(
    "the per-target table names the metadata axis alongside the dataset",
    c("clean", "metadata"),
    sort(info$targets)
  )
  qa_step(
    "and the message row that reports it is still there",
    1L, info$n_messages
  )

  # A clean transfer must be untouched by this: the overview stays a statement
  # about its targets, with no metadata row invented to say nothing happened.
  clean_dta <- DTA(datasets = list(clean = qrpt_clean_dataset(dir, "clean2")), metadata = DTAMetaData(title = "clean meta"))
  clean_out <- file.path(dir, "clean.html")
  write_validation_report(clean_dta, clean_out, quiet = TRUE)
  clean_info <- qa_html_report(clean_out)
  qa_step(
    "a transfer with sound metadata reports only its targets",
    c(pass = 1L, fail = 0L, pending = 0L),
    clean_info$counts[c("pass", "fail", "pending")]
  )
  qa_step("with no metadata row in the table", "clean2", clean_info$targets)
})

# ---- messages and inspect panels -------------------------------------------------

test_that("OQ-REPORT-004 | every message row in the document agrees with messages(), including rows hidden by the repeat cap | REQ-REPORT-004", {
  dir <- qa_tempdir()
  ds <- qrpt_dataset(dir, "rep", n_bad = 7L)
  dta <- DTA(datasets = list(rep = ds), metadata = create_example_DTAMetaData())
  qa_step("the fixture produces exactly seven identical messages", 7L, nrow(messages(dta)))

  out <- file.path(dir, "report.html")
  write_validation_report(dta, out, quiet = TRUE) # default max_repeats = 5
  info <- qa_html_report(out)

  qa_step("the document carries exactly as many message rows as messages() does", nrow(messages(dta)), info$n_messages)

  doc <- xml2::read_html(out)
  extra <- xml2::xml_find_all(doc, "//tr[contains(@class,'msg-row-extra')]")
  qa_step("two of the seven are beyond the default cap of five, hidden rather than dropped", 2L, length(extra))
  qa_step(
    "each hidden row is genuinely hidden (display:none), not merely tagged as extra",
    rep("display:none;", 2), xml2::xml_attr(extra, "style")
  )
})

test_that("OQ-REPORT-005 | one inspect panel exists per message, keyed by its id | REQ-REPORT-005", {
  dir <- qa_tempdir()
  ds <- qrpt_dataset(dir, "rep", n_bad = 7L)
  dta <- DTA(datasets = list(rep = ds), metadata = create_example_DTAMetaData())
  msgs <- as.data.frame(messages(dta, as_tibble = FALSE))

  out <- file.path(dir, "report.html")
  write_validation_report(dta, out, quiet = TRUE)
  info <- qa_html_report(out)

  qa_step("one inspect panel exists per message, cap or no cap", nrow(msgs), info$n_inspect_panels)

  doc <- xml2::read_html(out)
  qa_check(
    "and every message id in messages() has its own panel element in the document",
    all(vapply(msgs$id, function(id) {
      length(xml2::xml_find_all(doc, sprintf("//*[@id='inspect-panel-%d']", id))) == 1
    }, logical(1)))
  )
})

test_that("OQ-REPORT-006 | a metadata import error degrades every inspect panel in the report, not only its own | REQ-REPORT-006", {
  # Pinned, not endorsed (see REQ-REPORT-006 notes). Two transfers, identical
  # except for their metadata: one clean, one carrying a single trailing-
  # residue date. Both carry exactly one columnspec message, wholly unrelated
  # to metadata.
  dir <- qa_tempdir()
  make_dta <- function(name, metadata) {
    ds <- qrpt_dataset(dir, name, n_bad = 1L)
    DTA(datasets = stats::setNames(list(ds), name), metadata = metadata)
  }

  dta_clean_meta <- make_dta("clean_meta", DTAMetaData(title = "clean"))
  dta_dirty_meta <- make_dta(
    "dirty_meta",
    DTAMetaData(title = "dirty", transmission = list(date_last_transfer = "2026-12-31 at the earliest"))
  )

  msgs_clean <- as.data.frame(messages(dta_clean_meta, as_tibble = FALSE))
  msgs_dirty <- as.data.frame(messages(dta_dirty_meta, as_tibble = FALSE))
  qa_step("the dirty transfer carries one extra message: the metadata one", 2L, nrow(msgs_dirty))

  id_clean <- msgs_clean$id[msgs_clean$source == "columnspec"][[1]]
  id_dirty <- msgs_dirty$id[msgs_dirty$source == "columnspec"][[1]]

  out_clean <- file.path(dir, "clean.html")
  write_validation_report(dta_clean_meta, out_clean, quiet = TRUE)
  out_dirty <- file.path(dir, "dirty.html")
  write_validation_report(dta_dirty_meta, out_dirty, quiet = TRUE)

  qa_step(
    "with clean metadata, the columnspec message gets its real badge",
    "inspect-badge-columnspec", qrpt_panel_badge(qrpt_raw(out_clean), id_clean)
  )
  qa_step(
    "the identical kind of columnspec message degrades to the generic fallback once metadata carries an import issue",
    "inspect-badge-unknown", qrpt_panel_badge(qrpt_raw(out_dirty), id_dirty)
  )
  qa_known_deviation(
    "DEV-012",
    identical(qrpt_panel_badge(qrpt_raw(out_clean), id_clean), "inspect-badge-columnspec") &&
      identical(qrpt_panel_badge(qrpt_raw(out_dirty), id_dirty), "inspect-badge-unknown")
  )
})

# ---- max_repeats ------------------------------------------------------------------

test_that("OQ-REPORT-007 | repeats beyond max_repeats are hidden behind a show-more toggle | REQ-REPORT-007", {
  dir <- qa_tempdir()
  ds <- qrpt_dataset(dir, "rep", n_bad = 7L)
  dta <- DTA(datasets = list(rep = ds), metadata = create_example_DTAMetaData())

  out <- file.path(dir, "report.html")
  write_validation_report(dta, out, max_repeats = 3, quiet = TRUE)
  doc <- xml2::read_html(out)

  visible <- xml2::xml_find_all(doc, "//tr[contains(@class,'msg-row') and not(contains(@class,'msg-row-extra'))]")
  extra <- xml2::xml_find_all(doc, "//tr[contains(@class,'msg-row-extra')]")
  more_btn <- xml2::xml_find_all(doc, "//button[contains(@class,'show-more-btn')]")

  qa_step("three of the seven stay visible by default", 3L, length(visible))
  qa_step("the remaining four are hidden, not dropped from the document", 4L, length(extra))
  qa_step("one show-more toggle appears for the one repeated group", 1L, length(more_btn))
  qa_step("the toggle names exactly how many more there are", "Show 4 more like this", xml2::xml_text(more_btn[[1]]))
})

test_that("OQ-REPORT-008 | max_repeats = NULL or Inf disables capping | REQ-REPORT-007", {
  dir <- qa_tempdir()
  ds <- qrpt_dataset(dir, "rep", n_bad = 7L)
  dta <- DTA(datasets = list(rep = ds), metadata = create_example_DTAMetaData())

  out_inf <- file.path(dir, "report_inf.html")
  write_validation_report(dta, out_inf, max_repeats = Inf, quiet = TRUE)
  info_inf <- qa_html_report(out_inf)
  qa_step("max_repeats = Inf keeps all seven rows visible", 7L, info_inf$n_messages)
  qa_step(
    "none of them are marked as hidden extras",
    0L, length(xml2::xml_find_all(xml2::read_html(out_inf), "//tr[contains(@class,'msg-row-extra')]"))
  )

  out_null <- file.path(dir, "report_null.html")
  write_validation_report(dta, out_null, max_repeats = NULL, quiet = TRUE)
  info_null <- qa_html_report(out_null)
  qa_step("max_repeats = NULL behaves the same way", 7L, info_null$n_messages)
  qa_step(
    "and likewise hides nothing",
    0L, length(xml2::xml_find_all(xml2::read_html(out_null), "//tr[contains(@class,'msg-row-extra')]"))
  )
})

test_that("OQ-REPORT-009 | max_repeats rejects a non-positive or fractional value | REQ-REPORT-007", {
  dir <- qa_tempdir()
  ds <- qrpt_dataset(dir, "rep", n_bad = 2L)
  dta <- DTA(datasets = list(rep = ds), metadata = create_example_DTAMetaData())

  for (bad_cap in list(0, -1, 2.5)) {
    e <- tryCatch(
      write_validation_report(dta, file.path(dir, "x.html"), max_repeats = bad_cap, quiet = TRUE),
      error = function(e) e
    )
    qa_check(
      sprintf("max_repeats = %s is rejected", bad_cap),
      inherits(e, "error") && grepl("must be a positive whole number", conditionMessage(e), fixed = TRUE)
    )
  }
})

# ---- several datasets, unchecked datasets, clean transfers -----------------------

test_that("OQ-REPORT-010 | several datasets each contribute one overview row and their own messages | REQ-REPORT-008", {
  dir <- qa_tempdir()
  ds1 <- qrpt_dataset(dir, "ds1", n_bad = 2L)
  ds2 <- qrpt_dataset(dir, "ds2", n_bad = 3L)
  dta <- DTA(datasets = list(ds1 = ds1, ds2 = ds2), metadata = create_example_DTAMetaData())

  out <- file.path(dir, "report.html")
  write_validation_report(dta, out, quiet = TRUE)
  doc <- xml2::read_html(out)

  overview_rows <- xml2::xml_find_all(doc, "//table[contains(@class,'report-summary-table')]//tr[@data-dataset]")
  qa_step("one overview row per dataset", c("ds1", "ds2"), sort(xml2::xml_attr(overview_rows, "data-dataset")))

  info <- qa_html_report(out)
  qa_step("their messages are combined in one table: two plus three", 5L, info$n_messages)

  msg_rows <- xml2::xml_find_all(doc, "//tr[contains(@class,'msg-row')]")
  qa_step(
    "and every dataset's own name is carried on its own message rows",
    c("ds1", "ds2"), sort(unique(xml2::xml_attr(msg_rows, "data-dataset")))
  )
})

test_that("OQ-REPORT-011 | a dataset that was never checked is reported pending, not omitted | REQ-REPORT-009", {
  dir <- qa_tempdir()
  ds_ok <- qrpt_clean_dataset(dir, "checked")
  ds_never <- qrpt_unchecked_dataset(dir, "never")
  dta <- DTA(datasets = list(checked = ds_ok, never = ds_never), metadata = create_example_DTAMetaData())

  out <- file.path(dir, "report.html")
  write_validation_report(dta, out, quiet = TRUE)
  doc <- xml2::read_html(out)

  overview_rows <- xml2::xml_find_all(doc, "//table[contains(@class,'report-summary-table')]//tr[@data-dataset]")
  qa_step(
    "both datasets appear in the overview - the unchecked one is not silently dropped",
    c("checked", "never"), sort(xml2::xml_attr(overview_rows, "data-dataset"))
  )

  never_row <- overview_rows[xml2::xml_attr(overview_rows, "data-dataset") == "never"][[1]]
  qa_step(
    "the never-checked dataset is reported pending - not failed, and not silently passed",
    "pending", xml2::xml_attr(never_row, "data-status")
  )

  info <- qa_html_report(out)
  qa_step(
    "the overview counts one pass and one pending, no failures",
    c(pass = 1L, fail = 0L, pending = 1L),
    info$counts[c("pass", "fail", "pending")]
  )
})

test_that("OQ-REPORT-012 | a clean transfer shows zero failures and no message rows | REQ-REPORT-010", {
  dir <- qa_tempdir()
  ds <- qrpt_clean_dataset(dir, "clean")
  dta <- DTA(datasets = list(clean = ds), metadata = create_example_DTAMetaData())
  qa_step("the fixture is genuinely clean", 0L, nrow(messages(dta)))

  out <- file.path(dir, "report.html")
  write_validation_report(dta, out, quiet = TRUE)
  info <- qa_html_report(out)

  qa_step("zero failed, one passed, none pending", c(pass = 1L, fail = 0L, pending = 0L), info$counts[c("pass", "fail", "pending")])
  qa_step("no message rows", 0L, info$n_messages)
  qa_step("no inspect panels either - there is nothing to inspect", 0L, info$n_inspect_panels)
})

# ---- file handling and its arguments ----------------------------------------------

test_that("OQ-REPORT-013 | an existing file is only replaced with overwrite = TRUE, and quiet suppresses only the success message | REQ-REPORT-011", {
  dir <- qa_tempdir()
  ds <- qrpt_clean_dataset(dir, "ok")
  dta <- DTA(datasets = list(ok = ds), metadata = create_example_DTAMetaData())
  out <- file.path(dir, "report.html")

  write_validation_report(dta, out, quiet = TRUE)
  qa_check("the file was written on the first call", file.exists(out))

  e <- tryCatch(write_validation_report(dta, out, quiet = TRUE), error = function(e) e)
  qa_check(
    "a second call without overwrite is rejected",
    inherits(e, "error") && grepl("already exists", conditionMessage(e), fixed = TRUE)
  )

  replaced <- tryCatch(write_validation_report(dta, out, overwrite = TRUE, quiet = TRUE), error = function(e) e)
  qa_check("overwrite = TRUE replaces it without error", !inherits(replaced, "error"))

  out2 <- file.path(dir, "report2.html")
  loud <- testthat::capture_messages(write_validation_report(dta, out2, quiet = FALSE))
  qa_step("quiet = FALSE emits exactly one success message", 1L, length(loud))
  qa_check("naming the file it wrote", grepl("Report saved to", loud[[1]], fixed = TRUE))

  out3 <- file.path(dir, "report3.html")
  silent <- testthat::capture_messages(write_validation_report(dta, out3, quiet = TRUE))
  qa_step("quiet = TRUE emits no message at all", 0L, length(silent))
  qa_check("but the file is written all the same - quiet silences reporting, not the write", file.exists(out3))
})

test_that("OQ-REPORT-014 | the title falls back from an explicit value to the metadata title to a fixed default | REQ-REPORT-012", {
  dir <- qa_tempdir()
  ds <- qrpt_clean_dataset(dir, "ok")
  dta_with_title <- DTA(datasets = list(ok = ds), metadata = DTAMetaData(title = "Metadata Title"))

  out1 <- file.path(dir, "r1.html")
  write_validation_report(dta_with_title, out1, title = "Explicit Title", quiet = TRUE)
  qa_step("an explicit title wins over the metadata title", "Explicit Title", qrpt_doc_title(out1))

  out2 <- file.path(dir, "r2.html")
  write_validation_report(dta_with_title, out2, quiet = TRUE)
  qa_step("with no explicit title, the metadata title is used", "Metadata Title", qrpt_doc_title(out2))

  out3 <- file.path(dir, "r3.html")
  write_validation_report(dta_with_title, out3, title = NA_character_, quiet = TRUE)
  qa_step("a title of NA is treated as not supplied, so the metadata title is used", "Metadata Title", qrpt_doc_title(out3))

  ds2 <- qrpt_clean_dataset(dir, "ok2")
  dta_no_title <- DTA(datasets = list(ok2 = ds2), metadata = DTAMetaData())
  out4 <- file.path(dir, "r4.html")
  write_validation_report(dta_no_title, out4, quiet = TRUE)
  qa_step(
    "with neither an explicit title nor a metadata title, the fixed default is used",
    "Validation Report", qrpt_doc_title(out4)
  )
})

test_that("OQ-REPORT-015 | the document header shows the generation timestamp | REQ-REPORT-013", {
  dir <- qa_tempdir()
  ds <- qrpt_clean_dataset(dir, "ok")
  dta <- DTA(datasets = list(ok = ds), metadata = create_example_DTAMetaData())
  out <- file.path(dir, "report.html")

  before <- as.numeric(Sys.time())
  write_validation_report(dta, out, quiet = TRUE)
  after <- as.numeric(Sys.time())

  meta_txt <- xml2::xml_text(xml2::xml_find_first(xml2::read_html(out), "//*[contains(@class,'report-meta')]"))
  stamp <- regmatches(meta_txt, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}", meta_txt))
  qa_check("the header carries a timestamp in the documented format", length(stamp) == 1 && nzchar(stamp))

  # Parsed with the session's own default timezone, matching the Sys.time()
  # call the report itself made - never a formatted month name, so this holds
  # regardless of the system locale.
  parsed <- as.numeric(as.POSIXct(stamp, format = "%Y-%m-%d %H:%M:%S"))
  qa_check(
    "and it is genuinely the moment of generation, not a placeholder",
    !is.na(parsed) && parsed >= (before - 2) && parsed <= (after + 2)
  )
})

test_that("OQ-REPORT-016 | x must be a DTA object, and title must be a scalar string or NULL | REQ-REPORT-014", {
  dir <- qa_tempdir()
  ds <- qrpt_clean_dataset(dir, "ok")

  e_x <- tryCatch(write_validation_report(ds, file.path(dir, "x.html"), quiet = TRUE), error = function(e) e)
  qa_check(
    "a dataset, not a DTA, is rejected",
    inherits(e_x, "error") && grepl("'x' must be a DTA object", conditionMessage(e_x), fixed = TRUE)
  )

  dta <- DTA(datasets = list(ok = ds), metadata = create_example_DTAMetaData())
  e_title <- tryCatch(
    write_validation_report(dta, file.path(dir, "t.html"), title = c("a", "b"), quiet = TRUE),
    error = function(e) e
  )
  qa_check(
    "a title of length two is rejected",
    inherits(e_title, "error") &&
      grepl("'title' must be a single character string, or NULL", conditionMessage(e_title), fixed = TRUE)
  )
})
