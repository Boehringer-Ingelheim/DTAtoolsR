# Packaging judged apart from content: container_findings(), the verdict of
# check() and the validation report. REQ-CONT-025.
#
# Expected values are written from the requirement and the container-manager
# plan (D4, section 4.8 and Appendix D), never captured from a run. Every
# document here requires its entries to arrive inside delivery.zip (the default
# container) unless it says otherwise; the content of the tables is valid, so a
# verdict that is not "passed" is the packaging's alone.
#
# LIM-019 (a table replaced by assigning `tables` keeps the old record) is
# pinned in OQ-CONT-050.

# ---- local helpers --------------------------------------------------------------

FN_COLS <- c("dataset", "item", "file", "container", "arrived", "status", "message")

fn_default <- function() DTAtools::DTAContainer("main", "delivery.zip", default = TRUE)

# One dataset, ae, whose entry must arrive inside delivery.zip.
fn_ae_only <- function() cont_doc(cont_csv_ds("ae"), containers = list(fn_default()))

# Datasets ae and dm, both inside delivery.zip.
fn_ae_dm <- function() cont_doc(cont_csv_ds("ae"), cont_csv_ds("dm"), containers = list(fn_default()))

# check() with its messages and its warning discarded; findings are in the attribute.
fn_check <- function(x, ...) suppressMessages(suppressWarnings(check(x, quiet = TRUE, persist = FALSE, ...)))

# The closing "Validation ..." line check() prints.
fn_summary_line <- function(x) {
  msgs <- character()
  withCallingHandlers(
    check(x, persist = FALSE),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) invokeRestart("muffleWarning")
  )
  line <- grep("Validation (FAILED|INCOMPLETE|PASSED)", msgs, value = TRUE)
  trimws(regmatches(line[[length(line)]], regexpr("Validation (FAILED|INCOMPLETE|PASSED).*$", line[[length(line)]])))
}

# Load a delivery without printing its summary.
fn_deliver <- function(dta, files, ...) suppressMessages(DTAtools::load_delivery(dta, files, ...))

# The same document with the table of `id` replaced by assigning `tables`.
fn_assign_table <- function(dta, id, name = id) {
  ds <- dta@datasets[[id]]
  ds@tables <- stats::setNames(list(arrow::arrow_table(USUBJID = "S-001", AETERM = "X", AESEQ = 1)), name)
  dta@datasets[[id]] <- ds
  dta
}

# ---- container_findings() ------------------------------------------------------------

test_that("OQ-CONT-050 | container_findings() reports a file loose or inside another container as outside, one read on its own as unverified, and a table with no record as unrecorded | REQ-CONT-025", {
  none <- DTAtools::container_findings(cont_dta("csv"))
  qa_step(
    "a document whose entries need no container has a zero-row frame of the seven character columns",
    list(cols = FN_COLS, rows = 0L, character = TRUE),
    list(cols = names(none), rows = nrow(none), character = all(vapply(none, is.character, logical(1))))
  )
  declared_empty <- DTAtools::container_findings(fn_ae_dm())
  qa_step("containers declared but nothing delivered give no row", 0L, nrow(declared_empty))
  qa_step("a value that is not a DTA is refused", TRUE, inherits(cont_catch(DTAtools::container_findings(list())), "error"))

  # outside: loose.
  loose <- DTAtools::container_findings(fn_deliver(fn_ae_dm(), cont_write("ae.csv", CONT_AE_CSV)))
  qa_step(
    "a file delivered loose is outside, naming the file, the container and how it arrived",
    list(
      dataset = "ae", item = "ae", file = "ae.csv", container = "delivery.zip", arrived = "loose", status = "outside",
      message = "ae.csv (dataset 'ae') arrived loose; the agreement requires it inside 'delivery.zip'."
    ),
    as.list(loose[, c("dataset", "item", "file", "container", "arrived", "status", "message")])
  )

  # outside: another container.
  other <- DTAtools::container_findings(fn_deliver(fn_ae_dm(), cont_zip(list(ae.csv = CONT_AE_CSV), name = "other.zip")))
  qa_step(
    "a file inside another archive is outside, naming that archive",
    list(
      arrived = "other.zip", status = "outside",
      message = "ae.csv (dataset 'ae') arrived inside 'other.zip'; the agreement requires it inside 'delivery.zip'."
    ),
    list(arrived = other$arrived, status = other$status, message = other$message)
  )
  shouting <- DTAtools::container_findings(fn_deliver(fn_ae_dm(), cont_zip(list(ae.csv = CONT_AE_CSV), name = "DELIVERY.ZIP")))
  qa_step(
    "an archive that differs only in letter case says so",
    "ae.csv (dataset 'ae') arrived inside 'DELIVERY.ZIP'; the agreement requires it inside 'delivery.zip' (letter case must match).",
    shouting$message
  )
  qa_step(
    "a file inside its container gives no row",
    0L,
    nrow(DTAtools::container_findings(fn_deliver(fn_ae_dm(), cont_zip(list(ae.csv = CONT_AE_CSV), name = "delivery.zip"))))
  )

  # unverified: read with load_file().
  own <- DTAtools::container_findings(load_file(fn_ae_dm(), dataset = "ae", file = cont_write("ae.csv", CONT_AE_CSV)))
  qa_step(
    "a file read on its own is unverified, and the message says its packaging is not verified",
    list(
      rows = 1L, file = "ae.csv", container = "delivery.zip", arrived = "on its own", status = "unverified",
      message = "ae.csv (dataset 'ae') was checked on its own; the agreement requires it inside 'delivery.zip', so its packaging is not verified."
    ),
    list(rows = nrow(own), file = own$file, container = own$container, arrived = own$arrived, status = own$status, message = own$message)
  )

  # unrecorded: a table bound by assigning `tables`.
  bare <- DTAtools::container_findings(fn_assign_table(fn_ae_dm(), "ae"))
  qa_step(
    "a table that was only assigned has no record: unrecorded, with no file and no arrival",
    list(
      rows = 1L, item = "ae", file = NA_character_, container = "delivery.zip", arrived = NA_character_, status = "unrecorded",
      message = "ae (dataset 'ae'): it is not recorded how this file arrived; load it with load_file() or load_delivery() to judge its container."
    ),
    list(rows = nrow(bare), item = bare$item, file = bare$file, container = bare$container, arrived = bare$arrived, status = bare$status, message = bare$message)
  )
  qa_step(
    "a table with no record but no entry that needs a container gives no row",
    0L,
    nrow(DTAtools::container_findings(fn_assign_table(cont_dta("csv"), "ae")))
  )
  # LIM-019: a table replaced by assigning `tables` keeps the record of the table it replaced.
  inside <- fn_deliver(fn_ae_dm(), cont_zip(list(ae.csv = CONT_AE_CSV), name = "delivery.zip"))
  qa_step(
    "LIM-019: replacing a recorded table by assignment keeps its record, so the new table still reads as inside",
    0L,
    nrow(DTAtools::container_findings(fn_assign_table(inside, "ae")))
  )

  # A file dataset is judged by the entry that took its file.
  docs <- cont_doc(cont_filedata("docs", DTAtools::DTAFileAny("define.pdf", container = "pkg.zip")))
  d <- DTAtools::container_findings(fn_deliver(docs, cont_write("define.pdf", "x")))
  qa_step(
    "a file dataset's file delivered loose is outside, under the entry's container",
    list(dataset = "docs", status = "outside", file = "define.pdf", container = "pkg.zip"),
    list(dataset = d$dataset, status = d$status, file = d$file, container = d$container)
  )
})

# ---- check() --------------------------------------------------------------------------

test_that("OQ-CONT-051 | check() fails a document with a file outside its container, leaves one with an unverified file incomplete, and never passes while a finding exists | REQ-CONT-025", {
  # Outside: FAILED, one warning of the documented classes, content untouched.
  out <- fn_deliver(fn_ae_only(), cont_write("ae.csv", CONT_AE_CSV))
  classes <- NULL
  withCallingHandlers(
    suppressMessages(check(out, quiet = TRUE, persist = FALSE)),
    warning = function(w) {
      classes <<- c(classes, list(class(w)))
      invokeRestart("muffleWarning")
    }
  )
  qa_step(
    "an outside file is signalled once, as dta_container_mismatch and dta_validation_finding",
    list(n = 1L, mismatch = TRUE, finding = TRUE),
    list(n = length(classes), mismatch = "dta_container_mismatch" %in% unlist(classes), finding = "dta_validation_finding" %in% unlist(classes))
  )
  checked <- fn_check(out)
  qa_step(
    "the verdict is not ok, the findings are in the attribute, and the content of the dataset is still valid",
    list(ok = FALSE, status = "outside", content = TRUE),
    list(
      ok = attr(checked, "last_validation_ok"), status = attr(checked, "last_container_findings")$status,
      content = all(validation_status(DTAtools::datasets(checked, "ae"))$ok)
    )
  )
  qa_step("and the closing line says so", "Validation FAILED: 1 file not inside the required container", fn_summary_line(out))

  # Unverified: INCOMPLETE.
  own <- load_file(fn_ae_only(), dataset = "ae", file = cont_write("ae.csv", CONT_AE_CSV))
  own_checked <- fn_check(own)
  qa_step(
    "a file read on its own leaves the verdict not ok, with the content valid",
    list(ok = FALSE, status = "unverified", content = TRUE),
    list(
      ok = attr(own_checked, "last_validation_ok"), status = attr(own_checked, "last_container_findings")$status,
      content = all(validation_status(DTAtools::datasets(own_checked, "ae"))$ok)
    )
  )
  qa_step("and the line says incomplete", "Validation INCOMPLETE: 1 file whose packaging is not verified", fn_summary_line(own))
  qa_step(
    "it is signalled as dta_container_mismatch too",
    TRUE,
    inherits(tryCatch(suppressMessages(check(own, quiet = TRUE, persist = FALSE)), warning = function(w) w), "dta_container_mismatch")
  )

  # Unrecorded: the same line.
  qa_step(
    "a table with no record is incomplete in the same words",
    "Validation INCOMPLETE: 1 file whose packaging is not verified",
    fn_summary_line(fn_assign_table(fn_ae_only(), "ae"))
  )

  # Both kinds, and the plural.
  mixed <- load_file(
    fn_deliver(fn_ae_dm(), cont_write("ae.csv", CONT_AE_CSV)),
    dataset = "dm", file = cont_write("dm.csv", CONT_DM_CSV)
  )
  qa_step(
    "an outside and an unverified file together fail the run and name both",
    "Validation FAILED: 1 file not inside the required container; 1 file whose packaging is not verified",
    fn_summary_line(mixed)
  )
  both_loose <- fn_deliver(fn_ae_dm(), c(cont_write("ae.csv", CONT_AE_CSV), cont_write("dm.csv", CONT_DM_CSV)))
  qa_step("two outside files are counted in the plural", "Validation FAILED: 2 files not inside the required container", fn_summary_line(both_loose))

  # Inside the container: nothing to report.
  ok_doc <- fn_deliver(fn_ae_only(), cont_zip(list(ae.csv = CONT_AE_CSV), name = "delivery.zip"))
  ok_checked <- fn_check(ok_doc)
  qa_step(
    "a file inside its container passes: ok, no finding, no warning",
    list(ok = TRUE, findings = 0L, warnings = 0L),
    list(
      ok = attr(ok_checked, "last_validation_ok"), findings = nrow(attr(ok_checked, "last_container_findings")),
      warnings = {
        n <- 0L
        withCallingHandlers(
          suppressMessages(check(ok_doc, quiet = TRUE, persist = FALSE)),
          warning = function(w) {
            n <<- n + 1L
            invokeRestart("muffleWarning")
          }
        )
        n
      }
    )
  )
  qa_check("and the closing line starts with Validation PASSED", startsWith(fn_summary_line(ok_doc), "Validation PASSED"), fn_summary_line(ok_doc))

  # A subset check keeps the findings of the datasets it did not check.
  sub <- load_file(fn_ae_dm(), dataset = "ae", file = cont_write("ae.csv", CONT_AE_CSV))
  sub <- fn_deliver(sub, cont_zip(list(dm.csv = CONT_DM_CSV), name = "delivery.zip"), datasets = "dm")
  sub_checked <- fn_check(sub, datasets = "dm")
  qa_step(
    "checking only dm still reports the unverified file of ae, and the run is not ok",
    list(dataset = "ae", status = "unverified", ok = FALSE),
    list(
      dataset = attr(sub_checked, "last_container_findings")$dataset, status = attr(sub_checked, "last_container_findings")$status,
      ok = attr(sub_checked, "last_validation_ok")
    )
  )
})

# ---- the validation report --------------------------------------------------------------

test_that("OQ-CONT-052 | the validation report shows each finding as a row and a message, and names the container each file arrived through | REQ-CONT-025", {
  d <- cont_doc(cont_csv_ds("ae"), cont_csv_ds("dm"), cont_csv_ds("vs"), containers = list(fn_default()))
  d <- fn_deliver(d, cont_write("ae.csv", CONT_AE_CSV)) # outside
  d <- load_file(d, dataset = "dm", file = cont_write("dm.csv", CONT_DM_CSV)) # unverified
  ds <- d@datasets[["vs"]] # unrecorded: a table that is only assigned
  ds@tables <- list(vs = arrow::arrow_table(USUBJID = "S-001", VSTESTCD = "SBP", VSSTRESN = 120, VSDT = as.Date("2020-01-01")))
  d@datasets[["vs"]] <- ds
  x <- fn_check(d)
  qa_step(
    "the fixture has one finding of each kind",
    c("outside", "unrecorded", "unverified"),
    sort(attr(x, "last_container_findings")$status)
  )

  out <- file.path(qa_tempdir(), "report.html")
  write_validation_report(x, out, quiet = TRUE)
  info <- qa_html_report(out)
  rows <- stats::setNames(info$target_statuses, info$targets)
  qa_step(
    "each finding is a row of the overview: failed for the outside file, pending for the two not verified",
    c("container: ae.csv" = "fail", "container: dm.csv" = "pending", "container: vs" = "pending"),
    rows[startsWith(names(rows), "container")]
  )
  text <- paste(unlist(info$messages), collapse = "\n")
  qa_check("the outside file's message is in the report", grepl("ae.csv (dataset 'ae') arrived loose; the agreement requires it inside 'delivery.zip'.", text, fixed = TRUE), text)
  qa_check("so is the unverified file's", grepl("dm.csv (dataset 'dm') was checked on its own", text, fixed = TRUE), text)
  qa_check("and the unrecorded table's", grepl("vs (dataset 'vs'): it is not recorded how this file arrived", text, fixed = TRUE), text)

  doc <- xml2::read_html(out)
  headers <- xml2::xml_text(xml2::xml_find_all(doc, "//th"), trim = TRUE)
  cells <- xml2::xml_text(xml2::xml_find_all(doc, "//td"), trim = TRUE)
  qa_check("the inputs table has a Container column", "Container" %in% headers, headers)
  qa_step(
    "and says loose, on its own and not recorded for the three files",
    c(TRUE, TRUE, TRUE),
    c("loose" %in% cells, "on its own" %in% cells, "not recorded" %in% cells)
  )

  # An archive is named as the container.
  z <- fn_check(fn_deliver(fn_ae_only(), cont_zip(list(ae.csv = CONT_AE_CSV), name = "delivery.zip"), stream = "always"))
  out_z <- file.path(qa_tempdir(), "report-zip.html")
  write_validation_report(z, out_z, quiet = TRUE)
  cells_z <- xml2::xml_text(xml2::xml_find_all(xml2::read_html(out_z), "//td"), trim = TRUE)
  qa_check("a file that arrived in delivery.zip is reported with that container", "delivery.zip" %in% cells_z, cells_z)
  qa_step(
    "and no container row is added for it",
    0L,
    sum(startsWith(qa_html_report(out_z)$targets, "container"))
  )
})
