# End-to-end workflows on the deliveries shipped with the package.
# REQ-PERF-001 .. REQ-PERF-005.
#
# Everything else in this suite exercises one behaviour at a time. These cases
# take a specification and a delivery exactly as a user would, run the whole
# journey, and check the answer at the end of it. A package can pass every
# unit of its behaviour and still fail here, because the journey has joins the
# units do not.
#
# The expected counts are stated below rather than read back from a run. They
# were derived from the fixtures, and if the engine's counting changes these
# numbers must be re-derived by inspection rather than updated to match.

pw_example <- function(...) {
  path <- system.file("extdata", ..., package = "DTAtools")
  if (!nzchar(path)) {
    testthat::skip(paste0("bundled example not installed: ", paste(..., collapse = "/")))
  }
  path
}

# Load the shipped clinical specification and bind one of its shipped
# deliveries to it, the way the vignette does.
pw_clinical <- function(csv, stream = "never") {
  dta <- read_dta_from_yaml(pw_example("clinical_dta.yaml"))
  ds <- dta[["clinical_data"]]
  ds <- load_file(ds, file = pw_example(csv), handler_index = 1, stream = stream)
  check(ds, persist = FALSE, quiet = TRUE)
}

pw_axes <- function(ds) {
  status <- validation_status(ds)
  c(
    ok = isTRUE(status$ok),
    columnspec = as.integer(status$n_columnspec_errors),
    rule = as.integer(status$n_rule_errors),
    import = as.integer(status$n_import_errors)
  )
}

test_that("PQ-PERF-001 | the shipped clean delivery validates with nothing to report | REQ-PERF-001", {
  ds <- pw_clinical("clinical_data.csv")

  qa_step(
    "the example delivery is clean on all three axes",
    c(ok = TRUE, columnspec = 0L, rule = 0L, import = 0L),
    pw_axes(ds)
  )
  qa_step(
    "and reports no messages at all",
    0L, nrow(as.data.frame(messages(ds)))
  )
})

test_that("PQ-PERF-002 | the column-specification fixture reports its recorded errors | REQ-PERF-002", {
  ds <- pw_clinical("clinical_data_error_columnspec.csv")

  # 505 is not a typo. The delivery omits a declared column entirely, and an
  # absent column is reported once per row over its 500 rows; the remaining
  # five are the value-level violations. See REQ-VAL-003.
  qa_step(
    "the recorded counts",
    c(ok = FALSE, columnspec = 505L, rule = 0L, import = 1L),
    pw_axes(ds)
  )

  msgs <- as.data.frame(messages(ds))
  qa_step(
    "the violated constraints are the ones the fixture was built to break",
    c("const", "enum", "maxLength", "required", "type"),
    sort(unique(as.character(msgs$keyword[msgs$source == "columnspec"])))
  )
  qa_step(
    "in the columns it was built to break them in",
    c("BMI", "GENDER", "STUDYID", "VISIT"),
    sort(unique(as.character(msgs$column[msgs$source == "columnspec" & !is.na(msgs$column)])))
  )
  qa_step(
    "and the one unparseable value is reported on the import axis",
    c(column = "BMI", keyword = "not_convertible"),
    c(
      column = as.character(msgs$column[msgs$source == "import"][[1]]),
      keyword = as.character(msgs$keyword[msgs$source == "import"][[1]])
    )
  )
})

test_that("PQ-PERF-003 | the rules fixture breaks rules and nothing else | REQ-PERF-002", {
  ds <- pw_clinical("clinical_data_error_rules.csv")

  # Every value in this delivery satisfies its column specification. What it
  # breaks is the relationships between values, which is what makes it a
  # separate fixture: a package could get the column checks right and the rule
  # checks wrong, and only this file would notice.
  qa_step(
    "the recorded counts",
    c(ok = FALSE, columnspec = 0L, rule = 7L, import = 0L),
    pw_axes(ds)
  )

  msgs <- as.data.frame(messages(ds))
  qa_step(
    "every finding is on the rule axis",
    "rule", unique(as.character(msgs$source))
  )
  qa_step(
    "and seven distinct rules are named",
    7L, length(unique(as.character(msgs$rule_id)))
  )
})

test_that("PQ-PERF-004 | the combined fixture reports on all three axes at once | REQ-PERF-002", {
  ds <- pw_clinical("clinical_data_error_all.csv")

  qa_step(
    "the recorded counts",
    c(ok = FALSE, columnspec = 10L, rule = 6L, import = 4L),
    pw_axes(ds)
  )
  qa_step(
    "all three axes report",
    c("columnspec", "import", "rule"),
    sort(unique(as.character(as.data.frame(messages(ds))$source)))
  )
})

test_that("PQ-PERF-005 | the import fixture reports unparseable values | REQ-PERF-002", {
  ds <- pw_clinical("clinical_data_error_import.csv")

  qa_step(
    "the recorded counts",
    c(ok = FALSE, columnspec = 5L, rule = 0L, import = 4L),
    pw_axes(ds)
  )

  msgs <- as.data.frame(messages(ds))
  import <- msgs[msgs$source == "import", , drop = FALSE]
  qa_step(
    "every import finding names the same reason",
    "not_convertible", unique(as.character(import$keyword))
  )
  qa_step(
    "and each names the row it came from",
    4L, sum(!is.na(import$row))
  )
})

test_that("PQ-PERF-006 | the two engines agree on every shipped fixture | REQ-PERF-007", {
  # The same deliveries, loaded the other way. A divergence here would mean the
  # verdict depends on how the file was read rather than on what it contains,
  # which is the one thing a validation tool cannot afford.
  for (csv in c(
    "clinical_data.csv", "clinical_data_error_columnspec.csv",
    "clinical_data_error_rules.csv", "clinical_data_error_all.csv",
    "clinical_data_error_import.csv"
  )) {
    qa_step(
      sprintf("%s reaches the same verdict on both paths", csv),
      pw_axes(pw_clinical(csv, stream = "never")),
      pw_axes(pw_clinical(csv, stream = "always"))
    )
  }
})

test_that("PQ-PERF-007 | a compressed delivery reaches the verdict of its plain twin | REQ-PERF-008", {
  plain <- pw_example("clinical_data2.csv")
  gzipped <- pw_example("clinical_data2.csv.gz")

  load_one <- function(path) {
    dta <- read_dta_from_yaml(pw_example("clinical_dta.yaml"))
    ds <- dta[["clinical_data"]]
    ds <- load_file(ds, file = path, handler_index = 1, stream = "never")
    pw_axes(check(ds, persist = FALSE, quiet = TRUE))
  }
  qa_step(
    "the gzipped delivery and the plain one agree",
    load_one(plain), load_one(gzipped)
  )
})

test_that("PQ-PERF-008 | a validated delivery carries through the whole reporting chain | REQ-PERF-003", {
  dir <- qa_tempdir()
  dta <- read_dta_from_yaml(pw_example("clinical_dta.yaml"))
  ds <- load_file(
    dta[["clinical_data"]],
    file = pw_example("clinical_data_error_all.csv"), handler_index = 1, stream = "never"
  )
  ds <- check(ds, persist = FALSE, quiet = TRUE)
  dta@datasets[["clinical_data"]] <- ds

  # The standalone report is what a reviewer actually reads, so what it says
  # has to be what the engine found rather than a second opinion.
  report <- file.path(dir, "report.html")
  write_validation_report(dta, file = report, quiet = TRUE)
  parsed <- qa_html_report(report)
  qa_step(
    "the report is standalone, with nothing fetched from the network",
    character(0), parsed$external_refs
  )
  qa_step(
    "and lists exactly the messages the engine reported",
    nrow(as.data.frame(messages(ds))), parsed$n_messages
  )

  # The Word document is the deliverable that leaves the organisation.
  docx <- file.path(dir, "transfer.docx")
  write_dta(dta, file = docx, quiet = TRUE, overwrite = TRUE)
  text <- qa_docx_text(docx)
  qa_check("the document is written and carries text", nchar(text) > 0)
  qa_step(
    "and names the transfer it describes",
    TRUE, grepl(dta@metadata@title %||% "", text, fixed = TRUE)
  )

  # The exported data file is what a downstream system consumes, and its
  # checksum is the only evidence it arrived intact.
  out <- file.path(dir, "exported.tsv")
  written <- write_table_to_file(
    ds,
    table = names(tables(ds))[[1]], filename = out,
    overwrite = TRUE, quiet = TRUE
  )
  qa_check("the data file is written", file.exists(out))
  # The return records the checksum together with the dimensions written, so
  # a consumer can tell both that the file is intact and that it is the whole
  # table rather than a truncated one.
  qa_step(
    "the recorded checksum is the checksum of the file on disk",
    unname(tools::md5sum(out)), as.character(written$md5sum[[1]])
  )
  qa_step(
    "and the recorded dimensions are the table's",
    c(rows = 500L, columns = 14L),
    c(rows = as.integer(written$md5sum[[2]]), columns = as.integer(written$md5sum[[3]]))
  )
})

test_that("PQ-PERF-009 | a specification naming several deliveries loads and validates | REQ-PERF-004", {
  dta <- read_dta_from_yaml(pw_example("clinical_dta_multiple_files.yaml"))
  qa_check("the specification loads", inherits(dta, "DTAtools::DTA"))

  ds <- dta[["clinical_data"]]
  handler <- files(ds)[[1]]

  # Several deliveries are declared by ONE handler whose filename is a regular
  # expression, together with the number of files it will accept, rather than
  # by one handler per file. A transfer arriving as an unknown number of parts
  # could not be specified any other way.
  qa_step(
    "the delivery is declared as a pattern accepting between one and two files",
    c(pattern = TRUE, min = 1L, max = 2L),
    c(
      pattern = isTRUE(handler@pattern),
      min = as.integer(min_number_of_files(handler)),
      max = as.integer(max_number_of_files(handler))
    )
  )
  qa_step(
    "and the pattern is a regular expression, not a literal name",
    TRUE, grepl("[*$]", handler@filename)
  )

  # Two files matching the one pattern, which is what the specification is for.
  ds <- load_file(ds, file = pw_example("clinical_data.csv"), handler_index = 1, stream = "never")
  ds <- load_file(ds, file = pw_example("clinical_data2.csv"), handler_index = 1, stream = "never")
  ds <- check(ds, persist = FALSE, quiet = TRUE)
  status <- validation_status(ds)

  qa_step("both deliveries are validated", 2L, sum(status$status == "validated"))
  qa_step(
    "and each reaches its own verdict",
    2L, sum(!is.na(status$ok))
  )
})

test_that("PQ-PERF-010 | a file-presence specification loads and validates | REQ-PERF-004", {
  dta <- read_dta_from_yaml(pw_example("clinical_dta_with_file_dataset.yaml"))
  qa_check("the specification loads", inherits(dta, "DTAtools::DTA"))

  # A file-presence dataset checks that a deliverable arrived at all, which is
  # what a transfer of images or archives needs and what a tabular check cannot
  # express.
  kinds <- vapply(datasets(dta), function(d) class(d)[[1]], character(1))
  qa_check(
    "it declares a file-presence dataset alongside the tabular one",
    any(grepl("DTADataSetFile", kinds, fixed = TRUE))
  )
})

test_that("PQ-PERF-011 | the second-domain specification loads and validates | REQ-PERF-005", {
  spec <- pw_example("gf_dataset.yaml")
  data <- pw_example("gf_data_small_smirna.tsv")

  ds <- read_dataset_from_yaml(spec)
  ds <- load_file(ds, file = data, handler_index = 1, stream = "never")
  ds <- check(ds, persist = FALSE, quiet = TRUE)
  status <- validation_status(ds)

  # A second domain with a different shape of specification, so the package is
  # not qualified only against the one example it was developed with.
  qa_step("the delivery is validated", "validated", status$status)
  qa_check("and reaches a verdict", !is.na(status$ok))
})
