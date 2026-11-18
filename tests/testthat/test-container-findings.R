# container_findings(): packaging is judged apart from content (plan D4, 4.8).

cf_cols <- c("dataset", "item", "file", "container", "arrived", "status", "message")
cf_default <- function() DTAContainer("main", "delivery.zip", default = TRUE)
# ae, dm, vs; every entry must arrive inside delivery.zip.
cf_dta <- function() cont_dta("csv", containers = list(cf_default()))
# One dataset only, so a check has nothing else outstanding.
cf_ae_only <- function() {
  DTA(datasets = list(tab_ds("ae", list(DTAFileCSV("ae.csv")))), containers = list(cf_default()))
}
cf_check <- function(x, ...) {
  suppressMessages(suppressWarnings(check(x, quiet = TRUE, persist = FALSE, ...)))
}
test_that("a document without findings gives a zero-row frame of character columns", {
  f <- container_findings(cont_dta("csv"))
  expect_identical(names(f), cf_cols)
  expect_identical(nrow(f), 0L)
  expect_true(all(vapply(f, is.character, logical(1))))
  # Containers declared but nothing delivered: still nothing to report.
  f2 <- container_findings(cf_dta())
  expect_identical(names(f2), cf_cols)
  expect_identical(nrow(f2), 0L)
})

test_that("container_findings() needs a DTA", {
  expect_error(container_findings(list()), "must be a")
})

test_that("a loose delivery is outside", {
  d <- ld(cf_dta(), cont_write("ae.csv", CONT_AE_CSV))
  f <- container_findings(d)
  expect_identical(names(f), cf_cols)
  expect_identical(nrow(f), 1L)
  expect_identical(f$dataset, "ae")
  expect_identical(f$item, "ae")
  expect_identical(f$file, "ae.csv")
  expect_identical(f$container, "delivery.zip")
  expect_identical(f$arrived, "loose")
  expect_identical(f$status, "outside")
  expect_identical(
    f$message,
    "ae.csv (dataset 'ae') arrived loose; the agreement requires it inside 'delivery.zip'."
  )
})

test_that("a file inside another container is outside and names the chain", {
  z <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "other.zip")
  f <- container_findings(ld(cf_dta(), z))
  expect_identical(f$status, "outside")
  expect_identical(f$arrived, "other.zip")
  expect_identical(
    f$message,
    "ae.csv (dataset 'ae') arrived inside 'other.zip'; the agreement requires it inside 'delivery.zip'."
  )
})

test_that("a container that differs only in letter case says so", {
  z <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "DELIVERY.ZIP")
  f <- container_findings(ld(cf_dta(), z))
  expect_identical(f$status, "outside")
  expect_identical(
    f$message,
    "ae.csv (dataset 'ae') arrived inside 'DELIVERY.ZIP'; the agreement requires it inside 'delivery.zip' (letter case must match)."
  )
  loose_case <- container_findings(ld(
    DTA(datasets = list(tab_ds("ae", list(DTAFileCSV("ae.csv")))), containers = list(DTAContainer("m", "Delivery.zip", default = TRUE))),
    cont_write("ae.csv", CONT_AE_CSV)
  ))
  expect_identical(
    loose_case$message,
    "ae.csv (dataset 'ae') arrived loose; the agreement requires it inside 'Delivery.zip'."
  )
})

test_that("a file inside its container gives no row", {
  z <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "delivery.zip")
  expect_identical(nrow(container_findings(ld(cf_dta(), z))), 0L)
})

test_that("a file read with load_file() is unverified", {
  d <- load_file(cf_dta(), dataset = "ae", file = cont_write("ae.csv", CONT_AE_CSV))
  f <- container_findings(d)
  expect_identical(nrow(f), 1L)
  expect_identical(f$status, "unverified")
  expect_identical(f$arrived, "on its own")
  expect_identical(f$file, "ae.csv")
  expect_identical(f$container, "delivery.zip")
  expect_identical(
    f$message,
    "ae.csv (dataset 'ae') was checked on its own; the agreement requires it inside 'delivery.zip', so its packaging is not verified."
  )
})

test_that("a table bound by assignment is unrecorded when the dataset has a container entry", {
  d <- cf_dta()
  ds <- d@datasets[["ae"]]
  ds@tables <- list(ae = arrow::arrow_table(USUBJID = "S-001", AETERM = "X", AESEQ = 1))
  d@datasets[["ae"]] <- ds
  f <- container_findings(d)
  expect_identical(nrow(f), 1L)
  expect_identical(f$status, "unrecorded")
  expect_identical(f$item, "ae")
  expect_identical(f$container, "delivery.zip")
  expect_true(is.na(f$file))
  expect_true(is.na(f$arrived))
  expect_identical(
    f$message,
    "ae (dataset 'ae'): it is not recorded how this file arrived; load it with load_file() or load_delivery() to judge its container."
  )
})

test_that("an unrecorded table gives no row when no entry has a container", {
  d <- cont_dta("csv")
  ds <- d@datasets[["ae"]]
  ds@tables <- list(ae = arrow::arrow_table(USUBJID = "S-001", AETERM = "X", AESEQ = 1))
  d@datasets[["ae"]] <- ds
  expect_identical(nrow(container_findings(d)), 0L)
})

test_that("an unrecorded table of a dataset whose entries need different containers has no single container", {
  d <- DTA(
    datasets = list(tab_ds("ae", list(DTAFileCSV("ae.csv", container = "a.zip"), DTAFileCSV("ae2.csv", container = "b.zip")))),
    containers = list()
  )
  ds <- d@datasets[["ae"]]
  ds@tables <- list(ae = arrow::arrow_table(USUBJID = "S-001", AETERM = "X", AESEQ = 1))
  d@datasets[["ae"]] <- ds
  f <- container_findings(d)
  expect_identical(f$status, "unrecorded")
  expect_true(is.na(f$container))
})

test_that("a file dataset is judged by the entry that took its file", {
  d <- DTA(
    datasets = list(file_ds("docs", DTAFileAny("define.pdf", container = "pkg.zip"))),
    containers = list()
  )
  d <- ld(d, cont_write("define.pdf", "x"))
  f <- container_findings(d)
  expect_identical(f$dataset, "docs")
  expect_identical(f$status, "outside")
  expect_identical(f$file, "define.pdf")
  expect_identical(f$container, "pkg.zip")
})

# --- check(DTA) ---------------------------------------------------------------

# The text of the closing "Validation ..." line check() prints.
cf_summary_line <- function(d) {
  msgs <- character()
  withCallingHandlers(
    check(d, persist = FALSE),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) invokeRestart("muffleWarning")
  )
  line <- grep("Validation (FAILED|INCOMPLETE|PASSED)", msgs, value = TRUE)
  trimws(regmatches(line[[length(line)]], regexpr("Validation (FAILED|INCOMPLETE|PASSED).*$", line[[length(line)]])))
}

test_that("an outside file fails the run, signals the class and leaves content valid", {
  d <- ld(cf_ae_only(), cont_write("ae.csv", CONT_AE_CSV))
  expect_warning(
    suppressMessages(check(d, quiet = TRUE, persist = FALSE)),
    class = "dta_container_mismatch"
  )
  expect_warning(
    suppressMessages(check(d, quiet = TRUE, persist = FALSE)),
    class = "dta_validation_finding"
  )
  checked <- cf_check(d)
  expect_false(attr(checked, "last_validation_ok"))
  expect_true(all(validation_status(datasets(checked, "ae"))$ok))
  expect_identical(attr(checked, "last_container_findings")$status, "outside")
  expect_identical(
    cf_summary_line(d),
    "Validation FAILED: 1 file not inside the required container"
  )
})

test_that("an unverified file leaves the run incomplete but its content valid", {
  d <- load_file(cf_ae_only(), dataset = "ae", file = cont_write("ae.csv", CONT_AE_CSV))
  expect_warning(
    suppressMessages(check(d, quiet = TRUE, persist = FALSE)),
    class = "dta_container_mismatch"
  )
  checked <- cf_check(d)
  expect_false(attr(checked, "last_validation_ok"))
  expect_true(all(validation_status(datasets(checked, "ae"))$ok))
  expect_identical(attr(checked, "last_container_findings")$status, "unverified")
  expect_identical(
    cf_summary_line(d),
    "Validation INCOMPLETE: 1 file whose packaging is not verified"
  )
})

test_that("a file inside its container passes the packaging axis", {
  z <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "delivery.zip")
  checked <- cf_check(ld(cf_ae_only(), z))
  expect_true(attr(checked, "last_validation_ok"))
  expect_identical(nrow(attr(checked, "last_container_findings")), 0L)
  expect_no_warning(suppressMessages(check(ld(cf_ae_only(), z), quiet = TRUE, persist = FALSE)))
})

test_that("a subset check keeps the findings of the datasets it did not check", {
  d <- DTA(
    datasets = list(tab_ds("ae", list(DTAFileCSV("ae.csv"))), tab_ds("dm", list(DTAFileCSV("dm.csv")))),
    containers = list(cf_default())
  )
  d <- load_file(d, dataset = "ae", file = cont_write("ae.csv", CONT_AE_CSV))
  d <- ld(d, cont_zip(list(dm.csv = CONT_DM_CSV), name = "delivery.zip"), datasets = "dm")
  checked <- cf_check(d, datasets = "dm")
  f <- attr(checked, "last_container_findings")
  expect_identical(f$dataset, "ae")
  expect_identical(f$status, "unverified")
  expect_false(attr(checked, "last_validation_ok"))
})

test_that("a file dataset whose entry has an id reference checks and names the glob", {
  d <- DTA(
    datasets = list(file_ds("docs", DTAFileAny("define.pdf", container = "sdtm"))),
    containers = list(DTAContainer("sdtm", "sdtm_*.zip"))
  )
  checked <- NULL
  expect_no_error(checked <- cf_check(d))
  msg <- messages(checked, as_tibble = FALSE)$message
  expect_true(any(grepl("was not delivered inside 'sdtm_*.zip'", msg, fixed = TRUE)))
})

test_that("an outside file next to other failures is named in every FAILED line", {
  expect_identical(
    dta_overall_summary_message(2, 0, 0, total_container_errors = 1L)$text,
    "Validation FAILED: 2 tables with validation errors; 1 file not inside the required container"
  )
  expect_identical(
    dta_overall_summary_message(0, 3, 0, total_container_errors = 2L)$text,
    "Validation FAILED: 3 values could not be imported in the declared type; 2 files not inside the required container"
  )
  expect_identical(
    dta_overall_summary_message(0, 0, 0, total_count_errors = 1L, total_container_errors = 1L)$text,
    "Validation FAILED: 1 dataset with a file count outside the declared bounds; 1 file not inside the required container"
  )
  # Unchecked targets are still named after it.
  expect_identical(
    dta_overall_summary_message(0, 0, 2, total_container_errors = 1L)$text,
    "Validation FAILED: 1 file not inside the required container; 2 targets not checked"
  )
})

test_that("the INCOMPLETE line joins its parts with a semicolon", {
  m <- dta_overall_summary_message(0, 0, 1, total_container_unverified = 2L)
  expect_identical(m$severity, "warning")
  expect_identical(
    m$text,
    "Validation INCOMPLETE: 1 target was not checked; 2 files whose packaging is not verified"
  )
  expect_identical(
    dta_overall_summary_message(0, 0, 2, 1, total_container_unverified = 1L)$text,
    "Validation INCOMPLETE: 2 targets were not checked (1 dataset with no tables loaded); 1 file whose packaging is not verified"
  )
  # No container findings: the existing line is unchanged.
  expect_identical(
    dta_overall_summary_message(0, 0, 1)$text,
    "Validation INCOMPLETE: 1 target was not checked"
  )
})

# --- scenarios from the other work packages ------------------------------------

test_that("the default container with an ADaM container and an archive-taking docs entry leaves no finding", {
  empty_specs <- DTAColumnSpecCollection(columns = list())
  dta <- DTA(
    datasets = list(
      tab_ds("ae", list(DTAFileCSV("ae.csv"))),
      tab_ds("adsl", list(DTAFileCSV("adsl.csv", container = "adam.zip")), empty_specs),
      file_ds("docs", DTAFileAny("[.]pdf$|[.]zip$", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 10))
    ),
    containers = list(cf_default())
  )
  main <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "delivery.zip")
  adam <- cont_zip(list(adsl.csv = "USUBJID\nS-001\n"), name = "adam.zip")
  expect_identical(nrow(container_findings(ld(dta, c(main, adam)))), 0L)
})

# An object as an older version saved it: no `containers`, no `provenance`.
cf_age_class <- function(cls) {
  parent <- cls@parent
  parent_old <- if (inherits(parent, "S7_class") && !identical(parent@name, "S7_object")) {
    cf_age_class(parent)
  } else {
    S7::S7_object
  }
  own <- setdiff(names(cls@properties), names(parent@properties))
  S7::new_class(cls@name,
    package = "DTAtools", parent = parent_old,
    properties = cls@properties[setdiff(own, c("containers", "provenance", "file_handlers"))],
    validator = function(self) NULL
  )
}
cf_age <- function(obj) {
  if (is.environment(obj) || is.data.frame(obj)) {
    return(obj)
  }
  if (inherits(obj, "S7_object")) {
    cur <- attr(obj, "S7_class", exact = TRUE)
    if (!identical(cur@package, "DTAtools")) {
      return(obj)
    }
    for (p in setdiff(names(attributes(obj)), c("class", "S7_class"))) {
      attr(obj, p) <- cf_age(attr(obj, p))
    }
    for (p in intersect(c("containers", "provenance", "file_handlers"), names(cur@properties))) attr(obj, p) <- NULL
    attr(obj, "S7_class") <- cf_age_class(cur)
    return(obj)
  }
  if (is.list(obj)) {
    for (i in seq_along(obj)) if (!is.null(obj[[i]])) obj[[i]] <- cf_age(obj[[i]])
  }
  obj
}

test_that("check() and container_findings() work on a stale document without upgrade_dta()", {
  old <- cf_age(cont_dta("csv"))
  expect_true(dta_is_stale(old))
  f <- container_findings(old)
  expect_identical(names(f), cf_cols)
  expect_identical(nrow(f), 0L)
  old1 <- cf_age(DTA(datasets = list(tab_ds("ae", list(DTAFileCSV("ae.csv"))))))
  checked <- NULL
  expect_no_error(checked <- cf_check(old1))
  expect_identical(nrow(attr(checked, "last_container_findings")), 0L)
})

test_that("a loose deliverable archive under a default container agrees with routing (N23)", {
  dta <- DTA(
    datasets = list(file_ds("docs", DTAFileAny("report.zip")), tab_ds("ae", list(DTAFileCSV("ae.csv")))),
    containers = list(cf_default())
  )
  d <- ld(dta, cont_zip(list(ae.csv = CONT_AE_CSV, x.pdf = "%PDF"), name = "report.zip"))
  f <- container_findings(d)
  # Routing keeps report.zip closed as a misfit of docs; nothing reached ae.
  expect_identical(nrow(f), 1L)
  expect_identical(f$dataset, "docs")
  expect_identical(f$file, "report.zip")
  expect_identical(f$container, "delivery.zip")
  expect_identical(f$arrived, "loose")
  expect_identical(f$status, "outside")
  expect_false("ae" %in% f$dataset)
})

# --- review fixes (WP4) --------------------------------------------------------

cf_messages <- function(d, ...) {
  msgs <- character()
  withCallingHandlers(
    check(d, persist = FALSE, ...),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) invokeRestart("muffleWarning")
  )
  msgs
}

# ae delivered loose (outside), dm read on its own (unverified), vs assigned (unrecorded).
cf_three <- function() {
  d <- DTA(
    datasets = list(
      tab_ds("ae", list(DTAFileCSV("ae.csv"))),
      tab_ds("dm", list(DTAFileCSV("dm.csv"))),
      tab_ds("vs", list(DTAFileCSV("vs.csv")))
    ),
    containers = list(cf_default())
  )
  d <- ld(d, cont_write("ae.csv", CONT_AE_CSV))
  d <- load_file(d, dataset = "dm", file = cont_write("dm.csv", CONT_DM_CSV))
  ds <- d@datasets[["vs"]]
  ds@tables <- list(vs = arrow::arrow_table(
    USUBJID = "S-001", VSTESTCD = "SBP", VSSTRESN = 120, VSDT = as.Date("2020-01-01")
  ))
  d@datasets[["vs"]] <- ds
  d
}

test_that("every FAILED line also names unverified packaging", {
  unv <- "1 file whose packaging is not verified"
  expect_identical(
    dta_overall_summary_message(1, 0, 0, total_container_unverified = 1L)$text,
    paste0("Validation FAILED: 1 table with validation errors; ", unv)
  )
  expect_identical(
    dta_overall_summary_message(0, 2, 0, total_container_unverified = 1L)$text,
    paste0("Validation FAILED: 2 values could not be imported in the declared type; ", unv)
  )
  expect_identical(
    dta_overall_summary_message(0, 0, 0, total_count_errors = 1L, total_container_unverified = 1L)$text,
    paste0("Validation FAILED: 1 dataset with a file count outside the declared bounds; ", unv)
  )
  expect_identical(
    dta_overall_summary_message(0, 0, 0, total_container_errors = 1L, total_container_unverified = 2L)$text,
    "Validation FAILED: 1 file not inside the required container; 2 files whose packaging is not verified"
  )
  expect_identical(
    dta_overall_summary_message(0, 0, 3, total_container_errors = 1L, total_container_unverified = 1L)$text,
    paste0("Validation FAILED: 1 file not inside the required container; ", unv, "; 3 targets not checked")
  )
  expect_identical(
    dta_overall_summary_message(2, 0, 0, total_container_errors = 1L, total_container_unverified = 1L)$text,
    paste0("Validation FAILED: 2 tables with validation errors; 1 file not inside the required container; ", unv)
  )
})

test_that("an outside and an unverified file together fail the run and name both", {
  d <- load_file(
    ld(
      DTA(
        datasets = list(tab_ds("ae", list(DTAFileCSV("ae.csv"))), tab_ds("dm", list(DTAFileCSV("dm.csv")))),
        containers = list(cf_default())
      ),
      cont_write("ae.csv", CONT_AE_CSV)
    ),
    dataset = "dm", file = cont_write("dm.csv", CONT_DM_CSV)
  )
  expect_identical(
    cf_summary_line(d),
    "Validation FAILED: 1 file not inside the required container; 1 file whose packaging is not verified"
  )
  checked <- cf_check(d)
  expect_false(attr(checked, "last_validation_ok"))
  expect_setequal(attr(checked, "last_container_findings")$status, c("outside", "unverified"))
})

test_that("check() prints the findings under a Containers heading unless quiet", {
  msgs <- cf_messages(cf_three())
  expect_true(any(grepl("Containers", msgs, fixed = TRUE)))
  expect_true(any(grepl("ae.csv (dataset 'ae') arrived loose", msgs, fixed = TRUE)))
  expect_true(any(grepl("dm.csv (dataset 'dm') was checked on its own", msgs, fixed = TRUE)))
  expect_true(any(grepl("vs (dataset 'vs'): it is not recorded", msgs, fixed = TRUE)))
  expect_false(any(grepl("Containers", cf_messages(cf_three(), quiet = TRUE), fixed = TRUE)))
  # Without findings there is no heading.
  z <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "delivery.zip")
  expect_false(any(grepl("Containers", cf_messages(ld(cf_ae_only(), z)), fixed = TRUE)))
})

test_that("an unrecorded table is reported through check() and leaves the run incomplete", {
  d <- cf_ae_only()
  ds <- d@datasets[["ae"]]
  ds@tables <- list(ae = arrow::arrow_table(USUBJID = "S-001", AETERM = "X", AESEQ = 1))
  d@datasets[["ae"]] <- ds
  expect_warning(
    suppressMessages(check(d, quiet = TRUE, persist = FALSE)),
    class = "dta_container_mismatch"
  )
  checked <- cf_check(d)
  expect_identical(attr(checked, "last_container_findings")$status, "unrecorded")
  expect_false(attr(checked, "last_validation_ok"))
  expect_identical(cf_summary_line(d), "Validation INCOMPLETE: 1 file whose packaging is not verified")
})

test_that("an unrecorded file names the file, not the server path", {
  d <- DTA(
    datasets = list(file_ds("docs", DTAFileAny("define.pdf", container = "pkg.zip"))),
    containers = list()
  )
  p <- cont_write("define.pdf", "x")
  ds <- d@datasets[["docs"]]
  ds@file_paths <- p
  d@datasets[["docs"]] <- ds
  f <- container_findings(d)
  expect_identical(f$status, "unrecorded")
  expect_identical(f$item, p)
  expect_false(grepl(dirname(p), f$message, fixed = TRUE))
  expect_match(f$message, "^define.pdf \\(dataset 'docs'\\)")
})

test_that("a provenance record without a file name counts as no record", {
  d <- ld(cf_ae_only(), cont_write("ae.csv", CONT_AE_CSV))
  for (bad in list(NA_character_, NULL, character(), c("a", "b"))) {
    ds <- d@datasets[["ae"]]
    prov <- ds@provenance
    prov[["ae"]] <- list(file = bad, container = character(), via = "delivery")
    ds@provenance <- prov
    d2 <- d
    d2@datasets[["ae"]] <- ds
    f <- NULL
    expect_no_error(f <- container_findings(d2))
    expect_identical(f$status, "unrecorded")
    expect_true(is.na(f$file))
  }
})

test_that("a stale document holding a loose table under a container entry is unrecorded", {
  d <- DTA(
    datasets = list(tab_ds("ae", list(DTAFileCSV("ae.csv", container = "delivery.zip")))),
    containers = list()
  )
  d <- ld(d, cont_write("ae.csv", CONT_AE_CSV))
  old <- cf_age(d)
  expect_true(dta_is_stale(old))
  f <- container_findings(old)
  expect_identical(f$status, "unrecorded")
  expect_identical(f$container, "delivery.zip")
  checked <- cf_check(old)
  expect_false(attr(checked, "last_validation_ok"))
  expect_identical(attr(checked, "last_container_findings")$status, "unrecorded")
})

test_that("a subset check re-judges the datasets it did not check", {
  d <- DTA(
    datasets = list(tab_ds("ae", list(DTAFileCSV("ae.csv"))), tab_ds("dm", list(DTAFileCSV("dm.csv")))),
    containers = list(cf_default())
  )
  d <- load_file(d, dataset = "ae", file = cont_write("ae.csv", CONT_AE_CSV))
  d <- ld(d, cont_zip(list(dm.csv = CONT_DM_CSV), name = "delivery.zip"), datasets = "dm")
  first <- cf_check(d, datasets = "dm")
  expect_identical(attr(first, "last_container_findings")$dataset, "ae")
  # ae is delivered properly now; checking only dm must drop its finding.
  d <- ld(first, cont_zip(list(ae.csv = CONT_AE_CSV), name = "delivery.zip"), datasets = "ae")
  second <- cf_check(d, datasets = "dm")
  expect_identical(nrow(attr(second, "last_container_findings")), 0L)
  expect_true(attr(second, "last_validation_ok"))
})

test_that("an entry list edited after loading does not hide an outside file (M1)", {
  d <- DTA(
    datasets = list(file_ds(
      "docs",
      DTAFileAny("a[.]pdf", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 1),
      DTAFileAny("define.pdf")
    )),
    containers = list(DTAContainer("main", "delivery.zip", default = TRUE))
  )
  d <- ld(d, cont_write("define.pdf", "x"))
  expect_identical(container_findings(d)$status, "outside")
  ds <- d@datasets[["docs"]]
  ds@files <- ds@files[2]
  d@datasets[["docs"]] <- ds
  f <- container_findings(d)
  expect_identical(f$status, "outside")
  expect_identical(f$file, "define.pdf")
  checked <- cf_check(d)
  expect_identical(nrow(attr(checked, "last_container_findings")), 1L)
  expect_false(attr(checked, "last_validation_ok"))
  # No entry takes the file any more: nothing to judge, and no error.
  ds@files <- list(DTAFileAny("other.pdf"))
  d@datasets[["docs"]] <- ds
  expect_no_error(container_findings(d))
})
