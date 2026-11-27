# A packaging check that cannot run must never read as a pass.
#
# The Shiny app reads the packaging findings of the live document through
# DTAtools::container_findings(). When that function throws, the app used to
# treat the error as "no findings", and every reader of the findings then said
# the packaging was fine: a green "Packaging conforms" chip, "Passed all checks"
# in the dataset list, a green certifying summary button and a report banner
# that could read VALIDATION PASSED.
#
# The decision under test: a check that throws is a distinct state,
# "not_checkable", one row per dataset, shown amber and in words, and it never
# reads passed, conforms or green. Expected values come from that decision, not
# from the code's output. The cause text is this file's own stop() message.
#
# The mock cannot be a blanket one: DTAtools::check() calls container_findings()
# from inside the package, so a mock that always throws would stop check_all
# from ever producing a pass. nck_probe() therefore passes the real function
# through until the test flips its switch, and counts the calls it refused.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

nck_cause <- "boom: findings unavailable"

# Passes container_findings() through until `$boom` is TRUE, then throws `msg`.
# `$calls` counts the refused calls, so a test can prove the mock was reached.
nck_probe <- function(msg = nck_cause, env = parent.frame()) {
  probe <- new.env(parent = emptyenv())
  probe$boom <- FALSE
  probe$calls <- 0L
  orig <- DTAtools::container_findings
  testthat::local_mocked_bindings(
    container_findings = function(x) {
      if (isTRUE(probe$boom)) {
        probe$calls <- probe$calls + 1L
        stop(msg, call. = FALSE)
      }
      orig(x)
    },
    .package = "DTAtools", .env = env
  )
  probe
}

nck_fi <- function(path, name) app_upload_value(path, name)

# A top-level function of app.R. testServer() evaluates the test block in a data
# mask over the server's own bindings, so the builders defined at the top of
# app.R are not in scope there; `anchor` is a plain function defined inside
# server() (validation_report_html), whose enclosure is the server frame and
# then the app environment.
nck_app_fn <- function(name, anchor) get(name, envir = environment(anchor), mode = "function")

# A specification with two datasets that must both arrive inside delivery.zip.
nck_spec <- function(env = parent.frame()) {
  cols_ae <- c(
    "      - {id: USUBJID, type: SAS Char}", "      - {id: AETERM, type: SAS Char}",
    "      - {id: AESEQ, type: SAS Num}"
  )
  cols_dm <- c("      - {id: USUBJID, type: SAS Char}", "      - {id: AGE, type: SAS Num}")
  ds <- function(id, cols) {
    c(
      paste0("  - id: ", id), "    type: tabular",
      paste0("    files: {filename: ", id, ".csv, type: csv}"), "    columns:", cols
    )
  }
  f <- withr::local_tempfile(fileext = ".yaml", .local_envir = env)
  writeLines(
    c(
      "metadata:", "  title: Packaging not checkable", "  version: \"1\"",
      "containers:", "  - id: dlv", "    filename: delivery.zip", "    default: true",
      "datasets:", ds("ae", cols_ae), ds("dm", cols_dm)
    ),
    f
  )
  f
}

# The delivery that conforms: both files inside delivery.zip.
nck_good_zip <- function(env = parent.frame()) {
  cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "delivery.zip", env = env)
}

nck_row <- function(status, dataset = "ae", item = paste0(dataset, ".csv")) {
  data.frame(
    dataset = dataset, item = item, file = item, container = "delivery.zip",
    arrived = "loose", status = status, message = "m", stringsAsFactors = FALSE
  )
}

nck_columns <- c("dataset", "item", "file", "container", "arrived", "status", "message")

# ---- a. the findings the app reads --------------------------------------------------

test_that("a findings check that throws gives one not_checkable row per dataset, never none", {
  clean_session_file()
  spec <- nck_spec()
  zip <- nck_good_zip()
  probe <- nck_probe()

  shiny::testServer(app_server_dir(), {
    dv_findings <- nck_app_fn("dv_findings", validation_report_html)
    dv_empty_findings <- nck_app_fn("dv_empty_findings", validation_report_html)
    session$setInputs(dta_file = nck_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = nck_fi(zip, "delivery.zip"))

    # Control: the same document, a delivery that conforms, the check can run.
    ok <- dv_findings(rv$dta)
    expect_identical(nrow(ok), 0L)
    expect_identical(names(ok), nck_columns)

    probe$boom <- TRUE
    f <- dv_findings(rv$dta)
    expect_gt(probe$calls, 0L)
    expect_identical(nrow(f), 2L)
    expect_setequal(f$dataset, c("ae", "dm"))
    expect_true(all(f$status == "not_checkable"))
    # Same shape as the empty frame: same columns, same types.
    expect_identical(names(f), names(ok))
    expect_identical(vapply(f, typeof, ""), vapply(dv_empty_findings(), typeof, ""))
    expect_true(all(is.na(f$item) & is.na(f$file) & is.na(f$container) & is.na(f$arrived)))
    # The cause is carried, and the sentence says the packaging could not be checked.
    expect_true(all(grepl(nck_cause, f$message, fixed = TRUE)))
    expect_true(all(grepl("could not be checked", f$message, fixed = TRUE)))
  })
})

test_that("the findings helper returns the real findings unchanged, and a not_checkable frame when it throws", {
  clean_session_file()
  helper <- app_fn("dta_container_findings_or_not_checkable")
  probe <- nck_probe()
  empty_doc <- app_fn("dta_create_empty")("Empty", "1")$value

  # A loose file where the default container is required: one real finding.
  spec <- nck_spec()
  dta <- suppressMessages(DTAtools::read_dta_from_yaml(spec))
  dta <- DTAtools::load_file(dta, dataset = "ae", file = cont_write("ae.csv", CONT_AE_CSV))
  real <- DTAtools::container_findings(dta)
  expect_identical(real$status, "unverified")
  expect_identical(helper(dta), real)
  expect_identical(nrow(helper(NULL)), 0L)
  expect_identical(names(helper(NULL)), nck_columns)

  probe$boom <- TRUE
  f <- helper(dta)
  expect_gt(probe$calls, 0L)
  expect_setequal(f$dataset, c("ae", "dm"))
  expect_true(all(f$status == "not_checkable"))
  expect_false(anyNA(f$dataset))
  # A document with no datasets has nothing to mark: a zero-row frame of the right shape.
  none <- helper(empty_doc)
  expect_identical(nrow(none), 0L)
  expect_identical(names(none), nck_columns)
  # A missing document is still no findings.
  expect_identical(nrow(helper(NULL)), 0L)
})

# ---- b. the Delivery view's chip ----------------------------------------------------

test_that("the Delivery chip says Packaging not checkable, and only a working check can say it conforms", {
  clean_session_file()
  spec <- nck_spec()
  zip <- nck_good_zip()
  probe <- nck_probe()

  shiny::testServer(app_server_dir(), {
    dv_findings <- nck_app_fn("dv_findings", validation_report_html)
    dv_view_ui <- nck_app_fn("dv_view_ui", validation_report_html)
    session$setInputs(dta_file = nck_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = nck_fi(zip, "delivery.zip"))
    view <- function() {
      render_html(dv_view_ui(rv$dta, rv$last_delivery, dv_findings(rv$dta), NULL))
    }

    ok <- view()
    expect_match(ok, "Packaging conforms", fixed = TRUE)
    expect_false(grepl("not checkable", ok, ignore.case = TRUE))
    # Both members arrived inside delivery.zip.
    expect_match(ok, "<td>inside</td>", fixed = TRUE)

    probe$boom <- TRUE
    bad <- view()
    expect_gt(probe$calls, 0L)
    # The amber chip carries the words (the markup is pretty-printed, so the
    # whitespace between its parts is not part of the contract).
    expect_match(
      bad,
      'status-chip status-nodata">\\s*<span class="status-dot"></span>\\s*Packaging not checkable\\s*</span>'
    )
    expect_false(grepl("Packaging conforms", bad, fixed = TRUE))
    expect_false(grepl("Not conforming", bad, fixed = TRUE))
    expect_false(grepl("status-pass", bad, fixed = TRUE))
    # The members table does not claim the files arrived inside.
    expect_false(grepl("<td>inside</td>", bad, fixed = TRUE))
    expect_match(bad, "<td>packaging not checkable</td>", fixed = TRUE)
  })
})

# ---- c. every reader, through a running server --------------------------------------

test_that("a passed dataset is no pass in the list, the button, the report or the chips while the packaging cannot be checked", {
  clean_session_file()
  spec <- nck_spec()
  zip <- nck_good_zip()
  probe <- nck_probe()

  shiny::testServer(app_server_dir(), {
    dv_container_states <- nck_app_fn("dv_container_states", validation_report_html)
    dv_ds_packaging <- nck_app_fn("dv_ds_packaging", validation_report_html)
    dv_packaging_phrase <- nck_app_fn("dv_packaging_phrase", validation_report_html)
    session$setInputs(dta_file = nck_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = nck_fi(zip, "delivery.zip"))
    session$setInputs(check_all = 1)
    expect_identical(unlist(rv$status), c(ae = "pass", dm = "pass"))
    expect_identical(nrow(findings_now()), 0L)

    # Control: a working check lets the same state read as a pass.
    nav <- ui_text(output$dataset_nav_ui)
    expect_match(nav, "Passed all checks", fixed = TRUE)
    expect_match(nav, "nav-st-pass", fixed = TRUE)
    expect_false(grepl("could not be checked", nav, fixed = TRUE))
    button <- ui_text(output$validation_report_ui)
    expect_match(button, "btn btn-success", fixed = TRUE)
    expect_match(button, "certifying", fixed = TRUE)
    expect_match(validation_report_html(), "VALIDATION PASSED", fixed = TRUE)
    expect_match(ui_text(output$delivery_panel), "delivery.zip: 2 files", fixed = TRUE)
    expect_false(grepl("file-finding", ui_text(output$loaded_files), fixed = TRUE))
    expect_match(ui_text(output$dataset_status_line), "status-chip status-pass", fixed = TRUE)
    expect_match(ui_text(output$summary_metrics), "st-pass", fixed = TRUE)

    # The check can no longer run. The cached findings know nothing of that, so
    # touch the document (an inert attribute) to make them recompute.
    probe$boom <- TRUE
    touched <- rv$dta
    attr(touched, "nck_probe") <- TRUE
    rv$dta <- touched
    session$flushReact()
    expect_identical(unlist(rv$status), c(ae = "pass", dm = "pass"))
    expect_setequal(findings_now()$status, "not_checkable")
    expect_gt(probe$calls, 0L)

    # The dataset list: not a pass, amber, in words.
    nav <- ui_text(output$dataset_nav_ui)
    expect_false(grepl("Passed all checks", nav, fixed = TRUE))
    expect_false(grepl("nav-st-pass", nav, fixed = TRUE))
    expect_match(nav, "nav-st-nodata", fixed = TRUE)
    expect_match(nav, "Content passed, but the packaging could not be checked", fixed = TRUE)

    # The summary button: not green, not certifying, and it says why.
    button <- ui_text(output$validation_report_ui)
    expect_match(button, 'id="dl_validation_summary"', fixed = TRUE)
    expect_false(grepl("btn-success", button, fixed = TRUE))
    expect_false(grepl("certifying", button, fixed = TRUE))
    expect_match(button, "btn-warning", fixed = TRUE)
    expect_match(button, "Validation summary (INCOMPLETE: packaging not checkable)", fixed = TRUE)
    expect_match(button, "could not be checked", fixed = TRUE)
    expect_match(button, "incomplete rather than passed", fixed = TRUE)

    # The report: incomplete, never passed, with the packaging named as the reason.
    html <- validation_report_html()
    expect_match(html, "VALIDATION INCOMPLETE", fixed = TRUE)
    expect_false(grepl("VALIDATION PASSED", html, fixed = TRUE))
    expect_false(grepl("VALIDATION FAILED", html, fixed = TRUE))
    expect_match(html, "Not checkable: ", fixed = TRUE)
    expect_match(html, nck_cause, fixed = TRUE)
    expect_match(html, "could not be checked, so this DTA cannot be reported as passed", fixed = TRUE)

    # The sidebar line, the dataset's own chip, the loaded files, the status bar.
    panel <- ui_text(output$delivery_panel)
    expect_match(panel, "delivery.zip: packaging could not be checked", fixed = TRUE)
    expect_false(grepl("✓", panel, fixed = TRUE))
    expect_false(grepl("delivery.zip: 2 files", panel, fixed = TRUE))
    head <- ui_text(output$dataset_status_line)
    expect_match(head, "Passed · packaging not checkable", fixed = TRUE)
    expect_match(head, "status-chip status-nodata", fixed = TRUE)
    expect_false(grepl("status-chip status-pass", head, fixed = TRUE))
    expect_match(head, "ds-st-nodata", fixed = TRUE)
    files <- ui_text(output$loaded_files)
    expect_match(files, "Packaging not checkable", fixed = TRUE)
    expect_match(files, "ff-not_checkable", fixed = TRUE)
    bar <- ui_text(output$summary_metrics)
    expect_false(grepl("st-pass", bar, fixed = TRUE))
    expect_false(grepl("seg-pass", bar, fixed = TRUE))
    expect_match(bar, "seg-nodata", fixed = TRUE)
    expect_match(bar, "packaging issues", fixed = TRUE)

    # The Delivery view reached through the app, not only the builder.
    rv$main_view <- "delivery"
    session$flushReact()
    view <- ui_text(output$dataset_detail)
    expect_match(view, "Packaging not checkable", fixed = TRUE)
    expect_false(grepl("Packaging conforms", view, fixed = TRUE))

    # The counting readers.
    f <- findings_now()
    st <- dv_container_states(rv$dta, f)
    expect_length(st, 1L)
    expect_gt(st[[1]]$n_not_checkable, 0L)
    expect_identical(st[[1]]$n_outside, 0L)
    expect_identical(st[[1]]$n_unverified, 0L)
    expect_identical(dv_ds_packaging(f, "ae"), "not_checkable")
    expect_identical(dv_ds_packaging(f, "dm"), "not_checkable")
    expect_identical(dv_packaging_phrase(f), "2 datasets whose packaging is not checkable")
  })
})

# ---- the readers, on hand-made findings ---------------------------------------------

test_that("the packaging verdict of a dataset ranks outside, then not checkable, then unverified", {
  clean_session_file()
  nck <- nck_row("not_checkable", item = NA_character_)
  out <- nck_row("outside")
  unv <- nck_row("unverified")

  shiny::testServer(app_server_dir(), {
    dv_ds_packaging <- nck_app_fn("dv_ds_packaging", validation_report_html)
    dv_empty_findings <- nck_app_fn("dv_empty_findings", validation_report_html)
    expect_identical(dv_ds_packaging(rbind(unv, nck, out), "ae"), "outside")
    expect_identical(dv_ds_packaging(rbind(unv, nck), "ae"), "not_checkable")
    expect_identical(dv_ds_packaging(unv, "ae"), "unverified")
    expect_identical(dv_ds_packaging(nck_row("unrecorded"), "ae"), "unverified")
    expect_identical(dv_ds_packaging(dv_empty_findings(), "ae"), "none")
    # Another dataset's finding is not this one's.
    expect_identical(dv_ds_packaging(nck, "dm"), "none")
  })
})

test_that("the toast phrase counts not-checkable datasets apart and leaves the old wording alone", {
  clean_session_file()
  nck <- rbind(
    nck_row("not_checkable", "ae", NA_character_),
    nck_row("not_checkable", "dm", NA_character_)
  )

  shiny::testServer(app_server_dir(), {
    dv_packaging_phrase <- nck_app_fn("dv_packaging_phrase", validation_report_html)
    expect_identical(dv_packaging_phrase(nck), "2 datasets whose packaging is not checkable")
    expect_identical(
      dv_packaging_phrase(nck[1, ]),
      "1 dataset whose packaging is not checkable"
    )
    # Unchanged: outside and unverified keep their own phrases.
    expect_identical(
      dv_packaging_phrase(rbind(nck_row("outside"), nck_row("unverified", "dm"))),
      "1 file not inside its container, 1 file whose packaging is not verified"
    )
    expect_identical(
      dv_packaging_phrase(nck_row("unrecorded")),
      "1 file whose packaging is not verified"
    )
  })
})

test_that("a container line says its packaging could not be checked, after outside and before everything else", {
  clean_session_file()
  st <- function(...) {
    base <- list(
      row = list(filename = "delivery.zip", n_files = 2L),
      n_outside = 0L, n_not_checkable = 0L, n_unverified = 0L, n_unrecorded = 0L,
      inside = character(0)
    )
    modifyList(base, list(...))
  }

  shiny::testServer(app_server_dir(), {
    dv_container_line <- nck_app_fn("dv_container_line", validation_report_html)
    nck_line <- dv_container_line(st(n_not_checkable = 2L, inside = "delivery.zip"))
    expect_identical(nck_line$text, "delivery.zip: packaging could not be checked")
    expect_identical(nck_line$cls, "dp-warn")
    # Not the green tick of a container whose files arrived inside it.
    expect_false(identical(nck_line$cls, "dp-ok"))
    expect_identical(dv_container_line(st(n_not_checkable = 1L, n_unverified = 1L))$cls, "dp-warn")
    expect_identical(
      dv_container_line(st(n_not_checkable = 1L, n_unverified = 1L))$text,
      "delivery.zip: packaging could not be checked"
    )
    expect_identical(
      dv_container_line(st(n_outside = 1L, n_not_checkable = 1L))$text,
      "delivery.zip: 1 file not inside it"
    )
    # Unchanged lines.
    expect_identical(
      dv_container_line(st(n_unverified = 1L))$text,
      "delivery.zip: 1 file checked on its own"
    )
    expect_identical(dv_container_line(st(inside = "delivery.zip"))$cls, "dp-ok")
  })
})

# ---- d. the report, built directly --------------------------------------------------

test_that("a report built without findings is incomplete when the check cannot run, and a content failure still fails it", {
  clean_session_file()
  build <- app_fn("dta_build_validation_report")
  dta <- cont_dta()
  all_pass <- list(ae = "pass", dm = "pass", vs = "pass")
  one_fail <- list(ae = "fail", dm = "pass", vs = "pass")
  probe <- nck_probe("boom <i>x</i> & y")

  # Control: nothing to report, so all-pass reads PASSED.
  ok <- build(dta, all_pass, findings = NULL)
  expect_match(ok, "VALIDATION PASSED", fixed = TRUE)
  expect_false(grepl("<h2>Delivery</h2>", ok, fixed = TRUE))

  probe$boom <- TRUE
  html <- build(dta, all_pass, findings = NULL)
  expect_gt(probe$calls, 0L)
  expect_match(html, "VALIDATION INCOMPLETE", fixed = TRUE)
  expect_false(grepl("VALIDATION PASSED", html, fixed = TRUE))
  expect_false(grepl("VALIDATION FAILED", html, fixed = TRUE))
  expect_match(html, "<h2>Delivery</h2>", fixed = TRUE)
  # One row per dataset, each led by the not-checkable lead, all three of them.
  expect_identical(lengths(regmatches(html, gregexpr("<li>Not checkable: ", html, fixed = TRUE))), 3L)
  expect_false(grepl("Not verified: ", html, fixed = TRUE))
  expect_false(grepl("Not inside its container: ", html, fixed = TRUE))
  expect_match(html, "could not be checked, so this DTA cannot be reported as passed", fixed = TRUE)
  expect_match(html, "packaging not checkable for 3 datasets", fixed = TRUE)
  # The cause is user-controlled text: it is escaped, never markup.
  expect_match(html, "boom &lt;i&gt;x&lt;/i&gt; &amp; y", fixed = TRUE)
  expect_false(grepl("boom <i>x</i>", html, fixed = TRUE))

  # A content failure still wins.
  failed <- build(dta, one_fail, findings = NULL)
  expect_match(failed, "VALIDATION FAILED", fixed = TRUE)
  expect_false(grepl("VALIDATION INCOMPLETE", failed, fixed = TRUE))
  expect_false(grepl("VALIDATION PASSED", failed, fixed = TRUE))
})

test_that("not_checkable findings handed to the report leave it incomplete, and unverified findings are unchanged", {
  clean_session_file()
  build <- app_fn("dta_build_validation_report")
  dta <- cont_dta()
  status <- list(ae = "pass", dm = "pass", vs = "pass")
  nck <- rbind(
    nck_row("not_checkable", "ae", NA_character_),
    nck_row("not_checkable", "dm", NA_character_)
  )

  html <- build(dta, status, findings = nck)
  expect_match(html, "VALIDATION INCOMPLETE", fixed = TRUE)
  expect_false(grepl("VALIDATION PASSED", html, fixed = TRUE))
  expect_match(html, "<li>Not checkable: m</li>", fixed = TRUE)
  expect_match(html, "packaging not checkable for 2 datasets", fixed = TRUE)

  # The older verdicts keep their own lead and caveat.
  unv <- build(dta, status, findings = nck_row("unverified"))
  expect_match(unv, "VALIDATION INCOMPLETE", fixed = TRUE)
  expect_match(unv, "<li>Not verified: m</li>", fixed = TRUE)
  expect_false(grepl("could not be checked", unv, fixed = TRUE))
  expect_match(unv, "1 packaging finding.", fixed = TRUE)
  out <- build(dta, status, findings = nck_row("outside"))
  expect_match(out, "VALIDATION FAILED", fixed = TRUE)
  expect_match(out, "<li>Not inside its container: m</li>", fixed = TRUE)
})

# ---- e. the status vocabulary -------------------------------------------------------

test_that("status_display and status_chip give not_checkable the amber class and never pass", {
  display <- app_fn("status_display")
  chip <- app_fn("status_chip")

  expect_identical(display("pass", "not_checkable"), "nodata")
  expect_false(identical(display("pass", "not_checkable"), "pass"))
  # A content failure is not softened, and the older packaging verdicts are unchanged.
  expect_identical(display("fail", "not_checkable"), "fail")
  expect_identical(display("pass", "unverified"), "nodata")
  expect_identical(display("pass", "outside"), "fail")
  expect_identical(display("pass", "none"), "pass")
  expect_identical(display("pass"), "pass")

  html <- as.character(chip("pass", "not_checkable"))
  expect_match(html, "status-chip status-nodata", fixed = TRUE)
  expect_false(grepl("status-pass", html, fixed = TRUE))
  expect_match(html, "Passed · packaging not checkable", fixed = TRUE)
  # Content that did not pass keeps its own chip, whatever the packaging.
  expect_match(as.character(chip("fail", "not_checkable")), "Failed", fixed = TRUE)
  expect_match(as.character(chip("pass", "unverified")), "Passed · packaging not verified", fixed = TRUE)
})
