# The Delivery surfaces of the Shiny app: the sidebar's Delivery zone and its
# status lines, the Delivery view, the ambiguity dialog, a dataset's own slot
# (it takes that dataset's files only), the verdict surfaces that carry the
# packaging findings, and the autosave / restore of a delivery.
#
# Every output is read through ui_text(): a renderUI that no test reads is never
# rendered under testServer(). Files a test drops are written BEFORE
# shiny::testServer() starts (a withr::local_*() inside the block would defer to
# an environment that never exits).

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

ct_fi <- function(path, name) {
  data.frame(
    name = name, size = file.size(path), type = "",
    datapath = path, stringsAsFactors = FALSE
  )
}

ct_yaml <- function(lines, env = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".yaml", .local_envir = env)
  writeLines(lines, f)
  f
}

ct_header <- c("metadata:", "  title: Container test", "  version: \"1\"")
ct_ae_cols <- c(
  "      - {id: USUBJID, type: SAS Char}", "      - {id: AETERM, type: SAS Char}",
  "      - {id: AESEQ, type: SAS Num}"
)
ct_dm_cols <- c("      - {id: USUBJID, type: SAS Char}", "      - {id: AGE, type: SAS Num}")
ct_ds <- function(id, file, cols) {
  c(paste0("  - id: ", id), "    type: tabular", paste0("    files: ", file), "    columns:", cols)
}
# The `ae`, `dm` specification; `containers` are the lines of the document-level
# section, `extra` more dataset lines.
ct_spec <- function(containers = character(), ae = "{filename: ae.csv, type: csv}",
                    dm = "{filename: dm.csv, type: csv}", extra = character(), env = parent.frame()) {
  ct_yaml(
    c(ct_header, containers, "datasets:", ct_ds("ae", ae, ct_ae_cols), ct_ds("dm", dm, ct_dm_cols), extra),
    env
  )
}
ct_default <- c("containers:", "  - id: dlv", "    filename: delivery.zip", "    default: true")
# Two pattern entries that both take `ae.csv` (and `a1e.csv`, ...): a tie.
ct_tie_spec <- function(env = parent.frame()) {
  ct_spec(
    ae = "{filename: \"^a.*[.]csv$\", type: csv, pattern: true}",
    dm = "{filename: \"^.*e[.]csv$\", type: csv, pattern: true}",
    env = env
  )
}

ct_notes <- function(env = parent.frame()) {
  env$shown <- character(0)
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) {
      env$shown <- c(env$shown, paste(as.character(ui), collapse = ""))
      invisible(NULL)
    },
    .package = "shiny", .env = env
  )
}

ct_modals <- function(env = parent.frame()) {
  env$modals <- character(0)
  testthat::local_mocked_bindings(
    showModal = function(ui, ...) {
      env$modals <- c(env$modals, flat_message(paste(as.character(ui), collapse = "")))
      invisible(NULL)
    },
    .package = "shiny", .env = env
  )
}

ct_select_tab <- function(env = parent.frame()) {
  env$selected <- character(0)
  testthat::local_mocked_bindings(
    nav_select = function(id, selected = NULL, ...) {
      env$selected <- c(env$selected, paste(id, selected, sep = ":"))
      invisible(NULL)
    },
    .package = "bslib", .env = env
  )
}

# For a test whose fixture is linted on purpose (a pattern tie, an unused
# container): the app reports those findings itself, so only R's
# dta_lint_warning is muffled; every other warning still surfaces.
ct_quiet <- function(code) {
  withCallingHandlers(code, dta_lint_warning = function(w) invokeRestart("muffleWarning"))
}

# A delivery directory's staged copies: not the extraction itself.
ct_set_input <- function(session, id, value) do.call(session$setInputs, stats::setNames(list(value), id))

# ---- the Delivery block's gate and static wiring ----------------------------------

test_that("the Delivery block is offered for two datasets or any container, not for one plain dataset", {
  clean_session_file()
  one <- app_fixture_path("clinical_dta.yaml")
  two <- ct_spec()
  one_container <- ct_yaml(c(
    ct_header, ct_default, "datasets:", ct_ds("ae", "{filename: ae.csv, type: csv}", ct_ae_cols)
  ))

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(one, "clinical_dta.yaml"))
    expect_length(rv$structure, 1)
    expect_false(output$delivery_zone)
  })
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(two, "spec.yaml"))
    expect_true(output$delivery_zone)
  })
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(one_container, "spec.yaml"))
    expect_length(rv$structure, 1)
    expect_true(output$delivery_zone)
  })
})

test_that("the workspace carries the static drop zone, its flag and the tab id the server switches", {
  clean_session_file()
  spec <- ct_spec()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    html <- ui_text(output$main)
    expect_match(html, 'id="delivery_files"', fixed = TRUE)
    expect_match(html, "Drop the whole delivery (.zip, .xpt or loose files)", fixed = TRUE)
    expect_match(html, "delivery-drop", fixed = TRUE)
    expect_match(html, "output.delivery_zone", fixed = TRUE)
    expect_match(html, 'id="main_tabs"', fixed = TRUE)
    expect_match(html, 'id="delivery_panel"', fixed = TRUE)
    expect_match(html, 'id="delivery_hint"', fixed = TRUE)
  })
})

test_that("the hint says what a drop here means, and adds the packaging warning only with containers", {
  clean_session_file()
  plain <- ct_spec()
  boxed <- ct_spec(ct_default)

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(plain, "spec.yaml"))
    hint <- ui_text(output$delivery_hint)
    expect_match(hint, "Each file goes to the dataset whose expected name it matches.", fixed = TRUE)
    expect_false(grepl("judged as delivered", hint, fixed = TRUE))
  })
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(boxed, "spec.yaml"))
    hint <- ui_text(output$delivery_hint)
    expect_match(hint, "Files here are judged as delivered.", fixed = TRUE)
    expect_match(hint, "To check single files before packing them, drop them on a dataset's own zone.", fixed = TRUE)
    expect_false(grepl("does not conform", hint, fixed = TRUE))
  })
})

# ---- the sidebar's status lines ------------------------------------------------------

test_that("the panel names each container, and counts a file checked on its own", {
  clean_session_file()
  spec <- ct_spec(ct_default)
  ae <- cont_write("ae.csv", CONT_AE_CSV)

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    panel <- ui_text(output$delivery_panel)
    expect_match(panel, "delivery.zip: not delivered yet", fixed = TRUE)
    expect_match(panel, "\u25CB", fixed = TRUE)
    # The glyph is decoration: the words carry the state.
    expect_match(panel, 'aria-hidden="true"', fixed = TRUE)
    expect_match(panel, "Containers\u2026", fixed = TRUE)
    expect_false(grepl("Last delivery", panel, fixed = TRUE))

    session$setInputs(up_1_1 = ct_fi(ae, "ae.csv"))
    panel <- ui_text(output$delivery_panel)
    expect_match(panel, "delivery.zip: 1 file checked on its own", fixed = TRUE)
    expect_match(panel, "\u25D0", fixed = TRUE)
  })
})

test_that("a delivery inside the container reads as received, one loose reads as not inside it", {
  clean_session_file()
  spec <- ct_spec(ct_default)
  good <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "delivery.zip")
  other <- cont_fx("sdtm_csv.zip")
  ae <- cont_write("ae.csv", CONT_AE_CSV)

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(good, "delivery.zip"))
    panel <- ui_text(output$delivery_panel)
    expect_match(panel, "delivery.zip: 2 files", fixed = TRUE)
    expect_match(panel, "\u2713", fixed = TRUE)
    expect_match(panel, "Last delivery, ", fixed = TRUE)
    expect_match(panel, "2 files loaded, 0 unmatched", fixed = TRUE)
    expect_match(panel, 'id="delivery_details"', fixed = TRUE)
  })
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(other, "sdtm_csv.zip"))
    # Both members arrived inside another archive: the worst state wins.
    panel <- ui_text(output$delivery_panel)
    expect_match(panel, "delivery.zip: 2 files not inside it", fixed = TRUE)
    expect_match(panel, "\u26A0", fixed = TRUE)
    expect_false(grepl("delivery.zip: 2 files</span>", panel, fixed = TRUE))
  })
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(ae, "ae.csv"))
    expect_match(ui_text(output$delivery_panel), "delivery.zip: 1 file not inside it", fixed = TRUE)
  })
})

# ---- a sidebar drop and the Delivery view --------------------------------------------

test_that("a sidebar drop binds every dataset, records the delivery and opens the Delivery view", {
  clean_session_file()
  # vs declares sdtm_csv.zip as its container, and the archive holds no vs.csv.
  spec <- ct_spec(extra = ct_ds("vs", "{filename: vs.csv, type: csv, container: sdtm_csv.zip}", c(
    "      - {id: USUBJID, type: SAS Char}", "      - {id: VSSTRESN, type: SAS Num}"
  )))
  zip1 <- cont_fx("sdtm_csv.zip")
  ct_select_tab()
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    expect_identical(rv$main_view, "dataset")
    session$setInputs(delivery_files = ct_fi(zip1, "sdtm_csv.zip"))

    expect_setequal(names(rv$uploads), c("ae||1", "dm||1"))
    plan <- rv$last_delivery$plan
    expect_setequal(plan$name[plan$status == "loaded"], c("ae.csv", "dm.csv"))
    expect_identical(rv$last_delivery$missing, "vs.csv")
    expect_identical(rv$main_view, "delivery")
    # Metadata may be the open tab: the view lives on Datasets.
    expect_identical(selected, "main_tabs:Datasets")

    view <- ui_text(output$dataset_detail)
    expect_match(view, "<h4>Delivery</h4>", fixed = TRUE)
    expect_match(view, "ae.csv", fixed = TRUE)
    expect_match(view, "Inside sdtm_csv.zip", fixed = TRUE)
    # No container in the spec: the packaging conforms, and something is bound.
    expect_match(view, "Packaging conforms", fixed = TRUE)
    expect_false(grepl("Nothing delivered yet", view, fixed = TRUE))
    expect_match(view, "Not in the delivery: vs.csv.", fixed = TRUE)
    # Files the specification has no entry for are named, with the reason.
    expect_match(view, "define.xml", fixed = TRUE)
    expect_match(view, "no matching entry", fixed = TRUE)
    expect_match(view, 'id="check_all_delivery"', fixed = TRUE)
    # No R option or function reaches the user.
    expect_false(grepl("route =", view, fixed = TRUE))
  })
})

test_that("the Delivery view's Check all validates every dataset, and a dataset click leaves the view", {
  clean_session_file()
  spec <- ct_spec()
  zip1 <- cont_fx("sdtm_csv.zip")
  ct_select_tab()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(zip1, "sdtm_csv.zip"))
    session$setInputs(check_all_delivery = 1)
    expect_identical(unlist(rv$status), c(ae = "pass", dm = "pass"))

    # The sidebar's line reopens the view after a dataset was selected.
    ct_set_input(session, paste0("selds_", rv$structure[["dm"]]$index), 1)
    expect_identical(rv$main_view, "dataset")
    expect_identical(rv$active, "dm")
    session$setInputs(delivery_details = 1)
    expect_identical(rv$main_view, "delivery")
    expect_identical(utils::tail(selected, 1), "main_tabs:Datasets")

    # ... and a click on the dataset that is ALREADY active leaves it again.
    ct_set_input(session, paste0("selds_", rv$structure[["dm"]]$index), 2)
    expect_identical(rv$main_view, "dataset")
  })
})

test_that("with the document's default container the chip says Not conforming, and the container is listed with its members", {
  clean_session_file()
  spec <- ct_spec(c(
    "containers:", "  - id: dlv", "    filename: delivery.zip", "    description: All delivered files.",
    "    default: true"
  ))
  zip1 <- cont_fx("sdtm_csv.zip")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    rv$main_view <- "delivery"
    session$flushReact()
    expect_match(ui_text(output$dataset_detail), "Nothing delivered yet", fixed = TRUE)

    session$setInputs(delivery_files = ct_fi(zip1, "sdtm_csv.zip"))
    view <- ui_text(output$dataset_detail)
    expect_match(view, "Not conforming", fixed = TRUE)
    expect_false(grepl("Nothing delivered yet", view, fixed = TRUE))
    expect_match(view, "<h5>delivery.zip</h5>", fixed = TRUE)
    expect_match(view, "All delivered files.", fixed = TRUE)
    expect_match(view, "All other files", fixed = TRUE)
    expect_match(view, "inside sdtm_csv.zip", fixed = TRUE)
    expect_match(view, "not inside it", fixed = TRUE)
  })
})

test_that("a loose file on the Delivery zone is judged as delivered, and the view says it arrived loose", {
  clean_session_file()
  spec <- ct_spec(ct_default)
  ae <- cont_write("ae.csv", CONT_AE_CSV)

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(ae, "ae.csv"))
    view <- ui_text(output$dataset_detail)
    expect_match(view, "Not conforming", fixed = TRUE)
    expect_match(view, "<td>loose</td>", fixed = TRUE)
    expect_identical(findings_now()$status, "outside")
  })
})

# ---- the ambiguity dialog ---------------------------------------------------------------

test_that("an ambiguous file asks, refuses to go on unanswered, and binds only where it is sent", ct_quiet({
  clean_session_file()
  spec <- ct_tie_spec()
  ae <- cont_write("ae.csv", CONT_AE_CSV)
  ct_modals()
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(ae, "ae.csv"))

    pd <- rv$pending_delivery
    expect_false(is.null(pd))
    expect_length(modals, 1)
    id <- sprintf("amb_%s_1", pd$nonce)
    expect_match(modals, sprintf('name="%s"', id), fixed = TRUE)
    expect_match(modals, "Which dataset is this file for?", fixed = TRUE)
    # One file: the singular intro, and the radios say who they answer for.
    expect_match(modals, "This file matches more than one dataset. Choose one, or Don't load.", fixed = TRUE)
    expect_false(grepl("These files match", modals, fixed = TRUE))
    expect_match(modals, 'aria-label="Send ae.csv to"', fixed = TRUE)
    expect_match(modals, "modal-lg", fixed = TRUE)
    expect_match(modals, "radio-inline", fixed = TRUE)
    # Candidates are named with the expected name that matched; nothing is preselected.
    expect_match(modals, "ae \u2014 expects ^a.*[.]csv$", fixed = TRUE)
    expect_match(modals, "dm \u2014 expects ^.*e[.]csv$", fixed = TRUE)
    expect_match(modals, "Don't load", fixed = TRUE)
    expect_false(grepl("checked", modals, fixed = TRUE))
    expect_match(modals, "Cancel delivery (nothing is loaded)", fixed = TRUE)
    expect_match(modals, 'id="amb_load"', fixed = TRUE)
    # Edit mode is off: the hint on distinct patterns stays away.
    expect_false(grepl("make the expected names distinct", modals, fixed = TRUE))
    expect_false(grepl("Set all to", modals, fixed = TRUE))
    # Nothing is bound while it waits, and the staged copy is kept for the answer.
    expect_length(DTAtools::tables(DTAtools::datasets(rv$dta, "dm")), 0)
    expect_true(all(file.exists(pd$copies)))
    expect_identical(ui_text(output$amb_error), "")

    # Load with nothing chosen: refused inline, nothing bound, still waiting.
    session$setInputs(amb_load = 1)
    expect_match(ui_text(output$amb_error), "Choose a dataset or Don.t load for every file\\.")
    expect_length(DTAtools::tables(DTAtools::datasets(rv$dta, "dm")), 0)
    expect_false(is.null(rv$pending_delivery))

    ct_set_input(session, id, "dm")
    session$setInputs(amb_load = 2)
    expect_null(rv$pending_delivery)
    expect_identical(names(DTAtools::tables(DTAtools::datasets(rv$dta, "dm"))), "ae")
    expect_length(DTAtools::tables(DTAtools::datasets(rv$dta, "ae")), 0)
    expect_identical(names(rv$uploads), "dm||1")
    # The error of the earlier attempt does not outlive the dialog.
    expect_identical(ui_text(output$amb_error), "")
    expect_identical(rv$last_delivery$plan$dataset[rv$last_delivery$plan$status == "loaded"], "dm")
  })
}))

test_that("Don't load leaves the file out and the note says no dataset was chosen", ct_quiet({
  clean_session_file()
  spec <- ct_tie_spec()
  # de.csv is taken only by dm's pattern: it loads while ae.csv waits for an answer.
  both <- c(cont_write("ae.csv", CONT_AE_CSV), cont_write("de.csv", CONT_DM_CSV))
  ct_modals()
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(both, c("ae.csv", "de.csv")))
    pd <- rv$pending_delivery
    expect_false(is.null(pd))
    expect_length(DTAtools::tables(DTAtools::datasets(rv$dta, "dm")), 0)

    ct_set_input(session, sprintf("amb_%s_1", pd$nonce), "")
    session$setInputs(amb_load = 1)

    expect_null(rv$pending_delivery)
    expect_identical(names(DTAtools::tables(DTAtools::datasets(rv$dta, "dm"))), "de")
    expect_length(DTAtools::tables(DTAtools::datasets(rv$dta, "ae")), 0)
    expect_match(utils::tail(shown, 1), "Not loaded (no dataset chosen): ae.csv", fixed = TRUE)
  })
}))

test_that("Cancel delivery loads nothing and deletes the staged copies", ct_quiet({
  clean_session_file()
  spec <- ct_tie_spec()
  ae <- cont_write("ae.csv", CONT_AE_CSV)
  ct_modals()
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(ae, "ae.csv"))
    copies <- rv$pending_delivery$copies
    expect_length(copies, 1)
    expect_true(file.exists(copies))

    session$setInputs(amb_cancel = 1)
    expect_null(rv$pending_delivery)
    expect_false(file.exists(copies))
    expect_length(DTAtools::tables(DTAtools::datasets(rv$dta, "ae")), 0)
    expect_length(DTAtools::tables(DTAtools::datasets(rv$dta, "dm")), 0)
    expect_length(rv$uploads, 0)
    expect_null(rv$last_delivery)
  })
}))

test_that("ending the session while the dialog asks deletes the staged copies", ct_quiet({
  clean_session_file()
  spec <- ct_tie_spec()
  ae <- cont_write("ae.csv", CONT_AE_CSV)
  ct_modals()
  copies <- NULL

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(ae, "ae.csv"))
    copies <<- rv$pending_delivery$copies
    expect_true(file.exists(copies))
    session$close()
  })
  expect_false(file.exists(copies))
}))

test_that("five ambiguous files offer Set all to, which answers the rows the dataset is a candidate of", ct_quiet({
  clean_session_file()
  spec <- ct_tie_spec()
  names5 <- paste0("a", 1:5, "e.csv")
  files <- character(0)
  for (n in names5) files <- c(files, cont_write(n, CONT_AE_CSV))
  ct_modals()
  ct_notes()
  answered <- character(0)
  testthat::local_mocked_bindings(
    updateRadioButtons = function(session, inputId, ..., selected = NULL) {
      answered <<- c(answered, paste(inputId, selected, sep = "="))
      invisible(NULL)
    },
    .package = "shiny"
  )

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(files, names5))
    pd <- rv$pending_delivery
    expect_length(pd$rows, 5)
    expect_match(modals, "Set all to", fixed = TRUE)
    expect_match(modals, 'id="amb_set_all"', fixed = TRUE)
    expect_match(modals, "Which dataset are these files for?", fixed = TRUE)

    session$setInputs(amb_set_all = "dm")
    session$setInputs(amb_apply = 1)
    expect_setequal(answered, sprintf("amb_%s_%d=dm", pd$nonce, 1:5))

    # Answering all five in one go loads them.
    for (i in 1:5) ct_set_input(session, sprintf("amb_%s_%d", pd$nonce, i), "dm")
    session$setInputs(amb_load = 1)
    expect_setequal(names(DTAtools::tables(DTAtools::datasets(rv$dta, "dm"))), sub("[.]csv$", "", names5))
  })
}))

test_that("in edit mode the dialog says how to stop being asked", ct_quiet({
  clean_session_file()
  spec <- ct_tie_spec()
  ae <- cont_write("ae.csv", CONT_AE_CSV)
  ct_modals()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    unlock_editing(session)
    session$setInputs(delivery_files = ct_fi(ae, "ae.csv"))
    expect_match(
      utils::tail(modals, 1),
      "To stop being asked, make the expected names distinct in each dataset's Edit \u25B8 Files.",
      fixed = TRUE
    )
  })
}))

# ---- a dataset's own slot -----------------------------------------------------------------

test_that("a zip dropped on a slot loads only that dataset, and the note says what was left", {
  clean_session_file()
  spec <- ct_spec()
  zip1 <- cont_fx("sdtm_csv.zip")
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = ct_fi(zip1, "sdtm_csv.zip"))

    expect_identical(names(rv$uploads), "ae||1")
    expect_length(DTAtools::tables(DTAtools::datasets(rv$dta, "dm")), 0)
    expect_identical(
      utils::tail(shown, 1),
      "Loaded only ae's files from sdtm_csv.zip (ae.csv). Its other 3 files were not loaded."
    )
    # The sidebar's view is for a sidebar drop.
    expect_identical(rv$main_view, "dataset")
    expect_identical(rv$last_delivery$plan$dataset[rv$last_delivery$plan$status == "loaded"], "ae")
  })
})

test_that("a zip with nothing for the slot's dataset says so and loads nothing", {
  clean_session_file()
  spec <- ct_spec()
  dm_only <- cont_zip(list(dm.csv = CONT_DM_CSV), name = "dm_only.zip")
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = ct_fi(dm_only, "dm_only.zip"))

    expect_length(rv$uploads, 0)
    expect_length(DTAtools::tables(DTAtools::datasets(rv$dta, "dm")), 0)
    expect_identical(
      utils::tail(shown, 1),
      "dm_only.zip holds no file for ae (expected ae.csv). Nothing was loaded."
    )
    expect_null(rv$last_delivery)
  })
})

test_that("a slot takes an archive its own entry names as the file it is", {
  clean_session_file()
  spec <- ct_yaml(c(
    ct_header, "datasets:",
    "  - id: archive", "    type: file", "    files: {filename: sdtm.zip, type: any}"
  ))
  archive <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "sdtm.zip")
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = ct_fi(archive, "sdtm.zip"))
    expect_length(rv$uploads[["archive||1"]], 1)
    expect_identical(basename(DTAtools::datasets(rv$dta, "archive")@file_paths), "sdtm.zip")
    # Not a delivery: nothing recorded, no view.
    expect_null(rv$last_delivery)
    expect_identical(rv$main_view, "dataset")
  })
})

test_that("a .xpt on a csv entry's slot is a delivery for that dataset, on an xpt entry's slot a file", {
  clean_session_file()
  spec <- ct_yaml(c(
    ct_header, "datasets:",
    ct_ds("ae", "{filename: ae.csv, type: csv}", ct_ae_cols),
    ct_ds("lb", "{filename: adverse.xpt, type: xpt}", ct_ae_cols)
  ))
  xpt <- cont_fx("adverse.xpt")
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    # ae's entry reads no .xpt: the drop is a delivery, for ae only, and the file
    # is lb's, which this call does not load.
    session$setInputs(up_1_1 = ct_fi(xpt, "adverse.xpt"))
    expect_length(rv$uploads, 0)
    expect_identical(
      utils::tail(shown, 1),
      "adverse.xpt holds no file for ae (expected ae.csv). Nothing was loaded."
    )
  })
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    # lb's entry IS an xpt: the file binds as the file it is.
    session$setInputs(up_2_1 = ct_fi(xpt, "adverse.xpt"))
    expect_length(rv$uploads[["lb||1"]], 1)
    expect_null(rv$last_delivery)
  })
})

test_that("a loose file on a slot with a container is the individual check", {
  clean_session_file()
  spec <- ct_spec(ae = "{filename: ae.csv, type: csv, container: sdtm_csv.zip}")
  ae <- cont_write("ae.csv", CONT_AE_CSV)
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = ct_fi(ae, "ae.csv"))
    expect_length(rv$uploads[["ae||1"]], 1)
    expect_match(utils::tail(shown, 1), "Loaded ae.csv on its own for checking.", fixed = TRUE)
    expect_identical(findings_now()$status, "unverified")
  })
})

# ---- the verdict surfaces ----------------------------------------------------------------------

test_that("a file outside its container fails the report banner and the summary button is no pass", {
  clean_session_file()
  spec <- ct_spec(ct_default)
  both <- c(cont_write("ae.csv", CONT_AE_CSV), cont_write("dm.csv", CONT_DM_CSV))
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(both, c("ae.csv", "dm.csv")))
    session$setInputs(check_all = 1)
    # The content passed; the packaging did not.
    expect_identical(unlist(rv$status), c(ae = "pass", dm = "pass"))
    expect_setequal(findings_now()$status, "outside")

    expect_match(validation_report_html(), "VALIDATION FAILED", fixed = TRUE)
    button <- ui_text(output$validation_report_ui)
    expect_match(button, 'id="dl_validation_summary"', fixed = TRUE)
    expect_false(grepl("btn-success", button, fixed = TRUE))
    expect_match(button, "failed", fixed = TRUE)
    # The check's toast lists the finding, next to the tally.
    expect_match(utils::tail(shown, 1), "arrived loose; the agreement requires it inside", fixed = TRUE)
  })
})

test_that("a file checked on its own leaves the report incomplete, never passed", {
  clean_session_file()
  spec <- ct_spec(ct_default)
  ae <- cont_write("ae.csv", CONT_AE_CSV)
  dm <- cont_write("dm.csv", CONT_DM_CSV)

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = ct_fi(ae, "ae.csv"))
    session$setInputs(up_2_1 = ct_fi(dm, "dm.csv"))
    session$setInputs(check_all = 1)
    expect_identical(unlist(rv$status), c(ae = "pass", dm = "pass"))

    html <- validation_report_html()
    expect_match(html, "VALIDATION INCOMPLETE", fixed = TRUE)
    expect_false(grepl("VALIDATION PASSED", html, fixed = TRUE))
    button <- ui_text(output$validation_report_ui)
    expect_match(button, "btn-warning", fixed = TRUE)
    expect_false(grepl("btn-success", button, fixed = TRUE))
  })
})

test_that("a delivery that conforms is the one that can pass", {
  clean_session_file()
  spec <- ct_spec(ct_default)
  good <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "delivery.zip")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(good, "delivery.zip"))
    session$setInputs(check_all = 1)
    expect_identical(nrow(findings_now()), 0L)
    expect_match(validation_report_html(), "VALIDATION PASSED", fixed = TRUE)
    expect_match(ui_text(output$validation_report_ui), "btn-success", fixed = TRUE)
  })
})

test_that("a loaded file carries a chip that says how it arrived and what the agreement requires", {
  clean_session_file()
  spec <- ct_spec(ct_default)
  ae <- cont_write("ae.csv", CONT_AE_CSV)
  ae2 <- cont_write("ae.csv", CONT_AE_CSV)
  zip1 <- cont_fx("sdtm_csv.zip")
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(ae, "ae.csv"))
    expect_match(ui_text(output$loaded_files), "\u26A0 delivered loose; required inside delivery.zip", fixed = TRUE)
  })
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = ct_fi(ae2, "ae.csv"))
    expect_match(ui_text(output$loaded_files), "\u25D0 checked on its own; required inside delivery.zip", fixed = TRUE)
  })
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(zip1, "sdtm_csv.zip"))
    expect_match(ui_text(output$loaded_files), "\u26A0 came in sdtm_csv.zip; required inside delivery.zip", fixed = TRUE)
  })
})

test_that("a slot's container is the effective one, the default included, with its description", {
  clean_session_file()
  spec <- ct_spec(c(
    "containers:", "  - id: dlv", "    filename: delivery.zip", "    description: Everything we ship.",
    "    default: true"
  ))

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    expect_identical(rv$structure[["ae"]]$handlers[[1]]$container, "delivery.zip")
    detail <- ui_text(output$dataset_detail)
    expect_match(detail, "in delivery.zip", fixed = TRUE)
    expect_match(detail, "Must arrive inside delivery.zip.", fixed = TRUE)
    expect_match(detail, "Everything we ship.", fixed = TRUE)
  })
})

# ---- autosave and restore ------------------------------------------------------------------------

ct_clean_autosave <- function(env = parent.frame()) {
  withr::defer(unlink(list.files(tempdir(), "^dtatools_app_session", full.names = TRUE), recursive = TRUE, force = TRUE), envir = env)
}

test_that("the autosave keeps the last delivery and the provenance, and Start over clears them", {
  clean_session_file()
  ct_clean_autosave()
  spec <- ct_spec()
  zip1 <- cont_fx("sdtm_csv.zip")
  id <- strrep("7", 32)
  ct_notes()
  ct_select_tab()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = id)
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(zip1, "sdtm_csv.zip"))
    saved <- readRDS(list.files(tempdir(), "^dtatools_app_session.*[.]rds$", full.names = TRUE))
    expect_setequal(saved$last_delivery$plan$name[saved$last_delivery$plan$status == "loaded"], c("ae.csv", "dm.csv"))
    expect_s3_class(saved$last_delivery$at, "POSIXct")
    session$close()
  })

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = id)
    session$setInputs(restore_session = 1)
    expect_setequal(
      rv$last_delivery$plan$name[rv$last_delivery$plan$status == "loaded"],
      c("ae.csv", "dm.csv")
    )
    expect_identical(rv$main_view, "dataset")
    rec <- DTAtools::datasets(rv$dta, "ae")@provenance[["ae"]]
    expect_identical(rec$file, "ae.csv")
    expect_identical(rec$container, "sdtm_csv.zip")
    expect_identical(rec$via, "delivery")
    # The sidebar line comes back with it.
    expect_match(ui_text(output$delivery_panel), "Last delivery, ", fixed = TRUE)

    rv$main_view <- "delivery"
    session$setInputs(confirm_reset = 1)
    expect_null(rv$last_delivery)
    expect_null(rv$pending_delivery)
    expect_identical(rv$main_view, "dataset")
  })
})

test_that("a restore whose document cannot be upgraded is rebuilt from its YAML text", {
  clean_session_file()
  ct_clean_autosave()
  spec <- ct_spec(ct_default)
  zip1 <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "delivery.zip")
  id <- strrep("8", 32)
  ct_notes()
  ct_select_tab()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = id)
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(zip1, "delivery.zip"))
    session$close()
  })

  testthat::local_mocked_bindings(
    upgrade_dta = function(x, ...) stop("cannot be upgraded"),
    .package = "DTAtools"
  )
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = id)
    session$setInputs(restore_session = 1)
    # The document is the YAML's, with the data bound in the saved one.
    expect_identical(names(rv$dta@containers), "dlv")
    expect_setequal(names(DTAtools::tables(DTAtools::datasets(rv$dta, "ae"))), "ae")
    expect_identical(as.data.frame(DTAtools::tables(DTAtools::datasets(rv$dta, "ae"))[["ae"]])$AETERM, c("HEADACHE", "NAUSEA"))
    expect_identical(DTAtools::datasets(rv$dta, "ae")@provenance[["ae"]]$container, "delivery.zip")
    expect_identical(names(rv$structure), c("ae", "dm"))
    expect_true(any(grepl("restored from its YAML text", shown, fixed = TRUE)))
  })
})

test_that("a restore with neither an upgradable document nor usable YAML refuses", {
  clean_session_file()
  ct_clean_autosave()
  spec <- ct_spec()
  id <- strrep("6", 32)
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = id)
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = ct_fi(cont_write("ae.csv", CONT_AE_CSV), "ae.csv"))
    session$close()
  })
  f <- list.files(tempdir(), "^dtatools_app_session.*[.]rds$", full.names = TRUE)
  saved <- readRDS(f)
  saved$yaml_text <- NULL
  saveRDS(saved, f)

  testthat::local_mocked_bindings(
    upgrade_dta = function(x, ...) stop("cannot be upgraded"),
    .package = "DTAtools"
  )
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = id)
    session$setInputs(restore_session = 1)
    expect_null(rv$dta)
    expect_true(any(grepl("Could not restore the previous session.", shown, fixed = TRUE)))
  })
})

# An old-shaped DTA: its classes without the properties a later version added.
ct_old_shape_class <- function(cls, drop) {
  parent <- cls@parent
  parent_old <- if (inherits(parent, "S7_class") && !identical(parent@name, "S7_object")) {
    ct_old_shape_class(parent, drop)
  } else {
    S7::S7_object
  }
  own <- setdiff(names(cls@properties), names(parent@properties))
  S7::new_class(
    cls@name,
    package = "DTAtools", parent = parent_old,
    properties = cls@properties[setdiff(own, drop)], validator = function(self) NULL
  )
}
ct_age <- function(obj, drop = c("containers", "provenance", "file_handlers")) {
  if (is.environment(obj) || is.data.frame(obj)) {
    return(obj)
  }
  if (inherits(obj, "S7_object")) {
    cur <- attr(obj, "S7_class", exact = TRUE)
    if (!identical(cur@package, "DTAtools")) {
      return(obj)
    }
    for (p in setdiff(names(attributes(obj)), c("class", "S7_class"))) {
      attr(obj, p) <- ct_age(attr(obj, p), drop)
    }
    for (p in intersect(drop, names(cur@properties))) attr(obj, p) <- NULL
    attr(obj, "S7_class") <- ct_old_shape_class(cur, drop)
    return(obj)
  }
  if (is.list(obj)) {
    for (i in seq_along(obj)) if (!is.null(obj[[i]])) obj[[i]] <- ct_age(obj[[i]], drop)
  }
  obj
}

test_that("restore_session() upgrades a document saved by an older version", {
  clean_session_file()
  ct_clean_autosave()
  spec <- ct_spec()
  id <- strrep("5", 32)
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = id)
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = ct_fi(cont_write("ae.csv", CONT_AE_CSV), "ae.csv"))
    session$close()
  })
  f <- list.files(tempdir(), "^dtatools_app_session.*[.]rds$", full.names = TRUE)
  saved <- readRDS(f)
  saved$dump$dta <- ct_age(saved$dump$dta)
  saveRDS(saved, f)
  old <- readRDS(f)$dump$dta
  expect_true(dta_is_stale(old))

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = id)
    session$setInputs(restore_session = 1)
    expect_false(dta_is_stale(rv$dta))
    expect_length(rv$dta@containers, 0)
    expect_length(rv$dta@datasets$ae@provenance, 0)
    expect_identical(names(DTAtools::tables(DTAtools::datasets(rv$dta, "ae"))), "ae")
    # It works as a current document does: a container can be named on an entry.
    h <- rv$dta@datasets$ae@files[[1]]
    h@container <- "sdtm.zip"
    expect_identical(h@container, "sdtm.zip")
    expect_false(any(grepl("YAML text", shown, fixed = TRUE)))
  })
})

# =====================================================================================
# The container manager, the Metadata section, the Files editor's select and the
# file-name conflict guard (WP6b-2). Expected values come from the plan (D10, D16,
# 5.6, 5.7, 5.9), not from observed output.
# =====================================================================================

mg_sdtm <- c("containers:", "  - id: sdtm", "    filename: STUDY_SDTM_*.zip", "    description: SDTM data")
mg_two <- c(
  "containers:", "  - id: sdtm", "    filename: STUDY_SDTM_*.zip", "    default: true",
  "  - id: adam", "    filename: STUDY_ADAM_*.zip"
)
mg_ref <- function(dta, ds, i = 1) {
  r <- DTAtools::datasets(dta, ds)@files[[i]]@container
  if (is.null(r)) NA_character_ else r
}
# Every input the Files editor's form renders except the container select.
mg_file_form <- function(session, filename, type = "csv", pattern = FALSE) {
  session$setInputs(
    file_filename = filename, file_type = type, file_pattern = pattern,
    file_count_mode = "exact", file_number_of_files = 1,
    file_pattern_description = "", file_info = "", file_extensions = ""
  )
}
mg_xpt <- function(re) sprintf("{filename: '%s', type: xpt, pattern: true}", re)

test_that("in view mode the manager opens read-only and no input changes the document", {
  clean_session_file()
  spec <- ct_spec(mg_sdtm, ae = "{filename: ae.csv, type: csv, container: sdtm}")
  ct_modals()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    expect_false(editing())
    session$setInputs(open_containers = 1)
    expect_length(modals, 1)
    html <- ui_text(output$ctr_modal_body)
    expect_match(html, "STUDY_SDTM_*.zip", fixed = TRUE)
    expect_match(html, "SDTM data", fixed = TRUE)
    expect_match(html, "1 file in 1 dataset", fixed = TRUE)
    for (id in c("ctr_add", "ctr_edit", "ctr_remove")) expect_false(grepl(id, html, fixed = TRUE), info = id)

    # Assign tab: the choice is shown as text, with no select and no Save.
    session$setInputs(ctr_tab_assign = 1)
    html <- ui_text(output$ctr_modal_body)
    expect_match(html, "STUDY_SDTM_*.zip", fixed = TRUE)
    expect_false(grepl("<select", html, fixed = TRUE))
    expect_false(grepl("ctr_assign_save", html, fixed = TRUE))

    before <- rv$dta
    session$setInputs(ctr_tab_containers = 1)
    session$setInputs(ctr_add = 1)
    session$setInputs(ctr_filename = "X_*.zip", ctr_description = "", ctr_default = FALSE)
    session$setInputs(ctr_save = 1)
    expect_identical(rv$ctr_view, "list")
    expect_identical(names(rv$dta@containers), "sdtm")
    expect_identical(rv$dta, before)
  })
})

test_that("a container is added with an invisible id, assigned, renamed and the YAML follows", {
  clean_session_file()
  spec <- ct_spec()
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    unlock_editing(session)
    session$setInputs(open_containers = 1)
    expect_match(ui_text(output$ctr_modal_body), "No containers", fixed = TRUE)
    session$setInputs(ctr_add = 1)
    html <- ui_text(output$ctr_modal_body)
    expect_match(html, 'id="ctr_filename"', fixed = TRUE)
    expect_match(html, "Files added later are included automatically.", fixed = TRUE)
    expect_match(html, "All other files must arrive inside it too", fixed = TRUE)

    session$setInputs(ctr_filename = "STUDY_SDTM_*.zip", ctr_description = "SDTM data", ctr_default = FALSE)
    session$setInputs(ctr_save = 1)
    expect_identical(names(rv$dta@containers), "STUDY_SDTM")
    expect_identical(rv$dta@containers$STUDY_SDTM@filename, "STUDY_SDTM_*.zip")
    expect_match(rv$yaml_text, "containers:", fixed = TRUE)
    expect_match(rv$yaml_text, "STUDY_SDTM_*.zip", fixed = TRUE)
    expect_true(any(grepl("Container saved.", shown, fixed = TRUE)))
    expect_identical(ui_text(output$ctr_msg), "")
    # The id is never shown: the list names the file.
    expect_false(grepl("STUDY_SDTM<", ui_text(output$ctr_modal_body), fixed = TRUE))

    # Assign both datasets to it.
    session$setInputs(ctr_tab_assign = 1)
    html <- ui_text(output$ctr_modal_body)
    nonce <- rv$ctr_assign$nonce
    expect_match(html, sprintf('id="ctr_a_%s_1"', nonce), fixed = TRUE)
    expect_match(html, sprintf('id="ctr_a_%s_2"', nonce), fixed = TRUE)
    expect_match(html, "Loose or in any container", fixed = TRUE)
    expect_match(html, '<option value="STUDY_SDTM">STUDY_SDTM_*.zip</option>', fixed = TRUE)
    ct_set_input(session, sprintf("ctr_a_%s_1", nonce), "STUDY_SDTM")
    ct_set_input(session, sprintf("ctr_a_%s_2", nonce), "STUDY_SDTM")
    session$setInputs(ctr_assign_save = 1)
    expect_identical(mg_ref(rv$dta, "ae"), "STUDY_SDTM")
    expect_identical(mg_ref(rv$dta, "dm"), "STUDY_SDTM")
    expect_match(rv$yaml_text, "container: STUDY_SDTM", fixed = TRUE)

    # Renaming the file keeps the entries on the container.
    session$setInputs(open_containers = 2)
    expect_match(ui_text(output$ctr_modal_body), "2 files in 2 datasets", fixed = TRUE)
    session$setInputs(ctr_edit = 1)
    expect_match(ui_text(output$ctr_modal_body), "STUDY_SDTM_*.zip", fixed = TRUE)
    session$setInputs(ctr_filename = "STUDY_SDTM_2026_*.zip", ctr_description = "SDTM data", ctr_default = FALSE)
    session$setInputs(ctr_save = 2)
    expect_identical(names(rv$dta@containers), "STUDY_SDTM")
    expect_identical(rv$dta@containers$STUDY_SDTM@filename, "STUDY_SDTM_2026_*.zip")
    expect_identical(mg_ref(rv$dta, "ae"), "STUDY_SDTM")
    expect_identical(mg_ref(rv$dta, "dm"), "STUDY_SDTM")
    expect_match(rv$yaml_text, "STUDY_SDTM_2026_*.zip", fixed = TRUE)
  })
})

test_that("a form error is shown inline without a derived id", {
  clean_session_file()
  spec <- ct_spec()
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    unlock_editing(session)
    session$setInputs(open_containers = 1)
    session$setInputs(ctr_add = 1)
    session$setInputs(ctr_filename = "x.tar", ctr_description = "", ctr_default = FALSE)
    session$setInputs(ctr_save = 1)
    expect_length(rv$dta@containers, 0)
    msg <- flat_message(ui_text(output$ctr_msg))
    expect_match(msg, "must name a .zip archive or a .xpt SAS transport file", fixed = TRUE)
    expect_false(grepl("Container '", msg, fixed = TRUE))
    expect_identical(rv$ctr_view, "form")
  })
})

test_that("ticking the default takes it over, says so, and the options name it", ct_quiet({
  clean_session_file()
  spec <- ct_spec(mg_two)
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    unlock_editing(session)
    session$setInputs(open_containers = 1)
    # With a default container the "" option is that container.
    session$setInputs(ctr_tab_assign = 1)
    html <- ui_text(output$ctr_modal_body)
    expect_match(html, "STUDY_SDTM_*.zip (with all other files)", fixed = TRUE)
    expect_match(html, "No container required", fixed = TRUE)
    expect_false(grepl("Loose or in any container", html, fixed = TRUE))

    session$setInputs(ctr_tab_containers = 1)
    expect_match(ui_text(output$ctr_modal_body), "All other files", fixed = TRUE)
    session$setInputs(ctr_edit = 2)
    session$setInputs(ctr_filename = "STUDY_ADAM_*.zip", ctr_description = "", ctr_default = TRUE)
    expect_match(
      ui_text(output$ctr_default_warning),
      "'STUDY_SDTM_*.zip' currently collects all other files. Saving moves them to 'STUDY_ADAM_*.zip'; 'STUDY_SDTM_*.zip' will then hold no files.",
      fixed = TRUE
    )
    session$setInputs(ctr_save = 1)
    expect_true(rv$dta@containers$adam@default)
    expect_false(rv$dta@containers$sdtm@default)
    expect_true(any(grepl("'STUDY_ADAM_*.zip' now takes all other files; 'STUDY_SDTM_*.zip' no longer does.", shown, fixed = TRUE)))
  })
}))

test_that("removing a container asks first, can be called off, and clears every reference to it", ct_quiet({
  clean_session_file()
  spec <- ct_spec(mg_sdtm,
    ae = "{filename: ae.csv, type: csv, container: sdtm}",
    dm = "{filename: dm.csv, type: csv, container: STUDY_SDTM_*.zip}"
  )
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    unlock_editing(session)
    session$setInputs(open_containers = 1)
    expect_match(ui_text(output$ctr_modal_body), "2 files in 2 datasets", fixed = TRUE)
    session$setInputs(ctr_remove = 1)
    html <- flat_message(ui_text(output$ctr_modal_body))
    expect_match(html, "Remove STUDY_SDTM_*.zip? Its 2 files will no longer have to arrive inside it.", fixed = TRUE)
    session$setInputs(ctr_remove_keep = 1)
    expect_length(rv$dta@containers, 1)
    expect_false(grepl("Remove STUDY_SDTM_*.zip?", flat_message(ui_text(output$ctr_modal_body)), fixed = TRUE))
  })
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    unlock_editing(session)
    session$setInputs(open_containers = 1)
    ui_text(output$ctr_modal_body)
    session$setInputs(ctr_remove = 1)
    session$setInputs(ctr_remove_confirm = 1)
    expect_length(rv$dta@containers, 0)
    expect_true(is.na(mg_ref(rv$dta, "ae")))
    expect_true(is.na(mg_ref(rv$dta, "dm")))
    expect_false(grepl("containers:", rv$yaml_text, fixed = TRUE))
  })
}))

test_that("an implied container is materialised by editing it, with an id derived from the new name", {
  clean_session_file()
  spec <- ct_spec(ae = "{filename: ae.csv, type: csv, container: sdtm_csv.zip}")
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    unlock_editing(session)
    session$setInputs(open_containers = 1)
    expect_match(ui_text(output$ctr_modal_body), "sdtm_csv.zip", fixed = TRUE)
    expect_length(rv$dta@containers, 0)
    session$setInputs(ctr_edit = 1)
    session$setInputs(ctr_filename = "sdtm_csv_v2.zip", ctr_description = "Renamed", ctr_default = FALSE)
    session$setInputs(ctr_save = 1)
    expect_identical(names(rv$dta@containers), "sdtm_csv_v2")
    expect_identical(mg_ref(rv$dta, "ae"), "sdtm_csv_v2")
  })
})

test_that("a 0.27.0 document saved unchanged through the Files editor or Assign keeps its container", {
  clean_session_file()
  spec <- ct_spec(ae = "{filename: ae.csv, type: csv, container: sdtm_csv.zip}")
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    unlock_editing(session)
    session$setInputs(edit_files = 1)
    session$setInputs(file_edit_click = 1)
    html <- ui_text(output$file_modal_body)
    expect_match(html, '<option value="sdtm_csv.zip" selected>sdtm_csv.zip</option>', fixed = TRUE)

    # The select did not report: the stored reference is kept.
    mg_file_form(session, "ae.csv")
    session$setInputs(file_save = 1)
    expect_null(rv$file_msg)
    expect_identical(mg_ref(rv$dta, "ae"), "sdtm_csv.zip")
    expect_length(rv$dta@containers, 0)

    # The select reported what it shows.
    session$setInputs(file_edit_click = 2)
    session$setInputs(file_edit_click = 1)
    session$setInputs(file_container = "sdtm_csv.zip")
    session$setInputs(file_save = 2)
    expect_null(rv$file_msg)
    expect_identical(mg_ref(rv$dta, "ae"), "sdtm_csv.zip")
    expect_length(rv$dta@containers, 0)

    # Assign: saved unchanged, then with the select reporting what it shows.
    session$setInputs(open_containers = 1)
    session$setInputs(ctr_tab_assign = 1)
    html <- ui_text(output$ctr_modal_body)
    expect_match(html, '<option value="sdtm_csv.zip" selected>sdtm_csv.zip</option>', fixed = TRUE)
    session$setInputs(ctr_assign_save = 1)
    expect_identical(mg_ref(rv$dta, "ae"), "sdtm_csv.zip")

    session$setInputs(open_containers = 2)
    session$setInputs(ctr_tab_assign = 2)
    ui_text(output$ctr_modal_body)
    ct_set_input(session, sprintf("ctr_a_%s_1", rv$ctr_assign$nonce), "sdtm_csv.zip")
    session$setInputs(ctr_assign_save = 2)
    expect_identical(mg_ref(rv$dta, "ae"), "sdtm_csv.zip")
    expect_length(rv$dta@containers, 0)
  })
})

test_that("Assign sets, clears and keeps references by what each select reported", {
  clean_session_file()
  spec <- ct_spec(mg_sdtm,
    ae = "{filename: ae.csv, type: csv, container: sdtm}",
    dm = "{filename: dm.csv, type: csv, container: sdtm}"
  )
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    unlock_editing(session)
    session$setInputs(open_containers = 1)
    session$setInputs(ctr_tab_assign = 1)
    ui_text(output$ctr_modal_body)
    nonce <- rv$ctr_assign$nonce
    # ae's select reported "" (no container); dm's did not report at all.
    ct_set_input(session, sprintf("ctr_a_%s_1", nonce), "")
    session$setInputs(ctr_assign_save = 1)
    expect_true(is.na(mg_ref(rv$dta, "ae")))
    expect_identical(mg_ref(rv$dta, "dm"), "sdtm")
  })
})

test_that("Set all to answers every select", ct_quiet({
  clean_session_file()
  spec <- ct_spec(mg_sdtm)
  ct_notes()
  sent <- character(0)
  testthat::local_mocked_bindings(
    updateSelectInput = function(session, inputId, ..., selected = NULL) {
      sent <<- c(sent, paste(inputId, selected))
      invisible(NULL)
    },
    .package = "shiny"
  )

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    unlock_editing(session)
    session$setInputs(open_containers = 1)
    session$setInputs(ctr_tab_assign = 1)
    html <- ui_text(output$ctr_modal_body)
    expect_match(html, 'id="ctr_set_all"', fixed = TRUE)
    nonce <- rv$ctr_assign$nonce
    session$setInputs(ctr_set_all = "sdtm")
    session$setInputs(ctr_apply = 1)
    expect_identical(sent, paste(sprintf("ctr_a_%s_%d", nonce, 1:2), "sdtm"))
  })
}))

test_that("the Files editor offers the containers and the empty choice, and creates a typed one", ct_quiet({
  clean_session_file()
  spec <- ct_spec(mg_sdtm)
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    unlock_editing(session)
    session$setInputs(edit_files = 1)
    session$setInputs(file_edit_click = 1)
    html <- ui_text(output$file_modal_body)
    expect_match(html, 'id="file_container"', fixed = TRUE)
    expect_match(html, '<option value="sdtm">STUDY_SDTM_*.zip</option>', fixed = TRUE)
    # selectize drops "" options unless it is told to keep them.
    expect_match(html, "Loose or in any container", fixed = TRUE)
    expect_match(html, '"allowEmptyOption":true', fixed = TRUE)
    expect_match(html, '"create":true', fixed = TRUE)
    expect_match(html, "Containers are listed under Metadata \u25B8 Containers.", fixed = TRUE)

    # A new archive name typed into the select becomes an explicit container.
    mg_file_form(session, "ae.csv")
    session$setInputs(file_container = "adam.zip")
    session$setInputs(file_save = 1)
    expect_null(rv$file_msg)
    expect_setequal(names(rv$dta@containers), c("sdtm", "adam"))
    expect_identical(rv$dta@containers$adam@filename, "adam.zip")
    expect_identical(mg_ref(rv$dta, "ae"), "adam")
    expect_match(rv$yaml_text, "adam.zip", fixed = TRUE)

    # Typing the file name an existing container carries references its id.
    session$setInputs(file_edit_click = 2)
    session$setInputs(file_edit_click = 1)
    session$setInputs(file_container = "STUDY_SDTM_*.zip")
    session$setInputs(file_save = 2)
    expect_null(rv$file_msg)
    expect_length(rv$dta@containers, 2)
    expect_identical(mg_ref(rv$dta, "ae"), "sdtm")

    # A choice is stored as it is; "" clears.
    session$setInputs(file_edit_click = 2)
    session$setInputs(file_edit_click = 1)
    session$setInputs(file_container = "adam")
    session$setInputs(file_save = 3)
    expect_identical(mg_ref(rv$dta, "ae"), "adam")
    session$setInputs(file_edit_click = 2)
    session$setInputs(file_edit_click = 1)
    session$setInputs(file_container = "")
    session$setInputs(file_save = 4)
    expect_true(is.na(mg_ref(rv$dta, "ae")))
  })
}))

test_that("a typed archive name that cannot be a container is refused inline and nothing is created", {
  clean_session_file()
  spec <- ct_spec()
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    unlock_editing(session)
    session$setInputs(edit_files = 1)
    session$setInputs(file_edit_click = 1)
    ui_text(output$file_modal_body)
    before <- rv$dta
    mg_file_form(session, "ae.csv")
    session$setInputs(file_container = "x.tar")
    session$setInputs(file_save = 1)
    expect_false(rv$file_msg$ok)
    expect_match(rv$file_msg$error, "must name a .zip archive or a .xpt SAS transport file", fixed = TRUE)
    expect_identical(rv$dta, before)
  })
})

test_that("a Files-editor save that would make two datasets take one file name is refused", {
  clean_session_file()
  spec <- ct_spec(ae = mg_xpt("^lb[.]xpt$"), dm = mg_xpt("^lb_supp.*[.]xpt$"))
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    unlock_editing(session)
    expect_identical(nrow(DTAtools::file_name_conflicts(rv$dta)), 0L)
    session$setInputs(edit_files = 1)
    session$setInputs(file_edit_click = 1)
    before <- rv$dta
    mg_file_form(session, "^lb.*[.]xpt$", type = "xpt", pattern = TRUE)
    session$setInputs(file_save = 1)
    expect_false(rv$file_msg$ok)
    expect_match(rv$file_msg$error, "lb_supp.xpt", fixed = TRUE)
    expect_match(flat_message(ui_text(output$file_editor_msg)), "both take a file named 'lb_supp.xpt'", fixed = TRUE)
    expect_identical(rv$dta, before)

    # The create path is guarded as well.
    session$setInputs(file_back = 1)
    session$setInputs(file_add = 1)
    mg_file_form(session, "^lb_supp.*[.]xpt$", type = "xpt", pattern = TRUE)
    session$setInputs(file_save = 2)
    expect_false(rv$file_msg$ok)
    expect_identical(rv$dta, before)
  })
})

test_that("a document opened with an overlap warns, and an unrelated save is not refused", ct_quiet({
  clean_session_file()
  spec <- ct_spec(
    ae = sprintf("[%s, %s]", mg_xpt("^zz[.]xpt$"), mg_xpt("^lb.*[.]xpt$")),
    dm = mg_xpt("^lb_supp.*[.]xpt$")
  )
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    expect_identical(rv$dta@datasets$ae@files[[2]]@filename, "^lb.*[.]xpt$")
    warn <- "File names overlap with dataset 'dm' (e.g. 'lb_supp.xpt'). A delivered file matching both is asked about."
    html <- flat_message(ui_text(output$dataset_conflict_warning))
    expect_match(html, warn, fixed = TRUE)
    expect_false(grepl("Edit \u25B8 Files", html, fixed = TRUE))

    unlock_editing(session)
    expect_match(
      flat_message(ui_text(output$dataset_conflict_warning)),
      paste0(warn, " Edit \u25B8 Files to make them distinct."),
      fixed = TRUE
    )
    session$setInputs(edit_files = 1)
    expect_match(flat_message(ui_text(output$file_conflict_warning)), warn, fixed = TRUE)

    # Put an entry in front of the conflicting one: its index shifts, the
    # conflict does not change, and a save that does not touch it goes through.
    session$setInputs(file_add = 1)
    mg_file_form(session, "^yy[.]xpt$", type = "xpt", pattern = TRUE)
    session$setInputs(file_save = 1)
    expect_null(rv$file_msg)
    expect_length(rv$dta@datasets$ae@files, 3)
    session$setInputs(file_up_click = 3)
    session$setInputs(file_up_click = 2)
    expect_identical(rv$dta@datasets$ae@files[[1]]@filename, "^yy[.]xpt$")
    expect_identical(rv$dta@datasets$ae@files[[3]]@filename, "^lb.*[.]xpt$")
    session$setInputs(file_edit_click = 1)
    mg_file_form(session, "^yy2[.]xpt$", type = "xpt", pattern = TRUE)
    session$setInputs(file_save = 2)
    expect_null(rv$file_msg)
    expect_identical(rv$dta@datasets$ae@files[[1]]@filename, "^yy2[.]xpt$")
    expect_identical(nrow(DTAtools::file_name_conflicts(rv$dta)), 1L)
  })
}))

test_that("removing or assigning a container so that two scopes merge into a conflict is refused", {
  clean_session_file()
  # dm's pattern is inside archive.zip, ae's is loose: they never tie. Removing
  # the container frees dm's entry and now both are free; assigning ae into the
  # archive puts both inside it.
  spec <- ct_spec(
    c("containers:", "  - id: arc", "    filename: archive.zip"),
    ae = mg_xpt("^lb.*[.]xpt$"),
    dm = "{filename: '^lb_supp.*[.]xpt$', type: xpt, pattern: true, container: arc}"
  )
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    unlock_editing(session)
    expect_identical(nrow(DTAtools::file_name_conflicts(rv$dta)), 0L)
    session$setInputs(open_containers = 1)
    ui_text(output$ctr_modal_body)
    before <- rv$dta
    session$setInputs(ctr_remove = 1)
    session$setInputs(ctr_remove_confirm = 1)
    expect_identical(rv$dta, before)
    expect_match(flat_message(ui_text(output$ctr_msg)), "lb_supp.xpt", fixed = TRUE)
    expect_match(flat_message(ui_text(output$ctr_msg)), "Make the expected names distinct", fixed = TRUE)
    expect_length(rv$dta@containers, 1)

    session$setInputs(ctr_tab_assign = 1)
    ui_text(output$ctr_modal_body)
    ct_set_input(session, sprintf("ctr_a_%s_1", rv$ctr_assign$nonce), "arc")
    session$setInputs(ctr_assign_save = 1)
    expect_identical(rv$dta, before)
    expect_match(ui_text(output$ctr_msg), "lb_supp.xpt", fixed = TRUE)
  })
})

test_that("a container whose default tick or file name makes two scopes overlap is refused", {
  clean_session_file()
  # ae is inside archive.zip, dm's entry is loose. A new default container "*.zip"
  # would take dm's entry, and its glob overlaps archive.zip.
  spec <- ct_spec(
    c("containers:", "  - id: arc", "    filename: archive.zip"),
    ae = "{filename: '^lb.*[.]xpt$', type: xpt, pattern: true, container: arc}",
    dm = mg_xpt("^lb_supp.*[.]xpt$")
  )
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    unlock_editing(session)
    expect_identical(nrow(DTAtools::file_name_conflicts(rv$dta)), 0L)
    before <- rv$dta
    session$setInputs(open_containers = 1)
    session$setInputs(ctr_add = 1)
    session$setInputs(ctr_filename = "*.zip", ctr_description = "", ctr_default = TRUE)
    session$setInputs(ctr_save = 1)
    expect_identical(rv$dta, before)
    expect_match(flat_message(ui_text(output$ctr_msg)), "both take a file named 'lb_supp.xpt'", fixed = TRUE)
    expect_identical(rv$ctr_view, "form")
    # Without the default tick nothing overlaps, so the same file name is saved.
    session$setInputs(ctr_default = FALSE)
    session$setInputs(ctr_save = 2)
    expect_length(rv$dta@containers, 2)
  })
})

test_that("the Metadata tab lists the containers, offers the manager only in edit mode, and points to it", {
  clean_session_file()
  spec <- ct_spec(mg_sdtm, ae = "{filename: ae.csv, type: csv, container: sdtm}")
  ct_modals()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    html <- ui_text(output$metadata_editor)
    expect_match(html, "File names and contents of the delivery archives are defined under Containers below.", fixed = TRUE)
    expect_match(html, 'id="metadata_containers"', fixed = TRUE)
    expect_false(grepl("metadata_manage_containers", html, fixed = TRUE))
    # It follows Transmission and precedes the agreement sections.
    expect_lt(regexpr("Blinding requirements", html, fixed = TRUE), regexpr('id="metadata_containers"', html, fixed = TRUE))
    expect_lt(regexpr('id="metadata_containers"', html, fixed = TRUE), regexpr("Agreement sections", html, fixed = TRUE))
    list_html <- ui_text(output$metadata_containers)
    expect_match(list_html, "STUDY_SDTM_*.zip", fixed = TRUE)
    expect_match(list_html, "SDTM data", fixed = TRUE)
    expect_match(list_html, "1 file in 1 dataset", fixed = TRUE)

    unlock_editing(session)
    html <- ui_text(output$metadata_editor)
    expect_match(html, 'id="metadata_manage_containers"', fixed = TRUE)
    expect_match(html, "Manage containers\u2026", fixed = TRUE)
    session$setInputs(metadata_manage_containers = 1)
    expect_match(utils::tail(modals, 1), "ctr_modal_body", fixed = TRUE)
    expect_true(rv$ctr_open)
    expect_match(ui_text(output$ctr_modal_body), 'id="ctr_add"', fixed = TRUE)
  })
})

test_that("the manager's body is empty once it is closed, and the form's match line reads the last delivery", {
  clean_session_file()
  spec <- ct_spec()
  zip1 <- cont_fx("sdtm_csv.zip")
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(zip1, "sdtm_csv.zip"))
    unlock_editing(session)
    expect_identical(ui_text(output$ctr_modal_body), "")
    session$setInputs(open_containers = 1)
    session$setInputs(ctr_add = 1)
    session$setInputs(ctr_filename = "sdtm_*.zip")
    expect_match(ui_text(output$ctr_match), "Matches sdtm_csv.zip from the last delivery.", fixed = TRUE)
    session$setInputs(ctr_filename = "other_*.zip")
    expect_match(ui_text(output$ctr_match), "Matches none of the last delivery's archives (sdtm_csv.zip).", fixed = TRUE)

    # The browser reports the dialog hidden: the body empties, so a reopened
    # dialog never shows the cached form.
    session$setInputs(dta_modal_state = list(event = "hidden", kind = "containers", id = rv$ctr_nonce, at = 1))
    expect_false(rv$ctr_open)
    expect_identical(ui_text(output$ctr_modal_body), "")
    expect_identical(ui_text(output$ctr_match), "")
  })
})

test_that("a second ambiguity round keeps the first answers and asks only the new files", ct_quiet({
  clean_session_file()
  # sdtm.xpt is taken by an xpt pattern and by an any pattern listing xpt: it is
  # asked about. Routed to ae, the library is opened and its members are ambiguous
  # the same way: they are asked about in a second round (S5r).
  spec <- ct_yaml(c(
    ct_header, "datasets:", ct_ds("ae", "{filename: '[.]xpt$', type: xpt, pattern: true}", ct_ae_cols),
    "  - id: docs", "    type: file",
    "    files: {filename: '[.]xpt$', type: any, pattern: true, number_of_files: 1, extensions: [xpt]}"
  ))
  lib <- cont_fx("sdtm.xpt")
  ct_modals()
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(lib, "sdtm.xpt"))
    pd <- rv$pending_delivery
    expect_false(is.null(pd))
    expect_identical(vapply(pd$rows, function(r) r$name, ""), "sdtm.xpt")
    expect_length(modals, 1)
    ct_set_input(session, sprintf("amb_%s_1", pd$nonce), "ae")
    session$setInputs(amb_load = 1)

    # The library was opened; its members are the second round's rows.
    pd2 <- rv$pending_delivery
    expect_false(is.null(pd2))
    expect_length(modals, 2)
    expect_false(identical(pd2$nonce, pd$nonce))
    expect_identical(vapply(pd2$rows, function(r) r$name, ""), c("ae.xpt", "dm.xpt", "vs.xpt"))
    expect_identical(names(pd2$route), "sdtm.xpt")
    expect_identical(pd2$route[["sdtm.xpt"]], "ae")
    expect_length(DTAtools::tables(DTAtools::datasets(rv$dta, "ae")), 0)

    for (i in 1:3) ct_set_input(session, sprintf("amb_%s_%d", pd2$nonce, i), "ae")
    session$setInputs(amb_load = 2)
    expect_null(rv$pending_delivery)
    expect_setequal(names(DTAtools::tables(DTAtools::datasets(rv$dta, "ae"))), c("ae", "dm", "vs"))
  })
}))

# =====================================================================================
# UX review fixes. The rule: nothing says Passed while a packaging finding exists; the
# state is in words, not only in colour; and no regular expression or dataset id is
# shown where a description or a name exists.
# =====================================================================================

test_that("a dataset whose file is outside its container never reads Passed: chip, row, bar, toast, button", {
  clean_session_file()
  spec <- ct_spec(ct_default)
  both <- c(cont_write("ae.csv", CONT_AE_CSV), cont_write("dm.csv", CONT_DM_CSV))
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(both, c("ae.csv", "dm.csv")))
    session$setInputs(check_all = 1)
    # rv$status stays the CONTENT status; the display is derived.
    expect_identical(unlist(rv$status), c(ae = "pass", dm = "pass"))

    chip <- ui_text(output$dataset_status_line)
    expect_match(chip, "Content passed · not inside its container", fixed = TRUE)
    expect_match(chip, "status-fail", fixed = TRUE)
    expect_false(grepl("Passed\\s*</span>", chip))
    expect_match(chip, "ds-st-fail", fixed = TRUE)

    nav <- ui_text(output$dataset_nav_ui)
    expect_match(nav, "nav-st-fail", fixed = TRUE)
    expect_false(grepl("nav-st-pass", nav, fixed = TRUE))
    expect_match(nav, 'aria-label="Content passed, but a file is not inside its container"', fixed = TRUE)

    bar <- ui_text(output$summary_metrics)
    expect_match(bar, "packaging issues", fixed = TRUE)
    expect_match(bar, "0 passed, 2 with a packaging issue", fixed = TRUE)
    expect_false(grepl("st-pass", bar, fixed = TRUE))

    expect_true(startsWith(
      utils::tail(shown, 1), "Content: 2 passed; packaging: 2 files not inside their container."
    ))

    button <- ui_text(output$validation_report_ui)
    expect_match(button, "Validation summary (FAILED: packaging)", fixed = TRUE)
    expect_match(button, "btn-outline-danger", fixed = TRUE)
    expect_false(grepl("btn-success", button, fixed = TRUE))

    # The report counts the findings and says why it fails.
    html <- validation_report_html()
    expect_match(html, "2 packaging findings", fixed = TRUE)
    expect_match(html, "Files arrived outside the container the agreement requires, so this DTA fails.", fixed = TRUE)
  })
})

test_that("a dataset checked on its own reads Passed with the packaging unverified, and the toast and button say so", {
  clean_session_file()
  spec <- ct_spec(ct_default)
  ae <- cont_write("ae.csv", CONT_AE_CSV)
  dm <- cont_write("dm.csv", CONT_DM_CSV)
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = ct_fi(ae, "ae.csv"))
    session$setInputs(up_2_1 = ct_fi(dm, "dm.csv"))
    session$setInputs(check_all = 1)

    chip <- ui_text(output$dataset_status_line)
    expect_match(chip, "Passed · packaging not verified", fixed = TRUE)
    expect_match(chip, "status-nodata", fixed = TRUE)
    expect_match(ui_text(output$dataset_nav_ui), "nav-st-nodata", fixed = TRUE)
    expect_true(startsWith(
      utils::tail(shown, 1), "Content: 2 passed; packaging: 2 files whose packaging is not verified."
    ))
    expect_match(ui_text(output$validation_report_ui), "Validation summary (INCOMPLETE: packaging)", fixed = TRUE)
    expect_false(grepl("so this DTA fails", validation_report_html(), fixed = TRUE))
  })
})

test_that("a dataset with no finding still reads plain Passed, and the button has no state in its label", {
  clean_session_file()
  spec <- ct_spec(ct_default)
  good <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "delivery.zip")
  ct_notes()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(good, "delivery.zip"))
    session$setInputs(check_all = 1)
    expect_match(ui_text(output$dataset_status_line), "Passed\\s*</span>")
    expect_false(grepl("packaging", ui_text(output$dataset_status_line), fixed = TRUE))
    expect_match(utils::tail(shown, 1), "Validation complete", fixed = TRUE)
    expect_false(grepl("packaging", utils::tail(shown, 1), fixed = TRUE))
    expect_match(ui_text(output$validation_report_ui), "Validation summary\\s*</a>")
  })
})

test_that("the sidebar names a container with no files assigned", ct_quiet({
  clean_session_file()
  spec <- ct_spec(mg_sdtm)

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    panel <- ui_text(output$delivery_panel)
    expect_match(panel, "STUDY_SDTM_*.zip: no files assigned", fixed = TRUE)
    expect_false(grepl("not delivered yet", panel, fixed = TRUE))
    # The whole text is on the line, so a long name never hides the state words.
    expect_match(panel, 'title="STUDY_SDTM_*.zip: no files assigned"', fixed = TRUE)
  })
}))

test_that("the Delivery view says no packaging rules, offers a way back, and puts the time first", {
  clean_session_file()
  spec <- ct_spec()
  zip1 <- cont_fx("sdtm_csv.zip")
  ct_select_tab()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(zip1, "sdtm_csv.zip"))
    expect_identical(rv$main_view, "delivery")
    view <- ui_text(output$dataset_detail)
    expect_match(view, "No packaging rules", fixed = TRUE)
    expect_false(grepl("Packaging conforms", view, fixed = TRUE))
    expect_match(view, 'id="delivery_back"', fixed = TRUE)
    expect_match(view, "Back to ae", fixed = TRUE)
    expect_match(view, "Last delivery, [0-9]{2}:[0-9]{2}: sdtm_csv.zip — 2 files loaded", perl = TRUE)

    # No dataset is highlighted while the view stands in for them; going back restores it.
    expect_false(grepl("dataset-nav-row nav-st-[a-z]+ active", ui_text(output$dataset_nav_ui)))
    session$setInputs(delivery_back = 1)
    expect_identical(rv$main_view, "dataset")
    expect_match(ui_text(output$dataset_nav_ui), "dataset-nav-row nav-st-[a-z]+ active")
  })
})

test_that("the Note column drops the redundant 'loaded, but' and the Dataset column uses names", {
  clean_session_file()
  ae <- append(ct_ds("ae", "{filename: ae.csv, type: csv}", ct_ae_cols), "    name: Adverse events", after = 1)
  spec <- ct_yaml(c(
    ct_header, ct_default, "datasets:", ae,
    ct_ds("dm", "{filename: dm.csv, type: csv}", ct_dm_cols)
  ))
  loose <- cont_write("ae.csv", CONT_AE_CSV)

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = ct_fi(loose, "ae.csv"))
    view <- ui_text(output$dataset_detail)
    expect_false(grepl("loaded, but", view, fixed = TRUE))
    expect_match(view, "it arrived loose and the agreement requires it inside", fixed = TRUE)
    expect_match(view, "<td>Adverse events</td>", fixed = TRUE)
    expect_match(view, "<td>loaded</td>", fixed = TRUE)
  })
})

test_that("a pattern entry's slot hint uses its description, never the regex", {
  clean_session_file()
  described <- ct_spec(
    ct_default,
    ae = "{filename: \"^a.*[.]csv$\", type: csv, pattern: true, pattern_description: any AE extract}"
  )
  bare <- ct_spec(ct_default, ae = "{filename: \"^a.*[.]csv$\", type: csv, pattern: true}")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(described, "spec.yaml"))
    html <- ui_text(output$dataset_detail)
    expect_match(html, "or drop any AE extract here to check it on its own.", fixed = TRUE)
    expect_false(grepl("or drop ^a.*[.]csv$", html, fixed = TRUE))
  })
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(bare, "spec.yaml"))
    expect_match(ui_text(output$dataset_detail), "or drop a matching file here", fixed = TRUE)
  })
})

test_that("the file-name overlap warning and refusal name datasets, and the Files editor words its own fix", ct_quiet({
  clean_session_file()
  named <- function(id, name, files, cols) append(ct_ds(id, files, cols), paste0("    name: ", name), after = 1)
  spec <- ct_yaml(c(
    ct_header, "datasets:",
    named("ae", "Adverse events", "{filename: \"^a.*[.]csv$\", type: csv, pattern: true}", ct_ae_cols),
    named("dm", "Demographics", "{filename: \"^.*e[.]csv$\", type: csv, pattern: true}", ct_dm_cols)
  ))

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    expect_match(ui_text(output$dataset_conflict_warning), "overlap with dataset 'Demographics'", fixed = TRUE)
    expect_false(grepl("Edit", ui_text(output$dataset_conflict_warning), fixed = TRUE))

    unlock_editing(session)
    expect_match(ui_text(output$dataset_conflict_warning), "Edit ▸ Files to make them distinct.", fixed = TRUE)
    session$setInputs(edit_files = 1)
    inner <- ui_text(output$file_conflict_warning)
    expect_match(inner, "Make the expected names distinct.", fixed = TRUE)
    expect_false(grepl("Edit ▸ Files", inner, fixed = TRUE))

    conf <- DTAtools::file_name_conflicts(rv$dta)
    refusal <- app_fn("dta_conflict_refusal")
    expect_match(refusal(conf, rv$dta), "datasets 'Adverse events' and 'Demographics'", fixed = TRUE)
    expect_match(refusal(conf), "datasets 'ae' and 'dm'", fixed = TRUE)
  })
}))

test_that("the default container's summary names the datasets it excepts, and the Files list says what none means", {
  clean_session_file()
  named <- append(
    ct_ds("ae", "{filename: ae.csv, type: csv, container: none}", ct_ae_cols), "    name: Adverse events",
    after = 1
  )
  spec <- ct_yaml(c(
    ct_header, ct_default, "datasets:", named, ct_ds("dm", "{filename: dm.csv, type: csv}", ct_dm_cols)
  ))
  dta <- DTAtools::read_dta_from_yaml(spec)
  rows <- app_fn("dta_container_rows")(dta)
  expect_match(rows[[1]]$summary, "except: ae.csv (Adverse events)", fixed = TRUE)
  expect_false(grepl("(ae)", rows[[1]]$summary, fixed = TRUE))

  # The Files editor's list says what "none" means instead of a blank cell.
  overview <- app_fn("dv_files_overview")
  expect_identical(overview(dta, "ae")$container, "No container required")
  expect_identical(overview(dta, "dm")$container, "delivery.zip")
})

test_that("the manager is the Delivery containers dialog, says when it is view only, and labels its controls", {
  clean_session_file()
  spec <- ct_spec(
    mg_sdtm,
    ae = "{filename: ae.csv, type: csv, container: sdtm}",
    dm = "{filename: dm.csv, type: csv, container: other.zip}"
  )
  ct_modals()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    session$setInputs(open_containers = 1)
    expect_match(modals, "Delivery containers", fixed = TRUE)
    expect_match(modals, 'id="ctr_modal_footer"', fixed = TRUE)
    html <- ui_text(output$ctr_modal_body)
    expect_match(html, "View only — switch on Edit mode to change containers.", fixed = TRUE)
    expect_match(html, "defined by file entries (no container record yet)", fixed = TRUE)
    expect_false(grepl("named by the entries only", html, fixed = TRUE))
    expect_match(html, 'role="tab"', fixed = TRUE)
    expect_match(html, 'aria-selected="true"', fixed = TRUE)
    expect_match(ui_text(output$ctr_modal_footer), "Close", fixed = TRUE)
  })
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    unlock_editing(session)
    session$setInputs(open_containers = 1)
    html <- ui_text(output$ctr_modal_body)
    expect_false(grepl("View only", html, fixed = TRUE))
    expect_match(html, 'aria-label="Edit STUDY_SDTM_*.zip"', fixed = TRUE)
    expect_match(html, 'aria-label="Remove STUDY_SDTM_*.zip"', fixed = TRUE)
  })
})

test_that("the footer follows the view: no Close in the form, Cancel and Save on Assign, Cancel goes back to the list", ct_quiet({
  clean_session_file()
  spec <- ct_spec(mg_sdtm)
  ct_modals()
  ct_notes()
  removed <- 0L
  testthat::local_mocked_bindings(removeModal = function(...) removed <<- removed + 1L, .package = "shiny")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    unlock_editing(session)
    session$setInputs(open_containers = 1)
    expect_match(ui_text(output$ctr_modal_footer), "Close", fixed = TRUE)

    session$setInputs(ctr_add = 1)
    expect_identical(ui_text(output$ctr_modal_footer), "")
    session$setInputs(ctr_cancel = 1)
    expect_match(ui_text(output$ctr_modal_footer), "Close", fixed = TRUE)

    session$setInputs(ctr_tab_assign = 1)
    foot <- ui_text(output$ctr_modal_footer)
    expect_match(foot, 'id="ctr_assign_save"', fixed = TRUE)
    expect_match(foot, 'id="ctr_assign_cancel"', fixed = TRUE)
    expect_false(grepl("Close", foot, fixed = TRUE))
    # Save and Cancel are not in the scrolling body.
    expect_false(grepl("ctr_assign_save", ui_text(output$ctr_modal_body), fixed = TRUE))

    removed <- 0L
    session$setInputs(ctr_assign_cancel = 1)
    expect_true(rv$ctr_open)
    expect_identical(rv$ctr_tab, "containers")
    expect_identical(rv$ctr_view, "list")
    expect_identical(removed, 0L)
    expect_match(ui_text(output$ctr_modal_body), "+ Add container", fixed = TRUE)
  })
}))

test_that("the Assign tab filters, sizes and labels its rows, and Set the shown rows to applies to those shown", ct_quiet({
  clean_session_file()
  spec <- ct_spec(mg_sdtm)
  ct_notes()
  sent <- character(0)
  testthat::local_mocked_bindings(
    updateSelectInput = function(session, inputId, ..., selected = NULL) {
      sent <<- c(sent, paste(inputId, selected))
      invisible(NULL)
    },
    .package = "shiny"
  )

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = ct_fi(spec, "spec.yaml"))
    unlock_editing(session)
    session$setInputs(open_containers = 1)
    session$setInputs(ctr_tab_assign = 1)
    html <- ui_text(output$ctr_modal_body)
    nonce <- rv$ctr_assign$nonce
    expect_match(html, 'id="ctr_filter"', fixed = TRUE)
    expect_match(html, "Set the shown rows to", fixed = TRUE)
    expect_false(grepl("Set all to", html, fixed = TRUE))
    expect_match(html, "ctr-assign-scroll", fixed = TRUE)
    expect_match(html, "form-select-sm", fixed = TRUE)
    expect_match(html, 'aria-label="Delivered inside for ae ae.csv"', fixed = TRUE)
    expect_match(html, 'aria-label="Delivered inside for dm dm.csv"', fixed = TRUE)
    # Every row carries what the browser's filter matches on, dataset first.
    expect_match(html, 'data-ctr-text="ae ae ae.csv"', fixed = TRUE)

    # Only row 2 is shown by the filter: only its select is set.
    session$setInputs(ctr_set_all = "sdtm", ctr_shown = "2")
    session$setInputs(ctr_apply = 1)
    expect_identical(sent, sprintf("ctr_a_%s_2 sdtm", nonce))
    # Nothing shown, nothing set.
    sent <- character(0)
    session$setInputs(ctr_shown = "")
    session$setInputs(ctr_apply = 2)
    expect_identical(sent, character(0))
  })
}))
