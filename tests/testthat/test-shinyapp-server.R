# Reactive tests for the app's server function, driven through
# shiny::testServer() against the bundled app directory.
#
# These cover state TRANSITIONS in `rv` — the app's single source of truth —
# rather than rendered UI. Anything driven by a modal, a custom message, or a
# dynamically registered contact/nav button is out of reach here by design;
# those need an end-to-end driver.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

# The app directory testServer() runs.
app_server_dir <- function() .shiny_app_dir()

# A shiny fileInput value for a bundled fixture.
#
# fileInput() hands the server a one-row data.frame; the upload observers read
# `datapath` and `name` off it, so the fixture has to be presented the same way.
app_file_input <- function(filename) {
  path <- app_fixture_path(filename)
  data.frame(
    name = filename,
    size = file.size(path),
    type = "",
    datapath = path,
    stringsAsFactors = FALSE
  )
}

# The app autosaves to a fixed path in tempdir() on every state change, and
# offers to restore it on start-up. A file left by an earlier test would put the
# next server into "previous session available" state, so every test clears it
# before starting. (Clearing up front, rather than after, is what actually
# guarantees isolation — and it stays correct no matter what order testthat
# runs these in. The file itself dies with tempdir() at the end of the session.)
clean_session_file <- function() {
  f <- file.path(tempdir(), "dtatools_app_session.rds")
  unlink(f, force = TRUE)
  f
}

test_that("the server starts with an empty workspace", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    expect_null(rv$dta)
    expect_null(rv$active)
    expect_null(rv$yaml_text)
    expect_length(rv$uploads, 0)
    expect_length(rv$status, 0)
    expect_false(rv$dataset_only)
    expect_false(rv$is_example)
    expect_equal(rv$col_view, "list")
    expect_equal(rv$rule_view, "list")
  })
})

test_that("loading a DTA YAML populates the workspace and marks datasets pending", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))

    expect_s3_class(rv$dta, "DTAtools::DTA")
    expect_equal(names(DTAtools::datasets(rv$dta)), "clinical_data")
    # A freshly loaded spec has no data bound yet, so nothing can be validated.
    expect_equal(rv$status, c(clinical_data = "pending"))
    expect_equal(rv$active, "clinical_data")
    expect_length(rv$uploads, 0)
    expect_gt(nchar(rv$yaml_text), 0)

    # rv$structure drives the upload slots; one entry per dataset, each
    # carrying its file handlers.
    expect_equal(names(rv$structure), "clinical_data")
    expect_equal(rv$structure$clinical_data$index, 1)
    expect_length(rv$structure$clinical_data$handlers, 1)
  })
})

test_that("a standalone dataset YAML is wrapped into a full DTA workspace", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("gf_dataset.yaml"))

    # gf_dataset.yaml has no metadata/datasets keys — the app wraps it in a new
    # empty DTA so the user lands in the normal workspace.
    expect_s3_class(rv$dta, "DTAtools::DTA")
    expect_equal(names(DTAtools::datasets(rv$dta)), "gf_data_specs_pattern")
    # dataset_only is a legacy flag that must never be set again; the raw view
    # would switch to a restricted mode if it were.
    expect_false(rv$dataset_only)
    # The raw YAML view shows the wrapped DTA, not the original dataset file.
    expect_match(rv$yaml_text, "gf_data_specs_pattern", fixed = TRUE)
  })
})

test_that("binding a data file registers an upload against its dataset slot", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))

    # Uploads are keyed "<dataset>||<handler index>".
    expect_equal(names(rv$uploads), "clinical_data||1")
    # Binding data does not validate it — status stays pending until a check.
    expect_equal(rv$status, c(clinical_data = "pending"))
    expect_equal(
      DTAtools::tables(DTAtools::datasets(rv$dta, "clinical_data")) |> names(),
      "clinical_data"
    )
  })
})

test_that("check_all moves a clean dataset from pending to pass", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    expect_equal(rv$status, c(clinical_data = "pending"))

    session$setInputs(check_all = 1)
    expect_equal(rv$status, c(clinical_data = "pass"))
  })
})

test_that("check_all reports fail for data that violates the spec", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = app_file_input("clinical_data_error_all.csv"))

    session$setInputs(check_all = 1)
    expect_equal(rv$status, c(clinical_data = "fail"))

    # The failure must be backed by real messages, otherwise the dock is empty
    # and the user is told "fail" with nothing to act on.
    msgs <- DTAtools::messages(rv$dta, as_tibble = FALSE)
    expect_gt(nrow(msgs), 0)
    expect_setequal(unique(msgs$source), c("rule", "schema"))
  })
})

test_that("applying malformed YAML reports an error and leaves the DTA untouched", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    loaded <- rv$dta
    yaml_before <- rv$yaml_text

    session$setInputs(raw_yaml_editor = "datasets: [unclosed", apply_yaml = 1)

    # The rollback guarantee: a bad edit must not replace the loaded document.
    expect_false(isTRUE(rv$yaml_msg$ok))
    expect_true(nzchar(rv$yaml_msg$error))
    expect_identical(rv$dta, loaded)
    expect_identical(rv$yaml_text, yaml_before)
  })
})

test_that("applying valid YAML replaces the loaded DTA", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    # NB: in R's default regex engine `.` matches newlines too, so the character
    # class is what keeps this edit to a single line.
    edited <- sub("title:[^\n]*", "title: Retitled by test", rv$yaml_text)

    session$setInputs(raw_yaml_editor = edited, apply_yaml = 1)

    expect_true(isTRUE(rv$yaml_msg$ok))
    expect_equal(
      as.character(S7::prop(DTAtools::metadata(rv$dta), "title"))[1],
      "Retitled by test"
    )
    # The dataset must survive an edit that only touched metadata.
    expect_equal(names(DTAtools::datasets(rv$dta)), "clinical_data")
  })
})

test_that("reverting the YAML editor discards the pending error", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    session$setInputs(raw_yaml_editor = "datasets: [unclosed", apply_yaml = 1)
    expect_true(nzchar(rv$yaml_msg$error))

    session$setInputs(revert_yaml = 1)
    expect_null(rv$yaml_msg)
  })
})

test_that("confirming a reset clears every piece of workspace state", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    session$setInputs(check_all = 1)
    expect_equal(rv$status, c(clinical_data = "pass"))

    session$setInputs(reset_app = 1, confirm_reset = 1)

    expect_null(rv$dta)
    expect_null(rv$active)
    expect_null(rv$yaml_text)
    expect_length(rv$uploads, 0)
    expect_length(rv$status, 0)
  })
})

test_that("opening the column editor targets the active dataset", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    expect_null(rv$editor_dataset)

    session$setInputs(edit_cols = 1)

    expect_equal(rv$editor_dataset, "clinical_data")
    expect_equal(rv$col_view, "list")
    # The token drives the modal body re-render; without a bump the editor
    # opens showing stale content.
    expect_gt(rv$col_token, 0)

    session$setInputs(col_add = 1)
    expect_equal(rv$col_view, "form")
    expect_null(rv$col_edit_id) # NULL id == adding, not editing
  })
})

test_that("saving a new column updates the spec, the YAML view and clears validation", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    session$setInputs(check_all = 1)
    expect_equal(rv$status, c(clinical_data = "pass"))

    cols_before <- names(DTAtools::columns(DTAtools::datasets(rv$dta, "clinical_data")))

    session$setInputs(edit_cols = 1)
    session$setInputs(col_add = 1)
    session$setInputs(
      col_id = "ZZTEST", col_label = "Test column", col_backend = "SAS",
      col_type = "Char", col_format = "", col_length = "10",
      col_nullable = TRUE, col_values = "", col_pattern = "",
      col_desc = "Added by the test suite"
    )
    session$setInputs(col_save = 1)

    # A successful save clears the inline message and returns to the list view;
    # there is no ok = TRUE message to look for.
    expect_null(rv$col_msg)
    expect_equal(rv$col_view, "list")

    cols_after <- names(DTAtools::columns(DTAtools::datasets(rv$dta, "clinical_data")))
    expect_setequal(cols_after, c(cols_before, "ZZTEST"))

    # Editing the spec must invalidate the result computed against the old one,
    # otherwise the UI keeps showing a green "pass" for a spec that was never
    # validated.
    expect_equal(rv$status, c(clinical_data = "pending"))
    # ... and the raw YAML view must reflect the edit.
    expect_match(rv$yaml_text, "ZZTEST", fixed = TRUE)
  })
})

test_that("a column save with an incomplete type is rejected without touching the spec", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    session$setInputs(check_all = 1)

    cols_before <- names(DTAtools::columns(DTAtools::datasets(rv$dta, "clinical_data")))

    session$setInputs(edit_cols = 1)
    session$setInputs(col_add = 1)
    session$setInputs(
      col_id = "ZZBAD", col_label = "", col_backend = "",
      col_type = "Char", col_format = "", col_length = "",
      col_nullable = TRUE, col_values = "", col_pattern = "", col_desc = ""
    )
    session$setInputs(col_save = 1)

    expect_false(isTRUE(rv$col_msg$ok))
    expect_true(nzchar(rv$col_msg$error))
    # The form must stay open on the rejected input rather than dropping the
    # user back to the list and losing what they typed.
    expect_equal(rv$col_view, "form")

    # A rejected save must be a no-op: no column added, and the existing
    # validation result must NOT be thrown away.
    expect_setequal(
      names(DTAtools::columns(DTAtools::datasets(rv$dta, "clinical_data"))),
      cols_before
    )
    expect_equal(rv$status, c(clinical_data = "pass"))
  })
})
