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

# The app autosaves to a path in tempdir() keyed to the browser id reported by
# the client (input$dta_client_id), and offers to restore it on start-up. A file
# left by an earlier test would put the next server into "previous session
# available" state, so every test clears them before starting. (Clearing up
# front, rather than after, is what actually guarantees isolation — and it stays
# correct no matter what order testthat runs these in. The files themselves die
# with tempdir() at the end of the session.)
#
# testServer() reports no browser id unless a test sets one, so a server under
# test writes nothing at all by default; this sweep covers the tests that do set
# one, and any file left by an older build under the pre-2.x fixed name.
clean_session_file <- function() {
  f <- list.files(tempdir(),
    pattern = "^dtatools_app_session.*\\.rds$",
    full.names = TRUE
  )
  unlink(f, force = TRUE)
  invisible(f)
}

# The rendered HTML of a renderUI output, as one string.
#
# renderUI yields list(html=, deps=); NULL from the render function leaves no
# html at all, so collapse both shapes to a plain string. A renderUI output is
# never evaluated unless a test reads it, so anything asserted about a UI gate
# has to go through here.
ui_text <- function(out) {
  if (is.null(out) || is.null(out$html)) "" else paste(as.character(out$html), collapse = "")
}

# The markup of ONE control out of a rendered button group, sliced out so a
# per-button assertion cannot be satisfied by a sibling's markup.
#
# Do NOT assert on the string "disabled" here: shiny renders every
# downloadButton with class "disabled" and aria-disabled, and only its
# client-side binding clears them. The server-side tell is the binding class
# itself -- `shiny-download-link` present means a live download, absent means
# the app's inert stand-in.
btn_html <- function(out, id) {
  parts <- strsplit(ui_text(out), "<(a|button) ", perl = TRUE)[[1]]
  hit <- parts[grepl(paste0('id="', id, '"'), parts, fixed = TRUE)]
  expect_length(hit, 1)
  hit[[1]]
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

# --- autosave slot isolation ------------------------------------------------
#
# "Restore previous session" has to outlive the Shiny session to be worth
# anything (it exists to recover a reload or a crash), so the autosave cannot be
# keyed to session$token, which is minted afresh on every page load. It is keyed
# instead to a random id the browser keeps in localStorage. These tests pin the
# two properties that keying buys: the id is validated before it is allowed near
# a path, and a payload written by one browser is not loadable by another.

test_that("nothing is autosaved until the browser reports an id", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    expect_null(client_id())
    expect_null(session_file())

    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))

    # State loaded, but with nowhere to persist it there must be no slot on disk.
    expect_s3_class(rv$dta, "DTAtools::DTA")
    expect_length(list.files(tempdir(), pattern = "^dtatools_app_session"), 0)
  })
})

test_that("a malformed browser id is refused instead of reaching the filesystem", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # input$dta_client_id is client-supplied and can be any string, so it is
    # re-validated server-side; only 32 lowercase hex chars are accepted, which
    # also makes it path-safe by construction.
    for (bad in list(
      "../../../../etc/passwd",
      "..\\..\\windows\\win.ini",
      "a/b",
      "ABCDEF0123456789abcdef0123456789", # uppercase
      strrep("a", 31),
      strrep("a", 33),
      "",
      NA_character_
    )) {
      session$setInputs(dta_client_id = bad)
      expect_null(client_id())
      expect_null(session_file())
    }

    good <- strrep("a", 32)
    session$setInputs(dta_client_id = good)
    expect_equal(client_id(), good)
    expect_equal(
      basename(session_file()),
      paste0("dtatools_app_session_", good, ".rds")
    )
    expect_equal(
      normalizePath(dirname(session_file()), winslash = "/", mustWork = FALSE),
      normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
    )
  })
})

test_that("a browser restores the workspace from its own autosave", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = strrep("b", 32))
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    expect_true(file.exists(session_file()))

    # Simulate the reload: the slot on disk is all that survives.
    rv$dta <- NULL
    rv$structure <- NULL
    rv$active <- NULL
    session$setInputs(restore_session = 1)

    expect_s3_class(rv$dta, "DTAtools::DTA")
    expect_equal(names(DTAtools::datasets(rv$dta)), "clinical_data")
    expect_equal(rv$active, "clinical_data")
  })
})

test_that("an autosave carrying a different browser id is not restored", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = strrep("c", 32))
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))

    # Re-stamp this browser's slot as though another browser had written it,
    # which is what an attacker who guessed the filename would be doing.
    payload <- readRDS(session_file())
    payload$client_id <- strrep("d", 32)
    saveRDS(payload, session_file())

    rv$dta <- NULL
    rv$structure <- NULL
    rv$active <- NULL
    session$setInputs(restore_session = 2)

    expect_null(rv$dta)
    expect_null(rv$structure)
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
    # clinical_data_error_all.csv now also carries import errors (see
    # test-clinical-error-fixtures.R), so "import" joins the source set.
    expect_setequal(unique(msgs$source), c("import", "rule", "columnspec"))
  })
})

test_that("applying malformed YAML reports an error and leaves the DTA untouched", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
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
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
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
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
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
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
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
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
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

test_that("saving a group_condition rule from the rule editor updates spec and YAML", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)

    before_n <- length(DTAtools::datasets(rv$dta, "clinical_data")@specs@rules)

    session$setInputs(edit_rules = 1)
    session$setInputs(rule_add = 1)
    session$setInputs(rule_type = "group_condition")

    session$setInputs(
      rule_id = "sample_visit_status_logic",
      rule_desc = "Grouped check",
      rule_group_by = c("SUBJECT_ID", "VISIT"),
      gcond_name_1 = "c1_failed",
      gcond_col_1 = "STATUS",
      gcond_op_1 = "equals",
      gcond_val_1 = "FAILED"
    )

    session$setInputs(gcond_add = 1)
    session$setInputs(
      gcond_name_2 = "c2_reported",
      gcond_col_2 = "CONSENT_DATE",
      gcond_op_2 = "empty",
      gcond_val_2 = "false"
    )

    session$setInputs(
      gconstr_id_1 = "no_failed_and_reported",
      gconstr_type_1 = "mutually_exclusive",
      gconstr_left_1 = "c1_failed",
      gconstr_right_1 = "c2_reported",
      gconstr_lscope_1 = "any",
      gconstr_rscope_1 = "any",
      gconstr_msg_1 = "Conflict in group"
    )

    session$setInputs(rule_save = 1)

    expect_null(rv$rule_msg)
    expect_equal(rv$rule_view, "list")

    rules <- DTAtools::datasets(rv$dta, "clinical_data")@specs@rules
    expect_equal(length(rules), before_n + 1)

    new_rule <- rules[[length(rules)]]
    expect_s3_class(new_rule, "DTAtools::DTARuleGroupCondition")
    expect_equal(new_rule@id, "sample_visit_status_logic")
    expect_equal(new_rule@group_by, c("SUBJECT_ID", "VISIT"))
    expect_match(rv$yaml_text, "type: group_condition", fixed = TRUE)
  })
})

test_that("group_condition constraint selectors follow condition name changes", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)

    session$setInputs(edit_rules = 1)
    session$setInputs(rule_add = 1)
    session$setInputs(rule_type = "group_condition")

    session$setInputs(
      rule_id = "group_condition_name_sync",
      rule_group_by = c("SUBJECT_ID", "VISIT"),
      gcond_name_1 = "old_name",
      gcond_col_1 = "STATUS",
      gcond_op_1 = "equals",
      gcond_val_1 = "FAILED"
    )

    session$setInputs(gcond_add = 1)
    session$setInputs(
      gcond_name_2 = "reported",
      gcond_col_2 = "CONSENT_DATE",
      gcond_op_2 = "empty",
      gcond_val_2 = "false"
    )

    session$setInputs(gcond_name_1 = "failed")
    session$setInputs(
      gconstr_type_1 = "mutually_exclusive",
      gconstr_left_1 = "failed",
      gconstr_right_1 = "reported"
    )

    session$setInputs(rule_save = 1)

    expect_null(rv$rule_msg)
    saved <- DTAtools::datasets(rv$dta, "clinical_data")@specs@rules
    rule <- saved[[length(saved)]]
    expect_s3_class(rule, "DTAtools::DTARuleGroupCondition")
    expect_equal(rule@constraints[[1]]$left, "failed")
    expect_equal(rule@constraints[[1]]$right, "reported")
  })
})

test_that("a column save with an incomplete type is rejected without touching the spec", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
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

# ---- File-handler editing ----------------------------------------------------
# A file handler IS an upload slot, and uploads are keyed by the handler's
# POSITION ("<dataset>||<handler index>"). These tests pin the consequence: the
# specification and the loaded files must never drift apart, because a file that
# is bound to the dataset but reachable from no slot is invisible in the UI while
# still counting towards validation and export.

# Add one more file handler to the active dataset through the editor inputs.
add_second_handler <- function(session, filename = "extra.csv") {
  session$setInputs(edit_files = 1)
  session$setInputs(file_add = 1)
  session$setInputs(
    file_filename = filename,
    file_type = "csv",
    file_pattern = FALSE
  )
  session$setInputs(file_save = 1)
}

test_that("adding a file handler adds a slot and keeps the loaded files", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    expect_equal(names(rv$uploads), "clinical_data||1")

    add_second_handler(session)

    expect_length(rv$structure$clinical_data$handlers, 2)
    # The file loaded into slot 1 stays exactly where it was.
    expect_equal(names(rv$uploads), "clinical_data||1")
    expect_equal(
      names(DTAtools::tables(DTAtools::datasets(rv$dta, "clinical_data"))),
      "clinical_data"
    )
  })
})

test_that("a file-handler change resets the dataset's validation", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    session$setInputs(check_all = 1)
    expect_equal(unname(rv$status[["clinical_data"]]), "pass")

    add_second_handler(session)

    # The dataset now expects a file it does not have; a green tick would be a
    # claim about a specification that no longer exists.
    expect_equal(unname(rv$status[["clinical_data"]]), "pending")
  })
})

test_that("removing an empty file handler needs no confirmation", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    add_second_handler(session)
    expect_length(rv$structure$clinical_data$handlers, 2)

    # Slot 2 holds nothing, so the click removes it outright.
    session$setInputs(file_del_click = 2)

    expect_length(rv$structure$clinical_data$handlers, 1)
    expect_null(rv$pending_handler_removal)
  })
})

test_that("removing a handler with loaded files asks first and does nothing until confirmed", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    session$setInputs(edit_files = 1)

    session$setInputs(file_del_click = 1)

    expect_equal(rv$pending_handler_removal$index, 1)
    expect_equal(rv$pending_handler_removal$tables, "clinical_data")
    # Nothing has happened yet.
    expect_length(rv$structure$clinical_data$handlers, 1)
    expect_equal(names(rv$uploads), "clinical_data||1")

    session$setInputs(cancel_remove_handler = 1)

    expect_null(rv$pending_handler_removal)
    expect_length(rv$structure$clinical_data$handlers, 1)
    expect_equal(names(rv$uploads), "clinical_data||1")
  })
})

test_that("confirming the removal unloads the files that were loaded into that slot", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    session$setInputs(edit_files = 1)
    session$setInputs(file_del_click = 1)
    session$setInputs(confirm_remove_handler = 1)

    expect_length(rv$structure$clinical_data$handlers, 0)
    # Spec and data in step: no slot, no upload record, and no table left
    # bound inside the dataset either.
    expect_length(rv$uploads[["clinical_data||1"]], 0)
    expect_length(DTAtools::tables(DTAtools::datasets(rv$dta, "clinical_data")), 0)
    expect_equal(unname(rv$status[["clinical_data"]]), "nodata")
  })
})

test_that("removing the first of two handlers re-keys the second one's uploads", {
  # THE defect this guards: upload records are keyed by handler position, so
  # removing handler 1 shifts handler 2 into position 1 while its records still
  # sit under "||2". The file would vanish from the Loaded-files list and stay
  # bound to the dataset, reachable only by Discard all.
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    add_second_handler(session, filename = "clinical_data2.csv")

    # Load a file through the SECOND slot only.
    session$setInputs(up_1_2 = app_file_input("clinical_data2.csv"))
    expect_equal(names(rv$uploads), "clinical_data||2")

    session$setInputs(edit_files = 1)
    session$setInputs(file_del_click = 1)
    # Slot 1 is empty, so this removes it without asking.
    expect_null(rv$pending_handler_removal)

    expect_length(rv$structure$clinical_data$handlers, 1)
    # The record moved with its handler...
    expect_equal(names(rv$uploads), "clinical_data||1")
    expect_equal(rv$uploads[["clinical_data||1"]][[1]]$table, "clinical_data2")
    # ... and still describes what is actually bound to the dataset.
    expect_equal(
      names(DTAtools::tables(DTAtools::datasets(rv$dta, "clinical_data"))),
      "clinical_data2"
    )
  })
})

test_that("reordering handlers moves the upload records with them", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    add_second_handler(session, filename = "clinical_data2.csv")
    session$setInputs(up_1_2 = app_file_input("clinical_data2.csv"))
    expect_equal(names(rv$uploads), "clinical_data||2")

    session$setInputs(edit_files = 1)
    session$setInputs(file_up_click = 2)

    # The handler that was second is now first, and so is its record.
    expect_equal(
      rv$structure$clinical_data$handlers[[1]]$expected, "clinical_data2.csv"
    )
    expect_equal(names(rv$uploads), "clinical_data||1")
  })
})

test_that("a rejected file-handler form leaves the specification untouched", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    session$setInputs(edit_files = 1)
    session$setInputs(file_add = 1)
    session$setInputs(
      file_filename = "extra.*[.]csv$", file_type = "csv",
      file_pattern = TRUE, file_count_mode = "range",
      file_min_number_of_files = 4, file_max_number_of_files = 2
    )
    session$setInputs(file_save = 1)

    expect_false(rv$file_msg$ok)
    expect_match(rv$file_msg$error, "cannot exceed the maximum")
    # Still on the form, and no handler was added.
    expect_equal(rv$file_view, "form")
    expect_length(rv$structure$clinical_data$handlers, 1)
  })
})

test_that("the count controls cannot smuggle a bad count past a non-pattern entry", {
  # The count inputs are hidden while "is a pattern" is unticked, and whatever
  # value they still hold is ignored: a non-pattern entry matches one exact name
  # and is saved expecting exactly 1 file, rather than being rejected for a
  # number the user was never shown.
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    session$setInputs(edit_files = 1)
    session$setInputs(file_add = 1)
    session$setInputs(
      file_filename = "extra.csv", file_type = "csv",
      file_pattern = FALSE, file_count_mode = "range",
      file_number_of_files = 3,
      file_min_number_of_files = 2, file_max_number_of_files = 5
    )
    session$setInputs(file_save = 1)

    expect_null(rv$file_msg)
    expect_equal(rv$file_view, "list")
    expect_length(rv$structure$clinical_data$handlers, 2)
    added <- rv$structure$clinical_data$handlers[[2]]
    expect_equal(added$min, 1)
    expect_equal(added$max, 1)
  })
})

test_that("the handler form reaches the encoding and quoted-newline settings", {
  # Reading output$file_modal_body is the point: a renderUI output is never
  # evaluated unless a test asks for it, so a control that is only in the
  # markup -- or a form that errors while building it -- would otherwise ship.
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    session$setInputs(edit_files = 1)
    session$setInputs(file_edit_click = 1)

    html <- ui_text(output$file_modal_body)
    expect_match(html, "id=\"file_encoding\"")
    expect_match(html, "id=\"file_newlines_in_values\"")
    # The shortlist is on offer, and the panel is hidden for an unparsed file.
    expect_match(html, "value=\"windows-1252\"")
    expect_match(html, "input.file_type !=", fixed = TRUE)

    session$setInputs(
      file_filename = "clinical_data.*[.]csv$", file_type = "csv",
      file_pattern = TRUE, file_count_mode = "exact", file_number_of_files = 1,
      file_encoding = "latin1", file_newlines_in_values = TRUE
    )
    session$setInputs(file_save = 1)

    expect_null(rv$file_msg)
    h <- DTAtools::datasets(rv$dta, "clinical_data")@files[[1]]
    expect_equal(h@encoding, "latin1")
    expect_true(h@newlines_in_values)
  })
})

test_that("saving a handler keeps a reader setting the form never shows", {
  # `has_header` has no control. Before the form carried it, renaming a file
  # rebuilt the handler on the constructor defaults and the header row silently
  # became data again.
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)

    # Seed a handler that declares no header row, through the same helper the
    # form saves with.
    seeded <- app_fn("dta_set_handler")(
      rv$dta, "clinical_data",
      index = 1, filename = "clinical_data.*[.]csv$", type = "csv",
      pattern = TRUE, count_mode = "exact", number_of_files = 1,
      reader_settings = list(has_header = FALSE, quote = "'")
    )
    expect_true(seeded$ok)
    rv$dta <- seeded$value

    session$setInputs(edit_files = 1)
    session$setInputs(file_edit_click = 1)
    session$setInputs(
      file_filename = "renamed.csv", file_type = "csv", file_pattern = FALSE
    )
    session$setInputs(file_save = 1)

    expect_null(rv$file_msg)
    h <- DTAtools::datasets(rv$dta, "clinical_data")@files[[1]]
    expect_equal(h@filename, "renamed.csv")
    expect_false(h@has_header)
    expect_equal(h@quote, "'")
  })
})

test_that("applying raw YAML that changes files: keeps the loaded files", {
  # Editing `files:` in the Raw tab used to discard every file loaded into that
  # dataset. It now costs the same as the Edit-files dialog: the data stays, the
  # validation does not.
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    session$setInputs(check_all = 1)
    expect_equal(unname(rv$status[["clinical_data"]]), "pass")

    # A document identical to the loaded one except for an extra file handler.
    edited <- app_fn("dta_set_handler")(
      rv$dta, "clinical_data",
      index = NULL, filename = "extra.tsv", type = "tsv"
    )
    yaml_text <- app_fn("dta_to_yaml_text")(edited$value)
    expect_true(yaml_text$ok)

    session$setInputs(raw_yaml_editor = yaml_text$value)
    session$setInputs(apply_yaml = 1)

    expect_true(rv$yaml_msg$ok)
    expect_length(rv$structure$clinical_data$handlers, 2)
    # The file survived the re-parse ...
    expect_equal(
      names(DTAtools::tables(DTAtools::datasets(rv$dta, "clinical_data"))),
      "clinical_data"
    )
    expect_equal(names(rv$uploads), "clinical_data||1")
    # ... but its validation did not.
    expect_equal(unname(rv$status[["clinical_data"]]), "pending")
  })
})

test_that("applying raw YAML that deletes a slot unloads only that slot's files", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    add_second_handler(session, filename = "clinical_data2.csv")
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    session$setInputs(up_1_2 = app_file_input("clinical_data2.csv"))
    expect_equal(sort(names(rv$uploads)), c("clinical_data||1", "clinical_data||2"))

    # Drop the second handler in the document itself.
    trimmed <- app_fn("dta_remove_handler")(rv$dta, "clinical_data", 2)
    yaml_text <- app_fn("dta_to_yaml_text")(trimmed$value)
    session$setInputs(raw_yaml_editor = yaml_text$value)
    session$setInputs(apply_yaml = 1)

    expect_true(rv$yaml_msg$ok)
    expect_length(rv$structure$clinical_data$handlers, 1)
    expect_equal(names(rv$uploads), "clinical_data||1")
    # The first slot's file stays; the deleted slot's file is unloaded with it.
    expect_equal(
      names(DTAtools::tables(DTAtools::datasets(rv$dta, "clinical_data"))),
      "clinical_data"
    )
  })
})

test_that("applying raw YAML that reorders files: moves the loaded file with its slot", {
  # Keeping a record at its old POSITION would show a file under a slot that now
  # expects something else: the Loaded-files list would claim a file satisfies a
  # requirement it does not match.
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    add_second_handler(session, filename = "clinical_data2.csv")
    session$setInputs(up_1_2 = app_file_input("clinical_data2.csv"))
    expect_equal(names(rv$uploads), "clinical_data||2")

    # Swap the two entries in the document itself.
    swapped <- app_fn("dta_move_handler")(rv$dta, "clinical_data", 2, "up")
    yaml_text <- app_fn("dta_to_yaml_text")(swapped$value)
    session$setInputs(raw_yaml_editor = yaml_text$value)
    session$setInputs(apply_yaml = 1)

    expect_true(rv$yaml_msg$ok)
    expect_equal(
      rv$structure$clinical_data$handlers[[1]]$expected, "clinical_data2.csv"
    )
    # The record followed its own handler into position 1.
    expect_equal(names(rv$uploads), "clinical_data||1")
    expect_equal(rv$uploads[["clinical_data||1"]][[1]]$table, "clinical_data2")
    expect_equal(
      names(DTAtools::tables(DTAtools::datasets(rv$dta, "clinical_data"))),
      "clinical_data2"
    )
  })
})

test_that("applying raw YAML that rewrites a slot unloads the file it no longer asks for", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    expect_equal(names(rv$uploads), "clinical_data||1")

    # The one slot now asks for a different file entirely.
    rewritten <- app_fn("dta_set_handler")(
      rv$dta, "clinical_data",
      index = 1, filename = "something_else.csv", type = "csv", pattern = FALSE
    )
    yaml_text <- app_fn("dta_to_yaml_text")(rewritten$value)
    session$setInputs(raw_yaml_editor = yaml_text$value)
    session$setInputs(apply_yaml = 1)

    expect_true(rv$yaml_msg$ok)
    expect_length(rv$uploads[["clinical_data||1"]], 0)
    expect_length(DTAtools::tables(DTAtools::datasets(rv$dta, "clinical_data")), 0)
    expect_equal(unname(rv$status[["clinical_data"]]), "nodata")
  })
})

test_that("raw YAML that removes every file handler unloads that dataset's data", {
  # The most destructive raw-YAML edit there is: with no slot left, a bound
  # table could neither be shown nor removed, so it must not survive the apply.
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    expect_equal(names(rv$uploads), "clinical_data||1")

    stripped <- app_fn("dta_remove_handler")(rv$dta, "clinical_data", 1)
    expect_true(stripped$ok)
    yaml_text <- app_fn("dta_to_yaml_text")(stripped$value)
    session$setInputs(raw_yaml_editor = yaml_text$value)
    session$setInputs(apply_yaml = 1)

    expect_true(rv$yaml_msg$ok)
    expect_length(rv$structure$clinical_data$handlers, 0)
    expect_length(rv$uploads, 0)
    expect_length(DTAtools::tables(DTAtools::datasets(rv$dta, "clinical_data")), 0)
    expect_equal(unname(rv$status[["clinical_data"]]), "nodata")
  })
})

test_that("HTML validation report download handler produces parseable HTML", {
  skip_if_not_installed("xml2")
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # Load a DTA the same way other tests do
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    # Bind valid data to the dataset so that results() and messages() work
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    # Check the dataset to populate validation results
    session$setInputs(check_all = 1)

    # Verify that the exact object rv$dta produces a valid report by calling
    # the same function the handler uses. This tests that the object state
    # and function integration work correctly.
    report_file <- tempfile(fileext = ".html")
    on.exit(unlink(report_file, force = TRUE), add = TRUE)

    # This is the same call the downloadHandler uses.
    expect_no_error(
      DTAtools::write_validation_report(
        rv$dta,
        report_file,
        overwrite = TRUE,
        quiet = TRUE
      )
    )

    # Verify the file is valid HTML
    doc <- xml2::read_html(report_file)
    expect_true(!is.na(doc))

    # The button really is rendered, and enabled -- a check has just run.
    # Read the live output rather than grepping app.R for a downloadButton()
    # call: that stopped being the wiring when the dock's buttons moved into
    # output$msgs_dock_dl.
    expect_match(
      btn_html(output$msgs_dock_dl, "dl_msgs_html"),
      "shiny-download-link",
      fixed = TRUE
    )

    expect_true(
      grepl(
        "output$dl_msgs_html <- downloadHandler",
        app_source("app.R"),
        fixed = TRUE
      ),
      info = "downloadHandler wiring for dl_msgs_html not found in app.R"
    )
  })
})

# ---- Edit -> Metadata (dataset-level) ---------------------------------------

test_that("opening the metadata editor targets the active dataset and pre-fills it", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    expect_null(rv$editor_dataset)

    session$setInputs(edit_meta = 1)

    expect_equal(rv$editor_dataset, "clinical_data")
    # The form pre-fills from rv$meta_prefill rather than from live inputs, so
    # a re-render can never resurrect a stale value.
    expect_equal(rv$meta_prefill$name, "clinical_data")
    expect_equal(rv$meta_prefill$description, "Clinical data table")
    expect_gt(rv$meta_token, 0)
    expect_null(rv$meta_msg)
  })
})

test_that("the metadata modal body renders every field, pre-filled, and no type control", {
  # The only test that actually RENDERS this output. Everything else drives the
  # observers with setInputs(), which never evaluates renderUI() -- so a broken
  # modal body would pass the whole suite and fail on the user's first click.
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    session$setInputs(edit_meta = 1)

    html <- as.character(output$meta_modal_body$html)

    for (id in c(
      "meta_name", "meta_description", "meta_template_source",
      "meta_template_version", "meta_template_date", "meta_save"
    )) {
      expect_match(html, paste0("\"", id, "\""), fixed = TRUE)
    }
    # The form opens on the dataset's current values, not empty.
    expect_match(html, "value=\"clinical_data\"", fixed = TRUE)
    expect_match(html, "Clinical data table", fixed = TRUE)
    # A dataset's type is fixed by its S7 class; the editor offers no way in,
    # not even a disabled control (which would still put an id on the page).
    expect_no_match(html, "meta_type", fixed = TRUE)
  })
})

test_that("editing only the description leaves a passed check passed", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    session$setInputs(check_all = 1)
    expect_equal(rv$status, c(clinical_data = "pass"))
    structure_before <- rv$structure

    session$setInputs(edit_meta = 1)
    session$setInputs(
      meta_name = "clinical_data",
      meta_description = "Vitals collected at every visit",
      meta_template_source = "CDISC SDTM",
      meta_template_version = "3.4",
      meta_template_date = "2026-01-15"
    )
    session$setInputs(meta_save = 1)

    expect_null(rv$meta_msg)
    # rv$structure is left alone: reassigning it re-renders every
    # structure-dependent output (dataset nav, detail panel and its file
    # inputs) for no reason -- nothing name-keyed changed. Only a rename has
    # to pay that price.
    expect_identical(rv$structure, structure_before)
    expect_equal(
      DTAtools::datasets(rv$dta, "clinical_data")@description,
      "Vitals collected at every visit"
    )
    # The description and template fields take no part in validation, so
    # clearing a green result here would be gratuitous.
    expect_equal(rv$status, c(clinical_data = "pass"))
    expect_match(rv$yaml_text, "CDISC SDTM", fixed = TRUE)
  })
})

test_that("renaming a dataset migrates every piece of name-keyed state", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    session$setInputs(check_all = 1)
    expect_equal(names(rv$uploads), "clinical_data||1")
    expect_equal(rv$active, "clinical_data")

    session$setInputs(edit_meta = 1)
    session$setInputs(
      meta_name = "renamed_data", meta_description = "Clinical data table",
      meta_template_source = "", meta_template_version = "", meta_template_date = ""
    )
    session$setInputs(meta_save = 1)

    expect_null(rv$meta_msg)
    # Stale upload keys would leave the loaded file bound to the dataset --
    # still counted for validation and export -- while it vanished from the
    # "Loaded files" card with no way to reach it.
    expect_equal(names(rv$uploads), "renamed_data||1")
    expect_equal(rv$active, "renamed_data")
    expect_equal(rv$editor_dataset, "renamed_data")
    expect_equal(names(rv$structure), "renamed_data")
    expect_equal(names(rv$status), "renamed_data")
    # The data itself travelled with the dataset.
    expect_equal(
      DTAtools::tables(DTAtools::datasets(rv$dta, "renamed_data")) |> names(),
      "clinical_data"
    )
    expect_match(rv$yaml_text, "renamed_data", fixed = TRUE)
  })
})

test_that("renaming a dataset clears its validation", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    session$setInputs(check_all = 1)
    expect_equal(rv$status, c(clinical_data = "pass"))

    session$setInputs(edit_meta = 1)
    session$setInputs(
      meta_name = "renamed_data", meta_description = "",
      meta_template_source = "", meta_template_version = "", meta_template_date = ""
    )
    session$setInputs(meta_save = 1)

    # Every stored validation record carries the name it was checked under, so
    # results left in place would report a dataset that no longer exists.
    expect_equal(rv$status, c(renamed_data = "pending"))
  })
})

test_that("a rejected metadata save leaves the workspace untouched", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    # These tests were written when every surface was unconditionally
    # editable. Edit mode now gates them, so turning it on here preserves
    # each test's original intent; the gate itself is covered in
    # test-shinyapp-edit-mode.R.
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)
    session$setInputs(edit_meta = 1)
    session$setInputs(
      meta_name = "   ", meta_description = "irrelevant",
      meta_template_source = "", meta_template_version = "", meta_template_date = ""
    )
    session$setInputs(meta_save = 1)

    expect_false(rv$meta_msg$ok)
    expect_match(rv$meta_msg$error, "name is required")
    # Nothing was written: the dataset keeps its name and its description.
    expect_equal(names(rv$structure), "clinical_data")
    expect_equal(
      DTAtools::datasets(rv$dta, "clinical_data")@description,
      "Clinical data table"
    )
  })
})

# --- the Validation summary button ------------------------------------------
#
# The button is the only route to dta_build_validation_report() in the app, and
# it now carries a claim of its own: green means "this certifies a pass", amber
# means "you can have the summary, but it will say the run is incomplete". A
# renderUI output is never evaluated unless a test reads it, so without this the
# gate could invert and CI would stay green.

test_that("the Validation summary button is green only when nothing is left unvalidated", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))

    # Every dataset validated and passed: the summary really would certify one.
    rv$status <- c(clinical_data = "pass")
    session$flushReact()
    html <- ui_text(output$validation_report_ui)
    expect_match(html, "Validation summary", fixed = TRUE)
    expect_match(html, "btn-success", fixed = TRUE)
    expect_false(grepl("btn-warning", html, fixed = TRUE))

    # A second dataset skipped for missing data. The summary is still offered --
    # it is a summary, not a certificate -- but the button says so up front.
    rv$status <- c(clinical_data = "pass", lab_data = "nodata")
    session$flushReact()
    html <- ui_text(output$validation_report_ui)
    expect_match(html, "Validation summary", fixed = TRUE)
    expect_match(html, "btn-warning", fixed = TRUE)
    expect_match(html, "incomplete rather than passed", fixed = TRUE)
    expect_false(grepl("btn-success", html, fixed = TRUE))
  })
})

test_that("no Validation summary is offered while a dataset has failed", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))

    rv$status <- c(clinical_data = "fail")
    session$flushReact()
    expect_false(grepl("Validation summary", ui_text(output$validation_report_ui), fixed = TRUE))

    # Nor before anything has been checked at all.
    rv$status <- c(clinical_data = "pending")
    session$flushReact()
    expect_false(grepl("Validation summary", ui_text(output$validation_report_ui), fixed = TRUE))
  })
})

# --- the Validation messages dock download buttons ---------------------------
#
# Before a check has been run there is nothing to export, and the CSV/TSV/XLSX
# handlers would happily write a file whose single row reads "No validation
# messages for this dataset." -- indistinguishable from a clean result. The
# buttons are therefore disabled until a check has actually happened. A
# renderUI output is never evaluated unless a test reads it, so without these
# the gate could invert and CI would stay green.

test_that("the dock download buttons are disabled until a check has been run", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))

    ids <- c("dl_msgs_csv", "dl_msgs_tsv", "dl_msgs_xlsx", "dl_msgs_html")

    # Data bound, never validated: every button is present but inert.
    rv$status <- c(clinical_data = "pending")
    session$flushReact()
    for (id in ids) {
      btn <- btn_html(output$msgs_dock_dl, id)
      expect_false(grepl("shiny-download-link", btn, fixed = TRUE), info = id)
      expect_match(btn, "disabled", fixed = TRUE)
      expect_match(btn, "Run a check", fixed = TRUE)
    }

    # A dataset skipped for missing data is not a check either.
    rv$status <- c(clinical_data = "nodata")
    session$flushReact()
    expect_false(
      grepl(
        "shiny-download-link",
        btn_html(output$msgs_dock_dl, "dl_msgs_csv"),
        fixed = TRUE
      )
    )

    # Checked -- pass or fail, both are a result worth exporting.
    for (st in c("pass", "fail")) {
      rv$status <- stats::setNames(st, "clinical_data")
      session$flushReact()
      for (id in ids) {
        btn <- btn_html(output$msgs_dock_dl, id)
        expect_match(btn, "shiny-download-link", fixed = TRUE, info = paste(id, st))
        expect_match(btn, "Download the", fixed = TRUE, info = paste(id, st))
      }
    }
  })
})

test_that("the dock's Report follows the whole DTA, the table exports the active dataset", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))

    # Another dataset has been checked, the active one has not. The per-dataset
    # exports have nothing to give; the whole-DTA report does.
    rv$active <- "clinical_data"
    rv$status <- c(clinical_data = "pending", other_data = "pass")
    session$flushReact()
    expect_false(
      grepl(
        "shiny-download-link",
        btn_html(output$msgs_dock_dl, "dl_msgs_csv"),
        fixed = TRUE
      )
    )
    expect_match(
      btn_html(output$msgs_dock_dl, "dl_msgs_html"),
      "shiny-download-link",
      fixed = TRUE
    )
  })
})

test_that("the dock download handlers refuse to write before a check", {
  clean_session_file()

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    rv$status <- c(clinical_data = "pending")
    session$flushReact()

    # The disabled button is a client-side courtesy; the handler URL stays
    # reachable, so the gate has to hold on the server too. req() aborts the
    # download with a silent shiny.silent.error rather than writing a file.
    expect_error(
      output$dl_msgs_csv,
      class = "shiny.silent.error"
    )
    expect_error(
      output$dl_msgs_html,
      class = "shiny.silent.error"
    )

    # Once checked, the same download produces a real file.
    session$setInputs(check_all = 1)
    session$flushReact()
    csv <- output$dl_msgs_csv
    expect_true(file.exists(csv))
    expect_gt(length(readLines(csv, warn = FALSE)), 0)
  })
})
