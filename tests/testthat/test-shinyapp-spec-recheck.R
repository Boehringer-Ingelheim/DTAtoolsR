# A spec edit re-validates data that is already loaded.
#
# Editing a column or a rule used to reset the dataset to "pending" and wait for
# the user to press check, so a loaded dataset could read "pending" for data
# whose verdict the new spec already decided. These tests drive the server the
# way the browser does (inputs only) and read state, never the DOM.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

recheck_input <- function(filename) {
  path <- app_fixture_path(filename)
  data.frame(
    name = filename, size = file.size(path), type = "",
    datapath = path, stringsAsFactors = FALSE
  )
}

recheck_clean_session <- function() {
  unlink(
    list.files(tempdir(),
      pattern = "^dtatools_app_session.*\\.rds$", full.names = TRUE
    ),
    force = TRUE
  )
}

# Written as a quoted expression: testServer() evaluates its body in a
# session-scoped environment that a called function cannot reach.
#
# Loaded spec + data, checked to "pass", editing unlocked, column editor open
# on the dataset.
recheck_setup <- quote({
  session$setInputs(dta_file = recheck_input("clinical_dta.yaml"))
  session$setInputs(up_1_1 = recheck_input("clinical_data.csv"))
  session$setInputs(check_all = 1)
  testthat::expect_equal(unname(rv$status[["clinical_data"]]), "pass")
  unlock_editing(session)
  session$setInputs(edit_cols = 1)
})

# Inputs the column form would carry for SUBJECT_ID (SAS Char, not nullable),
# with only `length` and `values` varying.
recheck_subject_id_form <- function(length = "", values = "") {
  list(
    col_id = "SUBJECT_ID", col_label = "Subject Identifier",
    col_backend = "SAS", col_type = "Char", col_format = "",
    col_length = length, col_nullable = FALSE, col_values = values,
    col_pattern = "", col_desc = "Unique subject ID"
  )
}

recheck_col_msgs <- function(dta) {
  m <- DTAtools::messages(DTAtools::datasets(dta, "clinical_data"), as_tibble = FALSE)
  m[m$source == "columnspec", , drop = FALSE]
}

test_that("saving a column the loaded data breaks fails the dataset without pressing check", {
  recheck_clean_session()

  shiny::testServer(.shiny_app_dir(), {
    eval(recheck_setup)

    # SUBJECT_ID holds "SUBJ0001"-style ids: eight characters.
    do.call(session$setInputs, recheck_subject_id_form(length = "5"))
    session$setInputs(col_save = 1)

    expect_null(rv$col_msg)
    expect_equal(unname(rv$status[["clinical_data"]]), "fail")

    msgs <- recheck_col_msgs(rv$dta)
    expect_gt(nrow(msgs), 0)
    expect_setequal(unique(msgs$keyword), "maxLength")
    expect_setequal(unique(msgs$column), "SUBJECT_ID")
  })
})

test_that("saving a column the data still satisfies keeps the dataset passing", {
  recheck_clean_session()

  shiny::testServer(.shiny_app_dir(), {
    eval(recheck_setup)

    do.call(session$setInputs, recheck_subject_id_form(length = "20"))
    session$setInputs(col_save = 1)

    expect_null(rv$col_msg)
    # Re-validated, not merely reset: "pending" would mean no check ran.
    expect_equal(unname(rv$status[["clinical_data"]]), "pass")
  })
})

test_that("removing a column re-validates the loaded data", {
  recheck_clean_session()

  shiny::testServer(.shiny_app_dir(), {
    eval(recheck_setup)

    ids <- app_fn("dta_column_ids")(rv$dta, "clinical_data")
    session$setInputs(col_del_click = match("SUBJECT_ID", ids))

    expect_false("SUBJECT_ID" %in% app_fn("dta_column_ids")(rv$dta, "clinical_data"))
    # The loaded data still carries SUBJECT_ID, but three rules in
    # clinical_dta.yaml name it, and a column a rule names is described by the
    # specification, so it is not "undeclared" (no additionalProperties
    # finding). Every remaining check holds: the verdict is "pass", reached by
    # a re-check -- "pending" would mean none ran.
    expect_equal(unname(rv$status[["clinical_data"]]), "pass")
    expect_equal(nrow(recheck_col_msgs(rv$dta)), 0L)
  })
})

test_that("saving and deleting a rule re-validates the loaded data", {
  recheck_clean_session()

  shiny::testServer(.shiny_app_dir(), {
    eval(recheck_setup)
    n_rules <- nrow(app_fn("dta_rules_overview")(rv$dta, "clinical_data"))

    # GENDER repeats across the 500 rows, so uniqueness cannot hold.
    session$setInputs(edit_rules = 1)
    session$setInputs(rule_add = 1)
    session$setInputs(rule_type = "col_unique")
    session$setInputs(rule_id = "gender_unique", rule_desc = "", rule_cols = "GENDER")
    session$setInputs(rule_save = 1)

    expect_null(rv$rule_msg)
    expect_equal(unname(rv$status[["clinical_data"]]), "fail")

    ov <- app_fn("dta_rules_overview")(rv$dta, "clinical_data")
    expect_equal(nrow(ov), n_rules + 1L)
    session$setInputs(rule_del_click = nrow(ov))

    expect_equal(nrow(app_fn("dta_rules_overview")(rv$dta, "clinical_data")), n_rules)
    expect_equal(unname(rv$status[["clinical_data"]]), "pass")
  })
})

test_that("a dataset with no data stays 'nodata' after a spec edit", {
  recheck_clean_session()

  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = recheck_input("clinical_dta.yaml"))
    unlock_editing(session)
    session$setInputs(edit_cols = 1)

    do.call(session$setInputs, recheck_subject_id_form(length = "5"))
    session$setInputs(col_save = 1)

    expect_null(rv$col_msg)
    expect_equal(unname(rv$status[["clinical_data"]]), "nodata")
    # recheck_dataset() is gated on bound data, so nothing was validated.
    expect_equal(nrow(recheck_col_msgs(rv$dta)), 0L)
  })
})

test_that("a spec edit that only reorders columns does not re-validate", {
  recheck_clean_session()

  shiny::testServer(.shiny_app_dir(), {
    eval(recheck_setup)

    # Moving a column changes no verdict; it keeps today's reset to "pending".
    session$setInputs(col_down_click = 1)
    expect_equal(unname(rv$status[["clinical_data"]]), "pending")
  })
})

test_that("applying raw YAML that changes a spec re-validates the carried-over data", {
  recheck_clean_session()

  shiny::testServer(.shiny_app_dir(), {
    eval(recheck_setup)

    edited <- app_fn("dta_set_column")(
      rv$dta, "clinical_data",
      id = "SUBJECT_ID", label = "Subject Identifier", backend = "SAS",
      type = "Char", length = "5", nullable = FALSE,
      description = "Unique subject ID"
    )
    expect_true(edited$ok)
    txt <- app_fn("dta_to_yaml_text")(edited$value)
    expect_true(txt$ok)

    session$setInputs(raw_yaml_editor = txt$value)
    session$setInputs(apply_yaml = 1)

    expect_true(rv$yaml_msg$ok)
    expect_equal(unname(rv$status[["clinical_data"]]), "fail")
  })
})

test_that("applying raw YAML that leaves the specs alone keeps the verdict", {
  recheck_clean_session()

  shiny::testServer(.shiny_app_dir(), {
    eval(recheck_setup)

    session$setInputs(raw_yaml_editor = rv$yaml_text)
    session$setInputs(apply_yaml = 1)

    expect_true(rv$yaml_msg$ok)
    expect_equal(unname(rv$status[["clinical_data"]]), "pass")
  })
})

# Needs the spec self-check (dta_spec_value_mismatch, WP-A) in the DTAColumnSpec
# validator: until then nothing rejects a `values` list that cannot fit.
test_that("a column whose permitted values cannot fit it is refused and changes nothing", {
  recheck_clean_session()

  shiny::testServer(.shiny_app_dir(), {
    eval(recheck_setup)
    before <- rv$dta

    # "ABCD" is four characters in a three-character column.
    do.call(session$setInputs, recheck_subject_id_form(length = "3", values = "ABCD"))
    session$setInputs(col_save = 1)

    expect_false(isTRUE(rv$col_msg$ok))
    expect_true(nzchar(rv$col_msg$error))
    expect_identical(rv$dta, before)
    expect_equal(unname(rv$status[["clinical_data"]]), "pass")
  })
})
