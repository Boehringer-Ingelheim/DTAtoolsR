# The application's server logic. REQ-APP-001 .. REQ-APP-013, REQ-APP-026 and REQ-APP-027 .. REQ-APP-030.
#
# A user of the application never sees check()'s return value. They see a
# coloured status against a dataset name and act on it, so a difference between
# what the validation engine found and what the reactive state holds is
# invisible to anyone reading only the screen. Every case here reads that
# reactive state rather than the engine's own result.
#
# Driven through shiny::testServer(), which runs the real server function
# without a browser. Anything genuinely client-side is pinned structurally and
# says so.

as_status <- function(status, name) {
  value <- status[[name]]
  if (is.null(value)) NA_character_ else as.character(value)
}

test_that("OQ-APP-001 | the launcher takes the arguments its documentation names | REQ-APP-001", {
  formals_seen <- names(formals(run_dta_app))

  qa_step(
    "the launcher accepts a browser flag, a port and further arguments",
    c("launch.browser", "port", "..."),
    intersect(c("launch.browser", "port", "..."), formals_seen)
  )
  qa_step(
    "and opens a browser by default, which is what a desktop user expects",
    TRUE, eval(formals(run_dta_app)$launch.browser)
  )
})

test_that("OQ-APP-002 | the launcher resolves the application bundled with the package | REQ-APP-002", {
  dir <- qapp_skip_unless_app_installed()

  qa_check("the application directory is found", nzchar(dir))
  qa_step(
    "and it holds the application's entry point",
    TRUE, file.exists(file.path(dir, "app.R"))
  )
  qa_check(
    "together with the helper files the application sources",
    length(qapp_helper_files(dir)) > 0
  )
})

test_that("OQ-APP-003 | a missing dependency is reported once, naming every one | REQ-APP-003", {
  qapp_skip_unless_app_installed()

  # The application needs three packages. Reporting them one at a time would
  # make a user install, retry, and be told about the next one; naming all of
  # them at once is the difference between one round trip and three.
  condition <- qapp_with_packages_unavailable(
    c("bslib", "DT"),
    tryCatch(run_dta_app(launch.browser = FALSE), error = function(e) e)
  )
  qa_check("a missing dependency raises a condition", inherits(condition, "condition"))

  text <- paste(conditionMessage(condition), collapse = " ")
  qa_step(
    "and the condition names every package that is missing",
    c(bslib = TRUE, DT = TRUE),
    c(
      bslib = grepl("bslib", text, fixed = TRUE),
      DT = grepl("DT", text, fixed = TRUE)
    )
  )
})

test_that("OQ-APP-004 | the workspace starts empty | REQ-APP-004", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    qa_step(
      "no document, no active dataset, no upload and no status is held",
      c(document = TRUE, active = TRUE, uploads = 0L, status = 0L),
      c(
        document = is.null(rv$dta),
        active = is.null(rv$active),
        uploads = length(rv$uploads),
        status = length(rv$status)
      )
    )
  })
})

test_that("OQ-APP-005 | loading a specification populates the workspace | REQ-APP-005", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("a", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))

    qa_check("a document is held", !is.null(rv$dta))
    qa_step(
      "its declared dataset is the active one",
      "clinical_data", as.character(rv$active)
    )
    # Pending, not passing: nothing has been checked, and a status that read as
    # a pass before any data was bound would be a verdict on nothing.
    qa_step(
      "and every declared dataset is reported as awaiting a check",
      "pending", as_status(rv$status, "clinical_data")
    )
  })
})

test_that("OQ-APP-006 | loading a second specification replaces the first entirely | REQ-APP-006", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("b", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = qapp_file_input("clinical_data.csv"))
    session$setInputs(check_all = 1)

    first_status <- names(rv$status)
    qa_check("the first document was loaded and checked", length(first_status) > 0)

    # A carried-over upload or status would let a verdict reached against one
    # specification be read against another, and nothing on screen would say so.
    session$setInputs(dta_file = qapp_file_input("gf_dataset.yaml"))

    qa_step(
      "no upload survives the replacement",
      0L, length(rv$uploads)
    )
    qa_step(
      "and no status from the previous document does either",
      character(0), intersect(first_status, names(rv$status))
    )
    qa_check("while the new document is held", !is.null(rv$dta))
  })
})

test_that("OQ-APP-007 | a dataset can only be added while editing | REQ-APP-007", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("c", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    before <- names(rv$status)

    # A document arrives read-only. Adding a dataset without unlocking it would
    # change a specification the user had not chosen to edit.
    session$setInputs(add_ds_id = "locked_ds", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 1)
    qa_step(
      "an add attempted outside edit mode changes nothing",
      before, names(rv$status)
    )

    qapp_enter_edit_mode(session)
    session$setInputs(add_ds_id = "second_ds", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 1)

    qa_step(
      "while in edit mode the dataset joins the document",
      TRUE, "second_ds" %in% names(rv$status)
    )
    # Nodata, not pending: the dataset exists but no delivery has been offered,
    # which is a different thing from one awaiting a check.
    qa_step(
      "and is reported as having no data bound",
      "nodata", as_status(rv$status, "second_ds")
    )
  })
})

test_that("OQ-APP-008 | removing a dataset drops it from the document and the status map | REQ-APP-008", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("d", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    qapp_enter_edit_mode(session)
    session$setInputs(add_ds_id = "to_remove", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 1)
    qa_check("the dataset to remove exists", "to_remove" %in% names(rv$status))

    session$setInputs(remove_dataset = 1)
    session$setInputs(remove_dataset_confirm = 1)

    # A status left behind would keep reporting a verdict for a dataset the
    # specification no longer contains.
    qa_step(
      "after confirmation the dataset is gone from the status map",
      FALSE, "to_remove" %in% names(rv$status)
    )
  })
})

test_that("OQ-APP-009 | binding a delivery registers it without validating it | REQ-APP-009", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("e", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = qapp_file_input("clinical_data.csv"))

    qa_step("the delivery is registered", 1L, length(rv$uploads))
    qa_step(
      "keyed by the dataset it belongs to and the handler it arrived through",
      TRUE, grepl("clinical_data", names(rv$uploads)[[1]], fixed = TRUE)
    )
    # Binding is not checking. A status that turned green on upload would be
    # reporting a verdict nobody had asked for.
    qa_step(
      "and the dataset still awaits a check",
      "pending", as_status(rv$status, "clinical_data")
    )
  })
})

test_that("OQ-APP-010 | replacing a bound delivery requires confirmation | REQ-APP-009", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("f", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = qapp_file_input("clinical_data.csv"))
    qa_step("the first delivery binds without a question", 1L, length(rv$uploads))

    # The gate compares the table a dropped file would occupy against the
    # tables already bound, and a table is named after its file. Delivering the
    # same name again is therefore the case that would silently replace bound
    # data, and it is the one the gate exists for.
    session$setInputs(up_1_1 = qapp_file_input("clinical_data.csv"))
    qa_check(
      "a delivery that would replace a bound table is held for confirmation",
      !is.null(rv$pending_upload)
    )
    qa_step(
      "and it names the dataset and handler it was aimed at",
      c(dataset = 1L, handler = 1L),
      c(
        dataset = as.integer(rv$pending_upload$ds_idx),
        handler = as.integer(rv$pending_upload$hi)
      )
    )

    session$setInputs(confirm_overwrite = 1)
    qa_step("confirming clears the pending delivery", TRUE, is.null(rv$pending_upload))
    qa_step(
      "and the dataset still holds exactly one bound delivery",
      1L, length(rv$uploads)
    )
  })
})

test_that("OQ-APP-011 | a check maps each dataset onto the app's own status vocabulary | REQ-APP-010", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("g", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = qapp_file_input("clinical_data.csv"))
    session$setInputs(check_all = 1)

    qa_step(
      "a clean delivery is reported as passing",
      "pass", as_status(rv$status, "clinical_data")
    )
    qa_step(
      "and every reported status is drawn from the app's four values",
      TRUE,
      all(unlist(rv$status) %in% c("pass", "fail", "pending", "nodata"))
    )
  })
})

test_that("OQ-APP-012 | a broken delivery is reported as failing | REQ-APP-010", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("h", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = qapp_file_input("clinical_data_error_all.csv"))
    session$setInputs(check_all = 1)

    qa_step(
      "a delivery breaking every axis is reported as failing",
      "fail", as_status(rv$status, "clinical_data")
    )
  })
})

test_that("OQ-APP-013 | a dataset with no delivery is never reported as passing | REQ-APP-010", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("i", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    qapp_enter_edit_mode(session)
    session$setInputs(add_ds_id = "empty_ds", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 1)
    session$setInputs(check_all = 1)

    # This is the failure the whole area exists to prevent: a dataset nobody
    # delivered anything for has nothing to validate, and a green status
    # against it would read as a delivery that arrived and passed.
    qa_step(
      "a dataset with nothing bound reports that it has no data",
      "nodata", as_status(rv$status, "empty_ds")
    )
    qa_step(
      "and never reports a pass",
      FALSE, identical(as_status(rv$status, "empty_ds"), "pass")
    )
  })
})

test_that("OQ-APP-014 | an unparseable value fails the dataset through the import axis | REQ-APP-011", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("j", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = qapp_file_input("clinical_data_error_import.csv"))
    session$setInputs(check_all = 1)

    qa_step(
      "a delivery whose only fault is unparseable values is reported as failing",
      "fail", as_status(rv$status, "clinical_data")
    )

    # And the engine agrees about why, read from the document the app holds
    # rather than from a separate call: the import axis is a first-class
    # failure here, not a warning the app quietly tolerates.
    msgs <- as.data.frame(messages(rv$dta))
    qa_step(
      "the import axis is what reported it",
      TRUE, "import" %in% as.character(msgs$source)
    )
  })
})

test_that("OQ-APP-015 | edit mode unlocks and re-locks the specification | REQ-APP-012", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("k", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    history_before <- nrow(get_version_history_df(metadata(rv$dta)))

    qapp_enter_edit_mode(session)
    session$setInputs(add_ds_id = "while_editing", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 1)
    qa_step(
      "a change made while editing reaches the document",
      TRUE, "while_editing" %in% names(rv$status)
    )
    # Entering edit mode is not the same act as cutting a version. Recording a
    # version entry for it would put a release in the document's history that
    # nobody made.
    qa_step(
      "and entering edit mode records no version entry",
      history_before, nrow(get_version_history_df(metadata(rv$dta)))
    )

    qapp_leave_edit_mode(session)
    after_lock <- names(rv$status)
    session$setInputs(add_ds_id = "after_locking", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 1)
    qa_step(
      "while a change attempted after locking does not",
      after_lock, names(rv$status)
    )
  })
})

test_that("OQ-APP-016 | the double-click guard is a self-contained capture-phase script | REQ-APP-013", {
  qapp_skip_unless_app_installed()

  # Pinned structurally, not driven. The guard is client-side JavaScript with
  # no server-side observer, and nothing in the package's dependency set can
  # execute JavaScript; the package's own development suite pins it the same
  # way. It is covered rather than skipped because the application ships no
  # other protection against a click submitted twice.
  tag <- qapp_fn("click_guard_script")()
  qa_step("the guard renders as a script element", "script", tag$name)

  # The BODY, not the rendered element. The rendered form always ends with a
  # closing tag, so looking for one there would assert nothing at all.
  script <- paste(as.character(tag$children[[1]]), collapse = "\n")
  qa_check("the guard has a body", nzchar(script))

  # An unescaped closing tag inside the body would end the script element
  # early and spill the rest of the guard into the page as visible text --
  # silently, because the page would still render.
  qa_step(
    "its body cannot terminate its own script element",
    FALSE, grepl("</script", script, fixed = TRUE)
  )

  qa_step(
    "it listens for a click",
    TRUE, grepl("addEventListener('click', function(ev){", script, fixed = TRUE)
  )
  # The trailing `true` is the whole mechanism: it registers the listener on
  # the capture phase, ahead of the element's own handler, so a click that
  # handler swallows still reaches the guard.
  qa_step(
    "on the capture phase rather than the bubble phase",
    TRUE, grepl("}, true);", script, fixed = TRUE)
  )
})

# ---- editing a dataset's columns and rules ---------------------------------

test_that("OQ-APP-026 | renaming a column onto an id another column already holds is refused | REQ-APP-019", {
  qapp_skip_unless_app_installed()
  set_column <- qapp_fn("dta_set_column")

  dta <- DTAtools::read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))
  cols_before <- DTAtools::datasets(dta, "clinical_data")@specs@columns
  visit_before <- cols_before[["VISIT"]]
  studyid_before <- cols_before[["STUDYID"]]

  # Renaming VISIT onto STUDYID would otherwise fall straight through to
  # cols[[id]] <- spec and silently destroy the STUDYID column it collides with.
  res <- set_column(dta, "clinical_data", id = "STUDYID", old_id = "VISIT")

  qa_check("the rename is refused rather than silently applied", isFALSE(res$ok))
  qa_step(
    "naming the id that already exists",
    TRUE, grepl("already exists", res$error, fixed = TRUE)
  )

  cols_after <- DTAtools::datasets(dta, "clinical_data")@specs@columns
  qa_step(
    "both columns remain, unchanged, under their original ids",
    list(visit = visit_before@label, studyid = studyid_before@label),
    list(visit = cols_after[["VISIT"]]@label, studyid = cols_after[["STUDYID"]]@label)
  )

  # Control case: renaming onto an id nothing else holds proceeds normally.
  ok <- set_column(dta, "clinical_data", id = "VISIT_CODE", old_id = "VISIT")
  qa_check("renaming onto a free id succeeds", isTRUE(ok$ok))
})

test_that("OQ-APP-027 | saving a column keeps its examples and colclass, which the editor form has no field for | REQ-APP-020", {
  qapp_skip_unless_app_installed()
  set_column <- qapp_fn("dta_set_column")

  dta <- DTAtools::read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))
  age_before <- DTAtools::datasets(dta, "clinical_data")@specs@columns[["AGE"]]
  # AGE carries no values, so examples may legally be set alongside it. Set
  # directly on the specification, since the editor form itself has no field
  # for either property -- exactly the point under test.
  age_with_extras <- DTAtools::DTAColumnSpec(
    id = age_before@id, label = age_before@label,
    type = paste(age_before@structure@backend, age_before@structure@type),
    nullable = age_before@nullable, description = age_before@description,
    examples = c(34, 45), colclass = "measurement"
  )
  dta@datasets[["clinical_data"]]@specs@columns[["AGE"]] <- age_with_extras

  res <- set_column(
    dta, "clinical_data",
    id = "AGE", label = "Age (years)", backend = "SAS", type = "Num",
    nullable = FALSE, description = "reworded through the editor form"
  )

  qa_check("the save succeeds", isTRUE(res$ok))
  col <- DTAtools::datasets(res$value, "clinical_data")@specs@columns[["AGE"]]
  qa_step(
    "the fields the form controls are updated as saved",
    list(label = "Age (years)", description = "reworded through the editor form"),
    list(label = col@label, description = col@description)
  )
  qa_step(
    "and examples/colclass, invisible to the form, survive the save unchanged",
    list(examples = c(34, 45), colclass = "measurement"),
    list(examples = col@examples, colclass = col@colclass)
  )
})

test_that("OQ-APP-028 | a group-condition row removed with the rule editor's own control stays out of the saved rule | REQ-APP-021", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("g", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    qapp_enter_edit_mode(session)

    session$setInputs(edit_rules = 1)
    session$setInputs(rule_add = 1)
    session$setInputs(rule_type = "group_condition")
    session$setInputs(
      rule_id = "oq_app_028_removed_row",
      rule_desc = "row 2 of each is removed before Save",
      rule_group_by = c("SUBJECT_ID", "VISIT")
    )

    # Row 1 of each: the row expected to survive.
    session$setInputs(
      gcond_name_1 = "c1_failed", gcond_col_1 = "STATUS", gcond_op_1 = "equals", gcond_val_1 = "DROPPED"
    )
    session$setInputs(
      gconstr_id_1 = "keep_constraint", gconstr_type_1 = "mutually_exclusive",
      gconstr_left_1 = "c1_failed", gconstr_right_1 = "c1_failed",
      gconstr_lscope_1 = "any", gconstr_rscope_1 = "any", gconstr_msg_1 = "Row 1"
    )

    # Row 2 of each: added, filled in, then removed via the real remove
    # button inputs before Save is ever pressed.
    session$setInputs(gcond_add = 1)
    session$setInputs(
      gcond_name_2 = "c2_removed", gcond_col_2 = "CONSENT_DATE", gcond_op_2 = "empty", gcond_val_2 = "false"
    )
    session$setInputs(gconstr_add = 1)
    session$setInputs(
      gconstr_id_2 = "remove_constraint", gconstr_type_2 = "mutually_exclusive",
      gconstr_left_2 = "c2_removed", gconstr_right_2 = "c2_removed",
      gconstr_lscope_2 = "any", gconstr_rscope_2 = "any", gconstr_msg_2 = "Row 2"
    )
    # The constraint row first: removing the condition row it depends on
    # while the constraint is still visible would otherwise be blocked.
    session$setInputs(gconstr_remove_2 = 1)
    session$setInputs(gcond_remove_2 = 1)

    session$setInputs(rule_save = 1)

    qa_check("the save reports no error", is.null(rv$rule_msg))
    rules <- DTAtools::datasets(rv$dta, "clinical_data")@specs@rules
    new_rule <- rules[[length(rules)]]
    qa_step(
      "the saved rule carries only the condition row that was never removed",
      "c1_failed", names(new_rule@conditions)
    )
    qa_step(
      "and only the constraint row that was never removed",
      list(n = 1L, id = "keep_constraint"),
      list(n = length(new_rule@constraints), id = new_rule@constraints[[1]]$id)
    )
  })
})

# ---- REQ-APP-026: a spec edit re-validates loaded data ------------------------

# The inputs the column editor form carries for SUBJECT_ID (SAS Char, not
# nullable) with only `length` and `values` varying. The bundled delivery holds
# ids such as SUBJ0001: eight characters each.
qapp_subject_id_form <- function(length = "", values = "") {
  list(
    col_id = "SUBJECT_ID", col_label = "Subject Identifier",
    col_backend = "SAS", col_type = "Char", col_format = "",
    col_length = length, col_nullable = FALSE, col_values = values,
    col_pattern = "", col_desc = "Unique subject ID"
  )
}

test_that("OQ-APP-033 | saving a column re-validates the loaded data without pressing check | REQ-APP-026", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("i", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = qapp_file_input("clinical_data.csv"))
    session$setInputs(check_all = 1)
    qa_step("the delivery passes before any edit", "pass", as_status(rv$status, "clinical_data"))

    qapp_enter_edit_mode(session)
    session$setInputs(edit_cols = 1)

    # A length of 20 holds an eight-character id: the verdict is re-reached, and
    # it is still a pass rather than a pending that would say nothing ran.
    do.call(session$setInputs, qapp_subject_id_form(length = "20"))
    session$setInputs(col_save = 1)
    qa_step("a column save the data satisfies keeps the dataset passing", "pass", as_status(rv$status, "clinical_data"))

    # A length of 5 cannot hold eight characters.
    do.call(session$setInputs, qapp_subject_id_form(length = "5"))
    session$setInputs(col_save = 2)
    qa_step(
      "a column save the data breaks fails the dataset without pressing check",
      "fail", as_status(rv$status, "clinical_data")
    )
    found <- DTAtools::messages(DTAtools::datasets(rv$dta, "clinical_data"), as_tibble = FALSE)
    found <- found[found$source == "columnspec", , drop = FALSE]
    qa_step(
      "and the dataset's messages are the over-length ones, in the edited column only",
      list(keywords = "maxLength", columns = "SUBJECT_ID"),
      list(
        keywords = sort(unique(as.character(found$keyword))),
        columns = sort(unique(as.character(found$column)))
      )
    )
  })
})

test_that("OQ-APP-034 | saving or removing a rule re-validates, and a dataset without data is left alone | REQ-APP-026", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("j", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = qapp_file_input("clinical_data.csv"))
    session$setInputs(check_all = 1)
    qapp_enter_edit_mode(session)
    session$setInputs(edit_rules = 1)
    n_before <- nrow(qapp_fn("dta_rules_overview")(rv$dta, "clinical_data"))

    # GENDER takes three values over 500 rows, so a uniqueness rule on it must
    # fail on the data already loaded.
    session$setInputs(rule_add = 1)
    session$setInputs(rule_type = "col_unique")
    session$setInputs(rule_id = "gender_unique", rule_desc = "", rule_cols = "GENDER")
    session$setInputs(rule_save = 1)
    qa_step("a rule save the data breaks fails the dataset", "fail", as_status(rv$status, "clinical_data"))

    overview <- qapp_fn("dta_rules_overview")(rv$dta, "clinical_data")
    qa_step("the rule was added", n_before + 1L, nrow(overview))
    session$setInputs(rule_del_click = nrow(overview))
    qa_step(
      "removing it restores the pass, again without pressing check",
      "pass", as_status(rv$status, "clinical_data")
    )
  })

  qapp_clean_session_files()
  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("k", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    qapp_enter_edit_mode(session)
    session$setInputs(edit_cols = 1)

    # No delivery has been offered, so there is nothing to validate against.
    do.call(session$setInputs, qapp_subject_id_form(length = "5"))
    session$setInputs(col_save = 1)
    qa_step("a dataset with no data stays nodata after a column save", "nodata", as_status(rv$status, "clinical_data"))
  })
})

test_that("OQ-APP-035 | a column edit whose values cannot fit is refused and changes nothing | REQ-APP-026", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("l", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = qapp_file_input("clinical_data.csv"))
    session$setInputs(check_all = 1)
    qapp_enter_edit_mode(session)
    session$setInputs(edit_cols = 1)
    before <- rv$dta

    # "ABCD" is four characters in a column of three.
    do.call(session$setInputs, qapp_subject_id_form(length = "3", values = "ABCD"))
    session$setInputs(col_save = 1)

    qa_step("the save reports an error", FALSE, isTRUE(rv$col_msg$ok))
    qa_step("naming it", TRUE, nzchar(rv$col_msg$error))
    qa_step("the document is untouched", TRUE, identical(rv$dta, before))
    qa_step("and so is the dataset's verdict", "pass", as_status(rv$status, "clinical_data"))
  })
})

test_that("OQ-APP-036 | an IF or THEN row removed with the rule editor's own control stays out of the saved rule | REQ-APP-027", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("h", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    qapp_enter_edit_mode(session)

    session$setInputs(edit_rules = 1)
    session$setInputs(rule_add = 1)
    session$setInputs(rule_type = "col_condition")
    session$setInputs(
      rule_id = "oq_app_033_removed_row",
      rule_desc = "IF row 1 and THEN row 2 are removed before Save"
    )

    # Row 1 of each list, then a second row added to each. Every row uses a
    # different column because the saved condition is keyed by column.
    session$setInputs(
      cond_col_1 = "GENDER", cond_op_1 = "equals", cond_val_1 = "Female",
      then_col_1 = "AGE", then_op_1 = "equals", then_val_1 = "12"
    )
    session$setInputs(cond_add = 1, then_add = 1)
    session$setInputs(
      cond_col_2 = "STATUS", cond_op_2 = "equals", cond_val_2 = "DROPPED",
      then_col_2 = "HEIGHT", then_op_2 = "equals", then_val_2 = "170"
    )

    # Filled in, then removed via the real remove-button inputs before Save.
    session$setInputs(cond_remove_1 = 1)
    session$setInputs(then_remove_2 = 1)

    session$setInputs(rule_save = 1)

    qa_check("the save reports no error", is.null(rv$rule_msg))
    rules <- DTAtools::datasets(rv$dta, "clinical_data")@specs@rules
    new_rule <- rules[[length(rules)]]
    qa_step(
      "the saved rule carries only the IF row never removed",
      "STATUS", names(new_rule@condition)
    )
    qa_step(
      "and only the THEN row never removed",
      "AGE", names(new_rule@then)
    )
  })
})

# ---- REQ-APP-028 .. REQ-APP-030: the editors keep rules and columns consistent --

# The bundled clinical specification declares AGE, used by a range rule and a
# conditional rule, and SUBJECT_ID, used by a uniqueness rule; BMI is used by
# no rule.
qapp_clinical_columns <- function(rv) {
  names(DTAtools::datasets(rv$dta, "clinical_data")@specs@columns)
}

test_that("OQ-APP-037 | a column a rule uses cannot be removed, one no rule uses can | REQ-APP-028", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("n", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    qapp_enter_edit_mode(session)
    session$setInputs(edit_cols = 1)
    before_dta <- rv$dta
    before <- qapp_clinical_columns(rv)

    session$setInputs(col_del_click = match("AGE", before))
    qa_step("removing a column a rule uses leaves the columns as they were", before, qapp_clinical_columns(rv))
    qa_check("and the document itself is untouched", identical(rv$dta, before_dta))

    session$setInputs(col_del_click = match("BMI", before))
    qa_step(
      "removing a column no rule uses removes exactly that column",
      setdiff(before, "BMI"), qapp_clinical_columns(rv)
    )
  })
})

test_that("OQ-APP-038 | renaming a column in the editor renames it in the rules that use it | REQ-APP-029", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("o", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    qapp_enter_edit_mode(session)
    session$setInputs(edit_cols = 1)
    ids <- qapp_clinical_columns(rv)
    session$setInputs(col_edit_click = match("AGE", ids))

    # Only the id differs from the stored column.
    form <- qapp_fn("dta_column_fields")(rv$dta, "clinical_data", "AGE")
    do.call(session$setInputs, list(
      col_id = "PATIENT_AGE", col_label = form$label, col_backend = form$backend,
      col_type = form$type, col_format = form$format, col_length = form$length,
      col_nullable = form$nullable, col_values = form$values,
      col_pattern = form$pattern, col_examples = form$examples,
      col_desc = form$description
    ))
    session$setInputs(col_save = 1)

    after <- qapp_clinical_columns(rv)
    qa_step("the column carries its new id and not the old", c(new = TRUE, old = FALSE), c(new = "PATIENT_AGE" %in% after, old = "AGE" %in% after))
    rl <- DTAtools::datasets(rv$dta, "clinical_data")@specs@rules
    range_rule <- rl[[which(vapply(rl, function(r) r@id, character(1)) == "rule_range_example")]]
    qa_step("the range rule on it follows the rename", "PATIENT_AGE", range_rule@columns)
    qa_step("and the linter finds nothing left dangling", 0L, nrow(DTAtools::lint_dta(rv$dta)))
  })
})

test_that("OQ-APP-039 | raw YAML whose rules contradict its columns is refused and the same text repaired applies | REQ-APP-030", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("p", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    qapp_enter_edit_mode(session)
    good <- rv$yaml_text
    before_dta <- rv$dta

    # The uniqueness rule's column list "SUBJECT_ID, VISIT" gets a column the
    # specification does not declare.
    bad <- sub("- SUBJECT_ID(\\s+)- VISIT", "- NOPE\\1- VISIT", good)
    qa_check("the contradiction was introduced", !identical(bad, good))

    session$setInputs(raw_yaml_editor = bad)
    session$setInputs(apply_yaml = 1)
    qa_step("the apply is refused", FALSE, isTRUE(rv$yaml_msg$ok))
    qa_check(
      "with the undeclared column among the linter's findings",
      "rule_column_undeclared" %in% rv$yaml_msg$findings$code
    )
    qa_step("and every finding is an error", "error", unique(rv$yaml_msg$findings$severity[rv$yaml_msg$findings$code == "rule_column_undeclared"]))
    qa_check("the document the application holds is unchanged", identical(rv$dta, before_dta))

    session$setInputs(raw_yaml_editor = good)
    session$setInputs(apply_yaml = 2)
    qa_step("the unaltered text applies", TRUE, isTRUE(rv$yaml_msg$ok))
  })
})

test_that("OQ-APP-040 | a group-condition name whose row was removed is no longer offered to constraints | REQ-APP-031 | tags: white-box", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()

  shiny::testServer(app, {
    session$setInputs(dta_client_id = strrep("i", 32))
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    qapp_enter_edit_mode(session)

    # No browser here, so record what the server would send to one: the
    # options updateSelectInput() pushes, and the markup insertUI() inserts.
    inserted <- character(0)
    sent <- list()
    session$sendInsertUI <- function(selector, multiple, where, content) {
      inserted <<- c(inserted, as.character(content$html))
      invisible()
    }
    session$sendInputMessage <- function(inputId, message) {
      sent[[inputId]] <<- message
      invisible()
    }
    opt_values <- function(html) {
      sub('^value="(.*)"$', "\\1", regmatches(html, gregexpr('value="[^"]*"', html))[[1]])
    }

    session$setInputs(edit_rules = 1)
    session$setInputs(rule_add = 1)
    session$setInputs(rule_type = "group_condition")
    session$setInputs(
      gcond_name_1 = "c1_kept", gcond_col_1 = "STATUS",
      gcond_op_1 = "equals", gcond_val_1 = "FAILED"
    )
    session$setInputs(gcond_add = 1)
    session$setInputs(
      gcond_name_2 = "c2_removed", gcond_col_2 = "CONSENT_DATE",
      gcond_op_2 = "empty", gcond_val_2 = "false"
    )

    # Shiny keeps a removed input's last value, so the removal alone has to
    # refresh the selectors of the constraint row that already exists.
    sent <- list()
    session$setInputs(gcond_remove_2 = 1)
    qa_check(
      "the removal alone refreshes the existing constraint's selectors",
      !is.null(sent$gconstr_left_1)
    )
    qa_step(
      "Left/IF offers only the condition still present",
      c("", "c1_kept"), sort(opt_values(sent$gconstr_left_1$options))
    )
    qa_step(
      "Right/THEN offers only the condition still present",
      c("", "c1_kept"), sort(opt_values(sent$gconstr_right_1$options))
    )

    session$setInputs(gconstr_add = 1)
    html <- paste(inserted[grepl('id="gconstr_left_2"', inserted, fixed = TRUE)], collapse = "")
    qa_check(
      "a constraint row added afterwards does not offer the removed name",
      !grepl('value="c2_removed"', html, fixed = TRUE) &&
        grepl('value="c1_kept"', html, fixed = TRUE)
    )
  })
})
