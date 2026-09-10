# The application's server logic. REQ-APP-001 .. REQ-APP-013.
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
    session$setInputs(add_ds_name = "locked_ds", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 1)
    qa_step(
      "an add attempted outside edit mode changes nothing",
      before, names(rv$status)
    )

    qapp_enter_edit_mode(session)
    session$setInputs(add_ds_name = "second_ds", add_ds_type = "tabular")
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
    session$setInputs(add_ds_name = "to_remove", add_ds_type = "tabular")
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
    session$setInputs(add_ds_name = "empty_ds", add_ds_type = "tabular")
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
    session$setInputs(add_ds_name = "while_editing", add_ds_type = "tabular")
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
    session$setInputs(add_ds_name = "after_locking", add_ds_type = "tabular")
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
