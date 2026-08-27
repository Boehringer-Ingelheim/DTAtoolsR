# End-to-end server test of the Files-dataset journey.
#
# The unit tests around it cover each helper in isolation. This one drives the
# actual server the way a user does -- add a Files dataset, declare a file,
# upload into the slot, validate, overwrite, discard -- because the defect this
# fixes was invisible to every helper test: `load_file()` simply had no method
# for a file dataset, and only the wiring that runs between the upload input and
# the package showed it.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

file_ds_app_dir <- function() .shiny_app_dir()

# A fileInput() value for a file this test creates, rather than one bundled with
# the package -- the point is to deliver a non-tabular file, which the fixtures
# deliberately do not contain.
file_ds_upload <- function(path, name = basename(path)) {
  data.frame(
    name = name, size = file.size(path), type = "",
    datapath = path, stringsAsFactors = FALSE
  )
}

file_ds_write <- function(dir, name, contents = "report body") {
  path <- file.path(dir, name)
  writeLines(contents, path)
  path
}

# Every upload slot is keyed by the dataset's POSITION, not its name, so the
# input id has to be derived from the structure rather than written down: a
# fixture gaining a dataset would otherwise silently point these tests at the
# wrong slot.
file_ds_set <- function(session, name, value) {
  do.call(session$setInputs, stats::setNames(list(value), name))
}

# Select a dataset in the nav the way a user actually does: input$active_ds
# does not exist anywhere in inst/shiny/dta_app -- the app never reads it.
# What actually moves rv$active is clicking a nav row, which fires
# `selds_<position>`, keyed by the dataset's position in rv$structure (see the
# nav_registry observer in app.R). The position has to be read back from
# rv$structure rather than assumed, because it depends on add/remove order.
file_ds_select <- function(session, rv, name) {
  idx <- rv$structure[[name]]$index
  do.call(session$setInputs, stats::setNames(list(1), paste0("selds_", idx)))
}

file_ds_clean_session <- function() {
  unlink(
    list.files(tempdir(),
      pattern = "^dtatools_app_session.*\\.rds$", full.names = TRUE
    ),
    force = TRUE
  )
}

# Everything below starts from the same place: a loaded fixture, plus one Files
# dataset declaring any `report*` file whose ending is `.pdf`. Written as a
# quoted expression rather than a function because testServer() evaluates its
# body in a session-scoped environment that a called function cannot reach.
file_ds_setup <- quote({
  session$setInputs(edit_mode = TRUE)
  session$setInputs(dta_file = file_ds_upload(app_fixture_path("clinical_dta.yaml")))
  session$setInputs(add_ds_name = "reports", add_ds_type = "file")
  session$setInputs(add_ds_save = 1)
  file_ds_select(session, rv, "reports")
  testthat::expect_equal(rv$active, "reports")
  session$setInputs(edit_files = 1)
  session$setInputs(file_add = 1)
  session$setInputs(
    file_filename = "^report.*", file_type = "any", file_pattern = TRUE,
    file_extensions = "pdf", file_count_mode = "range",
    file_min_number_of_files = 1, file_max_number_of_files = 5
  )
  session$setInputs(file_save = 1)
  slot <- paste0("up_", rv$structure[["reports"]]$index, "_1")
})

test_that("a Files dataset accepts an upload, which is the regression this fixes", {
  file_ds_clean_session()
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  shiny::testServer(file_ds_app_dir(), {
    eval(file_ds_setup)

    # The handler the editor built is the reader-less one, carrying the
    # restriction the form asked for.
    handler <- DTAtools::datasets(rv$dta, "reports")@files[[1]]
    expect_s3_class(handler, "DTAtools::DTAFileAny")
    expect_equal(handler@extensions, "pdf")

    file_ds_set(session, slot, file_ds_upload(file_ds_write(dir, "report_2024.pdf")))

    ds <- DTAtools::datasets(rv$dta, "reports")
    # Bound at all: before the fix this aborted inside load_file() and the
    # upload was reported to the user as rejected.
    expect_equal(length(ds@file_paths), 1L)
    # Keyed by the basename WITH its extension, which is what every report for
    # a file dataset uses -- and what the app's own list and remove button key
    # on.
    expect_equal(basename(ds@file_paths), "report_2024.pdf")
  })
})

test_that("a Files dataset validates a delivered file, and reports a missing one", {
  file_ds_clean_session()
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  shiny::testServer(file_ds_app_dir(), {
    eval(file_ds_setup)

    pdf <- file_ds_write(dir, "report_a.pdf")
    file_ds_set(session, slot, file_ds_upload(pdf))

    session$setInputs(check_one = 1)
    expect_equal(rv$status[["reports"]], "pass")
    expect_true(DTAtools::validation_status(DTAtools::datasets(rv$dta, "reports"))$ok)

    # The delivered file goes away after it was bound. Note the app STAGES an
    # upload into a temp copy under its original name (so the handler sees the
    # real name rather than Shiny's "0.pdf"), which is what actually gets
    # bound -- so it is the bound path that has to be removed here, not the
    # file this test wrote.
    #
    # check() is the only thing that ever looks at the disk, which is why
    # load_file() deliberately stats nothing: a file can be declared and bound
    # before it exists, and its absence surfaces here as a failure rather than
    # as a refusal to record it.
    bound <- DTAtools::datasets(rv$dta, "reports")@file_paths
    unlink(bound)
    session$setInputs(check_one = 2)
    expect_equal(rv$status[["reports"]], "fail")

    msgs <- DTAtools::messages(DTAtools::datasets(rv$dta, "reports"), as_tibble = FALSE)
    expect_equal(nrow(msgs), 1L)
    expect_match(msgs$message, "not found")
  })
})

test_that("an ending the handler does not allow is refused as it is uploaded", {
  file_ds_clean_session()
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  shiny::testServer(file_ds_app_dir(), {
    eval(file_ds_setup)

    file_ds_set(
      session, slot,
      file_ds_upload(file_ds_write(dir, "report_2024.csv", "a,b\n1,2"))
    )

    # Nothing bound: the restriction is enforced by matches_filename(), which
    # the upload gate consults before load_file() is ever reached.
    expect_equal(length(DTAtools::datasets(rv$dta, "reports")@file_paths), 0L)
  })
})

test_that("re-uploading a file replaces it rather than binding it twice", {
  file_ds_clean_session()
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  shiny::testServer(file_ds_app_dir(), {
    eval(file_ds_setup)

    pdf <- file_ds_write(dir, "report_b.pdf")
    file_ds_set(session, slot, file_ds_upload(pdf))
    session$setInputs(check_one = 1)
    expect_true(DTAtools::validation_status(DTAtools::datasets(rv$dta, "reports"))$ok)

    # Delivering the same name again goes through the overwrite gate, which asks
    # before it replaces bound data.
    file_ds_set(session, slot, file_ds_upload(pdf))
    session$setInputs(confirm_overwrite = 1)

    ds <- DTAtools::datasets(rv$dta, "reports")
    expect_equal(length(ds@file_paths), 1L)
    # Replaced data is never left wearing the previous verdict.
    expect_equal(rv$status[["reports"]], "pending")
  })
})

test_that("discard-all empties a Files dataset", {
  file_ds_clean_session()
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  shiny::testServer(file_ds_app_dir(), {
    eval(file_ds_setup)

    file_ds_set(session, slot, file_ds_upload(file_ds_write(dir, "report_c.pdf")))
    file_ds_set(session, slot, file_ds_upload(file_ds_write(dir, "report_d.pdf")))
    expect_equal(length(DTAtools::datasets(rv$dta, "reports")@file_paths), 2L)

    session$setInputs(confirm_discard_all = 1)
    expect_equal(length(DTAtools::datasets(rv$dta, "reports")@file_paths), 0L)
    expect_equal(rv$status[["reports"]], "nodata")
  })
})

test_that("a Files dataset with nothing bound reports no data rather than passing", {
  file_ds_clean_session()

  shiny::testServer(file_ds_app_dir(), {
    session$setInputs(edit_mode = TRUE)
    session$setInputs(dta_file = file_ds_upload(app_fixture_path("clinical_dta.yaml")))
    session$setInputs(add_ds_name = "empty_reports", add_ds_type = "file")
    session$setInputs(add_ds_save = 1)
    file_ds_select(session, rv, "empty_reports")
    expect_equal(rv$active, "empty_reports")

    session$setInputs(check_one = 1)

    # A dataset with nothing bound must never be reported as passed, and the
    # empty validation_status() such a dataset produces must not break check().
    expect_equal(rv$status[["empty_reports"]], "nodata")
  })
})

# ---------------------------------------------------------------------------
# The handler form fixes the type at `any` for a Files dataset
# ---------------------------------------------------------------------------

test_that("the handler form for a Files dataset offers no type to choose", {
  # DTADataSetFile does not parse, so `csv`/`tsv` must not be reachable from
  # the form at all. The control stays in the markup rather than being removed:
  # the "Allowed file endings" panel is conditional on `input.file_type`, and
  # an absent input would read as falsy and hide it.
  file_ds_clean_session()

  shiny::testServer(file_ds_app_dir(), {
    session$setInputs(edit_mode = TRUE)
    session$setInputs(dta_file = file_ds_upload(app_fixture_path("clinical_dta.yaml")))
    session$setInputs(add_ds_name = "reports", add_ds_type = "file")
    session$setInputs(add_ds_save = 1)
    file_ds_select(session, rv, "reports")
    expect_equal(rv$active, "reports")
    session$setInputs(edit_files = 1)
    session$setInputs(file_add = 1)

    html <- paste(output$file_modal_body$html, collapse = "\n")

    # The control exists, carries the one legal value, and cannot be edited.
    expect_match(html, "id=\"file_type\"")
    expect_match(html, "value=\"any\"")
    expect_match(html, "disabled")
    # ...and neither parsing type is on offer.
    expect_false(grepl("value=\"csv\"", html, fixed = TRUE))
    expect_false(grepl("value=\"tsv\"", html, fixed = TRUE))
  })
})

test_that("the handler form for a tabular dataset still offers csv and tsv", {
  # The guard that the change above is scoped to file datasets: this drives
  # the SAME edit_files/file_add flow as the two tests above, but against
  # "clinical_data" (tabular) rather than a Files dataset.
  file_ds_clean_session()

  shiny::testServer(file_ds_app_dir(), {
    session$setInputs(edit_mode = TRUE)
    session$setInputs(dta_file = file_ds_upload(app_fixture_path("clinical_dta.yaml")))
    file_ds_select(session, rv, "clinical_data")
    expect_equal(rv$active, "clinical_data")
    session$setInputs(edit_files = 1)
    session$setInputs(file_add = 1)

    html <- paste(output$file_modal_body$html, collapse = "\n")

    expect_match(html, "value=\"csv\"")
    expect_match(html, "value=\"tsv\"")
    expect_false(grepl("value=\"any\"", html, fixed = TRUE))
  })
})

test_that("saving the form's own default builds an unparsed handler", {
  # The form can only ever send "any" for a Files dataset, so the save path is
  # driven here WITHOUT naming a type beyond that default -- what the user
  # actually gets when they fill in a name and press Save.
  file_ds_clean_session()

  shiny::testServer(file_ds_app_dir(), {
    session$setInputs(edit_mode = TRUE)
    session$setInputs(dta_file = file_ds_upload(app_fixture_path("clinical_dta.yaml")))
    session$setInputs(add_ds_name = "reports", add_ds_type = "file")
    session$setInputs(add_ds_save = 1)
    file_ds_select(session, rv, "reports")
    expect_equal(rv$active, "reports")
    session$setInputs(edit_files = 1)
    session$setInputs(file_add = 1)
    session$setInputs(
      file_filename = "summary.pdf", file_type = "any", file_pattern = FALSE
    )
    session$setInputs(file_save = 1)

    handler <- DTAtools::datasets(rv$dta, "reports")@files[[1]]
    expect_s3_class(handler, "DTAtools::DTAFileAny")
    expect_equal(handler@filename, "summary.pdf")
  })
})

# ---- the Loaded files panel actually renders --------------------------------
#
# THE GAP THIS CLOSES: every test above drives the upload and then inspects
# rv$dta. None of them ever asked for the OUTPUT, so the panel that the user
# actually looks at was never rendered by the suite -- and "Error: subscript out
# of bounds" in that panel survived a whole round of fixes to this journey.
# Reading output$loaded_files is the entire point of these two tests.

test_that("the Loaded files panel renders a freshly uploaded file as pending", {
  # THE REPORTED BUG: the upload succeeded (so a retry offered to overwrite a
  # file the user never saw arrive) and the panel then aborted drawing the
  # per-file tick, because a file dataset has no validation_status() rows at all
  # until check() runs and the tick looked its name up with `[[`.
  file_ds_clean_session()
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  shiny::testServer(file_ds_app_dir(), {
    eval(file_ds_setup)
    file_ds_set(session, slot, file_ds_upload(file_ds_write(dir, "report_2024.pdf")))

    # The precondition that made it abort: nothing validated, so the status map
    # this panel indexes is empty.
    expect_length(app_fn("dta_table_status_map")(rv$dta, "reports"), 0L)

    html <- paste(as.character(output$loaded_files$html), collapse = "")
    expect_match(html, "report_2024.pdf", fixed = TRUE)
    # Drawn with the not-validated-yet tick rather than a pass or a fail.
    expect_match(html, "file-pending", fixed = TRUE)
    expect_false(grepl("file-ok", html, fixed = TRUE))
  })
})

test_that("the Loaded files panel shows the passing tick once the dataset is checked", {
  # The guard must not flatten every file to "pending": a real status still has
  # to reach the panel.
  file_ds_clean_session()
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  shiny::testServer(file_ds_app_dir(), {
    eval(file_ds_setup)
    file_ds_set(session, slot, file_ds_upload(file_ds_write(dir, "report_2024.pdf")))
    session$setInputs(check_one = 1)
    expect_equal(rv$status[["reports"]], "pass")

    html <- paste(as.character(output$loaded_files$html), collapse = "")
    expect_match(html, "report_2024.pdf", fixed = TRUE)
    expect_match(html, "file-ok", fixed = TRUE)
  })
})

# ---------------------------------------------------------------------------
# The column and rule editors refuse a file dataset, even with edit mode on
# ---------------------------------------------------------------------------
#
# ds_edit_menu() (ui_components.R) only hides the Columns/Rules rows for a
# "file" dataset -- an input that is not on screen can still be driven over
# the websocket, so edit_cols/edit_rules and col_save/rule_save each re-check
# rv$structure[[...]]$type themselves rather than trust the render gate.
# Before the fix there was no re-check: driving edit_cols on a file dataset
# opened the column editor, and saving surfaced raw S7 internals ("Can't find
# property <DTAtools::DTADataSetFile>@specs") because DTADataSetFile has no
# @specs for dta_column_ids()/dta_set_column() to read.

test_that("edit_cols and edit_rules refuse to open on a file dataset", {
  file_ds_clean_session()

  shiny::testServer(file_ds_app_dir(), {
    session$setInputs(edit_mode = TRUE)
    session$setInputs(dta_file = file_ds_upload(app_fixture_path("clinical_dta.yaml")))
    session$setInputs(add_ds_name = "reports", add_ds_type = "file")
    session$setInputs(add_ds_save = 1)
    file_ds_select(session, rv, "reports")
    expect_equal(rv$active, "reports")

    session$setInputs(edit_cols = 1)
    expect_null(rv$editor_dataset)
    expect_equal(rv$col_view, "list") # its initial value -- never touched
    expect_equal(rv$col_token, 0) # the modal-render trigger never bumped

    session$setInputs(edit_rules = 1)
    expect_null(rv$editor_dataset)
    expect_equal(rv$rule_view, "list")
    expect_equal(rv$rule_token, 0)
  })
})

test_that("col_save and rule_save refuse a stashed editor_dataset that is a file dataset", {
  # rv$editor_dataset is shared across the file/column/rule editors. The one
  # way it can legitimately point at a file dataset is the File editor
  # (edit_files) -- open THAT, then drive col_save/rule_save directly,
  # bypassing edit_cols/edit_rules (and their own type guard) entirely. This
  # is exactly the "raced or bypassed the same way edit_cols can" scenario
  # the save handlers' own re-check comments describe.
  file_ds_clean_session()

  shiny::testServer(file_ds_app_dir(), {
    session$setInputs(edit_mode = TRUE)
    session$setInputs(dta_file = file_ds_upload(app_fixture_path("clinical_dta.yaml")))
    session$setInputs(add_ds_name = "reports", add_ds_type = "file")
    session$setInputs(add_ds_save = 1)
    file_ds_select(session, rv, "reports")

    session$setInputs(edit_files = 1)
    expect_equal(rv$editor_dataset, "reports")

    before_cols <- app_fn("dta_column_ids")(rv$dta, "clinical_data")
    session$setInputs(
      col_id = "SNEAKY", col_backend = "SAS", col_type = "Char", col_save = 1
    )
    expect_equal(app_fn("dta_column_ids")(rv$dta, "clinical_data"), before_cols)
    expect_null(rv$col_msg)

    before_rules <- nrow(app_fn("dta_rules_overview")(rv$dta, "clinical_data"))
    session$setInputs(rule_id = "SNEAKY", rule_save = 1)
    expect_equal(nrow(app_fn("dta_rules_overview")(rv$dta, "clinical_data")), before_rules)
    expect_null(rv$rule_msg)
  })
})

test_that("edit_cols and edit_rules still open on a tabular dataset (positive control)", {
  file_ds_clean_session()

  shiny::testServer(file_ds_app_dir(), {
    session$setInputs(edit_mode = TRUE)
    session$setInputs(dta_file = file_ds_upload(app_fixture_path("clinical_dta.yaml")))
    # "clinical_data" is tabular and already active on load -- no selection needed.
    expect_equal(rv$active, "clinical_data")

    session$setInputs(edit_cols = 1)
    expect_equal(rv$editor_dataset, "clinical_data")

    session$setInputs(edit_rules = 1)
    expect_equal(rv$editor_dataset, "clinical_data")
  })
})

# ---------------------------------------------------------------------------
# The zero-handlers banner in the file editor
# ---------------------------------------------------------------------------

test_that("the file editor shows the zero-handlers banner for a freshly added Files dataset", {
  # A dataset created via "+ Add dataset" starts with no handlers at all, and
  # nothing can be uploaded into it until at least one exists -- the banner is
  # what makes that consequence explicit in the editor that can fix it.
  file_ds_clean_session()

  shiny::testServer(file_ds_app_dir(), {
    session$setInputs(edit_mode = TRUE)
    session$setInputs(dta_file = file_ds_upload(app_fixture_path("clinical_dta.yaml")))
    session$setInputs(add_ds_name = "reports", add_ds_type = "file")
    session$setInputs(add_ds_save = 1)
    file_ds_select(session, rv, "reports")
    expect_equal(rv$active, "reports")

    session$setInputs(edit_files = 1)

    html <- paste(output$file_modal_body$html, collapse = "\n")
    expect_match(
      html,
      "This dataset expects no files at all, so nothing can be loaded into it. Add at least one entry.",
      fixed = TRUE
    )
  })
})

test_that("the zero-handlers banner is gone once the dataset has a handler", {
  file_ds_clean_session()

  shiny::testServer(file_ds_app_dir(), {
    eval(file_ds_setup) # adds one "any" handler to "reports"
    session$setInputs(edit_files = 1) # re-open in list view

    html <- paste(output$file_modal_body$html, collapse = "\n")
    expect_false(grepl("expects no files at all", html, fixed = TRUE))
  })
})
