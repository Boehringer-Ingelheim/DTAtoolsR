# Edit mode: the read-only/editable state for the Shiny app.
#
# The app is read-only by default. Editing state is server-owned (rv$editing,
# read through the editing() reactive) rather than a single toggle input; it
# is entered through one of three input ids -- enable_edit_mode (unlock the
# document as it stands), create_new_version (via its modal), or
# create_new_document (via its modal) -- and left through stop_editing.
# enable_edit_mode and stop_editing are the two halves of one menu row that
# flips (edit_menu()), so exactly one of them is on offer at any moment.
# Whichever route unlocks it, the same surfaces open up: the dataset Edit
# menu, the Metadata tab, the Raw YAML editor, and adding or removing
# datasets. The control (edit_menu()/edit_status_tag(), inst/shiny/dta_app/R/
# ui_components.R) is only the affordance; every observer behind it still
# calls req(editing()), which is the enforcement these tests exercise.

render_html <- function(tag) {
  paste(as.character(tag), collapse = "\n")
}

# ---- Pure UI: meta_field_text() ---------------------------------------------

test_that("meta_field_text renders the label and value as plain text, not an input", {
  html <- render_html(app_fn("meta_field_text")("Title", "Study A"))

  expect_match(html, "Title", fixed = TRUE)
  expect_match(html, "Study A", fixed = TRUE)
  expect_false(grepl("<input", html, fixed = TRUE))
})

test_that("meta_field_text renders an em dash instead of an empty gap for an empty value", {
  html <- render_html(app_fn("meta_field_text")("Title", ""))

  expect_match(html, "—", fixed = TRUE)
})

test_that("meta_field_text renders an em dash and does not error for a NULL value", {
  # NULL is what an unset DTAMetaData property reads back as; the field
  # needs to render as "not filled in", not throw, when the dataset simply
  # has no value for it yet.
  html <- render_html(app_fn("meta_field_text")("Title", NULL))

  expect_match(html, "—", fixed = TRUE)
})

test_that("meta_field_text survives a length-0 or NA value", {
  # An unset S7 property reads back as character(0), not NULL, and %||% only
  # guards NULL -- so going straight to nzchar() would hand `if` a logical(0)
  # and abort. Both collapse to the em dash instead.
  expect_match(render_html(app_fn("meta_field_text")("Title", character(0))), "—", fixed = TRUE)
  expect_match(render_html(app_fn("meta_field_text")("Title", NA_character_)), "—", fixed = TRUE)
})

# ---- Pure UI: contact_detail_block() ----------------------------------------
#
# The read-only counterpart of one contact row (render_contacts(), app.R).
# Editable mode only ever shows contact_display()'s short "name — role"; this
# is what makes the rest reachable while edit mode is off.

test_that("contact_detail_block renders email, department, phone and address when present", {
  html <- render_html(app_fn("contact_detail_block")(list(
    name = "Alice Smith", role = "Lead Data Manager",
    email = "alice.smith@testcompany.com", department = "Data Management",
    phone = "555-123-4567", address = "123 Main St, Cityville"
  )))

  expect_match(html, "alice.smith@testcompany.com", fixed = TRUE)
  expect_match(html, "Data Management", fixed = TRUE)
  expect_match(html, "555-123-4567", fixed = TRUE)
  expect_match(html, "123 Main St, Cityville", fixed = TRUE)
  # The heading carries the name/role the way contact_display() already does.
  expect_match(html, "Alice Smith", fixed = TRUE)
  expect_match(html, "Lead Data Manager", fixed = TRUE)
})

test_that("contact_detail_block omits an absent field rather than an empty row", {
  # Only name + one field set: department/phone/address must not appear at
  # all -- not as an empty "Department:" row, which the length-0/NA-safe
  # value handling below exists specifically to avoid.
  html <- render_html(app_fn("contact_detail_block")(list(
    name = "Bob Johnson", email = "bob@example.com"
  )))

  expect_match(html, "bob@example.com", fixed = TRUE)
  expect_no_match(html, "contact-detail-label\">Department", fixed = TRUE)
  expect_no_match(html, "contact-detail-label\">Phone", fixed = TRUE)
  expect_no_match(html, "contact-detail-label\">Address", fixed = TRUE)
})

test_that("contact_detail_block is NULL/character(0)/NA-safe for every optional field", {
  # Same guarantee meta_field_text() gives a Metadata field: none of the three
  # forms an unset value can take should error, and none should render as an
  # empty row.
  person <- list(
    name = "Edge Case", email = NULL, department = character(0),
    phone = NA_character_, address = "Real Address"
  )

  html <- expect_no_error(render_html(app_fn("contact_detail_block")(person)))

  expect_match(html, "Real Address", fixed = TRUE)
  expect_no_match(html, "contact-detail-label\">Email", fixed = TRUE)
  expect_no_match(html, "contact-detail-label\">Department", fixed = TRUE)
  expect_no_match(html, "contact-detail-label\">Phone", fixed = TRUE)
})

test_that("contact_detail_block renders every element of a list-valued field, not just the first", {
  # .ro_field_value() used to keep only element 1 of a list/vector value and
  # drop the rest. Read-only mode is the ONLY place these fields render (see
  # the WHY comment on contact_detail_block()), so a dropped line was
  # unreachable to whoever reads the page -- and a YAML sequence (`address:`
  # written as several lines) is exactly this shape.
  html <- render_html(app_fn("contact_detail_block")(list(
    name = "Alice Smith", address = list("123 Main St", "Suite 400")
  )))

  expect_match(html, "123 Main St", fixed = TRUE)
  expect_match(html, "Suite 400", fixed = TRUE)
})

test_that("contact_detail_block renders an all-empty list value as empty, not the string \"NULL\"", {
  # unlist() drops NULL entries on its own; without that, a value like
  # `address: [~]` (a one-element list holding NULL) would render as the
  # literal text "NULL" instead of being omitted like any other unset field.
  html <- render_html(app_fn("contact_detail_block")(list(
    name = "Alice Smith", address = list(NULL, NA_character_, "")
  )))

  expect_no_match(html, "NULL", fixed = TRUE)
  expect_no_match(html, "contact-detail-label\">Address", fixed = TRUE)
})

test_that("contact_detail_block shows the signature/reviewer flags only when TRUE", {
  both <- render_html(app_fn("contact_detail_block")(list(
    name = "Alice Smith", signature = TRUE, reviewer = TRUE
  )))
  neither <- render_html(app_fn("contact_detail_block")(list(
    name = "Bob Johnson", signature = FALSE
  )))
  unset <- render_html(app_fn("contact_detail_block")(list(name = "No Flags")))

  expect_match(both, "contact-detail-flag\">Signature", fixed = TRUE)
  expect_match(both, "contact-detail-flag\">Reviewer", fixed = TRUE)
  expect_no_match(neither, "contact-detail-flag", fixed = TRUE)
  expect_no_match(unset, "contact-detail-flag", fixed = TRUE)
})

# ---- Server-side edit-mode guards -------------------------------------------
#
# Every editing surface is gated twice: the control is not rendered, and the
# observer behind it calls req(editing()). These cover the observer half --
# the part that still has to hold when an input is driven directly, which is
# exactly what testServer() does.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

app_server_dir <- function() .shiny_app_dir()

app_file_input <- function(filename) {
  path <- app_fixture_path(filename)
  data.frame(
    name = filename, size = file.size(path), type = "",
    datapath = path, stringsAsFactors = FALSE
  )
}

clean_session_file <- function() {
  f <- list.files(tempdir(),
    pattern = "^dtatools_app_session.*\\.rds$", full.names = TRUE
  )
  unlink(f, force = TRUE)
  invisible(f)
}

# Load the fixture DTA into a running server, ready to edit.
load_fixture <- function(session) {
  session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
}

test_that("editing starts off for a fresh server session", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    expect_false(editing())
  })
})

test_that("enable_edit_mode is a working affordance into edit mode", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    enter_edit_mode(session)

    expect_true(editing())
  })
})

test_that("with edit mode off, adding a dataset does nothing", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    leave_edit_mode(session)
    load_fixture(session)
    before <- names(DTAtools::datasets(rv$dta))

    session$setInputs(add_ds_name = "sneaky", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 1)

    expect_equal(names(DTAtools::datasets(rv$dta)), before)
  })
})

test_that("with edit mode off, removing a dataset does nothing", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    leave_edit_mode(session)
    load_fixture(session)
    before <- names(DTAtools::datasets(rv$dta))

    session$setInputs(remove_dataset_confirm = 1)

    expect_equal(names(DTAtools::datasets(rv$dta)), before)
  })
})

test_that("with edit mode off, applying raw YAML does not replace the document", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    leave_edit_mode(session)
    load_fixture(session)
    before <- rv$yaml_text

    session$setInputs(raw_yaml_editor = "metadata:\n  title: HIJACKED\n")
    session$setInputs(apply_yaml = 1)

    expect_equal(rv$yaml_text, before)
    expect_false(identical(
      as.character(S7::prop(DTAtools::metadata(rv$dta), "title")), "HIJACKED"
    ))
  })
})

test_that("the Raw YAML (Ace) editor is born read-only when edit mode starts off", {
  # readOnly is set at aceEditor() creation time (isolate(!editing())), not
  # left to the later observe() toggle -- that observer only fires on LATER
  # editing() changes and messages an input id that does not exist yet the
  # first time it runs, since this editor lives inside output$main, which is
  # still showing the landing card at server start. Relying on the observer
  # alone would leave the very first render editable until edit mode was
  # toggled at least once. shinyAce serialises its config (including
  # readOnly) into a `<script type="application/json" data-for="...">` tag
  # alongside the editor, which is what makes this observable through
  # testServer without a real browser.
  skip_if_not_installed("shinyAce")
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    leave_edit_mode(session)
    load_fixture(session)

    html <- paste(as.character(output$main$html), collapse = "\n")

    expect_match(html, '"id":"raw_yaml_editor"', fixed = TRUE)
    expect_match(html, '"readOnly":true', fixed = TRUE)
  })
})

test_that("the Raw YAML (Ace) editor is born editable when edit mode starts on", {
  # The positive control for the test above.
  #
  # A document loaded via load_fixture() is ALWAYS version-locked at the
  # instant output$main first renders it: apply_loaded() sets
  # rv$version_locked <- TRUE before the doc_token bump that triggers that
  # render, so editing() cannot be TRUE at that instant for an upload no
  # matter what rv$editing already was. unlock_editing() alone cannot
  # reach a born-editable ace editor here -- it does not itself bump
  # doc_token, and output$main only re-renders on load/reset/restore (see
  # the WHY comment on doc_token in app.R), so an unlock arriving AFTER the
  # born-locked render leaves that already-rendered HTML untouched; only a
  # separate observe() flips the *live* editor via a session message, which
  # is exactly the mechanism this test exists to NOT rely on (see the
  # comment above). What DOES produce a document that is already unlocked
  # the moment output$main renders it is restoring a session that was
  # autosaved AFTER an unlock -- unlock_editing()'s sync_yaml_text() call
  # autosaves the unlocked state, and that is a real user journey (reload
  # the page after creating a new version), not a contrivance.
  skip_if_not_installed("shinyAce")
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = strrep("f", 32))
    load_fixture(session)
    unlock_editing(session)
    session$setInputs(restore_session = 1)

    html <- paste(as.character(output$main$html), collapse = "\n")

    expect_match(html, '"id":"raw_yaml_editor"', fixed = TRUE)
    expect_match(html, '"readOnly":false', fixed = TRUE)
  })
})

test_that("with edit mode off, a metadata field value is not written", {
  # The regression guard for the save_md()/save_tr()/save_affiliation() gates.
  #
  # A NULL input cannot reach save_md() -- observeEvent() defaults to
  # ignoreNULL = TRUE -- so the guard is not about the controls disappearing.
  # What it does stop is a NON-NULL value arriving while the document is
  # read-only: these fields save through a 700ms debounce, so a value typed
  # just before the switch is flipped off would otherwise still land after it,
  # and an input driven directly (as here) would land at any time.
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)
    session$setInputs(md_header = "Acme Corp")
    session$elapse(1000)
    expect_equal(
      as.character(S7::prop(DTAtools::metadata(rv$dta), "header")), "Acme Corp"
    )

    leave_edit_mode(session)
    session$setInputs(md_header = "Overwritten while read-only")
    session$elapse(1000)

    expect_equal(
      as.character(S7::prop(DTAtools::metadata(rv$dta), "header")), "Acme Corp"
    )
  })
})

test_that("adding a dataset selects it and marks it as having no data", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)

    session$setInputs(add_ds_name = "demographics", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 1)

    expect_true("demographics" %in% names(DTAtools::datasets(rv$dta)))
    expect_equal(rv$active, "demographics")
    expect_equal(rv$status[["demographics"]], "nodata")
    # build_structure() must have re-run, or the new dataset has no nav entry
    # and no upload slots.
    expect_true("demographics" %in% names(rv$structure))
  })
})

test_that("several datasets can be added in a row, each of either type", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)

    session$setInputs(add_ds_name = "alpha", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 1)
    session$setInputs(add_ds_name = "beta", add_ds_type = "file")
    session$setInputs(add_ds_save = 2)
    session$setInputs(add_ds_name = "gamma", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 3)

    # Appended in order, after the dataset the fixture came with.
    expect_equal(
      names(DTAtools::datasets(rv$dta)),
      c("clinical_data", "alpha", "beta", "gamma")
    )
    expect_s3_class(DTAtools::datasets(rv$dta, "beta"), "DTAtools::DTADataSetFile")
    expect_s3_class(DTAtools::datasets(rv$dta, "gamma"), "DTAtools::DTADataSetTabular")
    # Every one of them is addressable through the nav structure.
    expect_equal(names(rv$structure), c("clinical_data", "alpha", "beta", "gamma"))
    expect_equal(rv$structure$gamma$index, 4)
  })
})

test_that("a duplicate name is refused and leaves the document alone", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)

    session$setInputs(add_ds_name = "clinical_data", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 1)

    expect_equal(names(DTAtools::datasets(rv$dta)), "clinical_data")
    expect_false(isTRUE(rv$add_ds_msg$ok))
    expect_match(rv$add_ds_msg$error, "already exists", fixed = TRUE)
  })
})

test_that("removing the active dataset moves the selection to a remaining one", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)
    session$setInputs(add_ds_name = "alpha", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 1)
    expect_equal(rv$active, "alpha")

    # Drive the real flow: opening the modal (remove_dataset) is what stashes
    # the name the confirm handler acts on -- see rv$removing_dataset.
    session$setInputs(remove_dataset = 1)
    session$setInputs(remove_dataset_confirm = 1)

    expect_equal(names(DTAtools::datasets(rv$dta)), "clinical_data")
    expect_equal(rv$active, "clinical_data")
    expect_false("alpha" %in% names(rv$status))
    expect_false("alpha" %in% names(rv$structure))
  })
})

test_that("removing the last dataset leaves an empty but usable workspace", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)

    # Drive the real flow: opening the modal (remove_dataset) is what stashes
    # the name the confirm handler acts on -- see rv$removing_dataset.
    session$setInputs(remove_dataset = 1)
    session$setInputs(remove_dataset_confirm = 1)

    expect_length(DTAtools::datasets(rv$dta), 0)
    expect_null(rv$active)
    # rv$structure must stay a (empty) list rather than becoming NULL:
    # output$main falls back to the landing page on NULL, which would throw
    # away the document's metadata.
    expect_false(is.null(rv$structure))
    expect_length(rv$structure, 0)
  })
})

test_that("removing a dataset unloads the files bound to it", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    expect_true(any(startsWith(names(rv$uploads), "clinical_data||")))

    # Drive the real flow: opening the modal (remove_dataset) is what stashes
    # the name the confirm handler acts on -- see rv$removing_dataset.
    session$setInputs(remove_dataset = 1)
    session$setInputs(remove_dataset_confirm = 1)

    # The upload records are keyed "<dataset>||<handlerIdx>"; left behind they
    # would still count towards validation and export with no way to reach them.
    expect_false(any(startsWith(names(rv$uploads) %||% character(0), "clinical_data||")))
  })
})

test_that("remove_dataset_confirm removes the dataset the modal named, not whatever is active later", {
  # THE BUG THIS PINS: the confirm handler used to read rv$active at click
  # time. The modal is easyClose = TRUE, so it can be left open while rv$active
  # changes underneath it (e.g. the user clicks a different dataset in the
  # nav) -- and the old code would then delete THAT dataset instead of the one
  # tags$b() named in the modal, irreversibly. The fix stashes the name into
  # rv$removing_dataset when the modal opens and the confirm handler uses that.
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)
    session$setInputs(add_ds_name = "alpha", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 1)
    expect_equal(rv$active, "alpha")

    # Open the modal for "alpha" -- it stashes the name it named.
    session$setInputs(remove_dataset = 1)
    expect_equal(rv$removing_dataset, "alpha")

    # rv$active drifts to a different dataset WHILE the modal is still open.
    rv$active <- "clinical_data"

    session$setInputs(remove_dataset_confirm = 1)

    # "alpha" -- the dataset the modal named -- is gone. "clinical_data"
    # survives untouched, even though it was active at confirm time.
    expect_equal(names(DTAtools::datasets(rv$dta)), "clinical_data")
    expect_true("clinical_data" %in% names(rv$structure))
  })
})

test_that("cancelling the remove-dataset modal clears the stash", {
  # A confirm click that arrives after Cancel (e.g. a delayed/duplicate
  # websocket message) must not fall back to deleting anything.
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)
    session$setInputs(add_ds_name = "alpha", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 1)

    session$setInputs(remove_dataset = 1)
    expect_equal(rv$removing_dataset, "alpha")

    session$setInputs(remove_dataset_cancel = 1)
    expect_null(rv$removing_dataset)

    session$setInputs(remove_dataset_confirm = 1)

    expect_equal(names(DTAtools::datasets(rv$dta)), c("clinical_data", "alpha"))
  })
})

test_that("the Metadata tab renders form controls only while edit mode is on", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)
    session$setInputs(md_token = 1)
    editable <- as.character(output$metadata_editor$html)

    # Every field the tab offers is a real control...
    expect_match(editable, "id=\"md_title\"", fixed = TRUE)
    expect_match(editable, "id=\"md_header\"", fixed = TRUE)
    expect_match(editable, "id=\"tr_test_upload\"", fixed = TRUE)
    expect_match(editable, "id=\"add_receiver\"", fixed = TRUE)
    expect_false(grepl("md-ro-field", editable, fixed = TRUE))

    leave_edit_mode(session)
    ro <- as.character(output$metadata_editor$html)

    # ...and read-only replaces them with static text, keeping the values.
    expect_false(grepl("id=\"md_title\"", ro, fixed = TRUE))
    expect_false(grepl("id=\"md_header\"", ro, fixed = TRUE))
    expect_false(grepl("id=\"tr_test_upload\"", ro, fixed = TRUE))
    # The "Add person" buttons and the autosave hint go too -- neither is true
    # of a document that cannot be edited.
    expect_false(grepl("id=\"add_receiver\"", ro, fixed = TRUE))
    expect_false(grepl("saved automatically", ro, fixed = TRUE))
    expect_match(ro, "md-ro-field", fixed = TRUE)
    # The value itself is still on screen, just not in an input.
    expect_match(ro, "Clinical Data Specification", fixed = TRUE)
  })
})

test_that("an editor opened while editing stays shut once edit mode is off", {
  # Regression guard. The file/column/rule/dataset-metadata save handlers
  # resolve their target from rv$editor_dataset, which was set when the editor
  # was opened and previously cleared only by a rename or a removal -- so once
  # any editor had been opened, its save handler stayed armed for the rest of
  # the session and fired straight through a read-only document.
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)
    session$setInputs(edit_cols = 1) # arms rv$editor_dataset
    before <- app_fn("dta_column_ids")(rv$dta, "clinical_data")

    leave_edit_mode(session)
    expect_null(rv$editor_dataset)

    session$setInputs(
      col_id = "SNEAKY", col_backend = "SAS", col_type = "Char", col_save = 1
    )

    expect_equal(app_fn("dta_column_ids")(rv$dta, "clinical_data"), before)
  })
})

test_that("with edit mode off, the contact controls cannot change the document", {
  # The contact observers are registered up front for every contact the moment
  # a DTA loads, independent of edit mode, so hiding their controls was never
  # enough on its own.
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    leave_edit_mode(session)
    load_fixture(session)
    before <- length(app_fn("dta_contacts")(rv$dta, "receiver"))
    expect_gt(before, 0)

    session$setInputs(rm_receiver_1 = 1, confirm_rm_receiver_1 = 1)
    expect_equal(length(app_fn("dta_contacts")(rv$dta, "receiver")), before)

    session$setInputs(new_contact_name = "Mallory", confirm_add_receiver = 1)
    expect_equal(length(app_fn("dta_contacts")(rv$dta, "receiver")), before)
  })
})

test_that("with edit mode off, the column and rule editors cannot change the document", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    leave_edit_mode(session)
    load_fixture(session)
    cols <- app_fn("dta_column_ids")(rv$dta, "clinical_data")
    n_rules <- nrow(app_fn("dta_rules_overview")(rv$dta, "clinical_data"))

    session$setInputs(col_del_click = 1, col_up_click = 1, col_down_click = 1)
    session$setInputs(rule_del_click = 1, rule_up_click = 1, rule_down_click = 1)

    expect_equal(app_fn("dta_column_ids")(rv$dta, "clinical_data"), cols)
    expect_equal(nrow(app_fn("dta_rules_overview")(rv$dta, "clinical_data")), n_rules)
  })
})

test_that("a rejected Add dataset keeps what the user typed", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)
    session$setInputs(add_dataset_open = 1)
    token_before <- rv$add_ds_token

    # A name that collides with the dataset the fixture already has.
    session$setInputs(add_ds_name = "clinical_data", add_ds_type = "file")
    session$setInputs(add_ds_save = 1)

    expect_match(rv$add_ds_msg$error, "already exists", fixed = TRUE)
    # rv$add_ds_token is the observable here. It is output$add_ds_body's ONLY
    # reactive dependency, and the body it renders hardcodes value = "" and
    # selected = "tabular" -- so bumping it is exactly what would wipe the
    # form in the browser. Leaving it alone means no re-render is pushed and
    # what the user typed stays on screen.
    #
    # The rendered HTML cannot show this: reading output$add_ds_body here
    # forces a fresh render regardless, which always emits the blank defaults.
    expect_equal(rv$add_ds_token, token_before)
    # ...and the document itself is untouched.
    expect_equal(names(DTAtools::datasets(rv$dta)), "clinical_data")
  })
})

test_that("the contact and affiliation sub-outputs follow the switch too", {
  # These render through render_contacts() / render_affiliation(), which read
  # editing() inside a renderUI that isolates rv$dta. If that dependency did not
  # propagate out through the wrapper, the party cards would keep their inputs
  # (and their Remove buttons) after the switch was turned off, while the rest
  # of the tab went read-only.
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)
    session$setInputs(md_token = 1, contacts_token = 1)

    aff_on <- as.character(output$receiver_affiliation$html)
    con_on <- as.character(output$receiver_contacts$html)
    expect_match(aff_on, "id=\"receiver_aff_name\"", fixed = TRUE)
    expect_match(con_on, "rm_receiver_1", fixed = TRUE)

    leave_edit_mode(session)
    aff_off <- as.character(output$receiver_affiliation$html)
    con_off <- as.character(output$receiver_contacts$html)

    expect_false(grepl("id=\"receiver_aff_name\"", aff_off, fixed = TRUE))
    expect_match(aff_off, "md-ro-field", fixed = TRUE)
    # No per-row edit link and no Remove button while read-only...
    expect_false(grepl("rm_receiver_1", con_off, fixed = TRUE))
    expect_false(grepl("editc_receiver_1", con_off, fixed = TRUE))
    # ...but the people are still listed, and with the FULL detail
    # contact_detail_block() adds: read-only has no click to reach the rest
    # of a contact's fields the way editable mode's actionLink() does, so
    # email/address (etc.) have to be on the page outright or they are simply
    # unreachable. Alice Smith is clinical_dta.yaml's first receiver contact.
    expect_match(con_off, "Alice Smith", fixed = TRUE)
    expect_match(con_off, "alice.smith@testcompany.com", fixed = TRUE)
    expect_match(con_off, "123 Main St, Cityville", fixed = TRUE)
  })
})

test_that("a dataset added in the app survives the YAML the app writes for it", {
  # sync_yaml_text() re-serializes after every mutation, and the Raw YAML tab
  # parses that text back. A new dataset starts with no columns, so this is the
  # end-to-end guard for the specs_from_list(NULL) fix.
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    unlock_editing(session)
    session$setInputs(add_ds_name = "alpha", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 1)

    round_tripped <- app_fn("dta_read_yaml_text")(rv$yaml_text)

    expect_true(round_tripped$ok)
    expect_equal(
      names(DTAtools::datasets(round_tripped$value)),
      c("clinical_data", "alpha")
    )
  })
})
