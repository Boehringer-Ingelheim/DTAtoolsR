# Document versioning: the read-only lock on a LOADED document, and the
# "Create new version" flow that is the only way to unlock it.
#
# inst/shiny/dta_app/R/versioning.R holds every pure helper this feature is
# built from (see the file header there for the full list). app.R wires them
# into editing() -- see the WHY comment on that reactive for why a loaded
# document opens read-only and stays that way until a new version is
# created, and why the lock is enforced there rather than by simply not
# rendering the edit_mode switch.

render_html <- function(tag) {
  paste(as.character(tag), collapse = "\n")
}

# ---- Pure: dta_next_version() -----------------------------------------------

test_that("dta_next_version() suggests a MINOR bump for a dotted-integer scheme, and \"\" otherwise", {
  next_version <- app_fn("dta_next_version")

  expect_equal(next_version("1.0"), "1.1")
  expect_equal(next_version("2.3.4"), "2.3.5")
  # A lone numeric component gains a minor part rather than a major bump.
  expect_equal(next_version("1"), "1.1")
  # An INTEGER bump on the last component, not a string one -- "9" -> 10,
  # not "9" -> "10" via string concatenation tricks that would misbehave past
  # single digits.
  expect_equal(next_version("1.9"), "1.10")
  # A scheme this cannot parse gets "" rather than a guess -- see the WHY
  # comment on dta_next_version() for why silently guessing wrong is worse
  # than leaving the field for the author to fill in.
  expect_equal(next_version("Draft A"), "")
  expect_equal(next_version("v1.0"), "")
  expect_equal(next_version(NULL), "")
  expect_equal(next_version(""), "")
})

# ---- Pure: dta_version_change_summary() -------------------------------------
#
# Every test below hand-builds a diff shaped like dta_diff()'s own return
# value: list(metadata = <frame>, datasets = <frame>) with columns
# key/change/from/to -- the same contract dta_version_change_line() and
# dta_version_is_ignored_key() are documented against.

.empty_change_frame <- function() {
  data.frame(
    key = character(0), change = character(0),
    from = character(0), to = character(0),
    stringsAsFactors = FALSE
  )
}

test_that("dta_version_change_summary()'s counts line names both sections and their per-type counts", {
  summary_fn <- app_fn("dta_version_change_summary")

  diff <- list(
    metadata = data.frame(
      key = c("title", "header", "new_field"),
      change = c("changed", "changed", "added"),
      from = c("Old Title", "Old Header", NA),
      to = c("New Title", "New Header", "value"),
      stringsAsFactors = FALSE
    ),
    datasets = data.frame(
      key = "clinical_data.description", change = "removed",
      from = "desc", to = NA,
      stringsAsFactors = FALSE
    )
  )

  result <- summary_fn(diff)

  expect_match(result, "Metadata: 2 changed, 1 added.", fixed = TRUE)
  expect_match(result, "Datasets: 1 removed.", fixed = TRUE)
})

test_that("dta_version_change_summary() excludes version and version_history rows entirely", {
  # These are the machinery's OWN writes -- restating "changed version 1.0 ->
  # 1.1" would be restating the header of the very entry this summary is
  # written into. See the WHY comment on dta_version_is_ignored_key().
  summary_fn <- app_fn("dta_version_change_summary")

  diff <- list(
    metadata = data.frame(
      key = c(
        "version", "version_history.1.changes",
        "version_history.1.version", "title"
      ),
      change = c("changed", "added", "added", "changed"),
      from = c("1.0", NA, NA, "Old Title"),
      to = c("1.1", "a fabricated note", "1.1", "New Title"),
      stringsAsFactors = FALSE
    ),
    datasets = .empty_change_frame()
  )

  result <- summary_fn(diff)

  # Only the real content change (title) is visible ...
  expect_match(result, "metadata.title", fixed = TRUE)
  expect_match(result, "Metadata: 1 changed.", fixed = TRUE)
  # ... the version bump and the fabricated history entry are not, even
  # though both were part of the diff handed in.
  expect_no_match(result, "a fabricated note", fixed = TRUE)
  expect_no_match(result, "version_history", fixed = TRUE)
  expect_no_match(result, "1.0", fixed = TRUE)
  expect_no_match(result, "1.1", fixed = TRUE)
})

test_that("dta_version_change_summary() prefixes metadata keys but leaves dataset keys bare", {
  summary_fn <- app_fn("dta_version_change_summary")

  diff <- list(
    metadata = data.frame(
      key = "title", change = "changed", from = "Old", to = "New",
      stringsAsFactors = FALSE
    ),
    datasets = data.frame(
      key = "clinical_data.columns.AGE.type", change = "changed",
      from = "SAS Num", to = "SAS Char",
      stringsAsFactors = FALSE
    )
  )

  result <- summary_fn(diff)

  expect_match(result, "metadata.title", fixed = TRUE)
  # A dataset key already carries its own dataset-name prefix; doubling it
  # with "metadata." would misdescribe where the change actually is.
  expect_match(result, "clinical_data.columns.AGE.type", fixed = TRUE)
  expect_no_match(result, "metadata.clinical_data.columns.AGE.type", fixed = TRUE)
})

test_that("dta_version_change_summary() strips newlines and pipes from every value", {
  # R/exportDocuments.R writes `changes` straight into a `|`-delimited
  # Markdown table row (see test-exportDocuments.R for the end-to-end guard);
  # a raw newline or `|` reaching that table would break its structure.
  summary_fn <- app_fn("dta_version_change_summary")

  diff <- list(
    metadata = data.frame(
      key = c("description", "notes"),
      change = c("changed", "changed"),
      from = c("Line one\nLine two", "A | B"),
      to = c("Single line now", "A and B"),
      stringsAsFactors = FALSE
    ),
    datasets = .empty_change_frame()
  )

  result <- summary_fn(diff)

  expect_false(grepl("\n", result, fixed = TRUE))
  expect_false(grepl("|", result, fixed = TRUE))
  # The content survives -- flattened, not dropped.
  expect_match(result, "Line one Line two", fixed = TRUE)
  expect_match(result, "A B", fixed = TRUE)
})

test_that("dta_version_change_summary() truncates at max_items and reports the omitted count", {
  summary_fn <- app_fn("dta_version_change_summary")

  make_diff <- function(n) {
    list(
      metadata = data.frame(
        key = paste0("field_", seq_len(n)), change = "changed",
        from = "old", to = "new",
        stringsAsFactors = FALSE
      ),
      datasets = .empty_change_frame()
    )
  }

  one_omitted <- summary_fn(make_diff(2), max_items = 1)
  expect_match(one_omitted, "1 further change not listed", fixed = TRUE)

  three_omitted <- summary_fn(make_diff(4), max_items = 1)
  expect_match(three_omitted, "3 further changes not listed", fixed = TRUE)
})

test_that("dta_version_change_summary() never returns an empty string, even for an empty diff", {
  # DTAMetaData's validator rejects an empty `changes` outright
  # (R/DTAMetaData-class.R ~line 139) -- a blank summary here would make the
  # very entry it is written into invalid the moment it is created.
  summary_fn <- app_fn("dta_version_change_summary")

  truly_empty <- list(metadata = .empty_change_frame(), datasets = .empty_change_frame())
  expect_equal(summary_fn(truly_empty), "No changes recorded.")

  # Real rows exist, but every one of them is a version/version_history key
  # -- filtering must not silently produce a blank string either.
  filtered_only <- list(
    metadata = data.frame(
      key = "version", change = "changed", from = "1.0", to = "1.1",
      stringsAsFactors = FALSE
    ),
    datasets = .empty_change_frame()
  )
  expect_equal(summary_fn(filtered_only), "No changes recorded.")
})

test_that("dta_version_change_summary() puts a note first, separated from the body", {
  summary_fn <- app_fn("dta_version_change_summary")
  diff <- list(
    metadata = data.frame(
      key = "title", change = "changed", from = "Old", to = "New",
      stringsAsFactors = FALSE
    ),
    datasets = .empty_change_frame()
  )

  result <- summary_fn(diff, note = "Quarterly refresh")

  expect_true(startsWith(result, "Quarterly refresh - "))
})

# ---- Pure: dta_append_version_entry() / dta_set_version_entry_changes() ----

test_that("dta_append_version_entry() sets the version, appends one entry carrying the placeholder, and refuses a blank version", {
  append_entry <- app_fn("dta_append_version_entry")
  placeholder <- app_fn("dta_version_placeholder")

  dta <- app_fixture_dta()
  n_before <- length(S7::prop(DTAtools::metadata(dta), "version_history"))

  res <- append_entry(dta, "3.0", as.Date("2026-01-01"))
  expect_true(res$ok)

  md <- DTAtools::metadata(res$value)
  expect_equal(as.character(S7::prop(md, "version")), "3.0")
  vh <- S7::prop(md, "version_history")
  expect_equal(length(vh), n_before + 1)
  expect_equal(vh[[length(vh)]]$changes, placeholder())
  expect_equal(vh[[length(vh)]]$version, "3.0")

  blank <- append_entry(dta, "   ")
  expect_false(blank$ok)
})

test_that("dta_set_version_entry_changes() rewrites one entry, re-syncs its version, and is a genuine no-op for NULL or out-of-range", {
  append_entry <- app_fn("dta_append_version_entry")
  set_changes <- app_fn("dta_set_version_entry_changes")

  dta <- app_fixture_dta()
  dta <- append_entry(dta, "3.0")$value

  res <- set_changes(dta, 1, "Real summary here", version = "3.0.1")
  expect_true(res$ok)
  vh <- S7::prop(DTAtools::metadata(res$value), "version_history")
  expect_equal(vh[[1]]$changes, "Real summary here")
  expect_equal(vh[[1]]$version, "3.0.1")

  # NULL index: a genuine no-op, byte for byte -- not just "no error".
  noop_null <- set_changes(dta, NULL, "Should not land")
  expect_identical(noop_null$value, dta)

  # Out-of-range index: also a no-op.
  noop_oob <- set_changes(dta, 99, "Should not land")
  expect_identical(noop_oob$value, dta)
})

# ---- Pure UI: new_version_modal_body() / edit_mode_switch() ----------------

test_that("new_version_modal_body() prefills the suggestion and handles a missing current version", {
  modal_body <- app_fn("new_version_modal_body")

  html <- render_html(modal_body("1.0", "1.1"))
  expect_match(html, 'id="new_version_value"', fixed = TRUE)
  expect_match(html, 'value="1.1"', fixed = TRUE)
  expect_match(html, "Current version: 1.0", fixed = TRUE)

  # An unset S7 property can read back as NULL, character(0) or NA -- all
  # three must render the same "no version yet" text without erroring.
  for (missing in list(NULL, character(0), NA_character_)) {
    html_missing <- expect_no_error(render_html(modal_body(missing, "")))
    expect_match(html_missing, "This document has no version yet.", fixed = TRUE)
  }
})

test_that("edit_mode_switch()'s checked attribute follows `value`", {
  # test-shinyapp-edit-mode.R already covers value = FALSE (the absence of
  # "checked" is the signal that the switch starts off); this is the
  # positive control, exercised here because it is what the "Create new
  # version" flow relies on to land the author straight in edit mode -- see
  # the WHY comment on edit_mode_switch()'s `value` argument.
  switch_fn <- app_fn("edit_mode_switch")

  expect_false(grepl("checked", render_html(switch_fn()), fixed = TRUE))
  expect_match(render_html(switch_fn(TRUE)), "checked", fixed = TRUE)
})

# ---- A generated summary survives a YAML round trip ------------------------

test_that("a generated version summary survives a YAML round trip byte for byte", {
  # No running server needed: this only exercises app_fn() helpers.
  summary_fn <- app_fn("dta_version_change_summary")
  append_entry <- app_fn("dta_append_version_entry")
  to_yaml <- app_fn("dta_to_yaml_text")
  read_yaml_text <- app_fn("dta_read_yaml_text")

  diff <- list(
    metadata = data.frame(
      key = "title", change = "changed",
      from = "Old 'Title'", to = "New Title -> Renamed",
      stringsAsFactors = FALSE
    ),
    datasets = .empty_change_frame()
  )
  summary <- summary_fn(diff, note = "Revision; second clause")

  dta <- app_fixture_dta()
  res <- append_entry(dta, "2.0", Sys.Date(), summary)
  expect_true(res$ok)

  ser <- to_yaml(res$value)
  expect_true(ser$ok)

  loaded <- read_yaml_text(ser$value)
  expect_true(loaded$ok)

  round_tripped <- S7::prop(DTAtools::metadata(loaded$value), "version_history")[[1]]
  expect_equal(round_tripped$changes, summary)
})

# ---- Server-side: the version lock and the "Create new version" flow ------

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

load_fixture <- function(session) {
  session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
}

test_that("loading the fixture leaves the document locked, and edit_mode = TRUE alone does not unlock it", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    session$setInputs(edit_mode = TRUE)
    load_fixture(session)

    expect_true(rv$version_locked)

    # save_md() debounces 700ms -- see the identical guard in
    # test-shinyapp-edit-mode.R for why elapse(1000) is needed to observe it.
    session$setInputs(md_header = "Should Not Land")
    session$elapse(1000)

    expect_false(identical(
      as.character(S7::prop(DTAtools::metadata(rv$dta), "header")), "Should Not Land"
    ))
  })
})

test_that("output$edit_gate renders the Create-new-version button while locked, and the switch once unlocked", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    session$setInputs(edit_mode = TRUE)
    load_fixture(session)

    locked_html <- render_html(output$edit_gate$html)
    expect_match(locked_html, 'id="create_new_version"', fixed = TRUE)
    expect_no_match(locked_html, 'id="edit_mode"', fixed = TRUE)

    unlock_editing(session)

    unlocked_html <- render_html(output$edit_gate$html)
    expect_match(unlocked_html, 'id="edit_mode"', fixed = TRUE)
    expect_no_match(unlocked_html, 'id="create_new_version"', fixed = TRUE)
  })
})

test_that("creating a version bumps metadata@version, appends one history entry, sets the entry index, and clears the lock", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    session$setInputs(edit_mode = TRUE)
    load_fixture(session)
    n_before <- length(S7::prop(DTAtools::metadata(rv$dta), "version_history"))

    unlock_editing(session, version = "9.9")

    expect_equal(as.character(S7::prop(DTAtools::metadata(rv$dta), "version")), "9.9")
    expect_equal(
      length(S7::prop(DTAtools::metadata(rv$dta), "version_history")),
      n_before + 1
    )
    expect_equal(rv$version_entry_index, n_before + 1)
    expect_false(rv$version_locked)
  })
})

test_that("after unlocking, a metadata edit lands (the positive control)", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    session$setInputs(edit_mode = TRUE)
    load_fixture(session)
    unlock_editing(session)

    session$setInputs(md_header = "Acme Corp Ltd")
    session$elapse(1000)

    expect_equal(
      as.character(S7::prop(DTAtools::metadata(rv$dta), "header")), "Acme Corp Ltd"
    )
  })
})

test_that("a blank version is refused, leaving the document untouched and still locked", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    session$setInputs(edit_mode = TRUE)
    load_fixture(session)
    before <- rv$dta

    session$setInputs(create_new_version = 1)
    session$setInputs(new_version_value = "   ")
    session$setInputs(new_version_confirm = 1)

    expect_identical(rv$dta, before)
    expect_true(rv$version_locked)
    expect_false(isTRUE(rv$new_version_msg$ok))
    expect_match(rv$new_version_msg$error, "Enter a version", fixed = TRUE)
  })
})

test_that("a version equal to the current one is refused, leaving the document untouched and still locked", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    session$setInputs(edit_mode = TRUE)
    load_fixture(session)
    current <- as.character(S7::prop(DTAtools::metadata(rv$dta), "version"))
    before <- rv$dta

    session$setInputs(create_new_version = 1)
    session$setInputs(new_version_value = current)
    session$setInputs(new_version_confirm = 1)

    expect_identical(rv$dta, before)
    expect_true(rv$version_locked)
    expect_match(rv$new_version_msg$error, "already the current version", fixed = TRUE)
  })
})

test_that("loading a second document re-arms the lock, even though input$edit_mode is still TRUE from the first", {
  # Shiny does not clear an input's value when its control leaves the DOM:
  # the edit_mode switch is swapped out for the Create-new-version button
  # while locked (output$edit_gate), so a stale/duplicate input$edit_mode =
  # TRUE message can still land on a session whose switch has not actually
  # been on screen since the last unlock. apply_loaded() also defensively
  # resets the switch on every load, but the lock has to hold on its OWN, in
  # editing() itself, even if that reset were ever bypassed or raced -- this
  # is what actually pins that (see the WHY comment on editing() in app.R).
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    session$setInputs(edit_mode = TRUE)
    load_fixture(session)
    unlock_editing(session)
    expect_false(rv$version_locked)

    # A second document loads -- e.g. the example loader.
    load_fixture(session)
    expect_true(rv$version_locked)

    # Simulate the exact race the WHY comment describes.
    session$setInputs(edit_mode = TRUE)
    expect_false(editing())
  })
})

test_that("a document loaded through apply_loaded() at its default `versioned` is not locked", {
  # `versioned` defaults to FALSE, which is how the template-creation flow
  # calls apply_loaded() -- a document that is NEW rather than loaded from an
  # existing one is not gated behind "Create new version" (see the WHY
  # comment on editing() in app.R). App helpers like dta_read_yaml() are NOT
  # in scope inside testServer()'s expression environment, so the document is
  # built via the exported reader instead.
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    dta <- DTAtools::read_dta_from_yaml(app_fixture_path("clinical_dta.yaml"))
    apply_loaded(dta, "metadata:\n  title: Test\n")

    expect_false(isTRUE(rv$version_locked))
  })
})

test_that("applying raw YAML that names a different version and version_history leaves both as they were, while other content lands", {
  # The version record is owned by the new-version flow, not by whatever text
  # happens to be pasted -- see the WHY comment on this in app.R's
  # input$apply_yaml handler.
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    session$setInputs(edit_mode = TRUE)
    load_fixture(session)
    unlock_editing(session)
    live_version <- as.character(S7::prop(DTAtools::metadata(rv$dta), "version"))
    live_history <- S7::prop(DTAtools::metadata(rv$dta), "version_history")

    parsed <- yaml::yaml.load(rv$yaml_text)
    parsed$metadata$version <- "77.0"
    parsed$metadata$version_history <- list(
      list(version = "1.0", date = "2020-01-01", changes = "fabricated")
    )
    parsed$metadata$title <- "Retitled via Raw YAML"
    pasted <- yaml::as.yaml(parsed)

    session$setInputs(raw_yaml_editor = pasted, apply_yaml = 1)

    expect_true(isTRUE(rv$yaml_msg$ok))
    expect_equal(as.character(S7::prop(DTAtools::metadata(rv$dta), "version")), live_version)
    expect_equal(S7::prop(DTAtools::metadata(rv$dta), "version_history"), live_history)
    expect_equal(
      as.character(S7::prop(DTAtools::metadata(rv$dta), "title"))[1],
      "Retitled via Raw YAML"
    )
  })
})

test_that("export_dta() after unlocking and editing md_title names the change with both values, and rv$dta keeps the placeholder", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    session$setInputs(edit_mode = TRUE)
    load_fixture(session)
    old_title <- as.character(S7::prop(DTAtools::metadata(rv$dta), "title"))[1]
    unlock_editing(session)

    session$setInputs(md_title = "A Brand New Title")
    session$elapse(1000)

    exported <- export_dta()
    idx <- rv$version_entry_index
    entry <- S7::prop(DTAtools::metadata(exported), "version_history")[[idx]]

    expect_match(entry$changes, "metadata.title", fixed = TRUE)
    expect_match(entry$changes, old_title, fixed = TRUE)
    expect_match(entry$changes, "A Brand New Title", fixed = TRUE)

    # A pure read: rv$dta's own entry still carries the placeholder.
    live_entry <- S7::prop(DTAtools::metadata(rv$dta), "version_history")[[idx]]
    expect_equal(live_entry$changes, app_fn("dta_version_placeholder")())
  })
})

test_that("export_dta() with no version opened returns the document unchanged", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    session$setInputs(edit_mode = TRUE)
    load_fixture(session)
    # Still locked -- rv$version_entry_index is NULL, exactly the "no version
    # opened" state, per apply_loaded().

    expect_identical(export_dta(), rv$dta)
  })
})

test_that("binding a data file adds nothing to the export summary -- the diff is specification-only", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    session$setInputs(edit_mode = TRUE)
    load_fixture(session)
    unlock_editing(session)

    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))

    exported <- export_dta()
    idx <- rv$version_entry_index
    entry <- S7::prop(DTAtools::metadata(exported), "version_history")[[idx]]

    # dta_dataset_to_list() (the unit dta_diff() compares) never includes
    # bound file_paths/tables, only the declared specification -- so binding
    # data must leave the diff empty.
    expect_equal(entry$changes, "No changes recorded.")
  })
})
