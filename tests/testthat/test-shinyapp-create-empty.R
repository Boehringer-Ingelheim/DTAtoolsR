# "Create new": starting a DTA from nothing.
#
# The landing page's third way in, alongside uploading a YAML and expanding a
# template. It builds an EMPTY DTA -- metadata only, zero datasets -- and hands
# it to apply_loaded() with start_editing = TRUE.
#
# Two properties carry the feature and are asserted below rather than assumed:
#
#   1. rv$structure must be an empty LIST, not NULL. output$main decides
#      landing-vs-workspace on is.null(rv$structure), so a NULL here would
#      bounce the user straight back to the landing page after creating a
#      document -- the feature would silently do nothing.
#   2. The empty document must be USABLE: a DTA with no datasets is only worth
#      creating if the first dataset can then be added to it. Note that this
#      needs no enter_edit_mode() call: start_editing = TRUE is the whole
#      point, so a test that had to unlock editing by hand would be asserting
#      the opposite of the intended behaviour.
#
# Editing state is server-owned here (rv$editing -- see the WHY on editing()
# in app.R), so apply_loaded(start_editing = TRUE) is directly observable
# under testServer() rather than being a client-side message the harness
# cannot see.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

app_server_dir <- function() .shiny_app_dir()

clean_session_file <- function() {
  f <- list.files(tempdir(),
    pattern = "^dtatools_app_session.*[.]rds$", full.names = TRUE
  )
  unlink(f, force = TRUE)
  invisible(f)
}

app_file_input <- function(filename) {
  path <- app_fixture_path(filename)
  data.frame(
    name = filename, size = file.size(path), type = "",
    datapath = path, stringsAsFactors = FALSE
  )
}

# Flatten a renderUI() result to text, so a rendered output can be asserted on
# rather than trusted. Deliberately this file's own copy, for the same reason
# clean_session_file() above is: a test file owns its helpers rather than
# depending on another test file's internals.
render_html <- function(tag) {
  paste(as.character(tag), collapse = "\n")
}

# Drive the create-new modal to completion.
create_new <- function(session, title = "Brand New DTA", version = "1.0") {
  session$setInputs(create_new = 1)
  # create_new_version_value, not create_new_version (the Edit menu's own action-link id) -- a change to either is caught here.
  session$setInputs(create_new_title = title, create_new_version_value = version)
  session$setInputs(create_new_confirm = 1)
}

# ---- The helper: dta_create_empty() -----------------------------------------

test_that("dta_create_empty() builds a DTA with zero datasets and the given metadata", {
  res <- app_fn("dta_create_empty")("My Transfer", "2.1")

  expect_true(res$ok)
  expect_s3_class(res$value, "DTAtools::DTA")
  expect_length(DTAtools::datasets(res$value), 0)
  expect_equal(DTAtools::metadata(res$value)@title, "My Transfer")
  expect_equal(DTAtools::metadata(res$value)@version, "2.1")

  # Without this seeded entry, the exported document would have no Version
  # History section at all, and a later version bump would become the FIRST
  # entry the history ever had -- silently losing the version the document
  # was actually created at.
  vh <- DTAtools::metadata(res$value)@version_history
  expect_length(vh, 1)
  expect_equal(vh[[1]]$version, "2.1")
  expect_true(nzchar(vh[[1]]$changes))
})

test_that("dta_create_empty() stamps the supplied date", {
  res <- app_fn("dta_create_empty")("Dated", "1.0", date = as.Date("2020-01-02"))

  expect_true(res$ok)
  expect_equal(DTAtools::metadata(res$value)@date, as.Date("2020-01-02"))

  # Same seed as above -- the entry's own date has to match what was
  # supplied, not today's date, or the history would misdate the version it
  # claims to record.
  vh <- DTAtools::metadata(res$value)@version_history
  expect_equal(vh[[1]]$date, as.Date("2020-01-02"))
})

test_that("dta_create_empty() reports an empty title as an error rather than throwing", {
  # DTAMetaData()'s validator rejects "" outright. dta_try() has to turn that
  # into a value the modal can display, not let it reach the user as a crash.
  res <- app_fn("dta_create_empty")("", "1.0")

  expect_false(res$ok)
  expect_match(res$error, "title", ignore.case = TRUE)
})

# ---- The empty document is constructible and serialisable -------------------

test_that("an empty DTA round-trips through the app's own YAML serialiser", {
  dta <- app_fn("dta_create_empty")("Round Trip", "1.0")$value

  txt <- app_fn("dta_to_yaml_text")(dta)
  expect_true(txt$ok)

  back <- app_fn("dta_read_yaml_text")(txt$value)
  expect_true(back$ok)
  expect_length(DTAtools::datasets(back$value), 0)
  expect_equal(DTAtools::metadata(back$value)@title, "Round Trip")
  expect_equal(DTAtools::metadata(back$value)@version, "1.0")

  # The seeded version_history entry is metadata like any other -- it has to
  # survive the same serialise/parse cycle, or the document would silently
  # lose its own creation entry on the very first save.
  vh <- DTAtools::metadata(back$value)@version_history
  expect_length(vh, 1)
  expect_equal(vh[[1]]$version, "1.0")
})

# ---- Server: validation ------------------------------------------------------

test_that("create_new_confirm with a blank title creates nothing and reports why", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    create_new(session, title = "   ", version = "1.0")

    expect_false(rv$create_new_msg$ok)
    expect_equal(rv$create_new_msg$error, "Enter a title.")
    expect_null(rv$dta)
    # Still on the landing page.
    expect_null(rv$structure)
  })
})

test_that("create_new_confirm with a blank version creates nothing and reports why", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    create_new(session, title = "Titled", version = "")

    expect_false(rv$create_new_msg$ok)
    expect_equal(rv$create_new_msg$error, "Enter a version.")
    expect_null(rv$dta)
    expect_null(rv$structure)
  })
})

# ---- Server: the happy path --------------------------------------------------

test_that("create_new_confirm loads an empty DTA into the workspace", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    before_token <- rv$doc_token

    create_new(session, title = "Brand New DTA", version = "0.9")

    expect_s3_class(rv$dta, "DTAtools::DTA")
    expect_length(DTAtools::datasets(rv$dta), 0)
    expect_equal(DTAtools::metadata(rv$dta)@title, "Brand New DTA")
    expect_equal(DTAtools::metadata(rv$dta)@version, "0.9")

    # The workspace-vs-landing decision. An empty list keeps the user in the
    # workspace; NULL would send them back to the landing page.
    expect_false(is.null(rv$structure))
    expect_length(rv$structure, 0)
    expect_null(rv$active)
    expect_length(rv$status, 0)

    # A new document is not gated behind the "Create new version" flow, and
    # it arrives ready to edit -- an empty document is useless read-only.
    expect_false(rv$version_locked)
    expect_null(rv$version_baseline_yaml)
    expect_true(rv$editing)

    expect_gt(rv$doc_token, before_token)
    expect_null(rv$create_new_msg)
  })
})

test_that("the created document's Raw YAML text is the document itself", {
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    create_new(session, title = "Raw View", version = "1.2")

    expect_true(nzchar(rv$yaml_text))
    back <- app_fn("dta_read_yaml_text")(rv$yaml_text)
    expect_true(back$ok)
    expect_equal(DTAtools::metadata(back$value)@title, "Raw View")
    expect_length(DTAtools::datasets(back$value), 0)
  })
})

# ---- Server: the empty document is usable -----------------------------------

test_that("a dataset can be added to a freshly created empty DTA", {
  # The point of the feature: an empty document you cannot then fill in would
  # be worthless. This is the end-to-end assertion that it is not.
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    create_new(session, title = "Fill Me In", version = "1.0")
    # Deliberately NO enter_edit_mode() call -- see the file header.
    expect_true(rv$editing)

    session$setInputs(add_ds_name = "demographics", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 1)

    expect_equal(names(DTAtools::datasets(rv$dta)), "demographics")
    expect_equal(names(rv$structure), "demographics")
    expect_equal(rv$active, "demographics")
  })
})

test_that("the workspace tells an author with no datasets what to do next", {
  # rv$active is NULL for a document with no datasets, so output$dataset_detail
  # used to fail its own req() and render NOTHING: the first screen of a
  # document the author had just created was a blank panel. The empty state is
  # user-facing text named in the changelog, so it is asserted by rendering the
  # output rather than by trusting the branch to stay there -- reverting the
  # fix makes the html empty and both matches below fail.
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    create_new(session, title = "Nothing Yet", version = "1.0")

    html <- render_html(output$dataset_detail$html)
    expect_match(html, "no datasets yet", fixed = TRUE)
    expect_match(html, "+ Add dataset", fixed = TRUE)

    # It is the empty state that is showing, not a dataset view that happens
    # to mention the phrase: adding a dataset replaces it with that dataset.
    session$setInputs(add_ds_name = "demographics", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 1)
    expect_false(grepl("no datasets yet", render_html(output$dataset_detail$html), fixed = TRUE))
  })
})

# ---- Server: the landing-page guard -----------------------------------------

test_that("create_new_confirm cannot replace a document that is already loaded", {
  # The button only exists on the landing page, but its input id outlives that
  # DOM -- a delayed or duplicated websocket message must not silently discard
  # a loaded document. Mirrors req(rv$version_locked) on new_version_confirm.
  # Two separate guards stand in the way here, and both are pinned below
  # rather than just the one that happens to leave a visible trace: the modal
  # itself never opens (req(is.null(rv$structure)) on input$create_new, the
  # only place rv$create_new_token is incremented), and even if a stray
  # create_new_confirm message arrived without it, req(is.null(rv$structure))
  # on input$create_new_confirm would refuse it too. The rv$dta assertions
  # below only exercise the second guard; without the token check, deleting
  # the first guard would not fail this test.
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    before <- names(DTAtools::datasets(rv$dta))
    expect_gt(length(before), 0)

    create_new(session, title = "Should Not Land", version = "9.9")

    expect_equal(names(DTAtools::datasets(rv$dta)), before)
    expect_false(identical(DTAtools::metadata(rv$dta)@title, "Should Not Land"))
    expect_equal(rv$create_new_token, 0)
  })
})

# ---- Server: restoring after Create new -------------------------------------

test_that("edit mode from a freshly created document survives a reload", {
  # The direct regression guard, mirroring "edit mode from a template-created
  # document survives a reload" in test-shinyapp-template-ui.R: apply_loaded()
  # has to write rv$editing BEFORE it autosaves, or the snapshot on disk would
  # still say editing = FALSE, and restoring right after "Create new" would
  # strand the author read-only in a workspace with no "+ Add dataset"
  # control -- the empty document they had just created would then be
  # unusable without a trip through the Edit menu first.
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = strrep("f", 32))
    create_new(session, title = "Survives A Reload", version = "1.0")
    expect_true(rv$editing)

    session$setInputs(restore_session = 1)

    expect_true(rv$editing)

    # NULL, not a length-one NA: character(0)[1] is NA_character_, and this
    # app's %||% only substitutes for NULL or a genuinely zero-length value,
    # so for a document with no datasets that NA would sail straight through
    # restore_session() instead of becoming the NULL every other rv$active
    # assignment uses. expect_null() alone catches that regression -- is.null()
    # is FALSE for any NA, so a length-one NA fails this the same as a
    # dedicated is.na() check would.
    expect_null(rv$active)

    # NULL is the legitimate value here, not a gap restore_session() left
    # unfilled: this document was CREATED in this session rather than loaded,
    # so no baseline was ever captured, and the first "Create new version"
    # for it relies on that NULL to re-baseline at the moment of the bump. A
    # restore that invented a baseline would fold every edit made before that
    # bump into the new version's change summary.
    expect_null(rv$version_baseline_yaml)

    # The document itself survived the round trip.
    expect_length(DTAtools::datasets(rv$dta), 0)
    expect_equal(DTAtools::metadata(rv$dta)@title, "Survives A Reload")
  })
})

test_that("a session file that already carries an NA active dataset restores as no selection", {
  # Computing the fallback correctly is only half of it: a session file
  # written by the build that HAD this bug already holds the NA, and %||%
  # does not substitute a length-one NA, so such a file would keep leaking
  # "NA" into tooltips and download filenames however carefully the fallback
  # is derived. The file is rewritten here to look exactly like one of those.
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = strrep("e", 32))
    create_new(session, title = "Stale Snapshot", version = "1.0")

    f <- list.files(tempdir(),
      pattern = "^dtatools_app_session.*[.]rds$", full.names = TRUE
    )
    expect_length(f, 1)
    saved <- readRDS(f[[1]])
    saved$active <- NA_character_
    saveRDS(saved, f[[1]])

    session$setInputs(restore_session = 1)

    expect_null(rv$active)
    expect_equal(DTAtools::metadata(rv$dta)@title, "Stale Snapshot")
  })
})
