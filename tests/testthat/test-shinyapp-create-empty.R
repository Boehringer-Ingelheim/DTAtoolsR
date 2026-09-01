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

# Drive the create-new modal to completion.
create_new <- function(session, title = "Brand New DTA", version = "1.0") {
  session$setInputs(create_new = 1)
  session$setInputs(create_new_title = title, create_new_version = version)
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
})

test_that("dta_create_empty() stamps the supplied date", {
  res <- app_fn("dta_create_empty")("Dated", "1.0", date = as.Date("2020-01-02"))

  expect_true(res$ok)
  expect_equal(DTAtools::metadata(res$value)@date, as.Date("2020-01-02"))
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

# ---- Server: the landing-page guard -----------------------------------------

test_that("create_new_confirm cannot replace a document that is already loaded", {
  # The button only exists on the landing page, but its input id outlives that
  # DOM -- a delayed or duplicated websocket message must not silently discard
  # a loaded document. Mirrors req(rv$version_locked) on new_version_confirm.
  clean_session_file()
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    before <- names(DTAtools::datasets(rv$dta))
    expect_gt(length(before), 0)

    create_new(session, title = "Should Not Land", version = "9.9")

    expect_equal(names(DTAtools::datasets(rv$dta)), before)
    expect_false(identical(DTAtools::metadata(rv$dta)@title, "Should Not Land"))
  })
})
