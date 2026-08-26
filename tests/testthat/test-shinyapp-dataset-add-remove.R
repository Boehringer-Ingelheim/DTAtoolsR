# Adding and removing whole datasets in the Shiny app (the sidebar's
# "+ Add dataset" button and the Edit menu's "Remove dataset" item, both of
# which appear only while Edit mode is on). This is a different mutation from
# dta_set_dataset_meta() (test-shinyapp-dataset-meta.R), which edits an
# EXISTING dataset's fields in place -- these two helpers add or remove an
# entire entry from `dta@datasets`.
#
# `dta_add_dataset()` always APPENDS: every nav button, upload slot and
# example picker in the app is keyed by the dataset's POSITION in
# `dta@datasets`, resolved to a name only at click time, so appending is the
# only growth of the list that cannot silently repoint an existing control at
# the wrong dataset. `dta_remove_dataset()` is allowed to take the DTA down to
# zero datasets -- the workspace UI does not require at least one to render.
#
# Both return dta_try(); neither throws, so every failure here is asserted via
# `r$ok`/`r$error`, never via expect_error().

# ---- Adding a dataset -------------------------------------------------------

test_that("dta_add_dataset creates a tabular dataset with zero columns", {
  add <- app_fn("dta_add_dataset")

  r <- add(app_fixture_dta(), "new_ds", type = "tabular")

  expect_true(r$ok)
  ds <- DTAtools::datasets(r$value, "new_ds")
  expect_true(inherits(ds, "DTAtools::DTADataSetTabular"))
  expect_equal(ds@type, "tabular")
  expect_equal(length(ds@specs@columns), 0)
})

test_that("dta_add_dataset creates a file dataset", {
  add <- app_fn("dta_add_dataset")

  r <- add(app_fixture_dta(), "new_ds", type = "file")

  expect_true(r$ok)
  ds <- DTAtools::datasets(r$value, "new_ds")
  expect_true(inherits(ds, "DTAtools::DTADataSetFile"))
  expect_equal(ds@type, "file")
})

test_that("the default type is tabular when type is not supplied", {
  add <- app_fn("dta_add_dataset")

  r <- add(app_fixture_dta(), "new_ds")

  expect_true(r$ok)
  expect_true(inherits(
    DTAtools::datasets(r$value, "new_ds"),
    "DTAtools::DTADataSetTabular"
  ))
})

test_that("an unrecognised type is rejected, changing nothing", {
  add <- app_fn("dta_add_dataset")
  names_fn <- app_fn("dta_dataset_names")

  dta <- app_fixture_dta()
  r <- add(dta, "new_ds", type = "spreadsheet")

  expect_false(r$ok)
  expect_match(r$error, "must be one of")
  expect_equal(names_fn(dta), "clinical_data")
})

test_that("a duplicate name is rejected, changing nothing", {
  add <- app_fn("dta_add_dataset")
  names_fn <- app_fn("dta_dataset_names")

  dta <- app_fixture_dta()
  r <- add(dta, "clinical_data")

  expect_false(r$ok)
  expect_match(r$error, "already exists")
  # The DTA handed in is untouched: the collision was caught before any write.
  expect_equal(names_fn(dta), "clinical_data")
})

test_that("an empty name is rejected with a message rather than an S7 abort", {
  add <- app_fn("dta_add_dataset")

  for (bad in list("", "   ", NULL)) {
    r <- add(app_fixture_dta(), bad)
    expect_false(r$ok)
    expect_match(r$error, "name is required")
  }
})

test_that("a name is trimmed before it is stored", {
  add <- app_fn("dta_add_dataset")
  names_fn <- app_fn("dta_dataset_names")

  r <- add(app_fixture_dta(), "  new_ds  ")

  expect_true(r$ok)
  expect_true("new_ds" %in% names_fn(r$value))
  expect_false("  new_ds  " %in% names_fn(r$value))
})

test_that("a description is stored, and a blank one is unset rather than empty", {
  add <- app_fn("dta_add_dataset")

  with_desc <- add(app_fixture_dta(), "new_ds", description = "A new dataset")
  expect_true(with_desc$ok)
  expect_equal(
    DTAtools::datasets(with_desc$value, "new_ds")@description,
    "A new dataset"
  )

  blank_desc <- add(app_fixture_dta(), "new_ds", description = "   ")
  expect_true(blank_desc$ok)
  expect_null(DTAtools::datasets(blank_desc$value, "new_ds")@description)
})

test_that("a new dataset is appended last, leaving existing positions unchanged", {
  add <- app_fn("dta_add_dataset")
  names_fn <- app_fn("dta_dataset_names")

  dta <- DTAtools::create_example_DTA()
  expect_equal(names_fn(dta), c("demographics", "vitals"))

  r <- add(dta, "new_ds", type = "file")

  expect_true(r$ok)
  expect_equal(names_fn(r$value), c("demographics", "vitals", "new_ds"))
})

# ---- Round-trip regression guard --------------------------------------------

test_that("an added dataset of each type survives a YAML round trip", {
  # THE regression this guards: dta_add_dataset() builds an EMPTY
  # DTAColumnSpecCollection (columns = list()) for a tabular dataset, and
  # .dta_compact() then drops the resulting empty `columns:` key from the
  # serialized YAML entirely (see dta_to_yaml_text() above). Reading that
  # document back has to reconstruct a dataset with zero columns rather than
  # erroring or dropping the dataset -- this is a real bug another agent is
  # fixing in R/DTAColumnSpecCollection-class.R, not something to work around
  # here.
  add <- app_fn("dta_add_dataset")
  to_yaml <- app_fn("dta_to_yaml_text")
  read_yaml_text <- app_fn("dta_read_yaml_text")

  dta <- app_fixture_dta()
  dta <- add(dta, "new_tabular", type = "tabular")$value
  dta <- add(dta, "new_file", type = "file")$value

  ser <- to_yaml(dta)
  expect_true(ser$ok)

  loaded <- read_yaml_text(ser$value)
  expect_true(loaded$ok)

  tab <- DTAtools::datasets(loaded$value, "new_tabular")
  fil <- DTAtools::datasets(loaded$value, "new_file")
  expect_true(inherits(tab, "DTAtools::DTADataSetTabular"))
  expect_equal(tab@type, "tabular")
  expect_true(inherits(fil, "DTAtools::DTADataSetFile"))
  expect_equal(fil@type, "file")
})

# ---- Removing a dataset ------------------------------------------------------

test_that("dta_remove_dataset drops one dataset, keeping the others' order", {
  add <- app_fn("dta_add_dataset")
  remove <- app_fn("dta_remove_dataset")
  names_fn <- app_fn("dta_dataset_names")

  dta <- DTAtools::create_example_DTA()
  dta <- add(dta, "extra", type = "file")$value
  expect_equal(names_fn(dta), c("demographics", "vitals", "extra"))

  r <- remove(dta, "vitals")

  expect_true(r$ok)
  expect_equal(names_fn(r$value), c("demographics", "extra"))
})

test_that("removing a non-existent dataset fails with a message, changing nothing", {
  remove <- app_fn("dta_remove_dataset")
  names_fn <- app_fn("dta_dataset_names")

  dta <- app_fixture_dta()
  r <- remove(dta, "no_such_dataset")

  expect_false(r$ok)
  expect_match(r$error, "not found")
  expect_equal(names_fn(dta), "clinical_data")
})

test_that("removing the only dataset leaves a valid DTA with zero datasets", {
  remove <- app_fn("dta_remove_dataset")
  names_fn <- app_fn("dta_dataset_names")

  r <- remove(app_fixture_dta(), "clinical_data")

  expect_true(r$ok)
  expect_true(inherits(r$value, "DTAtools::DTA"))
  expect_equal(names_fn(r$value), character(0))
})

# ---- the sidebar survives a dataset removal (server journey) ----------------
#
# THE REPORTED BUG: with two datasets, removing one made the sidebar's overview
# and Datasets list disappear. The removal itself was sound -- what vanished was
# the RENDER: every rv$structure assignment re-renders output$main, replacing
# the whole workspace DOM, and when the client re-binds the sidebar's uiOutputs
# it can misreport them as hidden (a visibility snapshot racing the DOM swap).
# Under the default suspendWhenHidden the server then never sends their HTML,
# and nothing in the sidebar ever re-triggers the visibility scan the way a
# nav_panel's shown.bs.tab does -- so the panels stayed blank.
#
# The race itself lives in the browser and is out of testServer's reach. What
# is pinned here instead is (a) the server-side contract that immunises the
# sidebar against it -- suspendWhenHidden = FALSE, so the server pushes these
# outputs no matter what visibility the client claims -- and (b) that the
# render path the user actually looks at survives the removal journey, by
# reading the outputs themselves rather than only rv$* (an output an app test
# never reads is an output the suite never renders).

sidebar_upload <- function(path, name = basename(path)) {
  data.frame(
    name = name, size = file.size(path), type = "",
    datapath = path, stringsAsFactors = FALSE
  )
}

test_that("the sidebar outputs opt out of suspendWhenHidden", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("shinyjs")

  shiny::testServer(.shiny_app_dir(), {
    for (id in c(
      "workspace_header", "summary_metrics", "dataset_nav_ui",
      "add_dataset_ui", "validation_report_ui"
    )) {
      expect_false(
        isTRUE(outputOptions(output, id)$suspendWhenHidden),
        info = paste0("output$", id, " must not suspend when hidden")
      )
    }
  })
})

test_that("removing one of two datasets keeps the sidebar rendered", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("shinyjs")

  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(edit_mode = TRUE)
    session$setInputs(dta_file = sidebar_upload(app_fixture_path("clinical_dta.yaml")))
    session$setInputs(add_ds_name = "second_ds", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 1)
    expect_equal(names(rv$structure), c("clinical_data", "second_ds"))

    # The just-added dataset is active; remove it through the confirm modal.
    session$setInputs(remove_dataset = 1)
    session$setInputs(remove_dataset_confirm = 1)

    expect_equal(names(rv$structure), "clinical_data")
    expect_equal(rv$active, "clinical_data")
    expect_equal(names(rv$status), "clinical_data")

    # The panels the user looks at, not just the state behind them.
    nav <- paste(as.character(output$dataset_nav_ui$html), collapse = "")
    expect_match(nav, "clinical_data", fixed = TRUE)
    expect_false(grepl("second_ds", nav, fixed = TRUE))

    metrics <- paste(as.character(output$summary_metrics$html), collapse = "")
    expect_match(metrics, "datasets", fixed = TRUE)
    expect_match(metrics, ">1<") # one dataset left in the overview count

    header <- paste(as.character(output$workspace_header$html), collapse = "")
    expect_match(header, "workspace-header", fixed = TRUE)
  })
})

test_that("removing the first dataset keeps the sidebar pointing at the second", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("shinyjs")

  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(edit_mode = TRUE)
    session$setInputs(dta_file = sidebar_upload(app_fixture_path("clinical_dta.yaml")))
    session$setInputs(add_ds_name = "second_ds", add_ds_type = "tabular")
    session$setInputs(add_ds_save = 1)

    # Select the FIRST dataset via its nav row, then remove it.
    session$setInputs(selds_1 = 1)
    expect_equal(rv$active, "clinical_data")
    session$setInputs(remove_dataset = 1)
    session$setInputs(remove_dataset_confirm = 1)

    expect_equal(names(rv$structure), "second_ds")
    expect_equal(rv$active, "second_ds")

    nav <- paste(as.character(output$dataset_nav_ui$html), collapse = "")
    expect_match(nav, "second_ds", fixed = TRUE)
    expect_false(grepl("clinical_data", nav, fixed = TRUE))
  })
})
