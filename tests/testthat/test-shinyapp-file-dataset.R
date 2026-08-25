# App-level tests for Files datasets (DTADataSetFile) in the Shiny helpers.
#
# All helpers are fetched via app_fn() / app_env() from helper-shinyapp.R;
# the fixture DTA comes from app_fixture_dta() (the clinical spec, which
# contains only tabular datasets). Tests that need a file dataset build one
# from scratch and insert it via dta@datasets[[...]] or the app helpers.

# ---- Helper: build a minimal DTA with one file dataset ----------------------

make_file_dta <- function(
  ds_name = "reports",
  handler_filename = "report.pdf",
  extensions = NULL
) {
  h <- DTAtools::DTAFileAny(
    filename = handler_filename,
    extensions = extensions
  )
  ds <- DTAtools::DTADataSetFile(name = ds_name, files = list(h))
  DTAtools::DTA(datasets = setNames(list(ds), ds_name))
}

# ---- dta_load_file() into a Files dataset -----------------------------------

test_that("dta_load_file() into a Files dataset returns ok and increments the content count", {
  # This is the regression that started the whole task: the DTA-level
  # load_file() dispatch used to abort with "This method needs to be implemented
  # in derived classes" for DTADataSetFile.
  load_fn <- app_fn("dta_load_file")
  count_fn <- app_fn("dta_dataset_content_count")

  path <- tempfile(fileext = ".pdf")
  writeLines("dummy", path)
  on.exit(unlink(path), add = TRUE)
  # rename so basename matches handler
  target <- file.path(dirname(path), "report.pdf")
  file.rename(path, target)
  on.exit(unlink(target), add = TRUE)

  dta_obj <- make_file_dta()
  before <- count_fn(DTAtools::datasets(dta_obj, "reports"))
  expect_equal(before, 0L)

  res <- load_fn(dta_obj, dataset = "reports", file = target, handler_index = 1)
  expect_true(res$ok, info = res$error)

  after <- count_fn(DTAtools::datasets(res$value, "reports"))
  expect_equal(after, 1L)
})

# ---- dta_bound_item_name() agrees with dta_dataset_table_names() ------------

test_that("dta_bound_item_name('file') returns basename WITH extension", {
  fn <- app_fn("dta_bound_item_name")

  result <- fn("file", "/some/dir/report.pdf")
  expect_equal(result, "report.pdf")
})

test_that("dta_bound_item_name('tabular') returns basename WITHOUT extension", {
  fn <- app_fn("dta_bound_item_name")

  result <- fn("tabular", "/some/dir/clinical_data.csv")
  expect_equal(result, "clinical_data")
})

test_that("dta_bound_item_name('file') agrees with dta_dataset_table_names()", {
  # dta_dataset_table_names() and dta_bound_item_name() must produce the SAME
  # key for a loaded file, so dta_unload_table() removes exactly what the UI lists.
  table_names_fn <- app_fn("dta_dataset_table_names")
  bound_name_fn <- app_fn("dta_bound_item_name")
  load_fn <- app_fn("dta_load_file")

  path <- tempfile(fileext = ".txt")
  writeLines("hello", path)
  on.exit(unlink(path), add = TRUE)
  target <- file.path(dirname(path), "report.pdf")
  file.copy(path, target)
  on.exit(unlink(target), add = TRUE)

  dta_obj <- make_file_dta()
  res <- load_fn(dta_obj, dataset = "reports", file = target, handler_index = 1)
  expect_true(res$ok)

  listed <- table_names_fn(DTAtools::datasets(res$value, "reports"))
  named <- bound_name_fn("file", target)

  expect_equal(listed, named)
  expect_equal(listed, "report.pdf")
})

# ---- dta_clear_validation() on a Files dataset ------------------------------

test_that("dta_clear_validation() on a Files dataset succeeds", {
  clear_fn <- app_fn("dta_clear_validation")
  load_fn <- app_fn("dta_load_file")

  path <- tempfile(fileext = ".txt")
  writeLines("hello", path)
  on.exit(unlink(path), add = TRUE)
  target <- file.path(dirname(path), "report.pdf")
  file.copy(path, target)
  on.exit(unlink(target), add = TRUE)

  dta_obj <- make_file_dta()
  res <- load_fn(dta_obj, dataset = "reports", file = target, handler_index = 1)
  dta_checked <- DTAtools::check(res$value, quiet = TRUE)

  # validation_status should have an entry
  ds <- DTAtools::datasets(dta_checked, "reports")
  expect_equal(nrow(DTAtools::validation_status(ds)), 1)

  res2 <- clear_fn(dta_checked, dataset = "reports")
  expect_true(res2$ok, info = res2$error)

  ds2 <- DTAtools::datasets(res2$value, "reports")
  expect_equal(nrow(DTAtools::validation_status(ds2)), 0)
})

# ---- dta_table_status_map() yields "pass" after dta_check() -----------------

test_that("dta_table_status_map() yields 'pass' after a successful dta_check()", {
  status_fn <- app_fn("dta_table_status_map")
  check_fn <- app_fn("dta_check")
  load_fn <- app_fn("dta_load_file")

  path <- tempfile(fileext = ".txt")
  writeLines("content", path)
  on.exit(unlink(path), add = TRUE)
  target <- file.path(dirname(path), "report.pdf")
  file.copy(path, target)
  on.exit(unlink(target), add = TRUE)

  dta_obj <- make_file_dta()
  res <- load_fn(dta_obj, dataset = "reports", file = target, handler_index = 1)
  res_checked <- check_fn(res$value)
  expect_true(res_checked$ok, info = res_checked$error)

  sm <- status_fn(res_checked$value, "reports")
  # Should have an entry and it should be "pass"
  expect_true(length(sm) > 0)
  expect_true(any(sm == "pass"))
})

# ---- Full add-dataset -> add-handler -> load -> check -> YAML round trip -----

test_that("full file-dataset round trip: add dataset, handler, load, check, YAML", {
  skip_if_not_installed("yaml")

  add_fn <- app_fn("dta_add_dataset")
  set_handler_fn <- app_fn("dta_set_handler")
  load_fn <- app_fn("dta_load_file")
  to_yaml <- app_fn("dta_to_yaml_text")
  from_yaml <- app_fn("dta_read_yaml_text")
  handlers_fn <- app_fn("dta_handlers")

  path <- tempfile(fileext = ".txt")
  writeLines("report content", path)
  on.exit(unlink(path), add = TRUE)
  target <- file.path(dirname(path), "report.pdf")
  file.copy(path, target)
  on.exit(unlink(target), add = TRUE)

  # Start from the standard fixture (tabular only)
  dta_obj <- app_fixture_dta()

  # Add a file dataset
  res_add <- add_fn(dta_obj, "reports", type = "file")
  expect_true(res_add$ok, info = res_add$error)

  # Add a handler of type "any" to that file dataset
  res_handler <- set_handler_fn(
    res_add$value, "reports",
    index = NULL, filename = "report.pdf",
    type = "any", dataset_type = "file"
  )
  expect_true(res_handler$ok, info = res_handler$error)

  # Load the file
  res_load <- load_fn(
    res_handler$value,
    dataset = "reports",
    file = target, handler_index = 1
  )
  expect_true(res_load$ok, info = res_load$error)

  # Check only the file dataset to avoid the tabular dataset's empty-table error
  dta_checked <- DTAtools::check(res_load$value, datasets = "reports", quiet = TRUE)
  ds <- DTAtools::datasets(dta_checked, "reports")
  vs <- DTAtools::validation_status(ds)
  expect_true(vs$ok)

  # YAML round trip
  txt <- to_yaml(dta_checked)
  expect_true(txt$ok, info = txt$error)

  back <- from_yaml(txt$value)
  expect_true(back$ok, info = back$error)

  # The handler survived the round trip
  ds_back <- DTAtools::datasets(back$value, "reports")
  hs_back <- handlers_fn(ds_back)
  expect_length(hs_back, 1)
  expect_s3_class(hs_back[[1]], "DTAtools::DTAFileAny")
})
