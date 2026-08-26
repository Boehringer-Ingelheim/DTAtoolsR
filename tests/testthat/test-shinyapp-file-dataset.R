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

  # A Files dataset's load_file() keys every record by the delivered file's OWN
  # name (basename WITH extension) -- name defaults to file_path_sans_ext()
  # inside dta_load_file(), which is right for a tabular dataset's table name
  # but wrong here, so it has to be given explicitly.
  res <- load_fn(
    dta_obj,
    dataset = "reports", file = target, handler_index = 1, name = basename(target)
  )
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
  # A Files dataset's load_file() keys every record by the delivered file's OWN
  # name (basename WITH extension) -- name defaults to file_path_sans_ext()
  # inside dta_load_file(), which is right for a tabular dataset's table name
  # but wrong here, so it has to be given explicitly.
  res <- load_fn(
    dta_obj,
    dataset = "reports", file = target, handler_index = 1, name = basename(target)
  )
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
  # A Files dataset's load_file() keys every record by the delivered file's OWN
  # name (basename WITH extension) -- name defaults to file_path_sans_ext()
  # inside dta_load_file(), which is right for a tabular dataset's table name
  # but wrong here, so it has to be given explicitly.
  res <- load_fn(
    dta_obj,
    dataset = "reports", file = target, handler_index = 1, name = basename(target)
  )
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
  # A Files dataset's load_file() keys every record by the delivered file's OWN
  # name (basename WITH extension) -- name defaults to file_path_sans_ext()
  # inside dta_load_file(), which is right for a tabular dataset's table name
  # but wrong here, so it has to be given explicitly.
  res <- load_fn(
    dta_obj,
    dataset = "reports", file = target, handler_index = 1, name = basename(target)
  )
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
    file = target, handler_index = 1, name = basename(target)
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

# ---- dta_lookup(): name lookup that survives an atomic container ------------
#
# Regression cover for "Error: subscript out of bounds" in the Loaded files
# panel. `[[` is null-safe on a list and NOT on an atomic vector, and
# `x[["missing"]] %||% "pending"` cannot rescue the atomic case because the
# error is raised while evaluating the left operand. The status maps are atomic
# character vectors, so every read of one has to go through dta_lookup().

test_that("dta_lookup() returns the default for a name absent from an ATOMIC vector", {
  fn <- app_fn("dta_lookup")

  # The bare `[[` this replaces: the message is base R's and is translated on a
  # non-English machine, so pin the condition CLASS, never the text.
  expect_error(c(a = "1")[["b"]], class = "subscriptOutOfBoundsError")

  expect_equal(fn(c(a = "1"), "b", "pending"), "pending")
  expect_equal(fn(stats::setNames(character(0), character(0)), "b", "pending"), "pending")
})

test_that("dta_lookup() returns the stored value when the name IS present", {
  fn <- app_fn("dta_lookup")

  expect_equal(fn(c(a = "pass"), "a", "pending"), "pass")
  expect_equal(fn(list(a = "fail"), "a", "pending"), "fail")
})

test_that("dta_lookup() defaults for an unusable container or name", {
  fn <- app_fn("dta_lookup")

  expect_equal(fn(NULL, "a", "pending"), "pending")
  expect_equal(fn(c(a = "1"), NULL, "pending"), "pending")
  expect_equal(fn(c(a = "1"), NA_character_, "pending"), "pending")
  expect_equal(fn(c("1", "2"), "a", "pending"), "pending") # unnamed
  expect_equal(fn(c(a = "1", b = "2"), c("a", "b"), "pending"), "pending") # not length 1
  expect_null(fn(c(a = "1"), "b"))
})

# ---- the reported bug, end to end ------------------------------------------

test_that("a file bound but not yet checked reports 'pending', not a subscript error", {
  # THE REPORTED BUG: uploading into a Files dataset bound the file (so a retry
  # offered to overwrite it) and then blew up rendering the Loaded files panel.
  # A file dataset's validation_status() is EMPTY until check() runs, so the
  # per-file tick looked its name up in a zero-length named character vector.
  load_fn <- app_fn("dta_load_file")
  name_fn <- app_fn("dta_bound_item_name")
  status_fn <- app_fn("dta_table_status_map")
  lookup_fn <- app_fn("dta_lookup")

  target <- file.path(tempdir(), "report.pdf")
  writeLines("dummy", target)
  on.exit(unlink(target), add = TRUE)

  dta_obj <- make_file_dta()
  tbl <- name_fn(DTAtools::datasets(dta_obj, "reports")@type, target)
  expect_equal(tbl, "report.pdf")

  # A Files dataset's load_file() keys every record by the delivered file's OWN
  # name (basename WITH extension) -- name defaults to file_path_sans_ext()
  # inside dta_load_file(), which is right for a tabular dataset's table name
  # but wrong here, so it has to be given explicitly.
  res <- load_fn(
    dta_obj,
    dataset = "reports", file = target, handler_index = 1, name = basename(target)
  )
  expect_true(res$ok, info = res$error)

  # The precondition that made the panel throw: nothing is validated yet.
  tstatus <- status_fn(res$value, "reports")
  expect_length(tstatus, 0)
  expect_error(tstatus[[tbl]], class = "subscriptOutOfBoundsError")

  # What the Loaded files panel now does instead.
  expect_equal(lookup_fn(tstatus, tbl, "pending"), "pending")
})

test_that("the per-file tick still turns to 'pass' once the dataset is checked", {
  # The guard must not swallow a real status: the same lookup has to keep
  # reporting pass/fail after check() populates validation_status().
  load_fn <- app_fn("dta_load_file")
  name_fn <- app_fn("dta_bound_item_name")
  status_fn <- app_fn("dta_table_status_map")
  lookup_fn <- app_fn("dta_lookup")

  target <- file.path(tempdir(), "report.pdf")
  writeLines("dummy", target)
  on.exit(unlink(target), add = TRUE)

  dta_obj <- make_file_dta()
  tbl <- name_fn(DTAtools::datasets(dta_obj, "reports")@type, target)
  # A Files dataset's load_file() keys every record by the delivered file's OWN
  # name (basename WITH extension) -- name defaults to file_path_sans_ext()
  # inside dta_load_file(), which is right for a tabular dataset's table name
  # but wrong here, so it has to be given explicitly.
  res <- load_fn(
    dta_obj,
    dataset = "reports", file = target, handler_index = 1, name = basename(target)
  )
  expect_true(res$ok, info = res$error)

  checked <- DTAtools::check(res$value, datasets = "reports", quiet = TRUE)
  tstatus <- status_fn(checked, "reports")
  expect_named(tstatus, tbl)
  expect_equal(lookup_fn(tstatus, tbl, "pending"), "pass")
})

test_that("dta_load_file() derives its default key from the dataset type", {
  # The default used to hardcode the TABULAR rule (extension stripped) for
  # both dataset types. The app's own call site always passes `name`, so it
  # never noticed -- but every other caller bound a file dataset under a key
  # no report for that class looks up, and once load_file(DTADataSetFile)
  # began refusing a `name` that is not the delivered basename, the same
  # default aborted outright. The branch now lives in dta_bound_item_name().
  load_fn <- app_fn("dta_load_file")
  names_fn <- app_fn("dta_dataset_table_names")
  get_ds <- app_fn("dta_get_dataset")

  dir <- withr::local_tempdir()
  path <- file.path(dir, "report.pdf")
  writeLines("content", path)

  # A file dataset keys by the delivered name, extension KEPT.
  res <- load_fn(make_file_dta(), "reports", file = path, handler_index = 1)
  expect_true(isTRUE(res$ok))
  expect_equal(names_fn(get_ds(res$value, "reports")), "report.pdf")

  # A tabular dataset still names the table with the extension STRIPPED.
  csv <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
  tab <- DTAtools::DTA(datasets = list(clinical_data = DTAtools::DTADataSetTabular(
    name = "clinical_data",
    specs = DTAtools::create_example_DTAColumnSpecCollection(1),
    files = list(DTAtools::DTAFileCSV(filename = "clinical_data.csv"))
  )))
  res2 <- load_fn(tab, "clinical_data", file = csv, handler_index = 1)
  expect_true(isTRUE(res2$ok))
  expect_equal(names_fn(get_ds(res2$value, "clinical_data")), "clinical_data")
})
