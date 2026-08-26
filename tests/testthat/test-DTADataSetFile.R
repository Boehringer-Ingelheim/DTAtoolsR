test_that("DTADataSetFile validates readable, non-empty files", {
  path <- tempfile(fileext = ".txt")
  writeLines("hello world", path)

  ds <- DTADataSetFile(name = "notes", paths = path)
  ds <- check(ds, quiet = TRUE)

  status <- validation_status(ds)
  expect_equal(status$table, basename(path))
  expect_equal(status$status, "validated")
  expect_true(status$ok)

  res <- results(ds)
  expect_equal(nrow(res), 1)
  expect_equal(res$target, basename(path))
  expect_equal(res$status, "validated")

  msgs <- messages(ds, as_tibble = FALSE)
  expect_equal(nrow(msgs), 0)
})

test_that("DTADataSetFile reports missing or unreadable files", {
  path <- tempfile(fileext = ".txt")
  if (file.exists(path)) {
    unlink(path)
  }

  ds <- DTADataSetFile(name = "missing_file", paths = path)
  ds <- check(ds, quiet = TRUE)

  status <- validation_status(ds)
  expect_equal(status$status, "validated")
  expect_false(status$ok)

  res <- results(ds)
  expect_equal(res$n_rule_errors, 1)
  expect_equal(res$status, "failed")

  msgs <- messages(ds, as_tibble = FALSE)
  expect_equal(nrow(msgs), 1)
  expect_equal(msgs$source, "rule")
  # The old alternation "not found|readable|empty" could not tell the three
  # distinct failure reasons apart. This scenario is specifically a missing
  # file, so pin that reason.
  expect_match(msgs$message, "not found")
})

test_that("DTADataSetFile flags an existing but empty file", {
  path <- tempfile(fileext = ".txt")
  file.create(path)
  on.exit(unlink(path), add = TRUE)

  ds <- check(DTADataSetFile(name = "empty_file", paths = path), quiet = TRUE)

  status <- validation_status(ds)
  expect_false(status$ok)

  msgs <- messages(ds, as_tibble = FALSE)
  expect_equal(nrow(msgs), 1)
  expect_match(msgs$message, "empty")
})

test_that("DTADataSetFile keeps two paths that share a basename apart", {
  dir_a <- file.path(tempdir(), "dta-basename-a")
  dir_b <- file.path(tempdir(), "dta-basename-b")
  dir.create(dir_a, showWarnings = FALSE)
  dir.create(dir_b, showWarnings = FALSE)
  on.exit(unlink(c(dir_a, dir_b), recursive = TRUE), add = TRUE)

  present <- file.path(dir_a, "same.txt")
  writeLines("content", present)
  absent <- file.path(dir_b, "same.txt")

  ds <- check(
    DTADataSetFile(name = "collision", paths = c(present, absent)),
    quiet = TRUE
  )

  status <- validation_status(ds)

  expect_equal(nrow(status), 2)
  expect_setequal(status$ok, c(TRUE, FALSE))

  # Exactly one message, for the absent file, and inspect() must resolve it
  # back to the full path it came from.
  msgs <- messages(ds, as_tibble = FALSE)
  expect_equal(nrow(msgs), 1)
  expect_match(msgs$message, "not found")

  details <- inspect(ds, as_tibble = FALSE)
  expect_equal(nrow(details), 1)
  expect_equal(details$file_path, absent)
})

test_that("dta_file_target_keys disambiguates only where it has to", {
  expect_equal(
    dta_file_target_keys(c("a/one.txt", "b/two.txt")),
    c("one.txt", "two.txt")
  )
  expect_equal(
    dta_file_target_keys(c("a/same.txt", "b/same.txt")),
    c("a/same.txt", "b/same.txt")
  )
  # Identical paths cannot be told apart by path either, so fall back to a
  # suffix rather than silently dropping one.
  expect_equal(length(unique(dta_file_target_keys(c("a/x.txt", "a/x.txt")))), 2)
  expect_equal(dta_file_target_keys(character()), character())
})

test_that("DTA results and messages combine tabular and file datasets", {
  path <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  dta <- read_dta_from_yaml(path)
  dta <- load_file(
    dta,
    1,
    file = system.file("extdata", "clinical_data.csv", package = "DTAtools")
  )

  missing_path <- tempfile(fileext = ".txt")
  if (file.exists(missing_path)) {
    unlink(missing_path)
  }

  file_ds <- DTADataSetFile(name = "attachment", paths = missing_path)
  dta@datasets[["attachment"]] <- file_ds

  dta <- check(dta, persist = FALSE, quiet = TRUE)

  res <- results(dta)
  expect_true(all(c("clinical_data", "attachment") %in% res$dataset))
  expect_equal(nrow(res), 2)
  expect_equal(length(unique(res$validation_run)), 1)
  expect_equal(res$status[res$dataset == "clinical_data"], "validated")
  expect_equal(res$status[res$dataset == "attachment"], "failed")
  expect_false(any(is.na(res$run_id)))

  msgs <- messages(dta, as_tibble = FALSE)
  expect_true(any(msgs$dataset == "attachment"))
  expect_false(any(msgs$dataset == "clinical_data"))
})


# ---------------------------------------------------------------------------
# load_file() for DTADataSetFile -- section 4 of the implementation notes
# ---------------------------------------------------------------------------

test_that("load_file() binds a path and records the key as basename WITH extension", {
  path <- tempfile(fileext = ".txt")
  writeLines("content", path)
  on.exit(unlink(path), add = TRUE)

  ds <- DTADataSetFile(name = "delivery", paths = character())
  # No paths yet; add a handler first
  ds@files <- list(DTAFileAny(filename = basename(path)))
  ds <- load_file(ds, file = path, handler_index = 1)

  expect_equal(ds@file_paths, path)
  # The key used by validation_status() must be the basename WITH extension
  ds <- check(ds, quiet = TRUE)
  status <- validation_status(ds)
  expect_equal(status$table, basename(path))
  expect_true(status$ok)
})

test_that("load_file() via DTA-level call reaches a DTADataSetFile dataset", {
  # This is the exact call shape the Shiny app uses.
  # It used to abort with 'This method needs to be implemented in derived classes'.
  path <- tempfile(fileext = ".txt")
  writeLines("content", path)
  on.exit(unlink(path), add = TRUE)

  h <- DTAFileAny(filename = basename(path))
  ds <- DTADataSetFile(name = "file_ds", files = list(h))
  dta <- DTA(datasets = list(file_ds = ds))

  dta2 <- load_file(dta, dataset = "file_ds", file = path, handler_index = 1)
  expect_s3_class(dta2, "DTAtools::DTA")
  ds2 <- datasets(dta2, "file_ds")
  expect_equal(ds2@file_paths, path)
})

test_that("a DTA-level load without an explicit name still replaces, not appends", {
  # The DTA method used to default `name` to file_path_sans_ext(basename(file)),
  # the shape a TABULAR dataset keys by. A file dataset keys by the full file
  # name, so that default matched no existing entry and every re-delivery
  # appended: @file_paths grew without bound and dta_file_target_keys() then
  # disambiguated the duplicates into "<path>_1", "<path>_2". The DTA method now
  # leaves the default to whichever dataset method it dispatches to.
  path <- tempfile(fileext = ".txt")
  writeLines("content", path)
  on.exit(unlink(path), add = TRUE)

  ds <- DTADataSetFile(
    name = "file_ds",
    files = list(DTAFileAny(filename = basename(path)))
  )
  dta <- DTA(datasets = list(file_ds = ds))

  for (i in 1:3) {
    dta <- load_file(dta, dataset = "file_ds", file = path, handler_index = 1)
  }

  bound <- datasets(dta, "file_ds")@file_paths
  expect_length(bound, 1L)
  expect_equal(bound, path)
  expect_equal(dta_file_target_keys(bound), basename(path))
})

test_that("a DTA-level load into a tabular dataset still names the table without its extension", {
  # The other half of the contract above: removing the DTA-level default must
  # not change what a tabular dataset calls its table.
  file <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
  ds <- DTADataSetTabular(
    name = "tab",
    specs = create_example_DTAColumnSpecCollection(1),
    files = list(DTAFileCSV(filename = "clinical_data.csv"))
  )
  dta <- DTA(datasets = list(tab = ds))

  dta <- load_file(dta, dataset = "tab", file = file, handler_index = 1)

  expect_equal(names(tables(datasets(dta, "tab"))), "clinical_data")
})

test_that("re-loading the same name replaces rather than appends", {
  dir1 <- file.path(tempdir(), "dta-replace-a")
  dir2 <- file.path(tempdir(), "dta-replace-b")
  dir.create(dir1, showWarnings = FALSE)
  dir.create(dir2, showWarnings = FALSE)
  on.exit(unlink(c(dir1, dir2), recursive = TRUE), add = TRUE)

  fname <- "same.txt"
  path1 <- file.path(dir1, fname)
  path2 <- file.path(dir2, fname)
  writeLines("v1", path1)
  writeLines("v2", path2)

  h <- DTAFileAny(filename = fname)
  ds <- DTADataSetFile(name = "delivery", files = list(h))
  ds <- load_file(ds, file = path1, handler_index = 1)
  ds <- check(ds, quiet = TRUE)

  # should have one validated entry
  expect_equal(nrow(validation_status(ds)), 1)
  expect_true(validation_status(ds)$ok)

  # re-bind with different path but SAME basename key
  ds <- load_file(ds, file = path2, handler_index = 1)

  # stale verdict must have been cleared
  expect_equal(nrow(validation_status(ds)), 0)
  # and there is still only one path
  expect_equal(length(ds@file_paths), 1)
  expect_equal(ds@file_paths, path2)
})

test_that("load_file() aborts on a non-matching filename", {
  h <- DTAFileAny(filename = "expected.txt")
  ds <- DTADataSetFile(name = "delivery", files = list(h))

  expect_error(
    load_file(ds, file = "/some/dir/other.txt", handler_index = 1),
    "does not match the filename or pattern"
  )
})

test_that("load_file() aborts on an out-of-range handler_index", {
  h <- DTAFileAny(filename = "any.txt")
  ds <- DTADataSetFile(name = "delivery", files = list(h))

  expect_error(
    load_file(ds, file = "/dir/any.txt", handler_index = 5),
    "Invalid handler_index"
  )
  expect_error(
    load_file(ds, file = "/dir/any.txt", handler_index = 0),
    "Invalid handler_index"
  )
})

# ---------------------------------------------------------------------------
# clear_validation() for DTADataSetFile -- section 5
# ---------------------------------------------------------------------------

test_that("clear_validation() clears a single target and leaves others", {
  dir_tmp <- tempfile()
  dir.create(dir_tmp)
  on.exit(unlink(dir_tmp, recursive = TRUE), add = TRUE)

  p1 <- file.path(dir_tmp, "a.txt")
  p2 <- file.path(dir_tmp, "b.txt")
  writeLines("a", p1)
  writeLines("b", p2)

  ds <- check(
    DTADataSetFile(name = "two_files", paths = c(p1, p2)),
    quiet = TRUE
  )
  expect_equal(nrow(validation_status(ds)), 2)

  ds2 <- clear_validation(ds, tables = "a.txt")
  vs <- validation_status(ds2)
  # only the cleared one is gone
  expect_equal(nrow(vs), 1)
  expect_equal(vs$table, "b.txt")
})

test_that("clear_validation() with no tables clears all", {
  path <- tempfile(fileext = ".txt")
  writeLines("x", path)
  on.exit(unlink(path), add = TRUE)

  ds <- check(DTADataSetFile(name = "d", paths = path), quiet = TRUE)
  expect_equal(nrow(validation_status(ds)), 1)

  ds2 <- clear_validation(ds)
  expect_equal(nrow(validation_status(ds2)), 0)
})

test_that("clear_validation(remove_artifacts=TRUE) unlinks the artifact file", {
  path <- tempfile(fileext = ".txt")
  writeLines("content", path)
  on.exit(unlink(path), add = TRUE)

  art_dir <- tempfile()
  dir.create(art_dir)
  on.exit(unlink(art_dir, recursive = TRUE), add = TRUE)

  ds <- check(
    DTADataSetFile(name = "d", paths = path),
    persist = TRUE,
    artifact_dir = art_dir,
    quiet = TRUE
  )
  entry <- ds@validation_index[[basename(path)]]
  artifact <- entry$artifact_path

  # The artifact has to exist for its removal to mean anything.
  expect_true(file.exists(artifact))

  clear_validation(ds, remove_artifacts = TRUE)
  expect_false(file.exists(artifact))
})

# ---------------------------------------------------------------------------
# validation_status() returns zero-row frame when no targets -- section 6
# ---------------------------------------------------------------------------

test_that("validation_status() on an empty DTADataSetFile is a zero-row data.frame", {
  ds <- DTADataSetFile(name = "empty_ds", paths = character())
  vs <- validation_status(ds)

  expect_true(is.data.frame(vs))
  expect_equal(nrow(vs), 0)
  expect_true("table" %in% names(vs))
})

test_that("check() on a DTA with an empty DTADataSetFile does not error", {
  ds <- DTADataSetFile(name = "empty_ds", paths = character())
  dta <- DTA(datasets = list(empty_ds = ds))

  # Must not error even though there is nothing to validate
  expect_no_error(check(dta, quiet = TRUE))
})

# ---------------------------------------------------------------------------
# DTADataSetFile(paths=...) builds DTAFileAny handlers -- section 7
# ---------------------------------------------------------------------------

test_that("DTADataSetFile(paths=f)@files[[1]] is a DTAFileAny", {
  path <- tempfile(fileext = ".txt")
  writeLines("x", path)
  on.exit(unlink(path), add = TRUE)

  ds <- DTADataSetFile(name = "d", paths = path)
  expect_s3_class(ds@files[[1]], "DTAtools::DTAFileAny")
})
