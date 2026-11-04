# What 'Restore previous session' writes, who may read it back, and how long
# it is kept: the autosave is bound to the signed-in user, bound tables are
# stored once as Arrow files rather than re-serialised on every autosave, and
# every file carrying a user's data is aged out by a retention sweep.

app_server_dir <- function() .shiny_app_dir()

app_file_input <- function(filename) {
  path <- app_fixture_path(filename)
  data.frame(
    name = filename, size = file.size(path), type = "",
    datapath = path, stringsAsFactors = FALSE
  )
}

clean_session_file <- function() {
  f <- list.files(tempdir(), pattern = "^dtatools_app_session", full.names = TRUE)
  unlink(f, recursive = TRUE, force = TRUE)
  invisible(f)
}

saved_session <- function() {
  f <- list.files(tempdir(), pattern = "^dtatools_app_session.*[.]rds$", full.names = TRUE)
  expect_length(f, 1)
  list(path = f[[1]], value = readRDS(f[[1]]))
}

test_that("dta_session_key binds the browser id to the signed-in user", {
  key <- app_fn("dta_session_key")
  id <- strrep("9", 32)

  expect_identical(key(id, NULL), id)
  expect_identical(key(id, ""), id)
  alice <- key(id, "alice")
  expect_match(alice, "^[a-f0-9]{32}$")
  expect_false(identical(alice, id))
  expect_false(identical(alice, key(id, "mallory")))
})

test_that("dta_table_store writes a table once, reuses it, and deletes it once unbound", {
  dir <- tempfile("store-test-")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  store <- app_fn("dta_table_store")(dir)
  dta <- app_fixture_dta_with_data()
  n <- nrow(as.data.frame(DTAtools::datasets(dta, "clinical_data")@tables[["clinical_data"]]))

  dump1 <- store$dump(dta)
  store$commit()
  p1 <- dump1$tables$clinical_data$clinical_data
  expect_true(is.character(p1) && file.exists(p1))
  # Unchanged table: the same file, not a rewrite.
  expect_identical(store$dump(dta)$tables$clinical_data$clinical_data, p1)

  # The dump restores from the file -- but only from inside the store's own
  # directory: a dump pointing anywhere else restores no table.
  restore <- app_fn("dta_restore_session")
  restored <- restore(dump1, table_dir = dir)
  expect_equal(
    nrow(as.data.frame(DTAtools::datasets(restored, "clinical_data")@tables[["clinical_data"]])),
    n
  )
  elsewhere <- tempfile("elsewhere-")
  on.exit(unlink(elsewhere, recursive = TRUE), add = TRUE)
  expect_length(restore(dump1, table_dir = elsewhere)@datasets[["clinical_data"]]@tables, 0)
  expect_length(restore(dump1)@datasets[["clinical_data"]]@tables, 0)

  # A replaced table gets a new file; the old one survives until the autosave
  # that stops pointing at it is committed.
  ds <- dta@datasets[["clinical_data"]]
  ds@validation_index <- list()
  ds@validation_store <- list()
  ds@tables[["clinical_data"]] <- arrow::as_arrow_table(data.frame(x = 1:2))
  dta@datasets[["clinical_data"]] <- ds
  p2 <- store$dump(dta)$tables$clinical_data$clinical_data
  expect_false(identical(p2, p1))
  expect_true(file.exists(p1))
  store$commit()
  expect_false(file.exists(p1))
  expect_true(file.exists(p2))

  # Gone from the document: gone from disk, again only on commit.
  dta@datasets[["clinical_data"]] <- NULL
  store$dump(dta)
  expect_true(file.exists(p2))
  store$commit()
  expect_false(file.exists(p2))
})

test_that("dta_sweep_session_files deletes only expired, unowned session files", {
  dir <- tempfile("sweep-test-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  mk <- function(name, age_h) {
    p <- file.path(dir, name)
    writeLines("x", p)
    Sys.setFileTime(p, Sys.time() - age_h * 3600)
    p
  }
  old_autosave <- mk("dtatools_app_session_abc.rds", 48)
  old_upload <- mk("dta_upload_1", 48)
  old_export <- mk("dta-export-1.md", 48)
  fresh <- mk("dtatools_app_session_def.rds", 1)
  owned <- mk("dta_upload_2", 48)
  unrelated <- mk("something_else.rds", 48)

  swept <- app_fn("dta_sweep_session_files")(dir, ttl = 24 * 3600, keep = owned)

  expect_setequal(basename(swept), basename(c(old_autosave, old_upload, old_export)))
  expect_false(any(file.exists(c(old_autosave, old_upload, old_export))))
  expect_true(all(file.exists(c(fresh, owned, unrelated))))
})

test_that("autosave stores tables once; restore needs the same user; Start over wipes it all", {
  skip_if_not_installed("shiny")
  clean_session_file()
  on.exit(clean_session_file(), add = TRUE)
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = strrep("9", 32))
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    n <- nrow(as.data.frame(rv$dta@datasets[["clinical_data"]]@tables[["clinical_data"]]))
    # What the session registered for the sweep to spare -- the staged copy
    # of the upload among it. Read through own()'s environment, which is the
    # live server frame: testServer evaluates this block against a snapshot
    # of that frame, so `owned` here would not follow own()'s `<<-`, and the
    # snapshot leaves out dot-names such as the registry itself.
    registry <- get(".dta_live_session_paths", envir = environment(own))
    staged <- grep("dta_upload_", get(session$token, envir = registry), value = TRUE)
    expect_length(staged, 1)
    expect_true(dir.exists(staged))

    s1 <- saved_session()
    p1 <- s1$value$dump$tables$clinical_data$clinical_data
    expect_true(is.character(p1) && file.exists(p1))

    # Another autosave with the data unchanged reuses the table file.
    session$setInputs(stop_editing = 1)
    expect_identical(saved_session()$value$dump$tables$clinical_data$clinical_data, p1)

    # Restore brings the data back from it -- and keeps using that file.
    session$setInputs(restore_session = 1)
    expect_equal(nrow(as.data.frame(rv$dta@datasets[["clinical_data"]]@tables[["clinical_data"]])), n)
    session$setInputs(stop_editing = 2)
    expect_identical(saved_session()$value$dump$tables$clinical_data$clinical_data, p1)

    # A file written for somebody else is refused.
    s <- saved_session()
    s$value$user <- "mallory"
    saveRDS(s$value, s$path)
    token <- rv$doc_token
    session$setInputs(restore_session = 2)
    expect_equal(rv$doc_token, token)

    # Start over removes the autosave, its table files and the staged upload.
    session$setInputs(confirm_reset = 1)
    expect_false(file.exists(s$path))
    expect_false(file.exists(p1))
    expect_false(dir.exists(staged))
  })
})
