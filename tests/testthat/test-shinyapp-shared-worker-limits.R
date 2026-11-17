# Limits that keep one session from holding the shared Shiny worker or its
# tempdir: how many columns/rules a spec may declare, and (see the later
# sections) what a spec may make the app read or leave behind. Dummy data only.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

# ---- spec cardinality -------------------------------------------------------

# A bare dataset YAML (no metadata), so it needs no document scaffolding.
sw_dataset_yaml <- function(n_cols, n_rules) {
  cols <- sprintf("  - {id: C%03d, type: SAS Num}", seq_len(n_cols))
  rules <- sprintf(
    "  - {id: R%03d, type: col_range, columns: [C%03d], min: 0, max: 1}",
    seq_len(n_rules), (seq_len(n_rules) - 1L) %% n_cols + 1L
  )
  paste(c(
    "id: d1", "type: tabular", "name: Dummy", "files:", "  filename: d.csv",
    "  type: csv", "columns:", cols, "rules:", rules
  ), collapse = "\n")
}

test_that("a spec past the column+rule cap is refused before anything is built", {
  withr::local_options(dta_app.max_spec_objects = 10)
  res <- app_fn("dta_read_yaml_text")(sw_dataset_yaml(6, 6))
  expect_false(res$ok)
  expect_null(res$value)
  expect_match(res$error, "too many", fixed = TRUE)
})

test_that("the cap counts columns and rules across datasets", {
  withr::local_options(dta_app.max_spec_objects = 10)
  ds <- list(columns = as.list(1:3), rules = as.list(1:3))
  # 2 datasets x (3 + 3) = 12 > 10, though each dataset alone is under the cap.
  check <- app_fn("dta_check_spec_cardinality")
  expect_error(check(list(datasets = list(ds, ds))), "too many")
  expect_silent(check(list(datasets = list(ds))))
  expect_error(check(list(columns = as.list(1:11))), "too many")
})
test_that("a spec at the cap, and an ordinary spec under the default cap, still load", {
  withr::local_options(dta_app.max_spec_objects = 12)
  expect_true(app_fn("dta_read_yaml_text")(sw_dataset_yaml(6, 6))$ok)
  withr::local_options(dta_app.max_spec_objects = NULL)
  expect_true(app_fn("dta_read_yaml")(app_fixture_path("clinical_dta.yaml"))$ok)
})

test_that("an unusable cap option falls back to the default instead of disabling the guard", {
  withr::local_options(dta_app.max_spec_objects = "lots")
  expect_identical(app_fn("dta_max_spec_objects")(), 10000)
  withr::local_options(dta_app.max_spec_objects = -3)
  expect_identical(app_fn("dta_max_spec_objects")(), 10000)
})

# ---- retention of the package's working copies --------------------------------

sw_mk <- function(dir, name, age_h, dir_entry = FALSE) {
  p <- file.path(dir, name)
  if (dir_entry) dir.create(p) else writeLines("x", p)
  Sys.setFileTime(p, Sys.time() - age_h * 3600)
  p
}

test_that("the sweep ages out session scratch directories but leaves the package's own copies to the running process", {
  dir <- withr::local_tempdir()
  scratch <- sw_mk(dir, "dta_scratch_0123456789abcdef", 48, dir_entry = TRUE)
  copies <- c(
    sw_mk(dir, "dta_transcode_1a2b.csv", 48),
    sw_mk(dir, "dta_error_spill_1a2b", 48, dir_entry = TRUE)
  )
  fresh <- sw_mk(dir, "dta_scratch_fresh", 1, dir_entry = TRUE)
  live <- sw_mk(dir, "dta_scratch_live", 48, dir_entry = TRUE)
  other <- sw_mk(dir, "file1a2b.csv", 48)
  # An object read through the API may still stream from a copy in tempdir():
  # the own-process sweep must not take it.
  app_fn("dta_sweep_session_files")(dir, ttl = 24 * 3600, keep = live)
  expect_false(file.exists(scratch))
  expect_true(all(file.exists(c(copies, fresh, live, other))))
  # A worker known to be dead owns nothing, so its copies go once expired.
  app_fn("dta_sweep_session_files")(dir, ttl = 24 * 3600, keep = live, package_copies = TRUE)
  expect_false(any(file.exists(copies)))
  expect_true(all(file.exists(c(fresh, live, other))))
})

test_that("inside a session the package is pointed at the session's scratch directory, outside one it is not", {
  expect_null(app_fn("dta_scratch_options")()$DTAtools.temp_dir)
  token <- strrep("ab", 16)
  opts <- shiny::withReactiveDomain(list(token = token), app_fn("dta_scratch_options")())
  expect_identical(opts$DTAtools.temp_dir, app_fn("dta_session_scratch_path")(token))
  expect_true(dir.exists(opts$DTAtools.temp_dir))
  withr::defer(unlink(opts$DTAtools.temp_dir, recursive = TRUE))
  # The load and check helpers carry it with the container limits.
  expect_identical(
    shiny::withReactiveDomain(list(token = token), app_fn("dta_app_container_options")())$DTAtools.temp_dir,
    opts$DTAtools.temp_dir
  )
})

test_that("a transcoded copy and a spilled error part are written under the directory the caller names", {
  scratch <- withr::local_tempdir()
  src <- withr::local_tempfile(fileext = ".csv")
  writeBin(c(charToRaw("ID,NAME\n1,M_"), as.raw(0xE4), charToRaw("\n")), src)

  copy <- DTAtools:::dta_transcode_to_utf8(src, "latin1")
  expect_identical(normalizePath(dirname(copy)), normalizePath(tempdir()))
  unlink(copy)

  withr::local_options(DTAtools.temp_dir = scratch)
  copy2 <- DTAtools:::dta_transcode_to_utf8(src, "latin1")
  expect_identical(normalizePath(dirname(copy2)), normalizePath(scratch))
  expect_match(basename(copy2), "^dta_transcode_")

  sink <- DTAtools:::dta_error_sink(max_errors = 1)
  errs <- data.frame(row = 1:3, data = c("a", "b", "c"))
  sink <- DTAtools:::dta_error_sink_add(sink, errs)
  expect_identical(normalizePath(dirname(sink$spill_dir)), normalizePath(scratch))

  # a name that is not a directory falls back instead of failing
  withr::local_options(DTAtools.temp_dir = file.path(scratch, "gone"))
  expect_identical(DTAtools:::dta_temp_dir(), tempdir())
})

test_that("a cached transcoded copy whose file is gone is forgotten at the next conversion", {
  src <- withr::local_tempfile(fileext = ".csv")
  writeBin(c(charToRaw("ID\nM_"), as.raw(0xE4), charToRaw("\n")), src)
  cache <- DTAtools:::`__DTAtools_transcode_cache__`
  copy <- DTAtools:::dta_transcode_to_utf8(src, "latin1")
  n <- length(ls(cache, all.names = TRUE))
  unlink(copy)
  src2 <- withr::local_tempfile(fileext = ".csv")
  writeBin(c(charToRaw("ID\nM_"), as.raw(0xE4), charToRaw("\n")), src2)
  copy2 <- DTAtools:::dta_transcode_to_utf8(src2, "latin1")
  withr::defer(unlink(copy2))
  expect_identical(length(ls(cache, all.names = TRUE)), n)
})

test_that("Start over deletes the session's scratch directory", {
  skip_if_not_installed("shiny")
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = strrep("8", 32))
    scratch <- app_fn("dta_session_scratch_path")(session$token)
    dir.create(scratch, recursive = TRUE)
    writeLines("x", file.path(scratch, "dta_transcode_x.csv"))
    registry <- get(".dta_live_session_paths", envir = environment(own))
    expect_true(scratch %in% get(session$token, envir = registry))
    session$setInputs(confirm_reset = 1)
    expect_false(dir.exists(scratch))
    # still registered, so the next upload's copies are spared by the sweep
    expect_true(scratch %in% get(session$token, envir = registry))
  })
})

# ---- workers that ended without cleaning up -----------------------------------

test_that("the sweep of sibling temp directories removes only expired app files of a worker whose heartbeat has lapsed", {
  root <- withr::local_tempdir()
  mkworker <- function(name, heartbeat_age_h = NULL) {
    d <- file.path(root, name)
    dir.create(d)
    if (!is.null(heartbeat_age_h)) {
      sw_mk(d, "dtatools_worker.heartbeat", heartbeat_age_h)
    }
    d
  }
  self <- mkworker("RtmpSelf000", 100)
  dead <- mkworker("RtmpDead111", 48)
  alive <- mkworker("RtmpLive222", 1)
  foreign <- mkworker("RtmpOther33") # an R session of something else: no heartbeat
  notr <- file.path(root, "unrelated")
  dir.create(notr)
  sw_mk(notr, "dta_upload_x", 48)

  dead_old <- sw_mk(dead, "dtatools_app_session_k.rds", 48)
  dead_fresh <- sw_mk(dead, "dta_upload_fresh", 2)
  dead_other <- sw_mk(dead, "Rf1234.tmp", 48)
  dead_copy <- sw_mk(dead, "dta_transcode_old.csv", 48)
  alive_old <- sw_mk(alive, "dta_upload_old", 48)
  foreign_old <- sw_mk(foreign, "dta_upload_old", 48)
  self_old <- sw_mk(self, "dta_upload_old", 48)

  gone <- function(pid) FALSE
  swept <- app_fn("dta_sweep_dead_workers")(root, self = self, ttl = 24 * 3600, alive = gone)

  expect_identical(basename(swept), "RtmpDead111")
  expect_false(file.exists(dead_old))
  # a positively dead worker owns nothing, package copies included
  expect_false(file.exists(dead_copy))
  expect_true(all(file.exists(c(dead_fresh, dead_other, alive_old, foreign_old, self_old, file.path(notr, "dta_upload_x")))))

  # once nothing but the heartbeat is left, the directory goes too
  unlink(c(dead_fresh, dead_other))
  # ... but nothing in it is touched when the process is running (TRUE) or the
  # platform cannot tell (NA): either may be a live idle worker.
  idle <- c(sw_mk(dead, "dta_upload_idle", 48), sw_mk(dead, "dta_transcode_idle.csv", 48))
  for (unknown in list(NA, TRUE)) {
    swept <- app_fn("dta_sweep_dead_workers")(root, self = self, ttl = 24 * 3600, alive = function(pid) unknown)
    expect_identical(swept, character(0))
    expect_true(all(file.exists(idle)))
    expect_true(dir.exists(dead))
  }
  unlink(idle)
  # an empty directory is not removed for an unknown process either
  app_fn("dta_sweep_dead_workers")(root, self = self, ttl = 24 * 3600, alive = function(pid) NA)
  expect_true(dir.exists(dead))
  app_fn("dta_sweep_dead_workers")(root, self = self, ttl = 24 * 3600, alive = gone)
  expect_false(dir.exists(dead))
  expect_true(dir.exists(alive))
})

test_that("a worker's heartbeat is created, then refreshed", {
  d <- withr::local_tempdir()
  f <- app_fn("dta_worker_heartbeat")(d)
  expect_identical(readLines(f), as.character(Sys.getpid()))
  Sys.setFileTime(f, Sys.time() - 7200)
  app_fn("dta_worker_heartbeat")(d)
  expect_lt(as.numeric(difftime(Sys.time(), file.mtime(f), units = "secs")), 60)
})
test_that("the heartbeat is refreshed at least four times per retention window", {
  expect_identical(app_fn("dta_heartbeat_interval_ms")(24 * 3600), 3600 * 1000)
  expect_identical(app_fn("dta_heartbeat_interval_ms")(600), 150 * 1000)
  expect_gte(app_fn("dta_heartbeat_interval_ms")(0.1), 1000)
})

test_that("the process-liveness probe answers NA, not a guess, for a malformed pid", {
  expect_true(is.na(app_fn("dta_pid_alive")("not a pid")))
  expect_true(is.na(app_fn("dta_pid_alive")(character(0))))
})

test_that("the spec cap also sees a bare list of datasets and counts file entries", {
  withr::local_options(dta_app.max_spec_objects = 12)
  check <- app_fn("dta_check_spec_cardinality")
  ds <- list(columns = as.list(1:4), files = as.list(1:2))
  expect_silent(check(list(ds, ds)))
  expect_error(check(list(ds, ds, ds)), "too many")
})
