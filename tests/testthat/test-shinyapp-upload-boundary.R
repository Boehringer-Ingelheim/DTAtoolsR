# The upload boundary of the Shiny app: what an upload value may be, how much a
# compressed upload may inflate, and which browser the autosave belongs to.
# Dummy data only; sizes are a few KB.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")


# ---- which values the app will read ----------------------------------------
# The mechanism (the client-message guard, the upload-folder shape check and the
# staging rules) is pinned in test-shinyapp-upload-guard.R; these two cases pin
# what it must refuse whatever the mechanism: a URL (file() opens those) and a
# regular file that is not in one of Shiny's own upload folders.

test_that("a URL or a regular file outside an upload folder is not an upload", {
  problem <- app_fn("dta_upload_value_problem")
  real <- withr::local_tempfile(fileext = ".yaml")
  writeLines("a: 1", real)
  expect_false(is.null(problem(data.frame(name = "x", datapath = "http://127.0.0.1:1/x.yaml"))))
  expect_false(is.null(problem(data.frame(name = "x", datapath = real))))
  expect_false(is.null(problem(data.frame(name = "x", datapath = tempdir()))))
})

# ---- inflation caps --------------------------------------------------------

ub_gz <- function(bytes, name = "data.csv.gz", env = parent.frame()) {
  p <- file.path(withr::local_tempdir(.local_envir = env), name)
  con <- gzfile(p, "wb")
  writeBin(bytes, con)
  close(con)
  p
}

test_that("a gzip file is judged by what it inflates to, and only a gzip file", {
  gz <- ub_gz(rep(charToRaw("a,b\n1,2\n"), 131072)) # 1 MiB inflated
  expect_lt(file.size(gz), 5000)
  withr::local_options(DTAtools.container_max_member_bytes = 1e5)
  e <- expect_error(dta_check_inflated_size(gz), class = "dta_container_too_large")
  expect_identical(e$limit, "DTAtools.container_max_member_bytes")
  withr::local_options(DTAtools.container_max_member_bytes = 5e6)
  expect_no_error(dta_check_inflated_size(gz))
  plain <- withr::local_tempfile(fileext = ".csv")
  writeBin(rep(charToRaw("a,b\n"), 100000), plain)
  withr::local_options(DTAtools.container_max_member_bytes = 10)
  expect_no_error(dta_check_inflated_size(plain))
  expect_no_error(dta_check_inflated_size(file.path(tempdir(), "no-such.csv.gz")))
})

test_that("load_file() refuses a loose .csv.gz that inflates past the cap, and loads a small one", {
  dta <- dta_of(tab_ds("d", list(DTAFileCSV("data.csv.gz")), DTAColumnSpecCollection(columns = list())))
  big <- ub_gz(rep(charToRaw("a,b\n1,2\n"), 131072))
  small <- ub_gz(charToRaw("a,b\n1,2\n3,4\n"))
  withr::local_options(DTAtools.container_max_member_bytes = 1e5)
  expect_error(
    load_file(dta, dataset = "d", file = big, handler_index = 1, name = "data"),
    class = "dta_container_too_large"
  )
  out <- load_file(dta, dataset = "d", file = small, handler_index = 1, name = "data")
  expect_equal(nrow(as.data.frame(tables(datasets(out, "d"))[["data"]])), 2L)
})

test_that("dta_load_file() applies the app's limits for its call only", {
  dta <- dta_of(tab_ds("d", list(DTAFileCSV("data.csv.gz")), DTAColumnSpecCollection(columns = list())))
  big <- ub_gz(rep(charToRaw("a,b\n1,2\n"), 131072))
  withr::local_options(
    DTAtools.container_max_bytes = NULL, DTAtools.container_max_member_bytes = 1e5
  )
  res <- app_fn("dta_load_file")(dta, "d", big, 1L, name = "data", display_name = "data.csv.gz")
  expect_false(res$ok)
  expect_false(grepl("options(", res$error, fixed = TRUE))
  expect_match(res$error, "this application accepts", fixed = TRUE)
  expect_null(getOption("DTAtools.container_max_bytes"))
  expect_identical(getOption("DTAtools.container_max_member_bytes"), 1e5)
  expect_identical(app_fn("dta_app_container_options")()$DTAtools.container_max_bytes, 1e10)
})

ub_xpt_bytes <- function(inflated = 4000240) {
  marker <- charToRaw("HEADER RECORD*******LIBRARY HEADER RECORD!!!!!!!")
  head <- c(marker, rep(as.raw(0x20), 240 - length(marker)))
  c(head, raw(inflated - 240))
}

test_that("the transport scan caps the inflated bytes of gzip, bzip2 and xz alike, and not a plain file", {
  bytes <- ub_xpt_bytes()
  dir <- withr::local_tempdir()
  gz <- file.path(dir, "a.xpt")
  bz <- file.path(dir, "b.xpt")
  xz <- file.path(dir, "c.xpt")
  plain <- file.path(dir, "d.xpt")
  for (spec in list(list(gz, gzfile), list(bz, bzfile), list(xz, xzfile))) {
    con <- spec[[2]](spec[[1]], "wb")
    writeBin(bytes, con)
    close(con)
  }
  writeBin(bytes, plain)
  limits <- list(max_members = 1e4, max_bytes = 1e6, max_member_bytes = 1e13, max_ratio = 1100)
  for (p in c(gz, bz, xz)) {
    e <- expect_error(dta_xpt_members(p, limits), class = "dta_container_too_large")
    expect_identical(e$limit, "DTAtools.container_max_bytes")
  }
  # A plain file cannot inflate past its own size, so the cap does not touch it.
  expect_no_error(dta_xpt_members(plain, limits))
})

test_that("a slot-path transport read runs under the app's cap", {
  bytes <- ub_xpt_bytes()
  xz <- file.path(withr::local_tempdir(), "a.xpt")
  con <- xzfile(xz, "wb")
  writeBin(bytes, con)
  close(con)
  dta <- dta_of(tab_ds("d", list(DTAFileXPT("a.xpt")), DTAColumnSpecCollection(columns = list())))
  withr::local_options(DTAtools.container_max_bytes = 1e6)
  res <- app_fn("dta_load_file")(dta, "d", xz, 1L, name = "a")
  expect_false(res$ok)
  expect_match(res$error, "this application accepts", fixed = TRUE)
})

# ---- transcoder bounds -----------------------------------------------------

test_that("a newline-free stretch past 64 blocks aborts the conversion and leaves no copy", {
  withr::local_options(DTAtools.transcode_block_bytes = 64L)
  gz <- ub_gz(rep(charToRaw("a"), 20000), "nl.csv.gz")
  before <- list.files(tempdir(), pattern = "\\.csv$")
  expect_error(dta_transcode_to_utf8(gz, "latin1"), class = "dta_transcode_line_too_long")
  expect_identical(list.files(tempdir(), pattern = "\\.csv$"), before)
  # A line under the bound is converted.
  ok <- ub_gz(c(rep(charToRaw("a"), 3000), as.raw(0x0a)), "ok.csv.gz")
  out <- dta_transcode_to_utf8(ok, "latin1")
  expect_identical(file.size(out), 3001)
})

test_that("a converted copy past the byte cap aborts and is deleted", {
  withr::local_options(DTAtools.transcode_block_bytes = 64L, DTAtools.container_max_bytes = 1000)
  gz <- ub_gz(rep(charToRaw("a,b\n"), 5000), "cap.csv.gz")
  before <- list.files(tempdir(), pattern = "\\.csv$")
  e <- expect_error(dta_transcode_to_utf8(gz, "latin1"), class = "dta_container_too_large")
  expect_identical(e$limit, "DTAtools.container_max_bytes")
  expect_identical(list.files(tempdir(), pattern = "\\.csv$"), before)
})

# ---- the autosave slot -----------------------------------------------------

test_that("with session$user set, a slot is restored only for that user", {
  unlink(list.files(tempdir(), pattern = "^dtatools_app_session.*\\.rds$", full.names = TRUE), force = TRUE)
  id <- strrep("e", 32)
  n <- NULL
  shiny::testServer(.shiny_app_dir(), {
    session$user <- "alice"
    session$setInputs(dta_client_id = id)
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    n <<- length(DTAtools::datasets(rv$dta))
    expect_gt(n, 0L)
  })
  slots <- list.files(tempdir(), pattern = "^dtatools_app_session.*\\.rds$")
  expect_length(slots, 1L)
  expect_false(identical(slots, paste0("dtatools_app_session_", id, ".rds")))

  shiny::testServer(.shiny_app_dir(), {
    session$user <- "bob"
    session$setInputs(dta_client_id = id)
    session$setInputs(restore_session = 1)
    expect_null(rv$dta)
  })
  expect_false(file.exists(file.path(tempdir(), paste0("dtatools_app_session_", id, ".rds"))))

  shiny::testServer(.shiny_app_dir(), {
    session$user <- "alice"
    session$setInputs(dta_client_id = id)
    session$setInputs(restore_session = 1)
    expect_false(is.null(rv$dta))
  })
  unlink(list.files(tempdir(), pattern = "^dtatools_app_session.*\\.rds$", full.names = TRUE), force = TRUE)
})
