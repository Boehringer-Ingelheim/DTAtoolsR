# The upload boundary of the Shiny app: which paths the server may read, where an
# upload is staged, how much a compressed upload may inflate, and which
# browser the autosave belongs to. Dummy data only; sizes are a few KB.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

# A stand-in for a fileInput value: a one-row data frame.
ub_input <- function(path, name = basename(path)) {
  data.frame(name = name, size = file.size(path), type = "", datapath = path, stringsAsFactors = FALSE)
}

ub_upload_dirs <- function() list.files(tempdir(), pattern = "^dta_upload_")

# ---- dta_stage_upload() ----------------------------------------------------

test_that("staging an upload named '..' or '.' refuses it and writes nothing to tempdir()", {
  stage <- app_fn("dta_stage_upload")
  src <- withr::local_tempfile()
  writeLines(c("a,b", "1,2"), src)
  before <- sort(list.files(tempdir(), all.files = TRUE, no.. = TRUE))
  expect_error(stage(src, ".."), class = "dta_stage_failed")
  expect_error(stage(src, "."), class = "dta_stage_failed")
  expect_identical(sort(list.files(tempdir(), all.files = TRUE, no.. = TRUE)), before)
})

test_that("staging refuses a directory as the source instead of returning it", {
  stage <- app_fn("dta_stage_upload")
  d <- withr::local_tempdir()
  dir.create(file.path(d, "aa"))
  writeLines("x,y", file.path(d, "aa", "0.csv"))
  before <- ub_upload_dirs()
  expect_error(stage(d, "data.csv"), class = "dta_stage_failed")
  expect_identical(ub_upload_dirs(), before)
})

test_that("staging an upload gives a regular file named by the upload in a directory of its own", {
  stage <- app_fn("dta_stage_upload")
  src <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("a,b", "1,2"), src)
  out <- stage(src, "data.csv")
  withr::defer(unlink(dirname(out), recursive = TRUE))
  expect_identical(basename(out), "data.csv")
  expect_identical(readLines(out), c("a,b", "1,2"))
  expect_false(identical(normalizePath(dirname(out)), normalizePath(tempdir())))
  expect_identical(
    normalizePath(dirname(dirname(out)), winslash = "/"),
    normalizePath(tempdir(), winslash = "/")
  )
})

# ---- which datapaths a session trusts --------------------------------------

test_that("a datapath the client typed is refused, one it did not is trusted", {
  track <- app_fn("dta_track_client_paths")
  trusted <- app_fn("dta_upload_trusted")
  got <- NULL
  session <- list(onInputReceived = function(cb) got <<- cb)
  seen <- track(session, initial = list(dta_file = data.frame(name = "x", datapath = "/early/forged")))
  expect_true(is.function(got))

  real <- withr::local_tempfile(fileext = ".yaml")
  writeLines("a: 1", real)
  forged <- withr::local_tempfile(fileext = ".yaml")
  writeLines("b: 2", forged)

  expect_true(trusted(ub_input(real), seen))
  got(list(dta_file = list(name = "x.yaml", datapath = forged)))
  expect_false(trusted(ub_input(forged), seen))
  expect_true(trusted(ub_input(real), seen))
  # Nested and data-frame shaped values are noted as well.
  got(list(up_1_1 = data.frame(name = "x", datapath = real)))
  expect_false(trusted(ub_input(real), seen))
})

test_that("a URL, a missing file, a directory, a list and a many-row value are not trusted", {
  trusted <- app_fn("dta_upload_trusted")
  seen <- app_fn("dta_track_client_paths")(list(onInputReceived = function(cb) NULL))
  real <- withr::local_tempfile()
  writeLines("a", real)
  expect_false(trusted(data.frame(name = "x", datapath = "http://127.0.0.1:1/x.yaml"), seen))
  expect_false(trusted(data.frame(name = "x", datapath = file.path(tempdir(), "no-such-file")), seen))
  expect_false(trusted(data.frame(name = "x", datapath = tempdir()), seen))
  expect_false(trusted(list(name = "x", datapath = real), seen))
  expect_false(trusted(NULL, seen))
  two <- data.frame(name = c("a", "b"), datapath = c(real, real))
  expect_true(trusted(two, seen))
  expect_false(trusted(two, seen, one = TRUE))
})

test_that("a session that cannot say where its inputs came from trusts nothing", {
  seen <- app_fn("dta_track_client_paths")(list())
  real <- withr::local_tempfile()
  writeLines("a", real)
  expect_false(app_fn("dta_upload_trusted")(ub_input(real), seen))
})

test_that("shiny reports a client message to onInputReceived() but never a real upload", {
  skip_if_not(exists("ShinySession", envir = asNamespace("shiny")))
  ws <- list(request = new.env(), send = function(x) NULL, close = function() NULL)
  s <- tryCatch(shiny:::ShinySession$new(ws), error = function(e) NULL)
  skip_if(is.null(s), "no stand-in shiny session on this shiny version")
  seen <- app_fn("dta_track_client_paths")(s)

  # A forged value, as a websocket message delivers it.
  victim <- withr::local_tempfile(fileext = ".yaml")
  writeLines("a: 1", victim)
  s$manageInputs(list(dta_file = list(name = "x.yaml", size = 1, type = "", datapath = victim)), now = TRUE)
  expect_false(app_fn("dta_upload_trusted")(ub_input(victim), seen))

  # A real upload: init, POST, end -- the server stores the file and sets the id.
  job <- s[["@uploadInit"]](list(list(name = "y.yaml", size = 5, type = "")))
  body <- list(charToRaw("b: 2\n"), raw(0))
  req <- list(
    PATH_INFO = paste0("/upload/", job$jobId), REQUEST_METHOD = "POST",
    HTTP_SHINY_FILE_NAME = "y.yaml", HTTP_SHINY_FILE_TYPE = "", CONTENT_LENGTH = "5",
    rook.input = list(read = function(n) {
      out <- body[[1]]
      body <<- body[-1]
      if (is.null(out)) raw(0) else out
    })
  )
  s$handleRequest(req)
  shiny::withReactiveDomain(s, s[["@uploadEnd"]](job$jobId, "dta_file"))
  got <- shiny::isolate(s$input$dta_file)
  expect_true(is.data.frame(got))
  expect_identical(readLines(got$datapath), "b: 2")
  expect_true(app_fn("dta_upload_trusted")(got, seen))
})

# ---- the consumers ---------------------------------------------------------

test_that("a forged dta_file value is neither read nor shown back", {
  secret <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("SUBJID,AGE", "DUMMY-001,42"), secret)
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_client_id = strrep("b", 32))
    # An untagged websocket value stays a list; a URL is not a path.
    session$setInputs(dta_file = list(name = "x.yaml", size = 1, type = "", datapath = secret))
    expect_null(rv$load_failure)
    expect_null(rv$dta)
    session$setInputs(dta_file = data.frame(name = "x.yaml", size = 1, type = "", datapath = "http://127.0.0.1:1/x.yaml"))
    expect_null(rv$load_failure)
    # A data frame whose path the client typed is refused as well.
    assign(rlang::hash(secret), TRUE, envir = client_paths)
    session$setInputs(dta_file = ub_input(secret, "x.yaml"))
    expect_null(rv$load_failure)
    expect_null(rv$dta)
  })
})

test_that("a forged tmpl_carry_file is dropped", {
  p <- app_fixture_path("clinical_dta.yaml")
  shiny::testServer(.shiny_app_dir(), {
    assign(rlang::hash(p), TRUE, envir = client_paths)
    session$setInputs(tmpl_carry_file = ub_input(p))
    expect_null(rv$tmpl_carry_upload)
  })
})

test_that("a forged delivery_files and slot upload stage nothing", {
  p <- app_fixture_path("clinical_data.csv")
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_client_id = strrep("c", 32))
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    assign(rlang::hash(p), TRUE, envir = client_paths)
    before <- ub_upload_dirs()
    session$setInputs(delivery_files = ub_input(p))
    session$setInputs(up_1_1 = ub_input(p))
    expect_identical(ub_upload_dirs(), before)
  })
})

test_that("an upload the session did receive still loads", {
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_client_id = strrep("d", 32))
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    expect_false(is.null(rv$dta))
  })
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
