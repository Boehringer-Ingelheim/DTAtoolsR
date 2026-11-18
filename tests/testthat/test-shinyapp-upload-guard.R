# The app opens only files that were uploaded in the session. A browser can set
# a file input's value itself (Shiny.setInputValue), so the datapath in such a
# value must never be used as a server path. These tests pin the helpers and
# the four observers that guard it.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

# A directory directly under tempdir(), removed when the calling test ends.
ug_dir <- function(name, env = parent.frame()) {
  dir <- file.path(tempdir(), name)
  dir.create(dir, recursive = TRUE)
  withr::defer(unlink(dir, recursive = TRUE), envir = env)
  dir
}

# A fresh upload-shaped folder (<tempdir()>/<24 hex characters>) from the shared
# counter in helper-shinyapp.R, removed when the calling test ends.
ug_folder <- function(env = parent.frame()) {
  dir <- app_upload_folder()
  withr::defer(unlink(dir, recursive = TRUE), envir = env)
  dir
}

ug_value <- function(path, name = basename(path)) {
  data.frame(name = name, size = 1, type = "", datapath = path, stringsAsFactors = FALSE)
}

ug_csv <- function() app_fixture_path("clinical_data.csv")

test_that("dta_is_file_input_id() accepts the four id forms only", {
  is_id <- app_fn("dta_is_file_input_id")
  expect_true(all(is_id(c("dta_file", "tmpl_carry_file", "delivery_files", "up_1_1", "up_12_3"))))
  expect_false(any(is_id(c("dta_client_id", "up_1", "up_a_1", "xdta_file", "up_1_1x", NA_character_))))
  expect_identical(is_id(NULL), logical(0))
  expect_identical(is_id(c("dta_file", "x")), c(TRUE, FALSE))
  expect_false(is_id(1))
})

test_that("dta_upload_guard() marks client-written file inputs and consumes each mark once", {
  guard <- app_fn("dta_upload_guard")()
  expect_false(guard$take("dta_file"))

  guard$note(list(dta_file = list(datapath = "x")))
  expect_true(guard$take("dta_file"))
  expect_false(guard$take("dta_file"))

  # The ":type" suffix is stripped, and marks are per id.
  guard$note(list("up_1_1:DT.cellInfo" = list(1)))
  expect_false(guard$take("up_1_2"))
  expect_true(guard$take("up_1_1"))

  # A NULL value, a non-file id, and degenerate input never mark or fail.
  guard$note(list(dta_file = NULL, delivery_files = NULL))
  guard$note(list(dta_client_id = "abc", up_1 = "x"))
  expect_false(guard$take("dta_file"))
  expect_false(guard$take("delivery_files"))
  expect_false(guard$take("dta_client_id"))
  expect_null(guard$note(NULL))
  expect_null(guard$note(list()))
  expect_null(guard$note(list(1, 2)))
  expect_null(guard$note("dta_file"))
})

test_that("dta_upload_value_problem() accepts upload-shaped values and refuses everything else", {
  problem <- app_fn("dta_upload_value_problem")
  csv <- ug_csv()

  # Each refusal is pinned to its own reason, so a case cannot pass for the
  # wrong one.
  outside <- "not directly inside an upload directory"
  expect_null(problem(app_upload_value(csv, "clinical_data.csv")))
  expect_null(problem(app_upload_value(c(csv, csv), c("a.csv", "b.csv"))))

  good <- app_upload_value(csv, "clinical_data.csv")
  expect_match(problem(as.list(good)), "not a data frame", fixed = TRUE)
  expect_match(problem(good[0, ]), "no rows", fixed = TRUE)
  expect_match(problem(ug_value(csv)), outside, fixed = TRUE)
  expect_match(problem(ug_value(tempdir())), "not a file that exists and is not a directory", fixed = TRUE)

  gone <- ug_folder()
  unlink(gone, recursive = TRUE)
  expect_match(problem(ug_value(file.path(gone, "0.csv"))), "not a file that exists and is not a directory", fixed = TRUE)

  wrong_file <- file.path(ug_folder(), "data.csv")
  expect_true(file.copy(csv, wrong_file))
  expect_match(problem(ug_value(wrong_file)), "file name has the wrong shape", fixed = TRUE)

  wrong_dir <- file.path(ug_dir("notanupload"), "0.csv")
  expect_true(file.copy(csv, wrong_dir))
  expect_match(problem(ug_value(wrong_dir)), "upload directory name has the wrong shape", fixed = TRUE)

  deep_dir <- file.path(ug_folder(), "sub")
  dir.create(deep_dir)
  deep <- file.path(deep_dir, "0.csv")
  expect_true(file.copy(csv, deep))
  expect_match(problem(ug_value(deep)), outside, fixed = TRUE)

  expect_match(problem(ug_value(NA_character_, "x.csv")), "column 'datapath'", fixed = TRUE)
  expect_match(problem(ug_value("", "x.csv")), "column 'datapath'", fixed = TRUE)

  one_bad <- rbind(good, ug_value(csv))
  expect_match(problem(one_bad), outside, fixed = TRUE)
  expect_null(problem(good))
})

test_that("dta_upload_guard_install() seeds the guard and registers its note() as the callback", {
  install <- app_fn("dta_upload_guard_install")
  make <- app_fn("dta_upload_guard")

  registered <- NULL
  session <- structure(list(onInputReceived = function(cb) registered <<- cb), class = "FakeSession")
  guard <- make()
  expect_identical(install(guard, session, list()), guard)
  expect_true(is.function(registered))
  registered(list(dta_file = list(datapath = "x")))
  expect_true(guard$take("dta_file"))

  # Inputs already present when the server function starts are marked, and only
  # the file inputs among them.
  seeded <- make()
  install(seeded, session, list(up_1_1 = data.frame(datapath = "x"), other = 1))
  expect_true(seeded$take("up_1_1"))
  expect_false(seeded$take("other"))

  # No callback: a real session must fail loudly, the mock session has none.
  bare <- structure(list(), class = "FakeSession")
  expect_error(install(make(), bare, list()), regexp = "upload guard cannot run")
  mock <- structure(list(), class = "MockShinySession")
  expect_no_error(install(make(), mock, list()))
})

# CANARY. The upload guard rests on how Shiny writes a client message and an
# upload into the input store: only a client message passes through
# inputReceivedCallbacks, and it does so before the values are written; an
# upload is written by @uploadEnd without it. If a Shiny upgrade changes either,
# this fails, and the guard must be re-examined before the upgrade ships.
test_that("Shiny still reports client input messages and uploads the way the guard relies on", {
  skip_if_not_installed("shiny")
  expect_false("onInputReceived" %in% names(shiny::MockShinySession$public_methods))

  session_class <- get("ShinySession", envir = asNamespace("shiny"))
  methods <- session_class$public_methods
  expect_true("onInputReceived" %in% names(methods))

  manage <- paste(deparse(body(methods$manageInputs)), collapse = "\n")
  at_callbacks <- regexpr("inputReceivedCallbacks$invoke", manage, fixed = TRUE)
  at_write <- regexpr(".input$mset", manage, fixed = TRUE)
  expect_gt(at_callbacks, 0)
  expect_gt(at_write, 0)
  expect_lt(at_callbacks, at_write)

  upload_end <- paste(deparse(body(methods$`@uploadEnd`)), collapse = "\n")
  expect_true(grepl(".input$set", upload_end, fixed = TRUE))
  expect_false(grepl("inputReceivedCallbacks", upload_end, fixed = TRUE))
  expect_false(grepl("manageInputs", upload_end, fixed = TRUE))
})

test_that("the server installs the guard once and guards each of the four file inputs once", {
  src <- app_source("app.R")
  count <- function(x) lengths(regmatches(src, gregexpr(x, src, fixed = TRUE)))
  expect_identical(count("dta_upload_guard_install("), 1L)
  # The whole call: a guard installed without the inputs already present, or on
  # a different session, would leave a client-written value unmarked.
  expect_true(grepl(
    "dta_upload_guard_install\\(\\s*dta_upload_guard\\(\\),\\s*session,\\s*isolate\\(reactiveValuesToList\\(input\\)\\)\\s*\\)",
    src,
    perl = TRUE
  ))
  for (call in c(
    'accept_upload("dta_file"', 'accept_upload("tmpl_carry_file"',
    "accept_upload(UP", 'accept_upload("delivery_files"'
  )) {
    expect_identical(count(call), 1L, info = call)
  }
})

test_that("dta_stage_upload() keeps the copy in a fresh directory and refuses non-files", {
  stage <- app_fn("dta_stage_upload")
  src <- app_upload_value(ug_csv(), "clinical_data.csv")$datapath

  staged <- stage(src, "clinical_data.csv")
  expect_identical(basename(staged), "clinical_data.csv")
  expect_match(basename(dirname(staged)), "^dta_upload_")
  expect_identical(readLines(staged), readLines(ug_csv()))

  for (nm in c("..", ".")) {
    before <- setdiff(list.files(tempdir(), all.files = TRUE), c(".", ".."))
    staged <- stage(src, nm)
    after <- setdiff(list.files(tempdir(), all.files = TRUE), c(".", ".."))
    expect_identical(basename(staged), basename(src))
    expect_match(basename(dirname(staged)), "^dta_upload_")
    expect_identical(
      normalizePath(dirname(dirname(staged)), winslash = "/"),
      normalizePath(dirname(dirname(src)), winslash = "/")
    )
    new_entries <- setdiff(after, before)
    expect_true(all(grepl("^dta_upload_", new_entries)))
  }

  expect_identical(stage(tempdir(), "x.csv"), NA_character_)
  expect_identical(stage(file.path(tempdir(), "no-such-file.csv"), "x.csv"), NA_character_)
  expect_identical(stage(NA_character_, "x.csv"), NA_character_)
  expect_identical(stage(c(src, src), "x.csv"), NA_character_)
})

test_that("dta_stage_upload() leaves no folder behind when it returns NA", {
  stage <- app_fn("dta_stage_upload")
  tmp <- ug_dir("stage_cleanup")
  dir <- ug_folder()
  dir.create(file.path(dir, "0.csv"))

  expect_identical(stage(file.path(dir, "0.csv"), "x.csv", tmpdir = tmp), NA_character_)
  expect_identical(stage(dir, "x.csv", tmpdir = tmp), NA_character_)

  # These two fail after the folder exists: a 300-byte name cannot be created on
  # any OS, and a vector of names is an error inside the copy step.
  src <- app_upload_value(ug_csv(), "x.csv")$datapath
  expect_identical(stage(src, strrep("a", 300), tmpdir = tmp), NA_character_)
  expect_identical(stage(src, c("a.csv", "b.csv"), tmpdir = tmp), NA_character_)
  expect_length(list.files(tmp, all.files = TRUE, no.. = TRUE), 0L)
})

# normalizePath() only canonicalises paths that exist, so a tempdir() spelled
# through a junction or symlink (8.3 names, macOS /var) must not make every
# upload fail the "stays inside its folder" comparison.
test_that("dta_stage_upload() works when the temporary directory is reached through an alias", {
  stage <- app_fn("dta_stage_upload")
  real <- file.path(tempdir(), "stage_real_dir")
  alias <- file.path(tempdir(), "stage_alias_dir")
  dir.create(real)
  withr::defer(unlink(real, recursive = TRUE))
  made <- tryCatch(
    if (.Platform$OS.type == "windows") Sys.junction(real, alias) else file.symlink(real, alias),
    error = function(e) FALSE, warning = function(w) FALSE
  )
  skip_if_not(isTRUE(made) && dir.exists(alias), "cannot create a junction or symlink here")
  # recursive = FALSE removes the link and leaves the target alone.
  withr::defer(unlink(alias, recursive = FALSE))

  src <- app_upload_value(ug_csv(), "clinical_data.csv")$datapath
  inside_real <- function(p) {
    identical(
      dirname(dirname(normalizePath(p, winslash = "/"))),
      normalizePath(real, winslash = "/")
    )
  }

  staged <- stage(src, "clinical_data.csv", tmpdir = alias)
  expect_identical(basename(staged), "clinical_data.csv")
  expect_identical(readLines(staged), readLines(ug_csv()))
  expect_true(inside_real(staged))

  before <- list.files(real, all.files = TRUE, no.. = TRUE)
  staged <- stage(src, "..", tmpdir = alias)
  expect_identical(basename(staged), basename(src))
  expect_match(basename(dirname(staged)), "^dta_upload_")
  expect_true(inside_real(staged))
  new_entries <- setdiff(list.files(real, all.files = TRUE, no.. = TRUE), before)
  expect_length(new_entries, 1L)
  expect_match(new_entries, "^dta_upload_")
  expect_true(all(dir.exists(file.path(real, new_entries))))

  unlink(alias, recursive = FALSE)
  expect_false(dir.exists(alias))
  expect_true(dir.exists(real))
  expect_true(file.exists(staged))
})

# What a refused value must leave alone: the document and every record of loads.
ug_state <- function(rv) {
  list(
    dta = rv$dta, uploads = rv$uploads, load_errors = rv$load_errors,
    load_failure = rv$load_failure
  )
}

test_that("a dta_file value that is not a session upload is refused before it is read", {
  yaml <- app_fixture_path("clinical_dta.yaml")
  forged <- list(name = "x.yaml", size = 1, type = "", datapath = yaml)
  outside <- ug_value(yaml, "x.yaml")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = forged)
    expect_null(rv$dta)
    expect_null(rv$load_failure)

    session$setInputs(dta_file = outside)
    expect_null(rv$dta)
    expect_null(rv$load_failure)

    session$setInputs(dta_file = ug_value(tempdir(), "x.yaml"))
    expect_null(rv$dta)
    expect_null(rv$load_failure)
  })
})

test_that("a slot value whose datapath is a directory binds nothing", {
  dir <- ug_folder()
  dir.create(file.path(dir, "0.csv"))

  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    expect_false(is.null(rv$dta))
    before <- ug_state(rv)

    session$setInputs(up_1_1 = ug_value(file.path(dir, "0.csv"), "clinical_data.csv"))
    session$setInputs(up_1_1 = ug_value(dir, "clinical_data.csv"))
    expect_identical(ug_state(rv), before)
  })
})

# The guarded observers refuse a directory before it gets here, so these call
# the handlers directly: the staging step is the second line of defence.
test_that("handle_upload() binds nothing and logs it when the datapath cannot be staged", {
  dir <- ug_folder()

  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    expect_length(rv$dta@datasets[[1]]@tables, 0L)
    expect_identical(nrow(rv$load_errors), 0L)

    handle_upload(1, 1, ug_value(dir, "clinical_data_dir.csv"))
    expect_length(rv$dta@datasets[[1]]@tables, 0L)
    expect_identical(nrow(rv$load_errors), 1L)
    expect_match(rv$load_errors$message, "could not be staged", fixed = TRUE)
  })
})

test_that("handle_delivery() loads nothing when no file of the drop can be staged", {
  dir <- ug_folder()

  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    before <- ug_state(rv)

    handle_delivery(ug_value(dir, "clinical_data_dir.csv"))
    expect_identical(ug_state(rv), before)
    expect_length(rv$dta@datasets[[1]]@tables, 0L)
  })
})

test_that("handle_delivery() binds the files that can be staged and reports the one that cannot", {
  dir <- ug_folder()
  good <- app_upload_value(ug_csv(), "clinical_data.csv")
  drop <- rbind(good, ug_value(dir, "clinical_data_dir.csv"))

  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    expect_length(rv$dta@datasets[[1]]@tables, 0L)

    handle_delivery(drop)
    expect_length(rv$dta@datasets[[1]]@tables, 1L)
    expect_identical(nrow(rv$dta@datasets[[1]]@tables[[1]]), 500L)
  })
})

test_that("a tmpl_carry_file or delivery_files value outside an upload folder is refused", {
  outside <- ug_value(ug_csv(), "clinical_data.csv")

  shiny::testServer(app_server_dir(), {
    session$setInputs(tmpl_carry_file = outside)
    expect_null(rv$tmpl_carry_upload)

    load_fixture(session)
    n_err <- nrow(rv$load_errors)
    session$setInputs(delivery_files = outside)
    expect_length(rv$dta@datasets[[1]]@tables, 0L)
    expect_identical(nrow(rv$load_errors), n_err)
  })
})

test_that("the client-channel mark refuses one value and is then consumed", {
  shiny::testServer(app_server_dir(), {
    upload_guard$note(list(dta_file = TRUE))
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    expect_null(rv$dta)

    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    expect_false(is.null(rv$dta))
  })
})

test_that("a dta_file mark is consumed by a value that req() would drop", {
  shiny::testServer(app_server_dir(), {
    upload_guard$note(list(dta_file = TRUE))
    session$setInputs(dta_file = "")
    expect_null(rv$dta)

    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    expect_false(is.null(rv$dta))
  })
})

test_that("a delivery_files mark is consumed even when no document is open", {
  shiny::testServer(app_server_dir(), {
    # The observer has ignoreInit = TRUE, which swallows the first value it sees
    # (mark or not), so prime it before marking.
    session$setInputs(delivery_files = app_file_input("clinical_data.csv"))
    upload_guard$note(list(delivery_files = TRUE))
    session$setInputs(delivery_files = app_file_input("clinical_data.csv"))
    expect_null(rv$dta)
    expect_false(upload_guard$take("delivery_files"))

    load_fixture(session)
    session$setInputs(delivery_files = app_upload_value(ug_csv(), "clinical_data.csv"))
    expect_length(rv$dta@datasets[[1]]@tables, 1L)
  })
})

test_that("a genuine slot upload still binds its table", {
  shiny::testServer(app_server_dir(), {
    load_fixture(session)
    session$setInputs(up_1_1 = app_file_input("clinical_data.csv"))
    expect_length(rv$dta@datasets[[1]]@tables, 1L)
    expect_identical(nrow(rv$dta@datasets[[1]]@tables[[1]]), 500L)
  })
})
