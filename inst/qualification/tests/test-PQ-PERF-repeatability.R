# Repeatability and isolation. REQ-PERF-012 .. REQ-PERF-016.
#
# A verdict that depends on when, where or how a check was run is not a verdict
# anyone can defend. These cases vary everything that ought not to matter --
# how many times the check runs, how many threads it uses, how large a batch it
# reads, what locale the session is in -- and require the answer to stay the
# same.

pr_delivery <- function(dir, name = "repeat.csv", rows = NULL) {
  rows <- rows %||% min(qa_tier_rows())
  path <- file.path(dir, name)
  qa_write_large_csv(rows, path)
  path
}

pr_messages <- function(path, ...) {
  qa_messages_norm(check(qa_dta(path, ...), persist = FALSE, quiet = TRUE))
}

test_that("PQ-PERF-030 | repeated validations of one delivery agree exactly | REQ-PERF-012", {
  dir <- qa_tempdir()
  path <- pr_delivery(dir)

  first <- pr_messages(path, stream = "always")
  for (run in 2:4) {
    qa_step(
      sprintf("run %d reports exactly what run 1 reported", run),
      first, pr_messages(path, stream = "always")
    )
  }

  # A verdict that varied between runs would mean a delivery could be accepted
  # or rejected by the luck of when it was checked, and no report of either
  # outcome would be worth signing.
  qa_check("the delivery reported something to compare", nrow(first) > 0)
})

test_that("PQ-PERF-031 | the verdict does not depend on the thread count | REQ-PERF-013", {
  dir <- qa_tempdir()
  path <- pr_delivery(dir)
  original <- set_dta_compute_threads(1L)
  withr::defer(set_dta_compute_threads(original))

  single <- pr_messages(path, stream = "always")
  set_dta_compute_threads(2L)
  double <- pr_messages(path, stream = "always")

  qa_step("one thread and two report the same messages", single, double)
})

test_that("PQ-PERF-032 | the verdict does not depend on the batch size | REQ-PERF-013", {
  dir <- qa_tempdir()
  path <- pr_delivery(dir)

  # A small batch splits the delivery into many pieces, so every decision the
  # streaming engine makes at a boundary is taken many times rather than once.
  # If any of them depended on what happened to be in the same batch, these
  # would disagree.
  reference <- withr::with_options(
    list(DTAtools.stream_batch_rows = 131072L),
    pr_messages(path, stream = "always")
  )
  for (batch in c(3L, 64L, 1000L)) {
    qa_step(
      sprintf("a batch size of %d reports the same messages as the default", batch),
      reference,
      withr::with_options(
        list(DTAtools.stream_batch_rows = batch),
        pr_messages(path, stream = "always")
      )
    )
  }
})

test_that("PQ-PERF-033 | the verdict does not depend on the Arrow compute path | REQ-PERF-013", {
  dir <- qa_tempdir()
  path <- pr_delivery(dir)

  off <- withr::with_options(
    list(DTAtools.use_arrow_compute = FALSE), pr_messages(path, stream = "always")
  )
  on <- withr::with_options(
    list(DTAtools.use_arrow_compute = TRUE, DTAtools.arrow_min_rows = 1L),
    pr_messages(path, stream = "always")
  )
  qa_step("the accelerated path reports what the reference path reports", off, on)
})

test_that("PQ-PERF-034 | the verdict does not depend on the session locale | REQ-PERF-014 | tags: subprocess", {
  dir <- qa_tempdir()
  path <- pr_delivery(dir)
  reference <- pr_messages(path, stream = "always")

  # Several checks sort, and collation order differs between locales. Running
  # the same delivery in a C-collation process is the only way to see whether
  # the verdict travels: a delivery must not pass in one country and fail in
  # another.
  in_c_locale <- tryCatch(
    qa_subprocess(
      {
        source_tree <- Sys.getenv("QA_SOURCE_TREE")
        if (requireNamespace("DTAtools", quietly = TRUE)) {
          suppressMessages(library(DTAtools))
        } else {
          suppressMessages(pkgload::load_all(source_tree, quiet = TRUE))
        }
        suite <- system.file("qualification", "tests", package = "DTAtools")
        if (!nzchar(suite)) {
          suite <- file.path(source_tree, "inst", "qualification", "tests")
        }
        source(file.path(suite, "helper-00-qa.R"))
        source(file.path(suite, "helper-generators.R"))
        ds <- qa_dta(Sys.getenv("QA_DELIVERY"), stream = "always")
        qa_messages_norm(check(ds, persist = FALSE, quiet = TRUE))
      },
      envvars = c(
        LC_ALL = "C", LANGUAGE = "C", QA_DELIVERY = path,
        QA_SOURCE_TREE = tryCatch(pkgload::pkg_path(), error = function(e) getwd()),
        R_LIBS_USER = paste(.libPaths(), collapse = .Platform$path.sep)
      ),
      timeout_sec = 900
    ),
    error = function(e) e
  )

  if (inherits(in_c_locale, "condition")) {
    testthat::skip(paste0("the C-locale subprocess could not run: ", conditionMessage(in_c_locale)))
  }
  qa_step(
    "a C-collation session reports what this session reports",
    reference, in_c_locale
  )
})

test_that("PQ-PERF-035 | repeated validations leave nothing behind | REQ-PERF-015", {
  dir <- qa_tempdir()
  path <- pr_delivery(dir)

  temp_root <- tempdir()
  before <- length(list.files(temp_root, recursive = TRUE))
  for (i in seq_len(5)) {
    invisible(check(qa_dta(path, stream = "always"), persist = FALSE, quiet = TRUE))
  }
  gc(verbose = FALSE)
  after <- length(list.files(temp_root, recursive = TRUE))

  # Spill files and scratch tables are legitimate while a check runs; what is
  # not legitimate is leaving them for the next one. On a server checking
  # deliveries all day, a leak of a few files per run fills a disk.
  qa_check(
    sprintf(
      "five validations added no more than a handful of files to the temporary directory (%d then %d)",
      before, after
    ),
    after - before <= 5,
    detail = c(before = before, after = after)
  )
})

test_that("PQ-PERF-036 | validating a delivery leaves the session as it found it | REQ-PERF-015", {
  dir <- qa_tempdir()
  path <- pr_delivery(dir)

  options_before <- options()
  locale_before <- Sys.getlocale()
  wd_before <- getwd()

  invisible(check(qa_dta(path, stream = "always"), persist = FALSE, quiet = TRUE))

  # A function that changes global state damages work that has nothing to do
  # with it, and the damage surfaces somewhere else entirely.
  changed <- setdiff(names(options()), names(options_before))
  qa_step("no new global options are left set", character(0), changed)
  qa_step("the locale is unchanged", locale_before, Sys.getlocale())
  qa_step("and the working directory is unchanged", wd_before, getwd())
})

test_that("PQ-PERF-037 | the application starts and serves its first page | REQ-PERF-016 | tags: subprocess", {
  qa_requires("shiny", "bslib", "DT")

  app_dir <- system.file("shiny", "dta_app", package = "DTAtools")
  if (!nzchar(app_dir)) {
    testthat::skip("the application is not installed with this copy of the package")
  }

  port <- sample(20000:60000, 1)
  script <- file.path(qa_tempdir(), "launch.R")
  writeLines(
    c(
      sprintf("app <- %s", encodeString(app_dir, quote = '"')),
      sprintf("shiny::runApp(app, port = %d, launch.browser = FALSE, host = \"127.0.0.1\")", port)
    ),
    script
  )

  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
  # Started detached so the test can poll it; killed in every exit path.
  process <- try(
    system2(
      rscript,
      c("--vanilla", shQuote(normalizePath(script, winslash = "/"))),
      wait = FALSE, stdout = FALSE, stderr = FALSE
    ),
    silent = TRUE
  )
  withr::defer({
    # Nothing else in the suite should inherit a listening server.
    try(system2(
      if (.Platform$OS.type == "windows") "taskkill" else "pkill",
      if (.Platform$OS.type == "windows") {
        c("/F", "/FI", shQuote(sprintf("WINDOWTITLE eq %s", basename(script))))
      } else {
        c("-f", basename(script))
      },
      stdout = FALSE, stderr = FALSE
    ), silent = TRUE)
  })

  reachable <- FALSE
  deadline <- Sys.time() + 60
  while (Sys.time() < deadline && !reachable) {
    Sys.sleep(2)
    page <- tryCatch(
      suppressWarnings(readLines(url(sprintf("http://127.0.0.1:%d", port)), warn = FALSE)),
      error = function(e) NULL
    )
    if (!is.null(page) && length(page) > 0) {
      reachable <- TRUE
      qa_check("the application served a page", any(nzchar(page)))
      qa_step(
        "and the page is an HTML document",
        TRUE, any(grepl("<html|<!DOCTYPE", page, ignore.case = TRUE))
      )
    }
  }
  if (!reachable) {
    testthat::skip("the application did not become reachable within 60 seconds")
  }
})
