# run_dta_app() is the package's only exported entry point into the Shiny app.
# It cannot be called in a test (it blocks in shiny::runApp()), so these tests
# cover its contract: the signature it promises, the directory it resolves, and
# the dependency promises its error message makes.

test_that("run_dta_app() keeps its documented signature", {
  fmls <- formals(run_dta_app)

  # Documented defaults in R/run_dta_app.R — a silent change here would break
  # callers that rely on the app opening a browser by default.
  expect_equal(names(fmls), c("launch.browser", "port", "..."))
  expect_true(eval(fmls$launch.browser))
  expect_null(eval(fmls$port))
})

test_that("run_dta_app() resolves the app directory that is actually shipped", {
  # Same resolution run_dta_app() performs at R/run_dta_app.R:41.
  app_dir <- system.file("shiny", "dta_app", package = "DTAtools")

  expect_true(nzchar(app_dir))
  expect_true(dir.exists(app_dir))
  expect_identical(normalizePath(app_dir), normalizePath(.shiny_app_dir()))
  # shiny::runApp() needs app.R at the root of that directory.
  expect_true(file.exists(file.path(app_dir, "app.R")))
})

test_that("the packages run_dta_app() hard-requires are declared in DESCRIPTION", {
  # run_dta_app() aborts with an "install these" message naming exactly these
  # three (R/run_dta_app.R:28). If one were dropped from DESCRIPTION, the app
  # would promise a dependency the package never declares.
  required <- c("shiny", "bslib", "DT")

  desc <- read.dcf(system.file("DESCRIPTION", package = "DTAtools"))
  declared <- unlist(strsplit(
    paste(desc[, intersect(colnames(desc), c("Imports", "Suggests", "Depends"))],
      collapse = ","
    ),
    ","
  ))
  # Strip version constraints and whitespace: "shiny (>= 1.6)" -> "shiny".
  declared <- trimws(sub("\\(.*\\)", "", declared))
  declared <- declared[nzchar(declared)]

  expect_true(all(required %in% declared))
})

test_that("every package the app calls is available at run time", {
  app_files <- c(
    file.path(.shiny_app_dir(), "app.R"),
    .shiny_app_helper_files()
  )
  code <- unlist(lapply(app_files, readLines, warn = FALSE))

  # Every `pkg::` prefix the app code uses.
  hits <- regmatches(code, gregexpr("\\b[A-Za-z][A-Za-z0-9.]*(?=::)", code, perl = TRUE))
  used <- setdiff(sort(unique(unlist(hits))), "DTAtools")
  expect_gt(length(used), 5)

  # The app calls these unqualified by any requireNamespace() guard in some
  # paths, so a missing one is a run-time error in the user's face. This catches
  # a typo'd or removed dependency, which no other test in the suite would.
  missing <- used[!vapply(used, requireNamespace, logical(1), quietly = TRUE)]
  expect_equal(missing, character(0))
})
