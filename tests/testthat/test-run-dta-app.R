# run_dta_app() is the package's only exported entry point into the Shiny app.
# It cannot be called in a test (it blocks in shiny::runApp()), so these tests
# cover its contract: the signature it promises, the directory it resolves, and
# the dependency promises its error message makes.

test_that("run_dta_app() keeps its documented signature", {
  fmls <- formals(run_dta_app)

  # Documented defaults in R/run_dta_app.R — a silent change here would break
  # callers that rely on the app opening a browser by default.
  expect_equal(names(fmls), c("launch.browser", "port", "restore_session", "..."))
  expect_true(eval(fmls$launch.browser))
  expect_null(eval(fmls$port))
  # Autosave/restore is on for the local single-user run this function performs,
  # and is what tells the app it may write session state to disk at all.
  expect_true(eval(fmls$restore_session))
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

# Every `pkg::` prefix used by the app's own code.
app_packages_used <- function() {
  code <- unlist(lapply(
    c(file.path(.shiny_app_dir(), "app.R"), .shiny_app_helper_files()),
    readLines,
    warn = FALSE
  ))
  hits <- regmatches(code, gregexpr("\\b[A-Za-z][A-Za-z0-9.]*(?=::)", code, perl = TRUE))
  list(code = code, used = setdiff(sort(unique(unlist(hits))), "DTAtools"))
}

# Package names declared anywhere in DESCRIPTION, minus version constraints.
declared_packages <- function() {
  desc <- read.dcf(system.file("DESCRIPTION", package = "DTAtools"))
  fields <- intersect(colnames(desc), c("Depends", "Imports", "Suggests"))
  out <- trimws(sub("\\(.*\\)", "", unlist(strsplit(paste(desc[, fields], collapse = ","), ","))))
  out[nzchar(out)]
}

test_that("every package the app calls is available at run time", {
  used <- app_packages_used()$used
  expect_gt(length(used), 5)

  # A typo'd or removed dependency is a run-time error in the user's face, and
  # no other test in the suite would notice: R CMD check does not scan inst/.
  missing <- used[!vapply(used, requireNamespace, logical(1), quietly = TRUE)]
  expect_equal(missing, character(0))
})

test_that("every non-base package the app calls is declared or guarded", {
  info <- app_packages_used()

  # Base/recommended-priority packages ship with R itself and need no
  # declaration (stats, tools, graphics, grDevices, utils, ...).
  base_pkgs <- rownames(installed.packages(priority = "base"))
  candidates <- setdiff(info$used, base_pkgs)
  expect_gt(length(candidates), 5)

  declared <- declared_packages()
  undeclared <- setdiff(candidates, declared)

  # An undeclared package is acceptable only when every use is behind a
  # requireNamespace() guard, so a user without it gets a handled fallback
  # rather than an error. Anything else must be in DESCRIPTION.
  unguarded <- undeclared[!vapply(
    undeclared,
    function(p) any(grepl(sprintf('requireNamespace\\("%s"', p), info$code)),
    logical(1)
  )]

  expect_equal(unguarded, character(0))
})
