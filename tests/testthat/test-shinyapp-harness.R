# The Shiny app is shipped in inst/ and auto-sourced at launch, so nothing in
# the normal test suite ever parses it. These tests are the safety net: a syntax
# error or a lost file in inst/shiny/dta_app/ would otherwise only surface when
# a user runs run_dta_app().

test_that("the bundled app ships the files run_dta_app() needs", {
  app_dir <- .shiny_app_dir()

  expect_true(file.exists(file.path(app_dir, "app.R")))
  expect_true(dir.exists(file.path(app_dir, "R")))
  # app.R references this image by name in the brand bar; renaming it breaks the
  # header silently at runtime.
  expect_true(file.exists(file.path(app_dir, "www", "dtatools_logo_small.png")))
})

test_that("app.R parses and ends by constructing the app object", {
  app_file <- file.path(.shiny_app_dir(), "app.R")

  # parse() is the only cheap way to catch a syntax error in a 3,700-line file
  # that no other test loads.
  exprs <- parse(app_file, keep.source = FALSE)
  expect_gt(length(exprs), 0)

  last_call <- exprs[[length(exprs)]]
  expect_true(is.call(last_call))
  expect_equal(as.character(last_call[[1]]), "shinyApp")
})

test_that("every helper file in the app sources cleanly", {
  files <- .shiny_app_helper_files()
  expect_setequal(
    basename(files),
    c("template_core.R", "theme.R", "utils_dta.R", "utils_export.R")
  )

  # Source each file on its own, so a failure names the file that broke.
  for (f in files) {
    env <- new.env(parent = asNamespace("shiny"))
    expect_no_error(sys.source(f, envir = env, keep.source = FALSE))
    expect_gt(length(ls(env, all.names = TRUE)), 0)
  }
})

test_that("app_env() exposes the helper functions the app relies on", {
  env <- app_env()

  # A representative function from each helper file. If any of these disappear,
  # the corresponding test file below is testing nothing.
  for (nm in c(
    "dta_read_yaml", "dta_status_map", "dta_to_yaml_text", # utils_dta.R
    "create_dta_from_template", "list_set_path", # template_core.R
    "format_datasets_summary", "export_modal_ui", # utils_export.R
    "bi_theme", "yaml_highlight_html" # theme.R
  )) {
    expect_true(
      is.function(get0(nm, envir = env, inherits = FALSE)),
      info = paste0(nm, "() missing from the app helper environment")
    )
  }
})

test_that("app_env() is cached rather than re-sourced per call", {
  expect_identical(app_env(), app_env())
})

test_that("app_fn() returns the helper, and fails loudly when it is missing", {
  expect_true(is.function(app_fn("dta_try")))
  expect_error(
    app_fn("no_such_app_helper_function"),
    "is not defined in inst/shiny/dta_app/R",
    fixed = TRUE
  )
})
