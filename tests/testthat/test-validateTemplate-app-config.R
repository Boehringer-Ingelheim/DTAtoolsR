# validate_template() and the library's app configuration file (dta-app.yaml).
# The parsing rules themselves are pinned in test-shinyapp-app-config.R; this
# file covers how the validator reports them.

# A library directory with one valid template, so no_templates never fires.
app_config_library <- function(config_lines = NULL, filename = "dta-app.yaml") {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  writeLines(
    c(
      "kind: dta_creation_template", "id: clean_tpl", "version: \"1.0\"",
      "base:", "  metadata:", "    title: Clean Template",
      "datasets:", "  - id: ds1", "    type: tabular",
      "    files: {filename: x.csv, type: csv}",
      "    columns:", "      - {id: COL1, type: SAS Char}"
    ),
    file.path(dir, "clean.dta-template.yaml")
  )
  if (!is.null(config_lines)) writeLines(config_lines, file.path(dir, filename))
  dir
}

app_rows <- function(result) {
  result[grepl("^app_config_", result$code), , drop = FALSE]
}

test_that("a valid dta-app.yaml produces no app_config rows", {
  dir <- app_config_library(c(
    "kind: dta_app_config",
    "colors:", "  primary: \"#112233\"",
    "branding:", "  app_title: Acme DTA"
  ))
  expect_equal(nrow(app_rows(validate_template(dir))), 0)
})

test_that("a bad colour is an app_config_invalid error naming the key, and strict aborts", {
  dir <- app_config_library(c("kind: dta_app_config", "colors:", "  primary: red;}body{"))
  rows <- app_rows(validate_template(dir))
  expect_equal(rows$code, "app_config_invalid")
  expect_equal(rows$severity, "error")
  expect_equal(rows$kind, "dta_app_config")
  expect_match(rows$message, "colors.primary", fixed = TRUE)
  expect_error(validate_template(dir, strict = TRUE), "app_config_invalid", fixed = TRUE)
})

test_that("an unquoted hex colour (a YAML comment) is reported, not silently dropped", {
  dir <- app_config_library(c("kind: dta_app_config", "colors:", "  primary: #00625B"))
  rows <- app_rows(validate_template(dir))
  expect_equal(rows$code, "app_config_invalid")
  expect_match(rows$message, "colors.primary", fixed = TRUE)
})

test_that("an unknown key is a warning and does not fail strict", {
  dir <- app_config_library(c("kind: dta_app_config", "branding:", "  nonsense: 1"))
  rows <- app_rows(validate_template(dir, strict = TRUE))
  expect_equal(rows$code, "app_config_unknown_key")
  expect_equal(rows$severity, "warning")
  expect_match(rows$message, "branding.nonsense", fixed = TRUE)
})

test_that("a logo or Word template that does not exist is an error", {
  dir <- app_config_library(c(
    "kind: dta_app_config",
    "branding:", "  logo: missing.png",
    "exports:", "  word_templates:", "    - file: missing.docx"
  ))
  rows <- app_rows(validate_template(dir))
  expect_setequal(rows$code, "app_config_invalid")
  expect_true(any(grepl("branding.logo", rows$message, fixed = TRUE)))
  expect_true(any(grepl("exports.word_templates[1].file", rows$message, fixed = TRUE)))
})

test_that("a logo next to the config validates", {
  dir <- app_config_library(c("kind: dta_app_config", "branding:", "  logo: l.png"))
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), file.path(dir, "l.png"))
  expect_equal(nrow(app_rows(validate_template(dir))), 0)
})

test_that("both dta-app.yaml and dta-app.yml are an app_config_duplicate error on each file", {
  dir <- app_config_library(c("kind: dta_app_config"))
  writeLines("kind: dta_app_config", file.path(dir, "dta-app.yml"))
  rows <- app_rows(validate_template(dir))
  expect_equal(rows$code, rep("app_config_duplicate", 2))
  expect_setequal(basename(rows$file), c("dta-app.yaml", "dta-app.yml"))
  expect_equal(unique(rows$severity), "error")
})

test_that("a config at the repository root while templates/ is active is app_config_outside_library_dir", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "templates"))
  lib <- app_config_library()
  file.copy(
    file.path(lib, "clean.dta-template.yaml"),
    file.path(root, "templates", "clean.dta-template.yaml")
  )
  writeLines("kind: dta_app_config", file.path(root, "dta-app.yaml"))
  rows <- app_rows(validate_template(root))
  expect_equal(rows$code, "app_config_outside_library_dir")
  expect_equal(rows$severity, "error")
  expect_match(rows$message, "templates/", fixed = TRUE)

  # The same file inside templates/ is fine.
  file.rename(file.path(root, "dta-app.yaml"), file.path(root, "templates", "dta-app.yaml"))
  expect_equal(nrow(app_rows(validate_template(root))), 0)
})

test_that("the app-config check runs whatever `kinds` is", {
  dir <- app_config_library(c("kind: dta_app_config", "colors:", "  primary: red"))
  rows <- app_rows(validate_template(dir, kinds = "dta_vocabulary"))
  expect_equal(rows$code, "app_config_invalid")
})

test_that("the skeleton's dta-app.yaml passes validate_template(strict = TRUE)", {
  dir <- withr::local_tempdir()
  create_template_repo(dir, ci = FALSE)
  result <- validate_template(dir, strict = TRUE)
  expect_equal(nrow(result), 0)
})
