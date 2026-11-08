# validate_template() and the library's app configuration file (dta-app.yaml).
# The parsing rules themselves are pinned in test-shinyapp-app-config.R; this
# file covers how the validator reports them.

# A library root with one valid template in templates/, so no_templates never
# fires. dta-app.yaml belongs directly in the root.
app_config_library <- function(config_lines = NULL, filename = "dta-app.yaml") {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(dir, "templates"))
  writeLines(
    c(
      "kind: dta_creation_template", "id: clean_tpl", "version: \"1.0\"",
      "base:", "  metadata:", "    title: Clean Template",
      "datasets:", "  - id: ds1", "    type: tabular",
      "    files: {filename: x.csv, type: csv}",
      "    columns:", "      - {id: COL1, type: SAS Char}"
    ),
    file.path(dir, "templates", "clean.dta-template.yaml")
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

test_that("a config in the library root is valid; the same file in a kind folder is app_config_misplaced", {
  dir <- app_config_library(c("kind: dta_app_config"))
  expect_equal(nrow(validate_template(dir)), 0)

  # Moved into templates/: the app reads only the root, so the file is
  # invisible, and the root no longer has one.
  file.rename(file.path(dir, "dta-app.yaml"), file.path(dir, "templates", "dta-app.yaml"))
  result <- validate_template(dir)
  rows <- app_rows(result)
  expect_equal(rows$code, "app_config_misplaced")
  expect_equal(rows$severity, "error")
  expect_equal(rows$kind, "dta_app_config")
  expect_equal(rows$file, file.path(dir, "templates", "dta-app.yaml"))
  expect_match(rows$message, "repository root", fixed = TRUE)
  expect_equal(nrow(result), 1)
  expect_error(
    validate_template(dir, strict = TRUE),
    class = "rlang_error", regexp = "app_config_misplaced"
  )
})

test_that("a root dta-app.yaml is not an error, while one in any other folder is, wherever it sits", {
  dir <- app_config_library(c("kind: dta_app_config"))
  for (sub in c("parties", "vocabulary", "export-templates", "templates/archive", "elsewhere/deep")) {
    at <- file.path(dir, sub)
    dir.create(at, recursive = TRUE, showWarnings = FALSE)
    writeLines("kind: dta_app_config", file.path(at, "dta-app.yml"))
  }
  rows <- app_rows(validate_template(dir))
  expect_equal(rows$code, rep("app_config_misplaced", 5))
  expect_setequal(
    sub(paste0(dir, "/"), "", rows$file, fixed = TRUE),
    file.path(c("parties", "vocabulary", "export-templates", "templates/archive", "elsewhere/deep"), "dta-app.yml")
  )
  expect_equal(unique(rows$severity), "error")
})

test_that("a dta-app.yaml under a dot directory is not reported", {
  dir <- app_config_library(c("kind: dta_app_config"))
  dir.create(file.path(dir, ".git", "x"), recursive = TRUE)
  writeLines("kind: dta_app_config", file.path(dir, ".git", "x", "dta-app.yaml"))
  expect_equal(nrow(app_rows(validate_template(dir))), 0)
})

test_that("a misplaced config is reported when the root has none", {
  # No root file at all and one in templates/: exactly one row, no duplicate.
  dir <- app_config_library()
  writeLines("kind: dta_app_config", file.path(dir, "templates", "dta-app.yaml"))
  rows <- app_rows(validate_template(dir))
  expect_equal(rows$code, "app_config_misplaced")
})

test_that("a valid root config plus a misplaced copy reports only the copy", {
  dir <- app_config_library(c("kind: dta_app_config"))
  writeLines("kind: dta_app_config", file.path(dir, "templates", "dta-app.yaml"))
  rows <- app_rows(validate_template(dir))
  expect_equal(rows$code, "app_config_misplaced")
  expect_equal(rows$file, file.path(dir, "templates", "dta-app.yaml"))
})

test_that("a single template file gets no app-config rows", {
  dir <- app_config_library(c("kind: dta_app_config"))
  writeLines("kind: dta_app_config", file.path(dir, "templates", "dta-app.yaml"))
  expect_equal(nrow(validate_template(file.path(dir, "templates", "clean.dta-template.yaml"))), 0)
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
