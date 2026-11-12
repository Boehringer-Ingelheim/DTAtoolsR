# Coverage for R/lintTemplateRepo.R: lint_template_repo(), validate_template()
# plus the repository-format checks (folder case, file name vs id, ignored
# files), the printed report and the fail_on contract.

lint_columns <- c("file", "kind", "id", "version", "severity", "code", "message")

# A fresh scaffold: the clean baseline every test below perturbs.
lint_scaffold <- function(...) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  suppressMessages(create_template_repo(dir, ...))
  dir
}

lint_quiet <- function(dir, ...) {
  suppressMessages(lint_template_repo(dir, ...))
}

# The starter party file with its id replaced.
lint_party_text <- function(dir, id, version = NULL) {
  txt <- readLines(file.path(dir, "parties", "starter_supplier.dta-party.yaml"))
  txt <- sub("^id: .*$", paste0("id: ", id), txt)
  if (!is.null(version)) {
    txt <- sub("^version: .*$", paste0("version: \"", version, "\""), txt)
  }
  txt
}

test_that("a fresh scaffold and the bundled library lint clean, with the full column set", {
  dir <- lint_scaffold()
  res <- lint_quiet(dir, fail_on = "warning")
  expect_equal(nrow(res), 0L)
  expect_identical(names(res), lint_columns)

  lib <- system.file("extdata", "library", package = "DTAtools")
  expect_true(nzchar(lib), info = "bundled extdata/library missing")
  res <- lint_quiet(lib, fail_on = "warning")
  expect_equal(nrow(res), 0L)
  expect_identical(names(res), lint_columns)
})

test_that("id_filename_mismatch flags a stem that is not the id, and accepts qualified stems", {
  dir <- lint_scaffold()
  writeLines(lint_party_text(dir, "other_party"), file.path(dir, "parties", "acme.dta-party.yaml"))
  writeLines(
    lint_party_text(dir, "starter_supplier", version = "0.9"),
    file.path(dir, "parties", "starter_supplier_old.dta-party.yaml")
  )

  res <- lint_quiet(dir, fail_on = "none")
  hit <- res[res$code == "id_filename_mismatch", ]
  expect_equal(nrow(hit), 1L)
  expect_identical(basename(hit$file), "acme.dta-party.yaml")
  expect_identical(hit$severity, "warning")
  expect_identical(hit$id, "other_party")
})

test_that("folder_name_case flags a folder spelled with the wrong letter case", {
  dir <- lint_scaffold()
  unlink(file.path(dir, "vocabulary"), recursive = TRUE)
  dir.create(file.path(dir, "Vocabulary"))
  file.copy(
    system.file("extdata", "template-repo-skeleton", "vocabulary", "starter_terms.dta-vocabulary.yaml",
      package = "DTAtools"
    ),
    file.path(dir, "Vocabulary")
  )

  res <- lint_quiet(dir, fail_on = "none")
  hit <- res[res$code == "folder_name_case", ]
  expect_equal(nrow(hit), 1L)
  expect_identical(basename(hit$file), "Vocabulary")
  expect_identical(hit$severity, "error")
  expect_true(is.na(hit$kind))
})

test_that("file_ignored flags a kind-declaring YAML without the suffix and nothing else", {
  dir <- lint_scaffold()
  writeLines(
    c("kind: dta_creation_template", "id: forgot", "version: \"1.0\""),
    file.path(dir, "templates", "forgot_suffix.yaml")
  )
  writeLines("name: x", file.path(dir, "templates", "source_data.yaml"))

  res <- lint_quiet(dir, fail_on = "none")
  hit <- res[res$code == "file_ignored", ]
  expect_equal(nrow(hit), 1L)
  expect_identical(basename(hit$file), "forgot_suffix.yaml")
  expect_identical(hit$severity, "warning")
  expect_identical(hit$kind, "dta_creation_template")
})

test_that("CI configuration and dta-app.yaml are not reported as ignored files", {
  dir <- lint_scaffold(ci = "bitbucket")
  expect_true(file.exists(file.path(dir, "bitbucket-pipelines.yml")))
  expect_true(file.exists(file.path(dir, "dta-app.yaml")))
  res <- lint_quiet(dir, fail_on = "warning")
  expect_equal(nrow(res), 0L)
})

test_that("an error finding aborts by default with the lint class, and still returns nothing silently", {
  dir <- lint_scaffold()
  path <- file.path(dir, "templates", "starter.dta-template.yaml")
  txt <- readLines(path)
  expect_true(any(txt == "version: \"1.0\""))
  writeLines(sub("^version: \"1.0\"$", "version: 1.0", txt), path)

  expect_error(lint_quiet(dir), class = "dta_template_lint_error")
  res <- lint_quiet(dir, fail_on = "none")
  expect_true("version_unquoted" %in% res$code)
})

test_that("fail_on selects the severity that aborts", {
  dir <- lint_scaffold()
  writeLines(lint_party_text(dir, "other_party"), file.path(dir, "parties", "acme.dta-party.yaml"))

  # Only warnings are present: the default does not abort and returns invisibly.
  expect_invisible(suppressMessages(lint_template_repo(dir)))
  res <- lint_quiet(dir)
  expect_false(any(res$severity == "error"))
  expect_true(any(res$severity == "warning"))

  expect_error(lint_quiet(dir, fail_on = "warning"), class = "dta_template_lint_error")

  # "none" never aborts, even with an error present.
  path <- file.path(dir, "templates", "starter.dta-template.yaml")
  writeLines(sub("^version: \"1.0\"$", "version: 1.0", readLines(path)), path)
  res <- lint_quiet(dir, fail_on = "none")
  expect_true(any(res$severity == "error"))
})

test_that("bad arguments abort with a cli error", {
  dir <- lint_scaffold()
  not_dir <- "must be an existing directory"
  one_of <- "must be one of"
  expect_error(lint_template_repo(file.path(dir, "no-such-dir")), not_dir, fixed = TRUE, class = "rlang_error")
  expect_error(
    lint_template_repo(file.path(dir, "dta-app.yaml")),
    not_dir,
    fixed = TRUE, class = "rlang_error"
  )
  expect_error(lint_template_repo(dir, fail_on = "bogus"), one_of, fixed = TRUE, class = "rlang_error")
  expect_error(lint_template_repo(dir, fail_on = c("error", "warning")), one_of, fixed = TRUE, class = "rlang_error")
})

test_that("the report names each finding and says when there are none", {
  dir <- lint_scaffold()
  expect_message(lint_template_repo(dir), "No findings")

  writeLines(lint_party_text(dir, "other_party"), file.path(dir, "parties", "acme.dta-party.yaml"))
  expect_message(lint_template_repo(dir), "id_filename_mismatch")
})

# ---- non-ASCII paths ----------------------------------------------------------

# list.files() returns native-encoded names, and method = "radix" refuses
# those unless they are UTF-8: the linter used to abort on any such file.
lint_nonascii_repo <- function(file_name, id, repo_name = "repo") {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  root <- file.path(dir, repo_name)
  suppressMessages(create_template_repo(root, examples = FALSE))
  dir.create(file.path(root, "templates"), showWarnings = FALSE)
  writeLines(
    c(
      "dta_template: 1", paste0("id: ", id), "version: '1.0'",
      "kind: creation", "title: Test", "description: Test",
      "base: {}"
    ),
    enc2utf8(file.path(root, "templates", file_name))
  )
  root
}

test_that("a template file with a non-ASCII name is linted, not aborted on", {
  root <- lint_nonascii_repo("caf\u00e9.dta-template.yaml", "caf\u00e9")
  res <- expect_no_error(lint_quiet(root, fail_on = "none"))
  expect_identical(names(res), lint_columns)
  expect_no_error(suppressMessages(validate_template(root)))
  withr::local_dir(root)
  expect_no_error(lint_quiet(".", fail_on = "none"))
})

test_that("a repository folder with a non-ASCII name is linted, not aborted on", {
  root <- lint_nonascii_repo("starter.dta-template.yaml", "starter", "M\u00fcller_repo")
  expect_no_error(lint_quiet(root, fail_on = "none"))
})

test_that("export templates under a non-ASCII repository folder are checked, not aborted on", {
  root <- lint_nonascii_repo("starter.dta-template.yaml", "starter", "M\u00fcller_repo")
  exp_dir <- file.path(root, "export-templates")
  dir.create(exp_dir, showWarnings = FALSE)
  file.copy(.make_template("No markers"), file.path(exp_dir, "plain_a.docx"))
  file.copy(.make_template("No markers either"), file.path(exp_dir, "plain_b.docx"))
  res <- expect_no_error(lint_quiet(root, fail_on = "none"))
  expect_identical(sum(res$code == "export_template_no_placeholders"), 2L)
})
