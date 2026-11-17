# A dataset in a specification YAML cannot bind server paths: `paths:` would
# make the file-dataset check stat, size and probe any path the process can
# reach, and report the result. Dummy files only.

dk_yaml <- function(extra) {
  f <- withr::local_tempfile(fileext = ".yaml", .local_envir = parent.frame())
  writeLines(c("id: d1", "type: file", "name: Dummy", extra), f)
  f
}

test_that("a `paths:` key is a lint error and the reader refuses it, so no path is bound", {
  secret <- withr::local_tempfile(fileext = ".txt")
  writeLines("x", secret)
  f <- dk_yaml(c("paths:", paste0("  - '", secret, "'")))

  findings <- lint_dta(f)
  expect_gte(sum(findings$code == "dataset_invalid" & findings$severity == "error"), 1L)
  expect_error(read_dataset_from_yaml(f), class = "dta_lint_error")
})

test_that("tables and specs, and a paths key past the linter, are refused by the builder", {
  for (x in list(
    list(id = "d1", type = "file", paths = "x"),
    list(id = "d1", type = "tabular", tables = list()),
    list(id = "d1", type = "tabular", specs = list())
  )) {
    expect_error(dta_dataset_from_list(x), class = "dta_dataset_runtime_key")
  }
})

test_that("binding paths in R, and an ordinary file dataset YAML, are unaffected", {
  f <- dk_yaml(c("files:", "  filename: report.pdf", "  type: any"))
  expect_false("error" %in% lint_dta(f)$severity)
  expect_length(read_dataset_from_yaml(f)@file_paths, 0L)
  expect_identical(DTADataSetFile(id = "d2", paths = "a.txt")@file_paths, "a.txt")
})
