# Coverage for inst/shiny/dta_app/R/template_history.R -- the version history
# computed by building consecutive versions and diffing them -- and for the
# history section of the picker's detail panel (template_ui.R).

local_clean_template_env <- function(..., .local_envir = parent.frame()) {
  withr::local_envvar(
    c(
      DTATOOLS_TEMPLATE_SOURCES = NA,
      DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = NA,
      DTATOOLS_TEMPLATE_REFRESH_SECONDS = NA,
      DTATOOLS_TEMPLATE_CACHE_DIR = NA,
      DTATOOLS_TEMPLATE_GIT_TOKEN = NA,
      DTATOOLS_TEMPLATE_GIT_USER = NA,
      DTATOOLS_TEMPLATE_GIT_AUTH = NA,
      ...
    ),
    .local_envir = .local_envir
  )
  withr::local_options(list(DTAtools.template_dir = NULL), .local_envir = .local_envir)
}

index_over <- function(root, .local_envir = parent.frame()) {
  local_clean_template_env(.local_envir = .local_envir)
  withr::local_envvar(
    DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", root),
    .local_envir = .local_envir
  )
  app_fn("dta_template_index_invalidate")()
  app_fn("build_template_index")()
}

# One creation template file. `${today}` and `${version}` are in the metadata
# on purpose: they must resolve identically on both sides of a comparison.
write_ct <- function(root, id, version, header = "Header A",
                     columns = "COL_A", options = character(0), extends = NULL) {
  opt_lines <- if (length(options) == 0) {
    "options: []"
  } else {
    c("options:", unlist(lapply(options, function(o) {
      c(sprintf("  - id: %s", o), "    type: text", sprintf("    label: %s", o))
    })))
  }
  body <- if (is.null(extends)) {
    c(
      "base:",
      "  metadata:",
      "    title: Demo",
      '    date: "${today}"',
      sprintf('    header: "%s"', header),
      "    version_history:",
      '      - { version: "${version}", date: "${today}", changes: Initial }',
      "datasets:",
      "  - name: ds1",
      "    type: tabular",
      "    files: { filename: a.csv, type: csv }",
      "    columns:",
      sprintf("      - { id: %s, label: %s, type: SAS Char }", columns, columns)
    )
  } else {
    sprintf("extends: %s", extends)
  }
  writeLines(
    c(
      "kind: dta_creation_template",
      sprintf("id: %s", id),
      sprintf('version: "%s"', version),
      sprintf("label: %s", id),
      body,
      opt_lines
    ),
    file.path(root, sprintf("%s_%s.dta-template.yaml", id, gsub(".", "_", version, fixed = TRUE)))
  )
}

entry_for <- function(history, type, version) {
  hit <- Filter(function(e) e$type == type && identical(e$version, version), history)
  testthat::expect_length(hit, 1)
  hit[[1]]
}

test_that("a version's entry lists exactly what its author changed, one line per new thing", {
  root <- withr::local_tempdir()
  write_ct(root, "demo", "1.0")
  write_ct(root, "demo", "1.1",
    header = "Header B", columns = c("COL_A", "COL_NEW"), options = "extra"
  )
  idx <- index_over(root)

  history <- app_fn("template_history")(idx, "demo")
  expect_equal(vapply(history, `[[`, "", "type"), c("version", "initial"))

  ch <- entry_for(history, "version", "1.1")$changes
  expect_null(entry_for(history, "version", "1.1")$error)
  expect_setequal(
    paste(ch$area, ch$what, ch$change, sep = " | "),
    c(
      "Metadata | header | changed",
      "Datasets | Dataset 'ds1', column 'COL_NEW' | added",
      "Template | Option 'extra' | added"
    )
  )
  hdr <- ch[ch$what == "header", ]
  expect_equal(c(hdr$from, hdr$to), c("Header A", "Header B"))
})

test_that("versions with identical content produce an empty entry, placeholders included", {
  root <- withr::local_tempdir()
  write_ct(root, "demo", "1.0")
  write_ct(root, "demo", "2.0")
  idx <- index_over(root)

  e <- entry_for(app_fn("template_history")(idx, "demo"), "version", "2.0")
  expect_null(e$error)
  expect_equal(nrow(e$changes), 0)
})

test_that("versions are ordered numerically, not as strings", {
  root <- withr::local_tempdir()
  write_ct(root, "demo", "1.9")
  write_ct(root, "demo", "1.10", header = "Header B")
  idx <- index_over(root)

  history <- app_fn("template_history")(idx, "demo")
  expect_equal(history[[1]]$title, "1.10 (compared with 1.9)")
  expect_equal(history[[2]]$type, "initial")
})

test_that("a release of a followed parent appears in the child's history", {
  root <- withr::local_tempdir()
  write_ct(root, "parent", "1.0", header = "Old")
  write_ct(root, "parent", "1.1", header = "New")
  write_ct(root, "child", "1.0", extends = "parent@latest")
  idx <- index_over(root)

  history <- app_fn("template_history")(idx, "child")
  up <- Filter(function(e) e$type == "upstream", history)
  expect_length(up, 1)
  expect_equal(up[[1]]$title, "parent 1.0 -> 1.1, followed by child@1.0")
  ch <- up[[1]]$changes
  expect_true(all(c("header", "Based on") %in% ch$what))
  expect_equal(ch$to[ch$what == "header"], "New")
})

test_that("a pinned parent's later releases do not appear", {
  root <- withr::local_tempdir()
  write_ct(root, "parent", "1.0", header = "Old")
  write_ct(root, "parent", "1.1", header = "New")
  write_ct(root, "child", "1.0", extends = "parent@1.0")
  idx <- index_over(root)

  history <- app_fn("template_history")(idx, "child")
  expect_equal(vapply(history, `[[`, "", "type"), "initial")
})

test_that("a release of a dataset template imported via @latest appears, as one line per new column", {
  root <- withr::local_tempdir()
  write_dst <- function(version, columns) {
    writeLines(
      c(
        "kind: dta_dataset_template",
        "id: shared_ds",
        sprintf('version: "%s"', version),
        "dataset:",
        "  name: shared",
        "  type: tabular",
        "  files: { filename: s.tsv, type: tsv }",
        "  columns:",
        sprintf("    - { id: %s, label: %s, type: SAS Char }", columns, columns),
        "  rules: []"
      ),
      file.path(root, sprintf("shared_%s.dta-dataset-template.yaml", gsub(".", "_", version, fixed = TRUE)))
    )
  }
  write_dst("1.0", "COL_A")
  write_dst("2.0", c("COL_A", "COL_B"))
  writeLines(
    c(
      "kind: dta_creation_template", "id: user_ct", 'version: "1.0"', "label: User",
      "base:", "  metadata:", "    title: Demo",
      "datasets:", "  - template: shared_ds@latest", "options: []"
    ),
    file.path(root, "user_ct.dta-template.yaml")
  )
  idx <- index_over(root)

  up <- Filter(function(e) e$type == "upstream", app_fn("template_history")(idx, "user_ct"))
  expect_length(up, 1)
  expect_equal(up[[1]]$title, "shared_ds 1.0 -> 2.0, followed by user_ct@1.0")
  ch <- up[[1]]$changes
  expect_equal(
    ch$what[ch$change == "added"],
    "Dataset 'shared', column 'COL_B'"
  )
})

test_that("an option that is nothing but its id is still reported when added", {
  root <- withr::local_tempdir()
  write_ct(root, "demo", "1.0")
  write_ct(root, "demo", "1.1")
  # Rewrite 1.1's options block as a bare id.
  f <- file.path(root, "demo_1_1.dta-template.yaml")
  writeLines(c(sub("^options: \\[\\]$", "options:", readLines(f)), "  - id: bare"), f)
  idx <- index_over(root)

  ch <- entry_for(app_fn("template_history")(idx, "demo"), "version", "1.1")$changes
  expect_equal(paste(ch$what, ch$change), "Option 'bare' added")
})

test_that("a version that no longer builds is reported, not dropped", {
  root <- withr::local_tempdir()
  write_ct(root, "demo", "1.0")
  write_ct(root, "demo", "1.1", extends = "ghost@1.0")
  idx <- index_over(root)

  e <- entry_for(app_fn("template_history")(idx, "demo"), "version", "1.1")
  expect_null(e$changes)
  expect_match(e$error, "ghost", fixed = TRUE)
})

test_that("the detail panel shows the selected version's changes and the full history", {
  root <- withr::local_tempdir()
  write_ct(root, "demo", "1.0")
  write_ct(root, "demo", "1.1", header = "Header B")
  idx <- index_over(root)

  loaded <- app_fn("load_template_definition")("demo@1.1", index = idx)
  versions <- app_fn("template_id_versions")(idx, "demo")
  html <- paste(as.character(
    app_fn("template_picker_detail_ui")(loaded, versions, "1.1", idx)
  ), collapse = "")

  expect_match(html, "What changed in 1.1", fixed = TRUE)
  expect_match(html, "Header A → Header B", fixed = TRUE)
  expect_match(html, "Full history (2 entries)", fixed = TRUE)
  expect_match(html, "First version of this template.", fixed = TRUE)
})
