# R/utils_export.R lives in inst/shiny/dta_app/R/ and is auto-sourced by
# Shiny at launch, so it is invisible to the package namespace and to the
# normal test suite. These tests reach it through the app_env()/app_fn()
# harness in helper-shinyapp.R.

test_that("format_datasets_summary names every dataset and its type", {
  dta <- app_fixture_dta()
  ds_names <- names(dta@datasets)
  expect_gt(length(ds_names), 0)

  out <- app_fn("format_datasets_summary")(dta)
  expect_match(out, paste0(length(ds_names), " dataset"), fixed = TRUE)
  for (nm in ds_names) {
    ds <- DTAtools::datasets(dta, nm)
    expect_match(out, paste0(nm, ": ", as.character(ds@type)[1]), fixed = TRUE)
  }
})

test_that("format_datasets_summary on a DTA with zero datasets is valid, non-empty, non-NA text", {
  dta <- app_fixture_dta()
  dta@datasets <- list()
  expect_length(dta@datasets, 0)

  out <- app_fn("format_datasets_summary")(dta)
  expect_false(is.na(out))
  expect_gt(nchar(out), 0)
  expect_identical(out, "No datasets")
})

test_that("format_datasets_detail lists every dataset's column ids and rule ids", {
  dta <- app_fixture_dta()
  ds_names <- names(dta@datasets)
  out <- app_fn("format_datasets_detail")(dta)

  for (nm in ds_names) {
    expect_match(out, paste0("## Dataset: ", nm), fixed = TRUE)
    ds <- DTAtools::datasets(dta, nm)

    col_ids <- names(ds@specs@columns)
    for (col_id in col_ids) {
      expect_match(out, paste0("**", col_id, "**"), fixed = TRUE)
    }

    rule_ids <- vapply(ds@specs@rules, function(r) r@id, character(1))
    for (rule_id in rule_ids) {
      expect_match(out, paste0("**", rule_id, ":**"), fixed = TRUE)
    }
  }
})

test_that("format_datasets_detail on a DTA with zero datasets is valid, non-NA text (possibly empty)", {
  dta <- app_fixture_dta()
  dta@datasets <- list()

  out <- app_fn("format_datasets_detail")(dta)
  expect_false(is.na(out))
  expect_identical(out, "")
})

test_that(".format_rule_detail formats a col_condition (IF/THEN) rule", {
  f <- app_fn(".format_rule_detail")
  l <- list(
    type = "col_condition",
    condition = list(VISIT = list(equals = "V03")),
    then = list(STATUS = list(equals = "COMPLETED"))
  )
  expect_identical(f(l), "IF VISIT equals V03 THEN STATUS equals COMPLETED")
})

test_that(".format_rule_detail formats a col_range rule", {
  f <- app_fn(".format_rule_detail")
  l <- list(type = "col_range", columns = "AGE", min = 18, max = 65)
  expect_identical(f(l), "AGE in [18, 65]")
})

test_that(".format_rule_detail formats a col_unique rule", {
  f <- app_fn(".format_rule_detail")
  l <- list(type = "col_unique", columns = c("SUBJECT_ID", "VISIT"))
  expect_identical(f(l), "unique(SUBJECT_ID, VISIT)")
})

test_that(".format_rule_detail formats a group_condition rule", {
  f <- app_fn(".format_rule_detail")
  l <- list(
    type = "group_condition",
    group_by = c("SUBJECT_ID", "VISIT"),
    conditions = list(c1 = list(STATUS = list(equals = "FAILED"))),
    constraints = list(list(type = "requires", `if` = "c1", then = "c1"))
  )
  expect_identical(f(l), "group(SUBJECT_ID, VISIT): 1 condition(s), 1 constraint(s)")
})

test_that(".format_rule_detail falls back to the raw type token for an unrecognised rule type, and to empty for no rule", {
  f <- app_fn(".format_rule_detail")
  expect_identical(f(list(type = "mystery_type")), "mystery_type")
  expect_identical(f(NULL), "")
})

test_that("format_datasets_detail matches the fixture's real rule shapes end to end", {
  # The fixture DTA (inst/extdata/clinical_dta.yaml) carries all three rule
  # shapes .format_rule_detail() handles; assert the exact phrasing survives
  # the full format_datasets_detail() pipeline for one of each.
  dta <- app_fixture_dta()
  out <- app_fn("format_datasets_detail")(dta)

  expect_match(out, "IF VISIT equals V03 THEN STATUS equals COMPLETED", fixed = TRUE)
  expect_match(out, "AGE in [18, 65]", fixed = TRUE)
  expect_match(out, "unique(SUBJECT_ID, VISIT)", fixed = TRUE)
})

test_that("embed_yaml_markdown appends the DTA's YAML to the original markdown text", {
  dta <- app_fixture_dta()
  md <- "# My Export\n\nSome narrative text."

  out <- app_fn("embed_yaml_markdown")(md, dta)

  expect_match(out, "# My Export", fixed = TRUE)
  expect_match(out, "Some narrative text.", fixed = TRUE)
  # The embedded block is a machine-readable YAML rendering of the DTA,
  # fenced in an HTML comment; a dataset name from the fixture must appear
  # inside it.
  expect_match(out, "EMBEDDED DTA YAML", fixed = TRUE)
  expect_match(out, "name: clinical_data", fixed = TRUE)
  expect_match(out, "END EMBEDDED DTA YAML", fixed = TRUE)
})

test_that("embed_yaml_markdown still embeds the YAML when the markdown text is empty", {
  dta <- app_fixture_dta()
  out <- app_fn("embed_yaml_markdown")("", dta)

  expect_match(out, "EMBEDDED DTA YAML", fixed = TRUE)
  expect_match(out, "name: clinical_data", fixed = TRUE)
})

test_that("list_available_templates finds the real bundled .docx template", {
  # list_available_templates()/get_template_path() resolve the templates
  # directory via system.file(package = "DTAtools"). app_env() (see
  # helper-shinyapp.R) binds the caller-visible system.file() into the app
  # environment before sourcing the helper files, so this resolves correctly
  # both under devtools::test() (pkgload's shim) and under R CMD check
  # (base::system.file() against the installed package).
  templates <- app_fn("list_available_templates")()
  expect_true("dta_numbered_template.docx" %in% templates)

  path <- app_fn("get_template_path")("dta_numbered_template.docx")
  expect_true(file.exists(path))
  expect_match(path, "dta_numbered_template\\.docx$")
})

test_that("get_template_path returns NULL (not an error) for an unknown template name", {
  expect_null(app_fn("get_template_path")("no_such_template.docx"))
  expect_null(app_fn("get_template_path")(NULL))
  expect_null(app_fn("get_template_path")(""))
})

test_that("get_template_path refuses a name that escapes the templates directory", {
  # The name reaches this function from a selectInput, but a Shiny client is not
  # bound by the offered choices and can put any string on the websocket. The
  # name is therefore whitelisted against the bundled templates instead of being
  # pasted into a path -- otherwise a traversal would resolve to an arbitrary
  # server-side file, which export_with_template() would render and hand back as
  # a download.
  get_template_path <- app_fn("get_template_path")

  expect_null(get_template_path("../../DESCRIPTION"))
  expect_null(get_template_path("../../../../../../etc/passwd"))
  expect_null(get_template_path("..\\..\\DESCRIPTION"))
  expect_null(get_template_path(file.path(tempdir(), "planted.docx")))
  expect_null(get_template_path(NA_character_))
  expect_null(get_template_path(c("dta_numbered_template.docx", "other.docx")))

  # A traversal that ends in a real bundled template name must not be repaired
  # into a hit either -- it is simply not the offered name.
  expect_null(get_template_path("../templates/dta_numbered_template.docx"))
})

test_that("the export modal is built by app.R, with no orphaned UI builder", {
  # export_modal_ui() used to live in utils_export.R but was never called: the
  # real modal is built inline in app.R (around the `input$export_modal_open`
  # observer) with flat, unnamespaced ids, while the orphan module-namespaced
  # its ids with shiny::NS() and omitted several inputs entirely. It was
  # removed; this guards against a second UI builder drifting back in.
  expect_null(get0("export_modal_ui", envir = app_env(), inherits = FALSE))

  app_code <- readLines(file.path(.shiny_app_dir(), "app.R"), warn = FALSE)
  # The ids the server actually observes must be the ones the inline modal
  # creates.
  for (id in c("export_format", "export_cancel", "export_do")) {
    expect_true(
      any(grepl(id, app_code, fixed = TRUE)),
      info = paste0("export modal input `", id, "` no longer appears in app.R")
    )
  }
})

test_that("has_pdf_engine falls back cleanly when no engine is on the PATH", {
  # Whether this machine has pdflatex is not something a test can assert, but
  # the no-engine-found branch (which reaches the guarded tinytex call) is
  # otherwise never exercised on a developer machine that does have one.
  empty_dir <- file.path(tempdir(), "dtatools-empty-path")
  dir.create(empty_dir, showWarnings = FALSE)
  old_path <- Sys.getenv("PATH")
  on.exit(Sys.setenv(PATH = old_path), add = TRUE, after = FALSE)
  on.exit(unlink(empty_dir, recursive = TRUE, force = TRUE), add = TRUE)
  Sys.setenv(PATH = empty_dir)

  out <- app_fn("has_pdf_engine")()

  expect_type(out, "logical")
  expect_length(out, 1)
  expect_false(is.na(out))
})

test_that("find_chrome_binary honours the DTATOOLS_CHROME override", {
  fake_chrome <- tempfile(fileext = ".exe")
  writeLines("not really chrome", fake_chrome)
  on.exit(unlink(fake_chrome, force = TRUE), add = TRUE)

  old <- Sys.getenv("DTATOOLS_CHROME", unset = NA)
  on.exit(
    if (is.na(old)) Sys.unsetenv("DTATOOLS_CHROME") else Sys.setenv(DTATOOLS_CHROME = old),
    add = TRUE, after = FALSE
  )
  Sys.setenv(DTATOOLS_CHROME = fake_chrome)

  # The override wins over anything found on PATH or in the standard install
  # locations, so this branch is deterministic on every machine.
  expect_equal(app_fn("find_chrome_binary")(), normalizePath(fake_chrome))
})

test_that("find_chrome_binary ignores an override pointing at a missing file", {
  ghost <- file.path(tempdir(), "no-such-chrome-binary.exe")
  expect_false(file.exists(ghost))

  old <- Sys.getenv("DTATOOLS_CHROME", unset = NA)
  on.exit(
    if (is.na(old)) Sys.unsetenv("DTATOOLS_CHROME") else Sys.setenv(DTATOOLS_CHROME = old),
    add = TRUE, after = FALSE
  )
  Sys.setenv(DTATOOLS_CHROME = ghost)

  out <- app_fn("find_chrome_binary")()

  # A dead override must be skipped, not handed back to the caller, who would
  # then hand it to system2().
  expect_false(identical(out, ghost))
  expect_length(out, 1)
  expect_false(is.na(out))
  # Whatever it does return must be either "not found" or a real executable.
  if (nzchar(out)) {
    expect_true(file.exists(out))
  }
})

# markdown_to_pdf_via_chrome() is intentionally NOT tested here: it shells
# out to a real Chrome/Edge binary via system2() to print a PDF, which is an
# external-process integration concern outside the scope of these unit
# tests (and not reliably available/deterministic in CI).

test_that("the export modal defaults to Word with the YAML specification embedded", {
  # Deliberate defaults: Word is the format users actually hand over, and the
  # embedded YAML is what makes the document machine-readable, so neither
  # should need a click. The modal is rebuilt on every open (it lives inside
  # the input$export_modal_open observer), so these literals are what the user
  # sees each time.
  app_code <- paste(readLines(file.path(.shiny_app_dir(), "app.R"), warn = FALSE), collapse = "\n")

  fmt <- regmatches(
    app_code,
    regexpr('(?s)radioButtons\\("export_format".{0,300}?selected = "[a-z]+"', app_code, perl = TRUE)
  )
  expect_length(fmt, 1)
  expect_match(fmt, 'selected = "word"', fixed = TRUE)
  expect_false(grepl('selected = "markdown"', fmt, fixed = TRUE))

  yaml_box <- regmatches(
    app_code,
    regexpr('(?s)checkboxInput\\("export_include_yaml_word".*?\\)', app_code, perl = TRUE)
  )
  expect_length(yaml_box, 1)
  expect_match(yaml_box, "value = TRUE", fixed = TRUE)
})

test_that("the export modal names itself once", {
  # modalDialog() renders `title` as the dialog's own header. The body used to
  # open with an h4 carrying the same words, so the dialog read "Export
  # Document" twice, one above the other.
  app_code <- paste(readLines(file.path(.shiny_app_dir(), "app.R"), warn = FALSE), collapse = "\n")

  # The observer that builds and shows the modal, from `modal_content <- div(`
  # to the end of the showModal() call.
  modal <- regmatches(
    app_code,
    regexpr("(?s)modal_content <- div\\(.*?size = \"m\"", app_code, perl = TRUE)
  )
  expect_length(modal, 1)

  # One mention, and it is the modalDialog title rather than a heading.
  expect_match(modal, 'title = "Export Document"', fixed = TRUE)
  expect_false(grepl('h4("Export Document")', modal, fixed = TRUE))
  expect_equal(
    length(gregexpr('"Export Document"', modal, fixed = TRUE)[[1]]),
    1L
  )
})
