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
  #
  # The exemplar has to be a real fill-in template: dta_numbered_template.docx
  # was used here until the listing learned to exclude documents with no
  # placeholders in them, and it is no longer offered.
  templates <- app_fn("list_available_templates")()
  expect_true("clinical_dta_template.docx" %in% templates)

  path <- app_fn("get_template_path")("clinical_dta_template.docx")
  expect_true(file.exists(path))
  expect_match(path, "clinical_dta_template\\.docx$")
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
  expect_null(get_template_path(c("clinical_dta_template.docx", "other.docx")))

  # A traversal that ends in a real bundled template name must not be repaired
  # into a hit either -- it is simply not the offered name. The name has to be
  # one the listing actually offers, or this asserts nothing about traversal.
  expect_null(get_template_path("../templates/clinical_dta_template.docx"))
})

test_that("template_has_placeholders is TRUE for the real bundled fill-in template", {
  path <- app_fixture_path("templates/clinical_dta_template.docx")
  expect_true(app_fn("template_has_placeholders")(path))
})

test_that("template_has_placeholders is FALSE for the numbered-heading styles reference document", {
  # dta_numbered_template.docx is not a fill-in template: it is the reference
  # document .new_numbered_docx() (R/documentBuilders.R) opens purely for its
  # numbered heading styles, and it carries zero {PLACEHOLDER} markers. This is
  # the regression list_available_templates() must no longer offer it for --
  # before the fix, a user who picked it got a Word document containing none
  # of their DTA, with no warning.
  path <- app_fixture_path("templates/dta_numbered_template.docx")
  expect_false(app_fn("template_has_placeholders")(path))
})

test_that("template_has_placeholders is FALSE, not an error, for a non-docx file or a missing path", {
  fake_docx <- tempfile(fileext = ".docx")
  writeLines("just some text, not a zip archive", fake_docx)
  on.exit(unlink(fake_docx, force = TRUE), add = TRUE)

  expect_false(app_fn("template_has_placeholders")(fake_docx))
  expect_false(app_fn("template_has_placeholders")(file.path(tempdir(), "no-such-file.docx")))
})

test_that("template_has_placeholders finds a placeholder split across Word runs", {
  # Word freely splits a typed placeholder across multiple runs (spell-check
  # state, revision ids, a stray formatting toggle); a raw grep on
  # document.xml would miss a token split this way, which is why the scan
  # reads paragraph *text* instead.
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  doc <- officer::read_docx()
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(officer::ftext("{DTA_"), officer::ftext("TITLE}"))
  )
  print(doc, target = path)

  expect_true(app_fn("template_has_placeholders")(path))
})

test_that("list_available_templates excludes the numbered-heading styles reference document", {
  templates <- app_fn("list_available_templates")()
  expect_false("dta_numbered_template.docx" %in% templates)
  expect_true("clinical_dta_template.docx" %in% templates)
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

test_that("raw LaTeX in document text does not reach the generated .tex", {
  skip_if_not(rmarkdown::pandoc_available())

  # The markdown pandoc_convert() reads here is built from document text a
  # third party controls (metadata title, dataset description, contact
  # names, ...), so the reader has to be pinned to a dialect without raw
  # passthrough: left at the default "markdown" reader, a `\input{}` planted
  # in any of those fields is typeset verbatim into the PDF, reading an
  # arbitrary server-side file into the delivered document.
  backslash <- rawToChar(as.raw(92))
  md_file <- tempfile(fileext = ".md")
  on.exit(unlink(md_file, force = TRUE), add = TRUE)
  writeLines(
    c(
      "# Study Title",
      "",
      paste0("Some narrative text with a raw command: ", backslash, "input{some/path.tex}"),
      "",
      "More surrounding prose that must survive conversion."
    ),
    md_file
  )

  tex_file <- tempfile(fileext = ".tex")
  on.exit(unlink(tex_file, force = TRUE), add = TRUE)
  rmarkdown::pandoc_convert(
    input = normalizePath(md_file),
    from = "markdown-raw_tex-raw_html-raw_attribute",
    to = "latex",
    output = tex_file
  )
  tex <- paste(readLines(tex_file, warn = FALSE), collapse = "\n")

  expect_false(grepl(paste0(backslash, "input{"), tex, fixed = TRUE))
  # The rest of the line must still have made it through, so the assertion
  # above fails on a conversion that silently dropped the whole line rather
  # than on one that neutralised only the raw command.
  expect_match(tex, "More surrounding prose that must survive conversion", fixed = TRUE)
})

test_that("raw HTML in document text does not reach the generated HTML", {
  skip_if_not(rmarkdown::pandoc_available())

  # Same reasoning as the LaTeX case above: a <script> planted in document
  # text must not survive as a live tag, or it executes in the headless
  # browser that markdown_to_pdf_via_chrome() drives server-side.
  md_file <- tempfile(fileext = ".md")
  on.exit(unlink(md_file, force = TRUE), add = TRUE)
  writeLines(
    c(
      "# Study Title",
      "",
      "Some narrative text with a raw tag: <script>alert(1)</script>",
      "",
      "More surrounding prose that must survive conversion."
    ),
    md_file
  )

  html_file <- tempfile(fileext = ".html")
  on.exit(unlink(html_file, force = TRUE), add = TRUE)
  rmarkdown::pandoc_convert(
    input = normalizePath(md_file),
    from = "markdown-raw_tex-raw_html-raw_attribute",
    to = "html5",
    output = html_file
  )
  html <- paste(readLines(html_file, warn = FALSE), collapse = "\n")

  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_match(html, "More surrounding prose that must survive conversion", fixed = TRUE)
})

test_that("editor tables escape attacker-controlled text but keep the Actions buttons live", {
  skip_if_not_installed("DT")
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinyjs")

  # The previous version of this test built its OWN one-row data frame and
  # called DT::datatable() on it directly: it proved DT escapes correctly
  # when handed the right `escape=` argument, and nothing about
  # output$file_tbl, output$col_tbl or output$rule_tbl (app.R), the three real
  # call sites that each independently pass `escape = -ncol(ov)`. It would
  # have kept passing had all three reverted to `escape = FALSE`.
  #
  # Driving the real render through shiny::testServer() only gets partway,
  # and it is worth being explicit about the wall: DT::renderDataTable()
  # defaults to `server = TRUE` (all three call sites take that default), and
  # in server mode the render strips `x$data` before the value testServer can
  # read is built -- the escaped cells are computed later, per AJAX request,
  # by DT:::dataTablesFilter(), reached only through a live browser round
  # trip. shiny's MockShinySession (what testServer runs on) deliberately
  # refuses that round trip -- its handleRequest is `stop("for internal use
  # only")` and its data-object registry is guarded the same way -- so no
  # amount of testServer plumbing reaches the actual escaped output. What IS
  # reachable, and what the first block below checks, is that the real render
  # runs against real edited state without error and in the real
  # `serverSide = TRUE` configuration.
  #
  # The escaping guarantee itself -- the actual point of this test -- is
  # checked the same way the original test did (DT's own preRenderHook, kept
  # off DT's private internals), but now against the LIVE overview each call
  # site builds (dta_handlers_overview()/dta_columns_overview()/
  # dta_rules_overview(), fed by state this block edits through the real
  # editor inputs) and the identical `DT::datatable()` arguments each call
  # site uses. The Actions column is the one stand-in left: row_action_buttons()
  # is a closure private to the server function, unreachable from a test, so a
  # representative button tag takes its place there.
  #
  # A source-level pin closes the remaining gap: it fails if any call site's
  # `escape = -ncol(ov)` argument itself is ever edited away, which is exactly
  # the regression neither the render check nor the escaping check above can
  # see.

  app_server_dir <- function() .shiny_app_dir()
  app_file_input <- function(filename) {
    path <- app_fixture_path(filename)
    data.frame(
      name = filename, size = file.size(path), type = "", datapath = path,
      stringsAsFactors = FALSE
    )
  }

  xss <- "<img src=x onerror=alert(1)>"
  escaped_xss <- "&lt;img src=x onerror=alert(1)&gt;"

  # Re-escape the LIVE overview data through the same call DT::datatable()
  # receives at each real site (rownames/escape are what decide escaping;
  # class/width/options do not, and are dropped as noise).
  escaped_cells <- function(ov) {
    ov$Actions <- "<button>x</button>" # stand-in -- see the WHY comment above
    wid <- DT::datatable(ov, rownames = FALSE, selection = "none", escape = -ncol(ov))
    unlist(wid$preRenderHook(wid)$x$data)
  }

  check_table <- function(output_json, ov) {
    out <- unclass(output_json)
    expect_match(out, '"serverSide":true', fixed = TRUE)
    expect_match(out, ">Actions<", fixed = TRUE)

    cells <- escaped_cells(ov)
    expect_true(any(grepl(escaped_xss, cells, fixed = TRUE)))
    expect_false(any(grepl(xss, cells, fixed = TRUE)))
    expect_true(any(grepl("<button>x</button>", cells, fixed = TRUE)))
  }

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    unlock_editing(session)

    # file_tbl: attacker text in a handler's pattern description.
    session$setInputs(edit_files = 1)
    session$setInputs(file_add = 1)
    session$setInputs(
      file_filename = "extra.csv", file_type = "csv", file_pattern = FALSE,
      file_pattern_description = xss
    )
    session$setInputs(file_save = 1)
    expect_null(rv$file_msg)
    check_table(output$file_tbl, app_fn("dta_handlers_overview")(rv$dta, "clinical_data"))

    # col_tbl: attacker text in a column's description.
    session$setInputs(edit_cols = 1)
    session$setInputs(col_add = 1)
    session$setInputs(
      col_id = "ZZXSS", col_label = "Test column", col_backend = "SAS",
      col_type = "Char", col_format = "", col_length = "10",
      col_nullable = TRUE, col_values = "", col_pattern = "",
      col_desc = xss
    )
    session$setInputs(col_save = 1)
    expect_null(rv$col_msg)
    check_table(output$col_tbl, app_fn("dta_columns_overview")(rv$dta, "clinical_data"))

    # rule_tbl: attacker text in a rule's description.
    session$setInputs(edit_rules = 1)
    session$setInputs(rule_add = 1)
    session$setInputs(rule_type = "col_unique")
    session$setInputs(rule_id = "zzxss_unique", rule_desc = xss, rule_cols = "SUBJECT_ID")
    session$setInputs(rule_save = 1)
    expect_null(rv$rule_msg)
    check_table(output$rule_tbl, app_fn("dta_rules_overview")(rv$dta, "clinical_data"))
  })

  app_code <- app_source("app.R")
  for (id in c("file_tbl", "col_tbl", "rule_tbl")) {
    block <- regmatches(
      app_code,
      regexpr(
        paste0("(?s)output\\$", id, " <- DT::renderDataTable\\(\\{.*?\\n  \\}\\)"),
        app_code,
        perl = TRUE
      )
    )
    expect_length(block, 1)
    expect_match(block, "escape = -ncol(ov)", fixed = TRUE)
  }
})
