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
  # fenced in an HTML comment; the fixture dataset must appear inside it, as a
  # list entry that opens with its id. (Not "name: clinical_data": that is a
  # substring of the file pattern's `filename: clinical_data...` and would
  # pass whatever the dataset is called.)
  expect_match(out, "EMBEDDED DTA YAML", fixed = TRUE)
  expect_match(out, "- id: clinical_data", fixed = TRUE)
  expect_match(out, "END EMBEDDED DTA YAML", fixed = TRUE)
})

test_that("embed_yaml_markdown still embeds the YAML when the markdown text is empty", {
  dta <- app_fixture_dta()
  out <- app_fn("embed_yaml_markdown")("", dta)

  expect_match(out, "EMBEDDED DTA YAML", fixed = TRUE)
  expect_match(out, "- id: clinical_data", fixed = TRUE)
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
  expect_null(get_template_path("../word-templates/clinical_dta_template.docx"))
})

test_that("template_has_placeholders is TRUE for the real bundled fill-in template", {
  path <- app_fixture_path("word-templates/clinical_dta_template.docx")
  expect_true(app_fn("template_has_placeholders")(path))
})

test_that("template_has_placeholders is FALSE for the numbered-heading styles reference document", {
  # dta_numbered_template.docx is not a fill-in template: it is the reference
  # document .new_numbered_docx() (R/documentBuilders.R) opens purely for its
  # numbered heading styles, and it carries zero {PLACEHOLDER} markers. This is
  # the regression list_available_templates() must no longer offer it for --
  # before the fix, a user who picked it got a Word document containing none
  # of their DTA, with no warning.
  path <- app_fixture_path("word-templates/dta_numbered_template.docx")
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
# tests (and not reliably available/deterministic in CI). What it hands the
# browser is pinned instead: scripts are blocked by a CSP in the page itself,
# because the Chromium flags that claim to do it either do nothing
# (--disable-javascript) or also break --print-to-pdf (--blink-settings).
test_that("the Chrome PDF route blocks scripts with a CSP, not with a browser flag", {
  src <- app_source(file.path("R", "utils_export.R"))
  # default-src 'none' blocks scripts (and every fetch: images, frames) and
  # nothing re-allows a script source.
  expect_match(src, "Content-Security-Policy\\\" content=\\\"default-src 'none';", fixed = TRUE)
  expect_no_match(src, "script-src", fixed = TRUE)
  expect_no_match(src, "\"--disable-javascript\"", fixed = TRUE)
  expect_no_match(src, "\"--blink-settings", fixed = TRUE)
})

test_that("the export modal defaults to Word with the YAML specification embedded", {
  # Deliberate defaults: Word is the format users actually hand over, and the
  # embedded YAML is what makes the document machine-readable, so neither
  # should need a click. The modal is rebuilt on every open (it lives inside
  # the input$export_modal_open observer), so these literals are what the user
  # sees each time.
  app_code <- paste(readLines(file.path(.shiny_app_dir(), "app.R"), warn = FALSE), collapse = "\n")

  # The selection is no longer a literal in app.R: it comes from the library's
  # app config via export_document_options() (dta-app.yaml `default_document`),
  # whose stock default is Word. Pin both halves -- the dialog reads the option,
  # and the default option is Word.
  expect_match(app_code, "selected = export_choices$default_format", fixed = TRUE)
  expect_identical(app_fn("export_document_options")()$default_format, "word")

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
  # names, ...), so the reader and filter the app uses must not pass raw
  # commands on: with the default "markdown" reader, a `\input{}` planted in
  # any of those fields is typeset verbatim into the PDF, reading an
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
  filter <- app_fn("pandoc_markdown_filter")()
  on.exit(unlink(c(tex_file, filter), force = TRUE), add = TRUE)
  rmarkdown::pandoc_convert(
    input = normalizePath(md_file),
    from = app_fn("pandoc_markdown_reader")(),
    to = "latex",
    output = tex_file,
    options = c("--lua-filter", filter)
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
  filter <- app_fn("pandoc_markdown_filter")()
  on.exit(unlink(c(html_file, filter), force = TRUE), add = TRUE)
  rmarkdown::pandoc_convert(
    input = normalizePath(md_file),
    from = app_fn("pandoc_markdown_reader")(),
    to = "html5",
    output = html_file,
    options = c("--lua-filter", filter)
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

# ---- Markdown / PDF export: the file and the name must agree ---------------
#
# The handler made its temp file with the extension of the REQUESTED format, so
# a PDF export wrote Markdown into "dta-export-*.pdf" and then derived the PDF
# path from it with sub("\.md$", ".pdf", ...) -- the same path. Pandoc was then
# told to read and write one file, and when no PDF route worked the Markdown
# text was delivered under a ".pdf" name.

first_bytes <- function(path, n = 5L) {
  rawToChar(readBin(path, "raw", n = n))
}

# Drive the export modal's Export button in markdown mode with "PDF" ticked and
# hand back what the server queued for download. The file is read INSIDE the
# server block: the app deletes its export files when the session ends, which
# is before testServer() returns.
run_markdown_pdf_export <- function() {
  out <- list()
  shiny::testServer(.shiny_app_dir(), {
    rv$dta <- app_fixture_dta()
    session$setInputs(
      export_format = "markdown",
      export_as_pdf = TRUE,
      export_include_yaml_md = FALSE
    )
    session$setInputs(export_do = 1)
    out$path <<- export_state$file_path
    out$name <<- export_state$file_name
    out$exists <<- file.exists(out$path)
    out$head <<- if (out$exists) first_bytes(out$path) else ""
    out$text <<- if (out$exists) paste(readLines(out$path, warn = FALSE, n = 200L), collapse = "\n") else ""
  })
  out
}

test_that("a PDF export without pandoc, LaTeX or a browser still delivers a PDF", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("shinyjs")
  skip_if_not_installed("rmarkdown")
  # No pandoc rules out routes 1 and 2; R's own device must still deliver.
  local_mocked_bindings(pandoc_available = function(...) FALSE, .package = "rmarkdown")

  res <- run_markdown_pdf_export()

  expect_equal(tolower(tools::file_ext(res$name)), "pdf")
  expect_equal(tolower(tools::file_ext(res$path)), "pdf")
  expect_equal(res$head, "%PDF-")
})

test_that("a PDF export that cannot make a PDF delivers Markdown under a .md name", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("shinyjs")
  skip_if_not_installed("rmarkdown")
  # No pandoc, and R's own PDF devices fail too: the documented fallback.
  local_mocked_bindings(pandoc_available = function(...) FALSE, .package = "rmarkdown")
  local_mocked_bindings(
    cairo_pdf = function(...) stop("no device"),
    pdf = function(...) stop("no device"),
    .package = "grDevices"
  )

  res <- run_markdown_pdf_export()

  expect_equal(tolower(tools::file_ext(res$name)), "md")
  expect_equal(tolower(tools::file_ext(res$path)), "md")
  expect_true(res$exists)
  expect_false(startsWith(res$head, "%PDF"))
  expect_match(res$text, "#", fixed = TRUE)
})

test_that("a PDF export that succeeds delivers a real PDF under a .pdf name", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("shinyjs")
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc is not available")
  skip_if_not(
    app_fn("has_pdf_engine")() || nzchar(app_fn("find_chrome_binary")()),
    "no PDF engine or Chrome/Edge is available"
  )

  res <- run_markdown_pdf_export()

  # Whichever route produced it, the extension and the content agree.
  expect_equal(tolower(tools::file_ext(res$name)), "pdf")
  expect_equal(tolower(tools::file_ext(res$path)), "pdf")
  expect_true(res$exists)
  expect_equal(res$head, "%PDF-")
})

test_that("embed_yaml_markdown keeps a '-->' in the document from closing the comment", {
  # An HTML comment ends at the first "-->"; a title carrying one would
  # otherwise expose the rest of the YAML as live markdown/HTML.
  dta <- app_fixture_dta()
  dta@metadata@title <- "a --> <img src=x> --!> b"

  out <- app_fn("embed_yaml_markdown")("", dta)

  closers <- gregexpr("--!?>", out)[[1]]
  expect_length(closers, 1)
  expect_true(endsWith(out, "END EMBEDDED DTA YAML ========== -->"))
  expect_match(out, "a --&gt; <img src=x> --!&gt; b", fixed = TRUE)
})

test_that("pandoc_markdown_filter turns planted images into their alt text", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  filter <- app_fn("pandoc_markdown_filter")()
  on.exit(unlink(filter), add = TRUE)
  md <- tempfile(fileext = ".md")
  html <- tempfile(fileext = ".html")
  on.exit(unlink(c(md, html)), add = TRUE)
  writeLines(
    "**Title:** ![alt words](http://169.254.169.254/x.png) and ![](file:///etc/x.png)",
    md
  )

  rmarkdown::pandoc_convert(
    md,
    from = app_fn("pandoc_markdown_reader")(), to = "html5",
    output = html, options = c("--lua-filter", filter)
  )
  out <- paste(readLines(html, warn = FALSE), collapse = "\n")

  expect_false(grepl("<img", out, fixed = TRUE))
  expect_false(grepl("169.254.169.254", out, fixed = TRUE))
  expect_match(out, "alt words", fixed = TRUE)
})

# ---- Markdown -> PDF: what the package's Markdown needs from the PDF routes ----
#
# write_dta(format = "md") writes GFM with a linked, numbered table of contents,
# entities ("&amp;") and backslash escapes in headings and cells, and "<br>"
# for a line break inside a table cell. These tests run the app's pandoc setup
# (reader + filter) to latex/html5/native, which needs pandoc but no LaTeX, and
# the built-in renderer of the last-resort route. Every expectation is written
# out from the Markdown/GFM rules, not copied from an output.

# Convert `lines` with the app's reader and filter; returns the output as one
# string.
md_via_app_pandoc <- function(lines, to = "native") {
  md <- withr::local_tempfile(fileext = ".md", .local_envir = parent.frame())
  out <- withr::local_tempfile(fileext = ".out", .local_envir = parent.frame())
  filter <- app_fn("pandoc_markdown_filter")()
  withr::defer(unlink(filter), envir = parent.frame())
  writeLines(lines, md, useBytes = TRUE)
  rmarkdown::pandoc_convert(
    input = md, from = app_fn("pandoc_markdown_reader")(), to = to,
    output = out, options = c("--lua-filter", filter)
  )
  paste(readLines(out, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

count_matches <- function(pattern, x) {
  m <- gregexpr(pattern, x, fixed = TRUE)[[1]]
  if (m[[1]] == -1L) 0L else length(m)
}

test_that("the pandoc reader of the PDF routes is GFM", {
  # The table of contents links to GFM heading ids ("## 1 Title" -> "1-title");
  # pandoc's own dialect would derive "title" and break every link.
  expect_identical(app_fn("pandoc_markdown_reader")(), "gfm")
})

test_that("<br> in a table cell becomes a line break instead of vanishing", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  native <- md_via_app_pandoc(c(
    "| Name | Contact |",
    "| --- | --- |",
    "| Emily | e@x.org<br>321-654<br/>Berlin<BR /> |"
  ))

  # <br>, <br/> and the upper-case, spaced form are three breaks.
  expect_identical(count_matches("LineBreak", native), 3L)
  expect_false(grepl("RawInline", native, fixed = TRUE))
  expect_false(grepl("<br", native, fixed = TRUE))
  # The three lines are still there, in order.
  expect_match(native, "(?s)e@x.org.*LineBreak.*321-654.*LineBreak.*Berlin", perl = TRUE)

  # In LaTeX the cell is typeset as separate lines (pandoc's vtop/hbox form).
  tex <- md_via_app_pandoc(c(
    "| Name | Contact |", "| --- | --- |", "| Emily | e@x.org<br>321-654 |"
  ), to = "latex")
  expect_match(tex, "hbox{\\strut", fixed = TRUE)
  expect_false(grepl("<br", tex, fixed = TRUE))
})

test_that("a signature cell of only line breaks keeps its height", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  # pandoc drops trailing line breaks in a cell; the filter leaves a
  # non-breaking space (U+00A0, shown as "\160" in native) after them so the
  # row stays tall enough to sign in.
  native <- md_via_app_pandoc(c(
    "| Name | Signature |", "| --- | --- |", "| Alice | <br><br> |"
  ))
  expect_identical(count_matches("LineBreak", native), 2L)
  expect_match(native, "Str \"\\160\"", fixed = TRUE)
})

test_that("raw HTML other than <br> is dropped, its text kept", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  native <- md_via_app_pandoc(c(
    "Text <b>bold</b> and <script>alert(1)</script> and <img src=x onerror=y> end.",
    "",
    "<div>block</div>",
    "",
    "<!-- a comment -->"
  ))
  expect_false(grepl("RawInline|RawBlock", native))
  expect_false(grepl("script|onerror|<div", native))
  expect_match(native, "bold", fixed = TRUE)
  expect_match(native, "end.", fixed = TRUE)
})

test_that("maths and metadata in document text do not reach LaTeX", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  backslash <- rawToChar(as.raw(92))
  tex <- md_via_app_pandoc(c(
    "---",
    paste0("header-includes: ", backslash, "input{/etc/passwd}"),
    "---",
    "",
    paste0("A formula $", backslash, "input{/etc/hostname}$ and $$", backslash, "input{/etc/shadow}$$ and ", backslash, "$5.")
  ), to = "latex")
  expect_false(grepl(paste0(backslash, "input{/"), tex, fixed = TRUE))
  expect_false(grepl("/etc/passwd", tex, fixed = TRUE))
  # The text is still there, as text: the dollar signs are literal.
  expect_match(tex, "A formula", fixed = TRUE)
})

test_that("links to headings stay working internal links in LaTeX and HTML", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  md <- c(
    "## Contents",
    "",
    "- [1 Approval &amp; Signatures](#1-approval--signatures)",
    "  - [1.1 Sub](#11-sub)",
    "",
    "## 1 Approval &amp; Signatures",
    "",
    "### 1.1 Sub",
    "",
    "Body."
  )
  # GFM ids: lower-case, "&" dropped, spaces to "-" (so "& " leaves "--").
  tex <- md_via_app_pandoc(md, to = "latex")
  expect_match(tex, "\\hyperref[1-approval--signatures]{1 Approval \\& Signatures}", fixed = TRUE)
  expect_match(tex, "\\label{1-approval--signatures}", fixed = TRUE)
  expect_match(tex, "\\hyperref[11-sub]{1.1 Sub}", fixed = TRUE)
  expect_match(tex, "\\label{11-sub}", fixed = TRUE)

  html <- md_via_app_pandoc(md, to = "html5")
  expect_match(html, "href=\"#1-approval--signatures\"", fixed = TRUE)
  expect_match(html, "id=\"1-approval--signatures\"", fixed = TRUE)
})

test_that("entities and backslash escapes are resolved by the pandoc route", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  native <- md_via_app_pandoc(c(
    "## R &amp; D &lt;draft&gt;",
    "",
    "| Item | Details |",
    "| --- | --- |",
    "| clinical\\_data | a\\|b \\* c \\$5 |"
  ))
  expect_match(native, "Str \"&\"", fixed = TRUE)
  expect_match(native, "Str \"<draft>\"", fixed = TRUE)
  expect_match(native, "Str \"clinical_data\"", fixed = TRUE)
  expect_match(native, "Str \"a|b\"", fixed = TRUE)
  expect_match(native, "Str \"$5\"", fixed = TRUE)
  expect_false(grepl("&amp;|&lt;|\\\\_", native))
})

test_that("the package's own Markdown goes through the pandoc route intact", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  md <- withr::local_tempfile(fileext = ".md")
  DTAtools::write_dta(app_fixture_dta(), md, format = "md", overwrite = TRUE, quiet = TRUE)
  lines <- readLines(md, warn = FALSE, encoding = "UTF-8")
  # The document has what the filter exists for.
  expect_true(any(grepl("<br>", lines, fixed = TRUE)))
  expect_true(any(grepl("](#", lines, fixed = TRUE)))

  tex <- md_via_app_pandoc(lines, to = "latex")
  expect_false(grepl("<br", tex, fixed = TRUE))
  expect_false(grepl("&amp;|&lt;|&gt;", tex))
  # Every internal link has a heading to land on.
  targets <- unique(regmatches(tex, gregexpr("(?<=hyperref\\[)[^]]+", tex, perl = TRUE))[[1]])
  labels <- unique(regmatches(tex, gregexpr("(?<=\\\\label\\{)[^}]+", tex, perl = TRUE))[[1]])
  expect_gt(length(targets), 0L)
  expect_setequal(setdiff(targets, labels), character(0))
})

# ---- built-in renderer (route 3) ----------------------------------------------

# Draw `lines` with dta_render_markdown() on a null device and return what was
# drawn: one row per call of graphics::text() (a word, or a line of a cell),
# with its position, size (cex) and font (1 plain, 2 bold, 3 italic, 4 both).
# `ascii` is the transliteration of the fallback device (the bullet glyph is
# not in the fonts of a plain pdf() device); by default the bullet becomes "*".
render_drawn <- function(lines, ascii = function(x) gsub("\u2022", "*", x)) {
  drawn <- list()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  testthat::with_mocked_bindings(
    app_fn("dta_render_markdown")(lines, ascii = ascii),
    text = function(x, y = NULL, labels = NULL, cex = 1, font = 1, ...) {
      drawn[[length(drawn) + 1L]] <<- data.frame(
        x = x, y = y, label = as.character(labels), cex = cex, font = font,
        stringsAsFactors = FALSE
      )
      invisible(NULL)
    },
    .package = "graphics"
  )
  do.call(rbind, drawn)
}

test_that("dta_md_inline turns links into their text and <br> into a line feed", {
  inline <- app_fn("dta_md_inline")

  expect_identical(inline("[1 Title](#1-title)"), "1 Title")
  expect_identical(inline("See [the site](https://example.org/a_b) now"), "See the site now")
  expect_identical(inline("![alt words](http://x/y.png)"), "alt words")
  expect_identical(inline("a<br>b"), "a\nb")
  expect_identical(inline("a<br/>b<BR />c"), "a\nb\nc")
  expect_identical(inline("<br><br>"), "\n\n")
  expect_identical(inline("<b>x</b> and <script>y</script>"), "x and y")
  expect_identical(inline("<https://example.org/p>"), "https://example.org/p")
  expect_identical(inline("<!-- hidden -->shown"), "shown")
  # A link text may contain escapes and entities; they survive for decoding.
  expect_identical(inline("[clinical\\_data &amp; more](#x)"), "clinical\\_data &amp; more")
  expect_identical(inline(""), "")
  expect_identical(inline(NA_character_), "")
})

test_that("dta_md_inline never treats escaped or entity-encoded markup as markup", {
  inline <- app_fn("dta_md_inline")
  decode <- app_fn("dta_md_decode")

  # An escaped "<" is text, not a tag; an entity is text, not a break.
  expect_identical(decode(inline("\\<br\\>")), "<br>")
  expect_identical(decode(inline("&lt;br&gt;")), "<br>")
  expect_identical(decode(inline("&lt;script&gt;")), "<script>")
  expect_identical(decode(inline("\\[x\\](y)")), "[x](y)")
})

test_that("dta_md_decode resolves backslash escapes and entities, once", {
  decode <- app_fn("dta_md_decode")

  expect_identical(decode("R &amp; D"), "R & D")
  expect_identical(decode("&lt;a&gt; &quot;q&quot; &apos;s&apos;"), "<a> \"q\" 's'")
  expect_identical(decode("&#65;&#x42;&#X43;"), "ABC")
  expect_identical(decode("clinical\\_data\\*.csv\\$"), "clinical_data*.csv$")
  expect_identical(decode("a\\|b"), "a|b")
  expect_identical(decode("back\\\\slash"), "back\\slash")
  # An escaped ampersand is a literal "&", so the rest stays literal too.
  expect_identical(decode("\\&amp;"), "&amp;")
  # Unknown or malformed entities and a lone backslash are left alone.
  expect_identical(decode("&unknownthing; &#0; &#xD800; &"), "&unknownthing; &#0; &#xD800; &")
  expect_identical(decode("end\\"), "end\\")
  expect_identical(decode("plain text"), "plain text")
})

test_that("dta_md_split_cells splits a table row on unescaped pipes only", {
  split <- app_fn("dta_md_split_cells")

  expect_identical(split("| a | b | c |"), c("a", "b", "c"))
  expect_identical(split("a | b"), c("a", "b"))
  expect_identical(split("| a\\|b | c |"), c("a\\|b", "c"))
  expect_identical(split("| a |  |"), c("a", ""))
  expect_identical(split("|  | b |"), c("", "b"))
  # An escaped backslash before the closing pipe does not escape the pipe.
  expect_identical(split("| a\\\\ |"), "a\\\\")
  expect_identical(split("| x<br>y | z |"), c("x<br>y", "z"))
})

test_that("the built-in renderer draws links, entities, escapes and cell breaks as readable text", {
  drawn <- render_drawn(c(
    "# Data Transfer Agreement",
    "",
    "**Clinical Data Specification**",
    "",
    "## Contents",
    "",
    "- [1 Approval &amp; Signatures](#1-approval--signatures)",
    "  - [1.1 Sub &lt;part&gt;](#11-sub-part)",
    "",
    "## 1 Approval &amp; Signatures",
    "",
    "| Name | Contact | Note |",
    "| --- | --- | --- |",
    "| Emily | a@b.com<br>321-654 | clinical\\_data a\\|b |",
    "| Sign | <br><br> | **x** |"
  ))
  labels <- drawn$label

  # Resolved: the words of the links and headings are drawn, nothing of the
  # Markdown syntax around them.
  expect_true(all(c("Approval", "&", "Signatures", "Sub", "<part>") %in% labels))
  expect_false(any(grepl("](", labels, fixed = TRUE)))
  expect_false(any(grepl("#1-approval", labels, fixed = TRUE)))
  expect_false(any(grepl("&amp;|&lt;|&gt;", labels)))
  expect_false(any(grepl("<br", labels, fixed = TRUE)))
  expect_false(any(grepl("\\", labels, fixed = TRUE)))
  # Cell text with escapes comes out as the plain characters.
  expect_true("clinical_data a|b" %in% labels)
  expect_true("x" %in% labels)

  # The <br> in the contact cell is a line break: two lines, one under the other.
  e1 <- drawn[drawn$label == "a@b.com", ]
  e2 <- drawn[drawn$label == "321-654", ]
  expect_equal(nrow(e1), 1L)
  expect_equal(nrow(e2), 1L)
  expect_equal(e1$x, e2$x)
  expect_lt(e2$y, e1$y)
})

test_that("a <br> in running text starts a new line in the built-in renderer", {
  drawn <- render_drawn(c("First line<br>second line"))
  first <- drawn[drawn$label == "First", ]
  second <- drawn[drawn$label == "second", ]
  expect_lt(second$y, first$y)
  expect_equal(second$x, drawn$x[1])
})

test_that("nested list items are indented further than their parents in the built-in renderer", {
  drawn <- render_drawn(c("- parent", "  - child"))
  # The bullet glyph is drawn first, then the item's words.
  parent <- drawn[drawn$label == "parent", ]
  child <- drawn[drawn$label == "child", ]
  expect_gt(child$x, parent$x)
  expect_lt(child$y, parent$y)
})

test_that("the package's own Markdown renders in the built-in renderer without syntax left over", {
  md <- withr::local_tempfile(fileext = ".md")
  DTAtools::write_dta(app_fixture_dta(), md, format = "md", overwrite = TRUE, quiet = TRUE)
  lines <- readLines(md, warn = FALSE, encoding = "UTF-8")
  expect_true(any(grepl("<br>", lines, fixed = TRUE)))

  # Draw with the transliteration the non-Unicode fallback device uses, so the
  # base pdf() device can take the typographic quotes of the rule wording.
  drawn <- list()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  ascii <- function(x) iconv(enc2utf8(as.character(x)), "UTF-8", "ASCII", sub = "?")
  testthat::with_mocked_bindings(
    app_fn("dta_render_markdown")(lines, ascii = ascii),
    text = function(x, y = NULL, labels = NULL, ...) {
      drawn[[length(drawn) + 1L]] <<- as.character(labels)
      invisible(NULL)
    },
    .package = "graphics"
  )
  labels <- unlist(drawn)

  expect_gt(length(labels), 50L)
  expect_false(any(grepl("](#", labels, fixed = TRUE)))
  expect_false(any(grepl("<br", labels, fixed = TRUE)))
  expect_false(any(grepl("&amp;|&lt;|&gt;", labels)))
  expect_false(any(grepl("\\\\[_*|$]", labels)))
  # Autolinks, rule headings and the italic rule kind leave no syntax behind:
  # no address in angle brackets, no run of hashes, no asterisk around a word
  # (a file-name pattern such as "clinical_data.*.csv$" keeps its own).
  expect_false(any(grepl("[<>]|mailto:|^#+$", labels)))
  expect_false(any(grepl("^\\*[A-Za-z(]|[A-Za-z)]\\*$", labels)))
})

test_that("dta_md_inline shows autolinks as their address, whatever the scheme", {
  inline <- app_fn("dta_md_inline")

  # A bare e-mail address, a mailto: link and other schemes: the address only,
  # without the angle brackets and without "mailto:".
  expect_identical(inline("<a@b.org>"), "a@b.org")
  expect_identical(inline("<emily.turner@testcompany2.com>"), "emily.turner@testcompany2.com")
  expect_identical(inline("<first+tag_x@sub.example.co.uk>"), "first+tag_x@sub.example.co.uk")
  expect_identical(inline("<mailto:a@b.org>"), "a@b.org")
  expect_identical(inline("<MAILTO:a@b.org>"), "a@b.org")
  expect_identical(inline("<ftp://files.example.org/in/data.csv>"), "ftp://files.example.org/in/data.csv")
  expect_identical(inline("<sftp://host/x> <https://e.org> <http://e.org/a?b=1>"), "sftp://host/x https://e.org http://e.org/a?b=1")
  # An autolink followed by <br> keeps its break; "<br" as a local part is an address.
  expect_identical(inline("<a@b.org><br>321-654"), "a@b.org\n321-654")
  expect_identical(inline("<br@x.org>"), "br@x.org")
  # Real tags are still dropped, and a less-than sign that starts no autolink is not one.
  expect_identical(inline("<b>x</b> <span class=\"c\">y</span>"), "x y")
  expect_identical(inline("<not an autolink@b.org>"), "")
  # Escaped brackets are text, not an autolink.
  expect_identical(app_fn("dta_md_decode")(inline("\\<a@b.org\\>")), "<a@b.org>")
})

test_that("the built-in renderer draws *italic*, **bold** and both, without the asterisks", {
  drawn <- render_drawn(c(
    "Plain *slanted* and **heavy** and **heavy *both* heavy** end.",
    "",
    "Literal \\*stars\\* and 5 * 3 * 2 stay.",
    "",
    "| Cell | Other |",
    "| --- | --- |",
    "| *it* and **bo** | x |"
  ))
  font_of <- function(label) drawn$font[drawn$label == label]

  expect_identical(font_of("Plain"), 1)
  expect_identical(font_of("slanted"), 3)
  expect_identical(font_of("heavy"), c(2, 2, 2))
  expect_identical(font_of("both"), 4)
  expect_identical(font_of("end."), 1)
  expect_false(any(grepl("*", c("Plain", "slanted", "heavy", "both", "end."), fixed = TRUE)))
  # No asterisk of the markup is left anywhere in the drawn text.
  markup_left <- grepl("*", drawn$label, fixed = TRUE) &
    !drawn$label %in% c("*stars*", "*", "5 * 3 * 2")
  expect_false(any(markup_left))
  # An escaped or space-padded asterisk is the character itself.
  expect_true("*stars*" %in% drawn$label)
  expect_equal(sum(drawn$label == "*"), 2L)
  # A table cell shows the words only; cells are one font per column.
  expect_true("it and bo" %in% drawn$label)
})

test_that("a line that is one italic span is drawn as a small grey caption", {
  drawn <- render_drawn(c("*Range*", "*Conditional (IF/THEN)*", "*one* and *two*"))

  expect_identical(drawn$label[1:3], c("Range", "Conditional", "(IF/THEN)"))
  expect_true(all(drawn$font[1:3] == 3))
  expect_true(all(drawn$cex[1:3] < 1))
  # Two italic spans in one line are running text, not a caption: normal size.
  expect_identical(drawn$label[4:6], c("one", "and", "two"))
  expect_identical(drawn$font[4:6], c(3, 1, 3))
  expect_true(all(drawn$cex[4:6] == 1))
})

test_that("headings of every level 1-6 are drawn in graded sizes without their hashes", {
  lines <- unlist(lapply(1:6, function(k) {
    c(paste(strrep("#", k), paste0("Level", k, " name\\_x")), "")
  }))
  drawn <- render_drawn(lines)
  first_words <- drawn[grepl("^Level", drawn$label), ]

  expect_identical(first_words$label, paste0("Level", 1:6))
  # Strictly smaller from level to level, all bold.
  expect_true(all(diff(first_words$cex) < 0))
  expect_true(all(first_words$font == 2))
  expect_true(all(drawn$font[drawn$label == "name_x"] == 2))
  expect_false(any(grepl("#", drawn$label, fixed = TRUE)))
  expect_false(any(grepl("\\", drawn$label, fixed = TRUE)))
  # Headings are drawn below one another, in reading order.
  expect_true(all(diff(first_words$y) < 0))
})

test_that("a rule heading, an emphasised heading and a closing hash run are drawn as titles", {
  drawn <- render_drawn(c(
    "##### rule\\_equal\\_example",
    "##### File format",
    "## Title with *slant* and <a@b.org> ##",
    "####### seven hashes are text"
  ))

  expect_true(all(c("rule_equal_example", "File", "format") %in% drawn$label))
  expect_true(all(drawn$font[drawn$label %in% c("File", "format")] == 2))
  # An italic word inside a heading keeps its slant on top of the bold.
  expect_identical(drawn$font[drawn$label == "slant"], 4)
  expect_true("a@b.org" %in% drawn$label)
  expect_false("##" %in% drawn$label)
  # Seven hashes are not a heading: plain body text, hashes included.
  expect_true("#######" %in% drawn$label)
})

test_that("a bold title is followed by its list, with a bullet before every item", {
  drawn <- render_drawn(
    c(
      "**How to read the rules**", "",
      "- Each rule is checked separately.",
      "- Text values are compared exactly.",
      "  - nested point",
      "",
      "1. First step", "2) Second step"
    ),
    ascii = function(x) gsub("\u2022", "~", x)
  )

  title <- drawn[drawn$label == "How", ]
  expect_identical(title$font, 2)
  bullets <- drawn[drawn$label == "~", ]
  expect_equal(nrow(bullets), 3L)
  # Each bullet sits left of its item's first word and on the same line.
  for (w in c("Each", "Text", "nested")) {
    word <- drawn[drawn$label == w, ]
    near <- bullets[abs(bullets$y - word$y) < 1e-9, ]
    expect_equal(nrow(near), 1L, info = w)
    expect_lt(near$x, word$x)
  }
  # The bulleted lines come under the title, in order; the nested one is indented.
  expect_lt(drawn$y[drawn$label == "Each"], title$y)
  expect_lt(drawn$y[drawn$label == "Text"], drawn$y[drawn$label == "Each"])
  expect_gt(drawn$x[drawn$label == "nested"], drawn$x[drawn$label == "Text"])
  # Numbered items carry their number instead of a bullet.
  expect_true(all(c("1.", "2)") %in% drawn$label))
  expect_lt(drawn$x[drawn$label == "1."], drawn$x[drawn$label == "First"])
  expect_equal(drawn$y[drawn$label == "1."], drawn$y[drawn$label == "First"])
})

test_that("autolinks, escapes, entities and <br> in table cells are shown as plain text", {
  drawn <- render_drawn(c(
    "| Name | Contact |",
    "| --- | --- |",
    "| A | <a@b.org><br><ftp://f.example.org/x><br>**tel** 1 |",
    "| B | x &lt;br&gt; y &amp; z \\| w \\<k\\> |"
  ))
  labels <- drawn$label

  expect_true(all(c("a@b.org", "ftp://f.example.org/x", "tel 1") %in% labels))
  # The three lines of the first contact cell are one under the other.
  l1 <- drawn[drawn$label == "a@b.org", ]
  l2 <- drawn[drawn$label == "ftp://f.example.org/x", ]
  l3 <- drawn[drawn$label == "tel 1", ]
  expect_equal(l1$x, l2$x)
  expect_lt(l2$y, l1$y)
  expect_lt(l3$y, l2$y)
  # Entities and escapes are the characters themselves; "&lt;br&gt;" is not a break.
  expect_true("x <br> y & z | w <k>" %in% labels)
  expect_false(any(grepl("mailto|&amp;|&lt;", labels)))
  expect_false(any(grepl("\\", labels, fixed = TRUE)))
})

test_that("the specification Markdown with rule headings and autolinks renders without syntax left over", {
  lines <- c(
    "# Data Transfer Agreement", "",
    "**Clinical Data Specification**", "",
    "## Contents", "",
    "- [1 Approval and Signatures](#1-approval-and-signatures)",
    "- [3 Parties](#3-parties)",
    "  - [3.1 Supplier](#31-supplier)",
    "  - [5.1 Clinical data table (clinical\\_data)](#51-clinical-data-table-clinical_data)", "",
    "## 3 Parties", "",
    "### 3.1 Supplier", "",
    "| Name | Role | Contact | Responsibilities |",
    "| --- | --- | --- | --- |",
    paste0(
      "| Emily Turner | Senior Data Manager | ",
      "<emily@x.org><br>321-654-0987 | Signatory<br>Reviewer |"
    ),
    "| Bob | Analyst | <mailto:bob@x.org><br><ftp://ftp.x.org/in> | |",
    "| Alice | Lead | <br><br> | R &amp; D |", "",
    "## 5 Datasets", "",
    "### 5.1 Clinical data table (clinical\\_data)", "",
    "#### 5.1.1 Files", "",
    "| File Name / Pattern | Match Type |",
    "| --- | --- |",
    "| clinical\\_data.\\*.csv\\$ | Regex pattern |", "",
    "##### File format", "",
    "| File | Delimiter | Quote |",
    "| --- | --- | --- |",
    "| clinical\\_data.\\*.csv\\$ | Comma ( , ) | Double quote ( \" ) |", "",
    "#### 5.1.3 Validation Rules", "",
    "**How to read the rules**", "",
    "- Each rule is checked separately for every file of the dataset.",
    "- Tests such as greater than read the value as a number, such as 1e3 for 1000.", "",
    "##### rule\\_equal\\_example", "",
    "*Conditional (IF/THEN)*", "",
    "When VISIT is equal to \u201cV03\u201d, STATUS must be equal to \u201cCOMPLETED\u201d.", "",
    "**Missing values:**", "",
    "- If VISIT is missing, the condition is not met.", "",
    "##### rule\\_range\\_example", "",
    "*Range*", "",
    "Every value in column AGE must be a number from 18 to 65, inclusive.", "",
    "**Missing values:** Not checked.", "",
    "---", "",
    "Clinical Data Specification \u00b7 Version 0.1 \u00b7 2024-06-19"
  )
  ascii <- function(x) {
    x <- gsub("\u2022", "~", x)
    x <- gsub("[\u201c\u201d]", "\"", x)
    iconv(enc2utf8(as.character(x)), "UTF-8", "ASCII", sub = "?")
  }
  drawn <- render_drawn(lines, ascii = ascii)
  labels <- drawn$label

  expect_gt(length(labels), 100L)
  # The only angle bracket, backslash, asterisk or dollar sign in the drawn text
  # is the file-name pattern, whose characters are escaped in the Markdown.
  pattern <- "clinical_data.*.csv$"
  expect_true(pattern %in% labels)
  rest <- labels[labels != pattern]
  expect_false(any(grepl("[<>\\\\*#]", rest)))
  expect_false(any(grepl("&amp;|&lt;|&gt;|mailto", rest)))
  expect_false(any(grepl("](", labels, fixed = TRUE)))
  # What the Markdown wrote as autolinks and cell breaks is readable text.
  expect_true(all(c(
    "emily@x.org", "321-654-0987", "bob@x.org", "ftp://ftp.x.org/in"
  ) %in% labels))
  # Rule headings are titles; the rule kind is a grey italic caption.
  expect_identical(drawn$font[drawn$label == "rule_equal_example"], 2)
  expect_identical(drawn$font[drawn$label == "Range"], 3)
  expect_lt(drawn$cex[drawn$label == "Range"], 1)
  expect_identical(drawn$font[drawn$label == "format"], 2)
  expect_equal(drawn$cex[drawn$label == "format"], 1)
  # The list under the bold title: bullets before every item.
  expect_gte(sum(labels == "~"), 3L)
})

test_that("csv_formula_safe defuses formula-leading text cells and leaves the rest", {
  df <- data.frame(
    column = c("=HYPERLINK(\"http://x\")", "+1", "-2", "@SUM(A1)", "\tx", "plain", NA),
    row = c(1L, 2L, 3L, 4L, 5L, 6L, 7L),
    table = factor(c("=t", "t", "t", "t", "t", "t", "t")),
    stringsAsFactors = FALSE
  )

  out <- app_fn("csv_formula_safe")(df)

  expect_identical(
    out$column,
    c("'=HYPERLINK(\"http://x\")", "'+1", "'-2", "'@SUM(A1)", "'\tx", "plain", NA)
  )
  expect_identical(out$row, df$row)
  expect_identical(out$table, c("'=t", rep("t", 6)))
})

# ---- the app's own PDF chain (dta_export_pdf) --------------------------------
# soffice -> pandoc -> R's own graphics device. None of it was reached by any
# test before; the external converters are mocked away so the built-in route,
# the one every machine without LibreOffice or LaTeX takes, is what runs.

test_that("dta_is_pdf() accepts a PDF and rejects anything else", {
  is_pdf <- app_fn("dta_is_pdf")
  pdf_file <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(pdf_file)
  graphics::plot.new()
  grDevices::dev.off()
  expect_true(is_pdf(pdf_file))

  txt <- withr::local_tempfile(fileext = ".pdf")
  writeLines("not a pdf at all", txt)
  expect_false(is_pdf(txt))
  expect_false(is_pdf(file.path(tempdir(), "no-such-file.pdf")))
  expect_false(is_pdf(NULL))
})

test_that("dta_markdown_to_pdf() renders a long document onto several pages", {
  out <- withr::local_tempfile(fileext = ".pdf")
  lines <- c(
    "# Heading", "", "**Label:** value", "", "- a bullet",
    "| A | B |", "|---|---|", "| 1 | 2 |", "---",
    rep("A line of ordinary body text that takes up its own line.", 150)
  )
  app_fn("dta_markdown_to_pdf")(lines, out)
  expect_true(app_fn("dta_is_pdf")(out))

  # Pagination, checked on a device whose output can be read back:
  # dta_markdown_to_pdf() prefers cairo_pdf(), which packs its page objects into
  # compressed streams, so draw the same lines with the renderer it uses onto an
  # uncompressed pdf() and count "/Type /Page" (not "/Type /Pages") there.
  plain <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(plain, width = 8.27, height = 11.69, compress = FALSE)
  app_fn("dta_render_markdown")(lines)
  grDevices::dev.off()
  bytes <- readBin(plain, "raw", file.size(plain))
  pages <- length(grepRaw("/Type[[:space:]]*/Page[^s]", bytes, all = TRUE))
  expect_gt(pages, 1)
})

test_that("dta_markdown_to_pdf() falls back to pdf() when cairo only warns that it cannot load", {
  # macOS without XQuartz: capabilities("cairo") is TRUE, cairo_pdf() warns
  # "failed to load cairo DLL" and opens no device.
  testthat::local_mocked_bindings(
    cairo_pdf = function(...) {
      warning("failed to load cairo DLL")
      invisible(NULL)
    },
    .package = "grDevices"
  )
  out <- withr::local_tempfile(fileext = ".pdf")
  app_fn("dta_markdown_to_pdf")(c("# Heading", "", "Some body text."), out)
  expect_true(app_fn("dta_is_pdf")(out))
})

test_that("without LibreOffice or pandoc, the PDF is rendered from the full Markdown export", {
  env <- app_env()
  rendered <- NULL
  # rlang::local_bindings(), not testthat::local_mocked_bindings(): the latter
  # resolves `.env` to its top-level namespace (shiny's, for app_env()) and so
  # cannot see the app helpers. This patches app_env() itself and restores it
  # when the test ends, so the shared environment is left as it was found.
  rlang::local_bindings(
    dta_find_soffice = function() "",
    dta_pandoc_to_pdf = function(docx, file) FALSE,
    dta_markdown_to_pdf = function(lines, file) {
      rendered <<- lines
      grDevices::pdf(file)
      graphics::plot.new()
      grDevices::dev.off()
      invisible(file)
    },
    .env = env
  )
  dta <- create_example_DTA()
  out <- withr::local_tempfile(fileext = ".pdf")
  app_fn("dta_export_pdf")(dta, out)

  expect_true(app_fn("dta_is_pdf")(out))
  md <- withr::local_tempfile(fileext = ".md")
  write_dta(dta, file = md, format = "md", include_signatures = FALSE, signature_list = NULL, quiet = TRUE)
  expected <- readLines(md, warn = FALSE)
  # The whole export reached the renderer -- not a stand-in page. Only the
  # generation timestamp may differ between the two writes.
  stamp <- function(x) grep("Generated", x, invert = TRUE, value = TRUE)
  expect_identical(stamp(rendered), stamp(expected))
  expect_false(any(grepl("could not be generated", rendered, fixed = TRUE)))
})

test_that("when the Markdown export fails too, the PDF export fails instead of writing a stub", {
  env <- app_env()
  real_write_dta <- DTAtools::write_dta
  rlang::local_bindings(
    dta_find_soffice = function() "",
    dta_pandoc_to_pdf = function(docx, file) FALSE,
    .env = env
  )
  testthat::local_mocked_bindings(
    write_dta = function(dta, file, format = "docx", ...) {
      if (identical(format, "md")) stop("markdown writer exploded")
      real_write_dta(dta, file = file, format = format, ...)
    },
    .package = "DTAtools"
  )
  out <- withr::local_tempfile(fileext = ".pdf")
  writeLines("stale bytes", out)

  res <- app_fn("dta_export")(create_example_DTA(), out, "pdf")
  expect_false(res$ok)
  expect_match(res$error, "Could not produce a PDF", fixed = TRUE)
  expect_match(res$error, "markdown writer exploded", fixed = TRUE)
  expect_false(file.exists(out))
})

# ---- Library word templates and the config-driven export options -------------

app_fixture_template <- function() {
  path <- system.file("extdata", "word-templates", "clinical_dta_template.docx", package = "DTAtools")
  expect_true(nzchar(path))
  path
}

# A normalised app config built from a real dta-app.yaml in a temp library ROOT
# that holds a copy of the packaged Word template as export-templates/our_dta.docx.
library_cfg <- function(yaml_lines, .local_envir = parent.frame()) {
  lib <- withr::local_tempdir(.local_envir = .local_envir)
  dir.create(file.path(lib, "export-templates"))
  expect_true(file.copy(app_fixture_template(), file.path(lib, "export-templates", "our_dta.docx")))
  writeLines(yaml_lines, file.path(lib, "dta-app.yaml"))
  list(dir = lib, cfg = app_fn("parse_dta_app_config")(file.path(lib, "dta-app.yaml"))$config)
}

# A normalised config from `exports:` lines alone.
exports_cfg <- function(..., .local_envir = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".yaml", .local_envir = .local_envir)
  writeLines(c("exports:", paste0("  ", c(...))), f)
  app_fn("parse_dta_app_config")(f)$config
}

test_that("list_available_templates adds library templates after the bundled ones, with config labels", {
  lc <- library_cfg(c(
    "exports:",
    "  word_templates:",
    "    - file: export-templates/our_dta.docx",
    "      label: Our DTA template"
  ))
  got <- app_fn("list_available_templates")(lc$cfg)

  expect_true("clinical_dta_template.docx" %in% got)
  expect_identical(unname(got[["Our DTA template"]]), "library:our_dta.docx")
  expect_identical(names(got)[[length(got)]], "Our DTA template")
})

test_that("bundled_word_templates: false leaves only the library templates", {
  lc <- library_cfg(c(
    "exports:",
    "  bundled_word_templates: false",
    "  word_templates:",
    "    - file: export-templates/our_dta.docx"
  ))
  got <- app_fn("list_available_templates")(lc$cfg)

  expect_identical(unname(got), "library:our_dta.docx")
  expect_null(app_fn("get_template_path")("clinical_dta_template.docx", lc$cfg))
})

test_that("get_template_path resolves a library template inside the library and still refuses traversal", {
  lc <- library_cfg(c(
    "exports:",
    "  bundled_word_templates: false",
    "  word_templates:",
    "    - file: export-templates/our_dta.docx"
  ))
  get_path <- app_fn("get_template_path")

  path <- get_path("library:our_dta.docx", lc$cfg)
  expect_identical(
    normalizePath(path, winslash = "/"),
    normalizePath(file.path(lc$dir, "export-templates", "our_dta.docx"), winslash = "/")
  )
  # Not offered by the stock config, so not resolvable there.
  expect_null(get_path("library:our_dta.docx"))
  # The client may send anything over the websocket.
  expect_null(get_path("library:../../DESCRIPTION", lc$cfg))
  expect_null(get_path("library:other.docx", lc$cfg))
  expect_null(get_path("../our_dta.docx", lc$cfg))
  expect_null(get_path("our_dta.docx", lc$cfg))
  expect_null(get_path(file.path(lc$dir, "export-templates", "our_dta.docx"), lc$cfg))
})

test_that("export_document_options offers only what exports.document allows", {
  stock <- app_fn("export_document_options")()
  expect_true(stock$any)
  expect_identical(unname(stock$format_choices), c("markdown", "word"))
  expect_true(stock$pdf_choice)
  expect_identical(unname(stock$mode_choices), c("builtin", "custom"))

  pdf_only <- app_fn("export_document_options")(exports_cfg("document: [markdown_pdf]"))
  expect_identical(unname(pdf_only$format_choices), "markdown")
  expect_false(pdf_only$pdf_choice)
  expect_true(pdf_only$pdf_forced)
  expect_true(pdf_only$allowed("markdown", as_pdf = TRUE))
  expect_false(pdf_only$allowed("markdown", as_pdf = FALSE))
  expect_false(pdf_only$allowed("word"))

  md_only <- app_fn("export_document_options")(exports_cfg("document: [markdown]"))
  expect_false(md_only$pdf_choice)
  expect_false(md_only$pdf_forced)
  expect_true(md_only$allowed("markdown", as_pdf = FALSE))
  expect_false(md_only$allowed("markdown", as_pdf = TRUE))

  word_only <- app_fn("export_document_options")(exports_cfg("document: [word]"))
  expect_identical(unname(word_only$format_choices), "word")
  expect_identical(unname(word_only$mode_choices), "builtin")
  expect_true(word_only$allowed("word", word_mode = "builtin"))
  expect_false(word_only$allowed("word", word_mode = "custom"))

  none <- app_fn("export_document_options")(exports_cfg("document: []"))
  expect_false(none$any)
  expect_false(none$allowed("word"))
  expect_false(none$allowed("markdown"))
})

test_that("word_template counts as offered only when a template exists to pick", {
  lc <- library_cfg(c(
    "exports:",
    "  document: [word_template]",
    "  bundled_word_templates: false",
    "  word_templates:",
    "    - file: export-templates/our_dta.docx"
  ))
  o <- app_fn("export_document_options")(lc$cfg)
  expect_identical(unname(o$mode_choices), "custom")
  expect_identical(o$default_mode, "custom")
  expect_true(o$allowed("word", word_mode = "custom"))
  expect_false(o$allowed("word", word_mode = "builtin"))

  # Same selection with nothing to pick: not offered at all.
  cfg <- lc$cfg
  cfg$exports$word_templates <- cfg$exports$word_templates[0, ]
  expect_false(app_fn("export_document_options")(cfg)$any)
})

test_that("default_document picks the preselected format, mode and PDF state", {
  o <- app_fn("export_document_options")(exports_cfg("default_document: markdown_pdf"))
  expect_identical(o$default_format, "markdown")
  expect_true(o$default_pdf)

  o <- app_fn("export_document_options")(exports_cfg("default_document: markdown"))
  expect_identical(o$default_format, "markdown")
  expect_false(o$default_pdf)
})

test_that("two library Word templates with one file name: the first is offered and the duplicate is named in a warning", {
  lib <- withr::local_tempdir()
  dir.create(file.path(lib, "a"))
  dir.create(file.path(lib, "b"))
  expect_true(file.copy(app_fixture_template(), file.path(lib, "a", "same.docx")))
  expect_true(file.copy(app_fixture_template(), file.path(lib, "b", "same.docx")))
  writeLines(c(
    "exports:",
    "  bundled_word_templates: false",
    "  word_templates:",
    "    - {file: a/same.docx, label: First}",
    "    - {file: b/same.docx, label: Second}"
  ), file.path(lib, "dta-app.yaml"))
  cfg <- app_fn("parse_dta_app_config")(file.path(lib, "dta-app.yaml"))$config

  expect_warning(
    got <- app_fn("list_available_templates")(cfg),
    "same.docx",
    fixed = TRUE
  )
  expect_identical(got, c(First = "library:same.docx"))
  expect_match(
    # get_template_path() re-lists, so it repeats the duplicate warning asserted above.
    normalizePath(suppressWarnings(app_fn("get_template_path")("library:same.docx", cfg)), winslash = "/"),
    "/a/same.docx",
    fixed = TRUE
  )
})

# ---- dta_md_split_cells(): equal to the character-loop version, and linear ----
# ORACLE: the implementation the app shipped in 0.27.0, kept verbatim so the
# one-pass version can be compared with it. It grows a vector one character at a
# time, which is quadratic; its behaviour is the reference.
split_cells_oracle <- function(row) {
  s <- trimws(as.character(row)[1])
  chars <- strsplit(s, "")[[1]]
  cells <- character(0)
  cur <- character(0)
  ended_on_pipe <- FALSE
  i <- 1L
  n <- length(chars)
  while (i <= n) {
    ch <- chars[[i]]
    ended_on_pipe <- FALSE
    if (ch == "\\" && i < n) {
      cur <- c(cur, ch, chars[[i + 1L]])
      i <- i + 2L
      next
    }
    if (ch == "|") {
      cells <- c(cells, paste(cur, collapse = ""))
      cur <- character(0)
      ended_on_pipe <- TRUE
    } else {
      cur <- c(cur, ch)
    }
    i <- i + 1L
  }
  if (!ended_on_pipe) {
    cells <- c(cells, paste(cur, collapse = ""))
  }
  if (startsWith(s, "|") && length(cells) > 0) {
    cells <- cells[-1L]
  }
  trimws(cells)
}

test_that("dta_md_split_cells equals the character-loop version on random rows", {
  split <- app_fn("dta_md_split_cells")
  set.seed(20261112)
  alphabet <- c(
    "|", "|", "\\", "\\", "`", " ", " ", "a", "b", "<br>", "\u00e4", "\\|", "\\\\",
    "\u00e9", "\u65e5\u672c", "\U0001F600", "\r\n"
  )
  rows <- c(
    "", " ", "|", "||", "\\", "a\\", "|\\", "\\|", "\\\\|", "\\\\\\|", "| a |", "a|", "|a",
    vapply(seq_len(400), function(i) {
      paste(sample(alphabet, sample(1:30, 1), replace = TRUE), collapse = "")
    }, character(1))
  )
  for (r in rows) {
    expect_identical(split(r), split_cells_oracle(r), info = encodeString(r, quote = "'"))
  }
})

test_that("a row whose encoding is bytes still splits", {
  row <- "| a | b |"
  Encoding(row) <- "bytes"
  expect_identical(app_fn("dta_md_split_cells")(row), c("a", "b"))
})

test_that("dta_md_split_cells costs time in proportion to the row", {
  skip_on_cran()
  split <- app_fn("dta_md_split_cells")
  # One 10k-character cell, and a row of many short cells (the allowed-values
  # column), ASCII and not: character offsets in UTF-8 were quadratic on their own.
  one_cell <- function(n) paste0("| ", strrep("x", n), " |")
  many <- function(n) paste0("| ", paste(rep("v", n / 2), collapse = " | "), " |")
  many_utf8 <- function(n) paste0("| ", paste(rep("\u00e9", n / 2), collapse = " | "), " |")
  # Windows timers tick at ~10 ms, so one call is too short to measure: time 20
  # calls together, and take the median of three such measurements.
  med <- function(row) {
    stats::median(vapply(1:3, function(i) {
      unname(system.time(for (j in 1:20) split(row))[["elapsed"]])
    }, numeric(1)))
  }
  for (mk in list(one_cell, many, many_utf8)) {
    small <- max(med(mk(10000)), 0.05)
    big <- med(mk(40000))
    # Linear is 4x; quadratic is 16x. 8x leaves room for CI noise.
    expect_lt(big, 8 * small)
  }
  expect_length(split(many(40000)), 20000)
  expect_length(split(many_utf8(40000)), 20000)
})
