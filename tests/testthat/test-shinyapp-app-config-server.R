# The app side of the library app config (`dta-app.yaml`): the brand bar and
# footer builders, and the server-side feature gates. Every gate is asserted
# against the SERVER (testServer) and not only the markup, because a control
# that is not rendered can still be driven over the websocket.
#
# testServer() sources the app directory afresh, so the app gets its own config
# cache; pointing DTATOOLS_TEMPLATE_SOURCES at a temp library before the call is
# enough, no invalidation needed.

# Run `code` with a library holding `yaml_lines` as its dta-app.yaml.
with_app_library <- function(yaml_lines, code, .local_envir = parent.frame()) {
  lib <- normalizePath(withr::local_tempdir(.local_envir = .local_envir), winslash = "/")
  writeLines(yaml_lines, file.path(lib, "dta-app.yaml"))
  withr::local_envvar(
    c(
      DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", lib),
      DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = NA,
      DTATOOLS_TEMPLATE_REFRESH_SECONDS = NA,
      DTATOOLS_TEMPLATE_CACHE_DIR = NA
    ),
    .local_envir = .local_envir
  )
  withr::local_options(list(DTAtools.template_dir = NULL), .local_envir = .local_envir)
  clean_session_file()
  invisible(lib)
}

# ---- Brand bar and footer -----------------------------------------------------

test_that("brandbar_ui shows the configured title, subtitle, logo and links", {
  cfg <- app_fn("dta_app_config_defaults")()
  cfg$branding$app_title <- "Acme DTA"
  cfg$branding$app_subtitle <- "Our own subtitle"
  cfg$branding$logo <- "data:image/png;base64,AAAA"
  cfg$branding$logo_alt <- "Acme logo"
  cfg$links$documentation <- "https://example.org/docs"

  html <- as.character(app_fn("brandbar_ui")(cfg))

  expect_match(html, ">Acme DTA<", fixed = TRUE)
  expect_match(html, ">Our own subtitle<", fixed = TRUE)
  expect_match(html, 'src="data:image/png;base64,AAAA"', fixed = TRUE)
  expect_match(html, 'alt="Acme logo"', fixed = TRUE)
  expect_match(html, 'href="https://example.org/docs"', fixed = TRUE)
  expect_match(html, ">Documentation<", fixed = TRUE)
  expect_match(html, ">Report issues<", fixed = TRUE)
  expect_match(html, ">About<", fixed = TRUE)
  expect_match(html, 'id="edit_gate"', fixed = TRUE)
})

test_that("brandbar_ui with the stock config is today's bar, with no Documentation link", {
  html <- as.character(app_fn("brandbar_ui")())

  expect_match(html, 'src="dtatools_logo_small.png"', fixed = TRUE)
  expect_match(html, ">DTAtools<", fixed = TRUE)
  expect_match(html, "https://github.com/Boehringer-Ingelheim/DTAtoolsR/issues", fixed = TRUE)
  expect_no_match(html, "Documentation", fixed = TRUE)
})

test_that("brandbar_ui drops a link set to false and escapes text from the config", {
  cfg <- app_fn("dta_app_config_defaults")()
  cfg$links$report_issue <- NULL
  cfg$links$about <- NULL
  cfg$branding$app_title <- "<script>alert(1)</script>"

  html <- as.character(app_fn("brandbar_ui")(cfg))

  expect_no_match(html, "Report issues", fixed = TRUE)
  expect_no_match(html, ">About<", fixed = TRUE)
  expect_no_match(html, "<script>", fixed = TRUE)
  expect_match(html, "&lt;script&gt;alert(1)&lt;/script&gt;", fixed = TRUE)
})

# ---- Header tag ---------------------------------------------------------------

# The brand bar for a config whose header_tag is set to `label` / `color`.
brandbar_with_tag <- function(label = "", color = NULL) {
  cfg <- app_fn("dta_app_config_defaults")()
  cfg$header_tag$label <- label
  cfg$header_tag$color <- color
  as.character(app_fn("brandbar_ui")(cfg))
}

test_that("brandbar_ui shows no tag without a label, whatever the colour", {
  expect_no_match(as.character(app_fn("brandbar_ui")()), "brand-tag", fixed = TRUE)
  expect_no_match(brandbar_with_tag("", "#C0392B"), "brand-tag", fixed = TRUE)
})

test_that("brandbar_ui puts the header tag after the title, as escaped text", {
  html <- brandbar_with_tag("GxP")
  expect_match(html, '<span class="brand-tag">GxP</span>', fixed = TRUE)
  expect_match(
    html,
    '<div class="brand-title">\\s*DTAtools\\s*<span class="brand-tag">GxP</span>'
  )

  hostile <- brandbar_with_tag("<b>GxP</b>")
  expect_match(hostile, "&lt;b&gt;GxP&lt;/b&gt;", fixed = TRUE)
  expect_no_match(hostile, "<b>GxP", fixed = TRUE)
})

test_that("the header tag colour fills the label and gets a text colour chosen for contrast", {
  # Text colours are the WCAG-derived ones: white where it reaches 4.5:1
  # (#C0392B is about 5.4:1), the stock ink where it does not (#FFD700).
  red <- brandbar_with_tag("GxP", "#C0392B")
  expect_match(red, 'style="--tag-bg: #C0392B; --tag-fg: #FFFFFF; --tag-border: rgba(255,255,255,.3);"', fixed = TRUE)

  gold <- brandbar_with_tag("GxP", "#FFD700")
  expect_match(gold, "--tag-bg: #FFD700;", fixed = TRUE)
  expect_match(gold, "--tag-fg: #1A2B2A;", fixed = TRUE)
  # 0x1A, 0x2B, 0x2A: the outline is the dark text colour, not a white ring.
  expect_match(gold, "--tag-border: rgba(26,43,42,.3);", fixed = TRUE)
})

test_that("a hand-set non-hex header tag colour is re-checked at render time and never reaches the page", {
  html <- brandbar_with_tag("GxP", "red;}body{x:1")
  expect_match(html, '<span class="brand-tag">GxP</span>', fixed = TRUE)
  expect_no_match(html, "body{", fixed = TRUE)
  expect_no_match(html, "--tag-bg", fixed = TRUE)
})

test_that("a header_tag parsed from dta-app.yaml reaches the rendered brand bar", {
  f <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c("header_tag:", "  label: Non-GxP", "  color: '#5B6B6A'"), f)
  res <- app_fn("parse_dta_app_config")(f)
  expect_identical(nrow(res$problems), 0L)

  html <- as.character(app_fn("brandbar_ui")(res$config))
  expect_match(html, ">Non-GxP</span>", fixed = TRUE)
  expect_match(html, "--tag-bg: #5B6B6A;", fixed = TRUE)
})

test_that("app_footer_ui keeps the name and version, and shows the label and repository link only when set", {
  footer <- app_fn("app_footer_ui")

  stock <- as.character(footer(app_fn("dta_app_config_defaults")(), "v1.2.3"))
  expect_match(stock, ">DTAtools<", fixed = TRUE)
  expect_match(stock, ">v1.2.3<", fixed = TRUE)
  expect_match(stock, "GitHub repository", fixed = TRUE)
  # The organisation default is empty, so the old hard-coded name is gone.
  expect_no_match(stock, "Boehringer Ingelheim</span>", fixed = TRUE)

  cfg <- app_fn("dta_app_config_defaults")()
  cfg$branding$footer_label <- "Example Institute <b>"
  cfg$links$repository <- NULL
  custom <- as.character(footer(cfg, ""))
  expect_match(custom, "Example Institute &lt;b&gt;", fixed = TRUE)
  expect_no_match(custom, "GitHub repository", fixed = TRUE)
  expect_no_match(custom, "foot-ver", fixed = TRUE)
})

# ---- Server gates -------------------------------------------------------------

test_that("editing: false keeps the app read-only whatever the client sends", {
  with_app_library("features: {editing: false}")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    expect_false(editing())

    enter_edit_mode(session)
    expect_false(editing())
    expect_false(isTRUE(rv$editing))

    unlock_editing(session)
    expect_false(editing())
    expect_false(isTRUE(rv$editing))

    # The brandbar slot renders nothing at all.
    expect_identical(ui_text(output$edit_gate), "")
  })
})

test_that("with editing allowed (the default) the same inputs do unlock editing", {
  with_app_library("# all defaults")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    expect_false(editing())
    enter_edit_mode(session)
    expect_true(editing())
    expect_match(ui_text(output$edit_gate), "edit", ignore.case = TRUE)
  })
})

test_that("raw_yaml_tab: false removes the tab and ignores apply_yaml", {
  with_app_library("features: {raw_yaml_tab: false}")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    expect_no_match(ui_text(output$main), "Raw YAML", fixed = TRUE)

    enter_edit_mode(session)
    expect_true(editing())
    loaded <- rv$dta
    yaml_before <- rv$yaml_text
    session$setInputs(raw_yaml_editor = "datasets: [unclosed", apply_yaml = 1)

    expect_identical(rv$dta, loaded)
    expect_identical(rv$yaml_text, yaml_before)
    expect_null(rv$yaml_msg)
  })
})

test_that("the Raw YAML tab is present with the default config", {
  with_app_library("# all defaults")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    expect_match(ui_text(output$main), "Raw YAML", fixed = TRUE)
  })
})

test_that("start buttons follow features.start, and a disabled one is refused server-side", {
  with_app_library(c(
    "features:",
    "  start: {load_example: false, create_from_template: false, create_blank: false}"
  ))

  shiny::testServer(app_server_dir(), {
    landing <- ui_text(output$main)
    expect_match(landing, "dta_file", fixed = TRUE)
    expect_no_match(landing, "load_example", fixed = TRUE)
    expect_no_match(landing, "create_from_template", fixed = TRUE)
    expect_no_match(landing, 'id="create_new"', fixed = TRUE)
    # With nothing to offer and no session to restore, the row is not rendered.
    expect_no_match(landing, "Other ways to start", fixed = TRUE)

    session$setInputs(load_example = 1)
    expect_null(rv$dta)
    session$setInputs(create_new = 1)
    expect_null(rv$dta)
    expect_null(rv$create_new_msg)
  })
})

test_that("the landing page shows the configured headline and lede, escaped", {
  with_app_library(c(
    "branding:",
    "  landing_headline: \"Welcome <i>here</i>\"",
    "  landing_lede: Pick a file."
  ))

  shiny::testServer(app_server_dir(), {
    landing <- ui_text(output$main)
    expect_match(landing, "Welcome &lt;i&gt;here&lt;/i&gt;", fixed = TRUE)
    expect_match(landing, "Pick a file.", fixed = TRUE)
    # Default start buttons are all still there.
    expect_match(landing, "load_example", fixed = TRUE)
  })
})

test_that("an export format outside exports.document is refused by export_do", {
  with_app_library("exports: {document: [word]}")

  shiny::testServer(app_server_dir(), {
    rv$dta <- app_fixture_dta()
    session$setInputs(export_format = "markdown", export_as_pdf = FALSE)
    session$setInputs(export_do = 1)
    expect_null(export_state$file_path)

    # Word with the custom-template mode is not allowed either.
    session$setInputs(export_format = "word", export_word_mode = "custom")
    session$setInputs(export_do = 2)
    expect_null(export_state$file_path)

    # The allowed combination goes through.
    session$setInputs(export_format = "word", export_word_mode = "builtin")
    session$setInputs(export_do = 3)
    expect_true(file.exists(export_state$file_path))
    expect_match(export_state$file_name, "\\.docx$")
  })
})

test_that("with only markdown_pdf allowed, a plain Markdown request is refused and the PDF flag is forced", {
  with_app_library("exports: {document: [markdown_pdf]}")

  shiny::testServer(app_server_dir(), {
    rv$dta <- app_fixture_dta()
    expect_true(export_choices$pdf_forced)
    expect_identical(export_selection()$as_pdf, TRUE)
    # A client sending as_pdf = FALSE cannot get a Markdown file.
    session$setInputs(export_format = "markdown", export_as_pdf = FALSE)
    expect_true(export_selection()$as_pdf)
  })
})

test_that("an empty exports.document leaves nothing to export", {
  with_app_library("exports: {document: []}")

  shiny::testServer(app_server_dir(), {
    rv$dta <- app_fixture_dta()
    expect_false(export_choices$any)
    session$setInputs(export_format = "word", export_word_mode = "builtin")
    session$setInputs(export_do = 1)
    expect_null(export_state$file_path)
  })
})

test_that("the message-dock downloads follow exports.validation_messages", {
  with_app_library("exports: {validation_messages: [csv]}")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    html <- ui_text(output$msgs_dock_dl)
    expect_match(html, "dl_msgs_csv", fixed = TRUE)
    expect_no_match(html, "dl_msgs_tsv", fixed = TRUE)
    expect_no_match(html, "dl_msgs_xlsx", fixed = TRUE)
    expect_no_match(html, "dl_msgs_html", fixed = TRUE)
  })
})

test_that("validation_summary: false hides the sidebar summary even for a passing validation", {
  with_app_library("exports: {validation_summary: false}")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    rv$status <- c(clinical_data = "pass")
    session$flushReact()
    expect_identical(ui_text(output$validation_report_ui), "")
  })
})

test_that("with the default config the validation summary appears for a passing validation", {
  with_app_library("# all defaults")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    rv$status <- c(clinical_data = "pass")
    session$flushReact()
    expect_match(ui_text(output$validation_report_ui), "Validation summary", fixed = TRUE)
  })
})

test_that("the sidebar download buttons follow the yaml download flags", {
  with_app_library("features: {dta_yaml_download: false, dataset_yaml_download: false}")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    main <- ui_text(output$main)
    expect_no_match(main, "dl_yaml", fixed = TRUE)
    expect_match(main, "export_modal_open", fixed = TRUE)
  })
})

# ---- Start-page confirm inputs are gated server-side too -----------------------

test_that("a disabled start flag also refuses a forged confirm input", {
  with_app_library(c(
    "features:",
    "  start: {load_example: false, create_from_template: false, create_blank: false}"
  ))

  shiny::testServer(app_server_dir(), {
    example <- app_fn("dta_example_yaml_files")()[1]
    session$setInputs(example_dta_choice = example, example_dta_load = 1)
    expect_null(rv$dta)
    expect_null(rv$structure)

    session$setInputs(create_new_title = "Forged", create_new_version_value = "1.0")
    session$setInputs(create_new_confirm = 1)
    expect_null(rv$dta)
    expect_null(rv$structure)

    # Both template confirm steps, with a template ref planted as if a picker
    # had been opened before the gate was applied.
    rv$template_ref <- "any@1.0"
    session$setInputs(template_select_next = 1)
    session$setInputs(template_create_confirm = 1)
    expect_null(rv$dta)
    expect_null(rv$structure)
    expect_identical(rv$template_ref, "any@1.0")
  })
})

test_that("with the start flags on (the default) the example and blank confirms do load", {
  with_app_library("# all defaults")

  shiny::testServer(app_server_dir(), {
    example <- app_fn("dta_example_yaml_files")()[1]
    session$setInputs(example_dta_choice = example, example_dta_load = 1)
    expect_s3_class(rv$dta, "DTAtools::DTA")
  })

  shiny::testServer(app_server_dir(), {
    session$setInputs(create_new_title = "Real", create_new_version_value = "1.0")
    session$setInputs(create_new_confirm = 1)
    expect_s3_class(rv$dta, "DTAtools::DTA")
  })
})

# ---- document.name / document.abbreviation reach the app's wording -------------

# Run create-new and an export-modal open under the current library and return
# what the user would read: the create modal, the "created" notification, and
# the sidebar once a document is open.
# Replace the first `title:` line of a YAML text.
yaml_retitle <- function(x, title) {
  lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
  i <- grep("^ *title:", lines)[1]
  lines[i] <- sub("title:.*", paste0("title: ", title), lines[i])
  paste(lines, collapse = "\n")
}

document_wording <- function(.env = parent.frame()) {
  out <- new.env()
  out$modals <- character()
  out$notices <- character()
  testthat::local_mocked_bindings(
    showModal = function(ui, ...) {
      out$modals <- c(out$modals, paste(as.character(ui), collapse = ""))
      invisible(NULL)
    },
    showNotification = function(ui, ...) {
      out$notices <- c(out$notices, as.character(ui))
      invisible(NULL)
    },
    .package = "shiny",
    .env = .env
  )
  out
}

test_that("a configured abbreviation replaces DTA in the buttons and the created notification", {
  with_app_library(c(
    "document:",
    "  name: Data Transmission Specification",
    "  abbreviation: DTS"
  ))

  shiny::testServer(app_server_dir(), {
    landing <- ui_text(output$main)
    expect_no_match(landing, "Create DTA", fixed = TRUE)
    # Narrowed once the library names its documents: the default wording
    # "DTA / DTS" is deliberately broader because the uploader takes both.
    expect_match(landing, "Load a DTS file", fixed = TRUE)

    seen <- document_wording()
    session$setInputs(create_new = 1)
    expect_match(seen$modals[[1]], "Create a new DTS", fixed = TRUE)
    expect_match(seen$modals[[1]], "Create DTS", fixed = TRUE)

    session$setInputs(create_new_title = "Real", create_new_version_value = "1.0")
    session$setInputs(create_new_confirm = 1)
    expect_s3_class(rv$dta, "DTAtools::DTA")
    expect_true(any(grepl("New DTS", seen$notices, fixed = TRUE)))
    expect_false(any(grepl("New DTA", seen$notices, fixed = TRUE)))

    workspace <- ui_text(output$main)
    expect_match(workspace, "Export DTS", fixed = TRUE)
    expect_no_match(workspace, "Export DTA", fixed = TRUE)
  })
})

test_that("with both kinds configured the buttons carry both abbreviations", {
  with_app_library(c(
    "document:",
    "  name: [Data Transfer Agreement, Data Transmission Specification]",
    "  abbreviation: [DTA, DTS]"
  ))

  shiny::testServer(app_server_dir(), {
    seen <- document_wording()
    session$setInputs(create_new = 1)
    expect_match(seen$modals[[1]], "Create DTA/DTS", fixed = TRUE)
  })
})

test_that("a hostile abbreviation is escaped in the rendered page", {
  with_app_library(c(
    "document:",
    "  abbreviation: \"<b>X</b>\""
  ))

  shiny::testServer(app_server_dir(), {
    seen <- document_wording()
    session$setInputs(create_new = 1)
    expect_match(seen$modals[[1]], "Create &lt;b&gt;X&lt;/b&gt;", fixed = TRUE)
    expect_no_match(seen$modals[[1]], "<b>X</b>", fixed = TRUE)
  })
})

test_that("without a document section the wording is unchanged", {
  with_app_library("# all defaults")

  shiny::testServer(app_server_dir(), {
    landing <- ui_text(output$main)
    expect_match(landing, "Load a DTA / DTS specification file", fixed = TRUE)
    seen <- document_wording()
    session$setInputs(create_new = 1)
    expect_match(seen$modals[[1]], "Create DTA", fixed = TRUE)
    session$setInputs(create_new_title = "Real", create_new_version_value = "1.0")
    session$setInputs(create_new_confirm = 1)
    expect_true(any(grepl("New DTA", seen$notices, fixed = TRUE)))
  })
})

# ---- features.raw_yaml_editable --------------------------------------------------

test_that("raw_yaml_editable: false keeps the tab read-only and refuses a forged apply_yaml", {
  with_app_library("features: {raw_yaml_editable: false}")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    expect_match(ui_text(output$main), "Raw YAML", fixed = TRUE)

    enter_edit_mode(session)
    expect_true(editing())
    # The rest of the app is still editable, only the raw tab is not.
    expect_false(yaml_editable())
    expect_no_match(ui_text(output$yaml_edit_actions), "apply_yaml", fixed = TRUE)
    expect_no_match(ui_text(output$yaml_edit_hint), "Apply changes", fixed = TRUE)

    loaded <- rv$dta
    yaml_before <- rv$yaml_text
    edited <- yaml_retitle(yaml_before, "Forged title")
    expect_false(identical(edited, yaml_before))
    session$setInputs(raw_yaml_editor = edited, apply_yaml = 1)

    expect_identical(rv$dta, loaded)
    expect_identical(rv$yaml_text, yaml_before)
    expect_null(rv$yaml_msg)
  })
})

test_that("with the default config the apply button is rendered and apply_yaml applies", {
  with_app_library("# all defaults")

  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_file = app_file_input("clinical_dta.yaml"))
    enter_edit_mode(session)
    expect_true(yaml_editable())
    expect_match(ui_text(output$yaml_edit_actions), "apply_yaml", fixed = TRUE)

    edited <- yaml_retitle(rv$yaml_text, "Edited title")
    session$setInputs(raw_yaml_editor = edited, apply_yaml = 1)
    expect_true(rv$yaml_msg$ok, info = rv$yaml_msg$error)
    expect_identical(DTAtools::metadata(rv$dta)@title, "Edited title")
  })
})
