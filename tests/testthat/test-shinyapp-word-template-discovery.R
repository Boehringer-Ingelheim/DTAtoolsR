# Word templates found by convention in <library root>/export-templates/*.docx
# (dta_app_config_discover_word_templates() in app_config.R), how they reach the
# export dialog (list_available_templates(), get_template_path(),
# export_document_options()), and the library-layout warnings the template
# picker shows (template_source_diagnostics_ui()).
#
# The .docx files here are generated into temp dirs; no binary fixture is
# committed. Discovery keeps only files that hold a {PLACEHOLDER} marker, so
# the files make_library() writes are real templates built once per call.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

app_server_dir <- function() .shiny_app_dir()

ui_text <- function(out) {
  if (is.null(out) || is.null(out$html)) "" else paste(as.character(out$html), collapse = "")
}

# A fresh library ROOT (normalised), optionally with an export-templates/
# folder holding one placeholder-bearing .docx per name in `docx`.
make_library <- function(docx = character(0), .local_envir = parent.frame()) {
  root <- normalizePath(withr::local_tempdir(.local_envir = .local_envir), winslash = "/")
  if (length(docx) > 0) {
    dir.create(file.path(root, "export-templates"))
    tmpl <- .make_template(c("Agreement {DTA_TITLE}", "{DTA_VERSION}"))
    on.exit(unlink(tmpl), add = TRUE)
    for (f in docx) file.copy(tmpl, file.path(root, "export-templates", f))
  }
  root
}

# Point the app at `roots` (precedence order) as dir: sources, with a clean
# config cache.
use_libraries <- function(roots, .local_envir = parent.frame()) {
  withr::local_envvar(
    c(
      DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", roots, collapse = ";"),
      DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = NA,
      DTATOOLS_TEMPLATE_REFRESH_SECONDS = NA,
      DTATOOLS_TEMPLATE_CACHE_DIR = NA
    ),
    .local_envir = .local_envir
  )
  withr::local_options(list(DTAtools.template_dir = NULL), .local_envir = .local_envir)
  invalidate <- app_fn("dta_app_config_invalidate")
  app_fn("dta_template_index_invalidate")()
  invalidate()
  withr::defer(
    {
      invalidate()
      app_fn("dta_template_index_invalidate")()
    },
    envir = .local_envir
  )
  invisible(roots)
}

discover <- function(roots, listed = character(0)) {
  app_fn("dta_app_config_discover_word_templates")(roots, listed)
}

norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)

# Warning messages raised while evaluating `expr`, whitespace-collapsed (cli
# wraps long messages); `value` is the expression's result.
collect_warnings <- function(expr) {
  msgs <- character(0)
  value <- withCallingHandlers(expr, warning = function(w) {
    msgs <<- c(msgs, gsub("\\s+", " ", conditionMessage(w)))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = msgs)
}

# ---- Discovery ------------------------------------------------------------------

test_that("discovery lists export-templates/*.docx with the file stem as label, in radix order", {
  root <- make_library(c("Zeta.docx", "alpha.docx", "Beta.docx"))
  got <- discover(root)

  expect_identical(names(got), c("label", "path"))
  # Radix (C-locale) order, not the machine's collation: upper case sorts first.
  expect_identical(got$label, c("Beta", "Zeta", "alpha"))
  expect_identical(got$path, norm(file.path(root, "export-templates", paste0(got$label, ".docx"))))
  expect_true(all(file.exists(got$path)))
})

test_that("discovery returns an empty (label, path) frame without an export-templates folder or roots", {
  empty <- make_library()
  for (roots in list(empty, character(0), NA_character_, "", file.path(empty, "no-such-dir"))) {
    got <- discover(roots)
    expect_s3_class(got, "data.frame")
    expect_identical(names(got), c("label", "path"))
    expect_identical(nrow(got), 0L)
  }
})

test_that("discovery ignores non-docx files, hidden and Word lock files, sub-folders and directories", {
  root <- make_library(c("keep.docx", ".hidden.docx", "~$lock.docx", "notes.txt"))
  et <- file.path(root, "export-templates")
  dir.create(file.path(et, "sub"))
  writeBin(as.raw(1:3), file.path(et, "sub", "inner.docx"))
  dir.create(file.path(et, "folder.docx"))
  # A stray docx beside the folder, not in it, is not read either.
  writeBin(as.raw(1:3), file.path(root, "loose.docx"))

  got <- discover(root)

  expect_identical(got$label, "keep")
  expect_identical(got$path, norm(file.path(et, "keep.docx")))
})

test_that("a symlink to a .docx outside the library is rejected", {
  root <- make_library("real.docx")
  outside <- withr::local_tempfile(fileext = ".docx")
  writeBin(as.raw(1:3), outside)
  link <- file.path(root, "export-templates", "escape.docx")
  made <- suppressWarnings(tryCatch(file.symlink(outside, link), error = function(e) FALSE))
  skip_if_not(
    isTRUE(made) && file.exists(link),
    "this platform/account cannot create symbolic links (Windows needs Developer Mode or elevation)"
  )

  got <- discover(root)

  expect_identical(got$label, "real")
  expect_false(any(grepl("escape", got$path, fixed = TRUE)))
})

test_that("a symlink to a .docx inside the library is allowed", {
  root <- make_library("real.docx")
  link <- file.path(root, "export-templates", "alias.docx")
  made <- suppressWarnings(tryCatch(
    file.symlink(file.path(root, "export-templates", "real.docx"), link),
    error = function(e) FALSE
  ))
  skip_if_not(
    isTRUE(made) && file.exists(link),
    "this platform/account cannot create symbolic links (Windows needs Developer Mode or elevation)"
  )

  got <- discover(root)

  expect_setequal(got$label, c("real", "real"))
  expect_true(all(startsWith(got$path, paste0(norm(root), "/"))))
})

test_that("earlier root wins on a basename collision, with a warning for the shadowed file", {
  a <- make_library(c("report.docx", "only_a.docx"))
  b <- make_library(c("Report.docx", "only_b.docx"))

  res <- collect_warnings(discover(c(a, b)))
  got <- res$value

  # Matched case-insensitively: B's Report.docx is shadowed by A's report.docx.
  expect_setequal(got$label, c("report", "only_a", "only_b"))
  expect_identical(got$path[got$label == "report"], norm(file.path(a, "export-templates", "report.docx")))
  expect_length(res$warnings, 1L)
  expect_match(res$warnings, "already provided by", fixed = TRUE)
  expect_match(res$warnings, "Report.docx", fixed = TRUE)

  # Reversing the precedence reverses the winner.
  res2 <- collect_warnings(discover(c(b, a)))
  expect_identical(
    basename(res2$value$path[res2$value$label == "Report"]),
    "Report.docx"
  )
  expect_length(res2$warnings, 1L)
})

test_that("the same file reached twice (same root listed twice) is skipped silently", {
  a <- make_library("report.docx")

  expect_no_warning(got <- discover(c(a, a)))
  expect_identical(got$label, "report")
})

test_that("files already listed in the config are not rediscovered, and a different-path namesake warns", {
  a <- make_library(c("listed.docx", "extra.docx"))
  listed <- norm(file.path(a, "export-templates", "listed.docx"))

  expect_no_warning(got <- discover(a, listed))
  expect_identical(got$label, "extra")

  # Same basename listed from somewhere else: the listed one holds the name.
  elsewhere <- withr::local_tempfile(pattern = "x", fileext = ".docx")
  other <- file.path(dirname(elsewhere), "listed.docx")
  writeBin(as.raw(1:3), other)
  withr::defer(unlink(other))
  res <- collect_warnings(discover(a, norm(other)))
  expect_identical(res$value$label, "extra")
  expect_length(res$warnings, 1L)
  expect_match(res$warnings, "already provided by", fixed = TRUE)
})

test_that("a discovered .docx without {PLACEHOLDER} markers is skipped with a warning naming it", {
  root <- make_library("good.docx")
  et <- file.path(root, "export-templates")
  # A styles-only reference document: valid Word file, nothing to fill.
  expect_true(file.copy(.make_template("Just prose, no markers."), file.path(et, "styles_only.docx")))

  res <- collect_warnings(discover(root))

  expect_identical(res$value$label, "good")
  expect_length(res$warnings, 1L)
  expect_match(res$warnings, "styles_only.docx", fixed = TRUE)
  expect_match(res$warnings, "no {PLACEHOLDER} markers", fixed = TRUE)
})

test_that("a corrupt discovered .docx is skipped with a warning, not an error", {
  root <- make_library("good.docx")
  writeBin(as.raw(1:3), file.path(root, "export-templates", "broken.docx"))

  res <- collect_warnings(discover(root))

  expect_identical(res$value$label, "good")
  expect_length(res$warnings, 1L)
  expect_match(res$warnings, "broken.docx", fixed = TRUE)
})

test_that("a .docx without placeholders that dta-app.yaml lists explicitly is still offered", {
  root <- make_library()
  et <- file.path(root, "export-templates")
  dir.create(et)
  expect_true(file.copy(.make_template("Just prose, no markers."), file.path(et, "listed.docx")))
  writeLines(
    c("exports:", "  word_templates:", "    - file: export-templates/listed.docx", "      label: Listed"),
    file.path(root, "dta-app.yaml")
  )
  use_libraries(root)

  # Silently: the listed file is the author's explicit choice, and is not
  # rediscovered and then rejected.
  expect_no_warning(cfg <- app_fn("dta_app_config")())

  expect_identical(cfg$exports$word_templates$label, "Listed")
})

# ---- Through the app config ------------------------------------------------------

test_that("dta_app_config() offers discovered Word templates with no dta-app.yaml at all", {
  root <- make_library(c("our_dta.docx", "other.docx"))
  use_libraries(root)

  cfg <- app_fn("dta_app_config")()

  expect_true(is.na(cfg$source))
  expect_identical(cfg$exports$word_templates$label, c("other", "our_dta"))
  expect_identical(
    cfg$exports$word_templates$path,
    norm(file.path(root, "export-templates", c("other.docx", "our_dta.docx")))
  )
  # The rest of the config is still the stock one.
  expect_identical(cfg$branding$app_title, "DTAtools")
  expect_true(cfg$exports$bundled_word_templates)
})

test_that("a yaml-listed template keeps its label and comes first; the rest follow, no duplicate", {
  root <- make_library(c("a_first.docx", "listed.docx", "z_last.docx"))
  writeLines(
    c(
      "exports:",
      "  word_templates:",
      "    - file: export-templates/listed.docx",
      "      label: Our House Style"
    ),
    file.path(root, "dta-app.yaml")
  )
  use_libraries(root)

  cfg <- app_fn("dta_app_config")()

  wt <- cfg$exports$word_templates
  expect_identical(wt$label, c("Our House Style", "a_first", "z_last"))
  expect_identical(
    basename(wt$path),
    c("listed.docx", "a_first.docx", "z_last.docx")
  )
  expect_identical(anyDuplicated(wt$path), 0L)
  expect_identical(rownames(wt), as.character(seq_len(nrow(wt))))
})

test_that("with two libraries the earlier one wins a name and the shadowed file warns once", {
  a <- make_library("report.docx")
  b <- make_library(c("Report.docx", "b_only.docx"))
  use_libraries(c(a, b))

  res <- collect_warnings(app_fn("dta_app_config")())

  wt <- res$value$exports$word_templates
  expect_setequal(wt$label, c("report", "b_only"))
  expect_identical(wt$path[wt$label == "report"], norm(file.path(a, "export-templates", "report.docx")))
  expect_identical(sum(grepl("already provided by", res$warnings, fixed = TRUE)), 1L)
})

# ---- The export dialog ------------------------------------------------------------

test_that("list_available_templates offers discovered files as library:<basename> after the bundled ones", {
  root <- make_library(c("our_dta.docx", "other.docx"))
  use_libraries(root)
  cfg <- app_fn("dta_app_config")()

  got <- app_fn("list_available_templates")(cfg)

  expect_true("clinical_dta_template.docx" %in% got)
  expect_identical(
    tail(unname(got), 2),
    c("library:other.docx", "library:our_dta.docx")
  )
  expect_identical(tail(names(got), 2), c("other", "our_dta"))
})

test_that("get_template_path resolves discovered templates and rejects names that were not offered", {
  root <- make_library(c("our_dta.docx"))
  use_libraries(root)
  cfg <- app_fn("dta_app_config")()
  get_path <- app_fn("get_template_path")

  expect_identical(
    norm(get_path("library:our_dta.docx", cfg)),
    norm(file.path(root, "export-templates", "our_dta.docx"))
  )
  # Not offered: never discovered, wrong namespace, traversal, absolute path.
  expect_null(get_path("library:not_there.docx", cfg))
  expect_null(get_path("our_dta.docx", cfg))
  expect_null(get_path("library:../dta-app.yaml", cfg))
  expect_null(get_path("library:export-templates/our_dta.docx", cfg))
  expect_null(get_path(file.path(root, "export-templates", "our_dta.docx"), cfg))
  # The stock config knows nothing of this library.
  expect_null(get_path("library:our_dta.docx"))
})

test_that("a file dropped into export-templates/ after startup appears once the config is refreshed", {
  root <- make_library("one.docx")
  use_libraries(root)
  expect_identical(app_fn("dta_app_config")()$exports$word_templates$label, "one")

  file.copy(.make_template("{DTA_TITLE}"), file.path(root, "export-templates", "two.docx"))

  # Cached until invalidated -- the same "Refresh templates" path as dta-app.yaml.
  expect_identical(app_fn("dta_app_config")()$exports$word_templates$label, "one")
  app_fn("dta_template_index_invalidate")()
  expect_identical(app_fn("dta_app_config")()$exports$word_templates$label, c("one", "two"))
})

test_that("exports.bundled_word_templates: false hides the bundled templates but keeps discovered ones", {
  root <- make_library("our_dta.docx")
  writeLines(
    c("exports:", "  bundled_word_templates: false", "  document: [word_template]"),
    file.path(root, "dta-app.yaml")
  )
  use_libraries(root)
  cfg <- app_fn("dta_app_config")()

  got <- app_fn("list_available_templates")(cfg)
  expect_identical(got, c(our_dta = "library:our_dta.docx"))
  expect_null(app_fn("get_template_path")("clinical_dta_template.docx", cfg))
  expect_false(is.null(app_fn("get_template_path")("library:our_dta.docx", cfg)))

  # ...and it is what makes the custom-template mode available at all.
  opts <- app_fn("export_document_options")(cfg)
  expect_identical(unname(opts$mode_choices), "custom")
  expect_identical(opts$templates, got)
  expect_true(opts$allowed("word", word_mode = "custom"))
})

test_that("with bundled templates off and no export-templates folder, custom-template mode is not offered", {
  root <- make_library()
  writeLines(
    c("exports:", "  bundled_word_templates: false", "  document: [word_template]"),
    file.path(root, "dta-app.yaml")
  )
  use_libraries(root)
  cfg <- app_fn("dta_app_config")()

  expect_length(app_fn("list_available_templates")(cfg), 0L)
  expect_false(app_fn("export_document_options")(cfg)$any)
})

test_that("a discovered template really exports: the dialog's choice resolves to the file that is filled", {
  root <- make_library()
  tmpl <- .make_template(c("Agreement {DTA_TITLE}", "{DTA_VERSION}"))
  dir.create(file.path(root, "export-templates"))
  expect_true(file.copy(tmpl, file.path(root, "export-templates", "house_style.docx")))
  use_libraries(root)
  cfg <- app_fn("dta_app_config")()

  path <- app_fn("get_template_path")("library:house_style.docx", cfg)

  expect_identical(norm(path), norm(file.path(root, "export-templates", "house_style.docx")))
  expect_true(app_fn("template_has_placeholders")(path))
})

test_that("the app's export dialog gets the discovered templates (server wiring)", {
  root <- make_library("our_dta.docx")
  writeLines(
    c("exports:", "  bundled_word_templates: false", "  document: [word_template]"),
    file.path(root, "dta-app.yaml")
  )
  use_libraries(root)

  shiny::testServer(app_server_dir(), {
    expect_true(export_choices$any)
    expect_identical(export_choices$templates, c(our_dta = "library:our_dta.docx"))
    expect_identical(unname(export_choices$mode_choices), "custom")
  })
})

# ---- Library-layout warnings in the picker -----------------------------------------

# A library root whose template files are in the wrong place, as the index
# builder reports them.
misplaced_library <- function(n = 2L, .local_envir = parent.frame()) {
  root <- make_library(.local_envir = .local_envir)
  for (i in seq_len(n)) {
    writeLines(
      c("kind: dta_creation_template", sprintf("id: loose_%d", i), 'version: "1.0"'),
      file.path(root, sprintf("loose_%02d.dta-template.yaml", i))
    )
  }
  root
}

index_warnings <- function(root, .local_envir = parent.frame()) {
  use_libraries(root, .local_envir = .local_envir)
  idx <- suppressWarnings(app_fn("build_template_index")())
  attr(idx, "layout_warnings")
}

test_that("template_source_diagnostics_ui renders the layout warnings from a real index build", {
  root <- misplaced_library(2L)
  warns <- index_warnings(root)
  expect_length(warns, 2L)

  html <- as.character(app_fn("template_source_diagnostics_ui")(list(), warns))

  expect_match(html, "loose_01.dta-template.yaml", fixed = TRUE)
  expect_match(html, "loose_02.dta-template.yaml", fixed = TRUE)
  expect_match(html, "belongs in", fixed = TRUE)
  expect_match(html, "a creation template belongs in", fixed = TRUE)
  expect_no_match(html, "dta_creation_template", fixed = TRUE)
  expect_no_match(html, basename(dirname(root)), fixed = TRUE)
  expect_match(html, "templates/", fixed = TRUE)
  expect_no_match(html, "more</li>", fixed = TRUE)
  # Failed sources and layout problems are two separate blocks.
  expect_no_match(html, "could not be loaded", fixed = TRUE)
})

test_that("more than ten layout warnings are cut to ten plus an 'and N more' line", {
  root <- misplaced_library(13L)
  warns <- index_warnings(root)
  expect_length(warns, 13L)

  html <- as.character(app_fn("template_source_diagnostics_ui")(list(), warns))

  expect_identical(lengths(regmatches(html, gregexpr("belongs in", html, fixed = TRUE))), 10L)
  expect_match(html, "… and 3 more", fixed = TRUE)
  # Exactly ten shown: the first ten by file name, not the last three.
  expect_match(html, "loose_10.dta-template.yaml", fixed = TRUE)
  expect_no_match(html, "loose_11.dta-template.yaml", fixed = TRUE)
})

test_that("exactly ten layout warnings show no 'and N more' line", {
  warns <- sprintf("lib: f%02d.dta-template.yaml - a creation template belongs in templates/ (library layout: x)", 1:10)

  html <- as.character(app_fn("template_source_diagnostics_ui")(list(), warns))

  expect_identical(lengths(regmatches(html, gregexpr("belongs in", html, fixed = TRUE))), 10L)
  expect_no_match(html, "more", fixed = TRUE)
})

test_that("template_source_diagnostics_ui shows failed sources and layout problems together", {
  warns <- "lib: a.dta-template.yaml - a creation template belongs in templates/ (library layout: x)"
  sources <- list(
    list(name = "good", scheme = "dir", ok = TRUE),
    list(name = "broken", scheme = "git", ok = FALSE, error = "clone failed")
  )

  html <- as.character(app_fn("template_source_diagnostics_ui")(sources, warns))

  expect_match(html, "broken (git): clone failed", fixed = TRUE)
  expect_no_match(html, "good (dir)", fixed = TRUE)
  expect_match(html, "a.dta-template.yaml", fixed = TRUE)
  expect_match(html, "belongs in", fixed = TRUE)
})

test_that("template_source_diagnostics_ui is NULL when there is nothing to show", {
  fn <- app_fn("template_source_diagnostics_ui")
  ok <- list(list(name = "good", scheme = "dir", ok = TRUE))

  expect_null(fn(list()))
  expect_null(fn(NULL))
  expect_null(fn(ok))
  expect_null(fn(ok, character(0)))
  expect_null(fn(ok, NULL))
})

test_that("template_layout_warnings reads the index's layout_warnings attribute, not its message text", {
  fn <- app_fn("template_layout_warnings")
  layout <- c(
    "lib: a.dta-party.yaml - a party profile belongs in parties/ (library layout: templates/, parties/)",
    "lib: vocabulary/ looks like a library sub-folder; point the source at its parent directory."
  )
  idx <- data.frame(id = "x")
  attr(idx, "warnings") <- c("Skipped x.yaml: invalid YAML", layout)
  attr(idx, "layout_warnings") <- layout

  # The index: only the dedicated attribute counts (an unrelated warning in
  # "warnings" is not shown, whatever it says).
  expect_identical(fn(idx), layout)
  # A message that merely LOOKS like a layout warning is not one unless it is in the attribute.
  attr(idx, "layout_warnings") <- NULL
  attr(idx, "warnings") <- layout
  expect_identical(fn(idx), character(0))
  # The attribute's own value, and nothing at all.
  expect_identical(fn(layout), layout)
  expect_identical(fn(NULL), character(0))
})

# ---- The picker itself: the two empty-state messages ------------------------------

test_that("a source that loads but holds no templates points at the folder layout, not at a failed source", {
  root <- misplaced_library(2L)
  use_libraries(root)

  shiny::testServer(app_server_dir(), {
    suppressWarnings(session$setInputs(create_from_template = 1)) # index build warns about the loose files
    html <- suppressWarnings(ui_text(output$template_picker_ui))

    # The layout diagnostics name the file and the folder it belongs in...
    expect_match(html, "loose_01.dta-template.yaml", fixed = TRUE)
    expect_match(html, "belongs in", fixed = TRUE)
    # ...and the empty message is the layout one.
    expect_match(html, "No creation templates were found", fixed = TRUE)
    expect_match(html, "folder layout", fixed = TRUE)
    expect_no_match(html, "failed to load", fixed = TRUE)
    expect_no_match(html, "could not be loaded", fixed = TRUE)
  })
})

test_that("when every configured source fails, the picker says so instead of blaming the layout", {
  missing <- file.path(withr::local_tempdir(), "no-such-library")
  use_libraries(missing)

  shiny::testServer(app_server_dir(), {
    session$setInputs(create_from_template = 1)
    html <- suppressWarnings(ui_text(output$template_picker_ui))

    expect_match(html, "could not be loaded", fixed = TRUE)
    expect_match(html, "failed to load", fixed = TRUE)
    expect_no_match(html, "folder layout", fixed = TRUE)
  })
})

test_that("a correct library lists its template and shows no layout warning", {
  root <- make_library()
  dir.create(file.path(root, "templates"))
  writeLines(
    c(
      "kind: dta_creation_template", "id: good_tpl", 'version: "1.0"', "label: Good Template",
      "base:", "  metadata:", "    title: T", '    version: "1.0"',
      "datasets:", "  - id: mini_ds", "    type: file", "    files: { filename: mini.csv, type: csv }",
      "options: []"
    ),
    file.path(root, "templates", "good_tpl.dta-template.yaml")
  )
  use_libraries(root)

  shiny::testServer(app_server_dir(), {
    session$setInputs(create_from_template = 1)
    frame <- suppressWarnings(ui_text(output$template_picker_ui))
    list_html <- ui_text(output$template_picker_list)

    expect_match(list_html, "Good Template", fixed = TRUE)
    expect_no_match(frame, "belongs in", fixed = TRUE)
    expect_no_match(frame, "No creation templates were found", fixed = TRUE)
  })
})
