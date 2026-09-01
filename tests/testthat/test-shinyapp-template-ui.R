# Coverage for the "Create new from template" UI wiring in app.R: the
# template picker (output$template_picker_ui), the options-step additions
# (party slots, metadata carry-over) and the read-only provenance block on
# the Metadata tab (output$metadata_editor). The underlying engine --
# template_index.R, template_sources.R, template_create.R, party_profiles.R,
# template_core.R -- is exercised directly by its own test files; this file
# is only about the server wiring that joins it to the UI, driven through
# shiny::testServer() exactly like test-shinyapp-server.R.

Sys.setenv(NOT_CRAN = "true")

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

app_server_dir <- function() .shiny_app_dir()

# Local copy of the isolation helper defined in test-shinyapp-template-
# sources.R and test-shinyapp-template-create.R -- deliberately duplicated
# (per those files' own stated convention) so this file does not depend on
# another test file's internals. Every DTATOOLS_TEMPLATE_* variable is
# cleared for the duration of each test: a developer machine with
# DTATOOLS_TEMPLATE_SOURCES exported would flip the app into private-only
# mode and fail unrelated tests confusingly.
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
  withr::local_options(
    list(DTAtools.template_dir = NULL),
    .local_envir = .local_envir
  )
}

# The rendered HTML of a renderUI output, as one string -- copied from
# test-shinyapp-server.R's own ui_text(), same rationale: a renderUI output is
# never evaluated unless a test reads it, so anything asserted about a UI gate
# (or, here, about a UI CRASH) has to go through this.
ui_text <- function(out) {
  if (is.null(out) || is.null(out$html)) "" else paste(as.character(out$html), collapse = "")
}

# How many times `pattern` (a fixed substring) occurs in `x` -- used as a
# structural stand-in for "exactly N templates were listed" that does not
# depend on knowing any template's exact label text.
count_occurrences <- function(x, pattern) {
  m <- gregexpr(pattern, x, fixed = TRUE)[[1]]
  if (m[[1]] == -1L) 0L else length(m)
}

# A minimal, always-buildable creation template: a title/version base and one
# inline "file" dataset (the simplest form create_dta_from_template() can
# build with no further resolution). `filename` lets two versions of the same
# `id` be written under distinct on-disk names.
write_min_template <- function(root, id, version, label = id, filename = NULL,
                               extra_lines = character(0)) {
  fname <- filename
  if (is.null(fname) || !nzchar(fname)) {
    fname <- paste0(id, "_", gsub("[^A-Za-z0-9]", "", version))
  }
  writeLines(
    c(
      "kind: dta_creation_template",
      paste0("id: ", id),
      paste0("version: \"", version, "\""),
      paste0("label: ", label),
      "base:",
      "  metadata:",
      "    title: Test Title",
      "    version: \"1.0\"",
      "datasets:",
      "  - name: mini_ds",
      "    type: file",
      "    files: { filename: mini.csv, type: csv }",
      "options: []",
      extra_lines
    ),
    file.path(root, paste0(fname, ".dta-template.yaml"))
  )
}

write_party_profile <- function(root, id = "supplier_x", label = "Supplier X", role = "supplier") {
  writeLines(
    c(
      "kind: dta_party_profile",
      paste0("id: ", id),
      'version: "1.0"',
      paste0("role: ", role),
      paste0("label: ", label),
      "affiliation:",
      "  name: Party Affiliation Name",
      "  country: DE"
    ),
    file.path(root, paste0(id, ".dta-party.yaml"))
  )
}

write_template_with_party_slot <- function(root, id = "party_tpl") {
  writeLines(
    c(
      "kind: dta_creation_template",
      paste0("id: ", id),
      'version: "1.0"',
      "label: Party Template",
      "base:",
      "  metadata:",
      "    title: T",
      '    version: "1.0"',
      "party_slots:",
      "  - id: supplier_choice",
      "    target: metadata.supplier",
      "    label: Supplier",
      "datasets:",
      "  - name: mini_ds",
      "    type: file",
      "    files: { filename: mini.csv, type: csv }",
      "options: []"
    ),
    file.path(root, paste0(id, ".dta-template.yaml"))
  )
}

# Open the picker, force-read output$template_picker_ui (catches a render
# crash, per the task's own warning about testServer never rendering an
# output unless a test reads it), then move to the options step by directly
# setting the id/version and clicking Next.
pick_template_step1 <- function(session, output, id, version) {
  session$setInputs(create_from_template = 1)
  html <- ui_text(output$template_picker_ui)
  session$setInputs(template_select_name = id, template_select_version = version)
  session$setInputs(template_select_next = 1)
  invisible(html)
}

# Local copy of the autosave-slot cleanup defined in test-shinyapp-server.R /
# test-shinyapp-versioning.R / test-shinyapp-edit-mode.R -- deliberately
# duplicated, same rationale as local_clean_template_env() above: this is not
# part of helper-shinyapp.R, so a test file that needs it owns its own copy
# rather than depending on another test file's internals. Only the one
# reload test below (dta_client_id + restore_session) needs it.
clean_session_file <- function() {
  f <- list.files(tempdir(),
    pattern = "^dtatools_app_session.*\\.rds$", full.names = TRUE
  )
  unlink(f, force = TRUE)
  invisible(f)
}

# ---- Picker: grouping by source, excluding the packaged template ----------

test_that("opening the picker with a dir: source lists the private templates and not the packaged one", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("myprivate=dir:", root))
  write_min_template(root, id = "priv_tpl", version = "1.0", label = "Private Template")

  shiny::testServer(app_server_dir(), {
    session$setInputs(create_from_template = 1)
    html <- ui_text(output$template_picker_ui)

    expect_match(html, "Private Template", fixed = TRUE)
    expect_match(html, "myprivate", fixed = TRUE)
    # Structural stand-in for "the packaged demo is not offered": with a
    # private source configured, dta_template_include_builtin() defaults to
    # FALSE (template_sources.R), so the builtin root is never scanned at
    # all -- exactly one entry should be rendered, not knowledge of the
    # packaged template's own label.
    expect_equal(count_occurrences(html, "class=\"tmpl-entry"), 1)
  })
})

# ---- Picker: version selector -----------------------------------------------

test_that("the version selector lists both versions of a two-version template, newest first", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  write_min_template(root, id = "multi_tpl", version = "1.0", label = "Multi Template", filename = "multi_v1")
  write_min_template(root, id = "multi_tpl", version = "2.0", label = "Multi Template", filename = "multi_v2")

  shiny::testServer(app_server_dir(), {
    session$setInputs(create_from_template = 1)
    html <- ui_text(output$template_picker_ui)

    expect_match(html, "value=\"2.0\"", fixed = TRUE)
    expect_match(html, "value=\"1.0\"", fixed = TRUE)
    # Newest first: "2.0" must be rendered before "1.0" in the version
    # <select>.
    expect_true(regexpr("2.0", html, fixed = TRUE) < regexpr("1.0", html, fixed = TRUE))
  })
})

# ---- Creating from a template -----------------------------------------------

test_that("creating from a template sets rv$dta and stamps metadata.template", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  write_min_template(root, id = "stamp_tpl", version = "1.0", label = "Stamp Template")

  shiny::testServer(app_server_dir(), {
    pick_template_step1(session, output, "stamp_tpl", "1.0")
    session$setInputs(tmpl_carry_source = "none")
    session$setInputs(template_create_confirm = 1)

    expect_s3_class(rv$dta, "DTAtools::DTA")
    tpl <- DTAtools::metadata(rv$dta)@template
    expect_equal(tpl$id, "stamp_tpl")
    expect_equal(tpl$version, "1.0")

    # The read-only provenance block on the Metadata tab must show it too --
    # and reading this output catches a crash in template_provenance_block()
    # the same way ui_text(output$template_picker_ui) does above.
    meta_html <- ui_text(output$metadata_editor)
    expect_match(meta_html, "stamp_tpl@1.0", fixed = TRUE)
  })
})

test_that("a creation failure leaves the modal open instead of losing the user's choices", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  # A template whose lone dataset entry cannot be built (no such legacy
  # file) -- create_dta_from_template() must fail cleanly.
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: broken_tpl",
      'version: "1.0"',
      "label: Broken Template",
      "base:",
      "  metadata:",
      "    title: T",
      '    version: "1.0"',
      "datasets:",
      "  - does-not-exist.yaml",
      "options: []"
    ),
    file.path(root, "broken_tpl.dta-template.yaml")
  )

  shiny::testServer(app_server_dir(), {
    pick_template_step1(session, output, "broken_tpl", "1.0")
    session$setInputs(tmpl_carry_source = "none")
    session$setInputs(template_create_confirm = 1)

    expect_null(rv$dta)
    # The template selection is still in rv, ready for another confirm --
    # the confirm handler returned early rather than clearing it.
    expect_equal(rv$template_ref, "broken_tpl@1.0")
  })
})

# ---- Party slots -------------------------------------------------------

test_that("party slot selection reaches the created document's supplier block", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  write_template_with_party_slot(root)
  write_party_profile(root)

  shiny::testServer(app_server_dir(), {
    pick_template_step1(session, output, "party_tpl", "1.0")
    session$setInputs(tmpl_party_supplier_choice = "supplier_x")
    session$setInputs(tmpl_carry_source = "none")
    session$setInputs(template_create_confirm = 1)

    expect_s3_class(rv$dta, "DTAtools::DTA")
    supplier <- DTAtools::metadata(rv$dta)@supplier
    expect_equal(supplier$affiliation$name, "Party Affiliation Name")
  })
})

test_that("leaving a party slot on '(use template default)' does not touch its target", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  write_template_with_party_slot(root)
  write_party_profile(root)

  shiny::testServer(app_server_dir(), {
    pick_template_step1(session, output, "party_tpl", "1.0")
    # tmpl_party_supplier_choice deliberately left unset (its default choice
    # value is "", the "(use template default)" sentinel).
    session$setInputs(tmpl_party_supplier_choice = "")
    session$setInputs(tmpl_carry_source = "none")
    session$setInputs(template_create_confirm = 1)

    expect_s3_class(rv$dta, "DTAtools::DTA")
    expect_length(DTAtools::metadata(rv$dta)@supplier, 0)
  })
})

# ---- Metadata carry-over ------------------------------------------------

test_that("carry-over from the open document copies the chosen fields and not template", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  write_min_template(root, id = "carry_tpl", version = "1.0", label = "Carry Template")

  shiny::testServer(app_server_dir(), {
    ancestor <- app_fixture_dta()
    anc_meta <- DTAtools::metadata(ancestor)
    S7::prop(anc_meta, "supplier") <- list(affiliation = list(name = "Ancestor Supplier"))
    S7::prop(anc_meta, "receiver") <- list(affiliation = list(name = "Ancestor Receiver"))
    ancestor@metadata <- anc_meta
    rv$dta <- ancestor

    pick_template_step1(session, output, "carry_tpl", "1.0")
    # "From the open document" is the default once rv$dta is set (see
    # show_template_options_modal()), but is set explicitly here for clarity.
    session$setInputs(tmpl_carry_source = "open")
    session$setInputs(tmpl_carry_fields = "supplier")
    session$setInputs(template_create_confirm = 1)

    md <- DTAtools::metadata(rv$dta)
    expect_equal(md@supplier$affiliation$name, "Ancestor Supplier")
    # receiver was NOT in tmpl_carry_fields -- must not have been carried.
    expect_null(md@receiver$affiliation)
    # The new document's OWN provenance, never the ancestor's machine-owned
    # fields (apply_metadata_carry_over() strips template/import_issues
    # unconditionally -- see template_create.R).
    expect_equal(md@template$id, "carry_tpl")
    expect_length(md@import_issues, 0)
  })
})

test_that("carry-over 'From a file' reads the uploaded DTA's metadata", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  write_min_template(root, id = "filecarry2_tpl", version = "1.0", label = "File Carry Template 2")

  anc_path <- file.path(withr::local_tempdir(), "ancestor.yaml")
  writeLines(
    c(
      "metadata:",
      "  title: File Ancestor",
      '  version: "1.0"',
      "  supplier:",
      "    affiliation:",
      "      name: File Supplier",
      "datasets:",
      "  - name: anc_ds",
      "    type: file",
      "    files: { filename: a.csv, type: csv }"
    ),
    anc_path
  )
  anc_upload <- data.frame(
    name = "ancestor.yaml", size = file.size(anc_path), type = "",
    datapath = anc_path, stringsAsFactors = FALSE
  )

  shiny::testServer(app_server_dir(), {
    pick_template_step1(session, output, "filecarry2_tpl", "1.0")
    session$setInputs(tmpl_carry_source = "file")
    session$setInputs(tmpl_carry_file = anc_upload)
    session$setInputs(tmpl_carry_fields = "supplier")
    session$setInputs(template_create_confirm = 1)

    expect_s3_class(rv$dta, "DTAtools::DTA")
    expect_equal(DTAtools::metadata(rv$dta)@supplier$affiliation$name, "File Supplier")
  })
})

test_that("'From a file' with no file chosen is a clear error, and the modal stays open", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  write_min_template(root, id = "filecarry_tpl", version = "1.0", label = "File Carry Template")

  shiny::testServer(app_server_dir(), {
    pick_template_step1(session, output, "filecarry_tpl", "1.0")
    session$setInputs(tmpl_carry_source = "file")
    session$setInputs(template_create_confirm = 1)

    expect_null(rv$dta)
  })
})

# ---- Diagnostics for a broken source ----------------------------------------

test_that("a broken source with no cache shows the diagnostic and offers no templates", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  bad_dir <- file.path(withr::local_tempdir(), "does-not-exist")
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("badsource=dir:", bad_dir))

  shiny::testServer(app_server_dir(), {
    session$setInputs(create_from_template = 1)
    html <- ui_text(output$template_picker_ui)

    expect_match(html, "could not be loaded", fixed = TRUE)
    expect_match(html, "badsource", fixed = TRUE)
    expect_match(html, "No templates are available", fixed = TRUE)
    # No fallback to the packaged demo: no template entry rendered at all.
    expect_equal(count_occurrences(html, "class=\"tmpl-entry"), 0)
  })
})

# ---- Refresh -----------------------------------------------------------

test_that("'Refresh templates' picks up a template added to the source directory after the first listing", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  write_min_template(root, id = "early_tpl", version = "1.0", label = "Early Template")

  shiny::testServer(app_server_dir(), {
    session$setInputs(create_from_template = 1)
    html1 <- ui_text(output$template_picker_ui)
    expect_match(html1, "Early Template", fixed = TRUE)
    expect_equal(count_occurrences(html1, "class=\"tmpl-entry"), 1)

    write_min_template(root, id = "late_tpl", version = "1.0", label = "Late Template")
    session$setInputs(tmpl_refresh_templates = 1)

    html2 <- ui_text(output$template_picker_ui)
    expect_match(html2, "Late Template", fixed = TRUE)
    expect_equal(count_occurrences(html2, "class=\"tmpl-entry"), 2)
  })
})

# ---- No private source configured: today's packaged-template behaviour -----

test_that("with no private source configured, the picker opens and offers the packaged template(s)", {
  # Deliberately does NOT assert on any specific template's id/label/
  # description -- inst/extdata/templates/*.dta-template.yaml is owned and
  # concurrently edited elsewhere; this only pins the STRUCTURAL guarantee
  # that dta_template_include_builtin() defaults to TRUE with nothing private
  # configured (template_sources.R), so the packaged directory is scanned
  # exactly as list_dta_creation_templates() always scanned it.
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()

  shiny::testServer(app_server_dir(), {
    session$setInputs(create_from_template = 1)
    html <- ui_text(output$template_picker_ui)

    expect_gte(count_occurrences(html, "class=\"tmpl-entry"), 1)
    expect_match(html, "id=\"template_select_name\"", fixed = TRUE)
    expect_match(html, "id=\"template_select_version\"", fixed = TRUE)
  })
})

# ---- Creating from a template enters edit mode ------------------------------

test_that("creating a document from a template leaves the author editing it", {
  # THE BUG THIS GUARDS: template_create_confirm() built the document via
  # apply_loaded(), which always leaves rv$editing FALSE (correct for a real
  # load) -- but a template-created document is new, not loaded, and there is
  # no switch left for the author to flip themselves. See the WHY comment on
  # the template_create_confirm observer in app.R.
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  write_min_template(root, id = "edit_tpl", version = "1.0", label = "Edit Template")

  shiny::testServer(app_server_dir(), {
    pick_template_step1(session, output, "edit_tpl", "1.0")
    session$setInputs(tmpl_carry_source = "none")
    session$setInputs(template_create_confirm = 1)

    expect_true(editing())

    # Behavioural check, not just the flag: an edit really lands. save_md()
    # debounces 700ms -- see the identical guard in test-shinyapp-edit-mode.R
    # for why elapse(1000) is needed to observe it.
    session$setInputs(md_header = "Acme Corp Ltd")
    session$elapse(1000)
    expect_equal(
      as.character(S7::prop(DTAtools::metadata(rv$dta), "header")), "Acme Corp Ltd"
    )
  })
})

test_that("edit mode from a template-created document survives a reload", {
  # The direct regression guard: template_create_confirm() used to autosave
  # BEFORE setting rv$editing <- TRUE, so the snapshot on disk still said
  # editing = FALSE and a reload right after creating the document dropped
  # the author into a read-only view of what they had just made.
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  write_min_template(root, id = "reload_tpl", version = "1.0", label = "Reload Template")

  clean_session_file()
  shiny::testServer(app_server_dir(), {
    session$setInputs(dta_client_id = strrep("f", 32))
    pick_template_step1(session, output, "reload_tpl", "1.0")
    session$setInputs(tmpl_carry_source = "none")
    session$setInputs(template_create_confirm = 1)
    expect_true(editing())

    session$setInputs(restore_session = 1)

    expect_true(editing())
  })
})
