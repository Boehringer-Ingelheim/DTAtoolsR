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

# Open the picker and force-read all THREE of its outputs (a renderUI is
# never evaluated unless a test reads it, so a crash in any of them would
# otherwise pass silently), then move to the options step by setting the
# id/version directly and pressing "Use this template".
pick_template_step1 <- function(session, output, id, version) {
  session$setInputs(create_from_template = 1)
  html <- ui_text(output$template_picker_ui)
  ui_text(output$template_picker_list)
  ui_text(output$template_picker_detail)
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
    list_html <- ui_text(output$template_picker_list)

    # The source's own name is part of the picker FRAME (its status row);
    # the template rows live in their own output, so that typing in the
    # search box re-renders only them.
    expect_match(html, "myprivate", fixed = TRUE)
    expect_match(list_html, "Private Template", fixed = TRUE)
    # Structural stand-in for "the packaged demo is not offered": with a
    # private source configured, dta_template_include_builtin() defaults to
    # FALSE (template_sources.R), so the builtin root is never scanned at
    # all -- exactly one entry should be rendered, not knowledge of the
    # packaged template's own label.
    expect_equal(count_occurrences(list_html, "class=\"tmpl-entry"), 1)
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
    ui_text(output$template_picker_ui)
    session$setInputs(template_select_name = "multi_tpl")
    # The version <select> belongs to the detail panel: it describes the
    # SELECTED template, so it re-renders with the selection and not with
    # the frame around it.
    html <- ui_text(output$template_picker_detail)

    expect_match(html, "value=\"2.0\"", fixed = TRUE)
    expect_match(html, "value=\"1.0\"", fixed = TRUE)
    # Newest first: "2.0" must be rendered before "1.0" in the version
    # <select>.
    expect_true(regexpr("2.0", html, fixed = TRUE) < regexpr("1.0", html, fixed = TRUE))
  })
})

# ---- Picker: search and source filter ---------------------------------------

test_that("a version chosen on one template does not carry onto the next", {
  # THE BUG THIS GUARDS: Shiny keeps the previous template's version input
  # until the re-rendered control reports, so moving from a template whose
  # only version is 1.0 to one whose newest is 2.0 used to open its 1.0.
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  write_min_template(root, id = "old_tpl", version = "1.0", label = "Old Template")
  write_min_template(root, id = "new_tpl", version = "1.0", label = "New Template", filename = "new_v1")
  write_min_template(root, id = "new_tpl", version = "2.0", label = "New Template", filename = "new_v2")

  shiny::testServer(app_server_dir(), {
    session$setInputs(create_from_template = 1)
    ui_text(output$template_picker_ui)
    session$setInputs(template_select_name = "old_tpl", template_select_version = "1.0")
    expect_match(ui_text(output$template_picker_detail), "value=\"1.0\"[^>]*selected")

    # The version input still says "1.0" -- nobody has touched it -- but it
    # was chosen for old_tpl, so new_tpl opens at its newest.
    session$setInputs(template_select_name = "new_tpl")
    expect_match(ui_text(output$template_picker_detail), "value=\"2.0\"[^>]*selected")
    expect_equal(template_picker_version(), "2.0")

    # Choosing 1.0 FOR new_tpl is honoured.
    session$setInputs(template_select_version = "1.0")
    expect_equal(template_picker_version(), "1.0")
  })
})

test_that("typing in the search box narrows the list and highlights the match", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  # Labels deliberately avoid the word "Template", so that exactly one row can
  # carry the token below: the search matches at the START of a word, so the
  # "late" inside "template" would not hit anyway -- and that is worth keeping
  # true here rather than relying on it.
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("lib=dir:", root))
  write_min_template(root, id = "early_tpl", version = "1.0", label = "Early Draft")
  write_min_template(root, id = "late_tpl", version = "1.0", label = "Late Draft")

  shiny::testServer(app_server_dir(), {
    session$setInputs(create_from_template = 1)
    ui_text(output$template_picker_ui)
    expect_equal(count_occurrences(ui_text(output$template_picker_list), "class=\"tmpl-entry"), 2)

    session$setInputs(template_search = "late")
    narrowed <- ui_text(output$template_picker_list)

    expect_equal(count_occurrences(narrowed, "class=\"tmpl-entry"), 1)
    expect_false(grepl("Early Draft", narrowed, fixed = TRUE))
    # The matched token is marked in the label, so a hit on the id or the
    # description is not left looking like an unexplained result.
    expect_match(narrowed, "<mark>Late</mark>", fixed = TRUE)

    # Clearing the box restores the full list rather than leaving the last
    # filter in place.
    session$setInputs(template_search = "")
    expect_equal(count_occurrences(ui_text(output$template_picker_list), "class=\"tmpl-entry"), 2)
  })
})

test_that("the source filter appears only with several sources, and restricts the list", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root_a <- withr::local_tempdir()
  root_b <- withr::local_tempdir()
  write_min_template(root_a, id = "one_tpl", version = "1.0", label = "One Template")
  write_min_template(root_b, id = "two_tpl", version = "1.0", label = "Two Template")
  withr::local_envvar(
    DTATOOLS_TEMPLATE_SOURCES = paste0("one=dir:", root_a, ";two=dir:", root_b)
  )

  shiny::testServer(app_server_dir(), {
    session$setInputs(create_from_template = 1)
    expect_match(ui_text(output$template_picker_ui), "id=\"template_select_source\"", fixed = TRUE)
    expect_equal(count_occurrences(ui_text(output$template_picker_list), "class=\"tmpl-entry"), 2)

    session$setInputs(template_select_source = "two")
    filtered <- ui_text(output$template_picker_list)

    expect_equal(count_occurrences(filtered, "class=\"tmpl-entry"), 1)
    expect_match(filtered, "Two Template", fixed = TRUE)
    expect_false(grepl("One Template", filtered, fixed = TRUE))
  })
})

test_that("a source filter that no longer names a source is ignored, not applied", {
  # THE BUG THIS GUARDS: the filter control exists only while two or more
  # sources have templates, but Shiny keeps its last value after the control
  # is gone -- so a refresh that emptied the chosen source used to leave
  # every row filtered out behind "pick another source", with nothing left to
  # pick from.
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root_a <- withr::local_tempdir()
  root_b <- withr::local_tempdir()
  write_min_template(root_a, id = "keep_tpl", version = "1.0", label = "Keep Template")
  write_min_template(root_b, id = "gone_tpl", version = "1.0", label = "Gone Template")
  withr::local_envvar(
    DTATOOLS_TEMPLATE_SOURCES = paste0("one=dir:", root_a, ";two=dir:", root_b)
  )

  shiny::testServer(app_server_dir(), {
    session$setInputs(create_from_template = 1)
    ui_text(output$template_picker_ui)
    session$setInputs(template_select_source = "two")
    expect_match(ui_text(output$template_picker_list), "Gone Template", fixed = TRUE)

    unlink(file.path(root_b, "gone_tpl_10.dta-template.yaml"))
    session$setInputs(tmpl_refresh_templates = 1)

    html <- ui_text(output$template_picker_list)
    expect_equal(count_occurrences(html, "class=\"tmpl-entry"), 1)
    expect_match(html, "Keep Template", fixed = TRUE)
    expect_false(grepl("template_select_source", ui_text(output$template_picker_ui), fixed = TRUE))
  })
})

test_that("with a single source the picker offers no source filter to choose from", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("solo=dir:", root))
  write_min_template(root, id = "solo_tpl", version = "1.0", label = "Solo Template")

  shiny::testServer(app_server_dir(), {
    session$setInputs(create_from_template = 1)

    expect_false(grepl("template_select_source", ui_text(output$template_picker_ui), fixed = TRUE))
  })
})

# ---- Picker: families -------------------------------------------------------

# A base and one template that `extends:` it. write_min_template() appends
# extra_lines at the end of the document; YAML mappings are unordered, so an
# `extends:` written there is the same key it would be next to `label:`.
write_family <- function(root) {
  write_min_template(root, id = "base_tpl", version = "1.0", label = "Base Template")
  write_min_template(root,
    id = "child_tpl", version = "1.0", label = "Child Template",
    extra_lines = "extends: base_tpl"
  )
}

test_that("a deviation template is listed under its base, named as extending it", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  write_family(root)

  shiny::testServer(app_server_dir(), {
    session$setInputs(create_from_template = 1)
    ui_text(output$template_picker_ui)
    html <- ui_text(output$template_picker_list)

    expect_equal(count_occurrences(html, "class=\"tmpl-entry"), 2)
    # Exactly one row is a "top" row: the base. The child hangs off it and is
    # revealed by expanding the family, which is what makes a library of 40
    # standards with vendor variants each readable at all.
    expect_equal(count_occurrences(html, "data-top"), 1)
    expect_match(html, "extends Base Template", fixed = TRUE)
    # The base advertises what expanding it will reveal. Asserted as a
    # prefix so the count's own pluralisation is not pinned here.
    expect_match(html, "1 variant", fixed = TRUE)
  })
})

test_that("the detail panel names the base a selected deviation template resolves against", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  write_family(root)

  shiny::testServer(app_server_dir(), {
    session$setInputs(create_from_template = 1)
    ui_text(output$template_picker_ui)
    session$setInputs(template_select_name = "child_tpl")

    # The lineage comes from load_template_definition(), i.e. the resolved
    # chain the created document would actually be built from -- not from the
    # `extends:` string as written.
    expect_match(ui_text(output$template_picker_detail), "based on Base Template", fixed = TRUE)
  })
})

# ---- Picker: "Back to templates" and an empty result ------------------------

test_that("'Back to templates' reopens the picker with its search and selection intact", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  write_min_template(root, id = "back_tpl", version = "1.0", label = "Back Template")
  write_min_template(root, id = "other_tpl", version = "1.0", label = "Other Template")

  shiny::testServer(app_server_dir(), {
    session$setInputs(create_from_template = 1)
    ui_text(output$template_picker_ui)
    session$setInputs(
      template_search = "back", template_select_name = "back_tpl",
      template_select_version = "1.0"
    )
    session$setInputs(template_select_next = 1)
    expect_equal(rv$template_ref, "back_tpl@1.0")

    # The browser reports the picker modal closed once "Use this template" has
    # removed it. While it is closed the picker's outputs render NOTHING --
    # that is what leaves the client no stale copy to re-insert, and no stale
    # search box or checked radio to report as input, on the way back.
    session$setInputs(template_picker_closed = 1)
    expect_equal(ui_text(output$template_picker_ui), "")
    expect_equal(ui_text(output$template_picker_list), "")
    expect_equal(ui_text(output$template_picker_detail), "")

    session$setInputs(template_options_back = 1)

    # Going back does not discard the choice already made -- the author can
    # press Cancel and still be where they were -- and the search typed
    # before "Use this template" is still filtering the list.
    expect_equal(rv$template_ref, "back_tpl@1.0")
    frame <- ui_text(output$template_picker_ui)
    expect_match(frame, "value=\"back\"", fixed = TRUE)
    html <- ui_text(output$template_picker_list)
    expect_equal(count_occurrences(html, "class=\"tmpl-entry"), 1)
    # The search is still applied, so its match is still marked in the label.
    expect_match(html, "<mark>Back</mark>", fixed = TRUE)
    expect_match(html, "data-id=\"back_tpl\"", fixed = TRUE)
    expect_match(html, "value=\"back_tpl\"[^>]*checked")
  })
})

test_that("'Use this template' warns rather than proceeding when the search hides everything", {
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  write_min_template(root, id = "hidden_tpl", version = "1.0", label = "Hidden Template")

  shiny::testServer(app_server_dir(), {
    session$setInputs(create_from_template = 1)
    ui_text(output$template_picker_ui)
    session$setInputs(template_search = "zzz-nomatch")
    expect_equal(count_occurrences(ui_text(output$template_picker_list), "class=\"tmpl-entry"), 0)

    session$setInputs(template_select_next = 1)

    # Nothing was selected, so nothing was carried into step 2 -- rather than
    # the first entry of an unfiltered list the user cannot see.
    expect_null(rv$template_ref)
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
    # With nothing to list, the frame does not even place the list output, so
    # the count is taken over both -- "0 here and 0 there", either way.
    expect_equal(
      count_occurrences(paste0(html, ui_text(output$template_picker_list)), "class=\"tmpl-entry"),
      0
    )
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
    ui_text(output$template_picker_ui)
    html1 <- ui_text(output$template_picker_list)
    expect_match(html1, "Early Template", fixed = TRUE)
    expect_equal(count_occurrences(html1, "class=\"tmpl-entry"), 1)

    write_min_template(root, id = "late_tpl", version = "1.0", label = "Late Template")
    session$setInputs(tmpl_refresh_templates = 1)

    html2 <- ui_text(output$template_picker_list)
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
    ui_text(output$template_picker_ui)
    list_html <- ui_text(output$template_picker_list)

    expect_gte(count_occurrences(list_html, "class=\"tmpl-entry"), 1)
    # The radio group is the list output's own container -- Shiny's radio
    # binding finds the inputs by the group id, so this pins the contract
    # that makes clicking a row set input$template_select_name at all.
    expect_match(list_html, "id=\"template_select_name\"", fixed = TRUE)
    expect_match(
      ui_text(output$template_picker_detail), "id=\"template_select_version\"",
      fixed = TRUE
    )
  })
})

# ---- Creating from a template enters edit mode ------------------------------

test_that("creating a document from a template leaves the author editing it", {
  # THE BUG THIS GUARDS: apply_loaded() writes rv$editing unconditionally, to
  # isTRUE(start_editing) -- FALSE unless the caller says otherwise, which is
  # correct for a real load -- so template_create_confirm() has to pass
  # start_editing = TRUE itself. A template-created document is new, not
  # loaded, and there is no switch left for the author to flip themselves.
  # See the WHY comment on the template_create_confirm observer in app.R.
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

# ---- Vocabulary slots: the live "n of m selected" counter -------------------

# A vocabulary plus a template with one slot bound to it. The slot's `target`
# only has to PARSE here (vocabulary_slot_target_parts(), vocabulary.R) -- this
# test stops at the options step and never builds a document.
write_vocab_slot_fixture <- function(root) {
  writeLines(
    c(
      "kind: dta_vocabulary",
      "id: visit",
      'version: "1.0"',
      "label: Visit identifiers",
      "type: text",
      "terms:",
      "  - code: TERM_A",
      "    label: First term",
      "  - code: TERM_B",
      "    label: Second term"
    ),
    file.path(root, "visit.dta-vocabulary.yaml")
  )
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: vocab_tpl",
      'version: "1.0"',
      "label: Vocabulary Template",
      "base:",
      "  metadata:",
      "    title: T",
      '    version: "1.0"',
      "vocabulary_slots:",
      "  - id: visit_choice",
      "    label: Visits",
      "    target: datasets.mini_ds.columns.VISIT.values",
      "    vocabulary: visit@1.0",
      "    default: [TERM_A, TERM_B]",
      "datasets:",
      "  - name: mini_ds",
      "    type: file",
      "    files: { filename: mini.csv, type: csv }",
      "options: []"
    ),
    file.path(root, "vocab_tpl.dta-template.yaml")
  )
}

test_that("the vocabulary counter follows the control, and an emptied control means no terms", {
  # The counter is a server-side renderText registered per slot on every open
  # of the options modal, which is the only reason a test can see it at all --
  # the All/None/Default links that drive it are client-side.
  local_clean_template_env()
  app_fn("dta_template_index_invalidate")()
  root <- withr::local_tempdir()
  withr::local_envvar(DTATOOLS_TEMPLATE_SOURCES = paste0("dir:", root))
  write_vocab_slot_fixture(root)

  shiny::testServer(app_server_dir(), {
    pick_template_step1(session, output, "vocab_tpl", "1.0")

    # In a browser the control binds already holding the slot's `default:`
    # and reports it at once; testServer has no browser, so the report is
    # made by hand.
    session$setInputs(tmpl_vocab_visit_choice = c("TERM_A", "TERM_B"))
    expect_equal(output$tmpl_vocab_visit_choice_count, "2 of 2 selected")

    session$setInputs(tmpl_vocab_visit_choice = "TERM_A")
    expect_equal(output$tmpl_vocab_visit_choice_count, "1 of 2 selected")
    expect_identical(collect_vocab_selections(), list(visit_choice = "TERM_A"))

    # A term the vocabulary does not offer (typed into an `open` slot) is
    # counted apart from the offered ones rather than silently dropped.
    session$setInputs(tmpl_vocab_visit_choice = c("TERM_A", "OWN"))
    expect_equal(output$tmpl_vocab_visit_choice_count, "1 of 2 selected, 1 custom")

    # THE POINT OF "None": an emptied control is "0 of 2", and is collected
    # as an explicit empty selection -- which vocabulary_slot_values() reads
    # as "no terms" -- not omitted, which would have put the default back
    # behind the author's back.
    session$setInputs(tmpl_vocab_visit_choice = character(0))
    expect_equal(output$tmpl_vocab_visit_choice_count, "0 of 2 selected")
    expect_identical(collect_vocab_selections(), list(visit_choice = character(0)))
  })
})
