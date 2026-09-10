# Coverage for inst/shiny/dta_app/R/template_ui.R -- the pure UI builders
# behind "Create new from template". Everything here is asserted on rendered
# HTML, with no server: that is the whole point of moving these out of app.R,
# and test-shinyapp-template-ui.R covers the server wiring that calls them.

Sys.setenv(NOT_CRAN = "true")

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("shinyjs")

# Local copy of the isolation helper defined in test-shinyapp-template-
# sources.R, -create.R and -ui.R -- deliberately duplicated (per those files'
# own stated convention) so this file does not depend on another test file's
# internals. A developer machine with DTATOOLS_TEMPLATE_SOURCES exported would
# otherwise flip the index into private-only mode and fail these confusingly.
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

# The rendered HTML of any tag, tagList or NULL, as one string.
html_of <- function(x) paste(as.character(x), collapse = "")

# How many times `pattern` (a fixed substring) occurs in `x` -- same helper,
# same rationale, as test-shinyapp-template-ui.R: a structural stand-in for
# "exactly N rows were listed" that does not depend on any label's exact text.
count_occurrences <- function(x, pattern) {
  m <- gregexpr(pattern, x, fixed = TRUE)[[1]]
  if (m[[1]] == -1L) 0L else length(m)
}

# `%||%` is the app's own; these tests need it before app_env() is touched.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# One row per template, in the shape template_picker_lineage() produces
# (template_index.R): the picker's own columns plus the family/tree ones.
make_entries <- function(id, label, family, depth, indent,
                         version = NULL, description = NULL,
                         source_name = NULL, lineage = NULL) {
  n <- length(id)
  data.frame(
    id = id,
    version = version %||% rep("1.0", n),
    label = label,
    description = description %||% rep("", n),
    source_name = source_name %||% rep("lab", n),
    family = family,
    family_label = family,
    depth = as.integer(depth),
    indent = as.integer(indent),
    lineage = lineage %||% rep("", n),
    search_text = tolower(paste(label, id)),
    sort_key = label,
    stringsAsFactors = FALSE
  )
}

# Two families in tree order: base (with two children) and two lone roots.
tree_entries <- function() {
  make_entries(
    id = c("base", "child_a", "child_b", "other", "solo"),
    label = c("Base template", "Child A", "Child B", "Other template", "Solo template"),
    family = c("base", "base", "base", "other", "solo"),
    depth = c(0, 1, 1, 0, 0),
    indent = c(0, 1, 1, 0, 0),
    description = c("Root description", "", "", "Other description", ""),
    source_name = c("lab", "lab", "lab", "packaged", "packaged"),
    lineage = c("", "Base template", "Base template", "", "")
  )
}

# ---- Fixtures for the index-backed builders --------------------------------

write_vocabulary_file <- function(root) {
  writeLines(
    c(
      "kind: dta_vocabulary",
      "id: visit",
      'version: "1.0"',
      "label: Visit identifiers",
      "description: Standard visit codes",
      "type: text",
      "terms:",
      "  - code: SCR",
      "    label: Screening",
      "    description: Pre-randomisation screening visit.",
      "  - code: C1D1",
      "    label: Cycle 1 Day 1",
      "  - code: EOT",
      "    label: End of treatment"
    ),
    file.path(root, "visit.dta-vocabulary.yaml")
  )
}

write_supplier_profile_file <- function(root) {
  writeLines(
    c(
      "kind: dta_party_profile",
      "id: supplier_acme",
      'version: "1.0"',
      "role: supplier",
      "label: ACME Labs",
      "affiliation:",
      "  name: ACME Laboratories",
      "  country: DE"
    ),
    file.path(root, "supplier_acme.dta-party.yaml")
  )
}

# Two creation templates whose LABELS are what template_lineage_text() looks up.
write_chain_files <- function(root) {
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: root_tpl",
      'version: "1.0"',
      "label: Root Template",
      "base:",
      "  metadata:",
      "    title: T"
    ),
    file.path(root, "root_tpl.dta-template.yaml")
  )
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: mid_tpl",
      'version: "1.0"',
      "label: Mid Template",
      "extends: root_tpl@1.0",
      "base:",
      "  metadata:",
      "    title: T"
    ),
    file.path(root, "mid_tpl.dta-template.yaml")
  )
}

# A freshly built index over `dir` alone. The caller must already have set the
# env vars via local_clean_template_env().
fixture_index <- function() {
  app_fn("dta_template_index_invalidate")()
  app_fn("build_template_index")(refresh = TRUE)
}

# A definition in the shape load_template_definition() yields: a plain list,
# with the party/vocabulary slot blocks the options dialog reads.
demo_def <- function(options = list(), party_slots = list(), vocabulary_slots = list(),
                     datasets = list("gf_data.yaml")) {
  list(
    id = "biomarker_gf_acme",
    version = "1.0",
    label = "Biomarker GF DTS (ACME Labs)",
    description = "ACME Labs supplier deviation.",
    base = list(metadata = list(
      title = "BIOMARKER GF",
      supplier = list(affiliation = list(name = "External Supplier"))
    )),
    options = options,
    party_slots = party_slots,
    vocabulary_slots = vocabulary_slots,
    datasets = datasets
  )
}

demo_loaded <- function(def, lineage = character(0)) {
  list(ok = TRUE, value = list(
    def = def, lineage = lineage, path = "x.yaml",
    source_name = "lab", id = def$id, version = def$version
  ), error = NULL)
}

vocab_slot <- function(id = "reporting", mode = "closed", default = list("SCR"),
                       include = NULL, min = 0, label = "How are results reported?",
                       description = "") {
  slot <- list(
    id = id,
    target = "datasets.gf.columns.RES.values",
    vocabulary = "visit@1.0",
    label = label,
    description = description,
    mode = mode,
    default = default,
    min = min
  )
  if (!is.null(include)) slot$include <- include
  slot
}

# ---- Search field ----------------------------------------------------------

test_that("the search field is a type=search input carrying autofocus and its value", {
  html <- html_of(app_fn("template_picker_search_ui")("acme"))

  expect_match(html, 'id="template_search"', fixed = TRUE)
  expect_match(html, 'type="search"', fixed = TRUE)
  expect_match(html, "autofocus", fixed = TRUE)
  expect_match(html, 'value="acme"', fixed = TRUE)
  expect_match(html, "Search templates", fixed = TRUE)
})

# ---- Source filter ---------------------------------------------------------

test_that("the source filter appears only with a choice to make, and scales past four", {
  source_ui <- app_fn("template_picker_source_ui")

  expect_null(source_ui(character(0)))
  expect_null(source_ui("lab"))

  two <- html_of(source_ui(c("lab", "packaged")))
  expect_match(two, 'name="template_select_source"', fixed = TRUE)
  expect_match(two, "All sources", fixed = TRUE)
  expect_match(two, 'type="radio"', fixed = TRUE)
  # "All sources" is always the first row, and always the empty value that
  # template_picker_filter() reads as "do not restrict".
  expect_lt(
    regexpr("All sources", two, fixed = TRUE),
    regexpr("packaged", two, fixed = TRUE)
  )

  five <- html_of(source_ui(c("a", "b", "c", "d", "e")))
  expect_match(five, "<select", fixed = TRUE)
  expect_false(grepl('type="radio"', five, fixed = TRUE))
})

# ---- Token highlighting ----------------------------------------------------

test_that("highlighting marks matches case-insensitively and never emits raw HTML", {
  highlight <- app_fn("template_picker_highlight")

  html <- html_of(highlight("A <b>bold</b> Biomarker label", c("biomarker")))
  # The template author's text is escaped, not spliced in as markup -- a label
  # containing a tag must never be able to inject anything into the picker.
  expect_false(grepl("<b>", html, fixed = TRUE))
  expect_match(html, "&lt;b&gt;", fixed = TRUE)
  # Case-insensitive match, ORIGINAL casing kept inside the mark.
  expect_match(html, "<mark>Biomarker</mark>", fixed = TRUE)

  expect_identical(html_of(highlight("plain text", character(0))), "plain text")
  expect_identical(html_of(highlight("plain text", "")), "plain text")

  # Longest token wins where two match at the same position.
  both <- html_of(highlight("biomarker", c("bio", "biomarker")))
  expect_match(both, "<mark>biomarker</mark>", fixed = TRUE)
})

# ---- The list --------------------------------------------------------------

test_that("the list is one radio group with one .tmpl-entry per row", {
  entries <- tree_entries()
  html <- html_of(app_fn("template_picker_list_ui")(entries))

  expect_equal(count_occurrences(html, 'class="tmpl-entry'), 5)
  expect_match(html, 'id="template_select_name"', fixed = TRUE)
  expect_match(html, "shiny-input-radiogroup", fixed = TRUE)
  expect_match(html, 'role="radiogroup"', fixed = TRUE)
  expect_equal(count_occurrences(html, 'name="template_select_name"'), 5)
})

test_that("with no selection the first row is checked, so the server agrees with Shiny", {
  html <- html_of(app_fn("template_picker_list_ui")(tree_entries()))

  expect_equal(count_occurrences(html, "checked"), 1)
  expect_match(html, 'value="base" checked', fixed = TRUE)
})

test_that("a child row is indented and marked as non-top, and flat mode drops both", {
  entries <- tree_entries()

  tree <- html_of(app_fn("template_picker_list_ui")(entries))
  # Three top rows (base, other, solo); the two children carry no data-top,
  # which is exactly what the collapse CSS keys off.
  expect_equal(count_occurrences(tree, 'data-top=""'), 3)
  expect_match(tree, "margin-left:18px", fixed = TRUE)

  flat <- html_of(app_fn("template_picker_list_ui")(entries, flat = TRUE))
  expect_false(grepl("margin-left", flat, fixed = TRUE))
  expect_match(flat, "tmpl-picker-list flat", fixed = TRUE)
})

test_that("only the selected row's family is rendered open", {
  entries <- tree_entries()
  html <- html_of(app_fn("template_picker_list_ui")(entries, selected = "child_a"))

  # base/child_a/child_b are one family: all three open, child_a also selected.
  expect_equal(count_occurrences(html, 'class="tmpl-row open'), 3)
  expect_match(html, 'class="tmpl-row open selected"', fixed = TRUE)
  # The other two families stay collapsed.
  expect_equal(count_occurrences(html, 'class="tmpl-row"'), 2)
  expect_match(html, 'value="child_a" checked', fixed = TRUE)
})

test_that("the extends line and the variants badge appear only where they mean something", {
  html <- html_of(app_fn("template_picker_list_ui")(tree_entries()))

  # Only the two depth > 0 rows say what they extend.
  expect_equal(count_occurrences(html, "extends Base template"), 2)
  # Only the family with hidden descendants gets a badge, and it counts them.
  expect_equal(count_occurrences(html, "tmpl-variants"), 1)
  expect_match(html, "2 variants", fixed = TRUE)
})

test_that("the count line says how many of how many, and pluralises", {
  entries <- tree_entries()
  list_ui <- app_fn("template_picker_list_ui")

  expect_match(html_of(list_ui(entries)), "5 templates", fixed = TRUE)

  two <- entries[c(1, 4), , drop = FALSE]
  expect_match(html_of(list_ui(two, total = 5)), "2 of 5 templates", fixed = TRUE)

  one <- entries[1, , drop = FALSE]
  expect_match(html_of(list_ui(one, total = 5)), "1 of 5 templates", fixed = TRUE)
})

test_that("the source badge is rendered only when asked for", {
  entries <- tree_entries()
  with_source <- html_of(app_fn("template_picker_list_ui")(entries, show_source = TRUE))
  expect_equal(count_occurrences(with_source, "tmpl-source"), 5)

  without <- html_of(app_fn("template_picker_list_ui")(entries))
  expect_equal(count_occurrences(without, "tmpl-source"), 0)
})

test_that("an empty result gives direction and renders NO radio group", {
  empty <- tree_entries()[0, , drop = FALSE]
  html <- html_of(app_fn("template_picker_list_ui")(empty, total = 5))

  expect_match(html, "No templates match.", fixed = TRUE)
  expect_match(html, "Clear the search or pick another source.", fixed = TRUE)
  expect_false(grepl("shiny-input-radiogroup", html, fixed = TRUE))
  expect_equal(count_occurrences(html, 'class="tmpl-entry'), 0)
})

test_that("matched tokens are highlighted in the label, id and description", {
  entries <- tree_entries()
  html <- html_of(app_fn("template_picker_list_ui")(entries, tokens = "other", flat = TRUE))

  # "Other template" (label), "other" (id) and "Other description".
  expect_equal(count_occurrences(html, "<mark>"), 3)
})

# ---- The client-side script ------------------------------------------------

test_that("the picker script guards itself so a re-render cannot double-bind", {
  html <- html_of(app_fn("template_picker_scripts")())

  expect_match(html, "window.DTA_pickerBound", fixed = TRUE)
  expect_match(html, "template_select_next", fixed = TRUE)
  expect_match(html, "setInputValue('template_search'", fixed = TRUE)
})

# ---- Lineage text ----------------------------------------------------------

test_that("a nearest-first lineage reads root-first, with labels from the index", {
  dir <- withr::local_tempdir()
  write_chain_files(dir)
  local_clean_template_env(
    DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", dir),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = ""
  )
  idx <- fixture_index()
  lineage_text <- app_fn("template_lineage_text")

  expect_identical(lineage_text(character(0), idx), "")

  # resolve_template_inheritance() conses each parent onto the FRONT, so this
  # is nearest-first; the breadcrumb must read the other way round.
  txt <- lineage_text(c("mid_tpl@1.0", "root_tpl@1.0"), idx)
  expect_match(txt, "based on Root Template (root_tpl@1.0)", fixed = TRUE)
  expect_lt(
    regexpr("Root Template", txt, fixed = TRUE),
    regexpr("Mid Template", txt, fixed = TRUE)
  )

  # An ancestor the index does not know (abstract, or from a source that has
  # gone away) still names the family, by its raw id.
  expect_match(lineage_text("ghost@9.9", idx), "based on ghost (ghost@9.9)", fixed = TRUE)
})

# ---- The detail panel ------------------------------------------------------

test_that("the detail panel lists versions newest first and marks the selected one", {
  versions <- stats::setNames(c("1.1", "1.0"), c("1.1", "1.0"))
  html <- html_of(app_fn("template_picker_detail_ui")(
    demo_loaded(demo_def()), versions, "1.0", NULL
  ))

  expect_match(html, 'id="template_select_version"', fixed = TRUE)
  expect_lt(
    regexpr('value="1.1"', html, fixed = TRUE),
    regexpr('value="1.0"', html, fixed = TRUE)
  )
  expect_match(html, '<option value="1.0" selected>', fixed = TRUE)
  expect_match(html, "Biomarker GF DTS (ACME Labs)", fixed = TRUE)
})

test_that("the summary sentence pluralises datasets, options, parties and vocabularies", {
  detail_ui <- app_fn("template_picker_detail_ui")
  versions <- stats::setNames("1.0", "1.0")

  one <- demo_def(
    options = list(list(id = "title", label = "Title")),
    party_slots = list(list(id = "sup", target = "metadata.supplier", label = "Supplier")),
    vocabulary_slots = list(vocab_slot()),
    datasets = list(list(as = "gf_data"))
  )
  html_one <- html_of(detail_ui(demo_loaded(one), versions, "1.0", NULL))
  expect_match(html_one, "Builds 1 dataset, gf_data.", fixed = TRUE)
  expect_match(html_one, "Asks for 1 option, 1 party and 1 vocabulary.", fixed = TRUE)

  two <- demo_def(
    options = list(list(id = "a"), list(id = "b")),
    party_slots = list(
      list(id = "sup", target = "metadata.supplier", label = "Supplier"),
      list(id = "rec", target = "metadata.receiver", label = "Receiver")
    ),
    vocabulary_slots = list(vocab_slot("a"), vocab_slot("b")),
    datasets = list(list(as = "ds_a"), "ds_b.yaml")
  )
  html_two <- html_of(detail_ui(demo_loaded(two), versions, "1.0", NULL))
  expect_match(html_two, "Builds 2 datasets, ds_a, ds_b.yaml.", fixed = TRUE)
  expect_match(html_two, "Asks for 2 options, 2 parties and 2 vocabularies.", fixed = TRUE)
})

test_that("an unusable template reports why and STILL offers the version control", {
  versions <- stats::setNames(c("1.1", "1.0"), c("1.1", "1.0"))
  html <- html_of(app_fn("template_picker_detail_ui")(
    list(ok = FALSE, value = NULL, error = "is abstract"), versions, "1.0", NULL
  ))

  expect_match(html, "This template cannot be used: is abstract.", fixed = TRUE)
  expect_match(html, "Choose another one, or fix the file in its source.", fixed = TRUE)
  # The user may only be one version away from a working template; hiding the
  # dropdown behind the error would leave no way to reach it.
  expect_match(html, 'id="template_select_version"', fixed = TRUE)
})

# ---- Option controls -------------------------------------------------------

test_that("an option with choices is one combobox that allows typing by default", {
  opt <- list(
    id = "header", label = "Header", type = "text",
    choices = c("Alpha", "Beta")
  )
  html <- html_of(app_fn("render_template_option_input")(opt))

  expect_match(html, 'id="tmpl_opt_header"', fixed = TRUE)
  expect_match(html, '"create":true', fixed = TRUE)
  # No default, so the blank sentinel collect_template_selections() already
  # understands is what the control starts on.
  expect_match(html, '<option value="__blank__" selected>(leave blank)</option>', fixed = TRUE)
  # The sentinel row and the companion text field of the old dialog are gone.
  expect_false(grepl("__custom__", html, fixed = TRUE))
  expect_false(grepl("tmpl_opt_header_custom", html, fixed = TRUE))
})

test_that("allow_custom follows the template, in both directions", {
  render <- app_fn("render_template_option_input")
  ch <- c("Alpha", "Beta")

  # type: select forbids typing unless the template opts in.
  expect_match(
    html_of(render(list(id = "x", type = "select", choices = ch))),
    '"create":false',
    fixed = TRUE
  )
  expect_match(
    html_of(render(list(id = "x", type = "select", choices = ch, allow_custom = TRUE))),
    '"create":true',
    fixed = TRUE
  )
  # type: text permits it unless the template says otherwise.
  expect_match(
    html_of(render(list(id = "x", type = "text", choices = ch, allow_custom = FALSE))),
    '"create":false',
    fixed = TRUE
  )
})

test_that("a default that is not among the choices is added and selected", {
  opt <- list(
    id = "header", type = "text", choices = c("Alpha", "Beta"),
    default = "ACME Labs"
  )
  html <- html_of(app_fn("render_template_option_input")(opt))

  expect_match(html, '<option value="ACME Labs" selected>ACME Labs</option>', fixed = TRUE)
})

test_that("boolean is a checkbox, textarea spans the grid, and help is rendered", {
  render <- app_fn("render_template_option_input")

  bool <- html_of(render(list(
    id = "blinded", label = "Blinded?", type = "boolean",
    default = TRUE
  )))
  expect_match(bool, 'type="checkbox"', fixed = TRUE)
  expect_match(bool, "checked", fixed = TRUE)

  area <- html_of(render(list(id = "notes", type = "textarea", help = "Free prose.")))
  expect_match(area, "tmpl-opt-wide", fixed = TRUE)
  expect_match(area, "<textarea", fixed = TRUE)
  expect_match(area, "Free prose.", fixed = TRUE)
  expect_match(area, "tmpl-help", fixed = TRUE)

  plain <- html_of(render(list(id = "free", type = "text")))
  expect_match(plain, 'placeholder="Type a value"', fixed = TRUE)
  expect_false(grepl("tmpl-opt-wide", plain, fixed = TRUE))
})

test_that("an option with no id builds nothing", {
  expect_null(app_fn("render_template_option_input")(list(label = "Nameless")))
})

# ---- Vocabulary slot specs -------------------------------------------------

test_that("slot specs resolve each term to a value/label/description item", {
  dir <- withr::local_tempdir()
  write_vocabulary_file(dir)
  local_clean_template_env(
    DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", dir),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = ""
  )
  idx <- fixture_index()

  specs <- app_fn("template_vocab_slot_specs")(
    demo_def(vocabulary_slots = list(vocab_slot())), idx
  )
  expect_null(specs$error)
  expect_length(specs$slots, 1)

  items <- specs$slots[[1]]$items
  expect_length(items, 3)
  expect_identical(items[[1]]$value, "SCR")
  expect_match(items[[1]]$label, "SCR", fixed = TRUE)
  expect_match(items[[1]]$label, "Screening", fixed = TRUE)
  expect_identical(items[[1]]$description, "Pre-randomisation screening visit.")
  # A term with no description carries "", not NULL -- the selectize render
  # function tests the field directly.
  expect_identical(items[[2]]$description, "")
})

test_that("an unresolvable vocabulary is reported per slot, not as a dead dialog", {
  dir <- withr::local_tempdir()
  write_vocabulary_file(dir)
  local_clean_template_env(
    DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", dir),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = ""
  )
  idx <- fixture_index()

  slot <- vocab_slot()
  slot$vocabulary <- "nosuch@1.0"
  specs <- app_fn("template_vocab_slot_specs")(demo_def(vocabulary_slots = list(slot)), idx)

  expect_null(specs$error)
  expect_length(specs$slots, 1)
  expect_false(is.null(specs$slots[[1]]$error))
  expect_length(specs$slots[[1]]$items, 0)
})

# ---- The options dialog body -----------------------------------------------

# The fixture index every modal-body test uses: one vocabulary, one supplier
# party profile.
local_modal_index <- function(.local_envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .local_envir)
  write_vocabulary_file(dir)
  write_supplier_profile_file(dir)
  local_clean_template_env(
    DTATOOLS_TEMPLATE_SOURCES = paste0("t=dir:", dir),
    DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = "",
    .local_envir = .local_envir
  )
  fixture_index()
}

test_that("a closed vocabulary slot renders arrays, quick actions and its min hint", {
  idx <- local_modal_index()
  slot <- vocab_slot(include = list("SCR"), default = list("SCR"), min = 1)
  def <- demo_def(vocabulary_slots = list(slot))
  specs <- app_fn("template_vocab_slot_specs")(def, idx)

  html <- html_of(app_fn("template_options_modal_body")(def, demo_loaded(def), idx, specs))

  # Shiny serialises this block with auto_unbox on, so a length-one option list
  # or default would collapse to a scalar unless they are passed as LISTS --
  # which is exactly the case a bigger fixture would never catch.
  expect_match(html, '"options":[{"value":"SCR"', fixed = TRUE)
  expect_match(html, '"items":["SCR"]', fixed = TRUE)
  # Shiny appends its own a11y plugin after ours, so this is a prefix match.
  expect_match(html, '"plugins":["remove_button"', fixed = TRUE)
  expect_match(html, '"searchField":["label","value","description"]', fixed = TRUE)
  expect_match(html, '"create":false', fixed = TRUE)
  expect_match(html, '"description":"Pre-randomisation screening visit."', fixed = TRUE)

  expect_match(html, 'id="tmpl_vocab_reporting"', fixed = TRUE)
  expect_match(html, 'id="tmpl_vocab_reporting_count"', fixed = TRUE)
  expect_match(html, "DTA_vocabSet(this,", fixed = TRUE)
  # The slot id never reaches script text: a template author's quote in it
  # must stay an attribute value, never a way out of a JS string literal.
  expect_false(grepl("DTA_vocabSet('tmpl_vocab_", html, fixed = TRUE))
  expect_match(html, "All (1)", fixed = TRUE)
  expect_match(html, ">None<", fixed = TRUE)
  expect_match(html, "Default (1)", fixed = TRUE)
  expect_match(html, "Choose at least 1.", fixed = TRUE)
  # The typing hint belongs to open mode only.
  expect_false(grepl("press Enter", html, fixed = TRUE))
})

test_that("an open slot allows typing, says so, and carries its defaults for the link", {
  idx <- local_modal_index()
  slot <- vocab_slot(mode = "open", default = list("SCR", "C1D1"))
  def <- demo_def(vocabulary_slots = list(slot))
  specs <- app_fn("template_vocab_slot_specs")(def, idx)

  html <- html_of(app_fn("template_options_modal_body")(def, demo_loaded(def), idx, specs))

  expect_match(html, '"create":true', fixed = TRUE)
  expect_match(
    html, "Type a term and press Enter to add one that is not in the list.",
    fixed = TRUE
  )
  expect_match(html, "Default (2)", fixed = TRUE)
  # A unit separator, because a term code may hold a comma or a space.
  expect_match(html, "SCR\u001fC1D1", fixed = TRUE)
})

test_that("a slot list that cannot be read at all is reported once, in place", {
  idx <- local_modal_index()
  def <- demo_def()
  html <- html_of(app_fn("template_options_modal_body")(
    def, demo_loaded(def), idx, list(slots = list(), error = "bad target")
  ))

  expect_match(html, "This template's vocabulary slots could not be read: bad target",
    fixed = TRUE
  )
})

test_that("a party slot names the template's own default and marks required slots", {
  idx <- local_modal_index()
  def <- demo_def(party_slots = list(
    list(id = "supplier", target = "metadata.supplier", label = "Supplier", required = TRUE)
  ))
  html <- html_of(app_fn("template_options_modal_body")(
    def, demo_loaded(def), idx, list(slots = list(), error = NULL)
  ))

  expect_match(html, 'id="tmpl_party_supplier"', fixed = TRUE)
  # Named, not the opaque "(use template default)" the old dialog showed.
  expect_match(
    html, '<option value="" selected>(template default: External Supplier)</option>',
    fixed = TRUE
  )
  expect_match(html, "Supplier *", fixed = TRUE)
  # The eligible profile from the index is offered alongside it.
  expect_match(html, '<option value="supplier_acme">ACME Labs</option>', fixed = TRUE)
})

test_that("the header names id, version, source and lineage, and options land in a grid", {
  idx <- local_modal_index()
  def <- demo_def(options = list(
    list(id = "title", label = "Document title", type = "text"),
    list(id = "notes", label = "Notes", type = "textarea")
  ))
  html <- html_of(app_fn("template_options_modal_body")(
    def, demo_loaded(def, lineage = "biomarker_gf@1.1"), idx,
    list(slots = list(), error = NULL)
  ))

  expect_match(html, "biomarker_gf_acme", fixed = TRUE)
  expect_match(html, "version 1.0", fixed = TRUE)
  expect_match(html, "based on biomarker_gf (biomarker_gf@1.1)", fixed = TRUE)
  expect_match(html, "ACME Labs supplier deviation.", fixed = TRUE)
  expect_match(html, "tmpl-opts-grid", fixed = TRUE)
  expect_match(html, 'id="tmpl_opt_title"', fixed = TRUE)
  expect_match(html, "tmpl-opt-wide", fixed = TRUE)
})

test_that("a template with no options says so rather than showing an empty grid", {
  idx <- local_modal_index()
  def <- demo_def()
  html <- html_of(app_fn("template_options_modal_body")(
    def, demo_loaded(def), idx, list(slots = list(), error = NULL)
  ))

  expect_match(html, "This template has no configurable options.", fixed = TRUE)
  expect_false(grepl("tmpl-opts-grid", html, fixed = TRUE))
})

test_that("carry-over offers the open document only when one is open", {
  idx <- local_modal_index()
  def <- demo_def()
  body <- app_fn("template_options_modal_body")

  closed <- html_of(body(def, demo_loaded(def), idx, list(slots = list(), error = NULL)))
  expect_match(closed, "Carry over metadata", fixed = TRUE)
  expect_match(closed, "Don't carry anything over", fixed = TRUE)
  expect_false(grepl("From the open document", closed, fixed = TRUE))

  open <- html_of(body(
    def, demo_loaded(def), idx, list(slots = list(), error = NULL),
    has_open_doc = TRUE
  ))
  expect_match(open, "From the open document", fixed = TRUE)
  expect_match(open, 'id="tmpl_carry_fields"', fixed = TRUE)
})
