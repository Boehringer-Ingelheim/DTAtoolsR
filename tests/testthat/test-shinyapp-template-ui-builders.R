# Coverage for inst/shiny/dta_app/R/template_ui.R -- the pure UI builders
# behind "Create new from template". Everything here is asserted on rendered
# HTML, with no server: that is the whole point of moving these out of app.R,
# and test-shinyapp-template-ui.R covers the server wiring that calls them.

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

test_that("the library filter appears only with a choice to make, and lets several be chosen", {
  source_ui <- app_fn("template_picker_source_ui")

  expect_null(source_ui(character(0)))
  expect_null(source_ui("lab"))

  two <- html_of(source_ui(c("lab", "packaged"), counts = c(lab = 3L, packaged = 12L)))
  # Checkboxes, one per library: a radio group could only ever say one or
  # all. There is no "All" row -- nothing checked means every library.
  expect_equal(count_occurrences(two, 'type="checkbox"'), 2)
  expect_equal(count_occurrences(two, 'name="template_select_source"'), 2)
  expect_false(grepl('type="radio"', two, fixed = TRUE))
  expect_false(grepl("checked", two, fixed = TRUE))
  expect_match(two, '<span class="tmpl-chip-count">12</span>', fixed = TRUE)

  # A selection is restored on re-render, and a name that is no longer a
  # library is dropped rather than rendered as a phantom choice.
  chosen <- html_of(source_ui(c("a", "b", "c"), selected = c("a", "c", "gone")))
  expect_equal(count_occurrences(chosen, 'checked="checked"'), 2)
  expect_match(chosen, 'value="a" checked', fixed = TRUE)
  expect_match(chosen, 'value="c" checked', fixed = TRUE)
  expect_false(grepl("gone", chosen, fixed = TRUE))
})

test_that("a source status line appears only for a source served from a stale cache", {
  status_row <- app_fn("template_source_status_row")

  # Healthy: the library filter and each row's badge already name it.
  expect_null(status_row(list(name = "lib", scheme = "git", ok = TRUE, stale = FALSE)))
  # Failed outright: that is the diagnostics block's job, with the error.
  expect_null(status_row(list(name = "lib", scheme = "git", ok = FALSE, stale = FALSE)))

  stale <- html_of(status_row(
    list(name = "lib", scheme = "git", ok = TRUE, stale = TRUE, stale_age = 7200)
  ))
  expect_match(stale, "<strong>lib</strong>", fixed = TRUE)
  expect_match(stale, "could not be refreshed", fixed = TRUE)
  expect_match(stale, "last fetched 2 hours ago", fixed = TRUE)
})

test_that("past six libraries the filter becomes a multi-select dropdown", {
  seven <- html_of(app_fn("template_picker_source_ui")(letters[1:7], selected = "b"))

  expect_match(seven, "<select", fixed = TRUE)
  expect_match(seven, "multiple", fixed = TRUE)
  expect_match(seven, '<option value="b" selected>', fixed = TRUE)
  expect_false(grepl('type="checkbox"', seven, fixed = TRUE))
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
  expect_match(html, "Clear the search or choose other libraries.", fixed = TRUE)
  expect_false(grepl("shiny-input-radiogroup", html, fixed = TRUE))
  expect_equal(count_occurrences(html, 'class="tmpl-entry'), 0)
})

test_that("matched tokens are highlighted in the label, id and description", {
  entries <- tree_entries()
  html <- html_of(app_fn("template_picker_list_ui")(entries, tokens = "other", flat = TRUE))

  # "Other template" (label), "other" (id) and "Other description".
  expect_equal(count_occurrences(html, "<mark>"), 3)
})

test_that("a highlight adds no whitespace around the matched text", {
  # htmltools breaks the line around a child tag by default, which the browser
  # shows as a space: "(LabCorp)" came out as "( LabCorp )".
  html <- html_of(htmltools::tags$strong(
    app_fn("template_picker_highlight")("Oncology (LabCorp)", "labcorp")
  ))

  expect_match(html, "Oncology (<mark>LabCorp</mark>)", fixed = TRUE)
})

test_that("siblings under a hidden root are separate families, each with its own variants", {
  # a and b both extend an abstract (never listed) root, so the `family`
  # column names that root for both; a has one child of its own. Grouping by
  # that column made a AND b each claim "1 variant" and open together.
  entries <- make_entries(
    id = c("a", "a_child", "b"),
    label = c("A", "A child", "B"),
    family = c("root", "root", "root"),
    depth = c(1, 2, 1),
    indent = c(0, 1, 0),
    lineage = c("Root", "Root › A", "Root")
  )
  html <- html_of(app_fn("template_picker_list_ui")(entries))

  expect_equal(count_occurrences(html, "tmpl-variants"), 1)
  expect_match(html, 'data-family="a" data-id="a_child"', fixed = TRUE)
  expect_match(html, 'data-family="b" data-id="b"', fixed = TRUE)
  # Only a's family is open: a and its child, not b.
  expect_equal(count_occurrences(html, 'class="tmpl-row open'), 2)
})

test_that("a child whose parent is not listed is shown as a top row, not hidden", {
  entries <- make_entries(
    id = c("orphan_child"), label = "Orphan child", family = "gone",
    depth = 1, indent = 1, lineage = "Gone"
  )
  html <- html_of(app_fn("template_picker_list_ui")(entries))

  expect_match(html, 'data-top=""', fixed = TRUE)
  expect_false(grepl("margin-left", html, fixed = TRUE))
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

test_that("a variant-only summary explains the required platform choice", {
  def <- demo_def(datasets = list())
  def$variants <- list(id = "trial_platform", label = "Trial platform")
  expect_identical(
    app_fn("tmpl_detail_summary")(def),
    "Choose Trial platform first. The selected platform determines the datasets and agreement options."
  )
})

test_that("the summary sentence names an inline dataset by its id, not by its label", {
  detail_ui <- app_fn("template_picker_detail_ui")
  versions <- stats::setNames("1.0", "1.0")

  # `as:` outranks `id:`; an inline entry's `name` is only its label, so it is
  # never what the sentence prints when an id (or `as:`) is there.
  def <- demo_def(datasets = list(
    list(id = "dm", name = "Demographics", type = "tabular"),
    list(as = "vs_copy", id = "vs", name = "Vital signs")
  ))
  html <- html_of(detail_ui(demo_loaded(def), versions, "1.0", NULL))

  expect_match(html, "Builds 2 datasets, dm, vs_copy.", fixed = TRUE)
  expect_false(grepl("Demographics", html, fixed = TRUE))
  expect_false(grepl("Vital signs", html, fixed = TRUE))
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

test_that("an option with choices is a typeable field plus a suggestion picker", {
  opt <- list(
    id = "header", label = "Header", type = "text",
    choices = c("Alpha", "Beta")
  )
  html <- html_of(app_fn("render_template_option_input")(opt))
  doc <- xml2::read_html(html)

  # The value lives in a real text input, which the row's label points at ...
  field <- xml2::xml_find_first(doc, "//input[@id='tmpl_opt_header']")
  expect_identical(xml2::xml_attr(field, "type"), "text")
  expect_match(html, 'for="tmpl_opt_header"', fixed = TRUE)
  # ... and the picker is a separate control that comes AFTER it, so the field
  # is the first stop in tab order.
  expect_identical(
    xml2::xml_attr(xml2::xml_find_first(doc, "//select"), "id"),
    "tmpl_opt_header_suggestion"
  )
  expect_lt(
    regexpr('id="tmpl_opt_header"', html, fixed = TRUE),
    regexpr('id="tmpl_opt_header_suggestion"', html, fixed = TRUE)
  )
  # No default, so the field starts empty. The picker starts on nothing: a
  # picker sitting on an item cannot fire for that same item, so "(leave blank)"
  # (or the default) could not be re-picked after typing.
  expect_true(xml2::xml_attr(field, "value") %in% c("", NA_character_))
  # (Shiny's markup still flags the first row `selected`; the script below
  # clears it on initialisation, which is what the user sees.)
  expect_match(html, "(leave blank)", fixed = TRUE)
  picker_config <- jsonlite::fromJSON(xml2::xml_text(xml2::xml_find_first(
    doc, "//script[@data-for='tmpl_opt_header_suggestion']"
  )))
  expect_match(picker_config$onInitialize, "this.clear(true)", fixed = TRUE)
  expect_match(html, "Alpha", fixed = TRUE)
  expect_match(html, "Beta", fixed = TRUE)
  # The picker cannot create values: typing happens in the field.
  expect_false(grepl('"create":true', html, fixed = TRUE))
  # Its label names the field for screen readers.
  expect_match(html, "Choose a suggestion", fixed = TRUE)
  expect_match(html, 'class="visually-hidden"> for Header', fixed = TRUE)
  expect_false(grepl("__custom__", html, fixed = TRUE))
})

test_that("the field starts on the default and the picker script writes into the field", {
  html <- html_of(app_fn("render_template_option_input")(list(
    id = "header", type = "text", choices = c("Alpha", "Beta"), default = "Beta"
  )))
  doc <- xml2::read_html(html)

  expect_identical(
    xml2::xml_attr(xml2::xml_find_first(doc, "//input[@id='tmpl_opt_header']"), "value"),
    "Beta"
  )
  config <- xml2::xml_text(xml2::xml_find_first(
    doc, "//script[@data-for='tmpl_opt_header_suggestion']"
  ))
  expect_match(
    jsonlite::fromJSON(config)$onChange,
    'document.getElementById("tmpl_opt_header")',
    fixed = TRUE
  )
})

test_that("allow_custom follows the template, in both directions", {
  render <- app_fn("render_template_option_input")
  ch <- c("Alpha", "Beta")

  # type: select forbids typing unless the template opts in: a strict dropdown,
  # with no field to type into.
  strict <- html_of(render(list(id = "x", type = "select", choices = ch)))
  expect_match(strict, '"create":false', fixed = TRUE)
  expect_false(grepl("tmpl_opt_x_suggestion", strict, fixed = TRUE))
  expect_match(strict, 'for="tmpl_opt_x-selectized"', fixed = TRUE)

  opted_in <- html_of(render(list(id = "x", type = "select", choices = ch, allow_custom = TRUE)))
  expect_match(opted_in, 'id="tmpl_opt_x_suggestion"', fixed = TRUE)
  expect_match(opted_in, 'for="tmpl_opt_x"', fixed = TRUE)

  # type: text permits it unless the template says otherwise.
  closed <- html_of(render(list(id = "x", type = "text", choices = ch, allow_custom = FALSE)))
  expect_match(closed, '"create":false', fixed = TRUE)
  expect_false(grepl("tmpl_opt_x_suggestion", closed, fixed = TRUE))
})

test_that("a strict dropdown still shows a default that is not among its choices", {
  html <- html_of(app_fn("render_template_option_input")(list(
    id = "x", type = "select", choices = c("Alpha", "Beta"), default = "Gamma"
  )))

  expect_match(html, '<option value="Gamma" selected>Gamma</option>', fixed = TRUE)
})

test_that("a single free-text choice is a text field holding it, ready to edit", {
  render <- app_fn("render_template_option_input")

  html <- html_of(render(list(id = "header", type = "text", choices = "Alpha")))
  expect_match(html, '<input id="tmpl_opt_header" type="text"', fixed = TRUE)
  expect_match(html, 'value="Alpha"', fixed = TRUE)
  expect_false(grepl("selectize", html, fixed = TRUE))
  # The label points at the input itself, not a selectize proxy.
  expect_match(html, 'for="tmpl_opt_header"', fixed = TRUE)

  # A declared default is still what the field starts with.
  with_default <- html_of(render(list(id = "header", type = "text", choices = "Alpha", default = "Beta")))
  expect_match(with_default, 'value="Beta"', fixed = TRUE)

  # A strict select keeps its dropdown: typing is not allowed there.
  strict <- html_of(render(list(id = "header", type = "select", choices = "Alpha")))
  expect_match(strict, '"create":false', fixed = TRUE)
})

test_that("a default that is not among the choices fills the field and is offered in the picker", {
  opt <- list(
    id = "header", type = "text", choices = c("Alpha", "Beta"),
    default = "ACME Labs"
  )
  html <- html_of(app_fn("render_template_option_input")(opt))

  # Offered as a row so it can be put back after typing, but not pre-selected.
  expect_match(html, '<option value="ACME Labs">ACME Labs</option>', fixed = TRUE)
  expect_identical(
    xml2::xml_attr(
      xml2::xml_find_first(xml2::read_html(html), "//input[@id='tmpl_opt_header']"), "value"
    ),
    "ACME Labs"
  )
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

test_that("a textarea suggestion is selectable without losing multiline editing", {
  html <- html_of(app_fn("render_template_option_input")(list(
    id = "scope", label = "Delivery scope", type = "textarea",
    allow_custom = TRUE,
    choices = c("Complete data", "Incremental data"),
    default = "Complete data"
  )))

  expect_match(html, 'id="tmpl_opt_scope"', fixed = TRUE)
  expect_match(html, "<textarea", fixed = TRUE)
  expect_match(html, 'id="tmpl_opt_scope_suggestion"', fixed = TRUE)
  expect_match(html, "Complete data", fixed = TRUE)
  expect_match(html, "Incremental data", fixed = TRUE)
  expect_match(html, "(leave blank)", fixed = TRUE)
  expect_match(html, "dispatchEvent", fixed = TRUE)
  expect_match(html, 'rows="3"', fixed = TRUE)
  config <- xml2::xml_text(xml2::xml_find_first(
    xml2::read_html(html), "//script[@data-for='tmpl_opt_scope_suggestion']"
  ))
  callback <- jsonlite::fromJSON(config)$onChange
  expect_identical(callback, paste0(
    "function(value) {",
    'var target = document.getElementById("tmpl_opt_scope");',
    "if (target) {",
    "target.value = value === \"__blank__\" ? '' : (value || '');",
    "target.dispatchEvent(new Event('input', { bubbles: true }));",
    "}",
    "this.clear(true);",
    "}"
  ))
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
  # min: 1 is said by the badge alone; only a higher floor gets a sentence.
  expect_match(html, '<span class="tmpl-req">Required</span>', fixed = TRUE)
  expect_false(grepl("Choose at least", html, fixed = TRUE))
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
  # The template's own party is the selected row, shown by its name -- no
  # "template default" wording.
  expect_match(html, '<option value="" selected>External Supplier</option>', fixed = TRUE)
  expect_false(grepl("template default", html, fixed = TRUE))
  expect_match(html, '<span class="tmpl-req">Required</span>', fixed = TRUE)
  # The eligible profile from the index is offered alongside it.
  expect_match(html, '<option value="supplier_acme">ACME Labs</option>', fixed = TRUE)
})

test_that("a party row is never two identical rows, and says so when the template has no party", {
  idx <- local_modal_index()
  slot <- list(id = "supplier", target = "metadata.supplier", label = "Supplier")
  body <- function(def) {
    html_of(app_fn("template_options_modal_body")(
      def, demo_loaded(def), idx, list(slots = list(), error = NULL)
    ))
  }

  # A saved profile that shares the template party's name is told apart.
  same <- demo_def(party_slots = list(slot))
  same$base$metadata$supplier$affiliation$name <- "ACME Labs"
  html <- body(same)
  expect_match(html, '<option value="" selected>ACME Labs</option>', fixed = TRUE)
  expect_match(
    html, '<option value="supplier_acme">ACME Labs (saved profile)</option>',
    fixed = TRUE
  )

  # No party block at all: leaving the row empties the slot, so the row says "(none)".
  none <- demo_def(party_slots = list(slot))
  none$base$metadata$supplier <- NULL
  expect_match(body(none), '<option value="" selected>(none)</option>', fixed = TRUE)

  # A block without a name is still the template's party.
  unnamed <- demo_def(party_slots = list(slot))
  unnamed$base$metadata$supplier <- list(contacts = list())
  expect_match(body(unnamed), '<option value="" selected>(as in template)</option>', fixed = TRUE)
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

test_that("options are grouped by the metadata block they target, and the sidebar links each section", {
  idx <- local_modal_index()
  def <- demo_def(
    options = list(
      list(id = "title", label = "Document title", type = "text", target = "metadata.title"),
      list(id = "freq", label = "Frequency", type = "text", target = "metadata.transmission.frequency"),
      list(id = "blind", label = "Blinded?", type = "boolean", target = "metadata.transmission.blinded_transfer")
    ),
    party_slots = list(list(id = "supplier", target = "metadata.supplier", label = "Supplier"))
  )
  html <- html_of(app_fn("template_options_modal_body")(
    def, demo_loaded(def), idx, list(slots = list(), error = NULL)
  ))

  expect_match(html, '<h4 class="tmpl-group-title">General</h4>', fixed = TRUE)
  expect_match(html, '<h4 class="tmpl-group-title">Transmission</h4>', fixed = TRUE)
  # Grouping keeps each option's own control id -- the collectors read them.
  expect_lt(regexpr('id="tmpl_opt_title"', html), regexpr("Transmission</h4>", html))
  expect_gt(regexpr('id="tmpl_opt_blind"', html), regexpr("Transmission</h4>", html))
  # The label sits in the row's left column and points at its control.
  expect_match(html, '<label class="tmpl-field-label" for="tmpl_opt_freq">Frequency', fixed = TRUE)
  expect_match(html, 'class="tmpl-switch"', fixed = TRUE)

  # One link per section present, with its count; no vocabulary section here.
  expect_match(html, 'class="tmpl-toc-link active" data-sec="options"', fixed = TRUE)
  expect_match(html, 'data-sec="parties"', fixed = TRUE)
  expect_match(html, 'data-sec="carry"', fixed = TRUE)
  expect_false(grepl('data-sec="vocab"', html, fixed = TRUE))
  expect_match(html, '<span class="tmpl-toc-count">3</span>', fixed = TRUE)
})

test_that("a single group gets no group heading", {
  idx <- local_modal_index()
  def <- demo_def(options = list(list(id = "title", type = "text", target = "metadata.title")))
  html <- html_of(app_fn("template_options_modal_body")(
    def, demo_loaded(def), idx, list(slots = list(), error = NULL)
  ))
  expect_false(grepl("tmpl-group-title", html, fixed = TRUE))
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
  expect_match(closed, "<span>Nothing</span>", fixed = TRUE)
  expect_false(grepl("The open document", closed, fixed = TRUE))

  open <- html_of(body(
    def, demo_loaded(def), idx, list(slots = list(), error = NULL),
    has_open_doc = TRUE
  ))
  expect_match(open, "The open document", fixed = TRUE)
  expect_match(open, 'id="tmpl_carry_fields"', fixed = TRUE)
})

# ---- The column editor's term checklist ------------------------------------

checklist_terms <- function(n) {
  lapply(seq_len(n), function(i) {
    list(code = sprintf("T%03d", i), label = sprintf("Term %d", i), description = sprintf("About term %d.", i))
  })
}

test_that("a small vocabulary is a plain checklist: every term visible, no filter", {
  html <- html_of(app_fn("vocab_term_checklist")("pick", checklist_terms(3), selected = c("T001", "T003")))

  expect_match(html, 'value="T001" checked', fixed = TRUE)
  expect_false(grepl('value="T002" checked', html, fixed = TRUE))
  # htmltools puts a newline between a tag and the text after it.
  expect_match(html, "T002</span>\\s+— Term 2")
  expect_match(html, "About term 2.", fixed = TRUE)
  expect_match(html, "2 of 3 selected", fixed = TRUE)
  expect_false(grepl("vocab-pick-filter", html, fixed = TRUE))
  # The script text is constant: nothing vocabulary-authored is spliced into it.
  expect_match(html, 'onclick="return DTA_vocabCheck(this, false)"', fixed = TRUE)
})

test_that("a large vocabulary gets a filter naming its size", {
  html <- html_of(app_fn("vocab_term_checklist")("pick", checklist_terms(300), selected = "T001"))

  expect_match(html, "Filter 300 terms by code, label or description", fixed = TRUE)
  expect_match(html, "1 of 300 selected", fixed = TRUE)
  expect_equal(count_occurrences(html, 'type="checkbox"'), 300L)
  # Typing and the native clear (Escape / the x) are different events: Escape
  # fires `search` and no `input`, so both must run the filter or the field
  # and the rows disagree. Constant text, as with the links.
  expect_match(html, 'oninput="DTA_vocabFilter(this)"', fixed = TRUE)
  expect_match(html, 'onsearch="DTA_vocabFilter(this)"', fixed = TRUE)
})

test_that("values the vocabulary does not have are listed first and counted", {
  html <- html_of(app_fn("vocab_term_checklist")(
    "pick", checklist_terms(2),
    selected = c("OWN", "T001"), extras = "OWN"
  ))

  expect_match(html, 'value="OWN" checked', fixed = TRUE)
  expect_match(html, "Not in this vocabulary", fixed = TRUE)
  expect_lt(regexpr('value="OWN"', html, fixed = TRUE), regexpr('value="T001"', html, fixed = TRUE))
  expect_match(html, "2 of 3 selected", fixed = TRUE)
})
