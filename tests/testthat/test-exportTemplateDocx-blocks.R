# Tests for block placeholders in export_with_template() -- placeholders that
# expand to real Word content (tables, headings, bulleted paragraphs) instead
# of a text run. See thoughts/shared/plans/PLAN-word-template-block-export.md
# for the design these tests pin.
#
# helper-docx.R (.make_template(), .docx_text(), .docx_paragraphs(),
# .read_docx_body_xml()) is sourced automatically by testthat before this
# file; it is reused rather than duplicated here.

# officer::docx_summary() of a filled-in template, for structural assertions
# (content_type / doc_index / text). Reused by every test below instead of
# repeating the read_docx() + docx_summary() pair.
.block_docx_summary <- function(path) {
  officer::docx_summary(officer::read_docx(path))
}

# The text of every table cell in a docx_summary() data frame, in document
# order.
.block_table_cells <- function(summary) {
  summary$text[summary$content_type == "table cell"]
}

# Given a docx_summary() that contains exactly one "Rule ID" / "Description" /
# "Definition" table (the shape .build_rules_table() always produces), return
# the Description text keyed by Rule ID. The header row is dropped positionally
# rather than by matching its label text, because a rule id or description
# could legitimately equal the header text itself.
.block_rule_table_descriptions <- function(summary) {
  cells <- .block_table_cells(summary)
  data_cells <- cells[-(1:3)]
  ids <- data_cells[c(TRUE, FALSE, FALSE)]
  descriptions <- data_cells[c(FALSE, TRUE, FALSE)]
  names(descriptions) <- ids
  descriptions
}

# Same table, the Definition column (exact YAML) keyed by Rule ID.
.block_rule_table_definitions <- function(summary) {
  cells <- .block_table_cells(summary)
  data_cells <- cells[-(1:3)]
  definitions <- data_cells[c(FALSE, FALSE, TRUE)]
  names(definitions) <- data_cells[c(TRUE, FALSE, FALSE)]
  definitions
}

# The Definition text of every rules table in the document (found by its
# "Rule ID" first cell, so other tables in the same document are ignored),
# keyed by Rule ID, in document order.
.block_all_rule_definitions <- function(summary) {
  cells <- summary[summary$content_type == "table cell", ]
  out <- character(0)
  for (ti in unique(cells$table_index)) {
    text <- cells$text[cells$table_index == ti]
    if (!identical(text[[1]], "Rule ID")) next
    body <- text[-(1:3)]
    out <- c(out, stats::setNames(body[c(FALSE, FALSE, TRUE)], body[c(TRUE, FALSE, FALSE)]))
  }
  out
}

# Build a template whose header (not body) contains header_text as its whole
# paragraph. officer::body_add_par() only reaches the body, and officer's
# blank starter document has no header part at all, so one has to be spliced
# into the OPC package by hand: a word/header1.xml part, a Content_Types
# override, a relationship, and a headerReference inside the body sectPr.
# The relationship id must have a numeric suffix ("rIdNN") -- officer's own
# reader strips "rId" and coerces the remainder to an integer, and warns
# ("invalid id(s)") if that fails.
.make_template_with_header <- function(body_lines, header_text) {
  base_path <- .make_template(body_lines)
  on.exit(unlink(base_path, force = TRUE), add = TRUE)

  ex <- tempfile("block_hdr_build_")
  dir.create(ex)
  on.exit(unlink(ex, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(base_path, exdir = ex)

  header_xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<w:hdr xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
    '<w:p><w:r><w:t xml:space="preserve">', header_text, "</w:t></w:r></w:p></w:hdr>"
  )
  writeLines(header_xml, file.path(ex, "word", "header1.xml"), useBytes = TRUE)

  ct_path <- file.path(ex, "[Content_Types].xml")
  ct <- paste(readLines(ct_path, warn = FALSE), collapse = "")
  ct <- sub(
    "</Types>",
    paste0(
      '<Override PartName="/word/header1.xml" ',
      'ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml"/>',
      "</Types>"
    ),
    ct,
    fixed = TRUE
  )
  writeLines(ct, ct_path, useBytes = TRUE)

  rel_path <- file.path(ex, "word", "_rels", "document.xml.rels")
  rel <- paste(readLines(rel_path, warn = FALSE), collapse = "")
  rel <- sub(
    "</Relationships>",
    paste0(
      '<Relationship Id="rId900" ',
      'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/header" ',
      'Target="header1.xml"/></Relationships>'
    ),
    rel,
    fixed = TRUE
  )
  writeLines(rel, rel_path, useBytes = TRUE)

  doc_path <- file.path(ex, "word", "document.xml")
  docxml <- paste(readLines(doc_path, warn = FALSE), collapse = "")
  docxml <- sub(
    "(<w:sectPr[^>]*>)",
    '\\1<w:headerReference w:type="default" r:id="rId900"/>',
    docxml
  )
  writeLines(docxml, doc_path, useBytes = TRUE)

  out <- tempfile(fileext = ".docx")
  .zip_docx_dir(ex, out)
  out
}

test_that("{COLUMN_SPECS} renders a real table in place, between the surrounding paragraphs", {
  template <- .make_template(c("BEFORE", "{COLUMN_SPECS}", "AFTER"))
  on.exit(unlink(template, force = TRUE), add = TRUE)

  dta <- create_example_DTA()
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  summary <- .block_docx_summary(out)
  before_idx <- summary$doc_index[summary$content_type == "paragraph" & summary$text == "BEFORE"]
  after_idx <- summary$doc_index[summary$content_type == "paragraph" & summary$text == "AFTER"]
  expect_length(before_idx, 1)
  expect_length(after_idx, 1)

  cell_idx <- summary$doc_index[summary$content_type == "table cell"]
  expect_gt(length(cell_idx), 0)
  expect_true(all(cell_idx > before_idx & cell_idx < after_idx))

  cell_text <- .block_table_cells(summary)
  ids <- unlist(lapply(dta@datasets, function(ds) {
    vapply(ds@specs@columns, function(spec) spec@id, character(1))
  }))
  for (id in ids) {
    expect_true(id %in% cell_text)
  }

  expect_false(grepl("{COLUMN_SPECS}", .docx_text(out), fixed = TRUE))
})

# Per body-level table of a docx: its layout type, its width attribute, its
# column grid (dxa) and its first cell's text, plus the text width (dxa) of the
# section THE TABLE LIES IN -- the next paragraph-level w:sectPr after the
# table, else the body-level one (a section break ends the section that holds
# everything before it). Taken from the file's own page size, margins and
# columns, not from the renderer under test; a section of equal columns offers
# a table one column's width.
.block_table_grids <- function(path) {
  doc <- xml2::read_xml(.read_docx_body_xml(path))
  body <- xml2::xml_find_first(doc, ".//*[local-name()='body']")
  num <- function(node, attr) as.numeric(xml2::xml_attr(node, attr))
  sect_width <- function(sect) {
    pg <- xml2::xml_find_first(sect, "./*[local-name()='pgSz']")
    mar <- xml2::xml_find_first(sect, "./*[local-name()='pgMar']")
    cols <- xml2::xml_find_first(sect, "./*[local-name()='cols']")
    width <- num(pg, "w") - num(mar, "left") - num(mar, "right")
    n <- num(cols, "num")
    if (!is.na(n) && n > 1) {
      width <- (width - (n - 1) * num(cols, "space")) / n
    }
    width
  }
  # The sectPr that ends the section containing `node`.
  section_of <- function(node) {
    brk <- xml2::xml_find_first(
      node,
      "./following-sibling::*[local-name()='p'][./*[local-name()='pPr']/*[local-name()='sectPr']][1]/*[local-name()='pPr']/*[local-name()='sectPr']"
    )
    if (inherits(brk, "xml_missing")) {
      brk <- xml2::xml_find_first(body, "./*[local-name()='sectPr']")
    }
    brk
  }
  tables <- xml2::xml_find_all(body, "./*[local-name()='tbl']")
  list(
    # Text width of the document's first section, for tests that must tell
    # the section a table lies in from the first one.
    first_dxa = sect_width(xml2::xml_find_first(body, ".//*[local-name()='sectPr']")),
    tables = lapply(tables, function(tbl) {
      layout <- xml2::xml_find_first(tbl, "./*[local-name()='tblPr']/*[local-name()='tblLayout']")
      width <- xml2::xml_find_first(tbl, "./*[local-name()='tblPr']/*[local-name()='tblW']")
      list(
        layout = xml2::xml_attr(layout, "type"),
        width_type = xml2::xml_attr(width, "type"),
        width = xml2::xml_attr(width, "w"),
        grid = as.numeric(xml2::xml_attr(
          xml2::xml_find_all(tbl, "./*[local-name()='tblGrid']/*[local-name()='gridCol']"),
          "w"
        )),
        first = xml2::xml_text(xml2::xml_find_first(tbl, ".//*[local-name()='tc']")),
        text_dxa = sect_width(section_of(tbl))
      )
    })
  )
}

# A DTA whose first dataset carries one rule, so {VALIDATION_RULES} and
# {DATASETS} each emit a "Rule ID" table.
.block_rules_dta <- function() {
  ds <- create_example_DTADataSetTabular(2)
  ds@specs@rules <- list(DTARuleColRange(id = "age_range_check", columns = "AGE", min = 18, max = 65))
  DTA(datasets = list(ds), metadata = create_example_DTAMetaData())
}

# Export a template built with officer and return the grids of its tables.
.block_export_grids <- function(doc, dta = .block_rules_dta()) {
  template <- tempfile(fileext = ".docx")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(c(template, out), force = TRUE), add = TRUE)
  print(doc, target = template)
  export_with_template(dta, template, out, quiet = TRUE)
  .block_table_grids(out)
}

# Every table's grid must sum to its section's text width, losing at most one
# twip per column to rounding, under a fixed layout.
.expect_block_tables_fill_section <- function(g) {
  expect_gte(length(g$tables), 1L)
  for (tb in g$tables) {
    expect_identical(tb$layout, "fixed")
    expect_lte(sum(tb$grid), tb$text_dxa)
    expect_gte(sum(tb$grid), tb$text_dxa - length(tb$grid))
  }
}

test_that("block tables use a fixed grid that fills the template's text column", {
  template <- .make_template("{DATASETS}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  export_with_template(.block_rules_dta(), template, out, quiet = TRUE)

  g <- .block_table_grids(out)
  expect_gt(length(g$tables), 1L)
  .expect_block_tables_fill_section(g)
  rules <- Filter(function(tb) identical(tb$first, "Rule ID"), g$tables)
  expect_length(rules, 1L)
  # The id column must stay readable beside the long Definition column.
  expect_gte(rules[[1]]$grid[[1]], 1440)
})

test_that("a template shaped like the clinical one sizes tables to the section the placeholder lies in", {
  # Placeholder in a first section that a paragraph-level section break ends,
  # then a continuous two-column section with other margins. The break and the
  # final section share a page size; the margins differ, so a grid measured
  # from the wrong section misses the first section's text width.
  portrait <- officer::page_size(orient = "portrait", width = 8.27, height = 11.69)
  doc <- officer::body_set_default_section(
    officer::read_docx(),
    officer::prop_section(
      page_size = portrait,
      page_margins = officer::page_mar(left = 1.5, right = 1.5),
      type = "continuous",
      section_columns = officer::section_columns(widths = c(2.4, 2.4), space = 0.5)
    )
  )
  doc <- officer::body_add_par(doc, "Intro")
  doc <- officer::body_add_par(doc, "{VALIDATION_RULES}")
  doc <- officer::body_end_block_section(doc, officer::block_section(
    officer::prop_section(
      page_size = portrait,
      page_margins = officer::page_mar(left = 0.8, right = 0.8)
    )
  ))
  doc <- officer::body_add_par(doc, "Appendix")

  g <- .block_export_grids(doc)
  .expect_block_tables_fill_section(g)
  # The tables lie in the first section (the premise of the test) ...
  expect_equal(g$tables[[1]]$text_dxa, g$first_dxa)
  # ... and the rules table keeps a readable id column.
  rules <- Filter(function(tb) identical(tb$first, "Rule ID"), g$tables)
  expect_length(rules, 1L)
  expect_gte(rules[[1]]$grid[[1]], 1440)
})

test_that("block tables in a portrait section after a landscape cover use the portrait text width", {
  doc <- officer::body_add_par(officer::read_docx(), "cover")
  doc <- officer::body_end_section_landscape(doc)
  doc <- officer::body_add_par(doc, "{VALIDATION_RULES}")

  g <- .block_export_grids(doc)
  .expect_block_tables_fill_section(g)
  # The first section is the wider, landscape one; the tables must not take
  # its width.
  expect_lt(g$tables[[1]]$text_dxa, g$first_dxa)
})

test_that("block tables in a landscape section followed by a portrait one use the landscape text width", {
  doc <- officer::body_add_par(officer::read_docx(), "{VALIDATION_RULES}")
  doc <- officer::body_end_section_landscape(doc)
  doc <- officer::body_add_par(doc, "portrait body")

  g <- .block_export_grids(doc)
  .expect_block_tables_fill_section(g)
  expect_equal(g$tables[[1]]$text_dxa, g$first_dxa)
})

test_that("block tables keep the 100% autofit layout in a section of unequal columns", {
  # officer writes explicit w:col widths with equalWidth = 0, so there is no
  # single column width to bake in.
  doc <- officer::body_set_default_section(
    officer::read_docx(),
    officer::prop_section(section_columns = officer::section_columns(widths = c(2, 3), space = 0.5))
  )
  doc <- officer::body_add_par(doc, "{VALIDATION_RULES}")

  g <- .block_export_grids(doc)
  expect_gte(length(g$tables), 1L)
  for (tb in g$tables) {
    expect_identical(tb$layout, "autofit")
    expect_identical(tb$width_type, "pct")
    expect_identical(tb$width, "5000")
  }
})

test_that("block tables in a section of equal columns fit one column", {
  doc <- officer::body_set_default_section(
    officer::read_docx(),
    officer::prop_section(section_columns = officer::section_columns(widths = c(2, 2), space = 0.5))
  )
  doc <- officer::body_add_par(doc, "{VALIDATION_RULES}")
  template <- tempfile(fileext = ".docx")
  patched <- tempfile(fileext = ".docx")
  out <- tempfile(fileext = ".docx")
  ex <- tempfile("block_cols_")
  on.exit(unlink(c(template, patched, out), force = TRUE), add = TRUE)
  on.exit(unlink(ex, recursive = TRUE, force = TRUE), add = TRUE)
  print(doc, target = template)
  # Word's own two-column section: equal columns, no w:col children.
  utils::unzip(template, exdir = ex)
  doc_xml <- file.path(ex, "word", "document.xml")
  txt <- paste(readLines(doc_xml, warn = FALSE), collapse = "")
  txt <- sub("<w:cols .*?</w:cols>", '<w:cols w:num="2" w:space="720"/>', txt, perl = TRUE)
  writeLines(txt, doc_xml, useBytes = TRUE)
  .zip_docx_dir(ex, patched)
  export_with_template(.block_rules_dta(), patched, out, quiet = TRUE)

  g <- .block_table_grids(out)
  .expect_block_tables_fill_section(g)
  # The width on offer is one of two columns: (text width - gap) / 2.
  sect <- xml2::xml_find_first(
    xml2::read_xml(.read_docx_body_xml(out)),
    ".//*[local-name()='sectPr']"
  )
  attr <- function(name, a) {
    as.numeric(xml2::xml_attr(xml2::xml_find_first(sect, paste0("./*[local-name()='", name, "']")), a))
  }
  page_text <- attr("pgSz", "w") - attr("pgMar", "left") - attr("pgMar", "right")
  expect_equal(g$tables[[1]]$text_dxa, (page_text - 720) / 2)
})

test_that("block tables follow the template's own margins", {
  ds <- create_example_DTADataSetTabular(2)
  ds@specs@rules <- list(DTARuleColRange(id = "age_range_check", columns = "AGE", min = 18, max = 65))
  dta <- DTA(datasets = list(ds), metadata = create_example_DTAMetaData())

  narrow <- officer::body_add_par(
    officer::body_set_default_section(
      officer::read_docx(),
      officer::prop_section(page_margins = officer::page_mar(left = 2, right = 2))
    ),
    "{VALIDATION_RULES}"
  )
  narrow_path <- tempfile(fileext = ".docx")
  print(narrow, target = narrow_path)
  wide_path <- .make_template("{VALIDATION_RULES}")
  on.exit(unlink(c(narrow_path, wide_path), force = TRUE), add = TRUE)

  widths <- vapply(c(narrow_path, wide_path), function(tpl) {
    out <- tempfile(fileext = ".docx")
    on.exit(unlink(out, force = TRUE), add = TRUE)
    export_with_template(dta, tpl, out, quiet = TRUE)
    g <- .block_table_grids(out)
    expect_lte(sum(g$tables[[1]]$grid), g$tables[[1]]$text_dxa)
    sum(g$tables[[1]]$grid)
  }, numeric(1))
  expect_lt(widths[[1]], widths[[2]])
})

test_that("{VALIDATION_RULES} renders each rule's translate_rule_to_human() description", {
  ds <- create_example_DTADataSetTabular(2)
  range_rule <- DTARuleColRange(id = "age_range_check", columns = "AGE", min = 18, max = 65)
  unique_rule <- DTARuleColUnique(id = "unique_subj_visit", columns = c("SUBJID", "VISIT"))
  ds@specs@rules <- list(range_rule, unique_rule)
  dta <- DTA(datasets = list(ds), metadata = create_example_DTAMetaData())

  template <- .make_template(c("BEFORE", "{VALIDATION_RULES}", "AFTER"))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  summary <- .block_docx_summary(out)
  cell_text <- .block_table_cells(summary)
  expect_true("age_range_check" %in% cell_text)
  expect_true("unique_subj_visit" %in% cell_text)

  descriptions <- .block_rule_table_descriptions(summary)
  expect_identical(unname(descriptions[["age_range_check"]]), translate_rule_to_human(range_rule))
})

test_that("a DTARuleGroupCondition renders its grouping column and constraints via translate_rule_to_human()", {
  ds <- create_example_DTADataSetTabular(2)
  rule <- DTARuleGroupCondition(
    id = "block_group_condition_example",
    group_by = "SUBJID",
    conditions = list(
      visit_v03 = list(VISIT = list(equals = "V01")),
      adult = list(AGE = list(greater_equal = 18))
    ),
    constraints = list(
      list(type = "requires", `if` = "visit_v03", `then` = "adult")
    )
  )
  ds@specs@rules <- list(rule)
  dta <- DTA(datasets = list(ds), metadata = create_example_DTAMetaData())

  template <- .make_template("{VALIDATION_RULES}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  summary <- .block_docx_summary(out)
  descriptions <- .block_rule_table_descriptions(summary)
  cell_text <- descriptions[["block_group_condition_example"]]

  expect_match(cell_text, "SUBJID", fixed = TRUE)
  expect_match(cell_text, "must hold", fixed = TRUE)
  expect_identical(unname(cell_text), translate_rule_to_human(rule))
})

test_that("{VALIDATION_RULES} and {DATASETS} carry each rule's exact definition", {
  # The Definition cell must rebuild the rule it was written from. The expected
  # value is the rule object put in, not anything the renderer produced.
  rules <- list(
    DTARuleColRange(id = "age_range_check", columns = "AGE", min = -Inf, max = 65),
    DTARuleColUnique(id = "unique_subj_visit", columns = c("SUBJID", "VISIT")),
    DTARuleColCondition(
      id = "visit_needs_age",
      description = "Baseline visits need an age",
      condition = list(VISIT = list(equals = "V01")),
      then = list(AGE = list(empty = FALSE))
    ),
    DTARuleGroupCondition(
      id = "subject_has_visit",
      group_by = "SUBJID",
      conditions = list(
        v01 = list(VISIT = list(equals = "V01")),
        old = list(AGE = list(greater_equal = 18))
      ),
      constraints = list(
        list(type = "requires", `if` = "v01", `then` = "old", then_scope = "all", message = "v01: needs adult")
      )
    )
  )
  ds <- create_example_DTADataSetTabular(2)
  ds@specs@rules <- rules
  dta <- DTA(datasets = list(ds), metadata = create_example_DTAMetaData())

  for (token in c("{VALIDATION_RULES}", "{DATASETS}")) {
    template <- .make_template(token)
    out <- tempfile(fileext = ".docx")
    export_with_template(dta, template, out, quiet = TRUE)
    unlink(template, force = TRUE)

    definitions <- .block_all_rule_definitions(.block_docx_summary(out))
    unlink(out, force = TRUE)

    expect_identical(names(definitions), vapply(rules, function(r) r@id, character(1)), info = token)
    for (rule in rules) {
      rebuilt <- do.call(
        DTARuleFactory,
        c(list(id = rule@id, description = rule@description), yaml::yaml.load(gsub("\u00a0", " ", definitions[[rule@id]], fixed = TRUE)))
      )
      expect_identical(as.list(rebuilt), as.list(rule), info = paste(token, rule@id))
    }
  }

  # The single-table helpers read the same third column.
  template <- .make_template("{VALIDATION_RULES}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  export_with_template(dta, template, out, quiet = TRUE)
  summary <- .block_docx_summary(out)
  expect_identical(names(.block_rule_table_definitions(summary)), names(.block_rule_table_descriptions(summary)))
  expect_identical(
    unname(.block_rule_table_definitions(summary)),
    unname(.block_all_rule_definitions(summary))
  )
})

test_that("dataset block headings carry the name and keep the id visible beside it", {
  # A named dataset is headed "<name> (<id>)", an unnamed one by its bare id.
  # The id stays in view because {COLUMN_SPECS:<id>} and every validation
  # message refer to the dataset by it.
  unnamed <- create_example_DTADataSetTabular(3)
  unnamed@name <- NULL
  dta <- DTA(
    datasets = list(create_example_DTADataSetTabular(2), unnamed),
    metadata = create_example_DTAMetaData()
  )
  template <- .make_template(c(
    "{COLUMN_SPECS}", "{VALIDATION_RULES}", "{FILE_SPECS}", "{DATASETS}"
  ))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  headings <- .docx_paragraphs(out)
  expect_true("Column Specifications — Demographics (demographics)" %in% headings)
  expect_true("Column Specifications — vitals" %in% headings)
  expect_true("Validation Rules — Demographics (demographics)" %in% headings)
  expect_true("Validation Rules — vitals" %in% headings)
  expect_true("Files — Demographics (demographics)" %in% headings)
  expect_true("Files — vitals" %in% headings)
  # {DATASETS} heads each dataset with the heading alone.
  expect_true("Demographics (demographics)" %in% headings)
  expect_true("vitals" %in% headings)
  expect_false(any(grepl("Vital signs", headings, fixed = TRUE)))
})

test_that("{COLUMN_SPECS:NAME} renders only the named dataset", {
  # create_example_DTA() bundles "demographics" (has AGE) and "vitals" (has
  # AVAL instead) -- verified in R/DTADataSetTabular-class.R and
  # R/DTAColumnSpecCollection-class.R.
  dta <- create_example_DTA()
  template <- .make_template("{COLUMN_SPECS:demographics}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  cell_text <- .block_table_cells(.block_docx_summary(out))
  expect_true("AGE" %in% cell_text)
  expect_false("AVAL" %in% cell_text)
})

test_that("{COLUMN_SPECS:NOPE} is left in place and warns when the dataset is unknown", {
  dta <- create_example_DTA()
  template <- .make_template("{COLUMN_SPECS:NOPE}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  suppressMessages(
    expect_warning(
      export_with_template(dta, template, out),
      "block placeholder",
      ignore.case = TRUE
    )
  )
  expect_match(.docx_text(out), "{COLUMN_SPECS:NOPE}", fixed = TRUE)
})

test_that("a block token inside a sentence is left untouched and does not become a table", {
  dta <- create_example_DTA()
  template <- .make_template("Columns: {COLUMN_SPECS}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  suppressMessages(expect_warning(
    export_with_template(dta, template, out),
    regexp = "block placeholders were left unchanged"
  ))

  expect_true("Columns: {COLUMN_SPECS}" %in% .docx_paragraphs(out))
  summary <- .block_docx_summary(out)
  expect_equal(sum(summary$content_type == "table cell"), 0)
})

test_that("a block token in a header is left untouched and never reaches the body", {
  template <- .make_template_with_header("BODY", "{COLUMN_SPECS}")
  on.exit(unlink(template, force = TRUE), add = TRUE)

  dta <- create_example_DTA()
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  ex <- tempfile("block_hdr_out_")
  dir.create(ex)
  on.exit(unlink(ex, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(out, files = "word/header1.xml", exdir = ex)
  header_xml <- paste(readLines(file.path(ex, "word", "header1.xml"), warn = FALSE), collapse = "")
  expect_match(header_xml, "{COLUMN_SPECS}", fixed = TRUE)

  summary <- .block_docx_summary(out)
  expect_equal(sum(summary$content_type == "table cell"), 0)
})

test_that("a user-supplied variables entry wins over the block renderer", {
  dta <- create_example_DTA()
  template <- .make_template(c("BEFORE", "{COLUMN_SPECS}", "AFTER"))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(
    dta, template, out,
    variables = list(COLUMN_SPECS = "plain text here"),
    quiet = TRUE
  )

  expect_true("plain text here" %in% .docx_paragraphs(out))
  summary <- .block_docx_summary(out)
  expect_equal(sum(summary$content_type == "table cell"), 0)
})

test_that("a rendered block token is never reported by the unresolved-placeholder warning", {
  dta <- create_example_DTA()
  template <- .make_template(c("{COLUMN_SPECS}", "{NOT_A_FIELD}"))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  caught <- list()
  suppressMessages(
    withCallingHandlers(
      export_with_template(dta, template, out),
      warning = function(w) {
        caught[[length(caught) + 1]] <<- w
        invokeRestart("muffleWarning")
      }
    )
  )

  expect_length(caught, 1)
  msg <- conditionMessage(caught[[1]])
  expect_match(msg, "NOT_A_FIELD", fixed = TRUE)
  expect_false(grepl("COLUMN_SPECS", msg, fixed = TRUE))
})

test_that("an inline-only template is untouched by the block-rendering pass", {
  dta <- create_example_DTA()
  template <- .make_template("{DTA_TITLE}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  expect_true(dta@metadata@title %in% .docx_paragraphs(out))
  summary <- .block_docx_summary(out)
  expect_equal(sum(summary$content_type == "table cell"), 0)
})

test_that("the bundled clinical template renders the dataset section from plain R (regression)", {
  # Today {DATASETS_DETAIL} is not in the inline catalogue, so plain R usage
  # (no template_variables, i.e. no Shiny app in the loop) leaves it printed
  # literally and the dataset section empty. {DATASETS_DETAIL} is a block
  # alias for {DATASETS}, so this must now render for real.
  dta <- read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))
  tmpl <- system.file("extdata", "templates", "clinical_dta_template.docx", package = "DTAtools")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  write_dta(dta, file = out, template = tmpl, overwrite = TRUE, quiet = TRUE)

  txt <- .docx_text(out)
  ids <- vapply(dta@datasets[[1]]@specs@columns, function(spec) spec@id, character(1))
  for (id in ids) {
    expect_match(txt, id, fixed = TRUE)
  }
  expect_false(grepl("{DATASETS_DETAIL}", txt, fixed = TRUE))
  # {YAML_EMBEDDED} is a caller-supplied inline value, not a block token, and
  # nobody supplied template_variables here -- it is expected to survive.
})

test_that("the bundled clinical template gets a fixed grid sized to its own section", {
  # The template has two sections: the placeholders sit in the first (one
  # column), a continuous two-column section holds only the appendix. Tables
  # must be sized to the first, not left at the autofit fallback.
  dta <- create_example_DTA()
  dta@datasets[[1]]@specs@rules <- list(
    DTARuleColRange(id = "age_range_check", columns = "AGE", min = 18, max = 65)
  )
  tmpl <- system.file("extdata", "templates", "clinical_dta_template.docx", package = "DTAtools")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  # {YAML_EMBEDDED} is a caller-supplied value nobody supplies here.
  expect_warning(
    export_with_template(dta, tmpl, out, quiet = TRUE),
    "YAML_EMBEDDED",
    fixed = TRUE
  )

  g <- .block_table_grids(out)
  rules <- Filter(function(tb) identical(tb$first, "Rule ID"), g$tables)
  expect_gte(length(rules), 1L)
  # The expected width is read from the exported file's own first section.
  for (tb in rules) {
    expect_identical(tb$layout, "fixed")
    expect_equal(tb$text_dxa, g$first_dxa)
    expect_lte(sum(tb$grid), g$first_dxa)
    expect_gte(sum(tb$grid), g$first_dxa - length(tb$grid))
    expect_gte(tb$grid[[1]], 1440)
  }
})

test_that("SIGNATURES_TABLE, VERSION_HISTORY_TABLE, SUPPLIER_CONTACTS_TABLE, AUTHORIZED_CORRECTIONS_LIST and FILE_SPECS each render identifiable fixture content", {
  # create_example_DTAMetaData(2) carries signatories, version history, a
  # supplier contact and authorized-for-corrections names; the clinical_data
  # dataset from the YAML fixture carries a file handler. Neither fixture
  # alone has all five, so the two are combined.
  yaml_dta <- read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))
  dta <- DTA(datasets = yaml_dta@datasets, metadata = create_example_DTAMetaData(2))

  template <- .make_template(c(
    "{SIGNATURES_TABLE}",
    "{VERSION_HISTORY_TABLE}",
    "{SUPPLIER_CONTACTS_TABLE}",
    "{AUTHORIZED_CORRECTIONS_LIST}",
    "{FILE_SPECS}"
  ))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)
  txt <- .docx_text(out)

  expect_match(txt, "Emily Turner", fixed = TRUE) # SIGNATURES_TABLE: a signatory name
  expect_match(txt, "1.5", fixed = TRUE) # VERSION_HISTORY_TABLE: a version string
  expect_match(txt, "emily.turner@supplier.com", fixed = TRUE) # SUPPLIER_CONTACTS_TABLE: a contact detail
  expect_match(txt, "Alice Smith", fixed = TRUE) # AUTHORIZED_CORRECTIONS_LIST: an authorized name
  expect_match(txt, "clinical_data.*.csv$", fixed = TRUE) # FILE_SPECS: a filename/pattern
})

test_that("dta_template_placeholders() reports both inline and block tokens with a kind attribute", {
  x <- dta_template_placeholders()
  expect_true("{COLUMN_SPECS}" %in% names(x))

  kind <- attr(x, "kind")
  expect_identical(kind[["{COLUMN_SPECS}"]], "block")
  expect_identical(kind[["{DTA_TITLE}"]], "inline")

  inline_catalog <- .tv_placeholder_catalog()
  expect_identical(x[names(inline_catalog)], inline_catalog)
})

test_that("a substituted value that spells a block token is never mistaken for a placeholder (regression)", {
  # Pass 2 used to identify block placeholders by their text in the FINISHED
  # document -- so a DTA whose title happens to be the literal string
  # "{COLUMN_SPECS}" would have its title paragraph silently replaced by a
  # table. .tv_mark_block_paragraphs() now stamps the template's own
  # paragraphs before any substitution runs, so what the *template* asked for
  # can never be confused with what a substituted *value* happens to spell.
  dta <- create_example_DTA()
  dta@metadata@title <- "{COLUMN_SPECS}"
  template <- .make_template(c("BEFORE", "{DTA_TITLE}", "AFTER"))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  summary <- .block_docx_summary(out)
  expect_equal(sum(summary$content_type == "table cell"), 0)
  expect_true("BEFORE" %in% .docx_paragraphs(out))
  expect_true("{COLUMN_SPECS}" %in% .docx_paragraphs(out))
  expect_true("AFTER" %in% .docx_paragraphs(out))
})

test_that("a one-cell table whose only content is a block token is left untouched (regression)", {
  # officer::cursor_reach() matches the text of TOP-LEVEL body children, and
  # xml2::xml_text() of a <w:tbl> concatenates every cell -- so a one-cell
  # table whose only content is the token used to match as a whole and be
  # replaced, deleting the table it lived in. .tv_mark_block_paragraphs() only
  # stamps real <w:body>/<w:p> children, never a paragraph nested inside a
  # <w:tbl>, so a token sitting in a table cell is left alone instead.
  dta <- create_example_DTA()
  template <- tempfile(fileext = ".docx")
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "BEFORE")
  doc <- officer::body_add_table(doc, data.frame(x = "{COLUMN_SPECS}"), header = FALSE)
  doc <- officer::body_add_par(doc, "AFTER")
  print(doc, target = template)
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  summary <- .block_docx_summary(out)
  cell_text <- .block_table_cells(summary)
  expect_length(cell_text, 1)
  expect_identical(cell_text, "{COLUMN_SPECS}")
  expect_false(any(grepl("STUDYID", cell_text, fixed = TRUE)))
  expect_true("BEFORE" %in% .docx_paragraphs(out))
  expect_true("AFTER" %in% .docx_paragraphs(out))
})

test_that("{COLUMN_SPECS:} with an empty dataset argument renders all datasets rather than a silent no-op", {
  # An empty `:DATASET` argument is what a deleted dataset name leaves behind
  # in a template. The grammar treats it as "no argument", i.e. every dataset
  # -- not as a token that matches nothing and is silently left in place.
  dta <- create_example_DTA()
  template <- .make_template("{COLUMN_SPECS:}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(dta, template, out, quiet = TRUE)

  cell_text <- .block_table_cells(.block_docx_summary(out))
  expect_gt(length(cell_text), 0)
  expect_true("STUDYID" %in% cell_text)
})

test_that("a template that defines no heading styles at all still exports, via the direct-formatted fallback", {
  # .tv_block_heading() exists because a user template need not define
  # "heading 2" -- but every template in this suite happens to define
  # heading 1-3, so the fallback branch (.add_bold_subheading()) never runs
  # without help. Force it by making officer report no paragraph styles at
  # all; export_with_template(fallback = TRUE) swallowing this error and
  # silently handing back the built-in layout instead of the user's template
  # is the failure mode this pins against.
  dta <- create_example_DTA()
  template <- .make_template("{COLUMN_SPECS}")
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  # Scoped to this local function (not the whole test_that()), so the mock is
  # gone again by the time .block_docx_summary() below calls officer's own
  # styles_info() to build its style_name/style_type columns.
  export_without_heading_styles <- function() {
    testthat::local_mocked_bindings(
      styles_info = function(...) {
        data.frame(style_name = character(0), style_type = character(0), stringsAsFactors = FALSE)
      },
      .package = "officer"
    )
    export_with_template(dta, template, out, quiet = TRUE)
  }
  expect_no_error(export_without_heading_styles())

  # create_example_DTA()'s first dataset is "demographics", named "Demographics":
  # the heading carries the name and keeps the id visible beside it.
  summary <- .block_docx_summary(out)
  expect_true(
    "Column Specifications \u2014 Demographics (demographics)" %in%
      summary$text[summary$content_type == "paragraph"]
  )
  expect_gt(sum(summary$content_type == "table cell"), 0)
})
