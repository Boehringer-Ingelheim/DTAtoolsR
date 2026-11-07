# The COLLECT half of write_template_guide(): .template_guide_collect() turns a
# template library into plain data frames. Expected values here are read
# straight from the YAML (or spelled out), never from the collector.

bundled_library <- function() {
  system.file("extdata", "library", package = "DTAtools")
}

# The bundled library is read once per test file; building every template is
# the slow part.
guide_cache <- new.env()
bundled_guide <- function() {
  if (is.null(guide_cache$guide)) {
    guide_cache$guide <- .template_guide_collect(bundled_library())
  }
  guide_cache$guide
}

entry_by_id <- function(entries, id) {
  hit <- Filter(function(e) identical(e$id, id), entries)
  expect_length(hit, 1L)
  hit[[1]]
}

# A scratch library ROOT holding a copy of the bundled library's kind folders.
copy_bundled_library <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  file.copy(
    file.path(bundled_library(), c("templates", "parties", "vocabulary")),
    dir,
    recursive = TRUE
  )
  dir
}

# The kind folder of a library root that holds `name`, by its kind suffix; a
# name with no kind suffix lands directly in the root.
kind_folder_of <- function(name) {
  if (grepl("[.]dta-(dataset-)?template[.]ya?ml$", name)) {
    "templates"
  } else if (grepl("[.]dta-party[.]ya?ml$", name)) {
    "parties"
  } else if (grepl("[.]dta-vocabulary[.]ya?ml$", name)) {
    "vocabulary"
  } else {
    ""
  }
}

# Writes `lines` as `name` into its kind folder under library root `dir`.
write_lines <- function(dir, name, lines) {
  folder <- file.path(dir, kind_folder_of(name))
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(folder, name)
  writeLines(lines, path)
  path
}

right_angle <- intToUtf8(0x203a)
arrow <- intToUtf8(0x2192)
dash <- intToUtf8(0x2014)

test_that("the guide has the documented shape, with a data frame for every table", {
  g <- bundled_guide()
  expect_named(g, c("library", "overview", "creation", "datasets", "vocabularies", "parties", "problems"))
  expect_named(g$library, c("name", "path", "generated", "package_version"))
  expect_match(g$library$generated, "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
  expect_named(g$overview, c("Name", "Kind", "ID", "Version", "What it is"))
  expect_named(g$problems, c("File", "Problem"))
  expect_identical(nrow(g$problems), 0L)
  expect_setequal(
    g$overview$ID,
    c(
      "biomarker_gf", "biomarker_gf_acme", "gf_smrnaseq", "gf_test", "gf_tstdtl",
      "supplier_acme", "receiver_ourco"
    )
  )
  expect_identical(
    g$overview$Kind[g$overview$ID == "gf_test"], "Term list"
  )
  expect_identical(
    g$overview$Kind[g$overview$ID == "biomarker_gf"], "DTA template"
  )
  expect_true(all(vapply(g$creation[[1]][c("changes", "questions", "variants", "parties", "term_choices", "prefilled", "locked")], is.data.frame, logical(1))))
})

test_that("biomarker_gf lists every option of the YAML as a question", {
  e <- entry_by_id(bundled_guide()$creation, "biomarker_gf")
  yaml_def <- yaml::read_yaml(file.path(bundled_library(), "templates", "biomarker_gf.dta-template.yaml"))
  labels <- vapply(yaml_def[["options"]], function(o) o[["label"]], character(1))
  expect_setequal(e$questions$Question, labels)
  expect_identical(nrow(e$questions), 12L)
  expect_match(e$summary, "Asks 12 questions", fixed = TRUE)
  expect_match(e$summary, "1 dataset", fixed = TRUE)
  expect_match(e$summary, "33 columns", fixed = TRUE)
  expect_match(e$summary, "Supplier", fixed = TRUE)

  q <- function(label) e$questions[e$questions$Question == label, ]
  freq <- q("Transfer frequency")
  expect_identical(freq[["Answer type"]], "Free text, with suggestions")
  expect_identical(freq[["Appears in the DTA as"]], paste0("Transmission ", right_angle, " Frequency"))
  expect_identical(freq[["Choices"]], "one-time\nweekly\nmonthly\nupon request")
  expect_identical(freq[["Pre-filled answer"]], "one-time")
  expect_identical(freq[["Explanation"]], "How often data is transferred.")

  upload <- q("Test upload required?")
  expect_identical(upload[["Answer type"]], "Yes / No")
  expect_identical(upload[["Pre-filled answer"]], "Yes")
  expect_identical(upload[["Choices"]], dash)
  expect_identical(q("Error handling")[["Answer type"]], "Longer free text, with suggestions")
  expect_identical(q("Document version")[["Answer type"]], "Free text")
})

test_that("biomarker_gf shows its dataset once and does not repeat the dataset template", {
  e <- entry_by_id(bundled_guide()$creation, "biomarker_gf")
  expect_length(e$datasets, 1L)
  ds <- e$datasets[[1]]
  expect_identical(ds$source, "GF domain smrnaseq")
  expect_identical(nrow(ds$columns), 0L)
  expect_match(ds$same_columns, "All 33 columns are exactly as in the dataset template 'GF domain smrnaseq'", fixed = TRUE)
  expect_match(ds$same_rules, "All 7 rules", fixed = TRUE)
  about <- stats::setNames(ds$about$value, ds$about$key)
  expect_identical(about[["Columns"]], "33")
  expect_identical(about[["File type"]], "TSV")
  expect_identical(about[["File name"]], "gf_data_small_smirna.tsv")
  expect_identical(e$at_a_glance$value[e$at_a_glance$key == "Status"], "Ready to use")
  expect_identical(e$at_a_glance$value[e$at_a_glance$key == "Based on"], dash)
  expect_identical(e$parent, "")
  expect_identical(nrow(e$changes), 0L)
})

test_that("the party and term-list choices and the pre-filled fields are listed", {
  e <- entry_by_id(bundled_guide()$creation, "biomarker_gf")
  expect_setequal(e$parties$Party, c("Supplier", "Receiver"))
  sup <- e$parties[e$parties$Party == "Supplier", ]
  expect_identical(sup[["Available profiles"]], "ACME Labs")
  expect_identical(sup[["Must choose"]], "No")
  expect_identical(nrow(e$term_choices), 1L)
  tc <- e$term_choices
  expect_identical(tc[["Term list"]], "Genomic measurement detail")
  expect_identical(tc[["How it works"]], "Pick from the list only")
  expect_identical(tc[["Selected by default"]], "FRAGMENT COUNT\nNORMALIZED LEVEL")
  expect_identical(tc[["Minimum to select"]], "1")
  pre <- stats::setNames(e$prefilled$Value, e$prefilled$Field)
  expect_identical(pre[["Header"]], "Test Company 1\n(can be changed by question 'Header / organization')")
  expect_identical(pre[[paste0("Receiver ", right_angle, " Contacts")]], "(chosen when the DTA is created: Receiver)")
  expect_identical(pre[["Date"]], "(the date the DTA is created)")
  expect_match(pre[["Version history 1"]], "Initial template-generated version", fixed = TRUE)
})

test_that("ACME shows its pre-filled header and only the columns it adds or changes", {
  e <- entry_by_id(bundled_guide()$creation, "biomarker_gf_acme")
  header <- e$questions[e$questions$Question == "Header / organization", ]
  expect_identical(header[["Pre-filled answer"]], "ACME Labs")
  expect_identical(e$parent, "Biomarker GF DTS")
  expect_identical(e$at_a_glance$value[e$at_a_glance$key == "Based on"], "Biomarker GF DTS")

  expect_match(paste(e$changes$What, collapse = "|"), "ACMEID", fixed = TRUE)
  expect_match(paste(e$changes$What, collapse = "|"), "GFNAM", fixed = TRUE)
  row <- e$changes[e$changes$What == "Question: Header / organization", ]
  expect_identical(row$Change, paste0("Pre-filled answer: Test Company 1 ", arrow, " ACME Labs"))
  added <- e$changes[grepl("ACMEID", e$changes$What), ]
  expect_identical(added$Change, "Added")

  ds <- e$datasets[[1]]
  expect_setequal(sub("\n.*$", "", ds$columns$Column), c("GFNAM", "ACMEID"))
  gfnam <- ds$columns[startsWith(ds$columns$Column, "GFNAM"), ]
  expect_identical(gfnam[["Allowed values"]], "ACME Labs")
  expect_match(ds$same_columns, "All other 32 columns", fixed = TRUE)
  expect_identical(e$at_a_glance$key[1:2], c("Status", "Based on"))
})

test_that("a dataset template lists settings, columns, pattern and term-list provenance", {
  d <- entry_by_id(bundled_guide()$datasets, "gf_smrnaseq")
  expect_identical(nrow(d$columns), 33L)
  detail <- d$columns[startsWith(d$columns$Column, "GFTSTDTL"), ]
  expect_match(detail[["Allowed values"]], "FRAGMENT COUNT", fixed = TRUE)
  expect_match(detail[["Allowed values"]], "(from term list 'Genomic measurement detail')", fixed = TRUE)
  subj <- d$columns[startsWith(d$columns$Column, "SUBJIDN"), ]
  expect_match(subj[["Allowed values"]], "Must match pattern ^[0-9]{10}$", fixed = TRUE)
  expect_identical(subj[["Type"]], "SAS Num\n10.")
  expect_identical(subj[["Can be empty"]], "No")
  expect_identical(d$settings[["Setting"]], "Vendor name")
  expect_identical(d$settings[["Appears in the DTA as"]], "Allowed values of column GFNAM")
  expect_identical(d$settings[["Pre-filled answer"]], "ExternalSupplierName")
  expect_identical(nrow(d$rules), 7L)
  used <- d$at_a_glance$value[d$at_a_glance$key == "Used by"]
  expect_match(used, "Biomarker GF DTS (ACME Labs)", fixed = TRUE)
})

test_that("term lists show their terms and party profiles their contacts", {
  g <- bundled_guide()
  v <- entry_by_id(g$vocabularies, "gf_tstdtl")
  expect_identical(v$terms$Code, c("FRAGMENT COUNT", "NORMALIZED LEVEL"))
  expect_identical(v$terms$Label, c("Fragment count", "Normalized level"))
  used <- stats::setNames(v$at_a_glance$value, v$at_a_glance$key)
  expect_identical(used[["Used for columns"]], "GFTSTDTL")
  expect_match(used[["Used by"]], "GF domain smrnaseq", fixed = TRUE)

  p <- entry_by_id(g$parties, "supplier_acme")
  expect_identical(p$at_a_glance$value[p$at_a_glance$key == "Role"], "Supplier")
  expect_identical(p$at_a_glance$value[p$at_a_glance$key == "Organisation"], "ACME Labs")
  jane <- p$contacts[p$contacts$Name == "Jane Doe", ]
  expect_identical(jane$Signs, "Yes")
  expect_identical(jane$Email, "jane.doe@example.com")
  john <- p$contacts[p$contacts$Name == "John Roe", ]
  expect_identical(john$Signs, dash)
  expect_identical(john$Phone, dash)
})

test_that("a vocabulary that extends another shows the inherited terms", {
  dir <- copy_bundled_library()
  write_lines(dir, "gf_tstdtl_plus.dta-vocabulary.yaml", c(
    "kind: dta_vocabulary", "id: gf_tstdtl_plus", "version: '1.0'",
    "label: Detail plus", "extends: gf_tstdtl@1.0",
    "add_terms:", "  - code: RPKM", "    label: Reads per kilobase"
  ))
  g <- .template_guide_collect(dir)
  v <- entry_by_id(g$vocabularies, "gf_tstdtl_plus")
  expect_identical(v$terms$Code, c("FRAGMENT COUNT", "NORMALIZED LEVEL", "RPKM"))
  expect_identical(v$at_a_glance$value[v$at_a_glance$key == "Based on"], "Genomic measurement detail")
})

test_that("an abstract template is a building block with no datasets and no problem", {
  dir <- withr::local_tempdir()
  write_lines(dir, "abs_parent.dta-template.yaml", c(
    "kind: dta_creation_template", "id: abs_parent", "version: '1.0'",
    "label: Shared base", "abstract: true",
    "base:", "  metadata:", "    title: Parent title",
    "options:", "  - id: title", "    label: Title", "    type: text", "    target: metadata.title"
  ))
  write_lines(dir, "abs_child.dta-template.yaml", c(
    "kind: dta_creation_template", "id: abs_child", "version: '1.0'",
    "label: Concrete child", "extends: abs_parent@1.0",
    "datasets:",
    "  - id: tiny", "    name: Tiny table", "    type: tabular",
    "    files: {filename: tiny.csv, type: csv}",
    "    columns:", "      - {id: AA, label: First, type: SAS Char, format: SAS $5., nullable: No}",
    "    rules: []"
  ))
  g <- .template_guide_collect(dir)
  expect_identical(nrow(g$problems), 0L)
  parent <- entry_by_id(g$creation, "abs_parent")
  expect_match(parent$summary, "Building block: cannot be used on its own", fixed = TRUE)
  expect_match(parent$at_a_glance$value[parent$at_a_glance$key == "Status"], "^Building block")
  expect_length(parent$datasets, 0L)
  expect_identical(parent$build_note, "")
  expect_identical(nrow(parent$questions), 1L)

  child <- entry_by_id(g$creation, "abs_child")
  expect_identical(child$at_a_glance$value[child$at_a_glance$key == "Status"], "Ready to use")
  expect_identical(child$parent, "Shared base")
  expect_identical(child$questions$Question, "Title")
  expect_identical(child$questions[["Pre-filled answer"]], "Parent title")
  cols <- child$datasets[[1]]$columns
  expect_identical(cols$Column, "AA\nFirst")
  expect_identical(cols$Type, "SAS Char\n$5.")
  expect_identical(cols[["Allowed values"]], "Any")
  expect_identical(child$datasets[[1]]$same_columns, "")
})

test_that("a broken file becomes a problem row and never aborts the guide", {
  dir <- copy_bundled_library()
  write_lines(dir, "broken.dta-template.yaml", c("kind: [unclosed", "id: broken"))
  g <- .template_guide_collect(dir)
  expect_identical(g$problems$File, "broken.dta-template.yaml")
  expect_true(nzchar(g$problems$Problem))
  expect_length(g$creation, 2L)
})

test_that("a template whose sample DTA cannot be built keeps its chapter and notes it", {
  dir <- copy_bundled_library()
  write_lines(dir, "nodata.dta-template.yaml", c(
    "kind: dta_creation_template", "id: nodata", "version: '1.0'", "label: Missing dataset",
    "base:", "  metadata:", "    title: T",
    "datasets:", "  - template: no_such_dataset@1.0"
  ))
  g <- .template_guide_collect(dir)
  e <- entry_by_id(g$creation, "nodata")
  expect_match(e$build_note, "^A sample DTA could not be built from this template:")
  expect_length(e$datasets, 0L)
  expect_identical(g$problems$File, "nodata.dta-template.yaml")
})

test_that("`templates` keeps the chosen templates and what they reference", {
  dir <- copy_bundled_library()
  write_lines(dir, "other.dta-template.yaml", c(
    "kind: dta_creation_template", "id: other_tpl", "version: '1.0'", "label: Unrelated",
    "base:", "  metadata:", "    title: T",
    "datasets:",
    "  - id: tiny", "    name: Tiny", "    type: tabular",
    "    files: {filename: t.csv, type: csv}",
    "    columns:", "      - {id: AA, type: SAS Char}", "    rules: []"
  ))
  all_ids <- .template_guide_collect(dir)$overview$ID
  expect_true("other_tpl" %in% all_ids)

  g <- .template_guide_collect(dir, templates = "biomarker_gf_acme")
  expect_setequal(
    g$overview$ID,
    c(
      "biomarker_gf_acme", "biomarker_gf", "gf_smrnaseq", "gf_test", "gf_tstdtl",
      "supplier_acme", "receiver_ourco"
    )
  )
  expect_identical(nrow(g$problems), 0L)

  only <- .template_guide_collect(dir, templates = "other_tpl")
  expect_identical(only$overview$ID, "other_tpl")
  expect_length(only$parties, 0L)
})

test_that("a single template file means its directory restricted to that id and its references", {
  dir <- copy_bundled_library()
  g <- .template_guide_collect(file.path(dir, "templates", "biomarker_gf_acme.dta-template.yaml"))
  expect_true(all(c("biomarker_gf_acme", "biomarker_gf", "gf_smrnaseq") %in% g$overview$ID))
  expect_identical(normalizePath(g$library$path, "/"), normalizePath(dir, "/"))
})

test_that("a library root is read from its kind folders, one chapter list across all of them", {
  root <- copy_bundled_library()
  # Folders the guide does not read must not disturb it.
  dir.create(file.path(root, "export-templates"))
  writeLines("x", file.path(root, "export-templates", "README.md"))
  writeLines("kind: dta_app_config", file.path(root, "dta-app.yaml"))

  g <- .template_guide_collect(root)
  expect_identical(nrow(g$overview), 7L)
  expect_identical(nrow(g$problems), 0L)
  expect_setequal(g$overview$Kind, c("DTA template", "Dataset template", "Term list", "Party profile"))
  by_kind <- split(g$overview$ID, g$overview$Kind)
  expect_setequal(by_kind[["DTA template"]], c("biomarker_gf", "biomarker_gf_acme"))
  expect_setequal(by_kind[["Dataset template"]], "gf_smrnaseq")
  expect_setequal(by_kind[["Term list"]], c("gf_test", "gf_tstdtl"))
  expect_setequal(by_kind[["Party profile"]], c("supplier_acme", "receiver_ourco"))
  expect_identical(normalizePath(g$library$path, "/"), normalizePath(root, "/"))
})

test_that("a flat directory of template files is not a library", {
  flat <- withr::local_tempdir()
  src <- copy_bundled_library()
  file.copy(
    list.files(src, pattern = "[.]ya?ml$", recursive = TRUE, full.names = TRUE),
    flat
  )
  # The message says what a library is (cli wraps it, so match across whitespace).
  expect_error(
    .template_guide_collect(flat),
    class = "dta_template_guide_no_templates",
    regexp = "templates/,\\s+parties/\\s+and\\s+vocabulary/\\s+folders;\\s+pass\\s+the\\s+library\\s+root"
  )
})

test_that("a kind folder given instead of the library root finds no templates", {
  root <- copy_bundled_library()
  for (sub in c("templates", "parties", "vocabulary")) {
    expect_error(
      .template_guide_collect(file.path(root, sub)),
      class = "dta_template_guide_no_templates", info = sub
    )
  }
})

test_that("a single template file is located through its kind folder; the library root is its parent", {
  root <- copy_bundled_library()
  party <- file.path(root, "parties", "supplier_acme.dta-party.yaml")
  g <- .template_guide_collect(party)
  expect_identical(g$overview$ID, "supplier_acme")
  expect_identical(normalizePath(g$library$path, "/"), normalizePath(root, "/"))

  vocab <- file.path(root, "vocabulary", "gf_test.dta-vocabulary.yaml")
  expect_identical(.template_guide_collect(vocab)$overview$ID, "gf_test")
})

test_that("a single template file outside the folder of its kind is refused with a classed error", {
  root <- copy_bundled_library()
  src <- file.path(root, "templates", "biomarker_gf.dta-template.yaml")

  # in the library root
  at_root <- file.path(root, "biomarker_gf.dta-template.yaml")
  file.copy(src, at_root)
  expect_error(.template_guide_collect(at_root), class = "dta_template_guide_no_templates")

  # in another kind's folder
  in_parties <- file.path(root, "parties", "biomarker_gf.dta-template.yaml")
  file.copy(src, in_parties)
  expect_error(.template_guide_collect(in_parties), class = "dta_template_guide_no_templates")

  # below its own folder
  dir.create(file.path(root, "templates", "archive"))
  nested <- file.path(root, "templates", "archive", "biomarker_gf.dta-template.yaml")
  file.copy(src, nested)
  expect_error(.template_guide_collect(nested), class = "dta_template_guide_no_templates")

  # a file with no kind suffix at all
  plain <- file.path(root, "templates", "biomarker_gf.yaml")
  file.copy(src, plain)
  expect_error(.template_guide_collect(plain), class = "dta_template_guide_no_templates")
})

test_that("a template with variants is built once per choice", {
  dir <- withr::local_tempdir()
  dataset <- function(col, type) {
    c(
      "        - id: fa", "          name: Adjudication findings", "          type: tabular",
      "          files: {filename: fa.csv, type: csv}",
      "          columns:", sprintf("            - {id: %s, type: %s}", col, type),
      "          rules: []"
    )
  }
  write_lines(dir, "adjudication.dta-template.yaml", c(
    "kind: dta_creation_template", "id: adjudication", "version: '1.0'", "label: Adjudication",
    "base:", "  metadata:", "    title: Adjudication", "    version: '1.0'",
    "variants:", "  id: trial_platform", "  label: Trial platform", "  choices:",
    "    - value: brave", "      label: BRAVE", "      datasets:", dataset("SUBJIDN", "SAS Num"),
    "    - value: veeva", "      label: Veeva", "      datasets:", dataset("SUBJECT", "SAS Char")
  ))
  g <- .template_guide_collect(dir)
  e <- entry_by_id(g$creation, "adjudication")
  expect_identical(nrow(g$problems), 0L)
  expect_identical(e$variants$Question, c("Trial platform", "Trial platform"))
  expect_identical(e$variants$Choice, c("BRAVE", "Veeva"))
  expect_length(e$datasets, 2L)
  expect_identical(
    vapply(e$datasets, function(d) d$variant, character(1)),
    c("For choice 'BRAVE'", "For choice 'Veeva'")
  )
  expect_identical(
    vapply(e$datasets, function(d) sub("\n.*$", "", d$columns$Column), character(1)),
    c("SUBJIDN", "SUBJECT")
  )
  expect_match(e$summary, "depending on Trial platform (2 choices)", fixed = TRUE)
})

test_that("required and fixed paths mark questions and fill the fixed table", {
  dir <- withr::local_tempdir()
  write_lines(dir, "locked.dta-template.yaml", c(
    "kind: dta_creation_template", "id: locked_tpl", "version: '1.0'", "label: Locked one",
    "base:", "  metadata:", "    title: Fixed title", "    error_handling: Already there",
    "sealed: [base.metadata.title]",
    "required: [base.metadata.header, base.metadata.error_handling, base.metadata.authorized_for_corrections]",
    "options:",
    "  - id: header", "    label: Header", "    type: text", "    target: metadata.header",
    "  - id: errors", "    label: Errors", "    type: text", "    target: metadata.error_handling",
    "  - id: auth", "    label: Corrections", "    type: text",
    "    target: metadata.authorized_for_corrections", "    default: Trial Data Manager",
    "  - id: freq", "    label: Frequency", "    type: select",
    "    target: metadata.transmission.frequency", "    choices: [weekly, monthly]",
    "datasets:",
    "  - id: tiny", "    name: Tiny", "    type: tabular",
    "    files: {filename: t.csv, type: csv}",
    "    columns:", "      - {id: AA, type: SAS Char}", "    rules: []"
  ))
  e <- entry_by_id(.template_guide_collect(dir)$creation, "locked_tpl")
  flags <- stats::setNames(e$questions[["Must answer / fixed"]], e$questions$Question)
  # Header has neither a pre-filled answer nor a base value: the only one to answer.
  expect_identical(flags[["Header"]], "Must answer")
  expect_identical(flags[["Errors"]], dash)
  expect_identical(flags[["Corrections"]], dash)
  expect_identical(flags[["Frequency"]], dash)
  expect_identical(e$questions[["Answer type"]][4], "Pick one from the list")
  expect_setequal(e$locked$Field, c("Title", "Header"))
  expect_match(e$locked$Rule[e$locked$Field == "Title"], "^Fixed: templates based on this one cannot change it")
  expect_match(e$locked$Rule[e$locked$Field == "Header"], "^Must be filled in")
  expect_false(any(grepl("ocked", c(names(e$questions), e$locked$Rule, e$questions[["Must answer / fixed"]]))))
})

test_that("a question is Fixed when its target is sealed", {
  dir <- withr::local_tempdir()
  write_lines(dir, "fixed.dta-template.yaml", c(
    "kind: dta_creation_template", "id: fixed_tpl", "version: '1.0'", "label: Fixed one",
    "base:", "  metadata:", "    title: T",
    "sealed: [base.metadata.title]",
    "options:", "  - id: title", "    label: Title", "    type: text", "    target: metadata.title",
    "datasets:",
    "  - id: tiny", "    name: Tiny", "    type: tabular",
    "    files: {filename: t.csv, type: csv}",
    "    columns:", "      - {id: AA, type: SAS Char}", "    rules: []"
  ))
  e <- entry_by_id(.template_guide_collect(dir)$creation, "fixed_tpl")
  expect_identical(e$questions[["Must answer / fixed"]], "Fixed")
})

test_that("every answer-type branch mirrors the app's input", {
  g <- .dta_template_engine_get
  row <- function(opt, base_md = NULL) .tgc_question_row(opt, base_md, g)
  cases <- list(
    list(list(type = "boolean"), "Yes / No", "No"),
    list(list(type = "boolean", default = TRUE), "Yes / No", "Yes"),
    list(list(type = "boolean", default = "yes"), "Yes / No", "Yes"),
    list(list(type = "number"), "Number", "0"),
    list(list(type = "number", default = 7), "Number", "7"),
    list(list(type = "textarea"), "Longer free text", dash),
    list(list(type = "textarea", default = "Hi"), "Longer free text", "Hi"),
    list(list(type = "textarea", choices = c("a", "b")), "Longer free text, with suggestions", dash),
    list(list(type = "select", choices = c("a", "b"), default = "b"), "Pick one from the list", "b"),
    list(list(type = "select", choices = c("a", "b")), "Pick one from the list", dash),
    list(list(type = "text", choices = c("a", "b"), allow_custom = FALSE), "Pick one from the list", dash),
    list(list(type = "select"), "Free text", dash),
    list(list(type = "select", choices = c("a", "b"), allow_custom = TRUE), "Free text, with suggestions", dash),
    list(list(type = "select", choices = "a", allow_custom = TRUE), "Free text", "a"),
    list(list(type = "text", choices = "a"), "Free text", "a"),
    list(list(type = "text", choices = "a", default = "z"), "Free text", "z"),
    list(list(type = "text", choices = c("a", "b")), "Free text, with suggestions", dash),
    list(list(type = "text", choices = c("a", "b"), default = "b"), "Free text, with suggestions", "b"),
    list(list(type = "text"), "Free text", dash),
    list(list(type = "text", default = "x"), "Free text", "x")
  )
  for (cs in cases) {
    r <- row(c(list(id = "q", label = "Q"), cs[[1]]))
    info <- paste(deparse(cs[[1]]), collapse = " ")
    expect_identical(r[[2]], cs[[2]], info = info)
    expect_identical(r[[4]], cs[[3]], info = info)
  }
  # A strict list shows the choice's label; a suggestion field shows the stored text.
  labelled <- list(
    list(value = "w", label = "Weekly"), list(value = "m", label = "Monthly")
  )
  strict <- row(list(id = "q", label = "Q", type = "select", choices = labelled, default = "w"))
  expect_identical(strict[[4]], "Weekly")
  loose <- row(list(id = "q", label = "Q", type = "text", choices = labelled, default = "w"))
  expect_identical(loose[[4]], "w")
  # An omitted default is inherited from the base metadata.
  inherited <- row(
    list(id = "q", label = "Q", type = "text", target = "metadata.header"),
    list(header = "Base header")
  )
  expect_identical(inherited[[4]], "Base header")
})

test_that("Filled in automatically shows the pre-filled answer, marked as changeable", {
  e <- entry_by_id(bundled_guide()$creation, "biomarker_gf_acme")
  pre <- stats::setNames(e$prefilled$Value, e$prefilled$Field)
  expect_true(startsWith(pre[["Header"]], "ACME Labs"))
  expect_false(grepl("Test Company 1", pre[["Header"]], fixed = TRUE))
  expect_match(pre[["Header"]], "\n(can be changed by question 'Header / organization')", fixed = TRUE)
  # Supplier and receiver are filled by choosing a party profile.
  sup <- pre[startsWith(names(pre), "Supplier")]
  expect_gt(length(sup), 0L)
  expect_true(all(startsWith(sup, "(chosen when the DTA is created: Supplier")))
})

test_that("a field a term-list choice or party fills is not shown with the base value", {
  dir <- withr::local_tempdir()
  write_lines(dir, "slots.dta-template.yaml", c(
    "kind: dta_creation_template", "id: slots_tpl", "version: '1.0'", "label: Slots",
    "base:", "  metadata:", "    title: T",
    "    supplier:", "      affiliation:", "        name: Base Supplier",
    "party_slots:",
    "  - id: sup", "    label: The supplier", "    target: metadata.supplier", "    role: supplier",
    "datasets:",
    "  - id: tiny", "    name: Tiny", "    type: tabular",
    "    files: {filename: t.csv, type: csv}",
    "    columns:", "      - {id: AA, type: SAS Char}", "    rules: []"
  ))
  e <- entry_by_id(.template_guide_collect(dir)$creation, "slots_tpl")
  pre <- stats::setNames(e$prefilled$Value, e$prefilled$Field)
  hit <- pre[startsWith(names(pre), "Supplier")]
  expect_gt(length(hit), 0L)
  expect_identical(unname(hit), rep("(chosen when the DTA is created: The supplier)", length(hit)))
  expect_false(any(grepl("Base Supplier", e$prefilled$Value, fixed = TRUE)))
})

test_that("Used by is computed from the whole library even when `templates` narrows the guide", {
  dir <- copy_bundled_library()
  g <- .template_guide_collect(dir, templates = "gf_smrnaseq")
  d <- entry_by_id(g$datasets, "gf_smrnaseq")
  used <- d$at_a_glance$value[d$at_a_glance$key == "Used by"]
  expect_match(used, "Biomarker GF DTS (ACME Labs)", fixed = TRUE)
  expect_match(used, "Biomarker GF DTS", fixed = TRUE)
  expect_match(d$summary, "Used by 2 DTA templates", fixed = TRUE)
  v <- entry_by_id(g$vocabularies, "gf_tstdtl")
  expect_match(v$at_a_glance$value[v$at_a_glance$key == "Used by"], "GF domain smrnaseq", fixed = TRUE)
})

test_that("each variant choice lists the datasets, questions and answers it changes", {
  dir <- withr::local_tempdir()
  dataset <- function(id, name) {
    c(
      sprintf("        - id: %s", id), sprintf("          name: %s", name), "          type: tabular",
      sprintf("          files: {filename: %s.csv, type: csv}", id),
      "          columns:", "            - {id: AA, type: SAS Char}", "          rules: []"
    )
  }
  write_lines(dir, "var.dta-template.yaml", c(
    "kind: dta_creation_template", "id: var_tpl", "version: '1.0'", "label: Var",
    "base:", "  metadata:", "    title: T", "    header: H0", "    error_handling: E0",
    "options:",
    "  - id: header", "    label: Header", "    type: text", "    target: metadata.header",
    "  - id: errors", "    label: Errors", "    type: text", "    target: metadata.error_handling",
    "variants:", "  id: size", "  label: Study size", "  choices:",
    "    - value: small", "      label: Small",
    "      base: {metadata: {header: H-small}}",
    "      options:",
    "        - {id: extra, label: Extra, type: text, target: metadata.authorized_for_corrections}",
    "        - {id: errors, remove: true}",
    "      datasets:", dataset("sm", "Small table"),
    "    - value: large", "      label: Large",
    "      datasets:", dataset("lg", "Large table"), dataset("lg2", "Second large table")
  ))
  g <- .template_guide_collect(dir)
  expect_identical(nrow(g$problems), 0L)
  e <- entry_by_id(g$creation, "var_tpl")
  # The Questions table is the template without any choice applied.
  expect_setequal(e$questions$Question, c("Header", "Errors"))
  expect_identical(e$questions[["Pre-filled answer"]], c("H0", "E0"))
  cell <- stats::setNames(e$variants[["What it changes"]], e$variants$Choice)
  small <- strsplit(cell[["Small"]], "\n", fixed = TRUE)[[1]]
  expect_true("Creates dataset: 'Small table'" %in% small)
  expect_true("Adds question 'Extra'" %in% small)
  expect_true("Removes question 'Errors'" %in% small)
  expect_true(paste0("Header: H0 ", arrow, " H-small") %in% small)
  large <- strsplit(cell[["Large"]], "\n", fixed = TRUE)[[1]]
  expect_identical(large, "Creates datasets: 'Large table', 'Second large table'")
  expect_match(e$summary, "Some answers to 'Study size' change the questions; see Variants.", fixed = TRUE)
})

test_that("a party slot that cannot be read becomes a problem row", {
  dir <- withr::local_tempdir()
  write_lines(dir, "badslot.dta-template.yaml", c(
    "kind: dta_creation_template", "id: badslot", "version: '1.0'", "label: Bad slot",
    "base:", "  metadata:", "    title: T",
    "party_slots:", "  - id: sup", "    label: Supplier", "    target: metadata.nonsense",
    "datasets:",
    "  - id: tiny", "    name: Tiny", "    type: tabular",
    "    files: {filename: t.csv, type: csv}",
    "    columns:", "      - {id: AA, type: SAS Char}", "    rules: []"
  ))
  g <- .template_guide_collect(dir)
  expect_true("badslot.dta-template.yaml" %in% g$problems$File)
  expect_true(any(grepl("party", g$problems$Problem, ignore.case = TRUE)))
})

test_that("the collector aborts with cli conditions, never stop()", {
  path <- file.path(testthat::test_path("..", ".."), "R", "templateGuide-collect.R")
  skip_if_not(file.exists(path), "source tree not available")
  code <- readLines(path, warn = FALSE)
  code <- code[!grepl("^\\s*#", code)]
  expect_false(any(grepl("\\bstop\\(", code)))
})

test_that("a file-name pattern without a description is shown by its file name", {
  dir <- withr::local_tempdir()
  write_lines(dir, "pat.dta-template.yaml", c(
    "kind: dta_creation_template", "id: pat_tpl", "version: '1.0'", "label: Pattern",
    "base:", "  metadata:", "    title: T",
    "datasets:",
    "  - id: tiny", "    name: Tiny", "    type: tabular",
    "    files: {filename: 'x_[0-9]+[.]csv', type: csv, pattern: true}",
    "    columns:", "      - {id: AA, type: SAS Char}", "    rules: []"
  ))
  e <- entry_by_id(.template_guide_collect(dir)$creation, "pat_tpl")
  about <- stats::setNames(e$datasets[[1]]$about$value, e$datasets[[1]]$about$key)
  expect_identical(about[["File name pattern"]], "x_[0-9]+[.]csv")
})

test_that("bad input is refused with a classed error", {
  expect_error(
    .template_guide_collect(file.path(withr::local_tempdir(), "nope")),
    class = "dta_template_guide_no_templates"
  )
  empty <- withr::local_tempdir()
  expect_error(.template_guide_collect(empty), class = "dta_template_guide_no_templates")
  expect_error(.template_guide_collect(NA_character_), class = "dta_template_guide_no_templates")
  expect_error(
    .template_guide_collect(bundled_library(), templates = "no_such_template"),
    class = "dta_template_guide_unknown"
  )
})
