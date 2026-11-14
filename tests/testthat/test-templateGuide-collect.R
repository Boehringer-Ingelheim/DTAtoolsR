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
  expect_named(g$library, c("name", "path", "generated", "package_version", "organisation", "organisation_unit"))
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

test_that("biomarker_gf lists every column and rule of its dataset, none marked as changed", {
  e <- entry_by_id(bundled_guide()$creation, "biomarker_gf")
  expect_length(e$datasets, 1L)
  ds <- e$datasets[[1]]
  expect_identical(ds$source, "GF domain smrnaseq")
  expect_identical(ds$source_version, "3.0")
  expect_identical(nrow(ds$columns), 33L)
  expect_identical(nrow(ds$rules), 7L)
  expect_named(ds$columns, c(.tgc_col_headers, "Change"))
  expect_named(ds$rules, c("Rule ID", "Description", "Change"))
  expect_true(all(ds$columns$Change == ""))
  expect_true(all(ds$rules$Change == ""))
  expect_length(ds$removed_columns, 0L)
  expect_length(ds$removed_rules, 0L)
  expect_null(ds$same_columns)
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
  expect_named(e$prefilled, c("Field", "Value", "Question"))
  pre <- stats::setNames(e$prefilled$Value, e$prefilled$Field)
  # The value no longer carries the question; its own column names it.
  expect_identical(pre[["Document header (organisation name)"]], "Test Company 1")
  by_q <- stats::setNames(e$prefilled$Question, e$prefilled$Field)
  expect_identical(by_q[["Document header (organisation name)"]], "Header / organization")
  # A party's fields are one row.
  expect_identical(pre[["Receiver: organisation and contacts"]], "From the receiver profile you choose")
  expect_identical(pre[["Supplier: organisation and contacts"]], "From the supplier profile you choose")
  expect_false(any(grepl("Contacts|Affiliation", names(pre))))
  expect_identical(pre[["Document date"]], "(the date the DTA is created)")
  expect_identical(by_q[["Document date"]], "")
  # The first version history entry says where each part comes from.
  y <- yaml::read_yaml(file.path(bundled_library(), "templates", "biomarker_gf.dta-template.yaml"))
  vh <- pre[["Version history (first entry)"]]
  expect_match(vh, "Version: your answer to question 'Document version'", fixed = TRUE)
  expect_match(vh, "Date: the day the DTA is created", fixed = TRUE)
  expect_match(vh, paste0("Comment: ", y$base$metadata$version_history[[1]]$changes), fixed = TRUE)
})

test_that("top-level metadata keys have document names, nested paths keep the arrow", {
  expect_identical(.tgc_path_text("metadata.title"), "Document title")
  expect_identical(.tgc_path_text("metadata.version"), "Document version")
  expect_identical(.tgc_path_text("metadata.header"), "Document header (organisation name)")
  expect_identical(.tgc_path_text("metadata.date"), "Document date")
  expect_identical(.tgc_path_text("metadata.error_handling"), "Error handling section")
  expect_identical(.tgc_path_text("metadata.authorized_for_corrections"), "Authorized for corrections section")
  expect_identical(.tgc_path_text("metadata.version_history"), "Version history")
  expect_identical(.tgc_path_text("metadata.transmission.frequency"), paste0("Transmission ", right_angle, " Frequency"))
})

test_that("the inheritance chain reads nearest parent first", {
  expect_identical(.tgc_based_text("P"), "P")
  expect_identical(.tgc_based_text(c("P", "G")), "P (which is based on G)")
  expect_identical(.tgc_based_text(c("P", "G", "R")), "P (which is based on G, which is based on R)")
})

test_that("changes_status says whether the differences were compared", {
  g <- bundled_guide()
  expect_identical(entry_by_id(g$creation, "biomarker_gf_acme")$changes_status, "compared")
  expect_identical(entry_by_id(g$creation, "biomarker_gf")$changes_status, "none")
  # An abstract parent cannot be built, so nothing is compared with it.
  dir <- withr::local_tempdir()
  write_lines(dir, "abs_parent.dta-template.yaml", c(
    "kind: dta_creation_template", "id: abs_parent", "version: '1.0'",
    "label: Shared base", "abstract: true", "base:", "  metadata:", "    title: Parent title"
  ))
  write_lines(dir, "abs_child.dta-template.yaml", c(
    "kind: dta_creation_template", "id: abs_child", "version: '1.0'",
    "label: Concrete child", "extends: abs_parent@1.0",
    "datasets:", "  - id: tiny", "    name: Tiny", "    type: tabular",
    "    files: {filename: tiny.csv, type: csv}",
    "    columns:", "      - {id: AA, type: SAS Char}", "    rules: []"
  ))
  e <- entry_by_id(.template_guide_collect(dir)$creation, "abs_child")
  expect_identical(e$changes_status, "parent_abstract")
  expect_identical(nrow(e$changes), 0L)
})

test_that("a rule row keeps the author's purpose and the full condition", {
  # Wording from the specification in R/rulePhrasing.R: the purpose is its own
  # line; the statement reads "When <IF form>, <THEN form>." with typographic
  # double quotes around text values.
  lq <- "\u201c"
  rq <- "\u201d"
  e <- entry_by_id(bundled_guide()$creation, "biomarker_gf")
  rules <- e$datasets[[1]]$rules
  row <- rules[rules[["Rule ID"]] == "check_equals", ]
  expect_identical(nrow(row), 1L)
  lines <- strsplit(row$Description, "\n", fixed = TRUE)[[1]]
  expect_identical(lines[[1]], "Purpose: If STUDYID is '1234-5678', then DOMAIN must be 'GF'")
  expect_identical(lines[[2]], paste0(
    "When STUDYID is equal to ", lq, "1234-5678", rq, ", DOMAIN must be equal to ", lq, "GF", rq, "."
  ))
  # The unique rule has no author text at all: only the mechanical sentence.
  uq <- rules[rules[["Rule ID"]] == "unique_check", ]
  expect_match(
    strsplit(uq$Description, "\n", fixed = TRUE)[[1]][[1]],
    "^No two rows may have the same combination of values in columns SUBJIDN, VISIT and GFSEQID\\.$"
  )
  # The old two-line "IF ... THEN ..." form is gone from every rule row.
  for (e in bundled_guide()$creation) {
    for (d in e$datasets) expect_false(any(grepl("(^|\n)(IF|THEN) ", d$rules[["Description"]])))
  }
})

test_that("the rule lines kept for the Word table spell out the same text as the row", {
  e <- entry_by_id(bundled_guide()$creation, "biomarker_gf")
  ds <- e$datasets[[1]]
  expect_length(ds$rule_lines, nrow(ds$rules))
  for (i in seq_len(nrow(ds$rules))) {
    l <- ds$rule_lines[[i]]
    again <- ifelse(
      l$bullet, paste0("\u2022 ", l$text),
      ifelse(nzchar(l$label), trimws(paste0(l$label, ": ", l$text)), l$text)
    )
    expect_identical(paste(again, collapse = "\n"), ds$rules$Description[[i]])
  }
})

# A creation template that takes the bundled dataset template and sets the
# given rule fields; returns the rule rows of the dataset it builds.
rules_with_patch <- function(set) {
  dir <- copy_bundled_library()
  write_lines(dir, "rule_edit.dta-template.yaml", c(
    "kind: dta_creation_template", "id: rule_edit", "version: '1.0'", "label: Rule edit",
    "base:", "  metadata:", "    title: T",
    "datasets:", "  - template: gf_smrnaseq@3.0", "    as: gf",
    "    patch:", "      set:",
    paste0("        ", names(set), ": ", unlist(set))
  ))
  g <- .template_guide_collect(dir)
  expect_identical(nrow(g$problems), 0L)
  rules <- entry_by_id(g$creation, "rule_edit")$datasets[[1]]$rules
  stats::setNames(rules$Change, rules[["Rule ID"]])
}

test_that("a rule is marked Changed when only its condition or a limit differs", {
  # Regression: the change key used to be the author's description alone, so
  # a changed condition or limit under an unchanged description was missed.
  unchanged <- rules_with_patch(list(
    "rules.GFORRES must have a range.max" = "100000000"
  ))
  expect_true(all(unchanged == ""))

  # A limit of a range rule (description stays "GFORRES must be between ...").
  limit <- rules_with_patch(list("rules.GFORRES must have a range.max" = "5"))
  expect_identical(limit[["GFORRES_must_have_a_range"]], "Changed")
  expect_true(all(limit[names(limit) != "GFORRES_must_have_a_range"] == ""))

  # The requirement of a condition rule (its description stays the same).
  then <- rules_with_patch(list("rules.mutual_exclusivity_check.then.GFORRES.empty" = "false"))
  expect_identical(then[["mutual_exclusivity_check"]], "Changed")
  expect_true(all(then[names(then) != "mutual_exclusivity_check"] == ""))

  # The author's description is part of the wording as well.
  descr <- rules_with_patch(list("rules.unique_check.description" = "'Rows are unique'"))
  expect_identical(descr[["unique_check"]], "Changed")
  expect_true(all(descr[names(descr) != "unique_check"] == ""))
})

test_that("a list of allowed values is one value per line", {
  dir <- withr::local_tempdir()
  write_lines(dir, "vals.dta-template.yaml", c(
    "kind: dta_creation_template", "id: vals_tpl", "version: '1.0'", "label: Values",
    "base:", "  metadata:", "    title: T",
    "datasets:", "  - id: tiny", "    name: Tiny", "    type: tabular",
    "    files: {filename: t.csv, type: csv}",
    "    columns:", "      - {id: AA, type: SAS Char, values: [red, green, blue]}", "    rules: []"
  ))
  e <- entry_by_id(.template_guide_collect(dir)$creation, "vals_tpl")
  expect_identical(e$datasets[[1]]$columns[["Allowed values"]], "red\ngreen\nblue")
})

test_that("ACME shows its pre-filled header and marks only the columns it adds or changes", {
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
  expect_identical(nrow(ds$columns), 34L)
  id <- sub("\n.*$", "", ds$columns$Column)
  marks <- stats::setNames(ds$columns$Change, id)
  expect_identical(marks[["GFNAM"]], "Changed")
  expect_identical(marks[["ACMEID"]], "Added")
  expect_true(all(marks[!id %in% c("GFNAM", "ACMEID")] == ""))
  expect_true(all(ds$rules$Change == ""))
  gfnam <- ds$columns[startsWith(ds$columns$Column, "GFNAM"), ]
  expect_identical(gfnam[["Allowed values"]], "ACME Labs")
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
  expect_true(all(child$datasets[[1]]$columns$Change == ""))
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
  # Every question that writes to a required path must be answered, pre-filled
  # or not: the finished document is checked, and clearing the field aborts.
  expect_identical(flags[["Header"]], "Must answer")
  expect_identical(flags[["Errors"]], "Must answer")
  expect_identical(flags[["Corrections"]], "Must answer")
  expect_identical(flags[["Frequency"]], dash)
  expect_identical(e$required_count, 3L)
  expect_identical(e$questions[["Answer type"]][4], "Pick one from the list")
  expect_setequal(e$locked$Field, c("Document title", "Document header (organisation name)"))
  expect_match(e$locked$Rule[e$locked$Field == "Document title"], "^Fixed: templates based on this one cannot change it")
  expect_match(e$locked$Rule[e$locked$Field == "Document header (organisation name)"], "^Must be filled in")
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

test_that("Filled in automatically shows the pre-filled answer and the question that sets it", {
  e <- entry_by_id(bundled_guide()$creation, "biomarker_gf_acme")
  pre <- stats::setNames(e$prefilled$Value, e$prefilled$Field)
  by_q <- stats::setNames(e$prefilled$Question, e$prefilled$Field)
  hdr <- "Document header (organisation name)"
  expect_identical(pre[[hdr]], "ACME Labs")
  expect_identical(by_q[[hdr]], "Header / organization")
  expect_false(any(grepl("can be changed by", e$prefilled$Value, fixed = TRUE)))
  # Supplier and receiver are one row each, filled by choosing a party profile.
  expect_identical(sum(startsWith(names(pre), "Supplier")), 1L)
  expect_identical(pre[["Supplier: organisation and contacts"]], "From the supplier profile you choose")
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
  expect_identical(
    pre[["The supplier: organisation and contacts"]], "From the supplier profile you choose"
  )
  expect_identical(sum(grepl("upplier", names(pre))), 1L)
  expect_match(e$summary, "1 column, 0 rules", fixed = TRUE)
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
  expect_true(paste0("Document header (organisation name): H0 ", arrow, " H-small") %in% small)
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
  skip_if_no_source_tree("source tree not available")
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

tiny_dataset_lines <- c(
  "datasets:",
  "  - id: tiny", "    name: Tiny", "    type: tabular",
  "    files: {filename: t.csv, type: csv}",
  "    columns:", "      - {id: AA, type: SAS Char}", "    rules: []"
)

test_that("a creation entry carries its identity, counts, lineage and offered references", {
  g <- bundled_guide()
  yaml_of <- function(file) {
    folder <- if (grepl("vocabulary", file)) "vocabulary" else if (grepl("party", file)) "parties" else "templates"
    yaml::read_yaml(file.path(bundled_library(), folder, file))
  }
  gf <- yaml_of("biomarker_gf.dta-template.yaml")
  e <- entry_by_id(g$creation, "biomarker_gf")
  expect_false(e$abstract)
  expect_identical(e$status, "Ready to use")
  expect_identical(e$date, gf[["date"]])
  expect_identical(e$file, "biomarker_gf.dta-template.yaml")
  expect_identical(e$option_count, length(gf[["options"]]))
  expect_identical(e$required_count, 0L)
  expect_identical(nrow(e$lineage), 0L)
  expect_named(e$lineage, c("id", "version", "label"))
  expect_identical(nrow(e$mixins), 0L)

  ds <- yaml_of("gf_smrnaseq.dta-dataset-template.yaml")
  expect_identical(e$refs$dataset_templates$id, ds[["id"]])
  expect_identical(e$refs$dataset_templates$version, ds[["version"]])
  expect_identical(e$refs$dataset_templates$label, ds[["label"]])
  expect_setequal(
    e$refs$vocabularies$id,
    c(yaml_of("gf_tstdtl.dta-vocabulary.yaml")[["id"]], yaml_of("gf_test.dta-vocabulary.yaml")[["id"]])
  )
  expect_setequal(
    e$refs$parties$id,
    c(yaml_of("supplier_acme.dta-party.yaml")[["id"]], yaml_of("receiver_ourco.dta-party.yaml")[["id"]])
  )
  expect_identical(e$refs$parties$version[e$refs$parties$id == "supplier_acme"], "1.0")
  expect_identical(e$refs$parties$label[e$refs$parties$id == "supplier_acme"], "ACME Labs")

  acme <- entry_by_id(g$creation, "biomarker_gf_acme")
  expect_identical(acme$lineage$id, gf[["id"]])
  expect_identical(acme$lineage$version, gf[["version"]])
  expect_identical(acme$lineage$label, gf[["label"]])
})

test_that("a dataset template's term lists are offered, and a narrowed term list is not a change", {
  dir <- copy_bundled_library()
  write_lines(dir, "narrow.dta-template.yaml", c(
    "kind: dta_creation_template", "id: narrow", "version: '1.0'", "label: Narrow",
    "base:", "  metadata:", "    title: T",
    "datasets:", "  - template: gf_smrnaseq@3.0", "    as: gf_data_specs_pattern",
    "vocabulary_slots:",
    "  - id: detail", "    label: Detail",
    "    target: datasets.gf_data_specs_pattern.columns.GFTSTDTL.values",
    "    vocabulary: gf_tstdtl@1.0", "    mode: closed",
    "    default: [FRAGMENT COUNT]", "    min: 1"
  ))
  g <- .template_guide_collect(dir)
  expect_identical(nrow(g$problems), 0L)
  e <- entry_by_id(g$creation, "narrow")
  ds <- e$datasets[[1]]
  detail <- ds$columns[startsWith(ds$columns$Column, "GFTSTDTL"), ]
  expect_identical(detail[["Allowed values"]], "FRAGMENT COUNT")
  expect_true(all(ds$columns$Change == ""))
  expect_true("gf_test" %in% e$refs$vocabularies$id)
  expect_identical(nrow(e$refs$parties), 0L)
})

test_that("the library carries the organisation from dta-app.yaml, and empty strings without", {
  expect_identical(bundled_guide()$library$organisation, "")
  expect_identical(bundled_guide()$library$organisation_unit, "")

  dir <- copy_bundled_library()
  write_lines(dir, "dta-app.yaml", c(
    "kind: dta_app_config", "organisation:", "  name: Example Pharma", "  unit: Biometrics"
  ))
  lib <- .template_guide_collect(dir)$library
  expect_identical(lib$organisation, "Example Pharma")
  expect_identical(lib$organisation_unit, "Biometrics")

  # A config the app cannot read never stops the guide.
  bad <- copy_bundled_library()
  write_lines(bad, "dta-app.yaml", c("organisation: [unclosed"))
  lib <- .template_guide_collect(bad)$library
  expect_identical(lib$organisation, "")
  expect_identical(lib$organisation_unit, "")
})

test_that("stub entries carry every new field with empty defaults", {
  row <- data.frame(id = "x", version = "1.0", label = "X", path = "x.dta-template.yaml")
  for (stub in list(.tgc_stub, .tgc_creation_stub)) {
    s <- stub(row, list(def = list(date = "2026-01-02")), "boom")
    expect_false(s$abstract)
    expect_identical(s$status, "")
    expect_identical(s$date, "2026-01-02")
    expect_identical(s$file, "x.dta-template.yaml")
    expect_identical(s$option_count, 0L)
    expect_identical(s$required_count, 0L)
    for (f in list(s$lineage, s$mixins, s$refs$dataset_templates, s$refs$vocabularies, s$refs$parties)) {
      expect_identical(nrow(f), 0L)
      expect_named(f, c("id", "version", "label"))
    }
  }
  expect_identical(.tgc_creation_stub(row, list(def = NULL), "boom")$date, "")
})

test_that(".tgc_select_ids picks the newest version of every non-abstract template", {
  dir <- withr::local_tempdir()
  tpl <- function(id, version, extra = character(0)) {
    c(
      "kind: dta_creation_template", sprintf("id: %s", id), sprintf("version: '%s'", version),
      sprintf("label: %s", id), extra, "base:", "  metadata:", "    title: T", tiny_dataset_lines
    )
  }
  write_lines(dir, "a9.dta-template.yaml", tpl("ver_tpl", "1.9"))
  write_lines(dir, "a10.dta-template.yaml", tpl("ver_tpl", "1.10"))
  write_lines(dir, "other.dta-template.yaml", tpl("other_tpl", "1.0"))
  write_lines(dir, "abs.dta-template.yaml", c(
    "kind: dta_creation_template", "id: abs_tpl", "version: '1.0'", "label: Abs", "abstract: true",
    "base:", "  metadata:", "    title: T"
  ))
  write_lines(dir, "child.dta-template.yaml", tpl("child_tpl", "1.0", "extends: abs_tpl@1.0"))
  g <- .template_guide_collect(dir)

  sel <- .tgc_select_ids(g)
  expect_setequal(names(sel), c("ver_tpl", "other_tpl", "child_tpl"))
  expect_identical(g$creation[[sel[["ver_tpl"]]]]$version, "1.10")
  expect_identical(g$creation[[sel[["other_tpl"]]]]$id, "other_tpl")

  one <- .tgc_select_ids(g, "ver_tpl")
  expect_identical(names(one), "ver_tpl")
  expect_identical(g$creation[[one[["ver_tpl"]]]]$version, "1.10")

  err <- expect_error(.tgc_select_ids(g, "abs_tpl"), class = "dta_template_guide_abstract")
  expect_match(conditionMessage(err), "child_tpl", fixed = TRUE)
  expect_error(.tgc_select_ids(g, "no_such"), class = "dta_template_guide_unknown")
})
