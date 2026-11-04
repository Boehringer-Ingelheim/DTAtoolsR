# Synthetic agreement metadata contract, independent of private templates.
agreement_sections <- function() {
  list(
    scope = list(title = "Scope {draft}", text = "First line\n\nSecond line\n"),
    Handling_2 = list(title = "Handling", text = "  Keep indentation\nLast line"),
    draft = list(title = "Draft", text = "")
  )
}

agreement_template <- function() {
  list(
    kind = "dta_creation_template", id = "agreement", version = "2.0",
    label = "Synthetic agreement",
    base = list(metadata = list(
      title = "New agreement", version = "1.0",
      study = list(id = "BASE", protocol_id = "BASE-P"),
      sections = agreement_sections()
    )),
    datasets = list(list(
      id = "deliverable", type = "file",
      files = list(filename = "deliverable.pdf", type = "any")
    )),
    options = list()
  )
}

test_that("empty agreement fields leave legacy serialization and positional API unchanged", {
  md <- DTAMetaData()
  expect_identical(md@study, list())
  expect_identical(md@sections, list())
  expect_identical(as.list(md), stats::setNames(list(), character()))
  expect_identical(
    as.list(DTAMetaData(title = "Legacy", version = "1.0", header = "Organization")),
    list(title = "Legacy", version = "1.0", header = "Organization")
  )
  expect_identical(DTAMetaData(header = "Not a study ID")@study, list())

  old_args <- list(
    "Legacy", "1.0", as.Date("2026-01-01"), "Organization",
    list(list(version = "0.9", date = "2025-12-01", changes = "Draft")),
    list(name = "Receiver"), list(name = "Supplier"), list(type = "incremental"),
    "Handle errors", "Reviewer", list(id = "source", version = "1.0")
  )
  old_names <- c(
    "title", "version", "date", "header", "version_history", "receiver",
    "supplier", "transmission", "error_handling", "authorized_for_corrections",
    "template"
  )
  expect_identical(names(formals(DTAMetaData)), c(old_names, "study", "sections"))
  positional <- do.call(DTAMetaData, old_args)
  named <- do.call(DTAMetaData, stats::setNames(old_args, old_names))
  expect_identical(as.list(positional), as.list(named))
  extended <- do.call(DTAMetaData, c(old_args, list(list(id = "ST-001"), agreement_sections())))
  expect_identical(extended@template, positional@template)
  expect_identical(extended@study, list(id = "ST-001"))
  expect_identical(extended@sections, agreement_sections())
})

test_that("study accepts only optional named nonblank scalar identity fields", {
  for (study in list(list(id = "ST-001"), list(protocol_id = "P-001"), list(protocol_id = " P ", id = " S "))) {
    expect_identical(DTAMetaData(study = study)@study, study)
  }
  bad_shapes <- list(
    NULL, "ST-001", c(id = "ST-001"), list("ST-001"),
    list(id = "S", "P"), list(other = "S"), list(id = "S", id = "T"),
    stats::setNames(list("S"), NA_character_),
    stats::setNames(list("S"), ""),
    data.frame(id = "S"), matrix(list("S"), nrow = 1)
  )
  for (study in bad_shapes) {
    expect_error(DTAMetaData(study = study), "study")
  }
  bad_values <- list(NULL, NA_character_, NA, "", " \t\n", character(), c("S", "T"), 1, TRUE, list("S"), matrix("S"))
  for (field in c("id", "protocol_id")) {
    for (value in bad_values) {
      expect_error(
        DTAMetaData(study = stats::setNames(list(value), field)),
        paste0("study.*", field)
      )
    }
  }
})

test_that("sections require unique stable names and exactly title and text", {
  sections <- agreement_sections()
  expect_identical(DTAMetaData(sections = sections)@sections, sections)
  expect_identical(DTAMetaData(sections = list(a = list(text = " ", title = "A")))@sections$a$text, " ")
  record <- list(title = "Scope", text = "Text")
  for (sections in list(NULL, "text", list(record), c(title = "T", text = "X"), matrix(list(record)))) {
    expect_error(DTAMetaData(sections = sections), "sections")
  }
  for (name in c("", NA_character_, "1scope", "_scope", "scope.text", "scope-text", "scope text", "scope\n")) {
    expect_error(DTAMetaData(sections = stats::setNames(list(record), name)), "sections")
  }
  expect_error(DTAMetaData(sections = list(scope = record, scope = record)), "sections")
  bad_records <- list(
    NULL, "text", list(), list(title = "T"), list(text = "X"),
    list(title = "T", text = "X", extra = "Y"),
    list(title = "T", title = "X", text = "Y"),
    list("T", "X"), stats::setNames(list("T", "X"), c("title", NA_character_)),
    data.frame(title = "T", text = "X")
  )
  for (record in bad_records) {
    expect_error(DTAMetaData(sections = list(scope = record)), "sections.*scope")
  }
  bad_values <- list(NULL, NA_character_, NA, character(), c("a", "b"), 1, TRUE, list("a"), matrix("a"))
  for (field in c("title", "text")) {
    for (value in c(bad_values, if (field == "title") list("", " \t\n") else list())) {
      record <- list(title = "T", text = "X")
      record[field] <- list(value)
      expect_error(DTAMetaData(sections = list(scope = record)), paste0("sections.*scope.*", field))
    }
  }
})

test_that("new transmission text is validated without tightening legacy extensions", {
  fields <- c("extent", "scope", "packaging", "security", "checksum", "test_requirements", "blinding_requirements")
  legacy <- list(
    type = list(extension = TRUE), flags = list(full = TRUE),
    vendor_extension = list(nested = list(value = 42)),
    date_first_transfer = "after approval"
  )
  for (value in c("Text\nNext line", "", " \t\n")) {
    transmission <- c(legacy, stats::setNames(rep(list(value), length(fields)), fields))
    expect_identical(DTAMetaData(transmission = transmission)@transmission, transmission)
    expect_identical(as.list(DTAMetaData(transmission = transmission))$transmission, transmission)
  }
  for (field in fields) {
    for (value in list(NULL, NA_character_, NA, character(), c("a", "b"), 1, TRUE, list("a"), matrix("a"))) {
      expect_error(
        DTAMetaData(transmission = c(legacy, stats::setNames(list(value), field))),
        paste0("transmission.*", field)
      )
    }
    # Every supplied occurrence is validated, not only the first matching name.
    expect_error(
      DTAMetaData(transmission = stats::setNames(list("valid", NA_character_), rep(field, 2))),
      paste0("transmission.*", field)
    )
  }
})

test_that("agreement fields validate on S7 mutation and preserve the original after errors", {
  md <- DTAMetaData(study = list(id = "S"), sections = agreement_sections())
  original <- as.list(md)
  expect_error(md@study <- list(id = ""), "study.*id")
  expect_error(md@sections$scope$text <- NA_character_, "sections.*scope.*text")
  expect_error(md@sections$scope$extra <- "Unknown", "sections.*scope")
  expect_error(md@transmission <- list(security = FALSE), "transmission.*security")
  expect_identical(as.list(md), original)
  md@study$protocol_id <- "P"
  md@sections$scope$text <- "Revised\nverbatim"
  md@transmission <- list(security = "")
  expect_identical(md@study, list(id = "S", protocol_id = "P"))
  expect_identical(md@sections$scope$text, "Revised\nverbatim")
  expect_identical(names(md@sections), names(agreement_sections()))
  md@study <- list()
  md@sections <- list()
  expect_false(any(c("study", "sections") %in% names(as.list(md))))
})

test_that("package and app YAML roundtrips preserve section order and literal text", {
  md <- DTAMetaData(
    title = "Synthetic", version = "1.0", header = "Separate organization",
    study = list(protocol_id = "P-001", id = "ST-001"),
    sections = agreement_sections(),
    transmission = list(scope = "", packaging = "One\nTwo")
  )
  raw <- yaml::yaml.load(yaml::as.yaml(as.list(md)))
  expect_identical(as.list(do.call(DTAMetaData, raw)), as.list(md))
  path <- withr::local_tempfile(fileext = ".yaml")
  yaml::write_yaml(list(metadata = as.list(md), datasets = list()), path)
  from_file <- read_dta_from_yaml(path)
  expect_identical(from_file@metadata@study, md@study)
  expect_identical(from_file@metadata@sections, md@sections)
  expect_identical(names(from_file@metadata@sections), c("scope", "Handling_2", "draft"))
  saved <- app_fn("dta_to_yaml_text")(from_file)
  expect_true(saved$ok)
  loaded <- app_fn("dta_read_yaml_text")(saved$value)
  expect_true(loaded$ok)
  expect_identical(as.list(loaded$value@metadata), as.list(md))
})

test_that("YAML rejects malformed study and section mappings", {
  path <- withr::local_tempfile(fileext = ".yaml")
  malformed <- c(
    "study:\n    - S",
    "study:\n    id: S\n    id: T",
    "study:\n    id: 1",
    "sections:\n    - title: T\n      text: X",
    "sections:\n    scope: {title: T, text: X}\n    scope: {title: U, text: Y}",
    "sections:\n    scope: {title: T}",
    "sections:\n    scope: {title: T, text: null}"
  )
  for (block in malformed) {
    writeLines(paste0("metadata:\n  title: Synthetic\n  version: '1.0'\n  ", block, "\ndatasets: []\n"), path)
    expect_error(read_dta_from_yaml(path))
  }
})

test_that("metadata console summary includes study identity and ordered sections safely", {
  md <- DTAMetaData(study = list(id = "S-{001}", protocol_id = "P-001"), sections = agreement_sections())
  output <- testthat::capture_messages(print_info(md))
  flat <- paste(output, collapse = "\n")
  expect_match(flat, "S-{001}", fixed = TRUE)
  expect_match(flat, "P-001", fixed = TRUE)
  expect_match(flat, "Scope {draft}", fixed = TRUE)
  positions <- vapply(c("Scope {draft}", "Handling", "Draft"), function(x) regexpr(x, flat, fixed = TRUE)[1], integer(1))
  expect_true(all(positions > 0))
  expect_true(all(diff(positions) > 0))
})

test_that("template reflection and existing dot paths expose agreement fields but not provenance", {
  expect_true(all(c("study", "sections") %in% app_fn("dta_template_metadata_fields")()))
  expect_true(all(c("study", "sections") %in% app_fn("dta_template_list_fields")()))
  expect_false(any(c("template", "import_issues") %in% app_fn("dta_template_metadata_fields")()))
  dta <- DTA(datasets = list(), metadata = DTAMetaData(study = list(id = "S"), sections = agreement_sections()))
  changed <- app_fn("apply_template_metadata_path")(dta, "metadata.sections.scope.text", "Override\nverbatim")
  expect_identical(changed@metadata@sections$scope$text, "Override\nverbatim")
  expect_identical(names(changed@metadata@sections), names(dta@metadata@sections))
  expect_identical(dta@metadata@sections, agreement_sections())
  expect_identical(app_fn("dta_template_document_path_get")(changed, "metadata.sections.scope.text"), "Override\nverbatim")
  changed <- app_fn("apply_template_metadata_path")(changed, "metadata.study.protocol_id", "P")
  expect_identical(changed@metadata@study, list(id = "S", protocol_id = "P"))
  expect_error(app_fn("apply_template_metadata_path")(changed, "metadata.sections.scope.text", NA_character_), "sections.*text")
  expect_error(app_fn("apply_template_metadata_path")(changed, "metadata.template.id", "forged"), "Unsupported")
})

test_that("blank optional study path effects remove identifiers without weakening validation", {
  dta <- DTA(datasets = list(), metadata = DTAMetaData(study = list(id = "S", protocol_id = "P")))
  apply <- app_fn("apply_template_metadata_path")
  for (blank in c("", " \t\n")) {
    changed <- apply(dta, "metadata.study.id", blank)
    expect_identical(changed@metadata@study, list(protocol_id = "P"))
    changed <- apply(changed, "metadata.study.protocol_id", blank)
    expect_length(changed@metadata@study, 0)
    expect_false("study" %in% names(as.list(changed@metadata)))
  }
  expect_identical(dta@metadata@study, list(id = "S", protocol_id = "P"))
  expect_error(apply(dta, "metadata.study.unknown", ""), "study.*unknown")
  expect_error(apply(dta, "metadata.study.id", NA_character_), "study.*id")
  expect_error(apply(dta, "metadata.study", list(id = "")), "study.*id")
  expect_error(DTAMetaData(study = list(id = "")), "study.*id")

  def <- agreement_template()
  def$options <- list(
    list(id = "study_id", type = "text", target = "metadata.study.id", default = ""),
    list(id = "protocol_id", type = "text", target = "metadata.study.protocol_id", default = "")
  )
  result <- app_fn("create_dta_from_template")(def, tempfile(fileext = ".yaml"))
  expect_true(result$ok, info = result$error)
  expect_length(result$value@metadata@study, 0)
  expect_false("study" %in% names(as.list(result$value@metadata)))
  result <- app_fn("create_dta_from_template")(
    def, tempfile(fileext = ".yaml"), selections = list(study_id = "Selected")
  )
  expect_true(result$ok, info = result$error)
  expect_identical(result$value@metadata@study, list(id = "Selected"))
})

test_that("study metadata does not modify dataset constraints or previous validation", {
  dta <- app_fixture_dta_with_data(checked = TRUE)
  datasets <- dta@datasets
  md <- dta@metadata
  md@study <- list(id = "Independent study identity", protocol_id = "P-001")
  md@sections <- agreement_sections()
  dta@metadata <- md
  expect_identical(dta@datasets, datasets)
})

test_that("inherited sections and text options use ordinary named-list merging", {
  parent <- agreement_template()
  parent$id <- "parent"
  parent$abstract <- TRUE
  parent$options <- list(list(id = "scope_text", type = "text", target = "metadata.sections.scope.text"))
  child <- list(
    kind = "dta_creation_template", id = "child", version = "1.0",
    extends = "parent", abstract = FALSE,
    base = list(metadata = list(
      study = list(protocol_id = "CHILD-P"),
      sections = list(
        scope = list(text = "Child clause\nSecond line"),
        extra = list(title = "Additional", text = "Child addition")
      )
    ))
  )
  resolved <- app_fn("resolve_template_inheritance")(child, function(ref) {
    list(def = parent, id = parent$id, version = parent$version)
  })
  def <- resolved$def
  expect_identical(def$base$metadata$study, list(id = "BASE", protocol_id = "CHILD-P"))
  expect_identical(names(def$base$metadata$sections), c(names(agreement_sections()), "extra"))
  expect_identical(def$base$metadata$sections$scope$title, "Scope {draft}")
  expect_identical(
    app_fn("dta_template_default")(def$options[[1]], def$base$metadata),
    "Child clause\nSecond line"
  )
  built <- app_fn("create_dta_from_template")(def, tempfile(fileext = ".yaml"))
  expect_true(built$ok, info = built$error)
  expect_identical(built$value@metadata@sections, def$base$metadata$sections)
  selected <- app_fn("create_dta_from_template")(
    def, tempfile(fileext = ".yaml"),
    selections = list(scope_text = "Author clause\n\nFinal line")
  )
  expect_true(selected$ok, info = selected$error)
  expect_identical(selected$value@metadata@sections$scope$text, "Author clause\n\nFinal line")
  expect_identical(selected$value@metadata@sections$Handling_2, agreement_sections()$Handling_2)
  invalid <- def
  invalid$base$metadata$study <- list(unknown = "S")
  failed <- app_fn("create_dta_from_template")(invalid, tempfile(fileext = ".yaml"))
  expect_false(failed$ok)
  expect_match(failed$error, "study")
})

test_that("carry-over defaults retain study but leave revised template sections in place", {
  defaults <- app_fn("carry_over_default_fields")()
  expect_true("study" %in% defaults)
  expect_false("sections" %in% defaults)
  expect_false(any(c("template", "import_issues", "title", "version", "date", "version_history") %in% defaults))
  source_md <- DTAMetaData(
    study = list(id = "ORIGINAL", protocol_id = "ORIGINAL-P"),
    sections = list(scope = list(title = "Old scope", text = "Old clause"), old = list(title = "Old", text = "")),
    template = list(id = "old_template", version = "1.0")
  )
  def <- agreement_template()
  build <- function(fields, selections = list(), template = def, source = source_md) {
    app_fn("create_dta_from_template")(
      template, tempfile(fileext = ".yaml"), selections = selections,
      carry_over = list(metadata = source, fields = fields),
      provenance = list(id = "new_template", version = "2.0")
    )
  }
  default <- build(defaults)
  expect_true(default$ok, info = default$error)
  expect_identical(default$value@metadata@study, source_md@study)
  expect_identical(default$value@metadata@sections, def$base$metadata$sections)
  explicit <- build(c(defaults, "sections", "template", "import_issues"))
  expect_true(explicit$ok, info = explicit$error)
  expect_identical(explicit$value@metadata@sections, source_md@sections)
  expect_identical(explicit$value@metadata@template, list(id = "new_template", version = "2.0"))

  def$options <- list(
    list(id = "study_id", type = "text", target = "metadata.study.id"),
    list(id = "scope_text", type = "text", target = "metadata.sections.scope.text")
  )
  selected <- build(c(defaults, "sections"), list(study_id = "CHOSEN", scope_text = "Chosen clause"))
  expect_true(selected$ok, info = selected$error)
  expect_identical(selected$value@metadata@study, list(id = "CHOSEN", protocol_id = "ORIGINAL-P"))
  expect_identical(selected$value@metadata@sections$scope, list(title = "Old scope", text = "Chosen clause"))
  expect_identical(names(selected$value@metadata@sections), c("scope", "old"))
  expect_identical(source_md@sections$scope$text, "Old clause")
  empty <- build(c(defaults, "sections"), source = DTAMetaData())
  expect_true(empty$ok, info = empty$error)
  expect_identical(empty$value@metadata@sections, def$base$metadata$sections)
})
