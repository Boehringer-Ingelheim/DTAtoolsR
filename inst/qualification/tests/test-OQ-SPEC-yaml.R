# Reading and writing a specification. REQ-SPEC-001 .. REQ-SPEC-006,
# REQ-SPEC-014 .. REQ-SPEC-017 and REQ-SPEC-025.
#
# The specification is the agreement between the two parties to a transfer.
# Everything the package later decides follows from how it was read, so a
# specification misread produces a check against rules nobody agreed to -- and
# that check looks entirely successful while it does it.

sy_example <- function(...) {
  path <- system.file("extdata", ..., package = "DTAtools")
  if (!nzchar(path)) {
    testthat::skip(paste0("bundled example not installed: ", paste(..., collapse = "/")))
  }
  path
}

test_that("OQ-SPEC-001 | the shipped specification reads into a complete transfer object | REQ-SPEC-001", {
  dta <- read_dta_from_yaml(sy_example("clinical_dta.yaml"))

  qa_check("a transfer object is built", inherits(dta, "DTAtools::DTA"))
  qa_step(
    "it carries the dataset the specification names",
    "clinical_data", names(datasets(dta))
  )

  ds <- dta[["clinical_data"]]
  qa_check("the dataset is tabular", inherits(ds, "DTAtools::DTADataSetTabular"))
  # The three descriptive fields are separate: the id keys the dataset, the
  # name is the label a reader sees, and the description is the long text.
  lead <- "Subject-level clinical observations for the study,"
  qa_step(
    "it carries the id, the label and the long description the specification gives",
    list(id = "clinical_data", name = "Clinical data table", description_starts = lead),
    list(id = ds@id, name = ds@name, description_starts = substr(ds@description, 1, nchar(lead)))
  )
  qa_check("it declares at least one file handler", length(files(ds)) >= 1)
  qa_check("it declares column specifications", length(columns(ds)) > 0)
  qa_check("it declares rules", length(rules(specs(ds))) > 0)
  qa_check("and the transfer carries its metadata", inherits(metadata(dta), "DTAtools::DTAMetaData"))
})

test_that("OQ-SPEC-002 | a single-dataset specification reads on its own | REQ-SPEC-002", {
  ds <- read_dataset_from_yaml(sy_example("gf_dataset.yaml"))

  qa_check("a dataset is built", inherits(ds, "DTAtools::DTADataSet"))
  qa_step("with the id the specification gives", "gf_data_specs_pattern", ds@id)
  qa_step("and the label the specification gives", "Genomic Findings data table", ds@name)
  qa_check("and with column specifications", length(names(specs(ds))) > 0)
})

test_that("OQ-SPEC-003 | a column specification file reads into a collection | REQ-SPEC-003", {
  # A columns-rooted file, which is how a shared column dictionary is written
  # for reuse across transfers. It is a different document from the transfer
  # specification, and this reader takes only the former.
  dir <- qa_tempdir()
  path <- file.path(dir, "columns.yaml")
  writeLines(
    c(
      "columns:",
      "  SUBJID:",
      "    id: SUBJID",
      "    type: SAS Char",
      "    length: 8",
      "    nullable: false",
      "  AGE:",
      "    id: AGE",
      "    type: SAS Num",
      "    nullable: true"
    ),
    path
  )
  collection <- import_specs_from_yaml(path)

  qa_check("a collection is built", inherits(collection, "DTAtools::DTAColumnSpecCollection"))
  qa_step("carrying the columns the file names", c("SUBJID", "AGE"), names(collection))
  qa_step(
    "each column is keyed by its own identifier",
    TRUE,
    all(vapply(
      names(collection),
      function(id) identical(colspec(collection, id)@id, id),
      logical(1)
    ))
  )
  qa_step(
    "and each keeps its declared nullability",
    c(SUBJID = FALSE, AGE = TRUE),
    c(
      SUBJID = colspec(collection, "SUBJID")@nullable,
      AGE = colspec(collection, "AGE")@nullable
    )
  )

  # A transfer specification is not a column dictionary, and the reader says so
  # rather than returning an empty collection that would check nothing.
  transfer <- sy_example("clinical_dta.yaml")
  qa_check(
    "a transfer specification is rejected by this reader",
    inherits(tryCatch(import_specs_from_yaml(transfer), error = function(e) e), "condition")
  )
})

test_that("OQ-SPEC-004 | the parsed form of a specification builds the same object | REQ-SPEC-004", {
  path <- sy_example("clinical_dta.yaml")
  from_file <- read_dta_from_yaml(path)
  from_list <- dta_from_list(yaml::read_yaml(path))

  # The YAML reader parses and then delegates to the list constructor. If the
  # two disagreed, the file and its parsed form would describe different
  # agreements, and which one applied would depend on how it was loaded.
  qa_step(
    "the same datasets are built either way",
    names(datasets(from_file)), names(datasets(from_list))
  )
  qa_step(
    "with the same columns",
    names(columns(from_file[["clinical_data"]])),
    names(columns(from_list[["clinical_data"]]))
  )
  qa_step(
    "and the same rules",
    vapply(rules(specs(from_file[["clinical_data"]])), function(r) r@id, character(1)),
    vapply(rules(specs(from_list[["clinical_data"]])), function(r) r@id, character(1))
  )
})

test_that("OQ-SPEC-005 | an unreadable specification raises rather than half-loading | REQ-SPEC-005", {
  dir <- qa_tempdir()

  # A file that is not YAML at all.
  broken <- file.path(dir, "broken.yaml")
  writeLines(c("datasets: [", "  this is not: valid: yaml:"), broken)
  qa_check(
    "malformed YAML raises a condition",
    inherits(tryCatch(read_dta_from_yaml(broken), error = function(e) e), "condition")
  )

  # A file that parses but describes nothing the package can use. Yielding a
  # partial object here would be the dangerous outcome: the checks it did
  # describe would run and report success, and the ones it lost would never be
  # missed.
  empty <- file.path(dir, "empty.yaml")
  writeLines("something_else: true", empty)
  result <- tryCatch(read_dta_from_yaml(empty), error = function(e) e)
  qa_check(
    "a specification naming no datasets does not yield a usable transfer object",
    inherits(result, "condition") ||
      length(datasets(result)) == 0
  )

  missing <- file.path(dir, "does-not-exist.yaml")
  qa_check(
    "a path that does not exist raises a condition",
    inherits(tryCatch(read_dta_from_yaml(missing), error = function(e) e), "condition")
  )
})

test_that("OQ-SPEC-006 | a code written with leading zeros stays text | REQ-SPEC-006", {
  dir <- qa_tempdir()
  path <- file.path(dir, "codes.yaml")

  # YAML would read a bare 007 as the number seven. A specification that lost
  # the leading zeros would then permit "7" and reject "007", which is the
  # opposite of what was written -- and every delivery using the real code
  # would fail for a reason nobody could see in the specification.
  writeLines(
    c(
      "columns:",
      "  CODE:",
      "    id: CODE",
      "    type: SAS Char",
      "    length: 3",
      "    nullable: false",
      "    values:",
      '      - "007"',
      '      - "010"'
    ),
    path
  )
  collection <- import_specs_from_yaml(path)

  qa_step(
    "the permitted values keep the text they were written with",
    c("007", "010"),
    as.character(colspec(collection, "CODE")@values)
  )
})

test_that("OQ-SPEC-020 | a specification survives a round trip through YAML | REQ-SPEC-014", {
  dir <- qa_tempdir()
  original <- DTAColumnSpecCollection(
    columns = list(
      SUBJID = DTAColumnSpec(
        id = "SUBJID", type = "SAS Char", length = 8, nullable = FALSE,
        pattern = "^S[0-9]{7}$"
      ),
      SEX = DTAColumnSpec(
        id = "SEX", type = "SAS Char", length = 1, nullable = FALSE,
        values = c("M", "F")
      ),
      CODE = DTAColumnSpec(
        id = "CODE", type = "SAS Char", length = 3, nullable = TRUE,
        values = c("007", "010")
      ),
      AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    )
  )

  path <- file.path(dir, "roundtrip.yaml")
  write_columns_to_yaml(original, file = path)
  restored <- import_specs_from_yaml(path)

  qa_step(
    "the same columns come back",
    names(original), names(restored)
  )
  for (id in names(original)) {
    before <- colspec(original, id)
    after <- colspec(restored, id)
    qa_step(
      sprintf("column %s keeps its declared shape", id),
      list(
        nullable = before@nullable,
        values = as.character(before@values %||% character(0)),
        pattern = as.character(before@pattern %||% NA_character_)
      ),
      list(
        nullable = after@nullable,
        values = as.character(after@values %||% character(0)),
        pattern = as.character(after@pattern %||% NA_character_)
      )
    )
  }

  # The leading-zero case again, this time through the writer: a specification
  # that loses it on the way out describes a different column than the one
  # agreed.
  qa_step(
    "and a code with leading zeros survives being written and read",
    c("007", "010"), as.character(colspec(restored, "CODE")@values)
  )
})

test_that("OQ-SPEC-021 | a specification written to JSON parses and describes the same columns | REQ-SPEC-015", {
  dir <- qa_tempdir()
  collection <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
      AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    )
  )
  path <- file.path(dir, "columns.json")
  write_columns_to_json(collection, file = path)

  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  qa_check("the file parses as JSON", is.list(parsed))
  qa_step(
    "and names the same columns",
    TRUE,
    all(vapply(
      c("ID", "AGE"),
      function(id) any(grepl(id, paste(unlist(parsed), collapse = " "), fixed = TRUE)),
      logical(1)
    ))
  )
})

test_that("OQ-SPEC-022 | a column renders as a JSON Schema fragment | REQ-SPEC-016 REQ-SPEC-017", {
  bounded <- DTAColumnSpec(
    id = "ID", type = "SAS Char", length = 8, nullable = FALSE,
    pattern = "^S[0-9]{7}$"
  )
  schema <- as_json_schema(bounded)

  qa_check("a schema fragment is produced", is.list(schema))
  qa_step(
    "it names the type, the maximum length and the pattern",
    c("type", "maxLength", "pattern"), names(schema)
  )
  # The two contributors return the bare values the fragment is assembled from,
  # not fragments of their own.
  qa_step(
    "the length contributor yields the declared length",
    8, as.numeric(as_json_schema_length(bounded))
  )
  qa_step(
    "and the declared length reaches the fragment",
    8, as.numeric(schema$maxLength)
  )

  # Nullability decides whether an absent value is a violation, so it has to
  # reach the schema. A non-nullable column that admitted null would accept
  # every gap in the delivery.
  qa_step(
    "a non-nullable column is typed as a plain string",
    "string", as.character(unlist(as_json_schema_type(bounded)))
  )
  nullable <- DTAColumnSpec(id = "OPT", type = "SAS Char", length = 8, nullable = TRUE)
  qa_step(
    "while a nullable one does",
    TRUE, "null" %in% unlist(as_json_schema_type(nullable))
  )

  coded <- DTAColumnSpec(
    id = "SEX", type = "SAS Char", length = 1, nullable = FALSE,
    values = c("M", "F")
  )
  qa_step(
    "a codelist reaches the schema as its permitted values",
    c("M", "F"), as.character(unlist(as_json_schema(coded)$enum))
  )
})

test_that("OQ-SPEC-046 | a dataset entry written before datasets had an id is read with its old name as the id | REQ-SPEC-025", {
  dir <- qa_tempdir()
  column <- c(
    "    columns:",
    "      - id: ID",
    "        type: SAS Char",
    "        length: 8",
    "        nullable: false"
  )

  # Half one: the old shape. `name` was the identifier and `description` the
  # short label. A reader that took `name` for a label would mint no id at all
  # and leave the dataset unreachable under the name the agreement gave it.
  legacy <- file.path(dir, "legacy_dataset.yaml")
  writeLines(
    c(
      "name: dm_legacy",
      "type: tabular",
      "description: Demographics label",
      "columns:",
      "  - id: ID",
      "    type: SAS Char",
      "    length: 8",
      "    nullable: false"
    ),
    legacy
  )
  ds <- read_dataset_from_yaml(legacy)
  qa_step(
    "a legacy single dataset takes its old name as the id and its old description as the name",
    list(id = "dm_legacy", name = "Demographics label", has_description = FALSE),
    list(id = ds@id, name = ds@name, has_description = !is.null(ds@description))
  )

  # An old entry that never had a description has no label to carry over, and
  # must not be given an empty one.
  bare <- file.path(dir, "legacy_bare.yaml")
  writeLines(
    c("name: dm_bare", "type: tabular", "columns:", "  - id: ID", "    type: SAS Char", "    length: 8", "    nullable: false"),
    bare
  )
  ds_bare <- read_dataset_from_yaml(bare)
  qa_step(
    "a legacy entry without a description reads with its id and no name",
    list(id = "dm_bare", has_name = FALSE),
    list(id = ds_bare@id, has_name = !is.null(ds_bare@name))
  )

  # Half two, in one transfer: the old shape beside the current one. The
  # current entry must come through untouched -- in particular its `name` must
  # stay a label and not be taken for an identifier a second time.
  mixed <- file.path(dir, "mixed.yaml")
  writeLines(
    c(
      "metadata:",
      "  title: Mixed shapes",
      '  version: "1"',
      "datasets:",
      "  - name: dm_legacy",
      "    type: tabular",
      "    description: Demographics label",
      column,
      "  - id: lb_current",
      "    type: tabular",
      "    name: Laboratory results",
      "    description: Long free text about the laboratory results.",
      column
    ),
    mixed
  )
  dta <- read_dta_from_yaml(mixed)
  qa_step(
    "both entries are keyed by their ids",
    c("dm_legacy", "lb_current"), names(datasets(dta))
  )
  qa_step(
    "the legacy entry is upgraded",
    list(id = "dm_legacy", name = "Demographics label", has_description = FALSE),
    list(
      id = dta[["dm_legacy"]]@id, name = dta[["dm_legacy"]]@name,
      has_description = !is.null(dta[["dm_legacy"]]@description)
    )
  )
  qa_step(
    "the entry that has an id keeps its id, name and description unchanged",
    list(
      id = "lb_current", name = "Laboratory results",
      description = "Long free text about the laboratory results."
    ),
    list(
      id = dta[["lb_current"]]@id, name = dta[["lb_current"]]@name,
      description = dta[["lb_current"]]@description
    )
  )

  # The exported converter is the same rule on a bare list, and repeating it
  # changes nothing.
  entry <- list(name = "dm", type = "tabular", description = "Demographics")
  upgraded <- dta_dataset_list_upgrade(entry)
  qa_step(
    "dta_dataset_list_upgrade() moves name to id and description to name",
    list(id = "dm", name = "Demographics", type = "tabular"),
    upgraded[c("id", "name", "type")]
  )
  qa_check("and leaves no description behind", is.null(upgraded[["description"]]))
  qa_step(
    "and applying it again changes nothing",
    upgraded, dta_dataset_list_upgrade(upgraded)
  )
})
