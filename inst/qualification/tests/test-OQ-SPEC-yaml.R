# Reading and writing a specification. REQ-SPEC-001 .. REQ-SPEC-006,
# REQ-SPEC-014 .. REQ-SPEC-017, REQ-SPEC-025, REQ-SPEC-038 and REQ-SPEC-039.
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

test_that("OQ-SPEC-046 | a dataset entry without an id is refused | REQ-SPEC-025", {
  dir <- qa_tempdir()
  column <- c(
    "    columns:",
    "      - id: ID",
    "        type: SAS Char",
    "        length: 8",
    "        nullable: false"
  )
  refusal <- function(expr) {
    err <- tryCatch(expr, error = function(e) e)
    if (!inherits(err, "error")) {
      return(NA_character_)
    }
    gsub("[[:space:]]+", " ", conditionMessage(err))
  }

  # `name` is the human-readable label, never the identifier: an entry that
  # carries only a name must not be keyed by its label.
  qa_check(
    "a list with a name and a type but no id is refused, naming the missing id",
    grepl("needs an `id`", refusal(dta_dataset_from_list(list(name = "dm", type = "tabular"))), fixed = TRUE)
  )

  single <- file.path(dir, "no_id_dataset.yaml")
  writeLines(
    c("name: Demographics", "type: tabular", "columns:", "  - id: ID", "    type: SAS Char", "    length: 8", "    nullable: false"),
    single
  )
  qa_check(
    "a single dataset file without an id is refused",
    grepl("dataset_id_missing", refusal(read_dataset_from_yaml(single)), fixed = TRUE)
  )

  mixed <- file.path(dir, "mixed.yaml")
  writeLines(
    c(
      "metadata:",
      "  title: One entry lacks an id",
      '  version: "1"',
      "datasets:",
      "  - id: lb_current",
      "    type: tabular",
      "    name: Laboratory results",
      column,
      "  - name: Demographics",
      "    type: tabular",
      column
    ),
    mixed
  )
  qa_check(
    "a specification with one id-less entry among valid ones is refused as a whole",
    grepl("dataset_id_missing", refusal(read_dta_from_yaml(mixed)), fixed = TRUE)
  )

  current <- file.path(dir, "current.yaml")
  writeLines(
    c(
      "metadata:",
      "  title: Current shape",
      '  version: "1"',
      "datasets:",
      "  - id: lb_current",
      "    type: tabular",
      "    name: Laboratory results",
      "    description: Long free text about the laboratory results.",
      column
    ),
    current
  )
  dta <- read_dta_from_yaml(current)
  qa_step(
    "an entry that has an id keeps its id, name and description unchanged",
    list(
      id = "lb_current", name = "Laboratory results",
      description = "Long free text about the laboratory results."
    ),
    list(
      id = dta[["lb_current"]]@id, name = dta[["lb_current"]]@name,
      description = dta[["lb_current"]]@description
    )
  )
})

# ---- a specification is read strictly ----------------------------------------
#
# A key the reader does not know is an error that names it. It is never read as
# another key, as a default, or as nothing: a constraint dropped without a word
# reads as a delivery that satisfied it.

sy_cond <- function(expr) {
  tryCatch(
    {
      force(expr)
      NULL
    },
    error = function(e) e
  )
}

sy_file_dataset <- function(...) {
  list(id = "report", type = "file", files = list(type = "any", filename = "report.pdf"), ...)
}

test_that("OQ-SPEC-064 | a file dataset that declares columns or rules is refused when it is built | REQ-SPEC-038", {
  columns <- list(list(id = "ID", type = "SAS Char", length = 8))
  rules <- list(list(id = "u", type = "col_unique", columns = "ID"))
  files <- list(type = "any", filename = "report.pdf")

  qa_check(
    "columns on a file dataset is refused with the class dta_file_columns_key",
    inherits(sy_cond(DTADataSetFactory("file", id = "report", files = files, columns = columns)), "dta_file_columns_key")
  )
  qa_check(
    "rules on a file dataset is refused with the class dta_file_rules_key",
    inherits(sy_cond(DTADataSetFactory("file", id = "report", files = files, rules = rules)), "dta_file_rules_key")
  )
  qa_check(
    "the same refusal reaches a dataset built from a list, as a YAML file is",
    inherits(sy_cond(dta_dataset_from_list(sy_file_dataset(columns = columns))), "dta_file_columns_key")
  )

  qa_check(
    "an empty columns and an empty rules declare nothing, and a file dataset accepts them",
    inherits(
      DTADataSetFactory("file", id = "report", files = files, columns = list(), rules = list()),
      "DTAtools::DTADataSetFile"
    )
  )

  # The control: the same keys on a tabular dataset, whose files must be readable.
  tabular <- DTADataSetFactory(
    "tabular",
    id = "t", files = list(type = "csv", filename = "t.csv"), columns = columns, rules = rules
  )
  qa_step(
    "a tabular dataset keeps the one column and the one rule it declares",
    list(columns = "ID", rules = 1L),
    list(columns = names(specs(tabular)), rules = length(rules(specs(tabular))))
  )
})

test_that("OQ-SPEC-065 | lint_dta() reports columns and rules on a file dataset and a document carrying either is not read | REQ-SPEC-038", {
  dir <- qa_tempdir()
  header <- c("id: report", "type: file", "files:", "  type: any", "  filename: report.pdf")
  write <- function(name, extra = character()) {
    path <- file.path(dir, name)
    writeLines(c(header, extra), path)
    path
  }
  columns <- write("file_columns.yaml", c("columns:", "  - id: ID", "    type: SAS Char", "    length: 8"))
  rules <- write("file_rules.yaml", c("rules:", "  - id: u", "    type: col_unique", "    columns: ID"))
  both <- write("file_both.yaml", c(
    "columns:", "  - id: ID", "    type: SAS Char", "    length: 8",
    "rules:", "  - id: u", "    type: col_unique", "    columns: ID"
  ))
  clean <- write("file_clean.yaml")

  qa_step("columns alone is one finding, file_columns_key", "file_columns_key", lint_dta(columns)$code)
  qa_step("rules alone is one finding, file_rules_key", "file_rules_key", lint_dta(rules)$code)
  qa_step(
    "both are two findings, one each, all errors",
    list(codes = c("file_columns_key", "file_rules_key"), severities = c("error", "error")),
    list(codes = sort(lint_dta(both)$code), severities = lint_dta(both)$severity)
  )
  qa_step("a file dataset that declares neither lints clean", 0L, nrow(lint_dta(clean)))

  qa_check(
    "a dataset file carrying columns is not read",
    inherits(sy_cond(read_dataset_from_yaml(columns)), "dta_lint_error")
  )
  qa_check(
    "a dataset file carrying rules is not read",
    inherits(sy_cond(read_dataset_from_yaml(rules)), "dta_lint_error")
  )
  qa_step("while the clean file dataset is read", "report", read_dataset_from_yaml(clean)@id)

  # The same file dataset inside a document, as a transfer specification is written.
  document <- function(name, dataset_lines) {
    path <- file.path(dir, name)
    writeLines(
      c(
        "metadata:", "  title: A file dataset", "  version: \"1\"",
        "datasets:", paste0("  - ", dataset_lines[1]), paste0("    ", dataset_lines[-1])
      ),
      path
    )
    path
  }
  in_document <- document("doc_columns.yaml", c(
    header, "columns:", "  - id: ID", "    type: SAS Char", "    length: 8"
  ))
  qa_check(
    "a document whose file dataset declares columns is not read",
    inherits(sy_cond(read_dta_from_yaml(in_document)), "dta_lint_error")
  )
  qa_step(
    "while the same document with a file dataset that declares neither is read",
    "report",
    names(datasets(read_dta_from_yaml(document("doc_clean.yaml", header))))
  )
})

test_that("OQ-SPEC-066 | a column key that is not exactly an allowed key is refused, naming it and listing the allowed keys | REQ-SPEC-039", {
  allowed <- c(
    "id", "label", "type", "format", "length", "nullable", "pattern",
    "values", "examples", "description", "colclass"
  )
  refused <- function(...) {
    sy_cond(specs_from_list(list(list(id = "A", type = "SAS Char", ...))))
  }

  typo <- refused(exmaples = "x")
  qa_check("a misspelt key is refused with the class dta_column_unknown_field", inherits(typo, "dta_column_unknown_field"))
  qa_step("the condition names the misspelt key", "exmaples", typo$keys)
  qa_step(
    "and the message lists every allowed key",
    setNames(rep(TRUE, length(allowed)), allowed),
    vapply(allowed, function(key) grepl(key, conditionMessage(typo), fixed = TRUE), logical(1))
  )

  # R completes a name from its first letters; the specification does not.
  for (abbreviated in c("example", "desc", "null", "pat")) {
    column <- list(list(id = "A", type = "SAS Char"))
    column[[1]][[abbreviated]] <- "x"
    err <- sy_cond(specs_from_list(column))
    qa_step(paste0("the abbreviation ", abbreviated, " is refused, not completed"), abbreviated, err$keys)
  }

  # The same refusal reaches a dataset built from a list, and the row labels of a
  # matrix, whose own key `header` is not a misspelling.
  qa_step(
    "a misspelt key in a tabular dataset built from a list is refused",
    "nulable",
    sy_cond(dta_dataset_from_list(list(
      id = "t", type = "tabular", files = list(type = "csv", filename = "t.csv"),
      columns = list(list(id = "A", type = "SAS Char", nulable = FALSE))
    )))$keys
  )
  matrix_dataset <- function(label) {
    dta_dataset_from_list(list(
      id = "m", type = "matrix", files = list(type = "tsv", filename = "m.tsv"),
      row_labels = list(label), matrix = list(cells = list(type = "SAS Num"))
    ))
  }
  qa_step(
    "a misspelt key in a matrix row label is refused",
    "nulable",
    sy_cond(matrix_dataset(list(id = "G", type = "SAS Char", header = "gene", nulable = FALSE)))$keys
  )
  qa_step(
    "while a row label with header and every other key right is built",
    "G",
    names(specs(matrix_dataset(list(id = "G", type = "SAS Char", header = "gene", nullable = FALSE))))
  )

  value <- refused(zz = "SENTINEL_VALUE_Q7")
  qa_check(
    "an unknown key is named without printing its value",
    inherits(value, "dta_column_unknown_field") &&
      !grepl("SENTINEL_VALUE_Q7", conditionMessage(value), fixed = TRUE)
  )

  # `values` excludes `pattern` and `examples`, so three columns use the eleven keys.
  qa_step(
    "columns that use every allowed key are built and keep their ids",
    c("A", "B", "C"),
    names(specs_from_list(list(
      list(
        id = "A", label = "L", type = "SAS Char", length = 8, nullable = FALSE,
        values = c("X", "Y"), description = "d", colclass = "study_info"
      ),
      list(id = "B", type = "SAS Char", format = "SAS $8.", pattern = "^[A-Z]+$"),
      list(id = "C", type = "SAS Char", examples = "X")
    )))
  )
})

test_that("OQ-SPEC-067 | lint_dta() reports a misspelt or abbreviated column key as an invalid column, naming it | REQ-SPEC-039", {
  dir <- qa_tempdir()
  write <- function(name, key) {
    path <- file.path(dir, name)
    writeLines(
      c(
        "id: t", "type: tabular", "files:", "  type: csv", "  filename: t.csv",
        "columns:", "  - id: ID", "    type: SAS Char", "    length: 8",
        paste0("    ", key, ": x")
      ),
      path
    )
    path
  }
  typo <- lint_dta(write("typo.yaml", "exmaples"))
  qa_step("a misspelt key is one column_spec_invalid finding", "column_spec_invalid", typo$code[typo$severity == "error"])
  qa_check("whose message names the key", grepl("exmaples", typo$message[typo$code == "column_spec_invalid"], fixed = TRUE))

  abbreviated <- lint_dta(write("abbreviated.yaml", "example"))
  qa_step(
    "an abbreviated key is the same finding, not accepted as examples",
    "column_spec_invalid", abbreviated$code[abbreviated$severity == "error"]
  )

  exact <- lint_dta(write("exact.yaml", "examples"))
  qa_step("the exact spelling lints clean", 0L, sum(exact$code == "column_spec_invalid"))
  qa_check(
    "and the document with the misspelt key is not read",
    inherits(sy_cond(read_dataset_from_yaml(write("typo2.yaml", "exmaples"))), "dta_lint_error")
  )
})
