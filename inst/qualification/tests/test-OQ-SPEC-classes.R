# The classes a specification is built from. REQ-SPEC-007 .. REQ-SPEC-013 and
# REQ-SPEC-023.
#
# These are the objects a caller constructs by hand when a specification is
# written in R rather than in YAML, and they are also what the YAML readers
# produce. Their properties are the vocabulary the whole package speaks, so a
# property that quietly stops being carried takes a constraint with it.

sc_inherits <- function(x, class_name) {
  inherits(x, paste0("DTAtools::", class_name))
}

test_that("OQ-SPEC-030 | a column specification carries everything declared about a column | REQ-SPEC-007", {
  spec <- DTAColumnSpec(
    id = "SUBJID", label = "Subject identifier", description = "Unique per subject",
    type = "SAS Char", length = 8, nullable = FALSE, pattern = "^S[0-9]{7}$"
  )

  qa_check("the object is a column specification", sc_inherits(spec, "DTAColumnSpec"))
  qa_step(
    "every declared property is carried",
    list(
      id = "SUBJID", label = "Subject identifier", nullable = FALSE,
      pattern = "^S[0-9]{7}$"
    ),
    list(
      id = spec@id, label = spec@label, nullable = spec@nullable,
      pattern = as.character(spec@pattern)
    )
  )

  # A codelist and a pattern are two ways of saying which values are permitted,
  # and a column declaring both would leave a reader unable to tell which one
  # governs. The constructor refuses rather than picking one.
  qa_check(
    "a column may not declare both a codelist and a pattern",
    inherits(
      tryCatch(
        DTAColumnSpec(
          id = "X", type = "SAS Char", length = 8,
          values = c("a", "b"), pattern = "^a$"
        ),
        error = function(e) e
      ),
      "condition"
    )
  )
  qa_step(
    "a codelist alone is carried",
    c("a", "b"),
    as.character(DTAColumnSpec(id = "X", type = "SAS Char", length = 8, values = c("a", "b"))@values)
  )

  # A column with nothing declared but its identifier and type is the common
  # case, and it must not invent constraints it was not given.
  minimal <- DTAColumnSpec(id = "FREE", type = "SAS Char")
  qa_step(
    "an undeclared codelist and pattern stay absent rather than becoming empty constraints",
    c(values = 0L, pattern = 0L),
    c(
      values = length(minimal@values %||% NULL),
      pattern = length(minimal@pattern %||% NULL)
    )
  )
})

test_that("OQ-SPEC-031 | a collection keys its columns by identifier and carries its rules | REQ-SPEC-008", {
  collection <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
      AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    ),
    rules = list(DTARuleColRange(id = "age_range", columns = "AGE", range = c(18, 70)))
  )

  qa_check("the object is a collection", sc_inherits(collection, "DTAColumnSpecCollection"))
  qa_step("its columns are keyed by identifier", c("ID", "AGE"), names(collection))
  qa_step(
    "and each key names the column it holds",
    c("ID", "AGE"),
    vapply(names(collection), function(id) colspec(collection, id)@id, character(1),
      USE.NAMES = FALSE
    )
  )
  qa_step(
    "the rules are carried alongside the columns",
    "age_range", vapply(rules(collection), function(r) r@id, character(1))
  )
})

test_that("OQ-SPEC-032 | a column structure carries its type, format and length | REQ-SPEC-009", {
  plain <- DTAColumnSpecStructure(type = "Char", length = 8, backend = "SAS")
  qa_check("a structure is built", sc_inherits(plain, "DTAColumnSpecStructure"))
  qa_step(
    "carrying what it was given",
    list(type = "Char", length = 8), list(type = plain@type, length = plain@length)
  )

  # A SAS specification often gives only the format, and the type has to follow
  # from it: a transfer described in SAS terms should not have to restate what
  # the format already says.
  sas <- DTAColumnSpecStructureSAS(format = "DATE9.", length = 9)
  qa_check("a SAS structure is built", sc_inherits(sas, "DTAColumnSpecStructureSAS"))
  qa_step("and the type is inferred from the format", "Date", sas@type)

  qa_step(
    "a numeric format with decimals infers a numeric type",
    "Num", DTAColumnSpecStructureSAS(format = "8.2", length = 8)@type
  )
  qa_step(
    "and a character format infers a character type",
    "Char", DTAColumnSpecStructureSAS(format = "$10.", length = 10)@type
  )

  # The factory takes the backend and the type as one string, which is how a
  # specification writes it. A bare type names no backend and is refused rather
  # than assumed, so a specification cannot silently acquire one.
  built <- DTAColumnSpecStructureFactory(type = "SAS Char", length = 8)
  qa_check("the factory builds a structure", sc_inherits(built, "DTAColumnSpecStructureSAS"))
  qa_step("carrying the type named after the backend", "Char", built@type)
  qa_check(
    "a type naming no backend is refused",
    inherits(
      tryCatch(DTAColumnSpecStructureFactory(type = "Char", length = 8), error = function(e) e),
      "condition"
    )
  )
})

test_that("OQ-SPEC-033 | the dataset classes carry their name, handlers and state | REQ-SPEC-010", {
  specs <- DTAColumnSpecCollection(
    columns = list(ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE))
  )
  tabular <- DTADataSetTabular(
    id = "clinical", specs = specs,
    files = list(DTAFileCSV(filename = "clinical.csv"))
  )

  qa_check("a tabular dataset is built", sc_inherits(tabular, "DTADataSetTabular"))
  qa_check("it is a dataset", sc_inherits(tabular, "DTADataSet"))
  qa_step("carrying its id", "clinical", tabular@id)
  qa_check("and no name or description unless it is given one", is.null(tabular@name) && is.null(tabular@description))
  labelled <- DTADataSetTabular(
    id = "clinical", name = "Clinical data", description = "One row per subject and visit.",
    specs = specs, files = list(DTAFileCSV(filename = "clinical.csv"))
  )
  qa_step(
    "a name and a description, when given, are kept apart from the id",
    list(id = "clinical", name = "Clinical data", description = "One row per subject and visit."),
    list(id = labelled@id, name = labelled@name, description = labelled@description)
  )
  qa_step("and its file handlers", 1L, length(files(tabular)))
  # Nothing is on record until a delivery has been bound and checked: a
  # dataset with no table has no verdict to report, which is different from
  # having a verdict of "not yet checked" for a table it does hold.
  qa_step(
    "a dataset with no delivery bound reports no validation state at all",
    0L, nrow(validation_status(tabular))
  )

  # A file-presence dataset checks that a deliverable arrived, which is what a
  # transfer of archives or images needs and what a tabular check cannot say.
  presence <- DTADataSetFile(
    id = "images", files = list(DTAFileAny(filename = "scan.nii.gz"))
  )
  qa_check("a file-presence dataset is built", sc_inherits(presence, "DTADataSetFile"))
  qa_step("carrying its id", "images", presence@id)

  by_factory <- DTADataSetFactory(
    type = "tabular", id = "made",
    columns = list(), rules = list(), files = list()
  )
  qa_check("the factory builds the type it is asked for", sc_inherits(by_factory, "DTADataSetTabular"))
})

test_that("OQ-SPEC-034 | each file handler carries what its format needs | REQ-SPEC-011", {
  csv <- DTAFileCSV(filename = "a.csv")
  tsv <- DTAFileTSV(filename = "a.tsv")
  delim <- DTAFileDelim(filename = "a.txt", sep = ";")
  any_file <- DTAFileAny(filename = "a.tar.gz")

  qa_step(
    "each handler is the class its format names",
    c(csv = TRUE, tsv = TRUE, delim = TRUE, any = TRUE),
    c(
      csv = sc_inherits(csv, "DTAFileCSV"),
      tsv = sc_inherits(tsv, "DTAFileTSV"),
      delim = sc_inherits(delim, "DTAFileDelim"),
      any = sc_inherits(any_file, "DTAFileAny")
    )
  )

  # The separator is the whole difference between the tabular handlers, and a
  # handler reading with the wrong one produces a single column of joined text
  # that then fails every check for the wrong reason.
  qa_step(
    "the tabular handlers differ in the separator they read with",
    c(csv = ",", tsv = "\t", delim = ";"),
    c(csv = csv@sep, tsv = tsv@sep, delim = delim@sep)
  )
  qa_step(
    "the tabular handlers share a common class",
    c(csv = TRUE, tsv = TRUE, delim = TRUE),
    c(
      csv = sc_inherits(csv, "DTAFileTabular"),
      tsv = sc_inherits(tsv, "DTAFileTabular"),
      delim = sc_inherits(delim, "DTAFileTabular")
    )
  )
  qa_step(
    "and every handler is a file handler",
    TRUE,
    all(vapply(list(csv, tsv, delim, any_file), sc_inherits, logical(1), "DTAFile"))
  )

  qa_check(
    "the factory builds the handler its type names",
    sc_inherits(DTAFileFactory(type = "csv", filename = "b.csv"), "DTAFileCSV")
  )
})

test_that("OQ-SPEC-035 | metadata and rules are built as their own classes | REQ-SPEC-012", {
  meta <- create_example_DTAMetaData()
  qa_check("metadata is built", sc_inherits(meta, "DTAMetaData"))
  qa_check("carrying a title", nzchar(meta@title %||% ""))

  # Every rule type shares one class, which is what lets a specification hold a
  # list of rules without knowing which kinds it will get.
  every_rule <- list(
    DTARuleColRange(id = "r", columns = "A", range = c(0, 1)),
    DTARuleColUnique(id = "u", columns = "A"),
    DTARuleColCondition(
      id = "c", condition = list(A = list(equals = 1)), then = list(B = list(equals = 2))
    )
  )
  qa_step(
    "every rule type is a rule",
    TRUE, all(vapply(every_rule, sc_inherits, logical(1), "DTARule"))
  )
})

test_that("OQ-SPEC-036 | a factory refuses a type it does not know | REQ-SPEC-013", {
  # Silently building something else, or nothing, would put a specification
  # into service that does not describe what its author wrote.
  qa_check(
    "an unknown dataset type raises a condition",
    inherits(tryCatch(DTADataSetFactory(id = "x", type = "nonsense"), error = function(e) e), "condition")
  )
  qa_check(
    "an unknown file type raises a condition",
    inherits(tryCatch(DTAFileFactory(type = "nonsense", filename = "a"), error = function(e) e), "condition")
  )
  qa_check(
    "an unknown rule type raises a condition",
    inherits(tryCatch(DTARuleFactory(id = "x", type = "nonsense"), error = function(e) e), "condition")
  )
})

test_that("OQ-SPEC-037 | every shipped example constructor builds its class | REQ-SPEC-023", {
  # These are what a reader meets in every help page. One that does not build
  # makes the documentation wrong exactly where a new user trusts it most.
  expected <- c(
    create_example_DTA = "DTA",
    create_example_DTAColumnSpec = "DTAColumnSpec",
    create_example_DTAColumnSpecCollection = "DTAColumnSpecCollection",
    create_example_DTADataSetTabular = "DTADataSetTabular",
    create_example_DTAFileCSV = "DTAFileCSV",
    create_example_DTAFileTSV = "DTAFileTSV",
    create_example_DTAMetaData = "DTAMetaData",
    create_example_DTARuleColCondition = "DTARuleColCondition",
    create_example_DTARuleColRange = "DTARuleColRange",
    create_example_DTARuleColUnique = "DTARuleColUnique"
  )

  built <- vapply(names(expected), function(fun) {
    object <- tryCatch(do.call(fun, list()), error = function(e) e)
    if (inherits(object, "condition")) "<error>" else sub("^DTAtools::", "", class(object)[[1]])
  }, character(1))

  qa_step(
    "each example constructor builds the class it names",
    unname(expected), unname(built)
  )
})

test_that("OQ-SPEC-045 | a DTA whose datasets carry a duplicate, empty or NA name is refused | REQ-SPEC-024", {
  one_ds <- function(id) {
    DTADataSetTabular(id = id, specs = create_example_DTAColumnSpecCollection(1))
  }

  # An unnamed list: DTA() derives names from each dataset's own @id, and
  # two datasets sharing "clinical" collide there before check() ever runs.
  dup_err <- tryCatch(
    DTA(datasets = list(one_ds("clinical"), one_ds("clinical")), metadata = create_example_DTAMetaData()),
    error = function(e) e
  )
  qa_check("a repeated dataset name is refused rather than silently accepted", inherits(dup_err, "condition"))
  qa_step(
    "naming the duplicate rather than a generic complaint",
    TRUE,
    grepl("duplicate name", conditionMessage(dup_err), fixed = TRUE) &&
      grepl("clinical", conditionMessage(dup_err), fixed = TRUE)
  )

  # A named list this time, so DTA() keeps the caller's own (broken) names
  # instead of deriving them -- the route a hand-assembled @datasets<- or a
  # malformed YAML read can still reach.
  empty_err <- tryCatch(
    DTA(
      datasets = stats::setNames(list(one_ds("a"), one_ds("b")), c("", "b")),
      metadata = create_example_DTAMetaData()
    ),
    error = function(e) e
  )
  na_err <- tryCatch(
    DTA(
      datasets = stats::setNames(list(one_ds("a"), one_ds("b")), c(NA_character_, "b")),
      metadata = create_example_DTAMetaData()
    ),
    error = function(e) e
  )
  qa_step(
    "an empty name and an NA name are refused the same way",
    c(empty = TRUE, na = TRUE),
    c(empty = inherits(empty_err, "condition"), na = inherits(na_err, "condition"))
  )

  # Distinct, non-empty, non-NA names: the control case, so a reader knows
  # the validator is not simply refusing every DTA with two datasets.
  ok <- tryCatch(
    DTA(datasets = list(one_ds("clinical"), one_ds("labs")), metadata = create_example_DTAMetaData()),
    error = function(e) e
  )
  qa_check("two datasets with distinct names construct without complaint", !inherits(ok, "condition"))
})

test_that("OQ-SPEC-047 | a DTA whose list name for a dataset differs from that dataset's id is refused | REQ-SPEC-024", {
  one_ds <- function(id) {
    DTADataSetTabular(id = id, specs = create_example_DTAColumnSpecCollection(1))
  }
  flat <- function(e) gsub("[[:space:]]+", " ", conditionMessage(e))

  # results() reports a dataset under its @id while check(), messages() and the
  # exports report it under its key, so a key that is not the id would show one
  # dataset under two names in the same transfer.
  wrong_key <- tryCatch(
    DTA(datasets = list(wrong_key = one_ds("real_id")), metadata = create_example_DTAMetaData()),
    error = function(e) e
  )
  qa_check("a dataset listed under a key other than its id is refused", inherits(wrong_key, "condition"))
  qa_step(
    "saying that the key must be the dataset's id",
    TRUE, grepl("keyed by its dataset", flat(wrong_key), fixed = TRUE)
  )
  qa_step(
    "and naming both the key and the id it disagrees with",
    c(key = TRUE, id = TRUE),
    c(key = grepl("wrong_key", flat(wrong_key), fixed = TRUE), id = grepl("real_id", flat(wrong_key), fixed = TRUE))
  )

  # One matching entry beside one that is not: the refusal names only the
  # offender.
  one_bad <- tryCatch(
    DTA(
      datasets = list(good = one_ds("good"), other = one_ds("labs")),
      metadata = create_example_DTAMetaData()
    ),
    error = function(e) e
  )
  qa_step(
    "only the entry that disagrees is named",
    c(offender = TRUE, innocent = FALSE),
    c(
      offender = grepl("other", flat(one_bad), fixed = TRUE),
      innocent = grepl("\"good\"", flat(one_bad), fixed = TRUE)
    )
  )

  # The same rule holds for a DTA that already exists: assigning to @datasets
  # re-validates, so the route that bypasses the constructor is closed too.
  dta <- DTA(datasets = list(labs = one_ds("labs")), metadata = create_example_DTAMetaData())
  reassigned <- tryCatch(
    {
      dta@datasets <- list(renamed = one_ds("labs"))
      NULL
    },
    error = function(e) e
  )
  qa_check(
    "re-keying an existing DTA's dataset away from its id is refused as well",
    inherits(reassigned, "condition") && grepl("keyed by its dataset", flat(reassigned), fixed = TRUE)
  )
})

test_that("OQ-SPEC-048 | a column specification whose own values or examples cannot fit it is refused | REQ-SPEC-026", {
  mismatch <- function(...) {
    e <- tryCatch(DTAColumnSpec(...), error = function(e) e)
    inherits(e, "dta_spec_value_mismatch")
  }

  # Each refusal below is worked out from the rule: the declared value is longer
  # than the width, not a whole number, outside 2^31-1, not displayable, or not
  # display text.
  qa_check(
    "a codelist value longer than the declared length",
    mismatch(id = "X", type = "SAS Char", length = 3, values = "ABCD")
  )
  qa_check(
    "an example longer than the declared length",
    mismatch(id = "X", type = "SAS Char", length = 3, examples = "ABCD")
  )
  qa_check(
    "a codelist value longer than the $w. width, with no length declared",
    mismatch(id = "X", type = "SAS Char", format = "SAS $3.", values = c("AB", "ABCD"))
  )
  qa_check("text under Int", mismatch(id = "X", type = "SAS Int", values = "abc"))
  qa_check("a fraction under Int", mismatch(id = "X", type = "SAS Int", values = "1.5"))
  qa_check("2^31 under Int", mismatch(id = "X", type = "SAS Int", values = "2147483648"))
  qa_check("text under Num", mismatch(id = "X", type = "SAS Num", examples = "n/a"))
  qa_check(
    "three decimals under 8.2",
    mismatch(id = "X", type = "SAS Num", format = "SAS 8.2", values = c("1.5", "1.234"))
  )
  qa_check(
    "an ISO date under DATE9.",
    mismatch(id = "X", type = "SAS Date", format = "SAS DATE9.", examples = "2025-08-15")
  )

  # Controls: the same shapes with values that do fit are accepted, so the
  # refusals above are about the values and not about the construction.
  fits <- list(
    list(id = "X", type = "SAS Char", length = 3, values = c("AB", "ABC")),
    list(id = "X", type = "SAS Int", values = c("1", "2147483647", "-2147483647")),
    list(id = "X", type = "SAS Num", format = "SAS 8.2", values = c("1.5", "12345.67")),
    list(id = "X", type = "SAS Date", format = "SAS DATE9.", examples = c("15AUG2025", "15aug25"))
  )
  accepted <- vapply(
    fits, function(args) !inherits(tryCatch(do.call(DTAColumnSpec, args), error = function(e) e), "condition"),
    logical(1)
  )
  qa_step("values that fit construct without complaint", rep(TRUE, length(fits)), accepted)

  # An invalid regular expression is its own refusal, found at construction
  # rather than on the first value it meets.
  bad_pattern <- tryCatch(
    DTAColumnSpec(id = "X", type = "SAS Char", length = 8, pattern = "(["),
    error = function(e) e
  )
  qa_check("an unterminated character class is refused as an invalid pattern", inherits(bad_pattern, "dta_spec_invalid_pattern"))
  qa_check(
    "and it is not mistaken for a value mismatch",
    !inherits(bad_pattern, "dta_spec_value_mismatch")
  )

  # A specification file goes through the same constructor.
  dir <- qa_tempdir()
  path <- file.path(dir, "columns.yaml")
  writeLines(
    c(
      "columns:",
      "  CODE:",
      "    id: CODE",
      "    type: SAS Char",
      "    length: 3",
      "    values: [AB, ABCD]"
    ),
    path
  )
  from_yaml <- tryCatch(import_specs_from_yaml(path), error = function(e) e)
  qa_check("a column specification file with a value that cannot fit is refused", inherits(from_yaml, "condition"))
  qa_check(
    "and the refusal is the linter's, carrying a column_spec_invalid finding for that column",
    inherits(from_yaml, "dta_lint_error") &&
      identical(from_yaml$findings$code, "column_spec_invalid") &&
      identical(from_yaml$findings$id, "CODE")
  )
})

test_that("OQ-SPEC-056 | a DTA with an entry that is not a DTADataSet is refused, naming the entry | REQ-SPEC-031", {
  ds <- DTADataSetTabular(id = "good", specs = create_example_DTAColumnSpecCollection(1))
  md <- create_example_DTAMetaData()

  err <- tryCatch(
    DTA(datasets = list(good = ds, stray = "not a dataset", other = 3), metadata = md),
    error = function(e) e
  )
  qa_check("the refusal has class dta_invalid_dataset_entry", inherits(err, "dta_invalid_dataset_entry"))
  msg <- if (inherits(err, "condition")) conditionMessage(err) else ""
  qa_step(
    "the message names both offending entries and not the valid one",
    c(stray = TRUE, other = TRUE, good = FALSE),
    c(
      stray = grepl("stray", msg, fixed = TRUE),
      other = grepl("other", msg, fixed = TRUE),
      good = grepl("good", msg, fixed = TRUE)
    )
  )

  ok <- tryCatch(DTA(datasets = list(good = ds), metadata = md), error = function(e) e)
  qa_check("a list of DTADataSet entries alone constructs", !inherits(ok, "condition"))
})
