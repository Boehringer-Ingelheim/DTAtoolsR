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
    name = "clinical", specs = specs,
    files = list(DTAFileCSV(filename = "clinical.csv"))
  )

  qa_check("a tabular dataset is built", sc_inherits(tabular, "DTADataSetTabular"))
  qa_check("it is a dataset", sc_inherits(tabular, "DTADataSet"))
  qa_step("carrying its name", "clinical", tabular@name)
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
    name = "images", files = list(DTAFileAny(filename = "scan.nii.gz"))
  )
  qa_check("a file-presence dataset is built", sc_inherits(presence, "DTADataSetFile"))
  qa_step("carrying its name", "images", presence@name)

  by_factory <- DTADataSetFactory(
    type = "tabular", name = "made",
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
    inherits(tryCatch(DTADataSetFactory(name = "x", type = "nonsense"), error = function(e) e), "condition")
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
