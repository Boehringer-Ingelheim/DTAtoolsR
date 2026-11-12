test_that("DTAColumnSpec constructor works", {
  spec <- list()
  spec[[1]] <- create_example_DTAColumnSpec(1)
  spec[[2]] <- create_example_DTAColumnSpec(2)
  spec[[3]] <- create_example_DTAColumnSpec(3)
  spec[[4]] <- create_example_DTAColumnSpec(4)
  spec[[5]] <- create_example_DTAColumnSpec(5)

  purrr::walk(spec, function(s) {
    expect_s3_class(s, "DTAtools::DTAColumnSpec")
  })

  expect_equal(spec[[1]]@id, "STUDYID")
  expect_equal(spec[[1]]@label, "Study Identifier")
  expect_equal(spec[[1]]@structure@type, "Char")
  expect_equal(spec[[1]]@structure@backend, "SAS")
  expect_false(spec[[1]]@nullable)
  expect_equal(spec[[1]]@description, "Unique study identifier")
  # @values is normalised to an atomic vector on construction. YAML has no
  # list/vector distinction -- both write as the same sequence and read back as
  # a vector -- so a list representation could never survive a round trip.
  expect_equal(spec[[1]]@values, c("1234", "5678"))

  expect_equal(spec[[2]]@id, "VISIT")
  expect_equal(spec[[2]]@label, "Visit")
  expect_equal(spec[[1]]@structure@type, "Char")
  expect_equal(spec[[1]]@structure@backend, "SAS")
  expect_equal(spec[[2]]@description, "Visit code")
  expect_equal(spec[[2]]@values, c("V01", "EOT"))
  expect_false(spec[[2]]@nullable)

  expect_equal(spec[[4]]@id, "AGE")
  expect_equal(spec[[4]]@label, "Age")
  expect_equal(spec[[4]]@structure@type, "Int")
  expect_equal(spec[[4]]@structure@backend, "SAS")
  expect_equal(spec[[4]]@description, "Age in years")
  expect_equal(spec[[4]]@pattern, "^[0-9]{1,3}$")
  expect_true(spec[[4]]@nullable)
})

test_that("DTAColumnSpec rejects invalid metadata combinations", {
  expect_error(DTAColumnSpec(id = "STUDY ID"), "whitespaces")
  expect_error(DTAColumnSpec(id = "AGE", values = list("A"), pattern = "^[0-9]+$"), "pattern")
  expect_error(DTAColumnSpec(id = "AGE", values = list("A"), examples = c("A")), "examples")
  expect_error(DTAColumnSpec(id = "AGE", pattern = "^[0-9]+$", examples = c("A")), "must conform")
  expect_error(DTAColumnSpec(id = "AGE", colclass = "not_a_real_class"), "colclass")
})

test_that("DTAColumnSpec @id validator rejects empty, zero-length and multi-element ids", {
  expect_error(DTAColumnSpec(id = ""), class = "dta_spec_invalid_id")
  expect_error(DTAColumnSpec(id = character(0)), class = "dta_spec_invalid_id")
  expect_error(DTAColumnSpec(id = c("A", "B")), class = "dta_spec_invalid_id")
})

test_that("DTAColumnSpec builds structure metadata from type and format", {
  spec <- DTAColumnSpec(
    id = "AGE",
    type = "SAS Int",
    nullable = TRUE,
    description = "Age"
  )
  expect_s3_class(spec@structure, "DTAtools::DTAColumnSpecStructureSAS")
  expect_equal(spec@structure@type, "Int")
  expect_equal(spec@structure@backend, "SAS")

  spec_char <- DTAColumnSpec(
    id = "NAME",
    type = "SAS Char",
    format = "SAS $10.",
    length = 10,
    nullable = FALSE
  )
  expect_equal(spec_char@structure@type, "Char")
  expect_equal(spec_char@structure@format, "$10.")
  expect_equal(spec_char@structure@length, 10)
})

test_that("get_arrow_type maps supported column types", {
  expect_equal(get_arrow_type(create_example_DTAColumnSpec(1)), "utf8")
  expect_equal(get_arrow_type(create_example_DTAColumnSpec(4)), "int32")
  expect_equal(
    get_arrow_type(DTAColumnSpec(id = "AVAL", type = "SAS Num", format = "SAS 8.2")),
    "double"
  )
  expect_error(get_arrow_type(DTAColumnSpec(id = "FLAG", nullable = TRUE)), "Structure is not set")
  expect_error(get_arrow_type(structure(list(), class = "foo")), "must be a DTAColumnSpec")
})

test_that("get_arrow_type returns NA for SAS temporal types (documented gap)", {
  # The switch() in get_arrow_type() covers only Char/Num/Int/Bool, so
  # Date, Time and DateTime fall through to the NA_character_ default. That is
  # inconsistent with as_json_schema_type(), which maps all three to "string"
  # (see test-DTAColumnSpecStructureSAS.R), and it means an Arrow schema built
  # from a spec containing dates carries no type for those columns.
  #
  # Pinned, not asserted: the desired values ("date32"/"time32"/"timestamp")
  # require adding the branches in R/DTAColumnSpec-class.R.
  cases <- list(
    list(type = "SAS Date", format = "SAS DATE9."),
    list(type = "SAS Time", format = "SAS TIME8."),
    list(type = "SAS DateTime", format = "SAS DATETIME20.")
  )

  for (case in cases) {
    spec <- DTAColumnSpec(id = "TEMPORAL", type = case$type, format = case$format)
    expect_identical(
      get_arrow_type(spec),
      NA_character_,
      info = paste0("type = ", case$type)
    )
  }
})

test_that("create_example_DTAColumnSpec covers the supported indices", {
  expect_s3_class(create_example_DTAColumnSpec(1), "DTAtools::DTAColumnSpec")
  expect_s3_class(create_example_DTAColumnSpec(5), "DTAtools::DTAColumnSpec")
  expect_error(create_example_DTAColumnSpec(99), "Invalid index")
})

test_that("validator messages survive a column id containing braces", {
  # The validator used to build these messages with cli_abort(glue::glue(...)):
  # glue::glue() resolved `self@id` into the message text BEFORE cli ever saw
  # it, so an id such as "A{B}" left a literal, un-escaped "{B}" in the string
  # handed to cli_abort(), which then tried to evaluate it as an expression and
  # aborted with "Could not evaluate cli `{}` expression" instead of the
  # intended validation message.
  err <- expect_error(
    DTAColumnSpec(id = "A{B}", values = list("1"), pattern = "^[0-9]+$"),
    regexp = "pattern"
  )
  expect_match(conditionMessage(err), "A{B}", fixed = TRUE)
  expect_match(conditionMessage(err), "'pattern' cannot be set", fixed = TRUE)

  err2 <- expect_error(
    DTAColumnSpec(id = "A{B}", pattern = "^Z$", examples = c("nomatch")),
    regexp = "must conform"
  )
  expect_match(conditionMessage(err2), "A{B}", fixed = TRUE)
  expect_match(conditionMessage(err2), "must conform to the pattern", fixed = TRUE)
})

test_that("print() survives a column id containing braces", {
  # The id line is already interpolated directly (`{.field {x@id}}`), so this
  # pins the already-correct behaviour against a future regression. Note the
  # id/label lines both render only when `label` is set (see the guard on the
  # "id" alert in method(print, DTAColumnSpec)), so a label is provided here
  # purely to exercise the line -- unrelated to this test's purpose.
  spec <- DTAColumnSpec(id = "A{B}", label = "L{a}", type = "SAS Char")
  out <- capture.output(print(spec), type = "message")
  expect_true(any(grepl("A{B}", out, fixed = TRUE)))
})

test_that("DTAColumnSpecStructureFactory validates backend prefixes", {
  expect_error(
    DTAColumnSpecStructureFactory(type = "R Char", format = "R $10."),
    "supported backends"
  )

  expect_error(
    DTAColumnSpecStructureFactory(type = "SAS Char", format = "R $10."),
    "supported backends"
  )

  # With no type, format or length there is no backend to build: a package
  # error, not R's base "EXPR must be a length 1 vector" from switch(NULL, ...).
  expect_error(
    DTAColumnSpecStructureFactory(type = NULL, format = NULL, length = NULL),
    class = "dta_spec_no_backend"
  )
})

# ---- schema derivation for a column with no @structure ----------------------
#
# A DTAColumnSpec declared with only `values`/`pattern`/`nullable` -- no
# `type`, `format` or `length` -- has `@structure = NULL`. Before this fix,
# `as_json_schema_type(x@structure)` had no method for `NULL` and aborted;
# `as_json_schema()` caught that in R/columnSpecChecks.R and silently skipped
# the column's value checks entirely (see test-columnspec-checks.R for the
# end-to-end behaviour that pinned).

test_that("as_json_schema_type() on a typeless spec accepts every base type", {
  spec <- DTAColumnSpec(id = "SEX", nullable = FALSE, values = c("M", "F"))
  type <- as_json_schema_type(spec)

  # Every type dta_base_json_type() can report, so a present value of ANY
  # storage type never fails "type" -- only enum/pattern constrain it.
  expect_setequal(type, c("string", "integer", "number", "boolean"))
  expect_false("null" %in% type)
})

test_that("as_json_schema_type() on a typeless nullable spec also admits null", {
  spec <- DTAColumnSpec(id = "SEX", nullable = TRUE, values = c("M", "F"))
  type <- as_json_schema_type(spec)

  expect_true("null" %in% type)
  expect_true(all(c("string", "integer", "number", "boolean") %in% type))
})

test_that("as_json_schema_length() is NULL for a typeless spec", {
  spec <- DTAColumnSpec(id = "SEX", nullable = FALSE, values = c("M", "F"))
  expect_null(as_json_schema_length(spec))
})

test_that("as_json_schema() no longer errors for a typeless spec", {
  spec <- DTAColumnSpec(id = "SEX", nullable = FALSE, values = c("M", "F"))
  schema <- as_json_schema(spec)

  expect_null(schema$maxLength)
  expect_setequal(schema$type, c("string", "integer", "number", "boolean"))
  expect_setequal(schema$enum, c("M", "F"))
})

test_that("a typeless spec's enum matches both character and numeric data", {
  # `values: [1, 2]` with no declared type: the column might turn out to hold
  # text ("1", "2") or numbers (1, 2), and the schema has to match either --
  # coercing the declared values to one R type here would only ever satisfy
  # one of the two (see the comment in R/DTAColumnSpec-class.R).
  spec <- DTAColumnSpec(id = "CODE", nullable = FALSE, values = c(1, 2))
  schema <- as_json_schema(spec)

  expect_true(all(c("1", "2") %in% schema$enum))
  expect_true(all(c(1, 2) %in% schema$enum))
})

test_that("get_arrow_type rejects bad input with a cli condition, not a bare stop()", {
  # The three guards used stop(), so every other argument check in the package
  # raised an rlang_error and these three a plain simpleError. Pin the class and
  # the package-authored wording (never the base R text).
  expect_error(get_arrow_type("AGE"), "must be a DTAColumnSpec", class = "rlang_error")

  expect_error(
    get_arrow_type(DTAColumnSpec(id = "FLAG", nullable = TRUE)),
    "Structure is not set for FLAG",
    class = "rlang_error"
  )

  no_type <- DTAColumnSpec(id = "AGE", type = "SAS Char")
  no_type@structure@type <- NULL
  expect_error(
    get_arrow_type(no_type),
    "Type is not set for AGE",
    class = "rlang_error"
  )
})

test_that("get_arrow_type does not interpolate braces in a column id", {
  # The id is data. A brace in it must come out literally, not be evaluated.
  expect_error(
    get_arrow_type(DTAColumnSpec(id = "A{1+1}B", nullable = TRUE)),
    "A{1+1}B",
    fixed = TRUE,
    class = "rlang_error"
  )
})

# ---- review fixes: construction guards -------------------------------------

test_that("a column declaring only a length aborts with a package error naming the backend", {
  expect_error(
    DTAColumnSpec(id = "X", length = 20),
    class = "dta_spec_no_backend"
  )
  expect_error(
    specs_from_list(columns = list(list(id = "X", length = 20))),
    class = "dta_spec_no_backend"
  )
})

test_that("a column id must be a single, non-missing, non-empty string without whitespace", {
  for (bad in list("", NA_character_, character(0), c("A", "B"), "A B")) {
    expect_error(DTAColumnSpec(id = bad), class = "dta_spec_invalid_id")
  }
  expect_s3_class(DTAColumnSpec(id = "A_B"), "S7_object")
})

test_that("nullable must be a single TRUE or FALSE", {
  expect_error(DTAColumnSpec(id = "X", nullable = NA), class = "dta_spec_invalid_nullable")
  expect_error(DTAColumnSpec(id = "X", nullable = c(TRUE, FALSE)), class = "dta_spec_invalid_nullable")
  expect_false(DTAColumnSpec(id = "X", nullable = FALSE)@nullable)
})

test_that("values read from YAML as logicals abort with a quoting hint", {
  parsed <- yaml::yaml.load("values: [Y, N]")$values
  expect_true(is.logical(parsed))
  expect_error(DTAColumnSpec(id = "X", values = parsed), class = "dta_spec_logical_values")
  expect_error(
    DTAColumnSpec(id = "X", values = as.list(parsed)),
    class = "dta_spec_logical_values"
  )
})

test_that("create_example_DTAColumnSpec aborts on an unknown index with a cli condition", {
  expect_error(create_example_DTAColumnSpec(99), class = "rlang_error", regexp = "Invalid index")
})

test_that("print of a column spec shows the id even when there is no label", {
  out <- cli::cli_fmt(print(DTAColumnSpec(id = "NOLABEL", type = "SAS Char")))
  expect_true(any(grepl("NOLABEL", out, fixed = TRUE)))
})

test_that("a mixed list holding a YAML-logical Y/N still gets the quoting hint", {
  # `[Y, N, U]` parses to list(TRUE, FALSE, "U"); flattening it first turned
  # the logicals into the text "TRUE"/"FALSE" with no complaint.
  parsed <- yaml::yaml.load("values: [Y, N, U]")$values
  expect_error(DTAColumnSpec(id = "X", values = parsed), class = "dta_spec_logical_values")

  yml <- tempfile(fileext = ".yaml")
  writeLines(
    c("columns:", "  FLAG:", "    id: FLAG", "    type: SAS Char", "    nullable: false", "    values: [Y, N, U]"),
    yml
  )
  # The YAML entry point lints first and wraps the same hint.
  expect_error(import_specs_from_yaml(yml), class = "dta_lint_error")

  # Quoted, the same list is plain text and is kept as written.
  ok <- DTAColumnSpec(id = "X", values = list("Y", "N", "U"))
  expect_identical(ok@values, c("Y", "N", "U"))
})
