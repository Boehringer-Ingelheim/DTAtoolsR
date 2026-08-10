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
  expect_equal(spec[[1]]@values, list("1234", "5678"))

  expect_equal(spec[[2]]@id, "VISIT")
  expect_equal(spec[[2]]@label, "Visit")
  expect_equal(spec[[1]]@structure@type, "Char")
  expect_equal(spec[[1]]@structure@backend, "SAS")
  expect_equal(spec[[2]]@description, "Visit code")
  expect_equal(spec[[2]]@values, list("V01", "EOT"))
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

test_that("DTAColumnSpec @id validator only rejects whitespace (documented gap)", {
  # The validator checks `is.null(self@id) || any(grepl("\\s", self@id))` and
  # nothing else, so the three ids below are accepted today even though none of
  # them is a usable column id: an empty string, a zero-length vector, and a
  # multi-element vector (which silently makes @id non-scalar).
  #
  # These expectations PIN the current behaviour rather than assert the desired
  # behaviour -- asserting that they error requires tightening the validator in
  # R/DTAColumnSpec-class.R. When that fix lands these three lines must flip to
  # expect_error(), which is exactly the deliberate, visible change we want.
  expect_identical(DTAColumnSpec(id = "")@id, "")
  expect_identical(DTAColumnSpec(id = character(0))@id, character(0))
  expect_identical(DTAColumnSpec(id = c("A", "B"))@id, c("A", "B"))
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

test_that("get_arrow_schema_type maps supported column types", {
  expect_equal(get_arrow_schema_type(create_example_DTAColumnSpec(1)), "utf8")
  expect_equal(get_arrow_schema_type(create_example_DTAColumnSpec(4)), "int32")
  expect_equal(
    get_arrow_schema_type(DTAColumnSpec(id = "AVAL", type = "SAS Num", format = "SAS 8.2")),
    "double"
  )
  expect_error(get_arrow_schema_type(DTAColumnSpec(id = "FLAG", nullable = TRUE)), "Structure is not set")
  expect_error(get_arrow_schema_type(structure(list(), class = "foo")), "must be a DTAColumnSpec")
})

test_that("get_arrow_schema_type returns NA for SAS temporal types (documented gap)", {
  # The switch() in get_arrow_schema_type() covers only Char/Num/Int/Bool, so
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
      get_arrow_schema_type(spec),
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

test_that("DTAColumnSpecStructureFactory validates backend prefixes", {
  expect_error(
    DTAColumnSpecStructureFactory(type = "R Char", format = "R $10."),
    "supported backends"
  )

  expect_error(
    DTAColumnSpecStructureFactory(type = "SAS Char", format = "R $10."),
    "supported backends"
  )

  # With no type, format or length there is no deliberate guard: the factory
  # falls through to `switch(NULL, ...)` and R raises its own base-level
  # "EXPR must be a length 1 vector". That message is localised (it is German on
  # a German R install), so pin the condition class instead of the text. The
  # class is the informative part: a bare `simpleError` rather than the
  # `rlang_error` that cli_abort() would produce is itself the evidence that
  # this input is unguarded rather than deliberately rejected.
  err <- expect_error(
    DTAColumnSpecStructureFactory(type = NULL, format = NULL, length = NULL),
    class = "simpleError"
  )
  expect_false(inherits(err, "rlang_error"))
})
