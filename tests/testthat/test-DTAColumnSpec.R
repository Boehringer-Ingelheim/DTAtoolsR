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
  expect_error(get_arrow_schema_type(DTAColumnSpec(id = "FLAG", nullable = TRUE)), "Structure is not set")
  expect_error(get_arrow_schema_type(structure(list(), class = "foo")), "must be a DTAColumnSpec")
})

test_that("create_example_DTAColumnSpec covers the supported indices", {
  expect_s3_class(create_example_DTAColumnSpec(1), "DTAtools::DTAColumnSpec")
  expect_s3_class(create_example_DTAColumnSpec(5), "DTAtools::DTAColumnSpec")
  expect_error(create_example_DTAColumnSpec(99), "Invalid index")
})
