test_that("testing import_specs_from_yaml", {
  specs <- import_specs_from_yaml(system.file("extdata", "params_gf.yaml", package = "DTAtools"))

  expect_s3_class(specs, "DTAtools::DTAColumnSpecCollection")
})

test_that("DTAColumnSpecCollection stores and retrieves specs", {
  col1 <- DTAColumnSpec(
    id = "STUDYID",
    label = "Study ID",
    type = "Char",
    format = "8",
    nullable = FALSE,
    description = "ID"
  )
  col2 <- DTAColumnSpec(
    id = "VISIT",
    label = "Visit",
    type = "Char",
    format = "4",
    nullable = TRUE,
    description = "Visit"
  )

  collection <- DTAColumnSpecCollection(
    columns = list(STUDYID = col1, VISIT = col2)
  )

  expect_equal(colspec(collection, 1), col1)


  expect_s3_class(collection, "DTAtools::DTAColumnSpecCollection")
  expect_equal(get_column_ids(collection), c("STUDYID", "VISIT"))
  expect_equal(get_column(collection, "VISIT")@label, "Visit")
})

test_that("specs_from_list constructs valid object", {
  # Sample input
  columns <- list(
    list(
      id = "STUDYID",
      label = "Study ID",
      type = "Char",
      nullable = FALSE,
      values = list("1234-4579"),
      pattern = "^[0-9]{4}-[0-9]{4}$",
      description = "Study identifier"
    ),
    list(
      id = "VISIT",
      label = "Visit",
      type = "Char",
      nullable = TRUE,
      values = list("V01", "EOT"),
      description = "Visit code"
    )
  )

  rule1 <- list(
    id = "rule1",
    type = "check_range",
    column = "age",
    range = list(18, 65)
  )

  rules <- list(rule1)

  # Run function
  collection <- specs_from_list(
    columns = columns,
    rules = rules
  )

  # Assertions
  expect_s3_class(collection, "DTAtools::DTAColumnSpecCollection")
  expect_named(collection@columns, c("STUDYID", "VISIT"))
  expect_equal(collection@columns$STUDYID@id, "STUDYID")
  expect_equal(collection@columns$VISIT@values, list("V01", "EOT"))
  expect_equal(
    class(collection@columns[[1]]),
    c("DTAtools::DTAColumnSpec", "S7_object")
  )
  expect_equal(
    class(collection@rules[[1]]),
    c("DTAtools::DTARule", "S7_object")
  )

  # check metadata method
  expect_equal(get_metadata(collection), list())

  # Test specs_to_list

  list <- specs_to_list(collection)
  expect_type(list, "list")
})
