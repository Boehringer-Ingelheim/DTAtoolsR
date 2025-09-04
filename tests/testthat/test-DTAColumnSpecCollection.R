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

  expect_s3_class(collection, "DTAColumnSpecCollection")
  expect_equal(getColumnIds(collection), c("STUDYID", "VISIT"))
  expect_equal(getColumn(collection, "VISIT")@label, "Visit")
})

test_that("DTAColumnSpecCollectionFromList constructs valid object", {
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
  collection <- DTAColumnSpecCollectionFromList(
    columns = columns,
    rules = rules
  )

  # Assertions
  expect_s3_class(collection, "DTAColumnSpecCollection")
  expect_named(collection@columns, c("STUDYID", "VISIT"))
  expect_equal(collection@columns$STUDYID@id, "STUDYID")
  expect_equal(collection@columns$VISIT@values, list("V01", "EOT"))
  expect_equal(class(collection@columns[[1]]), c("DTAColumnSpec", "S7_object"))
  expect_equal(class(collection@rules[[1]]), c("DTARule", "S7_object"))

  # check getMetadata method
  expect_equal(getMetadata(collection), list())

  # Test DTAColumnSpecCollectionToList

  list <- DTAColumnSpecCollectionToList(collection)
  expect_type(list, "list")
})
