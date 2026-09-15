# as_json_schema_length() had no test anywhere, direct or indirect. It is a
# one-line accessor (`x@structure@length`), but it's also the value
# as_json_schema() folds into $maxLength for every other tool that consumes
# the schema -- so both the accessor and where it lands in the schema are
# pinned here.

test_that("as_json_schema_length() reports a spec's declared maximum length", {
  spec <- DTAColumnSpec(
    id = "NAME", type = "SAS Char", format = "SAS $10.", length = 10, nullable = FALSE
  )
  expect_equal(as_json_schema_length(spec), 10)
})

test_that("as_json_schema() folds the declared length into $maxLength", {
  spec <- DTAColumnSpec(
    id = "NAME", type = "SAS Char", format = "SAS $10.", length = 10, nullable = FALSE
  )
  expect_equal(as_json_schema(spec)$maxLength, 10)
})
