test_that("validate_table and validate_table_detailed pass for valid input", {
  specs <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 12, nullable = FALSE)
    )
  )
  table <- data.frame(ID = c("A001", "A002"), stringsAsFactors = FALSE)

  details <- validate_table_detailed(specs = specs, table = table, verbose = FALSE)
  expect_true(details$ok)
  expect_true(details$schema_valid)
  expect_true(details$rules_valid)
  expect_length(details$rule_errors, 0)
  expect_equal(details$n_schema_errors, 0)
  expect_equal(details$n_rule_errors, 0)

  validated <- validate_table(specs = specs, table = table, verbose = FALSE)
  expect_identical(validated, table)
})

test_that("validate_table returns schema errors and aborts on rule errors", {
  specs_schema <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 12, nullable = FALSE)
    )
  )
  invalid_table <- data.frame(ID = c("A001", NA), stringsAsFactors = FALSE)
  schema_errors <- validate_table(specs = specs_schema, table = invalid_table, verbose = FALSE)

  expect_true(is.list(schema_errors))
  expect_true(all(c("summarised_error", "full_error") %in% names(schema_errors)))
  expect_true(is.data.frame(schema_errors$full_error))

  # The NA in row 2 is the only violation: a non-nullable SAS Char column.
  full_error <- as.data.frame(schema_errors$full_error)
  expect_equal(nrow(full_error), 1)
  expect_equal(full_error$row, 2)
  expect_equal(full_error$column, "ID")
  expect_equal(full_error$keyword, "type")

  specs_rules <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 12, nullable = FALSE)
    ),
    rules = list(DTARuleColUnique(id = "unique_id", columns = "ID"))
  )
  duplicated_table <- data.frame(ID = c("A001", "A001"), stringsAsFactors = FALSE)

  expect_error(
    validate_table(specs = specs_rules, table = duplicated_table, verbose = FALSE),
    "Schema rule violations"
  )
})

test_that("Schema errors pin the offending row, column and keyword", {
  specs <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 3, nullable = FALSE)
    )
  )
  table <- data.frame(ID = c("AAA", "BBB", "TOOLONG"), stringsAsFactors = FALSE)

  schema_errors <- validate_table(specs = specs, table = table, verbose = FALSE)
  full_error <- as.data.frame(schema_errors$full_error)

  expect_equal(nrow(full_error), 1)
  expect_equal(full_error$row, 3)
  expect_equal(full_error$column, "ID")
  expect_equal(full_error$keyword, "maxLength")
  expect_match(full_error$message, "must NOT have more than 3 characters")
})

test_that("Row numbers stay correct for errors past the first 5000-row chunk", {
  # validate_table_detailed() validates in chunks of 5000 rows and offsets the
  # reported row by chunk_size * (chunk_index - 1). Only a violation in a later
  # chunk exercises that arithmetic.
  specs <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 3, nullable = FALSE)
    )
  )
  table <- data.frame(ID = c(rep("AAA", 5000), "TOOLONG"), stringsAsFactors = FALSE)

  schema_errors <- validate_table(specs = specs, table = table, verbose = FALSE)
  full_error <- as.data.frame(schema_errors$full_error)

  expect_equal(nrow(full_error), 1)
  expect_equal(full_error$row, 5001)
  expect_equal(full_error$column, "ID")
  expect_equal(full_error$keyword, "maxLength")
})
