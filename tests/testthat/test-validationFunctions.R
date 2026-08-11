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

test_that("validate_table reports schema errors and rule violations in one pass", {
  # Previously validate_table() return()ed on the first schema failure, so the
  # rules branch never ran: a user fixed the length error, re-ran, and only then
  # discovered the duplicate.
  specs <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 3, nullable = FALSE)
    ),
    rules = list(DTARuleColUnique(id = "unique_id", columns = "ID"))
  )
  table <- data.frame(ID = c("TOOLONG", "TOOLONG"), stringsAsFactors = FALSE)

  details <- validate_table_detailed(specs = specs, table = table, verbose = FALSE)
  expect_false(details$schema_valid)
  expect_false(details$rules_valid)

  result <- suppressWarnings(
    validate_table(specs = specs, table = table, verbose = FALSE)
  )

  # The documented schema-error contract is unchanged ...
  expect_true(all(c("summarised_error", "full_error") %in% names(result)))
  expect_equal(nrow(as.data.frame(result$full_error)), 2)

  # ... and the rule violations are reported in the same pass.
  expect_false(result$rules_valid)
  expect_length(result$rule_errors, 1)
  expect_equal(result$rule_errors[[1]]$id, "unique_id")
  expect_match(result$rule_errors[[1]]$message, "violated")

  # They are surfaced too, not merely buried in the return value.
  expect_warning(
    validate_table(specs = specs, table = table, verbose = FALSE),
    "unique_id"
  )
})

test_that("A schema-only failure still records that the rules were checked", {
  specs <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 3, nullable = FALSE)
    ),
    rules = list(DTARuleColUnique(id = "unique_id", columns = "ID"))
  )
  table <- data.frame(ID = c("AAA", "TOOLONG"), stringsAsFactors = FALSE)

  result <- validate_table(specs = specs, table = table, verbose = FALSE)
  expect_true(result$rules_valid)
  expect_length(result$rule_errors, 0)
})

test_that("Multi-operator THEN conditions are enforced end to end", {
  # The reported escape: AGE = 200/999 passed an 18..65 band because only the
  # first operator of the THEN clause was ever evaluated.
  specs <- DTAColumnSpecCollection(
    columns = list(
      AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = FALSE)
    ),
    rules = list(DTARuleColCondition(
      id = "age_band",
      condition = list(AGE = list(greater_equal = 0)),
      then = list(AGE = list(greater = 18, less = 65))
    ))
  )

  expect_error(
    validate_table(specs = specs, table = data.frame(AGE = c(200, 999)), verbose = FALSE),
    "Schema rule violations"
  )

  expect_no_error(
    validate_table(specs = specs, table = data.frame(AGE = c(20, 64)), verbose = FALSE)
  )
})

test_that("validate_table does not abort when a rule names an absent column", {
  specs <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 12, nullable = FALSE)
    ),
    rules = list(DTARuleColRange(id = "stale_age_rule", columns = "AGE", min = 0, max = 120))
  )

  err <- expect_error(
    validate_table(specs = specs, table = data.frame(ID = "A"), verbose = FALSE),
    "Schema rule violations"
  )
  expect_match(conditionMessage(err), "stale_age_rule")
  expect_match(conditionMessage(err), "AGE")
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
