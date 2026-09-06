test_that("validate_table and validate_table_detailed pass for valid input", {
  specs <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 12, nullable = FALSE)
    )
  )
  table <- data.frame(ID = c("A001", "A002"), stringsAsFactors = FALSE)

  details <- validate_table_detailed(specs = specs, table = table, verbose = FALSE)
  expect_true(details$ok)
  expect_true(details$columnspec_valid)
  expect_true(details$rules_valid)
  expect_length(details$rule_errors, 0)
  expect_equal(details$n_columnspec_errors, 0)
  expect_equal(details$n_rule_errors, 0)

  validated <- validate_table(specs = specs, table = table, verbose = FALSE)
  expect_identical(validated, table)
})

test_that("validate_table returns column spec errors and aborts on rule errors", {
  specs_columnspec <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 12, nullable = FALSE)
    )
  )
  invalid_table <- data.frame(ID = c("A001", NA), stringsAsFactors = FALSE)
  columnspec_errors <- validate_table(specs = specs_columnspec, table = invalid_table, verbose = FALSE)

  expect_true(is.list(columnspec_errors))
  expect_true(all(c("summarised_error", "full_error") %in% names(columnspec_errors)))
  expect_true(is.data.frame(columnspec_errors$full_error))

  # The NA in row 2 is the only violation: a non-nullable SAS Char column.
  full_error <- as.data.frame(columnspec_errors$full_error)
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
    "Rule violations"
  )
})

test_that("Column spec errors pin the offending row, column and keyword", {
  specs <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 3, nullable = FALSE)
    )
  )
  table <- data.frame(ID = c("AAA", "BBB", "TOOLONG"), stringsAsFactors = FALSE)

  columnspec_errors <- validate_table(specs = specs, table = table, verbose = FALSE)
  full_error <- as.data.frame(columnspec_errors$full_error)

  expect_equal(nrow(full_error), 1)
  expect_equal(full_error$row, 3)
  expect_equal(full_error$column, "ID")
  expect_equal(full_error$keyword, "maxLength")
  expect_match(full_error$message, "must NOT have more than 3 characters")
})

test_that("validate_table reports column spec errors and rule violations in one pass", {
  # Previously validate_table() return()ed on the first column spec failure, so the
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
  expect_false(details$columnspec_valid)
  expect_false(details$rules_valid)

  result <- suppressWarnings(
    validate_table(specs = specs, table = table, verbose = FALSE)
  )

  # The documented column-spec-error contract is unchanged ...
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

test_that("A column-spec-only failure still records that the rules were checked", {
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
    "Rule violations"
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
    "Rule violations"
  )
  expect_match(conditionMessage(err), "stale_age_rule")
  expect_match(conditionMessage(err), "AGE")
})

test_that("declared types are stamped per distinct column, not per error row", {
  # The lookup walks the collection and dispatches through S7, and it was run
  # once per ERROR. A wholly mistyped column of 10,000 rows therefore paid it
  # 10,000 times for one answer. Deriving it per distinct column is the same
  # function of the same inputs, so the frames must be identical -- asserted
  # here against the per-row form the fix replaced, over a frame mixing three
  # columns of which only two are declared.
  specs <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 8, nullable = FALSE),
      AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    )
  )

  errors <- data.frame(
    row = 1:6,
    column = c("AGE", "UNDECLARED", "ID", "AGE", "UNDECLARED", "ID"),
    raw = c("a", "b", "c", "d", "e", "f"),
    declared_type = c("character", "character", "double", "character", "integer", "double"),
    reason = "not_convertible",
    stringsAsFactors = FALSE
  )

  per_row <- errors
  declared <- vapply(
    per_row$column,
    function(column) dta_spec_declared_type(specs, column),
    character(1),
    USE.NAMES = FALSE
  )
  per_row$declared_type <- ifelse(is.na(declared), per_row$declared_type, declared)

  expect_identical(dta_apply_spec_declared_types(errors, specs), per_row)

  # And the answers themselves: the declared type where there is one, the
  # observed storage type where there is not.
  stamped <- dta_apply_spec_declared_types(errors, specs)
  expect_identical(stamped$declared_type[stamped$column == "AGE"], c("SAS Num", "SAS Num"))
  expect_identical(stamped$declared_type[stamped$column == "ID"], c("SAS Char", "SAS Char"))
  expect_identical(
    stamped$declared_type[stamped$column == "UNDECLARED"],
    c("character", "integer")
  )
})


test_that("dta_apply_spec_declared_types leaves an empty or absent frame alone", {
  expect_null(dta_apply_spec_declared_types(NULL, NULL))
  empty <- dta_empty_import_errors()
  expect_identical(dta_apply_spec_declared_types(empty, NULL), empty)
})


test_that("max_errors caps the retained detail without moving any count", {
  # The streaming path has bounded its error frames since it existed; a table
  # already in memory retained one row per bad cell without limit, so a wholly
  # mistyped column cost a second copy of the table in error detail.
  specs <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 2, nullable = FALSE),
      AGE = DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
    )
  )
  # Typed at the import choke point first, so the table carries import issues as
  # a loaded one would: both per-cell axes then have more rows than the cap.
  table <- dta_coerce_table_to_specs(
    data.frame(
      ID = paste0("TOOLONG", 1:5),
      AGE = rep("abc", 5),
      stringsAsFactors = FALSE
    ),
    specs
  )$table

  full <- validate_table_detailed(specs, table, verbose = FALSE)
  capped <- validate_table_detailed(specs, table, verbose = FALSE, max_errors = 2L)

  # Counts, verdict and the per-check breakdown come from the complete frames.
  expect_identical(capped$ok, full$ok)
  expect_identical(capped$n_columnspec_errors, full$n_columnspec_errors)
  expect_identical(capped$n_import_errors, full$n_import_errors)
  expect_identical(capped$columnspec_checks, full$columnspec_checks)
  expect_identical(
    capped$columnspec_errors$summarised_error,
    full$columnspec_errors$summarised_error
  )

  # Only retention shrank, and the frame says so.
  expect_identical(nrow(capped$columnspec_errors$full_error), 2L)
  expect_true(isTRUE(attr(capped$columnspec_errors$full_error, "truncated")))
  expect_identical(nrow(capped$import_errors), 2L)
  expect_true(isTRUE(attr(capped$import_errors, "truncated")))
  # The retained rows are the head of the complete frame, not a resample.
  expect_identical(
    capped$columnspec_errors$full_error$row,
    utils::head(full$columnspec_errors$full_error$row, 2L)
  )

  # A cap of zero still reports the same counts; it just keeps nothing.
  none <- validate_table_detailed(specs, table, verbose = FALSE, max_errors = 0L)
  expect_identical(none$n_columnspec_errors, full$n_columnspec_errors)
  expect_identical(nrow(none$columnspec_errors$full_error), 0L)

  # The default retains everything, exactly as before the parameter existed.
  expect_identical(
    validate_table_detailed(specs, table, verbose = FALSE),
    full
  )
  expect_null(attr(full$columnspec_errors$full_error, "truncated"))
})


test_that("an unusable max_errors is refused rather than read as no cap", {
  # `NA`, a negative number, a length-2 vector and a string were all read as
  # "keep everything": a caller who computed the cap wrongly got the unbounded
  # frame the argument exists to prevent, and got it without a word.
  errors <- data.frame(
    row = 1:5,
    column = "ID",
    stringsAsFactors = FALSE
  )

  for (bad in list(NA, NA_integer_, c(1, 2), -1, "3")) {
    expect_error(
      dta_truncate_error_frame(errors, bad),
      class = "rlang_error",
      regexp = "single non-negative number"
    )
  }

  # An unusable cap is refused whatever the frame is, so the report does not
  # depend on the input happening to have enough rows to reach the cap.
  expect_error(
    dta_truncate_error_frame(NULL, NA),
    class = "rlang_error",
    regexp = "single non-negative number"
  )

  # The two deliberate ways to say "no cap" still say it.
  expect_identical(dta_truncate_error_frame(errors, NULL), errors)
  expect_identical(dta_truncate_error_frame(errors, Inf), errors)
  # And a real cap still caps, including the boundary value zero.
  expect_identical(nrow(dta_truncate_error_frame(errors, 0)), 0L)
  expect_identical(nrow(dta_truncate_error_frame(errors, 2L)), 2L)
})


test_that("a capped import frame still reports the exact error count", {
  # dta_import_error_count() reads a frame-level attribute, and `[` drops it.
  # A truncated frame that lost it would report the rows it kept.
  specs <- DTAColumnSpecCollection(
    columns = list(VAL = DTAColumnSpec(id = "VAL", type = "SAS Num", nullable = TRUE))
  )
  table <- dta_coerce_table_to_specs(
    data.frame(VAL = rep("nope", 6), stringsAsFactors = FALSE),
    specs
  )$table

  capped <- validate_table_detailed(specs, table, verbose = FALSE, max_errors = 2L)

  expect_identical(nrow(capped$import_errors), 2L)
  expect_equal(dta_import_error_count(capped$import_errors), 6)
  expect_equal(capped$n_import_errors, 6L)
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

  columnspec_errors <- validate_table(specs = specs, table = table, verbose = FALSE)
  full_error <- as.data.frame(columnspec_errors$full_error)

  expect_equal(nrow(full_error), 1)
  expect_equal(full_error$row, 5001)
  expect_equal(full_error$column, "ID")
  expect_equal(full_error$keyword, "maxLength")
})
