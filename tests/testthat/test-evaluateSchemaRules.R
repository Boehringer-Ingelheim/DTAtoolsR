test_that("Range rules evaluate inclusive bounds and ignore missing values", {
  test_df <- data.frame(
    AGE = c(25, 70, NA, 65),
    stringsAsFactors = FALSE
  )

  result <- rule_check_range(
    DTARuleColRange(
      id = "rule_range_error",
      columns = "AGE",
      range = c(18, 65)
    ),
    test_df
  )
  expect_false(result$valid)
  expect_match(result$message, "not in range")
  # Only AGE = 70 violates: NA is ignored and 65 sits on the inclusive bound.
  expect_match(result$message, "violated: 1 rows")

  result <- rule_check_range(
    DTARuleColRange(
      id = "rule_range_success",
      columns = "AGE",
      range = c(18, 70)
    ),
    test_df
  )
  expect_true(result$valid)
  expect_null(result$message)
})

test_that("Range rule bounds are inclusive at both ends", {
  on_bounds <- rule_check_range(
    DTARuleColRange(id = "on_bounds", columns = "AGE", min = 18, max = 65),
    data.frame(AGE = c(18, 65), stringsAsFactors = FALSE)
  )
  expect_true(on_bounds$valid)
  expect_null(on_bounds$message)

  just_outside <- rule_check_range(
    DTARuleColRange(id = "just_outside", columns = "AGE", min = 18, max = 65),
    data.frame(AGE = c(17.999, 65.001), stringsAsFactors = FALSE)
  )
  expect_false(just_outside$valid)
  expect_match(just_outside$message, "violated: 2 rows")
})

test_that("Range rules reject malformed bounds and missing columns", {
  expect_error(
    DTARuleColRange(id = "bad_range", columns = "AGE", range = c(18, 65, 70)),
    "numeric vector of length 2"
  )
  expect_error(
    rule_check_range(
      DTARuleColRange(id = "bad_column", columns = "WEIGHT", min = 1, max = 2),
      data.frame(AGE = 1)
    ),
    "not found in table"
  )
})

test_that("Unique rules detect duplicate combinations and accept unique ones", {
  test_df <- data.frame(
    AGE = c(25, 70, 25, 65),
    SUBJECT_ID = c(1, 2, 1, 3),
    VISIT = c("V03", "EOT", "V03", "V05"),
    GFSEQID = c("KRAS", "KRAS", "MYC", "KRAS"),
    stringsAsFactors = FALSE
  )

  result <- rule_check_unique(
    DTARuleColUnique(
      id = "rule_unique_error",
      columns = c("SUBJECT_ID", "VISIT")
    ),
    test_df
  )
  expect_false(result$valid)
  expect_match(result$message, "duplicate row")
  # Rows 1 and 3 are the same (SUBJECT_ID, VISIT) pair => 1 duplicate.
  expect_match(result$message, "violated: 1 duplicate")
  expect_match(result$message, "SUBJECT_ID, VISIT")

  # The count is "rows beyond the first", not "rows involved": 3x"A" => 2.
  repeated <- rule_check_unique(
    DTARuleColUnique(id = "u", columns = "ID"),
    data.frame(ID = c("A", "A", "A", "B"), stringsAsFactors = FALSE)
  )
  expect_false(repeated$valid)
  expect_match(repeated$message, "violated: 2 duplicate")

  result <- rule_check_unique(
    DTARuleColUnique(
      id = "rule_unique_success",
      columns = c("SUBJECT_ID", "GFSEQID", "VISIT")
    ),
    test_df
  )
  expect_true(result$valid)
  expect_null(result$message)

  expect_error(
    rule_check_unique(
      DTARuleColUnique(id = "missing_cols", columns = c("SUBJECT_ID", "MISSING")),
      test_df
    ),
    "not found in table"
  )
})

test_that("DTARuleColCondition and DTARuleColUnique constructors validate inputs", {
  expect_error(DTARuleColCondition(id = "bad id", condition = list(A = list(equals = 1)), then = list(B = list(equals = 2))), "whitespaces")
  expect_error(DTARuleColCondition(id = "rule", condition = NULL, then = list(B = list(equals = 2))), "non-empty")
  expect_error(DTARuleColCondition(id = "rule", condition = list(A = list(equals = 1)), then = NULL), "non-empty")

  expect_error(DTARuleColUnique(id = "bad id", columns = "SUBJID"), "whitespaces")
  expect_error(DTARuleColUnique(id = "rule", columns = NULL), "non-empty")
  expect_error(DTARuleColUnique(id = "rule", columns = 1), "character vector")
})

test_that("create_example_DTARule helpers create runnable rules", {
  rule <- create_example_DTARuleColCondition()
  expect_true(methods::is(rule, "DTAtools::DTARuleColCondition"))
  expect_equal(rule@id, "rule3")
  expect_equal(rule@type, "check_col_condition")
  expect_equal(rule@condition, list(age = list(equals = 18)))
  expect_equal(rule@then, list(status = list(equals = "adult")))

  range_rule <- create_example_DTARuleColRange()
  expect_true(methods::is(range_rule, "DTAtools::DTARuleColRange"))
  expect_equal(range_rule@id, "check_age_range")
  expect_equal(range_rule@type, "check_range")
  expect_equal(range_rule@columns, "AGE")
  expect_equal(range_rule@min, 18)
  expect_equal(range_rule@max, 65)

  unique_rule <- create_example_DTARuleColUnique(index = 2)
  expect_true(methods::is(unique_rule, "DTAtools::DTARuleColUnique"))
  expect_equal(unique_rule@columns, c("SUBJID", "VISIT"))

  expect_error(create_example_DTARuleColUnique(index = 99), "No example found")
  expect_error(create_example_DTARuleColRange(99), "No example found with index 99")
  expect_error(create_example_DTARuleColCondition(2), "Invalid index: 2")
})

test_that("The example rules actually run against a matching data frame", {
  range_rule <- create_example_DTARuleColRange()
  expect_true(rule_check_range(range_rule, data.frame(AGE = c(18, 40, 65)))$valid)
  expect_false(rule_check_range(range_rule, data.frame(AGE = c(17, 40)))$valid)

  condition_rule <- create_example_DTARuleColCondition()
  passing <- data.frame(age = c(18, 21), status = c("adult", "minor"), stringsAsFactors = FALSE)
  failing <- data.frame(age = c(18, 21), status = c("minor", "minor"), stringsAsFactors = FALSE)
  expect_true(rule_check_col_condition(condition_rule, passing)$valid)
  expect_false(rule_check_col_condition(condition_rule, failing)$valid)
})

test_that("Conditional rules evaluate equals, not_equals, in, not_in, range and empty operators", {
  test_df <- data.frame(
    VISIT = c("V03", "EOT", "V03", "V05"),
    STATUS = c("COMPLETED", "DROPPED", "IN_PROGRESS", "COMPLETED"),
    CONSENT = c("YES", "NO", "YES", ""),
    CONSENT_DATE = c("2023-01-01", "2023-02-02", "2023-01-02", NA),
    STUDYID = c("1234-5678", "1234-5678", "5678-1234", "1234-5678"),
    DOMAIN = c("GF", "GF", "GF", "GF"),
    AGE = c(25, 70, 25, 65),
    WEIGHT = c(15, 4, 15, 20),
    stringsAsFactors = FALSE
  )

  expect_false(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "rule_equal_example",
        condition = list(VISIT = list(equals = "V03")),
        then = list(STATUS = list(equals = "COMPLETED"))
      ),
      test_df
    )$valid
  )

  expect_true(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "rule_not_equal_example",
        condition = list(VISIT = list(equals = "V03")),
        then = list(STATUS = list(not_equals = "DROPPED"))
      ),
      test_df
    )$valid
  )

  expect_true(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "rule_in_example",
        condition = list(VISIT = list(equals = "V03")),
        then = list(STATUS = list(`in` = c("COMPLETED", "IN_PROGRESS")))
      ),
      test_df
    )$valid
  )

  expect_false(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "rule_not_in_example",
        condition = list(VISIT = list(equals = "V03")),
        then = list(STATUS = list(not_in = "COMPLETED"))
      ),
      test_df
    )$valid
  )

  expect_true(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "rule_range_example",
        condition = list(VISIT = list(equals = "V03")),
        then = list(AGE = list(range = c(10, 100)))
      ),
      test_df
    )$valid
  )

  expect_true(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "rule_empty_false_example",
        condition = list(CONSENT = list(equals = "YES")),
        then = list(CONSENT_DATE = list(empty = FALSE))
      ),
      test_df
    )$valid
  )

  expect_false(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "rule_empty_true_example",
        condition = list(CONSENT = list(equals = "YES")),
        then = list(CONSENT_DATE = list(empty = TRUE))
      ),
      test_df
    )$valid
  )

  expect_true(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "rule_greater_equal_example",
        condition = list(VISIT = list(equals = "V03")),
        then = list(WEIGHT = list(greater_equal = 5))
      ),
      test_df
    )$valid
  )

  expect_true(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "rule_pattern_pass_example",
        condition = list(VISIT = list(equals = "V03")),
        then = list(STUDYID = list(pattern = "^[0-9]{4}-[0-9]{4}$"))
      ),
      test_df
    )$valid
  )

  expect_false(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "rule_pattern_fail_example",
        condition = list(VISIT = list(equals = "V03")),
        then = list(STUDYID = list(pattern = "^ZZZ"))
      ),
      test_df
    )$valid
  )
})

test_that("Conditional empty=false handles Date columns", {
  test_df <- data.frame(
    CONSENT = c("YES", "YES", "NO", "YES"),
    stringsAsFactors = FALSE
  )
  test_df$CONSENT_DATE <- as.Date(c("2023-01-01", "2023-01-02", NA, NA))

  result <- rule_check_col_condition(
    DTARuleColCondition(
      id = "rule_empty_false_date",
      condition = list(CONSENT = list(equals = "YES")),
      then = list(CONSENT_DATE = list(empty = FALSE))
    ),
    test_df
  )

  expect_false(result$valid)
  expect_match(result$message, "1 rows failed")

  passing_df <- test_df
  passing_df$CONSENT_DATE[4] <- as.Date("2023-01-04")

  passing_result <- rule_check_col_condition(
    DTARuleColCondition(
      id = "rule_empty_false_date_ok",
      condition = list(CONSENT = list(equals = "YES")),
      then = list(CONSENT_DATE = list(empty = FALSE))
    ),
    passing_df
  )

  expect_true(passing_result$valid)
  expect_null(passing_result$message)
})

test_that("apply_schema_rules handles canonical and legacy rule types", {
  df <- data.frame(AGE = c(20, 30), SUBJECT_ID = c("A", "B"), stringsAsFactors = FALSE)

  canonical_rules <- list(
    DTARuleColRange(id = "r1", columns = "AGE", min = 18, max = 65),
    DTARuleColUnique(id = "u1", columns = "SUBJECT_ID")
  )

  result <- apply_schema_rules(canonical_rules, df, verbose = FALSE)
  expect_true(all(vapply(result, function(x) isTRUE(x$valid), logical(1))))

  mixed_rule <- DTARuleColCondition(
    id = "c1",
    condition = list(AGE = list(greater_equal = 20)),
    then = list(SUBJECT_ID = list(not_equals = ""))
  )
  result <- apply_schema_rules(list(mixed_rule), df, verbose = FALSE)
  expect_true(isTRUE(result[[1]]$valid))

  legacy_rule <- DTARuleColRange(id = "legacy", columns = "AGE", min = 18, max = 65)
  legacy_rule@type <- "col_range"
  result <- apply_schema_rules(list(legacy_rule), df, verbose = FALSE)
  expect_true(isTRUE(result[[1]]$valid))
})

test_that("Range rules support min/max slots and reject multi-column usage", {
  df <- data.frame(AGE = c(20, 40, NA), stringsAsFactors = FALSE)

  ok <- rule_check_range(
    DTARuleColRange(id = "range_ok", columns = "AGE", min = 18, max = 65),
    df
  )
  expect_true(ok$valid)
  expect_null(ok$message)

  all_na <- rule_check_range(
    DTARuleColRange(id = "range_na", columns = "AGE", min = 18, max = 65),
    data.frame(AGE = c(NA, NA), stringsAsFactors = FALSE)
  )
  expect_true(all_na$valid)

  expect_error(
    rule_check_range(
      DTARuleColRange(id = "range_multi", columns = c("AGE", "WEIGHT"), min = 0, max = 100),
      data.frame(AGE = 1, WEIGHT = 2)
    ),
    "exactly one"
  )
})

test_that("Conditional rules support comparison operators and combined IF predicates", {
  df <- data.frame(
    VISIT = c("V03", "V03", "EOT"),
    STATUS = c("COMPLETED", "IN_PROGRESS", "DROPPED"),
    AGE = c(25, 30, 80),
    WEIGHT = c(80, 55, 40),
    stringsAsFactors = FALSE
  )

  expect_true(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "rule_greater",
        condition = list(VISIT = list(equals = "V03")),
        then = list(WEIGHT = list(greater = 50))
      ),
      df
    )$valid
  )

  expect_true(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "rule_less",
        condition = list(VISIT = list(equals = "EOT")),
        then = list(AGE = list(less = 100))
      ),
      df
    )$valid
  )

  expect_true(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "rule_less_equal",
        condition = list(VISIT = list(equals = "V03"), STATUS = list(not_equals = "DROPPED")),
        then = list(AGE = list(less_equal = 30))
      ),
      df
    )$valid
  )

  expect_true(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "rule_not_in_vector",
        condition = list(VISIT = list(equals = "V03")),
        then = list(STATUS = list(not_in = c("DROPPED", "FAILED")))
      ),
      df
    )$valid
  )
})

test_that("Comparison operators also detect violations", {
  # Same shape as the passing fixture above, but each row now breaks the
  # operator under test, so a branch stubbed out with TRUE would fail here.
  df <- data.frame(
    VISIT = c("V03", "V03", "EOT"),
    STATUS = c("COMPLETED", "DROPPED", "DROPPED"),
    AGE = c(25, 130, 80),
    WEIGHT = c(80, 20, 40),
    stringsAsFactors = FALSE
  )

  greater <- rule_check_col_condition(
    DTARuleColCondition(
      id = "rule_greater_violated",
      condition = list(VISIT = list(equals = "V03")),
      then = list(WEIGHT = list(greater = 50))
    ),
    df
  )
  expect_false(greater$valid)
  expect_match(greater$message, "violated: 1 rows")

  less <- rule_check_col_condition(
    DTARuleColCondition(
      id = "rule_less_violated",
      condition = list(VISIT = list(equals = "EOT")),
      then = list(AGE = list(less = 50))
    ),
    df
  )
  expect_false(less$valid)
  expect_match(less$message, "violated: 1 rows")

  less_equal <- rule_check_col_condition(
    DTARuleColCondition(
      id = "rule_less_equal_violated",
      condition = list(VISIT = list(equals = "V03")),
      then = list(AGE = list(less_equal = 30))
    ),
    df
  )
  expect_false(less_equal$valid)
  expect_match(less_equal$message, "violated: 1 rows")

  not_in_vector <- rule_check_col_condition(
    DTARuleColCondition(
      id = "rule_not_in_vector_violated",
      condition = list(VISIT = list(equals = "V03")),
      then = list(STATUS = list(not_in = c("DROPPED", "FAILED")))
    ),
    df
  )
  expect_false(not_in_vector$valid)
  expect_match(not_in_vector$message, "violated: 1 rows")
})

test_that("Conditional rules error for missing columns", {
  df <- data.frame(AGE = c(20, 30), stringsAsFactors = FALSE)

  expect_error(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "missing_if_col",
        condition = list(MISSING = list(equals = "X")),
        then = list(AGE = list(greater_equal = 18))
      ),
      df
    ),
    "Column not found in table: MISSING"
  )

  expect_error(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "missing_then_col",
        condition = list(AGE = list(greater_equal = 18)),
        then = list(MISSING = list(not_equals = "X"))
      ),
      df
    ),
    "Column not found in table: MISSING"
  )
})

test_that("Unique rules treat repeated NA combinations as duplicates", {
  df <- data.frame(ID = c("A", "B", "C"), VISIT = c(NA, NA, "V03"), stringsAsFactors = FALSE)

  result <- rule_check_unique(
    DTARuleColUnique(id = "na_dupes", columns = c("VISIT")),
    df
  )

  expect_false(result$valid)
  expect_match(result$message, "duplicate row")
  # Two NAs in VISIT => the second one counts as the single duplicate.
  expect_match(result$message, "violated: 1 duplicate")
})

test_that("apply_schema_rules returns empty list for empty rule set", {
  result <- apply_schema_rules(list(), data.frame(A = 1), verbose = FALSE)
  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("Rule checkers reject objects that are not DTARule instances", {
  df <- data.frame(AGE = c(20, 30), ID = c("A", "B"), stringsAsFactors = FALSE)
  not_a_rule <- list(id = "fake", columns = "AGE", min = 18, max = 65)

  expect_error(rule_check_range(not_a_rule, df), "Rule is not of class 'DTARule'")
  expect_error(rule_check_unique(not_a_rule, df), "Rule is not of class 'DTARule'")
  expect_error(rule_check_col_condition(not_a_rule, df), "Rule is not of class 'DTARule'")

  expect_error(rule_check_range(NULL, df), "Rule is not of class 'DTARule'")
  expect_error(rule_check_range("AGE", df), "Rule is not of class 'DTARule'")
})

test_that("validate_rules passes clean tables and aborts on rule violations", {
  specs <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 12, nullable = FALSE)
    ),
    rules = list(DTARuleColUnique(id = "unique_id", columns = "ID"))
  )

  results <- suppressMessages(
    validate_rules(specs, data.frame(ID = c("A001", "A002"), stringsAsFactors = FALSE))
  )
  expect_type(results, "list")
  expect_length(results, 1)
  expect_equal(results[[1]]$id, "unique_id")
  expect_true(results[[1]]$valid)
  expect_null(results[[1]]$message)

  expect_error(
    suppressMessages(
      validate_rules(specs, data.frame(ID = c("A001", "A001"), stringsAsFactors = FALSE))
    ),
    "Rule 'unique_id' violated: 1 duplicate row"
  )
})

test_that("validate_rules returns an empty result when the collection has no rules", {
  specs <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "SAS Char", length = 12, nullable = FALSE)
    )
  )

  results <- suppressMessages(
    validate_rules(specs, data.frame(ID = "A001", stringsAsFactors = FALSE))
  )
  expect_type(results, "list")
  expect_length(results, 0)
})

test_that("Unique rules compare values verbatim (pinned, not endorsed)", {
  # Pins current behaviour: uniqueness is case- and whitespace-sensitive, so
  # "A001", "a001" and "A001 " are three distinct values. Recorded so that a
  # future move to normalised comparison is a deliberate change, not a silent
  # one -- this is not an endorsement of the current semantics.
  result <- rule_check_unique(
    DTARuleColUnique(id = "verbatim", columns = "ID"),
    data.frame(ID = c("A001", "a001", "A001 "), stringsAsFactors = FALSE)
  )

  expect_true(result$valid)
  expect_null(result$message)
})

test_that("Unsupported condition operators abort (pinned, not endorsed)", {
  # Pins current behaviour: this path uses base stop(), not cli::cli_abort(), so
  # the condition is a plain simpleError rather than an rlang_error. Pinned so a
  # future conversion to cli_abort() is noticed; the repo convention is cli.
  err <- expect_error(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "bogus",
        condition = list(VISIT = list(equals = "V03")),
        then = list(AGE = list(bogus_op = 1))
      ),
      data.frame(VISIT = "V03", AGE = 1, stringsAsFactors = FALSE)
    ),
    "Unsupported condition type for column 'AGE'"
  )
  expect_s3_class(err, "simpleError")
})
