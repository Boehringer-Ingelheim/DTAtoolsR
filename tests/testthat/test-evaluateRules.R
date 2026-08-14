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

test_that("apply_rules handles canonical and legacy rule types", {
  df <- data.frame(AGE = c(20, 30), SUBJECT_ID = c("A", "B"), stringsAsFactors = FALSE)

  canonical_rules <- list(
    DTARuleColRange(id = "r1", columns = "AGE", min = 18, max = 65),
    DTARuleColUnique(id = "u1", columns = "SUBJECT_ID")
  )

  result <- apply_rules(canonical_rules, df, verbose = FALSE)
  expect_true(all(vapply(result, function(x) isTRUE(x$valid), logical(1))))

  mixed_rule <- DTARuleColCondition(
    id = "c1",
    condition = list(AGE = list(greater_equal = 20)),
    then = list(SUBJECT_ID = list(not_equals = ""))
  )
  result <- apply_rules(list(mixed_rule), df, verbose = FALSE)
  expect_true(isTRUE(result[[1]]$valid))

  legacy_rule <- DTARuleColRange(id = "legacy", columns = "AGE", min = 18, max = 65)
  legacy_rule@type <- "col_range"
  result <- apply_rules(list(legacy_rule), df, verbose = FALSE)
  expect_true(isTRUE(result[[1]]$valid))

  grouped <- DTARuleGroupCondition(
    id = "g1",
    group_by = "SUBJECT_ID",
    conditions = list(ok = list(AGE = list(greater_equal = 18))),
    constraints = list(list(type = "requires", `if` = "ok", then = "ok"))
  )
  result <- apply_rules(list(grouped), df, verbose = FALSE)
  expect_true(isTRUE(result[[1]]$valid))

  grouped_legacy <- grouped
  grouped_legacy@type <- "group_condition"
  result <- apply_rules(list(grouped_legacy), df, verbose = FALSE)
  expect_true(isTRUE(result[[1]]$valid))
})

test_that("group_condition detects mutually exclusive and requires violations by group", {
  df <- data.frame(
    SUBJIDN = c("S1", "S1", "S2", "S2"),
    GFREFID = c("R1", "R1", "R1", "R1"),
    VISIT = c("V1", "V1", "V1", "V1"),
    GFREASND = c("FAILED", "", "FAILED", ""),
    GFORRES = c(NA, 12, NA, NA),
    GFSTAT = c("DONE", "DONE", "NOT DONE", "NOT DONE"),
    stringsAsFactors = FALSE
  )

  rule <- DTARuleGroupCondition(
    id = "sample_visit_status_logic",
    group_by = c("SUBJIDN", "GFREFID", "VISIT"),
    conditions = list(
      c1_failed = list(GFREASND = list(empty = FALSE)),
      c2_reported = list(GFREASND = list(empty = TRUE), GFORRES = list(empty = FALSE)),
      c3_not_done = list(GFSTAT = list(equals = "NOT DONE"))
    ),
    constraints = list(
      list(type = "mutually_exclusive", left = "c1_failed", right = "c2_reported"),
      list(type = "requires", `if` = "c1_failed", then = "c3_not_done", then_scope = "all")
    )
  )

  res <- rule_check_group_condition(rule, df)
  expect_false(res$valid)
  expect_match(res$message, "sample_visit_status_logic", fixed = TRUE)
  expect_match(res$message, "SUBJIDN=S1", fixed = TRUE)
  expect_match(res$message, "must not both occur", fixed = TRUE)
  expect_match(res$message, "must also hold, but it does not", fixed = TRUE)
  expect_match(res$message, "c1_failed", fixed = TRUE)

  passing <- df
  passing$GFSTAT[passing$SUBJIDN == "S1"] <- "NOT DONE"
  passing$GFORRES[passing$SUBJIDN == "S1"] <- NA

  ok <- rule_check_group_condition(rule, passing)
  expect_true(ok$valid)
  expect_null(ok$message)
})

test_that("group_condition evaluates rows where group_by contains NA", {
  df <- data.frame(
    SUBJIDN = c("S1", "S1"),
    VISIT = c(NA, NA),
    GFREASND = c("FAILED", ""),
    GFORRES = c(NA, 10),
    stringsAsFactors = FALSE
  )

  rule <- DTARuleGroupCondition(
    id = "na_group_key_rule",
    group_by = c("SUBJIDN", "VISIT"),
    conditions = list(
      c1 = list(GFREASND = list(empty = FALSE)),
      c2 = list(GFREASND = list(empty = TRUE), GFORRES = list(empty = FALSE))
    ),
    constraints = list(list(type = "mutually_exclusive", left = "c1", right = "c2"))
  )

  res <- rule_check_group_condition(rule, df)
  expect_false(res$valid)
  expect_match(res$message, "VISIT=NA", fixed = TRUE)
})

test_that("group_condition accepts alias constraint names", {
  rule <- DTARuleGroupCondition(
    id = "alias_constraints",
    group_by = "SUBJECT_ID",
    conditions = list(
      c1 = list(STATUS = list(equals = "FAILED")),
      c2 = list(RESULT = list(empty = FALSE))
    ),
    constraints = list(
      list(type = "not_both", left = "c1", right = "c2"),
      list(type = "implies", `if` = "c1", then = "c2")
    )
  )

  expect_equal(rule@constraints[[1]]$type, "mutually_exclusive")
  expect_equal(rule@constraints[[2]]$type, "requires")
})

test_that("group_condition reports unknown condition references with context", {
  expect_error(
    DTARuleGroupCondition(
      id = "group_condition_unknown_condition",
      group_by = "SUBJECT_ID",
      conditions = list(c1 = list(STATUS = list(equals = "FAILED"))),
      constraints = list(list(type = "requires", `if` = "c1", then = "c_missing"))
    ),
    "unknown condition"
  )
  expect_error(
    DTARuleGroupCondition(
      id = "group_condition_unknown_condition",
      group_by = "SUBJECT_ID",
      conditions = list(c1 = list(STATUS = list(equals = "FAILED"))),
      constraints = list(list(type = "requires", `if` = "c1", then = "c_missing"))
    ),
    "Defined condition"
  )
})

test_that("group_condition missing group_by columns explains available columns", {
  df <- data.frame(SUBJECT_ID = "S1", STATUS = "FAILED", stringsAsFactors = FALSE)
  rule <- DTARuleGroupCondition(
    id = "group_missing_group_col",
    group_by = c("SUBJECT_ID", "VISIT"),
    conditions = list(c1 = list(STATUS = list(equals = "FAILED"))),
    constraints = list(list(type = "requires", `if` = "c1", then = "c1"))
  )

  expect_error(
    rule_check_group_condition(rule, df),
    "Available columns"
  )

  wrapped <- apply_rules(list(rule), df, verbose = FALSE)
  expect_false(wrapped[[1]]$valid)
  expect_match(wrapped[[1]]$message, "could not be evaluated", fixed = TRUE)
  expect_match(wrapped[[1]]$message, "Available columns", fixed = TRUE)
})

test_that("group_condition grouping key does not collide on separator-like values", {
  sep_like <- "\u001f"
  df <- data.frame(
    A = c(paste0("x", sep_like, "y"), paste0("x", sep_like, "y"), "x", "x"),
    B = c("z", "z", paste0("y", sep_like, "z"), paste0("y", sep_like, "z")),
    FLAG = c(1, 1, 2, 2),
    stringsAsFactors = FALSE
  )

  rule <- DTARuleGroupCondition(
    id = "group_key_collision_guard",
    group_by = c("A", "B"),
    conditions = list(
      c1 = list(FLAG = list(equals = 1)),
      c2 = list(FLAG = list(equals = 2))
    ),
    constraints = list(list(type = "mutually_exclusive", left = "c1", right = "c2"))
  )

  res <- rule_check_group_condition(rule, df)
  expect_true(res$valid)
  expect_null(res$message)
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

test_that("dta_as_numeric_strict separates missing from unconvertible", {
  # The whole point of the helper: `as.numeric()` collapses these into one NA.
  converted <- dta_as_numeric_strict(c("30", "ninety", NA, "", "  ", "1.50", "007"))

  expect_equal(converted$values, c(30, NA, NA, NA, NA, 1.5, 7))
  expect_equal(converted$missing, c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(
    converted$unconvertible,
    c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
  # The raw source text is kept verbatim, so an import error can quote it.
  expect_equal(converted$raw[[2]], "ninety")

  # Owner decision: a value that converts but changes representation is a clean
  # conversion, not an import error. "007" -> 7 and "1.50" -> 1.5 stay clean.
  expect_false(any(converted$unconvertible[c(6, 7)]))

  # Factors go via as.character(), never via their integer level codes.
  factor_converted <- dta_as_numeric_strict(factor(c("500", "600", "700")))
  expect_equal(factor_converted$values, c(500, 600, 700))
  expect_false(any(factor_converted$unconvertible))

  factor_labels <- dta_as_numeric_strict(factor(c("high", "12")))
  expect_equal(factor_labels$unconvertible, c(TRUE, FALSE))
  expect_equal(factor_labels$raw, c("high", "12"))

  # Numeric and logical columns are already numbers; nothing is unconvertible.
  expect_false(any(dta_as_numeric_strict(c(1, NA, 3))$unconvertible))
  expect_true(dta_as_numeric_strict(c(1, NA, 3))$missing[[2]])
})

test_that("Range rules read factor labels, not factor level codes", {
  # factor(c("500","600","700")) has level codes 1, 2, 3. `as.numeric()` on the
  # factor returned those codes, so every value sat inside [0, 100] and the rule
  # reported VALID on data that is an order of magnitude out of range.
  result <- rule_check_range(
    DTARuleColRange(id = "factor_range", columns = "AGE", min = 0, max = 100),
    data.frame(AGE = factor(c("500", "600", "700")))
  )

  expect_false(result$valid)
  expect_match(result$message, "violated: 3 rows")

  # The labels are real numbers, so this is a rule violation and NOT an import
  # error: nothing here is unrecoverable.
  import_errors <- dta_rule_import_errors(
    DTARuleColRange(id = "factor_range", columns = "AGE", min = 0, max = 100),
    data.frame(AGE = factor(c("500", "600", "700")))
  )
  expect_equal(nrow(import_errors), 0)

  # A factor level that is not a number is unrecoverable, and reported.
  label_errors <- dta_rule_import_errors(
    DTARuleColRange(id = "factor_range", columns = "AGE", min = 0, max = 100),
    data.frame(AGE = factor(c("500", "high", "700")))
  )
  expect_equal(nrow(label_errors), 1)
  expect_equal(label_errors$raw, "high")
  expect_equal(label_errors$reason, "not_convertible")
})

test_that("Range rules treat an unconvertible value as a violation, not a pass", {
  df <- data.frame(AGE = c("ninety", "N/A", ">65"), stringsAsFactors = FALSE)

  # `as.numeric()` made all three NA and `any(violated, na.rm = TRUE)` then
  # dropped them, so a column with no usable value at all reported VALID.
  result <- rule_check_range(
    DTARuleColRange(id = "unconvertible", columns = "AGE", min = 18, max = 65),
    df
  )
  expect_false(result$valid)
  expect_match(result$message, "violated: 3 rows")

  # Both axes: the same three values are also import errors, carrying the raw
  # text verbatim.
  import_errors <- dta_rule_import_errors(
    DTARuleColRange(id = "unconvertible", columns = "AGE", min = 18, max = 65),
    df
  )
  expect_equal(nrow(import_errors), 3)
  expect_equal(import_errors$raw, c("ninety", "N/A", ">65"))
  expect_equal(import_errors$row, 1:3)
  expect_true(all(import_errors$column == "AGE"))
  expect_true(all(import_errors$reason == "not_convertible"))
})

test_that("CANARY: a genuinely missing value is never an import error", {
  # If this flips, the implementation is wrong: NA in the source is missing
  # data, not a value that failed to convert.
  rule <- DTARuleColRange(id = "range_na", columns = "AGE", min = 18, max = 65)
  df <- data.frame(AGE = c(NA, NA), stringsAsFactors = FALSE)

  expect_true(rule_check_range(rule, df)$valid)
  expect_equal(nrow(dta_rule_import_errors(rule, df)), 0)

  # Empty and whitespace-only strings are missing too, not unconvertible.
  blank <- data.frame(AGE = c("", "   ", NA), stringsAsFactors = FALSE)
  expect_true(rule_check_range(rule, blank)$valid)
  expect_equal(nrow(dta_rule_import_errors(rule, blank)), 0)
})

test_that("Numeric comparisons compare numbers, not collated text", {
  # The escape: on a character column R coerced the BOUND to character, and
  # under locale collation "9" sorts after "65", so AGE = "9" passed
  # `greater: 65` and the rule reported VALID with zero violated rows.
  df <- data.frame(
    VISIT = c("V03", "V03"),
    AGE = c("9", "700"),
    stringsAsFactors = FALSE
  )

  result <- rule_check_col_condition(
    DTARuleColCondition(
      id = "collation_greater",
      condition = list(VISIT = list(equals = "V03")),
      then = list(AGE = list(greater = 65))
    ),
    df
  )
  expect_false(result$valid)
  expect_match(result$message, "violated: 1 rows")

  # The same values held as numbers must agree, on every comparison operator.
  numeric_df <- df
  numeric_df$AGE <- as.numeric(df$AGE)

  for (clause in list(
    list(greater = 65),
    list(less = 100),
    list(greater_equal = 65),
    list(less_equal = 100),
    list(range = c(65, 1000)),
    list(min = 65, max = 1000)
  )) {
    character_result <- rule_check_col_condition(
      DTARuleColCondition(
        id = "collation_case",
        condition = list(VISIT = list(equals = "V03")),
        then = list(AGE = clause)
      ),
      df
    )
    numeric_result <- rule_check_col_condition(
      DTARuleColCondition(
        id = "collation_case",
        condition = list(VISIT = list(equals = "V03")),
        then = list(AGE = clause)
      ),
      numeric_df
    )
    expect_identical(
      character_result$valid,
      numeric_result$valid,
      info = paste("operator:", paste(names(clause), collapse = "/"))
    )
    expect_identical(character_result$message, numeric_result$message)
  }
})

test_that("pattern and the equality/set operators still compare the raw value", {
  # Only the numeric comparisons changed. These operators must keep matching
  # text, and must not be dragged through numeric coercion.
  df <- data.frame(
    VISIT = c("V03", "V03"),
    CODE = c("007", "9"),
    stringsAsFactors = FALSE
  )

  # "007" == 7 numerically, but equals compares the value as written.
  expect_false(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "equals_raw",
        condition = list(VISIT = list(equals = "V03")),
        then = list(CODE = list(equals = "7"))
      ),
      df
    )$valid
  )
  expect_true(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "in_raw",
        condition = list(VISIT = list(equals = "V03")),
        then = list(CODE = list(`in` = c("007", "9")))
      ),
      df
    )$valid
  )
  expect_true(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "pattern_raw",
        condition = list(VISIT = list(equals = "V03")),
        then = list(CODE = list(pattern = "^[0-9]+$"))
      ),
      df
    )$valid
  )

  # A non-numeric column tested only with text operators reports no import
  # errors: it is never read as a number.
  expect_equal(
    nrow(dta_rule_import_errors(
      DTARuleColCondition(
        id = "pattern_raw",
        condition = list(VISIT = list(equals = "V03")),
        then = list(CODE = list(pattern = "^[0-9]+$"))
      ),
      df
    )),
    0
  )
})

test_that("dta_rule_numeric_columns names only the numerically compared columns", {
  expect_equal(
    dta_rule_numeric_columns(
      DTARuleColRange(id = "r", columns = "AGE", min = 0, max = 1)
    ),
    "AGE"
  )
  expect_equal(
    dta_rule_numeric_columns(DTARuleColUnique(id = "u", columns = "ID")),
    character(0)
  )
  expect_equal(
    dta_rule_numeric_columns(
      DTARuleColCondition(
        id = "c",
        condition = list(VISIT = list(equals = "V03"), AGE = list(greater = 1)),
        then = list(STATUS = list(pattern = "^C"), WEIGHT = list(min = 1, max = 2))
      )
    ),
    c("AGE", "WEIGHT")
  )
})

test_that("apply_rules reports import errors alongside the rule verdict", {
  df <- data.frame(AGE = c("30", "ninety", "700"), stringsAsFactors = FALSE)

  results <- apply_rules(
    list(DTARuleColRange(id = "age_range", columns = "AGE", min = 18, max = 65)),
    df,
    verbose = FALSE
  )

  expect_false(results[[1]]$valid)
  # Both rows are rule violations: the unconvertible one and the out-of-range
  # one. Reclassifying "ninety" as an import error ALONE would make a consumer
  # reading n_rule_errors see fewer errors than before.
  expect_match(results[[1]]$message, "violated: 2 rows")
  expect_equal(nrow(results[[1]]$import_errors), 1)
  expect_equal(results[[1]]$import_errors$raw, "ninety")

  # A rule that cannot be evaluated contributes no import errors.
  absent <- apply_rules(
    list(DTARuleColRange(id = "absent", columns = "MISSING", min = 0, max = 1)),
    df,
    verbose = FALSE
  )
  expect_false(absent[[1]]$valid)
  expect_equal(nrow(absent[[1]]$import_errors), 0)
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

test_that("apply_rules returns empty list for empty rule set", {
  result <- apply_rules(list(), data.frame(A = 1), verbose = FALSE)
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

test_that("Unsupported condition operators abort via cli, naming column and key", {
  err <- expect_error(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "bogus",
        condition = list(VISIT = list(equals = "V03")),
        then = list(AGE = list(bogus_op = 1))
      ),
      data.frame(VISIT = "V03", AGE = 1, stringsAsFactors = FALSE)
    ),
    class = "rlang_error"
  )
  # The abort must identify both the column and the offending key, otherwise a
  # typo in a large DTS is untraceable.
  expect_match(conditionMessage(err), "bogus_op")
  expect_match(conditionMessage(err), "AGE")

  # An unknown operator sitting next to a valid one must still abort; it must
  # never be quietly skipped because another operator was recognised first.
  expect_error(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "bogus_mixed",
        condition = list(VISIT = list(equals = "V03")),
        then = list(AGE = list(greater = 0, bogus_op = 1))
      ),
      data.frame(VISIT = "V03", AGE = 1, stringsAsFactors = FALSE)
    ),
    class = "rlang_error"
  )
})

test_that("Every operator supplied for a column is evaluated and AND-combined", {
  df <- data.frame(
    VISIT = c("V03", "V03"),
    AGE = c(200, 999),
    stringsAsFactors = FALSE
  )

  # Two operators, both satisfied.
  both_ok <- rule_check_col_condition(
    DTARuleColCondition(
      id = "multi_ok",
      condition = list(AGE = list(greater_equal = 0)),
      then = list(AGE = list(greater = 18, less = 1000))
    ),
    df
  )
  expect_true(both_ok$valid)
  expect_null(both_ok$message)

  # Two operators where only the SECOND is violated. The old if/else chain
  # return()ed on `greater`, so `less` was never evaluated and AGE = 200/999
  # passed an 18..65 band.
  second_violated <- rule_check_col_condition(
    DTARuleColCondition(
      id = "multi_second_violated",
      condition = list(AGE = list(greater_equal = 0)),
      then = list(AGE = list(greater = 18, less = 65))
    ),
    df
  )
  expect_false(second_violated$valid)
  expect_match(second_violated$message, "violated: 2 rows")

  # Three operators, only the last one fires.
  three_ops <- rule_check_col_condition(
    DTARuleColCondition(
      id = "multi_three",
      condition = list(VISIT = list(equals = "V03")),
      then = list(AGE = list(greater = 18, less = 1000, not_equals = 999))
    ),
    df
  )
  expect_false(three_ops$valid)
  expect_match(three_ops$message, "violated: 1 rows")

  # Several operators across several THEN columns are all enforced.
  multi_column <- rule_check_col_condition(
    DTARuleColCondition(
      id = "multi_column",
      condition = list(VISIT = list(equals = "V03")),
      then = list(
        AGE = list(greater = 18, less = 1000),
        VISIT = list(pattern = "^V", not_equals = "V03")
      )
    ),
    df
  )
  expect_false(multi_column$valid)
  expect_match(multi_column$message, "violated: 2 rows")
})

test_that("min and max still express one inclusive band", {
  df <- data.frame(
    VISIT = c("V03", "V03"),
    AGE = c(200, 999),
    stringsAsFactors = FALSE
  )

  band_ok <- rule_check_col_condition(
    DTARuleColCondition(
      id = "band_ok",
      condition = list(VISIT = list(equals = "V03")),
      then = list(AGE = list(min = 200, max = 999))
    ),
    df
  )
  expect_true(band_ok$valid)

  band_violated <- rule_check_col_condition(
    DTARuleColCondition(
      id = "band_violated",
      condition = list(VISIT = list(equals = "V03")),
      then = list(AGE = list(min = 0, max = 500))
    ),
    df
  )
  expect_false(band_violated$valid)
  expect_match(band_violated$message, "violated: 1 rows")

  # A lone `min` is still an open-ended lower bound, and combines with other
  # operators rather than shadowing them.
  min_only <- rule_check_col_condition(
    DTARuleColCondition(
      id = "min_plus_other",
      condition = list(VISIT = list(equals = "V03")),
      then = list(AGE = list(min = 0, less = 500))
    ),
    df
  )
  expect_false(min_only$valid)
  expect_match(min_only$message, "violated: 1 rows")
})

test_that("Conditions written as a YAML sequence are normalised to named form", {
  # A YAML sequence under `condition:` parses to an UNNAMED list -- this is the
  # exact shape a DTS author produces by writing `- VISIT:` under `condition:`.
  parsed <- yaml::yaml.load("condition:\n  - VISIT:\n      equals: V03\n")
  expect_null(names(parsed$condition))

  df <- data.frame(
    VISIT = c("V03", "EOT"),
    AGE = c(10, 80),
    stringsAsFactors = FALSE
  )

  seq_rule <- DTARuleColCondition(
    id = "seq_condition",
    condition = parsed$condition,
    then = list(AGE = list(less = 5))
  )
  # The constructor stores the canonical named form.
  expect_equal(seq_rule@condition, list(VISIT = list(equals = "V03")))

  # Previously names(conditions) was NULL, lapply() returned list(), Reduce()
  # returned NULL and the rule reported VALID no matter what the data held.
  result <- rule_check_col_condition(seq_rule, df)
  expect_false(result$valid)
  expect_match(result$message, "violated: 1 rows")

  # Sequences work for `then` as well, and across several entries.
  multi_seq <- DTARuleColCondition(
    id = "seq_then",
    condition = list(list(VISIT = list(equals = "V03")), list(AGE = list(less = 50))),
    then = list(list(AGE = list(greater = 100)))
  )
  expect_equal(names(multi_seq@condition), c("VISIT", "AGE"))
  expect_equal(names(multi_seq@then), "AGE")
  expect_false(rule_check_col_condition(multi_seq, df)$valid)

  # The engine normalises too, so a rule built by any other route is safe.
  expect_equal(
    evaluate_conditions(list(list(VISIT = list(equals = "V03"))), df),
    c(TRUE, FALSE)
  )
})

test_that("Malformed conditions abort with a clear message, never 'invalid argument type'", {
  df <- data.frame(VISIT = "V03", AGE = 5, stringsAsFactors = FALSE)

  chr_err <- expect_error(
    DTARuleColCondition(
      id = "chr_condition",
      condition = "VISIT == V03",
      then = list(AGE = list(less = 5))
    ),
    class = "rlang_error"
  )
  expect_match(conditionMessage(chr_err), "condition")
  expect_false(grepl("invalid argument", conditionMessage(chr_err), fixed = TRUE))

  engine_err <- expect_error(
    evaluate_conditions("VISIT == V03", df),
    class = "rlang_error"
  )
  expect_false(grepl("invalid argument", conditionMessage(engine_err), fixed = TRUE))

  # A sequence entry that does not name its column is ambiguous, not silently
  # ignored.
  expect_error(
    DTARuleColCondition(
      id = "unnamed_entry",
      condition = list(list(equals = "V03")),
      then = list(AGE = list(less = 5))
    ),
    class = "rlang_error"
  )

  # An empty operator map must abort, not wave every row through.
  expect_error(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "empty_ops",
        condition = list(VISIT = list(equals = "V03")),
        then = list(AGE = list())
      ),
      df
    ),
    class = "rlang_error"
  )

  # The same column named twice is ambiguous: one of the two would be lost.
  expect_error(
    DTARuleColCondition(
      id = "dup_column",
      condition = list(
        list(VISIT = list(equals = "V03")),
        list(VISIT = list(equals = "EOT"))
      ),
      then = list(AGE = list(less = 5))
    ),
    class = "rlang_error"
  )
})

test_that("A rule naming an absent column fails the rule instead of aborting the run", {
  df <- data.frame(ID = "A", stringsAsFactors = FALSE)

  range_results <- apply_rules(
    list(DTARuleColRange(id = "age_range", columns = "AGE", min = 0, max = 120)),
    df,
    verbose = FALSE
  )
  expect_length(range_results, 1)
  expect_false(range_results[[1]]$valid)
  expect_equal(range_results[[1]]$id, "age_range")
  expect_match(range_results[[1]]$message, "age_range")
  expect_match(range_results[[1]]$message, "AGE")

  unique_results <- apply_rules(
    list(DTARuleColUnique(id = "unique_subjid", columns = "SUBJID")),
    df,
    verbose = FALSE
  )
  expect_false(unique_results[[1]]$valid)
  expect_match(unique_results[[1]]$message, "unique_subjid")
  expect_match(unique_results[[1]]$message, "SUBJID")

  condition_results <- apply_rules(
    list(DTARuleColCondition(
      id = "visit_rule",
      condition = list(VISIT = list(equals = "V03")),
      then = list(ID = list(empty = FALSE))
    )),
    df,
    verbose = FALSE
  )
  expect_false(condition_results[[1]]$valid)
  expect_match(condition_results[[1]]$message, "visit_rule")
  expect_match(condition_results[[1]]$message, "VISIT")

  # One unevaluable rule must not hide the verdict of the others.
  mixed <- apply_rules(
    list(
      DTARuleColRange(id = "age_range", columns = "AGE", min = 0, max = 120),
      DTARuleColUnique(id = "unique_id", columns = "ID")
    ),
    df,
    verbose = FALSE
  )
  expect_length(mixed, 2)
  expect_false(mixed[[1]]$valid)
  expect_true(mixed[[2]]$valid)

  # Precision check: a malformed rule is a specification error, not an
  # unevaluable one, and must still abort rather than be reported as a failure.
  expect_error(
    apply_rules(
      list(DTARuleColRange(id = "multi_col", columns = c("A", "B"), min = 0, max = 1)),
      data.frame(A = 1, B = 2),
      verbose = FALSE
    ),
    "exactly one"
  )
})

test_that("an unconvertible IF operand keeps the row in scope", {
  rule <- DTARuleColCondition(
    id = "age_status",
    condition = list(AGE = list(greater = 18)),
    then = list(STATUS = list(equals = "adult"))
  )

  # "ninety-five" is a data error, not an exemption. The THEN clause
  # definitively fails, so this row is a violation rather than a row the rule
  # quietly skipped because it could not evaluate the IF.
  unconvertible <- data.frame(
    AGE = "ninety-five", STATUS = "minor", stringsAsFactors = FALSE
  )
  expect_false(rule_check_col_condition(rule, unconvertible)$valid)

  # An unconvertible IF whose THEN does hold is still not a violation: the
  # row is in scope, and it passes.
  passing <- data.frame(
    AGE = "ninety-five", STATUS = "adult", stringsAsFactors = FALSE
  )
  expect_true(rule_check_col_condition(rule, passing)$valid)

  # A MISSING IF operand says nothing about the row, so the rule does not
  # apply to it and a failing THEN is not reported.
  for (empty in list(NA_character_, "", "  ")) {
    df <- data.frame(AGE = empty, STATUS = "minor", stringsAsFactors = FALSE)
    expect_true(rule_check_col_condition(rule, df)$valid)
  }
})

test_that("the streaming and materialising conditional paths agree on scope", {
  rule <- DTARuleColCondition(
    id = "age_status",
    condition = list(AGE = list(greater = 18)),
    then = list(STATUS = list(equals = "adult"))
  )
  df <- data.frame(
    AGE = c("ninety-five", "20", NA, "5"),
    STATUS = c("minor", "minor", "minor", "minor"),
    stringsAsFactors = FALSE
  )

  violated <- dta_condition_violated(rule, df)
  expect_false(anyNA(violated))
  expect_equal(which(violated), c(1L, 2L))
})
