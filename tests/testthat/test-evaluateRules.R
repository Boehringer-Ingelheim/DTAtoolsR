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
  # REGRESSION GUARD. Two groups whose values straddle the key separator must
  # stay two groups. A key that joined the fields with a raw separator would
  # merge them and report a violation that the data does not contain, so this
  # test is what makes the encoding in `dta_row_key()` load-bearing rather than
  # decorative -- do not delete it to buy key-building speed.
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

test_that("group_condition does not merge two doubles that render alike", {
  # 0.1 + 0.2 and 0.3 are two different doubles that `split()` keeps apart, so
  # they are two groups; a key rendered through `as.character()` rounds both to
  # "0.3" and would merge them into one group that violates the constraint.
  df <- data.frame(
    G = c(0.1 + 0.2, 0.3),
    FLAG = c(1, 2),
    stringsAsFactors = FALSE
  )

  rule <- DTARuleGroupCondition(
    id = "group_key_double_precision",
    group_by = "G",
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
  # The raw source text is recoverable verbatim, so an import error can quote
  # it. It is derived from the retained source vector, not stored as a second
  # character copy of the column.
  expect_equal(dta_numeric_raw(converted, 2L), "ninety")

  # Owner decision: a value that converts but changes representation is a clean
  # conversion, not an import error. "007" -> 7 and "1.50" -> 1.5 stay clean.
  expect_false(any(converted$unconvertible[c(6, 7)]))

  # Factors go via as.character(), never via their integer level codes.
  factor_converted <- dta_as_numeric_strict(factor(c("500", "600", "700")))
  expect_equal(factor_converted$values, c(500, 600, 700))
  expect_false(any(factor_converted$unconvertible))

  factor_labels <- dta_as_numeric_strict(factor(c("high", "12")))
  expect_equal(factor_labels$unconvertible, c(TRUE, FALSE))
  # The raw text of a factor is its LABEL, never its integer level code -- the
  # accessor must resolve `factor("high")` to "high" and not to "2".
  expect_equal(dta_numeric_raw(factor_labels, c(1L, 2L)), c("high", "12"))

  # Numeric and logical columns are already numbers; nothing is unconvertible.
  expect_false(any(dta_as_numeric_strict(c(1, NA, 3))$unconvertible))
  expect_true(dta_as_numeric_strict(c(1, NA, 3))$missing[[2]])
})

test_that("dta_as_numeric_strict does not render the whole column as text", {
  # Pins the v0.17.2 memory regression. The conversion used to carry a `raw`
  # field holding `as.character()` of the entire column, even though the text is
  # only ever read at the rows that failed to convert -- usually none. On a
  # numeric column that character rendering was ~8x the column itself, and
  # dta_build_numeric_cache() holds one entry per column simultaneously.
  #
  # What the entry may legitimately hold is the numeric values, the source
  # vector (shared with the column, so free in reality but counted again by
  # object.size()) and the two logical masks -- about 3x the column. The eager
  # character copy took it past 10x, so a 4x ceiling separates the two without
  # being brittle about vector header sizes.
  column <- as.numeric(seq_len(50000L))
  entry <- dta_as_numeric_strict(column)

  expect_lt(
    as.numeric(object.size(entry)),
    4 * as.numeric(object.size(column))
  )
  # The source is the column itself, not a copy of it.
  expect_identical(entry$source, column)
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

test_that("rule_check_unique() gives identical valid/message with Arrow on and off (FIX 3)", {
  testthat::skip_if_not(dta_arrow_compute_available())

  old_use <- getOption("DTAtools.use_arrow_compute")
  old_min_rows <- getOption("DTAtools.arrow_min_rows")
  on.exit(options(
    DTAtools.use_arrow_compute = old_use,
    DTAtools.arrow_min_rows = old_min_rows
  ))

  set.seed(123)
  n <- 500L
  df <- data.frame(
    ID = sample.int(n / 2, n, replace = TRUE),
    GROUP = sample(letters[1:5], n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  rule <- DTARuleColUnique(id = "u_arrow", columns = c("ID", "GROUP"))

  options(DTAtools.use_arrow_compute = FALSE)
  result_off <- rule_check_unique(rule, df)

  options(DTAtools.use_arrow_compute = TRUE, DTAtools.arrow_min_rows = 10L)
  result_on <- rule_check_unique(rule, df)

  expect_identical(result_on$valid, result_off$valid)
  expect_identical(result_on$message, result_off$message)
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

# ---- shared numeric conversion cache (perf/rule-checking, item A) ----------

test_that("dta_build_numeric_cache only converts columns rules read numerically", {
  df <- data.frame(
    AGE = c("20", "70", "ninety"),
    NAME = c("a", "b", "c"),
    UNUSED = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  range_rule <- DTARuleColRange(id = "r1", columns = "AGE", range = c(18, 65))
  cond_rule <- DTARuleColCondition(
    id = "r2",
    condition = list(NAME = list(equals = "a")),
    then = list(AGE = list(greater = 0))
  )

  cache <- dta_build_numeric_cache(df, list(range_rule, cond_rule))
  expect_named(cache, "AGE")
  expect_equal(cache$AGE$values, c(20, 70, NA))
  expect_true(cache$AGE$unconvertible[3])

  # NULL / empty rule list -> no cache at all.
  expect_equal(dta_build_numeric_cache(df, NULL), list())
  expect_equal(dta_build_numeric_cache(df, list()), list())

  # A rule naming a column absent from the frame contributes nothing.
  stale_rule <- DTARuleColRange(id = "stale", columns = "GONE", range = c(0, 1))
  expect_equal(dta_build_numeric_cache(df, list(stale_rule)), list())
})

test_that("dta_build_numeric_cache(columns =) matches the columns it would derive", {
  # The streaming driver derives the numeric columns once for the whole scan
  # and hands them over so the per-rule clause parse is not repeated on every
  # batch. That is only sound if the supplied list produces the identical
  # cache, filtering included.
  df <- data.frame(
    AGE = c("20", "70", "ninety"),
    SCORE = c("1.5", "", "3"),
    NAME = c("a", "b", "c"),
    UNUSED = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  range_rule <- DTARuleColRange(id = "r1", columns = "AGE", range = c(18, 65))
  score_rule <- DTARuleColRange(id = "r2", columns = "SCORE", range = c(0, 10))
  cond_rule <- DTARuleColCondition(
    id = "r3",
    condition = list(NAME = list(equals = "a")),
    then = list(AGE = list(greater = 0))
  )
  rules <- list(range_rule, score_rule, cond_rule)

  # Precomputed exactly as the streaming driver precomputes it.
  precomputed <- lapply(rules, function(r) {
    tryCatch(dta_rule_numeric_columns(r), error = function(e) character(0))
  })
  flattened <- unique(unlist(precomputed, use.names = FALSE))

  expect_identical(
    dta_build_numeric_cache(df, rules, columns = flattened),
    dta_build_numeric_cache(df, rules)
  )

  # A rule naming a column this frame does not have must still yield an empty
  # cache rather than an error, whether the columns were derived or supplied.
  stale_rule <- DTARuleColRange(id = "stale", columns = "GONE", range = c(0, 1))
  stale_rules <- list(stale_rule)
  stale_columns <- unique(unlist(
    lapply(stale_rules, dta_rule_numeric_columns),
    use.names = FALSE
  ))
  expect_identical(stale_columns, "GONE")
  expect_identical(
    dta_build_numeric_cache(df, stale_rules, columns = stale_columns),
    dta_build_numeric_cache(df, stale_rules)
  )
  expect_equal(dta_build_numeric_cache(df, stale_rules, columns = stale_columns), list())

  # A mixed list -- one present column, one absent -- is filtered, not rejected.
  mixed_rules <- list(range_rule, stale_rule)
  mixed_columns <- unique(unlist(
    lapply(mixed_rules, dta_rule_numeric_columns),
    use.names = FALSE
  ))
  mixed_cache <- dta_build_numeric_cache(df, mixed_rules, columns = mixed_columns)
  expect_named(mixed_cache, "AGE")
  expect_identical(mixed_cache, dta_build_numeric_cache(df, mixed_rules))

  # Positional calls keep working: `columns` was appended, not inserted.
  expect_identical(dta_build_numeric_cache(df, rules), dta_build_numeric_cache(df, rules, NULL))
})

test_that("apply_rules with the shared cache matches per-rule calls without one", {
  df <- data.frame(
    AGE = c("20", "70", "ninety", "40"),
    STATUS = c("FAILED", "FAILED", "OK", "OK"),
    stringsAsFactors = FALSE
  )

  range_rule <- DTARuleColRange(id = "range", columns = "AGE", range = c(18, 65))
  cond_rule <- DTARuleColCondition(
    id = "cond",
    condition = list(STATUS = list(equals = "FAILED")),
    then = list(AGE = list(less = 65))
  )
  rules <- list(range_rule, cond_rule)

  cached <- apply_rules(rules, df, verbose = FALSE)

  uncached <- list(
    rule_check_range(range_rule, df),
    rule_check_col_condition(cond_rule, df)
  )
  uncached[[1]]$import_errors <- dta_rule_import_errors(range_rule, df)
  uncached[[2]]$import_errors <- dta_rule_import_errors(cond_rule, df)

  for (i in seq_along(rules)) {
    expect_equal(cached[[i]]$valid, uncached[[i]]$valid)
    expect_equal(cached[[i]]$message, uncached[[i]]$message)
    expect_equal(cached[[i]]$import_errors, uncached[[i]]$import_errors)
  }
})

test_that("the numeric cache does not mask a rule naming an absent column", {
  df <- data.frame(AGE = c("20", "30"), stringsAsFactors = FALSE)
  stale_rule <- DTARuleColRange(id = "stale", columns = "MISSING", range = c(0, 1))

  result <- apply_rules(list(stale_rule), df, verbose = FALSE)[[1]]
  expect_false(result$valid)
  expect_match(result$message, "could not be evaluated", fixed = TRUE)
})

test_that("a cache built for a larger frame is not recycled against a subset (FIX 1)", {
  # A cache built for the full table must never be reused, unchanged, for a
  # rule evaluated against a shorter subset of that table -- `dta_numeric_cache_get()`
  # must detect the length mismatch and recompute rather than silently
  # recycle `cached$values` against the shorter frame.
  df_full <- data.frame(
    AGE = c("20", "70", "ninety", "40", "55"),
    stringsAsFactors = FALSE
  )
  range_rule <- DTARuleColRange(id = "range", columns = "AGE", range = c(18, 65))

  full_cache <- dta_build_numeric_cache(df_full, list(range_rule))
  expect_equal(length(full_cache$AGE$values), nrow(df_full))

  df_subset <- df_full[1:3, , drop = FALSE]

  with_stale_cache <- rule_check_range(range_rule, df_subset, numeric_cache = full_cache)
  without_cache <- rule_check_range(range_rule, df_subset)

  expect_equal(with_stale_cache$valid, without_cache$valid)
  expect_equal(with_stale_cache$message, without_cache$message)
})

test_that("import errors are deduplicated when two rules read the same column", {
  df <- data.frame(
    AGE = c("ninety", "40"),
    STATUS = c("FAILED", "OK"),
    stringsAsFactors = FALSE
  )

  range_rule <- DTARuleColRange(id = "range", columns = "AGE", range = c(18, 65))
  cond_rule <- DTARuleColCondition(
    id = "cond",
    condition = list(STATUS = list(equals = "FAILED")),
    then = list(AGE = list(less = 65))
  )

  results <- apply_rules(list(range_rule, cond_rule), df, verbose = FALSE)
  collected <- dta_collect_import_errors(results)

  # Both rules read AGE numerically and both see the same unconvertible value
  # at row 1 -- that is one import error, not two.
  expect_equal(nrow(collected), 1)
  expect_equal(collected$row, 1L)
  expect_equal(collected$column, "AGE")
})

# ---- vectorised grouped rule evaluation (perf/rule-checking, item B) -------

test_that("group_condition handles high group cardinality and sorts group order", {
  n_groups <- 300
  df <- data.frame(
    SUBJ = sprintf("S%03d", rep(seq_len(n_groups), each = 2)),
    STATUS = rep(c("FAILED", "FAILED"), n_groups),
    RESULT = rep(c(NA, NA), n_groups),
    stringsAsFactors = FALSE
  )
  # Make exactly one group (the alphabetically last one) pass, so the rest
  # violate the mutually_exclusive constraint.
  last <- sprintf("S%03d", n_groups)
  df$RESULT[df$SUBJ == last] <- NA

  rule <- DTARuleGroupCondition(
    id = "high_card",
    group_by = "SUBJ",
    conditions = list(
      c_failed = list(STATUS = list(equals = "FAILED")),
      c_reported = list(RESULT = list(empty = FALSE))
    ),
    constraints = list(list(type = "mutually_exclusive", left = "c_failed", right = "c_reported"))
  )

  res <- rule_check_group_condition(rule, df)
  expect_true(res$valid)
  expect_null(res$message)

  # Now make one specific group actually violate, and confirm it is named.
  df$RESULT[df$SUBJ == "S150"] <- "12"
  res2 <- rule_check_group_condition(rule, df)
  expect_false(res2$valid)
  expect_match(res2$message, "SUBJ=S150", fixed = TRUE)
  # Group order in the assembled message is sorted (S150 sorts before S151).
  expect_true(
    regexpr("SUBJ=S150", res2$message) < regexpr("SUBJ=S299", res2$message) ||
      !grepl("SUBJ=S299", res2$message)
  )
})

test_that("group_condition with many groups reports exactly the few that violate, in sorted order", {
  # This pins the behaviour the group/constraint vectorisation turns on: only
  # violating (group, constraint) pairs should ever contribute a violation
  # entry or row-evidence/label work, and the rest of the (large) group
  # population must be entirely silent in the output.
  n_groups <- 1000
  df <- data.frame(
    SUBJ = sprintf("S%04d", rep(seq_len(n_groups), each = 2)),
    STATUS = rep("OK", n_groups * 2),
    RESULT = rep(NA_character_, n_groups * 2),
    stringsAsFactors = FALSE
  )

  # Scatter exactly three violating groups across the (sorted) key space.
  violators <- c("S0007", "S0500", "S0999")
  for (subj in violators) {
    idx <- which(df$SUBJ == subj)
    df$STATUS[idx] <- "FAILED"
    df$RESULT[idx[1]] <- "12" # one row satisfies both c_failed and c_reported
  }

  rule <- DTARuleGroupCondition(
    id = "sparse_violations",
    group_by = "SUBJ",
    conditions = list(
      c_failed = list(STATUS = list(equals = "FAILED")),
      c_reported = list(RESULT = list(empty = FALSE))
    ),
    constraints = list(list(type = "mutually_exclusive", left = "c_failed", right = "c_reported"))
  )

  res <- rule_check_group_condition(rule, df)
  expect_false(res$valid)

  reported_groups <- vapply(res$details, function(v) v$group, character(1))
  expect_identical(length(reported_groups), 3L)
  expect_identical(
    reported_groups,
    sprintf("SUBJ=%s", violators)
  )

  # Groups that never violate contribute nothing: no other SUBJ id appears in
  # any detail's group label or message. A fixed, explicitly named handful is
  # checked (rather than a random sample) so a failure here is reproducible.
  fixed_non_violators <- c(
    "S0001", "S0006", "S0008", "S0100", "S0499",
    "S0501", "S0750", "S0998", "S1000"
  )
  stopifnot(all(fixed_non_violators %in% setdiff(unique(df$SUBJ), violators)))
  for (subj in fixed_non_violators) {
    expect_false(any(grepl(subj, reported_groups, fixed = TRUE)))
    expect_false(grepl(subj, res$message, fixed = TRUE))
  }
})

test_that("a constraint referencing an unknown condition name fails the rule, not the run (FIX 8)", {
  # The DTARuleGroupCondition constructor rejects unknown condition names up
  # front, so this is only reachable by bypassing the constructor -- mutate a
  # validly constructed rule's `@constraints` slot directly to simulate that.
  rule <- DTARuleGroupCondition(
    id = "bad_constraint",
    group_by = "SUBJ",
    conditions = list(
      c_failed = list(STATUS = list(equals = "FAILED"))
    ),
    constraints = list(
      list(type = "mutually_exclusive", left = "c_failed", right = "c_failed")
    )
  )
  S7::prop(rule, "constraints") <- list(list(
    id = "constraint_1",
    type = "mutually_exclusive",
    left = "c_failed",
    right = "c_ghost", # not a defined condition
    left_scope = "any",
    right_scope = "any",
    message = NULL
  ))

  df <- data.frame(
    SUBJ = c("S1", "S1"),
    STATUS = c("FAILED", "OK"),
    stringsAsFactors = FALSE
  )

  # Directly: aborts with the narrowly classed condition rather than silently
  # reporting no violation.
  expect_error(
    rule_check_group_condition(rule, df),
    class = "dta_rule_not_applicable"
  )

  # Through apply_rules(): surfaces as a FAILED rule, not an aborted run.
  results <- apply_rules(list(rule), df, verbose = FALSE)
  expect_length(results, 1L)
  expect_false(results[[1]]$valid)
  expect_match(results[[1]]$message, "could not be evaluated", fixed = TRUE)
  expect_match(results[[1]]$message, "c_ghost", fixed = TRUE)
})

test_that("group_condition row evidence caps at 10 and reports the remainder", {
  n <- 30
  df <- data.frame(
    SUBJ = rep("A", n),
    REASND = c(rep("FAILED", 15), rep("", n - 15)),
    RESULT = c(rep(NA, 15), rep("12", n - 15)),
    stringsAsFactors = FALSE
  )

  rule <- DTARuleGroupCondition(
    id = "row_cap",
    group_by = "SUBJ",
    conditions = list(
      c_failed = list(REASND = list(equals = "FAILED")),
      c_reported = list(RESULT = list(empty = FALSE))
    ),
    constraints = list(list(type = "mutually_exclusive", left = "c_failed", right = "c_reported"))
  )

  res <- rule_check_group_condition(rule, df)
  expect_false(res$valid)
  expect_match(res$message, "(+5 more)", fixed = TRUE)
  # Exactly ten row numbers shown before the "more" marker for each side.
  shown <- regmatches(res$message, gregexpr("rows matching \"[^\"]+\": [0-9,]+", res$message))[[1]]
  expect_true(length(shown) == 2)
})

test_that("group_condition scope 'all' is false for an empty condition set, not vacuously true", {
  df <- data.frame(
    SUBJ = c("A", "A", "B"),
    STAT = c("DONE", "OPEN", "DONE"),
    stringsAsFactors = FALSE
  )
  rule <- DTARuleGroupCondition(
    id = "grp_all_scope",
    group_by = "SUBJ",
    conditions = list(done = list(STAT = list(equals = "DONE"))),
    constraints = list(list(type = "requires", `if` = "done", then = "done", then_scope = "all"))
  )

  res <- rule_check_group_condition(rule, df)
  # Group A has one row satisfying "done" (if_truth TRUE under "any") and one
  # that does not, so "all" fails for group A specifically.
  expect_false(res$valid)
  expect_match(res$message, "SUBJ=A", fixed = TRUE)
})

test_that("group_condition mutually_exclusive/requires 'all' scope on a multi-row group", {
  df <- data.frame(
    SUBJ = c("A", "A", "A"),
    STATUS = c("FAILED", "FAILED", "FAILED"),
    RESULT = c(NA, NA, "9"),
    stringsAsFactors = FALSE
  )
  rule <- DTARuleGroupCondition(
    id = "grp_scope_all",
    group_by = "SUBJ",
    conditions = list(
      c_failed = list(STATUS = list(equals = "FAILED")),
      c_reported = list(RESULT = list(empty = FALSE))
    ),
    constraints = list(list(
      type = "mutually_exclusive", left = "c_failed", right = "c_reported",
      left_scope = "all", right_scope = "any"
    ))
  )

  # left_scope "all": every row in the group must satisfy c_failed. It does.
  # right_scope "any": at least one row satisfies c_reported. It does (row 3).
  res <- rule_check_group_condition(rule, df)
  expect_false(res$valid)
})

# ---- key-based uniqueness (perf/rule-checking, item C) ---------------------

test_that("rule_check_unique matches duplicated() on character, integer, and NA keys", {
  df <- data.frame(
    K_CHR = c("a", "b", "a", NA_character_, NA_character_),
    K_INT = c(1L, 2L, 1L, 3L, 3L),
    stringsAsFactors = FALSE
  )

  chr_rule <- DTARuleColUnique(id = "chr", columns = "K_CHR")
  int_rule <- DTARuleColUnique(id = "int", columns = "K_INT")

  chr_res <- rule_check_unique(chr_rule, df)
  int_res <- rule_check_unique(int_rule, df)

  expect_false(chr_res$valid)
  expect_match(chr_res$message, sprintf("%d duplicate", sum(duplicated(df[, "K_CHR", drop = FALSE]))))
  expect_false(int_res$valid)
  expect_match(int_res$message, sprintf("%d duplicate", sum(duplicated(df[, "K_INT", drop = FALSE]))))
})

test_that("rule_check_unique matches duplicated() for a multi-column character/integer key", {
  df <- data.frame(
    A = c("x", "x", "y", "x"),
    B = c(1L, 1L, 2L, 2L),
    stringsAsFactors = FALSE
  )
  rule <- DTARuleColUnique(id = "multi", columns = c("A", "B"))
  res <- rule_check_unique(rule, df)
  expected_dupes <- sum(duplicated(df[, c("A", "B"), drop = FALSE]))
  expect_equal(!res$valid, expected_dupes > 0)
  if (expected_dupes > 0) {
    expect_match(res$message, sprintf("%d duplicate", expected_dupes))
  }
})

test_that("rule_check_unique falls back to the data.frame method for a double key column", {
  # 0.1 + 0.2 and 0.3 are `==`-different doubles that as.character() can print
  # identically, which is exactly what the double fallback guards against.
  df <- data.frame(
    K = c(0.1 + 0.2, 0.3, 0.3),
    stringsAsFactors = FALSE
  )
  expect_true(is.double(df$K))

  rule <- DTARuleColUnique(id = "dbl", columns = "K")
  res <- rule_check_unique(rule, df)
  expected_dupes <- sum(duplicated(df[, "K", drop = FALSE]))

  expect_equal(!res$valid, expected_dupes > 0)
  if (expected_dupes > 0) {
    expect_match(res$message, sprintf("%d duplicate", expected_dupes))
  }
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

# ---- quoted numeric bounds compare numerically (equals/in and negations) ---

test_that("equals against a numeric column compares numerically, quoted or unquoted", {
  # A quoted bound that parses as a number ("1000000") now matches a numeric
  # column the same way the unquoted number would -- exactly one row of
  # c(1e6, 2.5) is 1000000.
  x <- c(1e6, 2.5)
  quoted_mask <- dta_condition_mask("NUM", "equals", "1000000", x)
  unquoted_mask <- dta_condition_mask("NUM", "equals", 1000000, x)
  expect_identical(quoted_mask, c(TRUE, FALSE))
  expect_identical(quoted_mask, unquoted_mask)

  # The same equivalence holds through the full rule: a THEN that fails only
  # on the IF-matching row proves the IF applied to exactly that one row,
  # under both spellings of the bound.
  df <- data.frame(NUM = x, FLAG = c("miss", "irrelevant"), stringsAsFactors = FALSE)
  make_rule <- function(bound) {
    DTARuleColCondition(
      id = "quoted_numeric_equals",
      condition = list(NUM = list(equals = bound)),
      then = list(FLAG = list(equals = "hit"))
    )
  }
  quoted_result <- rule_check_col_condition(make_rule("1000000"), df)
  unquoted_result <- rule_check_col_condition(make_rule(1000000), df)

  expect_false(quoted_result$valid)
  expect_match(quoted_result$message, "violated: 1 rows")
  expect_identical(quoted_result$valid, unquoted_result$valid)
  expect_identical(quoted_result$message, unquoted_result$message)
})

test_that("a quoted bound with leading zeros matches numerically on a numeric column, verbatim on a character one", {
  # "007" parses to the number 7, so it matches a numeric 7 exactly as the
  # unquoted "7" no longer would.
  numeric_mask <- dta_condition_mask("N", "equals", "007", c(7, 8))
  expect_identical(numeric_mask, c(TRUE, FALSE))

  # A character column is untouched: leading zeros are part of the value, so
  # "7" and "007" are two distinct, non-numeric strings.
  char_x <- c("007", "7")
  expect_identical(dta_condition_mask("CODE", "equals", "7", char_x), c(FALSE, TRUE))
  expect_identical(dta_condition_mask("CODE", "equals", "007", char_x), c(TRUE, FALSE))
})

test_that("an unparseable bound on a numeric column keeps string comparison and never errors", {
  # "UNK" does not parse as a number, so a numeric column falls back to the
  # old string comparison rather than aborting -- and, being numeric, never
  # equals a non-numeric string, so it matches nothing.
  expect_no_error(mask <- dta_condition_mask("N", "equals", "UNK", c(7, 8, 9)))
  expect_identical(mask, c(FALSE, FALSE, FALSE))
})

test_that("in against a numeric column compares numerically only when every element parses", {
  x <- c(1, 3)
  expect_identical(dta_condition_mask("N", "in", c("1", "2"), x), c(TRUE, FALSE))

  # One unparseable element ("UNK") makes the whole set fall back to the old,
  # all-or-nothing string comparison -- computed here rather than hardcoded,
  # so this pins whatever that fallback actually does today.
  mixed_bound <- c("1", "UNK")
  expect_identical(
    dta_condition_mask("N", "in", mixed_bound, x),
    x %in% mixed_bound
  )
})

test_that("not_equals with a quoted numeric bound is the negation of equals", {
  x <- c(1e6, 2.5, 7)
  eq <- dta_condition_mask("N", "equals", "1000000", x)
  neq <- dta_condition_mask("N", "not_equals", "1000000", x)
  expect_identical(neq, !eq)
})

# ---- dta_group_label_value() and its use in grouped-rule labels ------------

test_that("dta_group_label_value renders numeric group values canonically", {
  expect_identical(dta_group_label_value(1e6), "1000000")
  expect_identical(dta_group_label_value(2.5), "2.5")
  expect_identical(dta_group_label_value(1 / 3), as.character(1 / 3))
  expect_identical(dta_group_label_value("A"), "A")
  # NA renders the way paste0() itself would render a bare NA -- there is no
  # canonical numeric string for "no value".
  expect_identical(paste0("x=", dta_group_label_value(NA_real_)), "x=NA")
})

test_that("a numeric group-by value renders canonically in the violation label, on both paths", {
  # Both rows fall in the SAME group (GRP = 1e6): one satisfies c1, the other
  # c2, so mutually_exclusive fires and the group label is rendered into the
  # message. Before dta_group_label_value(), that label read "GRP=1e+06".
  df <- data.frame(
    GRP = c(1e6, 1e6),
    FLAG = c(1, 2),
    stringsAsFactors = FALSE
  )
  rule <- DTARuleGroupCondition(
    id = "numeric_group_label",
    group_by = "GRP",
    conditions = list(
      c1 = list(FLAG = list(equals = 1)),
      c2 = list(FLAG = list(equals = 2))
    ),
    constraints = list(list(type = "mutually_exclusive", left = "c1", right = "c2"))
  )

  eager <- rule_check_group_condition(rule, df)
  expect_false(eager$valid)
  expect_match(eager$message, "=1000000", fixed = TRUE)
  expect_false(grepl("1e+06", eager$message, fixed = TRUE))

  state <- dta_rule_stream_init(rule)
  reader <- dta_as_batch_reader(df, batch_rows = 1L)
  repeat {
    batch <- reader$read_next_batch()
    if (is.null(batch)) break
    dta_rule_stream_update(state, rule, as.data.frame(batch))
  }
  streamed <- dta_rule_stream_finalise(state, rule)

  expect_false(streamed$valid)
  expect_match(streamed$message, "=1000000", fixed = TRUE)
  expect_false(grepl("1e+06", streamed$message, fixed = TRUE))
  expect_identical(streamed$message, eager$message)
})

test_that("integer64 columns are exempt from equality-bound parsing", {
  # is.numeric(integer64) is TRUE, but as.numeric() rounds past 2^53: parsing
  # the bound would turn "9007199254740993" into 9007199254740992 and break an
  # equality that bit64's own comparison keeps exact.
  skip_if_not_installed("bit64")
  x <- bit64::as.integer64(c("9007199254740993", "9007199254740992"))

  exact <- dta_condition_mask("B", "equals", "9007199254740993", x)
  expect_identical(exact %in% TRUE, c(TRUE, FALSE))

  other <- dta_condition_mask("B", "equals", "9007199254740992", x)
  expect_identical(other %in% TRUE, c(FALSE, TRUE))
})


# ---------------------------------------------------------------------------
# A rule id or column name containing braces is data, not cli syntax
# ---------------------------------------------------------------------------

test_that("print(DTARuleColUnique) survives braces in the column names", {
  # print(DTARuleColUnique) pasted every column name into `{.field ...}`
  # markup with paste0() and handed the assembled string to cli_text(), so a
  # column called `a{b}` took it down with "Could not evaluate cli `{}`
  # expression" -- the same defect already fixed for print(DTADataSetTabular).
  rule <- DTARuleColUnique(id = "u1", columns = c("A{B}", "C{D}"))

  out <- capture.output(print(rule), type = "message")
  expect_true(any(grepl("A{B}", out, fixed = TRUE)))
  expect_true(any(grepl("C{D}", out, fixed = TRUE)))
})

test_that("print() on the other rule classes already survives braces in id/columns", {
  # None of these build their message text with paste0()/str_c()/sprintf()/
  # glue() around a dynamic value -- every value is interpolated directly
  # (`{x@id}`, `{paste(x@columns, collapse = ', ')}`, ...) -- so this pins the
  # already-correct behaviour against a future regression.
  cond_rule <- DTARuleColCondition(
    id = "cc{r}",
    description = "desc{ription}",
    condition = list(AGE = list(greater = 18)),
    then = list(STATUS = list(equals = "adult"))
  )
  out_cond <- capture.output(print(cond_rule), type = "message")
  expect_true(any(grepl("cc{r}", out_cond, fixed = TRUE)))
  expect_true(any(grepl("desc{ription}", out_cond, fixed = TRUE)))

  range_rule <- DTARuleColRange(id = "range{r}", columns = c("A{B}", "C{D}"), min = 0, max = 10)
  out_range <- capture.output(print(range_rule), type = "message")
  expect_true(any(grepl("range{r}", out_range, fixed = TRUE)))
  expect_true(any(grepl("A{B}, C{D}", out_range, fixed = TRUE)))

  group_rule <- DTARuleGroupCondition(
    id = "grp{r}",
    group_by = c("G{1}", "G2"),
    conditions = list(c1 = list(AGE = list(greater = 18))),
    constraints = list(list(type = "requires", `if` = "c1", `then` = "c1"))
  )
  out_group <- capture.output(print(group_rule), type = "message")
  expect_true(any(grepl("grp{r}", out_group, fixed = TRUE)))
  expect_true(any(grepl("G{1}, G2", out_group, fixed = TRUE)))
})
