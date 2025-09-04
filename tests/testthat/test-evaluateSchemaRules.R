test_that("Rules validation with test data frame", {
  # Create a test data frame
  test_df <- data.frame(
    VISIT = c("V03", "EOT", "V03", "V05"),
    STATUS = c("COMPLETED", "DROPPED", "IN_PROGRESS", "COMPLETED"),
    AGE = c(25, 70, 25, 65),
    CONSENT = c("YES", "NO", "YES", ""),
    CONSENT_DATE = c("2023-01-01", "2023-02-02", "2023-01-02", NA),
    SUBJECT_ID = c(1, 2, 1, 3),
    STUDYID = c("1234-5678", "1234-5678", "5678-1234", "1234-5678"),
    DOMAIN = c("GF", "GF", "GF", "GF"),
    SUBJIDN = c(1, 2, 1, 3),
    GFSEQID = c("KRAS", "KRAS", "MYC", "KRAS"),
    GFORRES = c(NA, 7, NA, 32),
    GFREASND = c("DATA ANALYSIS QC FAILED", "", "DATA ANALYSIS QC FAILED", ""),
    GFSTAT = c("NOT DONE", NA, "NOT DONE", NA),
    GFSPEC = c("toRNA", "toRNA", "toRNA", "toRNA"),
    GFTESTCD = c("TRNSCPTN", "TRNSCPTN", "TRNSCPTN", "TRNSCPTN"),
    WEIGHT = c(15, 4, 15, 20)
  )

  # Rule: check_condition - rule_equal_example
  result <- rule_check_condition(
    DTARule(
      type = "check_condition",
      id = "rule_equal_example",
      condition = list(VISIT = list(equals = "V03")),
      then = list(STATUS = list(equals = "COMPLETED")),
    ),
    test_df
  )
  expect_false(result$valid)

  # Rule: check_condition - rule_unequal_example
  result <- rule_check_condition(
    DTARule(
      type = "check_condition",
      id = "rule_unequal_example",
      condition = list(VISIT = list(equals = "V03")),
      then = list(STATUS = list(not_equals = "DROPPED"))
    ),
    test_df
  )
  expect_true(result$valid)

  # Rule: check_range - rule_range_example
  result <- rule_check_range(
    DTARule(
      type = "check_range",
      id = "rule_range_error",
      column = "AGE",
      range = c(18, 65)
    ),
    test_df
  )
  expect_false(result$valid)

  # Rule: check_range - rule_range_example
  result <- rule_check_range(
    DTARule(
      type = "check_range",
      id = "rule_range_success",
      column = "AGE",
      range = c(18, 70)
    ),
    test_df
  )
  expect_true(result$valid)

  # Rule: check_condition - rule_dependency_example
  result <- rule_check_condition(
    DTARule(
      type = "check_condition",
      id = "rule_dependency_example",
      "condition" = list(CONSENT = list(equals = "YES")),
      then = list(CONSENT_DATE = list(empty = FALSE))
    ),
    test_df
  )
  expect_true(result$valid)

  # Rule: check_condition - rule_exclusivity
  result <- rule_check_condition(
    DTARule(
      type = "check_condition",
      id = "rule_exclusivity_example",
      "condition" = list(CONSENT = list(equals = "YES")),
      then = list(CONSENT_DATE = list(empty = TRUE))
    ),
    test_df
  )
  expect_false(result$valid)

  # Rule: check_condition - rule_dependency_column
  result <- rule_check_condition(
    DTARule(
      type = "check_condition",
      id = "rule_dependency_column",
      condition = list(CONSENT = list(empty = FALSE)),
      then = list(CONSENT_DATE = list(empty = FALSE))
    ),
    test_df
  )
  expect_true(result$valid)

  # Rule: check_unique - rule_unique_example
  result <- rule_check_unique(
    DTARule(
      type = "check_unique",
      id = "rule_unique_example",
      column = c("SUBJECT_ID", "VISIT")
    ),
    test_df
  )
  expect_false(result$valid)

  # Rule: check_unique - rule_unique_example
  result <- rule_check_unique(
    DTARule(
      type = "check_unique",
      id = "rule_unique_example",
      column = c("SUBJECT_ID", "GFSEQID", "VISIT")
    ),
    test_df
  )
  expect_true(result$valid)

  # Rule: check_condition - check_condition_example
  result <- rule_check_condition(
    DTARule(
      type = "check_condition",
      id = "check_condition_example",
      "condition" = list(VISIT = list(equals = "V03")),
      then = list(
        STATUS = list(`in` = c("COMPLETED", "IN_PROGRESS")),
        CONSENT = list(equals = "YES"),
        AGE = list(range = c(10, 100)),
        WEIGHT = list(greater_equal = 5)
      )
    ),
    test_df
  )
  expect_true(result$valid)

  # Rule: check_condition - check_equals
  result <- rule_check_condition(
    DTARule(
      type = "check_condition",
      id = "check_equals",
      condition = list(STUDYID = list(equals = "1234-5678")),
      then = list(DOMAIN = list(equals = "GF"))
    ),
    test_df
  )
  expect_true(result$valid)

  # Rule: check_condition - unequal_check
  result <- rule_check_condition(
    DTARule(
      type = "check_condition",
      id = "unequal_check",
      condition = list(STUDYID = list(equals = "1234-5678")),
      then = list(DOMAIN = list(not_equals = "DF"))
    ),
    test_df
  )
  expect_true(result$valid)

  # Rule: check_range - GFORRES must have a range
  result <- rule_check_range(
    DTARule(
      type = "check_range",
      id = "GFORRES must have a range",
      column = "GFORRES",
      range = c(0, 100000000)
    ),
    test_df
  )
  expect_true(result$valid)

  # Rule: check_condition - dependency_check
  result <- rule_check_condition(
    DTARule(
      type = "check_condition",
      id = "dependency_check",
      "condition" = list(GFREASND = list(empty = FALSE)),
      then = list(GFORRES = list(empty = TRUE))
    ),
    test_df
  )
  expect_true(result$valid)

  # Rule: check_condition - overlap_check
  result <- rule_check_condition(
    DTARule(
      type = "check_condition",
      id = "overlap_check",
      "condition" = list(GFSPEC = list(equals = "toRNA")),
      then = list(GFTESTCD = list(equals = "TRNSCPTN"))
    ),
    test_df
  )
  expect_true(result$valid)
})
