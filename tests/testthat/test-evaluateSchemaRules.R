test_that("Range and unique rules evaluate correctly", {
  test_df <- data.frame(
    AGE = c(25, 70, 25, 65),
    SUBJECT_ID = c(1, 2, 1, 3),
    VISIT = c("V03", "EOT", "V03", "V05"),
    GFSEQID = c("KRAS", "KRAS", "MYC", "KRAS"),
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

  result <- rule_check_range(
    DTARuleColRange(
      id = "rule_range_success",
      columns = "AGE",
      range = c(18, 70)
    ),
    test_df
  )
  expect_true(result$valid)

  result <- rule_check_unique(
    DTARuleColUnique(
      id = "rule_unique_error",
      columns = c("SUBJECT_ID", "VISIT")
    ),
    test_df
  )
  expect_false(result$valid)

  result <- rule_check_unique(
    DTARuleColUnique(
      id = "rule_unique_success",
      columns = c("SUBJECT_ID", "GFSEQID", "VISIT")
    ),
    test_df
  )
  expect_true(result$valid)
})

test_that("Conditional rules evaluate correctly", {
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

  result <- rule_check_col_condition(
    DTARuleColCondition(
      id = "rule_equal_example",
      condition = list(VISIT = list(equals = "V03")),
      then = list(STATUS = list(equals = "COMPLETED"))
    ),
    test_df
  )
  expect_false(result$valid)

  result <- rule_check_col_condition(
    DTARuleColCondition(
      id = "rule_unequal_example",
      condition = list(VISIT = list(equals = "V03")),
      then = list(STATUS = list(not_equals = "DROPPED"))
    ),
    test_df
  )
  expect_true(result$valid)

  result <- rule_check_col_condition(
    DTARuleColCondition(
      id = "rule_dependency_example",
      condition = list(CONSENT = list(equals = "YES")),
      then = list(CONSENT_DATE = list(empty = FALSE))
    ),
    test_df
  )
  expect_true(result$valid)

  result <- rule_check_col_condition(
    DTARuleColCondition(
      id = "rule_exclusivity_example",
      condition = list(CONSENT = list(equals = "YES")),
      then = list(CONSENT_DATE = list(empty = TRUE))
    ),
    test_df
  )
  expect_false(result$valid)

  result <- rule_check_col_condition(
    DTARuleColCondition(
      id = "check_col_condition_example",
      condition = list(VISIT = list(equals = "V03")),
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

  result <- rule_check_col_condition(
    DTARuleColCondition(
      id = "study_domain_consistency",
      condition = list(STUDYID = list(equals = "1234-5678")),
      then = list(DOMAIN = list(equals = "GF"))
    ),
    test_df
  )
  expect_true(result$valid)
})

test_that("apply_schema_rules handles canonical and legacy rule types", {
  df <- data.frame(AGE = c(20, 30), SUBJECT_ID = c("A", "B"), stringsAsFactors = FALSE)

  canonical_rules <- list(
    DTARuleColRange(id = "r1", columns = "AGE", min = 18, max = 65),
    DTARuleColUnique(id = "u1", columns = "SUBJECT_ID")
  )

  result <- apply_schema_rules(canonical_rules, df, verbose = FALSE)
  expect_true(all(vapply(result, function(x) isTRUE(x$valid), logical(1))))

  legacy_rule <- DTARuleColRange(id = "legacy", columns = "AGE", min = 18, max = 65)
  legacy_rule@type <- "col_range"
  result <- apply_schema_rules(list(legacy_rule), df, verbose = FALSE)
  expect_true(isTRUE(result[[1]]$valid))
})
