# R/rulePhrasing.R is the single source of the plain-English rule wording.
# Every expected string below is written by hand from the wording contract
# (the comment block at the top of R/rulePhrasing.R); none is copied from the
# output. Locale: only package-authored text is asserted.

lq <- "\u201c"
rq <- "\u201d"
lsq <- "\u2018"
rsq <- "\u2019"
bul <- "\u2022 "
q <- function(x) paste0(lq, x, rq)
sq <- function(x) paste0(lsq, x, rsq)

cond_rule <- function(condition, then, id = "r1", description = NULL) {
  DTARuleColCondition(
    id = id, description = description, condition = condition, then = then
  )
}

first_line <- function(rule) strsplit(.rule_text(rule), "\n", fixed = TRUE)[[1]][[1]]

# --- Notation ---------------------------------------------------------------

test_that(".rule_number gives the shortest exact decimal form", {
  expect_identical(.rule_number(0.1), "0.1")
  expect_identical(.rule_number(1 / 3), "0.3333333333333333")
  expect_identical(.rule_number(0.1 + 0.2), "0.30000000000000004")
  expect_identical(.rule_number(1e8), "100000000")
  expect_identical(.rule_number(1e-7), "0.0000001")
  expect_identical(.rule_number(1e21), "1 times 10 to the power of 21")
  expect_identical(.rule_number(1e20), "100000000000000000000")
  expect_identical(.rule_number(5e300), "5 times 10 to the power of 300")
  expect_identical(.rule_number(1e-8), "1 times 10 to the power of minus 8")
  expect_identical(.rule_number(-1e-8), "minus 1 times 10 to the power of minus 8")
  expect_identical(.rule_number(-2.5e-9), "minus 2.5 times 10 to the power of minus 9")
  expect_identical(.rule_number(-5e300), "minus 5 times 10 to the power of 300")
  expect_identical(.rule_number(-0), "0")
  expect_identical(.rule_number(18L), "18")
  expect_identical(.rule_number(18), .rule_number(18L))
  expect_identical(.rule_number(-12.25), "-12.25")
  expect_identical(.rule_number(Inf), "infinity")
  expect_identical(.rule_number(-Inf), "minus infinity")
  expect_identical(.rule_number(NaN), "not a number")
  expect_identical(.rule_number(NA_real_), "missing")
  expect_error(.rule_number(c(1, 2)), class = "dta_rule_phrasing_error")
  expect_error(.rule_number("1"), class = "dta_rule_phrasing_error")
})

test_that(".rule_number reads back exactly and never uses e-notation", {
  withr::local_seed(20261108)
  x <- c(
    runif(200, -1e6, 1e6), exp(runif(200, log(1e-7), log(1e20))),
    -exp(runif(100, log(1e-7), log(1e20)))
  )
  out <- vapply(x, .rule_number, character(1))
  expect_identical(as.numeric(out), x)
  expect_false(any(grepl("e", out, fixed = TRUE)))
})

test_that(".rule_value distinguishes text, numbers, logicals and missing", {
  expect_identical(.rule_value("18"), q("18"))
  expect_identical(.rule_value(18), "18")
  expect_identical(.rule_value(TRUE), "true")
  expect_identical(.rule_value(FALSE), "false")
  expect_identical(.rule_value(NA), "missing")
  expect_identical(.rule_value(NA_character_), "missing")
  expect_identical(.rule_value(NULL), "missing")
  expect_identical(.rule_value(""), q(""))
  expect_identical(.rule_value("say \"hi\""), q("say \"hi\""))
  expect_identical(.rule_value("a\nb"), q("a[line break]b"))
  expect_identical(.rule_value("a\tb"), q("a[tab]b"))
  expect_identical(.rule_value("a\rb"), q("a[carriage return]b"))
  expect_identical(.rule_value("a\001b"), q("a[character U+0001]b"))
})

test_that(".rule_name quotes only names that are not plain identifiers", {
  expect_identical(.rule_name("AGE"), "AGE")
  expect_identical(.rule_name("c_failed.2"), "c_failed.2")
  expect_identical(.rule_name("my col"), sq("my col"))
  expect_identical(.rule_name("1A"), sq("1A"))
  expect_identical(.rule_name("a`b"), sq("a`b"))
})

test_that(".rule_or and .rule_and join lists", {
  expect_identical(.rule_or("a"), "a")
  expect_identical(.rule_or(c("a", "b")), "a or b")
  expect_identical(.rule_or(c("a", "b", "c")), "a, b or c")
  expect_identical(.rule_and(c("a", "b", "c")), "a, b and c")
})

# --- Operators --------------------------------------------------------------

# Condition form / requirement form for each operator, written from the table.
operator_cases <- list(
  list(op = list(equals = 5), is = "is equal to 5", must = "must be equal to 5"),
  list(op = list(equal = "x"), is = paste0("is equal to ", q("x")), must = paste0("must be equal to ", q("x"))),
  list(op = list(not_equals = "x"), is = paste0("is not equal to ", q("x")), must = paste0("must not be equal to ", q("x"))),
  list(op = list(not_equal = 5), is = "is not equal to 5", must = "must not be equal to 5"),
  list(
    op = list(`in` = c("a", "b", "c")),
    is = paste0("is one of ", q("a"), ", ", q("b"), " or ", q("c")),
    must = paste0("must be one of ", q("a"), ", ", q("b"), " or ", q("c"))
  ),
  list(
    op = list(not_in = c("a", "b")),
    is = paste0("is not one of ", q("a"), " or ", q("b")),
    must = paste0("must not be one of ", q("a"), " or ", q("b"))
  ),
  list(op = list(greater = 5), is = "is greater than 5", must = "must be greater than 5"),
  list(op = list(less = 0.5), is = "is less than 0.5", must = "must be less than 0.5"),
  list(op = list(greater_equal = 5), is = "is at least 5", must = "must be at least 5"),
  list(op = list(min = 5), is = "is at least 5", must = "must be at least 5"),
  list(op = list(less_equal = 7), is = "is at most 7", must = "must be at most 7"),
  list(op = list(max = 7), is = "is at most 7", must = "must be at most 7"),
  list(op = list(range = c(1, 5)), is = "is from 1 to 5, inclusive", must = "must be from 1 to 5, inclusive"),
  list(op = list(min = 1, max = 5), is = "is from 1 to 5, inclusive", must = "must be from 1 to 5, inclusive"),
  list(
    op = list(pattern = "^A[0-9]+$"),
    is = paste0("matches the pattern ", q("^A[0-9]+$")),
    must = paste0("must match the pattern ", q("^A[0-9]+$"))
  ),
  list(op = list(empty = TRUE), is = "is empty", must = "must be empty"),
  list(op = list(empty = FALSE), is = "is not empty", must = "must not be empty")
)

test_that("every operator has its condition form and requirement form", {
  for (case in operator_cases) {
    as_then <- cond_rule(list(A = list(equals = "x")), list(B = case$op))
    expect_identical(
      first_line(as_then),
      paste0("When A is equal to ", q("x"), ", B ", case$must, ".")
    )
    as_if <- cond_rule(list(B = case$op), list(A = list(equals = "x")))
    expect_identical(
      first_line(as_if),
      paste0("When B ", case$is, ", A must be equal to ", q("x"), ".")
    )
  }
})

test_that("every supported operator of the evaluator is covered", {
  covered <- unique(unlist(lapply(operator_cases, function(case) names(case$op))))
  expect_setequal(covered, dta_condition_operators())
})

test_that("equivalent operator spellings give identical wording", {
  pairs <- list(
    list(list(equals = 3), list(equal = 3)),
    list(list(not_equals = "a"), list(not_equal = "a")),
    list(list(min = 3), list(greater_equal = 3)),
    list(list(max = 3), list(less_equal = 3)),
    list(list(range = c(2, 9)), list(min = 2, max = 9)),
    list(list(range = list(2, 9)), list(max = 9, min = 2)),
    list(list(equals = 18L), list(equals = 18))
  )
  for (p in pairs) {
    a <- cond_rule(list(A = p[[1]]), list(B = p[[1]]))
    b <- cond_rule(list(A = p[[2]]), list(B = p[[2]]))
    expect_identical(.rule_text(a), .rule_text(b))
  }
  r1 <- DTARuleColRange(id = "r", columns = "AGE", range = c(18, 65))
  r2 <- DTARuleColRange(id = "r", columns = "AGE", min = 18L, max = 65L)
  expect_identical(.rule_text(r1), .rule_text(r2))
})

test_that("one-sided bands read at least / at most / a number", {
  expect_identical(
    first_line(cond_rule(list(A = list(equals = 1)), list(B = list(min = 2, max = Inf)))),
    "When A is equal to 1, B must be at least 2."
  )
  expect_identical(
    first_line(cond_rule(list(A = list(equals = 1)), list(B = list(min = -Inf, max = 2)))),
    "When A is equal to 1, B must be at most 2."
  )
  expect_identical(
    first_line(cond_rule(list(A = list(equals = 1)), list(B = list(range = c(-Inf, Inf))))),
    "When A is equal to 1, B must be a number."
  )
})

test_that("several tests on one column share the column as subject", {
  rule <- cond_rule(
    list(A = list(equals = 1)),
    list(B = list(greater = 2, less = 9, pattern = "7"))
  )
  expect_identical(
    first_line(rule),
    paste0(
      "When A is equal to 1, B must be greater than 2, must be less than 9 ",
      "and must match the pattern ", q("7"), "."
    )
  )
})

# --- Rule types -------------------------------------------------------------

test_that("a condition rule reads as one sentence with its notes", {
  rule <- cond_rule(list(VISIT = list(equals = "V03")), list(STATUS = list(equals = "COMPLETED")))
  expect_identical(.rule_text(rule), paste(
    paste0("When VISIT is equal to ", q("V03"), ", STATUS must be equal to ", q("COMPLETED"), "."),
    "Missing values:",
    paste0(bul, "If VISIT is missing, the condition is not met and the row is not checked."),
    paste0(bul, "If STATUS is missing in a row that meets the condition, the rule is violated."),
    sep = "\n"
  ))
})

test_that("several columns in a clause become a list", {
  rule <- cond_rule(
    list(VISIT = list(equals = "V03"), AGE = list(greater = 17)),
    list(STATUS = list(`in` = c("DONE", "OK")), WEIGHT = list(max = 200))
  )
  expect_identical(.rule_text(rule), paste(
    "When all of the following are true:",
    paste0(bul, "VISIT is equal to ", q("V03")),
    paste0(bul, "AGE is greater than 17"),
    "Then all of the following must hold:",
    paste0(bul, "STATUS must be one of ", q("DONE"), " or ", q("OK")),
    paste0(bul, "WEIGHT must be at most 200"),
    "Missing values:",
    paste0(
      bul, "If VISIT is missing, the condition is not met and the row is not checked, ",
      "unless another column of the condition holds a value that is not a number ",
      "and no other test of the condition fails."
    ),
    paste0(bul, "If AGE is missing, the condition is not met and the row is not checked."),
    paste0(bul, "If STATUS is missing in a row that meets the condition, the rule is violated."),
    paste0(bul, "If WEIGHT is missing in a row that meets the condition, the rule is violated."),
    "Non-numeric values:",
    paste0(bul, "A value in AGE that is not a number does not by itself exempt the row from the check."),
    paste0(bul, "A value in WEIGHT that is not a number, in a row that meets the condition, violates the rule."),
    sep = "\n"
  ))

  one_if <- cond_rule(
    list(VISIT = list(equals = "V03")),
    list(STATUS = list(empty = FALSE), AGE = list(min = 18))
  )
  expect_identical(
    strsplit(.rule_text(one_if), "\n", fixed = TRUE)[[1]][1:3],
    c(
      paste0("When VISIT is equal to ", q("V03"), ", all of the following must hold:"),
      paste0(bul, "STATUS must not be empty"),
      paste0(bul, "AGE must be at least 18")
    )
  )

  many_if <- cond_rule(
    list(VISIT = list(equals = "V03"), AGE = list(empty = FALSE)),
    list(STATUS = list(empty = FALSE))
  )
  expect_identical(
    strsplit(.rule_text(many_if), "\n", fixed = TRUE)[[1]][1:4],
    c(
      "When all of the following are true:",
      paste0(bul, "VISIT is equal to ", q("V03")),
      paste0(bul, "AGE is not empty"),
      "Then STATUS must not be empty."
    )
  )
})

test_that("a range rule states its bounds and its missing-value handling", {
  rule <- DTARuleColRange(id = "age", columns = "AGE", min = 18, max = 65)
  expect_identical(.rule_text(rule), paste(
    "Every value in column AGE must be a number from 18 to 65, inclusive.",
    "Missing values: Not checked.",
    "Non-numeric values: A value that is present but is not a number violates the rule.",
    sep = "\n"
  ))
  expect_identical(
    first_line(DTARuleColRange(id = "a", columns = "AGE", min = -Inf, max = 65)),
    "Every value in column AGE must be a number that is at most 65."
  )
  expect_identical(
    first_line(DTARuleColRange(id = "a", columns = "AGE", min = 18, max = Inf)),
    "Every value in column AGE must be a number that is at least 18."
  )
  expect_identical(
    first_line(DTARuleColRange(id = "a", columns = "AGE", min = -Inf, max = Inf)),
    "Every value in column AGE must be a number."
  )
})

test_that("a unique rule distinguishes one column from a combination", {
  one <- DTARuleColUnique(id = "u", columns = "SUBJID")
  expect_identical(.rule_text(one), paste(
    "No two rows may have the same value in column SUBJID.",
    paste0(
      "Missing values: Two missing values are equal; so are two ",
      q("not a number"), " (NaN) values, which are not missing."
    ),
    sep = "\n"
  ))
  two <- DTARuleColUnique(id = "u", columns = c("SUBJID", "VISIT"))
  expect_identical(
    first_line(two),
    "No two rows may have the same combination of values in columns SUBJID and VISIT."
  )
})

group_rule <- function() {
  DTARuleGroupCondition(
    id = "g1",
    group_by = c("SUBJECT_ID", "VISIT"),
    conditions = list(
      c_failed = list(STATUS = list(equals = "FAILED")),
      c_reported = list(RESULT = list(empty = FALSE))
    ),
    constraints = list(
      list(
        id = "no_result_when_failed", type = "mutually_exclusive",
        left = "c_failed", right = "c_reported", right_scope = "all",
        message = "Failed tests must not report a result."
      ),
      list(type = "requires", `if` = "c_failed", then = "c_reported")
    )
  )
}

test_that("a group rule lists grouping, conditions and requirements", {
  expect_identical(.rule_text(group_rule()), paste(
    paste0(
      "Rows are grouped by the combination of values in columns SUBJECT_ID and VISIT; ",
      "within each group, the requirements below must hold."
    ),
    "Conditions:",
    paste0(bul, "c_failed: a row in which STATUS is equal to ", q("FAILED")),
    paste0(bul, "c_reported: a row in which RESULT is not empty"),
    "Requirements:",
    paste0(
      bul, "no_result_when_failed: Within a group, it must not happen that at least one row ",
      "meets condition c_failed and also every row meets condition c_reported. ",
      "A violation is reported with the message ", q("Failed tests must not report a result."), "."
    ),
    paste0(
      bul, "constraint_2: If at least one row of a group meets condition c_failed, then at ",
      "least one row of that group must meet condition c_reported. ",
      "A violation is reported with a message naming the group and its rows."
    ),
    "Missing values:",
    paste0(
      bul, "A missing value in a grouping column counts as a value of its own: ",
      "rows missing it are grouped together."
    ),
    paste0(bul, "A row in which STATUS is missing does not meet condition c_failed."),
    paste0(bul, "A row in which RESULT is missing does not meet condition c_reported."),
    sep = "\n"
  ))
})

test_that("the author's purpose is its own line and never replaces the mechanics", {
  rule <- cond_rule(
    list(A = list(equals = 1)), list(B = list(equals = 2)),
    description = "Adults only"
  )
  lines <- strsplit(.rule_text(rule), "\n", fixed = TRUE)[[1]]
  expect_identical(lines[[1]], "Purpose: Adults only")
  expect_identical(lines[[2]], "When A is equal to 1, B must be equal to 2.")
})

test_that("an unknown rule class is described as unsupported", {
  rule <- DTARule(id = "x", type = "col_magic", description = NULL)
  expect_identical(
    .rule_text(rule),
    paste0("This rule has the unsupported type ", sq("col_magic"), "; its parameters cannot be described.")
  )
  expect_false(.rule_describe(rule)$valid)
  expect_identical(.rule_kind_label(rule), "Unsupported rule type")
  expect_error(.rule_text(list(id = "x")), class = "dta_rule_phrasing_error")
})

test_that("rule kinds have their labels", {
  expect_identical(.rule_kind_label(group_rule()), "Grouped condition")
  expect_identical(.rule_kind_label(cond_rule(list(A = list(equals = 1)), list(B = list(equals = 1)))), "Conditional (IF/THEN)")
  expect_identical(.rule_kind_label(DTARuleColRange(id = "a", columns = "A", min = 1, max = 2)), "Range")
  expect_identical(.rule_kind_label(DTARuleColUnique(id = "a", columns = "A")), "Unique")
})

# --- Invalid rules ----------------------------------------------------------

test_that("invalid rules are described as not evaluable, never as valid", {
  unsupported <- cond_rule(list(A = list(foo = 1)), list(B = list(equals = "x")))
  expect_identical(.rule_text(unsupported), paste(
    paste0("When A passes the unsupported test ", sq("foo"), " with 1, B must be equal to ", q("x"), "."),
    paste0("Problem: This rule cannot be evaluated. The test ", sq("foo"), " on column A is not supported."),
    "Missing values: If B is missing in a row that meets the condition, the rule is violated.",
    sep = "\n"
  ))
  expect_false(.rule_describe(unsupported)$valid)
  # ... and the evaluator agrees that it cannot run.
  expect_error(
    rule_check_col_condition(unsupported, data.frame(A = 1, B = "x")),
    regexp = "Unsupported condition operator"
  )

  # A range rule cannot be built with two columns; one that bypassed
  # validation is still described, not crashed on.
  two_cols <- DTARuleColRange(id = "r", columns = "A", min = 1, max = 5)
  S7::prop(two_cols, "columns", check = FALSE) <- c("A", "B")
  expect_identical(.rule_text(two_cols), paste(
    "Every value in columns A and B would have to be a number from 1 to 5, inclusive.",
    "Problem: This rule cannot be evaluated. A range rule needs exactly one column, but this one names 2.",
    sep = "\n"
  ))
  expect_false(.rule_describe(two_cols)$valid)
  expect_error(
    rule_check_range(two_cols, data.frame(A = 1, B = 2)),
    class = "dta_rule_range_columns"
  )

  # A constraint given as a bare value instead of an operator map.
  bare <- S7::set_props(
    cond_rule(list(A = list(equals = 1)), list(B = list(equals = 1))),
    then = list(B = 18)
  )
  expect_identical(.rule_text(bare), paste(
    "When A is equal to 1, B is given the bare value 18 instead of a test.",
    "Problem: This rule cannot be evaluated. Column B is given a value instead of a test.",
    "Missing values: If A is missing, the condition is not met and the row is not checked.",
    sep = "\n"
  ))
  expect_false(.rule_describe(bare)$valid)

  valid <- cond_rule(list(A = list(equals = 1)), list(B = list(equals = 1)))
  expect_true(.rule_describe(valid)$valid)
})

# --- Defect regressions -----------------------------------------------------

test_that("equals true/false is a value comparison, not presence", {
  rule <- cond_rule(list(A = list(equals = TRUE)), list(B = list(equals = FALSE)))
  expect_identical(first_line(rule), "When A is equal to true, B must be equal to false.")
  expect_false(grepl("present", .rule_text(rule), fixed = TRUE))
})

test_that("a mixed in-list reads as one of, not as AND", {
  rule <- cond_rule(list(A = list(equals = 1)), list(B = list(`in` = list(1, "A"))))
  expect_identical(first_line(rule), paste0("When A is equal to 1, B must be one of 1 or ", q("A"), "."))
  expect_false(grepl("AND", .rule_text(rule), fixed = TRUE))
})

test_that("a group rule shows each requirement id and message", {
  text <- .rule_text(group_rule())
  expect_true(grepl("no_result_when_failed:", text, fixed = TRUE))
  expect_true(grepl(q("Failed tests must not report a result."), text, fixed = TRUE))
  expect_true(grepl("constraint_2:", text, fixed = TRUE))
})

test_that("text values keep quotes, line breaks and emptiness visible", {
  rule <- cond_rule(
    list(A = list(equals = "say \"hi\"")),
    list(B = list(equals = "line1\nline2"), C = list(equals = ""))
  )
  text <- .rule_text(rule)
  expect_true(grepl(paste0("A is equal to ", q("say \"hi\"")), text, fixed = TRUE))
  expect_true(grepl(paste0("B must be equal to ", q("line1[line break]line2")), text, fixed = TRUE))
  expect_true(grepl(paste0("C must be equal to ", q("")), text, fixed = TRUE))
})

# --- Missing values: the notes agree with the evaluator ----------------------

test_that("a missing entry in a plain in-vector reads as missing and matches one", {
  vec <- cond_rule(list(A = list(equals = 1)), list(B = list(`in` = c("x", NA))))
  expect_identical(first_line(vec), paste0("When A is equal to 1, B must be one of ", q("x"), " or missing."))
  expect_true(grepl("If B is missing, the requirement counts as met.", .rule_text(vec), fixed = TRUE))
  # ... and the evaluator agrees, on a text and on a number column.
  df <- data.frame(A = 1, B = NA_character_, stringsAsFactors = FALSE)
  expect_true(rule_check_col_condition(vec, df)$valid)
  num <- cond_rule(list(A = list(equals = 1)), list(B = list(`in` = c(2, NA))))
  expect_true(rule_check_col_condition(num, data.frame(A = 1, B = NA_real_))$valid)
})

# Regression (review finding W1): `in: [A, ~]` arrives from YAML as
# list("A", NULL). The empty entry is read as a missing value, so it matches
# one and never the text "NULL".
test_that("an empty YAML entry in an in-list is a missing value, in wording and evaluator", {
  y <- yaml::yaml.load("v: [A, ~]")$v
  expect_identical(y, list("A", NULL))
  note <- " (the list also holds a missing entry, which matches a missing value)"
  if_in <- cond_rule(list(K = list(`in` = y)), list(V = list(empty = FALSE)))
  expect_identical(.rule_text(if_in), paste(
    paste0("When K is one of ", q("A"), note, ", V must not be empty."),
    "Missing values:",
    paste0(bul, "If K is missing, the condition counts as met."),
    paste0(bul, "If V is missing in a row that meets the condition, the rule is violated."),
    sep = "\n"
  ))
  if_not_in <- cond_rule(list(K = list(not_in = y)), list(V = list(empty = FALSE)))
  expect_identical(
    first_line(if_not_in),
    paste0("When K is not one of ", q("A"), note, ", V must not be empty.")
  )
  expect_true(grepl("If K is missing, the condition is not met", .rule_text(if_not_in), fixed = TRUE))

  # The evaluator: rows A, missing, B, "NULL"; V is always missing, so a row
  # is violated exactly when it meets the condition.
  df <- data.frame(K = c("A", NA, "B", "NULL"), V = NA_character_, stringsAsFactors = FALSE)
  expect_identical(dta_condition_violated(if_in, df), c(TRUE, TRUE, FALSE, FALSE))
  expect_identical(dta_condition_violated(if_not_in, df), c(FALSE, FALSE, TRUE, TRUE))
  num <- data.frame(K = c(1, NA, 2), V = NA_character_)
  num_in <- cond_rule(list(K = list(`in` = yaml::yaml.load("v: [1, ~]")$v)), list(V = list(empty = FALSE)))
  expect_identical(dta_condition_violated(num_in, num), c(TRUE, TRUE, FALSE))
  num_not_in <- cond_rule(list(K = list(not_in = yaml::yaml.load("v: [1, ~]")$v)), list(V = list(empty = FALSE)))
  expect_identical(dta_condition_violated(num_not_in, num), c(FALSE, FALSE, TRUE))
})

# Regression (review round 2, W3): whether a missing entry of a list matches
# a missing value depends on the column (numbers or not) and on whether the
# rest of the list reads as numbers (dta_equality_bound()); the list is known,
# so the wording names only the column condition.
test_that("a missing entry in an in-list depends on the column, and says so", {
  na_depends <- paste0(
    " (the list also holds a missing entry, which matches a missing value if K ",
    "holds numbers and otherwise only the text ", q("NA"), ")"
  )
  na_never <- paste0(
    " (the list also holds a missing entry, which matches only the text ", q("NA"),
    ", never a missing value)"
  )
  rule <- cond_rule(list(K = list(`in` = list(1, NA))), list(V = list(empty = FALSE)))
  expect_identical(.rule_text(rule), paste(
    paste0("When K is one of 1", na_depends, ", V must not be empty."),
    "Missing values:",
    paste0(
      bul, "If K is missing, the condition is not met and the row is not checked, unless ",
      "K holds numbers; then the condition counts as met."
    ),
    paste0(bul, "If V is missing in a row that meets the condition, the rule is violated."),
    sep = "\n"
  ))
  # Numbers: the missing entry matches the missing value.
  expect_identical(
    dta_condition_violated(rule, data.frame(K = c(1, NA, 2), V = NA_character_)),
    c(TRUE, TRUE, FALSE)
  )
  # Text: it matches the text "NA", not the missing value.
  expect_identical(
    dta_condition_violated(rule, data.frame(K = c("1", NA, "NA"), V = NA_character_)),
    c(TRUE, FALSE, TRUE)
  )

  # A text that reads as a number counts as a number (the counterexample of
  # the review): list(1, "2", NA) on numbers matches a missing value.
  mixed <- cond_rule(list(A = list(equals = "a")), list(K = list(`in` = list(1, "2", NA))))
  expect_identical(
    first_line(mixed),
    paste0("When A is equal to ", q("a"), ", K must be one of 1 or ", q("2"), na_depends, ".")
  )
  expect_true(grepl(paste0(
    "If K is missing in a row that meets the condition, the rule is violated, unless K holds ",
    "numbers; then the requirement counts as met."
  ), .rule_text(mixed), fixed = TRUE))
  num <- data.frame(A = "a", K = c(1, NA, 2, 3))
  expect_identical(dta_condition_violated(mixed, num), c(FALSE, FALSE, FALSE, TRUE))
  int <- data.frame(A = "a", K = c(1L, NA, 2L, 3L))
  expect_identical(dta_condition_violated(mixed, int), c(FALSE, FALSE, FALSE, TRUE))
  txt <- data.frame(A = "a", K = c("1", NA, "NA", "3"))
  expect_identical(dta_condition_violated(mixed, txt), c(FALSE, TRUE, FALSE, TRUE))
  dates <- data.frame(A = "a", K = as.Date(c("2020-01-01", NA)))
  expect_identical(dta_condition_violated(mixed, dates), c(TRUE, TRUE))

  # A list whose other entries do not all read as numbers never matches a
  # missing value, on any column: a text, a logical value.
  for (lst in list(list("A", NA), list(1, "A", NA), list(TRUE, NA))) {
    never <- cond_rule(list(A = list(equals = "a")), list(K = list(`in` = lst)))
    expect_true(grepl(na_never, first_line(never), fixed = TRUE), info = deparse(lst))
    expect_true(grepl(
      "If K is missing in a row that meets the condition, the rule is violated.",
      .rule_text(never),
      fixed = TRUE
    ), info = deparse(lst))
    for (df in list(num, int, txt, data.frame(A = "a", K = c(TRUE, NA)), dates)) {
      expect_true(dta_condition_violated(never, df)[[2]], info = deparse(lst))
    }
    # ... and on text it matches the text "NA".
    expect_false(dta_condition_violated(never, txt)[[3]], info = deparse(lst))
  }

  # not_in: the two outcomes swapped, on the THEN and on the IF.
  not_then <- cond_rule(list(A = list(equals = "a")), list(K = list(not_in = list(1, "2", NA))))
  expect_true(grepl(
    "If K is missing, the requirement counts as met, unless K holds numbers; then the rule is violated.",
    .rule_text(not_then),
    fixed = TRUE
  ))
  expect_identical(dta_condition_violated(not_then, num), c(TRUE, TRUE, TRUE, FALSE))
  expect_identical(dta_condition_violated(not_then, txt), c(TRUE, FALSE, TRUE, FALSE))
  not_if <- cond_rule(list(K = list(not_in = list(1, NA))), list(V = list(empty = FALSE)))
  expect_true(grepl(
    paste0(
      "If K is missing, the condition counts as met, unless K holds numbers; then the ",
      "condition is not met and the row is not checked."
    ),
    .rule_text(not_if),
    fixed = TRUE
  ))
  expect_identical(
    dta_condition_violated(not_if, data.frame(K = c(1, NA, 2), V = NA_character_)),
    c(FALSE, FALSE, TRUE)
  )
  expect_identical(
    dta_condition_violated(not_if, data.frame(K = c("1", NA, "NA"), V = NA_character_)),
    c(FALSE, TRUE, FALSE)
  )

  # A group condition says the same.
  g <- DTARuleGroupCondition(
    id = "g", group_by = "S",
    conditions = list(c1 = list(K = list(`in` = list(1, "2", NA))), c2 = list(V = list(empty = TRUE))),
    constraints = list(list(id = "k", type = "requires", `if` = "c2", then = "c1", then_scope = "all"))
  )
  expect_true(grepl(
    "A row in which K is missing does not meet condition c1, unless K holds numbers; then the row meets condition c1.",
    .rule_text(g),
    fixed = TRUE
  ))
  expect_true(rule_check_group_condition(g, data.frame(S = "s", K = NA_real_, V = NA))$valid)
  expect_false(rule_check_group_condition(g, data.frame(S = "s", K = NA_character_, V = NA))$valid)

  # A list holding nothing else never matches a missing value.
  only <- cond_rule(list(K = list(`in` = list(NA))), list(V = list(empty = FALSE)))
  expect_identical(first_line(only), paste0(
    "When K is one of the entries of a list that holds only a missing entry, which matches ",
    "only the text ", q("NA"), ", never a missing value, V must not be empty."
  ))
  expect_identical(
    dta_condition_violated(only, data.frame(K = c(1, NA), V = NA_character_)),
    c(FALSE, FALSE)
  )
  expect_identical(
    dta_condition_violated(only, data.frame(K = c("NA", NA), V = NA_character_)),
    c(TRUE, FALSE)
  )

  # An empty and a missing entry together, each explained; the empty one reads
  # as a missing value, the other as before.
  both <- cond_rule(list(A = list(equals = "a")), list(K = list(`in` = list(1, NULL, NA))))
  expect_identical(first_line(both), paste0(
    "When A is equal to ", q("a"), ", K must be one of 1 (the list also holds a missing entry, ",
    "which matches a missing value, and another missing entry, which matches a missing ",
    "value if K holds numbers and otherwise only the text ", q("NA"), ")."
  ))
  expect_identical(dta_condition_violated(both, num), c(FALSE, FALSE, TRUE, TRUE))
  expect_identical(
    dta_condition_violated(both, data.frame(A = "a", K = c("NULL", NA, "NA"))),
    c(TRUE, FALSE, FALSE)
  )
})

# Regression (review round 3, W1): a missing TEXT entry (YAML .na.character)
# stays a true missing value when match() turns the list into text, so it
# matches a missing value in a column of any type -- unlike a logical or
# numeric missing entry, which becomes the text "NA".
test_that("a missing text entry in an in-list matches a missing value on every column", {
  y <- yaml::yaml.load("a: [true, .na.character]\nb: [1, .na.character]")
  expect_identical(y$a, list(TRUE, NA_character_))
  expect_identical(y$b, list(1L, NA_character_))
  na_text <- "a missing entry, which matches a missing value"
  # The evaluator, both column types: the missing value is always matched.
  for (lst in list(y$a, y$b, list("a", NA_character_), list(NA_character_))) {
    expect_identical(dta_condition_mask("C", "in", lst, c(NA_real_, 7))[[1]], TRUE, info = deparse(lst))
    expect_identical(dta_condition_mask("C", "in", lst, c(NA_character_, "x"))[[1]], TRUE, info = deparse(lst))
    expect_identical(dta_condition_mask("C", "not_in", lst, c(NA_real_, 7))[[1]], FALSE, info = deparse(lst))
    expect_identical(dta_condition_mask("C", "not_in", lst, c(NA_character_, "x"))[[1]], FALSE, info = deparse(lst))
  }

  # IF: the row is checked, and the wording says the condition is met.
  if_in <- cond_rule(list(C = list(`in` = y$a)), list(V = list(empty = FALSE)))
  expect_identical(.rule_text(if_in), paste(
    paste0("When C is one of true (the list also holds ", na_text, "), V must not be empty."),
    "Missing values:",
    paste0(bul, "If C is missing, the condition counts as met."),
    paste0(bul, "If V is missing in a row that meets the condition, the rule is violated."),
    sep = "\n"
  ))
  for (k in list(c(NA, TRUE), c(NA_character_, "TRUE"), c(NA_real_, 1))) {
    df <- data.frame(C = k, V = NA_character_, stringsAsFactors = FALSE)
    expect_identical(dta_condition_violated(if_in, df)[[1]], TRUE, info = class(k))
  }

  # THEN, text column and number column: a missing C passes.
  then_in <- cond_rule(list(A = list(equals = "a")), list(C = list(`in` = y$b)))
  expect_identical(
    first_line(then_in),
    paste0("When A is equal to ", q("a"), ", C must be one of 1 (the list also holds ", na_text, ").")
  )
  expect_true(grepl("If C is missing, the requirement counts as met.", .rule_text(then_in), fixed = TRUE))
  expect_false(grepl("unless C holds numbers", .rule_text(then_in), fixed = TRUE))
  expect_identical(
    dta_condition_violated(then_in, data.frame(A = "a", C = c(NA, "1", "2"))),
    c(FALSE, FALSE, TRUE)
  )
  expect_identical(
    dta_condition_violated(then_in, data.frame(A = "a", C = c(NA, 1, 2))),
    c(FALSE, FALSE, TRUE)
  )

  # not_in: the opposite, on the IF.
  if_not <- cond_rule(list(C = list(not_in = y$a)), list(V = list(empty = FALSE)))
  expect_true(grepl(
    "If C is missing, the condition is not met and the row is not checked.",
    .rule_text(if_not),
    fixed = TRUE
  ))
  expect_identical(
    dta_condition_violated(if_not, data.frame(C = c(NA, "x"), V = NA_character_)),
    c(FALSE, TRUE)
  )

  # Only a missing text entry.
  only <- cond_rule(list(C = list(`in` = list(NA_character_, NULL))), list(V = list(empty = FALSE)))
  expect_identical(first_line(only), paste0(
    "When C is one of the entries of a list that holds only ", na_text, ", V must not be empty."
  ))

  # Next to another missing entry, that one is named apart; the text entry
  # decides the outcome.
  both <- cond_rule(list(A = list(equals = "a")), list(C = list(`in` = list("x", NA, NA_character_))))
  expect_identical(first_line(both), paste0(
    "When A is equal to ", q("a"), ", C must be one of ", q("x"), " (the list also holds ",
    na_text, ", and another missing entry, which matches only the text ", q("NA"), ")."
  ))
  expect_identical(
    dta_condition_violated(both, data.frame(A = "a", C = c(NA, "NA", "y"))),
    c(FALSE, FALSE, TRUE)
  )
  both_num <- cond_rule(list(A = list(equals = "a")), list(C = list(`in` = list(1, NA, NA_character_))))
  expect_true(grepl(paste0(
    na_text, ", and another missing entry, which matches a missing value if C holds numbers ",
    "and otherwise only the text ", q("NA")
  ), first_line(both_num), fixed = TRUE))
  expect_true(grepl("If C is missing, the requirement counts as met.", .rule_text(both_num), fixed = TRUE))
  expect_identical(
    dta_condition_violated(both_num, data.frame(A = "a", C = c(NA, "NA", "y"))),
    c(FALSE, FALSE, TRUE)
  )
  expect_identical(
    dta_condition_violated(both_num, data.frame(A = "a", C = c(NA, 1, 2))),
    c(FALSE, FALSE, TRUE)
  )

  # A group condition says the same.
  g <- DTARuleGroupCondition(
    id = "g", group_by = "S",
    conditions = list(c1 = list(C = list(`in` = y$a)), c2 = list(V = list(empty = TRUE))),
    constraints = list(list(id = "k", type = "requires", `if` = "c2", then = "c1", then_scope = "all"))
  )
  expect_true(grepl("A row in which C is missing meets condition c1.", .rule_text(g), fixed = TRUE))
  expect_true(rule_check_group_condition(g, data.frame(S = "s", C = NA_real_, V = NA))$valid)
  expect_true(rule_check_group_condition(g, data.frame(S = "s", C = NA_character_, V = NA))$valid)
  expect_false(rule_check_group_condition(g, data.frame(S = "s", C = "x", V = NA))$valid)
})

test_that("the missing-value notes match what the evaluator does", {
  ops <- list(
    list(equals = "a"), list(not_equals = "a"), list(`in` = c("a", "b")),
    list(`in` = c("a", NA)), list(`in` = list("a", NULL)), list(not_in = c("a", "b")),
    list(not_in = c("a", NA)), list(pattern = "a"), list(empty = TRUE), list(empty = FALSE),
    list(greater = 1), list(less = 1), list(min = 1), list(max = 1), list(range = c(0, 2))
  )
  for (op in ops) {
    numeric <- names(op)[[1]] %in% c("greater", "less", "min", "max", "range")
    b_missing <- if (numeric) NA_real_ else NA_character_

    # THEN: the IF holds, B is missing.
    then_rule <- cond_rule(list(A = list(equals = "x")), list(B = op))
    df <- data.frame(A = "x", B = b_missing, stringsAsFactors = FALSE)
    passes <- rule_check_col_condition(then_rule, df)$valid
    says_met <- grepl("If B is missing, the requirement counts as met.", .rule_text(then_rule), fixed = TRUE)
    says_violated <- grepl("If B is missing in a row that meets the condition, the rule is violated.", .rule_text(then_rule), fixed = TRUE)
    expect_identical(says_met, passes, info = names(op)[[1]])
    expect_identical(says_violated, !passes, info = names(op)[[1]])

    # IF: B is missing, the THEN fails.
    if_rule <- cond_rule(list(B = op), list(A = list(equals = "y")))
    in_scope <- !rule_check_col_condition(if_rule, df)$valid
    says_not_checked <- grepl("If B is missing, the condition is not met and the row is not checked.", .rule_text(if_rule), fixed = TRUE)
    says_counts <- grepl("If B is missing, the condition counts as met.", .rule_text(if_rule), fixed = TRUE)
    expect_identical(says_not_checked, !in_scope, info = names(op)[[1]])
    expect_identical(says_counts, in_scope, info = names(op)[[1]])

    # Group condition: a missing value meets the condition or not.
    group <- DTARuleGroupCondition(
      id = "g", group_by = "G",
      conditions = list(c1 = list(B = op), c2 = list(A = list(equals = "never"))),
      constraints = list(list(type = "requires", `if` = "c1", then = "c2"))
    )
    gdf <- data.frame(G = "1", A = "x", B = b_missing, stringsAsFactors = FALSE)
    met <- !rule_check_group_condition(group, gdf)$valid
    says_not_met <- grepl("A row in which B is missing does not meet condition c1.", .rule_text(group), fixed = TRUE)
    says_meets <- grepl("A row in which B is missing meets condition c1.", .rule_text(group), fixed = TRUE)
    expect_identical(says_not_met, !met, info = names(op)[[1]])
    expect_identical(says_meets, met, info = names(op)[[1]])
  }
})

test_that("'this part of' appears only when a clause has several columns", {
  rule <- cond_rule(
    list(A = list(empty = TRUE), C = list(empty = TRUE)),
    list(B = list(empty = TRUE), D = list(empty = TRUE))
  )
  text <- .rule_text(rule)
  expect_true(grepl("If A is missing, this part of the condition counts as met.", text, fixed = TRUE))
  expect_true(grepl("If B is missing, this part of the requirement counts as met.", text, fixed = TRUE))
  group <- DTARuleGroupCondition(
    id = "g", group_by = "G",
    conditions = list(c1 = list(B = list(empty = TRUE), C = list(empty = TRUE)), c2 = list(A = list(equals = "x"))),
    constraints = list(list(type = "requires", `if` = "c1", then = "c2"))
  )
  expect_true(grepl("If B is missing, this part of condition c1 counts as met.", .rule_text(group), fixed = TRUE))
})

# Regression (review finding W2): a missing IF value does not always leave
# the row out -- when another column of the condition holds a value that is
# not a number, the evaluator keeps the row in scope.
test_that("a missing IF value is not checked unless another column is not a number", {
  rule <- cond_rule(
    list(A = list(equals = "x"), B = list(greater = 1)),
    list(V = list(empty = FALSE))
  )
  expect_true(grepl(paste0(
    "If A is missing, the condition is not met and the row is not checked, unless another ",
    "column of the condition holds a value that is not a number and no other test of the ",
    "condition fails."
  ), .rule_text(rule), fixed = TRUE))
  df <- data.frame(A = NA_character_, B = c("abc", "5", "0"), V = NA_character_, stringsAsFactors = FALSE)
  # Row 1 (B not a number) is checked; rows 2 and 3 (B a number) are not.
  expect_identical(dta_condition_violated(rule, df), c(TRUE, FALSE, FALSE))
  # A test that clearly fails still leaves the row out.
  three <- cond_rule(
    list(A = list(equals = "x"), B = list(greater = 1), C = list(equals = "x")),
    list(V = list(empty = FALSE))
  )
  df3 <- data.frame(A = NA_character_, B = "abc", C = c("x", "y"), V = NA_character_, stringsAsFactors = FALSE)
  expect_identical(dta_condition_violated(three, df3), c(TRUE, FALSE))
  # Without another numeric column there is no exception to state.
  plain <- cond_rule(list(A = list(equals = "x"), C = list(equals = "y")), list(V = list(empty = FALSE)))
  expect_false(grepl("unless", .rule_text(plain), fixed = TRUE))
  # A missing value that fails its test outright (in, pattern) is never checked.
  outright <- cond_rule(list(A = list(`in` = "x"), B = list(greater = 1)), list(V = list(empty = FALSE)))
  expect_true(grepl(
    "If A is missing, the condition is not met and the row is not checked.\n",
    .rule_text(outright),
    fixed = TRUE
  ))
  expect_identical(dta_condition_violated(outright, df), c(FALSE, FALSE, FALSE))
})

test_that("the non-number notes match what the evaluator does", {
  df <- data.frame(A = "x", B = "ninety", stringsAsFactors = FALSE)
  then_rule <- cond_rule(list(A = list(equals = "x")), list(B = list(greater = 1)))
  expect_false(rule_check_col_condition(then_rule, df)$valid)
  expect_true(grepl(
    "A value in B that is not a number, in a row that meets the condition, violates the rule.",
    .rule_text(then_rule),
    fixed = TRUE
  ))

  if_rule <- cond_rule(list(B = list(greater = 1)), list(A = list(equals = "y")))
  expect_false(rule_check_col_condition(if_rule, df)$valid)
  expect_true(grepl(
    "A value in B that is not a number does not by itself exempt the row from the check.",
    .rule_text(if_rule),
    fixed = TRUE
  ))

  range <- DTARuleColRange(id = "r", columns = "B", min = 0, max = 100)
  expect_false(rule_check_range(range, df)$valid)
  expect_true(grepl("A value that is present but is not a number violates the rule.", .rule_text(range), fixed = TRUE))
  expect_true(rule_check_range(range, data.frame(B = NA_real_))$valid)
})

# Regression (review finding W6): a numeric test reads a text bound as a
# number; a text bound that is not a number is a Problem, and the evaluator
# then lets no number pass.
test_that("a text bound of a numeric test reads as the number it is, or as a problem", {
  as_text <- cond_rule(list(K = list(min = "5")), list(V = list(empty = FALSE)))
  as_num <- cond_rule(list(K = list(min = 5)), list(V = list(empty = FALSE)))
  expect_identical(.rule_text(as_text), .rule_text(as_num))
  expect_identical(first_line(as_text), "When K is at least 5, V must not be empty.")
  df <- data.frame(K = c("4", "5", "10", NA, "abc"), V = NA_character_, stringsAsFactors = FALSE)
  expect_identical(dta_condition_violated(as_text, df), dta_condition_violated(as_num, df))

  bad_then <- cond_rule(list(V = list(empty = TRUE)), list(K = list(min = "abc")))
  expect_identical(.rule_text(bad_then), paste(
    paste0("When V is empty, K must be at least ", q("abc"), "."),
    paste0(
      "Problem: The test ", sq("min"), " on column K is given ", q("abc"),
      ", which is not a number: no value passes this test, and if K holds dates, ",
      "the validation stops with an error."
    ),
    "Missing values:",
    paste0(bul, "If V is missing, the condition counts as met."),
    paste0(bul, "If K is missing in a row that meets the condition, the rule is violated."),
    "Non-numeric values: A value in K that is not a number, in a row that meets the condition, violates the rule.",
    sep = "\n"
  ))
  expect_false(grepl("cannot be evaluated", .rule_text(bad_then), fixed = TRUE))
  expect_false(.rule_describe(bad_then)$valid)
  # THEN: every row is violated, numbers included.
  expect_true(all(dta_condition_violated(bad_then, df)))
  expect_true(all(dta_condition_violated(bad_then, data.frame(K = c(1, 1e9), V = NA_character_))))
  # IF: no number meets it; only a value that is not a number keeps its row.
  bad_if <- cond_rule(list(K = list(min = "abc")), list(V = list(empty = FALSE)))
  expect_identical(dta_condition_violated(bad_if, df), c(FALSE, FALSE, FALSE, FALSE, TRUE))

  two <- cond_rule(list(V = list(empty = TRUE)), list(L = list(range = list("x", 10))))
  expect_true(grepl(paste0(
    "The test ", sq("range"), " on column L is given ", q("x"), ", which is not a number"
  ), .rule_text(two), fixed = TRUE))
  expect_true(all(suppressWarnings(dta_condition_violated(two, data.frame(L = c(1, 5), V = NA_character_)))))
})

# Regression (review round 2, K1): on dates a number bound counts days (seconds
# with a time), a text bound must be a date, and any other text -- even one
# that reads as a number -- stops the validation.
test_that("dates: number bounds count days or seconds, text bounds must be dates", {
  then_min <- function(bound) cond_rule(list(V = list(empty = TRUE)), list(K = list(min = bound)))
  days <- data.frame(K = as.Date(c("1970-01-01", "1970-01-02")), V = NA)
  secs <- data.frame(K = as.POSIXct(c(0.5, 2), origin = "1970-01-01", tz = "UTC"), V = NA)
  # a number: days in a date column, seconds in a date-time column.
  expect_identical(dta_condition_violated(then_min(1), days), c(TRUE, FALSE))
  expect_identical(dta_condition_violated(then_min(1), secs), c(TRUE, FALSE))
  guide <- paste(.rule_reading_guide(), collapse = " ")
  expect_true(grepl(paste0(
    "a bound written as a number counts the days since 1 January 1970 (in a column of dates ",
    "with a time, the seconds since the start of 1 January 1970 in UTC), and a bound written as text ",
    "is read as a date written like 2024-01-31 (in a column of dates with a time, as midnight ",
    "in the time zone of the computer that runs the validation); any other text stops the ",
    "validation with an error, even text that reads as a number, such as ", q("5"), "."
  ), guide, fixed = TRUE))
  # a text that reads as a number reads "at least 5", and stops on dates.
  expect_identical(first_line(then_min("5")), "When V is empty, K must be at least 5.")
  expect_error(dta_condition_violated(then_min("5"), days), class = "simpleError")
  expect_error(dta_condition_violated(then_min("5"), secs), class = "simpleError")
  # a date: works on dates, no number passes it.
  date_bound <- then_min("1970-01-02")
  expect_true(grepl(paste0(
    "Problem: The test ", sq("min"), " on column K is given ", q("1970-01-02"),
    ", which is not a number: no value passes this test, unless K holds dates."
  ), .rule_text(date_bound), fixed = TRUE))
  expect_identical(dta_condition_violated(date_bound, days), c(TRUE, FALSE))
  expect_identical(dta_condition_violated(date_bound, data.frame(K = c(1, 1e9), V = NA)), c(TRUE, TRUE))
  # other text: stops on dates and date-times.
  expect_error(dta_condition_violated(then_min("abc"), days), class = "simpleError")
  expect_error(dta_condition_violated(then_min("abc"), secs), class = "simpleError")
  # a date and a text that is no date: the text decides.
  mixed <- cond_rule(list(V = list(empty = TRUE)), list(K = list(range = c("1970-01-01", "abc"))))
  expect_true(grepl(paste0(
    "Problem: The test ", sq("range"), " on column K is given ", q("1970-01-01"), " and ", q("abc"),
    ", which are not numbers: no value passes this test, and if K holds dates, the validation ",
    "stops with an error."
  ), .rule_text(mixed), fixed = TRUE))
  expect_error(dta_condition_violated(mixed, days), class = "simpleError")
  # bounds given as a list of mixed types: a date column reads them as
  # numbers, so a text bound lets no date pass either (no error).
  listed <- cond_rule(list(V = list(empty = TRUE)), list(K = list(range = list(0, "1970-01-02"))))
  expect_true(grepl(paste0(
    "Problem: The test ", sq("range"), " on column K is given ", q("1970-01-02"),
    ", which is not a number: no value passes this test, in a column of dates as well."
  ), .rule_text(listed), fixed = TRUE))
  expect_true(all(suppressWarnings(dta_condition_violated(listed, days))))
  expect_true(all(suppressWarnings(dta_condition_violated(listed, data.frame(K = c(0, 1), V = NA)))))
})

# Regression (review finding W5): a message that is not a single text is
# shown in full, never aborts the wording, and its effect on the evaluator
# (validation stops when the requirement fails) is stated.
test_that("a requirement message that is not a single text is shown in full", {
  make <- function(message) {
    DTARuleGroupCondition(
      id = "g", group_by = "S",
      conditions = list(c1 = list(A = list(greater = 1)), c2 = list(A = list(less = 0))),
      constraints = list(list(id = "k", type = "requires", `if` = "c1", then = "c2", message = message))
    )
  }
  problem <- paste0(
    "Problem: Requirement k has a message that is not a single text: when a group fails ",
    "it, the validation stops with an error instead of reporting the violation."
  )
  vec <- make(c("one", "two"))
  text <- .rule_text(vec)
  expect_true(grepl(paste0(
    "c2. Its message is given as ", q("one"), " and ", q("two"), " instead of a single text."
  ), text, fixed = TRUE))
  expect_true(grepl(problem, text, fixed = TRUE))
  lst <- make(list("one", "two"))
  expect_true(grepl(paste0("given as ", q("one"), " and ", q("two"), " instead"), .rule_text(lst), fixed = TRUE))
  expect_true(grepl(problem, .rule_text(lst), fixed = TRUE))
  # The evaluator: a failing group stops it (unclassed error, not a report).
  df <- data.frame(S = "a", A = 5)
  # (A base vapply() error; its text is translated, so only its class is
  # matched.)
  expect_error(rule_check_group_condition(vec, df), class = "simpleError")
  expect_error(rule_check_group_condition(lst, df), class = "simpleError")
  expect_error(apply_rules(list(vec), df, verbose = FALSE), class = "simpleError")
  # A single text is reported as written.
  one <- make("one")
  expect_true(grepl(paste0("A violation is reported with the message ", q("one"), "."), .rule_text(one), fixed = TRUE))
  expect_false(grepl("Problem", .rule_text(one), fixed = TRUE))
  expect_match(rule_check_group_condition(one, df)$message, "one", fixed = TRUE)
})

test_that("an unsupported requirement type is a problem of its own, not an abort", {
  rule <- S7::set_props(group_rule(), constraints = list(
    list(id = "k", type = "weird", left = "c_failed", message = "m")
  ))
  text <- .rule_text(rule)
  expect_true(grepl(paste0(
    "k: This requirement has the unsupported type ", q("weird"), "; it is never checked. ",
    "Its other settings are left ", q("c_failed"), " and message ", q("m"), "."
  ), text, fixed = TRUE))
  expect_true(grepl("Problem: Requirement k has an unsupported type and is never checked.", text, fixed = TRUE))
  expect_false(grepl("cannot be evaluated", text, fixed = TRUE))
  # The evaluator skips it: the rule runs and passes.
  df <- data.frame(SUBJECT_ID = "1", VISIT = "1", STATUS = "FAILED", RESULT = "x")
  expect_true(rule_check_group_condition(rule, df)$valid)
})

test_that("numbers outside the plain range read in words, with both signs", {
  rule <- DTARuleColRange(id = "r", columns = "X", min = -1e-8, max = Inf)
  expect_identical(
    first_line(rule),
    "Every value in column X must be a number that is at least minus 1 times 10 to the power of minus 8."
  )
})

# Regression (review findings W3, W4, W7, K1, K4, K5): the reading guide and
# the unique rule state what the evaluator does with numbers, text, patterns,
# dates and blanks.
test_that("the evaluator behaves as the reading guide and the unique rule say", {
  cv <- function(condition, df) {
    dta_condition_violated(cond_rule(condition, list(V = list(empty = FALSE))), df)
  }
  # W3: a number compared as text is written in R's short form.
  txt <- data.frame(K = c("100000", "1e+05", "0.0001", "1e-04"), V = NA_character_, stringsAsFactors = FALSE)
  expect_identical(cv(list(K = list(equals = 0.0001)), txt), c(FALSE, FALSE, FALSE, TRUE))
  expect_identical(cv(list(K = list(equals = 100000)), txt), c(FALSE, TRUE, FALSE, FALSE))
  expect_identical(cv(list(K = list(equals = 100000L)), txt), c(TRUE, FALSE, FALSE, FALSE))
  # K2: on a column of numbers, a listed text that reads as a number compares as a number ...
  num <- data.frame(K = c(1e5, 2), V = NA_character_)
  expect_identical(cv(list(K = list(`in` = c("100000", "3"))), num), c(TRUE, FALSE))
  # ... but not when one listed value does not.
  expect_identical(cv(list(K = list(`in` = c("1e5", "x"))), num), c(FALSE, FALSE))
  # W4: a pattern sees the value as text.
  expect_identical(cv(list(K = list(pattern = "^1e\\+05$")), num), c(TRUE, FALSE))
  expect_identical(cv(list(K = list(pattern = "^TRUE$")), data.frame(K = c(TRUE, FALSE), V = NA)), c(TRUE, FALSE))
  # K3: case-sensitive unless the pattern says otherwise.
  ab <- data.frame(K = c("a", "A"), V = NA_character_, stringsAsFactors = FALSE)
  expect_identical(cv(list(K = list(pattern = "a")), ab), c(TRUE, FALSE))
  expect_identical(cv(list(K = list(pattern = "(?i)a")), ab), c(TRUE, TRUE))
  # W7: on dates, a number counts days since 1 January 1970, text is a date
  # (seconds and text that is not a date: see the test of K1 above).
  dates <- data.frame(K = as.Date(c("1970-01-11", "1970-01-09")), V = NA)
  expect_identical(cv(list(K = list(min = 10)), dates), c(TRUE, FALSE))
  expect_identical(cv(list(K = list(min = "1970-01-10")), dates), c(TRUE, FALSE))
  expect_error(cv(list(K = list(min = "abc")), dates), class = "simpleError")
  # K1: a point is the decimal separator; comma and thousands separators are not numbers.
  s <- dta_as_numeric_strict(c("1234.5", "1,5", "1 000", " 5 ", "1e3", "0x10"))
  expect_identical(s$values, c(1234.5, NA, NA, 5, 1000, 16))
  expect_identical(s$unconvertible, c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
  # K4: only ordinary whitespace is empty; a non-breaking space is not.
  blanks <- data.frame(K = c(" \t\r\n", "\u00a0", ""), V = NA_character_, stringsAsFactors = FALSE)
  expect_identical(cv(list(K = list(empty = TRUE)), blanks), c(TRUE, FALSE, TRUE))
  # K5: unique compares exactly, 0 equals minus 0, NaN is not missing.
  expect_identical(dta_count_duplicates(data.frame(X = c(0, -0)), "X"), 1L)
  expect_identical(dta_count_duplicates(data.frame(X = c(0.1 + 0.2, 0.3)), "X"), 0L)
  expect_identical(dta_count_duplicates(data.frame(X = c(NaN, NA)), "X"), 0L)
  expect_identical(dta_count_duplicates(data.frame(X = c(NaN, NaN)), "X"), 1L)
  expect_identical(dta_count_duplicates(data.frame(X = c(NA, NA)), "X"), 1L)
})

# --- Completeness and mutation over random rules ------------------------------

# An independent mini formatter: the expected rendering of a leaf, written
# without calling any function of R/rulePhrasing.R.
mini_text <- function(x) {
  x <- gsub("\n", "[line break]", x, fixed = TRUE)
  paste0(lq, x, rq)
}
mini_name <- function(x) if (grepl("^[A-Za-z][A-Za-z0-9_.]*$", x)) x else paste0(lsq, x, rsq)

# Numbers are drawn as their canonical text, so the expected rendering is the
# text itself.
number_pool <- c("0", "1", "17", "-3", "0.5", "-12.25", "1000000", "0.001", "123.456", "65")
text_pool <- c("V03", "a b", "say \"hi\"", "x\ny", "", "18", "COMPLETED", "N/A", "*bold*", "a|b")
name_pool <- c("AGE", "VISIT", "SUBJECT_ID", "my col", "Weight.kg", "STATUS", "B2", "x_1")

rand_number <- function() sample(number_pool, 1)
rand_text <- function() sample(text_pool, 1)

# A random operator map for one column; returns the map and the substrings
# its leaves must produce.
rand_ops <- function() {
  kind <- sample(c("equals", "not_equals", "in", "not_in", "greater", "less", "min", "max", "range", "pattern", "empty"), 1)
  num <- rand_number()
  txt <- rand_text()
  use_num <- runif(1) < 0.5
  value <- if (use_num) as.numeric(num) else txt
  shown <- if (use_num) num else mini_text(txt)
  switch(kind,
    equals = ,
    not_equals = ,
    pattern = list(op = stats::setNames(list(if (kind == "pattern") txt else value), kind), expect = if (kind == "pattern") mini_text(txt) else shown),
    `in` = ,
    not_in = {
      nums <- sample(number_pool, 2)
      txts <- sample(text_pool, 2)
      vals <- list(as.numeric(nums[[1]]), txts[[1]], as.numeric(nums[[2]]), txts[[2]])
      list(op = stats::setNames(list(vals), kind), expect = c(nums, mini_text(txts)))
    },
    greater = ,
    less = ,
    min = ,
    max = list(op = stats::setNames(list(as.numeric(num)), kind), expect = num),
    range = {
      b <- sort(as.numeric(sample(number_pool, 2)))
      list(op = list(range = b), expect = number_pool[match(b, as.numeric(number_pool))])
    },
    empty = {
      flag <- runif(1) < 0.5
      list(op = list(empty = flag), expect = "empty")
    }
  )
}

rand_clause <- function() {
  cols <- sample(name_pool, sample(1:3, 1))
  map <- list()
  expect <- character(0)
  for (col in cols) {
    o <- rand_ops()
    map[[col]] <- o$op
    expect <- c(expect, mini_name(col), o$expect)
  }
  list(map = map, expect = expect)
}

rand_rule <- function(i) {
  type <- sample(c("col_range", "col_unique", "col_condition", "group_condition"), 1)
  description <- if (runif(1) < 0.5) paste("Purpose text", i) else NULL
  expect <- if (is.null(description)) character(0) else description
  id <- paste0("rule_", i)
  if (type == "col_range") {
    b <- sort(as.numeric(sample(number_pool, 2)))
    col <- sample(name_pool, 1)
    rule <- DTARuleFactory(id, type, description = description, columns = col, min = b[[1]], max = b[[2]])
    expect <- c(expect, mini_name(col), number_pool[match(b, as.numeric(number_pool))])
  } else if (type == "col_unique") {
    cols <- sample(name_pool, sample(1:3, 1))
    rule <- DTARuleFactory(id, type, description = description, columns = cols)
    expect <- c(expect, vapply(cols, mini_name, character(1)))
  } else if (type == "col_condition") {
    a <- rand_clause()
    b <- rand_clause()
    rule <- DTARuleFactory(id, type, description = description, condition = a$map, then = b$map)
    expect <- c(expect, a$expect, b$expect)
  } else {
    group_by <- sample(name_pool, sample(1:2, 1))
    n_cond <- sample(2:3, 1)
    cond_names <- paste0("c", seq_len(n_cond), "_", i)
    conditions <- list()
    for (nm in cond_names) {
      cl <- rand_clause()
      conditions[[nm]] <- cl$map
      expect <- c(expect, nm, cl$expect)
    }
    constraints <- list()
    for (k in seq_len(sample(1:2, 1))) {
      pair <- sample(cond_names, 2)
      scopes <- sample(c("any", "all"), 2, replace = TRUE)
      scope_text <- ifelse(scopes == "all", "every row", "at least one row")
      msg <- if (runif(1) < 0.5) rand_text() else NULL
      cid <- paste0("k", k)
      if (runif(1) < 0.5) {
        cst <- list(id = cid, type = "requires", `if` = pair[[1]], then = pair[[2]], if_scope = scopes[[1]], then_scope = scopes[[2]])
        expect <- c(expect, "must meet condition")
      } else {
        cst <- list(id = cid, type = "mutually_exclusive", left = pair[[1]], right = pair[[2]], left_scope = scopes[[1]], right_scope = scopes[[2]])
        expect <- c(expect, "must not happen")
      }
      cst$message <- msg
      constraints[[k]] <- cst
      expect <- c(expect, paste0(cid, ":"), pair, scope_text, if (!is.null(msg)) mini_text(msg))
    }
    rule <- DTARuleFactory(id, type, description = description, group_by = group_by, conditions = conditions, constraints = constraints)
    expect <- c(expect, vapply(group_by, mini_name, character(1)))
  }
  list(rule = rule, expect = unique(expect))
}

# Every leaf of as.list(rule), except type, id and storage type.
leaves <- function(x, path = character(0)) {
  if (is.list(x)) {
    out <- list()
    for (i in seq_along(x)) {
      nm <- names(x)[i] %||% as.character(i)
      if (is.na(nm) || !nzchar(nm)) nm <- as.character(i)
      out <- c(out, leaves(x[[i]], c(path, nm)))
    }
    return(out)
  }
  if (length(path) == 1L && path %in% c("type", "id")) {
    return(list())
  }
  lapply(seq_along(x), function(i) list(path = c(path, as.character(i)), value = x[[i]]))
}

random_rules <- withr::with_seed(42, lapply(seq_len(300), rand_rule))

test_that("every random rule names every leaf in its plain text", {
  for (r in random_rules) {
    text <- .rule_text(r$rule)
    for (e in r$expect) {
      expect_true(grepl(e, text, fixed = TRUE), info = paste(r$rule@id, "missing", e))
    }
    expect_true(.rule_describe(r$rule)$valid, info = r$rule@id)
  }
})

test_that("the expectations of the random rules cover every leaf of as.list()", {
  # Guards the generator: each leaf value (other than type, id, scopes,
  # constraint types and logicals, which map to phrases) is in the expected
  # set of its rule, rendered as the generator rendered it.
  for (r in random_rules) {
    ls <- leaves(as.list(r$rule))
    for (leaf in ls) {
      last <- leaf$path[[length(leaf$path) - 1L]]
      v <- leaf$value
      if (last %in% c("type", "if_scope", "then_scope", "left_scope", "right_scope") || is.logical(v)) {
        next
      }
      top <- leaf$path[[1]]
      rendered <- if (is.numeric(v)) {
        number_pool[match(v, as.numeric(number_pool))]
      } else if (identical(top, "description")) {
        v
      } else if (top %in% c("columns", "group_by")) {
        mini_name(v)
      } else if (identical(top, "constraints") && identical(last, "id")) {
        paste0(v, ":")
      } else if (identical(top, "constraints") && last %in% c("left", "right", "if", "then")) {
        v
      } else {
        mini_text(v)
      }
      expect_true(rendered %in% r$expect, info = paste(r$rule@id, paste(leaf$path, collapse = "/")))
    }
  }
})

# Change one leaf of a rule's list form and rebuild it.
mutate_leaf <- function(x, path, fn) {
  if (length(path) == 1L) {
    idx <- suppressWarnings(as.integer(path))
    x[[idx]] <- fn(x[[idx]])
    return(x)
  }
  key <- path[[1]]
  idx <- if (!is.null(names(x)) && key %in% names(x)) key else as.integer(key)
  x[[idx]] <- mutate_leaf(x[[idx]], path[-1], fn)
  x
}

rebuild <- function(lst) {
  args <- lst[setdiff(names(lst), c("id", "type"))]
  args <- args[!vapply(args, is.null, logical(1))]
  do.call(DTARuleFactory, c(list(id = lst$id, type = lst$type), args))
}

test_that("changing any single leaf changes the wording", {
  for (r in random_rules[1:80]) {
    lst <- as.list(r$rule)
    base <- .rule_text(r$rule)
    for (leaf in leaves(lst)) {
      top <- leaf$path[[1]]
      last <- leaf$path[[length(leaf$path) - 1L]]
      # Condition names and constraint references must stay consistent, so
      # only free leaves are mutated here.
      if (last %in% c("left", "right", "if", "then") && top == "constraints") next
      if (top == "constraints" && last == "type") next
      v <- leaf$value
      fn <- if (is.logical(v)) {
        function(z) !z
      } else if (is.numeric(v)) {
        if (top %in% c("min")) function(z) z - 1 else function(z) z + 1
      } else if (last %in% c("if_scope", "then_scope", "left_scope", "right_scope")) {
        function(z) if (identical(z, "all")) "any" else "all"
      } else {
        function(z) paste0(z, "Z")
      }
      changed <- tryCatch(rebuild(mutate_leaf(lst, leaf$path, fn)), error = function(e) NULL)
      if (is.null(changed)) next
      expect_false(
        identical(.rule_text(changed), base),
        info = paste(r$rule@id, paste(leaf$path, collapse = "/"))
      )
    }
  }
})

test_that("switching a requirement type changes the wording", {
  rule <- group_rule()
  lst <- as.list(rule)
  lst$constraints[[2]] <- list(type = "mutually_exclusive", left = "c_failed", right = "c_reported")
  expect_false(identical(.rule_text(rebuild(lst)), .rule_text(rule)))
})

# --- Formats carry the same text --------------------------------------------

sample_rules <- function() {
  c(
    list(
      group_rule(),
      cond_rule(
        list(A = list(equals = "x*y_z")),
        list(B = list(pattern = "`a|b`"), C = list(`in` = c("<i>", "&amp;"))),
        description = "Check # [link](x) and $5"
      ),
      DTARuleColRange(id = "r_1", columns = "AGE", min = 1, max = 2),
      DTARuleColUnique(id = "u", columns = c("A", "B")),
      DTARule(id = "z", type = "zzz", description = NULL)
    ),
    lapply(random_rules[1:40], `[[`, "rule")
  )
}

expected_fragments <- function(rule) {
  lines <- .rule_lines(rule)
  out <- character(0)
  for (i in seq_len(nrow(lines))) {
    if (nzchar(lines$label[[i]])) out <- c(out, lines$label[[i]])
    if (nzchar(lines$text[[i]])) out <- c(out, lines$text[[i]])
  }
  out
}

html_unescape <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

test_that(".rule_lines is the layout: role, label, text, bullet", {
  lines <- .rule_lines(group_rule())
  expect_identical(names(lines), c("role", "label", "text", "bullet"))
  expect_type(lines$bullet, "logical")
  expect_identical(lines$label[!lines$bullet & !nzchar(lines$text)], c("Conditions", "Requirements", "Missing values"))
})

# The plain text and the lines are the source every format is built from
# (Markdown through .md_rule_block() in R/exportDocuments.R): the lines joined
# as the layout says are the plain text, and neither shows code.
test_that("the lines carry the plain text, with real lists and no code", {
  for (rule in sample_rules()) {
    lines <- .rule_lines(rule)
    joined <- ifelse(
      lines$bullet, paste0(bul, lines$text),
      ifelse(nzchar(lines$label) & nzchar(lines$text), paste0(lines$label, ": ", lines$text),
        ifelse(nzchar(lines$label), paste0(lines$label, ":"), lines$text)
      )
    )
    expect_identical(paste(joined, collapse = "\n"), .rule_text(rule), info = rule@id)
    # No YAML: no operator key followed by a colon.
    expect_false(any(grepl(
      "\\b(equals?|not_equals?|in|not_in|greater(_equal)?|less(_equal)?|min|max|range|pattern|empty)\\s*:",
      lines$text
    )), info = rule@id)
  }
  lines <- .rule_lines(group_rule())
  expect_identical(
    lines$text[lines$bullet][[1]],
    paste0("c_failed: a row in which STATUS is equal to ", q("FAILED"))
  )
})

test_that("HTML carries the plain text, escaped, without code markup", {
  for (rule in sample_rules()) {
    html <- .rule_html_dl(rule)
    frags <- regmatches(html, gregexpr("<(dt|p|li)>(.*?)</\\1>", html, perl = TRUE))[[1]]
    frags <- html_unescape(gsub("^<(dt|p|li)>|</(dt|p|li)>$", "", frags))
    expected <- expected_fragments(rule)
    expect_identical(frags[frags != "Rule"], expected[expected != "Rule"], info = rule@id)
    expect_false(grepl("<code>|<pre>", html), info = rule@id)
    stripped <- gsub("<[^>]+>", "", html)
    expect_false(grepl("<", stripped, fixed = TRUE), info = rule@id)
  }
})

test_that("the Word paragraph carries the plain text with bold labels", {
  for (rule in sample_rules()) {
    para <- .rule_docx_paragraph(rule)
    chunks <- para[[1]]
    expect_identical(paste(chunks$txt, collapse = ""), .rule_text(rule), info = rule@id)
    bold <- chunks$txt[chunks$bold %in% TRUE]
    lines <- .rule_lines(rule)
    labels <- lines$label[nzchar(lines$label)]
    expect_identical(bold, if (length(labels)) paste0(labels, ":") else character(0), info = rule@id)
  }
})

test_that("no format shows a rule as code", {
  for (rule in sample_rules()) {
    lines <- .rule_lines(rule)
    for (out in c(.rule_text(rule), .rule_summary(rule), lines$text, lines$label)) {
      expect_false(grepl("type:", out, fixed = TRUE), info = rule@id)
      expect_false(grepl("col_condition|col_range|group_condition", out), info = rule@id)
    }
  }
})

test_that("the summary is one line without purpose or notes", {
  s <- .rule_summary(cond_rule(
    list(VISIT = list(equals = "V03")),
    list(STATUS = list(empty = FALSE), AGE = list(min = 18)),
    description = "Hidden purpose"
  ))
  expect_identical(s, paste0(
    "When VISIT is equal to ", q("V03"), ", all of the following must hold: ",
    "STATUS must not be empty; AGE must be at least 18."
  ))
})

# Regression (review finding K11): sentence items keep their own period and
# are joined by a space, never ".;".
test_that("the summary joins sentence items without doubled punctuation", {
  s <- .rule_summary(group_rule())
  expect_false(grepl(".;", s, fixed = TRUE))
  expect_identical(s, paste0(
    "Rows are grouped by the combination of values in columns SUBJECT_ID and VISIT; ",
    "within each group, the requirements below must hold. ",
    "Conditions: c_failed: a row in which STATUS is equal to ", q("FAILED"), "; ",
    "c_reported: a row in which RESULT is not empty. ",
    "Requirements: no_result_when_failed: Within a group, it must not happen that at least ",
    "one row meets condition c_failed and also every row meets condition c_reported. ",
    "A violation is reported with the message ", q("Failed tests must not report a result."), ". ",
    "constraint_2: If at least one row of a group meets condition c_failed, then at least ",
    "one row of that group must meet condition c_reported. ",
    "A violation is reported with a message naming the group and its rows."
  ))
})

# Regression (review round 2, K2): a THEN note on a missing value is left out
# when the IF tests the same column and a missing value there never reaches
# the THEN; it stays when the evaluator can reach it.
test_that("a THEN note on a missing value that cannot occur is left out", {
  visit_note <- "If VISIT is missing in a row that meets the condition, the rule is violated."
  eq <- cond_rule(list(VISIT = list(equals = "V03")), list(VISIT = list(pattern = "^V")))
  expect_identical(.rule_text(eq), paste(
    paste0("When VISIT is equal to ", q("V03"), ", VISIT must match the pattern ", q("^V"), "."),
    "Missing values: If VISIT is missing, the condition is not met and the row is not checked.",
    sep = "\n"
  ))
  expect_false(any(dta_condition_violated(eq, data.frame(VISIT = c(NA, "V03", "X")))))
  # A test that gives no (not empty) on the IF: left out as well; another
  # THEN column keeps its note.
  ne <- cond_rule(list(VISIT = list(empty = FALSE)), list(VISIT = list(equals = "V03"), DOSE = list(greater = 0)))
  text <- .rule_text(ne)
  expect_false(grepl("If VISIT is missing in a row", text, fixed = TRUE))
  expect_true(grepl("If DOSE is missing in a row that meets the condition, this part of the requirement", text, fixed = TRUE) ||
    grepl("If DOSE is missing in a row that meets the condition, the rule is violated.", text, fixed = TRUE))
  expect_identical(
    dta_condition_violated(ne, data.frame(VISIT = c(NA, "V03"), DOSE = c(1, NA))),
    c(FALSE, TRUE)
  )
  # A numeric test on the IF alone: a missing value is undecided and leaves
  # the row out.
  num <- cond_rule(list(AGE = list(greater = 17)), list(AGE = list(less = 120)))
  expect_false(grepl("If AGE is missing in a row", .rule_text(num), fixed = TRUE))
  expect_false(any(dta_condition_violated(num, data.frame(AGE = c(NA, ""), stringsAsFactors = FALSE))))
  # Undecided, but another column of the IF compares numbers: a value there
  # that is not a number keeps the row, so the note stays and is true.
  kept <- cond_rule(
    list(VISIT = list(equals = "V03"), Z = list(greater = 1)),
    list(VISIT = list(pattern = "^V"))
  )
  expect_true(grepl(visit_note, .rule_text(kept), fixed = TRUE))
  expect_identical(
    dta_condition_violated(kept, data.frame(VISIT = c(NA, NA), Z = c("abc", "5"))),
    c(TRUE, FALSE)
  )
  # A list that depends on the column can let a missing value through: kept.
  dep <- cond_rule(list(K = list(`in` = list(1, NA))), list(K = list(less = 5)))
  expect_true(grepl("If K is missing in a row that meets the condition, the rule is violated.", .rule_text(dep), fixed = TRUE))
  expect_identical(dta_condition_violated(dep, data.frame(K = c(NA, 1))), c(TRUE, FALSE))
  # A column the IF does not test keeps its note.
  other <- cond_rule(list(A = list(equals = "a")), list(VISIT = list(pattern = "^V")))
  expect_true(grepl(visit_note, .rule_text(other), fixed = TRUE))
  # The example from the review: clinical_dta's rule on VISIT.
  dta <- read_dta_from_yaml(system.file("extdata", "clinical_dta.yaml", package = "DTAtools"))
  rules <- unlist(lapply(dta@datasets, function(ds) ds@specs@rules), recursive = FALSE)
  rule <- Filter(function(r) identical(r@id, "check_col_condition_example"), rules)[[1]]
  lines <- .rule_lines(rule)
  expect_true("If VISIT is missing, the condition is not met and the row is not checked." %in% lines$text)
  expect_false(any(startsWith(lines$text, "If VISIT is missing in a row")))
})

# Regression (review round 2, K8): a grouped condition on a file without rows
# checks only its grouping columns; the reading guide says so.
test_that("a grouped condition passes a file without rows when a condition column is absent", {
  g <- DTARuleGroupCondition(
    id = "g", group_by = "S",
    conditions = list(c1 = list(A = list(equals = "x")), c2 = list(B = list(empty = FALSE))),
    constraints = list(list(id = "k", type = "requires", `if` = "c1", then = "c2"))
  )
  expect_true(rule_check_group_condition(g, data.frame(S = character(0)))$valid)
  expect_true(apply_rules(list(g), data.frame(S = character(0)), verbose = FALSE)[[1]]$valid)
  expect_error(rule_check_group_condition(g, data.frame(S = "a")), class = "dta_rule_not_applicable")
  expect_error(rule_check_group_condition(g, data.frame(A = character(0))), class = "dta_rule_not_applicable")
  cond <- cond_rule(list(A = list(equals = "x")), list(B = list(empty = FALSE)))
  expect_error(rule_check_col_condition(cond, data.frame(S = character(0))), class = "dta_rule_not_applicable")
  expect_true(grepl(
    "except that a grouped condition passes for a file with no rows as long as its grouping columns are present.",
    paste(.rule_reading_guide(), collapse = " "),
    fixed = TRUE
  ))
})

# Regression (review round 2, K7): a multi-line Purpose keeps its line breaks
# in HTML, escaped.
test_that("HTML keeps the line breaks of a multi-line Purpose", {
  rule <- DTARuleColUnique(id = "u", columns = "X", description = "first <b>\nsecond\r\nthird")
  html <- .rule_html_dl(rule)
  expect_true(grepl(
    "<dt>Purpose</dt><dd><p>first &lt;b&gt;<br>second<br>third</p></dd>",
    html,
    fixed = TRUE
  ))
})

# Regression (review round 3, K4): a lone carriage return is a line break
# too, and a final line break adds no empty line.
test_that("HTML turns every kind of line break of a Purpose into <br>", {
  rule <- DTARuleColUnique(id = "u", columns = "X", description = "a\rb\r\nc\nd<i>\n")
  html <- .rule_html_dl(rule)
  expect_true(grepl(
    "<dt>Purpose</dt><dd><p>a<br>b<br>c<br>d&lt;i&gt;</p></dd>",
    html,
    fixed = TRUE
  ))
  expect_false(grepl("\r", html, fixed = TRUE))
})

test_that("the reading guide states the evaluation conventions once", {
  guide <- .rule_reading_guide()
  text <- paste(guide, collapse = " ")
  expect_true(grepl("for every file", text, fixed = TRUE))
  expect_true(grepl("compared exactly", text, fixed = TRUE))
  expect_true(grepl("Perl-compatible", text, fixed = TRUE))
  expect_true(grepl("case-sensitive", text, fixed = TRUE))
  expect_true(grepl(paste(
    "If a column that a rule needs is not present in a file, the rule cannot be evaluated",
    "and fails for that file, except that a grouped condition passes for a file with no",
    "rows as long as its grouping columns are present."
  ), text, fixed = TRUE))
  expect_true(grepl("ordinary spaces, tabs, carriage returns or line breaks", text, fixed = TRUE))
  expect_true(grepl("a non-breaking space is not empty", text, fixed = TRUE))
  expect_true(grepl("with a point as the decimal separator", text, fixed = TRUE))
  expect_true(grepl("decimal comma or a thousands separator", text, fixed = TRUE))
  expect_true(grepl("0.0001 becomes 1e-04 and 100000 can become 1e+05", text, fixed = TRUE))
  expect_true(grepl("Equal to, not equal to, one of and not one of compare numbers as numbers", text, fixed = TRUE))
  expect_true(grepl("checked against the value as text", text, fixed = TRUE))
  expect_true(grepl("case-sensitive unless the pattern itself says otherwise", text, fixed = TRUE))
  expect_true(grepl("a bound written as a number counts the days since 1 January 1970", text, fixed = TRUE))
  expect_true(grepl("Numbers are compared exactly; 0 and minus 0 count as the same.", text, fixed = TRUE))
  expect_false(grepl("`", text, fixed = TRUE))
})

test_that("the source of the wording is ASCII", {
  path <- test_path("..", "..", "R", "rulePhrasing.R")
  skip_if_not(file.exists(path), "R sources not available (installed package)")
  bytes <- readBin(path, "raw", file.info(path)$size)
  expect_false(any(as.integer(bytes) > 127L))
})
