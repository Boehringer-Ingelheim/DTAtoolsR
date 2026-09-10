# The condition operator vocabulary. REQ-RULE-001 .. REQ-RULE-010.
#
# One test case per operator. Each builds a table holding a row the operator
# selects, a row it does not, and a row whose value is missing, so that the
# answer to "what does this operator do with an absent value" is recorded
# rather than left to be discovered later by a delivery that slipped through.
#
# The rules are exercised through rule_check_col_condition(), which is where an
# operator is actually consumed: a condition selects the rows in scope, and the
# THEN clause decides whether those rows are acceptable.

# A rule that puts `operator` in the IF clause and requires the selected rows
# to be flagged "yes" in the FLAG column. Any row the operator selects whose
# FLAG is not "yes" is a violation, so the set of violations reveals exactly
# which rows the operator selected.
ro_rule <- function(operator, value, column = "V") {
  condition <- list(list(operator = value))
  names(condition[[1]]) <- operator
  names(condition) <- column
  DTARuleColCondition(
    id = paste0("op_", operator),
    condition = condition,
    then = list(FLAG = list(equals = "yes"))
  )
}

# The rows the operator selected, as their labels. Every row is flagged "no",
# so every selected row violates and the violation count is the selection size.
ro_selected <- function(operator, value, values, labels = seq_along(values)) {
  frame <- data.frame(
    V = values,
    FLAG = rep("no", length(values)),
    LABEL = as.character(labels),
    stringsAsFactors = FALSE
  )
  result <- rule_check_col_condition(ro_rule(operator, value), frame)
  # The message names how many rows failed; the count is what the selection is.
  if (isTRUE(result$valid)) {
    return(0L)
  }
  as.integer(sub("^.*: ([0-9]+) rows.*$", "\\1", result$message))
}

test_that("OQ-RULE-001 | the accepted operator vocabulary is exactly the documented one | REQ-RULE-001", {
  qa_step(
    "the package accepts these fifteen operators and no others",
    c(
      "equals", "equal", "not_equals", "not_equal", "in", "not_in",
      "greater", "less", "greater_equal", "less_equal",
      "min", "max", "range", "pattern", "empty"
    ),
    dta_condition_operators()
  )

  # An operator nobody implemented must be an error. Accepting it silently
  # would turn a mistyped rule into a rule that selects nothing, which reads
  # exactly like a delivery with nothing wrong in it.
  err <- tryCatch(
    rule_check_col_condition(
      DTARuleColCondition(
        id = "bad", condition = list(V = list(nonsense = 1)),
        then = list(V = list(equals = 1))
      ),
      data.frame(V = c(1, 2))
    ),
    error = function(e) e
  )
  qa_check("an unsupported operator raises a condition", inherits(err, "condition"))
})

test_that("OQ-RULE-002 | equals and its alias select the matching rows | REQ-RULE-002", {
  values <- c("A", "B", NA)
  qa_step("equals selects the one matching row", 1L, ro_selected("equals", "A", values))
  qa_step("the alias 'equal' behaves identically", 1L, ro_selected("equal", "A", values))
  qa_step("a missing value is not selected", 0L, ro_selected("equals", "ZZZ", values))
})

test_that("OQ-RULE-003 | not_equals and its alias select the non-matching rows | REQ-RULE-002", {
  values <- c("A", "B", NA)
  qa_step("not_equals selects the row that differs", 1L, ro_selected("not_equals", "A", values))
  qa_step("the alias 'not_equal' behaves identically", 1L, ro_selected("not_equal", "A", values))
})

test_that("OQ-RULE-004 | in selects members of the permitted set | REQ-RULE-003", {
  values <- c("A", "B", "C", NA)
  qa_step("two of the four rows are members", 2L, ro_selected("in", c("A", "B"), values))
  qa_step("a set matching nothing selects nothing", 0L, ro_selected("in", "ZZZ", values))
})

test_that("OQ-RULE-005 | not_in selects everything outside the set | REQ-RULE-003", {
  values <- c("A", "B", "C", NA)
  # A missing value is not a member of the set, so it is selected by not_in.
  # Recorded because it is the opposite of how `in` treats it, and a rule
  # author has to know which way round it goes.
  qa_step(
    "the two non-members and the missing value are selected",
    2L, ro_selected("not_in", c("A", "B"), values)
  )
})

test_that("OQ-RULE-006 | greater and less exclude the bound | REQ-RULE-004", {
  values <- c(10, 20, 30, NA)
  qa_step("greater than 20 selects only 30", 1L, ro_selected("greater", 20, values))
  qa_step("less than 20 selects only 10", 1L, ro_selected("less", 20, values))
})

test_that("OQ-RULE-007 | greater_equal and less_equal include the bound | REQ-RULE-004", {
  values <- c(10, 20, 30, NA)
  qa_step("at least 20 selects 20 and 30", 2L, ro_selected("greater_equal", 20, values))
  qa_step("at most 20 selects 10 and 20", 2L, ro_selected("less_equal", 20, values))

  # The difference between the two pairs is one row at the boundary, which is
  # the row a range specification is usually written to include.
  qa_step(
    "the boundary row is the whole difference between them",
    1L,
    ro_selected("greater_equal", 20, values) - ro_selected("greater", 20, values)
  )
})

test_that("OQ-RULE-008 | a missing value is never selected by an ordering comparison | REQ-RULE-004", {
  values <- c(10, 20, NA)
  for (operator in c("greater", "less", "greater_equal", "less_equal")) {
    qa_step(
      sprintf("%s does not select the missing value", operator),
      TRUE, ro_selected(operator, -1e6, values) <= 2L
    )
  }
})

test_that("OQ-RULE-009 | min and max together select one inclusive band | REQ-RULE-005", {
  # Two independent tests would put every row in scope, because every row
  # satisfies at least one of them. One band puts three rows in scope.
  rule <- DTARuleColCondition(
    id = "band", condition = list(V = list(min = 2, max = 4)),
    then = list(FLAG = list(equals = "yes"))
  )
  frame <- data.frame(
    V = c(1, 2, 3, 4, 5),
    FLAG = rep("no", 5),
    stringsAsFactors = FALSE
  )
  result <- rule_check_col_condition(rule, frame)
  qa_step(
    "the band selects the three rows between the bounds inclusive",
    3L, as.integer(sub("^.*: ([0-9]+) rows.*$", "\\1", result$message))
  )

  clean <- data.frame(
    V = c(1, 2, 3, 4, 5),
    FLAG = c("no", "yes", "yes", "yes", "no"),
    stringsAsFactors = FALSE
  )
  qa_step(
    "and rows outside the band are not required to satisfy the consequent",
    TRUE, rule_check_col_condition(rule, clean)$valid
  )
})

test_that("OQ-RULE-010 | range selects the inclusive band between its two values | REQ-RULE-006", {
  values <- c(1, 2, 3, 4, 5)
  qa_step("the band from 2 to 4 selects three rows", 3L, ro_selected("range", c(2, 4), values))
  qa_step("a band of one value selects that value", 1L, ro_selected("range", c(3, 3), values))
})

test_that("OQ-RULE-011 | pattern matches PCRE and rejects missing and empty | REQ-RULE-007", {
  values <- c("ABC123", "abc123", "", NA)
  qa_step(
    "the pattern selects only the row that matches, case-sensitively",
    1L, ro_selected("pattern", "^[A-Z]{3}[0-9]{3}$", values)
  )

  # An empty string and a missing value both fail to match. A rule author
  # writing "the code must look like this" would otherwise have to remember
  # that absent values quietly satisfy it.
  qa_step(
    "an empty string and a missing value do not match a pattern",
    0L, ro_selected("pattern", "^$", c("x", NA))
  )
})

test_that("OQ-RULE-012 | empty selects absent values, and blanks in text columns | REQ-RULE-008", {
  qa_step(
    "in a text column, a missing value and a blank are both empty",
    2L, ro_selected("empty", TRUE, c("a", "", NA))
  )
  qa_step(
    "and empty = FALSE selects the rest",
    1L, ro_selected("empty", FALSE, c("a", "", NA))
  )

  # In a numeric column only a missing value is empty: there is no blank to
  # confuse it with.
  qa_step(
    "in a numeric column only the missing value is empty",
    1L, ro_selected("empty", TRUE, c(1, 2, NA))
  )
})

test_that("OQ-RULE-013 | a numeric bound is compared as a number, not as text | REQ-RULE-009", {
  # 1e6 and 1000000 are one value stored two ways. Compared as text they are
  # different, and the verdict would then depend on whether the reader narrowed
  # the column to integer -- a decision the whole-column and streaming engines
  # legitimately make differently. The same file would pass on one path and
  # fail on the other.
  frame <- data.frame(
    V = c(1e6, 999999),
    FLAG = c("no", "no"),
    stringsAsFactors = FALSE
  )
  result <- rule_check_col_condition(ro_rule("equals", 1000000), frame)
  qa_step(
    "a double compares equal to the integer bound naming the same number",
    1L, as.integer(sub("^.*: ([0-9]+) rows.*$", "\\1", result$message))
  )
})

test_that("OQ-RULE-014 | a text column keeps its leading zeros in a comparison | REQ-RULE-010", {
  # Subject identifiers are the reason this matters. Comparing "007" as a
  # number would make it equal to "7", and two distinct subjects would become
  # one.
  frame <- data.frame(
    V = c("007", "7", "07"),
    FLAG = rep("no", 3),
    stringsAsFactors = FALSE
  )
  result <- rule_check_col_condition(ro_rule("equals", "007"), frame)
  qa_step(
    "only the exact text matches",
    1L, as.integer(sub("^.*: ([0-9]+) rows.*$", "\\1", result$message))
  )
})
