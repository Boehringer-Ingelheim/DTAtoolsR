# Grouped rules. REQ-RULE-022 .. REQ-RULE-028.
#
# A grouped rule says something about a subject rather than about a row: that
# a subject cannot both have failed and have reported a result, or that having
# failed obliges a subject to say why. The unit of judgement is the group, so
# the tests are built around groups whose intended verdict is stated before the
# rule ever runs.

rg_frame <- function(...) data.frame(..., stringsAsFactors = FALSE)

# One rule of each shape, over conditions named "failed" and "reported".
rg_conditions <- function() {
  list(
    failed = list(REASND = list(empty = FALSE)),
    reported = list(REASND = list(empty = TRUE), ORRES = list(empty = FALSE))
  )
}

rg_exclusive <- function(scope_left = NULL, scope_right = NULL) {
  constraint <- list(type = "mutually_exclusive", left = "failed", right = "reported")
  if (!is.null(scope_left)) constraint$left_scope <- scope_left
  if (!is.null(scope_right)) constraint$right_scope <- scope_right
  DTARuleGroupCondition(
    id = "grp_exclusive", group_by = "SUBJ",
    conditions = rg_conditions(), constraints = list(constraint)
  )
}

rg_violated_groups <- function(result) {
  if (isTRUE(result$valid)) {
    return(0L)
  }
  length(result$details %||% list())
}

test_that("OQ-RULE-040 | a grouped rule judges each group, not each row | REQ-RULE-022", {
  # Subject A has a failure on one row and a result on another, so the two
  # conditions hold in the same group even though no single row holds both.
  # Judging rows would find nothing wrong; judging groups is the point.
  frame <- rg_frame(
    SUBJ = c("A", "A", "B"),
    REASND = c("BROKEN", NA_character_, NA_character_),
    ORRES = c(NA_character_, "12", "13")
  )
  result <- rule_check_group_condition(rg_exclusive(), frame)

  qa_step("the rule is violated", FALSE, result$valid)
  qa_step("by exactly one group", 1L, rg_violated_groups(result))
  qa_step(
    "and no single row of that group holds both conditions",
    TRUE,
    all(is.na(frame$REASND) | is.na(frame$ORRES))
  )
})

test_that("OQ-RULE-041 | grouping by several columns partitions on the combination | REQ-RULE-022", {
  rule <- DTARuleGroupCondition(
    id = "grp_two_keys", group_by = c("SUBJ", "VISIT"),
    conditions = rg_conditions(),
    constraints = list(list(type = "mutually_exclusive", left = "failed", right = "reported"))
  )
  # The same subject at two different visits is two groups, so the failure at
  # one visit and the result at the other do not meet.
  separated <- rg_frame(
    SUBJ = c("A", "A"),
    VISIT = c("V1", "V2"),
    REASND = c("BROKEN", NA_character_),
    ORRES = c(NA_character_, "12")
  )
  qa_step(
    "a subject split across two visits is two groups and is not in violation",
    TRUE, rule_check_group_condition(rule, separated)$valid
  )

  together <- rg_frame(
    SUBJ = c("A", "A"),
    VISIT = c("V1", "V1"),
    REASND = c("BROKEN", NA_character_),
    ORRES = c(NA_character_, "12")
  )
  qa_step(
    "while the same rows at one visit are one group and are",
    FALSE, rule_check_group_condition(rule, together)$valid
  )
})

test_that("OQ-RULE-042 | mutually_exclusive reports a group where both conditions hold | REQ-RULE-023", {
  clean <- rg_frame(
    SUBJ = c("A", "B"),
    REASND = c("BROKEN", NA_character_),
    ORRES = c(NA_character_, "12")
  )
  qa_step(
    "a group holding only one of the two conditions is not in violation",
    TRUE, rule_check_group_condition(rg_exclusive(), clean)$valid
  )

  both <- rg_frame(
    SUBJ = c("A", "A"),
    REASND = c("BROKEN", NA_character_),
    ORRES = c(NA_character_, "12")
  )
  qa_step(
    "a group holding both is",
    FALSE, rule_check_group_condition(rg_exclusive(), both)$valid
  )
})

test_that("OQ-RULE-043 | requires reports a group whose antecedent holds without its consequent | REQ-RULE-024", {
  rule <- DTARuleGroupCondition(
    id = "grp_requires", group_by = "SUBJ",
    conditions = list(
      failed = list(REASND = list(empty = FALSE)),
      not_done = list(STAT = list(equals = "NOT DONE"))
    ),
    constraints = list(list(type = "requires", `if` = "failed", then = "not_done"))
  )

  # A failed subject whose status never says so.
  violating <- rg_frame(
    SUBJ = c("A", "B"),
    REASND = c("BROKEN", NA_character_),
    STAT = c("DONE", "DONE")
  )
  qa_step("the obligation is unmet", FALSE, rule_check_group_condition(rule, violating)$valid)

  satisfied <- rg_frame(
    SUBJ = c("A", "B"),
    REASND = c("BROKEN", NA_character_),
    STAT = c("NOT DONE", "DONE")
  )
  qa_step("and is met once the status says so", TRUE, rule_check_group_condition(rule, satisfied)$valid)

  # A subject that never failed carries no obligation at all.
  irrelevant <- rg_frame(
    SUBJ = c("B"),
    REASND = NA_character_,
    STAT = "DONE"
  )
  qa_step(
    "a group whose antecedent never holds is not obliged",
    TRUE, rule_check_group_condition(rule, irrelevant)$valid
  )
})

test_that("OQ-RULE-044 | the any and all scopes decide how much of a group must hold | REQ-RULE-023", {
  # Under "any" one failing row makes the condition hold for the group; under
  # "all" every row must. The difference decides whether a subject with one bad
  # row and one good one is in violation.
  frame <- rg_frame(
    SUBJ = c("A", "A"),
    REASND = c("BROKEN", NA_character_),
    ORRES = c("11", "12")
  )
  qa_step(
    "under the default scope one failing row is enough for the group to hold",
    FALSE, rule_check_group_condition(rg_exclusive(), frame)$valid
  )
  qa_step(
    "while requiring every row to fail lets the group through",
    TRUE, rule_check_group_condition(rg_exclusive(scope_left = "all"), frame)$valid
  )
})

test_that("OQ-RULE-045 | constraint types are accepted under their aliases | REQ-RULE-025", {
  frame <- rg_frame(
    SUBJ = c("A", "A"),
    REASND = c("BROKEN", NA_character_),
    ORRES = c(NA_character_, "12")
  )
  alias <- DTARuleGroupCondition(
    id = "grp_alias", group_by = "SUBJ",
    conditions = rg_conditions(),
    constraints = list(list(type = "not_both", left = "failed", right = "reported"))
  )
  qa_step(
    "not_both reaches the same verdict as mutually_exclusive",
    rule_check_group_condition(rg_exclusive(), frame)$valid,
    rule_check_group_condition(alias, frame)$valid
  )

  implies <- DTARuleGroupCondition(
    id = "grp_implies", group_by = "SUBJ",
    conditions = rg_conditions(),
    constraints = list(list(type = "implies", `if` = "failed", then = "reported"))
  )
  qa_check("implies is accepted as a constraint type", inherits(implies, "DTAtools::DTARuleGroupCondition"))
})

test_that("OQ-RULE-046 | a malformed constraint is rejected when the rule is built | REQ-RULE-026", {
  # This is the case that decides whether a mistyped specification checks
  # nothing or refuses to load. A rule that silently checks nothing is
  # indistinguishable, in the report, from a delivery with nothing wrong.
  unknown_type <- tryCatch(
    DTARuleGroupCondition(
      id = "g", group_by = "SUBJ", conditions = rg_conditions(),
      constraints = list(list(type = "no_such_type", left = "failed", right = "reported"))
    ),
    error = function(e) e
  )
  qa_check(
    "a constraint of an unrecognised type is rejected at construction",
    inherits(unknown_type, "condition")
  )
  qa_step(
    "and the condition names the types that are supported",
    TRUE,
    grepl("mutually_exclusive", paste(conditionMessage(unknown_type), collapse = " "), fixed = TRUE)
  )

  unknown_condition <- tryCatch(
    DTARuleGroupCondition(
      id = "g", group_by = "SUBJ", conditions = rg_conditions(),
      constraints = list(list(type = "requires", `if` = "failed", then = "nonexistent"))
    ),
    error = function(e) e
  )
  qa_check(
    "a constraint naming a condition the rule does not define is rejected too",
    inherits(unknown_condition, "condition")
  )
})

test_that("OQ-RULE-047 | a grouped rule whose grouping column is absent is not applicable | REQ-RULE-027", {
  err <- tryCatch(
    rule_check_group_condition(
      DTARuleGroupCondition(
        id = "g", group_by = "NOPE", conditions = rg_conditions(),
        constraints = list(list(type = "requires", `if` = "failed", then = "reported"))
      ),
      rg_frame(SUBJ = "A", REASND = "BROKEN", ORRES = NA_character_)
    ),
    error = function(e) e
  )
  qa_step(
    "the condition carries the not-applicable class",
    TRUE, inherits(err, "dta_rule_not_applicable")
  )
})

test_that("OQ-RULE-048 | a reported violation names its group and the rows behind it | REQ-RULE-028", {
  frame <- rg_frame(
    SUBJ = c("A", "A", "B"),
    REASND = c("BROKEN", NA_character_, NA_character_),
    ORRES = c(NA_character_, "12", "13")
  )
  result <- rule_check_group_condition(rg_exclusive(), frame)
  detail <- result$details[[1]]

  # Without the group and the rows, a report says only "some subject somewhere
  # is inconsistent", which costs a supplier a search through the whole
  # delivery.
  qa_step(
    "the violation names the group it belongs to",
    TRUE, grepl("A", paste(unlist(detail), collapse = " "), fixed = TRUE)
  )
  qa_check(
    "and carries the rows that produced it",
    length(detail$rows %||% detail[["rows"]] %||% NULL) > 0
  )
  qa_step(
    "the message explains which two conditions collided",
    TRUE,
    all(vapply(
      c("failed", "reported"),
      function(name) grepl(name, paste(unlist(detail), collapse = " "), fixed = TRUE),
      logical(1)
    ))
  )
})

test_that("OQ-RULE-049 | groups are ordered independently of the session locale | REQ-RULE-022", {
  # Group labels are sorted so a report reads in a stable order. Sorting in the
  # session locale would reorder them on a German machine relative to a C one,
  # and two runs of the same delivery would produce reports that diff.
  # Two rows per subject, because one row cannot hold both conditions: the
  # first has failed, the second has reported, and the pair is what puts the
  # group in violation.
  subjects <- c("b", "A", "a", "B")
  frame <- rg_frame(
    SUBJ = rep(subjects, each = 2),
    REASND = rep(c("BROKEN", NA_character_), times = length(subjects)),
    ORRES = rep(c(NA_character_, "12"), times = length(subjects))
  )
  result <- rule_check_group_condition(rg_exclusive(), frame)
  qa_step("every group is in violation", 4L, rg_violated_groups(result))

  labels <- vapply(
    result$details,
    function(d) sub("^.*?([AaBb]).*$", "\\1", paste(unlist(d), collapse = " ")),
    character(1)
  )
  qa_step(
    "and the groups come back in C-collation order, upper case first",
    sort(labels, method = "radix"), labels
  )
})
