# The four rule types and the rule set as a whole.
# REQ-RULE-011 .. REQ-RULE-021 and REQ-RULE-029 .. REQ-RULE-032.
#
# Each rule type is checked for what it reports, what it ignores, and what it
# does when it cannot be evaluated at all. The last of those matters most: a
# rule that cannot run is not a rule that passed, and a caller has to be able
# to tell the two apart or an unevaluated constraint reads as a satisfied one.

rt_frame <- function(...) data.frame(..., stringsAsFactors = FALSE)

# The number of rows a rule reported, taken from its message. Zero when the
# rule held.
rt_violations <- function(result) {
  if (isTRUE(result$valid)) {
    return(0L)
  }
  found <- regmatches(result$message, regexpr("[0-9]+", result$message))
  if (length(found) == 0) 0L else as.integer(found)
}

# ---- conditional rules -------------------------------------------------------

test_that("OQ-RULE-020 | a conditional rule reports rows where IF holds and THEN does not | REQ-RULE-011", {
  rule <- DTARuleColCondition(
    id = "adult_status",
    condition = list(AGE = list(greater_equal = 18)),
    then = list(STATUS = list(equals = "OK"))
  )
  # Row 1 satisfies both, row 2 is in scope and fails, row 3 is out of scope
  # and so its failing status is irrelevant.
  frame <- rt_frame(
    AGE = c(20, 20, 10),
    STATUS = c("OK", "BAD", "BAD")
  )
  result <- rule_check_col_condition(rule, frame)

  qa_step("the rule is violated", FALSE, result$valid)
  qa_step("by exactly the one row that was in scope and failed", 1L, rt_violations(result))
})

test_that("OQ-RULE-021 | several operators and several columns are combined conjunctively | REQ-RULE-012", {
  # Two operators on one column: the band is an intersection, not a union.
  banded <- DTARuleColCondition(
    id = "banded",
    condition = list(AGE = list(greater_equal = 18, less_equal = 65)),
    then = list(STATUS = list(equals = "OK"))
  )
  frame <- rt_frame(
    AGE = c(10, 30, 70),
    STATUS = c("BAD", "BAD", "BAD")
  )
  qa_step(
    "only the row satisfying both operators is in scope",
    1L, rt_violations(rule_check_col_condition(banded, frame))
  )

  # Two columns: likewise an intersection.
  paired <- DTARuleColCondition(
    id = "paired",
    condition = list(AGE = list(greater_equal = 18), SEX = list(equals = "F")),
    then = list(STATUS = list(equals = "OK"))
  )
  frame2 <- rt_frame(
    AGE = c(30, 30, 10),
    SEX = c("F", "M", "F"),
    STATUS = c("BAD", "BAD", "BAD")
  )
  qa_step(
    "only the row satisfying both columns is in scope",
    1L, rt_violations(rule_check_col_condition(paired, frame2))
  )
})

test_that("OQ-RULE-022 | a row whose condition cannot be decided is out of scope | REQ-RULE-013", {
  rule <- DTARuleColCondition(
    id = "adult_status",
    condition = list(AGE = list(greater_equal = 18)),
    then = list(STATUS = list(equals = "OK"))
  )
  # The second row's age is unknown. Treating it as in scope would report a
  # violation the data does not support; treating it as out of scope reports
  # nothing, and the missing value is the column specification's business.
  frame <- rt_frame(
    AGE = c(20, NA, 10),
    STATUS = c("OK", "BAD", "BAD")
  )
  qa_step(
    "a missing value in the condition puts the row out of scope",
    TRUE, rule_check_col_condition(rule, frame)$valid
  )
})

# ---- range rules -------------------------------------------------------------

test_that("OQ-RULE-023 | a range rule reports values outside its inclusive bounds | REQ-RULE-014", {
  rule <- DTARuleColRange(id = "age_range", columns = "AGE", range = c(18, 70))
  frame <- rt_frame(AGE = c(18, 70, 17, 71))

  result <- rule_check_range(rule, frame)
  qa_step("the rule is violated", FALSE, result$valid)
  qa_step(
    "by the two values outside the band, while both bounds themselves pass",
    2L, rt_violations(result)
  )
})

test_that("OQ-RULE-024 | a range rule accepts its bounds either way round | REQ-RULE-014", {
  frame <- rt_frame(AGE = c(17, 30, 71))
  by_range <- rule_check_range(
    DTARuleColRange(id = "r", columns = "AGE", range = c(18, 70)), frame
  )
  by_min_max <- rule_check_range(
    DTARuleColRange(id = "r", columns = "AGE", min = 18, max = 70), frame
  )
  qa_step(
    "a range and a min/max pair naming the same band agree",
    rt_violations(by_range), rt_violations(by_min_max)
  )
})

test_that("OQ-RULE-025 | a range rule ignores missing values | REQ-RULE-015", {
  rule <- DTARuleColRange(id = "age_range", columns = "AGE", range = c(18, 70))
  qa_step(
    "a column of nothing but missing values satisfies the rule",
    TRUE, rule_check_range(rule, rt_frame(AGE = c(NA_real_, NA_real_)))$valid
  )
  qa_step(
    "and a missing value alongside a bad one costs only the bad one",
    1L, rt_violations(rule_check_range(rule, rt_frame(AGE = c(NA, 99, 30))))
  )
})

test_that("OQ-RULE-026 | a range rule requires exactly one target column | REQ-RULE-016", {
  # Two columns would make "outside the range" ambiguous: outside for either,
  # or for both? Refusing to guess is the right answer.
  err <- tryCatch(
    rule_check_range(
      DTARuleColRange(id = "r", columns = c("A", "B"), range = c(1, 2)),
      rt_frame(A = 1, B = 2)
    ),
    error = function(e) e
  )
  qa_check("naming two columns raises a condition", inherits(err, "condition"))
})

test_that("OQ-RULE-027 | a range rule whose column is absent is not applicable | REQ-RULE-017", {
  err <- tryCatch(
    rule_check_range(
      DTARuleColRange(id = "r", columns = "NOPE", range = c(1, 2)),
      rt_frame(A = c(1, 2))
    ),
    error = function(e) e
  )
  # The class is what lets a caller distinguish "this rule could not be
  # evaluated" from "this rule passed". Without it, a specification naming a
  # column the delivery never sent would look like a clean result.
  qa_step(
    "the condition carries the not-applicable class",
    TRUE, inherits(err, "dta_rule_not_applicable")
  )
})

# ---- uniqueness rules --------------------------------------------------------

test_that("OQ-RULE-028 | a uniqueness rule keys on the combination of its columns | REQ-RULE-018", {
  rule <- DTARuleColUnique(id = "subj_visit", columns = c("SUBJ", "VISIT"))

  # Neither column is unique on its own; the pair is, except for the repeat.
  frame <- rt_frame(
    SUBJ = c("A", "A", "B"),
    VISIT = c("V1", "V1", "V1")
  )
  qa_step("the repeated pair is reported", 1L, rt_violations(rule_check_unique(rule, frame)))

  distinct <- rt_frame(
    SUBJ = c("A", "A", "B"),
    VISIT = c("V1", "V2", "V1")
  )
  qa_step(
    "while a column repeating within a distinct pair is not",
    TRUE, rule_check_unique(rule, distinct)$valid
  )
})

test_that("OQ-RULE-029 | repeated missing values count as duplicates | REQ-RULE-019", {
  # Pinned as observed. It follows base R, and it is the stricter reading, but
  # it is not the only defensible one: two keys of unknown value are arguably
  # not known to be equal. A rule author needs to know which way it goes.
  rule <- DTARuleColUnique(id = "k_unique", columns = "K")
  frame <- rt_frame(
    K = c("a", NA_character_, NA_character_),
    SITE = c("S01", "S02", "S03")
  )
  qa_step(
    "two missing keys are reported as one duplicate",
    1L, rt_violations(rule_check_unique(rule, frame))
  )
})

test_that("OQ-RULE-030 | a uniqueness rule compares case-sensitively | REQ-RULE-020", {
  rule <- DTARuleColUnique(id = "k_unique", columns = "K")
  qa_step(
    "values differing only in case are distinct",
    TRUE, rule_check_unique(rule, rt_frame(K = c("abc", "ABC")))$valid
  )
})

test_that("OQ-RULE-031 | a uniqueness rule whose column is absent is not applicable | REQ-RULE-021", {
  err <- tryCatch(
    rule_check_unique(DTARuleColUnique(id = "k", columns = "NOPE"), rt_frame(A = c(1, 2))),
    error = function(e) e
  )
  qa_step(
    "the condition carries the not-applicable class",
    TRUE, inherits(err, "dta_rule_not_applicable")
  )
})

# ---- the rule set ------------------------------------------------------------

test_that("OQ-RULE-032 | every rule is evaluated, whatever the ones before it did | REQ-RULE-029", {
  rules <- list(
    DTARuleColRange(id = "first", columns = "A", range = c(0, 1)),
    DTARuleColRange(id = "second", columns = "A", range = c(0, 100)),
    DTARuleColUnique(id = "third", columns = "A")
  )
  # The first rule fails. If evaluation stopped there, a delivery would be
  # fixed one rule at a time, one round trip each.
  results <- apply_rules(rules, rt_frame(A = c(5, 5)), verbose = FALSE)

  qa_step("one result per rule", 3L, length(results))
  qa_step(
    "and each is reported on its own merits",
    c(first = FALSE, second = TRUE, third = FALSE),
    c(
      first = results[[1]]$valid,
      second = results[[2]]$valid,
      third = results[[3]]$valid
    )
  )
})

test_that("OQ-RULE-033 | two rules sharing an identifier are both evaluated | REQ-RULE-029", {
  # Nothing rejects a repeated identifier, so both rules run and both are
  # reported under the same name. Pinned as observed: a reader of the report
  # cannot tell which of the two a result belongs to, which is worth knowing
  # before writing a specification that repeats one.
  rules <- list(
    DTARuleColRange(id = "same", columns = "A", range = c(0, 10)),
    DTARuleColRange(id = "same", columns = "A", range = c(100, 200))
  )
  results <- apply_rules(rules, rt_frame(A = c(5, 5)), verbose = FALSE)

  qa_step("both rules are evaluated", 2L, length(results))
  qa_step(
    "both report under the same identifier",
    c("same", "same"),
    vapply(results, function(x) as.character(x$id), character(1))
  )
  qa_step(
    "and their verdicts differ, so the identifier does not identify the result",
    c(TRUE, FALSE),
    vapply(results, function(x) isTRUE(x$valid), logical(1))
  )
})

test_that("OQ-RULE-034 | the rule factory builds each type and rejects the rest | REQ-RULE-030", {
  built <- list(
    col_condition = DTARuleFactory(
      id = "a", type = "col_condition",
      condition = list(A = list(equals = 1)), then = list(B = list(equals = 2))
    ),
    col_range = DTARuleFactory(id = "b", type = "col_range", columns = "A", range = c(1, 2)),
    col_unique = DTARuleFactory(id = "c", type = "col_unique", columns = "A"),
    group_condition = DTARuleFactory(
      id = "d", type = "group_condition", group_by = "A",
      conditions = list(x = list(B = list(empty = FALSE))),
      constraints = list(list(type = "requires", `if` = "x", then = "x"))
    )
  )
  qa_step(
    "each documented type builds the class that implements it",
    c(
      col_condition = "DTAtools::DTARuleColCondition",
      col_range = "DTAtools::DTARuleColRange",
      col_unique = "DTAtools::DTARuleColUnique",
      group_condition = "DTAtools::DTARuleGroupCondition"
    ),
    vapply(built, function(x) class(x)[[1]], character(1))
  )

  # The check_* spellings are the ones a YAML specification uses.
  qa_step(
    "the check_ spellings build the same class",
    "DTAtools::DTARuleColRange",
    class(DTARuleFactory(id = "e", type = "check_range", columns = "A", range = c(1, 2)))[[1]]
  )

  err <- tryCatch(DTARuleFactory(id = "f", type = "no_such_rule"), error = function(e) e)
  qa_check("an unknown type raises a condition", inherits(err, "condition"))
})

test_that("OQ-RULE-036 | rule_preview summarises a rule set by its identifiers | REQ-RULE-031", {
  make <- function(n) {
    DTAColumnSpecCollection(
      columns = list(A = DTAColumnSpec(id = "A", type = "SAS Num", nullable = TRUE)),
      rules = lapply(
        seq_len(n),
        function(i) DTARuleColRange(id = sprintf("r%d", i), columns = "A", range = c(0, 1))
      )
    )
  }

  qa_step(
    "a short rule set is listed in full",
    "r1, r2, r3", rule_preview(make(3))
  )

  # A long list is abbreviated in the middle rather than truncated at the end,
  # so a reader can still see how the set finishes.
  qa_step(
    "a long rule set keeps its first four and its last",
    "r1, r2, r3, r4, ..., r7", rule_preview(make(7))
  )

  qa_step(
    "a specification with no rules says so rather than showing nothing",
    "not set",
    rule_preview(DTAColumnSpecCollection(
      columns = list(A = DTAColumnSpec(id = "A", type = "SAS Num", nullable = TRUE))
    ))
  )
})

test_that("OQ-RULE-035 | validate_rules reports every violated rule at once | REQ-RULE-032", {
  specs <- DTAColumnSpecCollection(
    columns = list(A = DTAColumnSpec(id = "A", type = "SAS Num", nullable = TRUE)),
    rules = list(
      DTARuleColRange(id = "narrow", columns = "A", range = c(0, 1)),
      DTARuleColUnique(id = "unique_a", columns = "A")
    )
  )
  err <- suppressMessages(
    tryCatch(validate_rules(specs, rt_frame(A = c(5, 5))), error = function(e) e)
  )

  qa_check("a violated rule set raises a condition", inherits(err, "condition"))
  # Both rules are named, so a supplier can fix everything in one pass.
  text <- paste(conditionMessage(err), collapse = " ")
  qa_step(
    "and the condition names every violated rule",
    c(narrow = TRUE, unique_a = TRUE),
    c(narrow = grepl("narrow", text, fixed = TRUE), unique_a = grepl("unique_a", text, fixed = TRUE))
  )

  clean <- suppressMessages(tryCatch(
    {
      validate_rules(specs, rt_frame(A = c(0, 1)))
      "no condition"
    },
    error = function(e) "condition"
  ))
  qa_step("a satisfied rule set raises nothing", "no condition", clean)
})
