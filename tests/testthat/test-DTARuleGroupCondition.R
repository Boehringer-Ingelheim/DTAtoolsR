# Construction-time guards for DTARuleGroupCondition.
#
# This rule type backs hand-authored cross-visit clinical logic, so a typo in
# a DTS should fail loudly at construction rather than surface later as a rule
# that silently never fires. `test-evaluateRules.R` covers the *evaluation*
# semantics (grouping, scopes, alias constraint names) once a rule is already
# built; this file covers the guards that decide whether it gets built at all.
# Only the "references an unknown condition name" guard was covered before
# this file existed (see "group_condition reports unknown condition
# references with context" in test-evaluateRules.R), so it is not repeated
# here.

test_that("group_condition constructor rejects a missing or empty group_by", {
  expect_error(
    DTARuleGroupCondition(
      id = "no_group_by",
      group_by = NULL,
      conditions = list(c1 = list(A = list(equals = 1))),
      constraints = list(list(type = "requires", `if` = "c1", then = "c1"))
    ),
    "No grouping columns were provided"
  )
  expect_error(
    DTARuleGroupCondition(
      id = "empty_group_by",
      group_by = character(0),
      conditions = list(c1 = list(A = list(equals = 1))),
      constraints = list(list(type = "requires", `if` = "c1", then = "c1"))
    ),
    "No grouping columns were provided"
  )
})

test_that("group_condition constructor rejects non-character or empty group_by entries", {
  expect_error(
    DTARuleGroupCondition(
      id = "numeric_group_by",
      group_by = 123,
      conditions = list(c1 = list(A = list(equals = 1))),
      constraints = list(list(type = "requires", `if` = "c1", then = "c1"))
    ),
    "non-empty character column name"
  )
  expect_error(
    DTARuleGroupCondition(
      id = "blank_group_by_entry",
      group_by = c("SUBJ", ""),
      conditions = list(c1 = list(A = list(equals = 1))),
      constraints = list(list(type = "requires", `if` = "c1", then = "c1"))
    ),
    "non-empty character column name"
  )
})

test_that("group_condition constructor rejects missing or empty conditions", {
  expect_error(
    DTARuleGroupCondition(
      id = "null_conditions",
      group_by = "SUBJ",
      conditions = NULL,
      constraints = list(list(type = "requires", `if` = "c1", then = "c1"))
    ),
    "No named conditions were provided"
  )
  expect_error(
    DTARuleGroupCondition(
      id = "empty_conditions",
      group_by = "SUBJ",
      conditions = list(),
      constraints = list(list(type = "requires", `if` = "c1", then = "c1"))
    ),
    "No named conditions were provided"
  )
})

test_that("group_condition constructor rejects conditions without names", {
  # Entirely unnamed: names(conditions) is NULL.
  expect_error(
    DTARuleGroupCondition(
      id = "unnamed_conditions",
      group_by = "SUBJ",
      conditions = list(list(A = list(equals = 1))),
      constraints = list(list(type = "requires", `if` = "c1", then = "c1"))
    ),
    "Every condition needs a non-empty name"
  )
  # Partially named: names(conditions) exists but has a "" entry.
  expect_error(
    DTARuleGroupCondition(
      id = "partially_named_conditions",
      group_by = "SUBJ",
      conditions = list(c1 = list(A = list(equals = 1)), list(B = list(equals = 2))),
      constraints = list(list(type = "requires", `if` = "c1", then = "c1"))
    ),
    "Every condition needs a non-empty name"
  )
})

test_that("group_condition rejects duplicate condition names, at construction and after", {
  # Constructor-level guard: caught before the conditions are ever normalized,
  # so the message is the "Invalid conditions" family, not the validator's.
  expect_error(
    DTARuleGroupCondition(
      id = "dup_conditions",
      group_by = "SUBJ",
      conditions = list(
        dup_x = list(A = list(equals = 1)),
        dup_x = list(B = list(equals = 2))
      ),
      constraints = list(list(type = "requires", `if` = "dup_x", then = "dup_x"))
    ),
    "Condition names must be unique"
  )

  # Validator-level twin: the constructor's own duplicate check can only run
  # ONCE, at construction, so it says nothing about a `conditions` list
  # assigned afterwards via `@<-`. S7 re-runs the class validator on every
  # property assignment, and it carries its own, differently worded, check
  # for exactly this -- this is what makes `rule@conditions <- <bad list>`
  # unsafe rather than merely unchecked.
  rule <- DTARuleGroupCondition(
    id = "dup_conditions_after_construction",
    group_by = "SUBJ",
    conditions = list(c1 = list(A = list(equals = 1)), c2 = list(B = list(equals = 2))),
    constraints = list(list(type = "requires", `if` = "c1", then = "c2"))
  )
  expect_error(
    rule@conditions <- list(
      dup_y = list(A = list(equals = 1)),
      dup_y = list(B = list(equals = 2))
    ),
    "Condition names in 'conditions' must be unique"
  )
})

test_that("group_condition constructor rejects missing or empty constraints", {
  base_conditions <- list(
    c1 = list(A = list(equals = 1)),
    c2 = list(B = list(equals = 2))
  )
  expect_error(
    DTARuleGroupCondition(
      id = "null_constraints",
      group_by = "SUBJ",
      conditions = base_conditions,
      constraints = NULL
    ),
    "No constraint definitions were provided"
  )
  expect_error(
    DTARuleGroupCondition(
      id = "empty_constraints",
      group_by = "SUBJ",
      conditions = base_conditions,
      constraints = list()
    ),
    "No constraint definitions were provided"
  )
})

test_that("group_condition constructor rejects a constraint entry that is not a non-empty list", {
  base_conditions <- list(
    c1 = list(A = list(equals = 1)),
    c2 = list(B = list(equals = 2))
  )
  # Not a list at all.
  expect_error(
    DTARuleGroupCondition(
      id = "constraint_not_a_list",
      group_by = "SUBJ",
      conditions = base_conditions,
      constraints = list("not-a-list")
    ),
    "Each constraint must be a non-empty list"
  )
  # A list, but empty -- same guard, other disjunct.
  expect_error(
    DTARuleGroupCondition(
      id = "constraint_empty_list",
      group_by = "SUBJ",
      conditions = base_conditions,
      constraints = list(list())
    ),
    "Each constraint must be a non-empty list"
  )
})

test_that("group_condition constructor rejects a scope value other than any/all", {
  base_conditions <- list(
    c1 = list(A = list(equals = 1)),
    c2 = list(B = list(equals = 2))
  )
  # left/right both name real conditions, so this is purely the scope check,
  # not the "unknown condition name" guard it sits behind in the source.
  expect_error(
    DTARuleGroupCondition(
      id = "bad_left_scope",
      group_by = "SUBJ",
      conditions = base_conditions,
      constraints = list(list(
        type = "mutually_exclusive", left = "c1", right = "c2",
        left_scope = "sometimes"
      ))
    ),
    "Allowed values are"
  )
})

test_that("group_condition constructor rejects an unsupported constraint type", {
  base_conditions <- list(
    c1 = list(A = list(equals = 1)),
    c2 = list(B = list(equals = 2))
  )
  expect_error(
    DTARuleGroupCondition(
      id = "unsupported_constraint_type",
      group_by = "SUBJ",
      conditions = base_conditions,
      constraints = list(list(type = "xor", left = "c1", right = "c2"))
    ),
    "unsupported type"
  )
})
