# DTARuleFactory() is the one entry point every YAML/JSON-declared rule goes
# through, and it carries two things worth pinning on their own: the legacy
# check_* aliases stay accepted forever (specs written years ago still load),
# and an unrecognised type aborts instead of silently falling through. Neither
# had a test anywhere in this suite. DTARule itself -- the parent every
# concrete rule class extends -- was likewise never constructed directly.

test_that("DTARuleFactory() dispatches every modern type to its rule class", {
  col_condition <- DTARuleFactory(
    "r1", "col_condition",
    condition = list(age = list(equals = 18)),
    then = list(status = list(equals = "adult"))
  )
  expect_true(inherits(col_condition, "DTAtools::DTARuleColCondition"))
  expect_identical(col_condition@type, "check_col_condition")

  col_range <- DTARuleFactory("r2", "col_range", columns = "score", min = 0, max = 100)
  expect_true(inherits(col_range, "DTAtools::DTARuleColRange"))
  expect_identical(col_range@type, "check_range")

  col_unique <- DTARuleFactory("r3", "col_unique", columns = "id")
  expect_true(inherits(col_unique, "DTAtools::DTARuleColUnique"))
  expect_identical(col_unique@type, "check_unique")

  group_condition <- DTARuleFactory(
    "r4", "group_condition",
    group_by = "SUBJID",
    conditions = list(
      c1 = list(AGE = list(equals = 18)),
      c2 = list(AGE = list(equals = 19))
    ),
    constraints = list(list(type = "mutually_exclusive", left = "c1", right = "c2"))
  )
  expect_true(inherits(group_condition, "DTAtools::DTARuleGroupCondition"))
  expect_identical(group_condition@type, "check_group_condition")
})

test_that("DTARuleFactory() accepts the legacy check_* aliases", {
  # Every alias maps to the same modern type before the outer switch runs, so
  # each must land on exactly the class its modern spelling would.
  col_condition <- DTARuleFactory(
    "r1", "check_col_condition",
    condition = list(age = list(equals = 18)),
    then = list(status = list(equals = "adult"))
  )
  expect_true(inherits(col_condition, "DTAtools::DTARuleColCondition"))

  col_range <- DTARuleFactory("r2", "check_range", columns = "score", min = 0, max = 100)
  expect_true(inherits(col_range, "DTAtools::DTARuleColRange"))

  col_unique <- DTARuleFactory("r3", "check_unique", columns = "id")
  expect_true(inherits(col_unique, "DTAtools::DTARuleColUnique"))

  group_condition <- DTARuleFactory(
    "r4", "check_group_condition",
    group_by = "SUBJID",
    conditions = list(
      c1 = list(AGE = list(equals = 18)),
      c2 = list(AGE = list(equals = 19))
    ),
    constraints = list(list(type = "mutually_exclusive", left = "c1", right = "c2"))
  )
  expect_true(inherits(group_condition, "DTAtools::DTARuleGroupCondition"))
})

test_that("DTARuleFactory() aborts on an unrecognised rule type", {
  expect_error(
    DTARuleFactory("r5", "not_a_real_type"),
    regexp = "Unknown rule type: not_a_real_type"
  )
})

test_that("DTARule() stores its declared properties", {
  rule <- DTARule(id = "r1", type = "col_range", description = "desc")
  expect_identical(rule@id, "r1")
  expect_identical(rule@type, "col_range")
  expect_identical(rule@description, "desc")
})

test_that("DTARule() validator rejects whitespace in the id", {
  expect_error(
    DTARule(id = "bad id", type = "col_range", description = NULL),
    regexp = "cannot have whitespaces"
  )
})

test_that("as.list() on a bare DTARule reports only id and type", {
  rule <- DTARule(id = "r1", type = "col_range", description = "desc")
  expect_identical(as.list(rule), list(id = "r1", type = "col_range"))
})

test_that("check() on a bare DTARule refuses to run", {
  # DTARule is the parent every concrete rule extends; check() is only ever
  # meaningful on a subclass, so the base class must name that rather than
  # attempt anything.
  rule <- DTARule(id = "r1", type = "col_range", description = NULL)
  expect_error(
    check(rule),
    regexp = "Check needs to be run from Class derived from DTAtools::DTARule class.",
    fixed = TRUE
  )
})
