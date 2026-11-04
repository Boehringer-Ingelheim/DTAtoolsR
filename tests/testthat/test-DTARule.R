# DTARuleFactory() is the one entry point every YAML/JSON-declared rule goes
# through, and it carries things worth pinning on their own: the retired
# check_* spellings are rejected, an unrecognised type aborts instead of
# silently falling through, and an argument name that is not an exact
# constructor argument aborts instead of being partially matched. Neither
# had a test anywhere in this suite. DTARule itself -- the parent every
# concrete rule class extends -- was likewise never constructed directly.

test_that("DTARuleFactory() dispatches every modern type to its rule class", {
  col_condition <- DTARuleFactory(
    "r1", "col_condition",
    condition = list(age = list(equals = 18)),
    then = list(status = list(equals = "adult"))
  )
  expect_true(inherits(col_condition, "DTAtools::DTARuleColCondition"))
  expect_identical(col_condition@type, "col_condition")

  col_range <- DTARuleFactory("r2", "col_range", columns = "score", min = 0, max = 100)
  expect_true(inherits(col_range, "DTAtools::DTARuleColRange"))
  expect_identical(col_range@type, "col_range")

  col_unique <- DTARuleFactory("r3", "col_unique", columns = "id")
  expect_true(inherits(col_unique, "DTAtools::DTARuleColUnique"))
  expect_identical(col_unique@type, "col_unique")

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
  expect_identical(group_condition@type, "group_condition")
})

test_that("DTARuleFactory() rejects the retired check_* type spellings", {
  for (old in c("check_col_condition", "check_range", "check_unique", "check_group_condition")) {
    expect_error(
      DTARuleFactory("r1", old, columns = "id"),
      regexp = paste0("Unknown rule type: ", old),
      fixed = TRUE
    )
  }
})

test_that("DTARuleFactory() rejects an argument that is not an exact constructor argument", {
  # `column` partially matches `columns`; it must not be smuggled through.
  expect_error(
    DTARuleFactory("r1", "col_range", column = "score", min = 0, max = 100),
    regexp = "Unknown argument"
  )
  expect_error(
    DTARuleFactory("r1", "col_unique", column = "id"),
    regexp = "Unknown argument"
  )
  # An unnamed element is rejected too.
  expect_error(
    DTARuleFactory("r1", "col_unique", "id"),
    regexp = "Unknown argument"
  )
  # An argument that belongs to a different rule type is not accepted either.
  expect_error(
    DTARuleFactory("r1", "col_unique", columns = "id", min = 0),
    regexp = "Unknown argument"
  )
})

test_that("a YAML rule with the singular `column` key aborts on import", {
  path <- tempfile(fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)
  writeLines(
    c(
      "columns:",
      "  - id: AGE",
      "    type: SAS Num",
      "rules:",
      "  - id: bad_rule_key",
      "    type: col_range",
      "    column: AGE",
      "    min: 18",
      "    max: 65"
    ),
    path
  )
  expect_error(import_specs_from_yaml(path), regexp = "Unknown argument")

  # The same rule spelled `columns:` loads.
  writeLines(sub("    column: AGE", "    columns: AGE", readLines(path), fixed = TRUE), path)
  specs <- import_specs_from_yaml(path)
  expect_identical(specs@rules[[1]]@columns, "AGE")
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

test_that("print() of a column-condition rule names the columns and keeps the operators", {
  # The loop walked names(x@condition[[i]]) -- the OPERATORS ("equals") -- as
  # if they were column names, so the column was never printed and the value
  # appeared under a heading that was really an operator.
  rule <- DTARuleColCondition(
    id = "adult_status",
    condition = list(AGE = list(greater = 17), SEX = list(equals = "F")),
    then = list(STATUS = list(equals = "adult"))
  )

  out <- paste(cli::cli_fmt(print(rule)), collapse = "\n")

  expect_match(out, "'AGE' > '17'", fixed = TRUE)
  expect_match(out, "'SEX' = 'F'", fixed = TRUE)
  expect_match(out, "'STATUS' = 'adult'", fixed = TRUE)
})
