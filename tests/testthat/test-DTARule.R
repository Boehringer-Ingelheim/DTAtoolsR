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
  # appeared under a heading that was really an operator. print() now shows
  # the shared one-line wording; the expected sentence is written out from
  # the specification in R/rulePhrasing.R.
  rule <- DTARuleColCondition(
    id = "adult_status",
    condition = list(AGE = list(greater = 17), SEX = list(equals = "F")),
    then = list(STATUS = list(equals = "adult"))
  )

  out <- paste(cli::cli_fmt(print(rule)), collapse = " ")
  # cli wraps long lines; compare with the whitespace collapsed.
  out <- gsub("\\s+", " ", out)

  expect_match(out, "adult_status", fixed = TRUE)
  expect_match(
    out,
    paste(
      "When all of the following are true: AGE is greater than 17; SEX is equal to \u201cF\u201d.",
      "Then STATUS must be equal to \u201cadult\u201d."
    ),
    fixed = TRUE
  )
  # Notes on missing values are left to the full description.
  expect_false(grepl("Missing values", out, fixed = TRUE))
})

# ---- review fixes ----------------------------------------------------------

test_that("rule ids must be a single, non-missing, non-empty string without whitespace", {
  # Only the DTARule parent validator checks the id; every subclass inherits it,
  # on construction and on property assignment alike.
  ctors <- list(
    DTARule = function(id) DTARule(id = id, type = "col_unique", description = NULL),
    DTARuleColUnique = function(id) DTARuleColUnique(id = id, columns = "X"),
    DTARuleColRange = function(id) DTARuleColRange(id = id, columns = "X", min = 0, max = 1),
    DTARuleColCondition = function(id) {
      DTARuleColCondition(id = id, condition = list(A = list(greater = 0)), then = list(B = list(equals = "x")))
    },
    DTARuleGroupCondition = function(id) {
      DTARuleGroupCondition(
        id = id, group_by = "G",
        conditions = list(a = list(A = list(equals = "x"))),
        constraints = list(list(type = "requires", `if` = "a", then = "a"))
      )
    }
  )
  bad_ids <- list("", NA_character_, character(0), c("A", "B"), "A B", "A\tB")
  for (cls in names(ctors)) {
    make <- ctors[[cls]]
    # A valid id constructs, so the failures below are about the id alone.
    rule <- make("ok_id")
    for (bad in bad_ids) {
      info <- paste(cls, deparse(bad))
      expect_error(make(bad), class = "dta_rule_invalid_id", info = info)
      expect_error(
        rule@id <- bad,
        class = "dta_rule_invalid_id",
        info = paste("@<-", info)
      )
    }
    # The message names the problem in the package's own words.
    expect_error(make(""), regexp = "single, non-missing, non-empty string", class = "dta_rule_invalid_id", info = cls)
    expect_error(make("A B"), regexp = "cannot have whitespaces", class = "dta_rule_invalid_id", info = cls)
    expect_error(rule@id <- "A B", regexp = "cannot have whitespaces", class = "dta_rule_invalid_id", info = cls)
    # A rejected assignment leaves the rule unchanged.
    expect_identical(rule@id, "ok_id", info = cls)
  }
})

test_that("a col_range rule needs single, non-missing bounds", {
  expect_error(
    DTARuleColRange(id = "r", columns = "A", min = NA_real_, max = 5),
    class = "dta_rule_invalid_range"
  )
  expect_error(
    DTARuleColRange(id = "r", columns = "A", min = c(1, 2), max = 5),
    class = "dta_rule_invalid_range"
  )
})

test_that("check() on a concrete rule says it is not implemented instead of returning NULL", {
  tab <- arrow::arrow_table(A = 1:3)
  rules <- list(
    DTARuleColRange(id = "r1", columns = "A", min = 0, max = 5),
    DTARuleColUnique(id = "r2", columns = "A"),
    DTARuleColCondition(id = "r3", condition = list(A = list(greater = 0)), then = list(B = list(equals = "x"))),
    DTARuleGroupCondition(
      id = "r4", group_by = "A",
      conditions = list(a = list(A = list(equals = 1))),
      constraints = list(list(type = "requires", `if` = "a", then = "a"))
    )
  )
  for (rule in rules) {
    expect_error(check(rule, tab), class = "dta_rule_check_not_implemented")
  }
})

test_that("a col_range rule naming several columns is rejected at construction", {
  expect_error(
    DTARuleColRange(id = "r", columns = c("A", "B"), min = 0, max = 5),
    class = "dta_rule_range_columns"
  )

  # Assigning a second column to a valid rule is refused the same way.
  rule <- DTARuleColRange(id = "r", columns = "A", min = 0, max = 5)
  expect_error(
    S7::prop(rule, "columns") <- c("A", "B"),
    class = "dta_rule_range_columns"
  )

  # The evaluator keeps its own guard for a rule that bypassed validation.
  S7::prop(rule, "columns", check = FALSE) <- c("A", "B")
  expect_error(
    rule_check_range(rule, data.frame(A = 1, B = 2)),
    class = "dta_rule_range_columns"
  )
})
