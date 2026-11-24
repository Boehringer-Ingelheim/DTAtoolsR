# The condition operator vocabulary. REQ-RULE-001 .. REQ-RULE-010, the
# number of values each operator takes, REQ-RULE-033, and the operators on a
# column of R logical values, REQ-RULE-034 and REQ-RULE-035.
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

test_that("OQ-RULE-050 | an operator given the wrong number of values is rejected when the rule is built | REQ-RULE-033", {
  rejects <- function(operator, value) {
    err <- tryCatch(ro_rule(operator, value), error = function(e) e)
    inherits(err, "dta_condition_arity_error")
  }
  qa_check("equals with two values is rejected", rejects("equals", c("A", "B")))
  qa_check("not_equals with two values is rejected", rejects("not_equals", c("A", "B")))
  qa_check("greater with two values is rejected", rejects("greater", c(1, 2)))
  qa_check("pattern with two patterns is rejected", rejects("pattern", c("^A", "^B")))
  qa_check("min with a missing value is rejected", rejects("min", NA))
  qa_check("range with one value is rejected", rejects("range", 25))
  qa_check("range with three values is rejected", rejects("range", c(0, 100, 5)))
  qa_check("in with no values is rejected", rejects("in", character(0)))
  qa_check("empty with a non-logical value is rejected", rejects("empty", "yes"))

  qa_step("in with two values is accepted and selects both", 2L, ro_selected("in", c("A", "B"), c("A", "B", "C")))
  qa_step("range with two values is accepted", 3L, ro_selected("range", c(2, 4), c(1, 2, 3, 4, 5)))
})

test_that("OQ-RULE-051 | an empty YAML entry in an in or not_in list matches a missing value | REQ-RULE-033", {
  # `~` in a YAML list is an empty member. It stands for "missing": `in: [A, ~]`
  # selects the rows that are A or missing, never the text "NULL".
  members <- yaml::yaml.load("v: [A, ~]")$v
  text <- c("A", NA, "B", "NULL")
  qa_step("in selects the A row and the missing row of a text column", 2L, ro_selected("in", members, text))
  qa_step("not_in selects the B row and the text NULL row", 2L, ro_selected("not_in", members, text))

  numbers <- yaml::yaml.load("v: [1, ~]")$v
  qa_step("in selects the 1 row and the missing row of a number column", 2L, ro_selected("in", numbers, c(1, NA, 2)))
  qa_step("not_in selects only the 2 row of a number column", 1L, ro_selected("not_in", numbers, c(1, NA, 2)))
})

# ---- a column of R logical values -----------------------------------------------------
#
# REQ-RULE-034 and REQ-RULE-035. A logical column holds TRUE, FALSE and missing,
# and a rule operand written as text (equals: T) has to be read as the logical it
# spells. Rows 1 and 4 below hold TRUE, rows 2 and 5 FALSE, rows 3 and 6 are
# missing, so every operand selects a number of rows that is counted by hand.

ro_flags <- c(TRUE, FALSE, NA, TRUE, FALSE, NA)

# How many rows a conditional rule's message says failed its THEN clause.
ro_rows <- function(message) as.integer(sub("^.*: ([0-9]+) rows.*$", "\\1", message))

test_that("OQ-RULE-052 | equals, not_equals, in and not_in read T, true, True and a YAML true as the logical they spell, and a non-spelling matches nothing | REQ-RULE-034", {
  for (spelling in list("T", "TRUE", "true", "True", TRUE)) {
    qa_step(
      sprintf("equals %s selects the two TRUE rows", format(spelling)),
      2L, ro_selected("equals", spelling, ro_flags)
    )
  }
  for (spelling in list("F", "FALSE", "false", "False", FALSE)) {
    qa_step(
      sprintf("equals %s selects the two FALSE rows", format(spelling)),
      2L, ro_selected("equals", spelling, ro_flags)
    )
  }
  qa_step("the alias equal reads T the same way", 2L, ro_selected("equal", "T", ro_flags))

  qa_step("not_equals T selects the FALSE rows, and not the missing ones", 2L, ro_selected("not_equals", "T", ro_flags))
  qa_step("the alias not_equal reads F the same way", 2L, ro_selected("not_equal", "F", ro_flags))
  qa_step("not_equals true (a YAML true) selects the FALSE rows", 2L, ro_selected("not_equals", TRUE, ro_flags))

  qa_step("in [T, F] selects all four rows that hold a value", 4L, ro_selected("in", c("T", "F"), ro_flags))
  qa_step("in [T, False] written as a YAML list selects all four", 4L, ro_selected("in", list("T", "False"), ro_flags))
  qa_step("in [true, F], a YAML logical beside a text", 4L, ro_selected("in", list(TRUE, "F"), ro_flags))
  qa_step("in [T] selects the two TRUE rows", 2L, ro_selected("in", "T", ro_flags))

  # As for every other column, not_in selects the missing values (REQ-RULE-003).
  qa_step("not_in [T] selects the FALSE rows and the missing ones", 4L, ro_selected("not_in", "T", ro_flags))
  qa_step("not_in [T, F] selects the two missing rows only", 2L, ro_selected("not_in", c("T", "F"), ro_flags))

  # An operand that is no spelling of a logical value.
  for (operand in c("yes", "1", " T", "T ", "tRuE", "maybe")) {
    qa_step(sprintf("equals '%s' selects no row and raises nothing", operand), 0L, ro_selected("equals", operand, ro_flags))
    qa_step(sprintf("in ['%s'] selects no row", operand), 0L, ro_selected("in", operand, ro_flags))
    qa_step(
      sprintf("not_equals '%s' selects every row that holds a value", operand),
      4L, ro_selected("not_equals", operand, ro_flags)
    )
  }
  qa_step(
    "a set of a spelling and a non-spelling selects the rows of the spelling",
    2L, ro_selected("in", list("T", "yes"), ro_flags)
  )
})

test_that("OQ-RULE-053 | an ordering operator on a logical column reads TRUE as 1 and FALSE as 0 | REQ-RULE-004 REQ-RULE-034", {
  # LIM-032: the ordering operators are numeric, and a logical value is a
  # number in R, so greater: 0 means "is TRUE". Pinned as it is; the linter
  # does not report it either (OQ-SPEC-062).
  qa_step("greater 0 selects the two TRUE rows", 2L, ro_selected("greater", 0, ro_flags))
  qa_step("less_equal 0 selects the two FALSE rows", 2L, ro_selected("less_equal", 0, ro_flags))
  qa_step("greater_equal 1 selects the two TRUE rows", 2L, ro_selected("greater_equal", 1, ro_flags))
})

test_that("OQ-RULE-054 | the values a column holds decide how an operand is read, not the type its specification declares | REQ-RULE-035", {
  # The same operand against the same spellings, held as text and held as logical.
  qa_step(
    "a text column of T, TRUE, F, true and a missing value: equals T selects the text T only",
    1L, ro_selected("equals", "T", c("T", "TRUE", "F", "true", NA))
  )
  qa_step(
    "the same spellings typed as logical values: equals T selects the three TRUE rows",
    3L, ro_selected("equals", "T", c(TRUE, TRUE, FALSE, TRUE, NA))
  )

  # A rule that selects the TRUE rows and requires their ID to be a.
  rule <- DTARuleColCondition(
    id = "flagged",
    condition = list(FLAG = list(equals = "T")),
    then = list(ID = list(`in` = list("a")))
  )
  specs <- DTAColumnSpecCollection(
    columns = list(
      ID = DTAColumnSpec(id = "ID", type = "R character", nullable = FALSE),
      FLAG = DTAColumnSpec(id = "FLAG", type = "R logical", nullable = TRUE)
    ),
    rules = list(rule)
  )

  # validate_table() types nothing. The column is text there whatever it
  # declares (and its text is a type finding of its own), so only the text T
  # meets the condition, and its row is the one allowed to be a.
  raw <- data.frame(
    ID = c("a", "b", "c", "d", "e"), FLAG = c("T", "TRUE", "F", "true", NA),
    stringsAsFactors = FALSE
  )
  untyped <- validate_table(specs, raw, verbose = FALSE)
  qa_step(
    "validated as the text it is, the rule holds",
    TRUE, isTRUE(untyped$rules_valid)
  )

  # A file is typed first: TRUE, TRUE, FALSE, TRUE, missing. Rows a, b and d meet
  # the condition, b and d are not a, so the rule reports two rows.
  path <- file.path(qa_tempdir(), "flags.csv")
  writeLines(c("ID,FLAG", "a,T", "b,TRUE", "c,F", "d,true", "e,"), path)
  ds <- DTADataSetTabular(id = "flags", specs = specs, files = list(DTAFileCSV(filename = "flags.csv")))
  ds <- check(load_file(ds, file = path, handler_index = 1, stream = "never"), persist = FALSE, quiet = TRUE)
  msgs <- as.data.frame(messages(ds))
  qa_step("read from a file and typed by its declaration, the rule is violated once", 1L, as.integer(validation_status(ds)$n_rule_errors))
  qa_step(
    "and reports the two rows b and d",
    2L,
    ro_rows(msgs$message[msgs$source == "rule"])
  )
})
