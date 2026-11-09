# The cross-object invariant and the linter. REQ-SPEC-027 .. REQ-SPEC-029.
#
# A rule is only meaningful against the columns beside it. These cases check
# that a collection will not hold a rule that contradicts its columns, and that
# lint_dta() reports every such problem in one pass. Every expected value is
# derived from the requirement text and the fixture written in the test, never
# from a first run of the software.

sl_columns <- function() {
  list(
    SUBJ = DTAColumnSpec(id = "SUBJ", type = "SAS Char", length = 8),
    SEX = DTAColumnSpec(id = "SEX", type = "SAS Char", length = 1, values = c("M", "F")),
    AGE = DTAColumnSpec(id = "AGE", type = "SAS Num")
  )
}

# The refusal message of an expression, or NULL when it was accepted.
sl_refusal <- function(expr) {
  tryCatch(
    {
      force(expr)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
}

sl_has_code <- function(msg, code) {
  !is.null(msg) && grepl(code, msg, fixed = TRUE)
}

sl_codes <- c(
  "rule_id_duplicate", "rule_column_undeclared", "rule_range_non_numeric",
  "rule_condition_value_outside_values"
)

# The same four contradictions as raw lists, as a YAML file would carry them.
sl_raw_columns <- function() {
  list(
    list(id = "SUBJ", type = "SAS Char", length = 8),
    list(id = "SEX", type = "SAS Char", length = 1, values = c("M", "F")),
    list(id = "AGE", type = "SAS Num")
  )
}

sl_raw_rules <- function() {
  list(
    list(id = "r1", type = "col_unique", columns = "SUBJ"),
    list(id = "r1", type = "col_unique", columns = "SEX"),
    list(id = "r2", type = "col_unique", columns = "NOPE"),
    list(id = "r3", type = "col_range", columns = "SEX", min = 1, max = 2),
    list(
      id = "r4", type = "col_condition",
      condition = list(SEX = list(equals = "X")), then = list(AGE = list(greater_equal = 0))
    )
  )
}

test_that("OQ-SPEC-049 | a collection whose rules contradict its columns is refused, naming every error | REQ-SPEC-027", {
  clean <- DTAColumnSpecCollection(
    columns = sl_columns(),
    rules = list(
      DTARuleColRange(id = "age", columns = "AGE", range = c(0, 120)),
      DTARuleColCondition(
        id = "sex_m", condition = list(SEX = list(equals = "M")),
        then = list(AGE = list(greater_equal = 0))
      )
    )
  )
  qa_step("rules that fit their columns are accepted", 2L, length(clean@rules))

  dup <- DTARuleColUnique(id = "r1", columns = "SUBJ")
  undeclared <- DTARuleColUnique(id = "r2", columns = "NOPE")
  range_on_text <- DTARuleColRange(id = "r3", columns = "SEX", range = c(1, 2))
  outside <- DTARuleColCondition(
    id = "r4", condition = list(SEX = list(equals = "X")),
    then = list(AGE = list(greater_equal = 0))
  )
  single <- list(
    rule_id_duplicate = list(dup, DTARuleColUnique(id = "r1", columns = "SEX")),
    rule_column_undeclared = list(undeclared),
    rule_range_non_numeric = list(range_on_text),
    rule_condition_value_outside_values = list(outside)
  )
  for (code in names(single)) {
    msg <- sl_refusal(DTAColumnSpecCollection(columns = sl_columns(), rules = single[[code]]))
    qa_check(
      paste0("a collection with only ", code, " is refused, naming that code"),
      sl_has_code(msg, paste0("(", code, ")"))
    )
  }

  all_four <- c(single$rule_id_duplicate, list(undeclared, range_on_text, outside))
  msg <- sl_refusal(DTAColumnSpecCollection(columns = sl_columns(), rules = all_four))
  qa_step(
    "all four contradictions in one collection are named in the one refusal",
    setNames(rep(TRUE, 4), sl_codes),
    vapply(sl_codes, function(code) sl_has_code(msg, paste0("(", code, ")")), logical(1))
  )
})

test_that("OQ-SPEC-050 | assigning columns or rules that contradict the other is refused and changes nothing; warnings are accepted | REQ-SPEC-027", {
  x <- DTAColumnSpecCollection(
    columns = sl_columns(),
    rules = list(DTARuleColRange(id = "age", columns = "AGE", range = c(0, 120)))
  )
  before <- x

  bad_rule <- DTARuleColUnique(id = "dangling", columns = "NOPE")
  msg_rules <- sl_refusal(x@rules <- c(x@rules, list(bad_rule)))
  qa_check(
    "adding a rule on an undeclared column is refused",
    sl_has_code(msg_rules, "(rule_column_undeclared)")
  )
  qa_check("and the collection is as it was", identical(x, before))

  without_age <- x@columns[names(x@columns) != "AGE"]
  msg_cols <- sl_refusal(x@columns <- without_age)
  qa_check(
    "dropping the column a rule uses is refused",
    sl_has_code(msg_cols, "(rule_column_undeclared)")
  )
  qa_check("and the collection is as it was again", identical(x, before))

  # Warning only: a rule, and not one declared column.
  warn_only <- DTAColumnSpecCollection(
    columns = list(),
    rules = list(DTARuleColUnique(id = "r1", columns = "ANY"))
  )
  qa_step("rules with no declared column are accepted", 1L, length(warn_only@rules))
  found <- lint_dta(warn_only)
  qa_step("and reported as rules_without_columns", "rules_without_columns", found$code)
  qa_step("at severity warning", "warning", found$severity)
})

test_that("OQ-SPEC-051 | one pass reports every contradiction with its rule id, object and severity | REQ-SPEC-028", {
  out <- lint_dta(list(columns = sl_raw_columns(), rules = sl_raw_rules()))

  qa_step(
    "the result has the documented columns",
    c("dataset", "object", "id", "severity", "code", "message"), names(out)
  )
  # Sorted by dataset, object, id: r1, r2, r3, r4, one finding each (the
  # duplicated id r1 is one finding, not two).
  qa_step("one finding per contradicted rule, in id order", c("r1", "r2", "r3", "r4"), out$id)
  qa_step(
    "each under its own code",
    c(
      "rule_id_duplicate", "rule_column_undeclared", "rule_range_non_numeric",
      "rule_condition_value_outside_values"
    ),
    out$code
  )
  qa_step("each about a rule", rep("rule", 4), out$object)
  qa_step("each at severity error", rep("error", 4), out$severity)
  qa_check("the messages are text", is.character(out$message) && all(nzchar(out$message)))

  # not_equals against a value outside the list can only be true: pointless,
  # not wrong.
  mild <- lint_dta(list(columns = sl_raw_columns(), rules = list(list(
    id = "r5", type = "col_condition",
    condition = list(SEX = list(not_equals = "X")), then = list(AGE = list(greater_equal = 0))
  ))))
  qa_step("a negated operand outside the values list is not an error", 0L, sum(mild$severity == "error"))
})

test_that("OQ-SPEC-052 | each remaining code is reported alone, at its severity | REQ-SPEC-028", {
  columns <- list(
    list(id = "SEX", type = "SAS Char", values = c("M", "F")),
    list(id = "NAME", type = "SAS Char", length = 5),
    list(id = "AGE", type = "SAS Num")
  )
  one_rule <- function(rule) list(columns = columns, rules = list(rule))

  cases <- list(
    list(
      code = "rule_condition_operator_invalid", severity = "error",
      input = one_rule(list(
        id = "r", type = "col_condition",
        condition = list(AGE = list(bogus = 1)), then = list(NAME = list(empty = FALSE))
      ))
    ),
    # greater compares numbers, NAME is text.
    list(
      code = "rule_condition_numeric_on_text", severity = "error",
      input = one_rule(list(
        id = "r", type = "col_condition",
        condition = list(NAME = list(greater = 3)), then = list(AGE = list(empty = FALSE))
      ))
    ),
    # NAME holds at most five characters; the operand has ten.
    list(
      code = "rule_condition_value_unfit", severity = "error",
      input = one_rule(list(
        id = "r", type = "col_condition",
        condition = list(NAME = list(equals = "waytoolong")), then = list(AGE = list(empty = FALSE))
      ))
    ),
    # The constraint names condition "nope", which conditions does not define.
    list(
      code = "group_constraint_unknown_condition", severity = "error",
      input = one_rule(list(
        id = "g", type = "group_condition", group_by = "SEX",
        conditions = list(a = list(AGE = list(empty = FALSE))),
        constraints = list(list(type = "requires", `if` = "a", then = "nope"))
      ))
    ),
    # "x" is not a whole number under Int.
    list(
      code = "column_spec_invalid", severity = "error",
      input = list(columns = list(list(id = "A", type = "SAS Int", values = "x")))
    ),
    list(
      code = "rule_invalid", severity = "error",
      input = one_rule(list(id = "r", type = "no_such_type"))
    ),
    list(
      code = "column_id_duplicate", severity = "error",
      input = list(columns = list(list(id = "A", type = "SAS Char"), list(id = "A", type = "SAS Char")))
    ),
    # A $3. format shows three characters of a column declared eight long.
    list(
      code = "format_narrower_than_length", severity = "warning",
      input = list(columns = list(list(id = "X", type = "SAS Char", format = "SAS $3.", length = 8)))
    ),
    list(
      code = "rules_without_columns", severity = "warning",
      input = list(rules = list(list(id = "r", type = "col_unique", columns = "X")))
    )
  )
  for (case in cases) {
    out <- lint_dta(case$input)
    qa_step(paste0(case$code, " is the only code reported"), case$code, unique(out$code))
    qa_step(paste0("and at severity ", case$severity), case$severity, unique(out$severity))
  }

  doc <- lint_dta(list(
    metadata = list(title = "T", version = "1"),
    datasets = list(
      list(id = "d", type = "tabular"),
      list(id = "d", type = "tabular"),
      list(type = "tabular")
    )
  ))
  qa_step(
    "a duplicate and a missing dataset id are both reported",
    c("dataset_id_duplicate", "dataset_id_missing"), sort(unique(doc$code))
  )
  qa_step(
    "a document whose metadata lacks a title is metadata_invalid",
    "metadata_invalid",
    lint_dta(list(metadata = list(version = "1"), datasets = list()))$code
  )

  path <- file.path(qa_tempdir(), "unquoted.yaml")
  writeLines(c("metadata:", "  title: T", "  version: 1.10", "datasets: []"), path)
  unquoted <- lint_dta(path)
  qa_check("an unquoted numeric version in a file is version_unquoted", "version_unquoted" %in% unquoted$code)
  qa_step("at severity warning", "warning", unquoted$severity[unquoted$code == "version_unquoted"])
})

test_that("OQ-SPEC-053 | a clean specification lints with zero rows | REQ-SPEC-028", {
  path <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  if (!nzchar(path)) testthat::skip("bundled example not installed: clinical_dta.yaml")

  out <- lint_dta(path)
  qa_step("the bundled clinical specification has no findings", 0L, nrow(out))
  qa_step(
    "yet the result keeps its columns",
    c("dataset", "object", "id", "severity", "code", "message"), names(out)
  )
  qa_step("the object read from it has none either", 0L, nrow(lint_dta(read_dta_from_yaml(path))))
})

test_that("OQ-SPEC-054 | strict mode and the readers abort with every error and class dta_lint_error | REQ-SPEC-029", {
  columns <- sl_raw_columns()
  rules <- sl_raw_rules()

  strict <- tryCatch(lint_dta(list(columns = columns, rules = rules), strict = TRUE), error = function(e) e)
  qa_check("strict mode aborts with class dta_lint_error", inherits(strict, "dta_lint_error"))
  qa_step("carrying the four findings", 4L, if (inherits(strict, "dta_lint_error")) nrow(strict$findings) else NA_integer_)

  warn_only <- tryCatch(
    lint_dta(list(rules = list(list(id = "r", type = "col_unique", columns = "X"))), strict = TRUE),
    error = function(e) e
  )
  qa_check("but a warning alone does not abort", is.data.frame(warn_only))

  dir <- qa_tempdir()
  doc <- list(
    metadata = list(title = "T", version = "1"),
    datasets = list(list(
      id = "d", name = "D", type = "tabular",
      files = list(filename = "d.csv", type = "csv"),
      columns = columns, rules = rules
    ))
  )
  yaml_path <- file.path(dir, "bad_dta.yaml")
  yaml::write_yaml(doc, yaml_path)
  got <- tryCatch(read_dta_from_yaml(yaml_path), error = function(e) e)
  qa_check("read_dta_from_yaml() aborts with class dta_lint_error", inherits(got, "dta_lint_error"))
  qa_step("carrying every error", 4L, if (inherits(got, "dta_lint_error")) nrow(got$findings) else NA_integer_)
  msg <- if (inherits(got, "condition")) conditionMessage(got) else ""
  qa_step(
    "and the message names all four codes",
    setNames(rep(TRUE, 4), sl_codes),
    vapply(sl_codes, function(code) grepl(code, msg, fixed = TRUE), logical(1))
  )

  # import_specs_from_yaml() reads columns as a mapping keyed by id.
  specs_path <- file.path(dir, "bad_specs.yaml")
  yaml::write_yaml(
    list(
      columns = stats::setNames(
        lapply(columns, function(col) col[names(col) != "id"]),
        vapply(columns, function(col) col$id, character(1))
      ),
      rules = rules
    ),
    specs_path
  )
  got_specs <- tryCatch(import_specs_from_yaml(specs_path), error = function(e) e)
  qa_check("import_specs_from_yaml() aborts with class dta_lint_error", inherits(got_specs, "dta_lint_error"))
})

test_that("OQ-SPEC-055 | a numeric operator on a Date, Time or DateTime column is a warning, on a Char column an error, on a numeric column no finding | REQ-SPEC-030", {
  # One condition column X of the type under test and one numeric column Y for
  # the consequence, so the only thing that varies is the type of X.
  numeric_on <- function(type, op = "greater") {
    out <- lint_dta(list(
      columns = list(list(id = "X", type = type), list(id = "Y", type = "SAS Num")),
      rules = list(list(
        id = "r1", type = "col_condition",
        condition = list(X = stats::setNames(list(5), op)),
        then = list(Y = list(greater_equal = 0))
      ))
    ))
    out[out$code == "rule_condition_numeric_on_text", c("id", "severity")]
  }

  for (type in c("SAS Date", "SAS Time", "SAS DateTime")) {
    qa_step(
      paste0("a numeric operator on a ", type, " column is one warning on the rule"),
      "r1:warning",
      paste(numeric_on(type)$id, numeric_on(type)$severity, sep = ":")
    )
  }
  qa_step(
    "another numeric operator on a Date column is a warning too",
    "warning", numeric_on("SAS Date", "less_equal")$severity
  )
  qa_step(
    "on a Char column the same finding is an error",
    "error", numeric_on("SAS Char")$severity
  )
  for (type in c("SAS Num", "SAS Int")) {
    qa_step(paste0("on a ", type, " column there is no such finding"), 0L, nrow(numeric_on(type)))
  }

  # A warning does not refuse the specification: it still constructs.
  built <- tryCatch(
    DTAColumnSpecCollection(
      columns = list(
        DTAColumnSpec(id = "D", type = "SAS Date", format = "SAS DATE9."),
        DTAColumnSpec(id = "Y", type = "SAS Num")
      ),
      rules = list(DTARuleColCondition(
        id = "r1", condition = list(D = list(greater = 5)),
        then = list(Y = list(greater_equal = 0))
      ))
    ),
    error = function(e) e
  )
  qa_check("the specification with the warning still constructs", !inherits(built, "condition"))
})
