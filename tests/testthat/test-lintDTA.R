cols <- function(...) {
  list(
    list(id = "SEX", type = "SAS Char", values = c("M", "F")),
    list(id = "NAME", type = "SAS Char", length = 5),
    list(id = "AGE", type = "SAS Num"),
    list(id = "CODE", type = "SAS Int"),
    ...
  )
}

lint_rules <- function(rules, columns = cols()) {
  lint_dta(list(columns = columns, rules = rules))
}

codes_of <- function(findings, severity = NULL) {
  if (!is.null(severity)) findings <- findings[findings$severity == severity, ]
  sort(findings$code, method = "radix")
}

test_that("a clean bundled DTA gives zero rows with the documented columns", {
  f <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
  out <- lint_dta(f)
  expect_identical(nrow(out), 0L)
  expect_identical(names(out), c("dataset", "object", "id", "severity", "code", "message"))
})

test_that("an object that exists lints clean", {
  expect_identical(nrow(lint_dta(create_example_DTA())), 0L)
  expect_identical(nrow(lint_dta(create_example_DTAColumnSpecCollection(1))), 0L)
})

test_that("rule_id_duplicate and rule_column_undeclared are errors", {
  out <- lint_rules(list(
    list(id = "r1", type = "col_unique", columns = "NOPE"),
    list(id = "r1", type = "col_unique", columns = "SEX")
  ))
  expect_identical(codes_of(out, "error"), c("rule_column_undeclared", "rule_id_duplicate"))
})

test_that("rules_without_columns is a warning and blocks nothing", {
  out <- lint_dta(list(rules = list(list(id = "r1", type = "col_unique", columns = "X"))))
  expect_identical(codes_of(out), "rules_without_columns")
  expect_identical(out$severity, "warning")
  specs <- specs_from_list(NULL, list(list(id = "r1", type = "col_unique", columns = "X")))
  expect_length(specs@rules, 1L)
})

test_that("rule_range_non_numeric flags a Char column, passes Num and Int", {
  bad <- lint_rules(list(list(id = "r", type = "col_range", columns = "NAME", min = 1, max = 2)))
  expect_identical(codes_of(bad), "rule_range_non_numeric")
  expect_identical(bad$severity, "error")
  for (ok in c("AGE", "CODE")) {
    expect_identical(nrow(lint_rules(list(
      list(id = "r", type = "col_range", columns = ok, min = 1, max = 2)
    ))), 0L)
  }
})

test_that("rule_condition_operator_invalid covers unknown operators and malformed clauses", {
  unknown <- lint_rules(list(list(
    id = "r", type = "col_condition",
    condition = list(AGE = list(bogus = 1)), then = list(NAME = list(empty = FALSE))
  )))
  expect_identical(codes_of(unknown), "rule_condition_operator_invalid")
  malformed <- lint_rules(list(list(
    id = "r", type = "col_condition",
    condition = list(AGE = "x"), then = list(NAME = list(empty = FALSE))
  )))
  expect_identical(codes_of(malformed), "rule_condition_operator_invalid")
})

test_that("rule_condition_numeric_on_text flags a text column and a non-numeric bound", {
  on_text <- lint_rules(list(list(
    id = "r", type = "col_condition",
    condition = list(NAME = list(greater = 3)), then = list(AGE = list(empty = FALSE))
  )))
  expect_identical(codes_of(on_text), "rule_condition_numeric_on_text")
  bad_bound <- lint_rules(list(list(
    id = "r", type = "col_condition",
    condition = list(AGE = list(greater = "abc")), then = list(NAME = list(empty = FALSE))
  )))
  expect_identical(codes_of(bad_bound), "rule_condition_numeric_on_text")
  fine <- lint_rules(list(list(
    id = "r", type = "col_condition",
    condition = list(AGE = list(greater = 3)), then = list(NAME = list(empty = FALSE))
  )))
  expect_identical(nrow(fine), 0L)
})

test_that("rule_condition_value_unfit flags values the column can never hold", {
  too_long <- lint_rules(list(list(
    id = "r", type = "col_condition",
    condition = list(NAME = list(equals = "waytoolong")), then = list(AGE = list(empty = FALSE))
  )))
  expect_identical(codes_of(too_long), "rule_condition_value_unfit")
  not_int <- lint_rules(list(list(
    id = "r", type = "col_condition",
    condition = list(CODE = list(`in` = list("a", "b"))), then = list(AGE = list(empty = FALSE))
  )))
  expect_identical(codes_of(not_int), "rule_condition_value_unfit")
  patterned <- lint_rules(list(list(
    id = "r", type = "col_condition",
    condition = list(P = list(equals = "xx")), then = list(AGE = list(empty = FALSE))
  )), columns = c(cols(), list(list(id = "P", type = "SAS Char", pattern = "^[0-9]+$"))))
  expect_identical(codes_of(patterned), "rule_condition_value_unfit")
})

test_that("rule_condition_value_outside_values is an error for equals/in and silent for not_*", {
  mk <- function(op, value) {
    lint_rules(list(list(
      id = "r", type = "col_condition",
      condition = setNames(list(setNames(list(value), op)), "SEX"),
      then = list(AGE = list(empty = FALSE))
    )))
  }
  eq <- mk("equals", "X")
  expect_identical(codes_of(eq), "rule_condition_value_outside_values")
  expect_identical(eq$severity, "error")
  expect_identical(mk("in", list("M", "X"))$severity, "error")
  expect_identical(nrow(mk("not_equals", "X")), 0L)
  expect_identical(nrow(mk("not_in", list("X"))), 0L)
  expect_identical(nrow(mk("equals", "M")), 0L)
})

test_that("group_condition: a constraint on an undefined condition is reported by code", {
  out <- lint_rules(list(list(
    id = "g", type = "group_condition", group_by = "SEX",
    conditions = list(a = list(AGE = list(empty = FALSE))),
    constraints = list(list(type = "requires", `if` = "a", then = "nope"))
  )))
  expect_identical(codes_of(out), "group_constraint_unknown_condition")
  expect_identical(out$severity, "error")
})

test_that("group_condition clauses are checked like column conditions", {
  out <- lint_rules(list(list(
    id = "g", type = "group_condition", group_by = "SEX",
    conditions = list(
      a = list(NAME = list(greater = 1)),
      b = list(AGE = list(empty = FALSE))
    ),
    constraints = list(list(type = "requires", `if` = "a", then = "b"))
  )))
  expect_identical(codes_of(out), "rule_condition_numeric_on_text")
})

test_that("format_narrower_than_length is a warning on Char", {
  out <- lint_dta(list(columns = list(
    list(id = "X", type = "SAS Char", format = "SAS $3.", length = 8)
  )))
  expect_identical(codes_of(out), "format_narrower_than_length")
  expect_identical(out$severity, "warning")
})

test_that("rule_not_checkable is a warning for a rule type that cannot be enumerated", {
  rule <- DTARuleFactory("r", "col_unique", columns = "SEX")
  S7::prop(rule, "type", check = FALSE) <- "future_kind"
  out <- dta_specs_findings(list(DTAColumnSpec(id = "SEX", type = "SAS Char")), list(rule))
  expect_identical(out$code, "rule_not_checkable")
  expect_identical(out$severity, "warning")
})

test_that("column_spec_invalid and rule_invalid are collected, not thrown", {
  out <- lint_dta(list(
    columns = list(
      list(id = "A", type = "SAS Int", values = c("x")),
      list(id = "B", type = "SAS Char")
    ),
    rules = list(
      list(id = "r1", type = "no_such_type"),
      list(id = "r2", type = "col_unique", columns = "A")
    )
  ))
  expect_identical(codes_of(out), c("column_spec_invalid", "rule_invalid"))
  # the broken column A still counts as declared, so r2 is not dangling
  expect_false("rule_column_undeclared" %in% out$code)
})

test_that("duplicate column ids are reported", {
  out <- lint_dta(list(columns = list(
    list(id = "A", type = "SAS Char"), list(id = "A", type = "SAS Char")
  )))
  expect_identical(codes_of(out), "column_id_duplicate")
})

test_that("document level: dataset ids and metadata", {
  doc <- list(
    metadata = list(title = "T", version = "1"),
    datasets = list(
      list(id = "d", type = "tabular"),
      list(id = "d", type = "tabular"),
      list(type = "tabular"),
      list(id = "e")
    )
  )
  out <- lint_dta(doc)
  expect_setequal(out$code, c("dataset_id_duplicate", "dataset_id_missing", "dataset_invalid"))
  no_title <- lint_dta(list(metadata = list(version = "1"), datasets = list()))
  expect_identical(no_title$code, "metadata_invalid")
})

test_that("metadata import issues become warnings", {
  md <- DTAMetaData(
    title = "Q", version = "1",
    transmission = list(date_last_transfer = "2026-12-31 at the earliest")
  )
  expect_true(nrow(metadata_import_errors(md)) > 0)
  out <- lint_dta(DTA(datasets = list(), metadata = md))
  expect_identical(unique(out$code), "metadata_import_issue")
  expect_identical(unique(out$severity), "warning")
})

test_that("version_unquoted is raised for a path, not for a parsed list", {
  path <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c("metadata:", "  title: T", "  version: 1.10", "datasets: []"), path)
  out <- lint_dta(path)
  expect_true("version_unquoted" %in% out$code)
  expect_identical(out$severity[out$code == "version_unquoted"], "warning")
  expect_false("version_unquoted" %in% lint_dta(yaml::read_yaml(path))$code)
})

test_that("an unreadable file is a finding, a missing file an error", {
  path <- withr::local_tempfile(fileext = ".yaml")
  writeLines("a: [unclosed", path)
  expect_identical(lint_dta(path)$code, "yaml_unreadable")
  expect_error(lint_dta(file.path(tempdir(), "nope.yaml")), "does not exist")
})

test_that("output is sorted and strict aborts with dta_lint_error carrying the findings", {
  rules <- list(
    list(id = "z", type = "col_unique", columns = "NOPE"),
    list(id = "a", type = "col_unique", columns = "NOPE2")
  )
  out <- lint_rules(rules)
  expect_identical(out$id, c("a", "z"))
  expect_no_error(lint_rules(rules)) # default never aborts
  err <- tryCatch(lint_dta(list(columns = cols(), rules = rules), strict = TRUE), error = identity)
  expect_s3_class(err, "dta_lint_error")
  expect_identical(nrow(err$findings), 2L)
  # warnings alone never abort, even when strict
  expect_no_error(lint_dta(
    list(rules = list(list(id = "r", type = "col_unique", columns = "X"))),
    strict = TRUE
  ))
})

test_that("the class invariant refuses an inconsistent collection on construction and assignment", {
  col <- DTAColumnSpec(id = "SEX", type = "SAS Char")
  bad_rule <- DTARuleFactory("r", "col_unique", columns = "NOPE")
  err <- tryCatch(
    DTAColumnSpecCollection(columns = list(SEX = col), rules = list(bad_rule)),
    error = identity
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "rule_column_undeclared", fixed = TRUE)

  good <- DTAColumnSpecCollection(columns = list(SEX = col), rules = list(
    DTARuleFactory("ok", "col_unique", columns = "SEX")
  ))
  expect_error(
    {
      good@rules <- list(bad_rule)
    },
    "rule_column_undeclared"
  )
  expect_error(
    {
      good@columns <- list()
      good@rules <- list(bad_rule)
    },
    NA # an empty column list declares nothing, so there is nothing to contradict
  )
})

test_that("a rules-only collection stays valid, so the structure check still works", {
  specs <- specs_from_list(NULL, list(list(id = "r", type = "col_unique", columns = "X")))
  expect_length(specs@columns, 0L)
  expect_no_error(DTAColumnSpecCollection(columns = list(), rules = specs@rules))
})

test_that("the readers abort with every finding at once", {
  path <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "metadata:", "  title: T", "  version: '1'",
    "datasets:",
    "  - id: d1", "    type: tabular",
    "    columns:",
    "      - {id: SEX, type: SAS Char, values: [M, F]}",
    "      - {id: NAME, type: SAS Char}",
    "    rules:",
    "      - {id: r1, type: col_unique, columns: MISSING}",
    "      - {id: r2, type: col_range, columns: NAME, min: 1, max: 2}",
    "      - {id: r2, type: col_unique, columns: NAME}",
    "      - id: r3",
    "        type: col_condition",
    "        condition: {SEX: {equals: X}}",
    "        then: {NAME: {empty: false}}"
  ), path)
  expect_setequal(lint_dta(path)$code, c(
    "rule_column_undeclared", "rule_range_non_numeric", "rule_id_duplicate",
    "rule_condition_value_outside_values"
  ))
  for (reader in list(
    function() read_dta_from_yaml(path),
    function() dta_from_list(yaml::read_yaml(path))
  )) {
    err <- tryCatch(reader(), error = identity)
    expect_s3_class(err, "dta_lint_error")
    expect_identical(nrow(err$findings), 4L)
  }
  specs_file <- withr::local_tempfile(fileext = ".yaml")
  yaml::write_yaml(yaml::read_yaml(path)$datasets[[1]][c("columns", "rules")], specs_file)
  err <- tryCatch(import_specs_from_yaml(specs_file), error = identity)
  expect_s3_class(err, "dta_lint_error")
  expect_identical(nrow(err$findings), 4L)
})

test_that("dta_specs_findings stays cheap without rules", {
  columns <- lapply(seq_len(200), function(i) DTAColumnSpec(id = paste0("C", i), type = "SAS Char"))
  elapsed <- system.time(for (i in 1:20) dta_specs_findings(columns, NULL))[["elapsed"]]
  expect_lt(elapsed, 2)
})

test_that("read_dataset_from_yaml lints first and reports every finding at once", {
  path <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "id: d1", "type: tabular",
    "columns:",
    "  - {id: SEX, type: SAS Char}",
    "rules:",
    "  - {id: r1, type: col_unique, columns: MISSING}",
    "  - {id: r2, type: col_unique, columns: ALSO_MISSING}"
  ), path)
  err <- tryCatch(read_dataset_from_yaml(path), error = identity)
  expect_s3_class(err, "dta_lint_error")
  expect_identical(nrow(err$findings), 2L)
  expect_s3_class(
    read_dataset_from_yaml(system.file("extdata", "gf_dataset.yaml", package = "DTAtools")),
    "DTAtools::DTADataSet"
  )
})

test_that("a list or file without datasets still builds an empty DTA, with warnings only", {
  for (x in list(list(), list(foo = 1))) {
    expect_identical(nrow(lint_dta(x)), 0L)
    expect_length(suppressMessages(dta_from_list(x))@datasets, 0L)
  }
  path <- withr::local_tempfile(fileext = ".yaml")
  writeLines("foo: 1", path)
  expect_identical(nrow(lint_dta(path)), 0L)
  expect_length(suppressMessages(read_dta_from_yaml(path))@datasets, 0L)
  # An empty file never loaded: it stays an unreadable-document error.
  writeLines("", path)
  expect_identical(lint_dta(path)$code, "yaml_unreadable")
  expect_error(read_dta_from_yaml(path), class = "dta_lint_error")
})

test_that("numeric operators are wrong on Char columns only, not on Date/Time/DateTime", {
  temporal <- cols(
    list(id = "D", type = "SAS Date"), list(id = "T", type = "SAS Time"),
    list(id = "DT", type = "SAS DateTime"), list(id = "FREE")
  )
  for (col in c("D", "T", "DT", "FREE")) {
    out <- lint_rules(list(list(
      id = "r", type = "col_condition",
      condition = stats::setNames(list(list(greater = 3)), col),
      then = list(AGE = list(empty = FALSE))
    )), columns = temporal)
    expect_identical(nrow(out), 0L, info = col)
  }
  out <- lint_rules(list(list(
    id = "r", type = "col_condition",
    condition = list(NAME = list(greater = 3)), then = list(AGE = list(empty = FALSE))
  )), columns = temporal)
  expect_identical(codes_of(out), "rule_condition_numeric_on_text")
})

test_that("a rule operand finding names the condition value, not 'values'", {
  out <- lint_rules(list(list(
    id = "r", type = "col_condition",
    condition = list(CODE = list(equals = 2.5)), then = list(AGE = list(empty = FALSE))
  )))
  expect_identical(codes_of(out), "rule_condition_value_unfit")
  expect_match(out$message, "condition value '2.5' is not a valid Int", fixed = TRUE)
  expect_no_match(out$message, "values '2.5'", fixed = TRUE)
})
