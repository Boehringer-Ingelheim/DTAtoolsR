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
  expect_identical(nrow(lint_dta(list())), 0L)
  expect_length(suppressMessages(dta_from_list(list()))@datasets, 0L)
  # An unknown top-level key is the one warning (document_field_unknown, section
  # 4.13); it blocks nothing.
  for (x in list(list(foo = 1))) {
    out <- lint_dta(x)
    expect_identical(out$code, "document_field_unknown")
    expect_identical(out$severity, "warning")
    expect_length(suppressWarnings(suppressMessages(dta_from_list(x)))@datasets, 0L)
  }
  path <- withr::local_tempfile(fileext = ".yaml")
  writeLines("foo: 1", path)
  expect_identical(lint_dta(path)$code, "document_field_unknown")
  expect_length(suppressWarnings(suppressMessages(read_dta_from_yaml(path)))@datasets, 0L)
  # An empty file never loaded: it stays an unreadable-document error.
  writeLines("", path)
  expect_identical(lint_dta(path)$code, "yaml_unreadable")
  expect_error(read_dta_from_yaml(path), class = "dta_lint_error")
})

test_that("numeric operators are wrong on Char and on Date/Time/DateTime, fine on untyped", {
  # as_r_type() maps the three SAS date types to "character", so the engine
  # runs their values through a strict number conversion and every numeric
  # comparison is a false failure; lint must say so up front.
  temporal <- cols(
    list(id = "D", type = "SAS Date"), list(id = "T", type = "SAS Time"),
    list(id = "DT", type = "SAS DateTime"), list(id = "FREE")
  )
  one_rule <- function(col, bound = "2025-01-01") {
    list(list(
      id = "r", type = "col_condition",
      condition = stats::setNames(list(list(greater_equal = bound)), col),
      then = list(AGE = list(empty = FALSE))
    ))
  }
  for (col in c("D", "T", "DT")) {
    out <- lint_rules(one_rule(col), columns = temporal)
    expect_identical(codes_of(out), "rule_condition_numeric_on_text", info = col)
    expect_match(out$message, "read as text", info = col)
    # A warning: the engine handles Date-class data correctly, so the spec must
    # still construct (only character-ISO file reads fail).
    expect_identical(out$severity, "warning", info = col)
    expect_length(
      specs_from_list(temporal, one_rule(col))@rules, 1L
    )
  }
  # Char stays a hard error.
  expect_identical(
    lint_rules(list(list(
      id = "r", type = "col_condition",
      condition = list(NAME = list(greater = 3)), then = list(AGE = list(empty = FALSE))
    )))$severity,
    "error"
  )
  expect_identical(nrow(lint_rules(one_rule("FREE", 3), columns = temporal)), 0L)
  out <- lint_rules(list(list(
    id = "r", type = "col_condition",
    condition = list(NAME = list(greater = 3)), then = list(AGE = list(empty = FALSE))
  )), columns = temporal)
  expect_identical(codes_of(out), "rule_condition_numeric_on_text")
})

test_that("an unquoted YAML Y/N (logical) operand on a text column is an error", {
  for (op in c("equals", "not_equals")) {
    rule <- list(
      id = "r", type = "col_condition",
      condition = stats::setNames(list(stats::setNames(list(TRUE), op)), "NAME"),
      then = list(AGE = list(empty = FALSE))
    )
    out <- lint_rules(list(rule))
    expect_identical(codes_of(out), "rule_condition_value_unfit", info = op)
    expect_match(out$message, "logical value", info = op)
  }
  # ...and the same text, read from YAML, is caught end to end.
  path <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "columns:",
    "  - {id: AESDTH, type: SAS Char}",
    "  - {id: AESER, type: SAS Char}",
    "rules:",
    "  - id: r",
    "    type: col_condition",
    "    condition: {AESDTH: {equals: Y}}",
    "    then: {AESER: {equals: N}}"
  ), path)
  expect_identical(codes_of(lint_dta(path)), c("rule_condition_value_unfit", "rule_condition_value_unfit"))
})

test_that("a logical operand is fine on a Bool column", {
  out <- lint_rules(
    list(list(
      id = "r", type = "col_condition",
      condition = list(FLAG = list(equals = TRUE)), then = list(AGE = list(empty = FALSE))
    )),
    columns = cols(list(id = "FLAG", type = "SAS Bool"))
  )
  expect_false("rule_condition_value_unfit" %in% out$code)
})

test_that("a numeric operand on a text column is reported (YAML dropped the zeros)", {
  out <- lint_rules(list(list(
    id = "r", type = "col_condition",
    condition = list(NAME = list(equals = 1L)), then = list(AGE = list(empty = FALSE))
  )))
  expect_identical(codes_of(out), "rule_condition_value_unfit")
  expect_match(out$message, "quote the value")
  # A warning, not an error: `equals: 1` on text holding "1" is a working rule,
  # so the spec must still construct.
  expect_identical(out$severity, "warning")
  specs <- specs_from_list(
    cols(),
    list(list(
      id = "r", type = "col_condition",
      condition = list(NAME = list(`in` = list(1L, 2L))), then = list(AGE = list(empty = FALSE))
    ))
  )
  expect_length(specs@rules, 1L)
})

test_that("an unknown metadata key is named without printing its value (alias bomb)", {
  # An anchor/alias ladder: expanded, the value of the unknown key is 2^14
  # nested leaves. Printing it into an "unused argument" error never returns.
  lines <- c("a0: &a0 [x, x]")
  for (i in 1:14) lines <- c(lines, sprintf("a%d: &a%d [*a%d, *a%d]", i, i, i - 1L, i - 1L))
  path <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "x-bomb:", paste0("  ", lines),
    "metadata:",
    "  title: T",
    "  version: '1'",
    "  bogus_key: *a14"
  ), path)
  elapsed <- system.time(out <- lint_dta(path))[["elapsed"]]
  expect_lt(elapsed, 20)
  md <- out[out$code == "metadata_invalid", ]
  expect_identical(nrow(md), 1L)
  expect_match(md$message, "bogus_key", fixed = TRUE)
  expect_lt(nchar(md$message), 200L)
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

test_that("a rule operand is checked against the expanded date mask of its column", {
  dtc <- list(id = "DTC", type = "SAS Char", pattern = "^<<yyyyMMdd>>$")
  rule <- function(value) {
    list(list(
      id = "r", type = "col_condition",
      condition = list(DTC = list(equals = value)), then = list(AGE = list(empty = FALSE))
    ))
  }
  expect_false("rule_condition_value_unfit" %in% lint_rules(rule("20240131"), columns = cols(dtc))$code)
  out <- lint_rules(rule("20241331"), columns = cols(dtc))
  expect_identical(codes_of(out), "rule_condition_value_unfit")
  expect_match(out$message, "does not match the pattern", fixed = TRUE)
})

test_that("a malformed date mask in a rule's pattern condition is a rule_invalid finding", {
  out <- lint_rules(list(list(
    id = "r", type = "col_condition",
    condition = list(NAME = list(pattern = "^<<mmm>>$")), then = list(AGE = list(empty = FALSE))
  )))
  expect_identical(codes_of(out), "rule_invalid")
  expect_match(out$message, "mmm", fixed = TRUE)
})

test_that("a malformed date mask in a column pattern is a column_spec_invalid finding", {
  out <- lint_dta(list(columns = cols(list(id = "DTC", type = "SAS Char", pattern = "^<<yyyymmdd>>$"))))
  expect_identical(codes_of(out), "column_spec_invalid")
  expect_match(out$message, "mistyped", fixed = TRUE)
})

# ---- file entries: container and unknown keys ----------------------------------

# The one-dataset document of the container tests, with a chosen `container:`
# line (written exactly as it should appear in the YAML).
container_yaml_lines <- function(container_line) {
  c(
    "metadata:", "  title: T", "  version: '1'",
    "datasets:",
    "  - id: ae", "    type: tabular", "    files:",
    "      type: csv", "      filename: ae.csv",
    paste0("      ", container_line)
  )
}

test_that("file_container_invalid flags a container that is not a .zip or .xpt", {
  out <- lint_dta(list(datasets = list(list(
    id = "ae", type = "tabular",
    files = list(type = "csv", filename = "ae.csv", container = "sdtm.tar")
  ))))
  expect_identical(nrow(out), 1L)
  expect_identical(out$code, "file_container_invalid")
  expect_identical(out$severity, "error")
  expect_identical(out$dataset, "ae")
  expect_identical(out$object, "file")
  expect_identical(out$id, "ae.csv")
  expect_identical(
    out$message,
    "The container 'sdtm.tar' must name a .zip archive or a .xpt SAS transport file."
  )
})

test_that("valid containers lint clean", {
  out <- lint_dta(list(datasets = list(list(
    id = "ae", type = "tabular",
    files = list(
      list(type = "csv", filename = "ae.csv", container = "sdtm_*.zip"),
      list(type = "csv", filename = "dm.csv")
    )
  ))))
  expect_false(any(startsWith(out$code, "file_")))
})

test_that("file datasets are linted as well", {
  out <- lint_dta(list(datasets = list(list(
    id = "define", type = "file",
    files = list(type = "any", filename = "define.xml", container = "x.tar")
  ))))
  expect_identical(out$code, "file_container_invalid")
})

test_that("file_count_bound_invalid flags bounds the file entry would be refused for", {
  lint_files <- function(...) {
    lint_dta(list(datasets = list(list(
      id = "ae", type = "tabular",
      files = list(c(list(type = "csv", filename = "ae_.*[.]csv$", pattern = TRUE), list(...)))
    ))))
  }
  bad <- list(
    list(min_number_of_files = 3, max_number_of_files = 2),
    list(number_of_files = -1),
    list(number_of_files = 2.5),
    list(max_number_of_files = Inf),
    list(number_of_files = "2")
  )
  for (b in bad) {
    out <- do.call(lint_files, b)
    expect_identical(out$code, "file_count_bound_invalid", info = names(b)[[1]])
    expect_identical(out$severity, "error")
    # And the class refuses exactly what the lint flags.
    expect_error(
      do.call(DTAFileCSV, c(list(filename = "ae_.*[.]csv$", pattern = TRUE), b)),
      class = "dta_file_count_bound_invalid"
    )
  }
  expect_match(lint_files(number_of_files = -1)$message, "'number_of_files'", fixed = TRUE)
  # Round-3 review: the two rules the constructor kept to itself.
  both <- lint_files(number_of_files = 2, min_number_of_files = 1)
  expect_identical(both$code, "file_count_bound_invalid")
  lit <- function(...) {
    lint_dta(list(datasets = list(list(
      id = "ae", type = "tabular",
      files = list(c(list(type = "csv", filename = "ae.csv"), list(...)))
    ))))
  }
  for (b in list(list(number_of_files = 2), list(number_of_files = 0), list(min_number_of_files = 0, max_number_of_files = 0))) {
    expect_identical(do.call(lit, b)$code, "file_count_bound_invalid", info = paste(names(b), b, collapse = " "))
    expect_error(do.call(DTAFileCSV, c(list(filename = "ae.csv"), b)), class = "dta_file_count_bound_invalid")
  }
  expect_false(any(lit(number_of_files = 1)$code == "file_count_bound_invalid"))
  h <- DTAFileCSV(filename = "ae.csv")
  expect_error(h@max_number_of_files <- 5, class = "dta_file_count_bound_invalid")
  for (ok in list(list(number_of_files = 0), list(min_number_of_files = 1, max_number_of_files = 3), list())) {
    out <- do.call(lint_files, ok)
    expect_false(any(out$code == "file_count_bound_invalid"))
  }
})

test_that("date_pattern_invalid flags a malformed date mask the file entry would be refused for", {
  lint_mask <- function(filename, pattern = TRUE) {
    lint_dta(list(datasets = list(list(
      id = "ae", type = "tabular",
      files = list(list(type = "csv", filename = filename, pattern = pattern))
    ))))
  }
  for (bad in c("^ae_<<mmm>>\\.csv$", "^ae_<<yyyymmdd>>\\.csv$", "^ae_<<>>\\.csv$")) {
    out <- lint_mask(bad)
    expect_identical(out$code, "date_pattern_invalid", info = bad)
    expect_identical(out$severity, "error")
    expect_identical(out$object, "file")
    # And the class refuses exactly what the lint flags.
    expect_error(DTAFileCSV(filename = bad, pattern = TRUE), class = "dta_date_pattern_invalid")
  }
  expect_false(any(lint_mask("^ae_<<yyyyMMdd>>\\.csv$")$code == "date_pattern_invalid"))
  # A literal file name is no pattern, so `<<` in it is no mask.
  expect_false(any(lint_mask("ae_<<mmm>>.csv", pattern = FALSE)$code == "date_pattern_invalid"))
})

test_that("read_dta_from_yaml stops on an invalid container", {
  p <- withr::local_tempfile(fileext = ".yaml")
  writeLines(container_yaml_lines("container: deliveries/sdtm.zip"), p)
  expect_error(read_dta_from_yaml(p), class = "dta_lint_error")
})

test_that("a misspelt file key is named", {
  out <- lint_dta(list(datasets = list(list(
    id = "ae", type = "tabular",
    files = list(type = "csv", filename = "ae.csv", contianer = "sdtm.zip")
  ))))
  expect_identical(nrow(out), 1L)
  expect_identical(out$code, "file_field_unknown")
  expect_identical(out$message, "Unknown file field(s): 'contianer'.")
})

test_that("a container on the dataset is pointed to its place", {
  out <- lint_dta(list(datasets = list(list(
    id = "ae", type = "tabular", container = "sdtm.zip",
    files = list(type = "csv", filename = "ae.csv")
  ))))
  expect_identical(nrow(out), 1L)
  expect_identical(out$code, "file_container_misplaced")
  expect_identical(
    out$message,
    "'container' belongs to a file entry: write it under 'files:'."
  )
})

test_that("every bundled specification still lints without file findings", {
  specs <- list.files(
    system.file("extdata", package = "DTAtools"),
    pattern = "\\.ya?ml$", full.names = TRUE, recursive = TRUE
  )
  expect_gt(length(specs), 0L)
  for (f in specs) {
    out <- lint_dta(f)
    expect_false(any(startsWith(out$code, "file_")), info = f)
  }
})

test_that("an unquoted value starting with * is explained", {
  p <- withr::local_tempfile(fileext = ".yaml")
  writeLines(container_yaml_lines("container: *_SDTM.zip"), p)
  out <- lint_dta(p)
  hit <- out[out$code == "yaml_unreadable", ]
  expect_identical(nrow(hit), 1L)
  expect_match(hit$message, "must be quoted", fixed = TRUE)
})

test_that("a container value with braces is shown as written", {
  p <- withr::local_tempfile(fileext = ".yaml")
  writeLines(container_yaml_lines('container: "{1+1}.tar"'), p)
  e <- expect_error(read_dta_from_yaml(p), class = "dta_lint_error")
  expect_match(conditionMessage(e), "{1+1}", fixed = TRUE)
})

# ---- containers (D16, section 4.13) -----------------------------------------------

# A document of one tabular dataset per entry of `files` (a list of `files`
# values), with the given containers section.
lint_doc <- function(containers = NULL, files = list(list(type = "csv", filename = "ae.csv")),
                     extra = list()) {
  datasets <- lapply(seq_along(files), function(i) {
    list(id = paste0("ds", i), type = "tabular", files = files[[i]])
  })
  doc <- c(list(metadata = list(title = "T", version = "1"), datasets = datasets), extra)
  if (!is.null(containers)) doc$containers <- containers
  lint_dta(doc)
}
row_of <- function(out, code) out[out$code == code, , drop = FALSE]

test_that("container_invalid is an error on a container row", {
  bad_id <- row_of(lint_doc(list(list(id = "1a", filename = "x.zip"))), "container_invalid")
  expect_identical(nrow(bad_id), 1L)
  expect_identical(bad_id$severity, "error")
  expect_identical(bad_id$object, "container")
  expect_identical(bad_id$id, "1a")
  expect_identical(bad_id$dataset, "")

  for (containers in list(
    list(id = "a", filename = "a.zip"), # a mapping, not a list of mappings
    list(list(id = "a")), # no filename
    list(list(id = "a", filename = "a.zip", colour = "red")), # unknown field
    list(list(id = "a", filename = "a.tar")), # not a .zip/.xpt name
    list("a.zip") # not a mapping
  )) {
    out <- row_of(lint_doc(containers), "container_invalid")
    expect_identical(nrow(out), 1L)
    expect_identical(out$severity, "error")
    expect_identical(out$object, "container")
  }
})

test_that("container_duplicate covers ids, file names and several defaults", {
  dup_id <- lint_doc(list(
    list(id = "a", filename = "a.zip"), list(id = "a", filename = "b.zip")
  ))
  expect_identical(row_of(dup_id, "container_duplicate")$id, "a")
  dup_file <- lint_doc(list(
    list(id = "a", filename = "a.zip"), list(id = "b", filename = "a.zip")
  ))
  expect_identical(nrow(row_of(dup_file, "container_duplicate")), 1L)
  two_defaults <- lint_doc(list(
    list(id = "a", filename = "a.zip", default = TRUE),
    list(id = "b", filename = "b.zip", default = TRUE)
  ))
  hit <- row_of(two_defaults, "container_duplicate")
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$severity, "error")
  expect_identical(hit$object, "container")
})

test_that("container_unused is a warning for an explicit, non-default, unreferenced container", {
  files <- list(list(type = "csv", filename = "ae.csv", container = "used"))
  out <- lint_doc(
    list(
      list(id = "used", filename = "u.zip"),
      list(id = "idle", filename = "i.zip"),
      list(id = "dflt", filename = "d.zip", default = TRUE)
    ),
    files = files
  )
  hit <- row_of(out, "container_unused")
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$id, "idle")
  expect_identical(hit$severity, "warning")
  expect_identical(hit$object, "container")
  # a reference by file name counts as a use
  by_file <- lint_doc(
    list(list(id = "idle", filename = "i.zip")),
    files = list(list(type = "csv", filename = "ae.csv", container = "i.zip"))
  )
  expect_identical(nrow(row_of(by_file, "container_unused")), 0L)
})

test_that("file_container_unknown names an undotted reference nobody defines", {
  files <- list(list(type = "csv", filename = "ae.csv", container = "sdtm"))
  none_defined <- row_of(lint_doc(files = files), "file_container_unknown")
  expect_identical(nrow(none_defined), 1L)
  expect_identical(none_defined$severity, "error")
  expect_identical(none_defined$object, "file")
  expect_identical(none_defined$dataset, "ds1")
  expect_identical(none_defined$id, "ae.csv")
  defined <- lint_doc(list(list(id = "sdtm", filename = "s.zip")), files = files)
  expect_identical(nrow(row_of(defined, "file_container_unknown")), 0L)
  # "none" and file names are never unknown
  for (ref in c("none", "x.zip")) {
    out <- lint_doc(files = list(list(type = "csv", filename = "ae.csv", container = ref)))
    expect_identical(nrow(row_of(out, "file_container_unknown")), 0L)
  }
})

test_that("a dataset linted on its own skips file_container_unknown", {
  out <- lint_dta(list(
    id = "ae", type = "tabular",
    files = list(type = "csv", filename = "ae.csv", container = "sdtm")
  ))
  expect_identical(nrow(row_of(out, "file_container_unknown")), 0L)
})

test_that("an undotted value that is no id is file_container_invalid", {
  out <- lint_doc(files = list(list(type = "csv", filename = "ae.csv", container = "bad id!")))
  hit <- row_of(out, "file_container_invalid")
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$severity, "error")
  expect_match(hit$message, "container id", fixed = TRUE)
})

test_that("document_field_unknown is a warning per unexpected top-level key", {
  out <- lint_doc(extra = list(colour = "red"))
  hit <- row_of(out, "document_field_unknown")
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$severity, "warning")
  expect_identical(hit$object, "document")
  expect_identical(hit$id, "colour")
  expect_identical(nrow(row_of(lint_doc(), "document_field_unknown")), 0L)
})

test_that("container_reference_by_filename fires on YAML input", {
  p <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "metadata:", "  title: T", "  version: '1'",
    "containers:", "  - id: sdtm", "    filename: sdtm.zip",
    "datasets:",
    "  - id: ae", "    type: tabular", "    files:",
    "      type: csv", "      filename: ae.csv", "      container: sdtm.zip"
  ), p)
  hit <- row_of(lint_dta(p), "container_reference_by_filename")
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$severity, "warning")
  expect_identical(hit$object, "file")
  expect_identical(hit$dataset, "ae")
  expect_identical(hit$id, "ae.csv")
  expect_match(hit$message, "'sdtm'", fixed = TRUE)
})

test_that("container_reference_by_filename fires on the object form after @<-", {
  dta <- cont_dta(container = "sdtm", containers = list(DTAtools::DTAContainer("sdtm", "sdtm.zip")))
  expect_identical(nrow(row_of(lint_dta(dta), "container_reference_by_filename")), 0L)
  dta@datasets[[1]]@files[[1]]@container <- "sdtm.zip"
  hit <- row_of(lint_dta(dta), "container_reference_by_filename")
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$severity, "warning")
  expect_identical(hit$dataset, "ae")
})

test_that("container_unused fires on the object form", {
  dta <- cont_dta(containers = list(DTAtools::DTAContainer("idle", "idle.zip")))
  hit <- row_of(lint_dta(dta), "container_unused")
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$id, "idle")
})

test_that("a 0.27.0 document without overlaps or extra keys lints as before", {
  out <- lint_doc(files = list(
    list(type = "csv", filename = "ae.csv", container = "sdtm_*.zip"),
    list(type = "csv", filename = "dm.csv")
  ))
  expect_identical(nrow(out), 0L)
})

conflict_doc <- function(container_dm = NULL) {
  dm <- list(type = "csv", filename = "^.*e[.]csv$", pattern = TRUE)
  if (!is.null(container_dm)) dm$container <- container_dm
  list(
    metadata = list(title = "T", version = "1"),
    datasets = list(
      list(id = "ae", type = "tabular", files = list(type = "csv", filename = "^a.*[.]csv$", pattern = TRUE)),
      list(id = "dm", type = "tabular", files = dm)
    )
  )
}

test_that("file_name_conflict is a warning with the section 4.14 sentence", {
  out <- lint_dta(conflict_doc())
  expect_identical(nrow(out), 1L)
  expect_identical(out$code, "file_name_conflict")
  expect_identical(out$severity, "warning")
  expect_identical(out$dataset, "ae")
  expect_identical(out$object, "file")
  expect_identical(out$id, "ae[1] / dm[1]")
  expect_identical(
    out$message,
    paste0(
      "Datasets 'ae' (^a.*[.]csv$) and 'dm' (^.*e[.]csv$) could both take a file named 'ae.csv'. ",
      "Make their file names distinct so every file belongs to one dataset."
    )
  )
  # the document still reads; the conflict is a warning
  p <- withr::local_tempfile(fileext = ".yaml")
  yaml::write_yaml(conflict_doc(), p)
  dta <- NULL
  expect_warning(dta <- read_dta_from_yaml(p), class = "dta_lint_warning")
  expect_s3_class(dta, "DTAtools::DTA")
  # and the object form reports the same pair
  obj <- lint_dta(dta)
  expect_identical(obj$code, "file_name_conflict")
  expect_identical(obj$message, out$message)
})

test_that("conflicts are not looked for while the document has lint errors", {
  out <- lint_dta(conflict_doc(container_dm = "sdtm"))
  expect_identical(out$code, "file_container_unknown")
})

test_that("a malformed container flags only itself, not the files that name it", {
  files <- list(list(type = "csv", filename = "ae.csv", container = "sdtm"))
  out <- lint_doc(list(list(id = "sdtm")), files = files) # no filename
  expect_identical(out$code, "container_invalid")
  expect_identical(out$id, "sdtm")
  expect_identical(out$dataset, "")
})

test_that("the container_reference_by_filename message says it works but is rename-unsafe", {
  p <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "metadata:", "  title: T", "  version: '1'",
    "containers:", "  - id: sdtm", "    filename: sdtm.zip",
    "datasets:",
    "  - id: ae", "    type: tabular", "    files:",
    "      type: csv", "      filename: ae.csv", "      container: sdtm.zip"
  ), p)
  expect_identical(
    row_of(lint_dta(p), "container_reference_by_filename")$message,
    "Entry refers to container 'sdtm' by its file name 'sdtm.zip'; it works, but refer to it as 'sdtm' so renaming the file keeps it."
  )
})

test_that("document-level container rows carry an empty dataset, never NA", {
  out <- lint_doc(list(list(id = "a", filename = "a.zip"), list(id = "b", filename = "b.zip")))
  unused <- row_of(out, "container_unused")
  expect_identical(nrow(unused), 2L)
  expect_identical(unused$dataset, c("", ""))
  expect_false(anyNA(out$dataset))
})

# ---- column ids that spell a field of another column -------------------------

shadow_cols <- function(...) {
  ids <- c(...)
  lapply(ids, function(id) list(id = id, type = "SAS Char"))
}

test_that("column_id_shadows_field warns when a column X.<field> sits beside a column X", {
  cases <- list(
    c("Specimen", "Specimen.type"),
    c("a", "a.values"),
    c("V", "V.values_from"),
    c("Dose", "Dose.length"),
    c("Subject ID", "Subject ID.label")
  )
  for (ids in cases) {
    out <- lint_dta(list(columns = shadow_cols(ids)))
    info <- paste(ids, collapse = " + ")
    expect_identical(codes_of(out), "column_id_shadows_field", info = info)
    expect_identical(out$severity, "warning", info = info)
    expect_identical(out$object, "column", info = info)
    # reported on the longer id, the one that is read as a field
    expect_identical(out$id, ids[[2]], info = info)
  }
})

test_that("column_id_shadows_field is silent for dotted ids that spell no column field", {
  expect_identical(nrow(lint_dta(list(columns = shadow_cols("a", "a.b")))), 0L)
  expect_identical(nrow(lint_dta(list(columns = shadow_cols("Wt. (kg)", "No. of Doses")))), 0L)
  # the longer id alone, with no shorter column to be a field of
  expect_identical(nrow(lint_dta(list(columns = shadow_cols("Specimen.type")))), 0L)
  # a sibling that is not a prefix
  expect_identical(nrow(lint_dta(list(columns = shadow_cols("Specimen", "Other.type")))), 0L)
})

test_that("column_id_shadows_field is a warning only: the document still reads", {
  p <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "metadata:", "  title: T", "  version: '1'",
    "datasets:",
    "  - id: d",
    "    type: tabular",
    "    files: { filename: \"d.csv\", type: csv }",
    "    columns:",
    "      - { id: Specimen, type: SAS Char }",
    "      - { id: Specimen.type, type: SAS Char }"
  ), p)

  out <- lint_dta(p)
  expect_identical(out$code, "column_id_shadows_field")
  expect_identical(out$dataset, "d")
  expect_identical(out$severity, "warning")

  # a warning is passed on and the document is still built, both columns kept
  dta <- NULL
  expect_warning(dta <- suppressMessages(read_dta_from_yaml(p)), class = "dta_lint_warning")
  expect_identical(names(dta@datasets[["d"]]@specs@columns), c("Specimen", "Specimen.type"))
})

test_that("column ids with spaces lint clean", {
  out <- lint_dta(list(columns = shadow_cols(
    "Subject ID", "Subject  ID", "Visit Date (UTC)", "Dose mg/kg"
  )))
  expect_identical(nrow(out), 0L)
})

test_that("a column id with a leading space or a tab is a column_spec_invalid error", {
  for (bad in c(" Subject ID", "Subject ID ", "Subject\tID")) {
    out <- lint_dta(list(columns = shadow_cols(bad)))
    expect_identical(codes_of(out), "column_spec_invalid", info = bad)
    expect_identical(out$severity, "error", info = bad)
  }
})

test_that("a rule id with spaces is still rewritten to underscores, not kept as typed", {
  rule <- DTARuleFactory("my rule", "col_unique", columns = "SEX")
  expect_identical(rule@id, "my_rule")
  expect_identical(nrow(lint_rules(list(list(id = "my rule", type = "col_unique", columns = "SEX")))), 0L)
})

# ---- the R vocabulary --------------------------------------------------------------
# A column is judged by the family of its type (text, number, integer, logical,
# temporal), never by a SAS type name. Every check here is a construction gate: a
# lint error stops a collection from being built.

r_cols <- function(...) {
  list(
    list(id = "TXT", type = "R character", length = 5),
    list(id = "NUM", type = "R numeric"),
    list(id = "INT", type = "R integer"),
    list(id = "LGL", type = "R logical"),
    list(id = "LGV", type = "R logical", values = c("TRUE", "FALSE")),
    list(id = "LGO", type = "R logical", values = "TRUE"),
    list(id = "DAY", type = "R Date"),
    list(id = "STAMP", type = "R POSIXct"),
    list(id = "TOD", type = "R hms"),
    ...
  )
}

# One col_condition rule: `col` `op` `value`, with a harmless `then` on the same
# column.
r_cond <- function(col, op, value, id = paste(col, op)) {
  list(
    id = id, type = "col_condition",
    condition = stats::setNames(list(stats::setNames(list(value), op)), col),
    then = stats::setNames(list(list(empty = FALSE)), col)
  )
}

# The findings of one rule on r_cols(), as "code/severity" in sorted order.
r_lint <- function(rule, columns = r_cols()) {
  out <- lint_dta(list(columns = columns, rules = list(rule)))
  sort(sprintf("%s/%s", out$code, out$severity), method = "radix")
}

r_lint_df <- function(rule, columns = r_cols()) lint_dta(list(columns = columns, rules = list(rule)))

test_that("rule_range_non_numeric flags R character, logical and temporal columns, passes R numeric and integer", {
  range <- function(col) list(id = "r", type = "col_range", columns = col, min = 1, max = 2)
  for (bad in c("TXT", "LGL", "DAY", "STAMP", "TOD")) {
    expect_identical(r_lint(range(bad)), "rule_range_non_numeric/error", info = bad)
  }
  for (ok in c("NUM", "INT")) {
    expect_identical(r_lint(range(ok)), character(0), info = ok)
  }
})

test_that("a numeric operator on R character is an error, on an R temporal a warning, on R numbers none", {
  expect_identical(
    r_lint(r_cond("TXT", "greater", 3)), "rule_condition_numeric_on_text/error"
  )
  for (col in c("DAY", "STAMP", "TOD")) {
    expect_identical(
      r_lint(r_cond(col, "less", 3)), "rule_condition_numeric_on_text/warning",
      info = col
    )
  }
  for (col in c("NUM", "INT")) {
    expect_identical(r_lint(r_cond(col, "greater", 3)), character(0), info = col)
    expect_identical(
      r_lint(r_cond(col, "greater", "abc")), "rule_condition_numeric_on_text/error",
      info = col
    )
  }
  # a numeric operator on a logical column reads TRUE as 1, as the engine does
  expect_identical(r_lint(r_cond("LGL", "greater", 0)), character(0))
})

test_that("an R column is judged like its SAS counterpart in every operator check", {
  rules <- function(col) {
    list(
      list(id = "range", type = "col_range", columns = col, min = 1, max = 2),
      r_cond(col, "greater", 3, id = "gt3"),
      r_cond(col, "greater", "abc", id = "gtabc"),
      r_cond(col, "equals", TRUE, id = "eqtrue"),
      r_cond(col, "not_in", list(FALSE, TRUE), id = "notin")
    )
  }
  # The plain value checks differ by design for the temporal types: an R
  # date is strict about its format, a SAS date with no format is not.
  value_rules <- function(col) {
    list(
      r_cond(col, "equals", 7, id = "eq7"),
      r_cond(col, "equals", "waytoolong", id = "eqlong"),
      r_cond(col, "in", list("a", "b"), id = "ina")
    )
  }
  findings <- function(type, rules) {
    # a length belongs to the text types only; the R ones refuse it elsewhere
    col <- c(list(id = "C", type = type), if (grepl("Char$|character$", type)) list(length = 5))
    out <- lint_dta(list(columns = list(col), rules = rules))
    out[order(out$id, out$code, method = "radix"), c("id", "code", "severity")]
  }
  pairs <- list(
    c("SAS Char", "R character"), c("SAS Num", "R numeric"), c("SAS Int", "R integer"),
    c("SAS Date", "R Date"), c("SAS Time", "R hms"), c("SAS DateTime", "R POSIXct")
  )
  for (p in pairs) {
    sas <- findings(p[[1]], rules("C"))
    r <- findings(p[[2]], rules("C"))
    expect_identical(r, sas, info = paste(p, collapse = " / "))
  }
  for (p in pairs[1:3]) {
    sas <- findings(p[[1]], value_rules("C"))
    r <- findings(p[[2]], value_rules("C"))
    expect_identical(r, sas, info = paste(p, collapse = " / "))
  }
})

test_that("the operand checks of R numeric, R integer and R character are those of SAS Num, Int and Char", {
  expect_identical(r_lint(r_cond("INT", "equals", "abc")), "rule_condition_value_unfit/error")
  expect_identical(r_lint(r_cond("INT", "equals", 1.5)), "rule_condition_value_unfit/error")
  expect_identical(r_lint(r_cond("INT", "equals", 3)), character(0))
  expect_identical(r_lint(r_cond("NUM", "in", list("1.5", "abc"))), "rule_condition_value_unfit/error")
  expect_identical(r_lint(r_cond("NUM", "equals", 1.5)), character(0))
  expect_identical(r_lint(r_cond("TXT", "equals", "waytoolong")), "rule_condition_value_unfit/error")
  expect_identical(r_lint(r_cond("TXT", "equals", "ok")), character(0))
  # YAML has dropped the zeros of a number written against text
  expect_identical(r_lint(r_cond("TXT", "equals", 7)), "rule_condition_value_unfit/warning")
})

test_that("an R temporal operand must fit the column's format", {
  expect_identical(r_lint(r_cond("DAY", "equals", "2024-13-01")), "rule_condition_value_unfit/error")
  expect_identical(r_lint(r_cond("DAY", "equals", "2024-02-29")), character(0))
  expect_identical(r_lint(r_cond("DAY", "in", list("2024-02-29", "tomorrow"))), "rule_condition_value_unfit/error")
  expect_identical(r_lint(r_cond("TOD", "equals", "25:00:00")), "rule_condition_value_unfit/error")
  expect_identical(r_lint(r_cond("TOD", "equals", "23:59:59")), character(0))
  expect_identical(r_lint(r_cond("STAMP", "equals", "2024-01-31 12:00:00")), character(0))
  # a format written on the column is the one the operand must fit
  fmt <- list(list(id = "D", type = "R Date", format = "R %d.%m.%Y"))
  expect_identical(r_lint(r_cond("D", "equals", "31.01.2024"), fmt), character(0))
  expect_identical(r_lint(r_cond("D", "equals", "2024-01-31"), fmt), "rule_condition_value_unfit/error")
})

test_that("a YAML logical against a column that is not logical is an error naming the R type", {
  for (col in c("TXT", "NUM", "INT", "DAY", "TOD")) {
    out <- r_lint_df(r_cond(col, "equals", TRUE))
    expect_identical(out$code, "rule_condition_value_unfit", info = col)
    expect_identical(out$severity, "error", info = col)
    expect_match(out$message, paste0("column is R ", c(
      TXT = "character", NUM = "numeric", INT = "integer", DAY = "Date", TOD = "hms"
    )[[col]]), fixed = TRUE)
  }
  # and the negated operators too
  expect_identical(r_lint(r_cond("TXT", "not_equals", FALSE)), "rule_condition_value_unfit/error")
})

test_that("a SAS finding still names the bare type, an R finding the type with its backend", {
  sas <- lint_dta(list(
    columns = list(list(id = "A", type = "SAS Char")),
    rules = list(r_cond("A", "equals", TRUE, id = "r"))
  ))
  expect_match(sas$message, "the column is Char.", fixed = TRUE)
  r <- lint_dta(list(
    columns = list(list(id = "A", type = "R character")),
    rules = list(r_cond("A", "equals", TRUE, id = "r"))
  ))
  expect_match(r$message, "the column is R character.", fixed = TRUE)
})

# ---- operands of a logical column --------------------------------------------------

test_that("a logical column takes true, false and the eight logical tokens as operands", {
  for (v in list(TRUE, FALSE, "TRUE", "T", "true", "True", "FALSE", "F", "false", "False")) {
    expect_identical(r_lint(r_cond("LGL", "equals", v)), character(0), info = format(v))
    expect_identical(r_lint(r_cond("LGL", "not_equals", v)), character(0), info = format(v))
  }
  expect_identical(r_lint(r_cond("LGL", "in", list("T", "False", TRUE))), character(0))
  expect_identical(r_lint(r_cond("LGL", "not_in", list("F"))), character(0))
})

test_that("a logical column refuses any other operand with the operand-type code", {
  for (v in list("maybe", "yes", "1", "t", "TRUE ", "NA", " T", 1, 0, 2.5)) {
    out <- r_lint_df(r_cond("LGL", "equals", v))
    expect_identical(out$code, "rule_condition_value_unfit", info = format(v))
    expect_identical(out$severity, "error", info = format(v))
  }
  # a list with one bad member is refused, and so is a negated operator
  expect_identical(r_lint(r_cond("LGL", "in", list("T", "maybe"))), "rule_condition_value_unfit/error")
  expect_identical(r_lint(r_cond("LGL", "not_equals", "maybe")), "rule_condition_value_unfit/error")
  expect_identical(r_lint(r_cond("LGL", "not_in", list("maybe"))), "rule_condition_value_unfit/error")
  # no new code: the one every operand-type finding has
  expect_identical(
    unique(r_lint_df(r_cond("LGL", "equals", "maybe"))$code), "rule_condition_value_unfit"
  )
})

test_that("the refusal of a logical operand names the operand and the tokens", {
  out <- r_lint_df(r_cond("LGL", "in", list("T", "maybe", "nope")))
  expect_match(out$message, "'maybe', 'nope'", fixed = TRUE)
  expect_false(grepl("'T'", out$message, fixed = TRUE))
  expect_match(out$message, "TRUE, T, true, True, FALSE, F, false, False", fixed = TRUE)
})

test_that("an operand that is a token of a logical is judged against `values` by what it means", {
  # values: ["TRUE", "FALSE"]: T and false are both inside it
  expect_identical(r_lint(r_cond("LGV", "equals", "T")), character(0))
  expect_identical(r_lint(r_cond("LGV", "in", list("false", TRUE))), character(0))
  # values: ["TRUE"]: F and FALSE mean the same value, which is not listed
  expect_identical(r_lint(r_cond("LGO", "equals", "T")), character(0))
  expect_identical(r_lint(r_cond("LGO", "equals", "F")), "rule_condition_value_outside_values/error")
  expect_identical(r_lint(r_cond("LGO", "equals", FALSE)), "rule_condition_value_outside_values/error")
  # a bad token is the operand-type error, not an outside-values one
  expect_identical(r_lint(r_cond("LGO", "equals", "maybe")), "rule_condition_value_unfit/error")
  # a negated comparison outside the list is harmless, as for any column
  expect_identical(r_lint(r_cond("LGO", "not_equals", "F")), character(0))
})

test_that("SAS columns do not take the logical tokens as operands", {
  # `T` on a SAS Char column is plain text and needs no reading as a logical
  cols <- list(list(id = "A", type = "SAS Char"), list(id = "B", type = "SAS Num"))
  expect_identical(r_lint(r_cond("A", "equals", "T"), cols), character(0))
  expect_identical(r_lint(r_cond("B", "equals", "T"), cols), "rule_condition_value_unfit/error")
  expect_identical(r_lint(r_cond("A", "equals", TRUE), cols), "rule_condition_value_unfit/error")
})

test_that("the rules of a logical column are a construction gate", {
  columns <- list(list(id = "FLAG", type = "R logical"), list(id = "ID", type = "R character"))
  good <- list(list(
    id = "ok", type = "col_condition",
    condition = list(FLAG = list(equals = "T")), then = list(ID = list(empty = FALSE))
  ))
  bad <- list(list(
    id = "no", type = "col_condition",
    condition = list(FLAG = list(equals = "maybe")), then = list(ID = list(empty = FALSE))
  ))
  expect_no_error(specs_from_list(columns, good))
  err <- tryCatch(specs_from_list(columns, bad), error = identity)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "rule_condition_value_unfit", fixed = TRUE)
  # the same operand on a text column is no finding at all
  text <- list(list(id = "FLAG", type = "R character"), list(id = "ID", type = "R character"))
  expect_no_error(specs_from_list(text, bad))
})

# ---- the `$w.` check is a SAS format check -----------------------------------------

test_that("format_narrower_than_length never fires for an R column", {
  out <- lint_dta(list(columns = list(
    list(id = "A", type = "R character", length = 8),
    list(id = "B", type = "R Date", format = "R %Y-%m-%d"),
    list(id = "C", type = "R POSIXct", format = "R %Y-%m-%dT%H:%M:%SZ"),
    list(id = "D", type = "R hms")
  )))
  expect_identical(nrow(out), 0L)
  # and still fires for the SAS column it was written for
  sas <- lint_dta(list(columns = list(
    list(id = "X", type = "SAS Char", format = "SAS $3.", length = 8)
  )))
  expect_identical(sas$code, "format_narrower_than_length")
})

# ---- a pattern operator on a logical column ---------------------------------------
# The engine reads a logical column as TRUE/FALSE, which hold no text, so a rule
# `pattern` on one never matches a row. It is refused where the rule is read.

test_that("a pattern operator on an R logical column is refused, on text columns it is accepted", {
  pattern_rule <- function(col, then = "TXT", where = "condition", op = "pattern") {
    clause <- stats::setNames(list(stats::setNames(list("^T$"), op)), col)
    other <- stats::setNames(list(list(empty = FALSE)), then)
    rule <- list(id = "p", type = "col_condition")
    rule[[where]] <- clause
    rule[[setdiff(c("condition", "then"), where)]] <- other
    rule
  }
  columns <- c(r_cols(), list(list(id = "SX", type = "SAS Char")))
  expect_identical(
    r_lint(pattern_rule("LGL"), columns), "rule_condition_operator_invalid/error"
  )
  # in the `then` clause too, and on a logical column with a values list
  expect_identical(
    r_lint(pattern_rule("LGL", then = "TXT", where = "then"), columns),
    "rule_condition_operator_invalid/error"
  )
  expect_identical(
    r_lint(pattern_rule("LGV"), columns), "rule_condition_operator_invalid/error"
  )
  # a text column of either backend and the temporal ones still take a pattern
  for (col in c("TXT", "SX", "DAY", "STAMP", "TOD")) {
    expect_identical(r_lint(pattern_rule(col, then = "TXT"), columns), character(0), info = col)
  }
  # and a logical column still takes the equality operators
  expect_identical(r_lint(r_cond("LGL", "equals", "T"), columns), character(0))
})

test_that("the pattern operator on an R logical column is refused at every entry point", {
  columns <- list(list(id = "FLAG", type = "R logical"), list(id = "NAME", type = "R character"))
  bad <- list(list(
    id = "p", type = "col_condition",
    condition = list(FLAG = list(pattern = "^T$")), then = list(NAME = list(empty = FALSE))
  ))
  good <- bad
  good[[1]]$condition <- list(NAME = list(pattern = "^T$"))

  # lint_dta(): a finding of its own
  out <- lint_dta(list(columns = columns, rules = bad))
  expect_identical(out$code, "rule_condition_operator_invalid")
  expect_identical(out$severity, "error")
  expect_match(out$message, "R logical", fixed = TRUE)
  expect_identical(nrow(lint_dta(list(columns = columns, rules = good))), 0L)

  # the collection validator: no collection is built
  err <- tryCatch(specs_from_list(columns, bad), error = identity)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "rule_condition_operator_invalid", fixed = TRUE)
  expect_no_error(specs_from_list(columns, good))

  # the YAML reader lints first and aborts with every finding
  path <- withr::local_tempfile(fileext = ".yaml")
  yaml::write_yaml(list(columns = columns, rules = bad), path)
  err <- tryCatch(import_specs_from_yaml(path), error = identity)
  expect_s3_class(err, "dta_lint_error")
  expect_identical(err$findings$code, "rule_condition_operator_invalid")
  yaml::write_yaml(list(columns = columns, rules = good), path)
  expect_no_error(import_specs_from_yaml(path))

  # validate_template() reports it for a dataset template
  write_tpl <- function(dir, condition) {
    dir.create(file.path(dir, "templates"))
    writeLines(
      c(
        "kind: dta_dataset_template", "id: ruled", "version: \"1.0\"",
        "dataset:", "  id: ds1", "  type: tabular",
        "  files: {filename: x.csv, type: csv}",
        "  columns:",
        "    - {id: FLAG, type: R logical}",
        "    - {id: NAME, type: R character}",
        "  rules:",
        "    - id: p", "      type: col_condition",
        paste0("      condition: {", condition, ": {pattern: '^T$'}}"),
        "      then: {NAME: {empty: false}}"
      ),
      file.path(dir, "templates", "ruled.dta-dataset-template.yaml")
    )
  }
  dir <- withr::local_tempdir()
  write_tpl(dir, "FLAG")
  res <- validate_template(dir)
  hit <- res[res$code == "rule_condition_operator_invalid", , drop = FALSE]
  expect_equal(nrow(hit), 1L)
  expect_identical(hit$severity, "error")
  ok <- withr::local_tempdir()
  write_tpl(ok, "NAME")
  expect_false("rule_condition_operator_invalid" %in% validate_template(ok)$code)
})
