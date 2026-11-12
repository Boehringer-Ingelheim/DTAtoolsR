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
