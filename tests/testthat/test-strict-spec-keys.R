# A specification is read strictly: a key that is not one the reader knows is an
# error that names it, never a key read as another, a default or nothing.
#
# Three places read a mapping from YAML and used to be lenient:
#   * a `type: file` dataset dropped its `columns:` and `rules:` (the mirror-image
#     misplacements on a matrix dataset have always been errors);
#   * the keys of a group-condition constraint were never checked, so a
#     misspelt `then_scop: all` was read as the default "any", and `$` let an
#     absent `left` be answered by `left_scope`;
#   * a column mapping went to do.call(), which completes a name from its first
#     letters (`example:` for `examples:`) and, for a name it cannot place,
#     prints the key's whole value in its own, translated, error.
#
# Every case here has a TYPO or a misplaced key as its input. Expected values are
# written out by hand from the documented keys, never read back from the
# function under test. No assertion is on translated text.

sk_files <- list(type = "csv", filename = "a.csv")
sk_conditions <- list(
  c1 = list(A = list(equals = 1)),
  c2 = list(B = list(equals = 2))
)
sk_rule <- function(constraint) {
  DTARuleGroupCondition(
    id = "g", group_by = "SUBJ", conditions = sk_conditions,
    constraints = list(constraint)
  )
}
# The keys of a column, from the documented signature of DTAColumnSpec().
sk_column_keys <- c(
  "id", "label", "type", "format", "length", "nullable", "pattern",
  "values", "examples", "description", "colclass"
)
# A document with one tabular dataset whose only rule is `rule`, as read from YAML.
sk_document_with_rule <- function(rule) {
  list(
    id = "t", type = "tabular", files = sk_files,
    columns = list(list(id = "A", type = "SAS Char", nullable = FALSE)),
    rules = list(rule)
  )
}
sk_group_rule <- function(constraint) {
  list(
    id = "g", type = "group_condition", group_by = "A",
    conditions = sk_conditions, constraints = list(constraint)
  )
}

# ---- c1-spec-17: columns and rules on a file dataset ---------------------------

test_that("columns on a file dataset is refused by the factory, with its own class", {
  e <- expect_error(
    DTADataSetFactory(
      "file",
      id = "f", files = sk_files,
      columns = list(list(id = "A", type = "SAS Char", nullable = FALSE))
    ),
    class = "dta_file_columns_key"
  )
  expect_match(conditionMessage(e), "dataset has no columns", fixed = TRUE)
})

test_that("rules on a file dataset is refused by the factory, with its own class", {
  e <- expect_error(
    DTADataSetFactory(
      "file",
      id = "f", files = sk_files,
      rules = list(list(id = "u", type = "col_unique", columns = "A"))
    ),
    class = "dta_file_rules_key"
  )
  expect_match(conditionMessage(e), "dataset has no rules", fixed = TRUE)
})

test_that("the same refusal reaches a dataset read from a list, a bare dataset YAML and a document YAML", {
  body <- list(
    id = "f", type = "file", files = sk_files,
    columns = list(list(id = "A", type = "SAS Char"))
  )
  expect_error(dta_dataset_from_list(body), class = "dta_file_columns_key")

  # The readers lint first, so the author sees the lint's finding, every one at once.
  dataset_yaml <- withr::local_tempfile(fileext = ".yaml")
  yaml::write_yaml(body, dataset_yaml)
  expect_error(read_dataset_from_yaml(dataset_yaml), class = "dta_lint_error")

  document_yaml <- withr::local_tempfile(fileext = ".yaml")
  yaml::write_yaml(
    list(
      metadata = list(title = "T", version = "1.0"),
      datasets = list(body)
    ),
    document_yaml
  )
  e <- expect_error(read_dta_from_yaml(document_yaml), class = "dta_lint_error")
  expect_true("file_columns_key" %in% e$findings$code)
})

test_that("lint_dta() reports columns and rules on a file dataset, one error each, and a clean one not at all", {
  clean <- lint_dta(list(id = "f", type = "file", files = sk_files))
  expect_identical(nrow(clean), 0L)

  both <- lint_dta(list(
    id = "f", type = "file", files = sk_files,
    columns = list(list(id = "A", type = "SAS Char")),
    rules = list(list(id = "u", type = "col_unique", columns = "A"))
  ))
  expect_setequal(both$code, c("file_columns_key", "file_rules_key"))
  expect_true(all(both$severity == "error"))
  expect_identical(nrow(both), 2L)
})

test_that("a tabular dataset keeps its columns and rules; only the file type refuses them", {
  ds <- DTADataSetFactory(
    "tabular",
    id = "t", files = sk_files,
    columns = list(list(id = "A", type = "SAS Char", nullable = FALSE)),
    rules = list(list(id = "u", type = "col_unique", columns = "A"))
  )
  expect_identical(names(ds@specs@columns), "A")
  expect_length(ds@specs@rules, 1L)
})


test_that("an empty columns or rules on a file dataset declares nothing and is accepted", {
  # A template that keeps its parent's `rules: []` for a dataset it turns into a
  # file dataset drops no constraint, so it is not an error.
  ds <- DTADataSetFactory("file", id = "f", files = sk_files, columns = list(), rules = list())
  expect_identical(ds@id, "f")
  expect_identical(
    nrow(lint_dta(list(id = "f", type = "file", files = sk_files, columns = list(), rules = list()))),
    0L
  )
})

test_that("validate_template() reports columns and rules on a file dataset template, and nothing for empty ones", {
  validate <- function(extra = NULL) {
    root <- withr::local_tempdir(.local_envir = parent.frame())
    dir.create(file.path(root, "templates"))
    writeLines(
      c(
        "kind: dta_dataset_template", "id: fd", "version: \"1.0\"",
        "dataset:", "  id: report", "  type: file",
        "  files: {filename: report.pdf, type: any}",
        extra
      ),
      file.path(root, "templates", "fd.dta-dataset-template.yaml")
    )
    validate_template(root)
  }
  res <- validate(c(
    "  columns:", "    - {id: ID, type: SAS Char}",
    "  rules:", "    - {id: u, type: col_unique, columns: [ID]}"
  ))
  expect_identical(res$severity[res$code == "file_columns_key"], "error")
  expect_identical(res$severity[res$code == "file_rules_key"], "error")

  clean <- validate(c("  columns: []", "  rules: []"))
  expect_false(any(c("file_columns_key", "file_rules_key") %in% clean$code))
})

# ---- c1-spec-49: the keys of a group-condition constraint ----------------------

test_that("a misspelt scope on a requires constraint is refused, not read as 'any'", {
  e <- expect_error(
    sk_rule(list(type = "requires", `if` = "c1", then = "c2", then_scop = "all")),
    class = "dta_group_constraint_unknown_key"
  )
  expect_identical(e$keys, "then_scop")
  # The message offers the spelling that was meant.
  expect_match(conditionMessage(e), "then_scope", fixed = TRUE)
})

test_that("a misspelt scope on a mutually_exclusive constraint is refused, not read as 'any'", {
  e <- expect_error(
    sk_rule(list(type = "mutually_exclusive", left = "c1", right = "c2", left_scop = "all")),
    class = "dta_group_constraint_unknown_key"
  )
  expect_identical(e$keys, "left_scop")
})

test_that("the scope keys of the other type are refused, not dropped", {
  # `if_scope`/`then_scope` belong to requires; on a mutually_exclusive constraint
  # they used to be dropped and both scopes read as "any".
  e <- expect_error(
    sk_rule(list(
      type = "mutually_exclusive", left = "c1", right = "c2",
      if_scope = "all", then_scope = "all"
    )),
    class = "dta_group_constraint_unknown_key"
  )
  expect_setequal(e$keys, c("if_scope", "then_scope"))

  e <- expect_error(
    sk_rule(list(type = "requires", `if` = "c1", then = "c2", left_scope = "all")),
    class = "dta_group_constraint_unknown_key"
  )
  expect_identical(e$keys, "left_scope")
})

test_that("a misspelt condition key is named as unknown, not as a missing condition name", {
  e <- expect_error(
    sk_rule(list(type = "mutually_exclusive", left = "c1", rigth = "c2")),
    class = "dta_group_constraint_unknown_key"
  )
  expect_identical(e$keys, "rigth")
})

test_that("a misspelt common key (id, message) is refused", {
  e <- expect_error(
    sk_rule(list(type = "requires", `if` = "c1", then = "c2", mesage = "boom")),
    class = "dta_group_constraint_unknown_key"
  )
  expect_identical(e$keys, "mesage")
})

test_that("every unknown key is named at once, an unnamed one too", {
  e <- expect_error(
    sk_rule(list(type = "requires", `if` = "c1", then = "c2", thn_scope = "all", "stray")),
    class = "dta_group_constraint_unknown_key"
  )
  expect_setequal(e$keys, c("thn_scope", ""))
})

test_that("left_scope without left is a missing condition name, not the scope read as the left name", {
  # `cst$left` used to partial-match `left_scope`, so this read left = "all" and
  # failed as an unknown condition name called "all".
  e <- expect_error(
    sk_rule(list(type = "mutually_exclusive", right = "c2", left_scope = "all")),
    class = "dta_group_constraint_missing_condition"
  )
  expect_false(grepl("unknown condition", conditionMessage(e), fixed = TRUE))
})

test_that("a constraint with every key it knows builds, and keeps what was written", {
  rule <- DTARuleGroupCondition(
    id = "g", group_by = "SUBJ", conditions = sk_conditions,
    constraints = list(
      list(
        type = "not_both", id = "k1", message = "m1", left = "c1", right = "c2",
        left_scope = "all", right_scope = "any"
      ),
      list(
        type = "implies", id = "k2", message = "m2", `if` = "c1", then = "c2",
        if_scope = "any", then_scope = "all"
      )
    )
  )
  k1 <- rule@constraints[[1]]
  expect_identical(k1$left, "c1")
  expect_identical(k1$right, "c2")
  expect_identical(k1$left_scope, "all")
  expect_identical(k1$right_scope, "any")
  expect_identical(k1$message, "m1")
  k2 <- rule@constraints[[2]]
  expect_identical(k2[["if"]], "c1")
  expect_identical(k2[["then"]], "c2")
  expect_identical(k2$if_scope, "any")
  expect_identical(k2$then_scope, "all")
})

test_that("lint_dta() reports a misspelt constraint key as an invalid rule that names it", {
  l <- lint_dta(sk_document_with_rule(sk_group_rule(
    list(type = "requires", `if` = "c1", then = "c2", then_scop = "all")
  )))
  bad <- l[l$code == "rule_invalid", ]
  expect_identical(nrow(bad), 1L)
  # `then_scop` followed by anything but `e`: the message also lists `then_scope`.
  expect_match(bad$message, "then_scop(?!e)", perl = TRUE)
})


test_that("lint_dta() names a constraint key of the wrong type, not the undefined condition it carries", {
  # `if: c9` on a mutually_exclusive constraint is the cross-type spelling this
  # reader refuses. It names a condition that does not exist as well, and the
  # finding must be the key, not the condition.
  l <- lint_dta(sk_document_with_rule(sk_group_rule(
    list(type = "mutually_exclusive", left = "c1", right = "c2", `if` = "c9")
  )))
  expect_false("group_constraint_unknown_condition" %in% l$code)
  bad <- l[l$code == "rule_invalid", ]
  expect_identical(nrow(bad), 1L)
  expect_match(bad$message, "\"if\"", fixed = TRUE)
})

# ---- t-core-3-37: the keys of a column mapping ---------------------------------

test_that("a misspelt column key is refused by name, with the allowed keys in the message", {
  e <- expect_error(
    specs_from_list(list(list(id = "A", type = "SAS Char", exmaples = "x"))),
    class = "dta_column_unknown_field"
  )
  expect_identical(e$keys, "exmaples")
  for (key in sk_column_keys) {
    expect_match(conditionMessage(e), key, fixed = TRUE)
  }
})

test_that("an abbreviated column key is refused, not completed from its first letters", {
  # do.call() let `example:` reach `examples:`, `desc:` reach `description:`.
  for (abbreviated in c("example", "desc", "null", "pat")) {
    column <- list(id = "A", type = "SAS Char")
    column[[abbreviated]] <- "x"
    e <- expect_error(
      specs_from_list(list(column)),
      class = "dta_column_unknown_field"
    )
    expect_identical(e$keys, abbreviated)
  }
})

test_that("the error names an unknown key and never prints its value", {
  e <- expect_error(
    specs_from_list(list(list(id = "A", type = "SAS Char", zz = "SENTINEL_VALUE_Q7"))),
    class = "dta_column_unknown_field"
  )
  expect_identical(e$keys, "zz")
  expect_false(grepl("SENTINEL_VALUE_Q7", conditionMessage(e), fixed = TRUE))
})

test_that("every unknown key of a column is named at once, with the column", {
  e <- expect_error(
    specs_from_list(list(list(id = "SUBJ", type = "SAS Char", lable = "x", nulable = FALSE))),
    class = "dta_column_unknown_field"
  )
  expect_setequal(e$keys, c("lable", "nulable"))
  expect_match(conditionMessage(e), "SUBJ", fixed = TRUE)
})

test_that("a column with every key it knows builds", {
  # `values` excludes `pattern` and `examples`, so three columns carry the eleven.
  cols <- specs_from_list(list(
    list(
      id = "A", label = "Label", type = "SAS Char", length = 8,
      nullable = FALSE, values = c("X", "Y"),
      description = "d", colclass = "study_info"
    ),
    list(id = "B", type = "SAS Char", format = "SAS $8.", pattern = "^[A-Z]+$"),
    list(id = "C", type = "SAS Char", examples = "X")
  ))
  spec <- cols@columns[["A"]]
  expect_identical(spec@label, "Label")
  expect_identical(spec@description, "d")
  expect_identical(spec@nullable, FALSE)
  expect_identical(spec@values, c("X", "Y"))
  expect_identical(spec@colclass, "study_info")
  expect_identical(cols@columns[["B"]]@pattern, "^[A-Z]+$")
  expect_identical(cols@columns[["C"]]@examples, "X")
})

test_that("the row labels of a matrix are read strictly too, and keep their header", {
  expect_error(
    dta_row_label_specs(list(list(id = "G", type = "SAS Char", header = "gene", nulable = FALSE))),
    class = "dta_column_unknown_field"
  )
  ok <- dta_row_label_specs(list(list(id = "G", type = "SAS Char", header = "gene", nullable = FALSE)))
  expect_identical(ok@columns[["G"]]@header, "gene")
})

test_that("a dataset read from a list refuses a misspelt column key", {
  expect_error(
    dta_dataset_from_list(list(
      id = "t", type = "tabular", files = sk_files,
      columns = list(list(id = "A", type = "SAS Char", exmaples = "x"))
    )),
    class = "dta_column_unknown_field"
  )
})

test_that("lint_dta() reports a misspelt or abbreviated column key, naming it", {
  lint_column <- function(column) {
    lint_dta(list(id = "t", type = "tabular", files = sk_files, columns = list(column)))
  }
  l <- lint_column(list(id = "A", type = "SAS Char", exmaples = "x"))
  msg <- l$message[l$code == "column_spec_invalid"]
  expect_length(msg, 1L)
  expect_match(msg, "exmaples", fixed = TRUE)

  # What the old suite pinned as intended: `example:` reaching `examples`.
  l <- lint_column(list(id = "A", type = "SAS Char", example = "x"))
  msg <- l$message[l$code == "column_spec_invalid"]
  expect_length(msg, 1L)
  # `example` not followed by `s`: the message also lists the allowed `examples`.
  expect_match(msg, "example(?!s)", perl = TRUE)

  # ... while the exact spelling is clean.
  expect_false("column_spec_invalid" %in% lint_column(list(id = "A", type = "SAS Char", examples = "x"))$code)
})

test_that("lint_dta() keeps naming a header on a tabular column as a row-label key", {
  l <- lint_dta(list(
    id = "t", type = "tabular", files = sk_files,
    columns = list(list(id = "A", type = "SAS Char", header = "a"))
  ))
  msg <- l$message[l$code == "column_spec_invalid"]
  expect_length(msg, 1L)
  expect_match(msg, "row label of a matrix dataset", fixed = TRUE)
})

test_that("validate_template() reports a misspelt or abbreviated column key of a dataset template, and not values_from", {
  validate <- function(column) {
    root <- withr::local_tempdir(.local_envir = parent.frame())
    dir.create(file.path(root, "templates"))
    writeLines(
      c(
        "kind: dta_dataset_template", "id: tpl", "version: \"1.0\"",
        "dataset:", "  id: ds1", "  type: tabular",
        "  files: {filename: x.csv, type: csv}",
        "  columns:", paste0("    - ", column)
      ),
      file.path(root, "templates", "tpl.dta-dataset-template.yaml")
    )
    validate_template(root)
  }
  # The abbreviation used to be dropped before the lint and then completed by
  # do.call() when a creation template imported the dataset: clean here,
  # an abort there.
  res <- validate("{id: A, type: SAS Char, example: x}")
  hit <- res[res$code == "column_spec_invalid", ]
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$severity, "error")
  expect_match(hit$message, "\"example\"", fixed = TRUE)

  res <- validate("{id: A, type: SAS Char, exmaples: x}")
  expect_identical(sum(res$code == "column_spec_invalid"), 1L)

  # The one key a template column may carry beyond a column's own.
  res <- validate("{id: A, type: SAS Char, values_from: {vocabulary: nowhere}}")
  expect_false("column_spec_invalid" %in% res$code)
})
