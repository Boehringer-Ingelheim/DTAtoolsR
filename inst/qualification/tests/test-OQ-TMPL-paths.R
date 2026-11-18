# Ids that contain spaces or dots, in template paths. REQ-TMPL-056, with
# REQ-TMPL-012, -014, -015, -021, -030, -035, -040, -046, -048, -049, -051 and
# -052 where a case belongs to one of them as well.
#
# A column id is the text of a header ("Subject ID", "Wt. (kg)", "No. of
# Doses"), and every template path that names a column used to be split at each
# dot, so the id could not be read back. The expected values below are written
# from the requirement sentences over fixtures built here: which entry a path
# reaches, which row validate_template() reports, which line the history and
# the guide print. None of them is a recording of a run.
#
# Cases that call the application's own helpers (path resolution, slot targets,
# history, diff) are tagged white-box, as in test-OQ-TMPL-matrix.R: the harness
# has no browser and those helpers are what the template dialogs call.

tp_put <- function(root, rel, lines) {
  dir.create(dirname(file.path(root, rel)), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, file.path(root, rel))
  invisible(file.path(root, rel))
}

# validate_template() of a fresh library holding `files` (a named list: path
# relative to the library root -> lines).
tp_validate <- function(files) {
  root <- file.path(qa_tempdir(), paste0("lib", sample.int(1e6, 1L)))
  for (rel in names(files)) tp_put(root, rel, files[[rel]])
  validate_template(root)
}

# The messages of a result with the line breaks cli put in folded away.
tp_msg <- function(result, code) {
  gsub("\\s+", " ", paste(result$message[result$code == code], collapse = " "))
}

tp_codes <- function(result) sort(result$code, method = "radix")

tp_vocabulary <- c(
  "kind: dta_vocabulary", "id: v", "version: \"1.0\"", "label: V", "type: text",
  "terms:", "  - code: A", "  - code: B"
)

# ---- one tabular dataset body, as the app holds it before it builds a dataset ----

tp_body <- function() {
  list(
    id = "inl", type = "tabular", files = list(filename = "inl.csv", type = "csv"),
    columns = list(
      list(id = "Subject ID", type = "SAS Char"),
      list(id = "Subject.ID", type = "SAS Char"),
      list(id = "Wt. (kg)", type = "SAS Num"),
      list(id = "Dose No.", type = "SAS Int"),
      list(id = "a", type = "SAS Char"),
      list(id = "a.b", type = "SAS Char")
    ),
    rules = list(list(id = "r.1", type = "col_unique", columns = "a"))
  )
}

tp_ids <- function(ds, key = "columns") vapply(ds[[key]], function(x) x$id, "")
tp_field <- function(ds, field, key = "columns") {
  vapply(ds[[key]], function(x) as.character(x[[field]] %||% NA_character_), "")
}

test_that("OQ-TMPL-124 | a dataset path takes the whole id, spaces and dots included, and the longest declared id wins | REQ-TMPL-056 REQ-TMPL-048 | tags: white-box", {
  qapp_skip_unless_app_installed()
  set_path <- qapp_fn("apply_dataset_template_path")
  patch <- qapp_fn("apply_dataset_patch")
  refused <- function(expr) {
    tryCatch(
      {
        force(expr)
        NULL
      },
      error = function(e) e
    )
  }

  # Ids 1 to 6 are Subject ID, Subject.ID, Wt. (kg), Dose No., a, a.b. A path
  # sets the `label` of exactly the column it names and of no other.
  paths <- c(
    "dataset.columns.Subject ID.label", "dataset.columns.Subject.ID.label",
    "dataset.columns.Wt. (kg).label", "dataset.columns.Dose No..label",
    "dataset.columns.a.label", "dataset.columns.a.b.label"
  )
  for (i in seq_along(paths)) {
    expected <- rep(NA_character_, 6L)
    expected[[i]] <- "L"
    qa_step(
      sprintf("%s sets the label of column %d and of no other", paths[[i]], i),
      expected, tp_field(set_path(tp_body(), paths[[i]], "L"), "label")
    )
  }

  # The field path below the id is not flat: values_from is a mapping.
  nested <- set_path(tp_body(), "dataset.columns.Wt. (kg).values_from.exclude", "X")
  qa_step(
    "a nested field path below the id is written into that column only",
    c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    vapply(nested$columns, function(cl) !is.null(cl$values_from), NA)
  )
  qa_step("with the value it was given", "X", nested$columns[[3]]$values_from$exclude)

  # A rule id may contain a dot too.
  by_rule <- set_path(tp_body(), "dataset.rules.r.1.description", "D")
  qa_step("a path through the rule r.1 sets that rule's field", "D", by_rule$rules[[1]]$description)

  # Rules: the one-segment id comes first when it is declared; otherwise the
  # longest declared id that leaves a segment, so a dotted rule id with no
  # declared shorter prefix is read whole.
  rule_body <- list(
    id = "inl", type = "tabular", columns = list(list(id = "A", type = "SAS Char")),
    rules = list(
      list(id = "age.range", type = "col_range", columns = "A"),
      list(id = "chk", type = "col_unique", columns = "A"),
      list(id = "chk.description", type = "col_unique", columns = "A")
    )
  )
  qa_step(
    "rules.age.range.description sets the description of the rule age.range, and rules.chk.description that of chk, not of chk.description",
    list(c("D", NA, NA), c(NA, "D", NA)),
    list(
      unname(tp_field(set_path(rule_body, "dataset.rules.age.range.description", "D"), "description", "rules")),
      unname(tp_field(set_path(rule_body, "dataset.rules.chk.description", "D"), "description", "rules"))
    )
  )
  qa_check(
    "a rule path that stops at the rule age.range has no field and is refused",
    inherits(refused(set_path(rule_body, "dataset.rules.age.range", "D")), "error")
  )

  # A matrix dataset's row label GENE.ID beside a row label GENE.
  matrix_body <- list(
    id = "counts", type = "matrix", files = list(filename = "c.tsv", type = "tsv"),
    row_labels = list(
      list(id = "GENE", type = "SAS Char", nullable = FALSE),
      list(id = "GENE.ID", type = "SAS Char", nullable = FALSE)
    ),
    matrix = list(cells = list(type = "SAS Num"))
  )
  by_label <- set_path(matrix_body, "dataset.row_labels.GENE.ID.nullable", TRUE)
  qa_step(
    "a path through the row label GENE.ID sets that row label's field and not GENE's",
    c(FALSE, TRUE), vapply(by_label$row_labels, function(x) x$nullable, NA)
  )

  # An id that is not declared is refused, naming the id as written; nothing is
  # written to an entry whose id merely starts the same way.
  unknown <- refused(set_path(tp_body(), "dataset.columns.Nope. (x).label", "L"))
  qa_check("an id that is not declared is refused", inherits(unknown, "error"))
  qa_check(
    "naming the id as written",
    inherits(unknown, "error") && grepl("Nope. (x)", gsub("\\s+", " ", conditionMessage(unknown)), fixed = TRUE)
  )
  qa_check(
    "and it is not the refusal of a declared column id that has no field behind it",
    !inherits(unknown, "dta_template_path_no_field")
  )
  qa_check(
    "a path whose id is only the start of a declared id (Wt) is refused too",
    inherits(refused(set_path(tp_body(), "dataset.columns.Wt.label", "L")), "error")
  )
  # A column path has to end in one of the column's fields. One that stops at a
  # declared column id, or runs on with a segment that is not a field of it, is
  # refused with the class dta_template_path_no_field, naming the id.
  no_field <- function(path, body = tp_body()) {
    err <- refused(set_path(body, path, "L"))
    list(
      class = inherits(err, "dta_template_path_no_field"),
      names_id = inherits(err, "error") && grepl("Wt. (kg)", gsub("\\s+", " ", conditionMessage(err)), fixed = TRUE)
    )
  }
  qa_step(
    "a path that stops at the declared column Wt. (kg) is refused as a path with no field, naming the column",
    list(class = TRUE, names_id = TRUE), no_field("dataset.columns.Wt. (kg)")
  )
  qa_check(
    "a declared id followed by a segment that is not one of its fields (Subject ID.nofield) is refused the same way",
    inherits(refused(set_path(tp_body(), "dataset.columns.Subject ID.nofield", "L")), "dta_template_path_no_field")
  )

  # With a shorter declared id as a prefix: a column Wt beside a column Wt. (kg),
  # and a beside a.b. For a column the segment after the id has to be a column
  # field name, so a path that stops at the longer column is refused, and is not
  # written into the shorter one as a field called " (kg)" or "b".
  pair_body <- list(
    id = "inl", type = "tabular", files = list(filename = "inl.csv", type = "csv"),
    columns = list(list(id = "Wt", type = "SAS Char"), list(id = "Wt. (kg)", type = "SAS Num"))
  )
  qa_step(
    "with columns Wt and Wt. (kg), a path that stops at Wt. (kg) is refused as a path with no field, not written into Wt",
    list(class = TRUE, names_id = TRUE), no_field("dataset.columns.Wt. (kg)", pair_body)
  )
  qa_check(
    "and so is a path that stops at the column a.b beside a",
    inherits(refused(set_path(tp_body(), "dataset.columns.a.b", "L")), "dta_template_path_no_field")
  )
  qa_step(
    "yet each of the two is reached through a column field: Wt.label is the label of Wt, Wt. (kg).label that of Wt. (kg)",
    list(c("L", NA), c(NA, "L")),
    list(
      unname(tp_field(set_path(pair_body, "dataset.columns.Wt.label", "L"), "label")),
      unname(tp_field(set_path(pair_body, "dataset.columns.Wt. (kg).label", "L"), "label"))
    )
  )

  # The patch verbs read ids whole as well.
  out <- patch(tp_body(), list(
    modify_columns = list(list(id = "Wt. (kg)", label = "Weight")),
    add_columns = list(list(id = "Visit Date (UTC)", type = "SAS Char")),
    remove_columns = "a.b",
    set = list("columns.Dose No..description" = "Dose number")
  ))$dataset
  qa_step(
    "remove_columns, add_columns, modify_columns and set leave these columns, in this order",
    c("Subject ID", "Subject.ID", "Wt. (kg)", "Dose No.", "a", "Visit Date (UTC)"), tp_ids(out)
  )
  qa_step(
    "modify_columns changed the label of Wt. (kg) and set the description of Dose No., and nothing else carries either",
    list(label = c(NA, NA, "Weight", NA, NA, NA), description = c(NA, NA, NA, "Dose number", NA, NA)),
    list(label = unname(tp_field(out, "label")), description = unname(tp_field(out, "description")))
  )
  verbs <- patch(tp_body(), list(columns = list(
    modify = list(list(id = "a.b", label = "AB")), remove = "Subject.ID"
  )))$dataset
  qa_step(
    "the column verbs name a.b whole: it is modified, a is not, and Subject.ID is removed",
    list(ids = c("Subject ID", "Wt. (kg)", "Dose No.", "a", "a.b"), label = c(NA, NA, NA, NA, "AB")),
    list(ids = tp_ids(verbs), label = unname(tp_field(verbs, "label")))
  )
  unknown_patch <- refused(patch(tp_body(), list(modify_columns = list(list(id = "Wt", label = "x")))))
  qa_check(
    "a patch naming Wt, which is only the start of a declared id, is refused, naming Wt",
    inherits(unknown_patch, "error") && grepl("Wt", conditionMessage(unknown_patch), fixed = TRUE)
  )
})

# ---- REQ-TMPL-012, REQ-TMPL-015, REQ-TMPL-035: what validate_template() says ------

tp_dataset_template <- function(id = "dsx", options = NULL, columns = NULL) {
  c(
    "kind: dta_dataset_template", paste0("id: ", id), "version: \"1.0\"",
    options,
    "dataset:", "  id: inl", "  type: tabular", "  files: {filename: inl.csv, type: csv}",
    "  columns:",
    columns %||% c(
      "    - {id: Subject ID, type: SAS Char, length: 8, nullable: No}",
      "    - {id: Wt. (kg), type: SAS Num, nullable: Yes}",
      "    - {id: Dose No., type: SAS Int, nullable: Yes}",
      "    - {id: a, type: SAS Char, length: 3, nullable: Yes}",
      "    - {id: a.b, type: SAS Char, length: 3, nullable: Yes}"
    )
  )
}

tp_importer <- function(patch_lines, id = "c1") {
  c(
    "kind: dta_creation_template", paste0("id: ", id), "version: \"1.0\"", "base: {metadata: {title: T}}",
    "datasets:", "  - template: dsx@1.0", "    as: inl", "    patch:", patch_lines
  )
}

test_that("OQ-TMPL-125 | options and patches that name an id with spaces or dots validate cleanly, and an id that is not declared is reported by its whole text | REQ-TMPL-056 REQ-TMPL-012 REQ-TMPL-015 REQ-TMPL-035", {
  clean_option <- c(
    "options:",
    "  - {id: o1, label: Weight label, type: text, target: 'dataset.columns.Wt. (kg).label', default: Weight}",
    "  - {id: o2, label: AB label, type: text, target: 'dataset.columns.a.b.label', default: AB}"
  )
  clean_patch <- c(
    "      modify_columns:", "        - {id: Wt. (kg), label: Weight}",
    "      set:", "        columns.Wt. (kg).description: Body weight", "        columns.a.b.label: AB label",
    "      add_columns:", "        - {id: Visit Date (UTC), type: SAS Char, length: 10, nullable: Yes}"
  )
  files <- function(...) {
    c(list("templates/dsx.dta-dataset-template.yaml" = tp_dataset_template(options = clean_option)), list(...))
  }

  clean <- tp_validate(files("templates/c1.dta-template.yaml" = tp_importer(clean_patch)))
  qa_step(
    "options, modify_columns, add_columns and set paths that name Wt. (kg), a.b and Dose No. by their whole ids are not reported",
    0L, nrow(clean)
  )

  # REQ-TMPL-015: a patch naming an id the dataset template does not have.
  bad_modify <- tp_validate(files("templates/c1.dta-template.yaml" = tp_importer(
    c("      modify_columns:", "        - {id: Wt, label: Weight}")
  )))
  qa_step(
    "modify_columns naming Wt, the start of a declared id but not one, is patch_incoherent at severity warning",
    "warning", bad_modify$severity[bad_modify$code == "patch_incoherent"]
  )
  qa_check("and the message names it", grepl("'Wt'", tp_msg(bad_modify, "patch_incoherent"), fixed = TRUE))

  bad_set <- tp_validate(files("templates/c1.dta-template.yaml" = tp_importer(
    c("      set:", "        columns.Nope. (x).description: Body weight")
  )))
  qa_step(
    "a set: path through an id that is not declared is patch_incoherent, once, at severity warning",
    "warning", bad_set$severity[bad_set$code == "patch_incoherent"]
  )
  qa_check(
    "naming the whole id Nope. (x)",
    grepl("Nope. (x)", tp_msg(bad_set, "patch_incoherent"), fixed = TRUE)
  )

  # REQ-TMPL-012: the option of a dataset template.
  bad_option <- tp_validate(list("templates/dsx.dta-dataset-template.yaml" = tp_dataset_template(options = c(
    "options:",
    "  - {id: o1, label: L, type: text, target: 'dataset.columns.Nope. (x).label', default: x}",
    "  - {id: o2, label: L, type: text, target: 'dataset.columns.Wt. (kg).label', default: x}"
  ))))
  qa_step(
    "one option target through an undeclared id is target_invalid, and the one through Wt. (kg) is not",
    "target_invalid", bad_option$code
  )
  qa_step("at severity warning", "warning", bad_option$severity)
  qa_check("naming the whole id Nope. (x)", grepl("Nope. (x)", tp_msg(bad_option, "target_invalid"), fixed = TRUE))

  # A target that stops at a column whose id has a shorter declared id as its
  # prefix has no field behind it, so it is refused too.
  stops_at_column <- tp_validate(list("templates/dsx.dta-dataset-template.yaml" = tp_dataset_template(
    options = c(
      "options:",
      "  - {id: o1, label: L, type: text, target: 'dataset.columns.Wt. (kg)', default: x}",
      "  - {id: o2, label: L, type: text, target: 'dataset.columns.Wt. (kg).label', default: x}"
    ),
    columns = c(
      "    - {id: Wt, type: SAS Num, nullable: Yes}",
      "    - {id: Wt. (kg), type: SAS Num, nullable: Yes}"
    )
  )))
  qa_step(
    "an option target that stops at Wt. (kg) beside a column Wt is target_invalid, once, and the one with a field is not",
    c("target_invalid", "warning"), c(stops_at_column$code, stops_at_column$severity)
  )
  qa_check(
    "and the message names the column Wt. (kg) whole",
    grepl("Wt. (kg)", tp_msg(stops_at_column, "target_invalid"), fixed = TRUE)
  )

  # REQ-TMPL-035 and REQ-TMPL-056: the lint finding rides along as its own row.
  shadow <- tp_validate(list("templates/dsx.dta-dataset-template.yaml" = tp_dataset_template(columns = c(
    "    - {id: Specimen, type: SAS Char, length: 3, nullable: Yes}",
    "    - {id: Specimen.type, type: SAS Char, length: 3, nullable: Yes}",
    "    - {id: a, type: SAS Char, length: 3, nullable: Yes}",
    "    - {id: a.b, type: SAS Char, length: 3, nullable: Yes}",
    "    - {id: Wt. (kg), type: SAS Num, nullable: Yes}"
  ))))
  qa_step(
    "a dataset template declaring Specimen and Specimen.type is column_id_shadows_field, alone, at severity warning",
    list(code = "column_id_shadows_field", severity = "warning"),
    list(code = shadow$code, severity = shadow$severity)
  )
  qa_check("naming the longer id", grepl("Specimen.type", tp_msg(shadow, "column_id_shadows_field"), fixed = TRUE))
})

# ---- REQ-TMPL-014: vocabulary slot targets ---------------------------------------

test_that("OQ-TMPL-126 | a vocabulary slot target names a column by everything between its segment and .values, and the dataset by one segment | REQ-TMPL-056 REQ-TMPL-014 REQ-TMPL-049 | tags: white-box", {
  qapp_skip_unless_app_installed()
  parts <- qapp_fn("vocabulary_slot_target_parts")
  read <- function(target) {
    p <- parts(target)
    if (is.null(p)) NULL else c(dataset = p$dataset, column = p$column, segment = p$segment)
  }

  qa_step(
    "a column id with a space and a dot",
    c(dataset = "inl", column = "Wt. (kg)", segment = "columns"), read("datasets.inl.columns.Wt. (kg).values")
  )
  qa_step(
    "a column id that ends in a dot",
    c(dataset = "inl", column = "Dose No.", segment = "columns"), read("datasets.inl.columns.Dose No..values")
  )
  qa_step(
    "a column id that itself ends in .values: the id is everything before the last .values",
    c(dataset = "inl", column = "a.values", segment = "columns"), read("datasets.inl.columns.a.values.values")
  )
  qa_step(
    "a row label GENE.ID of a matrix",
    c(dataset = "counts", column = "GENE.ID", segment = "row_labels"), read("datasets.counts.row_labels.GENE.ID.values")
  )
  qa_check("a dataset id that contains a dot is not addressable: the target is malformed", is.null(read("datasets.v2.1.columns.X.values")))
  qa_check("a target with no column id is malformed", is.null(read("datasets.inl.columns.values")))

  # Applying a selection sets the values of the column named, and of no other.
  apply_overrides <- qapp_fn("apply_vocabulary_slot_overrides")
  out <- apply_overrides(tp_body(), list(
    list(dataset = "inl", column = "a.b", values = c("A", "B"), slot = "s1", segment = "columns"),
    list(dataset = "inl", column = "Wt. (kg)", values = "1", slot = "s2", segment = "columns")
  ))
  qa_step(
    "the selections for a.b and Wt. (kg) land in those columns and in no other",
    list(a_b = c("A", "B"), wt = "1", others = rep(TRUE, 4L)),
    list(
      a_b = out$columns[[6]]$values, wt = out$columns[[3]]$values,
      others = vapply(out$columns[c(1, 2, 4, 5)], function(cl) is.null(cl$values), NA)
    )
  )

  undeclared <- tryCatch(
    apply_overrides(tp_body(), list(list(dataset = "inl", column = "Nope. (kg)", values = "A", slot = "s1", segment = "columns"))),
    error = function(e) e
  )
  qa_check(
    "a selection through a column id the dataset does not declare is refused, naming the id as written",
    inherits(undeclared, "error") && grepl("Nope. (kg)", gsub("\\s+", " ", conditionMessage(undeclared)), fixed = TRUE)
  )

  # validate_template() reads the grammar the same way.
  slot_lib <- function(dataset_id, target) {
    list(
      "vocabulary/v.dta-vocabulary.yaml" = tp_vocabulary,
      "templates/c1.dta-template.yaml" = c(
        "kind: dta_creation_template", "id: c1", "version: \"1.0\"", "base: {metadata: {title: T}}",
        "datasets:", paste0("  - id: ", dataset_id), "    type: tabular",
        "    files: {filename: inl.csv, type: csv}", "    columns:",
        "      - {id: Wt. (kg), type: SAS Char, length: 3, nullable: Yes}",
        "vocabulary_slots:", paste0("  - {id: s1, label: S, target: '", target, "', vocabulary: v@1.0}")
      )
    )
  }
  ok <- tp_validate(slot_lib("inl", "datasets.inl.columns.Wt. (kg).values"))
  qa_step("a target through the column Wt. (kg) is not reported", 0L, nrow(ok))
  dotted_dataset <- tp_validate(slot_lib("v2.1", "datasets.v2.1.columns.Wt. (kg).values"))
  qa_step(
    "a target through a dataset whose id contains a dot is vocab_slot_invalid, at severity error",
    "error", dotted_dataset$severity[dotted_dataset$code == "vocab_slot_invalid"][[1]]
  )
})

# ---- REQ-TMPL-021: seals; and required: ---------------------------------------------

tp_columns <- function(wt_label = "Weight", wt_description = "Body weight", a_description = NULL,
                       ab_description = NULL, extra = NULL) {
  field <- function(name, value) if (is.null(value)) "" else paste0(", ", name, ": ", value)
  c(
    "      - {id: Subject ID, type: SAS Char, length: 8, nullable: No}",
    paste0("      - {id: Wt. (kg), type: SAS Num, nullable: Yes", field("label", wt_label), field("description", wt_description), "}"),
    paste0("      - {id: a, type: SAS Char, length: 3, nullable: Yes", field("description", a_description), "}"),
    paste0("      - {id: a.b, type: SAS Char, length: 3, nullable: Yes", field("description", ab_description), "}"),
    extra
  )
}

tp_inline <- function(id, columns, dataset_id = "inl", header = NULL, abstract = FALSE) {
  c(
    "kind: dta_creation_template", paste0("id: ", id), "version: \"1.0\"",
    if (abstract) "abstract: true",
    header,
    "base: {metadata: {title: T}}",
    "datasets:", paste0("  - id: ", dataset_id), "    type: tabular",
    "    files: {filename: inl.csv, type: csv}", "    columns:", columns
  )
}

# A parent that seals `sealed` and a child that extends it and restates the
# dataset with `child_columns`.
tp_seal_case <- function(sealed, child_columns, dataset_id = "inl", parent_columns = tp_columns()) {
  suppressWarnings(tp_validate(list(
    "templates/p.dta-template.yaml" = tp_inline(
      "p", parent_columns, dataset_id,
      header = c("sealed:", paste0("  - \"", sealed, "\"")), abstract = TRUE
    ),
    "templates/c.dta-template.yaml" = c(
      "kind: dta_creation_template", "id: c", "version: \"1.0\"", "extends: p@1.0",
      "datasets:", paste0("  - id: ", dataset_id), "    type: tabular",
      "    files: {filename: inl.csv, type: csv}", "    columns:", child_columns
    )
  )))
}

test_that("OQ-TMPL-127 | a seal or required path through an id with spaces or dots is enforced, may end at the column, and is not reported unknown while it resolves | REQ-TMPL-056 REQ-TMPL-021", {
  wt_label <- "datasets.inl.columns.Wt. (kg).label"
  whole_wt <- "datasets.inl.columns.Wt. (kg)"

  # A seal on a field of Wt. (kg): the child changes another column and the seal
  # holds, with no row at all (a seal that matched nothing would be reported).
  held <- tp_seal_case(wt_label, tp_columns(ab_description = "changed elsewhere"))
  qa_step("a child that leaves the sealed label alone is not reported, and the seal is not unknown", 0L, nrow(held))

  # The child changes the sealed field: loud, and the build fails.
  broken <- tp_seal_case(wt_label, tp_columns(wt_label = "Changed"))
  qa_step(
    "a child that changes the sealed label is sealed_violation and instantiate_failed",
    c("instantiate_failed", "sealed_violation"), tp_codes(broken)
  )
  qa_step("sealed_violation is a warning", "warning", broken$severity[broken$code == "sealed_violation"])
  qa_step("instantiate_failed is an error", "error", broken$severity[broken$code == "instantiate_failed"])
  qa_check("the message names the sealed path whole", grepl(wt_label, tp_msg(broken, "sealed_violation"), fixed = TRUE))

  # A path that ends at the column seals every field of it.
  whole_changed <- tp_seal_case(whole_wt, tp_columns(wt_description = "Changed"))
  qa_step(
    "a seal that ends at Wt. (kg) is violated by a change to any field of that column",
    c("instantiate_failed", "sealed_violation"), tp_codes(whole_changed)
  )
  whole_other <- tp_seal_case(whole_wt, tp_columns(ab_description = "changed elsewhere"))
  qa_step("and not by a change to another column", 0L, nrow(whole_other))

  # a and a.b are two columns: a seal on a field of a.b is not tripped by a.
  ab_sealed <- "datasets.inl.columns.a.b.description"
  qa_step(
    "a seal on a field of a.b is not tripped by a change to column a",
    0L, nrow(tp_seal_case(ab_sealed, tp_columns(a_description = "new", ab_description = "same"), parent_columns = tp_columns(ab_description = "same")))
  )
  qa_step(
    "but it is tripped by a change to a.b",
    c("instantiate_failed", "sealed_violation"),
    tp_codes(tp_seal_case(ab_sealed, tp_columns(ab_description = "new"), parent_columns = tp_columns(ab_description = "same")))
  )

  # A dataset whose own id contains a dot.
  dotted_dataset <- "datasets.inl.v2.columns.Wt. (kg).label"
  qa_step(
    "a seal through a dataset id that contains a dot holds when the child leaves it alone",
    0L, nrow(tp_seal_case(dotted_dataset, tp_columns(), dataset_id = "inl.v2"))
  )
  qa_step(
    "and is violated when the child changes it",
    c("instantiate_failed", "sealed_violation"),
    tp_codes(tp_seal_case(dotted_dataset, tp_columns(wt_label = "Changed"), dataset_id = "inl.v2"))
  )

  # A seal that matches nothing is still the warning that says so.
  typo <- tp_seal_case("datasets.inl.columns.Wt. (kg).nope", tp_columns())
  qa_step("a sealed field Wt. (kg) does not have is sealed_path_unknown, on the child and on the parent", rep("sealed_path_unknown", 2L), typo$code)
  qa_step("at severity warning", rep("warning", 2L), typo$severity)
  undeclared <- tp_seal_case("datasets.inl.columns.Nope. (kg).label", tp_columns())
  qa_step("so is a seal through an id that is not declared", rep("sealed_path_unknown", 2L), undeclared$code)

  # A column Specimen.type beside a column Specimen: in a seal, columns.Specimen.type
  # is the field `type` of Specimen (a declared id followed by a column field
  # name is read first), so adding the column changes nothing the seal guards,
  # and changing that field is still the violation.
  specimen <- c("      - {id: Specimen, type: SAS Char, length: 3, nullable: Yes}")
  specimen_type <- "      - {id: Specimen.type, type: SAS Char, length: 3, nullable: Yes}"
  specimen_seal <- "datasets.inl.columns.Specimen.type"
  failed <- c("instantiate_failed", "sealed_violation")
  qa_step(
    "a child that adds a column Specimen.type beside Specimen does not trip a seal on the field type of Specimen",
    character(0),
    intersect(tp_codes(tp_seal_case(specimen_seal, c(specimen, specimen_type), parent_columns = specimen)), failed)
  )
  qa_step(
    "and one that changes the field type of Specimen is sealed_violation, with or without that column",
    list(c("instantiate_failed", "sealed_violation"), c("instantiate_failed", "sealed_violation")),
    list(
      tp_codes(tp_seal_case(
        specimen_seal, "      - {id: Specimen, type: SAS Num, length: 3, nullable: Yes}",
        parent_columns = specimen
      )),
      tp_codes(tp_seal_case(
        specimen_seal, c("      - {id: Specimen, type: SAS Num, length: 3, nullable: Yes}", specimen_type),
        parent_columns = specimen
      ))
    )
  )
  qa_step(
    "and one that adds nothing is not reported at all",
    0L, nrow(tp_seal_case(specimen_seal, specimen, parent_columns = specimen))
  )

  # Columns X, X.values and X.description. The seal on columns.X.values and the
  # required path columns.X.description are meant for the column X: the child
  # changing X's values is the violation, the child changing the column X.values
  # is not; a blank description of X fails the required check, a blank
  # description of the column X.description does not.
  x_columns <- function(x_values = "[v1, v2]", x_description = "x-desc", xv_values = "[w1]", xd_description = "of X.description") {
    c(
      paste0("      - {id: X, type: SAS Char, values: ", x_values, if (!is.null(x_description)) paste0(", description: ", x_description), "}"),
      paste0("      - {id: X.values, type: SAS Char, values: ", xv_values, ", description: of X.values}"),
      paste0("      - {id: X.description, type: SAS Char", if (!is.null(xd_description)) paste0(", description: ", xd_description), "}")
    )
  }
  x_seal <- "datasets.inl.columns.X.values"
  x_unchanged <- tp_seal_case(x_seal, x_columns(), parent_columns = x_columns())
  qa_step(
    "with X, X.values and X.description, the unchanged child is reported only for the pair, once per column of it",
    rep("column_id_shadows_field", 2L), tp_codes(x_unchanged)
  )
  qa_step(
    "a child that changes the values of X is sealed_violation",
    c("instantiate_failed", "sealed_violation"),
    tp_codes(tp_seal_case(x_seal, x_columns(x_values = "[v1, CHANGED]"), parent_columns = x_columns()))
  )
  qa_step(
    "a child that changes only the values of the column X.values does not trip that seal",
    character(0),
    intersect(tp_codes(tp_seal_case(x_seal, x_columns(xv_values = "[CHANGED]"), parent_columns = x_columns())), failed)
  )
  # Building the dataset warns about the pair (column_id_shadows_field); that is expected here.
  x_required <- function(columns) {
    suppressWarnings(tp_validate(list("templates/p.dta-template.yaml" = tp_inline(
      "p", columns,
      header = c("required:", "  - \"datasets.inl.columns.X.description\"")
    ))))
  }
  qa_step(
    "a required path columns.X.description is satisfied while X has a description",
    character(0), intersect(tp_codes(x_required(x_columns())), "instantiate_failed")
  )
  qa_step(
    "and is instantiate_failed, an error, once X has none",
    list("instantiate_failed", "error"),
    {
      blank <- x_required(x_columns(x_description = NULL))
      list(blank$code[blank$code == "instantiate_failed"], blank$severity[blank$code == "instantiate_failed"])
    }
  )
  qa_step(
    "a blank description of the column X.description does not fail it",
    character(0), intersect(tp_codes(x_required(x_columns(xd_description = NULL))), "instantiate_failed")
  )

  # required: through the same ids.
  required_lib <- function(path, wt_description = "Body weight") {
    tp_validate(list("templates/p.dta-template.yaml" = tp_inline(
      "p", tp_columns(wt_description = wt_description),
      header = c("required:", paste0("  - \"", path, "\""))
    )))
  }
  qa_step(
    "a required path through Wt. (kg) that has a value is satisfied",
    0L, nrow(required_lib("datasets.inl.columns.Wt. (kg).description"))
  )
  qa_step(
    "a required path that ends at the column is satisfied by the column",
    0L, nrow(required_lib(whole_wt))
  )
  unfilled <- required_lib("datasets.inl.columns.Wt. (kg).description", wt_description = NULL)
  qa_step("a required path whose field has no value is instantiate_failed, an error", list("instantiate_failed", "error"), list(unfilled$code, unfilled$severity))
  qa_check(
    "naming the path whole",
    grepl("datasets.inl.columns.Wt. (kg).description", tp_msg(unfilled, "instantiate_failed"), fixed = TRUE)
  )
  qa_step(
    "so is a required path through an id that is not declared",
    "instantiate_failed", required_lib("datasets.inl.columns.Nope. (kg).description")$code
  )

  # In rules and options the one-segment id comes first when it is declared:
  # with rules chk and chk.description, rules.chk.description is the field
  # description of chk, and with options hdr and hdr.default,
  # options.hdr.default is the field default of hdr. Here chk and hdr HOLD the
  # field: a child that changes it is the violation; a child that only changes
  # the field of the longer entry is not.
  rule_lines <- function(chk = "chk-desc", dotted = "of-chk.description") {
    c(
      "    rules:",
      paste0("      - {id: chk, type: col_unique, columns: [A], description: ", chk, "}"),
      paste0("      - {id: chk.description, type: col_unique, columns: [A], description: ", dotted, "}")
    )
  }
  with_rules <- function(id, rules, header = NULL, abstract = FALSE, extends = NULL) {
    c(
      "kind: dta_creation_template", paste0("id: ", id), "version: \"1.0\"", if (abstract) "abstract: true",
      extends, header, "base: {metadata: {title: T}}",
      "datasets:", "  - id: inl", "    type: tabular", "    files: {filename: inl.csv, type: csv}",
      "    columns:", "      - {id: A, type: SAS Char, length: 3, nullable: Yes}", rules
    )
  }
  rule_case <- function(child_rules) {
    suppressWarnings(tp_validate(list(
      "templates/p.dta-template.yaml" = with_rules(
        "p", rule_lines(), c("sealed:", "  - \"datasets.inl.rules.chk.description\""),
        abstract = TRUE
      ),
      "templates/c.dta-template.yaml" = with_rules("c", child_rules, extends = "extends: p@1.0")
    )))
  }
  qa_step("with rules chk and chk.description, a child that changes nothing is not reported", 0L, nrow(rule_case(rule_lines())))
  qa_step(
    "a child that changes the description of the rule chk is sealed_violation",
    c("instantiate_failed", "sealed_violation"), tp_codes(rule_case(rule_lines(chk = "CHANGED")))
  )
  qa_step(
    "a child that changes only the description of the rule chk.description is not reported",
    0L, nrow(rule_case(rule_lines(dotted = "CHANGED")))
  )

  option_lines <- function(hdr = "p1", dotted = "p2") {
    c(
      "options:",
      paste0("  - {id: hdr, label: H, type: text, target: metadata.title, default: ", hdr, "}"),
      paste0("  - {id: hdr.default, label: H2, type: text, target: metadata.header, default: ", dotted, "}")
    )
  }
  option_case <- function(child_options) {
    suppressWarnings(tp_validate(list(
      "templates/p.dta-template.yaml" = c(
        "kind: dta_creation_template", "id: p", "version: \"1.0\"", "abstract: true",
        "sealed:", "  - \"options.hdr.default\"", option_lines(), "base: {metadata: {title: T}}",
        "datasets:", "  - id: inl", "    type: tabular", "    files: {filename: inl.csv, type: csv}",
        "    columns:", "      - {id: A, type: SAS Char, length: 3, nullable: Yes}"
      ),
      "templates/c.dta-template.yaml" = c(
        "kind: dta_creation_template", "id: c", "version: \"1.0\"", "extends: p@1.0", child_options
      )
    )))
  }
  qa_step("with options hdr and hdr.default, a child that restates them unchanged is not reported", 0L, nrow(option_case(option_lines())))
  qa_step(
    "a child that changes the default of the option hdr is sealed_violation",
    c("instantiate_failed", "sealed_violation"), tp_codes(option_case(option_lines(hdr = "CHANGED")))
  )
  qa_step(
    "a child that changes only the default of the option hdr.default is not reported",
    0L, nrow(option_case(option_lines(dotted = "CHANGED")))
  )

  # The shorter entry LACKING the field beside a longer <id>.<field> sibling:
  # the path still reads the shorter entry, so a required path on it fails and a
  # seal on it matches nothing (on the child and on the parent), exactly as when
  # there is no sibling. The sibling is not read instead.
  lacking_rules <- c(
    "    rules:",
    "      - {id: chk, type: col_unique, columns: [A]}",
    "      - {id: chk.description, type: col_unique, columns: [A], description: of-the-sibling}"
  )
  rule_path <- "datasets.inl.rules.chk.description"
  required_rule <- suppressWarnings(tp_validate(list("templates/p.dta-template.yaml" = with_rules(
    "p", lacking_rules,
    header = c("required:", paste0("  - \"", rule_path, "\""))
  ))))
  sealed_rule <- suppressWarnings(tp_validate(list(
    "templates/p.dta-template.yaml" = with_rules("p", lacking_rules, c("sealed:", paste0("  - \"", rule_path, "\"")), abstract = TRUE),
    "templates/c.dta-template.yaml" = with_rules("c", lacking_rules, extends = "extends: p@1.0")
  )))
  lacking_options <- c(
    "options:",
    "  - {id: hdr, label: H, type: text, target: metadata.title}",
    "  - {id: hdr.default, label: H2, type: text, target: metadata.header, default: of-the-sibling}"
  )
  sealed_option <- suppressWarnings(tp_validate(list(
    "templates/p.dta-template.yaml" = c(
      "kind: dta_creation_template", "id: p", "version: \"1.0\"", "abstract: true",
      "sealed:", "  - \"options.hdr.default\"", lacking_options, "base: {metadata: {title: T}}",
      "datasets:", "  - id: inl", "    type: tabular", "    files: {filename: inl.csv, type: csv}",
      "    columns:", "      - {id: A, type: SAS Char, length: 3, nullable: Yes}"
    ),
    "templates/c.dta-template.yaml" = c("kind: dta_creation_template", "id: c", "version: \"1.0\"", "extends: p@1.0")
  )))
  qa_step(
    "with rules chk (no description) and chk.description, a required path rules.chk.description fails, and a seal on it, like one on options.hdr.default beside an option hdr without a default, matches nothing",
    list(
      required = "instantiate_failed",
      sealed_rule = c("sealed_path_unknown", "sealed_path_unknown"),
      sealed_option = c("sealed_path_unknown", "sealed_path_unknown")
    ),
    list(required = required_rule$code, sealed_rule = sealed_rule$code, sealed_option = sealed_option$code)
  )
})

# ---- REQ-TMPL-051, REQ-TMPL-052: the diff and the history ---------------------------

tp_dta <- function(datasets) {
  path <- file.path(qa_tempdir(), paste0("spec", sample.int(1e6, 1L), ".yaml"))
  lines <- c("metadata:", "  title: T", "  version: \"1.0\"", "datasets:")
  for (name in names(datasets)) {
    lines <- c(
      lines, paste0("  - id: ", name), "    type: tabular", "    files: { filename: d.csv, type: csv }",
      "    columns:", datasets[[name]]
    )
  }
  writeLines(lines, path)
  suppressMessages(read_dta_from_yaml(path))
}

tp_cols_yaml <- function(a = "No", ab = "No", wt_description = "w", extra = NULL) {
  c(
    paste0("      - { id: a, type: SAS Char, nullable: ", a, " }"),
    paste0("      - { id: a.b, type: SAS Char, nullable: ", ab, " }"),
    paste0("      - { id: Wt. (kg), type: SAS Num, nullable: Yes, description: ", wt_description, " }"),
    extra
  )
}

test_that("OQ-TMPL-128 | the diff keys and the history name a column by its whole id, tell a from a.b, and count a new dataset as one line | REQ-TMPL-052 REQ-TMPL-051 REQ-TMPL-056 | tags: white-box", {
  qapp_skip_unless_app_installed()
  compare <- qapp_fn("template_history_compare")
  diff <- qapp_fn("dta_diff")
  build <- function(dta) list(def = list(label = "T"), lineage = NULL, mixins = NULL, dta = dta)
  before <- tp_dta(list(ds1 = tp_cols_yaml()))

  # One field of Wt. (kg).
  after <- tp_dta(list(ds1 = tp_cols_yaml(wt_description = "weight")))
  rows <- compare(build(before), build(after))
  qa_step("a change to the description of Wt. (kg) is one line, a change", list(n = 1L, change = "changed"), list(n = nrow(rows), change = rows$change))
  qa_check("worded with the whole id", grepl("column 'Wt. (kg)'", rows$what, fixed = TRUE))
  qa_step(
    "and keyed in the diff by the whole id",
    "ds1.columns.Wt. (kg).description", diff(before, after)$datasets$key
  )

  # a.b and a are two columns whose ids start alike.
  ab_only <- compare(build(before), build(tp_dta(list(ds1 = tp_cols_yaml(ab = "Yes")))))
  qa_step("a change to a.b is one line", 1L, nrow(ab_only))
  qa_check(
    "about the column a.b, and not about the column a",
    grepl("column 'a.b'", ab_only$what, fixed = TRUE) && !grepl("column 'a'", ab_only$what, fixed = TRUE)
  )
  a_only <- compare(build(before), build(tp_dta(list(ds1 = tp_cols_yaml(a = "Yes")))))
  qa_step("a change to a is one line", 1L, nrow(a_only))
  qa_check(
    "about the column a, and not about the column a.b",
    grepl("column 'a'", a_only$what, fixed = TRUE) && !grepl("a.b", a_only$what, fixed = TRUE)
  )
  qa_step(
    "the diff keys them apart as well",
    c("ds1.columns.a.b.nullable", "ds1.columns.a.nullable"),
    c(
      diff(before, tp_dta(list(ds1 = tp_cols_yaml(ab = "Yes"))))$datasets$key,
      diff(before, tp_dta(list(ds1 = tp_cols_yaml(a = "Yes"))))$datasets$key
    )
  )

  # A new column of an existing dataset, and a new dataset.
  new_column <- compare(build(before), build(tp_dta(list(ds1 = tp_cols_yaml(
    extra = "      - { id: Visit Date (UTC), type: SAS Char, nullable: Yes }"
  )))))
  qa_step("a new column Visit Date (UTC) is one line, added", list(n = 1L, change = "added"), list(n = nrow(new_column), change = new_column$change))
  qa_check("named with the whole id", grepl("column 'Visit Date (UTC)'", new_column$what, fixed = TRUE))

  new_dataset <- compare(build(before), build(tp_dta(list(ds1 = tp_cols_yaml(), ds2 = tp_cols_yaml()))))
  qa_step(
    "a new dataset of three columns is one line, added, and not one line for it and one for each column",
    list(n = 1L, change = "added"), list(n = nrow(new_dataset), change = new_dataset$change)
  )
  qa_check("about ds2", grepl("ds2", new_dataset$what, fixed = TRUE))
})

# ---- REQ-TMPL-040: the guide ------------------------------------------------------------

tp_guide_cells <- function(file) {
  cells <- character(0)
  for (tb in qa_docx_tables(file)) {
    for (i in seq_len(nrow(tb))) cells <- c(cells, as.character(unlist(tb[i, ])))
  }
  cells
}

test_that("OQ-TMPL-129 | the guide lists a column with spaces or dots by its whole id, and names it whole in what is fixed and what differs | REQ-TMPL-040 REQ-TMPL-056", {
  ids <- c("Subject ID", "Wt. (kg)", "a", "a.b", "O'Brien", "Dose No.")
  columns <- function(ab_description, brien_description) {
    c(
      "      - {id: Subject ID, type: SAS Char, length: 8, nullable: No, description: Subject}",
      "      - {id: Wt. (kg), type: SAS Num, nullable: Yes, label: Weight, description: Body weight}",
      "      - {id: a, type: SAS Char, length: 3, nullable: Yes}",
      paste0("      - {id: a.b, type: SAS Char, length: 3, nullable: Yes, description: ", ab_description, "}"),
      paste0("      - {id: O'Brien, type: SAS Char, length: 3, nullable: Yes, description: ", brien_description, "}"),
      "      - {id: Dose No., type: SAS Char, length: 3, nullable: Yes}"
    )
  }
  root <- file.path(qa_tempdir(), "guidelib")
  tp_put(root, "templates/p.dta-template.yaml", tp_inline(
    "p", columns("old", "old"),
    header = c("sealed:", "  - \"datasets.inl.columns.Wt. (kg).label\"")
  ))
  tp_put(root, "templates/c.dta-template.yaml", tp_inline("c", columns("new", "new"), header = "extends: p@1.0"))
  qa_step("the library validates cleanly", 0L, nrow(suppressWarnings(validate_template(root))))

  out <- file.path(qa_tempdir(), "guideout")
  paths <- suppressMessages(write_template_guide(root, out, quiet = TRUE))
  cells <- tp_guide_cells(paths[["c"]])

  # The column table: each id is the first line of one cell, nothing split off it.
  first_lines <- sub("\n.*$", "", cells)
  qa_step(
    "every column id is the first line of a cell of the guide, whole",
    character(0), ids[!ids %in% first_lines]
  )

  # What differs from the parent: a.b and O'Brien changed their description.
  for (id in c("a.b", "O'Brien")) {
    qa_check(
      sprintf("the list of what differs names the column %s whole", id),
      any(startsWith(cells, "Column ") & endsWith(cells, paste0(id, " in inl")))
    )
  }

  # What the parent fixed: the label of Wt. (kg).
  qa_check(
    "the fixed-fields list names the label of Wt. (kg) with the whole id",
    any(startsWith(cells, "Label of column") & endsWith(cells, "Wt. (kg) in inl"))
  )

  # A seal that ends at the column (no field behind the id) names the whole
  # column, for ids with a space, a dot, a dotted pair and a trailing dot, and a
  # row label of a matrix the same way.
  whole <- c(
    "datasets.inl.columns.Wt. (kg)", "datasets.inl.columns.Subject.ID",
    "datasets.inl.columns.a.b", "datasets.inl.columns.Dose No.",
    "datasets.cnt.row_labels.GENE.ID"
  )
  # ... and a seal on a nested field of a column names the field path, as a
  # breadcrumb of the field names, ahead of the column.
  nested <- "datasets.inl.columns.Status.values_from.vocabulary"
  whole_lib <- file.path(qa_tempdir(), "wholelib")
  tp_put(whole_lib, "vocabulary/v.dta-vocabulary.yaml", tp_vocabulary)
  tp_put(whole_lib, "templates/p2.dta-template.yaml", c(
    "kind: dta_creation_template", "id: p2", "version: \"1.0\"",
    "sealed:", paste0("  - \"", c(whole, nested), "\""),
    "base: {metadata: {title: T}}",
    "datasets:", "  - id: inl", "    type: tabular", "    files: {filename: inl.csv, type: csv}", "    columns:",
    "      - {id: Status, type: SAS Char, length: 3, nullable: Yes, values_from: {vocabulary: \"v@1.0\"}}",
    "      - {id: Wt. (kg), type: SAS Num, nullable: Yes}",
    "      - {id: Subject.ID, type: SAS Char, length: 8, nullable: No}",
    "      - {id: a, type: SAS Char, length: 3, nullable: Yes}",
    "      - {id: a.b, type: SAS Char, length: 3, nullable: Yes}",
    "      - {id: Dose No., type: SAS Int, nullable: Yes}",
    "  - id: cnt", "    type: matrix", "    files: {filename: c.tsv, type: tsv}", "    row_labels:",
    "      - {id: GENE, type: SAS Char, nullable: No}",
    "      - {id: GENE.ID, type: SAS Char, nullable: No}",
    "    matrix:", "      cells: {type: SAS Num}"
  ))
  tp_put(whole_lib, "templates/c2.dta-template.yaml", c(
    "kind: dta_creation_template", "id: c2", "version: \"1.0\"", "extends: p2@1.0"
  ))
  qa_step("the library of whole-column seals validates cleanly", 0L, nrow(suppressWarnings(validate_template(whole_lib))))
  whole_paths <- suppressMessages(write_template_guide(whole_lib, file.path(qa_tempdir(), "wholeout"), quiet = TRUE))
  fixed <- Filter(
    function(tb) identical(as.character(tb[1, 1]), "Field") && identical(as.character(tb[1, 2]), "Rule"),
    qa_docx_tables(whole_paths[["c2"]])
  )
  qa_step("the guide has one fixed-fields table", 1L, length(fixed))
  fixed_names <- if (length(fixed) == 1L) as.character(fixed[[1]][-1, 1]) else character(0)
  qa_step(
    "each seal that ends at a column or row label is worded as that whole column or row label",
    character(0),
    setdiff(
      c(
        "Column Wt. (kg) in inl", "Column Subject.ID in inl", "Column a.b in inl",
        "Column Dose No. in inl", "Row label GENE.ID in cnt",
        "Values from › Vocabulary of column Status in inl"
      ),
      fixed_names
    )
  )
})

# ---- REQ-TMPL-030, REQ-TMPL-046: a library that uses such ids is clean ------------------

test_that("OQ-TMPL-130 | a scaffolded library that adds columns with spaces and dots, an option, a patch and a slot on them lints to zero rows | REQ-TMPL-030 REQ-TMPL-046 REQ-TMPL-056", {
  dir <- qa_tempdir()
  suppressMessages(create_template_repo(dir, ci = FALSE))
  tp_put(dir, "templates/spaced_ds.dta-dataset-template.yaml", tp_dataset_template(
    id = "spaced_ds",
    options = c(
      "options:",
      "  - {id: wt_label, label: Weight label, type: text, target: 'dataset.columns.Wt. (kg).label', default: Weight}"
    )
  ))
  tp_put(dir, "templates/spaced_study.dta-template.yaml", c(
    "kind: dta_creation_template", "id: spaced_study", "version: \"1.0\"", "base: {metadata: {title: T}}",
    "datasets:", "  - template: spaced_ds@1.0", "    as: inl", "    patch:",
    "      modify_columns:", "        - {id: Wt. (kg), description: Body weight}",
    "      set:", "        columns.a.b.label: AB label",
    "      add_columns:", "        - {id: Visit Date (UTC), type: SAS Char, length: 10, nullable: Yes}",
    "vocabulary_slots:",
    "  - {id: terms, label: Terms, target: 'datasets.inl.columns.a.b.values', vocabulary: starter_terms@1.0}"
  ))

  res <- validate_template(dir)
  qa_step("validate_template() reports no row", 0L, nrow(res))
  linted <- suppressMessages(lint_template_repo(dir, fail_on = "warning"))
  qa_step("lint_template_repo() reports no row and does not abort, even at fail_on = warning", 0L, nrow(linted))
})

# ---- REQ-TMPL-056: the reader of a seal and the writer of a set path agree ----------------

test_that("OQ-TMPL-131 | the reader of a seal and the writer of a set path name the same entry for a well-formed path that ends in a field of it | REQ-TMPL-056 | tags: white-box", {
  qapp_skip_unless_app_installed()
  read_path <- qapp_fn("dta_template_path_get")
  write_path <- qapp_fn("apply_dataset_template_path")

  # Every entry carries its own id in each field, so the value a path reaches
  # says which entry it reached: label = the id, description = "desc-<id>",
  # values = "vals-<id>", values_from$vocabulary = "voc-<id>".
  ids <- c("X", "X.values", "X.values_from", "X.description", "a", "a.b", "Wt", "Wt. (kg)", "Dose No.", "Subject ID")
  column <- function(id) {
    list(
      id = id, type = "SAS Char", label = id, description = paste0("desc-", id),
      values = paste0("vals-", id), values_from = list(vocabulary = paste0("voc-", id))
    )
  }
  columns <- lapply(ids, column)
  def <- list(datasets = list(list(id = "inl", columns = columns)))
  body <- list(id = "inl", type = "tabular", files = list(filename = "inl.csv", type = "csv"), columns = columns)
  expected_value <- function(id, field) {
    switch(field,
      label = id,
      description = paste0("desc-", id),
      values = paste0("vals-", id),
      `values_from.vocabulary` = paste0("voc-", id)
    )
  }
  # (what the path says after "columns.", the id it means, the field path)
  cases <- list(
    list("X.values", "X", "values"), list("X.values.label", "X.values", "label"),
    list("X.description", "X", "description"), list("X.description.label", "X.description", "label"),
    list("X.values_from.vocabulary", "X", "values_from.vocabulary"),
    list("X.values_from.values", "X.values_from", "values"),
    list("X.values_from.values_from.vocabulary", "X.values_from", "values_from.vocabulary"),
    list("a.label", "a", "label"), list("a.b.label", "a.b", "label"),
    list("Wt.label", "Wt", "label"), list("Wt. (kg).label", "Wt. (kg)", "label"),
    list("Dose No..label", "Dose No.", "label"), list("Subject ID.description", "Subject ID", "description")
  )
  for (cs in cases) {
    field_path <- strsplit(cs[[3]], ".", fixed = TRUE)[[1]]
    read_value <- read_path(def, paste0("datasets.inl.columns.", cs[[1]]))
    written <- write_path(body, paste0("dataset.columns.", cs[[1]]), "SET")
    changed <- vapply(seq_along(columns), function(i) !identical(written$columns[[i]], columns[[i]]), NA)
    qa_step(
      sprintf("columns.%s: the seal reader and the set writer both mean the column %s", cs[[1]], cs[[2]]),
      list(read = expected_value(cs[[2]], cs[[3]]), written = cs[[2]]),
      list(read = read_value, written = ids[changed])
    )
  }

  # Rules, with rules age.range, chk and chk.description: both read the one-segment
  # id first when it is declared (chk), and the whole dotted id otherwise
  # (age.range). The rule chk.description cannot be reached with a field.
  rules <- lapply(c("age.range", "chk", "chk.description"), function(id) {
    list(id = id, type = "col_unique", columns = "A", description = paste0("desc-", id))
  })
  rule_def <- list(datasets = list(list(id = "inl", rules = rules)))
  rule_body <- list(id = "inl", type = "tabular", columns = list(list(id = "A", type = "SAS Char")), rules = rules)
  for (cs in list(list("age.range.description", "age.range"), list("chk.description", "chk"))) {
    written <- write_path(rule_body, paste0("dataset.rules.", cs[[1]]), "SET")
    changed <- vapply(seq_along(rules), function(i) !identical(written$rules[[i]], rules[[i]]), NA)
    qa_step(
      sprintf("rules.%s: the seal reader and the set writer both mean the rule %s", cs[[1]], cs[[2]]),
      list(read = paste0("desc-", cs[[2]]), written = cs[[2]]),
      list(
        read = read_path(rule_def, paste0("datasets.inl.rules.", cs[[1]])),
        written = vapply(rules, function(r) r$id, "")[changed]
      )
    )
  }
})
