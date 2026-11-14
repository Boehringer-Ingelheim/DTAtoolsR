# Templates for a matrix dataset. REQ-TMPL-048 .. REQ-TMPL-055.
#
# A matrix dataset keeps its row-label columns under `row_labels:` where a
# tabular one keeps `columns:`. A template reads a dataset's column specs in
# many places (patches, option and slot targets, vocabulary bindings, the diff,
# the history, the linter, the guide); a place that still read `columns:` would
# see a matrix as having none, and "nothing to do" looks exactly like success.
# Every expected value below is written from the requirement sentences, over
# fixtures built here, and never taken from a run.
#
# Cases that call the application's own helpers (patches, paths, slots,
# bindings, diff, history) are tagged white-box: the harness has no browser, and
# those helpers are what the template dialogs call.

tmx_put <- function(root, rel, lines) {
  dir.create(dirname(file.path(root, rel)), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, file.path(root, rel))
  invisible(file.path(root, rel))
}

# The body of a matrix dataset as the app holds it before it builds a dataset.
tmx_body <- function(row_labels = NULL) {
  list(
    id = "counts", type = "matrix",
    files = list(filename = "c.tsv", type = "tsv"),
    row_labels = row_labels %||% list(
      list(id = "GENE_ID", type = "SAS Char", nullable = FALSE),
      list(id = "GENE_NAME", type = "SAS Char", nullable = TRUE)
    ),
    matrix = list(
      header = list(corner = c("gene_id", "gene_name"), pattern = "^S[0-9]+$"),
      cells = list(type = "SAS Num")
    )
  )
}

# A dataset template of type matrix: `row_label_lines` fill row_labels, and
# `extra` any further lines of the dataset body (default: a clean matrix block).
tmx_dataset_template <- function(row_label_lines, matrix_lines = NULL, files = "{filename: c.tsv, type: tsv}",
                                 extra = NULL, options = NULL) {
  c(
    "kind: dta_dataset_template", "id: mx", "version: \"1.0\"",
    options,
    "dataset:", "  id: counts", "  type: matrix",
    paste0("  files: ", files),
    extra,
    "  row_labels:", row_label_lines,
    matrix_lines %||% c(
      "  matrix:",
      "    header: { corner: [gene_id, gene_name], pattern: '^S[0-9]+$' }",
      "    cells: { type: SAS Num }"
    )
  )
}

tmx_vocabulary <- function(root) {
  tmx_put(root, "vocabulary/genes.dta-vocabulary.yaml", c(
    "kind: dta_vocabulary", "id: genes", "version: \"1.0\"", "label: Genes",
    "type: text", "terms:", "  - code: TP53", "  - code: BRCA1", "  - code: EGFR"
  ))
}

# A library root holding one dataset template, validated.
tmx_validate <- function(lines, with_vocabulary = FALSE) {
  root <- file.path(qa_tempdir(), paste0("lib", sample.int(1e6, 1L)))
  tmx_put(root, "templates/mx.dta-dataset-template.yaml", lines)
  if (with_vocabulary) tmx_vocabulary(root)
  validate_template(root)
}

two_row_labels <- c(
  "    - {id: GENE_ID, type: SAS Char, nullable: No}",
  "    - {id: GENE_NAME, type: SAS Char, nullable: Yes}"
)

test_that("OQ-TMPL-116 | patch operations and set paths address a matrix dataset's row labels by id | REQ-TMPL-048 | tags: white-box", {
  qapp_skip_unless_app_installed()
  patch <- qapp_fn("apply_dataset_patch")
  set_path <- qapp_fn("apply_dataset_template_path")
  ids <- function(ds) vapply(ds$row_labels, function(x) x$id, "")

  out <- patch(tmx_body(), list(
    modify_columns = list(list(id = "GENE_ID", pattern = "^ENSG")),
    add_columns = list(list(id = "BIOTYPE", type = "SAS Char")),
    remove_columns = "GENE_NAME",
    set = list("row_labels.GENE_ID.length" = 20L)
  ))$dataset
  qa_step("adding BIOTYPE and removing GENE_NAME leaves these row labels, in this order", c("GENE_ID", "BIOTYPE"), ids(out))
  qa_step("modify_columns changed the pattern of the row label it names", "^ENSG", out$row_labels[[1]]$pattern)
  qa_step("a set path on the row label set its field", 20L, out$row_labels[[1]]$length)
  qa_check("and no columns key was created", is.null(out$columns))

  verbs <- patch(tmx_body(), list(columns = list(remove = "GENE_NAME")))$dataset
  qa_step("the collection-verb spelling of remove acts on the same list", "GENE_ID", ids(verbs))
  qa_check("and creates no columns key either", is.null(verbs$columns))

  by_path <- set_path(tmx_body(), "dataset.row_labels.GENE_NAME.nullable", FALSE)
  qa_step("a path dataset.row_labels.<id>.<field> sets that field of the row label it names", FALSE, by_path$row_labels[[2]]$nullable)
  qa_step("and leaves the other row label as it was", FALSE, tmx_body()$row_labels[[1]]$nullable)

  # Refusals are errors, never a silent no-op.
  qa_check(
    "a path rooted at columns on a matrix dataset is refused, pointing to row_labels",
    inherits(tryCatch(set_path(tmx_body(), "dataset.columns.GENE_ID.nullable", FALSE), error = function(e) e), "rlang_error") &&
      grepl("row_labels", conditionMessage(tryCatch(set_path(tmx_body(), "dataset.columns.GENE_ID.nullable", FALSE), error = function(e) e)), fixed = TRUE)
  )
  tabular <- list(id = "t", type = "tabular", columns = list(list(id = "A", type = "SAS Char")))
  qa_check(
    "a path rooted at row_labels on a tabular dataset is refused, pointing to columns",
    grepl("columns", conditionMessage(tryCatch(set_path(tabular, "dataset.row_labels.A.nullable", FALSE), error = function(e) e)), fixed = TRUE)
  )
  unknown <- tryCatch(set_path(tmx_body(), "dataset.row_labels.NOPE.nullable", FALSE), error = function(e) e)
  qa_check("an id that is not a row label is refused, naming the id", inherits(unknown, "rlang_error") && grepl("NOPE", conditionMessage(unknown), fixed = TRUE))

  # validate_template() judges a patch against the row labels too (REQ-TMPL-015).
  root <- file.path(qa_tempdir(), "patchlib")
  tmx_put(root, "templates/mx.dta-dataset-template.yaml", tmx_dataset_template(two_row_labels))
  creation <- function(id) {
    c(
      "kind: dta_creation_template", "id: c1", "version: \"1.0\"", "base: {}",
      "datasets:", "  - template: mx@1.0", "    as: counts", "    patch:", "      modify_columns:",
      paste0("        - { id: ", id, ", label: Renamed }")
    )
  }
  tmx_put(root, "templates/c1.dta-template.yaml", creation("GENE_NAME"))
  qa_step("a patch naming a row label the dataset template has is not reported", 0L, nrow(validate_template(root)))
  tmx_put(root, "templates/c1.dta-template.yaml", creation("NOSUCH"))
  res <- validate_template(root)
  hit <- res[res$code == "patch_incoherent", , drop = FALSE]
  qa_step("a patch naming an id that is not a row label is patch_incoherent, once", 1L, nrow(hit))
  qa_step("at severity warning", "warning", hit$severity)
  qa_check("naming the id", grepl("NOSUCH", hit$message, fixed = TRUE))
})

test_that("OQ-TMPL-117 | slot targets and option paths accept row_labels for a matrix dataset and refuse the wrong segment | REQ-TMPL-049 | tags: white-box", {
  qapp_skip_unless_app_installed()

  # The grammar of a vocabulary slot's target, checked as validate_template() does.
  slot_template <- function(target) {
    c(
      "kind: dta_creation_template", "id: c1", "version: \"1.0\"", "base: {}",
      "vocabulary_slots:",
      paste0("  - { id: s1, label: S, target: ", target, ", vocabulary: genes@1.0 }")
    )
  }
  validate_slot <- function(target) {
    root <- file.path(qa_tempdir(), paste0("lib", sample.int(1e6, 1L)))
    tmx_vocabulary(root)
    tmx_put(root, "templates/c1.dta-template.yaml", slot_template(target))
    validate_template(root)
  }
  ok <- validate_slot("datasets.counts.row_labels.GENE_ID.values")
  qa_step("a slot target datasets.<dataset>.row_labels.<id>.values is not malformed", FALSE, "vocab_slot_invalid" %in% ok$code)
  bad <- validate_slot("datasets.counts.rows.GENE_ID.values")
  qa_step("a target naming any other segment is vocab_slot_invalid, at severity error", "error", bad$severity[bad$code == "vocab_slot_invalid"])
  old <- validate_slot("datasets.counts.columns.GENE_ID.values")
  qa_step("and the columns spelling stays valid", FALSE, "vocab_slot_invalid" %in% old$code)

  # Applying a selection: the terms become the values of the row label named.
  apply_overrides <- qapp_fn("apply_vocabulary_slot_overrides")
  out <- apply_overrides(tmx_body(), list(
    list(dataset = "counts", column = "GENE_ID", values = "TP53", slot = "s1", segment = "row_labels")
  ))
  qa_step("a row_labels slot sets the values of the row label it names", "TP53", out$row_labels[[1]]$values)
  qa_check("and leaves the other row label without values", is.null(out$row_labels[[2]]$values))
  qa_check("and creates no columns key", is.null(out$columns))

  refused <- function(ds, segment) {
    cnd <- tryCatch(
      apply_overrides(ds, list(list(dataset = "counts", column = "GENE_ID", values = "x", slot = "s1", segment = segment))),
      error = function(e) e
    )
    inherits(cnd, "rlang_error")
  }
  qa_step("a columns slot on a matrix dataset is refused, not ignored", TRUE, refused(tmx_body(), "columns"))
  tabular <- list(id = "counts", type = "tabular", columns = list(list(id = "GENE_ID", type = "SAS Char")))
  qa_step("a row_labels slot on a tabular dataset is refused, not ignored", TRUE, refused(tabular, "row_labels"))

  # An option on a dataset template targets a path in the dataset body.
  built <- qapp_fn("apply_dataset_template_path")(tmx_body(), "dataset.row_labels.GENE_ID.nullable", TRUE)
  qa_step("an option path dataset.row_labels.<id>.<field> writes that field", TRUE, built$row_labels[[1]]$nullable)
})

test_that("OQ-TMPL-118 | values_from on a row label expands to the vocabulary's terms and is linted like a column's | REQ-TMPL-050 | tags: white-box", {
  qapp_skip_unless_app_installed()
  root <- file.path(qa_tempdir(), "vlib")
  tmx_vocabulary(root)
  vocab <- qapp_fn("read_vocabulary")(file.path(root, "vocabulary", "genes.dta-vocabulary.yaml"))
  qa_check("the vocabulary reads", isTRUE(vocab$ok))

  body <- tmx_body(list(
    list(id = "GENE_ID", type = "SAS Char", values_from = "genes@1.0"),
    list(id = "GENE_NAME", type = "SAS Char")
  ))
  qa_check("a row label with values_from makes the dataset count as vocabulary-bound", qapp_fn("dataset_has_vocabulary_binding")(body))
  expanded <- qapp_fn("expand_column_vocabularies")(body, function(ref) vocab$value)
  qa_step("the row label's values are the vocabulary's terms, in the vocabulary's order", c("TP53", "BRCA1", "EGFR"), expanded$row_labels[[1]]$values)
  qa_check("the binding key is gone after expansion", is.null(expanded$row_labels[[1]]$values_from))
  qa_check("the row label without a binding is left without values", is.null(expanded$row_labels[[2]]$values))
  qa_check("and no columns key is created", is.null(expanded$columns))

  # validate_template() reads the binding on a row label as it does on a column
  # (REQ-TMPL-017 and REQ-TMPL-018).
  mixed <- tmx_validate(tmx_dataset_template(c(
    "    - {id: GENE_ID, type: SAS Char, pattern: '^E', values_from: genes@1.0}",
    "    - {id: GENE_NAME, type: SAS Char, nullable: Yes}"
  )), with_vocabulary = TRUE)
  qa_step("values_from together with a pattern on a row label is values_from_pattern, error", "error", mixed$severity[mixed$code == "values_from_pattern"])
  missing <- tmx_validate(tmx_dataset_template(c(
    "    - {id: GENE_ID, type: SAS Char, values_from: nonexistent@1.0}",
    "    - {id: GENE_NAME, type: SAS Char, nullable: Yes}"
  )))
  qa_step("values_from naming an unresolved vocabulary on a row label is values_from_unresolved, warning", "warning", missing$severity[missing$code == "values_from_unresolved"])
})

# A DTA read from YAML lines, for the diff and history cases.
tmx_dta <- function(row_label_lines, corner = "[gene_id, gene_name]") {
  path <- file.path(qa_tempdir(), paste0("spec", sample.int(1e6, 1L), ".yaml"))
  writeLines(c(
    "metadata:", "  title: Counts", "  version: \"1.0\"", "datasets:", "  - id: counts", "    type: matrix",
    "    files: { filename: c.tsv, type: tsv }",
    "    row_labels:", row_label_lines,
    "    matrix:",
    paste0("      header: { corner: ", corner, " }"),
    "      cells: { type: SAS Num }"
  ), path)
  suppressMessages(read_dta_from_yaml(path))
}

rl_pair <- c(
  "      - { id: GENE_ID, type: SAS Char, nullable: No }",
  "      - { id: GENE_NAME, type: SAS Char, nullable: No }"
)

test_that("OQ-TMPL-119 | the template diff keys row labels by id and treats the corner as one value | REQ-TMPL-051 | tags: white-box", {
  qapp_skip_unless_app_installed()
  diff <- qapp_fn("dta_diff")
  before <- tmx_dta(rl_pair)

  # One row label's field changed, and one row label added.
  after <- tmx_dta(c(
    "      - { id: GENE_ID, type: SAS Char, nullable: No }",
    "      - { id: GENE_NAME, type: SAS Char, nullable: Yes }",
    "      - { id: BIOTYPE, type: SAS Char, nullable: Yes }"
  ), corner = "[gene_id, gene_name, biotype]")
  rows <- diff(before, after)$datasets
  changed <- rows[rows$change == "changed", "key"]
  qa_step(
    "a changed field of one row label is one change, keyed under row_labels by the id",
    c("counts.matrix.header.corner", "counts.row_labels.GENE_NAME.nullable"), sort(changed, method = "radix")
  )
  added <- rows$key[rows$change == "added"]
  qa_check("the added row label's fields are keyed by its id", length(added) > 0L && all(startsWith(added, "counts.row_labels.BIOTYPE.")))
  qa_check("a row label that did not change is not listed", !any(grepl("GENE_ID", rows$key, fixed = TRUE)))
  qa_check("no key sits under columns for a matrix dataset", !any(grepl("^counts[.]columns[.]", rows$key)))

  # The corner is one value: a corner whose two entries both differ is still one row.
  corner_only <- diff(before, tmx_dta(rl_pair, corner = "[~, description]"))$datasets
  qa_step("changing both corner entries is one change", "counts.matrix.header.corner", corner_only$key)
  qa_step("and it is a change, not an addition and a removal", "changed", corner_only$change)

  qa_step("two identical documents have no difference", 0L, nrow(diff(before, tmx_dta(rl_pair))$datasets))
})

test_that("OQ-TMPL-120 | the template history calls a matrix dataset's row labels row labels | REQ-TMPL-052 | tags: white-box", {
  qapp_skip_unless_app_installed()
  compare <- qapp_fn("template_history_compare")
  build <- function(dta) list(def = list(label = "Counts"), lineage = NULL, mixins = NULL, dta = dta)
  before <- build(tmx_dta(rl_pair))

  changed <- compare(before, build(tmx_dta(c(
    "      - { id: GENE_ID, type: SAS Char, nullable: No }",
    "      - { id: GENE_NAME, type: SAS Char, nullable: Yes }"
  ))))
  qa_step("one field of one row label changed: one line", 1L, nrow(changed))
  qa_check("it names a row label and its id, not a column", grepl("row label 'GENE_NAME'", changed$what, fixed = TRUE) && !grepl("column", changed$what, fixed = TRUE))
  qa_step("and reports a change", "changed", changed$change)

  added <- compare(before, build(tmx_dta(c(
    rl_pair, "      - { id: BIOTYPE, type: SAS Char, nullable: Yes }"
  ), corner = "[gene_id, gene_name, biotype]")))
  whole <- added[grepl("BIOTYPE", added$what, fixed = TRUE), , drop = FALSE]
  qa_step("a row label added is one line, as an added row label", 1L, nrow(whole))
  qa_check("worded as a row label", grepl("row label 'BIOTYPE'", whole$what, fixed = TRUE))
  qa_step("with the change added", "added", whole$change)

  # The wording of a tabular column is unchanged.
  tabular <- function(nullable) {
    path <- file.path(qa_tempdir(), paste0("tab", sample.int(1e6, 1L), ".yaml"))
    writeLines(c(
      "metadata:", "  title: T", "  version: \"1.0\"", "datasets:", "  - id: t", "    type: tabular", "    files: { filename: t.csv, type: csv }",
      "    columns:", paste0("      - { id: A, type: SAS Char, nullable: ", nullable, " }")
    ), path)
    suppressMessages(read_dta_from_yaml(path))
  }
  tab <- compare(build(tabular("No")), build(tabular("Yes")))
  qa_check("a changed tabular column is still worded as a column", nrow(tab) == 1L && grepl("column 'A'", tab$what, fixed = TRUE))
})

test_that("OQ-TMPL-121 | validate_template() judges a matrix dataset template's row labels and matrix block | REQ-TMPL-053", {
  clean <- tmx_validate(tmx_dataset_template(two_row_labels))
  qa_step("a clean matrix dataset template is not reported", 0L, nrow(clean))

  bad_label <- tmx_validate(tmx_dataset_template(c(
    "    - {id: GENE_ID, type: SAS Char, pattern: '[unclosed'}",
    "    - {id: GENE_NAME, type: SAS Char, nullable: Yes}"
  )))
  hit <- bad_label[bad_label$code == "column_spec_invalid", , drop = FALSE]
  qa_step("a row label that cannot be built is column_spec_invalid, once", 1L, nrow(hit))
  qa_step("at severity error", "error", hit$severity)
  qa_check("naming that row label and not the other", grepl("GENE_ID", hit$message, fixed = TRUE) && !grepl("GENE_NAME", hit$message, fixed = TRUE))

  unknown_key <- tmx_validate(tmx_dataset_template(two_row_labels, c(
    "  matrix:", "    header: { corner: [gene_id, gene_name], bogus: 1 }", "    cells: { type: SAS Num }"
  )))
  qa_step("a key the matrix block does not allow is matrix_field_unknown, error", "error", unknown_key$severity[unknown_key$code == "matrix_field_unknown"])

  missing_cells <- tmx_validate(tmx_dataset_template(two_row_labels, c(
    "  matrix:", "    header: { corner: [gene_id, gene_name] }"
  )))
  qa_step("a matrix block with no cells is matrix_field_missing, error", "error", missing_cells$severity[missing_cells$code == "matrix_field_missing"])

  bad_corner <- tmx_validate(tmx_dataset_template(two_row_labels, c(
    "  matrix:", "    header: { corner: [gene_id] }", "    cells: { type: SAS Num }"
  )))
  qa_step("a corner with one entry for two row labels is matrix_corner_invalid, error", "error", bad_corner$severity[bad_corner$code == "matrix_corner_invalid"])

  no_block <- tmx_validate(c(
    "kind: dta_dataset_template", "id: mx", "version: \"1.0\"",
    "dataset:", "  id: counts", "  type: matrix", "  files: {filename: c.tsv, type: tsv}",
    "  row_labels:", two_row_labels
  ))
  qa_step("a matrix dataset template with no matrix block is matrix_field_missing, error", "error", no_block$severity[no_block$code == "matrix_field_missing"])

  rule_on_data_column <- tmx_validate(tmx_dataset_template(two_row_labels, extra = c(
    "  rules:", "    - {id: r1, type: col_unique, columns: [S0001]}"
  )))
  qa_step("a rule naming a data column rather than a row label is rule_column_undeclared, error", "error", rule_on_data_column$severity[rule_on_data_column$code == "rule_column_undeclared"])
})

test_that("OQ-TMPL-122 | validate_template() reports a columns key and a non-CSV or TSV file on a matrix dataset template the way lint_dta() does | REQ-TMPL-054", {
  lines <- tmx_dataset_template(
    two_row_labels,
    files = "{filename: c.xpt, type: xpt}",
    extra = c("  columns:", "    - {id: A, type: SAS Char}")
  )
  res <- tmx_validate(lines)
  expected <- c("matrix_columns_key", "matrix_file_type")
  qa_step("both codes are reported", expected, intersect(expected, res$code))
  qa_step("each at severity error", c("error", "error"), res$severity[match(expected, res$code)])

  body <- list(
    id = "counts", type = "matrix", files = list(filename = "c.xpt", type = "xpt"),
    columns = list(list(id = "A", type = "SAS Char")),
    row_labels = list(list(id = "GENE_ID", type = "SAS Char", nullable = FALSE), list(id = "GENE_NAME", type = "SAS Char", nullable = TRUE)),
    matrix = list(header = list(corner = c("gene_id", "gene_name")), cells = list(type = "SAS Num"))
  )
  linted <- lint_dta(list(datasets = list(body)))
  qa_step("lint_dta() on the same dataset reports the same two codes at the same severity", c("error", "error"), linted$severity[match(expected, linted$code)])

  # The control: the same template with a CSV file and no columns key is clean.
  qa_step("with a TSV file and no columns key the template is clean", 0L, nrow(tmx_validate(tmx_dataset_template(two_row_labels))))
})

test_that("OQ-TMPL-123 | the template guide lists a matrix dataset's row labels | REQ-TMPL-055", {
  root <- file.path(qa_tempdir(), "guidelib")
  tmx_vocabulary(root)
  tmx_put(root, "templates/mx.dta-dataset-template.yaml", tmx_dataset_template(c(
    "    - {id: GENE_ID, type: SAS Char, nullable: No, values_from: genes@1.0}",
    "    - {id: GENE_NAME, type: SAS Char, nullable: Yes}"
  )))
  tmx_put(root, "templates/c1.dta-template.yaml", c(
    "kind: dta_creation_template", "id: c1", "version: \"1.0\"", "base: {}",
    "datasets:", "  - template: mx@1.0", "    as: counts"
  ))
  paths <- suppressMessages(suppressWarnings(write_template_guide(root, file.path(qa_tempdir(), "guides"), quiet = TRUE)))
  qa_step("one guide is written, for the creation template", "c1", names(paths))

  tables <- qa_docx_tables(paths[["c1"]])
  columns <- Filter(function(tb) nrow(tb) > 0 && identical(tb$col1[[1]], "Column"), tables)
  qa_step("the guide has one table of the dataset's columns", 1L, length(columns))
  tb <- columns[[1]]
  qa_step("with a row for each row label and none else", 2L, nrow(tb) - 1L)
  qa_step("the first listed by its id", "GENE_ID", tb$col1[[2]])
  qa_step("the second by its id", "GENE_NAME", tb$col1[[3]])
  qa_step("the types are those declared", c("SAS Char", "SAS Char"), tb$col2[2:3])
  qa_step("a row label that must have a value is marked required, one that may be blank is not", c("Yes", "No"), tb$col3[2:3])
  qa_step(
    "the terms of the vocabulary bound to a row label are its allowed values",
    c("TP53", "BRCA1", "EGFR"),
    strsplit(tb$col4[[2]], "\n", fixed = TRUE)[[1]]
  )
})
