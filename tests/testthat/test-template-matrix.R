# T27: a matrix TEMPLATE. Every site that reads a dataset body's column specs
# must read `row_labels` for type matrix: validation, patches, vocabulary
# bindings and slots, the diff, the history labels and the guide.

mxt_body <- function(row_labels = NULL) {
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

mxt_template_dir <- function(row_label_lines, matrix_lines = NULL, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(dir, "templates"))
  writeLines(
    c(
      "kind: dta_dataset_template", "id: mx", "version: \"1.0\"",
      "dataset:", "  id: counts", "  type: matrix",
      "  files: {filename: c.tsv, type: tsv}",
      "  row_labels:", row_label_lines,
      matrix_lines %||% c(
        "  matrix:",
        "    header: { corner: [gene_id], pattern: '^S[0-9]+$' }",
        "    cells: { type: SAS Num }"
      )
    ),
    file.path(dir, "templates", "mx.dta-dataset-template.yaml")
  )
  dir
}

mxt_vocab <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  path <- file.path(dir, "genes.dta-vocabulary.yaml")
  writeLines(
    c(
      "kind: dta_vocabulary", "id: genes", "version: \"1.0\"", "label: Genes",
      "type: text", "terms:", "  - code: TP53", "  - code: BRCA1", "  - code: EGFR"
    ),
    path
  )
  res <- app_fn("read_vocabulary")(path)
  stopifnot(isTRUE(res$ok))
  function(ref) res$value
}

# ---- validate_template() ---------------------------------------------------------

test_that("validate_template() accepts a clean matrix dataset template", {
  dir <- mxt_template_dir("    - {id: GENE_ID, type: SAS Char, nullable: No}")
  expect_equal(nrow(validate_template(dir)), 0)
})

test_that("validate_template() reports a row-label problem and names the row label", {
  dir <- mxt_template_dir("    - {id: GENE_ID, type: SAS Char, pattern: '[unclosed'}")
  res <- validate_template(dir)
  hit <- res[res$code == "column_spec_invalid", , drop = FALSE]
  expect_equal(nrow(hit), 1L)
  expect_identical(hit$severity, "error")
  expect_match(hit$message, "GENE_ID", fixed = TRUE)
})

test_that("validate_template() lints the matrix: block of a template too", {
  dir <- mxt_template_dir(
    "    - {id: GENE_ID, type: SAS Char, nullable: No}",
    c("  matrix:", "    header: { corner: [gene_id], bogus: 1 }", "    cells: { type: SAS Num }")
  )
  res <- validate_template(dir)
  expect_true("matrix_field_unknown" %in% res$code)
})

# ---- patches and paths -------------------------------------------------------------

test_that("a set path dataset.row_labels.<id>.<field> addresses the row label by id", {
  out <- app_fn("apply_dataset_template_path")(mxt_body(), "dataset.row_labels.GENE_NAME.nullable", FALSE)
  expect_false(out$row_labels[[2]]$nullable)
  expect_null(out$columns)
})

test_that("a path rooted at the other segment aborts with a hint", {
  apply_path <- app_fn("apply_dataset_template_path")
  expect_error(apply_path(mxt_body(), "dataset.columns.GENE_ID.nullable", FALSE), regexp = "row_labels", class = "rlang_error")
  tab <- list(id = "t", type = "tabular", columns = list(list(id = "A", type = "SAS Char")))
  expect_error(apply_path(tab, "dataset.row_labels.A.nullable", FALSE), regexp = "columns", class = "rlang_error")
  # An unknown row-label id is still the id error.
  expect_error(apply_path(mxt_body(), "dataset.row_labels.NOPE.nullable", FALSE), regexp = "NOPE", class = "rlang_error")
})

test_that("patch operations keep their names and act on row_labels for a matrix", {
  patch <- app_fn("apply_dataset_patch")
  out <- patch(mxt_body(), list(
    modify_columns = list(list(id = "GENE_ID", pattern = "^ENSG")),
    add_columns = list(list(id = "BIOTYPE", type = "SAS Char")),
    remove_columns = "GENE_NAME",
    set = list("row_labels.GENE_ID.length" = 20L)
  ))
  ds <- out$dataset
  expect_identical(vapply(ds$row_labels, function(x) x$id, ""), c("GENE_ID", "BIOTYPE"))
  expect_identical(ds$row_labels[[1]]$pattern, "^ENSG")
  expect_identical(ds$row_labels[[1]]$length, 20L)
  expect_null(ds$columns)
  expect_identical(
    vapply(out$deviations, function(d) d$op, ""),
    c("remove_columns", "add_columns", "modify_columns", "set")
  )

  # The collection-verb form acts on the same list.
  out2 <- patch(mxt_body(), list(columns = list(remove = "GENE_NAME")))
  expect_identical(vapply(out2$dataset$row_labels, function(x) x$id, ""), "GENE_ID")
  expect_null(out2$dataset$columns)
})

test_that("a built matrix template becomes a matrix dataset with its row labels", {
  def <- list(
    kind = "dta_dataset_template", id = "mx", version = "1.0",
    dataset = mxt_body()
  )
  built <- app_fn("build_dataset_from_template")(def, patch = list(
    modify_columns = list(list(id = "GENE_ID", pattern = "^ENSG"))
  ))
  expect_true(built$ok, info = built$error)
  ds <- dta_dataset_from_list(built$value$dataset)
  expect_s3_class(ds, "DTAtools::DTADataSetMatrix")
  expect_identical(ds@specs@columns[["GENE_ID"]]@pattern, "^ENSG")
})

# ---- vocabulary bindings and slots --------------------------------------------------

test_that("values_from on a row label expands, and is detected as a binding", {
  body <- mxt_body(list(
    list(id = "GENE_ID", type = "SAS Char", values_from = "genes@1.0"),
    list(id = "GENE_NAME", type = "SAS Char")
  ))
  expect_true(app_fn("dataset_has_vocabulary_binding")(body))
  out <- app_fn("expand_column_vocabularies")(body, mxt_vocab())
  expect_identical(out$row_labels[[1]]$values, c("TP53", "BRCA1", "EGFR"))
  expect_null(out$row_labels[[1]]$values_from)
  expect_null(out$columns)
})

test_that("a slot target accepts row_labels and a slot override finds the row label", {
  parts <- app_fn("vocabulary_slot_target_parts")
  expect_identical(parts("datasets.counts.row_labels.GENE_ID.values")$segment, "row_labels")
  expect_identical(parts("datasets.counts.columns.A.values")$segment, "columns")
  expect_null(parts("datasets.counts.rows.GENE_ID.values"))

  apply_overrides <- app_fn("apply_vocabulary_slot_overrides")
  out <- apply_overrides(mxt_body(), list(
    list(dataset = "counts", column = "GENE_ID", values = c("TP53"), slot = "s1", segment = "row_labels")
  ))
  expect_identical(out$row_labels[[1]]$values, "TP53")

  # The wrong segment for the dataset is refused, not silently ignored.
  expect_error(
    apply_overrides(mxt_body(), list(
      list(dataset = "counts", column = "GENE_ID", values = "x", slot = "s1", segment = "columns")
    )),
    regexp = "row_labels", class = "rlang_error"
  )
})

# ---- diff, history, guide -----------------------------------------------------------

test_that("the diff keys row labels by id under row_labels", {
  path <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "datasets:", "  - id: counts", "    type: matrix",
    "    files: { filename: c.tsv, type: tsv }",
    "    row_labels:",
    "      - { id: GENE_ID, type: SAS Char, nullable: No }",
    "      - { id: GENE_NAME, type: SAS Char }",
    "    matrix:",
    "      header: { corner: [gene_id, gene_name] }",
    "      cells: { type: SAS Num }"
  ), path)
  dta <- suppressMessages(read_dta_from_yaml(path))
  leaves <- app_fn("dta_dataset_leaves")(dta)
  expect_true("counts.row_labels.GENE_ID.type" %in% names(leaves))
  expect_true("counts.row_labels.GENE_NAME.type" %in% names(leaves))
  expect_false(any(grepl("^counts[.]columns[.]", names(leaves))))
  expect_true("counts.matrix.cells.type" %in% names(leaves))
})

test_that("history labels a row-label leaf as a row label", {
  lab <- app_fn("template_history_label")
  expect_match(lab("Datasets", "counts.row_labels.GENE_ID.nullable"), "row label 'GENE_ID'", fixed = TRUE)
  expect_match(lab("Datasets", "counts.columns.A.nullable"), "column 'A'", fixed = TRUE)
})

test_that("the guide words a row-label slot target as allowed values of the row label", {
  expect_identical(
    DTAtools:::.tgc_path_text("datasets.counts.row_labels.GENE_ID.values"),
    "Allowed values of row label GENE_ID in counts"
  )
  expect_identical(
    DTAtools:::.tgc_path_text("dataset.columns.A.values"),
    "Allowed values of column A"
  )
})

test_that("the guide summary of a matrix dataset template counts row labels, not columns", {
  dir <- mxt_template_dir("    - {id: GENE_ID, type: SAS Char, nullable: No}")
  g <- DTAtools:::.template_guide_collect(dir)
  e <- g$datasets[[which(vapply(g$datasets, function(x) identical(x$id, "mx"), logical(1)))]]
  expect_match(e$summary, "with 1 row label and", fixed = TRUE)
  expect_false(grepl("columns", e$summary, fixed = TRUE))
})

test_that("an unknown id in a matrix patch is a row label error, a tabular one a column error", {
  patch <- app_fn("apply_dataset_patch")
  expect_error(patch(mxt_body(), list(remove_columns = "NOPE")), regexp = "Unknown row label id 'NOPE'", class = "rlang_error")
  expect_error(
    patch(mxt_body(), list(modify_columns = list(list(id = "NOPE", pattern = "x")))),
    regexp = "Unknown row label id 'NOPE'", class = "rlang_error"
  )
  tab <- list(id = "t", type = "tabular", columns = list(list(id = "A", type = "SAS Char")))
  expect_error(patch(tab, list(remove_columns = "NOPE")), regexp = "Unknown column id 'NOPE'", class = "rlang_error")
})

test_that("the diff treats a corner as one leaf: changing one entry is one row", {
  flat <- app_fn(".dta_leaf_flatten")
  a <- list(matrix = list(header = list(corner = structure(list(NULL, "gene_name"), class = "dta_verbatim"))))
  b <- list(matrix = list(header = list(corner = structure(list("gene_id", "gene_name"), class = "dta_verbatim"))))
  fa <- flat(a)
  fb <- flat(b)
  expect_named(fa, "matrix.header.corner")
  expect_named(fb, "matrix.header.corner")
  expect_false(identical(fa, fb))
})

test_that("validate_template() lints a matrix template's `columns:` key and non-CSV file type like lint_dta()", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "templates"))
  writeLines(
    c(
      "kind: dta_dataset_template", "id: mx", "version: \"1.0\"",
      "dataset:", "  id: counts", "  type: matrix",
      "  files: {filename: c.xpt, type: xpt}",
      "  columns:", "    - {id: A, type: SAS Char}",
      "  row_labels:", "    - {id: GENE_ID, type: SAS Char, nullable: No}",
      "  matrix:",
      "    header: { corner: [gene_id], pattern: '^S[0-9]+$' }",
      "    cells: { type: SAS Num }"
    ),
    file.path(dir, "templates", "mx.dta-dataset-template.yaml")
  )
  res <- validate_template(dir)
  body <- list(
    id = "counts", type = "matrix", files = list(filename = "c.xpt", type = "xpt"),
    columns = list(list(id = "A", type = "SAS Char")),
    row_labels = list(list(id = "GENE_ID", type = "SAS Char", nullable = FALSE)),
    matrix = list(header = list(corner = "gene_id", pattern = "^S[0-9]+$"), cells = list(type = "SAS Num"))
  )
  wanted <- c("matrix_columns_key", "matrix_file_type")
  expect_true(all(wanted %in% lint_dta(list(datasets = list(body)))$code))
  expect_true(all(wanted %in% res$code))
})
