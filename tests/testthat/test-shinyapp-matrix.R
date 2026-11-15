# The Shiny app and a matrix dataset: save/round trip, signature, bound data
# (heads) on transfer and unload, and the accessor for the package helper.

# The plan's genes x samples example, with a null-first corner (the header has
# no field above the first row label).
mx_app_yaml <- c(
  "datasets:",
  "  - id: gene_counts",
  "    type: matrix",
  "    files: { filename: \"gene_counts.tsv\", type: tsv }",
  "    row_labels:",
  "      - id: GENE_ID",
  "        type: SAS Char",
  "        nullable: No",
  "      - id: GENE_NAME",
  "        type: SAS Char",
  "        nullable: Yes",
  "    matrix:",
  "      header:",
  "        corner: [~, gene_name]",
  "        pattern: \"^S[0-9]{4}$\"",
  "        count: { min: 1, max: 384 }",
  "      rows:",
  "        key: GENE_ID",
  "        count: { min: 1 }",
  "      cells:",
  "        type: SAS Num",
  "        nullable: No",
  "        range: { min: 0 }",
  "        infinite: false"
)

mx_app_dta <- function(lines = mx_app_yaml) {
  path <- withr::local_tempfile(fileext = ".yaml", .local_envir = parent.frame())
  writeLines(lines, path)
  suppressMessages(read_dta_from_yaml(path))
}

# A dataset with the bundled fixture bound (a corner of two fields, so the
# null-first corner of the example is swapped for the fixture's own).
mx_app_bound <- function() {
  lines <- sub("corner: [~, gene_name]", "corner: [gene_id, gene_name]", mx_app_yaml, fixed = TRUE)
  dta <- mx_app_dta(lines)
  ds <- dta@datasets[["gene_counts"]]
  ds <- load_file(ds, file = test_path("fixtures", "matrix", "gene_counts.tsv"), stream = "never")
  dta@datasets[["gene_counts"]] <- ds
  dta
}

test_that("the app reaches the package's spec-key helper from its own environment", {
  key <- app_fn("dta_dataset_spec_key")
  expect_identical(key(list(type = "matrix")), "row_labels")
  expect_identical(key(list(type = "tabular")), "columns")
  expect_identical(key(list(row_labels = list())), "row_labels")
})

test_that("matrix is a supported type", {
  expect_true("matrix" %in% app_fn("dta_supported_types")())
})

test_that("T1: a matrix survives the app's save with row_labels, matrix and a null corner entry", {
  dta <- mx_app_dta()
  ds <- dta@datasets[["gene_counts"]]

  lst <- app_fn("dta_dataset_to_list")(ds)
  expect_null(lst$columns)
  expect_identical(vapply(lst$row_labels, function(x) x$id, ""), c("GENE_ID", "GENE_NAME"))
  expect_s3_class(lst$matrix$header$corner, "dta_verbatim")
  expect_identical(unclass(lst$matrix$header$corner), as.list(ds@matrix)$header$corner)

  txt <- app_fn("dta_to_yaml_text")(dta)
  expect_true(txt$ok)
  # The null entry is written as the YAML null, not dropped.
  expect_match(gsub("[[:space:]]+", " ", txt$value), "corner: - ~ - gene_name", fixed = TRUE)

  again <- mx_app_dta(strsplit(txt$value, "\n")[[1]])
  ds2 <- again@datasets[["gene_counts"]]
  expect_identical(as.list(ds2@matrix), as.list(ds@matrix))
  expect_identical(
    vapply(ds2@specs@columns, function(x) x@id, ""),
    vapply(ds@specs@columns, function(x) x@id, "")
  )
  # ... and a second save is a fixed point (the guard survives both compact passes).
  expect_identical(app_fn("dta_to_yaml_text")(again)$value, txt$value)

  # Rename the dataset, save, read it back: the matrix block and the row labels
  # come through unchanged.
  renamed <- app_fn("dta_set_dataset_meta")(dta, "gene_counts", id = "counts2")
  expect_true(renamed$ok, info = renamed$error)
  saved <- app_fn("dta_to_yaml_text")(renamed$value)
  expect_true(saved$ok)
  back <- mx_app_dta(strsplit(saved$value, "\n")[[1]])
  expect_named(back@datasets, "counts2")
  ds3 <- back@datasets[["counts2"]]
  expect_identical(as.list(ds3@matrix), as.list(ds@matrix))
  expect_identical(
    vapply(ds3@specs@columns, function(x) x@id, ""),
    vapply(ds@specs@columns, function(x) x@id, "")
  )
})

test_that("a tabular dataset is written unchanged: columns, no matrix key", {
  dta <- app_fixture_dta()
  lst <- app_fn("dta_dataset_to_list")(dta@datasets[[1]])
  expect_true(length(lst$columns) > 0)
  expect_null(lst$row_labels)
  expect_null(lst$matrix)
})

test_that(".dta_compact leaves a dta_verbatim list alone and still drops other NULLs", {
  compact <- app_fn(".dta_compact")
  v <- structure(list(NULL, "x"), class = "dta_verbatim")
  out <- compact(list(a = v, b = list(c = NULL, d = 1)))
  expect_length(out$a, 2L)
  expect_null(out$a[[1]])
  expect_identical(out$b, list(d = 1))
})

test_that("T15: the specs signature changes when the matrix block changes", {
  sig <- app_fn("dta_specs_signature")
  a <- mx_app_dta()@datasets[["gene_counts"]]
  b <- mx_app_dta(sub("range: { min: 0 }", "range: { min: 1 }", mx_app_yaml, fixed = TRUE))@datasets[["gene_counts"]]
  expect_identical(sig(a), sig(mx_app_dta()@datasets[["gene_counts"]]))
  expect_false(identical(sig(a), sig(b)))
})

# A matrix dataset with two file entries and one table bound under each, plus
# the second file written beside the first so both can be loaded.
mx_app_two_tables <- function(env = parent.frame()) {
  lines <- c(
    "datasets:", "  - id: gc", "    type: matrix", "    files:",
    "      - { filename: \"a.tsv\", type: tsv }",
    "      - { filename: \"b.tsv\", type: tsv }",
    "    row_labels:",
    "      - { id: GENE_ID, type: SAS Char, nullable: No }",
    "      - { id: GENE_NAME, type: SAS Char, nullable: Yes }",
    "    matrix:", "      header:", "        corner: [gene_id, gene_name]",
    "      rows: { key: GENE_ID }", "      cells: { type: SAS Num }"
  )
  dta <- mx_app_dta(lines)
  dir <- withr::local_tempdir(.local_envir = env)
  file.copy(test_path("fixtures", "matrix", "gene_counts.tsv"), file.path(dir, c("a.tsv", "b.tsv")))
  ds <- dta@datasets[["gc"]]
  ds <- load_file(ds, file = file.path(dir, "a.tsv"), handler_index = 1, stream = "never")
  ds <- load_file(ds, file = file.path(dir, "b.tsv"), handler_index = 2, stream = "never")
  dta@datasets[["gc"]] <- ds
  list(dta = dta, lines = lines)
}

test_that("T10: heads travel with their tables on transfer and are dropped on unload", {
  two <- mx_app_two_tables()
  dta <- two$dta
  old <- dta@datasets[["gc"]]
  expect_named(old@heads, c("a", "b"))

  # Unloading one table keeps the other's head.
  one <- app_fn("dta_unload_table")(dta, "gc", "a")
  expect_true(one$ok, info = one$error)
  expect_named(one$value@datasets[["gc"]]@tables, "b")
  expect_named(one$value@datasets[["gc"]]@heads, "b")

  # Unloading everything drops every head.
  all <- app_fn("dta_unload_all")(dta, "gc")
  expect_true(all$ok, info = all$error)
  expect_length(all$value@datasets[["gc"]]@heads, 0L)

  # A transfer carries the heads of the bound tables ...
  fresh <- mx_app_dta(two$lines)
  tr <- app_fn("dta_transfer_bound_data")(fresh, "gc", old, keep_validation = FALSE)
  expect_true(tr$ok, info = tr$error)
  expect_identical(tr$value@datasets[["gc"]]@heads, old@heads)

  # ... and a stale head, one with no table behind it, is not carried.
  stale <- old
  stale@heads[["gone"]] <- old@heads[["a"]]
  tr2 <- app_fn("dta_transfer_bound_data")(fresh, "gc", stale, keep_validation = FALSE)
  expect_true(tr2$ok, info = tr2$error)
  expect_named(tr2$value@datasets[["gc"]]@heads, c("a", "b"))
})

test_that("a matrix dataset in a loaded DTA validates, with a verdict and a message", {
  dta <- mx_app_bound()
  res <- check(dta, quiet = TRUE, persist = FALSE)
  expect_true(validation_status(res@datasets[["gene_counts"]])$ok)
  expect_s3_class(app_fn("dta_dataset_messages")(res, "gene_counts"), "data.frame")
  expect_true(app_fn("dta_to_yaml_text")(res)$ok)
  expect_true(app_fn("dta_dataset_to_yaml_text")(res, "gene_counts")$ok)

  # The failing counterpart: the verdict is a fail and the finding is worded.
  bad <- withr::local_tempdir()
  lines <- readLines(test_path("fixtures", "matrix", "gene_counts.tsv"))
  lines[2] <- sub("\t10.5\t", "\t-10.5\t", lines[2], fixed = TRUE)
  writeLines(lines, file.path(bad, "gene_counts.tsv"))
  dta2 <- mx_app_dta(sub("corner: [~, gene_name]", "corner: [gene_id, gene_name]", mx_app_yaml, fixed = TRUE))
  ds <- load_file(dta2@datasets[["gene_counts"]], file = file.path(bad, "gene_counts.tsv"), stream = "never")
  dta2@datasets[["gene_counts"]] <- ds
  res2 <- check(dta2, quiet = TRUE, persist = FALSE)
  expect_false(validation_status(res2@datasets[["gene_counts"]])$ok)
  msgs <- app_fn("dta_dataset_messages")(res2, "gene_counts")
  expect_gt(nrow(msgs), 0L)
  expect_true(any(grepl("-10.5", unlist(msgs), fixed = TRUE)) || any(grepl(">= 0", unlist(msgs), fixed = TRUE)))
})

test_that("the signature of a tabular dataset carries no matrix part", {
  tab <- app_fixture_dta()@datasets[[1]]
  sig <- app_fn("dta_specs_signature")(tab)
  expect_false(grepl("matrix", sig, fixed = TRUE))
  expect_identical(
    sig,
    yaml::as.yaml(list(
      columns = unname(lapply(tab@specs@columns, app_fn("dta_column_to_list"))),
      rules = unname(Filter(Negate(is.null), lapply(tab@specs@rules, app_fn("dta_rule_to_list"))))
    ))
  )
  expect_match(app_fn("dta_specs_signature")(mx_app_dta()@datasets[["gene_counts"]]), "matrix:", fixed = TRUE)
})

test_that("a matrix dataset is offered csv and tsv handlers only", {
  types <- app_fn("dta_handler_types")
  expect_identical(types("matrix"), c("csv", "tsv"))
  expect_identical(types("tabular"), c("csv", "tsv", "sas7bdat", "xpt"))
  expect_identical(types("file"), "any")
})

# ---- App behaviour through the server (raw YAML, inspect panel) ------------------

mxs_input <- function(path) {
  data.frame(
    name = basename(path), size = file.size(path), type = "",
    datapath = path, stringsAsFactors = FALSE
  )
}

mxs_ext <- function(f) system.file("extdata", f, package = "DTAtools")

# A copy of the gene counts with one negative cell, so the minimum rule fails.
mxs_negative_counts <- function(env = parent.frame()) {
  path <- file.path(withr::local_tempdir(.local_envir = env), "matrix_gene_counts.tsv")
  lines <- readLines(mxs_ext("matrix_gene_counts.tsv"))
  lines[2] <- sub("\t118\t", "\t-118\t", lines[2], fixed = FALSE)
  writeLines(lines, path)
  path
}

mxs_skip <- function() {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("shinyjs")
  unlink(list.files(tempdir(), pattern = "^dtatools_app_session.*[.]rds$", full.names = TRUE), force = TRUE)
}

test_that("a raw-YAML edit from matrix to tabular drops that dataset's data and leaves the other", {
  mxs_skip()
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = mxs_input(mxs_ext("matrix_dta.yaml")))
    session$setInputs(up_1_1 = mxs_input(mxs_ext("matrix_gene_counts.tsv")))
    session$setInputs(up_2_1 = mxs_input(mxs_ext("matrix_mature_counts.csv")))
    session$setInputs(check_all = 1)
    expect_true(all(c("gene_counts||1", "mature_counts||1") %in% names(rv$uploads)))
    expect_length(rv$dta@datasets$mature_counts@tables, 1L)
    expect_length(rv$dta@datasets$gene_counts@heads, 1L)
    gene_verdict <- unname(rv$status[["gene_counts"]])
    expect_identical(gene_verdict, "pass")

    unlock_editing(session)
    y <- rv$yaml_text
    i <- regexpr("- id: mature_counts", y, fixed = TRUE)
    tail_txt <- substring(y, i)
    tail_txt <- sub("type: matrix", "type: tabular", tail_txt, fixed = TRUE)
    tail_txt <- sub("row_labels:", "columns:", tail_txt, fixed = TRUE)
    tail_txt <- sub("\n  matrix:.*$", "\n", tail_txt)
    session$setInputs(raw_yaml_editor = paste0(substring(y, 1, i - 1), tail_txt))
    session$setInputs(apply_yaml = 1)
    expect_true(isTRUE(rv$yaml_msg$ok), info = rv$yaml_msg$error)

    expect_identical(unname(vapply(rv$structure, function(s) s$type, "")), c("matrix", "tabular"))
    # The tabular dataset keeps nothing: no upload record, no table, the no-data state.
    expect_false(any(startsWith(names(rv$uploads), "mature_counts||")))
    expect_length(rv$dta@datasets$mature_counts@tables, 0L)
    expect_identical(unname(rv$status[["mature_counts"]]), "nodata")
    # The matrix dataset keeps its table, its head and its verdict.
    expect_true("gene_counts||1" %in% names(rv$uploads))
    expect_length(rv$dta@datasets$gene_counts@tables, 1L)
    expect_length(rv$dta@datasets$gene_counts@heads, 1L)
    expect_identical(unname(rv$status[["gene_counts"]]), gene_verdict)
  })
})

test_that("a raw-YAML corner edit with a loaded table reports 'load it again', not a crash", {
  mxs_skip()
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = mxs_input(mxs_ext("matrix_dta.yaml")))
    session$setInputs(up_1_1 = mxs_input(mxs_ext("matrix_gene_counts.tsv")))
    session$setInputs(check_all = 1)
    expect_identical(unname(rv$status[["gene_counts"]]), "pass")

    unlock_editing(session)
    y <- rv$yaml_text
    edited <- sub("corner:\n      - gene_id\n      - gene_name", "corner:\n      - ~\n      - gene_name", y, fixed = TRUE)
    expect_false(identical(y, edited))
    session$setInputs(raw_yaml_editor = edited)
    session$setInputs(apply_yaml = 1)
    expect_true(isTRUE(rv$yaml_msg$ok), info = rv$yaml_msg$error)
    expect_length(rv$dta@datasets$gene_counts@tables, 1L)

    session$setInputs(check_all = 2)
    msgs <- app_fn("dta_dataset_messages")(rv$dta, "gene_counts")
    expect_true(any(grepl("load it again", msgs$message, fixed = TRUE)), info = paste(msgs$message, collapse = " | "))
    expect_identical(unname(rv$status[["gene_counts"]]), "fail")
  })
})

test_that("the inspect panel words a minimum finding as 'at least 0'", {
  mxs_skip()
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = mxs_input(mxs_ext("matrix_dta.yaml")))
    session$setInputs(up_1_1 = mxs_input(mxs_negative_counts()))
    session$setInputs(check_all = 1)
    msgs <- app_fn("dta_dataset_messages")(rv$dta, "gene_counts")
    id <- msgs$id[match("minimum", msgs$keyword)]
    expect_false(is.na(id))
    res <- app_fn("dta_inspect")(rv$dta, "gene_counts", id)
    expect_true(res$ok, info = res$error)
    expect_identical(columnspec_expected_text(res$value[1, , drop = FALSE]), "at least 0")
  })
})

# ==== Wave C: the app editors for matrix datasets (PLAN-matrix-shiny section 4) ====

# A synthetic matrix dataset "m" with row labels `labels` (the first is
# non-nullable), a files entry and the given `matrix:` lines (already indented
# by four spaces under "matrix:").
mxc_dta <- function(matrix_lines, labels = c("A", "B"), key = "A", env = parent.frame()) {
  lbl <- vapply(seq_along(labels), function(i) {
    sprintf("      - { id: %s, type: SAS Char, nullable: %s }", labels[i], if (labels[i] %in% key) "No" else "Yes")
  }, "")
  lines <- c(
    "datasets:", "  - id: m", "    type: matrix",
    "    files: { filename: \"m.tsv\", type: tsv }",
    "    row_labels:", lbl, "    matrix:", paste0("      ", matrix_lines)
  )
  path <- withr::local_tempfile(fileext = ".yaml", .local_envir = env)
  writeLines(lines, path)
  suppressMessages(read_dta_from_yaml(path))
}

mxc_int <- function(env = parent.frame()) {
  mxc_dta(c(
    "header:", "  corner: [~, x]", "  values: [\"007\", \"S1\"]", "  count: 2",
    "rows: { key: [A, B], count: 0 }",
    "cells: { type: SAS Int, nullable: No, values: [0, 1, 2], length: 4 }"
  ), key = c("A", "B"), env = env)
}
mxc_num <- function(env = parent.frame()) {
  mxc_dta(c(
    "header: { corner: [a, b] }", "rows: { key: A }",
    "cells: { type: SAS Num, nullable: No, infinite: true, format: \"SAS 8.2\" }"
  ), env = env)
}
mxc_char <- function(env = parent.frame()) {
  mxc_dta(c(
    "header: { corner: blank, count: 96 }", "rows: { key: A, count: 0 }",
    "cells: { type: SAS Char, nullable: No, length: 3 }"
  ), env = env)
}
mxc_bundled <- function() read_dta_from_yaml(app_fixture_path("matrix_dta.yaml"))
mxc_mx <- function(dta, id = "m") as.list(dta@datasets[[id]]@matrix)
mxc_ids <- function(dta, id = "m") names(dta@datasets[[id]]@specs@columns)

# ---- dta_add_dataset: matrix ------------------------------------------------

test_that("Add dataset: each corner form is stored and round-trips through the YAML", {
  add <- app_fn("dta_add_dataset")
  base <- app_fixture_dta()
  cases <- list(
    named = list(corner = "named", expect = "named"),
    blank = list(corner = "blank", expect = "blank"),
    none = list(corner = "none", expect = list(NULL)),
    text = list(corner = "text", corner_text = "gene_id", expect = "gene_id")
  )
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    r <- add(base, "mx",
      type = "matrix", row_label = "GENE_ID", corner = cs$corner,
      corner_text = cs$corner_text, cell_type = "SAS Int"
    )
    expect_true(r$ok, info = paste(nm, r$error))
    ds <- r$value@datasets[["mx"]]
    expect_true(inherits(ds, "DTAtools::DTADataSetMatrix"), info = nm)
    expect_identical(as.list(ds@matrix)$header$corner, cs$expect, info = nm)
    # The row label is the one SAS Char, non-nullable column, and the key.
    col <- app_fn("dta_column_to_list")(ds@specs@columns[["GENE_ID"]])
    expect_identical(names(ds@specs@columns), "GENE_ID")
    expect_match(col$type, "Char")
    expect_false(col$nullable)
    expect_identical(as.character(ds@matrix@row_key), "GENE_ID")
    # Cells: the declared type, nullable written explicitly as FALSE.
    cells <- as.list(ds@matrix)$cells
    expect_identical(cells$type, "SAS Int")
    expect_false(cells$nullable)
    # The corner survives the app's save (null entry kept) and a re-read.
    txt <- app_fn("dta_to_yaml_text")(r$value)
    expect_true(txt$ok, info = nm)
    again <- mx_app_dta(strsplit(txt$value, "\n", fixed = TRUE)[[1]])
    expect_identical(as.list(again@datasets[["mx"]]@matrix), as.list(ds@matrix), info = nm)
  }
})

test_that("Add dataset: a matrix is refused until it is fully described", {
  add <- app_fn("dta_add_dataset")
  base <- app_fixture_dta()
  full <- list(row_label = "G", corner = "blank", cell_type = "SAS Num")
  call <- function(...) {
    a <- utils::modifyList(full, list(...))
    do.call(add, c(list(base, "mx", type = "matrix"), a))
  }
  expect_true(call()$ok)
  expect_false(call(corner = NULL)$ok)
  expect_match(call(corner = NULL)$error, "Choose the header text", fixed = TRUE)
  expect_false(call(cell_type = NULL)$ok)
  expect_match(call(cell_type = NULL)$error, "Choose the cell type", fixed = TRUE)
  expect_false(call(row_label = "")$ok)
  expect_match(call(row_label = "  ")$error, "row-label column ID is required", fixed = TRUE)
  empty <- call(corner = "text", corner_text = "")
  expect_false(empty$ok)
  expect_match(empty$error, "choose Empty", fixed = TRUE)
  # The text "named" would be read back as the keyword, so it is refused.
  kw <- call(corner = "text", corner_text = "named")
  expect_false(kw$ok)
  expect_match(kw$error, "read as a keyword", fixed = TRUE)
  # ... and a refused add leaves the DTA as it was.
  expect_false("mx" %in% names(base@datasets))
})

test_that("Add dataset: the matrix arguments are ignored for any other type", {
  r <- app_fn("dta_add_dataset")(app_fixture_dta(), "tb", type = "tabular",
    row_label = "X", corner = "named", cell_type = "SAS Num")
  expect_true(r$ok, info = r$error)
  ds <- r$value@datasets[["tb"]]
  expect_identical(ds@type, "tabular")
  expect_false(inherits(ds, "DTAtools::DTADataSetMatrix"))
  expect_length(ds@specs@columns, 0L)
})

# ---- row-label invariants ---------------------------------------------------

test_that("renaming a row label renames the row key in place, keeping a composite key's order", {
  set_col <- app_fn("dta_set_column")
  dta <- mxc_int()
  r <- set_col(dta, "m", id = "C", old_id = "A", type = "Char", nullable = FALSE)
  expect_true(r$ok, info = r$error)
  expect_identical(as.character(r$value@datasets$m@matrix@row_key), c("C", "B"))
  expect_identical(mxc_ids(r$value), c("C", "B"))
  r2 <- set_col(dta, "m", id = "D", old_id = "B", type = "Char", nullable = FALSE)
  expect_true(r2$ok, info = r2$error)
  expect_identical(as.character(r2$value@datasets$m@matrix@row_key), c("A", "D"))

  # The bundled gene counts: the key follows the rename.
  g <- set_col(mxc_bundled(), "gene_counts",
    id = "GENE_KEY", old_id = "GENE_ID",
    type = "Char", nullable = FALSE
  )
  expect_true(g$ok, info = g$error)
  expect_identical(as.character(g$value@datasets$gene_counts@matrix@row_key), "GENE_KEY")
  expect_identical(as.list(g$value@datasets$gene_counts@matrix)$rows$key, "GENE_KEY")
})

test_that("adding a row label appends an empty header entry to a listed corner only", {
  set_col <- app_fn("dta_set_column")
  g <- set_col(mxc_bundled(), "gene_counts", id = "GENE_DESC", type = "Char", nullable = TRUE)
  expect_true(g$ok, info = g$error)
  m <- g$value@datasets$gene_counts@matrix
  expect_identical(as.character(m@corner), c("gene_id", "gene_name", ""))
  expect_identical(mxc_ids(g$value, "gene_counts"), c("GENE_ID", "GENE_NAME", "GENE_DESC"))
  # A named corner follows the ids by definition.
  d <- mxc_dta(c("header: { corner: named }", "rows: { key: A }", "cells: { type: SAS Num }"))
  n <- set_col(d, "m", id = "C", type = "Char", nullable = TRUE)
  expect_true(n$ok, info = n$error)
  expect_identical(as.list(n$value@datasets$m@matrix)$header$corner, "named")
})

test_that("removing the row key is refused and changes nothing; a non-key label drops its corner entry", {
  rm_col <- app_fn("dta_remove_column")
  dta <- mxc_bundled()
  before <- as.list(dta@datasets$gene_counts@matrix)
  ids <- mxc_ids(dta, "gene_counts")
  r <- rm_col(dta, "gene_counts", "GENE_ID")
  expect_false(r$ok)
  expect_match(r$error, "row key", fixed = TRUE)
  expect_null(r$value)
  expect_identical(as.list(dta@datasets$gene_counts@matrix), before)
  expect_identical(mxc_ids(dta, "gene_counts"), ids)

  ok <- rm_col(dta, "gene_counts", "GENE_NAME")
  expect_true(ok$ok, info = ok$error)
  expect_identical(mxc_ids(ok$value, "gene_counts"), "GENE_ID")
  expect_identical(as.character(ok$value@datasets$gene_counts@matrix@corner), "gene_id")

  # The last row label cannot go either (it would be the key anyway).
  one <- rm_col(ok$value, "gene_counts", "GENE_ID")
  expect_false(one$ok)
})

test_that("removing position 1 with a null corner entry is refused", {
  d <- mxc_dta(c("header: { corner: [~, b] }", "rows: { key: B }", "cells: { type: SAS Num }"), key = "B")
  r <- app_fn("dta_remove_column")(d, "m", "A")
  expect_false(r$ok)
  expect_match(r$error, "no header field above it", fixed = TRUE)
  expect_identical(mxc_ids(d), c("A", "B"))
})

test_that("the keyword trap: a corner left as the single text 'blank' is refused on remove", {
  d <- mxc_dta(c("header: { corner: [\"blank\", x] }", "rows: { key: A }", "cells: { type: SAS Num }"))
  expect_identical(as.character(d@datasets$m@matrix@corner), c("blank", "x"))
  r <- app_fn("dta_remove_column")(d, "m", "B")
  expect_false(r$ok)
  expect_match(r$error, "read as a keyword", fixed = TRUE)
})

test_that("moving a row label swaps its corner entry; a move never changes the key", {
  mv <- app_fn("dta_move_column")
  d <- mxc_dta(c("header: { corner: [a, b, c] }", "rows: { key: A }", "cells: { type: SAS Num }"),
    labels = c("A", "B", "C")
  )
  r <- mv(d, "m", "B", "up")
  expect_true(r$ok, info = r$error)
  expect_identical(mxc_ids(r$value), c("B", "A", "C"))
  expect_identical(as.character(r$value@datasets$m@matrix@corner), c("b", "a", "c"))
  expect_identical(as.character(r$value@datasets$m@matrix@row_key), "A")
  r2 <- mv(d, "m", "B", "down")
  expect_true(r2$ok, info = r2$error)
  expect_identical(as.character(r2$value@datasets$m@matrix@corner), c("a", "c", "b"))
  expect_identical(as.character(r2$value@datasets$m@matrix@row_key), "A")
})

test_that("a move involving the null corner entry is refused, [null, a, b] moving b up is legal", {
  mv <- app_fn("dta_move_column")
  d <- mxc_dta(c("header: { corner: [~, b] }", "rows: { key: A }", "cells: { type: SAS Num }"))
  up <- mv(d, "m", "B", "up")
  expect_false(up$ok)
  expect_match(up$error, "must stay first", fixed = TRUE)
  down <- mv(d, "m", "A", "down")
  expect_false(down$ok)

  d3 <- mxc_dta(c("header: { corner: [~, a, b] }", "rows: { key: A }", "cells: { type: SAS Num }"),
    labels = c("A", "B", "C")
  )
  r <- mv(d3, "m", "C", "up")
  expect_true(r$ok, info = r$error)
  expect_identical(mxc_ids(r$value), c("A", "C", "B"))
  expect_identical(as.character(r$value@datasets$m@matrix@corner), c(NA, "b", "a"))
})

test_that("a key row label cannot be made nullable", {
  r <- app_fn("dta_set_column")(mxc_int(), "m", id = "A", type = "Char", nullable = TRUE)
  expect_false(r$ok)
  expect_match(r$error, "cannot be nullable", fixed = TRUE)
})

test_that("the same helpers behave as before on a tabular dataset", {
  dta <- app_fixture_dta()
  ds <- names(dta@datasets)[1]
  ids <- app_fn("dta_column_ids")(dta, ds)
  a <- app_fn("dta_set_column")(dta, ds, id = "ZZNEW", type = "Char", nullable = TRUE)
  expect_true(a$ok, info = a$error)
  expect_identical(app_fn("dta_column_ids")(a$value, ds), c(ids, "ZZNEW"))
  m <- app_fn("dta_move_column")(a$value, ds, "ZZNEW", "up")
  expect_true(m$ok, info = m$error)
  expect_identical(app_fn("dta_column_ids")(m$value, ds), c(ids[-length(ids)], "ZZNEW", ids[length(ids)]))
  r <- app_fn("dta_remove_column")(m$value, ds, "ZZNEW")
  expect_true(r$ok, info = r$error)
  expect_identical(app_fn("dta_column_ids")(r$value, ds), ids)
})

# ---- rules ------------------------------------------------------------------

test_that("a rule id starting '.matrix:' is refused on a matrix, allowed on a tabular dataset", {
  set_rule <- app_fn("dta_set_rule")
  r <- set_rule(mxc_bundled(), "gene_counts", id = ".matrix:foo", type = "col_unique", columns = "GENE_ID")
  expect_false(r$ok)
  expect_match(r$error, ".matrix:", fixed = TRUE)
  # An ordinary id on a matrix row label is fine.
  ok <- set_rule(mxc_bundled(), "gene_counts", id = "uniq_name", type = "col_unique", columns = "GENE_NAME")
  expect_true(ok$ok, info = ok$error)

  dta <- app_fixture_dta()
  ds <- names(dta@datasets)[1]
  col1 <- app_fn("dta_column_ids")(dta, ds)[1]
  t <- set_rule(dta, ds, id = ".matrix:foo", type = "col_unique", columns = col1)
  expect_true(t$ok, info = t$error)
})

test_that("the rule column picker of a matrix lists the row-label ids", {
  expect_identical(app_fn("dta_column_ids")(mxc_bundled(), "gene_counts"), c("GENE_ID", "GENE_NAME"))
})

# ---- dta_matrix_fields / dta_set_matrix --------------------------------------

mxc_save <- function(dta, id, edit = list()) {
  f <- app_fn("dta_matrix_fields")(dta, id)
  for (n in names(edit)) f[n] <- list(edit[[n]])
  app_fn("dta_set_matrix")(dta, id, f)
}

test_that("an unchanged Matrix layout save leaves the settings identical", {
  bundled <- mxc_bundled()
  for (id in c("gene_counts", "mature_counts")) {
    r <- mxc_save(bundled, id)
    expect_true(r$ok, info = paste(id, r$error))
    expect_identical(as.list(r$value@datasets[[id]]@matrix), as.list(bundled@datasets[[id]]@matrix), info = id)
    expect_identical(r$dropped, character(0))
  }
  for (mk in list(mxc_int, mxc_num, mxc_char)) {
    d <- mk()
    r <- mxc_save(d, "m")
    expect_true(r$ok, info = r$error)
    expect_identical(as.list(r$value@datasets$m@matrix), as.list(d@datasets$m@matrix))
  }
  # The synthetic Int fixture really carries the awkward shapes the save must keep.
  l <- mxc_mx(mxc_int())
  expect_identical(unlist(l$header$values), c("007", "S1"))
  expect_identical(as.character(l$rows$key), c("A", "B"))
  expect_identical(as.numeric(unlist(l$cells$values)), c(0, 1, 2))
})

test_that("a cell nullable that was never set stays unset on an unchanged save", {
  d <- mxc_dta(c("header: { corner: blank }", "rows: { key: A }", "cells: { type: SAS Num }"))
  expect_null(mxc_mx(d)$cells$nullable)
  r <- mxc_save(d, "m")
  expect_true(r$ok, info = r$error)
  expect_null(mxc_mx(r$value)$cells$nullable)
  expect_identical(mxc_mx(r$value), mxc_mx(d))
})

test_that("Matrix layout edits land", {
  dta <- mxc_bundled()
  gc <- function(r) as.list(r$value@datasets$gene_counts@matrix)

  r <- mxc_save(dta, "gene_counts", list(header_pattern = "^T[0-9]+$"))
  expect_true(r$ok, info = r$error)
  expect_identical(gc(r)$header$pattern, "^T[0-9]+$")

  # min == max is written as the scalar count.
  r <- mxc_save(dta, "gene_counts", list(n_columns_min = 5, n_columns_max = 5))
  expect_true(r$ok, info = r$error)
  expect_false(is.list(gc(r)$header$count))
  expect_equal(gc(r)$header$count, 5)

  # A blank maximum removes the bound.
  r <- mxc_save(dta, "gene_counts", list(n_columns_max = NA_real_))
  expect_true(r$ok, info = r$error)
  expect_identical(r$value@datasets$gene_counts@matrix@n_columns[[2L]], Inf)
  expect_equal(r$value@datasets$gene_counts@matrix@n_columns[[1L]], 1)

  r <- mxc_save(dta, "gene_counts", list(n_rows_min = 3, n_rows_max = 9))
  expect_true(r$ok, info = r$error)
  expect_equal(unlist(gc(r)$rows$count), c(min = 3, max = 9))

  r <- mxc_save(dta, "gene_counts", list(range_min = 1, range_max = 50, infinite = TRUE))
  expect_true(r$ok, info = r$error)
  expect_equal(gc(r)$cells$range$min, 1)
  expect_equal(gc(r)$cells$range$max, 50)
  expect_true(gc(r)$cells$infinite)

  # The key changes by id; a composite key is written in the order kept.
  r <- mxc_save(mxc_int(), "m", list(row_key = "B"))
  expect_true(r$ok, info = r$error)
  expect_identical(as.character(mxc_mx(r$value)$rows$key), "B")

  # List corner with the "no field" flag makes the first entry null.
  r <- mxc_save(dta, "gene_counts", list(corner_none = TRUE))
  expect_true(r$ok, info = r$error)
  expect_identical(as.character(r$value@datasets$gene_counts@matrix@corner), c(NA, "gene_name"))
  r <- mxc_save(dta, "gene_counts", list(corner_mode = "blank"))
  expect_true(r$ok, info = r$error)
  expect_identical(gc(r)$header$corner, "blank")
})

test_that("Matrix layout edits the form cannot carry are refused and change nothing", {
  dta <- mxc_bundled()
  before <- as.list(dta@datasets$gene_counts@matrix)
  refused <- list(
    "pattern and values" = list(header_values = "S1\nS2"),
    "min above max" = list(n_rows_min = 10, n_rows_max = 3),
    "fractional count" = list(n_rows_min = 1.5),
    "zero columns" = list(n_columns_min = 0),
    "no key" = list(row_key = character(0)),
    "non-numeric value" = list(cell_values = "abc")
  )
  for (nm in names(refused)) {
    r <- mxc_save(dta, "gene_counts", refused[[nm]])
    expect_false(r$ok, info = nm)
    expect_null(r$value, info = nm)
    expect_true(nzchar(r$error), info = nm)
    expect_identical(as.list(dta@datasets$gene_counts@matrix), before, info = nm)
  }
  expect_match(mxc_save(dta, "gene_counts", refused[["pattern and values"]])$error, "not both", fixed = TRUE)
  expect_match(mxc_save(dta, "gene_counts", refused[["no key"]])$error, "at least one row key", fixed = TRUE)
  expect_match(mxc_save(dta, "gene_counts", refused[["non-numeric value"]])$error, "must be numbers", fixed = TRUE)
})

test_that("a cell type change drops what the new type cannot carry and reports only real settings", {
  dta <- mxc_bundled()
  # Num -> Char: the range goes and is reported; Inf was off, so it is not.
  r <- mxc_save(dta, "gene_counts", list(cell_type = "SAS Char"))
  expect_true(r$ok, info = r$error)
  expect_identical(r$dropped, "range")
  cells <- as.list(r$value@datasets$gene_counts@matrix)$cells
  expect_null(cells$range)
  expect_null(cells$infinite)
  # Num -> Int with Inf off: dropped silently, nothing reported.
  r <- mxc_save(dta, "gene_counts", list(cell_type = "SAS Int"))
  expect_true(r$ok, info = r$error)
  expect_identical(r$dropped, character(0))
  cells <- as.list(r$value@datasets$gene_counts@matrix)$cells
  expect_null(cells$infinite)
  expect_equal(cells$range$min, 0)
  # Num -> Int with Inf allowed is reported.
  num_inf <- mxc_dta(c("header: { corner: blank }", "rows: { key: A }", "cells: { type: SAS Num, nullable: No, infinite: true }"))
  inf <- mxc_save(num_inf, "m", list(cell_type = "SAS Int"))
  expect_true(inf$ok, info = inf$error)
  expect_identical(inf$dropped, "Inf allowed")
})

test_that("a format edit is saved with the SAS prefix", {
  r <- mxc_save(mxc_bundled(), "gene_counts", list(cell_format = "8.2"))
  expect_true(r$ok, info = r$error)
  expect_identical(as.list(r$value@datasets$gene_counts@matrix)$cells$format, "SAS 8.2")
  # The prefill shows it without the prefix.
  expect_identical(app_fn("dta_matrix_fields")(r$value, "gene_counts")$cell_format, "8.2")
})

test_that("a composite key given in another order than the prefill keeps the declared order", {
  d <- mxc_int()
  r <- mxc_save(d, "m", list(row_key = c("B", "A")))
  expect_true(r$ok, info = r$error)
  expect_identical(as.character(mxc_mx(r$value)$rows$key), c("A", "B"))
})

# ---- heads follow the file entries -------------------------------------------

mxc_head_handlers <- function(ds) vapply(ds@heads, function(h) as.integer(h[["handler"]] %||% NA_integer_), 1L)

test_that("removing or moving a file entry re-points the heads of a matrix", {
  dta <- mx_app_two_tables()$dta
  ds <- dta@datasets$gc
  expect_identical(unname(mxc_head_handlers(ds)[c("a", "b")]), c(1L, 2L))
  remap <- app_fn("dta_remap_file_handlers")
  idxmap <- app_fn("dta_handler_index_map")

  gone <- remap(ds, idxmap(2L, "remove", index = 1L))
  expect_identical(unname(mxc_head_handlers(gone)[c("a", "b")]), c(NA_integer_, 1L))

  swapped <- remap(ds, idxmap(2L, "move", index = 1L, direction = "down"))
  expect_identical(unname(mxc_head_handlers(swapped)[c("a", "b")]), c(2L, 1L))

  # Through the editor's own functions.
  rm <- app_fn("dta_remove_handler")(dta, "gc", 1L)
  expect_true(rm$ok, info = rm$error)
  expect_identical(unname(mxc_head_handlers(rm$value@datasets$gc)[["b"]]), 1L)
  mv <- app_fn("dta_move_handler")(dta, "gc", 1L, "down")
  expect_true(mv$ok, info = mv$error)
  expect_identical(unname(mxc_head_handlers(mv$value@datasets$gc)[c("a", "b")]), c(2L, 1L))
})

test_that("a raw-YAML reorder of the file entries re-points the heads on transfer", {
  two <- mx_app_two_tables()
  old <- two$dta@datasets$gc
  lines <- two$lines
  ia <- grep("a.tsv", lines, fixed = TRUE)
  ib <- grep("b.tsv", lines, fixed = TRUE)
  lines[c(ia, ib)] <- lines[c(ib, ia)]
  fresh <- mx_app_dta(lines)
  expect_identical(vapply(fresh@datasets$gc@files, function(h) h@filename, ""), c("b.tsv", "a.tsv"))
  tr <- app_fn("dta_transfer_bound_data")(fresh, "gc", old, keep_validation = FALSE)
  expect_true(tr$ok, info = tr$error)
  expect_identical(unname(mxc_head_handlers(tr$value@datasets$gc)[c("a", "b")]), c(2L, 1L))
})

test_that("a lazily bound table still checks after an earlier file entry is removed", {
  skip_if_not_installed("arrow")
  two <- mx_app_two_tables()
  dir <- withr::local_tempdir()
  file.copy(test_path("fixtures", "matrix", "gene_counts.tsv"), file.path(dir, "b.tsv"))
  dta <- mx_app_dta(two$lines)
  ds <- DTAtools::load_file(dta@datasets$gc, file = file.path(dir, "b.tsv"), handler_index = 2, stream = TRUE)
  dta@datasets$gc <- ds
  expect_named(ds@tables, "b")
  rm <- app_fn("dta_remove_handler")(dta, "gc", 1L)
  expect_true(rm$ok, info = rm$error)
  res <- DTAtools::check(rm$value, quiet = TRUE, persist = FALSE)
  msgs <- app_fn("dta_dataset_messages")(res, "gc")
  expect_false(any(grepl("differs from the one recorded", unlist(msgs), fixed = TRUE)),
    info = paste(msgs$message, collapse = " | ")
  )
  expect_true(validation_status(res@datasets$gc)$ok)
})

# ---- load failures: text and log ----------------------------------------------

test_that("a refused load names the delivered file, hides the staging path and breaks only at bullets", {
  d <- withr::local_tempdir()
  y <- c(
    "datasets:", "  - id: gc", "    type: matrix",
    "    files: { filename: \"x.tsv\", type: tsv }",
    "    row_labels:",
    "      - { id: GENE_ID, type: SAS Char, nullable: No }",
    "      - { id: GENE_NAME, type: SAS Char, nullable: Yes }",
    "    matrix:", "      header: { corner: [gene_id, gene_name] }",
    "      rows: { key: GENE_ID }", "      cells: { type: SAS Num }"
  )
  yf <- file.path(d, "s.yaml")
  writeLines(y, yf)
  dta <- suppressMessages(read_dta_from_yaml(yf))
  # The header lacks one field: the first data row is a field longer than it implies.
  writeLines(c("gene_id\tS0001\tS0002", "g1\tn\t1\t2"), file.path(d, "x.tsv"))
  r <- app_fn("dta_load_file")(dta, "gc", file.path(d, "x.tsv"), 1L, name = "x", display_name = "x.tsv")
  expect_false(r$ok)
  expect_true(grepl("x.tsv", r$error, fixed = TRUE))
  expect_false(grepl(gsub("\\", "/", d, fixed = TRUE), gsub("\\", "/", r$error, fixed = TRUE), fixed = TRUE))
  lines <- strsplit(r$error, "\n", fixed = TRUE)[[1]]
  # A sentence that ends at its full stop, then the hint(s) as bullets.
  expect_match(lines[1], "\\.$")
  expect_gte(length(lines), 2L)
  for (ln in lines[-1]) expect_match(ln, "^(ℹ|i|!|x|✖|•|\\*)")
})

test_that("the load-error log: add replaces, clear by dataset or table, rename re-keys, rows are 'load'", {
  e <- app_fn("dta_load_errors_empty")()
  add <- app_fn("dta_load_errors_add")
  e <- add(e, "d1", "t1", "t1.csv", "first")
  e <- add(e, "d1", "t2", "t2.csv", "other table")
  e <- add(e, "d2", "t1", "t1.csv", "other dataset")
  e <- add(e, "d1", "t1", "t1.csv", "second")
  expect_identical(nrow(e), 3L)
  expect_identical(e$message[e$dataset == "d1" & e$table == "t1"], "second")

  expect_identical(sort(app_fn("dta_load_errors_clear")(e, "d1", "t1")$table[1:2]), c("t1", "t2"))
  expect_identical(nrow(app_fn("dta_load_errors_clear")(e, "d1", "t1")), 2L)
  expect_identical(app_fn("dta_load_errors_clear")(e, "d1")$dataset, "d2")

  rn <- app_fn("dta_load_errors_rename")(e, "d1", "new")
  expect_identical(sort(unique(rn$dataset)), c("d2", "new"))
  expect_identical(nrow(app_fn("dta_load_errors_keep")(e, "d2")), 1L)

  rows <- app_fn("dta_load_error_rows")(e, "d1")
  expect_identical(nrow(rows), 2L)
  expect_true(all(rows$source == "load"))
  expect_identical(sort(rows$target), c("t1", "t2"))
  expect_true(all(is.na(rows$id)) && all(is.na(rows$row)) && all(is.na(rows$column)) && all(is.na(rows$rule_id)))
  expect_identical(nrow(app_fn("dta_load_error_rows")(e, "none")), 0L)
})

test_that("binding message rows tolerates empty inputs and fills missing columns", {
  bind <- app_fn("dta_bind_message_rows")
  load <- app_fn("dta_load_error_rows")(app_fn("dta_load_errors_empty")(), "d")
  expect_identical(nrow(bind(load, data.frame())), 0L)
  expect_identical(nrow(bind(data.frame(), data.frame())), 0L)
  b <- data.frame(id = 3L, dataset = "d", message = "m", extra = "x", stringsAsFactors = FALSE)
  le <- app_fn("dta_load_errors_add")(app_fn("dta_load_errors_empty")(), "d", "t", "t.csv", "failed")
  out <- bind(app_fn("dta_load_error_rows")(le, "d"), b)
  expect_identical(nrow(out), 2L)
  expect_identical(out$source[1], "load")
  expect_true(is.na(out$source[2]))
  expect_identical(out$extra[2], "x")
  expect_true(is.na(out$extra[1]))
  expect_identical(out$message, c("failed", "m"))
})

# ---- menus and notices ----------------------------------------------------------

test_that("the edit menu offers the editors each dataset type supports", {
  menu <- function(t) render_html(app_fn("ds_edit_menu")(t))
  mx <- menu("matrix")
  expect_match(mx, "Row labels", fixed = TRUE)
  expect_true(grepl("edit_cols", mx, fixed = TRUE))
  expect_true(grepl("edit_matrix", mx, fixed = TRUE))
  expect_true(grepl("edit_rules", mx, fixed = TRUE))
  file <- menu("file")
  expect_false(grepl("edit_cols", file, fixed = TRUE))
  expect_false(grepl("edit_rules", file, fixed = TRUE))
  expect_false(grepl("edit_matrix", file, fixed = TRUE))
  tab <- menu("tabular")
  expect_true(grepl("edit_cols", tab, fixed = TRUE) && grepl("edit_rules", tab, fixed = TRUE))
  expect_false(grepl("edit_matrix", tab, fixed = TRUE))
})

test_that("the page loads Shiny's selectize, so DT's older copy cannot shadow it later", {
  # The client keeps the first "selectize" dependency it sees. DT's column
  # filters ship 0.12, which lacks the a11y plugin every selectizeInput asks
  # for; if it wins, a picker rendered after the messages table fails to bind.
  env <- environment(shiny::shinyAppDir(app_server_dir())$serverFuncSource())
  page <- get("ui_page", envir = env)(get("dta_app_config", envir = env)())
  sel <- Filter(function(d) identical(d$name, "selectize"), htmltools::findDependencies(page))
  expect_gte(length(sel), 1L)
  expect_true(all(package_version(vapply(sel, function(d) d$version, "")) >= "0.15"))
})

test_that("a load-failure notice keeps its line breaks and is marked multi-line", {
  html <- render_html(app_fn("load_failure_notice")("a\nb"))
  expect_true(grepl("a\nb", html, fixed = TRUE))
  expect_true(grepl("notice-multiline", html, fixed = TRUE))
})

# ---- through the server (read the outputs: renderUI never runs unread) --------

# The tags of the `<input>`s named `name`, as strings.
mxc_inputs_named <- function(html, name) {
  m <- gregexpr(sprintf("<input[^>]*name=\"%s\"[^>]*>", name), html)
  regmatches(html, m)[[1]]
}

mxc_load_bundled <- function(session) {
  session$setInputs(dta_file = mxs_input(mxs_ext("matrix_dta.yaml")))
  unlock_editing(session)
}

test_that("the Add dialog offers Matrix with its three fields and preselects none of them", {
  mxs_skip()
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = mxs_input(mxs_ext("matrix_dta.yaml")))
    unlock_editing(session)
    session$setInputs(add_dataset_open = 1)
    html <- ui_text(output$add_ds_body)
    expect_true(grepl("value=\"matrix\"", html, fixed = TRUE))
    for (id in c("add_ds_mx_label", "add_ds_mx_corner", "add_ds_mx_cells")) {
      expect_true(grepl(id, html, fixed = TRUE), info = id)
    }
    for (grp in c("add_ds_mx_corner", "add_ds_mx_cells")) {
      tags <- mxc_inputs_named(html, grp)
      expect_gte(length(tags), 3L)
      expect_false(any(grepl("checked", tags)), info = grp)
    }
    expect_false(any(grepl("checked", mxc_inputs_named(html, "add_ds_mx_corner_text"))))
    label_tag <- regmatches(html, regexpr("<input[^>]*id=\"add_ds_mx_label\"[^>]*>", html))
    expect_true(grepl("value=\"\"", label_tag, fixed = TRUE))
    # The type radio keeps its own default: Tabular.
    type_tags <- mxc_inputs_named(html, "add_ds_type")
    expect_identical(sum(grepl("checked", type_tags)), 1L)
    expect_true(grepl("tabular", type_tags[grepl("checked", type_tags)], fixed = TRUE))
  })
})

test_that("adding a matrix through the dialog needs every field, then lands a matrix dataset", {
  mxs_skip()
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = mxs_input(mxs_ext("matrix_dta.yaml")))
    unlock_editing(session)
    session$setInputs(add_dataset_open = 1)
    before <- rv$dta
    # No corner chosen: refused, nothing changes, and the reason is on screen.
    session$setInputs(
      add_ds_id = "new_mx", add_ds_type = "matrix", add_ds_mx_label = "G",
      add_ds_mx_cells = "SAS Int", add_ds_save = 1
    )
    expect_identical(names(rv$dta@datasets), names(before@datasets))
    expect_false("new_mx" %in% names(rv$dta@datasets))
    expect_match(ui_text(output$add_ds_msg), "Choose the header text", fixed = TRUE)

    session$setInputs(add_ds_mx_corner = "blank", add_ds_save = 2)
    expect_true("new_mx" %in% names(rv$dta@datasets))
    expect_true(inherits(rv$dta@datasets$new_mx, "DTAtools::DTADataSetMatrix"))
    expect_identical(rv$structure[["new_mx"]]$type, "matrix")
    expect_identical(as.list(rv$dta@datasets$new_mx@matrix)$header$corner, "blank")
    expect_identical(as.list(rv$dta@datasets$new_mx@matrix)$cells$type, "SAS Int")
  })
})

test_that("a tabular dataset added after switching away from Matrix gets no matrix settings", {
  mxs_skip()
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = mxs_input(mxs_ext("matrix_dta.yaml")))
    unlock_editing(session)
    session$setInputs(add_dataset_open = 1)
    # The hidden matrix inputs still report values after the user switched back.
    session$setInputs(
      add_ds_id = "plain", add_ds_type = "tabular", add_ds_mx_label = "G",
      add_ds_mx_corner = "named", add_ds_mx_cells = "SAS Num", add_ds_save = 1
    )
    expect_identical(rv$dta@datasets$plain@type, "tabular")
    expect_false(inherits(rv$dta@datasets$plain, "DTAtools::DTADataSetMatrix"))
    expect_identical(rv$structure[["plain"]]$type, "tabular")
  })
})

test_that("the row-label editor of a matrix says 'row label', renames the key and lists the generated rule", {
  mxs_skip()
  shiny::testServer(.shiny_app_dir(), {
    mxc_load_bundled(session)
    expect_identical(rv$active, "gene_counts")
    session$setInputs(edit_cols = 1)
    expect_match(ui_text(output$col_modal_body), "row label", ignore.case = TRUE)

    session$setInputs(col_edit_click = 1)
    session$setInputs(
      col_id = "GENE_KEY", col_label = "", col_backend = "SAS", col_type = "Char",
      col_format = "", col_length = "", col_nullable = FALSE, col_values = "",
      col_pattern = "", col_desc = ""
    )
    session$setInputs(col_save = 1)
    expect_null(rv$col_msg)
    expect_identical(names(rv$dta@datasets$gene_counts@specs@columns), c("GENE_KEY", "GENE_NAME"))
    expect_match(rv$yaml_text, "key:\\s+GENE_KEY")
    expect_false(grepl("key:\\s+GENE_ID", rv$yaml_text))

    session$setInputs(edit_rules = 1)
    expect_match(ui_text(output$rule_modal_body), ".matrix:row_key", fixed = TRUE)
  })
})

test_that("the Matrix layout editor shows the prefill, saves a change and refuses min above max", {
  mxs_skip()
  shiny::testServer(.shiny_app_dir(), {
    mxc_load_bundled(session)
    session$setInputs(edit_matrix = 1)
    expect_match(ui_text(output$mx_modal_body), "^S[0-9]{4}$", fixed = TRUE)

    y0 <- rv$yaml_text
    session$setInputs(mx_row_key = "GENE_ID", mx_n_rows_min = 3, mx_save = 1)
    expect_null(rv$mx_msg)
    expect_false(identical(rv$yaml_text, y0))
    expect_equal(rv$dta@datasets$gene_counts@matrix@n_rows[[1L]], 3)

    y1 <- rv$yaml_text
    session$setInputs(edit_matrix = 2)
    session$setInputs(mx_row_key = "GENE_ID", mx_n_rows_min = 10, mx_n_rows_max = 3, mx_save = 2)
    expect_identical(rv$yaml_text, y1)
    expect_equal(rv$dta@datasets$gene_counts@matrix@n_rows[[1L]], 3)
    expect_match(ui_text(output$mx_msg_ui), "yaml-valid err", fixed = TRUE)
  })
})

# ---- the inspect panel -----------------------------------------------------------

# The bundled gene counts with a row minimum of 5, and a two-row file whose
# rows share one key.
mxc_two_row_inputs <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  y <- readLines(mxs_ext("matrix_dta.yaml"))
  y <- sub("count: { min: 1 }", "count: { min: 5 }", y, fixed = TRUE)
  expect_true(any(grepl("min: 5", y, fixed = TRUE)))
  writeLines(y, file.path(dir, "spec.yaml"))
  writeLines(c(
    "gene_id\tgene_name\tS0001\tS0002",
    "ENSG00000000003.14\tTSPAN6\t1\t2",
    "ENSG00000000003.14\tTSPAN6\t3\t4"
  ), file.path(dir, "matrix_gene_counts.tsv"))
  list(spec = mxs_input(file.path(dir, "spec.yaml")), data = mxs_input(file.path(dir, "matrix_gene_counts.tsv")))
}

test_that("the inspect panel states a row-count bound and the actual count, and words the key rule", {
  mxs_skip()
  inp <- mxc_two_row_inputs()
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = inp$spec)
    session$setInputs(up_1_1 = inp$data)
    session$setInputs(check_all = 1)
    msgs <- app_fn("dta_dataset_messages")(rv$dta, "gene_counts")
    min_id <- msgs$id[match("minItems", msgs$keyword)]
    key_id <- msgs$id[match(".matrix:row_key", msgs$rule_id)]
    expect_false(is.na(min_id))
    expect_false(is.na(key_id))

    body <- function(id) {
      res <- app_fn("dta_inspect")(rv$dta, "gene_counts", id)
      expect_true(res$ok, info = res$error)
      render_html(render_inspect_body(res$value, "gene_counts"))
    }
    cnt <- body(min_id)
    expect_match(cnt, "at least 5 data rows", fixed = TRUE)
    expect_match(cnt, "2 data rows", fixed = TRUE)

    key <- body(key_id)
    expect_match(key, app_fn("dta_rule_type_label")("col_unique"), fixed = TRUE)
    expect_match(key, "Generated from the matrix layout.", fixed = TRUE)
  })
})

# ---- load failures ---------------------------------------------------------------

test_that("a refused upload is logged, counted in the dock, listed with the messages, and cleared by a good one", {
  mxs_skip()
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = mxs_input(mxs_ext("matrix_dta.yaml")))
    expect_identical(nrow(rv$load_errors), 0L)
    # Same table name as the entry's file, but not a name the entry takes.
    bad <- mxs_input(mxs_ext("matrix_gene_counts.tsv"))
    bad$name <- "matrix_gene_counts.csv"
    session$setInputs(up_1_1 = bad)
    expect_identical(nrow(rv$load_errors), 1L)
    expect_identical(rv$load_errors$dataset, "gene_counts")
    expect_identical(rv$load_errors$table, "matrix_gene_counts")
    expect_length(rv$dta@datasets$gene_counts@tables, 0L)

    expect_match(ui_text(output$msgs_dock_meta), "msgs-dock-count\">1<", fixed = FALSE)
    m <- msgs_r()
    expect_identical(sum(m$source == "load", na.rm = TRUE), 1L)
    expect_identical(m$target[m$source %in% "load"], "matrix_gene_counts")
    expect_match(m$message[m$source %in% "load"], "matrix_gene_counts.csv", fixed = TRUE)

    session$setInputs(up_1_1 = mxs_input(mxs_ext("matrix_gene_counts.tsv")))
    expect_identical(nrow(rv$load_errors), 0L)
    expect_length(rv$dta@datasets$gene_counts@tables, 1L)
    expect_match(ui_text(output$msgs_dock_meta), "msgs-dock-count zero\">0<", fixed = FALSE)
  })
})

test_that("a file refused by name under another stem is cleared once the right file loads into that entry", {
  mxs_skip()
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = mxs_input(mxs_ext("matrix_dta.yaml")))
    bad <- mxs_input(mxs_ext("matrix_gene_counts.tsv"))
    bad$name <- "not_the_expected_name.tsv"
    session$setInputs(up_1_1 = bad)
    expect_identical(rv$load_errors$table, "not_the_expected_name")
    expect_identical(rv$load_errors$slot, 1L)
    session$setInputs(up_1_1 = mxs_input(mxs_ext("matrix_gene_counts.tsv")))
    expect_identical(nrow(rv$load_errors), 0L)
  })
})

test_that("clearing by entry drops only rows refused by name through that entry", {
  add <- app_fn("dta_load_errors_add")
  clear <- app_fn("dta_load_errors_clear")
  e <- add(app_fn("dta_load_errors_empty")(), "d", "wrong", "wrong.csv", "name", slot = 1L)
  e <- add(e, "d", "other", "other.csv", "name", slot = 2L)
  e <- add(e, "d", "x", "x.csv", "gate failure")
  out <- clear(e, "d", "y", slot = 1L)
  expect_identical(sort(out$table), c("other", "x"))
  expect_identical(clear(e, "d", "x", slot = 1L)$table, "other")
})

test_that("a gate failure upload is logged with a message that names the delivered file", {
  mxs_skip()
  dir <- withr::local_tempdir()
  # The header lacks the gene_name field above the second row label.
  writeLines(c("gene_id\tS0001\tS0002", "ENSG00000000003.14\tTSPAN6\t1\t2"), file.path(dir, "matrix_gene_counts.tsv"))
  bad <- mxs_input(file.path(dir, "matrix_gene_counts.tsv"))
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = mxs_input(mxs_ext("matrix_dta.yaml")))
    session$setInputs(up_1_1 = bad)
    expect_identical(nrow(rv$load_errors), 1L)
    msg <- rv$load_errors$message
    expect_true(grepl("matrix_gene_counts.tsv", msg, fixed = TRUE))
    expect_false(grepl(gsub("\\", "/", dir, fixed = TRUE), gsub("\\", "/", msg, fixed = TRUE), fixed = TRUE))
    expect_length(rv$dta@datasets$gene_counts@tables, 0L)
    expect_identical(sum(msgs_r()$source == "load", na.rm = TRUE), 1L)
  })
})

# ---- load-error log: slots, dismissal, notices ------------------------------------

test_that("a refusal in a single-file entry is cleared by any later load into it", {
  mxs_skip()
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = mxs_input(mxs_ext("matrix_dta.yaml")))
    empty <- mxs_input(mxs_ext("matrix_gene_counts.tsv"))
    empty$name <- "elsewhere.tsv"
    empty$size <- 0
    session$setInputs(up_1_1 = empty)
    expect_identical(rv$load_errors$table, "elsewhere")
    expect_identical(rv$load_errors$slot, 1L)
    session$setInputs(up_1_1 = mxs_input(mxs_ext("matrix_gene_counts.tsv")))
    expect_identical(nrow(rv$load_errors), 0L)
  })
})

test_that("a delivery outcome that loads clears the rows of its entry", {
  mxs_skip()
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = mxs_input(mxs_ext("matrix_dta.yaml")))
    rv$load_errors <- app_fn("dta_load_errors_add")(
      rv$load_errors, "gene_counts", "stale", "stale.tsv", "was refused",
      slot = 1L
    )
    plan <- data.frame(
      dataset = "gene_counts", item = "matrix_gene_counts", name = "matrix_gene_counts.tsv",
      status = "loaded", reason = NA_character_, handler = 1L, stringsAsFactors = FALSE
    )
    record_delivery_outcomes(plan)
    expect_identical(nrow(rv$load_errors), 0L)
  })
})

test_that("load-error rows follow a file entry that moves and go with one that is removed", {
  add <- app_fn("dta_load_errors_add")
  remap <- app_fn("dta_load_errors_remap")
  e <- add(app_fn("dta_load_errors_empty")(), "d", "a", "a.csv", "m", slot = 1L)
  e <- add(e, "d", "b", "b.csv", "m", slot = 2L)
  e <- add(e, "d", "c", "c.csv", "m", slot = 3L)
  e <- add(e, "d", "n", "n.csv", "m")
  e <- add(e, "other", "o", "o.csv", "m", slot = 2L)
  map <- app_fn("dta_handler_index_map")(3, "remove", index = 2)
  out <- remap(e, "d", map)
  expect_setequal(out$table, c("a", "c", "n", "o"))
  expect_identical(out$slot[out$table == "c"], 2L)
  expect_identical(out$slot[out$table == "o"], 2L)
  expect_true(is.na(out$slot[out$table == "n"]))
  mv <- remap(e, "d", app_fn("dta_handler_index_map")(3, "move", index = 1, direction = "down"))
  expect_identical(mv$slot[mv$table == "a"], 2L)
  expect_identical(mv$slot[mv$table == "b"], 1L)
})

test_that("a load row can be removed from the list from its own dialog", {
  mxs_skip()
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = mxs_input(mxs_ext("matrix_dta.yaml")))
    bad <- mxs_input(mxs_ext("matrix_gene_counts.tsv"))
    bad$name <- "matrix_gene_counts.csv"
    session$setInputs(up_1_1 = bad)
    expect_identical(nrow(rv$load_errors), 1L)
    session$setInputs(msgs_rows_selected = which(msgs_r()$source == "load"))
    session$setInputs(load_err_dismiss = 1)
    expect_identical(nrow(rv$load_errors), 0L)
  })
})

test_that("a load-failure notice is registered and goes once its problem is gone", {
  mxs_skip()
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = mxs_input(mxs_ext("matrix_dta.yaml")))
    bad <- mxs_input(mxs_ext("matrix_gene_counts.tsv"))
    bad$name <- "matrix_gene_counts.csv"
    session$setInputs(up_1_1 = bad)
    expect_identical(ls(load_notices), "load_fail_gene_counts")
    session$setInputs(up_1_1 = mxs_input(mxs_ext("matrix_gene_counts.tsv")))
    expect_identical(nrow(rv$load_errors), 0L)
    expect_length(ls(load_notices), 0L)
  })
})

# ---- cli wrapping and the Matrix layout's plain sentences -------------------------------

test_that("a package refusal from the Matrix layout has no line break inside a sentence", {
  d <- mxc_bundled()
  r <- mxc_save(d, "gene_counts", list(header_pattern = "("))
  expect_false(r$ok)
  expect_match(r$error, "not a valid regular expression", fixed = TRUE)
  expect_false(grepl("\n(?![*ix!>v] )", r$error, perl = TRUE))
})

test_that("the Matrix layout words a bad count or range in plain sentences", {
  d <- mxc_bundled()
  err <- function(edit) {
    r <- mxc_save(d, "gene_counts", edit)
    expect_false(r$ok)
    r$error
  }
  expect_identical(
    err(list(n_columns_min = 0)),
    "Data columns: the minimum must be a whole number of at least 1."
  )
  expect_identical(
    err(list(n_columns_min = 1.5)),
    "Data columns: the minimum must be a whole number of at least 1."
  )
  expect_identical(
    err(list(n_columns_min = 5, n_columns_max = 3)),
    "Data columns: the maximum must be a whole number not below the minimum."
  )
  expect_identical(
    err(list(n_rows_min = -1)),
    "Data rows: the minimum must be a whole number of at least 0."
  )
  expect_identical(
    err(list(n_rows_min = 3, n_rows_max = 2.5)),
    "Data rows: the maximum must be a whole number not below the minimum."
  )
  expect_identical(
    err(list(range_min = 5, range_max = 1)),
    "Cell range: the lowest allowed value must not exceed the highest."
  )
})

test_that("a row label that may be missing cannot join the row key", {
  d <- mxc_num() # B is nullable, A is the key
  r <- mxc_save(d, "m", list(row_key = c("A", "B")))
  expect_false(r$ok)
  expect_identical(
    r$error,
    "Row label 'B' may be missing, so it cannot be part of the row key; make it non-nullable under Row labels first."
  )
})

test_that("a number the form hands back rounded to 15 digits is not a change", {
  d <- mxc_dta(c(
    "header: { corner: blank }", "rows: { key: A }",
    "cells: { type: SAS Num, nullable: No, range: { min: 0.12345678901234567 } }"
  ))
  f <- app_fn("dta_matrix_fields")(d, "m")
  f$range_min <- signif(f$range_min, 15)
  r <- app_fn("dta_set_matrix")(d, "m", f)
  expect_true(r$ok, info = r$error)
  expect_identical(as.list(r$value@datasets$m@matrix), as.list(d@datasets$m@matrix))
})

test_that("a cell type change carries the unedited allowed values along", {
  int <- mxc_dta(c(
    "header: { corner: blank }", "rows: { key: A }",
    "cells: { type: SAS Int, nullable: No, values: [1, 2, 1000] }"
  ))
  r <- mxc_save(int, "m", list(cell_type = "SAS Char"))
  expect_true(r$ok, info = r$error)
  expect_identical(as.character(unlist(mxc_mx(r$value)$cells$values)), c("1", "2", "1000"))
  expect_true(is.character(unlist(mxc_mx(r$value)$cells$values)))

  chr <- mxc_dta(c(
    "header: { corner: blank }", "rows: { key: A }",
    "cells: { type: SAS Char, nullable: No, values: [\"0\", \"1.5\"] }"
  ))
  r <- mxc_save(chr, "m", list(cell_type = "SAS Num"))
  expect_true(r$ok, info = r$error)
  expect_true(is.numeric(unlist(mxc_mx(r$value)$cells$values)))
  expect_equal(as.numeric(unlist(mxc_mx(r$value)$cells$values)), c(0, 1.5))

  word <- mxc_dta(c(
    "header: { corner: blank }", "rows: { key: A }",
    "cells: { type: SAS Char, nullable: No, values: [\"a\"] }"
  ))
  r <- mxc_save(word, "m", list(cell_type = "SAS Num"))
  expect_false(r$ok)
  expect_identical(r$error, "Allowed cell values must be numbers for SAS Num and SAS Int cells.")
  expect_identical(mxc_mx(word)$cells$type, "SAS Char")
})

# ---- wording ----------------------------------------------------------------------

test_that("renaming a row label onto an existing one says 'row label'; a column keeps 'column'", {
  d <- mxc_dta(c("header: { corner: blank }", "rows: { key: A }", "cells: { type: SAS Num }"))
  r <- app_fn("dta_set_column")(d, "m", id = "B", type = "Char", old_id = "A")
  expect_false(r$ok)
  expect_identical(r$error, "A row label named 'B' already exists.")
})

test_that("the Matrix layout leaves blank numeric fields empty and the vocabulary view says 'row label'", {
  mxs_skip()
  shiny::testServer(.shiny_app_dir(), {
    mxc_load_bundled(session)
    session$setInputs(edit_matrix = 1)
    html <- ui_text(output$mx_modal_body)
    expect_false(grepl("value=\"NA\"", html, fixed = TRUE))
    expect_true(grepl("mx_n_columns_max", html, fixed = TRUE))
    session$setInputs(edit_cols = 1)
    rv$col_view <- "vocab"
    rv$col_token <- rv$col_token + 1
    session$flushReact()
    body <- ui_text(output$col_modal_body)
    expect_match(body, "Back to row label", fixed = TRUE)
    expect_match(body, "save the row label", fixed = TRUE)
    expect_false(grepl("this column", body, fixed = TRUE))
  })
})

test_that("a whole-matrix finding is headed '(whole matrix)', not '(column)'", {
  mxs_skip()
  inp <- mxc_two_row_inputs()
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = inp$spec)
    session$setInputs(up_1_1 = inp$data)
    session$setInputs(check_all = 1)
    msgs <- app_fn("dta_dataset_messages")(rv$dta, "gene_counts")
    id <- msgs$id[match("minItems", msgs$keyword)]
    res <- app_fn("dta_inspect")(rv$dta, "gene_counts", id)
    expect_true(res$ok, info = res$error)
    html <- render_html(render_inspect_body(res$value, "gene_counts"))
    expect_match(html, "(whole matrix)", fixed = TRUE)
    expect_false(grepl("(column)", html, fixed = TRUE))
  })
})
