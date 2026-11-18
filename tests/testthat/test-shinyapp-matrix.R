# The Shiny app and a matrix dataset: save/round trip, signature, bound data
# (heads) on transfer and unload, and the accessor for the package helper.

# The plan's genes x samples example, with `header: No` on the first row label
# (the header line has no field above it).
mx_app_yaml <- c(
  "datasets:",
  "  - id: gene_counts",
  "    type: matrix",
  "    files: { filename: \"gene_counts.tsv\", type: tsv }",
  "    row_labels:",
  "      - id: GENE_ID",
  "        header: No",
  "        type: SAS Char",
  "        nullable: No",
  "      - id: GENE_NAME",
  "        header: gene_name",
  "        type: SAS Char",
  "        nullable: Yes",
  "    matrix:",
  "      columns:",
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

# The example with a header text above both row labels, as the bundled fixture
# file has (`header: No` of the example is swapped for `header: gene_id`).
mx_app_fields_yaml <- function() {
  sub("header: No", "header: gene_id", mx_app_yaml, fixed = TRUE)
}

# A dataset with the bundled fixture bound.
mx_app_bound <- function() {
  dta <- mx_app_dta(mx_app_fields_yaml())
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

test_that("T1: a matrix survives the app's save with row_labels, matrix and `header: No`", {
  dta <- mx_app_dta()
  ds <- dta@datasets[["gene_counts"]]
  hdr <- app_fn("dta_row_label_header")
  header_of <- function(d) unname(lapply(d@specs@columns, hdr))

  lst <- app_fn("dta_dataset_to_list")(ds)
  expect_null(lst$columns)
  expect_identical(vapply(lst$row_labels, function(x) x$id, ""), c("GENE_ID", "GENE_NAME"))
  # `header` sits right after `id`; "No" is the logical FALSE, not a missing entry.
  expect_identical(names(lst$row_labels[[1]])[1:2], c("id", "header"))
  expect_identical(lst$row_labels[[1]]$header, FALSE)
  expect_identical(lst$row_labels[[2]]$header, "gene_name")
  expect_identical(lst$matrix, as.list(ds@matrix))
  expect_identical(names(lst$matrix), c("columns", "rows", "cells"))

  txt <- app_fn("dta_to_yaml_text")(dta)
  expect_true(txt$ok)

  again <- mx_app_dta(strsplit(txt$value, "\n")[[1]])
  ds2 <- again@datasets[["gene_counts"]]
  expect_identical(as.list(ds2@matrix), as.list(ds@matrix))
  expect_identical(
    vapply(ds2@specs@columns, function(x) x@id, ""),
    vapply(ds@specs@columns, function(x) x@id, "")
  )
  # `header: No` is still "no field" after save -> read, and a second save is a
  # fixed point: an unchanged save gives identical YAML.
  expect_identical(header_of(ds2), list(FALSE, "gene_name"))
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

test_that("every header form of a row label round-trips through save -> read as written", {
  hdr <- app_fn("dta_row_label_header")
  # A header text that used to be a keyword is now only a text.
  cases <- list(
    id = list(line = NULL, expect = NULL),
    empty = list(line = "header: \"\"", expect = ""),
    text = list(line = "header: gene_id", expect = "gene_id"),
    named = list(line = "header: named", expect = "named"),
    blank = list(line = "header: blank", expect = "blank"),
    none = list(line = "header: No", expect = FALSE)
  )
  for (nm in names(cases)) {
    y <- mx_app_yaml
    i <- grep("header: No", y, fixed = TRUE)
    y <- append(y[-i], if (!is.null(cases[[nm]]$line)) paste0("        ", cases[[nm]]$line), after = i - 1L)
    dta <- mx_app_dta(y)
    expect_identical(hdr(dta@datasets$gene_counts@specs@columns[[1]]), cases[[nm]]$expect, info = nm)
    txt <- app_fn("dta_to_yaml_text")(dta)
    expect_true(txt$ok, info = nm)
    back <- mx_app_dta(strsplit(txt$value, "\n", fixed = TRUE)[[1]])
    expect_identical(hdr(back@datasets$gene_counts@specs@columns[[1]]), cases[[nm]]$expect, info = nm)
    expect_identical(as.list(back@datasets$gene_counts@matrix), as.list(dta@datasets$gene_counts@matrix), info = nm)
    expect_identical(app_fn("dta_to_yaml_text")(back)$value, txt$value, info = nm)
  }
})

test_that(".dta_compact drops NULLs and empty lists everywhere, a header FALSE and \"\" survive", {
  compact <- app_fn(".dta_compact")
  out <- compact(list(b = list(c = NULL, d = 1), h = list(id = "A", header = FALSE), e = list(header = "")))
  expect_identical(out$b, list(d = 1))
  expect_identical(out$h$header, FALSE)
  expect_identical(out$e$header, "")
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
    "      - { id: GENE_ID, header: gene_id, type: SAS Char, nullable: No }",
    "      - { id: GENE_NAME, header: gene_name, type: SAS Char, nullable: Yes }",
    "    matrix:",
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
  dta2 <- mx_app_dta(mx_app_fields_yaml())
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

mxs_input <- function(path) app_upload_value(path)

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
    # A tabular column has no header text: drop the row label's.
    tail_txt <- sub("\n *header:[^\n]*", "", tail_txt)
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

test_that("a raw-YAML header edit with a loaded table reports 'load it again', not a crash", {
  mxs_skip()
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = mxs_input(mxs_ext("matrix_dta.yaml")))
    session$setInputs(up_1_1 = mxs_input(mxs_ext("matrix_gene_counts.tsv")))
    session$setInputs(check_all = 1)
    expect_identical(unname(rv$status[["gene_counts"]]), "pass")

    unlock_editing(session)
    y <- rv$yaml_text
    # The first row label's header text becomes "no field above it".
    edited <- sub("header: gene_id", "header: No", y, fixed = TRUE)
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
# by four spaces under "matrix:"). `headers` holds the YAML text of each row
# label's `header:` value (NA = not written, so the header is the ID).
mxc_dta <- function(matrix_lines, labels = c("A", "B"), key = "A", headers = NULL,
                    env = parent.frame()) {
  headers <- c(headers, rep(NA_character_, length(labels)))[seq_along(labels)]
  lbl <- vapply(seq_along(labels), function(i) {
    sprintf(
      "      - { id: %s, %stype: SAS Char, nullable: %s }", labels[i],
      if (is.na(headers[i])) "" else sprintf("header: %s, ", headers[i]),
      if (labels[i] %in% key) "No" else "Yes"
    )
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
    "columns:", "  values: [\"007\", \"S1\"]", "  count: 2",
    "rows: { key: [A, B], count: 0 }",
    "cells: { type: SAS Int, nullable: No, values: [0, 1, 2], length: 4 }"
  ), key = c("A", "B"), headers = c("No", "x"), env = env)
}
mxc_num <- function(env = parent.frame()) {
  mxc_dta(c(
    "rows: { key: A }",
    "cells: { type: SAS Num, nullable: No, infinite: true, format: \"SAS 8.2\" }"
  ), headers = c("a", "b"), env = env)
}
mxc_char <- function(env = parent.frame()) {
  mxc_dta(c(
    "columns: { count: 96 }", "rows: { key: A, count: 0 }",
    "cells: { type: SAS Char, nullable: No, length: 3 }"
  ), headers = c("\"\"", "\"\""), env = env)
}
mxc_bundled <- function() read_dta_from_yaml(app_fixture_path("matrix_dta.yaml"))
mxc_mx <- function(dta, id = "m") as.list(dta@datasets[[id]]@matrix)
mxc_ids <- function(dta, id = "m") names(dta@datasets[[id]]@specs@columns)

# ---- dta_add_dataset: matrix ------------------------------------------------

test_that("Add dataset: each header mode is stored on the row label and round-trips through the YAML", {
  add <- app_fn("dta_add_dataset")
  hdr <- app_fn("dta_row_label_header")
  base <- app_fixture_dta()
  cases <- list(
    id = list(mode = "id", expect = NULL),
    empty = list(mode = "empty", expect = ""),
    text = list(mode = "text", text = "gene_id", expect = "gene_id"),
    none = list(mode = "none", expect = FALSE),
    # A text that used to be a keyword is only a text now.
    named = list(mode = "text", text = "named", expect = "named"),
    blank = list(mode = "text", text = "blank", expect = "blank")
  )
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    r <- add(base, "mx",
      type = "matrix", row_label = "GENE_ID", header_mode = cs$mode,
      header_text = cs$text, cell_type = "SAS Int"
    )
    expect_true(r$ok, info = paste(nm, r$error))
    ds <- r$value@datasets[["mx"]]
    expect_true(inherits(ds, "DTAtools::DTADataSetMatrix"), info = nm)
    expect_identical(hdr(ds@specs@columns[["GENE_ID"]]), cs$expect, info = nm)
    # The row label is the one SAS Char, non-nullable column, and the key.
    col <- app_fn("dta_column_to_list")(ds@specs@columns[["GENE_ID"]])
    expect_identical(names(ds@specs@columns), "GENE_ID")
    expect_match(col$type, "Char")
    expect_false(col$nullable)
    expect_identical(as.character(ds@matrix@row_key), "GENE_ID")
    # No data-column rule is set: only the default count stands.
    expect_null(as.list(ds@matrix)$columns$pattern)
    cells <- as.list(ds@matrix)$cells
    expect_identical(cells$type, "SAS Int")
    expect_false(cells$nullable)
    # The header survives the app's save and a re-read.
    txt <- app_fn("dta_to_yaml_text")(r$value)
    expect_true(txt$ok, info = nm)
    again <- mx_app_dta(strsplit(txt$value, "\n", fixed = TRUE)[[1]])
    expect_identical(hdr(again@datasets[["mx"]]@specs@columns[["GENE_ID"]]), cs$expect, info = nm)
    expect_identical(as.list(again@datasets[["mx"]]@matrix), as.list(ds@matrix), info = nm)
  }
})

test_that("Add dataset: a matrix is refused until it is fully described", {
  add <- app_fn("dta_add_dataset")
  base <- app_fixture_dta()
  full <- list(row_label = "G", header_mode = "empty", cell_type = "SAS Num")
  call <- function(...) {
    a <- utils::modifyList(full, list(...))
    do.call(add, c(list(base, "mx", type = "matrix"), a))
  }
  expect_true(call()$ok)
  expect_false(call(header_mode = NULL)$ok)
  expect_match(call(header_mode = NULL)$error, "Choose the header text", fixed = TRUE)
  expect_false(call(cell_type = NULL)$ok)
  expect_match(call(cell_type = NULL)$error, "Choose the cell type", fixed = TRUE)
  expect_false(call(row_label = "")$ok)
  expect_match(call(row_label = "  ")$error, "row-label column ID is required", fixed = TRUE)
  empty <- call(header_mode = "text", header_text = "")
  expect_false(empty$ok)
  expect_match(empty$error, "choose Empty", fixed = TRUE)
  blank <- call(header_mode = "text", header_text = "   ")
  expect_false(blank$ok)
  expect_match(blank$error, "Enter the header text", fixed = TRUE)
  # ... and a refused add leaves the DTA as it was.
  expect_false("mx" %in% names(base@datasets))
})

test_that("Add dataset: the matrix arguments are ignored for any other type", {
  r <- app_fn("dta_add_dataset")(app_fixture_dta(), "tb", type = "tabular",
    row_label = "X", header_mode = "id", cell_type = "SAS Num")
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

test_that("each row label keeps its own header when labels are added, removed or reordered", {
  set_col <- app_fn("dta_set_column")
  hdr <- app_fn("dta_row_label_header")
  hdrs <- function(dta, id = "m") unname(lapply(dta@datasets[[id]]@specs@columns, hdr))

  # Add: the new label's header is its ID (nothing written), the others stay.
  g <- set_col(mxc_bundled(), "gene_counts", id = "GENE_DESC", type = "Char", nullable = TRUE)
  expect_true(g$ok, info = g$error)
  expect_identical(mxc_ids(g$value, "gene_counts"), c("GENE_ID", "GENE_NAME", "GENE_DESC"))
  expect_identical(hdrs(g$value, "gene_counts"), list("gene_id", "gene_name", NULL))
  # ... and in the saved YAML: header lines for the first two only.
  saved <- app_fn("dta_dataset_to_list")(g$value@datasets$gene_counts)$row_labels
  expect_identical(lapply(saved, function(x) x$header), list("gene_id", "gene_name", NULL))
  # Adding with an explicit mode writes it.
  e <- set_col(mxc_bundled(), "gene_counts",
    id = "GENE_DESC", type = "Char", nullable = TRUE,
    header_mode = "empty"
  )
  expect_true(e$ok, info = e$error)
  expect_identical(hdrs(e$value, "gene_counts"), list("gene_id", "gene_name", ""))

  # Remove: the remaining labels keep theirs.
  d <- mxc_dta(c("rows: { key: A }", "cells: { type: SAS Num }"),
    labels = c("A", "B", "C"), headers = c("No", "b", "\"\"")
  )
  expect_identical(hdrs(d), list(FALSE, "b", ""))
  rm_col <- app_fn("dta_remove_column")
  r <- rm_col(d, "m", "B")
  expect_true(r$ok, info = r$error)
  expect_identical(hdrs(r$value), list(FALSE, ""))
  r2 <- rm_col(d, "m", "C")
  expect_true(r2$ok, info = r2$error)
  expect_identical(hdrs(r2$value), list(FALSE, "b"))

  # Reorder: each header travels with its row label; the key does not change.
  mv <- app_fn("dta_move_column")
  d3 <- mxc_dta(c("rows: { key: A }", "cells: { type: SAS Num }"),
    labels = c("A", "B", "C"), headers = c("a", "b", "c")
  )
  up <- mv(d3, "m", "B", "up")
  expect_true(up$ok, info = up$error)
  expect_identical(mxc_ids(up$value), c("B", "A", "C"))
  expect_identical(hdrs(up$value), list("b", "a", "c"))
  expect_identical(as.character(up$value@datasets$m@matrix@row_key), "A")
  down <- mv(d3, "m", "B", "down")
  expect_true(down$ok, info = down$error)
  expect_identical(mxc_ids(down$value), c("A", "C", "B"))
  expect_identical(hdrs(down$value), list("a", "c", "b"))
  # The saved YAML follows (order of ids and their header lines).
  y <- app_fn("dta_dataset_to_list")(down$value@datasets$m)$row_labels
  expect_identical(vapply(y, function(x) x$id, ""), c("A", "C", "B"))
  expect_identical(vapply(y, function(x) x$header, ""), c("a", "c", "b"))
})

test_that("removing the row key is refused and changes nothing; a non-key label goes with its own header", {
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
  expect_identical(
    app_fn("dta_row_label_header")(ok$value@datasets$gene_counts@specs@columns[["GENE_ID"]]),
    "gene_id"
  )

  # The last row label cannot go either (it would be the key anyway).
  one <- rm_col(ok$value, "gene_counts", "GENE_ID")
  expect_false(one$ok)
})

test_that("the first row label with no field above it must stay first and is the only one that can have none", {
  mv <- app_fn("dta_move_column")
  d <- mxc_dta(c("rows: { key: A }", "cells: { type: SAS Num }"), headers = c("No", "b"))
  up <- mv(d, "m", "B", "up")
  expect_false(up$ok)
  expect_match(up$error, "must stay first", fixed = TRUE)
  down <- mv(d, "m", "A", "down")
  expect_false(down$ok)
  expect_match(down$error, "must stay first", fixed = TRUE)
  expect_identical(mxc_ids(d), c("A", "B"))

  # Further down, a move is a plain reorder.
  d3 <- mxc_dta(c("rows: { key: A }", "cells: { type: SAS Num }"),
    labels = c("A", "B", "C"), headers = c("No", "b", "c")
  )
  r <- mv(d3, "m", "C", "up")
  expect_true(r$ok, info = r$error)
  expect_identical(mxc_ids(r$value), c("A", "C", "B"))
  expect_identical(
    unname(lapply(r$value@datasets$m@specs@columns, app_fn("dta_row_label_header"))),
    list(FALSE, "c", "b")
  )

  # "No field" on a label that is not first is refused; Empty is the way.
  set_col <- app_fn("dta_set_column")
  bad <- set_col(d, "m", id = "B", type = "Char", nullable = TRUE, header_mode = "none")
  expect_false(bad$ok)
  expect_match(bad$error, "Only the first row label can have no field", fixed = TRUE)
  newbad <- set_col(d, "m", id = "Z", type = "Char", nullable = TRUE, header_mode = "none")
  expect_false(newbad$ok)
  expect_match(newbad$error, "Only the first row label", fixed = TRUE)
  fine <- set_col(d, "m", id = "B", type = "Char", nullable = TRUE, header_mode = "empty")
  expect_true(fine$ok, info = fine$error)
  expect_identical(app_fn("dta_row_label_header")(fine$value@datasets$m@specs@columns[["B"]]), "")
  # ... and the first one can be given "no field" (and can lose it again).
  plain <- mxc_dta(c("rows: { key: A }", "cells: { type: SAS Num }"))
  first <- set_col(plain, "m", id = "A", type = "Char", nullable = FALSE, header_mode = "none")
  expect_true(first$ok, info = first$error)
  expect_identical(app_fn("dta_row_label_header")(first$value@datasets$m@specs@columns[["A"]]), FALSE)
  undone <- set_col(first$value, "m", id = "A", type = "Char", nullable = FALSE, header_mode = "id")
  expect_true(undone$ok, info = undone$error)
  expect_null(app_fn("dta_row_label_header")(undone$value@datasets$m@specs@columns[["A"]]))
})

test_that("dta_column_fields reports the header mode of each of the four forms", {
  d <- mxc_dta(c("rows: { key: A }", "cells: { type: SAS Num }"),
    labels = c("A", "B", "C", "D"), headers = c("No", "\"\"", "x", NA)
  )
  f <- function(id) app_fn("dta_column_fields")(d, "m", id)
  expect_identical(f("A")$header_mode, "none")
  expect_identical(f("B")$header_mode, "empty")
  expect_identical(f("C")$header_mode, "text")
  expect_identical(f("C")$header_text, "x")
  expect_identical(f("D")$header_mode, "id")
  expect_identical(f("D")$header_text, "")
  # A tabular column reads as "same as the ID".
  tab <- app_fixture_dta()
  tid <- app_fn("dta_column_ids")(tab, names(tab@datasets)[1])[1]
  expect_identical(app_fn("dta_column_fields")(tab, names(tab@datasets)[1], tid)$header_mode, "id")
})

test_that("the sketch and the overview show the effective header text of each of the four forms", {
  d <- mxc_dta(c("rows: { key: A }", "cells: { type: SAS Num }"),
    labels = c("A", "B", "C", "D"), headers = c("No", "\"\"", "x", NA)
  )
  sk <- app_fn("dta_matrix_sketch")(d, "m")
  expect_length(sk, 2L)
  # Header line: each header text above its column -- nothing where the header
  # line has no field (named in the note), "" for an empty field; second line:
  # the row labels' IDs.
  expect_match(sk[1], "^ +\"\" +x +D ", perl = TRUE)
  expect_match(sk[1], "no field above A", fixed = TRUE)
  expect_match(sk[2], "^A +B +C +D ", perl = TRUE)
  # One header form at a time.
  one <- function(h) {
    app_fn("dta_matrix_sketch")(
      mxc_dta(c("rows: { key: A }", "cells: { type: SAS Num }"), labels = "A", headers = h), "m"
    )
  }
  expect_match(one("\"\"")[1], "^\"\" ")
  expect_false(grepl("no field", one("\"\"")[1], fixed = TRUE))
  expect_match(one("gene_id")[1], "^gene_id ")
  expect_match(one(NA)[1], "^A ")
  expect_match(one("No")[1], "^ +label ")
  expect_match(one("No")[1], "no field above A", fixed = TRUE)

  ov <- app_fn("dta_columns_overview")(d, "m")
  expect_identical(names(ov)[1:2], c("id", "header"))
  expect_identical(ov$header, c("(no field)", "(empty)", "x", "D"))
  # Tabular: unchanged (no header column) and no sketch.
  tab <- app_fixture_dta()
  tn <- names(tab@datasets)[1]
  expect_false("header" %in% names(app_fn("dta_columns_overview")(tab, tn)))
  expect_identical(app_fn("dta_matrix_sketch")(tab, tn), character(0))
  expect_null(app_fn("matrix_sketch_ui")(tab, tn))
  expect_match(render_html(app_fn("matrix_sketch_ui")(d, "m")), "matrix-sketch", fixed = TRUE)
})

test_that("the Row labels list and the Matrix layout modal of a matrix carry the sketch; a tabular list does not", {
  mxs_skip()
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = mxs_input(mxs_ext("matrix_dta.yaml")))
    unlock_editing(session)
    session$setInputs(edit_cols = 1)
    expect_match(ui_text(output$col_modal_body), "matrix-sketch", fixed = TRUE)
    session$setInputs(edit_matrix = 1)
    expect_match(ui_text(output$mx_modal_body), "matrix-sketch", fixed = TRUE)
  })
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = mxs_input(mxs_ext("clinical_dta.yaml")))
    unlock_editing(session)
    session$setInputs(edit_cols = 1)
    expect_false(grepl("matrix-sketch", ui_text(output$col_modal_body), fixed = TRUE))
  })
})

test_that("a rename without a header mode keeps the header; a mode edits it; a keyword text is just text", {
  set_col <- app_fn("dta_set_column")
  hdr <- app_fn("dta_row_label_header")
  # NULL mode (a caller without the field) keeps it, also across a rename.
  g <- set_col(mxc_bundled(), "gene_counts",
    id = "GENE_KEY", old_id = "GENE_ID", type = "Char", nullable = FALSE
  )
  expect_true(g$ok, info = g$error)
  expect_identical(hdr(g$value@datasets$gene_counts@specs@columns[["GENE_KEY"]]), "gene_id")
  e <- mxc_dta(c("rows: { key: A }", "cells: { type: SAS Num }"), headers = c("No", "\"\""))
  r1 <- set_col(e, "m", id = "C", old_id = "A", type = "Char", nullable = FALSE)
  expect_true(r1$ok, info = r1$error)
  expect_identical(hdr(r1$value@datasets$m@specs@columns[["C"]]), FALSE)
  r2 <- set_col(e, "m", id = "D", old_id = "B", type = "Char", nullable = TRUE)
  expect_true(r2$ok, info = r2$error)
  expect_identical(hdr(r2$value@datasets$m@specs@columns[["D"]]), "")

  # An explicit mode edits it.
  t <- set_col(e, "m",
    id = "B", old_id = "B", type = "Char", nullable = TRUE,
    header_mode = "text", header_text = "named"
  )
  expect_true(t$ok, info = t$error)
  expect_identical(hdr(t$value@datasets$m@specs@columns[["B"]]), "named")
  # ... a blank text is refused, not stored.
  blank <- set_col(e, "m",
    id = "B", old_id = "B", type = "Char", nullable = TRUE,
    header_mode = "text", header_text = ""
  )
  expect_false(blank$ok)
  expect_match(blank$error, "choose Empty", fixed = TRUE)

  # The text 'named' or 'blank' round-trips through save -> read as that text.
  txt <- app_fn("dta_to_yaml_text")(t$value)
  expect_true(txt$ok)
  back <- mx_app_dta(strsplit(txt$value, "\n", fixed = TRUE)[[1]])
  expect_identical(hdr(back@datasets$m@specs@columns[["B"]]), "named")
  expect_identical(hdr(back@datasets$m@specs@columns[["A"]]), FALSE)
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
  expect_identical(unlist(l$columns$values), c("007", "S1"))
  expect_identical(as.character(l$rows$key), c("A", "B"))
  expect_identical(as.numeric(unlist(l$cells$values)), c(0, 1, 2))
})

test_that("a cell nullable that was never set stays unset on an unchanged save", {
  d <- mxc_dta(c("rows: { key: A }", "cells: { type: SAS Num }"))
  expect_null(mxc_mx(d)$cells$nullable)
  r <- mxc_save(d, "m")
  expect_true(r$ok, info = r$error)
  expect_null(mxc_mx(r$value)$cells$nullable)
  expect_identical(mxc_mx(r$value), mxc_mx(d))
})

test_that("Matrix layout edits land", {
  dta <- mxc_bundled()
  gc <- function(r) as.list(r$value@datasets$gene_counts@matrix)

  r <- mxc_save(dta, "gene_counts", list(columns_pattern = "^T[0-9]+$"))
  expect_true(r$ok, info = r$error)
  expect_identical(gc(r)$columns$pattern, "^T[0-9]+$")

  # min == max is written as the scalar count.
  r <- mxc_save(dta, "gene_counts", list(n_columns_min = 5, n_columns_max = 5))
  expect_true(r$ok, info = r$error)
  expect_false(is.list(gc(r)$columns$count))
  expect_equal(gc(r)$columns$count, 5)

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

  # The column-label rule edits land under `columns`.
  r <- mxc_save(dta, "gene_counts", list(columns_label = "Sample", columns_length = "12"))
  expect_true(r$ok, info = r$error)
  expect_identical(gc(r)$columns$label, "Sample")
  expect_equal(gc(r)$columns$length, 12)
})

test_that("the Matrix layout form no longer carries the header text of the row labels", {
  f <- app_fn("dta_matrix_fields")(mxc_bundled(), "gene_counts")
  expect_false(any(grepl("^(corner|header_)", names(f))))
  expect_true(all(c(
    "columns_label", "columns_description", "columns_pattern", "columns_values",
    "columns_length", "n_columns_min", "n_columns_max"
  ) %in% names(f)))
  # An unchanged save leaves every row label's header as it was.
  r <- mxc_save(mxc_bundled(), "gene_counts")
  expect_true(r$ok, info = r$error)
  hdr <- app_fn("dta_row_label_header")
  expect_identical(
    unname(lapply(r$value@datasets$gene_counts@specs@columns, hdr)),
    list("gene_id", "gene_name")
  )
})

test_that("Matrix layout edits the form cannot carry are refused and change nothing", {
  dta <- mxc_bundled()
  before <- as.list(dta@datasets$gene_counts@matrix)
  refused <- list(
    "pattern and values" = list(columns_values = "S1\nS2"),
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
  num_inf <- mxc_dta(c("rows: { key: A }", "cells: { type: SAS Num, nullable: No, infinite: true }"))
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
    "      - { id: GENE_ID, header: gene_id, type: SAS Char, nullable: No }",
    "      - { id: GENE_NAME, header: gene_name, type: SAS Char, nullable: Yes }",
    "    matrix:",
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
    for (id in c("add_ds_mx_label", "add_ds_mx_header", "add_ds_mx_cells")) {
      expect_true(grepl(id, html, fixed = TRUE), info = id)
    }
    for (grp in c("add_ds_mx_header", "add_ds_mx_cells")) {
      tags <- mxc_inputs_named(html, grp)
      expect_gte(length(tags), 3L)
      expect_false(any(grepl("checked", tags)), info = grp)
    }
    expect_false(any(grepl("checked", mxc_inputs_named(html, "add_ds_mx_header_text"))))
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
    # No header text chosen: refused, nothing changes, and the reason is on screen.
    session$setInputs(
      add_ds_id = "new_mx", add_ds_type = "matrix", add_ds_mx_label = "G",
      add_ds_mx_cells = "SAS Int", add_ds_save = 1
    )
    expect_identical(names(rv$dta@datasets), names(before@datasets))
    expect_false("new_mx" %in% names(rv$dta@datasets))
    expect_match(ui_text(output$add_ds_msg), "Choose the header text", fixed = TRUE)

    session$setInputs(add_ds_mx_header = "empty", add_ds_save = 2)
    expect_true("new_mx" %in% names(rv$dta@datasets))
    expect_true(inherits(rv$dta@datasets$new_mx, "DTAtools::DTADataSetMatrix"))
    expect_identical(rv$structure[["new_mx"]]$type, "matrix")
    expect_identical(app_fn("dta_row_label_header")(rv$dta@datasets$new_mx@specs@columns[["G"]]), "")
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
      add_ds_mx_header = "id", add_ds_mx_cells = "SAS Num", add_ds_save = 1
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
    # The form carried no header field, so the label keeps its header text.
    expect_identical(app_fn("dta_row_label_header")(rv$dta@datasets$gene_counts@specs@columns[["GENE_KEY"]]), "gene_id")
    expect_match(rv$yaml_text, "key:\\s+GENE_KEY")
    expect_false(grepl("key:\\s+GENE_ID", rv$yaml_text))

    session$setInputs(edit_rules = 1)
    expect_match(ui_text(output$rule_modal_body), ".matrix:row_key", fixed = TRUE)
  })
})

test_that("the row-label form shows the saved header mode as checked; a tabular column form has no such input", {
  mxs_skip()
  value_of <- function(tags) sub(".*value=\"([a-z]+)\".*", "\\1", tags)
  checked_mode <- function(html) {
    tags <- mxc_inputs_named(html, "col_header_mode")
    value_of(tags[grepl("checked", tags)])
  }
  shiny::testServer(.shiny_app_dir(), {
    mxc_load_bundled(session)
    expect_identical(rv$active, "gene_counts")
    session$setInputs(edit_cols = 1)
    session$setInputs(col_edit_click = 1)
    html <- ui_text(output$col_modal_body)
    expect_identical(sort(value_of(mxc_inputs_named(html, "col_header_mode"))), c("empty", "id", "none", "text"))
    # GENE_ID has the header text gene_id: "Other text" is checked, with the text.
    expect_identical(checked_mode(html), "text")
    expect_match(html, "value=\"gene_id\"", fixed = TRUE)

    # A new row label defaults to "Same as the ID".
    session$setInputs(col_add = 1)
    expect_identical(checked_mode(ui_text(output$col_modal_body)), "id")
  })
  # mature_counts' row label has an empty header text: "Empty" is checked.
  shiny::testServer(.shiny_app_dir(), {
    mxc_load_bundled(session)
    rv$active <- "mature_counts"
    session$flushReact()
    session$setInputs(edit_cols = 1)
    session$setInputs(col_edit_click = 1)
    expect_identical(checked_mode(ui_text(output$col_modal_body)), "empty")
  })
  # A tabular dataset's column form has no header input at all.
  shiny::testServer(.shiny_app_dir(), {
    session$setInputs(dta_file = mxs_input(mxs_ext("clinical_dta.yaml")))
    unlock_editing(session)
    session$setInputs(edit_cols = 1)
    session$setInputs(col_edit_click = 1)
    html <- ui_text(output$col_modal_body)
    expect_true(grepl("col_id", html, fixed = TRUE))
    expect_false(grepl("col_header_mode", html, fixed = TRUE))
    expect_false(grepl("col_header_text", html, fixed = TRUE))
  })
})

test_that("saving the row-label form with a header mode stores it; 'no field' is refused on a later label", {
  mxs_skip()
  fill <- function(session, id, ...) {
    session$setInputs(
      col_id = id, col_label = "", col_backend = "SAS", col_type = "Char",
      col_format = "", col_length = "", col_nullable = FALSE, col_values = "",
      col_pattern = "", col_desc = "", ...
    )
  }
  shiny::testServer(.shiny_app_dir(), {
    mxc_load_bundled(session)
    hdr <- app_fn("dta_row_label_header")
    cols <- function() rv$dta@datasets$gene_counts@specs@columns
    session$setInputs(edit_cols = 1)

    # First label: "No field" saves, and the YAML says so.
    session$setInputs(col_edit_click = 1)
    fill(session, "GENE_ID", col_header_mode = "none", col_header_text = "")
    session$setInputs(col_save = 1)
    expect_null(rv$col_msg)
    expect_identical(hdr(cols()[["GENE_ID"]]), FALSE)
    expect_match(rv$yaml_text, "header:\\s+(no|No|false)\\b")

    # Second label: "No field" is refused with a sentence, nothing changes.
    y <- rv$yaml_text
    session$setInputs(col_edit_click = 2)
    fill(session, "GENE_NAME", col_header_mode = "none", col_header_text = "")
    session$setInputs(col_save = 2)
    expect_match(ui_text(output$col_editor_msg), "Only the first row label", fixed = TRUE)
    expect_identical(hdr(cols()[["GENE_NAME"]]), "gene_name")
    expect_identical(rv$yaml_text, y)

    # Other text -- also a former keyword -- is stored as that text.
    session$setInputs(col_edit_click = 2)
    fill(session, "GENE_NAME", col_header_mode = "text", col_header_text = "blank")
    session$setInputs(col_save = 3)
    expect_null(rv$col_msg)
    expect_identical(hdr(cols()[["GENE_NAME"]]), "blank")
    session$setInputs(col_edit_click = 2)
    fill(session, "GENE_NAME", col_header_mode = "empty", col_header_text = "")
    session$setInputs(col_save = 4)
    expect_identical(hdr(cols()[["GENE_NAME"]]), "")
    session$setInputs(col_edit_click = 2)
    fill(session, "GENE_NAME", col_header_mode = "id", col_header_text = "")
    session$setInputs(col_save = 5)
    expect_null(hdr(cols()[["GENE_NAME"]]))
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
  r <- mxc_save(d, "gene_counts", list(columns_pattern = "("))
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
    "rows: { key: A }",
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
    "rows: { key: A }",
    "cells: { type: SAS Int, nullable: No, values: [1, 2, 1000] }"
  ))
  r <- mxc_save(int, "m", list(cell_type = "SAS Char"))
  expect_true(r$ok, info = r$error)
  expect_identical(as.character(unlist(mxc_mx(r$value)$cells$values)), c("1", "2", "1000"))
  expect_true(is.character(unlist(mxc_mx(r$value)$cells$values)))

  chr <- mxc_dta(c(
    "rows: { key: A }",
    "cells: { type: SAS Char, nullable: No, values: [\"0\", \"1.5\"] }"
  ))
  r <- mxc_save(chr, "m", list(cell_type = "SAS Num"))
  expect_true(r$ok, info = r$error)
  expect_true(is.numeric(unlist(mxc_mx(r$value)$cells$values)))
  expect_equal(as.numeric(unlist(mxc_mx(r$value)$cells$values)), c(0, 1.5))

  word <- mxc_dta(c(
    "rows: { key: A }",
    "cells: { type: SAS Char, nullable: No, values: [\"a\"] }"
  ))
  r <- mxc_save(word, "m", list(cell_type = "SAS Num"))
  expect_false(r$ok)
  expect_identical(r$error, "Allowed cell values must be numbers for SAS Num and SAS Int cells.")
  expect_identical(mxc_mx(word)$cells$type, "SAS Char")
})

# ---- wording ----------------------------------------------------------------------

test_that("renaming a row label onto an existing one says 'row label'; a column keeps 'column'", {
  d <- mxc_dta(c("rows: { key: A }", "cells: { type: SAS Num }"))
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

test_that("an edit of the row labels asks for a reload exactly when a loaded table goes stale", {
  needs <- app_fn("dta_matrix_needs_reload")
  ok <- function(r) {
    expect_true(isTRUE(r$ok), info = r$error)
    r$value
  }
  bound <- mx_app_bound()
  set <- function(dta, id, ...) {
    ok(app_fn("dta_set_column")(dta, "gene_counts", id = id, backend = "SAS", type = "Char", ...))
  }

  # What changes the names a table is read under, or the header line's layout.
  renamed <- set(bound, "GENE_UID", nullable = FALSE, old_id = "GENE_ID")
  added <- set(bound, "NOTE", nullable = TRUE)
  moved <- ok(app_fn("dta_move_column")(bound, "gene_counts", "GENE_NAME", "up"))
  removed <- ok(app_fn("dta_remove_column")(bound, "gene_counts", "GENE_NAME"))
  no_field <- set(bound, "GENE_ID", nullable = FALSE, header_mode = "none")
  for (after in list(renamed, added, moved, removed, no_field)) {
    expect_true(needs(bound, after, "gene_counts"))
  }

  # What does not: another header text, a pattern, nothing at all.
  other_text <- set(bound, "GENE_ID", nullable = FALSE, header_mode = "text", header_text = "Gene")
  expect_false(needs(bound, other_text, "gene_counts"))
  expect_false(needs(bound, set(bound, "GENE_NAME", nullable = TRUE, pattern = "^[A-Z]"), "gene_counts"))
  expect_false(needs(bound, bound, "gene_counts"))

  # With no table loaded there is nothing to load again; nor for another type.
  unbound <- mx_app_dta(mx_app_fields_yaml())
  expect_false(needs(unbound, set(unbound, "GENE_UID", nullable = FALSE, old_id = "GENE_ID"), "gene_counts"))
  tab <- app_fixture_dta_with_data()
  expect_false(needs(tab, tab, names(tab@datasets)[1]))
})
