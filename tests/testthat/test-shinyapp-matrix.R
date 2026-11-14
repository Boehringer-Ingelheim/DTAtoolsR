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
