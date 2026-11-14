# A matrix dataset in the document exports: its column table is the row-label
# table, and one "Matrix layout" block states the matrix
# contract. Expected strings are this renderer's documented wording (written
# from the contract, not copied from observed output).

exm_matrix <- function(corner = list(NULL, "gene_name"), cells = NULL) {
  dta_dataset_from_list(list(
    id = "gene_counts", type = "matrix",
    files = list(filename = "g.tsv", type = "tsv"),
    row_labels = list(
      list(id = "GENE_ID", type = "SAS Char", nullable = FALSE),
      list(id = "GENE_NAME", type = "SAS Char", nullable = TRUE)
    ),
    matrix = list(
      header = list(
        corner = corner, label = "Sample ID", pattern = "^S[0-9]{4}$",
        count = list(min = 1, max = 384)
      ),
      rows = list(key = "GENE_ID", count = list(min = 50000)),
      cells = cells %||% list(type = "SAS Num", nullable = FALSE, range = list(min = 0), infinite = FALSE)
    )
  ))
}

exm_dta <- function(matrix = exm_matrix()) {
  DTA(datasets = list(matrix, create_example_DTADataSetTabular(1)))
}

exm_value <- function(rows, key) rows$value[rows$key == key]

test_that("the layout rows state the matrix contract in plain English", {
  rows <- .matrix_layout_rows(exm_matrix())
  expect_equal(exm_value(rows, "Header corner"), "(no field), gene_name")
  expect_equal(exm_value(rows, "Column label"), "Sample ID")
  expect_equal(
    exm_value(rows, "Column label rule"),
    "matches ^S[0-9]{4}$; labels must be unique and must not repeat a row-label id"
  )
  expect_equal(exm_value(rows, "Data columns"), "1\u2013384")
  expect_equal(exm_value(rows, "Row key"), "GENE_ID (unique, no missing values)")
  expect_equal(exm_value(rows, "Data rows"), "at least 50000")
  expect_equal(exm_value(rows, "Cell type"), "SAS Num")
  expect_equal(exm_value(rows, "Missing cells"), "not allowed")
  expect_equal(exm_value(rows, "Cell range"), "[0, no upper bound]")
  expect_equal(exm_value(rows, "Infinite values"), "not allowed")
  expect_false(any(c("Cell values", "Cell pattern", "Cell label") %in% rows$key))
})

test_that("layout rows cover blank corners, exact counts, values and Int cells", {
  ds <- dta_dataset_from_list(list(
    id = "g", type = "matrix", files = list(filename = "g.tsv", type = "tsv"),
    row_labels = list(list(id = "SNP", type = "SAS Char", nullable = FALSE)),
    matrix = list(
      header = list(corner = "blank", count = 3L),
      rows = list(count = list(min = 0, max = 10)),
      cells = list(type = "SAS Int", nullable = TRUE, values = list(0, 1, 2))
    )
  ))
  rows <- .matrix_layout_rows(ds)
  expect_equal(exm_value(rows, "Header corner"), "blank")
  expect_equal(exm_value(rows, "Data columns"), "exactly 3")
  expect_equal(exm_value(rows, "Data rows"), "at most 10")
  expect_equal(exm_value(rows, "Missing cells"), "allowed")
  expect_equal(exm_value(rows, "Cell values"), "0, 1, 2")
  expect_false("Infinite values" %in% rows$key)
  expect_false("Cell range" %in% rows$key)
})

test_that("write_dta(format = 'md') titles the matrix table and adds the layout block", {
  out <- tempfile(fileext = ".md")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  write_dta(exm_dta(), file = out, format = "md", overwrite = TRUE, quiet = TRUE)
  md <- paste(readLines(out, warn = FALSE), collapse = "\n")

  expect_match(md, "Row label columns", fixed = TRUE)
  expect_match(md, "Matrix layout", fixed = TRUE)
  expect_match(md, "(no field), gene", fixed = TRUE)
  expect_match(md, "at least 50000", fixed = TRUE)
  expect_match(md, "no upper bound", fixed = TRUE)
  expect_match(md, "Infinite values", fixed = TRUE)
  # The tabular dataset keeps its title, and only one dataset gets a layout.
  expect_match(md, "Column Specifications", fixed = TRUE)
  expect_equal(lengths(regmatches(md, gregexpr("Matrix layout", md, fixed = TRUE))), 1L)
  expect_equal(lengths(regmatches(md, gregexpr("Row label columns", md, fixed = TRUE))), 1L)
})

test_that("write_dta(format = 'docx') carries the same texts", {
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  write_dta(exm_dta(), file = out, format = "docx", overwrite = TRUE, quiet = TRUE)
  text <- .docx_paragraphs(out)

  expect_true("Row label columns" %in% text)
  expect_true("Matrix layout" %in% text)
  expect_true("Column Specifications" %in% text)
  expect_true("(no field), gene_name" %in% text)
  expect_true("at least 50000" %in% text)
  expect_true("[0, no upper bound]" %in% text)
  expect_true("Infinite values" %in% text)
})

test_that("write_dataset_metadata renders a matrix standalone in Markdown and Word", {
  md <- tempfile(fileext = ".md")
  docx <- tempfile(fileext = ".docx")
  on.exit(unlink(c(md, docx), force = TRUE), add = TRUE)
  write_dataset_metadata(exm_matrix(), file = md, format = "md", overwrite = TRUE, quiet = TRUE)
  write_dataset_metadata(exm_matrix(), file = docx, format = "docx", overwrite = TRUE, quiet = TRUE)
  md_text <- paste(readLines(md, warn = FALSE), collapse = "\n")
  expect_match(md_text, "Row label columns", fixed = TRUE)
  expect_match(md_text, "Matrix layout", fixed = TRUE)
  expect_false(grepl("Column Specifications", md_text, fixed = TRUE))
  text <- .docx_paragraphs(docx)
  expect_true(all(c("Row label columns", "Matrix layout", "Infinite values") %in% text))
})

test_that("the template blocks title the matrix table and add the layout", {
  dta <- exm_dta()
  doc <- officer::read_docx()
  doc <- .tv_block_render_column_specs(doc, dta, NULL)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  print(doc, target = out)
  text <- .docx_paragraphs(out)
  expect_true(any(grepl("^Row label columns", text)))
  expect_true(any(grepl("^Column Specifications", text)))
  expect_true("Matrix layout" %in% text)
  expect_true("Infinite values" %in% text)

  doc <- .tv_block_render_datasets(officer::read_docx(), dta, NULL)
  print(doc, target = out)
  text <- .docx_paragraphs(out)
  expect_true(all(c("Row label columns", "Column Specifications", "Matrix layout") %in% text))
})

exm_rows <- function(corner = "blank", header = list(), cells = list(type = "SAS Num"), key = NULL,
                     n_labels = 2L, rows_count = NULL) {
  labels <- list(
    list(id = "GENE_ID", type = "SAS Char", nullable = FALSE),
    list(id = "GENE_NAME", type = "SAS Char", nullable = FALSE)
  )[seq_len(n_labels)]
  m <- list(
    header = c(list(corner = corner, count = 2L), header),
    cells = cells
  )
  if (!is.null(key)) m$rows <- list(key = key)
  if (!is.null(rows_count)) m$rows$count <- rows_count
  dta_dataset_from_list(list(
    id = "g", type = "matrix", files = list(filename = "g.tsv", type = "tsv"),
    row_labels = labels, matrix = m
  ))
}

test_that("an unwritten `infinite` on a Num cell reads as not allowed (D9)", {
  rows <- .matrix_layout_rows(exm_rows(cells = list(type = "SAS Num")))
  expect_equal(exm_value(rows, "Infinite values"), "not allowed")
})

test_that("corner forms are spelled out", {
  expect_equal(
    exm_value(.matrix_layout_rows(exm_rows(corner = "named")), "Header corner"),
    "named: GENE_ID, GENE_NAME"
  )
  expect_equal(
    exm_value(.matrix_layout_rows(exm_rows(corner = list("", "x"))), "Header corner"),
    "(blank), x"
  )
})

test_that("keys state their constraint and unbounded rows read 'any number'", {
  one <- .matrix_layout_rows(exm_rows(key = "GENE_ID", rows_count = list(min = 0)))
  expect_equal(exm_value(one, "Row key"), "GENE_ID (unique, no missing values)")
  expect_equal(exm_value(one, "Data rows"), "any number")
  two <- .matrix_layout_rows(exm_rows(key = c("GENE_ID", "GENE_NAME")))
  expect_equal(exm_value(two, "Row key"), "GENE_ID + GENE_NAME (unique together, no missing values)")
})

test_that("header and cell descriptions get their own rows, only when set", {
  rows <- .matrix_layout_rows(exm_rows(
    header = list(label = "Sample", description = "One column per sample"),
    cells = list(type = "SAS Num", label = "Count", description = "Raw read counts")
  ))
  expect_equal(exm_value(rows, "Column label description"), "One column per sample")
  expect_equal(exm_value(rows, "Cell description"), "Raw read counts")
  bare <- .matrix_layout_rows(exm_rows())
  expect_false(any(c("Column label description", "Cell description") %in% bare$key))
})

test_that("numbers read back exactly and big or small ones stay short", {
  rng <- function(v) {
    exm_value(
      .matrix_layout_rows(exm_rows(cells = list(type = "SAS Num", range = list(min = v)))),
      "Cell range"
    )
  }
  expect_equal(rng(0.5), "[0.5, no upper bound]")
  expect_equal(rng(50000), "[50000, no upper bound]")
  expect_equal(rng(1e-7), "[1e-07, no upper bound]")
  expect_equal(rng(1e300), "[1e+300, no upper bound]")
  shown <- sub("^\\[(.*), no upper bound\\]$", "\\1", rng(0.30000000000000004))
  expect_identical(as.numeric(shown), 0.30000000000000004)
})

test_that("the cell length row is for Char cells only", {
  num <- .matrix_layout_rows(exm_rows(cells = list(type = "SAS Num", length = 8)))
  expect_false("Cell length" %in% num$key)
  chr <- .matrix_layout_rows(exm_rows(cells = list(type = "SAS Char", length = 8)))
  expect_equal(exm_value(chr, "Cell length"), "at most 8 characters")
})

test_that("a pattern containing a pipe survives the Markdown table", {
  ds <- exm_rows(cells = list(type = "SAS Char", pattern = "^(a|b)$"))
  out <- tempfile(fileext = ".md")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  write_dataset_metadata(ds, file = out, format = "md", overwrite = TRUE, quiet = TRUE)
  line <- grep("Cell pattern", readLines(out, warn = FALSE), value = TRUE, fixed = TRUE)
  expect_length(line, 1L)
  expect_match(.md_unescape(line), "^(a|b)$", fixed = TRUE)
})

test_that("{COLUMN_SPECS} without a heading still renders the matrix's row-label table", {
  dta <- exm_dta()
  doc <- .tv_block_render_column_specs(officer::read_docx(), dta, NULL, heading = FALSE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  print(doc, target = out)
  text <- .docx_paragraphs(out)
  expect_true("GENE_ID" %in% text || any(grepl("GENE_ID", text)))
  expect_true("Matrix layout" %in% text)
})

test_that("DATASET_COLUMN_COUNT of a matrix counts its row-label columns", {
  vars <- .tv_dataset_variables(exm_matrix())
  expect_equal(vars[["{DATASET_COLUMN_COUNT}"]], "2")
})
