# A matrix dataset in the document exports: its column table is the row-label
# table, and one "Matrix layout" block states the matrix
# contract. Expected strings are this renderer's documented wording (written
# from the contract, not copied from observed output).

exm_matrix <- function(cells = NULL) {
  dta_dataset_from_list(list(
    id = "gene_counts", type = "matrix",
    files = list(filename = "g.tsv", type = "tsv"),
    row_labels = list(
      list(id = "GENE_ID", header = FALSE, type = "SAS Char", nullable = FALSE),
      list(id = "GENE_NAME", header = "gene_name", type = "SAS Char", nullable = TRUE)
    ),
    matrix = list(
      columns = list(
        label = "Sample ID", pattern = "^S[0-9]{4}$",
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
  expect_equal(
    exm_value(rows, "File layout"),
    "A header line, then one line per row. Each line starts with the row label columns GENE_ID, GENE_NAME; every further column is a data column."
  )
  expect_equal(
    exm_value(rows, "Header line"),
    "gene_name, then one label per data column (no field above GENE_ID)"
  )
  expect_false("Header corner" %in% rows$key)
  expect_equal(exm_value(rows, "Column label"), "Sample ID")
  expect_equal(
    exm_value(rows, "Column label rule"),
    "matches ^S[0-9]{4}$; labels must be unique and must not repeat a row-label id"
  )
  expect_equal(exm_value(rows, "Data columns"), "1 to 384")
  expect_equal(exm_value(rows, "Row key"), "GENE_ID (unique, no missing values)")
  expect_equal(exm_value(rows, "Data rows"), "at least 50000")
  expect_equal(exm_value(rows, "Cell type"), "SAS Num")
  expect_equal(exm_value(rows, "Missing cells"), "not allowed")
  expect_equal(exm_value(rows, "Cell range"), "at least 0")
  expect_equal(exm_value(rows, "Infinite values"), "not allowed")
  expect_false(any(c("Cell values", "Cell pattern", "Cell label") %in% rows$key))
})

test_that("layout rows cover empty header fields, exact counts, values and Int cells", {
  ds <- dta_dataset_from_list(list(
    id = "g", type = "matrix", files = list(filename = "g.tsv", type = "tsv"),
    row_labels = list(list(id = "SNP", header = "", type = "SAS Char", nullable = FALSE)),
    matrix = list(
      columns = list(count = 3L),
      rows = list(count = list(min = 0, max = 10)),
      cells = list(type = "SAS Int", nullable = TRUE, values = list(0, 1, 2))
    )
  ))
  rows <- .matrix_layout_rows(ds)
  expect_equal(exm_value(rows, "File layout"), paste0(
    "A header line, then one line per row. Each line starts with the row label column SNP; ",
    "every further column is a data column."
  ))
  expect_equal(exm_value(rows, "Header line"), "(empty), then one label per data column")
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
  expect_match(md, "File layout", fixed = TRUE)
  expect_match(md, "Header line", fixed = TRUE)
  expect_false(grepl("Header corner", md, fixed = TRUE))
  expect_match(.md_unescape(md), "no field above GENE_ID", fixed = TRUE)
  expect_match(md, "at least 50000", fixed = TRUE)
  expect_match(md, "at least 0", fixed = TRUE)
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
  expect_true("gene_name, then one label per data column (no field above GENE_ID)" %in% text)
  expect_true("at least 50000" %in% text)
  expect_true("at least 0" %in% text)
  expect_false("Header corner" %in% text)
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

exm_rows <- function(headers = list("", ""), columns = list(), cells = list(type = "SAS Num"), key = NULL,
                     n_labels = 2L, rows_count = NULL) {
  labels <- list(
    list(id = "GENE_ID", type = "SAS Char", nullable = FALSE),
    list(id = "GENE_NAME", type = "SAS Char", nullable = FALSE)
  )[seq_len(n_labels)]
  for (i in seq_along(labels)) {
    if (!is.null(headers[[i]])) labels[[i]]$header <- headers[[i]]
  }
  m <- list(
    columns = utils::modifyList(list(count = 2L), columns),
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

test_that("the header line is spelled out for every form of `header`", {
  line <- function(headers, n_labels = 2L) {
    exm_value(.matrix_layout_rows(exm_rows(headers = headers, n_labels = n_labels)), "Header line")
  }
  expect_equal(line(list(NULL, NULL)), "GENE_ID, GENE_NAME, then one label per data column")
  expect_equal(line(list("", "x")), "(empty), x, then one label per data column")
  expect_equal(
    line(list(FALSE, "x")),
    "x, then one label per data column (no field above GENE_ID)"
  )
  expect_equal(
    line(list(FALSE), n_labels = 1L),
    "one label per data column (no field above GENE_ID)"
  )
  expect_equal(line(list("gene id", NULL)), "gene id, GENE_NAME, then one label per data column")
  expect_equal(
    exm_value(.matrix_layout_rows(exm_rows(n_labels = 1L)), "File layout"),
    paste0(
      "A header line, then one line per row. Each line starts with the row label column GENE_ID; ",
      "every further column is a data column."
    )
  )
})

test_that("counts and the cell range are written in words", {
  rows <- function(columns = list(), rows_count = NULL, cells = list(type = "SAS Num")) {
    .matrix_layout_rows(exm_rows(columns = columns, rows_count = rows_count, cells = cells))
  }
  expect_equal(exm_value(rows(list(count = list(min = 1, max = 384))), "Data columns"), "1 to 384")
  expect_equal(exm_value(rows(list(count = list(min = 3))), "Data columns"), "at least 3")
  expect_equal(exm_value(rows(list(count = list(max = 5))), "Data columns"), "1 to 5")
  expect_equal(exm_value(rows(list(count = 96)), "Data columns"), "exactly 96")
  expect_equal(exm_value(rows(rows_count = list(max = 5)), "Data rows"), "1 to 5")
  expect_equal(exm_value(rows(rows_count = list(min = 0, max = 5)), "Data rows"), "at most 5")
  expect_equal(exm_value(rows(rows_count = list(min = 0)), "Data rows"), "any number")
  rng <- function(r) exm_value(rows(cells = list(type = "SAS Num", range = r)), "Cell range")
  expect_equal(rng(list(min = 0)), "at least 0")
  expect_equal(rng(list(max = 5)), "at most 5")
  expect_equal(rng(list(min = 0, max = 1)), "0 to 1 (both included)")
  expect_false("Cell range" %in% rows()$key)
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
    columns = list(label = "Sample", description = "One column per sample"),
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
  expect_equal(rng(0.5), "at least 0.5")
  expect_equal(rng(50000), "at least 50000")
  expect_equal(rng(1e-7), "at least 1e-07")
  expect_equal(rng(1e300), "at least 1e+300")
  shown <- sub("^at least ", "", rng(0.30000000000000004))
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

# ---- the R vocabulary ------------------------------------------------------------
# A matrix of R cells is described by the same layout rows as its SAS twin; only
# the type text differs.

# The layout rows without the one that names the type.
exm_without_type <- function(rows) rows[rows$key != "Cell type", , drop = FALSE]

exm_twin <- function(sas, r) {
  list(
    sas = .matrix_layout_rows(exm_rows(cells = sas)),
    r = .matrix_layout_rows(exm_rows(cells = r))
  )
}

test_that("R numeric cells state the range and infinite rows a SAS Num cell states", {
  both <- exm_twin(
    list(type = "SAS Num", nullable = FALSE, range = list(min = 0, max = 1), infinite = TRUE),
    list(type = "R numeric", nullable = FALSE, range = list(min = 0, max = 1), infinite = TRUE)
  )
  expect_equal(exm_value(both$sas, "Cell type"), "SAS Num")
  expect_equal(exm_value(both$r, "Cell type"), "R numeric")
  expect_equal(exm_value(both$r, "Cell range"), "0 to 1 (both included)")
  expect_equal(exm_value(both$r, "Infinite values"), "allowed")
  expect_equal(exm_without_type(both$r)$value, exm_without_type(both$sas)$value)
  expect_equal(exm_without_type(both$r)$key, exm_without_type(both$sas)$key)
  # unwritten means not allowed, as for SAS Num
  expect_equal(
    exm_value(.matrix_layout_rows(exm_rows(cells = list(type = "R numeric"))), "Infinite values"),
    "not allowed"
  )
})

test_that("R integer cells state a range and no infinite row, like SAS Int", {
  rows <- .matrix_layout_rows(exm_rows(cells = list(type = "R integer", range = list(min = 0))))
  expect_equal(exm_value(rows, "Cell type"), "R integer")
  expect_equal(exm_value(rows, "Cell range"), "at least 0")
  expect_false("Infinite values" %in% rows$key)
})

test_that("the cell length row is for R character cells too, and for no other R type", {
  chr <- .matrix_layout_rows(exm_rows(cells = list(type = "R character", length = 8)))
  expect_equal(exm_value(chr, "Cell type"), "R character")
  expect_equal(exm_value(chr, "Cell length"), "at most 8 characters")
  expect_false("Infinite values" %in% chr$key)
  for (type in c("R numeric", "R integer")) {
    rows <- .matrix_layout_rows(exm_rows(cells = list(type = type)))
    expect_false("Cell length" %in% rows$key, info = type)
  }
})

test_that("a document of an R matrix names the R types, and a % in a format survives", {
  ds <- dta_dataset_from_list(list(
    id = "r_counts", type = "matrix", files = list(filename = "r.tsv", type = "tsv"),
    row_labels = list(
      list(id = "SAMPLE", header = "", type = "R character", length = 12, nullable = FALSE),
      list(id = "TAKEN", type = "R Date", format = "R %d.%m.%Y", nullable = TRUE)
    ),
    matrix = list(
      columns = list(pattern = "^G[0-9]+$"),
      cells = list(type = "R numeric", nullable = FALSE, range = list(min = 0), infinite = FALSE)
    )
  ))
  out <- tempfile(fileext = ".md")
  docx <- tempfile(fileext = ".docx")
  on.exit(unlink(c(out, docx), force = TRUE), add = TRUE)
  write_dataset_metadata(ds, file = out, format = "md", overwrite = TRUE, quiet = TRUE)
  md <- .md_unescape(paste(readLines(out, warn = FALSE), collapse = "\n"))
  for (text in c(
    "Row label columns", "Matrix layout", "R character", "R Date", "R numeric",
    "%d.%m.%Y", "Infinite values", "Cell range", "at least 0"
  )) {
    expect_match(md, text, fixed = TRUE)
  }
  write_dataset_metadata(ds, file = docx, format = "docx", overwrite = TRUE, quiet = TRUE)
  text <- .docx_paragraphs(docx)
  expect_true(all(c("Row label columns", "Matrix layout", "Infinite values") %in% text))
  expect_true("at least 0" %in% text)
})
