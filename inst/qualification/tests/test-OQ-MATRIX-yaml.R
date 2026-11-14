# The specification contract of a matrix dataset. REQ-MATRIX-047 ..
# REQ-MATRIX-053.
#
# Which keys a matrix accepts, which it refuses and by which condition class,
# and which combinations the dataset refuses to be built from. Classes are the
# contract: a message is prose and may be reworded, a class is how a caller
# tells one refusal from another. The datasets are built from the list a YAML
# specification reads as.

# ---- local fixtures -------------------------------------------------------------

my_labels <- function() list(list(id = "ID", type = "SAS Char", nullable = FALSE))

my_matrix <- function() {
  list(header = list(corner = "blank"), cells = list(type = "SAS Int", nullable = FALSE))
}

# A matrix dataset body; named arguments replace or (with NULL) remove a key.
my_body <- function(...) {
  body <- list(
    id = "m", type = "matrix", files = list(filename = "m.tsv", type = "tsv"),
    row_labels = my_labels(), matrix = my_matrix()
  )
  over <- list(...)
  for (nm in names(over)) body[nm] <- list(over[[nm]])
  body
}

my_refused <- function(body, class) mx_is(mx_error(dta_dataset_from_list(body)), class)

test_that("OQ-MATRIX-045 | a matrix dataset cannot be built with tables, and records its header only when a file is loaded | REQ-MATRIX-047", {
  specs <- DTAColumnSpecCollection(columns = list(
    ID = DTAColumnSpec(id = "ID", type = "SAS Char", nullable = FALSE)
  ))
  refused <- mx_error(DTADataSetMatrix(
    id = "m", specs = specs, matrix = my_matrix(),
    tables = list(m = data.frame(a = 1))
  ))
  qa_step("a non-empty tables argument", TRUE, mx_is(refused, "dta_matrix_tables_refused"))

  ds <- DTADataSetMatrix(
    id = "m", specs = specs, matrix = my_matrix(),
    files = list(DTAFileTSV(filename = "m.tsv"))
  )
  qa_step("built without it, nothing is bound and no header is recorded", c(0L, 0L), c(length(tables(ds)), length(ds@heads)))
  loaded <- load_file(ds, file = mx_path("\tS1\nr1\t1\n"), stream = "never")
  qa_step("after load_file() the header of the bound table is recorded", "m", names(loaded@heads))
})

test_that("OQ-MATRIX-046 | row_labels are accepted for a matrix and become its column specifications in the order written | REQ-MATRIX-048", {
  labels <- list(
    list(id = "ID", type = "SAS Char", nullable = FALSE, length = 12),
    list(id = "DESC", type = "SAS Char", nullable = TRUE)
  )
  body <- my_body(row_labels = labels, matrix = list(
    header = list(corner = "named"), cells = list(type = "SAS Int", nullable = FALSE)
  ))
  ds <- dta_dataset_from_list(body)
  qa_step("the row labels are the column specifications, in order", c("ID", "DESC"), names(ds@specs@columns))
  qa_step("with the keys they were written with", c(12, NA), c(ds@specs@columns[["ID"]]@structure@length, NA))
  qa_step("and the dataset is a matrix", "matrix", ds@type)

  via_factory <- DTADataSetFactory(
    "matrix",
    id = "m", files = list(filename = "m.tsv", type = "tsv"),
    row_labels = labels, matrix = body$matrix
  )
  qa_step(
    "DTADataSetFactory() builds the same from row_labels",
    c("DTAtools::DTADataSetMatrix", "ID", "DESC"),
    c(class(via_factory)[[1]], names(via_factory@specs@columns))
  )

  # The key as a specification file writes it.
  path <- mx_path(
    paste(
      "id: m", "type: matrix", "files: { filename: m.tsv, type: tsv }",
      "row_labels:", "  - id: ID", "    type: SAS Char", "    nullable: No",
      "matrix:", "  header: { corner: blank }", "  cells: { type: SAS Int, nullable: No }", "",
      sep = "\n"
    ),
    "m.yaml"
  )
  from_file <- read_dataset_from_yaml(path)
  qa_step(
    "a dataset specification file with row_labels reads as a matrix",
    c("DTAtools::DTADataSetMatrix", "ID"),
    c(class(from_file)[[1]], names(from_file@specs@columns))
  )
})

test_that("OQ-MATRIX-047 | columns on a matrix, row_labels or matrix elsewhere are refused by class | REQ-MATRIX-049", {
  tab <- list(
    id = "t", type = "tabular", files = list(filename = "t.csv", type = "csv"),
    columns = list(list(id = "ID", type = "SAS Char"))
  )
  qa_step(
    "columns on a matrix dataset",
    TRUE, my_refused(my_body(columns = my_labels()), "dta_matrix_columns_key")
  )
  qa_step(
    "row_labels on a tabular dataset",
    TRUE, my_refused(c(tab, list(row_labels = my_labels())), "dta_row_labels_key")
  )
  qa_step(
    "matrix on a tabular dataset",
    TRUE, my_refused(c(tab, list(matrix = my_matrix())), "dta_matrix_key")
  )
  qa_step(
    "matrix on a file dataset",
    TRUE,
    my_refused(
      list(id = "f", type = "file", files = list(filename = "f.pdf", type = "any"), matrix = my_matrix()),
      "dta_matrix_key"
    )
  )
  qa_step(
    "the factory refuses the same three",
    c(TRUE, TRUE, TRUE),
    c(
      mx_is(mx_error(DTADataSetFactory("matrix", id = "a", files = list(type = "tsv", filename = "m.tsv"), columns = my_labels(), matrix = my_matrix())), "dta_matrix_columns_key"),
      mx_is(mx_error(DTADataSetFactory("tabular", id = "a", files = list(type = "tsv", filename = "m.tsv"), row_labels = my_labels())), "dta_row_labels_key"),
      mx_is(mx_error(DTADataSetFactory("file", id = "a", files = list(type = "tsv", filename = "m.tsv"), matrix = my_matrix())), "dta_matrix_key")
    )
  )
})

test_that("OQ-MATRIX-048 | an unknown key in the matrix block and a missing required key are refused by class | REQ-MATRIX-050", {
  m <- function(header = list(corner = "blank"), cells = list(type = "SAS Int"), ...) {
    c(list(header = header, cells = cells), list(...))
  }
  unknown <- function(x) my_refused(my_body(matrix = x), "dta_matrix_field_unknown")
  missing <- function(x) my_refused(my_body(matrix = x), "dta_matrix_field_missing")

  qa_step("an unknown key under matrix", TRUE, unknown(m(rnage = 1)))
  qa_step("an unknown key under matrix.header", TRUE, unknown(m(header = list(corner = "blank", foo = 1))))
  qa_step("an unknown key under matrix.rows", TRUE, unknown(m(rows = list(foo = 1))))
  qa_step("an unknown key under matrix.cells", TRUE, unknown(m(cells = list(type = "SAS Int", rnage = 1))))
  qa_step(
    "an unknown key under matrix.cells.range",
    TRUE, unknown(m(cells = list(type = "SAS Int", range = list(min = 1, mx = 2))))
  )
  qa_step("no matrix block at all", TRUE, my_refused(my_body(matrix = NULL), "dta_matrix_field_missing"))
  qa_step("no header", TRUE, missing(list(cells = list(type = "SAS Int"))))
  qa_step("no cells", TRUE, missing(list(header = list(corner = "blank"))))
  qa_step("no corner", TRUE, missing(m(header = list())))
  qa_step("no cell type", TRUE, missing(m(cells = list())))
  qa_step("a complete block is accepted", NULL, mx_error(dta_dataset_from_list(my_body())))
})

test_that("OQ-MATRIX-049 | header values that YAML made numbers or logicals are refused | REQ-MATRIX-051", {
  header <- function(values) list(corner = "blank", values = values)
  build <- function(values) {
    my_refused(
      my_body(matrix = list(header = header(values), cells = list(type = "SAS Int"))),
      "dta_matrix_header_values_text"
    )
  }
  qa_step("an unquoted 007 arrives as the number 7", TRUE, build(list("A", 7)))
  qa_step("an unquoted Y arrives as TRUE", TRUE, build(list("A", TRUE)))
  qa_step(
    "quoted labels are accepted",
    NULL, mx_error(dta_dataset_from_list(my_body(
      matrix = list(header = header(list("007", "Y")), cells = list(type = "SAS Int"))
    )))
  )
})

test_that("OQ-MATRIX-050 | no row label, a key that is not a row label and a corner of the wrong length are refused by class | REQ-MATRIX-052", {
  qa_step(
    "no row-label column",
    TRUE, my_refused(my_body(row_labels = list()), "dta_matrix_invalid_row_labels")
  )
  qa_step(
    "a row key naming an id that is not a row label",
    TRUE,
    my_refused(
      my_body(matrix = list(header = list(corner = "blank"), rows = list(key = "NOPE"), cells = list(type = "SAS Int"))),
      "dta_matrix_invalid_row_key"
    )
  )
  qa_step(
    "a corner list of two entries for one row label",
    TRUE,
    my_refused(
      my_body(matrix = list(header = list(corner = list("a", "b")), cells = list(type = "SAS Int"))),
      "dta_matrix_invalid_corner"
    )
  )
  two <- list(
    list(id = "ID", type = "SAS Char", nullable = FALSE),
    list(id = "DESC", type = "SAS Char", nullable = TRUE)
  )
  qa_step(
    "a corner list of one entry for two row labels",
    TRUE,
    my_refused(
      my_body(row_labels = two, matrix = list(header = list(corner = list("a")), cells = list(type = "SAS Int"))),
      "dta_matrix_invalid_corner"
    )
  )
  qa_step(
    "while a list of two entries for two row labels is accepted",
    NULL,
    mx_error(dta_dataset_from_list(my_body(
      row_labels = two, matrix = list(header = list(corner = list("a", "b")), cells = list(type = "SAS Int"))
    )))
  )
})

test_that("OQ-MATRIX-051 | a file that is not a CSV or TSV with a header and a reserved rule id are refused by class | REQ-MATRIX-053", {
  qa_step(
    "a SAS file handler",
    TRUE, my_refused(my_body(files = list(filename = "m.sas7bdat", type = "sas7bdat")), "dta_matrix_invalid_file")
  )
  qa_step(
    "a TSV handler without a header line",
    TRUE,
    my_refused(my_body(files = list(filename = "m.tsv", type = "tsv", has_header = FALSE)), "dta_matrix_invalid_file")
  )
  qa_step(
    "a rule id that starts with .matrix:",
    TRUE,
    my_refused(
      my_body(rules = list(list(id = ".matrix:mine", type = "col_unique", columns = "ID"))),
      "dta_matrix_invalid_rule"
    )
  )
  qa_step(
    "a CSV handler and an ordinary rule are accepted",
    NULL,
    mx_error(dta_dataset_from_list(my_body(
      files = list(filename = "m.csv", type = "csv"),
      rules = list(list(id = "key_once", type = "col_unique", columns = "ID"))
    )))
  )
})
