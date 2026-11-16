#' @title DTAColumnSpecRowLabel Class
#' @description
#' A row-label column of a matrix dataset: a \code{\link{DTAColumnSpec}} that
#' also says what the header line holds above the column. Internal; written in
#' a specification as an entry of \code{row_labels:} with the key
#' \code{header}.
#' @param spec A \code{DTAColumnSpec}: the column itself.
#' @param header What the header line holds above this column. \code{NULL}
#'   (the key is not written) is the column's \code{id}, as for a tabular
#'   column; a single string, \code{""} included, is that text (compared after
#'   surrounding spaces and quotes are removed);
#'   \code{FALSE} (YAML \code{header: No}) says the header line has no field
#'   above this column, which the dataset allows for the first row label only.
#' @return An object of class \code{DTAColumnSpecRowLabel}.
#' @keywords internal
#' @noRd
#' @include DTAColumnSpec-class.R
DTAColumnSpecRowLabel <- S7::new_class(
  "DTAColumnSpecRowLabel",
  parent = DTAColumnSpec,
  constructor = function(spec, header = NULL) {
    # Judged here as well as by the validator: a value of another type (the
    # number YAML makes of `header: 1`) would otherwise be refused by the
    # property's type, with S7's message and no class.
    dta_check_row_label_header(header, spec@id)
    # A row label handed in again keeps everything but its header.
    new_object(S7::convert(spec, DTAColumnSpec), header = header)
  },
  properties = list(
    header = class_character_or_null | S7::class_logical
  ),
  validator = function(self) {
    dta_check_row_label_header(self@header, self@id)
    NULL
  }
)

# Aborts unless `header` is NULL, one text or FALSE.
dta_check_row_label_header <- function(header, id) {
  ok <- is.null(header) || identical(header, FALSE) ||
    (is.character(header) && length(header) == 1L && !is.na(header))
  if (!ok) {
    cli_abort(
      c(
        "{id}: {.field header} must be the header text, or {.code No} when the header line has no field above this column.",
        "i" = "Quote the text: YAML reads {.code header: 1} as a number and {.code header: Y} as TRUE."
      ),
      class = "dta_row_label_header_invalid"
    )
  }
  invisible(header)
}

method(as.list, DTAColumnSpecRowLabel) <- function(x, ...) {
  # S7::super() does not reach an S3 generic such as as.list(); convert() does.
  out <- as.list(S7::convert(x, DTAColumnSpec))
  if (is.null(x@header)) {
    return(out)
  }
  # Next to the id: the two together name the column.
  append(out, list(header = x@header), after = match("id", names(out)))
}

method(print, DTAColumnSpecRowLabel) <- function(x, ...) {
  cli::cli_text("<{.emph DTAColumnSpecRowLabel}>")
  print(S7::convert(x, DTAColumnSpec))
  cli::cli_alert("header     : {dta_row_label_header_words(x)}")
  invisible(x)
}

# What the header line holds above a row-label column: NULL (the id), a string
# or FALSE (no field). A plain DTAColumnSpec is a row label that says nothing.
dta_row_label_header <- function(col) {
  if (S7::S7_inherits(col, DTAColumnSpecRowLabel)) col@header else NULL
}

# The same in words, for print() and the exported document.
dta_row_label_header_words <- function(col) {
  h <- dta_row_label_header(col)
  if (identical(h, FALSE)) {
    "no field"
  } else if (is.null(h)) {
    col@id
  } else if (nzchar(trimws(h))) {
    h
  } else {
    "empty"
  }
}

#' @title Row-Label Columns From a `row_labels:` List
#' @description
#' Builds the column specifications of a matrix dataset from the list a YAML
#' \code{row_labels:} block is read as. Each entry is a column specification
#' that may also carry \code{header}.
#' @param row_labels A list of column specification lists.
#' @param rules A list of rules, or \code{NULL}.
#' @return A \code{DTAColumnSpecCollection} whose columns are
#'   \code{DTAColumnSpecRowLabel} objects.
#' @keywords internal
dta_row_label_specs <- function(row_labels, rules = list()) {
  entries <- row_labels %||% list()
  if (!is.list(entries)) {
    cli_abort("{.field row_labels} must be a list.", class = "dta_matrix_invalid_row_labels")
  }
  headers <- lapply(entries, function(e) if (is.list(e)) e[["header"]])
  bare <- lapply(entries, function(e) {
    if (is.list(e)) e[["header"]] <- NULL
    e
  })
  specs <- specs_from_list(columns = bare, rules = rules)
  columns <- specs@columns
  for (i in seq_along(columns)) {
    columns[[i]] <- DTAColumnSpecRowLabel(columns[[i]], header = headers[[i]])
  }
  specs@columns <- columns
  specs
}
