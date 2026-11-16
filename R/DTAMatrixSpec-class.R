#' @title DTAMatrixSpec Class
#' @description
#' The settings of a matrix dataset: what the labels of the data columns must
#' look like, how many data columns and rows there may be, which row-label
#' column is the key, and what a cell may hold. Internal; written in a
#' specification as the \code{matrix:} block (\code{columns}, \code{rows},
#' \code{cells}) and read by \code{dta_matrix_spec_from_list()}. The header text
#' above a row-label column belongs to that column (\code{header} of a
#' \code{row_labels} entry), not to this object.
#' @param header A \code{\link{DTAColumnSpec}} (id \code{"matrix.columns"}, type
#'   \code{SAS Char}, not nullable) every data-column label must satisfy: the
#'   \code{label}, \code{description}, \code{pattern}, \code{values} and
#'   \code{length} of \code{matrix.columns}.
#' @param n_columns Numeric of length 2: the minimum (at least 1) and maximum
#'   number of data columns; the maximum may be \code{Inf}.
#' @param row_key Character or \code{NULL}. The row-label id(s) that identify a
#'   row; \code{NULL} is the first row label, resolved by the dataset.
#' @param n_rows Numeric of length 2: the minimum (at least 0) and maximum
#'   number of data rows.
#' @param cells A \code{DTAColumnSpecCell}: what every cell may hold.
#' @keywords internal
#' @noRd
#' @include DTAColumnSpecCell-class.R
DTAMatrixSpec <- S7::new_class(
  "DTAMatrixSpec",
  properties = list(
    header = DTAColumnSpec,
    n_columns = S7::class_numeric,
    row_key = class_character_or_null,
    n_rows = S7::class_numeric,
    cells = DTAColumnSpecCell
  ),
  validator = function(self) {
    if (!identical(self@header@id, "matrix.columns")) {
      cli_abort("The spec of the data-column labels must have the id {.val matrix.columns}.", class = "dta_matrix_header_invalid")
    }
    if (!identical(dta_cell_type(self@header), "Char") || !identical(self@header@nullable, FALSE)) {
      cli_abort(
        "The matrix header spec must be a non-nullable {.val SAS Char} column.",
        class = "dta_matrix_header_invalid"
      )
    }
    lowest <- c(n_columns = 1, n_rows = 0)
    for (nm in names(lowest)) {
      v <- S7::prop(self, nm)
      finite_v <- v[is.finite(v)]
      if (length(v) != 2L || anyNA(v) || v[[1L]] < lowest[[nm]] || v[[1L]] > v[[2L]] ||
        any(finite_v != trunc(finite_v)) || v[[1L]] == Inf) {
        cli_abort(
          "{.field {nm}} must be two whole numbers, minimum {lowest[[nm]]} or more, minimum not above maximum.",
          class = "dta_matrix_count_invalid"
        )
      }
    }
    if (!is.null(self@row_key) && (length(self@row_key) == 0L || anyNA(self@row_key))) {
      cli_abort("{.field row_key} must name at least one row-label id.", class = "dta_matrix_field_invalid")
    }
    if (!(dta_cell_type(self@cells) %in% c("Num", "Int", "Char"))) {
      cli_abort(
        "{.field cells.type} must be {.val SAS Num}, {.val SAS Int} or {.val SAS Char}.",
        class = "dta_matrix_cells_type"
      )
    }
  }
)

# ---- YAML -> object -----------------------------------------------------------

# A block of the raw `matrix:` mapping, as yaml::read_yaml() returns it. Every
# key is checked against a closed list, the way `files:` is: a misspelt `rnage`
# would otherwise be dropped and the range silently never enforced.
dta_matrix_block <- function(x, path, allowed, required = character(0)) {
  if (!is.list(x) || (length(x) > 0 && (is.null(names(x)) || any(!nzchar(names(x)))))) {
    cli_abort(
      "{.field {path}} must be a mapping of {.val {allowed}}.",
      class = "dta_matrix_field_unknown"
    )
  }
  unknown <- setdiff(names(x), allowed)
  if (length(unknown) > 0) {
    cli_abort(
      c(
        "Unknown key {.field {paste0(path, '.', unknown[[1L]])}}.",
        "i" = "Allowed under {.field {path}}: {.val {allowed}}."
      ),
      class = "dta_matrix_field_unknown"
    )
  }
  for (key in required) {
    if (is.null(x[[key]])) {
      cli_abort(
        "{.field {paste0(path, '.', key)}} is required.",
        class = "dta_matrix_field_missing"
      )
    }
  }
  x
}

dta_matrix_count <- function(x, path, default_min, lowest) {
  bad <- function() {
    cli_abort(
      c(
        "{.field {path}} must be a whole number of at least {lowest}, or a mapping with {.code min} and/or {.code max}.",
        "i" = "{.code min} must not exceed {.code max}."
      ),
      class = "dta_matrix_count_invalid"
    )
  }
  whole <- function(v, allow_inf) {
    is.numeric(v) && length(v) == 1L && !is.na(v) && v >= lowest &&
      ((allow_inf && is.infinite(v)) || (is.finite(v) && v == trunc(v)))
  }
  if (is.list(x)) {
    x <- dta_matrix_block(x, path, c("min", "max"))
    dta_matrix_text_number(x$min, paste0(path, ".min"))
    dta_matrix_text_number(x$max, paste0(path, ".max"))
    lo <- x$min %||% default_min
    hi <- x$max %||% Inf
    if (!whole(lo, FALSE) || !whole(hi, TRUE)) bad()
  } else {
    dta_matrix_text_number(x, path)
    if (!whole(x, FALSE)) bad()
    lo <- hi <- x
  }
  if (lo > hi) bad()
  c(as.numeric(lo), as.numeric(hi))
}

# YAML 1.1 reads 1e3 as text, which as.numeric() still parses: abort on it, so a
# written number is never silently dropped or misread.
dta_matrix_text_number <- function(x, path) {
  if (is.character(x) && length(x) == 1L && !is.na(suppressWarnings(as.numeric(x)))) {
    cli_abort(
      c(
        "{.field {path}} was read as the text {.val {x}}.",
        "i" = "YAML 1.1 reads {.code 1e3} as text: write {.code 1.0e+3} or {.code 1000}."
      ),
      class = "dta_matrix_number_text"
    )
  }
  invisible(x)
}

dta_matrix_bound <- function(x, path) {
  dta_matrix_text_number(x, path)
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    cli_abort(
      "{.field {path}} must be a single number.",
      class = "dta_cell_range_invalid"
    )
  }
  # An infinite bound bounds nothing: an open side is written by omitting it.
  if (is.infinite(x)) {
    cli_abort(
      c(
        "{.field {path}} must be finite.",
        "i" = "Leave the side out to leave it open."
      ),
      class = "dta_matrix_range_invalid"
    )
  }
  as.numeric(x)
}

#' @title Read a `matrix:` Block
#' @param x The block as a list, as `yaml::read_yaml()` returns it.
#' @return A `DTAMatrixSpec`.
#' @keywords internal
dta_matrix_spec_from_list <- function(x) {
  x <- dta_matrix_block(x, "matrix", c("columns", "rows", "cells"), "cells")
  hd <- dta_matrix_block(
    x$columns %||% list(), "matrix.columns",
    c("label", "description", "pattern", "values", "length", "count")
  )
  rows <- dta_matrix_block(x$rows %||% list(), "matrix.rows", c("key", "count"))
  cl <- dta_matrix_block(
    x$cells, "matrix.cells",
    c(
      "type", "nullable", "values", "pattern", "length", "format", "label",
      "description", "range", "infinite"
    ),
    "type"
  )

  # The RAW list, before DTAColumnSpec() flattens it: by then 007 is 7 and Y TRUE.
  if (!is.null(hd$values)) {
    texts <- if (is.list(hd$values)) hd$values else as.list(hd$values)
    # An empty permitted set would forbid every label.
    if (length(texts) == 0L || !all(vapply(texts, function(v) is.character(v) && length(v) == 1L, logical(1)))) {
      cli_abort(
        c(
          "{.field matrix.columns.values} must be quoted text.",
          "i" = "Quote the labels: YAML reads {.code 007} as 7 and {.code Y} as TRUE."
        ),
        class = "dta_matrix_columns_values_text"
      )
    }
  }
  header <- DTAColumnSpec(
    id = "matrix.columns", type = "SAS Char", nullable = FALSE,
    label = hd$label, description = hd$description,
    pattern = hd$pattern, values = hd$values, length = hd$length
  )

  rk <- rows$key
  if (!is.null(rk)) {
    # Checked on the RAW list: unlist() would drop a null and coerce 1 to "1".
    entries <- if (is.list(rk)) rk else as.list(rk)
    if (length(entries) == 0L || !all(vapply(
      entries,
      function(e) is.character(e) && length(e) == 1L && !is.na(e) && nzchar(e),
      logical(1)
    ))) {
      cli_abort(
        c(
          "{.field matrix.rows.key} must be a row-label id or a list of ids.",
          "i" = "Quote every id, and remove any {.code null}."
        ),
        class = "dta_matrix_key_invalid"
      )
    }
    rk <- unlist(entries, use.names = FALSE)
  }

  rng <- cl$range
  lo <- hi <- NULL
  # `range: []` and `range: {}` both read as an empty list: no bounds, as if absent.
  if (!is.null(rng)) {
    if (!is.list(rng)) {
      cli_abort(
        "{.field matrix.cells.range} must be a mapping with {.code min} and/or {.code max}.",
        class = "dta_cell_range_invalid"
      )
    }
    rng <- dta_matrix_block(rng, "matrix.cells.range", c("min", "max"))
    if (!is.null(rng$min)) lo <- dta_matrix_bound(rng$min, "matrix.cells.range.min")
    if (!is.null(rng$max)) hi <- dta_matrix_bound(rng$max, "matrix.cells.range.max")
  }

  cell_type <- if (is.character(cl$type) && length(cl$type) == 1L) {
    `__dta_normalize_sas_type__`(sub("^SAS\\s+", "", trimws(cl$type), ignore.case = TRUE))
  } else {
    NA_character_
  }
  infinite <- cl$infinite
  if (!is.null(infinite) && !(is.logical(infinite) && length(infinite) == 1L && !is.na(infinite))) {
    cli_abort("{.field matrix.cells.infinite} must be true or false.", class = "dta_cell_finite_type")
  }
  # Infinite values are rejected unless allowed -- for a Num cell. A written
  # `infinite` on any other type reaches the cell validator and aborts there.
  finite <- if (identical(cell_type, "Num")) !isTRUE(infinite) else if (!is.null(infinite)) !infinite

  cells <- DTAColumnSpecCell(
    id = "cells", type = cl$type, nullable = cl$nullable, values = cl$values,
    pattern = cl$pattern, length = cl$length, format = cl$format,
    label = cl$label, description = cl$description,
    minimum = lo, maximum = hi, finite = finite
  )

  DTAMatrixSpec(
    header = header,
    n_columns = dta_matrix_count(hd$count %||% list(), "matrix.columns.count", 1, 1),
    row_key = rk,
    n_rows = dta_matrix_count(rows$count %||% list(), "matrix.rows.count", 1, 0),
    cells = cells
  )
}

# ---- object -> YAML -----------------------------------------------------------

# A whole number is written as an integer, so saved YAML reads `min: 1`, not
# `min: 1.0`; from_list() converts back to double.
dta_matrix_as_whole <- function(v) {
  if (is.numeric(v) && length(v) == 1L && is.finite(v) && v == trunc(v) && abs(v) < .Machine$integer.max) {
    as.integer(v)
  } else {
    v
  }
}

dta_matrix_count_list <- function(n) {
  if (n[[1L]] == n[[2L]]) {
    dta_matrix_as_whole(n[[1L]])
  } else {
    out <- list(min = dta_matrix_as_whole(n[[1L]]))
    if (is.finite(n[[2L]])) out$max <- dta_matrix_as_whole(n[[2L]])
    out
  }
}

# A count in words: "exactly 96", "at least 1", "at most 384", "1 to 384",
# "any number". Shared by print() and the exported document.
dta_matrix_count_words <- function(n) {
  num <- function(v) format(v, big.mark = "", scientific = FALSE, trim = TRUE)
  lo <- n[[1L]]
  hi <- n[[2L]]
  if (lo == hi) {
    paste("exactly", num(lo))
  } else if (is.infinite(hi)) {
    if (lo == 0) "any number" else paste("at least", num(lo))
  } else if (lo == 0) {
    paste("at most", num(hi))
  } else {
    paste(num(lo), "to", num(hi))
  }
}

dta_drop_null <- function(x) x[!vapply(x, is.null, logical(1))]

method(as.list, DTAMatrixSpec) <- function(x, ...) {
  h <- x@header
  columns <- list(
    label = h@label,
    description = h@description,
    pattern = h@pattern,
    values = h@values,
    length = h@structure@length,
    count = dta_matrix_count_list(x@n_columns)
  )
  cells <- as.list(x@cells)
  cells$id <- NULL
  if (is.list(cells$range)) cells$range <- lapply(cells$range, dta_matrix_as_whole)
  list(
    columns = dta_drop_null(columns),
    rows = dta_drop_null(list(key = x@row_key, count = dta_matrix_count_list(x@n_rows))),
    cells = dta_drop_null(cells)
  )
}

method(print, DTAMatrixSpec) <- function(x, ...) {
  key <- if (is.null(x@row_key)) "first row label" else paste(x@row_key, collapse = ", ")
  cli::cli_text("<{.emph DTAMatrixSpec}>")
  cli::cli_alert("columns: {dta_matrix_count_words(x@n_columns)}")
  cli::cli_alert("rows   : {dta_matrix_count_words(x@n_rows)}; key {key}")
  cli::cli_alert("cells  : {dta_cell_type(x@cells)}")
  invisible(x)
}
