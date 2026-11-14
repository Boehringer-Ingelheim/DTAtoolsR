#' @title DTAMatrixSpec Class
#' @description
#' The settings of a matrix dataset: what the header must look like, how many
#' data columns and rows there may be, which row-label column is the key, and
#' what a cell may hold. Internal; written in a specification as the
#' \code{matrix:} block and read by \code{dta_matrix_spec_from_list()}.
#' @param header A \code{\link{DTAColumnSpec}} (id \code{"header"}, type
#'   \code{SAS Char}, not nullable) every data-column label must satisfy.
#' @param corner_mode \code{"named"} (the header text above the row labels is
#'   the row-label ids), \code{"blank"} (all empty) or \code{"list"} (the
#'   entries of \code{corner}).
#' @param corner Character. The expected corner fields for \code{"list"};
#'   \code{NA} (a YAML \code{null}) is allowed as the first entry only and means
#'   the header has no field there. Empty otherwise.
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
    corner_mode = S7::class_character,
    corner = S7::class_character,
    n_columns = S7::class_numeric,
    row_key = class_character_or_null,
    n_rows = S7::class_numeric,
    cells = DTAColumnSpecCell
  ),
  validator = function(self) {
    if (!identical(self@header@id, "header")) {
      cli_abort("The matrix header spec must have the id {.val header}.", class = "dta_matrix_header_invalid")
    }
    if (!identical(dta_cell_type(self@header), "Char") || !identical(self@header@nullable, FALSE)) {
      cli_abort(
        "The matrix header spec must be a non-nullable {.val SAS Char} column.",
        class = "dta_matrix_header_invalid"
      )
    }
    if (length(self@corner_mode) != 1L || !(self@corner_mode %in% c("named", "blank", "list"))) {
      cli_abort(
        "{.field corner_mode} must be one of {.val named}, {.val blank} or {.val list}.",
        class = "dta_matrix_corner_invalid"
      )
    }
    if (self@corner_mode == "list") {
      if (length(self@corner) == 0L || anyNA(self@corner[-1L])) {
        cli_abort(
          "{.field corner} needs at least one entry, and only the first may be {.code null}.",
          class = "dta_matrix_corner_invalid"
        )
      }
    } else if (length(self@corner) != 0L) {
      cli_abort(
        "{.field corner} is only used with {.val list}.",
        class = "dta_matrix_corner_invalid"
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

# yaml::read_yaml() returns a one-element sequence as a scalar, so
# `corner: [named]` cannot be told from `corner: named`. A corner whose text
# really is "named" or "blank" with a single row label must therefore use the
# `named` keyword with that id.
dta_matrix_corner <- function(x) {
  bad <- function() {
    cli_abort(
      c(
        "{.field matrix.header.corner} must be {.val named}, {.val blank} or a list of header texts.",
        "i" = "Quote every text: YAML reads {.code [1]} as a number and {.code [yes]} as TRUE.",
        "i" = "{.code null} is allowed as the first entry only."
      ),
      class = "dta_matrix_corner_invalid"
    )
  }
  # A one-entry R list is the same corner as the YAML scalar.
  if (is.list(x) && length(x) == 1L && is.character(x[[1L]]) && length(x[[1L]]) == 1L &&
    !is.na(x[[1L]]) && x[[1L]] %in% c("named", "blank")) {
    x <- x[[1L]]
  }
  if (is.character(x) && length(x) == 1L && !is.na(x) && x %in% c("named", "blank")) {
    return(list(mode = x, corner = character(0)))
  }
  entries <- if (is.list(x)) x else as.list(x)
  if (length(entries) == 0L) {
    bad()
  }
  out <- vapply(seq_along(entries), function(i) {
    e <- entries[[i]]
    if (is.null(e)) {
      if (i != 1L) bad()
      return(NA_character_)
    }
    if (!is.character(e) || length(e) != 1L || is.na(e)) bad()
    e
  }, character(1))
  list(mode = "list", corner = out)
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
  x <- dta_matrix_block(x, "matrix", c("header", "rows", "cells"), c("header", "cells"))
  hd <- dta_matrix_block(
    x$header, "matrix.header",
    c("corner", "pattern", "values", "length", "label", "description", "count"),
    "corner"
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

  corner <- dta_matrix_corner(hd$corner)

  # The RAW list, before DTAColumnSpec() flattens it: by then 007 is 7 and Y TRUE.
  if (!is.null(hd$values)) {
    texts <- if (is.list(hd$values)) hd$values else as.list(hd$values)
    # An empty permitted set would forbid every label.
    if (length(texts) == 0L || !all(vapply(texts, function(v) is.character(v) && length(v) == 1L, logical(1)))) {
      cli_abort(
        c(
          "{.field matrix.header.values} must be quoted text.",
          "i" = "Quote the labels: YAML reads {.code 007} as 7 and {.code Y} as TRUE."
        ),
        class = "dta_matrix_header_values_text"
      )
    }
  }
  header <- DTAColumnSpec(
    id = "header", type = "SAS Char", nullable = FALSE,
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
    corner_mode = corner$mode,
    corner = corner$corner,
    n_columns = dta_matrix_count(hd$count %||% list(), "matrix.header.count", 1, 1),
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

dta_drop_null <- function(x) x[!vapply(x, is.null, logical(1))]

method(as.list, DTAMatrixSpec) <- function(x, ...) {
  corner <- if (x@corner_mode != "list") {
    x@corner_mode
  } else if (anyNA(x@corner)) {
    lapply(x@corner, function(e) if (is.na(e)) NULL else e)
  } else {
    x@corner
  }
  h <- x@header
  header <- list(
    corner = corner,
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
    header = dta_drop_null(header),
    rows = dta_drop_null(list(key = x@row_key, count = dta_matrix_count_list(x@n_rows))),
    cells = dta_drop_null(cells)
  )
}

method(print, DTAMatrixSpec) <- function(x, ...) {
  corner <- if (x@corner_mode == "list") {
    paste(ifelse(is.na(x@corner), "null", x@corner), collapse = ", ")
  } else {
    x@corner_mode
  }
  key <- if (is.null(x@row_key)) "first row label" else paste(x@row_key, collapse = ", ")
  cli::cli_text("<{.emph DTAMatrixSpec}>")
  cli::cli_alert("corner : {corner}")
  cli::cli_alert("columns: {x@n_columns[[1]]} to {x@n_columns[[2]]}")
  cli::cli_alert("rows   : {x@n_rows[[1]]} to {x@n_rows[[2]]}; key {key}")
  cli::cli_alert("cells  : {dta_cell_type(x@cells)}")
  invisible(x)
}
