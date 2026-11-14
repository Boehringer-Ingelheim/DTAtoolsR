#' @title DTAColumnSpecCell Class
#' @description
#' The cell specification of a matrix dataset: a \code{\link{DTAColumnSpec}}
#' that also bounds the value (\code{minimum}, \code{maximum}) and can reject
#' infinite values (\code{finite}). Internal; reached through the \code{matrix:}
#' block of a specification.
#' @param id Character. The id of the cell spec; \code{"cells"} by default. The
#'   dataset renames a clone of it to each data column.
#' @param minimum,maximum Numeric or NULL. Inclusive bounds; \code{Num} and
#'   \code{Int} cells only.
#' @param finite Logical or NULL. \code{TRUE} rejects infinite values
#'   (YAML \code{infinite: false}); \code{Num} cells only. \code{NULL} performs
#'   no finiteness check.
#' @inheritParams DTAColumnSpec
#' @return An object of class \code{DTAColumnSpecCell}.
#' @keywords internal
#' @noRd
#' @include DTAColumnSpec-class.R
DTAColumnSpecCell <- S7::new_class(
  "DTAColumnSpecCell",
  parent = DTAColumnSpec,
  constructor = function(
    id = "cells",
    label = NULL,
    type = NULL,
    format = NULL,
    length = NULL,
    nullable = NULL,
    pattern = NULL,
    values = NULL,
    examples = NULL,
    description = NULL,
    colclass = NULL,
    minimum = NULL,
    maximum = NULL,
    finite = NULL
  ) {
    new_object(
      DTAColumnSpec(
        id = id, label = label, type = type, format = format, length = length,
        nullable = nullable, pattern = pattern, values = values,
        examples = examples, description = description, colclass = colclass
      ),
      minimum = minimum,
      maximum = maximum,
      finite = finite
    )
  },
  properties = list(
    minimum = class_numeric_or_null,
    maximum = class_numeric_or_null,
    finite = class_logical_or_null
  ),
  validator = function(self) {
    single <- function(v) length(v) == 1L && !is.na(v)
    for (nm in c("minimum", "maximum")) {
      v <- S7::prop(self, nm)
      if (!is.null(v) && !single(v)) {
        cli_abort(
          "{self@id}: {.field {nm}} must be a single, non-missing number.",
          class = "dta_cell_range_invalid"
        )
      }
    }
    if (!is.null(self@finite) && !single(self@finite)) {
      cli_abort(
        "{self@id}: {.field finite} must be a single TRUE or FALSE.",
        class = "dta_cell_finite_type"
      )
    }
    if (!is.null(self@minimum) && !is.null(self@maximum) && self@minimum > self@maximum) {
      cli_abort(
        "{self@id}: the minimum ({self@minimum}) is above the maximum ({self@maximum}).",
        class = "dta_cell_range_invalid"
      )
    }
    type <- dta_cell_type(self)
    if ((!is.null(self@minimum) || !is.null(self@maximum)) && !(type %in% c("Num", "Int"))) {
      cli_abort(
        "{self@id}: a range applies to {.val SAS Num} and {.val SAS Int} cells only.",
        class = "dta_cell_range_type"
      )
    }
    if (!is.null(self@finite) && !(type %in% "Num")) {
      cli_abort(
        "{self@id}: {.field infinite} applies to {.val SAS Num} cells only.",
        class = "dta_cell_finite_type"
      )
    }
  }
)

method(as_json_schema, DTAColumnSpecCell) <- function(x) {
  schema <- as_json_schema(S7::super(x, DTAColumnSpec))
  schema$minimum <- x@minimum
  schema$maximum <- x@maximum
  if (isTRUE(x@finite)) {
    schema$finite <- TRUE
  }
  schema
}

method(as.list, DTAColumnSpecCell) <- function(x, ...) {
  # S7::super() does not reach an S3 generic such as as.list(); convert() does.
  out <- as.list(S7::convert(x, DTAColumnSpec))
  range <- list(min = x@minimum, max = x@maximum)
  range <- range[!vapply(range, is.null, logical(1))]
  if (length(range) > 0) {
    out$range <- range
  }
  if (identical(dta_cell_type(x), "Num")) {
    out$infinite <- !isTRUE(x@finite)
  }
  out
}

# The bare type ("Num", "Int", "Char", ...) or NA when the spec declares none.
dta_cell_type <- function(x) {
  if (is.null(x@structure) || is.null(x@structure@type)) NA_character_ else x@structure@type
}

method(print, DTAColumnSpecCell) <- function(x, ...) {
  cli::cli_text("<{.emph DTAColumnSpecCell}>")
  print(S7::convert(x, DTAColumnSpec))
  if (!is.null(x@minimum) || !is.null(x@maximum)) {
    lo <- if (is.null(x@minimum)) "-Inf" else x@minimum
    hi <- if (is.null(x@maximum)) "Inf" else x@maximum
    cli::cli_alert("range      : [{lo}, {hi}]")
  }
  if (!is.null(x@finite)) {
    cli::cli_alert("finite     : {x@finite}")
  }
  invisible(x)
}
