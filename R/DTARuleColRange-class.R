#' @title DTARuleColRange Class
#' @description
#' Represents a rule for checking the range of values in a specific column of
#'  a data table.
#'
#' @import S7
#' @importFrom cli cli_abort
#' @export
#'
#' @param id Character. A unique identifier for the rule.
#' @param type Character. The rule type (e.g., "check_range", "check_unique").
#' @param column List. Used in check_range and check_unique. For check_unique,
#'   it is a list of columnss checked for unique combinations.
#' @param range Vector or List. Used in check_range to check value ranges in a
#'   columns.
#' @return An object of class `DTARuleColRange`.
#'
#' @examples
#' # Create a check_range rule
#' rule1 <- DTAtools::DTARuleColRange(
#'   id = "rule1",
#'   column = "age",
#'   range = c(18, 65)
#' )
#' @include DTARule-class.R
DTARuleColRange <- S7::new_class(
  # nolint: object_name_linter.
  "DTARuleColRange",
  parent = DTARule,
  # Constructor for the DTARuleColRange class
  constructor = function(
    id,
    column = NULL,
    range = NULL,
    description = NULL
  ) {
    type <- "check_range"

    if (is.list(range)) {
      range <- unlist(range)
    }

    if (
      !is.numeric(range) ||
        length(range) != 2
    ) {
      cli::cli_abort(
        "'range' must be a vector of two non-negative numbers (min and max)."
      )
    }

    min_range <- range[1]
    max_range <- range[2]

    # Create the class object
    new_object(
      .parent = DTAtools::DTARule(
        id = id,
        type = "col_range",
        description = description
      ),
      column = column,
      min_range = min_range,
      max_range = max_range
    )
  },

  # Define the properties of the class
  properties = list(
    id = class_character, # Unique identifier for the rule
    type = class_character, # Type of the rule
    column = class_character, # Column(s) the rule applies to
    min_range = class_numeric, # Minimum value of the range
    max_range = class_numeric # Maximum value of the range
  ),
  validator = function(self) {
    if (any(grepl(self@id, pattern = "\\s") || is.null(self@id))) {
      "@id cannot have whitespaces and needs to be defined."
    }

    if (!self@type == "check_range") {
      "'type' must be 'check_range'."
    }

    if (is.null(self@column) || length(self@column) < 1) {
      "A 'column' must be set."
    }

    if (!is.numeric(self@min_range) || !is.numeric(self@max_range)) {
      "Min and max range must be numeric."
    }

    if (self@min_range >= self@max_range) {
      "Min range must be less than max."
    }
  }
)


#' @title print
#' @description
#' Print overview for DTARuleColRange
#' @param x An object of class DTARuleColRange
#' @importFrom cli cli_alert_info cli_alert
#' @examples
#' \dontrun{
#'  print(rule)
#' }
#' @name print
#' @export
method(print, DTARuleColRange) <- function(x) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTARuleColRange}> : {.field {x@id}}")

  cli_alert_info("column(s): {paste(x@column, collapse = ', ')}")
  cli_alert("min: {x@min_range}")
  cli_alert("max: {x@max_range}")
}


#' @title create_example_DTARuleColRange
#' @description
#' create example for DTARuleColRange
#' @param index rule selector
#' @importFrom cli cli_abort
#' @examples
#'  library(DTAtools)
#'  create_example_DTARuleColRange()
#' @name create_example_DTARuleColRange
#' @export
create_example_DTARuleColRange <- function(index = 1) {
  # nolint
  if (index == 1) {
    return(DTAtools::DTARuleColRange(
      id = "check_age_range",
      column = "AGE",
      range = list(18, 65)
    ))
  } else {
    cli::cli_abort("No example found with index {index}.")
  }
}

#' @title check
#' @description
#' check rule against data
#' @importFrom cli cli_abort
#' @importFrom arrow Table
#' @examples
#' \dontrun{
#'  # TODO
#' }
#' @name check
#' @export
method(check, DTARuleColRange) <- function(x, tab) {
  # nolint

  if (!inherits(tab, "Table")) {
    cli::cli_abort("The 'tab' argument must be an arrow Table.")
  }

  # TODO from here
}

#' @title as.list for DTARuleColRange
#' @description
#' Convert a DTARuleColRange object to a list.
#' @param x An object of class DTARuleColRange
#' @param ... Additional arguments (not used).
#' @return A named list containing the properties of the DTARuleColRange object.
#' @export
#' @name as.list
#' @rdname as.list-DTARuleColRange
method(as.list, DTARuleColRange) <- function(x) {
  list(
    id = x@id,
    type = x@type,
    description = x@description,
    column = x@column,
    min_range = x@min_range,
    max_range = x@max_range
  )
}
