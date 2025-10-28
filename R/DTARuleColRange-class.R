#' @title DTARuleColRange Class
#' @description
#' Represents a rule for checking the range of values in a specific columns of
#'  a data table.
#'
#' @import S7
#' @importFrom cli cli_abort
#' @export
#'
#' @param id Character. A unique identifier for the rule.
#' @param type Character. The rule type (e.g., "check_range", "check_unique").
#' @param columns Charaterc. columns checked for unique combinations.
#' @param range Vector or List. Used in check_range to check value ranges in a
#'   columns.
#' @return An object of class `DTARuleColRange`.
#'
#' @examples
#' # Create a check_range rule
#' rule1 <- DTAtools::DTARuleColRange(
#'   id = "rule1",
#'   columns = "age",
#'   range = c(18, 65)
#' )
#' @include DTARule-class.R
DTARuleColRange <- new_class( # nolint: object_name_linter.
  "DTARuleColRange",
  parent = DTARule,
  # Constructor for the DTARuleColRange class
  constructor = function(
    id,
    columns = NULL,
    range = NULL,
    description = NULL
  ) {
    type <- "check_range"

    if (is.list(range)) {
      range <- unlist(range)
    }

    if (is.list(columns)) {
      columns <- unlist(columns)
    }

    if (!is.numeric(range) ||
        length(range) != 2) {
      cli::cli_abort("'range' must be a vector of two non-negative numbers (min and max).")
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
      columns = columns,
      min_range = min_range,
      max_range = max_range
    )
  },

  # Define the properties of the class
  properties = list(
    id = class_character, # Unique identifier for the rule
    type = class_character, # Type of the rule
    columns = class_character, # Column(s) the rule applies to
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

    if (is.null(self@columns) || length(self@columns) < 1) {
      "A 'columns' must be set."
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
  
  cli_alert_info("columns(s): {paste(x@columns, collapse = ', ')}")
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
create_example_DTARuleColRange <- function(index = 1) { # nolint
  if (index == 1) {
    return(DTAtools::DTARuleColRange(
      id = "check_age_range",
      columns = "AGE",
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
method(check, DTARuleColRange) <- function(x, tab) { # nolint

  if (!inherits(tab, "Table")) {
    cli::cli_abort("The 'tab' argument must be an arrow Table.")
  }

  # TODO from here
}

#' @title as.list for DTARuleColCondition
#' @description
#' Convert a DTARuleColCondition object to a list.
#' @param x An object of class DTARuleColCondition
#' @param ... Additional arguments (not used).
#' @return A named list containing the properties of the DTARuleColCondition object.
#' @export
as.list.DTARuleColCondition <- function(x, ...) {
  list(
    id = x@id,
    type = x@type,
    columns = x@columns,
    min_range = x@min_range,
    max_range = x@max_range
  )
}
