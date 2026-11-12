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
#' @param columns Character. The single column the range applies to. The
#'   property is a character vector for consistency with the other rule
#'   classes, but a range rule must name exactly one column; more than one is
#'   an error (class `dta_rule_range_columns`) when the rule is constructed.
#' @param range Numeric vector or list of length 2 (lower, upper). Legacy
#'   argument for range checks; it is used only when both \code{min} and
#'   \code{max} are \code{NULL}, and is ignored otherwise.
#' @param description Character or NULL. Free-text description of the rule.
#' @param min Numeric. Lower bound of the allowed range. Required unless
#'   \code{range} is given, and must be less than \code{max}.
#' @param max Numeric. Upper bound of the allowed range. Required unless
#'   \code{range} is given, and must be greater than \code{min}.
#' @return An object of class `DTARuleColRange`.
#'
#' @examples
#' # Create a col_range rule
#' rule1 <- DTAtools::DTARuleColRange(
#'   id = "rule1",
#'   columns = "age",
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
    columns = NULL,
    range = NULL,
    description = NULL,
    min = NULL,
    max = NULL
  ) {
    if (is.null(columns) || length(columns) < 1) {
      cli_abort("A 'columns' must be set.")
    }
    if (is.list(columns)) {
      columns <- unlist(columns)
    }

    if (!is.null(range) && is.null(min) && is.null(max)) {
      range_vec <- unlist(range)
      if (!is.numeric(range_vec) || length(range_vec) != 2) {
        cli_abort("'range' must be a numeric vector of length 2.")
      }
      min <- range_vec[[1]]
      max <- range_vec[[2]]
    }

    # Create the class object
    new_object(
      .parent = DTAtools::DTARule(
        id = id,
        type = "col_range",
        description = description
      ),
      columns = columns,
      min = min,
      max = max
    )
  },

  # Define the properties of the class
  properties = list(
    id = class_character, # Unique identifier for the rule
    type = class_character, # Type of the rule
    columns = class_character, # Column(s) the rule applies to
    min = class_numeric, # Minimum value of the range
    max = class_numeric # Maximum value of the range
  ),
  validator = function(self) {
    # The id is validated once, by the DTARule parent validator S7 runs first.

    if (!identical(self@type, "col_range")) {
      cli_abort("{.arg type} must be {.val col_range}.")
    }

    if (is.null(self@columns) || length(self@columns) < 1) {
      cli_abort("A 'columns' must be set.")
    }

    if (length(self@columns) != 1L) {
      cli_abort(
        "A range rule names exactly one column, not {length(self@columns)}.",
        class = "dta_rule_range_columns"
      )
    }

    if (!is.numeric(self@min) || !is.numeric(self@max)) {
      cli_abort("Min and max range must be numeric.")
    }

    if (length(self@min) != 1L || length(self@max) != 1L || is.na(self@min) || is.na(self@max)) {
      cli_abort(
        "Min and max must each be a single, non-missing number.",
        class = "dta_rule_invalid_range"
      )
    }

    if (self@min >= self@max) {
      cli_abort("Min range must be less than max.")
    }
  }
)


#' @importFrom cli cli_alert_info cli_alert
#' @name print
#' @export
method(print, DTARuleColRange) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTARuleColRange}> : {.field {x@id}}")
  if (!is.null(x@description)) cli_text("{x@description}")
  cli_alert_info("columns(s): {paste(x@columns, collapse = ', ')}")
  cli_alert("min: {x@min}")
  cli_alert("max: {x@max}")
}


#' @title Create an Example Column-Range Rule
#' @description
#' create example for DTARuleColRange
#' @param index rule selector
#' @importFrom cli cli_abort
#' @examples
#' create_example_DTARuleColRange()
#' @name create_example_DTARuleColRange
#' @return An example \code{DTARuleColRange} object.
#' @export
create_example_DTARuleColRange <- function(index = 1) {
  # nolint
  if (index == 1) {
    return(DTAtools::DTARuleColRange(
      id = "check_age_range",
      columns = "AGE",
      min = 18,
      max = 65
    ))
  } else {
    cli::cli_abort("No example found with index {index}.")
  }
}

# check() method for DTARuleColRange; documented at ?check (generic).
method(check, DTARuleColRange) <- function(x, tab) {
  cli::cli_abort(
    "{.fn check} is not implemented for rules; use {.fn validate_table} on a table instead.",
    class = "dta_rule_check_not_implemented"
  )
}

#' @export
#' @name as.list
method(as.list, DTARuleColRange) <- function(x, ...) {
  list(
    id = x@id,
    type = x@type,
    description = x@description,
    columns = x@columns,
    min = x@min,
    max = x@max
  )
}
