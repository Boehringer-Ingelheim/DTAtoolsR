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
#' @param columns Character vector. Column name(s) checked by the range rule.
#' @param range Numeric vector or list. Legacy argument for range checks.
#' @param description Character or NULL. Free-text description of the rule.
#' @param min Numeric or NULL. Lower bound of the allowed range.
#' @param max Numeric or NULL. Upper bound of the allowed range.
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
        type = "check_range",
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
    if (!is.null(self@id) && (length(self@id) != 1 || !is.character(self@id) || any(grepl("\\s", self@id)))) {
      cli_abort("@id cannot have whitespaces and needs to be defined.")
    }

    if (!self@type %in% c("check_range", "col_range")) {
      cli_abort("'type' must be 'check_range' or 'col_range'.")
    }

    if (is.null(self@columns) || length(self@columns) < 1) {
      cli_abort("A 'columns' must be set.")
    }

    if (!is.numeric(self@min) || !is.numeric(self@max)) {
      cli_abort("Min and max range must be numeric.")
    }

    if (self@min >= self@max) {
      cli_abort("Min range must be less than max.")
    }
  }
)


#' @title print
#' @description
#' Print overview for DTARuleColRange
#' @param x An object of class DTARuleColRange
#' @importFrom cli cli_alert_info cli_alert
#' @examples
#' rule <- create_example_DTARuleColRange()
#' print(rule)
#' @name print
#' @export
method(print, DTARuleColRange) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTARuleColRange}> : {.field {x@id}}")
  if(!is.null(x@description)) cli_text("{x@description}")
  cli_alert_info("columns(s): {paste(x@columns, collapse = ', ')}")
  cli_alert("min: {x@min}")
  cli_alert("max: {x@max}")
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
