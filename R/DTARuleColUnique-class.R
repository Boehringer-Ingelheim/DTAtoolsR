#' @title DTARuleColUnique Class
#' @description
#' Represents a single rule for validating data tables. The rule can be of various types,
#' such as `check_range`, `check_unique`, or `check_col_condition`
#'
#' @import S7
#' @importFrom cli cli_abort
#' @export
#'
#' @param id Character. A unique identifier for the rule.
#' @param column list of columns that will be collectively checked if the
#' combinations are unique throughout the table
#' @return An object of class `DTARule`.
#'
#' @examples
#' # Create a check_unique rule
#' rule2 <- DTAtools::DTARuleColUnique(
#'   id = "rule2",
#'   columns = "id"
#' )
#' @include DTARule-class.R
DTARuleColUnique <- new_class(
  "DTARuleColUnique",
  parent = DTARule,

  constructor = function(
    id,
    type,
    columns = NULL
  ) {
    new_object(
      .parent = DTAtools::DTARule(
        id = id,
        type = "check_unique"
      ),
      columns = columns
    )
  },

  # Define the properties of the class
  properties = list(
    id = class_character, # Unique identifier for the rule
    type = class_character, # Type of the rule
    columns = class_character_or_list
  ),
  validator = function(self) {
    if (any(grepl(self@id, pattern = "\\s") || is.null(self@id))) {
      "@id cannot have whitespaces and needs to be defined."
    }

    if (!self@type == "check_unique") {
      "'type' must be 'check_unique'."
    }

    if (is.null(self@columns) || length(self@columns) < 1) {
      "'columns' must be a non-empty list of column names."
    }
  }
)


#' @title print
#' @description
#' Print overview for DTARule
#' @param x An object of class DTARule
#' @importFrom cli cli_alert_info cli_alert cli_text cli_div
#' @examples
#' \dontrun{
#'  print(rule)
#' }
#' @name print
#' @export
method(print, DTARule) <- function(x) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTARuleColUnique}> : {.field {x@id}}")

  message <- paste0("column(s): ", 
                      paste(paste0("{.field ", x@columns, "}"), 
                          collapse = ", "))
  cli_text(message)
}

#' @title create_example_DTARuleColUnique
#' @description
#' create example for DTARuleColUnique
#' @param index rule selector
#' @importFrom cli cli_abort
#' @examples
#'  library(DTAtools)
#'  create_example_DTARuleColUnique()
#' @name create_example_DTARuleColUnique
#' @export
create_example_DTARuleColUnique <- function(index = 1) { # nolint
  if (index == 1) {
    return(DTAtools::DTARuleColUnique(
      id = "rule_unique1",
      type = "check_unique",
      column = "id"
    ))
  } else if (index == 2) {
    return(DTAtools::DTARuleColUnique(
      id = "rule_unqiue2",
      type = "check_unique",
      column = c("id", "visit")
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
method(check, DTARuleColUnique) <- function(x, tab) { # nolint

  if (!inherits(tab, "Table")) {
    cli::cli_abort("The 'tab' argument must be an arrow Table.")
  }

  # TODO from here
}

