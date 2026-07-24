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
#' @param type Optional character value kept for backward compatibility.
#'   If provided, it must be either "check_unique" or "col_unique".
#' @param columns Character vector. Columns that will be collectively checked
#'   for uniqueness throughout the table.
#' @return An object of class `DTARuleColUnique`.
#'
#' @examples
#' # Create a check_unique rule
#' rule2 <- DTAtools::DTARuleColUnique(
#'   id = "rule2",
#'   columns = "id"
#' )
#' @include DTARule-class.R
DTARuleColUnique <- S7::new_class(
  "DTARuleColUnique",
  parent = DTARule,

  constructor = function(
    id,
    type = NULL,
    columns = NULL,
    description = NULL
  ) {
    if (is.null(columns) || length(columns) < 1) {
      cli_abort("'columns' must be a non-empty list of column names.")
    }

    if (!is.null(type) && !type %in% c("check_unique", "col_unique")) {
      cli_abort("'type' must be NULL, 'check_unique', or 'col_unique'.")
    }

    if(is.list(columns)) {
      columns <- unlist(columns)
    }
    if(!is.character(columns)) {
      cli_abort("'columns' must be a character vector or list of column names.")
    }
    
    # Create the class object
    new_object(
      .parent = DTAtools::DTARule(
        id = id,
        type = "check_unique",
        description = description
      ),
      columns = columns
    )
  },

  # Define the properties of the class
  properties = list(
    id = class_character, # Unique identifier for the rule
    type = class_character, # Type of the rule
    columns = class_character
  ),
  validator = function(self) {
    if (!is.null(self@id) && (length(self@id) != 1 || !is.character(self@id) || any(grepl("\\s", self@id)))) {
      cli_abort("@id cannot have whitespaces and needs to be defined.")
    }

    if (!self@type %in% c("check_unique", "col_unique")) {
      cli_abort("'type' must be 'check_unique' or 'col_unique'.")
    }

    if (is.null(self@columns) || length(self@columns) < 1) {
      cli_abort("'columns' must be a non-empty list of column names.")
    }
  }
)


#' @title print
#' @description
#' Print overview for DTARuleColUnique
#' @param x An object of class DTARuleColUnique
#' @importFrom cli cli_alert_info cli_alert cli_text cli_div
#' @examples
#' rule <- create_example_DTARuleColUnique()
#' print(rule)
#' @name print
#' @export
method(print, DTARuleColUnique) <- function(x) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTARuleColUnique}> : {.field {x@id}}")
  if(!is.null(x@description)) cli_text("{x@description}")

  message <- paste0(
    "column(s): ",
    paste(paste0("{.field ", x@columns, "}"), collapse = ", ")
  )
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
create_example_DTARuleColUnique <- function(index = 1) {
  # nolint
  if (index == 1) {
    return(DTAtools::DTARuleColUnique(
      id = "rule_unique1",
      columns = "SUBJID"
    ))
  } else if (index == 2) {
    return(DTAtools::DTARuleColUnique(
      id = "rule_unqiue2",
      columns = c("SUBJID", "VISIT")
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
#'  # Example check method call:
#'  # check(rule, tab)
#' @name check
#' @export
method(check, DTARuleColUnique) <- function(x, tab) {
  # nolint

  if (!inherits(tab, "Table")) {
    cli::cli_abort("The 'tab' argument must be an arrow Table.")
  }

  # TODO from here
}

#' @title as.list for DTARuleColUnique
#' @description
#' Convert a DTARuleColUnique object to a list.
#' @param x An object of class DTARuleColUnique
#' @param ... Additional arguments (not used).
#' @return A named list containing the properties of the DTARuleColUnique object.
#' @export
#' @name as.list
#' @rdname as.list-DTARuleColUnique
method(as.list, DTARuleColUnique) <- function(x) {
  list(
    id = x@id,
    type = x@type,
    description = x@description,
    columns = x@columns
  )
}
