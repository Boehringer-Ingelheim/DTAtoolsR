#' @title DTARuleColCondition Class
#' @description
#' A rule for validating data tables with conditions
#'
#' @import S7
#' @importFrom cli cli_abort
#' @export
#'
#' @param id Character. A unique identifier for the rule.
#' @param description Character or NULL. Free-text description of the rule.
#' @param condition List. A list of conditions to check
#' @param then List. A list of conditions that must be true if the
#'   conditions are met
#' @return An object of class `DTARuleColCondition`.
#'
#' @examples
#' # Create a check_range rule
#' DTAtools::DTARuleColCondition(
#'  id = "check_gfreasnd2",
#'  condition = list(
#'    GFREASND = list(
#'      empty = FALSE
#'    )
#'  ),
#'  then = list(
#'    GFSTAT = list(
#'      empty = FALSE
#'    ),
#'    GFORRES = list(
#'      empty = TRUE
#'    )
#'  )
#' )
#' @include DTARule-class.R
DTARuleColCondition <- S7::new_class(
  # nolint: object_name_linter.
  "DTARuleColCondition",
  parent = DTARule,
  # Constructor for the DTARule class
  constructor = function(
    id,
    description = NULL,
    condition = NULL,
    then = NULL
  ) {
    if (is.null(condition) || length(condition) < 1) {
      cli_abort("'condition' must be a non-empty list of conditions.")
    }
    if (is.null(then) || length(then) < 1) {
      cli_abort("'then' must be a non-empty list of conditions.")
    }

    # Create the class object
    new_object(
      DTAtools::DTARule(
        id = id,
        type = "check_col_condition",
        description = description
      ),

      condition = condition,
      then = then
    )
  },

  # Define the properties of the class
  properties = list(
    id = class_character, # Unique identifier for the rule
    type = class_character, # Type of the rule
    condition = class_character_or_list,
    then = class_character_or_list
  ),
  validator = function(self) {
    if (!is.null(self@id) && (length(self@id) != 1 || !is.character(self@id) || any(grepl("\\s", self@id)))) {
      cli_abort("@id cannot have whitespaces and needs to be defined.")
    }
    if (!self@type %in% c("check_col_condition", "col_condition")) {
      cli_abort("'type' must be 'check_col_condition' or 'col_condition'.")
    }
    if (is.null(self@condition) || length(self@condition) < 1) {
      cli_abort("'condition' must be a non-empty list of conditions.")
    }
    if (is.null(self@then) || length(self@then) < 1) {
      cli_abort("'then' must be a non-empty list of conditions.")
    }
  }
)


#' @title print
#' @description
#' Print overview for DTADTARuleColConditionRule
#' @param x An object of class DTARuleColCondition
#' @importFrom cli cli_alert_info cli_alert cli_text
#' @examples
#' rule <- create_example_DTARuleColCondition()
#' print(rule)
#' @name print
#' @export
method(print, DTARuleColCondition) <- function(x, ...) {
  # nolint
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTARuleColCondition}> : {.field {x@id}}")
  if(!is.null(x@description)) cli_text("{x@description}")

  # TODO: check more complicated scenarios
  if (is.list(x@condition)) {
    for (i in 1:length(x@condition)) {
      for (nm in names(x@condition[[i]])) {
        cli_alert_info("  {.field {nm}}: {format(x@condition[[i]][[nm]])}")
      }
    }
  } else {
    cli_alert_info("  {format(x@condition)}")
  }

  cli_alert("then:")
  # TODO: check more complicated scenarios
  if (is.list(x@then)) {
    for (nm in names(x@then)) {
      cli_text("  {.field {nm}}: {format(x@then[[nm]])}")
    }
  } else {
    cli_text("  {format(x@then)}")
  }
}


#' @title create_example_DTARuleColCondition
#' @description
#' create example for DTARule
#' @param index rule selector
#' @importFrom cli cli_abort
#' @examples
#'  library(DTAtools)
#'  create_example_DTARuleColCondition()
#' @name create_example_DTARuleColCondition
#' @export
create_example_DTARuleColCondition <- function(index = 1) {
  # nolint
  if (index == 1) {
    return(DTAtools::DTARuleColCondition(
      id = "rule3",
      condition = list(age = list(equals = 18)),
      then = list(status = list(equals = "adult"))
    ))
  } else {
    cli::cli_abort("Invalid index: {index}. Must be 1.")
  }
}


# check() method for DTARuleColCondition; documented at ?check (generic).
method(check, DTARuleColCondition) <- function(x, tab) {
  # nolint

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
#' @name as.list
method(as.list, DTARuleColCondition) <- function(x, ...) {
  list(
    id = x@id,
    type = x@type,
    description = x@description,
    condition = x@condition,
    then = x@then
  )
}
