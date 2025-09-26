#' @title DTARuleCheckColCondition Class
#' @description
#' A rule for validating data tables with conditions
#'
#' @import S7
#' @importFrom cli cli_abort
#' @export
#'
#' @param id Character. A unique identifier for the rule.
#' @param condition List. A list of conditions to check
#' @param then List. A list of conditions that must be true if the
#'   conditions are met
#' @return An object of class `DTARuleCheckColCondition`.
#'
#' @examples
#' # Create a check_range rule
#' DTAtools::DTARule(
#'  id = "check_gfreasnd2",
#'  condition = list(
#'    GFREASND = list(
#'      empty = false
#'    )
#'  ),
#'  then = list(
#'    GFSTAT = list(
#'      empty = false
#'    ),
#'    GFORRES = list(
#'      empty = true
#'    )
#'  )
#' @include DTARule-class.R
DTARuleCheckColCondition <- new_class( # nolint: object_name_linter.
  "DTARuleCheckColCondition",
  parent = DTARule,
  # Constructor for the DTARule class
  constructor = function(
    id,
    condition = NULL,
    then = NULL
  ) {
    # Create the class object
    new_object(
      DTAtools::DTARule(
        id = id,
        type = "check_col_condition"
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
    if (any(grepl(self@id, pattern = "\\s") || is.null(self@id))) {
      "@id cannot have whitespaces and needs to be defined."
    }
    if (!self@type == "check_col_condition") {
      "'type' must be 'check_col_condition'."
    }
    if (is.null(self@condition) || length(self@condition) < 1) {
      "'condition' must be a non-empty list of conditions."
    }
    if (is.null(self@then) || length(self@then) < 1) {
      "'then' must be a non-empty list of conditions."
    }
  }
)


#' @title print
#' @description
#' Print overview for DTADTARuleCheckColConditionRule
#' @param x An object of class DTARuleCheckColCondition
#' @examples
#' \dontrun{
#'  print(rule)
#' }
#' @name print
#' @export
print <- new_generic("print", "x")

method(print, DTARuleCheckColCondition) <- function(x) { # nolint

  cat(stringr::str_glue("{x@id}:<DTAtools::DTARuleCheckColCondition>\n"))
  cat(stringr::str_glue("- condition: {x@condition}\n"))
  cat(stringr::str_glue("- then: {x@then}"))
}


#' @title create_example_DTARuleCheckColCondition
#' @description
#' create example for DTARule
#' @param index rule selector
#' @importFrom cli cli_abort
#' @examples
#'  library(DTAtools)
#'  create_example_DTARuleCheckColCondition()
#' @name create_example_DTARuleCheckColCondition
#' @export
create_example_DTARuleCheckColCondition <- function(index = 1) { # nolint
  if (index == 1) {
    return(DTAtools::DTARuleCheckColCondition(
      id = "rule3",
      condition = list(list("age" = 18)),
      then = list("status" = list(equal = 'adult'))
    ))
  } else {
    cli::cli_abort("Invalid index: {index}. Must be 1.")
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
method(check, DTARuleCheckColCondition) <- function(x, tab) { # nolint

  if (!inherits(tab, "Table")) {
    cli::cli_abort("The 'tab' argument must be an arrow Table.")
  }

  # TODO from here
}
