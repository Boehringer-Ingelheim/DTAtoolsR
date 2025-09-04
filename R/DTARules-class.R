#' @title DTARules Class
#' @description
#' The `DTARules` class represents a collection of rules for validating data tables.
#' It provides functionality to define, store, and validate rules using a structured schema.
#' This class is designed to be used in data validation workflows, particularly for ensuring
#' compliance with predefined rules.
#'
#' @import S7
#' @importFrom cli cli_abort cli_alert_success
#' @export
#'
#' @param rules A list of DTARule objects.
#' @return An object of class `DTARules`.
#'
#' @examples
#' # Define some example rules
#' example_rules <- list(
#'   list(
#'     id = "rule1",
#'     type = "check_range",
#'     column = "age",
#'     range = c(18, 65)
#'   ),
#'   list(
#'     id = "rule2",
#'     type = "check_unique",
#'     column = "id"
#'   )
#' )
#'
#' # Create a DTARules object
#' rules_obj <- DTARules(
#'   rules = example_rules
#' )
#'
#' # Print the object
#' print(rules_obj)

DTARules <- new_class(
  "DTARules",

  # Constructor for the DTARules class
  constructor = function(rules) {
    if (!all(sapply(rules, inherits, "DTARule"))) {
      cli::cli_abort(
        "All elements in 'columns' must be of class 'DTARule'"
      )
    }
    # Create the class object
    new_object(
      S7_object(),
      rules = rules
    )
  },

  # Define the properties of the class
  properties = list(
    rules = class_list # A list of rule definitions
  )
)
