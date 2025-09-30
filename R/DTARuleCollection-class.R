#' @title DTARuleCollection Class
#' @description
#' The `DTARuleCollection` class represents a collection of rules for validating data tables.
#' It provides functionality to define, store, and validate rules using a structured schema.
#' This class is designed to be used in data validation workflows, particularly for ensuring
#' compliance with predefined rules.
#'
#' @import S7
#' @importFrom cli cli_abort cli_alert_success
#' @export
#'
#' @param rules A list of DTARule objects.
#' @return An object of class `DTARuleCollection`.
#'
#' @examples
#' # Define some example rules
#' example_rules <- list(
#'   DTARule(
#'     id = "rule1",
#'     type = "check_range",
#'     column = "age",
#'     range = c(18, 65)
#'   ),
#'   DTARule(
#'     id = "rule2",
#'     type = "check_unique",
#'     column = "id"
#'   )
#' )
#'
#' # Create a DTARuleCollection object
#' #rules_obj <- DTARuleCollection(
#' #   rules = example_rules
#' #)
#'
#' # Print the object
#' # print(rules_obj)

DTARuleCollection <- new_class(
  "DTARuleCollection",

  # Constructor for the DTARuleCollection class
  constructor = function(rules) {
    if (!all(sapply(rules, inherits, "DTAtools::DTARule"))) {
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



#' @title create_example_DTARuleCollection
#' @description
#' create example for DTARuleCollection
#' @param index rule selector
#' @importFrom cli cli_abort
#' @examples
#'  library(DTAtools)
#'  create_example_DTARuleCollection()
#' @export
create_example_DTARuleCollection <- function(index = 1) { # nolint
  if (index == 1) {
    return(
      DTAtools::DTARuleCollection(
        rules = list(
          DTAtools::create_example_DTARuleCheckRange(1),
          DTAtools::create_example_DTARuleCheckUnique(1),
          DTAtools::create_example_DTARuleCheckColCondition(1)
        )
      )
    )
  } else {
    cli::cli_abort("No example found with index {index}.")
  }
}


#' @title Print Method for DTARuleCollection Objects
#' @description
#' Prints a summary of a \code{DTARuleCollection} object, including the number of rules and their IDs.
#'
#' @param x A \code{DTARuleCollection} object.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input \code{DTARuleCollection} object.
#' @importFrom cli cli_alert_info cli_alert cli_text
#' @examples
#' library(DTAtools)
#' collection <- create_example_DTARuleCollection()
#' print(collection)
#' @name print
#' @export
method(print, DTARuleCollection) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTARuleCollection}> : {length(x@rules)} rules")

  rule_ids <- sapply(x@rules, function(rule) rule@id)
  n_rules <- length(rule_ids)
  max_display <- 10
  if (n_rules <= max_display) {
    cli_alert("{rule_ids}")
  } else {
    cli_alert("{rule_ids[1:9]}")
    cli_alert("...")
  }
  invisible(x)
}


