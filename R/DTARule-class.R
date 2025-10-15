#' @title DTARule Class
#' @description
#' Represents a single rule for validating data tables. The rule can be of various types,
#' such as `check_range`, `check_unique`, or `check_col_condition`
#'
#' @import S7
#' @importFrom cli cli_abort
#'
#' @param id Character. A unique identifier for the rule.
#' @param type Character. The type of the rule (e.g., "check_range", "check_unique").
#' @return An object of class `DTARule`.
#'
#' @examples
#' \dontrun{
#'  # do not manually create DTARule objects, use derived classes instead
#' }
#' @export
DTARule <- S7::new_class(
  "DTARule",

  # Constructor for the DTARule class
  constructor = function(
    id,
    type,
    description
  ) {
    # Create the class object
    new_object(
      S7_object(),
      id = id,
      type = type,
      description = description
    )
  },

  # Define the properties of the class
  properties = list(
    id = class_character, # Unique identifier for the rule
    type = class_character, # Type of the rule
    description = class_character_or_null
  ),
  validator = function(self) {
    if (any(grepl(self@id, pattern = "\\s") || is.null(self@id))) {
      "@id cannot have whitespaces and needs to be defined."
    }

    # description can be NULL or a character of length 1
    if (
      !is.null(self@description) &&
        (!is.character(self@description) || length(self@description) != 1)
    ) {
      "'description' must be NULL or a character of length 1."
    }
  }
)


#' @title print
#' @description
#' Print overview for DTARule
#' @param x An object of class DTARule
#' @importFrom stringr str_glue
#' @importFrom cli cli_alert cli_text
#' @examples
#' \dontrun{
#'  print(rule)
#' }
#' @name print
#' @export
method(print, DTARule) <- function(x) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTARule}> : {.field {x@id}}")
  cli_alert("type: {x@type}")
}


#' @title check
#' @description
#' check rule against data
#' @importFrom cli cli_abort
#' @examples
#' \dontrun{
#'  # do not manually create DTARule objects, use derived classes instead
#' }
#' @name check
#' @export
check <- new_generic("check", "x")

method(check, DTARule) <- function(x, index = 1) {
  #nolint
  cli::cli_abort(stringr::str_c(
    "Check needs to be run from Class",
    " derived from DTAtools::DTARule class."
  ))
}


#' Create a DTARule Object
#'
#' Factory function to create a DTARule object of a specified type.
#'
#' @param id A character string specifying the rule identifier.
#' @param type A character string specifying the type of rule to create.
#'   Supported types are \code{"col_condition"}, \code{"col_range"}, and \code{"col_unique"}.
#' @param ... Additional arguments passed to the specific DTARule constructor.
#'
#' @return An object of class \code{DTARuleColCondition}, \code{DTARuleColRange}, or \code{DTARuleColUnique}, depending on \code{type}.
#' @export
#'
#' @examples
#' DTARuleFactory("rule1", "col_condition", column = "age", condition = "18", then = list("status" = list("equals" = "21"))) #TODO check
#' DTARuleFactory("rule2", "col_range", column = "score", min = 0, max = 100)
#' DTARuleFactory("rule3", "col_unique", column = "id")
DTARuleFactory <- function(id, type, ...) {
  switch(
    type,
    col_condition = DTAtools::DTARuleColCondition(
      id = id,
      ...
    ),
    col_range = DTAtools::DTARuleColRange(
      id = id,
      ...
    ),
    col_unique = DTAtools::DTARuleColUnique(
      id = id,
      ...
    ),
    {
      cli::cli_abort("Unknown rule type: {type}")
    }
  )
}


#' @title as.list for DTARule
#' @description
#' Convert a DTARule object to a list.
#' @param x An object of class DTARule.
#' @param ... Additional arguments (not used).
#' @return A named list containing the properties of the DTARule object.
#' @export
as.list.DTARule <- function(x, ...) {
  list(
    id = x@id,
    type = x@type
  )
}
