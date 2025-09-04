class_vector_or_null <- class_vector | class_null
#' @title DTARule Class
#' @description
#' Represents a single rule for validating data tables. The rule can be of various types,
#' such as `check_range`, `check_unique`, or `check_condition`
#'
#' @import S7
#' @importFrom cli cli_abort
#' @export
#'
#' @param id Character. A unique identifier for the rule.
#' @param type Character. The type of the rule (e.g., "check_range", "check_unique").
#' @param condition List. Optional depending on the rule id. A list of conditions to check
#' @param then List. Optional depending on the rule id. A list of conditions to check - used with condition
#' @param column List. Optional depending on the rule id. Used in check_range and check_unique. In check_unique it is a list of columns that will be collectively checked if the combinations are unique throughout the table
#' @param range Vector or List. Optional depending on the rule id. Will be used in check_range to check the range of the values in a given column.
#' @return An object of class `DTARule`.
#'
#' @examples
#' # Create a check_range rule
#' rule1 <- DTARule(
#'   id = "rule1",
#'   type = "check_range",
#'   column = "age",
#'   range = c(18, 65)
#' )
#'
#' # Create a check_unique rule
#' rule2 <- DTARule(
#'   id = "rule2",
#'   type = "check_unique",
#'   column = "id"
#' )
DTARule <- new_class(
  "DTARule",

  # Constructor for the DTARule class
  constructor = function(
    id,
    type,
    column = NULL,
    range = NULL,
    condition = NULL,
    then = NULL
  ) {
    # Validate the rule type
    valid_types <- c(
      "check_range",
      "check_unique",
      "check_condition"
    )
    if (!type %in% valid_types) {
      cli::cli_abort(
        "Invalid rule type: {type}. Must be one of {paste(valid_types, collapse = ', ')}."
      )
    }

    # Validate required fields based on the rule type
    required_fields <- list(
      check_range = c("column", "range"),
      check_unique = c("column"),
      check_condition = c("condition", "then")
    )

    params <- list(
      column = column,
      range = range,
      condition = condition,
      then = then
    )

    avail_params <- params[!is.null(params)]
    missing_fields <- setdiff(required_fields[[type]], names(avail_params))
    if (length(missing_fields) > 0) {
      cli::cli_abort(
        "Missing required fields for rule type '{type}': {paste(missing_fields, collapse = ', ')}."
      )
    }

    if (is.list(range)) {
      range <- unlist(range)
    }

    if (!is.null(range) & length(range) != 2) {
      cli::cli_abort(
        "'range' is not of length 2."
      )
    }

    cli::cli_alert_success("Rule '{id}' passes basic parameter checks.")

    # Create the class object
    new_object(
      S7_object(),
      id = id,
      type = type,
      condition = condition,
      then = then,
      column = column,
      range = range
    )
  },

  # Define the properties of the class
  properties = list(
    id = class_character, # Unique identifier for the rule
    type = class_character, # Type of the rule
    condition = class_character_or_null_or_list,
    then = class_character_or_null_or_list,
    column = class_character_or_null_or_list,
    range = class_vector_or_null
  )
)
