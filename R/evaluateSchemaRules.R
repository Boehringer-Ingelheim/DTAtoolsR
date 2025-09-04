#' @title Rule: check_range
#' @description Ensures a column's values fall within a specified numeric range.
#' @export
rule_check_range <- function(rule, df) {
  checkDTARule(rule)
  col <- rule@column
  range <- rule@range
  violated <- !(as.numeric(df[[col]]) >= range[1] &
    as.numeric(df[[col]]) <= range[2])
  if (any(violated, na.rm = TRUE)) {
    list(
      id = rule@id,
      valid = FALSE,
      message = sprintf(
        "Rule '%s' violated: %d rows where %s not in range [%s, %s]",
        rule@id,
        sum(violated, na.rm = TRUE),
        col,
        range[1],
        range[2]
      )
    )
  } else {
    list(id = rule@id, valid = TRUE, message = NULL)
  }
}

#' @title Rule: check_unique
#' @description Ensures that all values in a column are unique.
#' @export
rule_check_unique <- function(rule, df) {
  checkDTARule(rule)
  col <- rule@column
  duplicated_vals <- duplicated(df[col])

  if (any(duplicated_vals, na.rm = TRUE)) {
    list(
      id = rule@id,
      valid = FALSE,
      message = sprintf(
        "Rule '%s' violated: %d duplicate values found in column %s",
        rule@id,
        sum(duplicated_vals, na.rm = TRUE),
        col
      )
    )
  } else {
    list(id = rule@id, valid = TRUE, message = NULL)
  }
}


evaluate_condition <- function(column_name, condition, df) {
  if (!is.null(condition$equals)) {
    return(df[[column_name]] == condition$equals)
  } else if (!is.null(condition$not_equals)) {
    return(df[[column_name]] != condition$not_equals)
  } else if (!is.null(condition[["in"]])) {
    return(df[[column_name]] %in% condition[["in"]])
  } else if (!is.null(condition$not_in)) {
    return(!df[[column_name]] %in% condition$not_in)
  } else if (!is.null(condition$greater)) {
    return(df[[column_name]] > condition$greater)
  } else if (!is.null(condition$less)) {
    return(df[[column_name]] < condition$less)
  } else if (!is.null(condition$greater_equal)) {
    return(df[[column_name]] >= condition$greater_equal)
  } else if (!is.null(condition$less_equal)) {
    return(df[[column_name]] <= condition$less_equal)
  } else if (!is.null(condition$range)) {
    return(
      df[[column_name]] >= condition$range[1] &
        df[[column_name]] <= condition$range[2]
    )
  } else if (!is.null(condition$empty)) {
    if (condition$empty) {
      return(
        is.na(df[[column_name]]) |
          df[[column_name]] == "" |
          is.nan(df[[column_name]])
      )
    } else {
      return(
        !(is.na(df[[column_name]]) | df[[column_name]] == "") |
          is.nan(df[[column_name]])
      )
    }
  } else {
    stop(sprintf("Unsupported condition type for column '%s'.", column_name))
  }
}

# Evaluate the `if` and `then` conditions
evaluate_conditions <- function(conditions, df) {
  # Iterate over each condition (column name and its rules)
  results <- lapply(names(conditions), function(column_name) {
    condition <- conditions[[column_name]]
    evaluate_condition(column_name, condition, df)
  })
  # Combine results using logical AND
  Reduce(`&`, results)
}


# Main function to evaluate the rule
rule_check_condition <- function(rule, df) {
  checkDTARule(rule)
  if_conditions <- rule@condition
  then_conditions <- rule@then

  # Evaluate `if` conditions
  if_rows <- evaluate_conditions(if_conditions, df)

  # Evaluate `then` conditions for rows that meet the `if` conditions
  then_rows <- evaluate_conditions(then_conditions, df)

  # Identify rows where the `then` conditions are violated
  violated <- !(if_rows & then_rows)[if_rows]

  # Generate the result
  if (any(violated, na.rm = TRUE)) {
    list(
      id = rule@id,
      valid = FALSE,
      message = sprintf(
        "Rule '%s' violated: %d rows failed the `then` conditions after meeting the `if` conditions.",
        rule@id,
        sum(violated, na.rm = TRUE)
      )
    )
  } else {
    list(id = rule@id, valid = TRUE, message = NULL)
  }
}

#' @title Apply Schema Rules
#' @description Applies all schema rules to a data frame with CLI feedback.
#' @importFrom cli cli_h2 cli_alert_success cli_alert_danger cli_alert_info
#' @param rules A list of rule definitions.
#' @param df A data.frame to validate.
#' @return List of rule validation results.
#' @export
applySchemaRules <- function(rules, df) {
  rule_functions <- list(
    check_range = rule_check_range,
    check_unique = rule_check_unique,
    check_condition = rule_check_condition
  )

  results <- lapply(rules, function(rule) {
    rule_type <- rule@type
    if (!rule_type %in% names(rule_functions)) {
      cli::cli_alert_danger("Unknown rule type: {rule_type}")
      return(list(
        id = rule@id,
        valid = FALSE,
        message = paste("Unknown rule type:", rule_type)
      ))
    }

    result <- rule_functions[[rule_type]](rule, df)

    if (result$valid) {
      cli::cli_alert_success("Rule '{result$id}' passed.")
    } else {
      cli::cli_alert_danger(result$message)
    }

    return(result)
  })

  failed <- Filter(function(x) !x$valid, results)

  if (length(failed) == 0) {
    cli::cli_alert_success("All schema rules passed.")
  } else {
    cli::cli_alert_info(
      "{length(failed)} rule{if (length(failed) > 1) 's'} failed."
    )
  }

  invisible(results)
}

#' @title Validate Rules defined in DTAColumnSpecCollection and a Table
#' @description Validates the structure of table using predefined rules.
#' @param DTAColumnSpecCollection A DTAColumnSpecCollection object with rules defined.
#' @param table A table that will be validated
#' @return a list containing the results of the rules validation
validateRules <- function(DTAColumnSpecCollection, table) {
  rules <- getRules(DTAColumnSpecCollection)
  results <- applySchemaRules(rules, table)

  failed <- Filter(function(x) !x$valid, results)

  if (length(failed) > 0) {
    messages <- vapply(failed, function(x) x$message, character(1))
    cli::cli_abort(c("Schema rule violations:", messages))
  }

  invisible(results)
}

checkDTARule <- function(x) {
  if (inherits(x, "DTARule")) {
    invisible(TRUE)
  } else {
    cli::cli_abort("Rule is not of class 'DTARule'")
  }
}
