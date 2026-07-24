#' @keywords internal
rule_get_slot <- function(rule, name) {
  fields <- tryCatch(as.list(rule), error = function(e) NULL)
  if (is.null(fields) || !name %in% names(fields)) {
    return(NULL)
  }
  fields[[name]]
}

#' @keywords internal
normalize_rule_type <- function(type) {
  switch(
    as.character(type),
    col_range = "check_range",
    col_unique = "check_unique",
    col_condition = "check_col_condition",
    as.character(type)
  )
}

#' @title Rule: check_range
#' @param rule A DTARule object of type `"check_range"`. Expected slots:
#'   - `@id` character
#'   - `@type` = "check_range"
#'   - `@column` character: name of the column to check
#'   - `@range` numeric(2): inclusive lower/upper bounds, e.g. c(0, 1)
#' @param df A data.frame to validate.
#' @description Ensures all non-missing values in `rule@column` fall within
#' an **inclusive** numeric range `[lower, upper]`. Missing values are ignored.
#' @return A list with elements `id`, `valid`, and `message`.
#' @examples
#' # Suppose `rule` is a DTARule with column="age", range=c(18, 65)
#' # rule_check_range(rule, df)
#' @export
rule_check_range <- function(rule, df) {
  check_rule_class(rule)
  col <- rule_get_slot(rule, "column")
  if (is.null(col)) {
    col <- rule_get_slot(rule, "columns")
  }

  if (length(col) != 1) {
    cli::cli_abort("Range rules require exactly one target column.")
  }

  range <- rule_get_slot(rule, "range")
  if (is.null(range)) {
    min_value <- rule_get_slot(rule, "min")
    max_value <- rule_get_slot(rule, "max")
    if (!is.null(min_value) && !is.null(max_value)) {
      range <- c(min_value, max_value)
    }
  }

  if (is.null(range) || length(range) != 2 || !is.numeric(range)) {
    cli::cli_abort("Range rules require numeric bounds via 'range' or 'min'/'max'.")
  }

  if (!col %in% names(df)) {
    cli::cli_abort("Column '{col}' not found in table.")
  }

  x <- as.numeric(df[[col]])
  in_range <- x >= range[1] & x <= range[2]
  violated <- !in_range

  # NA handling: ignore NAs (they neither pass nor count as violations)
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
#' @param rule A DTARule object of type `"check_unique"`. Expected slots:
#'   - `@id` character
#'   - `@type` = "check_unique"
#'   - `@column` character: name of the column to check
#' @param df A data.frame to validate.
#' @description Ensures that all values in the specified column are unique.
#' Repeated `NA` values are considered duplicates by base R `duplicated()`.
#' @return A list with elements `id`, `valid`, and `message`.
#' @examples
#' # rule_check_unique(rule, df)
#' @export
rule_check_unique <- function(rule, df) {
  check_rule_class(rule)
  cols <- rule_get_slot(rule, "column")
  if (is.null(cols)) {
    cols <- rule_get_slot(rule, "columns")
  }

  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Column{?s} not found in table: {paste(missing_cols, collapse = ', ')}"
    )
  }

  # Check for uniqueness across combined columns
  duplicated_rows <- duplicated(df[, cols, drop = FALSE])

  if (any(duplicated_rows, na.rm = TRUE)) {
    list(
      id = rule@id,
      valid = FALSE,
      message = sprintf(
        "Rule '%s' violated: %d duplicate row found when selecting column(s): %s",
        rule@id,
        sum(duplicated_rows, na.rm = TRUE),
        paste(cols, collapse = ", ")
      )
    )
  } else {
    list(id = rule@id, valid = TRUE, message = NULL)
  }
}

#' @keywords internal
evaluate_condition <- function(column_name, condition, df) {
  x <- df[[column_name]]

  if (!is.null(condition$equals) || !is.null(condition$equal)) {
    value <- if (!is.null(condition$equals)) condition$equals else condition$equal
    return(x == value)
  } else if (!is.null(condition$not_equals) || !is.null(condition$not_equal)) {
    value <- if (!is.null(condition$not_equals)) condition$not_equals else condition$not_equal
    return(x != value)
  } else if (!is.null(condition[["in"]])) {
    return(x %in% condition[["in"]])
  } else if (!is.null(condition$not_in)) {
    return(!(x %in% condition$not_in))
  } else if (!is.null(condition$greater)) {
    return(x > condition$greater)
  } else if (!is.null(condition$less)) {
    return(x < condition$less)
  } else if (!is.null(condition$greater_equal)) {
    return(x >= condition$greater_equal)
  } else if (!is.null(condition$less_equal)) {
    return(x <= condition$less_equal)
  } else if (!is.null(condition$min) || !is.null(condition$max)) {
    lower <- if (!is.null(condition$min)) condition$min else -Inf
    upper <- if (!is.null(condition$max)) condition$max else Inf
    return(x >= lower & x <= upper)
  } else if (!is.null(condition$range)) {
    return(x >= condition$range[1] & x <= condition$range[2])
  } else if (!is.null(condition$empty)) {
    empty_mask <- is.na(x)

    if (is.character(x)) {
      empty_mask <- empty_mask | trimws(x) == ""
    } else if (is.factor(x)) {
      x_chr <- as.character(x)
      empty_mask <- is.na(x_chr) | trimws(x_chr) == ""
    }

    if (isTRUE(condition$empty)) {
      return(empty_mask)
    } else {
      return(!empty_mask)
    }
  } else {
    stop(sprintf("Unsupported condition type for column '%s'.", column_name))
  }
}

#' @keywords internal
evaluate_conditions <- function(conditions, df) {
  if (length(conditions) == 0L) {
    # No conditions => no restriction (all TRUE)
    return(rep(TRUE, nrow(df)))
  }

  # Iterate over each condition (column name and its rule)
  results <- lapply(names(conditions), function(column_name) {
    condition <- conditions[[column_name]]
    evaluate_condition(column_name, condition, df)
  })

  # Combine results using logical AND (NA propagates)
  Reduce(`&`, results)
}

#' @title Rule: check_col_condition
#' @param rule A DTARule object of type `"check_col_condition"`. Expected slots:
#'   - `@id` character
#'   - `@type` = "check_col_condition"
#'   - `@condition` list: named by column, each with one of:
#'       `equals`, `not_equals`, `in`, `not_in`,
#'       `greater`, `less`, `greater_equal`, `less_equal`, `range`, `empty`
#'   - `@then` list: same structure as `@condition`
#' @param df A data.frame to validate.
#' @description Evaluates an **IF/THEN** rule across rows:
#'   If all `@condition` predicates are TRUE for a row, then all `@then`
#'   predicates must also be TRUE. For rows where the IF holds, `NA` in THEN
#'   is considered a **violation**.
#' @details
#' Supported operators per column (single operator per column):
#' - Equality: `equals`, `not_equals`
#' - Set: `in`, `not_in`
#' - Numeric comparisons: `greater`, `less`, `greater_equal`, `less_equal`, `range`
#' - Emptiness: `empty` (TRUE means empty: `NA`, `NaN`, or `""`; FALSE means not empty)
#'
#' If `@condition` is empty, the `@then` part applies to **all rows**.
#' @return A list with elements `id`, `valid`, and `message`.
#' @examples
#' # Example: If species == "setosa", then petal_length in [1.0, 1.9]
#' # rule_check_col_condition(rule, iris)
#' @export
rule_check_col_condition <- function(rule, df) {
  check_rule_class(rule)
  if_conditions <- rule@condition
  then_conditions <- rule@then

  # Evaluate IF and THEN
  if_rows <- evaluate_conditions(if_conditions, df)
  then_rows <- evaluate_conditions(then_conditions, df)

  # Violations: rows where IF is TRUE but THEN is FALSE or NA
  violated_mask <- if_rows & (is.na(then_rows) | !then_rows)
  violated_count <- sum(violated_mask, na.rm = TRUE)

  if (violated_count > 0) {
    list(
      id = rule@id,
      valid = FALSE,
      message = sprintf(
        "Rule '%s' violated: %d rows failed the THEN conditions after meeting the IF conditions.",
        rule@id,
        violated_count
      )
    )
  } else {
    list(id = rule@id, valid = TRUE, message = NULL)
  }
}

#' @title Apply Schema Rules
#' @description Applies all schema rules to a data frame with CLI feedback.
#' @importFrom cli cli_alert_success cli_alert_danger cli_alert_info
#' @param rules A list of DTARule objects.
#' @param df A data.frame to validate.
#' @param verbose Logical. If TRUE (default), prints progress messages.
#' @return (Invisibly) a list of rule validation results, each as a list with
#'   elements `id`, `valid`, and `message`.
#' @export
apply_schema_rules <- function(rules, df, verbose = TRUE) {
  if (inherits(rules, "DTAtools::DTARuleCollection")) {
    rules <- as.list(rules)
  }

  rule_functions <- list(
    check_range = rule_check_range,
    check_unique = rule_check_unique,
    check_col_condition = rule_check_col_condition
  )

  results <- lapply(rules, function(rule) {
    rule_type <- normalize_rule_type(rule@type)
    if (!rule_type %in% names(rule_functions)) {
      msg <- paste("Unknown rule type:", rule_type)
      if (isTRUE(verbose)) {
        cli::cli_alert_danger(msg)
      }
      return(list(
        id = rule@id,
        valid = FALSE,
        message = msg
      ))
    }

    result <- rule_functions[[rule_type]](rule, df)

    if (isTRUE(verbose)) {
      if (isTRUE(result$valid)) {
        cli::cli_alert_success("Rule '{result$id}' passed")
      } else {
        cli::cli_alert_danger("{result$message}")
      }
    }

    result
  })

  failed <- Filter(function(x) isFALSE(x$valid), results)

  if (isTRUE(verbose)) {
    # print a separator
    cli::cli_text()

    n_failed <- length(failed)
    if (n_failed == 0) {
      cli::cli_alert_success("All schema rules validated successfully")
    } else {
      rule_word <- if (n_failed == 1) "rule" else "rules"
      cli::cli_alert_danger("{n_failed} schema {rule_word} failed validation")
    }
  }

  invisible(results)
}

#' @title Validate Rules defined in DTAColumnSpecCollection and a Table
#' @description Validates a table using rules defined in a `DTAColumnSpecCollection`.
#'   Aborts with a CLI error if any rule fails.
#' @param DTAColumnSpecCollection A `DTAColumnSpecCollection` with rules defined.
#' @param table A data.frame to validate.
#' @importFrom stats setNames
#' @return (Invisibly) the list of rule results from `applySchemaRules()`.
#' @export
validate_rules <- function(DTAColumnSpecCollection, table) {
  rules <- rules(DTAColumnSpecCollection)
  results <- apply_schema_rules(rules, table)

  failed <- Filter(function(x) isFALSE(x$valid), results)
  if (length(failed) > 0) {
    messages <- vapply(failed, function(x) x$message, character(1))
    # Bulleted abort for nice CLI output
    bullets <- c(
      "Schema rule violations:" = "!",
      setNames(messages, rep("x", length(messages)))
    )
    cli::cli_abort(bullets)
  }

  invisible(results)
}

#' @keywords internal
#' @importFrom methods is
check_rule_class <- function(x) {
  if (methods::is(x, "DTAtools::DTARule")) {
    invisible(TRUE)
  } else {
    cli::cli_abort("Rule is not of class 'DTARule'")
  }
}
