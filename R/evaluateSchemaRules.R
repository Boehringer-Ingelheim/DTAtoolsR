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
    cli::cli_abort(
      "Column '{col}' not found in table.",
      class = "dta_rule_not_applicable"
    )
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
      "Column{?s} not found in table: {paste(missing_cols, collapse = ', ')}",
      class = "dta_rule_not_applicable"
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

#' @title Normalise a condition mapping
#' @description
#' Brings the `condition` / `then` clause of a conditional rule into the
#' canonical named-list form `list(<column> = list(<operator> = <value>))`.
#'
#' YAML authors legitimately write the clause as a sequence of single-column
#' mappings:
#'
#' ```yaml
#' condition:
#'   - VISIT:
#'       equals: V03
#' ```
#'
#' which `yaml::read_yaml()` parses into an **unnamed** list. That form is
#' unambiguous (a column may be constrained only once), so it is accepted and
#' folded into the named form rather than rejected. Anything that cannot be
#' interpreted -- a bare character string, an entry that does not name its
#' column, or the same column named twice -- aborts with an explicit message.
#' @param conditions The raw clause as supplied by the user or the YAML parser.
#' @param arg Name of the clause, used in error messages.
#' @return A named list of column conditions, possibly empty.
#' @keywords internal
dta_normalize_conditions <- function(conditions, arg = "condition") {
  if (is.null(conditions)) {
    return(list())
  }

  if (is.character(conditions)) {
    cli::cli_abort(c(
      "{.arg {arg}} must map column names to operators, not a character string.",
      x = "Got the string {.val {conditions}}.",
      i = "Write conditions as {.code list(VISIT = list(equals = \"V03\"))}."
    ))
  }

  if (!is.list(conditions)) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a list mapping column names to operators.",
      x = "Got an object of type {.cls {class(conditions)}}."
    ))
  }

  if (length(conditions) == 0L) {
    return(list())
  }

  clause_names <- names(conditions)
  if (!is.null(clause_names) && all(nzchar(clause_names))) {
    return(conditions)
  }

  # YAML sequence form: fold the sequence of single-column mappings into one
  # named mapping.
  out <- list()
  for (i in seq_along(conditions)) {
    entry_name <- if (is.null(clause_names)) "" else clause_names[[i]]

    entry <- if (nzchar(entry_name)) {
      conditions[i]
    } else {
      conditions[[i]]
    }

    # A sequence entry must be `<column>: <operator mapping>`. Requiring the
    # value to be a mapping is what separates a real entry from an operator
    # mapping that forgot to name its column: `- equals: V03` would otherwise be
    # silently read as a column literally called "equals".
    entry_is_column_mapping <- is.list(entry) &&
      length(entry) > 0L &&
      !is.null(names(entry)) &&
      all(nzchar(names(entry))) &&
      all(vapply(entry, is.list, logical(1)))

    if (!entry_is_column_mapping) {
      cli::cli_abort(c(
        "{.arg {arg}} entry {i} must name the column it applies to.",
        i = "A sequence entry looks like {.code - VISIT:} followed by its operators."
      ))
    }

    for (column_name in names(entry)) {
      if (column_name %in% names(out)) {
        cli::cli_abort(c(
          "{.arg {arg}} constrains column {.field {column_name}} more than once.",
          i = "Merge the operators for {.field {column_name}} into a single entry."
        ))
      }
      out[column_name] <- entry[column_name]
    }
  }

  out
}

#' @title Evaluate one operator of a column condition
#' @description
#' Returns the row mask for a single `<operator>: <value>` pair. Unrecognised
#' operators abort, naming both the column and the offending key.
#' @param column_name Name of the column being tested.
#' @param operator The operator key supplied in the specification.
#' @param value The value supplied for that operator.
#' @param x The column vector taken from the table.
#' @return A logical vector, one element per row of the table.
#' @keywords internal
dta_condition_mask <- function(column_name, operator, value, x) {
  switch(
    operator,
    equals = ,
    equal = x == value,
    not_equals = ,
    not_equal = x != value,
    `in` = x %in% value,
    not_in = !(x %in% value),
    greater = x > value,
    less = x < value,
    greater_equal = x >= value,
    less_equal = x <= value,
    range = x >= value[1] & x <= value[2],
    pattern = grepl(value, as.character(x), perl = TRUE),
    empty = {
      empty_mask <- is.na(x)

      if (is.character(x)) {
        empty_mask <- empty_mask | trimws(x) == ""
      } else if (is.factor(x)) {
        x_chr <- as.character(x)
        empty_mask <- is.na(x_chr) | trimws(x_chr) == ""
      }

      if (isTRUE(value)) empty_mask else !empty_mask
    },
    cli::cli_abort(c(
      "Unsupported condition operator {.val {operator}} for column {.field {column_name}}.",
      i = "Supported operators: {.val {dta_condition_operators()}}."
    ))
  )
}

#' @title Supported condition operators
#' @description The operator keys accepted inside a conditional rule clause.
#' @return A character vector of operator names.
#' @keywords internal
dta_condition_operators <- function() {
  c(
    "equals", "equal", "not_equals", "not_equal", "in", "not_in",
    "greater", "less", "greater_equal", "less_equal",
    "min", "max", "range", "pattern", "empty"
  )
}

#' @keywords internal
evaluate_condition <- function(column_name, condition, df) {
  if (!column_name %in% names(df)) {
    cli::cli_abort(
      "Column not found in table: {column_name}",
      class = "dta_rule_not_applicable"
    )
  }

  # An empty or unnamed operator map is a specification error, not "no
  # restriction": silently passing every row would make the rule invisible.
  operators <- names(condition)
  if (length(condition) == 0L || is.null(operators) || !all(nzchar(operators))) {
    cli::cli_abort(c(
      "Condition for column {.field {column_name}} must map operators to values.",
      i = "Supported operators: {.val {dta_condition_operators()}}."
    ))
  }

  x <- df[[column_name]]
  masks <- list()

  # `min` and `max` are the one documented pair: together they describe a single
  # inclusive band, so they are consumed as a unit rather than as two operators.
  if (any(c("min", "max") %in% operators)) {
    lower <- if ("min" %in% operators) condition[["min"]] else -Inf
    upper <- if ("max" %in% operators) condition[["max"]] else Inf
    masks[[length(masks) + 1L]] <- x >= lower & x <= upper
  }

  for (i in seq_along(condition)) {
    operator <- operators[[i]]
    if (operator %in% c("min", "max")) {
      next
    }
    masks[[length(masks) + 1L]] <- dta_condition_mask(
      column_name = column_name,
      operator = operator,
      value = condition[[i]],
      x = x
    )
  }

  # Every operator supplied for a column must hold: combine with AND.
  # NA propagates, and is treated as a THEN violation by the caller.
  Reduce(`&`, masks)
}

#' @keywords internal
evaluate_conditions <- function(conditions, df) {
  conditions <- dta_normalize_conditions(conditions)

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
#'   - `@condition` list: named by column, each with one or more of:
#'       `equals`, `not_equals`, `in`, `not_in`,
#'       `greater`, `less`, `greater_equal`, `less_equal`, `min`, `max`,
#'       `range`, `pattern`, `empty`
#'   - `@then` list: same structure as `@condition`
#' @param df A data.frame to validate.
#' @description Evaluates an **IF/THEN** rule across rows:
#'   If all `@condition` predicates are TRUE for a row, then all `@then`
#'   predicates must also be TRUE. For rows where the IF holds, `NA` in THEN
#'   is considered a **violation**.
#' @details
#' A column may carry **several operators**; all of them must hold for the row
#' to satisfy that column (they are combined with logical AND). `min` and `max`
#' are the one exception: together they describe a single inclusive band rather
#' than two independent tests. An operator key that is not recognised aborts,
#' naming the column and the offending key.
#'
#' Supported operators per column:
#' - Equality: `equals`, `not_equals`
#' - Set: `in`, `not_in`
#' - Numeric comparisons: `greater`, `less`, `greater_equal`, `less_equal`,
#'   `min`, `max`, `range`
#' - Text: `pattern` (a regular expression; row passes when the value matches)
#' - Emptiness: `empty` (TRUE means empty: `NA`, `NaN`, or `""`; FALSE means not empty)
#'
#' Both `@condition` and `@then` may also be written as a YAML sequence of
#' single-column mappings; they are normalised to the named form.
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
#' @param rules A list of DTARule objects, or NULL.
#' @param df A data.frame to validate.
#' @param verbose Logical. If TRUE (default), prints progress messages.
#' @return (Invisibly) a list of rule validation results, each as a list with
#'   elements `id`, `valid`, and `message`.
#' @export
apply_schema_rules <- function(rules, df, verbose = TRUE) {
  if (is.null(rules)) {
    rules <- list()
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

    # A rule that cannot be evaluated against this table -- typically a stale
    # rule naming a column the table does not have -- is a rule FAILURE, not a
    # reason to abort validation of everything else. Only the narrowly classed
    # `dta_rule_not_applicable` condition is caught here; genuine programming
    # errors and malformed rule specifications still propagate.
    result <- tryCatch(
      rule_functions[[rule_type]](rule, df),
      dta_rule_not_applicable = function(cnd) {
        list(
          id = rule@id,
          valid = FALSE,
          message = sprintf(
            "Rule '%s' could not be evaluated: %s",
            rule@id,
            conditionMessage(cnd)
          )
        )
      }
    )

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
