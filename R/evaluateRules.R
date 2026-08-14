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
  switch(as.character(type),
    col_range = "check_range",
    col_unique = "check_unique",
    col_condition = "check_col_condition",
    group_condition = "check_group_condition",
    as.character(type)
  )
}

#' @title Strict Numeric Conversion
#' @description
#' Converts a column to numeric while keeping apart the three cases that a bare
#' `as.numeric()` collapses into a single `NA`:
#'
#' * **missing** -- `NA` or an empty string in the *source*. Not an error: a
#'   missing value neither passes nor violates a numeric rule.
#' * **unconvertible** -- present in the source but not representable as a
#'   number (`"ninety"`, `"N/A"`, `">65"`, the factor level `"high"`). This is
#'   an import error, and the row must not be treated as passing the rule.
#' * **convertible** -- the numeric value is used.
#'
#' Factors are converted through `as.character()` **first**. `as.numeric()` on a
#' factor returns its *integer level codes*, so `factor(c("500", "600", "700"))`
#' reads as `1, 2, 3` and sails through any range rule that admits small
#' integers.
#'
#' Only unrecoverable values are reported as unconvertible. A value that
#' converts but changes representation (`"007"` to `7`, `"1.50"` to `1.5`) is a
#' clean conversion, not an import error.
#'
#' Dates and date-times are converted through their own numeric representation
#' and can never be unconvertible; they are never text that failed to parse.
#' @param x A vector taken from a table column.
#' @return A list with `values` (numeric), `raw` (character, the source text
#'   verbatim), `missing` (logical) and `unconvertible` (logical), each the same
#'   length as `x`.
#' @keywords internal
dta_as_numeric_strict <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    values <- as.numeric(x)
    return(list(
      values = values,
      raw = as.character(x),
      missing = is.na(values),
      unconvertible = rep(FALSE, length(values))
    ))
  }

  raw <- if (is.factor(x)) as.character(x) else x

  if (is.numeric(raw) || is.logical(raw)) {
    values <- as.numeric(raw)
    return(list(
      values = values,
      raw = as.character(raw),
      missing = is.na(values),
      unconvertible = rep(FALSE, length(values))
    ))
  }

  raw_chr <- as.character(raw)
  # `trimws(NA) %in% ""` is FALSE, so the is.na() term is what catches NA here.
  missing <- is.na(raw_chr) | trimws(raw_chr) %in% ""
  values <- suppressWarnings(as.numeric(trimws(raw_chr)))

  list(
    values = values,
    raw = raw_chr,
    missing = missing,
    unconvertible = is.na(values) & !missing
  )
}

#' @title Operands for a Numeric Comparison
#' @description
#' Returns the column and the bound of a numeric comparison as numbers.
#'
#' Comparing a character column with `>` coerces the *bound* to character and
#' applies locale collation, under which `"9" > "65"` is `TRUE`. Converting both
#' sides first is what makes the comparison mean what it says.
#' @param x The column vector taken from the table.
#' @param value The bound supplied in the specification.
#' @return A list with the numeric `x` and `value`.
#' @keywords internal
dta_numeric_operands <- function(x, value) {
  # Dates and date-times carry their own comparison semantics -- a character
  # bound is parsed as a date -- so they are compared exactly as before.
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(list(x = x, value = value))
  }

  bound <- if (is.character(value) || is.factor(value)) {
    suppressWarnings(as.numeric(as.character(value)))
  } else {
    value
  }

  list(x = dta_as_numeric_strict(x)$values, value = bound)
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
  target <- dta_range_target(rule)
  violated <- dta_range_violated(rule, df)

  if (any(violated)) {
    list(
      id = rule@id,
      valid = FALSE,
      message = dta_range_violation_message(
        rule@id, sum(violated), target$col, target$range
      )
    )
  } else {
    list(id = rule@id, valid = TRUE, message = NULL)
  }
}

#' @title Resolved Column and Bounds of a Range Rule
#' @description
#' A range rule may state its bounds as `range` or as `min`/`max`, and its
#' target as `column` or `columns`. Resolving that once, here, keeps every
#' caller agreeing about what the rule actually says.
#' @param rule A range rule.
#' @return A list with `col` and `range`.
#' @keywords internal
dta_range_target <- function(rule) {
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

  list(col = col, range = range)
}

#' @title Rows a Range Rule Counts as Violations
#' @description
#' The per-row violation mask for a range rule.
#'
#' This exists so that evaluating a whole table and evaluating one batch of it
#' cannot drift apart: both go through this function, and a violation is
#' whatever this says it is. A count is an associative reduction over the mask,
#' which is what lets a batched scan reproduce a whole-table answer exactly.
#' @param rule A range rule.
#' @param df A data.frame.
#' @return A logical vector with one element per row.
#' @keywords internal
dta_range_violated <- function(rule, df) {
  target <- dta_range_target(rule)
  col <- target$col

  if (!col %in% names(df)) {
    cli::cli_abort(
      "Column '{col}' not found in table.",
      class = "dta_rule_not_applicable"
    )
  }

  converted <- dta_as_numeric_strict(df[[col]])
  in_range <- converted$values >= target$range[1] & converted$values <= target$range[2]

  # A genuinely missing value is ignored: it neither passes nor violates.
  # A value that is present but not representable as a number is a violation,
  # not a pass -- `any(!in_range, na.rm = TRUE)` used to drop it silently, so
  # c("ninety", "N/A", ">65") reported a clean 18..65 range.
  (in_range %in% FALSE) | converted$unconvertible
}

#' @title Message for a Range Rule Violation
#' @param id Character. The rule id.
#' @param n Integer. Number of violating rows.
#' @param col Character. The column checked.
#' @param range Numeric. The inclusive bounds.
#' @return A single string.
#' @keywords internal
dta_range_violation_message <- function(id, n, col, range) {
  sprintf(
    "Rule '%s' violated: %d rows where %s not in range [%s, %s]",
    id, n, col, range[1], range[2]
  )
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
      message = dta_unique_violation_message(
        rule@id, sum(duplicated_rows, na.rm = TRUE), cols
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
  # Numeric comparisons must compare numbers. Applied to the raw column, `>` on
  # a character vector coerces the bound to character and compares by locale
  # collation, so AGE = c("9", "700") passed `greater: 65` because "9" sorts
  # after "65". `pattern` and the equality/set operators are unaffected and
  # deliberately stay on the raw column.
  if (operator %in% dta_numeric_condition_operators()) {
    operands <- dta_numeric_operands(x, value)
    x <- operands$x
    value <- operands$value
  }

  switch(operator,
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

#' @title Condition Operators That Compare Numbers
#' @description
#' The subset of `dta_condition_operators()` whose operands are numeric. Only
#' these go through `dta_numeric_operands()`; the equality, set, text and
#' emptiness operators keep comparing the raw column.
#' @return A character vector of operator names.
#' @keywords internal
dta_numeric_condition_operators <- function() {
  c("greater", "less", "greater_equal", "less_equal", "min", "max", "range")
}

#' @title Rows Whose Condition Clause Cannot Be Decided
#' @description
#' Marks the rows for which a clause is `NA` because a value it compares
#' numerically is **unconvertible** -- `AGE = "ninety-five"` under
#' `greater: 18` -- as opposed to merely missing.
#'
#' The two look identical in the mask returned by `evaluate_conditions()` and
#' must not be resolved the same way; see [dta_condition_in_scope()].
#' @param conditions A clause, in either the named or the YAML sequence form.
#' @param df A data.frame.
#' @return A logical vector with one element per row of `df`, never `NA`.
#' @keywords internal
dta_condition_undecidable <- function(conditions, df) {
  conditions <- dta_normalize_conditions(conditions)
  out <- rep(FALSE, nrow(df))
  numeric_operators <- dta_numeric_condition_operators()

  for (column_name in names(conditions)) {
    if (!column_name %in% names(df)) {
      next
    }
    if (!any(names(conditions[[column_name]]) %in% numeric_operators)) {
      next
    }

    x <- df[[column_name]]
    # Dates and date-times bypass dta_as_numeric_strict() in
    # dta_numeric_operands(), so they can never be unconvertible here either.
    if (inherits(x, "Date") || inherits(x, "POSIXt")) {
      next
    }

    out <- out | dta_as_numeric_strict(x)$unconvertible
  }

  out
}

#' @title Rows a Conditional Rule's IF Clause Applies To
#' @description
#' Resolves the three-valued IF mask into the two-valued "is this row in scope"
#' answer the rule needs.
#' @details
#' `NA` in the IF clause has two causes that must be resolved differently:
#'
#' * the value is **missing** -- nothing is known about the row, so the
#'   condition genuinely does not apply and the row is out of scope.
#' * the value is **unconvertible** -- the row stays in scope. Dropping it
#'   would let a row whose THEN clause definitively fails escape the rule
#'   altogether, which is exactly what [dta_as_numeric_strict()] forbids: an
#'   unconvertible value must not be treated as passing.
#'
#' A row that fails some *other* predicate of the clause outright is out of
#' scope regardless, because the clause is a conjunction and one determinate
#' `FALSE` settles it.
#' @param conditions The rule's IF clause.
#' @param df A data.frame.
#' @param if_rows The mask returned by `evaluate_conditions()` for `conditions`.
#' @return A logical vector with one element per row of `df`, never `NA`.
#' @keywords internal
dta_condition_in_scope <- function(conditions, df, if_rows) {
  undecided <- is.na(if_rows)
  if (!any(undecided)) {
    return(if_rows)
  }

  if_rows[undecided] <- dta_condition_undecidable(conditions, df)[undecided]
  if_rows
}

#' @title Columns a Rule Compares Numerically
#' @description
#' Names the columns whose values this rule reads as numbers. These are the
#' columns scanned for values that are present but not representable as a
#' number, which is what the import axis reports.
#' @param rule A `DTARule` object.
#' @return A character vector of column names, possibly empty.
#' @keywords internal
dta_rule_numeric_columns <- function(rule) {
  type <- tryCatch(
    normalize_rule_type(rule@type),
    error = function(e) NA_character_
  )

  if (identical(type, "check_range")) {
    col <- rule_get_slot(rule, "column")
    if (is.null(col)) {
      col <- rule_get_slot(rule, "columns")
    }
    return(as.character(col))
  }

  if (identical(type, "check_col_condition")) {
    clauses <- c(
      dta_normalize_conditions(rule_get_slot(rule, "condition"), arg = "condition"),
      dta_normalize_conditions(rule_get_slot(rule, "then"), arg = "then")
    )

    if (length(clauses) == 0) {
      return(character(0))
    }

    numeric_ops <- dta_numeric_condition_operators()
    is_numeric_clause <- vapply(
      clauses,
      function(condition) {
        is.list(condition) && any(names(condition) %in% numeric_ops)
      },
      logical(1)
    )

    return(unique(as.character(names(clauses)[is_numeric_clause])))
  }

  if (identical(type, "check_group_condition")) {
    groups <- rule_get_slot(rule, "conditions")
    if (is.null(groups) || length(groups) == 0) {
      return(character(0))
    }

    all_clauses <- list()
    for (cond_name in names(groups)) {
      normalized <- dta_normalize_conditions(groups[[cond_name]], arg = cond_name)
      all_clauses <- c(all_clauses, normalized)
    }

    if (length(all_clauses) == 0) {
      return(character(0))
    }

    numeric_ops <- dta_numeric_condition_operators()
    is_numeric_clause <- vapply(
      all_clauses,
      function(condition) {
        is.list(condition) && any(names(condition) %in% numeric_ops)
      },
      logical(1)
    )

    return(unique(as.character(names(all_clauses)[is_numeric_clause])))
  }

  character(0)
}

#' @title Import Errors Contributed by One Rule
#' @description
#' Scans the columns this rule compares numerically and reports every value that
#' is present in the source but not representable as a number.
#'
#' Such a value is reported on **both** axes: here as an import error, and by
#' the rule itself as a violated row. Moving it to the import axis alone would
#' make any consumer reading `n_rule_errors` see fewer errors than before.
#' @param rule A `DTARule` object.
#' @param df A data.frame to scan.
#' @return A data.frame in the shape of `dta_empty_import_errors()`.
#' @keywords internal
dta_rule_import_errors <- function(rule, df) {
  columns <- tryCatch(
    dta_rule_numeric_columns(rule),
    error = function(e) character(0)
  )
  columns <- unique(columns[columns %in% names(df)])

  if (length(columns) == 0) {
    return(dta_empty_import_errors())
  }

  parts <- lapply(columns, function(column) {
    converted <- dta_as_numeric_strict(df[[column]])
    offending <- which(converted$unconvertible)

    if (length(offending) == 0) {
      return(dta_empty_import_errors())
    }

    data.frame(
      row = as.integer(offending),
      column = column,
      raw = converted$raw[offending],
      # A placeholder the caller replaces with the declared type from the
      # column spec; it is the observed storage type when no spec is at hand.
      declared_type = class(df[[column]])[[1]],
      reason = "not_convertible",
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
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
    # Same collation trap as the other comparisons: the band is numeric, so
    # both ends and the column are taken as numbers.
    lower_operands <- dta_numeric_operands(x, lower)
    upper_operands <- dta_numeric_operands(x, upper)
    masks[[length(masks) + 1L]] <-
      lower_operands$x >= lower_operands$value &
        upper_operands$x <= upper_operands$value
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
#' A row whose IF clause cannot be decided is in scope when the undecidable
#' value is **unconvertible** (`AGE = "ninety-five"` under `greater: 18`) and
#' out of scope when it is merely **missing**. An unconvertible value is a data
#' error, and must not buy the row an exemption from the rule; a missing one
#' says nothing about the row either way. See [dta_condition_in_scope()].
#'
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

  violated_count <- sum(dta_condition_violated(rule, df))

  if (violated_count > 0) {
    list(
      id = rule@id,
      valid = FALSE,
      message = dta_condition_violation_message(rule@id, violated_count)
    )
  } else {
    list(id = rule@id, valid = TRUE, message = NULL)
  }
}

#' @title Rows an IF/THEN Rule Counts as Violations
#' @description
#' The per-row violation mask for a conditional rule: the IF applies but the
#' THEN does not hold, where a missing or unconvertible THEN counts against the
#' row. Which rows the IF applies to is decided by [dta_condition_in_scope()].
#'
#' This is the single definition of a conditional violation. `rule_check_col_condition()`,
#' the streaming scan and `inspect()`'s row lookup all call it, so a batched
#' scan, a whole-table pass and the row preview cannot disagree.
#' @param rule A conditional rule.
#' @param df A data.frame.
#' @return A logical vector with one element per row, never `NA`.
#' @keywords internal
dta_condition_violated <- function(rule, df) {
  if_rows <- evaluate_conditions(rule@condition, df)
  then_rows <- evaluate_conditions(rule@then, df)
  dta_condition_in_scope(rule@condition, df, if_rows) &
    (is.na(then_rows) | !then_rows)
}

#' @title Message for a Conditional Rule Violation
#' @param id Character. The rule id.
#' @param n Integer. Number of violating rows.
#' @return A single string.
#' @keywords internal
dta_condition_violation_message <- function(id, n) {
  sprintf(
    "Rule '%s' violated: %d rows failed the THEN conditions after meeting the IF conditions.",
    id, n
  )
}

#' @title Message for a Uniqueness Rule Violation
#' @param id Character. The rule id.
#' @param n Integer. Number of duplicate rows.
#' @param cols Character. The columns forming the key.
#' @return A single string.
#' @keywords internal
dta_unique_violation_message <- function(id, n, cols) {
  sprintf(
    "Rule '%s' violated: %d duplicate row found when selecting column(s): %s",
    id, n, paste(cols, collapse = ", ")
  )
}

#' @title Render Row Numbers for a Grouped Constraint Message
#' @description
#' Shows the first `max_show` rows and, beyond that, how many more there were.
#'
#' Takes the leading rows and the total separately rather than the whole vector,
#' because a streaming evaluation never holds the whole vector: it keeps the
#' first few row numbers it sees and counts the rest. Passing both makes the
#' streamed and materialised messages identical by construction.
#' @param head_rows Integer. The leading row numbers, already sorted and unique.
#' @param total Integer. How many rows there were in total.
#' @param max_show Integer. How many to name before summarising the remainder.
#' @return A single string.
#' @keywords internal
dta_format_group_rows <- function(head_rows, total, max_show = 10L) {
  if (total == 0) {
    return("none")
  }
  head_rows <- sort(unique(as.integer(head_rows)))
  if (total > max_show) {
    paste0(
      paste(head_rows[seq_len(min(max_show, length(head_rows)))], collapse = ","),
      " (+",
      total - max_show,
      " more)"
    )
  } else {
    paste(head_rows, collapse = ",")
  }
}

#' @title Group Key for a Set of Grouping Columns
#' @description
#' The key by which rows are grouped. Shared between the materialising and
#' streaming paths so a group is the same group in both: a separator appearing
#' in the data is escaped, and a missing value groups with other missing values
#' rather than forming its own.
#' @param df A data frame.
#' @param group_by Character. The grouping columns.
#' @return A character vector, one key per row.
#' @keywords internal
dta_group_key <- function(df, group_by) {
  # ASCII unit separator. Written as a code point rather than an escape so the
  # source carries no raw control byte for tooling to mangle.
  group_sep <- intToUtf8(31L)
  key_parts <- lapply(df[, group_by, drop = FALSE], function(x) {
    chr <- as.character(x)
    chr[is.na(chr)] <- "<NA>"
    gsub(group_sep, paste0(group_sep, group_sep), chr, fixed = TRUE)
  })
  do.call(paste, c(key_parts, sep = group_sep))
}

#' @keywords internal
dta_group_scope_truth <- function(mask, scope) {
  hit <- mask %in% TRUE
  if (identical(scope, "all")) {
    length(hit) > 0 && all(hit)
  } else {
    any(hit)
  }
}

#' @title Rule: check_group_condition
#' @description
#' Evaluates named conditions per group and applies constraints between those
#' condition outcomes.
#' @param rule A `DTARuleGroupCondition` object.
#' @param df A data.frame to validate.
#' @return A list with elements `id`, `valid`, and `message`.
#' @examples
#' df <- data.frame(
#'   SUBJECT_ID = c("S1", "S1", "S2"),
#'   STATUS = c("FAILED", "FAILED", "DONE"),
#'   RESULT = c(NA, 12, NA),
#'   stringsAsFactors = FALSE
#' )
#' rule <- DTARuleGroupCondition(
#'   id = "group_example",
#'   group_by = "SUBJECT_ID",
#'   conditions = list(
#'     c_failed = list(STATUS = list(equals = "FAILED")),
#'     c_reported = list(RESULT = list(empty = FALSE))
#'   ),
#'   constraints = list(
#'     list(type = "mutually_exclusive", left = "c_failed", right = "c_reported")
#'   )
#' )
#' rule_check_group_condition(rule, df)
#' @export
rule_check_group_condition <- function(rule, df) {
  check_rule_class(rule)

  format_rows <- function(rows, max_show = 10L) {
    rows <- sort(unique(as.integer(rows)))
    dta_format_group_rows(rows, length(rows), max_show)
  }

  group_by <- rule_get_slot(rule, "group_by")
  conditions <- rule_get_slot(rule, "conditions")
  constraints <- rule_get_slot(rule, "constraints")

  missing_group_cols <- setdiff(group_by, names(df))
  if (length(missing_group_cols) > 0) {
    cli::cli_abort(
      c(
        "Rule {.val {rule@id}} cannot be evaluated as group_condition.",
        x = "Grouping column{?s} missing in input data: {.val {missing_group_cols}}.",
        i = "Available columns: {.val {names(df)}}."
      ),
      class = "dta_rule_not_applicable"
    )
  }

  if (nrow(df) == 0) {
    return(list(id = rule@id, valid = TRUE, message = NULL))
  }

  grouped <- df[, group_by, drop = FALSE]
  split_key <- dta_group_key(df, group_by)
  groups <- split(seq_len(nrow(df)), split_key)

  violations <- list()

  for (group_index in seq_along(groups)) {
    row_idx <- groups[[group_index]]
    gdf <- df[row_idx, , drop = FALSE]

    cond_hits <- lapply(names(conditions), function(cond_name) {
      spec <- conditions[[cond_name]]
      tryCatch(
        evaluate_conditions(spec, gdf),
        dta_rule_not_applicable = function(cnd) {
          cli::cli_abort(c(
            "Rule {.val {rule@id}} cannot evaluate condition {.field {cond_name}}.",
            x = "{conditionMessage(cnd)}",
            i = "Condition {.field {cond_name}} is defined as: {.val {paste(capture.output(str(spec, give.attr = FALSE)), collapse = ' ')}}"
          ), class = "dta_rule_not_applicable")
        }
      )
    })
    names(cond_hits) <- names(conditions)
    cond_rows <- lapply(cond_hits, function(mask) row_idx[mask %in% TRUE])

    group_values <- grouped[row_idx[1], , drop = FALSE]
    group_label <- paste(
      vapply(group_by, function(col) {
        paste0(col, "=", as.character(group_values[[col]][1]))
      }, character(1)),
      collapse = ", "
    )

    for (constraint in constraints) {
      ctype <- constraint$type

      if (identical(ctype, "mutually_exclusive")) {
        left <- constraint$left
        right <- constraint$right

        left_truth <- dta_group_scope_truth(
          cond_hits[[left]],
          constraint$left_scope %||% "any"
        )
        right_truth <- dta_group_scope_truth(
          cond_hits[[right]],
          constraint$right_scope %||% "any"
        )

        if (isTRUE(left_truth) && isTRUE(right_truth)) {
          message <- constraint$message %||%
            sprintf(
              "In group [%s]: \"%s\" and \"%s\" must not both occur, but both were found (rows matching \"%s\": %s; rows matching \"%s\": %s).",
              group_label,
              left,
              right,
              left,
              format_rows(cond_rows[[left]]),
              right,
              format_rows(cond_rows[[right]])
            )
          violations[[length(violations) + 1L]] <- list(
            constraint_id = constraint$id,
            group = group_label,
            message = message,
            rows = sort(unique(c(cond_rows[[left]], cond_rows[[right]]))),
            # The whole table is in hand here, so `rows` is never a head.
            rows_truncated = FALSE
          )
        }
      } else if (identical(ctype, "requires")) {
        if_name <- constraint[["if"]]
        then_name <- constraint[["then"]]

        if_truth <- dta_group_scope_truth(
          cond_hits[[if_name]],
          constraint$if_scope %||% "any"
        )
        then_scope <- constraint$then_scope %||% "any"
        then_truth <- dta_group_scope_truth(cond_hits[[then_name]], then_scope)

        if (isTRUE(if_truth) && !isTRUE(then_truth)) {
          if_rows <- cond_rows[[if_name]]
          then_rows <- cond_rows[[then_name]]
          then_failed <- if (identical(then_scope, "all")) {
            row_idx[!(cond_hits[[then_name]] %in% TRUE)]
          } else {
            integer(0)
          }
          then_scope_reason <- if (identical(then_scope, "all")) {
            sprintf("rows %s do not satisfy \"%s\"", format_rows(then_failed), then_name)
          } else {
            sprintf("no row in the group satisfies \"%s\"", then_name)
          }
          message <- constraint$message %||%
            sprintf(
              "In group [%s]: when \"%s\" occurs (rows: %s), \"%s\" must also hold, but it does not (%s).",
              group_label,
              if_name,
              format_rows(if_rows),
              then_name,
              then_scope_reason
            )
          violations[[length(violations) + 1L]] <- list(
            constraint_id = constraint$id,
            group = group_label,
            message = message,
            rows = sort(unique(c(if_rows, then_failed))),
            rows_truncated = FALSE
          )
        }
      }
    }
  }

  if (length(violations) == 0) {
    return(list(id = rule@id, valid = TRUE, message = NULL))
  }

  summary <- sprintf(
    "Rule '%s': %d group constraint violation%s found across %d group%s.",
    rule@id,
    length(violations),
    if (length(violations) == 1) "" else "s",
    length(unique(vapply(violations, function(v) v$group, character(1)))),
    if (length(unique(vapply(violations, function(v) v$group, character(1)))) == 1) "" else "s"
  )

  details <- vapply(violations, function(v) v$message, character(1))

  list(
    id = rule@id,
    valid = FALSE,
    message = paste(c(summary, details), collapse = " "),
    details = violations
  )
}

#' @title Apply Rules
#' @description Applies all rules to a data frame with CLI feedback.
#' @importFrom cli cli_alert_success cli_alert_danger cli_alert_info
#' @param rules A list of DTARule objects, or NULL.
#' @param df A data.frame to validate.
#' @param verbose Logical. If TRUE (default), prints progress messages.
#' @return (Invisibly) a list of rule validation results, each as a list with
#'   elements `id`, `valid`, and `message`.
#' @export
apply_rules <- function(rules, df, verbose = TRUE) {
  if (is.null(rules)) {
    rules <- list()
  }

  rule_functions <- list(
    check_range = rule_check_range,
    check_unique = rule_check_unique,
    check_col_condition = rule_check_col_condition,
    check_group_condition = rule_check_group_condition
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

    # The import axis is sourced from the same columns the rule just read as
    # numbers, so an unrepresentable value is reported on both axes rather than
    # reclassified from one to the other.
    result$import_errors <- dta_rule_import_errors(rule, df)

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
      cli::cli_alert_success("All rules validated successfully")
    } else {
      rule_word <- if (n_failed == 1) "rule" else "rules"
      cli::cli_alert_danger("{n_failed} {rule_word} failed validation")
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
  results <- apply_rules(rules, table)

  failed <- Filter(function(x) isFALSE(x$valid), results)
  if (length(failed) > 0) {
    messages <- vapply(failed, function(x) x$message, character(1))
    # Bulleted abort for nice CLI output
    bullets <- c(
      "Rule violations:" = "!",
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
