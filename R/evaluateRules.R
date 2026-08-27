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
#' @return A list with `values` (numeric), `missing` (logical) and
#'   `unconvertible` (logical) -- each the same length as `x` -- together with
#'   `source`, which is `x` itself. The source text of row `i` is *derived* from
#'   `source` on demand by [dta_numeric_raw()] rather than stored as a second,
#'   eagerly built character copy of the whole column.
#' @keywords internal
dta_as_numeric_strict <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    values <- as.numeric(x)
    return(list(
      values = values,
      source = x,
      missing = is.na(values),
      unconvertible = rep(FALSE, length(values))
    ))
  }

  raw <- if (is.factor(x)) as.character(x) else x

  if (is.numeric(raw) || is.logical(raw)) {
    values <- as.numeric(raw)
    return(list(
      values = values,
      source = x,
      missing = is.na(values),
      unconvertible = rep(FALSE, length(values))
    ))
  }

  raw_chr <- as.character(raw)
  # Parsed untrimmed: as.numeric() itself skips surrounding whitespace, so
  # trimming first changes no value -- it only mattered for telling a blank
  # string (missing) from text that failed to parse (unconvertible). That
  # distinction is needed only where parsing yielded NA, which on real data is
  # a handful of rows, so the trimws() pass -- two regex substitutions and a
  # full character-vector reallocation per numeric column per batch on the
  # streaming path -- runs on that subset instead of the whole column.
  values <- suppressWarnings(as.numeric(raw_chr))
  missing <- is.na(raw_chr)
  failed <- is.na(values) & !missing
  if (any(failed)) {
    idx <- which(failed)
    # A blank ("" or whitespace-only) value never parses, so blank rows are a
    # subset of `failed` -- reclassifying them here reproduces the old
    # whole-column `trimws(raw_chr) %in% ""` exactly.
    missing[idx[trimws(raw_chr[idx]) %in% ""]] <- TRUE
  }

  list(
    values = values,
    source = x,
    missing = missing,
    unconvertible = is.na(values) & !missing
  )
}

#' @title The Source Text Behind a Strict Numeric Conversion
#' @description
#' Returns the verbatim source text of selected rows of a
#' [dta_as_numeric_strict()] result.
#'
#' The conversion keeps the *source vector* rather than a character rendering of
#' the whole column, because the text is only ever read at the handful of rows
#' that failed to convert -- usually none. On a 200,000-row numeric column the
#' eager character copy was 13 MB against a 1.5 MB column, and one was held per
#' cached column simultaneously.
#'
#' Rendering at the index is exactly equivalent to indexing an eagerly rendered
#' vector, branch by branch:
#'
#' * numeric/logical: the text was `as.character(x)`, and `as.character()` is
#'   elementwise, so `as.character(x[i])` is the same string.
#' * factor: the text was `as.character(x)`, which yields the *labels*, not the
#'   integer level codes. `x[i]` on a factor is still a factor carrying its
#'   levels, so `as.character(x[i])` is the same label.
#' * character: the text was the **untrimmed** `as.character(x)`. `source` is
#'   the untrimmed column, so this must stay untrimmed here too -- the trimming
#'   applies to parsing only.
#' * Date/POSIXt: the text was `as.character(x)`; `[` preserves the class and
#'   the time zone, so formatting at the index is unchanged.
#' @param entry A list returned by [dta_as_numeric_strict()].
#' @param idx An index vector into the column.
#' @return A character vector of the source text at `idx`.
#' @keywords internal
dta_numeric_raw <- function(entry, idx) {
  as.character(entry$source[idx])
}

#' @title Build a Shared Numeric Conversion Cache for a Set of Rules
#' @description
#' Rules that compare a column numerically each convert that column through
#' [dta_as_numeric_strict()] independently. When several rules read the same
#' column, `apply_rules()` would otherwise convert it once per rule; this
#' collects the columns every rule in `rules` reads numerically and converts
#' each of them exactly once.
#' @param df A data.frame.
#' @param rules A list of `DTARule` objects, or `NULL`.
#' @param columns Character, or `NULL` (the default). The columns the rules read
#'   numerically, when the caller has already derived them. Deriving them means
#'   re-parsing every rule's clause structure -- for `check_col_condition` and
#'   `check_group_condition` rules that is `dta_normalize_conditions()` over
#'   every clause of every rule -- which is invariant across the batches of one
#'   scan. The streaming driver computes the list once before its batch loop and
#'   passes it here so the parse is not repeated per batch. When `NULL` the list
#'   is derived from `rules` exactly as before. A supplied list is filtered
#'   against `names(df)` on the same terms as a derived one, so a rule naming a
#'   column this frame does not have still yields an empty cache.
#' @return A named list mapping column name to the result of
#'   `dta_as_numeric_strict()` for that column. Empty when `rules` is `NULL`,
#'   empty, or names no columns present in `df`.
#' @keywords internal
dta_build_numeric_cache <- function(df, rules, columns = NULL) {
  if (is.null(rules) || length(rules) == 0) {
    return(list())
  }

  if (is.null(columns)) {
    columns <- unique(unlist(lapply(rules, function(rule) {
      tryCatch(dta_rule_numeric_columns(rule), error = function(e) character(0))
    })))
  }
  columns <- columns[columns %in% names(df)]

  if (length(columns) == 0) {
    return(list())
  }

  stats::setNames(
    lapply(columns, function(col) dta_as_numeric_strict(df[[col]])),
    columns
  )
}

#' @title Look Up a Column's Numeric Conversion, Cached or Not
#' @description
#' The single place the fallback to converting on demand lives: when
#' `numeric_cache` already holds the column's conversion, that is reused;
#' otherwise the column is converted directly.
#' @param df A data.frame.
#' @param column Character. The column name.
#' @param numeric_cache A named list produced by
#'   [dta_build_numeric_cache()], or `NULL`.
#' @return The result of `dta_as_numeric_strict()` for that column.
#' @keywords internal
dta_numeric_cache_get <- function(df, column, numeric_cache = NULL) {
  cached <- if (!is.null(numeric_cache)) numeric_cache[[column]] else NULL

  # A cache is only valid for the exact frame it was built from. If a cache
  # built for a larger (or otherwise different) frame is ever passed in
  # alongside a subset of that frame -- e.g. a future refactor that reuses a
  # table-level cache for a filtered copy -- `cached$values` would silently
  # recycle against `df[[column]]`, producing a wrong-length mask and a wrong
  # violation count instead of an error. Checking the length here means a
  # stale-length cache degrades to a correct recomputation rather than a
  # silent wrong verdict.
  if (!is.null(cached) && length(cached$values) == nrow(df)) {
    return(cached)
  }
  dta_as_numeric_strict(df[[column]])
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
#' @param converted The result of `dta_as_numeric_strict(x)`, when already
#'   available, so `x` is not converted twice for the same comparison.
#' @return A list with the numeric `x` and `value`.
#' @keywords internal
dta_numeric_operands <- function(x, value, converted = NULL) {
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

  values <- if (!is.null(converted)) converted$values else dta_as_numeric_strict(x)$values
  list(x = values, value = bound)
}

#' @title Bound of an Equality or Set Operator Against a Numeric Column
#' @description
#' `equals`/`not_equals`/`in`/`not_in` compare `value` against the raw
#' column, so R's implicit coercion decides the outcome whenever the column
#' is numeric: `1e6 == "1000000"` renders the left side via `as.character()`
#' first and is `FALSE`, while `1000000L == "1000000"` is `TRUE` -- the
#' verdict then depends on int-vs-double storage, which legitimately differs
#' between the streamed (per-batch) and eager (whole-table) narrowing
#' decisions. This coerces `value` to numeric first, but only against a
#' numeric column, and only when every element of `value` parses; a bound
#' that does not parse, or a non-numeric column, is returned unchanged so the
#' comparison stays textual exactly as before.
#' @param x The column vector taken from the table.
#' @param value The bound supplied for `equals`, `not_equals`, `in`, or
#'   `not_in`.
#' @return `value` as numeric when `x` is numeric and every element of
#'   `value` parses as a number; `value` unchanged otherwise.
#' @keywords internal
dta_equality_bound <- function(x, value) {
  if (!is.numeric(x)) {
    return(value)
  }

  # bit64::integer64 answers TRUE to is.numeric() but its values do not
  # survive as.numeric() beyond 2^53 -- parsing the bound would round it and
  # break an equality that bit64's own comparison kept exact
  # ("9007199254740993" matched its cell before; the parsed double cannot).
  # Mirrors dta_row_key()'s integer64 guard.
  if (inherits(x, "integer64")) {
    return(value)
  }
  values <- unlist(value, use.names = FALSE)
  if (is.numeric(values)) {
    return(values)
  }
  if (!is.character(values) && !is.factor(values)) {
    return(value)
  }
  parsed <- suppressWarnings(as.numeric(as.character(values)))
  # All-or-nothing: only when EVERY non-missing element parses does the
  # comparison go numeric. A mixed set like c("1", "UNK") keeps today's
  # string behaviour, which is predictable and backward compatible.
  if (any(is.na(parsed) & !is.na(values))) {
    return(value)
  }
  parsed
}

#' @title Rule: check_range
#' @param rule A DTARule object of type `"check_range"`. Expected slots:
#'   - `@id` character
#'   - `@type` = "check_range"
#'   - `@column` character: name of the column to check
#'   - `@range` numeric(2): inclusive lower/upper bounds, e.g. c(0, 1)
#' @param df A data.frame to validate.
#' @param numeric_cache A named list from [dta_build_numeric_cache()] mapping
#'   column name to its cached numeric conversion, or `NULL` to convert on
#'   demand.
#' @description Ensures all non-missing values in `rule@column` fall within
#' an **inclusive** numeric range `[lower, upper]`. Missing values are ignored.
#' @return A list with elements `id`, `valid`, and `message`.
#' @examples
#' # Suppose `rule` is a DTARule with column="age", range=c(18, 65)
#' # rule_check_range(rule, df)
#' @export
rule_check_range <- function(rule, df, numeric_cache = NULL) {
  check_rule_class(rule)
  target <- dta_range_target(rule)
  violated <- dta_range_violated(rule, df, numeric_cache)

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

#' @title Target Columns of a Rule, However They Are Spelled
#' @description
#' A rule may name its target under `column` or `columns`. Four call sites used
#' to restate that fallback independently; this is the one place that resolves
#' it, so a new spelling (or a change in precedence) is a one-line change.
#' @param rule A rule object.
#' @return The slot's value -- a character vector -- or `NULL` when the rule
#'   names no target either way.
#' @keywords internal
dta_rule_target_columns <- function(rule) {
  cols <- rule_get_slot(rule, "column")
  if (is.null(cols)) {
    cols <- rule_get_slot(rule, "columns")
  }
  cols
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
  col <- dta_rule_target_columns(rule)

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
#' @param numeric_cache A named list from [dta_build_numeric_cache()], or
#'   `NULL` to convert the column on demand.
#' @return A logical vector with one element per row.
#' @keywords internal
dta_range_violated <- function(rule, df, numeric_cache = NULL) {
  target <- dta_range_target(rule)
  col <- target$col

  if (!col %in% names(df)) {
    cli::cli_abort(
      "Column '{col}' not found in table.",
      class = "dta_rule_not_applicable"
    )
  }

  converted <- dta_numeric_cache_get(df, col, numeric_cache)
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
  # dta_format_count(), not %d: the streaming path hands in a double counter,
  # and sprintf("%d", <double past .Machine$integer.max>) errors rather than
  # rendering -- after the whole scan has already run.
  sprintf(
    "Rule '%s' violated: %s rows where %s not in range [%s, %s]",
    id, dta_format_count(n), col, range[1], range[2]
  )
}

#' @title Rule: check_unique
#' @param rule A DTARule object of type `"check_unique"`. Expected slots:
#'   - `@id` character
#'   - `@type` = "check_unique"
#'   - `@column` character: name of the column to check
#' @param df A data.frame to validate.
#' @param numeric_cache A named list from [dta_build_numeric_cache()], or
#'   `NULL`. Ignored: uniqueness never compares columns numerically, but the
#'   parameter is accepted so `apply_rules()` can dispatch to every rule
#'   function uniformly.
#' @description Ensures that all values in the specified column are unique.
#' Repeated `NA` values are considered duplicates by base R `duplicated()`.
#' @return A list with elements `id`, `valid`, and `message`.
#' @examples
#' # rule_check_unique(rule, df)
#' @export
rule_check_unique <- function(rule, df, numeric_cache = NULL) {
  check_rule_class(rule)
  cols <- dta_unique_columns(rule)

  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Column{?s} not found in table: {paste(missing_cols, collapse = ', ')}",
      class = "dta_rule_not_applicable"
    )
  }

  # The actual duplicate count is delegated to `dta_count_duplicates()`
  # (R/arrowCompute.R), which owns the R reference implementation (including
  # the double-key fallback documented there) and, only when opted in via
  # `options(DTAtools.use_arrow_compute = TRUE)`, an Arrow-accelerated path.
  n_duplicates <- dta_count_duplicates(df, cols)

  if (n_duplicates > 0) {
    list(
      id = rule@id,
      valid = FALSE,
      message = dta_unique_violation_message(rule@id, n_duplicates, cols)
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
#' @param converted The result of `dta_as_numeric_strict(x)`, when already
#'   available, so a numeric operator does not convert `x` again.
#' @return A logical vector, one element per row of the table.
#' @keywords internal
dta_condition_mask <- function(column_name, operator, value, x, converted = NULL) {
  # Numeric comparisons must compare numbers. Applied to the raw column, `>` on
  # a character vector coerces the bound to character and compares by locale
  # collation, so AGE = c("9", "700") passed `greater: 65` because "9" sorts
  # after "65". Equality and set operators go through the same treatment, but
  # only conditionally (see below); `pattern` and `empty` are unaffected and
  # deliberately stay on the raw column.
  if (operator %in% dta_numeric_condition_operators()) {
    operands <- dta_numeric_operands(x, value, converted)
    x <- operands$x
    value <- operands$value
  }

  # The equality and set operators compare numbers as numbers when both sides
  # are numbers. Left to R's implicit coercion, `x == "1000000"` renders x via
  # as.character(), so a double 1e6 fails a bound its integer twin passes --
  # the verdict then depends on int-vs-double storage, which legitimately
  # differs between the streamed (per-batch) and eager (whole-table)
  # narrowing decisions. A bound that does not parse as a number keeps the
  # string comparison exactly as before, and non-numeric columns (Char ids
  # with leading zeros above all) are never touched.
  if (operator %in% c("equals", "equal", "not_equals", "not_equal", "in", "not_in")) {
    value <- dta_equality_bound(x, value)
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
#' The subset of `dta_condition_operators()` that always go through
#' `dta_numeric_operands()`. Equality and set operators (`equals`,
#' `not_equals`, `in`, `not_in`) also compare numerically, but only
#' conditionally -- see `dta_equality_bound()`; text (`pattern`) and
#' emptiness (`empty`) operators always compare the raw column.
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
#' @param numeric_cache A named list from [dta_build_numeric_cache()], or
#'   `NULL` to convert each column on demand.
#' @return A logical vector with one element per row of `df`, never `NA`.
#' @keywords internal
dta_condition_undecidable <- function(conditions, df, numeric_cache = NULL) {
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

    out <- out | dta_numeric_cache_get(df, column_name, numeric_cache)$unconvertible
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
#' @param numeric_cache A named list from [dta_build_numeric_cache()], or
#'   `NULL` to convert each column on demand.
#' @return A logical vector with one element per row of `df`, never `NA`.
#' @keywords internal
dta_condition_in_scope <- function(conditions, df, if_rows, numeric_cache = NULL) {
  undecided <- is.na(if_rows)
  if (!any(undecided)) {
    return(if_rows)
  }

  if_rows[undecided] <- dta_condition_undecidable(conditions, df, numeric_cache)[undecided]
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
    return(as.character(dta_rule_target_columns(rule)))
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
#' @param numeric_cache A named list from [dta_build_numeric_cache()], or
#'   `NULL` to convert each column on demand.
#' @param columns Character vector of columns to scan, when already known
#'   (e.g. precomputed once per rule by the streaming driver), or `NULL` to
#'   recompute via [dta_rule_numeric_columns()].
#' @return A data.frame in the shape of `dta_empty_import_errors()`.
#' @keywords internal
dta_rule_import_errors <- function(rule, df, numeric_cache = NULL, columns = NULL) {
  if (is.null(columns)) {
    columns <- tryCatch(
      dta_rule_numeric_columns(rule),
      error = function(e) character(0)
    )
  }
  columns <- unique(columns[columns %in% names(df)])

  if (length(columns) == 0) {
    return(dta_empty_import_errors())
  }

  parts <- lapply(columns, function(column) {
    x <- df[[column]]
    # A numeric, logical, Date or POSIXt column can never contain an
    # unconvertible value by construction -- dta_as_numeric_strict() only ever
    # marks a value unconvertible when converting *text* fails, and none of
    # these types are text. Skipping them avoids building the conversion (or a
    # frame) for a column that is provably clean.
    if (is.numeric(x) || is.logical(x) || inherits(x, "Date") || inherits(x, "POSIXt")) {
      return(dta_empty_import_errors())
    }

    converted <- dta_numeric_cache_get(df, column, numeric_cache)
    offending <- which(converted$unconvertible)

    if (length(offending) == 0) {
      return(dta_empty_import_errors())
    }

    data.frame(
      row = as.integer(offending),
      column = column,
      raw = dta_numeric_raw(converted, offending),
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
evaluate_condition <- function(column_name, condition, df, numeric_cache = NULL) {
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

  # If any operator on this column compares numbers, convert the column ONCE
  # here and reuse it for every such operator below (the `min`/`max` band
  # needs it twice by itself). Dates/POSIXt are excluded because
  # dta_numeric_operands() never routes them through this conversion anyway.
  converted <- NULL
  if (any(operators %in% dta_numeric_condition_operators()) &&
    !inherits(x, "Date") && !inherits(x, "POSIXt")) {
    converted <- dta_numeric_cache_get(df, column_name, numeric_cache)
  }

  # `min` and `max` are the one documented pair: together they describe a single
  # inclusive band, so they are consumed as a unit rather than as two operators.
  if (any(c("min", "max") %in% operators)) {
    lower <- if ("min" %in% operators) condition[["min"]] else -Inf
    upper <- if ("max" %in% operators) condition[["max"]] else Inf
    # Same collation trap as the other comparisons: the band is numeric, so
    # both ends and the column are taken as numbers.
    lower_operands <- dta_numeric_operands(x, lower, converted)
    upper_operands <- dta_numeric_operands(x, upper, converted)
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
      x = x,
      converted = converted
    )
  }

  # Every operator supplied for a column must hold: combine with AND.
  # NA propagates, and is treated as a THEN violation by the caller.
  Reduce(`&`, masks)
}

#' @keywords internal
evaluate_conditions <- function(conditions, df, numeric_cache = NULL) {
  conditions <- dta_normalize_conditions(conditions)

  if (length(conditions) == 0L) {
    # No conditions => no restriction (all TRUE)
    return(rep(TRUE, nrow(df)))
  }

  # Iterate over each condition (column name and its rule)
  results <- lapply(names(conditions), function(column_name) {
    condition <- conditions[[column_name]]
    evaluate_condition(column_name, condition, df, numeric_cache)
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
#' Against a numeric column, `equals`/`not_equals`/`in`/`not_in` compare
#' numerically when the supplied value parses as a number (so `equals: "18"`
#' and `equals: 18` agree); otherwise, and for non-numeric columns, the
#' comparison is textual.
#'
#' Both `@condition` and `@then` may also be written as a YAML sequence of
#' single-column mappings; they are normalised to the named form.
#'
#' If `@condition` is empty, the `@then` part applies to **all rows**.
#' @param numeric_cache A named list from [dta_build_numeric_cache()], or
#'   `NULL` to convert each column on demand.
#' @return A list with elements `id`, `valid`, and `message`.
#' @examples
#' # Example: If species == "setosa", then petal_length in [1.0, 1.9]
#' # rule_check_col_condition(rule, iris)
#' @export
rule_check_col_condition <- function(rule, df, numeric_cache = NULL) {
  check_rule_class(rule)

  violated_count <- sum(dta_condition_violated(rule, df, numeric_cache))

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
#' @param numeric_cache A named list from [dta_build_numeric_cache()], or
#'   `NULL` to convert each column on demand.
#' @return A logical vector with one element per row, never `NA`.
#' @keywords internal
dta_condition_violated <- function(rule, df, numeric_cache = NULL) {
  if_rows <- evaluate_conditions(rule@condition, df, numeric_cache)
  then_rows <- evaluate_conditions(rule@then, df, numeric_cache)
  dta_condition_in_scope(rule@condition, df, if_rows, numeric_cache) &
    (is.na(then_rows) | !then_rows)
}

#' @title Message for a Conditional Rule Violation
#' @param id Character. The rule id.
#' @param n Integer. Number of violating rows.
#' @return A single string.
#' @keywords internal
dta_condition_violation_message <- function(id, n) {
  # dta_format_count(), not %d: see dta_range_violation_message().
  sprintf(
    "Rule '%s' violated: %s rows failed the THEN conditions after meeting the IF conditions.",
    id, dta_format_count(n)
  )
}

#' @title Message for a Uniqueness Rule Violation
#' @param id Character. The rule id.
#' @param n Integer. Number of duplicate rows.
#' @param cols Character. The columns forming the key.
#' @return A single string.
#' @keywords internal
dta_unique_violation_message <- function(id, n, cols) {
  # dta_format_count(), not %d: see dta_range_violation_message().
  sprintf(
    "Rule '%s' violated: %s duplicate row found when selecting column(s): %s",
    id, dta_format_count(n), paste(cols, collapse = ", ")
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
  # No as.integer() here: the streaming path keeps row numbers as doubles past
  # .Machine$integer.max on purpose (see the row_offset comments in
  # R/streamingValidation.R), and narrowing turned exactly those rows into NAs
  # that sort() then silently dropped -- the message reported empty row
  # evidence while the verdict still read authoritative. dta_format_count()
  # renders integers and whole doubles identically, in plain digits.
  head_rows <- sort(unique(head_rows))
  if (total > max_show) {
    paste0(
      paste(dta_format_count(head_rows[seq_len(min(max_show, length(head_rows)))]), collapse = ","),
      " (+",
      dta_format_count(total - max_show),
      " more)"
    )
  } else {
    paste(dta_format_count(head_rows), collapse = ",")
  }
}

#' @title Group Key for a Set of Grouping Columns
#' @description
#' The key by which rows are grouped. Shared between the materialising and
#' streaming paths so a group is the same group in both, and built by
#' [dta_row_key()] so that it is shared with the uniqueness key as well: a
#' separator appearing in the data is escaped rather than allowed to merge two
#' distinct groups, and a missing value groups with other missing values rather
#' than forming its own.
#' @param df A data frame.
#' @param group_by Character. The grouping columns.
#' @return A character vector, one key per row.
#' @keywords internal
dta_group_key <- function(df, group_by) {
  dta_row_key(df, group_by)
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
#' @param numeric_cache A named list from [dta_build_numeric_cache()], or
#'   `NULL` to convert each column on demand.
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
rule_check_group_condition <- function(rule, df, numeric_cache = NULL) {
  check_rule_class(rule)

  format_rows <- function(rows, max_show = 10L) {
    # No as.integer(): dta_format_group_rows() renders doubles safely, and the
    # narrowing is what dropped row evidence past .Machine$integer.max.
    rows <- sort(unique(rows))
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

  # Every condition operator -- equals, not_equals, in, not_in, the numeric
  # comparisons, pattern, empty -- is ELEMENTWISE: none of them consult
  # another row. That is what makes evaluate_conditions(spec, df[idx, ])
  # identical to evaluate_conditions(spec, df)[idx], and therefore valid to
  # evaluate each condition ONCE over the whole table and reduce per group
  # with tabulate(), rather than copying every column of `df` per group as
  # `df[row_idx, , drop = FALSE]` used to.
  #
  # Levels sorted with method = "radix", i.e. C-locale byte order, NOT the
  # session locale: group order decides the assembled violation message order,
  # and locale collation made the same file report groups in a different order
  # on a de_DE dev machine than under CI's C collation (and could even split
  # ties differently from the streaming finaliser). The streaming path sorts
  # its keys the same way -- change the two together or the documented
  # streamed/materialised message identity breaks.
  kf <- factor(split_key, levels = sort(unique(split_key), method = "radix"))
  gid <- as.integer(kf)
  n_groups <- nlevels(kf)
  n_seen <- tabulate(gid, nbins = n_groups)
  # `groups_idx` (the row indices belonging to each group) is only read in the
  # `requires` + `then_scope == "all"` branch below, and only for groups that
  # actually violate a constraint -- almost always a small minority. Building
  # it unconditionally for every grouped rule would be an O(nrow) list
  # allocation paid even when it is never consulted, so it is built lazily on
  # first use and memoised for the remainder of this call.
  groups_idx_cache <- NULL
  groups_idx <- function() {
    if (is.null(groups_idx_cache)) {
      groups_idx_cache <<- split(seq_len(nrow(df)), gid)
    }
    groups_idx_cache
  }

  # First row (in original table order) of each group, for the label -- the
  # same row `df[row_idx[1], ]` picked out before. The label string itself is
  # only ever needed for groups that end up violating a constraint (almost
  # always a small minority), so it is built lazily below rather than for
  # every group up front.
  first_row <- match(levels(kf), split_key)
  group_label_for <- function(g) {
    row <- first_row[g]
    paste(
      vapply(group_by, function(col) {
        # dta_group_label_value(), not as.character(): must render identically
        # to the streaming site in dta_group_stream_update() (streamingValidation.R).
        paste0(col, "=", dta_group_label_value(grouped[[col]][row]))
      }, character(1)),
      collapse = ", "
    )
  }

  cond_hits <- lapply(names(conditions), function(cond_name) {
    spec <- conditions[[cond_name]]
    tryCatch(
      evaluate_conditions(spec, df, numeric_cache),
      dta_rule_not_applicable = function(cnd) {
        cli::cli_abort(c(
          "Rule {.val {rule@id}} cannot evaluate condition {.field {cond_name}}.",
          x = "{conditionMessage(cnd)}",
          i = "Condition {.field {cond_name}} is defined as: {.val {paste(utils::capture.output(utils::str(spec, give.attr = FALSE)), collapse = ' ')}}"
        ), class = "dta_rule_not_applicable")
      }
    )
  })
  names(cond_hits) <- names(conditions)

  # `hit` per condition, over the whole table -- the same truth value
  # `mask %in% TRUE` computed once per group before.
  cond_hit <- lapply(cond_hits, function(mask) mask %in% TRUE)
  # Rows where the condition holds, but only for groups that have at least one
  # such row: cheap, because it only touches the (usually few) matching rows,
  # not every group.
  cond_rows_by_group <- lapply(cond_hit, function(hit) split(which(hit), gid[hit]))
  cond_n_true <- lapply(cond_hit, function(hit) tabulate(gid[hit], nbins = n_groups))
  cond_any_true <- lapply(cond_n_true, function(n_true) n_true > 0)
  # Matches dta_group_scope_truth()'s "all": a group with no rows is not
  # vacuously TRUE, hence the `n_seen > 0` guard.
  cond_all_true <- lapply(cond_n_true, function(n_true) n_seen > 0 & n_true == n_seen)

  # `scope_truth_vec()` is the vectorised counterpart of the previous
  # `group_scope_truth(cond_name, scope, g)`: it returns the whole per-group
  # truth vector for a condition/scope pair in one lookup, rather than one
  # element at a time, since cond_any_true/cond_all_true are already vectors
  # over all groups.
  scope_truth_vec <- function(cond_name, scope) {
    if (!cond_name %in% names(conditions)) {
      cli::cli_abort(
        c(
          "Rule {.val {rule@id}} references an unknown condition.",
          x = "Constraint refers to condition {.field {cond_name}}, which is not defined.",
          i = "Defined conditions: {.val {names(conditions)}}."
        ),
        class = "dta_rule_not_applicable"
      )
    }
    if (identical(scope, "all")) {
      cond_all_true[[cond_name]]
    } else {
      cond_any_true[[cond_name]]
    }
  }

  group_rows <- function(cond_name, g) {
    rows <- cond_rows_by_group[[cond_name]][[as.character(g)]]
    if (is.null(rows)) integer(0) else rows
  }

  # Evaluate each constraint's truth condition across ALL groups at once
  # (vectorised, O(n_groups) but with a tiny constant), so that the
  # subsequent per-group work -- building labels, row evidence, and messages
  # -- only ever touches groups that actually violate something. In real
  # data almost no group violates, so this makes the expensive part of the
  # loop proportional to the number of violations rather than the number of
  # groups.
  constraint_viol <- vector("list", length(constraints))
  for (ci in seq_along(constraints)) {
    constraint <- constraints[[ci]]
    ctype <- constraint$type

    if (identical(ctype, "mutually_exclusive")) {
      left_truth <- scope_truth_vec(constraint$left, constraint$left_scope %||% "any")
      right_truth <- scope_truth_vec(constraint$right, constraint$right_scope %||% "any")
      constraint_viol[[ci]] <- left_truth & right_truth
    } else if (identical(ctype, "requires")) {
      if_truth <- scope_truth_vec(constraint[["if"]], constraint$if_scope %||% "any")
      then_truth <- scope_truth_vec(constraint[["then"]], constraint$then_scope %||% "any")
      constraint_viol[[ci]] <- if_truth & !then_truth
    } else {
      constraint_viol[[ci]] <- logical(n_groups)
    }
  }

  violating_groups <- sort(unique(unlist(lapply(constraint_viol, which), use.names = FALSE)))

  violations <- list()

  for (g in violating_groups) {
    group_label <- NULL # computed lazily, once, on first use for this group

    for (ci in seq_along(constraints)) {
      if (!isTRUE(constraint_viol[[ci]][g])) {
        next
      }
      constraint <- constraints[[ci]]
      ctype <- constraint$type

      if (is.null(group_label)) {
        group_label <- group_label_for(g)
      }

      if (identical(ctype, "mutually_exclusive")) {
        left <- constraint$left
        right <- constraint$right

        left_rows <- group_rows(left, g)
        right_rows <- group_rows(right, g)
        message <- constraint$message %||%
          sprintf(
            "In group [%s]: \"%s\" and \"%s\" must not both occur, but both were found (rows matching \"%s\": %s; rows matching \"%s\": %s).",
            group_label,
            left,
            right,
            left,
            format_rows(left_rows),
            right,
            format_rows(right_rows)
          )
        violations[[length(violations) + 1L]] <- list(
          constraint_id = constraint$id,
          group = group_label,
          message = message,
          rows = sort(unique(c(left_rows, right_rows))),
          # The whole table is in hand here, so `rows` is never a head.
          rows_truncated = FALSE
        )
      } else if (identical(ctype, "requires")) {
        if_name <- constraint[["if"]]
        then_name <- constraint[["then"]]
        then_scope <- constraint$then_scope %||% "any"

        if_rows <- group_rows(if_name, g)
        row_idx <- groups_idx()[[as.character(g)]]
        then_failed <- if (identical(then_scope, "all")) {
          row_idx[!cond_hit[[then_name]][row_idx]]
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

  # Built once for every rule rather than once per rule: a column read
  # numerically by several rules would otherwise be strict-converted once per
  # rule that reads it.
  numeric_cache <- dta_build_numeric_cache(df, rules)

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
      rule_functions[[rule_type]](rule, df, numeric_cache),
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
    result$import_errors <- dta_rule_import_errors(rule, df, numeric_cache)

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
