# Schema-axis validation without a JSON Schema validator.
#
# This replaces the chunk -> jsonlite::toJSON -> ajv-on-V8 loop that previously
# dominated validation cost (~95% of total runtime, measured; see
# benchmarks/bench_validation.R). The constraint vocabulary the package can
# emit is only five keywords wide -- type, maxLength, enum/const, pattern and
# required -- so the validator was doing a general-purpose job for a closed,
# small problem.
#
# Constraint DERIVATION is deliberately unchanged: each column's constraints
# still come from `as_json_schema()`, which remains the single source of truth
# for what a spec means. Only the EVALUATION is reimplemented, as vectorised R
# over whole columns. That keeps the risk contained to "does this evaluate the
# same schema the same way", which the oracle in
# tests/testthat/test-validation-oracle.R answers directly.
#
# Message strings deliberately match the ones the previous validator produced.
# Nothing forces that -- but keeping them means `messages()`, `inspect()`, the
# vignette and the README need no revision, and a reader's error text does not
# churn for an internal change.

# ---- JSON type of an R value ------------------------------------------------

#' @title JSON Type of a Column
#' @description
#' The type this column's values would have carried after
#' `jsonlite::toJSON(..., na = "null")`, which is what the previous validator
#' actually saw.
#'
#' This is a scalar, not a vector, because the JSON type is a property of the
#' column rather than of each value: only missingness varies down a column, and
#' that is carried separately as a logical mask. The one genuine per-element
#' distinction -- a whole-valued double serialises without a decimal point and
#' so satisfies `integer` as well as `number` -- is resolved by the caller, and
#' only in the rare case where it can change the outcome.
#' @param x A vector.
#' @return A single JSON type name.
#' @keywords internal
dta_base_json_type <- function(x) {
  if (is.factor(x)) {
    return("string")
  }
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    # Serialised as strings, so that is the type the schema sees.
    return("string")
  }
  if (is.character(x)) {
    return("string")
  }
  if (is.logical(x)) {
    return("boolean")
  }
  if (is.integer(x)) {
    return("integer")
  }
  if (is.numeric(x)) {
    return("number")
  }
  "string"
}

# A value satisfies "number" when it is a number or an integer; "integer" only
# when it is whole. Every other type matches by name.
dta_base_type_ok <- function(base_type, allowed) {
  base_type %in% allowed || (base_type == "integer" && "number" %in% allowed)
}

# ---- error frame construction -----------------------------------------------

dta_empty_schema_errors <- function() {
  data.frame(
    row = integer(0),
    column = character(0),
    keyword = character(0),
    message = character(0),
    schema = character(0),
    data = character(0),
    stringsAsFactors = FALSE
  )
}

dta_schema_error_rows <- function(rows, column, keyword, message, schema, values) {
  if (length(rows) == 0) {
    return(NULL)
  }
  data.frame(
    row = as.integer(rows),
    column = column,
    keyword = keyword,
    message = message,
    schema = schema,
    data = as.character(values),
    stringsAsFactors = FALSE
  )
}

# ---- per-column evaluation --------------------------------------------------

#' @title Schema Violations for One Column
#' @description
#' Evaluates every constraint `as_json_schema()` emitted for a single column
#' against that column's values, returning one row per violated constraint per
#' value. All constraints are evaluated independently, so one value can produce
#' several errors -- matching the previous validator's greedy behaviour.
#' @param column_name Character. Name of the column.
#' @param col_schema List. The column's schema, from `as_json_schema()`.
#' @param x The column's values.
#' @return A data frame of violations, or `NULL` when there are none.
#' @keywords internal
dta_column_schema_errors <- function(column_name, col_schema, x) {
  n <- length(x)
  has_na <- anyNA(x)
  base_type <- dta_base_json_type(x)
  parts <- list()

  # Most columns in a real table have no missing values at all, and anyNA() is
  # far cheaper than building the mask. Only materialise it when it is needed.
  na_mask_cache <- NULL
  na_mask <- function() {
    if (is.null(na_mask_cache)) {
      na_mask_cache <<- is.na(x)
    }
    na_mask_cache
  }

  # The character form of the column is needed only by the string constraints,
  # and its values only for rows that actually fail. Materialising it up front
  # for every column was the dominant cost of this function.
  as_text <- function(idx) as.character(x[idx])
  text_column <- NULL
  string_text <- function() {
    if (is.null(text_column)) {
      text_column <<- if (is.character(x)) x else as.character(x)
    }
    text_column
  }

  # type ----------------------------------------------------------------------
  allowed <- col_schema$type
  if (!is.null(allowed)) {
    type_ok <- dta_base_type_ok(base_type, allowed)
    null_ok <- "null" %in% allowed
    # The only case where the answer varies down the column: a double column
    # against a schema that accepts integers but not numbers. A whole value
    # serialises without a decimal point and so satisfies it.
    elementwise <- !type_ok && base_type == "number" && "integer" %in% allowed

    bad <- if (!elementwise && type_ok && (null_ok || !has_na)) {
      # The whole column conforms and nothing can fail, so no per-row vector is
      # built at all. This is the common case for a table that is in spec.
      integer(0)
    } else {
      ok <- if (elementwise) {
        is.finite(x) & x == trunc(x)
      } else {
        rep(type_ok, n)
      }
      if (has_na) {
        ok[na_mask()] <- null_ok
      }
      which(!ok)
    }

    parts[[length(parts) + 1]] <- dta_schema_error_rows(
      bad, column_name, "type",
      paste0("must be ", paste(allowed, collapse = ",")),
      paste(allowed, collapse = ","),
      as_text(bad)
    )
  }

  # maxLength -----------------------------------------------------------------
  # Applies to strings only, and counts characters rather than bytes.
  max_length <- col_schema$maxLength
  if (!is.null(max_length) && !is.na(max_length) && base_type == "string") {
    txt <- string_text()

    # The limit is in characters, but counting characters is UTF-8-aware and
    # markedly slower than counting bytes. A string's byte length is never less
    # than its character length, so anything within the limit in bytes is
    # certainly within it in characters. The cheap count rules out almost every
    # value, and the exact count runs only on what survives -- for ASCII data,
    # nothing. `nchar(NA)` is 2, so missing values are excluded explicitly
    # rather than left to the comparison.
    within_bytes <- nchar(txt, type = "bytes") <= max_length
    candidates <- if (has_na) {
      which(!within_bytes & !na_mask())
    } else {
      which(!within_bytes)
    }

    bad <- if (length(candidates) == 0) {
      integer(0)
    } else {
      candidates[nchar(txt[candidates], type = "chars") > max_length]
    }

    parts[[length(parts) + 1]] <- dta_schema_error_rows(
      bad, column_name, "maxLength",
      paste0("must NOT have more than ", max_length, " characters"),
      as.character(max_length),
      as_text(bad)
    )
  }

  # enum / const --------------------------------------------------------------
  if (!is.null(col_schema$enum)) {
    allowed_values <- col_schema$enum
    # `%in%` matches NA against NA, which is what the generated schema relies on
    # when it appends NA to a nullable column's permitted values.
    bad <- which(!(x %in% allowed_values))
    parts[[length(parts) + 1]] <- dta_schema_error_rows(
      bad, column_name, "enum",
      "must be equal to one of the allowed values",
      paste(allowed_values, collapse = "; "),
      as_text(bad)
    )
  } else if (!is.null(col_schema$const)) {
    const_value <- col_schema$const
    bad <- which(!(x %in% const_value))
    parts[[length(parts) + 1]] <- dta_schema_error_rows(
      bad, column_name, "const",
      "must be equal to constant",
      as.character(const_value),
      as_text(bad)
    )
  }

  # pattern -------------------------------------------------------------------
  # Applies to strings only. The generated schema's patterns were previously
  # evaluated as ECMAScript regex; PCRE is used here, which agrees for the
  # character-class and anchor constructs these specs use.
  pattern <- col_schema$pattern
  if (!is.null(pattern) && !is.na(pattern) && base_type == "string") {
    present <- if (has_na) !na_mask() else rep(TRUE, n)
    matched <- rep(FALSE, n)
    matched[present] <- grepl(pattern, string_text()[present], perl = TRUE)
    bad <- which(present & !matched)
    parts[[length(parts) + 1]] <- dta_schema_error_rows(
      bad, column_name, "pattern",
      paste0("must match pattern \"", pattern, "\""),
      pattern,
      as_text(bad)
    )
  }

  parts <- Filter(Negate(is.null), parts)
  if (length(parts) == 0) {
    return(NULL)
  }
  do.call(rbind, parts)
}

# ---- whole-table evaluation -------------------------------------------------

#' @title Schema-Axis Validation of a Table
#' @description
#' Validates a table against its column specs, producing the same
#' `summarised_error` / `full_error` pair the JSON Schema validator produced.
#'
#' A column named by the specs but absent from the table yields one `required`
#' error per row, which is what the generated `type: array` schema meant: the
#' property is required of every element. That is faithful, and it is also why
#' a structural check belongs ahead of any row scan -- one absent column in a
#' 400M-row table produces 400M errors.
#' @param specs A `DTAColumnSpecCollection`.
#' @param table A data frame.
#' @return A list with `summarised_error` and `full_error`, each `NULL` when the
#'   table is valid.
#' @keywords internal
dta_schema_errors <- function(specs, table) {
  columns <- tryCatch(specs@columns, error = function(e) NULL)
  n_rows <- nrow(table)

  if (is.null(columns) || length(columns) == 0 || n_rows == 0) {
    return(list(summarised_error = NULL, full_error = NULL))
  }

  spec_names <- names(columns)
  if (is.null(spec_names)) {
    spec_names <- vapply(columns, function(s) s@id, character(1))
  }

  parts <- list()

  for (i in seq_along(columns)) {
    column_name <- spec_names[[i]]

    if (!column_name %in% names(table)) {
      # Object-level failure: reported for every row, as the array schema meant.
      parts[[length(parts) + 1]] <- data.frame(
        row = seq_len(n_rows),
        column = NA_character_,
        keyword = "required",
        message = paste0("must have required property '", column_name, "'"),
        schema = column_name,
        data = NA_character_,
        stringsAsFactors = FALSE
      )
      next
    }

    col_schema <- tryCatch(
      as_json_schema(columns[[i]]),
      error = function(e) NULL
    )
    if (is.null(col_schema)) {
      next
    }

    errs <- dta_column_schema_errors(column_name, col_schema, table[[column_name]])
    if (!is.null(errs)) {
      # Preserve the spec's column order within a row.
      errs$.col_order <- i
      parts[[length(parts) + 1]] <- errs
    }
  }

  parts <- Filter(Negate(is.null), parts)
  if (length(parts) == 0) {
    return(list(summarised_error = NULL, full_error = NULL))
  }

  # Missing-column errors carry no column order; they are object-level and sort
  # ahead of the property errors for the same row.
  parts <- lapply(parts, function(p) {
    if (is.null(p$.col_order)) p$.col_order <- 0L
    p
  })

  full_error <- do.call(rbind, parts)
  full_error <- full_error[order(full_error$row, full_error$.col_order), , drop = FALSE]
  full_error$.col_order <- NULL
  rownames(full_error) <- NULL

  list(
    summarised_error = dta_summarise_schema_errors(full_error),
    full_error = full_error
  )
}

#' @title Group Schema Violations into a Summary
#' @description
#' Collapses repeated violations of the same constraint into one row spanning
#' the affected rows. A missing required column is summarised by constraint
#' alone, because reporting a row range for a column that is absent everywhere
#' adds nothing.
#' @param full_error A data frame of violations.
#' @return A summarised data frame.
#' @importFrom rlang .data
#' @keywords internal
dta_summarise_schema_errors <- function(full_error) {
  if (is.null(full_error) || nrow(full_error) == 0) {
    return(NULL)
  }

  if (any(full_error$keyword == "required")) {
    required <- full_error[full_error$keyword == "required", c("keyword", "message"), drop = FALSE]
    out <- unique(required)
    rownames(out) <- NULL
    return(out)
  }

  grouped <- full_error %>%
    dplyr::group_by(dplyr::across(-"row")) %>%
    dplyr::summarise(
      first.row.affected = min(.data$row),
      last.row.affected = max(.data$row),
      n.rows.affected = dplyr::n(),
      .groups = "drop"
    )

  as.data.frame(grouped, stringsAsFactors = FALSE)
}
