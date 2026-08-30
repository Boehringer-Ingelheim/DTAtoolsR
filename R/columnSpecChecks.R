# Column-spec-axis validation without a JSON Schema validator.
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

dta_empty_columnspec_errors <- function() {
  data.frame(
    row = integer(0),
    column = character(0),
    keyword = character(0),
    message = character(0),
    columnspec = character(0),
    data = character(0),
    stringsAsFactors = FALSE
  )
}

dta_columnspec_error_rows <- function(rows, column, keyword, message, columnspec, values) {
  if (length(rows) == 0) {
    return(NULL)
  }
  data.frame(
    row = as.integer(rows),
    column = column,
    keyword = keyword,
    message = message,
    columnspec = columnspec,
    data = as.character(values),
    stringsAsFactors = FALSE
  )
}

# ---- per-column evaluation --------------------------------------------------

#' @title Column Spec Violations for One Column
#' @description
#' Evaluates every constraint `as_json_schema()` emitted for a single column
#' against that column's values, returning one row per violated constraint per
#' value. All constraints are evaluated independently, so one value can produce
#' several errors -- matching the previous validator's greedy behaviour.
#' @param column_name Character. Name of the column.
#' @param col_spec List. The column's schema, from `as_json_schema()`.
#' @param x The column's values.
#' @return A data frame of violations, or `NULL` when there are none.
#' @keywords internal
dta_check_column_spec <- function(column_name, col_spec, x) {
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
  allowed <- col_spec$type
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

    parts[[length(parts) + 1]] <- dta_columnspec_error_rows(
      bad, column_name, "type",
      paste0("must be ", paste(allowed, collapse = ",")),
      paste(allowed, collapse = ","),
      as_text(bad)
    )
  }

  # maxLength -----------------------------------------------------------------
  # Applies to strings only, and counts characters rather than bytes.
  max_length <- col_spec$maxLength
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

    parts[[length(parts) + 1]] <- dta_columnspec_error_rows(
      bad, column_name, "maxLength",
      paste0("must NOT have more than ", max_length, " characters"),
      as.character(max_length),
      as_text(bad)
    )
  }

  # enum / const --------------------------------------------------------------
  if (!is.null(col_spec$enum)) {
    allowed_values <- col_spec$enum
    # `%in%` matches NA against NA, which is what the generated schema relies on
    # when it appends NA to a nullable column's permitted values.
    bad <- which(!(x %in% allowed_values))
    parts[[length(parts) + 1]] <- dta_columnspec_error_rows(
      bad, column_name, "enum",
      "must be equal to one of the allowed values",
      paste(allowed_values, collapse = "; "),
      as_text(bad)
    )
  } else if (!is.null(col_spec$const)) {
    const_value <- col_spec$const
    bad <- which(!(x %in% const_value))
    parts[[length(parts) + 1]] <- dta_columnspec_error_rows(
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
  pattern <- col_spec$pattern
  if (!is.null(pattern) && !is.na(pattern) && base_type == "string") {
    present <- if (has_na) !na_mask() else rep(TRUE, n)
    matched <- rep(FALSE, n)
    matched[present] <- grepl(pattern, string_text()[present], perl = TRUE)
    bad <- which(present & !matched)
    parts[[length(parts) + 1]] <- dta_columnspec_error_rows(
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

# ---- compiled specs ---------------------------------------------------------

#' @title Compile a Collection's Column Schemas Once
#' @description
#' Derives the `as_json_schema()` form of every column in a collection, paired
#' with the name that column is expected to carry in the data.
#'
#' A column's schema is a pure function of its `DTAColumnSpec`, which does not
#' change while a table is being validated. Deriving it is nevertheless several
#' S7 dispatches deep -- `as_json_schema()` calls `as_json_schema_type()`, which
#' dispatches again on the structure -- so on the streaming path, where
#' `dta_columnspec_errors()` is called once per batch, deriving it inside that
#' call repeated the whole derivation once per column per batch. A 500-column
#' spec scanned in 1000 batches paid for it 500,000 times to obtain 500 distinct
#' answers.
#'
#' Compiling once and passing the result in makes that cost proportional to the
#' spec rather than to the data.
#' @param specs A `DTAColumnSpecCollection`.
#' @return A list with one element per column, each a list of `name` (the
#'   expected column name) and `schema` (the column's schema, or `NULL` when it
#'   could not be derived).
#' @keywords internal
dta_compile_columnspec_schemas <- function(specs) {
  columns <- tryCatch(specs@columns, error = function(e) NULL)
  if (is.null(columns) || length(columns) == 0) {
    return(list())
  }

  # Per-column fallback rather than all-or-nothing: a PARTIALLY named
  # collection (reachable only by validator bypass) would otherwise pair a
  # schema with the name "", which can never match a table column.
  spec_names <- names(columns)
  if (is.null(spec_names)) {
    spec_names <- rep("", length(columns))
  }
  ids <- vapply(
    columns,
    function(s) tryCatch(as.character(s@id)[[1]], error = function(e) NA_character_),
    character(1),
    USE.NAMES = FALSE
  )
  fallback <- is.na(spec_names) | !nzchar(spec_names)
  spec_names[fallback] <- ids[fallback]

  lapply(seq_along(columns), function(i) {
    list(
      name = spec_names[[i]],
      schema = tryCatch(as_json_schema(columns[[i]]), error = function(e) NULL)
    )
  })
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
#' @param schemas Optional. The collection's compiled schemas, from
#'   [dta_compile_columnspec_schemas()]. Derived from `specs` when absent.
#'   Callers that validate many tables against one collection -- the streaming
#'   path, which calls this once per batch -- should compile once and pass the
#'   result in, so that the derivation costs the spec rather than the data.
#' @param summarise Logical. Whether to also build `summarised_error`. The
#'   summary is a grouped `dplyr` aggregation whose cost scales with the number
#'   of violations *and* with the cardinality of the offending values -- the
#'   `data` column is part of the grouping key, so a batch of mostly-distinct
#'   bad cells is the expensive case. The streaming driver reads only
#'   `full_error` from each batch and recomputes the summary once at the end
#'   over the collected frame, so it passes `FALSE` and pays for that
#'   aggregation once per scan instead of once per batch. Defaults to `TRUE`,
#'   which is what the materialising path needs.
#' @return A list with `summarised_error` and `full_error`, each `NULL` when the
#'   table is valid. `summarised_error` is always `NULL` when `summarise` is
#'   `FALSE`; the shape of the returned list is unchanged either way.
#' @keywords internal
dta_columnspec_errors <- function(specs, table, schemas = NULL, summarise = TRUE) {
  n_rows <- nrow(table)
  if (n_rows == 0) {
    return(list(summarised_error = NULL, full_error = NULL))
  }

  if (is.null(schemas)) {
    schemas <- dta_compile_columnspec_schemas(specs)
  }

  if (length(schemas) == 0) {
    return(list(summarised_error = NULL, full_error = NULL))
  }

  table_names <- names(table)
  parts <- list()

  for (i in seq_along(schemas)) {
    column_name <- schemas[[i]]$name

    if (!column_name %in% table_names) {
      # Object-level failure: reported for every row, as the array schema meant.
      parts[[length(parts) + 1]] <- data.frame(
        row = seq_len(n_rows),
        column = NA_character_,
        keyword = "required",
        message = paste0("must have required property '", column_name, "'"),
        columnspec = column_name,
        data = NA_character_,
        stringsAsFactors = FALSE
      )
      next
    }

    col_spec <- schemas[[i]]$schema
    if (is.null(col_spec)) {
      next
    }

    errs <- dta_check_column_spec(column_name, col_spec, table[[column_name]])
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
    summarised_error = if (isTRUE(summarise)) {
      dta_summarise_columnspec_errors(full_error)
    } else {
      NULL
    },
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
dta_summarise_columnspec_errors <- function(full_error) {
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

# ---- per-check reporting ----------------------------------------------------
#
# The constraint vocabulary is closed and six keywords wide, so "did the table
# pass the column specs" can be answered per check kind rather than as one
# lumped verdict. That is what this section builds: a fixed-shape summary of the
# column spec axis, carried on the details list and rendered to the console by
# both the materialising and the streaming path.
#
# It exists because the axis used to report a single success line -- "Table
# format, length, pattern, and values are valid" -- and NOTHING at all when it
# failed. A reader of a failing run saw the section header, then the rules
# passing, then a FAILED verdict with no stated cause on this axis. The lumped
# line was also over-broad in the other direction: it asserted that patterns
# were valid on a table where no column declared one.
#
# `enum` and `const` are two spellings of one idea -- the declared value list --
# so they share the label "values" while keeping a row each; one row per keyword
# is what joins to `full_error$keyword` without translation.

#' @title The Column Spec Check Vocabulary
#' @description
#' Every constraint keyword `as_json_schema()` can emit, paired with the human
#' label the console report groups it under. The row order is the report order.
#' @return A data frame with columns `check` and `keyword`.
#' @keywords internal
dta_columnspec_check_kinds <- function() {
  data.frame(
    check = c("presence", "format", "length", "values", "values", "pattern"),
    keyword = c("required", "type", "maxLength", "enum", "const", "pattern"),
    stringsAsFactors = FALSE
  )
}

#' @title Which Columns Declare Each Constraint
#' @description
#' Reads the compiled schemas to find, for each keyword, the columns whose spec
#' declares it. This is the denominator of the per-check report: a keyword no
#' column declares is `not_applicable`, never `passed`, because reporting a pass
#' for a check that could not run is the same hollow certificate `check()`
#' refuses to issue for a spec that declares no columns at all.
#'
#' Declaration, not evaluation: `maxLength` and `pattern` are evaluated only
#' against string columns (see `dta_check_column_spec()`), so a column that
#' declares a length but holds numbers is counted here and skipped there. Such a
#' column always fails the `type` check as well, so the mismatch is still
#' reported -- on the axis that can name it.
#' @param schemas Compiled schemas, from `dta_compile_columnspec_schemas()`.
#' @return A named list, one character vector of column names per keyword.
#' @keywords internal
dta_columnspec_declared_columns <- function(schemas) {
  keywords <- dta_columnspec_check_kinds()$keyword
  out <- stats::setNames(rep(list(character(0)), length(keywords)), keywords)

  if (length(schemas) == 0) {
    return(out)
  }

  column_names <- vapply(
    schemas,
    function(entry) {
      nm <- entry$name
      if (is.character(nm) && length(nm) == 1 && !is.na(nm) && nzchar(nm)) {
        nm
      } else {
        NA_character_
      }
    },
    character(1),
    USE.NAMES = FALSE
  )
  named <- !is.na(column_names)

  # `entry$schema` is NULL when the derivation failed; `NULL$keyword` is NULL,
  # so every test below is FALSE for such a column rather than an error.
  declares <- function(test) {
    keep <- vapply(schemas, function(entry) isTRUE(test(entry$schema)), logical(1))
    unique(column_names[keep & named])
  }

  # A column whose schema could not be derived is still required to be present:
  # `dta_columnspec_errors()` reports the missing-column error before it looks
  # at the schema at all.
  out$required <- unique(column_names[named])
  out$type <- declares(function(s) !is.null(s$type))
  out$maxLength <- declares(function(s) !is.null(s$maxLength) && !anyNA(s$maxLength))
  out$enum <- declares(function(s) !is.null(s$enum))
  # The `else if` in `dta_check_column_spec()`: `enum` wins when a schema
  # somehow carries both, so `const` is only in force where `enum` is absent.
  out$const <- declares(function(s) is.null(s$enum) && !is.null(s$const))
  out$pattern <- declares(function(s) !is.null(s$pattern) && !anyNA(s$pattern))
  out
}

#' @title Empty Per-Keyword Violation Tally
#' @description
#' The zero state of the tally `dta_columnspec_check_summary()` consumes. Every
#' keyword is always present, so callers index it with `[[` without first
#' testing for the name -- `[[` on an atomic vector throws for a name that is
#' absent, it does not return `NULL`.
#' @return A list of `n_errors` (named numeric) and `columns` (named list).
#' @keywords internal
dta_empty_columnspec_tally <- function() {
  keywords <- dta_columnspec_check_kinds()$keyword
  list(
    n_errors = stats::setNames(rep(0, length(keywords)), keywords),
    columns = stats::setNames(rep(list(character(0)), length(keywords)), keywords)
  )
}

#' @title Add One Error Frame to a Tally
#' @description
#' Accumulates a batch's violations into a running tally. Counts are doubles
#' rather than integers for the reason given above `dta_narrow_count()`: a
#' missing column contributes one error per row, so on the files the streaming
#' path exists for, the count passes `.Machine$integer.max` and an integer
#' accumulator would silently become `NA`.
#' @param tally A tally, from `dta_empty_columnspec_tally()`.
#' @param full_error A column spec error frame, or `NULL`.
#' @return The updated tally.
#' @keywords internal
dta_columnspec_tally_add <- function(tally, full_error) {
  if (!is.data.frame(full_error) || nrow(full_error) == 0) {
    return(tally)
  }

  keyword <- as.character(full_error$keyword)

  # A missing-column error names the column it is about in `columnspec`, not in
  # `column`: the finding is about the object, not about a property of one, so
  # `column` is NA there. Without the fallback the presence check would report
  # failures in zero columns.
  column <- as.character(full_error$column)
  fallback <- is.na(column)
  if (any(fallback) && "columnspec" %in% names(full_error)) {
    column[fallback] <- as.character(full_error$columnspec)[fallback]
  }

  for (kw in names(tally$n_errors)) {
    hit <- which(keyword == kw)
    if (length(hit) == 0) {
      next
    }
    tally$n_errors[[kw]] <- tally$n_errors[[kw]] + length(hit)
    seen <- column[hit]
    tally$columns[[kw]] <- unique(c(tally$columns[[kw]], seen[!is.na(seen)]))
  }

  tally
}

#' @title Tally Violations by Keyword
#' @description
#' Counts one error frame per keyword and collects the columns each keyword
#' failed in. Used by the materialising path, which has the whole frame at once;
#' the streaming path accumulates the same tally batch by batch with
#' `dta_columnspec_tally_add()` so that its counts stay exact when the retained
#' error cap truncates the frame it keeps.
#' @param full_error A column spec error frame, or `NULL`.
#' @return A tally, in the shape of `dta_empty_columnspec_tally()`.
#' @keywords internal
dta_columnspec_error_tally <- function(full_error) {
  dta_columnspec_tally_add(dta_empty_columnspec_tally(), full_error)
}

#' @title Per-Check Summary of the Column Spec Axis
#' @description
#' Builds the fixed-shape frame carried as `details$columnspec_checks`: one row
#' per constraint keyword, saying whether that check passed, failed, was never
#' applicable, or could not be settled.
#'
#' The four statuses are deliberately distinct. `not_applicable` means no column
#' declares the constraint, so there was nothing to check; `not_checked` means
#' there was, but the scan could not reach a verdict -- a table with no rows, a
#' `fail_fast` run that stopped at the first problem, a structural early return
#' that read no rows at all, or a constraint whose every declaring column is
#' absent from the table. Neither is a pass, and neither is a failure. A keyword
#' with violations is `failed` whatever else is true of the scan: a found error
#' is certain even when the scan that found it was cut short.
#'
#' A column the table does not have is subtracted from every check but
#' `required`. Its type, length, pattern and permitted values were all
#' undefined, not satisfied -- and reporting "format check passed (4 columns)"
#' for a table holding three of them stated a result for a column nothing ever
#' looked at.
#' @param schemas Compiled schemas, from `dta_compile_columnspec_schemas()`.
#' @param tally A tally, from `dta_columnspec_error_tally()`. Defaults to the
#'   zero state.
#' @param settled Character. The keywords whose absence of violations may be
#'   reported as a pass. Defaults to all of them; pass a subset (or
#'   `character(0)`) for a scan that could not settle them.
#' @return A data frame with one row per keyword and columns `check`,
#'   `keyword`, `status`, `columns_declared`, `columns_checked`,
#'   `columns_failed`, `n_errors` and `failed_columns` (the failing column
#'   names, `NA` when there are none).
#' @keywords internal
dta_columnspec_check_summary <- function(schemas,
                                         tally = NULL,
                                         settled = NULL) {
  kinds <- dta_columnspec_check_kinds()
  keywords <- kinds$keyword

  if (is.null(tally)) {
    tally <- dta_empty_columnspec_tally()
  }
  if (is.null(settled)) {
    settled <- keywords
  }

  declared <- dta_columnspec_declared_columns(schemas)

  # The columns the table does not have, taken from the presence check's own
  # failures rather than re-derived: whichever path produced the tally, its
  # `required` violations name exactly the columns nothing else could examine.
  absent <- tally$columns[["required"]]

  columns_declared <- vapply(
    keywords, function(kw) length(declared[[kw]]), integer(1),
    USE.NAMES = FALSE
  )
  columns_checked <- vapply(
    keywords,
    function(kw) {
      # Presence is decidable for every declared column; an absent one is this
      # check's finding, not its blind spot.
      if (identical(kw, "required")) {
        length(declared[[kw]])
      } else {
        length(setdiff(declared[[kw]], absent))
      }
    },
    integer(1),
    USE.NAMES = FALSE
  )
  n_errors <- vapply(
    keywords, function(kw) as.numeric(tally$n_errors[[kw]]), numeric(1),
    USE.NAMES = FALSE
  )
  failed <- lapply(keywords, function(kw) tally$columns[[kw]])

  # Violations are tested FIRST, ahead of applicability. The two can only
  # disagree when the caller could not supply the schemas, and a check that
  # produced errors is failed whatever the denominator says -- reporting it as
  # "not applicable" would file real findings under "nothing to check here".
  status <- vapply(seq_along(keywords), function(i) {
    if (n_errors[[i]] > 0) {
      return("failed")
    }
    if (columns_declared[[i]] == 0) {
      return("not_applicable")
    }
    # Declared, but every column that declares it is missing from the table.
    if (columns_checked[[i]] == 0) {
      return("not_checked")
    }
    if (keywords[[i]] %in% settled) "passed" else "not_checked"
  }, character(1))

  out <- data.frame(
    check = kinds$check,
    keyword = keywords,
    status = status,
    columns_declared = columns_declared,
    columns_checked = columns_checked,
    columns_failed = vapply(failed, length, integer(1), USE.NAMES = FALSE),
    # Narrowed, not as.integer()'d: a missing column in a table past
    # `.Machine$integer.max` rows genuinely counts higher than an integer holds.
    n_errors = dta_narrow_rows(n_errors),
    failed_columns = vapply(
      failed,
      function(cols) if (length(cols) == 0) NA_character_ else paste(cols, collapse = ", "),
      character(1),
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

# Display labels and the reason a whole label is inapplicable. Keyed by the
# `check` column of `dta_columnspec_check_kinds()`, so the two cannot drift.
`__DTAtools_columnspec_check_labels__` <- c(
  presence = "Presence",
  format = "Format",
  length = "Length",
  values = "Values",
  pattern = "Pattern"
)

`__DTAtools_columnspec_check_absent__` <- c(
  presence = "the specs declare no columns",
  format = "no column spec declares a type",
  length = "no column spec declares a length",
  values = "no column spec declares a value list",
  pattern = "no column spec declares a pattern"
)

#' @title Print the Per-Check Column Spec Report
#' @description
#' Renders `details$columnspec_checks` to the console: one line per check kind
#' followed by a summary, mirroring how the rule axis reports one line per rule
#' followed by its own summary. Called by both the materialising and the
#' streaming path, so the two cannot report the same verdict differently.
#'
#' Rows sharing a `check` label are folded into one line -- `enum` and `const`
#' are both the "values" check -- and a column declares at most one of them, so
#' the folded counts are sums rather than an over-count.
#' @param checks A summary frame, from `dta_columnspec_check_summary()`.
#' @param unchecked_reason Character. Why a `not_checked` row could not be
#'   settled, phrased to complete "... not checked: <reason>".
#' @return `NULL`, invisibly. Called for the console output.
#' @keywords internal
dta_report_columnspec_checks <- function(checks,
                                         unchecked_reason = "the scan did not evaluate it") {
  if (!is.data.frame(checks) || nrow(checks) == 0) {
    return(invisible(NULL))
  }

  labels <- unique(checks$check)
  outcome <- character(length(labels))

  for (i in seq_along(labels)) {
    label <- labels[[i]]
    rows <- checks[checks$check == label, , drop = FALSE]
    title <- `__DTAtools_columnspec_check_labels__`[[label]]

    n_declared <- sum(rows$columns_declared)
    n_checked <- sum(rows$columns_checked)
    n_errors <- sum(rows$n_errors)
    n_columns <- sum(rows$columns_failed)
    # `as.character()` rather than the bare `unlist()`: unlisting an empty list
    # yields NULL, and `cli::cli_vec(NULL)` fails trying to set an attribute on
    # it -- on the passing checks, which is every check of a clean table.
    failed_columns <- as.character(unlist(
      strsplit(as.character(stats::na.omit(rows$failed_columns)), ", ", fixed = TRUE),
      use.names = FALSE
    ))

    if (any(rows$status == "failed")) {
      outcome[[i]] <- "failed"
      # Long enough to name the offenders, short enough that a 400-column table
      # does not print a paragraph per check.
      columns <- cli::cli_vec(failed_columns, list("vec-trunc" = 5))
      if (identical(label, "presence")) {
        cli::cli_alert_danger(
          "{title} check failed: {n_columns} of {n_declared} declared column{?s} missing: {.field {columns}}"
        )
      } else {
        # `cli::qty()` sits between the rendered count and the noun, not before
        # the count: cli takes the quantity from the LAST interpolation ahead of
        # the marker, and `dta_format_count()` returns a length-1 string -- so
        # putting the qty first made every count read as singular ("3 value").
        # The count is rendered rather than interpolated directly because past
        # `.Machine$integer.max` it is a double, which cli would otherwise print
        # in scientific notation.
        #
        # And the quantity handed to `qty()` is collapsed to 1-or-2 rather than
        # passed through. `cli::qty()` coerces to integer, so a genuine count
        # past the integer range became `NA` and cli then aborted the whole
        # report with "Multiple quantities for pluralization" -- after a scan
        # that had already run for hours. Only "is it one" matters here.
        n_errors_qty <- if (n_errors == 1) 1L else 2L
        cli::cli_alert_danger(
          "{title} check failed: {dta_format_count(n_errors)}{cli::qty(n_errors_qty)} value{?s} in {n_columns} of {n_checked} column{?s}: {.field {columns}}"
        )
      }
    } else if (all(rows$status == "not_applicable")) {
      outcome[[i]] <- "not_applicable"
      reason <- `__DTAtools_columnspec_check_absent__`[[label]]
      cli::cli_alert_info("{title} check not applicable: {reason}")
    } else if (any(rows$status == "not_checked")) {
      outcome[[i]] <- "not_checked"
      # Two different reasons reach the same status. "Every column that declares
      # it is missing" is a property of this check, and saying "the table has no
      # rows" for it would name the wrong cause.
      #
      # Read off the UNCHECKED rows, not the folded label. `enum` and `const`
      # fold into one "values" line, and they can be unchecked for different
      # reasons -- or one of them checked and the other not. Testing the folded
      # `n_checked` reported "the table has no rows" for a table with rows,
      # whenever one of the two was declared only by an absent column and the
      # other had been evaluated and passed.
      unchecked <- rows[rows$status == "not_checked", , drop = FALSE]
      n_unchecked_declared <- sum(unchecked$columns_declared)
      reason <- if (all(unchecked$columns_checked == 0)) {
        "no column that declares it is present"
      } else {
        unchecked_reason
      }
      cli::cli_alert_warning(
        "{title} check not checked: {reason} ({n_unchecked_declared} declaring column{?s})"
      )
    } else {
      outcome[[i]] <- "passed"
      if (identical(label, "presence")) {
        cli::cli_alert_success("{title} check passed: all {n_declared} declared column{?s} present")
      } else if (n_checked < n_declared) {
        # The skipped columns are absent from the table, which the presence
        # check reports as its own failure. Naming the real denominator here
        # keeps this line from certifying a column nothing examined.
        n_skipped <- n_declared - n_checked
        cli::cli_alert_success(
          "{title} check passed ({n_checked} of {n_declared} columns; {n_skipped} not present)"
        )
      } else {
        cli::cli_alert_success("{title} check passed ({n_declared} column{?s})")
      }
    }
  }

  cli::cli_text()

  n_failed <- sum(outcome == "failed")
  n_unchecked <- sum(outcome == "not_checked")
  n_applicable <- sum(outcome != "not_applicable")

  if (n_failed > 0) {
    cli::cli_alert_danger("{n_failed} of {n_applicable} column spec check{?s} failed")
  } else if (n_unchecked > 0) {
    cli::cli_alert_warning(
      "{n_unchecked} of {n_applicable} column spec check{?s} could not be settled"
    )
  } else if (n_applicable > 0) {
    cli::cli_alert_success("All column spec checks passed")
  } else {
    # Every check inapplicable means the specs constrain nothing. Saying "all
    # passed" here would be the hollow certificate this report exists to stop.
    cli::cli_alert_warning("No column spec check ran: the specs declare no constraints")
  }

  invisible(NULL)
}
