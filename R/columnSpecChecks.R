# Column-spec-axis validation without a JSON Schema validator.
#
# This replaces the chunk -> jsonlite::toJSON -> ajv-on-V8 loop that previously
# dominated validation cost (~95% of total runtime, measured; see
# benchmarks/bench_validation.R). The constraint vocabulary the package can
# emit is only eight keywords wide -- type, format, maxLength, enum, const,
# pattern, required and additionalProperties -- so the validator was doing a
# general-purpose job for a closed, small problem.
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
      text_column <<- if (is.character(x)) {
        x
      } else if (is.numeric(x)) {
        dta_numeric_text(x)
      } else {
        as.character(x)
      }
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
        # Whole AND representable: an integer column is stored as int32, so a
        # whole value beyond +-(2^31 - 1) is not an integer of this column.
        is.finite(x) & x == trunc(x) & abs(x) <= .Machine$integer.max
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
  # Applies to strings, and to numbers through their plain decimal text (see
  # dta_numeric_text()); booleans are skipped. The generated schema's patterns
  # were previously evaluated as ECMAScript regex; PCRE is used here, which
  # agrees for the character-class and anchor constructs these specs use.
  pattern <- col_spec$pattern
  if (!is.null(pattern) && !is.na(pattern) &&
    base_type %in% c("string", "integer", "number")) {
    present <- if (has_na) which(!na_mask()) else seq_len(n)
    # Evaluated once per DISTINCT value and mapped back: identifiers and codes
    # repeat down a column, and the regex (and, for numbers, the text
    # conversion) is the dominant cost of this check.
    matched <- if (base_type == "string") {
      dta_map_unique(string_text()[present], function(u) grepl(pattern, u, perl = TRUE))
    } else {
      dta_map_unique(
        x[present],
        function(u) grepl(pattern, dta_numeric_text(u), perl = TRUE)
      )
    }
    bad <- present[!matched]
    parts[[length(parts) + 1]] <- dta_columnspec_error_rows(
      bad, column_name, "pattern",
      paste0("must match pattern \"", pattern, "\""),
      pattern,
      as_text(bad)
    )
  }

  # format --------------------------------------------------------------------
  # A SAS display format: the value must be representable in it. BESTw. never
  # reaches here (as_json_schema() does not emit it), and is skipped regardless.
  fmt <- col_spec$format
  fmt_parts <- if (!is.null(fmt) && length(fmt) == 1 && !is.na(fmt)) {
    dta_sas_format_parts(fmt)
  }
  if (!is.null(fmt_parts) && fmt_parts$family != "best") {
    numeric_column <- base_type %in% c("integer", "number")
    # Date/POSIXt vectors are serialised as strings but carry no display text.
    text_column_ok <- base_type == "string" &&
      !inherits(x, "Date") && !inherits(x, "POSIXt")
    w <- fmt_parts$w

    bad <- if (fmt_parts$family == "char" && text_column_ok) {
      # Bytes first, characters only for what survives: see maxLength above.
      txt <- string_text()
      within_bytes <- nchar(txt, type = "bytes") <= w
      candidates <- if (has_na) {
        which(!within_bytes & !na_mask())
      } else {
        which(!within_bytes)
      }
      if (length(candidates) == 0) {
        integer(0)
      } else {
        candidates[nchar(txt[candidates], type = "chars") > w]
      }
    } else if (fmt_parts$family == "num" && numeric_column) {
      # A column that takes integers only reports a fraction under `type`
      # already; repeating it as a format violation would be one fault twice.
      integer_only <- !is.null(allowed) && !("number" %in% allowed)
      dta_sas_numeric_format_bad(x, w, fmt_parts$d, decimals = !integer_only)
    } else if (fmt_parts$family %in% c("date", "time", "datetime") && text_column_ok) {
      present <- if (has_na) which(!na_mask()) else seq_len(n)
      re <- dta_sas_temporal_regex(fmt_parts$family, fmt_parts$d)
      # SAS right-aligns display text, so surrounding spaces are not part of it;
      # the width is enforced on what is left.
      fits <- dta_map_unique(string_text()[present], function(u) {
        u <- gsub("^ +| +$", "", u)
        grepl(re, u, perl = TRUE, ignore.case = TRUE) & nchar(u, type = "chars") <= w
      })
      present[!fits]
    } else {
      integer(0)
    }

    parts[[length(parts) + 1]] <- dta_columnspec_error_rows(
      bad, column_name, "format",
      paste0("must fit format \"", fmt, "\""),
      as.character(fmt),
      as_text(bad)
    )
  }

  # finite / minimum / maximum --------------------------------------------------
  # Cell specs of a matrix dataset only (DTAColumnSpecCell). NA and NaN are never
  # range findings: missing is the type/nullable check's business. An infinite
  # value under `finite` yields the `finite` finding alone, so the bounds skip it.
  if (base_type %in% c("integer", "number") &&
    (isTRUE(col_spec$finite) || !is.null(col_spec$minimum) ||
      !is.null(col_spec$maximum))) {
    inf <- if (base_type == "number") is.infinite(x) else rep(FALSE, n)
    if (isTRUE(col_spec$finite)) {
      bad <- which(inf)
      parts[[length(parts) + 1]] <- dta_columnspec_error_rows(
        bad, column_name, "finite", "must be finite", "finite", as_text(bad)
      )
    }
    for (kw in c("minimum", "maximum")) {
      bound <- col_spec[[kw]]
      if (is.null(bound) || length(bound) != 1L || is.na(bound)) {
        next
      }
      outside <- if (kw == "minimum") x < bound else x > bound
      outside <- !is.na(outside) & outside
      if (isTRUE(col_spec$finite)) {
        outside <- outside & !inf
      }
      bad <- which(outside)
      bound_text <- dta_numeric_text(as.numeric(bound))
      parts[[length(parts) + 1]] <- dta_columnspec_error_rows(
        bad, column_name, kw,
        paste0(if (kw == "minimum") "must be >= " else "must be <= ", bound_text),
        bound_text,
        as_text(bad)
      )
    }
  }

  parts <- Filter(Negate(is.null), parts)
  if (length(parts) == 0) {
    return(NULL)
  }
  do.call(rbind, parts)
}

# ---- numeric text, SAS format helpers ----------------------------------------

#' @title Evaluate a Function Once per Distinct Value
#' @description
#' `fun(unique(v))` mapped back onto every element of `v`. For the per-value
#' tests of a column (a regex, a text conversion) whose values repeat, this turns
#' a cost proportional to the rows into one proportional to the distinct values.
#' It carries no state across calls, so a streaming batch stays self-contained.
#' @param v A vector without missing values.
#' @param fun A function of a vector returning one element per element.
#' @return `fun(v)`, computed from the distinct values.
#' @keywords internal
dta_map_unique <- function(v, fun) {
  u <- unique(v)
  fun(u)[match(v, u)]
}

#' @title Plain Decimal Text of a Numeric Vector
#' @description
#' The text a numeric value is matched against a `pattern` as, never in
#' scientific notation -- `as.character(1e9)` is `"1e+09"`, which would fail a
#' ten-digit pattern the value satisfies. Integers are written as they are; a
#' whole double of magnitude below 1e15 is written in full; any other double is
#' `as.character()`'s rendering (15 significant digits), re-expanded without an
#' exponent when that rendering has one. A whole value of 1e15 or more is thus
#' written out in full (`1e16` is `"10000000000000000"`), exactly as the double
#' holds it rather than rounded to 15 digits.
#' @param x A numeric vector.
#' @return A character vector, `NA` where `x` is `NA`.
#' @keywords internal
dta_numeric_text <- function(x) {
  if (is.integer(x)) {
    return(as.character(x))
  }
  txt <- rep(NA_character_, length(x))
  whole <- is.finite(x) & abs(x) < 1e15 & x == trunc(x)
  # `+ 0` turns a negative zero into zero, which "%.0f" would print as "-0".
  txt[whole] <- sprintf("%.0f", x[whole] + 0)
  rest <- which(!whole)
  if (length(rest) > 0) {
    r <- as.character(x[rest])
    sci <- which(grepl("e", r, fixed = TRUE))
    if (length(sci) > 0) {
      # "fg" pads to the digit count, hence the trim.
      r[sci] <- trimws(formatC(x[rest][sci], digits = 15, format = "fg", decimal.mark = "."))
    }
    txt[rest] <- r
  }
  txt
}

#' @title Width and Decimals of a SAS Format
#' @param format A SAS format string such as `"$12."`, `"8.2"` or `"DATE9."`.
#' @return `NULL` for an unrecognised format, otherwise a list of `family`
#'   (`"char"`, `"num"`, `"date"`, `"time"`, `"datetime"` or `"best"`), `w` (the
#'   width) and `d` (decimals, `NA` when the format declares none).
#' @keywords internal
dta_sas_format_parts <- function(format) {
  f <- toupper(trimws(as.character(format)))
  num <- function(s) if (nzchar(s)) as.integer(s) else NA_integer_
  hit <- function(re) {
    m <- regmatches(f, regexec(re, f))[[1]]
    if (length(m) == 0) NULL else m
  }

  if (!is.null(m <- hit("^\\$([0-9]+)\\.$"))) {
    return(list(family = "char", w = num(m[[2]]), d = NA_integer_))
  }
  if (!is.null(m <- hit("^BEST([0-9]+)\\.$"))) {
    return(list(family = "best", w = num(m[[2]]), d = NA_integer_))
  }
  if (!is.null(m <- hit("^DATETIME([0-9]+)\\.$"))) {
    return(list(family = "datetime", w = num(m[[2]]), d = NA_integer_))
  }
  if (!is.null(m <- hit("^DATE([0-9]+)\\.$"))) {
    return(list(family = "date", w = num(m[[2]]), d = NA_integer_))
  }
  if (!is.null(m <- hit("^TIME([0-9]+)\\.([0-9]*)$"))) {
    return(list(family = "time", w = num(m[[2]]), d = num(m[[3]])))
  }
  if (!is.null(m <- hit("^([0-9]+)\\.([0-9]*)$"))) {
    d <- num(m[[3]])
    return(list(family = "num", w = num(m[[2]]), d = if (is.na(d)) 0L else d))
  }
  NULL
}

# Hard-coded English month abbreviations: strptime()/%b follow the system
# locale, which would make the same data pass on one machine and fail on another.
`__DTAtools_sas_month_re__` <- "JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC"

dta_sas_temporal_regex <- function(family, d) {
  # A day is 01-31; SAS writes DATE11. with hyphens (15-AUG-2025), so a hyphen
  # may stand between day, month and year.
  date_core <- paste0(
    "(?:0[1-9]|[12][0-9]|3[01])-?(?:", `__DTAtools_sas_month_re__`,
    ")(?:-?(?:[0-9]{2}|[0-9]{4}))?"
  )
  fraction <- if (!is.na(d) && d > 0) paste0("(?:\\.[0-9]{1,", d, "})?") else ""
  # Hours are not capped at two digits: a TIME value is a duration (100:00:00).
  time_core <- paste0("[0-9]+:[0-9]{2}(?::[0-9]{2}", fraction, ")?")
  paste0(
    "^",
    switch(family,
      date = date_core,
      time = time_core,
      datetime = paste0(date_core, ":", time_core)
    ),
    "$"
  )
}

#' @title The Format a Column Schema Carries
#' @description
#' The `format` keyword `as_json_schema()` emits for a column, or `NULL` when it
#' emits none. A format is emitted only for a SAS structure, never for `BESTw.`
#' (adaptive width, so it constrains nothing), and for `$w.` only when the
#' declared `length` is absent or larger than `w` -- otherwise `maxLength`
#' already rejects everything the format would, and the streaming engine need
#' not pay for a second length pass.
#' @param x A `DTAColumnSpec`.
#' @return A length-1 character, or `NULL`.
#' @keywords internal
dta_columnspec_emitted_format <- function(x) {
  s <- x@structure
  if (is.null(s) || !identical(s@backend, "SAS") || !.structure_value_is_set(s@format)) {
    return(NULL)
  }
  parts <- dta_sas_format_parts(s@format)
  if (is.null(parts) || parts$family == "best") {
    return(NULL)
  }
  if (parts$family == "char" && !is.null(s@length) && !is.na(s@length) && s@length <= parts$w) {
    return(NULL)
  }
  as.character(s@format)
}

#' @title Values a Numeric SAS Format Cannot Display
#' @description
#' Indices of the finite values of `x` that do not fit the format `w.d`: more
#' decimals than `d`, or a rendered width -- integer digits, sign, and the
#' decimal point with its `d` digits -- beyond `w`.
#'
#' SAS drops the leading zero of a value below one in magnitude when the width
#' is tight (`0.5` under `3.2` shows `.50`), so a width exceeding `w` by exactly
#' one is accepted for such a value when `d` is above zero.
#' @param x A numeric vector.
#' @param w,d Width and decimals of the format.
#' @param decimals Logical. Whether to test the number of decimals. `FALSE` for a
#'   column that takes integers only, where a fraction is the `type` check's
#'   finding.
#' @return Integer indices into `x`.
#' @keywords internal
dta_sas_numeric_format_bad <- function(x, w, d, decimals = TRUE) {
  v <- as.numeric(x)
  idx <- which(is.finite(v))
  y <- v[idx]
  scaled <- y * 10^d
  too_precise <- decimals & is.finite(scaled) &
    abs(scaled - round(scaled)) > 1e-9 * pmax(1, abs(scaled))

  r <- abs(round(y, d))
  a <- pmax(r, 1)
  digits <- floor(log10(a)) + 1
  # log10() can land a hair either side of an exact power of ten.
  digits <- digits + (a >= 10^digits) - (digits > 1 & a < 10^(digits - 1))
  width <- digits + (y < 0) + (d > 0) * (1 + d)
  # The dropped leading zero: ".50" for 0.5, "-.50" for -0.5.
  tight <- d > 0 & r < 1 & width == w + 1

  idx[too_precise | (width > w & !tight)]
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
#'
#' A column whose schema cannot be derived (`as_json_schema()` errors) gets a
#' `NULL` schema and is warned about here, once for the whole scan rather than
#' once per batch -- `dta_columnspec_errors()` then skips its value checks
#' entirely, exactly as it does for a column absent from `schemas`.
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
    column_name <- spec_names[[i]]
    schema <- tryCatch(
      as_json_schema(columns[[i]]),
      error = function(e) {
        # Not silent: a column whose schema fails to compile loses ALL of its
        # value checks (type, length, pattern, enum) in both the materialising
        # and streaming engines, which is worth a warning rather than a table
        # that quietly reports "ok" on data it never actually examined. `NULL`
        # is still returned so the scan itself is not interrupted -- the same
        # degraded-but-running behaviour as before, just no longer silent.
        cli::cli_warn(
          "Column {.val {column_name}} could not be compiled into a schema; its value checks are skipped.",
          class = "dta_columnspec_schema_failed"
        )
        NULL
      }
    )
    list(name = column_name, schema = schema)
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

  # A table with no rows is NOT skipped. Its structure is still decidable, and
  # still required: an empty table is a legitimate result -- an analysis that
  # yielded nothing -- but it has to carry the columns its specs declare, so
  # that what it expresses is "the process ran and found no results" rather
  # than "something went wrong upstream". Returning early here reported a table
  # that was both empty and missing half its declared columns as clean.
  #
  # Only the value checks are skipped for such a table: type, length, pattern
  # and permitted values are properties of values, and there are none.
  empty_table <- n_rows == 0

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
      # An empty table has no row to attach it to, so it is reported once with
      # `row = NA` -- the same shape `dta_structure_errors()` uses for a finding
      # about the table rather than about a row in it.
      parts[[length(parts) + 1]] <- data.frame(
        row = if (empty_table) NA_integer_ else seq_len(n_rows),
        column = NA_character_,
        keyword = "required",
        message = paste0("must have required property '", column_name, "'"),
        columnspec = column_name,
        data = NA_character_,
        stringsAsFactors = FALSE
      )
      next
    }

    # Present, and there is nothing in it to check. The value constraints below
    # would all evaluate over a zero-length vector and find nothing; skipping
    # them says so without spending the dispatch.
    if (empty_table) {
      next
    }

    # A NULL schema means as_json_schema() could not be derived for this
    # column -- dta_compile_columnspec_schemas() already warned about it once
    # for the whole scan, so this is a silent skip of the value checks here.
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
#' the affected rows. An object-level finding -- a missing required column, or a
#' column no spec describes -- is summarised by constraint alone, because
#' reporting a row range for a fact about the table's shape adds nothing.
#' @param full_error A data frame of violations.
#' @return A summarised data frame.
#' @importFrom rlang .data
#' @keywords internal
dta_summarise_columnspec_errors <- function(full_error) {
  if (is.null(full_error) || nrow(full_error) == 0) {
    return(NULL)
  }

  # `required` alone triggers the object-level summary, exactly as it always
  # has: a declared column that is absent makes a row range for the rest of the
  # table beside the point.
  #
  # An UNDECLARED column must NOT trigger it. It says nothing about the values
  # in the declared columns, so suppressing their summary on account of it would
  # hide real per-value findings behind a stray column -- and a stray column is
  # far commoner in practice than a missing one. It flows through the grouped
  # aggregation below instead, where its `NA` row yields an `NA` row range: the
  # honest answer for a finding about the header rather than about a row.
  #
  # It is still listed by name when a required column is also missing, because
  # that branch reports every object-level finding it has.
  if (any(full_error$keyword == "required")) {
    object_level <- full_error$keyword %in% c("required", "additionalProperties")
    out <- unique(full_error[object_level, c("keyword", "message"), drop = FALSE])
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
# The constraint vocabulary is closed and seven keywords wide, so "did the
# table pass the column specs" can be answered per check kind rather than as one
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
#' @return A data frame with columns `check`, `keyword` and `matrix_only`. The
#'   matrix-only keywords (header, dimension, range and finiteness findings of a
#'   matrix dataset) are counted in every tally, but only reported in a summary
#'   built with `matrix = TRUE`.
#' @keywords internal
dta_columnspec_check_kinds <- function() {
  legacy <- data.frame(
    check = c(
      "presence", "extra", "format", "format", "length", "values", "values", "pattern"
    ),
    keyword = c(
      "required", "additionalProperties",
      "type", "format", "maxLength", "enum", "const", "pattern"
    ),
    matrix_only = FALSE,
    stringsAsFactors = FALSE
  )
  matrix_kinds <- data.frame(
    check = c(
      rep("header", 3), rep("dimensions", 4), rep("range", 2), "finite"
    ),
    keyword = c(
      "corner", "propertyNames", "uniqueNames",
      "minProperties", "maxProperties", "minItems", "maxItems",
      "minimum", "maximum", "finite"
    ),
    matrix_only = TRUE,
    stringsAsFactors = FALSE
  )
  rbind(legacy, matrix_kinds)
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
  # Not a per-column declaration: no column spec says "there shall be no other
  # columns". Closedness is a property of the COLLECTION, in force whenever it
  # declares any column at all -- so its denominator is every declared column.
  # That is also what makes a collection declaring nothing report
  # `not_applicable` here rather than certifying a check that could never have
  # found anything: a spec naming no column cannot call any column unexpected.
  out$additionalProperties <- unique(column_names[named])
  out$type <- declares(function(s) !is.null(s$type))
  out$format <- declares(function(s) !is.null(s$format))
  out$maxLength <- declares(function(s) !is.null(s$maxLength) && !anyNA(s$maxLength))
  out$enum <- declares(function(s) !is.null(s$enum))
  # The `else if` in `dta_check_column_spec()`: `enum` wins when a schema
  # somehow carries both, so `const` is only in force where `enum` is absent.
  out$const <- declares(function(s) is.null(s$enum) && !is.null(s$const))
  out$pattern <- declares(function(s) !is.null(s$pattern) && !anyNA(s$pattern))
  out$minimum <- declares(function(s) !is.null(s$minimum))
  out$maximum <- declares(function(s) !is.null(s$maximum))
  out$finite <- declares(function(s) isTRUE(s$finite))
  # The header and dimension keywords stay character(0): a matrix has no
  # per-column declaration for them, the caller supplies their denominators.
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
  # Only for `required`: a dimension finding (`minItems` ...) carries its bound
  # in `columnspec` and has no column, and would otherwise tally the bound as a
  # failed column.
  fallback <- is.na(column) & keyword == "required"
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
                                         settled = NULL,
                                         matrix = FALSE,
                                         structure_declared = NULL) {
  kinds <- dta_columnspec_check_kinds()
  if (is.null(tally)) {
    tally <- dta_empty_columnspec_tally()
  }
  # A tabular summary is byte-identical to what it was before the matrix
  # vocabulary existed: the matrix-only rows are dropped, not reported as
  # not applicable -- unless they hold violations. A DTAColumnSpecCell can sit
  # in a tabular dataset, and its range/finite findings must not vanish into
  # "all checks passed".
  if (!isTRUE(matrix)) {
    kinds <- kinds[
      !kinds$matrix_only | tally$n_errors[kinds$keyword] > 0, ,
      drop = FALSE
    ]
  }
  keywords <- kinds$keyword
  structure_keywords <- kinds$keyword[kinds$check %in% c("header", "dimensions")]
  if (is.null(settled)) {
    settled <- keywords
  }

  declared <- dta_columnspec_declared_columns(schemas)

  # The columns the table does not have, taken from the presence check's own
  # failures rather than re-derived: whichever path produced the tally, its
  # `required` violations name exactly the columns nothing else could examine.
  absent <- tally$columns[["required"]]

  # Header and dimension findings have no per-column declaration: the caller
  # counts them (columns of the head, bounds of the structure).
  structure_n <- function(kw) {
    # `[[` on a named atomic vector throws for an absent name.
    if (kw %in% names(structure_declared)) as.integer(structure_declared[[kw]]) else 0L
  }
  columns_declared <- vapply(
    keywords,
    function(kw) if (kw %in% structure_keywords) structure_n(kw) else length(declared[[kw]]),
    integer(1),
    USE.NAMES = FALSE
  )
  columns_checked <- vapply(
    keywords,
    function(kw) {
      # Presence is decidable for every declared column; an absent one is this
      # check's finding, not its blind spot. Closedness is decidable on the same
      # terms -- whether the table carries a column no spec describes does not
      # depend on which declared columns turned up -- so it is not narrowed by
      # `absent` either.
      if (kw %in% structure_keywords) {
        structure_n(kw)
      } else if (kw %in% c("required", "additionalProperties")) {
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
    # A matrix takes its columns from the header by position: nothing can be
    # missing or extra, whatever the specs declare.
    if (isTRUE(matrix) && keywords[[i]] %in% c("required", "additionalProperties")) {
      return("not_applicable")
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
  if (isTRUE(matrix)) {
    attr(out, "matrix") <- TRUE
  }
  out
}

# Display labels and the reason a whole label is inapplicable. Keyed by the
# `check` column of `dta_columnspec_check_kinds()`, so the two cannot drift.
`__DTAtools_columnspec_check_labels__` <- c(
  presence = "Presence",
  extra = "Extra columns",
  format = "Format",
  length = "Length",
  values = "Values",
  pattern = "Pattern",
  header = "Header",
  dimensions = "Dimensions",
  range = "Range",
  finite = "Finite values"
)

`__DTAtools_columnspec_check_absent__` <- c(
  presence = "the specs declare no columns",
  # Same reason, different consequence: a collection declaring no column cannot
  # name one missing, and equally cannot call one unexpected.
  extra = "the specs declare no columns",
  format = "no column spec declares a type or format",
  length = "no column spec declares a length",
  values = "no column spec declares a value list",
  pattern = "no column spec declares a pattern",
  header = "the matrix declares no header constraint",
  dimensions = "no dimension bound applies",
  range = "the cell spec declares no range",
  finite = "infinite values are allowed"
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

  is_matrix <- isTRUE(attr(checks, "matrix"))
  labels <- unique(checks$check)
  outcome <- character(length(labels))

  for (i in seq_along(labels)) {
    label <- labels[[i]]
    rows <- checks[checks$check == label, , drop = FALSE]
    title <- `__DTAtools_columnspec_check_labels__`[[label]]

    # `type` and `format` share the "format" label but, unlike enum/const, one
    # column declares both (a SAS format implies its type), so summing would
    # count that column twice. The format declarers are a subset of the type
    # declarers, which makes the larger of the two the exact column count.
    pool <- if (identical(label, "format")) max else sum
    n_declared <- pool(rows$columns_declared)
    n_checked <- pool(rows$columns_checked)
    n_errors <- sum(rows$n_errors)
    # `as.character()` rather than the bare `unlist()`: unlisting an empty list
    # yields NULL, and `cli::cli_vec(NULL)` fails trying to set an attribute on
    # it -- on the passing checks, which is every check of a clean table.
    failed_columns <- unique(as.character(unlist(
      strsplit(as.character(stats::na.omit(rows$failed_columns)), ", ", fixed = TRUE),
      use.names = FALSE
    )))
    n_columns <- length(failed_columns)

    if (any(rows$status == "failed")) {
      outcome[[i]] <- "failed"
      # Long enough to name the offenders, short enough that a 400-column table
      # does not print a paragraph per check.
      columns <- cli::cli_vec(failed_columns, list("vec-trunc" = 5))
      if (is_matrix && label %in% c("header", "dimensions")) {
        # Findings on the file's structure, not values in columns.
        n_findings <- dta_format_count(n_errors)
        n_findings_qty <- if (n_errors == 1) 1L else 2L
        cli::cli_alert_danger(
          "{title} check failed: {n_findings}{cli::qty(n_findings_qty)} finding{?s}"
        )
      } else if (identical(label, "presence")) {
        cli::cli_alert_danger(
          "{title} check failed: {n_columns} of {n_declared} declared column{?s} missing: {.field {columns}}"
        )
      } else if (identical(label, "extra")) {
        # Not phrased against `n_declared`: an undeclared column is not one of
        # the declared ones that went wrong, so "2 of 14" would name the wrong
        # denominator. The count that means something here is how many columns
        # the table carries that nothing describes.
        cli::cli_alert_danger(
          "{title} check failed: {n_columns} column{?s} in the table {?is/are} not described by the specs: {.field {columns}}"
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
      if (is_matrix && label %in% c("presence", "extra")) {
        reason <- "a matrix takes its columns from the header by position"
      }
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
      if (is_matrix && label %in% c("header", "dimensions")) {
        # No columns are declared here: the structure is what is unsettled.
        cli::cli_alert_warning("{title} check not checked: {reason}")
      } else {
        cli::cli_alert_warning(
          "{title} check not checked: {reason} ({n_unchecked_declared} declaring column{?s})"
        )
      }
    } else {
      outcome[[i]] <- "passed"
      if (is_matrix && label %in% c("header", "dimensions")) {
        cli::cli_alert_success("{title} check passed")
      } else if (identical(label, "presence")) {
        cli::cli_alert_success("{title} check passed: all {n_declared} declared column{?s} present")
      } else if (identical(label, "extra")) {
        # Not phrased as a count against `n_declared`: a column a RULE names is
        # described by the specification too, so it is not undeclared -- and
        # "no column beyond the 3 declared" would misdescribe a sound table
        # that legitimately carries a fourth.
        cli::cli_alert_success(
          "{title} check passed: every column is described by the specs"
        )
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

# ---- a spec checked against itself --------------------------------------------

#' @title Declared Values and Examples That Do Not Fit Their Own Column
#' @description
#' Runs a column spec's `values` and `examples` through the same evaluator the
#' data goes through, so a spec cannot enumerate a value its own type, length or
#' format would reject. `pattern` and `enum`/`const` are left out: a pattern is
#' checked against the examples by the `DTAColumnSpec` validator itself, and
#' testing an enumeration against itself proves nothing.
#'
#' Values are brought to the column's stored type first, exactly as an import
#' would, so `"abc"` under an `Int` column is reported as not being an integer
#' rather than silently becoming `NA`.
#' @param spec A `DTAColumnSpec`.
#' @return A character vector of problems, empty when the spec is clean.
#' @keywords internal
dta_spec_value_problems <- function(spec) {
  structure <- spec@structure
  target <- if (is.null(structure)) {
    "character"
  } else {
    tryCatch(as_r_type(structure), error = function(e) "character")
  }
  declared <- if (!is.null(structure) && .structure_value_is_set(structure@type)) {
    structure@type
  } else {
    target
  }

  # A structure that cannot say how it validates (as_json_schema_type() aborts
  # for a bare backend) has no constraints to test against; the column is then
  # reported where it always was, by dta_compile_columnspec_schemas().
  schema <- tryCatch(
    Filter(Negate(is.null), list(
      type = as_json_schema_type(spec),
      maxLength = as_json_schema_length(spec),
      format = dta_columnspec_emitted_format(spec)
    )),
    error = function(e) NULL
  )
  if (is.null(schema)) {
    return(character(0))
  }

  problems <- character(0)
  for (field in c("values", "examples")) {
    raw <- if (field == "values") spec@values else spec@examples
    if (is.null(raw)) {
      next
    }
    raw <- raw[!is.na(raw)]
    if (length(raw) == 0) {
      next
    }

    x <- raw
    if (target %in% c("double", "integer")) {
      coerced <- dta_coerce_column(as.character(raw), target)
      bad <- if (is.null(coerced)) integer(0) else coerced$offending
      if (length(bad) > 0) {
        problems <- c(problems, sprintf(
          "%s '%s' is not a valid %s", field, as.character(raw[bad]), declared
        ))
      }
      x <- if (is.null(coerced)) raw else coerced$values[setdiff(seq_along(raw), bad)]
    } else if (!inherits(raw, c("Date", "POSIXt"))) {
      # Date/POSIXt values carry no display text, exactly like a temporal data
      # column: the evaluator skips their format and so do we.
      x <- as.character(raw)
    }
    if (length(x) == 0) {
      next
    }

    errs <- dta_check_column_spec(spec@id, schema, x)
    if (!is.null(errs)) {
      problems <- c(problems, sprintf("%s '%s' %s", field, errs$data, errs$message))
    }
  }

  unique(problems)
}
