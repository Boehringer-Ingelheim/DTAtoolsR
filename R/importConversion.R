#' @title Maximum Characters Retained From an Offending Value
#' @description
#' An import issue keeps the source text verbatim so the original value is not
#' lost, but a single pathological cell must not be able to grow the validation
#' artifact without bound. The retained text is truncated at this many
#' characters; the error *count* is never truncated.
#' @include DTAColumnSpecStructureSAS-class.R
#' @keywords internal
dta_import_raw_max_chars <- 200L

#' @title Maximum Import Issues Retained Per Column
#' @description
#' A column that is entirely the wrong type produces one issue per row. Only
#' this many rows are retained per column, so a wholly mistyped column costs a
#' bounded amount of memory. The error *count* stays exact, which is what
#' `import_valid` and therefore `ok` are derived from -- truncation can never
#' turn a failing table into a passing one.
#' @keywords internal
dta_import_max_rows_per_column <- 10000L


#' @title Column Spec Structure for One Column
#' @description
#' Looks up the `DTAColumnSpecStructure` a collection declares for a column.
#' The collection is normally named by column id, but a spec built by another
#' route may not be, so the ids themselves are the fallback.
#' @param specs A `DTAColumnSpecCollection`, or `NULL`.
#' @param column Character. Name of the column.
#' @return The structure object, or `NULL` when the collection has no spec for
#'   the column.
#' @keywords internal
dta_spec_column_structure <- function(specs, column) {
  columns <- tryCatch(specs@columns, error = function(e) NULL)

  if (!is.list(columns) || length(columns) == 0) {
    return(NULL)
  }

  index <- match(column, names(columns))

  if (is.na(index)) {
    ids <- vapply(
      columns,
      function(spec) tryCatch(as.character(spec@id)[[1]], error = function(e) NA_character_),
      character(1)
    )
    index <- match(column, ids)
  }

  if (is.na(index)) {
    return(NULL)
  }

  tryCatch(columns[[index]]@structure, error = function(e) NULL)
}


#' @title Target R Type for One Column
#' @description
#' The R type a column should be stored as, according to its spec.
#' @param specs A `DTAColumnSpecCollection`, or `NULL`.
#' @param column Character. Name of the column.
#' @return A length-1 character naming an R type, or `NA_character_` when the
#'   specs say nothing about this column. `NA` means "leave the column exactly
#'   as it was read".
#' @keywords internal
dta_spec_r_type <- function(specs, column) {
  structure <- dta_spec_column_structure(specs, column)

  if (is.null(structure)) {
    return(NA_character_)
  }

  target <- tryCatch(as_r_type(structure), error = function(e) NA_character_)

  if (length(target) != 1 || is.na(target)) {
    return(NA_character_)
  }

  as.character(target)
}


#' @title Reader Column Types Declared by the Specs
#' @description
#' Builds the Arrow schema handed to the CSV/TSV/delimited reader, so the
#' declared type of a column -- not the reader's guess at it -- decides how the
#' bytes in the file are parsed.
#'
#' Arrow infers a column's type from its contents, and that inference runs
#' *before* any code in this package sees the data. A column of quoted subject
#' ids -- `"007"`, `"008"` -- is inferred as `int64` and arrives in R as `7` and
#' `8`. The leading zeros are gone by the time [dta_coerce_table_to_specs()] is
#' reached, so its "never coerce a `Char` column" guard has nothing left to
#' protect: the corruption already happened. The only place to stop it is the
#' read itself.
#'
#' The schema only ever *widens* a column to `utf8`, and only for columns whose
#' declared R type is `"character"`. Narrowing is deliberately left alone.
#' Telling Arrow that a column is `int64` makes it abort the entire read on the
#' first cell it cannot parse (`CSV conversion error to int64: invalid value
#' 'abc'`), which would turn a single reportable bad cell into a transfer that
#' will not load at all. Numeric columns are therefore still read by inference
#' and narrowed by [dta_coerce_table_to_specs()], where an unrepresentable value
#' becomes `NA` and is reported as an import error.
#'
#' Reading a `character` column as text can never fail and never loses
#' information, which is what makes widening the safe half of the operation.
#' @param specs A `DTAColumnSpecCollection`, or `NULL`. `NULL` means "no
#'   declared types are available", and yields `NULL`: the reader then infers
#'   every column exactly as it did before.
#' @param has_header Logical. When the file has no header, Arrow generates
#'   positional names (`f0`, `f1`, ...) that cannot correspond to spec ids, so
#'   no column spec is built.
#' @return An `arrow::schema()` naming the textual columns, or `NULL` when there
#'   is nothing to pin.
#' @keywords internal
dta_reader_col_types <- function(specs, has_header = TRUE) {
  if (is.null(specs) || !isTRUE(has_header)) {
    return(NULL)
  }

  columns <- tryCatch(specs@columns, error = function(e) NULL)

  if (!is.list(columns) || length(columns) == 0) {
    return(NULL)
  }

  ids <- vapply(
    columns,
    function(spec) tryCatch(as.character(spec@id)[[1]], error = function(e) NA_character_),
    character(1),
    USE.NAMES = FALSE
  )

  # A collection is normally named by column id, but one built by another route
  # may not be. `dta_spec_r_type()` resolves either, so both are offered to it:
  # the reader and the coercion choke point then agree on what a column is by
  # construction, rather than by two lookups that could drift apart.
  keys <- unique(c(names(columns), ids))
  keys <- keys[!is.na(keys) & nzchar(keys)]

  textual <- keys[vapply(
    keys,
    function(key) identical(dta_spec_r_type(specs, key), "character"),
    logical(1),
    USE.NAMES = FALSE
  )]

  if (length(textual) == 0) {
    return(NULL)
  }

  # A schema entry for a column the file does not contain is ignored by Arrow,
  # so a spec that declares more columns than the file carries is not an error
  # here. Whether the column is missing is the column spec axis's question.
  types <- rep(list(arrow::utf8()), length(textual))
  names(types) <- textual

  do.call(arrow::schema, types)
}


#' @title Coerce One Column to Its Declared R Type
#' @description
#' Converts a single column to the type its spec declares, reporting the values
#' that could not be represented.
#'
#' Only numeric targets are converted. Everything else -- `"character"` in
#' particular -- is returned untouched, which is what keeps a `SAS Char`
#' `SUBJECT_ID` of `"007"` from ever being round-tripped through a number.
#'
#' A column that is already numeric is left alone: there is nothing to parse, so
#' no value can fail to parse. This is also what stops the import axis from
#' inventing errors on a clean file.
#'
#' An `Int` target is narrowed to R `integer` only when every value is whole.
#' Rounding a fractional value into an integer column would silently discard the
#' fraction *and* hide the `type: integer` column spec error that exists to report
#' it, so a fractional value stays a double and is left to the column spec axis.
#' @param values A column vector taken from the table.
#' @param target Character. The target R type, from [as_r_type()].
#' @return `NULL` when the column is left untouched, otherwise a list with the
#'   converted `values`, the integer indices of the `offending` values, and the
#'   source text `raw`.
#' @keywords internal
dta_coerce_column <- function(values, target) {
  if (!isTRUE(target %in% c("double", "integer"))) {
    return(NULL)
  }

  # Nothing to parse: already a number, or already a temporal value carrying its
  # own numeric representation.
  if (is.numeric(values) || inherits(values, "Date") || inherits(values, "POSIXt")) {
    return(NULL)
  }

  # An all-missing column (Arrow reads an empty column as `null`) carries no
  # value that could have failed to convert.
  if (length(values) == 0 || all(is.na(values))) {
    return(NULL)
  }

  converted <- dta_as_numeric_strict(values)

  # `dta_as_numeric_strict()` already yields NA for the unconvertible values,
  # which is exactly the required semantics: the value becomes NA in the typed
  # column and its raw text is retained in the issue below.
  out <- converted$values

  if (identical(target, "integer")) {
    present <- out[!is.na(out)]
    is_whole <- length(present) == 0 ||
      (all(is.finite(present)) &&
        all(present == trunc(present)) &&
        all(abs(present) <= .Machine$integer.max))
    if (is_whole) {
      out <- as.integer(out)
    }
  }

  list(
    values = out,
    offending = which(converted$unconvertible),
    raw = converted$raw
  )
}


#' @title Type a Table by Its Column Specs at Import Time
#' @description
#' The typed import choke point. The declared type of every column is applied to
#' the data as it is read, instead of leaving the reader to infer a type per
#' column.
#'
#' Arrow infers the type of a CSV/TSV column from its contents, so a single
#' unparseable cell in an otherwise numeric column makes the reader fall back to
#' string for *every* row of that column. Every downstream check then sees a
#' column of text where the specification declared a number. Applying the
#' declared type here means the column is a number, one cell is missing, and
#' that one cell is reported.
#'
#' Semantics:
#' * A value that cannot be represented in the declared type becomes `NA` in the
#'   typed column, and its source text is retained verbatim in an import issue
#'   with `reason = "not_convertible"`.
#' * Only unrecoverable values are issues. `"007"` to `7` and `"1.50"` to `1.5`
#'   are clean conversions and are not reported.
#' * A column whose declared type is `Char` is never coerced.
#' * A column present in the table but absent from the specs is left untouched,
#'   not dropped.
#' * `NA`, `""` and whitespace in the source stay missing and are not issues.
#'
#' The issues are returned *and* attached to the returned table as the
#' `"dta_import_issues"` attribute. The carried copy is deliberate:
#' [check()] skips revalidation when the table hash and the specs hash are both
#' unchanged, and that hash is taken from the table itself. Issues living only
#' in the dataset's `import_issues` property would not be hashed, so a table
#' whose import issues had changed could be skipped while still reporting a
#' stale `ok = TRUE`. Riding on the table, they cannot be separated from the
#' data they describe.
#' @param table An Arrow Table or a data.frame.
#' @param specs A `DTAColumnSpecCollection`, or `NULL`.
#' @return A list with `table` (the typed table, same class as the input, with
#'   the issues attached) and `issues` (a data.frame in the shape of
#'   [dta_empty_import_errors()], carrying the exact error count in its
#'   `"n_import_errors"` attribute).
#' @keywords internal
dta_coerce_table_to_specs <- function(table, specs) {
  was_arrow <- inherits(table, "Table") || inherits(table, "ArrowTabular")
  df <- if (was_arrow) as.data.frame(table) else table

  if (!is.data.frame(df) || ncol(df) == 0 || is.null(names(df))) {
    return(list(table = table, issues = dta_empty_import_errors()))
  }

  parts <- list()
  n_total <- 0L
  changed <- FALSE

  for (column in names(df)) {
    target <- dta_spec_r_type(specs, column)

    # No spec for this column: it is left exactly as read, not dropped.
    if (is.na(target)) {
      next
    }

    coerced <- dta_coerce_column(df[[column]], target)

    if (is.null(coerced)) {
      next
    }

    df[[column]] <- coerced$values
    changed <- TRUE

    n_offending <- length(coerced$offending)

    if (n_offending == 0L) {
      next
    }

    # The count is accumulated before the cap is applied, so `ok` is decided by
    # how many values failed, never by how many were retained.
    n_total <- n_total + n_offending
    kept <- coerced$offending[seq_len(min(n_offending, dta_import_max_rows_per_column))]

    declared <- dta_spec_declared_type(specs, column)
    if (is.na(declared)) {
      declared <- target
    }

    parts[[length(parts) + 1L]] <- data.frame(
      row = as.integer(kept),
      column = column,
      raw = substr(as.character(coerced$raw[kept]), 1L, dta_import_raw_max_chars),
      declared_type = declared,
      reason = "not_convertible",
      stringsAsFactors = FALSE
    )
  }

  issues <- if (length(parts) == 0) {
    dta_empty_import_errors()
  } else {
    out <- do.call(rbind, parts)
    out <- out[order(out$row, out$column), , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  attr(issues, "n_import_errors") <- as.integer(n_total)

  # Nothing was typed and nothing failed: hand back the original object rather
  # than paying for a round trip that cannot have changed anything.
  if (!changed && nrow(issues) == 0) {
    return(list(table = table, issues = issues))
  }

  if (nrow(issues) > 0) {
    attr(df, "dta_import_issues") <- issues
  }

  list(
    table = if (was_arrow) arrow::as_arrow_table(df) else df,
    issues = issues
  )
}


#' @title Import Issues Carried by a Table
#' @description
#' Reads the import issues that [dta_coerce_table_to_specs()] attached to a
#' table. Survives the Arrow round trip, because Arrow stores R attributes in
#' the schema metadata and restores them on `as.data.frame()`.
#' @param table A data.frame (typically the one `check()` materialised from the
#'   Arrow table).
#' @return A data.frame in the shape of [dta_empty_import_errors()], or `NULL`
#'   when the table carries none.
#' @keywords internal
dta_carried_import_issues <- function(table) {
  issues <- attr(table, "dta_import_issues", exact = TRUE)

  if (!is.data.frame(issues) || nrow(issues) == 0) {
    return(NULL)
  }

  issues
}


#' @title Exact Import Error Count of an Issue Frame
#' @description
#' The number of values that failed to convert, which is not `nrow()` when the
#' per-column cap truncated the retained rows.
#' @param issues A data.frame in the shape of [dta_empty_import_errors()], or
#'   `NULL`.
#' @return A length-1 integer.
#' @keywords internal
dta_import_error_count <- function(issues) {
  if (!is.data.frame(issues)) {
    return(0L)
  }

  n <- attr(issues, "n_import_errors", exact = TRUE)

  if (is.null(n) || length(n) != 1 || !is.numeric(n) || is.na(n)) {
    return(as.integer(nrow(issues)))
  }

  # The retained rows are the floor: a frame can never report fewer errors than
  # the rows it actually carries.
  as.integer(max(as.integer(n), nrow(issues)))
}


#' @title Merge the Import-Time and Rule-Time Import Errors
#' @description
#' Combines the errors detected while typing the table with those the rule layer
#' found while reading a column as a number.
#'
#' After the import choke point the two rarely overlap: a value the import layer
#' could not represent is already `NA` by the time a rule sees it, so the rule
#' layer reports it only for columns the import layer does not type (a `Char`
#' column, or one with no spec). Where they do overlap it is one error, not two.
#' @param carried A data.frame of import-time issues, or `NULL`.
#' @param rule_errors A data.frame of rule-time issues, or `NULL`.
#' @return A data.frame in the shape of [dta_empty_import_errors()], carrying
#'   the exact total in its `"n_import_errors"` attribute.
#' @keywords internal
dta_merge_import_errors <- function(carried, rule_errors) {
  carried_total <- dta_import_error_count(carried)

  if (!is.data.frame(rule_errors) || nrow(rule_errors) == 0) {
    rule_errors <- NULL
  }

  if (!is.null(rule_errors) && !is.null(carried)) {
    already <- paste(carried$row, carried$column, sep = "\r")
    keep <- !(paste(rule_errors$row, rule_errors$column, sep = "\r") %in% already)
    rule_errors <- rule_errors[keep, , drop = FALSE]
    if (nrow(rule_errors) == 0) {
      rule_errors <- NULL
    }
  }

  out <- do.call(
    rbind,
    c(list(dta_empty_import_errors()), Filter(Negate(is.null), list(carried, rule_errors)))
  )

  if (nrow(out) > 0) {
    out <- out[order(out$row, out$column), , drop = FALSE]
    rownames(out) <- NULL
  }

  n_rule <- if (is.null(rule_errors)) 0L else as.integer(nrow(rule_errors))
  attr(out, "n_import_errors") <- carried_total + n_rule
  out
}
