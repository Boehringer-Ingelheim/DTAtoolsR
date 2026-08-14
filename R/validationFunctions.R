#' @title Validate Table Against Its Column Specs
#' @description Validates a data.frame against the constraints declared by its
#'   column specs, evaluating each column's values directly rather than
#'   serialising the table and running a JSON Schema validator over it.
#' @importFrom jsonlite toJSON
#' @importFrom dplyr select mutate group_by summarise across n distinct filter any_of everything where
#' @importFrom cli cli_alert_danger cli_alert_success cli_h3 cli_abort cli_alert_info
#' @param table A data.frame to validate.
#' @param specs A specs object.
#' @param verbose Logical. If TRUE (default), prints validation progress.
#' @details
#' Both axes -- the column specs and the rules -- are always evaluated,
#' and both are reported in a single pass. When the table has column spec errors, the
#' returned error list additionally carries `rules_valid` and `rule_errors`, and
#' any rule violations are raised as a warning so they cannot go unnoticed while
#' the column spec errors are being fixed.
#' @return Transformed and checked table (a data.frame) if valid. If the table
#'   has column spec errors, returns a list with `summarised_error`, `full_error`,
#'   `rules_valid` and `rule_errors`. If the schema is valid but rules are
#'   violated, aborts.
#' @export
# TODO: consider moving `validate_table()` into DTADataSet-class.R.
validate_table <- function(specs, table, verbose = TRUE) {
  details <- validate_table_detailed(specs = specs, table = table, verbose = verbose)

  # Always evaluate both axes: a column spec error must never hide a rule violation.
  rule_messages <- if (isTRUE(details$rules_valid)) {
    character(0)
  } else {
    vapply(details$rule_errors, function(x) x$message, character(1))
  }

  if (!isTRUE(details$columnspec_valid)) {
    columnspec_errors <- details$columnspec_errors
    columnspec_errors$rules_valid <- isTRUE(details$rules_valid)
    columnspec_errors$rule_errors <- details$rule_errors
    columnspec_errors$import_valid <- details$import_valid
    columnspec_errors$n_import_errors <- details$n_import_errors
    columnspec_errors["import_errors"] <- list(details$import_errors)

    if (length(rule_messages) > 0) {
      bullets <- c("Rule violations were also found:", rule_messages)
      names(bullets) <- c("", rep("x", length(rule_messages)))
      cli::cli_warn(bullets)
    }

    return(columnspec_errors)
  }

  if (length(rule_messages) > 0) {
    cli::cli_abort(c("Rule violations:", rule_messages))
  }

  # The import axis fails validation independently of column spec and rules: a value
  # that is present but not representable in its declared type fails the run on
  # its own, even when every rule that read it also reported it.
  if (!isTRUE(details$import_valid)) {
    cli::cli_abort(c(
      "Import errors:",
      x = "{details$n_import_errors} value{?s} could not be represented in the declared type."
    ))
  }

  if (isTRUE(verbose)) {
    cli::cli_alert_success("Table validation: all checks passed.")
  }

  table
}


#' @keywords internal
validate_table_detailed <- function(specs, table, verbose = TRUE) {
  # Read before the table is touched: these were recorded when the table was
  # typed at import, and they ride on the table so they cannot be separated
  # from the data they describe.
  carried_import_issues <- dta_carried_import_issues(table)

  # Arrow reads all-empty columns as its `null` type, which converts to a
  # `vctrs_unspecified` vector in R. jsonlite::toJSON() has no asJSON method for
  # that class, so serialising such a column would abort validation with
  # "No method asJSON S3 class: vctrs_unspecified". These columns hold only
  # missing values, so coerce them to an all-NA character vector (emitted as
  # JSON `null` via `na = "null"`). The schema still validates them correctly:
  # nullable columns pass, non-nullable columns are flagged as missing.
  unspecified_cols <- vapply(
    table,
    function(col) inherits(col, "vctrs_unspecified"),
    logical(1)
  )
  if (any(unspecified_cols)) {
    table[unspecified_cols] <- lapply(
      table[unspecified_cols],
      function(col) rep(NA_character_, length(col))
    )
  }

  if (isTRUE(verbose)) {
    cli::cli_h3("validating with column specs")
  }

  # Evaluated column-wise against the specs rather than row-wise against a JSON
  # Schema validator. The constraints are still derived by `as_json_schema()`;
  # only their evaluation changed. See R/columnSpecChecks.R.
  #
  # The chunking and progress bar this replaces existed to keep the JSON
  # payload handed to the validator bounded. Nothing is serialised now, so
  # neither is needed, and the whole-column form is what a streaming
  # implementation can later push into a scan.
  schema_result <- dta_columnspec_errors(specs, table)
  summarised_error <- schema_result$summarised_error
  full_error <- schema_result$full_error
  has_columnspec_errors <- !is.null(full_error) && nrow(full_error) > 0

  if (!has_columnspec_errors && isTRUE(verbose)) {
    cli::cli_alert_success(
      "Table format, length, pattern, and values are valid."
    )
  }

  rule_results <- list()
  rule_errors <- list()
  rules_valid <- TRUE
  rules_list <- tryCatch(specs@rules, error = function(e) NULL)
  if (is.null(rules_list)) {
    rules_list <- list()
  }

  if (length(rules_list) > 0) {
    if (isTRUE(verbose)) {
      cli::cli_h3("validating with rules")
    }

    rule_results <- apply_rules(rules_list, table, verbose = verbose)
    rule_errors <- Filter(function(x) !isTRUE(x$valid), rule_results)
    rules_valid <- length(rule_errors) == 0
  }

  # Import axis, from two sources.
  #
  # Import time: the table was typed against its column specs when it was read,
  # and every value that could not be represented in its declared type was made
  # NA and recorded. This is the primary source -- it covers every specified
  # column, whether or not a rule happens to read it.
  #
  # Rule time: every column a rule reads as a number is scanned for values that
  # are present in the source but not representable as a number. This still
  # catches the columns the import layer does not type (no spec, or a Char
  # column a rule nevertheless compares numerically), and those rows are *also*
  # counted as rule violations, so no error moves from one axis to the other.
  import_errors <- dta_merge_import_errors(
    carried_import_issues,
    dta_collect_import_errors(rule_results, specs)
  )
  # Exact, even when the per-column cap truncated the retained rows, so `ok` is
  # never affected by truncation.
  n_import_errors <- dta_import_error_count(import_errors)
  import_valid <- n_import_errors == 0L
  if (n_import_errors == 0L) {
    import_errors <- NULL
  }

  # Report the import axis with the same visibility as the column spec and rule
  # axes above. Without this, a table whose only defect is an unconvertible
  # value prints the schema success line and the rules success line, then
  # fails downstream with no stated cause -- actively misleading for a
  # clinical data package.
  if (isTRUE(verbose)) {
    cli::cli_h3("validating imports")

    if (import_valid) {
      cli::cli_alert_success("All values were imported cleanly into their declared types.")
    } else {
      preview <- utils::head(import_errors, 5)
      for (i in seq_len(nrow(preview))) {
        raw_display <- substr(as.character(preview$raw[i]), 1L, 80L)
        cli::cli_alert_danger(
          "Row {preview$row[i]}, column '{preview$column[i]}': value \"{raw_display}\" could not be represented as {preview$declared_type[i]}."
        )
      }
      n_more <- nrow(import_errors) - nrow(preview)
      if (n_more > 0) {
        cli::cli_alert_danger("... and {n_more} more import error{?s}.")
      }

      affected_columns <- unique(import_errors$column)
      cli::cli_alert_danger(
        "{n_import_errors} value{?s} could not be represented in the declared type ({length(affected_columns)} column{?s}: {affected_columns})."
      )
    }
  }

  details <- list(
    ok = NA,
    columnspec_valid = !has_columnspec_errors,
    rules_valid = isTRUE(rules_valid),
    import_valid = isTRUE(import_valid),
    # 0L, not 0: `nrow()` on the other branch is an integer, so a bare `0` made
    # this field a double exactly when the count was zero and an integer
    # otherwise. Every other count in the package is an integer, and the
    # streaming path reports one here too, so the type must not depend on
    # whether anything failed.
    n_columnspec_errors = if (is.null(full_error)) 0L else nrow(full_error),
    n_rule_errors = length(rule_errors),
    n_import_errors = n_import_errors,
    columnspec_errors = list(
      summarised_error = summarised_error,
      full_error = full_error
    ),
    rule_results = rule_results,
    rule_errors = rule_errors,
    import_errors = import_errors,
    result_version = 2L
  )

  details$ok <- dta_details_ok(details)
  details
}


#' @title Overall Validity from the Three Validation Axes
#' @description
#' A table is valid only when all three independent axes pass: the column specs
#' (schema), the rules, and the import axis. A value that could not be
#' represented in its declared type fails the run on its own, regardless of what
#' column spec and rules report about the coerced column.
#'
#' `NA` on any axis ("unknown", e.g. an artifact written before the import axis
#' existed) is not a pass.
#' @param details A list carrying `columnspec_valid`, `rules_valid` and
#'   `import_valid`.
#' @return `TRUE` only when all three axes are `TRUE`.
#' @keywords internal
dta_details_ok <- function(details) {
  isTRUE(details$columnspec_valid) &&
    isTRUE(details$rules_valid) &&
    isTRUE(details$import_valid)
}


#' @title Empty Import Error Table
#' @description
#' The canonical zero-row shape of `details$import_errors`. One row per value
#' that could not be represented in its declared type.
#' @return A zero-row data.frame with columns `row`, `column`, `raw`,
#'   `declared_type` and `reason`.
#' @keywords internal
dta_empty_import_errors <- function() {
  data.frame(
    row = integer(0),
    column = character(0),
    raw = character(0),
    declared_type = character(0),
    reason = character(0),
    stringsAsFactors = FALSE
  )
}


#' @title Declared Type of One Column
#' @description
#' Looks up the type a column spec declares, e.g. `"SAS Num"`. Returns `NA` when
#' the collection has no spec for the column, or the spec declares no type.
#' @param specs A `DTAColumnSpecCollection`, or `NULL`.
#' @param column Character. Name of the column.
#' @return A length-1 character, possibly `NA_character_`.
#' @keywords internal
dta_spec_declared_type <- function(specs, column) {
  structure <- dta_spec_column_structure(specs, column)

  if (is.null(structure)) {
    return(NA_character_)
  }

  declared <- tryCatch(as.list(structure)$type, error = function(e) NULL)
  if (is.null(declared) || length(declared) != 1 || is.na(declared)) {
    return(NA_character_)
  }

  as.character(declared)
}


#' @title Import Errors Collected From the Rule Layer
#' @description
#' Gathers the import errors every rule reported into the single frame carried
#' by `details$import_errors`, and stamps each one with the type its column
#' spec declares.
#' @param rule_results A list of rule results from `apply_rules()`.
#' @param specs A `DTAColumnSpecCollection`, or `NULL`.
#' @return A data.frame in the shape of `dta_empty_import_errors()`.
#' @keywords internal
dta_collect_import_errors <- function(rule_results, specs = NULL) {
  frames <- lapply(rule_results, function(result) {
    errors <- result$import_errors
    if (!is.data.frame(errors) || nrow(errors) == 0) NULL else errors
  })
  frames <- Filter(Negate(is.null), frames)

  if (length(frames) == 0) {
    return(dta_empty_import_errors())
  }

  out <- do.call(rbind, frames)

  # Two rules reading the same column report the same unrepresentable value.
  # That is one import error, not one per rule.
  out <- out[!duplicated(out[, c("row", "column"), drop = FALSE]), , drop = FALSE]
  out <- out[order(out$row, out$column), , drop = FALSE]

  declared <- vapply(
    out$column,
    function(column) dta_spec_declared_type(specs, column),
    character(1),
    USE.NAMES = FALSE
  )
  out$declared_type <- ifelse(is.na(declared), out$declared_type, declared)

  rownames(out) <- NULL
  out
}


#' @title Migrate Validation Details to the Current Schema Version
#' @description
#' Brings a `validate_table_detailed()` result recorded by an older version of
#' the package up to the current shape.
#'
#' Details written before the import axis existed carry no information about
#' whether values were representable in their declared type. Defaulting them to
#' `import_valid = TRUE` / `n_import_errors = 0` would be permissive by
#' construction: the artifact would assert a clean import axis it never checked.
#' They are therefore migrated to `NA` ("unknown"), and the recorded `ok` is
#' left exactly as it was rather than recomputed from incomplete data.
#' @param details A list as returned by `validate_table_detailed()`.
#' @return The same list, with the import fields and `result_version` present.
#' @keywords internal
dta_migrate_validation_details <- function(details) {
  if (!is.list(details)) {
    return(details)
  }

  if ("result_version" %in% names(details)) {
    return(details)
  }

  details$import_valid <- NA
  details$n_import_errors <- NA_integer_
  details["import_errors"] <- list(NULL)
  details$result_version <- 1L
  details
}


#' @title Tag Validation Details for Coercion
#' @description
#' Marks a `validate_table_detailed()` result so that `as.data.frame()` knows how
#' to flatten it. The list itself is untouched: names, order and contents are
#' unchanged, so every existing `details$...` caller keeps working.
#' @param details A list as returned by `validate_table_detailed()`.
#' @return The same list, with class `dta_validation_details` prepended.
#' @keywords internal
dta_as_validation_details <- function(details) {
  if (!is.list(details) || inherits(details, "dta_validation_details")) {
    return(details)
  }

  class(details) <- c("dta_validation_details", class(details))
  details
}


#' @title Coerce Validation Details to a Data Frame
#' @description
#' Flattens the detailed validation output for one table into one row per
#' reported error.
#'
#' The raw list cannot be coerced by the default method: `columnspec_errors` bundles
#' a *grouped* summary table with the *ungrouped* full error table, and those two
#' have different row counts, so `as.data.frame()` failed with "arguments imply
#' differing number of rows". This method flattens the errors themselves instead.
#' @param x A `dta_validation_details` object, as returned by
#'   `validation_errors()`.
#' @param row.names `NULL` or a character vector of row names.
#' @param optional Logical, passed to the default method's contract; unused.
#' @param ... Ignored.
#' @return A data.frame with one row per column spec error, followed by one row per
#'   rule failure, followed by one row per import error, and columns `source`,
#'   `rule_id`, `row`, `column`, `keyword` and `message`.
#' @examples
#' ds <- check(
#'   create_example_DTADataSetTabular(2),
#'   tables = "tab1",
#'   persist = FALSE,
#'   quiet = TRUE
#' )
#' errors <- as.data.frame(validation_errors(ds, table = "tab1"))
#' head(errors)
#' @export
as.data.frame.dta_validation_details <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  empty <- data.frame(
    source = character(0),
    rule_id = character(0),
    row = integer(0),
    column = character(0),
    keyword = character(0),
    message = character(0),
    stringsAsFactors = FALSE
  )

  full_error <- x$columnspec_errors$full_error
  schema_rows <- if (is.null(full_error) || nrow(full_error) == 0) {
    NULL
  } else {
    full_error <- as.data.frame(full_error, stringsAsFactors = FALSE)
    data.frame(
      source = "columnspec",
      rule_id = NA_character_,
      row = as.integer(full_error$row),
      column = as.character(full_error$column),
      keyword = as.character(full_error$keyword),
      message = as.character(full_error$message),
      stringsAsFactors = FALSE
    )
  }

  rule_errors <- x$rule_errors
  rule_rows <- if (is.null(rule_errors) || length(rule_errors) == 0) {
    NULL
  } else {
    data.frame(
      source = "rule",
      rule_id = vapply(
        rule_errors,
        function(e) if (is.null(e$id)) NA_character_ else as.character(e$id),
        character(1)
      ),
      row = NA_integer_,
      column = NA_character_,
      keyword = NA_character_,
      message = vapply(
        rule_errors,
        function(e) if (is.null(e$message)) NA_character_ else as.character(e$message),
        character(1)
      ),
      stringsAsFactors = FALSE
    )
  }

  import_errors <- x$import_errors
  import_rows <- if (!is.data.frame(import_errors) || nrow(import_errors) == 0) {
    NULL
  } else {
    data.frame(
      source = "import",
      rule_id = NA_character_,
      row = as.integer(import_errors$row),
      column = as.character(import_errors$column),
      keyword = as.character(import_errors$reason),
      message = dta_import_error_messages(import_errors),
      stringsAsFactors = FALSE
    )
  }

  out <- rbind(empty, schema_rows, rule_rows, import_rows)

  if (!is.null(row.names)) {
    rownames(out) <- row.names
  } else {
    rownames(out) <- NULL
  }

  out
}
