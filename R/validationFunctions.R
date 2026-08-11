#' @title Validate Table Against JSON Schema
#' @description Validates a data.frame against a JSON Schema using jsonvalidate. The table is split into smaller chunks for validation to avoid argument limits.
#' @importFrom jsonlite toJSON
#' @importFrom jsonvalidate json_schema
#' @importFrom dplyr select mutate group_by summarise across n distinct filter any_of everything where
#' @importFrom tidyr separate_wider_delim
#' @importFrom cli cli_alert_danger cli_alert_success cli_h3 cli_abort cli_alert_info
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @param table A data.frame to validate.
#' @param specs A specs object.
#' @param verbose Logical. If TRUE (default), prints validation progress.
#' @details
#' Both axes -- the column specs and the schema rules -- are always evaluated,
#' and both are reported in a single pass. When the table has schema errors, the
#' returned error list additionally carries `rules_valid` and `rule_errors`, and
#' any rule violations are raised as a warning so they cannot go unnoticed while
#' the schema errors are being fixed.
#' @return Transformed and checked table (a data.frame) if valid. If the table
#'   has schema errors, returns a list with `summarised_error`, `full_error`,
#'   `rules_valid` and `rule_errors`. If the schema is valid but rules are
#'   violated, aborts.
#' @export
# TODO: consider moving `validate_table()` into DTADataSet-class.R.
validate_table <- function(specs, table, verbose = TRUE) {
  details <- validate_table_detailed(specs = specs, table = table, verbose = verbose)

  # Always evaluate both axes: a schema error must never hide a rule violation.
  rule_messages <- if (isTRUE(details$rules_valid)) {
    character(0)
  } else {
    vapply(details$rule_errors, function(x) x$message, character(1))
  }

  if (!isTRUE(details$schema_valid)) {
    schema_errors <- details$schema_errors
    schema_errors$rules_valid <- isTRUE(details$rules_valid)
    schema_errors$rule_errors <- details$rule_errors
    schema_errors$import_valid <- details$import_valid
    schema_errors$n_import_errors <- details$n_import_errors
    schema_errors["import_errors"] <- list(details$import_errors)

    if (length(rule_messages) > 0) {
      bullets <- c("Schema rule violations were also found:", rule_messages)
      names(bullets) <- c("", rep("x", length(rule_messages)))
      cli::cli_warn(bullets)
    }

    return(schema_errors)
  }

  if (length(rule_messages) > 0) {
    cli::cli_abort(c("Schema rule violations:", rule_messages))
  }

  # The import axis fails validation independently of schema and rules. No
  # import errors are produced yet, so this branch is unreachable in this
  # version; it exists so that the axis can never be reported as passing here
  # once coercion starts recording them.
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

  schema_json <- tryCatch(
    specs@json_schema,
    error = function(e) NULL
  )

  if (is.null(schema_json)) {
    schema_json <- as_json_schema(specs)
  }

  # Confirm JSON schema
  obj <- jsonvalidate::json_schema$new(schema_json)

  # Split the table into smaller chunks
  num_rows <- nrow(table)
  chunk_size <- 5000
  chunks <- split(table, ceiling(seq_len(num_rows) / chunk_size))
  schema_error_summaries <- list()
  schema_error_full <- list()

  # progress bar settings
  n_chunks <- length(chunks)
  pb <- NULL
  if (isTRUE(verbose)) {
    cli::cli_h3("validating with column specs")
    if (n_chunks > 1) {
      pb <- txtProgressBar(min = 1, max = max(c(n_chunks, 2)), style = 3)
    }
  }

  for (name in names(chunks)) {
    i <- as.numeric(name)

    row_addition <- chunk_size * (i - 1)

    if (isTRUE(verbose) && !is.null(pb)) {
      setTxtProgressBar(pb, i)
    }

    chunk <- chunks[[name]]

    # Convert the chunk of the table to JSON
    json_data <- jsonlite::toJSON(
      chunk,
      dataframe = "rows",
      auto_unbox = TRUE,
      na = "null"
    )

    result <- obj$validate(json_data, verbose = TRUE, greedy = TRUE)

    if (!result) {
      error_df <- as.data.frame(attributes(result)$errors)
      params <- as.data.frame(error_df$params)
      colnames(params) <- paste0(c("params."), colnames(params))

      parent_schema <- as.data.frame(error_df$parentSchema)
      colnames(parent_schema) <- paste0(
        c("parentSchema."),
        colnames(parent_schema)
      )

      if (any(grepl("required", error_df$keyword))) {
        full_error_df <- error_df %>%
          dplyr::select(instancePath, keyword, message, schema, data) %>%
          cbind(., parent_schema, params) %>%
          dplyr::mutate(instancePath = gsub("^/", "", instancePath)) %>%
          tidyr::separate_wider_delim(
            names = c("row", "column"),
            delim = "/",
            cols = instancePath,
            too_few = "align_start"
          ) %>%
          dplyr::mutate(row = as.numeric(row) + 1 + row_addition)
        summarised_error <- full_error_df %>%
          dplyr::filter(keyword == "required") %>%
          dplyr::select(keyword, message) %>%
          dplyr::distinct()
      } else {
        full_error_df <- error_df %>%
          dplyr::select(instancePath, keyword, message, schema, data) %>%
          cbind(., parent_schema, params) %>%
          dplyr::mutate(instancePath = gsub("^/", "", instancePath)) %>%
          tidyr::separate_wider_delim(
            names = c("row", "column"),
            delim = "/",
            cols = instancePath,
            too_few = "align_start"
          ) %>%
          dplyr::mutate(across(
            where(is.list),
            ~ sapply(., function(x) paste(x, collapse = "; "))
          )) %>%
          dplyr::mutate(row = as.numeric(row) + 1 + row_addition)

        summarised_error <- full_error_df %>%
          dplyr::group_by(across(c(-row))) %>%
          dplyr::summarise(
            first.row.affected = min(row),
            last.row.affected = max(row),
            n.rows.affected = dplyr::n()
          )
      }

      schema_error_summaries[[length(schema_error_summaries) + 1]] <- summarised_error
      schema_error_full[[length(schema_error_full) + 1]] <- full_error_df
    }
  }

  if (isTRUE(verbose) && !is.null(pb)) {
    close(pb)
  }

  has_schema_errors <- length(schema_error_full) > 0
  summarised_error <- if (length(schema_error_summaries) > 0) {
    do.call(rbind, schema_error_summaries)
  } else {
    NULL
  }
  full_error <- if (length(schema_error_full) > 0) {
    do.call(rbind, schema_error_full)
  } else {
    NULL
  }

  if (!has_schema_errors && isTRUE(verbose)) {
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

    rule_results <- apply_schema_rules(rules_list, table, verbose = verbose)
    rule_errors <- Filter(function(x) !isTRUE(x$valid), rule_results)
    rules_valid <- length(rule_errors) == 0
  }

  # Import axis. Nothing produces import errors yet: the axis is wired through
  # every result shape first, so that when coercion starts recording errors it
  # cannot silently remove per-row schema errors and make a bad file look
  # cleaner than it is.
  import_errors <- NULL
  n_import_errors <- 0L
  import_valid <- TRUE

  details <- list(
    ok = NA,
    schema_valid = !has_schema_errors,
    rules_valid = isTRUE(rules_valid),
    import_valid = isTRUE(import_valid),
    n_schema_errors = if (is.null(full_error)) 0 else nrow(full_error),
    n_rule_errors = length(rule_errors),
    n_import_errors = n_import_errors,
    schema_errors = list(
      summarised_error = summarised_error,
      full_error = full_error
    ),
    rule_results = rule_results,
    rule_errors = rule_errors,
    import_errors = import_errors,
    schema_version = 2L
  )

  details$ok <- dta_details_ok(details)
  details
}


#' @title Overall Validity from the Three Validation Axes
#' @description
#' A table is valid only when all three independent axes pass: the column specs
#' (schema), the schema rules, and the import axis. A value that could not be
#' represented in its declared type fails the run on its own, regardless of what
#' schema and rules report about the coerced column.
#'
#' `NA` on any axis ("unknown", e.g. an artifact written before the import axis
#' existed) is not a pass.
#' @param details A list carrying `schema_valid`, `rules_valid` and
#'   `import_valid`.
#' @return `TRUE` only when all three axes are `TRUE`.
#' @keywords internal
dta_details_ok <- function(details) {
  isTRUE(details$schema_valid) &&
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
#' @return The same list, with the import fields and `schema_version` present.
#' @keywords internal
dta_migrate_validation_details <- function(details) {
  if (!is.list(details)) {
    return(details)
  }

  if ("schema_version" %in% names(details)) {
    return(details)
  }

  details$import_valid <- NA
  details$n_import_errors <- NA_integer_
  details["import_errors"] <- list(NULL)
  details$schema_version <- 1L
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
#' The raw list cannot be coerced by the default method: `schema_errors` bundles
#' a *grouped* summary table with the *ungrouped* full error table, and those two
#' have different row counts, so `as.data.frame()` failed with "arguments imply
#' differing number of rows". This method flattens the errors themselves instead.
#' @param x A `dta_validation_details` object, as returned by
#'   `validation_errors()`.
#' @param row.names `NULL` or a character vector of row names.
#' @param optional Logical, passed to the default method's contract; unused.
#' @param ... Ignored.
#' @return A data.frame with one row per schema error, followed by one row per
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

  full_error <- x$schema_errors$full_error
  schema_rows <- if (is.null(full_error) || nrow(full_error) == 0) {
    NULL
  } else {
    full_error <- as.data.frame(full_error, stringsAsFactors = FALSE)
    data.frame(
      source = "schema",
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
