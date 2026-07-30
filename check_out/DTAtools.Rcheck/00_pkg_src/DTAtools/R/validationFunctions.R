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
#' @return Transformed and checked table (a data.frame) if valid, aborts otherwise. If invalid, returns a list containing summarised and full error data frames.
#' @export
# TODO: consider moving `validate_table()` into DTADataSet-class.R.
validate_table <- function(specs, table, verbose = TRUE) {
  details <- validate_table_detailed(specs = specs, table = table, verbose = verbose)

  if (!isTRUE(details$schema_valid)) {
    return(details$schema_errors)
  }

  if (!isTRUE(details$rules_valid)) {
    messages <- vapply(details$rule_errors, function(x) x$message, character(1))
    cli::cli_abort(c("Schema rule violations:", messages))
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

  list(
    ok = !has_schema_errors && isTRUE(rules_valid),
    schema_valid = !has_schema_errors,
    rules_valid = isTRUE(rules_valid),
    n_schema_errors = if (is.null(full_error)) 0 else nrow(full_error),
    n_rule_errors = length(rule_errors),
    schema_errors = list(
      summarised_error = summarised_error,
      full_error = full_error
    ),
    rule_results = rule_results,
    rule_errors = rule_errors
  )
}
