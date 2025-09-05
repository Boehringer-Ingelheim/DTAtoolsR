#' @title Check Format
#' @description
#' This function checks if the length of each element in a vector is less than or equal to a specified format length.
#'
#' @param value A vector of values.
#' @param var_format Integer. The maximum length allowed for each element.
#' @return Logical. TRUE if all elements are within the specified format length, FALSE otherwise.
#' @examples
#' checkFormat(c("abc", "defg"), 4)
#' @export
checkFormat <- function(value, var_format) {
  value[is.na(value)] <- ""
  return(all(nchar(value) <= var_format))
}

#' @title Check Type
#' @description
#' This function checks if the type of a vector matches the specified type.
#'
#' @param vector A vector of values.
#' @param var_type Character. The expected type of the vector.
#' @return Logical. TRUE if the vector matches the specified type, FALSE otherwise.
#' @examples
#' checkType(c(1, 2, 3), "numeric")
#' @export
checkType <- function(vector, var_type) {
  if (all(is.na(vector))) {
    return(TRUE)
  } else {
    return(class(vector) == var_type)
  }
}

#' @title Check Nullable
#' @description
#' This function checks if a vector contains non-null values when nullable is FALSE.
#'
#' @param vector A vector of values.
#' @param nullable Logical. Indicates if the vector can contain null values.
#' @return Logical. TRUE if the vector meets the nullable condition, FALSE otherwise.
#' @examples
#' checkNullable(c(1, 2, NA), FALSE)
#' @export
checkNullable <- function(vector, nullable) {
  if (!nullable) {
    return(all(!is.na(vector) & vector != ""))
  } else {
    return(TRUE)
  }
}

#' @title Change NAs
#' @description
#' This function replaces NA values in a vector with an empty string if the vector type is character.
#'
#' @param vector A vector of values.
#' @param var_type Character. The type of the vector.
#' @return The modified vector with NAs replaced by an empty string if the type is character.
#' @examples
#' changeNAs(c(NA, "b", "c"), "character")
#' @export
changeNAs <- function(vector, var_type) {
  tmp <- vector
  if (var_type == "character") {
    tmp[is.na(tmp)] <- ""
    return(tmp)
  } else {
    return(vector)
  }
}


#' @title Change Type
#' @description
#' This function changes the type of a vector to the specified type.
#' @importFrom cli cli_abort
#'
#' @param vector A vector of values.
#' @param var_type Character. The desired type of the vector.
#' @return The vector converted to the specified type.
#' @examples
#' changeType(c("1", "2", "3"), "numeric")
#' @export
changeType <- function(vector, var_type) {
  if (var_type == "numeric") {
    return(as.numeric(vector))
  } else if (var_type == "character") {
    return(as.character(vector))
  } else {
    cli::cli_abort(c(
      "Change type error.",
      x = "Column '{column_name}' can not be transformed into '{column_type}' type"
    ))
  }
}

#' @title Validate Table Against JSON Schema
#' @description Validates a data.frame against a JSON Schema using jsonvalidate. The table is split into smaller chunks for validation to avoid argument limits.
#' @importFrom jsonlite toJSON
#' @importFrom jsonvalidate json_schema
#' @importFrom dplyr select mutate group_by summarise across n distinct
#' @importFrom tidyr separate_wider_delim
#' @importFrom cli cli_alert_danger cli_alert_success cli_h3 cli_abort cli_alert_info
#' @param table A data.frame to validate.
#' @param DTAColumnSpecCollection A DTAColumnSpecCollection object.
#' @return Transformed and checked table (a data.frame) if valid, aborts otherwise. If invalid, returns a list containing summarised and full error data frames.
#' @export
validateTable <- function(DTAColumnSpecCollection, table) {
  # Confirm JSON schema
  obj <- jsonvalidate::json_schema$new(DTAColumnSpecCollection@json_schema)

  # Split the table into smaller chunks
  num_rows <- nrow(table)
  chunk_size <- 5000
  chunks <- split(table, ceiling(seq_len(num_rows) / chunk_size))

  # progress bar settings
  n_chunks <- length(chunks)
  pb <- txtProgressBar(min = 1, max = max(c(n_chunks, 2)), style = 3)

  # Validate each chunk
  cli::cli_alert_info("Validate Table using jsonschema.\n")
  for (name in names(chunks)) {
    i <- as.numeric(name)

    row_addition <- chunk_size * (i - 1)

    setTxtProgressBar(pb, i)

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
      # print formatted error when the table is not valid
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

      cli::cli_alert_danger(
        "A chunk (size = {chunk_size}) of the table does not conform to the schema. Please check the data.frames containing the error messages in @tables of your dtatools object for more details."
      )
      return(list(
        summarised_error = summarised_error,
        full_error = full_error_df
      ))
    }
  }
  close(pb)

  cli::cli_alert_success(
    "Table format, length, pattern, and values are valid."
  )

  rules <- getRules(DTAColumnSpecCollection)
  if (length(rules) > 0) {
    cli::cli_h2("Checking schema rules")
    results <- applySchemaRules(rules, table)
    failed <- Filter(function(x) !x$valid, results)
    if (length(failed) > 0) {
      messages <- vapply(failed, function(x) x$message, character(1))
      cli::cli_abort(c("Schema rule violations:", messages))
    }
  }
  cli::cli_h3("")
  cli::cli_alert_success("Table is valid.")

  return(table)
}
