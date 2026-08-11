#' @title Generate table containing all DTA column specs
#' @description
#' This function takes a DTAColumnSpecCollection and generates a Word document containing a table with the DTA columns specs.
#' @importFrom yaml read_yaml
#' @importFrom dplyr any_of everything where
#' @importFrom cli cli_alert_success cli_abort
#' @importFrom magrittr %>%
#' @importFrom flextable flextable font bold bg width border_outer border_inner align padding valign save_as_docx
#' @param DTAColumnSpecCollection A DTAColumnSpecCollection object containing
#'   column specifications.
#' @param file Character. The name of the output Word file in which the DTA Spec Table shall be written. Default is "dta_spec_table.docx".
#' @param overwrite Logical. whether to overwrite the file.
#' @param colnames Vector. Vector containing column names of the DTA table. Default: c("Variable Name", "Variable Label", "Type", "Length", "Format", "Nullable", "Description")
#' @param quiet Logical. If TRUE, suppresses console output. Default is FALSE.
#' @return Flextable object that is saved.
#' @export
#' @examples
#' # No runnable example yet.
#' # Word export examples are intentionally skipped until the API is reworked.
export_specs_table <- function(
  DTAColumnSpecCollection,
  file = "dta_spec_table.docx",
  overwrite = FALSE,
  colnames = c(
    "Variable Name",
    "Variable Label",
    "Type",
    "Length",
    "Format",
    "Nullable",
    "Description"
  ),
  quiet = FALSE
) {
  if (!inherits(DTAColumnSpecCollection, "DTAtools::DTAColumnSpecCollection")) {
    cli::cli_abort("'DTAColumnSpecCollection' must be a DTAColumnSpecCollection object.")
  }

  specs <- DTAColumnSpecCollection@columns
  if (length(specs) == 0) {
    cli::cli_abort("'DTAColumnSpecCollection' has no columns to export.")
  }

  get_spec_type <- function(spec) {
    structure <- spec@structure
    if (is.null(structure) || is.null(structure@type)) {
      return(NA_character_)
    }
    if (!is.null(structure@backend) && nzchar(structure@backend)) {
      return(paste(structure@backend, structure@type))
    }
    structure@type
  }

  get_spec_format <- function(spec) {
    structure <- spec@structure
    if (is.null(structure) || is.null(structure@format)) {
      return(NA_character_)
    }
    as.character(structure@format)
  }

  get_spec_length <- function(spec) {
    structure <- spec@structure
    if (is.null(structure) || is.null(structure@length)) {
      return(NA_real_)
    }
    as.numeric(structure@length)
  }

  get_spec_description <- function(spec) {
    desc <- if (!is.null(spec@description)) spec@description else ""
    values <- spec@values
    pattern <- spec@pattern

    if (!is.null(values)) {
      value_line <- paste0(
        "\n#@values: ",
        paste(as.vector(unlist(values)), collapse = "; ")
      )
      desc <- paste(desc, value_line, sep = "\n")
    }

    if (!is.null(pattern)) {
      pattern_line <- paste0("\n#@pattern: ", pattern)
      desc <- paste(desc, pattern_line, sep = "\n")
    }

    desc
  }

  df <- data.frame(
    `Variable Name` = sapply(specs, function(spec) spec@id),
    `Variable Label` = sapply(specs, function(spec) {
      if (is.null(spec@label)) NA_character_ else as.character(spec@label)
    }),
    `Type` = sapply(specs, get_spec_type),
    `Format` = sapply(specs, get_spec_format),
    `Length` = sapply(specs, get_spec_length),
    `Nullable` = sapply(specs, function(x) {
      if (is.null(x@nullable)) NA else ifelse(x@nullable, "Yes", "No")
    }),
    `Description` = sapply(specs, get_spec_description),
    check.names = FALSE
  )

  if (
    suppressWarnings(all(
      colnames ==
        c(
          "Variable Name",
          "Variable Label",
          "Type",
          "Format",
          "Nullable",
          "Description"
        )
    ))
  ) {
    # Subset to the requested columns (as the 7-name branch does). Without this
    # the excluded "Length" column survived into the table and the j = 1..6
    # widths below were applied to a 7-column flextable.
    df <- df %>%
      dplyr::select(dplyr::any_of(colnames))

    ft <- flextable::flextable(df) %>%
      flextable::font(fontname = "Times New Roman", part = "all") %>%
      flextable::bold(part = "header") %>%
      flextable::bg(i = 1, j = NULL, bg = "grey", part = "header") %>%
      flextable::fontsize(size = 11, part = "all") %>%
      flextable::width(j = 1, width = 2.21 / 2.54) %>%
      flextable::width(j = 2, width = 2.28 / 2.54) %>%
      flextable::width(j = 3, width = 0.88 / 2.54) %>%
      flextable::width(j = 4, width = 0.89 / 2.54) %>%
      flextable::width(j = 5, width = 0.88 / 2.54) %>%
      flextable::width(j = 6, width = 9.75 / 2.5) %>%
      flextable::border_outer(part = "all") %>%
      flextable::border_inner(part = "all") %>%
      flextable::align(i = 1, j = NULL, align = "center", part = "header") %>%
      flextable::align(align = "left", part = "body") %>%
      flextable::padding(padding = 1, part = "all") %>%
      flextable::valign(part = "body", valign = "top")
  } else if (
    suppressWarnings(all(
      colnames ==
        c(
          "Variable Name",
          "Variable Label",
          "Type",
          "Length",
          "Format",
          "Nullable",
          "Description"
        )
    ))
  ) {
    df <- df %>%
      dplyr::select(dplyr::any_of(colnames))

    ft <- flextable::flextable(df) %>%
      flextable::font(fontname = "Times New Roman", part = "all") %>%
      flextable::bold(part = "header") %>%
      flextable::bg(i = 1, j = NULL, bg = "grey", part = "header") %>%
      flextable::fontsize(size = 9, part = "all") %>%
      flextable::width(j = 1, width = 1.99 / 2.54) %>%
      flextable::width(j = 2, width = 2.5 / 2.54) %>%
      flextable::width(j = 3, width = 1 / 2.54) %>%
      flextable::width(j = 4, width = 0.75 / 2.54) %>%
      flextable::width(j = 5, width = 0.75 / 2.54) %>%
      flextable::width(j = 6, width = 0.75 / 2.54) %>%
      flextable::width(j = 7, width = 9.5 / 2.5) %>%
      flextable::border_outer(part = "all") %>%
      flextable::border_inner(part = "all") %>%
      flextable::align(i = 1, j = NULL, align = "left", part = "header") %>%
      flextable::align(align = "left", part = "body") %>%
      flextable::padding(padding = 1, part = "all") %>%
      flextable::valign(part = "body", valign = "top")
  } else {
    cli::cli_abort(c(
      "colnames not supported!",
      x = "Specify one of these two: ",
      i = 'c("Variable Name", "Variable Label", "Type",
      "Length", "Format", "Nullable", "Description")',
      i = 'c("Variable Name", "Variable Label",
      "Type", "Format", "Nullable", "Description")'
    ))
  }
  if (file.exists(file) && !overwrite) {
    cli::cli_abort(c(
      "File exists!",
      i = "Specify 'overwrite = TRUE' to overwrite the current file."
    ))
  } else {
    ft %>%
      flextable::save_as_docx(path = file)
    if (!isTRUE(quiet)) {
      cli::cli_alert_success(
        "Table has been written to {file} successfully."
      )
    }
  }

  invisible(ft)
}

#' @title Generate table containing all potential values of a column
#' @description
#' This function takes all values defined in a column and prints a word table containing those values. Can be copied into the DTA
#' @importFrom yaml read_yaml
#' @importFrom magrittr %>%
#' @importFrom cli cli_alert_success
#' @importFrom flextable flextable font bold bg italic border_outer border_inner align padding valign save_as_docx
#' @param DTAColumnSpecCollection A DTAColumnSpecCollection object containing
#'   column specifications.
#' @param file Character. The name of the word file, to which the table shall be written. Default is "column_value_table.docx".
#' @param id Character. The id of the column for which a table with all its values shall be generated.
#' @param quiet Logical. If TRUE, suppresses console output. Default is FALSE.
#' @return None. The function creates a Word document.
#' @export
#' @examples
#' # No runnable example yet.
#' # Word export examples are intentionally skipped until the API is reworked.
export_column_value_table <- function(
  DTAColumnSpecCollection,
  file = "column_value_table.docx",
  id,
  quiet = FALSE
) {
  # get values from column within DTAColumnSpecCollection
  specs <- DTAColumnSpecCollection@columns[[id]]

  df <- data.frame(id = specs@values)
  colnames(df) <- id

  # Add a table with the specified format
  flextable::flextable(df) %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::italic(part = "all") %>%
    flextable::bold(part = "header") %>%
    flextable::bg(i = 1, j = NULL, bg = "grey", part = "header") %>%
    flextable::fontsize(size = 11, part = "all") %>%
    flextable::border_outer(part = "all") %>%
    flextable::border_inner(part = "all") %>%
    flextable::align(align = "left", part = "all") %>%
    flextable::padding(padding = 5, part = "all") %>%
    flextable::valign(part = "body", valign = "top") %>%
    flextable::save_as_docx(path = file)

  if (!isTRUE(quiet)) {
    cli::cli_alert_success(
      "Table has been written to {.file {file}} successfully."
    )
  }
  invisible(df)
}

#' Write Metadata to File
#' @description Writes metadata (MD5 checksum, number of rows and columns) to a separate `.md5` sidecar file.
#' @param file Path to the input data file.
#' @param table A data.frame that was written to the file.
#' @param write_to_file Logical. Whether to write information to an additional file.
#' @param quiet Logical. If TRUE, suppresses console output. Default is FALSE.
#' @return Invisibly returns a list with `md5sum`, `n_rows`, and `n_cols`.
#' @importFrom cli cli_alert_info cli_abort
#' @importFrom tools md5sum
#' @keywords internal
write_metadata <- function(file, table, write_to_file, quiet = FALSE) {
  if (!file.exists(file)) {
    cli::cli_abort("File does not exist: {file}")
  }

  # Calculate metadata
  checksum <- unname(tools::md5sum(file))
  n_rows <- nrow(table)
  n_cols <- ncol(table)

  if (!isTRUE(quiet)) {
    cli::cli_alert_info("md5sum: {checksum}")
    cli::cli_alert_info("Number of Columns: {n_cols}")
    cli::cli_alert_info("Number of Rows: {n_rows}")
  }

  if (write_to_file) {
    # Format metadata lines
    metadata_lines <- c(
      paste0("md5sum: ", checksum),
      paste0("Number of Columns: ", n_cols),
      paste0("Number of Rows: ", n_rows)
    )

    # Define metadata file path
    metadata_file <- paste0(file, ".md5")

    # Write metadata to separate file
    writeLines(metadata_lines, metadata_file, useBytes = TRUE)

    if (!isTRUE(quiet)) {
      cli::cli_alert_info("Metadata written to {.file {metadata_file}}")
    }
  }

  invisible(list(
    md5sum = checksum,
    n_rows = n_rows,
    n_cols = n_cols
  ))
}

