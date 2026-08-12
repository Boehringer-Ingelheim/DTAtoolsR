#' @keywords internal

`__dta_sas_type_aliases__` <- c(
  CHAR = "Char",
  NUM = "Num",
  INT = "Int",
  DATE = "Date",
  TIME = "Time",
  DATETIME = "DateTime"
)

`__dta_normalize_sas_type__` <- function(type) {
  if (is.null(type)) {
    return(NULL)
  }

  type_trimmed <- trimws(type)
  type_key <- toupper(type_trimmed)

  if (type_key %in% names(`__dta_sas_type_aliases__`)) {
    return(unname(`__dta_sas_type_aliases__`[[type_key]]))
  }

  type_trimmed
}

`__dta_sas_infer_type_from_format__` <- function(format) {
  if (is.null(format)) {
    return(NULL)
  }

  format_trimmed <- trimws(format)
  format_upper <- toupper(format_trimmed)

  if (grepl("^\\$[0-9]+\\.$", format_trimmed)) {
    return("Char")
  }
  if (grepl("^DATETIME[0-9]+\\.$", format_upper)) {
    return("DateTime")
  }
  if (grepl("^DATE[0-9]+\\.$", format_upper)) {
    return("Date")
  }
  if (grepl("^TIME[0-9]+\\.(?:[0-9]+)?$", format_upper)) {
    return("Time")
  }
  if (grepl("^[0-9]+\\.[0-9]+$", format_trimmed)) {
    return("Num")
  }
  if (grepl("^(?:BEST[0-9]+\\.|[0-9]+\\.)$", format_upper)) {
    return("Int")
  }

  NULL
}

`__dta_sas_normalize_format__` <- function(format) {
  if (is.null(format)) {
    return(NULL)
  }

  format_trimmed <- trimws(format)

  # Accept common shorthand for SAS character format and normalize to canonical form.
  if (grepl("^\\$[0-9]+$", format_trimmed)) {
    return(paste0(format_trimmed, "."))
  }

  format_trimmed
}

`__dta_sas_format_is_valid_for_type__` <- function(type, format) {
  if (is.null(type) || is.null(format)) {
    return(TRUE)
  }

  format_trimmed <- trimws(format)
  format_upper <- toupper(format_trimmed)

  switch(type,
    "Char" = grepl("^\\$[0-9]+\\.$", format_trimmed),
    "Num" = grepl("^(?:[0-9]+\\.|[0-9]+\\.[0-9]+|BEST[0-9]+\\.)$", format_upper),
    "Int" = grepl("^(?:[0-9]+\\.|BEST[0-9]+\\.)$", format_upper),
    "Date" = grepl("^DATE[0-9]+\\.$", format_upper),
    "Time" = grepl("^TIME[0-9]+\\.(?:[0-9]+)?$", format_upper),
    "DateTime" = grepl("^DATETIME[0-9]+\\.$", format_upper),
    FALSE
  )
}

#' @title DTA Column Spec Structure SAS
#' @description
#' Class for column types for SAS structures.
#'
#' This class defines the structure of a column in a DTA dataset.
#' @import S7
#' @importFrom stringr str_glue
#' @export
#'
#' @param type Character or NA. The type of the column.
#' @param format Character or NA. The format of the column.
#' @param length Numeric or NA. The max character length.
#' @examples
#' DTAColumnSpecStructureSAS(type = "Char", format = "$12.", length = 12)
DTAColumnSpecStructureSAS <- S7::new_class(
  "DTAColumnSpecStructureSAS",
  parent = DTAColumnSpecStructure,
  constructor = function(
    type = NULL,
    format = NULL,
    length = NULL
  ) {
    type <- `__dta_normalize_sas_type__`(type)
    format <- `__dta_sas_normalize_format__`(format)

    inferred_type <- `__dta_sas_infer_type_from_format__`(format)

    if (is.null(type) && !is.null(inferred_type)) {
      type <- inferred_type
    }

    new_object(
      .parent = DTAColumnSpecStructure(
        type = type,
        format = format,
        length = length,
        backend = "SAS"
      )
    )
  },
  validator = function(self) {
    # backend cannot be empty character
    if (is.null(self@backend) || self@backend == "") {
      "'backend' must be defined and cannot be an empty character."
    }

    supported_types <- unname(`__dta_sas_type_aliases__`)

    if (!is.null(self@type) && !(self@type %in% supported_types)) {
      return(str_glue(
        "'type' must be one of: {paste(supported_types, collapse = ', ')}."
      ))
    }

    if (!is.null(self@format) && is.null(`__dta_sas_infer_type_from_format__`(self@format))) {
      return(str_glue(
        "Unsupported SAS format: '{self@format}'. Supported families are $w., w., w.d, BESTw., DATEw., TIMEw(.d), DATETIMEw."
      ))
    }

    if (!`__dta_sas_format_is_valid_for_type__`(self@type, self@format)) {
      return(str_glue(
        "'format' '{self@format}' is not compatible with type '{self@type}'."
      ))
    }
  }
)


#' @title as.list method for as.list.DTAColumnSpecStructureSAS
#' @description
#' Converts a DTAColumnSpecStructureSAS object to a named list. An unset `type`
#' or `format` is omitted from the list instead of being written as the bare
#' backend prefix (`"SAS "`), which would re-parse to an empty value and fail
#' the SAS validator on the next read.
#' @param x A DTAColumnSpecStructureSAS object.
#' @param ... Additional arguments (ignored).
#' @return A named list with the DTAColumnSpecStructureSAS properties that are set.
#' @export
#' @name as.list
method(as.list, DTAColumnSpecStructureSAS) <- function(x, ...) {
  out <- list()
  if (.structure_value_is_set(x@type)) {
    out$type <- .structure_backend_value(x@backend, x@type)
  }
  if (.structure_value_is_set(x@format)) {
    out$format <- .structure_backend_value(x@backend, x@format)
  }
  if (!is.null(x@length)) {
    out$length <- x@length
  }
  out
}

#' @title as_json_schema_type
#' @description
#' Converts a DTAColumnSpecStructure to a JSON Schema type.
#' @name as_json_schema_type
#' @export
if (!exists("as_json_schema_type", mode = "function")) {
  as_json_schema_type <- new_generic("as_json_schema_type", "x")
}
method(as_json_schema_type, DTAColumnSpecStructureSAS) <- function(x) {
  switch(x@type,
    "Char" = "string",
    "Num" = "number",
    "Int" = "integer",
    "Date" = "string",
    "Time" = "string",
    "DateTime" = "string",
    "Bool" = "boolean",
    "string"
  ) # fallback
}


#' @title as_r_type
#' @description
#' The R storage type a declared column type maps to. This is the import-time
#' sibling of [as_json_schema_type()]: that generic says how a column is
#' *validated*, this one says how it is *stored* when the table is read.
#'
#' The two must agree, so every declared type whose JSON Schema type is
#' `"string"` maps to `"character"` here. `Date`, `Time` and `DateTime` are
#' validated as strings by their pattern and format, so they are stored as the
#' text that was read rather than parsed into `Date`/`POSIXct` -- parsing them
#' would re-render the value and validate something other than what the file
#' contained.
#'
#' `Char` maps to `"character"`, which is what stops the import layer from ever
#' touching a character column: a `SUBJECT_ID` of `"007"` must survive import
#' unchanged, and any numeric round trip would silently make it `7`.
#' @param x A `DTAColumnSpecStructure` object.
#' @return A length-1 character naming an R type: one of `"character"`,
#'   `"double"`, `"integer"` or `"logical"`. Defaults to `"character"` when the
#'   spec declares no type, so an unspecified column is never coerced.
#' @examples
#' as_r_type(DTAColumnSpecStructureSAS(type = "Num"))
#' as_r_type(DTAColumnSpecStructureSAS(type = "Char", format = "$12.", length = 12))
#' @name as_r_type
#' @export
if (!exists("as_r_type", mode = "function")) {
  as_r_type <- new_generic("as_r_type", "x")
}

#' @export
method(as_r_type, DTAColumnSpecStructureSAS) <- function(x) {
  # An unset type carries no instruction to convert anything, so it falls back
  # to the type that the coercion layer leaves alone.
  if (!.structure_value_is_set(x@type)) {
    return("character")
  }

  switch(x@type,
    "Char" = "character",
    "Num" = "double",
    "Int" = "integer",
    "Date" = "character",
    "Time" = "character",
    "DateTime" = "character",
    "Bool" = "logical",
    "character"
  ) # fallback
}


#' @title print
#' @description
#' prints info of the column spec structure
#' @name print
#' @export
if (!exists("print", mode = "function")) {
  print <- new_generic("print", "x")
}
#' @export
method(print, DTAColumnSpecStructureSAS) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTAColumnSpecStructureSAS}>")
  print_info(x)
  invisible(x)
}

## SAS Formats
# Character formats in SAS always begin with a dollar sign ($) followed by a number indicating the width of the field.
# For example, $10. indicates a character format with a width of 10 characters.

# $w.
# BothBasic character format of width w. As an informat,
# $w. reads up to w characters from raw text (trimming trailing blanks by default),
# and as an output format it displays character values left-aligned in a field of width w [SAS Format...n SAS Code].
# This is the default format if a character variable has length w and no other format is assigned.

# Numeric Formats:
# w.
# BothBasic numeric format with total width w and no decimal places. As output, it displays an integer with up to w
# digits (no decimal point) [SAS Format...n SAS Code]. As input, w. can read an integer field of width w.
# Example: value 1234 with format 6. prints as 1234 (right-aligned in 6 spaces).

# w.d
# BothGeneral numeric format with w total width including d decimal places [SAS Format...n SAS Code].
# Output: rounds and displays number with d digits after the decimal.
# Input: can read a number with d decimals (assuming an explicit decimal point in the raw data or a fixed decimal position).
# Example: 8.2 displays 123.456 as   123.46 (8 width, 2 decimals).

# BESTw.
# Output (default)The BEST format (with width w) is SASâ€™s default for numeric output.
# It attempts to choose the most compact representation of the number within w columns,
# using scientific notation if necessary [SAS Format...n SAS Code]. It prints integers without a decimal
# and shows as many significant digits as will fit. E.g., BEST12. might display 123456789 as 123456789 but a
# very large number in scientific notation.

# Date Formats:
# DATEw.
# Generic SAS date format. Output: DATE7. or DATE9. prints dates as ddMONyy or ddMONyyyy
# (day, 3-letter month, year) depending on width [SAS Format...n SAS Code]. For example, a SAS date value for
# 2025-08-15 displays as 15AUG25 with DATE7. or 15AUG2025 with DATE9. [SAS Format...n SAS Code].
# Input: the same format will read strings like "15AUG25" into a date value.

# TIMEw.d
# Time-of-day format. Output: prints time values as hh:mm:ss (with optional fractional seconds if d > 0).
#  For example, a SAS time value of 3661 seconds (1:01:01) with TIME8. displays 01:01:01. TIME5. would show
# 01:01 (hours:minutes). Input: can read times in hh:mm:ss form.

# DATETIMEw.
# Date-time format combining date and time. By default (e.g., DATETIME18.), it prints as ddMONyy:hh:mm:ss
#  (or with year as yyyy if width allows).
#  For example, a datetime value for 01-Oct-2025 14:30:00 with DATETIME18. -> 01OCT25:14:30 (with DATETIME20. -> 01OCT2025:14:30:00).
# These formats are useful for timestamp variables. (Corresponding informats like ANYDTDTM can read a wide range of datetime strings.)
