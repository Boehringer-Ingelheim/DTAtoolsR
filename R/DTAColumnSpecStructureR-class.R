#' @include DTAColumnSpecStructureSAS-class.R
NULL

`__dta_r_type_aliases__` <- c(
  CHARACTER = "character",
  NUMERIC = "numeric",
  DOUBLE = "numeric",
  INTEGER = "integer",
  LOGICAL = "logical",
  DATE = "Date",
  POSIXCT = "POSIXct",
  HMS = "hms"
)

`__dta_normalize_r_type__` <- function(type) {
  if (is.null(type)) {
    return(NULL)
  }

  type_trimmed <- trimws(type)
  type_key <- toupper(type_trimmed)

  if (length(type_key) == 1L && !is.na(type_key) && type_key %in% names(`__dta_r_type_aliases__`)) {
    return(unname(`__dta_r_type_aliases__`[[type_key]]))
  }

  type_trimmed
}

#' @title DTA Column Spec Structure R
#' @description
#' Class for column types of the R backend: the declared type of a column is
#' written \code{R numeric}, \code{R Date}, and so on.
#'
#' The accepted types are \code{character}, \code{numeric} (\code{double} is
#' accepted as a synonym), \code{integer}, \code{logical}, \code{Date},
#' \code{POSIXct} and \code{hms}; they are matched case-insensitively and
#' stored in that spelling. Only \code{Date}, \code{POSIXct} and \code{hms} take
#' a \code{format}, a strptime-style string built from \code{\%Y}, \code{\%m},
#' \code{\%d}, \code{\%H}, \code{\%M}, \code{\%S}, \code{\%OS1} to \code{\%OS6},
#' \code{\%F} and \code{\%T}, with a space, \code{- / . , : ;} \code{_} and the
#' letters \code{T} and \code{Z} as literals. A \code{Date} format needs year,
#' month and day, a \code{POSIXct} format also hour and minute, an \code{hms}
#' format hour and minute and no date part. When \code{format} is omitted the
#' default is stored: \code{\%Y-\%m-\%d}, \code{\%Y-\%m-\%d \%H:\%M:\%S} or
#' \code{\%H:\%M:\%S}. When \code{type} is omitted it is inferred from the
#' format. \code{length}, the maximum number of characters, is only for
#' \code{character}.
#' @import S7
#' @return An object of class \code{DTAColumnSpecStructureR}.
#' @export
#'
#' @param type Character or NULL. The type of the column.
#' @param format Character or NULL. The date/time format of the column.
#' @param length Numeric or NULL. The max character length.
#' @examples
#' DTAColumnSpecStructureR(type = "Date")
#' DTAColumnSpecStructureR(type = "POSIXct", format = "%Y-%m-%dT%H:%M:%SZ")
#' DTAColumnSpecStructureR(type = "character", length = 12)
DTAColumnSpecStructureR <- S7::new_class(
  "DTAColumnSpecStructureR",
  parent = DTAColumnSpecStructure,
  constructor = function(
    type = NULL,
    format = NULL,
    length = NULL
  ) {
    type <- `__dta_normalize_r_type__`(type)
    # A format that is empty after trimming is kept as "" so the validator
    # refuses it, rather than being silently dropped.
    if (!is.null(format)) {
      format <- trimws(format)
    }

    if (is.null(type) && !is.null(format)) {
      inferred <- dta_r_format_parse(format)$family
      if (!is.na(inferred)) {
        type <- inferred
      }
    }

    if (is.null(format) && length(type) == 1L && isTRUE(type %in% `__DTAtools_r_temporal_types__`)) {
      format <- dta_r_default_format(type)
    }

    new_object(
      .parent = DTAColumnSpecStructure(
        type = type,
        format = format,
        length = length,
        backend = "R"
      )
    )
  },
  validator = function(self) {
    # backend cannot be empty character
    if (is.null(self@backend) || self@backend == "") {
      return("'backend' must be defined and cannot be an empty character.")
    }
    # A structure of this class stays an R one: with another backend its stored
    # strptime format would never be emitted or checked, silently.
    if (!identical(self@backend, "R")) {
      return("'backend' must be \"R\" for a DTAColumnSpecStructureR; build a structure of the other backend with DTAColumnSpecStructureFactory().")
    }

    supported_types <- unique(unname(`__dta_r_type_aliases__`))
    type <- self@type

    if (!is.null(type)) {
      if (length(type) != 1L || is.na(type)) {
        return(paste0("'type' must be one of: ", paste(supported_types, collapse = ", "), "."))
      }
      if (!nzchar(type)) {
        return("'type' cannot be an empty string.")
      }
      if (!(type %in% supported_types)) {
        return(paste0(
          "'type' must be one of: ", paste(supported_types, collapse = ", "), "."
        ))
      }
    }

    format <- self@format
    if (!is.null(format)) {
      if (length(format) != 1L || is.na(format) || !nzchar(trimws(format))) {
        return("'format' must be one non-empty piece of text.")
      }
      if (!is.null(type) && !(type %in% `__DTAtools_r_temporal_types__`)) {
        return(paste0(
          "'format' is only allowed for an R Date, POSIXct or hms column, not for an R ",
          type, " column."
        ))
      }
      problem <- dta_r_format_problem(format, type)
      if (!is.null(problem)) {
        # dta_r_shown(): a control character in the format is shown escaped.
        return(paste0("Unsupported R format '", dta_r_shown(format), "': ", problem))
      }
    }

    if (!is.null(self@length) && !is.null(type) && !identical(type, "character")) {
      return(paste0(
        "'length' is only allowed for an R character column, not for an R ",
        type, " column."
      ))
    }

    NULL
  }
)


#' @export
#' @name as.list
method(as.list, DTAColumnSpecStructureR) <- function(x, ...) {
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

#' @export
method(as_json_schema_type, DTAColumnSpecStructureR) <- function(x) {
  if (!.structure_value_is_set(x@type)) {
    return("string")
  }
  switch(x@type,
    "character" = "string",
    "numeric" = "number",
    "integer" = "integer",
    "logical" = "boolean",
    "Date" = "string",
    "POSIXct" = "string",
    "hms" = "string",
    "string"
  ) # fallback
}

#' @export
method(as_r_type, DTAColumnSpecStructureR) <- function(x) {
  # An unset type carries no instruction to convert anything.
  if (!.structure_value_is_set(x@type)) {
    return("character")
  }

  switch(x@type,
    "character" = "character",
    "numeric" = "double",
    "integer" = "integer",
    "logical" = "logical",
    "Date" = "character",
    "POSIXct" = "character",
    "hms" = "character",
    "character"
  ) # fallback
}


#' @name print
#' @export
if (!exists("print", mode = "function")) {
  print <- new_generic("print", "x")
}
#' @export
method(print, DTAColumnSpecStructureR) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTAColumnSpecStructureR}>")
  print_info(x)
  invisible(x)
}
