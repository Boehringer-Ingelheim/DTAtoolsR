# `values` (and `examples`) enumerate the permitted/example values of ONE
# column, which has one declared type, so the canonical representation is a
# single atomic vector rather than a list of scalars.
#
# This is what makes the pair write_columns_to_yaml() ->
# import_specs_from_yaml() return the object it was given: YAML has no
# list/vector distinction -- both write as the same sequence -- and
# yaml::read_yaml() simplifies a homogeneous sequence back to an atomic vector.
# Normalising on construction means every entry point (YAML, Word, direct call)
# produces the same representation, so whole-object equality holds.
#
# Flattening is deliberately not as.character(): a numeric code set stays
# numeric, and as_json_schema() re-coerces to the column's declared type anyway.
#
# c() rather than unlist(): unlist() drops the class attribute, so a list of
# Dates collapses to the underlying numbers and a date enum renders as "20454"
# instead of "2026-01-01". c() preserves Date and POSIXct. unlist() remains the
# fallback for a nested list, which c() would leave as a list.
#
# NOTE: this helper must stay ABOVE the DTAColumnSpec roxygen block. Placed
# between that block and the class definition, roxygen attaches the block --
# including @export -- to this internal function instead, and DTAColumnSpec
# silently stops being exported. load_all() ignores NAMESPACE, so the test
# suite cannot catch it; only R CMD check or a real install would.
#' @keywords internal
dta_normalise_spec_values <- function(values) {
  if (is.null(values) || !is.list(values)) {
    return(values)
  }

  if (length(values) == 0) {
    return(NULL)
  }

  flattened <- do.call(c, values)

  if (is.list(flattened)) {
    return(unlist(values, recursive = TRUE, use.names = FALSE))
  }

  unname(flattened)
}

#' @title DTA Column Format Class
#' @description
#' Class for column format.
#'
#' This class defines the format of a column in a DTA dataset.
#' @import S7
#' @export
#'
#' @param id Character. The id of the column.
#' @param label Character or NA. The label of the column.
#' @param type Character or NA. The type of the column.
#' @param format Character or NA. The format of the column.
#' @param length Numeric or NA. The max character length.
#' @param nullable Logical or NA. Whether the column can be null.
#' @param pattern Character or NA. The pattern of the column.
#' @param values Character or numeric vector, or NULL. The permitted values of
#'   the column. A list is accepted and flattened to a vector.
#' @param examples Character or numeric vector, or NULL. Example value(s) for
#'   the column. A list is accepted and flattened to a vector.
#' @param description Character or NA. The description of the column.
#' @param colclass Character or NA. The R/SAS storage class of the column.
#' @return An object of class DTAColumnSpec.
#' @examples
#' col_format <- DTAColumnSpec(
#'   id = "STUDYID", type = "SAS Char", nullable = FALSE, values = "1234-1234"
#' )
DTAColumnSpec <- S7::new_class(
  "DTAColumnSpec",
  constructor = function(
    id,
    label = NULL,
    type = NULL,
    format = NULL,
    length = NULL,
    nullable = NULL,
    pattern = NULL,
    values = NULL,
    examples = NULL,
    description = NULL,
    colclass = NULL
  ) {
    structure <- NULL

    values <- dta_normalise_spec_values(values)
    examples <- dta_normalise_spec_values(examples)

    if (!is.null(type) || !is.null(format) || !is.null(length)) {
      structure <- DTAColumnSpecStructureFactory(
        type = type,
        format = format,
        length = length
      )
    }

    new_object(
      S7_object(),
      id = id,
      label = label,
      structure = structure,
      nullable = nullable,
      description = description,
      values = values,
      examples = examples,
      pattern = pattern,
      colclass = colclass
    )
  },
  properties = list(
    id = class_character,
    label = class_character_or_null,
    structure = class_DTAColumnSpecStructure_or_null,
    nullable = class_logical_or_null,
    description = class_character_or_null,
    values = class_character_or_numeric_or_null_or_list,
    examples = class_character_or_numeric_or_null_or_list,
    pattern = class_character_or_null,
    colclass = class_character_or_null
  ),
  validator = function(self) {
    if (is.null(self@id) || any(grepl("\\s", self@id))) {
      cli_abort("@id cannot have whitespaces and needs to be defined.")
    }

    # if values are provided, there cannot be a pattern or examples
    if (!is.null(self@values)) {
      if (!is.null(self@pattern)) {
        cli_abort(glue::glue("{self@id}: 'pattern' cannot be set if 'values' are provided."))
      }
      if (!is.null(self@examples)) {
        cli_abort(
          glue::glue("{self@id}: 'examples' cannot be set if 'values' are provided.")
        )
      }
    }

    # if a pattern is provided, there cannot be values and examples must conform with pattern provided
    if (!is.null(self@pattern)) {
      if (!is.null(self@values)) {
        cli_abort(glue::glue("{self@id}: 'values' cannot be set if pattern is provided."))
      }
      if (!is.null(self@examples)) {
        for (ex in self@examples) {
          if (!grepl(ex, pattern = self@pattern)) {
            cli_abort(
              glue::glue(
                "{self@id}: example '{ex}' must conform to the pattern '{self@pattern}' provided."
              )
            )
          }
        }
      }
    }

    if (!is.null(self@colclass)) {
      valid_colclasses <- c(
        "patient_info",
        "measurement_patient",
        "measurement",
        "visit_related",
        "date_related",
        "study_info",
        "wide_format",
        "long_format",
        "wide_and_long_format"
      )
      if (!(self@colclass %in% valid_colclasses)) {
        cli_abort(
          glue::glue("'colclass' must be one of: {paste(valid_colclasses, collapse = ', ')}")
        )
      }
    }
  }
)


#' @title Get Arrow Type
#' @description
#' Returns the corresponding Arrow schema type for a given DTAColumnSpec
#' object based on its `type` property.
#' @importFrom glue glue
#' @param x A DTAColumnSpec object.
#' @return A character string representing the Arrow schema type.
#' @examples
#' col <- DTAColumnSpec(id = "AGE", type = "SAS Char")
#' get_arrow_type(col)
#' @export
get_arrow_type <- function(x) {
  if (!inherits(x, "DTAtools::DTAColumnSpec")) {
    stop("Input must be a DTAColumnSpec object.")
  }
  if (is.null(x@structure)) {
    stop(glue::glue("Structure is not set for {x@id}."))
  }
  type <- x@structure@type
  if (is.null(type)) {
    stop(glue::glue("Type is not set for {x@id}."))
  }
  switch(type,
    "Char" = "utf8",
    "Num" = "double",
    "Int" = "int32",
    "Bool" = "bool",
    NA_character_
  )
}


#' @title Create Example DTAColumnSpec
#' @description
#' S7 method to create and return an example DTAColumnSpec object.
#' @param index Numeric. Selector for different example objects.
#' @importFrom cli cli_abort
#' @return An example DTAColumnSpec object based on the provided index.
#' @examples
#' library(DTAtools)
#' create_example_DTAColumnSpec()
#' @export
create_example_DTAColumnSpec <- function(index = 1) {
  switch(as.character(index),
    "1" = {
      DTAtools::DTAColumnSpec(
        id = "STUDYID",
        label = "Study Identifier",
        type = "SAS Char",
        nullable = FALSE,
        values = list("1234", "5678"),
        description = "Unique study identifier"
      )
    },
    "2" = {
      DTAtools::DTAColumnSpec(
        id = "VISIT",
        label = "Visit",
        type = "SAS Char",
        nullable = FALSE,
        values = list("V01", "EOT"),
        description = "Visit code"
      )
    },
    "3" = {
      DTAtools::DTAColumnSpec(
        id = "SUBJID",
        label = "Subject Identifier",
        type = "SAS Char",
        nullable = FALSE,
        values = list("001", "002"),
        description = "Unique subject identifier"
      )
    },
    "4" = {
      DTAtools::DTAColumnSpec(
        id = "AGE",
        label = "Age",
        type = "SAS Int",
        nullable = TRUE,
        pattern = "^[0-9]{1,3}$",
        description = "Age in years"
      )
    },
    "5" = {
      DTAtools::DTAColumnSpec(
        id = "AVAL",
        label = "Analysis Value",
        type = "SAS Int",
        nullable = FALSE,
        pattern = "^[0-9]+(\\.[0-9]{1,2})?$",
        description = "Analysis value"
      )
    },
    {
      stop("Invalid index value for example DTAColumnSpec.")
    }
  )
}


#' @title Print Method for DTAColumnSpec
#' @description
#' S7 print method for DTAColumnSpec objects.
#' @param x A DTAColumnSpec object.
#' @param ... Additional arguments (ignored).
#' @importFrom cli cli_alert_info cli_alert cli_text
#' @name print
#' @export
method(print, DTAColumnSpec) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTAColumnSpec}> ")
  if (!is.null(x@label)) {
    cli_alert("id         : {.field {x@id}}")
  }
  if (!is.null(x@label)) {
    cli_alert("label      : {x@label}")
  }
  if (!is.null(x@structure)) {
    print_info(x@structure)
  }
  if (!is.null(x@nullable)) {
    cli_alert(
      "nullable   : {ifelse(x@nullable, cli::symbol$tick, cli::symbol$cross)}"
    )
  }
  if (!is.null(x@pattern)) {
    cli_alert("pattern    : {x@pattern}")
  }
  if (!is.null(x@values)) {
    cli_alert(
      "values     : {paste0(utils::capture.output(utils::str(x@values, give.attr = FALSE)), collapse = ' ')}"
    )
  }
  if (!is.null(x@examples)) {
    cli_alert(
      "examples   : {paste0(utils::capture.output(utils::str(x@examples, give.attr = FALSE)), collapse = ' ')}"
    )
  }
  if (!is.null(x@description)) {
    cli_alert("description: {x@description}")
  }
  invisible(x)
}

#' @title as.list method for DTAColumnSpec
#' @name as.list
#' @description
#' Converts a DTAColumnSpec object to a named list.
#' @param x A DTAColumnSpec object.
#' @param ... Additional arguments (ignored).
#' @return A named list with the DTAColumnSpec properties.
#' @export
method(as.list, DTAColumnSpec) <- function(x, ...) {
  x1 <- list(
    id = x@id,
    label = x@label,
    nullable = x@nullable,
    description = x@description,
    values = x@values,
    examples = x@examples,
    pattern = x@pattern,
    colclass = x@colclass
  )

  x2 <- as.list(x@structure)

  c(x1, x2)
}

#' @param x A `DTAColumnSpec` object.
#' @name as_json_schema_type
#' @title as_json_schema_type
#' @description
#' Converts a DTAColumnSpec to a JSON Schema type.
#' @export
if (!exists("as_json_schema_type", mode = "function")) {
  as_json_schema_type <- new_generic("as_json_schema_type", "x")
}
#' @export
method(as_json_schema_type, DTAColumnSpec) <- function(x) {
  type <- as_json_schema_type(x@structure)

  if (!is.null(x@nullable) && x@nullable) {
    type <- c(type, "null")
  }

  type
}

#' @param x A `DTAColumnSpec` object.
#' @name as_json_schema_length
#' @rdname as_json_schema_length-DTAColumnSpec
#' @title as_json_schema_length
#' @description
#' Converts a DTAColumnSpec to a JSON Schema length.
#' @export
if (!exists("as_json_schema_length", mode = "function")) {
  as_json_schema_length <- new_generic("as_json_schema_length", "x")
}
#' @export
method(as_json_schema_length, DTAColumnSpec) <- function(x) {
  x@structure@length
}


#' @param x A `DTAColumnSpec` object.
#' @name as_json_schema
#' @title as_json_schema
#' @description
#' Converts a DTAColumnSpecStructure to a JSON Schema.
#' @export
if (!exists("as_json_schema", mode = "function")) {
  as_json_schema <- new_generic("as_json_schema", "x")
}
#' @export
method(as_json_schema, DTAColumnSpec) <- function(x) {
  schema <- list()

  schema$type <- as_json_schema_type(x)

  schema$maxLength <- as_json_schema_length(x)

  if (!is.null(x@values)) {
    values_raw <- x@values
    values_flat <- if (is.list(values_raw)) {
      unlist(values_raw, recursive = TRUE, use.names = FALSE)
    } else {
      values_raw
    }

    schema_types <- schema$type
    base_type <- schema_types[schema_types != "null"][1]

    values <- switch(base_type,
      "integer" = as.integer(values_flat),
      "number" = as.numeric(values_flat),
      "boolean" = as.logical(values_flat),
      "string" = {
        if (is.list(values_raw)) {
          unlist(lapply(values_raw, as.character), recursive = TRUE, use.names = FALSE)
        } else {
          as.character(values_flat)
        }
      },
      values_flat
    )

    if (!is.null(x@nullable)) {
      if (x@nullable) {
        if ("string" %in% schema$type) {
          if ("" %in% values) {
            values <- c(values, NA)
          } else {
            values <- c(values, NA, "")
          }
        } else if ("number" %in% schema$type) {
          values <- c(values, NA)
        } else if ("integer" %in% schema$type) {
          values <- c(values, NA_integer_)
        }
      }
    }

    if (length(values) > 1) {
      schema$enum <- values
    } else {
      schema$const <- values[[1]]
    }
  }

  if (!is.null(x@pattern) && !is.na(x@pattern)) {
    schema$pattern <- x@pattern
  }

  return(schema)
}
