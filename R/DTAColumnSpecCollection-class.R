#' @title DTAColumnSpecCollection Class
#' @description
#' This class represents a collection of DTAColumnSpec objects with optional metadata.
#' @import S7
#' @importFrom cli cli_abort
#' @export
#'
#' @param columns A named list of DTAColumnSpec objects. The names should correspond to the ids of the columns
#' @param metadata A list of metadata information.
#' @param rules A list of DTARule object.
#' @return An object of class DTAColumnSpecCollection.
#' @examples
#' col1 <- DTAColumnSpec(id = "STUDYID", type = "Char", nullable = TRUE)
#' col2 <- DTAColumnSpec(id = "VISIT", type = "Char", nullable = FALSE)
#' collection <- DTAColumnSpecCollection(columns = list(STUDIYID = col1, VISIT = col2))
DTAColumnSpecCollection <- new_class(
  "DTAColumnSpecCollection",
  constructor = function(columns, metadata = list(), rules = list()) {
    # Ensure columns is a list of DTAColumnSpec objects
    if (!all(sapply(columns, inherits, "DTAtools::DTAColumnSpec"))) {
      cli::cli_abort(
        "All elements in 'columns' must be of class 'DTAColumnSpec'"
      )
    }

    if (length(rules) > 0) {
      if (!all(sapply(rules, inherits, "DTAtools::DTARule"))) {
        cli::cli_abort(
          "All elements in 'rules' must be of class 'DTARule'"
        )
      }
    }

    # Transform to column specs schema to jsonschema
    json_schema <- specs_to_jsonschema(columns)

    # Validate json_schema
    invisible(jsonvalidate::json_schema$new(json_schema))
    cli::cli_alert_success("Column spec schema is correctly structured.")

    new_object(
      S7_object(),
      columns = columns,
      json_schema = json_schema,
      metadata = metadata,
      rules = rules
    )
  },
  properties = list(
    columns = class_list,
    json_schema = class_character,
    metadata = class_list,
    rules = class_any
  )
)

#' @title Get Names Method
#' @description
#' Method to get the names of columns in the collection.
#' @param x An object of class DTAColumnSpecCollection.
#' @return A character vector of column names.
#' @examples
#' names <- getColumnIds(collection)
#' @name getColumnIds-DTAColumnSpecCollection
#' @export
getColumnIds <- new_generic("getColumnIds", "x")
method(getColumnIds, DTAColumnSpecCollection) <- function(x) {
  return(names(x@columns))
}

#' @title Get Column by ID Method
#' @description
#' Method to get a column format by its ID from the collection.
#' @param x An object of class DTAColumnSpecCollection.
#' @param id Character. The ID of the column to retrieve.
#' @return A DTAColumnSpec object corresponding to the specified ID.
#' @examples
#' column_format <- getColumn(collection, "STUDYID")
#' @name getColumn-DTAColumnSpecCollection
if (!exists("getColumn", mode = "function")) {
  getColumn <- new_generic("getColumn", "x")
}
method(getColumn, DTAColumnSpecCollection) <- function(x, id) {
  return(x@columns[[id]])
}

#' @title Get Metadata
#' @description
#' Method to get Metadata from DTAColumnSpecCollection
#' @param x An object of class DTAColumnSpecCollection.
#' @return A list with metadata information
#' @examples
#' getMetadata(collection)
#' @name getMetadata-DTAColumnSpecCollection
if (!exists("getMetadata", mode = "function")) {
  getMetadata <- new_generic("getMetadata", "x")
}
method(getMetadata, DTAColumnSpecCollection) <- function(x) {
  return(x@metadata)
}

#' @title Get Rules
#' @description
#' Method to get Rules from DTAColumnSpecCollection
#' @param x An object of class DTAColumnSpecCollection.
#' @return A list with rules defined
#' @examples
#' getRules(collection)
#' @name getRules-DTAColumnSpecCollection
if (!exists("getRules", mode = "function")) {
  getRules <- new_generic("getRules", "x")
}
method(getRules, DTAColumnSpecCollection) <- function(x) {
  return(x@rules)
}

#' @title Create DTAColumnSpecCollection from YAML File
#' @description
#' This function parses a YAML file to extract column specifications and create a DTAColumnSpecCollection object.
#' @importFrom yaml read_yaml
#' @export
#'
#' @param file Character. Path to the YAML file containing specifications.
#' @return An object of class DTAColumnSpecCollection.
#' @examples
#' \dontrun{
#' # Sample YAML file content
#' yaml_content <- "
#' columns:
#'   - id: STUDYID
#'     label: Study Identifier
#'     type: Char
#'     nullable: false
#'     values: '1234'
#'   - id: VISIT
#'     label: Visit
#'     type: Char
#'     nullable: true
#'     values:
#'       - 'V03'
#'       - 'EOT'
#'       - 'V05'
#' "
#'
#' # Write the YAML content to a file
#' yaml_file <- tempfile(fileext = ".yaml")
#' writeLines(yaml_content, yaml_file)
#'
#' # Create the DTAColumnSpecCollection object
#' DTAColumnSpecCollection <- importDTAColumnSpecCollectionFromYaml(yaml_file)
#' }
importDTAColumnSpecCollectionFromYaml <- function(file) {
  yaml <- yaml::read_yaml(file)
  specs <- yaml$columns
  metadata <- ifelse(is.null(yaml$metadata), list(), yaml$metadata)
  if (is.null(yaml$rules)) {
    rules <- list()
  } else {
    rules <- yaml$rules
  }

  if (length(rules) > 0) {
    rules <- lapply(rules, function(x) {
      DTARule(
        id = x$id,
        type = x$type,
        column = x$column,
        range = x$range,
        condition = x$condition,
        then = x$then
      )
    })
  }

  column_list <- list()
  for (column in specs) {
    dta_column <- DTAColumnSpec(
      id = column$id,
      label = column$label,
      type = column$type,
      format = column$format,
      length = column$length,
      nullable = column$nullable,
      values = column$values,
      pattern = column$pattern,
      description = column$description
    )
    column_list <- append(column_list, list(dta_column))
  }
  names(column_list) <- sapply(specs, function(x) {
    return(x$id)
  })
  return(DTAColumnSpecCollection(
    columns = column_list,
    metadata = metadata,
    rules = rules
  ))
}

#' @title Create DTAColumnSpecCollection from Components
#' @description
#' Constructs a DTAColumnSpecCollection object from separate components: columns, metadata, and schema rules.
#' Supports both named and unnamed lists of column specifications.
#'
#' @importFrom cli cli_abort
#' @param columns A list of column specification lists. Each must contain at least an `id`.
#' @param metadata Optional list of metadata fields (e.g., author, version).
#' @param rules Optional list of schema rules.
#'
#' @return An object of class DTAColumnSpecCollection.
#' @export
#'
#' @examples
#' columns <- list(
#'   list(id = "STUDYID", label = "Study ID", type = "Char", nullable = FALSE, values = list("1234-5678")),
#'   list(id = "VISIT", label = "Visit", type = "Char", nullable = TRUE, values = list("V01", "EOT"))
#' )
#' dta_spec <- DTAColumnSpecCollectionFromList(columns)

DTAColumnSpecCollectionFromList <- function(
  columns,
  metadata = list(),
  rules = list()
) {
  if (!is.list(columns)) {
    cli::cli_abort("`columns` must be a list of column specifications.")
  }
  if (!is.list(rules)) {
    cli::cli_abort("`rules` must be a list of rule specifications.")
  }

  if (length(rules) > 0) {
    rules <- lapply(rules, function(x) {
      DTARule(
        id = x$id,
        type = x$type,
        column = x$column,
        range = x$range,
        condition = x$condition,
        then = x$then
      )
    })
  }

  column_list <- list()
  for (column in columns) {
    if (is.null(column$id)) {
      cli::cli_abort("Each column specification must include an `id` field.")
    }

    dta_column <- DTAColumnSpec(
      id = column$id,
      label = column$label,
      type = column$type,
      format = column$format,
      length = column$length,
      nullable = column$nullable,
      values = column$values,
      pattern = column$pattern,
      description = column$description
    )

    column_list <- append(column_list, list(dta_column))
  }

  # Assign names to the list based on column IDs if not already named
  if (is.null(names(column_list)) || any(names(column_list) == "")) {
    names(column_list) <- sapply(column_list, function(x) x@id)
  }

  return(DTAColumnSpecCollection(
    columns = column_list,
    metadata = metadata,
    rules = rules
  ))
}


#' @title Convert YAML Spec File to JSON
#' @description
#' Converts a YAML specification file to a JSON file.
#' @importFrom yaml read_yaml
#' @importFrom jsonlite write_json
#' @export
#' @param yaml_file Character. Path to the YAML file.
#' @param json_file Character. Path to the output JSON file.
#' @param pretty Logical. Whether to pretty-print the JSON. Default is TRUE.
#' @return NULL. Writes JSON to file.
convertYamlToJson <- function(yaml_file, json_file, pretty = TRUE) {
  yaml_content <- yaml::read_yaml(yaml_file)
  jsonlite::write_json(
    yaml_content,
    path = json_file,
    pretty = pretty,
    auto_unbox = TRUE
  )
}

#' @title Create DTAColumnSpecCollection from JSON File
#' @description
#' Parses a JSON file to extract column specifications and create a DTAColumnSpecCollection object.
#' @importFrom jsonlite fromJSON
#' @export
#' @param file Character. Path to the JSON file containing specifications.
#' @return A DTAColumnSpecCollection object.
importDTAColumnSpecCollectionFromJson <- function(file) {
  json <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  specs <- json$columns
  metadata <- if (is.null(json$metadata)) list() else json$metadata

  column_list <- lapply(specs, function(column) {
    DTAColumnSpec(
      id = column$id,
      label = column$label,
      type = column$type,
      format = column$format,
      length = column$format,
      nullable = column$nullable,
      values = column$values,
      pattern = column$pattern,
      description = column$description
    )
  })
  names(column_list) <- vapply(specs, function(x) x$id, character(1))
  DTAColumnSpecCollection(columns = column_list, metadata = metadata)
}

#' @title Create DTAColumnSpecCollection from DTA Word Document
#' @description
#' Parses a DTA Word document to extract column specifications and create a DTAColumnSpecCollection object.
#' @importFrom docxtractr read_docx docx_extract_all_tbls
#' @importFrom purrr map set_names
#' @importFrom dplyr mutate
#' @export
#' @param file Character. Path to the Word document.
#' @param table_position Integer. Index of the table to extract.
#' @param colnames Vector. Vector containing column names of the table. Essential column names are: id Variable Name), label (Variable Label), type (Type), nullable (Nullable), description (Description)
#' @param value_sep Character. Separator dividing the values. Default: ";"
#' @return A DTAColumnSpecCollection object.
importDTAColumnSpecCollectionFromDTA <- function(
  file,
  table_position = 1,
  colnames = c(
    "id",
    "label",
    "type",
    "length",
    "format",
    "nullable",
    "description"
  ),
  value_sep = ";"
) {
  doc <- docxtractr::read_docx(file)
  specs <- docxtractr::docx_extract_all_tbls(doc, preserve = TRUE)[[
    table_position
  ]]
  colnames(specs) <- colnames

  specs <- specs %>%
    dplyr::mutate(
      nullable = grepl("Yes|yes", nullable),
      values = purrr::map(description, function(desc) {
        if (grepl("#@values:", desc)) {
          # extract and clean values line
          value_line <- sub(".*#@values:", "", desc)
          value_line <- sub("\n.*", "", value_line)
          values <- unlist(strsplit(value_line, value_sep))
          values <- trimws(values)
          values <- gsub("^\"|^\'|\"$|\'$", "", values)
          return(values)
        }
        NULL
      }),
      pattern = purrr::map_chr(description, function(desc) {
        if (grepl("#@pattern:", desc)) {
          # extract and clean pattern line
          pattern <- sub(".*#@pattern:\\s*([^\\n]+).*", "\\1", desc)
          pattern <- trimws(pattern)
          pattern <- gsub("^\"|^\'|\"$|\'$", "", pattern)
          return(pattern)
        } else {
          NA
        }
      }),
      clean_description = trimws(gsub(
        "#@values:.*|#@pattern:.*",
        "",
        description
      ))
    ) %>%
    # remove whitespaces in id - not allowed
    mutate(id = gsub("\\s+", "", id)) %>%
    # remove empty ids - empty rows
    filter(!is.na(id)) %>%
    filter(id != "")

  column_list <- purrr::map(1:nrow(specs), function(i) {
    row <- specs[i, ]
    if (is.na(row$pattern)) {
      pattern <- NULL
    } else {
      pattern <- row$pattern
    }
    if (all(is.na(row$values[[1]]))) {
      values <- NULL
    } else {
      values <- row$values[[1]]
    }
    DTAColumnSpec(
      id = row$id,
      label = row$label,
      type = row$type,
      format = row$format,
      length = as.numeric(row$length),
      nullable = row$nullable,
      values = values,
      pattern = pattern,
      description = row$clean_description
    )
  }) %>%
    purrr::set_names(specs$id)

  DTAColumnSpecCollection(columns = column_list)
}


#' @title Write DTAColumnSpecCollection to YAML File
#' @description
#' This function writes a DTAColumnSpecCollection object to a YAML file.
#' @importFrom yaml write_yaml
#' @export
#'
#' @param DTAColumnSpecCollection An object of class DTAColumnSpecCollection.
#' @param file Character. Path to the YAML file to write the specifications to.
#' @return NULL. The function writes the DTAColumnSpecCollection to a YAML file.
#' @examples
#' \dontrun{
#' # Create a DTAColumnSpecCollection object
#' DTAColumnSpecCollection <- importDTAColumnSpecCollectionFromYaml("path/to/yaml/file.yaml")
#'
#' # Write the DTAColumnSpecCollection object to a YAML file
#' writeDTAColumnSpecCollectionToYaml(DTAColumnSpecCollection, "path/to/output/file.yaml")
#' }
writeDTAColumnSpecCollectionToYaml <- function(
  DTAColumnSpecCollection,
  file
) {
  specs <- lapply(DTAColumnSpecCollection@columns, function(column) {
    list(
      id = column@id,
      label = column@label,
      type = column@type,
      format = column@format,
      length = column@length,
      nullable = column@nullable,
      values = column@values,
      pattern = column@pattern,
      description = column@description
    )
  })

  rules <- lapply(DTAColumnSpecCollection@rules, function(rule) {
    list(
      id = rule@id,
      type = rule@type,
      condition = rule@condition,
      then = rule@then,
      column = rule@column,
      range = rule@range
    )
  })

  metadata <- DTAColumnSpecCollection@metadata
  yaml::write_yaml(
    list(metadata = metadata, columns = specs, rules = rules),
    file
  )
}

#' @title Write DTAColumnSpecCollection to JSON File
#' @description
#' This function writes a DTAColumnSpecCollection object to a JSON file.
#' @importFrom jsonlite write_json
#' @export
#'
#' @param DTAColumnSpecCollection An object of class DTAColumnSpecCollection.
#' @param file Character. Path to the JSON file to write the specifications to.
#' @param pretty Logical. Whether to pretty-print the JSON. Default is TRUE.
#' @return NULL. The function writes the DTAColumnSpecCollection to a JSON file.
#' @examples
#' \dontrun{
#' # Create a DTAColumnSpecCollection object
#' DTAColumnSpecCollection <- importDTAColumnSpecCollectionFromYaml("path/to/yaml/file.yaml")
#'
#' # Write the DTAColumnSpecCollection object to a JSON file
#' writeDTAColumnSpecCollectionToJson(DTAColumnSpecCollection, "path/to/output/file.json")
#' }
writeDTAColumnSpecCollectionToJson <- function(
  DTAColumnSpecCollection,
  file,
  pretty = TRUE
) {
  specs <- lapply(DTAColumnSpecCollection@columns, function(column) {
    list(
      id = column@id,
      label = column@label,
      type = column@type,
      format = column@format,
      length = column@length,
      nullable = column@nullable,
      values = column@values,
      pattern = column@pattern,
      description = column@description
    )
  })
  metadata <- DTAColumnSpecCollection@metadata
  rules <- DTAColumnSpecCollection@rules
  jsonlite::write_json(
    list(metadata = metadata, columns = specs, rules = rules),
    path = file,
    pretty = pretty,
    auto_unbox = TRUE
  )
}

#' @title Convert DTAColumnSpec s to JSON Schema
#' @description Converts a DTAColumnSpec s into a JSON Schema.
#' @param columns A list containing column spec information
#' @return A list representing the JSON Schema.
specs_to_jsonschema <- function(specs) {
  properties <- lapply(specs, function(spec) {
    type <- switch(
      spec@type,
      "Char" = "string",
      "Num" = "number",
      "Int" = "integer",
      "Bool" = "boolean",
      "string"
    ) # fallback

    schema <- list(type = type)

    if (!is.null(spec@nullable)) {
      if (spec@nullable) {
        schema$type <- c(type, "null")
      }
    }

    if (!is.null(spec@length) && !is.na(spec@length)) {
      schema$maxLength <- as.integer(spec@length)
    }

    if (!is.null(spec@values)) {
      values <- if (is.list(spec@values)) unlist(spec@values) else spec@values

      if (!is.null(spec@nullable)) {
        if (spec@nullable) {
          if (type == "string") {
            if ("" %in% values) {
              values <- c(values, NA)
            } else {
              values <- c(values, NA, "")
            }
          } else if (type == "number") {
            values <- c(values, NA)
          }
        }
        if (length(values) > 1) {
          schema$enum <- as.character(values)
        } else {
          schema$const <- as.character(values)
        }
      }
    }

    if (!is.null(spec@pattern) && !is.na(spec@pattern)) {
      schema$pattern <- spec@pattern
    }

    return(schema)
  })

  names(properties) <- names(specs)

  if (length(names(specs)) == 1) {
    required <- list(names(specs))
  } else {
    required <- names(specs)
  }

  schema <- list(
    type = "array",
    items = list(properties = properties, required = required)
  )

  json_schema <- jsonlite::toJSON(
    schema,
    auto_unbox = TRUE,
    na = "null"
  )

  return(json_schema)
}

#' @title Transform DTAColumnSpecCollection to List
#' @description
#' This function transforms a DTAColumnSpecCollection object to a list.
#' @export
#'
#' @param DTAColumnSpecCollection An object of class DTAColumnSpecCollection.
#' @return List.
#' @examples
#' \dontrun{
#' # Create a DTAColumnSpecCollection object
#' DTAColumnSpecCollection <- importDTAColumnSpecCollectionFromYaml("path/to/yaml/file.yaml")
#'
#' # Write the DTAColumnSpecCollection object to a YAML file
#' DTAColumnSpecCollectionToList(DTAColumnSpecCollection)
#' }
DTAColumnSpecCollectionToList <- function(
  DTAColumnSpecCollection
) {
  specs <- lapply(DTAColumnSpecCollection@columns, function(column) {
    list(
      id = column@id,
      label = column@label,
      type = column@type,
      format = column@format,
      length = column@length,
      nullable = column@nullable,
      values = column@values,
      pattern = column@pattern,
      description = column@description
    )
  })

  rules <- lapply(DTAColumnSpecCollection@rules, function(rule) {
    list(
      id = rule@id,
      type = rule@type,
      condition = rule@condition,
      then = rule@then,
      column = rule@column,
      range = rule@range
    )
  })

  metadata <- getMetadata(DTAColumnSpecCollection)

  return(list(columns = specs, rules = rules, metadata = metadata))
}
