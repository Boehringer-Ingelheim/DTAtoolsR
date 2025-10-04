#' @title DTAColumnSpecCollection Class
#' @description
#' This class represents a collection of DTAColumnSpec objects with optional template.
#' @import S7
#' @importFrom cli cli_abort
#'
#' @param columns A named list of DTAColumnSpec objects. The names should correspond to the ids of the columns
#' @param template A list of template information.
#' @param rules A list of DTARule object.
#' @return An object of class DTAColumnSpecCollection.
#' @examples
#' col1 <- DTAColumnSpec(id = "STUDYID", type = "Char", nullable = TRUE)
#' col2 <- DTAColumnSpec(id = "VISIT", type = "Char", nullable = FALSE)
#' collection <- DTAColumnSpecCollection(columns = list(STUDIYID = col1, VISIT = col2))
#' @export
DTAColumnSpecCollection <- new_class(
  "DTAColumnSpecCollection",
  constructor = function(
      columns,
      rules = list()) {
    
    if (!is.list(columns)) {
      cli::cli_abort("'columns' must be a list.")
    }

    if (!all(sapply(columns, inherits, "DTAtools::DTAColumnSpec"))) {
      cli::cli_abort(
        "All elements in 'columns' must be of class 'DTAColumnSpec'"
      )
    }

    new_object(
      S7_object(),
      columns = columns,
      rules = rules
    )
  },
  properties = list(
    columns = class_list,
    rules = class_any
  ),
  validator = function(self) {
    # Ensure columns is a list of DTAColumnSpec objects
    if (!all(sapply(self@columns, inherits, "DTAtools::DTAColumnSpec"))) {
      "All elements in 'columns' must be of class 'DTAColumnSpec'"
    }

    if (length(rules) > 0) {
      if (!all(sapply(self@rules, inherits, "DTAtools::DTARule"))) {
        "All elements in 'rules' must be of class 'DTARule'"
      }
    }

    if (is.null(names(self@columns))) {
      names(self@columns) <- columns_names
    } 
    
    columns_names <- sapply(columns, function(col) col@id)

    if (!all(names(self@columns) == columns_names)) {
      cli_abort(
        "Names of 'columns' must match the 'id' of each DTAColumnSpec"
      )
    }
  }
)


#' @title Preview Column IDs in a DTAColumnSpecCollection
#'
#' @description
#' Provides a preview of the column IDs contained within a \code{DTAColumnSpecCollection} object.
#' If the collection contains more than 5 columns, it returns the IDs of the first four columns,
#' an ellipsis, and the last column's ID. If there are 5 or fewer columns, it returns all column IDs.
#'
#' @param x A \code{DTAColumnSpecCollection} object.
#'
#' @return A character string representing a preview of the column IDs.
#'
#' @seealso \code{\link{DTAColumnSpecCollection}}
#' @examples
#' library(DTAtools)
#' x <- create_example_DTAColumnSpecCollection()
#' column_preview(x)
#' @export
column_preview <- new_generic("column_preview", "x")
method(column_preview, DTAColumnSpecCollection) <- function(x) {
  if (length(x@columns) > 5) {
    col_preview <- str_flatten_comma(
        c(unlist(map(x@columns[1:4], function(y) y@id)),
        "...",
        x@columns[[length(x@columns)]]@id)
    )

  } else if (length(x@columns) <= 5) {
    col_preview <- str_flatten_comma(map(x@columns, function(y) y@id))
  } else {
    col_preview <- "not set"
  }

  col_preview
}


#' @title Preview Rules in a DTAColumnSpecCollection
#'
#' @description
#' Provides a preview of the rules contained within a \code{DTAColumnSpecCollection} object.
#' If the collection contains more than 5 rules, it returns the IDs of the first four rules,
#' an ellipsis, and the rules ID. If there are 5 or fewer rules, it returns all rules.
#'
#' @param x A \code{DTAColumnSpecCollection} object.
#'
#' @return A character string representing a preview of the rules
#'
#' @seealso \code{\link{DTAColumnSpecCollection}}
#' @examples
#' library(DTAtools)
#' x <- create_example_DTAColumnSpecCollection()
#' rule_preview(x)
#' @export
rule_preview <- new_generic("rule_preview", "x")
method(rule_preview, DTAColumnSpecCollection) <- function(x) {
  if (length(x@rules) > 5) {
    rule_preview <- str_flatten_comma(
      c(map(x@rules[1:4], function(y) y@id),
      "...",
      x@rules[[length(x@rules)]]@id)
    )

  } else if (length(x@rules) <= 5) {
    rule_preview <- str_flatten_comma(map(x@rules, function(y) y@id))
  }  else {
    rule_preview <- "not set"
  }

  rule_preview
}

#' @title print
#' @description
#' Print overview for DTAColumnSpecCollection
#' @param x An object of class DTAColumnSpecCollection.
#' @importFrom stringr str_c str_flatten_comma
#' @importFrom cli cli_alert_info cli_alert cli_text cli_div
#' @importFrom purrr map
#' @examples
#' \dontrun{
#'  print(columns)
#' }
#' @name print
#' @export
method(print, DTAColumnSpecCollection) <- function(x) {
  col_preview <- column_preview(x)
  rule_preview <- rule_preview(x)

  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTAColumnSpecCollection}>")

  cli_alert_info("columns ({length(x@columns)}): {col_preview}")

  if (length(x@rules) > 0) {
    cli_alert_info("rules ({length(x@rules)}): {rule_preview}")
  } else {
    cli_alert("rules: {cli::symbol$cross}")
  }
}


#' @title Get Names Method
#' @description
#' Method to get the names of columns in the collection.
#' @param x An object of class DTAColumnSpecCollection.
#' @param ... void
#' @return A character vector of column names.
#' @examples
#' \dontrun{
#' names <- specids(collection)
#' }
#' @name names
#' @rdname names-DTAColumnSpecCollection
#' @export
if(!exists("names", mode="function")) {
  names <- new_generic("names", "x")
}
method(names, DTAColumnSpecCollection) <- function(x) {
  return(sapply(x@columns, function(col)  col@id))
}

#' @title Get Column by ID Method
#' @description
#' Method to get a column format by its ID from the collection.
#' @param x An object of class DTAColumnSpecCollection.
#' @param id Character. The ID of the column to retrieve.
#' @return A DTAColumnSpec object corresponding to the specified ID.
#' @examples
#' \dontrun{
#'  column_format <- colspec(collection, "STUDYID")
#' }
#' @name colspec
#' @rdname colspec-DTAColumnSpecCollection
#' @export
# colspec <- new_generic("colspec", "x", function(x, ...) {
#   S7_dispatch()
# })
colspec <- new_generic("colspec", "x")

#' @export
method(colspec, DTAColumnSpecCollection) <- function(x, id) {
  return(x@columns[[id]])
}


#' @title Get Rules
#' @description
#' Method to get Rules from DTAColumnSpecCollection
#' @param x An object of class DTAColumnSpecCollection.
#' @return A list with rules defined
#' @examples
#' \dontrun{
#'  rules(collection)
#' }
#' @name rules
#' @export
rules <- new_generic("rules", "x")

method(rules, DTAColumnSpecCollection) <- function(x) {
  return(x@rules)
}

#' @title Create DTAColumnSpecCollection from YAML File
#' @description
#' This function parses a YAML file to extract column specifications and create a DTAColumnSpecCollection object.
#' @importFrom yaml read_yaml
#' @importFrom cli cli_abort
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
#'     type: SAS Char
#'     nullable: false
#'     values: '1234'
#'   - id: VISIT
#'     label: Visit
#'     type: SAS Char
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
#' DTAColumnSpecCollection <- import_specs_from_yaml(yaml_file)
#' }
import_specs_from_yaml <- function(file) {
  yaml <- read_yaml(file)

  if (is.null(yaml$columns)) {
    cli_abort("YAML file must contain a 'columns' section.")
  }

  columns <- yaml$columns

  if (is.null(yaml$rules)) {
    rules <- list()
    cli_alert("No 'rules' section found in YAML. Proceeding without rules.")
  } else {
    rules <- yaml$rules
  }

  return(specs_from_list(columns, rules))
}

#' @title Create DTAColumnSpecCollection from Components
#' @description
#' Constructs a DTAColumnSpecCollection object from separate components: columns and schema rules.
#' Supports both named and unnamed lists of column specifications.
#'
#' @importFrom cli cli_abort
#' @param columns A list of column specification lists. Each must contain at least an `id`.
#' @param rules Optional list of schema rules.
#'
#' @return An object of class DTAColumnSpecCollection.
#' @export
#'
#' @examples
#' library(DTAtools)
#' @examples
#' # Load example YAML file from package extdata
#' yaml_file <- system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
#' input_list <- yaml::read_yaml(yaml_file)
#' specs <- specs_from_list(input_list$columns, input_list$rules)
#' 
#' 
specs_from_list <- function(
  columns,
  rules = list()
) {
  if (!is.list(columns)) {
    cli_abort("`columns` must be a list.")
  }
  if (!is.null(rules) && !is.list(rules)) {
    cli_abort("`rules` must be a list or null.")
  }

  if (length(rules) > 0) {
    dta_rules <- lapply(rules, function(x) {
      do.call(DTARuleFactory, x)
    })
  }

  if (length(columns) > 0) {
    dta_columns <- lapply(columns, function(x) {
      do.call(DTAColumnSpec, x)
    })
  }

  return(DTAColumnSpecCollection(
    columns = dta_columns,
    rules = dta_rules
  ))
}


# #' @title Convert YAML Spec File to JSON
# #' @description
# #' Converts a YAML specification file to a JSON file.
# #' @importFrom yaml read_yaml
# #' @importFrom jsonlite write_json
# #' @export
# #' @param yaml_file Character. Path to the YAML file.
# #' @param json_file Character. Path to the output JSON file.
# #' @param pretty Logical. Whether to pretty-print the JSON. Default is TRUE.
# #' @examples
# #' \dontrun{
# #'   convert_yaml_to_json(yaml_file, json_file)
# #' }
# #' @return NULL. Writes JSON to file.
# convert_yaml_to_json <- function(yaml_file, json_file, pretty = TRUE) {
#   yaml_content <- yaml::read_yaml(yaml_file)
#   jsonlite::write_json(
#     yaml_content,
#     path = json_file,
#     pretty = pretty,
#     auto_unbox = TRUE
#   )
# }

# #' @title Create DTAColumnSpecCollection from JSON File
# #' @description
# #' Parses a JSON file to extract column specifications and create a DTAColumnSpecCollection object.
# #' @importFrom jsonlite fromJSON
# #' @importFrom cli cli_abort
# #' @export
# #' @param file Character. Path to the JSON file containing specifications.
# #' @examples
# #' \dontrun{
# #'   import_specs_from_json(file)
# #' }
# #'
# #' @return A DTAColumnSpecCollection object.
# import_specs_from_json <- function(file) {
#   json <- jsonlite::fromJSON(file, simplifyVector = FALSE)
#   columns <- json$columns
#   rules <- json$rules
#   if (!is.list(columns)) {
#     cli_abort("`columns` must be a list.")
#   }
#   if (!is.null && !is.list(rules)) {
#     cli_abort("`rules` must be a list or null.")
#   }
#   specs_from_list(columns, rules)
#   # TODO check this function, was heavily modified
# }

#' @title Create DTAColumnSpecCollection from DTA Word Document
#' @description
#' Parses a DTA Word document to extract column specifications and create a DTAColumnSpecCollection object.
#' @importFrom docxtractr read_docx docx_extract_all_tbls
#' @importFrom purrr map set_names
#' @importFrom dplyr mutate
#' @export
#' @examples
#' \dontrun{
#'   import_columns_from_word(file)
#' }
#' @param file Character. Path to the Word document.
#' @param table_position Integer. Index of the table to extract.
#' @param colnames Vector. Vector containing column names of the table. Essential column names are: id Variable Name), label (Variable Label), type (Type), nullable (Nullable), description (Description)
#' @param value_sep Character. Separator dividing the values. Default: ";"
#' @return A DTAColumnSpecCollection object.
columns_specs_from_word <- function(
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
  columns <- docxtractr::docx_extract_all_tbls(doc, preserve = TRUE)[[
    table_position
  ]]
  colnames(columns) <- colnames

  columns <- columns %>%
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

  column_list <- purrr::map(1:nrow(columns), function(i) {
    row <- columns[i, ]
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
    set_names(columns$id)

  DTAColumnSpecCollection(columns = column_list)
}


#' @title Convert DTAColumnSpec s to JSON Schema
#' @description Converts a DTAColumnSpec s into a JSON Schema.
#' @param columns Column spec information
#' @name to_json_schema
#' @rdname to_json_schema-DTAColumnSpecCollection
#' @return A list representing the JSON Schema.
#' @examples
#' library(DTAtools)
#' specs <- create_example_DTAColumnSpecCollection()
#' to_json_schema(specs)
#' @export
if(!exists("to_json_schema", mode="function")) {
  to_json_schema <- new_generic("to_json_schema", "DTAColumnSpecCollection")
}
#' @export
method(to_json_schema, DTAColumnSpecCollection) <- function(x) {

  properties <- lapply(x@columns, to_json_schema_type)

  names(properties) <- names(x)

  if (length(names(x)) == 1) {
    required <- list(names(x))
  } else {
    required <- names(x)
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

#' @title as.list for DTAColumnSpecCollection
#' @description
#' This function transforms a DTAColumnSpecCollection object to a list.
#' @export
#'
#' @param x An object of class DTAColumnSpecCollection.
#' @param ... Additional arguments (not used)
#' @return A list representation of the DTAColumnSpecCollection object.
#' @examples
#' \dontrun{
#' # Create a DTAColumnSpecCollection object
#' x <- import_columns_from_yaml("path/to/yaml/file.yaml")
#'
#' # Write the DTAColumnSpecCollection object to a YAML file
#' as.list(x)
#' }
as.list.DTAColumnSpecCollection <- function(x, ...) {
  columns <- lapply(x@columns, function(column) {
    as.list(column)
  })

  rules <- as.list(x@rules)

  return(list(columns = columns, rules = rules))
}


#' @title Create Example DTAColumnSpecCollection
#' @description
#' S7 method to create and return an example DTAColumnSpecCollection object.
#' @importFrom cli cli_abort
#' @param index Integer. Index of the example to create.
#'
#' @return An example DTAColumnSpecCollection object.
#' @examples
#' library(DTAtools)
#' create_example_DTAColumnSpecCollection()
#' @export
create_example_DTAColumnSpecCollection <- function(index = 1) {
  col1 <- DTAtools::create_example_DTAColumnSpec(1)
  col2 <- DTAtools::create_example_DTAColumnSpec(2)
  col3 <- DTAtools::create_example_DTAColumnSpec(3)
  col4 <- DTAtools::create_example_DTAColumnSpec(4)
  col5 <- DTAtools::create_example_DTAColumnSpec(5)
  
  switch(index,
    `1` = {
      example_rules <- list()
      DTAColumnSpecCollection(
        columns = setNames(list(col1, col2, col3, col4), c(col1@id, col2@id, col3@id, col4@id)),
        rules = example_rules
      )
    },
    `2` = {
      example_rules <- list()
      DTAColumnSpecCollection(
        columns = setNames(list(col1, col2, col3, col5), c(col1@id, col2@id, col3@id, col5@id)),
        rules = example_rules
      )
    },
    cli::cli_abort("No example available for the provided index.")
  )

}



#' @title Write DTAColumnSpecCollection to YAML File
#' @description
#' This function writes a DTAColumnSpecCollection object to a YAML file.
#' @importFrom yaml write_yaml
#' @export
#'
#' @param columns An object of class DTAColumnSpecCollection.
#' @param file Character. Path to the YAML file to write the specifications to.
#' @return NULL. The function writes the DTAColumnSpecCollection to a YAML file.
#' @examples
#' \dontrun{
#' # Create a DTAColumnSpecCollection object
#' columns <- import_columns_from_yaml("path/to/yaml/file.yaml")
#'
#' # Write the DTAColumnSpecCollection object to a YAML file
#' write_columns_to_yaml(columns, "path/to/output/file.yaml")
#' }
write_columns_to_yaml <- function(
  DTAColumnSpecCollection,
  file
) {
  yaml::write_yaml(
    as.list(columns),
    file
  )
}

#' @title Write DTAColumnSpecCollection to JSON File
#' @description
#' This function writes a DTAColumnSpecCollection object to a JSON file.
#' @importFrom jsonlite write_json
#' @export
#'
#' @param columns An object of class DTAColumnSpecCollection.
#' @param file Character. Path to the JSON file to write the specifications to.
#' @param pretty Logical. Whether to pretty-print the JSON. Default is TRUE.
#' @return NULL. The function writes the DTAColumnSpecCollection to a JSON file.
#' @examples
#' \dontrun{
#' # Create a DTAColumnSpecCollection object
#' columns <- import_columns_from_yaml("path/to/yaml/file.yaml")
#'
#' # Write the DTAColumnSpecCollection object to a JSON file
#' write_columns_to_json(columns, "path/to/output/file.json")
#' }
write_columns_to_json <- function(
  columns,
  file,
  pretty = TRUE
) {
  jsonlite::write_json(
    as.list(columns),
    path = file,
    pretty = pretty,
    auto_unbox = TRUE
  )
}
