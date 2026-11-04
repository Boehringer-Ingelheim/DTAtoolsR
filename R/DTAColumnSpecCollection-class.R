#' @title DTAColumnSpecCollection Class
#' @description
#' This class represents a collection of DTAColumnSpec objects with optional template.
#' @import S7
#' @importFrom cli cli_abort
#' @importFrom stringr str_flatten_comma
#' @param columns A named list of DTAColumnSpec objects. The names should correspond to the ids of the columns
#' @param rules A list of DTARule objects, or NULL.
#' @return An object of class DTAColumnSpecCollection.
#' @examples
#' col1 <- DTAColumnSpec(id = "STUDYID", type = "SAS Char", nullable = TRUE)
#' col2 <- DTAColumnSpec(id = "VISIT", type = "SAS Char", nullable = FALSE)
#' collection <- DTAColumnSpecCollection(columns = list(STUDYID = col1, VISIT = col2))
#' @export
DTAColumnSpecCollection <- S7::new_class(
  "DTAColumnSpecCollection",
  constructor = function(
    columns,
    rules = NULL
  ) {
    if (is.list(rules) && length(rules) == 0) {
      rules <- NULL
    }

    if (!is.list(columns)) {
      cli::cli_abort("'columns' must be a list.")
    }

    if (!all(sapply(columns, inherits, "DTAtools::DTAColumnSpec"))) {
      cli::cli_abort(
        "All elements in 'columns' must be of class 'DTAColumnSpec'"
      )
    }

    if (!is.null(rules) && !all(sapply(rules, inherits, "DTAtools::DTARule"))) {
      cli::cli_abort(
        "All elements in 'rules' must be of class 'DTARule'"
      )
    }

    if (is.null(names(columns))) {
      names(columns) <- sapply(columns, function(col) col@id)
    }

    new_object(
      S7_object(),
      columns = columns,
      rules = rules
    )
  },
  properties = list(
    columns = S7::class_list,
    rules = class_list_or_null
  ),
  validator = function(self) {
    # Every branch here must `return()`. A bare string is the value of an `if`
    # that nothing reads, so the check it looks like it performs does not
    # happen: the class check below used to fall through to `col@id` on a plain
    # list and surface "no applicable method for `@`" instead of its own
    # message.
    if (!all(sapply(self@columns, inherits, "DTAtools::DTAColumnSpec"))) {
      return("All elements in 'columns' must be of class 'DTAColumnSpec'")
    }

    columns_ids <- sapply(self@columns, function(col) col@id)

    # A duplicated id is a specification error, and a silent one: `colspec()`
    # looks a column up with `[[`, which returns the FIRST match, so the second
    # definition is invisible to every consumer that goes by id -- while the
    # column spec axis, which iterates the list, evaluates the same table column
    # twice against two possibly contradictory specs.
    duplicated_ids <- unique(columns_ids[duplicated(columns_ids)])
    if (length(duplicated_ids) > 0) {
      return(paste0(
        "Column 'id' must be unique. Duplicated: ",
        paste(duplicated_ids, collapse = ", ")
      ))
    }

    if (is.null(names(self@columns))) {
      names(self@columns) <- columns_ids
    }

    if (!all(names(self@columns) == columns_ids)) {
      return(paste0(
        "Names of 'columns' must match the 'id' of each DTAColumnSpec.\n",
        "  ids:   ", paste(columns_ids, collapse = ", "), "\n",
        "  names: ", paste(names(self@columns), collapse = ", ")
      ))
    }

    if (!is.null(self@rules) && !all(sapply(self@rules, inherits, "DTAtools::DTARule"))) {
      return("All elements in 'rules' must be of class 'DTARule'")
    }

    NULL
  }
)


#' @title Preview Column IDs in a DTAColumnSpecCollection
#'
#' @description
#' Provides a preview of the column IDs contained within a \code{DTAColumnSpecCollection} object.
#' If the collection contains more than `n` columns, it returns the IDs of the first `n - 1`
#' columns, an ellipsis, and the last column's ID. If there are `n` or fewer columns, it returns
#' all column IDs. A collection with no columns is reported as `"not set"`.
#'
#' @param x A \code{DTAColumnSpecCollection} object.
#' @param ... Further arguments to the method: `n`, the most IDs to show
#'   (a single whole number of at least 2; default 8).
#'
#' @return A character string representing a preview of the column IDs.
#'
#' @seealso \code{\link{DTAColumnSpecCollection}}
#' @examples
#' x <- create_example_DTAColumnSpecCollection()
#' column_preview(x)
#' @export
column_preview <- new_generic("column_preview", "x")
method(column_preview, DTAColumnSpecCollection) <- function(x, n = 8) {
  # Below 2 the abbreviation `x@columns[1:(n - 1)]` indexes with 1:0 and
  # shows the first column anyway.
  if (length(n) != 1 || !is.numeric(n) || is.na(n) || n < 2 || n != trunc(n)) {
    cli::cli_abort("{.arg n} must be a single whole number of at least 2.")
  }
  ids <- vapply(x@columns, function(y) y@id, character(1), USE.NAMES = FALSE)
  if (length(ids) == 0) {
    return("not set")
  }
  if (length(ids) > n) {
    ids <- c(ids[seq_len(n - 1)], "...", ids[[length(ids)]])
  }
  str_flatten_comma(ids)
}


#' @title Preview Rules in a DTAColumnSpecCollection
#'
#' @description
#' Provides a preview of the rules contained within a \code{DTAColumnSpecCollection} object.
#' If the collection contains more than 5 rules, it returns the IDs of the first four rules,
#' an ellipsis, and the rules ID. If there are 5 or fewer rules, it returns all rules.
#'
#' @importFrom stringr str_flatten_comma
#'
#' @param x A \code{DTAColumnSpecCollection} object.
#' @param ... Not used by current methods; reserved for future extensions.
#'
#' @return A character string representing a preview of the rules
#'
#' @seealso \code{\link{DTAColumnSpecCollection}}
#' @examples
#' x <- create_example_DTAColumnSpecCollection()
#' rule_preview(x)
#' @export
rule_preview <- new_generic("rule_preview", "x")
method(rule_preview, DTAColumnSpecCollection) <- function(x) {
  if (!is.null(x@rules)) {
    rules <- x@rules
    if (length(rules) > 5) {
      rule_preview <- stringr::str_flatten_comma(
        c(
          map(rules[1:4], function(y) y@id),
          "...",
          rules[[length(rules)]]@id
        )
      )
    } else if (length(rules) <= 5) {
      rule_preview <- stringr::str_flatten_comma(map(rules, function(y) {
        y@id
      }))
    }
  } else {
    rule_preview <- "not set"
  }

  rule_preview
}

#' @importFrom cli cli_alert_info cli_alert cli_text cli_div
#' @importFrom purrr map
#' @name print
#' @export
method(print, DTAColumnSpecCollection) <- function(x, ...) {
  col_preview <- column_preview(x)
  rule_preview <- rule_preview(x)

  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli::cli_text("<{.emph DTAColumnSpecCollection}>")

  cli::cli_alert_info("columns ({length(x@columns)}): {col_preview}")

  if (!is.null(x@rules) && length(x@rules) > 0) {
    cli::cli_alert_info("rules ({length(x@rules)}): {rule_preview}")
  } else {
    cli::cli_alert("rules: {cli::symbol$cross}")
  }
}


#' @title Get Names Method
#' @description
#' Method to get the names of columns in the collection.
#' @param x An object of class DTAColumnSpecCollection.
#' @param ... Additional arguments (not used).
#' @return A character vector of column names.
#' @examples
#' collection <- create_example_DTAColumnSpecCollection()
#' names(collection)
#' @name names
#' @usage names(x, ...)
#' @export
if (!exists("names", mode = "function")) {
  names <- new_generic("names", "x")
}
method(names, DTAColumnSpecCollection) <- function(x) {
  return(as.character(sapply(x@columns, function(col) col@id)))
}

#' @title Get Column by ID Method
#' @description
#' Method to get a column format by its ID from the collection.
#' @param x An object of class DTAColumnSpecCollection.
#' @param ... Additional named arguments:
#'   \describe{
#'     \item{id}{The column to retrieve: its ID (a single string) or its
#'       position (a single whole number). An ID or position that does not
#'       exist is an error of class `dta_colspec_not_found`.}
#'   }
#' @return A DTAColumnSpec object corresponding to the specified ID.
#' @examples
#' collection <- create_example_DTAColumnSpecCollection()
#' colspec(collection, "STUDYID")
#' @usage colspec(x, ...)
#' @name colspec
#' @export
# colspec <- new_generic("colspec", "x", function(x, ...) {
#   S7_dispatch()
# })
colspec <- new_generic("colspec", "x")

#' @export
method(colspec, DTAColumnSpecCollection) <- function(x, id) {
  columns <- x@columns
  # `[[` leaked a raw base subscript error for a position past the end,
  # silently truncated a fractional one, and returned NULL for an unknown
  # name -- which the caller then dereferenced with `@` far from the cause.
  if (is.character(id)) {
    if (length(id) != 1 || is.na(id) || !id %in% names(columns)) {
      cli::cli_abort(
        c(
          "No column {.val {id}} in the specification.",
          i = "Columns: {.val {names(columns)}}."
        ),
        class = "dta_colspec_not_found"
      )
    }
    return(columns[[id]])
  }
  if (is.numeric(id)) {
    if (length(id) != 1 || is.na(id) || id != trunc(id)) {
      cli::cli_abort(
        "Argument {.arg id} must be a single, non-missing whole number or a column name.",
        class = "dta_colspec_not_found"
      )
    }
    if (id < 1 || id > length(columns)) {
      cli::cli_abort(
        "Column position {id} is out of bounds: the specification has {length(columns)} column{?s}.",
        class = "dta_colspec_not_found"
      )
    }
    return(columns[[id]])
  }
  cli::cli_abort(
    "Argument {.arg id} must be a column name or a position.",
    class = "dta_colspec_not_found"
  )
}


#' @title Get Rules
#' @description
#' Method to get Rules from DTAColumnSpecCollection
#' @param x An object of class DTAColumnSpecCollection.
#' @param ... Not used by current methods; reserved for future extensions.
#' @return A list of DTARule objects, or NULL if no rules are defined.
#' @examples
#' collection <- create_example_DTAColumnSpecCollection()
#' rules(collection)
#' @name rules
#' @export
rules <- new_generic("rules", "x")

method(rules, DTAColumnSpecCollection) <- function(x, ...) {
  return(x@rules)
}

#' @title Create DTAColumnSpecCollection from YAML File
#' @description
#' This function parses a YAML file to extract column specifications and create a DTAColumnSpecCollection object.
#' @importFrom yaml read_yaml
#' @importFrom cli cli_abort cli_alert
#' @export
#'
#' @param file Character. Path to the YAML file containing specifications.
#' @return An object of class DTAColumnSpecCollection.
#' @examples
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
#' specs <- import_specs_from_yaml(yaml_file)
#' specs
import_specs_from_yaml <- function(file) {
  yaml <- yaml::read_yaml(file)

  if (is.null(yaml$columns)) {
    cli::cli_abort("YAML file must contain a 'columns' section.")
  }

  columns <- yaml$columns

  if (is.null(yaml$rules)) {
    rules <- list()
    cli::cli_alert(
      "No 'rules' section found in YAML. Proceeding without rules."
    )
  } else {
    rules <- yaml$rules
  }

  return(specs_from_list(columns, rules))
}

#' @title Create DTAColumnSpecCollection from Components
#' @description
#' Constructs a DTAColumnSpecCollection object from separate components: columns and rules.
#' Supports both named and unnamed lists of column specifications.
#'
#' @importFrom cli cli_abort
#' @param columns A list of column specification lists. Each must contain at
#'   least an `id`. `NULL` or an empty list yields a collection with no
#'   columns -- and a `DTADataSetTabular` built on such a collection no longer
#'   validates as a pass: `check()` records status `"unspecified"` (`ok = NA`)
#'   for any table checked against zero column specs, rather than reporting a
#'   clean pass over checks that never ran.
#' @param rules Optional list of rules.
#'
#' @return An object of class DTAColumnSpecCollection.
#' @export
#'
#' @examples
#' # Load example YAML file from package extdata
#' yaml_file <- system.file("extdata", "gf_dataset.yaml", package = "DTAtools")
#' input_list <- yaml::read_yaml(yaml_file)
#' specs <- specs_from_list(input_list$columns, input_list$rules)
#'
specs_from_list <- function(
  columns,
  rules = list()
) {
  # An absent `columns:` key means "no columns declared", not an error. The
  # serializer omits the key entirely for a dataset with none, so rejecting
  # NULL here made the package unable to read YAML it had itself just written --
  # reachable by deleting a dataset's last column, and by every newly added
  # dataset, which starts empty. The resulting empty collection is not a
  # silent pass: `check(DTADataSetTabular)` treats zero columns as status
  # `"unspecified"` (`ok = NA`), not a validated table.
  if (is.null(columns)) {
    columns <- list()
  }
  if (!is.list(columns)) {
    cli::cli_abort("`columns` must be a list.")
  }
  if (!is.null(rules) && !is.list(rules)) {
    cli::cli_abort("`rules` must be a list or null.")
  }

  dta_rules <- list()
  dta_columns <- list()

  if (length(rules) > 0) {
    dta_rules <- lapply(rules, function(x) {
      do.call(DTARuleFactory, x)
    })
  }

  if (length(columns) > 0) {
    dta_columns <- lapply(columns, function(x) {
      do.call(DTAColumnSpec, x)
    })
    lapply(dta_columns, dta_warn_numeric_values_on_text_column)
  }

  return(DTAColumnSpecCollection(
    columns = dta_columns,
    rules = dta_rules
  ))
}

#' @title Warn About Permitted Values YAML Read as Numbers
#' @description
#' YAML applies implicit typing to unquoted scalars, so `values: [1.10, 2.00]`
#' arrives as the numbers `1.1` and `2`. For a text column the permitted values
#' are compared as text, and `as.character(1.1)` is `"1.1"` -- so the data the
#' author meant to allow, `"1.10"`, fails the check with no hint as to why.
#'
#' The original spelling is gone by the time the parser hands the list over, so
#' this cannot be repaired here. It can only be pointed at.
#' @param spec A `DTAColumnSpec`.
#' @return `invisible(NULL)`, called for the warning.
#' @keywords internal
dta_warn_numeric_values_on_text_column <- function(spec) {
  values <- spec@values
  if (is.null(values) || length(values) == 0) {
    return(invisible(NULL))
  }

  flat <- unlist(values, use.names = FALSE)
  if (!is.numeric(flat)) {
    return(invisible(NULL))
  }

  # A column with no declared type has no text type to warn about: its schema
  # admits every base type, "string" included, so the test below would fire
  # for any numeric `values` on it.
  if (is.null(spec@structure)) {
    return(invisible(NULL))
  }

  types <- tryCatch(as_json_schema_type(spec), error = function(e) NULL)
  if (!"string" %in% types) {
    return(invisible(NULL))
  }

  cli::cli_warn(c(
    "Column {.field {spec@id}} holds text, but its permitted {.field values} \\
     were read as numbers.",
    x = "YAML reads an unquoted {.code 1.10} as the number {.val {1.1}}, so a \\
         value written {.val 1.10} in the data will not match.",
    i = "Quote them in the specification: {.code values: [\"1.10\", \"2.00\"]}."
  ))
  invisible(NULL)
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
#' Parses a table in a Word document into a DTAColumnSpecCollection. The table
#' layout is the one [export_specs_table()] writes, so a specification exported
#' to Word, reviewed there, and read back arrives at the same columns.
#'
#' A column's permitted values and pattern travel in its Description cell, on
#' lines of their own: `#@values: "A"; "B"` and `#@pattern: ^[A-Z]{3}$`.
#' @importFrom docxtractr docx_extract_all_tbls
#' @importFrom purrr map set_names
#' @importFrom dplyr mutate filter
#' @export
#' @examples
#' specs <- DTAColumnSpecCollection(columns = list(
#'   ARM = DTAColumnSpec(
#'     id = "ARM", label = "Arm", type = "SAS Char", length = 8,
#'     nullable = FALSE, values = c("Placebo", "Active")
#'   ),
#'   VISIT = DTAColumnSpec(
#'     id = "VISIT", label = "Visit", type = "SAS Char", length = 20,
#'     nullable = TRUE, pattern = "^(Screening|Baseline|Week [0-9]+)$"
#'   )
#' ))
#' docx <- tempfile(fileext = ".docx")
#' export_specs_table(specs, file = docx, quiet = TRUE)
#' back <- columns_specs_from_word(docx)
#' back@columns$ARM@values
#' back@columns$VISIT@pattern
#' unlink(docx)
#' @param file Character. Path to the Word document.
#' @param table_position Integer. Index of the table to extract.
#' @param colnames Character vector naming the table's columns, in order. When
#'   left at its default and the table's header row is the one
#'   [export_specs_table()] writes (in either of its two layouts), columns are
#'   matched by their header instead, so the six-column layout without a
#'   Length column is read correctly too. Recognised names: `id`, `label`,
#'   `type`, `length`, `format`, `nullable`, `description`; `id` is required.
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
  # A soft line break (Shift+Enter in Word, and what flextable writes for a
  # newline in a cell) is a <w:br/>, which docxtractr drops without a trace, so
  # "#@pattern: ^A$" followed by a second line would read as one run-on line.
  # Write each one as a text node holding a newline before extracting.
  if (inherits(doc$docx, "xml_document")) {
    for (br in xml2::xml_find_all(doc$docx, "//*[local-name()='tbl']//*[local-name()='br']")) {
      xml2::xml_add_sibling(br, "w:t", intToUtf8(10L), .where = "after")
      xml2::xml_remove(br)
    }
  }
  tables <- docxtractr::docx_extract_all_tbls(doc, preserve = TRUE)
  if (
    length(table_position) != 1 || !is.numeric(table_position) ||
      is.na(table_position) || table_position != round(table_position) ||
      table_position < 1 || table_position > length(tables)
  ) {
    cli::cli_abort(
      c(
        "{.arg table_position} must be the index of a table in the document.",
        x = "The document holds {length(tables)} table{?s}."
      ),
      class = "dta_word_import_error"
    )
  }
  columns <- as.data.frame(tables[[table_position]], stringsAsFactors = FALSE)

  # The header row export_specs_table() writes, in either of its layouts.
  header_map <- c(
    "variable name" = "id",
    "variable label" = "label",
    "type" = "type",
    "length" = "length",
    "format" = "format",
    "nullable" = "nullable",
    "description" = "description"
  )
  header <- tolower(trimws(gsub("[^A-Za-z]+", " ", names(columns))))
  if (missing(colnames) && length(header) > 0 && all(header %in% names(header_map))) {
    colnames <- unname(header_map[header])
  }
  if (length(colnames) != ncol(columns)) {
    cli::cli_abort(
      c(
        "{.arg colnames} names {length(colnames)} column{?s}, but table {table_position} has {ncol(columns)}.",
        i = "Pass one name per table column, in order."
      ),
      class = "dta_word_import_error"
    )
  }
  if (!"id" %in% colnames) {
    cli::cli_abort("{.arg colnames} must include {.val id}.", class = "dta_word_import_error")
  }
  colnames(columns) <- colnames

  # docxtractr hands back an empty cell as either "" or NA.
  blank <- function(x) is.na(x) | !nzchar(trimws(x))
  cell <- function(row, name) {
    if (!name %in% names(row)) {
      return(NULL)
    }
    value <- as.character(row[[name]])
    if (length(value) != 1 || blank(value)) NULL else trimws(value)
  }
  unquote <- function(x) gsub("^[\"']|[\"']$", "", trimws(x))

  # The rest of the line after a tag. Cut at a real line break: the earlier
  # character class `[^\\n]` excluded a backslash and the LETTER n, which cut a
  # pattern such as ^(Screening|Baseline)$ down to ^(Scree.
  tag_line <- function(desc, tag) {
    if (is.null(desc) || !grepl(tag, desc, fixed = TRUE)) {
      return(NULL)
    }
    rest <- substring(desc, regexpr(tag, desc, fixed = TRUE) + nchar(tag))
    line <- trimws(sub("[\r\n].*$", "", rest))
    if (nzchar(line)) line else NULL
  }
  backend_of <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    first <- strsplit(x, "[[:space:]]+")[[1]][1]
    if (first %in% `__DTAtools_supported_backends__`) first else NULL
  }

  ids <- gsub("[[:space:]]+", "", as.character(columns$id))
  keep <- which(!blank(ids))
  column_list <- lapply(keep, function(i) {
    row <- as.list(columns[i, , drop = FALSE])
    description <- cell(row, "description")

    values_line <- tag_line(description, "#@values:")
    values <- if (is.null(values_line)) {
      NULL
    } else {
      unquote(strsplit(values_line, value_sep, fixed = TRUE)[[1]])
    }
    pattern_line <- tag_line(description, "#@pattern:")
    pattern <- if (is.null(pattern_line)) NULL else unquote(pattern_line)
    if (!is.null(description)) {
      # Drop each tag line together with the line break before it.
      description <- trimws(gsub("[\r\n]*#@(values|pattern):[^\r\n]*", "", description))
      if (!nzchar(description)) description <- NULL
    }

    # export_specs_table() writes a format without its backend prefix ("$10.",
    # not "SAS $10."), because the structure stores it that way; the type cell
    # carries the prefix. Restore it from the type, so the table that function
    # writes can be read back.
    type <- cell(row, "type")
    format <- cell(row, "format")
    if (!is.null(format) && is.null(backend_of(format)) && !is.null(backend_of(type))) {
      format <- paste(backend_of(type), format)
    }

    length_cell <- cell(row, "length")
    length_value <- if (is.null(length_cell)) NULL else suppressWarnings(as.numeric(length_cell))
    if (!is.null(length_value) && is.na(length_value)) {
      cli::cli_abort(
        "The Length cell of column {.field {ids[[i]]}} is not a number: {.val {length_cell}}.",
        class = "dta_word_import_error"
      )
    }
    nullable_cell <- cell(row, "nullable")

    DTAColumnSpec(
      id = ids[[i]],
      label = cell(row, "label"),
      type = type,
      format = format,
      length = length_value,
      # "Yes", "y", "TRUE", and a qualified "Yes (if consented)" all mean yes.
      nullable = if (is.null(nullable_cell)) NULL else grepl("^(yes|y|true)([^a-z]|$)", tolower(nullable_cell)),
      values = values,
      pattern = pattern,
      description = description
    )
  })
  names(column_list) <- ids[keep]

  DTAColumnSpecCollection(columns = column_list)
}


#' @name as_json_schema
#' @importFrom jsonlite toJSON
#' @usage as_json_schema(x, ...)
#' @export
# `inherits = FALSE` scopes this lookup to this package's namespace; without
# it, an attached package exporting a plain function of the same name would
# make this guard skip creating the generic. See `R/00_helpers.R` for the
# full account.
if (!exists("as_json_schema", mode = "function", inherits = FALSE)) {
  as_json_schema <- new_generic("as_json_schema", "DTAColumnSpecCollection")
}
#' @export
method(as_json_schema, DTAColumnSpecCollection) <- function(x) {
  properties <- lapply(x@columns, as_json_schema)

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

  # This previously compiled a JSON Schema validator here and discarded it --
  # a full schema compile on every call, for nothing. Validation no longer runs
  # through a JSON Schema validator at all (see R/columnSpecChecks.R); this
  # function's job is serialising a spec collection to JSON Schema for other
  # tools to consume, which is useful in its own right.
  return(json_schema)
}

#' @export
#' @name as.list
method(as.list, DTAColumnSpecCollection) <- function(x, ...) {
  columns <- lapply(x@columns, function(column) {
    as.list(column)
  })

  rules <- lapply(x@rules, function(rule) {
    as.list(rule)
  })

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
#' create_example_DTAColumnSpecCollection()
#' @export
create_example_DTAColumnSpecCollection <- function(index = 1) {
  col1 <- create_example_DTAColumnSpec(1)
  col2 <- create_example_DTAColumnSpec(2)
  col3 <- create_example_DTAColumnSpec(3)
  col4 <- create_example_DTAColumnSpec(4)
  col5 <- create_example_DTAColumnSpec(5)

  switch(index,
    `1` = {
      example_rules <- list()
      DTAColumnSpecCollection(
        columns = setNames(
          list(col1, col2, col3, col4),
          c(col1@id, col2@id, col3@id, col4@id)
        ),
        rules = example_rules
      )
    },
    `2` = {
      example_rules <- list()
      DTAColumnSpecCollection(
        columns = setNames(
          list(col1, col2, col3, col5),
          c(col1@id, col2@id, col3@id, col5@id)
        ),
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
#' columns <- create_example_DTAColumnSpecCollection()
#' out_file <- tempfile(fileext = ".yaml")
#' write_columns_to_yaml(columns, out_file)
#' unlink(out_file)
write_columns_to_yaml <- function(
  columns,
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
#' columns <- create_example_DTAColumnSpecCollection()
#' out_file <- tempfile(fileext = ".json")
#' write_columns_to_json(columns, out_file)
#' unlink(out_file)
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
