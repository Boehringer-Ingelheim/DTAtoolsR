#' @title DTA Class
#' @description This class helps checking validity of data tables against transmission
#'  specifications in data transfer agreements (DTA). Also it provides a way to
#'  generate DTA/DTS documents from specifications.
#' @import S7
#' @importFrom cli cli_h1
#'
#' @param container A names list of DTADataSet objects
#' @param ... arguments will be passed to DTAMetadata(...)
#' @return An object of class DTA.
#'
#' @examples
#'
#' \dontrun{
#' # Create sample tables
#' table1 <- data.frame(STUDYID = c("1234", "1234", "1234"), VISIT = c("V03", "V03", "EOT"))
#' table2 <- data.frame(STUDYID = c("1234", "1234", "1234"), VISIT = c("EOT", "V05", "EOT"))
#'
#' # List of tables
#' tables <- list(table1 = table1, table2 = table2)
#'
#' # Create the DTADataSet object
#' data_obj <- DTADataSet(DTAColumnSpecCollection, tables)
#'
#' DTA(container = list(data = data_obj))
#' }
#' @export
DTA <- new_class(
  "DTA",
  constructor = function(
    container = NULL,
    metadata = NULL,
    ...
  ) {
    if (inherits(container, "DTAtools::DTADataSet")) {
      container <- list(container)
      names(container) <- container[[1]]@name
    }
    if (is.list(container) && !is.null(container) && is.null(names(container))) {
      names(container) <- vapply(container, function(x) x@name, character(1))
    }

    if (is.null(metadata)) {
      metadata <- DTAMetaData(...)
    }
    new_object(
      S7_object(),
      container = container,
      metadata = metadata
    )
  },
  properties = list(
    container = class_list,
    metadata = class_DTAMetadata
  )
)


#' @title Get Metadata
#' @description
#' Method to get Metadata from a DTA object.
#' @param x An object of class DTA
#' @return A list with metadata information
#' @examples
#' library(DTAtools)
#' dta_obj <- create_example_DTA()
#' metadata(dta_obj)
#' @name metadata
#' @rdname metadata-DTA
metadata <- new_generic("metadata", "x")
#' @export
method(metadata, DTA) <- function(x) {
  return(x@metadata)
}


#' @title Get container
#' @description
#' Method to get one or more containers from a DTA object.
#' @importFrom cli cli_alert_info
#' @param x An object of class DTA.
#' @param name Optional single character or single integer. if NULL, returns a 
#' list of all containers. If character, returns the container with the specified name.
#' If integer, returns the container at the specified index.
#' @return Either a list of DTADataSet objects or a single DTADataSet.
#' @examples
#' libary(DTAtools)
#' x <- create_example_DTA()
#' container(x)
#' container(x, "vitals")
#' container(x, 1)
#' @name container
#' @export
if (!exists("container", mode = "function")) {
  container <- new_generic("container", "x")
}

#' @export
method(container, DTA) <- function(x, name = NULL) {
  if(!is.null(name) && !is.character(name) && !is.numeric(name) && length(name) != 1) {
    cli::cli_abort("'name' must be a single character vector, single numeric index or NULL.")
  }
  all_containers <- x@container

  if (is.null(name)) {
    return(all_containers)
  }

  if (is.numeric(name)) {
    if (any(name < 1) || any(name > length(all_containers))) {
      cli::cli_abort("Numeric 'name' index out of bounds.")
    }
    return(all_containers[[name]])
  }

  missing <- setdiff(name, names(all_containers))
  if (length(missing) > 0) {
    cli::cli_abort("The following container{?s} not found: {.field {missing}}")
  }

  return(all_containers[[name]])
}


#' @title Print DTA Object
#' @description
#' Print method for DTA objects.
#' @param x An object of class DTA
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#' @importFrom cli cli_alert_info cli_h1 cli_alert cli_text
#' @examples
#' \dontrun{
#'   print(dta_obj)
#' }
#' @name print
#' @export
method(print, DTA) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("<{.emph DTA}>")
  cli_alert_info("Metadata: {x@metadata@name} {x@metadata@version}")


  if (!is.null(x@container) && length(x@container) > 0) {
    message <- paste0("Containers ({length(x@container)}): ", 
                  paste(paste0("{.field ", names(x@container), "}"), 
                      collapse = ", "))
    cli_alert_info(message)
  } else {
    cli_alert("Containers: none")
  }

  invisible(x)
}

#' @title Create Example DTA Object
#' @description
#' Creates an example DTA object for demonstration purposes.
#' @param index index of the example to create 
#' @importFrom cli cli_abort
#' @return An object of class DTA with example data
#' @examples
#' \dontrun{
#'   example_dta <- create_example_DTA()
#'   print(example_dta)
#' }
#' @export
create_example_DTA <- function(index = 1) {
  switch (index,
    `1` = {
      DTA(
        container = list(
          create_example_DTADataSetTabular(2),
          concreate_example_DTADataSetTabular(3)
        ),
        metadata = create_example_DTAMetaData()
      )
    },
    `2` = {

    }
    cli_abort("No example found with index {index}.")
  )

}
