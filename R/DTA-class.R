#' @title DTA Class
#' @description This class helps checking validity of data tables against transmission
#'  specifications in data transfer agreements (DTA). Also it provides a way to
#'  generate DTA/DTS documents from specifications.
#' @import S7
#' @importFrom cli cli_h1
#'
#' @param container A names list of DTAContainer objects
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
#' # Create the DTAContainer object
#' data_obj <- DTAContainer(DTAColumnSpecCollection, tables)
#'
#' DTA(container = list(data = data_obj))
#' }
DTA <- new_class(
  "DTA",
  constructor = function(
    container = NULL,
    ...
  ) {
    new_object(
      S7_object(),
      container = container,
      metadata = DTAMetaData(...)
    )
  },
  properties = list(
    container = class_list,
    metadata = class_DTAMetadata
  )
)

#' @title Get Metadata
#' @description
#' Method to get Metadata from DTA.
#' @param x An object of class DTA
#' @return A list with metadata information
#' @examples
#' library(DTAtools)
#' DTA <- create_example_DTA()
#' metadata(DTA)
#'
#' @name metadata
#' @rdname metadata-DTA
#' @export
if (!exists("metadata", mode = "function")) {
  metadata <- new_generic("metadata", "x")
}
method(metadata, DTA) <- function(x) {
  return(x@metadata)
}


#' @title Get container
#' @description
#' Method to get one or more containers from a DTA object.
#' @importFrom cli cli_alert_info
#' @param x An object of class DTA.
#' @param name Optional character vector. One or more container names to retrieve.
#' @return Either a list of DTRContainer objects s or a single named DTAContainer.
#' @examples
#' \dontrun{
#' container(DTAContainer)
#' container(DTAContainer, "container1")
#' container(DTAContainer, c("container1", "container2"))
#' }
#' @name container
#' @export
if (!exists("container", mode = "function")) {
  container <- new_generic("container", "x")
}

#' @export
method(container, DTA) <- function(x, name = NULL) {
  all_containers <- x@container

  if (is.null(name)) {
    cli::cli_alert_info("Returning all containers in the DTA Object as a list")
    return(all_containers)
  }

  missing <- setdiff(name, names(all_containers))
  if (length(missing) > 0) {
    cli::cli_abort("The following container{?s} not found: {.field {missing}}")
  }

  cli::cli_alert_info("Returning the DTAContainer with the name{?s}: '{name}'")
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
  cli_text("<DTA> object")

  print(x@metadata)
  cli_alert("Number of containers: {length(x@container)}")

  if (length(x@container) > 0) {
    cli_alert_info("Container names: {names(x@container)}")
  }

  invisible(x)
}

#' @title Create Example DTA Object
#' @description
#' Creates an example DTA object for demonstration purposes.
#' @param title Character string. Title for the DTA object.
#' @param version Character string. Version of the DTA object.
#' @importFrom arrow arrow_table
#' @return An object of class DTA with example data
#' @examples
#' \dontrun{
#'   example_dta <- create_example_DTA()
#'   print(example_dta)
#' }
#' @export
create_example_DTA <- function() {
  cont1 <- create_example_DTAContainer(2)
  cont2 <- create_example_DTAContainer(3)
  
  # Create DTA object
  DTA(
    container = list(cont1, cont2),
    author = "create example function",
    version = "0.1"
  )
}