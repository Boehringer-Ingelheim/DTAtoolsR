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
    metadata = class_any #TODO: should be class DTAMetaData
  )
)

#' @title Get Metadata
#' @description
#' Method to get Metadata from DTA.
#' @param x An object of class DTA
#' @return A list with metadata information
#' @examples
#' \dontrun{
#'   get_metadata(DTA)
#' }
#' @name metadata-DTA
if (!exists("get_metadata", mode = "function")) {
  get_metadata <- new_generic("get_metadata", "x")
}
method(get_metadata, DTA) <- function(x) {
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
#' @name container-DTA
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
