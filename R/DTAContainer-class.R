#' @title DTAContainer Class
#' @description
#' A \code{DTAContainer} is one delivery archive (a \code{.zip} file or a
#' \code{.xpt} SAS transport file) that a data transfer agreement names once,
#' at document level, in the \code{containers} of a \code{\link{DTA}}. File
#' entries refer to it by its \code{id}, so renaming the archive is one edit.
#' One container may be the \code{default}: every file entry without a
#' container of its own then belongs to it.
#'
#' @include 00_helpers.R
#' @import S7
#' @importFrom cli cli_abort
#' @param id Character. A short name for the container: it starts with a
#'   letter and continues with letters, digits, \code{_} or \code{-}.
#'   \code{"none"} is reserved, in any letter case. File entries use the id
#'   as their \code{container}.
#' @param filename Character. The name of the archive: a \code{.zip} or
#'   \code{.xpt} file name. \code{*} matches any text and \code{?} one
#'   character (\code{"STUDY_SDTM_*.zip"}); the name is matched as a whole and
#'   letter case must match.
#' @param description Character or \code{NULL}. Free text shown in exported
#'   documents.
#' @param default Logical. If \code{TRUE}, every file entry that has no
#'   container of its own must arrive inside this one. At most one container
#'   of a document is the default.
#' @return An object of class \code{DTAContainer}.
#' @examples
#' sdtm <- DTAContainer(
#'   "sdtm", "STUDY_SDTM_*.zip",
#'   description = "All SDTM domains.", default = TRUE
#' )
#' adam <- DTAContainer("adam", "adam.zip")
#'
#' ae <- DTADataSetTabular(
#'   id = "ae",
#'   specs = create_example_DTAColumnSpecCollection(1),
#'   files = list(DTAFileCSV("ae.csv"))
#' )
#' adsl <- DTADataSetTabular(
#'   id = "adsl",
#'   specs = create_example_DTAColumnSpecCollection(1),
#'   files = list(DTAFileCSV("adsl.csv", container = "adam"))
#' )
#' dta <- DTA(
#'   datasets = list(ae, adsl),
#'   metadata = create_example_DTAMetaData(),
#'   containers = list(sdtm, adam)
#' )
#' print(dta)
#' @export
DTAContainer <- S7::new_class(
  "DTAContainer",
  constructor = function(id, filename, description = NULL, default = FALSE) {
    if (missing(filename)) filename <- NULL
    problem <- dta_container_def_problem(id, filename, description, default)
    if (!is.null(problem)) {
      cli::cli_abort("{problem}", class = "dta_container_invalid")
    }
    new_object(
      S7_object(),
      id = id,
      filename = filename,
      description = description,
      default = default
    )
  },
  properties = list(
    id = class_character,
    filename = class_character,
    description = class_character_or_null,
    default = class_logical
  ),
  validator = function(self) {
    problem <- dta_container_def_problem(
      self@id, self@filename, self@description, self@default
    )
    if (!is.null(problem)) {
      cli::cli_abort("{problem}", class = "dta_container_invalid")
    }
  }
)

# TRUE for a container id (and for a file entry's reference by id): one string,
# a letter first, then letters, digits, `_` or `-`; "none" is reserved. An id
# never contains a dot, so it can never be mistaken for a file name.
dta_container_id_ok <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    grepl("^[A-Za-z][A-Za-z0-9_-]*$", x) && tolower(x) != "none"
}

# NULL when the four fields make a container, else ONE sentence saying why not.
# Shared by the constructor and the validator; the caller hands the sentence to
# cli as data ("{problem}"), because it holds user text.
dta_container_def_problem <- function(id, filename, description, default) {
  shown <- function(v) paste(format(v), collapse = ", ")
  if (!dta_container_id_ok(id)) {
    return(sprintf(
      "A container's id must start with a letter and use only letters, digits, '_' and '-' ('none' is reserved); got '%s'.",
      shown(id)
    ))
  }
  if (is.null(filename)) {
    return(sprintf("Container '%s' needs a filename.", id))
  }
  if (!is.character(filename) || length(filename) != 1L || is.na(filename) || !nzchar(filename)) {
    return(sprintf("The filename of container '%s' must be a single non-empty string.", id))
  }
  problem <- dta_container_value_problem(filename)
  if (!is.null(problem)) {
    return(sprintf("Container '%s': %s", id, problem))
  }
  if (!is.null(description) &&
    !(is.character(description) && length(description) == 1L && !is.na(description))) {
    return(sprintf("The description of container '%s' must be a single string.", id))
  }
  if (!(is.logical(default) && length(default) == 1L && !is.na(default))) {
    return(sprintf("'default' of container '%s' must be TRUE or FALSE.", id))
  }
  NULL
}
