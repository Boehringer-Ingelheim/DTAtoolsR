#' @title Where Every File Entry Must Arrive
#' @description
#' One row per file entry of a \code{\link{DTA}}: which container the entry's
#' file must arrive inside, taken from the entry's own \code{container}
#' reference or, when it has none, from the document's default container.
#' Containers no entry belongs to are not rows; they are in
#' \code{x@containers}.
#' @param x A \code{\link{DTA}} object.
#' @return A data frame with one row per file entry and the columns
#'   \describe{
#'     \item{dataset}{The dataset's id.}
#'     \item{handler}{The entry's index in the dataset (integer).}
#'     \item{expected}{The entry's file name, or its pattern description
#'       (else its pattern) for a pattern entry; several names are joined
#'       with \code{", "}.}
#'     \item{reference}{The entry's own \code{container} value, \code{NA} when
#'       it has none.}
#'     \item{container}{The container it must arrive inside: the id of a
#'       defined container, or the file name of an implied one (a file name
#'       that no defined container carries); \code{NA} when none is required.}
#'     \item{filename}{The file name (glob) of that container, \code{NA} when
#'       none is required.}
#'     \item{default}{\code{TRUE} when the assignment comes from the default
#'       container.}
#'     \item{implied}{\code{TRUE} when the container is implied by a file name
#'       rather than defined.}
#'   }
#' @examples
#' dta <- DTA(
#'   datasets = list(
#'     DTADataSetTabular(
#'       id = "ae",
#'       specs = create_example_DTAColumnSpecCollection(1),
#'       files = list(DTAFileCSV("ae.csv", container = "sdtm"))
#'     ),
#'     DTADataSetTabular(
#'       id = "dm",
#'       specs = create_example_DTAColumnSpecCollection(1),
#'       files = list(DTAFileCSV("dm.csv", container = "none"))
#'     )
#'   ),
#'   metadata = create_example_DTAMetaData(),
#'   containers = list(DTAContainer("sdtm", "sdtm_*.zip", default = TRUE))
#' )
#' container_assignments(dta)
#' @export
container_assignments <- function(x) {
  if (!inherits(x, "DTAtools::DTA")) {
    cli::cli_abort("{.arg x} must be a {.cls DTA} object.")
  }
  containers <- dta_containers(x)

  cols <- list(
    dataset = character(), handler = integer(), expected = character(),
    reference = character(), container = character(), filename = character(),
    default = logical(), implied = logical()
  )
  for (ds in x@datasets) {
    for (i in seq_along(ds@files)) {
      h <- ds@files[[i]]
      ref <- dta_handler_container(h)
      info <- dta_effective_container(containers, h)
      expected <- if (isTRUE(h@pattern) && !is.null(h@pattern_description)) {
        h@pattern_description
      } else {
        h@filename
      }
      cols$dataset <- c(cols$dataset, ds@id)
      cols$handler <- c(cols$handler, i)
      cols$expected <- c(cols$expected, paste(expected, collapse = ", "))
      cols$reference <- c(cols$reference, if (is.null(ref)) NA_character_ else ref)
      cols$container <- c(cols$container, if (is.null(info)) NA_character_ else info$key)
      cols$filename <- c(cols$filename, if (is.null(info)) NA_character_ else info$filename)
      cols$default <- c(cols$default, is.null(ref) && !is.null(info))
      cols$implied <- c(cols$implied, !is.null(info) && isTRUE(info$implied))
    }
  }
  cols$handler <- as.integer(cols$handler)
  do.call(data.frame, c(cols, stringsAsFactors = FALSE))
}
