#' @title Files Not Verified Inside Their Container
#' @description
#' Judges the packaging of every file bound in a \code{\link{DTA}}: did it
#' arrive inside the container its entry requires? This is a verdict of its
#' own, separate from the content verdict of \code{validation_status()}; the
#' content of a file is validated wherever it came from.
#'
#' A row is one file that is not known to be inside its container, with one of
#' three statuses:
#' \describe{
#'   \item{\code{outside}}{Delivered with \code{\link{load_delivery}()} loose,
#'     or inside another container. Fails the run ("Validation FAILED").}
#'   \item{\code{unverified}}{Loaded on its own with \code{\link{load_file}()},
#'     so its packaging was never seen. Leaves the run incomplete
#'     ("Validation INCOMPLETE").}
#'   \item{\code{unrecorded}}{Bound without a record of how it arrived (a
#'     table assigned to \code{@tables} directly) in a dataset that has an
#'     entry with a container. Leaves the run incomplete.}
#' }
#' A file that arrived inside its container, and any file whose entries need no
#' container, produce no row.
#' @param x A \code{\link{DTA}} object. An object restored from an older
#'   version is upgraded first.
#' @return A data frame with the character columns \code{dataset},
#'   \code{item} (table name or file path), \code{file} (the delivered file
#'   name, \code{NA} when unrecorded), \code{container} (the container file
#'   name or pattern the entry requires), \code{arrived} (\code{"loose"},
#'   \code{"on its own"}, the container chain joined with \code{" > "}, or
#'   \code{NA}), \code{status} and \code{message}. Zero rows when there is
#'   nothing to report.
#' @seealso \code{\link{check}()}, \code{\link{load_delivery}()},
#'   \code{\link{container_assignments}()}
#' @examples
#' dta <- DTA(
#'   datasets = list(DTADataSetTabular(
#'     id = "ae",
#'     specs = create_example_DTAColumnSpecCollection(1),
#'     files = list(DTAFileCSV("ae.csv"))
#'   )),
#'   containers = list(DTAContainer("sdtm", "sdtm_*.zip", default = TRUE))
#' )
#' # A file read on its own was never seen inside its container.
#' ae_csv <- file.path(tempdir(), "ae.csv")
#' writeLines(c("a", "1"), ae_csv)
#' dta <- load_file(dta, dataset = "ae", file = ae_csv)
#' container_findings(dta)[, c("dataset", "status", "message")]
#' unlink(ae_csv)
#' @export
container_findings <- function(x) {
  if (!inherits(x, "DTAtools::DTA")) {
    cli::cli_abort("{.arg x} must be a {.cls DTA} object.")
  }
  if (dta_is_stale(x)) x <- dta_upgrade(x)

  cols <- c("dataset", "item", "file", "container", "arrived", "status", "message")
  slots <- dta_delivery_slots(x)
  slot_ds <- vapply(slots, `[[`, "", "dataset")
  by_ds <- split(slots, factor(slot_ds, levels = names(x@datasets)))
  # Declared-container test, shared by all datasets and cached per file name.
  glob_slots <- Filter(function(s) !is.null(s$glob), slots)
  declared_of <- new.env(parent = emptyenv())
  declared <- function(name) {
    if (is.null(declared_of[[name]])) declared_of[[name]] <- dta_delivery_declares(glob_slots, name)
    declared_of[[name]]
  }
  rows <- list()

  for (id in names(x@datasets)) {
    mine <- by_ds[[id]]
    globs <- unique(unlist(lapply(mine, function(s) s$glob)))
    # No entry of this dataset needs a container: nothing to judge.
    if (!length(globs)) next
    ds <- x@datasets[[id]]
    prov <- dta_provenance(ds)
    is_file <- inherits(ds, "DTAtools::DTADataSetFile")
    items <- if (is_file) {
      as.character(ds@file_paths)
    } else {
      nm <- names(tryCatch(ds@tables, error = function(e) NULL))
      if (is.null(nm)) character() else as.character(nm)
    }
    handlers <- if (is_file) ds@file_handlers else integer()
    aligned <- is_file && length(handlers) == length(items)
    best_of <- list()

    for (k in seq_along(items)) {
      item <- items[[k]]
      rec <- prov[[item]]
      # A record without a delivered file name says nothing about arrival.
      if (!is.null(rec) && !(is.character(rec$file) && length(rec$file) == 1L && !is.na(rec$file))) {
        rec <- NULL
      }
      if (is.null(rec)) {
        rows[[length(rows) + 1L]] <- list(
          dataset = id, item = item, file = NA_character_,
          container = if (length(globs) == 1L) globs else NA_character_,
          arrived = NA_character_, status = "unrecorded",
          message = sprintf(
            "%s (dataset '%s'): it is not recorded how this file arrived; load it with load_file() or load_delivery() to judge its container.",
            basename(item), id
          )
        )
        next
      }
      chain <- as.character(rec$container)
      name <- rec$file
      # Believe the recorded entry only while it still exists and still takes the
      # file (entries edited after loading leave stale indices); else any slot.
      hk <- NA_integer_
      if (aligned && !is.na(handlers[[k]]) && handlers[[k]] >= 1L && handlers[[k]] <= length(ds@files) &&
        isTRUE(any(matches_filename(ds@files[[handlers[[k]]]], basename(item))))) {
        hk <- as.integer(handlers[[k]])
      }
      key <- paste(name, paste(chain, collapse = "\r"), hk, sep = "\n")
      if (is.null(best_of[[key]])) {
        cand <- list(name = name, chain = chain)
        # As in routing: an archive no entry declares as its container may be a
        # misfit deliverable.
        if (!is.na(dta_container_kind(name))) cand$declared <- declared(name)
        pool <- if (is.na(hk)) mine else Filter(function(s) identical(s$handler, hk), mine)
        best <- dta_delivery_best_slot(pool, cand)
        if (is.null(best) && !is.na(hk)) best <- dta_delivery_best_slot(mine, cand)
        best_of[[key]] <- if (is.null(best)) FALSE else best
      }
      best <- best_of[[key]]
      if (isFALSE(best) || !isFALSE(best$fit)) next
      glob <- best$slot$glob
      if (identical(rec$via, "file") && length(chain) == 0L) {
        status <- "unverified"
        arrived <- "on its own"
        msg <- sprintf(
          "%s (dataset '%s') was checked on its own; the agreement requires it inside '%s', so its packaging is not verified.",
          name, id, glob
        )
      } else {
        status <- "outside"
        arrived <- if (length(chain)) paste(chain, collapse = " > ") else "loose"
        case <- if (dta_glob_matches(glob, c(chain, name), ignore.case = TRUE)) " (letter case must match)" else ""
        msg <- if (length(chain)) {
          sprintf(
            "%s (dataset '%s') arrived inside '%s'; the agreement requires it inside '%s'%s.",
            name, id, arrived, glob, case
          )
        } else {
          sprintf(
            "%s (dataset '%s') arrived loose; the agreement requires it inside '%s'%s.",
            name, id, glob, case
          )
        }
      }
      rows[[length(rows) + 1L]] <- list(
        dataset = id, item = item, file = name, container = glob,
        arrived = arrived, status = status, message = msg
      )
    }
  }

  if (!length(rows)) {
    return(as.data.frame(
      stats::setNames(rep(list(character()), length(cols)), cols),
      stringsAsFactors = FALSE
    ))
  }
  as.data.frame(
    stats::setNames(lapply(cols, function(cn) vapply(rows, function(r) r[[cn]], "")), cols),
    stringsAsFactors = FALSE
  )
}
