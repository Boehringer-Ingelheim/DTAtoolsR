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
  if (dta_is_stale(x)) x <- dta_upgrade(x)
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


# ---- upgrading objects restored from an older version ---------------------------
# An S7 object read back with readRDS() keeps the class it was SAVED with
# (`attr(x, "S7_class")`): reading or writing a property that class lacks fails
# ("Can't find property"), and assignments run the OLD validator. So objects
# restored from an older version are re-stamped with the current class.

# The class of this package's namespace that `obj` was saved as, or NULL when
# `obj` is not one of ours. NOT S7::S7_class(obj), which returns the stored one.
dta_current_class <- function(obj) {
  stored <- attr(obj, "S7_class", exact = TRUE)
  if (!inherits(stored, "S7_class") || !identical(stored@package, "DTAtools")) {
    return(NULL)
  }
  cur <- get0(stored@name, envir = asNamespace("DTAtools"), inherits = FALSE)
  if (inherits(cur, "S7_class")) cur else NULL
}

# The `class()` S7 gives a fresh object of `cls`: "pkg::name" up the parent
# chain, then "S7_object". (S7:::class_dispatch is internal.)
dta_class_chain <- function(cls) {
  out <- character()
  while (inherits(cls, "S7_class") && !identical(cls@name, "S7_object")) {
    out <- c(out, paste(c(cls@package, cls@name), collapse = "::"))
    cls <- cls@parent
  }
  c(out, "S7_object")
}

# TRUE when `obj` (a DTA or a dataset) lacks a property its CURRENT class
# defines. Entry points (load_file(), check(), load_delivery(), ...) upgrade only
# then. A restored 0.27.0 object always qualifies (no `containers`, no
# `provenance`), so its handlers get re-stamped with it.
#
# A future change that touches ONLY a validator -- no new property -- is
# invisible to this test, and a restored object would keep running the old
# validator. Such a change must add a property or bump a schema marker so that
# this function sees it.
dta_is_stale <- function(obj) {
  # attr() on the class objects, not `@`: S7's property access costs ~10x more
  # and this runs at every entry point. (No identical() shortcut on classes:
  # comparing two class objects is slower than the property scan.)
  lacks <- function(o) {
    stored <- attr(o, "S7_class", exact = TRUE)
    if (!inherits(stored, "S7_class") || !identical(attr(stored, "package", exact = TRUE), "DTAtools")) {
      return(FALSE)
    }
    cur <- get0(attr(stored, "name", exact = TRUE), envir = asNamespace("DTAtools"), inherits = FALSE)
    inherits(cur, "S7_class") &&
      length(setdiff(names(attr(cur, "properties", exact = TRUE)), names(attr(stored, "properties", exact = TRUE)))) > 0L
  }
  if (!inherits(obj, "S7_object")) {
    return(FALSE)
  }
  if (lacks(obj)) {
    return(TRUE)
  }
  if (inherits(obj, "DTAtools::DTA")) {
    return(any(vapply(obj@datasets, function(d) inherits(d, "S7_object") && lacks(d), logical(1))))
  }
  FALSE
}

# Re-stamps EVERY DTAtools S7 object inside `x` with its current class,
# children first, UNCONDITIONALLY (a restored handler must also get the current
# validator). Missing properties get their defaults. Only the DTA and each
# dataset are then validated: validating every object costs ~14x as much.
dta_upgrade <- function(x) {
  walk <- function(obj) {
    if (is.environment(obj) || is.data.frame(obj)) {
      return(obj) # Arrow R6 objects, frames
    }
    if (inherits(obj, "S7_object")) {
      cur <- dta_current_class(obj)
      if (is.null(cur)) {
        return(obj)
      }
      stored <- attr(obj, "S7_class", exact = TRUE)
      for (p in names(cur@properties)) {
        v <- attr(obj, p, exact = TRUE)
        if (is.list(v) || inherits(v, "S7_object")) {
          w <- walk(v)
          if (!identical(w, v)) attr(obj, p) <- w # raw set: validated at the end
        }
      }
      if (!identical(stored, cur)) { # pointer-equal when already current
        for (p in setdiff(names(cur@properties), names(stored@properties))) {
          d <- cur@properties[[p]]$default
          if (!is.null(d)) attr(obj, p) <- d
        }
        attr(obj, "S7_class") <- cur
        # `class(obj)` is the dispatch chain of the class it was saved with:
        # rebuild it from the current parent chain as S7 does for a new object,
        # so a changed hierarchy (a new intermediate class) dispatches correctly.
        class(obj) <- dta_class_chain(cur)
      }
      return(obj)
    }
    if (is.list(obj) && length(obj)) {
      for (i in seq_along(obj)) {
        v <- obj[[i]]
        if (is.list(v) || inherits(v, "S7_object")) {
          w <- walk(v)
          if (!identical(w, v)) obj[[i]] <- w
        }
      }
    }
    obj
  }
  out <- walk(x)
  if (inherits(out, "DTAtools::DTA")) {
    S7::validate(out)
    for (d in out@datasets) S7::validate(d)
  } else if (inherits(out, "DTAtools::DTADataSet")) {
    S7::validate(out)
  }
  out
}

#' @title Upgrade an Object Restored From an Older Version
#' @description
#' A \code{\link{DTA}} or dataset saved with \code{saveRDS()} by an older
#' version of DTAtools keeps the class definition it was saved with, which may
#' lack properties the current version has. \code{upgrade_dta()} re-creates
#' every DTAtools object inside \code{x} with the current class definition:
#' properties that are missing get their defaults, the rest is kept as it is.
#' The functions that read or write such an object (\code{load_file()},
#' \code{check()}, \code{load_delivery()}, \code{container_assignments()})
#' do this by themselves; call \code{upgrade_dta()} when you want to inspect or
#' modify the object's properties directly.
#' @param x A \code{\link{DTA}} or a dataset (\code{DTADataSet}).
#' @return \code{x}, with the current class definition. An object that is
#'   current already comes back with identical property values.
#' @examples
#' dta <- create_example_DTA()
#' dta2 <- upgrade_dta(dta)
#' identical(dta2@datasets, dta@datasets)
#' @export
upgrade_dta <- function(x) {
  if (!inherits(x, "DTAtools::DTA") && !inherits(x, "DTAtools::DTADataSet")) {
    cli::cli_abort("{.arg x} must be a {.cls DTA} or a {.cls DTADataSet} object.")
  }
  dta_upgrade(x)
}
