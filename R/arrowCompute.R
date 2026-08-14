# Package-local cache for the (non-trivial) arrow capability check, so that
# `dta_arrow_compute_available()` does not repeatedly pay the cost of
# `arrow::arrow_info()`. Kept in its own environment (parent = emptyenv())
# rather than an option, because it caches a *capability of the running
# process*, not user-facing configuration.
`__dta_arrow_cache__` <- new.env(parent = emptyenv())

#' @title Reset the Cached Arrow Capability Check
#' @description
#' Clears the memoised result of [dta_arrow_compute_available()] so that the
#' next call re-evaluates `arrow::arrow_info()`. Used only by tests, which
#' need to simulate arrow being available/unavailable across cases.
#' @return `NULL`, invisibly.
#' @keywords internal
dta_arrow_reset_cache <- function() {
  if (exists("available", envir = `__dta_arrow_cache__`)) {
    rm("available", envir = `__dta_arrow_cache__`)
  }
  invisible(NULL)
}

#' @title Is Arrow's Dataset/Acero Engine Available?
#' @description
#' Checks whether the `arrow` package is installed *and* was built with
#' dataset/Acero support (a minimal libarrow build can lack this). The result
#' is memoised per session because `arrow::arrow_info()` is not free.
#'
#' `arrow` is a HARD dependency of `DTAtools` -- it is listed under
#' `Imports:` in `DESCRIPTION`, so it is always installed alongside this
#' package. The `requireNamespace("arrow", quietly = TRUE)` guard below is
#' therefore not a "treat arrow as optional" escape hatch; it exists only so
#' that a partial or minimal-libarrow installation (one that lacks
#' dataset/Acero support, or in the pathological case is broken/incomplete)
#' degrades to `FALSE` here instead of erroring. Do not read this guard as
#' license to move `arrow` to `Suggests:`.
#' @return A single, non-`NA` logical.
#' @keywords internal
dta_arrow_compute_available <- function() {
  if (exists("available", envir = `__dta_arrow_cache__`)) {
    return(get("available", envir = `__dta_arrow_cache__`))
  }

  available <- tryCatch(
    {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        FALSE
      } else {
        isTRUE(arrow::arrow_info()$capabilities[["dataset"]])
      }
    },
    error = function(e) FALSE
  )

  assign("available", available, envir = `__dta_arrow_cache__`)
  available
}

#' @title Should Arrow Compute Be Used?
#' @description
#' Arrow compute is strictly opt-in (default `FALSE`). The R implementation is
#' the reference implementation for every rule check; gating Arrow behind an
#' explicit option means that a future arrow release which changes kernel
#' semantics (e.g. floating point rounding, NA handling) can never silently
#' change a validation verdict for a user who has not opted in.
#' @return A single, non-`NA` logical.
#' @keywords internal
dta_use_arrow_compute <- function() {
  isTRUE(getOption("DTAtools.use_arrow_compute", FALSE)) &&
    dta_arrow_compute_available()
}

#' @title Count Duplicate Rows for a Key
#' @description
#' Returns the number of duplicate rows for the given key columns, i.e. rows
#' beyond the first occurrence of each distinct key -- exactly
#' `sum(duplicated(df[, cols, drop = FALSE]))`.
#' @param df A data.frame.
#' @param cols Character vector of key column names.
#' @return A single integer.
#' @keywords internal
dta_count_duplicates <- function(df, cols) {
  # `dta_unique_key()` (R/streamingValidation.R) length-prefixes each part and
  # maps NA to a sentinel, which is much faster than the data.frame method of
  # `duplicated()` -- but it goes through `as.character()`, and *any* type
  # whose distinct values can render to the same string is at risk of
  # colliding on the same key string, not just doubles: a double only
  # round-trips through `as.character()` to about 15 significant digits (two
  # doubles that `==` reports as different, e.g. `0.1 + 0.2` and `0.3`, can
  # collide), and the same kind of risk applies to Dates, POSIXt, complex,
  # list columns, or any type this code does not specifically recognise as
  # safe. Rather than enumerate the unsafe types, the fast key path is used
  # only for types *provably* safe to render as character -- character,
  # factor, integer, and logical -- and everything else falls back to the
  # data.frame method of `duplicated()`, which keeps that case exactly as
  # precise as it is today. The streaming path (`dta_rule_stream_update()`)
  # already keys every type, including doubles, the same way regardless --
  # that drift between the two paths for an unsafe key column is pre-existing
  # and deliberately not addressed here.
  is_safe_key_type <- function(x) {
    is.character(x) || is.factor(x) || is.integer(x) || is.logical(x)
  }
  all_safe <- all(vapply(df[cols], is_safe_key_type, logical(1)))

  if (
    dta_use_arrow_compute() &&
      all_safe &&
      nrow(df) >= getOption("DTAtools.arrow_min_rows", 100000L)
  ) {
    arrow_count <- tryCatch(
      {
        key_cols <- df[, cols, drop = FALSE]
        tbl <- arrow::as_arrow_table(key_cols)
        # `nrow()` on a still-lazy arrow query returns `NA` (it does not know
        # the row count without executing the query), so the result must be
        # materialised with `dplyr::collect()` before counting rows.
        distinct_rows <- dplyr::collect(
          dplyr::summarise(
            dplyr::group_by(tbl, dplyr::across(dplyr::all_of(cols))),
            .groups = "drop"
          )
        )
        as.integer(nrow(df) - nrow(distinct_rows))
      },
      error = function(e) NULL
    )
    if (!is.null(arrow_count)) {
      return(arrow_count)
    }
  }

  duplicated_rows <- if (all_safe) {
    duplicated(dta_unique_key(df, cols))
  } else {
    duplicated(df[, cols, drop = FALSE])
  }

  sum(duplicated_rows, na.rm = TRUE)
}

#' @title Set (or Inspect) the Thread Count Used by Arrow Compute
#' @description
#' `DTAtools` never spawns its own R-level worker processes for rule
#' evaluation; when the opt-in Arrow compute path
#' (`options(DTAtools.use_arrow_compute = TRUE)`) is active, parallelism comes
#' entirely from arrow's already-multi-threaded C++ kernels. This function
#' exposes that setting rather than introducing a separate parallel backend.
#'
#' Calling it with `n = NULL` only reports the current value and changes
#' nothing -- this is the default so that simply loading or checking the
#' package never mutates global state.
#' @details
#' Arrow-accelerated rule evaluation is controlled by two user-facing options,
#' both unset (and therefore defaulted) unless a caller explicitly sets them:
#'
#' * `options(DTAtools.use_arrow_compute = TRUE/FALSE)` -- default `FALSE`.
#'   Arrow compute is strictly opt-in: the plain R implementation is the
#'   *reference* implementation for every rule check, so a user who never sets
#'   this option always gets the R result, unaffected by which version of
#'   `arrow` (or libarrow) happens to be installed. Set to `TRUE` to allow
#'   rule evaluation (e.g. [rule_check_unique()]'s duplicate counting) to use
#'   Arrow's compute kernels when it is safe and worthwhile to do so.
#' * `options(DTAtools.arrow_min_rows = <integer>)` -- default `100000L`. Even
#'   with Arrow compute enabled, the Arrow path is only used once a table has
#'   at least this many rows; below that, the R path is used regardless,
#'   because Arrow's dispatch overhead is not worth paying on small tables.
#'
#' Both options only ever affect internal compute *paths*; a table's
#' validation verdict does not depend on either option (the R and Arrow paths
#' are tested to agree), which is what makes Arrow compute safe to enable
#' selectively for performance without changing results.
#' @param n `NULL` (default, report only), or a single positive whole number
#'   giving the number of threads arrow's compute kernels should use.
#' @return The *previous* thread count, invisibly, as reported by
#'   `arrow::cpu_count()`. `NA_integer_` (invisibly) if the `arrow` package is
#'   not available, with a warning.
#' @examples
#' # Guarded: a no-op if arrow is not installed. CRAN check machines are
#' # limited to 2 cores, so this never requests more than that, and it
#' # restores the previous setting before returning.
#' if (requireNamespace("arrow", quietly = TRUE)) {
#'   old <- set_dta_compute_threads()
#'   set_dta_compute_threads(2L)
#'   set_dta_compute_threads(old)
#' }
#' @export
set_dta_compute_threads <- function(n = NULL) {
  if (!is.null(n)) {
    if (
      !is.numeric(n) ||
        length(n) != 1L ||
        is.na(n) ||
        n != as.integer(n) ||
        n <= 0
    ) {
      cli::cli_abort(
        "{.arg n} must be {.code NULL} or a single positive whole number.",
        class = "dta_invalid_thread_count"
      )
    }
  }

  if (!requireNamespace("arrow", quietly = TRUE)) {
    cli::cli_warn(
      "The {.pkg arrow} package is not available; thread count unchanged."
    )
    return(invisible(NA_integer_))
  }

  previous <- arrow::cpu_count()

  if (!is.null(n)) {
    arrow::set_cpu_count(as.integer(n))
  }

  invisible(previous)
}
