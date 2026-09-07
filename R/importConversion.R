#' @title Maximum Characters Retained From an Offending Value
#' @description
#' An import issue keeps the source text verbatim so the original value is not
#' lost, but a single pathological cell must not be able to grow the validation
#' artifact without bound. The retained text is truncated at this many
#' characters; the error *count* is never truncated.
#' @include DTAColumnSpecStructureSAS-class.R
#' @keywords internal
dta_import_raw_max_chars <- 200L

#' @title Maximum Import Issues Retained Per Column
#' @description
#' A column that is entirely the wrong type produces one issue per row. Only
#' this many rows are retained per column, so a wholly mistyped column costs a
#' bounded amount of memory. The error *count* stays exact, which is what
#' `import_valid` and therefore `ok` are derived from -- truncation can never
#' turn a failing table into a passing one.
#' @keywords internal
dta_import_max_rows_per_column <- 10000L

#' @title Attribute Name Carrying a Table's Content Stamp
#' @description
#' The R attribute under which [dta_coerce_table_to_specs()] records a digest of
#' the table it produced, so that `dta_table_change_signal()` can identify that
#' table without hashing it a second time.
#'
#' The typed table is already fully materialised in R at the moment the import
#' choke point runs, so the digest is taken there for the price of one in-memory
#' serialisation of a frame that was going to be built anyway. Read back off the
#' table, the same answer then costs nothing at all -- where deriving it later
#' means materialising and serialising the whole table again, which on a large
#' table is more expensive than validating it.
#'
#' An R attribute on the arrow object, and deliberately NOT the table's schema
#' metadata. Schema metadata is *carried forward* by every Arrow operation --
#' `arrow::concat_tables()`, `Table$Slice()`, `[`, `dplyr::filter() |>
#' compute()` -- so a table changed by any of them kept the stamp of the table
#' it was derived from, and `check()` skipped it with the stale verdict. (A
#' three-row table concatenated with itself, every id now duplicated, reported
#' status `skipped` and `ok = TRUE`.) The stamp must identify THIS object's
#' contents, so it rides on the object: an arrow table is an R6 object, i.e. an
#' environment, so the attribute is shared by every reference to it and absent
#' from anything Arrow builds anew.
#' @keywords internal
dta_table_hash_key <- "dta_table_hash"

#' @title Stamp an Arrow Table With a Digest of Its Contents
#' @description
#' Records `hash` on the table under [dta_table_hash_key] and returns it.
#'
#' An arrow table is an R6 object -- an environment -- so this attaches the
#' stamp to the object itself rather than copying it: every reference to that
#' table sees the stamp, and only that table carries it. A table Arrow builds
#' anew from it (`concat_tables()`, `Slice()`, `[`, a computed `dplyr` query)
#' is a different object and carries no stamp, which is exactly the required
#' behaviour -- its contents are not the contents that were hashed.
#' @param table An Arrow table, or any other object (returned unchanged).
#' @param hash A length-1 character digest.
#' @return The stamped table, or `table` unchanged when it is not an Arrow table
#'   or the stamp could not be applied.
#' @keywords internal
dta_stamp_table_hash <- function(table, hash) {
  if (!inherits(table, "ArrowTabular")) {
    return(table)
  }
  if (!is.character(hash) || length(hash) != 1 || is.na(hash) || !nzchar(hash)) {
    return(table)
  }

  # A stamp is an optimisation, never a requirement: an arrow build that refuses
  # the attribute yields an unstamped table, which `dta_table_change_signal()`
  # identifies by hashing it, exactly as before this existed.
  tryCatch(
    {
      attr(table, dta_table_hash_key) <- hash
      table
    },
    error = function(e) table
  )
}

#' @title The Content Stamp an Arrow Table Carries
#' @param table An Arrow table, or any other object.
#' @return The stamp recorded by [dta_stamp_table_hash()], or `NULL` when the
#'   table carries none.
#' @keywords internal
dta_table_hash_stamp <- function(table) {
  stamp <- attr(table, dta_table_hash_key, exact = TRUE)

  if (!is.character(stamp) || length(stamp) != 1 || is.na(stamp) || !nzchar(stamp)) {
    return(NULL)
  }

  stamp
}

#' @title Digest of a Table's Contents and Carried Import Issues
#' @description
#' The one content hash both the stamp [dta_stamp_table_hash()] records and the
#' fallback in `dta_table_change_signal()` derive, so that a stamped table and
#' the same table rebuilt from its own `as.data.frame()` hash identically.
#'
#' `rlang::hash()` of a data frame alone cannot do that: it digests the object
#' including its attributes IN ORDER, and the Arrow round trip returns
#' `dta_import_issues` and `class` in the opposite order to the frame the stamp
#' was taken from. Every table carrying import issues therefore hashed one way
#' at the choke point and another when read back, so a rebuilt table was
#' rescanned by `check()` on every run -- the exact cost the stamp exists to
#' avoid.
#'
#' Hashing a canonical list instead removes attribute order from the answer
#' entirely: the column names, the column vectors stripped of their names, and
#' the issues frame reduced the same way plus the exact error count it carries
#' (which is not `nrow()` when the per-column cap truncated it, and must change
#' the signal when it changes).
#' @param df A data.frame, typically carrying a `"dta_import_issues"` attribute.
#' @return A length-1 character digest.
#' @keywords internal
dta_table_content_hash <- function(df) {
  issues <- attr(df, "dta_import_issues", exact = TRUE)

  # `lapply()`, not `as.list()`: `as.list()` on a data.frame keeps the frame's
  # own attributes on the list it returns -- including `dta_import_issues`,
  # which is exactly the attribute whose position moves across the Arrow round
  # trip. `lapply()` builds a fresh list carrying nothing but the columns.
  rlang::hash(list(
    columns = names(df),
    values = unname(lapply(df, identity)),
    issues = if (is.data.frame(issues)) {
      list(
        columns = names(issues),
        values = unname(lapply(issues, identity)),
        n = attr(issues, "n_import_errors", exact = TRUE)
      )
    } else {
      NULL
    }
  ))
}


#' @title Would This Frame Come Back Unchanged From Arrow?
#' @description
#' `TRUE` when every column of `df` is of a type that
#' `as.data.frame(arrow::as_arrow_table(df))` returns identically, so that a
#' digest taken from the frame is also the digest of the Arrow table built from
#' it.
#'
#' The `DTADataSetTabular()` constructor stamps a data-frame table with
#' [dta_table_content_hash()] of the frame it coerced, rather than of the Table
#' it then builds -- which is what lets it convert the data once instead of
#' three times. That is only the same answer while the round trip is lossless,
#' and for three types it is not:
#'
#' * a `bit64::integer64` whose values fit in 32 bits comes back as `integer`
#'   (Arrow's `int64_downcast`);
#' * a `difftime` comes back in seconds whatever units it went in as;
#' * a `POSIXct` carrying `tzone = ""` -- what `as.POSIXct()` leaves when no
#'   timezone is named -- comes back with no `tzone` at all, and after a second
#'   round trip with the session's timezone. That one is not stable under
#'   REPEATED round trips either, so the route this predicate sends it down
#'   only helps while the coercion has nothing to type: when the coercion
#'   rebuilds the table, its stamp is the digest of the first round trip and
#'   `as.data.frame()` of the stored Table is the second. Such a table is
#'   rescanned whenever it is rebuilt from its own contents, on either route.
#'   It is pinned in `test-DTADataSetTabular-validation.R` rather than fixed;
#'   the fix would be Arrow's.
#' * a `Date` or a `POSIXct` held over INTEGER storage -- which
#'   `structure(18262L, class = "Date")` and `.POSIXct(1L, "UTC")` are, and
#'   which nothing in R forbids -- comes back over double storage, because Arrow
#'   has one date32/timestamp type and R's converter builds a double from it.
#'   The class is unchanged and the values are equal, but `identical()` is not,
#'   and `dta_table_content_hash()` digests the storage. Hence the `is.double()`
#'   clause: it is the STORAGE that has to survive, not the class.
#'
#' A frame holding any of them is typed through Arrow instead, so its stamp is
#' taken from the same frame `dta_table_change_signal()` would hash. The cost of
#' being wrong is not a wrong verdict -- the stamp is still a digest of real
#' contents, so it can never claim "unchanged" for data that changed -- but a
#' table rebuilt from its own `as.data.frame()` would hash differently from its
#' stamp and be rescanned on every `check()`, which is the one cost the stamp
#' exists to avoid.
#'
#' The list is a whitelist, not a blacklist: an R type Arrow handles in some way
#' nobody here has checked takes the slower, always-correct route.
#' @param df A data.frame, or any other object (which yields `FALSE`).
#' @return `TRUE` or `FALSE`. A frame with no columns is stable.
#' @keywords internal
dta_frame_is_arrow_stable <- function(df) {
  if (!is.data.frame(df)) {
    return(FALSE)
  }

  stable_column <- function(column) {
    cls <- class(column)

    if (identical(cls, "POSIXct") || identical(cls, c("POSIXct", "POSIXt"))) {
      tz <- attr(column, "tzone", exact = TRUE)
      return(
        is.double(column) &&
          is.character(tz) && length(tz) == 1 && !is.na(tz) && nzchar(tz)
      )
    }

    # Both time classes are checked for double STORAGE, not only for their
    # class: Arrow returns a date32 and a timestamp as doubles whichever
    # storage went in, so an integer-backed `Date` -- which `as.Date()` never
    # produces but `structure(x, class = "Date")` does -- comes back a
    # different vector carrying the same dates.
    if (identical(cls, "Date")) {
      return(is.double(column))
    }

    identical(cls, "character") ||
      identical(cls, "numeric") ||
      identical(cls, "integer") ||
      identical(cls, "logical") ||
      identical(cls, "factor") ||
      identical(cls, c("ordered", "factor"))
  }

  all(vapply(df, stable_column, logical(1)))
}


#' @title Column Spec Structure for One Column
#' @description
#' Looks up the `DTAColumnSpecStructure` a collection declares for a column.
#' The collection is normally named by column id, but a spec built by another
#' route may not be, so the ids themselves are the fallback.
#' @param specs A `DTAColumnSpecCollection`, or `NULL`.
#' @param column Character. Name of the column.
#' @return The structure object, or `NULL` when the collection has no spec for
#'   the column.
#' @keywords internal
dta_spec_column_structure <- function(specs, column) {
  columns <- tryCatch(specs@columns, error = function(e) NULL)

  if (!is.list(columns) || length(columns) == 0) {
    return(NULL)
  }

  index <- match(column, names(columns))

  if (is.na(index)) {
    ids <- vapply(
      columns,
      function(spec) tryCatch(as.character(spec@id)[[1]], error = function(e) NA_character_),
      character(1)
    )
    index <- match(column, ids)
  }

  if (is.na(index)) {
    return(NULL)
  }

  tryCatch(columns[[index]]@structure, error = function(e) NULL)
}


#' @title Target R Type for One Column
#' @description
#' The R type a column should be stored as, according to its spec.
#' @param specs A `DTAColumnSpecCollection`, or `NULL`.
#' @param column Character. Name of the column.
#' @return A length-1 character naming an R type, or `NA_character_` when the
#'   specs say nothing about this column. `NA` means "leave the column exactly
#'   as it was read".
#' @keywords internal
dta_spec_r_type <- function(specs, column) {
  structure <- dta_spec_column_structure(specs, column)

  if (is.null(structure)) {
    return(NA_character_)
  }

  target <- tryCatch(as_r_type(structure), error = function(e) NA_character_)

  if (length(target) != 1 || is.na(target)) {
    return(NA_character_)
  }

  as.character(target)
}


#' @title Compile Every Spec Column's Target Type Once
#' @description
#' A named character vector mapping every spec column key to the R type
#' [dta_spec_r_type()] would return for it, derived once per scan instead of
#' once per column per batch.
#'
#' Mirrors [dta_compile_columnspec_schemas()], but for the coercion axis. A
#' column's target type is a pure function of its spec and does not change
#' while a table is being validated, but deriving it is several
#' `tryCatch()`/S7 hops deep (`dta_spec_column_structure()` walks the
#' collection to find the column's structure, then `as_r_type()` dispatches on
#' that structure). [dta_coerce_table_to_specs()] runs once per batch on the
#' streaming path, so looking the type up per column inside that call repeats
#' the whole derivation once per column per batch across a scan's thousands of
#' batches, to obtain one answer per column every time. Compiling the map once
#' and passing it in makes that cost proportional to the spec rather than to
#' the data.
#' @param specs A `DTAColumnSpecCollection`, or `NULL`.
#' @return A named character vector keyed by every name under which a column
#'   can be looked up (`unique(c(names(columns), ids))`, with `NA`/`""` keys
#'   dropped), with values from [dta_spec_r_type()]. `character(0)` when
#'   `specs` is `NULL` or declares no columns.
#' @keywords internal
dta_compile_spec_types <- function(specs) {
  empty <- character(0)

  if (is.null(specs)) {
    return(empty)
  }

  columns <- tryCatch(specs@columns, error = function(e) NULL)

  if (!is.list(columns) || length(columns) == 0) {
    return(empty)
  }

  ids <- vapply(
    columns,
    function(spec) tryCatch(as.character(spec@id)[[1]], error = function(e) NA_character_),
    character(1),
    USE.NAMES = FALSE
  )

  # A collection is normally named by column id, but one built by another route
  # may not be, so both are offered as keys and deduplicated.
  keys <- unique(c(names(columns), ids))
  keys <- keys[!is.na(keys) & nzchar(keys)]

  if (length(keys) == 0) {
    return(empty)
  }

  values <- vapply(
    keys,
    function(key) dta_spec_r_type(specs, key),
    character(1),
    USE.NAMES = FALSE
  )
  names(values) <- keys
  values
}


#' @title Coerce One Column to Its Declared R Type
#' @description
#' Converts a single column to the type its spec declares, reporting the values
#' that could not be represented.
#'
#' Only numeric targets are converted. Everything else -- `"character"` in
#' particular -- is returned untouched, which is what keeps a `SAS Char`
#' `SUBJECT_ID` of `"007"` from ever being round-tripped through a number.
#'
#' A column that is already numeric is left alone: there is nothing to parse, so
#' no value can fail to parse. This is also what stops the import axis from
#' inventing errors on a clean file.
#'
#' An `Int` target is narrowed to R `integer` only when every value is whole.
#' Rounding a fractional value into an integer column would silently discard the
#' fraction *and* hide the `type: integer` column spec error that exists to report
#' it, so a fractional value stays a double and is left to the column spec axis.
#' @param values A column vector taken from the table.
#' @param target Character. The target R type, from [as_r_type()].
#' @return `NULL` when the column is left untouched, otherwise a list with the
#'   converted `values`, the integer indices of the `offending` values, and
#'   `source` -- the original input vector, from which the raw text of row `i`
#'   is `as.character(source[i])`, i.e. [dta_numeric_raw()].
#' @keywords internal
dta_coerce_column <- function(values, target) {
  if (!isTRUE(target %in% c("double", "integer"))) {
    return(NULL)
  }

  # Nothing to parse: already a number, or already a temporal value carrying its
  # own numeric representation.
  if (is.numeric(values) || inherits(values, "Date") || inherits(values, "POSIXt")) {
    return(NULL)
  }

  # An all-missing column (Arrow reads an empty column as `null`) carries no
  # value that could have failed to convert.
  if (length(values) == 0 || all(is.na(values))) {
    return(NULL)
  }

  converted <- dta_as_numeric_strict(values)

  # `dta_as_numeric_strict()` already yields NA for the unconvertible values,
  # which is exactly the required semantics: the value becomes NA in the typed
  # column and its raw text is retained in the issue below.
  out <- converted$values

  if (identical(target, "integer")) {
    present <- out[!is.na(out)]
    is_whole <- length(present) == 0 ||
      (all(is.finite(present)) &&
        all(present == trunc(present)) &&
        all(abs(present) <= .Machine$integer.max))
    if (is_whole) {
      out <- as.integer(out)
    }
  }

  list(
    values = out,
    offending = which(converted$unconvertible),
    # The input vector itself, not a character rendering of it: the raw text is
    # read at the offending indices only, and rendering the whole column up
    # front cost an order of magnitude more memory than the column.
    source = values
  )
}


#' @title Type a Table by Its Column Specs at Import Time
#' @description
#' The typed import choke point. The declared type of every column is applied to
#' the data as it is read, instead of leaving the reader to infer a type per
#' column.
#'
#' Arrow infers the type of a CSV/TSV column from its contents, so a single
#' unparseable cell in an otherwise numeric column makes the reader fall back to
#' string for *every* row of that column. Every downstream check then sees a
#' column of text where the specification declared a number. Applying the
#' declared type here means the column is a number, one cell is missing, and
#' that one cell is reported.
#'
#' Semantics:
#' * A value that cannot be represented in the declared type becomes `NA` in the
#'   typed column, and its source text is retained verbatim in an import issue
#'   with `reason = "not_convertible"`.
#' * Only unrecoverable values are issues. `"007"` to `7` and `"1.50"` to `1.5`
#'   are clean conversions and are not reported.
#' * A column whose declared type is `Char` is never coerced.
#' * A column present in the table but absent from the specs is left untouched,
#'   not dropped.
#' * `NA`, `""` and whitespace in the source stay missing and are not issues.
#'
#' The issues are returned *and* attached to the returned table as the
#' `"dta_import_issues"` attribute. The carried copy is deliberate:
#' [check()] skips revalidation when the table hash and the specs hash are both
#' unchanged, and that hash is taken from the table itself. Issues living only
#' in the dataset's `import_issues` property would not be hashed, so a table
#' whose import issues had changed could be skipped while still reporting a
#' stale `ok = TRUE`. Riding on the table, they cannot be separated from the
#' data they describe.
#'
#' An Arrow table additionally comes back stamped with a digest of the typed
#' frame -- issues attribute included -- under [dta_table_hash_key], because
#' this is the one moment at which the whole table is materialised in R anyway.
#' `dta_table_change_signal()` reads that stamp instead of re-deriving it, which
#' is what stops identifying a table from costing more than validating it. Both
#' returns carry it: the rebuilt table and the original handed back when nothing
#' needed typing. The digest is [dta_table_content_hash()], which is also what
#' the change signal falls back to for an unstamped table, so the two agree on
#' the same contents.
#' @param table An Arrow Table or a data.frame.
#' @param specs A `DTAColumnSpecCollection`, or `NULL`.
#' @param type_map Named character vector from [dta_compile_spec_types()], or
#'   `NULL`. When supplied, a column's target type is looked up in this map
#'   instead of being freshly derived via [dta_spec_r_type()] -- the same
#'   answer, precomputed once for the whole scan rather than once per column
#'   per batch.
#' @param max_rows_per_column Integer, or `Inf` to retain every offending row
#'   for every column. Cap on the number of per-column import issues retained.
#'   Defaults to `dta_import_max_rows_per_column` so every existing caller is
#'   unaffected. A caller that spills retained detail to disk instead of
#'   holding it in memory passes `Inf`.
#' @return A list with `table` (the typed table, same class as the input, with
#'   the issues attached and -- for an Arrow table -- the content stamp) and
#'   `issues` (a data.frame in the shape of [dta_empty_import_errors()],
#'   carrying the exact error count in its `"n_import_errors"` attribute).
#' @keywords internal
dta_coerce_table_to_specs <- function(table, specs, type_map = NULL, max_rows_per_column = dta_import_max_rows_per_column) {
  was_arrow <- inherits(table, "Table") || inherits(table, "ArrowTabular")
  df <- if (was_arrow) as.data.frame(table) else table

  if (!is.data.frame(df) || ncol(df) == 0 || is.null(names(df))) {
    return(list(table = table, issues = dta_empty_import_errors()))
  }

  parts <- list()
  # Double, not integer: one error per bad cell over every typed column can
  # exceed `.Machine$integer.max` on a wide dirty table, and an integer
  # accumulator silently becomes `NA` there. See `dta_narrow_count()`.
  n_total <- 0
  changed <- FALSE

  for (column in names(df)) {
    # A precompiled map is just dta_spec_r_type(specs, column), looked up
    # rather than re-derived -- see dta_compile_spec_types() for why that
    # matters on the streaming path, where this runs once per batch.
    target <- if (!is.null(type_map)) {
      if (column %in% names(type_map)) type_map[[column]] else NA_character_
    } else {
      dta_spec_r_type(specs, column)
    }

    # No spec for this column: it is left exactly as read, not dropped.
    if (is.na(target)) {
      next
    }

    coerced <- dta_coerce_column(df[[column]], target)

    if (is.null(coerced)) {
      next
    }

    df[[column]] <- coerced$values
    changed <- TRUE

    n_offending <- length(coerced$offending)

    if (n_offending == 0L) {
      next
    }

    # The count is accumulated before the cap is applied, so `ok` is decided by
    # how many values failed, never by how many were retained.
    n_total <- n_total + n_offending
    # A caller wanting everything retained (one that spills detail to disk
    # instead of holding it) passes `max_rows_per_column = Inf`. min() against
    # the always-finite `n_offending` yields `n_offending` itself in that case,
    # so the length handed to seq_len() below is never Inf even though the cap
    # may be.
    n_kept <- min(n_offending, max_rows_per_column)
    kept <- coerced$offending[seq_len(n_kept)]

    declared <- dta_spec_declared_type(specs, column)
    if (is.na(declared)) {
      declared <- target
    }

    parts[[length(parts) + 1L]] <- data.frame(
      row = as.integer(kept),
      column = column,
      raw = substr(dta_numeric_raw(coerced, kept), 1L, dta_import_raw_max_chars),
      declared_type = declared,
      reason = "not_convertible",
      stringsAsFactors = FALSE
    )
  }

  issues <- if (length(parts) == 0) {
    dta_empty_import_errors()
  } else {
    out <- do.call(rbind, parts)
    # method = "radix": the column tiebreak is a character sort, and locale
    # collation ordered this same frame differently on a de_DE dev machine than
    # under CI's C collation. Radix is byte order, identical everywhere.
    out <- out[order(out$row, out$column, method = "radix"), , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  attr(issues, "n_import_errors") <- dta_narrow_count(n_total)

  # Nothing was typed and nothing failed: hand back the original object rather
  # than paying for a round trip that cannot have changed anything.
  if (!changed && nrow(issues) == 0) {
    return(list(
      table = if (was_arrow) dta_stamp_table_hash(table, dta_table_content_hash(df)) else table,
      issues = issues
    ))
  }

  if (nrow(issues) > 0) {
    attr(df, "dta_import_issues") <- issues
  }

  # Stamped AFTER the issues are attached, so the digest covers them. `check()`
  # skips revalidation when the table's signal is unchanged, and issues that did
  # not contribute to the signal could change while a stale `ok = TRUE` stood.
  list(
    table = if (was_arrow) {
      dta_stamp_table_hash(arrow::as_arrow_table(df), dta_table_content_hash(df))
    } else {
      df
    },
    issues = issues
  )
}


# ---- parsing declared-numeric text in Arrow ----------------------------------

# The literal forms on which Arrow's string cast and R's `as.numeric()` return
# the SAME double, bit for bit. Nothing outside them may take the Arrow route:
# a value that differs by one ULP between the two engines is not a rounding
# curiosity here. `dta_row_key()` renders doubles with `%.17g`, so one ULP is
# enough to make a streamed uniqueness verdict disagree with the in-memory one
# on the same file -- which is precisely the class of divergence
# `test-streaming-parity.R` exists to forbid.
#
# What narrows these patterns so far is that R's parser is not correctly
# rounded. `R_strtod()` accumulates the digits into an `LDOUBLE` and then
# scales by a power of ten, so where `LDOUBLE` is x87's 80-bit type the result
# is rounded twice -- once to 64 mantissa bits, once to 53 -- while Arrow's
# parser rounds once. Measured on Windows/x86 while this was written: 20-digit
# integers disagree on about 1 value in 3,500, and 7-significant-digit
# decimals on about 1 in 3,400. Neither engine is wrong. They are different,
# and different is the whole problem.
#
# Two families are immune, on either `LDOUBLE` regime:
#
#   double   At most 15 digits in total, at most 3 of them after the point.
#            The digit string is then an integer p < 10^15 < 2^53, which every
#            arithmetic involved holds exactly, and the value is p / 10^k with
#            k <= 3. Rounding twice can only differ from rounding once when
#            the exact value lies within half a 64-bit ULP of a 53-bit
#            rounding boundary; for a value with denominator 10^k that
#            distance is at least 1/(10^k * 2^(53-E)) against the 2^(E-64) it
#            would have to be under, and the first beats the second exactly
#            when 10^k < 2^11, i.e. when k <= 3. A value landing exactly ON a
#            boundary is held exactly by both engines and rounds half to even
#            in both.
#
#   integer  At most 9 digits, and no sign but `-`. Every such value is whole
#            and within `.Machine$integer.max`, which is exactly when
#            `dta_coerce_column()` narrows the column to R `integer` -- so the
#            Arrow cast to int32 lands on the same vector
#            `as.integer(as.numeric(text))` would have, rather than leaving a
#            double where the R path produced an integer. Arrow's integer
#            parser rejects a leading `+`, so the pattern rejects it too.
#
# Both were checked against R over 1.2 million (double) and 800,000 (integer)
# random literals drawn from exactly these shapes, plus the boundary literals
# `1.`, `.5`, `+4`, `-0`, `-.5`, `007`, `999999999999999` and
# `123456789012.123`; `test-importConversion.R` re-runs that comparison.
# Everything outside them -- an exponent, a fourth decimal, a 16th digit,
# surrounding whitespace, `""`, `0x1F`, `Inf`, `NaN` -- is left to R, which
# either parses it identically but unprovably, parses it differently, or
# records it as unconvertible. Arrow's cast refuses most of them outright.
#
# Anchored with `^`/`$` and evaluated by RE2 inside Arrow. RE2's `$` matches at
# end of text only where PCRE's also matches before a final newline, which
# makes RE2 the stricter of the two: a value R would accept can only ever fall
# OUT of the fast path, never into it.
DTA_ARROW_DOUBLE_PATTERN <- "^[+-]?(?:[0-9]{1,15}[.]?|[0-9]{0,12}[.][0-9]{1,3})$"
DTA_ARROW_INTEGER_PATTERN <- "^-?[0-9]{1,9}$"

#' @title Parse a Batch's Declared-Numeric Text Columns in Arrow
#' @description
#' Typing the batch in R is the largest single cost of a streamed check:
#' `dta_coerce_table_to_specs()` is 58% of a clean scan of the reference file
#' by `Rprof`, nearly all of it `as.numeric()` and `is.na()` over
#' declared-numeric columns the reader pinned to text. Arrow's own cast does
#' the same parse about five times faster per value.
#'
#' It cannot simply be used instead. Arrow 25 has no cast that tolerates a bad
#' value -- one unparseable cell fails the whole column -- and, more
#' importantly, a successful cast is not necessarily R's answer. So the cast is
#' used only where a second Arrow-side test has proved that it is: every
#' non-null value of the column matches one of the patterns above, whose forms
#' R and Arrow are known to agree on bit for bit. Every other column is handed
#' to R exactly as before. The test is not cheap -- it costs about as much per
#' value as the R parse it saves, so what the fast path actually buys is closer
#' to the cast than to the whole parse.
#'
#' The consequence for the caller is that such a column reaches
#' [dta_coerce_table_to_specs()] already numeric, which leaves it alone
#' (`is.numeric()`), so no import issue can be recorded for it. That is the
#' right answer rather than a suppressed one: every value in the column parsed.
#'
#' A column is left to R when it holds no non-null value at all (the R path
#' leaves an all-missing column as text, and a column of `NA` doubles is not
#' the same thing to the column spec axis), when it is not held as `utf8` or
#' `large_utf8`, when any non-null value falls outside the patterns, or when
#' Arrow refuses either operation.
#'
#' `options(DTAtools.stream_arrow_numeric = FALSE)` sends every column down the
#' R path instead. That is a diagnostic switch -- it exists so the two parsers
#' can be compared on one input, and so a suspected disagreement can be ruled
#' in or out without rebuilding the package -- and not a supported way to
#' change a result: if flipping it changes one, that is a defect here.
#'
#' A column whose values do not all match is not retried on the very next
#' batch. The test costs about as much as the R parse it is trying to avoid, so
#' on a file that is dirty throughout, retrying every batch would make the scan
#' measurably SLOWER while saving nothing; and a column with scattered bad
#' values is overwhelmingly likely to have one in the next batch too. The wait
#' doubles with each failure, which bounds the wasted work on a file that is
#' dirty everywhere to a handful of batches while still recovering the fast
#' path, after a short delay, on a file whose only bad value happened to fall
#' in an early batch. A batch the column DOES qualify for resets the wait to
#' one, so the schedule describes how the column is behaving now rather than
#' the worst it has ever behaved. It is a scheduling decision and nothing else:
#' whichever way it goes, the column is typed and its bad values reported.
#' @param batch An Arrow `RecordBatch` (or any `ArrowTabular`). Anything else is
#'   returned unchanged.
#' @param state The environment [dta_arrow_numeric_state()] returned, or `NULL`
#'   to leave the batch alone.
#' @return The batch, with every eligible column replaced by its parsed
#'   `float64` or `int32` form.
#' @keywords internal
dta_arrow_parse_numeric_batch <- function(batch, state) {
  if (is.null(state) || !inherits(batch, "ArrowTabular")) {
    return(batch)
  }

  n_rows <- batch$num_rows
  if (length(n_rows) != 1 || is.na(n_rows) || n_rows == 0) {
    return(batch)
  }
  # A small batch is cheaper to type in R than to hand to the engine; see
  # dta_arrow_numeric_state() for the measurement behind the threshold.
  if (n_rows < state$min_rows) {
    return(batch)
  }

  type_map <- state$type_map
  # Read from the batch rather than carried in the state: a reader is free to
  # yield columns the source schema does not have in the same order, and an
  # index taken from the wrong list would replace the wrong column.
  column_names <- names(batch)

  for (i in seq_along(column_names)) {
    column <- column_names[[i]]
    # The same lookup dta_coerce_table_to_specs() does, so a column this
    # declines to touch is one that function will type in R.
    target <- if (column %in% names(type_map)) type_map[[column]] else NA_character_
    if (is.na(target) || !target %in% c("double", "integer")) {
      next
    }

    wait <- state$wait[[column]]
    if (!is.null(wait) && wait > 0) {
      state$wait[[column]] <- wait - 1
      next
    }

    # Read from the CURRENT batch: `SetColumn()` returns a new object rather
    # than mutating, so earlier iterations have already replaced `batch`.
    col <- batch$column(i - 1L)
    # `class()` rather than `DataType$Equals()`: the comparison is made once
    # per declared-numeric column per batch, and building the two `DataType`
    # objects to compare against is three R6 constructions and two calls into
    # C++ for a question the R class already answers.
    if (!class(col$type)[[1]] %in% c("Utf8", "LargeUtf8")) {
      next
    }

    n_null <- col$null_count
    if (n_null >= n_rows) {
      # No non-null value to parse. The R path leaves such a column as text and
      # this must not differ -- and a later batch may well have values, so this
      # is not a failure and does not count towards the backoff.
      next
    }

    is_integer_target <- identical(target, "integer")
    cast_type <- if (is_integer_target) state$int_type else state$double_type

    # The cast is tried FIRST because it is the cheaper of the two tests --
    # about a fifth of the regex per value -- and it fails almost immediately
    # on a batch with a bad value near the front. On a clean batch its result
    # is the answer; on a dirty one it is how the column is rejected without
    # paying for the match at all.
    cast <- tryCatch(col$cast(cast_type), error = function(e) NULL)

    # The cast succeeding is NOT enough. Arrow parses `1e5`, `Inf` and a
    # 20-digit mantissa perfectly happily, and R parses two of those to a
    # different double. Only the pattern says the two engines agree.
    n_matched <- if (is.null(cast)) {
      NA_real_
    } else {
      tryCatch(
        {
          matched <- arrow::call_function(
            "match_substring_regex", col,
            options = list(
              pattern = if (is_integer_target) {
                DTA_ARROW_INTEGER_PATTERN
              } else {
                DTA_ARROW_DOUBLE_PATTERN
              }
            )
          )
          # Exactly one scalar ever crosses into R. `sum` skips nulls, which is
          # what is wanted: a null is a missing value, not an unparseable one,
          # and the cast returns it as a null.
          as.numeric(as.vector(arrow::call_function("sum", matched)))
        },
        error = function(e) NA_real_
      )
    }

    if (!isTRUE(n_matched == (n_rows - n_null))) {
      backoff <- state$backoff[[column]]
      if (is.null(backoff)) {
        backoff <- 1
      }
      state$wait[[column]] <- backoff
      state$backoff[[column]] <- min(backoff * 2, 1024)
      next
    }

    # The column qualified, so its history of failing is spent. Without this the
    # wait only ever doubles: a column with one bad value in an early batch and
    # none thereafter kept its 2, 4, 8 ... schedule for the rest of the scan and
    # was skipped for most of the batches it would have qualified for. The
    # backoff exists to bound wasted work on a file that is dirty EVERYWHERE,
    # and such a file never reaches this line.
    state$backoff[[column]] <- 1

    # The replacement field is (name, type), and both are fixed for the whole
    # scan, so it is built once per column rather than once per column per
    # batch -- one more Arrow R6 object that would otherwise be constructed
    # thousands of times to say the same thing.
    field <- state$fields[[column]]
    if (is.null(field)) {
      field <- arrow::field(column, cast_type)
      state$fields[[column]] <- field
    }

    batch <- tryCatch(batch$SetColumn(i - 1L, field, cast), error = function(e) batch)
  }

  batch
}


#' @title Per-Scan State for the Arrow Numeric Fast Path
#' @description
#' Everything [dta_arrow_parse_numeric_batch()] needs that does not change from
#' batch to batch: the compiled target types, the two Arrow `DataType` objects
#' the cast uses, the replacement `Field` of each column once it has been seen,
#' and the per-column backoff counters. Built once per scan because
#' constructing an Arrow object is an R6 construction and a hop into C++, and
#' this path runs once per declared-numeric column per batch, for every batch
#' of a file of any size.
#'
#' Returns `NULL` -- which the batch function treats as "do nothing" -- when the
#' diagnostic switch `DTAtools.stream_arrow_numeric` is off, or when the specs
#' declare no numeric column at all, so that the whole fast path costs a single
#' `is.null()` per batch in the cases where it can do nothing.
#'
#' The state also carries `min_rows`, from
#' `options(DTAtools.stream_arrow_numeric_min_rows)` (20,000 by default): a
#' batch with fewer rows is typed in R. Each Arrow step is a fixed-cost call
#' per column per batch, so on the small batches a 1 MiB read block yields the
#' path costs more than the parse it saves (measured 34% slower at 1 MiB
#' blocks, 18% faster at 8 MiB, 26% faster at 32 MiB on a 1e6 x 20 file).
#' @param type_map Named character vector from [dta_compile_spec_types()].
#' @return An environment, or `NULL`.
#' @keywords internal
dta_arrow_numeric_state <- function(type_map) {
  if (!isTRUE(getOption("DTAtools.stream_arrow_numeric", TRUE))) {
    return(NULL)
  }
  if (length(type_map) == 0 || !any(type_map %in% c("double", "integer"))) {
    return(NULL)
  }

  # Below this many rows a batch is typed in R. Each Arrow step here is a
  # fixed-cost call into the engine per column per batch, and at the default
  # 1 MiB read block a delimited batch is only a few thousand rows: measured
  # on a 1e6 x 20 file, the Arrow path was 34% SLOWER at 1 MiB blocks and 18%
  # faster at 8 MiB (about 50,000 rows per batch), 26% faster at 32 MiB. The
  # threshold makes the step a no-op exactly where it cannot pay for itself.
  min_rows <- getOption("DTAtools.stream_arrow_numeric_min_rows", 20000L)
  if (
    !is.numeric(min_rows) || length(min_rows) != 1 || is.na(min_rows) ||
      min_rows < 0 || min_rows != trunc(min_rows)
  ) {
    cli::cli_abort(
      "{.code options(DTAtools.stream_arrow_numeric_min_rows)} must be a single non-negative whole number, not {.val {min_rows}}."
    )
  }

  state <- new.env(parent = emptyenv())
  state$type_map <- type_map
  state$min_rows <- as.numeric(min_rows)
  state$double_type <- arrow::float64()
  state$int_type <- arrow::int32()
  state$fields <- list()
  state$wait <- list()
  state$backoff <- list()
  state
}


#' @title Import Issues Carried by a Table
#' @description
#' Reads the import issues that [dta_coerce_table_to_specs()] attached to a
#' table. Survives the Arrow round trip, because Arrow stores R attributes in
#' the schema metadata and restores them on `as.data.frame()`.
#' @param table A data.frame (typically the one `check()` materialised from the
#'   Arrow table).
#' @return A data.frame in the shape of [dta_empty_import_errors()], or `NULL`
#'   when the table carries none.
#' @keywords internal
dta_carried_import_issues <- function(table) {
  issues <- attr(table, "dta_import_issues", exact = TRUE)

  if (!is.data.frame(issues) || nrow(issues) == 0) {
    return(NULL)
  }

  issues
}


#' @title Exact Import Error Count of an Issue Frame
#' @description
#' The number of values that failed to convert, which is not `nrow()` when the
#' per-column cap truncated the retained rows.
#' @param issues A data.frame in the shape of [dta_empty_import_errors()], or
#'   `NULL`.
#' @return A length-1 count: an integer, or a double when the count exceeds
#'   `.Machine$integer.max` (see `dta_narrow_count()`).
#' @keywords internal
dta_import_error_count <- function(issues) {
  if (!is.data.frame(issues)) {
    return(0L)
  }

  n <- attr(issues, "n_import_errors", exact = TRUE)

  if (is.null(n) || length(n) != 1 || !is.numeric(n) || is.na(n)) {
    return(as.integer(nrow(issues)))
  }

  # The retained rows are the floor: a frame can never report fewer errors than
  # the rows it actually carries. Compared and returned without an `as.integer()`
  # round trip, which turned a count above `.Machine$integer.max` into `NA` --
  # and `NA` here is read as "no attribute", falling back to the *capped*
  # `nrow()` and under-reporting by however much the cap threw away.
  dta_narrow_count(max(n, nrow(issues)))
}


#' @title Merge the Import-Time and Rule-Time Import Errors
#' @description
#' Combines the errors detected while typing the table with those the rule layer
#' found while reading a column as a number.
#'
#' After the import choke point the two rarely overlap: a value the import layer
#' could not represent is already `NA` by the time a rule sees it, so the rule
#' layer reports it only for columns the import layer does not type (a `Char`
#' column, or one with no spec). Where they do overlap it is one error, not two.
#' @param carried A data.frame of import-time issues, or `NULL`.
#' @param rule_errors A data.frame of rule-time issues, or `NULL`.
#' @return A data.frame in the shape of [dta_empty_import_errors()], carrying
#'   the exact total in its `"n_import_errors"` attribute.
#' @keywords internal
dta_merge_import_errors <- function(carried, rule_errors) {
  carried_total <- dta_import_error_count(carried)

  if (!is.data.frame(rule_errors) || nrow(rule_errors) == 0) {
    rule_errors <- NULL
  }

  if (!is.null(rule_errors) && !is.null(carried)) {
    already <- paste(carried$row, carried$column, sep = "\r")
    keep <- !(paste(rule_errors$row, rule_errors$column, sep = "\r") %in% already)
    rule_errors <- rule_errors[keep, , drop = FALSE]
    if (nrow(rule_errors) == 0) {
      rule_errors <- NULL
    }
  }

  out <- do.call(
    rbind,
    c(list(dta_empty_import_errors()), Filter(Negate(is.null), list(carried, rule_errors)))
  )

  if (nrow(out) > 0) {
    out <- out[order(out$row, out$column, method = "radix"), , drop = FALSE]
    rownames(out) <- NULL
  }

  n_rule <- if (is.null(rule_errors)) 0 else nrow(rule_errors)
  attr(out, "n_import_errors") <- dta_narrow_count(
    as.double(carried_total) + n_rule
  )
  out
}
