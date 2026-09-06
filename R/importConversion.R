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


#' @title Reader Column Types Declared by the Specs
#' @description
#' Builds the Arrow schema handed to the CSV/TSV/delimited reader, so the
#' declared type of a column -- not the reader's guess at it -- decides how the
#' bytes in the file are parsed.
#'
#' Arrow infers a column's type from its contents, and that inference runs
#' *before* any code in this package sees the data. Two classes of problem
#' arise:
#'
#' 1. **Character corruption.** A column of quoted subject ids -- `"007"`,
#'    `"008"` -- is inferred as `int64` and arrives in R as `7` and `8`. The
#'    leading zeros are gone before [dta_coerce_table_to_specs()] runs, so its
#'    "never coerce a `Char` column" guard has nothing left to protect.
#'
#' 2. **Hard abort on mixed numeric data.** Arrow locks in the inferred type
#'    after scanning enough rows. If it picks `int64` for a column that mostly
#'    looks like integers and then encounters `0.01` further down, it aborts the
#'    entire read: `CSV conversion error to int64: invalid value '0.01'`. That
#'    turns one reportable bad-cell into a file that will not load at all.
#'
#' Both problems are solved the same way: pin every declared column to `utf8`
#' at read time and let [dta_coerce_table_to_specs()] handle the conversion.
#' Reading as text can never fail and never loses information. An unrepresentable
#' value (`"0.01"` in an `Int` column, `"abc"` in a `Num` column) becomes `NA`
#' in the typed column and its source text is retained as an import error --
#' which is exactly what the schema-validation axis expects to see.
#' @param specs A `DTAColumnSpecCollection`, or `NULL`. `NULL` means "no
#'   declared types are available", and yields `NULL`: the reader then infers
#'   every column exactly as it did before.
#' @param has_header Logical. When the file has no header, Arrow generates
#'   positional names (`f0`, `f1`, ...) that cannot correspond to spec ids, so
#'   no column spec is built.
#' @return An `arrow::schema()` naming the textual columns, or `NULL` when there
#'   is nothing to pin.
#' @keywords internal
dta_reader_col_types <- function(specs, has_header = TRUE) {
  if (is.null(specs) || !isTRUE(has_header)) {
    return(NULL)
  }

  columns <- tryCatch(specs@columns, error = function(e) NULL)

  if (!is.list(columns) || length(columns) == 0) {
    return(NULL)
  }

  ids <- vapply(
    columns,
    function(spec) tryCatch(as.character(spec@id)[[1]], error = function(e) NA_character_),
    character(1),
    USE.NAMES = FALSE
  )

  # A collection is normally named by column id, but one built by another route
  # may not be. `dta_spec_r_type()` resolves either, so both are offered to it:
  # the reader and the coercion choke point then agree on what a column is by
  # construction, rather than by two lookups that could drift apart.
  keys <- unique(c(names(columns), ids))
  keys <- keys[!is.na(keys) & nzchar(keys)]

  # Pin every column with a declared type to utf8.  Arrow's inference is applied
  # *before* any R code runs, so two things can go wrong without this:
  # (a) a "007" Char id is inferred as int64 and arrives as 7; and
  # (b) a column inferred as int64 aborts the entire read when it later
  #     encounters "0.01", turning one reportable bad cell into a load failure.
  # Columns with no spec are left alone (NA from dta_spec_r_type) so Arrow
  # infers them exactly as it would without specs.
  typed_keys <- keys[vapply(
    keys,
    function(key) !is.na(dta_spec_r_type(specs, key)),
    logical(1),
    USE.NAMES = FALSE
  )]

  if (length(typed_keys) == 0) {
    return(NULL)
  }

  # A schema entry for a column the file does not contain is ignored by Arrow,
  # so a spec that declares more columns than the file carries is not an error
  # here. Whether the column is missing is the column spec axis's question.
  types <- rep(list(arrow::utf8()), length(typed_keys))
  names(types) <- typed_keys

  do.call(arrow::schema, types)
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
#' @return A named character vector, keyed exactly as [dta_reader_col_types()]
#'   derives its keys (`unique(c(names(columns), ids))`, with `NA`/`""` keys
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

  # Same key derivation as dta_reader_col_types(): a collection is normally
  # named by column id, but one built by another route may not be, so both are
  # offered and deduplicated.
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
