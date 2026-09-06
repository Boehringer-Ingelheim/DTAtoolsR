# Column pruning and Arrow-accelerated precompute for the streaming scan.
#
# Two independent techniques, both about spending no work on data a rule never
# actually reads:
#
#   projection   Tells the Scanner which columns to materialise into R at all.
#                A column no spec declares and no rule reads is never parsed,
#                converted, or copied into an R vector -- the difference
#                between a batch carrying 30 columns and one carrying all 200
#                a wide file might have.
#
#   precompute   For uniqueness rules over a Dataset, the duplicate count is
#                answered by one Arrow grouped aggregation, run once ahead of
#                the batch loop, instead of by the per-batch fastmap
#                accumulator in R/streamingValidation.R. Text key columns are
#                grouped as they stand; a key column the specs declare numeric
#                is cast first, so that both paths key on the same values.
#
# Both are opportunistic: whenever either cannot be shown safe, it says so
# (`NULL`) rather than guessing, and the existing, slower-but-always-correct
# path is what runs.

#' @title Every Column a Rule Reads
#' @description
#' Names every column a rule consults, for callers that want to prune a scan
#' to only what is needed (see [dta_scan_projection()]).
#'
#' Returns `NULL` -- rather than a guess -- when the rule's type is not one of
#' the four recognised kinds. `NULL` is the signal "cannot enumerate": an
#' unrecognised rule might read anything, and a caller that treated a partial
#' answer as the truth could prune away a column the rule actually needed.
#' @param rule A rule object.
#' @return A character vector of column names (possibly empty), or `NULL`
#'   when the rule's type is not recognised.
#' @keywords internal
dta_rule_all_columns <- function(rule) {
  type <- tryCatch(normalize_rule_type(rule@type), error = function(e) NULL)
  if (is.null(type)) {
    return(NULL)
  }

  switch(type,
    check_range = as.character(dta_rule_target_columns(rule)),
    check_unique = as.character(dta_unique_columns(rule)),
    check_col_condition = {
      # A malformed clause degrades to "no columns" rather than erroring at
      # scan setup -- the clause itself will still be reported, properly,
      # once the rule is actually evaluated against a batch.
      cond_cols <- tryCatch(
        names(dta_normalize_conditions(rule_get_slot(rule, "condition"), arg = "condition")),
        error = function(e) character(0)
      )
      then_cols <- tryCatch(
        names(dta_normalize_conditions(rule_get_slot(rule, "then"), arg = "then")),
        error = function(e) character(0)
      )
      as.character(union(cond_cols, then_cols))
    },
    check_group_condition = {
      conditions <- rule_get_slot(rule, "conditions")
      cond_cols <- character(0)
      for (cond_name in names(conditions)) {
        cond_cols <- union(
          cond_cols,
          tryCatch(
            names(dta_normalize_conditions(conditions[[cond_name]], arg = cond_name)),
            error = function(e) character(0)
          )
        )
      }
      as.character(union(rule_get_slot(rule, "group_by"), cond_cols))
    },
    NULL
  )
}

#' @title Columns a Streaming Scan Actually Needs
#' @description
#' The projection to hand to a Dataset `Scanner`: the columns the column specs
#' declare, plus every column any rule reads, and nothing else.
#'
#' Projection means a column outside that set is never parsed, never
#' converted, and never materialised into an R vector at all -- it is the
#' difference between a batch carrying 30 columns and one carrying all 200 a
#' wide file might have, for work (column spec checks, rule evaluation, import
#' typing) that was only ever going to look at 30 of them.
#' @param specs A `DTAColumnSpecCollection`.
#' @param rules_list A list of `DTARule` objects, or `NULL`.
#' @param schema_names Character. The full set of column names in the
#'   source's Arrow schema.
#' @return A character vector of column names, ordered as in `schema_names`
#'   (so batch column order stays stable), or `NULL` when projecting would
#'   not help: some rule's columns cannot be enumerated (see
#'   [dta_rule_all_columns()]) and might read anything, or the needed columns
#'   turn out to be empty or already everything.
#' @keywords internal
dta_scan_projection <- function(specs, rules_list, schema_names) {
  # The spec-declared column keys, exactly as dta_reader_col_types()
  # (R/importConversion.R) derives them for the same collection. That
  # function's job is to build an arrow::schema() for the reader -- pinned to
  # utf8 and ordered by the schema -- so it is not called here; this needs
  # plain names, unordered, to compare against schema_names below, and the
  # short derivation is repeated rather than shared for that reason.
  spec_columns <- tryCatch(specs@columns, error = function(e) NULL)
  declared <- character(0)
  if (is.list(spec_columns) && length(spec_columns) > 0) {
    ids <- vapply(
      spec_columns,
      function(spec) tryCatch(as.character(spec@id)[[1]], error = function(e) NA_character_),
      character(1),
      USE.NAMES = FALSE
    )
    declared <- unique(c(names(spec_columns), ids))
    declared <- declared[!is.na(declared) & nzchar(declared)]
  }

  rule_columns <- character(0)
  for (rule in rules_list) {
    cols <- dta_rule_all_columns(rule)
    if (is.null(cols)) {
      # An unenumerable rule might read anything a projection could have
      # excluded, so projection is disabled for the whole scan rather than
      # risking a column that rule silently never sees.
      return(NULL)
    }
    rule_columns <- union(rule_columns, cols)
  }

  # A projection naming a column the file lacks would error the Scanner; a
  # spec column absent from the file is still reported, by the header-level
  # structural gate, which reads the full schema before projection is ever
  # applied.
  needed <- intersect(union(declared, rule_columns), schema_names)

  if (length(needed) == 0 || length(needed) == length(schema_names)) {
    return(NULL)
  }

  schema_names[schema_names %in% needed]
}

#' @title Should Arrow Answer Uniqueness Rules Ahead of the Streaming Scan?
#' @description
#' Unlike [dta_use_arrow_compute()] (`options(DTAtools.use_arrow_compute = )`,
#' strictly opt-in because a future arrow release could change floating-point
#' kernel semantics -- rounding, NaN handling -- and silently change a
#' validation verdict for a user who never asked for it), this path is on by
#' default.
#'
#' It can be, because the two ways a key is formed here are both cases where
#' Arrow's grouped aggregation and R's `duplicated()` provably agree rather than
#' merely usually agreeing (see [dta_arrow_unique_eligible()] and
#' [dta_stream_unique_precompute()]): text keys are compared byte-for-byte by
#' both, and a declared-numeric key is grouped on a cast that Arrow either
#' performs exactly as R's parser does or refuses outright -- in which case the
#' whole precompute falls back. Neither leaves the floating-point latitude that
#' keeps the general Arrow-compute path opt-in.
#' What tips the default the rest of the way is that the R fallback -- a
#' `fastmap` accumulator holding one entry per distinct key -- is exactly the
#' component that runs out of memory at the scale streaming exists for.
#'
#' `options(DTAtools.stream_arrow_unique = FALSE)` restores the per-batch
#' accumulator unconditionally, for a user who wants to rule out any
#' behavioural difference at all.
#' @return A single, non-`NA` logical.
#' @keywords internal
dta_stream_unique_arrow_enabled <- function() {
  isTRUE(getOption("DTAtools.stream_arrow_unique", TRUE)) && dta_arrow_compute_available()
}

#' @title Can a Uniqueness Rule Be Answered by One Arrow Aggregation?
#' @description
#' `TRUE` only when every one of the following holds; any failure -- including
#' an error raised while checking -- means `FALSE`, which routes the rule to
#' the per-batch accumulator instead:
#'
#' * `source` is specifically a `Dataset`. It must be re-scannable -- a
#'   `RecordBatchReader` is consumed by reading it, and the uniqueness
#'   precompute must leave the source untouched for the batch loop that follows
#'   -- and its schema must be readable directly as `$schema`, whereas an
#'   `arrow_dplyr_query`'s schema lives inside its lazy query plan. That source
#'   is rare enough on this path that reconstructing it is not worth the
#'   complexity: it is simply never eligible, which routes it to the per-batch
#'   path rather than erroring.
#' * the rule names at least one key column.
#' * every key column is present in the schema and typed `utf8`. This is about
#'   what the SOURCE is, not about what the specs say: the streaming open pins
#'   every column to `utf8`, so a delimited scan qualifies, while a Parquet
#'   dataset carrying its own numeric types does not. That exclusion is
#'   conservative rather than necessary -- only text-typed sources have been
#'   validated against the per-batch path -- and it costs nothing but speed: an
#'   ineligible source takes the per-batch path, which answers the rule
#'   correctly either way.
#'
#' A key column's DECLARED type decides how it is grouped, not whether it is
#' eligible. The schema type alone cannot tell a genuine text column from a
#' declared-`Num` one -- everything is `utf8` at scan time -- and the two are
#' keyed differently: the per-batch accumulator keys a declared-numeric column
#' on its coerced NUMBERS, where `"1.50"` and `"1.5"` are one key, while raw
#' text grouping would see two. The numeric key columns are therefore reported
#' here and normalised in [dta_stream_unique_precompute()] before grouping, so
#' that both paths key on the same values.
#'
#' Text is grouped as it stands because R's `duplicated()` and Arrow's grouped
#' aggregation agree byte-for-byte on strings, including `NA == NA`.
#' @param rule A uniqueness rule.
#' @param source A table representation to be scanned.
#' @param specs A `DTAColumnSpecCollection`, or `NULL`: consulted for the key
#'   columns' declared types, as above.
#' @return A list with `ok` -- a single, non-`NA` logical -- and
#'   `numeric_columns`, the key columns whose spec declares a numeric type
#'   (`character(0)` for an all-text key, and always `character(0)` when `ok` is
#'   `FALSE`). A list rather than a bare logical because the caller needs both
#'   answers and deriving the second one twice could let them drift.
#' @keywords internal
dta_arrow_unique_eligible <- function(rule, source, specs = NULL) {
  ineligible <- list(ok = FALSE, numeric_columns = character(0))

  tryCatch(
    {
      # A `Dataset` and nothing else: re-scannable, and its schema is readable
      # without reconstructing a lazy query plan. See the eligibility bullets
      # above for why an `arrow_dplyr_query` is not admitted here.
      if (!inherits(source, "Dataset")) {
        return(ineligible)
      }

      cols <- dta_unique_columns(rule)
      if (!is.character(cols) || length(cols) == 0) {
        return(ineligible)
      }

      schema <- source$schema

      if (!all(cols %in% names(schema))) {
        return(ineligible)
      }

      types_ok <- all(vapply(
        cols,
        function(col) isTRUE(schema[[col]]$type$Equals(arrow::utf8())),
        logical(1)
      ))
      if (!types_ok) {
        return(ineligible)
      }

      numeric_columns <- cols[vapply(
        cols,
        function(col) {
          target <- dta_spec_r_type(specs, col)
          !is.na(target) && target %in% c("double", "integer")
        },
        logical(1),
        USE.NAMES = FALSE
      )]

      list(ok = TRUE, numeric_columns = as.character(numeric_columns))
    },
    error = function(e) ineligible
  )
}

#' @title Precompute Uniqueness Rules Through Arrow, Ahead of the Batch Loop
#' @description
#' What replaced the old `DTAtools.max_unique_keys` budget. That budget
#' aborted a multi-hour scan the moment the number of distinct keys crossed a
#' fixed limit -- worst at exactly the scale (a per-row-unique id column)
#' streaming exists for. Here, for every eligible rule (see
#' [dta_arrow_unique_eligible()]), the distinct keys are never materialised in
#' R at all: they live in Arrow's own grouped aggregation, and only the
#' one-row reduction -- duplicate count and total -- ever crosses back into R.
#'
#' The whole computation is one extra streaming pass over just the key
#' columns, run once before the main batch loop rather than accumulated
#' alongside it.
#' @section Declared-numeric key columns:
#' A key column the specs declare `Num` or `Int` is grouped on
#' `as.numeric(col)`, so that Arrow keys it on the same values the per-batch
#' accumulator keys its coerced column on -- `"1.50"` and `"1.5"` are one key on
#' both. Two properties of that cast are what make it safe, both measured on
#' arrow 25.0.1 rather than assumed:
#'
#' * it is exact where it succeeds. Every string both engines accept parses to
#'   the identical double, `"1e2"`, `"007"`, `".5"`, `"Inf"` and
#'   `"18446744073709551616"` included.
#' * it ERRORS where it would have to guess. `"abc"`, `"0x10"` and `" 3"` --
#'   all of which R's parser accepts or turns into `NA` -- make Acero raise,
#'   which the `tryCatch` below turns into the per-batch fallback. Divergence
#'   is therefore not a wrong answer but a slower correct one.
#'
#' One normalisation is still required on top: `-0` and `0` are one value to
#' `duplicated()` (the key encoding adds `+ 0` for exactly this) and two groups
#' to Acero, so the cast is followed by `if_else(k == 0, 0, k)`. Measured on the
#' eight values `NaN, NaN, <missing>, <missing>, -0, 0, 1.5, 1.50`, which
#' `duplicated()` calls 4 duplicates: without the fold Acero reported 3.
#'
#' No `NaN` normalisation is applied, deliberately. Acero already groups `NaN`
#' with itself and apart from null, which is exactly what the R key does --
#' `dta_row_key()` masks only `is.na(x) & !is.nan(x)`, leaving `NaN` its own
#' rendering. Folding `NaN` into null as well took those same eight values to 5
#' duplicates, one more than the table contains.
#' @param specs A `DTAColumnSpecCollection`. Consulted by
#'   [dta_arrow_unique_eligible()] for the key columns' declared types, which
#'   decide which key columns are normalised as numbers before grouping.
#' @param source A table representation to be scanned (typically the
#'   `Dataset` the streaming driver was opened from).
#' @param rules_list A list of `DTARule` objects, or `NULL`.
#' @return A list the same length as `rules_list`. Entry `i` is a result list
#'   (`id`, `valid`, `message`) when the rule at that position was answered
#'   here, or `NULL` when it was not -- not a keyed rule, Arrow disabled or
#'   unavailable, the rule or source ineligible, or the Arrow computation
#'   itself failed -- in which case the caller's per-batch accumulator handles
#'   it instead. `list()` when `rules_list` is empty. Never errors.
#' @keywords internal
dta_stream_unique_precompute <- function(specs, source, rules_list) {
  if (length(rules_list) == 0) {
    return(list())
  }

  arrow_enabled <- dta_stream_unique_arrow_enabled()

  lapply(rules_list, function(rule) {
    if (!isTRUE(arrow_enabled) || !identical(dta_rule_stream_kind(rule), "keyed")) {
      return(NULL)
    }

    eligibility <- dta_arrow_unique_eligible(rule, source, specs)
    if (!isTRUE(eligibility$ok)) {
      return(NULL)
    }

    # Any arrow failure here falls back silently to the per-batch accumulator
    # in R/streamingValidation.R -- a resource or engine problem on this
    # opportunistic path is not a reason to fail validation that the slower
    # path would still complete correctly. An unparseable value in a
    # declared-numeric key column arrives as exactly such a failure: the cast
    # below raises rather than inventing a key, and the per-batch path -- which
    # reads that value as NA and reports it on the import axis -- answers the
    # rule instead.
    tryCatch(
      {
        cols <- dta_unique_columns(rule)
        numeric_cols <- eligibility$numeric_columns

        query <- dplyr::select(source, dplyr::all_of(cols))
        if (length(numeric_cols) > 0) {
          # See the "Declared-numeric key columns" section above for why the
          # cast is safe and why the zero fold is needed but a NaN fold is not.
          query <- dplyr::mutate(
            query,
            dplyr::across(dplyr::all_of(numeric_cols), ~ as.numeric(.x))
          )
          query <- dplyr::mutate(
            query,
            dplyr::across(dplyr::all_of(numeric_cols), ~ dplyr::if_else(.x == 0, 0, .x))
          )
        }

        # This nested aggregation runs entirely inside Acero (verified on
        # arrow 25.0.1): the distinct keys live in the C++ engine, and only
        # the one-row reduction below -- duplicate count and total -- ever
        # reaches R.
        agg <- dplyr::summarise(
          dplyr::group_by(query, dplyr::across(dplyr::all_of(cols))),
          .n = dplyr::n(), .groups = "drop"
        )
        counts <- dplyr::collect(
          dplyr::summarise(agg, dups = sum(.n) - dplyr::n(), total = sum(.n))
        )
        # dups equals sum(duplicated(df[cols])) exactly for these keys.
        n <- as.double(counts$dups[[1]])

        if (n > 0) {
          list(
            id = rule@id, valid = FALSE,
            message = dta_unique_violation_message(rule@id, n, cols)
          )
        } else {
          list(id = rule@id, valid = TRUE, message = NULL)
        }
      },
      error = function(e) NULL
    )
  })
}
