# Validating a table without holding it.
#
# The non-streaming path takes a materialised data frame. That is fatal at the
# sizes this package is meant to reach: an 80 GB file cannot be an R data frame
# at all, whatever the validation costs once it is one.
#
# What makes streaming safe rather than merely possible is that the column spec axis
# is purely per-row. No constraint it evaluates -- type, maxLength, enum, const,
# pattern -- consults another row, so a batch can be checked in isolation and
# the results concatenated. The only cross-batch state is the row offset, so
# that reported row numbers are positions in the FILE rather than in whichever
# batch a value happened to fall in.
#
# `required` is the exception worth naming: a column absent from the schema
# produces one error per row, so it is emitted per batch like everything else
# and simply accumulates. That is faithful to the non-streaming behaviour, and
# it is also why a structural check belongs ahead of the scan entirely -- the
# gate further down this file decides it from the header instead.
#
# The rules axis is folded in the same pass; see the note on rule kinds below.


# ---- streaming the rules axis ------------------------------------------------
#
# Rules divide into three kinds by how much of the table they need at once.
#
#   decomposable  A count over a per-row mask. Range and IF/THEN rules are
#                 these. Sum the per-batch counts and the total is exactly the
#                 whole-table answer, because addition does not care how the
#                 rows were grouped.
#
#   keyed         Uniqueness. Not decomposable -- a duplicate may sit in a
#                 different batch from the value it duplicates -- but it needs
#                 only the KEY, not the row. Memory grows with the number of
#                 distinct keys rather than with the number of rows.
#
#   grouped       Grouped cross-row rules. A group can span any number of
#                 batches, but the questions asked of it -- does a condition
#                 hold for ANY row, or for ALL rows -- are OR and AND, which
#                 fold batch by batch. Memory grows with the number of distinct
#                 groups times conditions, not with the number of rows.
#
#   unsupported   An unrecognised rule type, reported as a rule failure rather
#                 than guessed at.
#
# The violation masks come from the same functions the materialising path uses
# (dta_range_violated, dta_condition_violated), so the two cannot drift.

#' @title How a Rule Can Be Streamed
#' @param rule A rule object.
#' @return One of `"decomposable"`, `"keyed"`, `"grouped"` or `"unsupported"`.
#' @keywords internal
dta_rule_stream_kind <- function(rule) {
  switch(normalize_rule_type(rule@type),
    check_range = "decomposable",
    check_col_condition = "decomposable",
    check_unique = "keyed",
    check_group_condition = "grouped",
    # An unrecognised type must not be silently treated as buffered and handed
    # to the grouped evaluator; it is reported as a rule failure, as the
    # materialising path does.
    "unsupported"
  )
}

# The bytes the row key encoding reserves. Written as code points rather than
# escapes so the source carries no raw control byte for tooling to mangle.
DTA_KEY_SEP <- intToUtf8(31L)
DTA_KEY_ESC <- intToUtf8(1L)
DTA_KEY_RESERVED <- paste0("[", intToUtf8(1L), intToUtf8(31L), "]")

#' @title Collision-Free Row Key for a Set of Columns
#' @description
#' One string per row, equal for two rows exactly when `duplicated()` would
#' call those rows duplicates, so that keys can stand in for the rows
#' themselves -- in a hash set across batches, or in `duplicated()` on a single
#' vector instead of on a data frame.
#'
#' The encoding is injective, which a plain separator-joined key is not:
#' `c("x", "y\037z")` and `c("x\037y", "z")` are different rows that would both
#' render to `x\037y\037z`, silently merging distinct keys. Within a field a
#' reserved byte is therefore escaped -- `ESC` becomes `ESC e`, the separator
#' becomes `ESC s` -- so an encoded field can contain neither a bare separator
#' nor a bare `ESC`, and splitting the joined string on the separator recovers
#' the fields exactly.
#'
#' Missing values are given the marker `ESC n`, which no literal can produce
#' (a literal `ESC n` encodes to `ESC e n`). They therefore compare equal to
#' each other and to nothing else, which is `duplicated()`'s notion, under
#' which repeated NAs are duplicates but `NaN` is not one of them.
#'
#' Doubles are rendered with `%.17g` rather than through `as.character()`,
#' which rounds to 15 significant digits: `0.1 + 0.2` and `0.3` are two
#' different doubles that `duplicated()` keeps apart and `as.character()` does
#' not. That applies to everything stored as a double, `POSIXct` and `Date`
#' included, so a sub-second timestamp keys exactly, and to both parts of a
#' `complex`. `integer64` is excluded: it is stored as a double but is not one,
#' and bit64 renders it exactly itself.
#'
#' What is left is as distinguishable as its character rendering -- a list
#' column, or any class with its own `as.character()` method -- which is why
#' the eager path in `dta_count_duplicates()` keeps its own type gate rather
#' than trusting every type to this function.
#'
#' The escaping pass is skipped when the column holds no reserved byte at all,
#' which is the overwhelmingly common case. That is a pure optimisation and not
#' a second encoding: `gsub()` is the identity on text it does not match, so
#' the result is the same string either way, and equal rows in different
#' batches still encode equally -- which is what the streaming path's key set
#' depends on.
#' @param df A data frame.
#' @param cols Character. The key columns.
#' @return A character vector, one key per row.
#' @keywords internal
dta_row_key <- function(df, cols) {
  if (length(cols) == 0) {
    # Every row is then the same row, which is what the materialising path
    # sees: `duplicated(df[, character(0), drop = FALSE])` is TRUE from the
    # second row on. Returning `character(0)` here -- what `paste()` with no
    # arguments would give -- would instead report no rows at all.
    # DTA_KEY_ESC, not "": fastmap rejects "" as a key outright, so the empty
    # key crashed the whole streaming scan. See the remap below for why the
    # substitution is collision-free.
    return(rep(DTA_KEY_ESC, nrow(df)))
  }

  parts <- lapply(cols, function(column_name) {
    values <- df[[column_name]]

    # `integer64` is stored as a double and would be reinterpreted as one by
    # the branch below -- `NA_integer64_` is INT64_MIN, whose bit pattern read
    # as a double is -0, which would then key as the integer64 value 0. bit64
    # renders it exactly through `as.character()`, so it takes that route.
    if (is.double(values) && !inherits(values, "integer64")) {
      # `unclass()` so that a POSIXct or Date keys on its underlying instant
      # rather than on a rendering that depends on the column's timezone
      # attribute, and `+ 0` because -0 and 0 are one value to `duplicated()`
      # but two renderings to `sprintf()`.
      numbers <- unclass(values) + 0
      text <- sprintf("%.17g", numbers)
      # NaN is not a missing value to `duplicated()`, and `sprintf()` has
      # already rendered it as "NaN", so only a true NA takes the marker.
      na_mask <- is.na(numbers) & !is.nan(numbers)
    } else if (is.complex(values)) {
      # Both parts are doubles and lose the same precision through
      # `as.character()`, which renders 0.1 + 0.2 + 0i and 0.3 + 0i alike.
      text <- paste0(
        sprintf("%.17g", Re(values) + 0), "+", sprintf("%.17g", Im(values) + 0), "i"
      )
      # `sprintf()` has already rendered NA and NaN parts distinguishably, and
      # no other value can render to the same string, so no marker is needed.
      na_mask <- logical(length(values))
    } else {
      text <- if (is.character(values)) values else as.character(values)
      na_mask <- is.na(values)
    }

    # A matrix or data frame column renders to more strings than the table has
    # rows and would recycle silently against the other fields, producing a
    # longer key vector and a duplicate count belonging to no real row.
    if (length(text) != nrow(df)) {
      cli::cli_abort(
        c(
          "Column {.val {column_name}} cannot be used as a key column.",
          x = "It yields {length(text)} value{?s} for {nrow(df)} row{?s}.",
          i = "Key columns must be plain vectors, not matrix or data frame columns."
        ),
        class = "dta_rule_not_applicable"
      )
    }

    # The key must depend on the value alone. Without this, a latin1-marked
    # string could be re-encoded by the escaping pass in a batch where some
    # other row carries a reserved byte, and left alone in a batch where none
    # does -- two different byte sequences for the same value, and a duplicate
    # that the key set never sees.
    text <- enc2utf8(text)

    # Scanned unconditionally rather than only for character columns: any type
    # whose `as.character()` returns user text -- a list column, or a class
    # with its own method -- can carry a reserved byte too. `perl = TRUE,
    # useBytes = TRUE` because this runs on every batch of every keyed rule and
    # is pure overhead in the common case: measured on 5e5 ids it costs ~0.00s,
    # against ~0.04s for the default engine.
    if (any(grepl(DTA_KEY_RESERVED, text, perl = TRUE, useBytes = TRUE))) {
      text <- gsub(DTA_KEY_ESC, paste0(DTA_KEY_ESC, "e"), text, fixed = TRUE, useBytes = TRUE)
      text <- gsub(DTA_KEY_SEP, paste0(DTA_KEY_ESC, "s"), text, fixed = TRUE, useBytes = TRUE)
      # `gsub(useBytes = TRUE)` returns the strings it rewrote unmarked while
      # leaving the ones it did not touch marked UTF-8, which would put two
      # encodings in one column again. The bytes are known to be UTF-8 (they
      # came from `enc2utf8()` and both replacements are ASCII), so they are
      # re-marked rather than re-translated.
      Encoding(text) <- "UTF-8"
    }

    if (any(na_mask)) {
      text[na_mask] <- paste0(DTA_KEY_ESC, "n")
    }
    text
  })
  out <- do.call(paste, c(parts, sep = DTA_KEY_SEP))

  # fastmap rejects "" as a key ('key must be not be "" or NA'), and that
  # error is not classed dta_rule_not_applicable, so a single empty-string
  # value in a key or grouping column aborted the entire streaming scan. The
  # empty key is remapped to a lone ESC byte, which the encoding can never
  # produce for any real value (a literal ESC field encodes to "ESC e", and a
  # multi-column key always contains the separator), so the remap is
  # injective and equal rows still key equally. Only a single-column key over
  # an empty string can be "" here -- with two or more columns the separator
  # alone makes the key non-empty -- so the scan is the cheap common path.
  empty <- !nzchar(out)
  if (any(empty)) {
    out[empty] <- DTA_KEY_ESC
  }
  out
}

# A key that reproduces `duplicated()`'s notion of an identical row. See
# `dta_row_key()` for why the encoding has to be injective rather than a plain
# separator join.
dta_unique_key <- function(df, cols) {
  dta_row_key(df, cols)
}

#' @title Start Accumulating a Rule Across Batches
#' @param rule A rule object.
#' @return A mutable accumulator.
#' @keywords internal
dta_rule_stream_init <- function(rule) {
  kind <- dta_rule_stream_kind(rule)
  state <- new.env(parent = emptyenv())
  state$kind <- kind
  # A double, not an integer. The counter accumulates across every batch in the
  # file, and an integer one silently becomes NA past .Machine$integer.max --
  # about 2.1 billion -- which is inside the range this path is built for. A
  # double counts whole numbers exactly to 2^53, and `sprintf("%d", ...)` still
  # renders it, because the value is always whole.
  state$count <- 0
  state$applicable <- TRUE
  state$condition <- NULL

  if (kind == "keyed") {
    # fastmap instead of an R environment: assign(key, ...) / env[[key]] intern
    # every key in R's global SYMBOL table, which is never garbage collected,
    # so an environment used as a hash set leaks roughly 278 bytes per distinct
    # key permanently -- measured -- and that is unrecoverable even after the
    # accumulator is dropped and gc() runs. fastmap is a C++ hash map that does
    # not touch the symbol table, so its memory is actually reclaimed.
    #
    # This per-batch accumulator is the FALLBACK: a Dataset-backed scan
    # answers eligible uniqueness rules through Arrow's own grouped
    # aggregation instead (see dta_stream_unique_precompute()), which holds
    # the distinct keys in the C++ engine rather than as R strings. There is
    # deliberately no key budget here any more: the old
    # DTAtools.max_unique_keys abort discarded a multi-hour scan at exactly
    # the per-row-unique-ID scale streaming exists for, which is worse than
    # the memory growth it guarded -- that growth is now the exception (non-
    # text keys, reader sources), not the rule, and it is documented instead
    # of enforced.
    state$seen <- fastmap::fastmap()
  }
  if (kind == "grouped") {
    state$grouped <- dta_group_stream_init(rule)
    # A double, for the same reason as `state$count` above. This one is added to
    # every row number the grouped path reports, so an integer offset gone `NA`
    # does not merely lose a count: it turns every violation message into
    # "rows: NA, NA, NA" while the verdict still reads as authoritative.
    state$row_offset <- 0
  }

  state
}

#' @title Fold One Batch into a Rule's Accumulator
#' @param state An accumulator from `dta_rule_stream_init()`.
#' @param rule The rule being accumulated.
#' @param df A data frame holding one batch.
#' @param numeric_cache A named list from [dta_build_numeric_cache()] built for
#'   this batch, or `NULL` to convert each column on demand.
#' @return The accumulator, updated in place.
#' @keywords internal
dta_rule_stream_update <- function(state, rule, df, numeric_cache = NULL) {
  # "precomputed": the rule's verdict was already reached over the whole
  # source (Arrow-side uniqueness); batches carry no further information.
  if (!state$applicable || state$kind %in% c("unsupported", "precomputed")) {
    return(state)
  }

  # A rule naming a column the table does not have is not applicable, and says
  # so once rather than once per batch.
  result <- tryCatch(
    {
      switch(state$kind,
        decomposable = {
          violated <- if (identical(normalize_rule_type(rule@type), "check_range")) {
            dta_range_violated(rule, df, numeric_cache)
          } else {
            dta_condition_violated(rule, df, numeric_cache)
          }
          state$count <- state$count + sum(violated, na.rm = TRUE)
        },
        keyed = {
          cols <- dta_unique_columns(rule)
          missing_cols <- setdiff(cols, names(df))
          if (length(missing_cols) > 0) {
            cli::cli_abort(
              "Column{?s} not found in table: {paste(missing_cols, collapse = ', ')}",
              class = "dta_rule_not_applicable"
            )
          }
          keys <- dta_unique_key(df, cols)
          # Membership is tested for the whole batch against the state BEFORE
          # any of it is inserted, so every occurrence of a key first seen in an
          # earlier batch counts, and within this batch only the repeats do.
          # That is exactly `duplicated()`'s notion, n - 1 per distinct key.
          already <- state$seen$has(keys)
          first_here <- !duplicated(keys)
          state$count <- state$count + sum(already | !first_here)

          new_keys <- keys[!already & first_here]
          # `mset(.list = )`, not a per-key `set()` loop: the loop cost one
          # interpreted closure call per distinct key (~3 us measured; hours
          # of pure dispatch on a per-row-unique key over billions of rows).
          # The names attribute of the list shares the existing CHARSXPs, so
          # the feared "second copy of every key" is a pointer vector, not a
          # copy of the strings.
          if (length(new_keys) > 0) {
            # rep(list(TRUE), n) is n pointers to one shared TRUE, not n
            # copies of anything.
            state$seen$mset(.list = stats::setNames(
              rep(list(TRUE), length(new_keys)), new_keys
            ))
          }
        },
        grouped = {
          dta_group_stream_update(state$grouped, rule, df, state$row_offset, numeric_cache)
          state$row_offset <- state$row_offset + nrow(df)
        }
      )
      NULL
    },
    dta_rule_not_applicable = function(cnd) cnd
  )

  if (!is.null(result)) {
    state$applicable <- FALSE
    state$condition <- result
  }

  state
}

#' @title Turn a Rule's Accumulator into a Result
#' @param state An accumulator that has seen every batch.
#' @param rule The rule being accumulated.
#' @return A list with `id`, `valid` and `message`, matching what the
#'   materialising rule functions return.
#' @keywords internal
dta_rule_stream_finalise <- function(state, rule) {
  if (state$kind == "precomputed") {
    return(state$result)
  }

  if (state$kind == "unsupported") {
    return(list(
      id = rule@id,
      valid = FALSE,
      message = paste("Unknown rule type:", normalize_rule_type(rule@type))
    ))
  }

  if (!state$applicable) {
    # Matches apply_rules(): a rule that cannot be evaluated against this
    # table is a rule FAILURE, not a reason to abandon the rest of validation.
    return(list(
      id = rule@id,
      valid = FALSE,
      message = sprintf(
        "Rule '%s' could not be evaluated: %s",
        rule@id,
        conditionMessage(state$condition)
      )
    ))
  }

  if (state$kind == "grouped") {
    # Mirrors apply_rules()'s handling of rule_check_group_condition(): a
    # constraint that references a condition name which does not exist (only
    # reachable by bypassing the DTARuleGroupCondition constructor) is a rule
    # FAILURE, not a reason to abort the whole streaming scan.
    return(tryCatch(
      dta_group_stream_finalise(state$grouped, rule),
      dta_rule_not_applicable = function(cnd) {
        list(
          id = rule@id,
          valid = FALSE,
          message = sprintf(
            "Rule '%s' could not be evaluated: %s",
            rule@id,
            conditionMessage(cnd)
          )
        )
      }
    ))
  }

  if (state$count == 0) {
    return(list(id = rule@id, valid = TRUE, message = NULL))
  }

  message <- if (state$kind == "keyed") {
    dta_unique_violation_message(rule@id, state$count, dta_unique_columns(rule))
  } else if (identical(normalize_rule_type(rule@type), "check_range")) {
    target <- dta_range_target(rule)
    dta_range_violation_message(rule@id, state$count, target$col, target$range)
  } else {
    dta_condition_violation_message(rule@id, state$count)
  }

  list(id = rule@id, valid = FALSE, message = message)
}

#' @title Whether a Partial Scan Can Already Call a Rule Failed
#' @description
#' The predicate behind `fail_fast`. A rule counts as failed only when no later
#' batch could overturn the verdict.
#'
#' `state$count` alone is not that predicate. A grouped rule never increments
#' it -- its verdict is reached in `dta_group_stream_finalise()`, after the
#' scan -- so reading `count` left `fail_fast` scanning a whole file whose very
#' first rows already broke a grouped constraint. Unsupported and
#' not-applicable rules are failures the moment they are discovered, and are
#' likewise invisible in `count`.
#' @param state An accumulator from `dta_rule_stream_init()`.
#' @return A single logical.
#' @keywords internal
dta_rule_stream_failing <- function(state) {
  if (state$kind == "unsupported" || !state$applicable) {
    return(TRUE)
  }
  if (state$kind == "precomputed") {
    # A precomputed verdict covered the whole source, so its failure is as
    # settled as any full scan's.
    return(!isTRUE(state$result$valid))
  }
  if (state$kind == "grouped") {
    return(state$grouped$certain > 0L)
  }
  state$count > 0L
}

#' @title Columns Forming a Uniqueness Key
#' @param rule A uniqueness rule.
#' @return A character vector of column names.
#' @keywords internal
dta_unique_columns <- function(rule) {
  dta_rule_target_columns(rule)
}

# ---- the streaming driver ----------------------------------------------------

# Bounded accumulation of a per-cell error frame.
#
# Both the schema and import axes can produce one error per bad cell, so on a
# dirty file the error frame is O(rows) and exhausts memory as surely as the
# data would. In-memory retention is capped; counting is not, so the reported
# totals stay exact and the pass/fail verdict is never an artefact of
# truncation. Rows past the cap are no longer discarded, though: they SPILL to
# a session-temporary directory, one RDS part per overflowing addition, so the
# full detail stays recoverable (see collect_full_errors()) while memory stays
# bounded by the cap.
dta_error_sink <- function(max_errors) {
  sink <- new.env(parent = emptyenv())
  sink$parts <- list()
  # Doubles, not integers. Counting is uncapped by design, so on a file dirty
  # enough to produce more than `.Machine$integer.max` errors an integer
  # accumulator returns `NA` with a warning instead of a count -- and `NA > 0`
  # is `NA`, so the truncation-proof verdict this sink exists to protect became
  # unknowable at exactly the scale that matters. See `dta_narrow_count()`.
  sink$retained <- 0
  sink$total <- 0
  sink$truncated <- FALSE
  sink$max <- max_errors
  # Created lazily on first overflow; NULL means nothing was ever spilled.
  sink$spill_dir <- NULL
  sink$spill_parts <- 0L
  sink$spilled <- 0
  # A zero-row copy of the first frame ever added, so collect() can hand back
  # a correctly-shaped frame carrying the spill pointer even when the
  # in-memory cap retained nothing at all (max_errors = 0): returning NULL
  # there would silently strand the spilled rows on disk.
  sink$prototype <- NULL
  sink
}

# One overflowing frame to disk. tempfile() scopes the spill to the R session:
# the counts in the collected frame stay exact forever, but row-level detail
# past the in-memory cap is recoverable for as long as the session's tempdir
# lives -- which is the honest trade against holding O(rows) of detail in RAM.
dta_error_sink_spill <- function(sink, errs) {
  if (nrow(errs) == 0) {
    return(invisible(sink))
  }
  if (is.null(sink$spill_dir)) {
    sink$spill_dir <- tempfile("dta_error_spill_")
    dir.create(sink$spill_dir, recursive = TRUE)
  }
  sink$spill_parts <- sink$spill_parts + 1L
  saveRDS(errs, file.path(sink$spill_dir, sprintf("part-%06d.rds", sink$spill_parts)))
  sink$spilled <- sink$spilled + nrow(errs)
  invisible(sink)
}

#' @param sink Environment. An error sink created by `dta_error_sink()`.
#' @param errs Data frame of errors to add.
#' @param n_total Integer or `NULL`. The true number of errors these rows
#'   represent, when the caller has already truncated them. Import typing caps
#'   retained rows per column but records the real total on the frame, so
#'   counting `nrow()` here would silently under-report exactly the case the cap
#'   exists for.
#' @noRd
dta_error_sink_add <- function(sink, errs, n_total = NULL) {
  if (is.null(errs) || nrow(errs) == 0) {
    # A caller may have truncated everything away while still knowing how many
    # there were.
    if (!is.null(n_total) && n_total > 0) {
      sink$total <- sink$total + n_total
      sink$truncated <- TRUE
    }
    return(sink)
  }

  if (is.null(sink$prototype)) {
    sink$prototype <- errs[0, , drop = FALSE]
  }

  arriving <- if (is.null(n_total)) nrow(errs) else max(n_total, nrow(errs))
  sink$total <- sink$total + arriving
  if (arriving > nrow(errs)) {
    # Rows were dropped before they reached this sink.
    sink$truncated <- TRUE
  }

  if (is.null(sink$max)) {
    sink$parts[[length(sink$parts) + 1]] <- errs
    sink$retained <- sink$retained + nrow(errs)
    return(sink)
  }

  room <- sink$max - sink$retained
  if (room <= 0) {
    sink$truncated <- TRUE
    dta_error_sink_spill(sink, errs)
    return(sink)
  }
  if (nrow(errs) > room) {
    dta_error_sink_spill(sink, errs[-seq_len(room), , drop = FALSE])
    errs <- errs[seq_len(room), , drop = FALSE]
    sink$truncated <- TRUE
  }
  sink$parts[[length(sink$parts) + 1]] <- errs
  sink$retained <- sink$retained + nrow(errs)
  sink
}

dta_error_sink_collect <- function(sink) {
  if (length(sink$parts) == 0) {
    if (sink$spilled > 0 && !is.null(sink$prototype)) {
      # Nothing was retained in memory, but rows WERE spilled: a NULL here
      # would strand them -- the caller could not tell "no errors" from
      # "every error is on disk". A zero-row frame of the right shape carries
      # the spill pointer instead.
      out <- sink$prototype
      rownames(out) <- NULL
      attr(out, "truncated") <- TRUE
      attr(out, "spilled_rows") <- sink$spilled
      attr(out, "spill_dir") <- sink$spill_dir
      return(out)
    }
    return(NULL)
  }
  out <- do.call(rbind, sink$parts)
  rownames(out) <- NULL
  if (sink$truncated) {
    attr(out, "truncated") <- TRUE
  }
  if (sink$spilled > 0) {
    # The frame holds the head; the rest is on disk. collect_full_errors()
    # reassembles the two.
    attr(out, "spilled_rows") <- sink$spilled
    attr(out, "spill_dir") <- sink$spill_dir
  }
  out
}

#' @title Every Retained and Spilled Error Row of a Validation Result
#' @description
#' A streaming scan keeps at most `max_errors` per-cell error rows in memory
#' and spills the rest to a session-temporary directory, so the reported
#' counts are always exact while memory stays bounded. This reassembles the
#' complete detail frame -- the in-memory head plus every spilled row -- for
#' the requested axis.
#'
#' The spill lives in the R session's temporary directory: it survives for the
#' session, not beyond it. Reading a persisted `details` artifact in a later
#' session still yields the exact counts and the retained head; if the spill
#' directory is gone, this warns and returns the head rather than failing.
#' @param details A validation details list, as returned by
#'   [validate_file_stream()] or stored by `check()`.
#' @param axis One of `"columnspec"` or `"import"`: which error frame to
#'   reassemble.
#' @return A data frame with one row per error, or `NULL` when the axis has
#'   none. For the import axis, rows flagged by more than one source are
#'   deduplicated by (row, column), matching how the counts were taken.
#' @examples
#' specs <- DTAtools::DTAColumnSpecCollection(
#'   columns = list(
#'     ID = DTAtools::DTAColumnSpec(
#'       id = "ID", type = "SAS Char", length = 2, nullable = FALSE
#'     )
#'   )
#' )
#'
#' path <- file.path(tempdir(), "dta_spill_example.csv")
#' utils::write.csv(
#'   data.frame(ID = c("TOOLONG1", "TOOLONG2", "TOOLONG3")),
#'   path,
#'   row.names = FALSE
#' )
#'
#' # Hold at most one error row in memory; the other two spill to disk.
#' details <- validate_file_stream(specs, path, max_errors = 1, verbose = FALSE)
#' nrow(collect_full_errors(details, axis = "columnspec"))
#'
#' unlink(path)
#' @export
collect_full_errors <- function(details, axis = c("columnspec", "import")) {
  axis <- match.arg(axis)

  frame <- if (identical(axis, "columnspec")) {
    tryCatch(details$columnspec_errors$full_error, error = function(e) NULL)
  } else {
    tryCatch(details$import_errors, error = function(e) NULL)
  }

  if (is.null(frame)) {
    return(NULL)
  }

  dirs <- if (identical(axis, "columnspec")) {
    attr(frame, "spill_dir", exact = TRUE)
  } else {
    attr(frame, "spill_dirs", exact = TRUE)
  }
  dirs <- dirs[!is.na(dirs)]

  if (length(dirs) == 0) {
    return(frame)
  }

  existing <- dirs[dir.exists(dirs)]
  if (length(existing) < length(dirs)) {
    cli::cli_warn(c(
      "Some spilled error detail is no longer on disk; returning what remains.",
      i = "Spilled rows live in the R session's temporary directory and do not survive the session. The reported counts are unaffected."
    ))
  }

  parts <- unlist(
    lapply(existing, function(d) {
      sort(list.files(d, pattern = "^part-\\d+\\.rds$", full.names = TRUE), method = "radix")
    }),
    use.names = FALSE
  )

  if (length(parts) == 0) {
    return(frame)
  }

  out <- do.call(rbind, c(list(frame), lapply(parts, readRDS)))
  attr(out, "truncated") <- NULL
  attr(out, "spilled_rows") <- NULL
  attr(out, "spill_dir") <- NULL
  attr(out, "spill_dirs") <- NULL

  if (identical(axis, "import")) {
    # A cell flagged by both the typing and the rule axis is one error; the
    # in-memory merge deduplicated the retained rows, and this is the same
    # dedup extended over the spilled ones.
    out <- out[!duplicated(out[, c("row", "column"), drop = FALSE]), , drop = FALSE]
    out <- out[order(out$row, out$column, method = "radix"), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

#' @title Validate a Table from a Stream of Record Batches
#' @description
#' The streaming counterpart of `validate_table_detailed()`. Evaluates all three
#' axes -- column specs, rules, and import typing -- reading one batch at a
#' time, and returns the same `details` structure, so every existing consumer
#' (`results()`, `messages()`, `inspect()`, the Shiny app) works unchanged.
#'
#' Nothing here scales with the number of rows. Peak memory is bounded by the
#' batch size for the column spec axis, by the number of distinct keys for uniqueness
#' rules, by the number of distinct groups for grouped rules, and by the
#' retained-error cap.
#'
#' Row numbers are positions in the input, not in the batch a value happened to
#' fall in.
#' @param specs A `DTAColumnSpecCollection`.
#' @param reader An object with a `read_next_batch()` method.
#' @param verbose Logical. Print progress. While scanning, a line reporting the
#'   rows read so far and the current rate is emitted at most once every
#'   `getOption("DTAtools.progress_seconds", 30)` seconds, so a scan that runs
#'   for hours is distinguishable from a hang. The interval is measured from the
#'   start of the scan, so a run shorter than it prints nothing extra. There is
#'   no percentage and no ETA: a stream has no total row count to compute either
#'   from.
#' @param max_errors Integer, or `NULL` to retain everything. Cap on retained
#'   per-cell errors, defaulting to `getOption("DTAtools.max_errors", 10000L)`
#'   so that the option is honoured wherever a sink is created, not only at the
#'   outer entry points. `NULL` retains everything, matching the materialising
#'   path.
#' @param coerce Logical. Type each batch against the specs as it arrives,
#'   recording values that cannot be represented. This is the streaming
#'   equivalent of typing the table once at import.
#' @param fail_fast Logical. Stop at the first batch that shows any problem
#'   instead of scanning to the end. The result then carries a `partial_scan`
#'   attribute and axes that could not be settled report `NA`.
#' @param precomputed A list parallel to `specs@rules`: entry `i` is a
#'   finalise-shaped result (`id`/`valid`/`message`) for a rule already
#'   answered outside the batch loop -- today, a uniqueness rule computed by
#'   Arrow's grouped aggregation over the whole dataset (see
#'   [dta_stream_unique_precompute()]) -- or `NULL` for a rule this scan must
#'   still accumulate. Precomputed rules are skipped per batch and their
#'   result is spliced in at finalise, in rule order.
#' @param known_columns Character. The source's full column names, when the
#'   caller can read them without consuming the source. Used only when the
#'   stream yields no rows: rules are then evaluated once against an empty
#'   table with these columns, so a rule naming a column the table lacks
#'   still reports "could not be evaluated" exactly as the materialising path
#'   does -- previously a header-only file certified such rules as passed.
#' @return A `details` list of the same shape `validate_table_detailed()`
#'   returns.
#' @keywords internal
dta_validate_table_stream <- function(specs,
                                      reader,
                                      verbose = FALSE,
                                      max_errors = getOption("DTAtools.max_errors", 10000L),
                                      coerce = TRUE,
                                      fail_fast = FALSE,
                                      precomputed = list(),
                                      known_columns = character(0)) {
  rules_list <- tryCatch(specs@rules, error = function(e) NULL)
  if (is.null(rules_list)) {
    rules_list <- list()
  }

  states <- lapply(seq_along(rules_list), function(i) {
    pre <- if (i <= length(precomputed)) precomputed[[i]] else NULL
    if (is.null(pre)) {
      return(dta_rule_stream_init(rules_list[[i]]))
    }
    state <- new.env(parent = emptyenv())
    state$kind <- "precomputed"
    state$applicable <- TRUE
    state$result <- pre
    state
  })
  # Which columns each rule reads numerically, computed once for the whole
  # scan rather than re-derived (by re-parsing the rule's clause structure)
  # on every batch.
  rule_numeric_columns <- lapply(rules_list, function(r) {
    tryCatch(dta_rule_numeric_columns(r), error = function(e) character(0))
  })

  # Each column's schema is a pure function of its spec, so it is derived once
  # for the whole scan rather than re-derived (through several S7 dispatches)
  # on every batch.
  columnspec_schemas <- dta_compile_columnspec_schemas(specs)
  # Likewise each column's target R type for the coercion axis.
  spec_type_map <- dta_compile_spec_types(specs)

  columnspec_sink <- dta_error_sink(max_errors)
  carried_sink <- dta_error_sink(max_errors)
  rule_import_sink <- dta_error_sink(max_errors)
  # Which constraint each violation broke, accumulated per batch rather than
  # read off the collected frame at the end. The sink's cap spills rows past
  # `max_errors` to disk, so a tally taken from what it kept in memory would
  # under-report exactly on the dirty files the cap exists for -- and would then
  # report a check as passed because its only violations were the truncated
  # ones.
  columnspec_tally <- dta_empty_columnspec_tally()
  # A double, not an integer. This is the number of rows already consumed, and
  # the streaming path is built for files past `.Machine$integer.max` rows. An
  # integer accumulator returns `NA` with a warning rather than erroring, and
  # the `NA` is then added to EVERY reported row number below -- counts and the
  # pass/fail verdict still look authoritative while every row pointer is gone.
  # Doubles represent whole numbers exactly to 2^53. `dta_narrow_rows()` puts
  # the reported row numbers back to integer whenever they still fit.
  row_offset <- 0
  partial_scan <- FALSE

  # Progress, throttled by WALL TIME rather than by batch count. Batch cost
  # varies by orders of magnitude between files, so "every N batches" is either
  # silent for minutes or a flood, depending on the file. The clock starts here,
  # which is also why nothing is printed until a full interval has passed: a
  # short scan -- and the test suite -- stay exactly as quiet as before.
  progress_seconds <- getOption("DTAtools.progress_seconds", 30)
  progress_start <- Sys.time()
  progress_last <- progress_start

  if (isTRUE(verbose)) {
    cli::cli_h3("validating with column specs")
  }

  repeat {
    batch <- reader$read_next_batch()
    if (is.null(batch)) {
      break
    }

    df <- as.data.frame(batch)
    # batch is no longer needed once df has been created from it; drop it
    # promptly so it does not stay live for the whole batch iteration,
    # multiplying whatever batch_rows the caller chose.
    rm(batch)
    n_batch_rows <- nrow(df)
    if (n_batch_rows == 0) {
      next
    }

    # Import typing, per batch. The materialising path types the whole table
    # once and hangs the issues on it as an attribute; with no single table to
    # hang anything on, the issues accumulate here instead.
    if (isTRUE(coerce)) {
      # max_rows_per_column = Inf: nothing is dropped before it reaches the
      # sink, whose cap now spills overflow to disk instead of losing it. A
      # single batch bounds the frame anyway.
      coerced <- dta_coerce_table_to_specs(
        df, specs,
        type_map = spec_type_map, max_rows_per_column = Inf
      )
      df <- coerced$table
      issues <- coerced$issues
      # coerced is no longer needed once df and issues have been extracted; drop
      # it promptly so it does not stay live for the whole batch iteration,
      # multiplying whatever batch_rows the caller chose.
      rm(coerced)
      if (is.data.frame(issues)) {
        # Read the true count BEFORE touching the frame: import typing caps the
        # rows it retains per column but records how many there really were on
        # the frame itself, and modifying a column can drop that attribute.
        n_issues <- dta_import_error_count(issues)
        if (nrow(issues) > 0) {
          issues$row <- dta_narrow_rows(issues$row + row_offset)
        }
        if (n_issues > 0) {
          dta_error_sink_add(carried_sink, issues, n_total = n_issues)
        }
      }
    }

    # `schemas`: compiled once for the whole scan rather than re-derived per
    # batch. `summarise = FALSE`: only `full_error` is read here, and the
    # summary is recomputed once at the end over the whole collected frame.
    # Building it per batch was pure waste, and expensive waste -- the
    # aggregation groups by the offending value, so a dirty batch of distinct
    # bad cells cost seconds.
    schema_result <- dta_columnspec_errors(
      specs, df,
      schemas = columnspec_schemas, summarise = FALSE
    )
    columnspec_errs <- schema_result$full_error
    if (!is.null(columnspec_errs) && nrow(columnspec_errs) > 0) {
      columnspec_errs$row <- dta_narrow_rows(columnspec_errs$row + row_offset)
      # Tallied from the batch's whole frame, before the sink decides how much
      # of it to keep.
      columnspec_tally <- dta_columnspec_tally_add(columnspec_tally, columnspec_errs)
      dta_error_sink_add(columnspec_sink, columnspec_errs)
    }

    # Built once per batch rather than once per rule per batch: a column read
    # numerically by several rules would otherwise be strict-converted once
    # per rule that reads it, per batch.
    # `columns` is the same flattened, de-duplicated list the function would
    # have derived itself, handed over so the per-rule clause parse behind
    # `rule_numeric_columns` is not repeated on every batch. The function still
    # filters it against this batch's columns, so a rule naming an absent
    # column yields an empty cache exactly as before.
    numeric_cache <- dta_build_numeric_cache(
      df, rules_list,
      columns = unique(unlist(rule_numeric_columns, use.names = FALSE))
    )

    batch_rule_errs <- vector("list", length(rules_list))
    for (i in seq_along(rules_list)) {
      dta_rule_stream_update(states[[i]], rules_list[[i]], df, numeric_cache)

      # Sourced from the same columns the rule just read as numbers, so an
      # unrepresentable value is reported on both axes rather than moved
      # from one to the other. Called unguarded, exactly as apply_rules()
      # calls it: the old tryCatch(error = NULL) was vestigial (everything
      # that can genuinely throw runs in the unguarded cache build above, on
      # both paths) and would have silently reported import_valid = TRUE for
      # an input that makes the materialising path abort loudly.
      rule_errs <- dta_rule_import_errors(
        rules_list[[i]], df,
        numeric_cache = numeric_cache,
        columns = rule_numeric_columns[[i]]
      )
      if (is.data.frame(rule_errs) && nrow(rule_errs) > 0) {
        batch_rule_errs[[i]] <- rule_errs
      }
    }

    # Deduplicated PER BATCH, before the sink counts anything: two rules
    # reading the same column report the same unrepresentable cell, and every
    # such duplicate is batch-local (row numbers are unique across batches).
    # Counting per rule and deduplicating only the retained rows inflated
    # n_import_errors k-fold once the retention cap hid the duplicates -- the
    # materialising path, which dedups its full frame, reported half as many
    # errors for the same file.
    batch_rule_errs <- Filter(Negate(is.null), batch_rule_errs)
    if (length(batch_rule_errs) > 0) {
      rule_errs <- if (length(batch_rule_errs) == 1) {
        batch_rule_errs[[1]]
      } else {
        do.call(rbind, batch_rule_errs)
      }
      rule_errs <- rule_errs[
        !duplicated(rule_errs[, c("row", "column"), drop = FALSE]), ,
        drop = FALSE
      ]
      # Declared types stamped per batch, before the sink: rows the cap sends
      # to the spill would otherwise carry the placeholder storage type
      # forever, with no later chance to resolve it against the specs.
      rule_errs <- dta_apply_spec_declared_types(rule_errs, specs)
      rule_errs$row <- dta_narrow_rows(rule_errs$row + row_offset)
      dta_error_sink_add(rule_import_sink, rule_errs)
    }

    row_offset <- row_offset + n_batch_rows

    if (isTRUE(verbose) &&
      as.numeric(difftime(Sys.time(), progress_last, units = "secs")) >= progress_seconds) {
      progress_last <- Sys.time()
      elapsed <- as.numeric(difftime(progress_last, progress_start, units = "secs"))
      # A stream has no total row count, so there is deliberately no percentage
      # and no ETA here: both would be invented, and a made-up ETA on a scan
      # that runs for hours is worse than none.
      rate <- if (elapsed > 0) {
        paste0(
          format(round(row_offset / elapsed), big.mark = ",", scientific = FALSE, trim = TRUE),
          " rows/sec"
        )
      } else {
        "rate not yet measurable"
      }
      rows <- format(row_offset, big.mark = ",", scientific = FALSE, trim = TRUE)
      cli::cli_alert_info("scanned {rows} rows so far ({rate})")
    }

    if (isTRUE(fail_fast) &&
      (columnspec_sink$total > 0 ||
        carried_sink$total > 0 ||
        rule_import_sink$total > 0 ||
        any(vapply(states, dta_rule_stream_failing, logical(1))))) {
      partial_scan <- TRUE
      break
    }
  }

  # A stream that yielded no rows never ran a single rule update, so a rule
  # naming a column the table lacks was never discovered and finalised as
  # PASSED -- while the materialising path checks column presence regardless
  # of rows and fails the rule. One update against an empty table with the
  # source's real columns reproduces exactly the eager presence checks (every
  # rule function tests its columns before looking at rows), contributes no
  # counts, and costs nothing on the ordinary non-empty scan.
  if (row_offset == 0 && length(known_columns) > 0 && length(rules_list) > 0) {
    empty_df <- as.data.frame(
      stats::setNames(
        rep(list(character(0)), length(known_columns)),
        known_columns
      ),
      optional = TRUE, stringsAsFactors = FALSE
    )
    for (i in seq_along(rules_list)) {
      dta_rule_stream_update(states[[i]], rules_list[[i]], empty_df, NULL)
    }
  }

  full_error <- dta_error_sink_collect(columnspec_sink)
  summarised_error <- dta_summarise_columnspec_errors(full_error)
  has_columnspec_errors <- columnspec_sink$total > 0

  # A check with no violations is only a pass when the scan that looked for them
  # ran to the end over rows that existed. A `fail_fast` run stopped at the
  # first problem and a stream that yielded no rows never evaluated a
  # constraint; in both cases the checks that reported nothing reported nothing
  # about the whole table, which is not the same as reporting a pass.
  columnspec_settled <- if (partial_scan || row_offset == 0) character(0) else NULL
  columnspec_checks <- dta_columnspec_check_summary(
    columnspec_schemas,
    tally = columnspec_tally,
    settled = columnspec_settled
  )

  if (isTRUE(verbose)) {
    dta_report_columnspec_checks(
      columnspec_checks,
      unchecked_reason = if (partial_scan) {
        "the scan stopped at the first problem"
      } else {
        "the stream yielded no rows"
      }
    )
  }

  rule_results <- lapply(seq_along(rules_list), function(i) {
    dta_rule_stream_finalise(states[[i]], rules_list[[i]])
  })
  rule_errors <- Filter(function(x) !isTRUE(x$valid), rule_results)
  rules_valid <- length(rule_errors) == 0

  carried <- dta_error_sink_collect(carried_sink)
  if (!is.null(carried)) {
    # The sink is the authority on how many import-typing errors there were.
    # Whatever `n_import_errors` attribute survived the rbind belongs to one
    # batch's frame, and letting dta_merge_import_errors() read that instead
    # would count the capped rows on top of the sink total.
    attr(carried, "n_import_errors") <- carried_sink$total
  }
  rule_import <- dta_error_sink_collect(rule_import_sink)

  # Rows the retained-error cap kept out of the collected frames, counted so
  # the totals stay exact: a capped row is one error whose identity is gone
  # from the frame but not from the count. The rule sink's rows were already
  # deduplicated per batch (see the batch loop), so its total counts each
  # cell once -- the k-fold inflation this arithmetic used to bake in is
  # fixed at the source. The carried axis needs its term only when the cap
  # left nothing at all for the merge to count.
  carried_capped <- if (is.null(carried)) carried_sink$total else 0
  rule_capped <- rule_import_sink$total - NROW(rule_import)

  # No re-deduplication or type stamping here: cross-rule duplicates are
  # batch-local and were removed before the sink counted them, global row
  # numbers make cross-batch duplicates impossible, and declared types were
  # stamped per batch so spilled rows carry them too.

  import_errors <- dta_merge_import_errors(carried, rule_import)
  # dta_merge_import_errors() is the only place that knows a cell flagged on
  # both the import-typing axis and the rule-reading axis is one error, not
  # two. Summing the raw sink totals bypassed it, and could report more import
  # errors than `import_errors` had rows -- while the materialising path, which
  # counts the merged frame, reported the right number for the same input.
  # Capped rows were never available to deduplicate, so they are added back
  # unreduced: a truncated scan over-reports rather than under-reports.
  n_import_errors <- as.double(dta_import_error_count(import_errors)) +
    carried_capped + rule_capped
  import_valid <- n_import_errors == 0L
  if (n_import_errors == 0L) {
    import_errors <- NULL
  }

  # The merged frame lost the per-sink spill attributes in rbind; put the
  # pointers back so collect_full_errors() can reassemble the full detail.
  if (!is.null(import_errors) &&
    (carried_sink$spilled > 0 || rule_import_sink$spilled > 0)) {
    attr(import_errors, "spilled_rows") <-
      carried_sink$spilled + rule_import_sink$spilled
    attr(import_errors, "spill_dirs") <- c(
      carried_sink$spill_dir %||% NA_character_,
      rule_import_sink$spill_dir %||% NA_character_
    )
  }

  details <- list(
    ok = NA,
    columnspec_valid = !has_columnspec_errors,
    rules_valid = isTRUE(rules_valid),
    import_valid = isTRUE(import_valid),
    n_columnspec_errors = dta_narrow_count(columnspec_sink$total),
    n_rule_errors = length(rule_errors),
    n_import_errors = dta_narrow_count(n_import_errors),
    columnspec_errors = list(
      summarised_error = summarised_error,
      full_error = full_error
    ),
    columnspec_checks = columnspec_checks,
    rule_results = rule_results,
    rule_errors = rule_errors,
    import_errors = import_errors,
    result_version = 2L
  )

  if (partial_scan) {
    # The scan stopped at the first problem, so the rest of the file was never
    # read. A rule that has not failed YET has not passed -- a duplicate later
    # in the file was simply never seen -- so the axes that could not be
    # settled report NA rather than a reassuring TRUE. `ok` is unaffected:
    # dta_details_ok() requires all three to be TRUE, and NA is not.
    #
    # Reported failures are filtered to the CERTAIN ones -- exactly the
    # predicate dta_rule_stream_failing() encodes as "no later batch could
    # overturn this". A grouped `requires` constraint that looked violated in
    # the batches read can still be satisfied by an unread row, and reporting
    # it as a definite failure asserted the opposite of what a full scan
    # concludes; only monotone grouped losses (and counted, unsupported, or
    # not-applicable failures) are settled mid-scan.
    certain <- vapply(states, dta_rule_stream_failing, logical(1))
    details$rule_errors <- lapply(
      which(certain),
      function(i) dta_rule_stream_finalise(states[[i]], rules_list[[i]])
    )
    details$n_rule_errors <- length(details$rule_errors)
    details$rules_valid <- if (length(details$rule_errors) > 0) FALSE else NA
    details$import_valid <- if (n_import_errors > 0L) FALSE else NA
    # Only rules whose failure is certain are reported. Their failures are
    # real; the silence of the others is not evidence.
    details$rule_results <- details$rule_errors
    attr(details, "partial_scan") <- TRUE
  }

  details$ok <- dta_details_ok(details)

  # Total rows actually read, exposed as an attribute for callers that want to
  # report a rate (e.g. `dta_benchmark_end(rows = )`). `dta_as_validation_details()`
  # only sets a class on the list and never touches other attributes, so this
  # survives the call below either way; set here so it travels with `details`
  # regardless of which caller reads it.
  attr(details, "n_rows_scanned") <- row_offset

  # Tagged before returning, so `as.data.frame()` dispatches to the method that
  # flattens it. The materialising path leaves this to its callers, which is
  # workable when every caller is inside the package -- but this result is
  # handed straight to a user, and an untagged list makes as.data.frame() fail
  # with a row-count error that says nothing about the cause.
  dta_as_validation_details(details)
}

#' @title Fill In Declared Types on Import Errors
#' @description
#' Import errors carry the observed storage type as a placeholder; the column
#' spec's declared type replaces it where one exists. Shared with the
#' materialising path's collection step.
#' @param errors A data frame of import errors.
#' @param specs A `DTAColumnSpecCollection`.
#' @return The data frame, with `declared_type` filled in.
#' @keywords internal
dta_apply_spec_declared_types <- function(errors, specs = NULL) {
  if (is.null(errors) || nrow(errors) == 0) {
    return(errors)
  }
  declared <- vapply(
    errors$column,
    function(column) dta_spec_declared_type(specs, column),
    character(1),
    USE.NAMES = FALSE
  )
  errors$declared_type <- ifelse(is.na(declared), errors$declared_type, declared)
  errors
}

# ---- grouped rules, without holding the rows ---------------------------------
#
# A grouped rule asks, per group, whether a named condition holds for ANY row or
# for ALL rows. Both are associative reductions -- OR and AND -- so a group's
# answer can be folded batch by batch and never needs the group's rows present
# together.
#
# What the messages additionally need is row numbers, and only the first ten:
# beyond that they say "(+N more)". So each condition keeps a capped head of the
# row numbers it saw and a count of the rest.
#
# The accumulator is COLUMNAR, not one nested R list per group. Every distinct
# group is a dense integer id, and every per-group fact -- rows seen, any_true,
# all_true, true_n, the row-number heads -- lives at position `id` in a shared
# vector or matrix, one such vector per condition (and, for the row-number
# heads, per side). Folding a batch is then a handful of vectorised indexed
# assignments across every group the batch touched at once (`state$any_true[[
# cond]][gids] <- ...`), rather than an interpreted R-level loop revisiting one
# group -- and, inside it, one condition -- at a time. Measured, the state
# costs roughly 294 bytes per group per condition (the two 10-wide double head
# matrices are 160 of them) plus the rendered label, against 1.1-3 KB/group for
# the old per-group lists: about 2.5-3x less, and without the interpreted
# loop's per-batch cost.
#
# Memory is therefore proportional to the number of distinct GROUPS times the
# number of conditions, not to the number of rows. That is the same class as
# uniqueness, and unbounded in group cardinality rather than in file size -- an
# improvement over retaining every row of every column the rule reads, but not
# a constant.

DTA_GROUP_ROW_HEAD <- 10L

# Doubles a per-id vector's or matrix's capacity, so that appending ids across
# a scan is amortised O(1) rather than an O(groups) reallocation on every new
# group. A plain vector grows through `length<-`, which pads with NA of the
# vector's own storage mode and, unlike `c()`, keeps the class of any vector
# whose class ships a `length<-` method (Date, POSIXct, factor, difftime and
# integer64 all do). A class without one is silently unclassed -- which costs
# nothing here, because every vector this grows is a plain character, double,
# logical or integer.
# A matrix (the row-number heads) grows by rows only; its column count
# (DTA_GROUP_ROW_HEAD) never changes. Growth only guarantees CAPACITY -- its
# padding is NA and otherwise meaningless -- every field a freshly created id
# needs is set explicitly by the caller (see the new-id initialisation block
# in dta_group_stream_update()), so growth itself carries no initialisation
# contract.
dta_group_grow <- function(x, needed) {
  if (is.matrix(x)) {
    cur <- nrow(x)
    if (cur >= needed) {
      return(x)
    }
    new_cap <- max(needed, if (cur == 0L) 16L else cur * 2L)
    grown <- matrix(NA_real_, nrow = new_cap, ncol = ncol(x))
    if (cur > 0L) {
      grown[seq_len(cur), ] <- x
    }
    return(grown)
  }
  cur <- length(x)
  if (cur >= needed) {
    return(x)
  }
  new_cap <- max(needed, if (cur == 0L) 16L else cur * 2L)
  length(x) <- new_cap
  x
}

#' @title Start Accumulating a Grouped Rule
#' @param rule A grouped rule.
#' @return A mutable accumulator.
#' @keywords internal
dta_group_stream_init <- function(rule) {
  state <- new.env(parent = emptyenv())
  # fastmap rather than an environment, for the symbol-table reason given in
  # dta_rule_stream_init(). A grouped rule leaks the same way a keyed one does:
  # the key is the group label instead of the row key, but it is interned just
  # the same. It maps a group's key to its dense integer id -- the columnar
  # state below is then addressed by that id, never by the key directly.
  state$ids <- fastmap::fastmap()
  state$n <- 0L
  state$keys <- character(0)
  # The RENDERED label, built when a group is first seen -- not the group's
  # raw values. Storing the values instead would freeze their storage mode to
  # whichever batch happened to arrive first: a later batch of a different
  # type coerces the whole vector, silently re-rendering labels already
  # recorded (a numeric column promoted to character by an all-missing batch
  # renders 1e+05 again, the very thing dta_group_label_value() exists to
  # prevent) and, for a factor column, matching by label against the first
  # batch's level set and yielding NA. Rendering once, at first sight, is
  # also what the eager path does.
  state$labels <- character(0)
  # Rows seen per group. ONE vector, not one copy per condition: every
  # condition sees every row of the group, so the old per-condition n_seen
  # copies were always identical to each other. A double, not an integer --
  # the streaming path is built for files past `.Machine$integer.max` rows,
  # and integer accumulation there yields `NA` with a warning rather than an
  # error. An `n_seen` gone `NA` is not cosmetic: `scope_truth_vec()` in
  # dta_group_stream_finalise() reads `n_seen > 0 & all_true` for an "all"
  # scope, so the `NA` would flow through the constraint and read as `FALSE`
  # -- a group that genuinely violates the constraint reported as passing.
  # Doubles represent whole numbers exactly to 2^53.
  state$n_seen <- double(0)

  state$condition_names <- names(rule_get_slot(rule, "conditions"))
  # Slots resolved once per scan rather than re-materialised (rule_get_slot
  # converts the whole S7 object to a list per call) on every batch.
  state$group_by <- rule_get_slot(rule, "group_by")
  state$conditions <- rule_get_slot(rule, "conditions")

  # One vector (or, for the row-number heads, one matrix) per condition, by
  # id. false counts are deliberately not stored: they are DERIVED at read
  # time as `n_seen - true_n`, because per batch a false-side count is always
  # `n_seen_batch - true_count_batch` -- storing both was redundant by
  # construction in the old per-group state too.
  state$any_true <- stats::setNames(
    lapply(state$condition_names, function(...) logical(0)),
    state$condition_names
  )
  state$all_true <- stats::setNames(
    lapply(state$condition_names, function(...) logical(0)),
    state$condition_names
  )
  state$true_n <- stats::setNames(
    lapply(state$condition_names, function(...) double(0)),
    state$condition_names
  )
  # DOUBLE matrices, [capacity x DTA_GROUP_ROW_HEAD]: row numbers stay double
  # past 2^31 on purpose, for the same reason `n_seen` is a double above.
  # dta_group_stream_finalise() narrows them back to integer, when they fit,
  # only once, on the assembled `rows` field of each violation.
  state$true_head <- stats::setNames(
    lapply(state$condition_names, function(...) {
      matrix(NA_real_, nrow = 0, ncol = DTA_GROUP_ROW_HEAD)
    }),
    state$condition_names
  )
  state$false_head <- stats::setNames(
    lapply(state$condition_names, function(...) {
      matrix(NA_real_, nrow = 0, ncol = DTA_GROUP_ROW_HEAD)
    }),
    state$condition_names
  )
  state$true_head_n <- stats::setNames(
    lapply(state$condition_names, function(...) integer(0)),
    state$condition_names
  )
  state$false_head_n <- stats::setNames(
    lapply(state$condition_names, function(...) integer(0)),
    state$condition_names
  )

  # `fail_fast` needs to know mid-scan that a group has already lost, which is
  # only sound for a constraint no later batch can rescue. `mutually_exclusive`
  # under ANY/ANY is that constraint: once a row has made each side true,
  # neither can go back to false. Every other shape can flip -- an `all` scope
  # falls over on a later row, and a `requires` THEN can still be satisfied by
  # one -- so those are decided at finalise, as before.
  state$monotone <- Filter(
    function(constraint) {
      identical(constraint$type, "mutually_exclusive") &&
        identical(constraint$left_scope %||% "any", "any") &&
        identical(constraint$right_scope %||% "any", "any")
    },
    rule_get_slot(rule, "constraints")
  )
  state$certain_flag <- logical(0)
  state$certain <- 0L
  state
}

#' @title Fold One Batch into a Grouped Rule's Accumulator
#' @param state An accumulator from `dta_group_stream_init()`.
#' @param rule The grouped rule.
#' @param df A data frame holding one batch.
#' @param row_offset Numeric. Rows already consumed, so row numbers are global.
#'   A double at the call site, so it stays exact past `.Machine$integer.max`.
#' @param numeric_cache A named list from [dta_build_numeric_cache()] built for
#'   this batch, or `NULL` to convert each column on demand.
#' @return The accumulator, updated in place.
#' @keywords internal
dta_group_stream_update <- function(state, rule, df, row_offset = 0L, numeric_cache = NULL) {
  # Resolved once at init: rule_get_slot() re-materialises the whole S7 rule
  # per call, which is pure per-batch overhead over thousands of batches.
  group_by <- state$group_by
  conditions <- state$conditions

  missing_group_cols <- setdiff(group_by, names(df))
  if (length(missing_group_cols) > 0) {
    cli::cli_abort(
      c(
        "Rule {.val {rule@id}} cannot be evaluated as group_condition.",
        x = "Grouping column{?s} missing in input data: {.val {missing_group_cols}}.",
        i = "Available columns: {.val {names(df)}}."
      ),
      class = "dta_rule_not_applicable"
    )
  }

  if (nrow(df) == 0) {
    return(state)
  }

  split_key <- dta_group_key(df, group_by)
  grouped <- df[, group_by, drop = FALSE]

  # Same reasoning as the materialising path: every condition operator is
  # elementwise, so it is evaluated ONCE over the whole batch and folded into
  # each group's accumulator by group id, instead of re-evaluated per group
  # per condition against a `df[local_idx, , drop = FALSE]` copy.
  #
  # `unique()` and `match()` rather than `factor()`: this only needs group ids
  # that agree with `local_levels`, and building a factor additionally sorts
  # the levels, which this loop does not need. Visiting groups in order of
  # first appearance rather than in sorted order is safe ONLY because the
  # groups are keyed by string in `state$ids` and
  # `dta_group_stream_finalise()` sorts those keys before assembling the
  # message -- that is what keeps the streamed message identical to the
  # materialised one, which sees groups in `split()`'s sorted order. Do not
  # report violations straight out of this function without restoring the sort.
  local_levels <- unique(split_key)
  gid <- match(split_key, local_levels)
  n_groups <- length(local_levels)
  first_row <- match(local_levels, split_key)
  n_seen_batch <- tabulate(gid, nbins = n_groups)

  cond_hit <- lapply(state$condition_names, function(cond_name) {
    spec <- conditions[[cond_name]]
    mask <- tryCatch(
      evaluate_conditions(spec, df, numeric_cache),
      dta_rule_not_applicable = function(cnd) {
        # Word-for-word the abort rule_check_group_condition() raises for the
        # same failure, "defined as" bullet included: the two paths' rule
        # failure messages are documented as identical, and this one had
        # silently drifted (the streamed message lacked the definition text).
        cli::cli_abort(
          c(
            "Rule {.val {rule@id}} cannot evaluate condition {.field {cond_name}}.",
            x = "{conditionMessage(cnd)}",
            i = "Condition {.field {cond_name}} is defined as: {.val {paste(utils::capture.output(utils::str(spec, give.attr = FALSE)), collapse = ' ')}}"
          ),
          class = "dta_rule_not_applicable"
        )
      }
    )
    mask %in% TRUE
  })
  names(cond_hit) <- state$condition_names

  cond_n_true <- lapply(cond_hit, function(hit) tabulate(gid[hit], nbins = n_groups))

  # ---- local group -> dense global id --------------------------------------
  #
  # `local_levels` mixes groups this batch is the first to see (which need a
  # fresh id and their group_by values recorded) with groups an earlier batch
  # already assigned an id to (which need neither). `has()` answers that for
  # every local group in one vectorised fastmap call.
  new_mask <- !state$ids$has(local_levels)
  if (any(new_mask)) {
    new_keys <- local_levels[new_mask]
    new_ids <- state$n + seq_len(length(new_keys))
    needed <- state$n + length(new_keys)

    state$keys <- dta_group_grow(state$keys, needed)
    state$labels <- dta_group_grow(state$labels, needed)
    state$n_seen <- dta_group_grow(state$n_seen, needed)
    state$certain_flag <- dta_group_grow(state$certain_flag, needed)
    for (cond_name in state$condition_names) {
      state$any_true[[cond_name]] <- dta_group_grow(state$any_true[[cond_name]], needed)
      state$all_true[[cond_name]] <- dta_group_grow(state$all_true[[cond_name]], needed)
      state$true_n[[cond_name]] <- dta_group_grow(state$true_n[[cond_name]], needed)
      state$true_head[[cond_name]] <- dta_group_grow(state$true_head[[cond_name]], needed)
      state$false_head[[cond_name]] <- dta_group_grow(state$false_head[[cond_name]], needed)
      state$true_head_n[[cond_name]] <- dta_group_grow(state$true_head_n[[cond_name]], needed)
      state$false_head_n[[cond_name]] <- dta_group_grow(state$false_head_n[[cond_name]], needed)
    }

    # New-id initialisation, set explicitly rather than relied on from growth
    # (whose padding is NA and otherwise meaningless): unseen so far, so no
    # row has made anything true (`any_true` FALSE), and vacuously every row
    # seen so far satisfies each condition (`all_true` TRUE, matching
    # `all(logical(0))`).
    state$keys[new_ids] <- new_keys
    state$n_seen[new_ids] <- 0
    state$certain_flag[new_ids] <- FALSE
    for (cond_name in state$condition_names) {
      state$any_true[[cond_name]][new_ids] <- FALSE
      state$all_true[[cond_name]][new_ids] <- TRUE
      state$true_n[[cond_name]][new_ids] <- 0
      state$true_head_n[[cond_name]][new_ids] <- 0L
      state$false_head_n[[cond_name]][new_ids] <- 0L
    }
    # Rendered here, from THIS batch's own columns and this group's first row
    # in scan order -- the same value, rendered the same way, that the eager
    # group_label_for() in rule_check_group_condition() (evaluateRules.R)
    # produces. Values are rendered one at a time on purpose: `format()` over
    # a vector chooses one common layout for all of it, which would pad
    # unrelated groups' labels to a shared width.
    new_first_rows <- first_row[new_mask]
    state$labels[new_ids] <- vapply(
      seq_along(new_ids),
      function(j) {
        row <- new_first_rows[[j]]
        paste(
          vapply(
            group_by,
            function(col) paste0(col, "=", dta_group_label_value(grouped[[col]][row])),
            character(1)
          ),
          collapse = ", "
        )
      },
      character(1)
    )

    state$ids$mset(.list = stats::setNames(as.list(new_ids), new_keys))
    state$n <- needed
  }

  # Aligned with `local_levels` (and so with `gid`'s levels) position for
  # position: `gids[g]` is the dense id of the group at `local_levels[g]`.
  gids <- unlist(state$ids$mget(local_levels), use.names = FALSE)

  for (cond_name in state$condition_names) {
    nt <- cond_n_true[[cond_name]]
    state$any_true[[cond_name]][gids] <- state$any_true[[cond_name]][gids] | (nt > 0)
    state$all_true[[cond_name]][gids] <- state$all_true[[cond_name]][gids] &
      (n_seen_batch > 0 & nt == n_seen_batch)
    state$true_n[[cond_name]][gids] <- state$true_n[[cond_name]][gids] + nt
  }
  state$n_seen[gids] <- state$n_seen[gids] + n_seen_batch

  # ---- row-number heads, built lazily ---------------------------------------
  #
  # The `which()` + `split()` work below runs only for a (condition, side)
  # that has, in THIS batch, at least one group still short of
  # DTA_GROUP_ROW_HEAD rows with matching rows to offer it. On a long scan
  # every group's heads fill within the first few batches it appears in,
  # after which this is a no-op for the rest of the file -- unlike the split
  # it replaces, which the old code built for every condition and side on
  # every batch regardless of whether any head still had room.
  for (cond_name in state$condition_names) {
    hit <- cond_hit[[cond_name]]
    nt <- cond_n_true[[cond_name]]

    # The head matrix and its counts are pulled into locals for the whole loop
    # and stored back ONCE. Writing through `state$true_head[[cond]][id, ]`
    # inside the loop instead duplicates the entire capacity-sized matrix (and
    # the count vector) on every iteration -- `state` is a function argument,
    # so the nested replacement has no in-place path -- which made a batch
    # cost O(groups accumulated) rather than O(groups touched) and turned the
    # scan quadratic in group cardinality: measured 71s vs 0.8s at 40,000
    # groups. Do not inline these back into the loop.
    needy_true <- which(state$true_head_n[[cond_name]][gids] < DTA_GROUP_ROW_HEAD & nt > 0)
    if (length(needy_true) > 0) {
      head_matrix <- state$true_head[[cond_name]]
      head_counts <- state$true_head_n[[cond_name]]
      rows_by_group <- split(which(hit), factor(gid[hit], levels = seq_len(n_groups)))
      for (g in needy_true) {
        id <- gids[g]
        local_rows <- rows_by_group[[g]]
        hn <- head_counts[id]
        k <- min(DTA_GROUP_ROW_HEAD - hn, length(local_rows))
        if (k > 0) {
          head_matrix[id, hn + seq_len(k)] <- local_rows[seq_len(k)] + row_offset
          head_counts[id] <- hn + k
        }
      }
      state$true_head[[cond_name]] <- head_matrix
      state$true_head_n[[cond_name]] <- head_counts
    }

    needy_false <- which(
      state$false_head_n[[cond_name]][gids] < DTA_GROUP_ROW_HEAD & (n_seen_batch - nt) > 0
    )
    if (length(needy_false) > 0) {
      head_matrix <- state$false_head[[cond_name]]
      head_counts <- state$false_head_n[[cond_name]]
      rows_by_group <- split(which(!hit), factor(gid[!hit], levels = seq_len(n_groups)))
      for (g in needy_false) {
        id <- gids[g]
        local_rows <- rows_by_group[[g]]
        hn <- head_counts[id]
        k <- min(DTA_GROUP_ROW_HEAD - hn, length(local_rows))
        if (k > 0) {
          head_matrix[id, hn + seq_len(k)] <- local_rows[seq_len(k)] + row_offset
          head_counts[id] <- hn + k
        }
      }
      state$false_head[[cond_name]] <- head_matrix
      state$false_head_n[[cond_name]] <- head_counts
    }
  }

  # ---- monotone certainty ----------------------------------------------------
  #
  # See dta_group_stream_init(): once a group has made both sides of a
  # `mutually_exclusive` ANY/ANY constraint true, no later batch can undo
  # that, so `fail_fast` can call it mid-scan. Judged AFTER the fold above, on
  # the post-batch truth -- matching the old per-group loop, which updated
  # `entry$conds` before checking `state$monotone`.
  #
  # Looped per CONSTRAINT rather than per group: there are normally a handful
  # of constraints against thousands of groups, so this is the cheap order to
  # put the interpreted loop on. Processing constraints one at a time, rather
  # than all of them in one vectorised pass over an unmodified snapshot, means
  # a group that satisfies two monotone constraints within the same batch is
  # still counted into `certain` exactly once -- the `!state$certain_flag[
  # gids]` guard sees the FIRST constraint's update before the second
  # constraint is evaluated, matching the old `any()`-over-constraints,
  # increment-by-one-per-group semantics.
  for (constraint in state$monotone) {
    lost <- !state$certain_flag[gids] &
      state$any_true[[constraint$left]][gids] &
      state$any_true[[constraint$right]][gids]
    if (any(lost)) {
      state$certain_flag[gids[lost]] <- TRUE
      state$certain <- state$certain + sum(lost)
    }
  }

  # There is deliberately no group budget any more: the old DTAtools.max_groups
  # abort threw away a multi-hour scan wholesale once group cardinality crossed
  # a line, which proved strictly worse than the memory growth it guarded.
  # Memory here is proportional to distinct groups times conditions and is
  # documented at the entry points instead of enforced mid-scan.
  state
}

#' @title Turn a Grouped Rule's Accumulator into a Result
#' @param state An accumulator that has seen every batch.
#' @param rule The grouped rule.
#' @return A list with `id`, `valid` and `message`, matching what
#'   `rule_check_group_condition()` returns for the same data.
#' @keywords internal
dta_group_stream_finalise <- function(state, rule) {
  constraints <- rule_get_slot(rule, "constraints")
  group_by <- state$group_by
  violations <- list()

  n_groups <- state$n

  # The materialising path orders groups by radix-sorted key (C-locale byte
  # order -- see the factor() levels in rule_check_group_condition()), so the
  # same sort here keeps the assembled message identical. Radix rather than
  # the session locale, because locale collation ordered the same violations
  # differently on a de_DE dev machine than under CI's C collation, and a
  # non-stable locale sort could even split collation ties differently
  # between the two paths. Change the two sites together. `state$keys` is
  # sliced to `state$n` first: its backing vector can be geometrically
  # over-allocated past the number of ids actually assigned (see
  # dta_group_grow()), and an unassigned slot's content is undefined.
  sorted <- order(state$keys[seq_len(n_groups)], method = "radix")

  # A (condition, side)'s row-number head as a plain double vector, or
  # `double(0)` for a group that never matched on that side.
  # `dta_format_group_rows()` reads `total == 0` before ever looking at the
  # head, so an empty vector here is never actually rendered -- and a
  # `true_n`/`false_n` of 0 is exactly when `true_head_n`/`false_head_n` is
  # also 0 (see the "needy" gate in dta_group_stream_update(): a head only
  # ever grows when this batch has at least one matching row to offer it).
  head_vec <- function(cond_name, id, which) {
    if (identical(which, "true")) {
      hn <- state$true_head_n[[cond_name]][id]
      if (hn == 0L) {
        return(double(0))
      }
      state$true_head[[cond_name]][id, seq_len(hn)]
    } else {
      hn <- state$false_head_n[[cond_name]][id]
      if (hn == 0L) {
        return(double(0))
      }
      state$false_head[[cond_name]][id, seq_len(hn)]
    }
  }

  # The false count is derived, not stored: see dta_group_stream_init().
  side_total <- function(cond_name, id, which) {
    if (identical(which, "true")) {
      state$true_n[[cond_name]][id]
    } else {
      state$n_seen[id] - state$true_n[[cond_name]][id]
    }
  }

  # Whether a violation's `rows` are only the head of a longer list. The
  # capped head is what keeps memory proportional to groups rather than to
  # rows, so the streamed `rows` can be shorter than the materialising
  # path's -- but silently shorter is a trap, and both paths therefore carry
  # this flag.
  side_truncated <- function(cond_name, id, which) {
    hn <- if (identical(which, "true")) {
      state$true_head_n[[cond_name]][id]
    } else {
      state$false_head_n[[cond_name]][id]
    }
    hn < side_total(cond_name, id, which)
  }

  fmt <- function(cond_name, id, which = "true") {
    dta_format_group_rows(
      head_vec(cond_name, id, which), side_total(cond_name, id, which), DTA_GROUP_ROW_HEAD
    )
  }

  # Vectorised per-(condition, scope) truth across ALL groups at once -- one
  # indexed lookup per group, not the label/row-evidence assembly --
  # mirroring the materialising path's `cond_any_true`/`cond_all_true`. This
  # is what lets the loop below only ever do per-group work (building
  # labels, formatting row evidence, composing messages) for groups that
  # actually violate something, instead of for every group seen in the
  # stream. "all" requires the group to have had at least one row, matching
  # the materialising path where all(logical(0)) would otherwise be
  # vacuously TRUE. Indexing by `sorted` both selects the valid ids (its
  # values never exceed `n_groups`, so the vector's own possible
  # over-allocation past `state$n` is never read) and reorders into the
  # radix-sorted group order in the same step.
  scope_truth_vec <- function(cond_name, scope) {
    if (!cond_name %in% state$condition_names) {
      cli::cli_abort(
        c(
          "Rule {.val {rule@id}} references an unknown condition.",
          x = "Constraint refers to condition {.field {cond_name}}, which is not defined.",
          i = "Defined conditions: {.val {state$condition_names}}."
        ),
        class = "dta_rule_not_applicable"
      )
    }
    if (identical(scope, "all")) {
      (state$n_seen[sorted] > 0) & state$all_true[[cond_name]][sorted]
    } else {
      state$any_true[[cond_name]][sorted]
    }
  }

  constraint_viol <- vector("list", length(constraints))
  for (ci in seq_along(constraints)) {
    constraint <- constraints[[ci]]
    ctype <- constraint$type

    if (identical(ctype, "mutually_exclusive")) {
      left_truth <- scope_truth_vec(constraint$left, constraint$left_scope %||% "any")
      right_truth <- scope_truth_vec(constraint$right, constraint$right_scope %||% "any")
      constraint_viol[[ci]] <- left_truth & right_truth
    } else if (identical(ctype, "requires")) {
      if_truth <- scope_truth_vec(constraint[["if"]], constraint$if_scope %||% "any")
      then_truth <- scope_truth_vec(constraint[["then"]], constraint$then_scope %||% "any")
      constraint_viol[[ci]] <- if_truth & !then_truth
    } else {
      constraint_viol[[ci]] <- logical(n_groups)
    }
  }

  violating_groups <- sort(unique(unlist(lapply(constraint_viol, which), use.names = FALSE)))

  for (g in violating_groups) {
    id <- sorted[g]
    # Rendered when the group was first seen (see dta_group_stream_update()),
    # through dta_group_label_value() -- the renderer the eager
    # group_label_for() in rule_check_group_condition() (evaluateRules.R)
    # also uses, so the two paths' labels are identical by construction.
    label <- state$labels[[id]]

    for (ci in seq_along(constraints)) {
      if (!isTRUE(constraint_viol[[ci]][g])) {
        next
      }
      constraint <- constraints[[ci]]
      ctype <- constraint$type

      if (identical(ctype, "mutually_exclusive")) {
        left <- constraint$left
        right <- constraint$right
        message <- constraint$message %||%
          sprintf(
            "In group [%s]: \"%s\" and \"%s\" must not both occur, but both were found (rows matching \"%s\": %s; rows matching \"%s\": %s).",
            label,
            left,
            right,
            left, fmt(left, id, "true"),
            right, fmt(right, id, "true")
          )
        violations[[length(violations) + 1L]] <- list(
          constraint_id = constraint$id,
          group = label,
          message = message,
          rows = dta_narrow_rows(sort(unique(c(
            head_vec(left, id, "true"), head_vec(right, id, "true")
          )))),
          rows_truncated = side_truncated(left, id, "true") || side_truncated(right, id, "true")
        )
      } else if (identical(ctype, "requires")) {
        if_name <- constraint[["if"]]
        then_name <- constraint[["then"]]
        then_scope <- constraint$then_scope %||% "any"

        then_scope_reason <- if (identical(then_scope, "all")) {
          sprintf("rows %s do not satisfy \"%s\"", fmt(then_name, id, "false"), then_name)
        } else {
          sprintf("no row in the group satisfies \"%s\"", then_name)
        }
        message <- constraint$message %||%
          sprintf(
            "In group [%s]: when \"%s\" occurs (rows: %s), \"%s\" must also hold, but it does not (%s).",
            label,
            if_name,
            fmt(if_name, id, "true"),
            then_name,
            then_scope_reason
          )
        then_failed <- if (identical(then_scope, "all")) {
          head_vec(then_name, id, "false")
        } else {
          double(0)
        }
        violations[[length(violations) + 1L]] <- list(
          constraint_id = constraint$id,
          group = label,
          message = message,
          rows = dta_narrow_rows(sort(unique(c(head_vec(if_name, id, "true"), then_failed)))),
          rows_truncated = side_truncated(if_name, id, "true") ||
            (identical(then_scope, "all") && side_truncated(then_name, id, "false"))
        )
      }
    }
  }

  if (length(violations) == 0) {
    return(list(id = rule@id, valid = TRUE, message = NULL))
  }

  summary <- sprintf(
    "Rule '%s': %d group constraint violation%s found across %d group%s.",
    rule@id,
    length(violations),
    if (length(violations) == 1) "" else "s",
    length(unique(vapply(violations, function(v) v$group, character(1)))),
    if (length(unique(vapply(violations, function(v) v$group, character(1)))) == 1) "" else "s"
  )
  details <- vapply(violations, function(v) v$message, character(1))

  list(
    id = rule@id,
    valid = FALSE,
    message = paste(c(summary, details), collapse = " "),
    details = violations
  )
}

# ---- detecting that a table changed ------------------------------------------

#' @title A Signal That a Table Has Changed
#' @description
#' Validation results are cached against a signature of the table they were
#' produced from, so an unchanged table is not revalidated. The signature for a
#' materialised table is a hash of its contents, which is exact but requires the
#' contents -- it serialises the whole table to compute.
#'
#' A lazy dataset cannot afford that: hashing an 80 GB table writes 80 GB to
#' disk before any validation happens, which defeats the purpose of not loading
#' it. For those, identity comes from the files behind it -- their names, sizes
#' and modification times -- plus the column names. That is cheap at any size.
#'
#' The trade is honest: file metadata can in principle miss an edit that
#' preserves size and timestamp. Returning `NULL` when identity cannot be
#' established at all is treated by callers as "assume changed", so the failure
#' direction is revalidating unnecessarily rather than skipping a table that
#' needs it.
#' @param x An Arrow `Table`, `Dataset`, or other table representation.
#' @return A single string, or `NULL` when no identity can be established.
#' @keywords internal
dta_table_change_signal <- function(x) {
  if (inherits(x, "Table")) {
    return(dta_hash_object(as.data.frame(x)))
  }

  if (inherits(x, "Dataset")) {
    files <- tryCatch(x$files, error = function(e) character(0))
    if (length(files) == 0) {
      return(NULL)
    }
    info <- file.info(files)
    return(dta_hash_object(list(
      files = files,
      size = info$size,
      mtime = info$mtime,
      columns = names(x$schema)
    )))
  }

  # A reader is consumable: reading it to identify it would spend the very
  # thing the caller needs. It has no stable identity, so it always revalidates.
  NULL
}

#' @title Is This Table Lazy?
#' @param x A table representation.
#' @return `TRUE` when the table is scanned rather than held in memory.
#' @keywords internal
dta_table_is_lazy <- function(x) {
  inherits(x, "Dataset") ||
    inherits(x, "arrow_dplyr_query") ||
    inherits(x, "RecordBatchReader")
}

#' @title Column Names of a Table, However It Is Held
#' @description
#' The column names of a table representation, read without scanning it.
#'
#' The structural gate decides a missing required column from the header alone,
#' so it has to know the columns BEFORE any row is read -- and by then the
#' table may be an Arrow `Table`, a `Dataset`, an `arrow_dplyr_query`, a
#' `RecordBatchReader` or a plain data frame. `names()` answers for all of
#' them, and on a reader it reports the schema without pulling a batch, so a
#' consumable source is not spent merely by being inspected. `names(x$schema)`
#' is not a substitute: an `arrow_dplyr_query` has no `$schema`.
#'
#' Anything else yields `character(0)`, which callers read as "not knowable
#' cheaply" and answer by scanning exactly as they always did. That is
#' deliberately not an error: an unfamiliar holding is a reason to fall back,
#' not a reason to fail.
#' @param table A table representation.
#' @return Character. The column names, or `character(0)` when they cannot be
#'   determined cheaply.
#' @keywords internal
dta_table_column_names <- function(table) {
  known <- inherits(table, "data.frame") ||
    inherits(table, "ArrowTabular") ||
    inherits(table, "Dataset") ||
    inherits(table, "arrow_dplyr_query") ||
    inherits(table, "RecordBatchReader")
  if (!known) {
    return(character(0))
  }

  names <- tryCatch(names(table), error = function(e) NULL)
  if (!is.character(names)) {
    return(character(0))
  }
  names
}

#' @title Validate a Table However It Is Held
#' @description
#' Dispatches to the streaming path for a lazy table and the materialising path
#' for one already in memory, returning the same details either way. This is
#' what lets `check()` accept both without its callers knowing which they have.
#' @param specs A `DTAColumnSpecCollection`.
#' @param table An Arrow `Table`, `Dataset`, or reader.
#' @param verbose Logical. Print progress.
#' @param batch_rows Integer. Rows per batch when scanning.
#' @param max_errors Integer, or `NULL` to hold everything in memory. Cap on
#'   the per-cell error detail held in RAM, defaulting to 10000 and
#'   configurable via `options(DTAtools.max_errors = )`. The default is finite
#'   because the error sinks hold one row per bad cell: on a large dirty file,
#'   unbounded retention exhausts memory exactly as holding the data would.
#'   Nothing is lost, though: rows past the cap spill to a session-temporary
#'   directory and [collect_full_errors()] reassembles the complete detail;
#'   counts and the pass/fail verdict are exact either way, and a frame whose
#'   in-memory head is incomplete is flagged as such.
#' @param use_threads Logical. Whether Arrow's Scanner should use multiple
#'   threads for I/O and decompression. Arrow buffers batches ahead of R in its
#'   own C++ pool, outside the R heap, so single-threaded scanning is the lever
#'   when RSS rather than speed is the constraint.
#' @param fail_fast Logical. Stop at the first batch that shows any problem
#'   instead of scanning to the end. The report is then explicitly incomplete
#'   and carries a `partial_scan` attribute. Ignored for a table already held in
#'   memory, which is validated in one pass.
#' @param on_missing_column One of `"scan"` or `"stop"`. `"scan"`, the default,
#'   preserves existing behaviour: a column the specs require but the table
#'   lacks is reported once per row. `"stop"` decides it from the column names
#'   alone and reads nothing.
#'
#'   Note this gate runs ahead of the lazy/in-memory dispatch, so unlike
#'   `fail_fast` it applies to a materialised table as well -- the column names
#'   are available whatever the holding. When they cannot be obtained without
#'   consuming the table, the gate is skipped and the scan proceeds as usual.
#' @return A validation details list.
#' @keywords internal
dta_validate_any_table <- function(specs,
                                   table,
                                   verbose = FALSE,
                                   batch_rows = 131072L,
                                   max_errors = getOption("DTAtools.max_errors", 10000L),
                                   use_threads = TRUE,
                                   fail_fast = FALSE,
                                   on_missing_column = c("scan", "stop")) {
  on_missing_column <- match.arg(on_missing_column)

  # The structural gate, applied here rather than only at the file entry point,
  # so it is reachable from check() -- which is where a caller with a 60 GB
  # table actually stands. It needs the column names before a row is read; when
  # the holding cannot be asked for them cheaply, the gate is skipped and the
  # scan proceeds exactly as it always did.
  if (identical(on_missing_column, "stop")) {
    column_names <- dta_table_column_names(table)
    if (length(column_names) > 0) {
      findings <- dta_structure_findings(specs, column_names)
      if (!findings$ok) {
        if (isTRUE(verbose)) {
          cli::cli_alert_danger(
            "Missing required column{?s}: {.field {findings$missing}}. Stopping without reading the table."
          )
        }
        structural <- dta_structural_failure_details(
          findings,
          schemas = dta_compile_columnspec_schemas(specs)
        )
        # The same per-check report the scan paths print. Presence was decided
        # from the header; the other checks were not reached, and saying so is
        # the point -- otherwise this early return is the one exit that leaves a
        # reader guessing what was and was not examined.
        if (isTRUE(verbose)) {
          dta_report_columnspec_checks(
            structural$columnspec_checks,
            unchecked_reason = "the table was not read"
          )
        }
        return(structural)
      }
    }
  }

  if (!dta_table_is_lazy(table)) {
    return(validate_table_detailed(specs, as.data.frame(table), verbose = verbose))
  }

  rules_list <- tryCatch(specs@rules, error = function(e) NULL)
  if (is.null(rules_list)) {
    rules_list <- list()
  }

  # The source's full column names, read without consuming it: they drive the
  # empty-stream rule presence check inside the driver, and they are taken
  # BEFORE any projection so a rule naming an unprojected column is still
  # judged against what the file really has.
  column_names <- dta_table_column_names(table)

  # Eligible uniqueness rules are answered by Arrow's grouped aggregation over
  # the whole source before the batch scan -- the distinct keys then live in
  # the C++ engine instead of an R hash that grows with key cardinality.
  # Never for a reader: it is consumable, and the precompute would spend it.
  precomputed <- if (inherits(table, "RecordBatchReader")) {
    list()
  } else {
    dta_stream_unique_precompute(specs, table, rules_list)
  }

  reader <- if (inherits(table, "RecordBatchReader")) {
    table
  } else {
    # Projection: columns nothing reads are never parsed, converted, or
    # materialised into R. NULL means "cannot narrow" (unknown rule shape, or
    # nothing to gain) and scans every column exactly as before.
    projection <- if (inherits(table, "Dataset")) {
      dta_scan_projection(specs, rules_list, names(table$schema))
    } else {
      NULL
    }
    if (is.null(projection)) {
      arrow::Scanner$create(
        table,
        batch_size = batch_rows, use_threads = use_threads
      )$ToRecordBatchReader()
    } else {
      arrow::Scanner$create(
        table,
        projection = projection,
        batch_size = batch_rows, use_threads = use_threads
      )$ToRecordBatchReader()
    }
  }

  dta_validate_table_stream(
    specs,
    reader,
    verbose = verbose,
    max_errors = max_errors,
    coerce = TRUE,
    fail_fast = fail_fast,
    precomputed = precomputed,
    known_columns = column_names
  )
}

# ---- the structural gate -----------------------------------------------------
#
# Some failures are decidable from the column names alone, before a single row
# is read. A column the specs require but the file does not have is the clearest
# case: every per-row check on it is undefined, and scanning 400 million rows to
# discover it is both slow and less useful than saying so immediately.
#
# The full scan reports a missing column once per ROW, because the generated
# schema made the property required of every element. That is faithful, and it
# is retained as the default. But it is a poor way to learn that a column is
# absent, so a caller can ask to be told structurally instead.

#' @title Structural Findings from Column Names Alone
#' @description
#' Compares the columns a spec collection declares against the columns a file
#' actually has. Costs nothing beyond reading the header, so it can run before
#' any scan.
#'
#' Unexpected columns -- present in the file, absent from the specs -- are
#' reported here and nowhere else; the per-row checks have no way to notice a
#' column no spec describes.
#' @param specs A `DTAColumnSpecCollection`.
#' @param column_names Character. The columns the file actually has.
#' @return A list with `missing`, `unexpected` and `ok`.
#' @keywords internal
dta_structure_findings <- function(specs, column_names) {
  columns <- tryCatch(specs@columns, error = function(e) NULL)
  declared <- if (is.null(columns) || length(columns) == 0) {
    character(0)
  } else {
    # Per-column fallback, not all-or-nothing: a PARTIALLY named collection
    # (reachable only by validator bypass, but defended against by every
    # sibling lookup) would otherwise contribute "" as a declared column and
    # reject a sound file with "must have required property ''".
    nm <- names(columns)
    if (is.null(nm)) {
      nm <- rep("", length(columns))
    }
    ids <- vapply(
      columns,
      function(s) tryCatch(as.character(s@id)[[1]], error = function(e) NA_character_),
      character(1),
      USE.NAMES = FALSE
    )
    fallback <- is.na(nm) | !nzchar(nm)
    nm[fallback] <- ids[fallback]
    nm[!is.na(nm) & nzchar(nm)]
  }

  missing <- setdiff(declared, column_names)
  unexpected <- setdiff(column_names, declared)

  list(
    missing = missing,
    unexpected = unexpected,
    ok = length(missing) == 0
  )
}

#' @title Structural Findings as Column Spec Errors
#' @description
#' Renders structural findings in the same shape the per-row column spec axis uses,
#' so a caller that stopped early still receives a recognisable error frame.
#' Row is `NA`: the finding is about the file, not about any row in it.
#' @param findings A list from `dta_structure_findings()`.
#' @return A data frame of errors, or `NULL` when the structure is sound.
#' @keywords internal
dta_structure_errors <- function(findings) {
  if (length(findings$missing) == 0) {
    return(NULL)
  }

  out <- data.frame(
    row = NA_integer_,
    column = NA_character_,
    keyword = "required",
    message = paste0("must have required property '", findings$missing, "'"),
    columnspec = findings$missing,
    data = NA_character_,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

# ---- reading a file without materialising it ---------------------------------

#' @title Open a Delimited File as a Lazy Dataset
#' @description
#' Opens a delimited file for scanning rather than reading it into memory. The
#' parse options and the spec-driven column types are the same ones the eager
#' reader uses, so the columns are typed identically -- the difference is only
#' that nothing is read until a scan asks for it.
#' @param path Character. Path to the file.
#' @param specs A `DTAColumnSpecCollection` or `NULL`. Declared types decide how
#'   columns are parsed; without it every column is inferred.
#' @param delim Character. The field separator.
#' @param quote Character. The quoting character.
#' @param has_header Logical. Whether the first line names the columns.
#' @return An `arrow::Dataset`.
#' @keywords internal
dta_open_delimited_dataset <- function(path,
                                       specs = NULL,
                                       delim = ",",
                                       quote = "\"",
                                       has_header = TRUE) {
  arrow::open_delim_dataset(
    path,
    delim = delim,
    quote = quote,
    col_names = has_header,
    col_types = dta_reader_col_types(specs, has_header)
  )
}

#' @title Open a File for Validation, Whatever Its Format
#' @description
#' Opens a path as a lazy dataset, choosing the reader from the path itself: a
#' directory or a `.parquet` file is read as Parquet, anything else as
#' delimited text.
#' @param path Character. Path to a file or to a Parquet directory.
#' @param specs A `DTAColumnSpecCollection` or `NULL`.
#' @param delim,quote,has_header Delimited-text parse options, ignored for
#'   Parquet, which carries its own schema.
#' @return An `arrow::Dataset`.
#' @keywords internal
dta_open_validation_dataset <- function(path,
                                        specs = NULL,
                                        delim = ",",
                                        quote = "\"",
                                        has_header = TRUE) {
  is_parquet <- dir.exists(path) ||
    grepl("\\.parquet$", path, ignore.case = TRUE)

  if (is_parquet) {
    # Parquet stores its own types, so the spec-driven column types that keep a
    # delimited reader from inferring "007" as 7 are unnecessary here: the
    # types were fixed when the cache was written.
    return(arrow::open_dataset(path, format = "parquet"))
  }

  # The normalized opener, exactly as load_file()'s lazy path uses: without it
  # this entry point kept a padded header's raw names, so the spec-keyed type
  # pinning matched nothing and the structural gate reported the clean name
  # missing plus the padded name unexpected -- for a file that validated clean
  # through load_file().
  dta_open_normalized_dataset(
    path,
    specs = specs,
    delim = delim,
    quote = quote,
    has_header = has_header
  )
}

#' @title Cache a Delimited File as Parquet
#' @description
#' Rewrites a delimited file as a Parquet dataset, once, so that repeated
#' validations read a columnar format instead of re-parsing text. The
#' conversion streams: the file is scanned in batches and written out, never
#' materialised.
#'
#' @section Whether this is worth doing:
#' Measured, on a 500,000-row file of 20 columns where the specs read every
#' column: validating from the cache was **0.95x the speed** of validating the
#' text — that is, slightly slower — on top of a one-off conversion. On that
#' shape the cache never repays itself, and `benchmarks/bench_parquet.R`
#' reports exactly that.
#'
#' The reason is that parsing is not the bottleneck. Evaluating the constraints
#' in R dominates, so a format that only makes reading cheaper cannot help.
#'
#' What that benchmark does *not* test is the case Parquet is strongest at: a
#' wide file where the specs and rules read only a few of its columns, so the
#' rest need never be read at all. If your data looks like that, measure it.
#' Otherwise, do not convert.
#'
#' The columns are typed by the specs during conversion, exactly as they would
#' be when validating the text directly, so the cache preserves the declared
#' types rather than Parquet's own inference of them.
#' @param specs A `DTAColumnSpecCollection`, used to type the columns.
#' @param path Character. Path to the delimited file.
#' @param cache_path Character or `NULL`. Where to write. Defaults to the input
#'   path with a `_parquet` suffix.
#' @param delim,quote,has_header Parse options for the input file.
#' @param compression Character. Parquet compression codec.
#' @return The cache path, invisibly.
#' @examples
#' specs <- DTAtools::DTAColumnSpecCollection(
#'   columns = list(
#'     ID = DTAtools::DTAColumnSpec(
#'       id = "ID", type = "SAS Char", length = 8, nullable = FALSE
#'     )
#'   )
#' )
#'
#' csv <- file.path(tempdir(), "dta_cache_example.csv")
#' utils::write.csv(data.frame(ID = c("A001", "A002")), csv, row.names = FALSE)
#'
#' cached <- cache_as_parquet(specs, csv)
#' details <- validate_file_stream(specs, cached, verbose = FALSE)
#' details$ok
#'
#' unlink(csv)
#' unlink(cached, recursive = TRUE)
#' @export
cache_as_parquet <- function(specs,
                             path,
                             cache_path = NULL,
                             delim = ",",
                             quote = "\"",
                             has_header = TRUE,
                             compression = "zstd") {
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }

  if (is.null(cache_path)) {
    cache_path <- paste0(tools::file_path_sans_ext(path), "_parquet")
  }

  # Normalized, like every other opener: a cache written from raw padded
  # header names would persist the dirty names forever, and the spec-keyed
  # typing would have silently matched nothing during the conversion.
  dataset <- dta_open_normalized_dataset(
    path,
    specs = specs,
    delim = delim,
    quote = quote,
    has_header = has_header
  )

  arrow::write_dataset(
    dataset,
    cache_path,
    format = "parquet",
    compression = compression
  )

  invisible(cache_path)
}

#' @title Validate a Delimited File Without Loading It
#' @description
#' Validates a delimited file against a set of column specs by scanning it in
#' batches, so peak memory is governed by the batch size rather than by the size
#' of the file. This is what makes a file larger than memory checkable at all:
#' the eager path has to hold the whole table as an R data frame before it can
#' validate a single row.
#'
#' The result is the same `details` structure the in-memory path returns, so
#' `results()`, `messages()` and `inspect()` accept it unchanged.
#'
#' Nothing here scales with the number of rows. Memory is bounded by the batch
#' size for the column-spec checks, by the number of distinct keys for
#' uniqueness rules, by the number of distinct groups for grouped rules, and by
#' `max_errors` for the retained error detail.
#'
#' @section Choosing between this and the in-memory path:
#' This buys feasibility, not speed. Measured over a 16-fold increase in input,
#' the working set this holds stayed flat at about 19 MB while the in-memory
#' path's grew from 51 MB to 272 MB -- but scanning ran roughly twice as slow,
#' because each batch pays its own dispatch and typing overhead.
#'
#' So: use this when the file is large enough that holding it is the problem.
#' For a file that fits in memory comfortably, `validate_table()` is the faster
#' choice and there is nothing to gain here.
#' @param specs A `DTAColumnSpecCollection`.
#' @param path Character. Path to the delimited file, or to a Parquet dataset
#'   written by [cache_as_parquet()]. The format is chosen from the path.
#' @param delim Character. The field separator. Defaults to a comma.
#' @param quote Character. The quoting character.
#' @param has_header Logical. Whether the first line names the columns.
#' @param batch_rows Integer. Rows per batch. Larger batches trade memory for
#'   fewer per-batch overheads.
#' @param max_errors Integer, or `NULL` to hold everything in memory. Cap on
#'   the per-cell error detail held in RAM, defaulting to 10000 and
#'   configurable via `options(DTAtools.max_errors = )`. The default is finite
#'   because the error sinks hold one row per bad cell: on a large dirty file,
#'   unbounded retention exhausts memory exactly as holding the data would.
#'   Nothing is lost, though: rows past the cap spill to a session-temporary
#'   directory and [collect_full_errors()] reassembles the complete detail;
#'   counts and the pass/fail verdict are exact either way, and a frame whose
#'   in-memory head is incomplete is flagged as such.
#' @param fail_fast Logical. Stop at the first batch that shows any problem,
#'   instead of scanning to the end. Answers "is this file valid?" without
#'   costing a full pass, which on a large file that fails early is the
#'   difference between seconds and hours.
#'
#'   The report is then explicitly incomplete: it carries a `partial_scan`
#'   attribute, only rules that actually failed are listed, and axes that could
#'   not be settled report `NA` rather than `TRUE`. A rule that has not failed
#'   yet has not passed -- a duplicate later in the file was never read.
#' @param on_missing_column One of `"scan"` or `"stop"`. A column the specs
#'   require but the file lacks is decidable from the header alone. `"scan"`,
#'   the default, preserves existing behaviour: the file is read and the absence
#'   is reported once per row. `"stop"` reports it structurally and reads
#'   nothing, which on a large file is the difference between an immediate
#'   answer and hours spent restating it per row.
#' @param use_threads Logical. Whether Arrow's Scanner should use multiple
#'   threads for I/O and decompression. Arrow buffers batches ahead of R in its
#'   own C++ pool, outside the R heap. Single-threaded scanning is the lever when
#'   RSS rather than speed is the constraint.
#' @param verbose Logical. Print progress.
#' @param benchmark Logical. If TRUE, measures runtime and memory for this call
#'   and attaches the result as the `"benchmark"` attribute on the returned
#'   `details`. Defaults to `getOption("DTAtools.benchmark", FALSE)`. Opt-in
#'   because measuring accurately resets R's `gc()` peak counters; see
#'   [validation_benchmark()] for the metrics shape and caveats.
#' @section Memory at scale:
#' Nothing in the scan aborts on a resource budget any more (the old
#' `DTAtools.max_unique_keys` / `DTAtools.max_groups` options are gone: they
#' discarded multi-hour scans at exactly the per-row-unique-key scale streaming
#' exists for). What bounds memory instead:
#'
#' * `check_unique` rules whose key columns are text are answered by Arrow's
#'   own grouped aggregation over the dataset, in one extra streaming pass over
#'   just the key columns -- the distinct keys live compactly in the C++
#'   engine, not as R strings. `options(DTAtools.stream_arrow_unique = FALSE)`
#'   restores the per-batch accumulator.
#' * A uniqueness rule that cannot take that path (non-text keys, or a
#'   consumable reader as the source) falls back to an R-side key set whose
#'   memory grows with the number of distinct keys (roughly 100 bytes per
#'   key). Grouped rules likewise hold state per distinct group.
#' * Columns nothing reads are projected out of the scan entirely, and every
#'   scanned column is read as text and typed in R, so a malformed value can
#'   never abort the scan mid-file.
#' @return A validation details list. It always carries an `n_rows_scanned`
#'   attribute (rows actually read; `0` for a structural early return). When
#'   `benchmark = TRUE` it additionally carries a `"benchmark"` attribute; see
#'   [validation_benchmark()].
#' @examples
#' specs <- DTAtools::DTAColumnSpecCollection(
#'   columns = list(
#'     ID = DTAtools::DTAColumnSpec(
#'       id = "ID", type = "SAS Char", length = 4, nullable = FALSE
#'     ),
#'     AGE = DTAtools::DTAColumnSpec(id = "AGE", type = "SAS Num", nullable = TRUE)
#'   )
#' )
#'
#' path <- file.path(tempdir(), "dta_stream_example.csv")
#' utils::write.csv(
#'   data.frame(ID = c("A001", "TOOLONG"), AGE = c(30, 40)),
#'   path,
#'   row.names = FALSE
#' )
#'
#' details <- validate_file_stream(specs, path, verbose = FALSE)
#' details$n_columnspec_errors
#'
#' unlink(path)
#' @export
validate_file_stream <- function(specs,
                                 path,
                                 delim = ",",
                                 quote = "\"",
                                 has_header = TRUE,
                                 batch_rows = 131072L,
                                 max_errors = getOption("DTAtools.max_errors", 10000L),
                                 fail_fast = FALSE,
                                 on_missing_column = c("scan", "stop"),
                                 use_threads = TRUE,
                                 verbose = TRUE,
                                 benchmark = getOption("DTAtools.benchmark", FALSE)) {
  if (!file.exists(path) && !dir.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }
  on_missing_column <- match.arg(on_missing_column)
  state <- dta_benchmark_begin(benchmark)
  # The reset cannot live only inside dta_benchmark_end(): opening the
  # dataset or streaming the table below can cli_abort() before end() is
  # ever reached, which would otherwise leave the nesting guard stuck TRUE
  # and silently kill benchmarking for the rest of the session. Registering
  # the reset here, in this call's own frame, fires on any exit -- normal or
  # error -- while the matching reset still inside dta_benchmark_end() keeps
  # that helper safe to call directly.
  if (!is.null(state)) {
    on.exit(dta_benchmark_env$active <- FALSE, add = TRUE)
  }

  dataset <- dta_open_validation_dataset(
    path,
    specs = specs,
    delim = delim,
    quote = quote,
    has_header = has_header
  )

  # Opening a dataset reads the header, not the data, so this costs nothing
  # even on a file of any size.
  findings <- dta_structure_findings(specs, names(dataset$schema))

  if (isTRUE(verbose) && length(findings$unexpected) > 0) {
    cli::cli_alert_warning(
      "{length(findings$unexpected)} column{?s} in the file {?is/are} not described by the specs: {.field {findings$unexpected}}"
    )
  }

  if (!findings$ok && identical(on_missing_column, "stop")) {
    if (isTRUE(verbose)) {
      cli::cli_alert_danger(
        "Missing required column{?s}: {.field {findings$missing}}. Stopping without reading the file."
      )
    }
    structural_details <- dta_structural_failure_details(
      findings,
      schemas = dta_compile_columnspec_schemas(specs)
    )
    if (isTRUE(verbose)) {
      dta_report_columnspec_checks(
        structural_details$columnspec_checks,
        unchecked_reason = "the file was not read"
      )
    }
    # No rows were read for a structural early return -- 0 is accurate here,
    # not a stand-in for "unknown".
    attr(structural_details, "n_rows_scanned") <- 0
    metrics <- dta_benchmark_end(state, rows = 0)
    attr(structural_details, "benchmark") <- metrics
    if (isTRUE(verbose)) {
      dta_benchmark_report(metrics)
    }
    return(structural_details)
  }

  rules_list <- tryCatch(specs@rules, error = function(e) NULL)
  if (is.null(rules_list)) {
    rules_list <- list()
  }

  # Eligible uniqueness rules are answered by Arrow's grouped aggregation
  # before the batch scan (see dta_stream_unique_precompute()); the batch loop
  # then skips them entirely.
  precomputed <- dta_stream_unique_precompute(specs, dataset, rules_list)

  # Projection pushes "which columns does validation read" into the scan:
  # unused columns are never parsed, converted, or materialised into R. The
  # structural findings above already used the full schema, so nothing about
  # missing/unexpected column reporting changes.
  projection <- dta_scan_projection(specs, rules_list, names(dataset$schema))
  reader <- if (is.null(projection)) {
    arrow::Scanner$create(
      dataset,
      batch_size = batch_rows, use_threads = use_threads
    )$ToRecordBatchReader()
  } else {
    arrow::Scanner$create(
      dataset,
      projection = projection,
      batch_size = batch_rows, use_threads = use_threads
    )$ToRecordBatchReader()
  }

  details <- dta_validate_table_stream(
    specs,
    reader,
    verbose = verbose,
    max_errors = max_errors,
    coerce = TRUE,
    fail_fast = fail_fast,
    precomputed = precomputed,
    known_columns = names(dataset$schema)
  )

  # Arrow buffers batches ahead of R in a C++ pool that gc() cannot see, so
  # every memory figure this package reported before this line understated the
  # true cost. The pool's high-water mark is per PROCESS, not per call, so it is
  # labelled as such rather than attributed to this scan alone.
  if (isTRUE(verbose)) {
    max_memory_mb <- tryCatch(
      ceiling(arrow::default_memory_pool()$max_memory / 1024^2),
      error = function(e) NULL
    )
    if (!is.null(max_memory_mb)) {
      cli::cli_alert_info(
        "Arrow's C++ pool has peaked at {max_memory_mb} MB this session (not counted by {.code gc()})."
      )
    }
  }

  metrics <- dta_benchmark_end(state, rows = attr(details, "n_rows_scanned"))
  attr(details, "benchmark") <- metrics
  if (isTRUE(verbose)) {
    dta_benchmark_report(metrics)
  }

  details
}

#' @title Details for a File That Failed Structurally
#' @description
#' The same `details` shape a scan produces, for a file rejected before any row
#' was read. The rule and import axes report as valid because they were never
#' evaluated -- there is no table to evaluate them against, and claiming a
#' failure would be as wrong as claiming a pass.
#' @param findings A list from `dta_structure_findings()`.
#' @param schemas Compiled schemas, from `dta_compile_columnspec_schemas()`, or
#'   `NULL`. Only the per-check summary reads them: presence was decided from
#'   the header, so it reports a verdict, while every other check reports
#'   `not_checked` rather than borrowing the presence failure or claiming a pass
#'   over rows that were never read.
#' @return A validation details list.
#' @keywords internal
dta_structural_failure_details <- function(findings, schemas = NULL) {
  full_error <- dta_structure_errors(findings)

  details <- list(
    ok = FALSE,
    columnspec_valid = FALSE,
    rules_valid = TRUE,
    import_valid = TRUE,
    n_columnspec_errors = nrow(full_error),
    n_rule_errors = 0L,
    n_import_errors = 0L,
    columnspec_errors = list(
      # The shared summariser, not an inline unique(): the inline copy had
      # already drifted (it kept rownames the helper resets), and a change to
      # how required-column errors summarise must reach structural early
      # returns too.
      summarised_error = dta_summarise_columnspec_errors(full_error),
      full_error = full_error
    ),
    columnspec_checks = dta_columnspec_check_summary(
      schemas,
      tally = dta_columnspec_error_tally(full_error),
      settled = "required"
    ),
    rule_results = list(),
    rule_errors = list(),
    import_errors = NULL,
    result_version = 2L
  )

  # Flags that this verdict rests on the header alone, so a reader is never
  # misled into thinking the rows were examined and found clean.
  attr(details, "structural_only") <- TRUE
  dta_as_validation_details(details)
}

#' @title Record Batch Reader for an In-Memory Table
#' @description
#' Wraps an Arrow table or an R data frame as a reader that yields fixed-size
#' record batches. Used to run the streaming path over data that is already in
#' memory -- chiefly to prove the streaming and non-streaming paths agree, and
#' to let a caller bound memory even when the input did not arrive as a stream.
#' @param x An Arrow table, record batch, or data frame.
#' @param batch_rows Integer. Rows per batch.
#' @return An `arrow::RecordBatchReader`.
#' @keywords internal
dta_as_batch_reader <- function(x, batch_rows = 65536L) {
  table_obj <- if (inherits(x, "Table")) x else arrow::as_arrow_table(x)

  # Scanner is the same machinery a file-backed dataset scan uses, so the
  # streaming path is exercised through its real interface rather than a
  # test-only substitute.
  scanner <- arrow::Scanner$create(table_obj, batch_size = batch_rows)
  scanner$ToRecordBatchReader()
}
