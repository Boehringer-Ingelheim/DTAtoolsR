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

# A key that reproduces `duplicated()`'s notion of an identical row.
#
# Repeated NAs are duplicates of each other, so missing values need a value of
# their own rather than being dropped. Each part is length-prefixed so that
# c("a", "b") and c("a\002b", "") cannot produce the same key -- without that,
# a separator appearing in the data would silently merge distinct keys.
dta_unique_key <- function(df, cols) {
  parts <- lapply(cols, function(column_name) {
    values <- df[[column_name]]
    text <- as.character(values)
    text[is.na(values)] <- "\001NA"
    paste0(nchar(text, type = "bytes"), ":", text)
  })
  do.call(paste, c(parts, sep = "\002"))
}

#' @title Start Accumulating a Rule Across Batches
#' @param rule A rule object.
#' @return A mutable accumulator.
#' @keywords internal
dta_rule_stream_init <- function(rule) {
  kind <- dta_rule_stream_kind(rule)
  state <- new.env(parent = emptyenv())
  state$kind <- kind
  state$count <- 0L
  state$applicable <- TRUE
  state$condition <- NULL

  if (kind == "keyed") {
    # Hashed environment: membership testing is what this is for, and it grows
    # with distinct keys rather than with rows.
    state$seen <- new.env(hash = TRUE, parent = emptyenv())
  }
  if (kind == "grouped") {
    state$grouped <- dta_group_stream_init(rule)
    state$row_offset <- 0L
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
  if (!state$applicable || state$kind == "unsupported") {
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
          for (key in keys) {
            if (is.null(state$seen[[key]])) {
              assign(key, TRUE, envir = state$seen)
            } else {
              state$count <- state$count + 1L
            }
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
  cols <- rule_get_slot(rule, "column")
  if (is.null(cols)) {
    cols <- rule_get_slot(rule, "columns")
  }
  cols
}

# ---- the streaming driver ----------------------------------------------------

# Bounded accumulation of a per-cell error frame.
#
# Both the schema and import axes can produce one error per bad cell, so on a
# dirty file the error frame is O(rows) and exhausts memory as surely as the
# data would. Retention is capped; counting is not, so the reported totals stay
# exact and the pass/fail verdict is never an artefact of truncation.
dta_error_sink <- function(max_errors) {
  sink <- new.env(parent = emptyenv())
  sink$parts <- list()
  sink$retained <- 0L
  sink$total <- 0L
  sink$truncated <- FALSE
  sink$max <- max_errors
  sink
}

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
    return(sink)
  }
  if (nrow(errs) > room) {
    errs <- errs[seq_len(room), , drop = FALSE]
    sink$truncated <- TRUE
  }
  sink$parts[[length(sink$parts) + 1]] <- errs
  sink$retained <- sink$retained + nrow(errs)
  sink
}

dta_error_sink_collect <- function(sink) {
  if (length(sink$parts) == 0) {
    return(NULL)
  }
  out <- do.call(rbind, sink$parts)
  rownames(out) <- NULL
  if (sink$truncated) {
    attr(out, "truncated") <- TRUE
  }
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
#' @param verbose Logical. Print progress.
#' @param max_errors Integer or `NULL`. Cap on retained per-cell errors. `NULL`
#'   retains everything, matching the materialising path.
#' @param coerce Logical. Type each batch against the specs as it arrives,
#'   recording values that cannot be represented. This is the streaming
#'   equivalent of typing the table once at import.
#' @return A `details` list of the same shape `validate_table_detailed()`
#'   returns.
#' @keywords internal
dta_validate_table_stream <- function(specs,
                                      reader,
                                      verbose = FALSE,
                                      max_errors = NULL,
                                      coerce = TRUE,
                                      fail_fast = FALSE) {
  rules_list <- tryCatch(specs@rules, error = function(e) NULL)
  if (is.null(rules_list)) {
    rules_list <- list()
  }

  states <- lapply(rules_list, dta_rule_stream_init)
  # Which columns each rule reads numerically, computed once for the whole
  # scan rather than re-derived (by re-parsing the rule's clause structure)
  # on every batch.
  rule_numeric_columns <- lapply(rules_list, function(r) {
    tryCatch(dta_rule_numeric_columns(r), error = function(e) character(0))
  })

  columnspec_sink <- dta_error_sink(max_errors)
  carried_sink <- dta_error_sink(max_errors)
  rule_import_sink <- dta_error_sink(max_errors)
  row_offset <- 0L
  partial_scan <- FALSE

  if (isTRUE(verbose)) {
    cli::cli_h3("validating with column specs")
  }

  repeat {
    batch <- reader$read_next_batch()
    if (is.null(batch)) {
      break
    }

    df <- as.data.frame(batch)
    n_batch_rows <- nrow(df)
    if (n_batch_rows == 0) {
      next
    }

    # Import typing, per batch. The materialising path types the whole table
    # once and hangs the issues on it as an attribute; with no single table to
    # hang anything on, the issues accumulate here instead.
    if (isTRUE(coerce)) {
      coerced <- dta_coerce_table_to_specs(df, specs)
      df <- coerced$table
      issues <- coerced$issues
      if (is.data.frame(issues)) {
        # Read the true count BEFORE touching the frame: import typing caps the
        # rows it retains per column but records how many there really were on
        # the frame itself, and modifying a column can drop that attribute.
        n_issues <- dta_import_error_count(issues)
        if (nrow(issues) > 0) {
          issues$row <- issues$row + row_offset
        }
        if (n_issues > 0) {
          dta_error_sink_add(carried_sink, issues, n_total = n_issues)
        }
      }
    }

    schema_result <- dta_columnspec_errors(specs, df)
    columnspec_errs <- schema_result$full_error
    if (!is.null(columnspec_errs) && nrow(columnspec_errs) > 0) {
      columnspec_errs$row <- columnspec_errs$row + row_offset
      dta_error_sink_add(columnspec_sink, columnspec_errs)
    }

    # Built once per batch rather than once per rule per batch: a column read
    # numerically by several rules would otherwise be strict-converted once
    # per rule that reads it, per batch.
    numeric_cache <- dta_build_numeric_cache(df, rules_list)

    for (i in seq_along(rules_list)) {
      dta_rule_stream_update(states[[i]], rules_list[[i]], df, numeric_cache)

      # Sourced from the same columns the rule just read as numbers, so an
      # unrepresentable value is reported on both axes rather than moved
      # from one to the other.
      rule_errs <- tryCatch(
        dta_rule_import_errors(
          rules_list[[i]], df,
          numeric_cache = numeric_cache,
          columns = rule_numeric_columns[[i]]
        ),
        error = function(e) NULL
      )
      if (is.data.frame(rule_errs) && nrow(rule_errs) > 0) {
        rule_errs$row <- rule_errs$row + row_offset
        dta_error_sink_add(rule_import_sink, rule_errs)
      }
    }

    row_offset <- row_offset + n_batch_rows

    if (isTRUE(fail_fast) &&
      (columnspec_sink$total > 0 ||
        carried_sink$total > 0 ||
        rule_import_sink$total > 0 ||
        any(vapply(states, dta_rule_stream_failing, logical(1))))) {
      partial_scan <- TRUE
      break
    }
  }

  full_error <- dta_error_sink_collect(columnspec_sink)
  summarised_error <- dta_summarise_columnspec_errors(full_error)
  has_columnspec_errors <- columnspec_sink$total > 0

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

  # Rows the retained-error cap threw away, counted before any deduplication so
  # the two are not confused: a duplicate is one error counted once, a capped
  # row is one error whose identity is gone. The carried axis needs this only
  # when the cap left nothing at all for the merge to count.
  carried_capped <- if (is.null(carried)) carried_sink$total else 0L
  rule_capped <- rule_import_sink$total - NROW(rule_import)

  if (!is.null(rule_import)) {
    rule_import <- rule_import[
      !duplicated(rule_import[, c("row", "column"), drop = FALSE]), ,
      drop = FALSE
    ]
    rule_import <- dta_apply_spec_declared_types(rule_import, specs)
  }

  import_errors <- dta_merge_import_errors(carried, rule_import)
  # dta_merge_import_errors() is the only place that knows a cell flagged on
  # both the import-typing axis and the rule-reading axis is one error, not
  # two. Summing the raw sink totals bypassed it, and could report more import
  # errors than `import_errors` had rows -- while the materialising path, which
  # counts the merged frame, reported the right number for the same input.
  # Capped rows were never available to deduplicate, so they are added back
  # unreduced: a truncated scan over-reports rather than under-reports.
  n_import_errors <- dta_import_error_count(import_errors) +
    carried_capped + rule_capped
  import_valid <- n_import_errors == 0L
  if (n_import_errors == 0L) {
    import_errors <- NULL
  }

  details <- list(
    ok = NA,
    columnspec_valid = !has_columnspec_errors,
    rules_valid = isTRUE(rules_valid),
    import_valid = isTRUE(import_valid),
    n_columnspec_errors = columnspec_sink$total,
    n_rule_errors = length(rule_errors),
    n_import_errors = as.integer(n_import_errors),
    columnspec_errors = list(
      summarised_error = summarised_error,
      full_error = full_error
    ),
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
    details$rules_valid <- if (length(details$rule_errors) > 0) FALSE else NA
    details$import_valid <- if (n_import_errors > 0L) FALSE else NA
    # Only rules that actually failed are reported. Their failures are real;
    # the silence of the others is not evidence.
    details$rule_results <- details$rule_errors
    attr(details, "partial_scan") <- TRUE
  }

  details$ok <- dta_details_ok(details)

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
# Memory is therefore proportional to the number of distinct GROUPS times the
# number of conditions, not to the number of rows. That is the same class as
# uniqueness, and unbounded in group cardinality rather than in file size -- an
# improvement over retaining every row of every column the rule reads, but not
# a constant.

DTA_GROUP_ROW_HEAD <- 10L

dta_group_cond_state <- function() {
  list(
    any_true = FALSE,
    all_true = TRUE,
    n_seen = 0L,
    true_head = integer(0),
    true_n = 0L,
    false_head = integer(0),
    false_n = 0L
  )
}

dta_group_fold_rows <- function(head, count, new_rows) {
  count <- count + length(new_rows)
  room <- DTA_GROUP_ROW_HEAD - length(head)
  if (room > 0 && length(new_rows) > 0) {
    head <- c(head, new_rows[seq_len(min(room, length(new_rows)))])
  }
  list(head = head, count = count)
}

#' @title Start Accumulating a Grouped Rule
#' @param rule A grouped rule.
#' @return A mutable accumulator.
#' @keywords internal
dta_group_stream_init <- function(rule) {
  state <- new.env(parent = emptyenv())
  state$groups <- new.env(hash = TRUE, parent = emptyenv())
  state$keys <- character(0)
  state$condition_names <- names(rule_get_slot(rule, "conditions"))

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
  state$certain <- 0L
  state
}

#' @title Fold One Batch into a Grouped Rule's Accumulator
#' @param state An accumulator from `dta_group_stream_init()`.
#' @param rule The grouped rule.
#' @param df A data frame holding one batch.
#' @param row_offset Integer. Rows already consumed, so row numbers are global.
#' @param numeric_cache A named list from [dta_build_numeric_cache()] built for
#'   this batch, or `NULL` to convert each column on demand.
#' @return The accumulator, updated in place.
#' @keywords internal
dta_group_stream_update <- function(state, rule, df, row_offset = 0L, numeric_cache = NULL) {
  group_by <- rule_get_slot(rule, "group_by")
  conditions <- rule_get_slot(rule, "conditions")

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
  # `factor(split_key)` sorts its levels the same way `split()` orders its
  # groups, so `local_groups <- split(seq_len(nrow(df)), split_key)` and
  # iterating group ids in level order visit groups identically.
  kf <- factor(split_key)
  gid <- as.integer(kf)
  n_groups <- nlevels(kf)
  first_row <- match(levels(kf), split_key)
  n_seen_batch <- tabulate(gid, nbins = n_groups)

  cond_hit <- lapply(state$condition_names, function(cond_name) {
    spec <- conditions[[cond_name]]
    mask <- tryCatch(
      evaluate_conditions(spec, df, numeric_cache),
      dta_rule_not_applicable = function(cnd) {
        cli::cli_abort(
          c(
            "Rule {.val {rule@id}} cannot evaluate condition {.field {cond_name}}.",
            x = "{conditionMessage(cnd)}"
          ),
          class = "dta_rule_not_applicable"
        )
      }
    )
    mask %in% TRUE
  })
  names(cond_hit) <- state$condition_names

  cond_n_true <- lapply(cond_hit, function(hit) tabulate(gid[hit], nbins = n_groups))
  # Rows matching / not matching, split by group -- with all `n_groups` levels
  # represented (even when empty), so each is addressable by group position.
  cond_true_rows <- lapply(cond_hit, function(hit) {
    split(which(hit), factor(gid[hit], levels = seq_len(n_groups)))
  })
  cond_false_rows <- lapply(cond_hit, function(hit) {
    split(which(!hit), factor(gid[!hit], levels = seq_len(n_groups)))
  })

  for (g in seq_len(n_groups)) {
    key <- levels(kf)[g]
    local_first <- first_row[g]

    entry <- state$groups[[key]]
    if (is.null(entry)) {
      state$keys <- c(state$keys, key)
      entry <- list(
        label = paste(
          vapply(group_by, function(col) {
            paste0(col, "=", as.character(grouped[[col]][local_first]))
          }, character(1)),
          collapse = ", "
        ),
        conds = stats::setNames(
          lapply(state$condition_names, function(...) dta_group_cond_state()),
          state$condition_names
        ),
        certain = FALSE
      )
    }

    n_seen_g <- n_seen_batch[g]

    for (cond_name in state$condition_names) {
      cond <- entry$conds[[cond_name]]
      n_true_g <- cond_n_true[[cond_name]][g]

      cond$any_true <- cond$any_true || (n_true_g > 0)
      cond$all_true <- cond$all_true && (n_seen_g > 0 && n_true_g == n_seen_g)
      cond$n_seen <- cond$n_seen + n_seen_g

      true_local <- cond_true_rows[[cond_name]][[g]]
      false_local <- cond_false_rows[[cond_name]][[g]]

      folded_true <- dta_group_fold_rows(cond$true_head, cond$true_n, true_local + row_offset)
      cond$true_head <- folded_true$head
      cond$true_n <- folded_true$count

      folded_false <- dta_group_fold_rows(cond$false_head, cond$false_n, false_local + row_offset)
      cond$false_head <- folded_false$head
      cond$false_n <- folded_false$count

      entry$conds[[cond_name]] <- cond
    }

    if (!isTRUE(entry$certain) && length(state$monotone) > 0) {
      lost <- vapply(state$monotone, function(constraint) {
        isTRUE(entry$conds[[constraint$left]]$any_true) &&
          isTRUE(entry$conds[[constraint$right]]$any_true)
      }, logical(1))
      if (any(lost)) {
        entry$certain <- TRUE
        state$certain <- state$certain + 1L
      }
    }

    assign(key, entry, envir = state$groups)
  }

  state
}

# Whether a violation's `rows` are only the head of a longer list. The capped
# head is what keeps memory proportional to groups rather than to rows, so the
# streamed `rows` can be shorter than the materialising path's -- but silently
# shorter is a trap, and both paths therefore carry this flag.
dta_group_head_truncated <- function(cond, which = "true") {
  if (identical(which, "true")) {
    length(cond$true_head) < cond$true_n
  } else {
    length(cond$false_head) < cond$false_n
  }
}

# Whether a condition holds for a group under the given scope. "all" requires
# the group to have had at least one row, matching the materialising path where
# all(logical(0)) would otherwise be vacuously TRUE.
dta_group_stream_truth <- function(cond, scope) {
  if (identical(scope, "all")) {
    cond$n_seen > 0 && cond$all_true
  } else {
    cond$any_true
  }
}

#' @title Turn a Grouped Rule's Accumulator into a Result
#' @param state An accumulator that has seen every batch.
#' @param rule The grouped rule.
#' @return A list with `id`, `valid` and `message`, matching what
#'   `rule_check_group_condition()` returns for the same data.
#' @keywords internal
dta_group_stream_finalise <- function(state, rule) {
  constraints <- rule_get_slot(rule, "constraints")
  violations <- list()

  fmt <- function(cond, which = "true") {
    if (identical(which, "true")) {
      dta_format_group_rows(cond$true_head, cond$true_n, DTA_GROUP_ROW_HEAD)
    } else {
      dta_format_group_rows(cond$false_head, cond$false_n, DTA_GROUP_ROW_HEAD)
    }
  }

  # split() orders groups by sorted key, so the materialising path reports
  # violations in that order. Sorting here keeps the assembled message
  # identical.
  sorted_keys <- sort(state$keys)
  n_groups <- length(sorted_keys)
  entries <- mget(sorted_keys, envir = state$groups)

  # Vectorised per-(condition, scope) truth across ALL groups at once -- one
  # lookup per group, not the label/row-evidence assembly -- mirroring the
  # materialising path's `cond_any_true`/`cond_all_true`. This is what lets
  # the loop below only ever do per-group work (building labels, formatting
  # row evidence, composing messages) for groups that actually violate
  # something, instead of for every group seen in the stream.
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
    vapply(entries, function(entry) {
      dta_group_stream_truth(entry$conds[[cond_name]], scope)
    }, logical(1))
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
    entry <- entries[[g]]
    conds <- entry$conds

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
            entry$label,
            left,
            right,
            left, fmt(conds[[left]]),
            right, fmt(conds[[right]])
          )
        violations[[length(violations) + 1L]] <- list(
          constraint_id = constraint$id,
          group = entry$label,
          message = message,
          rows = sort(unique(c(conds[[left]]$true_head, conds[[right]]$true_head))),
          rows_truncated = dta_group_head_truncated(conds[[left]], "true") ||
            dta_group_head_truncated(conds[[right]], "true")
        )
      } else if (identical(ctype, "requires")) {
        if_name <- constraint[["if"]]
        then_name <- constraint[["then"]]
        then_scope <- constraint$then_scope %||% "any"

        then_scope_reason <- if (identical(then_scope, "all")) {
          sprintf("rows %s do not satisfy \"%s\"", fmt(conds[[then_name]], "false"), then_name)
        } else {
          sprintf("no row in the group satisfies \"%s\"", then_name)
        }
        message <- constraint$message %||%
          sprintf(
            "In group [%s]: when \"%s\" occurs (rows: %s), \"%s\" must also hold, but it does not (%s).",
            entry$label,
            if_name,
            fmt(conds[[if_name]]),
            then_name,
            then_scope_reason
          )
        then_failed <- if (identical(then_scope, "all")) {
          conds[[then_name]]$false_head
        } else {
          integer(0)
        }
        violations[[length(violations) + 1L]] <- list(
          constraint_id = constraint$id,
          group = entry$label,
          message = message,
          rows = sort(unique(c(conds[[if_name]]$true_head, then_failed))),
          rows_truncated = dta_group_head_truncated(conds[[if_name]], "true") ||
            (identical(then_scope, "all") &&
              dta_group_head_truncated(conds[[then_name]], "false"))
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

#' @title Validate a Table However It Is Held
#' @description
#' Dispatches to the streaming path for a lazy table and the materialising path
#' for one already in memory, returning the same details either way. This is
#' what lets `check()` accept both without its callers knowing which they have.
#' @param specs A `DTAColumnSpecCollection`.
#' @param table An Arrow `Table`, `Dataset`, or reader.
#' @param verbose Logical. Print progress.
#' @param batch_rows Integer. Rows per batch when scanning.
#' @param max_errors Integer or `NULL`. Cap on retained per-cell error detail.
#' @return A validation details list.
#' @keywords internal
dta_validate_any_table <- function(specs,
                                   table,
                                   verbose = FALSE,
                                   batch_rows = 131072L,
                                   max_errors = NULL) {
  if (!dta_table_is_lazy(table)) {
    return(validate_table_detailed(specs, as.data.frame(table), verbose = verbose))
  }

  reader <- if (inherits(table, "RecordBatchReader")) {
    table
  } else {
    arrow::Scanner$create(table, batch_size = batch_rows)$ToRecordBatchReader()
  }

  dta_validate_table_stream(
    specs,
    reader,
    verbose = verbose,
    max_errors = max_errors,
    coerce = TRUE
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
  declared <- if (is.null(columns)) {
    character(0)
  } else {
    nm <- names(columns)
    if (is.null(nm)) vapply(columns, function(s) s@id, character(1)) else nm
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

  dta_open_delimited_dataset(
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

  dataset <- dta_open_delimited_dataset(
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
#' @param max_errors Integer or `NULL`. Cap on retained per-cell error detail.
#'   Counting is unaffected, so totals and the pass/fail verdict stay exact even
#'   when the retained detail is truncated.
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
#' @param verbose Logical. Print progress.
#' @return A validation details list.
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
                                 max_errors = NULL,
                                 fail_fast = FALSE,
                                 on_missing_column = c("scan", "stop"),
                                 verbose = TRUE) {
  if (!file.exists(path) && !dir.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }
  on_missing_column <- match.arg(on_missing_column)

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
    return(dta_structural_failure_details(findings))
  }

  reader <- arrow::Scanner$create(dataset, batch_size = batch_rows)$ToRecordBatchReader()

  dta_validate_table_stream(
    specs,
    reader,
    verbose = verbose,
    max_errors = max_errors,
    coerce = TRUE,
    fail_fast = fail_fast
  )
}

#' @title Details for a File That Failed Structurally
#' @description
#' The same `details` shape a scan produces, for a file rejected before any row
#' was read. The rule and import axes report as valid because they were never
#' evaluated -- there is no table to evaluate them against, and claiming a
#' failure would be as wrong as claiming a pass.
#' @param findings A list from `dta_structure_findings()`.
#' @return A validation details list.
#' @keywords internal
dta_structural_failure_details <- function(findings) {
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
      summarised_error = unique(full_error[, c("keyword", "message"), drop = FALSE]),
      full_error = full_error
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
