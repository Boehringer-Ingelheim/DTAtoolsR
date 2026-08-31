#' @keywords internal
#' @title Escape Text for HTML Embedding
#' @description
#' Vectorized function that escapes special HTML characters in character vectors
#' for safe embedding in HTML text and attribute contexts.
#' @param x A character vector (or coercible to character). Non-character values
#'   are coerced with `as.character()`. `NA` values are treated as empty strings.
#' @return A character vector of the same length as `x` with HTML entities escaped.
#'   Ampersands are always escaped first to avoid double-escaping.
.report_html_escape <- function(x) {
  x <- as.character(x)
  x <- ifelse(is.na(x), "", x)
  # Escape & first, then <, >, ", ' to avoid double-escaping
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

#' @keywords internal
#' @title Placeholder for Missing Values in the Report
#' @description
#' The em dash shown for missing/unknown values, built via `intToUtf8()`
#' rather than a literal character so this file's source stays pure ASCII
#' (`R CMD check` flags non-ASCII bytes in code, comments excepted).
#' @return A length-1 character string holding a single em dash.
.report_em_dash <- function() {
  intToUtf8(8212L)
}


#' @keywords internal
#' @title Map Validation Status to CSS-Safe Token
#' @description
#' Maps a status string from validation results (or a simplified three-way status)
#' to one of three CSS-class-safe tokens: "pass", "fail", or "pending".
#' @param status A character string (possibly `NA`). Expected values:
#'   "validated", "failed", "skipped", "not_validated", or the simplified
#'   "pass", "fail", "pending".
#' @return One of "pass", "fail", or "pending".
.report_status_label <- function(status) {
  if (length(status) != 1 || is.na(status)) {
    return("pending")
  }

  status <- as.character(status)

  if (status %in% c("failed", "fail")) {
    return("fail")
  }

  if (status %in% c("validated", "pass")) {
    return("pass")
  }

  # Anything else: "skipped", "not_validated", unrecognized -> "pending"
  "pending"
}


#' @keywords internal
#' @title Display Label for Status Class
#' @description
#' Maps a status CSS class ("pass", "fail", "pending") to a human-readable display label.
#' @param status_class One of "pass", "fail", or "pending".
#' @return A capitalized display label: "Pass", "Fail", or "Pending".
.report_status_display <- function(status_class) {
  switch(status_class,
    pass = "Pass",
    fail = "Fail",
    pending = "Pending",
    "Unknown"
  )
}


#' @keywords internal
#' @title Human-Readable Label for Rule Type
#' @description
#' Dispatches on an S7 rule object and returns a human-readable type description.
#' @param rule_def An S7 rule object (inherits "DTAtools::DTARule*"), or `NULL`.
#' @return A character string describing the rule type, or an em dash for `NULL`/unrecognized.
.report_rule_type_label <- function(rule_def) {
  if (is.null(rule_def)) {
    return(.report_em_dash()) # ASCII-only source; see helper below
  }

  if (inherits(rule_def, "DTAtools::DTARuleColCondition")) {
    return("Conditional (IF/THEN)")
  }

  if (inherits(rule_def, "DTAtools::DTARuleColRange")) {
    return("Range")
  }

  if (inherits(rule_def, "DTAtools::DTARuleColUnique")) {
    return("Unique")
  }

  if (inherits(rule_def, "DTAtools::DTARuleGroupCondition")) {
    return("Grouped condition")
  }

  # Unrecognized
  .report_em_dash()
}


#' @keywords internal
#' @title Build Validation Summary HTML Section
#' @description
#' Renders the top-level summary section with pass/fail/pending counts and
#' a table of per-dataset/target results.
#' @param results_df A data.frame from `DTAtools::results(dta)` with columns
#'   `dataset`, `target`, `target_type`, `status`, `n_columnspec_errors`,
#'   `n_rule_errors`, `n_import_errors`.
#' @return A length-1 character string containing the HTML section.
.report_summary_html <- function(results_df) {
  if (is.null(results_df) || nrow(results_df) == 0) {
    results_df <- data.frame(
      dataset = character(0),
      target = character(0),
      target_type = character(0),
      status = character(0),
      n_columnspec_errors = integer(0),
      n_rule_errors = integer(0),
      n_import_errors = integer(0),
      stringsAsFactors = FALSE
    )
  }

  # Classify each row by status. `vapply()`, not `sapply()`: `sapply()` over
  # a zero-length input (the empty-DTA case just above) returns `list()`
  # instead of `character(0)`, which breaks the `== "pass"` comparisons below.
  status_classes <- vapply(results_df$status, .report_status_label, character(1))
  n_pass <- sum(status_classes == "pass")
  n_fail <- sum(status_classes == "fail")
  n_pending <- sum(status_classes == "pending")

  # Build table rows
  tbody_rows <- if (nrow(results_df) == 0) {
    ""
  } else {
    paste0(
      "<tr data-dataset=\"",
      .report_html_escape(results_df$dataset),
      "\" data-target=\"",
      .report_html_escape(results_df$target),
      "\" data-status=\"",
      status_classes,
      "\">",
      "<td>", .report_html_escape(results_df$dataset), "</td>",
      "<td>", .report_html_escape(results_df$target), "</td>",
      "<td><span class=\"status-badge status-",
      status_classes,
      "\">",
      # vapply, not a direct vector call: .report_status_display() uses
      # switch(), which requires a length-1 EXPR per call (an R CMD check
      # ERROR under R >= 4, not just a warning, once results_df has more
      # than one row).
      vapply(status_classes, .report_status_display, character(1)),
      "</span></td>",
      "<td>",
      ifelse(is.na(results_df$n_columnspec_errors), .report_em_dash(), results_df$n_columnspec_errors),
      "</td>",
      "<td>",
      ifelse(is.na(results_df$n_rule_errors), .report_em_dash(), results_df$n_rule_errors),
      "</td>",
      "<td>",
      ifelse(is.na(results_df$n_import_errors), .report_em_dash(), results_df$n_import_errors),
      "</td>",
      "</tr>",
      collapse = "\n"
    )
  }

  glue::glue(
    '<section class="report-summary">
  <h2>Overview</h2>
  <div class="report-summary-cards">
    <div class="summary-card pass"><span class="summary-count">{n_pass}</span><span class="summary-label">Passed</span></div>
    <div class="summary-card fail"><span class="summary-count">{n_fail}</span><span class="summary-label">Failed</span></div>
    <div class="summary-card pending"><span class="summary-count">{n_pending}</span><span class="summary-label">Pending</span></div>
  </div>
  <table class="report-table report-summary-table">
    <thead><tr><th>Dataset</th><th>Target</th><th>Status</th><th>Columnspec errors</th><th>Rule errors</th><th>Import errors</th></tr></thead>
    <tbody>
{if (nrow(results_df) > 0) tbody_rows else ""}
    </tbody>
  </table>
</section>'
  )
}


#' @keywords internal
#' @title Build Messages Table HTML Section
#' @description
#' Renders the messages table with filter controls and support for "show more" toggles
#' on repeated identical messages.
#' @param messages_df A data.frame from `DTAtools::messages(dta, as_tibble = FALSE)`
#'   with columns `id`, `dataset`, `target`, `severity`, `source`, `rule_id`,
#'   `row`, `column`, `keyword`, `message`.
#' @param max_repeats Positive integer or `NULL`/`Inf`. Rows beyond the first
#'   `max_repeats` of an identical message group are hidden by default.
#' @return A length-1 character string containing the HTML section.
.report_messages_table_html <- function(messages_df, max_repeats = 5) {
  # Validate before the empty-data early return below, so an invalid
  # `max_repeats` is rejected consistently regardless of how much data
  # happens to be present -- a DTA with zero messages must not silently let
  # a bad `max_repeats` through. Capping compares `rank_within_group ==
  # max_repeats` with exact equality (see below), which only ever matches
  # for a whole number; a non-integer such as `2.5` would cap rows (`>`
  # matches ranks 3+) without ever emitting the "show more" toggle to reveal
  # them again, so whole-number-ness is enforced here, not just positivity.
  capping_enabled <- !is.null(max_repeats) && is.finite(max_repeats)
  if (capping_enabled && (max_repeats < 1 || max_repeats != round(max_repeats))) {
    cli::cli_abort(
      "'max_repeats' must be a positive whole number, or NULL/Inf to disable capping."
    )
  }

  if (is.null(messages_df) || nrow(messages_df) == 0) {
    # Empty table with filter controls
    return(
      '<section class="report-messages">
  <h2>Validation messages</h2>
  <div class="report-controls">
    <input type="text" class="report-filter-text" data-filter-target="messages-table" placeholder="Filter messages...">
    <select class="report-filter-select" data-filter-column="dataset" data-filter-target="messages-table">
      <option value="">All datasets</option>
    </select>
    <select class="report-filter-select" data-filter-column="target" data-filter-target="messages-table">
      <option value="">All targets</option>
    </select>
    <select class="report-filter-select" data-filter-column="source" data-filter-target="messages-table">
      <option value="">All sources</option>
    </select>
  </div>
  <table class="report-table report-messages-table" id="messages-table">
    <thead>
      <tr>
        <th class="sortable" data-sort-key="id">ID</th>
        <th class="sortable" data-sort-key="dataset">Dataset</th>
        <th class="sortable" data-sort-key="target">Target</th>
        <th class="sortable" data-sort-key="source">Source</th>
        <th class="sortable" data-sort-key="row">Row</th>
        <th class="sortable" data-sort-key="column">Column</th>
        <th class="sortable" data-sort-key="rule_id">Rule</th>
        <th class="sortable" data-sort-key="message">Message</th>
      </tr>
    </thead>
    <tbody>
    </tbody>
  </table>
</section>'
    )
  }

  # Build composite key for grouping: dataset|target|source|rule_id|column|keyword|message.
  # NA is mapped to a placeholder token so a real value can never collide with
  # a missing one, and the join separator is a control character unlikely to
  # appear in real data -- this key is never shown to the user, just used to
  # tell repeated messages apart. Both come from intToUtf8() rather than
  # literal characters so this file's source stays pure ASCII (R CMD check
  # flags non-ASCII bytes in code, comments excepted).
  na_marker <- intToUtf8(8709L) # U+2205 EMPTY SET
  key_sep <- intToUtf8(9247L) # U+241F SYMBOL FOR UNIT SEPARATOR
  make_key <- function(dataset, target, source, rule_id, column, keyword, message) {
    dataset <- if (is.na(dataset)) na_marker else as.character(dataset)
    target <- if (is.na(target)) na_marker else as.character(target)
    source <- if (is.na(source)) na_marker else as.character(source)
    rule_id <- if (is.na(rule_id)) na_marker else as.character(rule_id)
    column <- if (is.na(column)) na_marker else as.character(column)
    keyword <- if (is.na(keyword)) na_marker else as.character(keyword)
    message <- if (is.na(message)) na_marker else as.character(message)
    paste(dataset, target, source, rule_id, column, keyword, message, sep = key_sep)
  }

  keys <- mapply(
    make_key,
    messages_df$dataset, messages_df$target, messages_df$source,
    messages_df$rule_id, messages_df$column, messages_df$keyword,
    messages_df$message,
    SIMPLIFY = TRUE
  )

  # Assign group number based on first appearance
  unique_keys <- unique(keys)
  group_num <- match(keys, unique_keys)

  # Rank within each group (for capping logic)
  rank_within_group <- stats::ave(seq_along(keys), keys, FUN = seq_along)

  # Count total rows per group
  group_total <- stats::ave(seq_along(keys), keys, FUN = length)

  # `capping_enabled` and validation already resolved above, before the
  # empty-data early return. `&&`/`||` require length-1 operands (an error,
  # not just a warning, since R 4.3), so re-using that scalar here keeps the
  # length-N comparison below a plain vectorized `>`.
  is_capped <- if (capping_enabled) {
    rank_within_group > max_repeats
  } else {
    rep(FALSE, length(rank_within_group))
  }

  # `messages_df` is ordered by (dataset, target, source, row) -- rows
  # belonging to the same repeated-message group are NOT generally
  # contiguous (e.g. row 5 and row 6 can each contribute one row to the same
  # "HEIGHT required" group, interleaved with other columns' messages on
  # those same rows). Cluster identical-message groups together, preserving
  # first-appearance order of groups and original row order within each
  # group, so a group's "show more" toggle can be placed directly after its
  # own visible rows.
  ord <- order(group_num, rank_within_group)
  messages_df <- messages_df[ord, , drop = FALSE]
  group_num <- group_num[ord]
  rank_within_group <- rank_within_group[ord]
  group_total <- group_total[ord]
  is_capped <- is_capped[ord]

  # Build table rows
  n <- nrow(messages_df)
  table_rows <- character(n)
  for (i in seq_len(n)) {
    row_style <- if (is_capped[i]) ' style="display:none;"' else ""
    row_class <- if (is_capped[i]) ' class="msg-row msg-row-extra"' else ' class="msg-row"'

    row <- sprintf(
      '<tr%s data-id="%s" data-dataset="%s" data-target="%s" data-source="%s" data-group="g%d" data-capped="%s"%s>
  <td>%d</td>
  <td>%s</td>
  <td>%s</td>
  <td>%s</td>
  <td>%s</td>
  <td>%s</td>
  <td>%s</td>
  <td>%s</td>
</tr>',
      row_class,
      messages_df$id[i],
      .report_html_escape(messages_df$dataset[i]),
      .report_html_escape(messages_df$target[i]),
      .report_html_escape(messages_df$source[i]),
      group_num[i],
      if (is_capped[i]) "true" else "false",
      row_style,
      messages_df$id[i],
      .report_html_escape(messages_df$dataset[i]),
      .report_html_escape(messages_df$target[i]),
      .report_html_escape(messages_df$source[i]),
      if (is.na(messages_df$row[i])) "" else messages_df$row[i],
      .report_html_escape(if (is.na(messages_df$column[i])) "" else messages_df$column[i]),
      .report_html_escape(if (is.na(messages_df$rule_id[i])) "" else messages_df$rule_id[i]),
      .report_html_escape(messages_df$message[i])
    )
    table_rows[i] <- row
  }

  # Insert a "show more" row directly after the last VISIBLE row of each
  # capped group. Rows are now clustered by group (see `ord` above), so the
  # row at rank == max_repeats is immediately followed by its group's capped
  # rows (rank max_repeats+1 .. group_total) -- no need to also check "is
  # this the group's last row overall", because for a capped group it never
  # is: the capped rows are exactly what comes right after. (An earlier
  # version required both conditions, which meant the group boundary check
  # was never satisfied at rank == max_repeats for any group that actually
  # had rows left to cap, so no "more" row was ever emitted -- caught by
  # tests/testthat/test-exportValidationReport.R's exact-count assertions.)
  if (capping_enabled) {
    for (i in seq_len(n)) {
      if (rank_within_group[i] == max_repeats && group_total[i] > max_repeats) {
        more_row <- sprintf(
          '<tr class="msg-more-row" data-group="g%d">
  <td colspan="8"><button type="button" class="show-more-btn" data-group="g%d">Show %d more like this</button></td>
</tr>',
          group_num[i],
          group_num[i],
          group_total[i] - max_repeats
        )
        table_rows[i] <- paste0(table_rows[i], "\n", more_row)
      }
    }
  }

  tbody_html <- paste0(table_rows, collapse = "\n")

  # Build filter select options for unique non-NA values
  unique_datasets <- sort(unique(messages_df$dataset[!is.na(messages_df$dataset)]))
  dataset_options <- paste0(
    '<option value="', .report_html_escape(unique_datasets), '">',
    .report_html_escape(unique_datasets), "</option>",
    collapse = "\n    "
  )

  unique_targets <- sort(unique(messages_df$target[!is.na(messages_df$target)]))
  target_options <- paste0(
    '<option value="', .report_html_escape(unique_targets), '">',
    .report_html_escape(unique_targets), "</option>",
    collapse = "\n    "
  )

  unique_sources <- sort(unique(messages_df$source[!is.na(messages_df$source)]))
  source_options <- paste0(
    '<option value="', .report_html_escape(unique_sources), '">',
    .report_html_escape(unique_sources), "</option>",
    collapse = "\n    "
  )

  glue::glue(
    '<section class="report-messages">
  <h2>Validation messages</h2>
  <div class="report-controls">
    <input type="text" class="report-filter-text" data-filter-target="messages-table" placeholder="Filter messages...">
    <select class="report-filter-select" data-filter-column="dataset" data-filter-target="messages-table">
      <option value="">All datasets</option>
{dataset_options}
    </select>
    <select class="report-filter-select" data-filter-column="target" data-filter-target="messages-table">
      <option value="">All targets</option>
{target_options}
    </select>
    <select class="report-filter-select" data-filter-column="source" data-filter-target="messages-table">
      <option value="">All sources</option>
{source_options}
    </select>
  </div>
  <table class="report-table report-messages-table" id="messages-table">
    <thead>
      <tr>
        <th class="sortable" data-sort-key="id">ID</th>
        <th class="sortable" data-sort-key="dataset">Dataset</th>
        <th class="sortable" data-sort-key="target">Target</th>
        <th class="sortable" data-sort-key="source">Source</th>
        <th class="sortable" data-sort-key="row">Row</th>
        <th class="sortable" data-sort-key="column">Column</th>
        <th class="sortable" data-sort-key="rule_id">Rule</th>
        <th class="sortable" data-sort-key="message">Message</th>
      </tr>
    </thead>
    <tbody>
{tbody_html}
    </tbody>
  </table>
</section>'
  )
}


#' @keywords internal
#' @title Fallback for a Missing/Blank/NA Scalar
#' @description
#' Returns `default` when `x` is `NULL`, not length 1, or `NA`; otherwise
#' returns `x` coerced to character (or `default` if that coercion is blank).
#' Used throughout this file instead of `%||%`, which this package does not
#' import (base R only gained it in 4.4.0, and `DESCRIPTION` allows R >= 4.1).
#' @param x A scalar value, or `NULL`.
#' @param default Character scalar to fall back to.
#' @return A length-1 character string.
.report_or_default <- function(x, default) {
  if (is.null(x) || length(x) != 1 || is.na(x)) {
    return(default)
  }
  x_chr <- as.character(x)
  if (!nzchar(x_chr)) default else x_chr
}

#' @keywords internal
#' @title Human-Readable Text for One Condition Operator
#' @description
#' Converts a single condition-DSL operator/value pair (as used in
#' `DTARuleColCondition@condition`/`@then` and one entry of
#' `DTARuleGroupCondition@conditions`) into a short readable fragment such as
#' `"= 18"` or `"in {A, B}"`. Every recognized operator from
#' `dta_condition_operators()` (`R/evaluateRules.R`) is covered.
#' @param op Character scalar operator name.
#' @param value The operator's right-hand side.
#' @return A length-1 character string, HTML-escaped where it embeds data.
.report_condition_operator_text <- function(op, value) {
  op <- as.character(op)[1]

  if (identical(op, "range")) {
    v <- as.character(unlist(value))
    lo <- if (length(v) >= 1) .report_html_escape(v[[1]]) else ""
    hi <- if (length(v) >= 2) .report_html_escape(v[[2]]) else ""
    return(paste0("between ", lo, " and ", hi))
  }

  if (identical(op, "empty")) {
    return(if (isTRUE(value)) "is empty" else "is not empty")
  }

  val_text <- .report_html_escape(paste(as.character(unlist(value)), collapse = ", "))

  switch(op,
    equals = ,
    equal = paste0("= ", val_text),
    not_equals = ,
    not_equal = paste0("&#8800; ", val_text),
    `in` = paste0("in {", val_text, "}"),
    not_in = paste0("not in {", val_text, "}"),
    greater = paste0("&gt; ", val_text),
    less = paste0("&lt; ", val_text),
    greater_equal = ,
    min = paste0("&gt;= ", val_text),
    less_equal = ,
    max = paste0("&lt;= ", val_text),
    pattern = paste0("matches pattern ", val_text),
    paste0(.report_html_escape(op), " ", val_text)
  )
}

#' @keywords internal
#' @title Human-Readable Text for a Condition Map
#' @description
#' Converts a condition map -- a named list keyed by column, each value a
#' list of one or more `operator = value` pairs -- into one readable phrase,
#' e.g. `"AGE &gt;= 18 and STATUS = COMPLETED"`. A column can legitimately
#' carry more than one operator at once (e.g. `WEIGHT: {min: 1, max: 2}`;
#' `evaluate_condition()` in `R/evaluateRules.R` ANDs every operator
#' supplied), so every operator for a column is rendered, not just the
#' first. Used for `DTARuleColCondition`'s `condition`/`then` and each named
#' entry of `DTARuleGroupCondition@conditions`.
#' @param cond A named list, or `NULL`/empty/not list-shaped.
#' @return A length-1 character string (possibly `""`), HTML-escaped.
.report_condition_to_text <- function(cond) {
  if (is.null(cond) || length(cond) == 0 || is.null(names(cond))) {
    return("")
  }

  parts <- vapply(names(cond), function(col) {
    spec <- cond[[col]]
    if (is.list(spec) && length(spec) > 0 && !is.null(names(spec))) {
      op_texts <- vapply(names(spec), function(op) {
        paste0(.report_html_escape(col), " ", .report_condition_operator_text(op, spec[[op]]))
      }, character(1))
      paste(op_texts, collapse = " and ")
    } else {
      .report_html_escape(col)
    }
  }, character(1))

  paste(parts, collapse = " and ")
}

#' @keywords internal
#' @title Human-Readable "Should Be" Text for a Column Spec Violation
#' @description
#' Every columnspec keyword this package's validator emits (`required`,
#' `additionalProperties`, `type`, `maxLength`, `enum`, `const`, `pattern` --
#' see `dta_columnspec_error_rows()` call sites in `R/columnSpecChecks.R`) puts
#' the constraint's own value in the flattened `columnspec_columnspec`
#' column (e.g. the allowed values for `enum`, the limit for `maxLength`).
#' `additionalProperties` is the exception: there is no spec to quote, which is
#' the entire finding, so its sentence is fixed.
#' This turns that plus the keyword into one plain-language sentence instead
#' of surfacing the validator's generic message.
#' @param row1 A one-row data.frame: the first row of one message's
#'   `inspect()` output.
#' @return A length-1 character string, HTML-escaped.
.report_columnspec_expected_text <- function(row1) {
  fallback <- function() {
    msg <- if ("columnspec_message" %in% names(row1) && !is.na(row1[["columnspec_message"]])) {
      row1[["columnspec_message"]]
    } else if ("message" %in% names(row1) && !is.na(row1[["message"]])) {
      row1[["message"]]
    } else {
      "(unknown constraint)"
    }
    .report_html_escape(msg)
  }

  kw <- if ("columnspec_keyword" %in% names(row1)) row1[["columnspec_keyword"]] else NA
  if (is.na(kw)) {
    return(fallback())
  }
  kw <- as.character(kw)

  spec <- if ("columnspec_columnspec" %in% names(row1)) row1[["columnspec_columnspec"]] else NA
  # The enum keyword's spec is a "; "-joined list, and type's is a plain
  # ","-joined list -- different separators for different keywords
  # (dta_columnspec_error_rows() call sites in R/columnSpecChecks.R). Escape
  # first, then re-join for prose so the multi-value keywords each get their
  # own readable separator.
  spec_text <- if (!is.na(spec)) .report_html_escape(as.character(spec)) else ""
  spec_text_prose <- gsub("; ", ", ", spec_text, fixed = TRUE)

  switch(kw,
    required = "The value must be present (not missing).",
    additionalProperties = "The specs declare no such column; it must not be present.",
    type = paste0("Must be of type: ", gsub(",", ", ", spec_text, fixed = TRUE), "."),
    maxLength = paste0("Must be at most ", spec_text, " character(s)."),
    enum = paste0("Must be one of: ", spec_text_prose, "."),
    const = paste0("Must be exactly: ", spec_text, "."),
    pattern = paste0("Must match the pattern: ", spec_text),
    fallback()
  )
}

#' @keywords internal
#' @title The Actual Offending Value for a Column Spec Violation
#' @param row1 A one-row data.frame: the first row of one message's
#'   `inspect()` output.
#' @return A length-1 character string, HTML-escaped.
.report_columnspec_actual_text <- function(row1) {
  kw <- if ("columnspec_keyword" %in% names(row1)) as.character(row1[["columnspec_keyword"]]) else NA_character_
  data_val <- if ("columnspec_data" %in% names(row1)) row1[["columnspec_data"]] else NA

  if (identical(kw, "required") || is.na(data_val) || !nzchar(as.character(data_val))) {
    # `additionalProperties` reaches this through the NA `data` test, not the
    # keyword test, and lands on the same text -- which reads backwards for a
    # column that is emphatically present. Name what was actually found.
    if (identical(kw, "additionalProperties")) {
      return("(an undeclared column)")
    }
    return("(missing / not present)")
  }
  .report_html_escape(as.character(data_val))
}

#' @keywords internal
#' @title "Should Be" Text for an Import (Type-Coercion) Error
#' @param row1 A one-row data.frame: the first row of one message's
#'   `inspect()` output.
#' @return A length-1 character string, HTML-escaped.
.report_import_expected_text <- function(row1) {
  declared <- if ("import_declared_type" %in% names(row1)) row1[["import_declared_type"]] else NA
  declared_text <- .report_or_default(declared, "")
  if (nzchar(declared_text)) {
    paste0("A value representable as the declared type: ", .report_html_escape(declared_text), ".")
  } else {
    "A value representable in the column's declared type."
  }
}

#' @keywords internal
#' @title The Raw Offending Value for an Import (Type-Coercion) Error
#' @param row1 A one-row data.frame: the first row of one message's
#'   `inspect()` output.
#' @return A length-1 character string, HTML-escaped.
.report_import_actual_text <- function(row1) {
  raw <- if ("import_raw" %in% names(row1)) row1[["import_raw"]] else NA
  raw_text <- .report_or_default(raw, "")
  if (nzchar(raw_text)) .report_html_escape(raw_text) else "(empty)"
}

#' @keywords internal
#' @title Human-Readable "Should Be" Text for a (Non-Group) Rule Violation
#' @description
#' Builds a plain-language description of the constraint straight from the
#' rule's own S7 properties, specific to its class
#' (`DTARuleColRange`/`DTARuleColUnique`/`DTARuleColCondition`).
#' `DTARuleGroupCondition` is handled separately by
#' `.report_group_condition_expected_text()` -- its constraint is a
#' group-level relationship, not a single per-row bound.
#' @param rule_def An S7 rule object, or `NULL` when it could not be resolved.
#' @return A length-1 character string, HTML-escaped where it embeds data.
.report_rule_expected_text <- function(rule_def) {
  if (is.null(rule_def)) {
    return("(rule definition not available; see technical detail below)")
  }

  if (inherits(rule_def, "DTAtools::DTARuleColRange")) {
    cols <- .report_html_escape(paste(rule_def@columns, collapse = ", "))
    has_min <- length(rule_def@min) == 1 && !is.na(rule_def@min)
    has_max <- length(rule_def@max) == 1 && !is.na(rule_def@max)
    bound <- if (has_min && has_max) {
      paste0(
        "between ", .report_html_escape(rule_def@min), " and ",
        .report_html_escape(rule_def@max), " (inclusive)"
      )
    } else if (has_min) {
      paste0("at least ", .report_html_escape(rule_def@min))
    } else if (has_max) {
      paste0("at most ", .report_html_escape(rule_def@max))
    } else {
      "within the configured range"
    }
    return(paste0("Values in ", cols, " must be ", bound, "."))
  }

  if (inherits(rule_def, "DTAtools::DTARuleColUnique")) {
    cols <- .report_html_escape(paste(rule_def@columns, collapse = ", "))
    return(paste0("Values in ", cols, " must be unique across all rows."))
  }

  if (inherits(rule_def, "DTAtools::DTARuleColCondition")) {
    cond_text <- .report_condition_to_text(rule_def@condition)
    then_text <- .report_condition_to_text(rule_def@then)
    if (nzchar(cond_text) && nzchar(then_text)) {
      return(paste0("IF ", cond_text, " THEN ", then_text, " must hold."))
    }
    return("The configured IF/THEN condition must hold.")
  }

  "(see the rule definition below)"
}

#' @keywords internal
#' @title "Should Be" Text for a Group Condition Violation
#' @description
#' A group_condition rule's constraint is a relationship across the rows of
#' one group, not a single per-row bound, so the "should be" box only points
#' at the "What this rule checks" explanation (built by
#' `.report_rule_explain_html()`) rather than trying to compress the whole
#' constraint into one line.
#' @param rule_def A `DTARuleGroupCondition` object, or `NULL`.
#' @return A length-1 character string, HTML-escaped where it embeds data.
.report_group_condition_expected_text <- function(rule_def) {
  if (is.null(rule_def)) {
    return("A group-level constraint (see technical detail below) must hold.")
  }
  group_by <- .report_html_escape(paste(rule_def@group_by, collapse = ", "))
  paste0(
    "Within each group of rows sharing the same ", group_by,
    ", the constraint(s) described above (\"What this rule checks\") must hold."
  )
}

#' @keywords internal
#' @title Human-Readable Text for One Group Condition Constraint
#' @description
#' `constraints` is a plain list (not S7), each element itself a list with
#' `id`, `type` (`"mutually_exclusive"` or `"requires"`), and either
#' `left`/`right` or `` `if`  ``/`` `then` `` plus `if_scope`/`then_scope`
#' (see `R/DTARuleGroupCondition-class.R`, and the constraint construction in
#' `R/evaluateRules.R`).
#' @param constraint One element of `DTARuleGroupCondition@constraints`.
#' @return A length-1 character string: one `<dt>`/`<dd>` pair, HTML-escaped
#'   where it embeds data.
.report_group_constraint_text <- function(constraint) {
  if (!is.list(constraint)) {
    return("")
  }
  cid <- .report_or_default(constraint$id, "(constraint)")
  ctype <- .report_or_default(constraint$type, "")

  # DTARuleGroupCondition-class.R's normalize_scope() defaults EVERY scope
  # field (left_scope/right_scope/if_scope/then_scope) to "any" when unset --
  # not "all" -- so every fallback below matches that, not a guess.
  desc <- if (identical(ctype, "mutually_exclusive")) {
    left <- .report_html_escape(.report_or_default(constraint$left, ""))
    right <- .report_html_escape(.report_or_default(constraint$right, ""))
    left_scope <- .report_html_escape(.report_or_default(constraint$left_scope, "any"))
    right_scope <- .report_html_escape(.report_or_default(constraint$right_scope, "any"))
    paste0(
      "\"", left, "\" (", left_scope, " row(s)) and \"", right, "\" (", right_scope,
      " row(s)) must never both hold within the same group."
    )
  } else if (identical(ctype, "requires")) {
    if_name <- .report_html_escape(.report_or_default(constraint[["if"]], ""))
    then_name <- .report_html_escape(.report_or_default(constraint[["then"]], ""))
    if_scope <- .report_html_escape(.report_or_default(constraint$if_scope, "any"))
    then_scope <- .report_html_escape(.report_or_default(constraint$then_scope, "any"))
    paste0(
      "When \"", if_name, "\" holds for ", if_scope,
      " row(s) in the group, \"", then_name, "\" must hold for ",
      then_scope, " row(s) in the group."
    )
  } else {
    paste0("Constraint type: ", .report_html_escape(ctype), ".")
  }

  paste0(
    "<dt>Constraint ", .report_html_escape(cid), " (", .report_html_escape(ctype), ")</dt><dd>",
    desc, "</dd>"
  )
}

#' @keywords internal
#' @title "What This Rule Checks" Body for a Group Condition Rule
#' @param rule_def A `DTARuleGroupCondition` object.
#' @return A length-1 character string of `<dt>`/`<dd>` pairs.
.report_group_condition_explain_body <- function(rule_def) {
  group_html <- paste0(
    "<dt>Grouped by</dt><dd>",
    .report_html_escape(paste(rule_def@group_by, collapse = ", ")), "</dd>"
  )

  conditions <- rule_def@conditions
  cond_html <- ""
  if (!is.null(conditions) && length(conditions) > 0 && !is.null(names(conditions))) {
    cond_items <- vapply(names(conditions), function(nm) {
      text <- .report_condition_to_text(conditions[[nm]])
      paste0(
        "<dt>", .report_html_escape(nm), "</dt><dd>",
        if (nzchar(text)) text else "(condition)", "</dd>"
      )
    }, character(1))
    cond_html <- paste(cond_items, collapse = "")
  }

  constraints <- rule_def@constraints
  constraint_html <- ""
  if (!is.null(constraints) && length(constraints) > 0) {
    constraint_items <- vapply(constraints, .report_group_constraint_text, character(1))
    constraint_html <- paste(constraint_items, collapse = "")
  }

  paste0(group_html, cond_html, constraint_html)
}

#' @keywords internal
#' @title "What This Rule Checks" Explanation Box
#' @description
#' A neutral (not pass/fail-colored) box describing the rule's own
#' definition, shown above the should-be/actual comparison so a reader
#' understands the rule's intent before its failure. Only built for
#' `type == "rule"` messages; the caller skips it entirely for
#' columnspec/import messages, whose "should be" text already states the
#' constraint in full.
#' @param rule_def An S7 rule object, or `NULL`.
#' @return A length-1 character string (`""` when `rule_def` is `NULL`).
.report_rule_explain_html <- function(rule_def) {
  if (is.null(rule_def)) {
    return("")
  }

  id_text <- .report_or_default(tryCatch(rule_def@id, error = function(e) NULL), "")
  id_dt <- if (nzchar(id_text)) {
    paste0("<dt>Rule ID</dt><dd>", .report_html_escape(id_text), "</dd>")
  } else {
    ""
  }
  type_dt <- paste0("<dt>Rule type</dt><dd>", .report_html_escape(.report_rule_type_label(rule_def)), "</dd>")

  desc <- tryCatch(rule_def@description, error = function(e) NULL)
  desc_html <- if (!is.null(desc) && length(desc) == 1 && !is.na(desc) && nzchar(desc)) {
    paste0("<dt>Description</dt><dd>", .report_html_escape(desc), "</dd>")
  } else {
    ""
  }

  body <- if (inherits(rule_def, "DTAtools::DTARuleGroupCondition")) {
    .report_group_condition_explain_body(rule_def)
  } else if (inherits(rule_def, "DTAtools::DTARuleColCondition")) {
    cond_text <- .report_condition_to_text(rule_def@condition)
    then_text <- .report_condition_to_text(rule_def@then)
    paste0(
      "<dt>IF</dt><dd>", if (nzchar(cond_text)) cond_text else "(condition)", "</dd>",
      "<dt>THEN</dt><dd>", if (nzchar(then_text)) then_text else "(condition)", "</dd>"
    )
  } else if (inherits(rule_def, "DTAtools::DTARuleColRange")) {
    paste0(
      "<dt>Columns</dt><dd>", .report_html_escape(paste(rule_def@columns, collapse = ", ")), "</dd>",
      "<dt>Allowed range</dt><dd>", .report_rule_expected_text(rule_def), "</dd>"
    )
  } else if (inherits(rule_def, "DTAtools::DTARuleColUnique")) {
    paste0(
      "<dt>Columns</dt><dd>", .report_html_escape(paste(rule_def@columns, collapse = ", ")), "</dd>"
    )
  } else {
    ""
  }

  glue::glue(
    '<div class="inspect-explain">
  <div class="inspect-explain-title">What this rule checks</div>
  <dl>
{id_dt}
{type_dt}
{desc_html}
{body}
  </dl>
</div>'
  )
}

#' @keywords internal
#' @title Table of Every Captured Offending Row for a Rule Violation
#' @description
#' Unlike a single should-be/actual line, this renders EVERY row `inspect()`
#' captured for the message (not just the first) -- `failing_*` columns when
#' present, else `context_*` -- deduplicated, since flattening can recycle
#' the same preview across the multiple rows one message id can carry.
#' @param inspect_rows A data.frame with one or more rows, all for the same
#'   message id.
#' @return A length-1 character string: an HTML table, or a "none captured"
#'   placeholder.
.report_rule_failing_rows_html <- function(inspect_rows) {
  nms <- names(inspect_rows)
  failing_cols <- nms[grepl("^failing_", nms)]
  context_cols <- nms[grepl("^context_", nms)]
  use_cols <- if (length(failing_cols) > 0) failing_cols else context_cols
  prefix_pattern <- if (length(failing_cols) > 0) "^failing_" else "^context_"

  if (length(use_cols) == 0) {
    return('<p class="inspect-none">No offending rows were captured.</p>')
  }

  sub <- inspect_rows[, use_cols, drop = FALSE]
  names(sub) <- sub(prefix_pattern, "", names(sub))
  names(sub)[names(sub) == ".row"] <- "Row"
  if ("Row" %in% names(sub)) {
    sub <- sub[, c("Row", setdiff(names(sub), "Row")), drop = FALSE]
  }

  sub <- unique(sub)
  if (nrow(sub) == 0) {
    return('<p class="inspect-none">No offending rows were captured.</p>')
  }

  .report_kv_table_html(sub)
}

#' @keywords internal
#' @title Table of Every Captured Group Condition Violation
#' @description
#' A single message id can carry more than one violation record (e.g. more
#' than one group, or more than one failing constraint); this renders all of
#' them, deduplicated, rather than only the first.
#' @param inspect_rows A data.frame with one or more rows, all for the same
#'   message id.
#' @return A length-1 character string: an HTML table, or a "none captured"
#'   placeholder.
.report_group_violation_breakdown_html <- function(inspect_rows) {
  nms <- names(inspect_rows)
  gv_cols <- nms[grepl("^group_violation_", nms)]
  if (length(gv_cols) == 0) {
    return('<p class="inspect-none">No violation detail was captured.</p>')
  }

  sub <- inspect_rows[, gv_cols, drop = FALSE]
  names(sub) <- sub("^group_violation_", "", names(sub))

  # Drop rows that are entirely empty (padding from flattening's recycling
  # of a shorter detail frame up to the record's row count).
  has_content <- vapply(seq_len(nrow(sub)), function(i) {
    row_vals <- as.character(unlist(sub[i, , drop = FALSE]))
    any(!is.na(row_vals) & nzchar(row_vals))
  }, logical(1))
  sub <- sub[has_content, , drop = FALSE]
  sub <- unique(sub)

  if (nrow(sub) == 0) {
    return('<p class="inspect-none">No violation detail was captured.</p>')
  }

  names(sub) <- tools::toTitleCase(names(sub))
  .report_kv_table_html(sub)
}

#' @keywords internal
#' @title Render a Data Frame as an HTML Table
#' @description
#' Shared table-body builder for `.report_rule_failing_rows_html()` and
#' `.report_group_violation_breakdown_html()`: every cell is escaped, `NA`
#' renders as an empty cell.
#' @param df A data.frame with at least one row and one column.
#' @return A length-1 character string containing a `<table>`.
.report_kv_table_html <- function(df) {
  header <- paste0("<th>", .report_html_escape(names(df)), "</th>", collapse = "")
  body_rows <- vapply(seq_len(nrow(df)), function(i) {
    cells <- vapply(names(df), function(k) {
      val <- df[[k]][i]
      cell_text <- if (is.na(val)) "" else .report_html_escape(as.character(val))
      paste0("<td>", cell_text, "</td>")
    }, character(1))
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  }, character(1))

  paste0(
    '<table class="report-table inspect-rows-table"><thead><tr>', header,
    "</tr></thead><tbody>", paste(body_rows, collapse = ""), "</tbody></table>"
  )
}

#' @keywords internal
#' @title Build Inspection Detail Panel HTML
#' @description
#' Renders a hidden detail panel for one validation message: a plain-language
#' "should be" vs "actual" comparison, a "What this rule checks" box for rule
#' violations, and the raw technical detail in a collapsible section.
#' @param inspect_rows A data.frame with ONE OR MORE rows, all sharing the
#'   same message `id`, as returned by
#'   `DTAtools::inspect(dta, id = id, as_tibble = FALSE)`. `inspect()` can
#'   legitimately return more than one row per id (a rule's failing-row
#'   preview, or multiple group_condition violations); every row is used
#'   where it matters (the "actual" tables), while metadata that's constant
#'   across rows (`id`, `type`, `message`, `headline`) is read from the first.
#' @param rule_def Optional S7 rule object to describe; `NULL` for
#'   columnspec/import messages, or when the rule definition could not be
#'   resolved.
#' @return A length-1 character string containing the HTML detail panel.
.report_inspect_panel_html <- function(inspect_rows, rule_def = NULL) {
  row1 <- inspect_rows[1, , drop = FALSE]

  id <- if ("id" %in% names(row1)) row1[["id"]] else NA
  if (is.na(id)) {
    return("")
  }

  type <- if ("type" %in% names(row1) && !is.na(row1[["type"]])) as.character(row1[["type"]]) else "unknown"

  badge_label <- switch(type,
    rule = "Rule failure",
    columnspec = "Column spec violation",
    import = "Import error",
    "Message"
  )

  # Plain message only -- not the "[dataset/target] message" headline
  # dta_inspect_tabular_message() also provides (R/validationReporting.R),
  # which repeats context the panel's own dataset/target/badge already show.
  headline <- if ("message" %in% names(row1) && !is.na(row1[["message"]])) {
    .report_html_escape(row1[["message"]])
  } else {
    "Validation message"
  }

  is_group_condition <- !is.null(rule_def) && inherits(rule_def, "DTAtools::DTARuleGroupCondition")
  explain_html <- if (identical(type, "rule")) .report_rule_explain_html(rule_def) else ""

  if (identical(type, "columnspec")) {
    should_be <- .report_columnspec_expected_text(row1)
    actual <- .report_columnspec_actual_text(row1)
  } else if (identical(type, "import")) {
    should_be <- .report_import_expected_text(row1)
    actual <- .report_import_actual_text(row1)
  } else if (identical(type, "rule") && is_group_condition) {
    should_be <- .report_group_condition_expected_text(rule_def)
    # The violation summary (which group, which constraint, why) plus the
    # actual column VALUES for the rows involved -- dta_inspect_tabular_message()
    # populates failing_rows_preview for group_condition too (its preview_cols
    # includes group_by + every condition's columns), so the same
    # .report_rule_failing_rows_html() table applies here as for other rules.
    affected_rows_html <- .report_rule_failing_rows_html(inspect_rows)
    actual <- paste0(
      .report_group_violation_breakdown_html(inspect_rows),
      '<p class="inspect-actual-note">Affected rows (column values):</p>',
      affected_rows_html
    )
  } else if (identical(type, "rule")) {
    should_be <- .report_rule_expected_text(rule_def)
    actual <- .report_rule_failing_rows_html(inspect_rows)
  } else {
    should_be <- if ("message" %in% names(row1) && !is.na(row1[["message"]])) {
      .report_html_escape(row1[["message"]])
    } else {
      ""
    }
    actual <- "(see technical detail below)"
  }

  # Technical detail: the first row's raw columns, key/value.
  kv_rows <- character(0)
  for (col_name in names(row1)) {
    val <- row1[[col_name]]
    if (is.na(val) || is.null(val)) {
      next
    }
    kv_rows <- c(kv_rows, sprintf(
      "<tr><td>%s</td><td>%s</td></tr>",
      .report_html_escape(col_name),
      .report_html_escape(as.character(val))
    ))
  }

  kv_html <- if (length(kv_rows) > 0) {
    paste0(kv_rows, collapse = "\n      ")
  } else {
    ""
  }

  glue::glue(
    '<div class="inspect-panel" id="inspect-panel-{id}" hidden>
  <div class="inspect-summary">
    <span class="inspect-badge inspect-badge-{type}">{badge_label}</span>
    <div class="inspect-msg">{headline}</div>
  </div>
{explain_html}
  <div class="inspect-cmp">
    <div class="inspect-box inspect-expected">
      <div class="inspect-box-title">&#x2714; Should be</div>
      <div class="inspect-box-body">{should_be}</div>
    </div>
    <div class="inspect-box inspect-actual">
      <div class="inspect-box-title">&#x2716; Actual</div>
      <div class="inspect-box-body">{actual}</div>
    </div>
  </div>
  <details class="inspect-details">
    <summary>Full technical detail</summary>
    <table class="report-table inspect-kv-table">
      <tbody>
{kv_html}
      </tbody>
    </table>
  </details>
</div>'
  )
}
