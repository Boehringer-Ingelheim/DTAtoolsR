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
#' @title Build Inspection Detail Panel HTML
#' @description
#' Renders a hidden detail panel for a single validation message, showing
#' expected vs. actual values and technical details.
#' @param inspect_row A one-row data.frame/list as returned by
#'   `DTAtools::inspect(dta, id = id, as_tibble = FALSE)`.
#' @param rule_def Optional S7 rule object to describe; if `NULL`, a generic
#'   message is used.
#' @return A length-1 character string containing the HTML detail panel.
.report_inspect_panel_html <- function(inspect_row, rule_def = NULL) {
  # Extract id safely
  id <- if ("id" %in% names(inspect_row)) {
    inspect_row[["id"]] %||% NA
  } else {
    NA
  }

  if (is.na(id)) {
    return("")
  }

  # Extract type
  type <- if ("type" %in% names(inspect_row)) {
    inspect_row[["type"]] %||% "unknown"
  } else {
    "unknown"
  }

  # Map type to badge label
  badge_label <- switch(type,
    rule = "Rule failure",
    columnspec = "Column spec violation",
    import = "Import error",
    "Message"
  )

  # Extract message/headline
  headline <- if ("headline" %in% names(inspect_row) && !is.na(inspect_row[["headline"]])) {
    .report_html_escape(inspect_row[["headline"]])
  } else if ("message" %in% names(inspect_row) && !is.na(inspect_row[["message"]])) {
    .report_html_escape(inspect_row[["message"]])
  } else {
    "Validation message"
  }

  # Build "Should be" text
  should_be <- ""
  if (type == "columnspec") {
    should_be <- if ("columnspec_message" %in% names(inspect_row) &&
      !is.na(inspect_row[["columnspec_message"]]) &&
      nzchar(inspect_row[["columnspec_message"]])) {
      .report_html_escape(inspect_row[["columnspec_message"]])
    } else if ("message" %in% names(inspect_row) && !is.na(inspect_row[["message"]])) {
      .report_html_escape(inspect_row[["message"]])
    } else {
      ""
    }
  } else if (type == "rule") {
    if (!is.null(rule_def)) {
      # rule_label is one of this function's own fixed strings, already
      # safe; rule_id_text is spec-author-controlled (DTARule@id only
      # forbids whitespace, not HTML-special characters) and must be
      # escaped like any other embedded data.
      rule_label <- .report_rule_type_label(rule_def)
      rule_id_text <- if (!is.null(rule_def@id)) .report_html_escape(rule_def@id) else ""
      should_be <- if (nzchar(rule_id_text)) {
        paste0("Rule ", rule_id_text, " (", rule_label, ")")
      } else {
        rule_label
      }
    } else {
      should_be <- if ("message" %in% names(inspect_row) && !is.na(inspect_row[["message"]])) {
        .report_html_escape(inspect_row[["message"]])
      } else {
        ""
      }
    }
  } else if (type == "import") {
    should_be <- "a value representable in the column's declared type"
  } else {
    should_be <- if ("message" %in% names(inspect_row) && !is.na(inspect_row[["message"]])) {
      .report_html_escape(inspect_row[["message"]])
    } else {
      ""
    }
  }

  # Build "Actual" text
  actual <- ""
  if (type == "columnspec") {
    actual <- if ("columnspec_data" %in% names(inspect_row) && !is.na(inspect_row[["columnspec_data"]])) {
      .report_html_escape(as.character(inspect_row[["columnspec_data"]]))
    } else {
      "(see technical detail below)"
    }
  } else if (type == "rule") {
    # Look for failing_* or context_* columns
    failing_cols <- names(inspect_row)[grepl("^failing_", names(inspect_row))]
    context_cols <- names(inspect_row)[grepl("^context_", names(inspect_row))]
    combined_cols <- c(failing_cols, context_cols)

    actual_parts <- character(0)
    for (col in combined_cols) {
      val <- inspect_row[[col]]
      if (!is.na(val) && nzchar(as.character(val))) {
        # col_name is a real dataset column name (spec-author-controlled),
        # not literal source code -- escape it the same as the value.
        col_name <- .report_html_escape(sub("^(failing|context)_", "", col))
        actual_parts <- c(actual_parts, paste0(
          col_name, ": ", .report_html_escape(as.character(val))
        ))
      }
    }

    actual <- if (length(actual_parts) > 0) {
      paste(actual_parts, collapse = "; ")
    } else {
      "(see technical detail below)"
    }
  } else if (type == "import") {
    actual <- "(see technical detail below)"
  } else {
    actual <- "(see technical detail below)"
  }

  # Build key/value table for all fields
  kv_rows <- character(0)
  for (col_name in names(inspect_row)) {
    val <- inspect_row[[col_name]]
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
