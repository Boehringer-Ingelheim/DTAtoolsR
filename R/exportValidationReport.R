#' @title Export a Standalone HTML Validation Report
#' @description
#' Renders a self-contained HTML file (no external assets) summarizing
#' validation results for a `DTA` object: a pass/fail overview per
#' dataset/target, and a sortable, filterable table of every validation
#' message with click-to-inspect detail. Repeated identical messages (same
#' dataset/target/source/rule/column/keyword/message, differing only by row)
#' are capped at `max_repeats` in the default view with a "show more" toggle
#' in the resulting HTML.
#' @import S7
#' @importFrom cli cli_abort cli_alert_success
#' @importFrom glue glue
#'
#' @param x A `DTA` object, already validated via [check()].
#' @param file Character. Output `.html` file path.
#' @param max_repeats Positive integer (default `5`). Maximum number of
#'   repeated identical messages shown before the rest collapse behind a
#'   "show more" toggle in the report. Pass `NULL` or `Inf` to disable
#'   capping and always show every message.
#' @param overwrite Logical. Whether to overwrite an existing file at `file`.
#'   Default `FALSE`.
#' @param quiet Logical. If `TRUE`, suppresses the success message. Default
#'   `FALSE`.
#' @param title Optional character scalar used as the report's page title
#'   and heading. Defaults to the DTA's metadata title (`metadata(x)@title`)
#'   when available, else `"Validation Report"`.
#' @return Invisibly returns `file`.
#' @export
#' @examples
#' dta <- create_example_DTA()
#' dta <- check(dta, quiet = TRUE)
#' out <- tempfile(fileext = ".html")
#' write_validation_report(dta, out)
write_validation_report <- function(
  x, file, max_repeats = 5, overwrite = FALSE, quiet = FALSE, title = NULL
) {
  # Validate input
  if (!inherits(x, "DTAtools::DTA")) {
    cli::cli_abort("'x' must be a DTA object.")
  }

  # Check file overwrite
  if (file.exists(file) && !overwrite) {
    cli::cli_abort("File '{file}' already exists. Set overwrite = TRUE to replace.")
  }

  if (!is.null(title) && length(title) != 1) {
    cli::cli_abort("'title' must be a single character string, or NULL.")
  }

  # Determine title. `metadata(x)` returns a `DTAMetaData` S7 object -- `@title`,
  # not `$title` (S7 objects don't support `$` property access). `nzchar(NA)`
  # is TRUE by default (a well-known nzchar() gotcha), so `is.na()` is
  # checked explicitly -- otherwise a `title = NA_character_` caller would
  # skip the fallback entirely and end up with an empty `<title>`/`<h1>`.
  if (is.null(title) || is.na(title) || !nzchar(title)) {
    title <- tryCatch(
      {
        meta_title <- DTAtools::metadata(x)@title
        if (!is.null(meta_title) && length(meta_title) == 1 && nzchar(meta_title)) {
          meta_title
        } else {
          NULL
        }
      },
      error = function(e) NULL
    )

    if (is.null(title)) {
      title <- "Validation Report"
    }
  }

  # Get results and messages
  res <- DTAtools::results(x)
  msgs <- DTAtools::messages(x, as_tibble = FALSE)

  # Fetch inspect() detail for every message with ONE call, not one call per
  # row: inspect() internally re-collects and re-sorts the full messages()
  # table on every invocation (see dta_get_message_rows_by_id() /
  # dta_collect_messages_for_dataset() in R/validationReporting.R), so
  # calling it inside the per-message loop below made report generation
  # O(n^2) in the message count -- exactly the workload `max_repeats`
  # capping implies can be large. `split()` then gives each message id's
  # row(s) in O(1) average lookup instead of re-scanning per row.
  inspect_index <- NULL
  if (nrow(msgs) > 0) {
    inspect_all <- tryCatch(
      DTAtools::inspect(x, id = msgs$id, as_tibble = FALSE),
      error = function(e) NULL
    )
    if (!is.null(inspect_all) && nrow(inspect_all) > 0 && "id" %in% names(inspect_all)) {
      inspect_index <- list(df = inspect_all, by_id = split(seq_len(nrow(inspect_all)), inspect_all$id))
    }
  }

  # Build inspect panels for each message
  inspect_panels <- character(nrow(msgs))

  for (i in seq_len(nrow(msgs))) {
    msg_id <- msgs$id[i]
    msg_row <- msgs[i, , drop = FALSE]
    dataset_name <- as.character(msg_row$dataset)
    source_type <- as.character(msg_row$source)

    # inspect() can legitimately return more than one row for a single id
    # (e.g. multiple matching JSON-schema constraints); as before, only the
    # first is rendered.
    rows_for_id <- if (!is.null(inspect_index)) inspect_index$by_id[[as.character(msg_id)]] else NULL
    inspect_row <- if (!is.null(rows_for_id)) {
      inspect_index$df[rows_for_id[1], , drop = FALSE]
    } else {
      msg_row
    }

    # Look up rule definition if this is a rule message
    rule_def <- NULL
    if (source_type == "rule" && !is.na(msg_row$rule_id)) {
      rule_id <- as.character(msg_row$rule_id)
      tryCatch(
        {
          ds <- x@datasets[[dataset_name]]
          if (!is.null(ds) && !is.null(ds@specs) && !is.null(ds@specs@rules)) {
            rules_list <- ds@specs@rules
            for (r in rules_list) {
              if (!is.null(r@id) && identical(r@id, rule_id)) {
                rule_def <- r
                break
              }
            }
          }
        },
        error = function(e) NULL
      )
    }

    # Build inspect panel
    inspect_panels[i] <- .report_inspect_panel_html(inspect_row, rule_def = rule_def)
  }

  inspect_panels_html <- paste0(inspect_panels, collapse = "\n")

  # Read CSS (with fallback to empty string)
  css_content <- tryCatch(
    {
      css_path <- system.file("report", "report.css", package = "DTAtools")
      if (nzchar(css_path) && file.exists(css_path)) {
        paste0(readLines(css_path, warn = FALSE), collapse = "\n")
      } else {
        ""
      }
    },
    error = function(e) ""
  )

  # Read JS (with fallback to empty string)
  js_content <- tryCatch(
    {
      js_path <- system.file("report", "report.js", package = "DTAtools")
      if (nzchar(js_path) && file.exists(js_path)) {
        paste0(readLines(js_path, warn = FALSE), collapse = "\n")
      } else {
        ""
      }
    },
    error = function(e) ""
  )

  # Build HTML document
  summary_html <- .report_summary_html(res)
  messages_html <- .report_messages_table_html(msgs, max_repeats = max_repeats)
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  escaped_title <- .report_html_escape(title)

  html_doc <- glue::glue(
    '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>{escaped_title}</title>
<style>
{css_content}
</style>
</head>
<body>
<header class="report-header">
  <h1>{escaped_title}</h1>
  <div class="report-meta">Generated {timestamp}</div>
</header>
{summary_html}
{messages_html}
<div id="inspect-panels" hidden>
{inspect_panels_html}
</div>
<div id="inspect-overlay" class="inspect-overlay" hidden>
  <div class="inspect-overlay-backdrop" id="inspect-overlay-backdrop"></div>
  <div class="inspect-overlay-panel">
    <button type="button" class="inspect-overlay-close" id="inspect-overlay-close" aria-label="Close">&times;</button>
    <div id="inspect-overlay-content"></div>
  </div>
</div>
<script>
{js_content}
</script>
</body>
</html>'
  )

  # Write file
  writeLines(html_doc, con = file)

  # Report success
  if (!isTRUE(quiet)) {
    cli::cli_alert_success("Report saved to {file}")
  }

  invisible(file)
}
