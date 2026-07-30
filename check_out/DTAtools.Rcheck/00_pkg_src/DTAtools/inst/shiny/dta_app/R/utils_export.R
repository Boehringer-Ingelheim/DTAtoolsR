# =============================================================================
# Export modal dialog helpers for the DTAtools Shiny app
# =============================================================================

# List available custom templates from the templates directory
list_available_templates <- function() {
  templates_dir <- system.file("extdata", "templates", package = "DTAtools")
  if (!dir.exists(templates_dir)) {
    return(character(0))
  }
  files <- list.files(templates_dir, pattern = "\\.docx$", full.names = FALSE)
  sort(files)
}

# Get full path to a template by name
get_template_path <- function(template_name) {
  if (is.null(template_name) || !nzchar(template_name)) {
    return(NULL)
  }
  templates_dir <- system.file("extdata", "templates", package = "DTAtools")
  full_path <- file.path(templates_dir, template_name)
  if (file.exists(full_path)) full_path else NULL
}

# Embed YAML block at the end of markdown text, wrapped in HTML comment
embed_yaml_markdown <- function(markdown_text, dta) {
  res <- dta_to_yaml_text(dta)
  if (!res$ok) {
    return(markdown_text) # Return unchanged if YAML extraction fails
  }
  yaml_text <- res$value
  yaml_block <- paste(
    "",
    "<!-- ========== EMBEDDED DTA YAML (Machine-Readable, Do Not Edit) ==========",
    yaml_text,
    "========== END EMBEDDED DTA YAML ========== -->",
    sep = "\n"
  )
  paste0(markdown_text, yaml_block)
}

# Format a single-line summary of datasets
format_datasets_summary <- function(dta) {
  dta_names <- dta_dataset_names(dta)
  if (length(dta_names) == 0) {
    return("No datasets")
  }
  types <- vapply(
    dta_names,
    function(nm) {
      ds <- dta_get_dataset(dta, nm)
      tryCatch(as.character(ds@type)[1], error = function(e) "unknown")
    },
    character(1)
  )
  type_summary <- paste(dta_names, types, sep = ": ", collapse = ", ")
  paste0(
    length(dta_names), " dataset", if (length(dta_names) > 1) "s" else "",
    " (", type_summary, ")"
  )
}

# Format detailed dataset information with specs and rules (markdown format).
# Uses the app's canonical list-extractors (dta_column_to_list / dta_rule_to_list)
# so it stays consistent with the editor/YAML view and never coerces raw S7
# objects to character (which throws "cannot coerce type 'object'").
format_datasets_detail <- function(dta) {
  dta_names <- dta_dataset_names(dta)
  if (length(dta_names) == 0) {
    return("")
  }

  lines <- character(0)

  for (ds_name in dta_names) {
    ds <- dta_get_dataset(dta, ds_name)
    ds_type <- tryCatch(as.character(ds@type)[1], error = function(e) "unknown")

    lines <- c(lines, "", paste0("## Dataset: ", ds_name, " (", ds_type, ")"))

    # File Handlers
    handlers <- tryCatch(dta_handlers(ds), error = function(e) list())
    if (length(handlers) > 0) {
      lines <- c(lines, "", "**File Handlers:**")
      for (h in handlers) {
        expected <- tryCatch(handler_expected(h), error = function(e) "unknown")
        hint <- tryCatch(handler_hint(h), error = function(e) "")
        count_lbl <- tryCatch(handler_count_label(h), error = function(e) "")
        kind <- tryCatch(if (handler_is_pattern(h)) "regex" else "exact", error = function(e) "")

        hint_text <- if (length(hint) > 0 && nzchar(hint)) paste0(" \u2014 ", hint) else ""
        count_text <- if (length(count_lbl) > 0 && nzchar(count_lbl)) paste0(" (", count_lbl, ")") else ""
        kind_text <- if (nzchar(kind)) paste0(" [", kind, "]") else ""
        lines <- c(lines, paste0("- ", expected, count_text, hint_text, kind_text))
      }
    }

    # Column Specs
    cols <- tryCatch(ds@specs@columns, error = function(e) NULL)
    if (!is.null(cols) && length(cols) > 0) {
      lines <- c(lines, "", paste0("**Columns (", length(cols), " total):**"))
      for (col in cols) {
        l <- tryCatch(dta_column_to_list(col), error = function(e) NULL)
        if (is.null(l)) next
        col_id <- l$id %||% ""
        col_type <- l$type %||% ""
        nullable_str <- if (isTRUE(l$nullable)) "nullable" else "not null"

        meta <- character(0)
        if (nzchar(col_type)) meta <- c(meta, col_type)
        meta <- c(meta, nullable_str)
        if (!is.null(l$length)) meta <- c(meta, paste0("length ", l$length))
        meta_str <- paste0(" [", paste(meta, collapse = ", "), "]")

        constraint <- ""
        if (!is.null(l$values) && length(l$values) > 0) {
          constraint <- paste0(" | values: ", paste(l$values, collapse = ", "))
        } else if (!is.null(l$pattern) && nzchar(l$pattern)) {
          constraint <- paste0(" | pattern: ", l$pattern)
        }

        desc_text <- if (!is.null(l$description) && nzchar(l$description)) {
          paste0(": ", l$description)
        } else {
          ""
        }
        lines <- c(lines, paste0("- **", col_id, "**", meta_str, desc_text, constraint))
      }
    }

    # Rules
    rules <- tryCatch(ds@specs@rules, error = function(e) NULL)
    if (!is.null(rules) && length(rules) > 0) {
      lines <- c(lines, "", paste0("**Rules (", length(rules), " total):**"))
      for (i in seq_along(rules)) {
        l <- tryCatch(dta_rule_to_list(rules[[i]]), error = function(e) NULL)
        rule_id <- (l$id %||% "")
        if (!nzchar(rule_id)) rule_id <- paste0("Rule_", i)
        detail <- .format_rule_detail(l)
        desc_text <- if (!is.null(l$description) && nzchar(l$description)) {
          paste0(" \u2014 ", l$description)
        } else {
          ""
        }
        lines <- c(lines, paste0("- **", rule_id, ":** ", detail, desc_text))
      }
    }
  }

  if (length(lines) > 0) {
    paste(lines, collapse = "\n")
  } else {
    ""
  }
}

# Human-readable one-line description of a rule (from dta_rule_to_list()).
# Mirrors the phrasing used by the in-app rules overview table.
.format_rule_detail <- function(l) {
  if (is.null(l)) {
    return("")
  }
  ty <- l$type %||% ""
  if (identical(ty, "col_condition")) {
    sprintf(
      "IF %s THEN %s",
      .dta_cond_to_text(l$condition),
      .dta_cond_to_text(l$then)
    )
  } else if (identical(ty, "col_range")) {
    sprintf(
      "%s in [%s, %s]",
      paste(l$columns, collapse = ", "),
      l$min %||% "",
      l$max %||% ""
    )
  } else if (identical(ty, "col_unique")) {
    sprintf("unique(%s)", paste(l$columns, collapse = ", "))
  } else if (nzchar(ty)) {
    ty
  } else {
    ""
  }
}

# Create the export modal dialog UI
export_modal_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h4("Export Document"),
        shiny::p("Choose the format and options for your DTA export.")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h5("Format", class = "text-muted"),
        shiny::radioButtons(
          ns("format"),
          NULL,
          choices = c(
            "Markdown" = "markdown",
            "Word Document" = "word"
          ),
          selected = "markdown",
          inline = FALSE
        )
      )
    ),
    # Markdown options
    shiny::uiOutput(ns("markdown_options_ui")),
    # Word options
    shiny::uiOutput(ns("word_options_ui")),
    # Filename preview
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h5("Output filename", class = "text-muted"),
        shiny::textOutput(ns("filename_preview")),
        shiny::br()
      )
    ),
    # Footer buttons
    shiny::fluidRow(
      shiny::column(
        12,
        style = "margin-top: 20px; text-align: right;",
        shiny::actionButton(ns("cancel"), "Cancel", class = "btn btn-outline-secondary"),
        shiny::actionButton(ns("export"), "Export", class = "btn btn-primary", style = "margin-left: 8px;")
      )
    )
  )
}
