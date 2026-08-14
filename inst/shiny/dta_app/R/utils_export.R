# =============================================================================
# Export modal dialog helpers for the DTAtools Shiny app
# =============================================================================

# Is a PDF engine available for pandoc PDF conversion? Markdown -> PDF export
# needs pandoc AND a PDF engine (e.g. pdflatex/xelatex, a TinyTeX install, or
# wkhtmltopdf); pandoc on its own is not sufficient and otherwise fails with a
# "pdflatex not found" / "PDF program not found" error.
has_pdf_engine <- function() {
  engines <- c(
    "pdflatex", "xelatex", "lualatex", "tectonic", "wkhtmltopdf", "context"
  )
  if (any(nzchar(Sys.which(engines)))) {
    return(TRUE)
  }
  isTRUE(tryCatch(
    requireNamespace("tinytex", quietly = TRUE) && tinytex::is_tinytex(),
    error = function(e) FALSE
  ))
}

# Locate a Chrome/Edge/Chromium executable usable for headless HTML -> PDF
# printing. Honours the DTATOOLS_CHROME / CHROMOTE_CHROME / PAGEDOWN_CHROME
# environment overrides, then falls back to PATH and the standard install
# locations. Returns the executable path, or "" when none is found.
find_chrome_binary <- function() {
  for (env_var in c("DTATOOLS_CHROME", "CHROMOTE_CHROME", "PAGEDOWN_CHROME")) {
    p <- Sys.getenv(env_var)
    if (nzchar(p) && file.exists(p)) {
      return(normalizePath(p))
    }
  }
  pf <- Sys.getenv("ProgramFiles")
  pfx <- Sys.getenv("ProgramFiles(x86)")
  candidates <- c(
    Sys.which("chrome"),
    Sys.which("google-chrome"),
    Sys.which("chromium"),
    Sys.which("msedge"),
    file.path(pf, "Google/Chrome/Application/chrome.exe"),
    file.path(pfx, "Google/Chrome/Application/chrome.exe"),
    file.path(pf, "Microsoft/Edge/Application/msedge.exe"),
    file.path(pfx, "Microsoft/Edge/Application/msedge.exe"),
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge"
  )
  candidates <- candidates[nzchar(candidates)]
  hit <- candidates[file.exists(candidates)]
  if (length(hit) > 0) normalizePath(hit[[1]]) else ""
}

# Convert a Markdown file to PDF WITHOUT LaTeX: render it to standalone HTML with
# pandoc (injecting a small print stylesheet for readable tables/margins), then
# print that HTML to PDF with headless Chrome/Edge. Returns the PDF path on
# success and stops with an informative error on failure. `chrome` is the
# browser executable (see find_chrome_binary()).
markdown_to_pdf_via_chrome <- function(md_file, pdf_file,
                                       chrome = find_chrome_binary()) {
  if (!nzchar(chrome)) {
    stop("No Chrome or Edge browser was found for PDF printing.")
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE) ||
    !rmarkdown::pandoc_available()) {
    stop("pandoc is not available to render Markdown to HTML.")
  }

  html_file <- sub("\\.[^.]+$", ".html", pdf_file)
  header_file <- tempfile(fileext = ".html")
  writeLines(c(
    "<style>",
    "@page { margin: 1.6cm; }",
    "body { font-family: 'Segoe UI', Arial, sans-serif; font-size: 11pt;",
    "       color: #222; margin: 0; }",
    "h1, h2, h3 { color: #08312a; }",
    "table { border-collapse: collapse; width: 100%; margin: 0.5em 0; }",
    "th, td { border: 1px solid #999; padding: 4px 8px; text-align: left;",
    "         font-size: 10pt; vertical-align: top; }",
    "th { background: #f0f0f0; }",
    "code { background: #f5f5f5; padding: 1px 3px; }",
    "</style>"
  ), header_file)
  on.exit(unlink(c(html_file, header_file), force = TRUE), add = TRUE)

  rmarkdown::pandoc_convert(
    input = normalizePath(md_file),
    to = "html5",
    output = html_file,
    options = c("--standalone", "--include-in-header", header_file)
  )

  udd <- file.path(
    tempdir(),
    paste0("dta_chrome_", as.integer(Sys.time()), "_", Sys.getpid())
  )
  dir.create(udd, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(udd, recursive = TRUE, force = TRUE), add = TRUE)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(html_file)))

  print_pdf <- function(headless_flag) {
    args <- c(
      headless_flag, "--disable-gpu", "--no-first-run",
      "--no-default-browser-check", "--disable-extensions",
      sprintf("--user-data-dir=%s", udd),
      "--no-pdf-header-footer",
      sprintf("--print-to-pdf=%s", pdf_file),
      url
    )
    suppressWarnings(system2(
      chrome,
      args = shQuote(args),
      stdout = FALSE, stderr = FALSE, timeout = 120
    ))
    file.exists(pdf_file) && file.info(pdf_file)$size > 0
  }

  # Newer browsers use "--headless=new"; fall back to the classic flag.
  ok <- tryCatch(print_pdf("--headless=new"), error = function(e) FALSE)
  if (!isTRUE(ok)) {
    ok <- tryCatch(print_pdf("--headless"), error = function(e) FALSE)
  }
  if (!isTRUE(ok) || !file.exists(pdf_file)) {
    stop("The headless browser did not produce a PDF file.")
  }
  pdf_file
}

# List available custom templates from the templates directory
list_available_templates <- function() {
  templates_dir <- system.file("extdata", "templates", package = "DTAtools")
  if (!dir.exists(templates_dir)) {
    return(character(0))
  }
  files <- list.files(templates_dir, pattern = "\\.docx$", full.names = FALSE)
  sort(files)
}

# Get full path to a template by name.
# `template_name` arrives from a selectInput, but a Shiny client is not bound by
# the offered choices and can send any string over the websocket, so the name is
# checked for membership in the bundled set rather than pasted into a path. The
# match is exact, not basename()-normalised: the client must echo one of the
# offered names verbatim, so any string carrying a path separator fails outright
# instead of being silently repaired into a hit. Without this, a traversal would
# resolve to an arbitrary server-side file that export_with_template() then
# renders and hands back as a download.
get_template_path <- function(template_name) {
  if (is.null(template_name) || length(template_name) != 1L ||
    is.na(template_name) || !nzchar(template_name)) {
    return(NULL)
  }
  if (!(template_name %in% list_available_templates())) {
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
  } else if (identical(ty, "group_condition")) {
    sprintf(
      "group(%s): %s condition(s), %s constraint(s)",
      paste(l$group_by %||% character(0), collapse = ", "),
      length(l$conditions %||% list()),
      length(l$constraints %||% list())
    )
  } else if (nzchar(ty)) {
    ty
  } else {
    ""
  }
}
