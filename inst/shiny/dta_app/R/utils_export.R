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

# The canonical placeholder-token grammar for template files. Mirrors
# .tv_token_pattern() in R/exportTemplateDocx.R, extended with the optional
# `:DATASET` argument a placeholder may carry (e.g. `{COLUMN_SPECS:ADSL}`).
.template_placeholder_pattern <- "\\{[A-Za-z_][A-Za-z0-9_]*(?::[^{}]+)?\\}"

# Does this .docx contain at least one {PLACEHOLDER} marker?
#
# The templates directory also holds dta_numbered_template.docx, which is not a
# fill-in template at all: it is the reference document the *built-in* layout
# opens for its numbered heading styles, and it contains no placeholders. Offered
# in the export dialog it produces a Word file with none of the user's DTA in it,
# silently. Rather than blocklisting that one name -- which would let the next
# styles-only reference document reintroduce the same trap -- a candidate has to
# prove it has something to fill.
#
# The scan reads paragraph *text*, not the raw XML: Word freely splits a typed
# placeholder across runs, so a raw grep on document.xml misses tokens that the
# exporter itself has no trouble with.
template_has_placeholders <- function(path) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(path) ||
    !file.exists(path)) {
    return(FALSE)
  }

  temp_dir <- tempfile("dta_template_scan_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  extracted <- tryCatch(
    utils::unzip(path, exdir = temp_dir),
    warning = function(w) character(0),
    error = function(e) character(0)
  )
  if (length(extracted) == 0) {
    return(FALSE)
  }

  word_dir <- file.path(temp_dir, "word")
  main <- file.path(word_dir, "document.xml")
  parts <- c(
    if (file.exists(main)) main,
    list.files(
      word_dir,
      pattern = "^(header|footer)[0-9]*\\.xml$",
      full.names = TRUE
    )
  )

  texts <- unlist(lapply(parts, function(part) {
    doc <- tryCatch(xml2::read_xml(part), error = function(e) NULL)
    if (is.null(doc)) {
      return(character(0))
    }
    xml2::xml_text(xml2::xml_find_all(doc, ".//*[local-name()='p']"))
  }))

  any(grepl(.template_placeholder_pattern, texts, perl = TRUE))
}

# List available custom templates from the templates directory
list_available_templates <- function() {
  templates_dir <- system.file("extdata", "templates", package = "DTAtools")
  if (!dir.exists(templates_dir)) {
    return(character(0))
  }
  files <- list.files(templates_dir, pattern = "\\.docx$", full.names = FALSE)
  keep <- vapply(
    files,
    function(f) isTRUE(template_has_placeholders(file.path(templates_dir, f))),
    logical(1)
  )
  sort(files[keep], method = "radix")
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
