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
    cli::cli_abort("No Chrome or Edge browser was found for PDF printing.")
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE) ||
    !rmarkdown::pandoc_available()) {
    cli::cli_abort("pandoc is not available to render Markdown to HTML.")
  }

  html_file <- sub("\\.[^.]+$", ".html", pdf_file)
  header_file <- tempfile(fileext = ".html")
  writeLines(c(
    # Defence in depth behind the pandoc reader below: the page may run no
    # script and load no plugin, whatever reaches it. A meta CSP is what
    # actually does this -- tested 2026-11-04 with headless Edge:
    # `--disable-javascript` is not a Chromium switch (scripts still ran), and
    # `--blink-settings=scriptEnabled=false` blocks scripts but also stops
    # --print-to-pdf from writing anything.
    # default-src 'none' also stops every fetch the page could make -- an
    # image, frame or stylesheet URL planted in document text would otherwise
    # be requested by the server (SSRF) or read off its disk via file:///.
    # Only the inline <style> below and pandoc's own inline styles may apply.
    "<meta http-equiv=\"Content-Security-Policy\" content=\"default-src 'none'; img-src data:; style-src 'unsafe-inline'\">",
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

  # md_file is markdown built from document text a third party controls
  # (metadata title, dataset description, error_handling, contact names,
  # ...), so the reader is pinned to a dialect without raw passthrough --
  # left at the default "markdown" reader, a `\input{}` planted in any of
  # those fields reads a server-side file into the PDF, and a `<script>`
  # in one runs in the headless browser this HTML is about to be printed by.
  # Images are dropped as well (pandoc_no_images_filter()): raw passthrough
  # is off, but markdown image syntax is not raw and still becomes an <img>.
  no_images <- pandoc_no_images_filter()
  on.exit(unlink(no_images), add = TRUE)
  rmarkdown::pandoc_convert(
    input = normalizePath(md_file),
    from = "markdown-raw_tex-raw_html-raw_attribute",
    to = "html5",
    output = html_file,
    options = c(
      "--standalone", "--include-in-header", header_file,
      "--lua-filter", no_images
    )
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
    cli::cli_abort("The headless browser did not produce a PDF file.")
  }
  pdf_file
}

# Path of a fresh pandoc Lua filter that replaces every image with its alt
# text; the caller unlinks it. No exported document carries an image of its
# own, so any image in the markdown came from document text a third party
# controls -- and pandoc's PDF route downloads a remote image server-side
# (SSRF) while LaTeX embeds any local image path it is given.
pandoc_no_images_filter <- function() {
  path <- tempfile("dta-no-images-", fileext = ".lua")
  writeLines("function Image(el) return el.caption end", path)
  path
}

# The placeholder-token grammar for template files, a sibling of
# .tv_token_pattern() in R/exportTemplateDocx.R. It differs in two ways: it
# does not accept the `#`/`/` region-marker prefix (`{#DATASETS}`), and its
# optional `:DATASET` argument (e.g. `{COLUMN_SPECS:ADSL}`) must be non-empty.
.template_placeholder_pattern <- "\\{[A-Za-z_][A-Za-z0-9_]*(?::[^{}]+)?\\}"

# Does this .docx contain at least one {PLACEHOLDER} marker?
#
# The word-templates directory also holds dta_numbered_template.docx, which is not a
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

# Word templates the Export dialog may offer: a named character vector whose
# values are what the dialog sends back and whose names are the labels shown.
#   * bundled templates (inst/extdata/word-templates, only those with placeholders),
#     value = file name -- unless the library config switches them off;
#   * the library's own .docx files from dta-app.yaml, value = "library:<file
#     name>" (the prefix keeps the two namespaces apart), label from the config.
# `cfg` is the normalised app config (R/app_config.R); the default reproduces
# the stock app.
list_available_templates <- function(cfg = dta_app_config_defaults()) {
  out <- character(0)
  templates_dir <- system.file("extdata", "word-templates", package = "DTAtools")
  if (isTRUE(cfg$exports$bundled_word_templates) && dir.exists(templates_dir)) {
    files <- list.files(templates_dir, pattern = "\\.docx$", full.names = FALSE)
    keep <- vapply(
      files,
      function(f) isTRUE(template_has_placeholders(file.path(templates_dir, f))),
      logical(1)
    )
    files <- sort(files[keep], method = "radix")
    out <- stats::setNames(files, files)
  }
  lib <- cfg$exports$word_templates
  if (!is.null(lib) && nrow(lib) > 0) {
    values <- paste0("library:", basename(lib$path))
    first <- !duplicated(values) # two files of one name: the first wins
    if (!all(first)) {
      dup <- unique(basename(lib$path)[!first])
      cli::cli_warn(c(
        "Word templates with the same file name in the library: only the first is offered.",
        "i" = "Duplicate file name{?s}: {.file {dup}}."
      ))
    }
    out <- c(out, stats::setNames(values[first], lib$label[first]))
  }
  out
}

# Get full path to a template by name.
# `template_name` arrives from a selectInput, but a Shiny client is not bound by
# the offered choices and can send any string over the websocket, so the name is
# checked for membership in the offered set rather than pasted into a path. The
# match is exact, not basename()-normalised: the client must echo one of the
# offered values verbatim, so any string carrying a path separator fails outright
# instead of being silently repaired into a hit. Without this, a traversal would
# resolve to an arbitrary server-side file that export_with_template() then
# renders and hands back as a download. Library templates resolve to the path
# the config parser already confined to the library folder.
get_template_path <- function(template_name, cfg = dta_app_config_defaults()) {
  if (is.null(template_name) || length(template_name) != 1L ||
    is.na(template_name) || !nzchar(template_name)) {
    return(NULL)
  }
  if (!(template_name %in% unname(list_available_templates(cfg)))) {
    return(NULL)
  }
  if (startsWith(template_name, "library:")) {
    lib <- cfg$exports$word_templates
    hit <- which(paste0("library:", basename(lib$path)) == template_name)
    full_path <- lib$path[hit[[1]]]
  } else {
    templates_dir <- system.file("extdata", "word-templates", package = "DTAtools")
    full_path <- file.path(templates_dir, template_name)
  }
  if (file.exists(full_path)) full_path else NULL
}

# Embed YAML block at the end of markdown text, wrapped in HTML comment
embed_yaml_markdown <- function(markdown_text, dta) {
  res <- dta_to_yaml_text(dta)
  if (!res$ok) {
    return(markdown_text) # Return unchanged if YAML extraction fails
  }
  # An HTML comment ends at the first "-->" (or "--!>"); a value containing
  # one would close it early and expose everything after it as live markdown
  # and HTML to whatever renders the file. Only that sequence is altered.
  yaml_text <- gsub("--(!?)>", "--\\1&gt;", res$value)
  yaml_block <- paste(
    "",
    "<!-- ========== EMBEDDED DTA YAML (Machine-Readable, Do Not Edit) ==========",
    yaml_text,
    "========== END EMBEDDED DTA YAML ========== -->",
    sep = "\n"
  )
  paste0(markdown_text, yaml_block)
}

# Neutralise spreadsheet formulas in a data frame bound for CSV/TSV: a text
# cell starting with = + - @ (or a tab / carriage return) is executed as a
# formula when the file is opened in Excel, and these cells carry column names
# and values straight out of an uploaded data file. A leading apostrophe makes
# the spreadsheet show the text as text. XLSX is written as typed strings and
# needs none of this.
csv_formula_safe <- function(df) {
  for (nm in names(df)) {
    x <- df[[nm]]
    if (is.factor(x)) x <- as.character(x)
    if (!is.character(x)) next
    hit <- !is.na(x) & grepl("^[-=+@\t\r]", x)
    x[hit] <- paste0("'", x[hit])
    df[[nm]] <- x
  }
  df
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

# What the Export dialog may offer under an app config. `exports.document`
# lists the allowed kinds: markdown, markdown_pdf (Markdown rendered to PDF),
# word (the built-in layout) and word_template (a template picked from the
# list). A word_template with no template to pick counts as not offered.
# Returns the dialog's choices plus `allowed()`, the same rule applied to the
# values export_do receives, so the dialog and the server cannot disagree.
export_document_options <- function(cfg = dta_app_config_defaults()) {
  doc <- cfg$exports$document
  templates <- list_available_templates(cfg)
  md_plain <- "markdown" %in% doc
  md_pdf <- "markdown_pdf" %in% doc
  word_builtin <- "word" %in% doc
  word_tmpl <- "word_template" %in% doc && length(templates) > 0
  has_md <- md_plain || md_pdf
  has_word <- word_builtin || word_tmpl

  fmt_choices <- c(
    if (has_md) c("Markdown" = "markdown"),
    if (has_word) c("Word Document" = "word")
  )
  mode_choices <- c(
    if (word_builtin) c("Use built-in template" = "builtin"),
    if (word_tmpl) c("Use custom template" = "custom")
  )

  dd <- cfg$exports$default_document
  default_format <- if (isTRUE(dd %in% c("markdown", "markdown_pdf")) && has_md) {
    "markdown"
  } else if (isTRUE(dd %in% c("word", "word_template")) && has_word) {
    "word"
  } else if (length(fmt_choices) > 0) {
    unname(fmt_choices[[1]])
  } else {
    NA_character_
  }
  list(
    any = length(fmt_choices) > 0,
    format_choices = fmt_choices,
    default_format = default_format,
    # The PDF checkbox is shown only if both Markdown kinds are allowed; with
    # markdown_pdf alone the PDF is forced, with markdown alone it is off.
    pdf_choice = md_plain && md_pdf,
    pdf_forced = md_pdf && !md_plain,
    default_pdf = identical(dd, "markdown_pdf") || (md_pdf && !md_plain),
    mode_choices = mode_choices,
    default_mode = if (identical(dd, "word_template") && word_tmpl) {
      "custom"
    } else if (word_builtin) {
      "builtin"
    } else {
      "custom"
    },
    templates = templates,
    # Is this combination of the dialog's inputs one the config allows?
    allowed = function(format, as_pdf = FALSE, word_mode = "builtin") {
      if (identical(format, "markdown")) {
        if (isTRUE(as_pdf)) md_pdf else md_plain
      } else if (identical(format, "word")) {
        if (identical(word_mode, "custom")) word_tmpl else word_builtin
      } else {
        FALSE
      }
    }
  )
}
