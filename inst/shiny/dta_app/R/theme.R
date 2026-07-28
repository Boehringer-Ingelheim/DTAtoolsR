# -----------------------------------------------------------------------------
# Theme: modern Boehringer Ingelheim brand palette
# -----------------------------------------------------------------------------
# NOTE: Replace the hex values below with the official Boehringer Ingelheim
# brand palette where available. The values here are a BI-style green/teal
# family chosen to look on-brand and modern.

BI <- list(
  green       = "#00625B", # primary brand green/teal (deep)
  green_dark  = "#003D38",
  green_light = "#E6F1EF",
  accent      = "#00A886", # brighter accent green
  ink         = "#1A2B2A", # near-black text
  grey        = "#5B6B6A",
  grey_light  = "#F4F7F6",
  # Semantic status colors (harmonized with the BI green family)
  pass        = "#1E8E5A",
  pass_bg     = "#E5F4EC",
  pass_border = "#9BD3B4",
  fail        = "#C0392B",
  fail_bg     = "#FBEAE7",
  fail_border = "#E9B4AC",
  pending     = "#5B6B6A",
  pending_bg  = "#EEF1F1",
  pending_border = "#CDD5D4"
)

# bslib theme --------------------------------------------------------------
bi_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = BI$ink,
    primary = BI$green,
    secondary = BI$grey,
    success = BI$pass,
    danger = BI$fail,
    base_font = bslib::font_collection(
      bslib::font_google("Inter", local = FALSE),
      "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
    ),
    heading_font = bslib::font_collection(
      bslib::font_google("Inter", local = FALSE),
      "system-ui", "sans-serif"
    ),
    "border-radius" = "0.6rem",
    "card-border-radius" = "0.8rem"
  )
}

# Extra CSS ----------------------------------------------------------------
bi_css <- function() {
  root <- sprintf(
    ":root {
      --bi-green: %s; --bi-green-dark: %s; --bi-green-light: %s;
      --bi-accent: %s; --bi-ink: %s; --bi-grey: %s; --bi-grey-light: %s;
      --bi-pass: %s; --bi-pass-bg: %s; --bi-pass-border: %s;
      --bi-fail: %s; --bi-fail-bg: %s; --bi-fail-border: %s;
      --bi-pending: %s; --bi-pending-bg: %s; --bi-pending-border: %s;
    }",
    BI$green, BI$green_dark, BI$green_light,
    BI$accent, BI$ink, BI$grey, BI$grey_light,
    BI$pass, BI$pass_bg, BI$pass_border,
    BI$fail, BI$fail_bg, BI$fail_border,
    BI$pending, BI$pending_bg, BI$pending_border
  )
  shiny::HTML(paste0(root, "
    body { background: var(--bi-grey-light); }
    .app-brandbar {
      background: linear-gradient(90deg, var(--bi-green-dark), var(--bi-green));
      color: #fff; padding: 14px 20px; display: flex; align-items: center;
      gap: 14px; box-shadow: 0 2px 10px rgba(0,0,0,.08);
    }
    .app-brandbar .brand-logo { height: 34px; width: auto; display: block; flex: none; }
    .app-brandbar .brand-title { font-weight: 700; font-size: 1.15rem; letter-spacing: .2px; }
    .app-brandbar .brand-sub { opacity: .85; font-size: .85rem; }
    .app-actions { margin-left: auto; display: flex; gap: 8px; }

    /* Status chips */
    .status-chip {
      display: inline-flex; align-items: center; gap: 6px;
      padding: 3px 10px; border-radius: 999px; font-size: .78rem;
      font-weight: 600; border: 1px solid transparent; white-space: nowrap;
    }
    .status-pass    { color: var(--bi-pass); background: var(--bi-pass-bg); border-color: var(--bi-pass-border); }
    .status-fail    { color: var(--bi-fail); background: var(--bi-fail-bg); border-color: var(--bi-fail-border); }
    .status-pending { color: var(--bi-pending); background: var(--bi-pending-bg); border-color: var(--bi-pending-border); }
    .status-nodata  { color: #8A6D3B; background: #FCF4E6; border-color: #EBD9B6; }
    .status-dot { width: 8px; height: 8px; border-radius: 50%; background: currentColor; }

    /* Dataset tiles get a colored left edge by status */
    .tile-pass    { border-left: 5px solid var(--bi-pass) !important; }
    .tile-fail    { border-left: 5px solid var(--bi-fail) !important; }
    .tile-pending { border-left: 5px solid var(--bi-pending-border) !important; }
    .tile-nodata  { border-left: 5px solid #EBD9B6 !important; }

    /* Upload slot */
    .slot-card { background: #fff; }
    .slot-meta { font-size: .82rem; color: var(--bi-grey); }
    .slot-expected { font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
      background: var(--bi-green-light); color: var(--bi-green-dark);
      padding: 1px 6px; border-radius: 6px; }
    .slot-ok    { color: var(--bi-pass); font-weight: 600; }
    .slot-warn  { color: #B26A00; font-weight: 600; }

    /* Make Shiny fileInput look like a drop zone */
    .dropzone .form-group { margin-bottom: 0; }
    .dropzone .input-group, .dropzone .custom-file, .dropzone input[type=file] { width: 100%; }
    .dropzone .btn-file, .dropzone .form-control {
      border-style: dashed !important; border-width: 2px !important;
      border-color: var(--bi-pass-border) !important;
    }
    /* Suppress the native fileInput progress / 'Upload complete' bar: a finished
       byte transfer is NOT acceptance. Acceptance is shown only by the app's own
       per-slot state after matches_filename() + load_file() succeed and verify. */
    .dropzone .progress, .dropzone .shiny-file-input-progress { display: none !important; }

    .msg-hint { font-size: .82rem; color: var(--bi-grey); }
    .metric { font-size: 1.4rem; font-weight: 700; color: var(--bi-green-dark); }

    /* Sidebar workspace header -- DTA identity (title / version / date) */
    .workspace-header { margin-bottom: 12px; }
    .workspace-header .ws-title {
      font-weight: 700; font-size: 1.02rem; color: var(--bi-green-dark);
      line-height: 1.25; word-break: break-word;
    }
    .workspace-header .ws-meta {
      font-size: .8rem; color: var(--bi-grey); margin-top: 4px;
      display: flex; gap: 6px; flex-wrap: wrap;
    }
    .workspace-header .ws-pill {
      background: var(--bi-green-light); color: var(--bi-green-dark);
      border-radius: 999px; padding: 1px 9px; font-weight: 600;
    }
    .section-label {
      font-size: .72rem; font-weight: 700; letter-spacing: .04em;
      text-transform: uppercase; color: var(--bi-grey); margin: 2px 0 6px;
    }

    /* Raw YAML syntax-highlighted view (dark editor theme) */
    .yaml-view {
      margin: 0; background: #0d1117; color: #c9d1d9; padding: 14px 16px;
      border-radius: 8px; max-height: 70vh; overflow: auto;
      font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
      font-size: .84rem; line-height: 1.55; white-space: pre; tab-size: 2;
    }
    .yaml-view code { font: inherit; color: inherit; background: none; padding: 0; }
    .yaml-view .yml-key     { color: #7ee787; }
    .yaml-view .yml-str     { color: #a5d6ff; }
    .yaml-view .yml-num     { color: #f2cc60; }
    .yaml-view .yml-bool    { color: #ff7b72; }
    .yaml-view .yml-comment { color: #8b949e; font-style: italic; }
    .yaml-view .yml-punct   { color: #c9d1d9; }
    .yaml-view .yml-dash    { color: #ff7b72; }

    /* Editable raw YAML: dark editor textarea + Apply/Revert bar + result banner */
    .yaml-edit-bar { display: flex; justify-content: space-between; align-items: flex-start; gap: 12px; margin-bottom: 8px; }
    .yaml-edit-bar .msg-hint { flex: 1 1 auto; }
    .yaml-edit-actions { flex: none; display: flex; gap: 6px; }
    textarea#raw_yaml_editor {
      background: #0d1117; color: #c9d1d9; border: 1px solid #30363d; border-radius: 8px;
      font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
      font-size: .84rem; line-height: 1.55; tab-size: 2; white-space: pre;
      min-height: 55vh; overflow: auto;
    }
    textarea#raw_yaml_editor:focus { border-color: var(--bi-accent); box-shadow: 0 0 0 2px rgba(0,168,134,.25); }
    .yaml-valid { font-size: .82rem; padding: 7px 10px; border-radius: 8px; margin-bottom: 8px; border: 1px solid transparent; }
    .yaml-valid.ok  { color: var(--bi-pass); background: var(--bi-pass-bg); border-color: var(--bi-pass-border); }
    .yaml-valid.err { color: var(--bi-fail); background: var(--bi-fail-bg); border-color: var(--bi-fail-border); white-space: pre-wrap; }

    /* Loaded-files list: one row per bound file (name + table + status + trash) */
    .loaded-slot { margin-bottom: 12px; }
    .loaded-slot-head { margin-bottom: 5px; }
    .loaded-file-row {
      display: flex; align-items: center; gap: 10px;
      padding: 6px 10px; margin-bottom: 6px;
      border: 1px solid var(--bi-pending-border); border-radius: 8px; background: #fff;
    }
    .loaded-file-row .file-name { font-weight: 600; color: var(--bi-ink); word-break: break-all; }
    .loaded-file-row .file-table {
      font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-size: .8rem;
      color: var(--bi-green-dark); background: var(--bi-green-light);
      padding: 1px 7px; border-radius: 6px; white-space: nowrap;
    }
    .loaded-file-row .file-status { font-weight: 700; width: 1.2em; text-align: center; flex: none; }
    .loaded-file-row .file-ok      { color: var(--bi-pass); }
    .loaded-file-row .file-fail    { color: var(--bi-fail); }
    .loaded-file-row .file-pending { color: var(--bi-grey); }
    .loaded-file-row .file-remove {
      margin-left: auto; flex: none; color: var(--bi-fail);
      border: none; background: transparent; padding: 2px 7px; line-height: 1; font-size: 1rem;
    }
    .loaded-file-row .file-remove:hover { background: var(--bi-fail-bg); border-radius: 6px; }

    /* Validation messages: compact table + download buttons + top filters */
    .msgs-dl .btn { margin-left: 6px; }
    .msgs-table table.dataTable { font-size: .8rem; }
    .msgs-table table.dataTable td, .msgs-table table.dataTable th { padding: 5px 8px; }
    .msgs-table .dataTables_wrapper { font-size: .82rem; }
    .msgs-table .dataTables_filter input, .msgs-table .dataTables_length select { font-size: .82rem; }
    .msgs-table table.dataTable thead .form-control,
    .msgs-table table.dataTable thead .form-select,
    .msgs-table table.dataTable thead input,
    .msgs-table table.dataTable thead select { font-size: .78rem; padding: 2px 6px; }

    /* Sidebar dataset navigation: status-tinted rows + name (select) + check icon.
       The row BACKGROUND encodes status: not-checked (neutral grey), passed
       (green), failed (red), missing/no-data (orange). Selection is a brand-teal
       ring so it never collides with the status color. */
    .dataset-nav-list { display: flex; flex-direction: column; gap: 6px; margin-bottom: 4px; }
    .dataset-nav-row {
      display: flex; align-items: center; gap: 8px; padding: 7px 10px;
      border: 1px solid var(--bi-pending-border); border-left: 5px solid var(--bi-pending-border);
      border-radius: 8px; background: #fff;
      transition: background .12s ease, box-shadow .12s ease, border-color .12s ease;
    }
    .dataset-nav-row.nav-st-pending { background: var(--bi-pending-bg); border-color: var(--bi-pending-border); border-left-color: var(--bi-pending-border); }
    .dataset-nav-row.nav-st-pass    { background: var(--bi-pass-bg);    border-color: var(--bi-pass-border);    border-left-color: var(--bi-pass); }
    .dataset-nav-row.nav-st-fail    { background: var(--bi-fail-bg);    border-color: var(--bi-fail-border);    border-left-color: var(--bi-fail); }
    .dataset-nav-row.nav-st-nodata  { background: #FCF4E6;              border-color: #EBD9B6;                  border-left-color: #C77700; }
    .dataset-nav-row .nav-select {
      flex: 1 1 auto; display: flex; align-items: center; gap: 8px;
      text-decoration: none; color: var(--bi-ink); font-weight: 600; overflow: hidden;
    }
    .dataset-nav-row .nav-select:hover { text-decoration: none; }
    .dataset-nav-row .nav-name { word-break: break-word; color: var(--bi-ink); }
    .dataset-nav-row.active { box-shadow: 0 0 0 2px var(--bi-green); }
    .dataset-nav-row.active .nav-name { color: var(--bi-green-dark); font-weight: 700; }
    .nav-ic { flex: none; width: 1.15em; text-align: center; font-weight: 700; font-size: .95rem; }
    .nav-ic-pass    { color: var(--bi-pass); }
    .nav-ic-fail    { color: var(--bi-fail); }
    .nav-ic-nodata  { color: #C77700; }
    .nav-ic-pending { color: var(--bi-pending); }
    .dataset-nav-row .nav-check {
      flex: none; color: var(--bi-pass); border: 1px solid var(--bi-pass-border);
      background: #fff; padding: 1px 8px; line-height: 1.3; border-radius: 6px; font-size: .85rem;
    }
    .dataset-nav-row .nav-check:hover { background: var(--bi-pass-bg); }
    "))
}

# Status chip HTML ---------------------------------------------------------
status_chip <- function(status) {
  status <- match.arg(status, c("pass", "fail", "pending", "nodata"))
  label <- switch(status,
    pass = "Passed",
    fail = "Failed",
    pending = "Not validated",
    nodata = "No data"
  )
  cls <- paste0("status-chip status-", status)
  shiny::span(
    class = cls,
    shiny::span(class = "status-dot"),
    label
  )
}

# Accessible icon + label for a slot's state (not color-only)
slot_state_label <- function(state, detail = NULL) {
  icon <- switch(state,
    ok = "\u2714",      # check mark
    warn = "\u26A0",    # warning
    empty = "\u2014"    # em dash
  )
  cls <- switch(state, ok = "slot-ok", warn = "slot-warn", empty = "slot-meta")
  shiny::span(class = cls, paste0(icon, " ", detail %||% ""))
}

# Lightweight, dependency-free YAML syntax highlighter -> HTML. HTML-escapes
# first, then wraps tokens (keys, strings, numbers, booleans, comments, list
# dashes) in <span class="yml-*"> for the .yaml-view theme. Purely presentational;
# the DTA object -- not this HTML -- remains the source of truth.
yaml_highlight_html <- function(text) {
  esc <- function(s) {
    s <- gsub("&", "&amp;", s, fixed = TRUE)
    s <- gsub("<", "&lt;", s, fixed = TRUE)
    gsub(">", "&gt;", s, fixed = TRUE)
  }
  hl_value <- function(v) {
    lead <- sub("^([[:space:]]*).*$", "\\1", v)
    tv <- substring(v, nchar(lead) + 1L)
    if (!nzchar(tv)) return(v)
    cls <- if (grepl('^".*"$', tv) || grepl("^'.*'$", tv)) {
      "yml-str"
    } else if (grepl("^(true|false|yes|no|on|off|null|~)$", tv, ignore.case = TRUE)) {
      "yml-bool"
    } else if (grepl("^-?[0-9]+(\\.[0-9]+)?$", tv)) {
      "yml-num"
    } else {
      "yml-str"
    }
    paste0(lead, sprintf('<span class="%s">%s</span>', cls, tv))
  }
  lines <- strsplit(text %||% "", "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0) return("")
  out <- vapply(lines, function(line) {
    if (!nzchar(line)) return("")
    e <- esc(line)
    # Whole-line comment.
    if (grepl("^[[:space:]]*#", e)) {
      return(sprintf('<span class="yml-comment">%s</span>', e))
    }
    # Split a trailing inline comment (needs whitespace before '#').
    comment <- ""
    m <- regexpr("[[:space:]]+#.*$", e)
    if (m > 0) {
      comment <- sprintf('<span class="yml-comment">%s</span>', substring(e, m))
      e <- substring(e, 1L, m - 1L)
    }
    # Peel leading indent (+ optional list dashes) so keys stay clean.
    pre <- ""
    dm <- regexpr("^[[:space:]]*(- )*", e)
    if (dm > 0 && attr(dm, "match.length") > 0) {
      lead <- regmatches(e, dm)
      pre <- gsub("- ", '<span class="yml-dash">- </span>', lead, fixed = TRUE)
      e <- substring(e, attr(dm, "match.length") + 1L)
    }
    # key: value
    km <- regexpr("^([^:]+):([[:space:]]|$)", e)
    if (km > 0) {
      key <- sub("^([^:]+):.*$", "\\1", e)
      after <- substring(e, nchar(key) + 2L)
      body <- sprintf(
        '<span class="yml-key">%s</span><span class="yml-punct">:</span>%s',
        key, hl_value(after)
      )
      return(paste0(pre, body, comment))
    }
    paste0(pre, hl_value(e), comment)
  }, character(1), USE.NAMES = FALSE)
  paste(out, collapse = "\n")
}
