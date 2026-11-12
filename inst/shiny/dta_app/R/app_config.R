# -----------------------------------------------------------------------------
# Per-deployment app configuration: `dta-app.yaml` in a template library.
#
# A template library is already the per-deployment unit (it is what
# DTATOOLS_TEMPLATE_SOURCES points at), so it also carries that deployment's
# look and feature set: owner, title, logo, a header tag, colours, links, which
# start buttons / tabs / exports are offered, and its own Word templates. With no
# file the defaults reproduce the stock app -- except that the organisation is
# deliberately empty, so no organisation name appears unless a library sets one.
#
# TRUST BOUNDARY. Library content is written by someone other than the person
# running the app (a git repo, a companion package), yet it reaches two
# dangerous places: HTML attributes (links, logo) and a CSS sprintf() (colours).
# So every value is validated HERE, once, and a bad value falls back to that
# key's default rather than being passed on:
#   * colours, including header_tag.color (which goes into a style attribute),
#     must be #RGB / #RRGGBB -- the guard against CSS injection;
#   * links must be http(s)/mailto and free of whitespace, quotes and <>;
#   * the logo and Word templates must be relative paths that resolve (after
#     normalizePath, so symlinks count) inside the config's own directory --
#     the library root, where dta-app.yaml lives;
#   * the logo is embedded as a base64 data: URI, so the library folder is
#     never registered with addResourcePath() and never served over HTTP.
# The app must never fail to start over this file: parse_dta_app_config() does
# not throw for bad content, it reports `problems` and keeps the defaults.
#
# The validator (validateTemplate.R) reuses parse_dta_app_config() so that
# "what the check says" and "what the app does" cannot drift apart.
#
# dta_template_index_invalidate() (template_index.R) is expected to call
# dta_app_config_invalidate(), so "Refresh templates" refreshes this too.
#
# `%||%` is already bound in the app helper environment (see utils_dta.R).
# -----------------------------------------------------------------------------

dta_app_config_filenames <- function() c("dta-app.yaml", "dta-app.yml")

# Today's literal values, moved out of app.R / theme.R.
dta_app_config_defaults <- function() {
  list(
    source = NA_character_,
    organisation = list(name = "", unit = ""),
    # What the library calls its documents. `label` / `abbr` are derived (names
    # joined with " / ", abbreviations with "/"); `custom` is TRUE once the
    # file sets either key, so wording that is deliberately broader by default
    # (the landing drop zone accepts both DTA and DTS files) can narrow.
    document = list(
      names = "Data Transfer Agreement", abbreviations = "DTA",
      label = "Data Transfer Agreement", abbr = "DTA", custom = FALSE
    ),
    branding = list(
      app_title = "DTAtools",
      app_subtitle = "Validation and authoring of Data Transfer Agreements (DTA) and Data Transmission Specifications (DTS)",
      window_title = "DTAtools",
      footer_label = "",
      logo = "dtatools_logo_small.png",
      logo_alt = "DTAtools logo",
      landing_headline = "Check a data transfer against its specification.",
      landing_lede = "Open a DTA or DTS YAML file, load the data files it expects, run the checks, and export the agreement."
    ),
    # A label pill beside the app title in the brand bar, e.g. "GxP". "" shows
    # none; a NULL colour keeps the brand bar's own neutral pill look.
    header_tag = list(label = "", color = NULL),
    colors = c(
      primary = "#00625B",
      primary_dark = "#003D38",
      primary_light = "#E6F1EF",
      accent = "#00A886",
      text = "#1A2B2A",
      muted = "#5B6B6A",
      page_background = "#F4F7F6",
      surface = "#FFFFFF",
      warning = "#C77700",
      warning_dark = "#B26A00",
      nodata_border = "#EBD9B6",
      unknown = "#b8860b",
      pass = "#1E8E5A",
      pass_bg = "#E5F4EC",
      pass_border = "#9BD3B4",
      fail = "#C0392B",
      fail_bg = "#FBEAE7",
      fail_border = "#E9B4AC",
      pending = "#5B6B6A",
      pending_bg = "#EEF1F1",
      pending_border = "#CDD5D4"
    ),
    links = list(
      report_issue = "https://github.com/Boehringer-Ingelheim/DTAtoolsR/issues",
      about = "https://github.com/Boehringer-Ingelheim/DTAtoolsR#credits",
      repository = "https://github.com/Boehringer-Ingelheim/DTAtoolsR",
      documentation = NULL
    ),
    features = list(
      load_example = TRUE,
      create_from_template = TRUE,
      create_blank = TRUE,
      raw_yaml_tab = TRUE,
      raw_yaml_editable = TRUE,
      dataset_yaml_download = TRUE,
      dta_yaml_download = TRUE,
      editing = TRUE
    ),
    exports = list(
      document = c("markdown", "markdown_pdf", "word", "word_template"),
      default_document = "word",
      word_templates = data.frame(
        label = character(), path = character(),
        stringsAsFactors = FALSE
      ),
      bundled_word_templates = TRUE,
      validation_messages = c("csv", "tsv", "xlsx", "html"),
      validation_summary = TRUE
    )
  )
}

# ---- Value checks ------------------------------------------------------------
# Each returns list(ok = TRUE, value = ) or list(ok = FALSE, msg = ).

.cfg_ok <- function(value) list(ok = TRUE, value = value)
.cfg_bad <- function(msg) list(ok = FALSE, msg = msg)

.cfg_is_scalar_chr <- function(v) {
  is.character(v) && length(v) == 1 && !is.na(v)
}

.cfg_str <- function(v) {
  if (.cfg_is_scalar_chr(v)) .cfg_ok(v) else .cfg_bad("must be a single string.")
}

# A single-line string: whitespace runs (newlines included) collapse to one
# space, so a value cannot start a new Markdown block in an export.
.cfg_line <- function(v) {
  if (.cfg_is_scalar_chr(v)) .cfg_ok(trimws(gsub("\\s+", " ", v))) else .cfg_bad("must be a single string.")
}

.cfg_bool <- function(v) {
  if (is.logical(v) && length(v) == 1 && !is.na(v)) {
    .cfg_ok(v)
  } else {
    .cfg_bad("must be true or false.")
  }
}

.cfg_color <- function(v) {
  if (.cfg_is_scalar_chr(v) && grepl("^#([0-9A-Fa-f]{3}|[0-9A-Fa-f]{6})$", v)) {
    .cfg_ok(v)
  } else {
    .cfg_bad("must be a quoted hex colour such as \"#00625B\" (an unquoted # starts a YAML comment).")
  }
}

# The header tag: one line (see .cfg_line) of at most 32 characters, so it
# stays a short pill beside the title; "" means no tag. Never truncated: a
# cut-down status label could say something else.
.cfg_tag_label <- function(v) {
  r <- .cfg_line(v)
  if (r$ok && nchar(r$value) > 32) .cfg_bad("must be at most 32 characters.") else r
}

# `false` / null hide the link; anything else must be a plain http(s)/mailto URL.
.cfg_link <- function(v) {
  if (is.null(v) || identical(v, FALSE)) {
    return(.cfg_ok(NULL))
  }
  if (.cfg_is_scalar_chr(v) &&
    grepl("^(https?://|mailto:)", v, ignore.case = TRUE) &&
    !grepl("[[:space:]\"'<>]", v)) {
    .cfg_ok(v)
  } else {
    .cfg_bad("must be an http(s):// or mailto: URL without spaces or quotes, or false to hide it.")
  }
}

# One to three non-empty plain-text strings, as a single string or a list.
# `max_chars` caps each entry's length (abbreviations).
.cfg_doc_strings <- function(max_chars = Inf) {
  function(v) {
    if (is.list(v) && is.null(names(v)) && length(v) > 0 && all(vapply(v, .cfg_is_scalar_chr, logical(1)))) {
      v <- unlist(v, use.names = FALSE)
    }
    if (!is.character(v) || anyNA(v) || length(v) < 1 || length(v) > 3 ||
      !all(nzchar(trimws(v)))) {
      return(.cfg_bad("must be a non-empty string, or a list of one to three non-empty strings."))
    }
    v <- trimws(gsub("\\s+", " ", v))
    if (any(nchar(v) > max_chars)) {
      return(.cfg_bad(paste0("entries must be at most ", max_chars, " characters.")))
    }
    .cfg_ok(v)
  }
}

# A set of allowed words; `[]` (an empty list in R) means "none".
.cfg_subset <- function(allowed) {
  function(v) {
    if (is.list(v) && length(v) == 0) {
      return(.cfg_ok(character(0)))
    }
    if (is.character(v) && !anyNA(v) && all(v %in% allowed)) {
      return(.cfg_ok(unique(v)))
    }
    .cfg_bad(paste0("must be a list drawn from: ", paste(allowed, collapse = ", "), "."))
  }
}

# A relative path that resolves to an existing file INSIDE `base`. Lexical
# checks run first (so "../x" never touches the disk); normalizePath then
# catches a symlink pointing out of the directory.
.cfg_file_in_dir <- function(v, base) {
  if (!.cfg_is_scalar_chr(v) || !nzchar(v)) {
    return(.cfg_bad("must be a relative file path."))
  }
  if (grepl("^([A-Za-z]:|[/\\\\~])", v) || grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)", v)) {
    return(.cfg_bad("must be a relative path inside the library folder (no absolute path, no '..')."))
  }
  full <- file.path(base, v)
  if (!file.exists(full) || dir.exists(full)) {
    return(.cfg_bad(paste0("file not found: ", v)))
  }
  real <- normalizePath(full, winslash = "/", mustWork = TRUE)
  root <- normalizePath(base, winslash = "/", mustWork = TRUE)
  if (!startsWith(real, paste0(sub("/$", "", root), "/"))) {
    return(.cfg_bad("resolves outside the library folder."))
  }
  .cfg_ok(real)
}

.cfg_logo <- function(base) {
  function(v) {
    r <- .cfg_file_in_dir(v, base)
    if (!r$ok) {
      return(r)
    }
    mimes <- c(
      png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg",
      gif = "image/gif", svg = "image/svg+xml", webp = "image/webp"
    )
    ext <- tolower(tools::file_ext(r$value))
    if (!ext %in% names(mimes)) {
      return(.cfg_bad("must be a png, jpg, jpeg, gif, svg or webp image."))
    }
    if (file.size(r$value) > 1024^2) {
      return(.cfg_bad("must be at most 1 MB."))
    }
    bytes <- readBin(r$value, "raw", n = file.size(r$value))
    b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(bytes))
    .cfg_ok(paste0("data:", mimes[[ext]], ";base64,", b64))
  }
}

# ---- Parser ------------------------------------------------------------------

# Parse and normalise one config file. Never throws for bad content: returns
# the config (defaults filled in) plus a `problems` data frame, severity
# "error" (value rejected, default kept) or "warning" (unknown key, ignored).
# `type` is "unknown_key" for an unrecognised key and "value" otherwise, so
# validate_template() can pick its issue code without matching message text.
parse_dta_app_config <- function(path) {
  cfg <- dta_app_config_defaults()
  problems <- data.frame(
    severity = character(), key = character(), message = character(),
    type = character(), stringsAsFactors = FALSE
  )
  set_keys <- character(0)
  add <- function(severity, key, message, type = "value") {
    problems <<- rbind(problems, data.frame(
      severity = severity, key = key, message = message, type = type,
      stringsAsFactors = FALSE
    ))
  }
  finish <- function() list(config = cfg, problems = problems)

  raw <- tryCatch(
    suppressWarnings(yaml::read_yaml(path, fileEncoding = "UTF-8", readLines.warn = FALSE)),
    error = function(e) e
  )
  if (inherits(raw, "error")) {
    add("error", "(file)", paste0("cannot be read as YAML: ", conditionMessage(raw)))
    return(finish())
  }
  if (is.null(raw) || (is.list(raw) && length(raw) == 0)) {
    return(finish())
  }
  if (!is.list(raw) || is.null(names(raw)) || any(!nzchar(names(raw)))) {
    add("error", "(file)", "must be a mapping of settings at the top level.")
    return(finish())
  }
  if (!is.null(raw[["kind"]]) && !identical(raw[["kind"]], "dta_app_config")) {
    add("error", "kind", "must be \"dta_app_config\" when present.")
    return(finish())
  }

  base <- dirname(path)

  # A section is a mapping; unknown keys inside it are warned about, not fatal.
  section <- function(x, key, allowed) {
    if (is.null(x)) {
      return(list())
    }
    if (!is.list(x) || (length(x) > 0 && (is.null(names(x)) || any(!nzchar(names(x)))))) {
      add("error", key, "must be a mapping.")
      return(list())
    }
    for (u in setdiff(names(x), allowed)) {
      add("warning", paste0(key, ".", u), "is not a known setting and is ignored.", type = "unknown_key")
    }
    x
  }

  # Validate each present key of `x` with its check; on success store it in
  # cfg[[target]]. `prefix` is the dotted key shown in problems.
  fill <- function(target, x, checks, prefix = target, null_ok = character(0)) {
    for (nm in names(checks)) {
      if (!nm %in% names(x)) next
      v <- x[[nm]]
      if (is.null(v) && !nm %in% null_ok) next
      key <- paste0(prefix, ".", nm)
      r <- checks[[nm]](v)
      if (!r$ok) {
        add("error", key, r$msg)
        next
      }
      set_keys <<- c(set_keys, key)
      if (target == "colors") {
        cfg$colors[[nm]] <<- r$value
      } else {
        cfg[[target]][nm] <<- list(r$value)
      }
    }
  }

  top <- c("kind", "organisation", "document", "branding", "header_tag", "colors", "links", "features", "exports")
  for (u in setdiff(names(raw), top)) {
    add("warning", u, "is not a known setting and is ignored.", type = "unknown_key")
  }

  org <- section(raw[["organisation"]], "organisation", c("name", "unit"))
  fill("organisation", org, list(name = .cfg_line, unit = .cfg_line))

  dsec <- section(raw[["document"]], "document", c("name", "abbreviation"))
  doc_names <- doc_abbrs <- NULL
  if (!is.null(dsec[["name"]])) {
    r <- .cfg_doc_strings()(dsec[["name"]])
    if (r$ok) doc_names <- r$value else add("error", "document.name", r$msg)
  }
  if (!is.null(dsec[["abbreviation"]])) {
    r <- .cfg_doc_strings(12)(dsec[["abbreviation"]])
    if (r$ok) doc_abbrs <- r$value else add("error", "document.abbreviation", r$msg)
  }
  if (length(doc_names) > 1 && length(doc_abbrs) > 1 && length(doc_names) != length(doc_abbrs)) {
    add(
      "error", "document.abbreviation",
      "must list as many entries as document.name when both are lists."
    )
    doc_abbrs <- NULL
  }
  if (!is.null(doc_names)) cfg$document$names <- doc_names
  if (!is.null(doc_abbrs)) cfg$document$abbreviations <- doc_abbrs
  cfg$document$label <- paste(cfg$document$names, collapse = " / ")
  cfg$document$abbr <- paste(cfg$document$abbreviations, collapse = "/")
  cfg$document$custom <- !is.null(doc_names) || !is.null(doc_abbrs)

  br_keys <- c(
    "app_title", "app_subtitle", "window_title", "footer_label", "logo",
    "logo_alt", "landing_headline", "landing_lede"
  )
  br <- section(raw[["branding"]], "branding", br_keys)
  br_checks <- stats::setNames(lapply(br_keys, function(k) .cfg_str), br_keys)
  br_checks$logo <- .cfg_logo(base)
  fill("branding", br, br_checks)

  # null_ok: an unquoted #hex is a YAML comment, so `color: #C0392B` arrives as
  # null and must be reported, exactly as under colors:.
  ht <- section(raw[["header_tag"]], "header_tag", c("label", "color"))
  fill("header_tag", ht, list(label = .cfg_tag_label, color = .cfg_color), null_ok = "color")

  col_keys <- names(cfg$colors)
  col <- section(raw[["colors"]], "colors", col_keys)
  fill(
    "colors", col,
    stats::setNames(lapply(col_keys, function(k) .cfg_color), col_keys),
    null_ok = col_keys
  )

  link_keys <- names(cfg$links)
  lk <- section(raw[["links"]], "links", link_keys)
  fill(
    "links", lk,
    stats::setNames(lapply(link_keys, function(k) .cfg_link), link_keys),
    null_ok = link_keys
  )

  ft <- section(
    raw[["features"]], "features",
    c("start", "raw_yaml_tab", "raw_yaml_editable", "dataset_yaml_download", "dta_yaml_download", "editing")
  )
  start_keys <- c("load_example", "create_from_template", "create_blank")
  start <- section(ft[["start"]], "features.start", start_keys)
  fill(
    "features", start, stats::setNames(lapply(start_keys, function(k) .cfg_bool), start_keys),
    prefix = "features.start"
  )
  flat <- c("raw_yaml_tab", "raw_yaml_editable", "dataset_yaml_download", "dta_yaml_download", "editing")
  fill("features", ft, stats::setNames(lapply(flat, function(k) .cfg_bool), flat))

  ex <- section(
    raw[["exports"]], "exports",
    c(
      "document", "default_document", "word_templates", "bundled_word_templates",
      "validation_messages", "validation_summary"
    )
  )
  fill("exports", ex, list(
    document = .cfg_subset(c("markdown", "markdown_pdf", "word", "word_template")),
    default_document = .cfg_str,
    bundled_word_templates = .cfg_bool,
    validation_messages = .cfg_subset(c("csv", "tsv", "xlsx", "html")),
    validation_summary = .cfg_bool
  ))

  wt <- ex[["word_templates"]]
  if (!is.null(wt)) {
    k <- "exports.word_templates"
    if (!is.list(wt) || !is.null(names(wt))) {
      add("error", k, "must be a list of mappings, each with a 'file' key.")
    } else {
      rows <- list()
      for (i in seq_along(wt)) {
        e <- wt[[i]]
        ki <- sprintf("%s[%d]", k, i)
        if (!is.list(e) || is.null(names(e))) {
          add("error", ki, "must be a mapping with a 'file' key.")
          next
        }
        for (u in setdiff(names(e), c("file", "label"))) {
          add("warning", paste0(ki, ".", u), "is not a known setting and is ignored.", type = "unknown_key")
        }
        f <- .cfg_file_in_dir(e[["file"]], base)
        if (f$ok && !grepl("\\.docx$", f$value, ignore.case = TRUE)) {
          f <- .cfg_bad("must be a .docx file.")
        }
        if (!f$ok) {
          add("error", paste0(ki, ".file"), f$msg)
          next
        }
        label <- tools::file_path_sans_ext(basename(f$value))
        if (!is.null(e[["label"]])) {
          l <- .cfg_str(e[["label"]])
          if (l$ok) label <- l$value else add("error", paste0(ki, ".label"), l$msg)
        }
        rows[[length(rows) + 1]] <- data.frame(
          label = label, path = f$value, stringsAsFactors = FALSE
        )
      }
      if (length(rows) > 0) cfg$exports$word_templates <- do.call(rbind, rows)
    }
  }

  # default_document must be offered. Only an explicit bad value is worth a
  # warning; a narrowed `document:` list silently moves the implicit default.
  doc <- cfg$exports$document
  dd <- cfg$exports$default_document
  if (length(doc) == 0) {
    dd <- NULL
  } else if (!isTRUE(dd %in% doc)) {
    if ("exports.default_document" %in% set_keys) {
      add("warning", "exports.default_document", "is not in exports.document; using the first listed format.")
    }
    dd <- doc[[1]]
  }
  cfg$exports["default_document"] <- list(dd)

  # Derived defaults, unless the file set them explicitly.
  if (!"branding.window_title" %in% set_keys) {
    cfg$branding$window_title <- cfg$branding$app_title
  }
  if (!"branding.logo_alt" %in% set_keys) {
    cfg$branding$logo_alt <- paste(cfg$branding$app_title, "logo")
  }
  if (!"branding.footer_label" %in% set_keys) {
    org_name <- cfg$organisation$name
    org_unit <- cfg$organisation$unit
    cfg$branding$footer_label <- if (!nzchar(org_name)) {
      ""
    } else if (nzchar(org_unit)) {
      paste0(org_name, " · ", org_unit)
    } else {
      org_name
    }
  }

  # A blank document is useless when nothing can be edited.
  if (!cfg$features$editing) {
    if ("features.start.create_blank" %in% set_keys && isTRUE(start[["create_blank"]])) {
      add("warning", "features.start.create_blank", "has no effect while features.editing is false.")
    }
    cfg$features$create_blank <- FALSE
    # Nothing can be applied either, so the Raw YAML tab is read-only.
    cfg$features$raw_yaml_editable <- FALSE
  }

  finish()
}

# ---- Discovery ---------------------------------------------------------------

# First config file across `roots` (in precedence order) wins; any others are
# reported so the app can say they were ignored -- there is no per-key merge.
dta_app_config_find <- function(roots) {
  roots <- roots[!is.na(roots) & nzchar(roots)]
  hits <- character(0)
  for (r in roots) {
    for (f in dta_app_config_filenames()) {
      p <- file.path(r, f)
      if (file.exists(p) && !dir.exists(p)) {
        hits <- c(hits, normalizePath(p, winslash = "/", mustWork = TRUE))
      }
    }
  }
  hits <- unique(hits)
  list(
    path = if (length(hits)) hits[[1]] else NA_character_,
    ignored = hits[-1]
  )
}

# ---- Cached accessor -----------------------------------------------------------

# Own cache (not the template index's) so a config problem is warned once per
# fill rather than once per call; same TTL as the template index.
.dta_app_config_cache <- new.env(parent = emptyenv())

# Word templates found by convention in <root>/export-templates/*.docx, across
# `roots` in precedence order, as the same (label, path) frame as
# cfg$exports$word_templates. `listed_paths` are the ones dta-app.yaml already
# lists; they are not repeated. A file whose basename was already taken by an
# earlier file at a DIFFERENT path is skipped with a warning, never silently.
# A discovered file must also hold at least one {PLACEHOLDER} marker
# (template_has_placeholders(), utils_export.R): a styles-only reference document
# would export a Word file without the user's DTA in it, so it is skipped with a
# warning. Files listed in dta-app.yaml are the library author's explicit choice
# and are not filtered.
# Every candidate passes the same containment check as a configured file, so a
# symlink out of the library is skipped.
dta_app_config_discover_word_templates <- function(roots, listed_paths = character(0)) {
  out <- data.frame(label = character(), path = character(), stringsAsFactors = FALSE)
  norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  taken <- list()
  for (p in listed_paths) taken[[tolower(basename(p))]] <- norm(p)

  roots <- roots[!is.na(roots) & nzchar(roots)]
  for (root in roots) {
    sub <- dta_export_templates_subdir()
    if (!dir.exists(file.path(root, sub))) next
    files <- list.files(file.path(root, sub), pattern = "\\.docx$", ignore.case = TRUE)
    files <- files[!startsWith(files, ".") & !startsWith(files, "~$")]
    # enc2utf8(): a radix sort refuses native-encoded non-ASCII file names.
    for (f in sort(enc2utf8(files), method = "radix")) {
      r <- .cfg_file_in_dir(file.path(sub, f), root)
      if (!r$ok) next
      key <- tolower(basename(r$value))
      if (!is.null(taken[[key]])) {
        if (!identical(taken[[key]], r$value)) {
          cli::cli_warn(
            "Ignoring Word template {.path {r$value}}: {.file {basename(r$value)}} is already provided by {.path {taken[[key]]}}."
          )
        }
        next
      }
      if (!isTRUE(template_has_placeholders(r$value))) {
        cli::cli_warn(
          "Not offering Word template {.path {r$value}}: it contains no {{PLACEHOLDER}} markers, or cannot be read as a Word document."
        )
        next
      }
      taken[[key]] <- r$value
      out[nrow(out) + 1L, ] <- list(tools::file_path_sans_ext(basename(r$value)), r$value)
    }
  }
  out
}

.dta_app_config_build <- function() {
  roots <- tryCatch(dta_template_source_roots()$roots, error = function(e) character(0))
  found <- dta_app_config_find(roots)
  cfg <- dta_app_config_defaults()
  if (!is.na(found$path)) {
    parsed <- tryCatch(parse_dta_app_config(found$path), error = function(e) NULL)
    if (!is.null(parsed)) {
      cfg <- parsed$config
      cfg$source <- found$path
      src <- found$path
      for (i in seq_len(nrow(parsed$problems))) {
        key <- parsed$problems$key[[i]]
        msg <- parsed$problems$message[[i]]
        sev <- parsed$problems$severity[[i]]
        cli::cli_warn("{.path {src}} ({sev}) {.field {key}}: {msg}")
      }
    }
  }
  for (extra in found$ignored) {
    kept <- found$path
    cli::cli_warn("Ignoring {.path {extra}}: only the first app config found ({.path {kept}}) is used.")
  }
  # Configured Word templates keep their label and order; files dropped into
  # <root>/export-templates/ follow them (no dta-app.yaml needed).
  found_wt <- dta_app_config_discover_word_templates(roots, cfg$exports$word_templates$path)
  cfg$exports$word_templates <- rbind(cfg$exports$word_templates, found_wt)
  rownames(cfg$exports$word_templates) <- NULL
  cfg
}

dta_app_config <- function(refresh = FALSE) {
  now <- as.numeric(Sys.time())
  cached <- .dta_app_config_cache$config
  built_at <- .dta_app_config_cache$built_at %||% NA_real_
  if (!refresh && !is.null(cached) && !is.na(built_at) &&
    now - built_at < dta_template_refresh_seconds()) {
    return(cached)
  }
  cfg <- .dta_app_config_build()
  .dta_app_config_cache$config <- cfg
  .dta_app_config_cache$built_at <- now
  cfg
}

dta_app_config_invalidate <- function() {
  .dta_app_config_cache$config <- NULL
  .dta_app_config_cache$built_at <- NA_real_
  invisible(NULL)
}

# ---- Export arguments ----------------------------------------------------------

# The document-naming and organisation arguments every DTAtools writer takes,
# from the app config. An unset organisation is NULL (shown nowhere), not "".
dta_app_export_args <- function(cfg) {
  nz <- function(x) if (is.character(x) && length(x) == 1 && nzchar(x)) x else NULL
  list(
    document_name = cfg$document$label,
    document_abbreviation = cfg$document$abbr,
    organisation = nz(cfg$organisation$name),
    organisation_unit = nz(cfg$organisation$unit)
  )
}
