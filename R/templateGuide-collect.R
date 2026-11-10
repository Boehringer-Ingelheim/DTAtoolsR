# -----------------------------------------------------------------------------
# Template guide: the COLLECT half.
#
# .template_guide_collect() reads a template library directory and returns one
# plain list of character data frames (see the contract at the bottom of this
# header). It knows nothing about Word; R/templateGuide.R renders it.
#
# Like validate_template(), it builds a LOCAL index of exactly the directory it
# is given and drives the Shiny app's template engine through
# .dta_template_engine_get() -- never the env-var driven build_template_index().
#
# Rules this file obeys (see validateTemplate.R for the reasons):
#   * YAML lists are read with `[[`, never `$`.
#   * `%||%` here is base R's (NULL only); emptiness is tested with length().
#   * Every sort uses method = "radix".
#
# Contract (every table is a data.frame(check.names = FALSE) of character
# columns; zero rows are allowed, NULL never is):
#   list(library, overview, creation, datasets, vocabularies, parties, problems)
# -----------------------------------------------------------------------------

#' Collect everything the template guide prints
#'
#' @param path Library root (the directory holding `templates/`, `parties/` and
#'   `vocabulary/`), or one template file inside one of those folders.
#' @param templates Optional template ids; restricts the guide to those plus
#'   everything they reference.
#' @return A plain list; see the header of this file.
#' @keywords internal
#' @noRd
.template_guide_collect <- function(path, templates = NULL) {
  g <- .dta_template_engine_get
  src <- .tgc_locate(path, g)

  files <- .dta_template_list_dir(src$dir, g("dta_template_all_kinds")())
  if (length(files) == 0) {
    cli::cli_abort(
      c(
        "No template files found in {.path {path}}.",
        i = paste0(
          "A template library is a directory with templates/, parties/ and ",
          "vocabulary/ folders; pass the library root, not one of the folders."
        )
      ),
      class = "dta_template_guide_no_templates"
    )
  }
  records <- stats::setNames(lapply(files, .dta_template_read_raw), files)
  index_df <- .dta_template_build_index_df(records)
  rownames(index_df) <- NULL

  problems <- list()
  add_problem <- function(file, msg) {
    row <- c(basename(as.character(file)), as.character(msg))
    if (!any(vapply(problems, identical, logical(1), y = row))) {
      problems[[length(problems) + 1L]] <<- row
    }
    invisible(NULL)
  }

  # Anything that did not make it into the index gets a plain problem row.
  restrict <- !is.null(templates) || !is.null(src$only_id)
  for (p in files) {
    rec <- records[[p]]
    if (!p %in% index_df$path && !restrict) {
      add_problem(p, if (isTRUE(rec$ok)) {
        "Not a recognised template: it needs a known 'kind', an 'id' and a 'version'."
      } else {
        rec$error
      })
    }
  }

  if (nrow(index_df) == 0) {
    if (restrict) {
      cli::cli_abort("No templates found in {.path {path}}.", class = "dta_template_guide_no_templates")
    }
    cli::cli_abort(
      "No usable template files found in {.path {path}}.",
      class = "dta_template_guide_no_templates"
    )
  }

  index_df <- .tgc_pad_index(index_df, records)
  all_index <- index_df

  ids <- unique(c(src$only_id, templates))
  if (length(ids) > 0) {
    unknown <- setdiff(ids, index_df$id)
    if (length(unknown) > 0) {
      cli::cli_abort(
        "Unknown template id{?s}: {.val {unknown}}.",
        class = "dta_template_guide_unknown"
      )
    }
    keep <- .tgc_closure(ids, index_df, records)
    index_df <- index_df[keep, , drop = FALSE]
    rownames(index_df) <- NULL
  }

  ctx <- list(
    g = g, index = index_df, all_index = all_index, records = records, problem = add_problem,
    vocab = .dta_template_vocab_resolved(index_df),
    profiles = suppressWarnings(g("template_party_profiles")(index_df))
  )

  org <- .tgc_organisation(unique(c(src$root, src$dir)))
  out <- list(
    library = list(
      name = basename(normalizePath(src$dir, winslash = "/", mustWork = FALSE)),
      path = src$dir,
      generated = format(Sys.Date(), "%Y-%m-%d"),
      package_version = as.character(utils::packageVersion("DTAtools")),
      organisation = org$name,
      organisation_unit = org$unit
    ),
    overview = .tgc_overview(index_df),
    creation = .tgc_collect_kind(index_df, "dta_creation_template", ctx, .tgc_creation, .tgc_creation_stub),
    datasets = .tgc_collect_kind(index_df, "dta_dataset_template", ctx, .tgc_dataset_template, .tgc_stub),
    vocabularies = .tgc_collect_kind(index_df, "dta_vocabulary", ctx, .tgc_vocabulary, .tgc_stub),
    parties = .tgc_collect_kind(index_df, "dta_party_profile", ctx, .tgc_party, .tgc_stub)
  )
  out$problems <- .tgc_df(c("File", "Problem"), problems)
  out
}

# ---- Small helpers ----------------------------------------------------------

# Data frame of character columns from a list of equal-length character rows.
.tgc_df <- function(cols, rows = list()) {
  if (length(rows) == 0) {
    vals <- lapply(cols, function(x) character(0))
  } else {
    vals <- lapply(seq_along(cols), function(i) {
      vapply(rows, function(r) as.character(r[[i]]), character(1))
    })
  }
  names(vals) <- cols
  data.frame(vals, check.names = FALSE, stringsAsFactors = FALSE)
}

.tgc_kv <- function(...) {
  x <- list(...)
  .tgc_df(c("key", "value"), unname(Map(function(k, v) c(k, v), names(x), x)))
}

# First element as character, or `default` for NULL / empty / NA.
.tgc_chr <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }
  v <- as.character(x[[1]])
  if (length(v) == 0 || is.na(v)) default else v
}

.tgc_join <- function(x, sep = "\n", empty = "\u2014") {
  x <- as.character(unlist(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) empty else paste(x, collapse = sep)
}

.tgc_plural <- function(n, one, many = paste0(one, "s")) {
  sprintf("%d %s", n, if (n == 1L) one else many)
}

# The cell text for one value: Yes/No for logicals, placeholders spelled out.
.tgc_value_text <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("\u2014")
  }
  if (is.list(x)) {
    x <- unlist(x)
  }
  if (length(x) == 0) {
    return("\u2014")
  }
  if (is.logical(x)) {
    return(paste(ifelse(is.na(x), "\u2014", ifelse(x, "Yes", "No")), collapse = ", "))
  }
  x <- as.character(x)
  if (length(x) == 1L) {
    if (tolower(x) == "yes") x <- "Yes"
    if (tolower(x) == "no") x <- "No"
  }
  x <- gsub("${today}", "(the date the DTA is created)", x, fixed = TRUE)
  x <- gsub("${version}", "(the version you enter)", x, fixed = TRUE)
  x[!nzchar(x)] <- "(empty)"
  paste(x, collapse = ", ")
}

.tgc_first_sentence <- function(text, fallback) {
  text <- trimws(gsub("\\s+", " ", .tgc_chr(text)))
  if (!nzchar(text)) {
    return(fallback)
  }
  sub("^(.*?[.!?])(\\s.*)?$", "\\1", text, perl = TRUE)
}

.tgc_humanise <- function(x) .dta_template_engine_get("tmpl_humanise")(x)

.tgc_kind_words <- c(
  dta_creation_template = "DTA template",
  dta_dataset_template = "Dataset template",
  dta_vocabulary = "Term list",
  dta_party_profile = "Party profile"
)

# ---- Locating and indexing ----------------------------------------------------

.tgc_locate <- function(path, g) {
  bad <- !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)
  if (bad || !file.exists(path)) {
    cli::cli_abort(
      "No template library found at {.path {path}}.",
      class = "dta_template_guide_no_templates"
    )
  }
  if (dir.exists(path)) {
    return(list(dir = path, root = path, only_id = NULL))
  }
  rec <- .dta_template_read_raw(path)
  if (!isTRUE(rec$ok) || is.na(rec$id)) {
    cli::cli_abort(
      "{.path {path}} is not a readable template file.",
      class = "dta_template_guide_no_templates"
    )
  }
  # A file is only understood together with its library: the root is found from
  # where the file sits, and only when it sits in the folder of its kind.
  root <- g("dta_template_locate_root")(path)
  if (is.na(root)) {
    expected <- if (rec$kind %in% g("dta_template_all_kinds")()) {
      paste0(g("dta_template_kind_subdir")(rec$kind), "/")
    } else {
      "templates/, parties/ or vocabulary/"
    }
    cli::cli_abort(
      c(
        "Cannot find the template library of {.path {path}}.",
        "i" = "A template file must sit in the folder of its kind ({expected}) of the library root, with a name ending in its kind suffix (such as {.val .dta-template.yaml}).",
        "i" = "Pass the library root directory to cover the whole library."
      ),
      class = "dta_template_guide_no_templates"
    )
  }
  list(dir = root, root = root, only_id = rec$id)
}

# Organisation name and unit from the library's dta-app.yaml, "" for anything
# missing or unreadable. Never throws: a bad config must not stop the guide.
.tgc_organisation <- function(roots) {
  none <- list(name = "", unit = "")
  tryCatch(
    {
      f <- .dta_template_engine_get("dta_app_config_find")(roots)$path
      if (is.na(f)) {
        return(none)
      }
      org <- .dta_template_engine_get("parse_dta_app_config")(f)$config[["organisation"]]
      list(name = .tgc_chr(org[["name"]]), unit = .tgc_chr(org[["unit"]]))
    },
    error = function(e) none
  )
}

# Labels and the extra columns the engine's history/lineage helpers expect.
.tgc_pad_index <- function(index_df, records) {
  defs <- lapply(index_df$path, function(p) records[[p]]$def)
  index_df$label <- vapply(seq_along(defs), function(i) {
    .tgc_chr(defs[[i]][["label"]], index_df$id[[i]])
  }, character(1))
  index_df$description <- vapply(defs, function(d) .tgc_chr(d[["description"]]), character(1))
  index_df$abstract <- vapply(defs, function(d) isTRUE(d[["abstract"]]), logical(1))
  index_df$extends <- vapply(defs, function(d) .tgc_chr(d[["extends"]]), character(1))
  index_df$source_name <- "local"
  index_df$source_kind <- "local"
  index_df$resolved_commit <- NA_character_
  index_df
}

# Template ids a raw definition points at (extends, mixins, dataset templates,
# vocabularies, party profile allow-lists, column bindings), without versions.
.tgc_refs <- function(def, versions = FALSE) {
  keys <- c("extends", "template", "vocabulary", "mixins", "values_from", "profiles")
  refs <- character(0)
  walk <- function(x) {
    if (!is.list(x)) {
      return(invisible(NULL))
    }
    nms <- names(x)
    for (i in seq_along(x)) {
      e <- x[[i]]
      if (!is.null(nms) && nzchar(nms[[i]]) && nms[[i]] %in% keys) {
        v <- unlist(e)
        if (is.character(v)) refs <<- c(refs, v)
      }
      if (is.list(e)) walk(e)
    }
    invisible(NULL)
  }
  walk(def)
  if (versions) unique(refs) else unique(sub("@.*$", "", refs))
}

# id / version / label rows for references of one kind that resolve in the
# index; a pinned "id@version" keeps its version, a bare id means the newest.
.tgc_ref_frame <- function(ctx, refs, kind) {
  hits <- lapply(unique(refs), function(r) ctx$g("resolve_template_ref")(ctx$index, r, kind = kind))
  hits <- Filter(Negate(is.null), hits)
  out <- .tgc_df(
    c("id", "version", "label"),
    lapply(hits, function(h) c(h$id[[1]], h$version[[1]], h$label[[1]]))
  )
  out <- out[!duplicated(out[c("id", "version")]), , drop = FALSE]
  out <- out[order(out$label, out$id, out$version, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Same shape, in the order given (lineage / mixins); an entry that no longer
# resolves keeps its id and version.
.tgc_ref_chain <- function(ctx, refs, kind = "dta_creation_template") {
  .tgc_df(c("id", "version", "label"), lapply(refs, function(r) {
    hit <- ctx$g("resolve_template_ref")(ctx$index, r, kind = kind)
    if (is.null(hit)) {
      c(sub("@.*$", "", r), if (grepl("@", r, fixed = TRUE)) sub("^[^@]*@", "", r) else "", sub("@.*$", "", r))
    } else {
      c(hit$id[[1]], hit$version[[1]], hit$label[[1]])
    }
  }))
}

# The term lists, dataset templates and party profiles ONE creation template
# offers: references in its resolved definition (variant choices included),
# plus whatever the dataset templates it uses bind their columns to.
.tgc_offered_refs <- function(def, parties, ctx) {
  refs <- .tgc_refs(def, versions = TRUE)
  ds <- .tgc_ref_frame(ctx, refs, "dta_dataset_template")
  inner <- unlist(lapply(seq_len(nrow(ds)), function(i) {
    p <- ctx$index$path[ctx$index$kind == "dta_dataset_template" & ctx$index$id == ds$id[[i]] &
      ctx$index$version == ds$version[[i]]]
    if (length(p) == 0) character(0) else .tgc_refs(ctx$records[[p[[1]]]]$def, versions = TRUE)
  }))
  list(
    dataset_templates = ds,
    vocabularies = .tgc_ref_frame(ctx, c(refs, inner), "dta_vocabulary"),
    parties = parties
  )
}

# Selected ids plus everything they reference, to a fixed point. Every version
# of a referenced id is kept (simpler than resolving each ref); party profiles
# are all kept as soon as a kept template declares party slots.
.tgc_closure <- function(ids, index_df, records) {
  keep <- index_df$id %in% ids
  repeat {
    defs <- lapply(index_df$path[keep], function(p) records[[p]]$def)
    refs <- unique(unlist(lapply(defs, .tgc_refs)))
    has_slots <- any(vapply(defs, function(d) length(d[["party_slots"]]) > 0, logical(1)))
    new <- keep | index_df$id %in% refs |
      (has_slots & index_df$kind == "dta_party_profile")
    if (identical(new, keep)) {
      return(keep)
    }
    keep <- new
  }
}

# Labels of templates of the given kinds that reference `id`. Looked up in the
# UNFILTERED index: `templates =` narrows what the guide prints, not who uses
# what.
.tgc_users <- function(ctx, id, kinds) {
  idx <- ctx$all_index
  rows <- which(idx$kind %in% kinds & idx$id != id)
  hit <- vapply(rows, function(i) id %in% .tgc_refs(ctx$records[[idx$path[[i]]]]$def), logical(1))
  sort(unique(idx$label[rows[hit]]), method = "radix")
}

# ---- Overview and chapter driver ------------------------------------------------

.tgc_overview <- function(index_df) {
  ord <- order(
    match(index_df$kind, names(.tgc_kind_words)), index_df$label,
    -.tgc_rank(index_df$version),
    method = "radix"
  )
  idx <- index_df[ord, , drop = FALSE]
  rows <- lapply(seq_len(nrow(idx)), function(i) {
    c(
      idx$label[[i]], unname(.tgc_kind_words[[idx$kind[[i]]]]), idx$id[[i]], idx$version[[i]],
      .tgc_first_sentence(idx$description[[i]], idx$label[[i]])
    )
  })
  .tgc_df(c("Name", "Kind", "ID", "Version", "What it is"), rows)
}

.tgc_rank <- function(versions) {
  nv <- .dta_template_engine_get("template_version_rank")(versions)
  o <- order(nv, na.last = FALSE)
  r <- integer(length(o))
  r[o] <- seq_along(o)
  r
}

.tgc_collect_kind <- function(index_df, kind, ctx, build, stub) {
  idx <- index_df[index_df$kind == kind, , drop = FALSE]
  if (nrow(idx) == 0) {
    return(list())
  }
  idx <- idx[order(idx$label, -.tgc_rank(idx$version), method = "radix"), , drop = FALSE]
  lapply(seq_len(nrow(idx)), function(i) {
    row <- idx[i, , drop = FALSE]
    rec <- ctx$records[[row$path]]
    tryCatch(
      build(row, rec, ctx),
      error = function(e) {
        msg <- conditionMessage(e)
        ctx$problem(row$path, msg)
        stub(row, rec, msg)
      }
    )
  })
}

# "Parent (which is based on Grandparent, which is based on Root)": `labels`
# nearest parent first.
.tgc_based_text <- function(labels) {
  labels <- as.character(labels)
  if (length(labels) == 0) {
    return("")
  }
  if (length(labels) == 1L) {
    return(labels)
  }
  sprintf("%s (which is based on %s)", labels[[1]], paste(labels[-1], collapse = ", which is based on "))
}

.tgc_technical <- function(row) {
  sprintf("ID %s \u00b7 version %s \u00b7 file %s", row$id, row$version, basename(row$path))
}

.tgc_date <- function(def) .tgc_chr(def[["date"]], "\u2014")

.tgc_ref_cols <- c("id", "version", "label")

# The fields every entry carries beyond its kind's own, with empty defaults.
.tgc_meta_empty <- function(row, rec) {
  list(
    abstract = FALSE, status = "", date = .tgc_chr(rec$def[["date"]]),
    file = basename(row$path), option_count = 0L, required_count = 0L,
    lineage = .tgc_df(.tgc_ref_cols), mixins = .tgc_df(.tgc_ref_cols),
    refs = list(
      dataset_templates = .tgc_df(.tgc_ref_cols), vocabularies = .tgc_df(.tgc_ref_cols),
      parties = .tgc_df(.tgc_ref_cols)
    )
  )
}

.tgc_stub <- function(row, rec, msg) {
  c(list(
    title = row$label, id = row$id, version = row$version,
    description = .tgc_chr(rec$def[["description"]]),
    summary = "", at_a_glance = .tgc_kv(Version = row$version),
    terms = .tgc_df(c("Code", "Label", "Description")),
    contacts = .tgc_df(c("Name", "Role", "Email", "Phone", "Signs")),
    settings = .tgc_df(c("Setting", "Answer type", "Pre-filled answer", "Appears in the DTA as", "Explanation")),
    columns = .tgc_df(.tgc_col_headers), rules = .tgc_df(.tgc_rule_headers),
    build_note = paste("This file could not be read:", msg),
    technical = .tgc_technical(row)
  ), .tgc_meta_empty(row, rec))
}

.tgc_creation_stub <- function(row, rec, msg) {
  empty <- .tgc_df
  c(list(
    title = row$label, id = row$id, version = row$version,
    description = .tgc_chr(rec$def[["description"]]), summary = "",
    at_a_glance = .tgc_kv(Version = row$version), parent = "",
    changes = empty(c("What", "Change")), changes_status = "none",
    questions = empty(.tgc_question_headers),
    variants = empty(c("Question", "Choice", "What it changes")),
    parties = empty(c("Party", "Appears in the DTA as", "Available profiles", "Must choose")),
    term_choices = empty(c("Choice", "Explanation", "Term list", "How it works", "Selected by default", "Minimum to select")),
    prefilled = empty(c("Field", "Value", "Question")), locked = empty(c("Field", "Rule")),
    datasets = list(),
    build_note = paste("This template could not be read:", msg),
    technical = .tgc_technical(row)
  ), .tgc_meta_empty(row, rec))
}

# ---- Paths and options (shared by creation and dataset templates) ---------------

.tgc_question_headers <- c(
  "Question", "Answer type", "Choices", "Pre-filled answer",
  "Appears in the DTA as", "Explanation", "Must answer / fixed"
)

# Top-level metadata keys have plain document names.
.tgc_field_words <- c(
  title = "Document title", version = "Document version",
  header = "Document header (organisation name)", date = "Document date",
  error_handling = "Error handling section",
  authorized_for_corrections = "Authorized for corrections section",
  version_history = "Version history"
)

# Field name for a path split into parts: a lone top-level key is mapped,
# nested paths read "A \u203a B".
.tgc_field_name <- function(parts) {
  if (length(parts) == 1L && parts[[1]] %in% names(.tgc_field_words)) {
    return(unname(.tgc_field_words[[parts[[1]]]]))
  }
  paste(.tgc_humanise(parts), collapse = " \u203a ")
}

# "metadata.transmission.frequency" -> "Transmission \u203a Frequency".
.tgc_path_text <- function(path, ds_titles = character(0)) {
  path <- sub("^base\\.", "", as.character(path))
  parts <- strsplit(path, ".", fixed = TRUE)[[1]]
  m <- regmatches(path, regexec("^datasets?\\.(?:([^.]+)\\.)?columns\\.([^.]+)\\.values$", path, perl = TRUE))[[1]]
  if (length(m) == 3) {
    where <- if (nzchar(m[[2]])) {
      title <- if (m[[2]] %in% names(ds_titles)) ds_titles[[m[[2]]]] else m[[2]]
      paste0(" in ", title)
    } else {
      ""
    }
    return(paste0("Allowed values of column ", m[[3]], where))
  }
  if (length(parts) > 1 && identical(parts[[1]], "metadata")) {
    parts <- parts[-1]
  }
  .tgc_field_name(parts)
}

.tgc_option_paths <- function(opt, ds_titles = character(0)) {
  paths <- .dta_template_option_targets(opt)
  if (length(paths) == 0) {
    return("\u2014")
  }
  .tgc_join(unique(vapply(paths, .tgc_path_text, character(1), ds_titles = ds_titles)))
}

# The input the app shows for one option, decided in the SAME branch order as
# render_template_option_input() (inst/shiny/dta_app/R/template_ui.R):
# boolean / textarea / number by type; then a strict list only when there are
# choices and free text is not allowed; one choice = a text box starting on it;
# several = free text with suggestions; none = plain text.
.tgc_input_kind <- function(opt, g) {
  typ <- tolower(.tgc_chr(opt[["type"]], "text"))
  ch <- if (typ %in% c("boolean", "textarea", "number")) character(0) else g("dta_template_choices")(opt)
  allow <- g("dta_template_allow_custom")(opt, default = !identical(typ, "select"))
  kind <- if (typ %in% c("boolean", "textarea", "number")) {
    typ
  } else if (length(ch) > 0 && !allow) {
    "strict"
  } else if (length(ch) == 1) {
    "single"
  } else if (length(ch) > 1) {
    "suggest"
  } else {
    "text"
  }
  list(kind = kind, typ = typ, choices = ch)
}

.tgc_answer_type <- function(opt, g) {
  switch(.tgc_input_kind(opt, g)$kind,
    boolean = "Yes / No",
    number = "Number",
    textarea = if (length(g("dta_template_choices")(opt)) > 0) {
      "Longer free text, with suggestions"
    } else {
      "Longer free text"
    },
    strict = "Pick one from the list",
    suggest = "Free text, with suggestions",
    "Free text"
  )
}

# What the app pre-fills for an option, or NULL for nothing. A strict list shows
# the choice's label; every other input shows the stored text itself.
.tgc_default_value <- function(opt, base_md, g) {
  k <- .tgc_input_kind(opt, g)
  d <- g("dta_template_default")(opt, base_md)
  switch(k$kind,
    boolean = identical(d, TRUE) || identical(d, "yes"),
    number = {
      n <- suppressWarnings(as.numeric(d %||% 0))
      if (length(n) != 1L || is.na(n)) NULL else n
    },
    strict = if (is.null(d)) {
      NULL
    } else {
      dc <- as.character(d)[[1]]
      lab <- names(k$choices)[match(dc, k$choices)]
      if (is.na(lab) || !nzchar(lab)) dc else lab
    },
    single = if (is.null(d)) unname(k$choices[[1]]) else d,
    d
  )
}

.tgc_prefill <- function(opt, base_md, g) {
  .tgc_value_text(.tgc_default_value(opt, base_md, g))
}

.tgc_question_row <- function(opt, base_md, g, flags = "\u2014", ds_titles = character(0)) {
  ch <- g("dta_template_choices")(opt)
  typ <- tolower(.tgc_chr(opt[["type"]], "text"))
  id <- .tgc_chr(opt[["id"]])
  c(
    .tgc_chr(opt[["label"]], .tgc_humanise(id)),
    .tgc_answer_type(opt, g),
    if (length(ch) == 0 || typ %in% c("boolean", "number")) "\u2014" else .tgc_join(names(ch)),
    .tgc_prefill(opt, base_md, g),
    .tgc_option_paths(opt, ds_titles),
    .tgc_chr(opt[["help"]], "\u2014"),
    flags
  )
}

# ---- Creation templates -----------------------------------------------------------

.tgc_creation <- function(row, rec, ctx) {
  g <- ctx$g
  # Every problem raised below belongs to this template's file.
  report <- ctx$problem
  ctx$problem <- function(file, msg) report(row$path, msg)
  index <- ctx$index
  rd <- g("read_dta_creation_template")(row$path)
  if (!isTRUE(rd$ok)) cli::cli_abort("{rd$error}")
  res <- g("resolve_template_inheritance")(rd$value, .dta_template_extends_resolver(index))
  def <- g("dta_template_finalize_def")(res$def)
  abstract <- isTRUE(def[["abstract"]])
  base_md <- def[["base"]][["metadata"]]
  lineage <- as.character(res$lineage %||% character(0))
  mixins <- as.character(res$mixins %||% character(0))

  ref_label <- function(ref) {
    g("tmpl_index_label")(index, sub("@.*$", "", ref), sub("^[^@]*@", "", ref))
  }
  parent <- if (length(lineage) > 0) ref_label(lineage[[1]]) else ""

  # Datasets: one dry-run build, or one per variant choice.
  note <- ""
  datasets <- list()
  ds_titles <- character(0)
  spec <- def[["variants"]]
  if (!abstract) {
    built <- tryCatch(
      .tgc_dry_builds(def, row$path, spec, ctx),
      error = function(e) {
        note <<- paste("A sample DTA could not be built from this template:", conditionMessage(e))
        ctx$problem(row$path, note)
        NULL
      }
    )
    for (b in built) {
      datasets <- c(datasets, .tgc_built_datasets(b$dta, b$variant, ctx, b$overrides))
    }
    ds_titles <- .tgc_titles(datasets)
  }

  questions <- .tgc_questions(def, base_md, g, ds_titles)
  parties <- .tgc_parties(def, ctx)
  terms <- .tgc_term_choices(def, ctx)
  variants <- .tgc_variants(def, spec, datasets, ctx)

  # An abstract parent cannot be built, so there is nothing to compare against.
  parent_abstract <- length(lineage) > 0 && any(
    index$abstract[index$id == sub("@.*$", "", lineage[[1]]) & index$version == sub("^[^@]*@", "", lineage[[1]])]
  )
  changes_status <- if (abstract || length(lineage) == 0) {
    "none"
  } else if (parent_abstract) {
    "parent_abstract"
  } else {
    "compared"
  }
  changes <- if (changes_status != "compared") {
    .tgc_df(c("What", "Change"))
  } else {
    tryCatch(
      .tgc_changes(sprintf("%s@%s", row$id, row$version), lineage[[1]], def, datasets, ctx),
      error = function(e) {
        ctx$problem(row$path, paste("Could not compare with the parent template:", conditionMessage(e)))
        changes_status <<- "failed"
        .tgc_df(c("What", "Change"))
      }
    )
  }

  opts <- Filter(is.list, def[["options"]] %||% list())
  list(
    title = row$label, id = row$id, version = row$version,
    abstract = abstract,
    status = if (abstract) "Building block" else "Ready to use",
    date = .tgc_chr(rec$def[["date"]]),
    file = basename(row$path),
    option_count = length(opts),
    required_count = sum(grepl("Must answer", questions[["Must answer / fixed"]], fixed = TRUE)),
    lineage = .tgc_ref_chain(ctx, lineage),
    mixins = .tgc_ref_chain(ctx, mixins),
    refs = .tgc_offered_refs(def, .tgc_party_refs(def, ctx), ctx),
    description = .tgc_chr(def[["description"]]),
    summary = .tgc_creation_summary(abstract, datasets, spec, nrow(questions), parties, terms, variants$changes_questions),
    at_a_glance = do.call(.tgc_kv, c(
      list(Status = if (abstract) {
        "Building block \u2014 cannot be used on its own, only as a basis for other templates"
      } else {
        "Ready to use"
      }),
      list(`Based on` = if (length(lineage) > 0) {
        .tgc_based_text(vapply(lineage, ref_label, character(1)))
      } else {
        "\u2014"
      }),
      if (length(mixins) > 0) list(`Also includes` = paste(vapply(mixins, ref_label, character(1)), collapse = ", ")),
      list(Version = row$version, Date = .tgc_date(rec$def))
    )),
    parent = parent,
    changes = changes, changes_status = changes_status,
    questions = questions,
    variants = variants$table,
    parties = parties,
    term_choices = terms,
    prefilled = .tgc_prefilled(def, ctx),
    locked = .tgc_locked(def, ctx),
    datasets = datasets,
    build_note = note,
    technical = .tgc_technical(row)
  )
}

.tgc_dry_builds <- function(def, path, spec, ctx) {
  g <- ctx$g
  one <- function(sel) {
    sdef <- g("resolve_template_variant")(def, sel)
    vsel <- .dta_template_vocab_dry_selections(sdef, ctx$index)
    b <- g("create_dta_from_template")(sdef, path, sel, index = ctx$index, vocab_selections = vsel)
    if (!isTRUE(b$ok)) cli::cli_abort("{b$error}")
    # The same term-list narrowing, to lay over the dataset template when the
    # built dataset is compared with it.
    overrides <- tryCatch(
      g("resolve_vocabulary_slot_overrides")(
        suppressWarnings(g("normalise_vocabulary_slots")(sdef[["vocabulary_slots"]] %||% list())),
        vsel, ctx$vocab
      ),
      error = function(e) list()
    )
    list(dta = b$value, overrides = overrides)
  }
  if (is.null(spec)) {
    r <- one(list())
    return(list(list(dta = r$dta, overrides = r$overrides, variant = "")))
  }
  lapply(spec[["choices"]], function(choice) {
    sel <- stats::setNames(list(choice[["value"]]), spec[["id"]])
    r <- one(sel)
    list(
      dta = r$dta, overrides = r$overrides,
      variant = sprintf("For choice '%s'", .tgc_chr(choice[["label"]], choice[["value"]]))
    )
  })
}

.tgc_creation_summary <- function(abstract, datasets, spec, n_questions, parties, terms, variants_change = FALSE) {
  if (abstract) {
    return("Building block: cannot be used on its own; other templates are based on it.")
  }
  made <- if (!is.null(spec)) {
    sprintf(
      "Produces a different set of datasets depending on %s (%s).",
      .tgc_chr(spec[["label"]], spec[["id"]]), .tgc_plural(length(spec[["choices"]]), "choice")
    )
  } else if (length(datasets) == 0) {
    "Produces no datasets."
  } else {
    sprintf(
      "Produces %s (%s).", .tgc_plural(length(datasets), "dataset"),
      paste(vapply(datasets, function(d) {
        about <- stats::setNames(d$about$value, d$about$key)
        sprintf("%s, %s columns, %s", d$title, about[["Columns"]], .tgc_plural(as.integer(about[["Rules"]]), "rule"))
      }, character(1)), collapse = "; ")
    )
  }
  asks <- if (n_questions == 0) "Asks no questions." else sprintf("Asks %s.", .tgc_plural(n_questions, "question"))
  picks <- c(
    sprintf("a %s", parties$Party),
    if (nrow(terms) > 0) .tgc_plural(nrow(terms), "term list")
  )
  choose <- if (length(picks) == 0) {
    ""
  } else if (length(picks) == 1L) {
    sprintf(" You choose %s.", picks)
  } else {
    sprintf(" You choose %s and %s.", paste(picks[-length(picks)], collapse = ", "), picks[[length(picks)]])
  }
  note <- if (isTRUE(variants_change) && !is.null(spec)) {
    sprintf(" Some answers to '%s' change the questions; see Variants.", .tgc_chr(spec[["label"]], spec[["id"]]))
  } else {
    ""
  }
  paste0(made, " ", asks, choose, note)
}

.tgc_clean_path <- function(paths) {
  sub("^base\\.", "", as.character(unlist(paths)))
}

# Is the required `path` still open? A required path is satisfied by ANY filled
# value (dta_template_check_required), so one with a value in `base:`, or one
# that an option with a filled pre-filled answer writes to, asks nothing of the
# person creating the DTA.
.tgc_required_open <- function(def, path, opts, base_md, g) {
  filled <- g("dta_template_path_is_filled")
  overlap <- g("dta_template_paths_overlap")
  if (!startsWith(path, "options.")) {
    base_value <- g("list_get_path")(def[["base"]], strsplit(path, ".", fixed = TRUE)[[1]])
    if (filled(base_value)) {
      return(FALSE)
    }
  }
  for (opt in opts) {
    mine <- c(
      paste0("options.", .tgc_chr(opt[["id"]])),
      sub("^base\\.", "", .dta_template_option_targets(opt))
    )
    if (any(vapply(mine, overlap, logical(1), b = path)) &&
      filled(.tgc_default_value(opt, base_md, g))) {
      return(FALSE)
    }
  }
  TRUE
}

.tgc_questions <- function(def, base_md, g, ds_titles) {
  opts <- def[["options"]]
  opts <- Filter(is.list, opts %||% list())
  must <- .tgc_clean_path(def[["required"]])
  sealed <- .tgc_clean_path(def[["sealed"]])
  overlap <- g("dta_template_paths_overlap")
  matched <- function(paths, opt) {
    mine <- c(
      paste0("options.", .tgc_chr(opt[["id"]])),
      sub("^base\\.", "", .dta_template_option_targets(opt))
    )
    Filter(function(p) any(vapply(mine, overlap, logical(1), b = p)), paths)
  }
  # A required path is checked on the finished document, so a question that
  # writes to one must be answered even when it starts pre-filled: clearing it
  # aborts the creation.
  rows <- lapply(opts, function(opt) {
    flag <- c(if (length(matched(must, opt)) > 0) "Must answer", if (length(matched(sealed, opt)) > 0) "Fixed")
    .tgc_question_row(opt, base_md, g, .tgc_join(flag, ", "), ds_titles)
  })
  .tgc_df(.tgc_question_headers, rows)
}

.tgc_variant_label <- function(choice) {
  sprintf("For choice '%s'", .tgc_chr(choice[["label"]], choice[["value"]]))
}

# The Variants table. Each choice may carry its own `base:`, `options:` and
# datasets, so every row is worked out from the definition THAT choice selects
# and compared with the template as the Questions table shows it (no choice
# applied). Returns list(table, changes_questions).
.tgc_variants <- function(def, spec, datasets, ctx) {
  headers <- c("Question", "Choice", "What it changes")
  if (is.null(spec)) {
    return(list(table = .tgc_df(headers), changes_questions = FALSE))
  }
  g <- ctx$g
  q <- .tgc_chr(spec[["label"]], .tgc_humanise(spec[["id"]]))
  base_md <- def[["base"]][["metadata"]]
  opts <- Filter(is.list, def[["options"]] %||% list())
  ids <- function(o) vapply(o, function(x) .tgc_chr(x[["id"]]), character(1))
  opt_label <- function(x) .tgc_chr(x[["label"]], .tgc_humanise(.tgc_chr(x[["id"]])))
  main_fields <- .tgc_prefilled(def, ctx, annotate = FALSE)
  main_fields <- stats::setNames(main_fields$Value, main_fields$Field)
  any_change <- FALSE

  rows <- lapply(spec[["choices"]], function(ch) {
    lines <- character(0)
    mine <- Filter(function(d) identical(d$variant, .tgc_variant_label(ch)), datasets)
    if (length(mine) > 0) {
      lines <- c(lines, sprintf(
        "Creates %s: %s", if (length(mine) == 1L) "dataset" else "datasets",
        paste(vapply(mine, function(d) sprintf("'%s'", d$title), character(1)), collapse = ", ")
      ))
    }
    sdef <- tryCatch(
      g("resolve_template_variant")(def, stats::setNames(list(ch[["value"]]), spec[["id"]])),
      error = function(e) {
        ctx$problem(
          .tgc_chr(def[["id"]]),
          paste("A variant choice could not be compared:", conditionMessage(e))
        )
        NULL
      }
    )
    if (is.null(sdef)) {
      lines <- c(lines, "Its questions and pre-filled answers could not be worked out.")
    } else {
      sopts <- Filter(is.list, sdef[["options"]] %||% list())
      sbase_md <- sdef[["base"]][["metadata"]]
      new_ids <- setdiff(ids(sopts), ids(opts))
      gone_ids <- setdiff(ids(opts), ids(sopts))
      add <- vapply(sopts[ids(sopts) %in% new_ids], function(x) sprintf("Adds question '%s'", opt_label(x)), character(1))
      del <- vapply(opts[ids(opts) %in% gone_ids], function(x) sprintf("Removes question '%s'", opt_label(x)), character(1))

      fields <- .tgc_prefilled(sdef, ctx, annotate = FALSE)
      fields <- stats::setNames(fields$Value, fields$Field)
      both <- intersect(names(main_fields), names(fields))
      moved <- both[main_fields[both] != fields[both]]
      field_lines <- c(
        vapply(moved, function(f) sprintf("%s: %s", f, .tgc_arrow(main_fields[[f]], fields[[f]])), character(1)),
        vapply(setdiff(names(fields), names(main_fields)), function(f) sprintf("%s: pre-filled with %s", f, fields[[f]]), character(1)),
        vapply(setdiff(names(main_fields), names(fields)), function(f) sprintf("%s: no longer pre-filled", f), character(1))
      )
      changed_fields <- c(moved, setdiff(names(fields), names(main_fields)), setdiff(names(main_fields), names(fields)))

      # A question whose pre-filled answer moved because its field moved is
      # already covered by the field's own line.
      same <- intersect(ids(opts), ids(sopts))
      ans <- character(0)
      for (id in same) {
        o <- opts[[match(id, ids(opts))]]
        so <- sopts[[match(id, ids(sopts))]]
        from <- .tgc_prefill(o, base_md, g)
        to <- .tgc_prefill(so, sbase_md, g)
        where <- vapply(.dta_template_option_targets(so), .tgc_path_text, character(1))
        if (!identical(from, to) && !any(where %in% changed_fields)) {
          ans <- c(ans, sprintf("Question '%s' is pre-filled: %s", opt_label(so), .tgc_arrow(from, to)))
        }
      }
      extra <- c(add, del, ans, field_lines)
      if (length(extra) > 0) any_change <<- TRUE
      lines <- c(lines, extra)
    }
    c(q, .tgc_chr(ch[["label"]], ch[["value"]]), .tgc_join(lines, "\n", "Nothing else differs."))
  })
  list(table = .tgc_df(headers, rows), changes_questions = any_change)
}

# Party and vocabulary slots, normalised by the app's own code. One that fails
# to normalise is a problem the guide must report, not a section that quietly
# goes missing.
.tgc_slots <- function(def, ctx, what) {
  fn <- c(party = "normalise_party_slots", vocabulary = "normalise_vocabulary_slots")[[what]]
  key <- c(party = "party_slots", vocabulary = "vocabulary_slots")[[what]]
  raw <- def[[key]]
  if (length(raw) == 0) {
    return(list())
  }
  tryCatch(
    suppressWarnings(ctx$g(fn)(raw)),
    error = function(e) {
      ctx$problem(
        .tgc_chr(def[["id"]]),
        sprintf("The %s choices (%s) could not be read: %s", what, key, conditionMessage(e))
      )
      list()
    }
  )
}

.tgc_parties <- function(def, ctx) {
  g <- ctx$g
  slots <- .tgc_slots(def, ctx, "party")
  rows <- lapply(slots, function(slot) {
    profs <- g("party_profiles_for_slot")(ctx$profiles, slot)
    c(
      slot$label, .tgc_path_text(slot$target),
      .tgc_join(vapply(profs, function(p) .tgc_chr(p[["label"]], p[["id"]]), character(1))),
      if (isTRUE(slot$required)) "Yes" else "No"
    )
  })
  .tgc_df(c("Party", "Appears in the DTA as", "Available profiles", "Must choose"), rows)
}

# id / version / label of every profile the template's party slots offer.
.tgc_party_refs <- function(def, ctx) {
  profs <- unlist(lapply(.tgc_slots(def, ctx, "party"), function(slot) {
    lapply(ctx$g("party_profiles_for_slot")(ctx$profiles, slot), function(p) {
      c(
        .tgc_chr(p[["id"]]), .tgc_chr(p[["version"]]),
        .tgc_chr(p[["label"]], .tgc_chr(p[["id"]]))
      )
    })
  }), recursive = FALSE)
  out <- .tgc_df(.tgc_ref_cols, profs)
  out <- out[!duplicated(out[c("id", "version")]), , drop = FALSE]
  out <- out[order(out$label, out$id, out$version, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.tgc_term_choices <- function(def, ctx) {
  slots <- .tgc_slots(def, ctx, "vocabulary")
  rows <- lapply(slots, function(slot) {
    v <- tryCatch(ctx$vocab(slot$vocabulary), error = function(e) NULL)
    c(
      slot$label, .tgc_chr(slot$description, "\u2014"),
      .tgc_chr(v[["label"]], slot$vocabulary),
      if (identical(slot$mode, "open")) "Pick from the list or add your own" else "Pick from the list only",
      .tgc_join(slot$default), as.character(slot$min)
    )
  })
  .tgc_df(c("Choice", "Explanation", "Term list", "How it works", "Selected by default", "Minimum to select"), rows)
}

# base.metadata as Field / Value / Question rows, as the finished DTA will hold
# it before anyone answers a question: each option's pre-filled answer is laid
# over its target first (the same value the Questions table shows), a field a
# term-list choice fills is marked as chosen at creation, a party's fields are
# one row, and a field a question can change names that question in `Question`.
.tgc_prefilled <- function(def, ctx, annotate = TRUE) {
  g <- ctx$g
  md <- def[["base"]][["metadata"]]
  opts <- Filter(is.list, def[["options"]] %||% list())

  by_path <- list()
  shown <- md
  for (opt in opts) {
    label <- .tgc_chr(opt[["label"]], .tgc_humanise(.tgc_chr(opt[["id"]])))
    for (t in .dta_template_option_targets(opt)) {
      by_path[[length(by_path) + 1L]] <- c(path = t, label = label)
    }
    one <- g("dta_template_option_path")(opt)
    keys <- strsplit(one, ".", fixed = TRUE)[[1]]
    if (length(keys) >= 2 && identical(keys[[1]], "metadata")) {
      d <- .tgc_default_value(opt, md, g)
      if (!is.null(d)) {
        shown <- g("list_set_path")(if (is.list(shown)) shown else list(), keys[-1], d)
      }
    }
  }
  party_slots <- .tgc_slots(def, ctx, "party")
  vocab_slots <- .tgc_slots(def, ctx, "vocabulary")
  n_party <- length(party_slots)
  slot_targets <- c(
    vapply(party_slots, function(s) as.character(s$target), character(1)),
    vapply(vocab_slots, function(s) as.character(s$target %||% "")[[1]], character(1))
  )
  slot_labels <- c(
    vapply(party_slots, function(s) as.character(s$label), character(1)),
    vapply(vocab_slots, function(s) as.character(s$label), character(1))
  )
  covered_by <- function(path) {
    hit <- which(vapply(slot_targets, function(t) nzchar(t) && (identical(path, t) || startsWith(path, paste0(t, "."))), logical(1)))
    if (length(hit) == 0) NA_integer_ else hit[[1]]
  }
  overlap <- g("dta_template_paths_overlap")
  # The question that sets the document version, for the version history row.
  is_version <- vapply(by_path, function(b) identical(.tgc_clean_path(b[["path"]]), "metadata.version"), logical(1))
  version_q <- if (any(is_version)) by_path[[which(is_version)[[1]]]][["label"]] else ""

  rows <- list()
  party_done <- character(0)
  add <- function(prefix, value) {
    path <- paste(c("metadata", prefix), collapse = ".")
    question <- ""
    if (annotate) {
      hit <- covered_by(path)
      if (!is.na(hit) && hit <= n_party) {
        # One row per party, not one per field of the profile.
        lab <- slot_labels[[hit]]
        if (!lab %in% party_done) {
          party_done <<- c(party_done, lab)
          rows[[length(rows) + 1L]] <<- c(
            sprintf("%s: organisation and contacts", lab),
            sprintf("From the %s profile you choose", sub("^the ", "", tolower(lab))), ""
          )
        }
        return(invisible(NULL))
      }
      if (!is.na(hit)) {
        value <- sprintf("(chosen when the DTA is created: %s)", slot_labels[[hit]])
      } else {
        question <- paste(unique(vapply(
          Filter(function(b) overlap(path, b[["path"]]), by_path),
          function(b) b[["label"]], character(1)
        )), collapse = ", ")
      }
    }
    rows[[length(rows) + 1L]] <<- c(.tgc_field_name(prefix), value, question)
  }
  walk <- function(x, prefix) {
    name <- .tgc_field_name(prefix)
    last <- prefix[[length(prefix)]]
    if (identical(last, "version_history") && is.list(x) && length(x) > 0) {
      for (i in seq_along(x)) {
        e <- x[[i]]
        raw_v <- .tgc_chr(e[["version"]])
        raw_d <- .tgc_chr(e[["date"]])
        ver <- if (!nzchar(raw_v) || identical(raw_v, "${version}")) {
          if (nzchar(version_q)) sprintf("your answer to question '%s'", version_q) else "the version you enter"
        } else {
          raw_v
        }
        day <- if (identical(raw_d, "${today}")) "the day the DTA is created" else raw_d
        com <- .tgc_chr(e[["changes"]])
        rows[[length(rows) + 1L]] <<- c(
          if (i == 1L) paste0(name, " (first entry)") else sprintf("%s (entry %d)", name, i),
          paste(c(
            paste0("Version: ", ver),
            if (nzchar(day)) paste0("Date: ", day),
            if (nzchar(com)) paste0("Comment: ", com)
          ), collapse = " \u00b7 "),
          ""
        )
      }
    } else if (identical(last, "contacts") && is.list(x) && length(x) > 0) {
      add(prefix, paste(vapply(x, function(ct) {
        det <- c(.tgc_chr(ct[["role"]]), .tgc_chr(ct[["email"]]))
        det <- det[nzchar(det)]
        paste0(.tgc_chr(ct[["name"]], "(no name)"), if (length(det)) paste0(" (", paste(det, collapse = ", "), ")") else "")
      }, character(1)), collapse = "\n"))
    } else if (is.list(x) && length(x) == 0) {
      add(prefix, "(left empty)")
    } else if (is.list(x) && !is.null(names(x))) {
      for (n in names(x)) walk(x[[n]], c(prefix, n))
    } else {
      add(prefix, .tgc_value_text(x))
    }
  }
  if (is.list(shown)) {
    for (n in names(shown)) walk(shown[[n]], n)
  }
  .tgc_df(c("Field", "Value", "Question"), rows)
}

# Fixed paths cannot be changed by templates based on this one; required paths
# that nothing fills in must be answered by the person creating the DTA.
.tgc_locked <- function(def, ctx) {
  g <- ctx$g
  base_md <- def[["base"]][["metadata"]]
  opts <- Filter(is.list, def[["options"]] %||% list())
  one <- function(paths, rule) {
    paths <- unique(.tgc_clean_path(paths))
    lapply(paths, function(p) c(.tgc_path_text(p), rule))
  }
  open <- Filter(
    function(p) .tgc_required_open(def, p, opts, base_md, g),
    unique(.tgc_clean_path(def[["required"]]))
  )
  .tgc_df(c("Field", "Rule"), c(
    one(def[["sealed"]], "Fixed: templates based on this one cannot change it"),
    one(open, "Must be filled in when the DTA is created")
  ))
}


# Dataset id -> title lookup for the built datasets.
.tgc_titles <- function(datasets) {
  stats::setNames(
    vapply(datasets, function(d) d$title, character(1)),
    vapply(datasets, function(d) d$name, character(1))
  )
}

# ---- Changes versus the parent ------------------------------------------------------

.tgc_changes <- function(ref, parent_ref, def, datasets, ctx) {
  g <- ctx$g
  a <- g("template_history_build")(parent_ref, ctx$index)
  b <- g("template_history_build")(ref, ctx$index)
  if (!isTRUE(a$ok)) cli::cli_abort("{a$error}")
  if (!isTRUE(b$ok)) cli::cli_abort("{b$error}")
  d <- g("template_history_compare")(a$value, b$value)
  if (nrow(d) == 0) {
    return(.tgc_df(c("What", "Change")))
  }
  ds_titles <- .tgc_titles(datasets)
  labels <- c(
    stats::setNames(
      vapply(Filter(is.list, def[["options"]] %||% list()), function(o) .tgc_chr(o[["label"]], o[["id"]]), character(1)),
      vapply(Filter(is.list, def[["options"]] %||% list()), function(o) .tgc_chr(o[["id"]]), character(1))
    )
  )
  # A changed pre-filled answer also changes the metadata it fills in: say it once.
  def_rows <- d$area == "Template" & grepl(": default$", d$what)
  skip <- rep(FALSE, nrow(d))
  for (i in which(def_rows)) {
    j <- which(d$area == "Metadata" & !is.na(d$to) & d$to == d$to[[i]])
    if (length(j) > 0) {
      d$from[[i]] <- d$from[[j[[1]]]]
      d$change[[i]] <- "changed"
      skip[j] <- TRUE
    }
  }
  rows <- lapply(which(!skip), function(i) .tgc_change_row(d[i, , drop = FALSE], labels, ds_titles))
  .tgc_df(c("What", "Change"), Filter(Negate(is.null), rows))
}

.tgc_arrow <- function(from, to) {
  f <- if (is.na(from)) "" else from
  t <- if (is.na(to)) "" else to
  if (nzchar(f) && nzchar(t)) paste(f, "\u2192", t) else if (nzchar(t)) t else f
}

.tgc_change_row <- function(r, option_labels, ds_titles) {
  what <- r$what[[1]]
  change <- r$change[[1]]
  moved <- .tgc_arrow(r$from[[1]], r$to[[1]])
  whole <- switch(change,
    added = "Added",
    removed = "Removed",
    NULL
  )
  field_words <- c(
    default = "Pre-filled answer", label = "Question text", help = "Explanation",
    type = "Answer type", target = "Appears in the DTA as", choices = "Choices", values = "Allowed values",
    description = "Description"
  )
  field_text <- function(field) {
    key <- sub("[. >].*$", "", field)
    if (key %in% names(field_words)) field_words[[key]] else .tgc_humanise(gsub("[. >]+", " ", field))
  }
  detail <- function(field) {
    if (is.null(field) || !nzchar(field)) {
      return(whole %||% paste("Changed:", moved))
    }
    if (field == "description" && change == "changed") {
      now <- trimws(.tgc_chr(r$to[[1]]))
      return(if (nzchar(now) && nchar(now) <= 200) sprintf("Description now: \"%s\"", now) else "Description changed")
    }
    if (change == "changed") paste0(field_text(field), ": ", moved) else paste0(field_text(field), " ", tolower(whole), if (nzchar(moved)) paste0(": ", moved) else "")
  }

  if (identical(what, "Based on") || identical(what, "label")) {
    return(NULL)
  }
  if (identical(what, "description")) {
    return(NULL)
  }
  m <- regmatches(what, regexec("^(Option|Party slot|Vocabulary slot) '([^']*)'(?:: (.*))?$", what, perl = TRUE))[[1]]
  if (length(m) == 4) {
    nm <- if (m[[2]] == "Option" && m[[3]] %in% names(option_labels)) option_labels[[m[[3]]]] else m[[3]]
    kind <- c(Option = "Question", `Party slot` = "Party", `Vocabulary slot` = "Term choice")[[m[[2]]]]
    return(c(paste0(kind, ": ", nm), detail(m[[4]])))
  }
  m <- regmatches(what, regexec("^Dataset '([^']*)', column '([^']*)'(?:: (.*))?$", what, perl = TRUE))[[1]]
  if (length(m) == 4) {
    where <- if (m[[2]] %in% names(ds_titles)) ds_titles[[m[[2]]]] else m[[2]]
    return(c(sprintf("Column %s in %s", m[[3]], where), detail(m[[4]])))
  }
  m <- regmatches(what, regexec("^Dataset '([^']*)'(?:: (.*))?$", what, perl = TRUE))[[1]]
  if (length(m) == 3) {
    where <- if (m[[2]] %in% names(ds_titles)) ds_titles[[m[[2]]]] else m[[2]]
    return(c(paste("Dataset", where), detail(m[[3]])))
  }
  if (r$area[[1]] == "Metadata") {
    return(c(.tgc_humanise(gsub("[.>]+ *", " \u203a ", what)), if (change == "changed") paste0("Pre-filled value: ", moved) else detail(NULL)))
  }
  c(.tgc_humanise(what), detail(NULL))
}

# ---- Built datasets -----------------------------------------------------------------

.tgc_col_headers <- c("Column", "Type", "Can be empty", "Allowed values", "Description")
.tgc_rule_headers <- c("Rule ID", "Description")

# One list(id, rows) for the columns of a dataset. `notes` maps column id to a
# suffix line (term-list provenance) and is applied only when asked for.
.tgc_col_rows <- function(ds, notes = character(0)) {
  cols <- ds@specs@columns
  ids <- vapply(cols, function(cl) cl@id, character(1))
  rows <- lapply(cols, function(cl) {
    vals <- as.character(unlist(cl@values))
    pat <- .tgc_chr(cl@pattern)
    allowed <- if (length(vals) > 0) {
      paste(vals, collapse = "\n")
    } else if (nzchar(pat)) {
      paste("Must match pattern", pat)
    } else {
      "Any"
    }
    if (cl@id %in% names(notes)) {
      allowed <- paste0(allowed, "\n", notes[[cl@id]])
    }
    st <- cl@structure
    c(
      paste(c(cl@id, .tgc_chr(cl@label)[nzchar(.tgc_chr(cl@label))]), collapse = "\n"),
      paste(c(trimws(paste(.tgc_chr(st@backend), .tgc_chr(st@type))), .tgc_chr(st@format)[nzchar(.tgc_chr(st@format))]), collapse = "\n"),
      if (isTRUE(cl@nullable)) "Yes" else "No",
      allowed,
      .tgc_chr(cl@description)
    )
  })
  list(id = ids, rows = rows)
}

# One row per rule: the id and the full plain-English wording of the rule
# (see R/rulePhrasing.R). `lines` keeps the line layout so the Word table can
# bold the labels and set the lists as bullets.
.tgc_rule_rows <- function(ds) {
  rules <- ds@specs@rules
  list(
    id = vapply(rules, function(r) r@id, character(1)),
    rows = lapply(rules, function(r) c(r@id, .rule_text(r))),
    lines = lapply(rules, .rule_lines)
  )
}

.tgc_file_type <- function(f) {
  # The other handlers read as their upper-cased class name (CSV, TSV, ANY);
  # a SAS dataset uses the label the exported document uses.
  if (inherits(f, "DTAtools::DTAFileSAS7BDAT")) {
    return(.file_format_label(f))
  }
  cls <- sub("^DTAtools::DTAFile", "", class(f)[[1]])
  toupper(cls)
}

.tgc_about <- function(ds, n_cols, n_rules) {
  files <- ds@files
  pattern <- any(vapply(files, function(f) isTRUE(f@pattern), logical(1)))
  names_ <- vapply(files, function(f) {
    shown <- if (isTRUE(f@pattern)) .tgc_chr(f@pattern_description) else ""
    if (nzchar(shown)) shown else .tgc_chr(f@filename)
  }, character(1))
  kv <- list(
    .tgc_join(names_),
    .tgc_join(unique(vapply(files, .tgc_file_type, character(1))), ", "),
    as.character(n_cols), as.character(n_rules)
  )
  names(kv) <- c(if (pattern) "File name pattern" else "File name", "File type", "Columns", "Rules")
  do.call(.tgc_kv, kv)
}

# The dataset template (as a DTADataSet) a built dataset came from, or NULL.
.tgc_template_dataset <- function(ds, ctx, overrides = list()) {
  src <- .tgc_chr(ds@template_source)
  if (!nzchar(src)) {
    return(NULL)
  }
  ver <- .tgc_chr(ds@template_version)
  hit <- ctx$g("resolve_template_ref")(
    ctx$index, if (nzchar(ver)) paste0(src, "@", ver) else src, kind = "dta_dataset_template"
  )
  if (is.null(hit)) {
    return(NULL)
  }
  built <- .tgc_build_dataset_template(hit$path[[1]], ctx, ds@id, overrides)
  if (is.null(built)) {
    NULL
  } else {
    list(ds = built, label = hit$label[[1]], version = hit$version[[1]])
  }
}

# `as_name` and `overrides` make the template side go through the same
# term-list narrowing as the dataset the creation template built, so a narrowed
# term list is not reported as a change.
.tgc_build_dataset_template <- function(path, ctx, as_name = NULL, overrides = list()) {
  g <- ctx$g
  rd <- g("read_dataset_template")(path)
  if (!isTRUE(rd$ok)) {
    return(NULL)
  }
  b <- g("build_dataset_from_template")(
    rd$value,
    selections = list(), as_name = as_name, resolve_vocab = ctx$vocab
  )
  if (!isTRUE(b$ok)) {
    return(NULL)
  }
  plain <- tryCatch(g("apply_vocabulary_slot_overrides")(b$value$dataset, overrides), error = function(e) b$value$dataset)
  tryCatch(dta_dataset_from_list(plain), error = function(e) NULL)
}

# "Added" when the id is not in the template, "Changed" when its row differs.
.tgc_change_marks <- function(ids, keys, tpl_ids, tpl_keys) {
  hit <- match(ids, tpl_ids)
  ifelse(is.na(hit), "Added", ifelse(keys == tpl_keys[hit], "", "Changed"))
}

.tgc_built_datasets <- function(dta, variant, ctx, overrides = list()) {
  unname(lapply(dta@datasets, function(ds) {
    cols <- .tgc_col_rows(ds)
    rules <- .tgc_rule_rows(ds)
    cmark <- rep("", length(cols$id))
    rmark <- rep("", length(rules$id))
    removed <- character(0)
    removed_rules <- character(0)
    source <- ""
    source_version <- ""
    tpl <- .tgc_template_dataset(ds, ctx, overrides)
    if (!is.null(tpl)) {
      source <- tpl$label
      source_version <- tpl$version
      tc <- .tgc_col_rows(tpl$ds)
      tr <- .tgc_rule_rows(tpl$ds)
      key <- function(x) vapply(x$rows, paste, character(1), collapse = "\r")
      cmark <- .tgc_change_marks(cols$id, key(cols), tc$id, key(tc))
      removed <- setdiff(tc$id, cols$id)
      # The key is the wording itself: any change a reader can see in a rule
      # (a condition, a limit, the purpose) shows up as "Changed".
      rkey <- function(x) vapply(x$rows, `[[`, character(1), 2)
      rmark <- .tgc_change_marks(rules$id, rkey(rules), tr$id, rkey(tr))
      removed_rules <- setdiff(tr$id, rules$id)
    }
    list(
      name = ds@id, title = .tgc_chr(ds@name, ds@id), variant = variant,
      about = .tgc_about(ds, length(cols$id), length(rules$id)),
      source = source, source_version = source_version,
      columns = .tgc_df(c(.tgc_col_headers, "Change"), Map(c, cols$rows, cmark)),
      removed_columns = removed,
      rules = .tgc_df(c(.tgc_rule_headers, "Change"), Map(c, rules$rows, rmark)),
      rule_lines = rules$lines,
      removed_rules = removed_rules
    )
  }))
}

# Which ids to document: the newest version of every non-abstract creation
# template, or of the ids named in `templates`. Returns the row index into
# guide$creation of each chosen entry, named by id, so a renderer can pick
# the entry itself. A named abstract template aborts.
.tgc_select_ids <- function(guide, templates = NULL) {
  ents <- guide$creation
  ids <- vapply(ents, function(e) e$id, character(1))
  vers <- vapply(ents, function(e) e$version, character(1))
  abstract <- vapply(ents, function(e) isTRUE(e$abstract), logical(1))
  newest <- vapply(seq_along(ents), function(i) {
    same <- which(ids == ids[[i]])
    same[[which.max(.tgc_rank(vers[same]))]] == i
  }, logical(1))
  want <- if (is.null(templates)) {
    ids[newest & !abstract]
  } else {
    templates <- unique(as.character(templates))
    unknown <- setdiff(templates, ids)
    if (length(unknown) > 0) {
      cli::cli_abort(
        "Unknown template id{?s}: {.val {unknown}}.",
        class = "dta_template_guide_unknown"
      )
    }
    for (id in templates) {
      if (isTRUE(abstract[newest & ids == id])) {
        based <- vapply(ents, function(e) id %in% c(e$lineage$id, e$mixins$id), logical(1))
        users <- vapply(ents[!abstract & newest & based], function(e) e$title, character(1))
        users <- sort(unique(users), method = "radix")
        cli::cli_abort(
          c(
            "{.val {id}} is a building block and cannot be documented on its own.",
            "i" = if (length(users) > 0) "Templates based on it: {.val {users}}."
          ),
          class = "dta_template_guide_abstract"
        )
      }
    }
    templates
  }
  pos <- which(newest)
  stats::setNames(pos[match(want, ids[pos])], want)
}

# ---- Dataset templates ----------------------------------------------------------------

.tgc_dataset_template <- function(row, rec, ctx) {
  g <- ctx$g
  rd <- g("read_dataset_template")(row$path)
  if (!isTRUE(rd$ok)) cli::cli_abort("{rd$error}")
  def <- rd$value
  ds <- .tgc_build_dataset_template(row$path, ctx)

  # Column provenance: which term list feeds a column's allowed values.
  notes <- character(0)
  for (cl in def[["dataset"]][["columns"]] %||% list()) {
    vf <- cl[["values_from"]]
    ref <- if (is.list(vf)) .tgc_chr(vf[["vocabulary"]]) else .tgc_chr(vf)
    if (nzchar(ref)) {
      v <- tryCatch(ctx$vocab(ref), error = function(e) NULL)
      notes[[.tgc_chr(cl[["id"]])]] <- sprintf("(from term list '%s')", .tgc_chr(v[["label"]], ref))
    }
  }

  note <- ""
  if (is.null(ds)) {
    note <- "A sample dataset could not be built from this template; the columns below are read straight from the file."
    ctx$problem(row$path, note)
    raw <- def[["dataset"]][["columns"]] %||% list()
    cols <- .tgc_df(.tgc_col_headers, lapply(raw, function(cl) {
      c(
        paste(c(.tgc_chr(cl[["id"]]), .tgc_chr(cl[["label"]])[nzchar(.tgc_chr(cl[["label"]]))]), collapse = "\n"),
        paste(c(.tgc_chr(cl[["type"]]), sub("^\\S+ ", "", .tgc_chr(cl[["format"]]))[nzchar(.tgc_chr(cl[["format"]]))]), collapse = "\n"),
        if (isTRUE(cl[["nullable"]])) "Yes" else "No",
        .tgc_join(cl[["values"]], "\n", "Any"), .tgc_chr(cl[["description"]])
      )
    }))
    rules <- .tgc_df(.tgc_rule_headers)
    about <- .tgc_kv(Columns = as.character(nrow(cols)))
    nrules <- 0L
  } else {
    cr <- .tgc_col_rows(ds, notes)
    cols <- .tgc_df(.tgc_col_headers, cr$rows)
    rr <- .tgc_rule_rows(ds)
    rules <- .tgc_df(.tgc_rule_headers, rr$rows)
    about <- .tgc_about(ds, length(cr$id), length(rr$id))
    nrules <- length(rr$id)
  }
  about_v <- stats::setNames(about$value, about$key)
  title_ds <- .tgc_chr(def[["dataset"]][["name"]], .tgc_chr(def[["dataset"]][["id"]], row$label))

  opts <- Filter(is.list, def[["options"]] %||% list())
  settings <- .tgc_df(
    c("Setting", "Answer type", "Pre-filled answer", "Appears in the DTA as", "Explanation"),
    lapply(opts, function(opt) {
      .tgc_question_row(opt, NULL, g)[c(1, 2, 4, 5, 6)]
    })
  )

  users <- .tgc_users(ctx, row$id, "dta_creation_template")
  list(
    title = row$label, id = row$id, version = row$version,
    description = .tgc_chr(def[["description"]]),
    summary = sprintf(
      "Defines the dataset '%s' with %s columns and %s. Used by %s.",
      title_ds, about_v[["Columns"]], .tgc_plural(nrules, "rule"),
      if (length(users) == 0) "no DTA template" else .tgc_plural(length(users), "DTA template")
    ),
    at_a_glance = do.call(.tgc_kv, c(
      list(`Dataset name` = title_ds),
      as.list(stats::setNames(
        about$value[about$key %in% c("File name", "File name pattern", "File type")],
        about$key[about$key %in% c("File name", "File name pattern", "File type")]
      )),
      list(`Used by` = .tgc_join(users, ", "), Version = row$version, Date = .tgc_date(rec$def))
    )),
    settings = settings, columns = cols, rules = rules,
    build_note = note, technical = .tgc_technical(row)
  )
}

# ---- Term lists and party profiles ------------------------------------------------------

.tgc_vocabulary <- function(row, rec, ctx) {
  v <- ctx$vocab(paste0(row$id, "@", row$version))
  if (is.null(v)) cli::cli_abort("The term list could not be resolved.")
  terms <- Filter(is.list, v[["terms"]] %||% list())
  term_df <- .tgc_df(c("Code", "Label", "Description"), lapply(terms, function(t) {
    c(.tgc_chr(t[["code"]]), .tgc_chr(t[["label"]], .tgc_chr(t[["code"]])), .tgc_chr(t[["description"]], "\u2014"))
  }))
  cols <- vapply(rec$def[["columns"]] %||% list(), function(cl) {
    .tgc_chr(if (is.list(cl)) cl[["id"]] else cl)
  }, character(1))
  parent <- .tgc_chr(rec$def[["extends"]])
  users <- .tgc_users(ctx, row$id, c("dta_creation_template", "dta_dataset_template"))
  list(
    title = row$label, id = row$id, version = row$version,
    description = .tgc_chr(rec$def[["description"]]),
    summary = sprintf("A list of %s.", .tgc_plural(nrow(term_df), "term")),
    at_a_glance = .tgc_kv(
      `Based on` = if (nzchar(parent)) .tgc_vocab_label(ctx, parent) else "\u2014",
      `Used for columns` = .tgc_join(cols, ", "),
      `Used by` = .tgc_join(users, ", "),
      Version = row$version
    ),
    terms = term_df, technical = .tgc_technical(row)
  )
}

# Label of a vocabulary reference ("id" or "id@version"), else the reference itself.
.tgc_vocab_label <- function(ctx, ref) {
  hit <- ctx$g("resolve_template_ref")(ctx$index, ref, kind = "dta_vocabulary")
  if (is.null(hit)) sub("@.*$", "", ref) else hit$label[[1]]
}

.tgc_party <- function(row, rec, ctx) {
  def <- rec$def
  aff <- def[["affiliation"]] %||% list()
  contacts <- Filter(is.list, def[["contacts"]] %||% list())
  role <- .tgc_chr(def[["role"]])
  list(
    title = row$label, id = row$id, version = row$version,
    description = .tgc_chr(def[["description"]]),
    summary = "",
    at_a_glance = .tgc_kv(
      Role = switch(role,
        supplier = "Supplier",
        receiver = "Receiver",
        any = "Either",
        .tgc_humanise(role)
      ),
      Organisation = .tgc_chr(aff[["name"]], "\u2014"),
      Address = .tgc_chr(aff[["address"]], "\u2014"),
      Country = .tgc_chr(aff[["country"]], "\u2014")
    ),
    contacts = .tgc_df(c("Name", "Role", "Email", "Phone", "Signs"), lapply(contacts, function(ct) {
      c(
        .tgc_chr(ct[["name"]], "\u2014"), .tgc_chr(ct[["role"]], "\u2014"),
        .tgc_chr(ct[["email"]], "\u2014"), .tgc_chr(ct[["phone"]], "\u2014"),
        if (isTRUE(ct[["signature"]])) "Yes" else "\u2014"
      )
    })),
    technical = .tgc_technical(row)
  )
}
