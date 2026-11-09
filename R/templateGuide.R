#' Write a readable guide for each template of a library
#'
#' @title Write a readable guide for each template of a library
#'
#' @description
#' Turns the DTA templates of a template library into Word (or PDF) documents
#' that anyone can read, without starting the DTAtools app and without reading
#' YAML. One document is written per usable template. It says what the template
#' is for, what it asks the person creating a DTA, what it fills in
#' automatically, what is fixed, and which datasets, columns and rules it
#' produces. The term lists and party profiles the template offers are
#' described in the same document. Every page after the cover carries a running
#' header and footer with the template name, its version and "Page X of Y".
#'
#' Building blocks (abstract templates) get no document of their own: what they
#' contribute is part of the guide of every template based on them.
#'
#' @param path The template library: its root folder (the one holding
#'   `templates/`, `parties/` and `vocabulary/`), or a single template file
#'   inside one of those folders (the guide then covers that template only).
#' @param dir Folder to write the guides to. It is created when missing.
#' @param templates Optional template ids. `NULL` (default) writes a guide for
#'   the newest version of every usable, non-abstract DTA template. An id that
#'   names a building block is an error.
#' @param format `"docx"` (default) or `"pdf"`. PDF needs a converter; see
#'   [dta_pdf_backend()]. Only Microsoft Word and LibreOffice keep the running
#'   header, the footer and the column widths; the other routes re-typeset the
#'   document, and a warning says so.
#' @param organisation,organisation_unit Name of the organisation (and unit)
#'   shown on the cover and in the page header. `NULL` (default) uses the one
#'   set in the library's `dta-app.yaml`, if any; `""` shows none.
#' @param overwrite Replace guides that already exist? Default `FALSE`.
#' @param quiet Suppress the confirmation message? Default `FALSE`.
#'
#' @return A named character vector, invisibly: the template ids are the names,
#'   the paths of the written guides the values. The files are named
#'   `<id>_v<version>_template_guide.<format>`.
#'
#' @seealso [validate_template()], [write_dta()], [dta_pdf_backend()]
#'
#' @export
#' @examples
#' out_dir <- file.path(tempdir(), "template-guides")
#' write_template_guide(
#'   system.file("extdata", "library", package = "DTAtools"),
#'   out_dir,
#'   templates = "biomarker_gf",
#'   quiet = TRUE
#' )
#' list.files(out_dir)
write_template_guide <- function(
  path,
  dir,
  templates = NULL,
  format = c("docx", "pdf"),
  organisation = NULL,
  organisation_unit = NULL,
  overwrite = FALSE,
  quiet = FALSE
) {
  format <- if (length(format) == 0) "docx" else tolower(format[[1]])
  if (!format %in% c("docx", "pdf")) {
    cli::cli_abort(
      "'format' must be 'docx' or 'pdf'.",
      class = "dta_template_guide_format"
    )
  }
  if (!is.character(dir) || length(dir) != 1L || is.na(dir) || !nzchar(dir)) {
    cli::cli_abort("{.arg dir} must be a single folder path.")
  }
  # NULL means the library's value, "" means none (what write_dta() would call
  # blank is therefore kept as "").
  organisation <- .tg_org_arg(organisation, "organisation")
  organisation_unit <- .tg_org_arg(organisation_unit, "organisation_unit")

  guide <- .template_guide_collect(path, templates = templates)
  chosen <- .tg_select(guide, path, templates)

  ids <- vapply(chosen, function(e) e[["id"]], character(1))
  files <- vapply(chosen, function(e) {
    .tg_file_name(e[["id"]], e[["version"]], format)
  }, character(1))
  # Windows and macOS file systems ignore case: "A.docx" and "a.docx" collide.
  dup <- unique(files[duplicated(tolower(basename(files)))])
  if (length(dup) > 0) {
    cli::cli_abort(
      "Two templates would be written to the same file: {.file {dup}}.",
      class = "dta_template_guide_exists"
    )
  }
  targets <- file.path(dir, files)
  clash <- targets[file.exists(targets)]
  if (length(clash) > 0 && !isTRUE(overwrite)) {
    cli::cli_abort(
      c(
        "{length(clash)} guide file{?s} already exist{?s/} in {.path {dir}}.",
        stats::setNames(basename(clash), rep("*", length(clash))),
        i = "Set {.code overwrite = TRUE} to replace."
      ),
      class = "dta_template_guide_exists"
    )
  }

  stubs <- vapply(chosen, function(e) .tg_has(e[["build_note"]]), logical(1))
  if (any(stubs)) {
    cli::cli_warn(
      "Could not describe {.val {ids[stubs]}} completely; the guide lists the problem.",
      class = "dta_template_guide_problems"
    )
  }
  if (format == "pdf") {
    backend <- dta_pdf_backend()
    if (!is.null(backend) && backend$name %in% c("tinytex", "pandoc")) {
      cli::cli_warn(
        c(
          "The {backend$name} route re-typesets the document: the running header, the footer and the column widths are lost.",
          i = "Install LibreOffice or Microsoft Word for a faithful PDF."
        ),
        class = "dta_template_guide_pdf_layout"
      )
    }
  }

  lib <- guide[["library"]]
  org <- if (is.null(organisation)) .tg_chr(lib[["organisation"]]) else .tg_chr(organisation)
  unit <- if (!is.null(organisation_unit)) {
    .tg_chr(organisation_unit)
  } else if (is.null(organisation)) {
    .tg_chr(lib[["organisation_unit"]])
  } else {
    ""
  }

  # Build every document first (no I/O): a rendering failure leaves no files.
  docs <- lapply(chosen, function(e) {
    .template_guide_docx(guide, e, organisation = org, organisation_unit = unit)
  })

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  existed <- file.exists(targets)
  done <- FALSE
  temp_docx <- tempfile(fileext = ".docx")
  on.exit(
    {
      unlink(temp_docx, force = TRUE)
      # A failed run removes what it created, never what was already there.
      if (!done) unlink(targets[!existed], force = TRUE)
    },
    add = TRUE
  )
  for (k in seq_along(chosen)) {
    if (format == "docx") {
      .dta_write_staged(targets[[k]], function(stage) print(docs[[k]], target = stage))
    } else {
      print(docs[[k]], target = temp_docx)
      .convert_docx_to_pdf(temp_docx, targets[[k]])
    }
  }
  done <- TRUE

  if (!isTRUE(quiet)) {
    cli::cli_alert_success("Wrote {length(targets)} template guide{?s} to {.path {dir}}")
  }
  invisible(stats::setNames(targets, ids))
}

# ---- selection --------------------------------------------------------------

# NULL stays NULL (use the library's value); anything else is validated like
# write_dta() does, and a blank value becomes "" (show none).
.tg_org_arg <- function(x, arg, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }
  .check_export_text(x, arg, call = call) %||% ""
}

.tg_chr <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[[1]])) "" else as.character(x[[1]])
}
.tg_n <- function(x) if (is.data.frame(x)) nrow(x) else 0L
.tg_has <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
.tg_empty <- function(x) is.na(x) | !nzchar(trimws(x)) | x == "\u2014"

.tg_file_name <- function(id, version, ext) {
  base <- paste0(id, if (nzchar(version %||% "")) paste0("_v", version) else "", "_template_guide")
  paste0(gsub("[^A-Za-z0-9._-]", "_", base), ".", ext)
}

# The creation entries to write a document for (see .tgc_select_ids): the
# requested ids or every non-abstract template, newest version only, never the
# parents and building blocks the collector pulled in. A single template file
# selects the template it holds.
.tg_select <- function(guide, path, templates) {
  if (is.null(templates) && is.character(path) && length(path) == 1L &&
    file.exists(path) && !dir.exists(path)) {
    # That file's own entry, whatever other versions of the id exist.
    hit <- Filter(function(e) identical(.tg_chr(e[["file"]]), basename(path)), guide[["creation"]])
    if (length(hit) > 0L && isTRUE(hit[[1]][["abstract"]])) {
      cli::cli_abort(
        "{.val {hit[[1]][['id']]}} is a building block and cannot be documented on its own.",
        class = "dta_template_guide_abstract"
      )
    }
    if (length(hit) > 0L) {
      return(unname(hit[1]))
    }
    templates <- character(0)
  }
  pos <- .tgc_select_ids(guide, templates)
  if (length(pos) == 0) {
    cli::cli_abort(
      "No usable DTA template found in {.path {path}}.",
      class = "dta_template_guide_no_templates"
    )
  }
  unname(guide[["creation"]][pos])
}

# ---- text and paragraph helpers --------------------------------------------

.tg_ft <- function(text, size = FONT_SIZES$body, bold = FALSE, italic = FALSE,
                   color = THEME_COLORS$gray_dark, font = FONTS$primary) {
  officer::ftext(text, .house_fp_text(size = size, bold = bold, italic = italic, color = color, font = font))
}

# A body paragraph; `keep_next` glues it to what follows (lead-ins).
.tg_par <- function(doc, text, size = FONT_SIZES$body, bold = FALSE, italic = FALSE,
                    color = THEME_COLORS$gray_dark, keep_next = FALSE, after = 4, before = 0) {
  officer::body_add_fpar(doc, officer::fpar(
    .tg_ft(text, size, bold, italic, color),
    fp_p = officer::fp_par(
      padding.bottom = after, padding.top = before, keep_with_next = keep_next
    )
  ))
}

# The 6 pt gap between a table and what follows.
.tg_gap <- function(doc) {
  officer::body_add_fpar(doc, officer::fpar(
    officer::ftext("", .house_fp_text(size = 4)),
    fp_p = officer::fp_par(padding.bottom = 2)
  ))
}

.tg_first_sentence <- function(text) {
  text <- trimws(gsub("\\s+", " ", text))
  sub("^(.*?[.!?])(\\s.*)?$", "\\1", text, perl = TRUE)
}

.tg_lines <- function(x) strsplit(x, "\n", fixed = TRUE)[[1]]

# ---- table helpers ----------------------------------------------------------

# Rows never split; the header repeats; a table of at most 15 rows (or a card
# that fits in `max_lines`) is kept on one page.
.tg_props <- function(ft, keep = TRUE) {
  ft <- flextable::set_table_properties(ft,
    layout = "fixed",
    opts_word = list(split = FALSE, repeat_headers = TRUE)
  )
  if (keep) {
    ft <- flextable::paginate(ft, init = TRUE, hdr_ftr = TRUE)
    # The last row must not chain to whatever follows the table.
    ft$body$styles$pars$keep_with_next$data[nrow(ft$body$styles$pars$keep_with_next$data), ] <- FALSE
  }
  ft
}

.tg_add_table <- function(doc, ft) {
  doc <- flextable::body_add_flextable(doc, ft, align = "left")
  .tg_gap(doc)
}

.tg_clean_df <- function(df) {
  df[] <- lapply(df, function(col) {
    col <- as.character(col)
    col[is.na(col)] <- ""
    col
  })
  df
}

# A data.frame as a house-style table spanning `width` exactly.
.tg_table <- function(doc, df, widths, width = TEXT_WIDTH$portrait, pad = 4) {
  if (.tg_n(df) == 0L) {
    return(doc)
  }
  df <- .tg_clean_df(df)
  ft <- .style_table(flextable::flextable(df))
  ft <- flextable::padding(ft, padding.top = pad, padding.bottom = pad, part = "all")
  ft <- flextable::width(ft, j = seq_len(ncol(df)), width = widths)
  ft <- .fit_table_width(ft, width)
  ft <- .tg_props(ft, keep = nrow(df) <= 15)
  .tg_add_table(doc, ft)
}

# Key/value facts; rows with an empty value are dropped.
.tg_kv <- function(doc, keys, values, width = TEXT_WIDTH$portrait, label_width = 1.9) {
  values <- as.character(values)
  keep <- !is.na(values) & nzchar(trimws(values))
  if (!any(keep)) {
    return(doc)
  }
  ft <- .kv_table(data.frame(key = keys[keep], value = values[keep]), width = width, label_width = label_width)
  ft <- .tg_props(ft, keep = sum(keep) <= 15)
  .tg_add_table(doc, ft)
}

.tg_chunk <- function(text, size = 9, bold = FALSE, italic = FALSE,
                      color = THEME_COLORS$gray_dark, font = FONTS$primary) {
  flextable::as_chunk(text, props = .house_fp_text(size = size, bold = bold, italic = italic, color = color, font = font))
}

# Join paragraph pieces with line breaks (a "\n" chunk is a Word line break).
.tg_paragraph <- function(pieces, size = 9) {
  out <- list()
  for (k in seq_along(pieces)) {
    if (k > 1) out <- c(out, list(.tg_chunk("\n", size)))
    out <- c(out, pieces[[k]])
  }
  do.call(flextable::as_paragraph, out)
}

.tg_change_tag <- function(change) {
  switch(change,
    Added = "Added by this template",
    Changed = "Changed by this template",
    ""
  )
}

# The first cell of a column/rule row: ID in bold, label below, change tag last.
.tg_id_cell <- function(id, label, change) {
  pieces <- list(list(.tg_chunk(id, bold = TRUE)))
  if (nzchar(label)) pieces <- c(pieces, list(list(.tg_chunk(label))))
  tag <- .tg_change_tag(change)
  if (nzchar(tag)) {
    pieces <- c(pieces, list(list(.tg_chunk(tag, italic = TRUE, color = THEME_COLORS$primary))))
  }
  .tg_paragraph(pieces)
}

# "Allowed values": a pattern is shown in a monospace run, lists one per line.
.tg_allowed_cell <- function(text) {
  pat <- "^Must match pattern "
  if (grepl(pat, text)) {
    return(.tg_paragraph(list(list(
      .tg_chunk("Pattern: "),
      .tg_chunk(sub(pat, "", text), font = FONTS$monospace)
    ))))
  }
  lines <- .tg_lines(text)
  if (length(lines) == 0) lines <- ""
  .tg_paragraph(lapply(lines, function(l) list(.tg_chunk(l))))
}

# ---- cards ------------------------------------------------------------------

# Rendered line count of a card body (a rough wrap estimate at 9 pt).
.tg_card_lines <- function(values, chars = 70) {
  sum(vapply(values, function(v) {
    sum(pmax(1, ceiling(nchar(.tg_lines(v)) / chars)))
  }, numeric(1)))
}

# One boxed two-column card: a tinted header (left, right) and label/value rows.
.tg_card <- function(doc, left, right, required, rows, width = TEXT_WIDTH$portrait, label_width = 1.6) {
  rows <- rows[!.tg_empty(rows$value), , drop = FALSE]
  if (nrow(rows) == 0L) {
    rows <- data.frame(label = "", value = "", stringsAsFactors = FALSE)
  }
  body <- data.frame(a = rows$label, b = rows$value, stringsAsFactors = FALSE)
  ft <- flextable::flextable(body)
  ft <- flextable::set_header_labels(ft, a = left, b = right)
  ft <- flextable::font(ft, fontname = FONTS$primary, part = "all")
  ft <- flextable::fontsize(ft, size = 9, part = "all")
  ft <- flextable::color(ft, color = THEME_COLORS$gray_dark, part = "body")
  ft <- flextable::color(ft, j = 1, color = THEME_COLORS$gray_mid, part = "body")
  ft <- flextable::bold(ft, j = 1, bold = TRUE, part = "body")
  ft <- flextable::bg(ft, bg = THEME_COLORS$white, part = "body")
  ft <- flextable::bg(ft, j = 1, bg = THEME_COLORS$gray_light, part = "body")
  ft <- flextable::bg(ft, bg = THEME_COLORS$primary_light, part = "header")
  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft <- flextable::align(ft, align = "left", part = "all")
  ft <- flextable::padding(ft,
    padding.top = 3, padding.bottom = 3, padding.left = 6, padding.right = 6, part = "all"
  )
  ft <- flextable::border_remove(ft)
  ft <- flextable::border_inner_h(ft, border = officer::fp_border(color = THEME_COLORS$gray_border, width = 0.5), part = "body")
  ft <- flextable::border_outer(ft, border = officer::fp_border(color = THEME_COLORS$gray_border, width = 0.5), part = "all")
  ft <- flextable::hline_bottom(ft, border = officer::fp_border(color = THEME_COLORS$primary_dark, width = 1), part = "header")
  ft <- flextable::compose(ft,
    part = "header", j = 1,
    value = flextable::as_paragraph(.tg_chunk(left, 9, bold = TRUE, color = THEME_COLORS$primary_dark))
  )
  head_right <- list(.tg_chunk(right, 10, bold = TRUE, color = THEME_COLORS$primary_dark))
  if (isTRUE(required)) {
    head_right <- c(head_right, list(
      .tg_chunk("  \u00b7  Must be answered", 9, bold = TRUE, color = THEME_COLORS$red_error)
    ))
  }
  ft <- flextable::compose(ft, part = "header", j = 2, value = do.call(flextable::as_paragraph, head_right))
  ft <- flextable::width(ft, j = 1, width = label_width)
  ft <- flextable::width(ft, j = 2, width = width - label_width)
  ft <- .tg_props(ft, keep = .tg_card_lines(rows$value) <= 25)
  .tg_add_table(doc, ft)
}

.tg_card_rows <- function(...) {
  v <- c(...)
  data.frame(label = names(v), value = unname(as.character(v)), stringsAsFactors = FALSE)
}

# Choices, one per line; more than 12 are cut to 10 plus "... and N more".
.tg_choices <- function(text) {
  lines <- .tg_lines(text)
  if (length(lines) > 12) {
    lines <- c(paste0("\u2022 ", lines[1:10]), paste0("\u2026 and ", length(lines) - 10, " more"))
  } else {
    lines <- paste0("\u2022 ", lines)
  }
  paste(lines, collapse = "\n")
}

# ---- glossary ---------------------------------------------------------------

.tg_glossary <- function(e) {
  has <- function(x) .tg_n(x) > 0L
  flags <- if (has(e[["questions"]])) e[["questions"]][["Must answer / fixed"]] else character(0)
  fixed <- has(e[["locked"]]) || any(grepl("Fixed", flags, fixed = TRUE))
  all <- list(
    list("DTA template", "A recipe for a Data Transfer Agreement (DTA). It asks a few questions and creates a complete DTA from the answers.", TRUE),
    list(
      "Based on",
      if (identical(e[["changes_status"]], "compared")) {
        "The template this one starts from. It keeps everything of the original except what is listed under 'How it differs'."
      } else {
        "The template this one starts from."
      },
      .tg_has(e[["parent"]]) || has(e[["lineage"]])
    ),
    list("Also includes", "A building block that adds shared settings to this template.", has(e[["mixins"]])),
    list("Question", "Something the person creating the DTA is asked, for example 'Who is the supplier?'.", has(e[["questions"]])),
    list("Pre-filled answer", "The answer already entered when the question appears. You can change it.", has(e[["questions"]])),
    list("Appears in the DTA as", "Where the answer ends up in the finished DTA, for example 'Transmission \u203a Frequency'.", has(e[["questions"]])),
    list("Must be answered", "A question that cannot be left empty before the DTA can be created.", any(grepl("Must answer", flags, fixed = TRUE))),
    list("Fixed", "A value that cannot be changed when the DTA is created.", fixed),
    list("Variant", "A choice that changes which datasets, columns or rules the DTA gets.", has(e[["variants"]])),
    list("Dataset", "One data table that is transferred, usually one file.", length(e[["datasets"]]) > 0),
    list("Column", "One field of a dataset, such as a subject identifier or a date.", length(e[["datasets"]]) > 0),
    list("Type", "How the value is stored, for example 'SAS Char $50.' = text of up to 50 characters; 'SAS Num' = a number.", length(e[["datasets"]]) > 0),
    list("Required", "Yes means every row needs a value; No means a value may be left blank.", length(e[["datasets"]]) > 0),
    list("Allowed values", "The only values a column may contain. 'Any' means no restriction.", length(e[["datasets"]]) > 0),
    list("Rule", "A check the data must pass, for example that a value is within a range or that no row is repeated.", any(vapply(e[["datasets"]], function(d) .tg_n(d[["rules"]]) > 0L, logical(1)))),
    list("Term list", "A fixed list of codes and their meanings that questions or columns pick from.", has(e[["term_choices"]]) || has(e[["refs"]][["vocabularies"]])),
    list("Party profile", "A saved organisation (address and contact people) that can be chosen as supplier or receiver.", has(e[["parties"]]) || has(e[["refs"]][["parties"]]))
  )
  all <- Filter(function(g) isTRUE(g[[3]]), all)
  data.frame(
    key = vapply(all, function(g) g[[1]], character(1)),
    value = vapply(all, function(g) g[[2]], character(1)),
    stringsAsFactors = FALSE
  )
}

# ---- sections ---------------------------------------------------------------

.tg_ref_labels <- function(df, sep = " \u203a ") {
  if (.tg_n(df) == 0L) "" else paste(df[["label"]], collapse = sep)
}

.tg_ref_ids <- function(df) {
  if (.tg_n(df) == 0L) "" else paste0(df[["id"]], " ", df[["version"]], collapse = "\n")
}

# The inheritance chain as one line, nearest parent first.
.tg_chain_ids <- function(df) {
  if (.tg_n(df) == 0L) "" else .tgc_based_text(paste(df[["id"]], df[["version"]]))
}

.tg_questions_text <- function(e) {
  n <- e[["option_count"]] %||% 0L
  if (n == 0L) n <- .tg_n(e[["questions"]])
  req <- e[["required_count"]] %||% 0L
  if (n == 0L) {
    return("")
  }
  if (req > 0L) paste0(n, ", of which ", req, " must be answered") else as.character(n)
}

.tg_overview <- function(doc, e) {
  doc <- .add_heading(doc, "Overview", 1)
  if (.tg_has(e[["summary"]])) doc <- .tg_par(doc, e[["summary"]], bold = TRUE, keep_next = TRUE)
  if (.tg_has(e[["description"]])) doc <- .tg_par(doc, e[["description"]])
  if (.tg_has(e[["build_note"]])) {
    doc <- .tg_par(doc, e[["build_note"]], italic = TRUE, color = THEME_COLORS$gray_mid)
  }
  based <- if (.tg_n(e[["lineage"]]) > 0L) .tgc_based_text(e[["lineage"]][["label"]]) else .tg_chr(e[["parent"]])
  parties <- if (.tg_n(e[["parties"]]) > 0L) paste(e[["parties"]][["Party"]], collapse = ", ") else ""
  terms <- if (.tg_n(e[["term_choices"]]) > 0L) paste(e[["term_choices"]][["Term list"]], collapse = ", ") else ""
  variants <- if (.tg_n(e[["variants"]]) > 0L) paste(unique(e[["variants"]][["Question"]]), collapse = ", ") else ""
  ds <- vapply(e[["datasets"]], function(d) d[["title"]], character(1))
  glance <- c(
    `Based on` = based,
    `Also includes` = .tg_ref_labels(e[["mixins"]], ", "),
    Questions = .tg_questions_text(e),
    Datasets = paste(unique(ds), collapse = "\n"),
    `Parties chosen` = parties,
    `Term lists chosen` = terms,
    Variants = variants
  )
  if (any(nzchar(glance))) {
    doc <- .tg_par(doc, "At a glance", bold = TRUE, color = THEME_COLORS$primary_dark, keep_next = TRUE, before = 4)
    doc <- .tg_kv(doc, names(glance), glance)
  }
  doc
}

.tg_differs <- function(doc, e) {
  parent <- .tg_chr(e[["parent"]])
  doc <- .add_heading(doc, paste0("How it differs from ", parent), 1)
  status <- .tg_chr(e[["changes_status"]])
  if (.tg_n(e[["changes"]]) > 0L) {
    doc <- .tg_par(doc, paste0("This template starts from ", parent, " and changes the following."), keep_next = TRUE)
    doc <- .tg_table(doc, e[["changes"]], widths = c(2.4, 3.9))
  }
  # Only a comparison that was made may promise that nothing else differs.
  if (identical(status, "compared")) {
    .tg_par(doc, paste0("Everything else is the same as in ", parent, "."))
  } else if (identical(status, "parent_abstract")) {
    .tg_par(doc, paste0(
      parent, " is a building block, so this guide does not list differences; ",
      "everything is described in full below."
    ))
  } else {
    .tg_par(doc, paste0("The differences to ", parent, " could not be determined."))
  }
}

.tg_questions <- function(doc, e) {
  q <- e[["questions"]]
  doc <- .add_heading(doc, "Questions", 2)
  n <- nrow(q)
  any_required <- any(grepl("Must answer", q[["Must answer / fixed"]], fixed = TRUE))
  doc <- .tg_par(doc, paste0(
    "When you create a DTA from this template, DTAtools asks these ", n,
    if (n == 1L) " question" else " questions", ", in this order. ",
    if (any_required) {
      "Questions marked \"Must be answered\" cannot be left empty; all others are optional."
    } else {
      "All questions are optional; each starts with its pre-filled answer where one is shown."
    }
  ), keep_next = TRUE)
  for (i in seq_len(n)) {
    flags <- .tg_chr(q[["Must answer / fixed"]][[i]])
    fixed <- if (grepl("Fixed", flags, fixed = TRUE)) "Yes \u2013 cannot be changed when creating the DTA" else ""
    choices <- .tg_chr(q[["Choices"]][[i]])
    answer <- .tg_chr(q[["Answer type"]][[i]])
    args <- list(
      `What it is for` = .tg_chr(q[["Explanation"]][[i]]),
      `How you answer` = answer
    )
    args[[if (grepl("with suggestions", answer, fixed = TRUE)) "Suggestions" else "Choices"]] <-
      if (.tg_empty(choices)) "" else .tg_choices(choices)
    args[["Pre-filled answer"]] <- .tg_chr(q[["Pre-filled answer"]][[i]])
    args[["Appears in the DTA as"]] <- .tg_chr(q[["Appears in the DTA as"]][[i]])
    args[["Fixed"]] <- fixed
    rows <- do.call(.tg_card_rows, args)
    doc <- .tg_card(doc, paste("Question", i), .tg_chr(q[["Question"]][[i]]),
      required = grepl("Must answer", flags, fixed = TRUE), rows = rows
    )
  }
  doc
}

.tg_ask <- function(doc, e) {
  if (.tg_n(e[["questions"]]) > 0L) doc <- .tg_questions(doc, e)
  if (.tg_n(e[["variants"]]) > 0L) {
    doc <- .add_heading(doc, "Variants", 2)
    doc <- .tg_par(doc, "Some answers change what the DTA contains.", keep_next = TRUE)
    doc <- .tg_table(doc, e[["variants"]], widths = c(1.6, 1.6, 3.1))
  }
  if (.tg_n(e[["parties"]]) > 0L) {
    doc <- .add_heading(doc, "Supplier and receiver", 2)
    doc <- .tg_par(doc, "These parties are chosen from saved party profiles.", keep_next = TRUE)
    p <- e[["parties"]]
    p <- p[c("Party", "Appears in the DTA as", "Available profiles", "Must choose")]
    names(p)[3] <- "Profiles you can choose"
    if (all(tolower(p[["Party"]]) == tolower(p[["Appears in the DTA as"]]))) {
      doc <- .tg_table(doc, p[c("Party", "Profiles you can choose", "Must choose")], widths = c(1.2, 4.2, 0.9))
    } else {
      doc <- .tg_table(doc, p, widths = c(1.2, 1.6, 2.6, 0.9))
    }
  }
  tc <- e[["term_choices"]]
  if (.tg_n(tc) > 0L) {
    doc <- .add_heading(doc, "Term lists", 2)
    doc <- .tg_par(doc, "These choices are made from term lists.", keep_next = TRUE)
    for (i in seq_len(nrow(tc))) {
      rows <- .tg_card_rows(
        `What it is for` = .tg_chr(tc[["Explanation"]][[i]]),
        `Term list` = .tg_chr(tc[["Term list"]][[i]]),
        `How it works` = .tg_chr(tc[["How it works"]][[i]]),
        `Selected by default` = .tg_chr(tc[["Selected by default"]][[i]]),
        `Minimum to select` = .tg_chr(tc[["Minimum to select"]][[i]])
      )
      doc <- .tg_card(doc, paste("Term list choice", i), .tg_chr(tc[["Choice"]][[i]]),
        required = FALSE, rows = rows
      )
    }
  }
  doc
}

# Rows of "Filled in automatically": the pre-filled fields that no question sets.
.tg_auto <- function(e) {
  pre <- e[["prefilled"]]
  if (.tg_n(pre) == 0L) {
    return(pre)
  }
  pre <- .tg_clean_df(pre)
  if ("Question" %in% names(pre)) pre <- pre[!nzchar(pre[["Question"]]), , drop = FALSE]
  rownames(pre) <- NULL
  pre[c("Field", "Value")]
}

# `no` holds the section numbers the text points at (see .template_guide_docx).
.tg_filled <- function(doc, e, no = list()) {
  pre <- .tg_auto(e)
  if (.tg_n(pre) > 0L) {
    doc <- .add_heading(doc, "Filled in automatically", 2)
    lead <- if (nzchar(no$questions %||% "")) {
      paste0(
        "Your answers to the questions in section ", no$questions,
        " are also written into the DTA. In addition, DTAtools fills in:"
      )
    } else {
      "DTAtools enters these values for you."
    }
    doc <- .tg_par(doc, lead, keep_next = TRUE)
    party <- grepl(": organisation and contacts$", pre[["Field"]])
    if (any(party) && nzchar(no$parties %||% "")) {
      pre[["Value"]][party] <- paste0(pre[["Value"]][party], " (see section ", no$parties, ")")
    }
    doc <- .tg_table(doc, pre, widths = c(2.2, 4.1))
  }
  if (.tg_n(e[["locked"]]) > 0L) {
    doc <- .add_heading(doc, "Fixed and mandatory fields", 2)
    doc <- .tg_par(doc, "These fields cannot be changed when the DTA is created.", keep_next = TRUE)
    doc <- .tg_table(doc, e[["locked"]], widths = c(2.5, 3.8))
  }
  doc
}

# ---- datasets (landscape) ---------------------------------------------------

.tg_columns_table <- function(doc, cols) {
  cols <- .tg_clean_df(cols)
  n <- nrow(cols)
  change <- if ("Change" %in% names(cols)) cols[["Change"]] else rep("", n)
  parts <- strsplit(cols[["Column"]], "\n", fixed = TRUE)
  id <- vapply(parts, function(p) if (length(p)) p[[1]] else "", character(1))
  label <- vapply(parts, function(p) if (length(p) > 1L) paste(p[-1], collapse = " ") else "", character(1))
  df <- data.frame(
    Column = id, Type = cols[["Type"]],
    Required = ifelse(cols[["Can be empty"]] == "No", "Yes", "No"),
    `Allowed values` = cols[["Allowed values"]], Description = cols[["Description"]],
    check.names = FALSE, stringsAsFactors = FALSE
  )
  ft <- .style_table(flextable::flextable(df))
  ft <- flextable::padding(ft, padding.top = 3, padding.bottom = 3, part = "body")
  ft <- flextable::width(ft, j = 1:5, width = c(1.9, 1.1, 0.8, 2.4, 3.5))
  ft <- .fit_table_width(ft, TEXT_WIDTH$landscape)
  for (i in seq_len(n)) {
    ft <- flextable::compose(ft, i = i, j = 1, part = "body", value = .tg_id_cell(id[[i]], label[[i]], change[[i]]))
    ft <- flextable::compose(ft, i = i, j = 4, part = "body", value = .tg_allowed_cell(df[["Allowed values"]][[i]]))
  }
  .tg_add_table(doc, .tg_props(ft, keep = n <= 15))
}

# The cell of one rule: labels in bold, list items as bullets, one line per
# row of .rule_lines(); the text equals .rule_text(). `lines` is the layout
# from .rule_lines(); without it the wording is set as plain lines.
.tg_rule_cell <- function(text, lines = NULL) {
  if (is.null(lines)) {
    lines <- .tg_lines(text)
    if (length(lines) == 0L) lines <- ""
    return(.tg_paragraph(lapply(lines, function(l) list(.tg_chunk(l)))))
  }
  pieces <- lapply(seq_len(nrow(lines)), function(i) {
    label <- lines$label[[i]]
    body <- lines$text[[i]]
    if (lines$bullet[[i]]) {
      list(.tg_chunk(paste0("\u2022 ", body)))
    } else if (nzchar(label)) {
      c(
        list(.tg_chunk(paste0(label, ":"), bold = TRUE)),
        if (nzchar(body)) list(.tg_chunk(paste0(" ", body)))
      )
    } else {
      list(.tg_chunk(body))
    }
  })
  .tg_paragraph(pieces)
}

# How to read the rule descriptions; set once, before the first rules table.
.tg_rule_reading <- function(doc) {
  guide <- .rule_reading_guide()
  doc <- .tg_par(doc, "How to read the rules", bold = TRUE, keep_next = TRUE)
  for (i in seq_along(guide)) {
    doc <- .tg_par(doc, paste0("\u2022 ", guide[[i]]), keep_next = TRUE, after = 2)
  }
  doc
}

.tg_rules_table <- function(doc, rules, lines = NULL) {
  rules <- .tg_clean_df(rules)
  n <- nrow(rules)
  change <- if ("Change" %in% names(rules)) rules[["Change"]] else rep("", n)
  if (!is.list(lines) || length(lines) != n) lines <- NULL
  df <- data.frame(
    `Rule ID` = rules[["Rule ID"]], Description = rules[["Description"]],
    check.names = FALSE, stringsAsFactors = FALSE
  )
  ft <- .style_table(flextable::flextable(df))
  ft <- flextable::padding(ft, padding.top = 3, padding.bottom = 3, part = "body")
  ft <- flextable::width(ft, j = 1:2, width = c(2.0, 7.7))
  ft <- .fit_table_width(ft, TEXT_WIDTH$landscape)
  for (i in seq_len(n)) {
    if (nzchar(change[[i]])) {
      ft <- flextable::compose(ft, i = i, j = 1, part = "body", value = .tg_id_cell(df[["Rule ID"]][[i]], "", change[[i]]))
    }
    ft <- flextable::compose(ft, i = i, j = 2, part = "body", value = .tg_rule_cell(df[["Description"]][[i]], lines[[i]]))
  }
  # Rules are longer than one line: keep a table on one page only while the
  # rendered lines still fit (about 110 characters per line at 9 pt).
  keep <- n <= 15L && .tg_card_lines(df[["Description"]], chars = 110) <= 30
  .tg_add_table(doc, .tg_props(ft, keep = keep))
}

.tg_dataset <- function(doc, ds, reading = FALSE) {
  title <- ds[["title"]]
  if (.tg_has(ds[["variant"]])) {
    title <- paste0(title, " \u2014 ", sub("^For ", "for ", ds[["variant"]]))
  }
  doc <- .add_heading(doc, title, 2)
  about <- ds[["about"]]
  keys <- if (.tg_n(about) > 0L) about[["key"]] else character(0)
  vals <- if (.tg_n(about) > 0L) about[["value"]] else character(0)
  # The headings below carry the column and rule counts.
  drop <- keys %in% c("Columns", "Rules")
  keys <- keys[!drop]
  vals <- vals[!drop]
  if (.tg_has(ds[["source"]])) {
    ver <- .tg_chr(ds[["source_version"]])
    keys <- c(keys, "Based on dataset template")
    vals <- c(vals, paste0(ds[["source"]], if (nzchar(ver)) paste0(", version ", ver) else ""))
  }
  doc <- .tg_kv(doc, keys, vals, width = TEXT_WIDTH$landscape, label_width = 2.2)

  cols <- ds[["columns"]]
  if (.tg_n(cols) > 0L) {
    doc <- .add_heading(doc, paste0("Columns (", nrow(cols), ")"), 3)
    if ("Change" %in% names(cols) && any(nzchar(cols[["Change"]]))) {
      src <- paste0(
        ds[["source"]],
        if (nzchar(.tg_chr(ds[["source_version"]]))) paste0(", version ", ds[["source_version"]]) else ""
      )
      doc <- .tg_par(doc, paste0(
        "Columns marked Added or Changed by this template differ from the dataset template ", src, "."
      ), keep_next = TRUE)
    }
    doc <- .tg_columns_table(doc, cols)
  }
  if (length(ds[["removed_columns"]]) > 0L) {
    doc <- .tg_par(doc, paste0(
      "Removed from the dataset template: ", paste(ds[["removed_columns"]], collapse = ", "), "."
    ))
  }
  rules <- ds[["rules"]]
  if (.tg_n(rules) > 0L) {
    doc <- .add_heading(doc, paste0("Rules (", nrow(rules), ")"), 3)
    if (isTRUE(reading)) doc <- .tg_rule_reading(doc)
    doc <- .tg_rules_table(doc, rules, ds[["rule_lines"]])
  }
  if (length(ds[["removed_rules"]]) > 0L) {
    doc <- .tg_par(doc, paste0(
      "Rules removed from the dataset template: ", paste(ds[["removed_rules"]], collapse = ", "), "."
    ))
  }
  doc
}

# ---- reference chapters -----------------------------------------------------

.tg_glance_value <- function(entry, key) {
  g <- entry[["at_a_glance"]]
  if (.tg_n(g) == 0L || !key %in% g[["key"]]) "" else g[["value"]][[match(key, g[["key"]])]]
}

.tg_vocabularies <- function(doc, vocabs) {
  doc <- .add_heading(doc, "Term lists used", 1)
  for (v in vocabs) {
    doc <- .add_heading(doc, v[["title"]], 2)
    if (.tg_has(v[["description"]])) doc <- .tg_par(doc, v[["description"]], keep_next = TRUE)
    doc <- .tg_kv(
      doc,
      c("Used for columns", "Version"),
      c(.tg_glance_value(v, "Used for columns"), .tg_chr(v[["version"]]))
    )
    doc <- .tg_table(doc, v[["terms"]][c("Code", "Label", "Description")], widths = c(1.5, 2, 2.8))
  }
  doc
}

.tg_parties <- function(doc, parties) {
  doc <- .add_heading(doc, "Party profiles you can choose", 1)
  for (p in parties) {
    doc <- .add_heading(doc, p[["title"]], 2)
    if (.tg_has(p[["description"]])) doc <- .tg_par(doc, p[["description"]], keep_next = TRUE)
    keys <- c("Role", "Organisation", "Address", "Country")
    doc <- .tg_kv(doc, keys, vapply(keys, function(k) .tg_glance_value(p, k), character(1)))
    ct <- p[["contacts"]]
    if (.tg_n(ct) > 0L) {
      ct <- .tg_clean_df(ct)
      contact <- vapply(seq_len(nrow(ct)), function(i) {
        paste(c(ct[["Email"]][[i]], ct[["Phone"]][[i]])[!.tg_empty(c(ct[["Email"]][[i]], ct[["Phone"]][[i]]))], collapse = "\n")
      }, character(1))
      df <- data.frame(
        Name = ct[["Name"]], Role = ct[["Role"]], Contact = contact,
        `Signs the DTA` = ifelse(ct[["Signs"]] == "Yes", "Yes", "No"),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      doc <- .tg_table(doc, df, widths = c(1.6, 1.6, 2.2, 0.9))
    }
  }
  doc
}

.tg_technical <- function(doc, guide, e) {
  doc <- .add_heading(doc, "Technical details", 1)
  doc <- .tg_par(doc, "Readers can ignore this section. It is here for traceability.", italic = TRUE, color = THEME_COLORS$gray_mid, keep_next = TRUE)
  dash <- function(x) if (nzchar(x)) x else "\u2014"
  refs <- e[["refs"]]
  keys <- c(
    "Template file", "Template ID", "Version", "Date", "Based on", "Also includes",
    "Dataset templates", "Term lists", "Party profiles", "Generated by"
  )
  vals <- c(
    .tg_chr(e[["file"]]), e[["id"]], .tg_chr(e[["version"]]), .tg_chr(e[["date"]]),
    .tg_chain_ids(e[["lineage"]]), .tg_ref_ids(e[["mixins"]]),
    .tg_ref_ids(refs[["dataset_templates"]]), .tg_ref_ids(refs[["vocabularies"]]), .tg_ref_ids(refs[["parties"]]),
    paste0("DTAtools ", .tg_chr(guide[["library"]][["package_version"]]))
  )
  doc <- .tg_kv(doc, keys, vapply(vals, dash, character(1), USE.NAMES = FALSE))
  pr <- guide[["problems"]]
  if (.tg_n(pr) > 0L) {
    involved <- c(
      e[["id"]], e[["lineage"]][["id"]], e[["mixins"]][["id"]],
      refs[["dataset_templates"]][["id"]], refs[["vocabularies"]][["id"]], refs[["parties"]][["id"]]
    )
    hit <- pr[["File"]] == .tg_chr(e[["file"]]) |
      vapply(pr[["File"]], function(f) any(startsWith(f, paste0(involved, "."))), logical(1))
    if (any(hit)) {
      doc <- .tg_par(doc, "Problems found", bold = TRUE, color = THEME_COLORS$primary_dark, keep_next = TRUE, before = 4)
      doc <- .tg_table(doc, pr[hit, , drop = FALSE], widths = c(2.4, 3.9))
    }
  }
  doc
}

# ---- cover and furniture ----------------------------------------------------

.tg_cover <- function(doc, guide, e, org, sections) {
  lib <- guide[["library"]]
  label <- e[["title"]]
  desc <- if (.tg_has(e[["description"]])) .tg_first_sentence(e[["description"]]) else .tg_chr(e[["summary"]])
  doc <- .tg_par(doc, "TEMPLATE GUIDE", size = 9, bold = TRUE, color = THEME_COLORS$primary, before = 110)
  doc <- .tg_par(doc, label, size = FONT_SIZES$title, bold = TRUE, color = THEME_COLORS$primary_dark, after = 0)
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext("", .house_fp_text(size = FONT_SIZES$body)),
    fp_p = officer::fp_par(
      border.bottom = officer::fp_border(color = THEME_COLORS$primary, width = 2),
      padding.bottom = 6
    )
  ))
  if (nzchar(desc)) doc <- .tg_par(doc, desc, size = FONT_SIZES$subtitle, color = THEME_COLORS$gray_mid, before = 6)
  if (nzchar(org)) doc <- .tg_par(doc, org, bold = TRUE)
  doc <- .tg_gap(doc)
  doc <- .tg_kv(doc,
    c("Template ID", "Version", "Date", "Status", "Generated"),
    c(
      e[["id"]], .tg_chr(e[["version"]]), .tg_chr(e[["date"]]),
      if (.tg_has(e[["status"]])) e[["status"]] else "Ready to use",
      paste0(.tg_chr(lib[["generated"]]), " \u00b7 DTAtools ", .tg_chr(lib[["package_version"]]))
    ),
    width = 4.6, label_width = 1.3
  )
  glossary_no <- match("Words used in this document", sections)
  doc <- .tg_par(doc, "About this document", size = FONT_SIZES$heading3, bold = TRUE, color = THEME_COLORS$primary_dark, keep_next = TRUE, before = 10)
  doc <- .tg_par(doc, paste0(
    "This document describes the DTA template ", label, ": which questions DTAtools asks when you create a DTA from it, ",
    "what it fills in for you, and which datasets it creates. ",
    "You do not need the DTAtools app or any programming knowledge to read it. ",
    "Terms such as \"Pre-filled answer\" are explained in section ", glossary_no, ", \"Words used in this document\"."
  ))
  doc <- .tg_par(doc, "In this document", size = FONT_SIZES$heading3, bold = TRUE, color = THEME_COLORS$primary_dark, keep_next = TRUE, before = 10)
  for (k in seq_along(sections)) {
    doc <- .tg_par(doc, paste0(k, "  ", sections[[k]]), after = 2)
  }
  .end_section_orientation(doc, "portrait")
}

.tg_furniture <- function(guide, e, org) {
  lib <- guide[["library"]]
  label <- e[["title"]]
  if (nchar(label) > 60) label <- paste0(substr(label, 1, 59), "\u2026")
  dot <- "  \u00b7  "
  date <- .tg_chr(e[["date"]])
  left <- paste0(
    e[["id"]], dot, "Template version ", .tg_chr(e[["version"]]),
    if (nzchar(date)) paste0(" of ", date) else ""
  )
  right <- paste0("Generated ", .tg_chr(lib[["generated"]]), dot, "DTAtools ", .tg_chr(lib[["package_version"]]))
  function(width) {
    .running_furniture(
      header_title = paste0("Template guide: ", label),
      footer_left = left, footer_right = right,
      org_line = org, width = width
    )
  }
}

#' Render ONE creation template of the collected guide (see
#' `.template_guide_collect()`) as a Word document. Pure rendering: no file I/O.
#' @param guide The list returned by `.template_guide_collect()`.
#' @param entry One element of `guide$creation`.
#' @param organisation,organisation_unit Shown on the cover and in the page
#'   header; `""` shows none.
#' @return An `rdocx` object.
#' @keywords internal
.template_guide_docx <- function(guide, entry, organisation = "", organisation_unit = "") {
  e <- entry
  org <- .export_org_line(organisation, organisation_unit)
  has <- function(x) .tg_n(x) > 0L

  # By (id, version): the collector keeps every version of a referenced id.
  pair <- function(id, version) paste(id, version, sep = "\r")
  offered <- function(df) pair(df[["id"]], df[["version"]])
  vocabs <- Filter(function(v) pair(v[["id"]], v[["version"]]) %in% offered(e[["refs"]][["vocabularies"]]), guide[["vocabularies"]])
  parties <- Filter(function(p) pair(p[["id"]], p[["version"]]) %in% offered(e[["refs"]][["parties"]]), guide[["parties"]])
  asks <- has(e[["questions"]]) || has(e[["variants"]]) || has(e[["parties"]]) || has(e[["term_choices"]])
  filled <- has(.tg_auto(e)) || has(e[["locked"]])
  differs <- .tg_has(e[["parent"]])
  datasets <- length(e[["datasets"]]) > 0L

  sections <- c(
    "Overview",
    if (differs) paste0("How it differs from ", e[["parent"]]),
    if (asks) "What you will be asked",
    if (filled) "What is filled in for you",
    if (datasets) "Datasets it creates",
    if (length(vocabs) > 0L) "Term lists used",
    if (length(parties) > 0L) "Party profiles you can choose",
    "Words used in this document",
    "Technical details"
  )

  # Section numbers the text refers to: the questions are always the first
  # part of "What you will be asked", the party table follows the variants.
  ask_no <- match("What you will be asked", sections)
  subs <- c(
    questions = has(e[["questions"]]), variants = has(e[["variants"]]),
    parties = has(e[["parties"]]), terms = has(e[["term_choices"]])
  )
  sub_no <- function(k) {
    if (is.na(ask_no) || !subs[[k]]) "" else paste0(ask_no, ".", sum(subs[seq_len(match(k, names(subs)))]))
  }
  no <- list(questions = sub_no("questions"), parties = sub_no("parties"))

  doc <- .new_numbered_docx()
  doc <- officer::set_doc_properties(doc,
    title = paste0("Template guide: ", e[["title"]]),
    subject = e[["id"]],
    creator = if (nzchar(trimws(organisation))) trimws(organisation) else "DTAtools"
  )
  furniture <- .tg_furniture(guide, e, org)

  doc <- .tg_cover(doc, guide, e, org, sections)

  doc <- .tg_overview(doc, e)
  if (differs) doc <- .tg_differs(doc, e)
  if (asks) {
    doc <- .add_heading(doc, "What you will be asked", 1)
    doc <- .tg_ask(doc, e)
  }
  if (filled) {
    doc <- .add_heading(doc, "What is filled in for you", 1)
    doc <- .tg_filled(doc, e, no)
  }

  if (datasets) {
    # One landscape section for the whole chapter: close the portrait content
    # so far, add the chapter, then close it as landscape.
    doc <- .end_section_orientation(doc, "portrait", furniture = furniture)
    doc <- .add_heading(doc, "Datasets it creates", 1)
    # The reading guide for the rules comes once, before the first rules table.
    has_rules <- vapply(e[["datasets"]], function(d) .tg_n(d[["rules"]]) > 0L, logical(1))
    first_rules <- if (any(has_rules)) which(has_rules)[[1]] else 0L
    for (i in seq_along(e[["datasets"]])) {
      doc <- .tg_dataset(doc, e[["datasets"]][[i]], reading = i == first_rules)
    }
    doc <- .end_section_orientation(doc, "landscape", furniture = furniture)
  }

  if (length(vocabs) > 0L) doc <- .tg_vocabularies(doc, vocabs)
  if (length(parties) > 0L) doc <- .tg_parties(doc, parties)

  doc <- .add_heading(doc, "Words used in this document", 1)
  g <- .tg_glossary(e)
  doc <- .tg_kv(doc, g$key, g$value)
  doc <- .tg_technical(doc, guide, e)

  officer::body_set_default_section(doc, .section_props(doc, "portrait", furniture = furniture, type = "nextPage"))
}
