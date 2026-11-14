#' Every block placeholder a Word template may use, with a one-line description
#'
#' A block placeholder is replaced by real Word content -- a table, a heading, a
#' list -- rather than by a text value, so it must be the only text in its
#' paragraph. [.tv_placeholder_catalog()] is the inline counterpart.
#' @keywords internal
.tv_block_catalog <- function() {
  c(
    "{COLUMN_SPECS}" = "Column specification table per tabular dataset",
    "{VALIDATION_RULES}" = "Validation rule table per tabular dataset",
    "{FILE_SPECS}" = "Expected-file table per dataset",
    "{DATASETS}" = "Full section per dataset: description, files, columns, rules",
    "{SUPPLIER_CONTACTS_TABLE}" = "Supplier contacts as a table",
    "{RECEIVER_CONTACTS_TABLE}" = "Receiver contacts as a table",
    "{SIGNATURES_TABLE}" = "Approval and signatures table",
    "{VERSION_HISTORY_TABLE}" = "Version history as a table",
    "{STUDY_TABLE}" = "Study and protocol identity as a table; omitted when empty",
    "{AGREEMENT_SECTIONS}" = "Ordered agreement headings and plain narrative paragraphs; omitted when empty",
    "{AUTHORIZED_CORRECTIONS_LIST}" = "Authorized-for-corrections names as a bulleted list",
    "{CONTAINERS_TABLE}" = "Delivery containers: intro sentence and table; omitted when there are none"
  )
}

#' Block tokens that accept a `:DATASET` argument
#' @keywords internal
.tv_block_takes_arg <- function() {
  c("{COLUMN_SPECS}", "{VALIDATION_RULES}", "{FILE_SPECS}", "{DATASETS}")
}

#' Legacy block token spellings, mapped to their canonical name
#'
#' `{DATASETS_DETAIL}` is what `inst/extdata/word-templates/clinical_dta_template.docx`
#' contains, and what the Shiny app used to supply as a markdown string.
#' @keywords internal
.tv_block_aliases <- function() {
  c("{DATASETS_DETAIL}" = "{DATASETS}")
}

#' Parse a brace token into its block name and optional dataset argument
#'
#' @return `NULL` when the token is not a block placeholder; otherwise a list
#'   with `name` (canonical, brace-delimited) and `arg` (character scalar or
#'   `NULL`).
#' @keywords internal
.tv_parse_block_token <- function(token) {
  inner <- gsub("^\\{|\\}$", "", token)
  has_colon <- grepl(":", inner, fixed = TRUE)
  arg <- if (has_colon) trimws(sub("^[^:]*:", "", inner)) else NULL
  if (!is.null(arg) && !nzchar(arg)) {
    arg <- NULL
  }

  base <- sub(":.*$", "", inner)
  al <- .tv_block_aliases()
  name <- paste0("{", base, "}")
  # TRAP: al is an atomic named character vector, so al[[name]] throws a
  # subscript-out-of-bounds error when name is absent, and %||% does not save
  # you -- the error happens before %||% ever sees a value.
  name <- if (name %in% names(al)) al[[name]] else name

  if (!name %in% names(.tv_block_catalog())) {
    return(NULL)
  }
  if (!is.null(arg) && !name %in% .tv_block_takes_arg()) {
    return(NULL)
  }

  list(name = name, arg = arg)
}

#' Is this brace token a block placeholder?
#' @keywords internal
.tv_is_block_token <- function(token) !is.null(.tv_parse_block_token(token))

#' A per-export nonce, so a sentinel cannot be spelled by a substituted value
#' @keywords internal
.tv_block_nonce <- function() {
  gsub("[^A-Za-z0-9]", "", basename(tempfile("n")))
}

#' The marker that stands in for a block placeholder between the two passes
#'
#' Deliberately not brace-delimited: [.tv_token_pattern()] must not match it, or
#' the substitution pass would report every block placeholder as an unresolved
#' token.
#' @keywords internal
.tv_block_sentinel <- function(nonce, i) paste0("DTABLOCK", nonce, "_", i)

#' Stamp one paragraph's text runs with a sentinel, blanking the rest
#'
#' Shared by [.tv_mark_block_paragraphs()] (stamping the template's own block
#' paragraphs) and [.tv_bind_region_block()] (stamping a bare dataset block
#' inside a freshly-copied region repetition). A paragraph with no text run at
#' all -- possible for an empty or purely-graphical paragraph -- cannot carry
#' a sentinel, so it is left alone and reported as `FALSE`.
#'
#' @param p An `xml_node` for the paragraph to stamp.
#' @param sentinel Character. The sentinel text to write into the first run.
#' @return `TRUE` if a run was found and stamped, `FALSE` otherwise.
#' @keywords internal
.tv_stamp_paragraph <- function(p, sentinel) {
  t_nodes <- xml2::xml_find_all(p, ".//*[local-name()='r']/*[local-name()='t']")
  if (length(t_nodes) == 0) {
    return(FALSE)
  }
  .tv_set_run_text(t_nodes[[1]], sentinel)
  for (i in seq_along(t_nodes)[-1]) {
    .tv_set_run_text(t_nodes[[i]], "")
  }
  TRUE
}

#' Stamp every template paragraph that is wholly a block placeholder
#'
#' Called on the template's `word/document.xml` *before* any substitution, and
#' the reason the two passes can be told apart at all. Identifying block
#' paragraphs by their text in the finished document instead would conflate two
#' different things -- what the template asked for, and what a substituted value
#' happens to spell -- so a DTA whose title is the literal string
#' `"{COLUMN_SPECS}"` would have its title paragraph silently replaced by a
#' table. The nonce is drawn per export, so no value can predict or contain it.
#'
#' Only *top-level* body paragraphs are stamped, which is also what keeps a
#' placeholder inside a table from taking the table with it:
#' `officer::cursor_reach()` matches the text of a top-level body child, and
#' `xml2::xml_text()` of a `w:tbl` concatenates every cell, so a one-cell table
#' whose only content is the token would match as a whole. Unstamped, it is left
#' alone and reported instead.
#'
#' A token the caller supplied a value for is left unstamped too: `variables`
#' wins over a block, and pass 1 needs the token intact to substitute into. A
#' dataset value [.tv_expand_regions()] substituted into a region copy cannot
#' be mistaken for one: its braces are protected stand-ins until both passes
#' are done (see [.tv_protect_braces()]).
#'
#' @param xml_path Character. Path to the template's `word/document.xml`.
#' @param nonce Character. This export's nonce, from [.tv_block_nonce()].
#' @param variables Named list of placeholder values; keys here take precedence.
#' @return A named character vector mapping sentinel to the token it replaced,
#'   in document order. Empty when the template has no block placeholder.
#' @keywords internal
.tv_mark_block_paragraphs <- function(xml_path, nonce, variables = list()) {
  doc <- tryCatch(xml2::read_xml(xml_path), error = function(e) NULL)
  if (is.null(doc)) {
    return(character(0))
  }
  paras <- xml2::xml_find_all(
    doc,
    "/*[local-name()='document']/*[local-name()='body']/*[local-name()='p']"
  )

  keys <- names(variables)
  found <- character(0)
  for (p in paras) {
    token <- trimws(xml2::xml_text(p))
    if (!nzchar(token) || token %in% keys || !.tv_is_block_token(token)) {
      next
    }
    sentinel <- .tv_block_sentinel(nonce, length(found) + 1L)
    if (!.tv_stamp_paragraph(p, sentinel)) {
      next
    }
    found <- c(found, stats::setNames(token, sentinel))
  }

  if (length(found) > 0) {
    xml2::write_xml(doc, xml_path)
  }
  found
}

#' Every block token still present in a finished .docx
#'
#' Run over the *output*, after rendering, to find what could not be rendered:
#' a token inside a sentence, inside a table cell, in a header or a footer, or
#' one put back because its `:DATASET` argument named nothing. It reports rather
#' than drives -- what gets rendered is decided before substitution, by
#' [.tv_mark_block_paragraphs()].
#'
#' Scans the paragraph *text* of `word/document.xml` and every header/footer
#' part, not the raw XML: Word freely splits a typed placeholder across runs, so
#' a raw grep would miss a token the exporter itself handles perfectly well.
#' Also collects region markers (`{#DATASETS}` / `{/DATASETS}`) left behind by
#' a malformed region -- unclosed, unopened, or not alone in its paragraph.
#' @return Character vector of unique tokens exactly as they appear.
#' @keywords internal
.tv_scan_block_tokens <- function(docx_path) {
  temp_dir <- tempfile("dta_block_scan_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  extracted <- tryCatch(
    utils::unzip(docx_path, exdir = temp_dir),
    warning = function(w) character(0),
    error = function(e) character(0)
  )
  if (length(extracted) == 0) {
    return(character(0))
  }

  word_dir <- file.path(temp_dir, "word")
  main <- file.path(word_dir, "document.xml")
  parts <- if (file.exists(main)) main else character(0)
  parts <- c(
    parts,
    list.files(
      word_dir,
      pattern = "^(header|footer)[0-9]*\\.xml$",
      full.names = TRUE
    )
  )
  if (length(parts) == 0) {
    return(character(0))
  }

  pattern <- .tv_token_pattern()
  tokens <- character(0)
  for (part in parts) {
    part_doc <- tryCatch(xml2::read_xml(part), error = function(e) NULL)
    if (is.null(part_doc)) {
      next
    }
    paras <- xml2::xml_find_all(part_doc, ".//*[local-name()='p']")
    for (p in paras) {
      txt <- xml2::xml_text(p)
      m <- gregexpr(pattern, txt, perl = TRUE)
      matches <- regmatches(txt, m)[[1]]
      if (length(matches) == 0) {
        next
      }
      tokens <- c(tokens, Filter(function(t) .tv_is_block_token(t) || .tv_is_region_marker(t), matches))
    }
  }

  unique(tokens)
}

#' Add a heading using the template's own heading style when it has one
#'
#' officer raises an error for a style the document does not define, and a
#' user-authored template need not define "heading 2" at all, so the style is
#' resolved against the document and falls back to the direct-formatted
#' subheading used elsewhere for levels the template lacks.
#' @keywords internal
.tv_block_heading <- function(doc, text, level = 2) {
  style <- paste("heading", level)
  available <- tryCatch(
    officer::styles_info(doc, type = "paragraph")$style_name,
    error = function(e) character(0)
  )
  if (style %in% available) {
    officer::body_add_par(doc, text, style = style)
  } else {
    .add_bold_subheading(doc, text)
  }
}

#' Text width, in inches, of the section a table added at the cursor lands in
#'
#' `officer::docx_dim()` cannot be used for this: its XPath ends in a
#' `//w:sectPr` alternative, which matches every section and so always yields
#' the document's *first* one, wherever the cursor is. The section is therefore
#' looked up here. officer adds a block directly after the cursor's body child
#' (`/w:document/w:body/*[which]`), and a section is ended by the paragraph
#' whose `w:pPr` carries its `w:sectPr` (the last section by the body-level
#' `w:sectPr`), so the table belongs to the first section break after the
#' cursor.
#'
#' Columns are honoured: the width of a table inside a `w:cols num = "n"`
#' section is that of one column, `(w - (n - 1) * space) / n`. Unequal columns
#' (`w:col` children or `w:equalWidth = "0"`) have no single width.
#' @param doc An `rdocx`.
#' @return The width in inches, or `NA_real_` when the section has no page size
#'   or margins, or has unequal columns.
#' @keywords internal
.tv_cursor_text_width <- function(doc) {
  cursor <- doc$officer_cursor$which
  after <- if (length(cursor) == 1L && !is.na(cursor)) cursor else 0L
  later <- paste0("/*[local-name()='document']/*[local-name()='body']/*[position() > ", after, "]")
  sect <- xml2::xml_find_first(
    officer::docx_body_xml(doc),
    paste0(
      "(", later, "/self::*[local-name()='sectPr']",
      " | ", later, "/*[local-name()='pPr']/*[local-name()='sectPr'])[1]"
    )
  )
  if (inherits(sect, "xml_missing")) {
    return(NA_real_)
  }
  num <- function(node, attr) suppressWarnings(as.numeric(xml2::xml_attr(node, attr)))
  child <- function(name) xml2::xml_find_first(sect, paste0("./*[local-name()='", name, "']"))
  page <- num(child("pgSz"), "w")
  margin <- c(num(child("pgMar"), "left"), num(child("pgMar"), "right"))
  if (anyNA(c(page, margin))) {
    return(NA_real_)
  }
  twips <- page - sum(margin)
  cols <- child("cols")
  n <- num(cols, "num")
  if (!is.na(n) && n > 1) {
    unequal <- length(xml2::xml_find_all(cols, "./*[local-name()='col']")) > 0 ||
      xml2::xml_attr(cols, "equalWidth") %in% c("0", "false")
    if (unequal) {
      return(NA_real_)
    }
    space <- num(cols, "space")
    twips <- (twips - (n - 1) * (if (is.na(space)) 720 else space)) / n
  }
  twips / 1440
}

#' Fit a table to the template's text column rather than a fixed inch width
#'
#' The built-in layout gives its widest tables landscape pages of their own.
#' A user template's page geometry is not ours to change -- inserting section
#' breaks would re-orient content that is not ours -- so the builder's column
#' widths are scaled, in proportion, to the text width of the section the table
#' lands in (see [.tv_cursor_text_width()]). The layout is fixed so Word keeps
#' those widths: an autofit table is written without a column grid and Word
#' sizes the columns from their content, which squeezes a narrow column beside
#' a long one.
#'
#' When that section's width cannot be determined the table keeps the
#' width-adaptive layout (100% of the text column), which suits any page
#' geometry but leaves the column sizing to Word.
#' @param ft A flextable.
#' @param doc The `rdocx` the table is going into; the table is added at its
#'   cursor.
#' @keywords internal
.tv_block_fit <- function(ft, doc) {
  text_w <- .tv_cursor_text_width(doc)
  if (is.na(text_w)) {
    return(flextable::set_table_properties(ft, layout = "autofit", width = 1))
  }
  w <- dim(ft)$widths
  ft <- flextable::width(ft, width = w * text_w / sum(w))
  flextable::set_table_properties(ft, layout = "fixed")
}

#' Add a flextable fitted to the page, or an italic note when there is none
#' @keywords internal
.tv_block_table <- function(doc, ft, empty_note) {
  if (is.null(ft)) {
    doc <- .add_body_par(doc, empty_note, italic = TRUE, color = THEME_COLORS$gray_mid)
  } else {
    doc <- flextable::body_add_flextable(doc, .tv_block_fit(ft, doc))
  }
  .add_spacer(doc)
}

#' A validation-rules table for a block, preceded by the rule reading guide
#' when the document does not hold it yet (once per document)
#' @keywords internal
.tv_block_rules_table <- function(doc, rules) {
  ft <- .build_rules_table(rules, width = NULL)
  if (!is.null(ft)) {
    doc <- .add_rule_reading_guide(doc)
  }
  .tv_block_table(doc, ft, "No validation rules specified.")
}

#' Datasets a block should render, honouring its `:NAME` argument
#'
#' @param tabular_only Keep only `DTADataSetTabular` datasets (column specs and
#'   rules exist nowhere else).
#' @return A named list of datasets; `list()` when the selection is empty.
#' @keywords internal
.tv_block_datasets <- function(dta, arg = NULL, tabular_only = TRUE) {
  sets <- dta@datasets
  if (isTRUE(tabular_only)) {
    sets <- Filter(function(d) inherits(d, "DTAtools::DTADataSetTabular"), sets)
  }
  if (!is.null(arg)) {
    sets <- sets[names(sets) == arg]
  }
  sets
}

#' Does the DTA contain a dataset with this id (any kind)?
#' @keywords internal
.tv_block_dataset_exists <- function(dta, name) name %in% names(dta@datasets)

#' `{COLUMN_SPECS}` block: a column-specification table per tabular dataset,
#' optionally without its heading (for a region-bound repetition)
#' @keywords internal
.tv_block_render_column_specs <- function(doc, dta, arg, heading = TRUE) {
  sets <- .tv_block_datasets(dta, arg, tabular_only = TRUE)
  if (length(sets) == 0) {
    return(.add_body_par(doc, "No tabular datasets.", italic = TRUE, color = THEME_COLORS$gray_mid))
  }
  for (nm in names(sets)) {
    if (isTRUE(heading)) {
      doc <- .tv_block_heading(doc, paste0(.column_specs_title(sets[[nm]]), " \u2014 ", dta_dataset_heading(sets[[nm]])), level = 2)
    }
    doc <- .tv_block_table(
      doc, .build_column_specs_table(sets[[nm]]@specs, width = NULL),
      "No column specifications available."
    )
    if (.is_matrix_dataset(sets[[nm]])) {
      doc <- .tv_block_heading(doc, "Matrix layout", level = 3)
      doc <- .tv_block_table(doc, .kv_table(.matrix_layout_rows(sets[[nm]])), "")
    }
  }
  doc
}

#' `{VALIDATION_RULES}` block: a validation-rule table per tabular dataset,
#' optionally without its heading (for a region-bound repetition)
#' @keywords internal
.tv_block_render_validation_rules <- function(doc, dta, arg, heading = TRUE) {
  sets <- .tv_block_datasets(dta, arg, tabular_only = TRUE)
  if (length(sets) == 0) {
    return(.add_body_par(doc, "No tabular datasets.", italic = TRUE, color = THEME_COLORS$gray_mid))
  }
  for (nm in names(sets)) {
    if (isTRUE(heading)) {
      doc <- .tv_block_heading(doc, paste0("Validation Rules \u2014 ", dta_dataset_heading(sets[[nm]])), level = 2)
    }
    doc <- .tv_block_rules_table(doc, sets[[nm]]@specs@rules)
  }
  doc
}

#' `{FILE_SPECS}` block: an expected-files table per dataset, optionally
#' without its heading (for a region-bound repetition)
#' @keywords internal
.tv_block_render_file_specs <- function(doc, dta, arg, heading = TRUE) {
  sets <- .tv_block_datasets(dta, arg, tabular_only = FALSE)
  if (length(sets) == 0) {
    return(.add_body_par(doc, "No datasets.", italic = TRUE, color = THEME_COLORS$gray_mid))
  }
  ctx <- .container_ctx(dta)
  for (nm in names(sets)) {
    if (isTRUE(heading)) {
      doc <- .tv_block_heading(doc, paste0("Files \u2014 ", dta_dataset_heading(sets[[nm]])), level = 2)
    }
    doc <- .tv_block_table(
      doc, .build_file_specs_table(sets[[nm]]@files, width = NULL, ctx = ctx, dataset = sets[[nm]]@id),
      "No files specified."
    )
  }
  doc
}

#' `{DATASETS}` block: the full per-dataset section -- description, fact
#' table (ID, Type, template), files, columns, rules -- optionally without the dataset-name heading (for a
#' region-bound repetition; the Files/Column Specifications/Validation Rules
#' sub-headings always stay)
#' @keywords internal
.tv_block_render_datasets <- function(doc, dta, arg, heading = TRUE) {
  sets <- .tv_block_datasets(dta, arg, tabular_only = FALSE)
  if (length(sets) == 0) {
    return(.add_body_par(doc, "No datasets.", italic = TRUE, color = THEME_COLORS$gray_mid))
  }
  ctx <- .container_ctx(dta)
  for (nm in names(sets)) {
    ds <- sets[[nm]]

    if (isTRUE(heading)) {
      doc <- .tv_block_heading(doc, dta_dataset_heading(ds), level = 2)
    }
    # Description, then the fact table (ID, Type, template), as in the
    # built-in layout.
    if (!is.null(ds@description) && nzchar(ds@description)) {
      doc <- .add_body_par(doc, ds@description)
      doc <- .add_spacer(doc)
    }
    doc <- .tv_block_table(doc, .kv_table(.dataset_facts(ds)), "")

    doc <- .tv_block_heading(doc, "Files", level = 3)
    doc <- .tv_block_table(doc, .build_file_specs_table(ds@files, width = NULL, ctx = ctx, dataset = ds@id), "No files specified.")

    if (inherits(ds, "DTAtools::DTADataSetTabular")) {
      doc <- .tv_block_heading(doc, .column_specs_title(ds), level = 3)
      doc <- .tv_block_table(
        doc, .build_column_specs_table(ds@specs, width = NULL),
        "No column specifications available."
      )
      if (.is_matrix_dataset(ds)) {
        doc <- .tv_block_heading(doc, "Matrix layout", level = 3)
        doc <- .tv_block_table(doc, .kv_table(.matrix_layout_rows(ds)), "")
      }
      doc <- .tv_block_heading(doc, "Validation Rules", level = 3)
      doc <- .tv_block_rules_table(doc, ds@specs@rules)
    }

    doc <- .add_spacer(doc)
  }
  doc
}

#' `{SUPPLIER_CONTACTS_TABLE}` / `{RECEIVER_CONTACTS_TABLE}` block: contacts for
#' one side of the agreement, in the contact table of the built-in layout
#' (`.build_party_contacts_table()`)
#' @keywords internal
.tv_block_render_contacts <- function(doc, dta, arg, side) {
  org <- if (identical(side, "supplier")) dta@metadata@supplier else dta@metadata@receiver
  title <- if (identical(side, "supplier")) "Supplier Contacts" else "Receiver Contacts"
  doc <- .tv_block_heading(doc, title, level = 2)
  .tv_block_table(doc, .build_party_contacts_table(.tv_get(org, "contacts")), "No contacts specified.")
}

#' `{SIGNATURES_TABLE}` block: the approval and signatures table
#' @keywords internal
.tv_block_render_signatures <- function(doc, dta, arg) {
  doc <- .tv_block_heading(doc, "Approval & Signatures", level = 2)
  .tv_block_table(
    doc,
    .build_signature_table(.extract_signatories(dta@metadata, signature_list = NULL)),
    "No authorized signatories."
  )
}

#' `{VERSION_HISTORY_TABLE}` block: version history as a table
#' @keywords internal
.tv_block_render_version_history <- function(doc, dta, arg) {
  doc <- .tv_block_heading(doc, "Version History", level = 2)
  .tv_block_table(
    doc,
    .build_version_history_table(dta@metadata@version_history),
    "No version history recorded."
  )
}

#' `{AUTHORIZED_CORRECTIONS_LIST}` block: authorized-for-corrections names as a
#' bulleted list
#' @keywords internal
.tv_block_render_authorized_corrections <- function(doc, dta, arg) {
  doc <- .tv_block_heading(doc, "Authorized for Corrections", level = 2)

  names_vec <- .format_authorized_for_corrections_lines(dta@metadata@authorized_for_corrections)
  if (length(names_vec) == 0) {
    return(.add_body_par(
      doc, "Nobody is authorized for corrections.",
      italic = TRUE, color = THEME_COLORS$gray_mid
    ))
  }
  for (nm in names_vec) {
    doc <- .add_body_par(doc, paste0("\u2022  ", nm))
  }
  .add_spacer(doc)
}

#' `{CONTAINERS_TABLE}` block: the intro sentence and the Containers table; nothing
#' when no container is in effect
#' @keywords internal
.tv_block_render_containers <- function(doc, dta, arg) {
  ctx <- .container_ctx(dta)
  if (nrow(.containers_df(ctx)) == 0) {
    return(doc)
  }
  doc <- .add_body_par(doc, .containers_intro(ctx))
  .tv_block_table(doc, .build_containers_table(ctx, width = NULL), "")
}

#' Render one block placeholder at the cursor
#' @param heading Logical. Passed through to the four dataset-block renderers:
#'   `FALSE` renders the dataset content without its generated heading (used
#'   for a region-bound block, whose paragraph already names the dataset via
#'   `{DATASET_NAME}`). Ignored by every other block.
#' @keywords internal
.tv_block_render <- function(doc, spec, dta, heading = TRUE) {
  switch(spec$name,
    "{COLUMN_SPECS}" = .tv_block_render_column_specs(doc, dta, spec$arg, heading = heading),
    "{VALIDATION_RULES}" = .tv_block_render_validation_rules(doc, dta, spec$arg, heading = heading),
    "{FILE_SPECS}" = .tv_block_render_file_specs(doc, dta, spec$arg, heading = heading),
    "{DATASETS}" = .tv_block_render_datasets(doc, dta, spec$arg, heading = heading),
    "{SUPPLIER_CONTACTS_TABLE}" = .tv_block_render_contacts(doc, dta, spec$arg, side = "supplier"),
    "{RECEIVER_CONTACTS_TABLE}" = .tv_block_render_contacts(doc, dta, spec$arg, side = "receiver"),
    "{SIGNATURES_TABLE}" = .tv_block_render_signatures(doc, dta, spec$arg),
    "{VERSION_HISTORY_TABLE}" = .tv_block_render_version_history(doc, dta, spec$arg),
    "{STUDY_TABLE}" = .add_study_section(doc, dta@metadata@study, heading = .tv_block_heading),
    "{AGREEMENT_SECTIONS}" = .add_agreement_sections(doc, dta@metadata@sections, heading = .tv_block_heading),
    "{AUTHORIZED_CORRECTIONS_LIST}" = .tv_block_render_authorized_corrections(doc, dta, spec$arg),
    "{CONTAINERS_TABLE}" = .tv_block_render_containers(doc, dta, spec$arg),
    doc
  )
}

# The sentinels in the order their paragraphs appear in the document body;
# a sentinel that is not found (it renders nothing) goes last.
.tv_sentinels_in_document_order <- function(doc, sentinels) {
  if (length(sentinels) < 2L) {
    return(sentinels)
  }
  paras <- xml2::xml_find_all(officer::docx_body_xml(doc), ".//*[local-name()='p']")
  pos <- match(sentinels, trimws(xml2::xml_text(paras)))
  sentinels[order(is.na(pos), pos)]
}

#' Replace the stamped block-placeholder paragraphs with rendered Word content
#'
#' Runs only when [.tv_mark_block_paragraphs()] stamped something, so a template
#' built from inline placeholders alone is never re-serialised by officer.
#'
#' Each sentinel is unique and occupies exactly one paragraph, so the cursor
#' lands on the paragraph the *template* marked -- never on one that merely
#' reads like a placeholder because a substituted value spelled one.
#'
#' @param docx_path Character. The .docx produced by pass 1; rewritten in place.
#' @param dta A [DTA] object.
#' @param blocks Named character vector from [.tv_mark_block_paragraphs()] and
#'   [.tv_expand_regions()] combined: sentinel to the token it replaced.
#' @param quiet Logical. Suppress the warning about placeholders left behind.
#' @param bare Character vector of sentinels (a subset of `names(blocks)`)
#'   stamped by [.tv_expand_regions()] rather than [.tv_mark_block_paragraphs()]:
#'   these render their dataset content without the generated dataset-name
#'   heading a top-level `{DATASETS}` block would otherwise add.
#' @return Invisibly, `docx_path`.
#' @keywords internal
.tv_render_blocks <- function(docx_path, dta, blocks = character(0), quiet = FALSE, bare = character(0)) {
  if (length(blocks) > 0) {
    doc <- officer::read_docx(docx_path)
    # Render in document order, whichever pass stamped a block: what a block
    # adds once per document (the rule reading guide) belongs to the block
    # that comes first in the document.
    for (sentinel in .tv_sentinels_in_document_order(doc, names(blocks))) {
      token <- blocks[[sentinel]]
      anchored <- paste0("^\\s*", .tv_escape_regex(sentinel), "\\s*$")
      if (!isTRUE(officer::cursor_reach_test(doc, anchored))) {
        next
      }
      doc <- officer::cursor_reach(doc, anchored)

      spec <- .tv_parse_block_token(token)
      # An unknown dataset name puts the token back rather than rendering an
      # empty section: the leftover scan below is then what reports it.
      if (is.null(spec) || (!is.null(spec$arg) && !.tv_block_dataset_exists(dta, spec$arg))) {
        doc <- officer::body_add_par(doc, token, pos = "on")
        next
      }

      # A region binds a bare block to EVERY dataset it repeats over. For a
      # file dataset a tabular-only block has nothing to show, and the
      # renderer's "No tabular datasets." note would be false for the
      # document: drop the placeholder paragraph instead.
      if (
        sentinel %in% bare &&
          spec$name %in% c("{COLUMN_SPECS}", "{VALIDATION_RULES}") &&
          length(.tv_block_datasets(dta, spec$arg, tabular_only = TRUE)) == 0
      ) {
        doc <- officer::body_remove(doc)
        next
      }

      # Consume the placeholder paragraph, then let the renderer append after it.
      doc <- officer::body_add_par(doc, "", pos = "on")
      doc <- .tv_block_render(doc, spec, dta, heading = !sentinel %in% bare)
    }
    print(doc, target = docx_path)
  }

  leftover <- .tv_scan_block_tokens(docx_path)
  if (length(leftover) > 0 && !isTRUE(quiet)) {
    items <- gsub("}", "}}", gsub("{", "{{", leftover, fixed = TRUE), fixed = TRUE)
    names(items) <- rep("*", length(items))
    cli::cli_warn(c(
      "!" = "Some block placeholders were left unchanged:",
      items,
      i = "A block placeholder must be the only text in its paragraph, in the document body -- not inside a sentence, a table cell, a header or a footer.",
      i = "A {.code :DATASET} argument must name a dataset the DTA contains.",
      i = "Text that only reads like a placeholder because a value spelled one is listed here too, and is deliberately left as written.",
      i = "A {{#DATASETS}} marker must be the only text in its paragraph, in the document body, and be closed by a {{/DATASETS}} paragraph further down; regions do not nest."
    ))
  }
  invisible(docx_path)
}
