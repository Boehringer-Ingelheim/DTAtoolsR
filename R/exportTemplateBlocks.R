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
    "{AUTHORIZED_CORRECTIONS_LIST}" = "Authorized-for-corrections names as a bulleted list"
  )
}

#' Block tokens that accept a `:DATASET` argument
#' @keywords internal
.tv_block_takes_arg <- function() {
  c("{COLUMN_SPECS}", "{VALIDATION_RULES}", "{FILE_SPECS}", "{DATASETS}")
}

#' Legacy block token spellings, mapped to their canonical name
#'
#' `{DATASETS_DETAIL}` is what `inst/extdata/templates/clinical_dta_template.docx`
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
#' wins over a block, and pass 1 needs the token intact to substitute into.
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
    t_nodes <- xml2::xml_find_all(p, ".//*[local-name()='r']/*[local-name()='t']")
    if (length(t_nodes) == 0) {
      next
    }
    sentinel <- .tv_block_sentinel(nonce, length(found) + 1L)
    .tv_set_run_text(t_nodes[[1]], sentinel)
    for (i in seq_along(t_nodes)[-1]) {
      .tv_set_run_text(t_nodes[[i]], "")
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
      tokens <- c(tokens, Filter(.tv_is_block_token, matches))
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

#' Fit a table to the template's text column rather than a fixed inch width
#'
#' The built-in layout gives its widest tables landscape pages of their own.
#' A user template's page geometry is not ours to change -- inserting section
#' breaks would re-orient content that is not ours -- so the table is set to
#' 100% of whatever text column the template has.
#' @keywords internal
.tv_block_fit <- function(ft) {
  flextable::set_table_properties(ft, layout = "autofit", width = 1)
}

#' Add a flextable fitted to the page, or an italic note when there is none
#' @keywords internal
.tv_block_table <- function(doc, ft, empty_note) {
  if (is.null(ft)) {
    doc <- .add_body_par(doc, empty_note, italic = TRUE, color = THEME_COLORS$gray_mid)
  } else {
    doc <- flextable::body_add_flextable(doc, .tv_block_fit(ft))
  }
  .add_spacer(doc)
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

#' Does the DTA contain a dataset by this name (any kind)?
#' @keywords internal
.tv_block_dataset_exists <- function(dta, name) name %in% names(dta@datasets)

#' `{COLUMN_SPECS}` block: a column-specification table per tabular dataset
#' @keywords internal
.tv_block_render_column_specs <- function(doc, dta, arg) {
  sets <- .tv_block_datasets(dta, arg, tabular_only = TRUE)
  if (length(sets) == 0) {
    return(.add_body_par(doc, "No tabular datasets.", italic = TRUE, color = THEME_COLORS$gray_mid))
  }
  for (nm in names(sets)) {
    doc <- .tv_block_heading(doc, paste0("Column Specifications \u2014 ", nm), level = 2)
    doc <- .tv_block_table(
      doc, .build_column_specs_table(sets[[nm]]@specs),
      "No column specifications available."
    )
  }
  doc
}

#' `{VALIDATION_RULES}` block: a validation-rule table per tabular dataset
#' @keywords internal
.tv_block_render_validation_rules <- function(doc, dta, arg) {
  sets <- .tv_block_datasets(dta, arg, tabular_only = TRUE)
  if (length(sets) == 0) {
    return(.add_body_par(doc, "No tabular datasets.", italic = TRUE, color = THEME_COLORS$gray_mid))
  }
  for (nm in names(sets)) {
    doc <- .tv_block_heading(doc, paste0("Validation Rules \u2014 ", nm), level = 2)
    doc <- .tv_block_table(
      doc, .build_rules_table(sets[[nm]]@specs@rules),
      "No validation rules specified."
    )
  }
  doc
}

#' `{FILE_SPECS}` block: an expected-files table per dataset
#' @keywords internal
.tv_block_render_file_specs <- function(doc, dta, arg) {
  sets <- .tv_block_datasets(dta, arg, tabular_only = FALSE)
  if (length(sets) == 0) {
    return(.add_body_par(doc, "No datasets.", italic = TRUE, color = THEME_COLORS$gray_mid))
  }
  for (nm in names(sets)) {
    doc <- .tv_block_heading(doc, paste0("Files \u2014 ", nm), level = 2)
    doc <- .tv_block_table(
      doc, .build_file_specs_table(sets[[nm]]@files),
      "No files specified."
    )
  }
  doc
}

#' `{DATASETS}` block: the full per-dataset section -- description, files,
#' columns, rules
#' @keywords internal
.tv_block_render_datasets <- function(doc, dta, arg) {
  sets <- .tv_block_datasets(dta, arg, tabular_only = FALSE)
  if (length(sets) == 0) {
    return(.add_body_par(doc, "No datasets.", italic = TRUE, color = THEME_COLORS$gray_mid))
  }
  for (nm in names(sets)) {
    ds <- sets[[nm]]

    doc <- .tv_block_heading(doc, nm, level = 2)
    doc <- .add_body_par(doc, paste0("Type: ", ds@type))
    if (!is.null(ds@description) && nzchar(ds@description)) {
      doc <- .add_body_par(doc, ds@description)
    }

    doc <- .tv_block_heading(doc, "Files", level = 3)
    doc <- .tv_block_table(doc, .build_file_specs_table(ds@files), "No files specified.")

    if (inherits(ds, "DTAtools::DTADataSetTabular")) {
      doc <- .tv_block_heading(doc, "Column Specifications", level = 3)
      doc <- .tv_block_table(
        doc, .build_column_specs_table(ds@specs),
        "No column specifications available."
      )
      doc <- .tv_block_heading(doc, "Validation Rules", level = 3)
      doc <- .tv_block_table(
        doc, .build_rules_table(ds@specs@rules),
        "No validation rules specified."
      )
    }

    doc <- .add_spacer(doc)
  }
  doc
}

#' `{SUPPLIER_CONTACTS_TABLE}` / `{RECEIVER_CONTACTS_TABLE}` block: contacts for
#' one side of the agreement as a table
#' @keywords internal
.tv_block_render_contacts <- function(doc, dta, arg, side) {
  org <- if (identical(side, "supplier")) dta@metadata@supplier else dta@metadata@receiver
  title <- if (identical(side, "supplier")) "Supplier Contacts" else "Receiver Contacts"
  doc <- .tv_block_heading(doc, title, level = 2)
  .tv_block_table(doc, .build_contacts_flextable(.tv_get(org, "contacts")), "No contacts specified.")
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

#' Render one block placeholder at the cursor
#' @keywords internal
.tv_block_render <- function(doc, spec, dta) {
  switch(spec$name,
    "{COLUMN_SPECS}" = .tv_block_render_column_specs(doc, dta, spec$arg),
    "{VALIDATION_RULES}" = .tv_block_render_validation_rules(doc, dta, spec$arg),
    "{FILE_SPECS}" = .tv_block_render_file_specs(doc, dta, spec$arg),
    "{DATASETS}" = .tv_block_render_datasets(doc, dta, spec$arg),
    "{SUPPLIER_CONTACTS_TABLE}" = .tv_block_render_contacts(doc, dta, spec$arg, side = "supplier"),
    "{RECEIVER_CONTACTS_TABLE}" = .tv_block_render_contacts(doc, dta, spec$arg, side = "receiver"),
    "{SIGNATURES_TABLE}" = .tv_block_render_signatures(doc, dta, spec$arg),
    "{VERSION_HISTORY_TABLE}" = .tv_block_render_version_history(doc, dta, spec$arg),
    "{AUTHORIZED_CORRECTIONS_LIST}" = .tv_block_render_authorized_corrections(doc, dta, spec$arg),
    doc
  )
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
#' @param blocks Named character vector from [.tv_mark_block_paragraphs()]:
#'   sentinel to the token it replaced.
#' @param quiet Logical. Suppress the warning about placeholders left behind.
#' @return Invisibly, `docx_path`.
#' @keywords internal
.tv_render_blocks <- function(docx_path, dta, blocks = character(0), quiet = FALSE) {
  if (length(blocks) > 0) {
    doc <- officer::read_docx(docx_path)
    for (sentinel in names(blocks)) {
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

      # Consume the placeholder paragraph, then let the renderer append after it.
      doc <- officer::body_add_par(doc, "", pos = "on")
      doc <- .tv_block_render(doc, spec, dta)
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
      i = "Text that only reads like a placeholder because a value spelled one is listed here too, and is deliberately left as written."
    ))
  }
  invisible(docx_path)
}
