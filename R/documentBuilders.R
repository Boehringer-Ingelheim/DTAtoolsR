#' @keywords internal
#' @noRd
NULL

#' Create and add a title page section to a Word document
#' @keywords internal
.add_title_section <- function(doc, title, subtitle = NULL, date = Sys.Date(), version = NULL) {
  # A DTA may legitimately carry datasets without metadata (e.g. a standalone
  # dataset YAML), in which case `title` is NULL/empty. Normalize it to a single,
  # safe display string so we never pass a zero-length or NA value to officer.
  title_chr <- if (is.null(title) || length(title) == 0) "" else as.character(title)[1]
  if (is.na(title_chr)) title_chr <- ""
  display_title <- if (nzchar(title_chr)) title_chr else "Data Transfer Agreement"

  for (i in seq_len(6)) {
    doc <- .add_spacer(doc)
  }

  # Title. (Previously this reached the cursor back to the title via
  # officer::cursor_reach(keyword = title) before inserting the blank line, but
  # that matches the title as a regex and aborted the whole export when the
  # title was empty/NULL or contained regex metacharacters. The cursor already
  # sits on the freshly added paragraph, so a direct insert is equivalent and
  # robust.)
  doc <- .add_body_par(
    doc, display_title,
    size = FONT_SIZES$title, bold = TRUE, color = THEME_COLORS$primary_dark
  )

  # Hairline rule under the title, drawn as a bottom-bordered empty paragraph.
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext("", .house_fp_text(size = FONT_SIZES$body)),
      fp_p = officer::fp_par(
        border.bottom = officer::fp_border(color = THEME_COLORS$primary, width = 1.5),
        padding.bottom = 6
      )
    )
  )

  if (!is.null(subtitle) && nzchar(subtitle)) {
    doc <- .add_body_par(
      doc, subtitle,
      size = FONT_SIZES$subtitle, color = THEME_COLORS$gray_mid
    )
  }

  doc <- .add_spacer(doc)

  # ISO 8601 (YYYY-MM-DD): locale-independent, unlike "%B %d, %Y", whose month
  # name is taken from LC_TIME and differs per workstation.
  doc <- .add_body_par(doc, paste("Date:", .format_document_date(date)), color = THEME_COLORS$gray_mid)
  if (!is.null(version)) {
    doc <- .add_body_par(doc, paste("Version:", version), color = THEME_COLORS$gray_mid)
  }

  doc <- officer::body_add_break(doc)

  doc
}

#' Add a heading to the document
#' @keywords internal
.add_heading <- function(doc, text, level = 1) {
  style <- paste0("heading ", level)
  officer::body_add_par(doc, text, style = style)
}

#' Open the bundled numbered reference template so document headings 1-4
#' auto-number as 1 / 1.1 / 1.1.1 / 1.1.1.1 (true Word list fields that renumber
#' when the document is edited). Falls back to officer's default template if the
#' bundled asset cannot be located, in which case level-4 headings render without
#' a computed number.
#' @keywords internal
.new_numbered_docx <- function() {
  path <- system.file(
    "extdata", "templates", "dta_numbered_template.docx",
    package = "DTAtools"
  )
  if (nzchar(path) && file.exists(path)) {
    return(officer::read_docx(path = path))
  }
  officer::read_docx()
}

#' Append the machine-readable YAML specification as a final, very small-font
#' monospace section. Leading spaces are converted to non-breaking spaces so the
#' YAML indentation survives Word's whitespace collapsing.
#' @keywords internal
.add_embedded_yaml_section <- function(doc, yaml_text) {
  doc <- .add_heading(doc, "Embedded Specification (YAML)", level = 2)
  doc <- .add_body_par(
    doc,
    "The machine-readable YAML specification is embedded below for reference."
  )
  doc <- .add_spacer(doc)

  fp <- .house_fp_text(size = FONT_SIZES$code, color = THEME_COLORS$gray_dark, font = FONTS$monospace)
  lines <- strsplit(gsub("\r\n", "\n", yaml_text, fixed = TRUE), "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0) lines <- ""
  for (ln in lines) {
    # Preserve indentation: replace each leading space with a non-breaking space.
    lead <- attr(regexpr("^ *", ln), "match.length")
    if (is.null(lead) || is.na(lead)) lead <- 0L
    disp <- paste0(strrep("\u00a0", lead), substring(ln, lead + 1))
    if (!nzchar(disp)) disp <- "\u00a0"
    doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(disp, fp)))
  }
  doc <- .add_spacer(doc)
  doc
}

#' Add metadata section with key-value pairs
#' @keywords internal
.add_metadata_section <- function(doc, title, metadata_list) {
  if (!is.null(title) && nzchar(title)) {
    doc <- .add_heading(doc, title, level = 2)
  }

  # Create a two-column table for metadata
  if (length(metadata_list) > 0) {
    pairs <- .format_metadata_pairs(metadata_list)

    # Create flextable
    ft <- flextable::flextable(pairs)
    ft <- flextable::set_header_labels(ft, key = "Item", value = "Details")
    ft <- flextable::width(ft, j = 1, width = 1.6)
    ft <- flextable::width(ft, j = 2, width = 4.7)
    ft <- .style_table(ft)
    ft <- flextable::bold(ft, j = 1, bold = TRUE, part = "body")

    doc <- flextable::body_add_flextable(doc, ft)
  }

  doc <- .add_spacer(doc)
  doc
}

#' Build a professional flextable of individual contacts (one row per person)
#' @keywords internal
.build_contacts_flextable <- function(contacts) {
  df <- .contacts_to_df(contacts)
  if (nrow(df) == 0) {
    return(NULL)
  }

  ft <- flextable::flextable(df)
  ft <- flextable::width(ft, j = 1, width = 1.3)
  ft <- flextable::width(ft, j = 2, width = 1.5)
  ft <- flextable::width(ft, j = 3, width = 1.3)
  ft <- flextable::width(ft, j = 4, width = 2.0)
  ft <- flextable::width(ft, j = 5, width = 1.1)
  ft <- flextable::width(ft, j = 6, width = 0.8)
  ft <- flextable::width(ft, j = 7, width = 0.8)
  ft <- flextable::width(ft, j = 8, width = 1.1)
  ft <- .style_table(ft, center_cols = c(6, 7, 8))

  ft
}

#' Add a full organization section (affiliation + individual contact blocks).
#' Backup-only contacts are grouped last. Signature lines are NOT drawn here:
#' authorised signatories are collected into the single "Approval & Signatures"
#' table at the front of the document, so repeating an underline per contact
#' would be a second, competing place to sign.
#' @keywords internal
.add_organization_section <- function(doc, title, org_list) {
  doc <- .add_heading(doc, title, level = 2)

  if (is.null(org_list) || length(org_list) == 0) {
    doc <- .add_body_par(doc, "No information specified.", italic = TRUE, color = THEME_COLORS$gray_mid)
    return(doc)
  }

  # Affiliation details (name / country / address / ...)
  pairs <- .affiliation_pairs(org_list$affiliation)
  if (length(pairs) > 0) {
    aff_df <- .format_metadata_pairs(pairs)
    ft <- flextable::flextable(aff_df)
    ft <- flextable::set_header_labels(ft, key = "Item", value = "Details")
    ft <- flextable::width(ft, j = 1, width = 1.6)
    ft <- flextable::width(ft, j = 2, width = 4.7)
    ft <- .style_table(ft)
    ft <- flextable::bold(ft, j = 1, bold = TRUE, part = "body")
    doc <- flextable::body_add_flextable(doc, ft)
    doc <- .add_spacer(doc)
  }

  # Individual contacts - one block per contact
  contacts <- org_list$contacts
  if (!is.null(contacts) && length(contacts) > 0) {
    doc <- .add_heading(doc, "Contacts", level = 3)

    backups <- Filter(function(ct) isTRUE(ct$backup) && !isTRUE(ct$signature), contacts)
    primaries <- Filter(function(ct) !isTRUE(ct$backup) || isTRUE(ct$signature), contacts)

    .add_contact_block <- function(doc, ct) {
      nm <- if (!is.null(ct$name) && nzchar(ct$name)) ct$name else "(Unnamed)"
      doc <- .add_body_par(doc, nm, bold = TRUE, color = THEME_COLORS$primary_dark)

      fields <- list()
      if (!is.null(ct$role) && nzchar(ct$role)) fields[["Role"]] <- ct$role
      if (!is.null(ct$department) && nzchar(ct$department)) fields[["Department"]] <- ct$department
      if (!is.null(ct$email) && nzchar(ct$email)) fields[["Email"]] <- ct$email
      if (!is.null(ct$phone) && nzchar(ct$phone)) fields[["Phone"]] <- ct$phone

      flags <- character(0)
      if (isTRUE(ct$reviewer)) flags <- c(flags, "Reviewer")
      if (isTRUE(ct$backup)) flags <- c(flags, "Backup")
      if (isTRUE(ct$signature)) flags <- c(flags, "Signatory")
      if (length(flags) > 0) fields[["Roles"]] <- paste(flags, collapse = ", ")

      for (nm_f in names(fields)) {
        doc <- officer::body_add_fpar(
          doc,
          officer::fpar(
            officer::ftext(paste0(nm_f, ": "), .house_fp_text(size = FONT_SIZES$small, color = THEME_COLORS$gray_mid)),
            officer::ftext(fields[[nm_f]], .house_fp_text(size = FONT_SIZES$small)),
            fp_p = officer::fp_par(text.align = "left", padding.bottom = 1)
          )
        )
      }

      doc <- .add_spacer(doc)
      doc
    }

    for (ct in primaries) {
      doc <- .add_contact_block(doc, ct)
    }

    if (length(backups) > 0) {
      doc <- .add_bold_subheading(doc, "Backup Contacts")
      doc <- .add_spacer(doc)
      for (ct in backups) {
        doc <- .add_contact_block(doc, ct)
      }
    }
  } else if (length(pairs) == 0) {
    doc <- .add_body_par(doc, "No contacts specified.", italic = TRUE, color = THEME_COLORS$gray_mid)
  }

  doc
}

#' Add "Authorized for Corrections" section
#' @keywords internal
.add_authorized_for_corrections_section <- function(doc, authorized_for_corrections) {
  names_vec <- .format_authorized_for_corrections_lines(authorized_for_corrections)
  if (length(names_vec) == 0) {
    return(doc)
  }

  doc <- .add_heading(doc, "Authorized for Corrections", level = 2)
  for (nm in names_vec) {
    doc <- .add_body_par(doc, paste0("\u2022  ", nm))
  }
  doc <- .add_spacer(doc)
  doc
}

#' Add signature approval section.
#' @description
#' Accepts either a data.frame(Organization, Name, Role) (as returned by
#' \code{.extract_signatories()}), a plain list of list(name=, role=, organization=)
#' records, or NULL. One signature row is rendered per authorized signatory.
#'
#' When no signatory is known the section is omitted entirely rather than
#' padded with blank "Approved by / Signature" rules: an anonymous underline
#' is not something a reader can act on, and it left the document with a
#' heading whose content was pure filler.
#' @return The document, unchanged when there is nothing to sign.
#' @keywords internal
.add_signature_section <- function(doc, signatories = NULL) {
  sig_df <- .normalize_signatories(signatories)
  if (is.null(sig_df) || nrow(sig_df) == 0) {
    return(doc)
  }

  doc <- .add_heading(doc, "Approval & Signatures", level = 2)

  sig_data <- data.frame(
    Organization = sig_df$Organization,
    Name = sig_df$Name,
    Role = sig_df$Role,
    Signature = "",
    Date = "",
    stringsAsFactors = FALSE
  )

  ft <- flextable::flextable(sig_data)
  ft <- flextable::width(ft, j = 1, width = 1.3)
  ft <- flextable::width(ft, j = 2, width = 1.3)
  ft <- flextable::width(ft, j = 3, width = 1.5)
  ft <- flextable::width(ft, j = 4, width = 1.4)
  ft <- flextable::width(ft, j = 5, width = 0.8)
  ft <- .style_table(ft)
  ft <- flextable::bold(ft, j = 2, bold = TRUE, part = "body")
  # Signature/Date are filled in by hand: give them room and a ruled baseline
  # instead of a row of underscores.
  ft <- flextable::height(ft, height = 0.42, part = "body")
  ft <- flextable::hrule(ft, rule = "atleast", part = "body")
  ft <- flextable::bg(ft, j = c(4, 5), bg = THEME_COLORS$white, part = "body")

  doc <- flextable::body_add_flextable(doc, ft)
  doc <- .add_spacer(doc)
  doc
}

#' Add file specifications section
#' @keywords internal
.add_file_specifications <- function(doc, files, title = "File Specifications", heading_level = 2) {
  doc <- if (is.null(heading_level)) .add_bold_subheading(doc, title) else .add_heading(doc, title, level = heading_level)

  if (is.null(files) || length(files) == 0) {
    doc <- .add_body_par(doc, "No files specified.", italic = TRUE, color = THEME_COLORS$gray_mid)
    return(doc)
  }
  total_min <- sum(sapply(files, function(f) {
    tryCatch(f@min_number_of_files %||% 0, error = function(e) 0)
  }))

  total_max <- sum(sapply(files, function(f) {
    tryCatch(f@max_number_of_files %||% 0, error = function(e) 0)
  }))

  file_word <- if (total_min == 1 && total_max == 1) "file" else "files"
  count_txt <- if (total_min != total_max) {
    paste0(total_min, " to ", total_max)
  } else {
    as.character(total_min)
  }
  doc <- .add_body_par(
    doc,
    paste0(
      "The following ", file_word, " are expected for this dataset (",
      count_txt, " in total). Each file name is given either as an exact ",
      "name or as a regular-expression pattern that a delivered file must ",
      "match; the expected count states how many files may match it."
    )
  )
  doc <- .add_spacer(doc)

  # Create file listing table
  file_data <- data.frame(
    `File Name / Pattern` = character(),
    `Match Type` = character(),
    `Format` = character(),
    `Expected Count` = character(),
    `Description` = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  for (i in seq_along(files)) {
    f <- files[[i]]
    min_n <- f@min_number_of_files %||% 0
    max_n <- f@max_number_of_files %||% 0
    match_type <- if (isTRUE(f@pattern)) "Regex pattern" else "Exact name"

    desc <- if (!is.null(f@pattern_description)) f@pattern_description else ""
    info_txt <- tryCatch(f@info, error = function(e) NULL)
    if (!is.null(info_txt) && length(info_txt) > 0) {
      info_flat <- paste(unlist(info_txt), collapse = "; ")
      if (nzchar(info_flat)) {
        desc <- if (nzchar(desc)) paste0(desc, " (", info_flat, ")") else info_flat
      }
    }

    file_data <- rbind(file_data, data.frame(
      `File Name / Pattern` = if (!is.null(f@filename)) paste(f@filename, collapse = ", ") else "pattern_not_specified",
      `Match Type` = match_type,
      `Format` = .file_format_label(f),
      `Expected Count` = paste0(
        min_n,
        if (min_n != max_n) paste0(" to ", max_n) else ""
      ),
      `Description` = desc,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  ft <- flextable::flextable(file_data)
  ft <- flextable::width(ft, j = 1, width = 1.8)
  ft <- flextable::width(ft, j = 2, width = 1.0)
  ft <- flextable::width(ft, j = 3, width = 0.9)
  ft <- flextable::width(ft, j = 4, width = 1.0)
  ft <- flextable::width(ft, j = 5, width = 2.5)
  ft <- .style_table(ft, center_cols = c(2, 3, 4))

  doc <- flextable::body_add_flextable(doc, ft)
  doc <- .add_spacer(doc)

  doc
}

#' Friendly, human-readable format label for a DTAFile object, used in the
#' "Files" section table (e.g. CSV / TSV / Delimited).
#' @keywords internal
.file_format_label <- function(f) {
  if (inherits(f, "DTAtools::DTAFileCSV")) {
    "CSV"
  } else if (inherits(f, "DTAtools::DTAFileTSV")) {
    "TSV"
  } else if (inherits(f, "DTAtools::DTAFileDelim")) {
    "Delimited"
  } else if (inherits(f, "DTAtools::DTAFileTabular")) {
    "Tabular"
  } else {
    "File"
  }
}

#' Add a bold subheading paragraph — used for nesting deeper than officer's default
#' template supports (it only defines "heading 1" through "heading 3").
#' @keywords internal
.add_bold_subheading <- function(doc, text) {
  fp <- .house_fp_text(size = FONT_SIZES$heading3, bold = TRUE, color = THEME_COLORS$primary_dark)
  officer::body_add_fpar(doc, officer::fpar(officer::ftext(text, fp)))
}

#' Build a validation rules table (flextable). Shared by write_dataset_metadata()
#' and write_dta()'s per-dataset sections.
#' @param rules_collection A list of DTARule objects (e.g. `specs@rules`) or `NULL`.
#' @keywords internal
.build_rules_table <- function(rules_collection) {
  rules <- if (!is.null(rules_collection)) rules_collection else list()
  if (length(rules) == 0) {
    return(NULL)
  }

  rule_data <- data.frame(
    `Rule ID` = character(),
    `Description` = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  for (rule in rules) {
    human_desc <- translate_rule_to_human(rule)
    rule_data <- rbind(rule_data, data.frame(
      `Rule ID` = rule@id,
      `Description` = human_desc,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  ft <- flextable::flextable(rule_data)
  ft <- flextable::width(ft, j = 1, width = 1.2)
  ft <- flextable::width(ft, j = 2, width = 5.0)
  ft <- .style_table(ft)
  ft
}

#' Close the current document section with the given page orientation.
#'
#' officer's `body_end_section_*()` helpers terminate the content added *before*
#' the call, so a landscape block is produced by ending the preceding content as
#' portrait, adding the block, and only then ending that block as landscape.
#' Page size and margins are read back from the document rather than hardcoded,
#' so a Letter-based template is not silently forced onto A4, and the section
#' type is `nextPage` rather than officer's `oddPage` default, which would pad
#' the document with blank pages.
#' @keywords internal
.end_section_orientation <- function(doc, orient = c("portrait", "landscape")) {
  orient <- match.arg(orient)

  dims <- officer::docx_dim(doc)
  short_side <- min(dims$page[["width"]], dims$page[["height"]])
  long_side <- max(dims$page[["width"]], dims$page[["height"]])

  size <- if (identical(orient, "landscape")) {
    officer::page_size(width = long_side, height = short_side, orient = "landscape")
  } else {
    officer::page_size(width = short_side, height = long_side, orient = "portrait")
  }

  # docx_dim() reports top/bottom/left/right/header/footer only; gutter is absent.
  gutter <- if ("gutter" %in% names(dims$margins)) dims$margins[["gutter"]] else 0

  margins <- officer::page_mar(
    top = dims$margins[["top"]],
    bottom = dims$margins[["bottom"]],
    left = dims$margins[["left"]],
    right = dims$margins[["right"]],
    header = dims$margins[["header"]],
    footer = dims$margins[["footer"]],
    gutter = gutter
  )

  officer::body_end_block_section(
    doc,
    officer::block_section(
      officer::prop_section(page_size = size, page_margins = margins, type = "nextPage")
    )
  )
}

#' Add Column Specifications + Validation Rules sections for one dataset (DOCX).
#' Used standalone by write_dataset_metadata() (heading_level = 2) and embedded
#' within write_dta()'s per-dataset "Datasets" section (heading_level = NULL, which
#' uses bold subheadings instead of doc heading styles to avoid needing a
#' non-existent "heading 4" style).
#' @keywords internal
.add_dataset_specs_section <- function(doc, dataset, include_rules = TRUE, heading_level = 2) {
  if (!inherits(dataset, "DTAtools::DTADataSetTabular")) {
    return(doc)
  }

  add_section_heading <- function(doc, text) {
    if (is.null(heading_level)) .add_bold_subheading(doc, text) else .add_heading(doc, text, level = heading_level)
  }

  # The column specifications table is 8.4in wide and does not fit the ~6.3in
  # text column of an A4 portrait page, so the table block gets landscape pages
  # of its own. officer terminates a section for the content *already added*,
  # hence: close the preceding portrait content first, add the tables, then
  # close them as landscape.
  doc <- .end_section_orientation(doc, "portrait")

  doc <- add_section_heading(doc, "Column Specifications")
  ft <- .build_column_specs_table(dataset@specs)
  if (!is.null(ft)) {
    doc <- flextable::body_add_flextable(doc, ft)
  } else {
    doc <- .add_body_par(doc, "No column specifications available.", italic = TRUE, color = THEME_COLORS$gray_mid)
  }
  doc <- .add_spacer(doc)

  if (include_rules) {
    doc <- add_section_heading(doc, "Validation Rules")
    ft_rules <- .build_rules_table(dataset@specs@rules)
    if (!is.null(ft_rules)) {
      doc <- flextable::body_add_flextable(doc, ft_rules)
    } else {
      doc <- .add_body_par(doc, "No validation rules specified.", italic = TRUE, color = THEME_COLORS$gray_mid)
    }
    doc <- .add_spacer(doc)
  }

  doc <- .end_section_orientation(doc, "landscape")

  doc
}

#' Format column specifications as a professional table
#' @keywords internal
.build_column_specs_table <- function(specs_collection) {
  if (is.null(specs_collection) || length(specs_collection@columns) == 0) {
    return(NULL)
  }

  specs <- specs_collection@columns

  column_data <- data.frame(
    `Variable Name` = character(),
    `Label` = character(),
    `Type` = character(),
    `Nullable` = character(),
    `Allowed Values / Pattern` = character(),
    `Description` = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  for (spec in specs) {
    # Get type
    type_str <- "unspecified"
    if (!is.null(spec@structure) && !is.null(spec@structure@type)) {
      type_str <- spec@structure@type
      if (!is.null(spec@structure@backend) && nzchar(spec@structure@backend)) {
        type_str <- paste0(spec@structure@backend, " ", type_str)
      }
    }

    # Get nullable
    nullable_str <- if (!is.null(spec@nullable)) {
      ifelse(spec@nullable, "Yes", "No")
    } else {
      "Not specified"
    }

    # Get allowed values or pattern
    values_str <- ""
    if (!is.null(spec@values)) {
      values_str <- .format_value_list(spec@values, max_items = 10, max_width = 100)
    } else if (!is.null(spec@pattern)) {
      values_str <- paste0("Pattern: ", spec@pattern)
    }

    # Get description
    desc_str <- if (!is.null(spec@description)) spec@description else ""
    if (!is.null(spec@examples) && length(spec@examples) > 0) {
      examples_str <- .format_value_list(spec@examples, max_items = 3)
      desc_str <- paste0(desc_str, "\nExamples: ", examples_str)
    }

    column_data <- rbind(column_data, data.frame(
      `Variable Name` = spec@id,
      `Label` = if (!is.null(spec@label)) spec@label else "",
      `Type` = type_str,
      `Nullable` = nullable_str,
      `Allowed Values / Pattern` = values_str,
      `Description` = desc_str,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  # Create flextable
  ft <- flextable::flextable(column_data)

  # Set column widths
  ft <- flextable::width(ft, j = 1, width = 1.2) # Variable Name
  ft <- flextable::width(ft, j = 2, width = 1.2) # Label
  ft <- flextable::width(ft, j = 3, width = 1.0) # Type
  ft <- flextable::width(ft, j = 4, width = 0.8) # Nullable
  ft <- flextable::width(ft, j = 5, width = 2.0) # Values/Pattern
  ft <- flextable::width(ft, j = 6, width = 2.2) # Description

  # Formatting. Column specs are dense, so this table takes the compact variant
  # of the house style: same palette and family, one point smaller.
  ft <- .style_table(ft, center_cols = 4)
  ft <- flextable::fontsize(ft, size = FONT_SIZES$table_body - 1, part = "body")

  ft
}

#' Format validation rules as a professional list
#' @keywords internal
.build_validation_rules_section <- function(doc, rules_collection) {
  if (is.null(rules_collection) || length(rules_collection) == 0) {
    doc <- .add_body_par(doc, "No validation rules specified.", italic = TRUE, color = THEME_COLORS$gray_mid)
    return(doc)
  }

  rules <- rules_collection

  # Create a list of rules
  for (rule in rules) {
    # Rule ID as sub-heading
    doc <- officer::body_add_par(
      doc,
      rule@id,
      style = "List Bullet"
    )

    # Rule description
    human_desc <- translate_rule_to_human(rule)
    doc <- officer::body_add_par(
      doc,
      human_desc,
      style = "List Bullet 2"
    )
  }

  doc <- .add_spacer(doc)
  doc
}

#' Add footer with page numbers and metadata
#' @keywords internal
.add_footer_section <- function(doc, version = NULL, template_version = NULL, document_date = Sys.Date()) {
  # This is typically handled through section properties, but for simplicity,
  # we can add a footer-like paragraph at the end

  footer_text <- paste(
    "Generated:",
    format(Sys.time(), "%Y-%m-%d %H:%M"),
    if (!is.null(version)) paste("| Document Version:", version) else "",
    if (!is.null(template_version)) paste("| Template Version:", template_version) else "",
    sep = " "
  )

  doc <- .add_spacer(doc)
  doc <- .add_body_par(doc, footer_text, size = FONT_SIZES$footer, color = THEME_COLORS$gray_mid)

  doc
}
