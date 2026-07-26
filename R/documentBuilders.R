#' @keywords internal
#' @noRd
NULL

#' Create and add a title page section to a Word document
#' @keywords internal
.add_title_section <- function(doc, title, subtitle = NULL, date = Sys.Date(), version = NULL) {
  # Add title
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_par(doc, "", style = "Normal")
  
  # Title with large font
  doc <- officer::body_add_par(
    doc,
    title,
    style = "Normal"
  )
  
  # Apply title formatting
  doc <- officer::cursor_reach(doc, keyword = title)
  if (!is.null(doc)) {
    doc <- officer::body_add_par(doc, "", style = "Normal")
  }
  
  # Subtitle
  if (!is.null(subtitle)) {
    doc <- officer::body_add_par(doc, subtitle, style = "Normal")
  }
  
  # Spacing
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_par(doc, "", style = "Normal")
  
  # Date and version
  doc <- officer::body_add_par(doc, paste("Date:", format(date, "%B %d, %Y")), style = "Normal")
  
  if (!is.null(version)) {
    doc <- officer::body_add_par(doc, paste("Version:", version), style = "Normal")
  }
  
  # Page break
  doc <- officer::body_add_break(doc)
  
  doc
}

#' Add a heading to the document
#' @keywords internal
.add_heading <- function(doc, text, level = 1) {
  style <- paste0("heading ", level)
  officer::body_add_par(doc, text, style = style)
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
    ft <- flextable::width(ft, j = 1, width = 1.5)
    ft <- flextable::width(ft, j = 2, width = 4.0)
    ft <- flextable::align(ft, align = "left", part = "all")
    ft <- flextable::bg(ft, i = 1, bg = THEME_COLORS$primary_light, part = "header")
    ft <- flextable::bold(ft, part = "header")
    
    doc <- flextable::body_add_flextable(doc, ft)
  }
  
  doc <- officer::body_add_par(doc, "", style = "Normal")
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
  ft <- flextable::align(ft, j = c(6, 7, 8), align = "center", part = "all")
  ft <- flextable::align(ft, j = c(1, 2, 3, 4, 5), align = "left", part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft <- flextable::bg(ft, i = 1, bg = THEME_COLORS$primary_light, part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::fontsize(ft, size = 10, part = "body")
  ft <- flextable::fontsize(ft, size = 10, part = "header")
  ft <- flextable::padding(ft, padding = 3, part = "all")
  
  ft
}

#' Add a full organization section (affiliation + individual contact blocks)
#' Signatories come first with a signature underline; backup-only contacts are grouped last.
#' @keywords internal
.add_organization_section <- function(doc, title, org_list) {
  doc <- .add_heading(doc, title, level = 2)
  
  if (is.null(org_list) || length(org_list) == 0) {
    doc <- officer::body_add_par(doc, "No information specified.", style = "Normal")
    return(doc)
  }
  
  # Affiliation details (name / country / address / ...)
  pairs <- .affiliation_pairs(org_list$affiliation)
  if (length(pairs) > 0) {
    aff_df <- .format_metadata_pairs(pairs)
    ft <- flextable::flextable(aff_df)
    ft <- flextable::set_header_labels(ft, key = "Item", value = "Details")
    ft <- flextable::width(ft, j = 1, width = 1.5)
    ft <- flextable::width(ft, j = 2, width = 4.0)
    ft <- flextable::align(ft, align = "left", part = "all")
    ft <- flextable::bg(ft, i = 1, bg = THEME_COLORS$primary_light, part = "header")
    ft <- flextable::bold(ft, part = "header")
    doc <- flextable::body_add_flextable(doc, ft)
    doc <- officer::body_add_par(doc, "", style = "Normal")
  }
  
  # Individual contacts - one block per contact
  contacts <- org_list$contacts
  if (!is.null(contacts) && length(contacts) > 0) {
    doc <- .add_heading(doc, "Contacts", level = 3)
    
    backups  <- Filter(function(ct) isTRUE(ct$backup) && !isTRUE(ct$signature), contacts)
    primaries <- Filter(function(ct) !isTRUE(ct$backup) || isTRUE(ct$signature), contacts)
    
    .add_contact_block <- function(doc, ct, include_signature_line) {
      nm <- if (!is.null(ct$name) && nzchar(ct$name)) ct$name else "(Unnamed)"
      
      # Name as bold paragraph
      fp_name <- officer::fp_text(bold = TRUE, font.size = FONT_SIZES$body + 1, color = THEME_COLORS$accent)
      doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(nm, fp_name)))
      
      fields <- list()
      if (!is.null(ct$role)       && nzchar(ct$role))       fields[["Role"]]       <- ct$role
      if (!is.null(ct$department) && nzchar(ct$department)) fields[["Department"]] <- ct$department
      if (!is.null(ct$email)      && nzchar(ct$email))      fields[["Email"]]      <- ct$email
      if (!is.null(ct$phone)      && nzchar(ct$phone))      fields[["Phone"]]      <- ct$phone
      
      flags <- character(0)
      if (isTRUE(ct$reviewer)) flags <- c(flags, "Reviewer")
      if (isTRUE(ct$backup))   flags <- c(flags, "Backup")
      if (length(flags) > 0)   fields[["Roles"]] <- paste(flags, collapse = ", ")
      
      if (length(fields) > 0) {
        for (nm_f in names(fields)) {
          doc <- officer::body_add_par(
            doc,
            paste0(nm_f, ":  ", fields[[nm_f]]),
            style = "Normal"
          )
        }
      }
      
      if (include_signature_line) {
        doc <- officer::body_add_par(doc, "", style = "Normal")
        doc <- officer::body_add_par(
          doc,
          "Signature: __________________________________     Date: ______________",
          style = "Normal"
        )
      }
      
      doc <- officer::body_add_par(doc, "", style = "Normal")
      doc
    }
    
    for (ct in primaries) {
      doc <- .add_contact_block(doc, ct, include_signature_line = isTRUE(ct$signature))
    }
    
    if (length(backups) > 0) {
      doc <- .add_bold_subheading(doc, "Backup Contacts")
      doc <- officer::body_add_par(doc, "", style = "Normal")
      for (ct in backups) {
        doc <- .add_contact_block(doc, ct, include_signature_line = FALSE)
      }
    }
  } else if (length(pairs) == 0) {
    doc <- officer::body_add_par(doc, "No contacts specified.", style = "Normal")
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
    doc <- officer::body_add_par(doc, paste0("\u2022  ", nm), style = "Normal")
  }
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc
}

#' Add signature approval section
#' @description
#' Accepts either a data.frame(Organization, Name, Role) (as returned by
#' \code{.extract_signatories()}), a plain list of list(name=, role=, organization=)
#' records, or NULL. One signature row is rendered per authorized signatory.
#' @keywords internal
.add_signature_section <- function(doc, signatories = NULL) {
  doc <- .add_heading(doc, "Approval & Signatures", level = 2)
  
  sig_df <- .normalize_signatories(signatories)
  
  if (!is.null(sig_df) && nrow(sig_df) > 0) {
    sig_data <- data.frame(
      Organization = sig_df$Organization,
      Name = sig_df$Name,
      Role = sig_df$Role,
      Signature = "_________________",
      Date = "____________",
      stringsAsFactors = FALSE
    )
    
    ft <- flextable::flextable(sig_data)
    ft <- flextable::width(ft, j = 1, width = 1.3)
    ft <- flextable::width(ft, j = 2, width = 1.4)
    ft <- flextable::width(ft, j = 3, width = 1.4)
    ft <- flextable::width(ft, j = 4, width = 1.7)
    ft <- flextable::width(ft, j = 5, width = 1.1)
    ft <- flextable::align(ft, j = c(1, 2, 3), align = "left", part = "all")
    ft <- flextable::align(ft, j = c(4, 5), align = "center", part = "all")
    ft <- flextable::valign(ft, valign = "top", part = "all")
    ft <- flextable::bg(ft, i = 1, bg = THEME_COLORS$primary_light, part = "header")
    ft <- flextable::bold(ft, part = "header")
    
    doc <- flextable::body_add_flextable(doc, ft)
    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- officer::body_add_par(
      doc,
      "Note: signatories listed above are contacts marked as authorized signers (signature = TRUE).",
      style = "Normal"
    )
  } else {
    # Generic fallback signature fields (no contacts with signature = TRUE were found)
    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- officer::body_add_par(doc, "Approved by: _____________________________     Date: ______________", style = "Normal")
    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- officer::body_add_par(doc, "Signature:   _____________________________", style = "Normal")
  }
  
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc
}

#' Add file specifications section
#' @keywords internal
.add_file_specifications <- function(doc, files, title = "File Specifications", heading_level = 2) {
  doc <- if (is.null(heading_level)) .add_bold_subheading(doc, title) else .add_heading(doc, title, level = heading_level)
  
  if (is.null(files) || length(files) == 0) {
    doc <- officer::body_add_par(doc, "No files specified.", style = "Normal")
    return(doc)
  }
  
  total_min <- sum(sapply(files, function(f) {
    tryCatch(f@min_number_of_files %||% 0, error = function(e) 0)
  }))
  
  total_max <- sum(sapply(files, function(f) {
    tryCatch(f@max_number_of_files %||% 0, error = function(e) 0)
  }))
  
  doc <- officer::body_add_par(
    doc,
    paste0("Expected number of files: ", total_min, " to ", total_max),
    style = "Normal"
  )
  
  # Create file listing table
  file_data <- data.frame(
    `File Name Pattern` = character(),
    `Expected Count` = character(),
    `Description` = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  for (i in seq_along(files)) {
    f <- files[[i]]
    min_n <- f@min_number_of_files %||% 0
    max_n <- f@max_number_of_files %||% 0
    file_data <- rbind(file_data, data.frame(
      `File Name Pattern` = if (!is.null(f@filename)) f@filename else "pattern_not_specified",
      `Expected Count` = paste0(
        min_n,
        if (min_n != max_n) paste0(" to ", max_n) else ""
      ),
      `Description` = if (!is.null(f@pattern_description)) f@pattern_description else "",
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }
  
  ft <- flextable::flextable(file_data)
  ft <- flextable::width(ft, j = 1, width = 2.0)
  ft <- flextable::width(ft, j = 2, width = 1.2)
  ft <- flextable::width(ft, j = 3, width = 3.0)
  ft <- flextable::align(ft, j = 2, align = "center", part = "all")
  ft <- flextable::align(ft, j = c(1, 3), align = "left", part = "all")
  ft <- flextable::bg(ft, i = 1, bg = THEME_COLORS$primary_light, part = "header")
  ft <- flextable::bold(ft, part = "header")
  
  doc <- flextable::body_add_flextable(doc, ft)
  doc <- officer::body_add_par(doc, "", style = "Normal")
  
  doc
}

#' Add a bold subheading paragraph — used for nesting deeper than officer's default
#' template supports (it only defines "heading 1" through "heading 3").
#' @keywords internal
.add_bold_subheading <- function(doc, text) {
  fp <- officer::fp_text(bold = TRUE, font.size = FONT_SIZES$body + 1, color = THEME_COLORS$accent)
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
  ft <- flextable::bg(ft, i = 1, bg = THEME_COLORS$primary_light, part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft
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
  
  doc <- add_section_heading(doc, "Column Specifications")
  ft <- .build_column_specs_table(dataset@specs)
  if (!is.null(ft)) {
    doc <- flextable::body_add_flextable(doc, ft)
  } else {
    doc <- officer::body_add_par(doc, "No column specifications available.", style = "Normal")
  }
  doc <- officer::body_add_par(doc, "", style = "Normal")
  
  if (include_rules) {
    doc <- add_section_heading(doc, "Validation Rules")
    ft_rules <- .build_rules_table(dataset@specs@rules)
    if (!is.null(ft_rules)) {
      doc <- flextable::body_add_flextable(doc, ft_rules)
    } else {
      doc <- officer::body_add_par(doc, "No validation rules specified.", style = "Normal")
    }
    doc <- officer::body_add_par(doc, "", style = "Normal")
  }
  
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
  ft <- flextable::width(ft, j = 1, width = 1.2)  # Variable Name
  ft <- flextable::width(ft, j = 2, width = 1.2)  # Label
  ft <- flextable::width(ft, j = 3, width = 1.0)  # Type
  ft <- flextable::width(ft, j = 4, width = 0.8)  # Nullable
  ft <- flextable::width(ft, j = 5, width = 2.0)  # Values/Pattern
  ft <- flextable::width(ft, j = 6, width = 2.2)  # Description
  
  # Formatting
  ft <- flextable::align(ft, j = 4, align = "center", part = "all")
  ft <- flextable::align(ft, j = c(1, 2, 3, 5, 6), align = "left", part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft <- flextable::bg(ft, i = 1, bg = THEME_COLORS$primary_light, part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::fontsize(ft, size = 10, part = "body")
  ft <- flextable::fontsize(ft, size = 11, part = "header")
  ft <- flextable::padding(ft, padding = 3, part = "all")
  
  ft
}

#' Format validation rules as a professional list
#' @keywords internal
.build_validation_rules_section <- function(doc, rules_collection) {
  if (is.null(rules_collection) || length(rules_collection) == 0) {
    doc <- officer::body_add_par(doc, "No validation rules specified.", style = "Normal")
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
  
  doc <- officer::body_add_par(doc, "", style = "Normal")
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
  
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_par(doc, footer_text, style = "Normal")
  
  doc
}
