#' @title Professional Formatting Helpers for Document Export
#' @description Utility functions for creating professional document styling
#' @keywords internal
#' @importFrom officer fp_border fp_par fp_text

# Color scheme constants.
#
# These are the Boehringer Ingelheim brand palette, kept deliberately in step
# with the Shiny app's own theme (inst/shiny/dta_app/R/theme.R, the `BI` list),
# so a DTA exported from the app looks like the app it came from. The bundled
# reference template (inst/extdata/word-templates/dta_numbered_template.docx) is
# themed to match: its heading styles carry the same deep green, so Word's
# own heading colour cannot reintroduce a second, unrelated hue.
THEME_COLORS <- list(
  primary_dark = "#00625B", # BI deep green - headings, title, table header fill
  primary = "#00A886", # BI accent green - rules, secondary emphasis
  primary_light = "#E6F1EF", # Tint of the brand green - subtle fills
  accent = "#00625B", # Emphasis text (kept == primary_dark for congruence)
  gray_dark = "#1A2B2A", # BI ink - body text
  gray_mid = "#5B6B6A", # BI grey - secondary/muted text
  gray_light = "#F4F7F6", # Zebra striping
  gray_border = "#CDD5D4", # Hairline rules
  white = "#FFFFFF",
  red_error = "#C0392B" # BI status red, for attention items
)

# Font configuration.
#
# The template's theme sets majorFont (headings) to Calibri and minorFont
# (body) to Cambria, and flextable defaults to Arial -- three families in one
# document. We standardise on the heading family so headings, body text and
# tables agree.
FONTS <- list(
  primary = "Calibri",
  monospace = "Consolas"
)

FONT_SIZES <- list(
  title = 26,
  subtitle = 13,
  heading1 = 16,
  heading2 = 13,
  heading3 = 12,
  body = 10,
  table_header = 9,
  table_body = 9,
  small = 9,
  footer = 8,
  code = 6
)

#' A run format in the house style. Single source of truth for every explicitly
#' formatted run the built-in export draws, so font family and colour cannot
#' drift between call sites.
#' @keywords internal
.house_fp_text <- function(size = FONT_SIZES$body,
                           bold = FALSE,
                           italic = FALSE,
                           color = THEME_COLORS$gray_dark,
                           font = FONTS$primary) {
  officer::fp_text(
    font.family = font,
    font.size = size,
    bold = bold,
    italic = italic,
    color = color
  )
}

#' Add a body paragraph in the house style.
#'
#' `officer::body_add_par(style = "Normal")` inherits the template's Normal
#' style, which resolves to the theme's minorFont (Cambria) -- a serif face that
#' clashes with the Calibri headings and the tables. Routing body text through
#' here keeps one family throughout.
#' @keywords internal
.add_body_par <- function(doc, text,
                          size = FONT_SIZES$body,
                          bold = FALSE,
                          italic = FALSE,
                          color = THEME_COLORS$gray_dark,
                          align = "left") {
  text <- if (is.null(text) || length(text) == 0) "" else as.character(text)[1]
  if (is.na(text)) text <- ""

  officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(text, .house_fp_text(size = size, bold = bold, italic = italic, color = color)),
      fp_p = officer::fp_par(text.align = align, padding.bottom = 2)
    )
  )
}

#' Add an empty spacer paragraph.
#' @keywords internal
.add_spacer <- function(doc) {
  officer::body_add_par(doc, "", style = "Normal")
}

#' The house style for every flextable in the built-in export.
#'
#' Applying one function to all of them is the point: before this, each table
#' set its own font size, header fill and padding, so the document showed
#' several table designs at once.
#'
#' The style is deliberately light: a tinted header row closed by a brand-green
#' rule, hairlines between body rows and no zebra fill. Solid dark header bars
#' and alternating stripes made every table shout equally loud.
#' @param ft A flextable.
#' @param center_cols Integer column indices to centre; all others are left
#'   aligned.
#' @keywords internal
.style_table <- function(ft, center_cols = integer(0)) {
  hairline <- officer::fp_border(color = THEME_COLORS$gray_border, width = 0.5)
  rule <- officer::fp_border(color = THEME_COLORS$primary_dark, width = 1)

  ft <- flextable::font(ft, fontname = FONTS$primary, part = "all")
  ft <- flextable::fontsize(ft, size = FONT_SIZES$table_body, part = "body")
  ft <- flextable::fontsize(ft, size = FONT_SIZES$table_header, part = "header")
  ft <- flextable::color(ft, color = THEME_COLORS$gray_dark, part = "body")
  ft <- flextable::bg(ft, bg = THEME_COLORS$white, part = "body")
  ft <- flextable::bg(ft, bg = THEME_COLORS$primary_light, part = "header")
  ft <- flextable::color(ft, color = THEME_COLORS$primary_dark, part = "header")
  ft <- flextable::bold(ft, bold = TRUE, part = "header")

  ft <- flextable::border_remove(ft)
  ft <- flextable::hline(ft, border = hairline, part = "body")
  ft <- flextable::hline_bottom(ft, border = rule, part = "header")
  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft <- flextable::align(ft, align = "left", part = "all")
  if (length(center_cols) > 0) {
    ft <- flextable::align(ft, j = center_cols, align = "center", part = "all")
  }
  ft <- flextable::padding(ft,
    padding.top = 4, padding.bottom = 4,
    padding.left = 6, padding.right = 6, part = "all"
  )

  ft
}

# Usable text width of the bundled template's A4 page (2.5 cm margins), in
# inches. Tables are scaled to exactly this width so every table in a section
# shares the same left and right edge.
TEXT_WIDTH <- list(portrait = 6.3, landscape = 9.7)

#' Scale a flextable's column widths proportionally to a total width
#'
#' `width = NULL` leaves the table as built: user templates passed to
#' `export_with_template()` have page geometry the package cannot assume.
#' @keywords internal
.fit_table_width <- function(ft, width = TEXT_WIDTH$portrait) {
  if (is.null(width)) {
    return(ft)
  }
  # Per-column widths; flextable_dim() only reports their sum.
  w <- ft$body$colwidths
  if (is.null(w) || length(w) == 0 || sum(w) <= 0) {
    w <- rep(1, ncol(ft$body$dataset))
  }
  flextable::width(ft, j = seq_along(w), width = w * width / sum(w))
}

#' A borderless-header key/value table: muted label column, value column.
#'
#' Used for every "fact" table of the built-in layout (document information,
#' study, affiliation, transmission, dataset facts). A header row reading
#' "Item | Details" carried no information, so there is none.
#' @param pairs A data.frame with columns `key` and `value`.
#' @keywords internal
.kv_table <- function(pairs, width = TEXT_WIDTH$portrait, label_width = 1.9) {
  pairs <- data.frame(key = pairs$key, value = pairs$value, stringsAsFactors = FALSE)
  ft <- flextable::flextable(pairs)
  ft <- flextable::delete_part(ft, part = "header")
  ft <- flextable::width(ft, j = 1, width = label_width)
  ft <- flextable::width(ft, j = 2, width = width - label_width)
  hairline <- officer::fp_border(color = THEME_COLORS$gray_border, width = 0.5)
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline(ft, border = hairline, part = "body")
  ft <- flextable::hline_top(ft, border = hairline, part = "body")
  ft <- flextable::font(ft, fontname = FONTS$primary, part = "all")
  ft <- flextable::fontsize(ft, size = FONT_SIZES$table_body, part = "all")
  ft <- flextable::color(ft, j = 1, color = THEME_COLORS$gray_mid, part = "body")
  ft <- flextable::color(ft, j = 2, color = THEME_COLORS$gray_dark, part = "body")
  ft <- flextable::bold(ft, j = 1, bold = TRUE, part = "body")
  ft <- flextable::bg(ft, j = 1, bg = THEME_COLORS$gray_light, part = "body")
  ft <- flextable::bg(ft, j = 2, bg = THEME_COLORS$white, part = "body")
  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft <- flextable::align(ft, align = "left", part = "all")
  flextable::padding(ft,
    padding.top = 4, padding.bottom = 4,
    padding.left = 6, padding.right = 6, part = "all"
  )
}

#' Render any metadata value -- scalar, vector or nested list -- as display text
#'
#' `.format_scalar_value()` collapses a list to its names, which silently drops
#' structured extension values. This keeps every leaf: named elements become
#' `name: value` lines, unnamed ones bullet lines, nested levels indented.
#' @keywords internal
.format_any_value <- function(val, indent = "") {
  if (!is.list(val)) {
    return(.format_scalar_value(val, truncate = FALSE))
  }
  if (length(val) == 0) {
    return(MISSING_VALUE_DISPLAY)
  }
  nms <- names(val) %||% rep("", length(val))
  lines <- vapply(seq_along(val), function(i) {
    sub <- .format_any_value(val[[i]], indent = paste0(indent, "    "))
    if (is.list(val[[i]])) sub <- paste0("\n", sub)
    label <- if (nzchar(nms[[i]])) paste0(.title_case_field(nms[[i]]), ": ") else paste0(intToUtf8(0x2022), " ")
    paste0(indent, label, sub)
  }, character(1))
  paste(lines, collapse = "\n")
}

#' Reader-facing labels for the transmission fields the class documents.
#' Unknown (extension) keys fall back to `.title_case_field()`.
#' @keywords internal
.transmission_label <- function(key) {
  labels <- c(
    type = "Transfer method",
    frequency = "Frequency",
    notification = "Notification",
    test_upload = "Test upload",
    blinded_transfer = "Blinded transfer",
    date_first_transfer = "First transfer",
    date_last_transfer = "Last transfer",
    extent = "Extent",
    scope = "Scope",
    packaging = "Packaging",
    security = "Security",
    checksum = "Checksum",
    test_requirements = "Test requirements",
    blinding_requirements = "Blinding requirements"
  )
  unname(ifelse(key %in% names(labels), labels[key], vapply(key, .title_case_field, character(1))))
}

#' @keywords internal
.create_heading_style <- function(level = 1, color = THEME_COLORS$primary_dark) {
  size <- switch(as.character(level),
    "1" = FONT_SIZES$heading1,
    "2" = FONT_SIZES$heading2,
    "3" = FONT_SIZES$heading3,
    FONT_SIZES$body
  )

  officer::fp_text(
    font.name = FONTS$primary,
    font.size = size,
    bold = TRUE,
    color = color
  )
}

#' @keywords internal
.create_body_style <- function(size = FONT_SIZES$body, color = THEME_COLORS$gray_dark) {
  officer::fp_text(
    font.name = FONTS$primary,
    font.size = size,
    color = color
  )
}

#' @keywords internal
.create_table_header_style <- function() {
  officer::fp_par(
    text.align = "center",
    padding.top = 6,
    padding.bottom = 6,
    padding.left = 6,
    padding.right = 6
  )
}

#' @keywords internal
.create_table_cell_style <- function(align = "left") {
  officer::fp_par(
    text.align = align,
    padding.top = 4,
    padding.bottom = 4,
    padding.left = 6,
    padding.right = 6
  )
}

#' @keywords internal
.create_border <- function(pos = "all", color = THEME_COLORS$gray_border, size = 1) {
  officer::fp_border(color = color, width = size, style = "single")
}

#' @keywords internal
.format_value_list <- function(values, max_items = 5, max_width = 60) {
  if (is.null(values)) {
    return("(not specified)")
  }

  if (is.list(values)) {
    values <- unlist(values)
  }

  # Convert to character
  values <- as.character(values)

  # Truncate if too many
  if (length(values) > max_items) {
    values <- c(values[1:max_items], paste0("... and ", length(values) - max_items, " more"))
  }

  # Join with line breaks if too long
  combined <- paste(values, collapse = ", ")
  if (nchar(combined) > max_width) {
    paste(values, collapse = "\n")
  } else {
    combined
  }
}

#' Translate a rule to human-readable format
#'
#' Thin wrapper around the shared rule wording in `R/rulePhrasing.R`: the
#' complete plain-English description, one line per statement, list items
#' starting with a bullet. Rules are never rendered as code.
#' @param rule A `DTARule` object.
#' @return A single string, lines separated by `"\n"`.
#' @keywords internal
translate_rule_to_human <- function(rule) {
  .rule_text(rule)
}

# The single display string used for every missing/unset value. NULL, NA (of
# any type) and the empty string all render as this text, so a missing field
# looks the same in every export format.
MISSING_VALUE_DISPLAY <- "(not specified)"

#' Render a date in the locale-independent ISO 8601 form (`YYYY-MM-DD`)
#'
#' Exported documents must read identically no matter which workstation
#' produced them, so dates are never formatted with `%B`/`%b` (both are taken
#' from `LC_TIME`). Non-date values are passed through unchanged.
#'
#' @param x A `Date`, `POSIXt`, or any value convertible with [base::as.character()].
#' @return A character scalar; `""` for `NULL`/zero-length/`NA` input.
#' @keywords internal
.format_document_date <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("")
  }
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    x <- format(x, "%Y-%m-%d")
  } else {
    x <- as.character(x)
  }
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) {
    return("")
  }
  paste(x, collapse = ", ")
}

#' Format a single scalar value for display (shared by DOCX tables and Markdown)
#'
#' Always returns a `character(1)`. Missing values -- `NULL`, `NA` of any type,
#' and the empty string -- all render as `MISSING_VALUE_DISPLAY`
#' (`"(not specified)"`). Set `truncate = FALSE` for complete agreement text
#' instead of the default 80-character display summary.
#' @keywords internal
.format_scalar_value <- function(val, truncate = TRUE) {
  if (is.null(val) || length(val) == 0) {
    return(MISSING_VALUE_DISPLAY)
  }
  if (is.list(val)) {
    # Should rarely be hit: callers are expected to flatten nested lists
    # (e.g. affiliation/contacts) before building key-value pairs.
    nms <- names(val)
    if (is.null(nms) || !any(nzchar(nms))) {
      return(MISSING_VALUE_DISPLAY)
    }
    return(paste(nms, collapse = ", "))
  }
  # NA of any type (including the logical NA that used to slip past the
  # is.logical() branch as ifelse(NA, "Yes", "No") -> NA).
  if (length(val) == 1 && is.na(val)) {
    return(MISSING_VALUE_DISPLAY)
  }
  if (is.logical(val)) {
    return(if (isTRUE(val)) "Yes" else "No")
  }
  if (inherits(val, "Date") || inherits(val, "POSIXt")) {
    return(.format_document_date(val))
  }
  if (length(val) > 1) {
    val <- paste(as.character(val), collapse = ", ")
  }
  val <- as.character(val)
  if (is.na(val) || !nzchar(val)) {
    return(MISSING_VALUE_DISPLAY)
  }
  if (isTRUE(truncate) && nchar(val) > 80) {
    val <- paste0(substr(val, 1, 77), "...")
  }
  val
}

#' Format metadata as key-value pairs for display
#' @keywords internal
.format_metadata_pairs <- function(metadata_list, truncate = TRUE) {
  # Convert list to named vector of display strings
  pairs <- data.frame(
    key = character(),
    value = character(),
    stringsAsFactors = FALSE
  )

  for (name in names(metadata_list)) {
    val <- metadata_list[[name]]
    value_str <- .format_scalar_value(val, truncate = truncate)

    pairs <- rbind(pairs, data.frame(
      key = name,
      value = value_str,
      stringsAsFactors = FALSE
    ))
  }

  pairs
}

#' Study identity rows shared by Word and Markdown exports
#' @keywords internal
.study_to_df <- function(study) {
  labels <- c(id = "Study ID", protocol_id = "Protocol ID")
  keys <- names(labels)[names(labels) %in% names(study)]
  data.frame(
    Item = unname(labels[keys]),
    Details = vapply(keys, function(key) study[[key]], character(1)),
    stringsAsFactors = FALSE
  )
}

#' Escape narrative text rather than interpreting it as Markdown or HTML
#' @keywords internal
.plain_text_to_md <- function(text, table = FALSE) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  marks <- c("\\", "`", "*", "_", "{", "}", "[", "]", "#", "!", "~", "=")
  if (!isTRUE(table)) marks <- c(marks, "|")
  for (mark in marks) {
    text <- gsub(mark, paste0("\\", mark), text, fixed = TRUE)
  }
  text <- gsub("(?m)^([ \t]*)([-+])", "\\1\\\\\\2", text, perl = TRUE)
  text <- gsub("(?m)^([ \t]*[0-9]+)([.)])(?=[ \t])", "\\1\\\\\\2", text, perl = TRUE)

  # Visible indentation must not activate Markdown's indented-code syntax.
  indents <- gregexpr("(?m)^[ \t]+(?=\\S)", text, perl = TRUE)
  regmatches(text, indents) <- lapply(regmatches(text, indents), function(matches) {
    vapply(matches, function(indent) {
      width <- 0L
      for (char in strsplit(indent, "", fixed = TRUE)[[1]]) {
        width <- if (char == "\t") (width %/% 4L + 1L) * 4L else width + 1L
      }
      strrep("\u00a0", width)
    }, character(1))
  })
  text
}

#' Simple title-casing for arbitrary field names (e.g. "date_first_transfer")
#' @keywords internal
.title_case_field <- function(x) {
  x <- gsub("[_\\.]+", " ", x)
  words <- strsplit(x, " ")[[1]]
  words <- vapply(words, function(w) {
    if (nchar(w) == 0) {
      return(w)
    }
    paste0(toupper(substr(w, 1, 1)), substr(w, 2, nchar(w)))
  }, character(1))
  paste(words, collapse = " ")
}

#' Extract affiliation fields (name/country/address/...) as ordered key-value
#' pairs. Deliberately excludes the "contacts" element, which is rendered
#' separately: as per-contact blocks in the built-in layout, as a table in the
#' template block.
#' @keywords internal
.affiliation_pairs <- function(affiliation) {
  if (is.null(affiliation) || length(affiliation) == 0) {
    return(list())
  }

  known_order <- c("name", "country", "address")
  known_labels <- c(name = "Organization", country = "Country", address = "Address")

  out <- list()
  for (f in known_order) {
    if (!is.null(affiliation[[f]]) && nzchar(as.character(affiliation[[f]])[1])) {
      out[[known_labels[[f]]]] <- affiliation[[f]]
    }
  }

  extra_fields <- setdiff(names(affiliation), c(known_order, "contacts"))
  for (f in extra_fields) {
    if (!is.null(affiliation[[f]])) {
      out[[.title_case_field(f)]] <- affiliation[[f]]
    }
  }

  out
}

#' Convert a list of contact records into a data.frame with one row per contact,
#' exposing name, role, department, email, phone and the reviewer/backup/signature flags.
#' @keywords internal
.contacts_to_df <- function(contacts) {
  empty_df <- data.frame(
    Name = character(), Role = character(), Department = character(),
    Email = character(), Phone = character(), Reviewer = character(),
    Backup = character(), `Signature Required` = character(),
    stringsAsFactors = FALSE, check.names = FALSE
  )

  if (is.null(contacts) || length(contacts) == 0) {
    return(empty_df)
  }

  rows <- lapply(contacts, function(ct) {
    if (!is.list(ct)) {
      ct <- list(name = as.character(ct))
    }
    data.frame(
      Name = if (!is.null(ct$name)) as.character(ct$name) else "",
      Role = if (!is.null(ct$role)) as.character(ct$role) else "",
      Department = if (!is.null(ct$department)) as.character(ct$department) else "",
      Email = if (!is.null(ct$email)) as.character(ct$email) else "",
      Phone = if (!is.null(ct$phone)) as.character(ct$phone) else "",
      Reviewer = if (isTRUE(ct$reviewer)) "Yes" else "No",
      Backup = if (isTRUE(ct$backup)) "Yes" else "No",
      `Signature Required` = if (isTRUE(ct$signature)) "Yes" else "No",
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })

  do.call(rbind, rows)
}

#' Auto-derive the list of authorized signatories (Organization, Name, Role) from
#' the receiver/supplier contacts flagged with signature = TRUE. Explicit entries
#' supplied via `signature_list` are appended (duplicates by Name+Organization skipped).
#' @keywords internal
.extract_signatories <- function(meta, signature_list = NULL) {
  collect_org <- function(org_list, fallback_label) {
    if (is.null(org_list) || length(org_list) == 0) {
      return(NULL)
    }
    contacts <- org_list$contacts
    if (is.null(contacts) || length(contacts) == 0) {
      return(NULL)
    }
    org_name <- fallback_label
    if (!is.null(org_list$affiliation) && !is.null(org_list$affiliation$name)) {
      org_name <- org_list$affiliation$name
    }
    sig_contacts <- Filter(function(ct) isTRUE(ct$signature), contacts)
    if (length(sig_contacts) == 0) {
      return(NULL)
    }
    do.call(rbind, lapply(sig_contacts, function(ct) {
      data.frame(
        Organization = org_name,
        Name = if (!is.null(ct$name)) as.character(ct$name) else "",
        Role = if (!is.null(ct$role)) as.character(ct$role) else "",
        stringsAsFactors = FALSE
      )
    }))
  }

  parts <- list(
    collect_org(meta@receiver, "Receiver"),
    collect_org(meta@supplier, "Supplier")
  )
  parts <- Filter(Negate(is.null), parts)
  auto_df <- if (length(parts) > 0) do.call(rbind, parts) else NULL

  manual_df <- .normalize_signatories(signature_list)
  if (!is.null(manual_df)) {
    if (is.null(auto_df)) {
      auto_df <- manual_df
    } else {
      is_dup <- paste(manual_df$Name, manual_df$Organization) %in%
        paste(auto_df$Name, auto_df$Organization)
      if (any(!is_dup)) {
        auto_df <- rbind(auto_df, manual_df[!is_dup, , drop = FALSE])
      }
    }
  }

  auto_df
}

#' Normalize a user-supplied `signature_list` (list of list(name=, role=, organization=))
#' or an already-built data.frame(Organization, Name, Role) into a single data.frame.
#' Returns NULL if there is nothing to sign.
#' @keywords internal
.normalize_signatories <- function(signatories) {
  if (is.null(signatories)) {
    return(NULL)
  }

  if (is.data.frame(signatories)) {
    if (nrow(signatories) == 0) {
      return(NULL)
    }
    if (!"Organization" %in% names(signatories)) signatories$Organization <- ""
    if (!"Role" %in% names(signatories)) signatories$Role <- ""
    if (!"Name" %in% names(signatories)) signatories$Name <- ""
    return(signatories[, c("Organization", "Name", "Role")])
  }

  if (is.list(signatories) && length(signatories) > 0) {
    df <- do.call(rbind, lapply(signatories, function(s) {
      data.frame(
        Organization = if (!is.null(s$organization)) {
          as.character(s$organization)
        } else if (!is.null(s$org)) as.character(s$org) else "",
        Name = if (!is.null(s$name)) as.character(s$name) else "",
        Role = if (!is.null(s$role)) as.character(s$role) else "",
        stringsAsFactors = FALSE
      )
    }))
    return(df)
  }

  NULL
}

#' Extract "authorized for corrections" entries as a plain character vector,
#' regardless of whether the underlying field is a character vector or a list.
#' @keywords internal
.format_authorized_for_corrections_lines <- function(auth) {
  if (is.null(auth)) {
    return(character(0))
  }
  if (is.list(auth)) {
    auth <- unlist(auth)
  }
  as.character(auth)
}

#' Render a list of contacts as individual Markdown blocks.
#' Signatories (signature=TRUE, not a pure backup) are rendered first with a signature line.
#' Backup-only contacts are rendered after under a "Backup Contacts" sub-heading.
#' @keywords internal
.contacts_to_md_lines <- function(contacts, heading_level = 4) {
  if (is.null(contacts) || length(contacts) == 0) {
    return(character(0))
  }

  h_sub <- strrep("#", heading_level)

  # Split: backups-only vs primary (signatories/reviewers/others)
  backups <- Filter(function(ct) isTRUE(ct$backup) && !isTRUE(ct$signature), contacts)
  primaries <- Filter(function(ct) !isTRUE(ct$backup) || isTRUE(ct$signature), contacts)

  .render_contact <- function(ct, include_signature_line) {
    out <- character(0)
    nm <- if (!is.null(ct$name) && nzchar(ct$name)) ct$name else "(Unnamed)"
    out <- c(out, paste(h_sub, nm), "")

    if (!is.null(ct$role) && nzchar(ct$role)) out <- c(out, paste0("- **Role:** ", ct$role))
    if (!is.null(ct$department) && nzchar(ct$department)) out <- c(out, paste0("- **Department:** ", ct$department))
    if (!is.null(ct$email) && nzchar(ct$email)) out <- c(out, paste0("- **Email:** ", ct$email))
    if (!is.null(ct$phone) && nzchar(ct$phone)) out <- c(out, paste0("- **Phone:** ", ct$phone))

    flags <- character(0)
    if (isTRUE(ct$reviewer)) flags <- c(flags, "Reviewer")
    if (isTRUE(ct$backup)) flags <- c(flags, "Backup")
    if (length(flags) > 0) out <- c(out, paste0("- **Roles:** ", paste(flags, collapse = ", ")))

    if (include_signature_line) {
      out <- c(
        out, "",
        "Signature: __________________________________     Date: ______________",
        ""
      )
    } else {
      out <- c(out, "")
    }
    out
  }

  lines <- character(0)
  for (ct in primaries) {
    lines <- c(lines, .render_contact(ct, include_signature_line = isTRUE(ct$signature)))
  }

  if (length(backups) > 0) {
    lines <- c(lines, paste(h_sub, "Backup Contacts"), "")
    for (ct in backups) {
      lines <- c(lines, .render_contact(ct, include_signature_line = FALSE))
    }
  }

  lines
}

#' Render a data.frame as a GitHub-flavored Markdown pipe table.
#' @keywords internal
.df_to_md_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(character(0))
  }

  # A pipe table is line-based, so a cell must survive as one line. Escaping the
  # pipe alone was not enough: a description written as a YAML block scalar
  # carries real newlines, and each one split the row in two -- the renderer
  # then read the tail as a fresh table row, misattributing its text to the
  # first column and dropping whatever followed the last pipe. Newlines become
  # <br>, which GFM renders as a line break inside the cell.
  df[] <- lapply(df, function(col) {
    text <- as.character(col)
    text <- gsub("|", "\\|", text, fixed = TRUE)
    text <- gsub("\r\n|\r|\n", "<br>", text)
    text
  })

  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  rows <- apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))

  c(header, sep, unname(rows))
}

#' Render a named list of key-value pairs as Markdown bullet lines.
#' @keywords internal
.kv_bullets_md <- function(pairs_list, truncate = TRUE) {
  if (length(pairs_list) == 0) {
    return(character(0))
  }
  vapply(names(pairs_list), function(nm) {
    paste0("- **", nm, ":** ", .format_scalar_value(pairs_list[[nm]], truncate = truncate))
  }, character(1), USE.NAMES = FALSE)
}

#' Render a dataset's file specifications as a Markdown table.
#' Shared by write_dataset_metadata() and write_dta()'s per-dataset sections.
#' @keywords internal
.file_specs_to_md_lines <- function(files, heading = "### File Specifications") {
  lines <- c(heading, "")
  if (is.null(files) || length(files) == 0) {
    return(c(lines, "No files specified.", ""))
  }

  file_data <- data.frame(
    `File Pattern` = character(),
    `Expected Count` = character(),
    `Description` = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  for (f in files) {
    pattern <- if (!is.null(f@filename)) f@filename else "unspecified"
    min_n <- f@min_number_of_files %||% 0
    max_n <- f@max_number_of_files %||% 0
    count <- paste0(
      min_n,
      if (min_n != max_n) paste0("-", max_n) else ""
    )
    desc <- if (!is.null(f@pattern_description)) f@pattern_description else ""

    file_data <- rbind(file_data, data.frame(
      `File Pattern` = pattern,
      `Expected Count` = count,
      `Description` = desc,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  c(lines, .df_to_md_table(file_data), "")
}

#' Render Column Specifications + Validation Rules Markdown lines for one dataset.
#' Shared by write_dataset_metadata() and write_dta()'s per-dataset sections.
#' `base_level` controls how many '#' are used for the "Column Specifications"/
#' "Validation Rules" headings; per-column sub-headings use `base_level + 1`.
#' @keywords internal
.dataset_specs_to_md_lines <- function(dataset, include_rules = TRUE, base_level = 3) {
  if (!inherits(dataset, "DTAtools::DTADataSetTabular")) {
    return(character(0))
  }

  h <- strrep("#", base_level)
  h_sub <- strrep("#", base_level + 1)

  lines <- c(paste(h, "Column Specifications"), "")

  if (length(dataset@specs@columns) > 0) {
    for (spec in dataset@specs@columns) {
      type_str <- if (!is.null(spec@structure) && !is.null(spec@structure@type)) {
        spec@structure@type
      } else {
        "unspecified"
      }

      lines <- c(
        lines,
        paste(h_sub, spec@id),
        if (!is.null(spec@label)) paste0("**Label:** ", spec@label) else "",
        paste0("**Type:** ", type_str),
        if (!is.null(spec@nullable)) paste0("**Nullable:** ", ifelse(spec@nullable, "Yes", "No")) else "",
        if (!is.null(spec@values)) paste0("**Allowed Values:** ", .format_value_list(spec@values)) else "",
        if (!is.null(spec@pattern)) paste0("**Pattern:** ", spec@pattern) else "",
        if (!is.null(spec@description)) paste0("**Description:** ", spec@description) else "",
        ""
      )
    }
  } else {
    lines <- c(lines, "No column specifications available.", "")
  }

  if (include_rules) {
    lines <- c(lines, paste(h, "Validation Rules"), "")

    rules <- if (!is.null(dataset@specs@rules)) dataset@specs@rules else list()
    if (length(rules) > 0) {
      # Rules are described in prose only, never as code: the reading guide
      # once, then one block per rule from the shared wording.
      guide <- .plain_text_to_md(.rule_reading_guide())
      lines <- c(lines, as.vector(rbind(guide, "")))
      for (rule in rules) {
        lines <- c(lines, .rule_md(rule), "")
      }
    } else {
      lines <- c(lines, "No validation rules specified.", "")
    }
  }

  lines
}
