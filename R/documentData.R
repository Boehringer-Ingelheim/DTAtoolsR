# Shared data builders for the Word and Markdown document exports.
#
# Each builder turns DTA objects into plain, UNESCAPED text: a data.frame or a
# character scalar, never a flextable and never Markdown. The Word builders in
# documentBuilders.R wrap these data in flextables; the Markdown writer escapes
# them when it renders. Keeping the content here is what makes both formats
# state the same facts in the same order with the same words.
#
# Conventions
# * "Pairs" builders return a data.frame with the character columns `key` and
#   `value` (zero rows when there is nothing to show), which is what .kv_table()
#   consumes.
# * "Table" builders return a data.frame whose column names are the headers of
#   the Word table (check.names = FALSE); zero rows when there is nothing to
#   show. Cells may contain "\n" line breaks and the bullet U+2022 that
#   .format_any_value() produces.
# * Missing scalars become "" (and the row is dropped from a pairs builder).
#   A value the specification requires but does not give (a file name, a column
#   type, a file encoding) reads MISSING_VALUE_DISPLAY; "(none)" means a list
#   that is given and empty.

# A character data.frame with the given column names and no rows.
.empty_text_df <- function(columns) {
  stats::setNames(
    as.data.frame(rep(list(character()), length(columns)), stringsAsFactors = FALSE),
    columns
  )
}

# A named character vector as a key/value data.frame.
.pairs_df <- function(x) {
  data.frame(
    key = as.character(names(x) %||% character(0)),
    value = as.character(unname(x)),
    stringsAsFactors = FALSE
  )
}

# Row-bind a list of one-row data.frames (or return `empty` when there are none).
.rbind_rows <- function(rows, empty) {
  if (length(rows) == 0) {
    return(empty)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Facts of the cover page: version, date, study, parties and template.
# Empty values are dropped. `meta` is a DTAMetaData.
.cover_facts <- function(meta) {
  party <- function(org) org$affiliation$name %||% ""
  facts <- c(
    Version = meta@version %||% "",
    Date = .format_document_date(.current_version_date(meta)),
    `Study ID` = meta@study$id %||% "",
    `Protocol ID` = meta@study$protocol_id %||% "",
    Supplier = party(meta@supplier),
    Receiver = party(meta@receiver),
    Template = if (length(meta@template) > 0) sub("^Template: ", "", .template_provenance_text(meta@template)) else ""
  )
  .pairs_df(facts[nzchar(facts)])
}

# Rows of the "Document Information" table: title, header, version, date and,
# for a DTA created from a template, where it came from. Empty values are
# dropped. `meta` is a DTAMetaData.
.document_info_pairs <- function(meta) {
  info <- c(
    Title = .dta_display_title(meta@title),
    `Document header` = meta@header %||% "",
    Version = meta@version %||% "",
    Date = .format_document_date(.current_version_date(meta))
  )
  if (length(meta@template) > 0) {
    tpl <- meta@template
    info <- c(info,
      Template = if (!is.null(tpl$label) && !identical(tpl$label, tpl$id)) paste0(tpl$label, " (", tpl$id, ")") else tpl$id,
      `Template version` = tpl$version %||% "",
      `Template date` = .format_document_date(tpl$date)
    )
  }
  .pairs_df(info[nzchar(info)])
}

# Rows of a dataset's fact table inside write_dta(): id, type and the template
# the dataset was taken from. Empty values are dropped. `dataset` is a
# DTADataSet.
.dataset_facts <- function(dataset) {
  facts <- c(
    ID = dataset@id,
    Type = .title_case_field(dataset@type %||% ""),
    `Template source` = dataset@template_source %||% "",
    `Template version` = dataset@template_version %||% "",
    `Template date` = dataset@template_date %||% ""
  )
  .pairs_df(facts[nzchar(facts)])
}

# Rows of the "Transmission Details" table: reader-facing label and the full
# value (nested values keep every leaf). `transmission` is
# DTAMetaData@transmission; zero rows when it is empty.
.transmission_pairs <- function(transmission) {
  trans <- as.list(transmission)
  if (length(trans) == 0) {
    return(.pairs_df(character(0)))
  }
  data.frame(
    key = .transmission_label(names(trans)),
    value = unname(vapply(trans, .format_any_value, character(1))),
    stringsAsFactors = FALSE
  )
}

# Affiliation rows of a party (organization, country, address and every
# extension field) as key/value pairs; zero rows when there are none. `org` is
# DTAMetaData@supplier or @receiver.
.party_facts <- function(org) {
  pairs <- .affiliation_pairs(org$affiliation)
  if (length(pairs) == 0) {
    return(.pairs_df(character(0)))
  }
  data.frame(
    key = names(pairs),
    value = unname(vapply(pairs, .format_any_value, character(1))),
    stringsAsFactors = FALSE
  )
}

# One row per contact of a party: Name, Role, Department, Contact (email, phone,
# address and every other field the YAML carries, one per line), and
# Responsibilities (Signatory / Reviewer / Backup, one per line). Contacts that
# are only backups (backup without signature) are listed last; the order is
# otherwise the order of the YAML. Zero rows when there are no contacts.
.party_contacts_df <- function(contacts) {
  columns <- c("Name", "Role", "Department", "Contact", "Responsibilities")
  if (is.null(contacts) || length(contacts) == 0) {
    return(.empty_text_df(columns))
  }
  contacts <- lapply(contacts, function(ct) if (is.list(ct)) ct else list(name = as.character(ct)))
  is_backup <- vapply(contacts, function(ct) isTRUE(ct$backup) && !isTRUE(ct$signature), logical(1))
  contacts <- c(contacts[!is_backup], contacts[is_backup])

  text <- function(x) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) "" else paste(as.character(x), collapse = ", ")
  }
  known <- c("name", "role", "department", "email", "phone", "address", "reviewer", "backup", "signature")
  rows <- lapply(contacts, function(ct) {
    contact <- c(text(ct$email), text(ct$phone), text(ct$address))
    for (f in setdiff(names(ct), known)) {
      contact <- c(contact, paste0(.title_case_field(f), ": ", .format_any_value(ct[[f]])))
    }
    flags <- c(
      if (isTRUE(ct$signature)) "Signatory",
      if (isTRUE(ct$reviewer)) "Reviewer",
      if (isTRUE(ct$backup)) "Backup"
    )
    data.frame(
      Name = if (nzchar(text(ct$name))) text(ct$name) else "(Unnamed)",
      Role = text(ct$role),
      Department = text(ct$department),
      Contact = paste(contact[nzchar(contact)], collapse = "\n"),
      Responsibilities = paste(flags, collapse = "\n"),
      stringsAsFactors = FALSE
    )
  })
  .rbind_rows(rows, .empty_text_df(columns))
}

# One row per expected file: name or pattern, match type, format, expected count
# ("n", or "min to max" when they differ) and description (pattern description
# plus the file's `info`). `files` is a list of DTAFile objects; zero rows when
# it is empty.
.file_specs_df <- function(files) {
  columns <- c("File Name / Pattern", "Match Type", "Format", "Expected Count", "Description")
  if (is.null(files) || length(files) == 0) {
    return(.empty_text_df(columns))
  }
  rows <- lapply(files, function(f) {
    min_n <- f@min_number_of_files %||% 0
    max_n <- f@max_number_of_files %||% 0
    match_type <- if (isTRUE(f@pattern)) "Regex pattern" else "Exact name"

    desc <- if (!is.null(f@pattern_description)) f@pattern_description else ""
    info_txt <- tryCatch(f@info, error = function(e) NULL)
    if (!is.null(info_txt) && length(info_txt) > 0) {
      info_flat <- if (is.list(info_txt)) .format_any_value(info_txt) else paste(info_txt, collapse = "; ")
      if (nzchar(info_flat)) {
        desc <- if (nzchar(desc)) paste0(desc, " (", info_flat, ")") else info_flat
      }
    }

    data.frame(
      `File Name / Pattern` = if (!is.null(f@filename)) paste(f@filename, collapse = ", ") else MISSING_VALUE_DISPLAY,
      `Match Type` = match_type,
      `Format` = .file_format_label(f),
      `Expected Count` = paste0(
        min_n,
        if (min_n != max_n) paste0(" to ", max_n) else ""
      ),
      `Description` = desc,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  .rbind_rows(rows, .empty_text_df(columns))
}

# How each tabular file is to be parsed: one row per DTAFileTabular with
# delimiter, header row, quote, missing-value codes, encoding and embedded line
# breaks. Files that are not tabular are skipped; zero rows when none is.
.file_format_df <- function(files) {
  columns <- c(
    "File", "Delimiter", "Header row", "Quote", "Missing values", "Encoding",
    "Line breaks in values"
  )
  tabular <- Filter(function(f) inherits(f, "DTAtools::DTAFileTabular"), files)
  if (length(tabular) == 0) {
    return(.empty_text_df(columns))
  }
  named_char <- function(x, empty = "(none)") {
    if (is.null(x) || length(x) == 0) {
      return(empty)
    }
    labels <- stats::setNames(
      c(
        "Comma ( , )", "Tab", "Semicolon ( ; )", "Pipe ( | )",
        "Double quote ( \" )", "Single quote ( ' )", "Space", "(empty string)"
      ),
      c(",", "\t", ";", "|", "\"", "'", " ", "")
    )
    hit <- match(x, names(labels))
    shown <- ifelse(is.na(hit), x, unname(labels)[hit])
    shown[is.na(x)] <- MISSING_VALUE_DISPLAY
    paste(shown, collapse = ", ")
  }
  yes_no <- function(x) if (isTRUE(x)) "Yes" else "No"
  rows <- lapply(tabular, function(f) {
    data.frame(
      File = paste(f@filename, collapse = ", "),
      Delimiter = named_char(f@sep),
      `Header row` = yes_no(f@has_header),
      Quote = named_char(f@quote),
      `Missing values` = named_char(f@missing_values),
      Encoding = if (length(f@encoding) > 0 && nzchar(f@encoding)) f@encoding else MISSING_VALUE_DISPLAY,
      `Line breaks in values` = yes_no(f@newlines_in_values),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  .rbind_rows(rows, .empty_text_df(columns))
}

# One row per column specification: Variable Name, Label, Type (backend, type,
# length, then "Format:" and "R class:" lines; MISSING_VALUE_DISPLAY when the
# type is not given), Nullable (Yes / No, or "No (default)" when it is not given:
# the validation engine treats an unset `nullable` as not nullable), Allowed Values / Pattern (the complete list of values -- never
# shortened -- and the pattern) and Description (plus "Examples:"). `specs_collection`
# is a DTAColumnSpecCollection; zero rows when it has no columns.
.column_specs_df <- function(specs_collection) {
  columns <- c(
    "Variable Name", "Label", "Type", "Nullable", "Allowed Values / Pattern",
    "Description"
  )
  if (is.null(specs_collection) || length(specs_collection@columns) == 0) {
    return(.empty_text_df(columns))
  }

  rows <- lapply(specs_collection@columns, function(spec) {
    # Get type
    type_str <- MISSING_VALUE_DISPLAY
    if (!is.null(spec@structure) && !is.null(spec@structure@type)) {
      type_str <- spec@structure@type
      if (!is.null(spec@structure@backend) && nzchar(spec@structure@backend)) {
        type_str <- paste0(spec@structure@backend, " ", type_str)
      }
    }
    # Length and format are part of the contract a supplier must meet (a SAS
    # Char of length 10 is not a SAS Char of length 200), so they are shown.
    if (!is.null(spec@structure)) {
      len <- spec@structure@length
      if (!is.null(len) && length(len) == 1 && !is.na(len)) type_str <- paste0(type_str, " (", len, ")")
      fmt <- spec@structure@format
      if (!is.null(fmt) && length(fmt) == 1 && !is.na(fmt) && nzchar(fmt)) {
        type_str <- paste0(type_str, "\nFormat: ", fmt)
      }
    }
    if (!is.null(spec@colclass) && nzchar(spec@colclass)) {
      type_str <- paste0(type_str, "\nR class: ", spec@colclass)
    }

    # An unset `nullable` is checked as FALSE (columnSpecChecks.R), so it reads
    # "No (default)", not as if it were left open.
    nullable_str <- if (!is.null(spec@nullable)) {
      ifelse(spec@nullable, "Yes", "No")
    } else {
      "No (default)"
    }

    # Allowed values and pattern, both when both are given, never truncated:
    # a specification that hides its eleventh permitted value is wrong.
    all_values <- function(v) {
      if (is.list(v)) .format_any_value(v) else paste(as.character(v), collapse = ", ")
    }
    values_str <- c(
      if (!is.null(spec@values) && length(spec@values) > 0) all_values(spec@values),
      if (!is.null(spec@pattern) && nzchar(spec@pattern)) paste0("Pattern: ", spec@pattern)
    )
    values_str <- paste(values_str, collapse = "\n")

    # Get description
    desc_str <- if (!is.null(spec@description)) spec@description else ""
    if (!is.null(spec@examples) && length(spec@examples) > 0) {
      desc_str <- paste0(desc_str, if (nzchar(desc_str)) "\n", "Examples: ", all_values(spec@examples))
    }

    data.frame(
      `Variable Name` = spec@id,
      `Label` = if (!is.null(spec@label)) spec@label else "",
      `Type` = type_str,
      `Nullable` = nullable_str,
      `Allowed Values / Pattern` = values_str,
      `Description` = desc_str,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  .rbind_rows(rows, .empty_text_df(columns))
}

# The version history as Version / Date / Changes rows. `changes` is flattened
# to one string (entries joined with "; ") because the class permits a
# character vector there and data.frame() aborts on a vector of length > 1;
# the date goes through .format_document_date() so it reads the same whatever
# the machine's locale, and whether it arrives as a Date or as text.
# `version_history` is a list of list(version =, date =, changes =) records
# (DTAMetaData@version_history); zero rows when it is NULL or empty.
.version_history_df <- function(version_history) {
  columns <- c("Version", "Date", "Changes")
  if (is.null(version_history) || length(version_history) == 0) {
    return(.empty_text_df(columns))
  }
  rows <- lapply(version_history, function(vh) {
    data.frame(
      Version = if (is.null(vh$version)) "" else as.character(vh$version)[[1]],
      Date = .format_document_date(vh$date),
      Changes = paste(unlist(vh$changes), collapse = "; "),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  .rbind_rows(rows, .empty_text_df(columns))
}

# The paragraph that introduces a dataset's file table: how many files are
# expected in total (the sum over all file entries; "n to m files are" when the
# minimum and maximum differ, "1 file is" for exactly one) and how to read the
# file names. `files` is a list of DTAFile objects; "" when it is empty.
.file_count_sentence <- function(files) {
  if (is.null(files) || length(files) == 0) {
    return("")
  }
  total_min <- sum(sapply(files, function(f) {
    tryCatch(f@min_number_of_files %||% 0, error = function(e) 0)
  }))
  total_max <- sum(sapply(files, function(f) {
    tryCatch(f@max_number_of_files %||% 0, error = function(e) 0)
  }))

  count_txt <- if (total_min != total_max) {
    paste0(total_min, " to ", total_max, " files are")
  } else if (total_min == 1) {
    "1 file is"
  } else {
    paste(total_min, "files are")
  }
  paste0(
    count_txt, " expected for this dataset in total. Each file name is given ",
    "either as an exact name or as a regular-expression pattern that a ",
    "delivered file must match; the expected count states how many files may match it."
  )
}
