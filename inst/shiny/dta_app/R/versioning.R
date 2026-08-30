# -----------------------------------------------------------------------------
# Versioning: pure, server-free helpers for the "create a new document
# version" flow.
#
# Every function here is a plain value-in, value-out transformation -- none of
# them touch a Shiny session or reactive value -- so they can be unit tested
# by calling them directly. The server-side code that wires these into the
# app's version-bump UI lives elsewhere; this file only supplies:
#
#   dta_next_version(v)                     -- suggest the next MINOR version
#   dta_version_is_ignored_key(keys)        -- is a diff key the machinery's
#                                               own write, not authored content?
#   dta_version_sanitise(x)                 -- make a value safe for a
#                                               Markdown table cell
#   dta_version_change_line(key, ...)       -- render one diff row for humans
#   dta_version_change_summary(diff, ...)   -- render a whole dta_diff() as
#                                               one `changes` string
#   dta_version_placeholder()               -- the text a fresh entry carries
#                                               until an export fills it in
#   dta_append_version_entry(dta, ...)      -- add a new version_history entry
#   dta_set_version_entry_changes(dta, ...) -- rewrite one entry's `changes`
#                                               (and optionally its `version`)
# -----------------------------------------------------------------------------

# ---- Suggesting the next version -------------------------------------------

# Suggest the next MINOR version for the "new version" field, or "" when the
# current version does not follow a plain dotted-integer scheme.
#
# Deliberately conservative: a scheme this cannot parse ("Draft A", "v1.0",
# "1.0-rc1") gets "" rather than a guess, so the author is left to type a
# version deliberately instead of silently accepting a wrong one.
dta_next_version <- function(v) {
  v <- as.character(v)[1]
  if (is.null(v) || length(v) == 0 || is.na(v) || trimws(v) == "") {
    return("")
  }

  parts <- strsplit(trimws(v), ".", fixed = TRUE)[[1]]
  if (length(parts) == 0 || !all(grepl("^[0-9]+$", parts))) {
    return("")
  }

  n <- length(parts)
  # A lone numeric component gains a minor part ("1" -> "1.1") rather than a
  # major bump ("1" -> "2") -- appending ".1" is what "next minor version"
  # means for a version that has no minor component yet.
  if (n == 1) {
    return(paste0(parts[1], ".1"))
  }

  parts[n] <- as.character(as.integer(parts[n]) + 1L)
  paste(parts, collapse = ".")
}

# ---- Filtering the machinery's own writes out of a summary -----------------

# TRUE for keys the versioning machinery itself writes: the exact key
# "version", and anything under "version_history". These must never appear
# in a change summary, because a summary that led with "changed version 1.0
# -> 1.1" would be restating the header of the very entry it is written
# into.
#
# Deliberately NOT folded into dta_metadata_machine_fields() (template_core.R)
# -- that set is what dta_template_metadata_fields() subtracts from to decide
# which top-level fields a creation template may set, and a creation template
# legitimately sets `version`. Widening machine_fields() to include "version"
# would silently make `version` un-settable from a template, which is not
# what this filter is for.
dta_version_is_ignored_key <- function(keys) {
  keys == "version" | grepl("^version_history(\\.|$)", keys)
}

# ---- Sanitising values for Markdown --------------------------------------

# Make a value safe to put in a Markdown table cell.
#
# R/exportDocuments.R (around line 333) writes a version_history entry's
# `changes` straight into a `|`-delimited Markdown table row via
# .df_to_md_table(); an embedded newline or `|` would break the table
# structure, so both are replaced before the value goes anywhere near an
# export.
dta_version_sanitise <- function(x) {
  x <- as.character(x)[1]
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("")
  }
  x <- gsub("[\r\n|]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# ---- Rendering one diff row -------------------------------------------------

# One diff row rendered for a human, e.g.
# "changed metadata.title ('Old' -> 'New')". A plain ASCII "->" is used
# rather than a Unicode arrow because this string round-trips through YAML
# and into DOCX/Markdown exports.
dta_version_change_line <- function(key, change, from, to) {
  key <- dta_version_sanitise(key)
  from <- dta_version_sanitise(from)
  to <- dta_version_sanitise(to)

  if (change == "changed") {
    sprintf("changed %s ('%s' -> '%s')", key, from, to)
  } else if (change == "added") {
    sprintf("added %s ('%s')", key, to)
  } else if (change == "removed") {
    sprintf("removed %s ('%s')", key, from)
  } else {
    # Defensive: dta_diff_two_way() only ever emits changed/added/removed,
    # but a future change value must never be silently dropped from the
    # summary.
    paste(change, key)
  }
}

# ---- Rendering a whole diff as one `changes` string -------------------------

# Count "<n> <label>" fragments for one diff section, in changed/added/
# removed order, omitting any change type with zero rows.
.dta_version_count_fragment <- function(counts, label) {
  if (counts[[label]] == 0) {
    return(NULL)
  }
  paste(counts[[label]], label)
}

# Build the "<Section>: 2 changed, 1 added." counts line for one diff
# section, or "" when the section has no rows.
.dta_version_counts_line <- function(section_label, change_col) {
  if (length(change_col) == 0) {
    return("")
  }
  counts <- list(
    changed = sum(change_col == "changed"),
    added = sum(change_col == "added"),
    removed = sum(change_col == "removed")
  )
  fragments <- Filter(
    Negate(is.null),
    list(
      .dta_version_count_fragment(counts, "changed"),
      .dta_version_count_fragment(counts, "added"),
      .dta_version_count_fragment(counts, "removed")
    )
  )
  if (length(fragments) == 0) {
    return("")
  }
  paste0(section_label, ": ", paste(fragments, collapse = ", "), ".")
}

# Render a dta_diff() result (list(metadata = <frame>, datasets = <frame>))
# as one single-line, non-empty `changes` string for a version_history entry.
#
# Non-empty is not a style choice: the DTAMetaData validator rejects an
# empty `changes` string outright, so the "No changes recorded." fallback
# below is load-bearing, not decorative.
dta_version_change_summary <- function(diff, note = "", max_items = 50) {
  metadata <- diff$metadata
  datasets <- diff$datasets

  if (is.null(metadata) || nrow(metadata) == 0) {
    metadata <- data.frame(
      key = character(0), change = character(0),
      from = character(0), to = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    metadata <- metadata[!dta_version_is_ignored_key(metadata$key), , drop = FALSE]
  }

  if (is.null(datasets) || nrow(datasets) == 0) {
    datasets <- data.frame(
      key = character(0), change = character(0),
      from = character(0), to = character(0),
      stringsAsFactors = FALSE
    )
  }

  # A bare metadata key like "title" would be ambiguous next to a dataset
  # key -- prefixing metadata keys with "metadata." (a dataset key already
  # starts with the dataset name, e.g. "clinical_data.columns.AGE.type") is
  # what keeps every detail line self-describing on its own.
  meta_keys <- if (nrow(metadata) > 0) paste0("metadata.", metadata$key) else character(0)
  ds_keys <- if (nrow(datasets) > 0) datasets$key else character(0)

  counts_parts <- Filter(
    function(x) nzchar(x),
    c(
      .dta_version_counts_line("Metadata", metadata$change),
      .dta_version_counts_line("Datasets", datasets$change)
    )
  )
  counts_line <- paste(counts_parts, collapse = " ")

  # Detail rows: metadata rows first, then dataset rows, each in the order
  # the frame already has -- dta_diff_two_way() has already sorted with
  # method = "radix", and re-sorting here (or using sort() at all) would
  # make the output depend on this machine's collation, which diverges from
  # CI's.
  n_meta <- nrow(metadata)
  n_ds <- nrow(datasets)
  total_rows <- n_meta + n_ds

  if (total_rows == 0) {
    body <- "No changes recorded."
  } else {
    all_keys <- c(meta_keys, ds_keys)
    all_change <- c(metadata$change, datasets$change)
    all_from <- c(metadata$from, datasets$from)
    all_to <- c(metadata$to, datasets$to)

    n_shown <- min(total_rows, max_items)
    idx <- seq_len(n_shown)
    lines <- vapply(
      idx,
      function(i) {
        dta_version_change_line(all_keys[i], all_change[i], all_from[i], all_to[i])
      },
      character(1)
    )

    detail <- paste0("Details: ", paste(lines, collapse = "; "))

    omitted <- total_rows - n_shown
    if (omitted > 0) {
      unit <- if (omitted == 1) "change" else "changes"
      detail <- paste0(detail, "; ... and ", omitted, " further ", unit, " not listed.")
    }

    body <- if (nzchar(counts_line)) paste(counts_line, detail) else detail
  }

  note_raw <- as.character(note)[1]
  has_note <- !is.null(note_raw) && length(note_raw) > 0 && !is.na(note_raw) && trimws(note_raw) != ""
  if (has_note) {
    paste0(dta_version_sanitise(note_raw), " - ", body)
  } else {
    body
  }
}

# ---- Placeholder text --------------------------------------------------------

# The text a freshly opened version_history entry carries until an export
# fills it in with the real summary.
#
# Must be non-empty: the DTAMetaData validator rejects an empty `changes`
# string, so a truly blank placeholder would make the document invalid the
# moment the entry is added, before the author has changed anything. The
# wording is deliberate too -- a reader looking at the in-app Raw YAML tab
# before the next export should see an explanation, not what looks like a
# missing or corrupted field.
dta_version_placeholder <- function() {
  "(changes are summarised when the document is exported)"
}

# ---- Mutating a DTA's version history ---------------------------------------

# Append a new version_history entry and set the document's own `version` to
# match. Mirrors the entry shape rebase_apply() writes in
# template_diff.R (list(version=, date=, changes=)) so every write path
# produces structurally identical entries.
dta_append_version_entry <- function(dta, version, date = Sys.Date(),
                                     changes = dta_version_placeholder()) {
  dta_try({
    md <- DTAtools::metadata(dta)

    version <- as.character(version)[1]
    if (is.null(version) || length(version) == 0 || is.na(version) || trimws(version) == "") {
      stop("A version is required.")
    }

    changes <- as.character(changes)[1]
    if (is.null(changes) || length(changes) == 0 || is.na(changes) || trimws(changes) == "") {
      changes <- dta_version_placeholder()
    }

    entry <- list(version = version, date = date, changes = changes)
    S7::prop(md, "version_history") <- c(S7::prop(md, "version_history"), list(entry))
    S7::prop(md, "version") <- version

    dta@metadata <- md
    dta
  })
}

# Rewrite one existing version_history entry's `changes` in place, and
# optionally re-sync its `version`.
#
# The re-sync exists because the author can edit the Version field in the
# Metadata tab after the entry was already opened -- without this, the
# entry's own `version` would silently drift from the document's actual
# `version`, and the history would misreport which version the recorded
# changes belong to.
dta_set_version_entry_changes <- function(dta, index, changes, version = NULL) {
  dta_try({
    md <- DTAtools::metadata(dta)
    vh <- S7::prop(md, "version_history")

    out_of_range <- is.null(index) || length(index) != 1 || is.na(index) ||
      index < 1 || index > length(vh)

    if (out_of_range) {
      dta
    } else {
      changes <- dta_version_sanitise(changes)
      if (!nzchar(changes)) {
        changes <- dta_version_placeholder()
      }
      vh[[index]]$changes <- changes

      version <- if (is.null(version)) "" else dta_version_sanitise(version)
      if (nzchar(version)) {
        vh[[index]]$version <- version
      }

      S7::prop(md, "version_history") <- vh
      dta@metadata <- md
      dta
    }
  })
}
