# -----------------------------------------------------------------------------
# The diff engine, and the three-way rebase built on top of it.
#
# "Rebase" here means: a DTA was created from template T@1.0, T has since
# gained a 1.1, and the document's author wants to move onto 1.1 WITHOUT
# discarding whatever they themselves edited by hand since creation. That
# needs three documents, not two:
#
#   ancestor   what T@1.0 would produce with the RECORDED selections -- the
#              document's actual starting point, reconstructed from
#              provenance rather than kept around as a copy.
#   current    the document as it stands today (ancestor + every hand edit).
#   target     what T@1.1 would produce with the SAME selections.
#
# A key that differs between ancestor and current is a USER edit. A key that
# differs between ancestor and target is a TEMPLATE edit. Comparing all three
# is what tells "the template moved this field" apart from "the user moved
# this field" -- a plain two-way diff (current vs. target) cannot, because it
# has no way to know which of the two documents is the one that changed.
#
# This file provides:
#   dta_metadata_leaves() / dta_dataset_leaves()   flatten a DTA's metadata/
#                                                   datasets to dotted-path
#                                                   leaves, the unit both the
#                                                   diff and the rebase work
#                                                   in.
#   dta_diff()                                     a plain two-way diff over
#                                                   those leaves (added/
#                                                   removed/changed).
#   materialise_template()                         rebuilds the DTA a
#                                                   template ref WOULD
#                                                   produce -- how ancestor
#                                                   and target are obtained.
#   dta_template_provenance()                      reads back the
#                                                   metadata.template record
#                                                   template_provenance()
#                                                   (template_create.R)
#                                                   wrote.
#   rebase_plan() / rebase_apply()                 the three-way classifier
#                                                   and the (metadata-only)
#                                                   writer that acts on it.
#
# `%||%`, `dta_try()`, `list_get_path()`/`list_set_path()`,
# `apply_template_metadata_path()`, `template_provenance()`,
# `load_template_definition()`, `create_dta_from_template()`,
# `dta_metadata_machine_fields()`, `dta_dataset_names()`, `dta_get_dataset()`,
# `dta_dataset_to_list()`, `dta_template_index_cached()` and
# `load_template_definition()`'s own resolver are all bound elsewhere in the
# app helper environment (utils_dta.R, template_core.R, template_create.R,
# template_index.R) -- none of it is redefined here, same convention every
# other template file in this directory follows.
#
# Dataset-level rebase is explicitly OUT OF SCOPE for rebase_apply(): only
# metadata is three-way classified and written. dta_diff() still reports
# dataset-level differences in full (so a user reviewing a rebase can SEE
# that a dataset changed), and rebase_plan() carries that comparison on the
# plan as `dataset_changes`, flagged `datasets_reportable_only = TRUE` so a
# caller cannot mistake "reported" for "applied".
# -----------------------------------------------------------------------------

# ---- Flattening: DTA structure -> dotted-path leaves ------------------------

# Recursively flatten a (possibly nested) list to dotted-path leaves.
#
# A LIST is walked one level at a time: a NAMED list contributes its own
# names as the next path segment; an list with no names (or with any blank
# name) is treated as an ARRAY and contributes 1-based positions instead --
# this is what turns `version_history[[1]]$version` into the dotted key
# "version_history.1.version" the task specifies verbatim.
#
# Anything that is NOT a list (character, numeric, logical, Date, or an
# atomic vector of length > 1) is a LEAF, stored as-is -- not stringified
# here. Keeping the raw value is what lets rebase_plan()'s three-way compare
# use identical() directly (a Date, or a multi-value character vector,
# compares correctly that way); stringifying is display-only and happens at
# the point something is written into a data frame (see
# dta_diff_display_value()), never before.
#
# A NULL leaf (an unset scalar property, e.g. metadata$header when nothing
# set it) is dropped rather than stored: "key absent" and "key present with
# value NULL" would otherwise be indistinguishable from a caller's
# perspective, and "absent" is the only one of the two that ever actually
# occurs on a real DTAMetaData/DTADataSet (as.list()/dta_dataset_to_list()
# both already omit anything they consider empty -- this guard is what keeps
# that contract intact through the recursion, for any value that reaches
# here as an explicit NULL some other way).
.dta_leaf_flatten <- function(x, prefix = NULL) {
  out <- list()
  if (is.null(x)) {
    return(out)
  }
  if (!is.list(x)) {
    if (is.null(prefix) || !nzchar(prefix)) {
      # A bare non-list value with no path to hang it on cannot occur from a
      # real call (every caller here starts from a named top-level list or a
      # column dict), but returning nothing rather than guessing a key keeps
      # this function total instead of assuming its own callers' shapes.
      return(out)
    }
    out[[prefix]] <- x
    return(out)
  }
  if (length(x) == 0) {
    return(out)
  }

  nms <- names(x)
  named <- !is.null(nms) && all(nzchar(nms))
  for (i in seq_along(x)) {
    seg <- if (named) nms[[i]] else as.character(i)
    key <- if (is.null(prefix) || !nzchar(prefix)) seg else paste(prefix, seg, sep = ".")
    out <- c(out, .dta_leaf_flatten(x[[i]], key))
  }
  out
}

# Flatten a DTAMetaData to a named, dotted-path-keyed list of leaf values.
#
# Built from as.list(md) (R/DTAMetaData-helpers.R) -- the ONE canonical
# metadata -> list mapping -- rather than walking @properties by hand, so
# this automatically follows whatever that method decides to include/rename/
# reshape, instead of drifting from it over time.
#
# dta_metadata_machine_fields() (import_issues, template) are stripped at the
# TOP level before any recursion: provenance is not user content, and must
# never surface as a leaf a rebase could classify as changed/conflicting --
# a document's OWN @template is rewritten by rebase_apply() itself (see
# below), never diffed as if it were a fact about the document's subject
# matter.
dta_metadata_leaves <- function(md) {
  lst <- as.list(md)
  lst <- lst[!(names(lst) %in% dta_metadata_machine_fields())]
  .dta_leaf_flatten(lst)
}

# Flatten every dataset of a DTA to dotted-path leaves.
#
# Two shapes, per the task's own contract:
#   "<dataset>.<field>"                      every non-column field of
#                                             dta_dataset_to_list()'s output
#                                             (name/type/description/
#                                             template_source/
#                                             template_version/
#                                             template_date/files/rules),
#                                             flattened generically.
#   "<dataset>.columns.<COLUMN_ID>.<field>"  each column, keyed by its OWN
#                                             `id`, never by its position in
#                                             the list.
#
# Keying columns by id (not position) is the whole point: dta_column_to_list()
# always includes `id`, so build the key from THAT rather than from the
# column's index. A positional key would report every column after an
# inserted one as "changed" (it moved from slot i to i+1), burying whatever
# the real edit was under a wall of false positives -- see the column-
# insertion-stability test in tests/testthat/test-shinyapp-template-
# rebase.R.
#
# A column with no `id` (should not occur -- DTAColumnSpec requires one) is
# skipped rather than falling back to a positional key: a fallback here would
# silently reintroduce the exact bug this function exists to avoid, for
# whichever column happens to be missing one.
dta_dataset_leaves <- function(dta) {
  out <- list()
  for (nm in dta_dataset_names(dta)) {
    ds <- dta_get_dataset(dta, nm)
    lst <- dta_dataset_to_list(ds)

    cols <- lst$columns
    lst$columns <- NULL
    out <- c(out, .dta_leaf_flatten(lst, nm))

    for (col in cols) {
      cid <- as.character(col$id %||% "")
      if (!nzchar(cid)) {
        next
      }
      col$id <- NULL
      out <- c(out, .dta_leaf_flatten(col, paste(nm, "columns", cid, sep = ".")))
    }
  }
  out
}

# ---- Rendering a leaf value for display -------------------------------------

# Collapse one leaf value into a single, stable display string for a diff
# data frame cell -- NEVER via print()/dput(), which are for a console, not a
# value a user or a test is meant to read back.
#
# A leaf is normally already a scalar (see .dta_leaf_flatten()'s own
# contract), but a column's `values`/`examples` fields are exactly the kind
# of atomic vector of length > 1 that reaches here in practice (e.g. a
# column's declared value set); a bare Date needs ISO formatting rather than
# its internal numeric-days representation; and a list is handled
# defensively even though the flattener should never hand one to this
# function, so a future change to what counts as a "leaf" fails loudly with a
# readable string instead of an opaque one.
dta_diff_display_value <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }
  if (is.list(x)) {
    txt <- yaml::as.yaml(x, line.sep = "\n")
    return(paste(trimws(strsplit(txt, "\n", fixed = TRUE)[[1]]), collapse = "; "))
  }
  if (length(x) > 1) {
    return(paste(as.character(x), collapse = ", "))
  }
  as.character(x)
}

# ---- Two-way diff over a pair of leaf maps -----------------------------------

# The empty frame dta_diff_two_way() promises, shared by every early-return so
# a zero-difference comparison is never NULL.
.dta_diff_empty_change_frame <- function() {
  data.frame(
    key = character(0), change = character(0),
    from = character(0), to = character(0),
    stringsAsFactors = FALSE
  )
}

# Plain two-way diff of two leaf maps (as produced by dta_metadata_leaves()/
# dta_dataset_leaves()): every key in either map is classified "added"
# (right only), "removed" (left only) or "changed" (both, different values);
# a key present and identical(...) on both sides is OMITTED -- the "diff"
# only ever lists what actually differs.
#
# Sorted with method = "radix": this machine collates under German, CI does
# not, and a diff whose row order depends on which machine produced it would
# be a needless, previously-bitten difference (see the locale-collation
# lesson elsewhere in this codebase's history).
dta_diff_two_way <- function(a_leaves, b_leaves) {
  keys <- sort(union(names(a_leaves), names(b_leaves)), method = "radix")

  rows <- list()
  for (k in keys) {
    has_a <- k %in% names(a_leaves)
    has_b <- k %in% names(b_leaves)

    if (has_a && has_b) {
      if (identical(a_leaves[[k]], b_leaves[[k]])) {
        next
      }
      rows[[length(rows) + 1L]] <- data.frame(
        key = k, change = "changed",
        from = dta_diff_display_value(a_leaves[[k]]),
        to = dta_diff_display_value(b_leaves[[k]]),
        stringsAsFactors = FALSE
      )
    } else if (has_a) {
      rows[[length(rows) + 1L]] <- data.frame(
        key = k, change = "removed",
        from = dta_diff_display_value(a_leaves[[k]]), to = NA_character_,
        stringsAsFactors = FALSE
      )
    } else {
      rows[[length(rows) + 1L]] <- data.frame(
        key = k, change = "added",
        from = NA_character_, to = dta_diff_display_value(b_leaves[[k]]),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(.dta_diff_empty_change_frame())
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# The full diff between two DTAs: metadata leaves and dataset leaves,
# compared independently (a document can differ in either, or both).
dta_diff <- function(a, b) {
  list(
    metadata = dta_diff_two_way(
      dta_metadata_leaves(DTAtools::metadata(a)),
      dta_metadata_leaves(DTAtools::metadata(b))
    ),
    datasets = dta_diff_two_way(
      dta_dataset_leaves(a),
      dta_dataset_leaves(b)
    )
  )
}

# ---- Rebuilding a template's own output --------------------------------------

# Rebuild the DTA that `ref` ("id", "id@version" or "id@latest") WOULD
# produce, given `selections` and `index` -- i.e. load_template_definition()
# followed by create_dta_from_template(), exactly the pair every "Create new
# from template" flow already calls, just without a Shiny session around it.
#
# This is how rebase_plan() reconstructs the ancestor (from the version and
# selections recorded in the document's own provenance) and the target (from
# the version being rebased TO, with the SAME selections) -- both are, by
# definition, "what this template ref would build", nothing more specific
# than that.
#
# `carry_over` is accepted and threaded straight through to
# create_dta_from_template() for symmetry with that function's own contract
# and for any FUTURE caller that wants a materialised template merged with an
# ancestor's carried-over fields; rebase_plan() itself always calls this with
# no carry-over, because ancestor/target reconstruction must reproduce
# exactly what the template alone would build, not a document blended with
# something else.
materialise_template <- function(ref, index, selections, carry_over = NULL) {
  dta_try({
    loaded <- load_template_definition(ref, index = index)
    if (!isTRUE(loaded$ok)) {
      stop(loaded$error)
    }

    built <- create_dta_from_template(
      loaded$value$def, loaded$value$path,
      selections = selections %||% list(),
      index = index,
      carry_over = carry_over
    )
    if (!isTRUE(built$ok)) {
      stop(built$error)
    }

    built$value
  })
}

# The metadata.template provenance record template_provenance()
# (template_create.R) wrote onto `dta`, or NULL when it never was: a document
# created before this feature, or created without going through a template at
# all, carries an empty @template (its declared default) and has no ancestor
# to rebase from.
#
# Every field access below guards for a partial record (a hand-edited
# document, or one written by a future/older version of this feature) rather
# than assuming the shape template_provenance() currently produces --
# rebase_plan() is what turns "partial" into a named, specific error; this
# function's job is only to hand back whatever is actually there.
dta_template_provenance <- function(dta) {
  md <- tryCatch(DTAtools::metadata(dta), error = function(e) NULL)
  if (is.null(md)) {
    return(NULL)
  }
  prov <- tryCatch(S7::prop(md, "template"), error = function(e) NULL)
  if (is.null(prov) || length(prov) == 0) {
    return(NULL)
  }
  prov
}

# ---- Three-way (and degraded two-way) classification ------------------------

# The empty frames rebase_plan() promises for `changes`/`conflicts`, shared by
# every branch so "nothing to report" is a zero-row frame with the right
# columns, never NULL.
.rebase_empty_changes_frame <- function() {
  data.frame(
    key = character(0), resolution = character(0),
    ancestor = character(0), current = character(0), target = character(0),
    stringsAsFactors = FALSE
  )
}

.rebase_empty_conflicts_frame <- function() {
  data.frame(
    key = character(0),
    ancestor = character(0), current = character(0), target = character(0),
    stringsAsFactors = FALSE
  )
}

.rebase_leaf_present <- function(leaves, key) {
  key %in% names(leaves)
}

# Are two (presence, value) pairs the SAME state? Two absences are equal (a
# field neither side has is not a difference); one present and one absent
# never are, regardless of what the present one holds; two present values are
# compared with identical(), on the RAW leaf value, never the display string
# -- a Date or a multi-value vector must compare by content, not by whatever
# dta_diff_display_value() happens to render it as.
.rebase_leaf_equal <- function(a_present, a_value, b_present, b_value) {
  if (!a_present && !b_present) {
    return(TRUE)
  }
  if (a_present != b_present) {
    return(FALSE)
  }
  identical(a_value, b_value)
}

# Three-way classification of every leaf key across ancestor/current/target,
# per the task's own rule table:
#
#   changed in target only (current == ancestor)  -> "take_template"
#   changed in current only (target == ancestor)  -> "keep_user"
#   changed in both, to the SAME value            -> "agree" (not a conflict)
#   changed in both, to DIFFERENT values          -> conflict
#   changed in neither                            -> omitted entirely
#
# "Changed" and "value" both include PRESENCE as part of the state (see
# .rebase_leaf_equal()): a key the template REMOVES (present in ancestor,
# absent from target) is "target changed" exactly like a key whose value it
# edited, so removing a field classifies take_template/keep_user/agree/
# conflict identically to editing one -- there is no separate "removal" case
# to special-case here.
.rebase_classify_three_way <- function(ancestor_leaves, current_leaves, target_leaves) {
  keys <- sort(
    union(union(names(ancestor_leaves), names(current_leaves)), names(target_leaves)),
    method = "radix"
  )

  change_rows <- list()
  conflict_rows <- list()

  for (k in keys) {
    a_present <- .rebase_leaf_present(ancestor_leaves, k)
    c_present <- .rebase_leaf_present(current_leaves, k)
    t_present <- .rebase_leaf_present(target_leaves, k)
    a_val <- ancestor_leaves[[k]]
    c_val <- current_leaves[[k]]
    t_val <- target_leaves[[k]]

    cur_changed <- !.rebase_leaf_equal(a_present, a_val, c_present, c_val)
    tgt_changed <- !.rebase_leaf_equal(a_present, a_val, t_present, t_val)

    if (!cur_changed && !tgt_changed) {
      next # unchanged relative to the ancestor on both sides -- omitted.
    }

    row <- data.frame(
      key = k,
      ancestor = dta_diff_display_value(a_val),
      current = dta_diff_display_value(c_val),
      target = dta_diff_display_value(t_val),
      stringsAsFactors = FALSE
    )

    if (tgt_changed && !cur_changed) {
      row$resolution <- "take_template"
      change_rows[[length(change_rows) + 1L]] <- row[c("key", "resolution", "ancestor", "current", "target")]
    } else if (cur_changed && !tgt_changed) {
      row$resolution <- "keep_user"
      change_rows[[length(change_rows) + 1L]] <- row[c("key", "resolution", "ancestor", "current", "target")]
    } else if (.rebase_leaf_equal(c_present, c_val, t_present, t_val)) {
      row$resolution <- "agree"
      change_rows[[length(change_rows) + 1L]] <- row[c("key", "resolution", "ancestor", "current", "target")]
    } else {
      conflict_rows[[length(conflict_rows) + 1L]] <- row
    }
  }

  changes <- if (length(change_rows) > 0) do.call(rbind, change_rows) else .rebase_empty_changes_frame()
  conflicts <- if (length(conflict_rows) > 0) do.call(rbind, conflict_rows) else .rebase_empty_conflicts_frame()
  rownames(changes) <- NULL
  rownames(conflicts) <- NULL

  list(changes = changes, conflicts = conflicts)
}

# The degraded path for when the ancestor cannot be materialised (its version
# was pruned from the template source after this document was created): a
# straight current-vs-target comparison with NO three-way classification,
# because without the ancestor there is no way to tell "the template changed
# this" apart from "the user changed this" -- EVERY differing key becomes a
# conflict requiring an explicit human choice, never a guess. `changes` is
# therefore always empty in this mode; every actionable row lives in
# `conflicts`, with `ancestor` reported as NA (genuinely unknown, not merely
# absent) rather than guessed at from either side.
.rebase_classify_two_way <- function(current_leaves, target_leaves) {
  keys <- sort(union(names(current_leaves), names(target_leaves)), method = "radix")

  conflict_rows <- list()
  for (k in keys) {
    c_present <- .rebase_leaf_present(current_leaves, k)
    t_present <- .rebase_leaf_present(target_leaves, k)
    c_val <- current_leaves[[k]]
    t_val <- target_leaves[[k]]

    if (.rebase_leaf_equal(c_present, c_val, t_present, t_val)) {
      next
    }

    conflict_rows[[length(conflict_rows) + 1L]] <- data.frame(
      key = k,
      ancestor = NA_character_,
      current = dta_diff_display_value(c_val),
      target = dta_diff_display_value(t_val),
      stringsAsFactors = FALSE
    )
  }

  conflicts <- if (length(conflict_rows) > 0) do.call(rbind, conflict_rows) else .rebase_empty_conflicts_frame()
  rownames(conflicts) <- NULL

  list(changes = .rebase_empty_changes_frame(), conflicts = conflicts)
}

# ---- rebase_plan() -----------------------------------------------------------

# Build a rebase plan moving `current` from its recorded template version
# onto `to_version` of the SAME template.
#
# dta_try()-wrapped internally, but NOT returned as dta_try()'s own
# list(ok=, value=, error=) envelope: like read_template_header()
# (template_index.R), this reshapes the result into ONE flat list that always
# carries the same field names whether it succeeded or not (`ok` sitting
# ALONGSIDE from_version/to_version/... rather than nested under `value`), so
# a caller never has to branch on shape before reading e.g. `plan$to_version`.
#
# Steps, per the task's own contract:
#   1. Read provenance from `current`.
#   2. Materialise the ancestor at the RECORDED version/selections.
#   3. Materialise the target at `to_version`, same selections.
#   4. Three-way classify (or, if the ancestor did not materialise, degrade to
#      the two-way "everything is a conflict" comparison -- see
#      .rebase_classify_two_way()).
rebase_plan <- function(current, to_version, index = NULL) {
  res <- dta_try({
    index <- index %||% dta_template_index_cached()
    to_version_chr <- as.character(to_version)

    prov <- dta_template_provenance(current)
    if (is.null(prov)) {
      stop(paste(
        "This document has no template provenance ('metadata.template') and cannot be rebased.",
        "Documents created before this feature was added, or created without a template,",
        "have no recorded ancestor to compare against."
      ))
    }

    tpl_id <- as.character(prov$id %||% "")
    from_version <- as.character(prov$version %||% "")
    selections <- prov$selections

    missing_fields <- c(
      if (!nzchar(tpl_id)) "id",
      if (!nzchar(from_version)) "version",
      if (is.null(selections)) "selections"
    )
    if (length(missing_fields) > 0) {
      stop(sprintf(
        paste(
          "This document's template provenance is missing required field%s (%s) and cannot be",
          "rebased. A partial provenance record predates this feature, or was hand-edited."
        ),
        if (length(missing_fields) > 1) "s" else "",
        paste(missing_fields, collapse = ", ")
      ))
    }

    ancestor_ref <- paste0(tpl_id, "@", from_version)
    target_ref <- paste0(tpl_id, "@", to_version_chr)

    # The TARGET must resolve and build: without it there is nothing to
    # rebase ONTO, which is a hard failure -- unlike a missing ancestor
    # (below), there is no reduced-confidence mode for "the version being
    # rebased to does not exist".
    target_loaded <- load_template_definition(target_ref, index = index)
    if (!isTRUE(target_loaded$ok)) {
      stop(sprintf("Could not resolve target template '%s': %s", target_ref, target_loaded$error))
    }
    target_built <- create_dta_from_template(
      target_loaded$value$def, target_loaded$value$path,
      selections = selections, index = index
    )
    if (!isTRUE(target_built$ok)) {
      stop(sprintf("Could not build target template '%s': %s", target_ref, target_built$error))
    }
    target <- target_built$value

    # The provenance record rebase_apply() will stamp onto the rebased
    # document if this plan is applied: id/version/source/content_hash all
    # describe the TARGET (computed from the SAME definition target_built
    # was built from, so the two can never disagree) -- except `lineage`,
    # which is deliberately the CURRENT document's own lineage, not
    # whatever the target version's `extends:` chain looks like today. That
    # is what "preserving lineage" (the task's own wording) means: the
    # document's inheritance trail is a fact about ITS history, unaffected
    # by which template version it happens to be rebased onto.
    target_provenance <- template_provenance(
      target_loaded$value$def, target_loaded$value,
      selections = selections, lineage = prov$lineage %||% character(0)
    )

    # A missing ancestor -- e.g. 1.0 was pruned from the source after this
    # document was created -- degrades to the two-way, everything-is-a-
    # conflict comparison; see .rebase_classify_two_way() for why silently
    # guessing is never acceptable here.
    ancestor_res <- materialise_template(ancestor_ref, index, selections)
    ancestor_available <- isTRUE(ancestor_res$ok)
    ancestor <- if (ancestor_available) ancestor_res$value else NULL

    current_leaves <- dta_metadata_leaves(DTAtools::metadata(current))
    target_leaves <- dta_metadata_leaves(DTAtools::metadata(target))

    classified <- if (ancestor_available) {
      ancestor_leaves <- dta_metadata_leaves(DTAtools::metadata(ancestor))
      .rebase_classify_three_way(ancestor_leaves, current_leaves, target_leaves)
    } else {
      .rebase_classify_two_way(current_leaves, target_leaves)
    }

    # version_history is NEVER rebased. Two independent reasons, either alone
    # sufficient:
    #
    # 1. MEANING. version_history records what happened to THIS document. A
    #    rebase is a new event in that history, so it APPENDS an entry (see
    #    rebase_apply()); rewriting the existing entries would restate the past
    #    to match a template the document did not come from.
    #
    # 2. CORRECTNESS. These leaves are keyed positionally --
    #    `version_history.1.version` -- but list_set_path() indexes a list by
    #    NAME, so writing that key does not update entry one; it APPENDS a
    #    second, stub entry literally named "1" holding only that field. A
    #    one-entry history becomes a corrupt two-entry one. This is not
    #    hypothetical: every template that stamps `version: "${version}"` into
    #    base.metadata.version_history -- the packaged one does -- produces
    #    exactly this key on any version-to-version rebase.
    #
    # They are moved to `not_rebased` rather than dropped, so the UI can still
    # show that the template's own history block differs and nothing vanishes
    # silently.
    is_vh <- function(keys) grepl("^version_history(\\.|$)", keys)
    vh_cols <- c("key", "ancestor", "current", "target")
    not_rebased <- rbind(
      classified$changes[is_vh(classified$changes$key), vh_cols, drop = FALSE],
      classified$conflicts[is_vh(classified$conflicts$key), vh_cols, drop = FALSE]
    )
    classified$changes <- classified$changes[!is_vh(classified$changes$key), , drop = FALSE]
    classified$conflicts <- classified$conflicts[!is_vh(classified$conflicts$key), , drop = FALSE]
    rownames(classified$changes) <- NULL
    rownames(classified$conflicts) <- NULL
    rownames(not_rebased) <- NULL

    list(
      not_rebased = not_rebased,
      from_version = from_version,
      to_version = to_version_chr,
      ancestor_available = ancestor_available,
      changes = classified$changes,
      conflicts = classified$conflicts,
      ancestor = ancestor,
      target = target,
      target_provenance = target_provenance,
      # Dataset-level rebase is out of scope for rebase_apply() (see this
      # file's header comment) -- this flag says so on the plan itself, and
      # `dataset_changes` is included purely so a caller CAN show it; nothing
      # here acts on it.
      datasets_reportable_only = TRUE,
      dataset_changes = dta_diff(current, target)$datasets
    )
  })

  if (!isTRUE(res$ok)) {
    return(list(
      ok = FALSE,
      from_version = NA_character_,
      to_version = as.character(to_version %||% NA_character_),
      ancestor_available = NA,
      changes = NULL,
      conflicts = NULL,
      not_rebased = NULL,
      ancestor = NULL,
      target = NULL,
      target_provenance = NULL,
      datasets_reportable_only = TRUE,
      dataset_changes = NULL,
      error = res$error
    ))
  }

  c(list(ok = TRUE), res$value, list(error = NA_character_))
}

# ---- rebase_apply() -----------------------------------------------------------

# Apply a rebase_plan() to `current`, given a resolution ("current" or
# "target") for every conflict. Returns dta_try() (value = the rebased DTA).
#
# Metadata-only: this writes `changes` (take_template rows, from `plan
# $target`) and resolved `conflicts` onto `current`'s metadata, appends a
# version_history entry, and updates metadata.template -- and NOTHING about
# any dataset, even though dta_diff()/rebase_plan() report dataset
# differences for display (see `datasets_reportable_only` above).
# Reconciling a dataset structurally (columns, rules, files) is a
# substantially different problem than merging scalar/nested metadata
# fields and is left for a future iteration; this function only ever touches
# `current@metadata`.
rebase_apply <- function(current, plan, resolutions = list()) {
  dta_try({
    if (!isTRUE(plan$ok)) {
      stop(plan$error %||% "Cannot apply a rebase plan that failed to build.")
    }

    conflicts <- plan$conflicts
    conflict_keys <- if (is.data.frame(conflicts) && nrow(conflicts) > 0) conflicts$key else character(0)

    resolved_names <- names(resolutions)
    valid_choice <- vapply(
      resolutions,
      function(x) identical(x, "current") || identical(x, "target"),
      logical(1)
    )
    resolved_keys <- if (is.null(resolved_names)) character(0) else resolved_names[valid_choice]

    # Every conflict needs an explicit, valid resolution -- an invalid value
    # (anything other than the literal strings "current"/"target") is treated
    # exactly like a missing one, never coerced or guessed at.
    unresolved <- setdiff(conflict_keys, resolved_keys)
    if (length(unresolved) > 0) {
      # Nothing below this point may run before every conflict is resolved:
      # `current` must come out of a failed call byte-for-byte what it went
      # in as, never half-rebased.
      stop(sprintf(
        "The following rebase conflict%s must be resolved (choose 'current' or 'target' for each) before applying: %s.",
        if (length(unresolved) > 1) "s" else "",
        paste(sort(unresolved, method = "radix"), collapse = ", ")
      ))
    }

    target <- plan$target
    target_leaves <- dta_metadata_leaves(DTAtools::metadata(target))

    # Write the TARGET's value for `key` onto `dta`'s metadata. Reuses
    # apply_template_metadata_path() (template_core.R) rather than
    # hand-rolling the scalar-vs-nested-path branch again: that function
    # already merges into an existing nested container (list_set_path())
    # instead of replacing it wholesale, so a sibling field the user set
    # under the SAME top-level property (e.g. a different key.path under
    # "supplier" than the one being resolved) survives untouched.
    apply_from_target <- function(dta, key) {
      apply_template_metadata_path(dta, paste0("metadata.", key), target_leaves[[key]])
    }

    result <- current

    changes <- plan$changes
    if (is.data.frame(changes) && nrow(changes) > 0) {
      take_keys <- changes$key[changes$resolution == "take_template"]
      for (k in take_keys) {
        result <- apply_from_target(result, k)
      }
      # "keep_user" rows: `result` already has the user's value; nothing to
      # write. "agree" rows: current already equals target; nothing to write.
    }

    for (k in conflict_keys) {
      if (identical(resolutions[[k]], "target")) {
        result <- apply_from_target(result, k)
      }
      # resolutions[[k]] == "current": leave `result` untouched at `k`.
    }

    md <- DTAtools::metadata(result)

    # A synthesised entry recording THIS rebase event on THIS document's own
    # history -- never copied from the target's version_history, which
    # describes the TEMPLATE's history, not the document's.
    new_entry <- list(
      version = plan$to_version,
      date = Sys.Date(),
      changes = sprintf(
        "Rebased from template version %s to %s.",
        plan$from_version, plan$to_version
      )
    )
    S7::prop(md, "version_history") <- c(S7::prop(md, "version_history"), list(new_entry))

    # metadata.template becomes the TARGET's own provenance record --
    # rebase_plan() already built this with `lineage` carried over from the
    # CURRENT document rather than re-derived from the target definition
    # (see the comment there) -- so this assignment both "updates ... to the
    # target's provenance" and "preserves lineage" in one write.
    S7::prop(md, "template") <- plan$target_provenance

    result@metadata <- md
    result
  })
}
