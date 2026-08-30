# -----------------------------------------------------------------------------
# `extends:` inheritance for creation templates.
#
# Purpose: let a supplier- or study-specific DEVIATION template state only
# what differs from a standard parent, instead of copy-pasting the whole
# parent and drifting from it the moment the parent is fixed. A child names
# its parent with `extends:`; this file resolves the chain and deep-merges
# child over parent, nearest ancestor last.
#
# Design constraint: this file does NOT know how to look a reference up. A
# `resolve_ref` function is INJECTED into resolve_template_inheritance() by
# the caller. The template INDEX (which directories to search, which file
# matches an id, which version is "latest") is a separate, not-yet-written
# concern; keeping it out of this file means the merge logic here is pure list
# arithmetic, unit-testable with plain R lists and a stub resolver, and the
# index can be wired in later without touching a single merge rule.
#
# `%||%` and `dta_try()` are already bound in the app helper environment (see
# utils_dta.R) -- not redefined here. `resolve_template_inheritance()`
# deliberately does NOT use dta_try(): its failures (cycle, depth, unresolvable
# reference) are real conditions the caller is meant to see via expect_error()/
# tryCatch(), not a swallowed $ok/$error outcome.
# -----------------------------------------------------------------------------

# How many `extends:` hops a chain may take before it is almost certainly a
# mistake (or a cycle that the id@version comparison below did not happen to
# catch on a coincidental miss). A real template family is at most a handful
# of levels deep -- standard -> region -> supplier, say -- so 8 is generous
# headroom, not a tight budget.
dta_template_max_inheritance_depth <- 8L

# Is `x` a YAML MAPPING (every element named) rather than a YAML SEQUENCE
# (every element unnamed) or a scalar? An empty list() is treated as neither:
# YAML collapses an empty mapping and an empty sequence to the same R shape,
# so there is nothing here to recurse into either way, and callers fall
# through to plain replacement -- which, for an empty parent, produces the
# same result a key-by-key merge would have.
dta_template_is_mapping <- function(x) {
  is.list(x) && length(x) > 0 && !is.null(names(x)) && all(nzchar(names(x)))
}

# Recursive deep merge of two values, CHILD wins.
#
# The one non-obvious rule is what an explicit YAML null means. Verified
# empirically (see the "removes a key" test in the accompanying test file)
# that `yaml::yaml.load("a: ~")` and `yaml.load("a: null")` both parse to a list
# where "a" IS present in names() with element value NULL -- the exact same
# shape as writing `list(a = NULL, b = 1)` directly in R. That is what makes
# the check below correct: iterating `names(child)` and reading `child[[k]]`
# sees a real NULL for a key the author explicitly nulled out, and never sees
# a key that was simply never written (such a key never appears in
# names(child) at all, so the loop never visits it and `result[[k]]` -- copied
# from `parent` -- is left exactly as the parent had it). Assigning
# `result[[k]] <- NULL` then does what a child author means by "cancel this
# inherited field": in base R, `[[<-` with a NULL value REMOVES the element,
# so the key disappears from the merged result rather than surviving as a
# null-valued no-op.
dta_template_merge_value <- function(parent, child) {
  if (dta_template_is_mapping(parent) && dta_template_is_mapping(child)) {
    result <- parent
    for (k in names(child)) {
      cv <- child[[k]]
      if (is.null(cv)) {
        # Explicit `key: null` in the child cancels whatever the parent had.
        result[[k]] <- NULL
      } else {
        result[[k]] <- dta_template_merge_value(result[[k]], cv)
      }
    }
    return(result)
  }
  # Anything else -- a YAML sequence, a scalar, or a mapping paired with a
  # non-mapping parent -- is a wholesale replacement. This is deliberate for
  # sequences in particular: element-wise merging a list of column specs by
  # position would silently splice unrelated columns together the moment a
  # child's list is a different length than the parent's.
  child
}

# Drop options (or party_slots -- see below) with no id / an empty id, from
# ONE side, warning once with a count rather than once per item. An id is how
# every other rule here identifies an entry -- same-id merge, `remove:`,
# `order:` -- so an entry without one cannot participate in any of them, and
# silently discarding it would be a debugging nightmare no one could diagnose
# from the merged output alone.
dta_template_drop_unidentified <- function(items, side) {
  has_id <- vapply(
    items,
    function(it) is.list(it) && is.character(it$id) && length(it$id) == 1 && nzchar(it$id),
    logical(1)
  )
  if (any(!has_id)) {
    # cli's `{?y/ies}` pluralisation only tracks a plain variable reference,
    # not an inline expression -- `{sum(!has_id)}` measured 2 and still
    # printed "entry", verified empirically -- hence binding `n` first.
    n <- sum(!has_id)
    cli::cli_warn(
      "Dropping {n} {side} entr{?y/ies} with a missing or empty {.field id}."
    )
  }
  items[has_id]
}

# Reorder a list of id-bearing items to match a requested id order. Ids named
# in `order` that do not exist are ignored (with a warning); items not named
# in `order` keep their existing relative order and follow the ones that are.
dta_template_reorder_by_id <- function(items, order) {
  ids <- vapply(items, function(it) as.character(it$id), character(1))
  unknown <- setdiff(order, ids)
  n <- length(unknown)
  if (n > 0) {
    cli::cli_warn(
      "Ignoring {n} unknown id{?s} in {.field order}: {paste(unknown, collapse = ', ')}."
    )
  }
  known_order <- order[order %in% ids]
  c(items[match(known_order, ids)], items[!(ids %in% known_order)])
}

# Merge two unnamed lists of id-keyed maps (creation-template `options:`, and
# -- reused as-is -- `party_slots:`).
#
# `parent_opts`/`child_opts` are both allowed to be missing/empty; `order`, if
# supplied, is a character vector of ids the RESULT should end up sorted by.
dta_template_merge_options <- function(parent_opts, child_opts, order = NULL) {
  parent_opts <- dta_template_drop_unidentified(parent_opts %||% list(), "parent")
  child_opts <- dta_template_drop_unidentified(child_opts %||% list(), "child")

  parent_ids <- vapply(parent_opts, function(o) as.character(o$id), character(1))
  child_ids <- vapply(child_opts, function(o) as.character(o$id), character(1))
  child_by_id <- stats::setNames(child_opts, child_ids)

  # 1) Walk the parent in its own order. A same-id child entry either merges
  # in place (keeping the parent's position -- that is what "inherited" means
  # for ordering purposes) or, with `remove: true`, drops out entirely.
  result <- list()
  for (i in seq_along(parent_opts)) {
    id <- parent_ids[[i]]
    c_opt <- child_by_id[[id]]
    if (is.null(c_opt)) {
      result[[length(result) + 1L]] <- parent_opts[[i]]
    } else if (!isTRUE(c_opt$remove)) {
      merged <- dta_template_merge_value(parent_opts[[i]], c_opt)
      merged$remove <- NULL # a merge instruction, not template content
      result[[length(result) + 1L]] <- merged
    }
    # remove: true and matched -> dropped, nothing appended.
  }

  # 2) Append genuinely new child ids, in the order the child declared them.
  # `child_ids` is already in child order, so filtering it (rather than
  # `names(child_by_id)`, which setNames() does not reorder but which is one
  # extra indirection to reason about) keeps that order visible here.
  new_ids <- child_ids[!(child_ids %in% parent_ids)]
  for (id in unique(new_ids)) {
    c_opt <- child_by_id[[id]]
    if (isTRUE(c_opt$remove)) next # nothing inherited to remove
    c_opt$remove <- NULL
    result[[length(result) + 1L]] <- c_opt
  }

  if (!is.null(order) && length(order) > 0) {
    result <- dta_template_reorder_by_id(result, order)
  }

  result
}

# Identity key for one `datasets:` entry, by precedence: `as:`, else
# `template:` (with any `@version` pin stripped -- `gf@3.0` and `gf@4.0` name
# the SAME slot in a template family, just different pinned revisions of it,
# so a deviation that bumps the pin must still match and override, not
# duplicate), else `source:`, else `name:`, else -- for a bare character
# entry -- the string itself. NA when none of these apply, which marks the
# entry as unkeyable: it can never be matched against anything and is always
# appended as-is.
dta_template_dataset_key <- function(entry) {
  if (is.character(entry) && length(entry) == 1 && nzchar(entry)) {
    return(entry)
  }
  if (!is.list(entry)) {
    return(NA_character_)
  }
  for (field in c("as", "source", "name")) {
    v <- entry[[field]]
    if (!is.null(v) && is.character(v) && length(v) == 1 && nzchar(v)) {
      return(v)
    }
    # `as:` must beat `template:`, so `template:` cannot be folded into this
    # same loop -- handle it in between explicitly.
    if (identical(field, "as")) {
      tpl <- entry[["template"]]
      if (!is.null(tpl) && is.character(tpl) && length(tpl) == 1 && nzchar(tpl)) {
        return(sub("@.*$", "", tpl))
      }
    }
  }
  NA_character_
}

# Merge two unnamed lists of dataset entries by the identity key above.
dta_template_merge_datasets <- function(parent_ds, child_ds) {
  parent_ds <- parent_ds %||% list()
  child_ds <- child_ds %||% list()

  parent_keys <- vapply(parent_ds, dta_template_dataset_key, character(1))
  child_keys <- vapply(child_ds, dta_template_dataset_key, character(1))

  result <- list()
  matched_child_idx <- logical(length(child_ds))

  # 1) Walk parent entries; an unkeyable parent entry (key NA) can never match
  # anything below and simply survives untouched, same as one with no matching
  # child entry.
  for (i in seq_along(parent_ds)) {
    key <- parent_keys[[i]]
    hit <- if (!is.na(key)) which(!is.na(child_keys) & child_keys == key) else integer(0)
    if (length(hit) == 0) {
      result[[length(result) + 1L]] <- parent_ds[[i]]
      next
    }
    j <- hit[[1]]
    matched_child_idx[[j]] <- TRUE
    c_entry <- child_ds[[j]]
    if (is.list(c_entry) && isTRUE(c_entry$remove)) {
      next # dropped: the child cancels this inherited dataset entirely
    }
    merged <- dta_template_merge_value(parent_ds[[i]], c_entry)
    if (is.list(merged)) merged$remove <- NULL
    result[[length(result) + 1L]] <- merged
  }

  # 2) Append child entries that never matched a parent entry -- new keys, and
  # every unkeyable child entry (which, by construction, never set
  # matched_child_idx above) -- in the child's own order.
  for (j in seq_along(child_ds)) {
    if (matched_child_idx[[j]]) next
    c_entry <- child_ds[[j]]
    if (is.list(c_entry) && isTRUE(c_entry$remove)) next # nothing to remove
    if (is.list(c_entry)) c_entry$remove <- NULL
    result[[length(result) + 1L]] <- c_entry
  }

  result
}

# Merge one whole-section value (currently only `base:`) with
# dta_template_merge_value(), while keeping "the child never wrote this
# section at all" distinct from "the child wrote it as an explicitly empty
# mapping". Both collapse to the same list() once a bare `%||% list()` has
# been applied to `child_field`, and dta_template_merge_value() cannot tell
# them apart from that point on: an ABSENT `base:` must leave the parent's
# section untouched, while `base: {}` (rare, but distinguishable at the
# yaml::read_yaml() level -- an absent key parses to NULL, an empty mapping
# parses to list()) really is an instruction to replace it. Checking
# is.null() BEFORE normalising the two apart is what preserves that
# distinction; found the hard way when a template with no `base:` override at
# all wiped out everything the parent had set.
dta_template_merge_section <- function(parent_section, child_field) {
  if (is.null(child_field)) {
    return(parent_section %||% list())
  }
  dta_template_merge_value(parent_section %||% list(), child_field)
}

# Resolve a template's `extends:` chain into one fully-merged definition.
#
# `resolve_ref` is `function(ref) -> list(def = <parent definition list>, id =
# <chr>, version = <chr>)`, or NULL when `ref` cannot be resolved. Injected
# rather than hard-coded so this file never has to know how a reference is
# looked up (see the file banner). `.depth`/`.seen` are recursion bookkeeping,
# not meant to be supplied by an external caller.
resolve_template_inheritance <- function(def, resolve_ref, .depth = 0L, .seen = character(0)) {
  # Every template -- the original child, and every def a resolver hands
  # back -- must declare its own id/version (never inherited, see below), so
  # this identity is always available to check against the lineage visited so
  # far. Comparing on the RESOLVED id@version rather than the raw `extends:`
  # string means "parent" and "parent@1.0" are recognised as the same node,
  # and a cycle is caught the moment the SAME node is reached twice, however
  # it was spelled at each hop.
  own_key <- paste0(as.character(def$id %||% ""), "@", as.character(def$version %||% ""))
  if (own_key %in% .seen) {
    cli::cli_abort(
      "Template inheritance cycle detected: {paste(c(.seen, own_key), collapse = ' -> ')}."
    )
  }

  ref <- def$extends
  if (is.null(ref) || !nzchar(as.character(ref))) {
    return(list(def = def, lineage = character(0)))
  }

  if (.depth >= dta_template_max_inheritance_depth) {
    cli::cli_abort(
      "Template inheritance chain exceeds the depth limit of {dta_template_max_inheritance_depth}: {paste(c(.seen, own_key), collapse = ' -> ')}."
    )
  }

  parent <- resolve_ref(as.character(ref))
  if (is.null(parent)) {
    cli::cli_abort(
      "Template {.val {as.character(def$id %||% '<unknown>')}} extends unresolvable reference {.val {as.character(ref)}}."
    )
  }

  # Recurse into the PARENT first, so a grandparent is merged into the parent
  # before the parent (now fully resolved) is merged into this child -- that
  # ordering is what makes "the parent overrides the grandparent" hold no
  # matter how many levels away the grandparent is.
  parent_resolved <- resolve_template_inheritance(
    parent$def, resolve_ref,
    .depth = .depth + 1L, .seen = c(.seen, own_key)
  )
  parent_def <- parent_resolved$def

  merged <- parent_def
  merged$base <- dta_template_merge_section(parent_def$base, def$base)
  merged$options <- dta_template_merge_options(
    parent_def$options %||% list(), def$options %||% list(),
    order = def$order
  )
  merged$datasets <- dta_template_merge_datasets(
    parent_def$datasets %||% list(), def$datasets %||% list()
  )
  # party_slots have no `remove:` requirement in the spec, but reusing the
  # options merge costs nothing and honours `remove:` there too if a template
  # author ever writes one -- harmless, not a feature anyone has to maintain
  # separately.
  merged$party_slots <- dta_template_merge_options(
    parent_def$party_slots %||% list(), def$party_slots %||% list()
  )

  for (field in c("label", "description", "abstract", "kind")) {
    if (!is.null(def[[field]])) {
      merged[[field]] <- def[[field]]
    }
  }

  # id/version are NEVER inherited: every template file must declare its own,
  # and the merged result always keeps the CHILD's. A child that inherited its
  # parent's id would shadow the parent in the (not-yet-written) template
  # index and make `metadata$template` provenance a lie -- the very thing
  # dta_metadata_machine_fields() in template_core.R exists to prevent from
  # the other direction.
  merged$id <- def$id
  merged$version <- def$version
  # `extends`/`order` are merge INSTRUCTIONS consumed above, not template
  # content -- neither belongs in the merged shape.
  merged$extends <- NULL
  merged$order <- NULL

  list(
    def = merged,
    lineage = c(paste0(parent$id, "@", parent$version), parent_resolved$lineage)
  )
}
