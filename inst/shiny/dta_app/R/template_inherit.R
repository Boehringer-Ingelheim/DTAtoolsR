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

# -----------------------------------------------------------------------------
# Collection verbs.
#
# A keyed collection -- `options:`, `datasets:`, `party_slots:`,
# `vocabulary_slots:`, and a dataset patch's columns -- may be written two ways.
#
# The BARE form is a plain sequence of entries and carries its intent
# IMPLICITLY: an entry whose key matches the parent's modifies it, an entry
# with a key the parent does not have is an addition, and `remove: true` on an
# entry drops it. That form is unchanged and stays fully supported.
#
# The VERB form states the same intents explicitly, and expresses three the
# bare form cannot:
#
#   datasets:
#     inherit: all          # all (default) | none | [ids]
#     remove: [gf_legacy]
#     add:    [{template: acme_extra@1.0}]
#     modify: [{id: gf_smrnaseq, options: {vendor_name: Acme}}]
#     order:  [gf_smrnaseq, acme_extra]
#
#   * `inherit: [ids]` takes only a named subset -- otherwise a child that wants
#     two of twenty inherited entries has to write eighteen removals.
#   * `inherit: none` replaces the parent's set wholesale.
#   * `add:`/`modify:` are checked against what is actually inherited, so a
#     mistyped id is an error. In the bare form the same typo silently becomes
#     an extra entry instead of the modification that was meant -- the failure
#     mode that motivated the explicit form.
#
# The two shapes are told apart at no syntactic cost: a sequence of entries is
# an unnamed list, a verb mapping is a fully named one, and
# dta_template_is_mapping() already draws exactly that line.
dta_template_collection_verbs <- c("inherit", "add", "remove", "modify", "order")

# Coerce one verb's payload to a list of entries. A single entry written as a
# bare mapping (`add: {id: x}` rather than `add: [{id: x}]`) is the obvious
# beginner's slip and means one unambiguous thing, so accept it rather than
# fail on a distinction YAML barely surfaces.
dta_template_verb_entries <- function(x) {
  if (is.null(x)) {
    return(list())
  }
  if (dta_template_is_mapping(x)) {
    return(list(x))
  }
  as.list(x)
}

# Identity key for an id-keyed entry (`options:`, `party_slots:`,
# `vocabulary_slots:`). The dataset sections key differently -- see
# dta_template_dataset_key() -- which is exactly why the verb machinery takes
# the key function as an argument rather than reaching for `$id` itself.
dta_template_option_key <- function(entry) {
  value <- if (is.list(entry)) entry$id else NULL
  if (is.character(value) && length(value) == 1 && nzchar(value)) {
    return(value)
  }
  NA_character_
}

# Split a collection section into the verb form's five parts, or report that it
# is the bare sequence form. `section` names the field for error messages.
dta_template_parse_collection <- function(spec, section) {
  if (!dta_template_is_mapping(spec)) {
    return(list(form = "list", items = spec %||% list()))
  }

  unknown <- setdiff(names(spec), dta_template_collection_verbs)
  if (length(unknown) > 0) {
    n <- length(unknown)
    keys <- paste(unknown, collapse = ", ")
    verbs <- paste(dta_template_collection_verbs, collapse = "/")
    cli::cli_abort(c(
      "{n} unknown key{?s} in {.field {section}}: {keys}.",
      i = "A collection is either a sequence of entries, or a mapping of {verbs}."
    ))
  }

  # `all`/`none` are reserved words here, so an entry whose id is literally
  # "all" or "none" cannot be named in `inherit:`. Renaming that entry is the
  # remedy; reserving two words buys a form that reads as prose everywhere else.
  # NOT `spec$inherit %||% "all"`: this file's `%||%` (utils_dta.R) treats a
  # ZERO-LENGTH left operand as absent too, so `inherit: []` would come back
  # "all" -- the exact opposite of what an author writing an empty id list
  # means, and silently, since inheriting everything looks like success. Ask
  # names(spec) whether the key was written, then let an empty list say none.
  inherit <- if ("inherit" %in% names(spec)) as.character(spec$inherit) else "all"
  if (length(inherit) == 0) {
    inherit <- "none"
  }

  list(
    form = "verbs",
    verbs = list(
      inherit = inherit,
      remove = as.character(spec$remove %||% character(0)),
      add = dta_template_verb_entries(spec$add),
      modify = dta_template_verb_entries(spec$modify),
      order = as.character(spec$order %||% character(0))
    )
  )
}

# Apply the verb form to a parent collection. `key_of` extracts an entry's
# identity (an id for options, the dataset identity key for datasets) and
# returns NA for an entry that has none.
#
# The verbs run in a fixed order -- inherit, remove, add, modify, order -- and
# it is the same order apply_dataset_patch() already uses, for the same
# reasons: shrink before growing so an `add:` may reuse a key the parent had,
# and adjust last so `modify:` can address an entry `add:` just introduced.
dta_template_apply_collection_verbs <- function(parent_items, verbs, section, key_of) {
  parent_items <- parent_items %||% list()
  keys_of <- function(items) vapply(items, key_of, character(1))

  # 1) inherit -- what the parent contributes before any verb runs.
  parent_keys <- keys_of(parent_items)
  if (identical(verbs$inherit, "none")) {
    result <- list()
  } else if (identical(verbs$inherit, "all")) {
    result <- parent_items
  } else {
    missing <- setdiff(verbs$inherit, parent_keys[!is.na(parent_keys)])
    if (length(missing) > 0) {
      # A NUMERIC quantity has to be in the message for cli to pluralise: with
      # only `section` and the id list -- two length-1 strings -- cli cannot
      # tell which drives `{?s}` and aborts with "Multiple quantities for
      # pluralization" instead of the message you meant to raise. Same trap as
      # the one documented on dta_template_drop_unidentified() below.
      n <- length(missing)
      ids <- paste(missing, collapse = ", ")
      cli::cli_abort("{n} unknown id{?s} in {.field {section}} inherit: {ids}.")
    }
    result <- parent_items[!is.na(parent_keys) & parent_keys %in% verbs$inherit]
  }

  # 2) remove.
  if (length(verbs$remove) > 0) {
    keys <- keys_of(result)
    missing <- setdiff(verbs$remove, keys[!is.na(keys)])
    if (length(missing) > 0) {
      n <- length(missing)
      ids <- paste(missing, collapse = ", ")
      cli::cli_abort("{n} unknown id{?s} in {.field {section}} remove: {ids}.")
    }
    result <- result[is.na(keys) | !(keys %in% verbs$remove)]
  }

  # 3) add -- required to be NEW, which is the whole point of writing it out.
  for (entry in verbs$add) {
    k <- key_of(entry)
    if (is.na(k)) {
      cli::cli_abort("An entry in {.field {section}.add} has no identifying key.")
    }
    keys <- keys_of(result)
    if (k %in% keys[!is.na(keys)]) {
      cli::cli_abort(c(
        "Cannot add {.val {k}} to {.field {section}}: it is already inherited.",
        i = "Use {.field modify} to change an inherited entry."
      ))
    }
    result[[length(result) + 1L]] <- entry
  }

  # 4) modify -- required to be PRESENT, for the same reason.
  for (entry in verbs$modify) {
    k <- key_of(entry)
    keys <- keys_of(result)
    idx <- if (is.na(k)) integer(0) else which(!is.na(keys) & keys == k)
    if (length(idx) == 0) {
      shown <- if (is.na(k)) "<unkeyed>" else k
      cli::cli_abort(c(
        "Cannot modify {.val {shown}} in {.field {section}}: no such entry is inherited.",
        i = "Use {.field add} to introduce a new entry."
      ))
    }
    result[[idx[[1]]]] <- dta_template_merge_value(result[[idx[[1]]]], entry)
  }

  # 5) order.
  if (length(verbs$order) > 0) {
    result <- dta_template_reorder_by_id(result, verbs$order, key_of = key_of)
  }

  result
}

# Which of the four states a child expressed for one section.
#
# This takes the child MAPPING and the key name rather than the extracted
# value, and that is the whole point: `def$base` collapses "never written" and
# "written as null" into the same NULL the moment it is evaluated, and only
# `"base" %in% names(def)` can tell them apart. Every section is read through
# here so the four states mean the same thing in all of them -- the asymmetry
# this replaces was an accident of one section being normalised at read time
# and another not.
#
#   absent -- key not written        -> inherit the parent's value
#   value  -- key written, non-empty -> override (mappings merge, sequences replace)
#   empty  -- `{}` / `[]` / `""`     -> present but blank
#   drop   -- `null` / `~`           -> gone from the template and from the output
dta_template_section_state <- function(def, key) {
  if (!(key %in% names(def))) {
    return("absent")
  }
  value <- def[[key]]
  if (is.null(value)) {
    return("drop")
  }
  if (length(value) == 0) {
    return("empty")
  }
  "value"
}

# An explicitly empty COLLECTION still inherits today, while an explicitly
# empty `base:` replaces. Warn on the gesture that is going to change meaning
# rather than change it underneath a private template repository with no
# notice; the flip lands a release later.
dta_template_warn_empty_collection <- function(section, parent_items) {
  n <- length(parent_items %||% list())
  if (n == 0) {
    return(invisible(NULL))
  }
  cli::cli_warn(c(
    "An empty {.field {section}:} currently inherits the parent's {n} entr{?y/ies}.",
    i = "In a future release it will mean {.emph none}. Write {.code inherit: none} to mean that now, or omit {.field {section}:} to keep inheriting."
  ))
  invisible(NULL)
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
dta_template_reorder_by_id <- function(items, order,
                                       key_of = function(it) as.character(it$id)) {
  ids <- vapply(items, key_of, character(1))
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
dta_template_merge_options <- function(parent_opts, child_opts, order = NULL,
                                       section = "options") {
  parent_opts <- dta_template_drop_unidentified(parent_opts %||% list(), "parent")

  parsed <- dta_template_parse_collection(child_opts, section)
  if (identical(parsed$form, "verbs")) {
    result <- dta_template_apply_collection_verbs(
      parent_opts, parsed$verbs, section, dta_template_option_key
    )
    # A top-level `order:` still applies when the verb form carries none of its
    # own: the two spellings are the same instruction, not two competing ones.
    if (length(parsed$verbs$order) == 0 && length(order %||% character(0)) > 0) {
      result <- dta_template_reorder_by_id(result, order, key_of = dta_template_option_key)
    }
    return(result)
  }

  child_opts <- dta_template_drop_unidentified(parsed$items, "child")

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
dta_template_merge_datasets <- function(parent_ds, child_ds, section = "datasets") {
  parent_ds <- parent_ds %||% list()

  parsed <- dta_template_parse_collection(child_ds, section)
  if (identical(parsed$form, "verbs")) {
    return(dta_template_apply_collection_verbs(
      parent_ds, parsed$verbs, section, dta_template_dataset_key
    ))
  }

  child_ds <- parsed$items

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

# Apply the four states to one COLLECTION section, deferring the actual merge
# to `merge_fn(parent_items, child_spec)`.
#
# Absent and empty both hand the parent's items back through `merge_fn` with an
# empty child rather than returning them raw: that keeps the parent running
# through dta_template_drop_unidentified() exactly as it always has, so a
# malformed parent entry is still reported at the same point it used to be.
dta_template_merge_collection_section <- function(def, key, parent_items, merge_fn) {
  parent_items <- parent_items %||% list()
  state <- dta_template_section_state(def, key)

  if (identical(state, "drop")) {
    return(NULL)
  }
  if (identical(state, "empty")) {
    # Release A keeps today's meaning -- an explicitly empty collection still
    # inherits -- and only warns. Release B returns list() here instead, which
    # is what the four states say it should do; see the migration note in the
    # vignette. `base: {}` already behaves that way, and that split is the
    # whole reason this function exists.
    dta_template_warn_empty_collection(key, parent_items)
    return(merge_fn(parent_items, list()))
  }
  if (identical(state, "absent")) {
    return(merge_fn(parent_items, list()))
  }
  merge_fn(parent_items, def[[key]])
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

  # `base:` is a mapping and the rest are keyed sequences, but every one of
  # them reads the SAME four states off the child. That uniformity is the
  # point: absent inherits, a value overrides, `{}`/`[]` is empty, `null`
  # drops -- in every section, at every depth.
  merged$base <- switch(dta_template_section_state(def, "base"),
    absent = parent_def$base %||% list(),
    drop = NULL,
    empty = list(),
    value = dta_template_merge_value(parent_def$base %||% list(), def$base)
  )

  merged$options <- dta_template_merge_collection_section(
    def, "options", parent_def$options,
    function(parent_items, child_spec) {
      dta_template_merge_options(
        parent_items, child_spec,
        order = def$order, section = "options"
      )
    }
  )
  merged$datasets <- dta_template_merge_collection_section(
    def, "datasets", parent_def$datasets,
    function(parent_items, child_spec) {
      dta_template_merge_datasets(parent_items, child_spec, section = "datasets")
    }
  )
  # party_slots have no `remove:` requirement in the spec, but reusing the
  # options merge costs nothing and honours `remove:` there too if a template
  # author ever writes one -- harmless, not a feature anyone has to maintain
  # separately.
  # Vocabulary slots merge by id exactly as party slots do, and for the same
  # reason: a deviation template overriding one slot's default (or dropping it
  # with `remove: true`) must not have to restate the others.
  merged$vocabulary_slots <- dta_template_merge_collection_section(
    def, "vocabulary_slots", parent_def$vocabulary_slots,
    function(parent_items, child_spec) {
      dta_template_merge_options(parent_items, child_spec, section = "vocabulary_slots")
    }
  )
  merged$party_slots <- dta_template_merge_collection_section(
    def, "party_slots", parent_def$party_slots,
    function(parent_items, child_spec) {
      dta_template_merge_options(parent_items, child_spec, section = "party_slots")
    }
  )

  # The scalars read the same four states, so `label: ""` (blank but present)
  # and `label: null` (gone) stop being the same instruction.
  for (field in c("label", "description", "abstract", "kind")) {
    state <- dta_template_section_state(def, field)
    if (identical(state, "absent")) {
      next
    }
    merged[[field]] <- if (identical(state, "drop")) NULL else def[[field]]
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
