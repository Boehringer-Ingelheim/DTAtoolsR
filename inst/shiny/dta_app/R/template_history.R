# -----------------------------------------------------------------------------
# A template's version history, COMPUTED rather than written.
#
# Nobody types a changelog here. Every published version is its own immutable
# file in the library (the versioning convention the rebase already depends
# on), so "what changed in 1.1" is answered by building 1.0 and 1.1 exactly as
# the "Create new from template" flow would and diffing the two -- with the
# same engine rebase_plan() uses (template_diff.R). A forgotten or wrong hand-
# written note cannot make the history lie, because there is no such note.
#
# Two kinds of entry:
#
#   "version"   the template's OWN versions, each against its predecessor,
#               both built against today's library. This isolates what the
#               template's author changed.
#   "upstream"  a dependency the template FOLLOWS (a bare `id` or `id@latest`
#               in `extends:`, a `datasets:` import, a vocabulary reference)
#               published a new version. The template's newest version is
#               built twice, against the library cut off just before and just
#               after that version, so the entry shows what that release
#               changed in what this template builds -- even though the
#               template's own version number never moved. A PINNED
#               dependency produces no such entry: the cut-off either changes
#               nothing or leaves the pin unresolvable, and both are skipped.
#
# `@latest` is only ever an alias here: every entry names concrete versions.
#
# Three areas are compared, because the built document alone does not show
# everything a template is:
#   Template   label, description, lineage and the template's interface --
#              options, party and vocabulary slots, `required`, `sealed` --
#              which never reach the built DTA when defaults are used.
#   Metadata   dta_metadata_leaves() of the built DTA.
#   Datasets   dta_dataset_leaves() of the built DTA (columns keyed by id).
#
# Both sides of every comparison are built with the same selections (option
# defaults, vocabulary slots topped up to their `min:`) on the same day, so
# ${today} and ${version} resolve identically and never show up as changes.
# -----------------------------------------------------------------------------

# Build one creation-template ref the way a user accepting every default
# would, returning what the history compares -- or the error, never a throw.
template_history_build <- function(ref, index) {
  dta_try({
    loaded <- load_template_definition(ref, index = index)
    if (!isTRUE(loaded$ok)) {
      stop(loaded$error)
    }
    def <- loaded$value$def
    built <- create_dta_from_template(
      def, loaded$value$path,
      index = index,
      vocab_selections = template_history_vocab_selections(def, index)
    )
    if (!isTRUE(built$ok)) {
      stop(built$error)
    }
    list(def = def, lineage = loaded$value$lineage, dta = built$value)
  })
}

# Vocabulary selections a dry run needs: every slot's own default, topped up
# to its `min:` from the head of the offered terms. Same rule as the
# instantiate dry-run in R/validateTemplate.R -- a slot that requires a choice
# is not a broken template, it just has not been filled in.
template_history_vocab_selections <- function(def, index) {
  slots <- normalise_vocabulary_slots(def$vocabulary_slots)
  out <- list()
  for (slot in slots) {
    if (length(slot$default) >= slot$min) {
      next
    }
    terms <- vocabulary_slot_choices(slot, vocabulary_resolver(index))$terms
    codes <- vapply(terms, function(t) t$code, character(1))
    out[[slot$id]] <- utils::head(unique(c(slot$default, codes)), slot$min)
  }
  out
}

# A list of records keyed by their own `id` instead of by position, so
# inserting an option does not report every later one as changed -- the same
# reason dta_dataset_leaves() keys columns by id.
template_history_by_id <- function(x) {
  x <- x %||% list()
  if (!is.list(x) || length(x) == 0) {
    return(list())
  }
  ids <- vapply(seq_along(x), function(i) {
    e <- x[[i]]
    as.character((if (is.list(e)) e$id) %||% i)
  }, character(1))
  stats::setNames(lapply(x, function(e) {
    if (is.list(e)) e$id <- NULL
    # A record that was nothing but its id would flatten to no leaves at all,
    # and its arrival or removal would go unreported.
    if (length(e) == 0) list(defined = TRUE) else e
  }), ids)
}

# The "Template" area: what a template is beyond what it builds.
template_history_def_leaves <- function(def, lineage) {
  .dta_leaf_flatten(list(
    label = def$label,
    description = def$description,
    based_on = if (length(lineage) > 0) paste(lineage, collapse = " < ") else NULL,
    options = template_history_by_id(def$options),
    party_slots = template_history_by_id(def$party_slots),
    vocabulary_slots = template_history_by_id(def$vocabulary_slots),
    required = def$required,
    sealed = def$sealed
  ))
}

# Every leaf of one built template, per area.
template_history_leaves <- function(b) {
  list(
    Template = template_history_def_leaves(b$def, b$lineage),
    Metadata = dta_metadata_leaves(DTAtools::metadata(b$dta)),
    Datasets = dta_dataset_leaves(b$dta)
  )
}

# The leaf-key prefixes, per area, of things that can be wholly new or gone --
# a dataset, a column, an option, a slot -- so that adding a column reads as
# one line rather than as one "added" line per field of it. Read off the
# built objects rather than parsed out of the keys, because a dataset name or
# an id may itself contain a dot.
template_history_entities <- function(b) {
  slot_ids <- function(section) {
    paste0(section, ".", names(template_history_by_id(b$def[[section]])))
  }
  ds <- dta_dataset_names(b$dta)
  cols <- unlist(lapply(ds, function(nm) {
    cl <- dta_dataset_to_list(dta_get_dataset(b$dta, nm))$columns
    ids <- vapply(cl, function(col) as.character(col$id %||% ""), character(1))
    paste(nm, "columns", ids[nzchar(ids)], sep = ".")
  }))
  list(
    Template = c(slot_ids("options"), slot_ids("party_slots"), slot_ids("vocabulary_slots")),
    Metadata = character(0),
    Datasets = c(ds, cols)
  )
}

# One human-readable phrase for a leaf key or entity prefix.
template_history_label <- function(area, key) {
  parts <- strsplit(key, ".", fixed = TRUE)[[1]]
  rest <- function(from) {
    if (length(parts) >= from) paste0(": ", paste(parts[from:length(parts)], collapse = " > ")) else ""
  }
  thing <- c(
    options = "Option", party_slots = "Party slot", vocabulary_slots = "Vocabulary slot"
  )
  if (area == "Datasets") {
    if (length(parts) >= 3 && parts[[2]] == "columns") {
      return(sprintf("Dataset '%s', column '%s'%s", parts[[1]], parts[[3]], rest(4)))
    }
    return(sprintf("Dataset '%s'%s", parts[[1]], rest(2)))
  }
  if (area == "Template" && parts[[1]] %in% names(thing) && length(parts) >= 2) {
    return(sprintf("%s '%s'%s", thing[[parts[[1]]]], parts[[2]], rest(3)))
  }
  if (area == "Template" && identical(key, "based_on")) {
    return("Based on")
  }
  paste(parts, collapse = " > ")
}

# Compare two builds. Returns a data frame with one row per change:
# area, what (human-readable), change (added/removed/changed), from, to.
template_history_compare <- function(a, b) {
  la <- template_history_leaves(a)
  lb <- template_history_leaves(b)
  ea_all <- template_history_entities(a)
  eb_all <- template_history_entities(b)

  rows <- list()
  for (area in names(la)) {
    d <- dta_diff_two_way(la[[area]], lb[[area]])
    if (nrow(d) == 0) {
      next
    }
    ea <- ea_all[[area]]
    eb <- eb_all[[area]]
    # Shortest first, so a new dataset swallows its new columns too.
    whole <- c(setdiff(eb, ea), setdiff(ea, eb))
    whole <- whole[order(nchar(whole))]
    d$entity <- NA_character_
    for (e in whole) {
      hit <- is.na(d$entity) & startsWith(d$key, paste0(e, "."))
      d$entity[hit] <- e
    }
    single <- d[is.na(d$entity), , drop = FALSE]
    rows[[length(rows) + 1L]] <- data.frame(
      area = rep(area, nrow(single)),
      what = vapply(single$key, function(k) template_history_label(area, k), character(1)),
      change = single$change, from = single$from, to = single$to,
      stringsAsFactors = FALSE
    )
    for (e in unique(d$entity[!is.na(d$entity)])) {
      rows[[length(rows) + 1L]] <- data.frame(
        area = area, what = template_history_label(area, e),
        change = if (e %in% eb) "added" else "removed",
        from = NA_character_, to = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) {
    return(data.frame(
      area = character(0), what = character(0), change = character(0),
      from = character(0), to = character(0), stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, rows)
  out <- out[order(match(out$area, names(la)), out$what, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Parseable versions of one kind+id, OLDEST first. An unparseable version has
# no place in a sequence, so it is left out rather than guessed into one.
template_history_versions <- function(index, kind, id) {
  v <- index$version[index$kind == kind & index$id == id]
  r <- template_version_rank(v)
  v <- v[!is.na(r)]
  v[order(template_version_rank(v))]
}

# Every kind+id the template can reach: ids mentioned, as text, in its own
# file, then in any file of an id already reached, until nothing new turns
# up. Plain text on purpose -- every reference (`extends:`, a `datasets:`
# import, a vocabulary in a slot or a column) is written as the id somewhere
# in a file of the closure, so this cannot miss one; mentioning an id that is
# not a reference only costs an extra build that finds no difference.
template_history_dependencies <- function(index, path, kind, id) {
  read_text <- function(p) paste(readLines(p, warn = FALSE), collapse = "\n")
  text <- read_text(path)
  key <- paste(index$kind, index$id, sep = "\n")
  # The template's own older versions are not dependencies, and reading them
  # would drag in whatever only those versions referenced.
  reached <- paste(kind, id, sep = "\n")
  repeat {
    todo <- setdiff(unique(key), reached)
    hits <- todo[vapply(todo, function(k) {
      id <- sub("^[^\n]*\n", "", k)
      grepl(paste0("\\b\\Q", id, "\\E\\b"), text, perl = TRUE)
    }, logical(1))]
    if (length(hits) == 0) {
      break
    }
    reached <- c(reached, hits)
    text <- paste(c(text, vapply(unique(index$path[key %in% hits]), read_text, character(1))),
      collapse = "\n"
    )
  }
  lapply(strsplit(reached[-1], "\n", fixed = TRUE), function(p) list(kind = p[[1]], id = p[[2]]))
}

# The library as it stood when `id` had not yet gone past `version`.
template_history_cut <- function(index, kind, id, version) {
  r <- template_version_rank(index$version)
  later <- index$kind == kind & index$id == id &
    (is.na(r) | r > numeric_version(version))
  index[!later, , drop = FALSE]
}

# The whole history of one creation template, newest entry first. Each entry:
#   type      "version", "initial" (the oldest version) or "upstream"
#   title     what the entry is about, e.g. "1.1 (compared with 1.0)"
#   version   the template version the entry describes
#   changes   template_history_compare()'s frame (empty for "initial")
#   error     NULL, or why the entry could not be computed
#
# Costs one build per version plus one per release of every dependency; the
# picker goes through template_history_cached() so a version switch is free.
template_history <- function(index, id) {
  kind <- "dta_creation_template"
  if (is.null(index) || nrow(index) == 0) {
    return(list())
  }
  own <- template_history_versions(index, kind, id)
  if (length(own) == 0) {
    return(list())
  }
  ref <- function(v) paste0(id, "@", v)
  entry <- function(type, title, version, changes = NULL, error = NULL) {
    list(type = type, title = title, version = version, changes = changes, error = error)
  }
  outcome <- function(type, title, version, a, b) {
    if (!isTRUE(a$ok) || !isTRUE(b$ok)) {
      err <- if (!isTRUE(b$ok)) b$error else a$error
      return(entry(type, title, version, error = err))
    }
    entry(type, title, version, changes = template_history_compare(a$value, b$value))
  }

  builds <- lapply(own, function(v) template_history_build(ref(v), index))
  out <- list(entry("initial", sprintf("%s (first version)", own[[1]]), own[[1]]))
  for (i in seq_along(own)[-1]) {
    out[[length(out) + 1L]] <- outcome(
      "version", sprintf("%s (compared with %s)", own[[i]], own[[i - 1L]]),
      own[[i]], builds[[i - 1L]], builds[[i]]
    )
  }
  out <- rev(out)

  newest <- own[[length(own)]]
  path <- index$path[index$kind == kind & index$id == id & index$version == newest][[1]]
  for (dep in template_history_dependencies(index, path, kind, id)) {
    vers <- template_history_versions(index, dep$kind, dep$id)
    if (length(vers) < 2) {
      next
    }
    states <- lapply(vers, function(v) {
      template_history_build(ref(newest), template_history_cut(index, dep$kind, dep$id, v))
    })
    for (i in rev(seq_along(vers)[-1])) {
      # Not buildable BEFORE the release: the template pins a later version,
      # so this release is not one it follows.
      if (!isTRUE(states[[i - 1L]]$ok)) {
        next
      }
      e <- outcome(
        "upstream",
        sprintf("%s %s -> %s, followed by %s", dep$id, vers[[i - 1L]], vers[[i]], ref(newest)),
        newest, states[[i - 1L]], states[[i]]
      )
      if (!is.null(e$error) || nrow(e$changes) > 0) {
        out[[length(out) + 1L]] <- e
      }
    }
  }
  out
}

# template_history() memoised per index and template. The files' modification
# times are part of the key, so editing a template in a `dir:` source and
# clicking "Refresh templates" is never answered from a stale entry.
template_history_cache <- new.env(parent = emptyenv())

template_history_cached <- function(index, id) {
  key <- rlang::hash(list(index, id, file.mtime(index$path)))
  if (is.null(template_history_cache[[key]])) {
    assign(key, template_history(index, id), envir = template_history_cache)
  }
  template_history_cache[[key]]
}
