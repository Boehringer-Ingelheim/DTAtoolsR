# -----------------------------------------------------------------------------
# A template's version history, COMPUTED rather than written.
#
# Nobody types a changelog for a library version. (The one hand-written part,
# `prior_versions:`, covers only versions from BEFORE the library -- see
# template_prior_versions() below. `base.metadata.version_history` is the
# created DOCUMENT's history and plays no part here.)
#
# Every published version is its own immutable file in the library (the
# versioning convention the rebase already depends on), so "what changed in
# 1.1" is answered by building 1.0 and 1.1 exactly as the "Create new from
# template" flow would and diffing the two -- with the same engine
# rebase_plan() uses (template_diff.R). A forgotten or wrong hand-written note
# cannot make a library version's history lie, because there is no such note.
#
# Three kinds of entry ("initial", below, is the oldest version with nothing
# to compare against):
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

# Build every structural choice explicitly, or the ordinary template with its
# defaults, returning what the history compares -- or the error, never a throw.
template_history_build <- function(ref, index) {
  dta_try({
    loaded <- load_template_definition(ref, index = index)
    if (!isTRUE(loaded$ok)) {
      stop(loaded$error)
    }
    def <- loaded$value$def
    build <- function(def, selections = list()) {
      built <- create_dta_from_template(
        def, loaded$value$path,
        selections = selections, index = index,
        vocab_selections = template_history_vocab_selections(def, index),
        # Both sides get a stand-in study identity, the same one whenever the
        # study option's first suggestion is, so a study token never shows up
        # as a change either.
        study_fallback = dta_template_dry_study(def)
      )
      if (!isTRUE(built$ok)) {
        stop(built$error)
      }
      list(
        def = def, lineage = loaded$value$lineage,
        mixins = loaded$value$mixins, dta = built$value
      )
    }
    if (is.null(def$variants)) {
      build(def)
    } else {
      spec <- def$variants
      variants <- list()
      for (choice in spec$choices) {
        selections <- stats::setNames(list(choice$value), spec$id)
        built <- dta_try({
          resolved <- resolve_template_variant(def, selections)
          # Creation takes option defaults from this choice's merged options/base.
          value <- build(resolved, selections)
          value$variant <- list(
            id = spec$id, value = choice$value, label = choice$label %||% choice$value
          )
          value
        })
        if (!isTRUE(built$ok)) {
          stop(sprintf("Variant '%s=%s': %s", spec$id, choice$value, built$error))
        }
        variants[[paste(spec$id, choice$value, sep = "\n")]] <- built$value
      }
      list(
        def = def, lineage = loaded$value$lineage,
        mixins = loaded$value$mixins, variants = variants
      )
    }
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
template_history_def_leaves <- function(def, lineage, mixins = character(0)) {
  .dta_leaf_flatten(list(
    label = def$label,
    description = def$description,
    based_on = if (length(lineage) > 0) paste(lineage, collapse = " < ") else NULL,
    mixes_in = if (length(mixins) > 0) paste(mixins, collapse = ", ") else NULL,
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
    Template = template_history_def_leaves(b$def, b$lineage, b$mixins),
    Metadata = dta_metadata_leaves(DTAtools::metadata(b$dta)),
    Datasets = dta_dataset_leaves(b$dta)
  )
}

# The things, per area, that can be wholly new or gone -- a dataset, a column,
# an option, a slot -- so that adding a column reads as one line rather than as
# one "added" line per field of it. One row per thing: `prefix` is the leaf-key
# prefix its fields share, `kind` what it is ("dataset", "columns",
# "row_labels", or the template section), `dataset` and `id` what to call it in
# a label, `parent` the prefix of the dataset a column belongs to.
#
# Read off the built objects rather than parsed out of the keys, because a
# dataset name or an id may itself contain a dot -- and every later step (which
# thing a leaf belongs to, what to call it) reads this table instead of
# splitting the key again.
template_history_entities <- function(b) {
  # NB paste() of a zero-length id vector still yields one string ("ds.columns."),
  # so every caller below skips an empty id set instead of calling this.
  rows <- function(prefix, kind, dataset = NA_character_, id = NA_character_, parent = NA_character_) {
    data.frame(
      prefix = prefix, kind = kind, dataset = dataset, id = id, parent = parent,
      stringsAsFactors = FALSE
    )
  }
  none <- rows(character(0), character(0), character(0), character(0), character(0))
  slot_rows <- function(section) {
    ids <- names(template_history_by_id(b$def[[section]]))
    if (length(ids) > 0) rows(paste0(section, ".", ids), section, id = ids)
  }
  ds <- dta_dataset_names(b$dta)
  col_rows <- lapply(ds, function(nm) {
    lst <- dta_dataset_to_list(dta_get_dataset(b$dta, nm))
    key <- dta_dataset_spec_key(lst)
    ids <- vapply(lst[[key]], function(col) as.character(col$id %||% ""), character(1))
    ids <- ids[nzchar(ids)]
    if (length(ids) > 0) rows(paste(nm, key, ids, sep = "."), key, dataset = nm, id = ids, parent = nm)
  })
  list(
    Template = do.call(rbind, c(list(none), lapply(c("options", "party_slots", "vocabulary_slots"), slot_rows))),
    Metadata = none,
    Datasets = do.call(rbind, c(
      list(none),
      if (length(ds) > 0) list(rows(ds, "dataset", dataset = ds)),
      col_rows
    ))
  )
}

# The row of `entities` a leaf key belongs to, or NA: the LONGEST prefix, and
# only a strict one (`prefix` + "."), so that a leaf is never mistaken for the
# thing itself. Longest, because `a` and `a.b` may both be columns and `a.b`'s
# field `type` must not be read as a field `b.type` of `a`.
template_history_owner <- function(key, entities) {
  if (nrow(entities) == 0) {
    return(NA_integer_)
  }
  hit <- which(startsWith(key, paste0(entities$prefix, ".")))
  if (length(hit) == 0) {
    return(NA_integer_)
  }
  hit[[which.max(nchar(entities$prefix[hit]))]]
}

# What a thing is called in a history line, from its row of the entity table;
# `rest` is the field path below it, if any.
template_history_entity_label <- function(entity, rest = character(0)) {
  tail <- if (length(rest) > 0) paste0(": ", paste(rest, collapse = " > ")) else ""
  thing <- c(
    options = "Option", party_slots = "Party slot", vocabulary_slots = "Vocabulary slot"
  )
  switch(entity$kind,
    dataset = sprintf("Dataset '%s'%s", entity$dataset, tail),
    columns = sprintf("Dataset '%s', column '%s'%s", entity$dataset, entity$id, tail),
    row_labels = sprintf("Dataset '%s', row label '%s'%s", entity$dataset, entity$id, tail),
    sprintf("%s '%s'%s", thing[[entity$kind]], entity$id, tail)
  )
}

# One human-readable phrase for a leaf key.
#
# With `entities` (the area's table, see template_history_entities()) the
# thing the leaf belongs to is looked up, so a column id that contains a dot
# reads whole. Without it -- or for a key that belongs to no thing, such as
# `label` or `based_on` -- the key is split on its dots, which is only right
# while no id contains one.
template_history_label <- function(area, key, entities = NULL) {
  if (!is.null(entities)) {
    owner <- template_history_owner(key, entities)
    if (!is.na(owner)) {
      prefix <- entities$prefix[[owner]]
      rest <- strsplit(substring(key, nchar(prefix) + 2L), ".", fixed = TRUE)[[1]]
      return(template_history_entity_label(entities[owner, , drop = FALSE], rest))
    }
  }
  parts <- strsplit(key, ".", fixed = TRUE)[[1]]
  rest <- function(from) {
    if (length(parts) >= from) paste0(": ", paste(parts[from:length(parts)], collapse = " > ")) else ""
  }
  thing <- c(
    options = "Option", party_slots = "Party slot", vocabulary_slots = "Vocabulary slot"
  )
  if (area == "Datasets") {
    if (length(parts) >= 3 && parts[[2]] %in% c("columns", "row_labels")) {
      what <- if (parts[[2]] == "row_labels") "row label" else "column"
      return(sprintf("Dataset '%s', %s '%s'%s", parts[[1]], what, parts[[3]], rest(4)))
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

# The zero-change frame template_history_compare() and its variants branch
# both promise, so a comparison that finds nothing is never NULL.
.template_history_empty_changes <- function() {
  data.frame(
    area = character(0), what = character(0), change = character(0),
    from = character(0), to = character(0), stringsAsFactors = FALSE
  )
}

# Compare two builds. Returns a data frame with one row per change:
# area, what (human-readable), change (added/removed/changed), from, to.
template_history_compare <- function(a, b) {
  if (!is.null(a$variants) || !is.null(b$variants)) {
    return(template_history_compare_variants(a, b))
  }
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
    entities <- unique(rbind(ea, eb))
    whole <- c(setdiff(eb$prefix, ea$prefix), setdiff(ea$prefix, eb$prefix))
    # A leaf belongs to its owner (the longest thing whose prefix it carries).
    # It is folded into a wholly new or gone thing -- the owner's dataset first,
    # so that a new dataset is one line and not one per column, then the owner
    # itself -- and is otherwise reported on its own.
    d$entity <- vapply(d$key, function(k) {
      owner <- template_history_owner(k, entities)
      if (is.na(owner)) {
        return(NA_character_)
      }
      parent <- entities$parent[[owner]]
      if (!is.na(parent) && parent %in% whole) {
        parent
      } else if (entities$prefix[[owner]] %in% whole) {
        entities$prefix[[owner]]
      } else {
        NA_character_
      }
    }, character(1), USE.NAMES = FALSE)
    single <- d[is.na(d$entity), , drop = FALSE]
    rows[[length(rows) + 1L]] <- data.frame(
      area = rep(area, nrow(single)),
      what = vapply(single$key, function(k) template_history_label(area, k, entities), character(1)),
      change = single$change, from = single$from, to = single$to,
      stringsAsFactors = FALSE
    )
    for (e in unique(d$entity[!is.na(d$entity)])) {
      rows[[length(rows) + 1L]] <- data.frame(
        area = area, what = template_history_entity_label(entities[match(e, entities$prefix), , drop = FALSE]),
        change = if (e %in% eb$prefix) "added" else "removed",
        from = NA_character_, to = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) {
    return(.template_history_empty_changes())
  }
  out <- do.call(rbind, rows)
  out <- out[order(match(out$area, names(la)), out$what, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Match choices by selector/value, never their position or display label.
# At an ordinary/variant boundary, compare each choice with the ordinary build
# as well as reporting the choice's arrival/removal.
template_history_compare_variants <- function(a, b) {
  av <- a$variants %||% list()
  bv <- b$variants %||% list()
  rows <- list()
  for (key in union(names(av), names(bv))) {
    old <- av[[key]]
    new <- bv[[key]]
    variant <- (new %||% old)$variant
    label <- sprintf(
      "Variant '%s', choice '%s' (%s)", variant$id, variant$value, variant$label
    )
    if (is.null(old) || is.null(new)) {
      rows[[length(rows) + 1L]] <- data.frame(
        area = "Template", what = label,
        change = if (is.null(old)) "added" else "removed",
        from = NA_character_, to = NA_character_, stringsAsFactors = FALSE
      )
    }
    if (is.null(a$variants)) old <- a
    if (is.null(b$variants)) new <- b
    if (!is.null(old) && !is.null(new)) {
      changes <- template_history_compare(old, new)
      if (nrow(changes) > 0) {
        changes$what <- paste0(label, ": ", changes$what)
      }
      rows[[length(rows) + 1L]] <- changes
    }
  }
  # No choice on either side: do.call(rbind, list()) is NULL, which order()
  # rejects. template_core.R requires >= 1 choice, so this is defensive only.
  if (length(rows) == 0) {
    return(.template_history_empty_changes())
  }
  out <- do.call(rbind, rows)
  out <- out[order(match(out$area, c("Template", "Metadata", "Datasets")), out$what,
    method = "radix"
  ), , drop = FALSE]
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

# The one hand-written part of a history: `prior_versions:`, the versions a
# template went through BEFORE it entered the library, which have no file to
# build and so nothing to diff. Each entry is version, optional date, changes.
#
# Read straight from the file -- never from the merged definition -- so a
# child never inherits its parent's past through `extends:`, and with the
# exact scalar handlers so an unquoted `4.10` is not the number 4.1.
#
# `before`: a version every entry must precede (the oldest library version, or
# the file's own). Returns list(declared, entries, problems); a problem is a
# sentence naming the entry, and a broken entry is left out of `entries`.
template_prior_versions <- function(path, before = NULL) {
  res <- dta_template_read_yaml_quiet(path, handlers = dta_template_yaml_handlers())
  raw <- if (is.list(res$value)) res$value[["prior_versions"]]
  # `prior_versions: []` and a bare `prior_versions:` both say nothing, so
  # neither hides what an older file declared.
  out <- list(declared = length(raw) > 0, entries = list(), problems = character(0))
  if (length(raw) == 0) {
    return(out)
  }
  if (!is.list(raw) || !is.null(names(raw))) {
    out$problems <- "'prior_versions' must be a sequence of entries, each with version, changes and an optional date."
    return(out)
  }
  text <- function(x) {
    if (is.null(x) || length(x) != 1 || is.na(x)) "" else trimws(as.character(x))
  }
  before_rank <- if (is.null(before)) NULL else template_version_rank(before)
  seen <- character(0)
  for (i in seq_along(raw)) {
    e <- raw[[i]]
    if (!is.list(e) || is.null(names(e))) {
      out$problems <- c(out$problems, sprintf("prior_versions entry %d is not a mapping.", i))
      next
    }
    # `[[`, not `$`: `$` partial-matches, so a misspelt `change:` would be read.
    v <- text(e[["version"]])
    label <- if (nzchar(v)) sprintf("prior_versions entry %d (%s)", i, v) else sprintf("prior_versions entry %d", i)
    d <- text(e[["date"]])
    changes <- text(e[["changes"]])
    problems <- c(
      if (length(setdiff(names(e), c("version", "date", "changes"))) > 0) {
        sprintf(
          "%s has unknown key(s): %s.", label,
          paste(setdiff(names(e), c("version", "date", "changes")), collapse = ", ")
        )
      },
      if (!nzchar(v)) sprintf("%s has no version.", label),
      if (nzchar(v) && is.na(template_version_rank(v))) sprintf("%s: '%s' is not a version number.", label, v),
      if (nzchar(v) && !is.na(template_version_rank(v)) && !is.null(before_rank) && !is.na(before_rank) &&
        template_version_rank(v) >= before_rank) {
        sprintf("%s is not older than %s; a version in the library has its changes computed.", label, before)
      },
      if (nzchar(v) && v %in% seen) sprintf("%s repeats an earlier entry.", label),
      if (nzchar(d) && (!grepl("^\\d{4}-\\d{2}-\\d{2}$", d) || is.na(as.Date(d, optional = TRUE)))) {
        sprintf("%s: date '%s' is not a YYYY-MM-DD date.", label, d)
      },
      if (!nzchar(changes)) sprintf("%s has no changes text.", label)
    )
    seen <- c(seen, v)
    if (length(problems) > 0) {
      out$problems <- c(out$problems, problems)
      next
    }
    out$entries[[length(out$entries) + 1L]] <- list(version = v, date = d, changes = changes)
  }
  if (length(out$entries) > 0) {
    ranks <- template_version_rank(vapply(out$entries, `[[`, "", "version"))
    out$entries <- out$entries[order(ranks, decreasing = TRUE)]
  }
  out
}

# The whole history of one creation template, newest entry first. Each entry:
#   type      "version", "initial" (the oldest version), "manual" (written by
#             hand in `prior_versions:`, older than "initial") or "upstream"
#   title     what the entry is about, e.g. "1.1 (compared with 1.0)"
#   version   the template version the entry describes
#   changes   template_history_compare()'s frame (empty for "initial"/"manual")
#   note      a "manual" entry's free text
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

  path_of <- function(v) index$path[index$kind == kind & index$id == id & index$version == v][[1]]
  newest <- own[[length(own)]]
  path <- path_of(newest)
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

  # Hand-written entries are older than anything above, so they come last.
  # The newest version that declares `prior_versions:` speaks for the past, so
  # a later release can add to or correct it without rewriting an old file.
  for (declaring in rev(own)) {
    prior <- template_prior_versions(path_of(declaring), before = own[[1]])
    if (prior$declared) {
      break
    }
  }
  for (p in prior$entries) {
    title <- if (nzchar(p$date)) {
      sprintf("%s (%s, recorded by hand)", p$version, p$date)
    } else {
      sprintf("%s (recorded by hand)", p$version)
    }
    e <- entry("manual", title, p$version)
    e$note <- p$changes
    out[[length(out) + 1L]] <- e
  }
  if (length(prior$problems) > 0) {
    out[[length(out) + 1L]] <- entry(
      "manual", sprintf("Earlier versions (prior_versions in %s)", ref(declaring)), NA_character_,
      error = paste(prior$problems, collapse = " ")
    )
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
