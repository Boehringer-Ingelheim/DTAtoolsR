# -----------------------------------------------------------------------------
# Versioned template index over every root returned by
# dta_template_source_roots() (template_sources.R).
#
# Four kinds of file live side by side in the same directories:
#
#   dta_creation_template  *.dta-template.yaml / .yml
#   dta_dataset_template   *.dta-dataset-template.yaml / .yml
#   dta_party_profile      *.dta-party.yaml / .yml
#   dta_vocabulary         *.dta-vocabulary.yaml / .yml
#
# This file turns that filesystem layout into one data frame the picker UI can
# filter/sort, plus a small `id[@version]` reference resolver so a template can
# `extends:` another one (template_inherit.R) without hand-rolling version
# comparison every time it needs "the newest compatible base".
#
# Design goals, same as template_sources.R:
# - one bad file never takes the whole picker down (read_template_header()
#   never throws; build_template_index() turns every failure into a warning)
# - every result is a value, not a side effect, so the UI and the tests see
#   exactly the same thing
# -----------------------------------------------------------------------------

# ---- Filename patterns -------------------------------------------------------

# The four kinds indexed here, in no particular order -- build_template_index()
# iterates it once per root.
dta_template_all_kinds <- function() {
  c("dta_creation_template", "dta_dataset_template", "dta_party_profile", "dta_vocabulary")
}

# Filename regex for one kind.
#
# Two of the suffixes share a stem ("dta-template" is a literal substring of
# "dta-dataset-template"), so the pattern is anchored on the FULL dotted
# segment -- "\\.dta-template\\." -- not merely on "template\\." at the end.
# That is what rules the overlap out structurally rather than by a lookaround:
# the character immediately after the leading dot differs ("dta-template" vs
# "dta-dataset-template"), so the creation-template regex cannot match a
# dataset-template filename no matter how long the rest of the suffix runs.
# The same anchoring keeps "dta-party" and "dta-vocabulary" disjoint from
# those two and from each other for free.
dta_template_kind_pattern <- function(kind) {
  suffix <- switch(kind,
    dta_creation_template = "dta-template",
    dta_dataset_template = "dta-dataset-template",
    dta_party_profile = "dta-party",
    dta_vocabulary = "dta-vocabulary",
    cli::cli_abort("Unknown template kind {.val {kind}}.")
  )
  paste0("\\.", suffix, "\\.ya?ml$")
}

# ---- Header reading ------------------------------------------------------

# yaml scalar handlers that hand back the ORIGINAL text of an int/float scalar
# instead of the R number yaml::read_yaml() would otherwise coerce it to.
#
# This exists for exactly one field: `version`. An unquoted `version: 1.0` is,
# to a YAML parser, just the number one -- yaml::read_yaml() returns the double
# 1, and as.character(1) is "1", silently dropping the ".0" that is the entire
# point of a version string ("1.0" and "1" must stay distinguishable; "1.9"
# must not collapse into anything that sorts next to "1.10" as a number would
# have to). Overriding "int" and the two float subtypes to return their
# untouched source text fixes this for the WHOLE parsed document, not just the
# `version` key -- which is fine here, because read_template_header() only
# reads a handful of NAMED fields out of the result and discards the rest, so
# a numeric-looking value elsewhere in the file (a `base:`/`datasets:` body
# read_template_header() never looks at) coming back as a string instead of a
# number has no effect on anything this function returns.
#
# The handler names are yaml's own vocabulary, not a guess: see the "custom
# scalar handler" example under `?yaml::yaml.load`, which uses the identical
# "float#fix" spelling. A plain "float" handler name -- the intuitive first
# guess -- is simply never invoked by the underlying libyaml resolver.
# Retained as the index's own name for the shared handler set in
# template_core.R, so there is exactly ONE definition of which libyaml scalar
# tags preserve their source text. See dta_template_yaml_handlers() for why the
# obvious tag name "float" does not work.
dta_template_header_yaml_handlers <- function() {
  dta_template_yaml_handlers()
}

# Read ONLY the header fields of one template file, tolerating anything else
# in it (a full `base:`/`datasets:`/`columns:` body, unrelated keys). This
# NEVER throws: a file that will not parse as YAML, is not a mapping, or is
# missing kind/id/version comes back as ok = FALSE with a human-readable
# `error`, for build_template_index() to skip and warn about rather than
# abort over. A single bad file in a large (or private, admin-curated) template
# repository must not take the whole "Create new from template" picker down.
read_template_header <- function(path) {
  result <- dta_try({
    if (is.null(path) || !nzchar(path) || !file.exists(path)) {
      stop("Template file not found.")
    }
    def <- yaml::read_yaml(path, handlers = dta_template_header_yaml_handlers())
    if (!is.list(def)) {
      stop("Template YAML must be a mapping/object.")
    }

    kind <- as.character(def$kind %||% "")
    id <- as.character(def$id %||% "")
    version <- as.character(def$version %||% "")
    if (!nzchar(kind) || !nzchar(id) || !nzchar(version)) {
      stop("Template header must define 'kind', 'id', and 'version'.")
    }

    list(
      kind = kind,
      id = id,
      version = version,
      label = as.character(def$label %||% id),
      description = as.character(def$description %||% ""),
      extends = as.character(def$extends %||% NA_character_),
      abstract = isTRUE(def$abstract),
      role = as.character(def$role %||% NA_character_),
      date = as.character(def$date %||% NA_character_)
    )
  })

  if (!result$ok) {
    return(list(
      ok = FALSE, kind = NA_character_, id = NA_character_,
      version = NA_character_, label = NA_character_,
      description = NA_character_, extends = NA_character_,
      abstract = FALSE, role = NA_character_, date = NA_character_,
      path = path, error = result$error
    ))
  }

  h <- result$value
  list(
    ok = TRUE, kind = h$kind, id = h$id, version = h$version,
    label = h$label, description = h$description, extends = h$extends,
    abstract = h$abstract, role = h$role, date = h$date,
    path = path, error = NA_character_
  )
}

# ---- Index construction -------------------------------------------------

# The column set build_template_index() promises, independent of whether any
# root has anything in it. Kept as its own function so every return path --
# the populated case and the zero-row case -- builds an identically shaped
# frame without repeating the list.
dta_template_index_columns <- function() {
  c(
    "kind", "id", "version", "label", "description", "path",
    "source_name", "source_kind", "resolved_commit", "abstract", "extends"
  )
}

# A zero-row data frame with the full column set -- never NULL, so a caller
# with no configured roots (or a source that resolved to nothing) can treat
# "no templates found" and "some templates found" identically.
dta_template_index_empty <- function() {
  cols <- dta_template_index_columns()
  df <- as.data.frame(
    stats::setNames(lapply(cols, function(x) character(0)), cols),
    stringsAsFactors = FALSE
  )
  df$abstract <- logical(0)
  df
}

# The resolved-source metadata (name/scheme/resolved_commit) for one root
# directory, looked up by exact path match against the `sources` list
# dta_template_source_roots() returns alongside `roots`.
#
# A root can have NO matching source record: dta_template_source_roots()
# appends the legacy/packaged directories from dta_creation_template_dirs()
# directly onto `roots` when builtins are included (see that function's own
# comment), rather than producing a resolve_template_source() record for them.
# Such a root is always a plain, pre-existing local directory -- never a git or
# package source -- so "dir"/"builtin" here are not a guess, they are the only
# thing a builtin root can be.
dta_template_source_for_root <- function(sources, root) {
  for (s in sources) {
    if (!is.na(s$root) && nzchar(s$root) && identical(s$root, root)) {
      return(list(name = s$name, scheme = s$scheme, resolved_commit = s$resolved_commit))
    }
  }
  list(name = "builtin", scheme = "dir", resolved_commit = NA_character_)
}

# Build the full template index by scanning every root, in precedence order,
# for all three kinds.
#
# Collision policy: the same kind + id + version found under two different
# roots keeps the file from the EARLIER root and drops the later one, with a
# warning naming both paths -- roots are already in precedence order (private
# sources before the packaged demo), so "earlier wins" is the same rule
# dta_creation_template_dirs() has always used for a basename collision, just
# extended from "same filename" to "same declared identity".
build_template_index <- function(refresh = FALSE) {
  src <- dta_template_source_roots(refresh = refresh)
  warnings_acc <- character(0)

  # kind + id + version as a single string key, so "have we already indexed
  # this?" is an O(1) environment lookup rather than a data-frame filter
  # re-run for every candidate file. Joined with an embedded newline rather
  # than a visible separator like "@" or "/": id and version come straight
  # from a template author's YAML and could in principle contain any of the
  # usual punctuation, so a visible join character could manufacture an
  # accidental collision (id "a" version "b@c" vs id "a@b" version "c");
  # a raw newline inside a single-line scalar cannot, because YAML has no
  # syntax for one there.
  seen <- new.env(parent = emptyenv())
  rows <- list()

  for (root in src$roots) {
    source_meta <- dta_template_source_for_root(src$sources, root)

    for (kind in dta_template_all_kinds()) {
      files <- list.files(
        root,
        pattern = dta_template_kind_pattern(kind),
        ignore.case = TRUE, full.names = TRUE, recursive = FALSE
      )

      for (f in files) {
        header <- read_template_header(f)
        if (!isTRUE(header$ok)) {
          warnings_acc <- c(warnings_acc, sprintf(
            "Skipped %s: %s", f, header$error %||% "unknown error"
          ))
          next
        }
        if (!identical(header$kind, kind)) {
          # Filename suffix says one kind, the file itself declares another --
          # surfacing this as a warning is safer than either silently trusting
          # the filename (indexing party-profile content as a dataset
          # template) or silently trusting the body (which would make the
          # per-kind filename convention meaningless).
          warnings_acc <- c(warnings_acc, sprintf(
            "Skipped %s: filename suffix implies kind '%s' but the file declares kind '%s'.",
            f, kind, header$kind
          ))
          next
        }

        key <- paste(kind, header$id, header$version, sep = "\n")
        prior <- seen[[key]]
        if (!is.null(prior)) {
          warnings_acc <- c(warnings_acc, sprintf(
            "Duplicate %s '%s@%s': keeping %s, ignoring %s.",
            kind, header$id, header$version, prior, f
          ))
          next
        }
        seen[[key]] <- f

        rows[[length(rows) + 1L]] <- data.frame(
          kind = kind,
          id = header$id,
          version = header$version,
          label = header$label,
          description = header$description,
          path = f,
          source_name = source_meta$name,
          source_kind = source_meta$scheme,
          resolved_commit = source_meta$resolved_commit,
          abstract = header$abstract,
          extends = header$extends,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  out <- if (length(rows) > 0) do.call(rbind, rows) else dta_template_index_empty()
  rownames(out) <- NULL

  attr(out, "warnings") <- warnings_acc
  attr(out, "sources") <- src$sources
  out
}

# ---- Version ranking and reference resolution --------------------------

# A numeric_version for a well-formed version string, or NA for one that will
# not parse -- vectorised, since every caller here needs it applied down a
# whole `version` column.
#
# strict = FALSE is the entire point: base::numeric_version()'s default
# (strict = TRUE) throws on anything malformed, which is exactly the "one bad
# template must not take the picker down" failure mode this whole file exists
# to avoid. NA sorts identifiably-last wherever this is fed into order().
template_version_rank <- function(v) {
  if (is.null(v) || length(v) == 0) {
    return(numeric_version(character(0)))
  }
  numeric_version(as.character(v), strict = FALSE)
}

# Resolve an `id`, `id@version`, or `id@latest` reference against one kind's
# rows of `index`. Returns the single matching row (a one-row data frame) or
# NULL when nothing matches -- resolution failure is not this function's to
# judge fatal; the caller (e.g. template_inherit.R's `extends:` resolution)
# decides that.
#
# Bare `id` and `id@latest` both mean "the highest version", ranked by
# template_version_rank() -- NOT by a string/lexicographic comparison, which
# would put "1.10" before "1.9". A version that will not parse ranks last and
# is therefore never chosen by a bare/`@latest` reference; it is reachable only
# by naming it exactly (`id@version`), because at that point resolution is a
# literal string match against the stored version text, not a rank comparison.
resolve_template_ref <- function(index, ref, kind = "dta_creation_template") {
  ref <- as.character(ref %||% "")
  if (!nzchar(ref) || is.null(index) || nrow(index) == 0) {
    return(NULL)
  }

  at_pos <- regexpr("@", ref, fixed = TRUE)
  if (at_pos > 0) {
    id <- substr(ref, 1, at_pos - 1)
    version_sel <- substr(ref, at_pos + 1, nchar(ref))
  } else {
    id <- ref
    version_sel <- "latest"
  }
  if (!nzchar(id)) {
    return(NULL)
  }

  candidates <- index[index$kind == kind & index$id == id, , drop = FALSE]
  if (nrow(candidates) == 0) {
    return(NULL)
  }

  if (identical(version_sel, "latest")) {
    ranks <- template_version_rank(candidates$version)
    # order()'s stability is what implements "ties broken by root order": for
    # two rows whose version ranks compare equal, this keeps them in their
    # original (root-precedence) order rather than an arbitrary one, so the
    # row already earliest in `index` wins the tie -- verified directly
    # against base::order()'s documented stability guarantee, not assumed.
    top <- order(ranks, decreasing = TRUE, na.last = TRUE)[[1]]
    if (is.na(ranks[[top]])) {
      # Every candidate's version is unparseable: a bare/`@latest` reference
      # has nothing valid to prefer, and must NOT fall back to picking one of
      # them arbitrarily.
      return(NULL)
    }
    return(candidates[top, , drop = FALSE])
  }

  exact <- candidates[candidates$version == version_sel, , drop = FALSE]
  if (nrow(exact) == 0) {
    return(NULL)
  }
  exact[1, , drop = FALSE]
}

# ---- Listing for the picker UI -------------------------------------------

# Rows of one kind, abstract templates excluded unless `include_abstract`
# is set, ordered by label then by descending version.
#
# Implemented as two SEPARATE stable sorts (least-significant key first) rather
# than one order() call over both keys: numeric_version is not one of the
# atomic types method = "radix" accepts, so label (character, radix-sortable)
# and version (numeric_version, not radix-sortable) cannot be combined into a
# single order() call at all here. Sorting by version first and then doing a
# STABLE sort by label preserves the version-descending sub-order within each
# label tie, which is the same "ordered by label then descending version" as
# a combined sort would have produced.
#
# method = "radix" on the label sort specifically: this machine collates under
# a German locale, CI does not, and a user-facing list whose order silently
# depends on which machine built it is a needless (and previously bitten)
# difference -- see the locale-collation lesson pinned elsewhere in this
# codebase's history.
list_template_index_entries <- function(index, kind = "dta_creation_template", include_abstract = FALSE) {
  rows <- index[index$kind == kind, , drop = FALSE]
  if (!include_abstract) {
    rows <- rows[!rows$abstract, , drop = FALSE]
  }
  if (nrow(rows) <= 1) {
    rownames(rows) <- NULL
    return(rows)
  }

  by_version <- order(template_version_rank(rows$version), decreasing = TRUE, na.last = TRUE)
  rows <- rows[by_version, , drop = FALSE]

  by_label <- order(rows$label, method = "radix")
  rows <- rows[by_label, , drop = FALSE]

  rownames(rows) <- NULL
  rows
}

# One row per distinct, non-abstract creation-template id: the picker's
# first step shows one entry per id, keeping whichever row ranks highest by
# version (template_version_rank(), template_index.R) for its label/
# description/source -- the SPECIFIC version is a separate control
# (template_id_versions(), below). Ordered by source then label, both with
# method = "radix": this machine collates under German locale, CI under C
# collation, and a user-facing list must not silently depend on which one
# built it (the project's pinned "locale collation diverges from CI"
# lesson).
template_picker_entries <- function(index) {
  rows <- index[index$kind == "dta_creation_template" & !index$abstract, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(rows)
  }
  ids <- unique(rows$id)
  picked <- lapply(ids, function(id) {
    sub <- rows[rows$id == id, , drop = FALSE]
    ranks <- template_version_rank(sub$version)
    top <- order(ranks, decreasing = TRUE, na.last = TRUE)[[1]]
    sub[top, , drop = FALSE]
  })
  out <- do.call(rbind, picked)
  out <- out[order(out$label, method = "radix"), , drop = FALSE]
  out <- out[order(out$source_name, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Every version of ONE creation-template id, newest first -- the choices
# for the "Version" selectInput in the picker's detail panel
# (template_picker_detail_ui(), template_ui.R). The panel is a separate
# renderUI that re-renders whenever the selected id changes, rather than
# pushing an update via updateSelectInput(): that is what makes "changing the
# template updates the version list" directly observable in the rendered HTML
# instead of only in a client-side round trip a test harness cannot see.
template_id_versions <- function(index, id) {
  rows <- index[index$kind == "dta_creation_template" & index$id == id, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(character(0))
  }
  ord <- order(template_version_rank(rows$version), decreasing = TRUE, na.last = TRUE)
  vers <- rows$version[ord]
  stats::setNames(vers, vers)
}

# The picker's family tree: template_picker_lineage() below infers it purely
# from the `extends` column already in `index` (no file reads, no separate
# schema key), and template_picker_filter() implements the search box over
# its result.

# The id -> {label, parent} lookup a chain is walked against. Built over
# EVERY creation-template row of `index`, abstract ones included: an
# abstract root is never itself an entry (template_picker_entries() drops
# it), but it can still be somebody's parent, and the chain needs its label
# to name the family. One row per id is kept, the top version by
# template_version_rank() -- the same tie-break template_picker_entries()
# applies to the entries themselves, so a family's root is described
# identically whichever function is asking.
template_picker_parent_lookup <- function(index) {
  rows <- index[index$kind == "dta_creation_template", , drop = FALSE]
  lookup <- new.env(parent = emptyenv())
  if (nrow(rows) == 0) {
    return(lookup)
  }
  ids <- unique(rows$id)
  for (id in ids) {
    sub <- rows[rows$id == id, , drop = FALSE]
    ranks <- template_version_rank(sub$version)
    top <- order(ranks, decreasing = TRUE, na.last = TRUE)[[1]]
    row <- sub[top, , drop = FALSE]
    lookup[[id]] <- list(
      label = row$label[[1]],
      parent = template_picker_strip_version(row$extends[[1]])
    )
  }
  lookup
}

# The parent id an `extends:` value names, with any "@version" suffix
# dropped -- NA/"" means "no parent" (a root). Deliberately its own function
# rather than inlining resolve_template_ref()'s id/version split: that
# function resolves a REFERENCE to a specific indexed row (and treats a bare
# id as "@latest"); this only ever needs the bare parent id, for a family
# tree that does not care which version of the parent is current.
template_picker_strip_version <- function(ref) {
  if (is.na(ref) || !nzchar(ref)) {
    return(NA_character_)
  }
  at_pos <- regexpr("@", ref, fixed = TRUE)
  if (at_pos > 0) substr(ref, 1, at_pos - 1) else ref
}

# Add family/search columns to `entries` (normally template_picker_entries()'s
# result), walking each row's `extends` chain against
# template_picker_parent_lookup(index).
#
# Columns added:
#   family        root ancestor id (own id when the row has no parent)
#   family_label  the root's label; the raw id when the root is unknown
#   depth         ancestor count (0 = root)
#   indent        of those ancestors, how many are THEMSELVES rows of
#                 `entries` -- an abstract or otherwise hidden ancestor still
#                 names the family (family/family_label/lineage) but must
#                 not visually indent the row under nothing
#   lineage       ancestor labels, root-first, joined by " \u203a "; "" at
#                 depth 0
#   search_text   everything a search should match, lower-cased: the row's
#                 own fields plus every ancestor's label and id, so
#                 searching a family name finds every variant even when a
#                 variant's own label never mentions it
#   sort_key      one radix-sortable string that puts the whole table into
#                 depth-first tree order (parent immediately before its
#                 children, siblings ordered by label then id) with a SINGLE
#                 order(sort_key, method = "radix") and no recursion. Built
#                 by encoding each ancestor, root-first, as "label\u0001id"
#                 and joining those with "\u0002": both separators sort
#                 below any ordinary character, so a parent's key is always
#                 a byte-prefix of every descendant's key -- which is what
#                 keeps a family together and ahead of the next one however
#                 the labels compare (a root labelled "Base2" must not land
#                 between "Base" and "Base"'s own children; a plain
#                 same-case string join with no separator could not tell
#                 "Base" + "2" apart from "Base" + a child whose label
#                 starts with a character greater than "2").
#
# The walk stops after dta_template_max_inheritance_depth hops (template_
# inherit.R's own constant, reused rather than redefined here), when a
# parent id repeats (a cycle), or when a parent id is not in the lookup at
# all -- that last case still appends the unknown id to the chain, labelled
# with itself, and goes no further because there is nothing left to look up.
template_picker_lineage <- function(index, entries) {
  if (nrow(entries) == 0) {
    out <- entries
    out$family <- character(0)
    out$family_label <- character(0)
    out$depth <- integer(0)
    out$indent <- integer(0)
    out$lineage <- character(0)
    out$search_text <- character(0)
    out$sort_key <- character(0)
    return(out)
  }

  lookup <- template_picker_parent_lookup(index)
  entry_ids <- entries$id

  chain_for <- function(i) {
    ids <- character(0)
    labels <- character(0)
    seen <- entries$id[[i]]
    parent_id <- template_picker_strip_version(entries$extends[[i]])

    while (length(ids) < dta_template_max_inheritance_depth) {
      if (is.na(parent_id) || parent_id %in% seen) {
        break
      }
      node <- lookup[[parent_id]]
      if (is.null(node)) {
        ids <- c(ids, parent_id)
        labels <- c(labels, parent_id)
        break
      }
      ids <- c(ids, parent_id)
      labels <- c(labels, node$label)
      seen <- c(seen, parent_id)
      parent_id <- node$parent
    }
    list(ids = ids, labels = labels)
  }

  n <- nrow(entries)
  chains <- lapply(seq_len(n), chain_for)

  depth <- vapply(chains, function(ch) length(ch$ids), integer(1))
  indent <- vapply(chains, function(ch) sum(ch$ids %in% entry_ids), integer(1))
  family <- vapply(seq_len(n), function(i) {
    ch <- chains[[i]]
    if (length(ch$ids) == 0) entries$id[[i]] else ch$ids[[length(ch$ids)]]
  }, character(1))
  family_label <- vapply(seq_len(n), function(i) {
    ch <- chains[[i]]
    if (length(ch$labels) == 0) entries$label[[i]] else ch$labels[[length(ch$labels)]]
  }, character(1))
  lineage <- vapply(chains, function(ch) {
    paste(rev(ch$labels), collapse = " \u203a ")
  }, character(1))
  search_text <- vapply(seq_len(n), function(i) {
    ch <- chains[[i]]
    # Not the source name: that has its own filter, and a source called
    # "lab" must not make the word "lab" match every template it holds.
    tolower(paste(
      entries$label[[i]], entries$id[[i]], entries$description[[i]],
      entries$version[[i]],
      paste(ch$labels, collapse = " "), paste(ch$ids, collapse = " ")
    ))
  }, character(1))
  sort_key <- vapply(seq_len(n), function(i) {
    ch <- chains[[i]]
    ancestor_nodes <- if (length(ch$ids) == 0) {
      character(0)
    } else {
      paste0(rev(ch$labels), "\u0001", rev(ch$ids))
    }
    own_node <- paste0(entries$label[[i]], "\u0001", entries$id[[i]])
    paste(c(ancestor_nodes, own_node), collapse = "\u0002")
  }, character(1))

  out <- entries
  out$family <- family
  out$family_label <- family_label
  out$depth <- depth
  out$indent <- indent
  out$lineage <- lineage
  out$search_text <- search_text
  out$sort_key <- sort_key

  out <- out[order(out$sort_key, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Every whitespace-separated search word in `query`, lower-cased -- the AND
# terms template_picker_filter() matches against search_text, and (later)
# what the picker highlights in a search result. character(0) for
# NULL/""/whitespace-only, so `for (tok in template_picker_tokens(x))`
# simply does not iterate when there is nothing to search for.
template_picker_tokens <- function(query) {
  if (is.null(query) || length(query) == 0) {
    return(character(0))
  }
  q <- trimws(tolower(query[[1]]))
  if (!nzchar(q)) {
    return(character(0))
  }
  toks <- strsplit(q, "\\s+")[[1]]
  toks[nzchar(toks)]
}

# Search tokens as one regex that matches only at the START of a word: "one"
# must not find "stand-alone" in every description, while "acme" must still
# find "biomarker_gf_acme" (an underscore ends a word) and "bio" must still
# find "biomarker". Every character of a token is literal. Shared by the
# filter below and the highlighter (template_picker_highlight(), template_
# ui.R), so what is marked is exactly what matched.
#
# "Word" is spelled out with Unicode properties rather than [[:alnum:]]: the
# POSIX class is ASCII-only in PCRE unless Unicode mode happens to be on, and
# a label such as "Prüfung" must not make "fung" a word start.
template_picker_regex_escape <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

template_picker_token_regex <- function(tokens) {
  paste0(
    "(?<![\\p{L}\\p{N}])(",
    paste(template_picker_regex_escape(tokens), collapse = "|"),
    ")"
  )
}

# `entries` (normally template_picker_lineage()'s result) restricted to rows
# whose search_text contains every token of `query` at the start of a word,
# case-insensitively (AND, not OR -- "biomarker acme" must narrow, not
# widen), and, when `source` is non-empty, to that source_name alone.
template_picker_filter <- function(entries, query = "", source = "") {
  tokens <- template_picker_tokens(query)
  keep <- rep(TRUE, nrow(entries))
  for (tok in tokens) {
    keep <- keep & grepl(template_picker_token_regex(tok), entries$search_text, perl = TRUE)
  }
  if (nzchar(source %||% "")) {
    keep <- keep & entries$source_name == source
  }
  out <- entries[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# ---- Memoised access -------------------------------------------------------

# File-local cache: a fresh scan of every configured root (each of which may
# be a git fetch) is not something the UI should pay for on every render, but
# a stale index after a "Refresh templates" click would be worse than the cost
# it saves. Kept in its own environment, rather than as a `<<-`-mutated
# variable, so dta_template_index_invalidate() has a single, obvious thing to
# clear.
.dta_template_index_cache <- new.env(parent = emptyenv())

# Memoised build_template_index(). Rebuilds when: `refresh` is requested
# explicitly, nothing has been cached yet, or the cached build is older than
# dta_template_refresh_seconds() -- the same TTL governing git source
# refreshes, so the index does not go stale relative to the sources it was
# built from.
dta_template_index_cached <- function(refresh = FALSE) {
  now <- as.numeric(Sys.time())
  cached <- .dta_template_index_cache$index
  built_at <- .dta_template_index_cache$built_at %||% NA_real_

  stale <- is.null(cached) || is.na(built_at) ||
    (now - built_at) >= dta_template_refresh_seconds()

  if (refresh || stale) {
    cached <- build_template_index(refresh = refresh)
    .dta_template_index_cache$index <- cached
    .dta_template_index_cache$built_at <- now
  }

  cached
}

# Clear the memoised index. Called by the UI's "Refresh templates" action so
# the NEXT read is guaranteed to re-scan, without waiting out the TTL.
dta_template_index_invalidate <- function() {
  .dta_template_index_cache$index <- NULL
  .dta_template_index_cache$built_at <- NA_real_
  invisible(NULL)
}
