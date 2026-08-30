# -----------------------------------------------------------------------------
# The keystone: joins every already-finished template piece into one creation
# path.
#
# Before this file, template_core.R, template_index.R, template_inherit.R,
# dataset_template.R and party_profiles.R each worked on their own but had no
# caller that used them TOGETHER: an `extends:`-resolved definition never
# reached create_dta_from_template(), a `template:` dataset entry had nowhere
# to be built, and there was no single place that decided "carry-over, then
# party slots, then options, then provenance -- in that order, every time."
# This file is that place.
#
# Two entry points do the joining:
#
#   load_template_definition(ref, index)   ref ("id"/"id@version"/"id@latest")
#                                           -> a fully `extends:`-merged
#                                           definition, ready to build from.
#   build_template_datasets(def, index, ...) def$datasets -> built DTADataSet
#                                           objects, resolving `template:`
#                                           entries through the dataset-
#                                           template machinery in
#                                           dataset_template.R.
#
# create_dta_from_template() (template_core.R) is extended, not replaced, to
# call these when a caller supplies an `index` -- every caller that does not
# (all 55 pre-existing tests) takes the exact code path that predates this
# file, byte-identical.
#
# `%||%` and `dta_try()` are already bound in the app helper environment (see
# utils_dta.R) -- not redefined here, same convention as every other template
# file.
# -----------------------------------------------------------------------------

# ---- Index-backed inheritance resolver --------------------------------------

# A `resolve_ref` function for resolve_template_inheritance() (template_
# inherit.R), backed by the template index instead of a test's hand-written
# stub. `extends:` always names another CREATION template -- a dataset
# template or a party profile has no inheritance chain of its own -- so the
# kind is fixed here, not parameterised.
#
# Returns NULL for "not in the index at all" (an ordinary, expected failure
# that resolve_template_inheritance() itself turns into its own "extends
# unresolvable reference" message, naming the child and the ref). A row that
# IS in the index but whose file cannot actually be read is a sharper, more
# specific problem -- the index and the file it points at disagree -- and
# gets its own cli_abort() naming the file, rather than being folded into the
# same generic "unresolvable" message.
template_index_resolver <- function(index) {
  function(ref) {
    row <- resolve_template_ref(index, ref, kind = "dta_creation_template")
    if (is.null(row)) {
      return(NULL)
    }

    read <- read_dta_creation_template(row$path[[1]])
    if (!isTRUE(read$ok)) {
      cli::cli_abort(
        "Template {.val {ref}} is indexed at {.path {row$path[[1]]}} but could not be read: {read$error}"
      )
    }

    list(def = read$value, id = row$id[[1]], version = row$version[[1]])
  }
}

# ---- Loading one creation template by reference -----------------------------

# Resolve `ref` ("id", "id@version" or "id@latest") in `index` to a fully
# `extends:`-merged creation-template definition.
#
# `index` defaults to the memoised, process-wide index (dta_template_index_
# cached()) so a caller does not have to build one just to load a template --
# but every test in this suite passes its own, freshly-built index instead, so
# that DTATOOLS_TEMPLATE_SOURCES fixtures never depend on (or leak into) the
# shared cache.
#
# `kind` governs only which kind THIS ref is resolved as; the inheritance
# CHAIN it may then walk is always "dta_creation_template" (see
# template_index_resolver()) -- a creation template can only extend another
# creation template.
load_template_definition <- function(ref, index = NULL, kind = "dta_creation_template") {
  dta_try({
    index <- index %||% dta_template_index_cached()

    row <- resolve_template_ref(index, ref, kind = kind)
    if (is.null(row)) {
      stop(sprintf("Could not resolve template reference '%s'.", as.character(ref %||% "")))
    }

    read <- read_dta_creation_template(row$path[[1]])
    if (!isTRUE(read$ok)) {
      stop(read$error)
    }

    resolved <- resolve_template_inheritance(read$value, template_index_resolver(index))
    def <- resolved$def

    # Abstract exists ONLY to be extended: a family's shared base/options/
    # datasets live there so every concrete descendant states just its own
    # deviation via `extends:`. Instantiating the abstract definition itself
    # would produce a document with no author's actual choices behind it --
    # caught once, here, rather than left for whichever downstream consumer
    # first trips over a half-specified template.
    if (isTRUE(def$abstract)) {
      stop(sprintf(
        "Template '%s' is abstract and exists to be extended (see 'extends:'), not instantiated directly.",
        as.character(def$id %||% row$id[[1]])
      ))
    }

    list(
      def = def,
      lineage = resolved$lineage,
      path = row$path[[1]],
      source_name = row$source_name[[1]],
      source_kind = row$source_kind[[1]],
      resolved_commit = row$resolved_commit[[1]],
      id = row$id[[1]],
      version = row$version[[1]]
    )
  })
}

# ---- Classifying one `datasets:` entry --------------------------------------

# Which of the four accepted shapes is one `datasets:` entry?
#
#   "path"     a bare character, e.g. "gf_dataset.yaml"
#   "source"   a list carrying {source: ...}
#   "template" a list carrying a non-empty {template: ...} -- checked BEFORE
#              "source" so a (currently hypothetical) entry naming both is
#              resolved as a template reference, not silently read as a plain
#              file source
#   "inline"   anything else list-shaped: a full inline dataset definition
#
# `is.character(ref)` alone decides "path", with no nzchar() guard -- matching
# create_dta_from_template()'s ORIGINAL branch exactly, including its exact
# failure mode for an empty string (resolve_template_dataset_path("", ...)
# returns "", so it fails downstream with the same "Could not resolve dataset
# source ''" message it always has, not a new/different one from a stricter
# check added here).
template_dataset_entry_kind <- function(ref) {
  if (is.character(ref)) {
    return("path")
  }
  if (is.list(ref)) {
    if (nzchar(as.character(ref$template %||% ""))) {
      return("template")
    }
    if (nzchar(as.character(ref$source %||% ""))) {
      return("source")
    }
    return("inline")
  }
  "inline"
}

# ---- Building every dataset entry -------------------------------------------

# Build every `datasets:` entry of `def` into a DTADataSet, resolving a
# `template:` entry through the dataset-template machinery (dataset_
# template.R) and reproducing the three legacy forms exactly as create_dta_
# from_template() has always built them.
#
# `selections` is accepted for symmetry with create_dta_from_template()'s own
# top-level option selections, but is NOT threaded into a `template:` entry's
# own dataset-template options today -- those come only from that entry's OWN
# `options:` map, which is the literal, static contract the spec describes
# ("build_dataset_from_template() with that entry's options and patch and
# as"). A creation template wanting a dataset's option to track a top-level
# user choice has to say so explicitly via its own option effects, not an
# implicit fallback here; wiring one up implicitly, with no test describing
# its precedence against an explicit `options:` entry, would be a guess this
# file has no business making. `template_path` is NOT one of the four
# arguments the calling convention advertises, but the three legacy forms
# cannot resolve a dataset path "relative to the template file" without it --
# it defaults to NULL, under which those two branches degrade to exactly what
# resolve_template_dataset_path() already does with a NULL template_path
# (absolute-path and package-extdata resolution still work; the
# relative-to-template-file branch simply never matches).
#
# Returns list(datasets = <list of DTADataSet objects>, provenance = <list,
# one entry per "template" dataset entry, in encounter order>). The legacy
# forms contribute NOTHING to `provenance` -- they carry no template identity
# to record, unlike an entry built from a dataset template.
build_template_datasets <- function(def, index, selections, source_label = NULL, template_path = NULL) {
  # Mirrors create_dta_from_template()'s own ${today} handling: only base$
  # datasets (the legacy `base.datasets` fallback) is expression-resolved --
  # the preferred top-level `datasets:` never was, and changing that now would
  # be a silent behaviour change to a form none of this exists to touch.
  today_env <- dta_template_today_env()
  base <- resolve_template_expressions(def$base %||% list(), today_env)
  ds_refs <- def$datasets %||% base$datasets %||% list()

  ds_list <- list()
  provenance <- list()

  for (i in seq_along(ds_refs)) {
    ref <- ds_refs[[i]]
    entry_kind <- template_dataset_entry_kind(ref)
    ds <- NULL

    if (identical(entry_kind, "template")) {
      tpl_ref <- as.character(ref$template)
      row <- resolve_template_ref(index, tpl_ref, kind = "dta_dataset_template")
      if (is.null(row)) {
        cli::cli_abort(
          "Could not resolve dataset template {.val {tpl_ref}} referenced by template {.val {as.character(def$id %||% def$label %||% '')}}."
        )
      }

      read <- read_dataset_template(row$path[[1]])
      if (!isTRUE(read$ok)) {
        cli::cli_abort("Dataset template {.val {tpl_ref}} could not be read: {read$error}")
      }

      built <- build_dataset_from_template(
        read$value,
        selections = ref$options %||% list(),
        patch = ref$patch,
        as_name = ref$as,
        source_label = source_label
      )
      if (!isTRUE(built$ok)) {
        cli::cli_abort("Dataset template {.val {tpl_ref}} could not be built: {built$error}")
      }

      ds <- DTAtools::dta_dataset_from_list(built$value$dataset)
      provenance[[length(provenance) + 1L]] <- built$value$provenance
    } else if (identical(entry_kind, "path")) {
      # Verbatim from create_dta_from_template()'s original character branch.
      src <- ref
      p <- resolve_template_dataset_path(src, template_path)
      if (!nzchar(p)) {
        stop(sprintf("Could not resolve dataset source '%s' for template '%s'.", src, def$label %||% ""))
      }
      ds <- DTAtools::read_dataset_from_yaml(p)
    } else if (identical(entry_kind, "source")) {
      # Verbatim from create_dta_from_template()'s original {source: ...} branch.
      src <- as.character(ref$source %||% "")
      p <- resolve_template_dataset_path(src, template_path)
      if (!nzchar(p)) {
        stop(sprintf("Could not resolve dataset source '%s' for template '%s'.", src, def$label %||% ""))
      }
      ds <- DTAtools::read_dataset_from_yaml(p)
    } else if (identical(entry_kind, "inline") && is.list(ref)) {
      # Verbatim from create_dta_from_template()'s original inline branch. The
      # extra is.list(ref) guard (template_dataset_entry_kind() documents only
      # four kinds, but its own fallback for a value that is NEITHER character
      # NOR list also reads "inline") keeps that one truly unclassifiable case
      # falling through to the generic "Invalid dataset definition" check
      # below, matching the ORIGINAL code's if/else-if chain exactly: a
      # `datasets:` entry that is neither a string nor a list never matched
      # any of its three branches either, and `ds` was left NULL.
      tf <- tempfile(fileext = ".yaml")
      yaml_txt <- yaml::as.yaml(ref, indent = 2, line.sep = "\n")
      writeLines(yaml_txt, tf, useBytes = TRUE)
      ds <- DTAtools::read_dataset_from_yaml(tf)
    }

    if (is.null(ds)) {
      stop(sprintf("Invalid dataset definition at index %s in template '%s'.", i, def$label %||% ""))
    }
    ds_list[[length(ds_list) + 1L]] <- ds
  }

  list(datasets = ds_list, provenance = provenance)
}

# ---- Party profiles reachable through the index -----------------------------

# Every kind = "dta_party_profile" row of `index`, read via read_party_
# profile(). A row that fails to parse is skipped with a warning naming its
# path, rather than aborting the whole document creation over an unrelated
# party profile some other author broke -- the same "one bad file never takes
# the whole picker down" philosophy build_template_index() already applies to
# the index itself.
template_party_profiles <- function(index) {
  if (is.null(index) || nrow(index) == 0) {
    return(list())
  }
  rows <- index[index$kind == "dta_party_profile", , drop = FALSE]
  if (nrow(rows) == 0) {
    return(list())
  }

  profiles <- list()
  for (i in seq_len(nrow(rows))) {
    read <- read_party_profile(rows$path[[i]])
    if (isTRUE(read$ok)) {
      profiles[[length(profiles) + 1L]] <- read$value
    } else {
      cli::cli_warn("Skipping unreadable party profile {.path {rows$path[[i]]}}: {read$error}")
    }
  }
  profiles
}

# ---- Metadata carry-over (rebase) -------------------------------------------

# The metadata fields carried over from an ancestor document BY DEFAULT when a
# caller does not state its own list.
#
# receiver/supplier/transmission/error_handling/authorized_for_corrections
# describe the RELATIONSHIP the document is about, which does not change just
# because a new version is being drafted from a template.
#
# title/version/date/version_history are DELIBERATELY excluded: they are the
# ancestor document's own identity and revision history, not properties of
# the relationship. A new document must state its OWN title and start its OWN
# version_history -- carrying the old one's forward would make the new
# document impersonate the old one instead of superseding it.
carry_over_default_fields <- function() {
  c("receiver", "supplier", "transmission", "error_handling", "authorized_for_corrections")
}

# Copy `fields` of `source_meta` (a DTAMetaData) onto `dta`.
#
# `dta_metadata_machine_fields()` (import_issues, template) are silently
# dropped from `fields` before anything else runs -- NEVER an error, so that
# carry_over_default_fields() (or any future default list) stays safe to
# extend without every caller having to remember to exclude them by hand.
# Provenance describes the document being CREATED NOW, not the one it was
# carried over from; copying the ancestor's @template would make the new
# document falsely claim the ancestor's lineage, which is exactly the forgery
# dta_metadata_machine_fields() exists to rule out (see template_core.R).
#
# Anything else NOT in dta_template_metadata_fields() is a genuine mistake --
# a typo'd field name -- and DOES abort, naming it.
apply_metadata_carry_over <- function(dta, source_meta, fields) {
  requested <- setdiff(as.character(fields %||% character(0)), dta_metadata_machine_fields())

  unknown <- setdiff(requested, dta_template_metadata_fields())
  if (length(unknown) > 0) {
    cli::cli_abort("Unknown metadata field{?s} for carry-over: {.field {unknown}}.")
  }

  for (field in requested) {
    value <- tryCatch(S7::prop(source_meta, field), error = function(e) NULL)
    if (is.null(value) || length(value) == 0) {
      next # nothing on the ancestor to carry -- leave the new document's own default
    }
    dta <- apply_template_metadata_path(dta, paste0("metadata.", field), value)
  }
  dta
}

# ---- Provenance -------------------------------------------------------------

# Is `x` empty for provenance-omission purposes? A plain length-0 check is not
# enough: an unresolved index field (resolved_commit for a "dir"/"builtin"
# source, for instance) is NA_character_ -- length 1, but nothing worth
# writing into a document either.
.template_provenance_is_empty <- function(x) {
  is.null(x) || length(x) == 0 || (length(x) == 1 && is.character(x) && is.na(x))
}

# Build the `metadata.template` provenance record.
#
# `def` is the FULLY `extends:`-merged definition (what was actually built
# from) -- content_hash is computed over exactly this, so two documents built
# from the same effective definition (whatever the `extends:` chain that
# produced it) hash identically, and a document built from a definition that
# has since been edited hashes differently. `meta` is the sibling metadata
# load_template_definition() returns alongside `def` (source_name,
# source_kind, resolved_commit, path, id, version) -- kept as a separate
# argument rather than folded into `def` because it describes WHERE the
# template came from, not what it says.
#
# `content_hash` is prefixed "hash:", not "sha256:": rlang::hash() is xxhash128,
# a fast CHANGE DETECTOR, not a cryptographic digest -- labelling it as one
# would overstate what it actually guarantees.
#
# `selections` is NEVER omitted, even when it is an empty list(): the rebase
# feature reconstructs the ancestor definition's effective state FROM this
# field, and an omitted key is indistinguishable from "this document predates
# recording selections at all" -- which cannot be trusted the same way as
# "this document recorded that zero options were overridden."
#
# Every other field is omitted rather than written empty (see
# .template_provenance_is_empty()), so a document built from a plain local
# "dir:" source -- no lineage, no dataset-template entries, no ancestor -- gets
# a clean, minimal provenance block instead of a wall of NA/empty keys.
template_provenance <- function(def, meta, selections, lineage = character(0),
                                ds_provenance = list(), carried_over_from = NULL) {
  full <- list(
    id = as.character(meta$id %||% def$id %||% ""),
    version = as.character(meta$version %||% def$version %||% ""),
    source = meta$source_name,
    source_kind = meta$source_kind,
    # The resolved file path is the most specific "which file, exactly" trace
    # available at this point -- more useful for tracking down a provenance
    # question than re-stating the id@version already recorded above.
    source_ref = meta$path,
    resolved_commit = meta$resolved_commit,
    content_hash = paste0("hash:", rlang::hash(def)),
    created = Sys.Date(),
    lineage = if (length(lineage) > 0) as.character(lineage) else NULL,
    selections = selections %||% list(),
    datasets = if (length(ds_provenance) > 0) ds_provenance else NULL,
    carried_over_from = carried_over_from
  )

  protected <- c("id", "version", "created", "content_hash", "selections")
  keep <- vapply(
    names(full),
    function(nm) nm %in% protected || !.template_provenance_is_empty(full[[nm]]),
    logical(1)
  )
  full[keep]
}
