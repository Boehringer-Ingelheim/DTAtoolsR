# -----------------------------------------------------------------------------
# validate_template(): CI-friendly linting for DTA templates, WITHOUT starting
# the Shiny app.
#
# The template engine (reading/normalising a template, merging an `extends:`
# chain, resolving a metadata/dataset target, applying a dataset patch,
# building a whole DTA from a creation template) lives entirely in
# inst/shiny/dta_app/R/*.R. Those files are auto-sourced by Shiny at launch and
# are NOT part of this package's namespace -- package code in R/ cannot call
# them directly. That split is a historical accident of where the template
# feature grew up, not a design choice: the engine ought to live in R/ proper,
# reachable from both the app and the package namespace without a private
# loader like the one below. Moving it is a larger refactor, deliberately not
# done here, because it touches every file the app sources and the app's own
# test harness (tests/testthat/helper-shinyapp.R) that already assumes the
# current split.
#
# .dta_template_engine() below is this file's OWN, package-side version of
# exactly the technique tests/testthat/helper-shinyapp.R's app_env() already
# uses to reach that same code from a test: source every helper file into a
# private environment and call into it. This file deliberately does NOT
# reimplement any of the engine's own logic (what a target resolves to,
# how an `extends:` chain merges, how a dataset patch applies) -- every check
# below either (a) calls straight into the sourced engine and turns a thrown
# condition into a reported row, or (b) consults a small, already-derived fact
# the engine itself exposes (e.g. `dta_template_metadata_fields()`). A second,
# hand-rolled copy of any of that logic would drift from the real one over
# time and end up validating something other than what the app actually does
# -- worse than not validating it at all.
#
# RULE: every list parsed from (or copied out of) a template author's YAML --
# `def`, a column, an option, an effect operation, a dataset entry, a
# vocabulary definition, and so on -- is read with `[[` (exact match), never
# `$`. `$` on a list falls back to PARTIAL name matching when there is no
# exact key, so `x$foo` silently returns `x$foobar` when `foo` is absent and
# `foobar` is the only key starting with `foo`. On YAML with real, adjacent
# field names (`values`/`values_from`, `dataset`/`datasets`,
# `effects`/`effects_all`), that turns "this field is absent" into "this
# field is present, with a different field's value" -- see the
# `col[["values"]]` and `def[["dataset"]]` call sites below for two field
# collisions this already bit. `$` stays fine for a list this file's OWN code
# constructs with a fixed, known key set (an internal record, a `dta_try()`
# result, the result/index data frames) -- those names are ours, so the hazard
# does not apply.
# -----------------------------------------------------------------------------

# ---- Engine access -----------------------------------------------------

# File-local cache: the engine's ~150 KB of helper code is parsed once per R
# session, not once per validate_template() call.
.dta_template_engine_cache <- new.env(parent = emptyenv())

# Locate, source, and cache the app's template-engine code.
#
# The environment's parent is the PACKAGE namespace (not shiny's, unlike
# app_env() in the test harness): every function this file actually calls
# (template_core.R, template_index.R, template_inherit.R, dataset_template.R,
# party_profiles.R, vocabulary.R) resolves everything it needs either from its own sibling
# file in the same sourced environment, or via a namespaced `pkg::fn()` call --
# none of them call an unqualified shiny/htmltools UI function at the TOP
# level of the file (only inside function bodies belonging to the UI-only
# files, which are sourced but never invoked here), so there is nothing in
# this environment's lookup chain that needs to reach shiny's namespace.
.dta_template_engine <- function() {
  if (!is.null(.dta_template_engine_cache$env)) {
    return(.dta_template_engine_cache$env)
  }

  app_dir <- system.file("shiny", "dta_app", package = "DTAtools")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    cli::cli_abort(c(
      "Could not locate the bundled Shiny app directory ({.file inst/shiny/dta_app}).",
      "i" = "validate_template() reuses the template engine that lives there; a
        development checkout missing {.file inst/} or a broken installation
        are the usual causes."
    ))
  }
  r_dir <- file.path(app_dir, "R")
  if (!dir.exists(r_dir)) {
    cli::cli_abort("Found {.file {app_dir}} but it has no {.file R/} directory of engine code.")
  }
  files <- sort(list.files(r_dir, pattern = "[.][Rr]$", full.names = TRUE), method = "radix")
  if (length(files) == 0) {
    cli::cli_abort("Found {.file {r_dir}} but it contains no {.file .R} files.")
  }

  env <- new.env(parent = asNamespace("DTAtools"))
  # THE TRAP this line closes (identical to app_env()'s in
  # tests/testthat/helper-shinyapp.R): several engine helpers locate bundled
  # assets with an UNQUALIFIED system.file(package = "DTAtools")
  # (resolve_template_dataset_path() in template_core.R, in particular, which
  # the instantiate-dry-run check below exercises). Under pkgload::load_all(),
  # only pkgload's own shim -- installed on the GLOBAL SEARCH PATH -- knows how
  # to resolve inst/ to the right place; an environment parented on a
  # namespace, like this one, never consults the search path. Binding the
  # caller's own (possibly shimmed) system.file() directly into this
  # environment closes that gap under both devtools::test()/load_all() and a
  # fully installed package, at zero cost either way.
  env$system.file <- system.file

  for (f in files) {
    sys.source(f, envir = env, keep.source = FALSE)
  }
  .dta_template_engine_cache$env <- env
  env
}

# Fetch one function from the engine environment by name, failing loudly (not
# with a confusing "attempt to apply non-function") if the engine has been
# renamed or reshuffled out from under this file.
.dta_template_engine_get <- function(name) {
  fn <- get0(name, envir = .dta_template_engine(), inherits = FALSE)
  if (!is.function(fn)) {
    cli::cli_abort(
      "Template engine function {.fn {name}} was not found in {.file inst/shiny/dta_app/R}."
    )
  }
  fn
}

# ---- Result shape -------------------------------------------------------

.dta_template_validation_columns <- function() {
  c("file", "kind", "id", "version", "severity", "code", "message")
}

# A zero-row data frame with the full column set -- never NULL, so "nothing to
# report" and "some issues found" share exactly one shape all the way through
# rbind()/order() below. Mirrors dta_template_index_empty()'s idiom in
# template_index.R.
.dta_template_validation_empty <- function() {
  cols <- .dta_template_validation_columns()
  as.data.frame(
    stats::setNames(lapply(cols, function(x) character(0)), cols),
    stringsAsFactors = FALSE
  )
}

# One issue row. `file`/`id`/`version` fall back to NA (never ""), the
# idiomatic R "missing" for a character column, so a caller can tell "declared
# as empty string" (were that ever possible) from "not present at all".
.dta_template_row <- function(file, kind, id, version, severity, code, message) {
  data.frame(
    file = as.character(file %||% NA_character_),
    kind = as.character(kind %||% NA_character_),
    id = as.character(id %||% NA_character_),
    version = as.character(version %||% NA_character_),
    severity = as.character(severity),
    code = as.character(code),
    message = as.character(message %||% NA_character_),
    stringsAsFactors = FALSE
  )
}

.dta_template_bind_rows <- function(rows) {
  if (length(rows) == 0) {
    return(.dta_template_validation_empty())
  }
  do.call(rbind, rows)
}

# ---- File discovery -------------------------------------------------------

# Directory listing for one or more kinds, matching the engine's own
# non-recursive, per-kind-suffix convention (dta_template_kind_pattern() /
# build_template_index() in template_index.R) rather than a bare "*.yaml"
# glob, which would also sweep up unrelated YAML the directory happens to
# contain (a rules fixture, a dataset spec used by `source:`).
.dta_template_list_dir <- function(dir, kinds) {
  patterns <- vapply(kinds, function(k) .dta_template_engine_get("dta_template_kind_pattern")(k), character(1))
  files <- unlist(
    lapply(
      patterns,
      function(pat) list.files(dir, pattern = pat, ignore.case = TRUE, full.names = TRUE, recursive = FALSE)
    ),
    use.names = FALSE
  )
  # method = "radix" rather than the locale-dependent default: this file's
  # output (which file "wins" a duplicate-id@version, which file the strict
  # abort lists first) must not depend on which machine ran it. See the
  # project's own "locale collation diverges from CI" lesson.
  sort(unique(files), method = "radix")
}

# Recursive counterpart to .dta_template_list_dir(), used ONLY to find files a
# non-recursive scan misses (.dta_template_check_nested(), below) -- never for
# anything that resolves a cross-file reference, where "the directory being
# validated is flat" is the documented contract.
#
# Same per-kind patterns as the non-recursive scan, so the two listings stay
# comparable by exact path, but with `recursive = TRUE` and a filter dropping
# any hit under a dot-led directory component (`.git`, `renv`, `.Rproj.user`,
# or any other dotfile-style directory a clone or an IDE leaves behind) --
# none of those ever holds an authored template, and `.git` in particular can
# be large enough that walking it on every call would be a real cost for no
# benefit.
.dta_template_list_nested <- function(dir, kinds) {
  hits <- lapply(kinds, function(k) {
    pat <- .dta_template_engine_get("dta_template_kind_pattern")(k)
    rel <- list.files(dir, pattern = pat, ignore.case = TRUE, recursive = TRUE)
    if (length(rel) == 0) {
      return(NULL)
    }
    data.frame(rel = rel, kind = k, stringsAsFactors = FALSE)
  })
  hits <- hits[!vapply(hits, is.null, logical(1))]
  if (length(hits) == 0) {
    return(data.frame(file = character(0), kind = character(0), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, hits)

  # `rel` is relative to `dir`, so its OWN directory components -- never
  # anything above `dir` -- are what decide "is this file inside a dot
  # directory"; a `dir` argument that itself sits below a dotfile must not
  # disqualify every file it contains.
  parts <- strsplit(out$rel, "[/\\]")
  under_dotdir <- vapply(parts, function(p) any(grepl("^[.]", utils::head(p, -1L))), logical(1))
  out <- out[!under_dotdir, , drop = FALSE]

  data.frame(file = file.path(dir, out$rel), kind = out$kind, stringsAsFactors = FALSE)
}

# ---- Reading one file exactly, without engine-level normalisation ------

# Parse ONE candidate file with the version-preserving handlers
# (dta_template_yaml_handlers(), template_core.R) so `version` survives as the
# literal text the author wrote, not a lossily-coerced double. This is
# deliberately a raw yaml::read_yaml() call, not one of the engine's own
# read_dta_creation_template()/read_dataset_template()/read_party_profile() --
# those enforce shape requirements (a `base:` section, a non-empty
# `datasets:`) that would turn "this file has a bad target path" into an
# opaque "template must contain a 'base' section" instead, for a file that is
# otherwise perfectly checkable. The engine's own readers are still used
# below, but only where their specific requirements are exactly what is being
# tested (the instantiate dry-run, an `extends:` parent lookup).
.dta_template_read_raw <- function(path) {
  handlers <- .dta_template_engine_get("dta_template_yaml_handlers")()
  def <- tryCatch(yaml::read_yaml(path, handlers = handlers), error = function(e) e)
  if (inherits(def, "error")) {
    return(list(
      ok = FALSE, kind = NA_character_, id = NA_character_, version = NA_character_,
      def = NULL, error = conditionMessage(def)
    ))
  }
  if (!is.list(def)) {
    return(list(
      ok = FALSE, kind = NA_character_, id = NA_character_, version = NA_character_,
      def = NULL, error = "Template YAML must be a mapping/object."
    ))
  }
  list(
    ok = TRUE,
    kind = .dta_template_scalar_chr(def[["kind"]]),
    id = .dta_template_scalar_chr(def[["id"]]),
    version = .dta_template_scalar_chr(def[["version"]]),
    def = def,
    error = NA_character_
  )
}

.dta_template_scalar_chr <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  as.character(x[[1]])
}

# Was `version:` quoted as written? Re-reads the file WITHOUT the
# version-preserving handlers -- an unquoted numeric-looking scalar parses to
# a double there, a quoted one stays character -- which is exactly
# dta_template_version_is_exact()'s own contract (template_core.R). A second
# read is cheap and keeps this file from having to reverse-engineer "was it
# quoted" out of the handler-preserved text, which cannot be done: the
# handlers hand back the same string either way.
.dta_template_version_plain_is_exact <- function(path) {
  raw <- tryCatch(yaml::read_yaml(path), error = function(e) NULL)
  if (!is.list(raw)) {
    return(TRUE) # cannot tell; do not manufacture a false positive
  }
  .dta_template_engine_get("dta_template_version_is_exact")(raw[["version"]])
}

# ---- Cross-file index (extends / datasets[].template / party profile ids) --

# A minimal id/version index, restricted to files that parsed and declared a
# recognised kind + non-empty id + non-empty version -- the only files that
# could ever legitimately be the TARGET of a reference. Handed straight to the
# engine's own resolve_template_ref() (template_index.R) for `extends:` and
# `datasets[].template` lookups, so "which version is latest" is decided by
# the exact same numeric_version() ranking the real app uses, not a
# hand-rolled comparison here.
.dta_template_build_index_df <- function(records) {
  paths <- names(records)
  all_kinds <- .dta_template_engine_get("dta_template_all_kinds")()

  kind <- vapply(paths, function(p) records[[p]]$kind %||% NA_character_, character(1))
  id <- vapply(paths, function(p) records[[p]]$id %||% NA_character_, character(1))
  version <- vapply(paths, function(p) records[[p]]$version %||% NA_character_, character(1))
  ok <- vapply(paths, function(p) isTRUE(records[[p]]$ok), logical(1))

  df <- data.frame(kind = kind, id = id, version = version, path = paths, stringsAsFactors = FALSE)
  keep <- ok & !is.na(df$kind) & !is.na(df$id) & !is.na(df$version) & df$kind %in% all_kinds
  df[keep, , drop = FALSE]
}

# `resolve_ref` for resolve_template_inheritance() (template_inherit.R):
# looks a reference up in the LOCAL directory index only (never the app's own
# multi-root dta_creation_template_dirs()/dta_template_source_roots()) -- a
# private repo's CI runs against exactly the directory it checked out, and
# "resolved within the directory being validated" is the documented contract
# for extends_unresolved/extends_cycle.
.dta_template_extends_resolver <- function(index_df) {
  function(ref) {
    hit <- .dta_template_engine_get("resolve_template_ref")(index_df, ref, kind = "dta_creation_template")
    if (is.null(hit) || nrow(hit) == 0) {
      return(NULL)
    }
    parent <- .dta_template_engine_get("read_dta_creation_template")(hit$path[[1]])
    if (!isTRUE(parent$ok)) {
      return(NULL)
    }
    list(def = parent$value, id = hit$id[[1]], version = hit$version[[1]])
  }
}

# `resolve_ref` for resolve_vocabulary_inheritance() (vocabulary.R). Same
# local-directory-only contract as the creation-template resolver above, but a
# different return shape: that one feeds resolve_template_inheritance(), which
# wants list(def=, id=, version=), while the vocabulary resolver hands back the
# parent definition directly.
.dta_template_vocab_resolver <- function(index_df) {
  function(ref) {
    hit <- .dta_template_engine_get("resolve_template_ref")(index_df, ref, kind = "dta_vocabulary")
    if (is.null(hit) || nrow(hit) == 0) {
      return(NULL)
    }
    parent <- .dta_template_engine_get("read_vocabulary")(hit$path[[1]])
    if (!isTRUE(parent$ok)) {
      return(NULL)
    }
    parent$value
  }
}

# The same lookup, but returning a vocabulary whose `extends:` chain is
# resolved -- what every consumer that reads `terms` needs.
#
# The two are deliberately separate. resolve_vocabulary_inheritance() must be
# handed the RAW resolver above, because it owns the `.seen`/`.depth` recursion
# that detects a cycle; a resolver that resolved inheritance itself would reset
# that bookkeeping at every hop. Anything that then wants the terms -- a
# `values_from:` include list, a slot's offered menu -- must use THIS one, or a
# child vocabulary's inherited terms are invisible to the check and a perfectly
# valid `include:` gets reported as naming an unknown code.
.dta_template_vocab_resolved <- function(index_df) {
  lookup <- .dta_template_vocab_resolver(index_df)
  function(ref) {
    raw <- lookup(ref)
    if (is.null(raw)) {
      return(NULL)
    }
    .dta_template_engine_get("resolve_vocabulary_inheritance")(raw, lookup)
  }
}

# ---- Target-path extraction (shared by metadata and dataset targets) -----

# Every literal path an option could write, across EVERY branch of its
# `effects:`/`effects_all:` -- not just whichever branch a default selection
# would currently pick. This is deliberate and is the whole reason
# target_machine_owned exists as a standalone check rather than relying on the
# instantiate dry-run alone: a forged-provenance target hidden behind an
# option's non-default branch would never be exercised by a dry run using
# default selections, and would otherwise ship undetected.
.dta_template_option_targets <- function(opt) {
  if (!is.list(opt)) {
    return(character(0))
  }
  paths <- character(0)

  target <- as.character(opt[["target"]] %||% "")
  if (nzchar(target)) {
    paths <- c(paths, target)
  }

  # `opt[["effects"]]`, NOT `opt$effects`: 'effects' is a strict prefix of the
  # sibling key 'effects_all', read separately two lines down. An option
  # authored with ONLY 'effects_all:' (no 'effects:') would have `opt$effects`
  # partial-match onto 'effects_all', handing this branch a plain
  # list-of-operations where it expects either a single {path:,value:} map or
  # a list keyed by option value -- silently wrong input shape, currently
  # harmless only by accident (an unnamed list has no "path" name and no
  # names() to iterate, so this branch happens to contribute zero paths either
  # way), not by design.
  eff <- opt[["effects"]] %||% list()
  if (!is.null(eff[["path"]])) {
    # A single operation written directly as a map, not keyed by value --
    # mirrors collect_option_effects()'s own "eff[['path']]" branch
    # (template_core.R).
    paths <- c(paths, as.character(eff[["path"]]))
  } else if (is.list(eff)) {
    for (key in names(eff)) {
      paths <- c(paths, .dta_template_effect_op_paths(eff[[key]]))
    }
  }

  paths <- c(paths, .dta_template_effect_op_paths(opt[["effects_all"]] %||% list()))
  unique(paths[nzchar(paths)])
}

# The path(s) named by one branch of an `effects:`/`effects_all:` list -- each
# entry is either `{path:, value:}` or `{set: {<path>: <value>, ...}}`, the
# same two shapes collect_option_effects() (template_core.R) reads.
.dta_template_effect_op_paths <- function(ops) {
  paths <- character(0)
  if (!is.list(ops)) {
    return(paths)
  }
  for (op in ops) {
    if (!is.list(op)) next
    if (!is.null(op[["set"]]) && is.list(op[["set"]])) {
      paths <- c(paths, names(op[["set"]]))
    } else if (!is.null(op[["path"]])) {
      paths <- c(paths, as.character(op[["path"]]))
    }
  }
  paths
}

# Classify a `metadata.*`-rooted target against the engine's own allowed/
# machine-owned field lists (template_core.R). Deliberately just a name
# lookup, not a call into apply_template_metadata_path() against a throwaway
# DTA: dta_template_metadata_fields()/dta_metadata_machine_fields() ARE the
# engine's single source of truth for this decision, so consulting them
# directly is already full reuse, not a parallel copy of it.
.dta_template_classify_metadata_target <- function(path, allowed, machine) {
  parts <- strsplit(as.character(path %||% ""), "\\.")[[1]]
  if (length(parts) < 2 || !identical(parts[[1]], "metadata")) {
    return("invalid")
  }
  top <- parts[[2]]
  if (top %in% machine) {
    return("machine")
  }
  if (top %in% allowed) {
    return("ok")
  }
  "invalid"
}

# ---- Per-file checks --------------------------------------------------

.dta_template_check_file <- function(path, rec, index_df, resolve_extends) {
  rows <- list()
  add <- function(kind, id, version, severity, code, message) {
    rows[[length(rows) + 1L]] <<- .dta_template_row(path, kind, id, version, severity, code, message)
  }

  if (!isTRUE(rec$ok)) {
    add(NA_character_, NA_character_, NA_character_, "error", "parse_failed", rec$error)
    return(.dta_template_bind_rows(rows))
  }

  def <- rec$def
  kind <- rec$kind
  id <- rec$id
  version <- rec$version
  all_kinds <- .dta_template_engine_get("dta_template_all_kinds")()
  kind_known <- !is.na(kind) && kind %in% all_kinds

  if (!kind_known) {
    add(
      kind, id, version, "warning", "kind_unknown",
      sprintf("kind '%s' is not one of the known template kinds (%s).", kind, paste(all_kinds, collapse = ", "))
    )
  }

  if (is.na(id) || !nzchar(id)) {
    add(kind, id, version, "warning", "id_missing", "Template is missing a non-empty 'id' field.")
  }

  version_present <- !is.na(version) && nzchar(version)
  if (!version_present) {
    add(kind, id, version, "warning", "version_missing", "Template is missing a non-empty 'version' field.")
  } else {
    if (!.dta_template_version_plain_is_exact(path)) {
      add(
        kind, id, version, "error", "version_unquoted",
        sprintf(
          "version %s was not quoted in the YAML; an unquoted numeric version can silently lose precision (1.10 reads as 1.1). Quote it as \"%s\".",
          version, version
        )
      )
    }
    version_ok <- tryCatch(
      {
        numeric_version(version, strict = TRUE)
        TRUE
      },
      error = function(e) FALSE
    )
    if (!version_ok) {
      add(
        kind, id, version, "warning", "version_unparseable",
        sprintf("version '%s' does not parse as a numeric_version().", version)
      )
    }
  }

  if (!kind_known) {
    return(.dta_template_bind_rows(rows))
  }

  kind_rows <- if (identical(kind, "dta_creation_template")) {
    .dta_template_check_creation(path, id, version, def, index_df, resolve_extends)
  } else if (identical(kind, "dta_dataset_template")) {
    .dta_template_check_dataset_tpl(path, id, version, def, index_df)
  } else if (identical(kind, "dta_vocabulary")) {
    .dta_template_check_vocabulary(path, id, version, index_df)
  } else {
    list()
  }

  .dta_template_bind_rows(c(rows, kind_rows))
}

# Every structural + dry-run-build check specific to a creation template.
.dta_template_check_creation <- function(path, id, version, def, index_df, resolve_extends) {
  rows <- list()
  add <- function(severity, code, message) {
    rows[[length(rows) + 1L]] <<- .dta_template_row(path, "dta_creation_template", id, version, severity, code, message)
  }

  allowed_fields <- .dta_template_engine_get("dta_template_metadata_fields")()
  machine_fields <- .dta_template_engine_get("dta_metadata_machine_fields")()

  # ---- target_invalid / target_machine_owned ------------------------------
  opts <- def[["options"]] %||% list()
  if (is.list(opts)) {
    for (opt in opts) {
      for (p in .dta_template_option_targets(opt)) {
        cls <- .dta_template_classify_metadata_target(p, allowed_fields, machine_fields)
        if (identical(cls, "machine")) {
          add(
            "error", "target_machine_owned",
            sprintf("target '%s' names a machine-owned metadata field; a template may never set it.", p)
          )
        } else if (identical(cls, "invalid")) {
          add("warning", "target_invalid", sprintf("target '%s' does not resolve to a known metadata field.", p))
        }
      }
    }
  }

  # ---- party_slot_invalid --------------------------------------------------
  norm_slots <- tryCatch(
    .dta_template_engine_get("normalise_party_slots")(def[["party_slots"]] %||% list()),
    error = function(e) e
  )
  if (inherits(norm_slots, "error")) {
    add("warning", "party_slot_invalid", conditionMessage(norm_slots))
  } else {
    local_profile_ids <- index_df$id[index_df$kind == "dta_party_profile"]
    for (slot in norm_slots) {
      unknown <- setdiff(slot[["profiles"]], local_profile_ids)
      if (length(unknown) > 0) {
        add(
          "warning", "party_slot_invalid",
          sprintf("party slot '%s' names unknown profile id(s): %s.", slot[["id"]], paste(unknown, collapse = ", "))
        )
      }
    }
  }

  # ---- vocab_slot_invalid / vocab_slot_unresolved -------------------------
  rows <- c(rows, .dta_template_check_vocab_slots(path, id, version, def, index_df))

  # ---- dataset_template_unresolved / patch_incoherent ---------------------
  ds_entries <- def[["datasets"]] %||% def[["base"]][["datasets"]] %||% list()
  if (is.list(ds_entries)) {
    for (entry in ds_entries) {
      if (!is.list(entry)) next
      tpl_ref <- as.character(entry[["template"]] %||% "")
      if (!nzchar(tpl_ref)) {
        # An INLINE dataset body, authored in the creation template itself. Its
        # columns can carry a vocabulary binding exactly as a dataset
        # template's can, and nothing else in this function would look at them.
        rows <- c(rows, .dta_template_check_bindings(
          path, id, version, entry, index_df,
          kind = "dta_creation_template"
        ))
        next
      }

      hit <- .dta_template_engine_get("resolve_template_ref")(index_df, tpl_ref, kind = "dta_dataset_template")
      if (is.null(hit) || nrow(hit) == 0) {
        add(
          "warning", "dataset_template_unresolved",
          sprintf("datasets[].template '%s' does not resolve to a dataset template in this directory.", tpl_ref)
        )
        next
      }

      if (!is.null(entry[["patch"]])) {
        ds_read <- .dta_template_engine_get("read_dataset_template")(hit$path[[1]])
        if (isTRUE(ds_read$ok)) {
          patch_res <- tryCatch(
            .dta_template_engine_get("apply_dataset_patch")(ds_read$value[["dataset"]], entry[["patch"]]),
            error = function(e) e
          )
          if (inherits(patch_res, "error")) {
            add("warning", "patch_incoherent", conditionMessage(patch_res))
          } else {
            # The PATCHED body, not the template's own: a patch can add a
            # column with a binding, or re-bind an existing one, and the
            # post-patch column list is the only place both are visible.
            rows <- c(rows, .dta_template_check_bindings(
              path, id, version, patch_res$dataset, index_df,
              kind = "dta_creation_template"
            ))
          }
        }
      }
    }
  }

  # ---- extends resolution + instantiate dry-run ---------------------------
  #
  # These two share one pass, because a chain that cannot resolve has no
  # merged definition to build from at all -- both the specific diagnostic
  # (extends_unresolved/extends_cycle, reported at "warning" -- resolution is
  # scoped to just this directory, and a private repo legitimately extending a
  # base template shipped elsewhere, e.g. in this package's own
  # inst/extdata/templates, would otherwise show a false positive here every
  # single run) and the blunt, always-authoritative instantiate_failed
  # (reported at "error", since a template that cannot even resolve its own
  # base can never be built, regardless of what lives outside this directory)
  # are raised together.
  abstract <- isTRUE(def[["abstract"]])
  read_res <- .dta_template_engine_get("read_dta_creation_template")(path)
  if (!isTRUE(read_res$ok)) {
    if (!abstract) {
      add("error", "instantiate_failed", read_res$error)
    }
  } else {
    resolved <- tryCatch(
      .dta_template_engine_get("resolve_template_inheritance")(read_res$value, resolve_extends),
      error = function(e) e
    )
    if (inherits(resolved, "error")) {
      msg <- conditionMessage(resolved)
      code <- if (grepl("cycle detected", msg, fixed = TRUE) || grepl("exceeds the depth limit", msg, fixed = TRUE)) {
        "extends_cycle"
      } else {
        "extends_unresolved"
      }
      add("warning", code, msg)
      if (!abstract) {
        add("error", "instantiate_failed", msg)
      }
    } else if (!abstract) {
      # `index = index_df`: without it, create_dta_from_template() takes its
      # ORIGINAL dataset-building path, under which a `datasets[].template`
      # entry is simply unreachable (template_core.R's own docstring) and
      # would always fail here regardless of whether the entry -- and any
      # `patch:` on it -- is actually coherent. Passing the local index routes
      # dataset building through build_template_datasets() (template_create.R)
      # instead, so this dry-run exercises the SAME code path a real
      # instantiation would, and patch_incoherent/dataset_template_unresolved
      # stay genuinely distinct signals from a build failure, not a
      # foregone conclusion for every template using this feature.
      build_res <- .dta_template_engine_get("create_dta_from_template")(
        resolved$def, path,
        index = index_df,
        # Slots are read off the RESOLVED definition, so a slot a child
        # inherited through `extends:` is driven here too.
        vocab_selections = .dta_template_vocab_dry_selections(resolved$def, index_df)
      )
      if (!isTRUE(build_res$ok)) {
        add("error", "instantiate_failed", build_res$error)
      }
    }
  }

  rows
}

# Structural checks for a creation template's `vocabulary_slots:`.
.dta_template_check_vocab_slots <- function(path, id, version, def, index_df) {
  rows <- list()
  add <- function(severity, code, message) {
    rows[[length(rows) + 1L]] <<- .dta_template_row(path, "dta_creation_template", id, version, severity, code, message)
  }

  raw <- def[["vocabulary_slots"]] %||% list()
  if (!is.list(raw) || length(raw) == 0) {
    return(rows)
  }

  slots <- tryCatch(
    .dta_template_engine_get("normalise_vocabulary_slots")(raw),
    error = function(e) e
  )
  if (inherits(slots, "condition")) {
    add("error", "vocab_slot_invalid", conditionMessage(slots))
    return(rows)
  }

  resolver <- .dta_template_vocab_resolved(index_df)
  for (slot in slots) {
    vocab <- tryCatch(resolver(slot[["vocabulary"]]), error = function(e) e)
    if (inherits(vocab, "condition")) {
      add("error", "vocab_slot_invalid", conditionMessage(vocab))
      next
    }
    if (is.null(vocab)) {
      add(
        "warning", "vocab_slot_unresolved",
        sprintf(
          "vocabulary slot '%s' names vocabulary '%s', which does not resolve within the directory being validated.",
          slot[["id"]], slot[["vocabulary"]]
        )
      )
      next
    }

    terms <- tryCatch(
      .dta_template_engine_get("vocabulary_terms")(vocab, include = slot[["include"]], exclude = slot[["exclude"]]),
      error = function(e) e
    )
    if (inherits(terms, "condition")) {
      add("error", "vocab_slot_invalid", conditionMessage(terms))
      next
    }

    # A default the slot does not itself offer is dead on arrival: the picker
    # cannot show it, so the slot silently starts empty.
    codes <- vapply(terms, function(t) as.character(t[["code"]]), character(1))
    stale <- setdiff(slot[["default"]], codes)
    if (length(stale) > 0) {
      add(
        "error", "vocab_slot_invalid",
        sprintf(
          "vocabulary slot '%s' defaults to %s, which this slot does not offer.",
          slot[["id"]], paste(sprintf("'%s'", stale), collapse = ", ")
        )
      )
    }
  }

  rows
}

# Selections to drive the instantiate dry-run with.
#
# Every slot's own `default:`, topped up to its `min:` from the head of the
# offered terms when the default does not reach it. WITHOUT this, a slot that
# legitimately requires a choice (`min: 1`, no default -- "the author must
# pick") would abort the dry run and be reported as instantiate_failed, which
# would be wrong: the template is fine, it simply has not been filled in. The
# dry run stands in for a user who picked the minimum, not for one who
# submitted an empty form.
.dta_template_vocab_dry_selections <- function(def, index_df) {
  raw <- def[["vocabulary_slots"]] %||% list()
  if (!is.list(raw) || length(raw) == 0) {
    return(list())
  }
  slots <- tryCatch(
    .dta_template_engine_get("normalise_vocabulary_slots")(raw),
    error = function(e) NULL
  )
  if (is.null(slots)) {
    return(list())
  }

  resolver <- .dta_template_vocab_resolved(index_df)
  out <- list()
  for (slot in slots) {
    sel <- slot[["default"]]
    if (length(sel) < slot[["min"]]) {
      terms <- tryCatch(
        .dta_template_engine_get("vocabulary_terms")(
          resolver(slot[["vocabulary"]]),
          include = slot[["include"]], exclude = slot[["exclude"]]
        ),
        error = function(e) NULL
      )
      if (!is.null(terms)) {
        codes <- vapply(terms, function(t) as.character(t[["code"]]), character(1))
        sel <- unique(c(sel, utils::head(codes, slot[["min"]])))
      }
    }
    if (length(sel) > 0) {
      out[[slot[["id"]]]] <- sel
    }
  }
  out
}

# Every `values_from:` binding on a dataset body's columns, checked against the
# vocabularies available in the directory being validated.
#
# Split out from .dta_template_check_dataset_tpl() so a creation template's
# inline `datasets:` bodies can be run through the identical checks -- a
# binding is a binding wherever the column was authored.
.dta_template_check_bindings <- function(path, id, version, ds_body, index_df, kind = "dta_dataset_template") {
  rows <- list()
  add <- function(severity, code, message) {
    rows[[length(rows) + 1L]] <<- .dta_template_row(path, kind, id, version, severity, code, message)
  }

  columns <- ds_body[["columns"]] %||% list()
  if (!is.list(columns) || length(columns) == 0) {
    return(rows)
  }

  resolver <- NULL
  for (col in columns) {
    if (!is.list(col) || is.null(col[["values_from"]])) {
      next
    }
    col_id <- as.character(col[["id"]] %||% "<unnamed>")

    # Authored BOTH in the same literal column. Only detectable here, where the
    # raw per-file YAML is in hand: by build time a `values:` from the base and
    # a `values_from:` from a patch look exactly the same, and that combination
    # is legitimate. A warning, not an error, because the build is still
    # deterministic (the binding wins).
    #
    # `col[["values"]]`, NOT `col$values`: `$` on a list falls back to PARTIAL
    # matching, so `col$values` returns the `values_from` this branch has
    # already established is present whenever no literal `values:` was
    # authored. That made this warning fire for every column that used a
    # vocabulary binding correctly -- six times on this package's own shipped
    # templates, telling the author they had written something they had not.
    # `[[` matches exactly and is the only safe accessor for a name that is a
    # prefix of a sibling.
    if (!is.null(col[["values"]])) {
      add(
        "warning", "values_and_values_from",
        sprintf("column '%s' sets both 'values' and 'values_from'; the binding wins and 'values' is ignored.", col_id)
      )
    }

    if (!is.null(col[["pattern"]])) {
      add(
        "error", "values_from_pattern",
        sprintf("column '%s' combines 'values_from' with 'pattern'; a column takes a permitted-value list or a pattern, never both.", col_id)
      )
      next
    }

    binding <- tryCatch(
      .dta_template_engine_get("normalise_values_from")(col[["values_from"]], col_id),
      error = function(e) e
    )
    if (inherits(binding, "condition")) {
      add("error", "values_from_invalid", conditionMessage(binding))
      next
    }

    if (is.null(resolver)) {
      resolver <- .dta_template_vocab_resolved(index_df)
    }
    vocab <- tryCatch(resolver(binding[["vocabulary"]]), error = function(e) e)
    if (inherits(vocab, "condition")) {
      add("error", "values_from_invalid", conditionMessage(vocab))
      next
    }
    if (is.null(vocab)) {
      add(
        "warning", "values_from_unresolved",
        sprintf(
          "column '%s' binds to vocabulary '%s', which does not resolve within the directory being validated.",
          col_id, binding[["vocabulary"]]
        )
      )
      next
    }

    # include/exclude naming a code the vocabulary does not have is the exact
    # shape a typo takes, and its consequence -- a column whose permitted
    # values quietly omit a term -- only shows up much later, against real
    # data. vocabulary_terms() already aborts on it; surface that as a row.
    err <- tryCatch(
      {
        .dta_template_engine_get("vocabulary_terms")(
          vocab,
          include = binding[["include"]], exclude = binding[["exclude"]]
        )
        NULL
      },
      error = function(e) e
    )
    if (!is.null(err)) {
      add("error", "values_from_terms_invalid", conditionMessage(err))
    }
  }

  rows
}

# Structural checks specific to a dataset template: only its `dataset.*`
# option targets are in scope -- there is no `extends:`/instantiate-dry-run
# equivalent for a lone dataset template (see build_dataset_from_template()'s
# banner in dataset_template.R: it always needs a caller-chosen `as_name`/
# patch context that only exists once it is actually used from a creation
# template, so there is nothing standalone to dry-run build here).
.dta_template_check_dataset_tpl <- function(path, id, version, def, index_df) {
  rows <- list()
  add <- function(severity, code, message) {
    rows[[length(rows) + 1L]] <<- .dta_template_row(path, "dta_dataset_template", id, version, severity, code, message)
  }

  # `def[["dataset"]]`, NOT `def$dataset`: 'dataset' is a strict PREFIX of
  # 'datasets', a DIFFERENT, equally legitimate top-level key -- a
  # dta_dataset_template carries 'dataset:' (singular), a dta_creation_template
  # carries 'datasets:' (plural). `$` on a list falls back to PARTIAL name
  # matching, so a dta_dataset_template file that was slipped a 'datasets:'
  # array instead -- the obvious mistake when converting one kind of template
  # into the other -- had `def$dataset` silently return that array, and every
  # check below then walked it as if it were the dataset body: zero issues
  # reported for a file that has no usable dataset at all.
  ds_body <- def[["dataset"]] %||% list()
  if (length(ds_body) == 0) {
    # "error", not "warning": with no 'dataset:' body there is nothing this
    # template could ever produce -- the same "cannot produce anything"
    # reasoning as vocabulary_invalid/instantiate_failed/no_templates. Further
    # checks below (bindings, option targets) would either no-op silently
    # against an empty body or pile confusing target_invalid rows on top of
    # this same root cause, so this is the only row reported.
    hint <- if (!is.null(def[["datasets"]])) {
      " Found 'datasets:' (plural) instead -- that belongs to a dta_creation_template; a dta_dataset_template needs the singular 'dataset:'."
    } else {
      ""
    }
    add("error", "dataset_missing", paste0("Template is missing a 'dataset' body.", hint))
    return(rows)
  }
  rows <- c(rows, .dta_template_check_bindings(path, id, version, ds_body, index_df))
  opts <- def[["options"]] %||% list()
  if (is.list(opts)) {
    for (opt in opts) {
      for (p in .dta_template_option_targets(opt)) {
        root <- strsplit(p, "\\.")[[1]][[1]]
        if (!identical(root, "dataset")) {
          add("warning", "target_invalid", sprintf("target '%s' is not rooted at 'dataset.'.", p))
          next
        }
        # Delegate resolution FULLY to the engine's own grammar (id-addressed
        # columns/rules, the single-vs-multiple `files:` ambiguity) rather
        # than re-deriving it: apply_dataset_template_path() (dataset_
        # template.R) already throws a specific, well-worded error for every
        # way a path can fail to resolve. `NULL` as the value exercises the
        # resolution/lookup itself without mattering for a check that
        # discards the result either way.
        err <- tryCatch(
          {
            .dta_template_engine_get("apply_dataset_template_path")(ds_body, p, NULL)
            NULL
          },
          error = function(e) e
        )
        if (!is.null(err)) {
          add("warning", "target_invalid", conditionMessage(err))
        }
      }
    }
  }

  rows
}

# Structural checks specific to a controlled vocabulary.
#
# The file is RE-READ through the engine's own read_vocabulary() rather than
# checked against the raw `def` the record reader already parsed. That is not
# redundant: read_vocabulary() applies the wider scalar handlers that keep a
# zero-padded or Y/N code intact (see dta_vocabulary_yaml_handlers() in
# vocabulary.R), so checking the plainly-parsed `def` would be checking a
# different, already-mangled document than the one the app will actually use.
.dta_template_check_vocabulary <- function(path, id, version, index_df) {
  rows <- list()
  add <- function(severity, code, message) {
    rows[[length(rows) + 1L]] <<- .dta_template_row(path, "dta_vocabulary", id, version, severity, code, message)
  }

  read <- .dta_template_engine_get("read_vocabulary")(path)
  if (!isTRUE(read$ok)) {
    # "error", not "warning": every other kind can still be USED with a
    # questionable field, but a vocabulary that will not read contributes no
    # terms at all, so any column bound to it produces a spec with no
    # permitted values -- a silently unvalidated column.
    add("error", "vocabulary_invalid", read$error)
    return(rows)
  }

  vocab <- read$value
  ref <- as.character(vocab[["extends"]] %||% "")
  if (!nzchar(ref)) {
    return(rows)
  }

  resolver <- .dta_template_vocab_resolver(index_df)
  if (is.null(resolver(ref))) {
    add(
      "warning", "vocabulary_unresolved",
      sprintf("extends '%s', which does not resolve within the directory being validated.", ref)
    )
    return(rows)
  }

  # Resolution itself can still abort -- a cycle, or a chain past the depth
  # limit -- and those are errors, not warnings: neither produces a usable
  # term list.
  err <- tryCatch(
    {
      .dta_template_engine_get("resolve_vocabulary_inheritance")(vocab, resolver)
      NULL
    },
    error = function(e) e
  )
  if (!is.null(err)) {
    add("error", "vocabulary_extends_failed", conditionMessage(err))
  }

  rows
}

# ---- Cross-file: duplicate kind + id + version ---------------------------

.dta_template_check_duplicates <- function(records) {
  rows <- list()
  paths <- names(records)
  keys <- vapply(paths, function(p) {
    r <- records[[p]]
    if (!isTRUE(r$ok) || is.na(r$kind) || is.na(r$id) || is.na(r$version)) {
      return(NA_character_)
    }
    # Newline-joined, not a visible separator: id/version come straight from a
    # template author's YAML and could contain punctuation such as "@" that a
    # visible join character could turn into an accidental collision. Same
    # reasoning as build_template_index()'s own `seen` key (template_index.R).
    paste(r$kind, r$id, r$version, sep = "\n")
  }, character(1))

  seen <- new.env(parent = emptyenv())
  for (p in paths) {
    k <- keys[[p]]
    if (is.na(k)) next
    prior <- seen[[k]]
    if (is.null(prior)) {
      seen[[k]] <- p
      next
    }
    r <- records[[p]]
    rows[[length(rows) + 1L]] <- .dta_template_row(
      p, r$kind, r$id, r$version, "warning", "duplicate_id_version",
      sprintf("Same kind + id + version as %s.", prior)
    )
  }
  .dta_template_bind_rows(rows)
}

# ---- Cross-file: directory scanned has no template files at all ----------

# Fires only when `path` is a DIRECTORY: a single explicitly named file is
# never "empty" by definition. Checked against ALL_FILES (every kind), not
# the `kinds`-filtered report_files -- a directory holding only vocabularies,
# validated with kinds = "dta_creation_template", has nothing to REPORT, not
# nothing IN IT, and must not trip this.
#
# Severity "error", not "warning": inst/extdata/templates/validate-
# templates.yml's whole CI job is `validate_template(".", strict = TRUE)`,
# and strict = TRUE only aborts on an error row. Point that job at the wrong
# path -- a typo, a stale working directory, a checkout that put the
# templates one level down -- and, without this check, the job goes green
# having validated nothing: exactly the failure a repository linter exists
# to catch.
.dta_template_check_no_templates <- function(path, is_dir, all_files) {
  if (!is_dir || length(all_files) > 0) {
    return(.dta_template_validation_empty())
  }
  .dta_template_bind_rows(list(.dta_template_row(
    path, NA_character_, NA_character_, NA_character_, "error", "no_templates",
    sprintf(
      "No template files found in %s: looked for *.dta-template.yaml, *.dta-dataset-template.yaml, *.dta-party.yaml and *.dta-vocabulary.yaml (and their .yml spellings), non-recursively. A typo'd path, a stale working directory, and templates kept one level down in a subdirectory all look exactly like this.",
      path
    )
  )))
}

# ---- Cross-file: a template kept below the scanned directory's top level -

# The non-recursive scan (.dta_template_list_dir(), mirroring the app's own
# build_template_index() in template_index.R:215-219) means a template
# organised into a subfolder -- "by-study/", an archive -- is invisible to
# BOTH this validator and the Shiny app's picker. That is consistent, not a
# bug in either place, but nothing currently says so; this turns the silence
# into one row per offending file, so an author notices before wondering why
# a template never shows up in the app.
#
# Warning, not error: keeping an archived or work-in-progress file below the
# root is a legitimate choice, and this must not turn CI red on its own.
.dta_template_check_nested <- function(path, is_dir, all_files, all_kinds) {
  if (!is_dir) {
    return(.dta_template_validation_empty())
  }

  # Diffed against ALL_FILES, matching .dta_template_check_no_templates()'s
  # own reasoning: whether a file is missed by the flat scan does not depend
  # on which kinds the caller asked for a report row on.
  nested <- .dta_template_list_nested(path, all_kinds)
  nested <- nested[!(nested$file %in% all_files), , drop = FALSE]
  if (nrow(nested) == 0) {
    return(.dta_template_validation_empty())
  }
  nested <- nested[order(nested$file, method = "radix"), , drop = FALSE]

  rows <- lapply(seq_len(nrow(nested)), function(i) {
    .dta_template_row(
      nested$file[[i]], nested$kind[[i]], NA_character_, NA_character_,
      "warning", "template_in_subdirectory",
      sprintf(
        "%s is not validated (the scan is non-recursive) and the Shiny app's template loader will not see it either -- move it to the top level of the template directory.",
        nested$file[[i]]
      )
    )
  })
  .dta_template_bind_rows(rows)
}

# ---- strict = TRUE --------------------------------------------------------

.dta_template_strict_check <- function(result) {
  errs <- result[result$severity == "error", , drop = FALSE]
  n <- nrow(errs)
  if (n == 0) {
    return(invisible(NULL))
  }

  preview <- utils::head(errs, 5)
  lines <- sprintf("%s [%s]: %s", preview$file, preview$code, preview$message)
  n_more <- n - nrow(preview)

  body <- stats::setNames(lines, rep("x", length(lines)))
  if (n_more > 0) {
    body <- c(body, "i" = "... and {n_more} more error{?s}.")
  }

  cli::cli_abort(c("Template validation found {n} error{?s}.", body))
}

# ---- Public API -------------------------------------------------------

#' Validate DTA templates against the template engine's own rules
#'
#' @description
#' Runs the structural checks the bundled Shiny app's "Create new from
#' template" picker relies on -- unresolvable `extends:`, an option target
#' that does not resolve to a real metadata field, a party slot naming a
#' profile that does not exist, a dataset patch naming an absent column, a
#' `dta_dataset_template` with no `dataset:` body (code `"dataset_missing"`,
#' also caught when a `datasets:` array was authored instead), a scanned
#' directory with no template files in it at all, a template kept below the
#' scanned directory's top level, and (as a final, comprehensive check)
#' actually building a DTA from every non-abstract creation template with its
#' own default selections -- WITHOUT starting the app. This lets a
#' template repository (kept private, on Bitbucket Data Center in production
#' and mirrored to GitHub for CI) run its own continuous integration against
#' the templates it authors, with nothing more than an R installation of
#' DTAtools. See `system.file("extdata", "templates", "validate-templates.yml",
#' package = "DTAtools")` for a ready-to-copy GitHub Actions workflow.
#'
#' @details
#' An otherwise-empty directory is reported as an `"error"` (code
#' `"no_templates"`), not accepted silently: the packaged
#' `validate-templates.yml` workflow's entire CI job is
#' `validate_template(".", strict = TRUE)`, and `strict = TRUE` only aborts on
#' an error row. Point that job at the wrong path -- a typo, a stale working
#' directory, a checkout that put the templates one level down -- and, without
#' this check, the job goes green having validated nothing: exactly the
#' failure a repository linter exists to catch.
#'
#' @param path A single template file, or a directory. A directory is scanned
#'   NON-RECURSIVELY for `*.dta-template.yaml`, `*.dta-dataset-template.yaml`,
#'   `*.dta-party.yaml` and `*.dta-vocabulary.yaml` files (and their `.yml`
#'   spellings) -- the same
#'   filename convention the template engine itself uses
#'   (`dta_template_kind_pattern()`). `kinds`, below, only narrows which of
#'   those files get a report row of their OWN; it never changes how many
#'   files are considered when resolving a cross-file reference (an
#'   `extends:`, a `datasets[].template`, a party slot's profile allow-list).
#'   A template found below the scanned directory's top level is not read at
#'   all -- it is reported as `template_in_subdirectory` rather than silently
#'   ignored.
#' @param strict A single `TRUE`/`FALSE`. When `TRUE`, `validate_template()`
#'   raises an error via [cli::cli_abort()] summarising every row of severity
#'   `"error"` (and stays silent when there are none), instead of just
#'   returning them -- the one line a CI job needs.
#' @param kinds `NULL` (the default: every kind) or a character vector naming
#'   one or more of `"dta_creation_template"`, `"dta_dataset_template"`,
#'   `"dta_party_profile"`, `"dta_vocabulary"`. Restricts which files in a
#'   scanned directory get
#'   their own report row; has no effect when `path` is a single file (an
#'   explicitly named file is always checked).
#' @return A data frame (`stringsAsFactors = FALSE`) with one row per issue
#'   found, and columns `file`, `kind`, `id`, `version`, `severity`
#'   (`"error"` or `"warning"`), `code`, `message`. Zero rows means every
#'   checked file passed every check.
#' @export
#' @examples
#' library(DTAtools)
#' issues <- validate_template(system.file("extdata", "templates", package = "DTAtools"))
#' issues
validate_template <- function(path, strict = FALSE, kinds = NULL) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    cli::cli_abort("{.arg path} must be a single non-empty file or directory path.")
  }
  if (!is.logical(strict) || length(strict) != 1 || is.na(strict)) {
    cli::cli_abort("{.arg strict} must be a single {.code TRUE}/{.code FALSE}.")
  }

  all_kinds <- .dta_template_engine_get("dta_template_all_kinds")()
  if (!is.null(kinds)) {
    kinds <- as.character(kinds)
    unknown <- setdiff(kinds, all_kinds)
    if (length(unknown) > 0) {
      cli::cli_abort(c(
        "Unknown template kind{?s} in {.arg kinds}: {unknown}.",
        "i" = "Known kinds: {all_kinds}."
      ))
    }
  }

  is_dir <- dir.exists(path)
  is_file <- !is_dir && file.exists(path)
  if (!is_dir && !is_file) {
    cli::cli_abort("{.arg path} does not exist: {.file {path}}")
  }

  all_files <- if (is_dir) .dta_template_list_dir(path, all_kinds) else path
  report_files <- if (is_dir && !is.null(kinds)) .dta_template_list_dir(path, kinds) else all_files

  records <- stats::setNames(lapply(all_files, .dta_template_read_raw), all_files)
  index_df <- .dta_template_build_index_df(records)
  resolve_extends <- .dta_template_extends_resolver(index_df)

  file_rows <- lapply(report_files, function(p) {
    .dta_template_check_file(p, records[[p]], index_df, resolve_extends)
  })
  dup_rows <- .dta_template_check_duplicates(records[report_files])
  no_templates_rows <- .dta_template_check_no_templates(path, is_dir, all_files)
  nested_rows <- .dta_template_check_nested(path, is_dir, all_files, all_kinds)

  result <- do.call(rbind, c(
    list(.dta_template_validation_empty()), file_rows,
    list(dup_rows, no_templates_rows, nested_rows)
  ))
  if (nrow(result) > 0) {
    # Deterministic order across locales/OSes -- see the "locale collation
    # diverges from CI" lesson referenced in .dta_template_list_dir().
    result <- result[order(result$file, result$code, method = "radix"), , drop = FALSE]
  }
  rownames(result) <- NULL

  if (isTRUE(strict)) {
    .dta_template_strict_check(result)
  }

  result
}
