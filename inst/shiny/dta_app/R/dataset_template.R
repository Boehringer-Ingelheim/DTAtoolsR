# -----------------------------------------------------------------------------
# Dataset templates: "add a dataset from a reusable template" on top of the
# per-DTA creation templates in template_core.R.
#
# A creation template (template_core.R) builds a whole NEW DTA. A dataset
# template builds ONE dataset -- so it can be dropped into an existing DTA
# (add a second dataset to a study that already has one), not only used at
# creation time. The two files therefore share machinery (dta_try(),
# list_get_path()/list_set_path(), resolve_template_expressions(),
# collect_option_effects(), dta_template_default()) but keep separate entry
# points, because "which DTA object does this write into" differs: a creation
# template calls apply_template_metadata_path() against a whole DTA; a dataset
# template works entirely on a PLAIN LIST dataset spec, the same shape
# read_dataset_from_yaml()/dta_dataset_from_list() accept, right up until it is
# handed to the S7 layer.
#
# File kind (kind: dta_dataset_template):
#
#   kind: dta_dataset_template
#   id: gf_smrnaseq
#   version: "3.0"
#   label: GF domain smrnaseq
#   description: Genomic Findings data table
#   date: 2024-12-17
#   options:
#     - id: vendor_name
#       label: Vendor name
#       type: text
#       target: dataset.columns.GFNAM.values
#   dataset:
#     name: gf_data_specs_pattern
#     type: tabular
#     files: { filename: gf.tsv, type: tsv }
#     columns:
#       - { id: STUDYID, label: Study Identifier, type: SAS Char }
#       - { id: GFNAM, label: Vendor Name, type: SAS Char }
#     rules: []
# -----------------------------------------------------------------------------

# Message for a missing 'dataset:' section, with a hint when the file
# instead carries 'datasets:' (plural) -- the obvious slip when converting a
# creation template into a dataset template, or the reverse. Shared by the two
# guards below (read_dataset_template(), build_dataset_from_template()) so the
# wording -- and the hint condition -- cannot drift between them.
.dataset_template_missing_dataset_msg <- function(def) {
  hint <- if (!is.null(def[["datasets"]])) {
    " Found 'datasets:' (plural) instead -- that belongs to a dta_creation_template; a dta_dataset_template needs the singular 'dataset:'."
  } else {
    ""
  }
  paste0("Dataset template must contain a 'dataset' section.", hint)
}

# Read and minimally validate a dataset-template YAML.
#
# Kept deliberately close to read_dta_creation_template()'s shape (same
# dta_try() wrapping, same "normalise the optional pieces" tail) so the two
# template kinds read the same way to app code that only cares that it got
# back list(ok=, value=, error=).
read_dataset_template <- function(path) {
  dta_try({
    if (is.null(path) || !nzchar(path) || !file.exists(path)) {
      stop("Dataset template file not found.")
    }
    def <- yaml::read_yaml(path)
    if (!is.list(def)) stop("Dataset template YAML must be a mapping/object.")

    kind <- as.character(def$kind %||% "")
    if (!identical(kind, "dta_dataset_template")) {
      stop("Dataset template 'kind' must be 'dta_dataset_template'.")
    }

    id <- as.character(def$id %||% "")
    if (!nzchar(id)) {
      stop("Dataset template must define a non-empty 'id'.")
    }

    # version is required and is ALWAYS normalised to character below, but the
    # presence check has to happen first against the raw value: is.null()/
    # nzchar() on a not-yet-coerced numeric like 3.0 both do the right thing,
    # whereas testing nzchar(as.character(NULL)) would not (character(0)).
    if (is.null(def$version) || length(def$version) == 0 || !nzchar(as.character(def$version))) {
      stop("Dataset template must define a non-empty 'version'.")
    }

    # `def[["dataset"]]`, NOT `def$dataset` -- see the `$` vs `[[` rule in
    # utils_dta.R. 'dataset' is a strict PREFIX of 'datasets' (a creation
    # template's own top-level key), so a dta_dataset_template file that was
    # slipped a 'datasets:' array instead -- the obvious mistake when
    # converting one kind of template into the other -- used to have
    # `def$dataset` silently return that array. `is.list()` on it was still
    # TRUE (it's a list of dataset entries), so this guard passed, and the
    # only visible symptom was the unrelated-looking "'dataset' section must
    # contain a 'name'" error three lines below.
    if (!is.list(def[["dataset"]])) {
      stop(.dataset_template_missing_dataset_msg(def))
    }
    if (is.null(def[["dataset"]][["name"]]) || !nzchar(as.character(def[["dataset"]][["name"]]))) {
      stop("Dataset template's 'dataset' section must contain a 'name'.")
    }

    def$id <- id
    # WHY the version needs care: it is stamped straight onto a plain-list
    # dataset spec as `template_version`, which do.call(DTADataSetFactory, x)
    # forwards to DTADataSet@template_version -- a class_character_or_null S7
    # property. A raw double trips that validator the moment the dataset is
    # actually constructed, far away from this read function.
    #
    # Read it from the file TEXT rather than coercing what a plain parse
    # produced: as.character(3.0) is "3", and an unquoted `version: 1.10` is
    # already the double 1.1 before any R code sees it, which no coercion can
    # undo. Only the `dataset:` body below needs its real numeric types, so the
    # header field is re-read on its own -- the shared reader in template_core.R
    # is the single place that knows which libyaml tags preserve source text.
    exact <- dta_template_read_field_exact(path, "version")
    def$version <- if (!is.na(exact) && nzchar(exact)) {
      exact
    } else {
      dta_template_version_string(def$version, what = id)
    }
    def$label <- as.character(def$label %||% def$id)
    def$description <- as.character(def$description %||% "")
    def$options <- def$options %||% list()

    # `date` is the same story as `version`: yaml::read_yaml() parses an
    # unquoted 2024-12-17 as a Date, but the dataset's own template_date
    # property is a plain character, and formatting HERE means every later
    # consumer (build_dataset_from_template(), a UI label) can treat
    # def$date as "character or NULL" without re-deriving this branch.
    if (!is.null(def$date)) {
      def$date <- if (inherits(def$date, "Date")) format(def$date) else as.character(def$date)
    }

    def
  })
}

# Split a `dataset.`-rooted dotted path into its tail parts, validating the
# root along the way. Pulled out of apply_dataset_template_path() as its own
# function because the "wrong root" failure mode (a stray metadata.* path
# reused by copy-paste from a creation template) deserves one clear message,
# not a generic "field not found" three calls deep.
dataset_template_path_parts <- function(path) {
  parts <- strsplit(as.character(path %||% ""), "\\.")[[1]]
  if (length(parts) < 2 || !identical(parts[[1]], "dataset")) {
    cli::cli_abort("Dataset template path must be rooted at 'dataset.', got '{path}'.")
  }
  parts[-1]
}

# Is a dataset's `files:` block a single file handler (a named list, the
# shape a YAML mapping parses to) rather than several (an unnamed list of
# lists, the shape a YAML sequence parses to)?
#
# Mirrors the exact same fully-named/fully-unnamed test dta_file_handlers_from_
# list() (R/00_helpers.R) uses to tell the two apart -- length is deliberately
# NOT the deciding factor, because `files: [{...}]` is a sequence of one and is
# just as ambiguous under `dataset.files.<key>` as a sequence of two: a bare
# key still does not say which entry it belongs to.
dataset_files_is_single_map <- function(files) {
  if (!is.list(files) || length(files) == 0) {
    return(TRUE) # nothing to disambiguate; treat an absent/empty block as a map
  }
  nms <- names(files)
  !is.null(nms) && all(nzchar(nms))
}

# Locate an item (a column or a rule) by its `id` field within a list of
# specs, aborting with the id and the full known-id list when it is missing.
#
# Addressing by id rather than position is the entire point of this file's
# path model: a positional reference breaks the moment a patch inserts or
# removes an entry ahead of it, and would then silently edit the wrong column
# instead of failing loudly.
dataset_item_index_by_id <- function(items, id, kind, ds_name) {
  items <- items %||% list()
  ids <- vapply(items, function(it) as.character(it$id %||% ""), character(1))
  idx <- which(ids == id)
  if (length(idx) == 0) {
    cli::cli_abort(
      "Unknown {kind} id '{id}' in dataset '{ds_name}'. Known ids: {paste(ids, collapse = ', ')}."
    )
  }
  idx[[1]]
}

# Apply one dotted-path update to a PLAIN LIST dataset spec, returning the
# (also plain list) result. `ds` is never an S7 object here: this operates on
# the same list shape read_dataset_from_yaml() reads and dta_dataset_from_
# list() eventually converts, so a template's edits compose with a patch's
# edits using nothing but list operations, right up until the final
# do.call(DTADataSetFactory, ds) at the very end of the pipeline.
#
# Supported paths:
#   dataset.<field>                     -- a top-level scalar field
#   dataset.files.<key>                 -- only when `files` is a single map
#   dataset.columns.<id>.<field>        -- column located by id, not position
#   dataset.rules.<id>.<field>          -- rule located by id, not position
apply_dataset_template_path <- function(ds, path, value) {
  parts <- dataset_template_path_parts(path)
  ds_name <- as.character(ds$name %||% "?")

  # A single tail part is a bare top-level field -- `dataset.description`, or
  # any other key a template author adds. There is no allow-list here (unlike
  # DTAMetaData's fixed S7 property set in apply_template_metadata_path()):
  # a dataset spec is a plain list forwarded whole via do.call(
  # DTADataSetFactory, x), so an unrecognised key is simply an argument the
  # factory itself will reject -- nothing here needs to re-validate that.
  if (length(parts) == 1) {
    return(list_set_path(ds, parts, value))
  }

  root <- parts[[1]]

  if (identical(root, "files")) {
    if (!dataset_files_is_single_map(ds$files)) {
      cli::cli_abort(
        "Path '{path}' is ambiguous: dataset '{ds_name}' has multiple 'files' entries, so a bare key does not identify which one to update."
      )
    }
    ds$files <- list_set_path(ds$files %||% list(), parts[-1], value)
    return(ds)
  }

  if (identical(root, "columns")) {
    if (length(parts) < 3) {
      cli::cli_abort("Column path must be 'dataset.columns.<id>.<field>', got '{path}'.")
    }
    idx <- dataset_item_index_by_id(ds$columns, parts[[2]], "column", ds_name)
    ds$columns[[idx]] <- list_set_path(ds$columns[[idx]], parts[-c(1, 2)], value)
    return(ds)
  }

  if (identical(root, "rules")) {
    if (length(parts) < 3) {
      cli::cli_abort("Rule path must be 'dataset.rules.<id>.<field>', got '{path}'.")
    }
    idx <- dataset_item_index_by_id(ds$rules, parts[[2]], "rule", ds_name)
    ds$rules[[idx]] <- list_set_path(ds$rules[[idx]], parts[-c(1, 2)], value)
    return(ds)
  }

  # Any other multi-part root has no id-keyed addressing rule of its own, so a
  # plain nested set is exactly right -- list_set_path() already handles an
  # arbitrarily deep key chain.
  list_set_path(ds, parts, value)
}

# Apply a patch (add/remove/modify columns, plus a general `set:` map) to a
# plain-list dataset spec. Returns list(dataset = <plain list>, deviations =
# <list of list(op=, target=)>) so the caller (and eventually a UI "what did
# this patch actually change" summary) has a record of every edit, not just
# the final state.
#
# The FOUR ops run in this fixed order, and the order is load-bearing, not
# incidental:
#
#   1. remove_columns -- shrink first. A column a later step (modify_columns,
#      set) is about to address must already be gone if it was marked for
#      removal, rather than momentarily still present and then fought over.
#   2. add_columns    -- grow second. New columns join the list before
#      anything below tries to address them by id, and before modify_columns
#      could otherwise mistake "doesn't exist yet" for "absent id, abort".
#   3. modify_columns -- adjust existing THIRD, once the column list has its
#      final MEMBERSHIP (the right ids present, the wrong ones gone). An
#      id targeted here can therefore only be a genuinely pre-existing or
#      newly-added column, never one this same patch is still about to
#      remove.
#   4. set            -- general field sets LAST, so a `set:` path against
#      `columns.<id>.<field>` always resolves against the FINAL column list,
#      never a stale id an earlier op in this same patch was still going to
#      invalidate.
#
# An absent/empty patch is a no-op: the dataset comes back unchanged (not even
# copied) and deviations is an empty list.
apply_dataset_patch <- function(ds, patch) {
  if (is.null(patch) || length(patch) == 0) {
    return(list(dataset = ds, deviations = list()))
  }

  ds_name <- as.character(ds$name %||% "?")
  deviations <- list()

  # 0) columns: the same four ops written in the SHARED collection vocabulary
  # -- inherit/remove/add/modify/order -- that `options:` and `datasets:` use,
  # so a template author learns one set of words instead of three. The
  # `remove_columns:`/`add_columns:`/`modify_columns:` spellings below are the
  # original names for three of these and keep working unchanged; this form
  # additionally expresses the two things they cannot, `inherit: [ids]` (keep
  # only a named subset) and `inherit: none` (replace the column list
  # wholesale), plus `order:`.
  #
  # It runs BEFORE the named ops for the same reason remove_columns runs first
  # among them: it settles MEMBERSHIP, and everything after addresses columns
  # by id.
  if (!is.null(patch$columns)) {
    parsed <- dta_template_parse_collection(patch$columns, "columns")
    if (!identical(parsed$form, "verbs")) {
      verbs <- paste(dta_template_collection_verbs, collapse = "/")
      cli::cli_abort(c(
        "{.field columns} in the patch for dataset {.val {ds_name}} must be a mapping of {verbs}.",
        i = "A bare list of column specs belongs in {.field add_columns} or {.field modify_columns}."
      ))
    }
    id_of <- function(x) as.character(x$id %||% "")
    before <- vapply(ds$columns %||% list(), id_of, character(1))
    ds$columns <- dta_template_apply_collection_verbs(
      ds$columns %||% list(), parsed$verbs, "columns", dta_template_option_key
    )
    after <- vapply(ds$columns %||% list(), id_of, character(1))
    # Deviations stay in the `*_columns` vocabulary the UI already reads: the
    # verbs are a second spelling of these ops, not a second set of them.
    for (id in setdiff(before, after)) {
      deviations[[length(deviations) + 1L]] <- list(op = "remove_columns", target = id)
    }
    for (entry in parsed$verbs$add) {
      deviations[[length(deviations) + 1L]] <- list(op = "add_columns", target = id_of(entry))
    }
    for (entry in parsed$verbs$modify) {
      deviations[[length(deviations) + 1L]] <- list(op = "modify_columns", target = id_of(entry))
    }
  }

  # 1) remove_columns: a character vector of ids. An id that is not present is
  # a hard abort, never a silent no-op -- a caller relying on a column being
  # gone must be told when it never existed to begin with.
  for (id in (patch$remove_columns %||% character(0))) {
    idx <- dataset_item_index_by_id(ds$columns, id, "column", ds_name)
    ds$columns[[idx]] <- NULL
    deviations[[length(deviations) + 1L]] <- list(op = "remove_columns", target = id)
  }

  # 2) add_columns: a list of column specs, each carrying its own `id`.
  # Tracked against `existing_ids` (rather than re-deriving it from ds$columns
  # on every iteration) so two entries in the SAME add_columns list that
  # collide with each other are caught too, not just a collision with a
  # column that already existed before the patch.
  existing_ids <- vapply(ds$columns %||% list(), function(x) as.character(x$id %||% ""), character(1))
  for (spec in (patch$add_columns %||% list())) {
    new_id <- as.character(spec$id %||% "")
    if (!nzchar(new_id)) {
      cli::cli_abort("Each entry in 'add_columns' for dataset '{ds_name}' must have an 'id'.")
    }
    if (new_id %in% existing_ids) {
      cli::cli_abort("Cannot add column '{new_id}' to dataset '{ds_name}': a column with that id already exists.")
    }
    ds$columns[[length(ds$columns) + 1L]] <- spec
    existing_ids <- c(existing_ids, new_id)
    deviations[[length(deviations) + 1L]] <- list(op = "add_columns", target = new_id)
  }

  # 3) modify_columns: a list of specs, each `id` plus the fields to change.
  # The spec states only what is changing, and anything it omits is left
  # untouched on the existing column. `id` itself is stripped from the merge
  # operand first so a patch cannot use modify_columns to silently rename a
  # column's own identity out from under later steps of the SAME patch.
  #
  # dta_template_merge_value() (template_inherit.R), not utils::modifyList():
  # one definition of "merge" for the whole engine. The two agree on every
  # shape a column actually holds today -- scalars, an atomic `values:`, a
  # `values_from:` mapping -- and on `field: null` meaning "delete this
  # property". They part on a sequence of MAPPINGS, where modifyList() recurses
  # into two unnamed lists, finds no names to walk, and returns the parent
  # untouched: the child's value is discarded in silence. Nothing shipped hits
  # that shape, which is exactly why it would have gone unnoticed.
  for (spec in (patch$modify_columns %||% list())) {
    mod_id <- as.character(spec$id %||% "")
    idx <- dataset_item_index_by_id(ds$columns, mod_id, "column", ds_name)
    ds$columns[[idx]] <- dta_template_merge_value(ds$columns[[idx]], spec[names(spec) != "id"])
    deviations[[length(deviations) + 1L]] <- list(op = "modify_columns", target = mod_id)
  }

  # 4) set: a named list whose names are DATASET-RELATIVE dotted paths
  # (description, files.filename, columns.GFNAM.values) -- prefixed with
  # "dataset." here and delegated to apply_dataset_template_path(), so this
  # op reuses the exact same id-addressing and files-ambiguity rules as a
  # template's own option effects, rather than a second implementation of
  # them.
  set_map <- patch$set %||% list()
  for (key in names(set_map)) {
    ds <- apply_dataset_template_path(ds, paste0("dataset.", key), set_map[[key]])
    deviations[[length(deviations) + 1L]] <- list(op = "set", target = key)
  }

  list(dataset = ds, deviations = deviations)
}

# The effective value of every option in a dataset template: the caller's
# selection when one was made, else the option's own default. Returns a named
# list keyed by option id.
#
# Deliberately thin: dta_template_default() (template_core.R) already knows
# how to fall back from an explicit `default:` to nothing (dataset templates
# have no `base.metadata` equivalent to inherit from, so the second argument
# is always NULL here) -- this just applies that, per option, across a whole
# template rather than reimplementing option-default resolution a second
# time.
dataset_template_selection_values <- function(def, selections = list()) {
  opts <- def$options %||% list()
  vals <- list()
  for (opt in opts) {
    oid <- as.character(opt$id %||% "")
    if (!nzchar(oid)) next
    vals[[oid]] <- if (!is.null(selections[[oid]])) {
      selections[[oid]]
    } else {
      dta_template_default(opt, NULL)
    }
  }
  vals
}

# Build a new dataset (as a plain list, ready for dta_dataset_from_list()/
# DTADataSetFactory) from a dataset template + selected option values + an
# optional patch. Returns dta_try()'s list(ok=, value=, error=) where value is
# list(dataset = <plain list>, deviations = <list>, provenance = <list>).
build_dataset_from_template <- function(def, selections = list(), patch = NULL, as_name = NULL, source_label = NULL,
                                        resolve_vocab = NULL) {
  dta_try({
    if (!is.list(def)) stop("Dataset template definition is invalid.")
    # Same `[[` vs `$` hazard as read_dataset_template()'s guard above -- this
    # second check exists because a caller (a test, a future direct build
    # path) may hand build_dataset_from_template() a `def` that never went
    # through read_dataset_template()'s own validation.
    if (!is.list(def[["dataset"]])) stop(.dataset_template_missing_dataset_msg(def))

    # 1) ${today} must be resolved before anything else touches the dataset --
    # mirroring create_dta_from_template()'s ${today} pass over `base` -- since
    # an option effect or the patch below may read or overwrite a field that
    # still held a raw token, and there is no later pass here to catch one
    # that survived unresolved into the final dataset.
    today_env <- dta_template_today_env()
    ds <- resolve_template_expressions(def[["dataset"]], today_env)
    opts <- resolve_template_expressions(def$options %||% list(), today_env)

    # 2) Option-driven effects. dataset_template_selection_values() decides
    # WHAT value each option contributes (selection or default);
    # collect_option_effects() decides WHAT THAT VALUE DOES (one path, several
    # paths via a set: map, or nothing at all) -- both reused as-is from
    # template_core.R rather than re-derived here. The "__selection__"
    # sentinel is honoured exactly as create_dta_from_template() honours it
    # for metadata effects: it means "substitute the option's chosen value at
    # this point", not the literal string.
    sel_values <- dataset_template_selection_values(list(options = opts), selections)
    for (opt in opts) {
      oid <- as.character(opt$id %||% "")
      if (!nzchar(oid)) next
      chosen <- sel_values[[oid]]
      effects <- collect_option_effects(opt, chosen)
      if (length(effects) == 0) next

      for (op in effects) {
        if (!is.list(op)) next

        if (!is.null(op$set) && is.list(op$set)) {
          for (p in names(op$set)) {
            val <- op$set[[p]]
            if (is.character(val) && identical(val, "__selection__")) val <- chosen
            ds <- apply_dataset_template_path(ds, p, val)
          }
        } else {
          p <- as.character(op$path %||% "")
          if (!nzchar(p)) next
          val <- op$value
          if (is.character(val) && identical(val, "__selection__")) val <- chosen
          ds <- apply_dataset_template_path(ds, p, val)
        }
      }
    }

    # 3) The free-form patch: column add/remove/modify, then general field
    # sets, in the fixed order documented on apply_dataset_patch().
    patched <- apply_dataset_patch(ds, patch)
    ds <- patched$dataset
    deviations <- patched$deviations

    # 4) Expand every `values_from:` binding into a plain `values:` vector.
    #
    # AFTER the patch, deliberately and load-bearingly: add_columns may
    # introduce a binding, modify_columns may re-bind or unbind one, and a
    # `set:` path may address `columns.<id>.values_from`. Expanding earlier
    # would resolve bindings the patch was still going to change, and would
    # leave any binding the patch itself added unexpanded -- reaching
    # DTAColumnSpec as an unused `values_from` argument.
    #
    # BEFORE the rename in step 5, though nothing depends on that ordering
    # today: expansion addresses columns by id, never by dataset name.
    # A caller that passed no resolver but whose template needs one gets it
    # built on demand from the cached index, rather than an error. This is the
    # SAME reasoning create_dta_from_template() gives for building an index on
    # demand when a `template:` dataset entry needs one: requiring every call
    # site to learn about the resolver so that a template AUTHOR can bind a
    # column to a vocabulary is the wrong coupling, and would break every
    # existing caller the moment a shipped template started using the feature.
    #
    # A template with no bindings never reaches this, so nobody pays the index
    # scan for a feature they are not using.
    if (is.null(resolve_vocab) && dataset_has_vocabulary_binding(ds)) {
      resolve_vocab <- vocabulary_resolver(dta_template_index_cached())
    }
    if (!is.null(resolve_vocab)) {
      ds <- expand_column_vocabularies(ds, resolve_vocab, dataset_name = ds$name)
    }

    # 5) Rename, if the caller asked for a name different from the template's
    # own -- e.g. adding a second dataset built from the same template to one
    # DTA, where both cannot keep the template's literal dataset name.
    if (!is.null(as_name) && is.character(as_name) && nzchar(as_name)) {
      ds$name <- as_name
    }

    # 6) Stamp provenance as PLAIN LIST KEYS on `ds`, not as an S7 property
    # write -- `ds` is still a plain list at this point, and
    # dta_dataset_from_list() (R/DTADataSet-class.R) does
    # do.call(DTADataSetFactory, x) on exactly this list, so any key present
    # becomes a constructor argument. That is what lets provenance ride
    # through to DTADataSet@template_source/@template_version/@template_date
    # without a bespoke setter of its own.
    #
    # template_version MUST be as.character() again here: DTADataSet@
    # template_version is class_character_or_null, and def$version, while
    # already coerced once by read_dataset_template(), is read fresh from
    # `def` -- a caller that builds `def` by hand (as tests do) may not have
    # gone through that coercion at all.
    ds$template_source <- if (is.null(source_label) || !nzchar(as.character(source_label %||% ""))) {
      as.character(def$id)
    } else {
      sprintf("%s (%s)", def$id, source_label)
    }
    ds$template_version <- as.character(def$version)
    if (!is.null(def$date)) {
      ds$template_date <- def$date # already character, from read_dataset_template()
    }

    list(
      dataset = ds,
      deviations = deviations,
      provenance = list(
        name = ds$name,
        template = def$id,
        version = as.character(def$version),
        deviations = deviations
      )
    )
  })
}
