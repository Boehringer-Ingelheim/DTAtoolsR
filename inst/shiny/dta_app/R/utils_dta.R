# -----------------------------------------------------------------------------
# Utilities: thin, safe wrappers around the DTAtools API.
# The app mutates the DTA object ONLY through these helpers.
#
# `%||%` and `dta_try()` below are the two primitives every other engine file
# (template_core.R, template_create.R, template_inherit.R, template_index.R,
# dataset_template.R, vocabulary.R, party_profiles.R, ...) uses without
# redefining -- this is the file the rest of the engine most depends on, which
# is why the rule below lives here once rather than at every call site that
# follows it.
#
# RULE: a list parsed from (or copied out of) a template author's YAML -- a
# template definition, a dataset body, a column, an option, a vocabulary slot,
# a `values_from:` binding, and so on -- is read with `[[` (exact match),
# never `$`. `$` on a list falls back to PARTIAL name matching when there is
# no exact key, so `x$foo` silently returns `x$foobar` when `foo` is absent
# and `foobar` is the only key starting with `foo`. On YAML with real, adjacent
# field names that turns "this field is absent" into "this field is present,
# with a different field's value" -- four such collisions are real, not
# theoretical, in this package's own shipped template corpus: `values`/
# `values_from`, `dataset`/`datasets`, `column`/`columns` and `vocabulary`/
# `vocabulary_slots`. See dataset_template.R's read_dataset_template()/
# build_dataset_from_template() and vocabulary.R's normalise_vocabulary_
# slots()/normalise_values_from() for the fixed call sites, and
# R/validateTemplate.R (the package-side sibling of this engine) for the same
# rule applied to its own two independently-discovered collisions.
#
# `$` stays fine for a list this engine's OWN code constructs with a fixed,
# known key set (an internal record, a `dta_try()` result, a normalised slot,
# a reactive value) -- those names are ours, so the hazard does not apply.
# -----------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Run an expression, capturing errors as a value instead of crashing the app.
# Returns list(ok = TRUE/FALSE, value = <result>, error = <message or NULL>).
dta_try <- function(expr) {
  tryCatch(
    list(ok = TRUE, value = force(expr), error = NULL),
    error = function(e) list(ok = FALSE, value = NULL, error = conditionMessage(e))
  )
}

# ---- Loading -------------------------------------------------------------

# Read a YAML file into a DTA object, detecting its shape first.
# A DTA YAML has a top-level `metadata:` and/or `datasets:` key. A standalone
# DATASET YAML (e.g. gf_dataset.yaml) has the dataset fields at the top level
# (`name`, `type`, `files`, `columns`) and NO `datasets`/`metadata` key -- it is
# read via read_dataset_from_yaml() and dropped into a NEW, EMPTY DTA (default
# metadata) so the user continues in the full DTA workspace: they can fill in the
# metadata, add more datasets and export a complete DTA (rather than being stuck
# in a restricted dataset-only view).
# Returns the usual dta_try() list plus three flags:
#   dataset_only    : always FALSE now (a dataset YAML is wrapped into a full
#                     DTA); retained for back-compat with callers/old sessions.
#   has_metadata    : TRUE when the source YAML carried a `metadata:` section.
#   wrapped_dataset : TRUE when a standalone dataset YAML was wrapped into a new
#                     empty DTA (used only for an informative load message).
dta_read_yaml <- function(path) {
  raw <- dta_try(yaml::read_yaml(path))
  if (!raw$ok) {
    return(list(
      ok = FALSE, value = NULL, error = raw$error,
      dataset_only = FALSE, has_metadata = FALSE, wrapped_dataset = FALSE
    ))
  }
  y <- raw$value
  has_metadata <- is.list(y) && !is.null(y$metadata)
  is_dta <- is.list(y) && (has_metadata || !is.null(y$datasets))

  if (is_dta) {
    res <- dta_try(DTAtools::read_dta_from_yaml(path))
    res$dataset_only <- FALSE
    res$has_metadata <- has_metadata
    res$wrapped_dataset <- FALSE
    return(res)
  }

  # Standalone dataset YAML -> create a NEW, EMPTY DTA (metadata defaults to an
  # empty DTAMetaData()) and put the dataset into it, so the user continues in
  # the full DTA workspace instead of a restricted dataset-only view.
  res <- dta_try({
    ds <- DTAtools::read_dataset_from_yaml(path)
    DTAtools::DTA(datasets = ds)
  })
  res$dataset_only <- FALSE
  res$has_metadata <- FALSE
  res$wrapped_dataset <- isTRUE(res$ok)
  res
}

# Validate a raw YAML STRING as a DTA/DTADataSet by staging it to a temp file
# and reusing dta_read_yaml() (which checks YAML syntax FIRST, then DTA/dataset
# structure). Returns the same list shape as dta_read_yaml() -- so callers get
# ok/value/error plus dataset_only/has_metadata/wrapped_dataset. Used by the
# editable Raw YAML tab so a save only replaces the loaded document when the text
# is BOTH valid YAML AND a valid DTA / DTADataSet (a bare dataset YAML is wrapped
# into a new empty DTA, exactly like loading one on the landing page).
dta_read_yaml_text <- function(text) {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)
  written <- tryCatch(
    {
      con <- file(tmp, open = "wb")
      writeLines(enc2utf8(as.character(text)), con, useBytes = TRUE)
      close(con)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!isTRUE(written)) {
    return(list(
      ok = FALSE, value = NULL,
      error = "Could not stage the YAML text for validation.",
      dataset_only = FALSE, has_metadata = FALSE, wrapped_dataset = FALSE
    ))
  }
  dta_read_yaml(tmp)
}

# ---- Creating ------------------------------------------------------------

# Build an EMPTY DTA: metadata only, no datasets. Backs the landing page's
# "Create new" button, for starting a specification from nothing rather than
# from a file or a template.
#
# `datasets = list()` is load-bearing, not decorative. DTA()'s `datasets`
# argument defaults to NULL while the @datasets property is declared
# class_list, so the S7 property check REJECTS DTA(metadata = ...) with no
# datasets argument -- the constructor never coerces its own default to a
# list. Passing the empty list explicitly is what makes an empty document
# constructible at all. The same gap is recorded package-side in
# tests/testthat/test-DTA.R.
#
# A zero-dataset DTA is a state the workspace already supports: build_
# structure() returns list() (not NULL) for it, which is what keeps the app in
# the workspace instead of bouncing back to the landing page, and removing the
# last dataset already reaches the same state.
dta_create_empty <- function(title, version, date = Sys.Date()) {
  dta_try({
    DTAtools::DTA(
      datasets = list(),
      metadata = DTAtools::DTAMetaData(
        title = title,
        version = version,
        date = date
      )
    )
  })
}

# ---- Introspection -------------------------------------------------------

dta_dataset_names <- function(dta) {
  ds <- tryCatch(DTAtools::datasets(dta), error = function(e) list())
  nm <- names(ds)
  if (is.null(nm)) character(0) else nm
}

dta_get_dataset <- function(dta, name) {
  tryCatch(DTAtools::datasets(dta, name), error = function(e) NULL)
}

# File handlers (DTAFile objects) of a dataset.
dta_handlers <- function(ds) {
  h <- tryCatch(ds@files, error = function(e) list())
  if (is.null(h)) list() else h
}

handler_min <- function(h) {
  v <- tryCatch(DTAtools::min_number_of_files(h), error = function(e) NA_real_)
  suppressWarnings(as.numeric(v))[1]
}

handler_max <- function(h) {
  v <- tryCatch(DTAtools::max_number_of_files(h), error = function(e) NA_real_)
  suppressWarnings(as.numeric(v))[1]
}

handler_is_pattern <- function(h) {
  isTRUE(tryCatch(h@pattern, error = function(e) FALSE))
}

# The declared filename or pattern ONLY -- never glue anything else onto this.
# It is read back verbatim in the exported specification document
# (format_datasets_detail(), utils_export.R), so a suffix here would make a
# declared filename unrecoverable from the export. Callers that also want the
# allowed-endings restriction use handler_endings() below and surface it as
# its own, clearly-labelled field.
handler_expected <- function(h) {
  fn <- tryCatch(h@filename, error = function(e) NA_character_)
  paste(fn, collapse = ", ")
}

# The allowed-endings restriction ("extensions") a `type = "any"` handler may
# declare, as a display string ("pdf, zip") or "" when none is set. Kept
# separate from handler_expected() -- see the comment there.
handler_endings <- function(h) {
  ext <- tryCatch(h@extensions, error = function(e) NULL)
  if (is.null(ext) || length(ext) == 0) "" else paste(ext, collapse = ", ")
}

handler_hint <- function(h) {
  pd <- tryCatch(h@pattern_description, error = function(e) NULL)
  info <- tryCatch(h@info, error = function(e) NULL)
  parts <- c(
    if (!is.null(pd) && nzchar(pd)) pd else NULL,
    if (!is.null(info) && length(info) > 0) paste(unlist(info), collapse = " ") else NULL
  )
  paste(parts, collapse = " \u2014 ")
}

handler_count_label <- function(h) {
  mn <- handler_min(h)
  mx <- handler_max(h)
  if (is.na(mn) && is.na(mx)) {
    return("")
  }
  if (!is.na(mn) && !is.na(mx) && mn == mx) {
    sprintf("%d file%s", as.integer(mn), if (mn == 1) "" else "s")
  } else {
    sprintf(
      "%s\u2013%s files", ifelse(is.na(mn), "?", as.integer(mn)),
      ifelse(is.na(mx), "?", as.integer(mx))
    )
  }
}

# Does a dropped file name match this handler? Mirrors matches_filename():
# basename only, regex (pattern) or exact set membership, case-sensitive.
# matches_filename() may return a VECTOR when the handler carries multiple
# filenames/patterns -> reduce with any(). Never wrap in isTRUE() alone, which
# collapses a length>1 result to FALSE and silently rejects valid files.
handler_matches <- function(h, filename) {
  res <- tryCatch(
    DTAtools::matches_filename(h, filename),
    error = function(e) FALSE
  )
  isTRUE(any(as.logical(res), na.rm = TRUE))
}

# ---- Mutations -----------------------------------------------------------

# Copy an uploaded temp file to a fresh path whose basename is the ORIGINAL
# upload name. Shiny stores uploads as "0.csv", "1.csv", ... (input$file$datapath);
# matches_filename() / read_file() key off basename(file), so passing the raw
# datapath makes the package see "0.csv" -- which matches no handler pattern and
# a valid file is wrongly rejected inside load_file(). Returns the staged path
# (or the original datapath on any copy failure).
dta_stage_upload <- function(datapath, filename) {
  base <- basename(filename %||% "")
  if (!nzchar(base)) base <- basename(datapath)
  target_dir <- tempfile("dta_upload_")
  tryCatch(
    {
      dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
      target <- file.path(target_dir, base)
      if (isTRUE(file.copy(datapath, target, overwrite = TRUE))) target else datapath
    },
    error = function(e) datapath
  )
}

# List the data files bundled in inst/extdata that can populate an upload slot,
# i.e. everything EXCEPT the YAML specification documents. Returns basenames.
dta_example_data_files <- function() {
  dir <- system.file("extdata", package = "DTAtools")
  if (!nzchar(dir) || !dir.exists(dir)) {
    return(character(0))
  }
  files <- list.files(dir, full.names = FALSE, recursive = FALSE)
  if (length(files) == 0) {
    return(character(0))
  }
  files <- files[!dir.exists(file.path(dir, files))] # drop any subdirectories
  ext <- tolower(tools::file_ext(files))
  files <- files[!ext %in% c("yaml", "yml")] # drop YAML specification docs
  sort(files)
}

# Absolute path to a bundled example data file (given its basename). Returns ""
# when the name is empty or is not one of the bundled example files -- the
# whitelist check keeps a crafted input value from reading an arbitrary path.
dta_example_data_path <- function(filename) {
  if (is.null(filename) || !nzchar(filename)) {
    return("")
  }
  if (!basename(filename) %in% dta_example_data_files()) {
    return("")
  }
  system.file("extdata", basename(filename), package = "DTAtools")
}

# The order the landing page offers the bundled examples in: a deliberate
# teaching progression -- one tabular dataset, then one dataset fed by several
# files, then a never-parsed file dataset alongside a tabular one, then the
# genomics specification. Plain alphabetical order put that sequence at the
# mercy of the filenames (and of the user's collation locale); naming it here
# means a later example cannot silently jump the queue.
dta_example_yaml_preferred_order <- c(
  "clinical_dta.yaml",
  "clinical_dta_multiple_files.yaml",
  "clinical_dta_with_file_dataset.yaml",
  "gf_dataset.yaml"
)

# Applies that order to a set of basenames: the preferred names that are
# actually present, in the order named above, then everything else in
# C-collation order -- method = "radix", so an example added later lands in the
# same place on a German machine as it does in CI.
#
# Split out from dta_example_yaml_files() so the POLICY can be tested with
# inputs the bundled inst/extdata does not contain. The interesting cases are
# exactly the ones the real directory cannot produce today: an example that is
# not on the preferred list, a preferred name that is absent (a typo, or a file
# removed), and a duplicate in the constant. Nothing here may drop a file --
# every input basename must come out exactly once.
dta_order_example_yaml_files <- function(files) {
  known <- intersect(dta_example_yaml_preferred_order, files)
  c(known, sort(setdiff(files, known), method = "radix"))
}

# List the YAML specification documents bundled in inst/extdata (basenames),
# i.e. the counterpart of dta_example_data_files() that keeps ONLY .yaml/.yml.
# Used by the landing page to offer every bundled example DTA to load, in the
# order dta_order_example_yaml_files() decides.
dta_example_yaml_files <- function() {
  dir <- system.file("extdata", package = "DTAtools")
  if (!nzchar(dir) || !dir.exists(dir)) {
    return(character(0))
  }
  files <- list.files(dir, full.names = FALSE, recursive = FALSE)
  if (length(files) == 0) {
    return(character(0))
  }
  files <- files[!dir.exists(file.path(dir, files))] # drop any subdirectories
  ext <- tolower(tools::file_ext(files))
  files <- files[ext %in% c("yaml", "yml")] # keep YAML specs only
  dta_order_example_yaml_files(files)
}

# Absolute path to a bundled example YAML (given its basename). Returns "" when
# the name is empty or not one of the bundled YAML specs -- the whitelist check
# keeps a crafted input value from reading an arbitrary path.
dta_example_yaml_path <- function(filename) {
  if (is.null(filename) || !nzchar(filename)) {
    return("")
  }
  if (!basename(filename) %in% dta_example_yaml_files()) {
    return("")
  }
  system.file("extdata", basename(filename), package = "DTAtools")
}

# Load a file into a dataset via the package. Returns dta_try() result whose
# value (on success) is the UPDATED dta object.
#
# No `stream` argument: load_file() decides for itself. Its default is "auto",
# which reads a file into memory below the size threshold and scans a larger
# one in batches. Asking the user to choose put a storage-strategy question in
# front of someone who came to validate a file, and the answer is one the size
# of the file already determines.
dta_load_file <- function(dta, dataset, file, handler_index, name = NULL) {
  # The default key belongs to the DATASET TYPE, not to this helper. A tabular
  # dataset names a table after the file with its extension stripped; a file
  # dataset keys by the delivered name, extension kept, which is what
  # dta_file_target_keys() produces and what every report for that class uses.
  #
  # This used to hardcode the tabular rule for both. The app's own call site
  # passes `name` explicitly and so never noticed, but every other caller got a
  # key no report for a file dataset would ever look up -- and now that
  # load_file() for a DTADataSetFile refuses a `name` that is not the delivered
  # basename, that same default aborts outright instead of failing quietly.
  # dta_bound_item_name() is the single place that branch is written.
  ds_type <- tryCatch(
    dta_get_dataset(dta, dataset)@type,
    error = function(e) NA_character_
  )
  nm <- name %||% dta_bound_item_name(ds_type, file)
  dta_try(DTAtools::load_file(
    dta,
    dataset = dataset,
    file = file,
    handler_index = handler_index,
    name = nm
  ))
}

# Run validation for all datasets or one named dataset. Returns dta_try()
# result whose value (on success) is the UPDATED dta object.
dta_check <- function(dta, dataset = NULL) {
  dta_try({
    if (is.null(dataset)) {
      DTAtools::check(dta, quiet = TRUE, persist = FALSE)
    } else {
      DTAtools::check(dta, datasets = dataset, quiet = TRUE, persist = FALSE)
    }
  })
}

# Update a single metadata scalar field, returning an updated dta object.
# For the OPTIONAL nullable fields (date, error_handling,
# authorized_for_corrections) a blank/empty value UNSETS the property (NULL)
# instead of storing "" -- so an empty field is "not set at all" in the object
# (and is omitted from the serialized YAML).
dta_set_metadata_field <- function(dta, field, value) {
  dta_try({
    md <- DTAtools::metadata(dta)
    unset_when_blank <- c("date", "error_handling", "authorized_for_corrections")
    is_blank <- is.null(value) ||
      length(value) == 0 ||
      (is.character(value) && !any(nzchar(trimws(value)))) ||
      (length(value) == 1 && is.na(value))
    if (identical(field, "date")) {
      if (is_blank) {
        value <- NULL
      } else {
        value <- tryCatch(as.Date(value), error = function(e) value)
        if (inherits(value, "Date") && all(is.na(value))) value <- NULL
      }
    } else if (field %in% unset_when_blank && is_blank) {
      value <- NULL
    }
    S7::prop(md, field) <- value
    dta@metadata <- md
    dta
  })
}

# ---- People / contacts ---------------------------------------------------
# Contacts live under metadata receiver/supplier as `$contacts` (a list of
# person lists). Roles are free text; multiple roles are comma-joined so the
# document exporter (which reads a single `role`) keeps working.

dta_contacts <- function(dta, side) {
  md <- tryCatch(DTAtools::metadata(dta), error = function(e) NULL)
  if (is.null(md)) {
    return(list())
  }
  side_list <- tryCatch(S7::prop(md, side), error = function(e) NULL)
  cs <- side_list$contacts %||% list()
  if (is.null(cs)) list() else cs
}

dta_add_contact <- function(dta, side, name, roles = "", email = "",
                            department = "", phone = "", address = "") {
  dta_try({
    md <- DTAtools::metadata(dta)
    side_list <- tryCatch(S7::prop(md, side), error = function(e) NULL) %||% list()
    contacts <- side_list$contacts %||% list()
    role_str <- paste(roles[nzchar(roles)], collapse = ", ")
    person <- list(name = name, role = role_str, email = email)
    # Optional richer fields (omit when blank) so add + edit stay symmetrical.
    extra <- list(department = department, phone = phone, address = address)
    for (k in names(extra)) {
      v <- extra[[k]]
      if (!is.null(v) && is.character(v) && nzchar(v)) person[[k]] <- v
    }
    contacts[[length(contacts) + 1L]] <- person
    side_list$contacts <- contacts
    S7::prop(md, side) <- side_list
    dta@metadata <- md
    dta
  })
}

dta_remove_contact <- function(dta, side, index) {
  dta_try({
    md <- DTAtools::metadata(dta)
    side_list <- tryCatch(S7::prop(md, side), error = function(e) NULL) %||% list()
    contacts <- side_list$contacts %||% list()
    if (index >= 1 && index <= length(contacts)) {
      contacts[[index]] <- NULL
    }
    side_list$contacts <- contacts
    S7::prop(md, side) <- side_list
    dta@metadata <- md
    dta
  })
}

contact_display <- function(person) {
  nm <- person$name %||% "(unnamed)"
  role <- person$role %||% ""
  if (nzchar(role)) sprintf("%s \u2014 %s", nm, role) else nm
}

# ---- Affiliation (side-level) --------------------------------------------
# Affiliation belongs to the SIDE (receiver / supplier), NOT to individual
# contacts: metadata receiver/supplier each carry a single `$affiliation` list
# (fields: name [organization], country, address). Contacts are a sibling list.

dta_affiliation <- function(dta, side) {
  md <- tryCatch(DTAtools::metadata(dta), error = function(e) NULL)
  if (is.null(md)) {
    return(list())
  }
  side_list <- tryCatch(S7::prop(md, side), error = function(e) NULL)
  aff <- side_list$affiliation %||% list()
  if (is.null(aff)) list() else aff
}

# Update one affiliation field, returning an updated dta object. A NULL value
# leaves the field untouched; an empty string removes it.
dta_set_affiliation <- function(dta, side, name = NULL, country = NULL, address = NULL) {
  dta_try({
    md <- DTAtools::metadata(dta)
    side_list <- tryCatch(S7::prop(md, side), error = function(e) NULL) %||% list()
    aff <- side_list$affiliation %||% list()
    set_field <- function(a, key, val) {
      if (is.null(val)) {
        return(a)
      }
      if (nzchar(val)) a[[key]] <- val else a[[key]] <- NULL
      a
    }
    aff <- set_field(aff, "name", name)
    aff <- set_field(aff, "country", country)
    aff <- set_field(aff, "address", address)
    side_list$affiliation <- aff
    S7::prop(md, side) <- side_list
    dta@metadata <- md
    dta
  })
}

# ---- Status / results ----------------------------------------------------

# Number of data items actually bound to a dataset -- the SOURCE OF TRUTH for
# "does this dataset have data": tabular -> number of tables; file -> number of
# bound file paths. Robust to either slot being absent.
dta_dataset_content_count <- function(ds) {
  if (is.null(ds)) {
    return(0L)
  }
  ty <- tryCatch(ds@type, error = function(e) NA_character_)
  n <- if (identical(ty, "file")) {
    length(tryCatch(ds@file_paths, error = function(e) character(0)))
  } else {
    length(tryCatch(ds@tables, error = function(e) list()))
  }
  as.integer(n %||% 0L)
}

# Readiness of a dataset for validation. has_data guards the "vacuous pass" bug
# (a 0-table dataset must never validate as passed). Returns
# list(count, min, has_data, ready).
dta_dataset_readiness <- function(dta, dataset) {
  ds <- dta_get_dataset(dta, dataset)
  count <- dta_dataset_content_count(ds)
  mn <- tryCatch(DTAtools::min_number_of_files(ds), error = function(e) NA_real_)
  mn <- suppressWarnings(as.numeric(mn))[1]
  has_data <- count > 0
  ready <- has_data && (is.na(mn) || count >= mn)
  list(count = count, min = mn, has_data = has_data, ready = ready)
}

# Compute a per-dataset status map: "nodata" | "pending" | "pass" | "fail".
# A dataset with zero bound tables/files is ALWAYS "nodata" -- never "pass" --
# regardless of any vacuous validation result the package may have produced.
dta_status_map <- function(dta) {
  names_ds <- dta_dataset_names(dta)
  out <- stats::setNames(rep("pending", length(names_ds)), names_ds)
  if (length(names_ds) == 0) {
    return(out)
  }

  res <- tryCatch(DTAtools::results(dta), error = function(e) NULL)
  # One dataset must not be able to blind the whole map. results(dta) walks
  # every dataset and aborts outright if ANY of them cannot report -- a tabular
  # dataset with no tables loaded raises "No tables found in dataset." -- which
  # would leave every OTHER dataset, including ones that just validated
  # cleanly, showing as merely pending. That is easy to reach the moment a
  # document holds more than one dataset and the user fills in one of them:
  # they check it, it passes, and nothing turns green.
  #
  # Falling back to asking each dataset on its own keeps the failure local to
  # the dataset that actually cannot answer.
  if (is.null(res)) {
    per_dataset <- lapply(names_ds, function(nm) {
      tryCatch(
        DTAtools::results(dta_get_dataset(dta, nm)),
        error = function(e) NULL
      )
    })
    per_dataset <- Filter(
      function(r) !is.null(r) && nrow(r) > 0,
      per_dataset
    )
    res <- if (length(per_dataset) > 0) do.call(rbind, per_dataset) else NULL
  }
  have_res <- !is.null(res) && nrow(res) > 0
  if (have_res) {
    n_invalid <- if ("n_invalid" %in% names(res)) suppressWarnings(as.numeric(res$n_invalid)) else rep(NA_real_, nrow(res))
    n_valid <- if ("n_valid" %in% names(res)) suppressWarnings(as.numeric(res$n_valid)) else rep(NA_real_, nrow(res))
    n_validated <- if ("n_validated" %in% names(res)) suppressWarnings(as.numeric(res$n_validated)) else rep(NA_real_, nrow(res))
    ds_col <- if ("dataset" %in% names(res)) as.character(res$dataset) else names_ds
  }

  for (nm in names_ds) {
    # No bound data -> nodata, independent of results() (Contract C5).
    if (dta_dataset_content_count(dta_get_dataset(dta, nm)) == 0) {
      out[[nm]] <- "nodata"
      next
    }
    if (!have_res) {
      out[[nm]] <- "pending"
      next
    }
    idx <- which(ds_col == nm)
    if (length(idx) == 0) {
      out[[nm]] <- "pending"
      next
    }
    inv <- sum(n_invalid[idx], na.rm = TRUE)
    val <- sum(n_valid[idx], na.rm = TRUE)
    done <- sum(n_validated[idx], na.rm = TRUE)
    if (!is.na(inv) && inv > 0) {
      out[[nm]] <- "fail"
    } else if (done > 0 || val > 0) {
      out[[nm]] <- "pass"
    } else {
      out[[nm]] <- "pending"
    }
  }
  out
}

# ---- Loaded-file management ----------------------------------------------
# A dataset binds data as named items: tabular datasets keep an @tables list
# (name -> Arrow table), file datasets keep @file_paths. load_file() names each
# table after the file (file_path_sans_ext), so a file <-> table is 1:1. These
# helpers expose that mapping to the app: detect overwrite conflicts, show a
# per-file validation state, and remove individual / all bound files.

# Names of the data items currently bound to a dataset (dataset-wide truth):
# table names for tabular datasets, file basenames for file datasets.
dta_dataset_table_names <- function(ds) {
  if (is.null(ds)) {
    return(character(0))
  }
  ty <- tryCatch(ds@type, error = function(e) NA_character_)
  if (identical(ty, "file")) {
    fp <- tryCatch(ds@file_paths, error = function(e) character(0)) %||% character(0)
    basename(fp)
  } else {
    names(tryCatch(ds@tables, error = function(e) list())) %||% character(0)
  }
}

# Compute a bound item's key: the single source of truth for how a dataset's
# inputs are indexed. For file datasets, the key is the basename WITH extension
# (matching what the package's dta_file_target_keys() produces and what
# validation_status(), messages(), inspect(), and dta_unload_table() all key
# on). For tabular datasets, the key is the basename WITHOUT extension
# (a table name). Must be vectorised over filename.
dta_bound_item_name <- function(type, filename) {
  base <- basename(filename)
  if (identical(type, "file")) base else tools::file_path_sans_ext(base)
}

# Per-table validation status: named vector
# table -> "pass" | "fail" | "unknown" | "pending".
#
#   "pending" = not validated yet (no tick)
#   "pass"    = validated, all THREE axes clean (column spec, rules, import)
#   "fail"    = validated with column spec, rule OR import errors
#   "unknown" = validated, but the import axis was never checked
#
# Validation has three axes, and ok = columnspec_valid && rules_valid &&
# import_valid. A table whose only defect is a value that could not be
# represented in its declared type has zero column spec and zero rule errors, so
# reading only those two axes paints it green while ok is FALSE. n_import_errors
# is therefore weighed exactly like the other two counts.
#
# NA is not a pass. A validation artifact written before the import axis existed
# reports n_import_errors = NA ("unknown"), and its recorded `ok` is whatever the
# two-axis run concluded -- so an NA import count can arrive alongside ok = TRUE.
# That combination means "re-run check(force = TRUE)", not "clean", and gets its
# own third state rather than being folded into pass or fail. The column being
# entirely ABSENT is a different situation (an object older than the column
# itself, e.g. a hand-built status frame) and keeps the old two-axis behaviour.
dta_table_status_map <- function(dta, dataset) {
  ds <- dta_get_dataset(dta, dataset)
  if (is.null(ds)) {
    return(stats::setNames(character(0), character(0)))
  }
  vs <- tryCatch(as.data.frame(DTAtools::validation_status(ds)),
    error = function(e) NULL
  )
  dta_table_status_from_status_df(vs)
}

# Pure core of dta_table_status_map(): maps a validation_status() data.frame to
# the per-table status vector. Split out so the status logic is testable without
# constructing a DTA.
dta_table_status_from_status_df <- function(vs) {
  empty <- stats::setNames(character(0), character(0))
  if (is.null(vs) || !is.data.frame(vs) || nrow(vs) == 0) {
    return(empty)
  }
  tcol <- if ("table" %in% names(vs)) "table" else if ("target" %in% names(vs)) "target" else names(vs)[1]
  ok <- if ("ok" %in% names(vs)) vs$ok else rep(NA, nrow(vs))
  count <- function(col) {
    if (col %in% names(vs)) suppressWarnings(as.numeric(vs[[col]])) else rep(NA_real_, nrow(vs))
  }
  nse <- count("n_columnspec_errors")
  nre <- count("n_rule_errors")
  nie <- count("n_import_errors")
  positive <- function(n) !is.na(n) & n > 0
  has_err <- positive(nse) | positive(nre) | positive(nie) | (!is.na(ok) & !ok)
  import_unknown <- ("n_import_errors" %in% names(vs)) & is.na(nie)

  st <- rep("pending", nrow(vs))
  st[!is.na(ok) & ok] <- "pass"
  # An unchecked import axis downgrades a pass to "unknown" ...
  st[import_unknown & st == "pass"] <- "unknown"
  # ... but a defect observed on any axis is still a definite failure.
  st[has_err] <- "fail"
  stats::setNames(st, as.character(vs[[tcol]]))
}

# Look up ONE name in a named vector or list, with a default for an absent name.
#
# `[[` is null-safe on a LIST and is NOT null-safe on an ATOMIC vector:
# `list(a = 1)[["b"]]` is NULL, but `c(a = "1")[["b"]]` throws "subscript out of
# bounds". Crucially, `x[["b"]] %||% "default"` cannot rescue the atomic case --
# the error is raised while evaluating the left operand, so `%||%` never runs.
#
# Both status maps are atomic character vectors -- dta_status_map() and
# dta_table_status_from_status_df() both end in stats::setNames() over a
# character vector -- while rv$status is *also* assigned a bare list() when a
# document is closed. Neither shape may therefore be assumed at the point of
# use, and an absent name is entirely normal: a file bound but not yet checked
# has no row in validation_status() at all, which is exactly the state the app
# renders as "pending".
dta_lookup <- function(x, name, default = NULL) {
  if (is.null(x) || is.null(name) || length(name) != 1L) {
    return(default)
  }
  name <- as.character(name)
  if (is.na(name)) {
    return(default)
  }
  nms <- names(x)
  if (is.null(nms) || !(name %in% nms)) {
    return(default)
  }
  out <- x[[name]]
  if (is.null(out)) default else out
}

# Reset ("not validated") the validation status of one/all tables of a dataset,
# used after an overwrite so a stale pass/fail is never shown for changed data.
# Returns dta_try() result (value = updated DTA).
dta_clear_validation <- function(dta, dataset, tables = NULL) {
  dta_try({
    ds <- DTAtools::datasets(dta, dataset)
    ds <- DTAtools::clear_validation(ds, tables = tables, remove_artifacts = FALSE)
    dta@datasets[[dataset]] <- ds
    dta
  })
}

# Does this dataset carry an @import_issues property? Only DTADataSetTabular
# does; a file dataset has no typed tables and therefore no import issues.
# DTAMetaData also carries an @import_issues property (its own, metadata-level
# shape), so a bare prop_names() check would claim a metadata object has
# per-table issues. Require a dataset as well as the property.
dta_has_import_issues <- function(ds) {
  if (!inherits(ds, "DTAtools::DTADataSet")) {
    return(FALSE)
  }

  isTRUE("import_issues" %in% tryCatch(S7::prop_names(ds), error = function(e) character(0)))
}

# Remove ONE bound file/table (and its validation state) from a dataset.
# ORDER MATTERS: the table's per-table state (validation_index, validation_store
# AND import_issues) is cleared BEFORE the table/file itself is dropped. The
# DTADataSetTabular validator forbids any of those three lists being LONGER than
# `tables`, so dropping a VALIDATED table first would transiently leave more
# entries than tables (vindex > tables) and abort the whole removal (F20).
# Clearing that state first keeps every intermediate object valid, so removing a
# table that was already validated always succeeds.
dta_unload_table <- function(dta, dataset, table) {
  dta_try({
    ds <- DTAtools::datasets(dta, dataset)
    # 1) Drop this table's validation state FIRST (index + store together).
    vi <- tryCatch(ds@validation_index, error = function(e) list()) %||% list()
    vi[[table]] <- NULL
    ds@validation_index <- vi
    vsr <- tryCatch(ds@validation_store, error = function(e) list()) %||% list()
    vsr[[table]] <- NULL
    ds@validation_store <- vsr
    # ... and its import issues, which are keyed by table name just the same.
    # Left behind, they would outlive the table and be re-attached to a new file
    # that happens to reuse the name.
    if (dta_has_import_issues(ds)) {
      ii <- tryCatch(ds@import_issues, error = function(e) list()) %||% list()
      ii[[table]] <- NULL
      ds@import_issues <- ii
    }
    # 2) Then remove the table/file itself.
    ty <- tryCatch(ds@type, error = function(e) NA_character_)
    if (identical(ty, "file")) {
      fp <- tryCatch(ds@file_paths, error = function(e) character(0)) %||% character(0)
      ds@file_paths <- fp[basename(fp) != table]
    } else {
      tabs <- tryCatch(ds@tables, error = function(e) list()) %||% list()
      tabs[[table]] <- NULL
      ds@tables <- tabs
    }
    dta@datasets[[dataset]] <- ds
    dta
  })
}

# Remove ALL bound files/tables (and all validation state) from a dataset.
dta_unload_all <- function(dta, dataset) {
  dta_try({
    ds <- DTAtools::datasets(dta, dataset)
    # Same order as dta_unload_table(): clear the per-table state before the
    # tables, so no intermediate object has more state entries than tables.
    ds@validation_index <- list()
    ds@validation_store <- list()
    if (dta_has_import_issues(ds)) {
      ds@import_issues <- list()
    }
    ty <- tryCatch(ds@type, error = function(e) NA_character_)
    if (identical(ty, "file")) {
      ds@file_paths <- character(0)
    } else {
      ds@tables <- list()
    }
    dta@datasets[[dataset]] <- ds
    dta
  })
}

# Per-error messages for a single dataset (data.frame). Empty df if none.
dta_dataset_messages <- function(dta, dataset) {
  ds <- dta_get_dataset(dta, dataset)
  if (is.null(ds)) {
    return(data.frame())
  }
  res <- dta_try(as.data.frame(DTAtools::messages(ds)))
  if (!res$ok || is.null(res$value)) {
    return(data.frame())
  }
  res$value
}

# Metadata import errors, as message rows (data.frame; empty df if none).
#
# These are DTA-LEVEL: metadata belongs to the document, not to a dataset, so
# messages(dta) reports them with target == "metadata" and messages(ds) -- what
# the per-dataset messages dock shows -- never contains them. The metadata
# editor is the only place they can be seen, hence this helper. Queried through
# messages(metadata(dta)), which yields exactly those rows and, unlike
# messages(dta), does not require the DTA to have any datasets.
dta_metadata_import_messages <- function(dta) {
  md <- tryCatch(DTAtools::metadata(dta), error = function(e) NULL)
  if (is.null(md)) {
    return(data.frame())
  }
  res <- dta_try(as.data.frame(DTAtools::messages(md)))
  if (!res$ok || is.null(res$value)) {
    return(data.frame())
  }
  res$value
}

# Pull the fields an IMPORT inspect record carries out of one flattened
# inspect() row (as a list). inspect() puts the structured import error in
# `import_*` columns (from its import_matches frame); the flat message columns
# are the fallback for a record that carries only those.
#
# Kept here, out of the server, so the column names and their fallbacks -- the
# part that silently breaks when the package renames a field -- are testable
# without launching Shiny. Every element is a length-1 character, "" when
# absent.
dta_inspect_import_fields <- function(r) {
  first <- function(...) {
    for (v in list(...)) {
      if (!is.null(v) && length(v) >= 1 && !is.na(v[[1]]) && nzchar(as.character(v[[1]]))) {
        return(as.character(v[[1]]))
      }
    }
    ""
  }
  list(
    column = first(r[["import_column"]], r[["column"]]),
    raw = first(r[["import_raw"]]),
    declared_type = first(r[["import_declared_type"]]),
    reason = first(r[["import_reason"]], r[["keyword"]]),
    row = first(r[["import_row"]], r[["row"]], r[["context_.row"]])
  )
}

# Deep-dive detail for one message id within a dataset (data.frame).
dta_inspect <- function(dta, dataset, id) {
  ds <- dta_get_dataset(dta, dataset)
  if (is.null(ds)) {
    return(dta_try(stop("dataset not found")))
  }
  dta_try(as.data.frame(DTAtools::inspect(ds, id = id)))
}

# ---- Export --------------------------------------------------------------

dta_export <- function(dta, file, format, signature_list = NULL) {
  format <- tolower(format %||% "")
  # PDF gets a dedicated, robust path (see dta_export_pdf); other formats go
  # straight to the package writer.
  if (identical(format, "pdf")) {
    return(dta_try(dta_export_pdf(dta, file, signature_list)))
  }
  dta_try(DTAtools::write_dta(
    dta,
    file = file,
    format = format,
    overwrite = TRUE,
    include_signatures = !is.null(signature_list),
    signature_list = signature_list,
    quiet = TRUE
  ))
}

# Does `file` exist and begin with the %PDF- magic bytes? Used to VERIFY a
# converter actually produced a genuine PDF (rather than a mislabelled DOCX or
# an empty file, which is what made the export button appear to do nothing).
dta_is_pdf <- function(file) {
  if (is.null(file) || !file.exists(file)) {
    return(FALSE)
  }
  if (isTRUE(file.info(file)$size < 5)) {
    return(FALSE)
  }
  sig <- tryCatch(
    {
      con <- file(file, "rb")
      on.exit(close(con), add = TRUE)
      readChar(con, 5L, useBytes = TRUE)
    },
    error = function(e) ""
  )
  isTRUE(grepl("^%PDF-", sig %||% ""))
}

# Locate a LibreOffice/soffice binary (PATH first, then common install dirs).
dta_find_soffice <- function() {
  cand <- Sys.which(c("soffice", "libreoffice"))
  cand <- cand[nzchar(cand)]
  if (length(cand)) {
    return(unname(cand[[1]]))
  }
  guesses <- c(
    "C:/Program Files/LibreOffice/program/soffice.exe",
    "C:/Program Files (x86)/LibreOffice/program/soffice.exe",
    "/usr/bin/soffice", "/usr/bin/libreoffice", "/usr/local/bin/soffice",
    "/Applications/LibreOffice.app/Contents/MacOS/soffice"
  )
  hit <- guesses[file.exists(guesses)]
  if (length(hit)) hit[[1]] else ""
}

# Convert docx -> pdf with a headless LibreOffice call. Returns TRUE only when a
# real PDF lands at `file`.
dta_soffice_to_pdf <- function(soffice, docx, file) {
  outdir <- tempfile("pdfout")
  dir.create(outdir)
  on.exit(unlink(outdir, recursive = TRUE), add = TRUE)
  ok <- tryCatch(
    {
      system2(
        soffice,
        c(
          "--headless", "--norestore", "--convert-to", "pdf",
          "--outdir", shQuote(outdir), shQuote(docx)
        ),
        stdout = TRUE, stderr = TRUE, timeout = 120
      )
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) TRUE
  )
  produced <- file.path(outdir, paste0(tools::file_path_sans_ext(basename(docx)), ".pdf"))
  if (isTRUE(ok) && file.exists(produced)) file.copy(produced, file, overwrite = TRUE)
  dta_is_pdf(file)
}

# First pandoc-usable PDF engine found on the system, or "" when none exists.
# pandoc CANNOT make a PDF without one, so we probe before attempting (otherwise
# it errors with "pdflatex not found").
dta_pandoc_pdf_engine <- function() {
  engines <- c(
    "pdflatex", "xelatex", "lualatex", "tectonic",
    "wkhtmltopdf", "weasyprint", "typst", "context", "pdfroff"
  )
  for (eng in engines) {
    if (nzchar(Sys.which(eng))) {
      return(eng)
    }
  }
  if (requireNamespace("tinytex", quietly = TRUE) &&
    isTRUE(tryCatch(tinytex::is_tinytex(), error = function(e) FALSE))) {
    return("pdflatex")
  }
  ""
}

# Convert docx -> pdf via pandoc, but only when a PDF engine is actually present.
dta_pandoc_to_pdf <- function(docx, file) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    return(FALSE)
  }
  if (!isTRUE(tryCatch(rmarkdown::pandoc_available(), error = function(e) FALSE))) {
    return(FALSE)
  }
  engine <- dta_pandoc_pdf_engine()
  if (!nzchar(engine)) {
    return(FALSE)
  }
  ok <- tryCatch(
    {
      rmarkdown::pandoc_convert(
        input = normalizePath(docx, winslash = "/", mustWork = TRUE),
        to = "pdf",
        output = normalizePath(file, winslash = "/", mustWork = FALSE),
        options = c("--pdf-engine", engine)
      )
      TRUE
    },
    error = function(e) FALSE
  )
  isTRUE(ok) && dta_is_pdf(file)
}

# Render Markdown lines to a valid, self-contained, FORMATTED multi-page PDF
# using R's built-in graphics devices -- NO external tools (LaTeX / LibreOffice /
# Chrome) required, so a PDF can ALWAYS be produced. The Markdown is actually
# "compiled": headings, inline **bold** labels, bullet lists, `| pipe |` tables
# and `---` rules are drawn as formatted content (not dumped as raw source).
# cairo_pdf (full Unicode) is preferred; base pdf() with a Latin1 transliteration
# is the ultimate fallback.
dta_markdown_to_pdf <- function(lines, file) {
  cairo_ok <- tryCatch(as.logical(capabilities("cairo"))[1], error = function(e) FALSE)
  if (is.na(cairo_ok)) cairo_ok <- FALSE

  ascii <- function(x) x
  opened <- FALSE
  if (isTRUE(cairo_ok)) {
    opened <- tryCatch(
      {
        grDevices::cairo_pdf(file, width = 8.27, height = 11.69, onefile = TRUE, pointsize = 10)
        TRUE
      },
      error = function(e) FALSE
    )
  }
  if (!isTRUE(opened)) {
    # Base pdf() renders only Latin1 with the built-in fonts: transliterate.
    ascii <- function(x) {
      x <- enc2utf8(as.character(x) %||% "")
      x <- gsub("[\u2012-\u2015]", "-", x) # figure/en/em dashes
      x <- gsub("[\u2018\u2019]", "'", x) # curly single quotes
      x <- gsub("[\u201C\u201D]", '"', x) # curly double quotes
      x <- gsub("\u2022", "*", x) # bullet
      out <- iconv(x, from = "UTF-8", to = "latin1", sub = "?")
      ifelse(is.na(out), gsub("[^ -~]", "?", x), out)
    }
    grDevices::pdf(file, width = 8.27, height = 11.69, onefile = TRUE, pointsize = 10)
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  dta_render_markdown(lines, ascii = ascii)
  invisible(file)
}

# Draw "compiled" Markdown onto the CURRENT graphics device, managing its own
# A4 pages via plot.new(). Headings, inline **bold** labels, bullet lists,
# `| pipe |` tables and `---` rules are rendered as formatted content -- never
# dumped as raw source. `ascii` transliterates glyphs for devices whose fonts
# lack full Unicode (identity for cairo). Kept separate from the device set-up
# so it can also target other devices (e.g. PNG previews).
dta_render_markdown <- function(lines, ascii = function(x) x) {
  lines <- as.character(lines)
  if (length(lines) == 0) lines <- ""
  op <- graphics::par(mar = c(0, 0, 0, 0), family = "sans")
  on.exit(graphics::par(op), add = TRUE)

  pageW <- 8.27
  pageH <- 11.69
  mL <- 0.85
  mR <- 0.85
  mT <- 0.9
  mB <- 0.85
  contentW <- pageW - mL - mR
  y <- NA_real_

  new_page <- function() {
    graphics::plot.new()
    graphics::plot.window(
      xlim = c(0, pageW), ylim = c(0, pageH),
      xaxs = "i", yaxs = "i", asp = NA
    )
    y <<- pageH - mT
  }
  new_page()

  lh <- function(cex) graphics::strheight("Ag", cex = cex, font = 1) * 1.35
  sw <- function(s, cex, font) graphics::strwidth(s, cex = cex, font = font)
  ensure <- function(space) if (y - space < mB) new_page()

  break_token <- function(tok, cex, font, maxw) {
    if (sw(tok, cex, font) <= maxw || !nzchar(tok)) {
      return(tok)
    }
    out <- character(0)
    cur <- ""
    for (ch in strsplit(tok, "")[[1]]) {
      if (nzchar(cur) && sw(paste0(cur, ch), cex, font) > maxw) {
        out <- c(out, cur)
        cur <- ch
      } else {
        cur <- paste0(cur, ch)
      }
    }
    if (nzchar(cur)) out <- c(out, cur)
    out
  }

  # Draw a list of words (each list(text=, bold=)) with word-wrapping and proper
  # per-word bold/italic; advances the y cursor.
  draw_words <- function(words, cex = 1, indent = 0, italic = FALSE, color = "black") {
    startX <- mL + indent
    maxX <- mL + contentW
    spaceW <- sw(" ", cex, 1)
    step <- lh(cex)
    ensure(step)
    x <- startX
    started <- FALSE
    for (w in words) {
      font <- if (italic) 3 else if (isTRUE(w$bold)) 2 else 1
      pieces <- break_token(ascii(w$text), cex, font, maxX - startX)
      for (pi in seq_along(pieces)) {
        piece <- pieces[[pi]]
        ww <- sw(piece, cex, font)
        if (started && x + ww > maxX + 1e-9) {
          y <<- y - step
          ensure(step)
          x <- startX
        }
        graphics::text(x, y, piece, adj = c(0, 1), cex = cex, font = font, col = color)
        x <- x + ww + spaceW
        started <- TRUE
        if (pi < length(pieces)) {
          y <<- y - step
          ensure(step)
          x <- startX
        }
      }
    }
    y <<- y - step
  }

  # Split a line into **bold** / normal word tokens.
  parse_rich <- function(text) {
    segs <- list()
    rest <- text
    while (nzchar(rest)) {
      m <- regexpr("\\*\\*(.+?)\\*\\*", rest, perl = TRUE)
      if (m[1] == -1L) {
        segs[[length(segs) + 1L]] <- list(text = rest, bold = FALSE)
        break
      }
      before <- substr(rest, 1L, m[1] - 1L)
      if (nzchar(before)) segs[[length(segs) + 1L]] <- list(text = before, bold = FALSE)
      chunk <- substr(rest, m[1], m[1] + attr(m, "match.length") - 1L)
      segs[[length(segs) + 1L]] <- list(text = gsub("^\\*\\*|\\*\\*$", "", chunk), bold = TRUE)
      rest <- substr(rest, m[1] + attr(m, "match.length"), nchar(rest))
    }
    words <- list()
    for (s in segs) {
      toks <- strsplit(s$text, "[ \t]+")[[1]]
      toks <- toks[nzchar(toks)]
      for (t in toks) words[[length(words) + 1L]] <- list(text = t, bold = s$bold)
    }
    words
  }

  wrap_text <- function(text, maxw, cex, font) {
    text <- ascii(text)
    if (!nzchar(text)) {
      return("")
    }
    toks <- strsplit(text, "[ \t]+")[[1]]
    toks <- toks[nzchar(toks)]
    if (length(toks) == 0) {
      return("")
    }
    out <- character(0)
    cur <- ""
    for (t in toks) {
      for (p in break_token(t, cex, font, maxw)) {
        cand <- if (nzchar(cur)) paste(cur, p) else p
        if (nzchar(cur) && sw(cand, cex, font) > maxw) {
          out <- c(out, cur)
          cur <- p
        } else {
          cur <- cand
        }
      }
    }
    if (nzchar(cur)) out <- c(out, cur)
    out
  }

  draw_heading <- function(level, text) {
    cex <- c(1.7, 1.4, 1.22, 1.08, 1.0, 0.95)[min(level, 6)]
    y <<- y - lh(cex) * c(0.35, 0.85, 0.65, 0.5, 0.4, 0.3)[min(level, 6)]
    text <- gsub("\\*\\*", "", text)
    words <- lapply(strsplit(text, "[ \t]+")[[1]], function(t) list(text = t, bold = TRUE))
    draw_words(words, cex = cex, color = "#1b3a5b")
    if (level <= 2) {
      graphics::segments(mL, y + lh(cex) * 0.08, mL + contentW, y + lh(cex) * 0.08,
        col = "#9bb3c9", lwd = if (level == 1) 1.2 else 0.7
      )
      y <<- y - lh(cex) * 0.28
    }
  }

  draw_hr <- function() {
    y <<- y - lh(1) * 0.35
    ensure(lh(1) * 0.2)
    graphics::segments(mL, y, mL + contentW, y, col = "#9bb3c9", lwd = 0.7)
    y <<- y - lh(1) * 0.45
  }

  draw_bullet <- function(text) {
    ensure(lh(1))
    graphics::text(mL + 0.08, y, ascii("\u2022"), adj = c(0, 1), cex = 1, font = 1, col = "#1b3a5b")
    draw_words(parse_rich(text), cex = 1, indent = 0.26)
  }

  parse_cells <- function(s) {
    s <- trimws(s)
    s <- sub("^\\|", "", s)
    s <- sub("\\|$", "", s)
    trimws(strsplit(s, "\\|")[[1]])
  }

  draw_table <- function(header, rows) {
    cex <- 0.9
    pad <- 0.06
    ncol <- length(header)
    if (ncol == 0) {
      return(invisible())
    }
    fit <- function(v, k) if (length(v) >= k) v[[k]] else ""
    natural <- numeric(ncol)
    for (r in c(list(header), rows)) {
      for (j in seq_len(ncol)) {
        natural[j] <- max(natural[j], sw(ascii(fit(r, j)), cex, 2))
      }
    }
    natural <- natural + 2 * pad
    if (sum(natural) > contentW) natural <- natural * (contentW / sum(natural))
    colw <- natural
    xl <- mL + c(0, cumsum(colw)[-ncol])
    draw_row <- function(cells, header = FALSE) {
      font <- if (header) 2L else 1L
      wr <- lapply(seq_len(ncol), function(j) wrap_text(fit(cells, j), colw[j] - 2 * pad, cex, font))
      nlin <- max(1L, vapply(wr, length, 1L))
      rowH <- nlin * lh(cex) + pad
      ensure(rowH)
      top <- y
      if (header) graphics::rect(mL, top - rowH, mL + sum(colw), top, col = "#e8eef4", border = NA)
      for (j in seq_len(ncol)) {
        ty <- top - pad
        for (ln in wr[[j]]) {
          graphics::text(xl[j] + pad, ty, ln, adj = c(0, 1), cex = cex, font = font)
          ty <- ty - lh(cex)
        }
      }
      graphics::rect(mL, top - rowH, mL + sum(colw), top, border = "#b9c7d6", lwd = 0.6)
      for (j in seq_len(ncol)[-1]) graphics::segments(xl[j], top, xl[j], top - rowH, col = "#b9c7d6", lwd = 0.6)
      y <<- top - rowH
    }
    y <<- y - lh(cex) * 0.2
    draw_row(header, header = TRUE)
    for (r in rows) draw_row(r, header = FALSE)
    y <<- y - lh(cex) * 0.35
  }

  is_sep <- function(s) grepl("^[[:space:]|:\\-]+$", s) && grepl("-", s)

  n <- length(lines)
  i <- 1L
  while (i <= n) {
    ln <- lines[[i]]
    if (grepl("^\\s*$", ln)) {
      y <- y - lh(1) * 0.5
      i <- i + 1L
      next
    }
    if (grepl("^\\s*\\|", ln) && i < n && is_sep(lines[[i + 1L]])) {
      header <- parse_cells(ln)
      j <- i + 2L
      rows <- list()
      while (j <= n && grepl("^\\s*\\|", lines[[j]])) {
        rows[[length(rows) + 1L]] <- parse_cells(lines[[j]])
        j <- j + 1L
      }
      draw_table(header, rows)
      i <- j
      next
    }
    if (grepl("^#{1,6}\\s", ln)) {
      lvl <- nchar(sub("^(#+)\\s.*$", "\\1", ln))
      draw_heading(lvl, sub("^#{1,6}\\s+", "", ln))
      i <- i + 1L
      next
    }
    if (grepl("^-{3,}\\s*$", ln)) {
      draw_hr()
      i <- i + 1L
      next
    }
    if (grepl("^\\s*[-*]\\s+\\S", ln)) {
      draw_bullet(sub("^\\s*[-*]\\s+", "", ln))
      i <- i + 1L
      next
    }
    if (grepl("^\\*[^*].*\\*$", ln) && !grepl("\\*\\*", ln)) {
      txt <- sub("^\\*(.*)\\*$", "\\1", ln)
      words <- lapply(strsplit(trimws(txt), "[ \t]+")[[1]], function(t) list(text = t, bold = FALSE))
      draw_words(words, cex = 0.9, italic = TRUE, color = "#555555")
      i <- i + 1L
      next
    }
    draw_words(parse_rich(ln), cex = 1)
    i <- i + 1L
  }
  invisible()
}

# Robust PDF export (the "Export as PDF" option behind the "Export DTA" button):
# build the DOCX first (always works via officer), then
# convert with the best engine available and VERIFY a real PDF resulted. When no
# external converter exists we still produce a valid PDF from the Markdown export
# via R's own device -- so the export ALWAYS yields an openable PDF.
dta_export_pdf <- function(dta, file, signature_list = NULL) {
  docx <- tempfile(fileext = ".docx")
  on.exit(unlink(docx), add = TRUE)
  DTAtools::write_dta(
    dta,
    file = docx, format = "docx", overwrite = TRUE,
    include_signatures = !is.null(signature_list),
    signature_list = signature_list, quiet = TRUE
  )

  # 1) LibreOffice / soffice (best fidelity to the DOCX layout).
  soffice <- dta_find_soffice()
  if (nzchar(soffice) && dta_soffice_to_pdf(soffice, docx, file)) {
    return(invisible(file))
  }
  # 2) pandoc, but only when a PDF engine (LaTeX/typst/wkhtmltopdf) is present.
  if (dta_pandoc_to_pdf(docx, file)) {
    return(invisible(file))
  }

  # 3) Self-contained fallback: render the Markdown export to a real PDF with
  #    R's built-in device (no external tools). Guarantees a working download.
  md <- tempfile(fileext = ".md")
  on.exit(unlink(md), add = TRUE)
  ok_md <- tryCatch(
    {
      DTAtools::write_dta(
        dta,
        file = md, format = "md", overwrite = TRUE,
        include_signatures = !is.null(signature_list),
        signature_list = signature_list, quiet = TRUE
      )
      TRUE
    },
    error = function(e) FALSE
  )
  lines <- if (isTRUE(ok_md)) {
    tryCatch(readLines(md, warn = FALSE), error = function(e) character(0))
  } else {
    character(0)
  }
  if (length(lines) == 0) {
    ttl <- tryCatch(as.character(S7::prop(DTAtools::metadata(dta), "title"))[1],
      error = function(e) NULL
    )
    lines <- c(
      "# Data Transfer Agreement",
      if (!is.null(ttl) && nzchar(ttl)) paste0("**Title:** ", ttl) else NULL,
      paste0("**Generated:** ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      "",
      "(The full document layout could not be generated on this system.)"
    )
  }
  dta_markdown_to_pdf(lines, file)
  if (!dta_is_pdf(file)) {
    stop(paste0(
      "Could not produce a PDF on this system (no LibreOffice, LaTeX or ",
      "PDF-capable pandoc engine, and the built-in PDF device failed)."
    ))
  }
  invisible(file)
}

# Build a self-contained HTML validation SUMMARY for a validated DTA. Summarises
# per-dataset status (from the app's status map) and, when available, the
# per-target detail from results(). Returns a single HTML string.
#
# Distinct from DTAtools::write_validation_report(), which the Validation
# messages dock offers as "Report": that one lists the individual validation
# messages, this one certifies the overall per-dataset outcome.
dta_build_validation_report <- function(dta, status = NULL) {
  esc <- function(x) htmltools::htmlEscape(as.character(x %||% ""))
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  title <- tryCatch(
    {
      md <- dta@metadata
      t <- tryCatch(md@title, error = function(e) NULL)
      if (!is.null(t) && length(t) && nzchar(t)) t else NULL
    },
    error = function(e) NULL
  )

  st <- unlist(status %||% list())
  status_label <- function(s) {
    switch(s %||% "",
      pass = "Passed",
      fail = "Failed",
      nodata = "No data",
      pending = "Not validated",
      s %||% "Unknown"
    )
  }
  ds_names <- names(st)
  validated <- ds_names[st %in% c("pass", "fail")]
  n_pass <- sum(st[validated] == "pass")
  n_fail <- sum(st[validated] == "fail")
  n_nodata <- sum(st == "nodata")
  # Everything that was NOT validated: skipped for missing data, still pending,
  # or any status the app grows later. These are what make a run incomplete.
  n_unvalidated <- length(st) - length(validated)
  n_pending <- n_unvalidated - n_nodata

  res <- dta_try(as.data.frame(DTAtools::results(dta)))
  rdf <- if (isTRUE(res$ok)) res$value else NULL

  ds_rows <- paste0(vapply(ds_names, function(nm) {
    # Provably present today (ds_names IS names(st)), routed through the helper
    # so the invariant is enforced rather than merely true.
    s <- dta_lookup(st, nm, "pending")
    cls <- if (identical(s, "pass")) "ok" else if (identical(s, "fail")) "bad" else "muted"
    paste0(
      "<tr><td>", esc(nm), "</td><td class='", cls, "'>",
      esc(status_label(s)), "</td></tr>"
    )
  }, character(1)), collapse = "")

  detail_html <- ""
  if (!is.null(rdf) && nrow(rdf) > 0) {
    # n_import_errors is the third validation axis: without it a table that
    # failed ONLY because a value was unrepresentable in its declared type shows
    # up in the report with two zero counts and no visible reason.
    want <- intersect(
      c(
        "dataset", "target", "status", "n_columnspec_errors",
        "n_rule_errors", "n_import_errors", "validated_at"
      ),
      names(rdf)
    )
    if (length(want) > 0) {
      head_cells <- paste0(vapply(
        want, function(h) paste0("<th>", esc(h), "</th>"),
        character(1)
      ), collapse = "")
      body_rows <- paste0(vapply(seq_len(nrow(rdf)), function(i) {
        cells <- paste0(vapply(
          want, function(h) paste0("<td>", esc(rdf[[h]][i]), "</td>"),
          character(1)
        ), collapse = "")
        paste0("<tr>", cells, "</tr>")
      }, character(1)), collapse = "")
      detail_html <- paste0(
        "<h2>Per-target detail</h2><table><thead><tr>", head_cells,
        "</tr></thead><tbody>", body_rows, "</tbody></table>"
      )
    }
  }

  # Three-state banner. "Passed" is reserved for the case where EVERY dataset in
  # the DTA was actually validated and passed -- a dataset skipped for missing
  # data leaves the run incomplete, and calling that a pass overstates it.
  banner_cls <- if (n_fail > 0) {
    "warn"
  } else if (n_pass > 0 && n_unvalidated == 0) {
    "pass"
  } else {
    "incomplete"
  }
  banner_txt <- switch(banner_cls,
    warn = "VALIDATION FAILED",
    pass = "VALIDATION PASSED",
    "VALIDATION INCOMPLETE"
  )
  summary_line <- paste0(
    n_pass, " passed, ", n_fail, " failed, ",
    n_nodata, " without data",
    if (n_pending > 0) paste0(", ", n_pending, " not validated") else "",
    "."
  )
  # The caveat points at the table below it, so it is only worth printing when
  # that table actually has rows to point at.
  caveat <- if (identical(banner_cls, "incomplete") && length(st) > 0) {
    paste0(
      "<div class='caveat'>Not every dataset was validated, so this DTA ",
      "cannot be reported as passed. Load the missing data and re-check the ",
      "datasets listed below as &ldquo;No data&rdquo; or &ldquo;Not ",
      "validated&rdquo;.</div>"
    )
  } else {
    ""
  }
  subtitle <- if (!is.null(title)) {
    paste0("<div class='subtitle'>", esc(title), "</div>")
  } else {
    ""
  }

  css <- paste0(
    "body{font-family:Segoe UI,Arial,sans-serif;color:#222;margin:32px}",
    "h1{font-size:22px;margin:0 0 4px}h2{font-size:16px;margin:22px 0 8px}",
    ".subtitle{font-size:15px;color:#444;margin-bottom:8px}",
    ".meta{color:#666;font-size:13px;margin-bottom:16px}",
    ".banner{display:inline-block;padding:10px 18px;border-radius:8px;",
    "font-weight:700;letter-spacing:.04em}",
    ".banner.pass{background:#e6f4ea;color:#1e7e34;border:1px solid #2e7d32}",
    ".banner.warn{background:#fdecea;color:#b71c1c;border:1px solid #c62828}",
    ".banner.incomplete{background:#fcf4e6;color:#8a6d3b;border:1px solid #c77700}",
    ".summary{margin:10px 0 4px;font-size:14px}",
    ".caveat{margin:8px 0 0;font-size:14px;color:#8a6d3b;max-width:52em}",
    "table{border-collapse:collapse;font-size:13px;margin-top:6px}",
    "th,td{border:1px solid #ccc;padding:5px 10px;text-align:left}",
    "th{background:#f4f4f4}td.ok{color:#1e7e34;font-weight:600}",
    "td.bad{color:#b71c1c;font-weight:600}td.muted{color:#888}"
  )

  paste0(
    "<!doctype html><html lang='en'><head><meta charset='utf-8'>",
    "<title>DTA Validation Summary</title><style>", css, "</style></head><body>",
    "<h1>DTA Validation Summary</h1>", subtitle,
    "<div class='meta'>Generated ", esc(ts), "</div>",
    "<div class='banner ", banner_cls, "'>", banner_txt, "</div>",
    "<div class='summary'>", esc(summary_line), "</div>", caveat,
    "<h2>Datasets</h2><table><thead><tr><th>Dataset</th><th>Status</th></tr>",
    "</thead><tbody>", ds_rows, "</tbody></table>", detail_html,
    "</body></html>"
  )
}

# Supported dataset types (fallback to known set if internal symbol absent).
dta_supported_types <- function() {
  types <- tryCatch(
    get("__DTAtools_supported_dataset_types__", envir = asNamespace("DTAtools")),
    error = function(e) NULL
  )
  types %||% c("tabular", "file")
}

# Supported column-structure backends (drives the Edit-columns backend dropdown).
dta_supported_backends <- function() {
  b <- tryCatch(
    get("__DTAtools_supported_backends__", envir = asNamespace("DTAtools")),
    error = function(e) NULL
  )
  b %||% "SAS"
}

# Bare (backend-less) SAS storage types offered in the column editor.
dta_sas_types <- function() c("Char", "Num", "Int", "Date", "Time", "DateTime")

# Condition operators offered in the rule editor (mirrors evaluateRules()).
# Operators offered by the conditional-rule builder (IF / THEN). Named vector:
# names are the friendly labels shown in the UI, values are the keys the schema
# engine (evaluate_condition) understands. "min_max" is a UI-only convenience
# that the app expands into the engine's `min` + `max` keys.
dta_condition_operators <- function() {
  c(
    "equals" = "equals",
    "not equals" = "not_equals",
    "greater" = "greater",
    "greater or equal" = "greater_equal",
    "less" = "less",
    "less or equal" = "less_equal",
    "between (min/max)" = "min_max",
    "in (list)" = "in",
    "not in (list)" = "not_in",
    "matches pattern" = "pattern",
    "empty" = "empty"
  )
}

# ---- Serialization: DTA / DTADataSet -> YAML -----------------------------
# The DTAtools package ships write_columns_to_yaml() (collection only) but NO
# serializer for a whole DTA or a DTADataSet. These helpers build a plain list
# that mirrors the hand-written example YAMLs (proven to load) and emit it with
# yaml::as.yaml(). Columns are built MANUALLY (never via as.list()) because
# as.list(structure) yields "SAS " for a NULL format (paste("SAS", NULL)); here
# type/format/length are emitted only when actually present.

# Drop NULL and zero-length elements recursively so emitted YAML has no blanks.
.dta_compact <- function(x) {
  if (is.list(x)) {
    x <- lapply(x, .dta_compact)
    keep <- vapply(x, function(v) !(is.null(v) || length(v) == 0), logical(1))
    x <- x[keep]
    if (length(x) == 0) {
      return(NULL)
    }
  }
  x
}

# Convert any Date objects to "YYYY-MM-DD" strings (YAML has no Date scalar).
.dta_stringify_dates <- function(x) {
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }
  if (is.list(x)) {
    return(lapply(x, .dta_stringify_dates))
  }
  x
}

# Blank-line the sections of an emitted YAML document, for readability only.
#
# yaml::as.yaml() emits one unbroken block of text; a real specification runs to
# several hundred lines and reads as a wall. This separates `metadata:` from
# `datasets:`, each dataset from the next, and each of `files:`/`columns:`/
# `rules:` from what follows -- while leaving column entries, rule entries and
# `values:` lists tight, which is where blank lines would only add noise.
#
# It POST-PROCESSES the emitted string rather than pasting per-section chunks
# together. as.yaml() stays the single source of truth for quoting, escaping and
# indentation, this stays a pure character -> character function that can be
# tested on its own, and -- crucially -- the OTHER as.yaml() calls in this file
# are untouched: dta_match_handlers(), dta_handlers_signature() and
# dta_specs_signature() use the emitted text as an equality signature, not for
# display, and must stay byte-stable.
#
# `max_depth` is the deepest block that earns surrounding blank lines, counting
# a top-level key as depth 1. dta_to_yaml_text() passes 3 (metadata / datasets,
# then their children, then a dataset's own keys); dta_dataset_to_yaml_text()
# passes 1, because a standalone dataset document IS a `datasets:` entry hoisted
# to the root -- two levels shallower. Both views therefore lay out identical
# dataset content identically.
#
# Depth is tracked with a stack rather than computed as indent / 2, because
# as.yaml() renders a sequence at the SAME indent as its key -- `columns:` and
# its `- id:` entries are both indented two spaces -- so the arithmetic reading
# would collapse those two levels into one and blank-separate every column. A
# sequence entry therefore ranks half a level below a mapping key at its indent.
dta_yaml_blank_lines <- function(text, max_depth = 3L) {
  if (!is.character(text) || length(text) != 1L || is.na(text) || !nzchar(text)) {
    return(text)
  }
  if (max_depth < 1L) {
    return(text)
  }
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  if (length(lines) < 3L) {
    return(text)
  }
  indent_of <- function(s) attr(regexpr("^ *", s), "match.length")

  # Does this line open a block, or does it already carry its value inline?
  # `columns:` opens one; `description: some text` cannot -- in block style a
  # key with an inline value has no children, so anything indented below it is
  # the emitter FOLDING that value across lines.
  #
  # This asks about the line's OWN shape rather than trying to recognise a
  # continuation, because a continuation cannot be recognised: `and then Note:
  # something` wrapped off the end of a description is indistinguishable from a
  # mapping entry, and any test that reads it as one splits a user's prose.
  # A plain value that would end in a colon is quoted by the emitter, so the
  # trailing-colon test is not fooled by `label: 'Ratio:'`.
  opens_children <- function(body) {
    grepl(":[ \t]*$", sub("^(- )*", "", body))
  }
  # The header of a block scalar: `|` or `>`, then an indentation indicator and
  # a chomping indicator IN EITHER ORDER (the spec allows both, and as.yaml
  # writes `|2-` whenever the first content line begins with a space). Anchored
  # on whitespace so a plain value that merely ENDS in one of those characters
  # -- `pattern: .*>` -- is not mistaken for one.
  opens_block_scalar <- function(body) {
    grepl("(^|[ \t])[|>]([0-9]+[-+]?|[-+][0-9]*)?[ \t]*$", body)
  }

  # -- Group the lines into units. A unit is one structural line, plus every
  # line that belongs to its value rather than to the structure.
  units <- list()
  i <- 1L
  n <- length(lines)
  while (i <= n) {
    line <- lines[[i]]
    if (!nzchar(trimws(line))) {
      # as.yaml() never emits a blank line, so this text is either already laid
      # out or came from somewhere else. Either way it is not ours to respace.
      return(text)
    }
    ind <- indent_of(line)
    body <- substring(line, ind + 1L)
    dashes <- attr(regexpr("^(- )*", body), "match.length")
    floor_ind <- ind + dashes
    own <- line
    scalar <- opens_block_scalar(body)
    i <- i + 1L
    if (scalar) {
      # A block scalar's body is opaque: it may contain blank lines and lines
      # that look like keys. Carry it through untouched -- a blank line inserted
      # INSIDE one changes the value, not the layout.
      while (i <= n &&
        (!nzchar(trimws(lines[[i]])) || indent_of(lines[[i]]) > floor_ind)) {
        own <- c(own, lines[[i]])
        i <- i + 1L
      }
    } else if (!opens_children(body)) {
      # A folded value's continuations are indented exactly like children would
      # be. Reading them as structure brackets a long `description:` with blank
      # lines it never asked for, and splits any prose containing a colon.
      while (i <= n && indent_of(lines[[i]]) > floor_ind) {
        own <- c(own, lines[[i]])
        i <- i + 1L
      }
    }
    units[[length(units) + 1L]] <- list(
      rank = 2L * ind + as.integer(dashes > 0L),
      scalar = scalar,
      lines = own
    )
  }
  m <- length(units)
  if (m < 2L) {
    return(text)
  }
  ranks <- vapply(units, function(u) u$rank, integer(1))

  # -- Depth of each unit, and whether it has anything nested under it. Only a
  # block that spans several lines earns blank lines around it.
  depth <- integer(m)
  nested <- vapply(units, function(u) isTRUE(u$scalar), logical(1))
  stack <- integer(0)
  for (j in seq_len(m)) {
    while (length(stack) > 0L && ranks[[stack[[length(stack)]]]] >= ranks[[j]]) {
      stack <- stack[-length(stack)]
    }
    depth[[j]] <- length(stack) + 1L
    if (length(stack) > 0L) nested[[stack[[length(stack)]]]] <- TRUE
    stack <- c(stack, j)
  }
  spaced <- nested & depth <= max_depth

  # -- Emit. A blank goes before a spaced block and after one ends; both reduce
  # to "before this unit", so at most one is ever inserted. A unit that is the
  # FIRST child of its parent closes nothing, and gets no blank -- `datasets:`
  # stays flush against the dataset that opens it.
  chunks <- vector("list", m)
  stack <- integer(0)
  for (j in seq_len(m)) {
    gap <- FALSE
    closed <- FALSE
    while (length(stack) > 0L && ranks[[stack[[length(stack)]]]] >= ranks[[j]]) {
      if (spaced[[stack[[length(stack)]]]]) gap <- TRUE
      stack <- stack[-length(stack)]
      closed <- TRUE
    }
    if (closed && spaced[[j]]) gap <- TRUE
    chunks[[j]] <- if (gap && j > 1L) c("", units[[j]]$lines) else units[[j]]$lines
    stack <- c(stack, j)
  }
  out <- unlist(chunks, use.names = FALSE)
  res <- paste0(
    paste(out, collapse = "\n"),
    if (endsWith(text, "\n")) "\n" else ""
  )

  # -- Guard. Nothing but blank lines may have changed; if anything else did,
  # hand back the original. Losing the layout is a nuisance, losing a
  # specification is not, and this is cosmetic either way.
  #
  # The re-parse is UNCONDITIONAL, and deliberately so. An earlier version ran
  # it only where a block scalar had been detected -- which left the one case
  # that can actually corrupt a value guarded by the very detection that would
  # have to fail for it to arise. Reading the document back costs less than a
  # millisecond at any size this app serializes, so it is not worth being
  # clever about. The line comparison above stays as a cheaper first rejection;
  # on its own it proves only that no line was dropped or reordered, not that a
  # blank landed somewhere harmless.
  if (!identical(out[nzchar(out)], lines[nzchar(lines)])) {
    return(text)
  }
  same <- tryCatch(
    identical(yaml::yaml.load(res), yaml::yaml.load(text)),
    error = function(e) FALSE
  )
  if (!isTRUE(same)) {
    return(text)
  }
  res
}

# Recover a handler's file-type token ("csv" / "tsv") from its S7 class.
#
# KNOWN GAP, not reachable from the app: a DTAFileDelim reports "csv", which
# discards its `sep`. DTAFileFactory() -- the only route from a document back to
# an object -- implements csv and tsv only, so no YAML can produce a
# DTAFileDelim and the file editor deliberately does not offer one
# (dta_handler_types()). A Delim handler can therefore only arrive here from an
# object built in R and handed to the app. Giving it its own token would make
# every document containing one unreadable instead; the real fix is a `delim`
# branch in DTAFileFactory, at which point this should return "delim".
dta_handler_type <- function(h) {
  if (inherits(h, "DTAtools::DTAFileAny")) {
    return("any")
  }
  if (inherits(h, "DTAtools::DTAFileTSV")) {
    return("tsv")
  }
  if (inherits(h, "DTAtools::DTAFileCSV")) {
    return("csv")
  }
  "csv"
}

# One file handler -> plain list (mirrors the `files:` block of the examples).
dta_handler_to_list <- function(h) {
  out <- list()
  fn <- tryCatch(h@filename, error = function(e) NULL)
  if (!is.null(fn) && length(fn) > 0) {
    out$filename <- if (length(fn) == 1) fn else as.list(fn)
  }
  out$type <- dta_handler_type(h)
  if (isTRUE(tryCatch(h@pattern, error = function(e) FALSE))) out$pattern <- TRUE
  mn <- handler_min(h)
  mx <- handler_max(h)
  if (!is.na(mn) && !is.na(mx) && mn == mx) {
    out$number_of_files <- as.integer(mn)
  } else {
    if (!is.na(mn)) out$min_number_of_files <- as.integer(mn)
    if (!is.na(mx)) out$max_number_of_files <- as.integer(mx)
  }
  pd <- tryCatch(h@pattern_description, error = function(e) NULL)
  if (!is.null(pd) && length(pd) > 0 && any(nzchar(pd))) out$pattern_description <- pd
  info <- tryCatch(h@info, error = function(e) NULL)
  if (!is.null(info) && length(info) > 0) out$info <- info
  ext <- tryCatch(h@extensions, error = function(e) NULL)
  if (!is.null(ext) && length(ext) > 0) {
    out$extensions <- if (length(ext) == 1) ext else as.list(ext)
  }
  out
}

# One DTAColumnSpec -> plain list. Built by hand (see note above).
dta_column_to_list <- function(col) {
  g <- function(slot) tryCatch(S7::prop(col, slot), error = function(e) NULL)
  st <- tryCatch(col@structure, error = function(e) NULL)
  out <- list()
  out$id <- g("id")
  lbl <- g("label")
  if (!is.null(lbl) && nzchar(lbl)) out$label <- lbl
  if (!is.null(st)) {
    backend <- tryCatch(st@backend, error = function(e) NULL) %||% "SAS"
    ty <- tryCatch(st@type, error = function(e) NULL)
    fmt <- tryCatch(st@format, error = function(e) NULL)
    len <- tryCatch(st@length, error = function(e) NULL)
    if (!is.null(ty) && length(ty) && nzchar(ty)) out$type <- paste(backend, ty)
    if (!is.null(fmt) && length(fmt) && nzchar(fmt)) out$format <- paste(backend, fmt)
    if (!is.null(len) && length(len) && !is.na(len)) out$length <- as.integer(len)
  }
  nl <- g("nullable")
  if (!is.null(nl) && length(nl) > 0) out$nullable <- isTRUE(nl)
  vals <- g("values")
  if (!is.null(vals) && length(vals) > 0) out$values <- vals
  ex <- g("examples")
  if (!is.null(ex) && length(ex) > 0) out$examples <- ex
  patt <- g("pattern")
  if (!is.null(patt) && length(patt) && nzchar(patt)) out$pattern <- patt
  cc <- g("colclass")
  if (!is.null(cc) && length(cc) > 0) out$colclass <- cc
  desc <- g("description")
  if (!is.null(desc) && length(desc) && nzchar(desc)) out$description <- desc
  out
}

# One DTARule -> plain list, with the SHORT type token and `columns` (plural).
dta_rule_to_list <- function(rule) {
  raw <- tryCatch(as.list(rule), error = function(e) NULL)
  if (is.null(raw)) {
    return(NULL)
  }
  ty <- raw$type %||% ""
  ty <- switch(ty,
    check_col_condition = ,
    col_condition = "col_condition",
    check_range = ,
    col_range = "col_range",
    check_unique = ,
    col_unique = "col_unique",
    check_group_condition = ,
    group_condition = "group_condition",
    ty
  )
  out <- list(id = raw$id, type = ty)
  if (!is.null(raw$description) && length(raw$description) && nzchar(raw$description)) {
    out$description <- raw$description
  }
  if (identical(ty, "col_condition")) {
    out$condition <- raw$condition
    out$then <- raw$then
  } else if (identical(ty, "col_range")) {
    out$columns <- raw$columns
    if (!is.null(raw$min)) out$min <- raw$min
    if (!is.null(raw$max)) out$max <- raw$max
  } else if (identical(ty, "col_unique")) {
    out$columns <- raw$columns
  } else if (identical(ty, "group_condition")) {
    out$group_by <- raw$group_by
    out$conditions <- raw$conditions
    out$constraints <- raw$constraints
  }
  out
}

# Human-friendly label for a short rule-type token (used by the rule editor and
# the inspect popup).
dta_rule_type_label <- function(type) {
  t <- type %||% ""
  switch(t,
    col_condition = "Conditional (IF/THEN)",
    col_range = "Range",
    col_unique = "Unique",
    group_condition = "Grouped condition",
    if (nzchar(t)) t else "\u2014"
  )
}

# One DTADataSet -> plain list (standalone-dataset shape).
dta_dataset_to_list <- function(ds) {
  if (is.null(ds)) {
    return(list())
  }
  out <- list()
  out$name <- tryCatch(ds@name, error = function(e) NULL)
  out$type <- tryCatch(ds@type, error = function(e) NULL) %||% "tabular"
  desc <- tryCatch(ds@description, error = function(e) NULL)
  if (!is.null(desc) && length(desc) && nzchar(desc)) out$description <- desc
  ts <- tryCatch(ds@template_source, error = function(e) NULL)
  if (!is.null(ts) && length(ts) && nzchar(ts)) out$template_source <- ts
  tv <- tryCatch(ds@template_version, error = function(e) NULL)
  if (!is.null(tv) && length(tv) && nzchar(tv)) out$template_version <- tv
  td <- tryCatch(ds@template_date, error = function(e) NULL)
  if (!is.null(td) && length(td) > 0) out$template_date <- .dta_stringify_dates(td)
  handlers <- dta_handlers(ds)
  if (length(handlers) == 1) {
    out$files <- dta_handler_to_list(handlers[[1]])
  } else if (length(handlers) > 1) {
    out$files <- unname(lapply(handlers, dta_handler_to_list))
  }
  cols <- tryCatch(ds@specs@columns, error = function(e) NULL)
  if (!is.null(cols) && length(cols) > 0) {
    out$columns <- unname(lapply(cols, dta_column_to_list))
  }
  rules <- tryCatch(ds@specs@rules, error = function(e) NULL)
  if (!is.null(rules) && length(rules) > 0) {
    out$rules <- unname(Filter(Negate(is.null), lapply(rules, dta_rule_to_list)))
  }
  out
}

# Whole DTA -> plain list (metadata + datasets array).
dta_to_list <- function(dta) {
  out <- list()
  md <- tryCatch(DTAtools::metadata(dta), error = function(e) NULL)
  if (!is.null(md)) {
    ml <- tryCatch(as.list(md), error = function(e) NULL)
    ml <- .dta_compact(.dta_stringify_dates(ml))
    if (!is.null(ml) && length(ml) > 0) out$metadata <- ml
  }
  names_ds <- dta_dataset_names(dta)
  if (length(names_ds) > 0) {
    out$datasets <- unname(lapply(names_ds, function(nm) {
      .dta_compact(dta_dataset_to_list(dta_get_dataset(dta, nm)))
    }))
  }
  out
}

# Serialize a whole DTA to a YAML string. Returns dta_try() (value = character).
dta_to_yaml_text <- function(dta) {
  dta_try({
    lst <- .dta_compact(dta_to_list(dta))
    dta_yaml_blank_lines(
      yaml::as.yaml(lst %||% list(), indent = 2, line.sep = "\n"),
      max_depth = 3L
    )
  })
}

# Serialize ONE dataset to a YAML string (standalone-dataset shape).
dta_dataset_to_yaml_text <- function(dta, dataset) {
  dta_try({
    ds <- dta_get_dataset(dta, dataset)
    if (is.null(ds)) stop(sprintf("Dataset '%s' not found.", dataset))
    lst <- .dta_compact(dta_dataset_to_list(ds))
    # max_depth 1, not 3: this document is a `datasets:` entry hoisted to the
    # root, so the same keys sit two levels shallower than in the full document.
    dta_yaml_blank_lines(
      yaml::as.yaml(lst %||% list(), indent = 2, line.sep = "\n"),
      max_depth = 1L
    )
  })
}

# ---- Dataset metadata editing --------------------------------------------
# The DTADataSet-level properties -- name, description and the three template_*
# fields -- as opposed to the DOCUMENT-level DTAMetaData handled by
# dta_set_metadata_field() above. Both are called "metadata"; they are different
# objects, and only these belong to a single dataset.
#
# `type` is deliberately absent from both helpers. It is fixed by the concrete
# class (DTADataSetTabular's constructor hardcodes "tabular",
# DTADataSetFile's "file"), but the property itself is a plain character whose
# validator only checks set membership -- so `ds@type <- "file"` on a tabular
# dataset SUCCEEDS and yields an object that claims to be file-backed while
# still carrying @specs, @tables and @import_issues. Everything downstream
# dispatches on the S7 class, so such an object behaves as tabular while every
# document it generates says otherwise. Changing a type means rebuilding the
# dataset in the other class, not assigning a field, so no helper here offers
# it and the editor shows no control for it.

# The editable fields of one dataset, as a plain list the editor form pre-fills
# from. NULL when the dataset does not exist. Every element is a length-1
# character, "" when the property is unset -- so the form never has to reason
# about NULL.
dta_dataset_meta_fields <- function(dta, dataset) {
  ds <- dta_get_dataset(dta, dataset)
  if (is.null(ds)) {
    return(NULL)
  }
  g <- function(prop) {
    v <- tryCatch(S7::prop(ds, prop), error = function(e) NULL)
    if (is.null(v) || length(v) == 0) {
      return("")
    }
    as.character(v)[1]
  }
  list(
    name = g("name"),
    type = g("type"),
    description = g("description"),
    template_source = g("template_source"),
    template_version = g("template_version"),
    template_date = g("template_date")
  )
}

# Update one dataset's metadata, returning dta_try() whose value is the updated
# DTA.
#
# A blank optional field UNSETS the property (NULL) rather than storing "",
# matching dta_set_metadata_field()'s rule for the document-level fields: an
# empty field means "not set at all", and .dta_compact() then omits it from the
# serialized YAML entirely. `name` is not optional -- the DTADataSet validator
# rejects an empty one -- so it is required here.
#
# RENAMING RE-KEYS THE DATASET LIST IN PLACE. `dta@datasets` is a named list and
# datasets(dta, name) looks up by name, so the entry has to move; but every
# upload slot, example picker and nav button in the app is keyed by the
# dataset's POSITION and resolves its name only at click time. The obvious
# `datasets[[old]] <- NULL; datasets[[new]] <- ds` moves the dataset to the end
# of the list, after which those controls silently address the wrong dataset.
# The replace-in-place below is what keeps them correct.
dta_set_dataset_meta <- function(dta, dataset, name, description = NULL,
                                 template_source = NULL, template_version = NULL,
                                 template_date = NULL) {
  dta_try({
    nm <- trimws(as.character(name %||% "")[1])
    if (is.na(nm) || !nzchar(nm)) stop("A dataset name is required.")

    # Resolved by position rather than through datasets(dta, dataset): the
    # position is needed anyway for the in-place replacement below, and this
    # reports a missing dataset as one plain sentence instead of surfacing the
    # generic's own abort.
    all_names <- names(dta@datasets) %||% character(0)
    pos <- match(dataset, all_names)
    if (is.na(pos)) stop(sprintf("Dataset '%s' not found.", dataset))
    ds <- dta@datasets[[pos]]

    # Without this the named-list assignment below would overwrite the other
    # dataset outright, destroying it with no error and no warning.
    if (nm %in% all_names[-pos]) {
      stop(sprintf("A dataset named '%s' already exists.", nm))
    }

    blank_to_null <- function(x) {
      if (is.null(x) || length(x) == 0) {
        return(NULL)
      }
      v <- trimws(as.character(x)[1])
      if (is.na(v) || !nzchar(v)) NULL else v
    }

    ds@name <- nm
    ds@description <- blank_to_null(description)
    ds@template_source <- blank_to_null(template_source)
    ds@template_version <- blank_to_null(template_version)
    ds@template_date <- blank_to_null(template_date)

    dsets <- dta@datasets
    dsets[[pos]] <- ds
    names(dsets)[pos] <- nm
    dta@datasets <- dsets
    dta
  })
}

# ---- Dataset add/remove ---------------------------------------------------
# Whole-dataset lifecycle: creating a new, empty dataset and deleting an
# existing one. Both return dta_try() whose value is the updated DTA, and both
# mutate `dta@datasets` -- the same named list dta_set_dataset_meta() above
# re-keys in place for a rename.

# Create a new, empty dataset of the given `type` and append it to `dta`.
#
# `type` is a CREATION-TIME choice only, never revisited afterwards -- see the
# comment above dta_set_dataset_meta() (~L1629-1637) for why the property
# itself cannot be trusted once a dataset exists: assigning `ds@type` passes
# its validator without changing the S7 class, so nothing downstream would
# dispatch correctly on the new value. That is exactly why no helper in this
# file offers a way to change an existing dataset's type -- the only route to
# a different type is to add a new dataset and remove the old one.
#
# APPENDED AT THE END, never inserted anywhere else. Every nav button, upload
# slot and example picker in the app (app.R:774-799 and nearby) is keyed by
# the dataset's POSITION in `dta@datasets`, resolving the name only at click
# time -- so inserting ahead of an existing entry would silently repoint those
# controls at the wrong dataset. Appending is the one mutation that cannot do
# that: every existing index keeps meaning exactly what it meant before. Same
# reasoning as the in-place re-key in dta_set_dataset_meta() above; there it
# holds ONE index steady across a rename, here it holds every OTHER index
# steady across the list's growth.
dta_add_dataset <- function(dta, name, type = "tabular", description = NULL) {
  dta_try({
    nm <- trimws(as.character(name %||% "")[1])
    if (is.na(nm) || !nzchar(nm)) stop("A dataset name is required.")

    all_names <- names(dta@datasets) %||% character(0)
    if (nm %in% all_names) {
      stop(sprintf("A dataset named '%s' already exists.", nm))
    }

    type <- tolower(trimws(as.character(type %||% "")[1]))
    if (!type %in% c("tabular", "file")) {
      stop(sprintf(
        "Dataset type must be one of: %s.",
        paste(c("tabular", "file"), collapse = ", ")
      ))
    }

    blank_to_null <- function(x) {
      if (is.null(x) || length(x) == 0) {
        return(NULL)
      }
      v <- trimws(as.character(x)[1])
      if (is.na(v) || !nzchar(v)) NULL else v
    }
    desc <- blank_to_null(description)

    ds <- if (identical(type, "tabular")) {
      DTAtools::DTADataSetTabular(
        name = nm,
        specs = DTAtools::DTAColumnSpecCollection(columns = list()),
        description = desc
      )
    } else {
      DTAtools::DTADataSetFile(name = nm, description = desc)
    }

    dsets <- dta@datasets
    dsets[[nm]] <- ds
    dta@datasets <- dsets
    dta
  })
}

# Remove one dataset from `dta`, resolved by position exactly like
# dta_set_dataset_meta() resolves its `dataset` argument.
#
# Removing the LAST remaining dataset is ALLOWED, not refused: the app's
# output$main renders the workspace whenever rv$structure is non-NULL, and
# build_structure() returns list() -- not NULL -- once there are zero
# datasets, so the workspace UI survives an empty DTA rather than falling back
# to the landing page.
dta_remove_dataset <- function(dta, dataset) {
  dta_try({
    all_names <- names(dta@datasets) %||% character(0)
    pos <- match(dataset, all_names)
    if (is.na(pos)) stop(sprintf("Dataset '%s' not found.", dataset))

    dsets <- dta@datasets
    dsets[[pos]] <- NULL
    dta@datasets <- dsets
    dta
  })
}

# ---- Column editing ------------------------------------------------------

dta_column_ids <- function(dta, dataset) {
  ds <- dta_get_dataset(dta, dataset)
  cols <- tryCatch(ds@specs@columns, error = function(e) NULL) %||% list()
  names(cols) %||% vapply(cols, function(c) tryCatch(c@id, error = function(e) ""), character(1))
}

# A compact data.frame overview of a dataset's columns (for the editor table).
dta_columns_overview <- function(dta, dataset) {
  ds <- dta_get_dataset(dta, dataset)
  cols <- tryCatch(ds@specs@columns, error = function(e) NULL) %||% list()
  if (length(cols) == 0) {
    return(data.frame())
  }
  do.call(rbind, lapply(cols, function(c) {
    l <- dta_column_to_list(c)
    constraint <- if (!is.null(l$values)) {
      paste(unlist(l$values), collapse = ", ")
    } else if (!is.null(l$pattern)) {
      paste0("/", l$pattern, "/")
    } else {
      ""
    }
    data.frame(
      id = l$id %||% "",
      label = l$label %||% "",
      type = l$type %||% "",
      length = if (is.null(l$length)) "" else as.character(l$length),
      nullable = if (is.null(l$nullable)) "" else as.character(isTRUE(l$nullable)),
      constraint = constraint,
      description = l$description %||% "",
      stringsAsFactors = FALSE
    )
  }))
}

# Editable fields of a single column (backend/type/format split out).
dta_column_fields <- function(dta, dataset, id) {
  ds <- dta_get_dataset(dta, dataset)
  cols <- tryCatch(ds@specs@columns, error = function(e) NULL) %||% list()
  col <- cols[[id]]
  if (is.null(col)) {
    return(NULL)
  }
  l <- dta_column_to_list(col)
  bt <- l$type %||% ""
  backend <- "SAS"
  bare <- bt
  if (nzchar(bt) && grepl(" ", bt)) {
    sp <- strsplit(bt, " ", fixed = TRUE)[[1]]
    backend <- sp[1]
    bare <- paste(sp[-1], collapse = " ")
  }
  fmt <- l$format %||% ""
  if (nzchar(fmt) && grepl(" ", fmt)) fmt <- sub("^\\S+\\s+", "", fmt)
  list(
    id = l$id %||% "",
    label = l$label %||% "",
    backend = backend,
    type = bare,
    format = fmt,
    length = if (is.null(l$length)) "" else as.character(l$length),
    nullable = isTRUE(l$nullable),
    values = if (!is.null(l$values)) paste(unlist(l$values), collapse = "\n") else "",
    pattern = l$pattern %||% "",
    description = l$description %||% ""
  )
}

# Add or update a column (rename when old_id differs). Returns dta_try().
dta_set_column <- function(dta, dataset, id, label = NULL, backend = "SAS",
                           type = NULL, format = NULL, length = NULL,
                           nullable = NULL, values = NULL, pattern = NULL,
                           description = NULL, old_id = NULL) {
  dta_try({
    id <- trimws(as.character(id)[1] %||% "")
    if (!nzchar(id)) stop("A column id is required.")
    if (grepl("\\s", id)) stop("Column id cannot contain whitespace.")
    bk <- backend %||% "SAS"
    type_arg <- if (!is.null(type) && nzchar(type)) paste(bk, type) else NULL
    format_arg <- if (!is.null(format) && nzchar(format)) paste(bk, format) else NULL
    len_arg <- NULL
    if (!is.null(length) && length(length) && nzchar(as.character(length))) {
      len_arg <- suppressWarnings(as.integer(length))
      if (is.na(len_arg)) len_arg <- NULL
    }
    vals <- NULL
    if (!is.null(values) && length(values) > 0) {
      v <- trimws(values)
      v <- v[nzchar(v)]
      if (length(v) > 0) vals <- v
    }
    patt <- if (!is.null(pattern) && nzchar(pattern)) pattern else NULL
    lbl <- if (!is.null(label) && nzchar(label)) label else NULL
    desc <- if (!is.null(description) && nzchar(description)) description else NULL
    spec <- DTAtools::DTAColumnSpec(
      id = id, label = lbl, type = type_arg, format = format_arg,
      length = len_arg, nullable = nullable, pattern = patt, values = vals,
      description = desc
    )
    ds <- DTAtools::datasets(dta, dataset)
    specs <- ds@specs
    cols <- specs@columns %||% list()
    if (!is.null(old_id) && nzchar(old_id) && !identical(old_id, id)) cols[[old_id]] <- NULL
    cols[[id]] <- spec
    specs@columns <- cols
    ds@specs <- specs
    dta@datasets[[dataset]] <- ds
    dta
  })
}

dta_remove_column <- function(dta, dataset, id) {
  dta_try({
    ds <- DTAtools::datasets(dta, dataset)
    specs <- ds@specs
    cols <- specs@columns %||% list()
    cols[[id]] <- NULL
    specs@columns <- cols
    ds@specs <- specs
    dta@datasets[[dataset]] <- ds
    dta
  })
}

# Move a column one position up or down in the spec order. A move past either
# end is a no-op (the object is returned unchanged). Returns dta_try().
dta_move_column <- function(dta, dataset, id, direction) {
  dta_try({
    ds <- DTAtools::datasets(dta, dataset)
    specs <- ds@specs
    cols <- specs@columns %||% list()
    ids <- names(cols)
    pos <- match(id, ids)
    if (is.na(pos)) stop("Column not found.")
    n <- length(cols)
    target <- if (identical(direction, "up")) pos - 1L else pos + 1L
    if (target >= 1L && target <= n) {
      ord <- seq_len(n)
      ord[c(pos, target)] <- ord[c(target, pos)]
      specs@columns <- cols[ord]
      ds@specs <- specs
      dta@datasets[[dataset]] <- ds
    }
    dta
  })
}

# ---- Rule editing --------------------------------------------------------

# Render a nested condition/then list to a short, human-readable string.
.dta_cond_to_text <- function(cond) {
  if (is.null(cond) || length(cond) == 0) {
    return("")
  }
  parts <- vapply(names(cond), function(col) {
    spec <- cond[[col]]
    if (is.list(spec) && length(spec) > 0) {
      op <- names(spec)[1]
      val <- spec[[1]]
      sprintf("%s %s %s", col, op, paste(unlist(val), collapse = ", "))
    } else {
      col
    }
  }, character(1))
  paste(parts, collapse = "; ")
}

# A compact data.frame overview of a dataset's rules (for the editor table).
dta_rules_overview <- function(dta, dataset) {
  ds <- dta_get_dataset(dta, dataset)
  rules <- tryCatch(ds@specs@rules, error = function(e) NULL) %||% list()
  if (length(rules) == 0) {
    return(data.frame())
  }
  do.call(rbind, lapply(seq_along(rules), function(i) {
    l <- dta_rule_to_list(rules[[i]])
    detail <- if (identical(l$type, "col_condition")) {
      sprintf("IF %s THEN %s", .dta_cond_to_text(l$condition), .dta_cond_to_text(l$then))
    } else if (identical(l$type, "col_range")) {
      sprintf(
        "%s in [%s, %s]", paste(l$columns, collapse = ", "),
        l$min %||% "", l$max %||% ""
      )
    } else if (identical(l$type, "col_unique")) {
      sprintf("unique(%s)", paste(l$columns, collapse = ", "))
    } else if (identical(l$type, "group_condition")) {
      sprintf(
        "group(%s): %s condition(s), %s constraint(s)",
        paste(l$group_by %||% character(0), collapse = ", "),
        length(l$conditions %||% list()),
        length(l$constraints %||% list())
      )
    } else {
      ""
    }
    data.frame(
      index = i, id = l$id %||% "", type = l$type %||% "",
      detail = detail, description = l$description %||% "",
      stringsAsFactors = FALSE
    )
  }))
}

# Editable fields of a single rule (by 1-based index). Returns dta_rule_to_list().
dta_rule_fields <- function(dta, dataset, index) {
  ds <- dta_get_dataset(dta, dataset)
  rules <- tryCatch(ds@specs@rules, error = function(e) NULL) %||% list()
  if (index < 1 || index > length(rules)) {
    return(NULL)
  }
  dta_rule_to_list(rules[[index]])
}

# Build a DTARule from parts (dispatches on the short type token).
dta_build_rule <- function(id, type, description = NULL, condition = NULL,
                           then = NULL, columns = NULL, min = NULL, max = NULL,
                           group_by = NULL, conditions = NULL, constraints = NULL) {
  id <- trimws(as.character(id)[1] %||% "")
  if (!nzchar(id)) stop("A rule id is required.")
  if (grepl("\\s", id)) stop("Rule id cannot contain whitespace.")
  desc <- if (!is.null(description) && nzchar(description)) description else NULL
  ty <- switch(type,
    col_condition = ,
    check_col_condition = "col_condition",
    col_range = ,
    check_range = "col_range",
    col_unique = ,
    check_unique = "col_unique",
    group_condition = ,
    check_group_condition = "group_condition",
    type
  )
  if (identical(ty, "col_condition")) {
    DTAtools::DTARuleColCondition(
      id = id, description = desc,
      condition = condition, then = then
    )
  } else if (identical(ty, "col_range")) {
    DTAtools::DTARuleColRange(
      id = id, columns = columns, description = desc,
      min = min, max = max
    )
  } else if (identical(ty, "col_unique")) {
    DTAtools::DTARuleColUnique(id = id, columns = columns, description = desc)
  } else if (identical(ty, "group_condition")) {
    DTAtools::DTARuleGroupCondition(
      id = id,
      description = desc,
      group_by = group_by,
      conditions = conditions,
      constraints = constraints
    )
  } else {
    stop(sprintf("Unknown rule type '%s'.", type))
  }
}

# Add (index NULL) or replace (1-based index) a rule. Returns dta_try().
dta_set_rule <- function(dta, dataset, index = NULL, id, type, description = NULL,
                         condition = NULL, then = NULL, columns = NULL,
                         min = NULL, max = NULL,
                         group_by = NULL, conditions = NULL, constraints = NULL) {
  dta_try({
    rule <- dta_build_rule(
      id = id, type = type, description = description,
      condition = condition, then = then, columns = columns,
      min = min, max = max,
      group_by = group_by, conditions = conditions, constraints = constraints
    )
    ds <- DTAtools::datasets(dta, dataset)
    specs <- ds@specs
    rl <- specs@rules %||% list()
    if (is.null(index) || index < 1 || index > length(rl)) {
      rl[[length(rl) + 1L]] <- rule
    } else {
      rl[[index]] <- rule
    }
    specs@rules <- if (length(rl) == 0) NULL else rl
    ds@specs <- specs
    dta@datasets[[dataset]] <- ds
    dta
  })
}

dta_remove_rule <- function(dta, dataset, index) {
  dta_try({
    ds <- DTAtools::datasets(dta, dataset)
    specs <- ds@specs
    rl <- specs@rules %||% list()
    if (index >= 1 && index <= length(rl)) rl[[index]] <- NULL
    specs@rules <- if (length(rl) == 0) NULL else rl
    ds@specs <- specs
    dta@datasets[[dataset]] <- ds
    dta
  })
}

# Move a rule one position up or down. A move past either end is a no-op.
# @rules is a plain list (class_list_or_null), so we rebuild it via [[ to keep
# the reorder independent of any list-like wrapper. Returns dta_try().
dta_move_rule <- function(dta, dataset, index, direction) {
  dta_try({
    ds <- DTAtools::datasets(dta, dataset)
    specs <- ds@specs
    rl <- specs@rules %||% list()
    n <- length(rl)
    target <- if (identical(direction, "up")) index - 1L else index + 1L
    if (index >= 1L && index <= n && target >= 1L && target <= n) {
      items <- lapply(seq_len(n), function(i) rl[[i]])
      items[c(index, target)] <- items[c(target, index)]
      specs@rules <- if (length(items) == 0) NULL else items
      ds@specs <- specs
      dta@datasets[[dataset]] <- ds
    }
    dta
  })
}

# ---- File-handler editing -------------------------------------------------
# A dataset's file handlers (DTAFile objects in `ds@files`) are what the app
# shows as upload SLOTS. Unlike columns and rules they live in an UNNAMED,
# positional list, so every helper here addresses a handler by its 1-based
# index -- the same index the app uses in its upload keys ("<dataset>||<hi>").

# File types the editor can offer, per dataset type. This is the ONE list: the
# form builds its control from it and dta_set_handler() validates against it,
# so the two can never drift apart.
#
# A FILE dataset offers `any` and nothing else. DTADataSetFile exists to
# confirm that a deliverable arrived, is readable and is not empty -- it never
# reads a row -- so `csv` or `tsv` there would describe a parse that never
# happens, and invite a user to declare a PDF or an archive as something it is
# not.
#
# A TABULAR dataset is offered csv and tsv, deliberately narrower than the
# DTAFile class tree: DTAFileFactory() -- the only route from a YAML document
# back to an object -- implements csv and tsv only, so offering `delim` would
# create a handler that cannot be read back.
dta_handler_types <- function(dataset_type = "tabular") {
  if (identical(dataset_type, "file")) {
    "any"
  } else {
    c("csv", "tsv")
  }
}

# A compact data.frame overview of a dataset's file handlers (editor table).
dta_handlers_overview <- function(dta, dataset) {
  hs <- dta_handlers(dta_get_dataset(dta, dataset))
  if (length(hs) == 0) {
    return(data.frame())
  }
  do.call(rbind, lapply(hs, function(h) {
    data.frame(
      filename = handler_expected(h),
      type = dta_handler_type(h),
      pattern = if (handler_is_pattern(h)) "yes" else "no",
      files = handler_count_label(h),
      endings = handler_endings(h),
      description = handler_hint(h),
      stringsAsFactors = FALSE
    )
  }))
}

# Split a multi-line text-area value into trimmed, non-empty lines. Also
# tolerates an already-split character vector, so the same helper serves the
# editor's inputs and a direct programmatic call.
.dta_split_lines <- function(x) {
  parts <- unlist(strsplit(as.character(x %||% ""), "\n", fixed = TRUE))
  parts <- trimws(parts %||% character(0))
  parts[nzchar(parts)]
}

# A handler's `info` is free-form: the bundled specs use a YAML sequence of
# single-key mappings (`- data: smrnaseq`), others a plain list of strings. The
# editor shows one entry per line, so a KEYED entry has to render as "key: value"
# and parse back into the same mapping -- rendering it as a bare string would
# silently drop the key the moment the handler was saved.
.dta_info_to_lines <- function(info) {
  if (is.null(info) || length(info) == 0) {
    return(character(0))
  }
  unlist(lapply(seq_along(info), function(i) {
    entry <- info[[i]]
    nm <- names(info)[i]
    if (is.list(entry) && !is.null(names(entry))) {
      return(paste0(names(entry), ": ", unlist(entry)))
    }
    if (!is.null(nm) && nzchar(nm)) {
      return(paste0(nm, ": ", as.character(entry)))
    }
    as.character(entry)
  }), use.names = FALSE)
}

.dta_lines_to_info <- function(lines) {
  # Accepts either a character vector of lines or the single newline-joined
  # string dta_handler_fields() produces (and the textarea hands back), so a
  # fields -> set round trip cannot fold several entries into one value.
  lines <- .dta_split_lines(lines)
  if (length(lines) == 0) {
    return(NULL)
  }
  lapply(lines, function(line) {
    # POSIX classes, not \\s: inside a bracket expression R's default engine
    # reads \\s as the literal characters, so [^:\\s] would also exclude "s"
    # and a key like "class" would never match.
    m <- regmatches(
      line,
      regexec("^([^:[:space:]]+):[[:space:]]+(.*)$", line)
    )[[1]]
    if (length(m) == 3) stats::setNames(list(m[3]), m[2]) else line
  })
}

# Editable fields of a single handler, by index. NULL when out of bounds.
dta_handler_fields <- function(dta, dataset, index) {
  hs <- dta_handlers(dta_get_dataset(dta, dataset))
  index <- suppressWarnings(as.integer(index))[1]
  if (is.na(index) || index < 1 || index > length(hs)) {
    return(NULL)
  }
  h <- hs[[index]]
  fn <- tryCatch(h@filename, error = function(e) character(0)) %||% character(0)
  mn <- handler_min(h)
  mx <- handler_max(h)
  exact <- !is.na(mn) && !is.na(mx) && mn == mx
  info <- tryCatch(h@info, error = function(e) NULL)
  list(
    filename = paste(fn, collapse = "\n"),
    type = dta_handler_type(h),
    pattern = handler_is_pattern(h),
    count_mode = if (exact) "exact" else "range",
    number_of_files = if (exact) as.integer(mn) else 1L,
    min_number_of_files = if (is.na(mn)) 1L else as.integer(mn),
    max_number_of_files = if (is.na(mx)) 1L else as.integer(mx),
    pattern_description = tryCatch(h@pattern_description, error = function(e) NULL) %||% "",
    info = paste(.dta_info_to_lines(info), collapse = "\n"),
    extensions = paste(tryCatch(h@extensions, error = function(e) NULL) %||% character(0), collapse = ", ")
  )
}

# Add (index = NULL) or replace (index = i) one file handler.
#
# The count arguments mirror DTAFile's own contract, which this validates BEFORE
# constructing so the editor can report a sentence rather than surface a class
# validator's abort: a non-pattern handler matches exactly one literal name and
# must therefore expect exactly 1 file, and `number_of_files` may never be
# combined with a min/max range. Returns dta_try().
dta_set_handler <- function(dta, dataset, index = NULL, filename, type = "csv",
                            pattern = FALSE, count_mode = "exact",
                            number_of_files = 1, min_number_of_files = 1,
                            max_number_of_files = 1, pattern_description = NULL,
                            info = NULL, extensions = NULL, dataset_type = "tabular") {
  dta_try({
    # One name or pattern per line, so a multi-name handler survives the
    # fields -> form -> set round trip as several names rather than one.
    fn <- .dta_split_lines(filename)
    if (length(fn) == 0) stop("A file name or pattern is required.")

    type <- tolower(trimws(as.character(type %||% "")[1]))
    allowed <- dta_handler_types(dataset_type)
    if (!type %in% allowed) {
      # A file dataset has exactly one legal type, so "must be one of: any"
      # would state the rule without explaining it. Say why instead.
      if (identical(dataset_type, "file")) {
        stop(
          "A file dataset does not parse its files, so its file type is always 'any'."
        )
      }
      stop(sprintf(
        "File type must be one of: %s.",
        paste(allowed, collapse = ", ")
      ))
    }

    pattern <- isTRUE(pattern)
    exact <- identical(count_mode, "exact")
    as_count <- function(x, what) {
      v <- suppressWarnings(as.integer(x))[1]
      if (is.na(v) || v < 0) stop(sprintf("%s must be a whole number of 0 or more.", what))
      v
    }

    if (exact) {
      n <- as_count(number_of_files, "The number of files")
      if (!pattern && n != 1) {
        stop(
          "A handler that is not a pattern matches one exact file name, so it must expect exactly 1 file. Tick 'Filename is a pattern' to accept several."
        )
      }
      args <- list(number_of_files = n)
    } else {
      if (!pattern) {
        stop(
          "A range of files only makes sense for a pattern. Tick 'Filename is a pattern', or switch to an exact count of 1."
        )
      }
      mn <- as_count(min_number_of_files, "The minimum number of files")
      mx <- as_count(max_number_of_files, "The maximum number of files")
      if (mn > mx) stop("The minimum number of files cannot exceed the maximum.")
      args <- list(min_number_of_files = mn, max_number_of_files = mx)
    }

    if (!pattern && length(fn) > 1) {
      stop("Only a pattern handler can carry more than one file name.")
    }

    pd <- trimws(as.character(pattern_description %||% "")[1])

    # Parse extensions: split on newlines AND commas, trim, drop empties.
    ext <- .dta_split_lines(gsub(",", "\n", as.character(extensions %||% "")))
    ext <- if (length(ext) == 0) NULL else ext

    factory_args <- c(
      list(
        type = type,
        filename = fn,
        pattern = pattern,
        pattern_description = if (nzchar(pd)) pd else NULL,
        info = .dta_lines_to_info(info)
      ),
      args
    )

    # Only add extensions when type is "any" (csv/tsv do not accept this arg).
    if (identical(type, "any") && !is.null(ext)) {
      factory_args$extensions <- ext
    }

    handler <- do.call(DTAtools::DTAFileFactory, factory_args)

    ds <- DTAtools::datasets(dta, dataset)
    hs <- dta_handlers(ds)
    if (is.null(index)) {
      hs[[length(hs) + 1L]] <- handler
    } else {
      idx <- suppressWarnings(as.integer(index))[1]
      if (is.na(idx) || idx < 1 || idx > length(hs)) stop("File handler not found.")
      hs[[idx]] <- handler
    }
    ds@files <- unname(hs)
    dta@datasets[[dataset]] <- ds
    dta
  })
}

# Remove one file handler by index. Removing the LAST handler is allowed: the
# dataset then declares no expected files, which the reader and the app both
# handle, and is the only way back out of a handler added by mistake.
dta_remove_handler <- function(dta, dataset, index) {
  dta_try({
    ds <- DTAtools::datasets(dta, dataset)
    hs <- dta_handlers(ds)
    idx <- suppressWarnings(as.integer(index))[1]
    if (is.na(idx) || idx < 1 || idx > length(hs)) stop("File handler not found.")
    hs[[idx]] <- NULL
    ds@files <- unname(hs)
    dta@datasets[[dataset]] <- ds
    dta
  })
}

# Move a handler one position up or down. A move past either end is a no-op.
# Order is not cosmetic: the exported DTA document lists the expected files in
# this order. Returns dta_try().
dta_move_handler <- function(dta, dataset, index, direction) {
  dta_try({
    ds <- DTAtools::datasets(dta, dataset)
    hs <- dta_handlers(ds)
    n <- length(hs)
    idx <- suppressWarnings(as.integer(index))[1]
    target <- if (identical(direction, "up")) idx - 1L else idx + 1L
    if (!is.na(idx) && idx >= 1L && idx <= n && target >= 1L && target <= n) {
      hs[c(idx, target)] <- hs[c(target, idx)]
      ds@files <- unname(hs)
      dta@datasets[[dataset]] <- ds
    }
    dta
  })
}

# The index each handler of `dataset` ends up at after a mutation, as a map from
# the OLD 1-based index to the new one (NA = the handler is gone). The app's
# upload records are keyed by handler POSITION, so every mutation that shifts
# positions has to re-key them or bound files are orphaned: still in the dataset,
# no longer reachable from any slot.
dta_handler_index_map <- function(n, action = c("add", "remove", "move"),
                                  index = NULL, direction = NULL) {
  action <- match.arg(action)
  map <- stats::setNames(seq_len(n), as.character(seq_len(n)))
  if (n == 0 || identical(action, "add")) {
    return(map)
  }
  idx <- suppressWarnings(as.integer(index))[1]
  if (is.na(idx) || idx < 1 || idx > n) {
    return(map)
  }
  if (identical(action, "remove")) {
    map[idx] <- NA_integer_
    later <- seq_len(n) > idx
    map[later] <- map[later] - 1L
    return(map)
  }
  target <- if (identical(direction, "up")) idx - 1L else idx + 1L
  if (target >= 1L && target <= n) {
    map[c(idx, target)] <- map[c(target, idx)]
  }
  map
}

# Match a dataset's OLD file handlers against its NEW ones, by identity rather
# than by position: returns old index -> new index, NA where the old handler is
# not in the new list at all.
#
# The Edit-files dialog knows which operation it performed and can say exactly
# where every handler went (dta_handler_index_map()). A re-parsed document
# cannot: the user may have reordered the `files:` entries, inserted one in the
# middle, or rewritten one in place, and only the handlers themselves say which
# is which. Keeping upload records at their old POSITION would then show a file
# under a slot that expects something else entirely.
#
# Handlers are compared on their serialised form, and equal handlers are matched
# in order, so a document containing two identical entries still maps 1->1, 2->2.
dta_match_handlers <- function(old_ds, new_ds) {
  old_sigs <- vapply(
    dta_handlers(old_ds),
    function(h) yaml::as.yaml(dta_handler_to_list(h)),
    character(1)
  )
  new_sigs <- vapply(
    dta_handlers(new_ds),
    function(h) yaml::as.yaml(dta_handler_to_list(h)),
    character(1)
  )

  map <- rep(NA_integer_, length(old_sigs))
  taken <- rep(FALSE, length(new_sigs))
  for (i in seq_along(old_sigs)) {
    hit <- which(!taken & new_sigs == old_sigs[i])
    if (length(hit) > 0) {
      map[i] <- hit[1]
      taken[hit[1]] <- TRUE
    }
  }
  stats::setNames(map, as.character(seq_along(old_sigs)))
}

# ---- Metadata: transmission + generic scalar fields ----------------------

dta_transmission <- function(dta) {
  md <- tryCatch(DTAtools::metadata(dta), error = function(e) NULL)
  if (is.null(md)) {
    return(list())
  }
  tr <- tryCatch(S7::prop(md, "transmission"), error = function(e) NULL)
  if (is.null(tr)) list() else tr
}

# Update one transmission field. Empty string / NULL removes it. A logical is
# stored as-is (flags); Date/character dates are stored as-is (both accepted).
dta_set_transmission_field <- function(dta, field, value) {
  dta_try({
    md <- DTAtools::metadata(dta)
    tr <- tryCatch(S7::prop(md, "transmission"), error = function(e) NULL) %||% list()
    drop <- is.null(value) ||
      (is.character(value) && (length(value) == 0 || !any(nzchar(value))))
    if (drop) tr[[field]] <- NULL else tr[[field]] <- value
    S7::prop(md, "transmission") <- tr
    dta@metadata <- md
    dta
  })
}

# Map a stored transmission FLAG (logical / character / NULL) to the tri-state
# dropdown choice shown in the metadata editor ("undefined" / "yes" / "no").
# An unset flag (NULL / NA / absent) is "undefined".
dta_flag_to_choice <- function(v) {
  if (is.null(v) || length(v) == 0 || (length(v) == 1 && is.na(v))) {
    return("undefined")
  }
  if (is.logical(v)) {
    return(if (isTRUE(v[1])) "yes" else "no")
  }
  vs <- tolower(trimws(as.character(v)[1]))
  if (vs %in% c("true", "yes", "1")) {
    "yes"
  } else if (vs %in% c("false", "no", "0")) {
    "no"
  } else {
    "undefined"
  }
}

# Inverse of dta_flag_to_choice: dropdown choice -> stored flag.
# "yes" -> TRUE, "no" -> FALSE, "undefined" -> NULL (field is left UNSET).
dta_choice_to_flag <- function(x) {
  if (identical(x, "yes")) TRUE else if (identical(x, "no")) FALSE else NULL
}

# ---- Contacts: read one + update in place --------------------------------

dta_contact_at <- function(dta, side, index) {
  cs <- dta_contacts(dta, side)
  if (index < 1 || index > length(cs)) {
    return(NULL)
  }
  cs[[index]]
}
# Update fields of one contact, preserving any fields not in `fields`. An empty
# string removes that field. Returns dta_try() (value = updated DTA).
dta_update_contact <- function(dta, side, index, fields) {
  dta_try({
    md <- DTAtools::metadata(dta)
    side_list <- tryCatch(S7::prop(md, side), error = function(e) NULL) %||% list()
    contacts <- side_list$contacts %||% list()
    if (index < 1 || index > length(contacts)) stop("Contact index out of range.")
    person <- contacts[[index]] %||% list()
    for (k in names(fields)) {
      v <- fields[[k]]
      if (is.null(v) || (is.character(v) && !nzchar(v))) person[[k]] <- NULL else person[[k]] <- v
    }
    contacts[[index]] <- person
    side_list$contacts <- contacts
    S7::prop(md, side) <- side_list
    dta@metadata <- md
    dta
  })
}

# ---- Raw-YAML apply reconciliation (handler / spec diff + data transfer) --
# When the editable Raw YAML is applied, the app must NOT blindly discard bound
# uploads. These helpers let the server compare the OLD and NEW dataset shapes
# so it can: keep uploads whose file handlers are unchanged, clear a dataset's
# uploads when its handlers changed, drop deleted datasets, and clear only the
# affected dataset's validation when its columns/rules changed.

# A stable string signature of a dataset's file handlers (for equality tests).
dta_handlers_signature <- function(ds) {
  hs <- lapply(dta_handlers(ds), dta_handler_to_list)
  tryCatch(yaml::as.yaml(hs), error = function(e) paste(utils::capture.output(str(hs)), collapse = "\n"))
}

# A stable string signature of a dataset's columns + rules (for equality tests).
dta_specs_signature <- function(ds) {
  cols <- lapply(
    tryCatch(ds@specs@columns, error = function(e) list()) %||% list(),
    dta_column_to_list
  )
  rules <- Filter(
    Negate(is.null),
    lapply(
      tryCatch(ds@specs@rules, error = function(e) list()) %||% list(),
      dta_rule_to_list
    )
  )
  tryCatch(yaml::as.yaml(list(columns = unname(cols), rules = unname(rules))),
    error = function(e) ""
  )
}

# Copy bound data (tables / file paths) from an old dataset onto the freshly
# parsed dataset of the same name in `dta`. keep_validation carries the
# validation index/store too (only when the specs are unchanged).
dta_transfer_bound_data <- function(dta, dataset, old_ds, keep_validation = TRUE) {
  dta_try({
    new_ds <- dta@datasets[[dataset]]
    ty <- tryCatch(old_ds@type, error = function(e) NA_character_)
    if (identical(ty, "file")) {
      new_ds@file_paths <- tryCatch(old_ds@file_paths, error = function(e) character(0)) %||% character(0)
    } else {
      new_ds@tables <- tryCatch(old_ds@tables, error = function(e) list()) %||% list()
    }
    if (isTRUE(keep_validation)) {
      new_ds@validation_index <- tryCatch(old_ds@validation_index, error = function(e) list()) %||% list()
      new_ds@validation_store <- tryCatch(old_ds@validation_store, error = function(e) list()) %||% list()
    } else {
      new_ds@validation_index <- list()
      new_ds@validation_store <- list()
    }
    dta@datasets[[dataset]] <- new_ds
    dta
  })
}

# ---- Session persistence (saveRDS-safe) ----------------------------------
# Bound tables are `arrow::Table` objects, i.e. external pointers to C++ memory.
# saveRDS() serializes them as NULL pointers, so a restored DTA has tables that
# throw "Invalid <Table>, external pointer to null" the moment they are touched
# (e.g. clicking a validation message -> inspect() -> as.data.frame(table)).
# The DTADataSetTabular validator also REQUIRES every @tables element to inherit
# "Table", so we cannot simply stash data.frames inside the object. Instead we
# collect the tables to plain data.frames OUTSIDE the DTA (and empty the object's
# tables/validation slots, in an order the validator accepts) for saving, then
# rebuild real arrow tables on restore.

# Returns list(dta = <tables/validation stripped>, tables, vindex, vstore).
dta_dump_session <- function(dta) {
  tables <- list()
  vindex <- list()
  vstore <- list()
  ns <- tryCatch(dta_dataset_names(dta), error = function(e) character(0))
  for (nm in ns) {
    ds <- tryCatch(dta@datasets[[nm]], error = function(e) NULL)
    if (is.null(ds)) next
    tb <- tryCatch(ds@tables, error = function(e) NULL)
    if (length(tb) > 0) {
      tables[[nm]] <- lapply(tb, function(t) tryCatch(as.data.frame(t), error = function(e) NULL))
      vindex[[nm]] <- tryCatch(ds@validation_index, error = function(e) list()) %||% list()
      vstore[[nm]] <- tryCatch(ds@validation_store, error = function(e) list()) %||% list()
      # Clear in an order the S7 validator tolerates: index/store (<= tables)
      # first, then the tables themselves.
      ds@validation_index <- list()
      ds@validation_store <- list()
      ds@tables <- list()
      dta@datasets[[nm]] <- ds
    }
  }
  list(dta = dta, tables = tables, vindex = vindex, vstore = vstore)
}

# Rebuild a live DTA from dta_dump_session() output: re-materialize each saved
# data.frame as an arrow table and restore its validation index/store.
dta_restore_session <- function(dump) {
  if (is.null(dump)) {
    return(NULL)
  }
  dta <- dump$dta
  tables <- dump$tables %||% list()
  vindex <- dump$vindex %||% list()
  vstore <- dump$vstore %||% list()
  for (nm in names(tables)) {
    ds <- tryCatch(dta@datasets[[nm]], error = function(e) NULL)
    if (is.null(ds)) next
    arrow_tbls <- lapply(tables[[nm]], function(df) {
      if (is.null(df)) {
        return(NULL)
      }
      tryCatch(arrow::as_arrow_table(df), error = function(e) NULL)
    })
    arrow_tbls <- arrow_tbls[!vapply(arrow_tbls, is.null, logical(1))]
    ds@tables <- arrow_tbls
    vi <- vindex[[nm]] %||% list()
    vs <- vstore[[nm]] %||% list()
    # Never let the index/store exceed the tables we actually rebuilt.
    if (length(names(arrow_tbls)) > 0) {
      vi <- vi[intersect(names(vi), names(arrow_tbls))]
      vs <- vs[intersect(names(vs), names(arrow_tbls))]
    } else {
      vi <- list()
      vs <- list()
    }
    ds@validation_index <- vi
    ds@validation_store <- vs
    dta@datasets[[nm]] <- ds
  }
  dta
}

# Format the date/time suffix of an export file name as "%Y-%m-%d_%H-%M".
# Colons are illegal in Windows file names, so the clock uses a hyphen. A bare
# Date carries no clock, so it renders as midnight rather than silently
# borrowing the current time.
.dta_export_stamp <- function(when = Sys.time()) {
  if (inherits(when, "Date")) {
    when <- as.POSIXct(as.character(when), tz = "UTC")
  }
  format(when, "%Y-%m-%d_%H-%M")
}

# Build the stem of an exported document's file name: the DTA title, then the
# document version when one is set, then the date and time -- e.g.
# "Clinical_Data_Transfer-v0.2-2026-08-14_14-07". Kept in one place because the
# modal preview and the two export branches (markdown, Word) must agree -- the
# preview is a promise about the file the user is about to download.
dta_export_stem <- function(dta, when = Sys.time()) {
  stamp <- .dta_export_stamp(when)

  md <- tryCatch(DTAtools::metadata(dta), error = function(e) NULL)
  get_prop <- function(nm) {
    tryCatch(
      {
        if (is.null(md)) {
          return(NULL)
        }
        v <- as.character(S7::prop(md, nm))
        if (length(v) == 0) NULL else v[1]
      },
      error = function(e) NULL
    )
  }

  ttl <- get_prop("title")
  base <- if (!is.null(ttl) && !is.na(ttl) && nzchar(ttl)) {
    gsub("[^A-Za-z0-9]+", "_", ttl)
  } else {
    "DTA"
  }

  ver <- get_prop("version")
  if (is.null(ver) || is.na(ver) || !nzchar(ver)) {
    return(paste0(base, "_", stamp))
  }

  # Dots are kept so the version reads as authored ("v0.2", not "v0_2"); any
  # other separator a version may legitimately carry ("1.0 draft") is folded to
  # an underscore so the name stays file-system safe.
  ver_safe <- gsub("(^_+|_+$)", "", gsub("[^A-Za-z0-9.]+", "_", ver))
  if (!nzchar(ver_safe)) {
    return(paste0(base, "_", stamp))
  }

  paste0(base, "-v", ver_safe, "-", stamp)
}
