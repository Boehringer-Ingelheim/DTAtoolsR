# -----------------------------------------------------------------------------
# Utilities: thin, safe wrappers around the DTAtools API.
# The app mutates the DTA object ONLY through these helpers.
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

handler_expected <- function(h) {
  fn <- tryCatch(h@filename, error = function(e) NA_character_)
  paste(fn, collapse = ", ")
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

# List the YAML specification documents bundled in inst/extdata (basenames),
# i.e. the counterpart of dta_example_data_files() that keeps ONLY .yaml/.yml.
# Used by the landing page to offer every bundled example DTA to load.
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
  sort(files)
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
dta_load_file <- function(dta, dataset, file, handler_index, name = NULL) {
  nm <- name %||% tools::file_path_sans_ext(basename(file))
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

# Per-table validation status: named vector
# table -> "pass" | "fail" | "unknown" | "pending".
#
#   "pending" = not validated yet (no tick)
#   "pass"    = validated, all THREE axes clean (schema, rules, import)
#   "fail"    = validated with schema, rule OR import errors
#   "unknown" = validated, but the import axis was never checked
#
# Validation has three axes, and ok = schema_valid && rules_valid &&
# import_valid. A table whose only defect is a value that could not be
# represented in its declared type has zero schema and zero rule errors, so
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
  nse <- count("n_schema_errors")
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
# an empty file, which is what made the Export PDF button appear to do nothing).
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
  for (eng in engines) if (nzchar(Sys.which(eng))) {
    return(eng)
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

# Robust "Export PDF": build the DOCX first (always works via officer), then
# convert with the best engine available and VERIFY a real PDF resulted. When no
# external converter exists we still produce a valid PDF from the Markdown export
# via R's own device -- so the button ALWAYS yields an openable PDF.
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

# Build a self-contained HTML validation report for a validated DTA. Summarises
# per-dataset status (from the app's status map) and, when available, the
# per-target detail from results(). Returns a single HTML string.
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
  overall_ok <- length(validated) > 0 && n_fail == 0

  res <- dta_try(as.data.frame(DTAtools::results(dta)))
  rdf <- if (isTRUE(res$ok)) res$value else NULL

  ds_rows <- paste0(vapply(ds_names, function(nm) {
    s <- st[[nm]]
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
        "dataset", "target", "status", "n_schema_errors",
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

  banner_cls <- if (overall_ok) "pass" else "warn"
  banner_txt <- if (overall_ok) "VALIDATION PASSED" else "VALIDATION INCOMPLETE"
  summary_line <- paste0(
    n_pass, " passed, ", n_fail, " failed, ",
    n_nodata, " without data."
  )
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
    ".summary{margin:10px 0 4px;font-size:14px}",
    "table{border-collapse:collapse;font-size:13px;margin-top:6px}",
    "th,td{border:1px solid #ccc;padding:5px 10px;text-align:left}",
    "th{background:#f4f4f4}td.ok{color:#1e7e34;font-weight:600}",
    "td.bad{color:#b71c1c;font-weight:600}td.muted{color:#888}"
  )

  paste0(
    "<!doctype html><html lang='en'><head><meta charset='utf-8'>",
    "<title>DTA Validation Report</title><style>", css, "</style></head><body>",
    "<h1>DTA Validation Report</h1>", subtitle,
    "<div class='meta'>Generated ", esc(ts), "</div>",
    "<div class='banner ", banner_cls, "'>", banner_txt, "</div>",
    "<div class='summary'>", esc(summary_line), "</div>",
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

# Condition operators offered in the rule editor (mirrors evaluateSchemaRules()).
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
    yaml::as.yaml(lst %||% list(), indent = 2, line.sep = "\n")
  })
}

# Serialize ONE dataset to a YAML string (standalone-dataset shape).
dta_dataset_to_yaml_text <- function(dta, dataset) {
  dta_try({
    ds <- dta_get_dataset(dta, dataset)
    if (is.null(ds)) stop(sprintf("Dataset '%s' not found.", dataset))
    lst <- .dta_compact(dta_dataset_to_list(ds))
    yaml::as.yaml(lst %||% list(), indent = 2, line.sep = "\n")
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

# File types the editor can offer. Deliberately narrower than the DTAFile class
# tree: DTAFileFactory() -- the only route from a YAML document back to an
# object -- implements csv and tsv only, so offering `delim` would create a
# handler that cannot be read back.
dta_handler_types <- function() {
  c("csv", "tsv")
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
    info = paste(.dta_info_to_lines(info), collapse = "\n")
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
                            info = NULL) {
  dta_try({
    # One name or pattern per line, so a multi-name handler survives the
    # fields -> form -> set round trip as several names rather than one.
    fn <- .dta_split_lines(filename)
    if (length(fn) == 0) stop("A file name or pattern is required.")

    type <- tolower(trimws(as.character(type %||% "")[1]))
    if (!type %in% dta_handler_types()) {
      stop(sprintf(
        "File type must be one of: %s.",
        paste(dta_handler_types(), collapse = ", ")
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

    handler <- do.call(DTAtools::DTAFileFactory, c(
      list(
        type = type,
        filename = fn,
        pattern = pattern,
        pattern_description = if (nzchar(pd)) pd else NULL,
        info = .dta_lines_to_info(info)
      ),
      args
    ))

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
