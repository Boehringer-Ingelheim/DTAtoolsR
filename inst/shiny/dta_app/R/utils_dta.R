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
# read via read_dataset_from_yaml() and wrapped in a DTA so the rest of the app
# (which operates on a DTA) works unchanged.
# Returns the usual dta_try() list plus two flags:
#   dataset_only : TRUE when the source was a standalone dataset YAML.
#   has_metadata : TRUE when the source YAML carried a `metadata:` section.
dta_read_yaml <- function(path) {
  raw <- dta_try(yaml::read_yaml(path))
  if (!raw$ok) {
    return(list(ok = FALSE, value = NULL, error = raw$error,
                dataset_only = FALSE, has_metadata = FALSE))
  }
  y <- raw$value
  has_metadata <- is.list(y) && !is.null(y$metadata)
  is_dta <- is.list(y) && (has_metadata || !is.null(y$datasets))

  if (is_dta) {
    res <- dta_try(DTAtools::read_dta_from_yaml(path))
    res$dataset_only <- FALSE
    res$has_metadata <- has_metadata
    return(res)
  }

  # Standalone dataset YAML -> read it and wrap in a metadata-less DTA.
  res <- dta_try({
    ds <- DTAtools::read_dataset_from_yaml(path)
    DTAtools::DTA(datasets = ds)
  })
  res$dataset_only <- TRUE
  res$has_metadata <- FALSE
  res
}

# Validate a raw YAML STRING as a DTA/DTADataSet by staging it to a temp file
# and reusing dta_read_yaml() (which checks YAML syntax FIRST, then DTA/dataset
# structure). Returns the same list shape as dta_read_yaml() -- so callers get
# ok/value/error plus dataset_only/has_metadata. Used by the editable Raw YAML
# tab so a save only replaces the loaded document when the text is BOTH valid
# YAML AND a valid DTA / DTADataSet.
dta_read_yaml_text <- function(text) {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)
  written <- tryCatch({
    con <- file(tmp, open = "wb")
    writeLines(enc2utf8(as.character(text)), con, useBytes = TRUE)
    close(con)
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(written)) {
    return(list(ok = FALSE, value = NULL,
                error = "Could not stage the YAML text for validation.",
                dataset_only = FALSE, has_metadata = FALSE))
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
  if (is.na(mn) && is.na(mx)) return("")
  if (!is.na(mn) && !is.na(mx) && mn == mx) {
    sprintf("%d file%s", as.integer(mn), if (mn == 1) "" else "s")
  } else {
    sprintf("%s\u2013%s files", ifelse(is.na(mn), "?", as.integer(mn)),
            ifelse(is.na(mx), "?", as.integer(mx)))
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
  tryCatch({
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    target <- file.path(target_dir, base)
    if (isTRUE(file.copy(datapath, target, overwrite = TRUE))) target else datapath
  }, error = function(e) datapath)
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
dta_set_metadata_field <- function(dta, field, value) {
  dta_try({
    md <- DTAtools::metadata(dta)
    if (identical(field, "date")) {
      value <- tryCatch(as.Date(value), error = function(e) value)
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
  if (is.null(md)) return(list())
  side_list <- tryCatch(S7::prop(md, side), error = function(e) NULL)
  cs <- side_list$contacts %||% list()
  if (is.null(cs)) list() else cs
}

dta_add_contact <- function(dta, side, name, roles = "", email = "") {
  dta_try({
    md <- DTAtools::metadata(dta)
    side_list <- tryCatch(S7::prop(md, side), error = function(e) NULL) %||% list()
    contacts <- side_list$contacts %||% list()
    role_str <- paste(roles[nzchar(roles)], collapse = ", ")
    person <- list(name = name, role = role_str, email = email)
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
  if (is.null(md)) return(list())
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
      if (is.null(val)) return(a)
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
  if (is.null(ds)) return(0L)
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
  if (length(names_ds) == 0) return(out)

  res <- tryCatch(DTAtools::results(dta), error = function(e) NULL)
  have_res <- !is.null(res) && nrow(res) > 0
  if (have_res) {
    n_invalid <- if ("n_invalid" %in% names(res)) suppressWarnings(as.numeric(res$n_invalid)) else rep(NA_real_, nrow(res))
    n_valid   <- if ("n_valid" %in% names(res)) suppressWarnings(as.numeric(res$n_valid)) else rep(NA_real_, nrow(res))
    n_validated <- if ("n_validated" %in% names(res)) suppressWarnings(as.numeric(res$n_validated)) else rep(NA_real_, nrow(res))
    ds_col <- if ("dataset" %in% names(res)) as.character(res$dataset) else names_ds
  }

  for (nm in names_ds) {
    # No bound data -> nodata, independent of results() (Contract C5).
    if (dta_dataset_content_count(dta_get_dataset(dta, nm)) == 0) {
      out[[nm]] <- "nodata"; next
    }
    if (!have_res) { out[[nm]] <- "pending"; next }
    idx <- which(ds_col == nm)
    if (length(idx) == 0) { out[[nm]] <- "pending"; next }
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
  if (is.null(ds)) return(character(0))
  ty <- tryCatch(ds@type, error = function(e) NA_character_)
  if (identical(ty, "file")) {
    fp <- tryCatch(ds@file_paths, error = function(e) character(0)) %||% character(0)
    basename(fp)
  } else {
    names(tryCatch(ds@tables, error = function(e) list())) %||% character(0)
  }
}

# Per-table validation status: named vector table -> "pass" | "fail" | "pending".
# "pending" = not validated yet (no tick); "pass" = validated, zero errors;
# "fail" = validated with schema/rule errors. Drives the per-file tick color.
dta_table_status_map <- function(dta, dataset) {
  empty <- stats::setNames(character(0), character(0))
  ds <- dta_get_dataset(dta, dataset)
  if (is.null(ds)) return(empty)
  vs <- tryCatch(as.data.frame(DTAtools::validation_status(ds)),
                 error = function(e) NULL)
  if (is.null(vs) || nrow(vs) == 0) return(empty)
  tcol <- if ("table" %in% names(vs)) "table" else if ("target" %in% names(vs)) "target" else names(vs)[1]
  ok  <- if ("ok" %in% names(vs)) vs$ok else rep(NA, nrow(vs))
  nse <- if ("n_schema_errors" %in% names(vs)) suppressWarnings(as.numeric(vs$n_schema_errors)) else rep(NA_real_, nrow(vs))
  nre <- if ("n_rule_errors" %in% names(vs)) suppressWarnings(as.numeric(vs$n_rule_errors)) else rep(NA_real_, nrow(vs))
  has_err <- (!is.na(nse) & nse > 0) | (!is.na(nre) & nre > 0) | (!is.na(ok) & !ok)
  st <- rep("pending", nrow(vs))
  st[!is.na(ok) & ok] <- "pass"
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

# Remove ONE bound file/table (and its validation state) from a dataset.
dta_unload_table <- function(dta, dataset, table) {
  dta_try({
    ds <- DTAtools::datasets(dta, dataset)
    ty <- tryCatch(ds@type, error = function(e) NA_character_)
    if (identical(ty, "file")) {
      fp <- tryCatch(ds@file_paths, error = function(e) character(0)) %||% character(0)
      ds@file_paths <- fp[basename(fp) != table]
    } else {
      tabs <- tryCatch(ds@tables, error = function(e) list()) %||% list()
      tabs[[table]] <- NULL
      ds@tables <- tabs
    }
    vi <- tryCatch(ds@validation_index, error = function(e) list()) %||% list()
    vi[[table]] <- NULL
    ds@validation_index <- vi
    vsr <- tryCatch(ds@validation_store, error = function(e) list()) %||% list()
    vsr[[table]] <- NULL
    ds@validation_store <- vsr
    dta@datasets[[dataset]] <- ds
    dta
  })
}

# Remove ALL bound files/tables (and all validation state) from a dataset.
dta_unload_all <- function(dta, dataset) {
  dta_try({
    ds <- DTAtools::datasets(dta, dataset)
    ty <- tryCatch(ds@type, error = function(e) NA_character_)
    if (identical(ty, "file")) {
      ds@file_paths <- character(0)
    } else {
      ds@tables <- list()
    }
    ds@validation_index <- list()
    ds@validation_store <- list()
    dta@datasets[[dataset]] <- ds
    dta
  })
}

# Per-error messages for a single dataset (data.frame). Empty df if none.
dta_dataset_messages <- function(dta, dataset) {
  ds <- dta_get_dataset(dta, dataset)
  if (is.null(ds)) return(data.frame())
  res <- dta_try(as.data.frame(DTAtools::messages(ds)))
  if (!res$ok || is.null(res$value)) return(data.frame())
  res$value
}

# Deep-dive detail for one message id within a dataset (data.frame).
dta_inspect <- function(dta, dataset, id) {
  ds <- dta_get_dataset(dta, dataset)
  if (is.null(ds)) return(dta_try(stop("dataset not found")))
  dta_try(as.data.frame(DTAtools::inspect(ds, id = id)))
}

# ---- Export --------------------------------------------------------------

dta_export <- function(dta, file, format, signature_list = NULL) {
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

# Supported dataset types (fallback to known set if internal symbol absent).
dta_supported_types <- function() {
  types <- tryCatch(
    get("__DTAtools_supported_dataset_types__", envir = asNamespace("DTAtools")),
    error = function(e) NULL
  )
  types %||% c("tabular", "file")
}
