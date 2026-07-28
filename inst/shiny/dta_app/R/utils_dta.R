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

# List the data files bundled in inst/extdata that can populate an upload slot,
# i.e. everything EXCEPT the YAML specification documents. Returns basenames.
dta_example_data_files <- function() {
  dir <- system.file("extdata", package = "DTAtools")
  if (!nzchar(dir) || !dir.exists(dir)) return(character(0))
  files <- list.files(dir, full.names = FALSE, recursive = FALSE)
  if (length(files) == 0) return(character(0))
  files <- files[!dir.exists(file.path(dir, files))]  # drop any subdirectories
  ext <- tolower(tools::file_ext(files))
  files <- files[!ext %in% c("yaml", "yml")]          # drop YAML specification docs
  sort(files)
}

# Absolute path to a bundled example data file (given its basename). Returns ""
# when the name is empty or is not one of the bundled example files -- the
# whitelist check keeps a crafted input value from reading an arbitrary path.
dta_example_data_path <- function(filename) {
  if (is.null(filename) || !nzchar(filename)) return("")
  if (!basename(filename) %in% dta_example_data_files()) return("")
  system.file("extdata", basename(filename), package = "DTAtools")
}

# List the YAML specification documents bundled in inst/extdata (basenames),
# i.e. the counterpart of dta_example_data_files() that keeps ONLY .yaml/.yml.
# Used by the landing page to offer every bundled example DTA to load.
dta_example_yaml_files <- function() {
  dir <- system.file("extdata", package = "DTAtools")
  if (!nzchar(dir) || !dir.exists(dir)) return(character(0))
  files <- list.files(dir, full.names = FALSE, recursive = FALSE)
  if (length(files) == 0) return(character(0))
  files <- files[!dir.exists(file.path(dir, files))]  # drop any subdirectories
  ext <- tolower(tools::file_ext(files))
  files <- files[ext %in% c("yaml", "yml")]            # keep YAML specs only
  sort(files)
}

# Absolute path to a bundled example YAML (given its basename). Returns "" when
# the name is empty or not one of the bundled YAML specs -- the whitelist check
# keeps a crafted input value from reading an arbitrary path.
dta_example_yaml_path <- function(filename) {
  if (is.null(filename) || !nzchar(filename)) return("")
  if (!basename(filename) %in% dta_example_yaml_files()) return("")
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

# Build a self-contained HTML validation report for a validated DTA. Summarises
# per-dataset status (from the app's status map) and, when available, the
# per-target detail from results(). Returns a single HTML string.
dta_build_validation_report <- function(dta, status = NULL) {
  esc <- function(x) htmltools::htmlEscape(as.character(x %||% ""))
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  title <- tryCatch({
    md <- dta@metadata
    t <- tryCatch(md@title, error = function(e) NULL)
    if (!is.null(t) && length(t) && nzchar(t)) t else NULL
  }, error = function(e) NULL)

  st <- unlist(status %||% list())
  status_label <- function(s) switch(s %||% "",
    pass = "Passed", fail = "Failed", nodata = "No data",
    pending = "Not validated", s %||% "Unknown")
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
    paste0("<tr><td>", esc(nm), "</td><td class='", cls, "'>",
           esc(status_label(s)), "</td></tr>")
  }, character(1)), collapse = "")

  detail_html <- ""
  if (!is.null(rdf) && nrow(rdf) > 0) {
    want <- intersect(c("dataset", "target", "status", "n_schema_errors",
                        "n_rule_errors", "validated_at"), names(rdf))
    if (length(want) > 0) {
      head_cells <- paste0(vapply(want, function(h) paste0("<th>", esc(h), "</th>"),
                                  character(1)), collapse = "")
      body_rows <- paste0(vapply(seq_len(nrow(rdf)), function(i) {
        cells <- paste0(vapply(want, function(h) paste0("<td>", esc(rdf[[h]][i]), "</td>"),
                               character(1)), collapse = "")
        paste0("<tr>", cells, "</tr>")
      }, character(1)), collapse = "")
      detail_html <- paste0(
        "<h2>Per-target detail</h2><table><thead><tr>", head_cells,
        "</tr></thead><tbody>", body_rows, "</tbody></table>")
    }
  }

  banner_cls <- if (overall_ok) "pass" else "warn"
  banner_txt <- if (overall_ok) "VALIDATION PASSED" else "VALIDATION INCOMPLETE"
  summary_line <- paste0(n_pass, " passed, ", n_fail, " failed, ",
                         n_nodata, " without data.")
  subtitle <- if (!is.null(title)) {
    paste0("<div class='subtitle'>", esc(title), "</div>")
  } else ""

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
    "td.bad{color:#b71c1c;font-weight:600}td.muted{color:#888}")

  paste0(
    "<!doctype html><html lang='en'><head><meta charset='utf-8'>",
    "<title>DTA Validation Report</title><style>", css, "</style></head><body>",
    "<h1>DTA Validation Report</h1>", subtitle,
    "<div class='meta'>Generated ", esc(ts), "</div>",
    "<div class='banner ", banner_cls, "'>", banner_txt, "</div>",
    "<div class='summary'>", esc(summary_line), "</div>",
    "<h2>Datasets</h2><table><thead><tr><th>Dataset</th><th>Status</th></tr>",
    "</thead><tbody>", ds_rows, "</tbody></table>", detail_html,
    "</body></html>")
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
  c("equals"            = "equals",
    "not equals"        = "not_equals",
    "greater"           = "greater",
    "greater or equal"  = "greater_equal",
    "less"              = "less",
    "less or equal"     = "less_equal",
    "between (min/max)" = "min_max",
    "in (list)"         = "in",
    "not in (list)"     = "not_in",
    "matches pattern"   = "pattern",
    "empty"             = "empty")
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
    if (length(x) == 0) return(NULL)
  }
  x
}

# Convert any Date objects to "YYYY-MM-DD" strings (YAML has no Date scalar).
.dta_stringify_dates <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (is.list(x)) return(lapply(x, .dta_stringify_dates))
  x
}

# Recover a handler's file-type token ("csv" / "tsv") from its S7 class.
dta_handler_type <- function(h) {
  if (inherits(h, "DTAtools::DTAFileTSV")) return("tsv")
  if (inherits(h, "DTAtools::DTAFileCSV")) return("csv")
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
  mn <- handler_min(h); mx <- handler_max(h)
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
  lbl <- g("label"); if (!is.null(lbl) && nzchar(lbl)) out$label <- lbl
  if (!is.null(st)) {
    backend <- tryCatch(st@backend, error = function(e) NULL) %||% "SAS"
    ty  <- tryCatch(st@type,   error = function(e) NULL)
    fmt <- tryCatch(st@format, error = function(e) NULL)
    len <- tryCatch(st@length, error = function(e) NULL)
    if (!is.null(ty)  && length(ty)  && nzchar(ty))  out$type   <- paste(backend, ty)
    if (!is.null(fmt) && length(fmt) && nzchar(fmt)) out$format <- paste(backend, fmt)
    if (!is.null(len) && length(len) && !is.na(len)) out$length <- as.integer(len)
  }
  nl <- g("nullable"); if (!is.null(nl) && length(nl) > 0) out$nullable <- isTRUE(nl)
  vals <- g("values");   if (!is.null(vals) && length(vals) > 0) out$values <- vals
  ex   <- g("examples"); if (!is.null(ex)   && length(ex)   > 0) out$examples <- ex
  patt <- g("pattern");  if (!is.null(patt) && length(patt) && nzchar(patt)) out$pattern <- patt
  cc   <- g("colclass"); if (!is.null(cc)   && length(cc)   > 0) out$colclass <- cc
  desc <- g("description"); if (!is.null(desc) && length(desc) && nzchar(desc)) out$description <- desc
  out
}

# One DTARule -> plain list, with the SHORT type token and `columns` (plural).
dta_rule_to_list <- function(rule) {
  raw <- tryCatch(as.list(rule), error = function(e) NULL)
  if (is.null(raw)) return(NULL)
  ty <- raw$type %||% ""
  ty <- switch(ty,
    check_col_condition = , col_condition = "col_condition",
    check_range = , col_range = "col_range",
    check_unique = , col_unique = "col_unique",
    ty)
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
    if (nzchar(t)) t else "\u2014")
}

# One DTADataSet -> plain list (standalone-dataset shape).
dta_dataset_to_list <- function(ds) {
  if (is.null(ds)) return(list())
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
  if (length(cols) == 0) return(data.frame())
  do.call(rbind, lapply(cols, function(c) {
    l <- dta_column_to_list(c)
    constraint <- if (!is.null(l$values)) {
      paste(unlist(l$values), collapse = ", ")
    } else if (!is.null(l$pattern)) {
      paste0("/", l$pattern, "/")
    } else ""
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
  if (is.null(col)) return(NULL)
  l <- dta_column_to_list(col)
  bt <- l$type %||% ""
  backend <- "SAS"; bare <- bt
  if (nzchar(bt) && grepl(" ", bt)) {
    sp <- strsplit(bt, " ", fixed = TRUE)[[1]]
    backend <- sp[1]; bare <- paste(sp[-1], collapse = " ")
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
    type_arg   <- if (!is.null(type)   && nzchar(type))   paste(bk, type)   else NULL
    format_arg <- if (!is.null(format) && nzchar(format)) paste(bk, format) else NULL
    len_arg <- NULL
    if (!is.null(length) && length(length) && nzchar(as.character(length))) {
      len_arg <- suppressWarnings(as.integer(length))
      if (is.na(len_arg)) len_arg <- NULL
    }
    vals <- NULL
    if (!is.null(values) && length(values) > 0) {
      v <- trimws(values); v <- v[nzchar(v)]
      if (length(v) > 0) vals <- v
    }
    patt <- if (!is.null(pattern) && nzchar(pattern)) pattern else NULL
    lbl  <- if (!is.null(label) && nzchar(label)) label else NULL
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
  if (is.null(cond) || length(cond) == 0) return("")
  parts <- vapply(names(cond), function(col) {
    spec <- cond[[col]]
    if (is.list(spec) && length(spec) > 0) {
      op <- names(spec)[1]
      val <- spec[[1]]
      sprintf("%s %s %s", col, op, paste(unlist(val), collapse = ", "))
    } else col
  }, character(1))
  paste(parts, collapse = "; ")
}

# A compact data.frame overview of a dataset's rules (for the editor table).
dta_rules_overview <- function(dta, dataset) {
  ds <- dta_get_dataset(dta, dataset)
  rules <- tryCatch(ds@specs@rules, error = function(e) NULL) %||% list()
  if (length(rules) == 0) return(data.frame())
  do.call(rbind, lapply(seq_along(rules), function(i) {
    l <- dta_rule_to_list(rules[[i]])
    detail <- if (identical(l$type, "col_condition")) {
      sprintf("IF %s THEN %s", .dta_cond_to_text(l$condition), .dta_cond_to_text(l$then))
    } else if (identical(l$type, "col_range")) {
      sprintf("%s in [%s, %s]", paste(l$columns, collapse = ", "),
              l$min %||% "", l$max %||% "")
    } else if (identical(l$type, "col_unique")) {
      sprintf("unique(%s)", paste(l$columns, collapse = ", "))
    } else ""
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
  if (index < 1 || index > length(rules)) return(NULL)
  dta_rule_to_list(rules[[index]])
}

# Build a DTARule from parts (dispatches on the short type token).
dta_build_rule <- function(id, type, description = NULL, condition = NULL,
                           then = NULL, columns = NULL, min = NULL, max = NULL) {
  id <- trimws(as.character(id)[1] %||% "")
  if (!nzchar(id)) stop("A rule id is required.")
  if (grepl("\\s", id)) stop("Rule id cannot contain whitespace.")
  desc <- if (!is.null(description) && nzchar(description)) description else NULL
  ty <- switch(type,
    col_condition = , check_col_condition = "col_condition",
    col_range = , check_range = "col_range",
    col_unique = , check_unique = "col_unique",
    type)
  if (identical(ty, "col_condition")) {
    DTAtools::DTARuleColCondition(id = id, description = desc,
                                  condition = condition, then = then)
  } else if (identical(ty, "col_range")) {
    DTAtools::DTARuleColRange(id = id, columns = columns, description = desc,
                              min = min, max = max)
  } else if (identical(ty, "col_unique")) {
    DTAtools::DTARuleColUnique(id = id, columns = columns, description = desc)
  } else {
    stop(sprintf("Unknown rule type '%s'.", type))
  }
}

# Add (index NULL) or replace (1-based index) a rule. Returns dta_try().
dta_set_rule <- function(dta, dataset, index = NULL, id, type, description = NULL,
                         condition = NULL, then = NULL, columns = NULL,
                         min = NULL, max = NULL) {
  dta_try({
    rule <- dta_build_rule(id = id, type = type, description = description,
                           condition = condition, then = then, columns = columns,
                           min = min, max = max)
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

# ---- Metadata: transmission + generic scalar fields ----------------------

dta_transmission <- function(dta) {
  md <- tryCatch(DTAtools::metadata(dta), error = function(e) NULL)
  if (is.null(md)) return(list())
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

# ---- Contacts: read one + update in place --------------------------------

dta_contact_at <- function(dta, side, index) {
  cs <- dta_contacts(dta, side)
  if (index < 1 || index > length(cs)) return(NULL)
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
  cols <- lapply(tryCatch(ds@specs@columns, error = function(e) list()) %||% list(),
                 dta_column_to_list)
  rules <- Filter(Negate(is.null),
                  lapply(tryCatch(ds@specs@rules, error = function(e) list()) %||% list(),
                         dta_rule_to_list))
  tryCatch(yaml::as.yaml(list(columns = unname(cols), rules = unname(rules))),
           error = function(e) "")
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
  tables <- list(); vindex <- list(); vstore <- list()
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
  if (is.null(dump)) return(NULL)
  dta <- dump$dta
  tables <- dump$tables %||% list()
  vindex <- dump$vindex %||% list()
  vstore <- dump$vstore %||% list()
  for (nm in names(tables)) {
    ds <- tryCatch(dta@datasets[[nm]], error = function(e) NULL)
    if (is.null(ds)) next
    arrow_tbls <- lapply(tables[[nm]], function(df) {
      if (is.null(df)) return(NULL)
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
      vi <- list(); vs <- list()
    }
    ds@validation_index <- vi
    ds@validation_store <- vs
    dta@datasets[[nm]] <- ds
  }
  dta
}
