# -----------------------------------------------------------------------------
# Template core for "Create new from template" on the app landing page.
#
# Purpose:
# - Discover DTA-creation templates (*.dta-template.yaml)
# - Read and validate template definitions
# - Build a NEW DTA object from template base + selected options
# - Apply option effects where one choice can set multiple metadata fields
#
# Design goals:
# - declarative YAML templates (expandable to many template families)
# - deterministic behavior (no hidden side effects)
# - future-friendly option model (select/boolean/text/number)
# -----------------------------------------------------------------------------

# Built-in creation template directory (inside package extdata).
dta_creation_templates_dir <- function() {
  system.file("extdata", "templates", package = "DTAtools")
}

# List available creation-template files by basename.
# Custom templates can be added to the same folder, as long as they end in
# ".dta-template.yaml" or ".dta-template.yml".
list_dta_creation_templates <- function() {
  dir <- dta_creation_templates_dir()
  if (!nzchar(dir) || !dir.exists(dir)) {
    return(character(0))
  }
  files <- list.files(dir,
    pattern = "\\.dta-template\\.ya?ml$",
    ignore.case = TRUE,
    full.names = FALSE
  )
  sort(files)
}

# Resolve creation-template file path by basename.
get_dta_creation_template_path <- function(template_name) {
  if (is.null(template_name) || !nzchar(template_name)) {
    return(NULL)
  }
  full <- file.path(dta_creation_templates_dir(), basename(template_name))
  if (file.exists(full)) full else NULL
}

# Read and minimally validate a creation template YAML.
# Returns dta_try() with value = normalized template list.
read_dta_creation_template <- function(template_path) {
  dta_try({
    if (is.null(template_path) || !nzchar(template_path) || !file.exists(template_path)) {
      stop("Template file not found.")
    }
    def <- yaml::read_yaml(template_path)
    if (!is.list(def)) stop("Template YAML must be a mapping/object.")
    kind <- as.character(def$kind %||% "")
    if (!identical(kind, "dta_creation_template")) {
      stop("Template 'kind' must be 'dta_creation_template'.")
    }
    if (!is.list(def$base)) stop("Template must contain a 'base' section.")
    # Accept dataset definitions in either base.datasets (legacy) or top-level
    # datasets (preferred, explicit and reusable across templates).
    datasets_def <- def$datasets %||% def$base$datasets %||% list()
    if (length(datasets_def) == 0) {
      stop("Template must define at least one dataset in datasets or base.datasets.")
    }
    # Normalize optional pieces so server/UI code can rely on shape.
    def$id <- as.character(def$id %||% tools::file_path_sans_ext(basename(template_path)))
    def$label <- as.character(def$label %||% def$id)
    def$description <- as.character(def$description %||% "")
    def$datasets <- datasets_def
    def$options <- def$options %||% list()
    def
  })
}

# Allowed top-level fields of DTAMetaData for template metadata payloads.
dta_template_metadata_fields <- function() {
  c(
    "title", "version", "date", "header",
    "version_history", "receiver", "supplier", "transmission",
    "error_handling", "authorized_for_corrections"
  )
}

# Resolve a dataset reference from template YAML.
# Resolution order:
# 1) absolute path
# 2) relative to template file directory
# 3) package extdata root
resolve_template_dataset_path <- function(ref, template_path) {
  if (is.null(ref) || !nzchar(ref)) {
    return("")
  }
  # 1) absolute/direct
  if (file.exists(ref)) {
    return(normalizePath(ref, winslash = "/", mustWork = TRUE))
  }

  # 2) relative to template file
  td <- dirname(template_path %||% "")
  if (nzchar(td)) {
    p2 <- file.path(td, ref)
    if (file.exists(p2)) {
      return(normalizePath(p2, winslash = "/", mustWork = TRUE))
    }
  }

  # 3) package extdata root
  p3 <- system.file("extdata", ref, package = "DTAtools")
  if (nzchar(p3) && file.exists(p3)) {
    return(normalizePath(p3, winslash = "/", mustWork = TRUE))
  }

  ""
}

# Convert option definition choices into a named vector for selectInput.
# Supports either:
# - choices: ["yes", "no"]
# - choices:
#   - value: "yes"
#     label: "Yes"
#   - value: "no"
#     label: "No"
dta_template_choices <- function(opt) {
  ch <- opt$choices %||% list()
  if (length(ch) == 0) {
    return(character(0))
  }
  if (is.character(ch)) {
    return(stats::setNames(ch, ch))
  }
  # list entries
  vals <- vapply(ch, function(x) as.character(x$value %||% ""), character(1))
  labs <- vapply(ch, function(x) as.character(x$label %||% x$value %||% ""), character(1))
  ok <- nzchar(vals)
  stats::setNames(vals[ok], labs[ok])
}

# Default value for one option.
dta_template_default <- function(opt) {
  d <- opt$default
  if (is.null(d) || length(d) == 0) {
    return(NULL)
  }
  d[[1]]
}

# Should a choice-like option allow custom user entries?
dta_template_allow_custom <- function(opt, default = FALSE) {
  v <- opt$allow_custom
  if (is.null(v) || length(v) == 0) {
    return(default)
  }
  isTRUE(v)
}

# Set nested value in list by key path. If value is NULL, key is removed.
list_set_path <- function(x, keys, value) {
  if (!is.list(x)) x <- list()
  if (length(keys) == 0) {
    return(x)
  }
  k <- keys[[1]]
  if (length(keys) == 1) {
    if (is.null(value)) {
      x[[k]] <- NULL
    } else {
      x[[k]] <- value
    }
    return(x)
  }
  child <- x[[k]]
  child <- list_set_path(child %||% list(), keys[-1], value)
  x[[k]] <- child
  x
}

# Apply one metadata path update to DTA object.
# Supported path root is "metadata".
apply_template_metadata_path <- function(dta, path, value) {
  parts <- strsplit(as.character(path %||% ""), "\\.")[[1]]
  if (length(parts) < 2 || !identical(parts[[1]], "metadata")) {
    stop(sprintf("Unsupported effect path '%s'.", path))
  }

  md <- DTAtools::metadata(dta)
  top <- parts[[2]]
  tail_keys <- parts[-c(1, 2)]

  if (top %in% c("title", "version", "date", "header", "error_handling", "authorized_for_corrections")) {
    val <- value
    if (identical(top, "date") && !is.null(val) && is.character(val)) {
      parsed <- tryCatch(as.Date(val), error = function(e) as.Date(NA))
      if (is.na(parsed)) {
        stop(sprintf("Invalid date value for metadata.date: '%s'", val))
      }
      val <- parsed
    }
    S7::prop(md, top) <- val
  } else if (top %in% c("transmission", "receiver", "supplier", "version_history")) {
    current <- tryCatch(S7::prop(md, top), error = function(e) NULL) %||% list()
    if (length(tail_keys) == 0) {
      current <- value
    } else {
      current <- list_set_path(current, tail_keys, value)
    }
    S7::prop(md, top) <- current
  } else {
    stop(sprintf("Unsupported metadata top-level field '%s' in effect path '%s'.", top, path))
  }

  dta@metadata <- md
  dta
}

# Extract operations for one selected option value.
# Option `effects` format:
# effects:
#   yes:
#     - path: metadata.transmission.blinded
#       value: true
#     - path: metadata.transmission.blinded_description
#       value: "..."
# Also supports an always-applied list in `effects_all`.
collect_option_effects <- function(opt, selected_value) {
  out <- list()

  eff <- opt$effects %||% list()
  key <- as.character(selected_value %||% "")
  if (identical(key, "") && !is.null(eff[["__selection__"]])) {
    key <- "__selection__"
  }
  if (length(eff) > 0) {
    if (!is.null(eff[[key]])) {
      out <- c(out, eff[[key]])
    } else if (!is.null(eff[["__selection__"]])) {
      # Fallback for custom/unlisted values in text/select inputs.
      out <- c(out, eff[["__selection__"]])
    } else if (isTRUE(selected_value)) {
      for (k in c("yes", "true", "1")) if (!is.null(eff[[k]])) out <- c(out, eff[[k]])
    } else if (identical(selected_value, FALSE)) {
      for (k in c("no", "false", "0")) if (!is.null(eff[[k]])) out <- c(out, eff[[k]])
    } else if (is.list(eff) && !is.null(eff$path)) {
      # direct single operation map
      out <- c(out, list(eff))
    }
  }

  if (length(opt$effects_all %||% list()) > 0) {
    out <- c(out, opt$effects_all)
  }

  out
}

# Build a new DTA object from a creation template + selected option values.
# `selections` is a named list keyed by option id.
create_dta_from_template <- function(template_def, template_path, selections = list()) {
  dta_try({
    if (!is.list(template_def)) stop("Template definition is invalid.")
    base <- template_def$base %||% list()

    # 1) Build datasets from the template datasets section.
    ds_refs <- template_def$datasets %||% base$datasets %||% list()
    ds_list <- list()
    for (i in seq_along(ds_refs)) {
      ref <- ds_refs[[i]]
      ds <- NULL

      # accepted forms:
      # - "gf_dataset.yaml"
      # - { source: "gf_dataset.yaml" }
      # - full inline dataset object (name/type/columns/...)
      if (is.character(ref)) {
        src <- ref
        p <- resolve_template_dataset_path(src, template_path)
        if (!nzchar(p)) {
          stop(sprintf("Could not resolve dataset source '%s' for template '%s'.", src, template_def$label %||% ""))
        }
        ds <- DTAtools::read_dataset_from_yaml(p)
      } else if (is.list(ref) && nzchar(as.character(ref$source %||% ""))) {
        src <- as.character(ref$source %||% "")
        p <- resolve_template_dataset_path(src, template_path)
        if (!nzchar(p)) {
          stop(sprintf("Could not resolve dataset source '%s' for template '%s'.", src, template_def$label %||% ""))
        }
        ds <- DTAtools::read_dataset_from_yaml(p)
      } else if (is.list(ref)) {
        tf <- tempfile(fileext = ".yaml")
        yaml_txt <- yaml::as.yaml(ref, indent = 2, line.sep = "\n")
        writeLines(yaml_txt, tf, useBytes = TRUE)
        ds <- DTAtools::read_dataset_from_yaml(tf)
      }

      if (is.null(ds)) {
        stop(sprintf("Invalid dataset definition at index %s in template '%s'.", i, template_def$label %||% ""))
      }
      ds_list[[length(ds_list) + 1L]] <- ds
    }

    dta <- DTAtools::DTA(datasets = ds_list)

    # 2) Apply base metadata defaults.
    md_base <- base$metadata %||% list()
    if (length(md_base) > 0) {
      allowed <- dta_template_metadata_fields()
      unknown <- setdiff(names(md_base), allowed)
      if (length(unknown) > 0) {
        stop(sprintf(
          "Unknown base.metadata field(s): %s. Allowed fields: %s.",
          paste(unknown, collapse = ", "),
          paste(allowed, collapse = ", ")
        ))
      }
      for (k in names(md_base)) {
        v <- md_base[[k]]
        if (k %in% allowed) {
          dta <- apply_template_metadata_path(dta, paste0("metadata.", k), v)
        }
      }
    }

    # 3) Apply option-driven effects.
    opts <- template_def$options %||% list()
    for (opt in opts) {
      oid <- as.character(opt$id %||% "")
      if (!nzchar(oid)) next
      chosen <- if (!is.null(selections[[oid]])) selections[[oid]] else dta_template_default(opt)
      effects <- collect_option_effects(opt, chosen)
      if (length(effects) == 0) next

      for (op in effects) {
        if (!is.list(op)) next

        # Either explicit path/value pair or a set-map with multiple pairs.
        if (!is.null(op$set) && is.list(op$set)) {
          for (p in names(op$set)) {
            val <- op$set[[p]]
            if (is.character(val) && identical(val, "__selection__")) val <- chosen
            dta <- apply_template_metadata_path(dta, p, val)
          }
        } else {
          p <- as.character(op$path %||% "")
          if (!nzchar(p)) next
          val <- op$value
          if (is.character(val) && identical(val, "__selection__")) val <- chosen
          dta <- apply_template_metadata_path(dta, p, val)
        }
      }
    }

    dta
  })
}
