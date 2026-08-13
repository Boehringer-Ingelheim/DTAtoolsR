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

# Every directory searched for creation templates, in precedence order:
#
# 1. getOption("DTAtools.template_dir") -- one or more directories, so a site
#    or a project can ship its own template family
# 2. ./dta-templates, for a template kept next to the data it describes
# 3. the packaged directory
#
# The packaged directory sits inside the installed library, which users cannot
# write to and which a reinstall wipes, so it cannot be the only place a
# template may live. Earlier directories win a basename collision.
dta_creation_template_dirs <- function() {
  dirs <- c(
    as.character(getOption("DTAtools.template_dir") %||% character(0)),
    "dta-templates",
    dta_creation_templates_dir()
  )
  dirs <- dirs[nzchar(dirs)]
  dirs[dir.exists(dirs)]
}

# List available creation-template files by basename, across every search
# directory. Templates end in ".dta-template.yaml" or ".dta-template.yml". A
# basename present in more than one directory is listed once and resolves to
# the earliest.
list_dta_creation_templates <- function() {
  files <- unlist(
    lapply(
      dta_creation_template_dirs(),
      list.files,
      pattern = "\\.dta-template\\.ya?ml$",
      ignore.case = TRUE,
      full.names = FALSE
    ),
    use.names = FALSE
  )
  sort(unique(files %||% character(0)))
}

# Resolve creation-template file path by basename, searching in precedence
# order. basename() stays as a path-traversal guard: a template name can never
# escape the directory it was found in.
get_dta_creation_template_path <- function(template_name) {
  if (is.null(template_name) || !nzchar(template_name)) {
    return(NULL)
  }
  nm <- basename(template_name)
  for (dir in dta_creation_template_dirs()) {
    full <- file.path(dir, nm)
    if (file.exists(full)) {
      return(full)
    }
  }
  NULL
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

# The DTAMetaData S7 property definitions, keyed by property name.
dta_metadata_properties <- function() {
  attr(DTAtools::DTAMetaData, "properties") %||% list()
}

# Allowed top-level fields of DTAMetaData for template metadata payloads.
#
# Derived from the S7 class rather than mirrored by hand, so a new DTAMetaData
# property cannot silently become un-settable from a template. `import_issues`
# is excluded deliberately: it records how a file was read, not something a
# template author writes.
dta_template_metadata_fields <- function() {
  setdiff(names(dta_metadata_properties()), "import_issues")
}

# Which of those fields hold a container, and so accept a nested key path.
#
# Decided from the value a freshly constructed DTAMetaData carries: the
# container properties default to list(), the scalar ones to NULL. Deliberately
# NOT decided by comparing the declared property class against S7::class_list --
# that compares S7 class OBJECTS by identity, which holds under
# pkgload::load_all() (same in-memory instance) but fails once the class comes
# back from an installed package's lazy-load database. The suite passed and
# R CMD check did not, until this stopped touching S7 internals.
#
# authorized_for_corrections defaults to NULL -- it is a union of character,
# list and NULL -- so it is correctly treated as a scalar field.
dta_template_list_fields <- function() {
  md <- DTAtools::DTAMetaData()
  nms <- dta_template_metadata_fields()
  nms[vapply(
    nms,
    function(nm) is.list(tryCatch(S7::prop(md, nm), error = function(e) NULL)),
    logical(1)
  )]
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
  # 1) absolute only. A bare relative name must NOT be resolved against the
  # process working directory: the app's cwd is wherever it happened to be
  # launched from, so a packaged template asking for "gf_dataset.yaml" could
  # otherwise silently pick up an unrelated file of that name and quietly build
  # a different DTA. R.utils::isAbsolutePath() gets Windows drive letters and
  # UNC paths right, which a hand-rolled regex does not.
  if (R.utils::isAbsolutePath(ref) && file.exists(ref)) {
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

# The single metadata path an option writes to, if it has exactly one.
# Returns "" for an option that fans out to several paths -- there is no one
# value for such an option to inherit.
dta_template_option_path <- function(opt) {
  eff <- opt$effects %||% list()
  if (length(eff) > 0) {
    # An explicit effects block wins over `target:` in collect_option_effects(),
    # so the inherited default has to follow the same precedence -- otherwise an
    # option could show a default read from one field while writing another.
    sel <- eff[["__selection__"]]
    if (is.list(sel) && length(sel) == 1L && !is.null(sel[[1]]$path)) {
      return(as.character(sel[[1]]$path))
    }
    # A block that fans out to several paths has no single value to inherit.
    return("")
  }
  as.character(opt[["target"]] %||% "")
}

# Default value for one option.
#
# An option that omits `default:` inherits whatever already sits at its target
# path in `base.metadata`, so a template states each value once instead of
# duplicating it between `base` and the option -- a duplication nothing enforced
# and which the option silently won whenever the two drifted apart.
# `base_metadata` is optional so existing callers keep working.
dta_template_default <- function(opt, base_metadata = NULL) {
  d <- opt$default
  if (!is.null(d) && length(d) > 0) {
    return(d[[1]])
  }
  if (is.null(base_metadata) || length(base_metadata) == 0) {
    return(NULL)
  }
  keys <- strsplit(dta_template_option_path(opt), "\\.")[[1]]
  if (length(keys) < 2 || !identical(keys[[1]], "metadata")) {
    return(NULL)
  }
  v <- list_get_path(base_metadata, keys[-1])
  if (is.null(v) || length(v) == 0) NULL else v[[1]]
}

# Should a choice-like option allow custom user entries?
dta_template_allow_custom <- function(opt, default = FALSE) {
  v <- opt$allow_custom
  if (is.null(v) || length(v) == 0) {
    return(default)
  }
  isTRUE(v)
}

# Read a nested value from a list by key path; NULL if any level is missing.
list_get_path <- function(x, keys) {
  cur <- x
  for (k in keys) {
    if (!is.list(cur)) {
      return(NULL)
    }
    cur <- cur[[k]]
    if (is.null(cur)) {
      return(NULL)
    }
  }
  cur
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

  if (!(top %in% dta_template_metadata_fields())) {
    stop(sprintf("Unsupported metadata top-level field '%s' in effect path '%s'.", top, path))
  }

  # The branch turns on the PATH, not on the property's declared type: a nested
  # path is a merge into a container, a bare one is an assignment. That is the
  # distinction that actually matters here, and unlike class introspection it
  # behaves the same under load_all() and under an installed package.
  if (length(tail_keys) > 0) {
    if (!(top %in% dta_template_list_fields())) {
      stop(sprintf(
        "Metadata field '%s' holds a single value, not a nested path ('%s').",
        top, path
      ))
    }
    current <- tryCatch(S7::prop(md, top), error = function(e) NULL) %||% list()
    S7::prop(md, top) <- list_set_path(current, tail_keys, value)
  } else {
    val <- value
    if (identical(top, "date") && !is.null(val) && is.character(val)) {
      parsed <- tryCatch(as.Date(val), error = function(e) as.Date(NA))
      if (is.na(parsed)) {
        stop(sprintf("Invalid date value for metadata.date: '%s'", val))
      }
      val <- parsed
    }
    S7::prop(md, top) <- val
  }

  dta@metadata <- md
  dta
}

# Candidate `effects` keys for one selected value, in match order.
#
# YAML 1.1 parses an unquoted `yes:` / `no:` key as a boolean, and R then names
# the resulting list element "TRUE" / "FALSE"; a quoted `"yes":` key stays
# "yes". The app's boolean control hands us a real logical either way, so both
# spellings have to resolve. Building the candidates here keeps the lookup in
# collect_option_effects() a single first-hit-wins scan, rather than the chain
# of fallback branches it used to be -- that chain worked only because
# as.character(TRUE) and the YAML-coerced name happened to agree.
effect_key_candidates <- function(selected_value) {
  if (is.null(selected_value) || length(selected_value) == 0) {
    return(character(0))
  }
  v <- selected_value[[1]]
  keys <- if (isTRUE(v)) {
    c("TRUE", "yes", "true", "1")
  } else if (identical(v, FALSE)) {
    c("FALSE", "no", "false", "0")
  } else {
    as.character(v)
  }
  keys[!is.na(keys) & nzchar(keys)]
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
  target <- as.character(opt[["target"]] %||% "")
  if (length(eff) == 0 && nzchar(target)) {
    # `target:` is shorthand for what almost every option actually wants --
    # "write my selected value to this one metadata field" -- which otherwise
    # costs a four-line effects/__selection__ block per option. It also works
    # for boolean options, because the __selection__ value path passes the
    # logical straight through, which keeps templates clear of the YAML 1.1
    # yes/no key trap entirely. An explicit `effects:` block wins: `target:` is
    # sugar, not an extra layer.
    eff <- list("__selection__" = list(
      list(path = target, value = "__selection__")
    ))
  }
  if (length(eff) > 0) {
    hit <- NULL
    for (k in effect_key_candidates(selected_value)) {
      if (!is.null(eff[[k]])) {
        hit <- eff[[k]]
        break
      }
    }
    if (is.null(hit) && !is.null(eff[["__selection__"]])) {
      # Custom or unlisted values from text and select inputs.
      hit <- eff[["__selection__"]]
    }
    if (is.null(hit) && !is.null(eff[["path"]])) {
      # A single operation written directly as a map, not keyed by value.
      hit <- list(eff)
    }
    if (!is.null(hit)) {
      out <- c(out, hit)
    }
  }

  if (length(opt$effects_all %||% list()) > 0) {
    out <- c(out, opt$effects_all)
  }

  out
}

# The expression environment that is knowable before a DTA is built. ${version}
# is deliberately absent: it is not settled until the options have been applied,
# so it is resolved afterwards by apply_template_expressions().
dta_template_today_env <- function() {
  list("${today}" = format(Sys.Date(), "%Y-%m-%d"))
}

# Recursively substitute ${...} tokens through a nested list / character value.
# Anything that is neither a string nor a list (a Date, a logical) is returned
# untouched.
resolve_template_expressions <- function(x, env) {
  if (is.character(x)) {
    for (k in names(env)) {
      x <- gsub(k, env[[k]], x, fixed = TRUE)
    }
    return(x)
  }
  if (is.list(x)) {
    return(lapply(x, resolve_template_expressions, env = env))
  }
  x
}

# Resolve ${version} across the metadata of a freshly built DTA.
#
# This runs as a post-pass because ${version} is not knowable while base
# metadata is being written -- an option may still change it. ${today} is
# handled earlier, before base metadata is applied, since metadata.date is a
# Date property and would reject an unresolved token outright.
apply_template_expressions <- function(dta) {
  md <- DTAtools::metadata(dta)
  version <- tryCatch(S7::prop(md, "version"), error = function(e) NULL)
  env <- list("${version}" = as.character(version %||% ""))

  for (nm in dta_template_metadata_fields()) {
    cur <- tryCatch(S7::prop(md, nm), error = function(e) NULL)
    if (is.null(cur)) {
      next
    }
    new <- resolve_template_expressions(cur, env)
    if (!identical(new, cur)) {
      S7::prop(md, nm) <- new
    }
  }

  dta@metadata <- md
  dta
}

# Build a new DTA object from a creation template + selected option values.
# `selections` is a named list keyed by option id.
create_dta_from_template <- function(template_def, template_path, selections = list()) {
  dta_try({
    if (!is.list(template_def)) stop("Template definition is invalid.")
    # ${today} is known upfront and has to be resolved BEFORE base metadata is
    # applied: metadata.date is a Date property, so an unresolved token would be
    # rejected as an invalid date long before any post-pass could see it.
    today_env <- dta_template_today_env()
    base <- resolve_template_expressions(template_def$base %||% list(), today_env)

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
    opts <- resolve_template_expressions(
      template_def$options %||% list(),
      today_env
    )
    for (opt in opts) {
      oid <- as.character(opt$id %||% "")
      if (!nzchar(oid)) next
      chosen <- if (!is.null(selections[[oid]])) {
        selections[[oid]]
      } else {
        dta_template_default(opt, md_base)
      }
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

    # 4) Resolve ${version} now that the options have settled what it is.
    dta <- apply_template_expressions(dta)

    dta
  })
}
