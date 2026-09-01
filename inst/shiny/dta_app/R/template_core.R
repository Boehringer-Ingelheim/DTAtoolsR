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
    # A template that `extends:` another inherits its parent's `base:` and its
    # parent's `datasets:`, so requiring either of its own would make the most
    # useful kind of deviation template impossible to write.
    #
    # This is not hypothetical. Without it, a child that only overrides an
    # option has NO legal way to say "inherit the base verbatim": omitting
    # `base:` fails the check below, and writing `base: {}` is an EXPLICITLY
    # empty child section, which dta_template_merge_section() correctly treats
    # as "replace the parent's with nothing" -- silently wiping every field the
    # parent set. Absent and empty must stay distinguishable, so absent has to
    # be legal here.
    inherits_from <- nzchar(as.character(def$extends %||% ""))

    if (!is.list(def$base) && !inherits_from) {
      stop("Template must contain a 'base' section.")
    }
    if (!is.null(def$base) && !is.list(def$base)) {
      stop("Template 'base' must be a mapping/object.")
    }
    # Accept dataset definitions in either base.datasets (legacy) or top-level
    # datasets (preferred, explicit and reusable across templates). Fold the
    # legacy location up ONLY when the top-level key is genuinely absent: to
    # the inheritance merge, writing `datasets:` and writing nothing at all are
    # different instructions, and `%||%` cannot tell them apart.
    if (!("datasets" %in% names(def)) && !is.null(def$base$datasets)) {
      def$datasets <- def$base$datasets
    }
    if (length(def$datasets %||% list()) == 0 && !inherits_from) {
      stop("Template must define at least one dataset in datasets or base.datasets.")
    }
    # `id` is the one field that is never inherited -- every template file must
    # carry its own -- so defaulting it from the filename here cannot cost the
    # merge any information. Every OTHER shape default now happens in
    # dta_template_finalize_def(), AFTER inheritance has resolved: filling them
    # in here is what used to destroy the absent/empty distinction for
    # `options:` and `datasets:` before resolve_template_inheritance() ever saw
    # the definition, which is why those two sections behaved unlike `base:`.
    def$id <- as.character(def$id %||% tools::file_path_sans_ext(basename(template_path)))
    def
  })
}

# Shape defaults for a FULLY RESOLVED creation template, so server/UI code can
# rely on `label`/`description`/`datasets`/`options` being present without
# every reader repeating a `%||%`.
#
# This must run after resolve_template_inheritance(), never before: an absent
# key is how a child says "inherit", and a default written in ahead of the
# merge turns that into an override with the default.
dta_template_finalize_def <- function(def) {
  def$label <- as.character(def$label %||% def$id %||% "")
  def$description <- as.character(def$description %||% "")
  def$datasets <- def$datasets %||% list()
  def$options <- def$options %||% list()
  def
}

# yaml handlers that keep every scalar number as the text the author wrote.
#
# THE TRAP this exists to close: an unquoted `version: 1.0` in YAML is a
# double, and R renders a double 1.0 as "1" -- so as.character() silently
# turns version 1.0 into version 1. Worse, an unquoted `version: 1.10` is the
# double 1.1 by the time any R code sees it, and 1.10 and 1.1 are different
# versions; once parsed, nothing can tell them apart.
#
# yaml::read_yaml() can hand the RAW TEXT to a handler before that conversion
# happens -- but ONLY under libyaml's own scalar-subtype tag names. A handler
# registered under the obvious name "float" is NEVER invoked, which makes this
# look impossible if you probe it with the wrong name. The tags that actually
# fire are "int", "float#fix" and "float#exp".
#
#   yaml   1.0   -> "1.0"    exact
#   yaml   1.10  -> "1.10"   exact, and distinct from 1.9
#   yaml  "1.10" -> "1.10"   unchanged
#
# Booleans, dates and strings are untouched, so this is safe to apply to a
# whole header document.
dta_template_yaml_handlers <- function() {
  list(
    "int" = as.character,
    "float#fix" = as.character,
    "float#exp" = as.character
  )
}

# Read a YAML file without signalling anything.
#
# The readers below are all documented as reporting failure by RETURN VALUE --
# "returns NA when the file cannot be read", "cannot tell; do not manufacture a
# false positive" -- and each wrapped its read in tryCatch(error = ) to achieve
# that. tryCatch only intercepts the condition class it names, and a file that
# cannot be opened raises a WARNING from the connection before the error: base
# R's "cannot open file '...': No such file or directory". That warning escaped
# every one of them, so a function designed to signal nothing signalled anyway.
#
# It surfaced in the test suite as an unexplained warning attached to a test
# that deliberately reads a missing file. In the app it would reach the R
# console, untranslated into anything the user asked for, and on a non-English
# session it is not even in English -- which is the other reason not to let it
# out: nothing downstream can match on it.
#
# Errors are still captured and returned, because callers report them with
# their own file name in hand.
dta_template_read_yaml_quiet <- function(path, handlers = NULL) {
  withCallingHandlers(
    tryCatch(
      list(
        ok = TRUE,
        value = yaml::read_yaml(path, handlers = handlers),
        error = NULL
      ),
      error = function(e) list(ok = FALSE, value = NULL, error = conditionMessage(e))
    ),
    warning = function(w) invokeRestart("muffleWarning")
  )
}

# Read one top-level scalar field from a template file EXACTLY as written.
#
# Used where the rest of the document must be parsed normally -- a dataset
# template's `dataset:` body needs its real numeric types -- but a single
# header field must survive verbatim. Re-parsing a small header file twice is
# far cheaper than getting a version number wrong.
#
# Returns NA_character_ when the file cannot be read or the field is absent;
# the caller reports that with its own file name in hand.
dta_template_read_field_exact <- function(path, field) {
  res <- dta_template_read_yaml_quiet(path, handlers = dta_template_yaml_handlers())
  out <- res$value
  if (!is.list(out) || is.null(out[[field]]) || length(out[[field]]) == 0) {
    return(NA_character_)
  }
  as.character(out[[field]][[1]])
}

# Normalise an ALREADY-PARSED `version:` value to a character string.
#
# The reader above is the correct route and loses nothing. This is the fallback
# for a value that has already been through a plain parse -- a definition built
# in R, or a file that could not be re-read -- where the damage is already done
# and only a best effort is possible. It warns rather than guessing silently,
# because at this point 1.10 and 1.1 really are indistinguishable.
#
# `what` names the file or id in the warning so the author can find it.
dta_template_version_string <- function(version, what = "template") {
  if (is.null(version) || length(version) == 0) {
    return(NA_character_)
  }
  v <- version[[1]]
  if (is.character(v)) {
    return(v)
  }
  if (is.numeric(v)) {
    out <- format(v, nsmall = 1L, trim = TRUE, scientific = FALSE)
    cli::cli_warn(c(
      "Unquoted version {.val {v}} in {.field {what}} was read as a number.",
      "i" = "Using {.val {out}}. Quote it as {.code version: \"{out}\"} to be exact.",
      "!" = "An unquoted {.code 1.10} is read as {.code 1.1} and cannot be recovered."
    ))
    return(out)
  }
  as.character(v)
}

# Is this version exact as written, i.e. was it quoted in the YAML?
#
# Split out from dta_template_version_string() so validate_template() can fail
# on an ambiguous version without having to re-parse the file or catch a
# warning.
dta_template_version_is_exact <- function(version) {
  !is.null(version) && length(version) > 0 && is.character(version[[1]])
}

# The DTAMetaData S7 property definitions, keyed by property name.
dta_metadata_properties <- function() {
  attr(DTAtools::DTAMetaData, "properties") %||% list()
}

# Metadata fields the engine owns and a specification author must never set.
#
# `import_issues` records how a file was read; `template` records which template
# produced the document. Both are written by machinery, and a template that
# could set either could forge its own provenance -- which is exactly what the
# rebase feature would then trust.
dta_metadata_machine_fields <- function() {
  c("import_issues", "template")
}

# Allowed top-level fields of DTAMetaData for template metadata payloads.
#
# Derived from the S7 class rather than mirrored by hand, so a new DTAMetaData
# property cannot silently become un-settable from a template. The
# machine-owned fields (see dta_metadata_machine_fields()) are excluded
# deliberately: they are written by machinery, not a template author.
dta_template_metadata_fields <- function() {
  setdiff(names(dta_metadata_properties()), dta_metadata_machine_fields())
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

# Read the value at a dotted path in a BUILT DOCUMENT, or NULL.
#
# The document's own vocabulary is rooted at `metadata`, and its top level is
# S7 properties rather than list elements -- so the first hop is S7::prop() and
# only what lies below it is an ordinary nested list. Anything that is not a
# `metadata.…` path returns NULL; a template addresses its own sections through
# dta_template_path_get() (template_inherit.R) instead.
dta_template_document_path_get <- function(dta, path) {
  parts <- strsplit(as.character(path %||% ""), ".", fixed = TRUE)[[1]]
  if (length(parts) == 0 || !identical(parts[[1]], "metadata")) {
    return(NULL)
  }
  md <- DTAtools::metadata(dta)
  # `base.metadata` with no field after it means "the metadata block as a
  # whole". Rejecting it for having too few segments would make such a
  # requirement permanently unsatisfiable rather than merely unusual.
  if (length(parts) == 1) {
    return(as.list(md))
  }
  value <- tryCatch(S7::prop(md, parts[[2]]), error = function(e) NULL)
  if (length(parts) == 2) {
    return(value)
  }
  list_get_path(value, parts[-c(1, 2)])
}

# Warn about a sealed path that matches nothing in the template being built.
#
# A dead seal fails SILENTLY -- the path resolves to NULL before and after the
# merge, so every descendant compares equal and passes -- and only a typo or a
# since-renamed field ever produces one. validate_template() reports it as
# `sealed_path_unknown`, but that is opt-in and lives in the template
# repository's CI; a seal is worth nothing if the guarantee depends on
# remembering to run a linter. This says the same thing at build time, where it
# cannot be skipped.
#
# A WARNING rather than an error, deliberately: an abstract parent may seal a
# path that only its descendants create, so an unresolvable seal is not by
# itself proof of a mistake.
dta_template_warn_dead_seals <- function(template_def) {
  dead <- Filter(
    function(path) is.null(dta_template_path_get(template_def, path)),
    as.character(template_def$sealed %||% character(0))
  )
  if (length(dead) > 0) {
    n <- length(dead)
    shown <- paste(dead, collapse = ", ")
    cli::cli_warn(c(
      "{n} sealed path{?s} match{?es/} nothing in this template: {shown}.",
      i = "A sealed path that resolves to nothing protects nothing, because every descendant compares equal to it."
    ))
  }
  invisible(NULL)
}

# Abort unless every path an ancestor marked `required:` holds a real value.
#
# ONE path vocabulary, two places to look it up. A `base.…` path is resolved
# against the built DOCUMENT with the prefix dropped -- `base.metadata.supplier`
# becomes `metadata.supplier` -- because `base:` is the metadata seed, so a
# field the USER filled through a party or vocabulary slot satisfies the
# requirement exactly as a descendant template setting it would. Anything else
# (`options.…`, `datasets.…`) has no document counterpart and is resolved
# against the resolved template definition.
dta_template_check_required <- function(dta, template_def) {
  paths <- as.character(template_def$required %||% character(0))
  if (length(paths) == 0) {
    return(invisible(NULL))
  }

  filled <- vapply(paths, function(path) {
    value <- if (startsWith(path, "base.")) {
      dta_template_document_path_get(dta, sub("^base\\.", "", path))
    } else {
      dta_template_path_get(template_def, path)
    }
    dta_template_path_is_filled(value)
  }, logical(1))

  if (any(!filled)) {
    n <- sum(!filled)
    shown <- paste(paths[!filled], collapse = ", ")
    template_id <- as.character(template_def$id %||% "<unknown>")
    cli::cli_abort(c(
      "{n} required field{?s} not filled in template {.val {template_id}}: {shown}.",
      i = "An ancestor's {.field required:} names them. Set them in this template, or choose a value for them when creating the document."
    ))
  }
  invisible(NULL)
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
#
# `selections` is a named list keyed by option id (unchanged from before this
# function grew the four arguments below).
#
# The four new arguments each default to exactly what reproduces the ORIGINAL
# behaviour of this function -- every one of the 55 tests that predate them
# calls this with none of the four set, and must keep passing unmodified:
#
#   index            when supplied, `datasets:` entries are built via
#                    build_template_datasets() (template_create.R), which
#                    additionally understands a `template:` entry (built from
#                    a dataset template resolved through `index`) on top of
#                    the three legacy forms. When NULL (the default), dataset
#                    building takes the ORIGINAL inline code path below,
#                    verbatim -- so a `template:` entry is simply unreachable
#                    without an index, exactly as it always has been.
#   carry_over       list(metadata = <DTAMetaData>, fields = <character>) --
#                    an ancestor document's metadata to carry onto this one
#                    (see apply_metadata_carry_over(), template_create.R).
#                    NULL (default) carries nothing over.
#   party_selections named list, slot id -> chosen party-profile id. NULL
#                    (default) selects nothing; harmless even when the
#                    template DOES declare `party_slots:`, since
#                    apply_party_selections() (party_profiles.R) leaves an
#                    unselected slot untouched.
#   provenance       the already-built `metadata.template` record (see
#                    template_provenance(), template_create.R) to stamp onto
#                    the finished document. NULL (default) stamps nothing, so
#                    a document built without one carries no @template at
#                    all -- exactly the pre-existing behaviour, since
#                    @template did not exist as a settable concept before this
#                    file's own history.
#
# The ORDER these six steps run in is the contract, and is deliberately NOT
# "whatever order was easiest to bolt on":
#
#   1. datasets   2. base.metadata   3. carry-over   4. party slots
#   5. options    6. ${version}      7. provenance (LAST)
#
# Carry-over (3) runs before party slots (4) and options (5) so that a value
# THIS document's own author actively chooses always beats whatever an
# ancestor document happened to have -- carrying the old value over first and
# letting the current author's choices overwrite it is what makes "the
# current choice wins" true without a separate precedence table anywhere.
# Provenance (7) is last because it is a RECORD of what steps 1-6 decided
# (which dataset templates were used, which party profile was picked, which
# options were chosen) -- it cannot be written before those decisions exist.
create_dta_from_template <- function(template_def, template_path, selections = list(),
                                     index = NULL, carry_over = NULL,
                                     party_selections = NULL, provenance = NULL,
                                     vocab_selections = NULL) {
  dta_try({
    if (!is.list(template_def)) stop("Template definition is invalid.")
    # ${today} is known upfront and has to be resolved BEFORE base metadata is
    # applied: metadata.date is a Date property, so an unresolved token would be
    # rejected as an invalid date long before any post-pass could see it.
    today_env <- dta_template_today_env()
    base <- resolve_template_expressions(template_def$base %||% list(), today_env)

    # 1) Build datasets from the template datasets section.
    #
    # A `template:` dataset entry can only be resolved through the index, so a
    # caller that did not supply one gets it built on demand rather than an
    # error. WHY this is not merely a convenience: the moment ANY template --
    # including a packaged one -- imports a reusable dataset template, every
    # existing caller that legitimately passes no index (the standalone
    # readers, the examples, the pre-existing tests) would otherwise break.
    # Requiring every call site to learn about the index in order for a
    # template AUTHOR to refactor their file is the wrong coupling.
    #
    # A template using only the three legacy dataset forms never triggers this,
    # so no existing caller pays for the index scan.
    ds_refs_probe <- template_def$datasets %||% base$datasets %||% list()
    if (is.null(index) && length(ds_refs_probe) > 0) {
      needs_index <- any(vapply(
        ds_refs_probe,
        function(r) identical(template_dataset_entry_kind(r), "template"),
        logical(1)
      )) ||
        # A vocabulary slot, or a column binding inside an inline dataset
        # body, is resolved through the index too -- same reasoning as the
        # `template:` probe above: a template AUTHOR adding one must not break
        # every existing caller that legitimately passes no index.
        length(template_def$vocabulary_slots %||% list()) > 0 ||
        any(vapply(ds_refs_probe, function(r) is.list(r) && dataset_has_vocabulary_binding(r), logical(1)))
      if (needs_index) {
        index <- dta_template_index_cached()
      }
    }

    # Vocabulary slots are resolved BEFORE the datasets are built, because
    # their effect is written into the plain dataset lists on the way through
    # build_template_datasets() -- not onto the finished S7 objects, which
    # would mean column-spec surgery for something the list form expresses
    # directly.
    vocab_slots <- normalise_vocabulary_slots(template_def$vocabulary_slots)
    vocab_overrides <- list()
    if (length(vocab_slots) > 0) {
      if (is.null(index)) {
        stop("This template offers vocabulary slots, which need a template index to resolve.")
      }
      vocab_overrides <- resolve_vocabulary_slot_overrides(
        vocab_slots, vocab_selections %||% list(), vocabulary_resolver(index)
      )
    }

    if (!is.null(index)) {
      # New path: routes a `template:` dataset entry through the dataset-
      # template machinery, and reproduces the three legacy forms exactly (see
      # build_template_datasets(), template_create.R). `source_label = NULL`:
      # create_dta_from_template() has no notion of "this document was
      # uploaded vs. drawn from a template" the way the standalone "add a
      # dataset from a template" UI flow will -- that label is for THAT flow
      # to supply when it exists, not something this function can infer.
      built <- build_template_datasets(
        template_def, index, selections,
        source_label = NULL, template_path = template_path,
        vocab_overrides = vocab_overrides
      )
      ds_list <- built$datasets
      # Keep the provenance the ACTUAL build produced. Step 7 stamps this over
      # whatever the caller precomputed, so the recorded dataset lineage and
      # deviations can never describe a different build than the one that
      # happened -- see the comment there.
      built_ds_provenance <- built$provenance
    } else {
      # Original path, UNCHANGED: every pre-existing caller (none of which
      # passes `index`) must keep taking exactly this code, not a
      # reimplementation of it that merely intends to behave the same way.
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

    # 3) Carry over metadata from an ancestor document (rebase), BEFORE party
    # slots and options so that anything THIS creation actively decides
    # (below) overwrites whatever the ancestor had, never the other way round.
    if (!is.null(carry_over)) {
      dta <- apply_metadata_carry_over(dta, carry_over$metadata, carry_over$fields)
    }

    # 4) Party slots: a profile chosen FOR THIS document overrides whatever
    # carry-over (3) just wrote to the same target, for the identical reason
    # options (5) are applied after this -- the current author's active choice
    # always wins over an inherited one.
    party_slots <- normalise_party_slots(template_def$party_slots)
    if (length(party_slots) > 0) {
      party_profiles <- if (!is.null(index)) template_party_profiles(index) else list()
      dta <- apply_party_selections(dta, party_slots, party_selections %||% list(), party_profiles)
    }

    # 5) Apply option-driven effects.
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

    # 6) Resolve ${version} now that the options have settled what it is.
    dta <- apply_template_expressions(dta)

    # 6a) `required:` -- checked HERE, after every selection has been applied
    # and before provenance records the result. This is the first moment the
    # document is complete, and it is what lets a required field be satisfied
    # either by a descendant template setting it or by the person creating the
    # document choosing it: both have already happened by now, and a check any
    # earlier could not tell a deferred field from an unfilled one.
    dta_template_check_required(dta, template_def)
    dta_template_warn_dead_seals(template_def)

    # 7) Provenance, LAST: it records what steps 1-6 decided, so it cannot be
    # written before they run. Assigned directly onto the property, NOT
    # through apply_template_metadata_path() -- that function validates
    # against dta_template_metadata_fields(), which deliberately EXCLUDES
    # `template` (dta_metadata_machine_fields(), above): that gate exists
    # precisely so a template AUTHOR can never set this field from
    # `base.metadata` or an option effect, and routing the ENGINE's own write
    # through the same gate would defeat the very check it exists to enforce.
    if (!is.null(provenance)) {
      # The caller assembles the provenance record BEFORE calling this
      # function, which means its `datasets` section was computed from a
      # separate build. Overwrite it with what step 1 actually produced.
      #
      # WHY this matters more than the duplicated work it papers over: the
      # rebase feature reconstructs a document's ancestor from this record. A
      # provenance block describing datasets that were never built is worse
      # than no provenance at all, because rebase would trust it and merge
      # against the wrong ancestor. Making the stamp read from the real build
      # means the two cannot disagree even if a caller passes a stale record.
      if (exists("built_ds_provenance", inherits = FALSE)) {
        provenance$datasets <- built_ds_provenance
        if (length(provenance$datasets) == 0) {
          provenance$datasets <- NULL
        }
      }
      md <- DTAtools::metadata(dta)
      S7::prop(md, "template") <- provenance
      dta@metadata <- md
    }

    dta
  })
}
