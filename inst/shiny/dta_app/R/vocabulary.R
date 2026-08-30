# -----------------------------------------------------------------------------
# Controlled vocabularies: reusable, versioned term sets that a column's
# permitted `values:` can be drawn from, instead of every dataset template
# retyping (or worse, drifting copies of) the same list of visit codes, test
# codes, or Y/N flags.
#
# File kind (kind: dta_vocabulary), registered with the template index
# (template_index.R: dta_template_all_kinds(), "*.dta-vocabulary.ya?ml$"):
#
#   kind: dta_vocabulary
#   id: visit
#   version: "1.0"
#   label: Visit identifiers
#   description: Standard visit codes for the biomarker GF family
#   type: text                 # text | number | integer
#   terms:
#     - code: SCR
#       label: Screening
#       description: Pre-randomisation screening visit.
#     - code: C1D1
#       label: Cycle 1 Day 1
#
# A bare string in `terms:` is shorthand for `{ code: <string> }`.
#
# One vocabulary can extend another. This is NOT the generic `extends:` merge
# in template_inherit.R, and cannot be: dta_template_merge_value() replaces
# SEQUENCES wholesale, so "inherit these terms and add one" is simply not
# expressible that way. Extension here is explicit instead:
#
#   extends: visit@1.0
#   add_terms: [{ code: C2D1, label: Cycle 2 Day 1 }]
#   remove_terms: [EOT]
#
# WHAT THIS FILE DELIBERATELY DOES NOT DO: it never produces a DTAColumnSpec
# property of its own. A vocabulary is EXPANDED into an ordinary `values:`
# vector before any spec is built (see expand_column_vocabularies() in
# dataset_template.R), so `values_from:` never reaches DTAColumnSpec and never
# survives into a produced document. A DTA YAML handed to a supplier has to be
# readable without access to the vocabulary library that shaped it.
#
# `%||%` and `dta_try()` are already bound in the app helper environment (see
# utils_dta.R) -- not redefined here, same convention as party_profiles.R.
# -----------------------------------------------------------------------------

# ---- Reading ----------------------------------------------------------------

# yaml scalar handlers for a vocabulary file. WIDER than the shared
# dta_template_yaml_handlers() (template_core.R), and it has to be.
#
# That set covers "int", "float#fix" and "float#exp", which is exactly right
# for a `version:` field. A term CODE, though, is drawn from a much nastier
# corner of YAML 1.1, and every one of the following is a real, measured
# result of parsing a vocabulary file with only the shared handler set:
#
#   code: 01     -> int 1        leading zero read as OCTAL, padding lost
#   code: 007    -> int 7        same
#   code: 0x1F   -> int 31       read as HEX
#   code: Y      -> logi TRUE    YAML 1.1 boolean
#   code: N      -> logi FALSE   YAML 1.1 boolean
#   code: NO     -> logi FALSE   YAML 1.1 boolean
#
# Zero-padded codes and Y/N flags are not exotic edge cases in this domain --
# they are two of the most common controlled vocabularies there are. The
# shared handler set does not close them because its "int" handler is never
# invoked for an octal/hex scalar (those carry their own subtype tags) and it
# registers nothing at all for booleans.
#
# Adding these tags to the SHARED set instead would be wrong: the dataset
# templates rely on `nullable: No` parsing as FALSE, and turning that into the
# string "No" would break every column spec built from one. The wider set is
# therefore scoped to vocabulary files, where a boolean has no meaning and
# every scalar is either a code or a piece of display text.
#
# Coercion back to a number, for a vocabulary that genuinely holds numbers, is
# the job of `type:` below -- and it can only be done correctly from the
# original text, which is what these handlers preserve.
dta_vocabulary_yaml_handlers <- function() {
  c(
    dta_template_yaml_handlers(),
    list(
      "int#oct" = as.character,
      "int#hex" = as.character,
      "bool#yes" = as.character,
      "bool#no" = as.character
    )
  )
}

# The value types a vocabulary may declare.
dta_vocabulary_types <- function() {
  c("text", "number", "integer")
}

# Coerce the preserved source text of every term code to the vocabulary's
# declared `type:`.
#
# Kept separate from reading so the failure names the vocabulary AND the
# offending code: "type: integer but 'SCR' is not an integer" is actionable,
# whereas a vector of NAs appearing later in a column's permitted values is
# not traceable to anything.
.coerce_vocabulary_codes <- function(codes, type, vocab_id) {
  if (identical(type, "text")) {
    return(as.character(codes))
  }

  txt <- as.character(codes)
  out <- suppressWarnings(
    if (identical(type, "integer")) as.integer(txt) else as.numeric(txt)
  )

  bad <- is.na(out) & !is.na(txt)
  if (any(bad)) {
    cli::cli_abort(c(
      "Vocabulary {.val {vocab_id}} declares {.code type: {type}} but has non-{type} code{?s}.",
      "x" = "Offending: {.val {txt[bad]}}.",
      "i" = "Either quote them and use {.code type: text}, or correct the codes."
    ))
  }
  out
}

# Turn a raw parsed `terms:`/`add_terms:` sequence into a list of
# list(code=, label=, description=).
#
# Malformed entries are dropped in ONE counted warning rather than one warning
# per entry: the same reasoning as .normalise_party_contacts() and
# dta_template_drop_unidentified() -- expect_warning() captures only the FIRST
# matching warning and reports every later one as an unexpected extra, so a
# per-item warning makes the behaviour untestable.
#
# A duplicate code keeps the FIRST occurrence. Order is otherwise preserved
# exactly as authored, because a vocabulary's order is meaningful: it is the
# order the picker lists terms in and the order they land in `values:`, and an
# author who grouped visits chronologically did so on purpose.
.normalise_vocabulary_terms <- function(terms, vocab_id, what = "terms") {
  terms <- terms %||% list()
  if (length(terms) == 0) {
    return(list())
  }

  out <- list()
  malformed <- 0L
  for (t in terms) {
    # Shorthand: a bare scalar is a term with only a code.
    if (!is.list(t)) {
      if (length(t) != 1 || is.na(t) || !nzchar(as.character(t))) {
        malformed <- malformed + 1L
        next
      }
      out[[length(out) + 1L]] <- list(code = as.character(t), label = NULL, description = NULL)
      next
    }

    code <- t$code
    if (is.null(code) || length(code) != 1 || is.na(code) || !nzchar(as.character(code))) {
      malformed <- malformed + 1L
      next
    }

    out[[length(out) + 1L]] <- list(
      code = as.character(code),
      # NULL, not "", so vocabulary_values(field = "label") can fall back to
      # the code for a term that never had a label -- an empty string would
      # silently become an empty permitted value instead.
      label = if (is.null(t$label)) NULL else as.character(t$label),
      description = if (is.null(t$description)) NULL else as.character(t$description)
    )
  }

  if (malformed > 0L) {
    cli::cli_warn(
      "Vocabulary {.val {vocab_id}}: dropping {malformed} malformed {what} entr{?y/ies} (no usable {.field code})."
    )
  }

  codes <- vapply(out, function(t) t$code, character(1))
  dup <- duplicated(codes)
  if (any(dup)) {
    cli::cli_warn(
      "Vocabulary {.val {vocab_id}}: dropping {sum(dup)} duplicate {what} code{?s} ({.val {unique(codes[dup])}}); the first occurrence wins."
    )
    out <- out[!dup]
  }

  out
}

# Read and minimally validate a vocabulary YAML.
#
# Never throws for a bad FILE -- dta_try() turns that into list(ok = FALSE,
# error =) so one unreadable vocabulary cannot take the picker down, exactly
# as read_party_profile() does. It DOES throw for a bad `type:` coercion,
# because that is a content error the author must fix and silently serving a
# vocabulary of NAs would be worse.
#
# The returned list keeps `extends`/`add_terms`/`remove_terms` unresolved;
# resolve_vocabulary_inheritance() is a separate step so a caller that only
# wants the header (the index, the picker's label column) never pays for
# resolving a chain.
read_vocabulary <- function(path) {
  dta_try({
    if (is.null(path) || !nzchar(path) || !file.exists(path)) {
      stop("Vocabulary file not found.")
    }
    def <- yaml::read_yaml(path, handlers = dta_vocabulary_yaml_handlers())
    if (!is.list(def)) stop("Vocabulary YAML must be a mapping/object.")

    kind <- as.character(def$kind %||% "")
    if (!identical(kind, "dta_vocabulary")) {
      stop("Vocabulary 'kind' must be 'dta_vocabulary'.")
    }

    id <- as.character(def$id %||% "")
    if (!nzchar(id)) {
      stop("Vocabulary must define a non-empty 'id'.")
    }

    if (is.null(def$version) || length(def$version) == 0 || !nzchar(as.character(def$version))) {
      stop("Vocabulary must define a non-empty 'version'.")
    }

    type <- as.character(def$type %||% "text")
    if (!(type %in% dta_vocabulary_types())) {
      stop(sprintf(
        "Vocabulary '%s' has invalid type '%s'; must be one of %s.",
        id, type, paste(dta_vocabulary_types(), collapse = ", ")
      ))
    }

    extends <- as.character(def$extends %||% "")
    terms <- .normalise_vocabulary_terms(def$terms, id, "terms")
    add_terms <- .normalise_vocabulary_terms(def$add_terms, id, "add_terms")

    if (!nzchar(extends) && length(terms) == 0) {
      stop(sprintf(
        "Vocabulary '%s' defines no 'terms' and does not 'extends:' another vocabulary.",
        id
      ))
    }

    def$id <- id
    # Same version trap as everywhere else in this template family; see
    # dta_template_read_field_exact() in template_core.R.
    exact <- dta_template_read_field_exact(path, "version")
    def$version <- if (!is.na(exact) && nzchar(exact)) {
      exact
    } else {
      dta_template_version_string(def$version, what = id)
    }
    def$label <- as.character(def$label %||% def$id)
    def$description <- as.character(def$description %||% "")
    def$type <- type
    def$extends <- extends
    def$terms <- terms
    def$add_terms <- add_terms
    def$remove_terms <- as.character(def$remove_terms %||% character(0))

    def
  })
}

# ---- Inheritance ------------------------------------------------------------

# Resolve a vocabulary's `extends:` chain into one definition whose `terms:`
# are final.
#
# `resolve_ref` is `function(ref) -> <parent definition list>` or NULL when
# `ref` cannot be resolved -- injected for the same reason
# resolve_template_inheritance() injects its own (this file never has to know
# how a reference is looked up). Cycle detection, the depth limit and the
# recurse-into-the-parent-first ordering are all deliberately identical to
# that function; a reader who knows one knows the other.
#
# Term algebra, in this fixed order:
#   1. parent terms (already fully resolved)
#   2. minus `remove_terms:`
#   3. plus `add_terms:` -- an add whose code already exists REPLACES it in
#      place, keeping the parent's position, rather than appending a second
#      entry with the same code. Relabelling an inherited term is the common
#      case and must not reorder the list.
#   4. a child's own `terms:`, if it has any, REPLACE the result outright --
#      an explicit full restatement, for a child that wants to inherit only
#      the header and not the terms.
resolve_vocabulary_inheritance <- function(def, resolve_ref, .depth = 0L, .seen = character(0)) {
  own_key <- paste0(as.character(def$id %||% ""), "@", as.character(def$version %||% ""))
  if (own_key %in% .seen) {
    cli::cli_abort(
      "Vocabulary inheritance cycle detected: {paste(c(.seen, own_key), collapse = ' -> ')}."
    )
  }

  ref <- as.character(def$extends %||% "")
  if (!nzchar(ref)) {
    return(def)
  }

  if (.depth >= dta_template_max_inheritance_depth) {
    cli::cli_abort(
      "Vocabulary inheritance chain exceeds the depth limit of {dta_template_max_inheritance_depth}: {paste(c(.seen, own_key), collapse = ' -> ')}."
    )
  }

  parent <- resolve_ref(ref)
  if (is.null(parent)) {
    cli::cli_abort(
      "Vocabulary {.val {as.character(def$id %||% '<unknown>')}} extends unresolvable reference {.val {ref}}."
    )
  }

  parent <- resolve_vocabulary_inheritance(
    parent, resolve_ref,
    .depth = .depth + 1L, .seen = c(.seen, own_key)
  )

  terms <- parent$terms %||% list()

  remove <- as.character(def$remove_terms %||% character(0))
  if (length(remove) > 0 && length(terms) > 0) {
    codes <- vapply(terms, function(t) t$code, character(1))
    missing <- setdiff(remove, codes)
    if (length(missing) > 0) {
      # A stale remove_terms entry is how a child silently stops removing
      # anything after the parent renames a code -- worth a warning, not an
      # abort, because the resulting vocabulary is still coherent.
      cli::cli_warn(
        "Vocabulary {.val {def$id}}: {.field remove_terms} names {length(missing)} code{?s} not present in {.val {parent$id}} ({.val {missing}})."
      )
    }
    terms <- terms[!(codes %in% remove)]
  }

  add <- def$add_terms %||% list()
  if (length(add) > 0) {
    codes <- if (length(terms) > 0) vapply(terms, function(t) t$code, character(1)) else character(0)
    for (t in add) {
      hit <- match(t$code, codes)
      if (is.na(hit)) {
        terms[[length(terms) + 1L]] <- t
        codes <- c(codes, t$code)
      } else {
        terms[[hit]] <- t
      }
    }
  }

  own <- def$terms %||% list()
  if (length(own) > 0) {
    terms <- own
  }

  # The child's own header wins throughout; `type:` in particular, because a
  # child that narrows a text vocabulary to numeric codes is stating its own
  # contract, not the parent's.
  out <- def
  out$terms <- terms
  out$extends <- ""
  out$add_terms <- list()
  out$remove_terms <- character(0)
  out
}

# ---- Selecting --------------------------------------------------------------

# The terms of a resolved vocabulary, optionally narrowed.
#
# `include` is an allow-list, `exclude` a deny-list; both name term CODES, and
# `include` is applied first. Authored order is preserved -- `include` narrows
# the list, it does not reorder it to match the order the ids were listed in,
# because the vocabulary's own order is the one the author curated.
#
# An `include` naming a code the vocabulary does not have is an ERROR, not a
# silent no-op: it is the exact shape a typo takes ("SCRN" for "SCR"), and the
# consequence -- a column whose permitted values quietly omit a visit -- is a
# validation failure against real data much later, with nothing pointing back
# here. This mirrors apply_party_selections() aborting on an unknown profile
# id for the same reason.
vocabulary_terms <- function(vocab, include = NULL, exclude = NULL) {
  terms <- vocab$terms %||% list()
  if (length(terms) == 0) {
    return(list())
  }

  codes <- vapply(terms, function(t) t$code, character(1))
  vid <- as.character(vocab$id %||% "<unknown>")

  include <- as.character(include %||% character(0))
  if (length(include) > 0) {
    missing <- setdiff(include, codes)
    if (length(missing) > 0) {
      cli::cli_abort(c(
        "Vocabulary {.val {vid}}: {.field include} names {length(missing)} unknown code{?s}.",
        "x" = "Not in this vocabulary: {.val {missing}}.",
        "i" = "Available: {.val {codes}}."
      ))
    }
    keep <- codes %in% include
    terms <- terms[keep]
    codes <- codes[keep]
  }

  exclude <- as.character(exclude %||% character(0))
  if (length(exclude) > 0) {
    missing <- setdiff(exclude, codes)
    if (length(missing) > 0) {
      cli::cli_abort(c(
        "Vocabulary {.val {vid}}: {.field exclude} names {length(missing)} code{?s} not available to exclude.",
        "i" = "Available here: {.val {codes}}."
      ))
    }
    terms <- terms[!(codes %in% exclude)]
  }

  if (length(terms) == 0) {
    cli::cli_abort(
      "Vocabulary {.val {vid}}: {.field include}/{.field exclude} leave no terms at all."
    )
  }

  terms
}

# The permitted-values vector a term list contributes, coerced to the
# vocabulary's declared type.
#
# `field` is "code" (default) or "label". `field = "label"` exists because
# code/decode column PAIRS are the norm in this domain -- this repository's own
# example data carries GFTESTCD ("TRNSCPTN") alongside GFTEST
# ("Transcription") -- and one vocabulary describing both halves is the whole
# point of having a vocabulary at all. A term with no label falls back to its
# code so the two columns always have the same arity.
#
# A label vector is ALWAYS character: the type coercion applies to codes only.
# A numeric vocabulary's decodes are display text, not numbers.
vocabulary_values <- function(vocab, terms = NULL, field = c("code", "label")) {
  field <- match.arg(field)
  terms <- terms %||% vocab$terms %||% list()
  if (length(terms) == 0) {
    return(NULL)
  }

  if (identical(field, "label")) {
    return(vapply(terms, function(t) as.character(t$label %||% t$code), character(1)))
  }

  codes <- vapply(terms, function(t) t$code, character(1))
  .coerce_vocabulary_codes(codes, as.character(vocab$type %||% "text"), as.character(vocab$id %||% "<unknown>"))
}

# ---- The column binding -----------------------------------------------------

# Normalise a column's `values_from:` into a fixed shape.
#
# Accepts either the shorthand
#
#   values_from: visit@1.0
#
# or the full mapping
#
#   values_from:
#     vocabulary: visit@1.0
#     field: code
#     include: [SCR, C1D1]
#     exclude: [EOT]
#
# Returns list(vocabulary=, field=, include=, exclude=), or NULL when the
# column has no binding. Aborts on a malformed one -- naming the column,
# because by the time this runs the only thing identifying the offending YAML
# is the column id.
normalise_values_from <- function(x, column_id = "<column>") {
  if (is.null(x)) {
    return(NULL)
  }

  if (!is.list(x)) {
    ref <- as.character(x)
    if (length(ref) != 1 || is.na(ref) || !nzchar(ref)) {
      cli::cli_abort("Column {.val {column_id}}: {.field values_from} must name a vocabulary.")
    }
    return(list(vocabulary = ref, field = "code", include = character(0), exclude = character(0)))
  }

  ref <- as.character(x$vocabulary %||% "")
  if (length(ref) != 1 || !nzchar(ref)) {
    cli::cli_abort(
      "Column {.val {column_id}}: {.field values_from} must set {.field vocabulary} to an {.code id[@version]} reference."
    )
  }

  field <- as.character(x$field %||% "code")
  if (!(field %in% c("code", "label"))) {
    cli::cli_abort(
      "Column {.val {column_id}}: {.field values_from.field} must be {.val code} or {.val label}, got {.val {field}}."
    )
  }

  list(
    vocabulary = ref,
    field = field,
    include = as.character(x$include %||% character(0)),
    exclude = as.character(x$exclude %||% character(0))
  )
}

# ---- Vocabulary slots -------------------------------------------------------
#
# A `values_from:` binding is the template AUTHOR's decision. A vocabulary slot
# is the DOCUMENT author's: a creation template offers one, and the person
# creating the document picks which terms apply to their study.
#
#   vocabulary_slots:
#     - id: visit_choice
#       label: Visits collected in this study
#       target: datasets.gf_data.columns.VISIT.values
#       vocabulary: visit@1.0
#       mode: closed          # closed | open
#       include: [SCR, C1D1, EOT]   # the menu offered, if not the whole thing
#       default: [SCR, C1D1]        # pre-ticked
#       min: 1                      # optional cardinality floor
#
# This is deliberately the party_slots: shape, one concept over: a slot, a
# target, an optional allow-list narrowing what the dropdown offers, and a
# selection re-validated on the way in.
#
# WHY SLOTS EXIST ON THE CREATION TEMPLATE AND NOT ON A DATASET TEMPLATE'S
# COLUMN: show_template_options_modal() renders the CREATION template's own
# options/party_slots. A dataset template's `options:` are supplied by the
# creation template's `datasets[].options:` map and are never prompted for. A
# picker hung off a dataset-template column would therefore have nowhere to be
# drawn without first teaching the modal to prompt dataset-template options --
# a bigger, separable change. A creation-template slot reaches the same column
# through machinery that already exists.

# The one target shape a vocabulary slot may write:
# "datasets.<dataset name>.columns.<column id>.values".
#
# Returns list(dataset=, column=) or NULL. Checked up front for the same
# reason party_slot_target_valid() checks its own: a malformed target would
# otherwise fail somewhere far from the YAML that caused it.
#
# Splitting on "." caps the parts at five, so a dataset or column whose name
# CONTAINS a dot is not addressable. That is a real restriction and it is the
# right trade: the alternative -- a quoting or escaping grammar in a YAML
# target string -- buys an unusual naming choice at the cost of a parser
# nobody can read at a glance.
vocabulary_slot_target_parts <- function(target) {
  if (!is.character(target) || length(target) != 1 || is.na(target)) {
    return(NULL)
  }
  parts <- strsplit(target, ".", fixed = TRUE)[[1]]
  if (length(parts) != 5) {
    return(NULL)
  }
  if (!identical(parts[[1]], "datasets") ||
    !identical(parts[[3]], "columns") ||
    !identical(parts[[5]], "values")) {
    return(NULL)
  }
  if (!nzchar(parts[[2]]) || !nzchar(parts[[4]])) {
    return(NULL)
  }
  list(dataset = parts[[2]], column = parts[[4]])
}

# Normalise a template's `vocabulary_slots:`, filling in defaults and rejecting
# a malformed slot immediately.
#
# Id-less slots are dropped in ONE counted warning, matching
# normalise_party_slots() and dta_template_drop_unidentified() -- see the
# comment there for why a per-item warning would make the behaviour untestable.
normalise_vocabulary_slots <- function(slots) {
  slots <- slots %||% list()

  ids <- vapply(slots, function(slot) as.character(slot$id %||% ""), character(1))
  missing_id <- !nzchar(ids)
  if (any(missing_id)) {
    n <- sum(missing_id)
    cli::cli_warn("Dropping {n} vocabulary slot{?s} with a missing or empty {.field id}.")
  }
  slots <- slots[!missing_id]
  ids <- ids[!missing_id]

  out <- list()
  for (i in seq_along(slots)) {
    slot <- slots[[i]]
    id <- ids[[i]]

    target <- as.character(slot$target %||% "")
    parts <- vocabulary_slot_target_parts(target)
    if (is.null(parts)) {
      cli::cli_abort(c(
        "Vocabulary slot {.val {id}} has an invalid {.field target} {.val {target}}.",
        "i" = "Expected {.code datasets.<dataset>.columns.<column>.values}."
      ))
    }

    vocab_ref <- as.character(slot$vocabulary %||% "")
    if (!nzchar(vocab_ref)) {
      cli::cli_abort("Vocabulary slot {.val {id}} must name a {.field vocabulary}.")
    }

    mode <- as.character(slot$mode %||% "closed")
    if (!(mode %in% c("closed", "open"))) {
      cli::cli_abort(
        "Vocabulary slot {.val {id}} has an invalid {.field mode} {.val {mode}}; must be 'closed' or 'open'."
      )
    }

    field <- as.character(slot$field %||% "code")
    if (!(field %in% c("code", "label"))) {
      cli::cli_abort(
        "Vocabulary slot {.val {id}} has an invalid {.field field} {.val {field}}; must be 'code' or 'label'."
      )
    }

    min_n <- suppressWarnings(as.integer(slot$min %||% 0L))
    if (is.na(min_n) || min_n < 0L) {
      cli::cli_abort("Vocabulary slot {.val {id}} has an invalid {.field min}; must be a non-negative whole number.")
    }

    out[[length(out) + 1L]] <- list(
      id = id,
      label = as.character(slot$label %||% id),
      description = as.character(slot$description %||% ""),
      target = target,
      dataset = parts$dataset,
      column = parts$column,
      vocabulary = vocab_ref,
      field = field,
      mode = mode,
      include = as.character(slot$include %||% character(0)),
      exclude = as.character(slot$exclude %||% character(0)),
      default = as.character(slot$default %||% character(0)),
      min = min_n
    )
  }

  out
}

# The terms one slot offers, after its own include/exclude narrowing. This is
# what the picker lists.
vocabulary_slot_choices <- function(slot, resolve_vocab) {
  vocab <- resolve_vocab(slot$vocabulary)
  if (is.null(vocab)) {
    cli::cli_abort(
      "Vocabulary slot {.val {slot$id}}: cannot resolve vocabulary {.val {slot$vocabulary}}."
    )
  }
  list(
    vocab = vocab,
    terms = vocabulary_terms(vocab, include = slot$include, exclude = slot$exclude)
  )
}

# The permitted-values vector one slot's selection produces, or NULL when the
# slot was left alone.
#
# An absent or empty selection falls back to the slot's `default:`; when that
# is empty too the result is NULL, meaning "leave the column exactly as the
# dataset template left it". Same reasoning as apply_party_selections()
# skipping an unselected slot: a slot the author did not engage with must not
# silently rewrite something.
vocabulary_slot_values <- function(slot, selection, resolve_vocab) {
  selection <- as.character(selection %||% character(0))
  selection <- selection[!is.na(selection) & nzchar(selection)]
  if (length(selection) == 0) {
    selection <- slot$default
  }

  if (length(selection) < slot$min) {
    cli::cli_abort(
      "Vocabulary slot {.val {slot$id}} requires at least {slot$min} selection{?s}; got {length(selection)}."
    )
  }
  if (length(selection) == 0) {
    return(NULL)
  }

  choices <- vocabulary_slot_choices(slot, resolve_vocab)
  codes <- vapply(choices$terms, function(t) t$code, character(1))

  unknown <- setdiff(selection, codes)
  if (length(unknown) > 0 && identical(slot$mode, "closed")) {
    # A stale selection -- the vocabulary was republished, or the slot's
    # allow-list narrowed, between page load and submit. Silently dropping it
    # would leave the author believing their choice was applied.
    cli::cli_abort(c(
      "Vocabulary slot {.val {slot$id}}: {length(unknown)} selected code{?s} not offered here.",
      "x" = "Not available: {.val {unknown}}.",
      "i" = "This slot is {.val closed}; offered codes are {.val {codes}}."
    ))
  }

  known <- choices$terms[codes %in% selection]
  values <- vocabulary_values(choices$vocab, known, field = slot$field)

  if (length(unknown) > 0) {
    # mode: open -- the author's own terms, appended after the vocabulary's in
    # the order they were entered. Coerced through the same path as a code, so
    # a numeric vocabulary does not end up with a character value spliced in.
    extra <- if (identical(slot$field, "label")) {
      as.character(unknown)
    } else {
      # The VOCABULARY's id, not the slot's: the type being enforced is the
      # vocabulary's, and naming the slot here would report a coercion failure
      # against something that declares no type at all.
      .coerce_vocabulary_codes(
        unknown,
        as.character(choices$vocab$type %||% "text"),
        as.character(choices$vocab$id %||% slot$id)
      )
    }
    values <- c(values, extra)
  }

  values
}

# Turn slot definitions + the author's selections into a flat list of
# list(dataset=, column=, values=) overrides, ready for
# apply_vocabulary_slot_overrides().
resolve_vocabulary_slot_overrides <- function(slots, selections, resolve_vocab) {
  selections <- selections %||% list()
  out <- list()
  for (slot in slots) {
    values <- vocabulary_slot_values(slot, selections[[slot$id]], resolve_vocab)
    if (is.null(values)) {
      next
    }
    out[[length(out) + 1L]] <- list(
      dataset = slot$dataset, column = slot$column, values = values, slot = slot$id
    )
  }
  out
}

# Write every override addressed to THIS dataset into a plain dataset list.
#
# Applied to the plain list, after build_dataset_from_template() has renamed it
# and before dta_dataset_from_list() turns it into S7 objects -- so a slot
# targets the dataset by the name it ends up with (`as:`), which is the name
# the template author sees, and the write costs no S7 object surgery.
#
# An override naming a column the dataset does not have is an ERROR: a slot
# whose target has drifted from the dataset template it points at must not
# quietly do nothing.
apply_vocabulary_slot_overrides <- function(ds, overrides) {
  if (length(overrides) == 0) {
    return(ds)
  }
  ds_name <- as.character(ds$name %||% "")
  mine <- Filter(function(o) identical(o$dataset, ds_name), overrides)
  if (length(mine) == 0) {
    return(ds)
  }

  columns <- ds$columns %||% list()
  ids <- if (length(columns) > 0) {
    vapply(columns, function(col) as.character(col$id %||% ""), character(1))
  } else {
    character(0)
  }

  for (o in mine) {
    hit <- match(o$column, ids)
    if (is.na(hit)) {
      cli::cli_abort(c(
        "Vocabulary slot {.val {o$slot}} targets column {.val {o$column}} of dataset {.val {ds_name}}, which has no such column.",
        "i" = "Columns here: {.val {ids}}."
      ))
    }
    col <- columns[[hit]]
    if (!is.null(col$pattern)) {
      cli::cli_abort(
        "Vocabulary slot {.val {o$slot}} targets column {.val {o$column}}, which sets a {.field pattern}; a column takes permitted values or a pattern, never both."
      )
    }
    col$values <- o$values
    # A slot's choice supersedes whatever binding the dataset template had.
    col$values_from <- NULL
    columns[[hit]] <- col
  }

  ds$columns <- columns
  ds
}

# ---- Expansion --------------------------------------------------------------

# Look one `id[@version]` up in a real template index and read it, WITHOUT
# resolving its `extends:` chain.
#
# This raw, non-recursive shape is exactly what resolve_vocabulary_inheritance()
# requires of its `resolve_ref`, and the requirement is load-bearing rather than
# stylistic. That function detects a cycle by threading `.seen`/`.depth` through
# its OWN recursion; if the lookup it is handed resolves inheritance itself,
# every hop re-enters at `.depth = 0` with an empty `.seen`, the accumulated
# lineage is thrown away, and a mutual `extends:` recurses until R's protection
# stack overflows instead of raising "Vocabulary inheritance cycle detected".
# Keep this function a plain lookup. resolve_template_inheritance()'s own
# resolver (template_create.R) is the same shape for the same reason.
#
# Memoised: a dataset template that binds twenty columns to the same vocabulary
# (a study with twenty Y/N flags is not unusual) would otherwise re-read the
# same file twenty times, and a `git:` source makes that measurably worse.
vocabulary_lookup <- function(index) {
  cache <- new.env(parent = emptyenv())

  function(ref) {
    ref <- as.character(ref)
    hit <- cache[[ref]]
    if (!is.null(hit)) {
      # A miss is cached as the sentinel string rather than NULL, because an
      # environment cannot hold NULL: assigning NULL removes the binding, so a
      # NULL cache entry is indistinguishable from never having looked.
      return(if (identical(hit, "__none__")) NULL else hit)
    }

    row <- resolve_template_ref(index, ref, kind = "dta_vocabulary")
    if (is.null(row) || nrow(row) == 0) {
      cache[[ref]] <- "__none__"
      return(NULL)
    }
    read <- read_vocabulary(row$path[[1]])
    if (!isTRUE(read$ok)) {
      cli::cli_abort("Vocabulary {.val {ref}} could not be read: {read$error}")
    }

    cache[[ref]] <- read$value
    read$value
  }
}

# `id[@version]` -> a vocabulary whose `extends:` chain is fully resolved, or
# NULL when the reference is not in the index.
#
# One entry into resolve_vocabulary_inheritance() per reference, handed the raw
# lookup above -- so cycle detection and the depth limit stay intact. Resolved
# results are memoised separately from raw ones: the two are different values
# for the same key, and a caller wanting terms must never be served the
# unresolved parent by accident.
vocabulary_resolver <- function(index) {
  lookup <- vocabulary_lookup(index)
  cache <- new.env(parent = emptyenv())

  function(ref) {
    ref <- as.character(ref)
    hit <- cache[[ref]]
    if (!is.null(hit)) {
      return(if (identical(hit, "__none__")) NULL else hit)
    }

    raw <- lookup(ref)
    if (is.null(raw)) {
      cache[[ref]] <- "__none__"
      return(NULL)
    }

    out <- resolve_vocabulary_inheritance(raw, lookup)
    cache[[ref]] <- out
    out
  }
}

# Does any column of a plain dataset list carry a vocabulary binding?
#
# Used to tell "this build needs a resolver and did not get one" (a hard
# error, naming the cause) apart from "this build simply has no bindings"
# (perfectly normal, and the case for every template written before this
# feature existed).
dataset_has_vocabulary_binding <- function(ds) {
  columns <- ds$columns %||% list()
  if (length(columns) == 0) {
    return(FALSE)
  }
  any(vapply(
    columns,
    function(col) is.list(col) && !is.null(col$values_from),
    logical(1)
  ))
}

# Replace every column's `values_from:` binding with a plain `values:` vector.
#
# THIS IS THE POINT OF THE WHOLE FEATURE. `values_from` is authoring syntax; it
# must never reach DTAColumnSpec (which would reject it as an unused argument
# in specs_from_list()'s do.call()) and must never survive into a produced
# document (a DTA YAML handed to a supplier has to be readable without access
# to the vocabulary library that shaped it).
#
# `ds` is a plain dataset list, post-patch, exactly as
# build_dataset_from_template() holds it. Returns the same list with every
# binding expanded.
#
# PRECEDENCE: `values_from` wins over a `values:` that is already present.
# Deliberately silent at build time, because the legitimate case -- a patch's
# `modify_columns` re-binding a column the base template had given a hardcoded
# list -- is indistinguishable here from a confused one, and aborting would
# break the legitimate case. Authoring BOTH in the same literal column is the
# genuinely ambiguous case, and validate_template() flags exactly that, where
# the raw per-file YAML makes the two distinguishable.
expand_column_vocabularies <- function(ds, resolve_vocab, dataset_name = NULL) {
  columns <- ds$columns %||% list()
  if (length(columns) == 0) {
    return(ds)
  }

  where <- if (is.null(dataset_name)) "" else paste0(" in dataset ", dataset_name)

  for (i in seq_along(columns)) {
    col <- columns[[i]]
    if (!is.list(col) || is.null(col$values_from)) {
      next
    }

    col_id <- as.character(col$id %||% sprintf("<column %d>", i))
    binding <- normalise_values_from(col$values_from, col_id)

    # Caught here rather than left to the DTAColumnSpec validator, which would
    # abort with a message naming neither the vocabulary nor the template the
    # binding came from.
    if (!is.null(col$pattern)) {
      cli::cli_abort(c(
        "Column {.val {col_id}}{where}: {.field values_from} cannot be combined with {.field pattern}.",
        "i" = "A column is constrained by a permitted-value list or by a pattern, never both."
      ))
    }

    vocab <- resolve_vocab(binding$vocabulary)
    if (is.null(vocab)) {
      cli::cli_abort(
        "Column {.val {col_id}}{where}: cannot resolve vocabulary {.val {binding$vocabulary}}."
      )
    }

    terms <- vocabulary_terms(vocab, include = binding$include, exclude = binding$exclude)
    col$values <- vocabulary_values(vocab, terms, field = binding$field)
    col$values_from <- NULL
    columns[[i]] <- col
  }

  ds$columns <- columns
  ds
}
