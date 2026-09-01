# -----------------------------------------------------------------------------
# Party profiles: reusable supplier/receiver metadata blocks a creation
# template can offer as a pick-one dropdown, instead of every template author
# retyping the same affiliation/contacts block (or worse, drifting copies of
# it) into `base.metadata.supplier` / `base.metadata.receiver`.
#
# File kind (kind: dta_party_profile), already recognised by the template
# index (template_index.R: dta_template_all_kinds(), "*.dta-party.ya?ml$"),
# but not yet read anywhere -- that is what this file adds:
#
#   kind: dta_party_profile
#   id: supplier_acme
#   version: "1.0"
#   role: supplier          # supplier | receiver | any
#   label: ACME Labs
#   description: Standard ACME sequencing supplier block
#   affiliation:
#     name: ACME Labs
#     address: 1 Example Way
#     country: DE
#   contacts:
#     - name: Jane Doe
#       role: Data Manager
#       email: jane.doe@example.com
#       signature: true
#
# A template offers a "party slot" -- one dropdown bound to a metadata target
# (`metadata.supplier` or `metadata.receiver`) -- via its own `party_slots:`
# section:
#
#   party_slots:
#     - id: supplier_choice
#       target: metadata.supplier
#       label: Supplier
#       profiles: [supplier_acme, supplier_other]   # optional allow-list
#
# `party_slots:` merging across `extends:` is already handled generically by
# dta_template_merge_options() (template_inherit.R) -- it treats party_slots
# exactly like options, keyed by id. What is missing, and what this file
# supplies, is turning a raw parsed slot into something with defaults filled
# in (normalise_party_slots()), matching profiles against it
# (party_profiles_for_slot()), and writing a chosen profile into a DTA
# (apply_party_profile() / apply_party_selections()).
#
# `%||%` and `dta_try()` are already bound in the app helper environment (see
# utils_dta.R) -- not redefined here, same convention as dataset_template.R
# and template_inherit.R.
# -----------------------------------------------------------------------------

# ---- Reading ----------------------------------------------------------------

# Drop a contact that is not a named list (a bare string, an unnamed sequence
# entry, an empty list), warning once with a count rather than once per
# malformed entry. A contact without field names cannot be rendered by any of
# the contact-formatting code downstream (.format_contact() in
# DTAMetaData-class.R keys everything off $name/$role/$email/...), so letting
# it through would fail far from this file, with no indication which profile
# was the source.
.normalise_party_contacts <- function(contacts, profile_id) {
  if (length(contacts) == 0) {
    return(list())
  }
  ok <- vapply(
    contacts,
    function(x) is.list(x) && !is.null(names(x)) && all(nzchar(names(x))),
    logical(1)
  )
  if (any(!ok)) {
    n <- sum(!ok)
    cli::cli_warn(
      "Party profile {.val {profile_id}}: dropping {n} malformed contact{?s} (not a named list)."
    )
  }
  contacts[ok]
}

# Read and minimally validate a party-profile YAML.
#
# Kept close to read_dataset_template()'s shape (same dta_try() wrapping,
# same "check presence on the raw value, then normalise" tail): `version` in
# particular has to be checked for presence BEFORE
# dta_template_version_string() runs, because that helper turns a missing
# version into NA_character_ rather than erroring, and nzchar(NA_character_)
# is FALSE either way but only by accident -- the explicit check documents
# the requirement instead of relying on that coincidence.
read_party_profile <- function(path) {
  dta_try({
    if (is.null(path) || !nzchar(path) || !file.exists(path)) {
      stop("Party profile file not found.")
    }
    def <- yaml::read_yaml(path)
    if (!is.list(def)) stop("Party profile YAML must be a mapping/object.")

    kind <- as.character(def$kind %||% "")
    if (!identical(kind, "dta_party_profile")) {
      stop("Party profile 'kind' must be 'dta_party_profile'.")
    }

    id <- as.character(def$id %||% "")
    if (!nzchar(id)) {
      stop("Party profile must define a non-empty 'id'.")
    }

    if (is.null(def$version) || length(def$version) == 0 || !nzchar(as.character(def$version))) {
      stop("Party profile must define a non-empty 'version'.")
    }

    role <- as.character(def$role %||% "any")
    if (!(role %in% c("supplier", "receiver", "any"))) {
      stop(sprintf(
        "Party profile '%s' has invalid role '%s'; must be one of supplier, receiver, any.",
        id, role
      ))
    }

    def$id <- id
    # Same version trap as everywhere else in this template family: an
    # unquoted `version: 1.10` is already the double 1.1 once plainly parsed.
    # Re-read the header field from the file text, which loses nothing, and
    # fall back to the lossy coercion only if the file cannot be re-read.
    exact <- dta_template_read_field_exact(path, "version")
    def$version <- if (!is.na(exact) && nzchar(exact)) {
      exact
    } else {
      dta_template_version_string(def$version, what = id)
    }
    def$role <- role
    def$label <- as.character(def$label %||% def$id)
    def$description <- as.character(def$description %||% "")
    def$affiliation <- def$affiliation %||% list()
    def$contacts <- .normalise_party_contacts(def$contacts %||% list(), def$id)

    def
  })
}

# ---- Party slots --------------------------------------------------------

# Is `target` one of the two metadata paths a party slot may write to?
#
# A party slot is intentionally restricted to exactly these two paths. Any
# other metadata.* path would have `apply_party_profile()` replace a SCALAR
# property (e.g. metadata.title, a character-or-null) with a
# list(affiliation=, contacts=) block -- the S7 validator would reject that,
# but only once the DTA is next validated, far away from the template YAML
# that actually caused it. Checking the target up front turns that into an
# immediate, specific error at the point the slot is defined.
party_slot_target_valid <- function(target) {
  is.character(target) && length(target) == 1 && !is.na(target) &&
    target %in% c("metadata.supplier", "metadata.receiver")
}

# Normalise a template's `party_slots:` list, filling in defaults and
# rejecting an invalid target immediately (see party_slot_target_valid()).
#
# `role` is derived from `target` (metadata.supplier -> "supplier") unless the
# slot states its own -- a slot almost always wants exactly the profiles meant
# for the field it writes, so deriving it saves every template author from
# repeating the same role/target pair.
normalise_party_slots <- function(slots) {
  slots <- slots %||% list()

  # Drop every id-less slot in ONE warning naming the count, not one warning
  # per bad entry: template_inherit.R's dta_template_drop_unidentified() does
  # the same for options and (already) party_slots for the identical reason
  # -- a per-item warning here would let every warning past the first leak
  # through expect_warning(), which only captures the FIRST matching warning
  # and reports the rest as unexpected extras.
  ids <- vapply(slots, function(slot) as.character(slot$id %||% ""), character(1))
  missing_id <- !nzchar(ids)
  if (any(missing_id)) {
    n <- sum(missing_id)
    cli::cli_warn("Dropping {n} party slot{?s} with a missing or empty {.field id}.")
  }
  slots <- slots[!missing_id]
  ids <- ids[!missing_id]

  out <- list()
  for (i in seq_along(slots)) {
    slot <- slots[[i]]
    id <- ids[[i]]

    target <- as.character(slot$target %||% "")
    if (!party_slot_target_valid(target)) {
      cli::cli_abort(
        "Party slot {.val {id}} has an invalid {.field target} {.val {target}}; must be 'metadata.supplier' or 'metadata.receiver'."
      )
    }

    role <- as.character(slot$role %||% "")
    if (!nzchar(role)) {
      role <- switch(target,
        "metadata.supplier" = "supplier",
        "metadata.receiver" = "receiver"
      )
    }

    out[[length(out) + 1L]] <- list(
      id = id,
      target = target,
      label = as.character(slot$label %||% id),
      role = role,
      # No allow-list means "any profile whose role matches" -- an empty
      # character(0) rather than NULL so party_profiles_for_slot() can test
      # length() without a %||% at every call site.
      profiles = as.character(slot$profiles %||% character(0))
    )
  }

  out
}

# Which normalised profiles are eligible for one normalised slot?
#
# Eligibility is role match (a profile authored for the other role, or for a
# DIFFERENT slot's allow-list, never even gets offered) AND, when the slot
# names an explicit allow-list, membership in it. Ordered by label with
# method = "radix": the dev machine collates under German locale, CI under C
# collation, and a locale-dependent order in a user-facing dropdown is a
# needless difference between the two that this sidesteps entirely (see
# dta_creation_template_dirs()-adjacent sort() calls in template_core.R for
# the same reasoning, and the project's "locale collation diverges from CI"
# lesson).
party_profiles_for_slot <- function(profiles, slot) {
  eligible <- Filter(function(p) {
    role_ok <- identical(p$role, "any") || identical(p$role, slot$role)
    if (!role_ok) {
      return(FALSE)
    }
    allow <- slot$profiles %||% character(0)
    if (length(allow) > 0 && !(p$id %in% allow)) {
      return(FALSE)
    }
    TRUE
  }, profiles)

  if (length(eligible) == 0) {
    return(list())
  }

  labels <- vapply(eligible, function(p) as.character(p$label %||% p$id), character(1))
  eligible[order(labels, method = "radix")]
}

# ---- Applying -----------------------------------------------------------

# The metadata block one party profile contributes: just its affiliation and
# contacts, omitting either when the profile left it empty so an empty
# `list()` never overwrites a value the template's own base metadata already
# set for the other field.
party_profile_block <- function(profile) {
  block <- list()
  if (length(profile$affiliation %||% list()) > 0) {
    block$affiliation <- profile$affiliation
  }
  if (length(profile$contacts %||% list()) > 0) {
    block$contacts <- profile$contacts
  }
  block
}

# Write one profile's block to a validated party-slot target.
#
# WHY WHOLE-BLOCK REPLACEMENT, NOT A MERGE: `target` has no further nested
# keys ("metadata.supplier", never "metadata.supplier.contacts"), so
# apply_template_metadata_path() takes its scalar-assignment branch and
# replaces `metadata$supplier` outright. That is deliberate, not an
# oversight: merging the old block's contacts with the new profile's would
# leave one profile's signatory silently coexisting with a different
# profile's affiliation. A half-merged party block like that is worse than
# either input on its own, and -- because nothing about it looks wrong on a
# quick read -- is far harder to catch before it ships in a generated
# document than an obviously-wrong, fully-replaced block would be.
apply_party_profile <- function(dta, target, profile) {
  if (!party_slot_target_valid(target)) {
    cli::cli_abort(
      "Party profile target must be 'metadata.supplier' or 'metadata.receiver', got {.val {target}}."
    )
  }
  apply_template_metadata_path(dta, target, party_profile_block(profile))
}

# Apply a user's slot -> profile-id selections to a DTA.
#
# An ABSENT selection for a slot is not an error: it means the template's own
# default for that field (whatever `base.metadata` already put there) stands
# untouched. An EXPLICITLY EMPTY one is not the same answer -- it says the slot
# is deliberately unfilled, and empties the target instead of leaving the
# template's value showing. An unknown profile id, in contrast, names the slot
# and the id in the abort -- a stale selection (the profile was renamed or
# removed from the picker's allow-list between page load and submit) must be
# caught here, not silently ignored, because silently ignoring it would leave
# the user believing their choice was applied when it was not.
apply_party_selections <- function(dta, slots, selections, profiles) {
  selections <- selections %||% list()
  profile_ids <- vapply(profiles, function(p) as.character(p$id %||% ""), character(1))

  for (slot in slots) {
    # Absent and explicitly-empty are DIFFERENT instructions, and collapsing
    # them is what made "deliberately no party here" impossible to say. A slot
    # missing from `selections` is silence -- the author never engaged with it,
    # so whatever base.metadata put in the target stands. A slot present but
    # empty is an engagement that means none, and empties the target instead.
    engaged <- slot$id %in% names(selections)
    sel <- as.character(selections[[slot$id]] %||% "")
    sel <- sel[!is.na(sel) & nzchar(sel)]
    if (length(sel) == 0) {
      if (!engaged) {
        next
      }
      dta <- apply_template_metadata_path(dta, slot$target, list())
      next
    }
    sel <- sel[[1]]

    hit <- match(sel, profile_ids)
    if (is.na(hit)) {
      cli::cli_abort(
        "Party slot {.val {slot$id}}: unknown profile id {.val {sel}}."
      )
    }

    # Re-check ELIGIBILITY, not just existence. The UI builds its dropdown from
    # party_profiles_for_slot(), so this cannot fire from the app -- but a
    # direct caller, a restored session, or a saved selection from a template
    # whose slots have since changed could otherwise write a receiver profile
    # into the supplier field. That produces a document naming the wrong
    # organisation as the data supplier, which reads as plausible and is
    # exactly the failure this whole feature exists to prevent. Existence is
    # not the same check as eligibility, and only one of them is safe to skip.
    eligible <- party_profiles_for_slot(profiles, slot)
    eligible_ids <- vapply(eligible, function(p) as.character(p$id %||% ""), character(1))
    if (!(sel %in% eligible_ids)) {
      cli::cli_abort(c(
        "Party slot {.val {slot$id}} cannot take profile {.val {sel}}.",
        "i" = "Slot role is {.val {slot$role %||% NA_character_}}; that profile's role is {.val {as.character(profiles[[hit]]$role %||% NA_character_)}}.",
        "i" = "Eligible here: {.val {eligible_ids}}."
      ))
    }

    dta <- apply_party_profile(dta, slot$target, profiles[[hit]])
  }

  dta
}
