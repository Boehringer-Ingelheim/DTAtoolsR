# PLAN — Controlled vocabularies in the template library

Extends the template system built on `ai/planning-agent-template-privacy-d05774`
(worktree `wonderful-cannon-3b0931`, commit `88eb4cc`, `Version: 0.25.0`).

Prior plan: `thoughts/shared/plans/PLAN-template-system-private-sources.md`.

---

## 1. The central finding, which shapes everything below

`DTAColumnSpec@values` **is already the controlled-vocabulary mechanism.** It is
the permitted-value list, it is serialised to and from YAML, and it is enforced:
`as_json_schema()` turns it into `enum` (or `const` for a single value), and
`R/columnSpecChecks.R:206-217` fails every row that is not `%in%` it.

So this feature is **not a new validation axis**. It is an *authoring* feature:
a reusable, versioned, indexed library of term sets that gets **expanded into a
plain `values:` list** at document-creation time.

That framing is the whole design, and it buys three things:

- **Zero changes to the S7 classes.** `values` is consumed in 13 places —
  constructor normalisation, the validator's `values`/`pattern`/`examples`
  mutual exclusion, `print`, `as.list`, `as_json_schema`, the numeric-on-text
  warning, the enum/const checks, the Word/PDF table, `exportFunctions.R`
  (twice), and the markdown report. A new `@vocabulary` property would have to
  be threaded through all of them, including the hand-written `as.list()`.
  Expansion touches none of it.
- **Zero changes to the validation engine**, in-memory or streaming.
  `dta_compile_columnspec_schemas()` keeps seeing the same compiled `enum`.
- **Produced documents stay self-contained.** A DTA YAML handed to a supplier
  does not reference a vocabulary they cannot resolve. `values_from:` never
  survives into the output.

**Corollary, stated plainly:** a vocabulary version bump does *not* propagate
into documents already created from it. See §7 for why, and what that costs.

## 2. Requirements → mechanism

The ask has four parts. Each maps onto an existing precedent in the codebase,
which is how this stays small.

| Ask | Mechanism | Precedent it copies |
| --- | --- | --- |
| "introduce controlled vocabulary" | a 4th artifact kind, `dta_vocabulary`, `*.dta-vocabulary.yaml` | `dta_party_profile` / `*.dta-party.yaml` |
| "reuseable for different datasets" | indexed by `id@version`, referenced by ref | `resolve_template_ref(index, ref, kind = ...)` |
| "extendable" | `extends:` + `add_terms:` / `remove_terms:` | the `remove: true` idiom in `template_inherit.R` |
| "restricted for specific datasets" | `include:` / `exclude:` on the binding | `party_slots.profiles:` allow-list |
| "popup where user can select the options" | `vocabulary_slots:` + a create-time picker | `party_slots:` + `show_template_options_modal()` |
| "or could use their own vocabulary" | `mode: open` — picker seeds, user may add | new, see §4 |

The index is already fully generic over `kind` (`build_template_index()` loops
`dta_template_all_kinds()`), so registering a 4th kind is two lines plus a
reader.

## 3. The vocabulary artifact

`visit.dta-vocabulary.yaml`:

```yaml
kind: dta_vocabulary
id: visit
version: "1.0"
label: Visit identifiers
description: Standard visit codes for the biomarker GF template family.
type: text            # text | number | integer — coercion + mismatch check
terms:
  - code: SCR
    label: Screening
    description: Pre-randomisation screening visit.
  - code: C1D1
    label: Cycle 1 Day 1
  - code: EOT
    label: End of treatment
```

- A bare string in `terms:` is shorthand for `{ code: <string> }`.
- `code` is what lands in `values:`. `label`/`description` are authoring
  metadata: shown in the picker, never emitted — **except** via `field: label`
  (below), which exists because this repo's own example data already carries
  code/decode column pairs (`GFTESTCD: TRNSCPTN` / `GFTEST: Transcription`).
  One vocabulary can drive both columns.

**Extension** is explicit, not a generic merge, because
`dta_template_merge_value()` replaces sequences **wholesale** — inheriting a
`terms:` list and adding one entry is not expressible that way:

```yaml
kind: dta_vocabulary
id: visit_oncology
version: "1.0"
extends: visit@1.0
add_terms:
  - { code: C2D1, label: Cycle 2 Day 1 }
remove_terms: [EOT]
```

## 4. The two binding sites

### 4a. `values_from:` on a column — the declarative restriction

In a dataset template's `columns:`, or in a creation template's
`datasets[].patch.add_columns` / `modify_columns`:

```yaml
- id: VISIT
  label: Visit
  type: SAS Char
  format: SAS $20.
  nullable: No
  values_from:
    vocabulary: visit@1.0    # id[@version]; @latest allowed
    field: code              # code (default) | label
    include: [SCR, C1D1]     # optional allow-list — "restricted for this dataset"
    exclude: [EOT]           # optional deny-list
```

Shorthand `values_from: visit@1.0` means all terms, `code`, no restriction.

### 4b. `vocabulary_slots:` on a creation template — the popup

Strictly parallel to `party_slots:`, and for the same reason: the *user*, not
the template author, chooses.

```yaml
vocabulary_slots:
  - id: visit_choice
    label: Visits collected in this study
    target: datasets.gf_data_specs_pattern.columns.VISIT.values
    vocabulary: visit@1.0
    mode: closed             # closed | open
    include: [SCR, C1D1, EOT]   # the menu the user picks from
    default: [SCR, C1D1]        # pre-ticked
    min: 1                      # optional cardinality guard
```

`mode:` is the "or could use their own vocabulary" switch, and it means
**authoring freedom, not looser validation**:

- `closed` — the picker offers exactly the vocabulary's terms.
- `open` — the picker additionally offers a free-text box; the user's own
  entries are appended. The produced `values:` is *still* a closed enum
  containing the union.

There is a third semantic someone will eventually ask for — "advisory, warn
but do not fail". **It is deliberately out of scope.** The engine has no
warn-severity enum, and the only way to express "not enforced" today is to emit
`examples:` instead of `values:`, which the `DTAColumnSpec` validator makes
mutually exclusive and which turns validation for that column **completely
off**. Silently disabling a check is worse than the problem it solves. If it is
wanted later it is a validation-engine change, planned separately.

> **Why slots exist as well as `values_from:`, and a real limit.**
> `show_template_options_modal()` renders the **creation** template's own
> `options:`/`party_slots:`. A dataset template's `options:` are supplied by the
> creation template's `datasets[].options:` map — they are never prompted to
> the user. So a picker attached to a *dataset-template column* cannot be shown
> today without also teaching the modal to prompt for dataset-template options.
> Rather than build that, phase 3 puts the picker where the machinery already
> reaches: a creation-template slot. Prompting dataset-template options is a
> worthwhile follow-up, but it is a separate change and not needed for this ask.

## 5. Where expansion happens

One choke point per path, both before any `DTAColumnSpec` is constructed —
`specs_from_list()` does `do.call(DTAColumnSpec, x)`, and an unexpanded
`values_from` key would be an unused-argument error there.

1. **`build_dataset_from_template()`** (`dataset_template.R`) — a new step
   between the patch (step 3) and the return. **After** the patch, so that
   `add_columns`/`modify_columns` can introduce or override a binding, and so
   `set:` paths resolve against the final column list.
2. **`create_dta_from_template()`** (`template_create.R`) — for the inline,
   non-dataset-template dataset form, and for `vocabulary_slots:` application.
   Slots apply in the existing precedence chain, immediately after party
   profiles and before option effects.

Precedence and conflicts:

- A column carrying **both** `values:` and `values_from:` after patching is an
  **error** (`values_and_values_from`), matching the strictness of the existing
  `values`/`pattern` mutual exclusion rather than silently picking one.
- `values_from:` + `pattern:` is the same error the S7 validator would raise
  later; catch it in `validate_template()` first, with a template-level code.
- To drop an inherited binding in a patch: `set: columns.VISIT.values_from: null`
  (explicit YAML null already deletes a key in `dta_template_merge_value()`).

## 6. Type coherence — the trap worth pre-empting

`dta_warn_numeric_values_on_text_column()` exists because YAML silently reads an
unquoted `1.10` as the double `1.1`. A vocabulary of numeric-looking codes
(`"01"`, `"1.10"`) is exactly the shape that hits it. Two guards:

- `validate_template()` rejects an unquoted numeric-looking `code:` with the
  same reasoning (and the same severity) as the existing `version_unquoted`
  check. This is the vocabulary analogue of a bug the project has already been
  bitten by.
- The vocabulary's `type:` is checked against the bound column's declared type
  at expansion; a mismatch is a template error, not a runtime surprise.

## 7. Provenance, and the honest limitation

Slot choices are recorded in the machine-owned `metadata.template` block so a
document can be reconstructed. Preferred shape: a `vocabulary_selections`
sibling to `selections` in `template_provenance()`, rather than smuggling
generated ids into `selections` where rebase's diff would meet them.

**Rebase will not move a document onto a new vocabulary version.** Rebase is
*metadata only* — it explicitly never touches a dataset's columns, rules or
files. So publishing `visit@1.1` changes what *new* documents get and nothing
about existing ones. This is consistent with how `@latest` already behaves for
templates (resolve once, record the concrete version, never drift), but it is a
real limitation and should be documented as one rather than discovered.

## 8. Work breakdown

Each phase ends green: `Rscript -e "devtools::test()"`, then `styler::style_pkg()`
and `roxygen2::roxygenise()` before any commit (CI's `r-style` fails on a diff,
it does not auto-fix).

**Phase 1 — the artifact, indexed and validated.** New
`inst/shiny/dta_app/R/vocabulary.R`: `read_vocabulary()` (never throws, mirrors
`read_party_profile()`), term normalisation, `extends:`/`add_terms:`/
`remove_terms:` resolution, `vocabulary_terms()` with include/exclude. Register
`dta_vocabulary` in `dta_template_all_kinds()` and `dta_template_kind_pattern()`
(`template_index.R:27,40`). Extend `validate_template()`'s kind dispatch and its
`kinds` argument (`R/validateTemplate.R:412,694`). Tests:
`tests/testthat/test-template-vocabulary.R`.

**Phase 2 — column binding and expansion.** `expand_column_vocabularies()`,
wired into both choke points in §5. Conflict and type checks. Tests assert the
produced `DTAColumnSpec@values`, and that `values_from` is absent from the
written YAML.

**Phase 3 — `vocabulary_slots:` and the create-time picker.** Normalisation and
application mirroring `normalise_party_slots()` / `apply_party_selections()`,
including the re-validation of a stale selection that `apply_party_selections()`
already does. UI in `show_template_options_modal()`. Provenance recording.

**Phase 4 — the column-editor picker.** The app's column form already has
`textAreaInput("col_values", "Allowed values (one per line)")` at `app.R:2630`.
Add a "Choose from vocabulary…" button beside it. Render it as a **third
`rv$col_view`** (`"list" | "form" | "vocab"`) rather than a nested modal —
nested `showModal` is fragile, and the view switch is the pattern the file
already uses. Selecting terms writes them into the textarea; save goes through
the existing `dta_set_column()` path unchanged.

**Phase 5 — docs and examples.** A bundled `visit.dta-vocabulary.yaml`; wire it
into `gf_smrnaseq.dta-dataset-template.yaml` so the shipped example exercises
the feature. A "Controlled vocabularies" section in
`vignettes/private-templates.Rmd` — and update the **three** places that
currently enumerate the artifact suffixes (the vignette's CI section,
`validate_template()`'s roxygen, and `template_index.R`'s header comment).
`CHANGELOG.md` under `## [Unreleased]`, `DESCRIPTION` `Version:` → `0.26.0`.

**Housekeeping that is easy to forget:** any edit under `inst/shiny/dta_app/`
requires re-syncing `manifest.json` checksums (`bump_version.R --sync-manifest`),
and `bump_version.R` promotes `[Unreleased]` into a dated release heading —
undo that line on a feature branch.

## 9. Risks

| Risk | Mitigation |
| --- | --- |
| Scope creep into a "warn-only" enum severity | Explicitly out of scope (§4); it is an engine change |
| Numeric-looking codes silently mangled by YAML | `validate_template()` check, mirroring `version_unquoted` (§6) |
| `values_from` leaking into a produced DTA | Expansion before `specs_from_list()`; test asserts written YAML |
| Users expect vocab bumps to reach old documents | Documented limitation (§7), consistent with `@latest` |
| A 4th suffix colliding with the existing three | `dta-vocabulary` shares no full dotted segment with the others; the anchored regex handles it |

## 10. Open decision for the user

**Base branch.** This worktree (`ai/template-controlled-vocabulary-70f7ea`,
`13e4f9b`) does **not** contain the template system. It and the template branch
both descend from `a2a51f7` and have diverged; this branch carries 21 commits of
Shiny dataset-management work the template branch does not have. Nothing here
can be implemented until that is reconciled — and that is a history-rewriting
operation, so it needs an explicit go-ahead.
