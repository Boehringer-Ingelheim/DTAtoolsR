# Repository architecture

How to lay out a template library so that adding a term, a supplier or a study
is a small, reviewable change, and so that nothing already released moves.

## Contents

- [The layers](#the-layers)
- [Naming](#naming)
- [Versions](#versions)
- [Releasing a change: head and frozen files](#releasing-a-change-head-and-frozen-files)
- [Follow or pin](#follow-or-pin)
- [Ownership, review and deployment](#ownership-review-and-deployment)
- [Change recipes](#change-recipes)

## The layers

Every folder is flat, so the structure lives in the dependency graph, not in
directories. Build it in layers, and let references point in one direction
only: down this list.

```text
concrete templates   biomarker_study_acme      what authors pick in the app
        │ extends
family templates     biomarker_study           one per kind of transfer
        │ extends            │ mixins
company standard     company_dta (abstract)    mixin_privacy_eu, mixin_assay_biomarker
        │                    │ datasets / vocabulary_slots
dataset templates    biomarker_samples
        │ values_from
vocabularies         sample_status

party profiles       supplier_acme, receiver_examplepharma   (offered by party slots)
export-templates/    Word layouts                            (export dialog)
dta-app.yaml         app branding, features, exports
```

The rules that make this maintainable:

1. **Each fact lives in one file.** A clause every DTA carries belongs in the
   standard; a code list in a vocabulary; a column definition in a dataset
   template. A second copy is a second place to forget at the next update.
2. **References point down, never up or sideways.** A vocabulary never knows
   who uses it; the standard never extends a family; a mixin references
   nothing but dataset templates and vocabularies. Cycles are rejected
   (`extends_cycle`), but a sideways dependency that happens to resolve is
   still a maintenance trap.
3. **Keep `extends:` chains shallow**: standard → family → deviation. The
   engine stops at 8 levels; you should never need more than 3. Mixins do not
   add depth.
4. **Concrete templates are thin.** A deviation states only what differs from
   its parent. If it rewrites more than about a third of the parent, it is a
   new family, not a deviation.
5. **One library per organisation.** The linter checks only the repository it
   is pointed at, so a reference to a template that lives in another source
   shows up as an `*_unresolved` warning and is never dry-run. Keep everything
   a template depends on in this repository.

## Naming

Ids are permanent and are the only grouping a flat folder has, so choose them
for sorting and for reading in a reference.

| What | Id pattern | Examples |
| --- | --- | --- |
| Company standard (abstract root) | `<org>_dta`, `<org>_standard` | `company_dta` |
| Family | `<domain>_<topic>` | `biomarker_gf`, `central_lab` |
| Deviation | `<family id>_<qualifier>` | `biomarker_gf_acme`, `central_lab_study1234` |
| Mixin | `mixin_<topic>` | `mixin_privacy_eu`, `mixin_assay_smrnaseq` |
| Dataset template | `<domain>_<content>` | `gf_smrnaseq`, `biomarker_samples` |
| Vocabulary | the concept or the column it serves | `gf_test`, `sample_status` |
| Organisation subset of an external list | `<list>_<org>` | `lab_units_ourco` |
| Party profile | `<role>_<org>` | `supplier_acme`, `receiver_ourco` |

- Family-first ids keep `ls templates/` grouped by family, with each
  deviation next to its parent.
- Inside a mixin, prefix every option id, slot id and dataset `as:` with the
  mixin's topic (`privacy_clause`, not `clause`). Any id a mixin shares with
  the `extends:` chain or another mixin is a `mixin_conflict`, so topic
  prefixes keep blocks combinable later.
- Files: `<id>.<kind suffix>` for the current version, `<id>@<version>.<kind
  suffix>` for a frozen one (next section). The suffix decides the kind; the
  linter warns `id_filename_mismatch` when a stem does not start with the id.
- Renaming a file is harmless (`git mv`); renaming an id is not. Change an id
  only by creating a new template and retiring the old one.

## Versions

- Quoted `"MAJOR.MINOR"`, starting at `"1.0"`. Versions compare numerically:
  `"1.10"` is newer than `"1.9"`, and `"1.0"`/`"1.00"` tie (never do that).
- A reference names an exact version string or `latest`. There are no ranges
  (`@1.x`, `@^1` match nothing), which is why the follow/pin decision below
  matters.
- The template's `version:` is the version of the library definition. It is
  unrelated to `base.metadata.version`, the version of the document a template
  creates.
- **Minor** (`1.3` → `1.4`): adds or clarifies. A new term, a new optional
  option, a new nullable column, wording, a new choice in an option's
  `choices`, a new mixin block that adds content.
- **Major** (`1.4` → `2.0`): something a follower or a partner must react to.
  Removing or renaming any id (term code, column, option, slot, dataset `as`),
  changing a column's type, length or format, making a column non-nullable,
  sealing or requiring a path that already existed (a descendant may override
  it or leave it empty; lint names which), changing what an existing code or
  option means. Sealing or requiring a path that the same release introduces
  is minor: no descendant can have overridden it yet.
- Do not keep a changelog file for templates. The app computes each creation
  template's history by building its versions and diffing them, including
  releases of dependencies it follows. Explain *why* a release happened in the
  PR. Use `prior_versions:` only for versions a template went through before
  it entered this library.

## Releasing a change: head and frozen files

A version is **released** once it has been merged into the branch the app
reads (the `#ref` of the deployment's `DTATOOLS_TEMPLATE_SOURCES`, usually
`main`). From then on it is immutable: authors can still choose it in the
app's Version list, other templates may pin it, and the computed history is
built from it.

The library keeps every released version as a file. The **head** file
`<id>.<suffix>` always holds the newest version and is the one you edit; each
older release is a **frozen** byte-for-byte copy named
`<id>@<version>.<suffix>`.

To change a template, vocabulary or dataset template:

```sh
git fetch origin
git show origin/main:templates/company_dta.dta-template.yaml | grep '^version:'
```

- The command prints the version the head file has now: it is released.
  Freeze it, then edit and bump the head:

  ```sh
  cp templates/company_dta.dta-template.yaml "templates/company_dta@1.1.dta-template.yaml"
  # edit templates/company_dta.dta-template.yaml, set version: "1.2" (or "2.0")
  ```

- The command prints an older version, or fails because the file is new: the
  head's version is unreleased (this branch already bumped or created it).
  Edit in place; do not bump again.

Why this shape: the pull request shows a real diff of the head against the
released version, the frozen file proves the old version is unchanged, and
`git log templates/<id>.<suffix>` reads as the template's history.

Rules for frozen files:

- Never edit, rename or delete one. The guard in
  [linting.md](linting.md) checks this before every PR.
- A frozen file keeps following whatever it followed. If a change to a
  followed dependency makes a frozen version fail to build (lint reports it
  against `<id>@<version>.<suffix>`), that change is breaking for a version
  authors can still pick. Ask the owner. The one acceptable edit to a frozen
  file is pinning that reference to the dependency version it was released
  against, because it restores what the version always built.

Exception, **party profiles**: edit them in place and bump the version, with
no frozen copy. Slots reference profiles by id only, so nothing can ask for an
old version, and an outdated contact list (people who left) should not be kept
alive in a second file. Keep exactly one file per party id.

A new party profile is offered by **every** party slot of its role that has
no `profiles:` allow-list, in every template of the library, not only by the
template you add it for. Find those slots before adding a profile (a slot
block without a `profiles:` line), and name them in your report; restricting
one means changing a released template (freeze and bump it).

## Follow or pin

Every reference states its intent: `@latest` to **follow** the newest
version, `@x.y` to **pin** one.

| Reference | Default | Pin instead when |
| --- | --- | --- |
| `extends:` of a deviation or family | follow | the deviation was agreed with a partner against one release of its parent and must not move until re-agreed |
| `mixins:` | follow | a consumer must not receive the block's future content |
| `datasets[].template` without `patch:` | follow | the dataset structure is fixed by an agreement |
| `datasets[].template` **with** `patch:` | **pin** | (always) the patch names columns and was checked against that version only |
| `values_from:`, `vocabulary_slots[].vocabulary` | follow | the list is a dated external release, or a fixed list agreed with a partner |
| vocabulary `extends:` | follow your own lists | pin the external release you subset |
| `party_slots[].profiles` | ids only | (cannot pin) |

Following is safe in this layout because every pull request runs the linter,
which dry-runs every concrete template, frozen versions included, against the
changed library. A follower that a change breaks fails in the same pull
request that makes the change.

What the linter cannot see is a change in meaning: a removed code a partner
relies on, a reworded clause. That is what the major/minor rule is for. On a
major change to a dependency, list its followers and decide for each one:

```sh
grep -rnw 'sample_status' templates vocabulary
```

Following never changes a DTA that already exists. Creating a document
resolves `@latest` once, embeds the result, and records the concrete versions
in the document's provenance (`metadata.template.lineage`). Only documents
created afterwards see the new version. (The R-level rebase tooling rebuilds a
document's ancestor from the current library and so re-resolves `@latest`; pin
if a team relies on rebasing documents.)

## Ownership, review and deployment

- Protect the deploy branch: require the CI job scaffolded by
  `create_template_repo()` and one review before merge. A merge is a release.
  Check what that job runs: `lint_template_repo('.')` covers more than the
  `validate_template('.', strict = TRUE)` of older scaffolds (see the
  fallback in [linting.md](linting.md)); switch an older job to the linter
  once the deployment's DTAtools has it.
- Give each layer an owner with a `CODEOWNERS` file. Flat folders make id
  prefixes the selector, for example `vocabulary/` → data standards,
  `templates/company_*` and `templates/mixin_*` → governance, everything else
  → the study or vendor teams.
- Lint with the DTAtools version the deployment runs. A newer engine may
  accept what the deployed one rejects, and the reverse.
- To try a large change before release, point a second deployment (or a local
  app) at the branch: `DTATOOLS_TEMPLATE_SOURCES=staging=git:<url>#<branch>`.

## Change recipes

Each recipe assumes the loop in SKILL.md: baseline lint first, lint clean at
the end, report afterwards.

### Add a term to a vocabulary (minor)

1. Freeze-and-bump the vocabulary if its head is released.
2. Add the term: `code` (quoted if YAML could coerce it), `label`,
   `description`. Keep code order meaningful; it is the order authors see.
3. Followers receive it in new documents; pinned consumers do not. Name the
   pinned ones in the report and ask whether any should move.
4. Expected lint: no new findings.

### Remove or recode a term (major; ask first)

1. Freeze, bump the major version, remove or change the term.
2. Lint reports every consumer that names the code: `values_from_terms_invalid`
   (a binding's `include:`/`exclude:`), `vocab_slot_invalid` (a slot's
   `include:`, `exclude:` or `default:`), and `instantiate_failed` whose message
   names `rule_condition_value_outside_values` where a rule still compares a
   column against the removed code. Also search for the code in option
   `choices` and `effects`, which no check compares with the vocabulary:
   `grep -rn 'OLDCODE' templates`.
3. For each consumer, the owner decides: update it, or pin it to the previous
   vocabulary version.

### Change a dataset template

1. Freeze and bump (minor for a new nullable column or a clearer description;
   major for a removed, renamed or retyped column, or a tightened rule).
2. Consumers with a `patch:` are pinned and unaffected; to move one, re-pin it
   and lint: `patch_incoherent` means its patch no longer fits.
3. Expected lint: clean, or findings only on consumers that follow and that
   the change genuinely breaks.

### New supplier deviation

1. New file `templates/<family>_<supplier>.dta-template.yaml`, `version: "1.0"`,
   `extends: <family>@latest`, its own `label` and `description`.
2. Only the differences: option defaults through `options: {modify: …}`, base
   values, a party slot narrowed to the supplier's profile
   (`party_slots: {modify: [{id: supplier, profiles: [supplier_x]}]}`), and a
   pinned dataset `patch:` for column differences.
3. Add the supplier's party profile in `parties/` if it does not exist. It
   also appears in every supplier slot without an allow-list (see "Releasing
   a change"); say which in the report.
4. Verify with the template guide: its "How it differs from …" section must
   list exactly the intended differences, and its party-profile section must
   offer only the intended supplier. Check the second one there, because lint
   does not check profile ids written under `party_slots: {modify: …}` (see
   [linting.md](linting.md), `party_slot_invalid`).

### New family or study template from building blocks

1. `extends: <standard>@latest` plus `mixins: [mixin_a@latest, …]`; write
   only what no block provides.
2. If two blocks you need overlap, the overlap belongs in neither: move it to
   the standard or write it in this template.
3. Expected lint: clean. `mixin_conflict` names the overlapping key.

### Change the company standard (ask first)

1. Freeze and bump. Adding to `sealed:` or `required:` is a major change.
2. Lint lists every template the change breaks: `sealed_violation` where a
   descendant overrides a newly sealed path, `instantiate_failed` ("required
   field not filled") where a concrete template does not provide a newly
   required path.
3. Present the list to the owner: fix each descendant, or pin some to the
   previous standard (each pin is a change to a released template: freeze and
   bump it too).

### Update a party's contacts

Edit the profile in place and bump its minor version (see the exception
above). Documents already created keep the contacts they were created with.

### Extract repeated content into a mixin or the standard

A refactor must not change what any template builds.

1. Record the guide text of every affected concrete template (recipe in
   [linting.md](linting.md)).
2. Create the mixin (abstract, add-only, topic-prefixed ids) or move the
   content into the standard; remove it from the consumers; freeze and bump
   each changed consumer and the standard.
3. Lint clean, then compare the guide text: only the template versions may
   differ. Say "no change to what it builds" in the report.

### Retire a template

There is no retire flag, and authors can pick any version that exists.

- Preferred: release a new version whose `label` and `description` say
  "Superseded by <other template>; do not use for new agreements." The picker
  shows the newest version's label, so authors see it, and nothing is lost.
- Deleting every version of the id is the owner's call. Before proposing it,
  confirm nothing references the id (`grep -rnw '<id>' templates vocabulary`).
  Documents already created are unaffected, but the template's history and the
  ability to create or re-base from it are gone.

### Turn an existing DTA document into templates

1. Each dataset becomes a dataset template (body under `dataset:`); columns
   with a reusable value list become vocabularies bound with `values_from:`.
2. The metadata becomes the `base.metadata` of a family template (or of the
   standard, if it is company-wide); per-document fields (title, version,
   study) become options with `target:`; contacts become party profiles and
   slots; `date:` becomes `"${today}"`.
3. Record the document's earlier versions in `prior_versions:` only if they
   were versions of the *template*.
4. Compare the guide of the new template with the original document.
