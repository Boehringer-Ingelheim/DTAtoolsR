# Plan: Stronger DTA Creation Templates — Private Sources, Versions, Inheritance, Reuse

Status: awaiting review
Target branch: `dev` (PR into `dev`)
Version bump: `0.24.0` → `0.25.0` (user-facing; needs `CHANGELOG.md` `## [Unreleased]` entry)

---

## Goal

Turn "Create new from template" from a single packaged demo template into a
deployable template *system*:

1. **Private template sources** that never enter the open-source package, loaded
   at runtime from an internal directory, private R package, or Bitbucket git
   repo, configured with **one environment variable** so a Posit Connect deploy
   needs no further setup.
2. **Versioned templates** (`id@version`) with **inheritance** (`extends:`), so a
   study- or supplier-specific *deviation* is a small child template stating only
   what differs from the standard.
3. **Modular dataset templates** — a `kind: dta_dataset_template` file with its
   own id, version and options, imported by DTA templates and reusable across
   them, with declarative **patch** deviations at the point of import.
4. **Party reuse** — the same template serving different suppliers/receivers via
   reusable party profiles, metadata carried over from an existing YAML or from
   the currently open document, and a post-hoc "swap party" action.
5. **Rebase** — upgrade an existing DTA from template v1.0 to v1.1 with a
   three-way merge and conflict review.

## Decisions taken (from the requirements interview)

| Area | Decision |
| --- | --- |
| Private source | One env var, scheme-tagged: `DTATOOLS_TEMPLATE_SOURCES` with `dir:` / `pkg:` / `git:` |
| Git auth | HTTPS + token env var, refresh on demand |
| Failure mode | Serve last-good cached copy; hard error only when there is no cache |
| Public vs private | Private **replaces** public entirely when any private source is configured |
| Versioning | `id@version`, version declared **inside** the file, filenames free-form |
| Deviations | Template inheritance (`extends:`) **and** declarative dataset patch blocks |
| Dataset templates | First-class kind, **with its own options** |
| DTA-level provenance | New `DTAMetaData` property (see Task 6) — dataset-level provenance reuses existing fields |
| Metadata schema | Free to change — the model is still under construction, and **machine-owned fields that no author can set are explicitly acceptable** |
| Party reuse | All four mechanisms: profiles, import-from-YAML, carry-over-from-open, swap-party |
| Git host | **Bitbucket Server / Data Center** in production; **GitHub** must work too, for testing |
| Private repo CI | Wanted eventually → this package exports a public `validate_template()` for it to call (Task 15) |
| Sequencing | One release, everything together |

---

## Current State Analysis

### What exists and works

The template engine already lives in `inst/shiny/dta_app/R/template_core.R`
(574 lines) and is genuinely well built — the option model, `target:` shorthand,
`effects:` fan-out, `${today}` / `${version}` expressions and the YAML 1.1
`yes:`/`no:` key trap are all handled and tested (55 tests in
`tests/testthat/test-shinyapp-template.R`).

| File | Role in this feature |
| --- | --- |
| `inst/shiny/dta_app/R/template_core.R` | Discovery, read/validate, option model, DTA construction. All of Tasks 1–5 land here or in new siblings. |
| `inst/shiny/dta_app/app.R:769-839` | The three observers: open picker → load template → create. Modal UI at `app.R:548-689`. |
| `inst/shiny/dta_app/app.R:5803` | Landing-page `create_from_template` button. |
| `inst/shiny/dta_app/R/utils_dta.R:1900` | `dta_dataset_to_list()` — the hand-written dataset serialiser. |
| `inst/shiny/dta_app/R/utils_dta.R:1932` | `dta_to_list()` → calls `as.list(md)` for the metadata block. |
| `R/DTAMetaData-class.R:91-102` | The 11 metadata properties. |
| `R/DTAMetaData-helpers.R:45` | `as.list(DTAMetaData)` — **hand-written, field by field**. |
| `R/DTADataSet-class.R:55-57` | `template_source`, `template_version`, `template_date`. |
| `R/DTA-class.R:755` | `do.call(DTAMetaData, x$metadata)` — the read path. |
| `R/DTADataSet-class.R:326` | `do.call(DTADataSetFactory, x)` — the dataset read path. |
| `tests/testthat/helper-shinyapp.R` | `app_fn()` / `app_env()` — the only harness for app-helper unit tests. |

### Four verified findings that shape the plan

1. **✓ VERIFIED — `DTADataSet` already carries dataset-level template
   provenance.** `template_source`, `template_version`, `template_date` are real
   S7 properties (`R/DTADataSet-class.R:55-57`), inherited by
   `DTADataSetTabular` and `DTADataSetFile`, already emitted by the app's writer
   (`utils_dta.R:1909-1914`), already read back by `do.call(DTADataSetFactory, x)`,
   already printed by `print()` (`DTADataSet-class.R:184-191`) and already
   consumed by the export footer (`exportDocuments.R:922`). Today the packaged
   template hand-types them as prose
   (`biomarker_gf.dta-template.yaml`: `template_source: GF domain smrnaseq`).
   **Dataset-level provenance therefore needs no S7 change at all** — only a
   mechanism that fills the fields honestly. This materially shrinks the diff.

2. **✓ VERIFIED — `as.list(DTAMetaData)` is hand-written, not property-driven.**
   `R/DTAMetaData-helpers.R:45-89` lists each field explicitly. A new S7 property
   will **silently vanish on save** unless that function is edited too.

3. **✓ VERIFIED — the existing round-trip test would not catch that.**
   `tests/testthat/test-shinyapp-utils.R:838` asserts only dataset names, column
   ids and rule ids survive a round trip. **Metadata round-trip is untested.**

4. **✓ VERIFIED — `dta_template_metadata_fields()` derives from the S7 class.**
   `template_core.R:117` computes the writable set as
   `setdiff(names(dta_metadata_properties()), "import_issues")`. A new `template`
   property automatically becomes writable from template YAML — i.e. **forgeable
   provenance** — unless it is excluded alongside `import_issues`.

5. **✓ VERIFIED — exports name metadata fields explicitly** (`{VERSION_HISTORY}`,
   `meta@version_history`), so adding a property will *not* silently alter
   generated Word/PDF documents. Low risk there.

---

## Architecture

```
DTATOOLS_TEMPLATE_SOURCES  ──►  template_sources.R   resolve each source to a local root
    "dir:…;pkg:…;git:…#ref"          (cache + staleness + token redaction)
                                            │
                                            ▼
                                   template_index.R   scan roots, read headers,
                                            │         index by kind + id@version
                                            ▼
                                  template_inherit.R   resolve `extends:` chains
                                            │
                    ┌───────────────────────┼───────────────────────┐
                    ▼                       ▼                       ▼
          dta_creation_template    dta_dataset_template     dta_party_profile
                    │                       │                       │
                    └──────────► template_core.R ◄──────────────────┘
                                  create_dta_from_template()
                                            │
                                            ▼
                                  DTA + metadata.template  (provenance, selections, lineage)
                                      + DTADataSet@template_* (existing fields, now stamped)
                                            │
                                            ▼
                                   template_diff.R  →  rebase / conflict review
```

### Configuration surface (the whole of it)

| Variable | Required | Meaning |
| --- | --- | --- |
| `DTATOOLS_TEMPLATE_SOURCES` | no | `;`-separated scheme-tagged sources. Setting it switches the app to private-only. |
| `DTATOOLS_TEMPLATE_GIT_TOKEN` | only for `git:` | HTTP access token. Never logged, never persisted. |
| `DTATOOLS_TEMPLATE_GIT_USER` | no | Basic-auth username. Default `git`, which both Bitbucket Data Center and GitHub accept alongside a token-as-password. |
| `DTATOOLS_TEMPLATE_GIT_AUTH` | no | `basic` (default) or `bearer`. Bitbucket Data Center also accepts `Authorization: Bearer <token>`; `bearer` is there for a DC instance configured to require it. |
| `DTATOOLS_TEMPLATE_CACHE_DIR` | no | Overrides the cache location. Defaults to `tools::R_user_dir()`, falling back to `tempdir()` when that is not writable. |
| `DTATOOLS_TEMPLATE_REFRESH_SECONDS` | no | Git re-fetch TTL, default `900`. |
| `DTATOOLS_TEMPLATE_INCLUDE_BUILTIN` | no | Escape hatch to keep packaged templates visible alongside private ones. Default `false` when private sources are configured. |
| `options(DTAtools.template_dir=)` | no | **Retained unchanged** for backward compatibility (two existing tests pin it). |

Source string grammar:

```
sources  := source (";" source)*
source   := [name "="] scheme ":" locator ["#" ref]
scheme   := "dir" | "pkg" | "git"
```

Example for a Posit Connect content variable:

```
DTATOOLS_TEMPLATE_SOURCES=internal=git:https://bitbucket.example.com/scm/dta/templates.git#main
```

---

## Tasks

### Task 1: Template source providers

New file `inst/shiny/dta_app/R/template_sources.R`.

- [ ] `parse_template_sources(spec)` — parse the grammar above into a list of
      `list(name, scheme, locator, ref)`. Reject unknown schemes with a `cli`
      error naming the offending entry. Tolerate whitespace and a trailing `;`.
- [ ] `resolve_template_source(src)` → `list(name, scheme, origin, ref, root,
      ok, stale, stale_age, resolved_commit, error)`.
  - `dir:` — `dir.exists()` check, `normalizePath()`.
  - `pkg:` — `system.file("dta-templates", package = <pkg>)`; empty string means
    the package is installed but ships no templates: distinguish that from
    "package not installed".
  - `git:` — see Task 2.
- [ ] `template_cache_dir()` — resolve `DTATOOLS_TEMPLATE_CACHE_DIR`, else
      `tools::R_user_dir("DTAtools", "cache")`, **probe writability by actually
      writing a file**, else fall back to `file.path(tempdir(),
      "DTAtools-templates")`. Connect content accounts frequently cannot write
      `R_user_dir`.
- [ ] `template_source_roots()` — resolve every configured source in order and
      return the ok ones plus a diagnostics table of the failures.
- [ ] **Private-replaces-public.** When `DTATOOLS_TEMPLATE_SOURCES` is non-empty,
      exclude `system.file("extdata", "templates", package = "DTAtools")` and
      `./dta-templates` from the root list unless
      `DTATOOLS_TEMPLATE_INCLUDE_BUILTIN` is truthy. When it is empty, behave
      exactly as today (packaged + `./dta-templates` + `getOption()`).
- [ ] **Hard fail with no fallback.** If sources are configured and *every* one
      fails with no usable cache, return zero roots and a structured error. The
      caller must show the diagnostic and offer no templates — never silently
      fall back to the packaged demo template.

**Files to create:** `inst/shiny/dta_app/R/template_sources.R`

### Task 2: Git source provider

In `template_sources.R`.

- [ ] `git_source_root(locator, ref, name)` — cache path is
      `file.path(template_cache_dir(), "git", rlang::hash(paste0(locator, "#", ref)))`.
      (`rlang` is already in `Imports:`; no new dependency. It is xxhash128, so
      label hashes `hash:` and never `sha256:`.)
- [ ] First run: `git clone --depth 1 --branch <ref>`. Later runs: `git fetch
      --depth 1 origin <ref> && git reset --hard FETCH_HEAD`.
- [ ] Skip the fetch entirely when the cache was refreshed within
      `DTATOOLS_TEMPLATE_REFRESH_SECONDS` — a Connect process is long-lived and
      must not hit Bitbucket on every modal open.
- [ ] **Credentials via `http.extraHeader` passed in the child process's
      ENVIRONMENT** — not in the URL, not on the command line, and not on disk.
      `GIT_CONFIG_COUNT=2` with `GIT_CONFIG_KEY_0=http.extraHeader` /
      `GIT_CONFIG_VALUE_0=Authorization: …`, plus
      `GIT_CONFIG_KEY_1=credential.helper` / `GIT_CONFIG_VALUE_1=` so a
      developer's OS keychain cannot silently satisfy — or poison — the auth and
      mask a broken token in testing.
      *Revised during implementation:* this originally said `GIT_ASKPASS`, which
      needs a `.bat` shim on Windows and still writes the token to disk. The
      env-var route is portable, keeps the token out of `.git/config`
      (`remote.origin.url`) and out of the process table, and writes it nowhere.
- [ ] **One auth path for both hosts.** `basic` (the default) sends
      `Authorization: Basic base64(<user>:<token>)`; `bearer` sends
      `Authorization: Bearer <token>`. Basic auth with
      `DTATOOLS_TEMPLATE_GIT_USER` (default `git`) is accepted by **Bitbucket
      Data Center** HTTP access tokens and **GitHub** PATs alike, so production
      and the GitHub test repo share one code path. `base64` comes from
      `jsonlite`, already in `Imports:`.
- [ ] Hardening on every invocation: `GIT_TERMINAL_PROMPT=0` so a bad token fails
      fast instead of blocking the Shiny process on an interactive prompt,
      `GIT_CONFIG_NOSYSTEM=1`, and `GIT_ASKPASS=""`.
- [ ] **`system2()`'s `env=` argument is not supported on Windows.** Set the
      variables with `Sys.setenv()`, capture the prior values, and restore them —
      including `Sys.unsetenv()` for previously-unset ones — in `on.exit()`.
- [ ] **Redact.** Every captured stderr line passes through
      `redact_secrets()` before it reaches a notification, a log, or a test
      snapshot. Redact the token value and anything matching
      `://[^/@]*:[^/@]*@`.
- [ ] `git` absent from `PATH` → that source fails with a clear message, and the
      cache (if any) is still used.
- [ ] Record `resolved_commit` via `git rev-parse HEAD` for provenance.
- [ ] On any failure with a non-empty cache: return the cache, `stale = TRUE`,
      `stale_age = <seconds since last successful refresh>` (persist a
      `.last-refresh` stamp file).

**Files to modify:** `inst/shiny/dta_app/R/template_sources.R`

### Task 3: Versioned template index (`id@version`)

New file `inst/shiny/dta_app/R/template_index.R`.

- [ ] Scan every root for `*.dta-template.ya?ml`, `*.dta-dataset-template.ya?ml`,
      `*.dta-party.ya?ml`.
- [ ] `read_template_header(path)` — parse `kind`, `id`, `version`, `label`,
      `description`, `extends`, `abstract`, `role`, `date`. Malformed files are
      **skipped with a collected warning**, never fatal — one bad file in a
      private repo must not take down the picker.
- [ ] `build_template_index(roots)` → data frame with one row per
      `kind + id + version`, carrying `path`, `source_name`, `source_kind`,
      `resolved_commit`, `abstract`.
- [ ] `resolve_template_ref(index, ref, kind)` — accept `id`, `id@version`,
      `id@latest`. Bare `id` resolves to the highest version. Version ordering by
      `numeric_version()`; a version that will not parse is kept but ordered last
      **with a warning**, and is only reachable by exact reference.
- [ ] **Collision policy.** Same `kind+id+version` from two sources: earliest
      source in the configured order wins; emit a warning naming both paths.
- [ ] `abstract: true` entries resolve for `extends:` but are hidden from the
      picker.
- [ ] Index is memoised per process with the same TTL as the git refresh, and
      invalidated by the "Refresh templates" button (Task 11).

**Files to create:** `inst/shiny/dta_app/R/template_index.R`
**Files to modify:** `inst/shiny/dta_app/R/template_core.R` — reroute
`list_dta_creation_templates()` and `get_dta_creation_template_path()` through
the index, keeping their signatures so the existing 55 tests still describe real
behaviour.

### Task 4: Template inheritance (`extends:`)

New file `inst/shiny/dta_app/R/template_inherit.R`.

- [ ] `resolve_template_inheritance(def, index)` — walk the `extends:` chain,
      nearest-ancestor-last, and merge.
- [ ] Merge rules, stated explicitly because they are the whole contract:
  - `base.metadata` — recursive deep merge, child leaf wins. A child leaf that is
    explicitly YAML `null` **removes** the parent key.
  - `options` — merged by option `id`. Same id: deep-merge the option maps, child
    keys win. `remove: true` on a child option drops the inherited option.
    Ordering: inherited order first, then new child options appended, unless the
    child sets `order: [id, id, …]`.
  - `datasets` — merged by the entry's `as:` name (or the dataset template id, or
    the dataset `name:` for inline entries). Child may add entries and may
    `remove: true` an inherited one.
  - `party_slots` — merged by slot `id`, child wins.
  - Scalars (`label`, `description`, `abstract`) — child wins; `id` and `version`
    are **never** inherited and must be declared by every file.
- [ ] Cycle detection and a depth limit of 8, both raising a `cli` error that
      names the chain.
- [ ] Record the resolved chain for provenance as `lineage`.

**Files to create:** `inst/shiny/dta_app/R/template_inherit.R`

### Task 5: Dataset templates with options and patch deviations

New file `inst/shiny/dta_app/R/dataset_template.R`.

New file kind:

```yaml
kind: dta_dataset_template
id: gf_smrnaseq
version: "3.0"
label: GF domain smrnaseq
description: Genomic Findings data table
date: 2024-12-17          # stamped into DTADataSet@template_date
options:
  - id: vendor_name
    label: Vendor name
    type: text
    target: dataset.columns.GFNAM.values
dataset:                   # the DTADataSet spec, exactly today's shape
  name: gf_data_specs_pattern
  type: tabular
  files: { … }
  columns: [ … ]
  rules: [ … ]
```

Imported from a DTA template:

```yaml
datasets:
  - template: gf_smrnaseq@3.0
    as: gf_data_specs_pattern        # optional rename
    options:
      vendor_name: ACME Labs
    patch:                           # the "deviation" from the standard
      set:
        description: GF data, ACME variant
        files.filename: acme_gf.tsv
      add_columns:
        - { id: ACMEID, label: ACME internal id, type: SAS Char, format: SAS $20., nullable: "Yes" }
      modify_columns:
        - { id: GFNAM, values: [ACME Labs] }
      remove_columns: [GFPVRID]
```

- [ ] `read_dataset_template(path)` — validate `kind`, require `id`, `version`,
      `dataset`.
- [ ] `apply_dataset_template_path(ds_list, path, value)` — the `dataset.` path
      root, mirroring `apply_template_metadata_path()`. Support
      `dataset.<field>`, `dataset.files.<key>`,
      `dataset.columns.<COLUMN_ID>.<field>`, `dataset.rules.<RULE_ID>.<field>`.
      **Columns are addressed by `id`, never by index** — an index reference
      would break the moment a patch inserts a column.
- [ ] Reuse `collect_option_effects()` and `dta_template_default()` unchanged;
      only the path root differs. Do not fork the option engine.
- [ ] `apply_dataset_patch(ds_list, patch)` — `set` / `add_columns` /
      `modify_columns` / `remove_columns`. `remove_columns` naming an absent id
      is an error, not a silent no-op. `add_columns` duplicating an existing id
      is an error. Each applied op returns a deviation record
      `list(op, target)`.
- [ ] **Stamp provenance onto the built dataset:**
      `template_source = "<id> (<source_name>)"`,
      `template_version = <version>`,
      `template_date = format(<date>)`. Note `template_date` is
      `class_character_or_null` while YAML parses a bare `2024-12-17` as a
      `Date` — it **must** be `format()`ed or the S7 validator rejects it.
- [ ] `create_dta_from_template()` gains dataset-template resolution as a fourth
      accepted `datasets:` entry form. **The three existing forms — plain string,
      `{source: …}`, and full inline object — must keep working byte-identically.**

**Files to create:** `inst/shiny/dta_app/R/dataset_template.R`
**Files to modify:** `inst/shiny/dta_app/R/template_core.R:466-540`
(`create_dta_from_template` dataset loop)

### Task 6: `metadata.template` provenance property

The one S7 class change. Per `CLAUDE.md` this is a plan-mode decision, recorded
here: **a new `template` property on `DTAMetaData`, of `class_list`, default
`list()`.**

The metadata model is still under construction and machine-owned fields are
explicitly acceptable, so this property is **machine-owned**: written only by the
template engine, never settable from template YAML or the metadata editor, and
shown read-only in the app. That makes `import_issues` and `template` a category
rather than a one-off exception — Task 6 introduces
`dta_metadata_machine_fields()` naming both, and
`dta_template_metadata_fields()` subtracts it.

Serialised shape:

```yaml
metadata:
  template:
    id: biomarker_gf
    version: "1.1"
    source: internal
    source_kind: git
    source_ref: main
    resolved_commit: 9f3c1ab
    content_hash: "hash:8f2a…"        # rlang::hash of the merged definition
    created: 2026-08-29
    lineage: [biomarker_gf_base@1.0]  # extends chain, nearest first
    selections:                        # REQUIRED by rebase — see Task 10
      title: BIOMARKER GF DATA TRANSFER SPECIFICATIONS (DTS)
      version: "1.1"
    carried_over_from: acme_2025.yaml  # optional, Task 8
    datasets:
      - name: gf_data_specs_pattern
        template: gf_smrnaseq
        version: "3.0"
        content_hash: "hash:1c9d…"
        deviations:
          - { op: add_columns, target: ACMEID }
          - { op: remove_columns, target: GFPVRID }
```

- [ ] `R/DTAMetaData-class.R` — add `template = S7::new_property(S7::class_list,
      default = list())` to `properties`, a matching constructor argument
      defaulting to `list()`, the `@param` roxygen block, and a validator branch
      requiring a list.
- [ ] `R/DTAMetaData-helpers.R:45` — **`as.list()` must emit `template` when
      non-empty.** Without this the provenance is written to the object and lost
      on the next save. Place it after `authorized_for_corrections`.
- [ ] `inst/shiny/dta_app/R/template_core.R:117` — introduce
      `dta_metadata_machine_fields()` returning `c("import_issues", "template")`
      and have `dta_template_metadata_fields()` subtract it, so a template author
      cannot forge provenance from YAML. The metadata editor must exclude the
      same set.
- [ ] `R/DTAMetaData-class.R` validator — require a list, and when non-empty
      require `id` and `version` to be single non-empty strings. A partially
      written provenance block is worse than none, because rebase would trust it.
- [ ] Confirm `.dta_stringify_dates()` (`utils_dta.R`) recurses into the nested
      `created` value; it is applied to the whole metadata list in
      `dta_to_list()`, so this is expected to work — **verify with a test, do not
      assume.**
- [ ] Read path needs no change: `do.call(DTAMetaData, x$metadata)`
      (`R/DTA-class.R:755`) picks the key up automatically.
- [ ] Exports need no change (fields are named explicitly, not enumerated).
- [ ] `Rscript -e "roxygen2::roxygenise()"` — read `Config/roxygen2/version` from
      `DESCRIPTION` (currently `8.1.0`); never hardcode it.

**Files to modify:** `R/DTAMetaData-class.R`, `R/DTAMetaData-helpers.R`,
`inst/shiny/dta_app/R/template_core.R`

### Task 7: Party profiles

New kind `dta_party_profile` in `*.dta-party.yaml`, discovered by the same
providers.

```yaml
kind: dta_party_profile
id: supplier_acme
version: "1.0"
role: supplier        # supplier | receiver | any
label: ACME Labs
affiliation: { name: ACME Labs, address: "…", country: DE }
contacts:
  - { name: "…", role: Data Manager, email: "…", signature: true }
```

A DTA template declares slots:

```yaml
party_slots:
  - id: supplier
    target: metadata.supplier
    label: Supplier
    profiles: [supplier_acme, supplier_beta]   # optional allow-list; omit = all matching role
```

- [ ] `read_party_profile(path)`, indexed by Task 3.
- [ ] Creation modal renders one dropdown per slot, listing profiles whose `role`
      matches, plus "(leave as template default)".
- [ ] Applying a profile **replaces** the whole target block (`affiliation` +
      `contacts`), it does not merge — a half-merged contact list is worse than
      either input.

**Files to create:** `inst/shiny/dta_app/R/party_profiles.R`

### Task 8: Metadata carry-over at creation

- [ ] New optional step in the creation flow: **"Carry over metadata"**, with
      three sources — upload a `.yaml`, the currently open DTA (offered only when
      `rv$dta` is non-`NULL`), or none.
- [ ] Checkbox list of metadata blocks. **Defaults on:** `receiver`, `supplier`,
      `transmission`, `error_handling`, `authorized_for_corrections`.
      **Defaults off:** `title`, `version`, `date`, `version_history` — a new
      document must not silently inherit the old one's identity and history.
      `template` and `import_issues` are never offered.
- [ ] **Application order is `base metadata → carry-over → party profiles →
      options → ${version} expressions`.** An option the user explicitly set must
      win over a carried-over value, and a party profile chosen for this document
      must win over a party carried over from an old one.
- [ ] Record `carried_over_from` and the applied field list in the provenance.

**Files to modify:** `inst/shiny/dta_app/app.R` (modal + observers),
`inst/shiny/dta_app/R/template_core.R` (`create_dta_from_template` signature
gains `carry_over = NULL`)

### Task 9: Post-creation "swap party" action

- [ ] A control in the metadata editor: *Replace supplier / receiver from…* →
      a party profile, an uploaded YAML, or the open document.
- [ ] Replaces the whole `supplier` or `receiver` block. Guarded by the existing
      double-click guard added in 0.24.0 (PR #78).
- [ ] This is an ordinary metadata edit and is **not** recorded as a template
      deviation — provenance tracks template lineage, not user edits.

**Files to modify:** `inst/shiny/dta_app/app.R`, `inst/shiny/dta_app/R/utils_dta.R`

### Task 10: Diff engine and rebase

New file `inst/shiny/dta_app/R/template_diff.R`.

- [ ] `materialise_template(index, id, version, selections, carry_over = NULL)` —
      rebuild the DTA a template *would* produce. This is why `selections:` must
      be in the provenance block (Task 6); without it the ancestor cannot be
      reconstructed and rebase degrades to a manual two-way compare.
- [ ] `dta_diff(a, b)` — metadata leaf-path diff plus per-dataset column diff
      keyed by column `id`. Returns added / removed / changed with old and new
      values.
- [ ] `rebase_dta(current, index, to_version)`:
  1. Read `metadata.template` for `id`, `version`, `selections`.
  2. Materialise the **ancestor** at the recorded version.
  3. Materialise the **target** at `to_version`.
  4. Three-way classify each leaf: user-only change → keep user; template-only
     change → take template; both changed to different values → **conflict**.
  5. Present every change and conflict for review before anything is applied.
  6. On confirm: apply, append a `version_history` entry, update
     `metadata.template` (`version`, `content_hash`, `resolved_commit`,
     `lineage`).
- [ ] **Degradation path:** when the ancestor version is no longer resolvable
      (pruned from the private repo), skip the three-way merge, show a two-way
      target-vs-current comparison, and require an explicit per-field choice. Say
      plainly in the UI that the ancestor was unavailable.
- [ ] Rebase never writes without confirmation, and never silently drops a user
      edit.

**Files to create:** `inst/shiny/dta_app/R/template_diff.R`

### Task 11: App UI

- [ ] Rework the template picker (`app.R:769-798`) into a grouped list: source
      name, template label, version selector, description. Badge stale sources
      with their cache age.
- [ ] **Diagnostics panel** when sources fail: which source, which scheme, the
      redacted error, and whether a cache is being served.
- [ ] **"Refresh templates"** button — re-fetch git sources and invalidate the
      index memo.
- [ ] Party-slot dropdowns (Task 7) and the carry-over step (Task 8) in the
      creation modal.
- [ ] "Update from template…" action on a loaded DTA that has provenance
      (Task 10), with the conflict review screen.
- [ ] Show current provenance read-only in the metadata editor.
- [ ] Every new button guarded against double-click per the 0.24.0 pattern.

**Files to modify:** `inst/shiny/dta_app/app.R`,
`inst/shiny/dta_app/R/ui_components.R`

### Task 12: Split the packaged template to exercise the new machinery

- [ ] Extract the ~300-line inline dataset from
      `inst/extdata/templates/biomarker_gf.dta-template.yaml` into
      `inst/extdata/templates/gf_smrnaseq.dta-dataset-template.yaml`
      (`id: gf_smrnaseq`, `version: "3.0"`, `date: 2024-12-17` — the values the
      current file already hand-types as `template_source` / `template_version` /
      `template_date`).
- [ ] Rewrite the DTA template to import it, and add `id`/`version` headers.
- [ ] Add a small `biomarker_gf_acme.dta-template.yaml` that `extends:` it and
      patches the dataset — a worked example of a supplier deviation, and a live
      test of both mechanisms.
- [ ] Add one example party profile per role.
- [ ] The reduction in the packaged template's size is the proof the modular
      import works.

**Files to modify/create:** `inst/extdata/templates/*`

### Task 13: Tests

New files under `tests/testthat/`:

- [ ] `test-shinyapp-template-sources.R` — grammar parsing incl. malformed input;
      per-scheme resolution; **cache-serves-on-failure and hard-fail-without-cache**;
      staleness age; **token redaction** (assert the token string appears in no
      returned message); cache-dir writability fallback; private-replaces-public
      and the `INCLUDE_BUILTIN` escape hatch.
- [ ] `test-shinyapp-template-index.R` — `id@version` resolution, bare-id-is-highest,
      unparseable versions, cross-source collisions, `abstract` hidden from the
      picker, a malformed file skipped rather than fatal.
- [ ] `test-shinyapp-template-inherit.R` — each merge rule above, explicit-null
      removal, `remove: true`, `order:`, cycle detection, depth limit.
- [ ] `test-shinyapp-dataset-template.R` — dataset options, every patch op,
      error on removing an absent column and adding a duplicate id, column
      addressing by id survives an insertion, **`template_date` Date→character
      coercion**, and all three legacy `datasets:` forms still work.
- [ ] `test-shinyapp-template-parties.R` — slot rendering, role filtering,
      whole-block replacement, swap-party.
- [ ] `test-shinyapp-template-carryover.R` — default on/off sets, and the
      documented precedence `base → carry-over → profile → option`.
- [ ] `test-shinyapp-template-rebase.R` — user-only / template-only / conflict
      classification; missing-ancestor degradation; nothing applied without
      confirmation.
- [ ] **Extend `tests/testthat/test-shinyapp-utils.R:838`** so the round-trip test
      asserts *metadata* equality, not just dataset/column/rule ids. This is the
      test that would otherwise let Task 6's `as.list()` omission ship silently.
- [ ] `tests/testthat/test-DTAMetaData.R` — the `template` property round-trips
      through `as.list()` → YAML → `DTAMetaData()`, and is **rejected** as a
      `base.metadata` field in a template.

Git tests use a **local bare repo** created with `system2("git", "init --bare")`
in `tempdir()` — no network. Guard with
`skip_if(!nzchar(Sys.which("git")), "git not on PATH")`.

**Every test must set the new env vars through `withr::local_envvar()`** — a
developer machine with `DTATOOLS_TEMPLATE_SOURCES` exported would otherwise flip
the suite into private-only mode and break unrelated tests.

### Task 14: Documentation and deployment

- [ ] New vignette `vignettes/private-templates.Rmd` — authoring the three file
      kinds, the versioning and `extends:` contract, the patch grammar, and a
      **Posit Connect** section: set one content variable in the Vars pane,
      redeploy nothing else. All chunks `eval = FALSE`, so the check-time cost is
      negligible.
- [ ] `README.md` — a short "Private templates" section pointing at the vignette.
- [ ] `.gitignore` — add `dta-templates/`, `*.dta-party.yaml`,
      `*.dta-dataset-template.yaml` outside `inst/extdata/templates/`. Private
      templates carry real supplier names and contacts and must never be
      committable to the open-source repo by accident.
- [ ] `DESCRIPTION` — bump `Version:` to `0.25.0`. **No new dependencies**
      (`rlang` for hashing and `yaml` are already in `Imports:`; git is invoked
      via `system2()`).
- [ ] `CHANGELOG.md` — entry under `## [Unreleased]`.
- [ ] `inst/shiny/dta_app/manifest.json` — regenerate via the documented
      `--sync-manifest` path after the app files change.

### Task 15: Public `validate_template()` for the private repo's CI

The private template repo will eventually want its own CI. It cannot import the
app's auto-sourced helpers, so the validator has to be a **package export**.

- [ ] New `R/validateTemplate.R`, added to `Collate:` in `DESCRIPTION` (after
      `DTAMetaData-helpers.R`, before `documentBuilders.R`).
- [ ] `validate_template(path, ...)` — exported, works on a single file **or a
      directory**, and validates all three kinds:
  - `kind` is one of the three known values, and `id` / `version` are present and
    well-formed (`version` must parse as `numeric_version()`).
  - Every `target:` / effect `path:` resolves to a real settable field —
    `metadata.*` against `dta_template_metadata_fields()`, `dataset.*` against
    the dataset path grammar. **A machine-owned field is a validation error**,
    which is what stops a private template forging provenance.
  - Every `extends:` and every `datasets[].template` reference resolves within
    the directory being validated; cycles and depth-limit breaches are errors.
  - Patch ops are coherent: no `remove_columns` for an absent id, no
    `add_columns` duplicating one.
  - Party slots name a `target:` that is `metadata.supplier` or
    `metadata.receiver`, and referenced profile ids exist.
  - Duplicate `kind+id+version` within the directory is an error.
  - **Dry-run instantiation:** actually build the DTA from each non-abstract
    template with default selections and report any error. This catches far more
    than schema checks alone.
- [ ] Returns a structured result (a tibble of `file`, `severity`, `code`,
      `message`) and gains a `strict = TRUE` argument that `cli_abort()`s on any
      error — so the private repo's CI is a one-liner:
      `Rscript -e 'DTAtools::validate_template("templates/", strict = TRUE)'`
- [ ] The app's own loader routes through the same validator, so a template that
      passes CI cannot fail differently at runtime, and vice versa.
- [ ] Roxygen block with a runnable `@examples` pointing at
      `system.file("extdata", "templates", package = "DTAtools")` — it must
      execute under `R CMD check`, so no `\dontrun{}`.
- [ ] Ship `inst/extdata/templates/validate-templates.yml` — a ready-to-copy
      GitHub Actions workflow for the private repo, since that repo will be
      GitHub during testing. Documented in the vignette for translation to
      Bitbucket Pipelines.

**Files to create:** `R/validateTemplate.R`,
`inst/extdata/templates/validate-templates.yml`
**Files to modify:** `DESCRIPTION` (`Collate:`), `NAMESPACE` (generated)

---

## Success Criteria

### Automated

- [ ] `Rscript -e "devtools::test()"` — full suite green, **0 skips** on this
      machine (the TinyTeX/pandoc baseline is already met here).
- [ ] `Rscript -e "devtools::test(filter='template')"` — the 55 existing template
      tests still pass **unmodified**, proving backward compatibility.
- [ ] `Rscript -e "roxygen2::roxygenise()"` produces no diff after being run once.
- [ ] `Rscript -e "styler::style_pkg()"` produces no diff.
- [ ] `Rscript -e "rcmdcheck::rcmdcheck(args='--no-manual')"` — 0 errors,
      0 warnings, no new notes.
- [ ] `pre-commit run --all-files` passes (remember `export PATH="/c/Program Files/R/R-4.5.1/bin:$PATH"` in the same command).
- [ ] `.github/scripts/check_deps_in_desc.R` passes — every namespaced call is
      declared.

### Manual

- [ ] With no env var set, the app behaves exactly as 0.24.0: the packaged
      template is listed and creates the same DTA.
- [ ] `DTATOOLS_TEMPLATE_SOURCES=dir:<tmp>` — only the private templates are
      listed; the packaged one is gone.
- [ ] Point a `git:` source at a local bare repo, create a DTA, then commit a
      template change, press **Refresh templates**, and see the new version.
- [ ] Break the git source (bad ref): the app keeps working from cache and shows
      a staleness banner with the age.
- [ ] Delete the cache and break the source: the app shows a clear diagnostic and
      offers **no** templates — the packaged demo does not reappear.
- [ ] `DTATOOLS_TEMPLATE_GIT_TOKEN` never appears in any notification, log line,
      or `.git/config` in the cache directory.
- [ ] Create a DTA, save it, reload it: `metadata.template` survives intact.
- [ ] Create from the ACME deviation template: the dataset carries the patched
      columns and honest `template_source` / `template_version` / `template_date`.
- [ ] Create two DTAs from one template with different supplier profiles.
- [ ] Rebase a v1.0 document onto v1.1 with one user edit and one template edit
      to the same field: the conflict is shown and nothing is applied until
      confirmed.
- [ ] Deploy to a Connect instance with one content variable set and confirm the
      internal templates appear with no other configuration.

---

## Risks (Pre-Mortem)

### Tigers

- **New S7 property silently lost on save (HIGH).** `as.list(DTAMetaData)` is
  hand-written (`R/DTAMetaData-helpers.R:45`) and the existing round-trip test
  (`test-shinyapp-utils.R:838`) checks only dataset/column/rule identity, so the
  omission would pass CI and destroy every document's provenance on first save.
  *Mitigation:* Task 13 extends that test to assert metadata equality **before**
  Task 6 adds the property.

- **Forgeable provenance (HIGH).** `dta_template_metadata_fields()`
  (`template_core.R:117`) derives the writable set from the S7 class, so
  `template` becomes author-writable the instant it is added.
  *Mitigation:* exclude it alongside `import_issues`, with a test asserting a
  template declaring `base.metadata.template` is rejected.

- **Git token leakage (HIGH).** A token in the clone URL persists into
  `.git/config`; on the command line it is visible in the process table; in an
  error message it reaches a Shiny notification and the Connect log.
  *Mitigation:* `GIT_ASKPASS` only, `GIT_TERMINAL_PROMPT=0`, and a
  `redact_secrets()` pass over all captured output — with a test asserting the
  token value appears in no returned string.

- **Cache directory not writable on Connect (HIGH).** `tools::R_user_dir()` is
  frequently unwritable for a Connect content account, which would make every
  git source fail on a deployment that works locally — the exact failure this
  feature is meant to avoid.
  *Mitigation:* probe by writing, fall back to `tempdir()`, and surface the
  chosen directory in the diagnostics panel.

- **Developer env var breaking the suite (MEDIUM).** Any machine with
  `DTATOOLS_TEMPLATE_SOURCES` exported flips the app into private-only mode and
  fails unrelated tests confusingly.
  *Mitigation:* `withr::local_envvar()` in every affected test, and a helper that
  clears all `DTATOOLS_TEMPLATE_*` vars by default.

- **`template_date` type mismatch (MEDIUM).** YAML parses a bare `2024-12-17`
  as a `Date`, but `DTADataSet@template_date` is `class_character_or_null`;
  stamping it unformatted trips the S7 validator at creation time.
  *Mitigation:* `format()` at the stamping site, with a test.

- **Scope (MEDIUM).** Fifteen tasks across the S7 class, a new provider layer,
  a new template kind, four party-reuse mechanisms and a three-way merge, in one
  release. This is a large, long-lived branch by construction.
  *Mitigation:* the task order above is a valid landing order — Tasks 1–3 are
  independently reviewable and testable, and Tasks 7–9 do not depend on Task 10, and Task 15 is standalone.
  If the branch runs long, that is the natural cut line. **Stated as a concern,
  not a recommendation to re-scope: one release was the explicit decision.**

### Elephants

- **Private templates become a second release process.** Once templates live in
  Bitbucket with their own versions, someone must own reviewing, versioning and
  tagging them. Task 15 supplies the technical half — an exported
  `validate_template()` and a copy-ready workflow file — but the human half
  (who reviews a template change, who decides a version bump, what a hotfix
  looks like when the DTS is already signed) is not solved by code and is not
  in this plan.

- **"Private replaces public" removes the demo for internal users.** The
  packaged `biomarker_gf` template is also the vignette's worked example and the
  onboarding path. Internal users will never see it. The
  `DTATOOLS_TEMPLATE_INCLUDE_BUILTIN` escape hatch covers demos and training, but
  it needs to be documented as such rather than discovered.

- **Rebase quality is bounded by `selections:`.** The three-way merge is only as
  good as the recorded option selections. Documents created before 0.25.0 have no
  provenance at all and can never be rebased — only manually compared. If a
  meaningful number of DTAs already exist in the wild, a one-off "adopt this
  document into template X@V" action would be needed, and is not in this plan.

---

## Out of Scope

- **Drift report UI.** The diff engine (Task 10) fully supports "how has this
  document diverged from its template", but a standalone drift panel was not
  among the selected mechanisms and is not built here. It is a small UI on top of
  `dta_diff()` whenever it is wanted.
- **Retro-fitting provenance onto pre-0.25.0 documents** (see the third elephant).
- **The private template repo's own CI job.** Task 15 exports the validator and
  ships a copy-ready workflow file; wiring it into the actual private repo, and
  the review/versioning policy around it, happen in that repo.
- **A template authoring/editing UI.** Templates are authored as YAML in an
  editor or a git repo; the app reads them.
- **Writing templates back to a private source from the app.** Read-only by
  design; the git source never pushes.
- **Non-git private VCS** (SVN, Perforce) and non-HTTPS git transports.
- **Per-user or per-group template visibility.** Every viewer of a deployed app
  instance sees the same template set.
