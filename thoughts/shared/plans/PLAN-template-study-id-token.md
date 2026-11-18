# Plan: reuse the Study ID inside templates (`${study.id}`)

Status: implemented (base `origin/dev` @ dbcefd7c), with `${study.protocol_id}`
added alongside `${study.id}` at Tom's request (item 5 of "Not included" below
is therefore done). Changes made after review, beyond the plan below:

- The study tokens also resolve in the document metadata (a title, a clause),
  in the `${version}` post-pass (`apply_template_expressions()`).
- Dry builds (validator, history, guide) stand in the study option's first
  `choices:` entry (read through `collect_option_effects()`, so `effects:` /
  `set:` options count), else `STUDY-ID` / `PROT-ID`
  (`dta_template_dry_study(def)`); a dry build failing on the stand-in says so.
  The standalone dataset-template column lint drops a column's token-built
  `values` / `examples` / `pattern` (judged in the creation template's dry run
  instead) and still judges the rest of the column.
- New validator warning `study_token_unanswered` (a token no option or base
  value can answer; use is read from the definitions, not the built
  document); `token_unknown` also covers `${version}` in a dataset.
- A `pattern:` key holding a named map (a rule condition on a column called
  `pattern`) is not treated as a regex.
- The agent-skill docs item below does not apply: there is no agent skill file
  in this tree.

## Goal

The Study ID typed once at creation (e.g. `1001_0190`) must flow into the
document's datasets:

- a file name pattern: `s1001_0190_YYYYMMDDHHMM.csv`
- a column constraint: `STUDYID` may only be `1001_0190`

## What a template author writes

Creation template — the study ID is an ordinary option (the starter skeleton
already has exactly this one):

```yaml
options:
  - id: study_id
    label: Study identifier
    type: text
    target: metadata.study.id
```

Dataset template — the token, anywhere in the dataset body:

```yaml
dataset:
  files:
    filename: ^s${study.id}_[0-9]{12}\.csv$
    pattern: true
    pattern_description: s${study.id}_YYYYMMDDHHMM.csv
  columns:
    - id: STUDYID
      type: SAS Char
      format: SAS $9.
      nullable: No
      values: ${study.id}
  rules:
    - id: gf_domain
      type: col_condition
      condition: { STUDYID: { equals: "${study.id}" } }
      then: { DOMAIN: { equals: GF } }
```

Creating with `1001_0190` yields `filename: ^s1001_0190_[0-9]{12}\.csv$`,
`pattern_description: s1001_0190_YYYYMMDDHHMM.csv`, `values: 1001_0190`.

## Behaviour (the contract)

1. `${study.id}` is the document's `metadata.study.id` **as settled at
   creation**. The precedence is the existing one: base, then carry-over, then
   party slot, then option. Nothing new decides it.
2. It resolves in every dataset body the template builds, which means both
   `template:` entries and inline entries. `path:`/`source:` entries are
   finished spec files and are not touched.
3. **Regex contexts get the escaped ID; everything else gets it raw.**
   - Escaped: the string value of any `pattern` key (a column `pattern:`, the
     rule `pattern` operator), and `filename` when its sibling `pattern` is
     `true`.
   - Raw: `values`, `example`, `equals`, `pattern_description`,
     `description`, labels.
   - Why: the study ID has no character-set restriction
     (`R/DTAMetaData-helpers.R:18-33`). An unescaped `1199.187` would let `.`
     match any character, which silently loosens the check. Authors can't
     forget an escape that happens automatically.
4. **Blank or absent study ID + token used → creation aborts** with a message
   naming the dataset. Substituting `""` would mean "STUDYID must be empty"
   and a filename starting `^s_`, which is wrong without any error.
5. The value is **copied in at creation**, the same as `${today}`/`${version}`.
6. A study ID that doesn't fit the column's format (e.g. 12 characters into
   `$9.`) is already rejected by the column validator
   (`R/DTAColumnSpec-class.R:300`). That is correct, and it surfaces as a
   creation error.

## Why this shape

- It reuses the existing `${…}` mechanism (`resolve_template_expressions()`,
  `template_core.R:806`) rather than adding an option→dataset target. The
  filename needs the ID **inserted into** a string, not swapped in for the
  whole value, so a token is needed regardless. One mechanism then covers
  both use cases.
- It must resolve on the **plain lists, before the S7 objects are built**. The
  `DTAColumnSpec` validator compiles `pattern:` and requires each `example:`
  to match it (`R/DTAColumnSpec-class.R:244-268`), so a pass over the finished
  S7 datasets fails before it runs.
- The study ID is only settled after the option step (step 5), while datasets
  are built in step 1. The metadata steps 2–5 never read datasets, so
  **reorder**: settle the metadata first, then build the datasets. The tests
  pin precedence among steps 2–5 (`test-shinyapp-template-create.R:781+`),
  not the position of the dataset step.

## Changes

### 1. `inst/shiny/dta_app/R/template_core.R`

- `dta_template_regex_escape(x)`: a one-line `gsub` over `.\|()[]{}^$*+?`.
  No such helper exists today.
- `resolve_dataset_tokens(ds, env, dataset_name)`: walks a plain dataset list.
  - In regex contexts (rule 3 above) it substitutes the escaped value;
    everywhere else the raw one.
  - Hitting `${study.id}` with no value is an error, raised with
    `cli::cli_abort()` with the token and the name passed as **values**
    (`tok <- "${study.id}"; "{.code {tok}}"`). Never pasted into the format
    string, because cli would try to evaluate `{study.id}` (see the cli
    format-string finding in the security baselines).
  - It also resolves `${today}`.
- `dta_template_dataset_env(md, fallback = NULL)`: returns `${today}` and
  `${study.id}`. The study ID is `md@study$id`, else `fallback`, else
  absent.
- `create_dta_from_template()`:
  - Run steps 2–5 onto `DTA(datasets = list())`. An empty DTA is legal
    (`R/DTA-class.R:100`).
  - Then build the datasets with the env from the settled metadata.
  - Then construct the final document with
    `DTA(datasets = ds_list, metadata = metadata(dta))`, so naming and
    container normalisation still run. Assigning to `@datasets` directly
    would skip both.
  - Rewrite the numbered-order comment.
  - New argument `study_id_fallback = NULL`, used only for token resolution
    and never written to metadata.
  - Legacy no-index inline branch: resolve `ref` before it is written to the
    temp YAML.

### 2. `inst/shiny/dta_app/R/template_create.R`

`build_template_datasets()` gains `token_env`. Resolve `ds_plain` just
before `dta_dataset_from_list()`:

- template branch: after `apply_vocabulary_slot_overrides()`, around L241;
- inline branch.

Resolving that late also covers values that a creation template's `patch:`
or a dataset option injects. `build_dataset_from_template()` itself is
untouched.

### 3. Callers that build without a real Study ID

All but one need the fallback, or they turn into false failures once
rule 4 aborts.

| Caller | Fallback |
| --- | --- |
| `app.R:2830` real creation | none, so it aborts when the ID is blank |
| `R/validateTemplate.R:827-885` dry run | a short placeholder, e.g. `"S0001"` |
| `template_history.R:57` version comparison | the same placeholder |
| `template_diff.R:720` rebase target, `:749` ancestor via `materialise_template()` | the **current document's** study ID |

- Rebase already re-derives option answers from the document's current
  metadata (`template_diff.R:340-378`). The fallback covers templates whose
  study ID comes from base or carry-over rather than an option.
- Without it, every STUDYID value and filename would show as a template
  change, or the rebase would abort.
- `carry_over` cannot carry the placeholder: an option with no `default:`
  writes NULL over it in step 5 (`dta_template_default()`,
  `template_core.R:509-523`).

### 4. Validator lint: `token_unknown` (warning)

- Nothing reports an unresolved `${…}` today, so a typo like `${studyid}`
  would reach a signed DTS. Scan the raw template text for `\$\{[^}]*\}`
  outside `{today, version, study.id}`.
- The current `inst/` YAML holds only `${today}`/`${version}` (checked).

### 5. Template guide

- `R/templateGuide-collect.R:185`: add `${study.id}` → "(the study ID you
  enter)".
- Check that the guide's dataset section routes values and filenames through
  that function.

### 6. Docs

- Skeleton `starter.dta-template.yaml:74`: the help text currently says
  "does not constrain any dataset column". Rewrite it.
- `template-repo-skeleton/README.md:274`.
- The agent skill under `inst/extdata/agent-skills/`.
- The vignette's template section, including the advice to use the token in
  `example:` too.
- `CHANGELOG.md` under Unreleased. It must also disclose that inline
  creation-template datasets now resolve `${today}`. They never did before,
  by accident rather than by contract (`template_create.R:189` comment).

### 7. Qualification

- Follow `inst/qualification/docs/maintaining.md`: a new REQ-TMPL entry plus
  OQ cases covering resolution, escaping and the abort, plus `token_unknown`.
- Re-run `sync_qualification_manifest.R`.
- Expected values are written by hand, never pasted from output.

## Tests (behaviour, not existence)

- A template built with study ID `1001_0190`: the built DTA's `STUDYID`
  values are `1001_0190`, the filename regex is `^s1001_0190_[0-9]{12}\.csv$`,
  and `pattern_description` is resolved.
- **A regex metacharacter in the ID** (`1199.187(a)`):
  - `matches_filename()` accepts `s1199.187(a)_202611181200.csv` and rejects
    `s1199x187(a)_202611181200.csv`;
  - a column `pattern:` and a rule `pattern` op both compile;
  - `values:` and `equals` hold the raw string.
- Blank ID + token → error whose class or cli-authored text names the
  dataset, and no document is produced.
- No token + no ID → unchanged output (regression).
- The ID set via base / carry-over / option resolves to the option's value
  (proves the reorder uses the settled metadata).
- An inline creation-template dataset resolves.
- `validate_template()` on a token template whose option has no default →
  no `instantiate_failed`; `${studyid}` → `token_unknown`.
- `template_history_build()` and the rebase of a token template both succeed,
  and STUDYID does not show as a change.

Verify with `devtools::test()`, the full `rcmdcheck` (NOT_CRAN), and
`run_qualification()`.

## Not included — needs your call

1. **Later edits do not propagate.** Changing the Study ID in the metadata
   editor (`dta_set_study_field()`) leaves datasets with the old value. A
   live binding (resolving at check/export time) is a much larger change.
2. **No "add a dataset from a template" flow** exists on a loaded document,
   so creation is the only entry point.
3. `path:`/`source:` dataset entries are not substituted.
4. The packaged example library stays as is: `gf_smrnaseq@3.0` still
   hard-codes `1234-5678`. A demo needs `gf_smrnaseq@3.1` plus a `study_id`
   option with a default in `biomarker_gf`, otherwise rule 4 breaks the
   packaged example.
5. `${study.protocol_id}`: one more env entry if wanted.
