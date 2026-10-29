# DTAtools template library

## What this repository is

This repository is a library of reusable YAML templates for creating Data Transfer Specifications (DTS) with DTAtools. Its layout is deliberately
simple: README and repository automation live at the root; the four kinds of template files live directly inside `templates/`.

This README is intended for both template authors maintaining this repository and people using the DTAtools app to create a document from it.
Authors edit and validate the YAML. App users choose a creation template, answer its options, and save the generated DTA. Dataset templates,
party profiles, and controlled vocabularies support the creation template behind that choice.

A library may contain four kinds of files:

- A **creation template** describes a new DTA, including base metadata, datasets, and choices offered to its author.
- A **dataset template** packages a reusable dataset definition, including columns, rules, and file handlers.
- A **party profile** packages affiliation and contact details for a supplier or receiver.
- A **controlled vocabulary** holds reusable coded terms that can populate permitted column values.

The DTAtools app consumes these files as a private template source. The library is not itself a running app and does not change the DTAtools data
validation rules.

### Quick start for authors

1. Clone this repository.
2. Edit or add template files directly in `templates/`.
3. Validate from the repository root:

   ```sh
   Rscript -e "DTAtools::validate_template('.', strict = TRUE)"
   ```

4. Open a pull request and ask another author to review the change.
5. After merge, the configured DTAtools app source can load the updated library. A Git source is refreshed according to its configured interval.

Do not treat a successful YAML parse as proof that the template works. The validator also resolves references and dry-runs concrete creation
templates.

## Folder layout

Use this structure for a template library:

```text
repository/
├── README.md
├── templates/
│   ├── starter.dta-template.yaml
│   ├── starter_dataset.dta-dataset-template.yaml
│   ├── starter_supplier.dta-party.yaml
│   └── starter_terms.dta-vocabulary.yaml
├── .github/
│   └── workflows/
│       └── validate-templates.yml
├── bitbucket-pipelines.yml
└── Jenkinsfile
```

Only template-library files and files referenced by their `source:` entries belong in `templates/`. Keep README, CI files, scripts, tests, Docker
files, charts, and deployment configuration at the repository root or in their own repository folders.

### Keep the library flat

Every recognized template file must be directly inside `templates/`. The loader scans that directory non-recursively; it does not search
arbitrary subfolders. A file under `templates/by-study/` or `templates/archive/` is not part of the active catalogue.

The validator reports a template-shaped file in a subfolder as `template_in_subdirectory` with warning severity. The warning is a reminder that
the app will not load it, not evidence that recursive lookup occurred. Move an active file to the top level of `templates/`.

Do not leave a template-shaped file at the repository root. Once the conventional `templates/` directory contains template files, it is the
library directory and root-level template files are not merged into it. The validator reports root-level leftovers as
`template_outside_library_dir` with error severity. This catches migration leftovers that would otherwise be invisible to the app.

Supporting files named by a dataset entry's `source:` must be stored inside `templates/` as well. Keep the path relative to the template that
refers to it. The resolver prevents a template from using `..` or an absolute path to read arbitrary files outside its own folder.

A `chart/templates/` directory is not the library directory. The convention is the root-level `templates/` folder; other application or chart
folders are irrelevant to template discovery.

### How the directory is selected

`validate_template('.')` and the app's template sources recognize the conventional `templates/` folder when it contains at least one recognized
template file at its top level. If it does not, the root-layout fallback keeps older libraries working. A source may also point directly at a
template folder; in that case the folder itself is scanned.

DTAtools version support matters. The first release after 0.25.0 recognizes the repository-root `templates/` convention automatically. Earlier
releases expect template files at the root of the configured directory. For those releases, configure a `dir:` source pointing directly at
`<checkout>/templates` rather than at the repository root.

## File naming conventions

The suffix identifies the file kind during directory scanning. The `kind:` field inside the YAML must agree with the suffix.

| File kind | Required suffix | Example |
| --- | --- | --- |
| `dta_creation_template` | `.dta-template.yaml` or `.dta-template.yml` | `starter.dta-template.yaml` |
| `dta_dataset_template` | `.dta-dataset-template.yaml` or `.dta-dataset-template.yml` | `starter_dataset.dta-dataset-template.yaml` |
| `dta_party_profile` | `.dta-party.yaml` or `.dta-party.yml` | `starter_supplier.dta-party.yaml` |
| `dta_vocabulary` | `.dta-vocabulary.yaml` or `.dta-vocabulary.yml` | `starter_terms.dta-vocabulary.yaml` |

Use one template per file. Name the file `<id>.<kind suffix>` so a reviewer can match it to its YAML `id`. The filename is not the identifier:
references inside YAML use the `id` and optional version, not the filename.

Use lowercase `snake_case` identifiers for new files. IDs are the stable names used by references, so avoid changing a released ID just to
improve its spelling. Rename a file without changing the `id` only when there is a clear repository reason.

The suffix match is case-insensitive, and `.yml` is accepted as well as `.yaml`. Keep the kind and suffix consistent even if a file could
otherwise be parsed: a mismatch makes the directory convention misleading and may keep the file out of the expected index.

## Common header keys

Each file begins with a YAML mapping. Put the kind, identity, version, and human-readable label near the top so reviewers can understand the file
before reading its body.

A typical header is:

```yaml
kind: dta_creation_template
id: starter
label: Starter DTS
version: "1.0"
description: A short explanation of what this template creates.
```

### Identity and version fields

- `kind` selects the template kind and must match the filename suffix.
- `id` is the stable reference name. Use lowercase `snake_case` by convention.
- `version` is a string. Always quote it, for example `version: "1.0"`.
- `label` is the display name. If omitted, the ID is used as the label.
- `description` explains the template and may be omitted.
- `date` may describe the template's authored or revised date; it is not the document creation date placeholder.

Versions are parsed and compared as `numeric_version` values when choosing a latest version. An unquoted YAML number can lose its original
spelling before DTAtools sees it: `1.10` can be read as `1.1`, and `1.0` can become `1`. Quoting preserves the exact version text and lets the
validator detect unquoted versions.

DTAtools identifies a particular file by the combination of kind, `id`, and `version`. Each such combination should occur only once in the active
library. A different kind may use the same ID, but use distinct IDs across kinds when practical to make references and review easier.

### Kind-specific header fields

Creation templates may use:

- `extends` to inherit from another creation template.
- `abstract: true` to make a creation template available only as a parent, not as an item that can be instantiated directly.
- `sealed` to prevent descendants from changing selected template paths.
- `required` to require selected paths to be filled before a document is created.

Vocabularies may use `extends` to build on another vocabulary, with the vocabulary-specific `add_terms` and `remove_terms` operations described
below. Do not confuse that vocabulary extension with the creation-template merge rules.

Party profiles use `role`, whose supported values are `supplier`, `receiver`, and `any`. Other file kinds do not use `role` to select a party
profile.

A template's `metadata.template` field is not an author-settable header field. The app writes this machine-owned provenance onto a created
document.
## Creation templates

A creation template (`kind: dta_creation_template`) describes a whole new DTA. Its `base:` supplies initial document data; `options:` describes
inputs the author can choose; and `datasets:` supplies datasets to build into the new document.

The starter creation template has the following shape:

```yaml
kind: dta_creation_template
id: starter
label: Starter DTS
version: "1.0"
description: A minimal example for a new template family.

base:
  metadata:
    title: Starter Data Transfer Specification
    version: "1.0"
    date: "${today}"
    header: Starter Organisation

datasets:
  - template: starter_dataset@1.0
    as: starter_data

party_slots:
  - id: supplier
    label: Supplier
    target: metadata.supplier
    profiles: [starter_supplier]
```

A non-inheriting creation template needs a `base:` mapping and at least one dataset entry. A child template that has `extends:` may omit either
section to inherit it. Do not write an empty replacement section unless the intent is to replace the inherited section with an empty one.

### `base:` metadata

`base.metadata` contains defaults for `DTAMetaData` fields. The supported top-level metadata fields are:

- `title`
- `version`
- `date`
- `header`
- `version_history`
- `receiver`
- `supplier`
- `transmission`
- `error_handling`
- `authorized_for_corrections`

Nested mapping values such as `receiver`, `supplier`, and `transmission` can contain their own fields. Use the same structure as metadata in an
ordinary DTA definition. The template engine checks the top-level names; a misspelled metadata name is not a new extension point.

The machine-owned fields `template` and `import_issues` cannot be set by a template. The validator reports a template attempting to target either
with `target_machine_owned` when the field is reached through an option target.

The app resolves these expression tokens in template data:

- `${today}` becomes the date the document is created, formatted as an ISO date.
- `${version}` becomes the selected document metadata version after options have been applied.

For example, the first history entry can agree with the version the author chose:

```yaml
base:
  metadata:
    version: "1.0"
    date: "${today}"
    version_history:
      - version: "${version}"
        date: "${today}"
        changes: Initial template-generated version
```

`${today}` and `${version}` are not general-purpose environment-variable substitution. Do not invent other `${...}` tokens and expect the app to
resolve them.

### `options:`

Each creation-template option is a mapping with an `id` and optional display and behavior fields. A direct `target:` is shorthand for writing the
chosen value to one path. For example:

```yaml
options:
  - id: header
    label: Header / organisation
    type: text
    target: metadata.header
    help: Organisation shown in the document header.
    default: Starter Organisation
```

The app supports these input types:

| Type | Input behavior |
| --- | --- |
| `text` | Single-line text control; with `choices`, a choice box that can accept custom text by default. |
| `select` | Choice-oriented control; custom entries are disabled by default. Supply `choices` for a bounded set. |
| `boolean` | Checkbox; its selected value is a logical. |
| `textarea` | Multi-line text control. |
| `number` | Numeric input. |

If `type` is omitted, the option is treated as `text`. `choices` may be a list of values or a list of `{value, label}` mappings. A `default`
outside the supplied choices is retained and displayed as the selected value.

These option fields are supported:

- `id` identifies the option and is used as the selection key.
- `label` is the visible prompt; if absent, the ID is used.
- `type` selects the input behavior shown above.
- `choices` supplies suggested or selectable values.
- `default` preselects a value. When a direct metadata `target` has no explicit default, the value already in `base.metadata` can supply it.
- `help` displays explanatory text beneath the control.
- `allow_custom` controls whether a choice box accepts typed values. The default is true for `text` and false for `select`.
- `target` names the field that receives the value, such as `metadata.title` or `metadata.header`.
- `effects` maps selected values to one or more `{path, value}` operations.
- `effects_all` adds operations that apply regardless of the selected value.

For a simple one-field update, use `target:` instead of writing an `effects:` block. When `effects:` is present, it takes precedence over
`target:`. Each operation uses `path` and `value`:

```yaml
options:
  - id: blinded
    label: Is the transfer blinded?
    type: boolean
    effects:
      "true":
        - path: metadata.transmission.blinded_transfer
          value: true
      "false":
        - path: metadata.transmission.blinded_transfer
          value: false
```

An `effects:` mapping can also use `__selection__` for a value-independent selected value. Boolean YAML keys such as `yes` and `no` are easy to
have coerced by YAML 1.1; quoted string keys or `true`/`false` values avoid surprises. `effects_all:` is a list of operations with the same
`path` and `value` fields.

A target rooted at `metadata.` must resolve to a real, non-machine-owned metadata field. Unknown fields produce `target_invalid` as a warning;
machine-owned fields produce `target_machine_owned` as an error. Conditional effects must use paths the app can set.

There is no option-level `required`, `order`, or `min` control in the creation-option input model. `required` is a creation-template path
requirement, `order` belongs to collection inheritance, and `min` belongs to a vocabulary slot. Do not add these to an option expecting them to
have those meanings.

### `datasets:` and dataset entries

A creation template may use inline dataset definitions, dataset-template references, or source-based entries. A dataset-template reference is
the reusable-library form:

```yaml
datasets:
  - template: starter_dataset@1.0
    as: starter_data
```

- `template` names a dataset template by ID, optionally pinned to a version.
- `as` supplies the name for the dataset in the created DTA.
- `options` supplies values for the dataset template's options; these are static values for that dataset build, not additional creation-dialog
  controls.
- `patch` modifies the dataset template's result.
- `source` names a finished dataset YAML relative to the creation-template file. Keep such supporting files inside `templates/` and inside the
  template's own directory.

A bare string or `{source: ...}` reads a finished dataset YAML. An inline mapping contains the dataset definition itself. In a private library,
prefer `template:` when sharing a reusable dataset schema and use `source:` only when a ready-made dataset specification is actually the intended
input.

### `party_slots:`

A party slot lets the document's author choose a party profile for a metadata party field. Only `metadata.supplier` and `metadata.receiver` are
valid slot targets.

```yaml
party_slots:
  - id: supplier
    label: Supplier
    target: metadata.supplier
    profiles: [starter_supplier]
```

- `id` is the slot's selection key.
- `label` is the prompt shown in the app.
- `target` must be `metadata.supplier` or `metadata.receiver`.
- `role` may further specify `supplier`, `receiver`, or `any`; if omitted, the role is inferred from the target.
- `profiles` is an optional list of profile IDs. If omitted, every available profile with a matching role may be offered.
- `required` may be `true` to require a profile choice before creating the document.

A selected profile replaces the target party block rather than merging contacts from two different profiles. If a slot is required but no
eligible profile is available, the author cannot complete the intended choice; keep its role and allow-list consistent with the profiles in the
library.

### `vocabulary_slots:`

A vocabulary slot lets the document author choose the values that apply to a particular dataset column. It is defined on a creation template,
because the creation dialog presents its selections there.

```yaml
vocabulary_slots:
  - id: status_terms
    label: Status values used in this transfer
    target: datasets.starter_data.columns.STATUS.values
    vocabulary: starter_terms@1.0
    mode: closed
    include: [PASS, FAIL]
    default: [PASS]
    min: 1
```

The supported target form is `datasets.<dataset name>.columns.<column id>.values`. The `dataset` and column names must refer to objects the
template actually builds.

`mode` is `closed` or `open`. `closed` only offers vocabulary terms; `open` also permits the author to type additional terms. The final document
still receives the selected values as its permitted-value list. `include` and `exclude` narrow the terms offered by the vocabulary, and
`default` selects initial terms. `min` is a non-negative whole number that sets a minimum selection count; it is not a numeric-input lower bound.

`field` may be `code` or `label` and selects which vocabulary field populates the column values. The default is `code`. A vocabulary slot must
name an existing vocabulary and a valid target. Invalid slot definitions produce `vocab_slot_invalid`; an unresolved vocabulary produces
`vocab_slot_unresolved`.
## Dataset templates

A dataset template (`kind: dta_dataset_template`) describes one reusable dataset, which a creation template can import or an app user can add to
an existing DTA.

The body key is singular: `dataset:`. It contains the dataset definition accepted by the package's dataset reader. A top-level `datasets:` list
is not an equivalent spelling; it belongs to a creation template. A dataset template without the `dataset:` body is reported as
`dataset_missing`.

```yaml
kind: dta_dataset_template
id: starter_dataset
label: Starter dataset
version: "1.0"
description: A small reusable tabular dataset.

dataset:
  name: starter_data
  type: tabular
  description: Example dataset for a new template library.
  files:
    filename: starter_data.tsv
    type: tsv
  columns:
    - id: SUBJID
      label: Subject identifier
      type: SAS Char
      format: SAS $10.
      nullable: No
      description: Unique identifier for a study subject.
      examples: [SUBJ0001]
```

Supported dataset `type` values are `tabular` and `file`. A tabular dataset contains column specifications and may contain `rules`. A file
dataset specifies deliverable files rather than a tabular column schema.

### Column fields

A column specification is passed into `DTAColumnSpec`. Supported fields are:

| Field | Purpose |
| --- | --- |
| `id` | Required column identifier; it must not contain whitespace. |
| `label` | Human-readable column label. |
| `type` | Data type, including the supported SAS-prefixed types below. |
| `format` | Format string, such as a SAS format. |
| `length` | Maximum character length where applicable. |
| `nullable` | Whether an empty or missing value is permitted. |
| `values` | Explicit permitted values. |
| `values_from` | A controlled-vocabulary binding expanded to `values` when built. |
| `pattern` | Regular expression that values must match. |
| `examples` | Example value or values displayed with the specification. |
| `description` | Explanation of the column. |
| `colclass` | Supported R/SAS storage-class hint. |

Use `examples` (plural), matching the column-specification field. Do not use `example` as a synonym. `values` and `pattern` are mutually
exclusive. When a column uses `values_from`, do not also set `pattern`; that is an error. A literal `values` list combined with `values_from` is
a warning, and the vocabulary binding wins.

The supported SAS type names are:

- `SAS Char`
- `SAS Num`
- `SAS Int`
- `SAS Date`
- `SAS Time`
- `SAS DateTime`

Use the `SAS` prefix where the type is a SAS type. A SAS format may follow the same backend prefix convention, for example `format: SAS $10.` for
a character column. The package's column-structure factory supports the SAS backend; do not assume an unlisted backend is available.

Example of a column bound to a vocabulary:

```yaml
- id: STATUS
  label: Record status
  type: SAS Char
  format: SAS $4.
  nullable: No
  description: Quality status of the record.
  values_from: starter_terms@1.0
```

### File handlers and non-tabular deliverables

Tabular dataset `files:` describes the expected file or files. A single handler can be a mapping; multiple handlers are represented as a list of
mappings. The supported handler types are `csv`, `tsv`, and `any`.

```yaml
files:
  filename: starter_data.tsv
  type: tsv
```

A non-tabular deliverable is represented by a dataset with `type: file` and one or more file handlers. `any` is the general file handler; it can
also restrict accepted file endings when the handler supports `extensions`. The handler's `filename` may be a pattern when `pattern: true` is
used.

```yaml
dataset:
  name: supporting_documents
  type: file
  files:
    - type: any
      filename: "report.*"
      pattern: true
      extensions: [pdf, zip]
```

File handlers describe expected deliverables; they are not extra template kinds. Use the file type values recognized by the package rather than
inventing `xlsx`, `sas7bdat`, or other handler names.

### Dataset-template options

A dataset template may define `options:` that write to a path rooted at `dataset.`. For example:

```yaml
options:
  - id: vendor_name
    label: Vendor name
    type: text
    target: dataset.columns.SOURCE.values
    default: Starter Vendor
    help: Name of the organisation supplying this dataset.
```

Dataset paths address columns and rules by their `id`, not by list position. The specific path grammar is implemented by the dataset-template
engine; validate the target rather than assuming any dotted string is valid. Dataset-template options are supplied by the importing creation template entry's `options:` mapping, or by the dataset-template caller. They are not prompted as creation-template options merely because they
exist in the imported file.

### Dataset rules

Rules live under the dataset's `rules:` list. Each rule has an `id` and a `type`. The supported rule types are `col_condition`, `col_range`,
`col_unique`, and `group_condition`.

A column condition checks same-row conditions and consequences. Column names map to operator mappings. Supported operators include `equals`,
`not_equals`, `empty`, `in`, `not_in`, `pattern`, `greater`, `greater_equal`, `less`, `less_equal`, `min`, and `max`.

```yaml
rules:
  - id: consent_requires_date
    type: col_condition
    condition:
      CONSENT:
        equals: "YES"
    then:
      CONSENT_DATE:
        empty: false
```

A range rule constrains one or more columns to a numeric minimum and maximum. Use `columns`, not `column`:

```yaml
rules:
  - id: age_range
    type: col_range
    columns: [AGE]
    min: 18
    max: 65
```

A uniqueness rule checks that the combination of the listed columns is unique. One column gives single-column uniqueness; multiple columns give a
composite key.

```yaml
rules:
  - id: subject_visit_unique
    type: col_unique
    columns: [SUBJECT_ID, VISIT]
```

A group condition first groups rows by `group_by`, evaluates named conditions, then applies constraints across the rows in each group. A
`requires` constraint may also be written as `implies`; a `mutually_exclusive` constraint may also be written as `not_both`. The scope fields
accept `any` (the default) or `all`.

```yaml
rules:
  - id: consent_requires_date_per_subject
    type: group_condition
    group_by: [SUBJECT_ID]
    conditions:
      consent_given:
        CONSENT:
          equals: "YES"
      date_present:
        CONSENT_DATE:
          empty: false
    constraints:
      - id: consent_needs_date
        type: requires
        if: consent_given
        then: date_present
        if_scope: any
        then_scope: all
```

The rule keys above follow the package's YAML reader and rule constructors. Use the validator to catch malformed rules when a concrete creation
template is dry-run. A rule set can be valid YAML and still refer to the wrong column or express an unintended condition.

## Patches

A dataset entry may import a dataset template and apply `patch:` to its dataset body. Patches are useful for a supplier-specific deviation that
changes only a few columns instead of copying a complete dataset definition.

The longhand column operations are:

- `remove_columns`: a list of existing column IDs to delete.
- `add_columns`: a list of new column specifications. Each must have an ID not already present.
- `modify_columns`: a list of existing column IDs plus fields to change. Omitted fields stay as they were; `id` is not renamed.
- `set`: a mapping of dataset-relative dotted paths to values.

The operations are applied in this order: remove columns, add columns, modify columns, then apply `set` paths. This order lets a later operation
address the final list after removals and additions. A request to remove or modify an unknown ID, or add an already-used ID, fails rather than
silently changing nothing.

```yaml
datasets:
  - template: starter_dataset@1.0
    as: starter_data
    patch:
      modify_columns:
        - id: SOURCE
          description: Name of the partner supplying this record.
      add_columns:
        - id: SITEID
          label: Site identifier
          type: SAS Char
          format: SAS $8.
          nullable: No
      remove_columns: [LEGACY]
      set:
        description: Starter dataset with site identifiers.
```

`set` paths are relative to the dataset body. Examples include `description`, `files.filename`, and `columns.SOURCE.values`. Column and rule
paths are addressed by ID. Do not use a positional index in a patch path.

The generic collection form is also accepted under `patch.columns:`. It supports the collection verbs `inherit`, `remove`, `add`, `modify`, and
`order`:

```yaml
patch:
  columns:
    inherit: all
    remove: [LEGACY]
    add:
      - id: SITEID
        label: Site identifier
        type: SAS Char
        nullable: No
    modify:
      - id: SOURCE
        description: Name of the partner supplying this record.
    order: [SITEID, SOURCE]
```

`inherit` may be `all`, `none`, or a list of IDs to keep. `remove` names IDs; `add` introduces entries; `modify` changes existing entries;
`order` lists known IDs in the requested order. Unknown or incoherent operations are reported as `patch_incoherent` by the validator. Use one
style for a given change so the resulting intent is clear in review.

A patch is applied to the dataset template's `dataset:` body before the DTA is built. The imported dataset's rules and file handlers remain part
of that body unless a patch explicitly changes a supported path.

## Controlled vocabularies

A controlled vocabulary (`kind: dta_vocabulary`) stores coded terms once so multiple columns or templates can reuse the same authoritative list.

```yaml
kind: dta_vocabulary
id: starter_terms
label: Starter status terms
version: "1.0"
type: text
terms:
  - code: PASS
    label: Pass
    description: The record passed all quality checks.
  - code: FAIL
    label: Fail
    description: The record failed at least one quality check.
```

A term may be a mapping with `code`, `label`, and `description`; a bare scalar is shorthand for a term with only a code. The vocabulary `type` is
`text`, `number`, or `integer`; it defaults to `text`. Numeric vocabulary codes are converted according to that type. Quote codes when they are
text, especially codes with leading zeros or values such as `Y` and `N` that YAML might otherwise interpret as numbers or booleans.

### Binding a column with `values_from:`

A column can bind to a vocabulary by a shorthand reference:

```yaml
values_from: starter_terms@1.0
```

The full mapping form can choose the term field and narrow the offered terms:

```yaml
values_from:
  vocabulary: starter_terms@1.0
  field: code
  include: [PASS, FAIL]
  exclude: []
```

- `vocabulary` is an `id`, `id@version`, or `id@latest` reference.
- `field` is `code` or `label`, defaulting to `code`.
- `include` keeps the listed vocabulary codes.
- `exclude` removes the listed vocabulary codes.

An include or exclude code absent from the vocabulary is invalid. The binding is expanded into an ordinary `values:` list when the dataset is
built; the finished DTA does not depend on access to the template library.

A column must not combine `values_from` with `pattern`. The validator reports `values_from_pattern` as an error. A column that specifies both a
literal `values` list and `values_from` produces `values_and_values_from` as a warning; the binding wins and the literal list is ignored.

### Vocabulary inheritance

Vocabulary inheritance uses its own term operations:

```yaml
kind: dta_vocabulary
id: starter_terms_extended
version: "1.1"
extends: starter_terms@1.0
remove_terms: [FAIL]
add_terms:
  - code: REVIEW
    label: Review required
    description: The record requires manual review.
```

Resolution starts with the parent's terms, removes the codes in `remove_terms`, and applies `add_terms`. Adding a term whose code already exists
replaces it in place. If a child also supplies its own non-empty `terms:` list, that list replaces the inherited result. The generic creation
template collection-merge rules do not replace these vocabulary-specific operations.

## Party profiles

A party profile (`kind: dta_party_profile`) stores a reusable affiliation and its contacts. Profiles may serve supplier or receiver slots.

```yaml
kind: dta_party_profile
id: starter_supplier
version: "1.0"
role: supplier
label: Starter Supplier
description: Supplier contact block for the starter family.
affiliation:
  name: Starter Supplier Inc.
  address: 1 Example Street, Springfield
  country: DE
contacts:
  - name: Jane Doe
    role: Data Manager
    email: jane.doe@example.com
    signature: true
```

`role` must be `supplier`, `receiver`, or `any`. `affiliation` is a mapping written into the selected party block. `contacts` is a list of named
contact mappings. The example uses fields supported by the metadata contact structure; keep actual contact data accurate and approved for the
repository's audience.

A creation template offers profiles with a `party_slots:` entry. The slot's `target` determines the default matching role: supplier targets offer
supplier profiles, and receiver targets offer receiver profiles. A profile with role `any` can match either. An optional `profiles` list further
limits the choices to named profile IDs.

Do not put real personal information in a public repository. Private contact data belongs only in a repository with appropriate access controls,
and the profile should contain only information needed in the generated DTA.
## Inheritance, deviation templates and sealing

A creation template can extend another creation template with `extends:`. This supports a shared standard plus small supplier-, study-, or
region-specific deviations.

```yaml
kind: dta_creation_template
id: starter_supplier_deviation
label: Starter DTS for a supplier
version: "1.0"
extends: starter@1.0

options:
  - id: header
    default: Example Supplier
```

The child keeps its own `id` and `version`; inherited sections can be omitted. For example, omitting `base:` inherits the parent's base. A child
may override selected mapping fields without copying the entire parent definition.

### Values, collections, and empty states

When reading a child template, distinguish an absent key from an explicit empty value:

| Child YAML | Merge meaning |
| --- | --- |
| Key omitted | Inherit the parent's value. |
| Scalar or mapping value | Override the value; mappings merge recursively. |
| Empty scalar, `{}`, or `[]` | Explicitly empty value; do not treat as omission. |
| `key: null` or `key: ~` | Remove the inherited key. |

Sequences are replaced as a whole; their entries are not merged by list position. This avoids combining unrelated column definitions when a
child's list differs in length from its parent's.

Several keyed collections also support explicit verbs. These include `options:`, `datasets:`, `party_slots:`, `vocabulary_slots:`, and a dataset
patch's `columns:` collection. Use `inherit`, `remove`, `add`, `modify`, and `order` where appropriate. A bare list remains supported: entries
with the same identity modify inherited entries, new identities add entries, and `remove: true` on an entry removes it.

For dataset collections, identity is resolved from the dataset entry's available identifying fields; for options and slots it is based on `id`.
Prefer explicit `add` and `modify` verbs when the distinction matters, so a typo does not silently create a new entry instead of changing an
inherited one.

A collection-verb example:

```yaml
options:
  inherit: all
  modify:
    - id: header
      default: Example Supplier
  remove: [unused_choice]
```

The verbs are applied in a defined sequence: choose inherited entries, remove entries, add entries, modify entries, then apply requested order.
Unknown IDs in an explicit `add` or `modify` operation are reported rather than being guessed.

### `abstract`, `sealed`, and `required`

An abstract creation template is a reusable base that cannot be selected directly for document creation. Concrete descendants inherit its shared
metadata, datasets, and options.

`sealed` lists paths a descendant may not change. Paths use template structures, for example:

```yaml
sealed:
  - base.metadata.title
  - options.header
```

A seal on a parent constrains descendants, not the parent itself. An unknown sealed path protects nothing and is reported as
`sealed_path_unknown` with warning severity. A descendant that changes a sealed value is reported as `sealed_violation`; the validator also
reports `instantiate_failed` for a concrete template that cannot be built.

`required` lists paths that must have a value when the resulting document is created. For example:

```yaml
required:
  - base.metadata.supplier
```

A path may be satisfied by the template's base, an inherited or selected party profile, or an author choice, depending on what the path names.
This is not an option-level required flag. A party slot's own `required: true` makes that slot selection mandatory; the top-level creation template `required` paths express a separate path-filling requirement.

Creation-template inheritance is limited to a maximum chain depth. Cycles and unresolved references cannot produce a concrete document; the
validator reports `extends_cycle` or `extends_unresolved`, and a concrete template also receives `instantiate_failed`.

## Versioning

Treat a published template version as immutable. To change a released contract, add a new file version and update dependent references
deliberately instead of editing the old version in place.

Pin a reference with `id@version` when a parent or dataset must remain stable:

```yaml
extends: starter@1.0
```

A bare ID and `id@latest` resolve to the highest parseable `numeric_version` for that ID. A pinned `id@version` selects that exact version.
Prefer pinned references for production libraries so a new template release does not silently change the meaning of an existing parent.

Use quoted, semver-like strings such as `"1.0"`, `"1.1"`, or `"2.0"`. DTAtools compares versions numerically, not lexicographically. Quote every
version in every file kind, including dataset templates, party profiles, and vocabularies.

The validator reports `duplicate_id_version` when it finds multiple files with the same kind, ID, and version. Keep one authoritative file for
each combination.

A new vocabulary version affects future documents that resolve to it. It does not rewrite the values already embedded in a finished DTA.

## Provenance and upgrading documents

When a document is built from a creation template, the app records a `metadata.template` provenance record. It can include the template ID and
version, source information, resolved Git commit, file path, a hash of the resolved definition, creation date, inheritance lineage, the author's
selections, and dataset-template provenance.

`metadata.template` is machine-owned. Template authors must not set it in `base.metadata` or through an option effect. The app relies on its
provenance to identify the template that produced a document.

DTAtools supports rebasing a document created from one version of a template to another version while preserving independent metadata edits. It
compares the original template ancestor, the current document, and the target template; conflicting metadata changes require an explicit
resolution.

Rebase is limited to metadata. It does not update dataset columns, rules, or files. Dataset differences can be shown for review, but rebase does
not apply them. `version_history` is not rewritten to pretend the new template created the document; the rebase appends a history entry
describing the rebase.

A document without template provenance cannot be rebased, because DTAtools cannot reliably reconstruct the ancestor template or the selections
that created it. Publishing a new template or vocabulary version also does not modify already-created DTA documents automatically.

## Connecting the DTAtools app

Configure template sources in the environment of the process that runs the DTAtools app. `DTATOOLS_TEMPLATE_SOURCES` accepts one or more
semicolon-separated entries in this grammar:

```text
[name=]scheme:locator[#ref]; [name=]scheme:locator[#ref]
```

The optional name labels the source. Supported schemes are `dir:`, `pkg:`, and `git:`. Each source root uses the conventional `templates/`
directory automatically when present and populated with template files. A `dir:` value may instead point directly at the template directory.

### Source examples

A local directory source:

```text
DTATOOLS_TEMPLATE_SOURCES=site=dir:/srv/dta-library
```

The directory must exist and be readable. With this new layout, a repository root is detected and its `templates/` child is used.

A companion R package source:

```text
DTATOOLS_TEMPLATE_SOURCES=site=pkg:company.dta.templates
```

The package must be installed and expose its template files in `inst/dta-templates`. The conventional nested `templates/` folder may hold the
templates within that source root.

A Git source:

```text
DTATOOLS_TEMPLATE_SOURCES=company=git:https://example.org/group/dta-library.git#main
```

The optional `#ref` selects a branch or tag; if omitted, Git uses `HEAD`. The template library itself should have the repository-root
`templates/` folder. The locator is an HTTPS Git URL; do not put credentials in the URL.

### Environment variables

| Variable | Default or unset behavior | Purpose |
| --- | --- | --- |
| `DTATOOLS_TEMPLATE_SOURCES` | Empty; no private source configured. | Source list using `[name=]scheme:locator[#ref]`, with semicolon separators. |
| `DTATOOLS_TEMPLATE_INCLUDE_BUILTIN` | Effectively false after a private source is configured; effectively true when none is configured. | Set to `1`, `true`, or `yes` to keep packaged/legacy template roots alongside configured private sources. |
| `DTATOOLS_TEMPLATE_REFRESH_SECONDS` | `900` seconds. | Git checkout and index refresh interval. A non-positive or unparseable value warns and falls back to 900. |
| `DTATOOLS_TEMPLATE_CACHE_DIR` | Unset; defaults to a per-user DTAtools cache, then a writable directory under `tempdir()` if necessary. | Location used for Git clones. |
| `DTATOOLS_TEMPLATE_GIT_TOKEN` | Unset; anonymous Git access. | Credential passed to the Git child process for private Git sources. |
| `DTATOOLS_TEMPLATE_GIT_USER` | `git`. | Basic-auth username sent with the token when auth mode is `basic`. |
| `DTATOOLS_TEMPLATE_GIT_AUTH` | `basic`. | Token authentication mode: `basic` or `bearer`. |

Set the token through the deployment platform's secret mechanism. Do not commit it in this repository or include it in a source URL.

When a private source is configured, it replaces the packaged example sources by default. Set `DTATOOLS_TEMPLATE_INCLUDE_BUILTIN=1` if both
private and packaged/legacy templates should be available. A failing private source is a hard failure for the configured catalogue; it is not silently replaced by the built-in examples. If a Git
refresh fails and a last-good checkout exists, DTAtools continues to serve that stale checkout and reports a stale-source warning. If a Git
source has never resolved and has no cache, that source offers no templates.

For example, a Posit Connect deployment can set:

```text
DTATOOLS_TEMPLATE_SOURCES=company=git:https://example.org/group/dta-library.git#main
DTATOOLS_TEMPLATE_GIT_TOKEN=<deployment-managed-secret>
DTATOOLS_TEMPLATE_GIT_AUTH=bearer
```

The authentication mode and token format must match the Git host's requirements. The default user is `git`; it is used with basic auth. Do not
print or log the token while troubleshooting.

### Local alternatives

During local development, the app also checks `options(DTAtools.template_dir)` and a `./dta-templates/` directory, before the packaged template
directory. These are useful for a developer's local copy and do not replace the repository's canonical `templates/` location.

To test the source resolution without changing deployment settings, configure a local `dir:` source for the clone and run the app with that
environment value. Remember that earlier DTAtools releases need the `dir:` locator to point directly at `<checkout>/templates`.
## Validating

Run validation from the repository root after changing any template file:

```sh
Rscript -e "DTAtools::validate_template('.', strict = TRUE)"
```

The directory form scans its selected library directory, checks recognized files and cross-file references, and dry-runs each concrete creation
template. The scan is non-recursive. Calling `validate_template('templates', strict = TRUE)` explicitly is also useful when testing a folder
directly.

For an interactive report rather than a strict failure, omit `strict = TRUE`:

```r
issues <- DTAtools::validate_template(".")
issues
```

The result is a data frame with `file`, `kind`, `id`, `version`, `severity`, `code`, and `message` columns. A zero-row result means no validator
issues were found. `kinds` can limit which file kinds are included in per-kind checks; it does not turn an empty directory into a successful
validation.

Strict mode aborts when one or more rows have severity `error`. Warnings remain visible in the report but do not alone make strict validation
fail. The CI examples shipped with DTAtools use strict validation so a structurally broken library cannot merge unnoticed.

### CI

Use the GitHub Actions, Bitbucket Pipelines, or Jenkins validation file that matches the repository host. Each runs the same DTAtools validator;
keep its working directory at the repository root so `validate_template('.')` selects `templates/`.

`create_template_repo()` can scaffold validation CI for supported providers. If the repository uses more than one CI system, keep each
configuration focused on the same validation command and avoid maintaining different validation rules per provider.

### Validator codes

This table lists every code emitted by `validate_template()` in the current validator, including the new-library layout errors. Severity is the
literal severity reported in the result. A concrete template can receive `instantiate_failed` in addition to a more specific inheritance code.

| Code | Severity | Meaning | Typical fix |
| --- | --- | --- | --- |
| `parse_failed` | error | The template file cannot be parsed or read as YAML. | Fix indentation, quoting, or invalid YAML syntax. |
| `kind_unknown` | warning | The `kind` is not one of the four recognized kinds. | Use the correct kind value and matching filename suffix. |
| `id_missing` | warning | The file has no non-empty `id`. | Add a stable identifier. |
| `version_missing` | warning | The file has no non-empty `version`. | Add a quoted version string. |
| `version_unquoted` | error | A numeric-looking version was not quoted and may have lost precision. | Quote it, for example `version: "1.10"`. |
| `version_unparseable` | warning | The version does not parse as a `numeric_version`. | Use a numeric-version-compatible string such as `"1.0"`. |
| `duplicate_id_version` | warning | Another file has the same kind, ID, and version. | Keep one authoritative file or assign a new version. |
| `no_templates` | error | The requested directory has no recognized template files at its scanned level. | Check the path and put the files directly in the library directory. |
| `template_in_subdirectory` | warning | A template-shaped file is below the scan level and will not be loaded. | Move active files directly into `templates/`. |
| `template_outside_library_dir` | error | A template-shaped file is at the repository root while `templates/` is the selected library. | Move it into `templates/` and update related paths. |
| `target_invalid` | warning | An option target is unknown, malformed, or does not resolve in its dataset body. | Correct the target path and use a supported metadata or dataset field. |
| `target_machine_owned` | error | An option targets `metadata.template` or `metadata.import_issues`. | Remove the option/effect; these fields are written by DTAtools. |
| `party_slot_invalid` | warning | A party slot is malformed or names a profile ID not found in the library. | Check target, slot fields, role, and profile IDs. |
| `vocab_slot_invalid` | error | A vocabulary slot is malformed, has a bad target, or has invalid selections. | Correct its target, vocabulary, mode, field, or term selections. |
| `vocab_slot_unresolved` | warning | The slot's vocabulary reference is not found in the validated directory. | Add the vocabulary file or correct the reference. |
| `dataset_template_unresolved` | warning | A `datasets[].template` reference does not resolve to a dataset template. | Correct the ID/version or add the referenced file. |
| `patch_incoherent` | warning | A dataset patch cannot be applied to the referenced dataset template. | Check operation order, IDs, and patch fields. |
| `dataset_missing` | error | A dataset template lacks its required singular `dataset:` body. | Add a dataset body; do not use creation-template `datasets:` here. |
| `values_and_values_from` | warning | One column defines both literal values and a vocabulary binding. | Remove one; the vocabulary binding takes precedence. |
| `values_from_pattern` | error | A column combines `values_from` and `pattern`. | Choose a vocabulary-backed permitted-value list or a pattern. |
| `values_from_invalid` | error | The vocabulary binding is malformed or cannot be read. | Use a valid reference or mapping and a supported field. |
| `values_from_unresolved` | warning | A column's vocabulary reference is not found in the validated directory. | Add the vocabulary or correct its ID/version. |
| `values_from_terms_invalid` | error | An included or excluded term code is absent from the vocabulary. | Correct the code or update the vocabulary. |
| `vocabulary_invalid` | error | A vocabulary cannot be read or has no usable term definition. | Fix its header, type, terms, or YAML. |
| `vocabulary_unresolved` | warning | A vocabulary's `extends` reference is not found. | Add the parent vocabulary or correct the reference. |
| `vocabulary_extends_failed` | error | Vocabulary inheritance fails, including a cycle or invalid removal. | Repair the extension chain and term operations. |
| `extends_cycle` | warning | A creation-template inheritance chain cycles or exceeds its depth limit. | Break the cycle and keep the inheritance chain within the limit. |
| `extends_unresolved` | warning | A creation template's parent cannot be resolved in the validated library. | Add the parent or correct its reference. |
| `sealed_violation` | warning | A descendant changes a path sealed by an ancestor. | Remove the descendant change or revise the parent's seal. |
| `sealed_path_unknown` | warning | A sealed path does not match a value in the resolved template. | Correct the path or remove the ineffective seal. |
| `instantiate_failed` | error | A concrete creation template cannot be built with its resolved defaults and minimum slot selections. | Fix the template, referenced data, dataset definition, or required choices. |

A validator code names a detected issue; the message column adds the actual file, path, or reference involved. For warnings on unresolved
references, remember that validation is scoped to the directory passed to the validator. A reference available only in a separate app source may
therefore need to be included in the repository being validated or handled by a repo-specific validation setup.

## Authoring workflow

Use a small, reviewable change process for each template update.

1. Create a branch from the repository's normal development branch.
2. Add a new template file or increment the version of a released contract.
3. Keep all template and `source:` files in the top level of `templates/`.
4. Check every ID and version reference against the files in the library.
5. Run strict validation from the repository root.
6. Configure a local app source and preview the document when the change affects what authors see or what metadata is built.
7. Open a pull request with the purpose, affected template IDs, and expected behavior.
8. Ask the reviewer to check the YAML semantics, versioning, data privacy, and validation result.
9. Merge only after required checks pass. 10. Allow the deployed app to refresh its source, or trigger the deployment's normal refresh process.

A local preview source can be configured as:

```text
DTATOOLS_TEMPLATE_SOURCES=dev=dir:/path/to/your/clone
```

On systems where the app process uses R environment configuration, set the variable before launching the app. Do not store deployment tokens in
shell history or repository files.

### Pull-request review checklist

- Does each file's `kind` match its filename suffix?
- Is every `version` quoted?
- Is the file directly under `templates/`?
- Are IDs stable, unique within kind, and consistently referenced?
- Are every `extends`, `template`, `values_from`, and party-profile reference resolvable?
- Does a creation template have the intended base, datasets, and author choices?
- Are machine-owned metadata fields excluded from options and effects?
- Do dataset patches target the intended IDs and use supported operations?
- Are column `values`, `values_from`, and `pattern` mutually consistent?
- Are term codes exact, including leading zeroes and case?
- Are group conditions and scopes reviewed against representative data?
- Does strict validation pass without warnings that are understood and intentionally retained?
- Does the change include only contact data approved for this repository?
- If a released template changes behavior, was a new version added rather than rewriting the old one?
- If an app deployment consumes this repository, is the deployment ordering safe for its DTAtools version?

For a consequential rule or patch, consider a small representative input fixture in the repository's existing tests. The library validator checks
structure and template construction; it does not prove that every business interpretation is correct for every real dataset.

## Troubleshooting

### A template does not appear in the app

Check these items in order:

1. The file is inside the selected library folder, directly at its top level.
2. The suffix is one of the four supported suffixes and matches `kind`.
3. The file parses as a YAML mapping and has non-empty `kind`, `id`, and quoted `version` fields.
4. The app's configured source points to the repository root when using the new convention, or directly to the templates folder for an older
   release.
5. The configured source name and scheme are spelled correctly.
6. No earlier source already supplies the same kind, ID, and version; source precedence can retain the first one.
7. A private-source configuration has not replaced packaged examples unless `DTATOOLS_TEMPLATE_INCLUDE_BUILTIN` is enabled.
8. For a Git source, allow the refresh interval to expire or trigger the normal refresh operation, then inspect the app's source warnings.

A nested file is not promoted into the catalogue merely because validation can report it. Move it into `templates/`.

### The validator reports no templates

Confirm the current working directory, the path passed to `validate_template()`, and whether the files are one directory deeper than expected.
With the new layout, validate the repository root or explicitly validate the `templates/` folder. A typo or an empty directory is an error, not a
successful empty check.

### The validator reports root-level leftovers

If `templates/` contains recognized template files, it is the active library directory. Move all remaining template files from the repository
root into `templates/`. Do not expect the app to merge root files with that folder.

### A version appears to change unexpectedly

Quote every version. YAML may convert an unquoted scalar to a number before DTAtools sees it; once `1.10` has become `1.1`, the lost trailing
zero cannot be recovered. The validator reports `version_unquoted` as an error.

### A reference cannot be resolved

Check the exact spelling and kind of the target file. A dataset entry's `template:` must name a dataset template, `extends:` names a creation
template for creation templates, and vocabulary `extends:` names a vocabulary. A column's `values_from:` points to a vocabulary. Use `id@version`
to pin the intended version, or a bare ID only when latest version behavior is wanted.

Validation sees the files in the directory being checked. If an app's source list has several repositories, a reference that is available only in
another source may not resolve during validation of this repository alone.

### A patch is incoherent

Check that every removed or modified column ID exists in the imported dataset and every added ID is new. Review the patch operation order:
removals happen before additions, modifications, and `set` paths. In generic `columns:` verbs, check that `add` names a new entry and `modify`
names an inherited or already-added entry. Use `set` paths relative to `dataset`, not a path rooted at `metadata`.

### A values binding is rejected

Confirm that `values_from` points to a readable vocabulary and that `include` or `exclude` names existing term codes. Use `field: code` or
`field: label`. A column cannot combine the binding with `pattern`; remove one constraint. If both `values` and `values_from` are present, remove
the literal list unless the warning and binding precedence are intentional.

### A party slot has no suitable choices

Check that its target is exactly `metadata.supplier` or `metadata.receiver`. Ensure that at least one profile has the matching role (or role
`any`) and, if `profiles` is set, that the profile ID appears in that allow-list. A profile ID is not a filename.

### Private Git source authentication fails

Check that the deployment secret is set as `DTATOOLS_TEMPLATE_GIT_TOKEN`, that the source URL is HTTPS, and that `DTATOOLS_TEMPLATE_GIT_AUTH`
matches the host's token authentication method. `basic` uses `DTATOOLS_TEMPLATE_GIT_USER`, which defaults to `git`; `bearer` uses a bearer
authorization header. Never put a token in the URL or commit it to the repository.

A Git refresh failure with a last-good cache continues to serve the stale checkout, with a stale-source warning. A first fetch with no existing
cache cannot serve templates. Inspect the deployment's safe source diagnostics without printing the secret.

### Git cache or refresh behavior is unexpected

`DTATOOLS_TEMPLATE_REFRESH_SECONDS` defaults to 900. It controls how long a Git checkout and its in-process index are trusted before refresh. A
non-positive or unparseable value warns and falls back to 900. Use `DTATOOLS_TEMPLATE_CACHE_DIR` to select a writable cache location when the
default per-user cache is not suitable. A writable fallback under `tempdir()` may not persist across app restarts, so configure a durable
writable cache if cold-start availability is required.

### The Word export template is missing

Word `.docx` export templates are not a template-library feature. The library's four indexed kinds are creation templates, dataset templates,
party profiles, and vocabularies. Word export uses the DTAtools package's own packaged export templates through `write_dta(template = ...)`;
adding a `.docx` to this repository does not add it to the app's template catalogue.

## Migrating a root-layout repository

This section applies to an existing repository whose template YAML files are currently at its root.

1. Create the conventional folder:

   ```sh
   mkdir templates
   ```

2. Move each template file into it. For example:

   ```sh
   git mv starter.dta-template.yaml templates/
   git mv starter_dataset.dta-dataset-template.yaml templates/
   git mv starter_supplier.dta-party.yaml templates/
   git mv starter_terms.dta-vocabulary.yaml templates/
   ```

Include `.yml` files and every other recognized template file as well.

3. Move files referenced through dataset `source:` entries into `templates/`. Keep each referenced file inside the directory containing its
   template, and update the relative `source:` value if needed.
4. Update repository scripts, tests, and CI commands that explicitly name template file paths. Commands that validate the repository root can
   keep using `validate_template('.')`; update any script that assumes YAML files remain at the root.
5. Validate from the repository root and inspect the report. Resolve `template_outside_library_dir`, `template_in_subdirectory`, and every cross-
   file or instantiation issue before merging.
6. Preview the source in the app and confirm that all expected creation templates appear.

### Deployment ordering warning

A running app on a DTAtools release without `templates/` auto-detection will look for files at the repository root. If the repository is migrated
first, a source configured at the repository root appears to contain no templates. With a private source configured, this does not safely fall
back to the packaged examples; the catalogue can be empty or source loading can fail.

Deploy the DTAtools release that supports this convention first. Only after that release is running should the repository migration be merged and
published to the app. This order avoids a window in which the old app and new repository layout disagree.

For an older release that must use the migrated repository before it can be upgraded, configure a `dir:` source directly at
`<checkout>/templates`. The `git:` source grammar has no subpath option, so that workaround does not apply to a Git source. Upgrade and deploy
DTAtools before migrating a repository consumed through `git:`.
