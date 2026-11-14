# DTAtools template library

## What this repository is

This repository is a library of reusable YAML templates for creating Data Transfer Specifications (DTS) with DTAtools. Its layout is deliberately
simple: README, the optional `dta-app.yaml` and repository automation live at the root; each kind of template file lives directly inside its own folder
(`templates/`, `parties/`, `vocabulary/`), and Word export documents live in `export-templates/`.

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
2. Edit or add template files directly in the folder for their kind: `templates/`, `parties/` or `vocabulary/`.
3. Lint from the repository root:

   ```sh
   Rscript -e "DTAtools::lint_template_repo('.')"
   ```

4. Open a pull request and ask another author to review the change.
5. After merge, the configured DTAtools app source can load the updated library. A Git source is refreshed according to its configured interval.

Do not treat a successful YAML parse as proof that the template works. The validator also resolves references and dry-runs concrete creation
templates.

## Folder layout

Use this structure for a template library (only the CI file of each chosen provider is created, one per provider):

```text
repository/
├── README.md
├── dta-app.yaml
├── templates/
│   ├── starter.dta-template.yaml
│   └── starter_dataset.dta-dataset-template.yaml
├── parties/
│   └── starter_supplier.dta-party.yaml
├── vocabulary/
│   └── starter_terms.dta-vocabulary.yaml
├── export-templates/
│   └── README.md
├── .github/
│   └── workflows/
│       └── validate-templates.yml
├── bitbucket-pipelines.yml
└── Jenkinsfile
```

Each kind of file lives in its own folder, directly inside it:

| Folder | Holds |
| --- | --- |
| `templates/` | Creation templates (`*.dta-template.yaml`) and dataset templates (`*.dta-dataset-template.yaml`), plus any data file a dataset `source:` refers to. |
| `parties/` | Party profiles (`*.dta-party.yaml`). |
| `vocabulary/` | Controlled vocabularies (`*.dta-vocabulary.yaml`). |
| `export-templates/` | Word templates (`*.docx` containing `{PLACEHOLDER}` markers) used for Word export. |
| repository root | The optional `dta-app.yaml` app configuration, plus README, CI files, scripts, tests, Docker files, charts, and deployment configuration. |

Keep README, CI files, scripts, tests, Docker files, charts, and deployment configuration at the repository root or in their own repository
folders; they are not part of the library. A library needs none of the folders it does not use, but it must hold at least one file in one of the
four (`templates/`, `parties/`, `vocabulary/`, `export-templates/`).

### Keep each folder flat

Every recognized file must be directly inside the folder for its kind. The loader scans each folder non-recursively; it does not search
arbitrary subfolders. A file under `templates/by-study/` or `templates/archive/` is not part of the active catalogue.

The validator reports a template-shaped file in a subfolder as `template_in_subdirectory` with warning severity. The warning is a reminder that
the app will not load it, not evidence that recursive lookup occurred. Move an active file to the top level of its folder.

A file in the wrong folder is not loaded either. The validator reports it as `template_misplaced` with error severity: a template file at the
repository root, a party profile in `templates/`, a vocabulary in `parties/`, and so on. At runtime the app also logs a warning for such a
file and adds a note to the template picker (for example, "starter: templates/starter_supplier.dta-party.yaml - a party profile belongs in
parties/ (library layout: templates/, parties/, vocabulary/, export-templates/)", naming the source and the path relative to its root), and an
empty picker explains the folder layout. Folder names are case-sensitive on the server (Linux): `Templates/` is not `templates/`, even where your
local file system finds it, so it is misplaced there. Nothing of an older flat layout, or of an older layout that kept every kind in `templates/`, is read any more; see "Reorganising an
older repository" below.

Supporting files named by a dataset entry's `source:` must be stored in `templates/`, beside the template that refers to them. Keep the path
relative to that template. The resolver prevents a template from using `..` or an absolute path to read arbitrary files outside its own folder.

A `chart/templates/` directory is not the library's `templates/` folder. Only the folders at the library root count; other application or chart
folders are irrelevant to template discovery.

### How the library root is selected

The library root is the directory that holds these folders: the repository root of a clone, a `dir:` source's path, a `git:` source's checkout,
or the folder `validate_template()` is pointed at. `validate_template('.')` and the app's template sources scan `templates/`, `parties/`,
`vocabulary/` and `export-templates/` below it, and read `dta-app.yaml` from it.

Point the validator or a source at the repository root, not at one of the folders. `validate_template('templates')` (or `parties` or
`vocabulary`) is rejected with `library_subfolder_given` and error severity; validate the parent instead. If the folder is in fact a
flat library (template files directly in it), move its files into `templates/`, `parties/` and `vocabulary/` sub-folders instead; see
"Reorganising an older repository" below.

This layout needs a DTAtools release that reads it, and older releases cannot read it. Reorganise on a branch and merge it when the new package version goes live.

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
- `mixins` to compose the template from abstract, flat building blocks; see Mixins below.
- `required` to require selected paths to be filled before a document is created.
- `prior_versions` to record, by hand, the template's own versions from before its first file entered the library. Later history is computed;
  see "Template version history" below.

Vocabularies may use `extends` to build on another vocabulary, with the vocabulary-specific `add_terms` and `remove_terms` operations described
below. Do not confuse that vocabulary extension with the creation-template merge rules.

Party profiles use `role`, whose supported values are `supplier`, `receiver`, and `any`. Other file kinds do not use `role` to select a party
profile.

A template's `metadata.template` field is not an author-settable header field. The app writes this machine-owned provenance onto a created
document.

## Creation templates

A creation template (`kind: dta_creation_template`) describes a whole new DTA. Its `base:` supplies initial document data; `options:` describes
inputs the author can choose; `datasets:` supplies datasets to build into the new document; and the optional `variants:` offers mutually
exclusive record structures, such as one per trial platform.

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

A creation template that uses neither `extends:` nor `mixins:` needs a `base:` mapping. A concrete template
must have at least one dataset after inheritance (through `extends:` or `mixins:`); an abstract base may provide
metadata only. A child with `extends:` or `mixins:` may omit sections to inherit them, but
must supply datasets if its parent and mixins have none. A template may omit top-level
`datasets:` when every `variants:` choice supplies its own.

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
- `study`
- `sections`

Nested mapping values such as `receiver`, `supplier`, and `transmission` can contain their own fields. Use the same structure as metadata in an
ordinary DTA definition. The template engine checks the top-level names; a misspelled metadata name is not a new extension point.

### Study identity and agreement sections

`study` is an optional mapping with `id` and `protocol_id`. Supplied identifiers
must be nonblank strings. They describe the agreement; setting them does not
change a dataset's `STUDYID` values. `header` remains a presentation field.

`sections` is an ordered mapping with stable identifiers as keys, not an
unnamed sequence. Keys start with a letter and contain only letters, digits
and underscores. Each section contains exactly `title` (a nonblank string)
and `text` (plain text; an empty string is permitted for drafts).

```yaml
base:
  metadata:
    study:
      id: DEMO-001
      protocol_id: PROTOCOL-01
    sections:
      responsibilities:
        title: Responsibilities
        text: |
          The receiver reviews delivered files.

          Discrepancies are reported to the supplier.
      exceptions:
        title: Exceptions
        text: Agreed exceptions are documented before the affected transfer.
```

The stored order is the document order. Inheritance can override
`base.metadata.sections.exceptions.text` without copying unrelated clauses.
An option can target `metadata.study.id` or
`metadata.sections.exceptions.text`; use `type: textarea` for narrative text.
The app's Metadata tab supports section creation, editing, deletion and
reordering. Section content is text, not an HTML execution surface.

Both properties default to empty mappings and are omitted from exported YAML
when empty. Old specifications continue to work unchanged. When drafting a
revision, `study` is carried over by default; `sections` requires explicit
opt-in so a newer template's clauses are not silently replaced.

Three-way rebase treats the ordered section collection as one value. Conflicts
require choosing the current or target collection; it does not merge half of
one clause with half of another. Ordinary template inheritance still supports
overriding individual named clauses.

### Additional transmission requirements

The existing `transmission` mapping accepts these optional text fields:

| Field | Meaning |
| --- | --- |
| `extent` | Cumulative, incremental or another agreed transfer extent |
| `scope` | Records/data covered by the transfer |
| `packaging` | File grouping or archive requirements |
| `security` | Agreed secure handling and transfer requirements |
| `checksum` | Checksum algorithm or verification procedure |
| `test_requirements` | Test-transfer content and acceptance procedure |
| `blinding_requirements` | Which information must be withheld and under what conditions |

These fields are editable in the Metadata tab. Empty editor input removes a
field; existing custom transmission fields survive unrelated edits.
Statements describe the agreement: they do not send, encrypt, checksum or
redact data. Unspecified test-upload/blinding flags remain unspecified.

Default Word/Markdown exports include study information and ordered sections.
For a custom Word layout, use inline `{STUDY_ID}` and `{PROTOCOL_ID}` or
standalone `{STUDY_TABLE}`, `{AGREEMENT_SECTIONS}` and `{CONTAINERS_TABLE}` block paragraphs (the last renders the delivery containers
of the document, and is omitted when it has none).
The library does not alter user-supplied Word templates automatically.

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

`base.metadata.version_history` is the history of the *document* the template creates, and it is copied into every new DTA. It says
nothing about the template's own versions: those are listed in the template picker, computed from the library (see "Template version
history" below). Do not record template changes here.

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
| `text` | Single-line text field; with `choices`, a "Choose a suggestion" picker that fills it, and custom text is accepted by default. |
| `select` | Choice-oriented control; custom entries are disabled by default. Supply `choices` for a bounded set. |
| `boolean` | Checkbox; its selected value is a logical. |
| `textarea` | Multi-line text field; with `choices`, the same suggestion picker above it. |
| `number` | Numeric input. |

If `type` is omitted, the option is treated as `text`. `choices` may be a list of values or a list of `{value, label}` mappings. A `default`
outside the supplied choices is retained: it is pre-filled in the field and added to the suggestion picker as the selected value.

These option fields are supported:

- `id` identifies the option and is used as the selection key.
- `label` is the visible prompt; if absent, the ID is used.
- `type` selects the input behavior shown above.
- `choices` supplies suggested or selectable values.
- `default` preselects a value. When a direct metadata `target` has no explicit default, the value already in `base.metadata` can supply it.
- `help` displays explanatory text beneath the control.
- `allow_custom` controls whether the option accepts typed values; when false it is a plain dropdown of its choices (a `textarea` is always free text). The default is true for `text` and false for `select`.
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
- `as` supplies the id for the dataset in the created DTA.
- `options` supplies values for the dataset template's options; these are static values for that dataset build, not additional creation-dialog
  controls.
- `patch` modifies the dataset template's result.
- `source` names a finished dataset YAML relative to the creation-template file. Keep such supporting files in `templates/`, beside the creation template that names them.

A bare string or `{source: ...}` reads a finished dataset YAML. An inline mapping contains the dataset definition itself. In a private library,
prefer `template:` when sharing a reusable dataset schema and use `source:` only when a ready-made dataset specification is actually the intended
input.

### `variants:`

Use `variants:` when one agreement has mutually exclusive record structures, for example one dataset layout per trial platform. Unlike an
option, the choice is structural and mandatory: the creation dialog asks for it first, accepts only the declared values, and cannot leave it
unresolved.

```yaml
variants:
  id: trial_platform
  label: Trial platform
  choices:
    - value: brave
      label: BRAVE
      source: {document: Adjudication DTS, version: "5.0"}
      datasets:
        - template: adjudication_brave_records@1.0
    - value: veeva
      label: Veeva
      source: {document: Adjudication DTS, version: "5.0"}
      datasets:
        - template: adjudication_veeva_records@1.0
```

- `id` is required and must not equal any option id, including options a choice adds. `label` is optional.
- `choices` is a non-empty sequence. Each choice needs a unique, non-empty `value`; the reserved values `__blank__` and `__custom__` are rejected.
- Each choice must supply a non-empty `datasets:` sequence, which **replaces** the template's dataset list rather than appending to it.
- A choice may add `label`, a `source:` mapping, `base:` (deep-merged over the inherited base), and `options:` (merged by option id with the
  same rules as creation-template inheritance). No other keys are allowed.

Record the source document version in each choice's `source:`, not in the template's own `version:`, which identifies the library definition.
A created document records the chosen value and source under `metadata.template.variant`. The validator reports a malformed `variants:` block as
`variant_invalid` and dry-runs every choice, not only a default; seals inherited from an ancestor are checked against each choice too.

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

The supported target form is `datasets.<dataset id>.columns.<column id>.values`. The dataset and column ids must refer to objects the
template actually builds. For a `type: matrix` dataset, whose column specs are its row labels, write `row_labels` in place of `columns`:
`datasets.<dataset id>.row_labels.<row label id>.values`. The segment must match the dataset's own key.

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
`dataset_missing`. The body needs an `id`, even when every import renames the dataset with `as:`; without one it is reported as
`dataset_id_missing`.

```yaml
kind: dta_dataset_template
id: starter_dataset
label: Starter dataset
version: "1.0"
description: A small reusable tabular dataset.

dataset:
  id: starter_data
  type: tabular
  name: Starter dataset
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
mappings. The supported handler types are `csv`, `tsv`, `sas7bdat`, `xpt`, and `any`.

```yaml
files:
  filename: starter_data.tsv
  type: tsv
```

A file entry may also say which `.zip` archive or SAS transport library (`.xpt`) its file arrives inside, with `container:`. It is optional: leave it
out and the file is accepted loose or inside any container. In a template, name the container by its **file name** (`sdtm.zip`): a template has no
`containers:` section, so an id would not resolve, and the file name declares the container for the documents created from it. `*` matches any run of
characters and `?` one character (`sdtm_*.zip`), and letter case must match. Quote a value that starts with `*` or contains ` #`. `container: none` exempts
an entry. A file that arrives outside its container is still loaded and validated, and `check()` reports the packaging separately.

Pattern entries of different datasets must not be able to take the same file name (`^lb.*[.]csv$` next to `^lb_supp.*[.]csv$`): `validate_template()`
fails a template whose datasets overlap that way (`file_name_conflict`), and the `lint_dta()` of a document created from it warns.

```yaml
files:
  filename: ae.csv
  type: csv
  container: sdtm.zip
```

A non-tabular deliverable is represented by a dataset with `type: file` and one or more file handlers. `any` is the general file handler; it can
also restrict accepted file endings when the handler supports `extensions`. The handler's `filename` may be a pattern when `pattern: true` is
used.

```yaml
dataset:
  id: supporting_documents
  type: file
  files:
    - type: any
      filename: "report.*"
      pattern: true
      extensions: [pdf, zip]
```

File handlers describe expected deliverables; they are not extra template kinds. Use the file type values recognized by the package rather than
inventing `xlsx` or other handler names.

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

Dataset paths address columns and rules by their `id`, not by list position. A `type: matrix` dataset keeps its column specs under
`row_labels:` instead of `columns:`, so its paths read `dataset.row_labels.<id>.<field>`; a path that names the other one fails with a hint. The specific path grammar is implemented by the dataset-template
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

A range rule constrains one column to a numeric minimum and maximum. The key is `columns` (a single name):

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

`set` paths are relative to the dataset body. Examples include `name`, `description`, `files.filename`, and `columns.SOURCE.values`. Column and rule
paths are addressed by ID. Do not use a positional index in a patch path.

For a `type: matrix` dataset the same operations act on its `row_labels:` list (the operation names do not change), and `set` paths use
`row_labels.<id>.<field>`; the `matrix:` block is reached with `matrix.<key>` paths. A short example:

```yaml
dataset:
  id: gene_counts
  type: matrix
  files: { filename: counts.tsv, type: tsv }
  row_labels:
    - id: GENE_ID
      type: SAS Char
      nullable: No
  matrix:
    header: { corner: [gene_id], pattern: '^S[0-9]{4}$' }
    cells: { type: SAS Num, range: { min: 0 } }
```

with, in a creation template, `patch: { modify_columns: [{ id: GENE_ID, pattern: '^ENSG' }], set: { matrix.cells.type: SAS Int } }`.

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

### Naming the columns a vocabulary is for: `columns:`

A vocabulary may list the columns it belongs to. The app's column editor then preselects that vocabulary in "Choose from vocabulary…".

```yaml
columns:
  - STATUS                 # a bare id writes the codes
  - id: STATUS_TEXT        # field: label writes the labels instead
    field: label
```

- Column ids are compared without regard to case.
- `field` is `code` or `label` and defaults to `code`.
- An entry with no id, a key other than `id` and `field`, or any other `field` value makes the vocabulary invalid (`vocabulary_invalid`).
- `columns:` is not inherited through `extends:`.
- It affects only the suggestion; validation does not use it.

A column without a declaration still gets a suggestion when its current values are all codes, or all labels, of one vocabulary.

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
validator reports `extends_cycle` or `extends_unresolved`, and a concrete template also receives `instantiate_failed`. The same holds for
`mixin_unresolved`, `mixin_invalid` and `mixin_conflict`, described below.

### Mixins: composing abstract building blocks

`extends:` takes exactly one parent, so a template that needs a shared standard plus an assay block plus a privacy block cannot be built from
parents alone. A mixin is a small abstract template that only adds content, and `mixins:` lists the ones to fold in. Use `extends:` for "this is a
variant of that template", where the child may change anything the parent says; use `mixins:` for "this also contains these pieces", where
nothing may silently override anything else. Because a mixin can only add, the order of the list never changes the result.

A mixin file:

```yaml
kind: dta_creation_template
id: assay_smrnaseq
label: smRNA-seq assay block
version: "1.0"
abstract: true

base: {}

options:
  - id: panel
    label: Panel
    type: text

datasets:
  - template: smrnaseq_counts@1.0
    as: smrnaseq_counts
```

A template that extends a standard and mixes the block in:

```yaml
kind: dta_creation_template
id: study_x
label: Study X DTS
version: "1.0"
extends: company_standard@2.0
mixins:
  - privacy_eu@1.0
  - assay_smrnaseq@1.0
```

`mixins:` is a list of references in the same form as `extends:` (`id`, `id@version`, `id@latest`); a single string counts as a one-element list.
The rules:

- A mixin must be `abstract: true`, and it still needs `base:` like any abstract template (`base: {}` is enough).
- A mixin may not declare `extends`, `mixins`, `sealed`, `required`, `variants` or `order`. It references nothing, so mixins do not nest.
- Its collections (`options`, `datasets`, `party_slots`, `vocabulary_slots`) use the bare list form, with no `remove:` on any entry, and no
  section or `base:` value may be `null`. A mixin only adds, which is why the "`key: null` removes the inherited key" rule from the table above does
  not apply to it.
- Only `base`, `options`, `datasets`, `party_slots` and `vocabulary_slots` are contributed. Header fields such as `label` and `description` are
  not copied.
- `base:` mappings merge key by key. Any key contributed twice, whether by the `extends:` chain and a mixin or by two mixins, is a conflict, as is
  an option, dataset, party slot or vocabulary slot with the same identity. This is deliberately not the "same identity modifies" rule from
  above, which applies only to the child over the already folded result. The child resolves a conflict by writing the value itself.
- The child may then modify or remove any mixin-provided entry with the usual four states and collection verbs.
- Mixin content is checked against the `sealed:` paths of the template's ancestors, as if it had been written in the child.
- Descendants inherit the mixed-in content but not the `mixins:` key. Listing a mixin that an ancestor already mixes in is a conflict.
- Mixins do not count toward the depth limit and cannot create cycles.

A template that uses only `mixins:` may omit `base:` and `datasets:`, as one that uses `extends:` may. Problems are reported as `mixin_invalid`
(a malformed `mixins:` value, or a named template that is not a valid mixin), `mixin_conflict` and `mixin_unresolved`.

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

### Template version history

The template picker shows a creation template's history. It is computed, not written: each library version is built with its defaults and
compared with the one before it, across the template's own interface (label, description, lineage, mixins, options, slots, `required`, `sealed`),
the metadata and the datasets it builds. A dependency the template follows unpinned (a bare `id` or `id@latest` in `extends:`, a
`datasets:` import, a vocabulary) gets its own entry when it publishes a version that changes what the template builds. A pinned dependency
does not. Because every published version is its own immutable file, there is no changelog to keep in step for library versions.

This is unrelated to `base.metadata.version_history`, which is the history of the created document.

### Versions from before the library: `prior_versions:`

Versions the template went through before its first file entered the library have nothing to compare, so record them by hand:

```yaml
version: "5.0"
prior_versions:
  - version: "4.0"
    date: 2024-03-01          # optional, YYYY-MM-DD
    changes: Added the PK dataset.
  - version: "3.0"
    changes: Moved to SDTM-style column names.
```

Each entry needs `version` and `changes`, and must be older than the template's first version in the library. The newest version that declares the
block is the one shown, so a later release can correct it. It is not inherited through `extends:`.

## Provenance and upgrading documents

When a document is built from a creation template, the app records a `metadata.template` provenance record. It can include the template ID and
version, source information, resolved Git commit, file path, a hash of the resolved definition, creation date, inheritance lineage and the mixins, the author's
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

The optional name labels the source. Supported schemes are `dir:`, `pkg:`, and `git:`. Each source points at a library root: the directory that
holds `templates/`, `parties/`, `vocabulary/`, `export-templates/` and, optionally, `dta-app.yaml`. Do not point a source at one of those folders.

### Source examples

A local directory source:

```text
DTATOOLS_TEMPLATE_SOURCES=site=dir:/srv/dta-library
```

The directory must exist and be readable, and it must be the library root, for example the root of a clone of this repository.

A companion R package source:

```text
DTATOOLS_TEMPLATE_SOURCES=site=pkg:company.dta.templates
```

The package must be installed and expose a library root in `inst/dta-templates`, with the same folders as above (`templates/`, `parties/`,
`vocabulary/`, `export-templates/`). Empty folders do not survive `R CMD build`, so ship at least one file in each folder you use.

A Git source:

```text
DTATOOLS_TEMPLATE_SOURCES=company=git:https://example.org/group/dta-library.git#main
```

The optional `#ref` selects a branch or tag; if omitted, Git uses `HEAD`. The template folders (`templates/`, `parties/`,
`vocabulary/`, `export-templates/`) belong at the root of the repository. The locator is an HTTPS Git URL; do not put credentials in the URL.

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
directory. These are useful for a developer's local copy and do not replace the repository as the canonical library.

To test the source resolution without changing deployment settings, configure a local `dir:` source for the clone and run the app with that
environment value.

## App configuration (`dta-app.yaml`)

A library can also describe the app that serves it: who runs it, how it looks, which links and features it offers, and which Word templates it
exports to. Put one file named `dta-app.yaml` (or `dta-app.yml`) at the root of the library, beside the `templates/` folder, not inside it. It is not a template kind, so
the template scan ignores it and it never appears in the picker. Every key is optional; an absent file, or one that only holds comments, means
"the stock app". The scaffolded `dta-app.yaml` lists every key, commented out, with its default.

```yaml
kind: dta_app_config
organisation:
  name: Example Institute
  unit: Data Management
branding:
  logo: logo.png
header_tag:
  label: GxP
  color: "#C0392B"
colors:
  primary: "#1F4E79"
links:
  report_issue: mailto:dm-support@example.org
  about: false
features:
  editing: false
exports:
  document: [word_template]
  word_templates:
    - file: export-templates/our_dta.docx
      label: Our DTA template
```

The `exports.word_templates` list is optional. Without it, every `.docx` in `export-templates/` that contains a `{PLACEHOLDER}` marker is offered in the export dialog anyway.

The value of `kind:` must be `dta_app_config` when present. Each section is a mapping of `key: value` pairs.

### Key reference

`organisation`

| Key | Default | Meaning |
| --- | --- | --- |
| `name` | empty | Owner of the deployment. With no name, no organisation is shown anywhere in the app. |
| `unit` | empty | Department or group, shown after the name. |

`document`

| Key | Default | Meaning |
| --- | --- | --- |
| `name` | `Data Transfer Agreement` | What the library calls its documents: one string, or a list of one to three (for example `[Data Transfer Agreement, Data Transmission Specification]`). Shown as the main title of every export, with the document's own title beneath it, and available to Word templates as `{DOCUMENT_NAME}`. Several names are joined with ` / `. |
| `abbreviation` | `DTA` | Short form used in the app's labels ("Create DTA", "Export DTA") and as `{DOCUMENT_ABBREVIATION}`: one string, or a list of one to three, each at most 12 characters. When both keys are lists they must have the same length. Several abbreviations are joined with `/` ("Create DTA/DTS"). |

Both are plain text. The organisation (`organisation.name`, `organisation.unit`) is added to the front page and the running header of Word and PDF exports, and to the header block of Markdown and HTML exports; as Word-template placeholders it is `{ORGANISATION_NAME}` and `{ORGANISATION_UNIT}`.

`branding`

| Key | Default | Meaning |
| --- | --- | --- |
| `app_title` | `DTAtools` | Title in the brand bar. |
| `app_subtitle` | the stock tag line | Line under the title. |
| `window_title` | the value of `app_title` | Browser tab title. |
| `footer_label` | `<name> · <unit>`, or empty without a name | Text in the footer. `""` hides it. |
| `logo` | the DTAtools logo | Image file in the library, with a path relative to `dta-app.yaml` (see "Files the configuration refers to"). |
| `logo_alt` | `<app_title> logo` | Alternative text for the logo. |
| `landing_headline` | the stock headline | Heading on the start page. |
| `landing_lede` | the stock paragraph | Paragraph under the heading on the start page. |

`header_tag`

| Key | Default | Meaning |
| --- | --- | --- |
| `label` | empty | Short text shown beside the app title on every page, the start page included, for example `GxP`, `non-GxP`, `GcP` or `Non-GcP`. One line, at most 32 characters. Empty shows no tag. Shown in the app only; it is not added to exports or the browser tab title. |
| `color` | none | Fill colour of the tag, a quoted hex value, `#RGB` or `#RRGGBB`. The text colour is chosen automatically to meet WCAG AA contrast (4.5:1). Without a colour the tag uses the brand bar's neutral pill; leave the key out for that, since an empty `color:` is an error. |

`colors` takes a quoted hex value, `#RGB` or `#RRGGBB`, for each of the keys below. The default is the stock DTAtools scheme.

| Key | Used for |
| --- | --- |
| `primary`, `primary_dark`, `primary_light` | Brand bar, buttons, links and their tints. |
| `accent` | Highlights and focus rings. |
| `text`, `muted` | Body text and secondary text. |
| `page_background`, `surface` | Page and card backgrounds. |
| `warning`, `warning_dark` | Warning states. |
| `nodata_border` | Border of the "no data" status chip. |
| `unknown` | Marker for files whose import was never checked. |
| `pass`, `pass_bg`, `pass_border` | Passing validation results. |
| `fail`, `fail_bg`, `fail_border` | Failing validation results. |
| `pending`, `pending_bg`, `pending_border` | Results not yet computed. |

`links`

| Key | Default | Meaning |
| --- | --- | --- |
| `report_issue` | the DTAtools issue tracker | Target of the "Report issues" link. |
| `about` | the DTAtools credits | Target of the "About" link. |
| `repository` | the DTAtools repository | Footer link. |
| `documentation` | hidden | Adds a documentation link when set. |

Each link is an `http://`, `https://` or `mailto:` URL without spaces or quotes. `false` hides the link.

`features` (all default to `true`)

| Key | Meaning |
| --- | --- |
| `start.load_example` | "Load example" button on the start page. |
| `start.create_from_template` | "Create new from template" button. |
| `start.create_blank` | "Create new" button (a blank document). Forced off while `editing` is `false`. |
| `raw_yaml_tab` | The tab that shows and edits the raw YAML. |
| `raw_yaml_editable` | Whether the raw YAML can be edited and applied. `false` keeps the tab but makes it read-only and refuses applying server-side. Forced to `false` by `editing: false`. Default `true`. |
| `dataset_yaml_download` | Download of a single dataset's YAML. |
| `dta_yaml_download` | Download of the whole DTA's YAML. |
| `editing` | `false` makes the app read-only: no edit menu and no editing at all. |

`exports`

| Key | Default | Meaning |
| --- | --- | --- |
| `document` | `[markdown, markdown_pdf, word, word_template]` | Formats offered by "Export DTA". `[]` hides the button. |
| `default_document` | `word` | Format preselected in the export dialog. It must be offered; otherwise the first listed format is used (with a warning if you set it explicitly). |
| `word_templates` | none | Optional override for the label and order of the Word templates. A list of `{file, label}` entries whose `file` is a path relative to `dta-app.yaml`'s folder (the library root), for example `export-templates/report.docx`. `label` defaults to the file name without extension. Every `.docx` in `export-templates/` that contains a `{PLACEHOLDER}` marker is offered even without an entry; one without a marker is offered only when listed here. |
| `bundled_word_templates` | `true` | Keep the Word templates shipped with DTAtools in the list next to yours. |
| `validation_messages` | `[csv, tsv, xlsx, html]` | Download buttons for the validation messages. `[]` removes them. |
| `validation_summary` | `true` | The "Validation summary" HTML download. |

`markdown_pdf` is Markdown delivered as a PDF; `word` is the default Word layout; `word_template` lets the user pick one of the Word templates
described below.

### Rules

- **Quote every hex colour.** An unquoted `#00625B` is a YAML comment, so the key would be read as empty.
- **Features are enforced by the server**, not only by hiding the button: a disabled feature cannot be reached by other means.
  `editing: false` also removes "Create new", because an empty document that cannot be edited is of no use.
- **A bad value never stops the app.** The app warns, ignores that value and keeps the key's default. The validator reports the same problem as
  an error so CI fails first (see "Validating the configuration").
- **A file that cannot be used fails open.** If the file is not valid YAML, is not a mapping, or has a `kind:` other than `dta_app_config`, the
  app warns and uses the defaults for everything. Restrictions such as `editing: false` or a shortened `exports.document` are then not
  enforced. Run `validate_template()` before deploying.
- **Unknown keys** are ignored with a warning; this catches a misspelled section such as `colours:`.

### Files the configuration refers to

The logo and any Word template named in `exports.word_templates` are files in the library, referenced by a path relative to the folder holding
`dta-app.yaml` (the library root). An absolute path, a drive letter, `~`, `..`,
or a path that resolves (for example through a symbolic link) outside the library is rejected. The library folder itself is never
served over HTTP.

- **Logo:** `png`, `jpg`, `jpeg`, `gif`, `svg` or `webp`, at most 1 MB. It is embedded into the page.
- **Word template:** a `.docx` file in `export-templates/`. Every `.docx` there that contains a `{PLACEHOLDER}` marker is offered automatically in the app's export dialog, from every
  configured source, labelled with the file name without its extension; non-`.docx` files, dotfiles and Word lock files (`~$x.docx`) are ignored.
  A `.docx` without any marker would export with nothing substituted, so the app does not offer it unless `dta-app.yaml` lists it, and the
  validator warns `export_template_no_placeholders`. Markers are replaced wherever they appear. A marker is
  an upper-case name in single braces, for example `{DTA_TITLE}` or `{SUPPLIER_NAME}`, and `{#DATASETS}` ... `{/DATASETS}` repeats a span once
  per dataset. In R, `?DTAtools::export_with_template` documents the full set and `DTAtools::dta_template_placeholders()` lists it. The
  `.docx` files are not template-library files and do not appear in the creation-template picker.

### Several libraries

With more than one source in `DTATOOLS_TEMPLATE_SOURCES`, the first library, in the order the sources are listed, that has a `dta-app.yaml`
supplies the **whole** configuration. Keys are not merged across libraries. Any further `dta-app.yaml` is ignored and the app warns about it.
Packaged example sources have no configuration and give the defaults. If that first library's source is unreachable,
the next library with a `dta-app.yaml` supplies the configuration, or the defaults apply when none does, so a restriction set only in the
unreachable library is not enforced.

### When a change takes effect

The app reads the configuration together with the template index: after "Refresh templates", or once
`DTATOOLS_TEMPLATE_REFRESH_SECONDS` (default 900) have passed. Reload the page to see the new look.

### Validating the configuration

`validate_template()` checks the file with the same parser the app uses, so the two cannot disagree:

| Code | Severity | Meaning |
| --- | --- | --- |
| `app_config_invalid` | error | A value the app would reject: unreadable YAML, wrong `kind`, a bad colour, link or format, or a logo or `.docx` that is missing, too large, of the wrong type or outside the library. |
| `app_config_unknown_key` | warning | A key the app does not know. |
| `app_config_warning` | warning | A value the app accepts but adjusts, such as `create_blank: true` with `editing: false`. |
| `app_config_duplicate` | error | Both `dta-app.yaml` and `dta-app.yml` in the library. Keep one. |
| `app_config_misplaced` | error | A `dta-app.yaml` anywhere but the library root, for example inside `templates/`. Move it to the root. |
| `export_template_invalid` | warning | A file in `export-templates/` that is not a `.docx`; it is ignored. |

## Validating

Run validation from the repository root after changing any template file:

```sh
Rscript -e "DTAtools::validate_template('.', strict = TRUE)"
```

The directory form scans the `templates/`, `parties/`, `vocabulary/` and `export-templates/` folders of the library root, checks recognized files
and cross-file references, and dry-runs each concrete creation template. The scan is non-recursive. Always pass the library root; passing one of
the folders (`validate_template('templates')`) is rejected with `library_subfolder_given`.

For an interactive report rather than a strict failure, omit `strict = TRUE`:

```r
issues <- DTAtools::validate_template(".")
issues
```

The result is a data frame with `file`, `kind`, `id`, `version`, `severity`, `code`, and `message` columns. A zero-row result means no validator
issues were found. `kinds` can limit which file kinds are included in per-kind checks; it does not turn an empty directory into a successful
validation.

Strict mode aborts when one or more rows have severity `error`. Warnings remain visible in the report but do not alone make strict validation
fail.

### Linting the repository

`lint_template_repo()` is the check to run by hand and in CI. It returns every row `validate_template()` would, plus three repository-format
checks that need no template content to go wrong, only a file or folder in the wrong place:

```sh
Rscript -e "DTAtools::lint_template_repo('.')"
```

It prints one line per finding, then a count, and aborts (exit status 1 from `Rscript`) when any finding has severity `error`. To make warnings
fail as well, pass `fail_on = "warning"`; `fail_on = "none"` only reports. The findings are also returned invisibly, with the same columns as
`validate_template()`, for use in R.

| Code | Severity | Meaning | Typical fix |
| --- | --- | --- | --- |
| `folder_name_case` | error | A folder is spelled with different capitals than `templates`, `parties`, `vocabulary` or `export-templates`. A Linux server does not read it. | Rename it to the lowercase spelling (on a case-insensitive file system, rename it in two steps through a temporary name). |
| `id_filename_mismatch` | warning | A file's `id` is not the start of its file name, for example `acme.dta-party.yaml` holding `id: other_party`. A version or tag after the `id` (`biomarker_gf_2`, `id@1.0`) is fine. | Rename the file to `<id>.<suffix>`, or correct the `id`. |
| `file_ignored` | warning | A `.yaml`/`.yml` file in the repository root or in a kind folder declares a template `kind` but its file name lacks that kind's suffix, so the app never loads it. | Rename it to `<id>.<suffix>` (see "File naming conventions"). |

### CI

Use the GitHub Actions, Bitbucket Pipelines, or Jenkins file that matches the repository host. Each runs `lint_template_repo('.')`; keep its
working directory at the repository root so the linter sees the library folders.

`create_template_repo()` can scaffold this CI for supported providers. If the repository uses more than one CI system, keep each
configuration focused on the same command and avoid maintaining different rules per provider.

### Validator codes

This table lists every code emitted by `validate_template()` in the current validator, including the library-layout codes. Severity is the
literal severity reported in the result. A concrete template can receive `instantiate_failed` in addition to a more specific inheritance code.

| Code | Severity | Meaning | Typical fix |
| --- | --- | --- | --- |
| `parse_failed` | error | The template file cannot be parsed or read as YAML. | Fix indentation, quoting, or invalid YAML syntax. |
| `kind_unknown` | warning | The `kind` is not one of the four recognized kinds. | Use the correct kind value and matching filename suffix. |
| `kind_suffix_mismatch` | error | The declared `kind` differs from the kind the file name's suffix says, so the app reads the file as the suffix says, or skips it. | Make `kind` and the suffix agree, renaming the file or correcting `kind`. |
| `id_missing` | warning | The file has no non-empty `id`. | Add a stable identifier. |
| `version_missing` | warning | The file has no non-empty `version`. | Add a quoted version string. |
| `version_unquoted` | error | A numeric-looking version was not quoted and may have lost precision. | Quote it, for example `version: "1.10"`. |
| `version_unparseable` | warning | The version does not parse as a `numeric_version`. | Use a numeric-version-compatible string such as `"1.0"`. |
| `duplicate_id_version` | warning | Another file has the same kind, ID, and version. | Keep one authoritative file or assign a new version. |
| `no_templates` | error | None of `templates/`, `parties/`, `vocabulary/` and `export-templates/` holds a file. | Check the path and put the files directly in the folder for their kind. |
| `template_in_subdirectory` | warning | A template-shaped file is below the scan level of its folder and will not be loaded. | Move active files directly into the folder for their kind. |
| `template_misplaced` | error | A template file is outside the folder for its kind, for example at the repository root, or a party profile in `templates/`. | Move it into `templates/`, `parties/` or `vocabulary/` as its kind requires, and update related paths. |
| `library_subfolder_given` | error | The path given to the validator is itself `templates/`, `parties/` or `vocabulary/`. | Validate its parent, the library root; if it is a flat library, move its files into the kind sub-folders instead. |
| `target_invalid` | warning | An option target is unknown, malformed, or does not resolve in its dataset body. | Correct the target path and use a supported metadata or dataset field. |
| `target_machine_owned` | error | An option targets `metadata.template` or `metadata.import_issues`. | Remove the option/effect; these fields are written by DTAtools. |
| `prior_versions_invalid` | error | A `prior_versions:` entry lacks a version or changes text, has a bad date or unknown key, repeats a version, or is not older than the template's first version in the library. | Fix the entry; see "Versions from before the library: `prior_versions:`". |
| `party_slot_invalid` | warning | A party slot is malformed or names a profile ID not found in the library. | Check target, slot fields, role, and profile IDs. |
| `party_profile_invalid` | error | A party profile cannot be read the way the app reads it, for example because of an unknown `role`; the app skips it. The message is the reader's own. | Correct the field the message names. May appear next to `id_missing` or `version_missing`. |
| `vocab_slot_invalid` | error | A vocabulary slot is malformed, has a bad target, or has invalid selections. | Correct its target, vocabulary, mode, field, or term selections. |
| `vocab_slot_unresolved` | warning | The slot's vocabulary reference is not found in the validated directory. | Add the vocabulary file or correct the reference. |
| `dataset_template_unresolved` | warning | A `datasets[].template` reference does not resolve to a dataset template. | Correct the ID/version or add the referenced file. |
| `patch_incoherent` | warning | A dataset patch cannot be applied to the referenced dataset template. | Check operation order, IDs, and patch fields. |
| `dataset_missing` | error | A dataset template lacks its required singular `dataset:` body. | Add a dataset body; do not use creation-template `datasets:` here. |
| `dataset_id_missing` | error | A dataset template's body, or an inline dataset in a creation template, has no `id` (absent, null or empty). | Add an `id`; `as:` on an import does not replace it. |
| `dataset_field_invalid` | error | A dataset's `id`, `name` or `description` is a list, a sequence or a missing value. | Write a single text value. |
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
| `mixin_unresolved` | warning | A `mixins:` reference cannot be resolved in the validated library. | Add the mixin or correct its reference. |
| `mixin_invalid` | error | `mixins:` is malformed, or a named template is not a valid mixin (not abstract, or declares `extends`, `mixins`, `sealed`, `required`, `variants` or `order`, uses `remove:` or `null`). | Make `mixins:` a list of references, and fix the mixin to be abstract, flat and add-only. |
| `mixin_conflict` | error | A key, option, dataset or slot is contributed twice by the `extends:` chain and the mixins, or a mixin is already mixed in by an ancestor. | Write the value in the child itself, or drop the duplicate mixin. |
| `sealed_violation` | warning | A descendant changes a path sealed by an ancestor. | Remove the descendant change or revise the parent's seal. |
| `sealed_path_unknown` | warning | A sealed path does not match a value in the resolved template. | Correct the path or remove the ineffective seal. |
| `variant_invalid` | error | A creation template's `variants:` block is malformed: missing id or choices, an invalid or repeated choice value, a choice without datasets, or an id that collides with an option id. | Correct the block as described under `variants:`. |
| `instantiate_failed` | error | A concrete creation template cannot be built with its resolved defaults and minimum slot selections (for each `variants:` choice, if it has any). | Fix the template, referenced data, dataset definition, or required choices. |
| `app_config_invalid` | error | The library's `dta-app.yaml` holds a value the app would reject. | Correct the named key; see "App configuration". |
| `app_config_unknown_key` | warning | `dta-app.yaml` holds a key the app does not know. | Fix the spelling or remove the key. |
| `app_config_warning` | warning | `dta-app.yaml` holds a value the app adjusts. | Make the setting say what the app will do. |
| `app_config_duplicate` | error | Both `dta-app.yaml` and `dta-app.yml` exist in the library. | Delete one of them. |
| `app_config_misplaced` | error | A `dta-app.yaml` sits anywhere but the library root, for example inside `templates/`. | Move it to the repository root. |
| `export_template_invalid` | warning | A file in `export-templates/` is not a `.docx` and is ignored. | Remove it, or convert it to a `.docx`. |
| `export_template_no_placeholders` | warning | A `.docx` in `export-templates/` contains no `{PLACEHOLDER}` marker or is not a readable Word document, so the app does not offer it unless `dta-app.yaml` lists it (a listed file is not reported), and exporting through it inserts no DTA content. | Add the markers (see "Files the configuration refers to"), or remove the file. |

A validator code names a detected issue; the message column adds the actual file, path, or reference involved. For warnings on unresolved
references, remember that validation is scoped to the directory passed to the validator. A reference available only in a separate app source may
therefore need to be included in the repository being validated or handled by a repo-specific validation setup.

## Authoring workflow

Use a small, reviewable change process for each template update.

1. Create a branch from the repository's normal development branch.
2. Add a new template file or increment the version of a released contract.
3. Keep each file directly inside the folder for its kind, and `source:` data files beside the template that uses them in `templates/`.
4. Check every ID and version reference against the files in the library.
5. Run strict validation from the repository root.
6. Configure a local app source and preview the document when the change affects what authors see or what metadata is built.
7. Open a pull request with the purpose, affected template IDs, and expected behavior.
8. Ask the reviewer to check the YAML semantics, versioning, data privacy, and validation result.
9. Merge only after required checks pass.
10. Allow the deployed app to refresh its source, or trigger the deployment's normal refresh process.

A local preview source can be configured as:

```text
DTATOOLS_TEMPLATE_SOURCES=dev=dir:/path/to/your/clone
```

On systems where the app process uses R environment configuration, set the variable before launching the app. Do not store deployment tokens in
shell history or repository files.

### Pull-request review checklist

- Does each file's `kind` match its filename suffix (the linter checks this)?
- Is every `version` quoted?
- Is the file directly inside the folder for its kind (`templates/`, `parties/`, `vocabulary/`)?
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

1. The file is directly inside the folder for its kind (`templates/` for creation and dataset templates, `parties/`, `vocabulary/`), not in a
   subfolder, at the root, or in the folder of another kind. The app logs a warning for a misplaced file and the template picker notes it
   ("<source>: <path> - a party profile belongs in parties/ ...").
2. The suffix is one of the four supported suffixes and matches `kind`.
3. The file parses as a YAML mapping and has non-empty `kind`, `id`, and quoted `version` fields.
4. The app's configured source points to the library root (the directory holding `templates/`), not to one of the folders inside it.
5. The configured source name and scheme are spelled correctly.
6. No earlier source already supplies the same kind, ID, and version; source precedence can retain the first one.
7. A private-source configuration has not replaced packaged examples unless `DTATOOLS_TEMPLATE_INCLUDE_BUILTIN` is enabled.
8. For a Git source, allow the refresh interval to expire or trigger the normal refresh operation, then inspect the app's source warnings.

A nested file is not promoted into the catalogue merely because validation can report it. Move it up into its folder.

An empty template picker explains the expected folder layout; when it appears, check the folders first.

### The validator reports no templates

Confirm the current working directory, the path passed to `validate_template()`, and whether the files are one directory deeper than expected.
Validate the repository root, which holds `templates/`, `parties/`, `vocabulary/` and `export-templates/`. A typo or a library whose four folders
are all empty is an error (`no_templates`), not a successful empty check. If the path you gave is itself `templates/`, `parties/` or
`vocabulary/`, the validator reports `library_subfolder_given`: validate its parent instead.

### The validator reports misplaced files

`template_misplaced` means a template file is outside the folder for its kind, for example at the repository root or a party profile in
`templates/`. The app does not merge files from other places into a folder. Move each one into `templates/`, `parties/` or `vocabulary/`; see
"Reorganising an older repository". `app_config_misplaced` means `dta-app.yaml` is somewhere other than the repository root.

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

Word `.docx` export templates are not template kinds: the library's four indexed kinds are creation templates, dataset templates, party
profiles, and vocabularies. A `.docx` is picked up only from `export-templates/`, directly in it; one anywhere else is not offered. Every `.docx`
there that contains a `{PLACEHOLDER}` marker is offered automatically (non-`.docx` files, dotfiles and Word lock files such as `~$x.docx` are ignored, and the validator warns
`export_template_invalid` about a non-`.docx` file and `export_template_no_placeholders` about one without a marker). Keep `word_template` in `exports.document`. List a file under `exports.word_templates` in
`dta-app.yaml` (see "App configuration") to override its label or order, or to offer a file without markers, with a path relative to the root such as
`export-templates/report.docx`; a missing or misplaced file named there is reported as `app_config_invalid`.

## Reorganising an older repository

This section applies to an existing repository that keeps its template files at the root (the older flat layout), or keeps every kind of file
in one `templates/` folder. Nothing of those layouts is read any more: the files must sit in the folder for their kind.

1. Create the folders you need and move each file into the one for its kind:

   ```sh
   mkdir -p templates parties vocabulary export-templates
   git mv *.dta-template.yaml *.dta-dataset-template.yaml templates/
   git mv *.dta-party.yaml parties/
   git mv *.dta-vocabulary.yaml vocabulary/
   git mv *.docx export-templates/
   ```

   If every kind is currently inside `templates/`, move only the parties and vocabularies out of it: `git mv templates/*.dta-party.yaml parties/`
   and `git mv templates/*.dta-vocabulary.yaml vocabulary/`. Include the `.yml` variants. Skip any command with nothing to move.

2. Move `dta-app.yaml` to the repository root. In unreleased development builds it was kept inside `templates/`.
3. Keep each file referenced through a dataset `source:` entry beside the template that uses it, in `templates/`, and update the relative
   `source:` value if needed.
4. Word templates move to `export-templates/`. Every `.docx` there that contains a `{PLACEHOLDER}` marker is offered in the export dialog. If `dta-app.yaml` has an
   `exports.word_templates` list, change each `file:` to a path relative to the root, for example `export-templates/report.docx`.
5. Update repository scripts, tests, and CI commands that explicitly name template file paths. Commands that validate the repository root keep
   using `validate_template('.')`.
6. Validate from the repository root and inspect the report:

   ```sh
   Rscript -e "DTAtools::validate_template('.', strict = TRUE)"
   ```

   Resolve every `template_misplaced`, `template_in_subdirectory`, `app_config_misplaced` and `library_subfolder_given` row, and every cross-file
   or instantiation issue, before merging.
7. Preview the source in the app and confirm that all expected creation templates appear.
8. Merge the reorganised library at the moment the new DTAtools package version goes live; an older app cannot read it.
