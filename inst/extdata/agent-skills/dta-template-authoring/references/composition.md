# Composition: extends, mixins and the rest

How to reuse content without copying it. Field syntax is in the README
sections "Inheritance, deviation templates and sealing", "Mixins", "Patches"
and "Controlled vocabularies"; this file is about choosing and combining them.

## Contents

- [extends or mixins](#extends-or-mixins)
- [Designing the extends chain](#designing-the-extends-chain)
- [Designing mixins](#designing-mixins)
- [sealed and required](#sealed-and-required)
- [Overriding: values, collections and verbs](#overriding-values-collections-and-verbs)
- [Datasets: reuse without forking](#datasets-reuse-without-forking)
- [Vocabularies](#vocabularies)
- [variants](#variants)
- [A worked family](#a-worked-family)
- [Anti-patterns](#anti-patterns)

## extends or mixins

`extends:` says "this **is a variant of** that template". `mixins:` says
"this **also contains** these blocks".

| | `extends:` | `mixins:` |
| --- | --- | --- |
| How many | exactly one parent | any number, order irrelevant |
| What the source must be | any creation template, abstract or concrete | `abstract: true`, with `base:` (`base: {}` is enough) |
| What it may declare | anything, including `sealed`, `required`, `variants` | only `base`, `options`, `datasets`, `party_slots`, `vocabulary_slots`; no `extends`, `mixins`, `sealed`, `required`, `variants`, `order`, no `remove:`, no `null` |
| On overlap | the child wins | any key or id contributed twice (by the chain and a mixin, or by two mixins) is a `mixin_conflict` |
| Header fields (`label`, `description`) | inherited | not copied |
| Descendants | inherit everything | inherit the mixed-in content but not the `mixins:` key; listing a mixin an ancestor already mixes in is a conflict |
| Nesting | a chain, at most 8 deep | none; a mixin references no template |

Rules of thumb:

- If the shared content is "the same agreement with some differences", it is
  a parent. If it is "a block that templates of unrelated families also
  need", it is a mixin.
- If a block must be able to *replace* something, it cannot be a mixin. Put
  the default in the parent and let the child override it.
- A template may use both: `extends: company_dta@latest` plus
  `mixins: [mixin_privacy_eu@latest, mixin_assay_biomarker@latest]` is the
  normal shape of a family.

## Designing the extends chain

- **Company standard** (`abstract: true`): what every DTA shares. Base
  metadata (title pattern, header, receiver, error handling, agreement
  `sections`), the common options, the party slots, and the governance rules
  (`sealed:`, `required:`). Usually no datasets, which is why it is abstract;
  abstract templates are never offered in the picker and get no template
  guide of their own.
- **Family**: one per kind of transfer (an assay type, central lab, ePRO).
  Concrete when it is usable as it stands; abstract when every use needs a
  deviation to complete it.
- **Deviation**: only the differences for one supplier, study or region.
  Always its own `label` and `description`, because header fields are
  inherited and the parent's text would otherwise appear in the picker.

## Designing mixins

- **One topic, self-contained.** A mixin for an assay carries the dataset, the
  vocabulary slot on its column and the options for it together. A consumer
  then gains or drops the whole capability with one line.
- **Disjoint by construction.** `base:` merges leaf by leaf: two mixins may
  each add a different `metadata.sections.<key>` or
  `metadata.transmission.<field>`, but the same leaf twice is a conflict, and
  so is any option id, slot id or dataset `as:` the chain or another mixin
  already has. Prefix every id inside a mixin with its topic.
- **Never a second source of a default.** If the standard sets
  `transmission.security`, a mixin that also sets it is a conflict, not an
  override. Decide which layer owns each leaf.
- **Order.** Agreement `sections` reach the document with the `extends:`
  chain's sections first and mixin-provided ones after them (in the worked
  family below: Responsibilities, then Data protection). When the order
  matters, check it in the template guide.
- **Mixins can satisfy requirements.** A standard that declares
  `required: [base.metadata.transmission.security]` is satisfied by a concrete
  template that mixes in a block providing it. This is a clean way to force
  every concrete DTA to choose one of several transfer blocks.
- **Seals apply.** Mixin content is checked against the `sealed:` paths of the
  consuming template's ancestors, as if the child had written it.
- **Consumers may still adjust.** A template that mixes a block in can modify
  or remove the block's entries with the collection verbs below; do not copy a
  mixin to change one value.

## sealed and required

`sealed:` lists paths no descendant may change: use it for what governance
owns (regulated wording, error handling, the receiver block). `required:`
lists paths that must have a value when a document is created: use it for what
each concrete template, or the author, must supply.

Path grammar (both lists):

| Path | Means |
| --- | --- |
| `base.metadata.error_handling` | a metadata leaf; any depth (`base.metadata.sections.responsibilities`) |
| `options.header` | the whole option with `id: header` |
| `options.header.default` | one field of that option |
| `datasets.samples` | the dataset entry with `as: samples` |
| `party_slots.supplier`, `vocabulary_slots.samples_status` | the slot with that `id` |

- Always write the `base.` prefix. `metadata.error_handling` resolves to
  nothing, protects nothing, and only earns a `sealed_path_unknown` warning.
- A seal binds descendants, not the template that declares it, and seals
  accumulate down the chain; they cannot be lifted by a child.
- `required:` is checked when a concrete template is built, so an abstract
  standard can demand what only its descendants provide. Base values, option
  defaults, mixins and (in the app) the author's choices all count.
- Lint dry-runs each concrete template with its option defaults, the minimum
  vocabulary-slot selections, and, for every party slot with
  `required: true`, the first profile the app would offer for it. Optional
  party slots stay empty, so a `required:` path that only an optional party
  selection fills fails lint. A required slot that no profile in the library
  can fill is `instantiate_failed` ("Party slot … is required, and no profile
  was selected for it"): add or allow the profile; do not delete the
  requirement to silence it.

## Overriding: values, collections and verbs

Values, at any depth of a child template:

| Child writes | Effect |
| --- | --- |
| nothing (key omitted) | inherit |
| a value | override; mappings merge key by key |
| `""`, `{}`, `[]` | explicitly empty |
| `null` | remove the inherited key |

Sequences (a column's `values`, `choices`, `contacts`) are replaced whole, not
merged by position.

Keyed collections (`options`, `datasets`, `party_slots`,
`vocabulary_slots`, and a patch's `columns`) take either a bare list or a verb
mapping:

```yaml
options:
  modify:
    - id: header
      default: ACME Labs
  remove: [privacy_clause]
  add:
    - id: acme_reference
      label: ACME project reference
      type: text
      target: metadata.study.protocol_id
  order: [title, acme_reference, header]
```

Verbs run as `inherit` (default `all`; `none` or a list of ids to keep),
`remove`, `add`, `modify`, `order`. **Use verbs in every child.** With a verb,
a typo fails loudly: `modify: [{id: headr}]` reports "Cannot modify "headr" in
options: no such entry is inherited". In the bare-list form the same typo
silently adds a second option named `headr` and leaves `header` unchanged; no
check notices. `options: []` and `options: {inherit: none}` both drop every
inherited option.

Lint checks a slot a child modifies as merged with the slot it inherits, so a
`modify:` entry needs only the fields it changes. A finding in an inherited
slot is reported once, on the file that wrote it.

Identity: options and slots by `id`; dataset entries by `as` (then `source`,
then the dataset's own `id`).

## Datasets: reuse without forking

- Reference dataset templates (`template: ds@…`, `as: <dataset id>`); give the
  importing entry static `options:` values for the dataset template's own
  options.
- A `patch:` adapts an import for one consumer. Operations apply in the order
  remove → add → modify columns → `set`; `set` paths are relative to the
  dataset body and address columns and rules by id, never by position.
- **Pin the dataset template you patch.** The patch names columns and is only
  known to fit the version it was checked against.
- Fork into a new dataset template only when the patch would rewrite more
  than it keeps, or when several templates need the same patch (then that
  patched result is a dataset template of its own).
- A dataset template's `options:` are never shown to the author; only the
  creation template's options are. To let the author choose something about a
  dataset, use a creation-template option or a vocabulary slot targeting it.

## Vocabularies

- One concept per vocabulary. A code/decode pair is one vocabulary: `code`
  for the `--TESTCD` column, `label` for `--TEST`, bound with `field: label` on
  the second; declare both under `columns:` so the app's column editor
  suggests the vocabulary.
- Codes are identifiers: never recode a released term silently (major
  version). Quote codes YAML could coerce (`"Y"`, `"N"`, `"007"`).
- Narrow per column with `values_from: {vocabulary: x@latest, include: […]}`.
  Narrow per document, when the author knows which codes apply, with a
  `vocabulary_slots:` entry on the creation template. Narrow for the whole
  organisation with a child vocabulary: `extends:` + `remove_terms`/
  `add_terms` (a non-empty `terms:` in the child replaces the inherited list
  entirely).
- `values_from` and `pattern` are mutually exclusive; a literal `values` next
  to `values_from` is ignored (and warned about). Never keep both.
- `columns:` is a suggestion hint only and is not inherited through
  `extends:`.

## variants

Use `variants:` for mutually exclusive record structures inside one agreement
(one dataset layout per trial platform). The author must pick one; each choice
supplies its own `datasets:` (replacing, not appending) and may add `base:`,
`options:`, `label` and a `source:` naming the external document version.
Prefer variants over N sibling templates when the agreement is the same and
only the structure differs; prefer separate deviations when the agreements
differ. Lint dry-runs every choice.

## A worked family

This library lints clean with `fail_on = "warning"`: a vocabulary, two party
profiles, a dataset template, an abstract standard (with a frozen earlier
release), two mixins, a concrete family template and a supplier deviation.
The package's own tests rebuild it from this page and validate it.

`vocabulary/sample_status.dta-vocabulary.yaml`

```yaml
kind: dta_vocabulary
id: sample_status
label: Sample status
version: "1.0"
type: text
columns: [STATUS]
terms:
  - code: RECEIVED
    label: Received
  - code: ANALYSED
    label: Analysed
  - code: NOT DONE
    label: Not done
```

`parties/supplier_acme.dta-party.yaml` (both profiles name role mailboxes,
not people)

```yaml
kind: dta_party_profile
id: supplier_acme
version: "1.0"
role: supplier
label: ACME Labs
affiliation:
  name: ACME Labs
  country: DE
contacts:
  - name: Data Management Team
    role: Data Manager
    email: dm@acme.example
```

`parties/receiver_examplepharma.dta-party.yaml`

```yaml
kind: dta_party_profile
id: receiver_examplepharma
version: "1.0"
role: receiver
label: Example Pharma
affiliation:
  name: Example Pharma
contacts:
  - name: Trial Data Management
    role: Trial Data Manager
    email: tdm@pharma.example
```

`templates/biomarker_samples.dta-dataset-template.yaml`

```yaml
kind: dta_dataset_template
id: biomarker_samples
label: Biomarker samples
version: "1.0"
dataset:
  id: samples
  type: tabular
  name: Biomarker sample results
  description: One row per sample.
  files:
    filename: samples.tsv
    type: tsv
  columns:
    - id: STUDYID
      label: Study identifier
      type: SAS Char
      format: SAS $20.
      nullable: No
    - id: SAMPLEID
      label: Sample identifier
      type: SAS Char
      format: SAS $20.
      nullable: No
    - id: STATUS
      label: Sample status
      type: SAS Char
      format: SAS $12.
      nullable: No
      values_from: sample_status@latest   # follow: new statuses reach new DTAs
  rules:
    - id: sample_unique
      type: col_unique
      columns: [SAMPLEID]
```

`templates/company_dta.dta-template.yaml` (the head; its released `"1.0"` sits
beside it, frozen, as `company_dta@1.0.dta-template.yaml`)

```yaml
kind: dta_creation_template
id: company_dta
label: Company DTA standard
version: "1.1"
abstract: true
description: Governance-owned base for every DTA this library creates.

base:
  metadata:
    title: DATA TRANSFER SPECIFICATION
    version: "1.0"
    date: "${today}"
    header: Example Pharma
    version_history:
      - version: "${version}"
        date: "${today}"
        changes: Initial template-generated version
    error_handling: >-
      Format or content errors are reported by email to the authorized
      contacts and corrected in a follow-up transfer.
    sections:
      responsibilities:
        title: Responsibilities
        text: The receiver reviews delivered files and reports discrepancies.

options:
  - id: title
    label: Document title
    type: text
    target: metadata.title
  - id: header
    label: Header / organisation
    type: text
    target: metadata.header

party_slots:
  - id: supplier
    label: Supplier
    target: metadata.supplier
  - id: receiver
    label: Receiver
    target: metadata.receiver
    profiles: [receiver_examplepharma]

# Governance owns this wording; no family or deviation may change it.
sealed:
  - base.metadata.error_handling
  - base.metadata.sections.responsibilities

# Every concrete DTA must state how data is secured; a transfer block
# (mixin) or the concrete template itself provides it.
required:
  - base.metadata.transmission.security
```

`templates/mixin_privacy_eu.dta-template.yaml`

```yaml
kind: dta_creation_template
id: mixin_privacy_eu
label: EU data-protection clauses
version: "1.0"
abstract: true

base:
  metadata:
    sections:
      data_protection:            # a different leaf from the standard's
        title: Data protection
        text: Personal data is pseudonymised before transfer and processed under the GDPR.
    transmission:
      security: Encrypted in transit and at rest.   # satisfies company_dta's required:

options:
  - id: privacy_clause            # topic-prefixed id
    label: Data-protection clause
    type: textarea
    target: metadata.sections.data_protection.text
```

`templates/mixin_assay_biomarker.dta-template.yaml`

```yaml
kind: dta_creation_template
id: mixin_assay_biomarker
label: Biomarker sample dataset
version: "1.0"
abstract: true

base:
  metadata:
    transmission:
      checksum: SHA-256 checksum file delivered with every transfer.

datasets:
  - template: biomarker_samples@latest
    as: samples

vocabulary_slots:
  - id: samples_status
    label: Sample statuses used in this transfer
    target: datasets.samples.columns.STATUS.values
    vocabulary: sample_status@latest
    mode: closed
    min: 1
```

`templates/biomarker_study.dta-template.yaml` — the family is nothing but
composition:

```yaml
kind: dta_creation_template
id: biomarker_study
label: Biomarker study DTA
version: "1.0"
description: Biomarker sample transfer under the company standard, with EU data protection.
extends: company_dta@latest
mixins:
  - mixin_privacy_eu@latest
  - mixin_assay_biomarker@latest
```

`templates/biomarker_study_acme.dta-template.yaml` — the deviation states only
what differs:

```yaml
kind: dta_creation_template
id: biomarker_study_acme
label: Biomarker study DTA (ACME Labs)
version: "1.0"
description: ACME Labs deviation of biomarker_study, with ACME's own sample identifier.
extends: biomarker_study@latest   # follow: ACME gets standard updates

options:
  modify:
    - id: header
      default: ACME Labs

party_slots:
  modify:
    - id: supplier
      profiles: [supplier_acme]   # profile ids only, never id@version

datasets:
  modify:
    - as: samples
      template: biomarker_samples@1.0   # pinned: the patch below names columns
      patch:
        add_columns:
          - id: ACMEID
            label: ACME internal identifier
            type: SAS Char
            format: SAS $20.
            nullable: Yes
```

## Anti-patterns

| Instead of | Do |
| --- | --- |
| Copying a template to change two defaults | A child with `extends:` and `options: {modify: …}` |
| A parent that half its children have to undo with `null` | Split the optional half into a mixin |
| A mixin that "overrides" a parent value | Move the default into the parent; override in the child |
| Two mixins that both set the same leaf or id | Give the leaf one owner; prefix ids by topic |
| Bare-list collections in a child | Verbs (`modify:`, `add:`, `remove:`) |
| `sealed: [metadata.title]` | `sealed: [base.metadata.title]` |
| The same code list as literal `values:` in several dataset templates | One vocabulary, `values_from:` everywhere |
| A patched dataset import with `@latest` | Pin it |
| `profiles: [supplier_acme@1.0]` | `profiles: [supplier_acme]` |
| Organising files in sub-folders or an `archive/` folder | Flat folders, ids that sort, frozen files beside the head |
| A deviation without its own `description` | Always write `label` and `description` |
| Template change notes in `base.metadata.version_history` | The PR description; the app computes template history |
