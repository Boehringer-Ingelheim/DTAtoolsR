# Writing a DTAtools Data Transfer Specification (YAML)

Self-contained authoring reference (DTAtools, `dev` branch, Nov 2026). A
specification is **one YAML file** that is both the human-readable Data
Transfer Agreement (DTA) document and the machine contract that every
delivered file is validated against. You will not have the package, its docs or
its website: everything you need is here. Follow it literally — the parser
rejects unknown keys.

**Your goal is a spec that would catch real data errors and that a reviewer
could sign.** That means: every column precisely typed and constrained, rules
that encode the actual domain logic, and metadata filled in as completely as
the information you have allows.

---

## 1. File skeleton

```yaml
metadata:   # agreement-level information (document header, parties, transfer)
  ...
datasets:   # one or more deliverables; each is validated independently
  - id: ...
    ...
```

Only these two top-level keys exist. `metadata` is technically optional but if
present `title` and `version` are **required** — always include it.

---

## 2. `metadata`

The key set is **closed**: any key not listed here is a hard error.

| Key | Type | Notes |
| --- | --- | --- |
| `title` | string, **required**, non-empty | Full agreement title. |
| `version` | string, **required**, non-empty | **Quote it**: `"1.0"` (unquoted `1.0` becomes a number). |
| `date` | ISO date `YYYY-MM-DD` | Date of this version. Anything else (a phrase, a date + words) is flagged as an import error. |
| `header` | string | Organisation / header text printed on the document. |
| `study` | mapping | Only `id` and/or `protocol_id`, each a non-blank string. No other keys. |
| `version_history` | list of records | Each record **must** have `version` (quoted string), `date` (ISO), `changes` (non-empty text). Oldest first; last entry = current `version`. |
| `receiver` | mapping | Party receiving the data. See parties below. |
| `supplier` | mapping | Party delivering the data. |
| `transmission` | mapping | How/when data moves. See below. |
| `error_handling` | string | Procedure when delivered data fails validation (who is notified, deadlines, re-delivery). |
| `authorized_for_corrections` | string or list of strings | Names/roles allowed to request corrections. |
| `sections` | mapping | Ordered free-text agreement sections. See below. |

Never write `template:` — it is machine-owned provenance.

### Parties (`receiver`, `supplier`)

```yaml
receiver:
  affiliation:
    name: Example Pharma GmbH        # name, address, country
    address: Example Str. 1, 55216 Ingelheim
    country: Germany
  contacts:
    - name: Jane Doe
      role: Lead Data Manager
      email: jane.doe@example.com
      phone: "+49 6132 000000"       # quote phone numbers
      department: Data Management
      address: ...                   # optional
      company: ...                   # optional, if differs from affiliation
      signature: true                # signs the agreement
      reviewer: true                 # reviews the agreement
      backup: false                  # deputy contact
```

Give every party at least one contact with `signature: true`, and name a
backup contact on each side when known.

### `transmission`

| Key | Value |
| --- | --- |
| `type` | channel, e.g. `secure SFTP`, `S3 bucket` |
| `frequency` | e.g. `one-time`, `monthly`, `after each database lock` |
| `notification` | how the receiver is told a delivery arrived |
| `test_upload` | `true`/`false` — a test transfer precedes production |
| `blinded_transfer` | `true`/`false` |
| `date_first_transfer`, `date_last_transfer` | ISO date **or** a free phrase (`"2 weeks after approval"`). Never a date followed by words (`"2026-12-31 at the earliest"`) — that is flagged as an error; write a pure phrase or a pure date. |
| `extent`, `scope`, `packaging`, `security`, `checksum`, `test_requirements`, `blinding_requirements` | single strings (blank `""` allowed only as a draft placeholder) |

### `sections` (agreement prose)

A mapping of `identifier → {title, text}`, kept in the written order.
Identifiers match `^[A-Za-z][A-Za-z0-9_]*$` and are unique. Each record has
**exactly** `title` (non-blank) and `text` (may be blank in drafts). Use a
block scalar for multi-line text:

```yaml
sections:
  purpose:
    title: Purpose
    text: |
      Defines the transfer of central-lab biomarker results for study X.
  data_protection:
    title: Data Protection
    text: Data are pseudonymised; no direct identifiers are transferred.
```

Good sections to write when you have the facts: purpose, scope, data
protection/pseudonymisation, quality control, change management, archiving.

---

## 3. `datasets`

A list. Each dataset:

| Key | Notes |
| --- | --- |
| `id` | **required**, unique, no spaces (`lab_results`). The key every validation message, `load_file()` call and `{COLUMN_SPECS:<id>}` placeholder uses. |
| `type` | **required**: `tabular` (parsed and validated) or `file` (only checked to exist, be readable and non-empty: PDFs, archives, images). |
| `name` | Short human-readable name (`Laboratory results`), shown in the app and as the heading in exported documents. Always write it. |
| `description` | Longer free text: what the deliverable contains and why. Write it whenever there is more to say than the name. |
| `files` | One file handler (mapping) or a list of handlers. |
| `columns` | `tabular` only. |
| `rules` | `tabular` only. |

No other keys (`template_source/version/date` are machine-written).

A file written before `id` existed carried the identifier in `name` and the
short label in `description`. Such an entry (`name` but no `id`) is still
read: `name` becomes the `id` and `description` the `name`. Write the
current shape in new files.

### `files` — handler keys

| Key | Notes |
| --- | --- |
| `type` | **required**: `csv`, `tsv`, `sas7bdat` (for `tabular`), `any` (for `file`). |
| `filename` | **required**. Literal name, or a regex if `pattern: true`. |
| `pattern` | `true` → `filename` is a regex. Anchor it: `"^LAB_[0-9]{8}\\.csv$"`. |
| `pattern_description` | Human wording of the regex (`LAB_<YYYYMMDD>.csv`). Always add it when `pattern: true`. |
| `number_of_files` | Exact count (default 1). **Or** use `min_number_of_files` / `max_number_of_files` — never both styles. |
| `info` | One-line description of the file. |
| `encoding` | default `UTF-8` (e.g. `latin1`). |
| `has_header` | default `true`. |
| `quote` | default `"`. |
| `missing_values` | Extra token meaning "missing" besides empty, e.g. `"."` or `"NA"`. |
| `newlines_in_values` | `true` if quoted cells may contain line breaks. |
| `extensions` | `any` only: allowed endings, e.g. `[pdf, zip]`. |

CSV is comma-separated, TSV tab-separated; `.gz` files are read transparently.

`sas7bdat` reads a SAS dataset (the haven package does the decoding). It takes
none of `encoding`, `has_header`, `quote`, `missing_values` and
`newlines_in_values` — passing one to the factory fails with an unused-argument
error — and the file is always read into memory, never streamed. A `sas7bdat`
handler declared inside a `type: file` dataset is coerced to a reader-less `any`
handler, since that dataset never parses its files.

---

## 4. `columns` (tabular datasets)

**The delivered table must contain exactly the declared columns** — a missing
column and an undeclared extra column are both errors. Declare every column.

| Key | Notes |
| --- | --- |
| `id` | **required**, exact header text, no whitespace, unique. |
| `label` | Short human label (≤ 40 chars, SAS-label style). |
| `description` | What the value means, its unit, origin, derivation, and when it may be blank. |
| `type` | `SAS Char`, `SAS Num` (decimal), `SAS Int` (whole number), `SAS Date`, `SAS Time`, `SAS DateTime`. Always prefixed `SAS `. |
| `format` | Optional SAS format, prefixed `SAS `: `$w.` (Char), `w.` / `w.d` / `BESTw.` (Num), `w.` / `BESTw.` (Int), `DATEw.`, `TIMEw.`, `DATETIMEw.`. Must fit the type. A declared format **is** checked: a value that cannot be displayed in it (too wide, too many decimals, not date/time display text) is reported. |
| `length` | Max length in characters (applies to text values). Together with a declared `format`, this is what enforces width. |
| `nullable` | `true` / `false`. **Always set it explicitly** — if omitted, missing values fail. |
| `values` | Closed list of permitted values (controlled terminology). |
| `pattern` | Regex every non-missing value must match (Perl syntax). **Unanchored by default — always wrap in `^…$`.** |
| `examples` | Example values; must match `pattern` if one is given. |
| `colclass` | Optional category: `patient_info`, `measurement_patient`, `measurement`, `visit_related`, `date_related`, `study_info`, `wide_format`, `long_format`, `wide_and_long_format`. |

Constraints between keys:

- `values` excludes both `pattern` and `examples` (pick one: closed list *or* pattern).
- `format`/`length` require `type`.
- `nullable: true` admits empty cells; with `values`, empty is allowed in addition to the list.

What is enforced per value: type (non-numeric text in a Num/Int column is an
import error; a decimal in an Int column fails), a declared `format`, `length`,
`values`, `pattern`, and missingness via `nullable`.

**`SAS Date`/`Time`/`DateTime` are stored as text and their shape is not checked
by the type** — add a pattern, e.g. ISO 8601 date
`"^[0-9]{4}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$"`.

For a `sas7bdat` delivery, a SAS date, datetime or time column arrives as the
display text of its declared format (`15AUG2025` under `DATE9.`) and as ISO 8601
text when no format is declared. Do **not** declare `length` on such a column:
it is checked against that text, so the `length: 8` that PROC CONTENTS reports
for the 8-byte numeric storage would fail every row.

### Making column specs meaningful

Every column should constrain *something* beyond its type. Pick the strongest
true statement:

1. **Finite code list** (sex, visit, test code, unit, flag) → `values`.
2. **Structured identifier or date** → anchored `pattern` (+ `examples`).
3. **Free text** → `type: SAS Char` + realistic `length`.
4. **Measurement** → `SAS Num`/`SAS Int`, plus a `col_range` rule for plausible bounds.
5. `nullable: false` for keys, identifiers and anything mandatory; `true` only
   when blank is legitimate — and then say *when* in the `description`.

Write descriptions that a data manager at the supplier can act on: unit, source
system, derivation, CDISC/SDTM variable it maps to, blank-when conditions.

---

## 5. `rules` (tabular datasets)

Cross-column and cross-row logic. Every rule has `id` (unique, no whitespace),
`type`, and should have a `description` stating the business rule in plain
words. Rules must only reference declared columns.

### `col_unique` — key uniqueness

```yaml
- id: unique_subject_visit_test
  type: col_unique
  description: One result per subject, visit and test.
  columns: [USUBJID, VISIT, LBTESTCD]   # the combination must be unique
```

### `col_range` — numeric plausibility (one column, inclusive bounds)

```yaml
- id: age_adult_range
  type: col_range
  description: Only adults up to 85 were enrolled.
  columns: AGE        # exactly one column
  min: 18             # min < max, both required
  max: 85
```

(`range: [18, 85]` is equivalent to `min`/`max`.)

### `col_condition` — row-level IF → THEN

For each row where **all** `condition` clauses hold, **all** `then` clauses must
hold. Missing values in THEN count as a violation.

```yaml
- id: not_done_requires_reason
  type: col_condition
  description: A test that was not done must state why and carry no result.
  condition:
    LBSTAT:
      equals: NOT DONE
  then:
    LBREASND:
      empty: false
    LBORRES:
      empty: true
```

Operators (several on one column are ANDed):

| Operator | Value | Meaning |
| --- | --- | --- |
| `equals` / `not_equals` | scalar | equality (numeric compare if both sides are numbers) |
| `in` / `not_in` | list | set membership |
| `greater`, `less`, `greater_equal`, `less_equal` | number | numeric comparison |
| `min` + `max` | numbers | one inclusive band (either alone also works) |
| `range` | `[lo, hi]` | inclusive band |
| `pattern` | regex | value matches (anchor it) |
| `empty` | `true` / `false` | value is / is not missing or blank |

Each column may appear only once per clause; merge its operators.

### `group_condition` — logic across the rows of a group

Define named row conditions, then constrain how they co-occur inside each group.

```yaml
- id: subject_consent_logic
  type: group_condition
  description: A subject with any consented record must have a consent date on every record.
  group_by: [USUBJID]
  conditions:
    c_consented:
      CONSENT: { equals: "YES" }
    c_has_date:
      CONSENT_DATE: { empty: false }
  constraints:
    - id: consent_needs_date
      type: requires            # alias: implies
      if: c_consented
      then: c_has_date
      if_scope: any             # any row in the group (default)
      then_scope: all           # every row in the group
      message: Consented subject has records without a consent date.   # optional
    # - id: ...
    #   type: mutually_exclusive   # alias: not_both
    #   left: c_a
    #   right: c_b
    #   left_scope: any
    #   right_scope: any
```

- `requires`: if (`if` holds per `if_scope`) then (`then` must hold per `then_scope`).
- `mutually_exclusive`: `left` and `right` must not both hold (per their scopes).
- Scopes: `any` (≥ 1 row in the group) or `all` (every row). Default `any`.
- Constraint `id`s unique within the rule; names in `if/then/left/right` must be defined under `conditions`.

### Writing meaningful rules

Encode what a reviewer would check by hand. Typical, high-value rules:

- **Primary key** uniqueness (`col_unique`) — every tabular dataset should have one.
- **Plausibility ranges** for every numeric measurement (`col_range`), from the protocol or physiology.
- **Status consistency**: result present ⇔ status done; not-done ⇒ reason given (`col_condition`).
- **Units**: result present ⇒ unit present; specific test code ⇒ specific unit (`in`).
- **Date logic** expressible as patterns/presence (end date present ⇒ start date present).
- **Per-subject/per-visit consistency** (`group_condition`): baseline exists for every subject; a sample cannot be both failed and reported.

Don't write rules that restate a column spec (a `values` list already enforces
membership) and don't write rules that are always true. Each rule's
`description` states the business reason.

---

## 6. YAML pitfalls

- **Quote anything that must stay text**: `"1.0"`, `"007"`, `"Y"`, `"N"`,
  `"YES"`, `"NO"`, `"ON"`, `"OFF"`, `"NA"`, phone numbers, codes with leading
  zeros. Unquoted `yes/no/on/off/y/n` become booleans, `1.10` becomes `1.1`.
- `values` for a text column: quote them all. For a numeric column, unquoted numbers.
- Regexes: in double quotes, backslashes are doubled (`"\\."`); in single
  quotes they are not (`'^\d+$'`). Prefer character classes (`[0-9]`) to avoid both.
- Dates: write `2026-03-01` (ISO). No other date format is understood.
- 2-space indentation, no tabs; lists with `- `; inline `[a, b]` is fine.

---

## 7. Complete example

```yaml
metadata:
  title: Central Laboratory Data Transfer Agreement – Study EX-101
  version: "1.1"
  date: 2026-03-01
  header: Example Pharma GmbH
  study:
    id: EX-101
    protocol_id: EX-101-PROT-02
  version_history:
    - version: "1.0"
      date: 2026-01-15
      changes: Initial version.
    - version: "1.1"
      date: 2026-03-01
      changes: Added LBREASND and the not-done rule.
  receiver:
    affiliation:
      name: Example Pharma GmbH
      address: Example Str. 1, 55216 Ingelheim
      country: Germany
    contacts:
      - name: Jane Doe
        role: Lead Data Manager
        email: jane.doe@example.com
        department: Data Management
        signature: true
        reviewer: true
      - name: Max Muster
        role: Backup Data Manager
        email: max.muster@example.com
        backup: true
  supplier:
    affiliation:
      name: CentralLab Ltd.
      country: United Kingdom
    contacts:
      - name: John Smith
        role: Data Transfer Lead
        email: john.smith@centrallab.example
        phone: "+44 20 0000 0000"
        signature: true
  transmission:
    type: secure SFTP
    frequency: monthly, plus final transfer after database lock
    notification: email to the receiver data manager
    test_upload: true
    blinded_transfer: false
    date_first_transfer: 2026-04-01
    date_last_transfer: 4 weeks after database lock
    packaging: One CSV per transfer, gzip-compressed
    security: Pseudonymised data only; SFTP with key authentication
    checksum: SHA-256 file delivered alongside each transfer
    test_requirements: Test file with dummy data must pass validation before the first production transfer
  error_handling: >-
    Files failing validation are rejected within 2 business days; the supplier
    re-delivers a corrected file within 5 business days.
  authorized_for_corrections: [Jane Doe, Max Muster]
  sections:
    purpose:
      title: Purpose
      text: Transfer of central laboratory results for study EX-101.
    data_protection:
      title: Data Protection
      text: Subjects are identified by USUBJID only; no direct identifiers are transferred.

datasets:
  - id: lab_results
    type: tabular
    name: Laboratory results
    description: Central laboratory results, one row per subject, visit and test.
    files:
      type: csv
      filename: "^EX101_LB_[0-9]{8}\\.csv$"
      pattern: true
      pattern_description: EX101_LB_<YYYYMMDD>.csv
      number_of_files: 1
      info: Cumulative lab results as of the transfer date.
    columns:
      - id: STUDYID
        label: Study Identifier
        type: SAS Char
        length: 6
        nullable: false
        values: ["EX-101"]
        description: Study identifier; constant.
      - id: USUBJID
        label: Unique Subject Identifier
        type: SAS Char
        length: 14
        nullable: false
        pattern: "^EX-101-[0-9]{3}-[0-9]{3}$"
        examples: ["EX-101-001-004"]
        description: STUDYID-SITE-SUBJECT, as assigned in the EDC.
      - id: VISIT
        label: Visit Name
        type: SAS Char
        nullable: false
        values: ["SCREENING", "WEEK 4", "WEEK 12", "END OF TREATMENT"]
        description: Protocol visit at which the sample was taken.
      - id: LBDTC
        label: Date of Specimen Collection
        type: SAS Date
        nullable: false
        pattern: "^[0-9]{4}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$"
        examples: ["2026-04-14"]
        description: Collection date, ISO 8601 (YYYY-MM-DD).
      - id: LBTESTCD
        label: Lab Test Short Name
        type: SAS Char
        length: 8
        nullable: false
        values: ["GLUC", "HBA1C"]
        description: SDTM LBTESTCD controlled terminology.
      - id: LBSTAT
        label: Completion Status
        type: SAS Char
        nullable: true
        values: ["NOT DONE"]
        description: '"NOT DONE" if no result could be obtained, otherwise blank.'
      - id: LBREASND
        label: Reason Not Done
        type: SAS Char
        length: 200
        nullable: true
        description: Why the test was not done; required when LBSTAT is "NOT DONE".
      - id: LBORRES
        label: Result in Original Units
        type: SAS Num
        nullable: true
        description: Numeric result; blank when LBSTAT is "NOT DONE".
      - id: LBORRESU
        label: Original Units
        type: SAS Char
        nullable: true
        values: ["mmol/L", "%"]
        description: Unit of LBORRES; blank when there is no result.
    rules:
      - id: unique_subject_visit_test
        type: col_unique
        description: One result per subject, visit and test.
        columns: [USUBJID, VISIT, LBTESTCD]
      - id: glucose_unit
        type: col_condition
        description: Glucose is reported in mmol/L.
        condition:
          LBTESTCD: { equals: GLUC }
          LBORRES: { empty: false }
        then:
          LBORRESU: { equals: mmol/L }
          LBORRES: { range: [0.5, 40] }
      - id: hba1c_unit
        type: col_condition
        description: HbA1c is reported in percent within a plausible range.
        condition:
          LBTESTCD: { equals: HBA1C }
          LBORRES: { empty: false }
        then:
          LBORRESU: { equals: "%" }
          LBORRES: { range: [3, 20] }
      - id: not_done_requires_reason
        type: col_condition
        description: A test not done carries a reason and no result.
        condition:
          LBSTAT: { equals: NOT DONE }
        then:
          LBREASND: { empty: false }
          LBORRES: { empty: true }
      - id: screening_glucose_per_subject
        type: group_condition
        description: Every subject has a glucose record at screening.
        group_by: [USUBJID]
        conditions:
          c_any_row:
            USUBJID: { empty: false }
          c_screening_glucose:
            VISIT: { equals: SCREENING }
            LBTESTCD: { equals: GLUC }
        constraints:
          - id: subject_needs_screening_glucose
            type: requires
            if: c_any_row
            then: c_screening_glucose
            if_scope: any
            then_scope: any

  - id: lab_manual
    type: file
    name: Laboratory manual
    description: Laboratory manual describing methods and reference ranges.
    files:
      type: any
      filename: "^EX101_LabManual_v[0-9]+\\.pdf$"
      pattern: true
      pattern_description: EX101_LabManual_v<N>.pdf
      number_of_files: 1
      extensions: [pdf]
```

---

## 8. Final checklist

- [ ] Only allowed keys; `metadata.title` and quoted `metadata.version` present.
- [ ] `date`, `study`, `version_history`, both parties with signing contacts,
      `transmission`, `error_handling`, `authorized_for_corrections` and
      `sections` filled from the available information. Do not invent facts —
      if something is unknown, leave it out (or use a blank text section as a
      visible draft placeholder) rather than guessing.
- [ ] Every dataset has an `id` and a `name` (and a `description` where there is more to say); every file pattern is anchored and has a `pattern_description`.
- [ ] Every delivered column is declared, with `label`, `type`, explicit `nullable`, `description`.
- [ ] Every column carries `values`, an anchored `pattern`, or a `length`; dates have a pattern.
- [ ] `values` and `pattern` never on the same column; text codes quoted.
- [ ] Each tabular dataset has a key `col_unique` rule, ranges for measurements,
      and conditional rules for status/unit/result dependencies — each with a `description`.
- [ ] Rule and constraint ids unique, no whitespace; rules reference declared columns only.
