# DTAtools R-Package

<!-- badges: start -->

[![R-CMD-check](https://github.com/Boehringer-Ingelheim/DTAtoolsR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Boehringer-Ingelheim/DTAtoolsR/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

<img src="img/dtatools_logo.png" alt="DTAtools Logo" width="900" />

**Data Transmission Agreements (DTAs)** and **Data Transmission Specifications
(DTS)** play a pivotal role in the secure and compliant exchange of data
between stakeholders — such as research organizations or vendors working with
pharma companies — particularly in clinical or regulatory environments where
data integrity and specification adherence are critical. However, manually
validating these data transmissions is error-prone and time-consuming, leading
to repeated submissions and significant time loss.

`DTAtools` streamlines the management of DTAs/DTS end to end. Specifications
are stored in human-readable **YAML**, offering flexibility and compatibility
across DTA/DTS scenarios. The package validates tabular data against those
specifications — types, nullability, allowed values, regex patterns, and
cross-column logic — and can also confirm that expected deliverable files were
received, are non-empty, and are readable.

`DTAtools` is open-sourced by Boehringer Ingelheim to give external and
internal collaborators an easy tool to validate data before transmission,
following the DevOps principle of detecting errors fast and automatically.

📖 **For a comprehensive walkthrough of every feature, see the
[full package vignette](vignettes/DTAtools.Rmd)** (`vignette("DTAtools")`
once installed).

## Features

- Import/export DTA/DTS specifications from/to YAML and Word documents
- Comprehensive validation of tabular data: type, format, nullability,
  allowed values, and regex patterns
- Cross-column schema rule validation (`col_condition`, `col_range`,
  `col_unique`)
- File-presence validation (`DTADataSetFile`) for non-tabular deliverables
- Detailed, queryable validation results (`results()`, `messages()`,
  `inspect()`)
- Export validated tables to disk with optional compression and MD5
  checksums
- Generate Word documentation tables directly from specifications

## Installation

For now, it is only possible to install the development version from GitHub:

```r
remotes::install_github("Boehringer-Ingelheim/DTAtoolsR")
```

## Quickstart

```r
library(DTAtools)

# 1. Load a full DTA/DTS definition from YAML
#    (metadata + dataset specs + file handlers + rules — all in one file)
dta_file <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
dta <- read_dta_from_yaml(dta_file)

# 2. Load the data file into the dataset defined in the YAML
csv_path <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
dta <- load_file(dta, dataset = "clinical_data", file = csv_path)

# 3. Validate — check() runs schema + rule validation for all datasets
dta <- check(dta)

# 4. Summarise results (one row per table)
results(dta)
```

## Usage

### Import DTA/DTS specifications

Specifications are stored in the human- and machine-readable YAML format,
either as dedicated spec files or nested inside a larger DTA/DTS YAML
document. A specification contains definitions of:

- **columns:** column names, types, and optionally patterns or allowed
  values. See [YAML Column Format](#yaml-column-format).
- **rules:** cross-column logic, e.g. "if column A is empty, column B must
  contain a value". See
  [YAML Schema Rule Specification](#yaml-schema-rule-specification).
- **metadata:** DTA/DTS metadata — title, version, contacts, transmission
  schedule. See [YAML Metadata](#yaml-metadata).

Import a standalone spec file (`columns:` + optional `rules:` at the
top level) with `import_specs_from_yaml()`:

```r
specs <- import_specs_from_yaml("spec.yaml")
```

For a full DTA/DTS document (with `metadata:` and `datasets:` sections), use
`read_dta_from_yaml()` instead — see the [Quickstart](#quickstart) above.

### Load and validate tabular data

Create a `DTADataSetTabular` object from column specs and one or more data
tables. `check()` validates every table against the column specs and rules
and returns the updated object — always assign the result back.

```r
col1  <- DTAColumnSpec(id = "STUDYID", type = "SAS Char", nullable = FALSE)
col2  <- DTAColumnSpec(id = "VISIT",   type = "SAS Char", nullable = TRUE)
specs <- DTAColumnSpecCollection(columns = list(STUDYID = col1, VISIT = col2))

table <- data.frame(STUDYID = c("1234", "1234"), VISIT = c("V01", "V02"))
data_obj <- DTADataSetTabular(name = "my_dataset", specs = specs,
                              tables = list(my_table = table))
data_obj <- check(data_obj)

results(data_obj)   # one row per table: status + error counts
messages(data_obj)  # one row per error: column, row, rule, message, id
```

### Validate file presence (non-tabular deliverables)

Use `DTADataSetFile` when a DTA/DTS requires that certain files (PDFs,
archives, reports, raw instrument output) simply arrive, are non-empty, and
are readable — without validating their internal structure.

```r
ds_file <- DTADataSetFile(
  name  = "delivery_check",
  paths = c("clinical_data.csv", "study_report.pdf")
)
ds_file <- check(ds_file)
results(ds_file)
```

### Write validated data to file

Use `write_table_to_file()` to export a validated table with optional
sorting, compression, and an MD5 checksum sidecar file. This ensures only
tables that have already passed validation are written to disk.

```r
write_table_to_file(
  DTADataSetTabular    = data_obj,
  table                = "my_table",
  filename             = "validated_table.tsv",
  arrange_by           = "STUDYID",
  sep                  = "\t",
  na                   = "",
  get_md5sum           = TRUE,
  write_md5sum_to_file = TRUE
)
```

## Additional features

### Export Spec Table to Word

Export the column specifications as a formatted table in a Word document —
ready to paste directly into a DTA/DTS document.

```r
export_specs_table(specs, "dta_spec_table.docx")
```

### Export Column Values Table

Export all allowed values of a specific column to a Word table — useful for
documenting controlled vocabulary.

```r
export_column_value_table(specs, "column_value_table.docx", id = "VISIT")
```

## YAML Column Format {#yaml-column-format}

Column specifications can contain:

- `id`: (mandatory) the column identifier
- `label`: (optional) a human-readable label
- `description`: (optional) free-text description
- `type`: (mandatory) the SAS-style type, e.g. `SAS Char`, `SAS Num`, `SAS Int`
- `format`: (optional) a SAS format string, e.g. `SAS 10.`, `SAS $10.`
- `length`: (optional) maximum character length
- `nullable`: (mandatory) whether the column may be empty (`true`/`false` or
  `Yes`/`No`)
- `pattern`: (optional) a regex the values must match
- `values`: (optional) a closed list of allowed values

> **Note:** `values` and `pattern` are mutually exclusive — a column has
> either a closed value list or a regex pattern, not both.

Example of a column `SUBJECT_ID` with a pattern:

```yaml
columns:
  - id: SUBJECT_ID
    label: Subject Identifier
    type: SAS Char
    nullable: false
    pattern: "^SUBJ[0-9]{4}$"
```

Example of a column `GENDER` with a closed value list:

```yaml
columns:
  - id: GENDER
    label: Gender
    type: SAS Char
    nullable: true
    description: "Self reported gender, empty if not reported"
    values:
      - "Male"
      - "Female"
      - "Other"
```

## YAML Schema Rule Specification {#yaml-schema-rule-specification}

`DTAtools` supports schema-based validation of tabular data using declarative
rules defined in YAML. Rules are evaluated after column-level validation and
allow complex inter-column logic to be enforced.

### Structure

Rules are defined under the top-level key `rules`. Each rule is a list item
with a required `id`, `type`, and additional fields depending on the rule
type.

### Supported Rule Types

#### `col_condition`

The most versatile rule type. When `condition` is satisfied for a row, the
`then` block is enforced on that same row.

Supported operators in both `condition` and `then`:

- `equals`: String / Number
- `not_equals`: String / Number
- `greater_equal`: Number
- `greater`: Number
- `less_equal`: Number
- `less`: Number
- `min` / `max`: Number range
- `in`: List of strings / numbers
- `not_in`: List of strings / numbers
- `empty`: Boolean
- `pattern`: Regex string

```yaml
rules:
  # IF VISIT == V03 THEN STATUS must be COMPLETED
  - id: v03_must_be_completed
    type: col_condition
    condition:
      VISIT:
        equals: V03
    then:
      STATUS:
        equals: COMPLETED

  # IF CONSENT == YES THEN CONSENT_DATE must not be empty
  - id: consent_requires_date
    type: col_condition
    condition:
      CONSENT:
        equals: "YES"
    then:
      CONSENT_DATE:
        empty: false

  # Multiple then-clauses: age range and weight lower bound
  - id: v03_full_checks
    type: col_condition
    condition:
      VISIT:
        equals: V03
    then:
      STATUS:
        equals: COMPLETED
      AGE:
        min: 18
        max: 65
      WEIGHT:
        greater_equal: 5

  # Pattern matching in condition
  - id: internal_supplier
    type: col_condition
    condition:
      GFREFID:
        pattern: "^Internal_[0-9]+$"
    then:
      GFNAM:
        equals: "Internal"
```

#### `col_range`

Ensures that all values in a numeric column fall within `[min, max]`.

```yaml
rules:
  - id: age_range
    type: col_range
    column: AGE
    min: 18
    max: 65
```

#### `col_unique`

Ensures that all combinations of values across one or more columns are
unique. Specifying multiple columns checks the composite key.

```yaml
rules:
  # Single column uniqueness
  - id: subject_unique
    type: col_unique
    column: SUBJECT_ID

  # Composite key uniqueness
  - id: subject_visit_unique
    type: col_unique
    column:
      - SUBJECT_ID
      - VISIT
```

## YAML Metadata {#yaml-metadata}

`metadata:` captures the administrative information of a DTA/DTS: title,
version, receiver/supplier contacts, and the transmission schedule.

```yaml
metadata:
  title: Clinical Data Transmission Agreement
  version: "1.0"
  date: 2026-01-15
  header: Boehringer Ingelheim

  receiver:
    affiliation:
      name: External CRO
      country: Germany
    contacts:
      - name: Alice Smith
        role: Lead Data Manager
        email: alice@cro.com
        signature: true
        reviewer: true

  supplier:
    affiliation:
      name: Pharma Company
      country: USA
    contacts:
      - name: Emily Turner
        role: Senior Data Manager
        email: emily@pharma.com
        signature: true

  transmission:
    type: Secure SFTP
    frequency: one-time
    notification: email
    date_first_transfer: 2026-02-01
    date_last_transfer: 2026-03-31
```

## Technical

`DTAtools` is built on the S7 object system and uses JSON Schema for fast,
row-level validation.

### Core Classes

| Class                     | Purpose                                                     |
|----------------------------|--------------------------------------------------------------|
| `DTA`                     | Top-level container: metadata + multiple datasets           |
| `DTAMetaData`             | Agreement metadata (title, version, contacts, transmission) |
| `DTADataSet`              | Abstract base class for all dataset types                   |
| `DTADataSetTabular`       | Dataset with column specs, rules, and data tables            |
| `DTADataSetFile`          | Dataset that checks file presence and readability only       |
| `DTAColumnSpecCollection` | Named list of column specs + optional rule collection         |
| `DTAColumnSpec`           | Single column definition (type, nullable, values, pattern)   |
| `DTARuleCollection`       | Container of rule objects                                    |
| `DTARuleColCondition`     | If/then cross-column rule                                     |
| `DTARuleColRange`         | Numeric range constraint for a column                        |
| `DTARuleColUnique`        | Uniqueness constraint (single or composite key)               |
| `DTAFileCSV` / `DTAFileTSV` | File handlers used by `read_file()` / `load_file()`         |

### Key Functions

| Function                       | Description                                              |
|----------------------------------|--------------------------------------------------------------|
| `read_dta_from_yaml(file)`     | Load a full DTA/DTS from YAML                             |
| `import_specs_from_yaml(file)`| Load column specs + rules from a standalone YAML          |
| `columns_specs_from_word(file)`| Import column specs from a Word table                     |
| `load_file(dta, dataset, file)`| Read a data file into a dataset using its YAML-defined handler |
| `check(x)`                    | Validate all datasets/tables; returns the updated object   |
| `results(x)`                   | Summary table: status and error counts per table            |
| `messages(x)`                  | Detailed error table: row, column, rule, message, id         |
| `inspect(x, id)`               | Deep detail for a specific error: row context, failing rows |
| `write_table_to_file(...)`     | Write a validated table to disk (TSV/CSV, gzip, MD5)         |
| `export_specs_table(x, file)`  | Export spec table to Word                                    |
| `export_column_value_table(x, file, id)` | Export a column's allowed values to Word           |

### Manually defining specs

```r
col1 <- DTAColumnSpec(id = "STUDYID", type = "SAS Char", nullable = FALSE)
col2 <- DTAColumnSpec(id = "VISIT",   type = "SAS Char", nullable = TRUE)
specs <- DTAColumnSpecCollection(columns = list(STUDYID = col1, VISIT = col2))
```

## Important Notes

- All rules must include a unique `id`.
- Rule `type` must match exactly one of the supported types
  (`col_condition`, `col_range`, `col_unique`).
- Missing or malformed rules trigger validation errors before data
  evaluation.
- `values` and `pattern` on a column spec are mutually exclusive.
- `check()` always returns the updated object — assign the result back
  (e.g. `x <- check(x)`).

## Credits

`DTAtools` was developed at Boehringer Ingelheim by:

- **Thomas Schwarzl** (aut, cre)
- **Daniel Schreyer** (aut, ctb)
