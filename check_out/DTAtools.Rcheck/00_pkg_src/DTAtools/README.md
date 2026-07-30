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

## Package Architecture

`DTAtools` organises objects into a layered hierarchy that maps directly to
how a Data Transmission Specification (DTS) is structured — a top-level `DTA`
holding metadata and one or more datasets, each either fully-validated
tabular data (`DTADataSetTabular`) or a simple file-presence check
(`DTADataSetFile`):

<p align="center">
  <img src="img/DTAtools_architecture.svg" alt="DTAtools package architecture diagram" width="100%" />
</p>

<details>
<summary>Plain-text version (ASCII)</summary>

```
DTA                              ← top-level agreement container
├── metadata                     ← title, version, receiver/supplier
|   (DTAMetaData)                     contacts, transmission schedule, ..
│                                     
└── datasets (named list of DTADataSet)  ← one or more datasets per DTS
    │
    ├── DTADataSetTabular        ← tabular data: for tables with
    |   |                            full column-spec + rule validation
    │   ├── files (list)         ← expected file handlers for various  
    |   |   |                        input types, manage file names, 
    │   │   ├── DTAFileCSV           which can have patterns, 
    |   |   ├── DTAFileTSV           number of files, etc ..
    |   |   └── DTAFileDelim
    │   │       └── read_file()  → Arrow Table
    |   |
    │   ├── specs (DTAColumnSpecCollection)  ← column-level schema +
    |   |   |                                   cross-column rules
    │   │   |
    │   │   ├── columns (named list of DTAColumnSpec)
    |   |   |   └── DTAColumnSpec × N   ← one per column: type, nullable,
    │   │   │                            values/pattern, length, format
    |   |   └── rules (list of DTARule)
    │   │       ├── DTARuleColCondition × N  ← if/then cross-column logic
    │   │       ├── DTARuleColRange × N      ← numeric range constraint
    │   │       └── DTARuleColUnique × N     ← uniqueness (single or 
    |   |                                              composite key)
    |   |
    │   └── tables (named list)  ← actual data as Arrow Tables
    │       └── <table_name>     ← populated via load_file() or constructor
    │
    └── DTADataSetFile           ← non-tabular: file presence/readability check
        ├── file_paths           ← character vector of expected file paths
        └── files (list)         ← DTAFile descriptors (derived from file_paths)
```

</details>

**Validation flow**, regardless of which object type you use:

1. `load_file(dta, dataset, file)` — reads a file into the dataset using its file handler
2. `check(dta)` / `check(ds)` — runs JSON Schema validation + rule checks; always returns the updated object
3. `results(x)` — one-row-per-table summary (pass/fail, error counts)
4. `messages(x)` — one-row-per-error detail table (column, row, rule, message, `id`)
5. `inspect(x, id = N)` — deep-dive into a specific error message by its `id`

📖 See the [vignette's Package Architecture section](vignettes/DTAtools.Rmd) for the fully annotated diagram plus a node-by-node explanation.

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

Not every delivery needs a full DTA with metadata. When you only need to
validate *one* dataset, read the dataset definition directly with
`read_dataset_from_yaml()` — a standalone dataset YAML has the same shape as
one entry under a DTA's `datasets:` list (`name`, `type`, `files`, `columns`,
`rules`), just without the surrounding `metadata:` wrapper:

```r
# dataset.yaml has: name, type, files, columns, rules — no top-level metadata
ds <- read_dataset_from_yaml("dataset.yaml")

ds <- load_file(ds, file = csv_path, handler_index = 1)
ds <- check(ds)
results(ds)
```

## Usage

### Load specifications and datasets from YAML

Specifications are stored in the human- and machine-readable YAML format. A
specification contains definitions of:

- **columns:** column names, types, and optionally patterns or allowed
  values. See [YAML Column Format](#yaml-column-format).
- **rules:** cross-column logic, e.g. "if column A is empty, column B must
  contain a value". See
  [YAML Schema Rule Specification](#yaml-schema-rule-specification).
- **metadata:** DTA/DTS metadata — title, version, contacts, transmission
  schedule. See [YAML Metadata](#yaml-metadata).

There are three ways to bring a specification into R, from most to least
common:

1. **A full DTA/DTS document** (`metadata:` + one or more `datasets:`) —
   `read_dta_from_yaml()`. The recommended entry point for production use;
   see the [Quickstart](#quickstart) above.
2. **A single, self-contained dataset definition** (`name`, `type`, `files`,
   `columns`, `rules` — no top-level `metadata:`) — `read_dataset_from_yaml()`.
   Ideal when you only need to validate one dataset, e.g. while iterating on
   a spec. Also shown in the [Quickstart](#quickstart) above.
3. **A bare list of column specs** (just `columns:` plus optional `rules:`,
   no `name`/`type`/`files`) — `import_specs_from_yaml()`. The most manual
   option: you build the `DTADataSetTabular` wrapper yourself.

```r
# Use case 3: bare specs only — you supply name/type/data yourself
specs <- import_specs_from_yaml("spec.yaml")
```

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

### Inspecting results in depth

`results()` and `messages()` cover most needs, but `messages()` gives you a
row per error with an `id` column. When you want the full story behind a
specific error — the exact cell value, the row in context, the JSON Schema
constraint that failed, or the rows that violated a rule — call
`inspect(x, id = N)`. `validation_status()` gives a compact status data frame
when you just need pass/fail counts.

```r
results(data_obj)                 # per-table pass/fail summary
validation_status(data_obj)       # compact status data frame
msgs <- messages(data_obj)        # per-error table — note the `id` column
inspect(data_obj, id = msgs$id[1]) # deep detail for that error: row context,
                                    # schema clause, or failing rows for rules
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

### Building a full `DTA` object, mixing dataset types

A single `DTA` object can hold **any mix** of `DTADataSetTabular` and
`DTADataSetFile` datasets alongside its `metadata`, letting you validate an
entire delivery — column-level content *and* file presence — in one
`check()` call. This is exactly what `read_dta_from_yaml()` builds for you
under the hood when loading a full DTA YAML.

```r
# Tabular dataset (column spec validation)
ds_tab <- DTADataSetTabular(
  name   = "clinical_data",
  specs  = specs,
  tables = list(clinical_data = read.csv("clinical_data.csv"))
)

# File dataset (presence check)
ds_fi <- DTADataSetFile(
  name  = "delivery_manifest",
  paths = c("clinical_data.csv", "study_report.pdf")
)

dta_mixed <- DTA(
  datasets = list(ds_tab, ds_fi),
  metadata = DTAMetaData(title = "Mixed DTA", version = "1.0")
)

dta_mixed <- check(dta_mixed)
results(dta_mixed)          # one row per dataset

# Access datasets by name or index
datasets(dta_mixed, "clinical_data")
dta_mixed[["delivery_manifest"]]
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

## YAML Column Format

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

## YAML Schema Rule Specification

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

## YAML Metadata

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
| `DTAColumnSpecCollection` | Named list of column specs + optional list of rules           |
| `DTAColumnSpec`           | Single column definition (type, nullable, values, pattern)   |
| `DTARuleColCondition`     | If/then cross-column rule                                     |
| `DTARuleColRange`         | Numeric range constraint for a column                        |
| `DTARuleColUnique`        | Uniqueness constraint (single or composite key)               |
| `DTAFileCSV`              | CSV file handler for `read_file()`                            |
| `DTAFileTSV`              | TSV file handler for `read_file()`                            |
| `DTAFileDelim`            | Generic delimited-text file handler for `read_file()`         |

### Key Functions

| Function                       | Description                                              |
|----------------------------------|--------------------------------------------------------------|
| `read_dta_from_yaml(file)`     | Load a full DTA/DTS (metadata + datasets) from YAML       |
| `read_dataset_from_yaml(file)` | Load a single, self-contained dataset definition from YAML |
| `import_specs_from_yaml(file)`| Load bare column specs + rules from a standalone YAML      |
| `columns_specs_from_word(file)`| Import column specs from a Word table                     |
| `load_file(dta, dataset, file)`| Read a data file into a dataset using its YAML-defined handler |
| `read_file(handler, file)`     | Read a file using a file handler (`DTAFileCSV`, etc.)       |
| `check(x)`                    | Validate all datasets/tables; returns the updated object   |
| `results(x)`                   | Summary table: status and error counts per table            |
| `messages(x)`                  | Detailed error table: row, column, rule, message, id         |
| `inspect(x, id)`               | Deep detail for a specific error: row context, failing rows |
| `validation_status(x)`         | Compact status data frame                                   |
| `validation_errors(x, table)`  | Full raw error output for one table                          |
| `datasets(x, name)`            | Extract dataset(s) from a DTA object                         |
| `tables(x, i)`                 | Extract table(s) from a DTADataSetTabular                    |
| `get_table(x, id)`             | Extract a single Arrow Table by name or index                |
| `labels(x)`                    | Names of all tables in a DTADataSetTabular                   |
| `specs(x)`                     | DTAColumnSpecCollection from a DTADataSetTabular              |
| `colspec(x, id)`               | Single DTAColumnSpec by column ID                            |
| `rules(x)`                     | List of DTARule objects from a collection or dataset          |
| `metadata(x)`                  | DTAMetaData from a DTA object                                |
| `write_table_to_file(...)`     | Write a validated table to disk (TSV/CSV, gzip, MD5)         |
| `write_columns_to_yaml(x, file)` | Serialise specs to YAML                                    |
| `write_columns_to_json(x, file)` | Serialise specs to JSON                                    |
| `export_specs_table(x, file)`  | Export spec table to Word                                    |
| `export_column_value_table(x, file, id)` | Export a column's allowed values to Word           |
| `as_json_schema(x)`            | Convert specs to a JSON Schema string                        |

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
