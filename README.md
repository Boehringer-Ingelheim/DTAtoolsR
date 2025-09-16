# DTAtools R-Package

<!-- badges: start -->

[![R-CMD-check](https://github.com/Boehringer-Ingelheim/DTAtoolsR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Boehringer-Ingelheim/DTAtoolsR/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

<img src="img/dtatools_logo.png" alt="DTAtools Logo" width="900" />

Data Transfer Agreements (DTAs) or data transmission statements (DTS) play a pivotal role in the secure and compliant exchange of data between stakeholders such as research organizations or vendors with pharma companies, particularly in clinical or regulatory environments where data integrity and specification adherence are critical. However, the manual process of validating these data transfers is both error-prone and time-consuming, leading to repeated submissions and significant time loss.

Introducing `DTAtools`, a comprehensive solution designed to streamline the management of DTAs / DTS. `DTAtools` stores DTA definitions in the user-friendly YAML format, offering flexibility and compatibility across various DTA scenarios.

`DTAtools` is equipped with a suite of features to enhance user experience and ensure data integrity. It conducts validity checks to confirm adherence to required standards, formats and schemas, and provides convenience functions to eliminate repetitive coding and minimize errors. The package's error checks ensure consistent and error-free data processing. Additionally, DTAtools can read and generate human-readable DTA formats, expediting the drafting process of these documents.

`DTAtools` is open-sourced by Boehringer-Ingelheim to provide an easy tool for external and internal collaborators to validate data before file transfer, following DevOps principles of detecting error fast and automatically.

## Features

- Import/export DTA specifications from/to YAML and Word documents
- Comprehensive validation of data for: type, format, nullability, allowed values, and regex patterns
- Validation of schema rules (column based logic)

## Installation

For now, it is just possible to install the development version from GitHub:

```r
remotes::install_github("Boehringer-Ingelheim/DTAtoolsR")
```

## Quickstart

Load required libraries

```{r setup}
require(DTAtools)
```

Validate a table using specifications stored in a yaml file.

```{r}
# Load specs
specs_path <- system.file("extdata", "params_spec.yaml", package = "DTAtools")
specs <- importDTAColumnSpecCollectionFromYaml(specs_path)

# Load data
data_path <- system.file("extdata", "data_spec.yaml", package = "DTAtools")
data <- fread(data_path)

# Validate
dt <- DTAContainer(specs, list(my_data = data))
```

## Usage

### Import DTA specifications

DTA / DTS specifications are stored in the human and machine readable YAML format. Generally, they are store in either dedicated YAML files for are part of YAML configuration files.

Specification contain definitions of:

- **columns:** defines column names, format specification and optionally patterns or selection for values in the column. More details in section [YAML Column Format](#yaml-column-format).
- **rules:** defines rules for columns, e.g. if column A is empty, then columns B must contain a value. More details in section [YAML Schema Rule Specification](#yaml-schema-rule-specification).
- **metadata:** DTA meta data about version, author etc. More details in section [YAML Metadata].

First, you import the specifications from a YAML file.

```{r}
specs <- importDTAColumnSpecCollectionFromYaml("spec.yaml")
```

> Note: Use the `tag` argument of `importDTAColumnSpecCollectionFromYaml` if `columns`, `rules` and `metadata` are nested in the YAML file, e.g. if they are under `DTA: format:`, then add `tag=c("DTA", "format")`.

### Load and validate data

The next step is to import the data and create a `DTAContainer` object. Once created, the `DTAContainer` object will validate the data for the specifications right away.

```r
table <- data.frame(STUDYID = c("1234", "1234"), VISIT = c("V01", "V02"))
data_obj <- DTAContainer(specs, list(my_table = table))
```

### Write validated data to file

Use the `writeTableToFile()` function to export validated tables with optional sorting, compression, and metadata. This ensures, that only tables are saved that have been previously validated.

```r
writeTableToFile(
  DTAContainer = data_obj,
  table = "my_table",
  filename = "validated_table.tsv",
  arrange_by = "all",
  sep = "\t",
  na = "",
  get_md5sum = TRUE,
  write_md5sum_to_file = TRUE
)
```

## Additional features

### Export Spec Table to Word

If you want to export the specifications stored in the YAML as a table to Word file you can use this function:

```r
exportDTASpecTable(specs, "dta_spec_table.docx")
```

### Export Column Values Table

Exporting all defined potential values in a column to a word table.

```r
exportColumnValueTable(specs, "column_value_table.docx", id = "VISIT")
```

## YAML Column Format {#yaml-column-format}

Columns specifications can contain

- _id:_ (mandatory) the ID of the column
- _label:_ (mandatory) the label of columns
- _description:_ (optional) the description of the column
- _type:_ (mandatory) the SAS type like Num, Char, Date9
- _format:_ (mandatory) the SAS format like 10., \$10
- _nullable:_ (mandatory) if a columns has to contain values (Yes, No or True, False)
- _pattern:_ (optional): Regex for value check
- _values:_ (optional): A list of possible values

Here is an example of a column `SUBJIDN` with a pattern

```yaml
columns:
  - id: SUBJIDN
    label: Subject identifier for the study
    type: Num
    format: 10.
    nullable: No
    pattern: "^[0-9]{10}$"
```

Here is an example of a column `GFGRPID` with

```yaml
columns:
  - id: SEX
    label: Sex
    type: Char
    format: $6
    nullable: Yes
    description: "Self reported sex, empty if not reported"
    values:
      - "male"
      - "female"
      - "other"
```

## YAML Schema Rule Specification {#yaml-schema-rule-specification}

The `DTAtools` package supports schema-based validation of tabular data using declarative rules defined in YAML. These rules are evaluated after column-level validation and allow for complex inter-column logic enforcement.

### Structure

Rules must be defined under the top-level key `rules` in the YAML file. Each rule is a list item with a required `id`, `type`, and additional fields depending on the rule type.

### Supported Rule Types

#### `check_condition`

Implements logic to evaluate the validity of the table composition.

Several logical operators where transformed into yaml syntax making it easy to define logic rules.
Implemented operators are:

- `equals`: String / Number
- `not_equals`: String / Number
- `greater_equal`: Number
- `greater`: Number
- `less_equal`: Number
- `less`: Number
- `range`: List of Two Numbers
- `in`: List of Strings / Numbers
- `not_in`: List of Strings / Numbers

```yaml
- id: rule_equal_example
  type: check_condition
  condition:
    VISIT:
      equals: V03
  then:
    STATUS:
      equals: COMPLETED
- id: rule_unequal_example
  type: check_condition
  condition:
    VISIT:
      equals: V03
  then:
    STATUS:
      not_equals: DROPPED
- id: rule_dependency_example
  type: check_condition
  condition:
    CONSENT:
      equals: "YES"
  then:
    CONSENT_DATE:
      empty: false
- id: check_condition_example
  type: check_condition
  condition:
    VISIT:
      equals: V03
  then:
    STATUS:
      equals: COMPLETED
    VISIT:
      in:
        - V03
        - EOT
    AGE:
      range:
        - 10
        - 100
    WEIGHT:
      greater_equal: 5
```

#### `check_range`

Ensures that values in a numeric column fall within a specified range.

```yaml
- id: rule_range_example
  type: check_range
  column: AGE
  range:
    - 18
    - 65
```

#### `check_unique`

Ensures that all values in a column are unique.

```yaml
- id: rule_unique_example
  type: check_unique
  column:
    - SUBJECT_ID
    - VISIT
```

## YAML Metadata

Metadata can contain

- **version:** Version of specifications
- **author:** Author
- **create:** Date of creation
- **description:** Description of specifications

Example:

---

metadata:

- version: "1.0.0"
  author: "Thomas Schwarzl"
  created: "2025-07-08"
  description: "GF Domain Specification"

---

## Technical

`DTAtools` is build on the S7 object system and uses json schema for validation.

- Define column specifications with `DTAColumnSpec`
- Group specifications into collections with `DTAColumnSpecCollection`
- Validate data frames against specifications and logic using `DTAContainer`
- Export documentation tables to Word using `flextable`

### Core Classes

- `DTAColumnSpec`: Defines a single column's metadata and constraints
- `DTAColumnSpecCollection`: A named list of DTAColumnSpec objects
- `DTAContainer`: A validated specs of data frames against specifications

### Validation Functions

- `validateTable()`: Validates a data frame against specifications

### Export Functions

- `writeTableToFile()`: Write validated tables to disk with optional compression and metadata
- `exportDTASpecTable()`: Export full spec documentation to Word
- `exportColumnValueTable()`: Export allowed values of a column to Word

#### Rules Engine

- Rule types implemented:
  - `check_range`
  - `check_unique`
  - `check_condition`

### Manually add specs

Here an example how to manually define column specs

```r
col1 <- DTAColumnSpec(id = "STUDYID", type = "Char", nullable = FALSE)
col2 <- DTAColumnSpec(id = "VISIT", type = "Char", nullable = TRUE)
specs <- DTAColumnSpecCollection(columns = list(STUDYID = col1, VISIT = col2))
```

## Important Notes

- All rules must include a unique id.
- Rule types must match exactly one of the supported types.
- Missing or malformed rules will trigger validation errors before data evaluation.
- when importing yaml from DSO, patterns must be non-quoted strings

## Credits

`DTAtools` was developed by

- Daniel Schreyer
- Thomas Schwarzl
