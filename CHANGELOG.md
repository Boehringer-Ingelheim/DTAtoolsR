# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Unit tests for the bundled Shiny app, which previously had none: its helper
  files are auto-sourced by Shiny at launch and were therefore invisible to the
  test suite.

### Changed

- Removed the unused `export_modal_ui()` helper from the Shiny app. The export
  modal has always been built inline in `app.R`; the orphaned builder produced
  module-namespaced ids that no server ever observed.

### Fixed

- Declared `htmltools` and `tinytex` in `Suggests`. Both are used by the bundled
  Shiny app but were undeclared, working only because `htmltools` arrives
  transitively with `shiny`. `R CMD check` does not scan `inst/`, so neither was
  flagged.

## [0.12.2] - 2026-08-04

### Added

- Shiny app header now includes quick links for `Report issues` and `About`, so users can directly open the GitHub issue tracker, repository overview, and package documentation from the top bar.

### Changed

- Refined Shiny app brand-bar action styling for better visibility and responsive wrapping on smaller screens.

## [0.12.1] - 2026-07-30

### Added

- `export_with_template()` for filling a user-authored Word (`.docx`) template with values from a `DTA` object. Placeholders use a single-brace, upper-case convention (e.g. `{DTA_TITLE}`, `{SUPPLIER_NAME}`, `{RECEIVER_CONTACTS}`, `{TRANSMISSION_TYPE}`, `{TOTAL_COLUMNS}`) covering agreement metadata, supplier/receiver affiliation and contacts, transmission details, dataset content, and process information. Substitution runs at the WordprocessingML level so special characters (`&`, `<`, `>`) are escaped automatically and placeholders split across runs are still matched; callers can override or add values via `variables`, and template failures fall back to the built-in layout unless `fallback = FALSE`.
- `write_dta()` gains `template` and `template_variables` arguments that route DOCX/PDF output through `export_with_template()` when a template is supplied.
- Shiny app export dialog replacing the two separate export buttons with a single "Export" action that offers Markdown (optionally converted to PDF via Pandoc, with an optional hidden embedded-YAML block), built-in Word, and custom-template Word output. Custom templates are auto-discovered from `inst/extdata/templates`, and the `{DATASETS_SUMMARY}`, `{DATASETS_DETAIL}`, and `{YAML_EMBEDDED}` placeholders are filled from the current `DTA`. Adds `shinyjs` to Suggests.
- Shiny app "Create new from template" workflow on the landing page: a declarative YAML template engine (`inst/shiny/dta_app/R/template_core.R`, templates discovered from `inst/extdata/templates/*.dta-template.yaml`) builds a new `DTA` from a bundled dataset structure plus a two-step options modal, and ships a `biomarker_gf` GF DTS template. Template `base.metadata` and every option `effect` target only canonical `DTAMetaData` fields (`title`, `version`, `date`, `header`, `version_history`, `receiver`, `supplier`, `error_handling`, `authorized_for_corrections`, and `transmission.{type,frequency,notification,date_first_transfer,date_last_transfer,test_upload,blinded_transfer}`), so every generated value is visible and editable in the app metadata editor. A single option can set multiple fields, `boolean` options render as Yes/No, and every non-boolean dropdown offers its suggested values plus a `(leave blank)` entry and a `Custom...` entry that reveals a companion text field for a free-typed value. The bundled `biomarker_gf` template uses generic, vendor-neutral example values.
- `write_dta()` gains `include_yaml` and `yaml_text` arguments that append the machine-readable YAML specification as a final, very small-font monospace section of the built-in DOCX/PDF layout (YAML indentation is preserved). Bundles a numbered Word reference template (`inst/extdata/templates/dta_numbered_template.docx`) that adds a `heading 4` style bound to the template's multilevel heading list, so the built-in writer's headings render as true Word list fields that auto-number (`1` / `1.1` / `1.1.1` / `1.1.1.1`) and renumber if the document is edited.

### Changed

- Built-in Word/PDF export (`write_dta()`) now nests all content under a single numbered `Data Transfer Agreement` chapter, and folds each dataset's specifications into the `Datasets` section instead of emitting flat, unnumbered bold subheadings: `Datasets` → `<dataset>` → `Files` (e.g. `1.4.1.1`) and `Dataset Specifications` (e.g. `1.4.1.2`, with `Column Specifications` and `Validation Rules` beneath it). The `Files` section now states each expected file's match type (exact name vs. regex pattern), format (CSV/TSV/Delimited), expected count, and description.
- Shiny app export dialog: the "Embed YAML" checkbox now also applies to the built-in Word export (previously it only affected custom templates), appending the small-font YAML section, and is relabelled `Embed YAML specification at end of document`.

### Fixed

- `write_dta()` (and the built-in DOCX/PDF writer) no longer aborts with `invalid 'pattern' argument` when the `DTA` metadata has no `title` (for example a `DTA` built from a standalone dataset YAML) or the title contains regex metacharacters. The title section no longer matches the title text as a regex via `officer::cursor_reach()`.
- Shiny app export dialog: Markdown and built-in Word exports no longer fail with `argument is of length zero`. The handlers wrongly tested a non-existent `$ok` field on the `write_dta()` / `export_with_template()` return value; both functions signal failure by erroring, not by returning a status list.
- Shiny app export dialog: the dataset/specs detail block no longer fails with `cannot coerce type 'object' to vector of type 'character'`; column and rule descriptions are now built via the app's list extractors (`dta_column_to_list()`, `dta_rule_to_list()`) instead of coercing raw `S7` objects to character.
- Shiny app export dialog: the `{YAML_EMBEDDED}` placeholder is always filled (blanked when YAML embedding is off) instead of being left as literal text in the exported Word document.
- Shiny app export dialog: exporting now actually starts the browser download instead of only showing `Document exported successfully`. The hidden download button was fired with `shinyjs::click()` (a jQuery-style event that does not invoke a download link's native navigation) inside a `display: none` container (which prevents native download clicks in some browsers). It is now triggered via a custom message handler that calls the element's native `.click()`, and the button is rendered off-screen instead of `display: none`.
- Shiny app export dialog: Markdown-to-PDF export now works without a LaTeX installation. It first tries pandoc with a LaTeX/`wkhtmltopdf` engine (best typography when present) and otherwise prints the rendered HTML to PDF with a headless Chrome/Edge browser (no extra R packages required; honours the `DTATOOLS_CHROME` override). Only if neither route is available does it fall back to delivering the Markdown file with a clear notification, instead of aborting the export with `pandoc document conversion failed` / `pdflatex not found`.
- Shiny app: replaced `shiny::hidden()` (not exported by older `shiny`) with a CSS-hidden container so the app loads on those versions.

## [0.12.0] - 2026-07-28

### Added

- `run_dta_app()` and a bundled Shiny application (`inst/shiny/dta_app`) providing a modern, Boehringer Ingelheim-themed UI for the package: drag-and-drop loading of a DTA YAML, per-dataset file uploads driven by each dataset's `DTAFile` handlers (with filename/pattern and min/max-count guidance), one-click or per-dataset validation via `check()` with green/red status, a clickable messages list backed by `inspect()`, incremental metadata and contacts editing, Word/PDF export via `write_dta()`, a read-only raw-YAML view, and in-session autosave/recovery. Requires the suggested packages `shiny`, `bslib`, and `DT`.

### Fixed

- GitHub Actions CI configuration: pinned S7 dependency to version 0.2.2 to ensure consistent package behavior across local development and GitHub Actions runners
- renv bootstrap files (`renv/activate.R` and `renv.lock`) are now tracked in git to enable proper environment restoration in GitHub Actions CI; `.gitignore` updated to only exclude generated directories (`renv/library`, `renv/staging`, `renv/settings.json`)
- added `remotes` to package Suggests and GitHub Actions workflow setup-r-dependencies to resolve macOS CI failures during S7 installation
- `inspect()` (and the Shiny app's message inspector) no longer errors with `Can't find property <DTAtools::DTARuleColRange>@range` when highlighting the failing rows of a `check_range` rule. The failing-row helper now reads the rule's `min`/`max` properties directly instead of a non-existent `range` property.
- `validate_table()` / `check()` no longer abort with `No method asJSON S3 class: vctrs_unspecified` when a table contains a fully-empty column. Arrow reads such columns as its `null` type (a `vctrs_unspecified` vector in R), which `jsonlite::toJSON()` cannot serialise; they are now coerced to all-`NA` and emitted as JSON `null`, so nullable empty columns validate correctly instead of crashing the whole validation run.


## [0.11.0] - 2026-07-27

### Added

- `max_number_of_files()` and `min_number_of_files()` generics on `DTADataSet`, aggregating counts across all files in the dataset
- comprehensive test coverage for previously untested export APIs (`write_dta()`, `write_dataset_metadata()`, `write_file_specification()`, `export_specs_table()`, `export_column_value_table()`, `write_metadata()`)
- direct unit tests for `validate_table()` / `validate_table_detailed()` behavior on valid input, schema violations, and rule violations
- package architecture diagram (`img/DTAtools_architecture.svg`, also embedded in `vignettes/`) illustrating the `DTA`/`DTADataSet`/`DTAColumnSpecCollection`/`DTAFile` class hierarchy, referenced from both the vignette and a new `README.md` "Package Architecture" section

### Changed

- reworked the vignette and `README.md` guidance around DTA structure and YAML-first import workflows, clarifying the relationship between metadata and datasets and prioritizing `read_dta_from_yaml()` / `read_dataset_from_yaml()` as the primary entry points
- updated the `R-CMD-check` GitHub Actions workflow so pushes and pull requests targeting `dev` also trigger checks
- brought `README.md` closer to parity with the vignette: expanded the Core Classes / Key Functions reference tables (`DTAFileDelim`, `validation_status()`, `validation_errors()`, `datasets()`, `tables()`, `get_table()`, `labels()`, `specs()`, `colspec()`, `rules()`, `metadata()`, `write_columns_to_yaml()`/`write_columns_to_json()`, `as_json_schema()`), and added sections on inspecting results with `inspect()`/`validation_status()` and on building a full `DTA` object that mixes `DTADataSetTabular` and `DTADataSetFile` datasets
- added the package logo to the vignette

### Removed

- removed the `DTARuleCollection` class; `DTAColumnSpecCollection@rules` (and `rules(x)`) is now a plain list of `DTARule` objects (or `NULL`) instead of a `DTARuleCollection` wrapper object

### Fixed

- resolved all `R CMD check` WARNINGs (`checking Rd \usage sections`, code/documentation mismatches) stemming from S7 methods being documented against the generic dispatcher's `(x, ...)` signature rather than each method's real formals; S7 generics with multiple class-specific methods (`check`, `colspec`, `load_file`, `rules`, `read_file_execution`, `max_number_of_files`, `min_number_of_files`, `metadata`, `files`, `tables`, `columns`, `get_table`, `inspect`, `validation_status`, `validation_errors`, `clear_validation`, `column_preview`, `rule_preview`) now document all class methods on a single shared Rd page per generic
- fixed stale/incorrect constructor argument documentation: `DTAFile`/`DTAFileCSV`/`DTAFileDelim`/`DTAFileTSV` incorrectly documented a `sep` parameter that doesn't exist on their constructors, while omitting the real `min_number_of_files`/`max_number_of_files`/`info`/`pattern_description` parameters; `DTAColumnSpec` (`examples`, `colclass`), `DTAColumnSpecStructure` (`backend`), `DTADataSet`/`DTADataSetFile`/`DTADataSetTabular` (`description`, `template_source`, `template_version`, `template_date`), `DTARule`/`DTARuleColCondition`/`DTARuleColRange`/`DTARuleColUnique` (`description`, and `min`/`max` for `DTARuleColRange`) were also missing `@param` documentation for real constructor arguments
- removed the unused, dead `encoding` constructor parameter from `DTAFileTabular` (accepted but never stored or used)
- fixed malformed `\usage{}` blocks for the `` `[[` `` / `` `[` `` operator methods on `DTA`, and added missing `\alias{}` entries for the operators
- fixed duplicate Rd `\name{}` entries and out-of-tree scratch files leaking into the build (`.Rbuildignore` additions)
- closed test-coverage gaps for DTA/Dataset YAML error paths, `DTAFileDelim` read behavior, `DTAColumnSpecStructureFactory` backend-prefix validation, and rule-engine edge cases (`min`/`max`, comparison operators, missing columns, empty rule lists)

## [0.10.0] - 2026-07-25

### Added

- print method to all instantiable classes
- added `examples` to `DTAColumnSpecs`, which need to be following a pattern if provided and are mutually exclusive from `values`
- introduced `colclass`, which is a placeholder for further addition for automatic preprocessing efforts
- file handling with classes: `DTAFile`, `DTAFileTabular`, `DTAFileCSV`, `DTAFileTSV`, `DTAFileDelim`
- introduced classes `DTARule`, `DTARuleCollection`, `DTARuleColCondition`, `DTARuleColRange`, `DTARuleColUnique` for rules
- introduced classes `DTADataSetTabular` handling tabular data, deriving from `DTADataSet`
- introduced class `DTADataSetFile` for validating file presence, non-emptiness, and readability of non-tabular deliverables, with its own `check()`, `results()`, `messages()`, and `inspect()` methods
- introduced `inspect()` generic for `DTADataSetTabular` and `DTADataSetFile` to drill into a specific validation error by `id`, returning row context, the failing JSON Schema clause, or the rows that violated a rule
- functions to access slots
- S7 validators for all classes
- introduced `DTAColumnSpecStructure` and `DTAColumnSpecStructureSAS` for handling `type`, `format`, `length` of a column spec
- info variables to `DTADataSet` and `DTADataSetTabular` 
- example factory functions: `create_example_DTA()`, `create_example_DTAColumnSpec()`, `create_example_DTAColumnSpecCollection()`, `create_example_DTADataSetTabular()`, `create_example_DTAFileCSV()`, `create_example_DTAFileTSV()`, `create_example_DTAMetaData()`, `create_example_DTARuleColCondition()`, `create_example_DTARuleColUnique()`, `create_example_DTARuleColRange()`
- `as.list` methods for `DTAColumnSpecCollection`, `DTAColumnSpec`, and `DTARule` derivatives

### Changed

- renamed functions to snake_case
- renamed DTAFileInfo to DTAFile
- renamed DTAContainer to DTADataSet
- improved GitHub Action workflows
- reworked data backend to use arrow::Table for better performance and memory usage
- completely reworked the package vignette (`vignettes/DTAtools.Rmd`) with a full walkthrough of architecture, column specs, validation, schema rules, `DTADataSetFile`, file-based workflows, the full `DTA` object, `inspect()`, and exporting — every code chunk verified to run against the installed package
- completely reworked `README.md` to match the vignette: corrected terminology (Data Transmission Agreement/Specification instead of Data Transfer), fixed outdated/broken code examples, updated rule type names (`col_condition`/`col_range`/`col_unique`), and documented `DTADataSetFile` and `inspect()`
- re-prioritized YAML import guidance in the vignette and `README.md`: `read_dta_from_yaml()` and `read_dataset_from_yaml()` are now presented as the primary entry points, with `import_specs_from_yaml()` documented as the third, most manual option; added a `read_dataset_from_yaml()` walkthrough to the Quickstart in both documents
- fixed broken anchor links in `README.md` caused by unsupported Pandoc-style `{#custom-id}` heading syntax
- fixed `load_file()` so it is properly exported from the package namespace (previously only accessible via `DTAtools:::load_file`)
- improved testthat tests
- improved CLI messages
- moved rules to `DTARule` and derivative classes
- renamed getter functions to shorter names
- renamed constructor variables
- DTA-class constructor to handle DTAMetaData  
- changed `container` to `datasets` in class DTA
- moved json schema generation to classes
- removed stored `json_schema` as it can be dynamically generated 

## [0.9.0] - 2025-09-11

### Added

- Added GitHub Action workflows
- Added Metadata to DTAData

### Changed

- Moved project to GitHub.com
- Modified documentation and examples to adhere to R CMD checks for CRAN

## [0.8.1] - 2025-08-18

### Added

- `DTAColumnSpecCollectionToList` for returning a list object
- tests for new function

### Changed

- `write_specs_to_yaml` exports also rules

## [0.8.0] - 2025-08-18

### Added

- methods: `get_rules`, `metadata`
- better `cli` messages for improved reporing
- `DTARule` class for object-oriented usage of defined rules
- new tests, including tests with the test data sets

### Changed

- Vignette and `README.md` to fit new functions and usability
- simplified evaluation checks
- CLI
- `rules` usage caused by change to `DTARule`

### Removed

- BI specific information
- redundant validation code

### Deprecated

- `validate_table` without using jsonschema

## [0.7.5] - 2025-07-07

### Added

- `specs_from_list`: Simple import from lists. Can be used in combination with `params.yaml` files managed by dso

### Changed

- allowing numeric values
- pattern information - no quoted strings allowed
- required statement is now in the correct location in the jsonschema

### Fixed

- include dplyr functions

## [0.7.4] - 2025-06-26

### Changed

- jsonschema is now part of the `DTAColumnSpecCollection`
- implemented progress bar to signal status of table validation
- better function messages

## [0.7.3] - 2025-06-26

### Fixed

- export functions are now exported into the NAMESPACE

## [0.7.2] - 2025-06-26

### Fixed

- `DTAColumnSpecCollection_to_jsonschema` - fix values when values: ""

## [0.7.1] - 2025-06-26

### Fixed

- `validate_table_with_jsonschema` fix

## [0.7.0] - 2025-06-25

### Changed

- Switch to using `jsonschema` for validating the correctness of the table
- Improving error messages when using `jsonschema`
- introducing the variable length to check the maximum length of a value in a column

## [0.6.0] - 2025-06-13

### Changed

- `import_specs_from_word` has been updated to new value and pattern format, making it easier to read and extract
- `export_specs_table`, similarly to above. Values and pattern will now be printed in a new format

## [0.5.0] - 2025-06-13

### Added

#### Core Classes

- `DTAColumnSpec`: Defines metadata and validation rules for a single column.
- `DTAColumnSpecCollection`: Manages a collection of `DTAColumnSpec` objects with optional metadata and schema rules.
- `DTAContainer`: Encapsulates validated data tables and their associated column specifications.

#### Import/Export

- YAML and JSON import/export for `DTAColumnSpecCollection`.
- Word document import via `docxtractr` for DTA specifications.
- Word document export of column specs and value tables using `flextable`.

#### Validation

- Column-level validation:
  - Type checking
  - Format length enforcement
  - Nullability enforcement
  - Value list enforcement
  - Regex pattern matching
- Table-level validation:
  - Ensures all required columns are present
  - Applies all column validations
  - Applies schema rules if defined
- JSON Schema generation and validation support

#### Schema Rule Engine

- Rule types implemented:
  - `check_equal`
  - `check_unequal`
  - `check_range`
  - `check_dependency`
  - `check_mutual_exclusive`
  - `check_unique`
  - `check_allowed_combinations`
- `apply_schema_rules()` to evaluate all rules with CLI feedback

#### Utilities

- `checkFormat`, `checkType`, `checkNullable`, `checkValues`, `checkPattern`, `change_type`, `changeNAs`, `prepareTable`, `validateColumn`
- `validateSchemaRulesFormat()` to validate schema rule structure before use

#### Output

- `write_table_to_file()` to export validated tables with optional sorting, compression, and metadata
- MD5 checksum and dimension reporting

#### Developer Support

- `testthat` test suite for all validation and rule functions
- Pre-commit hook configuration to enforce test execution before commits

### Notes

- This is the initial release of the `dtatoolsr` package.
- Designed for robust, schema-driven validation of tabular datasets in regulated environments.

## [0.4.0]

- Implemented rule system for validating table format
- Started to implement the use of jsonschema as table validator
- added testthat tests

## [0.3.0]

- Implement function to write word table for DTA
- Improve warnings, errors, and info messages
- Make naming convention consistent
- Update write table function to include md5sum and dimension calculations
- Export tables into word file
- Import ColumnCollection from Word File

## [0.2.0]

- Improved information printouts
- improved naming conventions
- added write_table_to_file function to write valid DTA table
- added methods to extract tables, columns and columncollections

## [0.1.0]

- Initial internal release

[Unreleased]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.10.0...HEAD
[0.10.0]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.9.0...v0.10.0
[0.9.0]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.8.1...v0.9.0
[0.8.1]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.8.0...v0.8.1
[0.8.0]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.7.5...v0.8.0
[0.7.5]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.7.4...v0.7.5
[0.7.4]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.7.3...v0.7.4
[0.7.3]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.7.2...v0.7.3
[0.7.2]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.7.1...v0.7.2
[0.7.1]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.7.0...v0.7.1
[0.7.0]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.6.0...v0.7.0
[0.6.0]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.4.0...v0.5.0
[0.4.0]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/Boehringer-Ingelheim/DTAtoolsR/compare/v0.1.0...v0.2.0
