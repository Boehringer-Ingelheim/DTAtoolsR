# PLAN — Matrix format review (`type: matrix`)

Branch `ai/dta-dataset-matrix-review-bf9e6d`, on `dev` @ `5c98aa58` (PR #205
format, PR #208 app). The format is unreleased (`CHANGELOG` `[Unreleased]`), so
it changes without aliases or migration hints.

## 1. What the review found

Probed by running the package (R 4.5.1) and by reading the code. A cold-start
usability test was started but its agent was cut off before reporting; the
files it left show it wrote the R `write.table` case correctly at the first
attempt, so the findings below rest on the code and the probes, not on it.

| # | Finding | Evidence |
|---|---|---|
| F1 | **The corner is a parallel array with in-band keywords.** `matrix.header.corner` is `named`, `blank` or a list that must have one entry per `row_labels` entry, whose first entry may be `null`. It describes the row-label columns but lives in another block. | `corner: gene_id` and `corner: named` have the same shape and different meanings; a header text that really is `named`/`blank` cannot be written for one label ("keyword trap", refused in three app paths); add/remove/move of a row label needs corner bookkeeping in three app helpers, a validator class and two lints; the app needs a `dta_verbatim` escape so a `null` survives saving. |
| F2 | **`matrix.header` mixes three subjects**: the corner (row labels), the rule for the data-column labels, and the number of data columns (`header.count` next to `rows.count`). | Reading `count` under `header` as "number of data columns" needs the docs. |
| F3 | **Not congruent with tabular where it could be.** In `columns:` the `id` is the header text. For a row label the header text has no default and is required elsewhere. | `matrix_corner_missing` is the first error every newcomer meets. |
| F4 | **"Corner" reaches the supplier.** The exported agreement says `Header corner: blank` / `named: GENE_ID`; `print()` says `corner:`; findings carry keyword `corner` and "differs from the declared corner". | `write_dta(format = "md")` of `inst/extdata/matrix_dta.yaml`. |
| F5 | **Documentation gap.** `DTA_YAML_GUIDE.md` (the authoring reference) does not mention matrix datasets at all; `README.md` has no usage section and no key reference; only the vignette explains the format. No single normative key table exists. | grep. |
| F6 | Small wording defects: `print()` lists row labels as `columns (2)` and `rows: 1 to Inf`; `must have at least 1 rows (has 0)`; the row-key finding reads `Rule '.matrix:row_key' violated: …`; the `propertyNames` explanation says "does not satisfy the declared header specification". | probe output. |

What is good and stays: `row_labels:` (user decision), `rows.key`/`rows.count`,
`cells` with `range`/`infinite`, `count` as a number or `{min, max}`, the closed
key lists, the load gate and its hints, lint coverage, the engine, no hollow
pass.

## 2. The format after the change

```yaml
- id: gene_counts
  type: matrix
  files: { filename: "matrix_gene_counts.tsv", type: tsv }
  row_labels:                 # the leading columns, by position
    - id: GENE_ID
      header: gene_id         # text of the header line above this column
      type: SAS Char
      nullable: No
    - id: GENE_NAME
      header: gene_name
      type: SAS Char
      nullable: Yes
  matrix:
    columns:                  # the data columns: every column after the row labels
      label: Sample ID
      pattern: "^S[0-9]{4}$"
      count: { min: 1, max: 384 }
    rows:
      key: GENE_ID
      count: { min: 1 }
    cells:
      type: SAS Num
      nullable: No
      range: { min: 0 }
      infinite: No
```

- **`header` on a row label** (new): absent = the `id` (as for a tabular
  column); a text, `""` included = exactly that text; `No` = the header line
  has no field above this column (R `write.table` with row names) — first row
  label only. `Yes`, a number or several texts are refused with a hint to quote.
- **`matrix.columns`** replaces `matrix.header`: `label`, `description`,
  `pattern`, `values`, `length`, `count`. Optional (default: any non-empty,
  unique label; at least one data column).
- **`matrix.header.corner` is gone**, with `named`/`blank`/`null`, the
  length rule and the keyword trap.
- `matrix.rows`, `matrix.cells`, `rules`, `files`: unchanged.

Corner by writer, before → after:

| Writer | before | after |
|---|---|---|
| R `write.csv` / pandas unnamed index | `corner: blank` | `header: ""` |
| R `write.table` with row names | `corner: [null]` | `header: No` |
| pandas index named `protein_id` | `corner: [protein_id]` | `id: protein_id`, or `header: protein_id` |
| header text = ids | `corner: named` | nothing |

Glossary, used everywhere (YAML comments, docs, app, export, messages): **row
label column**, its **header text**; **data column**, its **column label**;
**row key**; **cell**. The word "corner" is not user-facing any more.

## 3. Implementation

**Package**
- New internal class `DTAColumnSpecRowLabel` (`R/DTAColumnSpecRowLabel-class.R`,
  parent `DTAColumnSpec`, one property `header`: `NULL`, a single string or
  `FALSE`), the precedent being `DTAColumnSpecCell`. Own `as.list()`.
  `dta_row_label_specs(row_labels, rules)` builds the collection from the YAML
  list (the factory and lint call it); `dta_row_label_header(col)` reads it and
  treats a plain `DTAColumnSpec` as "header = id".
- `DTAMatrixSpec`: properties `corner_mode`, `corner` removed; reader keys
  `columns`/`rows`/`cells`, `cells` required; `as.list()` writes `columns`.
- `DTADataSetMatrix`: corner helpers read the row labels; validator class
  `dta_matrix_invalid_header` (`header: No` anywhere but first) replaces
  `dta_matrix_invalid_corner`; gate hints name `header: No`; `print()`.
- Finding keyword `corner` → `rowLabelHeader`; wording F6.
- Lint: `row_label_header_invalid`, `row_label_header_position`; remove
  `matrix_corner_missing`, `matrix_corner_invalid`.
- Export (`.matrix_layout_rows`): a "Header line" row in words; plain wording.

**App**
- Row label form: "Header text above this column" (Same as the ID / Empty /
  Other text / No field — first row label only).
- Matrix layout: sections Data columns / Rows / Cells; the corner controls go.
- Add dataset → Matrix: same three questions, mapped to the row label's header.
- Helpers lose the corner bookkeeping, the keyword trap and `dta_verbatim`.

**Tests, qualification, docs**
- Migrate the matrix tests and OQ tests; rewrite the REQ sentences first
  (`REQ-MATRIX`, `REQ-APP`, `REQ-SPEC`, `REQ-TMPL`, `REQ-VAL`), then the tests;
  qualification manifest re-synced.
- `DTA_YAML_GUIDE.md`: new normative section "Matrix datasets" with the key
  table; `README.md`: usage section; vignette, man pages, template-repo
  skeleton README, `CHANGELOG` `[Unreleased]`.

## 4. Verification

`style.R`, `roxygenise()`, `devtools::test()`, `pre-commit run --all-files`,
the IQ+OQ quick run, `rcmdcheck`; the cold-start usability test repeated on the
new format; the app walked again with screenshots; then review of the diff.

## 5. Not included

- A missing cell is reported as "must be number" / "Actual: (missing / not
  present)": shared with tabular, changing it rewrites tabular messages.
- A layout sketch table in the exported document and in the app.
- `col_range` and `cells.range` keep their two spellings (`min`/`max` flat vs
  under `range`).

## 6. As built, and what the review of the implementation found (2026-11-16)

Differences from §3:

- `DTAMatrixSpec@header` keeps its property name; its spec has the id
  `matrix.columns`, so a message about a bad pattern names the key the author
  wrote. Abort class `dta_matrix_header_values_text` became
  `dta_matrix_columns_values_text`.
- An object saved in the earlier format (settings carrying `corner_mode`) is
  refused with `dta_matrix_format_outdated` instead of being read with its
  header texts silently replaced by the ids. The app's session restore then
  falls back to the saved YAML, which is refused for `matrix.header`.
- The app shows a two-line sketch of the file above the Row labels list and
  the Matrix layout form, and a `header` column in the Row labels list.
- The exported "Matrix layout" table gained a `File layout` row.

Review (one adversarial pass that ran the code, plus the three migrations):

| Finding | Resolution |
|---|---|
| The validator of `DTAColumnSpecRowLabel` returned its helper's value, which S7 reads as an error: no row label with a `header` could be built (introduced while fixing the next row) | The validator ends in `NULL` |
| A numeric `header` was refused by the property type with S7's message and no class | Checked in the constructor: `dta_row_label_header_invalid` |
| The load-refusal hints named remedies that could not help (add a row label; `header: No` when already declared) | Hints branch on whether the first row label declares `header: No` |
| Errors in `matrix.columns` were reported as `header: ...` | Spec id `matrix.columns` |
| A restored object of the earlier format changed meaning silently | Refused (above) |
| "Exactly that text" was false: both sides are trimmed and unquoted | Documented as such; a header of spaces only is shown as empty |
| The repeated-label finding said a label "appears 2 times" when it only repeated a row-label id | Its own sentence |
| A `rowLabelHeader` finding did not say what to change; an empty field read as "(missing / not present)" | Explanation names the remedy; "(an empty field, or no field)" |
| `header:` on a tabular column gave R's translated "unused argument" | Named in lint |
| A lint line did not say which row label | It does |
| The reload notice covered renaming only | Also adding; the list hint names all four operations |

Left as they are:

- A data label equal to a row label's *header text* (not its id) passes: the
  two columns are read under different names.
- `print()` of a `DTAColumnSpecRowLabel` prints two class lines, as
  `DTAColumnSpecCell` does.
- `infinite: false` on a `SAS Int` or `SAS Char` cell is refused although it
  states the default.
- The default (`header` left out = the id) is wrong for R `write.csv` and
  pandas files, which need `header: ""`; it was chosen to match tabular, and
  the finding it produces names the remedy.

Second pass, on the fixes above (no critical finding, no regression):

| Finding | Resolution |
|---|---|
| The document-level half of the old-format guard had no test; `REQ-CONT-029` said every older object is upgraded | Test with a document holding one old and one current matrix; `REQ-MATRIX-059` and its OQ test; the exception named in `REQ-CONT-029` and in `?upgrade_dta` |
| `lint_dta()` of an old-format dataset on its own was silent | It refuses too |
| Session restore said only "Could not restore the previous session" | It says why, and the notice stays |
| A dataset written without `type` but with `row_labels` got the "header belongs to a row label" line | `header` is stripped by the key the specs are under |
| The reload notice missed some stale cases, stayed with no file loaded and stacked | One rule (`dta_matrix_needs_reload()`): row-label IDs or their order changed, or "no field" switched, and a table is loaded; shown after save, remove and move; a repeat replaces it; `REQ-APP-049` |
| The file sketch drew "(no field)" as a field | Left blank, named in a note; an empty field is drawn as `""` |
| Requirement clauses and two explanation texts without a test | Tests and OQ steps added; the trimming rule and the row-label-ID sentence are in `REQ-MATRIX-010` and `-014` |

Also left as they are:

- `print()` of an old-format dataset still prints; every function that reads
  it refuses.
- `header:` on a tabular column is named by lint (so by `read_dta_from_yaml()`
  and `import_specs_from_yaml()`); `specs_from_list()` and the constructors
  still give R's "unused argument".
- A header written as two double quotes inside the text compares as empty but
  is shown as text in the app.
