# PLAN — `DTADataSetMatrix` (`type: matrix`)

**Baseline: `ai/multi-dataset-zip-xpt-5be318` @ `f6189bf`** ("feat: manage
delivery containers per document"; one commit on top of `dev` @ `456f97b`,
0.27.0; checked out in worktree `shiny-header-tag-bfdc88`). The matrix branch
is a **new** branch cut from `f6189bf`; its PR targets that branch, or `dev`
once the container PR has merged. **All line numbers are on `f6189bf`.**

Status: **r5 (final)** — red-team rounds 1 (4 reviewers), 2 (2) and 3 (2; no
blocker), user decisions (§12), rebased onto the container branch (remap +
interaction analysis, §4.7). History in §14.

---

## 0. Context

Suppliers deliver omics results as **matrices**: a CSV/TSV whose header row
labels the data columns, whose leading column(s) identify and describe each
row, and whose remaining cells hold one homogeneous measurement (counts,
normalised expression, NPX, beta values, genotype calls). Orientation is the
supplier's choice: genes × samples (nf-core/rnaseq merged counts) and samples ×
miRNAs (nf-core/smrnaseq `mature_counts.csv`, R `write.csv(t(x))`) both occur.
Today such a file cannot be specified:

- `type: tabular` validates a **closed** column set (`R/streamingValidation.R:2413-2487`,
  `additionalProperties`); a matrix's data columns are unknown at spec time,
  number up to tens of thousands and are described by a rule, not a list.
- `DTAColumnSpec` has no numeric range (ranges are one-column rules).
- Nothing can say "the corner is blank", "the header has no field above the
  row names", "1–384 samples", "row ids unique", "no `-Inf`".

Goal: `type: matrix` whose spec defines **(1) the header, (2) rows and row
descriptions, (3) dimensions — exact, flexible or min/max, (4) cell values —
type, range, missing, NaN, Inf**, validated by the existing engine (in memory
and streaming) with the same axes, reports, documents, container handling and
qualification evidence as tabular datasets.

---

## 1. Decisions

| # | Decision | Evidence / why |
|---|---|---|
| D1 | `DTADataSetMatrix` = S7 **subclass of `DTADataSetTabular`**; built with `new_object(DTADataSetTabular(...), type = "matrix", matrix = ..., heads = list())`. | Experiment: load, check (eager + streaming), `validation_status`, `messages`, `check(DTA)`, `results`, MD export work by inheritance once allowlisted. On the container base `load_delivery()` binds via `inherits(ds, DTADataSetTabular)` + `load_file()` (`R/loadDelivery.R:217-240`), so a matrix routes exactly like tabular. |
| D2 | Row-label columns are ordinary column specs, written under the YAML key **`row_labels:`** (user decision; `columns:` rejected for a matrix), stored in `@specs` like tabular columns, matched to the file **by position**; key translated only at the boundaries (§4.6). | Internal reuse (engine, docs table, column-spec lint); no confusion between row labels and data columns for authors. |
| D3 | Matrix settings: one property `@matrix`, internal (unexported) S7 class `DTAMatrixSpec`; the constructor's `matrix =` takes the YAML-shaped list. | One object to validate, serialise, fingerprint; no extra export. |
| D4 | Header labels checked against a `DTAColumnSpec` (`SAS Char`); cells are an internal subclass **`DTAColumnSpecCell`** adding `minimum`, `maximum`, `finite`, whose `as_json_schema()` emits them and `dta_check_column_spec()` evaluates them. | Experiment: dispatches through `dta_compile_columnspec_schemas()` into `dta_check_column_spec()` eager and streaming. Per-cell findings with row, column, value. Generated per-column rules (r0) measured 35 s build, 1.9–4.1× slower. |
| D5 | Effective specs per table: row-label specs + one clone of the cell spec per data column (`S7::prop(clone, "id", check = FALSE) <- name`) + one generated rule `.matrix:row_key` (`col_unique`). | 20k clones 0.13 s; fixed per-check overhead at 20k ≈ 14 s (within §7 targets, reducible). |
| D6 | **Head reader**: raw bytes of the first two non-blank records (quote-aware, no length cap, BOM dropped, gz-aware), each tokenised alone by Arrow with a trailing newline appended, `block_size` > record length, **every** field typed `binary()` (count bounded by delimiter bytes), `na = character(0)`; then `iconv()` from the handler encoding and `dta_clean_column_names()`. Data read with **explicit positional `column_names`, used verbatim**. | Experiments: Arrow's schema read aborts on duplicate / double-blank names; inference turns `007`→`7`, `""`/`NA`→NA; `utf8()` aborts on latin1; the single-record read needs newline, block size and full `col_types`; `dta_clean_column_names()` is not idempotent, so explicit names must not be cleaned twice. |
| D7 | Structural findings enter the engine through a new optional argument and are **retained ahead of cell findings**, order [header, row count, cells] on both paths: in memory prepended right after `R/validationFunctions.R:187` (before the cap at `:319`); streaming adds header findings to the sink before the batch loop and inserts the (≤ 1) row-count finding after the header rows between `R/streamingValidation.R:1148` and `:1149`, copying `truncated`/`spilled_rows`/`spill_dir` back, adding 1 to `columnspec_sink$total` and to the tally; the retained frame may exceed `max_errors` by one (documented). | Round 3 experiments: `n_columnspec_errors`, `messages()`, `results()`, `collect_full_errors()` (all rows, no duplicates, also at `max_errors = 0`) correct on both paths. |
| D8 | **No new numeric grammar**: cells typed by `dta_coerce_table_to_specs()` as a tabular column of that type; hex / `+12` / `012` accepted as in tabular (LIM); one notion of **missing**; **NaN is never a value** (user decisions). | Eager = lazy verified for 36 tokens. |
| D9 | **Infinite values rejected by default** for `SAS Num` (`infinite: false` → `finite`). | `-Inf` from `log2(0)` is the commonest real defect. |
| D10 | No hollow pass: data columns `count.min ≥ 1` always; rows `count.min = 1` by default (`0` only when written); `cells.type` and `header.corner` required; key non-nullable; for a matrix `required`/`additionalProperties` report `not_applicable`. | Zero-check-certificate rule (`R/columnSpecChecks.R:790-792`). |
| D11 | Files: `DTAFileCSV` / `DTAFileTSV` only (not `Delim`, not SAS), `has_header: true`; loose or inside a container, like tabular. | |
| D12 | **Tabular behaviour identical**, except four intended, tested changes: `"matrix"` in `dta_supported_types()` (`test-shinyapp-utils.R:141`; the function is not used by app UI); behaviour-neutral performance fixes (§7); the wide-header fix (headers > 1 MiB no longer falsely abort — CHANGELOG `Fixed`); `DTADataSetFactory()` gains `matrix`/`row_labels`, which abort with a cli message on a non-matrix type where R's "unused argument" error was raised before. Added after the final review (2026-11-14) — three pre-existing defects in shared code that matrices expose, each a CHANGELOG `Fixed` entry: an unconvertible `NaN` token stayed `NaN` (not `NA`) in an eagerly loaded `Num` column, so `nullable: No` missed it and pattern/values checks saw the text; `explain_messages()`/the HTML report showed "Actual: (missing / not present)" for findings without a row; a `fail_fast` partial scan rendered the unsettled import axis as "validation artifact predates import checking". | |
| D13 | Raw heads are a **dataset property `@heads`** (table name → raw header fields, default `list()`), set only by `load_file()`. Every `check()` recomputes the reading names from `@heads` under the *current* spec and compares with `names(table)`; a lazy table also re-reads its source head. Missing head, mismatch or changed file = **this table's failure** ("load it again") in the missing-file shape (`R/DTADataSetTabular-class.R:1608-1670`), never `cli_abort` (`check(DTA)` runs datasets unguarded, `R/DTA-class.R:1124`). The constructor refuses `tables =`. | An Arrow-object stamp is lost on transcode refresh, session restore (IPC), conversions. The property survives the app's `saveRDS`/`readRDS` session dump (`app.R:1665-1685, 10224`) and is copied like `@provenance` on transfer (§4.7). |

---

## 2. The YAML contract (normative)

### 2.1 Examples

Genes × samples (nf-core/rnaseq-style merged counts, named corner):

```yaml
datasets:
  - id: gene_counts
    type: matrix
    files: { filename: "salmon.merged.gene_counts.tsv", type: tsv }
    row_labels:                       # ROW-LABEL columns, by POSITION
      - id: GENE_ID                   # default row key
        type: SAS Char
        nullable: No
        pattern: "^ENSG[0-9]{11}(\\.[0-9]+)?(_PAR_Y)?$"
      - id: GENE_NAME                 # row description
        type: SAS Char
        nullable: Yes
    matrix:
      header:
        corner: [gene_id, gene_name]  # exact header text above the row-label columns
        label: Sample ID
        pattern: "^S[0-9]{4}$"
        count: { min: 1, max: 384 }
      rows:
        key: GENE_ID
        count: { min: 50000 }
      cells:
        type: SAS Num                 # salmon counts are fractional
        nullable: No
        range: { min: 0 }
        infinite: false
```

Samples × miRNAs (nf-core/smrnaseq, R `write.csv`, blank corner):

```yaml
    files: { filename: "mature_counts.csv", type: csv }
    row_labels: [ { id: SAMPLE, type: SAS Char, nullable: No } ]
    matrix:
      header: { corner: blank, pattern: "^hsa-(miR|let)-" }
      cells: { type: SAS Int, nullable: No, range: { min: 0 } }
```

Genotypes 0/1/2 with `./.` missing (missing tokens live on the file):

```yaml
    files: { filename: "geno.tsv", type: tsv, missing_values: "./." }
    row_labels: [ { id: SNP, type: SAS Char, nullable: No } ]
    matrix:
      header: { corner: [snp], pattern: "^S[0-9]+$" }
      cells: { type: SAS Int, nullable: Yes, values: [0, 1, 2] }
```

**Corner by writer** (k = number of row labels):

| Writer | Header starts | `corner` |
|---|---|---|
| R `write.csv(x)` (row names) | `"",S1,…` | `blank` (≡ `[""]`) |
| R `write.table(x, sep="\t", quote=FALSE)` (row names) | `S1\t…` — no field | `[null]` |
| R `write.table` of `cbind(gene_name = …, x)` | `gene_name\tS1…` (k=2, row names first) | `[null, gene_name]` |
| pandas `to_csv(index=True)`, unnamed index | `,S1,…` | `blank` |
| pandas, index named `protein_id` | `protein_id,S1,…` | `[protein_id]` |
| pandas, unnamed index + `gene_name` column | `,gene_name,S1,…` | `["", gene_name]` |
| header text = DTA ids | `GENE_ID\tS1…` | `named` |

### 2.2 Key reference

| Key | Allowed | Default | Meaning |
|---|---|---|---|
| `type` | `matrix` | — | Added to `__DTAtools_supported_dataset_types__` (`R/00_helpers.R:151`). |
| `row_labels` | ≥ 1 column specs (same keys as tabular `columns:` entries; `values_from:` only in templates) | required | File column *i* is `row_labels[[i]]`. Entry 1 is the default row key; further entries are **row descriptions** (optional; constrain them like any column, e.g. `nullable: No` to require one). |
| `columns` | — | — | **Rejected** for a matrix (abort + lint `matrix_columns_key`: "use `row_labels:`; data columns are described by `matrix.header` and `matrix.cells`"). |
| `rules` | tabular rules | `[]` | Row-label columns only; ids must not start with `.matrix:`. |
| `matrix.header.corner` | `named` \| `blank` \| list of *k* entries | **required** | Header text above the row labels (cleaned). `named` ≡ the ids; `blank` ≡ all `""`; list entries: a string (`""` = blank) or `null` (**first entry only**) = the header has **no field** there, i.e. *k − 1* corner fields. |
| `matrix.header.pattern` / `values` / `length` | as `DTAColumnSpec` | none | Each data label must satisfy it; `pattern` XOR `values`. `values` must be quoted strings (checked on the raw YAML list — `007` would otherwise become `"7"`). `values` (N entries) + `count: N` ⇒ **exactly those N labels, any order** (uniqueness always enforced). |
| `matrix.header.label` / `description` | text | none | Documentation. |
| `matrix.header.count` | int ≥ 1, or `{min ≥ 1, max}` | `{min: 1, max: unbounded}` | Data columns (never counts the corner). |
| `matrix.rows.key` | id or list of row-label ids | first row-label id | Unique (`.matrix:row_key`), non-missing (key columns must not declare `nullable: Yes`). |
| `matrix.rows.count` | int ≥ 0, or `{min, max}` | `{min: 1, max: unbounded}` | Data rows; `0` (header-only delivery) only when written. |
| `matrix.cells.type` | `SAS Num` \| `SAS Int` \| `SAS Char` | required | |
| `matrix.cells.nullable` | bool | `No` | Whether a **missing** cell (§3.4) is allowed. (Implementation: left unset when not written, which `DTAColumnSpec` already treats as not nullable — same check, and it round-trips without inventing a key.) |
| `matrix.cells.values` / `pattern` / `length` / `format` | as `DTAColumnSpec` | none | Evaluated together with `range` (all must hold). |
| `matrix.cells.range` | `{min, max}`, either optional, `min ≤ max` | none | Inclusive, `Num`/`Int` only (abort + lint for `Char`). Evaluated on infinite values **only when `infinite: true`** (then `+Inf` violates a finite `max`); with `infinite: false` an infinite cell yields the `finite` finding alone. A bound like `1e3` arrives from YAML 1.1 as the string `"1e3"` → abort with hint "write `1.0e+3` or `1000`". |
| `matrix.cells.infinite` | bool | `false` | `Num` only (abort + lint for `Int`/`Char`). Allow `-Inf` but not `+Inf`: `infinite: true` + finite `range.max`. |
| `matrix.cells.label` / `description` | text | none | Documentation. |

Unknown keys under `matrix:` → abort + lint `matrix_field_unknown` (mirrors
`file_field_unknown`, `R/lintDTA.R:922`; there is no generic unknown
dataset-key lint). `matrix:` or `row_labels:` on a non-matrix dataset →
explicit abort + lint `row_labels_key` / `matrix_key`.

---

## 3. Semantics

*k* row labels; *c* corner fields (*k*, or *k − 1* with a `null` first entry);
*h* header fields; *m = h − c* data columns; *r* data rows.

### 3.1 Header

1. **Head records**: leading blank lines skipped, UTF-8 BOM dropped; record 1
   = header, record 2 = first data row (absent for a header-only file); a
   record ends at the first line end outside quotes; no length cap.
2. **Tokenising**: D6. The handler's `missing_values` never apply to the header.
3. **Field-count gate** (before reading data): require
   `fields(record 2) = h + (k − c)`. Mismatch → read error with a hint: one
   fewer → "declare `corner: [null, …]` (R `write.table`)"; one more → "the
   corner has a field you did not declare"; else the ragged hint. Record 1 is
   one field containing another delimiter → "file appears to be X-separated";
   record 1 starts with `#` → "preamble lines are not supported (featureCounts,
   GCT)". Hints asserted by condition class.
4. **Corner**: each of the *c* fields must equal its entry → else `corner`,
   one finding per field, with file position.
5. **Data labels**: each checked as a non-nullable `SAS Char` against `header`
   via `dta_check_column_spec()` (keyword remap): empty → `propertyNames`
   ("column *p* has no label"); pattern/values/length → `propertyNames`. A label
   repeated among data labels **or equal to a row-label id** → `uniqueNames`,
   one finding per repeated label listing positions and reading names. Ordered
   by file position.
6. `m` outside `header.count` → `minProperties` / `maxProperties`.
7. **Reading names**: row-label ids, then per data column its label if
   non-empty and unique among all names, else `"<column p>"`; any label
   colliding with a generated name is itself replaced, until unique. Cell
   findings of a bad-label column carry `"<column p>"`, the same text its
   header finding names, so the two join.
8. Ragged rows further down: Arrow read error (abort, as tabular).

### 3.2 Rows and row descriptions

- Row labels get every tabular check plus user rules.
- Row key: non-missing via `nullable`, unique via `.matrix:row_key`
  (tuple-unique when composite).
- `r` outside `rows.count` → `minItems` / `maxItems` (`nrow()` in memory, the
  scan's row total when streaming). Partial scan (`fail_fast`): `max` already
  exceeded → fail; `min` already reached → pass; else `not_checked` —
  certainty about this check only. `on_missing_column = "stop"` has no effect
  for a matrix (documented).
- `SAS Char`: a whitespace-only label is present text (pinned tabular
  behaviour); constrain the key with `pattern`.

### 3.3 Dimensions

Scalar = exact (`count: 96` ≡ `{min: 96, max: 96}`); `{min: n}` = at least;
`{max: n}` = at most (min stays default); absent = default. Non-integer,
negative, `min > max`, `header.count` < 1 → abort + lint.

### 3.4 Cell values — the contract

Typed exactly as a tabular column of the cell type; the matrix adds `range`
(`minimum`/`maximum`) and `infinite` (`finite`). **Missing** = an empty field,
`NA`, quoted `""`/`"NA"`, a token declared in `files.missing_values`
(case-sensitive, added to `""` and `NA`), and — for `SAS Num`/`SAS Int` only —
a whitespace-only field. Empty and `NA` are deliberately not distinguished;
`NaN` is never a value — declare `files.missing_values: "NaN"` to treat it as
missing (user decisions).

| Raw cell | `SAS Num` | `SAS Int` | `SAS Char` |
|---|---|---|---|
| `12`, ` 12 `, `+12`, `012`, `1.2e1` | 12 | 12 | text as delivered |
| `1.5` | 1.5 | import `not_whole` | text |
| `1.0`, `1e3`, `1e+05`, `123.0` | 1, 1000, 1e5, 123 | same values | text |
| `0x1F` | 31 (accepted package-wide; LIM) | 31 | text |
| `2147483648` | 2147483648 | import `out_of_range` | text |
| `Inf`, `-Inf`, `inf`, `Infinity`, `1e400` | ±Inf → `finite` only; with `infinite: true`, `range` applies | import `not_whole` | text |
| `1.7976931348623157e308` | finite | import `out_of_range` | text |
| `NaN`, `nan`, `-nan` | import `not_convertible` (never a value) | same | text |
| `N/A`, `NULL`, `null`, `n/a`, `#N/A`, `" NA "` | import `not_convertible` (**not** missing) | same | text |
| `1,5` (TSV), `abc`, `TRUE` | import `not_convertible` | same | text |
| empty, `NA`, quoted `""`/`"NA"`, declared token | missing | missing | missing |
| whitespace-only | missing | missing | **present** text |

- Missing + `nullable: No` → `type` finding (existing wording).
- A cell failing typing is NA afterwards: import finding **plus**, with
  `nullable: No`, a `type` finding — two per cell, as in tabular; never a
  range finding.

### 3.5 Findings

| Finding | Axis | Keyword / rule | row | column |
|---|---|---|---|---|
| corner field wrong | column spec | `corner` | NA | row-label id |
| data label empty / fails header spec | column spec | `propertyNames` | NA | label or `<column p>` |
| data label repeated | column spec | `uniqueNames` | NA | label |
| data-column count | column spec | `minProperties` / `maxProperties` | NA | NA (bound in `columnspec`) |
| data-row count | column spec | `minItems` / `maxItems` | NA | NA (bound in `columnspec`) |
| cell below / above range | column spec | `minimum` / `maximum` | row | reading name |
| cell infinite | column spec | `finite` | row | reading name |
| cell missing, nullable No | column spec | `type` | row | reading name |
| cell values / pattern / length / format | column spec | existing | row | reading name |
| typing (cells and labels) | import | `not_convertible` / `not_whole` / `out_of_range` | row | reading name |
| row key repeated | rules | `.matrix:row_key` | NA | key |
| stale / missing head, changed file | table failure | missing-file shape (D13) | — | — |
| gate / ragged / quoted newline in header | read error | abort with hint | — | — |

**Vocabulary** (`R/columnSpecChecks.R`, unchanged on the container base):
- New keywords join the **global** tally vocabulary
  (`dta_columnspec_check_kinds()`, `:773`) with a column `matrix_only = TRUE`,
  so no call site can leave a keyword uncounted.
- The per-check summary (`dta_columnspec_check_summary()` `:959`,
  `dta_structural_failure_details()` `R/streamingValidation.R:3012`,
  `R/DTADataSetFile-class.R:464`) **drops `matrix_only` kinds unless
  `matrix = TRUE`** (measured: without it a tabular `columnspec_checks` grows
  from 8 to 18 rows, breaking `OQ-VAL-columnspec.R:316-320` and
  `REQ-VAL-013`). With the flag it adds `header`, `dimensions`, `range`,
  `finite` (denominators from `structure`) and reports `presence`/`extra` as
  `not_applicable` with a matrix reason.
- The summary carries `attr(, "matrix")`; `dta_report_columnspec_checks()`
  branches on it at its four call sites (`R/validationFunctions.R:230`,
  `R/streamingValidation.R:1189, 2268, 2893`).
- `__DTAtools_columnspec_check_labels__` (`:1092`) **and**
  `__DTAtools_columnspec_check_absent__` (`:1054`) gain entries (without the
  latter a verbose matrix check crashes, measured); explanations in
  `R/reportRendering.R:846-851`.
- `dta_columnspec_tally_add()` (`:892-896`) fills an `NA` `column` from
  `columnspec` only where `keyword == "required"` (else a phantom column is
  tallied, measured).
- `settled` gains the header keywords (any scan) and the row-count keywords
  (per §3.2) at `R/streamingValidation.R:1159-1181` and
  `R/validationFunctions.R:226`.
- `fail_fast` stops after one batch once a header finding is in the sink;
  the loop is **never** skipped (with `row_offset == 0` the empty-stream branch
  `:1167-1172` would report a false `minItems`) — pinned by a test.

---

## 4. Architecture

### 4.1 Classes (one per file; `@include` drives `Collate:`)

- `DTAColumnSpecCell` (internal): parent `DTAColumnSpec`; `minimum`, `maximum`
  (num/NULL), `finite` (lgl/NULL); `as_json_schema()` = parent + keys; **own
  `as.list()`** writing `range`/`infinite` (the inherited one drops them,
  measured); validator per §2.2.
- `DTAMatrixSpec` (internal): `header` (`DTAColumnSpec`), `corner` (chr, `NA`
  = `null`), `n_columns`, `n_rows` (num[2], max may be `Inf`), `row_key`
  (chr), `cells` (`DTAColumnSpecCell`); `as.list()` = §2 shape;
  `dta_matrix_spec_from_list()` with unknown-key and raw-YAML checks.
- `DTADataSetMatrix` (exported): parent `DTADataSetTabular`; `matrix`,
  `heads` (`new_property(class_list, default = list())` — a non-NULL default
  is what `dta_upgrade()` fills, `R/containerAssignments.R:169-171`).
  Validator: ≥ 1 row label; `row_key ⊆` label ids; key columns not
  `nullable = TRUE`; corner length = *k*, `null` only first; handlers
  `DTAFileCSV`/`DTAFileTSV` with `has_header`; tables Arrow `Table` or
  `Dataset` only; user rules on label columns, no `.matrix:` ids. No
  invariant between `heads` and `tables` (the app clears per-table state in
  a fixed order, `inst/shiny/dta_app/R/utils_dta.R:1249-1256`).
- All three are top-level namespace objects named like their classes —
  `dta_is_stale()`/`dta_upgrade()` look classes up by name
  (`R/containerAssignments.R:130, 156`).

### 4.2 Reading

- `dta_head_records(path, n, quote, delim)` — shared, chunked, quote-aware, no
  cap; `dta_header_layout()` (`R/DTAFileTabular-class.R:1069-1120`) is
  reimplemented on it (wide-header fix for tabular too).
- `dta_matrix_read_head(handler, path)` → `list(fields, n_first_row, hint)` per
  D6 (newline, block size, full binary `col_types`; schema built in O(m) —
  1.65 s vs 8.1 s at 20k measured via `do.call`, which cannot carry empty
  names; step 0 picks a constructor that handles both).
- `column_names = NULL` threaded: `read_file()` / `open_file()` on `DTAFile`
  (`R/DTAFile-class.R:725, 860`, signatures unchanged on this base) →
  `dta_reader_args()` (`R/DTAFileTabular-class.R:273-305`, picks named args
  by exact match) → the six CSV/TSV/Delim `*_execution` methods (e.g.
  `R/DTAFileCSV-class.R:112-113, 139-140`) → `dta_read_delim_normalized()` /
  `dta_open_normalized_dataset()` (`:1394-1552`) → `dta_delim_reader_plan()`
  (`:1260-1347`): when given, skip `dta_delim_header_names()` **and** the
  cleaning/duplicate step (`:1314-1326`) — names verbatim. The transcode
  re-open records `column_names` in its arguments (`:1052, 1466-1478`).
- **Load body factored once**: `dta_load_tabular(x, file, handler, name,
  stream, column_names = NULL, coerce_specs = x@specs)` = the current body of
  `load_file(DTADataSetTabular)` **after its guards** (`R/DTADataSetTabular-class.R:914-993`:
  provenance record, replacement clearing, lazy/eager read, coercion, import
  issues, `dta_set_provenance(x, name, rec)` on both returns). The guards stay
  in each method, in this order: `dta_is_stale()`/`dta_upgrade()`
  (`:891-893`), handler-index resolution, `dta_assert_single_file_path()`,
  the `name` check (reason at `:903-906`), `dta_assert_not_container()`
  (`:913`). Tabular: guards → `dta_load_tabular()` (behaviour-neutral). The
  `name` default moves to `dta_default_table_name(file)` for both signatures.
- `load_file(DTADataSetMatrix)`: guards (so a `.zip`/`.xpt` path is refused
  before any head read, and a stale object is upgraded before `@heads` or
  `@matrix` is touched) → head read → gate → reading names →
  `dta_load_tabular(column_names =, coerce_specs =)` with effective specs built
  **only on the eager branch** → `x@heads[[name]] <- head`. Provenance is
  written inside `dta_load_tabular()`; `load_delivery()` then overwrites it
  with the container chain (`R/loadDelivery.R:232`).

### 4.3 Effective specs

`dta_matrix_effective_specs(x, names)`: label specs + cell clones +
`.matrix:row_key`. Clones never pass through `S7::validate()`. Built per
`check()` and per eager load.

### 4.4 Hooks in `DTADataSetTabular` (no copy of `check()`)

| Internal generic | Tabular | Matrix | Call sites |
|---|---|---|---|
| `dta_table_specs(x, table)` | `x@specs` | effective specs from `names(table)` | `check()` `R/DTADataSetTabular-class.R:1689`; `inspect()` `R/validationReporting.R:1390` |
| `dta_specs_fingerprint(x)` | `as.list(x@specs)` | `list(as.list(x@specs), as.list(x@matrix))` | `check()` `:1469` |
| `dta_table_structure(x, name, table)` | `NULL` | recompute reading names from `@heads[[name]]`; lazy: re-read the source head; missing head, mismatch or changed file → a table failure (D13) that `check()` records like an absent file and `next`s; else header findings + row bounds + flag | before `dta_validate_any_table()` `:1688`, after the transcode refresh `:1601` (verified: the hook sees the refreshed table) |

`check(DTADataSetTabular)` keeps its `.container_globs` argument (`:1431`);
a matrix never overrides `check()` (`check(DTA)` passes that argument,
`R/DTA-class.R:1117-1125`). `dta_find_rule_def()` (`R/validationReporting.R:567`;
callers `explain_messages()` `:641`, `write_validation_report()`
`R/exportValidationReport.R:162`) resolves `.matrix:row_key` from `x@matrix`.

### 4.5 Structure into the engine

`dta_validate_any_table(..., structure = NULL)` → `validate_table_detailed()` /
`dta_validate_table_stream()`; retention per D7; vocabulary per §3.5;
`structure = NULL` is today's path, byte for byte.

### 4.6 `row_labels:` at the boundaries

```r
dta_dataset_spec_key <- function(body) {
  type <- body[["type"]]
  if (is.null(type)) {
    # A child template's inline dataset may omit `type` (merged over the
    # parent's by key, template_inherit.R:483): decide from the keys present.
    return(if (!is.null(body[["row_labels"]]) && is.null(body[["columns"]])) "row_labels" else "columns")
  }
  if (identical(type, "matrix")) "row_labels" else "columns"
}
```

Writers call it only on fully resolved bodies. The app cannot see an
unexported package function (helpers are sourced into an environment whose
parent is shiny's namespace, `tests/testthat/helper-shinyapp.R:85`;
`load_all()` hides this), so the app uses the accessor pattern of
`utils_dta.R:2355`:
`dta_dataset_spec_key <- function(body) get("dta_dataset_spec_key", envir = asNamespace("DTAtools"))(body)`.

- **In**: `DTADataSetFactory(type, columns, rules, files, matrix, row_labels)`
  (`R/00_helpers.R:618-650`) → matrix: `specs_from_list(columns = row_labels,
  rules)`; wrong key for the type → abort.
- **Lint**: `R/lintDTA.R:880-884` admits matrix past the tabular gate and lints
  `d[[dta_dataset_spec_key(d)]]` (`dta_lint_dataset_list(d, label,
  container_ids)` keeps its new argument); the message at `:974` names the
  key; findings keep kind `column` and the row-label id; lints
  `matrix_columns_key`, `row_labels_key`, `matrix_key`, `matrix_field_unknown`,
  plus `dta_lint_matrix_block()`. The file-name-conflict lint and container
  lints are type-agnostic and need nothing.
- **Out**: app `dta_dataset_to_list(ds, dta, apply_default)`
  (`utils_dta.R:2854`; `out$columns` at `:2886`) writes the specs under the
  helper's key and `matrix` via `as.list()`. **`.dta_compact()`
  (`utils_dta.R:2403`, run at `:2917, :2955`) drops `NULL` list elements
  recursively** and would erase a `null` corner entry: it gains one guard that
  leaves a list classed `dta_verbatim` untouched, and the corner is emitted
  with that class (`yaml::as.yaml` then writes `~`).
- **Body sites switched to the helper**: `R/validateTemplate.R:1024, 1176`
  (`:1179` keeps `columns =` as a standalone lint input and additionally lints
  `matrix:`), `R/templateGuide-collect.R:1483, 1496`; app
  `dataset_template.R:226-231` (path root must equal the body's key, else abort
  with a hint — an unknown root silently adds a key at `:247` today),
  `:297-372` (patch ops keep their names and act on `ds[[key]]`),
  `vocabulary.R:892-919, 1007, 1038-1077`, `template_diff.R:184-194`,
  `template_history.R:172-174` (key per side; a type change reads as
  "remove `columns.*`, add `row_labels.*`, `type` changed").
- **Path parsers** accept `columns` or `row_labels`, checked against the
  dataset when known: `vocabulary.R:663-710`, `R/templateGuide-collect.R:520`,
  `template_history.R:193` ("row label").
- **Unchanged**: standalone spec files (`R/DTAColumnSpecCollection-class.R:355-534`),
  a vocabulary's own `columns:` (`vocabulary.R:302, 541`,
  `R/templateGuide-collect.R:1558`), `R/templateGuide.R:763`, rule `columns`
  slots, S7 `@columns`.

### 4.7 Container-base obligations

| Mechanism (on `f6189bf`) | What the matrix does |
|---|---|
| `dta_is_stale()` / `dta_upgrade()` (`R/containerAssignments.R:121-200`) | guards first in `load_file()`; `heads` has a non-NULL default; classes resolvable by name |
| `dta_assert_not_container()` (`R/DTAFile-class.R:544`) | called in the matrix guards, **before** the head read |
| `provenance` (`R/DTADataSet-class.R:83, 111-125`) | written by `dta_load_tabular()`; overwritten by `load_delivery()` (`R/loadDelivery.R:232`); `container_findings()` (`R/containerFindings.R:55-168`) then sees the table as recorded |
| `load_delivery()` routing (`R/loadDelivery.R:383-752`; `dta_delivery_item()` `:403-405`) | none: type-agnostic routing, extension stripped as for tabular, binds through `load_file()` |
| `check(DTA)` `.container_globs` (`R/DTA-class.R:1117-1125`) | inherited `check()` accepts it |
| `file_name_conflicts()` (`R/fileNameConflicts.R:302`), container lints | none: type-agnostic |
| App transfer `dta_transfer_bound_data()` (`utils_dta.R:4035-4064`; provenance copy `:4047-4051`) | copy `@heads` filtered to bound tables the same way; this also covers the second caller `rebuild_from_yaml` (`app.R:10212-10213`) |
| App unload (`dta_drop_provenance()` `utils_dta.R:1241`, called `:1285, :1291, :1316`) | drop `@heads[[name]]` alongside |
| App session dump/restore (`utils_dta.R:4084-4120`; restore upgrades at `app.R:10250`) | none: `@heads` travels in `dump$dta` |
| Export `ctx`/`dataset` arguments and Containers chapter (`R/exportDocuments.R:687-708`, `R/documentData.R`) | the "Matrix layout" block sits beside the `if (tabular)` branch and takes the same arguments |

---

## 5. Blast radius

| Site | Change |
|---|---|
| `R/00_helpers.R:151`, `:618-650` | allowlist; factory `matrix`, `row_labels` + aborts; `dta_dataset_spec_key()` |
| `R/DTADataSet-class.R:9-11` | `type` docs |
| `R/DTADataSetTabular-class.R:877-993, 1469, 1601-1689` | guards + `dta_load_tabular()`; hooks |
| `R/DTAFile-class.R:725, 860`; `R/DTAFileTabular-class.R:273-305, 1052, 1069-1120, 1260-1347, 1394-1552`; `*_execution` ×6 | `column_names`; head reader; transcode args |
| `R/validationFunctions.R:119-347` (`:187`, `:226`, `:230`, `:319`); `R/streamingValidation.R:826-1366` (`:1148-1149`, `:1159-1181`, `:1189`), `:2229+`, `:2268`, `:2893`, `:3012` | `structure`; retention; `settled`; report branch |
| `R/columnSpecChecks.R:110-178, 634, 773, 802-849, 892-896, 959, 1054, 1092` | keyword evaluation; hoisted `%in%`; `matrix_only` vocabulary; tally fill-in; summary filter + attribute; labels and `_absent_` entries |
| `R/reportRendering.R:809-880`; `R/validationReporting.R:567, 1390` | explanations; rule lookup; inspect |
| `R/lintDTA.R:865, 880-884, 974, 1067` | §4.6 lint |
| `R/exportDocuments.R:687-708, 1587`; `R/documentBuilders.R:923`; `R/exportTemplateBlocks.R:371, 469` | inherited tabular branch, row-label table titled "Row label columns", + one "Matrix layout" block (MD + DOCX) |
| `row_labels` sites | §4.6 |
| app `utils_dta.R:2403` (`.dta_compact`), `:2854-2891` (`dta_dataset_to_list`) | `dta_verbatim` guard; write `row_labels`/`matrix` |
| app `utils_dta.R:4015-4030` (`dta_specs_signature`) | include `matrix` (a Raw-YAML edit of `matrix:` must invalidate validation) |
| app `utils_dta.R:4035-4064`, `:1241-1316` | copy / drop `@heads` (§4.7) |
| app `app.R:8889, 8897` | type-change guard as a `next` beside these lines (not inside the transfer function: on `tr$ok` the caller moves upload records, `:8913-8926`) |
| app `app.R:7510-7511` (inspect panel) | `minimum`/`maximum` fall back to `columnspec_columnspec` when `columnspec_params.limit` is absent |
| app `utils_dta.R:2353-2358` + `test-shinyapp-utils.R:141` | `"matrix"` in supported types |
| app editors, `dta_add_dataset()` (`utils_dta.R:3088-3099`, hard-codes tabular/file) | unchanged in v1 (no "add matrix" dialog); no-crash `testServer` |
| `inst/extdata/template-repo-skeleton/README.md:499-507, 657-662, 742-778` | `row_labels` in slot targets, `set:` paths, patch ops |
| `R/exportTemplateRegions.R`, `R/exportTemplateDocx.R`, `R/loadDelivery.R`, `R/DTA-class.R`, `R/fileNameConflicts.R`, `R/containerFindings.R` | none (inherit / type-agnostic) — pinned by tests where behaviour matters |

---

## 6. Red-team register

| # | Threat | Mitigation | Test |
|---|---|---|---|
| T1 | App save drops `matrix:` or a `null` corner | `dta_dataset_to_list` + `dta_verbatim` | load → rename → save → re-read → identical, incl. `corner: [null, gene_name]` |
| T2 | `matrix:` / `row_labels:` on tabular, `columns:` on matrix | explicit abort + lint | yes |
| T3 | Hollow pass | D10 | header-only (default) → `minItems`; `count: 0` explicit → pass |
| T4 | Duplicate / double-blank labels crash Arrow | D6 + reading names | `uniqueNames`, cells still validated, join on `<column p>` |
| T5 | Label corruption (`007`, `""`, `NA`, `.`, latin1) | `binary()`, `na = character(0)`, `iconv` | per case, eager + lazy |
| T6 | Short / mixed / spaced corner | corner list | each §2.1 writer row |
| T7 | Field-count gate wrong hint | §3.1.3 | one fewer / one more / ragged, by class |
| T8 | Wide header > 1 MiB | `dta_head_records()` | 60k quoted labels, matrix **and tabular** |
| T9 | Structural finding evicted / order differs by path | D7 | `max_errors = 2` and `0`, both paths |
| T10 | Heads lost (refresh, restore, Raw-YAML and `rebuild_from_yaml` transfer, unload) | D13, §4.7 | each path |
| T11 | Eager table stale after spec edit | recompute in `dta_table_structure()` | table failure; other datasets of the DTA still validate |
| T12 | Lazy file changed in place | head re-read | table failure, not an abort |
| T13 | §3.4 tokens | contract table | oracle × {never, always} |
| T14 | Vocabulary flag missed | global tally vocabulary | any matrix keyword → `columnspec_valid = FALSE` and a failed summary row |
| T15 | Stale verdict after `matrix:` edit | fingerprint; app signature | change `range` → revalidated |
| T16 | Subclass props lost in `as.list` | own `as.list()` | round trip |
| T17 | Generated rule unresolvable | `dta_find_rule_def()`, inspect | explain + report |
| T18 | Labels with `{ }`, spaces | interpolation; clones skip id validator | `a{b}`, `S 1` |
| T19 | YAML traps (`[007, Y]`, `1e3`, `1.5`, `min > max`, range on Char, `infinite` on Int, corner `[1]`, `[~]` vs `[""]`) | raw-list checks + hints | yes |
| T20 | Partial scan certifies row count | §3.2 | `fail_fast` cases |
| T21 | Locale | file-position order | `de_DE` and `C` |
| T22 | macOS arm64 bound parsing | bound with 17 significant digits; value equal to the bound passes | CI arm64 leg |
| T23 | Export omits matrix semantics | layout block | read MD/DOCX back |
| T24 | Transposed delivery | `propertyNames` / `maxProperties` | yes |
| T25 | Error volume | unchanged cap/spill; vignette: `fail_fast = TRUE` first | doc |
| T26 | Tabular regressions | D12 | full suite; tabular summary pinned |
| T27 | A site still reading `columns` (missing `columns` reads as "nothing to do") | §4.6 helper + app accessor | against a matrix **template**: row labels linted, `validate_template()` reports them, `values_from` expands, slot override finds the label, guide lists it, diff keys by id; patch `dataset.row_labels.<id>.<field>`; accessor works from the app environment, not only under `load_all()` |
| T28 | Phantom-column tally | fill-in only for `required` | `failed_columns` excludes bounds |
| T29 | `fail_fast` loop skipped → false `minItems` | never skip | header finding + `fail_fast` |
| T30 | Explicit names cleaned twice | verbatim `column_names` | label `'"" a'` round trips |
| T31 | Container path head-read before the guard; stale object touched before upgrade | guard order §4.2 | `.zip` path to `load_file()` refused before any read; a 0.27.0-shaped object loads |
| T32 | Matrix delivered inside a container | §4.7 | `load_delivery()` binds a matrix from a `.zip`; `container_findings()` sees it recorded; `route =` / `datasets =` work for a matrix |

---

## 7. Step 0 — measure first (spike, not committed)

Measured so far: plain tabular 200 × 5,000 `Int` — eager load 17.8 s, check
3.3 s; streaming 5.3 s / 6.7 s. Matrix fixed per-check overhead at 20k ≈ 14 s.
Existing `utf8()` schema build ≈ 8 s per load at 20k.

1. Shapes (plain tabular + programmatic specs, on `f6189bf`): wide 1,000 ×
   20,000 `Int`; bulk 60,000 × 2,000 `Num`; tall 60,000 × 500 `Num`; EPIC
   866,000 × 500 `Num` 0..1 with NA (streaming). Load + check,
   `never`/`always`, peak RSS, `Rprof` on wide.
2. Behaviour-neutral hot-spot fixes in shared code, one commit each, existing
   tests unchanged: `%in%` per schema (`R/columnSpecChecks.R:634`); O(m) schema
   construction (`R/DTAFileTabular-class.R:1333`), empty names preserved;
   what `Rprof` shows in per-column coercion; optionally compile the shared
   cell schema once.
   **Baseline measured 2026-11-13** (plain tabular on `f6189bf`, expanded
   specs, one process per case, R 4.5.1, ~3 GB free RAM; build / load / check
   in s, peak working set):

   | shape | mode | build | load | check | peak MB |
   |---|---|---|---|---|---|
   | wide 1k × 5k `Int` | stream | 0.2 | 3.7 | 10.7 | 513 |
   | wide 1k × 5k `Int` | eager | 0.1 | 13 | 1.6 | ~450 |
   | wide 1k × 20k `Int` | stream | 0.4 | 27 | 391–447 | 978 |
   | wide 1k × 20k `Int` | eager | 0.3 | 86 | 11.3 | 970 |
   | tall 10k × 500 `Num` | eager | 0.1 | 7.4 | 0.4 | 479 |
   | tall 60k × 500 `Num` | stream | 0.1 | 0.5 | 40 | 825 |
   | bulk 6k × 2k `Num` (10 %) | stream | 0.1 | 1.4 | 18 | 622 |
   | EPIC 86.6k × 500 (10 %, 2 % NA) | stream | 0.1 | 0.7 | 14.5 | 854 |

   Extrapolated (linear in cells): bulk 60k × 2k ≈ 180 s, EPIC 866k × 500 ≈
   145–560 s streaming. All targets below are met by the baseline, but the
   streaming check grows **quadratically** with column count (4× columns →
   36× time). Profiles: `df[[column]] <- coerced$values` per column in
   `dta_coerce_table_to_specs()` (`R/importConversion.R:527`; 41 % of the wide
   streaming check, 37 % of the wide eager load) plus its per-column `%in%`
   and `[[.data.frame`; one `arrow::utf8()`/`arrow::field()` R6 object per
   column in `dta_delim_reader_plan()` (90 % of the wide streaming load). The
   suspected `%in%` at `R/columnSpecChecks.R:634` does not show.
   **After step 2** (A/B, one process per run, interleaved, same machine
   state; `R/importConversion.R` loop on a plain list + one shared
   `arrow::utf8()`; equivalence `identical()`/`$Equals()` against `f6189bf`):

   | case | load before → after | check before → after |
   |---|---|---|
   | wide 1k × 5k, stream | 3.5 → 1.6 s | 13.2 → 8.7 s |
   | wide 1k × 20k, stream | 14.8 → 7.6 s | 407.7 → 124.4 s |
   | wide 1k × 20k, eager | 94.2 → 59.4 s | 14.9 → 11.7 s |

   Open: eager 20k peak `gc` max went 495 → 648 MB in one run — re-measure in
   the final review before calling it noise.
3. Targets (numbers recorded here after step 2): wide ≤ 5 min in memory /
   ≤ 10 min streaming; bulk ≤ 10 min; EPIC ≤ 60 min streaming; matrix ≤ 1.25×
   plain tabular on the same data. A miss goes to the user with the profile;
   no silent redesign. Dense single-cell CSV is out of scope.

---

## 8. Implementation order

0. New branch from `f6189bf` (the source branch is checked out elsewhere, so a new name); copy this plan to `thoughts/shared/plans/PLAN-matrix-dataset.md`.
1. Step 0 + hot-spot commits.
2. `dta_head_records()`; `dta_header_layout()` on it (tabular fix + test).
3. `column_names` chain + transcode args (eager/lazy, latin1, gz tests).
4. `DTAColumnSpecCell` + keyword evaluation; `DTAMatrixSpec` + from-list.
5. Engine `structure` + retention + vocabulary/summary/settled/labels/tally.
6. `DTADataSetMatrix`, factory, allowlist, guards + `dta_load_tabular()`, hooks, head reader, `@heads`, rule lookup, §4.7 obligations.
7. Lint, `validate_template()`, guide sites (§4.6).
8. Export block.
9. App: `dta_dataset_to_list` + `.dta_compact` guard, signature, transfer/unload of `@heads`, type-change guard, inspect fallback, supported types, template-engine sites, helper accessor.
10. `inst/extdata` fixtures (the three §2.1 examples, small); CHANGELOG `[Unreleased]` (`Added`; `Fixed` wide headers); vignette section; README types sentence + class tree; template-repo-skeleton README.
11. Qualification (§9); `Rscript .github/scripts/sync_qualification_manifest.R` after every `inst/` change.
12. `Rscript .github/scripts/style.R`; roxygenise with `Config/roxygen2/version` (8.1.0); `rcmdcheck(args = "--no-manual", env = c(NOT_CRAN = "true"))`; `r-review`.

Sonnet implementers with exact per-file specs, one writer per file; then a
full-strength adversarial review with randomized differential testing of a
matrix against the equivalent hand-declared tabular spec.

---

## 9. Tests and qualification

- testthat: `test-DTAColumnSpecCell.R`, `test-DTAMatrixSpec.R`,
  `test-DTADataSetMatrix.R`, `test-matrix-reading.R`, matrix cases in
  `test-loadDelivery.R` / `test-container-findings.R` (T32); fixtures in
  `tests/testthat/fixtures/matrix/`; every semantic case on `never` and
  `always`; conditions by class.
- Qualification (`inst/qualification/docs/maintaining.md` first; unchanged on
  this base):
  - area `MATRIX` in `_index.yaml`; `min_tests` raised in **every** area that
    gains tests — current floors SPEC 34, FILE 54, EXPORT 73, APP 38,
    TMPL 114, VAL 46, CONT 62;
  - `REQ-MATRIX.yaml` **written before tests**: one requirement per §3 rule and
    per §3.4 row (token lists in the text, so every expected value comes from a
    sentence), plus `row_labels:` acceptance/rejection;
  - `test-OQ-MATRIX-*.R`; traceability = OQ titles
    (`OQ-MATRIX-nnn | … | REQ-MATRIX-nnn`), enforced by M1/M3/M4;
  - `risk-assessment.yaml` entry for the area;
  - LIM `affects` (LIM-001, LIM-002) and new LIMs from **LIM-023** (019–022 are
    taken by CONT), each with the full schema and a `qa_step()` pin naming it:
    whitespace-only Char present; `N/A`-family not missing; hex / `+` / leading
    zeros accepted; empty and `NA` indistinguishable;
  - rewritten texts: `REQ-VAL-013` (`REQ-VAL.yaml:159-166`, matrix-only kinds
    only for matrix datasets), `REQ-SPEC` (dataset classes, factory,
    `lint_dta`), `REQ-EXPORT` (`write_dta`), `REQ-APP` (save), `REQ-FILE` (wide
    header), `REQ-TMPL` (row labels in patches, slot targets, `values_from`),
    `REQ-CONT` where a matrix-in-container case is added (T32);
  - `_api.yaml` (sorted; it gained 5 container exports on this base) +
    `covers:` for `DTADataSetMatrix`;
  - `sync_qualification_manifest.R` after every `inst/` change
    (`baseline/inst-files.sha256`); deviations checked.

## 10. Docs

Roxygen with runnable examples reading `inst/extdata`; vignette section after
the File section (examples, corner-by-writer table, §3.4 table, `fail_fast`
advice, matrix inside a container); README types sentence + class tree;
template-repo-skeleton README; CHANGELOG. No version bump.

## 11. Out of scope (v1)

Preamble lines (featureCounts, GCT — candidate `skip_lines:`); semicolon /
decimal-comma files; trailing annotation columns (candidate
`matrix.trailing:`); header-less / label-less matrices; sparse and single-cell
formats; symmetric matrices; cross-dataset references (v1 recipe: quoted
`header.values` + exact `count`); cross-file consistency; missingness limits;
column order; template guide / regions matrix blocks; app editor and "add
matrix" dialog; SAS/XPT and `Delim` matrices; in-memory `tables =`.

## 12. Decisions taken

By the user (2026-11-13):
- Q1 Empty vs `NA`: **one notion of missing** (empty, `NA`, `files.missing_values` tokens); `cells.nullable` decides.
- Q2 NaN: **never a value** — import error, or missing if declared as a token.
- Q3 Hex / `+12` / `012`: **accepted as in tabular**; recorded as a LIM.
- Q4 Row-label key: **`row_labels:`**; `columns:` rejected for a matrix (§4.6).
- Base: **`ai/multi-dataset-zip-xpt-5be318`** (`f6189bf`).

By the plan: `corner` required (no default — `named` is wrong for most real
files); zero data columns never valid, zero rows only when written; lazy drift
caught by a head re-read.

## 13. Verification

- `devtools::test()` green; `rcmdcheck` `Status: OK` with `NOT_CRAN = "true"`.
- `run_qualification()` meta-checks green; qualification manifest `--check` clean.
- Manual: the three `inst/extdata` examples through `read_dta_from_yaml()`,
  `load_delivery()` (loose and inside a `.zip`), `check()`, `messages()`,
  `explain_messages()`, `container_findings()`, `write_dta("md"/"docx")` read
  back; app: load, validate, Raw-YAML edit of `matrix:` (revalidates), session
  restore, save, diff YAML.
- Step-0 shapes re-run on the finished code; within targets.

## 14. Revision history

- **r0→r1**: range/finite from 20k rules to cell keywords; corner lists with
  `null`; per-record head tokenising; structural findings into the engine;
  qualification obligations; §3.4 corrected; real-workload perf shapes.
- **r1→r2**: structural findings retained ahead of the cap; heads as a dataset
  property; recompute-and-compare guards; head-reader specifics;
  `dta_load_tabular()`; global tally vocabulary; own `as.list()`; `settled`;
  `corner` required; zero-column/zero-row rules; corner-by-writer table.
- **r2→r3**: user decisions Q1–Q4; `row_labels:` with one boundary helper.
- **r3→r4** (round 3, no blocker): helper handles a missing `type`, app
  accessor; classified `row_labels` sites; `range` on infinite only with
  `infinite: true`; finding order and streaming insertion point; `matrix_only`
  vocabulary + summary filter + report attribute + `_absent_` entries; tally
  fill-in; stale heads as a table failure; verbatim names; guard order; app
  transfer/type-change/inspect; `REQ-VAL-013`; traceability via OQ titles.
- **r4→r5**: rebased onto `ai/multi-dataset-zip-xpt-5be318` (`f6189bf`): all
  line numbers remapped; §4.7 container obligations (stale upgrade, container
  guard before the head read, provenance through `dta_load_tabular()`,
  `.container_globs`, routing, `@heads` copy/drop beside provenance incl. the
  second transfer caller); `.dta_compact()` would erase a `null` corner —
  `dta_verbatim` guard; LIM numbering from 023; current `min_tests` floors;
  T31, T32.
