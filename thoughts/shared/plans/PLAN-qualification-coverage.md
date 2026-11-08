# PLAN: Line coverage of R/ by the qualification suite to >95%

**Status:** approved 2026-11-05; Phase 0 in progress
**Target branch:** `dev`
**Version bump:** none

## 1. Measured baseline (2026-11-05, Windows, R 4.5.1, covr)

Qualification run: quick tier, `formats = "md"`, `include_unit_tests = "never"`;
474/478 cases passed, 4 not executed by design, 14/14 meta checks passed.

| Scope | Qualification alone | Dev suite | Union |
| --- | ---: | ---: | ---: |
| All of `R/` (8,580 lines) | 75.3% | 88.1% | 89.9% |
| Product code (7,751 lines) | 75.5% | -- | 91.2% |

"Product code" excludes the runner (`R/qualification.R`,
`R/qualification-report.R`), `R/run_dta_app.R` and `R/benchmarking.R`.

95% of product code needs about 1,514 more lines executed by the qualification
suite: ~1,220 are run only by the dev suite (port them as traced OQ cases),
~300 are run by no test at all. Of the 683 product lines no test reaches,
~520 are logic, ~130 are error/guard branches, ~33 print/message.

The figure is the qualification suite **alone**. `include_unit_tests = "auto"`
would fold the dev suite in, but unit tests are not traced to requirements, so
they would weaken the evidence that the number stands for.

## 2. Phases

### Phase 0 -- measure in CI, fix the scope

- Second job in `.github/workflows/test-coverage.yaml` measuring the
  qualification suite with `covr::package_coverage(type = "none", code =
  run_qualification(...))`, runner files excluded via `line_exclusions` in that
  job only (the dev-suite job keeps its denominator). Floor ratchets with each
  phase: 73 (baseline - 2.5) -> 85 -> 92 -> 95.
- `inst/qualification/docs/validation-plan.md` section 4b states the scope and
  why the runner is out of it (it is test tooling; its execution is the IQ and
  OQ-CORE evidence).

### Phase 1 -- highest risk (~85%)

Each with a requirement, a risk entry and OQ cases.

1. UTF-8 transcoding, 0% today (`dta_transcode_to_utf8`, `dta_transcode_spans`,
   `dta_transcode_write_block`, `dta_first_undecodable_byte`,
   `dta_last_newline`; ~90 lines): Latin-1, CP1252, BOM transcode byte-exact;
   an undecodable byte is reported with its offset.
2. `dta_arrow_parse_numeric_batch` (41 lines): streaming == in-memory for
   declared numeric types, edge values included.
3. `check()` branches of `DTADataSetTabular`, `DTADataSetFile`, `DTA`
   (~93 lines); `validate_table_detailed`, `dta_normalize_conditions`.

### Phase 2 -- port the remaining dev-only behaviour (~92%)

Reporting (`dta_report_columnspec_checks`, `dta_inspect_tabular_message`,
`dta_flatten_inspect_record`, `messages`/`results`/`inspect` methods), document
export (`.tv_block_render_datasets`, `.tv_markdown_bullet_to_word_bullet`,
`.tv_substitute_paragraphs`, `write_dataset_metadata`,
`.write_dataset_markdown`, `.build_contacts_flextable`), streaming
(`dta_validate_table_stream`, `dta_group_stream_finalise`,
`dta_validate_any_table`). Port intent, never expected values: an expected
value never comes from the software.

### Phase 3 -- code no test reaches (~95%)

Logic in `dta_filter_columnspec_matches`, `dta_value_to_text`,
`.build_validation_rules_section`, `.report_condition_operator_text`,
`as_json_schema`, rule-class validators, signatory normalisation. Every
user-facing error branch gets an OQ-ROBUST case matched by condition class.
Second PDF backend (`.soffice_docx_to_pdf` / `.pandoc_docx_to_pdf`): selection
tested with mocked bindings; the backend in use recorded by IQ; the other a
documented limitation.

### Phase 4 -- the deliberate remainder (<=5%)

Print/summary methods and interactive-only branches: cheap text assertions or
`# nocov` with a reason beside it.

## 3. Notes

- Estimate: ~60 functions, ~40 new requirements, 100+ OQ cases.
- Every `inst/` change: `Rscript .github/scripts/sync_qualification_manifest.R`.
- Measurement script used for the baseline: covr per-line union of both
  suites, keyed by file and first line.
