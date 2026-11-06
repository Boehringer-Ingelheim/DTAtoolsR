# PLAN: `write_template_guide()` — a readable handbook for a template library

Date: 2026-11-06. Branch: `ai/template-docs-generator-79e2dd` (off `dev` @ 19dfb51). PR → `dev`.
Status: v2, revised after three parallel reviews (engine feasibility with a
working dry-run spike; non-programmer readability; repo/qualification rules).
Auto-approved.

## Goal

One exported function that turns a template library directory into a single
Word (or PDF) handbook that a non-programmer (study manager, data manager,
supplier contact) can read to understand **what each template does, what it
will ask, what it fills in, and which datasets/columns it produces** — without
starting the Shiny app and without reading YAML.

Non-goals (YAGNI, state in PR): Markdown/HTML output, an app download button,
one file per template, landscape sections, version-to-version history.

## User-facing API

```r
write_template_guide(path, file, format = NULL, templates = NULL,
                     overwrite = FALSE, quiet = FALSE)
```

- `path` — library root, its `templates/` dir, or one template file. A dir goes
  through engine `dta_template_library_dir()`; a file means "its directory,
  restricted to that file's id".
- `format` inferred from `file`'s extension like `write_dta()` (`""` → docx);
  only `"docx"`/`"pdf"`; else `cli_abort("'format' must be 'docx' or 'pdf'.",
  class = "dta_template_guide_format")`.
- `templates` — optional ids; restricts chapters to those ids **plus everything
  they reference** (parent chain, mixins, dataset templates, vocabularies,
  party profiles). Unknown id → abort, class `dta_template_guide_unknown`.
- Existing `file` without `overwrite = TRUE` → abort with `write_dta()`'s
  wording ("File '…' already exists. Set overwrite = TRUE to replace."),
  class `dta_template_guide_exists`, file untouched.
- No template files at `path` / path missing → abort, class
  `dta_template_guide_no_templates`.
- PDF: build docx to a tempfile, then `.convert_docx_to_pdf()` (it stages the
  PDF itself). DOCX saved via `.dta_write_staged()`.
- Returns `invisible(file)`; `cli::cli_alert_success()` unless `quiet`.
- Roxygen with `@export`; `@examples` **docx only**, bundled library
  (`system.file("extdata", "templates", package = "DTAtools")`) to `tempfile()`.

## Architecture: collect → render (two files, one contract)

### `R/templateGuide-collect.R` — `.template_guide_collect(path, templates = NULL)`

Pure data, no officer. Local index exactly like `validate_template()`, never
the env-var driven `build_template_index()`, never `Sys.setenv()`.
`g <- .dta_template_engine_get`. Working spike (verified):

```r
files    <- .dta_template_list_dir(dir, g("dta_template_all_kinds")())
records  <- setNames(lapply(files, .dta_template_read_raw), files)
index_df <- .dta_template_build_index_df(records)
index_df$label <- <records' def$label, falling back to id>   # needed for labels in lineage
res <- g("resolve_template_inheritance")(g("read_dta_creation_template")(path)$value,
                                         .dta_template_extends_resolver(index_df))
def <- g("dta_template_finalize_def")(res$def)
b   <- g("create_dta_from_template")(def, path, selections, index = index_df,
         vocab_selections = .dta_template_vocab_dry_selections(def, index_df))
```

Traps (from review — obey all):
- Read YAML lists with `[[`, never `$` (`$effects` partially matches
  `effects_all`, `$dataset` matches `datasets`).
- Package code gets base `%||%` (NULL only); the engine's treats zero-length as
  absent. Test emptiness with `length(x) == 0` explicitly.
- `.dta_template_read_raw()` values: wrap scalars in `as.character()`;
  `nullable: Yes/No` stays logical.
- Abstract: `isTRUE(def[["abstract"]])` → resolve + finalize only, no build.
- Resolved vocabularies: `.dta_template_vocab_resolved(index_df)` (or
  `g("vocabulary_resolver")(index_df)`), NOT `.dta_template_vocab_resolver`
  (raw parent, no inherited terms).
- Sort with `method = "radix"`.

Returns a **plain list**; every table a `data.frame(check.names = FALSE,
stringsAsFactors = FALSE)` of character columns with the exact headers below;
zero-row frames allowed, never `NULL`:

```
list(
  library  = list(name, path, generated = "YYYY-MM-DD", package_version),
  overview = df(Name, Kind, ID, Version, `What it is`),
  creation     = list(<creation entry>),   # sorted label, then version desc
  datasets     = list(<dataset-template entry>),
  vocabularies = list(<vocab entry>),
  parties      = list(<party entry>),
  problems     = df(File, Problem)
)
```
Kinds in plain words: "DTA template", "Dataset template", "Term list",
"Party profile". `What it is` = first sentence of description, else label.

#### Creation entry
```
list(
  title, id, version, description,
  summary   = chr(1)  # "Produces 1 dataset (Genomic Findings data table…, 33
                      # columns, 7 rules). Asks 12 questions. You choose a
                      # Supplier and a Receiver, and 1 term list."
                      # Abstract: "Building block: cannot be used on its own;
                      # other templates are based on it."
  at_a_glance = df(key, value):  Status ("Ready to use" | "Building block —
                 cannot be used on its own, only as a basis for other
                 templates"), Based on (labels only, via engine
                 template_lineage_text() with the label column, or "—"),
                 Also includes (mixin labels; row omitted if none), Version, Date
  parent    = chr(1)  # parent label, "" if none
  changes   = df(`What`, `Change`)   # vs direct parent; zero rows if none
  questions = df(Question, `Answer type`, `Choices`, `Pre-filled answer`,
                 `Appears in the DTA as`, `Explanation`, `Must answer / locked`),
  variants  = df(Question, Choice, `What it changes`)
  parties   = df(Party, `Appears in the DTA as`, `Available profiles`, `Must choose`),
  term_choices = df(Choice, Explanation, `Term list`, `How it works`,
                    `Selected by default`, `Minimum to select`),
  prefilled = df(Field, Value),       # flattened resolved base.metadata
  locked    = df(Field, Rule),
  datasets  = list(<built dataset>),
  build_note = chr(1),                # "" normally (see below)
  technical = chr(1)  # "ID biomarker_gf · version 1.0 · file biomarker_gf.dta-template.yaml"
)
```

**changes**: use the engine — do not diff raw YAML:
```r
a <- g("template_history_build")(<parent id@ver = res$lineage[[1]]>, index_df)$value
b <- g("template_history_build")(<this id@ver>, index_df)$value
g("template_history_compare")(a, b)   # df: area, what, change, from, to
```
Map each row to plain text (`What` = option label from the resolved def, or the
humanised field / "Column ACMEID in <dataset title>"; `Change` = e.g. "Pre-filled
answer: Test Company 1 → ACME Labs", "Added", "Removed", "Description changed",
"Allowed values: ExternalSupplierName → ACME Labs"). Drop the "Based on added"
row (already in at_a_glance). Read `template_history_compare()` /
`template_history_label()` in `inst/shiny/dta_app/R/template_history.R` for the
`area`/`change` vocabulary. Any error → zero rows + a `problems` row.

**Answer type** — copy the app's rules (`template_ui.R:642-760`):
`typ <- tolower(opt[["type"]] %||% "text")`,
`custom <- g("dta_template_allow_custom")(opt, default = !identical(typ, "select"))`,
`ch <- g("dta_template_choices")(opt)`:
- `boolean` → "Yes / No"; `number` → "Number" (choices ignored).
- `textarea` → "Longer free text" (+ ", with suggestions" when choices).
- `select` (or text with choices and `!custom`) → "Pick one from the list".
- `text` → "Free text", + ", with suggestions" when choices and custom.
- `Choices`: labels (for `{value,label}`) joined `"\n"`; "—" when none.
- `Pre-filled answer`: `g("dta_template_default")(opt, def[["base"]][["metadata"]])`;
  a `text` option with exactly one choice and no default is pre-filled with it;
  logical/"yes"/"no" → "Yes"/"No"; empty → "—".
  `${today}` → "(the date the DTA is created)", `${version}` → "(the version you enter)".

**Appears in the DTA as** (one humaniser, used everywhere):
`g("dta_template_option_path")(opt)`; for effects options read `opt[["effects"]]`
and `opt[["effects_all"]]`, list each distinct path, joined `"\n"`.
Path → text: strip `^base\.`, then
- `metadata.a.b` → `tmpl_humanise` each segment joined " › " ("Transmission › Frequency");
- `datasets.<ds>.columns.<col>.values` / `dataset.columns.<col>.values` →
  "Allowed values of column <col>" (+ " in <dataset title>" when known);
- anything else → humanised segments joined " › ".

**Must answer / locked**: union `def[["required"]]`, `def[["sealed"]]`; strip
`^base\.`; an option matches when `options.<id>` or its target path overlaps
(`g("dta_template_paths_overlap")`, parent covers children). "Must answer",
"Locked", both ", "-joined, else "—". `locked` table rows: every
required/sealed path humanised, Rule = "Locked: templates based on this one
cannot change it" / "Must be filled in when the DTA is created".

**Parties**: `g("normalise_party_slots")(def[["party_slots"]])`, profiles via
`g("party_profiles_for_slot")(g("template_party_profiles")(index_df), slot)` →
labels joined "\n"; `Must choose` "Yes"/"No" from `slot$required`.
**Term choices**: `g("normalise_vocabulary_slots")(def[["vocabulary_slots"]])`;
`Term list` = vocabulary label; `How it works`: closed → "Pick from the list
only", open → "Pick from the list or add your own"; default joined "\n".

**prefilled**: flatten `def$base$metadata`: humanised field names;
version_history rows as "Version history 1" = "1.0 · (the date…) · Initial…";
contacts as "Name (Role, email)" joined "\n"; length-0 → "(left empty)";
logicals "Yes"/"No".

**Variants**: if `def[["variants"]]`, one `variants` row per choice, and build
once PER CHOICE with
`selections = setNames(list(choice[["value"]]), spec[["id"]])` passed to both
`resolve_template_variant()` and `create_dta_from_template()`; datasets get a
`variant` label ("For choice '<label>'"). Otherwise one build with `list()`.

**Built dataset** (creation templates; from the dry-run DTA):
Accessors (verified): `dta@datasets` named list; `ds@id`, `ds@name`;
`ds@files[[i]]@filename`, `@pattern` (logical), file type from class
(`DTAtools::DTAFileTSV` → "TSV"); `ds@specs@columns[[i]]` with `@id @label
@nullable @description @values @pattern @structure@backend @structure@type
@structure@format`; rules `ds@specs@rules[[i]]`, text via existing
`translate_rule_to_human(rule)` (`R/formattingHelpers.R:542`).
```
list(name, title, variant = "",
     about   = df(key, value): File name (or "File name pattern"), File type,
                               Columns (n), Rules (n),
     source  = chr(1)   # dataset-template title it came from, "" if inline
     columns = df(Column, Type, `Can be empty`, `Allowed values`, Description),
     same_columns = chr(1)  # "All other 31 columns are exactly as in the dataset
                            # template 'GF domain smrnaseq' (chapter Dataset
                            # templates)." or ""
     removed_columns = chr vector of removed ids
     rules   = df(Rule, `What it checks`),
     same_rules = chr(1))
```
Column cells: `Column` = "STUDYID\nStudy Identifier"; `Type` =
"SAS Char\n$9." (backend + type, format on 2nd line); `Can be empty` Yes/No;
`Allowed values` = values joined ", " — **never truncated** (house rule,
`R/documentBuilders.R:1030`); if none but `@pattern` → "Must match pattern
<p>"; else "Any". `Description` full text.
**De-duplication (readability must)**: when the dataset came from a dataset
template (`template:` entry), render that dataset template's own default build
(below) with the same row function, and keep in `columns` only rows that are
added or differ; removed ids → `removed_columns`; the rest → `same_columns`
sentence. Same for rules. Inline/path datasets: full tables, `same_*` = "".

**build_note**: "" normally; abstract → ""(summary says it); dry-run failure →
"A sample DTA could not be built from this template: <msg>" + `problems` row.

#### Dataset-template entry
Built with `g("build_dataset_from_template")(def, selections = <option
defaults>, resolve_vocab = <resolved vocab lookup>)` (read its signature in
`dataset_template.R:418`; if it cannot be built, fall back to raw YAML columns
and note it). Columns rendered with the same row function; for columns whose
raw YAML has `values_from`, append "\n(from term list '<label>')" to Allowed values.
```
list(title, id, version, description, summary,
     at_a_glance = df(key, value): Dataset name, File name / pattern, File type,
                   Used by (creation-template labels), Version, Date,
     settings = df(Setting, `Answer type`, `Pre-filled answer`,
                   `Appears in the DTA as`, Explanation)   # heading: "Settings a
                   # DTA template can fill in" — the end user is never asked
     columns, rules, technical)
```

#### Vocabulary entry
`title, id, version, description, summary, at_a_glance` (Based on, Used for
columns, Used by, Version), `terms = df(Code, Label, Description)` from the
RESOLVED vocabulary, `technical`.

#### Party entry
`title, id, version, description, at_a_glance` (Role: supplier→"Supplier",
receiver→"Receiver", any→"Either"; Organisation; Address; Country), `contacts =
df(Name, Role, Email, Phone, Signs)` (Signs "Yes" if `signature: true`, else
"—"; missing fields "—"), `technical`.

Per-file failures never abort the guide: a `problems` row, and the chapter
(when a header was readable) shows a `build_note`.

### `R/templateGuide.R` — exported `write_template_guide()` + `.template_guide_docx(guide)`

Built only from the contract above, reusing `.new_numbered_docx()`,
`.add_heading()`, `.add_body_par()`, `.add_spacer()`, `.kv_table()`,
`.style_table()`, `.fit_table_width()`, `flextable::body_add_flextable()`,
`officer::body_add_toc()`, `officer::body_add_break()`. No new dependency
(namespaced calls only). `"\n"` in a flextable cell already renders as
`<w:br/>` (verified, flextable 0.10.0) — nothing special needed.

Layout (portrait throughout):
1. Cover: "Template guide", library name, "Generated <date> with DTAtools
   <version>", the plain intro paragraph ("This guide describes every template
   in the library: what each one asks, what it fills in for you, and which
   datasets it creates. You do not need the DTAtools app or any programming
   knowledge to read it."), a "Reading tip" line (most readers need the DTA
   templates chapter; the other chapters are reference). Page break.
2. Table of contents (`body_add_toc(level = 2)`) + note "Right-click › Update
   field in Word if page numbers are missing". Page break.
3. H1 "How to read this guide": glossary kv-table with short examples —
   DTA template, Building block, Based on, Question, Pre-filled answer,
   Appears in the DTA as, Must answer, Locked, Dataset, Column, Can be empty
   (blank allowed), Allowed values, Term list, Party profile.
4. H1 "What is in this library": count sentence + `overview` table.
5. H1 "DTA templates"; per entry H2 "<title>":
   **summary** paragraph (bold) first; description; at-a-glance kv-table;
   if `changes` rows: H3 "What it changes compared to <parent>" table +
   sentence "Everything else is the same as in <parent>.";
   H3 "Questions you will be asked" (or "This template asks no questions.");
   H3 "Variants" only when rows;
   H3 "Choices made when creating the DTA": parties table, term-choices table
   (each omitted with a sentence when empty);
   H3 "Filled in automatically";
   H3 "Locked and mandatory fields" only when rows;
   H3 "Datasets it creates" → per dataset H4 title (+ variant): about kv,
   columns table (or sentence), `same_columns`, removed columns sentence,
   rules table / `same_rules`;
   `build_note` italic; `technical` as small grey 8pt line at the end.
6. H1 "Dataset templates", "Term lists", "Party profiles" — same pattern.
7. H1 "Problems found" only when `problems` has rows.
Empty table → one italic sentence, never an empty grid. Wide tables (questions,
columns, terms) body 8pt; set explicit widths (columns table: Column 1.2in,
Type 0.9, Can be empty 0.6, Allowed values 1.5, Description rest of
`TEXT_WIDTH$portrait`); header row repeats on each page (check flextable emits
`w:tblHeader`; set it if not).

## Tests (testthat)

`tests/testthat/test-templateGuide-collect.R` (bundled library +
`create_template_repo()` copy in `withr::local_tempdir()`):
- biomarker_gf: `Question` set == option labels read straight from the YAML;
  "Transfer frequency" → "Free text, with suggestions", appears as
  "Transmission › Frequency"; "Test upload required?" → "Yes / No", pre-filled
  "Yes"; columns table has 0 rows and `same_columns` names 33 columns (dedup) —
  or rows only for genuinely differing columns; summary mentions 12 questions.
- ACME: "Header / organization" pre-filled "ACME Labs"; `changes` mentions
  ACMEID and GFNAM; built `columns` contains ACMEID and GFNAM rows only (+ any
  real differences), GFNAM allowed values "ACME Labs".
- dataset-template entry: GFTSTDTL allowed values contain "FRAGMENT COUNT" and
  "(from term list"; SUBJIDN-like pattern column says "Must match pattern".
- vocabulary terms; fixture vocabulary that `extends:` another shows inherited
  terms; party contacts "Jane Doe" Signs "Yes"; Role "Supplier".
- abstract fixture (parent abstract + child) → building-block status, no
  datasets, no problem row; a broken YAML file → `problems` row, no abort;
  `templates = "biomarker_gf_acme"` keeps parent, gf_smrnaseq, gf_tstdtl and
  both parties; variants fixture builds once per choice.
- aborts matched by class (`dta_template_guide_no_templates`,
  `dta_template_guide_unknown`).
`tests/testthat/test-templateGuide.R` (renderer + API): hand-made `guide`
fixture from the contract for renderer unit tests; end-to-end docx of bundled
library read back with `officer::docx_summary()`: headings include every
creation-template title and fixed section names; a cell holds "ACME Labs"; a
multi-choice cell contains "\n"; empty table → sentence, not a table; overwrite
guard (class + file unchanged); bad format (class); `.pdf` extension infers
pdf: `skip_if(is.null(dta_pdf_backend()))`, file starts with `%PDF` (the
existing CI guard in `test-exportDocuments.R:650` already ensures a backend in
CI). Never assert on translated base-R text.

## Qualification suite

1. `requirements/_api.yaml`: `  - write_template_guide` between
   `write_table_to_file` and `write_validation_report`.
2. `requirements/REQ-TMPL.yaml`: **REQ-TMPL-040** (`risk: medium`,
   `category: functional`, `covers: [write_template_guide]`, `source:
   R/templateGuide.R; R/templateGuide-collect.R`). Text (falsifiable):
   "write_template_guide() shall write a DOCX document for a template library
   in which, for every creation template, each option's label and its
   pre-filled answer appear as text, and each column a dataset template adds
   appears; an existing output file shall not be replaced unless overwrite =
   TRUE, and a format other than docx or pdf shall be refused."
3. `inst/qualification/tests/test-OQ-TMPL-guide.R`: OQ-TMPL-086 (every option
   label of bundled biomarker_gf, read with `qa_read_yaml()`, appears in
   `qa_docx_text()`), OQ-TMPL-087 (ACME's `default: ACME Labs` and added column
   id `ACMEID`, both read from the YAML, appear), OQ-TMPL-088 (existing file
   not replaced without overwrite; hash unchanged), OQ-TMPL-089 (format "html"
   refused). Titles `"OQ-TMPL-0NN | … | REQ-TMPL-040"`; `qa_tempdir()`;
   `qa_step()`/`qa_check()` evidence; no PDF; no collector calls for
   expectations.
4. `_index.yaml`: TMPL `min_tests` 85 → 89.
5. `Rscript .github/scripts/sync_qualification_manifest.R` (then `--check`).

## Docs / housekeeping

- `CHANGELOG.md` `## [Unreleased]` → `### Added`. No version bump.
- Vignette: one short paragraph + non-evaluated-safe chunk writing to
  `tempfile()` in the templates section (chunk must run).
- `roxygen2::roxygenise()` (version per `Config/roxygen2/version`; then check
  `git status man/ NAMESPACE DESCRIPTION` — roxygen manages `Collate:`),
  `Rscript .github/scripts/style.R`, `check_deps_in_desc.R`, `pre-commit`.

## Execution

1. ✅ Plan v1 → three reviews → this v2 (auto-approved).
2. Parallel implementers (sonnet), this worktree, strict file ownership, each
   runs ONLY its own test file via `devtools::test(filter = "...")` with
   `--no-init-file`, never the full suite, never roxygenise/style:
   - A: `R/templateGuide-collect.R`, `tests/testthat/test-templateGuide-collect.R`
     (+ fixtures written to tempdirs inside tests).
   - B: `R/templateGuide.R`, `tests/testthat/test-templateGuide.R` (renderer
     against a hand-made guide; the e2e test is written now, run after A).
3. C (sonnet), after A+B: qualification files, CHANGELOG, vignette, roxygen,
   style, manifest sync.
4. Lead: render the bundled library to PDF and inspect pages; r-verify (full
   suite + `devtools::test(filter='qualification')` + the TMPL OQ area);
   r-review + an adversarial full-strength review of the diff; fix; commit; PR.
