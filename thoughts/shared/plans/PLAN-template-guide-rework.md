# Template guide rework: one document per template

Supersedes the renderer and API parts of `PLAN-template-guide.md`. The
collector in that plan (`R/templateGuide-collect.R`) stays, with the changes
listed below. PR #189 is unmerged, so nothing about the old API needs to be
kept compatible.

## Why (user feedback on v1, confirmed by rendering it)

The v1 guide was rendered through Word (COM, which is faithful) and through
TinyTeX, the route that produced the PDF the user saw:

- **Narrow columns.** The question table has 7 columns at 8 pt. Words wrap
  every few characters ("answe / r / fixed"), one cell runs 15 lines, and a
  single row takes a third of a page.
- **Overlapping text, and a 53-page PDF.** pandoc rebuilds each table in LaTeX
  at roughly a quarter of the page width, so columns collide and every word
  sits on its own line. That is the TinyTeX backend, not the docx.
- **No running header or footer.** There is no page X of Y, company or
  document name, or version. The docx never set section furniture, and the
  pandoc route drops furniture regardless.
- **One book for the whole library.** The real use is "I want to see what
  biomarker_gf does", which means one document per usable template.
- Mixed type sizes (8, 9 and 10 pt), tables without headings ("Choice …"
  directly after the parties table), and rows split across pages.

## User-facing API

```r
write_template_guide(
  path,                  # library root, its templates/ folder, or one template file
  dir,                   # output folder; created if missing
  templates = NULL,      # ids; NULL = every non-abstract DTA (creation) template
  format = c("docx", "pdf"),
  organisation = NULL,   # NULL = the library's dta-app.yaml organisation.name, else none
  organisation_unit = NULL,
  overwrite = FALSE,
  quiet = FALSE
)
```

- The function writes **one file per non-abstract creation template**. It
  returns a named character vector (names are template ids, values are file
  paths), invisibly.
- File name: `<id>_v<version>_template_guide.<ext>`. Every character outside
  `[A-Za-z0-9._-]` becomes `_`; when there is no version, `_v<version>` is
  dropped. Two selected templates with the same id are a configuration error,
  because the index keeps only the newest version of an id. If two output
  files would get the same name, abort.
- If `path` is a single file, that template is the selection.
- Errors (`cli_abort` with a class; all of them before anything is written):
  - `dta_template_guide_abstract`: the requested id is a building block. The
    message names the templates based on it, whose guides include its content.
  - `dta_template_guide_unknown` (existing): no template has that id.
  - `dta_template_guide_no_templates` (existing): no usable non-abstract
    template was found.
  - `dta_template_guide_exists`: lists every output that already exists when
    `overwrite = FALSE`. Checked for all outputs before the first file is
    written.
  - `dta_template_guide_format` (existing).
- PDF uses `.convert_docx_to_pdf()`. If the backend that will run is `tinytex`
  or `pandoc`, issue a `cli_warn` (class `dta_template_guide_pdf_layout`):
  that route re-typesets the document, so the running header, footer and
  column widths are lost. The warning names LibreOffice and Microsoft Word as
  the faithful backends.
- The guide is collected once with `templates` as the selection. Each
  document is then rendered from that single collection, which is cheap.

## Microsoft Word as a PDF backend (new; benefits `write_dta()` too)

On this machine and on most corporate Windows desktops, Word is the faithful
renderer. It also updates the TOC and PAGE/NUMPAGES fields while exporting.

- `.find_word()`: Windows only. Returns "" elsewhere. Read the default value of
  `SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\Winword.exe` with
  `utils::readRegistry(..., hive = "HLM")`, then `"HCU"`, each in `tryCatch`.
  Return the path only if `file.exists()` is true.
- `.word_docx_to_pdf(docx_file, pdf_file)`: write a `.ps1` file to
  `tempfile()` and run `system2("powershell", c("-NoProfile",
  "-NonInteractive", "-ExecutionPolicy", "Bypass", "-File", <ps1>, "-In",
  <docx>, "-Out", <pdf>), stdout = TRUE, stderr = TRUE, timeout = 300)`.
  Quote every path with `shQuote(type = "cmd")`, because `system2()` does not
  quote arguments. Use absolute Windows paths (`normalizePath(winslash =
  "\\")`).
  - The script creates `Word.Application` with `Visible = $false` and
    `DisplayAlerts = 0`, then calls `Documents.Open($In, $false, $true,
    $false)`.
  - It updates each `TablesOfContents`, then `Fields.Update()`, then calls
    `ExportAsFixedFormat($Out, 17)` and `Close(0)`.
  - `finally { $w.Quit(); [Runtime.InteropServices.Marshal]::ReleaseComObject($w) }`.
  - Abort if no PDF was produced.
- Backend order: `word`, `libreoffice`, `tinytex`, `pandoc`.
  - Update `.pdf_backends_available()`, the `switch` in
    `.convert_docx_to_pdf()`, `dta_pdf_backend()` (its `engine` is the Word
    path) and that function's roxygen text.
  - Update `.pdf_no_backend_bullets()` with one more bullet: "or Microsoft
    Word on Windows".
- Test: skip unless Windows and `nzchar(.find_word())`. Convert a small docx
  that has a running header (a document from the guide or from `write_dta`),
  then assert `.is_pdf_file()`. Also mock `.find_word` to `""` and assert
  that `word` is not in `.pdf_backends_available()`.

## Running header and footer (shared with write_dta)

In `R/documentBuilders.R`, generalise without changing what `write_dta()`
produces:

- `.running_furniture(header_title, footer_left, footer_right, footer_small,
  org_line = .export_org_line(), header_right_prefix = "", width)` returns
  `list(header, footer)` with exactly today's styling: ink and muted runs, a
  right tab stop at `width`, a hairline under the header and over the footer,
  and Word PAGE / NUMPAGES fields.
- `.page_furniture(meta, width)` becomes a thin call to it, producing
  byte-identical output. The existing write_dta furniture tests must stay
  green and unchanged.
- `.section_props(doc, orient, furniture_meta = NULL, type, furniture = NULL)`
  and `.end_section_orientation(..., furniture = NULL)`: `furniture` is a
  `function(width)` that returns `list(header, footer)`, and it is used when
  `furniture_meta` is NULL.

Guide furniture, on every page after the cover:

- Header, line 1 (tiny, grey): the organisation line, only when it is set.
- Header, line 2: **"Template guide · <template label>"** on the left, "Page X
  of Y" on the right.
- Footer, line 1: **"<template label> (<id>)"** on the left, "Template version
  <v> · <date>" on the right.
- Footer, line 2 (tiny): "Generated <YYYY-MM-DD> with DTAtools <ver> from
  <file name>".

Also set the docx core properties with `officer::set_doc_properties()`:
title = "Template guide: <label>", subject = the template id, creator = the
organisation or "DTAtools".

## Document layout (one creation template)

The page is A4 portrait, from `.new_numbered_docx()` (house margins; headings
auto-number). **Body text is 10 pt and every table is 9 pt; 8 pt is never
used.** No table has more than 5 columns, and only landscape tables have 5.
Every table spans exactly the text width. Body rows never split across pages
(`flextable::set_table_properties(opts_word = list(split = FALSE))`), and
header rows repeat on every page.

**Cover** (its own section, without header or footer, like `.add_cover_page`):

- An eyebrow line "TEMPLATE GUIDE" (9 pt, bold, `primary`).
- The template label (title size, bold, `primary_dark`), then the green rule.
- The first sentence of the description (subtitle size, grey).
- The organisation line, in bold.
- A facts key-value table, 4.6 in wide:
  - Template ID
  - Version
  - Date
  - Status ("Ready to use")
  - Based on (the chain of labels)
  - Creates (dataset names)
  - Questions ("12, of which 3 must be answered")
  - Generated ("2026-11-07 · DTAtools 0.25.0")
- **About this document**, two sentences: what the document is, and that no
  app and no programming knowledge are needed to read it.
- **In this document**: a static list of the numbered top-level section
  titles that are actually present. It is plain text, not a TOC field: officer
  writes the TOC field without a cached result, so an un-updated docx shows an
  empty TOC.

**1 Summary**

- The summary sentence, in bold.
- The description in full.
- An "At a glance" key-value table: Purpose, Based on, Also includes
  (mixins), Questions, Datasets, Parties chosen, Term lists chosen,
  Variants. A row is left out when it has no value.

**2 How it differs from <parent label>** (only with `extends:`)

- A two-column table, What | Change (2.4 in and 3.9 in).
- Then "Everything else is the same as in <parent>."

**3 Questions you will be asked**

- Intro: "When you create a DTA from this template, DTAtools asks these N
  questions, in this order."
- **One card per question.** A card is a two-column flextable:
  - The header row is merged across both columns and reads "Q<n>  <question
    label>" in bold `primary_dark` on a `primary_light` tint. When the
    question must be answered, " · must be answered" follows in `red_error`.
  - Body rows have a 1.7 in label column (grey, bold, `gray_light` tint) and a
    4.6 in value column. The rows are What it is for (Explanation), How you
    answer (Answer type), Choices (one per line), Pre-filled answer, Appears
    in the DTA as, and Fixed (only when fixed).
  - A row whose value is "—" or empty is dropped.
  - A card never splits across pages: `flextable::paginate(ft, init = TRUE,
    hdr_ftr = TRUE)` plus `split = FALSE`. A small spacer separates the
    cards.
- Variants, when present, get the subheading "3.x Variants" and a three-column
  table (Question | Choice | What it changes, 1.6 in, 1.6 in and 3.1 in).

**4 Choices from saved lists** (only when there are parties or term-list
choices)

- 4.1 Supplier and receiver: Party | Appears in the DTA as | Profiles you can
  choose | Must choose (1.2 in, 1.6 in, 2.6 in and 0.9 in).
- 4.2 Term lists: one card per choice, in the same card style as the
  questions. The rows are Explanation, Term list, How it works, Selected by
  default, and Minimum to select.

**5 Filled in automatically**

- A two-column table, Field | Value (2.2 in and 4.1 in).
- When a value carries a "(can be changed by question 'X')" note, the note is
  set as a second paragraph in grey, built with `flextable::compose()` and
  `as_paragraph()`.

**6 Fixed and mandatory fields** (only when there are any)

- A two-column table, Field | Rule (2.5 in and 3.8 in).

**7 Datasets it creates**

- Each dataset gets a heading 2, "<title>". A variant appends "— for choice
  '<label>'".
- Then a facts key-value table: File name or File name pattern, File type,
  Columns, Rules, and Based on dataset template ("<label>, version <v>").
- Then, in a **landscape section** that carries the running furniture:
  - A heading 3, "Columns of <title>", and a 5-column table at 9.7 in:
    - Column (1.9 in): the ID in bold, with the label below it in regular
      weight.
    - Type (1.1 in).
    - Required (0.8 in): "Yes" when it cannot be empty, else "No".
    - Allowed values (2.4 in).
    - Description (3.5 in).
  - A column added or changed by this template (`Change` in the contract)
    gets a third line in its Column cell, "Added by this template" or
    "Changed by this template", in `primary` italics.
  - Then "Removed from the dataset template: A, B." when there are removals.
  - A heading 3, "Rules for <title>", and a table Rule | What it checks (2.6 in
    and 7.1 in). Rules added or changed by this template are tagged in the
    same way.
- The landscape section ends there, and the next dataset or section returns to
  portrait. Both orientations carry the furniture.

**8 Term lists used** (only when the template references any)

- Each term list gets a heading 2 with its label, then its description.
- Then a facts key-value table: Used for columns, Version.
- Then a table Code | Label | Description (1.5 in, 2.0 in and 2.8 in).

**9 Party profiles you can choose** (only when there are any)

- Each profile gets a heading 2, then a facts key-value table: Role,
  Organisation, Address, Country.
- Then a contacts table Name | Role | Email | Phone | Signs (1.4 in, 1.4 in,
  1.9 in, 1.0 in and 0.6 in).

**10 How to read this document**

- The glossary as a key-value table. Glossary entries for features this
  template does not use (Term list, Party profile, Variant, Based on) are
  omitted.

**11 Technical details**

- A key-value table: Template file, Template ID, Version, Date, Based on (ids
  with versions), Dataset templates (id and version), Term lists (id and
  version), Party profiles (id and version), Generated by.
- Then "Problems found", when the collector reported problems for any file
  this template involves: a File | Problem table.
- Readers can ignore this section. Its purpose is traceability.

Because the numbered template auto-numbers the headings, section numbers shift
when optional sections are absent. That is intended.

## Collector changes (`R/templateGuide-collect.R`)

1. **Full column and rule lists, with a change marker; no de-duplication.**
   - In `.tgc_built_datasets`, `columns` holds every column.
   - The new last column `Change` holds "Added", "Changed" or "". It is
     "Added" when the id is not in the dataset template, and "Changed" when it
     is but the row differs.
   - `rules` gets the same treatment: `Rule`, `What it checks`, `Change`.
   - Keep `removed_columns` and add `removed_rules` (rule ids).
   - Drop `same_columns` and `same_rules`.
   - Inline datasets have no dataset template, so `Change` is "" everywhere.
   - Each built dataset gains `source_version`, the dataset template version,
     or "".
2. **The creation entry gains**:
   - `abstract` (logical), `status`, `date`, `file` (basename),
     `option_count`, `required_count`.
   - `lineage`: a data.frame `id, version, label` from the direct parent to
     the root.
   - `mixins`: a data.frame of the same shape.
   - `refs = list(dataset_templates = df(id, version, label), vocabularies =
     df(id, version, label), parties = df(id, version, label))`.
     `vocabularies` and `parties` are the term lists and party profiles this
     template actually offers (its slots, `values_from` of its datasets, and
     anything the dataset templates it uses refer to), not every one in the
     library.
3. **The library entry gains** `organisation` and `organisation_unit`, read
   from the library's `dta-app.yaml` through the engine's
   `dta_app_config_find` and `parse_dta_app_config` (the same parser
   `validateTemplate.R` uses). The value is "" when there is no file, or when
   it is unreadable or does not set them. Never throw over the config file.
4. Everything else (questions, parties, term_choices, prefilled, locked,
   changes, variants, vocab and party entries, `problems`) keeps its current
   contract. `overview` and the dataset-template entries may stay; the
   renderer does not use them.

## Files and ownership (implementation agents, sonnet)

| Agent | Owns | Delivers |
| --- | --- | --- |
| A, collector | `R/templateGuide-collect.R`, `tests/testthat/test-templateGuide-collect.R` | collector changes 1–4, with tests |
| B, renderer | `R/templateGuide.R`, `R/documentBuilders.R` (furniture generalisation only), `tests/testthat/test-templateGuide.R` | new API and layout, with tests |
| C, Word backend | `R/exportDocuments.R`, `tests/testthat/test-exportDocuments.R` | Word backend, with tests |
| D, housekeeping | `inst/qualification/**`, `CHANGELOG.md`, `vignettes/DTAtools.Rmd`, `man/`, `NAMESPACE` | runs after A–C: OQ rewrite, REQ text, roxygenise, style, manifest |

A, B and C run in parallel; B codes against the contract above. The lead (me)
integrates, renders the samples through Word, and gets a separate designer
review of the rendered pages. An Opus code review follows, then the fixes,
then a sonnet agent commits and pushes to PR #189.

## Tests that pin the user's complaints

These are structural, read back from the docx. The renderer tests check:

- One file per non-abstract template. Abstract ids abort with
  `dta_template_guide_abstract`. An `overwrite` collision aborts before any
  file is written.
- The cover title equals the template's `label`, read from the YAML (not from
  the software).
- `word/header*.xml` contains `PAGE` and `NUMPAGES` fields and the
  organisation, which is passed in explicitly. `word/footer*.xml` contains the
  template id and version, read from the YAML.
- `document.xml` contains a landscape `sectPr` (`w:orient="landscape"`).
- **No table has more than 5 grid columns, no portrait table more than 4, and
  every `gridCol` is at least 0.6 in (864 twips).** Each table's grid sum is at
  most the text width of its section, plus a tolerance.
- Every option label and every column id of the built dataset appears in the
  text.
- A changed column carries "Changed by this template". This uses the bundled
  `biomarker_gf_acme`: GFNAM is changed and ACMEID is added.

## Qualification

- REQ-TMPL-040 is reworded to "one readable document per usable template,
  with page furniture".
- OQ-TMPL-086…089 are rewritten for per-template output: file names, cover
  title, header and footer fields, and the column ids from the YAML. Expected
  values come from the YAML files only.
- `min_tests` stays at 89 unless the count changes. Sync the manifest.

## Docs

- CHANGELOG `[Unreleased]`:
  - Added: per-template guides, and Word as a PDF backend.
  - The ≠ / xelatex fix stays as it is.
- Vignette: update the section's example to `write_template_guide(lib,
  tempdir(), templates = "biomarker_gf")`.

---

## Revisions after the design and engineering reviews (BINDING — these override the sections above)

### Selection and versions
- `templates = "x"` documents the NEWEST version of x (`.tgc_rank`). Default selection = newest version of
  every non-abstract creation template id. One file per selected id. Only the requested ids are written,
  never parents/mixins pulled in by the closure. Duplicate-name abort only if two outputs map to the same
  file name after sanitising.
- A selected template whose entry is a stub (unreadable/unbuildable) still gets a document: cover + an
  Overview carrying the build note + "Problems found" in Technical details, and a `cli_warn`
  (class `dta_template_guide_problems`) naming it. Stubs carry every new field (empty defaults).

### Structure (replaces "Document layout" section list)
Cover (identity only) → 1 Overview → 2 How it differs from <parent> (only with extends) →
3 What you will be asked (3.1 Questions, 3.2 Variants?, 3.3 Supplier and receiver?, 3.4 Term lists?) →
4 What is filled in for you (4.1 Filled in automatically, 4.2 Fixed and mandatory fields?) →
5 Datasets it creates (ONE landscape section for the whole chapter) → 6 Term lists used? →
7 Party profiles you can choose? → 8 Words used in this document → 9 Technical details.
(`?` = only when non-empty; numbering shifts, intended.)

- **Cover**: eyebrow "TEMPLATE GUIDE" (9 pt bold primary), label (26 bold primary_dark), green rule,
  first sentence of description (13 grey), org line (bold) when set; facts kv 4.6 in / label 1.3 in:
  Template ID, Version, Date, Status, Generated ("2026-11-07 · DTAtools 0.25.0"). Then "About this
  document" (3 sentences, third points to section "Words used in this document" by its number), then
  "In this document": "<n>  <title>" one per line, 10 pt, no page numbers/leader dots.
- **1 Overview**: bold summary sentence, full description, "At a glance" kv: Based on, Also includes,
  Questions ("12, of which 3 must be answered"), Datasets, Parties chosen, Term lists chosen, Variants
  (drop empty rows).
- **Question card**: 6.3 in, label col 1.6 / value 4.7. Header row = two NON-merged cells on
  primary_light with the primary_dark 1 pt rule below: cell 1 "Question 3" (9 pt bold primary_dark),
  cell 2 the label (10 pt bold primary_dark) + when required a run "  ·  Must be answered" (9 pt bold
  red_error) — the WORDS carry the meaning. Section intro: "Questions marked Must be answered cannot be
  left empty; all others are optional." Body rows in order: What it is for, How you answer, Choices (one
  per line; > 12 → first 10 + "… and N more"), Pre-filled answer, Appears in the DTA as, Fixed (only when
  fixed: "Yes – cannot be changed when creating the DTA"). Drop empty / "—" rows. Labels 9 bold gray_mid
  on gray_light, values 9 gray_dark. Card boxed with 0.5 pt gray_border outer border + hairlines (only
  boxed table). Gap between cards = a 6 pt empty fpar (font 4 pt, padding.bottom 2), never `.add_spacer()`.
  Keep-together (`paginate(init=TRUE, hdr_ftr=TRUE)`) only when the card has <= 25 rendered lines.
  Term-list choice cards use the same component (header "Term list choice n" | choice label).
- **Contacts table**: 4 columns: Name 1.6 | Role 1.6 | Contact 2.4 (email, phone on 2nd line) |
  Signs the DTA 0.7.
- **Datasets chapter**: the chapter heading, every dataset's H2, its facts kv, its tables — all inside
  ONE landscape section (9.7 in). H3s: "Columns (34)", "Rules (7)". A key sentence above the column
  table when any row has a Change: "Columns marked Added/Changed by this template differ from the dataset
  template <label>, version <v>." Allowed values: pattern → "Pattern: " + pattern run in Consolas 9;
  lists one per line; "Any" stays. Column/rule tables use 3 pt top/bottom padding. Header row repeats;
  rows never split; tables may break between rows. Return to portrait once, after the chapter.
- **Glossary** renamed "Words used in this document"; omit entries for features the template doesn't use.

### Typography (binding)
1. Calibri only; Consolas 9 only for patterns/regexes/file names. 2. Sizes: title 26, subtitle 13, H1 16,
H2 13, H3 12, body 10, tables 9, furniture 8/7 (the ONLY place < 9 pt). 3. Body paragraphs single
spaced, 4 pt after. 4. Never add spacers before headings; every heading and every lead-in sentence before
a table is keep-with-next. 5. No forced page break per H1; page breaks only from the cover section and the
landscape section. 6. Gaps are 6 pt fpars, never empty Normal paragraphs. 7. Tables <= 15 rows stay
whole (paginate); longer tables break between rows only and repeat their header. 8. Every table spans
the full text width (exception: cover facts 4.6 in). 9. An empty value is a dropped row, never "—",
except in Technical details. 10. Bold only for labels, IDs, headings; caps only for the cover eyebrow.

### Furniture (replaces earlier furniture text)
- Header line 1 (7 pt gray_mid): org line, only when set (organisation "" or NULL with no library value
  = none; `.running_furniture` treats "" as none).
- Header line 2: left "Template guide: <label>" (8 pt bold primary_dark; label truncated to 60 chars with
  "…"), right "Page X of Y" (8 pt gray_mid) — PAGE/NUMPAGES fields.
- Footer line 1: left "<id>  ·  Template version <v> of <date>" (" of <date>" omitted when no date),
  right "Generated <YYYY-MM-DD>  ·  DTAtools <ver>". No second footer line. Nothing invented
  (no confidentiality, no document status, no document number).
- `.running_furniture` must reproduce write_dta's furniture exactly through `.page_furniture`: FIRST
  capture `word/header*.xml` + `word/footer*.xml` of a fixed write_dta docx (strip nothing; the XML parts
  are deterministic) as a golden comparison in a test, refactor, keep the test.
- `.section_props`: early return only when BOTH `furniture_meta` and `furniture` are NULL. Guide builds
  its own cover and sections via `.end_section_orientation(..., furniture = fn)` and the final
  `officer::body_set_default_section()`.

### Collector (additions to "Collector changes")
- Change markers must not be false positives: build the dataset-template comparison side with the SAME
  vocabulary dry selections the creation dry-run uses, so a term-list narrowing is not "Changed".
- Offered refs: `.tgc_refs` over the resolved def (incl. variants), follow dataset templates' raw defs for
  `values_from`, filter by kind, resolve each to (id, version, label); parties from
  `party_profiles_for_slot` per slot.
- Organisation: `dta_app_config_find(src$dir)` → `parse_dta_app_config(f)$config$organisation`; never throw.
- Variant label string stays the collector's "For choice 'X'".

### Word backend (additions)
- Registry: try `view = "64-bit"` and `"32-bit"` (check `args(utils::readRegistry)`), HLM then HCU.
- Export to a `tempfile(fileext = ".pdf")` with an ASCII path, then `file.copy` to `pdf_file`.
- Script: only `Quit()` when no other documents are open after closing ours (`$w.Documents.Count -eq 0`),
  so an attached user session is never closed; script ASCII-only; `system2(timeout = 300)`, non-zero
  status / no output → abort with `.cli_escape`d output.
- Order word, libreoffice, tinytex, pandoc. `switch` arm `word =`. Update test-exportDocuments.R:641/644
  backend sets, roxygen of `dta_pdf_backend`, `.pdf_no_backend_bullets`, R/DTAtools-package.R:40.
  (Vignette + qualification OQ-EXPORT/qualification-report.R:555 backend sets = agent D.)
