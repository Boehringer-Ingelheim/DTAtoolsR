# PLAN — Word template export: real tables, not re-parsed bullet text

Branch `ai/word-template-export-plan-f95682`, off `dev` at `1773376`
(`Version: 0.26.0`).

Scope: make `{PLACEHOLDER}` templates able to emit column specification tables,
validation rules and the rest of the `DTA` object as real Word content, instead
of the flat single-paragraph text they emit today.

---

## 1. What the template path can emit today

Every placeholder resolves to a **character scalar** that is written into a Word
text run. That is the whole contract:

- `.extract_template_variables()` (`R/exportTemplateDocx.R:249`) returns 34
  entries, all `character(1)`.
- `.replace_placeholders_in_xml()` (`:1019`) substitutes inside `<w:t>` nodes.
- `.tv_set_run_text()` (`:1277`) turns `\n` into `<w:br/>` and `\t` into
  `<w:tab/>` — **inside one run of one paragraph**.

So the richest thing a template can produce is one paragraph with manual line
breaks and leading spaces. Consequences, all verified by reading:

**Column specs and rules have no table representation at all.** The catalogue
(`.tv_placeholder_catalog()`, `:145`) offers `{TOTAL_COLUMNS}` and
`{TOTAL_RULES}` — two integers. There is no `{COLUMN_SPECS}`, no
`{VALIDATION_RULES}`, no per-dataset token.

**The only way to get dataset content in is a markdown round-trip, and it is
lossy by construction.** The Shiny app builds a markdown string in
`format_datasets_detail()` (`inst/shiny/dta_app/R/utils_export.R:201-296`) —
`- **USUBJID** [character, not null, length 20]: Subject ID | values: a, b` —
passes it as `{DATASETS_DETAIL}`, and `exportTemplateDocx.R` then *re-parses its
own output with regexes*:

| Function | Line | What it does |
| --- | --- | --- |
| `.tv_looks_markdown()` | `:598` | heuristically decides "is this markdown?" |
| `.tv_reformat_dataset_bullet()` | `:648` | splits `**name** [meta]: desc \| values:` back apart |
| `.tv_parse_column_meta()` | `:722` | re-parses `[character, not null, length 20]` |
| `.tv_is_group_condition_summary()` | `:764` | regex-detects a rule summary line |
| `.tv_expand_group_condition_summary()` | `:770` | rebuilds a prose outline from it |
| `.tv_find_group_rule()` | `:847` | **searches the DTA for the rule object by matching the id back out of the text** |
| `.tv_condition_to_text()` / `.tv_constraint_to_text()` | `:871` / `:901` | re-render the rule from the object |

That last pair is the tell: the code goes object → markdown → text → *find the
object again* → text. And `translate_rule_to_human()`
(`R/formattingHelpers.R:431`) already renders every rule class properly,
including `DTARuleGroupCondition` with its conditions and constraints, and is
what the built-in DOCX layout uses. The template path duplicated it, worse,
through a string.

**Everything else in the object is missing or flattened.** Not reachable from a
template in any form: per-dataset file handlers, dataset descriptions,
`template_source`/`template_version`/`template_date`, contacts as a table,
signatories as a table, version history as a table (`{VERSION_HISTORY}` is one
semicolon-joined line, `:505`), authorized-for-corrections as a list.

**Two defects fall straight out of this, both verified:**

1. `inst/extdata/templates/clinical_dta_template.docx` contains
   `{DATASETS_DETAIL}` and `{YAML_EMBEDDED}`. Neither is in the catalogue, so
   neither is supplied by `.extract_template_variables()`. `write_dta(dta,
   template = clinical_dta_template.docx)` from plain R therefore emits a
   document with those two tokens **printed literally** — the dataset section
   is empty. It only works from inside the Shiny app, which injects them at
   `app.R:6645-6648`.
2. `list_available_templates()` (`utils_export.R:127`) lists *every* `.docx`
   under `inst/extdata/templates`, which includes `dta_numbered_template.docx`
   — the reference document that supplies numbered heading styles to the
   built-in layout (`.new_numbered_docx()`, `documentBuilders.R:76`). It
   contains **zero placeholders**. A user who picks it in the export dialog
   gets a document with none of their DTA in it and no warning.

## 2. Root cause

One pipeline, not two. Every placeholder is an *inline* value; there is no
notion of a *block* placeholder that expands to document structure. The
markdown re-parsing exists purely to fake structure inside that single string.

Meanwhile the built-in layout already has the block renderers:

| Builder | Line | Returns |
| --- | --- | --- |
| `.build_column_specs_table()` | `documentBuilders.R:551` | flextable, 6 cols, house style |
| `.build_rules_table()` | `documentBuilders.R:428` | flextable, `translate_rule_to_human()` |
| `.build_contacts_flextable()` | `documentBuilders.R:142` | flextable, 8 cols |
| `.add_signature_section()` | `documentBuilders.R:275` | doc + signature table |
| `.add_file_specifications()` | `documentBuilders.R:313` | doc + file table |
| `.add_embedded_yaml_section()` | `documentBuilders.R:91` | doc + monospace YAML |

The fix is to let a template reach these, not to write new ones.

## 3. Design: inline placeholders and block placeholders

Two kinds of token, one grammar.

**Inline** — unchanged. Resolves to a string, substituted into a run, works in
body, headers and footers, may sit mid-sentence.

**Block** — new. A paragraph whose *entire trimmed text* is exactly one block
token is **replaced** by rendered Word content. Body only.

The "whole paragraph or nothing" rule is what keeps this predictable:
`Columns: {COLUMN_SPECS}` cannot be replaced by a table without destroying the
sentence, so it is left alone and reported as unresolved, exactly as an unknown
token is today.

### Precedence keeps every existing template working

`variables` still wins. `export_with_template(dta, tmpl, out, variables =
list(DATASETS_DETAIL = "..."))` renders the caller's string as text; only when a
block token has *no* user-supplied value does the block renderer run. Existing
callers — including the Shiny app before it is migrated in phase 3 — are
unaffected.

### Rendering mechanism

Two passes over the output:

1. **Pass 1 (existing, unchanged).** Unzip → substitute inline tokens in
   `word/document.xml` plus every `header*.xml` / `footer*.xml` → rezip. Block
   tokens are left untouched and not reported as unresolved.
2. **Pass 2 (new), only if the document still contains a block token.** Open
   the pass-1 output with `officer::read_docx()`, and for each block token:
   `officer::cursor_reach()` → `body_add_par(pos = "on")` to consume the
   placeholder paragraph → call the existing builders, which append after the
   cursor → `print()`.

Verified by probe: `cursor_reach()` + `body_add_flextable(pos = "on")` produces
a valid `.docx` with the table spliced **in place** (`doc_index` 1 = preceding
paragraph, 2-7 = table cells, 8 = following paragraph), and an officer
read/write round-trip of a template containing a table is lossless at the
`docx_summary()` level.

`cursor_reach(x, keyword)` turns out to be `grepl(keyword, xml_text(nodes))`
over the **top-level children of `/w:document/w:body`**.

**Matching on the token text is wrong, and adversarial review proved it twice.**
Both failures are the same mistake: pass 2 was asking "what does the finished
document say?" when the question is "what did the template ask for?" Those are
different sets, because pass 1 writes substituted values into the same file.

- A DTA whose title is the literal string `{COLUMN_SPECS}` had its **title
  paragraph replaced by two column-specification tables**, silently. Reachable
  through any of the ~25 inline placeholders and through caller-supplied
  `variables` — a Shiny free-text field, or someone pasting the example text
  out of this very README.
- "A paragraph nested inside a `w:tbl` is unreachable" is true of a nested
  `w:p`, but `xml_text()` of a `w:tbl` concatenates *every* cell — so a
  **one-cell table** whose only content is the token matches as a whole, and the
  table is deleted and replaced. A one-cell callout box is a common template
  idiom.

So pass 1 **stamps** each top-level body paragraph that is wholly a block token,
before substituting anything, replacing its text with a sentinel carrying a
nonce drawn per export (`DTABLOCK<nonce>_<n>`, deliberately not brace-delimited
so the token grammar does not match it). Pass 2 addresses those sentinels and
nothing else. This fixes both failures at the root: a substituted value cannot
predict the nonce, and a token inside a table is never stamped because only
top-level body paragraphs are. A token the caller supplied a value for is not
stamped either, which is what keeps `variables` winning over a block.

Whatever pass 2 did not render — a token in a sentence, in a table cell, in a
header or footer, or one put back because its `:DATASET` argument named nothing
— is caught by one final scan of the output and reported in a single warning.

Pass 2 is gated on an actual block token so that a template using only inline
placeholders — which is every bundled template today except via
`{DATASETS_DETAIL}` — never goes through officer's re-serialisation at all. That
confines the round-trip risk to templates that opt in.

The alternative considered and rejected: splicing `officer::to_wml(ft)` into
the XML with `xml2::xml_replace()`. It works (verified, identical `doc_index`
result, and `to_wml()` emits a self-contained `<w:tbl xmlns:w=...>`), and it
avoids the officer round-trip, but it needs its own style-id sanitising, its
own trailing-paragraph handling, and it cannot reuse the six `doc`-mutating
builders above. More code for less reuse.

### Two mechanical traps the renderers must respect

**Do not call `.end_section_orientation()`.** `.add_dataset_specs_section()`
(`documentBuilders.R:508`) wraps the specs table in landscape pages via
`body_end_block_section()`. In a user template that would re-section content
that is not ours and silently re-orient the pages around it. Instead set
`flextable::set_table_properties(ft, layout = "autofit", width = 1)`, which
emits `<w:tblW w:type="pct" w:w="5000"/>` + `<w:tblLayout w:type="autofit"/>`
(verified) — the table fits whatever text column the template's page happens to
have, A4 or Letter, portrait or landscape. So the block path calls
`.build_column_specs_table()` / `.build_rules_table()` directly rather than
reusing `.add_dataset_specs_section()` wholesale.

**Do not assume a heading style exists.** Verified: `clinical_dta_template.docx`
has `heading 1/2/3` but **no `heading 4`**, and `dta_numbered_template.docx` has
`heading 1-4` but no `footer`. And `officer::body_add_par(doc, x, style =
"heading 4")` on a document lacking that style is not a soft degrade — it is a
**hard error** (`could not match any style named 'heading 4'`). With
`fallback = TRUE` that error is caught and the user silently gets the built-in
layout instead of their template, which is the worst possible failure mode here.

So block renderers resolve the style against
`officer::styles_info(doc, type = "paragraph")` and use it when present (the
block then joins the template's TOC — `clinical_dta_template.docx` has `toc 1`
and `toc 2`), otherwise fall back to `.add_bold_subheading()`, which is direct
formatting and always works. This is the same problem
`.add_dataset_specs_section()` already documents in its comment.

It also means the block path **cannot reuse `.add_signature_section()`,
`.add_file_specifications()` or `.add_authorized_for_corrections_section()`
wholesale** — all three call `.add_heading()`. Instead, two pure table factories
are extracted from the first two (`.build_file_specs_table()`,
`.build_signature_table()`), matching the `.build_*` convention the other three
already follow, and the block renderers supply their own headings. The extraction
is mechanical and leaves the built-in layout's behaviour unchanged.

## 4. New tokens

Grammar widens from `\{[A-Za-z_][A-Za-z0-9_]*\}` to
`\{[A-Za-z_][A-Za-z0-9_]*(:[^{}]+)?\}` so a block can name one dataset. Both the
"is this worth rewriting" gate and the unresolved report read
`.tv_token_pattern()`, so they stay in step (the comment at `:1094` records what
happened last time they drifted).

Without an argument a block renders **every** applicable dataset, each under its
own heading. With `:NAME` it renders that one; an unknown name is a warning and
the token is left in place.

| Block token | Renders | Built from |
| --- | --- | --- |
| `{COLUMN_SPECS}` / `{COLUMN_SPECS:ADSL}` | heading + specs table per tabular dataset | `.build_column_specs_table()` |
| `{VALIDATION_RULES}` / `:ADSL` | heading + rules table per dataset | `.build_rules_table()` |
| `{FILE_SPECS}` / `:ADSL` | file-handler table per dataset | `.add_file_specifications()` |
| `{DATASETS}` | full per-dataset section: name, type, description, files, columns, rules | the three above |
| `{SUPPLIER_CONTACTS_TABLE}` | contacts table | `.build_contacts_flextable()` |
| `{RECEIVER_CONTACTS_TABLE}` | contacts table | `.build_contacts_flextable()` |
| `{SIGNATURES_TABLE}` | signature table with ruled Signature/Date columns | `.add_signature_section()` |
| `{VERSION_HISTORY_TABLE}` | version / date / changes table | new, ~15 lines |
| `{AUTHORIZED_CORRECTIONS_LIST}` | bulleted list | `.format_authorized_for_corrections_lines()` |

One compatibility alias, which fixes defect (1) of §1: `{DATASETS_DETAIL}` →
`{DATASETS}`. After this, `clinical_dta_template.docx` produces its dataset
section from plain `write_dta()`, with no app involved.

`{VERSION_HISTORY_TABLE}` is the only genuinely new renderer — and it turned out
not to be new either. The built-in layout already built that table, inline in
`.write_dta_docx()`, the one place it never extracted a `.build_*` factory. The
first implementation duplicated it and the two copies immediately disagreed
(centred columns, date formatting, and `changes` handling — the built-in passes
`vh$changes` straight into `data.frame()`, which aborts outright when it holds
more than one string). Extracted to `.build_version_history_table()` and called
from both, which is what the rest of this design does anyway.

Grammar note: the `:ARG` tail is `[^{}]*`, not `[^{}]+`. With `+`,
`{COLUMN_SPECS:}` — the shape a deleted dataset name leaves behind — matched no
pattern at all, so it was neither substituted, nor rendered, nor reported. An
empty argument now means "no argument", i.e. every applicable dataset.

**`{DTA_YAML}` is dropped from scope.** The plan assumed a package-level
DTA→YAML serializer; there is none — `dta_to_list()` / `dta_to_yaml_text()` live
in the Shiny app (`inst/shiny/dta_app/R/utils_dta.R:2146`), not in `R/`, and
`.add_embedded_yaml_section()` takes the YAML text as an argument rather than
producing it. `{YAML_EMBEDDED}` therefore keeps working exactly as today: a
caller-supplied `variables` value, monospaced by the existing
`.tv_needs_yaml_style()` path. Adding a package-level serializer is its own
change, not a rider on this one.

## 5. What gets deleted

Phase 3 removes the markdown round-trip once the app stops feeding it:

- `R/exportTemplateDocx.R`: `.tv_reformat_dataset_bullet`, `.tv_parse_column_meta`,
  `.tv_is_group_condition_summary`, `.tv_expand_group_condition_summary`,
  `.tv_expand_group_condition_from_dta`, `.tv_find_group_rule`,
  `.tv_condition_to_text`, `.tv_constraint_to_text` — **~250 lines**.
- `inst/shiny/dta_app/R/utils_export.R`: `format_datasets_detail()`,
  `.format_rule_detail()` — **~150 lines**.
- Their tests: `test-exportTemplateDocx.R:283-466` (5 blocks),
  `test-shinyapp-export.R:30` and `:99`.

`.tv_template_markdown_to_text()`, `.tv_looks_markdown()` and
`.tv_markdown_bullet_to_word_bullet()` **stay** — a caller-supplied `variables`
value may still be markdown, and de-marking it is still right.

Measured after the fact, rather than as claimed here beforehand: 536 lines of
`R/` and `inst/` deleted against a 486-line `R/exportTemplateBlocks.R`, so
production code is **+228 net**, not negative. Tests are net **-203** (60 added
to existing files, 263 deleted) plus a new 497-line block-placeholder file. What
the deletion actually bought is not size but the removal of an entire class of
bug: nothing re-parses generated text any more, and there is one renderer per
table instead of two that could drift.

## 6. Phases

Each phase is independently shippable and independently testable.

**Phase 1 — the block engine.** Token grammar; block/inline split in
`.replace_placeholders_in_xml()`; pass 2 with `cursor_reach`; style resolution
against `styles_info()`; the whole-paragraph-only rule; `:NAME` argument
parsing; header/footer and inside-a-table-cell cases warn rather than splice.
Ship with exactly one block token, `{COLUMN_SPECS}`, to prove the mechanism.

**Phase 2 — the rest of the catalogue.** The other nine tokens plus the two
aliases. Mostly one function call each. Extend `.tv_placeholder_catalog()` with
a block catalogue; `dta_template_placeholders()` gains a `kind` column so a
template author can tell the two apart. *The existing drift test
(`test-exportTemplateDocx.R:598`) asserts the catalogue and
`.extract_template_variables()` are the same set — it must be extended, because
block tokens deliberately have no entry there.*

**Phase 3 — collapse the round-trip.** Point the app at `{DATASETS}`; delete
the eight `.tv_*` re-parsers and the two app formatters; delete their tests.

**Phase 4 — fix defect (2).** `list_available_templates()` must not offer
`dta_numbered_template.docx`. **Decided: scan, not name filter.** A candidate has
to prove it contains at least one `{PLACEHOLDER}`; a blocklisted name would let
the next styles-only reference document reintroduce the same trap. The scan reads
paragraph *text* rather than raw XML, because Word splits a typed placeholder
across runs freely and a raw grep on `document.xml` would produce a false
negative on a template the exporter handles fine. No caching — two files, one
unzip each, on a modal open.

## 7. Tests

New file `tests/testthat/test-exportTemplateDocx-blocks.R`. Assertions read the
document back with `officer::docx_summary()` — never `file.exists()`:

- `{COLUMN_SPECS}` yields `content_type == "table cell"` rows whose text
  contains every `spec@id` of the fixture.
- The table sits **between** the surrounding paragraphs (compare `doc_index`),
  not appended at the end.
- `{VALIDATION_RULES}` cell text matches `translate_rule_to_human()` for a
  `DTARuleGroupCondition` fixture — the case the deleted regex path handled
  worst.
- `{COLUMN_SPECS:ADSL}` renders one dataset; `{COLUMN_SPECS:NOPE}` leaves the
  token and warns.
- A paragraph reading `Columns: {COLUMN_SPECS}` is left untouched and reported
  unresolved.
- `variables = list(COLUMN_SPECS = "plain text")` still wins over the block
  renderer.
- A template with only inline tokens produces a byte-identical result to the
  current implementation — the pass-2 gate does not fire.
- Regression for defect (1): `write_dta(dta, template =
  clinical_dta_template.docx)` with **no** `template_variables` produces a
  document containing the fixture's column ids, and no literal
  `{DATASETS_DETAIL}` anywhere.

## 8. Risks

- **officer round-trip fidelity on a real corporate template.** Verified to
  survive pass 2 unchanged, by count and by relationship target: TOC fields
  (`w:fldChar` / `w:instrText`, including the `TOC \o` instruction),
  hyperlinks and their `document.xml.rels` entry, bookmarks, section properties
  (`w:sectPr`), header and footer references, and existing tables — while the
  block still renders in place and the document keeps its order. **Untested:**
  content controls (`w:sdt`), footnotes, custom XML parts, embedded images.
  Mitigated for those by the pass-2 gate (only templates that stamp a block
  token are re-serialised at all) and by `fallback = TRUE`, which catches an
  error and reverts to the built-in layout. Still worth one run against a
  genuine Boehringer template before this reaches users.
- **Wide tables in a narrow template.** `layout = "autofit", width = 1` fits the
  table to the page but a 6-column specs table on A4 portrait will be cramped.
  The built-in layout solves this with landscape sections, which we deliberately
  do not do here. If it reads badly, the follow-up is a `{COLUMN_SPECS_LANDSCAPE}`
  variant that does insert section breaks — not a change to the default.
- **A consumed placeholder leaves one empty paragraph** ahead of the rendered
  block: the cursor element is replaced with `body_add_par(doc, "", pos = "on")`
  and the renderer then appends after it. Accepted as a spacer rather than
  complicating every renderer with a "first element replaces, rest append"
  contract. If it reads badly, the fix is local to `.tv_render_blocks()`.
- **Repeating regions are out of scope.** No `{#DATASETS}...{/DATASETS}` loop
  construct. `{DATASETS}` expands to all datasets with generated headings; a
  template author who wants their own per-dataset layout must write
  `{COLUMN_SPECS:ADSL}` once per dataset. Add the loop only if someone asks.

## 9. Obligations

- `CHANGELOG.md` entry under `## [Unreleased]` — user-facing feature.
- `Rscript -e "roxygen2::roxygenise()"` with the version in
  `Config/roxygen2/version`; `dta_template_placeholders()` and
  `export_with_template()` both gain documentation.
- `Rscript .github/scripts/style.R` before committing (it also styles `inst/`).
- `README.md` and `vignettes/DTAtools.Rmd` both list the placeholder set — both
  need the block tokens.
- No version bump.
