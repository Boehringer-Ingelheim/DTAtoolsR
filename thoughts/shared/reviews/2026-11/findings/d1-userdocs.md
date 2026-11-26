# Findings — key d1-userdocs (user-facing documentation)

Slice: README.md, vignettes/* (DTAtools.Rmd, private-templates.Rmd, DTAtools-manuscript.Rmd),
DTA_YAML_GUIDE.md, CHANGELOG.md, docs/ (index.html, 404.html, _config.yml),
dts_metadata_coverage_overview_2026-07-28.md, inst/extdata/agent-skills/**,
inst/extdata/template-repo-skeleton/**, inst/extdata/library/** (docs + YAML),
inst/extdata/ci/*, example YAMLs (clinical_dta*.yaml, matrix_dta.yaml, gf_dataset.yaml).

Method: single reviewer (no sub-agents; the part-reviewers launched first were cut off by the
account limit and left no findings). Every statement of the slice was read against the code in
R/ and inst/shiny where it was read (see Coverage); in addition cross-document mechanical passes:
function-call tokens vs definitions, named arguments of example calls vs formals (control-tested),
system.file targets, anchors, option/env-var/condition-class/lint-code existence, defaults, version
strings, CHANGELOG structure, doubled-word/typo scan. Counts: 3 major, 27 minor, 9 nit; no blocker.

---
## Findings (written as confirmed; numbering is final)

### F-d1-userdocs-01  [minor] [doc-wrong]
- Where: CHANGELOG.md:4091-4170 (second `## [0.18.1] - 2026-08-16` block)
- What: The 0.18.1 release section appears twice; the second block is an exact 80-line copy of the first block's opening (intro + "Fixed" subsections), so the heading, the manifest/overflow entries and the anchor `0.18.1` are duplicated.
- Evidence: line 3946 `## [0.18.1] - 2026-08-16` and line 4091 `## [0.18.1] - 2026-08-16`; `diff <(sed -n 3946,4025p CHANGELOG.md) <(sed -n 4091,4170p CHANGELOG.md)` prints nothing (identical). The first block continues (`### Added` benchmark mode at 4026-4090), the second ends at 4170 and `## [0.18.0]` follows at 4171. A reader scanning down sees 0.18.1 twice, with the "Added: benchmark mode" entries only in the first.
- Failure scenario: Keep-a-Changelog tooling / release scripts that extract "the section of version 0.18.1" get either two sections or a truncated one; the duplicated "Fixed" text reads as two separate sets of fixes.
- Confidence: verified-by-reading (diff run over the two ranges)
- Suggested direction: delete lines 4091-4170 (keep one 0.18.1 section).

### F-d1-userdocs-02  [minor] [doc-wrong]
- Where: CHANGELOG.md:5489-5515 (link-reference list) vs the `## [x.y.z]` headings
- What: 20 release headings have no link-reference definition, so `## [0.21.0]` etc. render as literal bracketed text instead of compare links; several existing definitions compare non-adjacent tags (they skip headed releases).
- Evidence: headings without a `[x.y.z]: url` definition: 0.1.0 0.2.0 0.3.0 0.4.0 0.5.0 0.6.0 0.7.0 0.7.1 0.7.2 0.7.3 0.7.4 0.7.5 0.8.0 0.15.0 0.16.1 0.17.0 0.18.2 0.19.0 0.20.0 0.21.0 (computed with `grep '^## \['` vs `grep '^\[..\]:'`). Defined but skipping: `[0.23.0]: .../compare/v0.20.1...v0.23.0`, `[0.20.1]: .../compare/v0.18.1...v0.20.1`, `[0.17.1]: .../compare/v0.16.0...v0.17.1`, `[0.15.1]: .../compare/v0.13.0...v0.15.1` (headings 0.21.0, 0.20.0, 0.19.0, 0.18.2, 0.16.1, 0.15.0 exist between those tags).
- Failure scenario: A reader clicking release headings 0.15.0-0.21.0 and 0.1.0-0.8.0 gets no link; the compare ranges for the neighbouring releases silently contain the changes of the skipped, headed releases.
- Confidence: verified-by-reading for the missing definitions; inferred for "tag exists" (no git history in the snapshot)
- Suggested direction: either add a definition for every headed release or fold the untagged releases' headings into the tagged ones; make each compare link adjacent.

### F-d1-userdocs-03  [minor] [inconsistency]
- Where: README.md:11-12,166; vignettes/DTAtools.Rmd:2,8,31,76; DESCRIPTION:2; inst/shiny/dta_app/R/app_config.R:60 (vs R/DTAtools-package.R:4-5, DESCRIPTION:11, DTA_YAML_GUIDE.md:1, docs/index.html:603)
- What: The package uses two different expansions for the same abbreviations: "Data Transmission Agreement/Specification" (README, vignette title) and "Data Transfer Agreement/Specification" (package help, DESCRIPTION Description, YAML guide, app default document name); DESCRIPTION's own Title and the app's default subtitle mix them inside one sentence.
- Evidence: README.md:11 `**Data Transmission Agreements (DTAs)** and **Data Transmission Specifications (DTS)**`; vignettes/DTAtools.Rmd:2 `title: "DTAtools: Validating Data Transmission Specifications in R"`; R/DTAtools-package.R:4-5 `A Data Transfer Agreement (DTA) is a written contract ...; a Data Transfer Specification (DTS) is the machine-readable part`; DESCRIPTION:2 `Data Transfer Agreements (DTAs) and Data Transmission Specifications (DTS)` but DESCRIPTION:11 `data transfer agreements (DTAs) and data transfer specifications (DTS)`; app_config.R:60 `Validation and authoring of Data Transfer Agreements (DTA) and Data Transmission Specifications (DTS)`; code default document name is "Data Transfer Agreement" (exportDocuments.R:63, documentBuilders.R:199). README.md:1052 and DTA_YAML_GUIDE.md:1 say "Data Transfer Specification".
- Failure scenario: A reader cannot tell whether DTS means Transfer or Transmission; search for the term finds half the documents; the vignette title (shown in `vignette()` index) disagrees with the package title and the app subtitle.
- Confidence: verified-by-reading
- Suggested direction: pick one (the code, the app and the YAML guide use "Data Transfer Agreement/Specification") and replace the others, including DESCRIPTION Title and the app subtitle.

### F-d1-userdocs-04  [minor] [doc-wrong]
- Where: README.md:202-206 (ASCII architecture) and img/DTAtools_architecture.svg (the figure README.md:172 embeds)
- What: The architecture diagram's rule list shows three rule classes; the fourth, `DTARuleGroupCondition`, is missing although the same README documents `group_condition` as a rule type and the class exists.
- Evidence: README.md:202-206 `└── rules (list of DTARule)  ├── DTARuleColCondition × N  ├── DTARuleColRange × N  └── DTARuleColUnique × N`; R/DTARuleGroupCondition-class.R:43-45 `DTARuleGroupCondition <- S7::new_class(... parent = DTARule`; NAMESPACE exports it (line 34); `grep -o 'DTARule[A-Za-z]*' img/DTAtools_architecture.svg` yields only DTARule, DTARuleColCondition, DTARuleColRange, DTARuleColUnique.
- Failure scenario: A reader of the architecture section concludes group rules are not part of `rules` (and the README feature list at line 45 contradicts the diagram).
- Confidence: verified-by-reading (README, SVG text, class file); the vignette's own diagram not yet checked here
- Suggested direction: add `DTARuleGroupCondition × N  <- group-level logic` to the ASCII tree and the SVG/drawio source.

### F-d1-userdocs-05  [minor] [doc-wrong]
- Where: README.md:617-619; vignettes/DTAtools.Rmd:1786-1788; docs/index.html:1743-1744 (vs benchmarks/streaming.csv)
- What: The quoted "measured across a 16-fold increase in input" memory figures (in-memory path grew from 51 MB to 272 MB) do not match the committed benchmark record, which shows 49.8 MB to 312.2 MB for the same 16-fold span.
- Evidence: README.md:618-619 `the working set stayed flat at ~19 MB while the in-memory path grew from 51 MB to 272 MB`; benchmarks/streaming.csv rows (n_rows 50000 -> 800000, a 16-fold increase): `eager_hold_mb` 49.8 -> 134.3 -> 312.2 and `stream_hold_mb` 18.4 -> 18.6 -> 19.3. No column contains 51 or 272. The numbers originate from the 0.17.0 entry (CHANGELOG.md:4755-4756), i.e. an earlier benchmark harness (bench_streaming.R now injects dirty rows, keyed and grouped rules).
- Failure scenario: A reader who re-runs `benchmarks/bench_streaming.R` (or reads the CSV shipped next to it) cannot reproduce the figures quoted as "measured".
- Confidence: verified-by-reading (CSV and three doc locations); inferred which run produced 51/272
- Suggested direction: quote the committed 49.8 -> 312.2 MB (and ~19 MB) figures, or re-measure and update CSV and all three documents together.

### F-d1-userdocs-06  [minor] [doc-wrong]
- Where: README.md:1026-1027; vignettes/DTAtools.Rmd:2686 (vs R/run_dta_app.R:15-19,32)
- What: Both documents say the app "needs the suggested packages shiny, bslib and DT", but `run_dta_app()` also requires `shinyjs` and aborts without it.
- Evidence: README.md:1027 `the suggested packages **shiny**, **bslib** and **DT**`; R/run_dta_app.R:32 `required <- c("shiny", "bslib", "DT", "shinyjs")` followed by `cli::cli_abort("The DTAtools app needs additional package{?s}: ...")`. The function's own roxygen (run_dta_app.R:15-19) lists four packages in its first sentence and then says "If any of **shiny**, **bslib** or **DT** is missing, an informative error ..." (also omits shinyjs).
- Failure scenario: A user installs the three documented packages, calls `run_dta_app()` and gets an abort naming `shinyjs`, which no user document mentions.
- Confidence: verified-by-reading
- Suggested direction: list `shiny`, `bslib`, `DT` and `shinyjs` everywhere (and optionally `shinyAce`/`writexl` as optional extras, as run_dta_app.R:17-19 does).

### F-d1-userdocs-07  [minor] [doc-missing]
- Where: README.md:1064-1085 (scaffold listing) vs R/createTemplateRepo.R:197-232; the same omission in vignettes/private-templates.Rmd:194-209
- What: The directory tree shown for `create_template_repo()` omits the agent skill folder the function writes by default (`agent_skill = TRUE`) and presents the GitHub workflow as the only CI file, although `ci` can also write `bitbucket-pipelines.yml` and `Jenkinsfile`.
- Evidence: README.md:1070-1085 ends with `.github/workflows/validate-templates.yml`; createTemplateRepo.R details list `<path>/.[assistant-tool]/skills/dta-template-authoring/SKILL.md (agent_skill = TRUE, default)`, `<path>/.[assistant-tool]/skills/dta-template-authoring/references/`, `<path>/bitbucket-pipelines.yml (ci = "bitbucket")`, `<path>/Jenkinsfile (ci = "jenkins")`; the signature is `create_template_repo(path, examples = TRUE, ci = TRUE, overwrite = FALSE, agent_skill = TRUE)`.
- Failure scenario: A user running `create_template_repo("my-templates")` finds a `.[assistant-tool]/` folder (and, with `ci = "jenkins"`, other files) the README never mentions, and does not learn that `agent_skill`/`ci` exist.
- Confidence: verified-by-reading
- Suggested direction: add the `.[assistant-tool]/skills/...` lines and one sentence on `ci = c("github","bitbucket","jenkins")` / `agent_skill` to the README block.

### F-d1-userdocs-08  [minor] [dead-code]
- Where: R/exportTemplateBlocks.R:30-37 (legacy block-token alias and its comment)
- What: `.tv_block_aliases()` keeps the legacy token `{DATASETS_DETAIL}` mapped to `{DATASETS}` purely for compatibility, and its roxygen claims the bundled Word template still contains that token, which is no longer true.
- Evidence: `#' `{DATASETS_DETAIL}` is what `inst/extdata/word-templates/clinical_dta_template.docx` contains, and what the Shiny app used to supply as a markdown string.` / `.tv_block_aliases <- function() { c("{DATASETS_DETAIL}" = "{DATASETS}") }`. Extracting the docx (`unzip -p clinical_dta_template.docx word/document.xml | grep -o '{[A-Z_#/:]*}'`) lists `{DATASETS}`, `{#DATASETS}`/`{/DATASETS}` once each and no `{DATASETS_DETAIL}`; the app (app.R:10145) states it deliberately does not supply it.
- Failure scenario: A maintainer trusts the comment, keeps an alias nothing in the package uses, and a user reading `dta_template_placeholders()` never sees `{DATASETS_DETAIL}` documented although it still works (undocumented synonym).
- Confidence: verified-by-reading (docx text extracted; usages of the alias grepped across R/, inst/shiny, tests)
- Suggested direction: pre-1.0, delete the alias (and its test/comment) rather than keep a hidden synonym.

### F-d1-userdocs-09  [minor] [dead-code]
- Where: R/DTADataSetFile-class.R:62-87
- What: The `DTADataSetFile` constructor silently rewrites every non-`DTAFileAny` handler into a `DTAFileAny` purely so that "an existing document that named a reader type before this constraint existed must keep loading" -- a backwards-compatibility coercion, with errors swallowed by `tryCatch(..., error = function(e) NULL)` on each property read.
- Evidence: `# is not. The fix is COERCION, not rejection: an existing document that named a reader type before this constraint existed must keep loading, so every non-DTAFileAny handler is rebuilt as a DTAFileAny ...` then `filename = tryCatch(h@filename, error = function(e) NULL), pattern = tryCatch(h@pattern, error = function(e) NULL), ... number_of_files = NULL`.
- Failure scenario: `DTADataSetFile(id = "x", files = list(DTAFileCSV("a.csv")))` (or a `files:` entry with `type: csv` under a `type: file` dataset) is accepted and silently becomes a `DTAFileAny` presence entry; the reader settings (separator, header, quote, encoding) are discarded with no message, while the documentation (README.md:698-701) describes only `type: any` for file datasets, so a misdeclared dataset is never reported to its author.
- Confidence: verified-by-reading (code only; min/max file counts are carried over, only reader-specific properties are lost)
- Suggested direction: pre-1.0 no compat is needed -- reject non-`DTAFileAny` handlers with a `cli_abort` (as DTADataSetTabular does for reader-less handlers) and delete the coercion.

### F-d1-userdocs-10  [minor] [doc-wrong]
- Where: README.md:1598-1599; vignettes/DTAtools.Rmd:54, 146, 612; vignettes/DTAtools-manuscript.Rmd:275
- What: Four documents still describe column-spec validation as running a JSON Schema validator ("uses JSON Schema for fast, row-level validation", "JSON Schema validation of every row", "compiled to JSON Schema and evaluated row-wise"), but the package no longer runs any JSON Schema validator; JSON Schema is only the vocabulary the constraints are derived in.
- Evidence: README.md:1598 `uses JSON Schema for fast, row-level validation`; vignettes/DTAtools.Rmd:612 `**Column spec errors** — JSON Schema validation of every row against column types,`; but R/columnSpecChecks.R:1 `# Column-spec-axis validation without a JSON Schema validator.` and R/DTAColumnSpecCollection-class.R:783-786 `Validation no longer runs through a JSON Schema validator at all (see R/columnSpecChecks.R); this function's job is serialising a spec collection to JSON Schema for other tools to consume`; R/validationFunctions.R:3-4 `evaluating each column's values directly rather than serialising the table and running a JSON Schema validator over it`. DESCRIPTION Imports contain no JSON Schema validator package.
- Failure scenario: A reader (or a qualification reviewer) takes the documents as the description of the validation mechanism and expects JSON Schema semantics (draft behaviour, `additionalProperties`, etc.) that the vectorised R evaluator does not implement; the claim is also used as a selling point ("fast ... validation").
- Confidence: verified-by-reading
- Suggested direction: say that constraints are derived as JSON Schema keywords (`as_json_schema()`) and evaluated directly in vectorised R; drop "uses JSON Schema for ... validation".

### F-d1-userdocs-11  [minor] [doc-wrong]
- Where: README.md:1662 (vs R/exportTemplateDocx.R:136-144; correct in vignettes/DTAtools.Rmd:3089)
- What: The Key Functions table gives `export_with_template(x, template, file)`, but the real formals are `export_with_template(dta, template, output, variables, quiet, fallback, overwrite)`; there is no `x` or `file` argument.
- Evidence: README.md:1662 `` `export_with_template(x, template, file)` ``; R/exportTemplateDocx.R:136 `export_with_template <- function(dta, template, output, variables = NULL, quiet = FALSE, fallback = TRUE, overwrite = FALSE)`. The vignette table at DTAtools.Rmd:3089 has `export_with_template(dta, template, output)`, so the two documents also disagree with each other.
- Failure scenario: `export_with_template(x = dta, template = "t.docx", file = "out.docx")` aborts with an "unused argument" error.
- Confidence: verified-by-reading
- Suggested direction: change the README row to `export_with_template(dta, template, output)`.

### F-d1-userdocs-12  [minor] [inconsistency]
- Where: README.md:1114-1117,1133; DTA_YAML_GUIDE.md:390-393 (vs R/DTAColumnSpec-class.R:160-172)
- What: The README says a column's `type` and `nullable` are "(mandatory)", the YAML guide says only `id` is required and `nullable` merely should be set ("if omitted, missing values fail"), and the code accepts a column with neither.
- Evidence: README.md:1117 `` `type`: (mandatory) the type ``, README.md:1133 `` `nullable`: (mandatory) whether the column may be empty ``; DTA_YAML_GUIDE.md:387 `` `id` | **required** ``, :393 `` `nullable` | `true` / `false`. **Always set it explicitly** — if omitted, missing values fail. ``; R/DTAColumnSpec-class.R:160-166 `constructor = function(id, label = NULL, type = NULL, format = NULL, length = NULL, nullable = NULL, ...)`; no lint code covers a missing type/nullable (the `lint_dta()` code list at R/lintDTA.R:506-546 has none), and `as_json_schema_type()` (DTAColumnSpec-class.R:609-624) deliberately admits every base type when `structure` is NULL. The vignette (DTAtools.Rmd:230-242, only `id` ticked "Required") and docs/index.html:998-1005 (`type`, `nullable` "optional") agree with the guide, so the README is the outlier.
- Failure scenario: A spec author who follows the README writes `{id: AGE}` expecting a load error (mandatory) but gets a column that accepts any value type and, with `nullable` omitted, silently rejects every blank cell; a reader of the guide gets a different rule from the README.
- Confidence: verified-by-reading
- Suggested direction: make the two documents say the same (and match the code): only `id` is enforced; `type` and `nullable` should be written explicitly. Or enforce them in `lint_dta()` and keep "mandatory".

### F-d1-userdocs-13  [minor] [dead-code]
- Where: R/DTARuleColRange-class.R:12-16,38 (documented as supported at README.md:1383-1384 and DTA_YAML_GUIDE.md:634)
- What: The `range` argument of `col_range` is labelled "Legacy argument" in the class documentation, yet README and YAML guide present `range: [lo, hi]` as an equal alternative to `min`/`max`.
- Evidence: R/DTARuleColRange-class.R `@param range Numeric vector or list of length 2 (lower, upper). Legacy argument for range checks; it is used only when both min and max are NULL, and is ignored otherwise.`; README.md:1383-1384 `The bounds can also be given as `range: [18, 65]` in place of `min`/`max`.`; DTA_YAML_GUIDE.md:634 `(`range: [18, 85]` is equivalent to `min`/`max`.)`.
- Failure scenario: The code and the documents disagree about whether `range:` on `col_range` is current or a legacy spelling; pre-1.0 a second spelling for the same key pair is dead weight (and `range` combined with `min` is silently ignored).
- Confidence: verified-by-reading
- Suggested direction: pick one spelling; if `range:` stays, drop the word "Legacy" and make `range` + `min`/`max` an error rather than "ignored"; if not, remove the alias and the two documentation sentences.

### F-d1-userdocs-14  [nit] [dead-code]
- Where: R/DTADataSet-class.R:350-366
- What: Exported `dta_dataset_from_list(x, recursive = TRUE)` carries a parameter that is never read.
- Evidence: roxygen `@param recursive Unused. It is only set to FALSE by the function's own recursive call for a list of datasets and is never read; leave it at its default.` and the body `return(lapply(x, dta_dataset_from_list, recursive = FALSE))`; no other use of `recursive` in the function.
- Failure scenario: A caller passing `recursive = FALSE` expecting no recursion gets recursion anyway; the argument is part of the exported signature for no purpose.
- Confidence: verified-by-reading
- Suggested direction: delete the parameter.

### F-d1-userdocs-15  [minor] [doc-wrong]
- Where: DTA_YAML_GUIDE.md:193-195 (vs R/DTADataSetFile-class.R:62-87)
- What: The guide says only a `sas7bdat`/`xpt` handler inside a `type: file` dataset is coerced to `any`, but the code coerces EVERY non-`any` handler (csv and tsv too), while the handler table (line 173) lists `any` as the only type for `file`.
- Evidence: guide `A sas7bdat or xpt handler declared inside a type: file dataset is coerced to a reader-less any handler`; code `files <- lapply(files, function(h) { if (inherits(h, "DTAtools::DTAFileAny")) return(h); DTAFileAny(filename = tryCatch(h@filename, ...` for every handler class (DTADataSetFactory passes `type = csv` handlers through `dta_file_handlers_from_list()` unchanged).
- Failure scenario: A `type: file` dataset with `files: {type: csv, filename: report.csv}` loads without complaint (not an error, not mentioned in the guide) and is silently checked as a presence-only file.
- Confidence: verified-by-reading
- Suggested direction: either document "any reader handler is coerced" or (preferred, see the dead-code finding on DTADataSetFile) reject it with an error and delete the coercion.

### F-d1-userdocs-16  [minor] [dead-code]
- Where: README.md:507-508; vignettes/DTAtools.Rmd:657-659,3063; docs/index.html:773; R/containerAssignments.R:84-262 (`upgrade_dta()`), R/validationFunctions.R:554-600 (`dta_migrate_validation_details`), R/DTARuleColUnique-class.R:12, R/DTARuleColCondition-class.R:91-92
- What: The documentation advertises backward-compatibility machinery for objects, artifacts and arguments from older versions (exported `upgrade_dta()`, automatic re-stamping of "objects saved by an older version", `n_import_errors = NA` for results that "predate the import axis", a rule `type` argument "kept for backward compatibility"); the owner states no backwards compatibility is needed before 1.0.0, so this is dead weight and documented surface that can only mislead.
- Evidence: README.md:507-508 `Objects saved by an older version are upgraded when they are used; `upgrade_dta()` does it explicitly.`; vignettes/DTAtools.Rmd:657-659 `` `n_import_errors` is `NA` rather than `0` when the recorded result predates the import axis ``; vignettes/DTAtools.Rmd:3063 `upgrade_dta(x) | Re-stamp a DTA or dataset saved by an older version with the current classes`; R/DTARuleColUnique-class.R:12 `@param type Optional character value kept for backward compatibility.`; R/DTARuleColCondition-class.R:91 `The property union still admits a bare character for backward compatibility, but nothing can evaluate one`. `dta_is_stale()`/`dta_upgrade()` calls open check(), load_file(), load_delivery() (DTA-class.R:519,1033; DTADataSetTabular-class.R:891,1491; loadDelivery.R:159) and `dta_migrate_validation_details()` is applied on every artifact read (DTADataSetTabular-class.R:1289-1310, DTADataSetFile-class.R:1007-1028).
- Failure scenario: Extra code paths run on every call purely for old objects; the docs imply a supported upgrade contract (`upgrade_dta()`, a refused "matrix_format_outdated" object) that the project explicitly does not need, and every compat branch is untested surface that can hide real defects.
- Confidence: verified-by-reading (docs and the code sites cited); not traced: whether the qualification suite requires any of these (inst/qualification lists `upgrade_dta` in requirements/_api.yaml if so)
- Suggested direction: remove `upgrade_dta()` and the stale-object/migration code, the `NA`-for-old-result statements and the deprecated `type =` arguments; delete the matching README/vignette/index.html lines.

### F-d1-userdocs-17  [minor] [inconsistency]
- Where: README.md:178-219 (ASCII tree) and img/DTAtools_architecture.svg vs vignettes/DTAtools.Rmd:84-128
- What: The package's three architecture diagrams disagree: only the vignette's ASCII tree shows the `containers` level (`DTAContainer`), none of the three shows `DTARuleGroupCondition`, and the README tree and the SVG (identical copies in img/ and vignettes/) have no `DTAContainer` node although the README documents containers at length (README.md:402-535) and lists `DTAContainer` as a core class (README.md:1607).
- Evidence: vignettes/DTAtools.Rmd:89-90 `├── containers   ← delivery archives named once, ... (list of DTAContainer)` is absent from README.md:178-183 (`DTA ... ├── metadata ... └── datasets`); `grep -o 'DTAContainer\|DTARuleGroupCondition' img/DTAtools_architecture.svg` finds neither; `cmp img/DTAtools_architecture.svg vignettes/DTAtools_architecture.svg` reports identical files.
- Failure scenario: Readers of the README diagram do not learn that a `DTA` holds containers; the picture the vignette (R CMD check output) embeds is out of date relative to the vignette's own text tree right below it.
- Confidence: verified-by-reading (SVG searched as text; the .drawio sources were not diffed)
- Suggested direction: regenerate SVG/drawio with the `containers` node and the fourth rule class and make the README tree the same as the vignette's.

### F-d1-userdocs-18  [minor] [doc-wrong]
- Where: vignettes/DTAtools.Rmd:752
- What: The vignette explains the missing `HEIGHT` column of the error file as "required by spec, since `nullable: false`", but column presence does not depend on `nullable`: every declared column must be present.
- Evidence: vignettes/DTAtools.Rmd:752 `- A missing `HEIGHT` column (required by spec, since `nullable: false`)`; R/columnSpecChecks.R:912-915 `# A column whose schema could not be derived is still required to be present: ... out$required <- unique(column_names[named])` (every named column, whatever its `nullable`), and DTA_YAML_GUIDE.md:382-383 `The delivered table must contain exactly the declared columns — a missing column and an undeclared extra column are both errors.`
- Failure scenario: A reader concludes a `nullable: true` column may be absent from the delivered file; it may not (`must have required property '<col>'`).
- Confidence: verified-by-reading
- Suggested direction: write "(every declared column must be present)" and drop the `nullable` explanation.

### F-d1-userdocs-19  [nit] [doc-missing]
- Where: README.md:656-681; vignettes/DTAtools.Rmd:683-691 (vs R/validationReporting.R:540-546)
- What: Both describe the extra columns of `inspect()` as prefixed `columnspec_`, `context_`, `failing_` and `import_`, but the function documents and returns more: `group_violation_*` columns (for `group_condition` rules), `details_*` summary columns and `rule_definition_class`.
- Evidence: R/validationReporting.R:540-546 `Detail fields are appended as flat columns prefixed with columnspec_, context_, failing_, import_ and group_violation_. Summary fields of the validation result are added as details_* columns, and the class of the rule behind a rule message as rule_definition_class.`; the two tables list only `import`, `columnspec` and `rule` rows with the four prefixes.
- Failure scenario: A user inspecting a `group_condition` failure looks for `failing_*` per the table and finds the offending groups under `group_violation_*`.
- Confidence: verified-by-reading
- Suggested direction: add the `group_violation_` row (and a sentence on `details_*`) to both tables.

### F-d1-userdocs-20  [major] [doc-wrong]
- Where: vignettes/DTAtools.Rmd:1716-1721 (vs R/DTADataSetTabular-class.R:1412-1427 and R/validationFunctions.R:352-353)
- What: The vignette says `check()`'s two scan knobs `batch_rows` and `max_errors` are "both ignored for a table already in memory"; only `batch_rows` is. `max_errors` (default 10000) also caps the per-cell detail kept for an in-memory table, with no spill, so `messages()`/`inspect()` silently hold at most 10000 rows per axis there.
- Evidence: vignette `check() takes the two scan knobs, both ignored for a table already in memory: batch_rows (...) and max_errors (how much per-cell detail is held in RAM ...)`; check() roxygen (DTADataSetTabular-class.R:1422-1427) `It applies to a table held in memory as well, where it bounds retained detail only: there is no spill there, so the dropped rows are recovered only by checking again with a larger cap. A table constructed in R and checked straight away, whose complete detail is wanted, should therefore pass max_errors = Inf`; code `full_error <- dta_truncate_error_frame(full_error, max_errors)` (validationFunctions.R:352) runs on the in-memory path.
- Failure scenario: A user checks an in-memory table with 50,000 failing cells, believes (per the vignette) that `max_errors` is irrelevant for in-memory tables, and gets a `messages()` table of 10,000 rows (counts in `results()` remain exact); the missing 40,000 rows cannot be recovered with `collect_full_errors()` (no spill).
- Confidence: verified-by-reading
- Suggested direction: state that `batch_rows` is ignored for in-memory tables while `max_errors` applies to both (use `max_errors = Inf` for complete detail of an in-memory table); also mention it in the README (README.md:589-597) which says only "spills ... collect_full_errors() reassembles".

### F-d1-userdocs-21  [minor] [dead-code]
- Where: vignettes/DTAtools.Rmd:2918-2919; README.md:1430-1433,1455,1479; DTA_YAML_GUIDE.md:690,697 (vs R/DTARuleGroupCondition-class.R:18-24,180-187,239-241)
- What: Constraint-type aliases (`not_both` for `mutually_exclusive`, `implies` for `requires`) are kept, and documented, "for compatibility with earlier drafts"; the code additionally lets `mutually_exclusive` read `if`/`then` and `requires` read `left`/`right` as hidden synonyms. Pre-1.0 these are compat leftovers.
- Evidence: vignette `For compatibility with earlier drafts, not_both is accepted as an alias for mutually_exclusive, and implies as an alias for requires.`; code `ctype <- switch(as.character(ctype), not_both = "mutually_exclusive", implies = "requires", as.character(ctype))`, `left <- cst$left %||% cst[["if"]]` (mutually_exclusive) and `if_name <- cst[["if"]] %||% cst$left` (requires) -- the cross-spelling is documented nowhere.
- Failure scenario: A constraint `{type: requires, left: a, right: b}` loads silently with the meaning `if: a, then: b`; two spellings per concept double the surface the docs and tests must cover.
- Confidence: verified-by-reading
- Suggested direction: drop the aliases and cross-spellings (and the README/guide/vignette mentions) -- pre-1.0 one spelling per concept.

### F-d1-userdocs-22  [minor] [inconsistency]
- Where: README.md:1625-1667 and vignettes/DTAtools.Rmd:3050-3094 (the two "Key Functions" tables)
- What: The two reference tables claim to list the key functions but disagree with each other and are both incomplete: the vignette table lacks `open_file` and `explain_messages` (README has them); the README table lacks `container_assignments`, `container_findings`, `file_name_conflicts` and `upgrade_dta` (vignette has them); neither lists `lint_dta`, `validate_template`, `lint_template_repo`, `create_template_repo`, `write_template_guide`, `collect_full_errors`, `run_qualification`/`qualification_requirements` (README mentions the last two only in prose).
- Evidence: `grep -c` for each name inside the two tables: `explain_messages(x, id, n)` appears at README.md:1643 and nowhere in the vignette's table; `container_findings(x)` at vignettes/DTAtools.Rmd:3061 and not in README's table (the README documents containers at length, lines 402-535); NAMESPACE exports all of them.
- Failure scenario: A reader using the "API Quick Reference" of one document never learns of functions the other lists (e.g. `explain_messages()`, `container_findings()`); the README's own container section needs functions its reference table omits.
- Confidence: verified-by-reading
- Suggested direction: generate both tables from one source (or make them identical) and add the authoring/linting functions.

### F-d1-userdocs-23  [minor] [doc-missing]
- Where: README.md, vignettes/DTAtools.Rmd, DTA_YAML_GUIDE.md, vignettes/private-templates.Rmd, docs/index.html (absence)
- What: Several exported functions are not mentioned in any user-facing document: `set_dta_compute_threads`, `validation_benchmark` (and the `benchmark = TRUE` argument of `check()`/`validate_file_stream()`), `write_dataset_metadata`, `write_file_specification`, `dta_from_list`, `dta_dataset_from_list`, `validate_rules`, `apply_rules`, `get_authorized_for_corrections`, `get_receiver_reviewers`, `get_transmission_dates`, `get_version_history_df`, `validate_transmission_dates`.
- Evidence: a loop over every `export(...)` in NAMESPACE with `grep -c -F` against the five documents printed `UNDOC:` for 39 names, among them `set_dta_compute_threads`, `validation_benchmark`, `write_dataset_metadata`, `write_file_specification`, `dta_from_list`, `validate_rules`, `apply_rules`, `get_version_history_df` (they have man pages and CHANGELOG entries, e.g. CHANGELOG.md:4028 introduces `validation_benchmark()`).
- Failure scenario: Features such as the benchmark mode, thread control for Arrow compute, or writing a single dataset's specification as a document are discoverable only from `?` help or the changelog.
- Confidence: verified-by-reading (grep over the five documents; the man/ pages themselves not reviewed)
- Suggested direction: add a short "Other exported helpers" table (or sections for benchmarking/threads and `write_dataset_metadata`/`write_file_specification`) to the vignette.

### F-d1-userdocs-24  [minor] [doc-missing]
- Where: DTA_YAML_GUIDE.md (absent; sections 3-5), README.md:1135,1146-1155 (column `pattern`), vignettes/DTAtools.Rmd:2705-2750; present only in inst/extdata/template-repo-skeleton/README.md:393-418 and vignettes/private-templates.Rmd:607-627
- What: The `<<yyyyMMdd>>`-style date masks (usable in any specification's file-name `pattern`, a column `pattern:` and a rule `pattern` condition) are documented only for template authors; the YAML guide, which says "everything you need is here", the README column/rule reference and the vignette's YAML reference never mention them, although the guide's own complete example spells a date by hand (`"^[0-9]{4}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$"`). Where the masks are described elsewhere the wording is looser than the code: the `DTAColumnSpec` roxygen and CHANGELOG say they match "only an in-range date or time", but R/datePatternTokens.R:11-12 says `dd`/`d` is "not calendar-aware (`02-30` passes) and `yyyy` is 2000-2099 by design".
- Evidence: `grep -n '<<yyyy' README.md DTA_YAML_GUIDE.md vignettes/DTAtools.Rmd` prints nothing; R/DTAColumnSpec-class.R:121-127 `It may contain a date mask such as <<yyyy-MM-dd>>, matching only an in-range date or time written that way`; the skeleton README (line 414) states the real limits (`<<yyyy-MM-dd>>` accepts `2024-02-30`).
- Failure scenario: An author following the YAML guide hand-writes a date regex, or, reading the CHANGELOG/roxygen, assumes a mask rejects `2024-02-30` or accepts the year 1999 -- neither is true.
- Confidence: verified-by-reading
- Suggested direction: add a short "Date masks in patterns" subsection (with the 2000-2099 and not-calendar-aware caveats) to the YAML guide and README, and correct "in-range" wording in the roxygen and CHANGELOG.

### F-d1-userdocs-25  [nit] [doc-wrong]
- Where: CHANGELOG.md:95 and CHANGELOG.md:118
- What: The `[Unreleased]` section has two separate `### Changed` headings (the second holds a single entry), instead of one.
- Evidence: `### Changed` at line 95 (followed by the `get_arrow_type()` entries) and again at line 118 (followed by the Delivery-zone entry).
- Failure scenario: Release tooling that promotes `[Unreleased]` or merges sections by heading treats them as two groups; the entry order reads as two unrelated "Changed" lists.
- Confidence: verified-by-reading
- Suggested direction: merge the Delivery-zone entry into the first `### Changed` list.

### F-d1-userdocs-26  [major] [doc-wrong]
- Where: docs/index.html:756-758,783-784 (and 732,687)
- What: The tutorial page documents an `ok` column in the output of `results()` and recommends `if (results(dta)$ok)` for programmatic checks; `results()` has no `ok` column (that column belongs to `validation_status()`).
- Evidence: docs/index.html:756-758 `ok` / `Boolean pass/fail — use this for programmatic checks (e.g. if (results(dta)$ok))` / `TRUE / FALSE`; docs/index.html:783 `A dataset with n_import_errors = 1 will always have ok = FALSE (status: "failed")`. The data frame built by `dta_results_from_status()` (R/validationReporting.R:70-92) has exactly: dataset, target, target_type, status, validated_at, run_id, validation_run, n_columnspec_errors, n_rule_errors, n_import_errors, n_targets, n_validated, n_valid, n_invalid, n_skipped, n_not_validated -- no `ok`; the vignette (DTAtools.Rmd:651-655) lists the same columns correctly.
- Failure scenario: A user copies `if (results(dta)$ok) ...`: `results(dta)$ok` is `NULL`, so the `if` aborts with "argument is of length zero" (and a pipeline gate built on it never runs its pass branch).
- Confidence: verified-by-reading
- Suggested direction: document `status == "validated"` / `n_invalid == 0` for results() (or point to `validation_status(ds)$ok`), and drop `ok` from the results() table; also say "one row per table (target)", not "per validated dataset" (index.html:732, 687).

### F-d1-userdocs-27  [minor] [doc-wrong]
- Where: docs/index.html:1672-1674 (vs R/00_helpers.R:232,482; README.md:581; vignettes/DTAtools.Rmd:1711-1714)
- What: The "Which one should you use?" table tells readers to pass `stream = "always"` explicitly for a large `.csv.gz` because `"auto"` "under-triggers", but `"auto"` already multiplies a `.gz` file's size on disk by an expansion estimate (4x), which the README and vignette state correctly.
- Evidence: docs/index.html:1672-1674 `A large compressed file (.csv.gz)` / `"always", explicitly` / `"auto" compares the size on disk, which is far smaller than the materialised size — so it under-triggers here.`; R/00_helpers.R:232 ``__DTAtools_gz_expansion_ratio__` <- 4`` and :482 `size <- size * ...` inside the auto decision; README.md:581 `"auto" handles it | The size test multiplies a .gz's on-disk bytes by an expansion estimate (4×)`.
- Failure scenario: A reader follows the page and forces `"always"` for every compressed file (losing the ~2x speed of in-memory checking for small ones) and distrusts a documented default that works.
- Confidence: verified-by-reading
- Suggested direction: align the row with README/vignette ("auto" handles it: a `.gz`'s on-disk size is multiplied by 4).

### F-d1-userdocs-28  [minor] [doc-wrong]
- Where: docs/index.html:1110-1112 (vs R/validationFunctions.R:11-14; vignettes/DTAtools.Rmd:617)
- What: The page says rules "run after all column spec checks have passed", i.e. only for tables whose column specs pass, but all three axes (column spec, rule, import) are always evaluated and reported in one pass.
- Evidence: docs/index.html:1111-1112 `They are defined under a rules: key inside your dataset spec and run after all column spec checks have passed.`; R/validationFunctions.R roxygen `All three axes -- the column specs, the rules and the import typing -- are always evaluated, and all are reported in a single pass`; vignette `All three are always evaluated, and a failure on any one of them fails the table.`
- Failure scenario: A reader expects rule errors to appear only after the column errors are fixed (and may read an all-clear on the rule axis into a failing table), while `n_rule_errors` is reported from the first run.
- Confidence: verified-by-reading
- Suggested direction: replace with "rules are checked in the same pass as the column specs".

### F-d1-userdocs-29  [minor] [inconsistency]
- Where: docs/index.html:900, 1000, 1030, 575-582 (nav) and 1573-1609
- What: Several stale or dangling details on the tutorial page: (a) "See Section 4.1 for an example" refers to numbered sections, but no heading shows a number (the h3 ids `section-4-1` are internal); (b) the Column Properties table still describes `type` as "SAS type (see above)" and `format` as "SAS format" although the same page documents `R` types and `R` formats; (c) section 4.7 "group_condition Rule (Advanced)" (id `section-4-7`) is missing from the sidebar navigation (the nav jumps from `section-4-6b` to `section-4-8`) and repeats the group_condition example of section 3.5.
- Evidence: docs/index.html:900 `See Section 4.1 for an example.`; :1000 `SAS type (see above)`, :1030 `SAS format; values that do not fit it are reported`; sidebar links at :575-582 list section-4-1 ... 4-6, 4-6b, 4-8 only; `id="section-4-7"` exists at :1573.
- Failure scenario: Readers cannot find "Section 4.1"; the properties table tells them types are SAS-only; one section is unreachable from the navigation.
- Confidence: verified-by-reading
- Suggested direction: number the headings or link "Validating a Single Dataset"; say "type (SAS or R system, see above)"/"format (SAS or R)"; add the nav item or delete the duplicate section.

### F-d1-userdocs-30  [minor] [doc-wrong]
- Where: R/createTemplateRepo.R:246-250; README.md:1064-1068; inst/extdata/ci/validate-templates.yml:66, bitbucket-pipelines.yml:55, Jenkinsfile:73 (vs vignettes/private-templates.Rmd:1580-1584)
- What: The scaffold's CI files are described as "ready-to-run" (README, `create_template_repo()` roxygen), but every provider file contains a deliberately failing step, so the written workflow fails until the user edits it; the private-templates vignette (correctly) calls it "ready-to-copy ... fill in the one placeholder step".
- Evidence: createTemplateRepo.R:248 `Each provider writes one ready-to-run file that lints the repository with lint_template_repo(".")`; README.md:1066-1068 `... and a ready-to-run validation CI workflow, not a blank directory to fill in from nothing`; inst/extdata/ci/validate-templates.yml `- name: Install DTAtools (placeholder -- replace with your internal source)` / `run: Rscript -e 'stop("Replace this step with an install of DTAtools from your internal package repository.")'` (the same `stop(...)` at bitbucket-pipelines.yml:55 and Jenkinsfile:73).
- Failure scenario: A user runs `create_template_repo("x")`, pushes, and the first CI run aborts at the install step, contradicting "ready-to-run"; a reader of the roxygen help never learns that an edit is required.
- Confidence: verified-by-reading (all three CI files grepped for the stop() line)
- Suggested direction: say "ready to copy; one install step must be filled in" in the README and the `ci` parameter doc.

### F-d1-userdocs-31  [minor] [doc-wrong]
- Where: vignettes/DTAtools-manuscript.Rmd:1-40,188-243,294-320,355-359 (listed in the package vignette index)
- What: An unfinished manuscript draft ("a working document, not a submission", with `[TO BE FILLED IN ...]` placeholders, an empty benchmark `files` vector and a `sessionInfo()` chunk) is built by `R CMD check`, installed and indexed as a user vignette ("Manuscript draft: DTAtools (Nature Methods format)").
- Evidence: DESCRIPTION `VignetteBuilder: knitr`; the file's front matter `%\VignetteIndexEntry{Manuscript draft: DTAtools (Nature Methods format)}`; `.Rbuildignore` excludes `^docs$`, `^DTA_YAML_GUIDE.md$`, `^thoughts$` but not this vignette; body text `**[TO BE FILLED IN — benchmark results.]**`, `**[TO BE FILLED IN — conclusion depends on the benchmark outcome.]**`, `**[To be completed and confirmed by all authors.]**`; line 326-327 `All analyses in this manuscript are contained in the package vignettes and run with R CMD check` although the benchmark chunk is `eval = FALSE`.
- Failure scenario: Users calling `browseVignettes("DTAtools")` see an unfinished paper draft next to the real documentation, with `sessionInfo()` output baked in at build time and claims ("analyses ... run with R CMD check") the draft itself contradicts.
- Confidence: verified-by-reading
- Suggested direction: move the draft out of `vignettes/` (e.g. to `thoughts/`/`docs/`) or exclude it via `.Rbuildignore` until it is complete.

### F-d1-userdocs-32  [minor] [doc-wrong]
- Where: vignettes/private-templates.Rmd:151-152,1584-1586; inst/extdata/library/biomarker_gf_metadata_overview.md (contacts example)
- What: (a) The public vignette states facts about the maintainer's private deployment ("Bitbucket Data Center (production, in this deployment's case)"; "If your Bitbucket is Server or Data Center (as this project's own production deployment is — see "Deploying on Posit Connect" above)"), which no reader of the open-source documentation can interpret. (b) The GF metadata overview shows a contact `backup: "..."` as a string although a contact's `backup` is a boolean flag (`backup: true`) everywhere else (clinical_dta.yaml, the YAML guide) and in the code (`isTRUE(contact$backup)`).
- Evidence: private-templates.Rmd:151 `Both **Bitbucket Data Center** (production, in this deployment's case) and **GitHub** (used to mirror the same repository for CI)`; biomarker_gf_metadata_overview.md `metadata.receiver.contacts[] | ... [{name: "Max Mustermann", role: "TDM", phone: "+49...", email: "max@example.com", backup: "..."}]`; R/DTAMetaData-class.R:584 `if (isTRUE(contact$backup)) flags <- c(flags, "backup")`; DTA_YAML_GUIDE.md:76 `backup: false                  # deputy contact`.
- Failure scenario: (a) A reader cannot tell which "deployment"/"project" is meant; (b) `backup: "..."` is silently treated as FALSE (isTRUE of a string), so the deputy flag is lost.
- Confidence: verified-by-reading for the texts and the isTRUE test; inferred that `backup` is not further interpreted as a string elsewhere
- Suggested direction: rewrite (a) neutrally ("for example"); use `backup: true` in (b).

### F-d1-userdocs-33  [nit] [dead-code]
- Where: vignettes/private-templates.Rmd:402-436; inst/extdata/template-repo-skeleton/README.md:1871-1905 ("Reorganising an older repository"), :113-116; README.md:1091-1093; inst/extdata/agent-skills/dta-template-authoring/references/linting.md:20-70 ("Older DTAtools")
- What: Several documents carry migration/fallback guidance for layouts and tool versions that are already retired ("Libraries written for DTAtools 0.25.0 and earlier are flat ... That layout is no longer read", the "mixed `templates/` layout of an unreleased development build", an "Older DTAtools" fallback for installs without `lint_template_repo()`); pre-1.0 these describe removed features.
- Evidence: private-templates.Rmd:404-408 `Libraries written for DTAtools 0.25.0 and earlier are flat: every file sits at the repository root. That layout is no longer read, and neither is the mixed layout of an unreleased development build`; linting.md:30-35 `If the installed DTAtools has no lint_template_repo(), use the fallback ...`.
- Failure scenario: Stale migration instructions (git mv recipes, version cut-offs) must be kept in step with every later release and are read by authors who have no such old repository.
- Confidence: verified-by-reading; whether the owner wants the guidance kept is a policy call
- Suggested direction: drop the migration/fallback sections (the CHANGELOG already records the breaking change).

### F-d1-userdocs-34  [nit] [dead-code]
- Where: R/exportDocuments.R:1668-1686 (`write_file_specification()`), NAMESPACE:130
- What: `write_file_specification(x, file, ...)` is an exported, one-line alias of `write_dataset_metadata()` ("Convenience alias"); no user document mentions either name, so the package exports two names for one function and documents neither outside the help pages.
- Evidence: `#' @title Alias: Export File Dataset Specifications ... Convenience alias for write_dataset_metadata()` / `write_file_specification <- function(x, file, ...) { write_dataset_metadata(x, file, ...) }`; `grep -c` of both names in README.md, DTAtools.Rmd, DTA_YAML_GUIDE.md, private-templates.Rmd and docs/index.html is 0.
- Failure scenario: Users find two functions with identical behaviour and no guidance on which to use; the alias must be kept in step with every change to `write_dataset_metadata()`'s argument list (its roxygen already has to enumerate them in `...`).
- Confidence: verified-by-reading
- Suggested direction: drop the alias (pre-1.0), and document `write_dataset_metadata()` in the vignette's export chapter.

### F-d1-userdocs-35  [major] [doc-missing]
- Where: README.md:61-67; vignettes/DTAtools.Rmd:59-69; docs/index.html:646-661 (installation instructions) vs DESCRIPTION:45 and .github/workflows/{R-CMD-check,qualification,release,r-style,test-coverage}.y*ml
- What: All three installation instructions are a plain `remotes::install_github("Boehringer-Ingelheim/DTAtoolsR")`, while the repository's own CI installs `cran::S7@0.2.2` explicitly in five workflows and DESCRIPTION only requires `S7 (>= 0.2.2)`; the documents never mention that a newer S7 may not work.
- Evidence: .github/workflows/R-CMD-check.yaml:135 `extra-packages: any::rcmdcheck,any::remotes,any::devtools,cran::S7@0.2.2` (also qualification.yml:94, release.yml:33, r-style.yaml:67, test-coverage.yaml:79,166); DESCRIPTION:45 `S7 (>= 0.2.2)`; R/ builds objects with `new_object(.parent = ...)` at 10 sites (`grep -c '\.parent = ' R/*.R`); README.md:66 `remotes::install_github("Boehringer-Ingelheim/DTAtoolsR")`.
- Failure scenario: A user following the README on a fresh library gets the newest S7 from CRAN; if that release changed `new_object(.parent=)` (the reason the project pins S7 in CI -- not verified here), class construction fails on `library(DTAtools)`/first call, and no document says to install S7 0.2.2.
- Confidence: inferred (the incompatibility of newer S7 releases was not run or read here; what is verified is the CI pin, the DESCRIPTION lower bound and the absence of any note in the install docs)
- Suggested direction: either make the code work with current S7 or state the supported S7 range in DESCRIPTION and in the install instructions (e.g. `install.packages("S7", ...)` pin) until it does.

### F-d1-userdocs-36  [nit] [doc-wrong]
- Where: DTA_YAML_GUIDE.md:3
- What: The guide dates itself by branch and month ("DTAtools, `dev` branch, Nov 2026") instead of the package version, so a reader cannot tell which release it describes once `dev` moves on.
- Evidence: DTA_YAML_GUIDE.md:3 `Self-contained authoring reference (DTAtools, `dev` branch, Nov 2026).`; DESCRIPTION `Version: 0.28.0`; the guide documents unreleased features (R type system, `[Unreleased]` in CHANGELOG.md:30-93) alongside released ones.
- Failure scenario: An author (or an LLM agent given this file, which is its stated use) cannot know whether a key it reads exists in the installed version.
- Confidence: verified-by-reading
- Suggested direction: state the version the guide matches (and which keys need >= 0.29.0 / the unreleased build).

### F-d1-userdocs-37  [nit] [doc-wrong]
- Where: CHANGELOG.md:840-2099 (`## [0.26.0]`), CHANGELOG.md:4246-4291 (`## [0.17.3]`)
- What: Several releases repeat the same `###` subsection heading instead of merging the entries: 0.26.0 has `### Changed` three times (940, 1231, 1568), `### Added` twice (842, 1299) and `### Fixed` twice (1038, 1752); 0.17.3 has `### Added` twice (4248, 4286). (The duplicate `### Changed` of `[Unreleased]` at 95/118 and the duplicated 0.18.1 block are reported separately.)
- Evidence: heading line numbers from `awk '/^## \[/{rel=$0} /^### /{k=rel" | "$0; c[k]++; if (c[k]==2) print ...}'` and `awk 'NR>=840 && NR<=2099 && /^### /'` printing Added 842, Changed 940, Fixed 1038, Changed 1231, Added 1299, Changed 1568, Removed 1741, Fixed 1752, Security 2014.
- Failure scenario: The 1260-line 0.26.0 section reads as several accumulated release notes pasted together; tools that map `### Added/Changed/Fixed` to GitHub release notes get repeated groups.
- Confidence: verified-by-reading
- Suggested direction: merge each release's entries under a single heading per category.

### F-d1-userdocs-38  [nit] [dead-code]
- Where: vignettes/DTAtools_classes.png, img/DTAtools_classes.drawio; duplicates: img/dtatools_logo.png = vignettes/dtatools_logo.png, img/dtatools_logo_small.png = docs/dtatools_logo_small.png, img/DTAtools_architecture.svg = vignettes/DTAtools_architecture.svg
- What: The class diagram `DTAtools_classes.png` (74 KB, shipped in the package build because it sits in `vignettes/`) and its `.drawio` source are referenced by no document; the logo and the architecture SVG exist as byte-identical copies in two or three directories that must be kept in step by hand.
- Evidence: `grep -rIl 'DTAtools_classes' --exclude-dir=renv --exclude-dir=thoughts .` prints nothing; `cmp img/dtatools_logo.png vignettes/dtatools_logo.png`, `cmp img/dtatools_logo_small.png docs/dtatools_logo_small.png` and `cmp img/DTAtools_architecture.svg vignettes/DTAtools_architecture.svg` all report identical files (the SVG copies already omit the same two nodes, see the architecture finding).
- Failure scenario: A future edit of the diagram (e.g. adding the missing `DTARuleGroupCondition`/`DTAContainer` nodes) updates one copy and leaves the others stale; the unused class diagram is never shown to readers.
- Confidence: verified-by-reading
- Suggested direction: embed the class diagram in the vignette (or delete it) and keep one source for each asset.

### F-d1-userdocs-39  [nit] [doc-wrong]
- Where: vignettes/DTAtools.Rmd:1356-1357; docs/index.html:1445-1448
- What: Two small example/prose slips: (a) the container chapter says `last_delivery` records what became of every file: "loaded, opened ..., unused, ignored or refused", omitting the sixth status `ambiguous` that the same chapter describes 220 lines later (and that README.md:412 and R/loadDelivery.R:125-127 list); (b) the tutorial page's "Attach rules to specs" snippet writes `columns = list(...)` as a literal placeholder, which errors when copied (`'...' used in an incorrect context`).
- Evidence: DTAtools.Rmd:1356-1357 `The last_delivery attribute records what became of every file: loaded, opened (a container that was expanded), unused, ignored or refused.`; R/loadDelivery.R:125-127 `status ("loaded", "opened", "unused", "ignored", "refused" or "ambiguous")`; index.html:1445-1447 `specs_with_rules <- DTAColumnSpecCollection(columns = list(...), rules = list(age_rule, unique_rule, condition_rule))`.
- Failure scenario: A reader filtering `last_delivery` by the listed statuses misses ambiguous files; a reader pasting the snippet gets an error instead of a collection.
- Confidence: verified-by-reading
- Suggested direction: add "ambiguous" to the list; replace `list(...)` by `list(PATIENT_ID = col_id, AGE = col_age, VISIT = col_visit)`.

---
## Checked and found correct (depth reached)

- Every function name called in README, the YAML guide, both main vignettes, the manuscript, CHANGELOG, index.html, the skeleton README and the agent-skill files resolves to a definition in R/ or inst/shiny (mechanical call-token pass; the only unmatched tokens were base/other-package functions and local helpers such as `lab()`/`accepted()` that the vignette chunks define themselves).
- Every named argument in every R/YAML example chunk of those documents is a formal of the called exported function (perl pass with a formals parser; control run with five deliberately wrong calls detected all of them). Functions with `...` (e.g. `load_file`) cannot be checked this way.
- Every `system.file("extdata", ...)` / `system.file("qualification", "fx", "container", ...)` target named in the docs exists under inst/; intra-document `#anchor` links resolve in README, vignettes, skeleton README, skill files and index.html.
- Option names (`DTAtools.*`), environment variables (`DTATOOLS_*`), condition classes (`dta_*`) and lint/validator codes mentioned in the user documents exist in the code; all 53 codes in the skeleton README "Validator codes" table are emitted by validate_template(); the repository-lint codes match lintTemplateRepo.R; defaults checked: stream threshold 512 MB, gz expansion 4x, batch rows 131072, `DTAtools.container_max_*` (10000 / 1e13 / 1e13 / 1100), app limits 1e10/2e9, upload 1 GB, `DTATOOLS_SESSION_DELIVERY_BYTES` 2e10, template refresh 900 s, git user "git"/auth "basic", header tag 32 chars, logo 1 MB, extends depth 8.
- YAML key sets verified against the loaders: metadata (closed key set, `title`/`version` required by `dta_from_list`), `containers:` (`id`, `filename`, `description`, `default`; id grammar; `none` reserved), file-handler keys per type (csv/tsv/sas7bdat/xpt/any), matrix blocks (`matrix.columns/rows/cells`, `row_labels.header`), group_condition constraint types/scopes, col_condition operator list, R type system and `%` directive grammar, `dta-app.yaml` key table.
- Bundled example data claims: clinical_dta.yaml (dataset id, rule ids, columns), clinical_data_error_columnspec.csv (missing HEIGHT, "Alien", "heavy", "9999-9999", over-long VISIT), matrix_dta.yaml / matrix_gene_counts.tsv / matrix_mature_counts.csv (vignette's edit of `S0003` and `\t0\t1.5`), biomarker_gf template header/dataset-template version.
- CHANGELOG `[Unreleased]` and `[0.28.0]`: 44 function-like and every `dta_*`/`DTAtools.*` token resolve in code/tests; `last_delivery` has the stated 12 columns; heading dates decrease monotonically; version in DESCRIPTION, inst/shiny/dta_app/VERSION and docs/index.html (badge and footer) is 0.28.0.
- App UI labels cited by docs: "+ Add dataset" / Matrix / "ID of the first column (the row label)", "Row labels", "Matrix layout", "Delivered inside", "Containers…", "Check all", "Don't load", the "Report" download beside CSV/TSV/XLSX, Raw YAML "Copy".
- Doubled-word and common-typo scans over all markdown/Rmd documents found nothing (index.html hits were CSS class names).

## Coverage

Fully read (every line): README.md (1-1699), DTA_YAML_GUIDE.md (1-1148), vignettes/DTAtools.Rmd (1-3109), vignettes/DTAtools-manuscript.Rmd (1-359), docs/index.html (all visible text, lines 536-2030; CSS/JS head not reviewed beyond title/version), docs/404.html, docs/_config.yml, dts_metadata_coverage_overview_2026-07-28.md, inst/extdata/agent-skills/dta-template-authoring/SKILL.md, inst/extdata/clinical_dta.yaml, inst/extdata/matrix_dta.yaml, inst/extdata/ci/validate-templates.yml and bitbucket-pipelines.yml, inst/extdata/library/biomarker_gf_metadata_overview.md, inst/extdata/template-repo-skeleton/export-templates/README.md, CHANGELOG.md lines 1-548 (header, `[Unreleased]`, `[0.28.0]`) and the link-reference tail.

Partially read: vignettes/private-templates.Rmd (lines 1-1130 and 1512-1598 read; 1131-1511 -- versioning, parties, controlled vocabularies, provenance -- only grepped for cross-references, legacy/version statements and numbers); inst/extdata/template-repo-skeleton/README.md (lines 1-300, 393-420, 1330-1545, 1555-1715 read; the remaining ~900 lines checked only mechanically: validator-code table vs code, anchors, quoted cross-references, limits, function names); agent-skills references/linting.md (lines 20-75, 120-210), composition.md and repository-architecture.md (mechanical checks only: function names, codes, depth limit); CHANGELOG.md lines 549-5515 (structure, duplicate headings, link references, dates, identifier existence for 0.26.0-0.27.0 only).

Not read / not verified: the example YAMLs clinical_dta_multiple_files.yaml, clinical_dta_with_file_dataset.yaml and gf_dataset.yaml and the library YAML templates (only comments and `files:` blocks looked at; they are loaded by tests); inst/extdata/ci/Jenkinsfile (grepped only); CHANGELOG entries of releases 0.25.0 and older (not checked for content accuracy, by the brief's carve-out); YAML snippets could not be parsed mechanically (no YAML parser available without running R) -- they were traced by reading against the loaders; external URLs were not fetched; hand-written `#>` outputs were verified by reading the producing code only for README's `validation_status` example.

Cross-slice notes: R/DTADataSetFile-class.R (compat coercion), R/exportTemplateBlocks.R (legacy alias), R/DTARuleColRange-class.R / DTARuleGroupCondition-class.R (aliases), R/DTADataSet-class.R (`recursive` parameter), R/containerAssignments.R + R/validationFunctions.R (older-version upgrade/migration) were reported as dead-code from the documentation side only.
