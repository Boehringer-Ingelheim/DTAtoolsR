# Linting and verifying a template library

## Contents

- [Run the linter](#run-the-linter)
- [Read the output](#read-the-output)
- [Triage order](#triage-order)
- [Codes and fixes](#codes-and-fixes)
- [Beyond the linter](#beyond-the-linter)
- [Before you open a pull request](#before-you-open-a-pull-request)

## Run the linter

From the repository root (the folder holding `templates/`, `parties/`,
`vocabulary/`):

```sh
Rscript -e "DTAtools::lint_template_repo('.', fail_on = 'warning')"
```

- It runs every check of `validate_template()`, including a real dry-run build
  of each concrete creation template (every `variants:` choice, every frozen
  version), plus checks of the repository layout and file names.
- It prints one line per finding, then a count, and exits with status 1 when a
  finding reaches `fail_on`. CI fails on errors only (the default
  `fail_on = 'error'`, or `validate_template('.', strict = TRUE)` in an older
  scaffold); run your own passes with `'warning'` so nothing slips through
  unseen.
- To work with the findings in R:

  ```r
  f <- DTAtools::lint_template_repo(".", fail_on = "none")
  f[f$severity == "error", c("file", "code", "message")]
  ```

  The data frame has `file`, `kind`, `id`, `version`, `severity`, `code`,
  `message`.
- Never point it at `templates/` or another kind folder
  (`library_subfolder_given`); always at the root.

**Older DTAtools.** Check first:

```sh
Rscript -e "cat(exists('lint_template_repo', envir = asNamespace('DTAtools')))"
```

If that prints `FALSE`, run `Rscript -e "DTAtools::validate_template('.', strict = TRUE)"`
instead (or `print(DTAtools::validate_template('.'))` for the full table). It
runs the template checks and the dry-run build, but a DTAtools without the
linter reports none of the six codes below, so check them by eye:

| Code | Check by eye |
| --- | --- |
| `folder_name_case` | the kind folders are spelled exactly `templates`, `parties`, `vocabulary`, `export-templates` |
| `id_filename_mismatch` | every file name starts with its `id` |
| `file_ignored` | every template YAML carries its kind's suffix |
| `kind_suffix_mismatch` | each suffix names the same kind as `kind:` |
| `party_profile_invalid` | each profile's `role` is `supplier`, `receiver` or `any`, with `affiliation` a mapping and `contacts` a list |
| `export_template_no_placeholders` | each `.docx` in `export-templates/` contains `{PLACEHOLDER}` markers |

DTAtools 0.26.0 and earlier also misread slots a child writes: a required
party slot is always `instantiate_failed`; a `party_slots:` or
`vocabulary_slots:` written with verbs prints "Dropping 1 … slot with a
missing or empty id." and goes unchecked; and a plain-list entry that changes
only some fields of an inherited slot is reported as having an invalid target
`""`. Treat those findings as the validator's, not the template's, and check
the slots in the template guide instead.

Lint with the DTAtools version the deployment runs; check it with
`Rscript -e "cat(format(packageVersion('DTAtools')))"`.

## Read the output

- The `file` column is the template the finding was reported **for**, not
  necessarily the file to fix. One broken mixin or parent is reported once on
  every concrete template that uses it. Read the message: it names the
  template and key at fault (for example "mixes in "mixin_privacy_eu@1.0",
  which is not a valid mixin: it must not contain sealed").
- Read the message, not just the code. `extends_unresolved` covers both a
  parent that cannot be found and a collection verb naming an entry that is
  not inherited ("Cannot modify "headr" in options").
- `instantiate_failed` accompanies most problems of a concrete template. It
  carries the build's own error text, which is often the most precise
  description of the fault.
- Validation sees only this repository. A reference to a template that lives
  in another app source is reported as unresolved and never dry-run.

## Triage order

Fix in this order and re-lint after each group; later findings often
disappear with earlier ones.

1. **Unreadable files**: `parse_failed`, `kind_unknown`,
   `kind_suffix_mismatch`, `version_unquoted`, `version_unparseable`,
   `id_missing`, `version_missing`, `dataset_missing`, `dataset_id_missing`,
   `dataset_field_invalid`, `vocabulary_invalid`, `party_profile_invalid`,
   `app_config_invalid`.
2. **Files the app does not see**: `no_templates`, `library_subfolder_given`,
   `folder_name_case`, `template_misplaced`, `template_in_subdirectory`,
   `file_ignored`, `id_filename_mismatch`, `duplicate_id_version`,
   `app_config_misplaced`, `app_config_duplicate`, `export_template_invalid`,
   `export_template_no_placeholders`.
3. **References**: `extends_unresolved`, `mixin_unresolved`,
   `dataset_template_unresolved`, `values_from_unresolved`,
   `vocab_slot_unresolved`, `vocabulary_unresolved`, `party_slot_invalid`.
4. **Composition**: `extends_cycle`, `mixin_invalid`, `mixin_conflict`,
   `sealed_violation`, `sealed_path_unknown`, `variant_invalid`,
   `patch_incoherent`, `values_from_invalid`, `values_from_pattern`,
   `values_and_values_from`, `values_from_terms_invalid`, `vocab_slot_invalid`,
   `vocabulary_extends_failed`, `target_invalid`, `target_machine_owned`,
   `prior_versions_invalid`, `app_config_unknown_key`, `app_config_warning`.
5. **The build**: `instantiate_failed`, and the warnings of `lint_dta()` that
   the dry-run passes through for a DTA that did build
   (`format_narrower_than_length`, `rule_not_checkable`, …;
   `?DTAtools::lint_dta` lists them).

## Codes and fixes

Severity is what the linter reports. "May stay" marks the only warnings that
can be legitimate in a finished change; say why in your report when one does.

### Reading and identity

| Code | Sev | Fix |
| --- | --- | --- |
| `parse_failed` | error | Invalid YAML: indentation, an unclosed quote, a tab. Fix the syntax. |
| `kind_unknown` | warning | `kind:` must be `dta_creation_template`, `dta_dataset_template`, `dta_party_profile` or `dta_vocabulary`. |
| `kind_suffix_mismatch` | error | The file name's suffix and `kind:` disagree; the app reads the kind from the name. Rename the file or correct `kind:`. |
| `id_missing`, `version_missing` | warning | Add a non-empty `id` / a quoted `version`. |
| `version_unquoted` | error | Quote it: `version: "1.10"`. Check the intended text; YAML may already have turned `1.10` into `1.1`. |
| `version_unparseable` | warning | Use digits and dots: `"1.0"`. Such a version is never chosen as latest. |
| `duplicate_id_version` | warning | Two files with one kind, id and version. Usually a frozen copy whose head was not bumped: bump the head. |
| `party_profile_invalid` | error | The app's reader rejects the profile (for example an unknown `role:`). Fix the field the message names. |
| `vocabulary_invalid` | error | Header, `type`, `terms` or `columns:` malformed, or no usable terms. |
| `dataset_missing` | error | A dataset template needs a singular `dataset:` body. |
| `dataset_id_missing` | error | The dataset body needs an `id`, even when every import renames it with `as:`. |
| `dataset_field_invalid` | error | A dataset's `id`, `name` or `description` must be a single text value. |

### Layout and naming

| Code | Sev | Fix |
| --- | --- | --- |
| `no_templates` | error | Wrong path, or files one level too deep. Lint the repository root. |
| `library_subfolder_given` | error | You linted a kind folder. Lint its parent. |
| `folder_name_case` | error | Rename to the exact lowercase name. On Windows or macOS rename in two steps through a temporary name (`git mv Vocabulary tmp && git mv tmp vocabulary`). |
| `template_misplaced` | error | Move the file to the folder of its kind (creation and dataset templates → `templates/`, profiles → `parties/`, vocabularies → `vocabulary/`). |
| `template_in_subdirectory` | warning | Move it up into its folder; a sub-folder is never loaded. If it is an old version kept "for the record", it belongs beside the head as a frozen file. |
| `file_ignored` | warning | A YAML file declares a template kind but lacks the suffix, so the app never loads it. Rename it to `<id>.<suffix>`. |
| `id_filename_mismatch` | warning | Rename the file to `<id>.<suffix>` (head) or `<id>@<version>.<suffix>` (frozen). Never change the id to fit the file name. |
| `app_config_invalid` | error | A value in `dta-app.yaml` the app would reject (bad colour, link, format, or a missing/oversized logo or `.docx`). Fix the named key. |
| `app_config_unknown_key` | warning | A misspelled key (`colours:`). Fix it. |
| `app_config_warning` | warning | A value the app adjusts (`create_blank: true` with `editing: false`). Make the file say what the app does. |
| `app_config_duplicate`, `app_config_misplaced` | error | Exactly one `dta-app.yaml`, at the root. |
| `export_template_invalid` | warning | A non-`.docx` file in `export-templates/`. Remove or convert it. |
| `export_template_no_placeholders` | warning | A `.docx` without `{PLACEHOLDER}` markers is not offered and inserts nothing. Add markers (`DTAtools::dta_template_placeholders()` lists them) or remove it. **May stay** only if `dta-app.yaml` lists it deliberately. |

### References

| Code | Sev | Fix |
| --- | --- | --- |
| `extends_unresolved` | warning | Parent id or version not found: check spelling, kind and the exact version text. Also used for a collection verb naming an entry that is not inherited: fix the id in `modify:`/`remove:`, or use `add:`. |
| `mixin_unresolved` | warning | Mixin not found; same checks. |
| `dataset_template_unresolved` | warning | A `datasets[].template` reference does not name a dataset template. |
| `values_from_unresolved`, `vocab_slot_unresolved`, `vocabulary_unresolved` | warning | The vocabulary is missing or the reference is misspelled. |
| `party_slot_invalid` | warning | Slot target must be `metadata.supplier` or `metadata.receiver`; every id in `profiles:` must be an existing profile id, written without `@version`. Reported on the file that wrote the slot or the modification. |

These are warnings because a reference may resolve in another app source. In
the one-library layout of repository-architecture.md, treat every one of them
as an error; none may stay.

### Composition

| Code | Sev | Fix |
| --- | --- | --- |
| `extends_cycle` | warning | The chain loops or exceeds 8 levels. Break the loop; flatten the chain. |
| `mixin_invalid` | error | The mixin must be `abstract: true`, have `base:`, use only `base`/`options`/`datasets`/`party_slots`/`vocabulary_slots`, bare lists, no `remove:`, no `null`; or `mixins:` itself is not a list of references. |
| `mixin_conflict` | error | A key or id is contributed twice (chain and mixin, or two mixins), or an ancestor already mixes this mixin in. Give the leaf a single owner, or write the value in the consuming template itself; prefix mixin ids by topic. |
| `sealed_violation` | warning | A descendant changes a sealed path. Remove the override. Unsealing is a governance decision: ask. |
| `sealed_path_unknown` | warning | The seal protects nothing. Usually a missing `base.` prefix or a renamed id. |
| `variant_invalid` | error | `variants:` needs `id`, non-empty `choices` with unique `value`s, `datasets:` in every choice, and an `id` that is not also an option id. |
| `patch_incoherent` | warning | The patch does not fit the imported dataset template: a removed or modified column id that does not exist, an added id that already does, a `set` path outside the dataset body. Check against the **pinned** version. |
| `values_from_invalid` | error | Malformed binding: a reference, or a mapping with `vocabulary`, `field` (`code`/`label`), `include`, `exclude`. |
| `values_from_pattern` | error | A column cannot have both `values_from` and `pattern`. |
| `values_and_values_from` | warning | Delete the literal `values:`; the binding wins anyway. |
| `values_from_terms_invalid` | error | `include:`/`exclude:` name a code the vocabulary does not have (check case and quoting), or a term was removed upstream (see the recipe in repository-architecture.md). |
| `vocab_slot_invalid` | error | Target must be `datasets.<as>.columns.<column id>.values` of a dataset the template builds; `mode` `closed`/`open`; `include`/`exclude`/`default` codes must exist; `min` a whole number. |
| `vocabulary_extends_failed` | error | Vocabulary inheritance failed: a cycle, or `remove_terms` naming a code the parent does not have. |
| `target_invalid` | warning | An option `target` or effect `path` is not a real field. Use a supported metadata field (README "base: metadata") or a `dataset.` path that resolves. |
| `target_machine_owned` | error | Never target `metadata.template` or `metadata.import_issues`. Remove it. |
| `prior_versions_invalid` | error | Each entry needs `version` and `changes`, an optional `YYYY-MM-DD` `date`, no repeats, and must be older than the first library version. |

### The build

| Code | Sev | Fix |
| --- | --- | --- |
| `instantiate_failed` | error | The concrete template (or one `[choice]` of its variants) cannot be built. Fix any other finding for it first; then read the message. Frequent causes: a `required:` path nothing fills ("1 required field not filled … base.metadata.…": set it in this template or mix in a block that provides it); a sealed path changed; no datasets after inheritance; a patch that does not apply. If it reports a required party slot with no profile selected, no profile in the library can fill that slot: add one with the slot's role, or name an existing one in its `profiles:`. |
| `lint_dta()` codes | warning | The DTA built, but its columns and rules have a weakness (a format narrower than the length, a rule that cannot be checked). Fix the dataset template or the patch. The `lint_dta()` *errors* never appear as rows of their own: a DTA with such an error cannot be built, so they arrive inside an `instantiate_failed` message ("(rule_condition_value_outside_values) Rule 'x' … uses value(s) 'LOST' …", "(rule_column_undeclared) …"). |

## Beyond the linter

The linter proves that every template builds. It does not prove that a
template builds what was asked for. When a change alters what authors see or
what a DTA contains, inspect the result.

**Compare the template guide.** `write_template_guide()` renders, per concrete
template, the questions it asks, what it fills in, what is fixed, its datasets,
columns, rules, term lists, party profiles, and a "How it differs from
<parent>" section. Its text is a readable, diffable picture of the build.

Save this as a script in a temporary directory (not in the repository) and run
it with `Rscript compare-guides.R <base ref> <template id>` from the repository
root:

```r
args <- commandArgs(trailingOnly = TRUE)
base_ref <- args[[1]]
id <- args[[2]]

guide_text <- function(lib, dir) {
  f <- DTAtools::write_template_guide(lib, dir, templates = id, overwrite = TRUE, quiet = TRUE)
  s <- officer::docx_summary(officer::read_docx(f[[1]]))
  s$text[nzchar(s$text)]
}

before_lib <- file.path(tempdir(), "before")
dir.create(before_lib)
# A clean copy of the base ref, without touching the working tree.
archive <- file.path(tempdir(), "before.tar")
status <- system2("git", c("archive", "--format=tar", "-o", shQuote(archive), shQuote(base_ref)))
if (status != 0) stop("git archive failed for ", base_ref)
utils::untar(archive, exdir = before_lib)

before <- guide_text(before_lib, file.path(tempdir(), "guide-before"))
after <- guide_text(".", file.path(tempdir(), "guide-after"))
cat("Only before:\n", paste("-", setdiff(before, after), collapse = "\n"), "\n")
cat("Only after:\n", paste("+", setdiff(after, before), collapse = "\n"), "\n")
```

For a refactor, the only differences may be the template's version. For a
feature change, every difference must be one you intended. A template that is
new on the branch has no "before": read its guide text in full instead.
Abstract templates and mixins have no guide; compare the concrete templates
that use them.

**Preview in the app** when a human will look at it, or when you can drive a
browser:

```r
Sys.setenv(DTATOOLS_TEMPLATE_SOURCES = paste0("dev=dir:", normalizePath(".")))
DTAtools::run_dta_app()
```

"Create new from template" then offers this working tree; after editing a
file, click "Refresh templates" instead of restarting. Never commit the
variable, and never put a Git token in it.

## Before you open a pull request

Commit, then run these from the repository root, against the deploy branch:

```sh
Rscript -e "DTAtools::lint_template_repo('.', fail_on = 'warning')"
git diff --name-only --diff-filter=MDR origin/main...HEAD -- '*@*'
git diff --name-only --diff-filter=D origin/main...HEAD -- templates vocabulary
git diff --name-only --diff-filter=A origin/main...HEAD -- '*@*'
```

- The linter reports no errors, and every remaining warning is one marked
  "may stay" above, with the reason in the description.
- The second command prints nothing: no frozen file was modified, deleted or
  renamed (the single exception, a pin that restores what a frozen version
  built, needs the owner's approval).
- The third prints nothing unless the owner approved deleting a released
  version.
- The fourth lists the files frozen on this branch. Each must be the released
  head, byte for byte; this prints nothing when it is:

  ```sh
  git diff origin/main:templates/company_dta.dta-template.yaml "HEAD:templates/company_dta@1.1.dta-template.yaml"
  ```

Then describe the change:

```markdown
## Purpose
<what the authors of new DTAs get, in one or two sentences>

## Library changes
| File | id | Version | Change |
| --- | --- | --- | --- |
| templates/company_dta.dta-template.yaml | company_dta | 1.1 → 1.2 | new data-protection section (minor) |
| templates/company_dta@1.1.dta-template.yaml | company_dta | 1.1 | frozen copy of the released version |

## References
- `biomarker_study_acme` → `biomarker_study@latest` (follow: the deviation should track the family)
- `biomarker_study_acme` → `biomarker_samples@1.0` (pin: patched)

## Verification
- `lint_template_repo('.', fail_on = 'warning')`: "No findings in '.'."
- Guide comparison for `biomarker_study_acme`: only the version line differs.

## Needs a decision
<anything from "Ask a human before you" in SKILL.md, or "none">
```
