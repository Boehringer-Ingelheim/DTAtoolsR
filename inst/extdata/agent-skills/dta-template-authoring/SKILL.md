---
name: dta-template-authoring
description: >-
  Create, change, review and lint a DTAtools template library: creation
  templates (*.dta-template.yaml), dataset templates
  (*.dta-dataset-template.yaml), controlled vocabularies
  (*.dta-vocabulary.yaml), party profiles (*.dta-party.yaml), Word export
  templates and dta-app.yaml. Use for any task that touches templates/,
  parties/, vocabulary/ or export-templates/ of a DTA template repository: a
  new DTA/DTS template, a supplier or study deviation, a new vocabulary term or
  version, reuse through extends or mixins, restructuring the library, or
  fixing findings from DTAtools::lint_template_repo() or validate_template().
---

# DTA template authoring

This repository is a **template library** for DTAtools. The DTAtools app reads
it to offer "Create new from template": an author picks a creation template,
answers its options, and gets a new Data Transfer Agreement/Specification
(DTA). Nothing here is executed; everything is YAML that the DTAtools template
engine resolves, merges and builds.

Your job on any task here has three parts, in this order of importance:

1. The library still builds: `lint_template_repo()` reports no errors.
2. Nothing already released changes under someone's feet.
3. The next change stays cheap: content lives in exactly one place and every
   reference says whether it follows or pins its target.

## Where the facts live

Do not write a field from memory. The engine ignores or rejects keys it does
not know, and several look plausible but do not exist (`example`, an option
`required`, `${anything}`).

| Need | Read |
| --- | --- |
| Field syntax of any file kind, every validator code | `README.md` at the repository root |
| The same, guaranteed to match the installed engine | The file whose path `Rscript -e "cat(system.file('extdata','template-repo-skeleton','README.md', package='DTAtools'))"` prints. Prefer it when the two disagree: the root copy dates from when the repository was created |
| How to lay out the library, versions, follow vs pin, step-by-step change recipes | [references/repository-architecture.md](references/repository-architecture.md) |
| extends vs mixins, seals, required, collection verbs, patches, anti-patterns, a worked family | [references/composition.md](references/composition.md) |
| Running the linter, triaging and fixing every finding, verifying what a template builds | [references/linting.md](references/linting.md) |

Read the reference for the part of the task you are on before you write YAML
for it. Read the README section of a file kind before writing a kind you have
not written in this session.

## The loop

Follow it for every change, including "small" ones; most broken libraries were
broken by a one-line edit.

1. **Baseline.** From the repository root, lint before touching anything:
   `Rscript -e "DTAtools::lint_template_repo('.', fail_on = 'warning')"`. If
   the installed DTAtools has no `lint_template_repo()`, use the fallback in
   [linting.md](references/linting.md) ("Older DTAtools") every time this
   skill says "lint". Note pre-existing findings so you neither hide them nor
   mistake them for yours.
2. **Inventory.** Find what exists and what depends on what you will touch:

   ```sh
   grep -nE '^(id|version|abstract|extends):' templates/*.y*ml vocabulary/*.y*ml parties/*.y*ml
   grep -rnw '<id>' templates vocabulary          # every file that references <id>
   ```

   Reuse before you add: an existing vocabulary, dataset template, mixin or
   party profile almost always beats a new near-duplicate.
3. **Choose the mechanism** with the table below. If two fit, prefer the one
   that keeps the content in one place.
4. **Decide the release handling.** Is the version you are about to change
   already released (merged into the branch the app reads)? Then freeze it
   and bump; otherwise edit in place. See "Releasing a change" in
   [repository-architecture.md](references/repository-architecture.md). Never
   guess: check with `git log`/`git show` against the deploy branch.
5. **Write the YAML** in house style (below).
6. **Lint, fix, repeat** until there are no errors. Fix in the triage order of
   [linting.md](references/linting.md); `instantiate_failed` is usually a
   consequence of an earlier finding for the same template, so fix that first.
   Then deal with every warning: fix it, or say in your report why it stays.
7. **Verify what it builds** when the change alters what an author sees or
   what a DTA contains: compare the template guide text before and after, or
   preview in the app ([linting.md](references/linting.md), "Beyond the
   linter"). Lint proves the library builds, not that it builds the right
   thing.
8. **Report** (in the PR description or your final message): purpose; every
   id and version added or bumped, and which files were frozen; each new
   reference with its follow/pin decision and the reason; the final lint
   summary line; anything you need a human to decide.

## Choosing the mechanism

| The request sounds like | Use | Avoid |
| --- | --- | --- |
| "Like template X, but supplier/study Y differs in a few defaults, texts or columns" | A child: `extends: X@latest` plus only the differences (`options: {modify: …}`, a `base:` override, a dataset `patch:`) | Copying X and editing the copy |
| "Every DTA of family F (and maybe others) also needs block B" (a clause, an assay dataset with its slot, a transfer-method set) | A **mixin** listed in `mixins:` | Adding B to a parent that families without B also extend |
| "Every DTA we create must say Z" | The abstract company standard at the root of the `extends:` chain; `sealed:` if governance owns the wording; `required:` if each concrete template must supply it | Repeating Z in every template |
| "The author chooses a value" | An option with `target:` (or `effects:` when one choice sets several fields) | A separate template per value |
| "The author chooses one of a few *structures* (platform A or B)" | `variants:` | Separate near-identical templates |
| "The author chooses which codes apply to a column" | `vocabulary_slots:` on the creation template | Asking for free text |
| "This column only allows these codes" | A vocabulary plus `values_from:` on the column | Literal `values:` copied into several dataset templates |
| "This one column allows only some of the vocabulary" | `values_from: {vocabulary: …, include: […]}` | A second, overlapping vocabulary |
| "Our organisation uses a subset/extension of a standard code list" | A vocabulary with `extends:` + `remove_terms`/`add_terms` | Copying the list |
| "One supplier delivers the dataset with a few extra or different columns" | A `patch:` on that supplier's dataset entry, pinned (`template: ds@1.0`) | Forking the dataset template |
| "A new kind of dataset" | A new dataset template, referenced with `template:` | An inline dataset in one creation template, if anything else may reuse it |
| "The same company's contact block in many DTAs" | A party profile plus a `party_slots:` entry | Contacts written into `base.metadata` |
| "Logo, colours, document name, which exports or features the app offers" | `dta-app.yaml` at the root | Anything in `templates/` |

[composition.md](references/composition.md) explains each row, with examples
that lint clean.

## Non-negotiables

Each has bitten someone; the reason is why it is not a style preference.

- **Quote every version**: `version: "1.10"`. Unquoted, YAML reads `1.10` as
  `1.1` and the text is lost before DTAtools sees it (`version_unquoted`).
- **Never edit a released version in place, and never edit a frozen
  `<id>@<version>.*` file.** Authors can still pick old versions in the app,
  and the app's version history is computed by building each version; editing
  one rewrites history.
- **Never delete a released version** (any file whose version reached the
  deploy branch). Its history entries vanish and so does the ancestor that
  re-basing a document needs. Retiring a template is the owner's decision;
  ask (see the recipe in repository-architecture.md).
- **Ids are permanent.** References, provenance in already-created DTAs and
  the version history all use the id. Do not rename an id to improve its
  spelling.
- **Keep every file directly in the folder for its kind.** The loader does not
  descend into sub-folders (`templates/archive/` is invisible) and the server's
  file system is case-sensitive (`Templates/` is not read).
- **Write every reference with an explicit version token**: `id@latest`
  (follow) or `id@1.2` (pin). A bare `id` also means latest but reads like an
  oversight. Party-slot `profiles:` are the exception: they take plain ids
  only, and `id@1.0` there silently never matches.
- **Do not invent**: no `metadata.template` or `metadata.import_issues` (the app
  owns them), no `${…}` token other than `${today}` and `${version}`, no
  `example:` (it is `examples:`), no option-level `required`/`min`/`order`.
- **Template changes do not go in `base.metadata.version_history`.** That list
  is the history of the *document* a template creates and is copied into every
  new DTA.
- **No personal data beyond what the DTA needs**, and only in a repository with
  matching access control. Prefer role mailboxes over named people in party
  profiles.
- **Done means lint-clean** (with the fallback's by-eye checks on an older
  DTAtools). Never report a change as finished on the strength of the YAML
  parsing, the app starting, or your reading of the merge rules.

## House style

- Header first, in this order: `kind`, `id`, `label`, `version`, then
  `abstract`, `extends`, `mixins`, `description`; body sections after a blank
  line. Every concrete template writes its own `label` and `description`:
  header fields are inherited through `extends:`, so a child without them
  shows its parent's description in the picker and the template guide.
- File name `<id>.<kind suffix>` for the current version (the "head") and
  `<id>@<version>.<kind suffix>` for a frozen release. Lowercase `snake_case`
  ids.
- Two-space indentation; `>-` for long prose.
- Quote any scalar YAML might coerce: codes such as `Y`, `N`, `yes`, `no`,
  `on`, `off`, `true`, `null`, numbers with leading zeros (`"007"`), anything
  that looks like a number but is text, and every hex colour (`"#1F4E79"`;
  unquoted it is a comment).
- Comments explain decisions, not syntax: why this reference is pinned, why
  this path is sealed, which agreement a deviation implements.

## Ask a human before you

- delete, retire or rename anything that has been released;
- make a breaking change to a vocabulary, dataset template, mixin or standard
  that other templates follow (removing or renaming a term, column, option,
  slot or dataset; tightening a type, `nullable`, a seal or `required:`);
- add `sealed:` or `required:` paths to a company standard, or remove any,
  unless the request asks for exactly that (a request that says governance
  owns some wording does); either way, name every template it affects in your
  report;
- change what a template promises a named partner (a supplier deviation, a
  pinned contract version);
- commit contact details of real people;
- leave a lint error, or a warning you cannot explain.

State what you found, the options, and your recommendation; do not pick the
destructive option to make lint pass.
