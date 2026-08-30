# Starting your own DTAtools template repository

This directory is a working example of a *private template repository* --
the kind of flat folder of YAML files that the DTAtools Shiny app's "Create
new from template" picker reads. Copy it as your starting point, or run
`DTAtools::create_template_repo()` to generate it (plus a `.gitignore` and a
CI workflow) into a fresh directory for you.

## What is here

Four YAML files make up one small, self-consistent template family, wired
together the same way a real one would be:

- `starter.dta-template.yaml` is the entry point -- a *creation template*.
  It is what a user picks from the app's "Create new from template" list. It
  supplies a minimal `base:` metadata block, two options that edit that
  metadata, one dataset (imported, not inlined), and a party slot.
- `starter_dataset.dta-dataset-template.yaml` is the dataset the creation
  template imports via `datasets: [{template: starter_dataset@1.0, ...}]`,
  rather than inlining its columns directly. Keeping it as its own file
  means the same column set could be reused from a second creation template
  without copying it.
- `starter_terms.dta-vocabulary.yaml` is a small controlled vocabulary. The
  dataset template's `STATUS` column binds to it with `values_from:` instead
  of writing out a permitted-value list inline, so the two can never drift
  apart.
- `starter_supplier.dta-party.yaml` is a party profile. The creation
  template's `party_slots:` offers it, so choosing it in the app fills in a
  whole affiliation-plus-contacts block in one step instead of retyping it.

Rename `starter`/`starter_dataset`/`starter_terms`/`starter_supplier` to your
own ids as you build out your own family, and replace the placeholder
metadata (organisation name, supplier details, transmission terms) with your
own. The `id@version` references between the files (`starter_dataset@1.0`,
`starter_terms@1.0`, `starter_supplier`) need to be updated to match wherever
you rename something they point at.

## The directory must be flat

Both the validator and the app's own template loader scan a template
repository **non-recursively** -- they list the files directly inside the
configured directory and go no further. A template file placed in a
subdirectory is not an error and produces no warning; it is simply invisible
to both, as if it did not exist. Keep every `.dta-*.yaml` file directly in
this directory (or in whichever directory you point DTAtools at), never
nested under a subfolder.

## The four file kinds

Every file's `kind:` field and its filename suffix must agree; the suffix is
what a directory scan actually keys off:

| Kind | Filename suffix |
| --- | --- |
| `dta_creation_template` | `*.dta-template.yaml` |
| `dta_dataset_template` | `*.dta-dataset-template.yaml` |
| `dta_party_profile` | `*.dta-party.yaml` |
| `dta_vocabulary` | `*.dta-vocabulary.yaml` |

(The `.yml` spelling of each suffix is accepted too.)

## Pointing the app at this directory

Set the `DTATOOLS_TEMPLATE_SOURCES` environment variable wherever `run_dta_app()`
runs (a Posit Connect deployment's environment variables, or your own shell
before launching the app locally) to a `dir:` source naming this directory:

```r
Sys.setenv(DTATOOLS_TEMPLATE_SOURCES = "starter=dir:/path/to/this/directory")
```

See `vignette("private-templates", package = "DTAtools")` for the full
variable reference, including the `git:` source scheme for a shared,
version-controlled repository and how to keep the packaged examples
available alongside your own.

## Validating

Before committing a change to this directory, or in CI, run:

```r
DTAtools::validate_template(".", strict = TRUE)
```

This runs every structural check the app's own template picker relies on --
an unresolvable `extends:`, an option `target:` that does not resolve to a
real metadata field, a party slot naming a profile that does not exist, an
unquoted `version:`, a `values_from:` naming a vocabulary that is not there
-- and, as a final and most comprehensive check, actually builds a document
from every non-abstract creation template using its own default selections.
`strict = TRUE` raises an error naming every problem found, which is what
`create_template_repo()`'s generated `.github/workflows/validate-templates.yml`
runs on every push and pull request. Drop `strict = TRUE` to get a data frame
back instead, for interactive inspection.
