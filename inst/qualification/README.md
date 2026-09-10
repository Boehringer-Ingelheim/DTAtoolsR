# DTAtools qualification suite

This directory holds the qualification suite: a second, separately-run test
suite whose job is not to help develop the package but to produce **evidence
that the copy installed on a particular machine behaves as specified**.

It ships inside the package, so it is available wherever the package is
installed, including a server you cannot check out a git repository onto.

It covers every exported function of the package, across fifteen functional
areas: the runner itself, the installed environment, specifications, declared
types, the validation engine, rules, file handlers, streaming, metadata,
templates and controlled vocabulary, export, the standalone report, the
application, robustness, and end-to-end behaviour at scale. Run
`qualification_requirements()` for the current list, and the report's
traceability matrix for what verified each of them on a given run.

## Running it

```r
DTAtools::run_qualification("~/dta-qualification")
```

That is the whole procedure. It runs all three stages at the `full` scale
tier and writes an evidence bundle into the directory you name.

From a shell:

```bash
Rscript -e 'DTAtools::run_qualification("~/dta-qualification")'
```

Useful variations:

```r
# Faster tiers. Correctness is checked identically in all three; the tier
# controls how much data the performance stage generates.
run_qualification("out", scale = "standard")
run_qualification("out", scale = "quick")

# One stage only.
run_qualification("out", stages = "IQ")

# Record who performed and who will review the run. Both appear in the
# report's approval block.
run_qualification("out", tester = "A. Operator", reviewer = "B. Reviewer")

# Require the machine to reach a throughput. Without this the performance
# stage records what it measured and passes no judgement on it, which is the
# right default when the target hardware is unknown. With it, a measurement
# below the floor fails the run and the report names it.
run_qualification("out", perf_floor = 20000)
```

### What it needs

Only R, the package and its dependencies, plus `testthat`. Everything else is
optional and its absence is recorded rather than fatal:

| Optional | What it adds | Without it |
| --- | --- | --- |
| `rmarkdown` + pandoc | HTML and Word reports | Markdown report only |
| a PDF backend (LibreOffice, TinyTeX, LaTeX) | PDF report | no PDF |
| `R6` | the live event log during the run | results are unaffected |
| `shiny`, `bslib`, `DT` | the application tests | reported as not executed |
| `shinytest2`, `chromote` + Chrome | browser tests of the application | reported as not executed |
| `ps` | memory measurements | those figures are blank |
| package installed with `--install-tests` | the developer suite as supplementary evidence | reported as unavailable |
| installed from a binary, or with `--build` | a checksum check of every installed file | reported as not executed |

A test that cannot run is never silently dropped. It appears in the report
under "Tests not executed" with the reason, and the requirements it would have
verified are marked not verified.

To get the fullest evidence from a target system, install like this:

```bash
R CMD INSTALL --install-tests --build DTAtools_<version>.tar.gz
```

`--install-tests` ships the developer suite; `--build` writes a checksum
manifest into the library, which is what the installation stage compares the
installed files against. Neither is required and neither changes the verdict.
A package installed from a binary already carries the manifest.

### Time and space

| Tier | Largest table | Roughly |
| --- | --- | --- |
| `quick` | 10 thousand rows | minutes |
| `standard` | 1 million rows | tens of minutes, under 1 GB |
| `full` | 10 million rows | hours, and several GB of disk and memory |

The installation stage records the free disk and memory it found, and the
performance stage skips a tier it does not have the memory for rather than
failing.

## What you get

```
<output_dir>/QUAL-<version>-<timestamp>/
  SUMMARY.txt      the verdict, the counts, and how to verify the bundle
  run.log          every event as it happened
  report/          the Validation Summary Report (.md, and .html/.docx/.pdf when possible)
  results/         results.json plus CSVs: tests, expectations, traceability,
                   requirements, coverage, deviations, meta checks, environment
  artifacts/       files individual test cases chose to keep
  SHA256SUMS       a hash of every other file in the bundle
```

The report is the document a reviewer reads and signs. The CSVs and
`results.json` are the same information in a form a spreadsheet or a script can
read, so that nothing in the report has to be taken on trust.

## Reviewing and signing

1. Read `SUMMARY.txt`. The verdict is `PASS`, `PASS WITH NOT-EXECUTED TESTS`,
   or `FAIL`. A run marked `(PARTIAL)` used a filter or ran only some stages,
   and cannot support a complete claim.
2. Open `report/qualification-report.md` (or the HTML or Word rendering).
3. Check section 3 describes the system you intended to qualify.
4. Check section 7, the traceability matrix, for requirements that are not
   verified, and section 9 for tests that did not run. Decide whether each is
   acceptable for your intended use.
5. Check section 8 for deviations. Known deviations are defects that were
   already assessed and accepted; anything under "New failures" was not.
6. Sign the approval table in section 1.

## Verifying the bundle has not been altered

From inside the bundle directory:

```bash
sha256sum -c SHA256SUMS
```

The manifest covers every file except itself and is written last. The report
also quotes the hash of `results.json`, computed before the report was
rendered, which ties the document to the data it describes.

## What is in this directory

| Path | Contents |
| --- | --- |
| `requirements/` | one file per functional area; each requirement is one testable statement with a risk rating and the exported symbols it constrains |
| `requirements/_api.yaml` | the frozen list of exported symbols the installation stage checks against |
| `deviations.yaml` | known defects and documented limitations |
| `docs/` | the Validation Plan and supporting documents |
| `tests/` | the test cases, one file per stage and area |
| `fixtures/` | small hand-built inputs; anything large is generated at run time |
| `baseline/` | the hash manifest of the installed files, checked by the installation stage |

## How a test case is written

Test titles carry their own traceability:

```r
test_that("OQ-VAL-007 | codelist violation reported per row | REQ-VAL-001", {
  qa_step("a value outside the codelist is reported", 1L, observed_count)
})
```

`qa_step()` records the description, the expected value and the observed value
in one expectation, so the evidence shows all three without anyone having to
read the test code. The requirements named in the title are how the
traceability matrix is built, so it cannot drift from the tests.

Two rules matter more than the rest. Expected values are written down before
the run, never captured from the software's own output: the suite contains no
snapshot expectations and a meta-check enforces that. And a known defect is
bound with `qa_known_deviation()`, which fails when the defect stops
reproducing, so the register cannot quietly become a description of software
that no longer exists.
