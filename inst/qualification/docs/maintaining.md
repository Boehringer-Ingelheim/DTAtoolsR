# Maintaining the qualification suite

This is for whoever changes DTAtools next, human or agent. It answers one
question: **the package changed, so what has to change here, and how?**

The suite is not an ordinary test suite. It produces evidence that a
pharmaceutical quality unit signs, so it is held to rules an ordinary suite is
not, and several of those rules are enforced mechanically. A change that
ignores them does not fail quietly later: it fails the next run, by design.

Read [the Validation Plan](validation-plan.md) for why the suite exists and
[the README](../README.md) for how to run one. This file is only about keeping
the suite true as the package moves.

---

## The one rule everything else serves

**An expected value never comes from the software.**

Write what the specification says should happen, then check the software
against it. Never run the code, look at the output and paste it in as the
expectation. A test written that way cannot detect a behaviour that was
already wrong when you wrote it, which is exactly the failure the suite exists
to prevent.

This is why `expect_snapshot()` is banned outright, and why meta-check M8
fails the run if one appears anywhere. It is also why, when you change a test,
you should be able to say **which sentence of which requirement** the new
expected value comes from. If you cannot, stop: either the requirement needs
updating first, or you are recording behaviour rather than specifying it.

---

## What to change, by what you changed

### You added, renamed or removed an exported function

Four things, in this order.

1. **`requirements/_api.yaml`** is the frozen export list. IQ-ENV-014 compares
   `getNamespaceExports("DTAtools")` against it element for element, so an
   export that is not listed fails the installation stage. Add, rename or
   remove the entry. Keep it sorted.
2. **Cover it with a requirement.** Meta-check M5 fails if any exported symbol
   is absent from every requirement's `covers:` list. Pick the area file that
   matches the function's domain (`REQ-EXPORT.yaml` for a writer,
   `REQ-RULE.yaml` for a rule type, and so on) and add a requirement, or add
   the symbol to an existing requirement's `covers:` if it genuinely serves
   that same statement.
3. **Write at least one test case** for the new requirement. M4 fails if a
   requirement has no test case.
4. **Raise the area's `min_tests`** in `requirements/_index.yaml` to the new
   count. That floor is what stops an area being quietly emptied by a later
   refactor; it is a ratchet, so move it up when you add, and only lower it
   deliberately.

For a **removal**, delete the requirement and its test cases too, and lower
the floor. Do not leave a requirement covering a symbol that no longer exists:
M6 fails on a `covers:` entry that is not an export.

### You changed what an existing function does

Find what already describes it:

```r
qualification_requirements()                      # every requirement
qualification_requirements(area = "VAL")          # one area
```

or grep the `covers:` lists for the function name.

Then decide which of these you are doing, because they are different:

- **The specification changed.** Rewrite the requirement `text:` first, then
  the test's expected values to match it. The requirement is the contract; the
  test is the check. Changing only the test leaves the suite asserting one
  thing and claiming another.
- **The specification did not change, the implementation did.** The requirement
  stays. If a test now fails, the implementation is wrong, or the test was
  wrong about the specification. Do not adjust the expected value to make the
  failure go away without establishing which.

### You fixed a defect that the register records

**The suite will fail, and that is correct.** Each open entry in
`deviations.yaml` is bound to a test asserting the defect is *still
observable*, via `qa_known_deviation()`. When the defect stops reproducing
that assertion fails, deliberately, so the register cannot quietly become a
description of software that no longer exists.

Do all four:

1. **Close the entry** in `deviations.yaml`: `status: closed` and
   `closed: <YYYY-MM-DD>`. Leave the entry in place; closed entries are
   history and still appear in the report.
2. **Remove the `qa_known_deviation()` call** and rewrite that test to assert
   the correct behaviour. Assert the *fix*, not merely the absence of the bug:
   cover the shapes a caller actually arrives with.
3. **Check whether a requirement was written to describe the defect.** Some
   are, and they say so in their `notes:` with the phrase *"Pinned, not
   endorsed."* Those requirement texts must be rewritten to state the correct
   behaviour, and any sibling requirement that referred to the old behaviour
   updated with them. Search the area file for `Pinned` before you assume the
   test is the only thing to change.
4. **Add a `CHANGELOG.md` entry** under `## [Unreleased]` → `### Fixed`,
   saying what went wrong and why it mattered, not just what you changed.

### You found a new defect

Register it rather than working around it.

- A **defect** (the software does not do what the requirement says) becomes a
  `DEV-nnn` entry with `status: open`, and **exactly one**
  `qa_known_deviation("DEV-nnn", <expr>)` binding somewhere in the suite. M7
  fails if an open entry has no binding; more than one binding makes the
  register ambiguous about what is being tracked.

  `<expr>` must be `TRUE` **while the defect is still observable**. Write it as
  the narrowest observation you can: if it is too broad it will keep passing
  after a partial fix.

- **`status: pending`** is for a defect you have documented but this suite does
  not itself reproduce, usually because it was found elsewhere. A pending entry
  needs no binding and M7 ignores it. Use it honestly, not to avoid writing a
  binding.

- A **limitation** (the software behaves as designed, but the design has a
  consequence worth recording) becomes a `LIM-nnn` entry. Limitations get no
  binding. Pin the behaviour with a normal `qa_step()` and a comment naming the
  entry, the way `LIM-004` and `LIM-009` already are.

Every entry needs the full schema: `title`, `severity` for a deviation,
`affects` (requirement ids that must exist, M3), `observed`, `expected`,
`impact`, `workaround`, `reference`, `opened`. Write `impact` for a reader
deciding whether they can ship: what does this cost the person relying on the
software?

### You changed anything under `inst/`

Regenerate the installed-file manifest:

```bash
Rscript .github/scripts/sync_qualification_manifest.R
```

IQ-ENV-016 hashes every file the manifest lists against the installed tree, so
a stale manifest fails the installation stage, and the `r-style` workflow fails
on the diff. This bites most often when someone *else's* merge touches
`inst/shiny/dta_app/`, so check it after every merge from `dev`, not only after
your own edits:

```bash
Rscript .github/scripts/sync_qualification_manifest.R --check
```

Two paths are deliberately excluded and must stay excluded:
`qualification/baseline/` (a manifest cannot describe itself) and
`shiny/dta_app/manifest.json` (generated and resynced by the manifest-sync
workflow; hashing one generated manifest inside another makes the two chase
each other on every bot push).

### You changed the Shiny app

The APP tests drive the real server function through `shiny::testServer()`, so
**input names are a contract**. Renaming an input, or changing which input a
handler reads, breaks them.

The trap that has caught us before: **`testServer` has no browser**, so any
input the browser would have created does not exist. That is not the same as
an unset input — the app may read an absent input as a deliberate empty
answer. If a flow depends on a control the dialog renders, the test has to
supply it, as `qapp_fill_template_vocab()` does for vocabulary slots. Reproduce
the browser; do not work around it.

`inst/shiny/dta_app/manifest.json` is regenerated by CI, so leave it alone.

### You added or changed a package option

IQ-ENV-020 pins the default of every option read via
`getOption("DTAtools....")` in the package's own source. Add the new option and
its default to that test.

### You added a dependency

`DESCRIPTION` as usual. The installation stage then checks it automatically:
`Depends` and `Imports` must be installed and satisfy their minimum version,
and every `Suggests` package is inventoried with the test cases its absence
would disable. Nothing to write by hand, but a new hard dependency is a
decision worth stating in the Validation Plan's scope.

### The package version was bumped

Update `package_version:` in `requirements/_index.yaml` to match. M12 fails
otherwise, on the reasoning that requirements written for one version say
nothing certain about another. `bump_version.R` does not do this for you.

---

## Writing a test case

### The title is the traceability

Titles are parsed, not decorative. The grammar is

```
<STAGE>-<AREA>-<nnn> | <what it shows> | REQ-<AREA>-<nnn> [REQ-... ...] [| tags: a,b]
```

with `STAGE` one of `IQ`, `OQ`, `PQ`. For example:

```r
test_that("OQ-VAL-007 | a codelist violation is reported once per row | REQ-VAL-001", {
```

- M1 fails on a title that does not parse. There is no second place to record
  which requirement a test verifies: the title *is* the traceability matrix.
- M2 requires ids unique, and the file name must carry the same stage and
  area: `test-OQ-VAL-*.R`.
- M3 requires every requirement id named to exist.
- M13 restricts tags to: `white-box`, `subprocess`, `browser`, `slow`,
  `scale-standard`, `scale-full`.

Number within the area's own sequence, continuing from the highest in use.

### Record evidence, not just assertions

Use the helpers so the report shows expected and observed without a reader
having to open the test:

```r
qa_step("the delivery reports three bad rows", 3L, observed)   # expected vs actual
qa_check("and is invalid overall", isFALSE(status$ok))         # a boolean claim
```

Both record the description, the values and the file and line that produced
them. Plain `expect_*()` calls work but contribute nothing a reviewer can
read, so use them only where a value comparison makes no sense.

The evidence helpers are in `tests/helper-00-qa.R`:

`qa_step` `qa_check` `qa_known_deviation` `qa_requires`
`qa_skip_unless_scale` `qa_skip_unless_memory_gb` `qa_skip_unless_browser`
`qa_tempdir` `qa_artifact_dir` `qa_perf` `qa_perf_recorded` `qa_subprocess`
`qa_messages_norm` `qa_counts` `qa_tier_rows` `qa_parity_seeds`

Fixtures and the closed-form oracle are in `tests/helper-generators.R`:
`qa_specs` `qa_frame` `qa_oracle` `qa_oracle_counts` `qa_write_csv`
`qa_write_large_csv` `qa_dta`. Document parsing is in `tests/helper-parse.R`
(`qa_docx_text`, `qa_docx_tables`, `qa_html_report`, `qa_read_yaml`) and the
app harness in `tests/helper-app.R` (`qapp_*`).

### Rules that are enforced

- **Never write outside `qa_tempdir()` or `qa_artifact_dir()`.** On a target
  system the suite runs from a read-only library, and testthat sets the working
  directory to the test folder inside it.
- **No `expect_snapshot*`** (M8, parse-based, so a mention in a string is fine
  but a call is not).
- **No existence-only assertions.** Reading a file back beats
  `file.exists()`; a count beats "the column is present". A test that cannot
  fail is worse than no test, because it reports coverage it does not have.
- **Never assert on translated text.** Base R messages come out in the system
  language. Match a condition class or a package-authored `cli` string.
- **Skip with a reason, through the helpers only.** Every skip is printed in
  the report with its reason and marks its requirements unverified, so an
  unexplained skip is an unexplained gap.

### Writing a requirement

One testable statement per requirement, in `requirements/REQ-<AREA>.yaml`:

```yaml
  - id: REQ-VAL-001
    text: >
      check() shall report a table as valid if and only if it has zero
      column-specification, zero rule and zero import errors.
    risk: high                    # high | medium | low
    category: functional          # functional interface installation performance
                                  # integrity security documentation
    covers: [check, validation_status]   # exported symbols only (M6)
    source: "README section The Three Validation Axes; R/DTADataSetTabular-class.R"
    notes: >
      Optional. Where an expected value came from, or why a behaviour is
      pinned rather than endorsed.
```

Write `text:` so it can be **falsified**. "shall handle errors gracefully"
cannot be tested; "shall raise a condition of class `x` and write nothing to
disk" can. `risk:` drives how deeply the area is tested; be honest about what a
failure would cost a data transfer.

---

## When a meta check fails

The meta checks guard the suite against itself. Each names what is wrong.

| Check | Means | Fix |
| --- | --- | --- |
| M1 | A `test_that()` title does not parse | Correct the title to the grammar above |
| M2 | Duplicate test-case id, or id disagrees with the file name | Renumber, or move the test to the right file |
| M3 | A test names a requirement or deviation that does not exist | Add it, or correct the reference |
| M4 | A requirement has no test case | Write one, or delete the requirement |
| M5 | An exported symbol is in no requirement's `covers:` | Cover it, usually with a new requirement |
| M6 | A `covers:` entry is not an export | Remove it, or export the symbol |
| M7 | An open deviation has no `qa_known_deviation()` binding | Bind it, or close the entry if it is fixed |
| M8 | A snapshot expectation exists | Replace it with a stated expected value |
| M9 | Duplicate deviation or limitation id | Renumber |
| M10 | A discovered test case did not run | Usually a file-level error; read `run.log` |
| M11 | An area is below its `min_tests` floor | Restore the tests, or lower the floor deliberately |
| M12 | `_index.yaml` version differs from the installed package | Update `package_version:` |
| M13 | An unknown tag | Use one of the six permitted tags |
| M14 | Measured throughput is below a `perf_floor` that was set | Investigate the machine, or drop the floor |

M7 and M10 are the two that most often fire mid-edit, and both are usually
telling you that you are halfway through a change: a binding removed before
the register was closed, or a test file that no longer parses.

---

## Verifying your change

Work outward. Do not start with the full run; it takes half an hour.

**One area, seconds to a couple of minutes:**

```bash
Rscript --no-init-file -e "pkgload::load_all(quiet = TRUE); r <- DTAtools::run_qualification(tempfile('q'), stages = 'OQ', scale = 'quick', formats = 'md', filter = 'VAL', quiet = TRUE); print(summary(r))"
```

`filter` is a regular expression on the test file name, so `"VAL|ROBUST"`
works, and `"APP"` runs the application area alone.

**The pull-request guard**, which is what gates a PR and needs no install:

```bash
Rscript --no-init-file -e "devtools::test(filter = 'qualification')"
```

**The developer suite**, whenever you touch anything under `R/`:

```bash
Rscript --no-init-file -e "devtools::test()"
```

**A full run**, before you call a change done:

```bash
Rscript --no-init-file -e "pkgload::load_all(quiet = TRUE); DTAtools::run_qualification('~/qual-check', scale = 'standard')"
```

Then read `SUMMARY.txt` and confirm the verdict, and that the bundle verifies:

```bash
sha256sum -c SHA256SUMS
```

**The repository checks**, all of which CI enforces:

```bash
Rscript .github/scripts/style.R
Rscript -e "roxygen2::roxygenise()"
Rscript .github/scripts/sync_qualification_manifest.R
Rscript --no-init-file .github/scripts/check_deps_in_desc.R
pre-commit run --all-files
```

A pull request that targets a branch other than `master` or `dev` runs **no
CI at all** — every workflow here is gated on those two bases. If you stack
work on a feature branch, your local runs are the only gate until it reaches
`dev`.

---

## Traps that have actually caught us

Each of these cost real time. None is hypothetical.

- **Roxygen block placement can delete another function's help page.** A new
  `#'` block inserted between an existing function's documentation and its
  definition orphans that documentation, and `roxygenise()` then deletes the
  `.Rd`. Always run `roxygen2::roxygenise()` and check `git status man/` after
  adding a documented internal helper.

- **Anything written after the stages must be non-fatal.** By the time the
  bundle is written, every stage has already run; aborting there throws away
  the whole run's evidence. Two separate additions did exactly that, one where
  `write.csv()` refused a list column and one where `jsonlite` refused the
  condition objects inside it. Route new evidence files through
  `qual_write_evidence()`, which flattens and logs rather than aborting.

- **A `qa_known_deviation()` that fails is the system working.** It means the
  defect stopped reproducing. Close the register entry; do not "fix" the test
  by loosening the assertion.

- **`testServer` has no browser.** See the app section above.

- **The installed-file manifest goes stale from other people's merges**, not
  just your own edits. Check it after every merge from `dev`.

- **Line endings matter, because files are hashed.** The repository forces LF
  through `.gitattributes`. A file written by R through a text connection on
  Windows comes out CRLF, and if it is a file the manifest hashes, a fresh
  checkout then disagrees with the recorded hash. Write through a binary
  connection when generating a tracked file.

- **`R CMD INSTALL --md5` is not a thing.** The checksum manifest that
  `tools::checkMD5sums()` needs comes from `R CMD INSTALL --build` or from
  installing a binary package. `R CMD build --md5` leaves it in the tarball,
  where install does not carry it forward.

- **A guard that checks one item out of many checks nothing.** The bundle
  verification originally recomputed a single hash from the manifest and passed
  while the log's own entry was wrong. If you are verifying a set, verify the
  set.

---

## House rules that apply here too

These come from `CLAUDE.md` and are not negotiable in this directory either.

- Style with `Rscript .github/scripts/style.R`, never `styler::style_pkg()`,
  which misses `inst/` and writes CRLF.
- Regenerate documentation with the roxygen2 version named by
  `Config/roxygen2/version` in `DESCRIPTION`. A different version rewrites the
  whole of `NAMESPACE` and CI fails on the diff.
- Never hand-edit `man/`, `NAMESPACE` or `renv.lock`.
- Never bump the version unless asked.
- User-facing changes get a `CHANGELOG.md` entry under `## [Unreleased]`.
- Never attribute work to an AI assistant anywhere in this repository,
  including commit messages, pull request bodies and code comments.
