# Glossary

Two vocabularies meet in this package and they use one word differently. The
distinction below is the one to hold on to; everything else follows from it.

**Validation** (the package's meaning). Checking a delivery of data against the
Data Transfer Specification that was agreed for it. This is what the package
does for a living: `check()`, `validate_table()`, `validate_file_stream()`, the
three validation axes, the HTML validation report.

**Qualification** (this suite's meaning). Checking the software itself against
its own specification. This is what `run_qualification()` does. The word is
used throughout the suite so that the two ideas never have to share a name.
The formal document titles keep the regulatory spelling — a *Validation Plan*
and a *Validation Summary Report* — because that is what a quality unit files
them as.

---

**ALCOA+**. The properties a regulated record must have: attributable, legible,
contemporaneous, original and accurate, plus complete, consistent, enduring and
available. Section 3 of the validation plan says how the evidence bundle
provides each.

**Area**. A functional part of the package that requirements and tests are
grouped by, such as `VAL` for the validation engine or `FILE` for readers.
Appears in every requirement and test case identifier.

**Axis**. One of the three independent ways a data delivery can fail: the
**column-specification** axis (a value breaks its column's declared type,
length, codelist or pattern), the **rule** axis (a cross-column or grouped rule
is violated), and the **import** axis (a value cannot be represented in the
type its column declares). A delivery is valid only if all three are clean.

**Deviation**. A defect: the software does something other than what its
requirement says. Declared in `deviations.yaml` before a run, bound to a test
that asserts it still occurs, and reported at every run. Distinguished from a
**limitation**, which is behaviour that is correct as designed but surprising
enough to be worth writing down.

**Evidence bundle**. The directory a run produces: the report, the
machine-readable results, the environment record, the run log, any artefacts,
and the hash manifest covering them.

**GAMP 5 category 5**. Custom software written for one organisation. It gets no
benefit from a supplier's testing, so it needs its own specification, its own
testing against that specification, and traceability between the two.

**IQ / OQ / PQ**. Installation qualification (is the right software correctly
installed here), operational qualification (does each specified behaviour work
against expected values written in advance), performance qualification (does it
work end to end, on realistic data, at the volumes it will meet).

**Meta-consistency check**. A check on the suite rather than on the package:
every test traced to a requirement that exists, every requirement exercised,
every exported function claimed by a requirement, no snapshot expectations, and
every declared test case actually executed. Recorded as `M1` upward, and a
failure fails the run.

**Requirement**. One testable statement about the software, with a risk rating,
a category and the exported symbols it constrains. Identified as
`REQ-<AREA>-<nnn>`.

**Scale tier**. How much data the performance stage generates: `quick`,
`standard` or `full`. Correctness is checked identically in all three — the
same defects are injected at the same relative positions — so a larger tier
tests behaviour at volume, never different behaviour.

**Snapshot expectation**. A test that records what the software produced and
asserts it produces the same again. Useful for catching unintended change,
useless for catching a behaviour that was already wrong, and therefore
forbidden in this suite.

**Test case**. One `test_that()` block, identified as `<STAGE>-<AREA>-<nnn>` and
naming the requirements it verifies in its title. That title is how the
traceability matrix is built, which is why it cannot drift from the tests.

**Traceability matrix**. The mapping from each requirement to the test cases
that verify it and to their outcome. Section 7 of the report.

**Verdict**. `PASS`, `PASS WITH NOT-EXECUTED TESTS`, or `FAIL`, optionally
marked `(PARTIAL)` when a filter or a stage selection means the run cannot
support a complete claim.
