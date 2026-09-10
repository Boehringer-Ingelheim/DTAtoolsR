# Validation Plan — DTAtools

This plan states what is qualified, how, and what counts as acceptable. It is
the document the evidence bundle is measured against; the report produced by
`run_qualification()` refers back to it.

## 1. Purpose and approach

DTAtools validates tabular data transfers against written Data Transfer
Specifications. Where those transfers carry clinical or biomarker data, the
question the tool answers — does this delivery conform to what was agreed —
becomes a question about data integrity, and the tool itself has to be shown
to work before its answer can be relied on.

The software is bespoke and its behaviour is not exercised by any supplier's
qualification, so it is handled as **GAMP 5 category 5**: it needs its own
functional specification, its own testing against that specification, and
documented evidence tying the two together.

Testing is proportionate to risk, in the sense the FDA's *Computer Software
Assurance* guidance uses. Features whose failure could let a non-conforming
delivery be accepted — the validation axes, the rule engine, the reader, the
integrity of exported data — are tested with scripted cases and recorded
expected-versus-observed results. Features whose failure is visible and
recoverable — document formatting, the interactive application's layout — are
tested more lightly, and the report says which is which through each
requirement's risk rating.

Three stages are run:

| Stage | Question it answers |
| --- | --- |
| Installation qualification (IQ) | Is the right software, with the right dependencies, correctly installed on this machine? |
| Operational qualification (OQ) | Does each specified behaviour do what the specification says, against expected values written before the run? |
| Performance qualification (PQ) | Does it do so end to end, on realistic data, at the volumes it will meet in use? |

## 2. Scope

**In scope.** Every symbol the package exports. The frozen list is
`requirements/_api.yaml`, and the installation stage fails if the installed
package's exports differ from it, so the scope cannot drift without being
noticed.

**Out of scope.** The correctness of R itself and of the package's
dependencies; these are covered by their own releases and their versions are
recorded in the environment section of every report. The infrastructure the
package runs on. The content of any particular Data Transfer Specification: the
tool is qualified to enforce a specification, not to judge whether a
specification is the right one. The interactive application's visual
appearance.

## 3. Evidence and its properties

Each run writes a bundle whose properties are the ones a regulated record needs.

- **Attributable.** The report names who performed the run, on what host, with
  which build of the package, and who is to review it.
- **Contemporaneous.** `run.log` is written and flushed as events occur, not
  assembled afterwards, so a run that dies half way still leaves usable
  evidence of how far it got.
- **Original and accurate.** Each step records the expected value, the observed
  value, and the source file and line that produced it. Nothing in the report
  is asserted that `results/results.json` does not also contain.
- **Complete.** Test cases are discovered by parsing the suite before it runs,
  so a case that never executed is reported as not executed rather than
  omitted. Skips carry their reason.
- **Enduring and available.** The bundle is plain text and self-contained. The
  report is also rendered to HTML, Word and PDF where the tooling exists.
- **Tamper-evident.** A hash manifest covers every file and is written last. The
  report quotes the hash of `results.json`, taken before the report was
  rendered, which ties the document to the data it describes.

## 4. Two rules that make the evidence mean something

**No expected value comes from the software.** Snapshot testing — recording
what the software produced and asserting it produces that again — cannot
detect a behaviour that was wrong when the snapshot was taken. Every expected
value in this suite is written from the specification, and a meta-consistency
check fails the run if a snapshot expectation appears anywhere in it.

**Known defects are declared, not discovered.** `deviations.yaml` lists the
defects already found and assessed. Each open entry is bound to a test that
asserts the defect is still observable. A failure the run finds that is not in
the register is therefore a new finding; and a registered defect that stops
reproducing fails the run, because the register has stopped describing the
software and a register that misstates the software is worse than none.

## 4a. Evidence that these tests can fail

A test suite that passes proves nothing until someone has shown it is capable
of failing. Before this suite was released, five defects were deliberately
introduced into the package one at a time, the suite was run against each, and
the affected test cases were recorded. Every mutation was reverted immediately
after its run.

| Defect introduced | Where | Detected by |
| --- | --- | --- |
| A range rule's lower bound made exclusive | `R/evaluateRules.R`, the range operator | OQ-RULE-010 |
| The permitted-value check made to report nothing | `R/columnSpecChecks.R`, the enum branch | OQ-VAL-006, 011, 012, 024, 028, 030, 031, 033 |
| The recorded count of import errors forced to zero | `R/DTADataSetTabular-class.R`, the validation index entry | OQ-TYPE-019, OQ-VAL-014, 030, 035 |
| Declared length measured in bytes rather than characters | `R/columnSpecChecks.R`, the length branch | OQ-VAL-005 |
| Errors past the in-memory cap discarded instead of spilled | `R/streamingValidation.R`, the error sink | OQ-STREAM-006, 007 |

Each mutation was caught, and each was caught by a test case whose subject is
the behaviour that was broken rather than by an unrelated one.

One negative result is recorded with them. The same import-error count was also
forced to zero in the separate branch that runs when a delivery's file cannot be
read at all, and no test case failed. That branch reports its own absence
through a different path, so the count it writes is not what any requirement
reads. It is stated here because a control that was run and did not fire is part
of the evidence, not something to leave out of it.

## 5. Acceptance criteria

The run is acceptable when all of the following hold, and the verdict line of
the report states which of them did not.

1. Every installation-qualification case passes.
2. No operational or performance case fails or errors.
3. Every meta-consistency check passes: every test is traced to a requirement
   that exists, every requirement is exercised, every exported symbol is
   claimed by a requirement, no snapshot expectations, and every declared test
   case actually ran.
4. Every open deviation still reproduces.
5. Any requirement left unverified — because an optional dependency was absent,
   or a scale tier was not selected — is listed, and the reviewer accepts each
   one as immaterial to the intended use of this installation.

A verdict of `PASS WITH NOT-EXECUTED TESTS` requires the reviewer's judgement
on point 5. A verdict marked `(PARTIAL)` cannot support a complete claim about
the software: a filter or a stage selection was applied, and the run should be
repeated in full before it is signed.

## 6. Roles

| Role | Responsibility |
| --- | --- |
| Performed by | Runs the qualification on the target system and files the bundle. Recorded automatically; may be overridden with the `tester` argument. |
| Reviewed by | Checks the environment is the intended one, judges each unverified requirement and each deviation, and confirms the evidence supports the verdict. Must not be the person who performed the run. |
| Approved by | Accepts the software for the intended use on this system. |

## 7. When to re-qualify

A qualification describes one build of the software on one system. Repeat it
when any of these changes:

- the package version;
- the R minor version (4.5 to 4.6, say), or the major version of Arrow;
- the operating system, or the deployment target the package is installed onto;
- any dependency that fails its declared minimum after an environment change;
- a defect is fixed that was reproduced by the previous run, since its register
  entry and its test both change.

A patch to an unrelated dependency does not require re-qualification, but the
next run will record the new version and the difference will be visible.

## 8. Deviation handling

A failure that is not in the register is a new finding. It is recorded in
section 8.2 of the report with the file and line that produced it, and it must
be assessed before the software is used: either fixed, or added to the register
with an impact statement and a workaround, and bound to a test.

Registered deviations are reviewed at every run. An entry that no longer
reproduces is closed, and its test is promoted to assert the correct behaviour.
