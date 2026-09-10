# References

The standards and guidance this qualification approach follows. Status was
checked on 2026-09-09; where a document is in revision, both the version in
force and the pending one are named, because a qualification performed now is
assessed against the former and will be read under the latter.

## Software validation and computerised systems

**ISPE GAMP 5: A Risk-Based Approach to Compliant GxP Computerized Systems,
second edition** (July 2022). Current. Supplies the lifecycle model, the
software categories, the IQ/OQ/PQ structure and the expectation that testing is
traced to requirements. DTAtools is treated as a category 5 (custom)
application, which is why it carries its own specification and its own testing
rather than leaning on a supplier's.

**FDA, *Computer Software Assurance for Production and Quality System
Software*.** Final guidance issued 24 September 2025, updated 3 February 2026.
Supersedes section 6 of the 2002 software validation guidance. Establishes the
risk-based split this suite uses: scripted testing with recorded results for
high-risk features, lighter and less formal evidence for low-risk ones.

**FDA, *General Principles of Software Validation*** (11 January 2002). Current
except for section 6. Supplies the underlying validation principles and the
expectation that review is independent of the person who performed the work.

**EudraLex Volume 4, Annex 11: Computerised Systems** (in force since June
2011). A substantially expanded revision was released in draft on 7 July 2025;
its consultation closed on 7 October 2025 and the final text is expected during
2026, with a transition period after that. The draft extends the annex to cloud
services, artificial intelligence and machine learning components, and
strengthens the requirements on audit trails, access management and security.
Nothing in this suite depends on which version is in force, but a report
produced now will be read against the revised text once it applies.

**ICH E6(R3), Good Clinical Practice.** Principles and Annex 1 adopted 6
January 2025; effective in the European Union from 23 July 2025; published by
the FDA in September 2025. Annex 2 follows later. Requires that computerised
systems used in trials be validated proportionately to the risk they carry to
participant safety and to the reliability of results.

**PIC/S PI 041-1, *Good Practices for Data Management and Integrity in
Regulated GMP/GDP Environments*.** Supplies the ALCOA+ properties the evidence
bundle is built to satisfy, described in section 3 of the validation plan.

**21 CFR Part 11**, *Electronic Records; Electronic Signatures*. Applies to the
records this suite produces where they are retained as electronic records. The
suite produces the record and its integrity controls; applying an electronic
signature to the report is outside the tool and is the responsibility of the
quality system it is filed in.

## R in a regulated setting

**R Validation Hub, *A Risk-based Approach for Assessing R Package Accuracy
within a Validated Infrastructure*.** The cross-industry white paper on using R
for regulatory work. Its four risk dimensions — documentation, testing,
maintenance and community — inform the risk assessment accompanying this plan.

**riskmetric** (R Validation Hub). Version 0.2.5, July 2023. Scores a package
against those dimensions. Not a dependency of this suite; its dimensions are
used as a checklist, not as an automated gate.

**valtools** (PHUSE R Package Validation Framework working group). Version
0.4.1, available from GitHub rather than CRAN. Its artefact set — requirements,
test cases, test code, validation report — is the shape this suite reproduces.
It is deliberately **not** a dependency: the suite has to run on a target
system with nothing but R, this package and testthat, and adding a
GitHub-only dependency to the qualification path would undermine the thing
being qualified.

## A note on citing these

None of these documents is reproduced here, and none of them is a substitute
for the judgement of the quality unit that owns the system. They are cited so
that a reviewer can see which framework a decision came from, and so that a
future reader knows which text was in force when the run was made.
