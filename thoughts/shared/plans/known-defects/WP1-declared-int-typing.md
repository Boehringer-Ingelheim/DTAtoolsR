# WP1: Declared Int typing (DEV-001, DEV-004, DEV-006)

Part of `thoughts/shared/plans/PLAN-known-defects.md`. Paste the standard brief
from §4 of that file into the agent prompt along with this file. Implements
decision **D1** (recommended option) unless the owner says otherwise.

## DEV-001: `BESTw.` infers Int

**Root cause.** `R/DTAColumnSpecStructureSAS-class.R`, function
`__dta_sas_infer_type_from_format__` (about lines 27–55). Line ~50 maps
`^(?:BEST[0-9]+\.|[0-9]+\.)$` to `"Int"`. `BESTw.` is SAS's general numeric
output format and can show decimals and scientific notation.

**Fix.**
- Split that regex: `BEST[0-9]+\.` returns `"Num"`, and `[0-9]+\.` keeps returning `"Int"`.
- Leave `__dta_sas_format_is_valid_for_type__` (about lines 72–89) unchanged. It
  already accepts `BESTw.` for both Num and Int, so an explicit
  `type = "Int", format = "BEST8."` stays valid.
- Only inference, when `type` is NULL, changes.

**Consequence to state in CHANGELOG.** A spec that gives a `BESTw.` format and
no type now types the column as Num. It becomes `number` in the JSON schema and
`double` in R.

## DEV-004 and DEV-006: one root cause, one fix

**Root cause.** `R/importConversion.R`, `dta_coerce_column()` (about lines
375–418), integer branch (about 399–408):

```r
present <- out[!is.na(out)]
is_whole <- length(present) == 0 || (all(is.finite(present)) &&
  all(present == trunc(present)) && all(abs(present) <= .Machine$integer.max))
if (is_whole) out <- as.integer(out)
```

- The storage type is decided from the values in hand.
- The in-memory engine calls this once per column, from
  `dta_coerce_table_to_specs()` (about 480–589, call at about 510).
- The streaming engine calls the same function once per **batch**, at
  `R/streamingValidation.R` about 957–960.
- So the two can disagree (DEV-004). A value outside the int32 range just keeps
  the column double and records no issue (DEV-006).
- The column-spec check `R/columnSpecChecks.R` (about 139–170, line ~154) tests
  only `is.finite(x) & x == trunc(x)`, so 5000000000 passes.

**Fix (D1, recommended).** In `dta_coerce_column()`, integer target:

1. Flag `bad <- !is.na(out) & (!is.finite(out) | out != trunc(out) | abs(out) > .Machine$integer.max)`.
2. Set `out[bad] <- NA`, then **always** `out <- as.integer(out)`.
3. Return the offending indices as the union of `which(converted$unconvertible)`
   and `which(bad)`. Add a parallel `reason` vector to the returned list:
   `"not_convertible"` for unparseable values, `"not_whole"` for fractional or
   non-finite values, and `"out_of_range"` for values outside int32.
4. In `dta_coerce_table_to_specs()`, the per-column issue frame (about
   541–548) hard-codes `reason = "not_convertible"`. Use the returned vector.
5. In `dta_import_error_messages()` (`R/validationReporting.R`, about 285–295),
   add package-authored messages for the two new reasons. Keep the existing
   text for `not_convertible` unchanged.
6. Rewrite the roxygen paragraph above `dta_coerce_column()` (about 360–367,
   "a fractional value stays a double and is left to the column spec axis").
   It must describe the new contract: the declaration decides storage; a value
   that cannot be stored as declared is an import error, with its raw text kept.
7. `R/columnSpecChecks.R` (~line 154): add `abs(x) <= .Machine$integer.max` to
   the elementwise integer check. This covers R-built double columns, which never
   reach `dta_coerce_column()` because of its early `is.numeric()` return.

Both engines call the one function and each cell is now judged alone, so
batching can no longer change the result.

**Arrow fast path.**
- `dta_arrow_parse_numeric_batch()` (`R/importConversion.R` about 703–827) casts a
  batch to int32 only when it matches `DTA_ARROW_INTEGER_PATTERN` (at most nine
  digits).
- Other batches fall through to the R path, which now also yields integer.
- Do not change the pattern. Do prove agreement: extend the Arrow-vs-R parity
  tests in `tests/testthat/test-importConversion.R` (about 1105–1150) with the
  cases listed below.

**Check before finishing.** Grep `inst/shiny/dta_app` and `vignettes/` for
anything that relies on a fractional value staying double, or that switches on
`reason == "not_convertible"`. Report what you find. Do not edit the app; it is
not owned here.

## Files owned

- `R/DTAColumnSpecStructureSAS-class.R`, `R/importConversion.R`,
  `R/columnSpecChecks.R`, and in `R/validationReporting.R` the function
  `dta_import_error_messages()` only. WP2 edits other functions in that file.
- `tests/testthat/test-DTAColumnSpecStructureSAS.R`,
  `tests/testthat/test-importConversion.R`,
  `tests/testthat/test-streaming-parity.R`.
- `inst/qualification/tests/test-OQ-TYPE-declared.R`,
  `inst/qualification/tests/test-OQ-CORE-runner.R`,
  `inst/qualification/tests/helper-parity.R`.
- `inst/qualification/requirements/REQ-TYPE.yaml`, `REQ-CORE.yaml` (only if the
  CORE-006 requirement text needs it).
- Your entries in `inst/qualification/deviations.yaml` (DEV-001, DEV-004,
  DEV-006), your bullets in `CHANGELOG.md`, and the regenerated manifest.

## Tests: flip, add

**Developer suite (`tests/testthat`).**
- `test-DTAColumnSpecStructureSAS.R` about 24–35: remove the
  "KNOWN DEFECT (deferred, needs an R/ fix)" comment and assert `BEST12.` gives
  `"Num"`.
  - Add: `as_json_schema_type()` gives `"number"`.
  - Add: an explicit `type = "Int", format = "BEST8."` is still accepted and stays Int.
  - Keep the test at about line 7.
- `test-importConversion.R` about 136–156 asserts a fractional value stays double
  with no issues. Rewrite it to the new contract: integer class, NA at that row,
  one issue with `reason = "not_whole"` and the original raw text.
- Add cases for `"2147483647"` (stays as is), `"2147483648"`, `"-2147483648"`
  (note: `NA_integer_` occupies the int32 minimum, so this is out of range),
  `"5000000000"`, `"Inf"`, `"1.0"` (whole, so `1L`) and `"8.5"`. For each, check
  value, class and reason.
- Arrow-vs-R parity (about 1105–1150): add a column whose batches differ. One
  batch is all nine-digit whole values (Arrow route); another holds `8.5` or
  `2147483648` (R route). Assert identical class and identical import-issue
  rows from both routes.
- `test-streaming-parity.R` about 340–346 has a comment saying the generator
  avoids this case. Remove the avoidance: let the randomised generator place
  fractional and out-of-range values in a declared Int column in some batches
  only. That is the real proof for DEV-004.

**Qualification suite (`inst/qualification/tests`).**
- `test-OQ-TYPE-declared.R`, OQ-TYPE-013 (about 160–167): retitle it to "a
  BESTw. format infers Num" and flip the expectation.
- OQ-TYPE-024 (about 345–380):
  - Remove `qa_known_deviation("DEV-006", ...)`.
  - `2147483648` and `-2147483648` are now import errors: `import = 1`,
    `ok = FALSE`, class integer, value NA.
  - Keep a `qa_step` showing `2147483647` is accepted clean.
- OQ-TYPE-025 (about 382–433): remove `qa_known_deviation("DEV-004", ...)`.
  Assert whole-column and batched typing give identical class **and**
  identical import counts for a column fractional in one batch only.
- OQ-TYPE-022 (about 285–313) pins the integer64 `5e9` rule-error count. Re-run
  it. If its expected values change, the requirement changed: update the
  requirement text first, then the test, and report it.
- `helper-parity.R` about 281–288: update the comment, and extend the generator
  as in testthat if that file has its own.
- `test-OQ-CORE-runner.R`, OQ-CORE-006 (about 256–272) uses DEV-001 as its live
  example. Rewrite it to exercise `qa_known_deviation()` against a **fixture
  register** with a synthetic open entry:
  - a `TRUE` observation is recorded as reproduced;
  - a `FALSE` observation is recorded as a failure, captured the same way the
    M10 example earlier in that file captures a failing step;
  - an unknown id still aborts.

  Read `qa_known_deviation()` in `inst/qualification/tests/helper-00-qa.R` first.
  **If it cannot be pointed at a fixture register without changing the helper's
  contract, stop and report. Do not bind another real DEV.**

**Requirements.** In `REQ-TYPE.yaml`, find every requirement mentioning DEV-001,
DEV-004 or DEV-006, or carrying "Pinned, not endorsed". Rewrite the `text:` to
the new contract first, then make the tests match it. Expect REQ-TYPE-001, -002,
-009 and the BESTw. inference requirement (line ~26). Any requirement describing
"fractional stays double" changes under D1.

**Register.** DEV-001, DEV-004, DEV-006: set `status: closed` and
`closed: <date of your run>`.

**CHANGELOG** (`### Fixed`), three bullets that say what went wrong and why it
mattered:
- the integer-range acceptance;
- the engine disagreement;
- `BESTw.` inference.

Add the behaviour change: a fractional value in a declared Int column is now an
import error and becomes NA in the typed table.

## Verification

```bash
Rscript --no-init-file -e "devtools::test(filter = 'importConversion|DTAColumnSpecStructureSAS|streaming-parity|columnSpec')"
Rscript --no-init-file -e "devtools::test()"
Rscript --no-init-file -e "pkgload::load_all(quiet = TRUE); r <- DTAtools::run_qualification(tempfile('q'), stages = 'OQ', scale = 'quick', formats = 'md', filter = 'TYPE|CORE|STREAM|VAL', quiet = TRUE); print(summary(r))"
Rscript --no-init-file -e "devtools::test(filter = 'qualification')"
Rscript .github/scripts/sync_qualification_manifest.R --check
```

Report the full `devtools::test()` counts before and after, and every test
whose expectation you changed and why.
