Sys.setenv(NOT_CRAN = "true")

# Coverage for R/validateTemplate.R -- the exported, package-namespace
# validate_template(), which re-uses (never re-implements) the Shiny app's own
# template engine (inst/shiny/dta_app/R/*.R) via a private, memoised
# environment (.dta_template_engine(), the same technique
# tests/testthat/helper-shinyapp.R's app_env() uses to reach that code from a
# test). See that file's header comment for why the app's helper files are not
# directly callable from package code.

# ---- Fixture helpers --------------------------------------------------------

# A minimal, fully valid, buildable creation template: zero issues expected
# from ANY check. Parameterised by filename/id/version so the same shape can
# be reused (with a different id, or a deliberately colliding one) across
# several tests without duplicating the whole YAML body each time.
write_clean_template <- function(dir, filename = "clean.dta-template.yaml",
                                 id = "clean_tpl", version_line = "version: \"1.0\"") {
  writeLines(
    c(
      "kind: dta_creation_template",
      sprintf("id: %s", id),
      version_line,
      "base:",
      "  metadata:",
      "    title: Clean Template",
      "datasets:",
      "  - name: ds1",
      "    type: tabular",
      "    files: {filename: x.csv, type: csv}",
      "    columns:",
      "      - {id: COL1, type: SAS Char}"
    ),
    file.path(dir, filename)
  )
}

# Every row of `result` for `code`, asserted non-empty and (when `severity` is
# given) uniformly of that severity -- the one assertion shape almost every
# test below needs.
expect_code_row <- function(result, code, severity = NULL) {
  rows <- result[result$code == code, , drop = FALSE]
  testthat::expect_gt(nrow(rows), 0)
  if (!is.null(severity)) {
    testthat::expect_true(all(rows$severity == severity))
  }
  rows
}

expect_no_code_row <- function(result, code) {
  rows <- result[result$code == code, , drop = FALSE]
  testthat::expect_equal(nrow(rows), 0)
}

# ---- Argument validation ----------------------------------------------------

test_that("validate_template() aborts for a path that does not exist", {
  expect_error(
    validate_template(file.path(withr::local_tempdir(), "nope")),
    class = "rlang_error"
  )
})

test_that("validate_template() aborts for a non-scalar/empty path", {
  expect_error(validate_template(character(0)), class = "rlang_error")
  expect_error(validate_template(NA_character_), class = "rlang_error")
  expect_error(validate_template(c("a", "b")), class = "rlang_error")
})

test_that("validate_template() aborts for a non-scalar-logical strict", {
  dir <- withr::local_tempdir()
  expect_error(validate_template(dir, strict = "yes"), class = "rlang_error")
  expect_error(validate_template(dir, strict = NA), class = "rlang_error")
})

test_that("validate_template() aborts for an unknown kind in `kinds`", {
  dir <- withr::local_tempdir()
  expect_error(validate_template(dir, kinds = "not_a_kind"), class = "rlang_error")
})

# ---- Clean directory --------------------------------------------------------

test_that("a directory containing only a fully valid template returns zero rows", {
  dir <- withr::local_tempdir()
  write_clean_template(dir)

  result <- validate_template(dir)

  expect_s3_class(result, "data.frame")
  expect_equal(
    names(result),
    c("file", "kind", "id", "version", "severity", "code", "message")
  )
  expect_equal(nrow(result), 0)
})

test_that("strict = TRUE stays silent on a clean directory", {
  dir <- withr::local_tempdir()
  write_clean_template(dir)

  expect_no_error(validate_template(dir, strict = TRUE))
})

# ---- kind_unknown ------------------------------------------------------------

test_that("kind_unknown fires for a kind that is not one of the four known values", {
  dir <- withr::local_tempdir()
  writeLines(
    c("kind: not_a_real_kind", "id: foo", "version: \"1.0\""),
    file.path(dir, "bad_kind.dta-template.yaml")
  )

  result <- validate_template(dir)
  expect_code_row(result, "kind_unknown", "warning")
})

# ---- id_missing / version_missing -------------------------------------------

test_that("id_missing fires when 'id' is absent", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_creation_template", "version: \"1.0\"",
      "base: {metadata: {}}", "datasets: []"
    ),
    file.path(dir, "no_id.dta-template.yaml")
  )

  result <- validate_template(dir)
  expect_code_row(result, "id_missing", "warning")
})

test_that("version_missing fires when 'version' is absent", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_creation_template", "id: foo",
      "base: {metadata: {}}", "datasets: []"
    ),
    file.path(dir, "no_version.dta-template.yaml")
  )

  result <- validate_template(dir)
  expect_code_row(result, "version_missing", "warning")
})

# ---- version_unquoted / version_unparseable ---------------------------------

test_that("version_unquoted fires on an unquoted numeric version but not on a quoted one", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_creation_template", "id: unquoted_tpl", "version: 1.0",
      "base: {metadata: {}}", "datasets: []"
    ),
    file.path(dir, "unquoted.dta-template.yaml")
  )
  writeLines(
    c(
      "kind: dta_creation_template", "id: quoted_tpl", "version: \"1.0\"",
      "base: {metadata: {}}", "datasets: []"
    ),
    file.path(dir, "quoted.dta-template.yaml")
  )

  result <- validate_template(dir)

  unquoted_rows <- expect_code_row(result, "version_unquoted", "error")
  expect_equal(unquoted_rows$file, file.path(dir, "unquoted.dta-template.yaml"))
  expect_false("quoted.dta-template.yaml" %in% basename(unquoted_rows$file))
})

test_that("version_unparseable fires for a version that does not parse as numeric_version()", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_creation_template", "id: foo", "version: \"not-a-version!!\"",
      "base: {metadata: {}}", "datasets: []"
    ),
    file.path(dir, "bad_version.dta-template.yaml")
  )

  result <- validate_template(dir)
  expect_code_row(result, "version_unparseable", "warning")
})

# ---- duplicate_id_version ----------------------------------------------------

test_that("duplicate_id_version fires when two files share kind + id + version", {
  dir <- withr::local_tempdir()
  write_clean_template(dir, filename = "a.dta-template.yaml", id = "dup_tpl")
  write_clean_template(dir, filename = "b.dta-template.yaml", id = "dup_tpl")

  result <- validate_template(dir)
  expect_code_row(result, "duplicate_id_version", "warning")
})

# ---- target_invalid / target_machine_owned ----------------------------------

test_that("target_invalid fires for an option target that is not a real metadata field", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: bad_target",
      "version: \"1.0\"",
      "abstract: true",
      "base: {metadata: {}}",
      "datasets: []",
      "options:",
      "  - id: opt1",
      "    type: text",
      "    target: metadata.not_a_real_field"
    ),
    file.path(dir, "bad_target.dta-template.yaml")
  )

  result <- validate_template(dir)
  expect_code_row(result, "target_invalid", "warning")
})

test_that("target_machine_owned fires for a target naming a machine-owned metadata field", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: forged_provenance",
      "version: \"1.0\"",
      "abstract: true",
      "base: {metadata: {}}",
      "datasets: []",
      "options:",
      "  - id: opt1",
      "    type: text",
      "    target: metadata.template"
    ),
    file.path(dir, "forged.dta-template.yaml")
  )

  result <- validate_template(dir)
  expect_code_row(result, "target_machine_owned", "error")
})

test_that("target_machine_owned also fires when the forged target is hidden in a non-default effects branch", {
  # This is the entire reason target_machine_owned is its own static check
  # rather than relying on the instantiate dry-run alone: a dry run using
  # DEFAULT selections never chooses the "yes" branch of a boolean option
  # whose default is FALSE, so a forged target hidden there would otherwise
  # ship undetected.
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: forged_hidden",
      "version: \"1.0\"",
      "abstract: true",
      "base: {metadata: {}}",
      "datasets: []",
      "options:",
      "  - id: sneaky",
      "    type: boolean",
      "    default: false",
      "    effects:",
      "      \"yes\":",
      "        - path: metadata.import_issues",
      "          value: []"
    ),
    file.path(dir, "forged_hidden.dta-template.yaml")
  )

  result <- validate_template(dir)
  expect_code_row(result, "target_machine_owned", "error")
})

# ---- extends_unresolved / extends_cycle -------------------------------------

test_that("extends_unresolved fires when 'extends:' names a template not in this directory", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: child_tpl",
      "version: \"1.0\"",
      "abstract: true",
      "extends: nonexistent_parent",
      "base: {metadata: {}}",
      # A non-empty (if bare) `datasets:` list: read_dta_creation_template()
      # requires at least one entry regardless of `abstract:`, and this file
      # is never actually built (abstract templates skip the instantiate
      # dry-run), so the entry's own content never has to be valid.
      "datasets:",
      "  - name: placeholder"
    ),
    file.path(dir, "child.dta-template.yaml")
  )

  result <- validate_template(dir)
  expect_code_row(result, "extends_unresolved", "warning")
})

test_that("extends_cycle fires for a two-node extends cycle", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_creation_template", "id: cyc_a", "version: \"1.0\"",
      "abstract: true", "extends: cyc_b", "base: {metadata: {}}",
      "datasets:", "  - name: placeholder"
    ),
    file.path(dir, "a.dta-template.yaml")
  )
  writeLines(
    c(
      "kind: dta_creation_template", "id: cyc_b", "version: \"1.0\"",
      "abstract: true", "extends: cyc_a", "base: {metadata: {}}",
      "datasets:", "  - name: placeholder"
    ),
    file.path(dir, "b.dta-template.yaml")
  )

  result <- validate_template(dir)
  expect_code_row(result, "extends_cycle", "warning")
})

# ---- dataset_template_unresolved / patch_incoherent -------------------------

test_that("dataset_template_unresolved fires for a datasets[].template ref not in this directory", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: uses_missing_ds_tpl",
      "version: \"1.0\"",
      "abstract: true",
      "base: {metadata: {}}",
      "datasets:",
      "  - template: no_such_dataset_tpl@1.0"
    ),
    file.path(dir, "uses_missing.dta-template.yaml")
  )

  result <- validate_template(dir)
  expect_code_row(result, "dataset_template_unresolved", "warning")
})

test_that("patch_incoherent fires when remove_columns names an absent column id", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_dataset_template",
      "id: ds_tpl",
      "version: \"1.0\"",
      "dataset:",
      "  name: ds1",
      "  type: tabular",
      "  files: {filename: x.csv, type: csv}",
      "  columns:",
      "    - {id: COL1, type: SAS Char}"
    ),
    file.path(dir, "ds_tpl.dta-dataset-template.yaml")
  )
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: uses_patch",
      "version: \"1.0\"",
      "abstract: true",
      "base: {metadata: {}}",
      "datasets:",
      "  - template: ds_tpl@1.0",
      "    patch:",
      "      remove_columns: [NOPE_NOT_A_COLUMN]"
    ),
    file.path(dir, "uses_patch.dta-template.yaml")
  )

  result <- validate_template(dir)
  expect_code_row(result, "patch_incoherent", "warning")
})

test_that("a coherent template:/patch: dataset entry does not fire patch_incoherent or instantiate_failed", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_dataset_template",
      "id: ds_tpl_ok",
      "version: \"1.0\"",
      "dataset:",
      "  name: ds1",
      "  type: tabular",
      "  files: {filename: x.csv, type: csv}",
      "  columns:",
      "    - {id: COL1, type: SAS Char}",
      "    - {id: COL2, type: SAS Char}"
    ),
    file.path(dir, "ds_tpl_ok.dta-dataset-template.yaml")
  )
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: uses_patch_ok",
      "version: \"1.0\"",
      "base: {metadata: {}}",
      "datasets:",
      "  - template: ds_tpl_ok@1.0",
      "    patch:",
      "      remove_columns: [COL2]"
    ),
    file.path(dir, "uses_patch_ok.dta-template.yaml")
  )

  result <- validate_template(dir)
  expect_no_code_row(result, "patch_incoherent")
  expect_no_code_row(result, "instantiate_failed")
})

# ---- dataset_missing: def$dataset / def$datasets partial-matching guard ----

test_that("dataset_missing fires, as an error, when a dta_dataset_template carries 'datasets:' instead of 'dataset:'", {
  # PINNED BUG: .dta_template_check_dataset_tpl() used to read def$dataset,
  # and R's `$` on a list falls back to partial matching. 'dataset' is a
  # strict prefix of 'datasets' -- a DIFFERENT, equally legitimate top-level
  # key (a dta_creation_template's own datasets: array) -- so a
  # dta_dataset_template file that was slipped a 'datasets:' array instead
  # (the obvious mistake when converting one kind of template into the other)
  # had def$dataset silently return that array, and every check downstream
  # walked it as if it were the real dataset body: zero issues reported for a
  # file with no usable dataset at all. Fixed by reading def[["dataset"]],
  # which matches exactly, and reporting the absence explicitly.
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_dataset_template",
      "id: slipped",
      "version: \"1.0\"",
      "datasets:",
      "  - name: one",
      "    type: tabular",
      "    files:",
      "      filename: one.tsv",
      "    columns:",
      "      - id: C1",
      "        type: SAS Char"
    ),
    file.path(dir, "slipped.dta-dataset-template.yaml")
  )

  result <- validate_template(dir)
  rows <- expect_code_row(result, "dataset_missing", "error")
  # The specific hint distinguishing "absent entirely" from "the sibling
  # plural key was used instead" -- the exact, actionable diagnosis for the
  # mistake this test pins.
  expect_match(rows$message, "datasets:", fixed = TRUE)
})

test_that("a well-formed dta_dataset_template with a proper 'dataset:' body validates clean", {
  # The positive control for the guard above: dataset_missing must not fire
  # (or anything else) for a template that correctly uses the singular key,
  # so the new check cannot be satisfied by rejecting every dataset template.
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_dataset_template",
      "id: has_dataset_body",
      "version: \"1.0\"",
      "dataset:",
      "  name: ds1",
      "  type: tabular",
      "  files: {filename: x.csv, type: csv}",
      "  columns:",
      "    - {id: COL1, type: SAS Char}"
    ),
    file.path(dir, "has_dataset_body.dta-dataset-template.yaml")
  )

  result <- validate_template(dir)
  expect_equal(nrow(result), 0, info = paste(utils::capture.output(print(result)), collapse = "\n"))
})

test_that("a dta_creation_template's own 'datasets:' key is correct and must not trip dataset_missing", {
  # The guard against over-firing: 'datasets:' (plural) is the CORRECT,
  # documented key for a dta_creation_template -- dataset_missing is scoped to
  # dta_dataset_template's .dta_template_check_dataset_tpl() only, and must
  # never fire for the kind where 'datasets:' genuinely belongs.
  dir <- withr::local_tempdir()
  write_clean_template(dir)

  result <- validate_template(dir)
  expect_no_code_row(result, "dataset_missing")
  expect_equal(nrow(result), 0, info = paste(utils::capture.output(print(result)), collapse = "\n"))
})

# ---- party_slot_invalid ------------------------------------------------------

test_that("party_slot_invalid fires for a slot target that is not metadata.supplier/receiver", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: bad_slot_target",
      "version: \"1.0\"",
      "abstract: true",
      "base: {metadata: {}}",
      "datasets: []",
      "party_slots:",
      "  - id: supplier_choice",
      "    target: metadata.not_a_party_target"
    ),
    file.path(dir, "bad_slot.dta-template.yaml")
  )

  result <- validate_template(dir)
  expect_code_row(result, "party_slot_invalid", "warning")
})

test_that("party_slot_invalid fires for a named profile id that does not exist", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: bad_slot_profile",
      "version: \"1.0\"",
      "abstract: true",
      "base: {metadata: {}}",
      "datasets: []",
      "party_slots:",
      "  - id: supplier_choice",
      "    target: metadata.supplier",
      "    profiles: [not_a_real_profile]"
    ),
    file.path(dir, "bad_slot_profile.dta-template.yaml")
  )

  result <- validate_template(dir)
  expect_code_row(result, "party_slot_invalid", "warning")
})

# ---- instantiate_failed ------------------------------------------------------

test_that("instantiate_failed fires when a non-abstract template cannot actually be built", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: broken_build",
      "version: \"1.0\"",
      "base: {metadata: {}}",
      "datasets:",
      "  - no_such_dataset_file.yaml"
    ),
    file.path(dir, "broken_build.dta-template.yaml")
  )

  result <- validate_template(dir)
  expect_code_row(result, "instantiate_failed", "error")
})

test_that("instantiate_failed does not fire for an abstract template that cannot itself be built", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_creation_template",
      "id: broken_abstract_base",
      "version: \"1.0\"",
      "abstract: true",
      "base: {metadata: {}}",
      "datasets:",
      "  - no_such_dataset_file.yaml"
    ),
    file.path(dir, "broken_abstract.dta-template.yaml")
  )

  result <- validate_template(dir)
  expect_no_code_row(result, "instantiate_failed")
})

# ---- parse_failed -------------------------------------------------------------

test_that("a malformed file yields one parse_failed error row, and other files are still validated", {
  dir <- withr::local_tempdir()
  # A leading tab is invalid YAML indentation and reliably fails to parse.
  writeLines(
    c("kind: dta_creation_template", "\tid: bad"),
    file.path(dir, "malformed.dta-template.yaml")
  )
  write_clean_template(dir, filename = "clean.dta-template.yaml", id = "still_checked")

  result <- validate_template(dir)

  malformed_rows <- result[result$file == file.path(dir, "malformed.dta-template.yaml"), , drop = FALSE]
  expect_equal(nrow(malformed_rows), 1)
  expect_equal(malformed_rows$code, "parse_failed")
  expect_equal(malformed_rows$severity, "error")

  # The clean sibling file is unaffected: no rows of its own at all.
  clean_rows <- result[result$file == file.path(dir, "clean.dta-template.yaml"), , drop = FALSE]
  expect_equal(nrow(clean_rows), 0)
})

# ---- strict = TRUE ------------------------------------------------------------

test_that("strict = TRUE aborts, summarising the error count, when any row is severity 'error'", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_creation_template", "id: unquoted_tpl", "version: 1.0",
      "base: {metadata: {}}", "datasets: []"
    ),
    file.path(dir, "unquoted.dta-template.yaml")
  )

  expect_error(
    validate_template(dir, strict = TRUE),
    regexp = "Template validation found",
    class = "rlang_error"
  )
})

# ---- kinds filter -------------------------------------------------------------

test_that("kinds restricts which files get their own report row but not cross-file resolution", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_dataset_template", "id: ds_tpl_k", "version: \"1.0\"",
      "dataset:",
      "  name: ds1", "  type: tabular", "  files: {filename: x.csv, type: csv}",
      "  columns:", "    - {id: COL1, type: SAS Char}"
    ),
    file.path(dir, "ds_tpl_k.dta-dataset-template.yaml")
  )
  writeLines(
    c(
      "kind: dta_creation_template", "id: uses_ds_tpl_k", "version: \"1.0\"",
      "base: {metadata: {}}",
      "datasets:", "  - template: ds_tpl_k@1.0"
    ),
    file.path(dir, "uses_ds_tpl_k.dta-template.yaml")
  )

  result <- validate_template(dir, kinds = "dta_creation_template")

  # The dataset template file itself is never reported on (excluded by
  # `kinds`), but its OWN presence still resolves the creation template's
  # datasets[].template ref cleanly -- no false dataset_template_unresolved.
  expect_false(any(grepl("ds_tpl_k[.]dta-dataset-template", result$file)))
  expect_no_code_row(result, "dataset_template_unresolved")
  expect_no_code_row(result, "instantiate_failed")
})

# ---- Controlled vocabularies ------------------------------------------------

# A valid `dta_vocabulary` file. Parameterised the same way
# write_clean_template() is, for the same reason.
write_vocab_file <- function(dir, id = "visit", filename = NULL,
                             body = c("terms:", "  - code: SCR", "  - code: EOT")) {
  if (is.null(filename)) {
    filename <- paste0(id, ".dta-vocabulary.yaml")
  }
  writeLines(
    c(
      "kind: dta_vocabulary",
      sprintf("id: %s", id),
      "version: \"1.0\"",
      "label: Visits",
      body
    ),
    file.path(dir, filename)
  )
}

test_that("a valid vocabulary file yields zero rows", {
  dir <- withr::local_tempdir()
  write_vocab_file(dir)

  result <- validate_template(dir)
  expect_equal(nrow(result), 0, info = paste(utils::capture.output(print(result)), collapse = "\n"))
})

test_that("vocabulary_invalid fires, as an error, for a vocabulary that will not read", {
  dir <- withr::local_tempdir()
  # A declared type its codes cannot satisfy. Severity is "error", not
  # "warning": a vocabulary that will not read contributes no terms at all, so
  # any column bound to it would end up silently unvalidated.
  writeLines(
    c(
      "kind: dta_vocabulary", "id: broken", "version: \"1.0\"",
      "type: categorical",
      "terms:", "  - code: SCR"
    ),
    file.path(dir, "broken.dta-vocabulary.yaml")
  )

  result <- validate_template(dir)
  expect_code_row(result, "vocabulary_invalid", "error")
})

test_that("vocabulary_unresolved fires when extends: names a vocabulary not in this directory", {
  dir <- withr::local_tempdir()
  write_vocab_file(
    dir,
    id = "child",
    body = c("extends: nosuch@1.0", "add_terms: [X]")
  )

  result <- validate_template(dir)
  expect_code_row(result, "vocabulary_unresolved", "warning")
})

test_that("a resolvable extends: chain does not fire either vocabulary code", {
  dir <- withr::local_tempdir()
  write_vocab_file(dir, id = "visit")
  write_vocab_file(
    dir,
    id = "visit_onc",
    body = c("extends: visit@1.0", "add_terms: [C2D1]", "remove_terms: [EOT]")
  )

  result <- validate_template(dir)
  expect_no_code_row(result, "vocabulary_unresolved")
  expect_no_code_row(result, "vocabulary_invalid")
  expect_no_code_row(result, "vocabulary_extends_failed")
})

test_that("vocabulary_extends_failed fires, as an error, for a two-node cycle", {
  dir <- withr::local_tempdir()
  write_vocab_file(dir, id = "ping", body = c("extends: pong@1.0", "add_terms: [P]"))
  write_vocab_file(dir, id = "pong", body = c("extends: ping@1.0", "add_terms: [Q]"))

  result <- validate_template(dir)
  expect_code_row(result, "vocabulary_extends_failed", "error")
})

test_that("kinds = 'dta_vocabulary' reports only vocabulary files", {
  dir <- withr::local_tempdir()
  write_vocab_file(dir, id = "broken", body = c("type: categorical", "terms:", "  - code: X"))
  # A creation template with its own, unrelated problem: it must not be
  # reported when `kinds` excludes it.
  writeLines(
    c("kind: dta_creation_template", "id: nover", "datasets: []"),
    file.path(dir, "nover.dta-template.yaml")
  )

  result <- validate_template(dir, kinds = "dta_vocabulary")
  expect_true(all(result$kind == "dta_vocabulary"))
  expect_code_row(result, "vocabulary_invalid", "error")
})

# ---- Column bindings (values_from:) -----------------------------------------

# A dataset template whose single column carries whatever binding the test is
# exercising.
write_bound_dataset_tpl <- function(dir, col_lines, filename = "bound.dta-dataset-template.yaml") {
  writeLines(
    c(
      "kind: dta_dataset_template",
      "id: bound_ds",
      "version: \"1.0\"",
      "dataset:",
      "  name: ds1",
      "  type: tabular",
      "  files: {filename: x.csv, type: csv}",
      "  columns:",
      col_lines
    ),
    file.path(dir, filename)
  )
}

test_that("a resolvable binding yields no binding rows", {
  dir <- withr::local_tempdir()
  write_vocab_file(dir)
  write_bound_dataset_tpl(dir, c(
    "    - id: VISIT",
    "      type: SAS Char",
    "      values_from: visit@1.0"
  ))

  result <- validate_template(dir)
  expect_no_code_row(result, "values_from_unresolved")
  expect_no_code_row(result, "values_from_invalid")
  expect_no_code_row(result, "values_from_terms_invalid")
})

test_that("an include naming a code the vocabulary INHERITED is accepted", {
  dir <- withr::local_tempdir()
  write_vocab_file(dir, id = "visit")
  write_vocab_file(
    dir,
    id = "visit_onc",
    body = c("extends: visit@1.0", "add_terms: [C2D1]")
  )
  write_bound_dataset_tpl(dir, c(
    "    - id: VISIT",
    "      type: SAS Char",
    "      values_from:",
    "        vocabulary: visit_onc@1.0",
    "        include: [SCR, C2D1]"
  ))

  # "SCR" comes from the PARENT. A checker that read the child's own `terms:`
  # without resolving `extends:` first would report it as an unknown code and
  # fail a perfectly valid template.
  result <- validate_template(dir)
  expect_no_code_row(result, "values_from_terms_invalid")
  expect_no_code_row(result, "values_from_unresolved")
})

test_that("values_from_unresolved fires for a vocabulary not in this directory", {
  dir <- withr::local_tempdir()
  write_bound_dataset_tpl(dir, c(
    "    - id: VISIT",
    "      type: SAS Char",
    "      values_from: nosuch@1.0"
  ))

  result <- validate_template(dir)
  expect_code_row(result, "values_from_unresolved", "warning")
})

test_that("values_from_terms_invalid fires, as an error, for an include naming an unknown code", {
  dir <- withr::local_tempdir()
  write_vocab_file(dir)
  write_bound_dataset_tpl(dir, c(
    "    - id: VISIT",
    "      type: SAS Char",
    "      values_from:",
    "        vocabulary: visit@1.0",
    "        include: [SCR, SCRN]"
  ))

  # "SCRN" is the exact shape a typo for "SCR" takes. Silently dropping it
  # would leave a column whose permitted values quietly omit a visit, and that
  # only surfaces much later, against real data.
  result <- validate_template(dir)
  rows <- expect_code_row(result, "values_from_terms_invalid", "error")
  expect_match(paste(rows$message, collapse = " "), "SCRN", fixed = TRUE)
})

test_that("values_from_pattern fires, as an error, when a bound column also sets a pattern", {
  dir <- withr::local_tempdir()
  write_vocab_file(dir)
  write_bound_dataset_tpl(dir, c(
    "    - id: VISIT",
    "      type: SAS Char",
    "      pattern: \"^S\"",
    "      values_from: visit@1.0"
  ))

  result <- validate_template(dir)
  expect_code_row(result, "values_from_pattern", "error")
})

test_that("values_and_values_from warns when one literal column authors both", {
  dir <- withr::local_tempdir()
  write_vocab_file(dir)
  write_bound_dataset_tpl(dir, c(
    "    - id: VISIT",
    "      type: SAS Char",
    "      values: [OLD]",
    "      values_from: visit@1.0"
  ))

  # A warning, not an error: the build stays deterministic (the binding wins).
  # This is only detectable per-file, where the raw YAML shows both in ONE
  # authored column -- by build time a base `values:` and a patch's
  # `values_from:` are indistinguishable, and that combination is legitimate.
  result <- validate_template(dir)
  expect_code_row(result, "values_and_values_from", "warning")
})

# ---- values_and_values_from: partial-matching regression guard -------------

test_that("a column authoring only values_from: produces no values_and_values_from row", {
  # PINNED BUG: the check used to read col$values, and R's `$` on a list
  # falls back to partial matching -- so on a column that authored ONLY
  # `values_from:`, col$values returned that same value instead of NULL, and
  # this fired for every column using a binding correctly (6 times on this
  # package's own shipped templates). Fixed by reading col[["values"]], which
  # matches exactly. Written alongside the positive case above so a
  # regression that deleted the check entirely -- not just broke its
  # accessor -- would also be caught.
  dir <- withr::local_tempdir()
  write_vocab_file(dir)
  write_bound_dataset_tpl(dir, c(
    "    - id: VISIT",
    "      type: SAS Char",
    "      values_from: visit@1.0"
  ))

  result <- validate_template(dir)
  expect_no_code_row(result, "values_and_values_from")
})

test_that("values_from_invalid fires for a malformed binding", {
  dir <- withr::local_tempdir()
  write_vocab_file(dir)
  write_bound_dataset_tpl(dir, c(
    "    - id: VISIT",
    "      type: SAS Char",
    "      values_from:",
    "        vocabulary: visit@1.0",
    "        field: decode"
  ))

  result <- validate_template(dir)
  expect_code_row(result, "values_from_invalid", "error")
})

# ---- no_templates -------------------------------------------------------------

test_that("no_templates fires for an empty directory, naming the suffixes and the non-recursive scan", {
  dir <- withr::local_tempdir()

  result <- validate_template(dir)

  # Exactly one row: an empty directory has nothing else to report on.
  expect_equal(nrow(result), 1)
  rows <- expect_code_row(result, "no_templates", "error")
  expect_match(rows$message, "dta-template.yaml", fixed = TRUE)
  expect_match(rows$message, "dta-dataset-template.yaml", fixed = TRUE)
  expect_match(rows$message, "dta-party.yaml", fixed = TRUE)
  expect_match(rows$message, "dta-vocabulary.yaml", fixed = TRUE)
  expect_match(rows$message, "non-recursively", fixed = TRUE)
})

test_that("strict = TRUE aborts on an empty directory instead of passing silently", {
  # The regression guard for the CI false-negative no_templates exists to
  # catch: before this check existed, a directory that scanned to zero rows
  # sailed through strict = TRUE, so a CI job pointed at a typo'd or stale
  # path went green having validated nothing.
  dir <- withr::local_tempdir()
  expect_error(validate_template(dir, strict = TRUE), class = "rlang_error")
})

test_that("no_templates does not fire when path names a single file", {
  # A single explicitly named file is never "empty" by definition.
  dir <- withr::local_tempdir()
  write_clean_template(dir)

  result <- validate_template(file.path(dir, "clean.dta-template.yaml"))
  expect_no_code_row(result, "no_templates")
})

test_that("no_templates does not fire when kinds filters out every file present", {
  # no_templates is about what is IN the directory, not what was SELECTED for
  # reporting -- a directory holding only vocabularies, validated with
  # kinds = "dta_creation_template", has nothing to REPORT, not nothing IN IT.
  dir <- withr::local_tempdir()
  write_vocab_file(dir)

  result <- validate_template(dir, kinds = "dta_creation_template")
  expect_no_code_row(result, "no_templates")
})

# ---- template_in_subdirectory --------------------------------------------------

test_that("template_in_subdirectory fires, as a warning, for a template kept one level down", {
  dir <- withr::local_tempdir()
  sub <- file.path(dir, "by-study")
  dir.create(sub)
  write_clean_template(sub, filename = "nested.dta-template.yaml", id = "nested_tpl")

  result <- validate_template(dir)
  rows <- expect_code_row(result, "template_in_subdirectory", "warning")
  expect_match(rows$file, "nested.dta-template.yaml", fixed = TRUE)
})

test_that("strict = TRUE stays silent when the only OTHER issue is a nested template", {
  # A directory holding ONLY a nested template also trips no_templates (its
  # top-level scan is empty), so this needs a valid top-level template too --
  # otherwise it would not be testing template_in_subdirectory "alone".
  # template_in_subdirectory itself must not turn CI red on its own: keeping
  # an archived or work-in-progress file below the root is a legitimate
  # choice.
  dir <- withr::local_tempdir()
  write_clean_template(dir, filename = "top.dta-template.yaml", id = "top_tpl")
  sub <- file.path(dir, "by-study")
  dir.create(sub)
  write_clean_template(sub, filename = "nested.dta-template.yaml", id = "nested_tpl")

  expect_no_error(validate_template(dir, strict = TRUE))
})

test_that("a top-level template alongside a nested one is validated normally, the nested one is reported, and no_templates does not fire", {
  dir <- withr::local_tempdir()
  write_clean_template(dir, filename = "top.dta-template.yaml", id = "top_tpl")
  sub <- file.path(dir, "archive")
  dir.create(sub)
  write_clean_template(sub, filename = "old.dta-template.yaml", id = "old_tpl")

  result <- validate_template(dir)

  expect_code_row(result, "template_in_subdirectory", "warning")
  expect_no_code_row(result, "no_templates")
  # The top-level file's own row set stays empty -- it is a fully valid
  # template, so anything else reported against it would be a false positive.
  top_rows <- result[result$file == file.path(dir, "top.dta-template.yaml"), , drop = FALSE]
  expect_equal(nrow(top_rows), 0)
})

test_that("files under a dot-directory are not reported as nested templates", {
  # A cloned repository's .git/ (or an IDE's .Rproj.user/) must not produce
  # noise just because it happens to contain something matching a template
  # filename suffix.
  dir <- withr::local_tempdir()
  write_clean_template(dir, filename = "top.dta-template.yaml", id = "top_tpl")
  dotdir <- file.path(dir, ".git", "refs")
  dir.create(dotdir, recursive = TRUE)
  write_clean_template(dotdir, filename = "phantom.dta-template.yaml", id = "phantom_tpl")

  result <- validate_template(dir)
  expect_no_code_row(result, "template_in_subdirectory")
  expect_false(any(grepl("phantom", result$file, fixed = TRUE)))
})

# ---- Regression guard: the bundled templates -------------------------------

test_that("the bundled inst/extdata/templates directory validates with no error rows", {
  # KNOWN CONFLICT (see the implementation report): the shipped
  # biomarker_gf.dta-template.yaml has an unquoted `version: 1.0` at its top
  # level, which the mandated version_unquoted check (severity "error")
  # correctly flags. Fixing that file is outside this task's allowed file
  # list, so this assertion is expected to fail until it is fixed -- kept as
  # specified, per instruction, rather than weakened.
  result <- validate_template(system.file("extdata", "templates", package = "DTAtools"))
  errors <- result[result$severity == "error", , drop = FALSE]
  expect_equal(nrow(errors), 0, info = paste(utils::capture.output(print(errors)), collapse = "\n"))
})


# ---- The readers report by value, never by signal ---------------------------
#
# Both helpers below are documented as reporting failure through their return
# value: `.dta_template_read_raw()` hands the message back for the caller to
# attach to a file name, and `.dta_template_version_plain_is_exact()` says
# "cannot tell; do not manufacture a false positive". Each wrapped its read in
# `tryCatch(error = )` to achieve that, which does not intercept the warning a
# connection raises before it fails -- so a validator built to return findings
# also emitted one, in the session language, where nothing could match on it.
#
# Asserted as the ABSENCE of a warning. The text itself is base R's and is
# translated, so matching on it would pass in English and fail in German.

test_that(".dta_template_read_raw() does not signal on a file it cannot open", {
  for (bad in list(file.path(tempdir(), "no-such-template-xyz.yaml"), tempdir())) {
    res <- expect_no_warning(.dta_template_read_raw(bad))

    expect_false(res$ok)
    expect_null(res$def)
    # The message survives for the caller to report; it is only not signalled.
    expect_true(is.character(res$error) && nzchar(res$error))
  }
})


test_that(".dta_template_read_raw() still reads a real template unchanged", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "clean.dta-template.yaml")
  write_clean_template(dir)

  res <- expect_no_warning(.dta_template_read_raw(path))

  expect_true(res$ok)
  expect_identical(res$id, "clean_tpl")
  expect_identical(res$version, "1.0")
  expect_true(is.list(res$def))
})


test_that(".dta_template_version_plain_is_exact() does not signal on an unreadable file", {
  # TRUE is the documented "cannot tell" answer, chosen so an unreadable file
  # never manufactures a false positive.
  expect_true(
    expect_no_warning(
      .dta_template_version_plain_is_exact(file.path(tempdir(), "no-such-template-xyz.yaml"))
    )
  )
})


test_that(".dta_template_version_plain_is_exact() still tells quoted from unquoted", {
  dir <- withr::local_tempdir()

  quoted <- file.path(dir, "quoted.yaml")
  writeLines(c("kind: dta_creation_template", "id: q", 'version: "1.10"'), quoted)
  expect_true(.dta_template_version_plain_is_exact(quoted))

  unquoted <- file.path(dir, "unquoted.yaml")
  writeLines(c("kind: dta_creation_template", "id: u", "version: 1.10"), unquoted)
  expect_false(.dta_template_version_plain_is_exact(unquoted))
})
