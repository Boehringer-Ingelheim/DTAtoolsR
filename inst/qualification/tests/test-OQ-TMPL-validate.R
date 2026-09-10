# validate_template(): the codes it emits, and its strict/kinds/path arguments.
# REQ-TMPL-001 .. REQ-TMPL-018.
#
# Every fixture below lives under fixtures/tpl/ and is built to trigger
# EXACTLY the one code its test names -- verified by running validate_template()
# over it and reading back what actually came out, not by reasoning about the
# source alone. Where a fixture is abstract: true, that is deliberate: it stops
# the dry-run instantiate step from adding an unrelated instantiate_failed row
# on top of the structural check under test.

tmpl_fixture <- function(...) file.path(qa_suite_root(), "fixtures", "tpl", ...)

# The severity recorded for one code, or NA if the code did not appear at all --
# so a test that expects a code absent can say so with the same helper.
tmpl_severity <- function(result, code) {
  hit <- result$severity[result$code == code]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

tmpl_message <- function(result, code) {
  hit <- result$message[result$code == code]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

# ---- REQ-TMPL-001: result shape and the severity domain --------------------

test_that("OQ-TMPL-001 | the result carries the documented columns and only two severities | REQ-TMPL-001", {
  clean <- validate_template(tmpl_fixture("base"))
  qa_step(
    "a clean directory still returns the full column set, zero rows",
    c("file", "kind", "id", "version", "severity", "code", "message"),
    names(clean)
  )
  qa_check("and genuinely zero rows", nrow(clean) == 0)

  # Every code fixture built for this file, in one pass: whatever severities
  # they produce between them, none may be a third value.
  code_dirs <- list.dirs(tmpl_fixture("codes"), recursive = FALSE)
  all_severities <- unlist(lapply(code_dirs, function(d) validate_template(d)$severity))
  qa_step(
    "across every code fixture, only 'error' and 'warning' ever appear",
    character(0), setdiff(unique(all_severities), c("error", "warning"))
  )
})

# ---- REQ-TMPL-002: non-recursive scan ---------------------------------------

test_that("OQ-TMPL-002 | a template kept one level down is reported, not silently dropped | REQ-TMPL-002", {
  dir <- tmpl_fixture("codes", "template_in_subdirectory")
  result <- validate_template(dir)

  qa_step(
    "the top-level template is clean",
    0L, sum(basename(result$file) == "top.dta-template.yaml")
  )
  qa_step(
    "the nested one is reported template_in_subdirectory, at warning",
    "warning", tmpl_severity(result, "template_in_subdirectory")
  )
  qa_check(
    "naming the non-recursive scan as the reason",
    grepl("is not validated (the scan is non-recursive)", tmpl_message(result, "template_in_subdirectory"), fixed = TRUE)
  )
})

# ---- REQ-TMPL-003: parse_failed ---------------------------------------------

test_that("OQ-TMPL-003 | YAML that will not parse is reported parse_failed | REQ-TMPL-003", {
  result <- validate_template(tmpl_fixture("codes", "parse_failed"))
  qa_step("severity is error", "error", tmpl_severity(result, "parse_failed"))
  qa_check(
    "the file's own parser error is carried in the message",
    grepl("arser error", tmpl_message(result, "parse_failed"))
  )
})

# ---- REQ-TMPL-004: kind_unknown / id_missing / version_missing -------------

test_that("OQ-TMPL-004 | an unrecognised kind is reported kind_unknown, alone | REQ-TMPL-004", {
  result <- validate_template(tmpl_fixture("codes", "kind_unknown"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is warning", "warning", tmpl_severity(result, "kind_unknown"))
})

test_that("OQ-TMPL-005 | a missing id is reported id_missing, alone | REQ-TMPL-004", {
  result <- validate_template(tmpl_fixture("codes", "id_missing"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is warning", "warning", tmpl_severity(result, "id_missing"))
})

test_that("OQ-TMPL-006 | a missing version is reported version_missing, alone | REQ-TMPL-004", {
  result <- validate_template(tmpl_fixture("codes", "version_missing"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is warning", "warning", tmpl_severity(result, "version_missing"))
})

# ---- REQ-TMPL-005: version_unquoted (the important one) --------------------

test_that("OQ-TMPL-007 | an unquoted version is an error, because it silently loses precision | REQ-TMPL-005", {
  path <- tmpl_fixture("codes", "version_unquoted", "v.dta-template.yaml")

  # Make the consequence concrete against the ACTUAL fixture file, not an
  # illustration: read it the naive way (no version-preserving handlers) and
  # show what a consumer trusting that value would get.
  naive_version <- yaml::read_yaml(path)$version
  qa_step(
    "read naively, the file's '1.10' silently becomes the double 1.1",
    1.1, naive_version,
    tolerance = 1e-9
  )
  qa_step(
    "and printed back for a human, that lost precision reads as \"1.1\", not \"1.10\"",
    "1.1", as.character(naive_version)
  )

  result <- validate_template(dirname(path))
  qa_step(
    "validate_template() reports this as an error, not a warning",
    "error", tmpl_severity(result, "version_unquoted")
  )
  qa_check(
    "and the message states the 1.10 -> 1.1 risk by name",
    grepl("1.10 reads as 1.1", tmpl_message(result, "version_unquoted"), fixed = TRUE)
  )
})

test_that("OQ-TMPL-008 | a quoted but unparseable version is a warning, not the unquoted error | REQ-TMPL-006", {
  result <- validate_template(tmpl_fixture("codes", "version_unparseable"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is warning", "warning", tmpl_severity(result, "version_unparseable"))
  qa_check(
    "version_unquoted is NOT also raised -- the version text was quoted as written",
    is.na(tmpl_severity(result, "version_unquoted"))
  )
})

# ---- REQ-TMPL-007: duplicate_id_version -------------------------------------

test_that("OQ-TMPL-009 | two files sharing kind+id+version are reported once, naming the earlier one | REQ-TMPL-007", {
  result <- validate_template(tmpl_fixture("codes", "duplicate_id_version"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step(
    "reported against the file that sorts later",
    "two.dta-template.yaml", basename(result$file[[1]])
  )
  qa_check(
    "naming the earlier file as the one it duplicates",
    grepl("one.dta-template.yaml", result$message[[1]], fixed = TRUE)
  )
})

# ---- REQ-TMPL-008: no_templates ---------------------------------------------

test_that("OQ-TMPL-010 | a directory with no recognised template files is reported, not accepted silently | REQ-TMPL-008", {
  dir <- tmpl_fixture("codes", "no_templates")
  qa_check("the directory is non-empty (a decoy file), yet still trips the check", length(list.files(dir)) > 0)
  result <- validate_template(dir)
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is error", "error", tmpl_severity(result, "no_templates"))
})

# ---- REQ-TMPL-009: strict = TRUE --------------------------------------------

test_that("OQ-TMPL-011 | strict = TRUE raises a condition summarising the errors | REQ-TMPL-009", {
  err <- tryCatch(
    validate_template(tmpl_fixture("codes", "target_machine_owned"), strict = TRUE),
    error = function(e) e
  )
  qa_check("a condition is raised", inherits(err, "condition"))
  msg <- conditionMessage(err)
  qa_check("naming the count of errors", grepl("found 1 error", msg, fixed = TRUE))
  qa_check("and the offending code", grepl("target_machine_owned", msg, fixed = TRUE))
})

test_that("OQ-TMPL-012 | strict = TRUE does not abort when only warnings were found | REQ-TMPL-009", {
  outcome <- tryCatch(
    {
      validate_template(tmpl_fixture("codes", "target_invalid"), strict = TRUE)
      "no condition"
    },
    error = function(e) "condition"
  )
  qa_step("a warnings-only result raises nothing", "no condition", outcome)
})

# ---- REQ-TMPL-010: kinds ------------------------------------------------------

test_that("OQ-TMPL-013 | kinds restricts which files get a report row | REQ-TMPL-010", {
  dir <- tmpl_fixture("codes_args", "kinds_filter")

  both <- validate_template(dir)
  qa_step("with no kinds filter, both files report", c("dta_creation_template", "dta_vocabulary"), sort(both$kind))

  voc_only <- validate_template(dir, kinds = "dta_vocabulary")
  qa_step("kinds = 'dta_vocabulary' reports only the vocabulary's own row", "dta_vocabulary", voc_only$kind[[1]])
  qa_step("exactly one row", 1L, nrow(voc_only))

  creation_only <- validate_template(dir, kinds = "dta_creation_template")
  qa_step("kinds = 'dta_creation_template' reports only the creation template's row", "dta_creation_template", creation_only$kind[[1]])
})

test_that("OQ-TMPL-014 | kinds also narrows the duplicate_id_version check | REQ-TMPL-010", {
  dir <- tmpl_fixture("codes_args", "kinds_duplicate_scope")

  default_run <- validate_template(dir)
  qa_step(
    "by default, the two same-id creation templates are flagged duplicates",
    "duplicate_id_version", default_run$code[[1]]
  )

  vocab_only <- validate_template(dir, kinds = "dta_vocabulary")
  qa_step(
    "excluding creation templates from the report also excludes them from the duplicate check",
    0L, nrow(vocab_only)
  )
})

test_that("OQ-TMPL-015 | an unrecognised kind in `kinds` raises a condition naming the known set | REQ-TMPL-010", {
  err <- tryCatch(
    validate_template(tmpl_fixture("codes_args", "kinds_filter"), kinds = "not_a_kind"),
    error = function(e) e
  )
  qa_check("a condition is raised", inherits(err, "condition"))
  msg <- conditionMessage(err)
  qa_check("naming the bad value", grepl("not_a_kind", msg, fixed = TRUE))
  qa_check("and every known kind", grepl("dta_vocabulary", msg, fixed = TRUE))
})

test_that("OQ-TMPL-016 | kinds has no effect when path names a single file | REQ-TMPL-010", {
  file <- tmpl_fixture("codes_args", "kinds_filter", "t.dta-template.yaml")
  result <- validate_template(file, kinds = "dta_vocabulary")
  qa_step(
    "the explicitly named creation-template file is still checked",
    "target_invalid", result$code[[1]]
  )
})

# ---- REQ-TMPL-011: independence from the private-template-source env vars --

test_that("OQ-TMPL-017 | validate_template() ignores the app's private-template-source variables | REQ-TMPL-011", {
  dir <- tmpl_fixture("codes_args", "kinds_filter")
  baseline <- validate_template(dir)

  under_env <- withr::with_envvar(
    c(
      DTATOOLS_TEMPLATE_SOURCES = "dir:/nonexistent/bogus/path",
      DTATOOLS_TEMPLATE_INCLUDE_BUILTIN = "0",
      DTATOOLS_TEMPLATE_REFRESH_SECONDS = "-1",
      DTATOOLS_TEMPLATE_CACHE_DIR = "/nonexistent/cache",
      DTATOOLS_TEMPLATE_GIT_TOKEN = "bogus",
      DTATOOLS_TEMPLATE_GIT_USER = "bogus",
      DTATOOLS_TEMPLATE_GIT_AUTH = "bearer"
    ),
    validate_template(dir)
  )

  qa_step(
    "the result is identical whether or not the app's source variables are set",
    baseline[, c("kind", "severity", "code")], under_env[, c("kind", "severity", "code")]
  )
})

# ---- REQ-TMPL-012: target_machine_owned / target_invalid -------------------

test_that("OQ-TMPL-018 | an option targeting a machine-owned metadata field is an error | REQ-TMPL-012", {
  result <- validate_template(tmpl_fixture("codes", "target_machine_owned"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is error", "error", tmpl_severity(result, "target_machine_owned"))
  qa_check(
    "naming the machine-owned field",
    grepl("metadata.template", tmpl_message(result, "target_machine_owned"), fixed = TRUE)
  )
})

test_that("OQ-TMPL-019 | an option targeting no known metadata field is a warning | REQ-TMPL-012", {
  result <- validate_template(tmpl_fixture("codes", "target_invalid"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is warning", "warning", tmpl_severity(result, "target_invalid"))
})

test_that("OQ-TMPL-020 | a dataset-template option not rooted at 'dataset.' is target_invalid too | REQ-TMPL-012", {
  result <- validate_template(tmpl_fixture("codes", "target_invalid_dataset"))
  qa_step("severity is warning", "warning", tmpl_severity(result, "target_invalid"))
  qa_check(
    "naming the wrong root",
    grepl("not rooted at 'dataset.'", tmpl_message(result, "target_invalid"), fixed = TRUE)
  )
})

# ---- REQ-TMPL-013: party_slot_invalid --------------------------------------

test_that("OQ-TMPL-021 | a party slot naming an unknown profile id is a warning | REQ-TMPL-013", {
  result <- validate_template(tmpl_fixture("codes", "party_slot_invalid"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is warning", "warning", tmpl_severity(result, "party_slot_invalid"))
  qa_check(
    "naming the unknown id",
    grepl("nonexistent_profile", tmpl_message(result, "party_slot_invalid"), fixed = TRUE)
  )
})

test_that("OQ-TMPL-022 | a party slot with a malformed target is party_slot_invalid too | REQ-TMPL-013", {
  result <- validate_template(tmpl_fixture("codes", "party_slot_invalid_shape"))
  qa_step("severity is warning", "warning", tmpl_severity(result, "party_slot_invalid"))
  qa_check(
    "naming the two legal targets",
    grepl("metadata.supplier", tmpl_message(result, "party_slot_invalid"), fixed = TRUE)
  )
})

# ---- REQ-TMPL-014: vocab_slot_invalid / vocab_slot_unresolved --------------

test_that("OQ-TMPL-023 | a malformed vocabulary slot is an error | REQ-TMPL-014", {
  result <- validate_template(tmpl_fixture("codes", "vocab_slot_invalid"))
  qa_step("severity is error", "error", tmpl_severity(result, "vocab_slot_invalid"))
})

test_that("OQ-TMPL-024 | a vocabulary slot naming an unresolved vocabulary is a warning | REQ-TMPL-014", {
  result <- validate_template(tmpl_fixture("codes", "vocab_slot_unresolved"))
  qa_step("severity is warning", "warning", tmpl_severity(result, "vocab_slot_unresolved"))
  qa_check(
    "naming the unresolved reference",
    grepl("nonexistent_vocab@1.0", tmpl_message(result, "vocab_slot_unresolved"), fixed = TRUE)
  )
})

# ---- REQ-TMPL-015: dataset_template_unresolved / patch_incoherent ---------

test_that("OQ-TMPL-025 | an unresolved dataset-template reference is a warning | REQ-TMPL-015", {
  result <- validate_template(tmpl_fixture("codes", "dataset_template_unresolved"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is warning", "warning", tmpl_severity(result, "dataset_template_unresolved"))
})

test_that("OQ-TMPL-026 | a patch naming a column the dataset template lacks is patch_incoherent | REQ-TMPL-015", {
  result <- validate_template(tmpl_fixture("codes", "patch_incoherent"))
  qa_step("severity is warning", "warning", tmpl_severity(result, "patch_incoherent"))
  qa_check(
    "naming the unknown column",
    grepl("NOSUCHCOLUMN", tmpl_message(result, "patch_incoherent"), fixed = TRUE)
  )
})

# ---- REQ-TMPL-016: dataset_missing ------------------------------------------

test_that("OQ-TMPL-027 | a dataset template with no dataset body is an error | REQ-TMPL-016", {
  result <- validate_template(tmpl_fixture("codes", "dataset_missing"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is error", "error", tmpl_severity(result, "dataset_missing"))

  hinted <- validate_template(tmpl_fixture("codes", "dataset_missing_plural"))
  qa_check(
    "authoring the plural 'datasets:' by mistake earns a specific hint",
    grepl("Found 'datasets:' (plural) instead", tmpl_message(hinted, "dataset_missing"), fixed = TRUE)
  )
})

# ---- REQ-TMPL-017: values_and_values_from / values_from_pattern -----------

test_that("OQ-TMPL-028 | a column setting both values and values_from is a warning, the binding wins | REQ-TMPL-017", {
  result <- validate_template(tmpl_fixture("codes", "values_and_values_from"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is warning", "warning", tmpl_severity(result, "values_and_values_from"))
})

test_that("OQ-TMPL-029 | combining values_from with pattern is an error, and short-circuits | REQ-TMPL-017", {
  result <- validate_template(tmpl_fixture("codes", "values_from_pattern"))
  qa_step("exactly one row -- the unresolved vocabulary is never even reached", 1L, nrow(result))
  qa_step("severity is error", "error", tmpl_severity(result, "values_from_pattern"))
})

# ---- REQ-TMPL-018: values_from_invalid / _unresolved / _terms_invalid -----

test_that("OQ-TMPL-030 | a malformed values_from mapping is an error | REQ-TMPL-018", {
  result <- validate_template(tmpl_fixture("codes", "values_from_invalid"))
  qa_step("severity is error", "error", tmpl_severity(result, "values_from_invalid"))
})

test_that("OQ-TMPL-031 | values_from naming an unresolved vocabulary is a warning | REQ-TMPL-018", {
  result <- validate_template(tmpl_fixture("codes", "values_from_unresolved"))
  qa_step("severity is warning", "warning", tmpl_severity(result, "values_from_unresolved"))
})

test_that("OQ-TMPL-032 | include naming a code the vocabulary lacks is an error | REQ-TMPL-018", {
  result <- validate_template(tmpl_fixture("codes", "values_from_terms_invalid"))
  qa_step("severity is error", "error", tmpl_severity(result, "values_from_terms_invalid"))
  qa_check(
    "naming the unknown code",
    grepl("NOPE", tmpl_message(result, "values_from_terms_invalid"), fixed = TRUE)
  )
})
