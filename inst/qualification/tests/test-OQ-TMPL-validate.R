# validate_template(): the codes it emits, and its strict/kinds/path arguments.
# REQ-TMPL-001 .. REQ-TMPL-018, REQ-TMPL-031, REQ-TMPL-033 and REQ-TMPL-034.
#
# Every fixture below lives under fx/t/ and is built to trigger
# EXACTLY the one code its test names -- verified by running validate_template()
# over it and reading back what actually came out, not by reasoning about the
# source alone. Where a fixture is abstract: true, that is deliberate: it stops
# the dry-run instantiate step from adding an unrelated instantiate_failed row
# on top of the structural check under test.

tmpl_fixture <- function(...) file.path(qa_suite_root(), "fx", "t", ...)

# The severity recorded for one code, or NA if the code did not appear at all --
# so a test that expects a code absent can say so with the same helper.
tmpl_severity <- function(result, code) {
  hit <- result$severity[result$code == code]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

# A Word file with the given paragraphs; its default is a fill-in template.
tmpl_docx <- function(path, lines = c("Agreement {TITLE}", "{VERSION}")) {
  doc <- officer::read_docx()
  for (ln in lines) doc <- officer::body_add_par(doc, ln)
  print(doc, target = path)
  invisible(path)
}

# The text of a party profile with this id; role and kind are the free choices.
tmpl_party_text <- function(id, role = "supplier", kind = "dta_party_profile") {
  c(
    sprintf("kind: %s", kind),
    sprintf("id: %s", id),
    'version: "1.0"',
    sprintf("role: %s", role),
    sprintf("label: Party %s", id),
    "affiliation:",
    "  name: Example Inc.",
    "  country: DE"
  )
}

# Write one file below a library root, creating its folder.
tmpl_put <- function(root, rel, lines) {
  dir.create(dirname(file.path(root, rel)), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, file.path(root, rel))
  invisible(file.path(root, rel))
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
  code_dirs <- list.dirs(tmpl_fixture("c"), recursive = FALSE)
  all_severities <- unlist(lapply(code_dirs, function(d) validate_template(d)$severity))
  qa_step(
    "across every code fixture, only 'error' and 'warning' ever appear",
    character(0), setdiff(unique(all_severities), c("error", "warning"))
  )
})

# ---- REQ-TMPL-002: non-recursive scan ---------------------------------------

test_that("OQ-TMPL-002 | a template kept below the folder of its kind is reported, not silently dropped | REQ-TMPL-002", {
  # Fixture: templates/top.dta-template.yaml (id nest_top) and
  # templates/archive/n.dta-template.yaml (id nest_sub).
  dir <- tmpl_fixture("c", "template_in_subdir")
  result <- validate_template(dir)

  qa_step(
    "the template directly in templates/ is clean",
    0L, sum(basename(result$file) == "top.dta-template.yaml")
  )
  qa_step(
    "exactly one row is reported, for the nested file",
    "n.dta-template.yaml", basename(result$file)
  )
  qa_step(
    "it is reported template_in_subdirectory, at warning",
    "warning", tmpl_severity(result, "template_in_subdirectory")
  )
  qa_check(
    "naming the non-recursive scan as the reason",
    grepl("the template loader will not see it (the scan is non-recursive)", tmpl_message(result, "template_in_subdirectory"), fixed = TRUE)
  )
  qa_check(
    "and the folder to move it to",
    grepl("templates/", tmpl_message(result, "template_in_subdirectory"), fixed = TRUE)
  )

  # A reference into the nested file does not resolve: the scan does not see it.
  root <- qa_tempdir()
  dir.create(file.path(root, "templates", "archive"), recursive = TRUE)
  yaml::write_yaml(list(
    kind = "dta_creation_template", id = "nest_parent", version = "1.0", abstract = TRUE
  ), file.path(root, "templates", "archive", "parent.dta-template.yaml"))
  yaml::write_yaml(list(
    kind = "dta_creation_template", id = "nest_child", version = "1.0",
    abstract = TRUE, extends = "nest_parent@1.0"
  ), file.path(root, "templates", "child.dta-template.yaml"))
  nested_ref <- validate_template(root)
  qa_step(
    "a template extending the nested one finds no parent: the nested file takes no part in resolution",
    "extends_unresolved", nested_ref$code[nested_ref$code == "extends_unresolved"]
  )
})

# ---- REQ-TMPL-003: parse_failed ---------------------------------------------

test_that("OQ-TMPL-003 | YAML that will not parse is reported parse_failed | REQ-TMPL-003", {
  result <- validate_template(tmpl_fixture("c", "parse_failed"))
  qa_step("severity is error", "error", tmpl_severity(result, "parse_failed"))
  qa_check(
    "the file's own parser error is carried in the message",
    grepl("arser error", tmpl_message(result, "parse_failed"))
  )
})

# ---- REQ-TMPL-004: kind_unknown / id_missing / version_missing -------------

test_that("OQ-TMPL-004 | an unrecognised kind is reported kind_unknown, alone | REQ-TMPL-004", {
  result <- validate_template(tmpl_fixture("c", "kind_unknown"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is warning", "warning", tmpl_severity(result, "kind_unknown"))
})

test_that("OQ-TMPL-005 | a missing id is reported id_missing, alone | REQ-TMPL-004", {
  result <- validate_template(tmpl_fixture("c", "id_missing"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is warning", "warning", tmpl_severity(result, "id_missing"))
})

test_that("OQ-TMPL-006 | a missing version is reported version_missing, alone | REQ-TMPL-004", {
  result <- validate_template(tmpl_fixture("c", "version_missing"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is warning", "warning", tmpl_severity(result, "version_missing"))
})

# ---- REQ-TMPL-005: version_unquoted (the important one) --------------------

test_that("OQ-TMPL-007 | an unquoted version is an error, because it silently loses precision | REQ-TMPL-005", {
  path <- tmpl_fixture("c", "version_unquoted", "templates", "v.dta-template.yaml")

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

  result <- validate_template(dirname(dirname(path)))
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
  result <- validate_template(tmpl_fixture("c", "version_unparseable"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is warning", "warning", tmpl_severity(result, "version_unparseable"))
  qa_check(
    "version_unquoted is NOT also raised -- the version text was quoted as written",
    is.na(tmpl_severity(result, "version_unquoted"))
  )
})

# ---- REQ-TMPL-007: duplicate_id_version -------------------------------------

test_that("OQ-TMPL-009 | two files sharing kind+id+version are reported once, naming the earlier one | REQ-TMPL-007", {
  result <- validate_template(tmpl_fixture("c", "duplicate_id_version"))
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
  dir <- tmpl_fixture("c", "no_templates")
  qa_check("the directory is non-empty (a decoy file), yet still trips the check", length(list.files(dir)) > 0)
  result <- validate_template(dir)
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is error", "error", tmpl_severity(result, "no_templates"))
})

# ---- REQ-TMPL-009: strict = TRUE --------------------------------------------

test_that("OQ-TMPL-011 | strict = TRUE raises a condition summarising the errors | REQ-TMPL-009", {
  err <- tryCatch(
    validate_template(tmpl_fixture("c", "target_machine_owned"), strict = TRUE),
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
      validate_template(tmpl_fixture("c", "target_invalid"), strict = TRUE)
      "no condition"
    },
    error = function(e) "condition"
  )
  qa_step("a warnings-only result raises nothing", "no condition", outcome)
})

# ---- REQ-TMPL-010: kinds ------------------------------------------------------

test_that("OQ-TMPL-013 | kinds restricts which files get a report row | REQ-TMPL-010", {
  dir <- tmpl_fixture("ca", "kinds_filter")

  both <- validate_template(dir)
  qa_step("with no kinds filter, both files report", c("dta_creation_template", "dta_vocabulary"), sort(both$kind))

  voc_only <- validate_template(dir, kinds = "dta_vocabulary")
  qa_step("kinds = 'dta_vocabulary' reports only the vocabulary's own row", "dta_vocabulary", voc_only$kind[[1]])
  qa_step("exactly one row", 1L, nrow(voc_only))

  creation_only <- validate_template(dir, kinds = "dta_creation_template")
  qa_step("kinds = 'dta_creation_template' reports only the creation template's row", "dta_creation_template", creation_only$kind[[1]])
})

test_that("OQ-TMPL-014 | kinds also narrows the duplicate_id_version check | REQ-TMPL-010", {
  dir <- tmpl_fixture("ca", "kinds_duplicate_scope")

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
    validate_template(tmpl_fixture("ca", "kinds_filter"), kinds = "not_a_kind"),
    error = function(e) e
  )
  qa_check("a condition is raised", inherits(err, "condition"))
  msg <- conditionMessage(err)
  qa_check("naming the bad value", grepl("not_a_kind", msg, fixed = TRUE))
  qa_check("and every known kind", grepl("dta_vocabulary", msg, fixed = TRUE))
})

test_that("OQ-TMPL-016 | kinds has no effect when path names a single file | REQ-TMPL-010", {
  file <- tmpl_fixture("ca", "kinds_filter", "templates", "t.dta-template.yaml")
  result <- validate_template(file, kinds = "dta_vocabulary")
  qa_step(
    "the explicitly named creation-template file is still checked",
    "target_invalid", result$code[[1]]
  )
})

# ---- REQ-TMPL-011: independence from the private-template-source env vars --

test_that("OQ-TMPL-017 | validate_template() ignores the app's private-template-source variables | REQ-TMPL-011", {
  dir <- tmpl_fixture("ca", "kinds_filter")
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
  result <- validate_template(tmpl_fixture("c", "target_machine_owned"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is error", "error", tmpl_severity(result, "target_machine_owned"))
  qa_check(
    "naming the machine-owned field",
    grepl("metadata.template", tmpl_message(result, "target_machine_owned"), fixed = TRUE)
  )
})

test_that("OQ-TMPL-019 | an option targeting no known metadata field is a warning | REQ-TMPL-012", {
  result <- validate_template(tmpl_fixture("c", "target_invalid"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is warning", "warning", tmpl_severity(result, "target_invalid"))
})

test_that("OQ-TMPL-020 | a dataset-template option not rooted at 'dataset.' is target_invalid too | REQ-TMPL-012", {
  result <- validate_template(tmpl_fixture("c", "target_invalid_dataset"))
  qa_step("severity is warning", "warning", tmpl_severity(result, "target_invalid"))
  qa_check(
    "naming the wrong root",
    grepl("not rooted at 'dataset.'", tmpl_message(result, "target_invalid"), fixed = TRUE)
  )
})

# ---- REQ-TMPL-013: party_slot_invalid --------------------------------------

test_that("OQ-TMPL-021 | a party slot naming an unknown profile id is a warning | REQ-TMPL-013", {
  result <- validate_template(tmpl_fixture("c", "party_slot_invalid"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is warning", "warning", tmpl_severity(result, "party_slot_invalid"))
  qa_check(
    "naming the unknown id",
    grepl("nonexistent_profile", tmpl_message(result, "party_slot_invalid"), fixed = TRUE)
  )
})

test_that("OQ-TMPL-022 | a party slot with a malformed target is party_slot_invalid too | REQ-TMPL-013", {
  result <- validate_template(tmpl_fixture("c", "party_slot_invalid_shape"))
  qa_step("severity is warning", "warning", tmpl_severity(result, "party_slot_invalid"))
  qa_check(
    "naming the two legal targets",
    grepl("metadata.supplier", tmpl_message(result, "party_slot_invalid"), fixed = TRUE)
  )
})

# ---- REQ-TMPL-014: vocab_slot_invalid / vocab_slot_unresolved --------------

test_that("OQ-TMPL-023 | a malformed vocabulary slot is an error | REQ-TMPL-014", {
  result <- validate_template(tmpl_fixture("c", "vocab_slot_invalid"))
  qa_step("severity is error", "error", tmpl_severity(result, "vocab_slot_invalid"))
})

test_that("OQ-TMPL-024 | a vocabulary slot naming an unresolved vocabulary is a warning | REQ-TMPL-014", {
  result <- validate_template(tmpl_fixture("c", "vocab_slot_unresolved"))
  qa_step("severity is warning", "warning", tmpl_severity(result, "vocab_slot_unresolved"))
  qa_check(
    "naming the unresolved reference",
    grepl("nonexistent_vocab@1.0", tmpl_message(result, "vocab_slot_unresolved"), fixed = TRUE)
  )
})

# ---- REQ-TMPL-015: dataset_template_unresolved / patch_incoherent ---------

test_that("OQ-TMPL-025 | an unresolved dataset-template reference is a warning | REQ-TMPL-015", {
  result <- validate_template(tmpl_fixture("c", "dataset_template_unresolved"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is warning", "warning", tmpl_severity(result, "dataset_template_unresolved"))
})

test_that("OQ-TMPL-026 | a patch naming a column the dataset template lacks is patch_incoherent | REQ-TMPL-015", {
  result <- validate_template(tmpl_fixture("c", "patch_incoherent"))
  qa_step("severity is warning", "warning", tmpl_severity(result, "patch_incoherent"))
  qa_check(
    "naming the unknown column",
    grepl("NOSUCHCOLUMN", tmpl_message(result, "patch_incoherent"), fixed = TRUE)
  )
})

# ---- REQ-TMPL-016: dataset_missing ------------------------------------------

test_that("OQ-TMPL-027 | a dataset template with no dataset body is an error | REQ-TMPL-016", {
  result <- validate_template(tmpl_fixture("c", "dataset_missing"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is error", "error", tmpl_severity(result, "dataset_missing"))

  hinted <- validate_template(tmpl_fixture("c", "dataset_missing_plural"))
  qa_check(
    "authoring the plural 'datasets:' by mistake earns a specific hint",
    grepl("Found 'datasets:' (plural) instead", tmpl_message(hinted, "dataset_missing"), fixed = TRUE)
  )
})

# ---- REQ-TMPL-017: values_and_values_from / values_from_pattern -----------

test_that("OQ-TMPL-028 | a column setting both values and values_from is a warning, the binding wins | REQ-TMPL-017", {
  result <- validate_template(tmpl_fixture("c", "values_and_values_from"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step("severity is warning", "warning", tmpl_severity(result, "values_and_values_from"))
})

test_that("OQ-TMPL-029 | combining values_from with pattern is an error, and short-circuits | REQ-TMPL-017", {
  result <- validate_template(tmpl_fixture("c", "values_from_pattern"))
  qa_step("exactly one row -- the unresolved vocabulary is never even reached", 1L, nrow(result))
  qa_step("severity is error", "error", tmpl_severity(result, "values_from_pattern"))
})

# ---- REQ-TMPL-018: values_from_invalid / _unresolved / _terms_invalid -----

test_that("OQ-TMPL-030 | a malformed values_from mapping is an error | REQ-TMPL-018", {
  result <- validate_template(tmpl_fixture("c", "values_from_invalid"))
  qa_step("severity is error", "error", tmpl_severity(result, "values_from_invalid"))
})

test_that("OQ-TMPL-031 | values_from naming an unresolved vocabulary is a warning | REQ-TMPL-018", {
  result <- validate_template(tmpl_fixture("c", "values_from_unresolved"))
  qa_step("severity is warning", "warning", tmpl_severity(result, "values_from_unresolved"))
})

test_that("OQ-TMPL-032 | include naming a code the vocabulary lacks is an error | REQ-TMPL-018", {
  result <- validate_template(tmpl_fixture("c", "values_terms_invalid"))
  qa_step("severity is error", "error", tmpl_severity(result, "values_from_terms_invalid"))
  qa_check(
    "naming the unknown code",
    grepl("NOPE", tmpl_message(result, "values_from_terms_invalid"), fixed = TRUE)
  )
})


# ---- REQ-TMPL-031: placement of template files -----------------------------
# Fixture contents and expected findings are specified independently of the
# engine: one clean abstract creation template per layout (layout/folder adds
# a dataset template, a party profile and a vocabulary, one per kind folder);
# mixed adds a copy of the template with an identical id and version at the
# root, to prove that file is excluded from resolution and the duplicate check.

# A copy of a fixture library in a fresh temporary directory, so a test can
# damage it without touching the committed fixture.
tmpl_copy_library <- function(from) {
  to <- qa_tempdir(parent.frame())
  file.copy(list.files(from, full.names = TRUE, all.files = TRUE, no.. = TRUE), to, recursive = TRUE)
  to
}

# Rewrite `version: "1.0"` to the unquoted `version: 1.10` in the given
# files: a defect validate_template() reports as version_unquoted for every
# file it actually reads, whatever that file's kind.
tmpl_unquote_versions <- function(files) {
  for (f in files) {
    writeLines(sub('version: "1.0"', "version: 1.10", readLines(f), fixed = TRUE), f)
  }
}

test_that("OQ-TMPL-062 | a library root with a folder per kind validates cleanly, and each kind is read from its own folder | REQ-TMPL-031", {
  root <- tmpl_fixture("lay", "folder")
  result <- validate_template(root)
  qa_step("the four-folder library has no findings", 0L, nrow(result))
  strict <- tryCatch(
    {
      validate_template(root, strict = TRUE)
      "no condition"
    },
    error = function(e) conditionMessage(e)
  )
  qa_step("strict validation accepts the repository root", "no condition", strict)

  # Proof that every folder is read, not merely that nothing was found: break
  # the version of the one file in each of templates/ (a creation template and
  # a dataset template), parties/ and vocabulary/, and each must be reported.
  copy <- tmpl_copy_library(root)
  files <- c(
    file.path(copy, "templates", "example.dta-template.yaml"),
    file.path(copy, "templates", "example_dataset.dta-dataset-template.yaml"),
    file.path(copy, "parties", "example.dta-party.yaml"),
    file.path(copy, "vocabulary", "example.dta-vocabulary.yaml")
  )
  tmpl_unquote_versions(files)
  damaged <- validate_template(copy)
  qa_step(
    "each of the four kinds is read from the folder of its kind",
    c("dta_creation_template", "dta_dataset_template", "dta_party_profile", "dta_vocabulary"),
    damaged$kind[match(files, damaged$file)]
  )
  qa_step("each of the four is reported version_unquoted", rep("version_unquoted", 4L), damaged$code[match(files, damaged$file)])
})

test_that("OQ-TMPL-063 | a template left in the root is an error and never a duplicate candidate, while the folders are still read | REQ-TMPL-031", {
  root <- tmpl_fixture("lay", "mixed")
  result <- validate_template(root)
  qa_step("exactly one finding, not a duplicate or nested warning", 1L, nrow(result))
  qa_step("the code identifies a misplaced file", "template_misplaced", result$code)
  qa_step("the severity fails CI", "error", result$severity)
  qa_step("the finding names the root file", file.path(root, "leftover.dta-template.yaml"), result$file)
  qa_step("the readable kind is retained", "dta_creation_template", result$kind)
  qa_step("the readable id is retained", "layout_example", result$id)
  qa_step("the readable version is retained", "1.0", result$version)
  qa_check(
    "the message names the folder the file belongs in and explains invisibility",
    grepl("templates/", result$message, fixed = TRUE) &&
      grepl("this file is invisible", result$message, fixed = TRUE)
  )
  qa_check(
    "and says that folder names are case-sensitive on the server",
    grepl("case-sensitive", result$message, fixed = TRUE)
  )
  err <- tryCatch(validate_template(root, strict = TRUE), error = function(e) e)
  qa_check("strict validation raises a package-authored condition", inherits(err, "rlang_error"))
  qa_check(
    "the condition names the misplaced-file code",
    grepl("template_misplaced", conditionMessage(err), fixed = TRUE)
  )

  # The valid template in templates/ is still read: damaged, it is reported.
  copy <- tmpl_copy_library(root)
  tmpl_unquote_versions(file.path(copy, "templates", "example.dta-template.yaml"))
  damaged <- validate_template(copy)
  qa_step(
    "a defect in the valid template is still found, beside the misplaced file",
    c("template_misplaced", "version_unquoted"), sort(damaged$code)
  )
})

test_that("OQ-TMPL-064 | templates kept directly in the root are no library: each is misplaced and nothing else is reported | REQ-TMPL-031", {
  root <- tmpl_fixture("lay", "root")
  result <- validate_template(root)
  qa_step("exactly one finding for the one root-level template", 1L, nrow(result))
  qa_step("it is template_misplaced", "template_misplaced", result$code)
  qa_step("at severity error", "error", result$severity)
  qa_step(
    "and no_templates is not reported on top: the misplaced row already explains the empty library",
    NA_character_, tmpl_severity(result, "no_templates")
  )
  err <- tryCatch(validate_template(root, strict = TRUE), error = function(e) e)
  qa_check("strict validation rejects the old layout", inherits(err, "rlang_error"))
})

test_that("OQ-TMPL-065 | kind folders holding nothing of their kind are no_templates, and a template in the root turns that into template_misplaced | REQ-TMPL-031", {
  root <- qa_tempdir()
  dir.create(file.path(root, "templates"))
  qa_step("an empty templates/ folder is no_templates", "no_templates", validate_template(root)$code)
  writeLines("Supporting notes, not a template.", file.path(root, "templates", "notes.txt"))
  qa_step("a folder holding only notes is still no_templates", "no_templates", validate_template(root)$code)
  file.copy(
    file.path(tmpl_fixture("lay", "root"), "example.dta-template.yaml"),
    file.path(root, "example.dta-template.yaml")
  )
  qa_step(
    "a template in the root does not make the library non-empty: it is misplaced",
    "template_misplaced", validate_template(root)$code
  )
})

# ---- REQ-TMPL-033: dataset_id_missing / dataset_field_invalid --------------

test_that("OQ-TMPL-068 | a dataset with no id, or a list for a field, is reported, the pre-id shape included | REQ-TMPL-033", {
  tpl <- validate_template(tmpl_fixture("c", "dataset_id_missing"))
  qa_step("dataset template without id: exactly one row", 1L, nrow(tpl))
  qa_step("dataset template without id: severity is error", "error", tmpl_severity(tpl, "dataset_id_missing"))

  inline <- validate_template(tmpl_fixture("c", "dataset_id_missing_inline"))
  qa_step("inline dataset without id, abstract template: severity is error", "error", tmpl_severity(inline, "dataset_id_missing"))

  field <- validate_template(tmpl_fixture("c", "dataset_field_invalid"))
  qa_step("a list for name: severity is error", "error", tmpl_severity(field, "dataset_field_invalid"))
  qa_check("a list for name: the message names 'name'", grepl("'name'", tmpl_message(field, "dataset_field_invalid"), fixed = TRUE))

  field_legacy <- validate_template(tmpl_fixture("c", "dataset_field_legacy"))
  qa_check(
    "a list for a pre-id name: the message names 'name', the key as written",
    grepl("'name'", tmpl_message(field_legacy, "dataset_field_invalid"), fixed = TRUE)
  )

  legacy <- validate_template(tmpl_fixture("c", "dataset_legacy_shape"))
  qa_step("pre-id shape (name: with no id:): dataset_id_missing is an error", "error", tmpl_severity(legacy, "dataset_id_missing"))
  qa_step("pre-id shape: its scalar fields are not dataset_field_invalid", NA_character_, tmpl_severity(legacy, "dataset_field_invalid"))

  verbs <- validate_template(tmpl_fixture("c", "dataset_verb_form"))
  qa_step("verb-form datasets: with ids under add:/modify:: no row is reported", 0L, nrow(verbs))

  verb_add <- validate_template(tmpl_fixture("c", "dataset_id_no_verb_add"))
  qa_step("verb-form add: entry without id: exactly one dataset_id_missing row", 1L, sum(verb_add$code == "dataset_id_missing"))
  qa_step("verb-form add: entry without id: severity is error", "error", tmpl_severity(verb_add, "dataset_id_missing"))
})

# ---- REQ-TMPL-034: column_spec_invalid ---------------------------------------

test_that("OQ-TMPL-069 | a dataset template column that cannot be built is reported, alone and by name | REQ-TMPL-034", {
  # C1 declares length 3 and the example ABCD; C2 is the same with ABC. The
  # first contradicts itself, the second does not.
  result <- validate_template(tmpl_fixture("c", "column_spec_invalid"))
  qa_step("exactly one row is reported", "column_spec_invalid", result$code)
  qa_step("at severity error", "error", tmpl_severity(result, "column_spec_invalid"))
  qa_step(
    "naming the column that cannot be built and not the one that can",
    c(c1 = TRUE, c2 = FALSE),
    c(
      c1 = grepl("column 'C1'", tmpl_message(result, "column_spec_invalid"), fixed = TRUE),
      c2 = grepl("C2", tmpl_message(result, "column_spec_invalid"), fixed = TRUE)
    )
  )

  # The control: a dataset template whose columns all build is not reported.
  qa_step("a dataset template with buildable columns is not reported", 0L, nrow(validate_template(tmpl_fixture("base"))))

  err <- tryCatch(validate_template(tmpl_fixture("c", "column_spec_invalid"), strict = TRUE), error = function(e) e)
  qa_check("under strict = TRUE the finding aborts with the package's own error", inherits(err, "rlang_error"))
  qa_check(
    "and the abort names the code and the error count",
    is.character(tryCatch(conditionMessage(err), error = function(e) NULL)) &&
      grepl("Template validation found 1 error", conditionMessage(err), fixed = TRUE) &&
      grepl("[column_spec_invalid]", conditionMessage(err), fixed = TRUE)
  )
})

test_that("OQ-TMPL-070 | an inline column that cannot be built is an instantiate_failed, not a column_spec_invalid | REQ-TMPL-034", {
  result <- validate_template(tmpl_fixture("c", "column_spec_invalid_inline"))
  qa_step("the build failure is the one row reported", "instantiate_failed", result$code)
  qa_step("at severity error", "error", tmpl_severity(result, "instantiate_failed"))
  qa_step("and the dataset-template code is absent", NA_character_, tmpl_severity(result, "column_spec_invalid"))
})

# ---- REQ-TMPL-035: the linter's findings become template rows ---------------

test_that("OQ-TMPL-071 | a dataset template's rules are checked against its columns, under the linter's codes | REQ-TMPL-035", {
  # lint_a: rule r1 names C2, but only C1 is declared.
  # lint_b: a col_range on C1, a Char column.
  # lint_c: a rule, but no column at all, so there is nothing to check it against.
  result <- validate_template(tmpl_fixture("c", "lint_rules"))
  by_file <- function(prefix) result[startsWith(basename(result$file), prefix), , drop = FALSE]

  qa_step("one row per template, three in all", 3L, nrow(result))
  qa_step("an undeclared column is rule_column_undeclared", "rule_column_undeclared", by_file("a.")$code)
  qa_step("at severity error", "error", by_file("a.")$severity)
  qa_check("naming the rule and the column", grepl("r1", by_file("a.")$message, fixed = TRUE) && grepl("C2", by_file("a.")$message, fixed = TRUE))
  qa_step("a range on a text column is rule_range_non_numeric", "rule_range_non_numeric", by_file("b.")$code)
  qa_step("at severity error", "error", by_file("b.")$severity)
  qa_step("rules without any column are rules_without_columns", "rules_without_columns", by_file("c.")$code)
  qa_step("at severity warning", "warning", by_file("c.")$severity)

  only_warning <- tryCatch(
    validate_template(file.path(tmpl_fixture("c", "lint_rules"), "templates", "c.dta-dataset-template.yaml"), strict = TRUE),
    error = function(e) e
  )
  qa_check("strict = TRUE does not abort on a warning alone", is.data.frame(only_warning))
  aborted <- tryCatch(
    validate_template(file.path(tmpl_fixture("c", "lint_rules"), "templates", "a.dta-dataset-template.yaml"), strict = TRUE),
    error = function(e) e
  )
  qa_check("but aborts on the error", inherits(aborted, "error"))
})

test_that("OQ-TMPL-072 | a creation template whose instance breaks the invariant is instantiate_failed alone | REQ-TMPL-035", {
  # The only dataset has column C1 and a rule on NOPE, which the collection
  # refuses to build.
  result <- validate_template(tmpl_fixture("c", "lint_instance_invalid"))
  qa_step("the build failure is the one row reported", "instantiate_failed", result$code)
  qa_step("at severity error", "error", result$severity)
  qa_step(
    "and the linter's code is not reported on top of it",
    0L, sum(result$code == "rule_column_undeclared")
  )
})

# ---- REQ-TMPL-036: prior_versions_invalid ----------------------------------

test_that("OQ-TMPL-073 | a malformed prior_versions entry is prior_versions_invalid, a good one is not | REQ-TMPL-036", {
  # Entry 1 (4.0) is well formed; entry 2 (3.0) has no changes; entry 3 (5.0)
  # is not older than the file's own version 5.0.
  result <- validate_template(tmpl_fixture("c", "prior_versions_invalid"))
  rows <- result[result$code == "prior_versions_invalid", , drop = FALSE]
  qa_step("one row per malformed entry", 2L, nrow(rows))
  qa_step("at severity error", "error", unique(rows$severity))
  qa_check("the entry without changes is named", any(grepl("entry 2 (3.0)", rows$message, fixed = TRUE)))
  qa_check("the entry not older than 5.0 is named", any(grepl("entry 3 (5.0)", rows$message, fixed = TRUE)))
  qa_check("the well-formed entry is not reported", !any(grepl("(4.0)", rows$message, fixed = TRUE)))
})

# ---- REQ-TMPL-037: app configuration (dta-app.yaml) -------------------------

test_that("OQ-TMPL-074 | a valid app configuration adds no rows, and is not read as a template | REQ-TMPL-037", {
  result <- validate_template(tmpl_fixture("appconfig", "valid"))
  qa_step("no issue is reported for a valid dta-app.yaml", 0L, nrow(result))
})

test_that("OQ-TMPL-075 | a colour that could break out of the stylesheet is app_config_invalid | REQ-TMPL-037", {
  result <- validate_template(tmpl_fixture("appconfig", "invalid"))
  rows <- result[result$code == "app_config_invalid", , drop = FALSE]
  qa_step("exactly one rejected value", 1L, nrow(rows))
  qa_step("at severity error", "error", rows$severity)
  qa_check("naming the key", grepl("colors.primary", rows$message, fixed = TRUE))
  qa_step(
    "and nothing else is reported",
    character(0), setdiff(result$code, "app_config_invalid")
  )
})

test_that("OQ-TMPL-076 | an unknown app configuration key is a warning, not an error | REQ-TMPL-037", {
  result <- validate_template(tmpl_fixture("appconfig", "unknown_key"))
  qa_step("severity", "warning", tmpl_severity(result, "app_config_unknown_key"))
  qa_check("naming the misspelled section", grepl("colours", tmpl_message(result, "app_config_unknown_key"), fixed = TRUE))
  qa_step(
    "strict validation does not abort on it", "no condition",
    tryCatch(
      {
        validate_template(tmpl_fixture("appconfig", "unknown_key"), strict = TRUE)
        "no condition"
      },
      error = function(e) "aborted"
    )
  )
})

test_that("OQ-TMPL-077 | a dta-app.yaml kept in templates/ instead of the library root is app_config_misplaced | REQ-TMPL-037", {
  result <- validate_template(tmpl_fixture("appconfig", "outside"))
  rows <- result[result$code == "app_config_misplaced", , drop = FALSE]
  qa_step("exactly one misplaced configuration file", 1L, nrow(rows))
  qa_step("severity", "error", rows$severity)
  qa_step(
    "the finding names the file in templates/",
    file.path(tmpl_fixture("appconfig", "outside"), "templates", "dta-app.yaml"), rows$file
  )
  qa_step(
    "and the library's one template is clean",
    character(0), setdiff(result$code, "app_config_misplaced")
  )
})

test_that("OQ-TMPL-078 | an invalid document name or abbreviation in dta-app.yaml is app_config_invalid naming its key | REQ-TMPL-037", {
  lib <- file.path(qa_tempdir(), "doclib")
  dir.create(file.path(lib, "templates"), recursive = TRUE)
  file.copy(tmpl_fixture("appconfig", "invalid", "templates", "t.dta-template.yaml"), file.path(lib, "templates"))
  writeLines(c(
    "kind: dta_app_config",
    "document:",
    "  name: [A, B, C, D]",
    "  abbreviation: ABCDEFGHIJKLM",
    "features:",
    "  raw_yaml_editable: maybe"
  ), file.path(lib, "dta-app.yaml"))
  result <- validate_template(lib)
  rows <- result[result$code == "app_config_invalid", , drop = FALSE]
  qa_step("one row per rejected value", 3L, nrow(rows))
  qa_step("at severity error", "error", unique(rows$severity))
  qa_check("naming features.raw_yaml_editable", any(grepl("features.raw_yaml_editable", rows$message, fixed = TRUE)))
  qa_check("naming document.name", any(grepl("document.name", rows$message, fixed = TRUE)))
  qa_check("naming document.abbreviation", any(grepl("document.abbreviation", rows$message, fixed = TRUE)))

  ok <- file.path(qa_tempdir(), "doclib_ok")
  dir.create(file.path(ok, "templates"), recursive = TRUE)
  file.copy(tmpl_fixture("appconfig", "invalid", "templates", "t.dta-template.yaml"), file.path(ok, "templates"))
  writeLines(c(
    "kind: dta_app_config",
    "document:",
    "  name: [Data Transfer Agreement, Data Transmission Specification]",
    "  abbreviation: [DTA, DTS]"
  ), file.path(ok, "dta-app.yaml"))
  qa_step("a valid pair of lists adds no rows", 0L, nrow(validate_template(ok)))
})

# ---- REQ-TMPL-031 / REQ-TMPL-037 / REQ-TMPL-042: the layout codes ----------

test_that("OQ-TMPL-100 | a template in the wrong folder is template_misplaced and invisible, a single file is not placed | REQ-TMPL-031", {
  # Fixture: templates/ok.dta-template.yaml (id misplaced_ok, in its folder),
  # templates/stray.dta-party.yaml (a party profile in templates/) and
  # stray_root.dta-vocabulary.yaml (a vocabulary directly in the root).
  root <- tmpl_fixture("c", "template_misplaced")
  result <- validate_template(root)

  qa_step(
    "one row per misplaced file, and none for the file in its own folder",
    c("stray.dta-party.yaml", "stray_root.dta-vocabulary.yaml"), sort(basename(result$file))
  )
  qa_step("both are template_misplaced", c("template_misplaced", "template_misplaced"), result$code)
  qa_step("both at severity error", c("error", "error"), result$severity)
  rows <- result[order(basename(result$file)), , drop = FALSE]
  qa_step(
    "the readable kind of each file is retained",
    c("dta_party_profile", "dta_vocabulary"), rows$kind
  )
  qa_step("and its readable id", c("stray_party", "stray_terms"), rows$id)
  qa_check(
    "the party profile's message names parties/, the folder it belongs in",
    grepl("parties/", rows$message[[1]], fixed = TRUE)
  )
  qa_check(
    "the vocabulary's message names vocabulary/",
    grepl("vocabulary/", rows$message[[2]], fixed = TRUE)
  )
  qa_step(
    "no_templates is not reported: the file in templates/ is a template, and the misplaced rows explain the rest",
    NA_character_, tmpl_severity(result, "no_templates")
  )
  err <- tryCatch(validate_template(root, strict = TRUE), error = function(e) e)
  qa_check(
    "strict validation rejects the library and names the code",
    inherits(err, "rlang_error") && grepl("template_misplaced", conditionMessage(err), fixed = TRUE)
  )

  # A single file is checked in isolation: it is not placed, whatever folder
  # it sits in.
  single <- validate_template(file.path(root, "templates", "stray.dta-party.yaml"))
  qa_step("a misplaced file given by itself adds no placement row", 0L, nrow(single))

  # Invisible means invisible to resolution too: a creation template in the
  # root cannot be the parent another template extends.
  lib <- qa_tempdir()
  dir.create(file.path(lib, "templates"))
  yaml::write_yaml(list(
    kind = "dta_creation_template", id = "root_parent", version = "1.0", abstract = TRUE
  ), file.path(lib, "root_parent.dta-template.yaml"))
  yaml::write_yaml(list(
    kind = "dta_creation_template", id = "folder_child", version = "1.0",
    abstract = TRUE, extends = "root_parent@1.0"
  ), file.path(lib, "templates", "folder_child.dta-template.yaml"))
  hidden <- validate_template(lib)
  qa_step(
    "a template extending one that sits in the root finds no parent",
    c("extends_unresolved", "template_misplaced"), sort(hidden$code)
  )
})

test_that("OQ-TMPL-101 | a kind folder given instead of the library root is one library_subfolder_given error | REQ-TMPL-031", {
  # Fixture: codes/library_subfolder_given/templates/t.dta-template.yaml. The
  # directory handed over is the templates/ folder itself.
  lib_root <- tmpl_fixture("c", "library_subfolder_given")
  folder <- file.path(lib_root, "templates")
  result <- validate_template(folder)
  qa_step("exactly one row, not one per file", 1L, nrow(result))
  qa_step("it is library_subfolder_given", "library_subfolder_given", result$code)
  qa_step("at severity error", "error", result$severity)
  qa_check(
    "the message says to validate the parent directory",
    grepl("Validate its parent directory instead", result$message, fixed = TRUE)
  )
  qa_check(
    "and advises a flat library to move its files into the kind sub-folders, noting that folder names are case-sensitive",
    grepl("flat library", result$message, fixed = TRUE) &&
      grepl("Reorganising an older repository", result$message, fixed = TRUE) &&
      grepl("case-sensitive", result$message, fixed = TRUE)
  )
  err <- tryCatch(validate_template(folder, strict = TRUE), error = function(e) e)
  qa_check(
    "strict validation rejects it and names the code",
    inherits(err, "rlang_error") && grepl("library_subfolder_given", conditionMessage(err), fixed = TRUE)
  )
  qa_step("the parent directory of the same folder is a clean library", 0L, nrow(validate_template(lib_root)))

  # The same mistake with parties/ and vocabulary/.
  copy <- qa_tempdir()
  dir.create(file.path(copy, "parties"))
  file.copy(
    file.path(tmpl_fixture("lay", "folder"), "parties", "example.dta-party.yaml"),
    file.path(copy, "parties")
  )
  dir.create(file.path(copy, "vocabulary"))
  file.copy(
    file.path(tmpl_fixture("lay", "folder"), "vocabulary", "example.dta-vocabulary.yaml"),
    file.path(copy, "vocabulary")
  )
  qa_step(
    "parties/ given by itself is the same error",
    "library_subfolder_given", validate_template(file.path(copy, "parties"))$code
  )
  qa_step(
    "so is vocabulary/",
    "library_subfolder_given", validate_template(file.path(copy, "vocabulary"))$code
  )
})

test_that("OQ-TMPL-102 | a dta-app.yaml outside the root is app_config_misplaced and unread; both spellings in the root are app_config_duplicate | REQ-TMPL-037", {
  # Fixture: dta-app.yaml in templates/ holding a bad colour, dta-app.yml in
  # parties/ holding nothing but its kind line, and one creation template.
  root <- tmpl_fixture("c", "app_config_misplaced")
  result <- validate_template(root)
  rows <- result[result$code == "app_config_misplaced", , drop = FALSE]
  qa_step(
    "both files are reported, one row each, whichever folder and spelling",
    c(file.path(root, "parties", "dta-app.yml"), file.path(root, "templates", "dta-app.yaml")),
    sort(rows$file)
  )
  qa_step("at severity error", c("error", "error"), rows$severity)
  qa_step(
    "the bad colour in the misplaced file is not parsed: no app_config_invalid",
    0L, sum(result$code == "app_config_invalid")
  )

  # Both spellings directly in the root.
  lib <- qa_tempdir()
  dir.create(file.path(lib, "templates"))
  file.copy(
    file.path(tmpl_fixture("lay", "folder"), "templates", "example.dta-template.yaml"),
    file.path(lib, "templates")
  )
  writeLines("kind: dta_app_config", file.path(lib, "dta-app.yaml"))
  writeLines("kind: dta_app_config", file.path(lib, "dta-app.yml"))
  dup <- validate_template(lib)
  qa_step(
    "each of the two spellings in the root is reported",
    c(file.path(lib, "dta-app.yaml"), file.path(lib, "dta-app.yml")),
    sort(dup$file[dup$code == "app_config_duplicate"])
  )
  qa_step("at severity error", "error", unique(dup$severity[dup$code == "app_config_duplicate"]))
  qa_step("and nothing else is reported", character(0), setdiff(dup$code, "app_config_duplicate"))
})

test_that("OQ-TMPL-103 | a file in export-templates/ that is not a .docx is an export_template_invalid warning, clutter files are not | REQ-TMPL-042", {
  # Fixture export-templates/: notes.txt, legacy.doc, README.md. The ".keep"
  # dotfile is added here, not committed: R CMD check flags hidden files
  # inside the package.
  root <- qa_tempdir()
  file.copy(
    list.files(tmpl_fixture("c", "export_template_invalid"), full.names = TRUE),
    root,
    recursive = TRUE
  )
  writeLines("", file.path(root, "export-templates", ".keep"))
  result <- validate_template(root)
  qa_step(
    "exactly the two files the app would ignore are reported",
    c("legacy.doc", "notes.txt"), sort(basename(result$file))
  )
  qa_step("as export_template_invalid", c("export_template_invalid", "export_template_invalid"), result$code)
  qa_step("at severity warning", c("warning", "warning"), result$severity)
  qa_step("with the export-template kind", c("dta_export_template", "dta_export_template"), result$kind)
  qa_step(
    "strict validation does not abort on warnings alone", "no condition",
    tryCatch(
      {
        validate_template(root, strict = TRUE)
        "no condition"
      },
      error = function(e) "aborted"
    )
  )
  qa_step(
    "the check runs whatever kinds says",
    2L, nrow(validate_template(root, kinds = "dta_vocabulary"))
  )

  # Names that are never reported: .docx files (also in capitals), Word's
  # "~$" lock files, a dotfile, files named README (.md, .txt), and a sub-folder.
  copy <- qa_tempdir()
  file.copy(list.files(root, full.names = TRUE, all.files = TRUE, no.. = TRUE), copy, recursive = TRUE)
  folder <- file.path(copy, "export-templates")
  file.remove(file.path(folder, c("notes.txt", "legacy.doc")))
  # Real Word files with a marker: a .docx without one is its own finding
  # (export_template_no_placeholders, OQ-TMPL-106).
  tmpl_docx(file.path(folder, "good.docx"))
  tmpl_docx(file.path(folder, "CAPITALS.DOCX"))
  writeLines("x", file.path(folder, "~$good.tmp"))
  dir.create(file.path(folder, "sub"))
  writeLines("x", file.path(folder, "readme.TXT"))
  writeLines("x", file.path(folder, "sub", "inner.txt"))
  qa_step("none of those is reported", 0L, nrow(validate_template(copy)))
  writeLines("x", file.path(folder, "README_old.pdf"))
  qa_step(
    "but a file that merely begins with README is",
    "README_old.pdf", basename(validate_template(copy)$file)
  )
  file.remove(file.path(folder, "README_old.pdf"))
  qa_check(
    "the base fixture library has no export-templates/ folder",
    !dir.exists(file.path(tmpl_fixture("base"), "export-templates"))
  )
  qa_step("and a library without that folder adds no row", 0L, nrow(validate_template(tmpl_fixture("base"))))
})

# ---- REQ-TMPL-045: findings the app answers by skipping the file --------------

test_that("OQ-TMPL-104 | a file whose name says one kind and whose content declares another is kind_suffix_mismatch, an error | REQ-TMPL-045", {
  root <- qa_tempdir()
  tmpl_put(root, "templates/mismatch.dta-template.yaml", tmpl_party_text("mismatch"))
  result <- validate_template(root)
  hit <- result[result$code == "kind_suffix_mismatch", ]
  qa_step("exactly one kind_suffix_mismatch row", 1L, nrow(hit))
  qa_step("at severity error", "error", hit$severity)
  qa_step("on the file with the wrong suffix", "mismatch.dta-template.yaml", basename(hit$file))
  qa_step("recording the kind the file declares", "dta_party_profile", hit$kind)
  qa_step(
    "strict validation aborts with an error condition", "aborted",
    tryCatch(
      {
        validate_template(root, strict = TRUE)
        "no condition"
      },
      error = function(e) "aborted"
    )
  )

  # Positive control: the same content under its own suffix, in its own folder.
  right <- qa_tempdir()
  tmpl_put(right, "parties/mismatch.dta-party.yaml", tmpl_party_text("mismatch"))
  qa_step(
    "the same content under the party suffix has no kind_suffix_mismatch row",
    0L, sum(validate_template(right)$code == "kind_suffix_mismatch")
  )

  # A declared kind that is not a known kind stays kind_unknown only.
  unknown <- qa_tempdir()
  tmpl_put(unknown, "templates/odd.dta-template.yaml", tmpl_party_text("odd", kind = "dta_nonsense"))
  odd <- validate_template(unknown)
  qa_step("an unknown declared kind adds no kind_suffix_mismatch row", 0L, sum(odd$code == "kind_suffix_mismatch"))
  qa_step("and is reported as kind_unknown", 1L, sum(odd$code == "kind_unknown"))
})

test_that("OQ-TMPL-105 | a party profile the reader rejects is party_profile_invalid naming the reason; a valid one beside it is not | REQ-TMPL-045", {
  root <- qa_tempdir()
  tmpl_put(root, "parties/vendor_co.dta-party.yaml", tmpl_party_text("vendor_co", role = "vendor"))
  tmpl_put(root, "parties/good_co.dta-party.yaml", tmpl_party_text("good_co"))
  result <- validate_template(root)
  hit <- result[result$code == "party_profile_invalid", ]
  qa_step("exactly one party_profile_invalid row", 1L, nrow(hit))
  qa_step("at severity error", "error", hit$severity)
  qa_step("on the profile with the unknown role", "vendor_co.dta-party.yaml", basename(hit$file))
  qa_check("whose message names the rejected value", grepl("vendor", hit$message, fixed = TRUE))
  qa_step(
    "the valid profile beside it has no row of any code",
    0L, sum(basename(result$file) == "good_co.dta-party.yaml")
  )

  # The row is added to, not instead of, the missing-field findings.
  bare <- qa_tempdir()
  tmpl_put(bare, "parties/bare.dta-party.yaml", c("kind: dta_party_profile", "role: vendor"))
  codes <- validate_template(bare)$code
  qa_check("a rejected profile without id and version also reports id_missing", "id_missing" %in% codes)
  qa_check("and version_missing", "version_missing" %in% codes)
  qa_check("and party_profile_invalid", "party_profile_invalid" %in% codes)
})

test_that("OQ-TMPL-106 | a .docx in export-templates/ without a {PLACEHOLDER} marker, or not a Word document, is export_template_no_placeholders, a warning | REQ-TMPL-045", {
  root <- qa_tempdir()
  # A valid party profile, so that no_templates does not fire on a folder with
  # nothing but Word files.
  tmpl_put(root, "parties/ok_co.dta-party.yaml", tmpl_party_text("ok_co"))
  folder <- file.path(root, "export-templates")
  dir.create(folder)
  tmpl_docx(file.path(folder, "no_markers.docx"), "Prose without any marker.")
  writeLines("not a zip archive", file.path(folder, "corrupt.docx"))
  tmpl_docx(file.path(folder, "fill_in.docx"))
  writeLines("x", file.path(folder, "~$lock.docx"))

  result <- validate_template(root)
  hit <- result[result$code == "export_template_no_placeholders", ]
  qa_step(
    "the Word file without a marker and the unreadable one are reported",
    c("corrupt.docx", "no_markers.docx"), sort(basename(hit$file))
  )
  qa_step("each at severity warning", c("warning", "warning"), hit$severity)
  qa_step("the fill-in template and the lock file are not reported, so these are the only rows", 2L, nrow(result))
  qa_step(
    "strict validation does not abort on warnings alone", "no condition",
    tryCatch(
      {
        validate_template(root, strict = TRUE)
        "no condition"
      },
      error = function(e) "aborted"
    )
  )

  # The app offers a file dta-app.yaml lists whatever it contains, so listing
  # the marker-less file removes its row and leaves the unreadable one.
  writeLines(
    c("exports:", "  word_templates:", "    - file: export-templates/no_markers.docx"),
    file.path(root, "dta-app.yaml")
  )
  listed <- validate_template(root)
  qa_step(
    "a marker-less file that dta-app.yaml lists is no longer reported",
    "corrupt.docx",
    basename(listed$file[listed$code == "export_template_no_placeholders"])
  )
})
