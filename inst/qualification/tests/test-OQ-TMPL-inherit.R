# extends: inheritance, sealed paths, and controlled vocabulary.
# REQ-TMPL-018 .. REQ-TMPL-025.
#
# The four value states (REQ-TMPL-022) are the whole point of the inheritance
# mechanism, and the easiest thing to get wrong -- so every one of the four is
# exercised on the SAME parent, changing only the one section under test, and
# each result is read back through validate_template()'s pass/fail signal
# rather than assumed from reading the merge code.

tmpl_fixture <- function(...) file.path(qa_suite_root(), "fixtures", "tpl", ...)

tmpl_severity <- function(result, code) {
  hit <- result$severity[result$code == code]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

tmpl_message <- function(result, code) {
  hit <- result$message[result$code == code]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

# Rows belonging to one file in a multi-file directory result, by basename --
# the four-states and chain fixtures deliberately put several templates in one
# directory so a reader can see all of them resolve against the same parent.
tmpl_rows_for <- function(result, basename_) {
  result[basename(result$file) == basename_, , drop = FALSE]
}

# ---- REQ-TMPL-019: extends_unresolved / extends_cycle ----------------------

test_that("OQ-TMPL-033 | extends naming an unresolvable reference is a warning | REQ-TMPL-019", {
  result <- validate_template(tmpl_fixture("codes", "extends_unres_instantiate"))
  qa_step("severity is warning", "warning", tmpl_severity(result, "extends_unresolved"))
  qa_check(
    "naming the unresolvable reference",
    grepl("nonexistent_parent@1.0", tmpl_message(result, "extends_unresolved"), fixed = TRUE)
  )
})

test_that("OQ-TMPL-034 | a chain that revisits itself is a cycle, not an infinite loop | REQ-TMPL-019", {
  result <- validate_template(tmpl_fixture("codes", "extends_cycle"))

  a_row <- tmpl_rows_for(result, "a.dta-template.yaml")
  b_row <- tmpl_rows_for(result, "b.dta-template.yaml")
  qa_step("both files in the cycle are reported extends_cycle", c("extends_cycle", "extends_cycle"), sort(c(
    a_row$code[a_row$code == "extends_cycle"], b_row$code[b_row$code == "extends_cycle"]
  )))
  qa_check(
    "the cycle message traces the actual path, starting from cyc_a",
    grepl("cyc_a@1.0 -> cyc_b@1.0 -> cyc_a@1.0", a_row$message[a_row$code == "extends_cycle"], fixed = TRUE)
  )
})

# ---- REQ-TMPL-020: instantiate_failed coupling + the abstract exemption ---

test_that("OQ-TMPL-035 | an unresolved extends chain also fails the dry-run build | REQ-TMPL-020", {
  result <- validate_template(tmpl_fixture("codes", "extends_unres_instantiate"))
  qa_step("severity is error", "error", tmpl_severity(result, "instantiate_failed"))
  qa_step(
    "both rows report the same underlying cause",
    tmpl_message(result, "extends_unresolved"), tmpl_message(result, "instantiate_failed")
  )
})

test_that("OQ-TMPL-036 | an abstract template is exempt from the dry-run, even with a broken extends | REQ-TMPL-020", {
  result <- validate_template(tmpl_fixture("inherit", "abstract-extends-unresolved"))
  qa_step("exactly one row", 1L, nrow(result))
  qa_step(
    "extends_unresolved is still reported -- the reference really is broken",
    "warning", tmpl_severity(result, "extends_unresolved")
  )
  qa_check(
    "but instantiate_failed is NOT -- abstract: true means nothing here is ever built",
    is.na(tmpl_severity(result, "instantiate_failed"))
  )
})

test_that("OQ-TMPL-066 | metadata-only abstract bases require concrete children to provide datasets | REQ-TMPL-020", {
  root <- qa_tempdir()
  yaml::write_yaml(list(
    kind = "dta_creation_template", id = "shared_metadata", version = "1.0",
    abstract = TRUE, base = list(metadata = list(title = "Shared transfer title"))
  ), file.path(root, "base.dta-template.yaml"))
  child <- list(
    kind = "dta_creation_template", id = "child", version = "1.0",
    extends = "shared_metadata@1.0",
    datasets = list(list(
      id = "observations", type = "tabular",
      files = list(filename = "observations.tsv", type = "tsv"),
      columns = list(list(id = "SUBJECT", label = "Subject", type = "SAS Char"))
    ))
  )
  path <- file.path(root, "child.dta-template.yaml")
  yaml::write_yaml(child, path)

  valid <- validate_template(root, strict = TRUE)
  qa_step("abstract base and concrete child both pass strict lint", 0L, nrow(valid))

  child$datasets <- NULL
  yaml::write_yaml(child, path)
  invalid <- validate_template(root)
  rows <- tmpl_rows_for(invalid, "child.dta-template.yaml")
  qa_step("a metadata-only concrete child fails the build", "instantiate_failed", rows$code)
  qa_step("the missing dataset is an error", "error", rows$severity)
  qa_check("the diagnostic identifies the missing dataset requirement", grepl(
    "at least one dataset", rows$message,
    fixed = TRUE
  ))
  qa_step("the abstract parent remains valid", 0L, nrow(tmpl_rows_for(invalid, "base.dta-template.yaml")))
})

# ---- REQ-TMPL-021: sealed_violation / sealed_path_unknown ------------------

test_that("OQ-TMPL-037 | overriding a sealed path is reported, and fails the build | REQ-TMPL-021", {
  result <- validate_template(tmpl_fixture("codes", "sealed_violation"))
  qa_step("severity is warning", "warning", tmpl_severity(result, "sealed_violation"))
  qa_step("and also error, on instantiate_failed", "error", tmpl_severity(result, "instantiate_failed"))
  qa_check(
    "naming the sealed field and the child that changed it",
    grepl("seal_c", tmpl_message(result, "sealed_violation"), fixed = TRUE) &&
      grepl("which an ancestor sealed", tmpl_message(result, "sealed_violation"), fixed = TRUE)
  )
})

test_that("OQ-TMPL-038 | a sealed path matching nothing is reported, not silently harmless | REQ-TMPL-021", {
  result <- validate_template(tmpl_fixture("codes", "sealed_path_unknown"))
  qa_step("severity is warning", "warning", tmpl_severity(result, "sealed_path_unknown"))
  qa_check(
    "naming the path that protects nothing",
    grepl("nonexistent_field", tmpl_message(result, "sealed_path_unknown"), fixed = TRUE)
  )
})

test_that("OQ-TMPL-061 | a child adding an option that writes a sealed field is a sealed violation too | REQ-TMPL-021", {
  # Unlike OQ-TMPL-037, nothing in the merged DEFINITION TREE changes here:
  # the child adds no base: section at all. The field is only reached once
  # create_dta_from_template() applies the new option's effect -- the seal
  # has to be checked against what an option WOULD write, not only against
  # base:'s own resolved value.
  result <- validate_template(tmpl_fixture("codes", "sealed_violation_option"))
  qa_step("severity is warning", "warning", tmpl_severity(result, "sealed_violation"))
  qa_step("and also error, on instantiate_failed", "error", tmpl_severity(result, "instantiate_failed"))
  qa_check(
    "naming the sealed field and the child that added the option",
    grepl("so_c", tmpl_message(result, "sealed_violation"), fixed = TRUE) &&
      grepl("base.metadata.title", tmpl_message(result, "sealed_violation"), fixed = TRUE)
  )
})

# ---- REQ-TMPL-022: the four value states -----------------------------------

test_that("OQ-TMPL-039 | an absent section inherits the parent's, and the build succeeds | REQ-TMPL-022", {
  result <- validate_template(tmpl_fixture("inherit", "four-states"))
  absent_rows <- tmpl_rows_for(result, "child_absent.dta-template.yaml")
  qa_step("zero issues -- fs_parent's own dataset carried the build", 0L, nrow(absent_rows))
})

test_that("OQ-TMPL-040 | an explicit null and an empty collection both wipe the section identically | REQ-TMPL-022 REQ-TMPL-020", {
  # PINNED (REQ-TMPL-022 notes): validate_template() cannot currently tell
  # these two states apart for a collection section, because every consumer
  # reads the merged value through `%||% list()`. Demonstrated on two
  # different section shapes -- datasets: (a sequence) and base: (a mapping)
  # -- so this is not an accident of one section's implementation.
  result <- validate_template(tmpl_fixture("inherit", "four-states"))

  drop_rows <- tmpl_rows_for(result, "child_drop.dta-template.yaml")
  empty_rows <- tmpl_rows_for(result, "child_empty.dta-template.yaml")
  qa_step(
    "datasets: null and datasets: [] both fail the concrete dataset requirement",
    c("instantiate_failed", "instantiate_failed"), c(drop_rows$code, empty_rows$code)
  )
  qa_step("both missing-dataset diagnostics are errors", c("error", "error"), c(drop_rows$severity, empty_rows$severity))

  base_drop_rows <- tmpl_rows_for(result, "child_base_drop.dta-template.yaml")
  base_empty_rows <- tmpl_rows_for(result, "child_base_empty.dta-template.yaml")
  qa_step("base: null and base: {} both leave zero issues too", c(0L, 0L), c(nrow(base_drop_rows), nrow(base_empty_rows)))
})

test_that("OQ-TMPL-041 | a value state's own new content is checked as if authored standalone | REQ-TMPL-022", {
  result <- validate_template(tmpl_fixture("inherit", "four-states"))
  value_rows <- tmpl_rows_for(result, "child_value.dta-template.yaml")

  qa_step(
    "a new unresolved dataset reference is identified specifically",
    "warning", tmpl_severity(value_rows, "dataset_template_unresolved")
  )
  qa_step("and the build fails as a consequence", "error", tmpl_severity(value_rows, "instantiate_failed"))
})

# ---- REQ-TMPL-023: multi-level chain ---------------------------------------

test_that("OQ-TMPL-042 | a fix two levels up an extends chain reaches a grandchild that says nothing itself | REQ-TMPL-023", {
  result <- validate_template(tmpl_fixture("inherit", "chain"))

  gp_rows <- tmpl_rows_for(result, "grandparent.dta-template.yaml")
  qa_step(
    "the grandparent's own broken dataset reference is reported on ITS row",
    "warning", tmpl_severity(gp_rows, "dataset_template_unresolved")
  )

  parent_rows <- tmpl_rows_for(result, "parent.dta-template.yaml")
  child_rows <- tmpl_rows_for(result, "child.dta-template.yaml")
  qa_step(
    "the parent's override and the child that never mentions datasets: at all both build clean",
    c(0L, 0L), c(nrow(parent_rows), nrow(child_rows))
  )
})

# ---- REQ-TMPL-024: vocabulary_invalid / _unresolved / _extends_failed -----

test_that("OQ-TMPL-043 | a vocabulary with neither terms nor extends is invalid | REQ-TMPL-024", {
  result <- validate_template(tmpl_fixture("codes", "vocabulary_invalid"))
  qa_step("severity is error", "error", tmpl_severity(result, "vocabulary_invalid"))
})

test_that("OQ-TMPL-044 | a vocabulary extending an absent parent is unresolved, not invalid | REQ-TMPL-024", {
  result <- validate_template(tmpl_fixture("codes", "vocabulary_unresolved"))
  qa_step("severity is warning", "warning", tmpl_severity(result, "vocabulary_unresolved"))
  qa_check(
    "naming the unresolved parent",
    grepl("nonexistent_parent_voc@1.0", tmpl_message(result, "vocabulary_unresolved"), fixed = TRUE)
  )
})

test_that("OQ-TMPL-045 | a vocabulary extends cycle is reported on every file in it | REQ-TMPL-024", {
  result <- validate_template(tmpl_fixture("codes", "vocabulary_extends_failed"))
  a_row <- tmpl_rows_for(result, "a.dta-vocabulary.yaml")
  b_row <- tmpl_rows_for(result, "b.dta-vocabulary.yaml")
  qa_step(
    "both files report vocabulary_extends_failed, at error",
    c("error", "error"), c(tmpl_severity(a_row, "vocabulary_extends_failed"), tmpl_severity(b_row, "vocabulary_extends_failed"))
  )
})

# ---- REQ-TMPL-018 (positive case): a clean binding, include/exclude -------

test_that("OQ-TMPL-046 | a values_from binding that resolves cleanly is accepted without comment | REQ-TMPL-018", {
  result <- validate_template(tmpl_fixture("inherit", "vocab-basic"))
  qa_step("zero issues", 0L, nrow(result))
})

test_that("OQ-TMPL-047 | include and exclude each select a clean subset of a vocabulary's terms | REQ-TMPL-018", {
  result <- validate_template(tmpl_fixture("inherit", "vocab-incl-excl"))
  qa_step("zero issues across both the include-based and the exclude-based binding", 0L, nrow(result))
})

# ---- REQ-TMPL-025: vocabulary term algebra (extends/remove_terms/add_terms)

test_that("OQ-TMPL-048 | a child vocabulary's terms are the parent's, minus removed, plus added | REQ-TMPL-025", {
  result <- validate_template(tmpl_fixture("inherit", "vocab-ext-chain"))

  ok_rows <- tmpl_rows_for(result, "ds_ok.dta-dataset-template.yaml")
  qa_step(
    "including an inherited code (A) and the child's own added code (D) both resolve",
    0L, nrow(ok_rows)
  )

  removed_rows <- tmpl_rows_for(result, "ds_rem.dta-dataset-template.yaml")
  qa_step(
    "including the code the child removed (C) fails",
    "error", tmpl_severity(removed_rows, "values_from_terms_invalid")
  )
  msg <- tmpl_message(removed_rows, "values_from_terms_invalid")
  qa_check("naming C as unavailable", grepl("\"C\"", msg, fixed = TRUE))
  qa_check(
    "and listing exactly what remains: the surviving parent terms plus the added one",
    grepl("\"A\", \"B\", and \"D\"", msg, fixed = TRUE)
  )
})

# ---- REQ-TMPL-038 / REQ-TMPL-039: mixins: composition -----------------------
#
# Expected values come from the requirements, not from a run: a composed child
# is valid iff the mixins supplied what it needs, and the negative control
# (the same parent without mixins) shows that they really did.

test_that("OQ-TMPL-079 | a child composed from disjoint mixins carries their content and is valid | REQ-TMPL-038", {
  result <- validate_template(tmpl_fixture("inherit", "mixins"))

  child_rows <- tmpl_rows_for(result, "child.dta-template.yaml")
  qa_step(
    "extends + two disjoint mixins (a base leaf, an option, a dataset): zero issues",
    0L, nrow(child_rows)
  )
  qa_step(
    "the mixin templates themselves (abstract, partial) are clean too",
    c(0L, 0L),
    c(
      nrow(tmpl_rows_for(result, "mixin_privacy.dta-template.yaml")),
      nrow(tmpl_rows_for(result, "mixin_assay.dta-template.yaml"))
    )
  )

  # Content check: the parent has no dataset and the child writes none, so the
  # concrete-dataset requirement can only be met by the mixin's dataset.
  bare_rows <- tmpl_rows_for(result, "child_bare.dta-template.yaml")
  qa_step(
    "the same parent WITHOUT the mixins has no dataset and fails the build",
    "instantiate_failed", bare_rows$code
  )

  qa_step(
    "a descendant of the composed child inherits the content without listing the mixins",
    0L, nrow(tmpl_rows_for(result, "grandchild.dta-template.yaml"))
  )
  qa_step(
    "a template using only mixins (single-string form, no extends) needs no datasets of its own",
    0L, nrow(tmpl_rows_for(result, "mixins_only.dta-template.yaml"))
  )
  qa_step("no row anywhere in the directory is an error other than the control", 1L, sum(result$severity == "error"))
})

test_that("OQ-TMPL-080 | two mixins providing the same dataset key conflict, an error not an override | REQ-TMPL-039 REQ-TMPL-038", {
  result <- validate_template(tmpl_fixture("codes", "mixin_conflict"))
  qa_step("mixin_conflict is an error", "error", tmpl_severity(result, "mixin_conflict"))
  qa_step("and the concrete template also fails the build", "error", tmpl_severity(result, "instantiate_failed"))
  msg <- tmpl_message(result, "mixin_conflict")
  qa_check(
    "naming the section, the key, and both contributors",
    grepl("datasets", msg, fixed = TRUE) && grepl("obs", msg, fixed = TRUE) &&
      grepl("mc_a@1.0", msg, fixed = TRUE) && grepl("mc_b@1.0", msg, fixed = TRUE)
  )
})

test_that("OQ-TMPL-081 | a non-abstract mixin and a malformed mixins: value are mixin_invalid errors | REQ-TMPL-039", {
  result <- validate_template(tmpl_fixture("codes", "mixin_invalid"))

  child <- tmpl_rows_for(result, "child.dta-template.yaml")
  qa_step("naming a concrete template as a mixin: error", "error", tmpl_severity(child, "mixin_invalid"))
  qa_step("and the concrete child also fails the build", "error", tmpl_severity(child, "instantiate_failed"))
  qa_check("naming the offending mixin", grepl("mi_concrete@1.0", tmpl_message(child, "mixin_invalid"), fixed = TRUE))

  qa_step(
    "a duplicated reference in the file itself: error",
    "error", tmpl_severity(tmpl_rows_for(result, "bad_shape.dta-template.yaml"), "mixin_invalid")
  )
  qa_step(
    "a mapping where a list belongs: error",
    "error", tmpl_severity(tmpl_rows_for(result, "bad_shape_mapping.dta-template.yaml"), "mixin_invalid")
  )
  qa_check(
    "an abstract template with a malformed mixins: gets no instantiate_failed",
    is.na(tmpl_severity(tmpl_rows_for(result, "bad_shape.dta-template.yaml"), "instantiate_failed"))
  )
})

test_that("OQ-TMPL-082 | an unresolvable mixin is a warning, and only a concrete template also fails the build | REQ-TMPL-039", {
  result <- validate_template(tmpl_fixture("codes", "mixin_unresolved"))
  concrete <- tmpl_rows_for(result, "concrete.dta-template.yaml")
  abstract <- tmpl_rows_for(result, "abstract.dta-template.yaml")

  qa_step("concrete: mixin_unresolved is a warning", "warning", tmpl_severity(concrete, "mixin_unresolved"))
  qa_step("concrete: also instantiate_failed, an error", "error", tmpl_severity(concrete, "instantiate_failed"))
  qa_check(
    "naming the missing reference",
    grepl("nonexistent_mixin@1.0", tmpl_message(concrete, "mixin_unresolved"), fixed = TRUE)
  )
  qa_step("abstract: mixin_unresolved is still a warning", "warning", tmpl_severity(abstract, "mixin_unresolved"))
  qa_check("abstract: but no instantiate_failed", is.na(tmpl_severity(abstract, "instantiate_failed")))
  qa_step("abstract: exactly one row", 1L, nrow(abstract))
})

test_that("OQ-TMPL-083 | mixin content reaching an ancestor's sealed path is a sealed violation | REQ-TMPL-039", {
  # Nothing in the child or the parent changes base.metadata.title: only the
  # mixin's option targets it, so the seal has to be enforced on what the
  # mixins add, not only on the child's own text.
  result <- validate_template(tmpl_fixture("codes", "sealed_violation_mixin"))
  qa_step("severity is warning", "warning", tmpl_severity(result, "sealed_violation"))
  qa_step("and also error, on instantiate_failed", "error", tmpl_severity(result, "instantiate_failed"))
  qa_check(
    "naming the sealed path and the child",
    grepl("base.metadata.title", tmpl_message(result, "sealed_violation"), fixed = TRUE) &&
      grepl("sm_c", tmpl_message(result, "sealed_violation"), fixed = TRUE)
  )
  qa_check("and it is not reported as a mixin code", all(!grepl("^mixin_", result$code)))
})

test_that("OQ-TMPL-084 | a child may modify and remove mixin-provided entries like any inherited entry | REQ-TMPL-038", {
  result <- validate_template(tmpl_fixture("inherit", "mixin_edit"))

  # Both mixin datasets (obs, extra) are keyed entries of the folded parent.
  # child_ok restates obs (same key, so a modification rather than an addition
  # colliding with the mixin) and removes extra; a mixin entry that could not be
  # modified or removed would surface as an error here.
  qa_step(
    "modifying one mixin dataset and removing another: zero issues",
    0L, nrow(tmpl_rows_for(result, "child_ok.dta-template.yaml"))
  )

  # Control: removing every mixin dataset leaves none, which only holds if the
  # removals took effect on mixin-provided entries.
  gone <- tmpl_rows_for(result, "child_remove_all.dta-template.yaml")
  qa_step("removing every mixin-provided dataset fails the build", "instantiate_failed", gone$code)
  qa_step("as an error", "error", gone$severity)
  qa_check("because no dataset remains", grepl("at least one dataset", gone$message, fixed = TRUE))
})

test_that("OQ-TMPL-085 | re-listing a mixin an ancestor already mixes in is a conflict | REQ-TMPL-038 REQ-TMPL-039", {
  result <- validate_template(tmpl_fixture("codes", "mixin_relist"))

  child <- tmpl_rows_for(result, "child.dta-template.yaml")
  qa_step("mixin_conflict is an error", "error", tmpl_severity(child, "mixin_conflict"))
  qa_step("and the concrete grandchild also fails the build", "error", tmpl_severity(child, "instantiate_failed"))
  qa_check(
    "naming the mixin, as already mixed in by an ancestor",
    grepl("rl_m@1.0", tmpl_message(child, "mixin_conflict"), fixed = TRUE) &&
      grepl("ancestor", tmpl_message(child, "mixin_conflict"), fixed = TRUE)
  )
  qa_step(
    "control: the same descendant without the redundant line is clean",
    0L, nrow(tmpl_rows_for(result, "child_ok.dta-template.yaml"))
  )
})
