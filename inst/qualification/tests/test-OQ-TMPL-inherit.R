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

# ---- REQ-TMPL-022: the four value states -----------------------------------

test_that("OQ-TMPL-039 | an absent section inherits the parent's, and the build succeeds | REQ-TMPL-022", {
  result <- validate_template(tmpl_fixture("inherit", "four-states"))
  absent_rows <- tmpl_rows_for(result, "child_absent.dta-template.yaml")
  qa_step("zero issues -- fs_parent's own dataset carried the build", 0L, nrow(absent_rows))
})

test_that("OQ-TMPL-040 | an explicit null and an empty collection both wipe the section identically | REQ-TMPL-022", {
  # PINNED (REQ-TMPL-022 notes): validate_template() cannot currently tell
  # these two states apart for a collection section, because every consumer
  # reads the merged value through `%||% list()`. Demonstrated on two
  # different section shapes -- datasets: (a sequence) and base: (a mapping)
  # -- so this is not an accident of one section's implementation.
  result <- validate_template(tmpl_fixture("inherit", "four-states"))

  drop_rows <- tmpl_rows_for(result, "child_drop.dta-template.yaml")
  empty_rows <- tmpl_rows_for(result, "child_empty.dta-template.yaml")
  qa_step("datasets: null and datasets: [] both leave zero issues", c(0L, 0L), c(nrow(drop_rows), nrow(empty_rows)))

  base_drop_rows <- tmpl_rows_for(result, "child_base_drop.dta-template.yaml")
  base_empty_rows <- tmpl_rows_for(result, "child_base_empty.dta-template.yaml")
  qa_step("base: null and base: {} both leave zero issues too", c(0L, 0L), c(nrow(base_drop_rows), nrow(base_empty_rows)))
})

test_that("OQ-TMPL-041 | a value state's own new content is checked as if authored standalone | REQ-TMPL-022", {
  result <- validate_template(tmpl_fixture("inherit", "four-states"))
  value_rows <- tmpl_rows_for(result, "child_value.dta-template.yaml")

  qa_step(
    "unlike absent/empty/null, a genuinely new (and broken) reference is caught",
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
