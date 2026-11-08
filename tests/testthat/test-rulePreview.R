# rule_preview() had no test anywhere. Its branching is exactly the kind an
# off-by-one hides in: "more than 5" takes the first four plus the last, so
# the boundary is between 5 (all shown) and 6 (the fifth rule silently
# dropped, not just elided). Pin both sides of that boundary, plus the
# no-rules case, rather than a single mid-range collection that would pass
# even if the boundary were wrong.

# A minimal, valid rule per id -- DTARuleColUnique needs nothing but a column
# name, so it is the cheapest way to get a rule with a chosen @id.
mk_rules <- function(n) {
  stats::setNames(
    lapply(seq_len(n), function(i) DTARuleColUnique(id = paste0("rule", i), columns = "A")),
    paste0("rule", seq_len(n))
  )
}
rule_preview_col <- list(A = DTAColumnSpec(id = "A", type = "SAS Char"))

test_that("rule_preview() reports 'not set' when there are no rules", {
  specs <- DTAColumnSpecCollection(columns = rule_preview_col, rules = NULL)
  expect_identical(rule_preview(specs), "not set")
})

test_that("rule_preview() lists every id at exactly 5 rules", {
  specs <- DTAColumnSpecCollection(columns = rule_preview_col, rules = mk_rules(5))
  expect_identical(rule_preview(specs), "rule1, rule2, rule3, rule4, rule5")
})

test_that("rule_preview() truncates at exactly 6 rules", {
  # The fifth rule is neither in the "first four" nor the "last" -- it drops
  # out of the preview entirely once a sixth rule is added.
  specs <- DTAColumnSpecCollection(columns = rule_preview_col, rules = mk_rules(6))
  expect_identical(rule_preview(specs), "rule1, rule2, rule3, rule4, ..., rule6")
})
