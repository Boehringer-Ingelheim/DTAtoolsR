# A DTAColumnSpecCollection whose RULES skip the cross-object check.
#
# The class validator refuses rules that contradict their columns (a range on a
# text column, a rule naming an undeclared column). The engine must still cope
# with such objects -- a session restored from an old RDS, or specs built before
# the check existed -- so tests of that robustness build them through here.
# Columns are validated normally; only the rule/column consistency is bypassed.
unchecked_collection <- function(columns, rules = NULL) {
  specs <- DTAColumnSpecCollection(columns = columns)
  if (length(rules) > 0) {
    S7::prop(specs, "rules", check = FALSE) <- rules
  }
  specs
}
