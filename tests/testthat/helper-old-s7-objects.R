# Objects as DTAtools saved them under S7 < 1.0.0.
#
# S7 0.2.x kept an object's class in the attribute `S7_class`; S7 1.0.0 keeps a
# reference to it in `_S7_class` and marks the object with `_S7_version`. The
# tests that age an object (to see what upgrade_dta() restores) stamp the class
# the old way, so they exercise what a file saved by 0.29.0 or earlier holds.
stamp_old_class <- function(obj, cls) {
  attr(obj, "_S7_class") <- NULL
  attr(obj, "_S7_version") <- NULL
  attr(obj, "S7_class") <- cls
  obj
}
