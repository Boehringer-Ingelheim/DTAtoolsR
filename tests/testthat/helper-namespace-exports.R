# The exports NAMESPACE declares, read from the file.
#
# getNamespaceExports() is the same list in an installed package, but under
# pkgload::load_all() (how the suite usually runs) it leaves out an export that
# is not an object of the namespace itself. `names`, `print` and `labels` are
# exports of that kind: they are base functions, and S7_on_build() (zzz.R)
# removes the placeholders S7 1.0.0 leaves for them in the namespace.
declared_exports <- function() {
  dir <- dirname(system.file("NAMESPACE", package = "DTAtools"))
  parseNamespaceFile(basename(dir), dirname(dir))$exports
}
