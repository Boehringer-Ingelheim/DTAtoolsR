#!/usr/bin/env Rscript
# Builds the .sas7bdat fixtures under inst/qualification/fx/sas/. Run from the
# repository root:  Rscript --no-init-file .github/scripts/build_sas7bdat_fixtures.R
#
# - iris.sas7bdat is haven's own SAS-written example (SAS 9.04.01M2), copied
#   unchanged; see NOTICE-haven-iris.txt next to it.
# - delivery.sas7bdat and empty.sas7bdat are written by haven/ReadStat because
#   no SAS-written file with these shapes is available. Expected values in the
#   tests come from SAS semantics and literals written in the test, never from
#   the reader under test.
# - haven::write_sas() is deprecated, so the generated files are committed and
#   the qualification suite never depends on the writer.
# - After regenerating, re-run .github/scripts/sync_qualification_manifest.R.
#
# The self-check at the end uses haven and base R only, never DTAtools.

stopifnot(utils::packageVersion("haven") >= "2.5.2")

out_dir <- file.path("inst", "qualification", "fx", "sas")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# --- iris: SAS-written, copied as is --------------------------------------
iris_src <- system.file("examples", "iris.sas7bdat", package = "haven")
stopifnot(nzchar(iris_src))
stopifnot(file.copy(iris_src, file.path(out_dir, "iris.sas7bdat"), overwrite = TRUE))

# --- delivery: six rows, one per case the reader must get right -----------
# VISTM is an hms; built by hand so this script does not need hms attached.
hms_secs <- function(secs) {
  structure(secs, class = c("hms", "difftime"), units = "secs")
}
utc <- function(x) as.POSIXct(x, tz = "UTC")

delivery <- data.frame(
  SUBJID = c("007", "  lead", "trail   ", "", "NA", "Müller"),
  AGE = c(30, 41.5, NA, 0, -2, 1e10),
  VISDT = as.Date(c(
    "1960-01-01", "2025-08-15", NA, "1959-12-31", "2000-02-29", "1970-01-01"
  )),
  VISDTM = utc(c(
    "1960-01-01 00:00:00", "2025-08-15 14:30:00", NA,
    "1959-12-31 23:59:59", "2000-02-29 12:00:00", "1970-01-01 00:00:00"
  )),
  VISTM = hms_secs(c(0, 52200, NA, 86399, 43200, 3600)),
  stringsAsFactors = FALSE
)
attr(delivery$VISDT, "format.sas") <- "DATE9"
attr(delivery$VISDTM, "format.sas") <- "DATETIME20"
attr(delivery$VISTM, "format.sas") <- "TIME8"

write_fx <- function(df, name) {
  suppressWarnings(haven::write_sas(df, file.path(out_dir, name)))
}
write_fx(delivery, "delivery.sas7bdat")
write_fx(delivery[0, ], "empty.sas7bdat")

# --- self-check ------------------------------------------------------------
path <- function(name) file.path(out_dir, name)
back <- haven::read_sas(path("delivery.sas7bdat"))
stopifnot(
  nrow(back) == 6L,
  inherits(back$VISDT, "Date"),
  inherits(back$VISDTM, "POSIXct"),
  inherits(back$VISTM, "hms"),
  identical(as.numeric(back$VISDT), as.numeric(delivery$VISDT)),
  isTRUE(all.equal(as.numeric(back$VISDTM), as.numeric(delivery$VISDTM))),
  isTRUE(all.equal(as.numeric(back$VISTM), as.numeric(delivery$VISTM)))
)

empty <- haven::read_sas(path("empty.sas7bdat"))
stopifnot(nrow(empty) == 0L, identical(names(empty), names(delivery)))

# Byte oracle: the SAS numbers that independent date arithmetic predicts must
# sit in the file as little-endian doubles. 20315 is the Unix-epoch day count
# of 2025-08-15, i.e. the value the file would hold had the writer used the
# wrong epoch; it must not appear. (0 and -1 are useless as probes.)
bytes <- readBin(path("delivery.sas7bdat"), "raw", n = file.size(path("delivery.sas7bdat")))
hits <- function(v) {
  length(grepRaw(writeBin(v, raw(), endian = "little"), bytes, fixed = TRUE, all = TRUE))
}
present <- c(23968, 2070887400, 14669, 1267444800, 315619200, 52200, 3653)
absent <- 20315
counts <- vapply(c(present, absent), hits, integer(1))
names(counts) <- c(present, absent)
cat("byte-oracle hit counts (little-endian double):\n")
print(counts)
stopifnot(all(counts[as.character(present)] >= 1L), counts[["20315"]] == 0L)

cat("sas fixtures OK:\n")
for (f in list.files(out_dir)) {
  cat(sprintf("  %-28s %7d bytes\n", f, file.size(file.path(out_dir, f))))
}
