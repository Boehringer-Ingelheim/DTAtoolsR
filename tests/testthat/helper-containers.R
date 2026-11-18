# Path of a committed container fixture; fails (never skips) when missing.
cont_fx <- function(name) {
  p <- system.file("qualification", "fx", "container", name, package = "DTAtools")
  testthat::expect_true(nzchar(p), info = paste("container fixture missing:", name))
  p
}
# The literal data every fixture was built from (the fixture builder,
# .github/scripts/build_container_fixtures.R, writes the same text; a
# disagreement between the two is a test failure, which is the point).
CONT_AE_CSV <- "USUBJID,AETERM,AESEQ\nS-001,HEADACHE,1\nS-002,NAUSEA,2\n"
CONT_DM_CSV <- "USUBJID,AGE\nS-001,34\nS-002,51\n"
# Write `text` (or raw bytes) to a fresh temp file called `name`, return its path.
cont_write <- function(name, text, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  p <- file.path(d, name)
  writeBin(if (is.raw(text)) text else charToRaw(text), p)
  p
}
# A zip built at test time from name = content pairs (content: text or raw).
cont_zip <- function(entries, name = "test.zip", env = parent.frame()) {
  src <- withr::local_tempdir(.local_envir = env)
  for (nm in names(entries)) {
    dir.create(dirname(file.path(src, nm)), recursive = TRUE, showWarnings = FALSE)
    writeBin(if (is.raw(entries[[nm]])) entries[[nm]] else charToRaw(entries[[nm]]), file.path(src, nm))
  }
  z <- file.path(withr::local_tempdir(.local_envir = env), name)
  zip::zip(z, files = names(entries), root = src)
  z
}
# Column specs for a domain. vs declares VSDT as a SAS date with DATE9.
cont_specs <- function(domain) {
  col <- function(id, type, ...) DTAtools::DTAColumnSpec(id = id, type = type, nullable = FALSE, ...)
  cols <- switch(domain,
    ae = list(col("USUBJID", "SAS Char"), col("AETERM", "SAS Char"), col("AESEQ", "SAS Num")),
    dm = list(col("USUBJID", "SAS Char"), col("AGE", "SAS Num")),
    vs = list(
      col("USUBJID", "SAS Char"), col("VSTESTCD", "SAS Char"), col("VSSTRESN", "SAS Num"),
      col("VSDT", "SAS Date", format = "SAS DATE9.")
    )
  )
  DTAtools::DTAColumnSpecCollection(
    columns = stats::setNames(cols, vapply(cols, function(x) x@id, character(1))),
    rules = list()
  )
}
# A DTA with datasets ae, dm, vs; one file entry each, of `type`, named
# "<id>.<ext>", optionally declaring `container` (a reference: id, file name or
# "none") and carrying the document-level `containers` (a list of
# DTAContainer).
cont_dta <- function(type = "csv", container = NULL, ext = type, containers = list()) {
  mk <- function(id) {
    DTAtools::DTADataSetTabular(
      id = id, specs = cont_specs(id),
      files = list(DTAtools::DTAFileFactory(type = type, filename = paste0(id, ".", ext), container = container))
    )
  }
  DTAtools::DTA(datasets = list(mk("ae"), mk("dm"), mk("vs")), containers = containers)
}

# --- Shared by the delivery, container and provenance tests -------------------

# load_delivery() without its summary message.
ld <- function(...) suppressMessages(load_delivery(...))

# The last_delivery table of a result.
plan_of <- function(d) attr(d, "last_delivery")

# The rows of a plan with one status.
rows_of <- function(plan, status) plan[plan$status == status, , drop = FALSE]

# A tabular dataset; specs default to the domain's.
tab_ds <- function(id, files, specs = cont_specs(id)) {
  DTADataSetTabular(id = id, specs = specs, files = files)
}

# A file dataset.
file_ds <- function(id, ...) DTADataSetFile(id = id, files = list(...))

dta_of <- function(...) DTA(datasets = list(...))
# A table bound in a dataset of a DTA, as a plain data.frame.
cont_table <- function(dta, dataset, table) {
  as.data.frame(DTAtools::tables(DTAtools::datasets(dta, dataset))[[table]])
}
# The first `n` bytes of a fixture, written to a temp file called `name`.
cont_cut <- function(fixture, n, name = fixture, env = parent.frame()) {
  b <- readBin(cont_fx(fixture), "raw", file.size(cont_fx(fixture)))
  cont_write(name, b[seq_len(n)], env = env)
}
