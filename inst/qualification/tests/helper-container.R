# Helpers for the container-delivery cases (area CONT).
#
# Fixtures are read from fx/container/ of the suite root, so the same files
# serve a source tree and an installed copy. Files the cases build themselves
# (zips, cut or compressed copies of fixtures) are written under qa_tempdir():
# every helper that creates a file takes `env` and hands it on, because
# qa_tempdir() defers the deletion of its directory to `env`, and with the
# default it would delete the directory the moment the helper returned.
#
# The literal data every fixture was built from is restated here, so a
# fixture that disagrees with it fails a case instead of being trusted.

cont_fx <- function(name) file.path(qa_suite_root(), "fx", "container", name)

CONT_AE_CSV <- "USUBJID,AETERM,AESEQ\nS-001,HEADACHE,1\nS-002,NAUSEA,2\n"
CONT_DM_CSV <- "USUBJID,AGE\nS-001,34\nS-002,51\n"

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
# "<id>.<ext>", optionally declaring `container`.
cont_dta <- function(type = "csv", container = NULL, ext = type) {
  mk <- function(id) {
    DTAtools::DTADataSetTabular(
      id = id, specs = cont_specs(id),
      files = list(DTAtools::DTAFileFactory(type = type, filename = paste0(id, ".", ext), container = container))
    )
  }
  DTAtools::DTA(datasets = list(mk("ae"), mk("dm"), mk("vs")))
}

# A table bound in a dataset of a DTA, as a plain data.frame.
cont_table <- function(dta, dataset, table) {
  as.data.frame(DTAtools::tables(DTAtools::datasets(dta, dataset))[[table]])
}

# A directory for load_delivery() to extract into; it does not exist yet.
cont_exdir <- function(id, env = parent.frame()) file.path(qa_tempdir(env), id)

# Write `text` (or raw bytes) to a fresh temp file called `name`, return its path.
cont_write <- function(name, text, env = parent.frame()) {
  p <- file.path(qa_tempdir(env), name)
  writeBin(if (is.raw(text)) text else charToRaw(text), p)
  p
}

# A zip built at test time from name = content pairs (content: text or raw).
cont_zip <- function(entries, name = "test.zip", env = parent.frame()) {
  src <- qa_tempdir(env)
  for (nm in names(entries)) {
    dir.create(dirname(file.path(src, nm)), recursive = TRUE, showWarnings = FALSE)
    writeBin(if (is.raw(entries[[nm]])) entries[[nm]] else charToRaw(entries[[nm]]), file.path(src, nm))
  }
  z <- file.path(qa_tempdir(env), name)
  zip::zip(z, files = names(entries), root = src)
  z
}

# The first `n` bytes of a fixture, written to a temp file called `name`.
cont_cut <- function(fixture, n, name = fixture, env = parent.frame()) {
  b <- readBin(cont_fx(fixture), "raw", file.size(cont_fx(fixture)))
  cont_write(name, b[seq_len(n)], env = env)
}

# A shiny fileInput row for a file on disk.
cont_fi <- function(path, name = basename(path)) {
  data.frame(name = name, size = file.size(path), type = "", datapath = path, stringsAsFactors = FALSE)
}

# ---- helpers shared by the case files -----------------------------------------

# The bytes of a fixture.
cont_bytes <- function(fixture) readBin(cont_fx(fixture), "raw", file.size(cont_fx(fixture)))

# `text` or raw bytes, gzip-compressed into a temp file called `name`.
cont_gz <- function(x, name, env = parent.frame()) {
  p <- file.path(qa_tempdir(env), name)
  con <- gzfile(p, "wb")
  writeBin(if (is.raw(x)) x else charToRaw(x), con)
  close(con)
  p
}

# A copy of the file `path` with every occurrence of the bytes of each `from`
# replaced by the same number of bytes of the matching `to`, written to a temp
# file called `name`. Entry names that the file system of a test machine could
# not create, or that zip::zip() would not write, are built this way from a
# stand-in name of the same length. Aborts the case if a name is not found.
cont_swap <- function(path, from, to, name = basename(path), env = parent.frame()) {
  stopifnot(nchar(to, type = "bytes") == nchar(from, type = "bytes"))
  b <- readBin(path, "raw", file.size(path))
  for (i in seq_along(from)) {
    hits <- grepRaw(charToRaw(from[[i]]), b, fixed = TRUE, all = TRUE)
    stopifnot(length(hits) > 0)
    for (h in hits) b[h + seq_len(nchar(from[[i]], type = "bytes")) - 1L] <- charToRaw(to[[i]])
  }
  cont_write(name, b, env = env)
}

# A condition's message with cli's line wrapping undone.
cont_flat <- function(e) gsub("\\s+", " ", conditionMessage(e))

# The error `expr` raises, or NULL when it raises none.
cont_catch <- function(expr) {
  tryCatch(
    {
      expr
      NULL
    },
    error = function(e) e
  )
}

# Does the condition belong to `class`? FALSE for NULL (no condition at all).
cont_is <- function(cnd, class) !is.null(cnd) && inherits(cnd, class)

# load_delivery() with its summary message and its skip-mode warning captured
# instead of printed. Returns list(result, summary, warning, n_warnings, error,
# plan): `warning` is the last dta_delivery_refused warning and `n_warnings`
# how many there were; the plan is the last_delivery table of the result, or
# the `plan` field of the error that refused a delivery with nothing in it.
cont_run <- function(...) {
  out <- list(result = NULL, summary = NULL, warning = NULL, n_warnings = 0L, error = NULL, plan = NULL)
  out$result <- tryCatch(
    withCallingHandlers(
      DTAtools::load_delivery(...),
      dta_delivery_summary = function(m) {
        out$summary <<- m
        invokeRestart("muffleMessage")
      },
      dta_delivery_refused = function(w) {
        out$warning <<- w
        out$n_warnings <<- out$n_warnings + 1L
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      out$error <<- e
      NULL
    }
  )
  out$plan <- if (!is.null(out$result)) attr(out$result, "last_delivery") else out$error$plan
  out
}

# The rows of a plan with one status.
cont_rows <- function(plan, status) plan[plan$status == status, , drop = FALSE]

# The number of tables a dataset of a DTA holds.
cont_ntab <- function(dta, dataset) length(DTAtools::tables(DTAtools::datasets(dta, dataset)))

# Column specs with no columns, for datasets whose values are not under test.
cont_empty_specs <- function() DTAtools::DTAColumnSpecCollection(columns = list())

# One tabular dataset, with the specs of its domain unless others are given.
cont_tab <- function(id, files, specs = cont_specs(id)) {
  DTAtools::DTADataSetTabular(id = id, specs = specs, files = files)
}

# One file dataset (the files are not parsed).
cont_filedata <- function(id, ...) DTAtools::DTADataSetFile(id = id, files = list(...))

# A DTA of the given datasets.
cont_of <- function(...) DTAtools::DTA(datasets = list(...))

# A copy of the zip `base` with extra EMPTY stored entries called `names` added
# to it, written to a temp file called `name`. Entry names of 511 bytes or more
# cannot be created on Windows and zip::zip() would not write them, so the
# entries are appended as bytes: a local header and a central-directory record
# each (UTF-8 flag set, stored, no data), then a new end record. `base` must be
# a zip without a comment, as zip::zip() writes.
cont_zip_add_empty <- function(base, names, name = "long.zip", env = parent.frame()) {
  b <- readBin(base, "raw", file.size(base))
  u16 <- function(n) as.raw(c(n %% 256, n %/% 256))
  u32 <- function(n) as.raw(c(n %% 256, (n %/% 256) %% 256, (n %/% 65536) %% 256, n %/% 16777216))
  num <- function(r) sum(as.numeric(r) * 256^(seq_along(r) - 1))
  e <- length(b) - 21 # the end record is the last 22 bytes
  stopifnot(identical(b[e:(e + 3)], as.raw(c(0x50, 0x4b, 0x05, 0x06))))
  count <- num(b[e + 10:11])
  local <- b[seq_len(num(b[e + 16:19]))]
  central <- b[num(b[e + 16:19]) + seq_len(num(b[e + 12:15]))]
  for (nm in names) {
    raw_name <- charToRaw(enc2utf8(nm))
    header_at <- length(local)
    local <- c(
      local, as.raw(c(0x50, 0x4b, 0x03, 0x04)), u16(20), u16(0x0800), u16(0), u16(0), u16(0x21),
      u32(0), u32(0), u32(0), u16(length(raw_name)), u16(0), raw_name
    )
    central <- c(
      central, as.raw(c(0x50, 0x4b, 0x01, 0x02)), u16(20), u16(20), u16(0x0800), u16(0), u16(0),
      u16(0x21), u32(0), u32(0), u32(0), u16(length(raw_name)), u16(0), u16(0), u16(0), u16(0),
      u32(0), u32(header_at), raw_name
    )
  }
  out <- c(
    local, central, as.raw(c(0x50, 0x4b, 0x05, 0x06)), u16(0), u16(0),
    u16(count + length(names)), u16(count + length(names)),
    u32(length(central)), u32(length(local)), u16(0)
  )
  cont_write(name, out, env = env)
}
