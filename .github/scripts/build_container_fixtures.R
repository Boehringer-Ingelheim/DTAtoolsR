#!/usr/bin/env Rscript
# Builds the container fixtures under inst/qualification/fx/container/. Run from
# the repository root:
#
#   Rscript --no-init-file .github/scripts/build_container_fixtures.R
#
# An optional first argument names a different output directory (used to check
# a rebuild is byte-identical: build into a temp dir and compare md5 sums).
#
# Deterministic by construction, so a rebuild changes no byte and the
# qualification manifest does not churn:
# - every .zip is written by write_zip() below: stored (or one stored deflate
#   block) entries, one fixed DOS date/time, no extra fields;
# - every .xpt is written by haven::write_xpt() and then has its creation and
#   modification stamps overwritten with 01JAN26:00:00:00;
# - the one gzip member is a stored deflate block with MTIME 0.
# Several archives are deliberately hostile (unsafe names, symlink, encrypted,
# lying sizes). Nothing here ever extracts them.
#
# The self-check at the end uses haven, zip and base R only, never DTAtools.
# After regenerating, run .github/scripts/sync_qualification_manifest.R.

stopifnot(utils::packageVersion("haven") >= "2.5.2")
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) args[[1]] else file.path("inst", "qualification", "fx", "container")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
work <- tempfile("container-fixtures-")
dir.create(work)
on.exit(unlink(work, recursive = TRUE), add = TRUE)

# ---- bytes ------------------------------------------------------------------
u16 <- function(x) writeBin(as.integer(x), raw(), size = 2, endian = "little")
u32 <- function(x) {
  x <- x %% 2^32
  c(u16(x %% 65536), u16(x %/% 65536))
}
u64 <- function(x) c(u32(x %% 2^32), u32(x %/% 2^32))

crc_table <- vapply(0:255, function(n) {
  c <- n
  for (k in 1:8) {
    c <- if (c %% 2 == 1) bitwXor(bitwShiftR(c, 1), -306674912L) else bitwShiftR(c, 1)
  }
  c
}, integer(1))
crc32 <- function(bytes) {
  c <- -1L
  for (b in as.integer(bytes)) {
    c <- bitwXor(crc_table[bitwAnd(bitwXor(c, b), 255L) + 1L], bitwShiftR(c, 8))
  }
  c <- bitwNot(c)
  if (c < 0) c + 2^32 else c
}
stopifnot(crc32(charToRaw("123456789")) == 0xCBF43926) # the standard check value

text_bytes <- function(x) charToRaw(enc2utf8(x))

# One deflate STORED block (BTYPE 00): valid deflate data that is byte-stable
# everywhere, unlike the output of a compressor.
deflate_stored <- function(bytes) {
  stopifnot(length(bytes) <= 65535)
  len <- length(bytes)
  c(as.raw(0x01), u16(len), u16(bitwAnd(bitwNot(len), 0xFFFF)), bytes)
}

# ---- a minimal ZIP writer -----------------------------------------------------
# entries: list of list(name = chr or raw, data = raw, method = 0L, flags = 0L,
#   made_by = 20L, ext_attr = 0, declared_size = NULL). method 8 wraps the data
#   in one stored deflate block. declared_size overrides the uncompressed size
#   written in BOTH headers (to build lying archives; miniz refuses to even list
#   a METHOD 0 entry whose two sizes differ, so lying entries use method 8).
# zip64 = TRUE adds a ZIP64 end-of-central-directory record and locator;
# eocd_count then overrides the 16-bit entry count of the classic record
# (default 0xFFFF, "see the ZIP64 record") to build an archive that lies.
DOS_TIME <- 0L # 00:00:00
DOS_DATE <- 0x5C21L # 2026-01-01: ((2026 - 1980) << 9) | (1 << 5) | 1
write_zip <- function(path, entries, zip64 = FALSE, eocd_count = 0xFFFF) {
  local <- raw()
  central <- raw()
  offset <- 0
  for (e in entries) {
    # A raw name is written byte for byte WITHOUT the UTF-8 flag (a legacy,
    # code-page name); a character name is UTF-8 and flagged when not ASCII.
    name <- if (is.raw(e$name)) e$name else text_bytes(e$name)
    data <- if (is.null(e$data)) raw() else e$data
    method <- if (is.null(e$method)) 0L else e$method
    payload <- if (method == 8L) deflate_stored(data) else data
    flags <- if (is.null(e$flags)) 0L else e$flags
    if (!is.raw(e$name) && any(name > as.raw(127))) flags <- bitwOr(flags, 0x0800L) # UTF-8 name flag
    crc <- crc32(data)
    usize <- if (is.null(e$declared_size)) length(data) else e$declared_size
    lh <- c(
      u32(0x04034b50), u16(20), u16(flags), u16(method), u16(DOS_TIME), u16(DOS_DATE),
      u32(crc), u32(length(payload)), u32(usize), u16(length(name)), u16(0), name
    )
    ch <- c(
      u32(0x02014b50), u16(if (is.null(e$made_by)) 20L else e$made_by), u16(20),
      u16(flags), u16(method), u16(DOS_TIME), u16(DOS_DATE), u32(crc), u32(length(payload)),
      u32(usize), u16(length(name)), u16(0), u16(0), u16(0), u16(0),
      u32(if (is.null(e$ext_attr)) 0 else e$ext_attr), u32(offset), name
    )
    local <- c(local, lh, payload)
    central <- c(central, ch)
    offset <- offset + length(lh) + length(payload)
  }
  n <- length(entries)
  tail <- if (zip64) {
    rec_off <- offset + length(central)
    rec <- c(
      u32(0x06064b50), u64(44), u16(45), u16(45), u32(0), u32(0),
      u64(n), u64(n), u64(length(central)), u64(offset)
    )
    loc <- c(u32(0x07064b50), u32(0), u64(rec_off), u32(1))
    eocd <- c(
      u32(0x06054b50), u16(0), u16(0), u16(eocd_count), u16(eocd_count),
      u32(length(central)), u32(offset), u16(0)
    )
    c(rec, loc, eocd)
  } else {
    c(
      u32(0x06054b50), u16(0), u16(0), u16(n), u16(n),
      u32(length(central)), u32(offset), u16(0)
    )
  }
  writeBin(c(local, central, tail), path)
  invisible(path)
}

# gzip of `bytes` as ONE stored deflate block, MTIME 0: byte-stable everywhere.
gzip_stored <- function(bytes) {
  stopifnot(length(bytes) <= 65535)
  len <- length(bytes)
  c(
    as.raw(c(0x1f, 0x8b, 0x08, 0x00, 0, 0, 0, 0, 0x00, 0xff)),
    as.raw(0x01), u16(len), u16(bitwAnd(bitwNot(len), 0xFFFF)), bytes,
    u32(crc32(bytes)), u32(len)
  )
}

# ---- the data every fixture carries (the tests restate these as literals) ---
AE_CSV <- "USUBJID,AETERM,AESEQ\nS-001,HEADACHE,1\nS-002,NAUSEA,2\n"
DM_CSV <- "USUBJID,AGE\nS-001,34\nS-002,51\n"
AE <- data.frame(USUBJID = c("S-001", "S-002"), AETERM = c("HEADACHE", "NAUSEA"), AESEQ = c(1, 2))
DM <- data.frame(USUBJID = c("S-001", "S-002"), AGE = c(34, 51))
VS <- data.frame(
  USUBJID = c("S-001", "S-001", "S-002"),
  VSTESTCD = c("SYSBP", "DIABP", "SYSBP"),
  VSSTRESN = c(120, 80, 135),
  VSDT = as.Date(c("2025-08-15", "2025-08-15", "2025-08-16"))
)

# ---- SAS transport ----------------------------------------------------------
STAMP <- charToRaw("01JAN26:00:00:00")
LIB5 <- charToRaw("HEADER RECORD*******LIBRARY HEADER RECORD!!!!!!!")
LIB8 <- charToRaw("HEADER RECORD*******LIBV8   HEADER RECORD!!!!!!!")
MEM5 <- charToRaw("HEADER RECORD*******MEMBER  HEADER RECORD!!!!!!!")
MEM8 <- charToRaw("HEADER RECORD*******MEMBV8  HEADER RECORD!!!!!!!")
record_starts <- function(b, marker) {
  n <- length(b) %/% 80
  which(vapply(seq_len(n), function(i) identical(b[((i - 1) * 80 + 1):((i - 1) * 80 + 48)], marker), logical(1)))
}
# Overwrite the creation/modification stamps: record 2 bytes 65-80, record 3
# bytes 1-16, and for every member the record two after its header (bytes
# 65-80) and the one after that (bytes 1-16). Layout per SAS TS-140.
stamp_xpt <- function(b) {
  set <- function(b, rec, from) {
    i <- (rec - 1) * 80 + from
    b[i:(i + 15)] <- STAMP
    b
  }
  b <- set(b, 2, 65)
  b <- set(b, 3, 1)
  for (m in c(record_starts(b, MEM5), record_starts(b, MEM8))) {
    b <- set(b, m + 2, 65)
    b <- set(b, m + 3, 1)
  }
  b
}
xpt_bytes <- function(frame, name, version = 5) {
  p <- tempfile(tmpdir = work, fileext = ".xpt")
  haven::write_xpt(frame, p, version = version, name = name)
  stamp_xpt(readBin(p, "raw", file.size(p)))
}
# A library: the library header of the first part, then each part's members.
xpt_library <- function(parts) c(parts[[1]][1:240], unlist(lapply(parts, function(b) b[241:length(b)])))

x_ae5 <- xpt_bytes(AE, "AE")
x_dm5 <- xpt_bytes(DM, "DM")
x_vs5 <- xpt_bytes(VS, "VS")
x_ae_lower5 <- xpt_bytes(AE, "ae")
x_ae8 <- xpt_bytes(AE, "adverse_events", version = 8)
x_dm8 <- xpt_bytes(DM, "DM", version = 8)

sdtm_xpt <- xpt_library(list(x_ae5, x_dm5, x_vs5))
put <- function(name, bytes) writeBin(bytes, file.path(out_dir, name))
put("sdtm.xpt", sdtm_xpt)
put("sdtm_v8.xpt", xpt_library(list(x_ae8, x_dm8)))
put("adverse.xpt", x_ae5)
put("dup_member.xpt", xpt_library(list(x_ae5, x_ae_lower5)))
put("not_transport.xpt", text_bytes("this is not a SAS transport file\n"))
put("cport.xpt", c(text_bytes(sprintf("%-80s", "**COMPRESSED** **COMPRESSED** **COMPRESSED** **COMPRESSED** **COMPRESSED**")), raw(80)))

# ---- archives ---------------------------------------------------------------
f <- function(name, data = raw(), ...) list(name = name, data = data, ...)
csv <- function(x) text_bytes(x)
inner <- tempfile(tmpdir = work, fileext = ".zip")
write_zip(inner, list(f("ae.csv", csv(AE_CSV))))
sdtm_csv_entries <- list(
  f("ae.csv", csv(AE_CSV)), f("dm.csv", csv(DM_CSV)),
  f("define.xml", csv("<define/>\n")), f("docs/"), f("docs/readme.txt", csv("readme\n")),
  f("__MACOSX/._ae.csv", raw(8)), f(".DS_Store", raw(8))
)
archives <- list(
  sdtm_csv.zip = sdtm_csv_entries,
  sdtm_dirs.zip = list(f("sdtm/"), f("sdtm/ae.csv", csv(AE_CSV)), f("sdtm/dm.csv.gz", gzip_stored(csv(DM_CSV)))),
  sdtm_xpt.zip = list(f("ae.xpt", x_ae5), f("dm.xpt", x_dm5)),
  # Windows PowerShell 5.1's Compress-Archive writes a folder separator as a
  # backslash: these names are safe and are extracted.
  backslash_sep.zip = list(f("sdtm\\ae.csv", csv(AE_CSV)), f("dm.csv", csv(DM_CSV))),
  # A long name and an entry spelled like its Windows 8.3 short alias.
  alias83.zip = list(f("abcdefghij.csv", csv(AE_CSV)), f("ABCDEF~1.CSV", csv(DM_CSV))),
  lib_in_zip.zip = list(f("sdtm.xpt", sdtm_xpt), f("define.xml", csv("<define/>\n"))),
  nested.zip = list(f("inner.zip", readBin(inner, "raw", file.size(inner)))),
  unsafe_dotdot.zip = list(f("ae.csv", csv(AE_CSV)), f("../evil.csv", csv(AE_CSV))),
  unsafe_dotdot_mid.zip = list(f("ae.csv", csv(AE_CSV)), f("a/../../evil.csv", csv(AE_CSV))),
  unsafe_absolute.zip = list(f("ae.csv", csv(AE_CSV)), f("/tmp/dtatools-evil.csv", csv(AE_CSV))),
  unsafe_drive.zip = list(f("ae.csv", csv(AE_CSV)), f("C:/dtatools-evil.csv", csv(AE_CSV))),
  unsafe_backslash_dotdot.zip = list(f("ae.csv", csv(AE_CSV)), f("..\\evil.csv", csv(AE_CSV))),
  unsafe_colon.zip = list(f("ae.csv:stream", csv(AE_CSV))),
  unsafe_reserved.zip = list(f("CON.csv", csv(AE_CSV))),
  unsafe_trailing_dot.zip = list(f("ae.csv.", csv(AE_CSV))),
  unsafe_control.zip = list(f("ae\tx.csv", csv(AE_CSV))),
  symlink.zip = list(f("ae.csv", csv("../../etc/passwd"), made_by = 0x0314L, ext_attr = 0xA1FF * 65536)),
  encrypted.zip = list(f("ae.csv", csv(AE_CSV), flags = 1L)),
  dup_entry.zip = list(f("ae.csv", csv(AE_CSV)), f("ae.csv", csv(AE_CSV))),
  dup_case.zip = list(f("ae.csv", csv(AE_CSV)), f("AE.CSV", csv(AE_CSV))),
  deflated.zip = list(f("ae.csv", csv(AE_CSV), method = 8L), f("dm.csv", csv(DM_CSV), method = 8L)),
  # "a<0xE9>.csv": a Windows-1252 e-acute with no UTF-8 flag. The ZIP
  # specification says such a name is code page 437, where 0xE9 is a Greek
  # capital theta, so readers decode it as "a<theta>.csv".
  cp1252_name.zip = list(f(as.raw(c(0x61, 0xE9, 0x2E, 0x63, 0x73, 0x76)), csv(AE_CSV))),
  bomb_ratio.zip = list(f("ae.csv", csv(AE_CSV), method = 8L, declared_size = 2e9)),
  lying_size.zip = list(f("ae.csv", csv(AE_CSV), method = 8L, declared_size = 5))
)
for (nm in names(archives)) write_zip(file.path(out_dir, nm), archives[[nm]])
write_zip(file.path(out_dir, "zip64_one.zip"), list(f("ae.csv", csv(AE_CSV))), zip64 = TRUE)
# The classic record says 1 entry, the ZIP64 record (which the zip library
# trusts) says 3.
write_zip(file.path(out_dir, "zip64_count_lie.zip"), sdtm_csv_entries[1:3], zip64 = TRUE, eocd_count = 1)
put("not_a_zip.zip", text_bytes("this is not a zip archive\n"))
whole <- readBin(file.path(out_dir, "sdtm_csv.zip"), "raw", file.size(file.path(out_dir, "sdtm_csv.zip")))
put("truncated.zip", whole[seq_len(length(whole) - 30)])

# ---- self-check (haven, zip and base R only) --------------------------------
fx <- function(name) file.path(out_dir, name)
listed <- function(name) zip::zip_list(fx(name))$filename
stopifnot(
  identical(listed("sdtm_csv.zip"), vapply(sdtm_csv_entries, `[[`, "", "name")),
  identical(listed("sdtm_dirs.zip"), c("sdtm/", "sdtm/ae.csv", "sdtm/dm.csv.gz")),
  identical(listed("zip64_one.zip"), "ae.csv"),
  identical(listed("zip64_count_lie.zip"), c("ae.csv", "dm.csv", "define.xml")),
  identical(listed("backslash_sep.zip"), c("sdtm\\ae.csv", "dm.csv")),
  identical(listed("alias83.zip"), c("abcdefghij.csv", "ABCDEF~1.CSV")),
  identical(zip::zip_list(fx("symlink.zip"))$type, "symlink"),
  identical(zip::zip_list(fx("encrypted.zip"))$encryption, "zipcrypto"),
  identical(zip::zip_list(fx("bomb_ratio.zip"))$uncompressed_size, 2e9),
  identical(listed("unsafe_dotdot_mid.zip"), c("ae.csv", "a/../../evil.csv")),
  identical(enc2utf8(listed("cp1252_name.zip")), "a\u0398.csv")
)
# The gzip member decompresses to the DM text.
gz <- tempfile(tmpdir = work, fileext = ".gz")
writeBin(gzip_stored(csv(DM_CSV)), gz)
stopifnot(identical(paste0(paste(readLines(gzfile(gz)), collapse = "\n"), "\n"), DM_CSV))
# Each single-member transport file reads back as the frame it was written from.
same_values <- function(got, want) {
  got <- as.data.frame(got)
  identical(names(got), names(want)) &&
    all(vapply(names(want), function(n) isTRUE(all.equal(unclass(got[[n]]), unclass(want[[n]]), check.attributes = FALSE)), logical(1)))
}
one <- tempfile(tmpdir = work, fileext = ".xpt")
writeBin(x_ae5, one)
stopifnot(same_values(haven::read_xpt(one), AE))
writeBin(x_vs5, one)
stopifnot(same_values(haven::read_xpt(one), VS))
# Three member headers, at records 4, 17 and 28 (bytes 240, 1280, 2160).
stopifnot(identical(record_starts(sdtm_xpt, MEM5), c(4L, 17L, 28L)))
cat("Wrote", length(list.files(out_dir)), "fixtures to", out_dir, "\n")
