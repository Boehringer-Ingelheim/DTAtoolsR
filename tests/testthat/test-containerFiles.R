# The container layer (R/containerFiles.R): names, counts, inspection and
# extraction of zip archives, and the member scan and split of SAS transport
# libraries. Every expected value is the literal data the fixtures were built
# from (helper-containers.R) or the specification, never the code's own output.

# The message of a condition with cli's line wrapping undone.
flat_message <- function(e) gsub("\\s+", " ", conditionMessage(e))

# A copy of the file `path` with every occurrence of the bytes of each `from`
# replaced by the same number of bytes of the matching `to`, written to a temp
# file called `name`.
patched_copy <- function(path, from, to, name = basename(path), env = parent.frame()) {
  expect_equal(nchar(to, type = "bytes"), nchar(from, type = "bytes"))
  b <- readBin(path, "raw", file.size(path))
  for (i in seq_along(from)) {
    hits <- grepRaw(charToRaw(from[[i]]), b, fixed = TRUE, all = TRUE)
    expect_gt(length(hits), 0)
    for (h in hits) b[h + seq_len(nchar(from[[i]], type = "bytes")) - 1L] <- charToRaw(to[[i]])
  }
  cont_write(name, b, env = env)
}

# A copy of a committed fixture, with entries renamed by patched_copy().
rename_entry <- function(fixture, from, to, env = parent.frame()) {
  patched_copy(cont_fx(fixture), from, to, env = env)
}

# A copy of sdtm.xpt with the 8 bytes of AE's member name (file bytes 409 to
# 416: 240 for the library header, 160 into the member's records, 8 for the
# "SAS     " prefix of that record) replaced by `name`.
sdtm_with_member_name <- function(name, env = parent.frame()) {
  src <- cont_fx("sdtm.xpt")
  b <- readBin(src, "raw", file.size(src))
  expect_length(name, 8)
  b[409:416] <- name
  cont_write("named.xpt", b, env = env)
}

# A zip holding one file `a/x.csv`, with the 7 bytes of that name replaced by
# `to` wherever they occur in the archive (local header and central directory).
patched_name_zip <- function(to, env = parent.frame()) {
  src <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(src, "a"))
  writeBin(charToRaw(CONT_AE_CSV), file.path(src, "a", "x.csv"))
  z <- file.path(withr::local_tempdir(.local_envir = env), "patched.zip")
  zip::zip(z, files = "a/x.csv", root = src)
  b <- readBin(z, "raw", file.size(z))
  hits <- grepRaw(charToRaw("a/x.csv"), b, fixed = TRUE, all = TRUE)
  expect_length(hits, 2)
  expect_length(to, 7)
  for (h in hits) b[h:(h + 6)] <- to
  writeBin(b, z)
  z
}

# The gzip of a transport fixture, in a temp file called "<fixture>.gz".
gz_fixture <- function(fixture, env = parent.frame()) {
  src <- cont_fx(fixture)
  p <- file.path(withr::local_tempdir(.local_envir = env), paste0(fixture, ".gz"))
  con <- gzfile(p, "wb")
  writeBin(readBin(src, "raw", file.size(src)), con)
  close(con)
  p
}

# --- Names and counts ---------------------------------------------------------

test_that("dta_container_kind recognises .zip, .xpt and .xpt.gz in any case", {
  expect_identical(
    dta_container_kind(c("a.zip", "A.ZIP", "b.xpt", "b.XPT.gz", "c.csv", "d.zip.gz", "e.xpt.zip", "dir/f.Zip")),
    c("zip", "zip", "xpt", "xpt", NA, NA, "zip", "zip")
  )
  expect_identical(dta_container_kind(character(0)), character(0))
})

test_that("ordinary entry names pass", {
  ordinary <- c(
    "ae.csv", "sdtm/ae.csv", "sdtm\\ae.csv", "\u00e9t\u00e9.csv", "sdtm/", "a b.csv",
    ".DS_Store", "__MACOSX/._ae.csv", "{Sys.setenv(DTA_PWNED = '1')}.csv"
  )
  for (nm in ordinary) {
    expect_true(is.na(dta_zip_entry_problem(nm)), info = nm)
  }
})

test_that("each hostile entry name is caught by the first rule it breaks", {
  cases_name <- c(
    "",
    rawToChar(as.raw(c(0x61, 0xff, 0x2e, 0x63, 0x73, 0x76))),
    "a\tb.csv",
    "/tmp/e.csv", "\\tmp\\e.csv",
    "C:/e.csv", "C:e.csv",
    "ae.csv:s",
    "a?b.csv", "a*b.csv", "a<b.csv", "a\"b.csv", "a|b.csv",
    "a//b.csv", "a\\\\b.csv",
    "../evil.csv", "a/../../evil.csv", "./ae.csv", "..\\e.csv", "sub\\..\\e.csv",
    "ae.csv.", "ae.csv ", "sdtm./ae.csv",
    "CON.csv", "lpt1", "con", "COM\u00b9.csv", "CONIN$", "sdtm/aux.txt",
    strrep("a", 256)
  )
  cases_phrase <- c(
    "has an empty name",
    "has a name that is not valid text",
    "contains a control character",
    rep("is an absolute path", 2),
    rep("starts with a drive letter", 2),
    "contains a colon",
    rep("contains a character Windows does not allow in file names", 5),
    rep("has an empty path segment", 2),
    rep("contains a '.' or '..' path segment", 5),
    rep("has a path segment ending in a dot or a space", 3),
    rep("uses a reserved Windows device name", 6),
    "has a path segment longer than 255 bytes"
  )
  expect_length(cases_name, 30)
  expect_length(cases_phrase, 30)
  for (i in seq_along(cases_name)) {
    expect_identical(dta_zip_entry_problem(cases_name[[i]]), cases_phrase[[i]], info = paste("case", i))
  }
})

test_that("junk entries are recognised", {
  for (nm in c(".DS_Store", "__MACOSX/._ae.csv", "docs/.hidden/x.csv", "docs\\.hidden\\x.csv", "Icon\r", "sdtm/Icon\r")) {
    expect_true(dta_zip_entry_ignored(nm), info = encodeString(nm))
  }
  for (nm in c("ae.csv", "docs/readme.txt", "docs\\readme.txt", "Icon", "sdtm/Icon.png", "Icons/ae.csv", "sdtm/Icon\r.txt")) {
    expect_false(dta_zip_entry_ignored(nm), info = encodeString(nm))
  }
})

test_that("the entry count comes from the end-of-central-directory record", {
  expect_equal(dta_zip_entry_count(cont_fx("sdtm_csv.zip")), 7)
  expect_equal(dta_zip_entry_count(cont_fx("zip64_one.zip")), 1)
  expect_equal(dta_zip_entry_count(cont_fx("nested.zip")), 1)
  # The 16-bit count says 1; the ZIP64 record says 3.
  expect_equal(dta_zip_entry_count(cont_fx("zip64_count_lie.zip")), 3)
})

test_that("the count is checked before the archive is listed", {
  p <- cont_write("many.zip", c(
    raw(30), as.raw(c(0x50, 0x4b, 0x05, 0x06)), raw(4),
    writeBin(60000L, raw(), size = 2, endian = "little"),
    writeBin(60000L, raw(), size = 2, endian = "little"),
    raw(10)
  ))
  e <- expect_error(dta_zip_inspect(p), class = "dta_container_too_large")
  expect_false(inherits(e, "dta_container_unreadable"))
  expect_equal(e$value, 60000)
  expect_identical(e$limit, "DTAtools.container_max_members")
})

test_that("a lying 16-bit count does not bypass the cap", {
  withr::local_options(DTAtools.container_max_members = 2)
  e <- expect_error(dta_zip_inspect(cont_fx("zip64_count_lie.zip")), class = "dta_container_too_large")
  expect_equal(e$value, 3)
  e <- expect_error(dta_zip_inspect(cont_fx("sdtm_csv.zip")), class = "dta_container_too_large")
  expect_equal(e$value, 7)
  expect_equal(e$allowed, 2)
})

test_that("an archive without a central directory is unreadable", {
  expect_error(dta_zip_inspect(cont_fx("truncated.zip")), class = "dta_container_unreadable")
  expect_error(dta_zip_inspect(cont_fx("not_a_zip.zip")), class = "dta_container_unreadable")
})

test_that("the entry cap is exact: as many entries as allowed are accepted", {
  # sdtm_csv.zip holds 7 entries (6 files and a folder).
  res <- withr::with_options(
    list(DTAtools.container_max_members = 7),
    dta_zip_inspect(cont_fx("sdtm_csv.zip"))
  )
  expect_equal(nrow(res), 6)
  e <- withr::with_options(
    list(DTAtools.container_max_members = 6),
    expect_error(dta_zip_inspect(cont_fx("sdtm_csv.zip")), class = "dta_container_too_large")
  )
  expect_equal(e$value, 7)
  expect_equal(e$allowed, 6)
  # The count alone decides, before the archive is listed: a file that is only
  # an end-of-central-directory record claiming 3 entries is refused for a limit
  # of 2; one claiming 2 is not refused for its count (whatever else happens).
  eocd_only <- function(n, env = parent.frame()) {
    count <- writeBin(as.integer(n), raw(), size = 2, endian = "little")
    cont_write("claims.zip", c(raw(30), as.raw(c(0x50, 0x4b, 0x05, 0x06)), raw(4), count, count, raw(10)), env = env)
  }
  withr::local_options(DTAtools.container_max_members = 2)
  e <- expect_error(dta_zip_inspect(eocd_only(3)), class = "dta_container_too_large")
  expect_equal(e$value, 3)
  e <- tryCatch(dta_zip_inspect(eocd_only(2)), error = identity)
  expect_false(inherits(e, "dta_container_too_large"))
})

test_that("an unreadable central directory is reported without the server path", {
  b <- readBin(cont_fx("sdtm_csv.zip"), "raw", file.size(cont_fx("sdtm_csv.zip")))
  # Four zero bytes over the first central-directory signature (PK 01 02): the
  # end-of-central-directory record still counts 7 entries, but the listing fails.
  at <- grepRaw(as.raw(c(0x50, 0x4b, 0x01, 0x02)), b)
  b[at:(at + 3)] <- raw(4)
  p <- cont_write("cd.zip", b)
  e <- expect_error(dta_zip_inspect(p), class = "dta_container_unreadable")
  expect_true(nzchar(e$reason))
  expect_false(grepl(basename(dirname(p)), e$reason, fixed = TRUE))
  expect_false(grepl(basename(dirname(p)), conditionMessage(e), fixed = TRUE))
  expect_false(grepl("rzip.c", e$reason, fixed = TRUE))
  # The same file reached through its normalised path (backslashes on Windows).
  e <- expect_error(dta_zip_inspect(normalizePath(p)), class = "dta_container_unreadable")
  expect_false(grepl(basename(dirname(p)), e$reason, fixed = TRUE))
  expect_false(grepl("rzip.c", e$reason, fixed = TRUE))
})

# --- Inspection ---------------------------------------------------------------

test_that("dta_zip_inspect lists file entries with declared sizes", {
  res <- dta_zip_inspect(cont_fx("sdtm_csv.zip"))
  expect_identical(
    res$entry,
    c("ae.csv", "dm.csv", "define.xml", "docs/readme.txt", "__MACOSX/._ae.csv", ".DS_Store")
  )
  expect_identical(res$name, c("ae.csv", "dm.csv", "define.xml", "readme.txt", "._ae.csv", ".DS_Store"))
  # The byte counts of the literal texts; the two junk entries are 8 zero bytes.
  expect_equal(nchar(CONT_AE_CSV, type = "bytes"), 53)
  expect_equal(nchar(CONT_DM_CSV, type = "bytes"), 30)
  expect_equal(nchar("<define/>\n", type = "bytes"), 10)
  expect_equal(nchar("readme\n", type = "bytes"), 7)
  expect_equal(res$size, c(53, 30, 10, 7, 8, 8))
  expect_identical(res$ignored, c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE))
})

test_that("backslash folders are folders", {
  res <- dta_zip_inspect(cont_fx("backslash_sep.zip"))
  expect_identical(res$entry, c("sdtm\\ae.csv", "dm.csv"))
  expect_identical(res$name, c("ae.csv", "dm.csv"))
  expect_equal(res$size, c(53, 30))
})

test_that("every hostile archive is refused and nothing is extracted", {
  # Refused whole: the zip library is never asked to extract any of them.
  for (fx in c(
    "unsafe_dotdot.zip", "unsafe_dotdot_mid.zip", "unsafe_absolute.zip",
    "unsafe_drive.zip", "unsafe_backslash_dotdot.zip", "symlink.zip"
  )) {
    e <- expect_error(dta_zip_inspect(cont_fx(fx)), class = "dta_container_unsafe_member", info = fx)
    expect_match(flat_message(e), "would be unsafe to extract", fixed = TRUE, info = fx)
    if (fx == "unsafe_absolute.zip") {
      expect_identical(e$problems, "is an absolute path")
    }
    if (fx == "unsafe_drive.zip") {
      expect_identical(e$problems, "starts with a drive letter")
    }
    if (fx == "unsafe_dotdot_mid.zip") {
      expect_identical(e$members, "a/../../evil.csv")
      expect_identical(e$problems, "contains a '.' or '..' path segment")
    }
    if (fx == "unsafe_backslash_dotdot.zip") {
      expect_identical(e$members, "..\\evil.csv")
      expect_identical(e$problems, "contains a '.' or '..' path segment")
    }
    if (fx == "symlink.zip") {
      expect_identical(e$problems, "is a symbolic link")
    }
  }
  # Only impossible to write on some file systems: listed with a problem, and
  # refused later only when something needs the entry.
  problems <- c(
    unsafe_colon.zip = "contains a colon",
    unsafe_reserved.zip = "uses a reserved Windows device name",
    unsafe_trailing_dot.zip = "has a path segment ending in a dot or a space",
    unsafe_control.zip = "contains a control character"
  )
  for (fx in names(problems)) {
    res <- dta_zip_inspect(cont_fx(fx))
    expect_equal(nrow(res), 1, info = fx)
    expect_identical(res$problem, unname(problems[[fx]]), info = fx)
  }
})

test_that("an encrypted archive is refused", {
  e <- expect_error(dta_zip_inspect(cont_fx("encrypted.zip")), class = "dta_container_encrypted")
  expect_identical(e$members, "ae.csv")
})

test_that("duplicate entry names are refused, also when only the case differs", {
  e <- expect_error(dta_zip_inspect(cont_fx("dup_entry.zip")), class = "dta_container_duplicate_member")
  expect_identical(e$members, c("ae.csv", "ae.csv"))
  e <- expect_error(dta_zip_inspect(cont_fx("dup_case.zip")), class = "dta_container_duplicate_member")
  expect_identical(e$members, c("ae.csv", "AE.CSV"))
  # Same base name in different folders: different entries, listed twice.
  z <- cont_zip(list("a/ae.csv" = CONT_AE_CSV, "b/ae.csv" = CONT_AE_CSV))
  res <- dta_zip_inspect(z)
  expect_identical(res$name[!res$ignored], c("ae.csv", "ae.csv"))
})

test_that("duplicate entries are found across separators", {
  # "sdtm/xe.csv" becomes "SDTM\\AE.CSV" (11 bytes each) wherever it occurs.
  z <- patched_copy(
    cont_zip(list("sdtm/ae.csv" = CONT_AE_CSV, "sdtm/xe.csv" = CONT_DM_CSV)),
    "sdtm/xe.csv", "SDTM\\AE.CSV"
  )
  e <- expect_error(dta_zip_inspect(z), class = "dta_container_duplicate_member")
  expect_identical(e$members, c("sdtm/ae.csv", "SDTM\\AE.CSV"))
})

test_that("a directory that claims far more than deflate can deliver is refused", {
  e <- expect_error(dta_zip_inspect(cont_fx("bomb_ratio.zip")), class = "dta_container_too_large")
  expect_identical(e$limit, "DTAtools.container_max_ratio")
  res <- withr::with_options(
    list(DTAtools.container_max_ratio = 1e12),
    dta_zip_inspect(cont_fx("bomb_ratio.zip"))
  )
  expect_equal(nrow(res), 1)
  expect_equal(res$size, 2e9)
})

test_that("an old zip package is refused, not trusted", {
  testthat::local_mocked_bindings(
    zip_list = function(...) data.frame(filename = "ae.csv", uncompressed_size = 53),
    .package = "zip"
  )
  e <- expect_error(dta_zip_inspect(cont_fx("sdtm_csv.zip")), class = "dta_container_unreadable")
  expect_true(grepl("zip 3.0.0", e$reason, fixed = TRUE))
})

# --- Extraction ---------------------------------------------------------------

test_that("each entry is extracted into its own directory, byte for byte", {
  d <- withr::local_tempdir()
  got <- dta_zip_extract(cont_fx("sdtm_csv.zip"), c("ae.csv", "docs/readme.txt"), c(53, 7), d)
  expect_identical(got, c(file.path(d, "e00001", "ae.csv"), file.path(d, "e00002", "readme.txt")))
  expect_setequal(list.files(d, recursive = TRUE), c("e00001/ae.csv", "e00002/readme.txt"))
  expect_identical(readBin(got[[1]], "raw", 100), charToRaw(CONT_AE_CSV))
  expect_identical(readBin(got[[2]], "raw", 100), charToRaw("readme\n"))
})

test_that("a deflated member is inflated", {
  d <- withr::local_tempdir()
  got <- dta_zip_extract(cont_fx("deflated.zip"), c("ae.csv", "dm.csv"), c(53, 30), d)
  expect_identical(readBin(got[[1]], "raw", 100), charToRaw(CONT_AE_CSV))
  expect_identical(readBin(got[[2]], "raw", 100), charToRaw(CONT_DM_CSV))
})

test_that("a member whose size differs from the directory is refused and removed", {
  d <- withr::local_tempdir()
  e <- expect_error(
    dta_zip_extract(cont_fx("lying_size.zip"), "ae.csv", 5, d),
    class = "dta_container_corrupt"
  )
  expect_identical(e$member, "ae.csv")
  expect_false(dir.exists(d))
})

test_that("a gzip member inside a zip is extracted unchanged", {
  d <- withr::local_tempdir()
  got <- dta_zip_extract(cont_fx("sdtm_dirs.zip"), "sdtm/dm.csv.gz", 53, d)
  expect_identical(got, file.path(d, "e00001", "dm.csv.gz"))
  con <- gzfile(got, "rt")
  lines <- readLines(con)
  close(con)
  expect_identical(lines, c("USUBJID,AGE", "S-001,34", "S-002,51"))
})

test_that("a backslash entry is extracted under its base name", {
  d <- withr::local_tempdir()
  got <- dta_zip_extract(cont_fx("backslash_sep.zip"), "sdtm\\ae.csv", 53, d)
  expect_identical(got, file.path(d, "e00001", "ae.csv"))
  expect_identical(readBin(got, "raw", 100), charToRaw(CONT_AE_CSV))
  # No folder of the archive is left behind.
  expect_identical(
    list.files(d, recursive = TRUE, all.files = TRUE, include.dirs = TRUE),
    c("e00001", "e00001/ae.csv")
  )
})

test_that("an 8.3 short name cannot overwrite another member", {
  d <- withr::local_tempdir()
  got <- dta_zip_extract(cont_fx("alias83.zip"), c("abcdefghij.csv", "ABCDEF~1.CSV"), c(53, 30), d)
  expect_identical(readBin(got[[1]], "raw", 100), charToRaw(CONT_AE_CSV))
  expect_identical(readBin(got[[2]], "raw", 100), charToRaw(CONT_DM_CSV))
})

test_that("a legacy-encoded name cannot be extracted and says why", {
  z <- cont_fx("cp1252_name.zip")
  nm <- dta_zip_inspect(z)$entry
  # The archive stores the byte 0xE9 without the UTF-8 flag; the zip library
  # decodes it as code page 437.
  expect_identical(nm, "a\u0398.csv")
  d <- withr::local_tempdir()
  e <- expect_error(dta_zip_extract(z, nm, 53, d), class = "dta_container_corrupt")
  expect_true(grepl("UTF-8", conditionMessage(e), fixed = TRUE))
  expect_false(dir.exists(d))
})

# --- Transport files ----------------------------------------------------------

test_that("dta_xpt_members lists every member of a V5 library in file order", {
  res <- dta_xpt_members(cont_fx("sdtm.xpt"))
  expect_identical(res$member, c("AE", "DM", "VS"))
  expect_identical(res$name, c("ae.xpt", "dm.xpt", "vs.xpt"))
  expect_equal(res$start, c(240, 1280, 2160))
  expect_equal(res$end, c(1280, 2160, 3280))
  expect_equal(res$version, c(5, 5, 5))
})

test_that("the same members are found when a record boundary falls inside a chunk", {
  expect_equal(dta_xpt_members(cont_fx("sdtm.xpt"), chunk = 400L)$start, c(240, 1280, 2160))
  expect_equal(dta_xpt_members(cont_fx("sdtm.xpt"), chunk = 480L)$start, c(240, 1280, 2160))
})

test_that("a V8 library keeps a long member name", {
  res <- dta_xpt_members(cont_fx("sdtm_v8.xpt"))
  expect_identical(res$member, c("adverse_events", "DM"))
  expect_identical(res$name, c("adverse_events.xpt", "dm.xpt"))
  expect_equal(res$version, c(8, 8))
})

test_that("a gzip-compressed library is read through", {
  res <- dta_xpt_members(gz_fixture("adverse.xpt"))
  expect_identical(res$member, "AE")
})

test_that("files that are not SAS transport are refused", {
  expect_error(dta_xpt_members(cont_fx("not_transport.xpt")), class = "dta_xpt_not_transport")
  e <- expect_error(dta_xpt_members(cont_fx("cport.xpt")), class = "dta_xpt_not_transport")
  expect_true(grepl("CPORT", conditionMessage(e), fixed = TRUE))
})

test_that("members whose names differ only by case are refused", {
  e <- expect_error(dta_xpt_members(cont_fx("dup_member.xpt")), class = "dta_container_duplicate_member")
  expect_identical(e$members, c("AE", "ae"))
})

test_that("a library without members gives an empty listing", {
  res <- dta_xpt_members(cont_cut("sdtm.xpt", 240, "head.xpt"))
  expect_equal(nrow(res), 0)
  expect_identical(names(res), c("member", "name", "start", "end", "version"))
})

test_that("a member name that is not valid text is refused", {
  src <- cont_fx("sdtm.xpt")
  b <- readBin(src, "raw", file.size(src))
  b[409] <- as.raw(0xff) # 240 + 160 + 9: the first name byte of AE
  p <- cont_write("badname.xpt", b)
  expect_error(dta_xpt_members(p), class = "dta_container_unsafe_member")
})

test_that("a library cut inside a member's headers or before its OBS header is refused", {
  # 640, 1120: AE's NAMESTR header, and then its NAMESTR records, are the last
  # thing in the file (its OBS header record starts at 1120); 1300: inside DM's
  # member header record; 1360: DM's member header without its descriptor;
  # 1440, 1600: DM's member header and descriptor without the NAMESTR header
  # (1600: and one more record); 2000: DM's NAMESTR records without its OBS
  # header (it starts at 2000); 2240: VS's member header without its descriptor;
  # 3250 and 3270: inside the last record.
  for (n in c(640, 1120, 1300, 1360, 1440, 1600, 2000, 2240, 3250, 3270)) {
    expect_error(
      dta_xpt_members(cont_cut("sdtm.xpt", n)),
      class = "dta_container_corrupt",
      info = paste("cut at", n)
    )
  }
  # Exactly AE's end: its OBS header and its rows are all there.
  res <- dta_xpt_members(cont_cut("sdtm.xpt", 1280))
  expect_identical(res$member, "AE")
  expect_equal(res$end, 1280)
})

test_that("a V8 library cut before the OBS header of its first member is refused", {
  # adverse_events: member header at 240, OBSV8 header record at 1120.
  expect_error(dta_xpt_members(cont_cut("sdtm_v8.xpt", 1120)), class = "dta_container_corrupt")
  res <- dta_xpt_members(cont_cut("sdtm_v8.xpt", 1280))
  expect_identical(res$member, "adverse_events")
})

test_that("a member without an OBS header record is refused", {
  src <- cont_fx("sdtm.xpt")
  whole <- readBin(src, "raw", file.size(src))
  # Spoil the "OBS" of one member's OBS header record (record starts at 1120,
  # 2000 and 3120; "HEADER RECORD*******" is 20 bytes): the next member's header,
  # or the end of the file, shows that the member is incomplete.
  for (case in list(list(at = 1120, member = "AE"), list(at = 2000, member = "DM"), list(at = 3120, member = "VS"))) {
    b <- whole
    b[case$at + 20 + 1] <- charToRaw("X")
    e <- expect_error(
      dta_xpt_members(cont_write("noobs.xpt", b)),
      class = "dta_container_corrupt",
      info = case$member
    )
    expect_identical(e$member, case$member)
  }
})

test_that("an OBS header record far from its member header is found across chunks", {
  # With 400-byte chunks AE's OBS header (880 bytes after its member header)
  # arrives two chunks after the header; the members are still all accepted.
  res <- dta_xpt_members(cont_fx("sdtm.xpt"), chunk = 400L)
  expect_identical(res$member, c("AE", "DM", "VS"))
  expect_equal(res$end, c(1280, 2160, 3280))
})

test_that("a gzip library cut anywhere is refused, never listed short", {
  gz <- gz_fixture("sdtm.xpt")
  full <- readBin(gz, "raw", file.size(gz))
  expect_identical(dta_xpt_members(gz)$member, c("AE", "DM", "VS"))
  p <- file.path(withr::local_tempdir(), "cut.xpt.gz")
  not_refused <- integer()
  leaked <- character()
  for (n in 10:(length(full) - 1)) {
    writeBin(full[seq_len(n)], p)
    e <- withCallingHandlers(
      tryCatch(
        {
          dta_xpt_members(p)
          NULL
        },
        error = identity
      ),
      warning = function(w) {
        leaked <<- c(leaked, paste(n, conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    )
    if (is.null(e) || !startsWith(class(e)[[1]], "dta_")) not_refused <- c(not_refused, n)
  }
  expect_identical(not_refused, integer())
  expect_identical(leaked, character())
})

test_that("a gzip library cut where its inflated data ends on a record is refused", {
  # The first 348 of the 396 bytes of the gzip of sdtm.xpt inflate to 3040 bytes,
  # a whole number of records: only the missing gzip trailer shows the damage.
  gz <- gz_fixture("sdtm.xpt")
  expect_gt(file.size(gz), 348)
  p <- cont_write("cut.xpt.gz", readBin(gz, "raw", 348))
  expect_error(dta_xpt_members(p), class = "dta_container_corrupt")
})

test_that("a gzip file of several members is not refused for its trailer", {
  src <- cont_fx("sdtm.xpt")
  whole <- readBin(src, "raw", file.size(src))
  p <- file.path(withr::local_tempdir(), "multi.xpt.gz")
  con <- gzfile(p, "wb")
  writeBin(whole[1:1280], con)
  close(con)
  con <- gzfile(p, "ab")
  writeBin(whole[1281:length(whole)], con)
  close(con)
  res <- dta_xpt_members(p)
  expect_identical(res$member, c("AE", "DM", "VS"))
  expect_equal(res$end, c(1280, 2160, 3280))
  d <- withr::local_tempdir()
  parts <- dta_xpt_split(p, res, d)
  b <- readBin(parts[[3]], "raw", file.size(parts[[3]]))
  expect_identical(b[-(1:240)], whole[(res$start[[3]] + 1):res$end[[3]]])
})

test_that("splitting a gzip library cut anywhere is refused, never written short", {
  gz <- gz_fixture("sdtm.xpt")
  full <- readBin(gz, "raw", file.size(gz))
  members <- dta_xpt_members(gz)
  p <- file.path(withr::local_tempdir(), "cut.xpt.gz")
  d <- file.path(withr::local_tempdir(), "parts")
  not_refused <- integer()
  leaked <- character()
  for (n in 10:(length(full) - 1)) {
    writeBin(full[seq_len(n)], p)
    unlink(d, recursive = TRUE)
    e <- withCallingHandlers(
      tryCatch(
        {
          dta_xpt_split(p, members, d)
          NULL
        },
        error = identity
      ),
      warning = function(w) {
        leaked <<- c(leaked, paste(n, conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    )
    if (is.null(e) || !startsWith(class(e)[[1]], "dta_")) not_refused <- c(not_refused, n)
  }
  expect_identical(not_refused, integer())
  expect_identical(leaked, character())
  # Whole again: the same call succeeds.
  writeBin(full, p)
  unlink(d, recursive = TRUE)
  expect_length(dta_xpt_split(p, members, d), 3)
})

test_that("a split whose stream ends before a requested member starts is refused", {
  # sdtm.xpt cut at 2000 bytes never reaches VS (it starts at 2160).
  members <- dta_xpt_members(cont_fx("sdtm.xpt"))
  d <- withr::local_tempdir()
  e <- expect_error(
    dta_xpt_split(cont_cut("sdtm.xpt", 2000, "c.xpt"), members[3, ], d),
    class = "dta_container_corrupt"
  )
  expect_identical(e$member, "VS")
  expect_false(dir.exists(d))
})

test_that("a member count over the cap is refused", {
  withr::local_options(DTAtools.container_max_members = 2)
  e <- expect_error(dta_xpt_members(cont_fx("sdtm.xpt")), class = "dta_container_too_large")
  expect_equal(e$value, 3)
  expect_identical(e$limit, "DTAtools.container_max_members")
})

test_that("a header-like block that is not on a record boundary is data", {
  hdr <- "HEADER RECORD*******MEMBER  HEADER RECORD!!!!!!!000000000000000001600000000140  "
  dsc <- "HEADER RECORD*******DSCRPTR HEADER RECORD!!!!!!!000000000000000000000000000000  "
  nam <- "HEADER RECORD*******NAMESTR HEADER RECORD!!!!!!!000000000000000000000000000000  "
  mrec <- sprintf("%-80s", "SAS     ZZ      SASDATA 9.4     X64_10HO                        01JAN26:00:00:00")
  # A complete forged member (header, descriptor, two member records, NAMESTR
  # header: 400 bytes), starting 40 bytes into a data row.
  stream <- paste0(strrep(" ", 40), hdr, dsc, mrec, mrec, nam, strrep(" ", 40))
  expect_equal(nchar(stream), 480)
  rows <- substring(stream, seq(1, 480, by = 80), seq(80, 480, by = 80))
  p <- withr::local_tempfile(fileext = ".xpt")
  haven::write_xpt(data.frame(X = rows), p, version = 5, name = "AE")
  expect_identical(dta_xpt_members(p)$member, "AE")
})

test_that("a member name that is not a SAS name is refused", {
  # "A B": a blank inside the name.
  expect_error(
    dta_xpt_members(sdtm_with_member_name(charToRaw("A B     "))),
    class = "dta_container_unsafe_member"
  )
  # "A", a NUL byte, "E": only trailing NULs and blanks are padding.
  expect_error(
    dta_xpt_members(sdtm_with_member_name(c(charToRaw("A"), as.raw(0), charToRaw("E     ")))),
    class = "dta_container_unsafe_member"
  )
  # A name padded with NUL bytes instead of blanks is the name without them.
  res <- dta_xpt_members(sdtm_with_member_name(c(charToRaw("AE"), raw(6))))
  expect_identical(res$member, c("AE", "DM", "VS"))
})

test_that("values that look like header records are data", {
  hdr <- "HEADER RECORD*******MEMBER  HEADER RECORD!!!!!!!000000000000000001600000000140  "
  expect_equal(nchar(hdr), 80)
  p <- withr::local_tempfile(fileext = ".xpt")
  haven::write_xpt(data.frame(X = c(hdr, hdr, "x")), p, version = 5, name = "ODD")
  expect_identical(dta_xpt_members(p)$member, "ODD")

  # A value block forging a member header, a descriptor header and a member
  # record, but no NAMESTR header.
  dsc <- "HEADER RECORD*******DSCRPTR HEADER RECORD!!!!!!!000000000000000000000000000000  "
  mrec <- sprintf("%-80s", "SAS     ZZ      SASDATA 9.4     X64_10HO                        01JAN26:00:00:00")
  p <- withr::local_tempfile(fileext = ".xpt")
  haven::write_xpt(
    data.frame(X = c(sprintf("%-80s", paste0("ROW", 1:3)), hdr, dsc, mrec, sprintf("%-80s", paste0("ROW", 7:9)))),
    p,
    version = 5, name = "AE"
  )
  expect_identical(dta_xpt_members(p)$member, "AE")

  # A member header and a NAMESTR header but no descriptor header: all three
  # header records are required.
  nam <- "HEADER RECORD*******NAMESTR HEADER RECORD!!!!!!!000000000000000000000000000000  "
  fill <- strrep("y", 80)
  p <- withr::local_tempfile(fileext = ".xpt")
  haven::write_xpt(
    data.frame(X = c(sprintf("%-80s", paste0("ROW", 1:3)), hdr, fill, mrec, fill, nam, "z")),
    p,
    version = 5, name = "AE"
  )
  expect_identical(dta_xpt_members(p)$member, "AE")
})

test_that("a gzip stream is bounded while it is scanned", {
  src <- cont_fx("sdtm.xpt")
  p <- file.path(withr::local_tempdir(), "bomb.xpt.gz")
  con <- gzfile(p, "wb")
  writeBin(readBin(src, "raw", 240), con)
  for (i in 1:10) writeBin(raw(1e6), con)
  close(con)
  withr::local_options(DTAtools.container_max_bytes = 1e6)
  e <- expect_error(dta_xpt_members(p), class = "dta_container_too_large")
  expect_identical(e$limit, "DTAtools.container_max_bytes")
})

test_that("dta_xpt_split writes byte-identical single-member files", {
  src <- cont_fx("sdtm.xpt")
  whole <- readBin(src, "raw", file.size(src))
  members <- dta_xpt_members(src)
  d <- withr::local_tempdir()
  parts <- dta_xpt_split(src, members, d)
  expect_setequal(list.files(d), c("ae.xpt", "dm.xpt", "vs.xpt"))
  expect_identical(parts, file.path(d, c("ae.xpt", "dm.xpt", "vs.xpt")))
  for (k in seq_len(nrow(members))) {
    b <- readBin(parts[[k]], "raw", file.size(parts[[k]]))
    expect_identical(b[1:240], whole[1:240])
    expect_identical(b[-(1:240)], whole[(members$start[[k]] + 1):members$end[[k]]])
  }
  vs <- haven::read_xpt(parts[[3]])
  expect_equal(vs$VSSTRESN, c(120, 80, 135), ignore_attr = TRUE)
  expect_equal(vs$VSDT, as.Date(c("2025-08-15", "2025-08-15", "2025-08-16")), ignore_attr = TRUE)
})

test_that("only the requested members are written", {
  src <- cont_fx("sdtm_v8.xpt")
  members <- dta_xpt_members(src)
  d <- withr::local_tempdir()
  dta_xpt_split(src, members[2, ], d)
  expect_identical(list.files(d), "dm.xpt")
})

test_that("a split part is never overwritten", {
  src <- cont_fx("sdtm.xpt")
  members <- dta_xpt_members(src)
  d <- withr::local_tempdir()
  dta_xpt_split(src, members[1, ], d)
  expect_error(dta_xpt_split(src, members[1, ], d), class = "dta_container_duplicate_member")
  expect_false(dir.exists(d))
})

test_that("splitting a single-member gzip file reproduces the plain file", {
  gz <- gz_fixture("adverse.xpt")
  members <- dta_xpt_members(gz)
  d <- withr::local_tempdir()
  parts <- dta_xpt_split(gz, members, d)
  expect_length(parts, 1)
  expect_identical(
    unname(tools::md5sum(parts)),
    unname(tools::md5sum(cont_fx("adverse.xpt")))
  )
})

# --- Options ------------------------------------------------------------------

test_that("the limits have the documented defaults", {
  withr::local_options(
    DTAtools.container_max_members = NULL,
    DTAtools.container_max_bytes = NULL,
    DTAtools.container_max_member_bytes = NULL,
    DTAtools.container_max_ratio = NULL
  )
  expect_identical(
    dta_container_limits(),
    list(max_members = 10000, max_bytes = 1e13, max_member_bytes = 1e13, max_ratio = 1100)
  )
})

test_that("the limits reject nonsense", {
  for (opt in c(
    "DTAtools.container_max_members", "DTAtools.container_max_bytes",
    "DTAtools.container_max_member_bytes", "DTAtools.container_max_ratio"
  )) {
    for (bad in list(-1, "a", NA, c(1, 2))) {
      withr::with_options(
        stats::setNames(list(bad), opt),
        expect_error(dta_container_limits(), class = "dta_container_bad_option")
      )
    }
  }
})

# --- Cleanup after a failure, and device names ---------------------------------

test_that("a failed extraction removes what the container already extracted", {
  d <- withr::local_tempdir()
  # dm.csv holds 30 bytes, not 31.
  e <- expect_error(
    dta_zip_extract(cont_fx("sdtm_csv.zip"), c("ae.csv", "dm.csv"), c(53, 31), d),
    class = "dta_container_corrupt"
  )
  expect_identical(e$member, "dm.csv")
  expect_false(dir.exists(d))
})

test_that("a member named like a Windows device is refused", {
  p <- file.path(withr::local_tempdir(), "lib.xpt")
  haven::write_xpt(data.frame(USUBJID = "S-001"), p, version = 5, name = "CON")
  e <- expect_error(dta_xpt_members(p), class = "dta_container_unsafe_member")
  expect_identical(e$problems, "uses a reserved Windows device name")
  m <- flat_message(e)
  expect_match(m, "cannot write", fixed = TRUE)
  expect_false(grepl("would be unsafe", m, fixed = TRUE))
})

# --- Names reach cli text only as values, never as format strings -------------

test_that("an unsafe entry with braces in its name is data in the refusal", {
  z <- patched_name_zip(charToRaw("{1}/../"))
  e <- expect_error(dta_zip_inspect(z), class = "dta_container_unsafe_member")
  expect_true(grepl("{1}", conditionMessage(e), fixed = TRUE))
})

test_that("entry and library names with braces are data in every refusal", {
  e <- expect_error(
    dta_zip_inspect(rename_entry("encrypted.zip", "ae.csv", "{1+1}c")),
    class = "dta_container_encrypted"
  )
  expect_true(grepl("{1+1}", conditionMessage(e), fixed = TRUE))

  z <- rename_entry("dup_case.zip", c("ae.csv", "AE.CSV"), c("{1+1}c", "{1+1}C"))
  e <- expect_error(dta_zip_inspect(z), class = "dta_container_duplicate_member")
  expect_true(grepl("{1+1}", conditionMessage(e), fixed = TRUE))

  d <- withr::local_tempdir()
  e <- expect_error(
    dta_zip_extract(rename_entry("lying_size.zip", "ae.csv", "{1+1}c"), "{1+1}c", 5, d),
    class = "dta_container_corrupt"
  )
  expect_true(grepl("{1+1}", conditionMessage(e), fixed = TRUE))

  d <- withr::local_tempdir()
  e <- expect_error(
    dta_xpt_split(
      cont_cut("sdtm.xpt", 2000, "{1+1}c.xpt"),
      dta_xpt_members(cont_fx("sdtm.xpt")),
      d
    ),
    class = "dta_container_corrupt"
  )
  expect_true(grepl("{1+1}", conditionMessage(e), fixed = TRUE))
  expect_false(dir.exists(d))
})

# --- Hostile versus un-extractable entry names --------------------------------

test_that("only path-escaping names are hostile", {
  for (nm in c("/etc/x.csv", "\\\\server\\x.csv", "//server/share/x.csv", "C:x.csv", "a/../x.csv", "a//x.csv", "./x.csv")) {
    expect_false(is.na(dta_zip_entry_escape(nm)), info = nm)
  }
  # Names Windows cannot create, but which stay inside their directory: they
  # have a phrase from dta_zip_entry_problem() and none from the escape rules.
  for (nm in c("run_10:00.log", "CON.csv", "ae.csv.", "a\tb.csv", "Questions?.txt", "sdtm/Icon\r", "x<y>.csv")) {
    expect_true(is.na(dta_zip_entry_escape(nm)), info = encodeString(nm))
    expect_false(is.na(dta_zip_entry_problem(nm)), info = encodeString(nm))
  }
})

test_that("a hostile name is shown escaped", {
  z <- patched_name_zip(charToRaw("\r/../xy"))
  e <- expect_error(dta_zip_inspect(z), class = "dta_container_unsafe_member")
  expect_true(grepl("\\r/../xy", conditionMessage(e), fixed = TRUE))
  expect_false(grepl("\r", conditionMessage(e), fixed = TRUE))
})

# --- The message of a failed extraction ---------------------------------------

test_that("a failed extraction names the entry once and shows no zip internals", {
  d <- withr::local_tempdir()
  e <- tryCatch(dta_zip_extract(cont_fx("lying_size.zip"), "ae.csv", 5, d), error = identity)
  m <- conditionMessage(e)
  expect_s3_class(e, "dta_container_corrupt")
  expect_identical(lengths(regmatches(m, gregexpr("ae.csv", m, fixed = TRUE))), 1L)
  expect_false(grepl("rzip.c", m, fixed = TRUE))
  expect_true(is.null(e$parent))
})
