# What load_delivery() refuses, and what it leaves on disk when it does.
# REQ-CONT-007 (hostile and un-extractable entry names), REQ-CONT-008
# (encryption and size limits), REQ-CONT-009 (names that clash once letter case
# or an alias is ignored) and REQ-CONT-010 (damaged and foreign files), with
# REQ-CONT-002 for LIM-015.
#
# Archives are untrusted input. zip::unzip() corrupted its own error text and
# once terminated the R process on the entry a/../../evil.csv, and both it and
# utils::unzip() write an absolute entry name below their target directory
# under its full path (observed with zip 3.0.2 while the feature was
# designed). Every refusal below must therefore happen before an entry is
# extracted, and every case that refuses checks that nothing was written.
#
# Every expected value is written down from the specification or from the
# literal data of the hostile fixtures (fx/container/, built by
# .github/scripts/build_container_fixtures.R), never captured from a run:
#
#   unsafe_dotdot.zip           ae.csv and ../evil.csv
#   unsafe_dotdot_mid.zip       ae.csv and a/../../evil.csv (crashed zip::unzip)
#   unsafe_backslash_dotdot.zip ae.csv and ..\evil.csv
#   unsafe_absolute.zip         ae.csv and /tmp/dtatools-evil.csv
#   unsafe_drive.zip            ae.csv and C:/dtatools-evil.csv
#   symlink.zip                 ae.csv as a Unix symlink to ../../etc/passwd
#   unsafe_colon.zip            ae.csv:stream        (NTFS stream)
#   unsafe_reserved.zip         CON.csv              (device name)
#   unsafe_trailing_dot.zip     ae.csv.
#   unsafe_control.zip          ae<TAB>x.csv
#   encrypted.zip               ae.csv with the encryption flag
#   bomb_ratio.zip              ae.csv (53 bytes) declaring 2,000,000,000 bytes
#   lying_size.zip              ae.csv (53 bytes) declaring 5 bytes
#   zip64_count_lie.zip         three entries; 16-bit count 1, ZIP64 count 3
#   dup_entry.zip, dup_case.zip ae.csv twice; ae.csv and AE.CSV
#   dup_member.xpt              transport members AE and ae
#   alias83.zip                 abcdefghij.csv (CONT_AE_CSV), ABCDEF~1.CSV (CONT_DM_CSV)
#   cp1252_name.zip             one entry named with the bytes 61 E9 2E 63 73 76
#   truncated.zip, not_a_zip.zip, not_transport.xpt, cport.xpt
#
# A name holding braces is data in every message: a name containing {1+1}
# would show as 2 if it reached a cli format string, and one holding
# Sys.setenv() would run.
#
# LIM-015 (zip entry names without the UTF-8 flag are read as code page 437)
# is pinned in OQ-CONT-024.

# ---- local helpers --------------------------------------------------------------

# Everything under a directory, hidden files and folders included.
sf_listing <- function(dir) list.files(dir, recursive = TRUE, all.files = TRUE, include.dirs = TRUE)

# Load a fixture with exdir inside a fresh directory `p`, and say what is left
# under `p` afterwards.
sf_try <- function(fixture, dta = cont_dta("csv"), env = parent.frame()) {
  p <- qa_tempdir(env)
  r <- cont_run(dta, cont_fx(fixture), exdir = file.path(p, "x"))
  list(run = r, left = list.files(p, recursive = TRUE, all.files = TRUE))
}

# A file dataset with a catch-all type: any entry, which claims every file.
sf_docs <- function() {
  cont_of(cont_filedata(
    "docs",
    DTAtools::DTAFileAny(".*", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 10)
  ))
}

# ---- hostile and un-extractable names ----------------------------------------------

test_that("OQ-CONT-019 | a hostile entry name refuses the archive, a name that cannot be extracted here refuses it only when claimed, and nothing is written | REQ-CONT-007", {
  # Names that would write outside the extraction directory: the archive is
  # refused as a whole, before any entry is extracted, whether or not an entry
  # of the specification claims the name.
  hostile <- c(
    unsafe_dotdot.zip = "contains a '.' or '..' path segment",
    unsafe_dotdot_mid.zip = "contains a '.' or '..' path segment",
    unsafe_backslash_dotdot.zip = "contains a '.' or '..' path segment",
    unsafe_absolute.zip = "is an absolute path",
    unsafe_drive.zip = "starts with a drive letter"
  )
  for (fx in names(hostile)) {
    t <- sf_try(fx)
    e <- t$run$error
    qa_step(sprintf("%s is refused as unsafe", fx), TRUE, cont_is(e, "dta_container_unsafe_member"))
    qa_step(sprintf("%s: the problem is named", fx), unname(hostile[[fx]]), e$problems)
    qa_check(
      sprintf("%s: the message says an entry would be unsafe to extract", fx),
      !is.null(e) && grepl("would be unsafe to extract", cont_flat(e), fixed = TRUE)
    )
    qa_step(sprintf("%s: nothing is written", fx), character(0), t$left)
  }
  # The archive that crashed zip::unzip(): reaching the assertion at all shows
  # it was never handed to it.
  mid <- sf_try("unsafe_dotdot_mid.zip")$run$error
  qa_step("the entry a/../../evil.csv is the one reported", "a/../../evil.csv", mid$members)
  back <- sf_try("unsafe_backslash_dotdot.zip")$run$error
  qa_step("and ..\\evil.csv, with a backslash", "..\\evil.csv", back$members)

  # Names that some file systems (Windows) cannot hold, but that write nothing
  # outside the directory: unused when nothing claims them ...
  phrases <- c(
    unsafe_colon.zip = "contains a colon",
    unsafe_reserved.zip = "uses a reserved Windows device name",
    unsafe_trailing_dot.zip = "has a path segment ending in a dot or a space",
    unsafe_control.zip = "contains a control character"
  )
  for (fx in names(phrases)) {
    t <- sf_try(fx)
    unused <- if (is.null(t$run$plan)) NULL else cont_rows(t$run$plan, "unused")
    qa_step(
      sprintf("%s, claimed by no entry, leaves nothing to load", fx),
      TRUE, cont_is(t$run$error, "dta_delivery_nothing_loaded")
    )
    qa_step(
      sprintf("%s: the one entry is unused, its reason giving the problem", fx),
      list(rows = 1L, reason = TRUE),
      list(rows = nrow(unused), reason = isTRUE(grepl(phrases[[fx]], unused$reason, fixed = TRUE)))
    )
    qa_step(sprintf("%s, unclaimed: nothing is written", fx), character(0), t$left)

    # ... and refused, with a different message, when an entry claims them.
    c1 <- sf_try(fx, dta = sf_docs())
    e <- c1$run$error
    qa_step(sprintf("%s, claimed by a catch-all entry, is refused as unsafe", fx), TRUE, cont_is(e, "dta_container_unsafe_member"))
    qa_step(
      sprintf("%s, claimed: the message says some file systems cannot write the name, not that it is unsafe", fx),
      list(cannot_write = TRUE, unsafe = FALSE),
      list(
        cannot_write = !is.null(e) && grepl("cannot write", cont_flat(e), fixed = TRUE),
        unsafe = !is.null(e) && grepl("would be unsafe", cont_flat(e), fixed = TRUE)
      )
    )
    qa_step(sprintf("%s, claimed: nothing is written", fx), character(0), c1$left)
  }

  # Names are data in every message.
  z <- cont_zip(list("a/x.csv" = CONT_AE_CSV))
  braces <- cont_swap(z, "a/x.csv", "{1}/../")
  b <- cont_run(cont_dta("csv"), braces, exdir = cont_exdir("OQ-CONT-019"))
  qa_step("an unsafe entry named {1}/../ is refused as unsafe", TRUE, cont_is(b$error, "dta_container_unsafe_member"))
  qa_check("and its message shows {1} as written", !is.null(b$error) && grepl("{1}", conditionMessage(b$error), fixed = TRUE))

  withr::local_envvar(DTA_PWNED = "")
  nm <- "{Sys.setenv(DTA_PWNED = '1')}.sas7bdat"
  dta <- cont_of(
    cont_tab("x", list(DTAtools::DTAFileSAS7BDAT(nm)), cont_empty_specs()),
    cont_tab("dm", list(DTAtools::DTAFileCSV("dm.csv")))
  )
  skip <- cont_run(dta, cont_zip(stats::setNames(list("not SAS\n", CONT_DM_CSV), c(nm, "dm.csv"))), on_error = "skip", exdir = cont_exdir("OQ-CONT-019"))
  qa_step("a refused file named with Sys.setenv() in braces runs nothing", "", Sys.getenv("DTA_PWNED"))
  qa_check(
    "and its warning shows the name as written",
    !is.null(skip$warning) && grepl("{Sys.setenv", conditionMessage(skip$warning), fixed = TRUE)
  )
  braced_sas <- cont_of(cont_tab("x", list(DTAtools::DTAFileSAS7BDAT("{1+1}.sas7bdat")), cont_empty_specs()))
  ab <- cont_run(braced_sas, cont_zip(list("{1+1}.sas7bdat" = "not SAS\n")), exdir = cont_exdir("OQ-CONT-019"))
  qa_step("an unreadable file named {1+1}.sas7bdat aborts as a load error", TRUE, cont_is(ab$error, "dta_delivery_load_error"))
  qa_check("whose message shows {1+1} as written", !is.null(ab$error) && grepl("{1+1}", conditionMessage(ab$error), fixed = TRUE))

  # Archives whose own names hold braces, refused in every way an archive can be.
  src <- qa_tempdir()
  refusals <- c(
    encrypted.zip = "dta_container_encrypted",
    dup_case.zip = "dta_container_duplicate_member",
    not_a_zip.zip = "dta_container_unreadable",
    truncated.zip = "dta_container_unreadable",
    bomb_ratio.zip = "dta_container_too_large",
    lying_size.zip = "dta_container_corrupt",
    unsafe_dotdot.zip = "dta_container_unsafe_member"
  )
  for (fx in names(refusals)) {
    braced_zip <- file.path(src, paste0("{1+1}", fx))
    file.copy(cont_fx(fx), braced_zip)
    a <- cont_run(cont_dta("csv"), braced_zip, exdir = cont_exdir("OQ-CONT-019"))
    qa_step(
      sprintf("{1+1}%s, abort mode: refused with its own class, the name shown as written", fx),
      list(class = TRUE, braces = TRUE),
      list(
        class = cont_is(a$error, refusals[[fx]]),
        braces = !is.null(a$error) && grepl("{1+1}", conditionMessage(a$error), fixed = TRUE)
      )
    )
    s <- cont_run(cont_dta("csv"), braced_zip, on_error = "skip", exdir = cont_exdir("OQ-CONT-019"))
    qa_step(
      sprintf("{1+1}%s, skip mode: nothing loads, the name shown as written", fx),
      list(class = TRUE, braces = TRUE),
      list(
        class = cont_is(s$error, "dta_delivery_nothing_loaded"),
        braces = !is.null(s$error) && grepl("{1+1}", conditionMessage(s$error), fixed = TRUE)
      )
    )
  }
  big <- file.path(src, "{1+1}s.zip")
  file.copy(cont_fx("sdtm_csv.zip"), big)
  limited <- withr::with_options(
    list(DTAtools.container_max_bytes = 60),
    cont_run(cont_dta("csv"), big, exdir = cont_exdir("OQ-CONT-019"))
  )
  qa_step(
    "a size refusal of {1+1}s.zip names the archive as written and the sizes",
    list(class = TRUE, braces = TRUE, sizes = TRUE),
    list(
      class = cont_is(limited$error, "dta_container_too_large"),
      braces = !is.null(limited$error) && grepl("{1+1}", conditionMessage(limited$error), fixed = TRUE),
      sizes = !is.null(limited$error) && grepl("83 bytes against a limit of 60 bytes", cont_flat(limited$error), fixed = TRUE)
    )
  )
})

test_that("OQ-CONT-020 | a symbolic link is refused | REQ-CONT-007", {
  t <- sf_try("symlink.zip")
  e <- t$run$error
  qa_step("an archive holding a symbolic link is refused as unsafe", TRUE, cont_is(e, "dta_container_unsafe_member"))
  qa_step("the entry is named", "ae.csv", e$members)
  qa_step("and the problem is that it is a symbolic link", "is a symbolic link", e$problems)
  qa_check("the message says the entry would be unsafe to extract", !is.null(e) && grepl("would be unsafe to extract", cont_flat(e), fixed = TRUE))
  qa_step("nothing is written", character(0), t$left)
})

test_that("OQ-CONT-021 | an encrypted entry is refused | REQ-CONT-008", {
  t <- sf_try("encrypted.zip")
  e <- t$run$error
  qa_step("an archive with an encrypted entry is refused as encrypted", TRUE, cont_is(e, "dta_container_encrypted"))
  qa_step("the encrypted entry is named", "ae.csv", e$members)
  qa_step("nothing is written", character(0), t$left)
})

# ---- limits -------------------------------------------------------------------------

test_that("OQ-CONT-022 | every limit refuses, before writing where it can | REQ-CONT-008", {
  too_large <- function(r) {
    list(class = cont_is(r$error, "dta_container_too_large"), limit = r$error$limit %||% NA_character_)
  }
  lim_step <- function(label, r, limit, ex) {
    qa_step(sprintf("%s: refused as too large, naming the option", label), list(class = TRUE, limit = limit), too_large(r))
    qa_step(sprintf("%s: nothing is written", label), character(0), sf_listing(ex))
  }

  # Ratio: 53 bytes declaring two billion.
  ex <- cont_exdir("OQ-CONT-022")
  lim_step("a directory declaring 2e9 bytes in 168", cont_run(cont_dta("csv"), cont_fx("bomb_ratio.zip"), exdir = ex), "DTAtools.container_max_ratio", ex)

  # Entry count: the directory's own count, which the ZIP64 record can contradict.
  withr::with_options(list(DTAtools.container_max_members = 2), {
    ex <- cont_exdir("OQ-CONT-022")
    lie <- cont_run(cont_dta("csv"), cont_fx("zip64_count_lie.zip"), exdir = ex)
    lim_step("zip64_count_lie.zip (16-bit count 1, ZIP64 count 3) against 2 entries", lie, "DTAtools.container_max_members", ex)
    qa_step("and the count is the ZIP64 one", 3, lie$error$value, tolerance = 0)
    ex <- cont_exdir("OQ-CONT-022")
    seven <- cont_run(cont_dta("csv"), cont_fx("sdtm_csv.zip"), exdir = ex)
    lim_step("sdtm_csv.zip (7 entries) against 2 entries", seven, "DTAtools.container_max_members", ex)
    qa_step("and its count is the seven entries of the directory", c(value = 7, allowed = 2), c(value = seven$error$value, allowed = seven$error$allowed), tolerance = 0)
  })

  # Bytes one call writes: the two claimed entries are 53 + 30 bytes.
  withr::with_options(list(DTAtools.container_max_bytes = 60), {
    ex <- cont_exdir("OQ-CONT-022")
    bytes <- cont_run(cont_dta("csv"), cont_fx("sdtm_csv.zip"), exdir = ex)
    lim_step("sdtm_csv.zip against a byte limit of 60", bytes, "DTAtools.container_max_bytes", ex)
    qa_step("and the bytes counted are those of the two claimed entries", 83, bytes$error$value, tolerance = 0)
    qa_check(
      "the message states both sizes",
      !is.null(bytes$error) && grepl("83 bytes against a limit of 60 bytes", cont_flat(bytes$error), fixed = TRUE)
    )
  })

  # Files one call creates: three one-entry archives against a cap of two.
  withr::with_options(list(DTAtools.container_max_members = 2), {
    ex <- cont_exdir("OQ-CONT-022")
    three <- c(
      cont_zip(list(ae.csv = CONT_AE_CSV), "z1.zip"),
      cont_zip(list(dm.csv = CONT_DM_CSV), "z2.zip"),
      cont_zip(list(vs.csv = CONT_AE_CSV), "z3.zip")
    )
    files <- cont_run(cont_dta("csv"), three, exdir = ex)
    lim_step("three archives of one file each against a cap of 2 files", files, "DTAtools.container_max_members", ex)
    qa_step("and the files counted are the three", 3, files$error$value, tolerance = 0)
    qa_check(
      "the message states both counts",
      !is.null(files$error) && grepl("3 entries or files against a limit of 2 entries or files", cont_flat(files$error), fixed = TRUE)
    )
  })

  # The largest single file: ae.csv is 53 bytes, dm.csv 30.
  withr::with_options(list(DTAtools.container_max_member_bytes = 40), {
    ex <- cont_exdir("OQ-CONT-022")
    member <- cont_run(cont_dta("csv"), cont_fx("sdtm_csv.zip"), exdir = ex)
    lim_step("sdtm_csv.zip against a member limit of 40 bytes", member, "DTAtools.container_max_member_bytes", ex)
    qa_step("and the member counted is the 53-byte ae.csv", 53, member$error$value, tolerance = 0)
    skip <- cont_run(cont_dta("csv"), cont_fx("sdtm_csv.zip"), on_error = "skip", exdir = cont_exdir("OQ-CONT-022"))
    qa_step("skip mode: the 30-byte dm.csv is loaded", "dm.csv", cont_rows(skip$plan, "loaded")$name)
    qa_step(
      "and ae.csv is refused, with the cap in its reason",
      "is larger (53 bytes) than DTAtools.container_max_member_bytes (40)",
      skip$plan$reason[skip$plan$name == "ae.csv" & skip$plan$status == "refused"]
    )
  })

  # A library's members are judged before the library is split: 1280, 1120 and
  # 1360 bytes against a cap of 1000.
  withr::with_options(list(DTAtools.container_max_member_bytes = 1000), {
    ex <- cont_exdir("OQ-CONT-022")
    lib <- cont_run(cont_dta("xpt"), cont_fx("sdtm.xpt"), on_error = "skip", exdir = ex)
    qa_step("skip mode: a library whose members are all over the cap loads nothing", TRUE, cont_is(lib$error, "dta_delivery_nothing_loaded"))
    refused <- cont_rows(lib$plan, "refused")
    qa_step(
      "ae.xpt is refused with the cap and its size in the reason",
      "is larger (1280 bytes) than DTAtools.container_max_member_bytes (1000)",
      refused$reason[refused$name == "ae.xpt"]
    )
    qa_step("the path of every refused row is NA: none was split", rep(TRUE, nrow(refused)), is.na(refused$path))
    qa_step("and nothing is written", character(0), sf_listing(ex))
  })

  # A gzip stream is bounded while it is scanned: 240 header bytes, then ten
  # million zeros.
  bomb <- cont_gz(c(cont_bytes("sdtm.xpt")[1:240], rep(raw(1e6), 10)), "bomb.xpt.gz")
  withr::with_options(list(DTAtools.container_max_bytes = 1e6), {
    ex <- cont_exdir("OQ-CONT-022")
    gz <- cont_run(cont_dta("xpt", container = "bomb.xpt.gz"), bomb, exdir = ex)
    lim_step("a gzip transport file inflating to 10 MB against a byte limit of 1 MB", gz, "DTAtools.container_max_bytes", ex)

    # The same file with no container declared: a type: xpt entry reads it and
    # the limit applies the same way; a delivery whose entries are all type: csv
    # claims it with none, so it is listed as unused and the reason names the limit.
    ex <- cont_exdir("OQ-CONT-022")
    claimed <- cont_run(cont_dta("xpt"), bomb, exdir = ex)
    lim_step("the same gzip file read by a type: xpt entry", claimed, "DTAtools.container_max_bytes", ex)
    ex <- cont_exdir("OQ-CONT-022")
    unclaimed <- cont_run(cont_dta("csv"), bomb, exdir = ex)
    unused <- cont_rows(unclaimed$plan, "unused")
    qa_step("the same gzip file claimed by no entry is the one unused row", "bomb.xpt.gz", unused$name)
    qa_step(
      "and its reason names the byte limit",
      TRUE,
      grepl("DTAtools.container_max_bytes", unused$reason, fixed = TRUE)
    )
  })

  # A library inside an archive is extracted to be probed for members, or because
  # an entry declares it as a container: the cap applies to it as to any entry
  # the call extracts. lib_in_zip.zip holds sdtm.xpt, 3280 bytes (see
  # test-OQ-CONT-xpt.R), against a cap of 1000.
  withr::with_options(list(DTAtools.container_max_member_bytes = 1000), {
    ex <- cont_exdir("OQ-CONT-022")
    probed <- cont_run(cont_dta("xpt"), cont_fx("lib_in_zip.zip"), exdir = ex)
    lim_step("a library only probed for members, 3280 bytes against a member limit of 1000", probed, "DTAtools.container_max_member_bytes", ex)
    qa_step("and the size counted is the library's 3280 bytes", 3280, probed$error$value, tolerance = 0)
    ex <- cont_exdir("OQ-CONT-022")
    declared <- cont_run(cont_dta("xpt", container = "sdtm.xpt"), cont_fx("lib_in_zip.zip"), exdir = ex)
    lim_step("the same library declared as a container by the entries", declared, "DTAtools.container_max_member_bytes", ex)
  })

  # A gzip file is judged by what it inflates to, not by its size on disk. The
  # transport file is 1280 bytes inflated and 248 bytes on disk.
  adverse_gz <- cont_gz(cont_bytes("adverse.xpt"), "adverse.xpt.gz")
  e <- withr::with_options(
    list(DTAtools.container_max_member_bytes = 1000),
    cont_catch(read_file(DTAtools::DTAFileXPT("adverse.xpt"), adverse_gz))
  )
  qa_step(
    "read_file: a gzip transport file of 1280 inflated bytes is refused against a cap of 1000",
    list(class = TRUE, limit = "DTAtools.container_max_member_bytes", value = 1280),
    list(class = cont_is(e, "dta_container_too_large"), limit = e$limit, value = e$value),
    tolerance = 0
  )
  within_cap <- withr::with_options(
    list(DTAtools.container_max_member_bytes = 2000),
    nrow(read_file(DTAtools::DTAFileXPT("adverse.xpt"), adverse_gz))
  )
  qa_step("and read when the cap is 2000", 2L, within_cap)

  # A gzip file that will be read into memory: a zip member declares its
  # compressed size, so only the inflated one can exceed the cap.
  big <- paste0("USUBJID,AETERM,AESEQ\n", strrep("S-001,HEADACHE,1\n", 200))
  qa_step("the test file inflates to 3421 bytes", 3421L, nchar(big))
  big_gz <- cont_gz(big, "ae.csv.gz")
  member_gz <- withr::with_options(
    list(DTAtools.container_max_member_bytes = 1000),
    cont_run(cont_dta("csv"), cont_zip(list(ae.csv.gz = readBin(big_gz, "raw", file.size(big_gz)))), exdir = cont_exdir("OQ-CONT-022"))
  )
  qa_step(
    "a gzip CSV inside an archive that inflates past the member cap aborts as a load error caused by the cap",
    list(class = TRUE, cause = TRUE, limit = "DTAtools.container_max_member_bytes"),
    list(
      class = cont_is(member_gz$error, "dta_delivery_load_error"),
      cause = cont_is(member_gz$error$parent, "dta_container_too_large"),
      limit = member_gz$error$parent$limit
    )
  )
  iris <- file.path(qa_suite_root(), "fx", "sas", "iris.sas7bdat")
  qa_step("the SAS dataset used below is 131072 bytes", 131072, file.size(iris), tolerance = 0)
  iris_gz <- cont_gz(readBin(iris, "raw", file.size(iris)), "iris.sas7bdat.gz")
  iris_dta <- cont_of(cont_tab("iris", list(DTAtools::DTAFileSAS7BDAT("iris.sas7bdat")), cont_empty_specs()))
  sas_gz <- withr::with_options(
    list(DTAtools.container_max_member_bytes = 1e5),
    cont_run(iris_dta, iris_gz, exdir = cont_exdir("OQ-CONT-022"))
  )
  qa_step(
    "a gzip SAS dataset of 131072 inflated bytes against a cap of 100000 aborts as a load error caused by the cap",
    list(class = TRUE, cause = TRUE),
    list(class = cont_is(sas_gz$error, "dta_delivery_load_error"), cause = cont_is(sas_gz$error$parent, "dta_container_too_large"))
  )
})

# ---- names the archive library cannot return in full --------------------------------

test_that("OQ-CONT-027 | an entry name of 511 bytes or more is never extracted and refuses the archive only when something needs it | REQ-CONT-007", {
  # zip::zip_list() returns at most 511 bytes of an entry name. The names below
  # are written by cont_zip_add_empty(), as Windows cannot create such paths.
  # An umlaut is two bytes: 2 + 240 + 1 + 240 + 1 + 42 + 4 = 530 bytes, cut by the
  # library after the first byte of a two-byte character.
  umlaut <- paste0("r/", strrep("ä", 120), "/", strrep("ä", 120), "/", strrep("ä", 21), ".txt")
  qa_step("the long name is 530 bytes", 530L, nchar(umlaut, type = "bytes"))
  base <- cont_zip(list(ae.csv = CONT_AE_CSV))

  # Nothing needs the long entry: the archive is not refused, the needed sibling
  # loads, the long entry is listed as unused with the reason, and is not written.
  ex <- cont_exdir("OQ-CONT-027")
  r <- cont_run(cont_dta("csv"), cont_zip_add_empty(base, umlaut), exdir = ex)
  qa_check("an unneeded long entry does not refuse the archive", is.null(r$error), r$error)
  qa_step("the sibling ae.csv is loaded into dataset ae", "ae", cont_rows(r$plan, "loaded")$dataset)
  qa_step("holding the AETERM values of ae.csv", c("HEADACHE", "NAUSEA"), cont_table(r$result, "ae", "ae")$AETERM)
  long_row <- cont_rows(r$plan, "unused")
  qa_step("the long entry is the one unused row", 1L, nrow(long_row))
  qa_step("its reason says the name cannot be read in full", TRUE, grepl("511 bytes or more", long_row$reason, fixed = TRUE))
  qa_step("only ae.csv is written", "ae.csv", basename(list.files(ex, recursive = TRUE)))

  # Two long names that differ only past the cut are not duplicates.
  stem <- paste0(strrep("a", 200), "/", strrep("b", 200), "/", strrep("c", 150), "/")
  d <- cont_run(cont_dta("csv"), cont_zip_add_empty(base, paste0(stem, c("one.txt", "two.txt"))), exdir = cont_exdir("OQ-CONT-027"))
  qa_check("two long names differing only after byte 511 are not refused as duplicates", is.null(d$error), d$error)
  qa_step("and the sibling is still loaded", "ae", cont_rows(d$plan, "loaded")$dataset)

  # An entry the specification needs, 511 bytes exactly: 200 + 1 + 200 + 1 + 102 + 7.
  needed <- paste0(strrep("d", 200), "/", strrep("e", 200), "/", strrep("f", 102), "/ae.csv")
  qa_step("the needed name is 511 bytes", 511L, nchar(needed, type = "bytes"))
  ex <- cont_exdir("OQ-CONT-027")
  n <- cont_run(cont_dta("csv"), cont_zip_add_empty(cont_zip(list(x.txt = "x")), needed), exdir = ex)
  qa_step("a needed entry whose name cannot be read in full refuses the archive as an unsafe member", TRUE, cont_is(n$error, "dta_container_unsafe_member"))
  qa_step(
    "the message says the name cannot be read in full",
    TRUE, !is.null(n$error) && grepl("511 bytes or more, which cannot be read in full", cont_flat(n$error), fixed = TRUE)
  )
  qa_step(
    "and does not say that the name would be unsafe",
    FALSE, !is.null(n$error) && grepl("would be unsafe", cont_flat(n$error), fixed = TRUE)
  )
  qa_step("nothing is written", character(0), sf_listing(ex))
})

# ---- clashing names -----------------------------------------------------------------

test_that("OQ-CONT-023 | names equal but for letter case, or aliases, cannot replace each other | REQ-CONT-009 | tags: white-box", {
  for (fx in c("dup_case.zip", "dup_entry.zip")) {
    ex <- cont_exdir("OQ-CONT-023")
    r <- cont_run(cont_dta("csv"), cont_fx(fx), exdir = ex)
    qa_step(sprintf("%s is refused as holding duplicate names", fx), TRUE, cont_is(r$error, "dta_container_duplicate_member"))
    qa_step(sprintf("%s: nothing is written", fx), character(0), sf_listing(ex))
  }
  # The library is declared as a container so that the scan's own class
  # reaches the caller; undeclared, it would only be listed as unused.
  ex <- cont_exdir("OQ-CONT-023")
  lib <- cont_run(cont_dta("xpt", container = "dup_member.xpt"), cont_fx("dup_member.xpt"), exdir = ex)
  qa_step("a library with members AE and ae is refused as holding duplicate names", TRUE, cont_is(lib$error, "dta_container_duplicate_member"))
  qa_step("naming both members", c("AE", "ae"), lib$error$members)
  qa_step("and nothing is written", character(0), sf_listing(ex))

  # Behind the scan: a split part is never written over another one, so a
  # member offered twice cannot replace the file the first one wrote.
  lib_path <- cont_fx("sdtm.xpt")
  first <- DTAtools:::dta_xpt_members(lib_path)[1, ]
  into <- qa_tempdir()
  DTAtools:::dta_xpt_split(lib_path, first, into)
  again <- cont_catch(DTAtools:::dta_xpt_split(lib_path, first, into))
  qa_step("splitting the same member into one directory twice is refused as a duplicate", TRUE, cont_is(again, "dta_container_duplicate_member"))
  qa_step("and the directory of the first split is removed", FALSE, dir.exists(into))

  # 8.3 aliases: on a volume that makes short names, abcdefghij.csv can also be
  # reached as ABCDEF~1.CSV. Each member is extracted into a directory of its
  # own, so neither replaces the other.
  both <- cont_of(
    cont_tab("alpha", list(DTAtools::DTAFileCSV("abcdefghij.csv")), cont_empty_specs()),
    cont_tab("beta", list(DTAtools::DTAFileCSV("ABCDEF~1.CSV")), cont_empty_specs())
  )
  a <- cont_run(both, cont_fx("alias83.zip"), exdir = cont_exdir("OQ-CONT-023"))
  qa_check("the archive with an entry and its 8.3 alias is accepted", is.null(a$error), a$error)
  alpha <- cont_table(a$result, "alpha", "abcdefghij")
  beta <- cont_table(a$result, "beta", "ABCDEF~1")
  qa_step(
    "abcdefghij.csv holds the AE columns and rows",
    list(names = c("USUBJID", "AETERM", "AESEQ"), rows = 2L),
    list(names = names(alpha), rows = nrow(alpha))
  )
  qa_step(
    "ABCDEF~1.CSV holds the DM columns and rows",
    list(names = c("USUBJID", "AGE"), rows = 2L),
    list(names = names(beta), rows = nrow(beta))
  )
})

# ---- damaged and foreign files --------------------------------------------------------

test_that("OQ-CONT-024 | damaged and foreign files are refused | REQ-CONT-010 REQ-CONT-002", {
  # Zip archives.
  ex <- cont_exdir("OQ-CONT-024")
  lying <- cont_run(cont_dta("csv"), cont_fx("lying_size.zip"), exdir = ex)
  qa_step("a member that is not the size its archive declares is refused as corrupt", TRUE, cont_is(lying$error, "dta_container_corrupt"))
  qa_step("and nothing extracted from it remains", character(0), sf_listing(ex))
  for (fx in c("truncated.zip", "not_a_zip.zip")) {
    r <- cont_run(cont_dta("csv"), cont_fx(fx), exdir = cont_exdir("OQ-CONT-024"))
    qa_step(sprintf("%s is refused as not a readable archive", fx), TRUE, cont_is(r$error, "dta_container_unreadable"))
  }

  # Transport files, declared so that the class of the scan reaches the caller.
  for (fx in c("not_transport.xpt", "cport.xpt")) {
    r <- cont_run(cont_dta("xpt", container = fx), cont_fx(fx), exdir = cont_exdir("OQ-CONT-024"))
    qa_step(sprintf("%s, declared as a container, is refused as not SAS transport", fx), TRUE, cont_is(r$error, "dta_xpt_not_transport"))
    h <- cont_catch(read_file(DTAtools::DTAFileXPT(fx), cont_fx(fx)))
    qa_step(sprintf("%s, read by a type: xpt entry, is refused the same way", fx), TRUE, cont_is(h, "dta_xpt_not_transport"))
  }

  # A library cut inside a member header (1300, 2240) or inside the last record
  # (3250) is refused, and no member of it is read.
  for (n in c(1300, 2240, 3250)) {
    cut <- cont_cut("sdtm.xpt", n, "sdtm.xpt")
    r <- cont_run(cont_dta("xpt", container = "sdtm.xpt"), cut, exdir = cont_exdir("OQ-CONT-024"))
    qa_step(
      sprintf("sdtm.xpt cut at %d bytes is refused as corrupt, and no table is bound", n),
      list(class = TRUE, result = NULL),
      list(class = cont_is(r$error, "dta_container_corrupt"), result = r$result)
    )
  }
  # Undeclared, the same cut is only unused, and the reason says why.
  u <- cont_run(cont_dta("xpt"), cont_cut("sdtm.xpt", 1300, "sdtm.xpt"), exdir = cont_exdir("OQ-CONT-024"))
  qa_step("undeclared, the 1300-byte cut leaves nothing to load", TRUE, cont_is(u$error, "dta_delivery_nothing_loaded"))
  row <- u$plan[u$plan$name == "sdtm.xpt", ]
  qa_step(
    "and is listed as unused with the reason that it is truncated",
    list(status = "unused", truncated = TRUE),
    list(status = row$status, truncated = grepl("truncated", row$reason, fixed = TRUE))
  )

  # LIM-015: an entry name without the UTF-8 flag is read as code page 437, so
  # the Windows-1252 byte E9 (e-acute) is a Greek capital theta.
  entry <- cont_of(cont_tab("ae", list(DTAtools::DTAFileCSV("a\u00e9.csv")), cont_empty_specs()))
  c437 <- cont_run(entry, cont_fx("cp1252_name.zip"), exdir = cont_exdir("OQ-CONT-024"))
  qa_step("LIM-015: an entry declared as a-e-acute.csv finds nothing in the archive", TRUE, cont_is(c437$error, "dta_delivery_nothing_loaded"))
  qa_step(
    "LIM-015: the plan lists the entry under the code page 437 reading of its name, as unused",
    "a\u0398.csv",
    cont_rows(c437$plan, "unused")$name
  )
})
