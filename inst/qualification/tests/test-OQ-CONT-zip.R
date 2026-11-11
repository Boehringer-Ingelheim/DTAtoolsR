# Zip archives delivered to load_delivery(). REQ-CONT-001, REQ-CONT-002,
# REQ-CONT-011 and REQ-CONT-013.
#
# Every expected value in this file is written down from the specification or
# from the literal data the fixtures were built from, never captured from a
# run. The fixtures live in fx/container/ and are built by
# .github/scripts/build_container_fixtures.R from CONT_AE_CSV and CONT_DM_CSV
# (helper-container.R):
#
#   sdtm_csv.zip      ae.csv, dm.csv, define.xml, docs/ (a folder), docs/readme.txt,
#                     __MACOSX/._ae.csv and .DS_Store: seven entries, six files
#   sdtm_dirs.zip     sdtm/ae.csv and sdtm/dm.csv.gz (the gzip of CONT_DM_CSV)
#   backslash_sep.zip sdtm\ae.csv (a backslash, as Windows PowerShell 5.1 writes
#                     it) and dm.csv
#   nested.zip        inner.zip, holding ae.csv
#   sdtm.xpt          a transport library of three datasets
#
# AE holds S-001/HEADACHE/1 and S-002/NAUSEA/2, DM holds S-001/34 and S-002/51.
# A zipped delivery is compared with the same bytes delivered loose, which is
# what REQ-CONT-002 states; the loose delivery is the oracle, not the zip path.
# Cases that need an entry name no file system here could create patch the
# bytes of a stand-in name in a zip built at run time.
#
# LIM-014 (extracted members stay on disk while they are used) is pinned in
# OQ-CONT-005.

# ---- local helpers --------------------------------------------------------------

# The table names a dataset of a delivery holds.
zp_tables <- function(d, id) names(tables(datasets(d, id)))

# Verdict and error counts of one dataset after check().
zp_verdict <- function(d, id) {
  s <- validation_status(datasets(check(d, quiet = TRUE, persist = FALSE), id))
  list(
    ok = isTRUE(s$ok[[1]]),
    columnspec = as.integer(s$n_columnspec_errors[[1]]),
    rule = as.integer(s$n_rule_errors[[1]]),
    import = as.integer(s$n_import_errors[[1]])
  )
}

ZP_KEPT <- paste0(
  "kept as one file: a type: any entry takes it as a deliverable; ",
  "to have its files checked, name it as the container of their entries"
)

zp_any_all <- function() {
  DTAtools::DTAFileAny(".*", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 10)
}

# ---- routing ----------------------------------------------------------------------

test_that("OQ-CONT-001 | a zip of CSVs is routed to every dataset whose entry matches a member | REQ-CONT-001 REQ-CONT-002", {
  ex <- cont_exdir("OQ-CONT-001")
  r <- cont_run(cont_dta("csv"), cont_fx("sdtm_csv.zip"), exdir = ex)
  qa_check("the delivery is accepted", is.null(r$error), r$error)
  d <- r$result
  plan <- r$plan

  qa_step(
    "last_delivery has exactly the documented columns, in order",
    c("delivered", "container", "member", "name", "dataset", "handler", "item", "status", "reason", "path"),
    names(plan)
  )
  qa_step("the handler column is an integer", TRUE, is.integer(plan$handler))

  qa_step("dataset ae holds the table ae", "ae", zp_tables(d, "ae"))
  qa_step("dataset dm holds the table dm", "dm", zp_tables(d, "dm"))
  qa_step("dataset vs, whose file the archive lacks, holds no table", 0L, cont_ntab(d, "vs"))
  qa_step("ae holds the AETERM values of the archive's ae.csv", c("HEADACHE", "NAUSEA"), cont_table(d, "ae", "ae")$AETERM)

  loaded <- cont_rows(plan, "loaded")
  qa_step("the two matching members are loaded, in archive order", c("ae.csv", "dm.csv"), loaded$name)
  qa_step("into datasets ae and dm", c("ae", "dm"), loaded$dataset)
  qa_step("as the items ae and dm", c("ae", "dm"), loaded$item)
  qa_step("each recording the archive as its container", rep("sdtm_csv.zip", 2), loaded$container)

  unused <- cont_rows(plan, "unused")
  qa_step("define.xml and readme.txt match no entry and are unused", c("define.xml", "readme.txt"), sort(unused$name))
  qa_step("readme.txt is recorded under its folder", "docs/readme.txt", unused$member[unused$name == "readme.txt"])
  qa_step(
    "the resource fork and .DS_Store are ignored",
    c(".DS_Store", "._ae.csv"),
    sort(cont_rows(plan, "ignored")$name, method = "radix")
  )
  opened <- cont_rows(plan, "opened")
  qa_step("the archive is opened once and is not itself inside a container", list("sdtm_csv.zip", NA_character_), list(opened$name, opened$container))

  ae_path <- loaded$path[loaded$name == "ae.csv"]
  qa_step("the ae member is extracted into a directory named after its archive", "1-sdtm_csv.zip", basename(dirname(dirname(ae_path))))
  qa_step("and into its own entry directory", "e00001", basename(dirname(ae_path)))
  on_disk <- list.files(ex, recursive = TRUE)
  qa_step("exactly the two bound files are on disk", c("ae.csv", "dm.csv"), sort(basename(on_disk)))
  if (.Platform$OS.type != "windows") {
    run_dir <- dirname(dirname(dirname(ae_path)))
    qa_step("extracted clinical data is readable by its owner only", "700", format(file.info(run_dir)$mode))
  }
})

test_that("OQ-CONT-002 | a zipped file gets the same verdict and counts as the same file loose | REQ-CONT-002", {
  # Clean: the same two files loose and in the archive.
  loose <- c(cont_write("ae.csv", CONT_AE_CSV), cont_write("dm.csv", CONT_DM_CSV))
  d_loose <- cont_run(cont_dta("csv"), loose, exdir = cont_exdir("OQ-CONT-002"))$result
  d_zip <- cont_run(cont_dta("csv"), cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-002"))$result
  clean <- list(ok = TRUE, columnspec = 0L, rule = 0L, import = 0L)
  for (id in c("ae", "dm")) {
    qa_step(sprintf("%s delivered loose is clean", id), clean, zp_verdict(d_loose, id))
    qa_step(sprintf("%s delivered in the archive is clean", id), clean, zp_verdict(d_zip, id))
  }

  # Defective: one AESEQ value, x, cannot be read as the declared SAS Num.
  bad <- paste0(CONT_AE_CSV, "S-003,VOMITING,x\n")
  v_loose <- zp_verdict(cont_run(cont_dta("csv"), cont_write("ae.csv", bad), exdir = cont_exdir("OQ-CONT-002"))$result, "ae")
  v_zip <- zp_verdict(cont_run(cont_dta("csv"), cont_zip(list(ae.csv = bad)), exdir = cont_exdir("OQ-CONT-002"))$result, "ae")
  qa_step("the defective file delivered loose has one import error", 1L, v_loose$import)
  qa_step("the defective file delivered in an archive has one import error", 1L, v_zip$import)
  qa_step("and the archive's verdict and every count equal the loose file's", v_loose, v_zip)
  qa_step("the defective delivery is not valid", FALSE, v_zip$ok)
})

test_that("OQ-CONT-003 | folders inside a zip, backslash folders and a gzip member are read | REQ-CONT-002", {
  d <- cont_run(cont_dta("csv"), cont_fx("sdtm_dirs.zip"), exdir = cont_exdir("OQ-CONT-003"))$result
  qa_step("ae.csv inside the folder sdtm/ is routed by its base name", c(1, 2), cont_table(d, "ae", "ae")$AESEQ, tolerance = 0)
  qa_step("dm.csv.gz inside a folder is read through gzip", c(34, 51), cont_table(d, "dm", "dm")$AGE, tolerance = 0)
  plan <- attr(d, "last_delivery")
  dm <- cont_rows(plan, "loaded")
  dm <- dm[dm$dataset == "dm", ]
  qa_step(
    "the dm row keeps the entry name and reports the base name and the item",
    list(member = "sdtm/dm.csv.gz", name = "dm.csv.gz", item = "dm"),
    list(member = dm$member, name = dm$name, item = dm$item)
  )

  b <- cont_run(cont_dta("csv"), cont_fx("backslash_sep.zip"), exdir = cont_exdir("OQ-CONT-003"))
  loaded <- cont_rows(b$plan, "loaded")
  qa_step("with backslash folders ae and dm are loaded", c("ae", "dm"), sort(loaded$dataset))
  ae <- loaded[loaded$dataset == "ae", ]
  qa_step(
    "the backslash entry is routed by the name after the last backslash",
    list(member = "sdtm\\ae.csv", name = "ae.csv"),
    list(member = ae$member, name = ae$name)
  )
  qa_step("and its rows are the archive's", c("HEADACHE", "NAUSEA"), cont_table(b$result, "ae", "ae")$AETERM)
})

test_that("OQ-CONT-004 | junk is ignored, folders give no row, a zip inside a zip is not opened, and a deliverable archive stays one file | REQ-CONT-011", {
  # Junk and folders: sdtm_csv.zip holds six files and the folder docs/.
  r <- cont_run(cont_dta("csv"), cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-004"))
  plan <- r$plan
  qa_step(
    "the system files are ignored with the reason 'is a system file'",
    rep("is a system file", 2),
    cont_rows(plan, "ignored")$reason
  )
  qa_step("the folder entry docs/ produces no row", 0L, sum(plan$member %in% "docs/" | plan$name %in% "docs"))
  qa_step("the plan has one row per file and one for the archive", 7L, nrow(plan))

  # A zip inside a zip is not opened.
  n <- cont_run(cont_dta("csv"), cont_fx("nested.zip"), exdir = cont_exdir("OQ-CONT-004"))
  qa_step("a delivery of only a nested archive loads nothing", TRUE, cont_is(n$error, "dta_delivery_nothing_loaded"))
  inner <- n$plan[n$plan$name == "inner.zip", ]
  qa_step(
    "the inner archive is unused, with the reason that it is not opened",
    list(rows = 1L, status = "unused", reason = "is a .zip inside a .zip, which is not opened"),
    list(rows = nrow(inner), status = inner$status, reason = inner$reason)
  )
  bundle <- cont_of(
    cont_filedata("bundle", DTAtools::DTAFileAny(".*\\.zip$", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 10)),
    cont_tab("ae", list(DTAtools::DTAFileCSV("ae.csv")))
  )
  b <- cont_run(bundle, cont_fx("nested.zip"), exdir = cont_exdir("OQ-CONT-004"))
  qa_step("an entry that takes archives binds inner.zip as a file", "loaded", b$plan$status[b$plan$name == "inner.zip"])
  qa_step("and the ae.csv inside it is never seen", 0L, cont_ntab(b$result, "ae"))

  # The Finder's Icon file and its resource fork are system files, whatever
  # their names would need on disk, also against a catch-all entry.
  z <- cont_zip(list(
    "sdtm/ae.csv" = CONT_AE_CSV, "sdtm/define.xml" = "<define/>\n",
    "sdtm/IconX" = "", "__MACOSX/sdtm/._IconX" = ""
  ))
  finder <- cont_swap(z, "IconX", "Icon\r")
  f <- cont_run(cont_dta("csv"), finder, exdir = cont_exdir("OQ-CONT-004"))
  qa_step("ae.csv of a Finder-made archive is loaded", "loaded", f$plan$status[f$plan$name == "ae.csv"])
  for (nm in c("Icon\r", "._Icon\r")) {
    row <- f$plan[f$plan$name == nm, ]
    qa_step(
      sprintf("%s is ignored as a system file", encodeString(nm)),
      list(rows = 1L, status = "ignored", reason = "is a system file"),
      list(rows = nrow(row), status = row$status, reason = row$reason)
    )
  }
  qa_check("no carriage return reaches the summary message", !grepl("\r", conditionMessage(f$summary), fixed = TRUE))
  catch_all <- cont_of(cont_tab("ae", list(DTAtools::DTAFileCSV("ae.csv"))), cont_filedata("docs", zp_any_all()))
  c_all <- cont_run(catch_all, finder, exdir = cont_exdir("OQ-CONT-004"))
  qa_check("a catch-all entry does not refuse the archive for its Finder files", is.null(c_all$error) && is.null(c_all$warning), c_all$error)
  loaded <- cont_rows(c_all$plan, "loaded")
  qa_step(
    "ae.csv goes to ae and define.xml to docs",
    list(ae = "ae", define = "docs"),
    list(ae = loaded$dataset[loaded$name == "ae.csv"], define = loaded$dataset[loaded$name == "define.xml"])
  )
  qa_step("and Icon\\r stays ignored", "ignored", c_all$plan$status[c_all$plan$name == "Icon\r"])

  # A .zip or .xpt that a type: any entry takes as a file stays one file.
  reports <- cont_of(cont_filedata(
    "reports",
    DTAtools::DTAFileAny("^report_.*", pattern = TRUE, number_of_files = 1, extensions = c("pdf", "zip"))
  ))
  rep_zip <- cont_zip(list(a.pdf = "%PDF-1\n", b.pdf = "%PDF-2\n"), name = "report_2024.zip")
  rz <- cont_run(reports, rep_zip, exdir = cont_exdir("OQ-CONT-004"))
  qa_step("a report archive that an any entry lists is bound as one file, not opened", "loaded", rz$plan$status)
  qa_step(
    "and passes the file-count check as one file",
    TRUE,
    all(validation_status(datasets(check(rz$result, quiet = TRUE, persist = FALSE), "reports"))$ok)
  )
  enc <- file.path(qa_tempdir(), "report_enc.zip")
  file.copy(cont_fx("encrypted.zip"), enc)
  re <- cont_run(reports, enc, exdir = cont_exdir("OQ-CONT-004"))
  qa_step("an encrypted archive taken as a file is never inspected, so never refused", "loaded", re$plan$status)

  docs_ae <- cont_of(
    cont_filedata("docs", DTAtools::DTAFileAny("sdtm_csv.zip")),
    cont_tab("ae", list(DTAtools::DTAFileCSV("ae.csv")))
  )
  dd <- cont_run(docs_ae, cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-004"))
  qa_step("the transfer archive named by a file entry is bound and not opened", "loaded", dd$plan$status)
  qa_step("so ae receives nothing", 0L, cont_ntab(dd$result, "ae"))
  docs_decl <- cont_of(
    cont_filedata("docs", DTAtools::DTAFileAny("sdtm_csv.zip")),
    cont_tab("ae", list(DTAtools::DTAFileCSV("ae.csv", container = "sdtm_csv.zip")))
  )
  dc <- cont_run(docs_decl, cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-004"))
  qa_step(
    "an entry that declares the archive as its container opens it as well",
    c("loaded", "opened"),
    sort(dc$plan$status[dc$plan$name == "sdtm_csv.zip"])
  )
  qa_step("and ae receives its table", "ae", zp_tables(dc$result, "ae"))

  cport160 <- cont_cut("cport.xpt", 160, "ae.xpt")
  x_any <- cont_of(cont_filedata("docs", DTAtools::DTAFileAny("ae.xpt")))
  xa <- cont_run(x_any, cport160, exdir = cont_exdir("OQ-CONT-004"))
  qa_step("a .xpt that a literal any entry names is bound unscanned, though a scan would refuse it", "loaded", xa$plan$status)

  lib_any <- cont_of(cont_filedata(
    "docs",
    DTAtools::DTAFileAny("\\.xpt$", pattern = TRUE, number_of_files = 1, extensions = "xpt")
  ))
  la <- cont_run(lib_any, cont_fx("sdtm.xpt"), exdir = cont_exdir("OQ-CONT-004"))
  qa_step("a library that a pattern any entry lists by extension is kept whole", "loaded", la$plan$status)
  qa_step(
    "and passes the file-count check as one file",
    TRUE,
    all(validation_status(datasets(check(la$result, quiet = TRUE, persist = FALSE), "docs"))$ok)
  )

  # The reason on a kept row, and in the summary.
  kept <- cont_run(docs_ae, cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-004"))
  qa_step("a deliverable kept closed says so on its loaded row", ZP_KEPT, cont_rows(kept$plan, "loaded")$reason)
  qa_step("the summary names it", "sdtm_csv.zip", kept$summary$kept)
  qa_check("and says it was kept as one file", grepl("kept as one file", conditionMessage(kept$summary), fixed = TRUE))
  qa_check(
    "an archive opened because an entry declares it carries no such reason",
    is.na(dc$plan$reason[dc$plan$name == "sdtm_csv.zip" & dc$plan$status == "loaded"])
  )
  catch_docs <- cont_of(cont_filedata("archive", zp_any_all()), cont_tab("ae", list(DTAtools::DTAFileCSV("ae.csv"))))
  cd <- cont_run(catch_docs, cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-004"))
  qa_check(
    "a catch-all entry is no deliverable: the archive is opened and no row says it was kept",
    "opened" %in% cd$plan$status[cd$plan$name == "sdtm_csv.zip"] && !ZP_KEPT %in% cd$plan$reason
  )

  # A library beside named single-dataset files is not opened, and says why.
  p2 <- cont_zip(list(ae.xpt = cont_bytes("adverse.xpt"), supp.xpt = cont_bytes("sdtm_v8.xpt")), name = "p2.zip")
  xpt_ae_dm <- cont_of(
    cont_tab("ae", list(DTAtools::DTAFileXPT("ae.xpt")), cont_empty_specs()),
    cont_tab("dm", list(DTAtools::DTAFileXPT("dm.xpt")), cont_empty_specs())
  )
  px <- cont_run(xpt_ae_dm, p2, exdir = cont_exdir("OQ-CONT-004"))
  qa_step("ae.xpt of the archive is loaded", "loaded", px$plan$status[px$plan$name == "ae.xpt"])
  supp <- px$plan[px$plan$name == "supp.xpt", ]
  qa_step("the library supp.xpt is unused", "unused", supp$status)
  qa_check(
    "its reason says it was not opened because other .xpt files are named by the specification",
    startsWith(supp$reason, "is a .xpt that was not opened: other .xpt files of p2.zip are named by the specification")
  )
  qa_check("and names the declaration that would open it", grepl("(container: supp.xpt)", supp$reason, fixed = TRUE))
  # Declaring the library as the container of dm opens it, and its member fills dm.
  decl <- cont_of(
    cont_tab("ae", list(DTAtools::DTAFileXPT("ae.xpt")), cont_empty_specs()),
    cont_tab("dm", list(DTAtools::DTAFileXPT("dm.xpt", container = "supp.xpt")), cont_empty_specs())
  )
  pd <- cont_run(decl, p2, exdir = cont_exdir("OQ-CONT-004"))
  dm_row <- pd$plan[pd$plan$name == "dm.xpt", ]
  qa_step(
    "with dm.xpt declared inside supp.xpt, the library is opened and dm.xpt is loaded from it",
    list(status = "loaded", container = "p2.zip > supp.xpt"),
    list(status = dm_row$status, container = dm_row$container)
  )
  # With every type: xpt entry satisfied there is nothing to say about it.
  ae_only <- cont_of(cont_tab("ae", list(DTAtools::DTAFileXPT("ae.xpt")), cont_empty_specs()))
  pa <- cont_run(ae_only, p2, exdir = cont_exdir("OQ-CONT-004"))
  qa_step(
    "when every type: xpt entry is satisfied, supp.xpt simply matches no entry",
    "matches no file entry of the specification",
    pa$plan$reason[pa$plan$name == "supp.xpt"]
  )
})

test_that("OQ-CONT-005 | a member two datasets need is extracted once, under its container, and stays readable | REQ-CONT-013", {
  ex <- cont_exdir("OQ-CONT-005")
  two <- cont_of(
    cont_tab("ae1", list(DTAtools::DTAFileCSV("ae.csv")), cont_specs("ae")),
    cont_tab("ae2", list(DTAtools::DTAFileCSV("ae.csv")), cont_specs("ae"))
  )
  r <- cont_run(two, cont_fx("sdtm_csv.zip"), exdir = ex)
  d <- r$result
  qa_step("both datasets receive the table ae", list("ae", "ae"), list(zp_tables(d, "ae1"), zp_tables(d, "ae2")))
  loaded <- cont_rows(r$plan, "loaded")
  qa_step("there are two loaded rows", 2L, nrow(loaded))
  qa_step("which name the same extracted file", loaded$path[[1]], loaded$path[[2]])
  qa_step("ae.csv is on disk once", 1L, sum(basename(list.files(ex, recursive = TRUE)) == "ae.csv"))
  qa_step(
    "under a directory named after its container",
    "1-sdtm_csv.zip",
    basename(dirname(dirname(loaded$path[[1]])))
  )

  # stream = "auto" decides on the member's size, not the archive's: ae.csv is
  # 53 bytes and dm.csv 30, with the threshold at 40.
  withr::local_options(DTAtools.stream_threshold = 40)
  s <- cont_run(cont_dta("csv"), cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-005"))
  qa_step(
    "the 53-byte member is opened lazily and the 30-byte member held in memory",
    list(ae_lazy = TRUE, dm_memory = TRUE),
    list(
      ae_lazy = inherits(tables(datasets(s$result, "ae"))[["ae"]], "Dataset"),
      dm_memory = inherits(tables(datasets(s$result, "dm"))[["dm"]], "Table")
    )
  )
  qa_step(
    "the lazily loaded member is still readable by check() after load_delivery() returned",
    TRUE,
    zp_verdict(s$result, "ae")$ok
  )

  # LIM-014: a lazily loaded member is read from its extracted copy, which
  # therefore stays on disk after load_delivery() has returned.
  a <- cont_run(cont_dta("csv"), cont_fx("sdtm_csv.zip"), stream = "always", exdir = cont_exdir("OQ-CONT-005"))
  lazy <- tables(datasets(a$result, "ae"))[["ae"]]
  qa_step(
    "LIM-014: a lazily loaded member is still readable after load_delivery() returned",
    2L,
    nrow(dplyr::collect(lazy))
  )
})
