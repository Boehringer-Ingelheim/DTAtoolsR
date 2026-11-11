# SAS transport (XPORT) files and libraries. REQ-CONT-003, REQ-CONT-004,
# REQ-CONT-005 and, for a library inside a zip, REQ-CONT-011.
#
# Every expected value is written down from the data the fixtures were built
# from, or from byte offsets of the library, never captured from a run. The
# fixtures live in fx/container/, built by .github/scripts/build_container_fixtures.R:
#
#   sdtm.xpt       a version 5 library of 3280 bytes, 41 records of 80:
#                  AE (bytes 240-1280), DM (1280-2160) and VS (2160-3280)
#   sdtm_v8.xpt    a version 8 library: `adverse_events` (the AE data) and DM
#   adverse.xpt    one member, AE
#   sdtm_xpt.zip   ae.xpt and dm.xpt, each a one-member file
#   lib_in_zip.zip sdtm.xpt and define.xml
#
#   AE  USUBJID  AETERM    AESEQ        DM  USUBJID  AGE
#       S-001    HEADACHE  1                S-001    34
#       S-002    NAUSEA    2                S-002    51
#   VS  USUBJID  VSTESTCD  VSSTRESN  VSDT (a SAS date, shown as DATE9.)
#       S-001    SYSBP     120       15AUG2025
#       S-001    DIABP     80        15AUG2025
#       S-002    SYSBP     135       16AUG2025
#
# haven::read_xpt() on a three-member library returned 102 rows of the first
# member's columns (observed with haven 2.5.5 when the feature was designed),
# which is why each case here states the rows and the columns of every member.
#
# LIM-013 (transport members are found by their header records) is pinned in
# OQ-CONT-008.

# ---- local helpers --------------------------------------------------------------

# Columns of a table in the order the member stored them.
xp_cols <- function(d, id, table = id) names(cont_table(d, id, table))

# The three header records of a member, as 80-character values a column can hold.
XP_HDR <- "HEADER RECORD*******MEMBER  HEADER RECORD!!!!!!!000000000000000001600000000140  "
XP_DSC <- "HEADER RECORD*******DSCRPTR HEADER RECORD!!!!!!!000000000000000000000000000000  "
XP_NAM <- "HEADER RECORD*******NAMESTR HEADER RECORD!!!!!!!000000000000000000000000000000  "
XP_MREC <- sprintf("%-80s", "SAS     ZZ      SASDATA 9.4     X64_10HO                        01JAN26:00:00:00")

# A one-column transport file whose values are `x`, written as member `name`.
xp_write <- function(x, name, file = "forged.xpt", env = parent.frame()) {
  p <- file.path(qa_tempdir(env), file)
  haven::write_xpt(data.frame(X = x), p, version = 5, name = name)
  p
}

# The nine-value AE file: values that look like the header records of a second
# member, written as ae.xpt.
xp_forged_ae <- function(env = parent.frame()) {
  xp_write(
    c(sprintf("%-80s", paste0("ROW", 1:3)), XP_HDR, XP_DSC, XP_MREC, sprintf("%-80s", paste0("ROW", 7:9))),
    name = "AE", file = "ae.xpt", env = env
  )
}

# ---- reading a library --------------------------------------------------------------

test_that("OQ-CONT-006 | each member of a V5 library is read exactly | REQ-CONT-003", {
  r <- cont_run(cont_dta("xpt"), cont_fx("sdtm.xpt"), exdir = cont_exdir("OQ-CONT-006"))
  qa_check("the library is accepted", is.null(r$error), r$error)
  d <- r$result
  loaded <- cont_rows(r$plan, "loaded")

  qa_step("each member is offered as a file named after it in lower case", c("ae.xpt", "dm.xpt", "vs.xpt"), loaded$name)
  qa_step("with the member's own name recorded", c("AE", "DM", "VS"), loaded$member)
  qa_step("inside the library as container", rep("sdtm.xpt", 3), loaded$container)
  qa_step("and routed to the datasets of the same names", c("ae", "dm", "vs"), loaded$dataset)

  # Rows and columns of each member are its own, none of another's.
  qa_step("AE keeps its three variables", c("USUBJID", "AETERM", "AESEQ"), xp_cols(d, "ae"))
  qa_step("and its two observations", 2L, nrow(cont_table(d, "ae", "ae")))
  qa_step("AETERM holds the AE values", c("HEADACHE", "NAUSEA"), cont_table(d, "ae", "ae")$AETERM)
  qa_step("DM keeps its two variables", c("USUBJID", "AGE"), xp_cols(d, "dm"))
  qa_step("and its two observations", 2L, nrow(cont_table(d, "dm", "dm")))
  qa_step("AGE holds the DM values", c(34, 51), cont_table(d, "dm", "dm")$AGE, tolerance = 0)
  vs <- cont_table(d, "vs", "vs")
  qa_step("VS keeps its four variables", c("USUBJID", "VSTESTCD", "VSSTRESN", "VSDT"), names(vs))
  qa_step("and its three observations", 3L, nrow(vs))
  qa_step("VSTESTCD holds the VS values", c("SYSBP", "DIABP", "SYSBP"), vs$VSTESTCD)
  qa_step("VSSTRESN holds the VS values", c(120, 80, 135), vs$VSSTRESN, tolerance = 0)
  qa_step("VSDT, declared a SAS date under DATE9., is read as the text SAS displays", c("15AUG2025", "15AUG2025", "16AUG2025"), vs$VSDT)
})

test_that("OQ-CONT-007 | a V8 library keeps a long member name | REQ-CONT-003", {
  dta <- cont_of(cont_tab("ae", list(DTAtools::DTAFileXPT("adverse_events.xpt")), cont_specs("ae")))
  r <- cont_run(dta, cont_fx("sdtm_v8.xpt"), exdir = cont_exdir("OQ-CONT-007"))
  qa_check("the library is accepted", is.null(r$error), r$error)
  d <- r$result
  qa_step("the member whose name has 14 characters is bound under it", "adverse_events", names(tables(datasets(d, "ae"))))
  t <- cont_table(d, "ae", "adverse_events")
  qa_step("with its two observations", 2L, nrow(t))
  qa_step("and its own variables", c("USUBJID", "AETERM", "AESEQ"), names(t))
  qa_step("and values", c("HEADACHE", "NAUSEA"), t$AETERM)
  qa_step("the other member is offered as dm.xpt and, matching no entry, unused", "dm.xpt", cont_rows(r$plan, "unused")$name)
})

test_that("OQ-CONT-008 | split members are the library's own member blocks, byte for byte | REQ-CONT-003 | tags: white-box", {
  src <- cont_fx("sdtm.xpt")
  whole <- readBin(src, "raw", file.size(src))
  members <- DTAtools:::dta_xpt_members(src)
  qa_step("the members are listed in file order", c("AE", "DM", "VS"), members$member)
  qa_step("as lower-case .xpt names", c("ae.xpt", "dm.xpt", "vs.xpt"), members$name)
  qa_step("starting at the byte offsets of the library", c(240, 1280, 2160), members$start, tolerance = 0)
  qa_step("and ending where the next member starts", c(1280, 2160, 3280), members$end, tolerance = 0)
  qa_step("all version 5", c(5, 5, 5), members$version, tolerance = 0)

  d <- qa_tempdir()
  parts <- DTAtools:::dta_xpt_split(src, members, d)
  qa_step("three single-member files are written", c("ae.xpt", "dm.xpt", "vs.xpt"), sort(list.files(d)))
  for (k in seq_len(nrow(members))) {
    b <- readBin(parts[[k]], "raw", file.size(parts[[k]]))
    qa_step(
      sprintf("%s begins with the 240 bytes of the library header", members$name[[k]]),
      whole[1:240], b[1:240]
    )
    qa_step(
      sprintf("%s continues with bytes %d to %d of the library", members$name[[k]], members$start[[k]] + 1, members$end[[k]]),
      whole[(members$start[[k]] + 1):members$end[[k]]], b[-(1:240)]
    )
  }
  # haven adds label and format attributes to every column it reads; the values
  # are what is compared.
  vs <- haven::read_xpt(parts[[3]])
  qa_step("haven reads VSSTRESN of the VS part", c(120, 80, 135), as.numeric(vs$VSSTRESN))
  qa_step(
    "and VSDT as the dates of the VS observations",
    as.Date(c("2025-08-15", "2025-08-15", "2025-08-16")),
    as.Date(as.numeric(vs$VSDT), origin = "1970-01-01")
  )

  # LIM-013: the scan finds a member by its header records. Values that copy
  # them only in part are data: the library has one member in each case.
  qa_step(
    "LIM-013: a value that copies a member header record alone starts no member",
    "ODD",
    DTAtools:::dta_xpt_members(xp_write(c(XP_HDR, XP_HDR, "x"), "ODD"))$member
  )
  qa_step(
    "LIM-013: values that copy a member header, a descriptor header and a member record, but no NAMESTR header, start no member",
    "AE",
    DTAtools:::dta_xpt_members(xp_write(
      c(sprintf("%-80s", paste0("ROW", 1:3)), XP_HDR, XP_DSC, XP_MREC, sprintf("%-80s", paste0("ROW", 7:9))),
      "AE"
    ))$member
  )
  fill <- strrep("y", 80)
  qa_step(
    "LIM-013: values that copy a member header and a NAMESTR header, but no descriptor header, start no member",
    "AE",
    DTAtools:::dta_xpt_members(xp_write(
      c(sprintf("%-80s", paste0("ROW", 1:3)), XP_HDR, fill, XP_MREC, fill, XP_NAM, "z"),
      "AE"
    ))$member
  )

  # LIM-013: a file that a type: xpt entry names is never split, so the same
  # forged values give all nine observations.
  forged <- xp_forged_ae()
  f <- cont_run(cont_dta("xpt"), forged, exdir = cont_exdir("OQ-CONT-008"))
  qa_check("LIM-013: the forged file named by its entry is accepted", is.null(f$error), f$error)
  qa_step("LIM-013: and read whole, with all nine values", 9L, nrow(cont_table(f$result, "ae", "ae")))
})

test_that("OQ-CONT-009 | a library named by a type: xpt entry is refused, naming every member | REQ-CONT-004", {
  e <- cont_catch(read_file(DTAtools::DTAFileXPT("sdtm.xpt"), cont_fx("sdtm.xpt")))
  qa_step("the handler refuses a three-member library", TRUE, cont_is(e, "dta_xpt_multiple_members"))
  qa_step("naming every member", c("AE", "DM", "VS"), e$members)

  # An entry named like the file it receives: a two-member file called ae.xpt
  # (AE and DM are the first 2160 bytes of the library).
  two <- cont_cut("sdtm.xpt", 2160, "ae.xpt")
  a <- cont_run(cont_dta("xpt"), two, exdir = cont_exdir("OQ-CONT-009"))
  qa_step("load_delivery refuses it in abort mode as a load error", TRUE, cont_is(a$error, "dta_delivery_load_error"))
  qa_step(
    "whose cause is the handler's refusal, naming the two members",
    list(cause = TRUE, members = c("AE", "DM")),
    list(cause = cont_is(a$error$parent, "dta_xpt_multiple_members"), members = a$error$parent$members)
  )
  qa_step("and binds no table", NULL, a$result)

  s <- cont_run(cont_dta("xpt"), two, on_error = "skip", exdir = cont_exdir("OQ-CONT-009"))
  qa_step("in skip mode nothing is left to load", TRUE, cont_is(s$error, "dta_delivery_nothing_loaded"))
  qa_step("and the file is listed as refused, not split", "refused", s$plan$status[s$plan$name == "ae.xpt"])
})

test_that("OQ-CONT-010 | DTAFileXPT is built three ways, accepted by a tabular dataset and held in memory | REQ-CONT-005", {
  direct <- DTAtools::DTAFileXPT(filename = "ae.xpt")
  via_factory <- DTAtools::DTAFileFactory(type = "xpt", filename = "ae.xpt")
  via_yaml <- DTAtools::dta_file_handlers_from_list(list(type = "xpt", filename = "ae.xpt"))
  qa_step("the YAML declaration yields exactly one handler", 1L, length(via_yaml))
  built <- list(direct = direct, factory = via_factory, yaml = via_yaml[[1]])
  for (how in names(built)) {
    h <- built[[how]]
    qa_step(
      sprintf("built by %s: a transport handler, a SAS handler and no delimited-text handler", how),
      list(class = "DTAtools::DTAFileXPT", sas = TRUE, tabular = FALSE, filename = "ae.xpt"),
      list(
        class = class(h)[[1]],
        sas = S7::S7_inherits(h, DTAtools::DTAFileSAS7BDAT),
        tabular = S7::S7_inherits(h, DTAtools::DTAFileTabular),
        filename = h@filename
      )
    )
  }

  ds <- DTAtools::DTADataSetTabular(id = "ae", specs = cont_specs("ae"), files = list(direct))
  qa_check("a tabular dataset accepts the handler", S7::S7_inherits(ds, DTAtools::DTADataSetTabular))

  # With a one-byte threshold every file is large, the case where a delimited
  # file would be streamed. A transport file is read into memory and the load
  # says so (LIM-010 records the consequence).
  withr::local_options(DTAtools.stream_threshold = 1)
  seen <- character(0)
  one <- DTAtools::DTADataSetTabular(id = "adverse", specs = cont_specs("ae"), files = list(DTAtools::DTAFileXPT("adverse.xpt")))
  loaded <- withCallingHandlers(
    load_file(one, file = cont_fx("adverse.xpt")),
    message = function(m) {
      seen <<- c(seen, class(m))
      invokeRestart("muffleMessage")
    }
  )
  held <- tables(loaded)[["adverse"]]
  qa_step(
    "stream = 'auto' above the threshold holds an in-memory table of both rows and says so",
    list(message = TRUE, in_memory_table = TRUE, lazy_dataset = FALSE, rows = 2L),
    list(
      message = "dta_sas7bdat_in_memory" %in% seen,
      in_memory_table = inherits(held, "Table"),
      lazy_dataset = inherits(held, "Dataset"),
      rows = nrow(held)
    )
  )
})

test_that("OQ-CONT-011 | a library inside a zip is opened, and a zip of single-dataset files is not probed | REQ-CONT-011 REQ-CONT-003", {
  ex <- cont_exdir("OQ-CONT-011")
  r <- cont_run(cont_dta("xpt"), cont_fx("lib_in_zip.zip"), exdir = ex)
  qa_check("the archive is accepted", is.null(r$error), r$error)
  loaded <- cont_rows(r$plan, "loaded")
  qa_step("the three members of the library inside are loaded", 3L, nrow(loaded))
  qa_step("each recording the archive and the library as its containers", rep("lib_in_zip.zip > sdtm.xpt", 3), loaded$container)
  qa_step("define.xml, which no entry takes, is unused", "define.xml", cont_rows(r$plan, "unused")$name)
  qa_step(
    "only the split members are on disk",
    c("ae.xpt", "dm.xpt", "vs.xpt"),
    sort(basename(list.files(ex, recursive = TRUE)))
  )
  qa_step("in a directory named after the library", "2-sdtm.xpt", unique(basename(dirname(loaded$path))))

  # An archive whose .xpt files are named by the specification is not probed
  # for libraries: lb.xpt is not an entry's file, so it is not extracted.
  dd <- qa_tempdir()
  zip::unzip(cont_fx("sdtm_xpt.zip"), files = "dm.xpt", exdir = dd)
  dm_bytes <- readBin(file.path(dd, "dm.xpt"), "raw", file.size(file.path(dd, "dm.xpt")))
  z <- cont_zip(list(ae.xpt = cont_bytes("adverse.xpt"), dm.xpt = dm_bytes, lb.xpt = cont_bytes("adverse.xpt")))
  ex2 <- cont_exdir("OQ-CONT-011")
  p <- cont_run(cont_dta("xpt"), z, exdir = ex2)
  qa_step("ae and dm are loaded", c("ae", "dm"), sort(cont_rows(p$plan, "loaded")$dataset))
  lb <- p$plan[p$plan$name == "lb.xpt", ]
  qa_step(
    "lb.xpt is unused and was never extracted",
    list(status = "unused", path = NA_character_),
    list(status = lb$status, path = lb$path)
  )
  qa_step("only the two bound files are on disk", c("ae.xpt", "dm.xpt"), sort(basename(list.files(ex2, recursive = TRUE))))

  # 1280 + 1120 bytes fit under 3000; the 1280 more of lb.xpt would not.
  withr::local_options(DTAtools.container_max_bytes = 3000)
  q <- cont_run(cont_dta("xpt"), z, exdir = cont_exdir("OQ-CONT-011"))
  qa_step(
    "a byte limit that only a probe of lb.xpt would exceed does not refuse the delivery",
    c("ae", "dm"),
    sort(cont_rows(q$plan, "loaded")$dataset)
  )
})

test_that("OQ-CONT-012 | a single-dataset transport file is matched by its own name | REQ-CONT-003", {
  dta <- cont_of(cont_tab("adverse", list(DTAtools::DTAFileXPT("adverse.xpt")), cont_empty_specs()))
  r <- cont_run(dta, cont_fx("adverse.xpt"), exdir = cont_exdir("OQ-CONT-012"))
  qa_check("a one-dataset file named as declared is accepted", is.null(r$error), r$error)
  qa_step("it is read whole, not opened", 0L, nrow(cont_rows(r$plan, "opened")))
  loaded <- cont_rows(r$plan, "loaded")
  qa_step("it is one loaded row without a container", list(rows = 1L, container = NA_character_), list(rows = nrow(loaded), container = loaded$container))
  qa_step(
    "read from the path it was given",
    normalizePath(cont_fx("adverse.xpt"), winslash = "/"),
    normalizePath(loaded$path, winslash = "/")
  )
  qa_step("with both observations", 2L, nrow(cont_table(r$result, "adverse", "adverse")))

  # Named otherwise, the same file matches no entry and is not used.
  o <- cont_run(cont_dta("xpt"), cont_fx("adverse.xpt"), exdir = cont_exdir("OQ-CONT-012"))
  qa_step("a one-dataset file called adverse.xpt matches none of ae.xpt, dm.xpt and vs.xpt", TRUE, cont_is(o$error, "dta_delivery_nothing_loaded"))
  qa_step(
    "and is listed with that reason",
    "matches no file entry of the specification",
    o$plan$reason[o$plan$name == "adverse.xpt"]
  )
  # A declaration of the file as a container opens it, and member AE fills ae.
  c1 <- cont_run(cont_dta("xpt", container = "adverse.xpt"), cont_fx("adverse.xpt"), exdir = cont_exdir("OQ-CONT-012"))
  qa_step("declared as a container, the file is opened and member AE fills ae", "ae", names(tables(datasets(c1$result, "ae"))))
})
