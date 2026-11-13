# The exported agreement: the Delivered In column resolved to the container's
# file name, the Containers section, and the {CONTAINERS_TABLE} template block.
# REQ-CONT-014 and REQ-CONT-027.
#
# Every expected sentence, heading and cell is written from the requirement and
# the container-manager plan (D12, section 4.10), never copied from a document
# the package wrote. The Markdown writer escapes `_` and `*` in table cells, so
# STUDY_SDTM_*.zip reads STUDY\_SDTM\_\*.zip there (Appendix D).
#
#   fixture A   no default; container "sdtm" (STUDY_SDTM_*.zip, "All SDTM domains.")
#               holds ae and dm; dataset ts needs none.
#   fixture B   the default container delivery.zip; dataset docs (define.pdf)
#               says none.

# ---- local helpers --------------------------------------------------------------

# Metadata with a transmission, so that the document has a Process Information
# chapter for the Containers section to follow.
dc_meta <- function() {
  meta <- DTAtools::create_example_DTAMetaData(1)
  meta@transmission <- list(type = "sftp")
  meta
}

# A tabular dataset with one XPT file entry.
dc_ds <- function(id, file, name = NULL, container = NULL) {
  DTAtools::DTADataSetTabular(
    id = id, name = name, specs = DTAtools::create_example_DTAColumnSpecCollection(1),
    files = list(DTAtools::DTAFileXPT(file, container = container))
  )
}

dc_fixture_a <- function() {
  DTAtools::DTA(
    datasets = list(
      dc_ds("ae", "ae.xpt", "Adverse Events", "sdtm"),
      dc_ds("dm", "dm.xpt", "Demographics", "sdtm"),
      dc_ds("ts", "ts.xpt", "Trial Summary")
    ),
    metadata = dc_meta(),
    containers = list(DTAtools::DTAContainer("sdtm", "STUDY_SDTM_*.zip", description = "All SDTM domains."))
  )
}

dc_fixture_b <- function() {
  DTAtools::DTA(
    datasets = list(
      dc_ds("ae", "ae.xpt"),
      DTAtools::DTADataSetFile(id = "docs", files = list(DTAtools::DTAFileAny("define.pdf", container = "none")))
    ),
    metadata = dc_meta(),
    containers = list(DTAtools::DTAContainer("main", "delivery.zip", default = TRUE))
  )
}

dc_md <- function(dta, env = parent.frame()) {
  out <- file.path(qa_tempdir(env), "doc.md")
  DTAtools::write_dta(dta, out, format = "md", quiet = TRUE)
  readLines(out, warn = FALSE, encoding = "UTF-8")
}

dc_docx <- function(dta, env = parent.frame()) {
  out <- file.path(qa_tempdir(env), "doc.docx")
  DTAtools::write_dta(dta, out, format = "docx", quiet = TRUE)
  qa_docx_summary(out)
}

# A Word template holding one paragraph per element of `lines`.
dc_template <- function(lines, env = parent.frame()) {
  path <- file.path(qa_tempdir(env), "template.docx")
  doc <- officer::read_docx()
  for (ln in lines) doc <- officer::body_add_par(doc, ln)
  print(doc, target = path)
  path
}

DC_LOOSE <- "A file that arrives loose or inside another container does not conform to this agreement."
DC_INTRO_SOME <- "The files listed for a container must be delivered inside it. Any other file may be delivered loose or inside any container."

# ---- Markdown ---------------------------------------------------------------------------

test_that("OQ-CONT-053 | the Markdown agreement has a Containers section after Process Information, a Delivered In column that names the container's file, and the sentence that fits | REQ-CONT-027 REQ-CONT-014", {
  a <- dc_md(dc_fixture_a())
  heads <- grep("^## [0-9]+ ", a, value = TRUE)
  proc <- grep("Process Information$", heads)
  qa_step("the document has one Process Information chapter", 1L, length(proc))
  qa_check("and the next chapter is Containers", grepl("^## [0-9]+ Containers$", heads[[proc + 1L]]), heads[[proc + 1L]])
  i <- grep("^## [0-9]+ Containers$", a)
  qa_step("the sentence for entries that only some have a container follows the heading", DC_INTRO_SOME, a[[i + 2L]])
  qa_step("and then the table with its three columns", "| Container | Description | Files |", a[[i + 4L]])
  qa_check(
    "the container's row has its file name, its description and the files with their datasets' names",
    "| STUDY\\_SDTM\\_\\*.zip | All SDTM domains. | ae.xpt (Adverse Events); dm.xpt (Demographics) |" %in% a,
    a[(i + 4L):(i + 7L)]
  )

  headers <- grep("^\\| File Name / Pattern ", a, value = TRUE)
  qa_step(
    "Delivered In ends the Files table of ae and dm, and the free dataset ts has no such column",
    c(TRUE, TRUE, FALSE),
    c(endsWith(headers[[1]], "| Delivered In |"), endsWith(headers[[2]], "| Delivered In |"), grepl("Delivered In", headers[[3]], fixed = TRUE))
  )
  qa_check(
    "and the row of ae.xpt names the container's file, not its id",
    any(grepl("^\\| ae\\.xpt .*\\| STUDY\\\\_SDTM\\\\_\\\\\\*\\.zip \\|$", a)) && !any(grepl("| sdtm |", a, fixed = TRUE)),
    grep("ae.xpt", a, value = TRUE)
  )

  # Fixture B: the default container.
  b <- dc_md(dc_fixture_b())
  j <- grep("^## [0-9]+ Containers$", b)
  qa_step(
    "a default container gives the sentence for all files, with the exception for none",
    paste0(
      "All files must be delivered inside the containers below, except those whose Delivered In is Not required. ",
      DC_LOOSE
    ),
    b[[j + 2L]]
  )
  qa_check(
    "the default container's Files are all other files, except the none entry with its dataset",
    "| delivery.zip |  | All other files; except: define.pdf (docs) |" %in% b,
    b[(j + 4L):(j + 7L)]
  )
  qa_check("an entry of the default container shows its file name", any(grepl("^\\| ae\\.xpt .*\\| delivery\\.zip \\|$", b)))
  qa_check("an entry that says none shows Not required", any(grepl("^\\| define\\.pdf .*\\| Not required \\|$", b)))

  # The other sentences and cells.
  plain_default <- DTAtools::DTA(
    datasets = list(dc_ds("ae", "ae.xpt")), metadata = dc_meta(),
    containers = list(DTAtools::DTAContainer("main", "d.zip", default = TRUE))
  )
  pd <- dc_md(plain_default)
  qa_step(
    "a default container and no none entry: the sentence has no exception",
    paste("All files must be delivered inside the containers below.", DC_LOOSE),
    pd[[grep("^## [0-9]+ Containers$", pd) + 2L]]
  )
  every <- DTAtools::DTA(datasets = list(dc_ds("ae", "ae.xpt", container = "a.zip")), metadata = dc_meta())
  ev <- dc_md(every)
  qa_step(
    "no default but every entry in a container: the sentence for all files",
    paste("All files must be delivered inside the containers below.", DC_LOOSE),
    ev[[grep("^## [0-9]+ Containers$", ev) + 2L]]
  )
  several_none <- DTAtools::DTA(
    datasets = list(
      dc_ds("ae", "ae.xpt"),
      DTAtools::DTADataSetFile(id = "docs", files = list(
        DTAtools::DTAFileAny("a.pdf", container = "none"), DTAtools::DTAFileAny("b.pdf", container = "none")
      ))
    ),
    metadata = dc_meta(),
    containers = list(DTAtools::DTAContainer("main", "delivery.zip", default = TRUE))
  )
  sn <- dc_md(several_none)
  qa_check(
    "several none entries are joined with commas after 'except:'",
    "| delivery.zip |  | All other files; except: a.pdf (docs), b.pdf (docs) |" %in% sn,
    sn[grep("Container \\| Description", sn) + 2L]
  )
  idle <- DTAtools::DTA(
    datasets = list(dc_ds("ae", "ae.xpt", container = "late.zip"), dc_ds("dm", "dm.xpt", container = "first.zip")),
    metadata = dc_meta(),
    containers = list(DTAtools::DTAContainer("unused", "unused.zip"))
  )
  id <- dc_md(idle)
  k <- grep("^\\| Container \\| Description \\| Files \\|$", id)
  rows <- id[k + 2:4]
  qa_step(
    "defined containers come first in definition order, then implied ones in order of first use; one nobody uses says so",
    c("| unused.zip |  | No file assigned |", "| late.zip |  | ae.xpt (ae) |", "| first.zip |  | dm.xpt (dm) |"),
    rows
  )

  # A document written for 0.27.0 has only implied containers: the section is there all the same.
  glob <- "STUDY_SDTM_*.zip"
  old <- DTAtools::DTA(
    datasets = list(dc_ds("ae", "ae.xpt", container = glob), dc_ds("dm", "dm.xpt", container = glob), dc_ds("ts", "ts.xpt")),
    metadata = dc_meta()
  )
  o <- dc_md(old)
  qa_check(
    "implied containers get a section too, with the dataset ids",
    "| STUDY\\_SDTM\\_\\*.zip |  | ae.xpt (ae); dm.xpt (dm) |" %in% o,
    o[grep("Container \\| Description", o) + 2L]
  )
  qa_step("with the sentence for some entries", DC_INTRO_SOME, o[[grep("^## [0-9]+ Containers$", o) + 2L]])

  # No container in effect: no section.
  none <- dc_md(DTAtools::DTA(datasets = list(dc_ds("ae", "ae.xpt")), metadata = dc_meta()))
  qa_step("a document with no container in effect has no Containers section", 0L, sum(grepl("^## [0-9]+ Containers$", none)))
  # A document with only `none` entries has none in effect.
  only_none <- DTAtools::DTA(datasets = list(dc_ds("ae", "ae.xpt", container = "none")), metadata = dc_meta())
  qa_step("nor does one whose only reference is none", 0L, sum(grepl("^## [0-9]+ Containers$", dc_md(only_none))))
})

# ---- Word -------------------------------------------------------------------------------

test_that("OQ-CONT-054 | the Word agreement has a Containers chapter like Process Information, after it, and its Files tables show the resolved Delivered In | REQ-CONT-027 REQ-CONT-014", {
  s <- dc_docx(dc_fixture_a())
  par <- s[s$content_type == "paragraph", ]
  style_of <- function(txt) par$style_name[par$text == txt][[1]]
  qa_step("the Containers heading has the style of Process Information", style_of("Process Information"), style_of("Containers"))
  qa_check(
    "and comes after it",
    par$doc_index[par$text == "Process Information"][[1]] < par$doc_index[par$text == "Containers"][[1]]
  )
  qa_check("the sentence is under the heading", par$doc_index[par$text == DC_INTRO_SOME][[1]] > par$doc_index[par$text == "Containers"][[1]])
  cells <- s[s$content_type == "table cell", ]
  tbl <- cells$table_index[cells$text == "Container"][[1]]
  ct <- cells[cells$table_index == tbl, ]
  qa_step("the table's header row", c("Container", "Description", "Files"), ct$text[1:3])
  qa_step(
    "and its row: the file name, the description, the files with their datasets' names",
    c("STUDY_SDTM_*.zip", "All SDTM domains.", "ae.xpt (Adverse Events); dm.xpt (Demographics)"),
    ct$text[4:6]
  )

  # The Files tables name the file, 'Not required' for none.
  b <- dc_docx(dc_fixture_b())
  bc <- b[b$content_type == "table cell", ]
  last_cell <- function(first_cell, row_cell) {
    tbl <- unique(bc$table_index[bc$text == first_cell])
    t <- bc[bc$table_index == tbl, ]
    header <- t[t$row_id == min(t$row_id), ]
    row <- t[t$row_id == t$row_id[t$text == row_cell][[1]], ]
    c(header = header$text[[which.max(header$cell_id)]], cell = row$text[[which.max(row$cell_id)]])
  }
  qa_step(
    "the Files table of ae ends in Delivered In, and the cell is the default container's file",
    c(header = "Delivered In", cell = "delivery.zip"),
    last_cell("ae.xpt", "ae.xpt")
  )
  qa_step(
    "that of docs shows Not required for the none entry",
    c(header = "Delivered In", cell = "Not required"),
    last_cell("define.pdf", "define.pdf")
  )

  # No container: no heading.
  plain <- dc_docx(DTAtools::DTA(datasets = list(dc_ds("ae", "ae.xpt")), metadata = dc_meta()))
  qa_check("a document with no container has no Containers heading", !"Containers" %in% plain$text[plain$content_type == "paragraph"])

  # The placeholders show the file name, not the id.
  for (token in c("{FILE_SPECS}", "{DATASETS}")) {
    out <- file.path(qa_tempdir(), "tokens.docx")
    DTAtools::export_with_template(dc_fixture_a(), dc_template(token), out, quiet = TRUE)
    texts <- qa_docx_summary(out)
    tc <- texts$text[texts$content_type == "table cell"]
    qa_step(
      sprintf("%s: Delivered In is a column and holds the file name, never the id", token),
      c(TRUE, TRUE, FALSE),
      c("Delivered In" %in% tc, "STUDY_SDTM_*.zip" %in% tc, "sdtm" %in% tc)
    )
  }

  # A dataset exported on its own keeps the entry's reference as written (LIM-020).
  ds <- dc_ds("ae", "ae.xpt", container = "sdtm")
  out <- file.path(qa_tempdir(), "ae.docx")
  write_dataset_metadata(ds, out, format = "docx", quiet = TRUE)
  own <- qa_docx_summary(out)
  oc <- own$text[own$content_type == "table cell"]
  qa_step("LIM-020: a dataset exported on its own prints the reference as written", c(TRUE, TRUE), c("Delivered In" %in% oc, "sdtm" %in% oc))
})

# ---- the template block -------------------------------------------------------------------

test_that("OQ-CONT-055 | {CONTAINERS_TABLE} renders the sentence and the table, nothing when there is no container, and is listed with the placeholders | REQ-CONT-027", {
  template <- dc_template(c("A", "{CONTAINERS_TABLE}", "B"))
  out <- file.path(qa_tempdir(), "block.docx")
  DTAtools::export_with_template(dc_fixture_a(), template, out, quiet = TRUE)
  s <- qa_docx_summary(out)
  par <- s$text[s$content_type == "paragraph" & nzchar(trimws(s$text))]
  qa_step("the block is the sentence between its neighbours", c("A", DC_INTRO_SOME, "B"), par)
  tabs <- qa_docx_tables(out)
  qa_step(
    "and one table with the three columns and the container's row",
    list(n = 1L, header = c("Container", "Description", "Files"), row = c("STUDY_SDTM_*.zip", "All SDTM domains.", "ae.xpt (Adverse Events); dm.xpt (Demographics)")),
    list(n = length(tabs), header = unname(unlist(tabs[[1]][1, ])), row = unname(unlist(tabs[[1]][2, ])))
  )

  plain <- DTAtools::DTA(datasets = list(dc_ds("ae", "ae.xpt")), metadata = dc_meta())
  out2 <- file.path(qa_tempdir(), "block-none.docx")
  DTAtools::export_with_template(plain, template, out2, quiet = TRUE)
  s2 <- qa_docx_summary(out2)
  qa_step(
    "without a container the block leaves nothing: no paragraph, no table",
    list(par = c("A", "B"), tables = 0L),
    list(par = s2$text[s2$content_type == "paragraph" & nzchar(trimws(s2$text))], tables = sum(s2$content_type == "table cell"))
  )

  ph <- DTAtools::dta_template_placeholders()
  qa_step(
    "dta_template_placeholders() lists {CONTAINERS_TABLE} as a block",
    list(listed = TRUE, kind = "block"),
    list(listed = "{CONTAINERS_TABLE}" %in% names(ph), kind = unname(attr(ph, "kind")[["{CONTAINERS_TABLE}"]]))
  )
})
