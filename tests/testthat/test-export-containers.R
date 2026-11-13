# The Containers section of the exported documents (Word, Markdown, the
# {CONTAINERS_TABLE} template block) and the resolved "Delivered In" column.
# Every expectation is written by hand from the specification (heading name and
# level, header cells, intro sentences, Files texts), never pasted from output.

# Metadata with a transmission, so that the document has a Process Information
# chapter.
.ec_meta <- function() {
  meta <- create_example_DTAMetaData(1)
  meta@transmission <- list(type = "sftp")
  meta
}

# A tabular dataset with one file entry.
.ec_ds <- function(id, file, name = NULL, container = NULL) {
  DTADataSetTabular(
    id = id, name = name, specs = create_example_DTAColumnSpecCollection(1),
    files = list(DTAFileXPT(file, container = container))
  )
}

# The Markdown writer escapes `_` and `*` in cells, so STUDY_SDTM_*.zip reads
# STUDY\_SDTM\_\*.zip in the table.
# Fixture A: no default; container `sdtm` (STUDY_SDTM_*.zip) holds ae and dm,
# dataset `ts` is free.
.ec_fixture_a <- function() {
  DTA(
    datasets = list(
      .ec_ds("ae", "ae.xpt", "Adverse Events", "sdtm"),
      .ec_ds("dm", "dm.xpt", "Demographics", "sdtm"),
      .ec_ds("ts", "ts.xpt", "Trial Summary")
    ),
    metadata = .ec_meta(),
    containers = list(DTAContainer("sdtm", "STUDY_SDTM_*.zip", description = "All SDTM domains."))
  )
}

# Fixture B: default container delivery.zip; define.pdf (dataset `docs`) is
# exempt (`none`).
.ec_fixture_b <- function() {
  DTA(
    datasets = list(
      .ec_ds("ae", "ae.xpt"),
      DTADataSetFile(id = "docs", files = list(DTAFileAny("define.pdf", container = "none")))
    ),
    metadata = .ec_meta(),
    containers = list(DTAContainer("main", "delivery.zip", default = TRUE))
  )
}

.ec_md <- function(dta) {
  out <- withr::local_tempfile(fileext = ".md", .local_envir = parent.frame())
  write_dta(dta, out, format = "md", quiet = TRUE)
  readLines(out, warn = FALSE, encoding = "UTF-8")
}

.ec_docx <- function(dta) {
  out <- withr::local_tempfile(fileext = ".docx", .local_envir = parent.frame())
  write_dta(dta, out, format = "docx", quiet = TRUE)
  officer::docx_summary(officer::read_docx(out))
}

.ec_intro3 <- "The files listed for a container must be delivered inside it. Any other file may be delivered loose or inside any container."
.ec_loose <- "A file that arrives loose or inside another container does not conform to this agreement."

test_that("markdown fixture A: Containers follows Process Information at its level", {
  lines <- .ec_md(.ec_fixture_a())
  heads <- grep("^## [0-9]+ ", lines, value = TRUE)
  proc <- grep("Process Information$", heads)
  expect_length(proc, 1L)
  expect_match(heads[[proc + 1L]], "^## [0-9]+ Containers$")

  i <- grep("^## [0-9]+ Containers$", lines)
  expect_identical(lines[[i + 2L]], .ec_intro3)
  expect_identical(lines[[i + 4L]], "| Container | Description | Files |")
  expect_true(
    "| STUDY\\_SDTM\\_\\*.zip | All SDTM domains. | ae.xpt (Adverse Events); dm.xpt (Demographics) |" %in% lines
  )
})

test_that("markdown fixture A: Delivered In is the container file name; a free table has no such column", {
  lines <- .ec_md(.ec_fixture_a())
  file_headers <- grep("^\\| File Name / Pattern ", lines, value = TRUE)
  expect_length(file_headers, 3L)
  expect_match(file_headers[[1]], "\\| Delivered In \\|$")
  expect_match(file_headers[[2]], "\\| Delivered In \\|$")
  expect_false(grepl("Delivered In", file_headers[[3]], fixed = TRUE))
  expect_true(any(grepl("^\\| ae\\.xpt .*\\| STUDY\\\\_SDTM\\\\_\\\\\\*\\.zip \\|$", lines)))
})

test_that("markdown fixture B: intro variant 1 with the except clause, default Files text", {
  lines <- .ec_md(.ec_fixture_b())
  i <- grep("^## [0-9]+ Containers$", lines)
  expect_length(i, 1L)
  expect_identical(
    lines[[i + 2L]],
    paste0(
      "All files must be delivered inside the containers below, except those whose ",
      "Delivered In is Not required. ", .ec_loose
    )
  )
  expect_true("| delivery.zip |  | All other files; except: define.pdf (docs) |" %in% lines)
  expect_true(any(grepl("^\\| ae\\.xpt .*\\| delivery\\.zip \\|$", lines)))
  expect_true(any(grepl("^\\| define\\.pdf .*\\| Not required \\|$", lines)))
})

test_that("a document without containers has no Containers section", {
  dta <- DTA(datasets = list(.ec_ds("ae", "ae.xpt")), metadata = .ec_meta())
  expect_false(any(grepl("^## [0-9]+ Containers$", .ec_md(dta))))
  s <- .ec_docx(dta)
  expect_false("Containers" %in% s$text[s$content_type == "paragraph"])
})

test_that("word: Containers heading shares the style of Process Information; intro and first row", {
  s <- .ec_docx(.ec_fixture_a())
  par <- s[s$content_type == "paragraph", ]
  style_of <- function(txt) par$style_name[par$text == txt][[1]]
  expect_identical(style_of("Containers"), style_of("Process Information"))
  expect_lt(par$doc_index[par$text == "Process Information"][[1]], par$doc_index[par$text == "Containers"][[1]])

  expect_true(.ec_intro3 %in% par$text)
  expect_gt(par$doc_index[par$text == .ec_intro3][[1]], par$doc_index[par$text == "Containers"][[1]])

  cells <- s[s$content_type == "table cell", ]
  tbl <- cells$table_index[cells$text == "Container"][[1]]
  ct <- cells[cells$table_index == tbl, ]
  expect_identical(ct$text[1:3], c("Container", "Description", "Files"))
  expect_identical(
    ct$text[4:6],
    c("STUDY_SDTM_*.zip", "All SDTM domains.", "ae.xpt (Adverse Events); dm.xpt (Demographics)")
  )
})

test_that("word fixture B shows the resolved Delivered In, 'Not required' for none", {
  s <- .ec_docx(.ec_fixture_b())
  cells <- s[s$content_type == "table cell", ]
  # The Files table whose first data cell is `first`: header and data row.
  files_table <- function(first) {
    tbl <- unique(cells$table_index[cells$text == first])
    expect_length(tbl, 1L)
    cells[cells$table_index == tbl, ]
  }
  last_cell <- function(ct, row) {
    r <- ct[ct$row_id == row, ]
    r$text[[which.max(r$cell_id)]]
  }
  ae <- files_table("ae.xpt")
  expect_identical(last_cell(ae, min(ae$row_id)), "Delivered In")
  expect_identical(last_cell(ae, ae$row_id[ae$text == "ae.xpt"][[1]]), "delivery.zip")
  pdf <- files_table("define.pdf")
  expect_identical(last_cell(pdf, min(pdf$row_id)), "Delivered In")
  expect_identical(last_cell(pdf, pdf$row_id[pdf$text == "define.pdf"][[1]]), "Not required")
})

test_that("a template {FILE_SPECS} and {DATASETS} show the container file name, not its id", {
  for (token in c("{FILE_SPECS}", "{DATASETS}")) {
    template <- .make_template(token)
    out <- tempfile(fileext = ".docx")
    export_with_template(.ec_fixture_a(), template, out, quiet = TRUE)
    s <- officer::docx_summary(officer::read_docx(out))
    unlink(c(template, out), force = TRUE)
    cells <- s$text[s$content_type == "table cell"]
    expect_true("Delivered In" %in% cells, info = token)
    expect_true("STUDY_SDTM_*.zip" %in% cells, info = token)
    expect_false("sdtm" %in% cells, info = token)
  }
})

test_that("markdown of a 0.27.0-style document (implied containers only) has the Containers section", {
  esc <- function(x) gsub("([_*])", "\\\\\\1", x)
  glob <- "STUDY_SDTM_*.zip"
  dta <- DTA(
    datasets = list(
      .ec_ds("ae", "ae.xpt", container = glob),
      .ec_ds("dm", "dm.xpt", container = glob),
      .ec_ds("ts", "ts.xpt")
    ),
    metadata = .ec_meta()
  )
  lines <- .ec_md(dta)
  i <- grep("^## [0-9]+ Containers$", lines)
  expect_length(i, 1L)
  expect_identical(lines[[i + 2L]], .ec_intro3)
  expect_true(paste0("| ", esc(glob), " |  | ae.xpt (ae); dm.xpt (dm) |") %in% lines)
})

test_that("a none entry reads '' (no Delivered In column) when no container is in effect", {
  files <- list(DTAFileXPT("x.xpt", container = "none"))
  dta <- DTA(
    datasets = list(DTADataSetTabular(id = "ae", specs = create_example_DTAColumnSpecCollection(1), files = files)),
    metadata = .ec_meta()
  )
  df <- .file_specs_df(files, ctx = .container_ctx(dta), dataset = "ae")
  expect_false("Delivered In" %in% names(df))
})

test_that(".containers_intro is '' for a NULL context and for a context without containers", {
  expect_identical(.containers_intro(NULL), "")
  dta <- DTA(datasets = list(.ec_ds("ae", "ae.xpt")), metadata = .ec_meta())
  expect_identical(.containers_intro(.container_ctx(dta)), "")
})

test_that(".file_specs_df without ctx keeps the raw reference; with ctx it resolves", {
  files <- list(DTAFileXPT("ae.xpt", container = "sdtm"), DTAFileXPT("dm.xpt"), DTAFileXPT("x.xpt", container = "none"))
  raw <- .file_specs_df(files)
  expect_identical(raw[["Delivered In"]], c("sdtm", "", "none"))

  dta <- DTA(
    datasets = list(DTADataSetTabular(id = "ae", specs = create_example_DTAColumnSpecCollection(1), files = files)),
    metadata = .ec_meta(),
    containers = list(DTAContainer("sdtm", "s_*.zip"))
  )
  ctx <- .container_ctx(dta)
  expect_identical(.file_specs_df(files, ctx = ctx)[["Delivered In"]], c("s_*.zip", "", "Not required"))
  expect_identical(.file_specs_df(files, ctx = ctx, dataset = "ae")[["Delivered In"]], c("s_*.zip", "", "Not required"))
  expect_null(.container_ctx(NULL))
})

test_that(".containers_df orders explicit then implied, and words empty and none cases", {
  dta <- DTA(
    datasets = list(
      .ec_ds("ae", "ae.xpt", container = "late.zip"),
      .ec_ds("dm", "dm.xpt", container = "first.zip")
    ),
    metadata = .ec_meta(),
    containers = list(DTAContainer("unused", "unused.zip"))
  )
  df <- .containers_df(.container_ctx(dta))
  expect_identical(names(df), c("Container", "Description", "Files"))
  expect_identical(df$Container, c("unused.zip", "late.zip", "first.zip"))
  expect_identical(df$Files, c("No file assigned", "ae.xpt (ae)", "dm.xpt (dm)"))
})

test_that("several none entries are joined with ', ' after 'except:'", {
  dta <- DTA(
    datasets = list(
      .ec_ds("ae", "ae.xpt"),
      DTADataSetFile(id = "docs", files = list(
        DTAFileAny("a.pdf", container = "none"), DTAFileAny("b.pdf", container = "none")
      ))
    ),
    metadata = .ec_meta(),
    containers = list(DTAContainer("main", "delivery.zip", default = TRUE))
  )
  expect_identical(
    .containers_df(.container_ctx(dta))$Files,
    "All other files; except: a.pdf (docs), b.pdf (docs)"
  )
})

test_that("the intro takes variant 2 when every entry has a container, 1 without except for a plain default", {
  all_in <- DTA(
    datasets = list(.ec_ds("ae", "ae.xpt", container = "a.zip")),
    metadata = .ec_meta()
  )
  expect_identical(
    .containers_intro(.container_ctx(all_in)),
    paste("All files must be delivered inside the containers below.", .ec_loose)
  )
  plain_default <- DTA(
    datasets = list(.ec_ds("ae", "ae.xpt")),
    metadata = .ec_meta(),
    containers = list(DTAContainer("main", "d.zip", default = TRUE))
  )
  expect_identical(
    .containers_intro(.container_ctx(plain_default)),
    paste("All files must be delivered inside the containers below.", .ec_loose)
  )
})

test_that("{CONTAINERS_TABLE} renders intro + table between its neighbours; nothing without containers", {
  template <- .make_template(c("A", "{CONTAINERS_TABLE}", "B"))
  on.exit(unlink(template, force = TRUE), add = TRUE)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out, force = TRUE), add = TRUE)

  export_with_template(.ec_fixture_a(), template, out, quiet = TRUE)
  s <- officer::docx_summary(officer::read_docx(out))
  par <- s$text[s$content_type == "paragraph" & nzchar(trimws(s$text))]
  expect_identical(par, c("A", .ec_intro3, "B"))
  expect_identical(length(unique(s$table_index[s$content_type == "table cell"])), 1L)

  plain <- DTA(datasets = list(.ec_ds("ae", "ae.xpt")), metadata = .ec_meta())
  export_with_template(plain, template, out, quiet = TRUE, overwrite = TRUE)
  s <- officer::docx_summary(officer::read_docx(out))
  expect_identical(s$text[s$content_type == "paragraph" & nzchar(trimws(s$text))], c("A", "B"))
  expect_false(any(s$content_type == "table cell"))
})

test_that("{CONTAINERS_TABLE} is in the block catalogue", {
  expect_true("{CONTAINERS_TABLE}" %in% names(.tv_block_catalog()))
  expect_false(is.null(.tv_parse_block_token("{CONTAINERS_TABLE}")))
})

test_that("a dataset-only Word export shows the raw reference and does not error", {
  ds <- .ec_ds("ae", "ae.xpt", container = "sdtm")
  out <- withr::local_tempfile(fileext = ".docx")
  expect_no_error(write_dataset_metadata(ds, out, format = "docx", quiet = TRUE))
  s <- officer::docx_summary(officer::read_docx(out))
  cells <- s$text[s$content_type == "table cell"]
  expect_true("Delivered In" %in% cells)
  expect_true("sdtm" %in% cells)
})
