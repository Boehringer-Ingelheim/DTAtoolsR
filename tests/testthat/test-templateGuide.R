# The RENDER half of write_template_guide(): one document per template, read
# back from the written docx. Expected values come from the YAML files (or are
# spelled out), never from the software. Messages are never asserted on, only
# condition classes (R and cli translate them).

tg_lib <- function() system.file("extdata", "templates", package = "DTAtools")

tg_yaml <- function(id) {
  yaml::read_yaml(file.path(tg_lib(), paste0(id, ".dta-template.yaml")))
}

# Bundled guides are written once per test file.
tg_cache <- new.env()
tg_written <- function(org = "Example Pharma", unit = "Biometrics") {
  key <- paste(org, unit)
  if (is.null(tg_cache[[key]])) {
    dir <- tempfile("guides")
    tg_cache$dirs <- c(tg_cache$dirs, dir)
    tg_cache[[key]] <- write_template_guide(
      tg_lib(), dir,
      organisation = org, organisation_unit = unit, quiet = TRUE
    )
  }
  tg_cache[[key]]
}

tg_unzip <- function(file) {
  ex <- tempfile("unz")
  utils::unzip(file, exdir = ex)
  ex
}

tg_xml_texts <- function(file, part_pattern) {
  ex <- tg_unzip(file)
  on.exit(unlink(ex, recursive = TRUE), add = TRUE)
  parts <- list.files(file.path(ex, "word"), part_pattern, full.names = TRUE)
  vapply(parts, function(p) {
    x <- xml2::read_xml(p)
    paste(xml2::xml_text(xml2::xml_find_all(x, ".//*[local-name()='t' or local-name()='instrText']")), collapse = "")
  }, character(1))
}

tg_text <- function(file) {
  s <- officer::docx_summary(officer::read_docx(file))
  paste(s$text[!is.na(s$text)], collapse = "\n")
}

# Every table with its section's orientation and text width (twips).
tg_tables <- function(file) {
  ex <- tg_unzip(file)
  on.exit(unlink(ex, recursive = TRUE), add = TRUE)
  body <- xml2::xml_children(xml2::xml_find_first(
    xml2::read_xml(file.path(ex, "word", "document.xml")), "//*[local-name()='body']"
  ))
  out <- list()
  pending <- list()
  close_section <- function(sect) {
    sz <- xml2::xml_find_first(sect, "./*[local-name()='pgSz']")
    mar <- xml2::xml_find_first(sect, "./*[local-name()='pgMar']")
    num <- function(node, a) as.numeric(xml2::xml_attr(node, a))
    info <- list(
      orient = xml2::xml_attr(sz, "orient"),
      text_w = num(sz, "w") - num(mar, "left") - num(mar, "right")
    )
    for (t in pending) {
      grid <- as.numeric(xml2::xml_attr(
        xml2::xml_find_all(t, "./*[local-name()='tblGrid']/*[local-name()='gridCol']"), "w"
      ))
      out[[length(out) + 1L]] <<- c(info, list(grid = grid))
    }
    pending <<- list()
  }
  for (node in body) {
    nm <- xml2::xml_name(node)
    if (nm == "tbl") {
      pending[[length(pending) + 1L]] <- node
    } else if (nm == "p") {
      sect <- xml2::xml_find_first(node, "./*[local-name()='pPr']/*[local-name()='sectPr']")
      if (!inherits(sect, "xml_missing")) close_section(sect)
    } else if (nm == "sectPr") {
      close_section(node)
    }
  }
  out
}

# ---- selection and file names ----------------------------------------------

test_that("one file per non-abstract template, named from the YAML", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  written <- tg_written()
  files <- list.files(tg_lib(), "\\.dta-template\\.yaml$", full.names = TRUE)
  y <- lapply(files, yaml::read_yaml)
  y <- Filter(function(d) !isTRUE(d$abstract), y)
  expected <- vapply(y, function(d) sprintf("%s_v%s_template_guide.docx", d$id, d$version), character(1))
  expect_setequal(basename(written), expected)
  expect_setequal(names(written), vapply(y, function(d) d$id, character(1)))
  expect_true(all(file.exists(written)))
  expect_identical(
    sort(list.files(dirname(written[[1]])), method = "radix"),
    sort(expected, method = "radix")
  )
})

test_that("a building block has no guide of its own", {
  dir <- withr::local_tempdir()
  tpl <- function(id, label, ...) {
    c(
      "kind: dta_creation_template", paste0("id: ", id), "version: '1.0'",
      paste0("label: ", label), ..., "base:", "  metadata:", "    title: T"
    )
  }
  writeLines(tpl("shared", "Shared", "abstract: true"), file.path(dir, "shared.dta-template.yaml"))
  writeLines(tpl("child", "Child", "extends: shared@1.0"), file.path(dir, "child.dta-template.yaml"))
  out <- withr::local_tempdir()
  expect_error(
    write_template_guide(dir, out, templates = "shared", quiet = TRUE),
    class = "dta_template_guide_abstract"
  )
  expect_error(
    write_template_guide(dir, out, templates = "nope", quiet = TRUE),
    class = "dta_template_guide_unknown"
  )
  expect_length(list.files(out), 0L)
  # By default only the usable template is documented, not its building block.
  # (the child has no datasets, so it cannot be built and is warned about)
  expect_warning(
    got <- write_template_guide(dir, out, quiet = TRUE),
    class = "dta_template_guide_problems"
  )
  expect_identical(names(got), "child")
})

test_that("an existing output aborts before anything is written", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  out <- withr::local_tempdir()
  precious <- file.path(out, "biomarker_gf_acme_v1.0_template_guide.docx")
  writeLines("precious", precious)
  before <- unname(tools::md5sum(precious))
  expect_error(
    write_template_guide(tg_lib(), out, templates = c("biomarker_gf", "biomarker_gf_acme"), quiet = TRUE),
    class = "dta_template_guide_exists"
  )
  expect_identical(list.files(out), basename(precious))
  expect_identical(unname(tools::md5sum(precious)), before)
  got <- write_template_guide(tg_lib(), out,
    templates = c("biomarker_gf", "biomarker_gf_acme"), overwrite = TRUE, quiet = TRUE
  )
  expect_length(list.files(out), 2L)
  expect_identical(basename(got[["biomarker_gf_acme"]]), basename(precious))
})

test_that("a bad format is refused before any work, and a single file selects its template", {
  expect_error(
    write_template_guide(tempfile("missing"), tempfile(), format = "md"),
    class = "dta_template_guide_format"
  )
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  out <- withr::local_tempdir()
  got <- write_template_guide(
    file.path(tg_lib(), "biomarker_gf_acme.dta-template.yaml"), out,
    quiet = TRUE
  )
  expect_identical(names(got), "biomarker_gf_acme")
})

test_that("a PDF made through a re-typesetting backend warns about the lost layout", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  local_mocked_bindings(
    dta_pdf_backend = function() list(name = "tinytex", engine = "", available = "tinytex"),
    .convert_docx_to_pdf = function(docx_file, pdf_file) writeLines("%PDF", pdf_file)
  )
  out <- withr::local_tempdir()
  expect_warning(
    write_template_guide(tg_lib(), out, templates = "biomarker_gf", format = "pdf", quiet = TRUE),
    class = "dta_template_guide_pdf_layout"
  )
  expect_identical(list.files(out), "biomarker_gf_v1.0_template_guide.pdf")
})

# ---- cover, properties, furniture -------------------------------------------

test_that("cover title, properties, header and footer come from the template", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  written <- tg_written()
  for (id in c("biomarker_gf", "biomarker_gf_acme")) {
    y <- tg_yaml(id)
    file <- written[[id]]
    s <- officer::docx_summary(officer::read_docx(file))
    paras <- Filter(nzchar, s$text[s$content_type == "paragraph"])
    expect_identical(paras[[1]], "TEMPLATE GUIDE")
    expect_identical(paras[[2]], y$label)

    props <- officer::doc_properties(officer::read_docx(file))
    expect_identical(props$value[props$tag == "title"], paste0("Template guide: ", y$label))
    expect_identical(props$value[props$tag == "subject"], id)
    expect_identical(props$value[props$tag == "creator"], "Example Pharma")

    headers <- tg_xml_texts(file, "^header[0-9]+\\.xml$")
    expect_gt(length(headers), 0)
    for (h in headers) {
      expect_match(h, "Example Pharma", fixed = TRUE)
      expect_match(h, "Biometrics", fixed = TRUE)
      expect_match(h, "Template guide: ", fixed = TRUE)
      expect_match(h, "Page\\s*PAGE\\s*of\\s*NUMPAGES")
    }
    footers <- tg_xml_texts(file, "^footer[0-9]+\\.xml$")
    expect_gt(length(footers), 0)
    for (f in footers) {
      expect_match(f, id, fixed = TRUE)
      expect_match(f, paste("Template version", y$version), fixed = TRUE)
    }
  }
})

test_that("the organisation comes from the library unless given, and '' shows none", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  lib <- withr::local_tempdir()
  file.copy(list.files(tg_lib(), "\\.ya?ml$", full.names = TRUE), lib)
  writeLines(
    c("kind: dta_app_config", "organisation:", "  name: Library Org", "  unit: Library Unit"),
    file.path(lib, "dta-app.yaml")
  )
  headers_of <- function(...) {
    out <- withr::local_tempdir()
    got <- write_template_guide(lib, out, templates = "biomarker_gf", quiet = TRUE, ...)
    tg_xml_texts(got[[1]], "^header[0-9]+\\.xml$")
  }
  for (h in headers_of()) expect_match(h, "Library Org", fixed = TRUE)
  for (h in headers_of(organisation = "")) {
    expect_false(grepl("Library", h, fixed = TRUE))
    expect_match(h, "Template guide: ", fixed = TRUE)
  }
  for (h in headers_of(organisation = "Explicit Org")) {
    expect_match(h, "Explicit Org", fixed = TRUE)
    expect_false(grepl("Library", h, fixed = TRUE))
  }
})

# ---- layout: the user's complaints, pinned structurally ---------------------

test_that("no narrow or crowded tables, and every table fits its section", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  for (file in tg_written()) {
    tabs <- tg_tables(file)
    expect_gt(length(tabs), 5)
    for (t in tabs) {
      expect_lte(length(t$grid), 5)
      if (!identical(t$orient, "landscape")) expect_lte(length(t$grid), 4)
      expect_true(all(t$grid >= 864))
      expect_lte(sum(t$grid), t$text_w * 1.01)
    }
  }
})

test_that("datasets sit in one landscape section", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  file <- tg_written()[["biomarker_gf_acme"]]
  ex <- tg_unzip(file)
  on.exit(unlink(ex, recursive = TRUE), add = TRUE)
  doc <- readLines(file.path(ex, "word", "document.xml"), warn = FALSE)
  expect_match(paste(doc, collapse = ""), "w:orient=\"landscape\"", fixed = TRUE)
  tabs <- tg_tables(file)
  landscape <- Filter(function(t) identical(t$orient, "landscape"), tabs)
  expect_gt(length(landscape), 0)
  # a column table of 5 columns exists, and only landscape tables have that many
  expect_true(any(vapply(landscape, function(t) length(t$grid) == 5, logical(1))))
  expect_false(any(vapply(Filter(function(t) !identical(t$orient, "landscape"), tabs), function(t) length(t$grid) == 5, logical(1))))
  # exactly one landscape section: portrait, landscape, portrait
  orient <- xml2::xml_attr(xml2::xml_find_all(
    xml2::read_xml(file.path(ex, "word", "document.xml")), "//*[local-name()='sectPr']/*[local-name()='pgSz']"
  ), "orient")
  expect_identical(sum(orient %in% "landscape"), 1L)
})

test_that("every question and every built column is in the document", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  written <- tg_written()
  ds <- yaml::read_yaml(file.path(tg_lib(), "gf_smrnaseq.dta-dataset-template.yaml"))$dataset
  ids <- vapply(ds$columns, function(cl) cl$id, character(1))

  gf <- tg_text(written[["biomarker_gf"]])
  for (o in tg_yaml("biomarker_gf")$options) {
    expect_match(gf, o$label, fixed = TRUE)
  }
  for (id in ids) expect_match(gf, id, fixed = TRUE)

  acme <- tg_text(written[["biomarker_gf_acme"]])
  for (id in c(ids, "ACMEID")) expect_match(acme, id, fixed = TRUE)
})

test_that("changes against the dataset template are marked", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  acme <- tg_text(tg_written()[["biomarker_gf_acme"]])
  expect_match(acme, "Changed by this template", fixed = TRUE)
  expect_match(acme, "Added by this template", fixed = TRUE)
  gf <- tg_text(tg_written()[["biomarker_gf"]])
  expect_false(grepl("by this template", gf, fixed = TRUE))
})

test_that("sections that do not apply are left out, and the list on the cover matches", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  for (id in c("biomarker_gf", "biomarker_gf_acme")) {
    s <- officer::docx_summary(officer::read_docx(tg_written()[[id]]))
    h1 <- s$text[s$style_name %in% "heading 1"]
    paras <- s$text[s$content_type == "paragraph"]
    listed <- sub("^[0-9]+  ", "", paras[grepl("^[0-9]+  \\S", paras)])
    expect_identical(listed, h1)
  }
  gf <- officer::docx_summary(officer::read_docx(tg_written()[["biomarker_gf"]]))
  expect_false(any(grepl("^How it differs", gf$text[gf$style_name %in% "heading 1"])))
})

# ---- stubs and rendering of hand-made entries --------------------------------

test_that("a template that cannot be built still gets a document and a warning", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "kind: dta_creation_template", "id: broken", "version: '1.0'", "label: Broken one",
      "extends: missing_parent@1.0", "base:", "  metadata:", "    title: T"
    ),
    file.path(dir, "broken.dta-template.yaml")
  )
  out <- withr::local_tempdir()
  expect_warning(
    got <- write_template_guide(dir, out, quiet = TRUE),
    class = "dta_template_guide_problems"
  )
  expect_identical(basename(got[["broken"]]), "broken_v1.0_template_guide.docx")
  s <- officer::docx_summary(officer::read_docx(got[["broken"]]))
  expect_true("Overview" %in% s$text[s$style_name %in% "heading 1"])
  expect_true("Technical details" %in% s$text[s$style_name %in% "heading 1"])
})

test_that("long choice lists are cut, and the cut says how many are left", {
  bullet <- paste0(intToUtf8(0x2022), " ")
  expect_identical(.tg_choices("a\nb"), paste0(bullet, c("a", "b"), collapse = "\n"))
  cut <- .tg_choices(paste(letters[1:14], collapse = "\n"))
  lines <- strsplit(cut, "\n", fixed = TRUE)[[1]]
  expect_length(lines, 11L)
  expect_identical(lines[1:10], paste0(bullet, letters[1:10]))
  expect_match(lines[[11]], "4 more", fixed = TRUE)
})

test_that("the file name is sanitised and drops an empty version", {
  expect_identical(.tg_file_name("a b/c", "1.0", "pdf"), "a_b_c_v1.0_template_guide.pdf")
  expect_identical(.tg_file_name("x", "", "docx"), "x_template_guide.docx")
})

test_that("a PDF is a real PDF when a backend exists", {
  skip_if(is.null(dta_pdf_backend()), "no PDF backend on this machine")
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  out <- withr::local_tempdir()
  got <- suppressWarnings(
    write_template_guide(tg_lib(), out, templates = "biomarker_gf", format = "pdf", quiet = TRUE)
  )
  expect_identical(rawToChar(readBin(got[[1]], "raw", 4)), "%PDF")
})

# ---- review fixes: wording, versions, safe writing ----------------------------

# Bundled library plus extra files, in a scratch folder.
tg_scratch <- function(extra = list()) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  file.copy(list.files(tg_lib(), "\\.ya?ml$", full.names = TRUE), dir)
  for (n in names(extra)) writeLines(extra[[n]], file.path(dir, n))
  dir
}

tg_heading_texts <- function(file, style) {
  s <- officer::docx_summary(officer::read_docx(file))
  s$text[s$style_name %in% style]
}

# Cells (text per table) of every table in a docx.
tg_cell_texts <- function(file) {
  ex <- tg_unzip(file)
  on.exit(unlink(ex, recursive = TRUE), add = TRUE)
  x <- xml2::read_xml(file.path(ex, "word", "document.xml"))
  lapply(xml2::xml_find_all(x, "//*[local-name()='tbl']"), function(t) {
    vapply(xml2::xml_find_all(t, ".//*[local-name()='tc']"), function(tc) {
      paste(xml2::xml_text(xml2::xml_find_all(tc, ".//*[local-name()='t']")), collapse = "")
    }, character(1))
  })
}

test_that("a building block parent gets no 'everything else is the same' promise", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  dir <- tg_scratch(list(
    "biomarker_gf.dta-template.yaml" = sub(
      "^id: biomarker_gf$", "id: biomarker_gf\nabstract: true",
      readLines(file.path(tg_lib(), "biomarker_gf.dta-template.yaml")),
      perl = TRUE
    )
  ))
  out <- withr::local_tempdir()
  got <- suppressWarnings(write_template_guide(dir, out, templates = "biomarker_gf_acme", quiet = TRUE))
  txt <- tg_text(got[[1]])
  expect_false(grepl("Everything else is the same", txt, fixed = TRUE))
  expect_match(txt, "is a building block, so this guide does not list differences", fixed = TRUE)
  # The bundled (buildable) parent keeps the promise.
  expect_match(tg_text(tg_written()[["biomarker_gf_acme"]]), "Everything else is the same as in", fixed = TRUE)
})

test_that("a term list is documented once, in the version the template pins", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  v1 <- readLines(file.path(tg_lib(), "gf_tstdtl.dta-vocabulary.yaml"))
  v2 <- sub("^version: .*$", "version: \"2.0\"", v1)
  v2 <- sub("code: FRAGMENT COUNT", "code: NEWER ONLY", v2, fixed = TRUE)
  dir <- tg_scratch(list("gf_tstdtl_2.dta-vocabulary.yaml" = v2))
  out <- withr::local_tempdir()
  got <- write_template_guide(dir, out, templates = "biomarker_gf", quiet = TRUE)
  h2 <- tg_heading_texts(got[[1]], "heading 2")
  expect_identical(sum(h2 == "Genomic measurement detail"), 1L)
  txt <- tg_text(got[[1]])
  expect_match(txt, "FRAGMENT COUNT", fixed = TRUE)
  expect_false(grepl("NEWER ONLY", txt, fixed = TRUE))
})

test_that("a single template file documents that file's version, not the newest", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  old <- readLines(file.path(tg_lib(), "biomarker_gf.dta-template.yaml"))
  newer <- sub("^version: .*$", "version: \"2.0\"", old)
  dir <- tg_scratch(list("biomarker_gf_2.dta-template.yaml" = newer))
  out <- withr::local_tempdir()
  got <- write_template_guide(file.path(dir, "biomarker_gf.dta-template.yaml"), out, quiet = TRUE)
  expect_identical(basename(got[["biomarker_gf"]]), "biomarker_gf_v1.0_template_guide.docx")
  got2 <- write_template_guide(file.path(dir, "biomarker_gf_2.dta-template.yaml"), out, quiet = TRUE)
  expect_identical(basename(got2[["biomarker_gf"]]), "biomarker_gf_v2.0_template_guide.docx")
})

test_that("a failed write removes what this call created and keeps what was there", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  out <- withr::local_tempdir()
  n <- 0L
  local_mocked_bindings(
    .dta_write_staged = function(target, writer) {
      n <<- n + 1L
      if (n == 2L) cli::cli_abort("disk full", class = "tg_test_disk")
      writeLines("x", target)
    }
  )
  expect_error(
    write_template_guide(tg_lib(), out, templates = c("biomarker_gf", "biomarker_gf_acme"), quiet = TRUE),
    class = "tg_test_disk"
  )
  expect_length(list.files(out), 0L)
})

test_that("a failed write leaves a file that already existed alone", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  out <- withr::local_tempdir()
  keep <- file.path(out, "biomarker_gf_v1.0_template_guide.docx")
  writeLines("old", keep)
  local_mocked_bindings(.dta_write_staged = function(target, writer) cli::cli_abort("no", class = "tg_test_disk"))
  expect_error(
    write_template_guide(tg_lib(), out, templates = c("biomarker_gf", "biomarker_gf_acme"), overwrite = TRUE, quiet = TRUE),
    class = "tg_test_disk"
  )
  expect_identical(list.files(out), basename(keep))
  expect_identical(readLines(keep), "old")
})

test_that("file names that differ only in case collide", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  mk <- function(id) {
    c(
      "kind: dta_creation_template", paste0("id: ", id), "version: '1.0'", "label: Case",
      "datasets:", "  - id: tiny", "    name: Tiny", "    type: tabular",
      "    files: {filename: t.csv, type: csv}",
      "    columns:", "      - {id: AA, type: SAS Char}", "    rules: []"
    )
  }
  dir <- withr::local_tempdir()
  writeLines(mk("Case_tpl"), file.path(dir, "a.dta-template.yaml"))
  writeLines(mk("case_tpl"), file.path(dir, "b.dta-template.yaml"))
  out <- withr::local_tempdir()
  expect_error(write_template_guide(dir, out, quiet = TRUE), class = "dta_template_guide_exists")
  expect_length(list.files(out), 0L)
})

test_that("organisation arguments are validated like write_dta, and '' still means none", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  out <- withr::local_tempdir()
  expect_error(
    write_template_guide(tg_lib(), out, templates = "biomarker_gf", organisation = c("a", "b"), quiet = TRUE),
    "organisation"
  )
  expect_error(
    write_template_guide(tg_lib(), out, templates = "biomarker_gf", organisation_unit = 3, quiet = TRUE),
    "organisation_unit"
  )
  expect_length(list.files(out), 0L)
  got <- write_template_guide(tg_lib(), out, templates = "biomarker_gf", organisation = "", quiet = TRUE)
  props <- officer::doc_properties(officer::read_docx(got[[1]]))
  expect_identical(props$value[props$tag == "creator"], "DTAtools")
})

test_that("questions, filled-in fields and the glossary use the reviewed wording", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  gf <- tg_text(tg_written()[["biomarker_gf"]])
  # No question of biomarker_gf is required (checked in the YAML).
  expect_null(tg_yaml("biomarker_gf")$required)
  expect_match(gf, "All questions are optional; each starts with its pre-filled answer where one is shown.", fixed = TRUE)
  expect_match(gf, "Your answers to the questions in section", fixed = TRUE)
  expect_match(gf, "Version history (first entry)", fixed = TRUE)
  expect_false(grepl("Version history 1", gf, fixed = TRUE))
  expect_match(gf, "Supplier: organisation and contacts", fixed = TRUE)
  expect_match(gf, "Document header (organisation name)", fixed = TRUE)
  expect_false(grepl("can be changed by question", gf, fixed = TRUE))
  # Glossary entries for columns and rules; a cover pointer with quoted terms.
  expect_match(gf, "How the value is stored", fixed = TRUE)
  expect_match(gf, "A check the data must pass", fixed = TRUE)
  expect_match(gf, "Terms such as \"Pre-filled answer\" are explained in section", fixed = TRUE)
  # Suggestions and bullets.
  expect_match(gf, "Suggestions", fixed = TRUE)
  expect_match(gf, paste0(intToUtf8(0x2022), " "), fixed = TRUE)
})

test_that("a required question reads 'Must be answered' in the intro and the card", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  dir <- withr::local_tempdir()
  writeLines(c(
    "kind: dta_creation_template", "id: req_tpl", "version: '1.0'", "label: Required one",
    "base:", "  metadata:", "    title: T",
    "required: [base.metadata.header]",
    "options:",
    "  - {id: header, label: Header, type: text, target: metadata.header, default: Acme}",
    "datasets:", "  - id: tiny", "    name: Tiny", "    type: tabular",
    "    files: {filename: t.csv, type: csv}",
    "    columns:", "      - {id: AA, type: SAS Char}", "    rules: []"
  ), file.path(dir, "req.dta-template.yaml"))
  out <- withr::local_tempdir()
  got <- write_template_guide(dir, out, quiet = TRUE)
  txt <- tg_text(got[[1]])
  expect_match(txt, "Questions marked \"Must be answered\" cannot be left empty; all others are optional.", fixed = TRUE)
  expect_match(txt, "Must be answered", fixed = TRUE)
  expect_false(grepl("All questions are optional", txt, fixed = TRUE))
})

test_that("small tables stay on one page, and table widths follow the review", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  file <- tg_written()[["biomarker_gf"]]
  ex <- tg_unzip(file)
  on.exit(unlink(ex, recursive = TRUE), add = TRUE)
  x <- xml2::read_xml(file.path(ex, "word", "document.xml"))
  tbls <- xml2::xml_find_all(x, "//*[local-name()='tbl']")
  txt <- vapply(tbls, function(t) paste(xml2::xml_text(xml2::xml_find_all(t, ".//*[local-name()='t']")), collapse = " "), character(1))
  grid <- function(t) as.numeric(xml2::xml_attr(xml2::xml_find_all(t, "./*[local-name()='tblGrid']/*[local-name()='gridCol']"), "w"))
  # Rules table (<= 15 rows): every row but the last keeps with the next one.
  rules <- tbls[[which(grepl("What it checks", txt, fixed = TRUE))[[1]]]]
  rows <- xml2::xml_find_all(rules, "./*[local-name()='tr']")
  expect_lte(length(rows) - 1L, 15L)
  keep <- vapply(rows, function(r) length(xml2::xml_find_all(r, ".//*[local-name()='keepNext']")) > 0L, logical(1))
  expect_true(all(keep[-length(keep)]))
  expect_false(keep[[length(keep)]])
  g <- grid(rules)
  expect_equal(g[[1]] / g[[2]], 2.0 / 7.7, tolerance = 0.02)
  # Supplier and receiver: the repeated column is gone, three columns remain.
  par <- tbls[[which(grepl("Profiles you can choose", txt, fixed = TRUE))[[1]]]]
  expect_length(grid(par), 3L)
  # Facts of a dataset no longer repeat the counts the headings carry.
  cells <- unlist(tg_cell_texts(file))
  expect_false(any(cells %in% c("Columns", "Rules")))
})

test_that("contact widths follow the review", {
  skip_if(!nzchar(tg_lib()), "bundled templates not installed")
  file <- tg_written()[["biomarker_gf"]]
  ex <- tg_unzip(file)
  on.exit(unlink(ex, recursive = TRUE), add = TRUE)
  x <- xml2::read_xml(file.path(ex, "word", "document.xml"))
  tbls <- xml2::xml_find_all(x, "//*[local-name()='tbl']")
  txt <- vapply(tbls, function(t) paste(xml2::xml_text(xml2::xml_find_all(t, ".//*[local-name()='t']")), collapse = " "), character(1))
  ct <- tbls[[which(grepl("Signs the DTA", txt, fixed = TRUE))[[1]]]]
  g <- as.numeric(xml2::xml_attr(xml2::xml_find_all(ct, "./*[local-name()='tblGrid']/*[local-name()='gridCol']"), "w"))
  expect_equal(g / sum(g), c(1.6, 1.6, 2.2, 0.9) / 6.3, tolerance = 0.02)
})
