# How load_delivery() chooses the file entry for a file, what a failure leaves
# behind, and how the specification and its exports describe a container.
# REQ-CONT-002, REQ-CONT-006, REQ-CONT-009, REQ-CONT-012, REQ-CONT-014 and
# REQ-CONT-015.
#
# Every expected value is written down from the specification, from the
# literal data of the fixtures (helper-container.R, built by
# .github/scripts/build_container_fixtures.R) or from the bundled example
# specification, never captured from a run:
#
#   sdtm_csv.zip   ae.csv, dm.csv, define.xml, docs/readme.txt and two system files
#   encrypted.zip  ae.csv, flagged as encrypted
#   nested.zip     inner.zip
#   lib_in_zip.zip sdtm.xpt (a library of AE, DM and VS) and define.xml
#
# Archives that must differ from a fixture (a name in capitals, two folders
# holding the same file name) are built at run time.
#
# LIM-016 (files are matched by base name) is pinned in OQ-CONT-016.

# ---- local helpers --------------------------------------------------------------

# The reason a file gets when a container-declaring entry would take it, but it
# arrived outside that container.
RT_REASON <- "matches the file entry of dataset 'ae', which expects it inside sdtm_csv.zip"

# A dataset ae reading a SAS dataset (which the archive's text is not) and a
# dataset dm reading a CSV.
rt_sas_and_csv <- function() {
  cont_of(
    cont_tab("ae", list(DTAtools::DTAFileSAS7BDAT("ae.sas7bdat")), cont_empty_specs()),
    cont_tab("dm", list(DTAtools::DTAFileCSV("dm.csv")), cont_specs("dm"))
  )
}

# Lint rows of a one-dataset document whose files entry is `files`, with extra
# keys on the dataset itself.
rt_lint <- function(files, dataset = list()) {
  lint_dta(list(datasets = list(c(list(id = "ae", type = "tabular"), dataset, list(files = files)))))
}

# A one-dataset YAML document with a chosen `container:` line on its file entry.
rt_yaml <- function(container_line, env = parent.frame()) {
  p <- file.path(qa_tempdir(env), "doc.yaml")
  writeLines(
    c(
      "metadata:", "  title: T", "  version: '1'",
      "datasets:",
      "  - id: ae", "    type: tabular", "    files:",
      "      type: csv", "      filename: ae.csv",
      paste0("      ", container_line)
    ),
    p
  )
  p
}

# ---- containers -------------------------------------------------------------------

test_that("OQ-CONT-013 | a declared container accepts only its own members, with wildcards, at any level of nesting | REQ-CONT-006", {
  declared <- cont_dta("csv", container = "sdtm_csv.zip")
  loose_ae <- cont_write("ae.csv", CONT_AE_CSV)

  # A loose file of the right name is not taken by an entry that declares a
  # container, and the reason says where the entry expects it.
  l <- cont_run(declared, loose_ae, exdir = cont_exdir("OQ-CONT-013"))
  qa_step("a loose ae.csv is refused by an entry that expects it inside an archive", TRUE, cont_is(l$error, "dta_delivery_nothing_loaded"))
  qa_check("and its reason names the entry and the container", any(grepl(RT_REASON, l$plan$reason, fixed = TRUE)), l$plan$reason)
  z <- cont_run(declared, cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-013"))
  qa_step("the same file from inside the named archive is loaded", c("ae", "dm"), sort(cont_rows(z$plan, "loaded")$dataset))

  # Both in one call, in either order: the member loads, the loose file stays
  # unused.
  for (files in list(c(loose_ae, cont_fx("sdtm_csv.zip")), c(cont_fx("sdtm_csv.zip"), loose_ae))) {
    r <- cont_run(declared, files, exdir = cont_exdir("OQ-CONT-013"))
    loaded <- cont_rows(r$plan, "loaded")
    qa_step("one call: ae and dm are loaded", c("ae", "dm"), sort(loaded$dataset))
    qa_step("both recording the archive as their container", rep("sdtm_csv.zip", 2), loaded$container)
    loose_row <- r$plan[is.na(r$plan$container) & r$plan$name == "ae.csv", ]
    qa_step(
      "one call: the loose ae.csv is unused, with the same reason",
      list(rows = 1L, status = "unused", reason = TRUE),
      list(rows = nrow(loose_row), status = loose_row$status, reason = grepl(RT_REASON, loose_row$reason, fixed = TRUE))
    )
  }

  # An entry without a container accepts the file loose or inside any container.
  plain <- cont_run(cont_dta("csv"), c(loose_ae, cont_write("dm.csv", CONT_DM_CSV)), exdir = cont_exdir("OQ-CONT-013"))
  qa_step(
    "an entry without a container takes loose files",
    list(dataset = c("ae", "dm"), container = c(NA_character_, NA_character_)),
    list(dataset = sort(cont_rows(plain$plan, "loaded")$dataset), container = cont_rows(plain$plan, "loaded")$container)
  )
  inside <- cont_run(cont_dta("csv"), cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-013"))
  qa_step("and the same files from inside an archive", c("ae", "dm"), sort(cont_rows(inside$plan, "loaded")$dataset))

  # Another container's member is not taken, and a name differing only in case
  # is explained.
  o <- cont_run(cont_dta("csv", container = "other.zip"), cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-013"))
  qa_step("a member of another archive is not taken", TRUE, cont_is(o$error, "dta_delivery_nothing_loaded"))
  qa_check("and nothing mentions letter case", !any(grepl("letter case", o$plan$reason, fixed = TRUE)), o$plan$reason)
  shouting <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "SDTM_CSV.ZIP")
  s <- cont_run(cont_dta("csv", container = "sdtm_csv.zip"), shouting, exdir = cont_exdir("OQ-CONT-013"))
  qa_step("an archive named in capitals does not match a lower-case declaration", TRUE, cont_is(s$error, "dta_delivery_nothing_loaded"))
  reason <- s$plan$reason[s$plan$name == "ae.csv"]
  qa_step("and its member's reason says letter case must match", TRUE, length(reason) == 1L && endsWith(reason, "(letter case must match)"))

  # * matches any text and ? one character, and the whole name must match.
  w <- cont_run(cont_dta("csv", container = "sdtm_*.zip"), cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-013"))
  qa_step("sdtm_*.zip matches sdtm_csv.zip", c("ae", "dm"), sort(cont_rows(w$plan, "loaded")$dataset))
  q <- cont_run(cont_dta("csv", container = "sdtm_????.zip"), cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-013"))
  qa_step("sdtm_????.zip, whose four characters cannot be 'csv', does not", TRUE, cont_is(q$error, "dta_delivery_nothing_loaded"))

  # An outer container declaration reaches a library's members: routing matches
  # every container of the chain, not only the innermost.
  n <- cont_run(cont_dta("xpt", container = "lib_in_zip.zip"), cont_fx("lib_in_zip.zip"), exdir = cont_exdir("OQ-CONT-013"))
  qa_step(
    "a declaration of the outer archive reaches the members of the library inside it",
    c("ae.xpt", "dm.xpt", "vs.xpt"),
    sort(cont_rows(n$plan, "loaded")$name)
  )
})

test_that("OQ-CONT-014 | a literal entry wins over pattern entries, the first pattern entry over later ones, and a pattern judges a gzip file without its .gz | REQ-CONT-002", {
  # clinical_data matches clinical_data.csv by a pattern, raw_export names
  # clinical_data2.csv.gz literally (inst/extdata/clinical_dta_with_file_dataset.yaml).
  dta <- suppressWarnings(read_dta_from_yaml(
    system.file("extdata", "clinical_dta_with_file_dataset.yaml", package = "DTAtools")
  ))
  files <- system.file("extdata", c("clinical_data.csv", "clinical_data2.csv.gz"), package = "DTAtools")
  r <- cont_run(dta, files, exdir = cont_exdir("OQ-CONT-014"))
  qa_check("the delivery is accepted", is.null(r$error), r$error)
  qa_step("the pattern entry's dataset receives clinical_data.csv", "clinical_data", names(tables(datasets(r$result, "clinical_data"))))
  qa_step(
    "the literal entry of another dataset takes clinical_data2.csv.gz, which the pattern would also match",
    "clinical_data2.csv.gz",
    basename(datasets(r$result, "raw_export")@file_paths)
  )

  # Within a dataset: a literal entry before a pattern entry, and the first of
  # two pattern entries.
  lit <- cont_of(cont_tab("ae", list(
    DTAtools::DTAFileCSV("^a.*\\.csv$", pattern = TRUE), DTAtools::DTAFileCSV("ae.csv")
  )))
  a <- cont_run(lit, cont_write("ae.csv", CONT_AE_CSV), exdir = cont_exdir("OQ-CONT-014"))
  qa_step("the literal entry (the second) wins over a pattern entry of the same dataset", 2L, cont_rows(a$plan, "loaded")$handler)
  pat <- cont_of(cont_tab("ae", list(
    DTAtools::DTAFileCSV("^a.*\\.csv$", pattern = TRUE), DTAtools::DTAFileCSV("^ae.*\\.csv$", pattern = TRUE)
  )))
  b <- cont_run(pat, cont_write("ae.csv", CONT_AE_CSV), exdir = cont_exdir("OQ-CONT-014"))
  qa_step("the first of two matching pattern entries wins", 1L, cont_rows(b$plan, "loaded")$handler)

  # A pattern entry judges a gzip file without its compression suffix, loose
  # or inside an archive.
  gz_entry <- cont_of(cont_tab("ae", list(DTAtools::DTAFileCSV("^ae_.*\\.csv$", pattern = TRUE)), cont_empty_specs()))
  g <- cont_gz(CONT_AE_CSV, "ae_2025.csv.gz")
  loose <- cont_run(gz_entry, g, exdir = cont_exdir("OQ-CONT-014"))
  qa_step(
    "a loose ae_2025.csv.gz is taken by the pattern ^ae_.*\\.csv$",
    list(rows = 1L, name = "ae_2025.csv.gz", item = "ae_2025"),
    list(rows = nrow(cont_rows(loose$plan, "loaded")), name = cont_rows(loose$plan, "loaded")$name, item = cont_rows(loose$plan, "loaded")$item)
  )
  zipped <- cont_run(gz_entry, cont_zip(list(ae_2025.csv.gz = readBin(g, "raw", file.size(g)))), exdir = cont_exdir("OQ-CONT-014"))
  qa_step(
    "so is the same file inside an archive, which is then its container",
    list(rows = 1L, name = "ae_2025.csv.gz", item = "ae_2025", container = "test.zip"),
    list(
      rows = nrow(cont_rows(zipped$plan, "loaded")), name = cont_rows(zipped$plan, "loaded")$name,
      item = cont_rows(zipped$plan, "loaded")$item, container = cont_rows(zipped$plan, "loaded")$container
    )
  )
})

test_that("OQ-CONT-015 | abort leaves no trace, skip loads the rest, nothing loaded is an error | REQ-CONT-012", {
  # A member that is not a SAS dataset, and a good CSV beside it.
  z <- cont_zip(list(ae.sas7bdat = "not a SAS dataset\n", dm.csv = CONT_DM_CSV))
  dta0 <- rt_sas_and_csv()

  ex <- cont_exdir("OQ-CONT-015")
  a <- cont_run(dta0, z, exdir = ex)
  qa_step("abort mode (the default): the failure aborts as a load error", TRUE, cont_is(a$error, "dta_delivery_load_error"))
  qa_step(
    "naming the dataset and carrying the reader's own refusal",
    list(dataset = "ae", cause = TRUE),
    list(dataset = a$error$dataset, cause = cont_is(a$error$parent, "dta_sas7bdat_read_error"))
  )
  qa_step("no table is bound", 0L, length(tables(datasets(dta0, "dm"))))
  qa_step("and no extracted file is left behind", character(0), list.files(ex, recursive = TRUE))

  s <- cont_run(dta0, z, on_error = "skip", exdir = cont_exdir("OQ-CONT-015"))
  qa_check("skip mode: the delivery is accepted", is.null(s$error), s$error)
  qa_step("one warning of class dta_delivery_refused is signalled", 1L, s$n_warnings)
  qa_step("every other file is bound", c(34, 51), cont_table(s$result, "dm", "dm")$AGE, tolerance = 0)
  row <- s$plan[s$plan$name == "ae.sas7bdat", ]
  qa_step("the failure is listed as refused", "refused", row$status)
  paths <- c(tempdir(), normalizePath(tempdir(), winslash = "/"), normalizePath(tempdir(), winslash = "\\"))
  qa_step(
    "and its reason names no server path",
    rep(FALSE, 3),
    vapply(paths, function(p) grepl(p, row$reason, fixed = TRUE), logical(1), USE.NAMES = FALSE)
  )

  # Nothing that can be bound: refused in both modes.
  unrelated <- cont_write("unrelated.txt", "x")
  for (mode in c("abort", "skip")) {
    n <- cont_run(cont_dta("csv"), unrelated, on_error = mode, exdir = cont_exdir("OQ-CONT-015"))
    qa_step(
      sprintf("%s mode: a delivery from which nothing can be bound is refused", mode),
      list(refused = TRUE, reason = "matches no file entry of the specification"),
      list(refused = cont_is(n$error, "dta_delivery_nothing_loaded"), reason = n$plan$reason)
    )
  }

  # A container that fails to open: when an entry also binds it as a file the
  # file loads and the container is `opened` with the reason; nothing is
  # refused, and the files its entries declared are not reported missing.
  docs_ae <- cont_of(
    cont_filedata("docs", DTAtools::DTAFileAny(".*", pattern = TRUE, min_number_of_files = 0, max_number_of_files = 10)),
    cont_tab("ae", list(DTAtools::DTAFileCSV("ae.csv")))
  )
  e <- cont_run(docs_ae, cont_fx("encrypted.zip"), on_error = "skip", exdir = cont_exdir("OQ-CONT-015"))
  qa_step("an encrypted archive that an entry binds as a file raises no warning", 0L, e$n_warnings)
  qa_step("it is loaded as a file and listed as opened", c("loaded", "opened"), sort(e$plan$status))
  qa_check(
    "the opened row says it could not be opened",
    startsWith(e$plan$reason[e$plan$status == "opened"], "could not be opened: ")
  )
  declared <- cont_of(
    cont_tab("ae", list(DTAtools::DTAFileCSV("ae.csv", container = "encrypted.zip"))),
    cont_tab("dm", list(DTAtools::DTAFileCSV("dm.csv")))
  )
  files <- c(cont_fx("encrypted.zip"), cont_zip(list(dm.csv = CONT_DM_CSV), name = "other.zip"))
  m <- cont_run(declared, files, on_error = "skip", exdir = cont_exdir("OQ-CONT-015"))
  qa_step("a refused container leaves no declared file 'not in the delivery'", 0L, length(m$summary$missing))
  qa_step("and its refusal is listed", "refused", m$plan$status[m$plan$name == "encrypted.zip"])
})

test_that("OQ-CONT-016 | two files of one delivery that would be the same table are refused | REQ-CONT-009", {
  # LIM-016: folders are not part of matching, so ae.csv of two folders are two
  # files for the one entry that names ae.csv.
  folders <- cont_zip(list("a/ae.csv" = CONT_AE_CSV, "b/ae.csv" = CONT_AE_CSV))
  a <- cont_run(cont_dta("csv"), folders, exdir = cont_exdir("OQ-CONT-016"))
  qa_step(
    "LIM-016: the same file name in two folders is refused in abort mode",
    list(class = TRUE, dataset = "ae", item = "ae"),
    list(class = cont_is(a$error, "dta_delivery_duplicate_name"), dataset = a$error$dataset, item = a$error$item)
  )
  loose <- cont_write("ae.csv", CONT_AE_CSV)
  b <- cont_run(cont_dta("csv"), c(loose, cont_fx("sdtm_csv.zip")), exdir = cont_exdir("OQ-CONT-016"))
  qa_step("a loose ae.csv and the one inside an archive clash the same way", TRUE, cont_is(b$error, "dta_delivery_duplicate_name"))

  files <- c(folders, cont_write("dm.csv", CONT_DM_CSV))
  s <- cont_run(cont_dta("csv"), files, on_error = "skip", exdir = cont_exdir("OQ-CONT-016"))
  qa_check("skip mode: the delivery is accepted", is.null(s$error), s$error)
  qa_step("one warning of class dta_delivery_refused is signalled", 1L, s$n_warnings)
  qa_step("the rest is loaded: dm has its table", 1L, cont_ntab(s$result, "dm"))
  qa_step("and ae has none", 0L, cont_ntab(s$result, "ae"))
  refused <- cont_rows(s$plan, "refused")
  qa_step(
    "LIM-016: both clashing files are refused, naming the item and the dataset",
    list(
      names = c("ae.csv", "ae.csv"),
      reason = rep("is one of several files of the delivery that would be loaded as 'ae' into dataset 'ae'", 2)
    ),
    list(names = refused$name, reason = refused$reason)
  )

  # Equal names across the paths of one delivery are one file when they are the
  # same path: the same archive given twice is not a clash.
  twice <- cont_run(cont_dta("csv"), c(cont_fx("sdtm_csv.zip"), cont_fx("sdtm_csv.zip")), exdir = cont_exdir("OQ-CONT-016"))
  qa_step("the same archive given twice loads once", list(error = FALSE, loaded = c("ae", "dm")), list(
    error = !is.null(twice$error),
    loaded = sort(cont_rows(twice$plan, "loaded")$dataset)
  ))
})

# ---- the specification --------------------------------------------------------------

test_that("OQ-CONT-017 | lint and YAML reading refuse an invalid or misplaced container and an unknown file key | REQ-CONT-015", {
  bad <- rt_lint(list(type = "csv", filename = "ae.csv", container = "sdtm.tar"))
  qa_step(
    "a container that is not a .zip or .xpt file name is one error finding naming the entry",
    list(
      rows = 1L, code = "file_container_invalid", severity = "error", dataset = "ae",
      object = "file", id = "ae.csv",
      message = "The container 'sdtm.tar' must name a .zip archive or a .xpt SAS transport file."
    ),
    list(
      rows = nrow(bad), code = bad$code, severity = bad$severity, dataset = bad$dataset,
      object = bad$object, id = bad$id, message = bad$message
    )
  )
  ok <- rt_lint(list(
    list(type = "csv", filename = "ae.csv", container = "sdtm_*.zip"),
    list(type = "csv", filename = "dm.csv")
  ))
  qa_step("a wildcard container and an entry without one produce no file finding", FALSE, any(startsWith(ok$code, "file_")))

  typo <- rt_lint(list(type = "csv", filename = "ae.csv", contianer = "sdtm.zip"))
  qa_step(
    "a misspelt file key is one error finding that names the key",
    list(rows = 1L, code = "file_field_unknown", severity = "error", message = "Unknown file field(s): 'contianer'."),
    list(rows = nrow(typo), code = typo$code, severity = typo$severity, message = typo$message)
  )
  misplaced <- rt_lint(list(type = "csv", filename = "ae.csv"), dataset = list(container = "sdtm.zip"))
  qa_step(
    "a container written on the dataset is one error finding that points to the file entry",
    list(
      rows = 1L, code = "file_container_misplaced", severity = "error",
      message = "'container' belongs to a file entry: write it under 'files:'."
    ),
    list(rows = nrow(misplaced), code = misplaced$code, severity = misplaced$severity, message = misplaced$message)
  )

  # read_dta_from_yaml() refuses the same documents.
  p <- rt_yaml("container: deliveries/sdtm.zip")
  e <- cont_catch(read_dta_from_yaml(p))
  qa_step("a container with a folder in it is refused when the YAML is read", TRUE, cont_is(e, "dta_lint_error"))
  b <- cont_catch(read_dta_from_yaml(rt_yaml("container: \"{1+1}.tar\"")))
  qa_step(
    "a refused value is shown as written, with its braces",
    list(class = TRUE, braces = TRUE),
    list(class = cont_is(b, "dta_lint_error"), braces = !is.null(b) && grepl("{1+1}", conditionMessage(b), fixed = TRUE))
  )
  good <- read_dta_from_yaml(rt_yaml("container: sdtm.zip"))
  qa_step(
    "a valid container is carried into the file entry",
    "sdtm.zip",
    datasets(good, "ae")@files[[1]]@container
  )
})

test_that("OQ-CONT-018 | the exported Files table shows Delivered In only when declared | REQ-CONT-014", {
  fixed_cells <- function(line) trimws(strsplit(sub("\\|\\s*$", "", sub("^\\|", "", line)), "|", fixed = TRUE)[[1]])
  export <- function(files) {
    ds <- DTAtools::DTADataSetTabular(id = "ae", specs = cont_specs("ae"), files = files)
    out <- file.path(qa_tempdir(), "ae.md")
    write_dataset_metadata(ds, out, format = "md", quiet = TRUE)
    readLines(out, warn = FALSE, encoding = "UTF-8")
  }
  header_of <- function(lines) lines[startsWith(lines, "| File Name / Pattern")]

  declared <- export(list(
    DTAtools::DTAFileCSV("ae.csv", container = "sdtm.zip"),
    DTAtools::DTAFileCSV("dm.csv")
  ))
  head <- header_of(declared)
  qa_step("with a declared container the Files table has one header line", 1L, length(head))
  qa_check("which ends in the column Delivered In", endsWith(head, "| Delivered In |"), head)
  cells <- fixed_cells(head)
  rows <- declared[startsWith(declared, "| ae.csv") | startsWith(declared, "| dm.csv")]
  qa_step(
    "the entry that declares a container shows it under that column",
    "sdtm.zip",
    fixed_cells(rows[startsWith(rows, "| ae.csv")][[1]])[length(cells)]
  )
  qa_step(
    "and an entry without one leaves the cell empty",
    "",
    fixed_cells(rows[startsWith(rows, "| dm.csv")][[1]])[length(cells)]
  )

  plain <- export(list(DTAtools::DTAFileCSV("ae.csv")))
  head_plain <- header_of(plain)
  qa_step("without any container the Files table has one header line", 1L, length(head_plain))
  qa_step("which has no Delivered In column", FALSE, grepl("Delivered In", head_plain, fixed = TRUE))
  qa_step(
    "and is the four-column table (no entry has a description) the export always had",
    "| File Name / Pattern | Match Type | Format | Expected Count |",
    head_plain
  )

  # Markdown leaves out a column that is empty throughout, so the Word export
  # is where the five-column table is told from the six-column one.
  word_files <- function(files) {
    ds <- DTAtools::DTADataSetTabular(id = "ae", specs = cont_specs("ae"), files = files)
    out <- file.path(qa_tempdir(), "ae.docx")
    write_dataset_metadata(ds, out, quiet = TRUE)
    tabs <- Filter(function(t) nrow(t) > 0 && identical(t$col1[[1]], "File Name / Pattern"), qa_docx_tables(out))
    if (length(tabs) == 0) NULL else tabs[[1]]
  }
  w_declared <- word_files(list(DTAtools::DTAFileCSV("ae.csv", container = "sdtm.zip"), DTAtools::DTAFileCSV("dm.csv")))
  qa_step(
    "Word: with a declared container the Files table has six columns, the last headed Delivered In",
    list(columns = 6L, last = "Delivered In"),
    list(columns = ncol(w_declared), last = unname(unlist(w_declared[1, ncol(w_declared)])))
  )
  qa_step("Word: and shows the container in the row of the entry that declares it", "sdtm.zip", unname(unlist(w_declared[2, 6])))
  w_plain <- word_files(list(DTAtools::DTAFileCSV("ae.csv"), DTAtools::DTAFileCSV("dm.csv")))
  qa_step(
    "Word: without any container the Files table keeps its five columns",
    list(columns = 5L, delivered_in = FALSE),
    list(columns = ncol(w_plain), delivered_in = "Delivered In" %in% unlist(w_plain[1, ]))
  )
})

# ---- a literal entry outranks pattern entries, container or not -----------------

test_that("OQ-CONT-028 | a file a literal entry names, but expects inside another container, is taken by no pattern entry | REQ-CONT-002 REQ-CONT-006", {
  # Dataset ae names ae.csv literally but only inside sdtm.zip; dataset listings
  # takes every .csv by pattern. The loose ae.csv is named by a literal entry, so
  # no pattern entry of another dataset may take it; lb.csv, named by no literal
  # entry, goes to listings.
  dta <- cont_of(
    cont_tab("ae", list(DTAtools::DTAFileCSV("ae.csv", container = "sdtm.zip")), cont_empty_specs()),
    cont_tab("listings", list(DTAtools::DTAFileCSV("[.]csv$", pattern = TRUE)), cont_empty_specs())
  )
  loose <- c(cont_write("ae.csv", CONT_AE_CSV), cont_write("lb.csv", "A,B\n1,2\n"))
  r <- cont_run(dta, loose, exdir = cont_exdir("OQ-CONT-028"))
  qa_check("the delivery is accepted, as lb.csv can be bound", is.null(r$error), r$error)
  loaded <- cont_rows(r$plan, "loaded")
  qa_step("only lb.csv is loaded, into dataset listings", list(name = "lb.csv", dataset = "listings"), list(name = loaded$name, dataset = loaded$dataset))
  qa_step("dataset listings holds the one table lb", "lb", names(DTAtools::tables(DTAtools::datasets(r$result, "listings"))))
  unused <- cont_rows(r$plan, "unused")
  qa_step("ae.csv is the one unused file", "ae.csv", unused$name)
  qa_step("with the reason that dataset ae expects it inside sdtm.zip", "matches the file entry of dataset 'ae', which expects it inside sdtm.zip", unused$reason)
  qa_step("and no dataset took it", NA_character_, unused$dataset)
  qa_step("dataset ae holds no table", 0L, cont_ntab(r$result, "ae"))

  # Control: the same name inside the archive it expects is taken by ae, and the
  # pattern entry still takes the member of another name.
  z <- cont_run(dta, cont_zip(list(ae.csv = CONT_AE_CSV, lb.csv = "A,B\n1,2\n"), name = "sdtm.zip"), exdir = cont_exdir("OQ-CONT-028"))
  zl <- cont_rows(z$plan, "loaded")
  qa_step(
    "inside sdtm.zip, ae.csv goes to ae and lb.csv to listings",
    list(ae = "ae.csv", listings = "lb.csv"),
    list(ae = zl$name[zl$dataset == "ae"], listings = zl$name[zl$dataset == "listings"])
  )

  # A pattern entry that declares the container the file did come from keeps
  # it: sdtm_docs names define.xml literally inside sdtm.zip, adam_docs takes
  # every .xml inside adam.zip, and each archive holds a define.xml.
  docs <- cont_of(
    cont_filedata("sdtm_docs", DTAtools::DTAFileAny("define.xml", container = "sdtm.zip")),
    cont_filedata("adam_docs", DTAtools::DTAFileAny("[.]xml$", pattern = TRUE, container = "adam.zip"))
  )
  both <- c(
    cont_zip(list(define.xml = "<sdtm/>"), name = "sdtm.zip"),
    cont_zip(list(define.xml = "<adam/>"), name = "adam.zip")
  )
  d <- cont_run(docs, both, exdir = cont_exdir("OQ-CONT-028"))
  dl <- cont_rows(d$plan, "loaded")
  qa_step(
    "each define.xml goes to the dataset whose entry names its archive",
    c("adam_docs", "sdtm_docs"),
    sort(dl$dataset[dl$name == "define.xml"], method = "radix")
  )
  qa_step("no file is left unused", 0L, nrow(cont_rows(d$plan, "unused")))
})

# ---- a refused file was delivered ------------------------------------------------

test_that("OQ-CONT-029 | a file that was delivered but refused is not reported missing and keeps its dataset and entry | REQ-CONT-006 REQ-CONT-012", {
  # Three entries each expect their file inside sdtm_csv.zip. The archive holds
  # ae.csv (53 bytes) and dm.csv (30 bytes) and no vs.csv. With a member limit of
  # 40 bytes ae.csv is refused and dm.csv loads.
  entries <- function(id) cont_tab(id, list(DTAtools::DTAFileCSV(paste0(id, ".csv"), container = "sdtm_csv.zip")), cont_specs(id))
  dta <- cont_of(entries("ae"), entries("dm"), entries("vs"))
  r <- withr::with_options(
    list(DTAtools.container_max_member_bytes = 40),
    cont_run(dta, cont_fx("sdtm_csv.zip"), on_error = "skip", exdir = cont_exdir("OQ-CONT-029"))
  )
  qa_check("skip mode: the delivery is accepted", is.null(r$error), r$error)
  ae <- r$plan[r$plan$name == "ae.csv", ]
  qa_step("ae.csv is listed as refused", "refused", ae$status)
  qa_step("with the dataset and the entry (the first) it was meant for", list(dataset = "ae", handler = 1L), list(dataset = ae$dataset, handler = ae$handler))
  qa_step("dm.csv is loaded", "dm", cont_rows(r$plan, "loaded")$dataset)
  qa_step(
    "only vs.csv, which the archive does not hold, is reported as not in the delivery",
    "vs.csv", r$summary$missing
  )
})
