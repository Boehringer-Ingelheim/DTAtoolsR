# The document-level container model: DTAContainer, DTA@containers, references,
# container_assignments(), the YAML section, lint, provenance, the load_file()
# guard, upgrade_dta() and file-name conflicts. REQ-CONT-019, REQ-CONT-024,
# REQ-CONT-026, REQ-CONT-028, REQ-CONT-029 and REQ-CONT-032.
#
# Every expected value is written down from the requirement text and from the
# container-manager plan (docs of the sections named in each requirement's
# source), never captured from a run. Fixtures are built here or are the
# archives of fx/container/ (helper-container.R).
#
# LIM-019 (a table bound by assigning `tables` is unrecorded, and a direct
# replacement keeps the old record), LIM-020 (a dataset outside its DTA resolves
# only file-name references), LIM-021 (templates have no `containers:` section)
# and LIM-022 (file-name conflicts are found by example only) are pinned where
# they are met below.

# ---- local helpers --------------------------------------------------------------

# A YAML document of one dataset ae whose file entry carries `entry_lines`, with
# the lines of a `containers:` section (or none) before the datasets.
md_yaml <- function(containers_lines, entry_lines, env = parent.frame()) {
  p <- file.path(qa_tempdir(env), "doc.yaml")
  writeLines(
    c(
      "metadata:", "  title: T", "  version: '1'",
      containers_lines,
      "datasets:",
      "  - id: ae", "    type: tabular", "    files:",
      "      type: csv", "      filename: ae.csv",
      paste0("      ", entry_lines)
    ),
    p
  )
  p
}

# read_dta_from_yaml() with its lint warnings muffled.
md_read <- function(p) suppressWarnings(read_dta_from_yaml(p))

# Lint rows of a document of one dataset per element of `files` (a list of
# `files:` values), with `containers` as the containers section.
md_lint_doc <- function(containers = NULL, files = list(list(type = "csv", filename = "ae.csv")), extra = list()) {
  datasets <- lapply(seq_along(files), function(i) list(id = paste0("ds", i), type = "tabular", files = files[[i]]))
  doc <- c(list(metadata = list(title = "T", version = "1"), datasets = datasets), extra)
  if (!is.null(containers)) doc$containers <- containers
  lint_dta(doc)
}
md_rows <- function(out, code) out[out$code == code, , drop = FALSE]

# ---- DTAContainer -----------------------------------------------------------------

test_that("OQ-CONT-040 | a container keeps its four fields and refuses an id, a file name or a flag that is not valid | REQ-CONT-019", {
  c1 <- DTAtools::DTAContainer("sdtm", "STUDY_SDTM_*.zip", description = "All SDTM domains.", default = TRUE)
  qa_step(
    "the four fields are kept as given",
    list(id = "sdtm", filename = "STUDY_SDTM_*.zip", description = "All SDTM domains.", default = TRUE),
    list(id = c1@id, filename = c1@filename, description = c1@description, default = c1@default)
  )
  c2 <- DTAtools::DTAContainer("adam", "adam.zip")
  qa_step("the description is NULL and default FALSE unless given", list(NULL, FALSE), list(c2@description, c2@default))

  ids <- c("", "1a", "a.b", "none", "None", "NONE", "a b", "-a", "_a", "{x}")
  refused <- vapply(ids, function(id) cont_is(cont_catch(DTAtools::DTAContainer(id, "a.zip")), "dta_container_invalid"), logical(1))
  qa_step(
    "an id that is empty, starts with a digit, hyphen or underscore, holds a dot, a space or a brace, or is none in any case is refused",
    rep(TRUE, length(ids)), unname(refused)
  )
  fine <- c("a", "A1", "sdtm-2_x", "nonet")
  made <- vapply(fine, function(id) inherits(cont_catch(DTAtools::DTAContainer(id, "a.zip")), "error"), logical(1))
  qa_step("a letter, then letters, digits, underscore and hyphen, and nonet, are accepted", rep(FALSE, length(fine)), unname(made))

  files <- c("x.tar", "a/b.zip", "", "noextension")
  bad_file <- vapply(files, function(f) cont_is(cont_catch(DTAtools::DTAContainer("a", f)), "dta_container_invalid"), logical(1))
  qa_step("a file name that is not a single .zip or .xpt file name is refused", rep(TRUE, length(files)), unname(bad_file))
  qa_step(
    "a .zip, an .xpt and a wildcard name are accepted",
    rep(FALSE, 3),
    vapply(c("a.zip", "b.xpt", "s_*.zip"), function(f) inherits(cont_catch(DTAtools::DTAContainer("a", f)), "error"), logical(1), USE.NAMES = FALSE)
  )
  qa_step(
    "a default that is not TRUE or FALSE is refused",
    list(TRUE, TRUE),
    list(
      cont_is(cont_catch(DTAtools::DTAContainer("a", "a.zip", default = NA)), "dta_container_invalid"),
      cont_is(cont_catch(DTAtools::DTAContainer("a", "a.zip", default = "yes")), "dta_container_invalid")
    )
  )
  qa_step(
    "a description that is not a single string is refused",
    TRUE,
    cont_is(cont_catch(DTAtools::DTAContainer("a", "a.zip", description = c("x", "y"))), "dta_container_invalid")
  )
  qa_step(
    "an assignment is refused as a construction is",
    TRUE,
    cont_is(cont_catch({
      c3 <- DTAtools::DTAContainer("a", "a.zip")
      c3@filename <- "x.tar"
    }), "dta_container_invalid")
  )
  # Braces in a name are text, never evaluated.
  braces <- DTAtools::DTAContainer("a", "{1+1}.zip")
  qa_step("braces in a file name are kept as text", "{1+1}.zip", braces@filename)
})

test_that("OQ-CONT-041 | a DTA keeps containers keyed by id, refuses duplicates, a second default and an id nobody defines, and normalises a file name to the id | REQ-CONT-019", {
  a <- DTAtools::DTAContainer("a", "a.zip")
  refuse <- function(containers) cont_is(cont_catch(DTAtools::DTA(containers = containers)), "dta_container_invalid")
  qa_step(
    "a duplicate id, a duplicate file name, two defaults and a non-container are refused with dta_container_invalid",
    rep(TRUE, 4),
    c(
      refuse(list(a, DTAtools::DTAContainer("a", "b.zip"))),
      refuse(list(a, DTAtools::DTAContainer("b", "a.zip"))),
      refuse(list(DTAtools::DTAContainer("a", "a.zip", default = TRUE), DTAtools::DTAContainer("b", "b.zip", default = TRUE))),
      refuse(list(1))
    )
  )
  qa_step("a single container is a list of one, keyed by its id", "a", names(DTAtools::DTA(containers = a)@containers))
  d <- DTAtools::DTA(containers = list(a))
  qa_step(
    "assigning a duplicate afterwards is refused too",
    TRUE,
    cont_is(cont_catch(d@containers <- list(a, DTAtools::DTAContainer("a", "b.zip"))), "dta_container_invalid")
  )

  # An id that nobody defines is an error; file names and none never are.
  e <- cont_catch(cont_doc(cont_csv_ds("ae", "sdtm", cont_empty_specs())))
  qa_step("an id the document does not define is refused with dta_container_unknown", TRUE, cont_is(e, "dta_container_unknown"))
  qa_step(
    "naming the entry, the dataset and the id, and telling to pass the DTA",
    paste0(
      "File entry 1 of dataset 'ae' refers to container 'sdtm', which this document does not define. ",
      "(A dataset on its own does not carry its document's containers: pass the DTA.)"
    ),
    cont_flat(e)
  )
  for (ref in c("sdtm.zip", "none")) {
    ok <- cont_catch(cont_doc(cont_csv_ds("ae", ref, cont_empty_specs())))
    qa_step(sprintf("the reference '%s' is never unknown", ref), FALSE, inherits(ok, "error"))
  }
  defined <- cont_doc(cont_csv_ds("ae", "sdtm", cont_empty_specs()), containers = list(DTAtools::DTAContainer("sdtm", "s.zip")))
  qa_step("an id the document defines is kept as written", "sdtm", defined@datasets$ae@files[[1]]@container)

  # A file name equal to a defined container's file name is stored as its id.
  n <- cont_doc(
    cont_csv_ds("ae", "STUDY_SDTM_*.zip", cont_empty_specs()), cont_csv_ds("dm", "other.zip", cont_empty_specs()),
    containers = list(DTAtools::DTAContainer("sdtm", "STUDY_SDTM_*.zip"))
  )
  qa_step(
    "a reference equal to a container's file name becomes its id; another file name stays",
    c("sdtm", "other.zip"),
    c(n@datasets$ae@files[[1]]@container, n@datasets$dm@files[[1]]@container)
  )
  # LIM-020: a dataset on its own does not carry the document's containers, so an
  # id it carries can only be placed in a DTA; a file name or none stands alone.
  qa_step(
    "LIM-020: a dataset on its own is built with an id the DTA must later define",
    "sdtm",
    cont_csv_ds("ae", "sdtm", cont_empty_specs())@files[[1]]@container
  )
  alone <- cont_catch(DTAtools::load_delivery(cont_csv_ds("ae", "sdtm", cont_empty_specs()), cont_write("ae.csv", CONT_AE_CSV)))
  qa_step(
    "LIM-020: delivering to that dataset on its own refuses the id, telling to pass the DTA",
    list(class = TRUE, hint = TRUE),
    list(class = cont_is(alone, "dta_container_unknown"), hint = grepl("pass the DTA", cont_flat(alone), fixed = TRUE))
  )
})

test_that("OQ-CONT-042 | container_assignments() has one row per entry naming the container it must arrive inside, and print() names the containers in effect | REQ-CONT-019", {
  d <- cont_doc(
    cont_csv_ds("ae", "sdtm", cont_empty_specs()),
    cont_csv_ds("dm", NULL, cont_empty_specs()),
    cont_csv_ds("vs", "none", cont_empty_specs()),
    cont_csv_ds("lb", "zzz.zip", cont_empty_specs()),
    cont_tab("ex", list(DTAtools::DTAFileCSV("^ex", pattern = TRUE, pattern_description = "ex files")), cont_empty_specs()),
    containers = list(DTAtools::DTAContainer("sdtm", "s.zip"), DTAtools::DTAContainer("all", "all.zip", default = TRUE))
  )
  ca <- DTAtools::container_assignments(d)
  qa_step(
    "the columns, in order",
    c("dataset", "handler", "expected", "reference", "container", "filename", "default", "implied"),
    names(ca)
  )
  qa_step(
    "dataset, entry index and the expected name (the description for a pattern)",
    list(dataset = c("ae", "dm", "vs", "lb", "ex"), handler = rep(1L, 5), expected = c("ae.csv", "dm.csv", "vs.csv", "lb.csv", "ex files")),
    list(dataset = ca$dataset, handler = ca$handler, expected = ca$expected)
  )
  qa_step(
    "the reference as written: an id, nothing, none, a file name, nothing",
    c("sdtm", NA, "none", "zzz.zip", NA),
    ca$reference
  )
  qa_step(
    "the container each must arrive inside: its id, the default, none, an implied file name, the default",
    c("sdtm", "all", NA, "zzz.zip", "all"),
    ca$container
  )
  qa_step("and the file name of that container", c("s.zip", "all.zip", NA, "zzz.zip", "all.zip"), ca$filename)
  qa_step("default marks the assignments that come from the default container", c(FALSE, TRUE, FALSE, FALSE, TRUE), ca$default)
  qa_step("implied marks the container that no definition carries", c(FALSE, FALSE, FALSE, TRUE, FALSE), ca$implied)
  qa_step(
    "a document without entries gives a zero-row frame with the same columns",
    list(0L, names(ca)),
    list(nrow(DTAtools::container_assignments(DTAtools::DTA())), names(DTAtools::container_assignments(DTAtools::DTA())))
  )
  qa_step("a value that is not a DTA is refused", TRUE, inherits(cont_catch(DTAtools::container_assignments(list())), "error"))

  shown <- function(x) paste(cli::ansi_strip(testthat::capture_messages(print(x))), collapse = "")
  p <- shown(cont_doc(
    cont_csv_ds("ae", NULL, cont_empty_specs()), cont_csv_ds("lb", "zzz.zip", cont_empty_specs()),
    cont_csv_ds("dm", "yyy.zip", cont_empty_specs()),
    containers = list(DTAtools::DTAContainer("all", "STUDY_*.zip", default = TRUE))
  ))
  qa_check(
    "print() names the default container, then the implied ones in order of first use, joined by commas",
    grepl("Containers: STUDY_*.zip (all other files), zzz.zip, yyy.zip", p, fixed = TRUE), p
  )
  qa_check("and says nothing of containers when none is in effect", !grepl("Containers", shown(cont_of(cont_csv_ds("ae", NULL, cont_empty_specs()))), fixed = TRUE))
})

test_that("OQ-CONT-043 | a containers section of a YAML document is read as written, and a 0.27.0 document reads as one implied container | REQ-CONT-019", {
  p <- md_yaml(
    c(
      "containers:", "  - id: sdtm", "    filename: 'STUDY_SDTM_*.zip'",
      "    description: All SDTM.", "    default: yes", "  - id: adam", "    filename: adam.zip"
    ),
    "container: adam"
  )
  d <- md_read(p)
  qa_step("the containers are keyed by id, in the order written", c("sdtm", "adam"), names(d@containers))
  qa_step(
    "the first keeps its file name, description and default flag",
    list(filename = "STUDY_SDTM_*.zip", description = "All SDTM.", default = TRUE),
    list(filename = d@containers$sdtm@filename, description = d@containers$sdtm@description, default = d@containers$sdtm@default)
  )
  qa_step(
    "the second has no description and is not the default",
    list(NULL, FALSE),
    list(d@containers$adam@description, d@containers$adam@default)
  )
  qa_step("the entry's id reference is kept", "adam", d@datasets$ae@files[[1]]@container)

  by_name <- md_read(md_yaml(c("containers:", "  - id: adam", "    filename: adam.zip"), "container: adam.zip"))
  qa_step("a file name reference to a defined container is read as its id", "adam", by_name@datasets$ae@files[[1]]@container)

  old <- md_read(md_yaml(character(), "container: sdtm_csv.zip"))
  ca <- DTAtools::container_assignments(old)
  qa_step(
    "a document without a containers section keeps its file name as one implied container",
    list(n = 0L, container = "sdtm_csv.zip", implied = TRUE),
    list(n = length(old@containers), container = ca$container, implied = ca$implied)
  )

  e <- cont_catch(read_dta_from_yaml(md_yaml(c("containers:", "  - id: a", "    filename: a.zip", "    required: true"), "container: a")))
  qa_step("a container field that does not exist refuses the document", TRUE, cont_is(e, "dta_lint_error"))
  qa_check(
    "and the finding names the field",
    any(grepl("required", e$findings$message[e$findings$code == "container_invalid"], fixed = TRUE)),
    e$findings$message
  )
})

# ---- lint -----------------------------------------------------------------------------

test_that("OQ-CONT-044 | lint reports a malformed container section, an unknown id, an unused container, an unknown top-level key and a rename-unsafe reference | REQ-CONT-026", {
  bad_id <- md_rows(md_lint_doc(list(list(id = "1a", filename = "x.zip"))), "container_invalid")
  qa_step(
    "a container whose id is invalid is one error on a container row, with an empty dataset",
    list(rows = 1L, severity = "error", object = "container", id = "1a", dataset = ""),
    list(rows = nrow(bad_id), severity = bad_id$severity, object = bad_id$object, id = bad_id$id, dataset = bad_id$dataset)
  )
  shapes <- list(
    mapping_not_list = list(id = "a", filename = "a.zip"),
    no_filename = list(list(id = "a")),
    unknown_field = list(list(id = "a", filename = "a.zip", colour = "red")),
    not_zip = list(list(id = "a", filename = "a.tar")),
    not_a_mapping = list("a.zip")
  )
  for (nm in names(shapes)) {
    out <- md_rows(md_lint_doc(shapes[[nm]]), "container_invalid")
    qa_step(sprintf("%s: one container_invalid error", nm), list(1L, "error"), list(nrow(out), out$severity))
  }

  dup_id <- md_rows(md_lint_doc(list(list(id = "a", filename = "a.zip"), list(id = "a", filename = "b.zip"))), "container_duplicate")
  qa_step("a duplicate id is a container_duplicate error naming the id", list(1L, "a", "error"), list(nrow(dup_id), dup_id$id, dup_id$severity))
  dup_file <- md_rows(md_lint_doc(list(list(id = "a", filename = "a.zip"), list(id = "b", filename = "a.zip"))), "container_duplicate")
  qa_step("a duplicate file name is one container_duplicate", 1L, nrow(dup_file))
  two <- md_rows(
    md_lint_doc(list(list(id = "a", filename = "a.zip", default = TRUE), list(id = "b", filename = "b.zip", default = TRUE))),
    "container_duplicate"
  )
  qa_step("two defaults are one container_duplicate error on a container", list(1L, "error", "container"), list(nrow(two), two$severity, two$object))

  files <- list(list(type = "csv", filename = "ae.csv", container = "used"))
  unused <- md_rows(
    md_lint_doc(list(
      list(id = "used", filename = "u.zip"), list(id = "idle", filename = "i.zip"), list(id = "dflt", filename = "d.zip", default = TRUE)
    ), files = files),
    "container_unused"
  )
  qa_step(
    "a defined, non-default container no entry refers to is one warning; a default is never unused",
    list(rows = 1L, id = "idle", severity = "warning", dataset = ""),
    list(rows = nrow(unused), id = unused$id, severity = unused$severity, dataset = unused$dataset)
  )
  by_file <- md_lint_doc(list(list(id = "idle", filename = "i.zip")), files = list(list(type = "csv", filename = "ae.csv", container = "i.zip")))
  qa_step("a reference by file name counts as a use", 0L, nrow(md_rows(by_file, "container_unused")))

  unknown <- md_rows(md_lint_doc(files = files), "file_container_unknown")
  qa_step(
    "an undotted reference that names no container is an error on the entry",
    list(rows = 1L, severity = "error", object = "file", dataset = "ds1", id = "ae.csv"),
    list(rows = nrow(unknown), severity = unknown$severity, object = unknown$object, dataset = unknown$dataset, id = unknown$id)
  )
  qa_step(
    "it is not reported once the container is defined",
    0L,
    nrow(md_rows(md_lint_doc(list(list(id = "used", filename = "u.zip")), files = files), "file_container_unknown"))
  )
  qa_step(
    "none and a file name are never unknown",
    c(0L, 0L),
    vapply(c("none", "x.zip"), function(ref) {
      nrow(md_rows(md_lint_doc(files = list(list(type = "csv", filename = "ae.csv", container = ref))), "file_container_unknown"))
    }, integer(1), USE.NAMES = FALSE)
  )
  alone <- lint_dta(list(id = "ae", type = "tabular", files = list(type = "csv", filename = "ae.csv", container = "sdtm")))
  qa_step("a dataset linted on its own reports no unknown id (LIM-020)", 0L, nrow(md_rows(alone, "file_container_unknown")))

  extra <- md_rows(md_lint_doc(extra = list(colour = "red")), "document_field_unknown")
  qa_step(
    "an unexpected top-level key is one warning naming it",
    list(rows = 1L, severity = "warning", object = "document", id = "colour"),
    list(rows = nrow(extra), severity = extra$severity, object = extra$object, id = extra$id)
  )
  qa_step("and the three known keys give none", 0L, nrow(md_rows(md_lint_doc(), "document_field_unknown")))

  p <- md_yaml(c("containers:", "  - id: sdtm", "    filename: sdtm.zip"), "container: sdtm.zip")
  rename <- md_rows(lint_dta(p), "container_reference_by_filename")
  qa_step(
    "a reference by the file name of a container that has an id is one warning on the entry",
    list(rows = 1L, severity = "warning", object = "file", dataset = "ae", id = "ae.csv"),
    list(rows = nrow(rename), severity = rename$severity, object = rename$object, dataset = rename$dataset, id = rename$id)
  )
  qa_check("and it tells to refer to the container by its id", grepl("'sdtm'", rename$message, fixed = TRUE), rename$message)
  obj <- md_read(p)
  obj@datasets[[1]]@files[[1]]@container <- "sdtm.zip"
  qa_step(
    "the object form reports the same",
    1L,
    nrow(md_rows(lint_dta(obj), "container_reference_by_filename"))
  )
  qa_step("a clean 0.27.0 document lints without a finding", 0L, nrow(md_lint_doc(files = list(
    list(type = "csv", filename = "ae.csv", container = "sdtm_*.zip"), list(type = "csv", filename = "dm.csv")
  ))))
})

# ---- provenance -------------------------------------------------------------------------

test_that("OQ-CONT-045 | every bound item records the file as delivered, the containers it came through and how it was bound | REQ-CONT-024", {
  # load_file(): the file on its own, no container.
  ds <- load_file(cont_csv_ds("ae"), file = cont_write("ae.csv", CONT_AE_CSV))
  qa_step(
    "a table read with load_file() is recorded under its table name, on its own",
    list(file = "ae.csv", container = character(), via = "file"),
    ds@provenance[["ae"]]
  )
  streamed <- load_file(cont_csv_ds("ae"), file = cont_write("ae.csv", CONT_AE_CSV), stream = "always")
  qa_step(
    "a streamed table is recorded the same way",
    list(file = "ae.csv", container = character(), via = "file"),
    streamed@provenance[["ae"]]
  )

  # load_delivery(): the chain, outermost first.
  z <- cont_run(cont_dta("csv"), cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-045"))
  qa_step(
    "a member of an archive records the archive",
    list(file = "ae.csv", container = "sdtm_csv.zip", via = "delivery"),
    DTAtools::datasets(z$result, "ae")@provenance$ae
  )
  lib <- cont_run(cont_dta("xpt"), cont_fx("lib_in_zip.zip"), exdir = cont_exdir("OQ-CONT-045"))
  qa_step(
    "a member of a library inside an archive records both, the outer first",
    list(file = "ae.xpt", container = c("lib_in_zip.zip", "sdtm.xpt"), via = "delivery"),
    DTAtools::datasets(lib$result, "ae")@provenance$ae
  )
  loose <- cont_run(cont_dta("csv"), cont_write("ae.csv", CONT_AE_CSV), exdir = cont_exdir("OQ-CONT-045"))
  qa_step(
    "a loose file in a delivery records an empty chain, and via delivery",
    list(file = "ae.csv", container = character(), via = "delivery"),
    DTAtools::datasets(loose$result, "ae")@provenance$ae
  )

  # A file dataset is keyed by the path the file was bound under, and a
  # replacement drops the old key.
  docs <- cont_filedata("docs", DTAtools::DTAFileAny("define.pdf"))
  p1 <- cont_write("define.pdf", "one")
  p2 <- cont_write("define.pdf", "two")
  d1 <- load_file(docs, file = p1)
  qa_step("a file dataset is recorded under its bound path", p1, names(d1@provenance))
  qa_step(
    "with the delivered name and no container",
    list(file = "define.pdf", container = character(), via = "file"),
    d1@provenance[[p1]]
  )
  d2 <- load_file(d1, file = p2)
  qa_step("a replacement drops the old key and records the new path", p2, names(d2@provenance))
  dz <- cont_run(cont_of(cont_filedata("docs", DTAtools::DTAFileAny("define.xml"))), cont_fx("sdtm_csv.zip"), exdir = cont_exdir("OQ-CONT-045"))
  docs_z <- DTAtools::datasets(dz$result, "docs")
  qa_step(
    "a file dataset filled by a delivery is keyed by the extracted path and records the archive",
    list(file = "define.xml", container = "sdtm_csv.zip", via = "delivery"),
    docs_z@provenance[[docs_z@file_paths]]
  )

  # A later load overwrites the record, and the record travels inside a DTA.
  again <- load_file(z$result, dataset = "ae", file = cont_write("ae.csv", CONT_AE_CSV))
  qa_step(
    "a load_file() after a delivery overwrites the record of that table",
    list(file = "ae.csv", container = character(), via = "file"),
    DTAtools::datasets(again, "ae")@provenance$ae
  )
  qa_step("the other datasets keep theirs", "delivery", DTAtools::datasets(again, "dm")@provenance$dm$via)

  # LIM-019: a table bound by assigning `tables` has no record.
  bare <- cont_csv_ds("ae")
  bare@tables <- list(ae = arrow::arrow_table(USUBJID = "S-001", AETERM = "X", AESEQ = 1))
  qa_step("LIM-019: a table assigned directly has no provenance record", 0L, length(bare@provenance))
})

# ---- the load_file() guard ---------------------------------------------------------------

test_that("OQ-CONT-046 | load_file() refuses a .zip or .xpt for an entry that does not read one, and points to load_delivery() | REQ-CONT-028", {
  zip <- cont_fx("sdtm_csv.zip")
  e <- cont_catch(load_file(cont_dta("csv"), dataset = "ae", file = zip))
  qa_step("a .zip for a csv entry is refused with dta_load_file_container", TRUE, cont_is(e, "dta_load_file_container"))
  msg <- cont_flat(e)
  qa_step(
    "the message names the file by its base name, says it is a container, and points to load_delivery()",
    list(base = TRUE, container = TRUE, hint = TRUE, no_dir = TRUE),
    list(
      base = grepl("sdtm_csv.zip", msg, fixed = TRUE),
      container = grepl("is a container", msg, fixed = TRUE),
      hint = grepl("load_delivery(", msg, fixed = TRUE),
      no_dir = !grepl(dirname(zip), msg, fixed = TRUE)
    )
  )
  x <- cont_catch(load_file(cont_dta("csv"), dataset = "ae", file = cont_fx("sdtm.xpt")))
  qa_step("a .xpt for a csv entry is refused the same way", TRUE, cont_is(x, "dta_load_file_container"))

  # An xpt entry reads a .xpt; a type any entry takes a .zip as one file.
  ok <- cont_catch(load_file(cont_dta("xpt"), dataset = "ae", file = cont_fx("adverse.xpt")))
  qa_step("a type xpt entry is not refused for a .xpt file", FALSE, cont_is(ok, "dta_load_file_container"))
  any_ds <- cont_filedata("docs", DTAtools::DTAFileAny("sdtm_csv.zip"))
  bound <- load_file(any_ds, file = zip)
  qa_step("a type any entry takes the .zip as one file", "sdtm_csv.zip", basename(bound@file_paths))
})

# ---- upgrade_dta() ---------------------------------------------------------------------

test_that("OQ-CONT-047 | upgrade_dta() gives an object saved by an older version the current definition, keeps what was set, and leaves a current object unchanged | REQ-CONT-029 | tags: white-box", {
  d <- cont_doc(cont_csv_ds("ae"), cont_csv_ds("dm"), containers = list(DTAtools::DTAContainer("main", "delivery.zip")))
  d <- suppressMessages(load_file(d, dataset = "ae", file = cont_write("ae.csv", CONT_AE_CSV)))
  up <- DTAtools::upgrade_dta(d)
  qa_step(
    "a current object comes back with identical datasets, metadata and containers",
    list(TRUE, TRUE, TRUE),
    list(identical(up@datasets, d@datasets), identical(up@metadata, d@metadata), identical(up@containers, d@containers))
  )
  qa_step("and the provenance of its datasets", d@datasets$ae@provenance, up@datasets$ae@provenance)

  # An object as 0.27.0 saved it: its DTA class has no `containers`. The class
  # object is what saveRDS() keeps, so the round trip restores the old shape.
  cur <- attr(d, "S7_class")
  old_class <- S7::new_class(
    "DTA",
    package = "DTAtools", parent = cur@parent,
    properties = cur@properties[setdiff(names(cur@properties), "containers")],
    validator = function(self) NULL
  )
  # (An Arrow table does not survive saveRDS(), so the old object holds none.)
  old <- cont_doc(cont_csv_ds("ae"), cont_csv_ds("dm"))
  attr(old, "containers") <- NULL
  attr(old, "S7_class") <- old_class
  f <- file.path(qa_tempdir(), "old.rds")
  saveRDS(old, f)
  old <- readRDS(f)
  qa_step("the old object cannot read a property of the current class", TRUE, inherits(cont_catch(old@containers), "error"))
  qa_step(
    "the functions that read it upgrade it by themselves: container_assignments() has a row per entry",
    2L,
    nrow(DTAtools::container_assignments(old))
  )
  fixed <- DTAtools::upgrade_dta(old)
  qa_step(
    "upgrade_dta() gives the missing property its default and keeps the datasets and their entries as they were",
    list(containers = list(), datasets = c(ae = "ae.csv", dm = "dm.csv")),
    list(
      containers = fixed@containers,
      datasets = vapply(fixed@datasets, function(x) x@files[[1]]@filename, character(1))
    )
  )
  qa_step("anything that is not a DTA or a dataset is refused", TRUE, inherits(cont_catch(DTAtools::upgrade_dta("x")), "error"))
})

# ---- file-name conflicts ------------------------------------------------------------------

# A DTA of one tabular dataset per element of `spec`, each element
# list(id, files = list(list(type, filename, ...))); every entry is a pattern entry
# unless it says pattern = FALSE.
md_conflicts_doc <- function(spec, containers = list()) {
  datasets <- lapply(spec, function(d) {
    files <- lapply(d$files, function(f) {
      if (is.null(f[["pattern"]])) f[["pattern"]] <- TRUE
      do.call(DTAtools::DTAFileFactory, f)
    })
    cont_tab(d$id, files, cont_empty_specs())
  })
  do.call(cont_doc, c(unname(datasets), list(containers = containers)))
}
md_xpt <- function(filename, ...) list(type = "xpt", filename = filename, ...)
# Dataset lb takes `^lb.*\.xpt$`, dataset lb_supp takes `^lb_supp.*\.xpt$`: the
# shortest name both take is lb_supp.xpt.
md_lb_pair <- function(...) {
  list(
    list(id = "lb", files = list(md_xpt("^lb.*\\.xpt$", ...))),
    list(id = "lb_supp", files = list(md_xpt("^lb_supp.*\\.xpt$", ...)))
  )
}

test_that("OQ-CONT-048 | file_name_conflicts() names each pair of pattern entries of two datasets that could take one file, with the shortest example | REQ-CONT-032", {
  cols <- c("dataset_a", "entry_a", "expected_a", "dataset_b", "entry_b", "expected_b", "example")
  none <- DTAtools::file_name_conflicts(md_conflicts_doc(list(
    list(id = "ae", files = list(list(type = "csv", filename = "^ae[.]csv$"))),
    list(id = "dm", files = list(list(type = "csv", filename = "^dm[.]csv$")))
  )))
  qa_step(
    "two patterns no name satisfies together give a zero-row frame with the seven columns",
    list(cols = cols, rows = 0L, entry_a = "integer"),
    list(cols = names(none), rows = nrow(none), entry_a = typeof(none$entry_a))
  )
  out <- DTAtools::file_name_conflicts(md_conflicts_doc(md_lb_pair()))
  qa_step(
    "lb and lb_supp conflict: one row, the shortest file name both take as example",
    list(
      rows = 1L, dataset_a = "lb", entry_a = 1L, expected_a = "^lb.*\\.xpt$",
      dataset_b = "lb_supp", entry_b = 1L, expected_b = "^lb_supp.*\\.xpt$", example = "lb_supp.xpt"
    ),
    list(
      rows = nrow(out), dataset_a = out$dataset_a, entry_a = out$entry_a, expected_a = out$expected_a,
      dataset_b = out$dataset_b, entry_b = out$entry_b, expected_b = out$expected_b, example = out$example
    )
  )

  # What is not a conflict: literal entries, and entries of one dataset.
  same_literal <- md_conflicts_doc(list(
    list(id = "ae", files = list(list(type = "csv", filename = "ae.csv", pattern = FALSE))),
    list(id = "dm", files = list(list(type = "csv", filename = "ae.csv", pattern = FALSE)))
  ))
  qa_step("two literal entries of the same name are not a conflict", 0L, nrow(DTAtools::file_name_conflicts(same_literal)))
  literal_vs_pattern <- md_conflicts_doc(list(
    list(id = "ae", files = list(list(type = "csv", filename = "ae.csv", pattern = FALSE))),
    list(id = "dm", files = list(list(type = "csv", filename = ".*\\.csv")))
  ))
  qa_step("a literal entry and a pattern entry are not a conflict", 0L, nrow(DTAtools::file_name_conflicts(literal_vs_pattern)))
  one_dataset <- md_conflicts_doc(list(list(id = "lb", files = list(md_xpt("^lb.*\\.xpt$"), md_xpt("^lb_supp.*\\.xpt$")))))
  qa_step("entries of one dataset are not a conflict", 0L, nrow(DTAtools::file_name_conflicts(one_dataset)))
  shadow <- md_conflicts_doc(c(md_lb_pair(), list(list(id = "other", files = list(md_xpt("lb_supp.xpt", pattern = FALSE))))))
  qa_step(
    "a literal entry of a third dataset takes its own name away from the pair, so the example is the next name",
    "lb_suppa.xpt",
    DTAtools::file_name_conflicts(shadow)$example
  )

  # Containers decide whether two entries can tie on one correctly packaged file.
  with_containers <- function(glob_a, glob_b, containers = list()) {
    spec <- md_lb_pair()
    spec[[1]]$files[[1]]$container <- glob_a
    spec[[2]]$files[[1]]$container <- glob_b
    nrow(DTAtools::file_name_conflicts(md_conflicts_doc(spec, containers)))
  }
  qa_step("entries for two archives that cannot be one name are not a conflict", 0L, with_containers("sdtm.zip", "adam.zip"))
  qa_step("entries for the same archive are", 1L, with_containers("same.zip", "same.zip"))
  qa_step("entries for archives one glob of which takes the other's name (STUDY_*.zip, STUDY_SDTM_*.zip) are", 1L, with_containers("STUDY_*.zip", "STUDY_SDTM_*.zip"))
  qa_step("a free entry and one that needs a container never tie", c(0L, 0L), c(with_containers("sdtm.zip", NULL), with_containers(NULL, "sdtm.zip")))
  by_id <- with_containers(
    "a", "b", list(DTAtools::DTAContainer("a", "STUDY_*.zip"), DTAtools::DTAContainer("b", "STUDY_SDTM_*.zip"))
  )
  qa_step("references by id are resolved to the file names first", 1L, by_id)
  default <- list(DTAtools::DTAContainer("d", "delivery.zip", default = TRUE))
  qa_step("two entries under the default container conflict", 1L, nrow(DTAtools::file_name_conflicts(md_conflicts_doc(md_lb_pair(), default))))
  spec <- md_lb_pair()
  spec[[2]]$files[[1]]$container <- "none"
  qa_step("and none takes an entry out of it", 0L, nrow(DTAtools::file_name_conflicts(md_conflicts_doc(spec, default))))

  # LIM-022: detection is by example. A look-around hides an overlap, and a prefix
  # glob and a suffix glob are never seen to name one archive (STUDY_SDTM.zip is
  # taken by both).
  look <- md_conflicts_doc(list(
    list(id = "a", files = list(list(type = "csv", filename = "^a(?!b).*[.]csv$"))),
    list(id = "b", files = list(list(type = "csv", filename = "^.*b[.]csv$")))
  ))
  qa_step("LIM-022: an overlap that only a look-around hides is not reported", 0L, nrow(DTAtools::file_name_conflicts(look)))
  affix <- md_lb_pair()
  affix[[1]]$files[[1]]$container <- "STUDY_*.zip"
  affix[[2]]$files[[1]]$container <- "*_SDTM.zip"
  qa_step("LIM-022: a prefix glob and a suffix glob are never seen to overlap", 0L, nrow(DTAtools::file_name_conflicts(md_conflicts_doc(affix))))

  qa_step("a value that is not a DTA is refused", TRUE, inherits(cont_catch(DTAtools::file_name_conflicts(list())), "error"))
})

test_that("OQ-CONT-049 | lint reports a file-name conflict as a warning and validate_template() as an error | REQ-CONT-032 REQ-CONT-026", {
  doc <- function(container_dm = NULL) {
    dm <- list(type = "csv", filename = "^.*e[.]csv$", pattern = TRUE)
    if (!is.null(container_dm)) dm$container <- container_dm
    list(
      metadata = list(title = "T", version = "1"),
      datasets = list(
        list(id = "ae", type = "tabular", files = list(type = "csv", filename = "^a.*[.]csv$", pattern = TRUE)),
        list(id = "dm", type = "tabular", files = dm)
      )
    )
  }
  out <- lint_dta(doc())
  qa_step(
    "two datasets whose patterns both take ae.csv are one warning on the first, naming both entries",
    list(rows = 1L, code = "file_name_conflict", severity = "warning", dataset = "ae", object = "file", id = "ae[1] / dm[1]"),
    list(rows = nrow(out), code = out$code, severity = out$severity, dataset = out$dataset, object = out$object, id = out$id)
  )
  qa_step(
    "its message says which datasets, which patterns and which file name",
    paste0(
      "Datasets 'ae' (^a.*[.]csv$) and 'dm' (^.*e[.]csv$) could both take a file named 'ae.csv'. ",
      "Make their file names distinct so every file belongs to one dataset."
    ),
    out$message
  )
  p <- file.path(qa_tempdir(), "conflict.yaml")
  yaml::write_yaml(doc(), p)
  warned <- NULL
  read <- withCallingHandlers(
    read_dta_from_yaml(p),
    warning = function(w) {
      warned <<- w
      invokeRestart("muffleWarning")
    }
  )
  qa_step(
    "the document still reads, with a lint warning",
    list(dta = TRUE, warning = TRUE),
    list(dta = inherits(read, "DTAtools::DTA"), warning = inherits(warned, "dta_lint_warning"))
  )
  qa_step("and the object form reports the same message", out$message, lint_dta(read)$message)
  qa_step(
    "conflicts are not looked for while the document has an error: only the unknown container is reported",
    "file_container_unknown",
    lint_dta(doc(container_dm = "sdtm"))$code
  )

  # A creation template whose datasets conflict is an error.
  dir <- qa_tempdir()
  dir.create(file.path(dir, "templates"))
  writeLines(
    c(
      "kind: dta_creation_template", "id: conflicting", "version: \"1.0\"",
      "base:", "  metadata:", "    title: Conflicting",
      "datasets:",
      "  - id: ae", "    type: tabular",
      "    files: {filename: '^a.*[.]csv$', type: csv, pattern: true}",
      "    columns:", "      - {id: COL1, type: SAS Char}",
      "  - id: dm", "    type: tabular",
      "    files: {filename: '^.*e[.]csv$', type: csv, pattern: true}",
      "    columns:", "      - {id: COL1, type: SAS Char}"
    ),
    file.path(dir, "templates", "conflicting.dta-template.yaml")
  )
  result <- validate_template(dir)
  rows <- result[result$code == "file_name_conflict", ]
  qa_step(
    "validate_template() reports the conflict once, as an error, naming both datasets",
    list(rows = 1L, severity = "error", ae = TRUE, dm = TRUE),
    list(
      rows = nrow(rows), severity = rows$severity,
      ae = grepl("'ae'", rows$message, fixed = TRUE), dm = grepl("'dm'", rows$message, fixed = TRUE)
    )
  )
  # LIM-021: a creation template has no containers section (REQ-CONT-019 notes),
  # so what it can say about packaging is file names; the conflict above is the
  # only packaging rule a template library is held to.
})
