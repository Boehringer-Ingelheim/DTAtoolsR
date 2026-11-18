# DTAContainer and the document-level `containers` of a DTA. Every expected
# value is written out from the specification, never taken from the code.

# One-dataset document text with a `containers:` section; `entry` is the
# container line of the file entry.
cn_yaml <- function(containers_lines, entry) {
  c(
    "metadata:", "  title: T", "  version: '1'",
    containers_lines,
    "datasets:",
    "  - id: ae", "    type: tabular", "    files:",
    "      type: csv", "      filename: ae.csv",
    paste0("      ", entry)
  )
}

csv_ds <- function(id, container = NULL) {
  specs <- if (id %in% c("ae", "dm", "vs")) cont_specs(id) else DTAColumnSpecCollection(columns = list())
  tab_ds(id, list(DTAFileCSV(paste0(id, ".csv"), container = container)), specs = specs)
}

test_that("a container keeps its four fields", {
  c1 <- DTAContainer("sdtm", "STUDY_SDTM_*.zip")
  expect_identical(c1@id, "sdtm")
  expect_identical(c1@filename, "STUDY_SDTM_*.zip")
  expect_identical(c1@default, FALSE)
  expect_null(c1@description)
  c2 <- DTAContainer("adam", "adam.zip", description = "ADaM.", default = TRUE)
  expect_identical(c2@description, "ADaM.")
  expect_true(c2@default)
})

test_that("a container refuses what the specification refuses", {
  bad_ids <- list("", "1a", "a.b", "none", c("a", "b"), "{x}", NA_character_)
  for (id in bad_ids) {
    expect_error(DTAContainer(id, "a.zip"), class = "dta_container_invalid")
  }
  e <- expect_error(DTAContainer("{x}", "a.zip"), class = "dta_container_invalid")
  expect_match(conditionMessage(e), "{x}", fixed = TRUE)

  e <- expect_error(DTAContainer("sdtm", "x.tar"), class = "dta_container_invalid")
  expect_match(conditionMessage(e), "must name a .zip archive or a .xpt SAS transport file", fixed = TRUE)
  e <- expect_error(DTAContainer("sdtm", "a/b.zip"), class = "dta_container_invalid")
  expect_identical(
    conditionMessage(e),
    "Container 'sdtm': The container 'a/b.zip' must be a file name, not a path."
  )
  expect_error(DTAContainer("sdtm", "a.zip", default = NA), class = "dta_container_invalid")
  expect_error(DTAContainer("sdtm", "a.zip", description = NA_character_), class = "dta_container_invalid")

  c1 <- DTAContainer("sdtm", "a.zip")
  expect_error(c1@filename <- "x.tar", class = "dta_container_invalid")
})

test_that("braces in a file name are data, never evaluated", {
  c1 <- DTAContainer("sdtm", "{1+1}.zip")
  expect_identical(c1@filename, "{1+1}.zip")
  shown <- paste(utils::capture.output(print(c1)), collapse = "\n")
  expect_match(shown, "{1+1}.zip", fixed = TRUE)
  expect_no_match(shown, "2.zip", fixed = TRUE)
  e <- expect_error(DTAContainer("sdtm", "{1+1}.tar"), class = "dta_container_invalid")
  expect_match(conditionMessage(e), "{1+1}.tar", fixed = TRUE)
  # ... and an entry's reference is judged for syntax, not evaluated.
  e <- expect_error(DTAFileCSV("x.csv", container = "{x}"), class = "dta_container_invalid")
  expect_match(conditionMessage(e), "{x}", fixed = TRUE)
})

test_that("a DTA refuses malformed container sets", {
  a <- DTAContainer("a", "a.zip")
  expect_error(DTA(containers = list(a, DTAContainer("a", "b.zip"))), class = "dta_container_invalid")
  expect_error(DTA(containers = list(a, DTAContainer("b", "a.zip"))), class = "dta_container_invalid")
  expect_error(
    DTA(containers = list(DTAContainer("a", "a.zip", default = TRUE), DTAContainer("b", "b.zip", default = TRUE))),
    class = "dta_container_invalid"
  )
  expect_error(DTA(containers = list(1)), class = "dta_container_invalid")
  # The rules run on a DTA that has no datasets, too.
  expect_length(DTA(containers = list(a))@containers, 1L)
  # A single container is a list of one, keyed by its id.
  expect_identical(names(DTA(containers = a)@containers), "a")
})

test_that("the validator keeps checking containers after construction", {
  d <- DTA(containers = list(DTAContainer("a", "a.zip")))
  expect_error(
    S7::`prop<-`(d, "containers", value = list(DTAContainer("a", "a.zip"), DTAContainer("a", "b.zip"))),
    class = "dta_container_invalid"
  )
})

test_that("an id nobody defines is refused, a file name never is", {
  e <- expect_error(DTA(datasets = list(csv_ds("ae", "sdtm"))), class = "dta_container_unknown")
  expect_match(conditionMessage(e), "pass the DTA", fixed = TRUE)
  expect_match(conditionMessage(e), "File entry 1 of dataset 'ae' refers to container 'sdtm'", fixed = TRUE)

  expect_s3_class(DTA(datasets = list(csv_ds("ae", "sdtm.zip"))), "DTAtools::DTA")
  expect_s3_class(DTA(datasets = list(csv_ds("ae", "none"))), "DTAtools::DTA")
  d <- DTA(
    datasets = list(csv_ds("ae", "sdtm")),
    containers = list(DTAContainer("sdtm", "s.zip"))
  )
  expect_identical(d@datasets$ae@files[[1]]@container, "sdtm")
})

test_that("a reference equal to a container's file name becomes its id", {
  d <- DTA(
    datasets = list(csv_ds("ae", "STUDY_SDTM_*.zip"), csv_ds("dm", "other.zip")),
    containers = list(DTAContainer("sdtm", "STUDY_SDTM_*.zip"))
  )
  expect_identical(d@datasets$ae@files[[1]]@container, "sdtm")
  expect_identical(d@datasets$dm@files[[1]]@container, "other.zip")
})

test_that("dta_effective_container resolves own, implied, default and none", {
  sdtm <- DTAContainer("sdtm", "s_*.zip", description = "S.")
  adam <- DTAContainer("adam", "adam.zip", default = TRUE)
  cl <- list(sdtm = sdtm, adam = adam)
  eff <- function(ref, containers = cl) dta_effective_container(containers, DTAFileCSV("x.csv", container = ref))

  e <- eff("sdtm")
  expect_identical(e$key, "sdtm")
  expect_identical(e$filename, "s_*.zip")
  expect_identical(e$description, "S.")
  expect_false(e$implied)
  expect_identical(eff("s_*.zip")$key, "sdtm") # a file name equal to a container's
  imp <- eff("other.zip")
  expect_identical(imp, list(key = "other.zip", filename = "other.zip", description = NULL, default = FALSE, implied = TRUE))
  expect_identical(eff(NULL)$key, "adam") # the default
  expect_null(eff("none")) # none beats the default
  expect_null(eff(NULL, list(sdtm = sdtm))) # no default
  expect_null(eff("ghost")) # an id nobody defines
  # x may be the DTA itself
  d <- DTA(containers = cl)
  expect_identical(dta_effective_container(d, DTAFileCSV("x.csv"))$key, "adam")
})

test_that("container_assignments lists every entry with its container", {
  d <- DTA(
    datasets = list(
      csv_ds("ae", "sdtm"), csv_ds("dm"), csv_ds("vs", "none"), csv_ds("lb", "zzz.zip"),
      tab_ds("ex", list(DTAFileCSV("^ex", pattern = TRUE, pattern_description = "ex files")), specs = DTAColumnSpecCollection(columns = list()))
    ),
    containers = list(DTAContainer("sdtm", "s.zip"), DTAContainer("all", "all.zip", default = TRUE))
  )
  ca <- container_assignments(d)
  expect_identical(
    names(ca),
    c("dataset", "handler", "expected", "reference", "container", "filename", "default", "implied")
  )
  expect_identical(ca$dataset, c("ae", "dm", "vs", "lb", "ex"))
  expect_identical(ca$handler, rep(1L, 5L))
  expect_identical(ca$expected, c("ae.csv", "dm.csv", "vs.csv", "lb.csv", "ex files"))
  expect_identical(ca$reference, c("sdtm", NA, "none", "zzz.zip", NA))
  expect_identical(ca$container, c("sdtm", "all", NA, "zzz.zip", "all"))
  expect_identical(ca$filename, c("s.zip", "all.zip", NA, "zzz.zip", "all.zip"))
  expect_identical(ca$default, c(FALSE, TRUE, FALSE, FALSE, TRUE))
  expect_identical(ca$implied, c(FALSE, FALSE, FALSE, TRUE, FALSE))

  empty <- container_assignments(DTA())
  expect_identical(nrow(empty), 0L)
  expect_identical(names(empty), names(ca))
})

test_that("print names the containers in effect", {
  d <- DTA(
    datasets = list(csv_ds("ae"), csv_ds("lb", "zzz.zip")),
    containers = list(DTAContainer("all", "STUDY_*.zip", default = TRUE))
  )
  out <- paste(cli::ansi_strip(testthat::capture_messages(print(d))), collapse = "")
  # Exactly two containers: separated by ", " like any other count (plan §4.2).
  expect_match(out, "Containers: STUDY_*.zip (all other files), zzz.zip", fixed = TRUE)
  out <- paste(cli::ansi_strip(testthat::capture_messages(print(DTA(datasets = list(csv_ds("ae")))))), collapse = "")
  expect_no_match(out, "Containers", fixed = TRUE)
})

test_that("YAML: a containers section is read as written, an id reference included", {
  p <- withr::local_tempfile(fileext = ".yaml")
  writeLines(cn_yaml(
    c(
      "containers:", "  - id: sdtm", "    filename: 'STUDY_SDTM_*.zip'",
      "    description: All SDTM.", "    default: yes", "  - id: adam", "    filename: adam.zip"
    ),
    "container: adam"
  ), p)
  d <- suppressWarnings(read_dta_from_yaml(p))
  expect_identical(names(d@containers), c("sdtm", "adam"))
  expect_identical(d@containers$sdtm@filename, "STUDY_SDTM_*.zip")
  expect_identical(d@containers$sdtm@description, "All SDTM.")
  expect_true(d@containers$sdtm@default)
  expect_false(d@containers$adam@default)
  expect_null(d@containers$adam@description)
  expect_identical(d@datasets$ae@files[[1]]@container, "adam")

  # A file-name reference that normalises to an id.
  writeLines(cn_yaml(c("containers:", "  - id: adam", "    filename: adam.zip"), "container: adam.zip"), p)
  d2 <- suppressWarnings(read_dta_from_yaml(p))
  expect_identical(d2@datasets$ae@files[[1]]@container, "adam")
})

test_that("YAML: an unknown container field is a lint error naming it", {
  p <- withr::local_tempfile(fileext = ".yaml")
  writeLines(cn_yaml(c("containers:", "  - id: a", "    filename: a.zip", "    required: true"), "container: a"), p)
  e <- expect_error(suppressWarnings(read_dta_from_yaml(p)), class = "dta_lint_error")
  rows <- e$findings[e$findings$code == "container_invalid", , drop = FALSE]
  expect_gte(nrow(rows), 1L)
  expect_match(paste(rows$message, collapse = " "), "required", fixed = TRUE)
})

test_that("a container section the lint never sees is still refused with a classed error", {
  y <- yaml::yaml.load(paste(
    cn_yaml(c("containers:", "  - id: a", "    filename: a.zip", "    required: true"), "container: a"),
    collapse = "
"
  ))
  e <- expect_error(dta_from_list_build(y), class = "dta_container_invalid")
  expect_match(conditionMessage(e), "Unknown field(s) in container 1: 'required'", fixed = TRUE)
  y$containers[[1]]$required <- NULL
  y$containers[[1]]$filename <- NULL
  e <- expect_error(dta_from_list_build(y), class = "dta_container_invalid")
  expect_identical(conditionMessage(e), "Container 1 needs 'filename'.")
  y$containers[[1]]$id <- NULL
  e <- expect_error(dta_from_list_build(y), class = "dta_container_invalid")
  expect_identical(conditionMessage(e), "Container 1 needs 'id' and 'filename'.")
})

test_that("a container without a usable filename says so, in container terms", {
  e <- expect_error(DTAContainer("a"), class = "dta_container_invalid")
  expect_identical(conditionMessage(e), "Container 'a' needs a filename.")
  e <- expect_error(DTAContainer("a", NULL), class = "dta_container_invalid")
  expect_identical(conditionMessage(e), "Container 'a' needs a filename.")
  e <- expect_error(DTAContainer("a", c("a.zip", "b.zip")), class = "dta_container_invalid")
  expect_identical(conditionMessage(e), "The filename of container 'a' must be a single non-empty string.")
  e <- expect_error(DTAContainer("a", ""), class = "dta_container_invalid")
  expect_identical(conditionMessage(e), "The filename of container 'a' must be a single non-empty string.")
})

test_that("'none' is reserved as a container id in any letter case", {
  msg <- "A container's id must start with a letter and use only letters, digits, '_' and '-' ('none' is reserved); got '%s'."
  for (id in c("none", "None", "NONE", "nOnE")) {
    e <- expect_error(DTAContainer(id, "a.zip"), class = "dta_container_invalid")
    expect_identical(conditionMessage(e), sprintf(msg, id))
  }
  expect_error(DTAFileCSV("x.csv", container = "None"), class = "dta_container_invalid")
  expect_s3_class(DTAContainer("nonet", "a.zip"), "DTAtools::DTAContainer")
})

test_that("YAML: a 0.27.0 document reads as one implied container", {
  p <- withr::local_tempfile(fileext = ".yaml")
  writeLines(cn_yaml(character(), "container: sdtm_csv.zip"), p)
  d <- suppressWarnings(read_dta_from_yaml(p))
  expect_length(d@containers, 0L)
  ca <- container_assignments(d)
  expect_identical(ca$container, "sdtm_csv.zip")
  expect_true(ca$implied)
})
