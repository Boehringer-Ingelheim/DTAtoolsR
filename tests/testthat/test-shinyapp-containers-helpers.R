# The container helpers of the Shiny app (inst/shiny/dta_app/R/containers.R) and
# the container-aware parts of utils_dta.R. Expected values come from the plan
# (PLAN-container-manager.md sections 3, 4.5, 5.1, 5.8, 5.9), never from output.

# A DTA of datasets ae, dm, vs (one csv entry each, "<id>.csv"); `refs` gives the
# entries' own container reference by dataset id (absent = none declared).
ch_dta <- function(refs = list(), containers = list()) {
  mk <- function(id) {
    DTAtools::DTADataSetTabular(
      id = id, specs = cont_specs(id),
      files = list(DTAtools::DTAFileFactory(
        type = "csv", filename = paste0(id, ".csv"), container = refs[[id]]
      ))
    )
  }
  DTAtools::DTA(datasets = list(mk("ae"), mk("dm"), mk("vs")), containers = containers)
}
ch_ctr <- function(id, filename, ...) DTAtools::DTAContainer(id, filename, ...)
ch_refs <- function(dta) {
  vapply(DTAtools::datasets(dta), function(ds) {
    v <- ds@files[[1]]@container
    if (is.null(v)) NA_character_ else v
  }, character(1))
}

# ---- id derivation (plan 5.1) ------------------------------------------------------

test_that("dta_container_id_from_filename() derives ids as the plan lists", {
  f <- app_fn("dta_container_id_from_filename")
  expect_identical(f("sdtm.zip"), "sdtm")
  expect_identical(f("STUDY_SDTM_*.zip"), "STUDY_SDTM")
  expect_identical(f("2026 data.xpt.gz"), "container_2026_data")
  expect_identical(f("*.zip"), "container")
  expect_identical(f("sdtm.zip", taken = "sdtm"), "sdtm_2")
  expect_identical(f("sdtm.zip", taken = c("sdtm", "sdtm_2")), "sdtm_3")
  expect_identical(f("none.zip"), "container_none")
  expect_identical(f("ADaM.ZIP"), "ADaM")
})

# ---- choices ---------------------------------------------------------------------

test_that("dta_container_choices() without a default lists loose, explicit, implied", {
  dta <- ch_dta(
    refs = list(ae = "adam", dm = "sdtm_csv.zip"),
    containers = list(ch_ctr("adam", "adam.zip"))
  )
  ch <- app_fn("dta_container_choices")(dta)
  expect_identical(
    ch,
    c("Loose or in any container" = "", "adam.zip" = "adam", "sdtm_csv.zip" = "sdtm_csv.zip")
  )
})

test_that("dta_container_choices() with a default offers it as the empty value", {
  dta <- ch_dta(containers = list(
    ch_ctr("sdtm", "STUDY_SDTM_*.zip", default = TRUE), ch_ctr("adam", "adam.zip")
  ))
  ch <- app_fn("dta_container_choices")(dta)
  expect_identical(
    ch,
    c(
      "STUDY_SDTM_*.zip (with all other files)" = "", "adam.zip" = "adam",
      "No container required" = "none"
    )
  )
})

test_that("dta_container_choices() never drops the stored value (no silent loss)", {
  # An implied container is a choice by itself ...
  dta <- ch_dta(refs = list(ae = "sdtm_csv.zip"))
  ch <- app_fn("dta_container_choices")(dta, current = "sdtm_csv.zip")
  expect_true("sdtm_csv.zip" %in% ch)
  expect_identical(sum(ch == "sdtm_csv.zip"), 1L)
  # ... and a value that is nowhere else is appended LAST.
  ch2 <- app_fn("dta_container_choices")(dta, current = "legacy.zip")
  expect_identical(unname(utils::tail(ch2, 1)), "legacy.zip")
})

# ---- rows ------------------------------------------------------------------------

test_that("dta_container_rows() summarises explicit and implied containers", {
  dta <- ch_dta(
    refs = list(ae = "adam", dm = "adam", vs = "other.zip"),
    containers = list(ch_ctr("adam", "adam.zip", description = "ADaM"), ch_ctr("empty", "empty.zip"))
  )
  rows <- app_fn("dta_container_rows")(dta)
  keys <- vapply(rows, function(r) r$key, character(1))
  expect_identical(keys, c("adam", "empty", "other.zip"))
  adam <- rows[[1]]
  expect_identical(adam$summary, "2 files in 2 datasets")
  expect_identical(adam$description, "ADaM")
  expect_false(adam$implied)
  expect_identical(rows[[2]]$summary, "No files assigned")
  expect_true(rows[[3]]$implied)
  expect_identical(rows[[3]]$summary, "1 file in 1 dataset")
})

test_that("dta_container_rows() names the default's exceptions", {
  dta <- ch_dta(
    refs = list(dm = "none"),
    containers = list(ch_ctr("sdtm", "STUDY_*.zip", default = TRUE))
  )
  r <- app_fn("dta_container_rows")(dta)[[1]]
  expect_true(r$default)
  expect_identical(r$summary, "All other files; except: dm.csv (dm)")
  expect_identical(r$n_files, 2L)
})

# ---- effective glob ----------------------------------------------------------------

test_that("dta_effective_glob() resolves ids, the default and none", {
  dta <- ch_dta(
    refs = list(ae = "adam", dm = "none"),
    containers = list(ch_ctr("adam", "adam.zip"), ch_ctr("sdtm", "sdtm_*.zip", default = TRUE))
  )
  g <- app_fn("dta_effective_glob")
  expect_identical(g(dta, "ae", 1), "adam.zip")
  expect_identical(g(dta, "dm", 1), "")
  expect_identical(g(dta, "vs", 1), "sdtm_*.zip")
  expect_identical(g(dta, "nope", 1), "")
})

test_that("dta_handlers_overview() shows the effective glob, not the stored id", {
  dta <- ch_dta(refs = list(ae = "adam"), containers = list(ch_ctr("adam", "adam.zip")))
  ov <- app_fn("dta_handlers_overview")(dta, "ae")
  expect_identical(ov$container, "adam.zip")
})

# ---- save --------------------------------------------------------------------------

test_that("dta_container_save() adds a container with a derived id", {
  dta <- ch_dta()
  res <- app_fn("dta_container_save")(dta, NULL, "STUDY_SDTM_*.zip", "SDTM", FALSE)
  expect_true(res$ok)
  expect_identical(names(res$value@containers), "STUDY_SDTM")
  expect_identical(res$value@containers[["STUDY_SDTM"]]@description, "SDTM")
  expect_identical(res$key, "STUDY_SDTM")
})

test_that("dta_container_save() shows the package's message for a bad file name", {
  res <- app_fn("dta_container_save")(ch_dta(), NULL, "sdtm.tar", NULL, FALSE)
  expect_false(res$ok)
  expect_match(res$error, "sdtm.tar", fixed = TRUE)
})

test_that("dta_container_save() refuses a file name another container has", {
  dta <- ch_dta(containers = list(ch_ctr("adam", "adam.zip")))
  res <- app_fn("dta_container_save")(dta, NULL, "adam.zip", NULL, FALSE)
  expect_false(res$ok)
  expect_match(res$error, "adam.zip", fixed = TRUE)
})

test_that("dta_container_save() keeps entries on a file-name edit and normalises by name", {
  dta <- ch_dta(
    refs = list(ae = "adam", dm = "adam.zip"),
    containers = list(ch_ctr("adam", "adam.zip"))
  )
  res <- app_fn("dta_container_save")(dta, "adam", "adam_v2.zip", NULL, FALSE)
  expect_true(res$ok)
  expect_identical(res$value@containers[["adam"]]@filename, "adam_v2.zip")
  # The entry that named the container by its OLD file name now names its id.
  expect_identical(unname(ch_refs(res$value)[c("ae", "dm")]), c("adam", "adam"))
  expect_identical(app_fn("dta_effective_glob")(res$value, "dm", 1), "adam_v2.zip")
})

test_that("dta_container_save() materialises an implied container", {
  dta <- ch_dta(refs = list(ae = "sdtm_csv.zip", dm = "sdtm_csv.zip", vs = "other.zip"))
  res <- app_fn("dta_container_save")(dta, "sdtm_csv.zip", "sdtm_csv.zip", "CSV", FALSE)
  expect_true(res$ok)
  expect_identical(names(res$value@containers), "sdtm_csv")
  expect_identical(unname(ch_refs(res$value)), c("sdtm_csv", "sdtm_csv", "other.zip"))
})

test_that("dta_container_save() moves the default and says which container lost it", {
  dta <- ch_dta(containers = list(ch_ctr("a", "a.zip", default = TRUE), ch_ctr("b", "b.zip")))
  res <- app_fn("dta_container_save")(dta, "b", "b.zip", NULL, TRUE)
  expect_true(res$ok)
  expect_identical(
    vapply(res$value@containers, function(x) x@default, logical(1)),
    c(a = FALSE, b = TRUE)
  )
  expect_match(res$note, "a.zip", fixed = TRUE)
  # No move, no note.
  res2 <- app_fn("dta_container_save")(res$value, "b", "b.zip", "d", TRUE)
  expect_null(res2$note)
})

# ---- assign ------------------------------------------------------------------------

test_that("dta_container_assign() with unchanged values keeps an implied reference", {
  dta <- ch_dta(refs = list(ae = "sdtm_csv.zip"))
  a <- data.frame(dataset = c("ae", "dm"), handler = 1L, value = c("sdtm_csv.zip", ""))
  res <- app_fn("dta_container_assign")(dta, a)
  expect_true(res$ok)
  expect_identical(res$value@datasets[["ae"]]@files[[1]]@container, "sdtm_csv.zip")
  expect_null(res$value@datasets[["dm"]]@files[[1]]@container)
})

test_that("dta_container_assign() writes ids, none and file names, and only the listed rows", {
  dta <- ch_dta(refs = list(vs = "keep.zip"), containers = list(ch_ctr("adam", "adam.zip")))
  a <- data.frame(dataset = c("ae", "dm"), handler = 1L, value = c("adam.zip", "none"))
  res <- app_fn("dta_container_assign")(dta, a)
  expect_true(res$ok)
  expect_identical(unname(ch_refs(res$value)), c("adam", "none", "keep.zip"))
})

test_that("dta_container_assign() reports an unknown entry", {
  res <- app_fn("dta_container_assign")(
    ch_dta(), data.frame(dataset = "ae", handler = 2L, value = "x.zip")
  )
  expect_false(res$ok)
  expect_match(res$error, "ae", fixed = TRUE)
})

# ---- remove ------------------------------------------------------------------------

test_that("dta_container_remove() clears every reference, by id and by file name", {
  dta <- ch_dta(
    refs = list(ae = "adam", dm = "adam.zip", vs = "other.zip"),
    containers = list(ch_ctr("adam", "adam.zip"))
  )
  res <- app_fn("dta_container_remove")(dta, "adam")
  expect_true(res$ok)
  expect_length(res$value@containers, 0)
  expect_identical(unname(ch_refs(res$value)), c(NA_character_, NA_character_, "other.zip"))
})

test_that("dta_container_remove() clears an implied container's references", {
  dta <- ch_dta(refs = list(ae = "x.zip", dm = "x.zip", vs = "y.zip"))
  res <- app_fn("dta_container_remove")(dta, "x.zip")
  expect_true(res$ok)
  expect_identical(unname(ch_refs(res$value)), c(NA_character_, NA_character_, "y.zip"))
  expect_false(app_fn("dta_container_remove")(dta, "nothing")$ok)
})

# ---- conflicts (by content) ----------------------------------------------------------

lb_dta <- function() {
  ent <- function(pat) DTAtools::DTAFileFactory(type = "csv", filename = pat, pattern = TRUE)
  mk <- function(id, files) {
    DTAtools::DTADataSetTabular(
      id = id, specs = DTAtools::create_example_DTAColumnSpecCollection(1), files = files
    )
  }
  DTAtools::DTA(datasets = list(
    mk("lb", list(ent("^lb.*[.]csv$"))),
    mk("lb_supp", list(ent("^lb_supp.*[.]csv$")))
  ))
}

test_that("conflict keys survive an entry inserted at position 1 of a dataset", {
  before <- lb_dta()
  keys <- app_fn("dta_conflict_keys")(before)
  expect_length(keys, 1)
  # The conflict is identified by the datasets and what they expect.
  expect_identical(keys, "lb ^lb.*[.]csv$ <> lb_supp ^lb_supp.*[.]csv$")

  after <- before
  ds <- after@datasets[["lb"]]
  extra <- DTAtools::DTAFileFactory(type = "csv", filename = "lb_other.csv")
  ds@files <- c(list(extra), ds@files)
  after@datasets[["lb"]] <- ds
  # The entry index of the lb side moved from 1 to 2 ...
  expect_identical(DTAtools::file_name_conflicts(before)$entry_a, 1L)
  expect_identical(DTAtools::file_name_conflicts(after)$entry_a, 2L)
  # ... but nothing new is reported.
  expect_identical(app_fn("dta_conflict_keys")(after), keys)
  expect_identical(nrow(app_fn("dta_new_conflicts")(before, after)), 0L)
})

test_that("dta_new_conflicts() returns the rows an edit introduced", {
  none <- ch_dta()
  new <- app_fn("dta_new_conflicts")(none, lb_dta())
  expect_identical(nrow(new), 1L)
  expect_identical(new$dataset_a, "lb")
  expect_match(app_fn("dta_conflict_refusal")(new), "lb_supp", fixed = TRUE)
})

# ---- writer: YAML ------------------------------------------------------------------

test_that("containers and entry references round-trip through the YAML writer", {
  dta <- ch_dta(
    refs = list(ae = "adam", dm = "none", vs = "x.zip"),
    containers = list(
      ch_ctr("sdtm", "STUDY_SDTM_*.zip", description = "All SDTM", default = TRUE),
      ch_ctr("adam", "adam.zip")
    )
  )
  lst <- app_fn("dta_to_list")(dta)
  expect_identical(names(lst)[seq_len(2)], c("containers", "datasets"))
  expect_identical(lst$containers[[1]], list(id = "sdtm", filename = "STUDY_SDTM_*.zip", description = "All SDTM", default = TRUE))
  expect_identical(lst$containers[[2]], list(id = "adam", filename = "adam.zip"))

  txt <- app_fn("dta_to_yaml_text")(dta)
  expect_true(txt$ok)
  expect_match(txt$value, "containers:", fixed = TRUE)
  f <- withr::local_tempfile(fileext = ".yaml")
  writeLines(txt$value, f)
  back <- suppressMessages(DTAtools::read_dta_from_yaml(f))
  expect_identical(names(back@containers), c("sdtm", "adam"))
  expect_identical(back@containers[["sdtm"]]@filename, "STUDY_SDTM_*.zip")
  expect_identical(back@containers[["sdtm"]]@description, "All SDTM")
  expect_true(back@containers[["sdtm"]]@default)
  expect_false(back@containers[["adam"]]@default)
  expect_identical(unname(ch_refs(back)), c("adam", "none", "x.zip"))
})

test_that("a document without containers writes no containers section", {
  expect_null(app_fn("dta_to_list")(ch_dta())$containers)
})

test_that("a standalone dataset is written with file-name references (plan 4.5)", {
  dta <- ch_dta(
    refs = list(ae = "adam", dm = "none"),
    containers = list(ch_ctr("adam", "adam.zip"), ch_ctr("sdtm", "sdtm_*.zip", default = TRUE))
  )
  f <- app_fn("dta_dataset_to_list")
  expect_identical(f(dta@datasets[["ae"]], dta)$files$container, "adam.zip")
  expect_identical(f(dta@datasets[["dm"]], dta)$files$container, "none")
  # The default becomes an explicit dotted reference ...
  expect_identical(f(dta@datasets[["vs"]], dta)$files$container, "sdtm_*.zip")
  # ... and without a document the stored reference is written as it is.
  expect_identical(f(dta@datasets[["ae"]])$files$container, "adam")
  expect_null(f(dta@datasets[["vs"]])$files$container)
  yml <- app_fn("dta_dataset_to_yaml_text")(dta, "vs")
  expect_match(yml$value, "container: sdtm_*.zip", fixed = TRUE)
})

# ---- check / report ----------------------------------------------------------------

test_that("dta_check() muffles the container warning and keeps the finding", {
  dta <- ch_dta(containers = list(ch_ctr("sdtm", "sdtm_*.zip", default = TRUE)))
  p <- cont_write("ae.csv", CONT_AE_CSV)
  dta <- DTAtools::load_file(dta, dataset = "ae", file = p)
  res <- expect_no_warning(app_fn("dta_check")(dta, "ae"))
  expect_true(res$ok)
  f <- app_fn("dta_container_findings")(res$value)
  expect_identical(f$status, "unverified")
  expect_length(app_fn("dta_container_messages")(res$value), 1L)
  expect_length(app_fn("dta_container_messages")(res$value, datasets = "dm"), 0L)
})

test_that("dta_container_messages() is empty without findings", {
  expect_identical(app_fn("dta_container_messages")(ch_dta()), character(0))
  expect_null(app_fn("dta_container_findings")(ch_dta()))
})

report_status <- function() list(ae = "pass", dm = "pass", vs = "pass")
finding <- function(status, msg = "ae.csv arrived loose") {
  data.frame(
    dataset = "ae", item = "ae.csv", file = "ae.csv", container = "sdtm_*.zip",
    arrived = "loose", status = status, message = msg, stringsAsFactors = FALSE
  )
}

test_that("the report banner fails on an outside finding and lists it", {
  html <- app_fn("dta_build_validation_report")(ch_dta(), report_status(), findings = finding("outside", "ae.csv <b>x</b>"))
  expect_match(html, "VALIDATION FAILED", fixed = TRUE)
  expect_match(html, "<h2>Delivery</h2>", fixed = TRUE)
  expect_match(html, "ae.csv &lt;b&gt;x&lt;/b&gt;", fixed = TRUE)
})

test_that("the report banner is INCOMPLETE on unverified or unrecorded findings only", {
  for (s in c("unverified", "unrecorded")) {
    html <- app_fn("dta_build_validation_report")(ch_dta(), report_status(), findings = finding(s))
    expect_match(html, "VALIDATION INCOMPLETE", fixed = TRUE)
    expect_no_match(html, "VALIDATION PASSED", fixed = TRUE)
    expect_match(html, "<h2>Delivery</h2>", fixed = TRUE)
  }
})

test_that("without findings the banner is PASSED and has no Delivery section", {
  html <- app_fn("dta_build_validation_report")(ch_dta(), report_status())
  expect_match(html, "VALIDATION PASSED", fixed = TRUE)
  expect_no_match(html, "<h2>Delivery</h2>", fixed = TRUE)
})

test_that("the report computes the findings itself when none are passed", {
  dta <- ch_dta(containers = list(ch_ctr("sdtm", "sdtm_*.zip", default = TRUE)))
  p <- cont_write("ae.csv", CONT_AE_CSV)
  dta <- DTAtools::load_file(dta, dataset = "ae", file = p)
  html <- app_fn("dta_build_validation_report")(dta, report_status())
  expect_match(html, "VALIDATION INCOMPLETE", fixed = TRUE)
})

# ---- lint consumers ------------------------------------------------------------------

test_that("the lint display helpers tolerate rows with an empty dataset", {
  f <- data.frame(
    dataset = c("", "ae"), object = c("", "file"), id = c(NA, "x"),
    severity = c("warning", "error"), code = c("container_unused", "c2"),
    message = c("Container unused.", "Bad."), stringsAsFactors = FALSE
  )
  expect_identical(app_fn("dta_finding_lines")(f), c("Container unused.", "ae / x: Bad."))
  html <- as.character(app_fn("dta_findings_ui")(f))
  expect_match(html, "Container unused.", fixed = TRUE)
  expect_match(html, "ae / x: Bad.", fixed = TRUE)
})

# ---- delivery wrapper and note ---------------------------------------------------------

plan_rows <- function(...) {
  base <- list(
    delivered = "d.zip", container = NA_character_, member = NA_character_, name = "ae.csv",
    dataset = "ae", handler = 1L, item = "ae.csv", status = "loaded", reason = NA_character_,
    conformant = NA, path = "p"
  )
  rows <- lapply(list(...), function(o) {
    r <- base
    r[names(o)] <- o
    as.data.frame(r, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

test_that("dta_load_delivery() forwards route and datasets", {
  seen <- NULL
  testthat::local_mocked_bindings(
    load_delivery = function(x, files, ..., route = NULL, datasets = NULL) {
      seen <<- list(route = route, datasets = datasets)
      x
    },
    .package = "DTAtools"
  )
  r <- list("a.csv" = "ae")
  res <- app_fn("dta_load_delivery")(ch_dta(), "p", tempdir(), 1e9, route = r, datasets = "ae")
  expect_true(res$ok)
  expect_identical(seen, list(route = r, datasets = "ae"))
})

test_that("dta_delivery_note() reports ambiguous and misfit files", {
  plan <- plan_rows(
    list(name = "ae.csv"),
    list(name = "dm.csv", dataset = "dm", item = "dm.csv", conformant = FALSE),
    list(name = "x.csv", dataset = NA_character_, item = NA_character_, status = "ambiguous", reason = "several")
  )
  note <- app_fn("dta_delivery_note")(plan)
  expect_identical(note$type, "warning")
  expect_match(note$text, "Not loaded (no dataset chosen): x.csv.", fixed = TRUE)
  expect_match(note$text, "Not inside its required container: dm.csv.", fixed = TRUE)
})

test_that("dta_delivery_note() works on a plan without the routing columns", {
  plan <- plan_rows(list(name = "ae.csv"))
  plan$conformant <- NULL
  note <- app_fn("dta_delivery_note")(plan)
  expect_identical(note$type, "message")
  expect_match(note$text, "loaded 1 file(s) into 1 dataset(s).", fixed = TRUE)
  expect_no_match(note$text, "Not inside", fixed = TRUE)
})

# ---- provenance ----------------------------------------------------------------------

test_that("removing a loaded file or all files drops its provenance", {
  dta <- ch_dta()
  dta <- DTAtools::load_file(dta, dataset = "ae", file = cont_write("ae.csv", CONT_AE_CSV))
  dta <- DTAtools::load_file(dta, dataset = "ae", file = cont_write("ae.csv", CONT_AE_CSV), name = "ae2")
  expect_setequal(names(dta@datasets[["ae"]]@provenance), names(dta@datasets[["ae"]]@tables))
  one <- app_fn("dta_unload_table")(dta, "ae", "ae2")$value
  expect_identical(length(one@datasets[["ae"]]@provenance), 1L)
  expect_false("ae2" %in% names(one@datasets[["ae"]]@provenance))
  none <- app_fn("dta_unload_all")(dta, "ae")$value
  expect_length(none@datasets[["ae"]]@provenance, 0)
})

test_that("a raw-YAML reconciliation carries the provenance of bound tables", {
  dta <- ch_dta()
  dta <- DTAtools::load_file(dta, dataset = "ae", file = cont_write("ae.csv", CONT_AE_CSV))
  old <- dta@datasets[["ae"]]
  fresh <- ch_dta()
  res <- app_fn("dta_transfer_bound_data")(fresh, "ae", old, keep_validation = FALSE)
  expect_true(res$ok)
  expect_identical(res$value@datasets[["ae"]]@provenance, old@provenance)
  expect_identical(res$value@datasets[["ae"]]@provenance[[1]]$via, "file")
})

# ---- review fixes (WP6a) -----------------------------------------------------------

test_that("a file routed to no dataset is listed as not loaded, not as not used", {
  plan <- plan_rows(
    list(name = "ae.csv"),
    list(name = "lb_supp1.csv", dataset = NA_character_, item = NA_character_, status = "unused", reason = "route sends it to no dataset"),
    list(name = "zz.csv", dataset = NA_character_, item = NA_character_, status = "unused", reason = "matches no file entry of the specification")
  )
  note <- app_fn("dta_delivery_note")(plan)
  expect_match(note$text, "Not loaded (no dataset chosen): lb_supp1.csv.", fixed = TRUE)
  expect_match(note$text, "Not used: zz.csv.", fixed = TRUE)
  expect_no_match(note$text, "route sends it", fixed = TRUE)
})

test_that("dta_delivery_note() survives a zero-row plan without `conformant`", {
  plan <- data.frame(
    name = character(0), status = character(0), reason = character(0),
    delivered = character(0), dataset = character(0), item = character(0)
  )
  note <- app_fn("dta_delivery_note")(plan)
  expect_identical(note$type, "message")
  expect_match(note$text, "loaded 0 file(s) into 0 dataset(s).", fixed = TRUE)
})

test_that("a derived id is never `none`, in any case", {
  f <- app_fn("dta_container_id_from_filename")
  expect_identical(f("None.zip"), "container_none")
  expect_identical(f("NONE.zip"), "container_none")
  res <- app_fn("dta_container_save")(ch_dta(), NULL, "None.zip", NULL, FALSE)
  expect_true(res$ok)
  expect_identical(res$key, "container_none")
})

test_that("dta_new_conflicts() flags a key whose count grew", {
  ent <- function(pat) DTAtools::DTAFileFactory(type = "csv", filename = pat, pattern = TRUE, pattern_description = "Lab files")
  mk <- function(e2) {
    ds <- function(id, files) {
      DTAtools::DTADataSetTabular(id = id, specs = DTAtools::create_example_DTAColumnSpecCollection(1), files = files)
    }
    DTAtools::DTA(datasets = list(
      ds("lb", list(ent("^lb.*[.]csv$"), ent(e2))),
      ds("lb_supp", list(ent("^lb_supp.*[.]csv$")))
    ))
  }
  before <- mk("^mb.*[.]csv$")
  after <- mk("^lb_s.*[.]csv$")
  # Entry 2 of lb now clashes with lb_supp as entry 1 already did: the same key,
  # twice.
  expect_identical(length(DTAtools::file_name_conflicts(before)$dataset_a), 1L)
  expect_identical(nrow(app_fn("dta_new_conflicts")(before, after)), 1L)
  expect_identical(nrow(app_fn("dta_new_conflicts")(after, after)), 0L)
})

test_that("dta_container_choices() labels an appended value through the containers", {
  dta <- ch_dta(containers = list(ch_ctr("sdtm", "sdtm.zip", default = TRUE), ch_ctr("adam", "adam.zip")))
  f <- app_fn("dta_container_choices")
  # An id that is not an option (the default's) -> its plain file name, so it is
  # not a second "(with all other files)" beside the empty choice.
  ch <- f(dta, current = "sdtm")
  expect_identical(unname(utils::tail(ch, 1)), "sdtm")
  expect_identical(names(utils::tail(ch, 1)), "sdtm.zip")
  expect_false(anyDuplicated(names(ch)) > 0)
  # A file name an explicit container carries -> that container's option, once.
  ch2 <- f(dta, current = "adam.zip")
  expect_identical(ch2, f(dta))
  expect_identical(sum(names(ch2) == "adam.zip"), 1L)
  expect_identical(app_fn("dta_container_normalise_ref")(dta, "adam.zip"), "adam")
  # `none` with no default container -> a readable label; unknown -> raw.
  plain <- ch_dta(containers = list(ch_ctr("adam", "adam.zip")))
  expect_identical(names(utils::tail(f(plain, current = "none"), 1)), "No container required")
  expect_identical(names(utils::tail(f(plain, current = "legacy.zip"), 1)), "legacy.zip")
})

test_that("a shared base is used as given, and equals the one built per select", {
  dta <- ch_dta(containers = list(ch_ctr("adam", "adam.zip")))
  ca <- DTAtools::container_assignments(dta)
  base <- app_fn("dta_container_choices_base")(dta, ca)
  f <- app_fn("dta_container_choices")
  # Not rebuilt: a marker base comes back as it went in, the value appended.
  expect_identical(names(f(dta, current = "legacy.zip", base = c(Marker = ""))), c("Marker", "legacy.zip"))
  expect_identical(f(dta, "legacy.zip", ca = ca, base = base), f(dta, "legacy.zip"))
})

test_that("a container name takes an archive also with .gz on it, and nothing else", {
  f <- app_fn("dv_glob_hits")
  expect_identical(
    f("x.xpt", c("x.xpt", "x.xpt.gz", "X.xpt", "x.xpt.zip", "ax.xpt")),
    c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  expect_identical(f("STUDY_*.zip", c("STUDY_1.zip", "STUDY_.zip", "study_1.zip")), c(TRUE, TRUE, FALSE))
  expect_identical(f("a?.zip", c("ab.zip", "a.zip", "a.b.zip")), c(TRUE, FALSE, FALSE))
  expect_identical(f("a+b.zip", c("a+b.zip", "aab.zip")), c(TRUE, FALSE))
})

test_that("NA in an assignment keeps the stored reference; only \"\" clears it", {
  dta <- ch_dta(refs = list(ae = "keep.zip", dm = "adam", vs = "none"), containers = list(ch_ctr("adam", "adam.zip")))
  a <- data.frame(dataset = c("ae", "dm", "vs"), handler = 1L, value = c(NA, "", NA), stringsAsFactors = FALSE)
  res <- app_fn("dta_container_assign")(dta, a)
  expect_true(res$ok)
  expect_identical(unname(ch_refs(res$value)), c("keep.zip", NA, "none"))
})

test_that("a container error does not show the derived id", {
  res <- app_fn("dta_container_save")(ch_dta(), NULL, "sdtm.tar", NULL, FALSE)
  expect_false(res$ok)
  expect_no_match(res$error, "^Container '")
})

test_that("dta_container_save() requires the description and the default flag", {
  # No default, so a caller cannot drop them by accident (judged on the
  # signature, never on R's translated missing-argument text).
  fm <- formals(app_fn("dta_container_save"))
  expect_true(identical(fm$description, quote(expr = )))
  expect_true(identical(fm$default, quote(expr = )))
})
