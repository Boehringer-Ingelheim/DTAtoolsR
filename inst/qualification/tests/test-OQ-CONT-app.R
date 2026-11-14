# Deliveries and containers in the interactive application. REQ-CONT-016,
# REQ-CONT-017, REQ-CONT-030, REQ-CONT-031 and the application clause of
# REQ-CONT-032.
#
# Driven through shiny::testServer(), which runs the real server function
# without a browser, so every input name below is a contract with the
# application (maintaining.md, "You changed the Shiny app"). What is checked is
# what a user could observe: the records the workspace holds, the files that
# are on disk, what the notifications and the dialogs say, and the verdict the
# report states.
#
# Every expected value is written down from the requirement text, from the
# plan's wording of what the user is told, and from the literal data of the
# fixtures (helper-container.R), never captured from a run. sdtm_csv.zip holds
# ae.csv (53 bytes), dm.csv (30 bytes), define.xml, docs/readme.txt, and two
# entries the delivery never binds (__MACOSX/._ae.csv, .DS_Store), so a
# delivery of it writes 83 bytes for ae and dm. The session budget is the
# environment variable DTATOOLS_SESSION_DELIVERY_BYTES; with 100 bytes, one
# 53-byte extraction fits and a second one of 83 bytes does not, until the
# first is deleted.
#
# Notes on the harness. testServer() evaluates the test block against a copy of
# the server environment, so a plain local such as `owned` is frozen at the
# value it had before the first upload; it is read through session$env, the live
# server environment. Every file a case drops is written before testServer()
# starts: files written inside the block would be deleted with an environment
# that never exits, after the case. A "nothing is left on disk" step always
# names a path that existed before the action, so it fails if the application
# leaves the file behind. An output nobody reads is never rendered under
# testServer(), so every dialog and view a case asserts on is read through
# ap_text(). testServer() has no browser: an input the browser would have
# created (the container select of a form, the choice of a dialog row) is set
# explicitly, as the browser would report it.
#
# The three "warning: package 'shiny' was built under R version" expectations
# that the first case of this file records on a machine whose R is older than
# the one the packages were built under come from library(shiny) in the
# application's own app.R. They are reported as warnings, not as failures.
#
# LIM-014 (extracted members stay on disk while they are used) is the reason
# the cleanup below is the application's job.

# ---- local helpers --------------------------------------------------------------

# The delivery directories the session still owns and that exist.
ap_deliveries <- function(session) {
  u <- session$env$owned$upload
  u[startsWith(basename(u), "dta_upload_delivery_") & dir.exists(u)]
}

# The delivery directory a path lies under.
ap_root <- function(path) {
  if (!length(path)) {
    return(character(0))
  }
  while (!startsWith(basename(path), "dta_upload_delivery_") && dirname(path) != path) path <- dirname(path)
  path
}

# The HTML of a rendered output as one string, and with its white space collapsed.
ap_text <- function(out) if (is.null(out) || is.null(out$html)) "" else paste(as.character(out$html), collapse = "")
ap_flat <- function(x) gsub("\\s+", " ", x)

# Set one input whose id is only known at run time.
ap_set <- function(session, id, value) do.call(session$setInputs, stats::setNames(list(value), id))

# The lines of a YAML document, written to a file of its own.
ap_yaml <- function(lines, env = parent.frame()) {
  f <- file.path(qa_tempdir(env), "spec.yaml")
  writeLines(lines, f)
  f
}

AP_META <- c("metadata:", "  title: Delivery test", "  version: \"1\"")
AP_AE_COLS <- c(
  "      - {id: USUBJID, type: SAS Char}", "      - {id: AETERM, type: SAS Char}", "      - {id: AESEQ, type: SAS Num}"
)
AP_DM_COLS <- c("      - {id: USUBJID, type: SAS Char}", "      - {id: AGE, type: SAS Num}")
AP_VS_COLS <- c("      - {id: USUBJID, type: SAS Char}", "      - {id: VSSTRESN, type: SAS Num}")

ap_ds <- function(id, files, cols) {
  c(paste0("  - id: ", id), "    type: tabular", paste0("    files: ", files), "    columns:", cols)
}

# A document of the datasets ae and dm; `containers` are the lines of the
# document-level section, `extra` further dataset lines.
ap_doc <- function(containers = character(), ae = "{filename: ae.csv, type: csv}",
                   dm = "{filename: dm.csv, type: csv}", extra = character(), env = parent.frame()) {
  ap_yaml(
    c(AP_META, containers, "datasets:", ap_ds("ae", ae, AP_AE_COLS), ap_ds("dm", dm, AP_DM_COLS), extra),
    env
  )
}

ap_spec <- function(env = parent.frame()) ap_doc(env = env)

# The document in which the entry of ae declares `container: sdtm_csv.zip`.
ap_spec_container <- function(env = parent.frame()) {
  ap_doc(ae = "{filename: ae.csv, type: csv, container: sdtm_csv.zip}", env = env)
}

# The same document with a default container, which every entry without a
# reference of its own then has to arrive inside.
AP_DEFAULT <- c("containers:", "  - id: dlv", "    filename: delivery.zip", "    default: true")

# Two pattern entries that both take ae.csv: a file that matches several datasets.
ap_tie_spec <- function(env = parent.frame()) {
  ap_doc(
    ae = "{filename: \"^a.*[.]csv$\", type: csv, pattern: true}",
    dm = "{filename: \"^.*e[.]csv$\", type: csv, pattern: true}",
    env = env
  )
}

AP_SDTM <- c("containers:", "  - id: sdtm", "    filename: STUDY_SDTM_*.zip", "    description: SDTM data")
AP_TWO <- c(
  "containers:", "  - id: sdtm", "    filename: STUDY_SDTM_*.zip", "    default: true",
  "  - id: adam", "    filename: STUDY_ADAM_*.zip"
)
ap_xpt <- function(re) sprintf("{filename: '%s', type: xpt, pattern: true}", re)

# The reference entry `i` of dataset `ds` stores, NA when it has none.
ap_ref <- function(dta, ds, i = 1) {
  r <- DTAtools::datasets(dta, ds)@files[[i]]@container
  if (is.null(r)) NA_character_ else r
}

# Every input the file editor's form renders except the container select.
ap_file_form <- function(session, filename, type = "csv", pattern = FALSE) {
  session$setInputs(
    file_filename = filename, file_type = type, file_pattern = pattern,
    file_count_mode = "exact", file_number_of_files = 1,
    file_min_number_of_files = 1, file_max_number_of_files = 1,
    file_pattern_description = "", file_info = "", file_extensions = ""
  )
}

# shiny::testServer() with one condition handled: the application lints a
# document when it reads or saves it and signals the findings as a warning
# ("The DTA specification has n warning(s)"). Several cases use documents that
# deliberately carry such a finding (an unused container, an overlap), and
# whether the application reports it to the user is asserted where it is the
# subject (OQ-CONT-062), so here it is muffled rather than left to show up as a
# warning of the case. No other condition is touched.
ap_serve <- function(app, expr) {
  q <- rlang::enquo(expr)
  withCallingHandlers(
    shiny::testServer(app, !!q),
    warning = function(w) {
      if (grepl("^The DTA specification has", conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )
}

# A copy of a fixture at another path: testServer() fires an observer only when
# the value of an input changes.
ap_copy <- function(fixture, env = parent.frame()) cont_write(fixture, cont_bytes(fixture), env = env)

# Capture every notification the application shows into `shown` of the caller.
ap_notes <- function(env = parent.frame()) {
  env$shown <- character(0)
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) {
      env$shown <- c(env$shown, paste(as.character(ui), collapse = ""))
      invisible(NULL)
    },
    .package = "shiny", .env = env
  )
}

# Capture the HTML of every dialog the application shows into `modals`.
ap_modals <- function(env = parent.frame()) {
  env$modals <- character(0)
  testthat::local_mocked_bindings(
    showModal = function(ui, ...) {
      env$modals <- c(env$modals, ap_flat(paste(as.character(ui), collapse = "")))
      invisible(NULL)
    },
    .package = "shiny", .env = env
  )
}

# Record the tab the server selects instead of sending it to a browser.
ap_tabs <- function(env = parent.frame()) {
  env$selected <- character(0)
  testthat::local_mocked_bindings(
    nav_select = function(id, selected = NULL, ...) {
      env$selected <- c(env$selected, paste(id, selected, sep = ":"))
      invisible(NULL)
    },
    .package = "bslib", .env = env
  )
}

# ---- cases --------------------------------------------------------------------------

test_that("OQ-CONT-025 | a zip dropped on a dataset's zone loads only that dataset; the Delivery zone loads the whole delivery, asks before replacing, cleans up and frees the session budget | REQ-CONT-016", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()
  ap_notes()

  # One archive on the ae slot binds only ae's file, and the extraction is the
  # session's to delete. The notice says what was bound and how many files of
  # the archive were left: dm.csv, define.xml and docs/readme.txt.
  spec <- ap_spec()
  zip0 <- cont_fx("sdtm_csv.zip")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(zip0, "sdtm_csv.zip"))

    qa_step("a drop on the ae slot binds a file to ae only", "ae||1", names(rv$uploads))
    qa_step("and dm holds no table", 0L, length(tables(datasets(rv$dta, "dm"))))
    qa_step(
      "the notice names what was bound and counts what was left",
      "Loaded only ae's files from sdtm_csv.zip (ae.csv). Its other 3 files were not loaded.",
      utils::tail(shown, 1)
    )
    qa_step("the ae record names its archive", "sdtm_csv.zip", rv$uploads[["ae||1"]][[1]]$container)
  })

  # The Delivery zone loads every dataset the archive feeds, and the extraction
  # is the session's to delete.
  spec <- ap_spec()
  zip1 <- cont_fx("sdtm_csv.zip")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = cont_fi(zip1, "sdtm_csv.zip"))

    qa_step("a drop on the Delivery zone binds a file to ae and to dm", c("ae||1", "dm||1"), sort(names(rv$uploads)))
    rec <- rv$uploads[["dm||1"]][[1]]
    qa_step(
      "the dm record names its table and the archive it came from",
      list(table = "dm", container = "sdtm_csv.zip"),
      list(table = rec$table, container = rec$container)
    )
    dirs <- ap_deliveries(session)
    qa_step("one delivery directory is owned by the session", 1L, length(dirs))
    qa_check(
      "the dm file lies under it and exists",
      startsWith(normalizePath(rec$path, winslash = "/"), normalizePath(dirs, winslash = "/")) && file.exists(rec$path)
    )
    qa_step("dm holds its table", "dm", names(tables(datasets(rv$dta, "dm"))))
    owned <- session$env$owned$upload
    staged <- owned[!startsWith(basename(owned), "dta_upload_delivery_")]
    qa_check("the archive was staged for the delivery", length(staged) >= 1)
    qa_step("and the staged copy of the archive is gone", FALSE, any(file.exists(file.path(staged, "sdtm_csv.zip"))))
  })

  # A second delivery asks first. Cancel keeps the old data and deletes the
  # whole new extraction.
  spec <- ap_spec()
  zip2 <- ap_copy("sdtm_csv.zip")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = cont_fi(zip1, "sdtm_csv.zip"))
    session$setInputs(delivery_files = cont_fi(zip2, "sdtm_csv.zip"))

    qa_check("a second delivery over loaded tables is held pending the user's answer", !is.null(rv$pending_upload$delivery))
    x <- rv$pending_upload$delivery$exdir
    copies <- rv$pending_upload$delivery$copies
    qa_step(
      "the pending delivery's extraction and the staged copy exist",
      list(extraction = TRUE, copies = 1L, copy_exists = TRUE),
      list(extraction = dir.exists(x), copies = length(copies), copy_exists = all(file.exists(copies)))
    )
    session$setInputs(cancel_overwrite = 1)
    qa_step("Cancel: nothing is pending any more", TRUE, is.null(rv$pending_upload))
    qa_step("Cancel: the extraction that existed is deleted", FALSE, dir.exists(x))
    qa_step("Cancel: so is the staged copy that existed", FALSE, any(file.exists(copies)))
    qa_step("Cancel: the old data is kept", 1L, length(rv$uploads[["ae||1"]]))
  })

  # Replace commits the new delivery and deletes the superseded extraction.
  spec <- ap_spec()
  zip3 <- ap_copy("sdtm_csv.zip")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = cont_fi(zip1, "sdtm_csv.zip"))
    first <- ap_deliveries(session)
    old <- rv$uploads[["ae||1"]][[1]]$path
    qa_check("the first delivery's ae file exists", file.exists(old))
    session$setInputs(delivery_files = cont_fi(zip3, "sdtm_csv.zip"))
    session$setInputs(confirm_overwrite = 1)

    qa_step("Replace: ae has one record", 1L, length(rv$uploads[["ae||1"]]))
    qa_step("Replace: the superseded file that existed is deleted", FALSE, file.exists(old))
    now <- ap_deliveries(session)
    qa_step("Replace: one delivery directory remains", 1L, length(now))
    qa_step("and it is not the first one", FALSE, identical(now, first))
    qa_step(
      "and holds the new delivery's two files",
      c("ae.csv", "dm.csv"),
      sort(basename(list.files(now, recursive = TRUE)))
    )
  })

  # A hostile archive binds nothing.
  spec <- ap_spec()
  hostile <- cont_fx("unsafe_dotdot.zip")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(hostile, "unsafe_dotdot.zip"))
    qa_step("a hostile archive binds no file", 0L, length(rv$uploads))
    qa_step("and ae holds no table", 0L, length(tables(datasets(rv$dta, "ae"))))
  })

  # A delivery that fails after extraction began leaves no directory.
  spec <- ap_spec()
  lying <- cont_fx("lying_size.zip")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(lying, "lying_size.zip"))
    qa_step("an archive refused while it is extracted binds no file", 0L, length(rv$uploads))
    qa_step("and leaves no extraction directory behind", 0L, length(ap_deliveries(session)))
  })

  # The session budget: removing or discarding files frees it.
  withr::local_envvar(DTATOOLS_SESSION_DELIVERY_BYTES = "100")
  spec <- ap_spec()
  a1 <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  a2 <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(a1, "a.zip"))
    qa_step("a 53-byte extraction is held by the session", 1L, length(ap_deliveries(session)))
    session$setInputs(discard_all = 1)
    session$setInputs(confirm_discard_all = 1)
    qa_step("discarding the dataset's files deletes the extraction", 0L, length(ap_deliveries(session)))
    session$setInputs(up_1_1 = cont_fi(a2, "a.zip"))
    qa_step("so the same 53 bytes fit in a budget of 100 again", 1L, length(rv$uploads[["ae||1"]]))
    qa_step("in an extraction of its own", 1L, length(ap_deliveries(session)))
  })

  # One delivery that filled two datasets: discarding one frees exactly its files.
  spec <- ap_spec()
  a1 <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  a2 <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  b1 <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "b.zip")
  b2 <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "b.zip")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(a1, "a.zip"))
    session$setInputs(delivery_files = cont_fi(b1, "b.zip"))
    qa_step("83 bytes against the 47 that are left are refused: only ae holds a file", "ae||1", names(rv$uploads))
    qa_check(
      "and the notification says what to do",
      grepl("remove or discard", utils::tail(shown, 1), fixed = TRUE)
    )

    session$setInputs(discard_all = 1)
    session$setInputs(confirm_discard_all = 1)
    session$setInputs(delivery_files = cont_fi(b2, "b.zip"))
    qa_step("after ae's files are discarded the delivery fits and fills both datasets", c("ae||1", "dm||1"), sort(names(rv$uploads)))
    ae_file <- rv$uploads[["ae||1"]][[1]]$path
    dm_file <- rv$uploads[["dm||1"]][[1]]$path
    qa_step("both files are in the delivery", TRUE, file.exists(ae_file) && file.exists(dm_file))

    session$setInputs(discard_all = 2)
    session$setInputs(confirm_discard_all = 2)
    qa_step("discarding ae again deletes ae's file", FALSE, file.exists(ae_file))
    qa_step("and keeps dm's, which a record still names", TRUE, file.exists(dm_file))
    session$setInputs(up_1_1 = cont_fi(a2, "a.zip"))
    qa_step("the 70 bytes then free hold ae's 53 again", "ae", names(tables(datasets(rv$dta, "ae"))))
  })

  # An extraction whose dataset was removed is deleted before the next delivery
  # is counted against the budget: 57 bytes are more than the 47 left while
  # ae's 53 still count.
  spec <- ap_spec()
  a <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  b <- cont_zip(list(dm.csv = paste0(CONT_DM_CSV, "S-003,40\nS-004,22\nS-005,63\n")), name = "b.zip")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(a, "a.zip"))
    held <- ap_deliveries(session)
    qa_step("ae's 53-byte extraction is held", 1L, length(held))
    qapp_unlock_editing(session)
    session$setInputs(remove_dataset = 1)
    session$setInputs(remove_dataset_confirm = 1)
    qa_step("ae is removed from the specification", "dm", names(rv$structure))
    session$setInputs(up_1_1 = cont_fi(b, "b.zip"))
    qa_step(
      "a 57-byte delivery is accepted, because the removed dataset's extraction no longer counts",
      "dm",
      names(tables(datasets(rv$dta, "dm")))
    )
    qa_step("and the extraction of the removed dataset was deleted", FALSE, any(dir.exists(held)))
  })

  # Removing the one file of a delivery deletes its extraction.
  spec <- ap_spec()
  a <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "a.zip")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(a, "a.zip"))
    held <- ap_deliveries(session)
    qa_step("the delivery is held", 1L, length(held))
    session$setInputs(rmfile_1 = 1)
    qa_step("removing its file empties the record", 0L, length(rv$uploads[["ae||1"]]))
    qa_step("and deletes the extraction that existed", FALSE, any(dir.exists(held)))
  })

  # Session end keeps only the files a file dataset uses: "Restore previous
  # session" re-reads file datasets from their files.
  spec <- ap_doc(extra = c(
    "  - id: define", "    type: file", "    files: {filename: define.xml, type: any}"
  ))
  d_zip <- cont_zip(list(ae.csv = CONT_AE_CSV, define.xml = "<define/>\n"), name = "d.zip")
  ae_path <- NULL
  define_path <- NULL
  withr::defer(unlink(ap_root(define_path), recursive = TRUE))
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = cont_fi(d_zip, "d.zip"))
    ae_path <<- rv$uploads[["ae||1"]][[1]]$path
    define_path <<- rv$uploads[["define||1"]][[1]]$path
    qa_step("one delivery filled both the table and the file dataset", TRUE, file.exists(ae_path) && file.exists(define_path))
    session$close()
  })
  qa_step("at session end the table's extracted file is deleted", FALSE, file.exists(ae_path))
  qa_step("and the file dataset's is kept", TRUE, file.exists(define_path))
})

test_that("OQ-CONT-026 | the file editor offers Delivered inside as a select, keeps what an entry stores, creates a typed archive and refuses a bad name | REQ-CONT-017", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()
  ap_notes()

  # ae declares the container legacy.zip by its file name, which no defined
  # container carries: an implied container. The document defines `sdtm`.
  spec <- ap_doc(AP_SDTM, ae = "{filename: ae.csv, type: csv, container: legacy.zip}")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    qapp_unlock_editing(session)
    session$setInputs(edit_files = 1)
    session$setInputs(file_edit_click = 1)
    html <- ap_text(output$file_modal_body)

    qa_check("the field is a select", grepl('id="file_container"', html, fixed = TRUE))
    qa_check("it offers the defined container under its file name and stores its id", grepl('<option value="sdtm">STUDY_SDTM_*.zip</option>', html, fixed = TRUE))
    qa_check("it offers the implied container the entry itself names", grepl('<option value="legacy.zip"', html, fixed = TRUE))
    qa_check("and the choice that leaves the entry without a reference of its own", grepl("Loose or in any container", html, fixed = TRUE))
    qa_check("the empty choice survives selectize, which drops it unless told to keep it", grepl('"allowEmptyOption":true', html, fixed = TRUE))
    qa_check("a typed archive name is allowed", grepl('"create":true', html, fixed = TRUE))
    qa_check("a tabular dataset is offered the type xpt", "xpt" %in% qapp_fn("dta_handler_types")("tabular"))

    # The browser did not report the select: the stored reference is kept.
    ap_file_form(session, "ae.csv")
    session$setInputs(file_save = 1)
    qa_step("a save that the select did not report keeps the stored reference", "legacy.zip", ap_ref(rv$dta, "ae"))

    # The file name of a defined container, typed, is that container's id.
    session$setInputs(file_edit_click = 2)
    session$setInputs(file_edit_click = 1)
    session$setInputs(file_container = "STUDY_SDTM_*.zip")
    session$setInputs(file_save = 2)
    qa_step("a defined container's file name typed is saved as its id", "sdtm", ap_ref(rv$dta, "ae"))
    qa_step("and creates no second container", "sdtm", names(rv$dta@containers))

    # A new archive name typed creates a container and references it.
    session$setInputs(file_edit_click = 2)
    session$setInputs(file_edit_click = 1)
    session$setInputs(file_container = "adam.zip")
    session$setInputs(file_save = 3)
    qa_step(
      "a new archive name typed creates a container with that file name",
      list(ids = c("adam", "sdtm"), filename = "adam.zip"),
      list(ids = sort(names(rv$dta@containers)), filename = rv$dta@containers$adam@filename)
    )
    qa_step("and the entry references it", "adam", ap_ref(rv$dta, "ae"))
    yaml_text <- paste(qapp_fn("dta_to_yaml_text")(rv$dta)$value, collapse = "\n")
    qa_check("the exported YAML writes the container section", grepl("filename: adam.zip", yaml_text, fixed = TRUE))
    qa_check("and the reference", grepl("container: adam", yaml_text, fixed = TRUE))

    # A defined container's id chosen is stored as it is; the empty choice clears.
    session$setInputs(file_edit_click = 2)
    session$setInputs(file_edit_click = 1)
    session$setInputs(file_container = "sdtm")
    session$setInputs(file_save = 4)
    qa_step("a choice is stored as it is", "sdtm", ap_ref(rv$dta, "ae"))
    session$setInputs(file_edit_click = 2)
    session$setInputs(file_edit_click = 1)
    session$setInputs(file_container = "")
    session$setInputs(file_save = 5)
    qa_step("the empty choice leaves the entry without a reference", NA_character_, ap_ref(rv$dta, "ae"))
  })

  # A name that cannot be a container changes nothing.
  spec <- ap_spec()
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    qapp_unlock_editing(session)
    session$setInputs(edit_files = 1)
    session$setInputs(file_edit_click = 1)
    ap_text(output$file_modal_body)
    before <- rv$dta
    ap_file_form(session, "ae.csv")
    session$setInputs(file_container = "x.tar")
    session$setInputs(file_save = 1)
    qa_step("a typed name that is no .zip or .xpt file name is refused", FALSE, rv$file_msg$ok)
    qa_check(
      "with a message that says what a container must be named",
      grepl("must name a .zip archive or a .xpt SAS transport file", rv$file_msg$error, fixed = TRUE)
    )
    qa_check("and nothing was saved", identical(rv$dta, before))
  })

  # A new entry of type xpt keeps its container, derived and defined from the
  # typed name, and writes both to the YAML.
  spec <- ap_spec()
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    qapp_unlock_editing(session)
    session$setInputs(edit_files = 1)
    session$setInputs(file_add = 1)
    ap_file_form(session, "lb.xpt", type = "xpt")
    session$setInputs(file_container = "lb_set.xpt")
    session$setInputs(file_save = 1)

    qa_step("the save is accepted", TRUE, is.null(rv$file_msg))
    handler <- datasets(rv$dta, "ae")@files[[2]]
    qa_step(
      "the new entry is a transport-file handler that references the container made from the typed name",
      list(xpt = TRUE, container = "lb_set", filename = "lb_set.xpt"),
      list(
        xpt = inherits(handler, "DTAtools::DTAFileXPT"), container = handler@container,
        filename = rv$dta@containers$lb_set@filename
      )
    )
    yaml_text <- paste(qapp_fn("dta_to_yaml_text")(rv$dta)$value, collapse = "\n")
    qa_check("the exported YAML writes the type", grepl("type: xpt", yaml_text, fixed = TRUE))
    qa_check("and the container the entry references", grepl("container: lb_set", yaml_text, fixed = TRUE))
  })
})

test_that("OQ-CONT-056 | a loose file on a dataset's zone is the individual check, a mixed drop is one delivery for that dataset, and an archive without its files says so | REQ-CONT-016", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()
  ap_notes()
  ap_modals()

  # A loose file for an entry that declares a container is bound: it is checked
  # on its own, and the notice says what the agreement requires.
  spec_c <- ap_spec_container()
  ae1 <- cont_write("ae.csv", CONT_AE_CSV)
  ae2 <- cont_write("ae.csv", CONT_AE_CSV)
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec_c, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(ae1, "ae.csv"))
    qa_step("a loose ae.csv for an entry declared inside sdtm_csv.zip is bound", 1L, length(rv$uploads[["ae||1"]]))
    qa_check(
      "and the notice says it was checked on its own and what the agreement requires",
      grepl(
        "Loaded ae.csv on its own for checking. The agreement requires it inside sdtm_csv.zip, so this is not yet a conforming delivery.",
        utils::tail(shown, 1),
        fixed = TRUE
      )
    )
    qa_step("the packaging is recorded as unverified, not as outside", "unverified", findings_now()$status)

    session$setInputs(up_1_1 = cont_fi(ae2, "ae.csv"))
    qa_check("a second loose file asks before it replaces the first", !is.null(rv$pending_upload))
  })

  # A drop that holds a container and a loose file is one delivery for the
  # dataset of the zone: the loose file is dm's, so it is not loaded either.
  spec <- ap_spec()
  zip1 <- cont_fx("sdtm_csv.zip")
  dm <- cont_write("dm.csv", CONT_DM_CSV)
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(c(zip1, dm), c("sdtm_csv.zip", "dm.csv")))
    qa_step("only ae is bound from a mixed drop on ae's zone", "ae||1", names(rv$uploads))
    qa_step("and dm holds no table", 0L, length(tables(datasets(rv$dta, "dm"))))
    qa_step(
      "the archive and the loose file travelled as one delivery",
      c("sdtm_csv.zip", "dm.csv"),
      unique(rv$last_delivery$plan$delivered)
    )
    qa_check(
      "and the notice says the loose file was judged as delivered",
      grepl("judged as delivered", utils::tail(shown, 1), fixed = TRUE)
    )
    qa_step("a drop on a dataset's zone does not take over the main area", "dataset", rv$main_view)
  })

  # An archive that holds nothing for the dataset loads nothing, and says so.
  spec <- ap_spec()
  dm_only <- cont_zip(list(dm.csv = CONT_DM_CSV), name = "dm_only.zip")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(dm_only, "dm_only.zip"))
    qa_step("nothing is bound", 0L, length(rv$uploads))
    qa_step(
      "and the notice says the archive holds no file for the dataset and names the expected name",
      "dm_only.zip holds no file for ae (expected ae.csv). Nothing was loaded.",
      utils::tail(shown, 1)
    )
  })

  # An archive that the dataset's own entry names is the file it is.
  spec <- ap_yaml(c(
    AP_META, "datasets:",
    "  - id: archive", "    type: file", "    files: {filename: sdtm.zip, type: any}"
  ))
  archive <- cont_zip(list(ae.csv = CONT_AE_CSV), name = "sdtm.zip")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(archive, "sdtm.zip"))
    qa_step("an archive that an entry names as its file is bound as that file", 1L, length(rv$uploads[["archive||1"]]))
    qa_step("under its own name", "sdtm.zip", basename(datasets(rv$dta, "archive")@file_paths))
    qa_step("and is not a delivery", TRUE, is.null(rv$last_delivery))
  })
})

test_that("OQ-CONT-057 | the Delivery zone is offered for every document with a dataset, loads the whole delivery and opens a Delivery view that names every file | REQ-CONT-030", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()
  ap_notes()
  ap_tabs()

  # The zone is offered for every document with a dataset: one plain dataset,
  # two datasets, one dataset with a container.
  one_container <- ap_yaml(c(
    AP_META, AP_DEFAULT, "datasets:", ap_ds("ae", "{filename: ae.csv, type: csv}", AP_AE_COLS)
  ))
  two <- ap_spec()
  ap_serve(app, {
    qa_step("no document is offered no Delivery zone", FALSE, output$delivery_zone)
    session$setInputs(dta_file = qapp_file_input("clinical_dta.yaml"))
    qa_step("a document with one dataset and no container has one dataset", 1L, length(rv$structure))
    qa_step("and is offered the Delivery zone", TRUE, output$delivery_zone)
  })
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(two, "spec.yaml"))
    qa_step("a document with two datasets is offered the Delivery zone", TRUE, output$delivery_zone)
  })
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(one_container, "spec.yaml"))
    qa_step("so is a document with one dataset and a container", TRUE, output$delivery_zone)
  })

  # A drop binds every dataset, records the delivery and opens the view. vs
  # expects vs.csv inside sdtm_csv.zip, which the archive does not hold.
  spec <- ap_doc(extra = ap_ds("vs", "{filename: vs.csv, type: csv, container: sdtm_csv.zip}", AP_VS_COLS))
  zip1 <- cont_fx("sdtm_csv.zip")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    qa_step("the main area shows a dataset before the drop", "dataset", rv$main_view)
    session$setInputs(delivery_files = cont_fi(zip1, "sdtm_csv.zip"))

    plan <- rv$last_delivery$plan
    qa_step("the datasets the archive feeds hold a file each", c("ae||1", "dm||1"), sort(names(rv$uploads)))
    qa_step("the delivery records the files it loaded", c("ae.csv", "dm.csv"), sort(plan$name[plan$status == "loaded"]))
    qa_step("and the file the document expects and the archive does not hold", "vs.csv", rv$last_delivery$missing)
    qa_step("the main area switches to the Delivery view", "delivery", rv$main_view)
    qa_step("on the Datasets tab, whichever tab was open", "main_tabs:Datasets", utils::tail(selected, 1))

    view <- ap_text(output$dataset_detail)
    qa_check("the view is headed Delivery", grepl("<h4>Delivery</h4>", view, fixed = TRUE))
    qa_check("it names the loaded files", grepl("ae.csv", view, fixed = TRUE) && grepl("dm.csv", view, fixed = TRUE))
    qa_check("and the archive they came inside", grepl("Inside sdtm_csv.zip", view, fixed = TRUE))
    qa_check("the packaging conforms: nothing arrived outside its container", grepl("Packaging conforms", view, fixed = TRUE))
    qa_check("the missing file is named", grepl("Not in the delivery: vs.csv.", view, fixed = TRUE))
    qa_check(
      "a file of the archive that no entry takes is named, with the reason",
      grepl("define.xml", view, fixed = TRUE) && grepl("no matching entry", view, fixed = TRUE)
    )
    qa_check("the view offers Check all", grepl('id="check_all_delivery"', view, fixed = TRUE))
    qa_check("no R argument reaches the user", !grepl("route =", view, fixed = TRUE))
  })

  # Check all validates every dataset; a click on a dataset leaves the view and
  # the sidebar line reopens it.
  spec <- ap_spec()
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = cont_fi(zip1, "sdtm_csv.zip"))
    session$setInputs(check_all_delivery = 1)
    qa_step("Check all validates every dataset the delivery fed", c(ae = "pass", dm = "pass"), unlist(rv$status))

    ap_set(session, paste0("selds_", rv$structure[["dm"]]$index), 1)
    qa_step("a click on a dataset leaves the view", c("dataset", "dm"), c(rv$main_view, rv$active))
    session$setInputs(delivery_details = 1)
    qa_step("the sidebar's Last delivery line reopens it", "delivery", rv$main_view)
    ap_set(session, paste0("selds_", rv$structure[["dm"]]$index), 2)
    qa_step("a click on the dataset that is already active leaves it again", "dataset", rv$main_view)
  })

  # With a default container the sidebar block says what the delivery did to
  # each container, and the view names the verdict.
  spec <- ap_doc(AP_DEFAULT)
  good <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "delivery.zip")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    panel <- ap_text(output$delivery_panel)
    qa_check("before a delivery the block says the container is not delivered yet", grepl("delivery.zip: not delivered yet", panel, fixed = TRUE))
    qa_check("and offers the container manager", grepl("Containers…", panel, fixed = TRUE))
    qa_check("it names no last delivery", !grepl("Last delivery", panel, fixed = TRUE))
    qa_step("the view says nothing was delivered", TRUE, {
      rv$main_view <- "delivery"
      session$flushReact()
      grepl("Nothing delivered yet", ap_text(output$dataset_detail), fixed = TRUE)
    })

    session$setInputs(delivery_files = cont_fi(good, "delivery.zip"))
    panel <- ap_text(output$delivery_panel)
    qa_check("a delivery inside the container reads as received", grepl("delivery.zip: 2 files", panel, fixed = TRUE))
    qa_check("with the last delivery counted", grepl("2 files loaded, 0 unmatched", panel, fixed = TRUE))
    qa_check("and a link to the details", grepl('id="delivery_details"', panel, fixed = TRUE))
  })
  other <- cont_fx("sdtm_csv.zip")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = cont_fi(other, "sdtm_csv.zip"))
    qa_check(
      "files that arrived inside another archive are counted as not inside the container",
      grepl("delivery.zip: 2 files not inside it", ap_text(output$delivery_panel), fixed = TRUE)
    )
    view <- ap_text(output$dataset_detail)
    qa_check("and the view says the delivery does not conform", grepl("Not conforming", view, fixed = TRUE))
    qa_check("and where each file came from", grepl("inside sdtm_csv.zip", view, fixed = TRUE))
  })
})

test_that("OQ-CONT-058 | a file that matches several datasets is asked about with explicit choices, answers decide where it goes, Cancel loads nothing and a second round keeps the first answers | REQ-CONT-030", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()
  ap_notes()
  ap_modals()

  # ae.csv matches both patterns: ^a.*[.]csv$ of ae and ^.*e[.]csv$ of dm.
  spec <- ap_tie_spec()
  ae <- cont_write("ae.csv", CONT_AE_CSV)
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = cont_fi(ae, "ae.csv"))

    pd <- rv$pending_delivery
    qa_check("the delivery waits for an answer", !is.null(pd))
    qa_step("one dialog is shown", 1L, length(modals))
    id <- sprintf("amb_%s_1", pd$nonce)
    qa_check("it has one answer per file name", grepl(sprintf('name="%s"', id), modals, fixed = TRUE))
    qa_check("it asks which dataset the file is for", grepl("Which dataset is this file for?", modals, fixed = TRUE))
    qa_check(
      "it says what to do",
      grepl("This file matches more than one dataset. Choose one, or Don't load.", modals, fixed = TRUE)
    )
    qa_check(
      "each candidate is named with the expected name that matched",
      grepl("ae — expects ^a.*[.]csv$", modals, fixed = TRUE) && grepl("dm — expects ^.*e[.]csv$", modals, fixed = TRUE)
    )
    qa_check("not loading is an explicit choice", grepl("Don't load", modals, fixed = TRUE))
    qa_check("no choice is preselected", !grepl("checked", modals, fixed = TRUE))
    qa_check("the delivery can be called off", grepl("Cancel delivery (nothing is loaded)", modals, fixed = TRUE))
    qa_check("the hint on distinct names is for edit mode only", !grepl("make the expected names distinct", modals, fixed = TRUE))
    qa_step("nothing is bound while the dialog waits", 0L, length(tables(datasets(rv$dta, "dm"))) + length(tables(datasets(rv$dta, "ae"))))
    qa_check("the staged copy is kept for the answer", all(file.exists(pd$copies)))

    session$setInputs(amb_load = 1)
    qa_check(
      "Load delivery with no answer is refused inline",
      grepl("Choose a dataset or Don.t load for every file\\.", ap_text(output$amb_error))
    )
    qa_check("and the delivery still waits", !is.null(rv$pending_delivery))
    qa_step("with nothing bound", 0L, length(rv$uploads))

    ap_set(session, id, "dm")
    session$setInputs(amb_load = 2)
    qa_step("answered, the dialog is done", TRUE, is.null(rv$pending_delivery))
    qa_step("the file is bound to the dataset it was sent to", "ae", names(tables(datasets(rv$dta, "dm"))))
    qa_step("and to no other", 0L, length(tables(datasets(rv$dta, "ae"))))
    qa_step("the record is that of dm", "dm||1", names(rv$uploads))
    qa_step("the earlier error is gone", "", ap_text(output$amb_error))
  })

  # Don't load leaves the file out, and de.csv, which only dm's pattern takes,
  # is bound once the question is answered.
  both <- c(cont_write("ae.csv", CONT_AE_CSV), cont_write("de.csv", CONT_DM_CSV))
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = cont_fi(both, c("ae.csv", "de.csv")))
    pd <- rv$pending_delivery
    qa_check("only the file that matches both is asked about", !is.null(pd) && length(pd$rows) == 1L)
    ap_set(session, sprintf("amb_%s_1", pd$nonce), "")
    session$setInputs(amb_load = 1)
    qa_step("Don't load binds the file to no dataset, and the other file is loaded", "de", names(tables(datasets(rv$dta, "dm"))))
    qa_step("ae holds nothing", 0L, length(tables(datasets(rv$dta, "ae"))))
    qa_check(
      "the notice names the file and says no dataset was chosen",
      grepl("Not loaded (no dataset chosen): ae.csv", utils::tail(shown, 1), fixed = TRUE)
    )
  })

  # Cancel loads nothing and deletes the staged copies.
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = cont_fi(ae, "ae.csv"))
    copies <- rv$pending_delivery$copies
    qa_check("a staged copy exists while the dialog waits", length(copies) == 1L && file.exists(copies))
    session$setInputs(amb_cancel = 1)
    qa_step("Cancel: nothing waits any more", TRUE, is.null(rv$pending_delivery))
    qa_step("Cancel: the staged copy that existed is deleted", FALSE, any(file.exists(copies)))
    qa_step("Cancel: nothing is bound", 0L, length(rv$uploads))
    qa_step("Cancel: and no delivery is recorded", TRUE, is.null(rv$last_delivery))
  })

  # A transport library routed to ae is opened; its members match both datasets
  # the same way, and only they are asked about, the first answer kept.
  lib_spec <- ap_yaml(c(
    AP_META, "datasets:", ap_ds("ae", "{filename: '[.]xpt$', type: xpt, pattern: true}", AP_AE_COLS),
    "  - id: docs", "    type: file",
    "    files: {filename: '[.]xpt$', type: any, pattern: true, number_of_files: 1, extensions: [xpt]}"
  ))
  lib <- cont_fx("sdtm.xpt")
  ap_serve(app, {
    shown_before <- length(modals)
    session$setInputs(dta_file = cont_fi(lib_spec, "spec.yaml"))
    session$setInputs(delivery_files = cont_fi(lib, "sdtm.xpt"))
    pd <- rv$pending_delivery
    qa_step("the library itself is asked about first", "sdtm.xpt", vapply(pd$rows, function(r) r$name, ""))
    ap_set(session, sprintf("amb_%s_1", pd$nonce), "ae")
    session$setInputs(amb_load = 1)

    pd2 <- rv$pending_delivery
    qa_check("the opened library's members are asked about in a second round", !is.null(pd2) && !identical(pd2$nonce, pd$nonce))
    qa_step("two dialogs were shown, one per round", 2L, length(modals) - shown_before)
    qa_step(
      "the second round asks about the three members, in the library's order",
      c("ae.xpt", "dm.xpt", "vs.xpt"),
      vapply(pd2$rows, function(r) r$name, "")
    )
    qa_step("and keeps the first answer", list(`sdtm.xpt` = "ae"), pd2$route)
    qa_step("nothing is bound before the second answer", 0L, length(tables(datasets(rv$dta, "ae"))))

    for (i in 1:3) ap_set(session, sprintf("amb_%s_%d", pd2$nonce, i), "ae")
    session$setInputs(amb_load = 2)
    qa_step("answered, the dialog is done", TRUE, is.null(rv$pending_delivery))
    qa_step("every member is bound to ae", c("ae", "dm", "vs"), sort(names(tables(datasets(rv$dta, "ae")))))
  })
})

test_that("OQ-CONT-059 | the report fails a file outside its container, is incomplete for a file checked on its own, passes only a conforming delivery, and the summary button never reads as a pass otherwise | REQ-CONT-030", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()
  ap_notes()

  # Both files arrive loose, the agreement has a default container: outside.
  spec <- ap_doc(AP_DEFAULT)
  both <- c(cont_write("ae.csv", CONT_AE_CSV), cont_write("dm.csv", CONT_DM_CSV))
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = cont_fi(both, c("ae.csv", "dm.csv")))
    session$setInputs(check_all = 1)
    qa_step("the content of both files passes", c(ae = "pass", dm = "pass"), unlist(rv$status))
    qa_step("the packaging of both is outside", c("outside", "outside"), findings_now()$status)

    qa_check("the report is FAILED", grepl("VALIDATION FAILED", validation_report_html(), fixed = TRUE))
    button <- ap_text(output$validation_report_ui)
    qa_check("the summary button is offered", grepl('id="dl_validation_summary"', button, fixed = TRUE))
    qa_check("but not as a pass", !grepl("btn-success", button, fixed = TRUE))
    qa_check("it says failed", grepl("failed", button, fixed = TRUE))
    qa_check(
      "the check's notice lists the finding next to the tally",
      grepl("arrived loose; the agreement requires it inside", utils::tail(shown, 1), fixed = TRUE)
    )
  })

  # Both files checked on their own: nothing is outside, nothing is verified.
  ae <- cont_write("ae.csv", CONT_AE_CSV)
  dm <- cont_write("dm.csv", CONT_DM_CSV)
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(up_1_1 = cont_fi(ae, "ae.csv"))
    session$setInputs(up_2_1 = cont_fi(dm, "dm.csv"))
    session$setInputs(check_all = 1)
    qa_step("the content of both files passes", c(ae = "pass", dm = "pass"), unlist(rv$status))
    qa_step("the packaging of both is unverified", c("unverified", "unverified"), findings_now()$status)

    html <- validation_report_html()
    qa_check("the report is INCOMPLETE", grepl("VALIDATION INCOMPLETE", html, fixed = TRUE))
    qa_check("and is never PASSED", !grepl("VALIDATION PASSED", html, fixed = TRUE))
    button <- ap_text(output$validation_report_ui)
    qa_check("the summary button is a warning", grepl("btn-warning", button, fixed = TRUE))
    qa_check("and not a pass", !grepl("btn-success", button, fixed = TRUE))
  })

  # A delivery that arrives inside its container is the one that can pass.
  good <- cont_zip(list(ae.csv = CONT_AE_CSV, dm.csv = CONT_DM_CSV), name = "delivery.zip")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = cont_fi(good, "delivery.zip"))
    session$setInputs(check_all = 1)
    qa_step("a delivery inside its container has no finding", 0L, nrow(findings_now()))
    qa_check("and the report is PASSED", grepl("VALIDATION PASSED", validation_report_html(), fixed = TRUE))
    qa_check("with a summary button that is a pass", grepl("btn-success", ap_text(output$validation_report_ui), fixed = TRUE))
  })
})

test_that("OQ-CONT-060 | in view mode the container manager lists the containers as text and changes nothing; the Metadata tab lists them and offers the manager only in edit mode | REQ-CONT-031", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()
  ap_notes()
  ap_modals()

  spec <- ap_doc(AP_SDTM, ae = "{filename: ae.csv, type: csv, container: sdtm}")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    qa_step("a document that was opened is in view mode", FALSE, editing())
    session$setInputs(open_containers = 1)
    qa_step("the manager opens", 1L, length(modals))
    html <- ap_text(output$ctr_modal_body)
    qa_check("it lists the file name", grepl("STUDY_SDTM_*.zip", html, fixed = TRUE))
    qa_check("the description", grepl("SDTM data", html, fixed = TRUE))
    qa_check("and what the container takes", grepl("1 file in 1 dataset", html, fixed = TRUE))
    for (id in c("ctr_add", "ctr_edit", "ctr_remove")) {
      qa_check(paste("it offers no control", id), !grepl(id, html, fixed = TRUE))
    }

    session$setInputs(ctr_tab_assign = 1)
    html <- ap_text(output$ctr_modal_body)
    qa_check("the Assign tab shows the choice", grepl("STUDY_SDTM_*.zip", html, fixed = TRUE))
    qa_check("as text, not as a select", !grepl("<select", html, fixed = TRUE))
    qa_check("with no Save", !grepl("ctr_assign_save", html, fixed = TRUE))

    before <- rv$dta
    session$setInputs(ctr_tab_containers = 1)
    session$setInputs(ctr_add = 1)
    session$setInputs(ctr_filename = "X_*.zip", ctr_description = "", ctr_default = FALSE)
    session$setInputs(ctr_save = 1)
    qa_step("an input that would add a container changes nothing", "sdtm", names(rv$dta@containers))
    qa_check("the document is the one it was", identical(rv$dta, before))
    qa_step("and the manager stays on its list", "list", rv$ctr_view)
  })

  # The Metadata tab.
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    html <- ap_text(output$metadata_editor)
    qa_check(
      "the Packaging field points to the Containers section",
      grepl("File names and contents of the delivery archives are defined under Containers below.", html, fixed = TRUE)
    )
    qa_check("the section exists", grepl('id="metadata_containers"', html, fixed = TRUE))
    qa_check("it comes after Transmission and before the agreement sections", {
      at <- regexpr('id="metadata_containers"', html, fixed = TRUE)
      regexpr("Blinding requirements", html, fixed = TRUE) < at && at < regexpr("Agreement sections", html, fixed = TRUE)
    })
    qa_check("in view mode it offers no manager", !grepl("metadata_manage_containers", html, fixed = TRUE))
    list_html <- ap_text(output$metadata_containers)
    qa_check(
      "it lists the file name, the description and what the container takes",
      grepl("STUDY_SDTM_*.zip", list_html, fixed = TRUE) && grepl("SDTM data", list_html, fixed = TRUE) &&
        grepl("1 file in 1 dataset", list_html, fixed = TRUE)
    )

    qapp_unlock_editing(session)
    html <- ap_text(output$metadata_editor)
    qa_check("in edit mode it offers Manage containers", grepl("Manage containers…", html, fixed = TRUE))
    session$setInputs(metadata_manage_containers = 1)
    qa_check("which opens the manager", grepl("ctr_modal_body", utils::tail(modals, 1), fixed = TRUE))
    qa_check("with its Add button", grepl('id="ctr_add"', ap_text(output$ctr_modal_body), fixed = TRUE))
  })

  # Once the manager is closed its body is empty, so a reopened dialog never
  # shows a cached form; the form's match line reads the last delivery.
  spec <- ap_spec()
  zip1 <- cont_fx("sdtm_csv.zip")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    session$setInputs(delivery_files = cont_fi(zip1, "sdtm_csv.zip"))
    qapp_unlock_editing(session)
    qa_step("before it is opened the body is empty", "", ap_text(output$ctr_modal_body))
    session$setInputs(open_containers = 1)
    session$setInputs(ctr_add = 1)
    session$setInputs(ctr_filename = "sdtm_*.zip")
    match_line <- function() trimws(gsub("<[^>]*>", "", ap_text(output$ctr_match)))
    qa_check(
      "a file name the last delivery's archive matches says so",
      grepl("Matches sdtm_csv.zip from the last delivery.", match_line(), fixed = TRUE) && grepl("✓", match_line(), fixed = TRUE)
    )
    session$setInputs(ctr_filename = "other_*.zip")
    qa_check(
      "one that matches none says so and names the archive",
      grepl("Matches none of the last delivery's archives (sdtm_csv.zip).", match_line(), fixed = TRUE) && grepl("✗", match_line(), fixed = TRUE)
    )
    session$setInputs(dta_modal_state = list(event = "hidden", kind = "containers", id = rv$ctr_nonce, at = 1))
    qa_step("when the browser reports the dialog hidden the manager is closed", FALSE, rv$ctr_open)
    qa_step("and the body is empty again", "", ap_text(output$ctr_modal_body))
  })
})

test_that("OQ-CONT-061 | in edit mode the container manager adds, assigns, renames, takes the default over and removes containers, and an invalid name is refused inline | REQ-CONT-031", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()
  ap_notes()

  # Add, assign, rename.
  spec <- ap_spec()
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    qapp_unlock_editing(session)
    session$setInputs(open_containers = 1)
    qa_check("a document without containers says so", grepl("No containers", ap_text(output$ctr_modal_body), fixed = TRUE))
    session$setInputs(ctr_add = 1)
    html <- ap_text(output$ctr_modal_body)
    qa_check("the form asks for a file name", grepl('id="ctr_filename"', html, fixed = TRUE))
    qa_check("says files added later are included", grepl("Files added later are included automatically.", html, fixed = TRUE))
    qa_check("and offers the default option", grepl("All other files must arrive inside it too", html, fixed = TRUE))

    session$setInputs(ctr_filename = "STUDY_SDTM_*.zip", ctr_description = "SDTM data", ctr_default = FALSE)
    session$setInputs(ctr_save = 1)
    qa_step(
      "the container is saved under an id derived from its file name",
      list(ids = "STUDY_SDTM", filename = "STUDY_SDTM_*.zip"),
      list(ids = names(rv$dta@containers), filename = rv$dta@containers$STUDY_SDTM@filename)
    )
    qa_check("the YAML follows", grepl("containers:", rv$yaml_text, fixed = TRUE) && grepl("STUDY_SDTM_*.zip", rv$yaml_text, fixed = TRUE))
    qa_check("the user is told", any(grepl("Container saved.", shown, fixed = TRUE)))
    qa_check("the id is never shown: the list names the file", !grepl("STUDY_SDTM<", ap_text(output$ctr_modal_body), fixed = TRUE))

    session$setInputs(ctr_tab_assign = 1)
    html <- ap_text(output$ctr_modal_body)
    nonce <- rv$ctr_assign$nonce
    qa_check(
      "the Assign tab has a select for each entry",
      grepl(sprintf('id="ctr_a_%s_1"', nonce), html, fixed = TRUE) && grepl(sprintf('id="ctr_a_%s_2"', nonce), html, fixed = TRUE)
    )
    qa_check("with the choice of no container", grepl("Loose or in any container", html, fixed = TRUE))
    qa_check("and the container", grepl('<option value="STUDY_SDTM">STUDY_SDTM_*.zip</option>', html, fixed = TRUE))
    ap_set(session, sprintf("ctr_a_%s_1", nonce), "STUDY_SDTM")
    ap_set(session, sprintf("ctr_a_%s_2", nonce), "STUDY_SDTM")
    session$setInputs(ctr_assign_save = 1)
    qa_step("both entries reference the container", c("STUDY_SDTM", "STUDY_SDTM"), c(ap_ref(rv$dta, "ae"), ap_ref(rv$dta, "dm")))
    qa_check("and the YAML says so", grepl("container: STUDY_SDTM", rv$yaml_text, fixed = TRUE))

    session$setInputs(open_containers = 2)
    qa_check("the list counts them", grepl("2 files in 2 datasets", ap_text(output$ctr_modal_body), fixed = TRUE))
    session$setInputs(ctr_edit = 1)
    session$setInputs(ctr_filename = "STUDY_SDTM_2026_*.zip", ctr_description = "SDTM data", ctr_default = FALSE)
    session$setInputs(ctr_save = 2)
    qa_step(
      "renamed, the container keeps its id and its new file name",
      list(ids = "STUDY_SDTM", filename = "STUDY_SDTM_2026_*.zip"),
      list(ids = names(rv$dta@containers), filename = rv$dta@containers$STUDY_SDTM@filename)
    )
    qa_step(
      "and both entries stay on it",
      c("STUDY_SDTM", "STUDY_SDTM"),
      c(ap_ref(rv$dta, "ae"), ap_ref(rv$dta, "dm"))
    )
    qa_check("the YAML carries the new file name", grepl("STUDY_SDTM_2026_*.zip", rv$yaml_text, fixed = TRUE))
  })

  # The default: ticking it takes it from the container that had it.
  spec <- ap_doc(AP_TWO)
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    qapp_unlock_editing(session)
    session$setInputs(open_containers = 1)
    session$setInputs(ctr_tab_assign = 1)
    html <- ap_text(output$ctr_modal_body)
    qa_check("with a default container the empty choice is that container", grepl("STUDY_SDTM_*.zip (with all other files)", html, fixed = TRUE))
    qa_check("the choice of no container is named", grepl("No container required", html, fixed = TRUE))
    qa_check("and the loose choice is gone", !grepl("Loose or in any container", html, fixed = TRUE))

    session$setInputs(ctr_tab_containers = 1)
    qa_check("the list says the default takes all other files", grepl("All other files", ap_text(output$ctr_modal_body), fixed = TRUE))
    session$setInputs(ctr_edit = 2)
    session$setInputs(ctr_filename = "STUDY_ADAM_*.zip", ctr_description = "", ctr_default = TRUE)
    qa_check(
      "the form warns before it takes the default over",
      grepl(
        "'STUDY_SDTM_*.zip' currently collects all other files. Saving moves them to 'STUDY_ADAM_*.zip'; 'STUDY_SDTM_*.zip' will then hold no files.",
        ap_text(output$ctr_default_warning),
        fixed = TRUE
      )
    )
    session$setInputs(ctr_save = 1)
    qa_step("saved, the container is the default", c(TRUE, FALSE), c(rv$dta@containers$adam@default, rv$dta@containers$sdtm@default))
    qa_check(
      "and the user is told which container lost it",
      any(grepl("'STUDY_ADAM_*.zip' now takes all other files; 'STUDY_SDTM_*.zip' no longer does.", shown, fixed = TRUE))
    )
  })

  # Remove asks first, can be called off, and clears every reference.
  spec <- ap_doc(AP_SDTM,
    ae = "{filename: ae.csv, type: csv, container: sdtm}",
    dm = "{filename: dm.csv, type: csv, container: STUDY_SDTM_*.zip}"
  )
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    qapp_unlock_editing(session)
    session$setInputs(open_containers = 1)
    qa_check("the container takes the entry that names it by id and the one that names it by file name", grepl("2 files in 2 datasets", ap_text(output$ctr_modal_body), fixed = TRUE))
    session$setInputs(ctr_remove = 1)
    qa_check(
      "Remove asks first and says how many files lose their requirement",
      grepl("Remove STUDY_SDTM_*.zip? Its 2 files will no longer have to arrive inside it.", ap_flat(ap_text(output$ctr_modal_body)), fixed = TRUE)
    )
    session$setInputs(ctr_remove_keep = 1)
    qa_step("Keep leaves the container", "sdtm", names(rv$dta@containers))
  })
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    qapp_unlock_editing(session)
    session$setInputs(open_containers = 1)
    ap_text(output$ctr_modal_body)
    session$setInputs(ctr_remove = 1)
    session$setInputs(ctr_remove_confirm = 1)
    qa_step("Remove deletes the container", 0L, length(rv$dta@containers))
    qa_step("and clears the reference by id and the one by file name", c(NA_character_, NA_character_), c(ap_ref(rv$dta, "ae"), ap_ref(rv$dta, "dm")))
    qa_check("the YAML has no containers section", !grepl("containers:", rv$yaml_text, fixed = TRUE))
  })

  # A file name that cannot be a container is refused inline, without an id.
  spec <- ap_spec()
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    qapp_unlock_editing(session)
    session$setInputs(open_containers = 1)
    session$setInputs(ctr_add = 1)
    session$setInputs(ctr_filename = "x.tar", ctr_description = "", ctr_default = FALSE)
    session$setInputs(ctr_save = 1)
    msg <- ap_flat(ap_text(output$ctr_msg))
    qa_step("nothing is saved", 0L, length(rv$dta@containers))
    qa_check("the message says what a container must be named", grepl("must name a .zip archive or a .xpt SAS transport file", msg, fixed = TRUE))
    qa_check("and does not show the id the user never chose", !grepl("Container '", msg, fixed = TRUE))
    qa_step("the form stays open", "form", rv$ctr_view)
  })

  # A reference an assignment select did not report is kept; one it reported
  # as empty is cleared.
  spec <- ap_doc(AP_SDTM,
    ae = "{filename: ae.csv, type: csv, container: sdtm}",
    dm = "{filename: dm.csv, type: csv, container: sdtm}"
  )
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    qapp_unlock_editing(session)
    session$setInputs(open_containers = 1)
    session$setInputs(ctr_tab_assign = 1)
    ap_text(output$ctr_modal_body)
    ap_set(session, sprintf("ctr_a_%s_1", rv$ctr_assign$nonce), "")
    session$setInputs(ctr_assign_save = 1)
    qa_step(
      "the select that reported nothing chosen clears ae, the one that did not report keeps dm",
      c(NA_character_, "sdtm"),
      c(ap_ref(rv$dta, "ae"), ap_ref(rv$dta, "dm"))
    )
  })

  # An implied container is a container the user can edit: saving defines it.
  spec <- ap_doc(ae = "{filename: ae.csv, type: csv, container: sdtm_csv.zip}")
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    qapp_unlock_editing(session)
    session$setInputs(open_containers = 1)
    qa_check("the container the entry names is listed", grepl("sdtm_csv.zip", ap_text(output$ctr_modal_body), fixed = TRUE))
    qa_step("though the document defines none", 0L, length(rv$dta@containers))
    session$setInputs(ctr_edit = 1)
    session$setInputs(ctr_filename = "sdtm_csv_v2.zip", ctr_description = "Renamed", ctr_default = FALSE)
    session$setInputs(ctr_save = 1)
    qa_step("saving it defines a container with an id derived from the new name", "sdtm_csv_v2", names(rv$dta@containers))
    qa_step("and the entry references it", "sdtm_csv_v2", ap_ref(rv$dta, "ae"))
  })
})

test_that("OQ-CONT-062 | the application refuses an edit that makes two datasets take one file name, compares by what entries expect, and warns about a document opened with an overlap | REQ-CONT-032", {
  app <- qapp_skip_unless_app_installed()
  qapp_clean_session_files()
  ap_notes()

  # ^lb[.]xpt$ and ^lb_supp.*[.]xpt$ take no file in common. Widening the first
  # to ^lb.*[.]xpt$ makes both take lb_supp.xpt, the shortest name of the second.
  spec <- ap_doc(ae = ap_xpt("^lb[.]xpt$"), dm = ap_xpt("^lb_supp.*[.]xpt$"))
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    qapp_unlock_editing(session)
    qa_step("the document has no conflict", 0L, nrow(DTAtools::file_name_conflicts(rv$dta)))
    session$setInputs(edit_files = 1)
    session$setInputs(file_edit_click = 1)
    before <- rv$dta
    ap_file_form(session, "^lb.*[.]xpt$", type = "xpt", pattern = TRUE)
    session$setInputs(file_save = 1)
    qa_step("a save that would introduce a conflict is refused", FALSE, rv$file_msg$ok)
    qa_check("the message names the example file name", grepl("lb_supp.xpt", rv$file_msg$error, fixed = TRUE))
    qa_check(
      "and says both datasets would take it",
      grepl("both take a file named 'lb_supp.xpt'", ap_flat(ap_text(output$file_editor_msg)), fixed = TRUE)
    )
    qa_check("nothing was saved", identical(rv$dta, before))

    session$setInputs(file_back = 1)
    session$setInputs(file_add = 1)
    ap_file_form(session, "^lb_supp.*[.]xpt$", type = "xpt", pattern = TRUE)
    session$setInputs(file_save = 2)
    qa_step("a new entry that would introduce one is refused as well", FALSE, rv$file_msg$ok)
    qa_check("and nothing was saved", identical(rv$dta, before))
  })

  # dm's pattern is inside archive.zip, ae's is loose: they never tie. Removing
  # the container frees dm's entry, and assigning ae's into the archive puts
  # both inside it: either makes the two scopes one.
  spec <- ap_doc(
    c("containers:", "  - id: arc", "    filename: archive.zip"),
    ae = ap_xpt("^lb.*[.]xpt$"),
    dm = "{filename: '^lb_supp.*[.]xpt$', type: xpt, pattern: true, container: arc}"
  )
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    qapp_unlock_editing(session)
    qa_step("the document has no conflict while the scopes differ", 0L, nrow(DTAtools::file_name_conflicts(rv$dta)))
    session$setInputs(open_containers = 1)
    ap_text(output$ctr_modal_body)
    before <- rv$dta
    session$setInputs(ctr_remove = 1)
    session$setInputs(ctr_remove_confirm = 1)
    msg <- ap_flat(ap_text(output$ctr_msg))
    qa_check("removing the container that keeps them apart is refused", identical(rv$dta, before))
    qa_check("with a message that names the file name and says what to do", grepl("lb_supp.xpt", msg, fixed = TRUE) && grepl("Make the expected names distinct", msg, fixed = TRUE))
    qa_step("the container is still there", 1L, length(rv$dta@containers))

    session$setInputs(ctr_tab_assign = 1)
    ap_text(output$ctr_modal_body)
    ap_set(session, sprintf("ctr_a_%s_1", rv$ctr_assign$nonce), "arc")
    session$setInputs(ctr_assign_save = 1)
    qa_check("assigning the other entry into the same archive is refused", identical(rv$dta, before))
    qa_check("with a message that names the file name", grepl("lb_supp.xpt", ap_text(output$ctr_msg), fixed = TRUE))
  })

  # ae is inside archive.zip, dm's entry is loose. A default container *.zip
  # would take dm's entry, and its glob overlaps archive.zip.
  spec <- ap_doc(
    c("containers:", "  - id: arc", "    filename: archive.zip"),
    ae = "{filename: '^lb.*[.]xpt$', type: xpt, pattern: true, container: arc}",
    dm = ap_xpt("^lb_supp.*[.]xpt$")
  )
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    qapp_unlock_editing(session)
    before <- rv$dta
    session$setInputs(open_containers = 1)
    session$setInputs(ctr_add = 1)
    session$setInputs(ctr_filename = "*.zip", ctr_description = "", ctr_default = TRUE)
    session$setInputs(ctr_save = 1)
    qa_check("a default container that merges the scopes is refused", identical(rv$dta, before))
    qa_check(
      "with the message that names the file name",
      grepl("both take a file named 'lb_supp.xpt'", ap_flat(ap_text(output$ctr_msg)), fixed = TRUE)
    )
    qa_step("and the form stays open", "form", rv$ctr_view)
    session$setInputs(ctr_default = FALSE)
    session$setInputs(ctr_save = 2)
    qa_step("without the default tick nothing overlaps and the same file name is saved", 2L, length(rv$dta@containers))
  })

  # A document opened with an overlap is accepted and warns; an edit that does
  # not touch the overlap is accepted after an entry was put in front of it,
  # because conflicts are compared by what the entries expect, not by position.
  spec <- ap_doc(
    ae = sprintf("[%s, %s]", ap_xpt("^zz[.]xpt$"), ap_xpt("^lb.*[.]xpt$")),
    dm = ap_xpt("^lb_supp.*[.]xpt$")
  )
  ap_serve(app, {
    session$setInputs(dta_file = cont_fi(spec, "spec.yaml"))
    qa_step("a document with an overlap is opened", "^lb.*[.]xpt$", rv$dta@datasets$ae@files[[2]]@filename)
    warn <- "File names overlap with dataset 'dm' (e.g. 'lb_supp.xpt'). A delivered file matching both is asked about."
    qa_check("and warns, naming the other dataset and an example", grepl(warn, ap_flat(ap_text(output$dataset_conflict_warning)), fixed = TRUE))
    qa_check("without telling a viewer to edit", !grepl("Edit ▸ Files", ap_flat(ap_text(output$dataset_conflict_warning)), fixed = TRUE))

    qapp_unlock_editing(session)
    qa_check(
      "in edit mode it says where to make the names distinct",
      grepl(paste0(warn, " Edit ▸ Files to make them distinct."), ap_flat(ap_text(output$dataset_conflict_warning)), fixed = TRUE)
    )
    session$setInputs(edit_files = 1)
    qa_check("the file editor warns too", grepl(warn, ap_flat(ap_text(output$file_conflict_warning)), fixed = TRUE))

    session$setInputs(file_add = 1)
    ap_file_form(session, "^yy[.]xpt$", type = "xpt", pattern = TRUE)
    session$setInputs(file_save = 1)
    qa_step("a new entry that adds no conflict is accepted", TRUE, is.null(rv$file_msg))
    qa_step("it is the third entry", 3L, length(rv$dta@datasets$ae@files))
    session$setInputs(file_up_click = 3)
    session$setInputs(file_up_click = 2)
    qa_step(
      "moved to the front, it shifts the entry that conflicts",
      c("^yy[.]xpt$", "^lb.*[.]xpt$"),
      c(rv$dta@datasets$ae@files[[1]]@filename, rv$dta@datasets$ae@files[[3]]@filename)
    )
    session$setInputs(file_edit_click = 1)
    ap_file_form(session, "^yy2[.]xpt$", type = "xpt", pattern = TRUE)
    session$setInputs(file_save = 2)
    qa_step("an unrelated edit of the document with the overlap is accepted", TRUE, is.null(rv$file_msg))
    qa_step("it was saved", "^yy2[.]xpt$", rv$dta@datasets$ae@files[[1]]@filename)
    qa_step("and the one conflict that was there is the only one", 1L, nrow(DTAtools::file_name_conflicts(rv$dta)))
  })
})
