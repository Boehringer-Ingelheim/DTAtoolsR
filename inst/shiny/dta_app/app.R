# =============================================================================
# DTAtools Shiny app
# Modern UI for the DTAtools R package.
# Launched via DTAtools::run_dta_app(). Helper code lives in ./R (auto-sourced).
# =============================================================================

library(shiny)
library(bslib)

# Allow large uploads (clinical data files can be big).
options(shiny.maxRequestSize = 1024 * 1024^2) # 1 GB

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
brandbar <- div(
  class = "app-brandbar",
  tags$img(class = "brand-logo", src = "dtatools_logo_small.png", alt = "DTAtools logo"),
  div(
    div(class = "brand-title", "DTAtools"),
    div(class = "brand-sub", "Data Tranfer Agreements (DTA) / Data Transmission Specifications (DTS) \u2014 validation & authoring")
  )
)

# Non-floating footer: DTAtools version + author + link to the GitHub repo.
dta_pkg_version <- tryCatch(as.character(utils::packageVersion("DTAtools")),
                            error = function(e) "")
app_footer <- tags$footer(
  class = "app-footer",
  tags$span(class = "foot-name", "DTAtools"),
  if (nzchar(dta_pkg_version)) tags$span(class = "foot-ver", paste0("v", dta_pkg_version)),
  tags$span(class = "foot-sep", "\u2022"),
  tags$span("Boehringer Ingelheim"),
  tags$span(class = "foot-sep", "\u2022"),
  tags$a(href = "https://github.com/Boehringer-Ingelheim/DTAtoolsR",
         target = "_blank", rel = "noopener noreferrer", "GitHub repository")
)

# Clear a fileInput's text field after an upload is processed (so the control
# reads empty and is ready for the next drop). Triggered from the server via
# session$sendCustomMessage("dta_reset_fileinput", <input id>).
reset_fileinput_js <- "
Shiny.addCustomMessageHandler('dta_reset_fileinput', function(id) {
  var el = document.getElementById(id);
  if (!el) return;
  try { el.value = ''; } catch (e) {}
  var box = el.closest('.shiny-input-container') || el.closest('.form-group');
  if (box) {
    var txt = box.querySelector('input[type=text].form-control');
    if (txt) { txt.value = ''; }
    var bar = box.querySelector('.progress-bar');
    if (bar) { bar.style.width = '0%'; bar.textContent = ''; }
  }
});
"

# Floating validation-messages dock: fold/unfold on header click, plus a custom
# handler the server calls to open (reveal) it after a validation run. Opening
# fires a window resize so the DataTable inside (rendered while hidden) re-fits.
msgs_dock_js <- "
function DTA_toggleMsgsDock(e){
  if (e) { e.stopPropagation(); }
  var d = document.getElementById('dta-msgs-dock');
  if (!d) return;
  d.classList.toggle('collapsed');
  if (!d.classList.contains('collapsed')) { window.dispatchEvent(new Event('resize')); }
}
Shiny.addCustomMessageHandler('dta_msgs_dock', function(action){
  var d = document.getElementById('dta-msgs-dock');
  if (!d) return;
  if (action === 'open') { d.classList.remove('collapsed'); window.dispatchEvent(new Event('resize')); }
  else if (action === 'close') { d.classList.add('collapsed'); }
  else { d.classList.toggle('collapsed'); }
});
"

ui <- bslib::page_fluid(
  theme = bi_theme(),
  tags$head(tags$style(bi_css()),
            tags$script(shiny::HTML(reset_fileinput_js)),
            tags$script(shiny::HTML(msgs_dock_js))),
  brandbar,
  div(style = "padding: 18px;", uiOutput("main")),
  app_footer,
  uiOutput("floating_msgs")
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # --- reactive state (single source of truth) ---------------------------
  rv <- reactiveValues(
    dta = NULL,          # the DTA S7 object
    yaml_text = NULL,    # original uploaded YAML text (for raw view)
    structure = NULL,    # stable per-dataset handler metadata (for slots)
    active = NULL,       # currently selected dataset name
    uploads = list(),    # key "dataset||handlerIdx" -> list of {file, table} records
    status = list(),     # dataset name -> "pass" | "fail" | "pending" | "nodata"
    pending_upload = NULL, # deferred upload awaiting an overwrite confirmation
    example_target = NULL, # list(ds_idx, hi) the example-file modal loads into
    dataset_only = FALSE, # TRUE when a standalone dataset YAML was loaded (no metadata)
    is_example = FALSE,  # TRUE when the bundled example DTA is loaded (enables example-file pickers)
    md_token = 0,        # bump to re-render metadata editor
    contacts_token = 0,  # bump to re-render contacts list
    yaml_msg = NULL,     # raw-YAML apply result: NULL | list(ok, error)
    editing_contact = NULL, # list(side, index) while a contact edit modal is open
    editor_dataset = NULL,  # dataset name the column/rule editor modal targets
    col_view = "list",   # column editor view: "list" | "form"
    col_token = 0,       # bump to re-render the column editor body
    col_edit_id = NULL,  # id of the column being edited (NULL = adding new)
    col_prefill = NULL,  # list() of the column fields loaded in the form
    rule_view = "list",  # rule editor view: "list" | "form"
    rule_token = 0,      # bump to re-render the rule editor body
    rule_edit_index = NULL, # index of the rule being edited (NULL = adding new)
    rule_prefill = NULL, # list() of the rule fields currently loaded in the form
    col_msg = NULL,      # inline column-editor result: NULL | list(ok, error)
    rule_msg = NULL,     # inline rule-editor result: NULL | list(ok, error)
    cond_n = 1L,         # condition-builder row count (IF ...)
    then_n = 1L          # condition-builder row count (THEN ...)
  )

  upload_registry <- new.env(parent = emptyenv())
  session_file <- file.path(tempdir(), "dtatools_app_session.rds")

  # Stable id per bound file so its trash button keeps working across renders.
  file_id_env      <- new.env(parent = emptyenv())  # "ds\u0001hi\u0001table" -> integer id
  file_id_meta     <- new.env(parent = emptyenv())  # id (as chr) -> list(dataset, hi, table)
  file_rm_registry <- new.env(parent = emptyenv())  # button id -> TRUE once observed
  file_id_counter  <- 0L
  get_file_id <- function(dsname, hi, table) {
    key <- paste(dsname, hi, table, sep = "\u0001")
    id <- file_id_env[[key]]
    if (is.null(id)) {
      file_id_counter <<- file_id_counter + 1L
      id <- file_id_counter
      file_id_env[[key]] <- id
      file_id_meta[[as.character(id)]] <- list(dataset = dsname, hi = hi, table = table)
    }
    id
  }

  # --- helpers ------------------------------------------------------------
  build_structure <- function(dta) {
    names_ds <- dta_dataset_names(dta)
    if (length(names_ds) == 0) return(list())
    stats::setNames(lapply(seq_along(names_ds), function(i) {
      ds <- dta_get_dataset(dta, names_ds[i])
      handlers <- lapply(dta_handlers(ds), function(h) list(
        expected = handler_expected(h),
        hint     = handler_hint(h),
        count    = handler_count_label(h),
        min      = handler_min(h),
        max      = handler_max(h),
        pattern  = handler_is_pattern(h)
      ))
      list(
        index = i,
        name = names_ds[i],
        type = tryCatch(ds@type, error = function(e) NA_character_),
        handlers = handlers
      )
    }), names_ds)
  }

  autosave <- function() {
    try(saveRDS(
      list(
        dump = dta_dump_session(isolate(rv$dta)),
        yaml_text = isolate(rv$yaml_text),
        structure = isolate(rv$structure),
        active = isolate(rv$active),
        uploads = isolate(rv$uploads),
        status = isolate(rv$status),
        dataset_only = isolate(rv$dataset_only),
        is_example = isolate(rv$is_example)
      ),
      session_file
    ), silent = TRUE)
  }

  apply_loaded <- function(dta, yaml_text, dataset_only = FALSE, is_example = FALSE) {
    names_ds <- dta_dataset_names(dta)
    rv$dta <- dta
    rv$yaml_text <- yaml_text
    rv$structure <- build_structure(dta)
    rv$uploads <- list()
    rv$status <- stats::setNames(rep("pending", length(names_ds)), names_ds)
    rv$active <- if (length(names_ds) > 0) names_ds[1] else NULL
    rv$dataset_only <- isTRUE(dataset_only)
    rv$is_example <- isTRUE(is_example)
    rv$md_token <- rv$md_token + 1
    rv$contacts_token <- rv$contacts_token + 1
    autosave()
  }

  # --- landing: load a DTA YAML ------------------------------------------
  observeEvent(input$dta_file, {
    f <- input$dta_file
    req(f)
    txt <- tryCatch(paste(readLines(f$datapath, warn = FALSE), collapse = "\n"),
                    error = function(e) NULL)
    res <- dta_read_yaml(f$datapath)
    if (!res$ok) {
      showNotification(
        paste("Could not load DTA YAML:", res$error),
        type = "error", duration = 10
      )
      return()
    }
    apply_loaded(res$value, txt, dataset_only = isTRUE(res$dataset_only))
    showNotification(
      if (isTRUE(res$dataset_only)) "Dataset loaded (no metadata)." else "DTA loaded.",
      type = "message"
    )
  })

  observeEvent(input$load_example, {
    files <- dta_example_yaml_files()
    if (length(files) == 0) {
      showNotification("No example specifications found.", type = "error")
      return()
    }
    showModal(modalDialog(
      title = "Load example DTA",
      radioButtons("example_dta_choice",
                   "Choose a bundled example specification:",
                   choices = files, selected = files[[1]]),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("example_dta_load", "Load", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$example_dta_load, {
    sel <- input$example_dta_choice
    if (is.null(sel) || length(sel) == 0 || !nzchar(sel)) {
      showNotification("Choose an example specification first.", type = "warning")
      return()
    }
    path <- dta_example_yaml_path(sel)
    if (!nzchar(path)) {
      showNotification("Example file not found.", type = "error")
      return()
    }
    txt <- tryCatch(paste(readLines(path, warn = FALSE), collapse = "\n"),
                    error = function(e) NULL)
    res <- dta_read_yaml(path)
    if (!res$ok) {
      showNotification(paste("Could not load example:", res$error), type = "error")
      return()
    }
    removeModal()
    apply_loaded(res$value, txt, dataset_only = isTRUE(res$dataset_only), is_example = TRUE)
    showNotification(sprintf("Example \u201c%s\u201d loaded.", sel), type = "message")
  })

  # --- dataset navigation (custom list: select + per-dataset check icon) --
  output$dataset_nav_ui <- renderUI({
    req(rv$structure)
    names_ds <- names(rv$structure)
    active <- rv$active
    st_map <- rv$status
    div(
      class = "dataset-nav",
      div(class = "section-label", "Datasets"),
      div(
        class = "dataset-nav-list",
        lapply(seq_along(names_ds), function(i) {
          nm <- names_ds[i]
          st <- st_map[[nm]] %||% "pending"
          # Row background + icon encode status: passed (green), failed (red),
          # missing/no-data (orange), not-checked-yet (neutral grey).
          st2 <- switch(st, pass = "pass", fail = "fail", nodata = "nodata", "pending")
          ic_ch <- switch(st,
            pass = "\u2714", fail = "\u2716", nodata = "\u2716", "\u2013")
          ic_cls <- switch(st,
            pass = "nav-ic-pass", fail = "nav-ic-fail",
            nodata = "nav-ic-nodata", "nav-ic-pending")
          ic_ttl <- switch(st,
            pass = "Passed all checks", fail = "Validation failed",
            nodata = "No data loaded (missing data)", "Not validated yet")
          row_cls <- paste0("dataset-nav-row nav-st-", st2,
                            if (identical(nm, active)) " active" else "")
          div(
            class = row_cls,
            actionLink(
              paste0("selds_", i), class = "nav-select",
              label = tagList(
                span(class = paste("nav-ic", ic_cls), title = ic_ttl, ic_ch),
                span(class = "nav-name", nm)
              )
            ),
            actionButton(
              paste0("checkds_", i), label = HTML("&#x25B6;"),
              class = "btn btn-sm nav-check",
              title = sprintf("Check '%s'", nm)
            )
          )
        })
      )
    )
  })

  # Register one select + one check observer per dataset slot (index-stable;
  # resolves the dataset name at click time so it tracks the loaded DTA).
  nav_registry <- new.env(parent = emptyenv())
  observe({
    req(rv$structure)
    for (i in seq_along(names(rv$structure))) {
      sel_id <- paste0("selds_", i)
      if (is.null(nav_registry[[sel_id]])) {
        nav_registry[[sel_id]] <- TRUE
        local({
          IDX <- i
          observeEvent(input[[paste0("selds_", IDX)]], {
            nms <- names(rv$structure)
            if (IDX <= length(nms)) rv$active <- nms[IDX]
          }, ignoreInit = TRUE)
          observeEvent(input[[paste0("checkds_", IDX)]], {
            nms <- names(rv$structure)
            if (IDX <= length(nms)) run_check(nms[IDX])
          }, ignoreInit = TRUE)
        })
      }
    }
  })

  # --- upload observers (registered once per handler) ---------------------
  handle_upload <- function(ds_idx, hi, fileinfo, overwrite = FALSE) {
    if (is.null(fileinfo)) return()
    names_ds <- dta_dataset_names(rv$dta)
    if (ds_idx < 1 || ds_idx > length(names_ds)) return()
    dsname <- names_ds[ds_idx]
    ds <- dta_get_dataset(rv$dta, dsname)
    handlers <- dta_handlers(ds)
    if (hi < 1 || hi > length(handlers)) return()
    h <- handlers[[hi]]
    key <- paste0(dsname, "||", hi)

    # A dropped file will occupy a table named after it (load_file uses
    # file_path_sans_ext). This mapping drives overwrite detection and binds.
    tbl_of <- function(nm) tools::file_path_sans_ext(basename(nm))
    existing <- dta_dataset_table_names(ds)  # dataset-wide bound items

    # Overwrite gate: if a dropped file targets an already-bound table and the
    # user has not confirmed, ask first -- never silently replace bound data.
    if (!isTRUE(overwrite)) {
      dropped_tbls <- vapply(as.character(fileinfo$name), tbl_of, character(1))
      conflicts <- unique(dropped_tbls[dropped_tbls %in% existing])
      if (length(conflicts) > 0) {
        rv$pending_upload <- list(ds_idx = ds_idx, hi = hi, fileinfo = fileinfo)
        showModal(modalDialog(
          title = "Overwrite existing file(s)?",
          tags$p(sprintf("These file(s) are already loaded in '%s':", dsname)),
          tags$ul(lapply(conflicts, function(t) tags$li(tags$code(t)))),
          tags$p("Overwrite them with the new upload? The affected table(s) will be marked as not validated."),
          footer = tagList(
            actionButton("cancel_overwrite", "Cancel"),
            actionButton("confirm_overwrite", "Overwrite", class = "btn btn-warning")
          ),
          easyClose = TRUE
        ))
        return()
      }
    }

    # G2 -- count gate: only NEW (non-replacing) files add to the slot's count.
    mx <- handler_max(h)
    slot_recs <- rv$uploads[[key]] %||% list()
    slot_tbls <- vapply(slot_recs, function(r) r$table %||% "", character(1))
    dropped_tbls <- vapply(as.character(fileinfo$name), tbl_of, character(1))
    kept_after <- length(setdiff(slot_tbls, dropped_tbls))
    if (!is.na(mx) && (kept_after + nrow(fileinfo)) > mx) {
      showNotification(
        sprintf("This slot accepts at most %d file(s); remove one before adding more.",
                as.integer(mx)),
        type = "error"
      )
      return()
    }

    loaded <- character(0)
    rejected <- character(0)
    overwritten <- character(0)
    loaded_recs <- list()
    for (r in seq_len(nrow(fileinfo))) {
      nm <- fileinfo$name[r]
      dp <- fileinfo$datapath[r]
      sz <- suppressWarnings(as.numeric(fileinfo$size[r]))
      tbl <- tbl_of(nm)
      was_existing <- tbl %in% existing

      # G1 -- transfer complete & non-empty (guards truncated/partial uploads).
      if (is.na(sz) || sz <= 0) {
        rejected <- c(rejected, sprintf("'%s' (empty or truncated)", nm))
        next
      }
      # G3 -- filename must match the handler (mirrors matches_filename()).
      if (!handler_matches(h, nm)) {
        rejected <- c(rejected,
                      sprintf("'%s' (name does not match %s)", nm, handler_expected(h)))
        next
      }
      # G4 -- bind via load_file(). Shiny stores the upload under a temp name
      # like "0.csv"; matches_filename()/read_file() key off basename(file), so
      # stage the bytes under the ORIGINAL name first, else a valid file is
      # rejected inside load_file() (failure mode F7).
      staged <- dta_stage_upload(dp, nm)
      before <- dta_dataset_content_count(dta_get_dataset(rv$dta, dsname))
      res <- dta_load_file(
        rv$dta, dataset = dsname, file = staged, handler_index = hi, name = tbl
      )
      if (!res$ok) {
        rejected <- c(rejected, sprintf("'%s' (%s)", nm, res$error))
        next
      }
      # G5 -- VERIFY the file landed. A new file increases the count; an
      # overwrite replaces in place (count unchanged) and is still valid.
      after <- dta_dataset_content_count(dta_get_dataset(res$value, dsname))
      if (after <= before && !was_existing) {
        rejected <- c(rejected, sprintf("'%s' (could not be bound)", nm))
        next
      }
      rv$dta <- res$value
      # Overwrite -> clear the stale validation status of the replaced table.
      if (was_existing) {
        cv <- dta_clear_validation(rv$dta, dsname, tables = tbl)
        if (cv$ok) rv$dta <- cv$value
        overwritten <- c(overwritten, tbl)
      }
      loaded <- c(loaded, nm)
      loaded_recs[[length(loaded_recs) + 1L]] <- list(file = nm, table = tbl)
      existing <- unique(c(existing, tbl))
    }

    # Record only VERIFIED binds; changed data must be re-validated.
    if (length(loaded) > 0) {
      up <- rv$uploads
      cur <- up[[key]] %||% list()
      new_tbls <- vapply(loaded_recs, function(x) x$table, character(1))
      cur <- Filter(function(x) !((x$table %||% "") %in% new_tbls), cur)
      up[[key]] <- c(cur, loaded_recs)
      rv$uploads <- up
      st <- rv$status
      st[[dsname]] <- "pending"
      rv$status <- st
      autosave()
    }

    # ONE reconciled outcome (Contract C1) with honest counts (C8): never a
    # success message when nothing was actually bound.
    ow <- if (length(overwritten) > 0)
      sprintf(" (%d overwritten)", length(overwritten)) else ""
    if (length(loaded) > 0 && length(rejected) == 0) {
      showNotification(
        sprintf("Loaded %d file(s) into '%s'%s. Run Check to validate.",
                length(loaded), dsname, ow),
        type = "message"
      )
    } else if (length(loaded) > 0) {
      showNotification(
        sprintf("Loaded %d file(s) into '%s'%s; rejected %d: %s",
                length(loaded), dsname, ow, length(rejected),
                paste(rejected, collapse = "; ")),
        type = "warning", duration = 10
      )
    } else {
      showNotification(
        sprintf("No files added to '%s'. Rejected %d: %s",
                dsname, length(rejected), paste(rejected, collapse = "; ")),
        type = "warning", duration = 10
      )
    }

    # Clear the fileInput's text field so the control reads empty and is ready
    # for the next drop (runs whether files were accepted or rejected).
    session$sendCustomMessage("dta_reset_fileinput", sprintf("up_%d_%d", ds_idx, hi))
  }

  # Open a modal listing the bundled example files (inst/extdata). The chosen
  # file is loaded into the slot recorded in rv$example_target (set when that
  # slot's button was clicked) and confirmed via input$example_pick_confirm.
  show_example_modal <- function() {
    files <- dta_example_data_files()
    if (length(files) == 0) {
      showNotification("No example files are bundled with this app.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Load a bundled example file",
      div(class = "msg-hint", style = "margin-bottom:10px;",
          "Pick one of the bundled example files. It is validated against the ",
          "expected file name exactly as if you had uploaded it yourself."),
      radioButtons("example_pick_choice", label = NULL,
                   choices = files, selected = character(0), width = "100%"),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("example_pick_confirm", "Load file", class = "btn btn-primary")
      )
    ))
  }

  # Populate a slot from a bundled example file (inst/extdata) instead of an
  # upload. Builds the same fileinfo shape Shiny's fileInput produces and reuses
  # the exact upload pipeline (handler match, count/overwrite gates, binding).
  handle_example_pick <- function(ds_idx, hi, sel) {
    if (is.null(sel) || length(sel) == 0 || !nzchar(sel)) return()
    path <- dta_example_data_path(sel)
    if (!nzchar(path) || !file.exists(path)) {
      showNotification(sprintf("Example file '%s' is not available.", sel), type = "error")
      return()
    }
    fileinfo <- data.frame(
      name = basename(sel),
      size = as.numeric(file.size(path)),
      type = "",
      datapath = path,
      stringsAsFactors = FALSE
    )
    handle_upload(ds_idx, hi, fileinfo)
  }

  observeEvent(rv$structure, {
    req(rv$structure)
    for (dsname in names(rv$structure)) {
      s <- rv$structure[[dsname]]
      for (hi in seq_along(s$handlers)) {
        upid <- sprintf("up_%d_%d", s$index, hi)
        if (is.null(upload_registry[[upid]])) {
          upload_registry[[upid]] <- TRUE
          local({
            UP <- upid; DSIDX <- s$index; HI <- hi
            observeEvent(input[[UP]], {
              handle_upload(DSIDX, HI, input[[UP]])
            }, ignoreInit = TRUE)
          })
        }
        exid <- sprintf("expick_%d_%d", s$index, hi)
        if (is.null(upload_registry[[exid]])) {
          upload_registry[[exid]] <- TRUE
          local({
            EX <- exid; DSIDX <- s$index; HI <- hi
            observeEvent(input[[EX]], {
              rv$example_target <- list(ds_idx = DSIDX, hi = HI)
              show_example_modal()
            }, ignoreInit = TRUE)
          })
        }
      }
    }
  })

  # --- overwrite confirmation (deferred upload) --------------------------
  observeEvent(input$confirm_overwrite, {
    pu <- rv$pending_upload
    rv$pending_upload <- NULL
    removeModal()
    if (!is.null(pu)) handle_upload(pu$ds_idx, pu$hi, pu$fileinfo, overwrite = TRUE)
  })
  observeEvent(input$cancel_overwrite, {
    rv$pending_upload <- NULL
    removeModal()
  })

  # Confirm the example-file modal: load the chosen file into the target slot.
  observeEvent(input$example_pick_confirm, {
    tgt <- rv$example_target
    sel <- input$example_pick_choice
    removeModal()
    rv$example_target <- NULL
    if (is.null(tgt) || is.null(sel) || length(sel) == 0 || !nzchar(sel)) return()
    handle_example_pick(tgt$ds_idx, tgt$hi, sel)
  })

  # --- remove one loaded file / discard all ------------------------------
  do_remove_file <- function(dsname, hi, table) {
    req(rv$dta)
    r <- dta_unload_table(rv$dta, dsname, table)
    if (!r$ok) {
      showNotification(paste("Could not remove file:", r$error), type = "error")
      return()
    }
    rv$dta <- r$value
    key <- paste0(dsname, "||", hi)
    up <- rv$uploads
    up[[key]] <- Filter(function(x) !identical(x$table, table), up[[key]] %||% list())
    rv$uploads <- up
    cnt <- dta_dataset_content_count(dta_get_dataset(rv$dta, dsname))
    st <- rv$status
    st[[dsname]] <- if (cnt == 0) "nodata" else "pending"
    rv$status <- st
    autosave()
    showNotification(sprintf("Removed '%s' from '%s'.", table, dsname), type = "message")
  }

  # Register one remove-observer per bound file (stable id per ds|slot|table).
  observe({
    up <- rv$uploads
    req(rv$dta)
    for (key in names(up)) {
      parts <- strsplit(key, "||", fixed = TRUE)[[1]]
      if (length(parts) < 2) next
      dsname <- parts[1]
      hi <- suppressWarnings(as.integer(parts[2]))
      for (rec in (up[[key]] %||% list())) {
        fid <- get_file_id(dsname, hi, rec$table)
        bid <- paste0("rmfile_", fid)
        if (is.null(file_rm_registry[[bid]])) {
          file_rm_registry[[bid]] <- TRUE
          local({
            BID <- bid
            META <- file_id_meta[[as.character(fid)]]
            observeEvent(input[[BID]], {
              do_remove_file(META$dataset, META$hi, META$table)
            }, ignoreInit = TRUE)
          })
        }
      }
    }
  })

  observeEvent(input$discard_all, {
    req(rv$active)
    showModal(modalDialog(
      title = "Discard all loaded files?",
      sprintf("Remove all loaded files from '%s'? You will need to upload them again.",
              rv$active),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_discard_all", "Discard all", class = "btn btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  observeEvent(input$confirm_discard_all, {
    req(rv$active)
    r <- dta_unload_all(rv$dta, rv$active)
    if (r$ok) {
      rv$dta <- r$value
      up <- rv$uploads
      pref <- paste0(rv$active, "||")
      for (k in names(up)) if (startsWith(k, pref)) up[[k]] <- list()
      rv$uploads <- up
      st <- rv$status; st[[rv$active]] <- "nodata"; rv$status <- st
      autosave()
      showNotification(sprintf("Discarded all files from '%s'.", rv$active), type = "message")
    } else {
      showNotification(r$error, type = "error")
    }
    removeModal()
  })

  # --- validation ---------------------------------------------------------
  run_check <- function(dataset = NULL) {
    req(rv$dta)
    names_ds <- dta_dataset_names(rv$dta)
    targets <- if (is.null(dataset)) names_ds else intersect(dataset, names_ds)
    if (length(targets) == 0) return()

    # C7 -- pre-flight: only validate datasets that actually have data bound.
    ready <- Filter(function(nm) dta_dataset_readiness(rv$dta, nm)$has_data, targets)
    nodata <- setdiff(targets, ready)

    # Mark no-data datasets explicitly; they are never reported as passed (C5).
    if (length(nodata) > 0) {
      st <- rv$status
      for (nm in nodata) st[[nm]] <- "nodata"
      rv$status <- st
    }

    if (length(ready) == 0) {
      msg <- if (length(targets) == 1) {
        sprintf("'%s' has no data loaded \u2014 upload the required file(s) before validating.",
                targets[1])
      } else {
        "No selected dataset has data loaded yet \u2014 upload files before validating."
      }
      showNotification(msg, type = "warning", duration = 8)
      return()
    }

    ok <- FALSE
    err <- NULL
    withProgress(message = "Validating\u2026", value = 0.4, {
      res <- dta_check(rv$dta, dataset = ready)
      setProgress(0.8)
      if (res$ok) {
        rv$dta <- res$value
        rv$status <- dta_status_map(rv$dta)
        ok <- TRUE
      } else {
        err <- res$error
      }
      setProgress(1)
    })

    if (!ok) {
      showNotification(paste("Validation error:", err), type = "error", duration = 10)
      return()
    }
    autosave()

    # ONE reconciled summary (C1/C6) derived from the verified status map.
    st <- unlist(rv$status)
    n_pass <- sum(st[ready] == "pass", na.rm = TRUE)
    n_fail <- sum(st[ready] == "fail", na.rm = TRUE)
    parts <- character(0)
    if (n_pass > 0) parts <- c(parts, sprintf("%d passed", n_pass))
    if (n_fail > 0) parts <- c(parts, sprintf("%d failed", n_fail))
    if (length(nodata) > 0) parts <- c(parts, sprintf("%d skipped (no data)", length(nodata)))
    if (length(parts) == 0) parts <- "nothing to validate"
    showNotification(
      sprintf("Validation complete \u2014 %s.", paste(parts, collapse = ", ")),
      type = if (n_fail > 0) "warning" else "message"
    )

    # Reveal the floating messages dock when the active dataset now has any
    # validation messages to show.
    active_has_msgs <- tryCatch({
      m <- dta_dataset_messages(rv$dta, rv$active)
      !is.null(m) && nrow(m) > 0
    }, error = function(e) FALSE)
    if (isTRUE(active_has_msgs)) {
      session$sendCustomMessage("dta_msgs_dock", "open")
    }
  }

  observeEvent(input$check_all, run_check(NULL))
  observeEvent(input$check_one, {
    req(rv$active)
    run_check(rv$active)
  })

  # After a spec edit, refresh the in-memory YAML text + editor and persist.
  sync_yaml_text <- function() {
    res <- if (isolate(rv$dataset_only)) {
      dta_dataset_to_yaml_text(isolate(rv$dta), isolate(rv$active))
    } else {
      dta_to_yaml_text(isolate(rv$dta))
    }
    if (isTRUE(res$ok)) {
      rv$yaml_text <- res$value
      set_yaml_editor(res$value)
    }
    autosave()
  }
  # A spec change invalidates only THIS dataset's validation.
  invalidate_dataset <- function(ed) {
    cv <- dta_clear_validation(rv$dta, ed)
    if (isTRUE(cv$ok)) rv$dta <- cv$value
    st <- rv$status
    st[[ed]] <- if (dta_dataset_content_count(dta_get_dataset(rv$dta, ed)) == 0) {
      "nodata"
    } else {
      "pending"
    }
    rv$status <- st
  }

  # Per-row edit (pencil) + delete (bin) buttons for the editor DT tables.
  # Clicking sets a Shiny input to the 1-based data-row index (priority=event
  # so re-clicking the same row still fires). Render the column with escape=FALSE.
  row_action_buttons <- function(edit_input, del_input, n,
                                 up_input = NULL, down_input = NULL) {
    vapply(seq_len(n), function(i) {
      up_btn <- if (is.null(up_input)) "" else if (i > 1L) {
        sprintf(
          "<button class=\"btn btn-sm btn-outline-secondary dta-row-btn\" title=\"Move up\" onclick=\"Shiny.setInputValue('%s', %d, {priority:'event'})\">&#x25B2;</button> ",
          up_input, i)
      } else {
        "<button class=\"btn btn-sm btn-outline-secondary dta-row-btn\" title=\"Move up\" disabled>&#x25B2;</button> "
      }
      down_btn <- if (is.null(down_input)) "" else if (i < n) {
        sprintf(
          "<button class=\"btn btn-sm btn-outline-secondary dta-row-btn\" title=\"Move down\" onclick=\"Shiny.setInputValue('%s', %d, {priority:'event'})\">&#x25BC;</button> ",
          down_input, i)
      } else {
        "<button class=\"btn btn-sm btn-outline-secondary dta-row-btn\" title=\"Move down\" disabled>&#x25BC;</button> "
      }
      paste0(
        up_btn, down_btn,
        sprintf(
          "<button class=\"btn btn-sm btn-outline-secondary dta-row-btn\" title=\"Edit\" onclick=\"Shiny.setInputValue('%s', %d, {priority:'event'})\">&#x270E;</button> ",
          edit_input, i),
        sprintf(
          "<button class=\"btn btn-sm btn-outline-danger dta-row-btn\" title=\"Remove\" onclick=\"Shiny.setInputValue('%s', %d, {priority:'event'})\">&#x1F5D1;</button>",
          del_input, i)
      )
    }, character(1))
  }

  # ============================ Edit columns ==============================
  # A single modal with two swappable views (list <-> form) so only ONE popup
  # is ever open. rv$col_view drives which view renders; rv$col_token forces a
  # re-render. The form pre-fills from rv$col_prefill (never stale inputs).
  observeEvent(input$edit_cols, {
    req(rv$active)
    rv$editor_dataset <- rv$active
    rv$col_view <- "list"
    rv$col_edit_id <- NULL
    rv$col_prefill <- list()
    rv$col_msg <- NULL
    rv$col_token <- rv$col_token + 1
    showModal(modalDialog(
      title = paste("Edit columns \u2014", rv$active),
      size = "xl", easyClose = FALSE,
      uiOutput("col_modal_body"),
      footer = NULL
    ))
  })

  output$col_modal_body <- renderUI({
    rv$col_token
    ed <- isolate(rv$editor_dataset)
    req(ed)
    if (identical(isolate(rv$col_view), "list")) {
      tagList(
        div(
          class = "spec-toolbar",
          actionButton("col_add", HTML("&#x2795; Add column"),
                       class = "btn btn-sm btn-outline-primary"),
          span(class = "spec-hint",
               "Use the pencil to edit a column or the bin to remove it. Any change resets this dataset's validation.")
        ),
        DT::dataTableOutput("col_tbl"),
        tags$hr(),
        div(style = "text-align:right;", modalButton("Close"))
      )
    } else {
      pf <- isolate(rv$col_prefill) %||% list()
      g <- function(k, d = "") pf[[k]] %||% d
      tagList(
        div(
          class = "spec-form",
          layout_columns(
            col_widths = c(4, 8),
            textInput("col_id", "Column ID", value = g("id"), width = "100%"),
            textInput("col_label", "Label", value = g("label"), width = "100%")
          ),
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            selectInput("col_backend", "Backend", choices = dta_supported_backends(),
                        selected = g("backend", dta_supported_backends()[1]), width = "100%"),
            selectInput("col_type", "Type", choices = dta_sas_types(),
                        selected = g("type", "Char"), width = "100%"),
            textInput("col_format", "Format", value = g("format"), width = "100%",
                      placeholder = "e.g. $9. / 8.2"),
            textInput("col_length", "Length", value = g("length"), width = "100%",
                      placeholder = "e.g. 8")
          ),
          checkboxInput("col_nullable", "Nullable (missing values allowed)",
                        value = if (is.null(pf$nullable)) TRUE else isTRUE(pf$nullable)),
          layout_columns(
            col_widths = c(6, 6),
            textAreaInput("col_values", "Allowed values (one per line)",
                          value = g("values"), width = "100%", rows = 3),
            textInput("col_pattern", "Pattern (regex)", value = g("pattern"), width = "100%")
          ),
          textAreaInput("col_desc", "Description", value = g("description"), width = "100%", rows = 2)
        ),
        uiOutput("col_editor_msg"),
        div(
          style = "display:flex; justify-content:space-between; margin-top:8px;",
          actionButton("col_back", HTML("&#x2190; Back to list"),
                       class = "btn btn-outline-secondary"),
          actionButton("col_save", "Save column", class = "btn btn-primary")
        )
      )
    }
  })

  output$col_tbl <- DT::renderDataTable({
    rv$col_token
    ed <- isolate(rv$editor_dataset)
    req(ed)
    ov <- dta_columns_overview(isolate(rv$dta), ed)
    if (is.null(ov) || nrow(ov) == 0) {
      ov <- data.frame(id = character(0), label = character(0), type = character(0),
                       length = character(0), nullable = character(0),
                       constraint = character(0), description = character(0),
                       stringsAsFactors = FALSE)
    }
    ov$Actions <- if (nrow(ov) > 0) {
      row_action_buttons("col_edit_click", "col_del_click", nrow(ov),
                         "col_up_click", "col_down_click")
    } else character(0)
    DT::datatable(
      ov, rownames = FALSE, selection = "none", escape = FALSE,
      class = "display compact", width = "100%",
      options = list(pageLength = 8, dom = "tp", scrollX = TRUE,
                     columnDefs = list(list(orderable = FALSE, targets = ncol(ov) - 1L)))
    )
  })

  output$col_editor_msg <- renderUI({
    m <- rv$col_msg
    if (is.null(m)) return(NULL)
    if (isTRUE(m$ok)) {
      div(class = "yaml-valid ok", HTML("&#x2714;"), " Column saved.")
    } else {
      div(class = "yaml-valid err", HTML("&#x2716;"), " ", m$error)
    }
  })

  observeEvent(input$col_add, {
    rv$col_edit_id <- NULL
    rv$col_prefill <- list()
    rv$col_msg <- NULL
    rv$col_view <- "form"
    rv$col_token <- rv$col_token + 1
  })

  observeEvent(input$col_back, {
    rv$col_view <- "list"
    rv$col_msg <- NULL
    rv$col_token <- rv$col_token + 1
  })

  observeEvent(input$col_edit_click, {
    idx <- as.integer(input$col_edit_click)
    ed <- isolate(rv$editor_dataset)
    req(ed)
    ids <- dta_column_ids(isolate(rv$dta), ed)
    if (length(idx) != 1 || is.na(idx) || idx < 1 || idx > length(ids)) return()
    f <- dta_column_fields(isolate(rv$dta), ed, ids[[idx]])
    if (is.null(f)) return()
    rv$col_edit_id <- ids[[idx]]
    rv$col_prefill <- f
    rv$col_msg <- NULL
    rv$col_view <- "form"
    rv$col_token <- rv$col_token + 1
  })

  observeEvent(input$col_del_click, {
    idx <- as.integer(input$col_del_click)
    ed <- isolate(rv$editor_dataset)
    req(ed)
    ids <- dta_column_ids(isolate(rv$dta), ed)
    if (length(idx) != 1 || is.na(idx) || idx < 1 || idx > length(ids)) return()
    r <- dta_remove_column(isolate(rv$dta), ed, ids[[idx]])
    if (!isTRUE(r$ok)) {
      showNotification(r$error, type = "error")
      return()
    }
    rv$dta <- r$value
    invalidate_dataset(ed)
    rv$col_token <- rv$col_token + 1
    sync_yaml_text()
    showNotification(sprintf("Removed column \u201c%s\u201d.", ids[[idx]]), type = "message")
  })

  observeEvent(input$col_up_click, {
    idx <- as.integer(input$col_up_click)
    ed <- isolate(rv$editor_dataset)
    req(ed)
    ids <- dta_column_ids(isolate(rv$dta), ed)
    if (length(idx) != 1 || is.na(idx) || idx <= 1 || idx > length(ids)) return()
    r <- dta_move_column(isolate(rv$dta), ed, ids[[idx]], "up")
    if (!isTRUE(r$ok)) {
      showNotification(r$error, type = "error")
      return()
    }
    rv$dta <- r$value
    invalidate_dataset(ed)
    rv$col_token <- rv$col_token + 1
    sync_yaml_text()
  })

  observeEvent(input$col_down_click, {
    idx <- as.integer(input$col_down_click)
    ed <- isolate(rv$editor_dataset)
    req(ed)
    ids <- dta_column_ids(isolate(rv$dta), ed)
    if (length(idx) != 1 || is.na(idx) || idx < 1 || idx >= length(ids)) return()
    r <- dta_move_column(isolate(rv$dta), ed, ids[[idx]], "down")
    if (!isTRUE(r$ok)) {
      showNotification(r$error, type = "error")
      return()
    }
    rv$dta <- r$value
    invalidate_dataset(ed)
    rv$col_token <- rv$col_token + 1
    sync_yaml_text()
  })

  observeEvent(input$col_save, {
    ed <- isolate(rv$editor_dataset)
    req(ed)
    id <- trimws(input$col_id %||% "")
    if (!nzchar(id)) {
      rv$col_msg <- list(ok = FALSE, error = "A column ID is required.")
      return()
    }
    vals <- trimws(strsplit(input$col_values %||% "", "\n")[[1]])
    vals <- vals[nzchar(vals)]
    r <- dta_set_column(
      isolate(rv$dta), ed, id = id, label = input$col_label,
      backend = input$col_backend, type = input$col_type,
      format = input$col_format, length = input$col_length,
      nullable = isTRUE(input$col_nullable),
      values = if (length(vals)) vals else NULL,
      pattern = input$col_pattern, description = input$col_desc,
      old_id = isolate(rv$col_edit_id)
    )
    if (!isTRUE(r$ok)) {
      rv$col_msg <- list(ok = FALSE, error = r$error)
      return()
    }
    rv$dta <- r$value
    invalidate_dataset(ed)
    rv$col_msg <- NULL
    rv$col_view <- "list"
    rv$col_token <- rv$col_token + 1
    sync_yaml_text()
    showNotification("Column saved.", type = "message")
  })

  # ============================= Edit rules ===============================
  # One editable condition row (Column / Operator / Value). The value field is
  # a single text box whose meaning depends on the operator (see the hint under
  # the builder). Values come from the prefill only (fresh renders happen on
  # token bumps where prefill is authoritative; insertUI rows keep DOM state).
  one_cond_row <- function(prefix, i, cols, pf = list(col = "", op = "equals", val = "")) {
    cid <- paste0(prefix, "_col_", i)
    oid <- paste0(prefix, "_op_", i)
    vid <- paste0(prefix, "_val_", i)
    div(
      class = "cond-row",
      selectInput(cid, if (i == 1) "Column" else NULL,
                  choices = c("(select)" = "", cols),
                  selected = pf$col %||% "", width = "100%"),
      selectInput(oid, if (i == 1) "Operator" else NULL,
                  choices = dta_condition_operators(),
                  selected = pf$op %||% "equals", width = "100%"),
      textInput(vid, if (i == 1) "Value" else NULL,
                value = pf$val %||% "", width = "100%",
                placeholder = "5 | a, b | 0, 99 | true")
    )
  }
  build_cond_rows <- function(prefix, n, cols, prefill) {
    lapply(seq_len(max(1L, n)), function(i) {
      pf <- if (i <= length(prefill)) prefill[[i]] else list(col = "", op = "equals", val = "")
      one_cond_row(prefix, i, cols, pf)
    })
  }
  # named-list {COL:{op:val}} -> list of editable rows. "min"/"max" keys (single
  # or combined) map to the UI "min_max" operator with a "min, max" value.
  cond_to_rows <- function(cond) {
    if (is.null(cond) || length(cond) == 0) return(list())
    lapply(names(cond), function(col) {
      spec <- cond[[col]]
      if (is.list(spec) && length(spec) > 0) {
        keys <- names(spec)
        if ("min" %in% keys || "max" %in% keys) {
          op <- "min_max"
          mn <- if ("min" %in% keys) as.character(spec[["min"]]) else ""
          mx <- if ("max" %in% keys) as.character(spec[["max"]]) else ""
          val <- sub(",\\s*$", "", paste0(mn, ", ", mx))
        } else {
          op <- keys[1]
          raw <- spec[[1]]
          val <- if (identical(op, "empty")) {
            if (isTRUE(raw)) "true" else "false"
          } else {
            paste(as.character(unlist(raw)), collapse = ", ")
          }
        }
      } else {
        op <- "equals"
        val <- as.character(spec)
      }
      list(col = col, op = op, val = val)
    })
  }
  parse_cond_value <- function(op, txt) {
    txt <- txt %||% ""
    if (identical(op, "empty")) {
      v <- tolower(trimws(txt))
      return(!(v %in% c("false", "no", "0", "f", "n")))  # default TRUE
    }
    if (identical(op, "pattern")) return(as.character(txt))
    if (op %in% c("in", "not_in")) {
      parts <- trimws(strsplit(txt, ",")[[1]])
      parts <- parts[nzchar(parts)]
      if (length(parts) == 0) return(character(0))
      nums <- suppressWarnings(as.numeric(parts))
      if (all(!is.na(nums))) return(nums)
      return(parts)
    }
    if (!nzchar(trimws(txt))) return("")
    num <- suppressWarnings(as.numeric(txt))
    if (!is.na(num)) return(num)
    txt
  }
  collect_cond <- function(prefix, n) {
    out <- list()
    for (i in seq_len(max(1L, n))) {
      col <- trimws(input[[paste0(prefix, "_col_", i)]] %||% "")
      if (!nzchar(col)) next
      op <- input[[paste0(prefix, "_op_", i)]] %||% "equals"
      txt <- input[[paste0(prefix, "_val_", i)]]
      inner <- list()
      if (identical(op, "min_max")) {
        # Positional "min, max" parse (no nzchar filter so ", 9" means max-only).
        parts <- trimws(strsplit(txt %||% "", ",", fixed = TRUE)[[1]])
        mn <- if (length(parts) >= 1 && nzchar(parts[1])) suppressWarnings(as.numeric(parts[1])) else NA_real_
        mx <- if (length(parts) >= 2 && nzchar(parts[2])) suppressWarnings(as.numeric(parts[2])) else NA_real_
        if (!is.na(mn)) inner$min <- mn
        if (!is.na(mx)) inner$max <- mx
        if (length(inner) == 0) next
      } else {
        inner[[op]] <- parse_cond_value(op, txt)
      }
      out[[col]] <- inner
    }
    out
  }

  # A single modal with two swappable views (list <-> form) so only ONE popup is
  # ever open. rv$rule_view drives the view; rv$rule_token forces re-renders.
  observeEvent(input$edit_rules, {
    req(rv$active)
    rv$editor_dataset <- rv$active
    rv$rule_view <- "list"
    rv$rule_edit_index <- NULL
    rv$rule_prefill <- list()
    rv$rule_msg <- NULL
    rv$cond_n <- 1L
    rv$then_n <- 1L
    rv$rule_token <- rv$rule_token + 1
    showModal(modalDialog(
      title = paste("Edit rules \u2014", rv$active),
      size = "xl", easyClose = FALSE,
      uiOutput("rule_modal_body"),
      footer = NULL
    ))
  })

  output$rule_modal_body <- renderUI({
    rv$rule_token
    ed <- isolate(rv$editor_dataset)
    req(ed)
    if (identical(isolate(rv$rule_view), "list")) {
      return(tagList(
        div(
          class = "spec-toolbar",
          actionButton("rule_add", HTML("&#x2795; Add rule"),
                       class = "btn btn-sm btn-outline-primary"),
          span(class = "spec-hint",
               "Use the pencil to edit a rule or the bin to remove it. Any change resets this dataset's validation.")
        ),
        DT::dataTableOutput("rule_tbl"),
        tags$hr(),
        div(style = "text-align:right;", modalButton("Close"))
      ))
    }
    pf <- isolate(rv$rule_prefill) %||% list()
    is_edit <- !is.null(isolate(rv$rule_edit_index))
    rt <- pf$type %||% ""
    cols <- dta_column_ids(isolate(rv$dta), ed)
    type_ui <- if (identical(rt, "col_condition")) {
      tagList(
        div(
          class = "cond-builder",
          div(class = "cond-title", "IF (all of these hold):"),
          div(id = "cond_rows",
              build_cond_rows("cond", isolate(rv$cond_n), cols, cond_to_rows(pf$condition))),
          actionButton("cond_add", HTML("&#x2795; Add condition"),
                       class = "btn btn-sm btn-outline-secondary")
        ),
        div(
          class = "cond-builder",
          div(class = "cond-title", "THEN (all of these must hold):"),
          div(id = "then_rows",
              build_cond_rows("then", isolate(rv$then_n), cols, cond_to_rows(pf$then))),
          actionButton("then_add", HTML("&#x2795; Add THEN condition"),
                       class = "btn btn-sm btn-outline-secondary")
        ),
        div(class = "cond-hint", HTML(paste0(
          "Value formats &mdash; single: <code>5</code> &middot; ",
          "list (in / not in): <code>a, b, c</code> &middot; ",
          "between (min/max): <code>0, 99</code> &middot; ",
          "empty: <code>true</code> / <code>false</code> &middot; ",
          "pattern: a regular expression."
        )))
      )
    } else if (identical(rt, "col_range")) {
      tagList(
        selectInput("rule_col_single", "Column",
                    choices = c("(select)" = "", cols),
                    selected = (pf$columns %||% "")[1], width = "100%"),
        layout_columns(
          col_widths = c(6, 6),
          textInput("rule_min", "Minimum", value = pf$min %||% "", width = "100%"),
          textInput("rule_max", "Maximum", value = pf$max %||% "", width = "100%")
        ),
        div(class = "cond-hint",
            "A range rule checks ONE column against a minimum and/or maximum.")
      )
    } else if (identical(rt, "col_unique")) {
      tagList(
        selectizeInput("rule_cols", "Column(s) that are unique together",
                       choices = cols, selected = pf$columns, multiple = TRUE, width = "100%"),
        div(class = "cond-hint",
            "Rows must be unique across the selected column(s) taken together.")
      )
    } else {
      div(class = "cond-hint",
          "Choose a rule type above to configure it.")
    }
    # The rule type is chosen only when the rule is created. When editing an
    # existing rule the type is locked (shown read-only) so it cannot change.
    type_field <- if (is_edit) {
      div(
        class = "form-group shiny-input-container", style = "width:100%;",
        tags$label(class = "control-label", "Type"),
        div(class = "rule-type-fixed", dta_rule_type_label(rt))
      )
    } else {
      selectInput("rule_type", "Type",
                  choices = c("\u2014 select a rule type \u2014" = "",
                              "Conditional (IF/THEN)" = "col_condition",
                              "Range" = "col_range",
                              "Unique" = "col_unique"),
                  selected = rt, width = "100%")
    }
    tagList(
      div(
        class = "spec-form",
        layout_columns(
          col_widths = c(4, 4, 4),
          textInput("rule_id", "Rule ID", value = pf$id %||% "", width = "100%"),
          type_field,
          textInput("rule_desc", "Description", value = pf$description %||% "", width = "100%")
        ),
        type_ui
      ),
      uiOutput("rule_editor_msg"),
      div(
        style = "display:flex; justify-content:space-between; margin-top:8px;",
        actionButton("rule_back", HTML("&#x2190; Back to list"),
                     class = "btn btn-outline-secondary"),
        actionButton("rule_save", "Save rule", class = "btn btn-primary")
      )
    )
  })

  output$rule_tbl <- DT::renderDataTable({
    rv$rule_token
    ed <- isolate(rv$editor_dataset)
    req(ed)
    ov <- dta_rules_overview(isolate(rv$dta), ed)
    if (is.null(ov) || nrow(ov) == 0) {
      ov <- data.frame(index = integer(0), id = character(0), type = character(0),
                       detail = character(0), description = character(0),
                       stringsAsFactors = FALSE)
    }
    ov$Actions <- if (nrow(ov) > 0) {
      row_action_buttons("rule_edit_click", "rule_del_click", nrow(ov),
                         "rule_up_click", "rule_down_click")
    } else character(0)
    DT::datatable(
      ov, rownames = FALSE, selection = "none", escape = FALSE,
      class = "display compact", width = "100%",
      options = list(pageLength = 8, dom = "tp", scrollX = TRUE,
                     columnDefs = list(list(orderable = FALSE, targets = ncol(ov) - 1L)))
    )
  })

  output$rule_editor_msg <- renderUI({
    m <- rv$rule_msg
    if (is.null(m)) return(NULL)
    if (isTRUE(m$ok)) {
      div(class = "yaml-valid ok", HTML("&#x2714;"), " Rule saved.")
    } else {
      div(class = "yaml-valid err", HTML("&#x2716;"), " ", m$error)
    }
  })

  observeEvent(input$rule_add, {
    # New rule: the user must first pick a type (no default), so the type-
    # specific fields only appear once a type is chosen.
    rv$rule_edit_index <- NULL
    rv$rule_prefill <- list()
    rv$rule_msg <- NULL
    rv$cond_n <- 1L
    rv$then_n <- 1L
    rv$rule_view <- "form"
    rv$rule_token <- rv$rule_token + 1
  })

  observeEvent(input$rule_back, {
    rv$rule_view <- "list"
    rv$rule_msg <- NULL
    rv$rule_token <- rv$rule_token + 1
  })

  observeEvent(input$rule_edit_click, {
    idx <- as.integer(input$rule_edit_click)
    ed <- isolate(rv$editor_dataset)
    req(ed)
    if (length(idx) != 1 || is.na(idx)) return()
    f <- dta_rule_fields(isolate(rv$dta), ed, idx)
    if (is.null(f)) return()
    rv$rule_edit_index <- idx
    rv$rule_prefill <- f
    rv$rule_msg <- NULL
    rv$cond_n <- max(1L, length(cond_to_rows(f$condition)))
    rv$then_n <- max(1L, length(cond_to_rows(f$then)))
    rv$rule_view <- "form"
    rv$rule_token <- rv$rule_token + 1
  })

  observeEvent(input$rule_del_click, {
    idx <- as.integer(input$rule_del_click)
    ed <- isolate(rv$editor_dataset)
    req(ed)
    if (length(idx) != 1 || is.na(idx)) return()
    r <- dta_remove_rule(isolate(rv$dta), ed, idx)
    if (!isTRUE(r$ok)) {
      showNotification(r$error, type = "error")
      return()
    }
    rv$dta <- r$value
    invalidate_dataset(ed)
    rv$rule_token <- rv$rule_token + 1
    sync_yaml_text()
    showNotification("Rule removed.", type = "message")
  })

  observeEvent(input$rule_up_click, {
    idx <- as.integer(input$rule_up_click)
    ed <- isolate(rv$editor_dataset)
    req(ed)
    if (length(idx) != 1 || is.na(idx) || idx <= 1) return()
    r <- dta_move_rule(isolate(rv$dta), ed, idx, "up")
    if (!isTRUE(r$ok)) {
      showNotification(r$error, type = "error")
      return()
    }
    rv$dta <- r$value
    invalidate_dataset(ed)
    rv$rule_token <- rv$rule_token + 1
    sync_yaml_text()
  })

  observeEvent(input$rule_down_click, {
    idx <- as.integer(input$rule_down_click)
    ed <- isolate(rv$editor_dataset)
    req(ed)
    n <- nrow(dta_rules_overview(isolate(rv$dta), ed))
    if (length(idx) != 1 || is.na(idx) || idx < 1 || idx >= n) return()
    r <- dta_move_rule(isolate(rv$dta), ed, idx, "down")
    if (!isTRUE(r$ok)) {
      showNotification(r$error, type = "error")
      return()
    }
    rv$dta <- r$value
    invalidate_dataset(ed)
    rv$rule_token <- rv$rule_token + 1
    sync_yaml_text()
  })

  observeEvent(input$cond_add, {
    ed <- isolate(rv$editor_dataset)
    req(ed)
    cols <- dta_column_ids(isolate(rv$dta), ed)
    rv$cond_n <- isolate(rv$cond_n) + 1L
    insertUI("#cond_rows", where = "beforeEnd",
             ui = one_cond_row("cond", isolate(rv$cond_n), cols))
  })
  observeEvent(input$then_add, {
    ed <- isolate(rv$editor_dataset)
    req(ed)
    cols <- dta_column_ids(isolate(rv$dta), ed)
    rv$then_n <- isolate(rv$then_n) + 1L
    insertUI("#then_rows", where = "beforeEnd",
             ui = one_cond_row("then", isolate(rv$then_n), cols))
  })

  # When the user switches the rule type, reset the type-specific part but keep
  # the id/description they typed. A programmatic sync (the select rendering with
  # its prefilled value) is ignored by comparing against the prefill's type.
  observeEvent(input$rule_type, {
    if (!identical(isolate(rv$rule_view), "form")) return()
    newt <- input$rule_type %||% ""
    pf <- isolate(rv$rule_prefill) %||% list()
    if (!nzchar(newt)) return()                    # ignore the "(select)" placeholder
    if (identical(newt, pf$type %||% "")) return()  # no real change
    pf$id <- input$rule_id %||% pf$id
    pf$description <- input$rule_desc %||% pf$description
    pf$type <- newt
    pf$condition <- NULL
    pf$then <- NULL
    pf$columns <- NULL
    pf$min <- NULL
    pf$max <- NULL
    rv$rule_prefill <- pf
    rv$cond_n <- 1L
    rv$then_n <- 1L
    rv$rule_msg <- NULL
    rv$rule_token <- rv$rule_token + 1
  }, ignoreInit = TRUE)

  observeEvent(input$rule_save, {
    ed <- isolate(rv$editor_dataset)
    req(ed)
    id <- trimws(input$rule_id %||% "")
    if (!nzchar(id)) {
      rv$rule_msg <- list(ok = FALSE, error = "A rule ID is required.")
      return()
    }
    # On edit the type is locked, so read it from the prefill (the select input
    # is not rendered); on add the user must have picked a type.
    rt <- if (!is.null(isolate(rv$rule_edit_index))) {
      isolate(rv$rule_prefill$type) %||% "col_condition"
    } else {
      trimws(input$rule_type %||% "")
    }
    if (!nzchar(rt)) {
      rv$rule_msg <- list(ok = FALSE, error = "Please choose a rule type.")
      return()
    }
    args <- list(dta = isolate(rv$dta), dataset = ed, index = isolate(rv$rule_edit_index),
                 id = id, type = rt, description = input$rule_desc)
    if (identical(rt, "col_condition")) {
      cond <- collect_cond("cond", isolate(rv$cond_n))
      then <- collect_cond("then", isolate(rv$then_n))
      if (length(cond) == 0 || length(then) == 0) {
        rv$rule_msg <- list(ok = FALSE,
                            error = "A conditional rule needs at least one IF and one THEN condition.")
        return()
      }
      args$condition <- cond
      args$then <- then
    } else if (identical(rt, "col_range")) {
      col1 <- trimws(input$rule_col_single %||% "")
      if (!nzchar(col1)) {
        rv$rule_msg <- list(ok = FALSE, error = "Select the column to range-check.")
        return()
      }
      args$columns <- col1
      mn <- suppressWarnings(as.numeric(input$rule_min))
      mx <- suppressWarnings(as.numeric(input$rule_max))
      args$min <- if (!is.na(mn)) mn else NULL
      args$max <- if (!is.na(mx)) mx else NULL
      if (is.null(args$min) && is.null(args$max)) {
        rv$rule_msg <- list(ok = FALSE, error = "A range rule needs a minimum and/or maximum.")
        return()
      }
    } else {
      cols <- input$rule_cols
      if (length(cols) == 0) {
        rv$rule_msg <- list(ok = FALSE, error = "Select at least one column.")
        return()
      }
      args$columns <- cols
    }
    r <- do.call(dta_set_rule, args)
    if (!isTRUE(r$ok)) {
      rv$rule_msg <- list(ok = FALSE, error = r$error)
      return()
    }
    rv$dta <- r$value
    invalidate_dataset(ed)
    rv$rule_msg <- NULL
    rv$rule_view <- "list"
    rv$rule_token <- rv$rule_token + 1
    sync_yaml_text()
    showNotification("Rule saved.", type = "message")
  })

  # --- messages + inspect -------------------------------------------------
  msgs_r <- reactive({
    req(rv$active)
    rv$status # depend on validation state
    dta_dataset_messages(rv$dta, rv$active)
  })

  # Floating, foldable dock that holds the validation messages for the ACTIVE
  # dataset. Rendered once per loaded structure (so the DT inside stays stable);
  # the table + count badge update reactively via their own outputs. The server
  # sends 'dta_msgs_dock' -> 'open' after a check that produced messages.
  output$floating_msgs <- renderUI({
    if (is.null(rv$structure)) return(NULL)
    div(
      id = "dta-msgs-dock", class = "msgs-dock collapsed",
      div(
        class = "msgs-dock-bar", onclick = "DTA_toggleMsgsDock(event)",
        title = "Click to fold or unfold the validation messages",
        tags$span(class = "msgs-dock-title", HTML("&#x2696;&#xFE0F; Validation messages")),
        uiOutput("msgs_dock_meta", inline = TRUE),
        div(
          class = "msgs-dock-actions",
          tags$span(
            class = "msgs-dock-dl", onclick = "event.stopPropagation();",
            downloadButton("dl_msgs_csv", "CSV", class = "btn btn-sm btn-outline-secondary"),
            downloadButton("dl_msgs_tsv", "TSV", class = "btn btn-sm btn-outline-secondary"),
            downloadButton("dl_msgs_xlsx", "XLSX", class = "btn btn-sm btn-outline-secondary")
          ),
          tags$span(class = "msgs-dock-chevron", HTML("&#x25BC;"))
        )
      ),
      div(
        class = "msgs-dock-body",
        div(class = "msgs-table", DT::dataTableOutput("msgs")),
        div(class = "msg-hint",
            "Use the filters at the top of each column to search or pick a Dataset / Table. Click a message row to open the detailed inspect report.")
      )
    )
  })

  # Count badge + active-dataset label shown in the dock header bar.
  output$msgs_dock_meta <- renderUI({
    req(rv$active)
    m <- msgs_r()
    n <- if (is.null(m)) 0L else nrow(m)
    tagList(
      tags$span(class = paste0("msgs-dock-count", if (n == 0) " zero" else ""),
                sprintf("%d", n)),
      tags$span(class = "msgs-dock-ds", rv$active)
    )
  })

  output$msgs <- DT::renderDataTable({
    disp <- messages_display(msgs_r())
    if (is.null(disp) || nrow(disp) == 0) {
      return(DT::datatable(
        data.frame(Message = "No validation messages."),
        rownames = FALSE, options = list(dom = "t"), selection = "none"
      ))
    }
    # Dropdown (select) filters for categorical columns; search boxes for the
    # rest -- lets the user pick a Dataset/Table or type a free-text search.
    for (fc in intersect(c("Dataset", "Table", "Source"), names(disp))) {
      disp[[fc]] <- as.factor(disp[[fc]])
    }
    # Keep the table full-width but bias the available space toward the
    # Message column: the short metadata columns get small explicit widths so
    # the (usually long) Message text gets the lion's share of the row.
    col_w <- c(ID = "46px", Dataset = "110px", Table = "120px", Source = "82px",
               Row = "54px", Column = "96px", Rule = "130px", Message = "50%")
    nm <- names(disp)
    coldefs <- lapply(seq_along(nm), function(i) {
      cls <- if (identical(nm[i], "Message")) "msg-cell" else "dt-nowrap"
      d <- list(targets = i - 1L, className = cls)
      if (nm[i] %in% names(col_w)) d$width <- col_w[[nm[i]]]
      d
    })
    DT::datatable(
      disp,
      rownames = FALSE, selection = "single", filter = "top",
      class = "display compact", width = "100%",
      options = list(pageLength = 8, dom = "tp", scrollX = TRUE,
                     autoWidth = TRUE, columnDefs = coldefs)
    )
  })

  # Shared display shape (column selection + pretty names) for the messages
  # table AND the CSV/TSV/XLSX downloads, so the two never diverge.
  messages_display <- function(m) {
    disp <- as.data.frame(m)
    if (is.null(disp) || nrow(disp) == 0) return(disp)
    # Surface WHERE each message comes from: dataset + table (source table).
    if ("target" %in% names(disp)) names(disp)[names(disp) == "target"] <- "table"
    cols <- intersect(c("id", "dataset", "table", "source", "row", "column", "rule_id", "message"),
                      names(disp))
    disp <- disp[, cols, drop = FALSE]
    pretty <- c(id = "ID", dataset = "Dataset", table = "Table", source = "Source",
                row = "Row", column = "Column", rule_id = "Rule", message = "Message")
    hit <- names(disp) %in% names(pretty)
    names(disp)[hit] <- unname(pretty[names(disp)[hit]])
    disp
  }

  # Downloadable messages (CSV / TSV / XLSX). Same display shape as the table;
  # falls back to a one-row note when the dataset has no messages.
  msgs_dl_df <- reactive({
    disp <- messages_display(msgs_r())
    if (is.null(disp) || nrow(disp) == 0) {
      disp <- data.frame(Message = "No validation messages for this dataset.")
    }
    disp
  })
  msgs_dl_base <- function() {
    nm <- rv$active %||% "dataset"
    paste0(gsub("[^A-Za-z0-9._-]+", "_", nm), "_validation_messages")
  }
  output$dl_msgs_csv <- downloadHandler(
    filename = function() paste0(msgs_dl_base(), ".csv"),
    content = function(file) utils::write.csv(msgs_dl_df(), file, row.names = FALSE, na = "")
  )
  output$dl_msgs_tsv <- downloadHandler(
    filename = function() paste0(msgs_dl_base(), ".tsv"),
    content = function(file) utils::write.table(
      msgs_dl_df(), file, sep = "\t", row.names = FALSE, na = "", qmethod = "double"
    )
  )
  output$dl_msgs_xlsx <- downloadHandler(
    filename = function() paste0(msgs_dl_base(), ".xlsx"),
    content = function(file) {
      if (!requireNamespace("writexl", quietly = TRUE)) {
        showNotification(
          "XLSX export needs the 'writexl' package. Install it, or use CSV / TSV.",
          type = "error", duration = 10
        )
        stop("writexl not available")
      }
      writexl::write_xlsx(msgs_dl_df(), file)
    }
  )

  df_to_kv <- function(d) {
    # single-row -> transposed Field/Value (drop NA/empty)
    row <- d[1, , drop = FALSE]
    keep <- vapply(names(row), function(k) {
      v <- row[[k]]
      !(is.null(v) || all(is.na(v)) || (is.character(v) && all(!nzchar(v))))
    }, logical(1))
    fields <- names(row)[keep]
    tags$table(
      class = "table table-sm",
      tags$tbody(
        lapply(fields, function(k) {
          tags$tr(
            tags$th(style = "white-space:nowrap; color:var(--bi-grey);", k),
            tags$td(as.character(row[[k]]))
          )
        })
      )
    )
  }

  df_to_html_table <- function(d) {
    # drop all-NA columns
    keep <- vapply(d, function(v) !all(is.na(v)), logical(1))
    d <- d[, keep, drop = FALSE]
    tags$div(
      style = "overflow-x:auto;",
      tags$table(
        class = "table table-sm table-striped",
        tags$thead(tags$tr(lapply(names(d), tags$th))),
        tags$tbody(
          lapply(seq_len(nrow(d)), function(i) {
            tags$tr(lapply(names(d), function(k) tags$td(as.character(d[[k]][i]))))
          })
        )
      )
    )
  }

  # --- inspect helpers: a plain-language description of the failing rule/
  # constraint (what it was) plus a highlighted "should be" vs "actual" split.
  .first_nonempty <- function(...) {
    for (v in list(...)) {
      if (!is.null(v) && length(v) >= 1 && !is.na(v[[1]]) && nzchar(as.character(v[[1]]))) {
        return(as.character(v[[1]]))
      }
    }
    ""
  }

  # Human "should be" text for a schema violation, derived from its keyword.
  schema_expected_text <- function(r) {
    kw <- as.character(r[["schema_keyword"]] %||% "")
    switch(kw,
      enum = paste0("one of: ", .first_nonempty(r[["schema_params.allowedValues"]],
                                                r[["schema_parentSchema.enum"]],
                                                r[["schema_schema"]])),
      const = paste0("exactly: ", .first_nonempty(r[["schema_parentSchema.const"]],
                                                  r[["schema_schema"]])),
      maxLength = paste0("at most ", .first_nonempty(r[["schema_params.limit"]],
                                                     r[["schema_parentSchema.maxLength"]]),
                         " character(s)"),
      minLength = paste0("at least ", .first_nonempty(r[["schema_params.limit"]],
                                                      r[["schema_parentSchema.minLength"]]),
                         " character(s)"),
      maximum = paste0("at most ", .first_nonempty(r[["schema_params.limit"]])),
      minimum = paste0("at least ", .first_nonempty(r[["schema_params.limit"]])),
      type = paste0("type: ", .first_nonempty(r[["schema_parentSchema.type"]])),
      pattern = paste0("match pattern ", .first_nonempty(r[["schema_params.pattern"]],
                                                         r[["schema_schema"]])),
      required = "the value must be present (not missing)",
      .first_nonempty(r[["schema_message"]], r[["message"]], "(see message)"))
  }

  # Highlighted table of the failing rows captured for a rule violation.
  inspect_failing_rows_ui <- function(d) {
    fcols <- grep("^failing_", names(d), value = TRUE)
    fcols <- setdiff(fcols, "failing_row_count")
    if (length(fcols) == 0) {
      return(tags$em(class = "inspect-none", "No offending rows were captured."))
    }
    sub <- d[, fcols, drop = FALSE]
    names(sub) <- sub("^failing_", "", names(sub))
    names(sub)[names(sub) == ".row"] <- "Row"
    if ("Row" %in% names(sub)) {
      sub <- sub[, c("Row", setdiff(names(sub), "Row")), drop = FALSE]
    }
    tags$table(
      class = "inspect-hl-table",
      tags$thead(tags$tr(lapply(names(sub), function(k) tags$th(k)))),
      tags$tbody(
        lapply(seq_len(nrow(sub)), function(i) {
          tags$tr(lapply(names(sub), function(k) {
            is_row <- identical(k, "Row")
            tags$td(class = if (is_row) "inspect-hl-row" else "inspect-hl-val",
                    as.character(sub[[k]][i]))
          }))
        })
      )
    )
  }

  # Build the full inspect modal body: a summary card describing the failing
  # rule/constraint (Task: "what the rule was"), a highlighted should-be vs
  # actual split, and the raw technical detail in a collapsible section.
  render_inspect_body <- function(d, dataset) {
    r <- as.list(d[1, , drop = FALSE])
    typ <- r$type %||% (if ("rule_id" %in% names(d)) "rule" else "schema")
    msg <- .first_nonempty(r$message, r$headline)

    if (identical(typ, "rule")) {
      rid <- as.character(r$rule_id %||% "")
      ov <- tryCatch(dta_rules_overview(rv$dta, dataset), error = function(e) NULL)
      rrow <- if (!is.null(ov) && nrow(ov) > 0 && nzchar(rid)) {
        ov[ov$id == rid, , drop = FALSE]
      } else NULL
      have <- !is.null(rrow) && nrow(rrow) > 0
      rtype <- if (have) rrow$type[1] else ""
      rdetail <- if (have) rrow$detail[1] else ""
      rdesc <- if (have) rrow$description[1] else ""
      badge <- tags$span(class = "inspect-badge rule", "Rule failure")
      desc <- div(
        class = "inspect-desc",
        div(class = "inspect-desc-main",
            tags$strong(if (nzchar(rid)) rid else "(rule)"),
            if (nzchar(rtype)) tags$span(class = "inspect-desc-type",
                                         dta_rule_type_label(rtype))),
        if (nzchar(rdetail)) div(class = "inspect-desc-detail", rdetail),
        if (nzchar(rdesc)) div(class = "inspect-desc-note", rdesc)
      )
      expected_ui <- div(class = "inspect-should",
                         if (nzchar(rdetail)) rdetail else msg)
      actual_ui <- inspect_failing_rows_ui(d)
      actual_title <- "Offending row(s) \u2014 actual values"
    } else {
      col <- .first_nonempty(r[["schema_column"]], r[["column"]])
      kw  <- .first_nonempty(r[["schema_keyword"]], r[["keyword"]])
      smsg <- .first_nonempty(r[["schema_message"]], msg)
      badge <- tags$span(class = "inspect-badge schema", "Schema violation")
      desc <- div(
        class = "inspect-desc",
        div(class = "inspect-desc-main",
            tags$strong(if (nzchar(col)) col else "(column)"),
            if (nzchar(kw)) tags$span(class = "inspect-desc-type", kw)),
        if (nzchar(smsg)) div(class = "inspect-desc-detail", smsg)
      )
      expected_ui <- div(class = "inspect-should", schema_expected_text(r))
      aval <- .first_nonempty(r[["schema_data"]],
                              if (nzchar(col)) r[[paste0("context_", col)]] else NULL)
      arow <- .first_nonempty(r[["schema_row"]], r[["context_.row"]])
      loc <- paste0(
        if (nzchar(col)) paste0("column ", col) else "",
        if (nzchar(arow)) paste0(if (nzchar(col)) ", " else "", "row ", arow) else ""
      )
      actual_ui <- tagList(
        div(class = "inspect-actual-val", if (nzchar(aval)) aval else "(empty)"),
        if (nzchar(loc)) div(class = "inspect-actual-loc", loc)
      )
      actual_title <- "Actual value"
    }

    full <- if (nrow(d) == 1) df_to_kv(d) else df_to_html_table(d)

    tagList(
      div(
        class = "inspect-summary",
        div(class = "inspect-summary-head", badge),
        div(class = "inspect-msg", msg),
        desc
      ),
      div(
        class = "inspect-cmp",
        div(
          class = "inspect-box inspect-expected",
          div(class = "inspect-box-title", HTML("&#x2714; Should be")),
          div(class = "inspect-box-body", expected_ui)
        ),
        div(
          class = "inspect-box inspect-actual",
          div(class = "inspect-box-title", HTML(paste0("&#x2716; ", actual_title))),
          div(class = "inspect-box-body", actual_ui)
        )
      ),
      tags$details(
        class = "inspect-details",
        tags$summary("Full technical detail"),
        div(class = "dta-inspect-wrap", full)
      )
    )
  }

  show_inspect_modal <- function(dataset, id) {
    res <- dta_inspect(rv$dta, dataset, id)
    if (!res$ok || is.null(res$value) || nrow(res$value) == 0) {
      showModal(modalDialog(
        title = paste("Message", id),
        tags$p("No detailed inspect report is available for this message."),
        if (!is.null(res$error)) tags$pre(res$error),
        easyClose = TRUE, footer = modalButton("Close")
      ))
      return()
    }
    showModal(modalDialog(
      title = paste("Inspect \u2014 message", id),
      size = "xl", easyClose = TRUE, footer = modalButton("Close"),
      div(class = "inspect-modal-body", render_inspect_body(res$value, dataset))
    ))
  }

  observeEvent(input$msgs_rows_selected, {
    sel <- input$msgs_rows_selected
    m <- msgs_r()
    if (is.null(sel) || is.null(m) || nrow(m) == 0) return()
    id <- if ("id" %in% names(m)) m$id[sel] else sel
    show_inspect_modal(rv$active, id)
  })

  # --- metadata editing ---------------------------------------------------
  output$metadata_editor <- renderUI({
    rv$md_token
    dta <- isolate(rv$dta)
    req(dta)
    md <- DTAtools::metadata(dta)
    getf <- function(field) {
      v <- tryCatch(S7::prop(md, field), error = function(e) NULL)
      if (is.null(v) || length(v) == 0) "" else as.character(v)[1]
    }
    date_val <- tryCatch(S7::prop(md, "date"), error = function(e) NULL)
    tr <- dta_transmission(dta)
    trf <- function(k) {
      v <- tr[[k]]
      if (is.null(v)) "" else if (inherits(v, "Date")) format(v, "%Y-%m-%d") else as.character(v)[1]
    }
    tagList(
      div(class = "md-section-title", "Document"),
      layout_columns(
        col_widths = c(6, 6),
        textInput("md_title", "Title", value = getf("title"), width = "100%"),
        textInput("md_version", "Version", value = getf("version"), width = "100%")
      ),
      layout_columns(
        col_widths = c(6, 6),
        dateInput("md_date", "Date",
                  value = if (inherits(date_val, "Date")) date_val else NULL,
                  width = "100%"),
        textInput("md_header", "Header / organization", value = getf("header"), width = "100%")
      ),
      tags$hr(),
      div(class = "md-section-title", "Parties"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Receiver"),
          card_body(
            div(class = "section-label", "Affiliation"),
            uiOutput("receiver_affiliation"),
            tags$hr(),
            div(class = "section-label",
                style = "display:flex; justify-content:space-between; align-items:center; gap:8px;",
                tags$span("Contacts"),
                actionButton("add_receiver", "Add person", class = "btn btn-sm btn-outline-primary")),
            uiOutput("receiver_contacts")
          )
        ),
        card(
          card_header("Supplier"),
          card_body(
            div(class = "section-label", "Affiliation"),
            uiOutput("supplier_affiliation"),
            tags$hr(),
            div(class = "section-label",
                style = "display:flex; justify-content:space-between; align-items:center; gap:8px;",
                tags$span("Contacts"),
                actionButton("add_supplier", "Add person", class = "btn btn-sm btn-outline-primary")),
            uiOutput("supplier_contacts")
          )
        )
      ),
      tags$hr(),
      div(class = "md-section-title", "Transmission"),
      layout_columns(
        col_widths = c(4, 4, 4),
        textInput("tr_type", "Type", value = trf("type"), width = "100%",
                  placeholder = "e.g. secure S3 bucket"),
        textInput("tr_frequency", "Frequency", value = trf("frequency"), width = "100%",
                  placeholder = "e.g. one-time, weekly"),
        textInput("tr_notification", "Notification", value = trf("notification"), width = "100%",
                  placeholder = "e.g. email")
      ),
      layout_columns(
        col_widths = c(6, 6),
        textInput("tr_date_first", "Date of first transfer", value = trf("date_first_transfer"),
                  width = "100%", placeholder = "YYYY-MM-DD or phrase"),
        textInput("tr_date_last", "Date of last transfer", value = trf("date_last_transfer"),
                  width = "100%", placeholder = "YYYY-MM-DD or phrase")
      ),
      div(
        style = "display:flex; gap:24px; margin-top:2px;",
        checkboxInput("tr_test_upload", "Test upload", value = isTRUE(tr$test_upload)),
        checkboxInput("tr_blinded", "Blinded transfer", value = isTRUE(tr$blinded_transfer))
      ),
      tags$hr(),
      div(class = "md-section-title", "Error handling & corrections"),
      textAreaInput("md_error_handling", "Error handling",
                    value = getf("error_handling"), width = "100%", rows = 2,
                    placeholder = "How data/format errors are handled and communicated."),
      textInput("md_authorized", "Authorized for corrections",
                value = getf("authorized_for_corrections"), width = "100%",
                placeholder = "Contact(s) authorized to request corrections"),
      div(class = "msg-hint",
          "Changes are saved automatically to the current session as you type.")
    )
  })

  # per-field debounced saves (incremental, non-destructive)
  save_md <- function(field, value) {
    req(rv$dta)
    r <- dta_set_metadata_field(rv$dta, field, value)
    if (r$ok) {
      rv$dta <- r$value
      # Keep the raw YAML text (and autosave) in sync so the edit is not
      # reverted when the user later applies the Raw YAML document.
      sync_yaml_text()
    } else {
      showNotification(paste("Could not update", field, "\u2014", r$error),
                       type = "error")
    }
  }
  save_tr <- function(field, value) {
    req(rv$dta)
    r <- dta_set_transmission_field(rv$dta, field, value)
    if (r$ok) {
      rv$dta <- r$value
      sync_yaml_text()
    } else {
      showNotification(paste("Could not update transmission", field, "\u2014", r$error),
                       type = "error")
    }
  }
  title_d   <- debounce(reactive(input$md_title), 700)
  version_d <- debounce(reactive(input$md_version), 700)
  header_d  <- debounce(reactive(input$md_header), 700)
  errh_d    <- debounce(reactive(input$md_error_handling), 700)
  auth_d    <- debounce(reactive(input$md_authorized), 700)
  observeEvent(title_d(),   save_md("title", title_d()),     ignoreInit = TRUE)
  observeEvent(version_d(), save_md("version", version_d()), ignoreInit = TRUE)
  observeEvent(header_d(),  save_md("header", header_d()),   ignoreInit = TRUE)
  observeEvent(errh_d(),    save_md("error_handling", errh_d()), ignoreInit = TRUE)
  observeEvent(auth_d(),    save_md("authorized_for_corrections", auth_d()), ignoreInit = TRUE)
  observeEvent(input$md_date, {
    req(input$md_date)
    save_md("date", input$md_date)
  }, ignoreInit = TRUE)
  # transmission fields (debounced text + immediate flags)
  tr_type_d   <- debounce(reactive(input$tr_type), 700)
  tr_freq_d   <- debounce(reactive(input$tr_frequency), 700)
  tr_notif_d  <- debounce(reactive(input$tr_notification), 700)
  tr_first_d  <- debounce(reactive(input$tr_date_first), 700)
  tr_last_d   <- debounce(reactive(input$tr_date_last), 700)
  observeEvent(tr_type_d(),  save_tr("type", tr_type_d()),               ignoreInit = TRUE)
  observeEvent(tr_freq_d(),  save_tr("frequency", tr_freq_d()),          ignoreInit = TRUE)
  observeEvent(tr_notif_d(), save_tr("notification", tr_notif_d()),      ignoreInit = TRUE)
  observeEvent(tr_first_d(), save_tr("date_first_transfer", tr_first_d()), ignoreInit = TRUE)
  observeEvent(tr_last_d(),  save_tr("date_last_transfer", tr_last_d()),  ignoreInit = TRUE)
  observeEvent(input$tr_test_upload, save_tr("test_upload", isTRUE(input$tr_test_upload)), ignoreInit = TRUE)
  observeEvent(input$tr_blinded, save_tr("blinded_transfer", isTRUE(input$tr_blinded)), ignoreInit = TRUE)

  # --- people / contacts --------------------------------------------------
  # Shared field set so the Add and Edit person modals capture the SAME details
  # (name, roles, email, department, phone, address). `prefix` namespaces the
  # input ids ("new_contact" for add, "edit_contact" for edit); `p` pre-fills.
  contact_modal_inputs <- function(prefix, p = list()) {
    g <- function(k) p[[k]] %||% ""
    tagList(
      textInput(paste0(prefix, "_name"), "Name", value = g("name")),
      textInput(paste0(prefix, "_roles"), "Role(s)", value = g("role"),
                placeholder = "e.g. Data Manager, Reviewer"),
      textInput(paste0(prefix, "_email"), "Email", value = g("email")),
      layout_columns(
        col_widths = c(6, 6),
        textInput(paste0(prefix, "_department"), "Department", value = g("department")),
        textInput(paste0(prefix, "_phone"), "Phone", value = g("phone"))
      ),
      textInput(paste0(prefix, "_address"), "Address", value = g("address"))
    )
  }

  render_contacts <- function(side) {
    cs <- dta_contacts(isolate(rv$dta), side)
    if (length(cs) == 0) {
      return(div(class = "msg-hint", "No contacts yet. Click \u201cAdd person\u201d to create one."))
    }
    tags$ul(
      class = "list-group",
      lapply(seq_along(cs), function(i) {
        tags$li(
          class = "list-group-item contact-item d-flex justify-content-between align-items-center",
          actionLink(
            paste0("editc_", side, "_", i),
            class = "flex-grow-1 text-decoration-none",
            title = "Click to edit this person",
            label = tagList(
              span(contact_display(cs[[i]])),
              span(class = "contact-edit-ic", "\u270E edit")
            )
          ),
          actionButton(
            paste0("rm_", side, "_", i), "Remove",
            class = "btn btn-sm btn-outline-danger"
          )
        )
      })
    )
  }

  # Open a pre-filled modal to edit one contact's details.
  edit_contact_flow <- function(side, index) {
    p <- dta_contact_at(isolate(rv$dta), side, index)
    if (is.null(p)) return()
    rv$editing_contact <- list(side = side, index = index)
    showModal(modalDialog(
      title = paste("Edit", side, "contact"),
      contact_modal_inputs("edit_contact", p),
      div(class = "msg-hint",
          "Separate multiple roles with commas. Other fields on this person (e.g. signature flags) are preserved."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_edit_contact", "Save", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
  }

  observeEvent(input$confirm_edit_contact, {
    ec <- rv$editing_contact
    req(ec)
    nm <- trimws(input$edit_contact_name %||% "")
    if (!nzchar(nm)) {
      showNotification("A name is required.", type = "warning")
      return()
    }
    roles <- paste(trimws(strsplit(input$edit_contact_roles %||% "", ",")[[1]]), collapse = ", ")
    fields <- list(
      name = nm,
      role = roles,
      email = input$edit_contact_email %||% "",
      department = input$edit_contact_department %||% "",
      phone = input$edit_contact_phone %||% "",
      address = input$edit_contact_address %||% ""
    )
    r <- dta_update_contact(rv$dta, ec$side, ec$index, fields)
    if (r$ok) {
      rv$dta <- r$value
      rv$editing_contact <- NULL
      rv$contacts_token <- rv$contacts_token + 1
      sync_yaml_text()
      removeModal()
    } else {
      showNotification(r$error, type = "error")
    }
  })

  output$receiver_contacts <- renderUI({ rv$contacts_token; req(isolate(rv$dta)); render_contacts("receiver") })
  output$supplier_contacts <- renderUI({ rv$contacts_token; req(isolate(rv$dta)); render_contacts("supplier") })

  # --- affiliation (side-level: receiver / supplier) ----------------------
  render_affiliation <- function(side) {
    aff <- dta_affiliation(isolate(rv$dta), side)
    g <- function(k) aff[[k]] %||% ""
    tagList(
      textInput(paste0(side, "_aff_name"), "Organization",
                value = g("name"), width = "100%",
                placeholder = "e.g. Test Company"),
      layout_columns(
        col_widths = c(6, 6),
        textInput(paste0(side, "_aff_country"), "Country",
                  value = g("country"), width = "100%"),
        textInput(paste0(side, "_aff_address"), "Address",
                  value = g("address"), width = "100%")
      )
    )
  }
  output$receiver_affiliation <- renderUI({ rv$md_token; req(isolate(rv$dta)); render_affiliation("receiver") })
  output$supplier_affiliation <- renderUI({ rv$md_token; req(isolate(rv$dta)); render_affiliation("supplier") })

  save_affiliation <- function(side, field, value) {
    req(rv$dta)
    kv <- list(); kv[[field]] <- value %||% ""
    r <- do.call(dta_set_affiliation, c(list(rv$dta, side), kv))
    if (r$ok) {
      rv$dta <- r$value
      sync_yaml_text()
    } else {
      showNotification(paste("Could not update affiliation \u2014", r$error), type = "error")
    }
  }
  for (.side in c("receiver", "supplier")) {
    for (.field in c("name", "country", "address")) {
      local({
        SIDE <- .side; FIELD <- .field
        inid <- paste0(SIDE, "_aff_", FIELD)
        d <- debounce(reactive(input[[inid]]), 700)
        observeEvent(d(), save_affiliation(SIDE, FIELD, d()), ignoreInit = TRUE)
      })
    }
  }

  # dynamic remove observers for contacts
  contact_rm_registry <- new.env(parent = emptyenv())
  observe({
    rv$contacts_token
    req(isolate(rv$dta))
    for (side in c("receiver", "supplier")) {
      cs <- dta_contacts(isolate(rv$dta), side)
      for (i in seq_along(cs)) {
        edid <- paste0("editc_", side, "_", i)
        if (is.null(contact_rm_registry[[edid]])) {
          contact_rm_registry[[edid]] <- TRUE
          local({
            SIDE <- side; IDX <- i; EID <- edid
            observeEvent(input[[EID]], edit_contact_flow(SIDE, IDX), ignoreInit = TRUE)
          })
        }
        rmid <- paste0("rm_", side, "_", i)
        if (is.null(contact_rm_registry[[rmid]])) {
          contact_rm_registry[[rmid]] <- TRUE
          local({
            SIDE <- side; IDX <- i; ID <- rmid
            observeEvent(input[[ID]], {
              showModal(modalDialog(
                title = "Remove contact?",
                sprintf("Remove this %s contact?", SIDE),
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton(paste0("confirm_", ID), "Remove", class = "btn btn-danger")
                ),
                easyClose = TRUE
              ))
            }, ignoreInit = TRUE)
            observeEvent(input[[paste0("confirm_", ID)]], {
              r <- dta_remove_contact(rv$dta, SIDE, IDX)
              if (r$ok) {
                rv$dta <- r$value
                rv$contacts_token <- rv$contacts_token + 1
                sync_yaml_text()
              } else {
                showNotification(r$error, type = "error")
              }
              removeModal()
            }, ignoreInit = TRUE)
          })
        }
      }
    }
  })

  add_contact_flow <- function(side) {
    showModal(modalDialog(
      title = paste("Add", side, "contact"),
      contact_modal_inputs("new_contact"),
      div(class = "msg-hint",
          "Separate multiple roles with commas. Affiliation is set once per side (above), not per person."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(paste0("confirm_add_", side), "Add", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
  }
  observeEvent(input$add_receiver, add_contact_flow("receiver"))
  observeEvent(input$add_supplier, add_contact_flow("supplier"))

  confirm_add <- function(side) {
    nm <- trimws(input$new_contact_name %||% "")
    if (!nzchar(nm)) {
      showNotification("A name is required.", type = "warning")
      return()
    }
    roles <- trimws(strsplit(input$new_contact_roles %||% "", ",")[[1]])
    r <- dta_add_contact(
      rv$dta, side, name = nm, roles = roles,
      email = input$new_contact_email %||% "",
      department = input$new_contact_department %||% "",
      phone = input$new_contact_phone %||% "",
      address = input$new_contact_address %||% ""
    )
    if (r$ok) {
      rv$dta <- r$value
      rv$contacts_token <- rv$contacts_token + 1
      sync_yaml_text()
      removeModal()
    } else {
      showNotification(r$error, type = "error")
    }
  }
  observeEvent(input$confirm_add_receiver, confirm_add("receiver"))
  observeEvent(input$confirm_add_supplier, confirm_add("supplier"))

  # --- raw YAML: editable, validated as YAML AND as a DTA/DTADataSet ------
  output$yaml_validation_msg <- renderUI({
    m <- rv$yaml_msg
    if (is.null(m)) return(NULL)
    if (isTRUE(m$ok)) {
      div(class = "yaml-valid ok", HTML("&#x2714;"),
          " Valid — document applied.")
    } else {
      div(class = "yaml-valid err", HTML("&#x2716;"), " ", m$error)
    }
  })

  # Push text into the Raw YAML editor (Ace when available, else textarea).
  set_yaml_editor <- function(text) {
    if (requireNamespace("shinyAce", quietly = TRUE)) {
      shinyAce::updateAceEditor(session, "raw_yaml_editor", value = text %||% "")
    } else {
      updateTextAreaInput(session, "raw_yaml_editor", value = text %||% "")
    }
  }

  observeEvent(input$revert_yaml, {
    set_yaml_editor(rv$yaml_text %||% "")
    rv$yaml_msg <- NULL
  })

  observeEvent(input$apply_yaml, {
    txt <- input$raw_yaml_editor %||% ""
    if (!nzchar(trimws(txt))) {
      rv$yaml_msg <- list(ok = FALSE, error = "The editor is empty — nothing to apply.")
      return()
    }
    res <- dta_read_yaml_text(txt)
    if (!isTRUE(res$ok)) {
      rv$yaml_msg <- list(ok = FALSE, error = res$error %||% "Invalid YAML or DTA.")
      showNotification(paste("YAML not applied:", res$error), type = "error", duration = 10)
      return()
    }
    # Smart reconcile: keep uploads for datasets whose file handlers are
    # unchanged, keep validation only when columns/rules are also unchanged,
    # and drop uploads for datasets that were removed from the document.
    new_dta     <- res$value
    old_dta     <- isolate(rv$dta)
    old_uploads <- isolate(rv$uploads)
    old_status  <- isolate(rv$status)
    prev_active <- isolate(rv$active)
    new_names   <- dta_dataset_names(new_dta)
    old_names   <- if (is.null(old_dta)) character(0) else dta_dataset_names(old_dta)

    new_uploads <- list()
    new_status  <- stats::setNames(rep("pending", length(new_names)), new_names)
    for (nm in new_names) {
      if (!(nm %in% old_names)) next # brand-new dataset -> fresh, pending
      old_ds <- dta_get_dataset(old_dta, nm)
      new_ds <- dta_get_dataset(new_dta, nm)
      handlers_same <- identical(dta_handlers_signature(old_ds),
                                 dta_handlers_signature(new_ds))
      specs_same <- identical(dta_specs_signature(old_ds),
                              dta_specs_signature(new_ds))
      if (handlers_same && dta_dataset_content_count(old_ds) > 0) {
        tr <- dta_transfer_bound_data(new_dta, nm, old_ds, keep_validation = specs_same)
        if (isTRUE(tr$ok)) {
          new_dta <- tr$value
          for (k in names(old_uploads)) {
            if (startsWith(k, paste0(nm, "||"))) new_uploads[[k]] <- old_uploads[[k]]
          }
          new_status[[nm]] <- if (specs_same) (old_status[[nm]] %||% "pending") else "pending"
        }
      }
      # handlers changed OR no bound data -> fresh (uploads dropped, pending)
    }

    rv$dta <- new_dta
    rv$yaml_text <- txt
    rv$structure <- build_structure(new_dta)
    rv$uploads <- new_uploads
    for (nm in new_names) {
      if (dta_dataset_content_count(dta_get_dataset(new_dta, nm)) == 0 &&
          identical(new_status[[nm]], "pending")) {
        new_status[[nm]] <- "nodata"
      }
    }
    rv$status <- new_status
    rv$dataset_only <- isTRUE(res$dataset_only)
    rv$active <- if (!is.null(prev_active) && prev_active %in% new_names) {
      prev_active
    } else if (length(new_names) > 0) {
      new_names[1]
    } else {
      NULL
    }
    rv$md_token <- rv$md_token + 1
    rv$contacts_token <- rv$contacts_token + 1
    autosave()
    rv$yaml_msg <- list(ok = TRUE, error = NULL)
    showNotification(
      if (isTRUE(res$dataset_only)) {
        "Dataset YAML applied (uploads kept where handlers are unchanged)."
      } else {
        "DTA YAML applied (uploads kept where handlers are unchanged)."
      },
      type = "message"
    )
  })

  # --- export -------------------------------------------------------------
  # helper: write a UTF-8 YAML string to a download file (binary-safe)
  write_yaml_download <- function(text, file) {
    con <- file(file, "wb")
    on.exit(close(con))
    writeLines(enc2utf8(text), con, useBytes = TRUE)
  }
  yaml_export_stub <- function() {
    md <- tryCatch(DTAtools::metadata(rv$dta), error = function(e) NULL)
    ttl <- tryCatch(as.character(S7::prop(md, "title"))[1], error = function(e) NULL)
    base <- if (!is.null(ttl) && nzchar(ttl)) gsub("[^A-Za-z0-9]+", "_", ttl) else "DTA"
    base
  }
  output$dl_yaml <- downloadHandler(
    filename = function() paste0(yaml_export_stub(), "_", Sys.Date(), ".yaml"),
    content = function(file) {
      req(rv$dta)
      res <- dta_to_yaml_text(rv$dta)
      if (!res$ok) {
        showNotification(paste("YAML export failed:", res$error), type = "error", duration = 10)
        stop(res$error)
      }
      write_yaml_download(res$value, file)
    }
  )
  output$dl_ds_yaml <- downloadHandler(
    filename = function() {
      nm <- rv$active %||% "dataset"
      paste0(gsub("[^A-Za-z0-9]+", "_", nm), "_", Sys.Date(), ".yaml")
    },
    content = function(file) {
      req(rv$dta, rv$active)
      res <- dta_dataset_to_yaml_text(rv$dta, rv$active)
      if (!res$ok) {
        showNotification(paste("Dataset YAML export failed:", res$error),
                         type = "error", duration = 10)
        stop(res$error)
      }
      write_yaml_download(res$value, file)
    }
  )
  output$dl_docx <- downloadHandler(
    filename = function() paste0("DTA_", Sys.Date(), ".docx"),
    content = function(file) {
      req(rv$dta)
      res <- dta_export(rv$dta, file, "docx")
      if (!res$ok) {
        showNotification(paste("Word export failed:", res$error), type = "error", duration = 10)
        stop(res$error)
      }
    }
  )
  output$dl_pdf <- downloadHandler(
    filename = function() paste0("DTA_", Sys.Date(), ".pdf"),
    content = function(file) {
      req(rv$dta)
      res <- dta_export(rv$dta, file, "pdf")
      if (!res$ok) {
        showNotification(
          paste("PDF export failed (a DOCX converter such as LibreOffice may be required):",
                res$error),
          type = "error", duration = 12
        )
        stop(res$error)
      }
    }
  )

  # Validation report: offered ONLY once a validation has actually succeeded
  # (at least one dataset passed and none failed). Hidden otherwise.
  output$validation_report_ui <- renderUI({
    st <- unlist(rv$status)
    validated <- names(st)[st %in% c("pass", "fail")]
    ok <- length(validated) > 0 && all(st[validated] == "pass")
    if (!isTRUE(ok)) return(NULL)
    tagList(
      tags$hr(),
      downloadButton("dl_validation_report", "Validation report",
                     class = "btn btn-success w-100",
                     title = "Download a report certifying this successful validation")
    )
  })

  output$dl_validation_report <- downloadHandler(
    filename = function() {
      paste0("validation_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      req(rv$dta)
      html <- dta_build_validation_report(rv$dta, rv$status)
      writeLines(html, file, useBytes = TRUE)
    }
  )

  # --- reset --------------------------------------------------------------
  observeEvent(input$reset_app, {
    showModal(modalDialog(
      title = "Start over?",
      "This clears the loaded DTA and all uploads from this session.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Start over", class = "btn btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  observeEvent(input$confirm_reset, {
    rv$dta <- NULL; rv$yaml_text <- NULL; rv$structure <- NULL
    rv$active <- NULL; rv$uploads <- list(); rv$status <- list()
    rv$is_example <- FALSE
    rv$example_target <- NULL
    try(unlink(session_file), silent = TRUE)
    removeModal()
  })

  # --- status / summary outputs ------------------------------------------
  output$dataset_status_line <- renderUI({
    req(rv$active)
    st <- rv$status[[rv$active]] %||% "pending"
    s <- rv$structure[[rv$active]]
    type_txt <- if (!is.null(s) && !is.na(s$type)) s$type else ""
    ds <- dta_get_dataset(rv$dta, rv$active)
    desc <- tryCatch(ds@description, error = function(e) NULL) %||% ""
    heading <- if (nzchar(desc)) desc else rv$active
    div(
      div(
        class = "ds-head",
        tags$h4(class = "ds-desc", heading)
      ),
      div(
        style = "display:flex; align-items:center; gap:8px; margin-bottom:6px; flex-wrap:wrap;",
        # Every dataset carries a tag with its name (alongside the type tag),
        # so the dataset is always clearly identified on the Datasets page.
        tags$span(class = "dta-name-chip", rv$active),
        if (nzchar(type_txt)) tags$span(class = "status-chip status-pending", type_txt),
        status_chip(st)
      )
    )
  })

  output$loaded_files <- renderUI({
    req(rv$active, rv$structure)
    rv$uploads; rv$status  # re-render when files or validation state change
    dsname <- rv$active
    s <- rv$structure[[dsname]]
    tstatus <- dta_table_status_map(rv$dta, dsname)
    any_files <- FALSE
    blocks <- lapply(seq_along(s$handlers), function(hi) {
      h <- s$handlers[[hi]]
      key <- paste0(dsname, "||", hi)
      recs <- rv$uploads[[key]] %||% list()
      if (length(recs) == 0) return(NULL)
      any_files <<- TRUE
      rows <- lapply(recs, function(rec) {
        fid <- get_file_id(dsname, hi, rec$table)
        st <- tstatus[[rec$table]] %||% "pending"
        icon_ch <- switch(st, pass = "\u2714", fail = "\u2716", "\u2014")
        icls <- switch(st, pass = "file-ok", fail = "file-fail", "file-pending")
        ttl <- switch(st,
          pass = "Validated \u2014 no errors",
          fail = "Validation errors \u2014 see messages below",
          "Not validated yet")
        div(
          class = "loaded-file-row",
          tags$span(class = paste("file-status", icls), title = ttl, icon_ch),
          tags$span(class = "file-name", rec$file),
          tags$span(class = "file-table", title = "Table name",
                    paste0("\u2192 ", rec$table)),
          actionButton(paste0("rmfile_", fid), label = HTML("&#x1F5D1;&#xFE0F;"),
                       class = "btn btn-sm file-remove", title = "Remove this file")
        )
      })
      div(
        class = "loaded-slot",
        div(class = "loaded-slot-head", tags$span(class = "slot-expected", h$expected)),
        tagList(rows)
      )
    })
    if (!any_files) return(div(class = "msg-hint", "No files loaded yet."))
    tagList(blocks)
  })

  output$workspace_header <- renderUI({
    req(rv$dta)
    md <- tryCatch(DTAtools::metadata(rv$dta), error = function(e) NULL)
    getf <- function(field) {
      v <- tryCatch(S7::prop(md, field), error = function(e) NULL)
      if (is.null(v) || length(v) == 0) "" else as.character(v)[1]
    }
    dataset_only <- isTRUE(rv$dataset_only)
    title <- getf("title")
    if (!nzchar(title)) {
      title <- if (dataset_only) {
        dta_dataset_names(rv$dta)[1] %||% "Dataset"
      } else "Untitled DTA"
    }
    version <- getf("version")
    date <- getf("date")
    div(
      class = "workspace-header",
      div(class = "ws-title", title),
      if (dataset_only || nzchar(version) || nzchar(date)) div(
        class = "ws-meta",
        if (dataset_only) tags$span(class = "ws-pill", "Dataset \u2014 no metadata"),
        if (nzchar(version)) tags$span(class = "ws-pill", paste0("v", version)),
        if (nzchar(date)) tags$span(class = "ws-pill", date)
      )
    )
  })

  output$summary_metrics <- renderUI({
    req(rv$dta)
    st <- unlist(rv$status)
    n <- length(dta_dataset_names(rv$dta))
    np <- sum(st == "pass"); nf <- sum(st == "fail"); nd <- sum(st == "nodata")
    div(
      style = "display:flex; gap:18px; margin-bottom:8px; flex-wrap:wrap;",
      div(div(class = "metric", n), div(class = "slot-meta", "datasets")),
      div(div(class = "metric", np), div(class = "slot-meta", "passed")),
      div(div(class = "metric", nf), div(class = "slot-meta", "failed")),
      div(div(class = "metric", nd), div(class = "slot-meta", "no data"))
    )
  })

  # --- dataset detail (structure only -> stable file inputs) --------------
  output$dataset_detail <- renderUI({
    req(rv$active, rv$structure)
    s <- rv$structure[[rv$active]]
    req(!is.null(s))
    ds_idx <- s$index
    example_files <- if (isTRUE(rv$is_example)) dta_example_data_files() else character(0)

    if (length(s$handlers) == 0) {
      slots <- div(class = "msg-hint", "This dataset has no file handlers.")
    } else {
      slot_cards <- lapply(seq_along(s$handlers), function(hi) {
        h <- s$handlers[[hi]]
        upid <- sprintf("up_%d_%d", ds_idx, hi)
        exid <- sprintf("expick_%d_%d", ds_idx, hi)
        multiple <- isTRUE(!is.na(h$max) && h$max > 1)
        up_ctrl <- div(class = "dropzone",
                       fileInput(upid,
                                 label = if (multiple) "Drop or choose file(s)" else "Drop or choose a file",
                                 multiple = multiple))
        card(
          class = "slot-card",
          card_header(
            div(
              tags$strong("Expected: "),
              tags$span(class = "slot-expected", h$expected),
              tags$span(class = "slot-meta", paste0("  \u2022  ", h$count))
            )
          ),
          card_body(
            if (nzchar(h$hint)) div(class = "slot-meta", style = "margin-bottom:8px;", h$hint),
            if (length(example_files) > 0) {
              div(
                class = "slot-example",
                up_ctrl,
                div(
                  class = "slot-example-or",
                  tags$span("or"),
                  actionButton(
                    exid,
                    label = HTML("&#x1F4C2; Load an example file\u2026"),
                    class = "btn slot-example-btn",
                    title = "Pick a bundled example file to load into this slot"
                  )
                )
              )
            } else {
              up_ctrl
            }
          )
        )
      })
      slots <- do.call(bslib::layout_columns, c(list(col_widths = 12), slot_cards))
    }

    tagList(
      uiOutput("dataset_status_line"),
      div(
        class = "ds-actions",
        style = "margin-bottom:12px;",
        actionButton("check_one", "Check this dataset",
                     class = "btn btn-primary"),
        actionButton("edit_cols", label = HTML("&#x1F4D0; Edit columns"),
                     class = "btn btn-outline-secondary",
                     title = "Add, remove or edit column specifications"),
        actionButton("edit_rules", label = HTML("&#x2696;&#xFE0F; Edit rules"),
                     class = "btn btn-outline-secondary",
                     title = "Add, remove or edit validation rules"),
        downloadButton("dl_ds_yaml", "Export DataSet YAML",
                       class = "btn btn-outline-primary",
                       title = "Download this dataset's specification as YAML")
      ),
      #div(
      #  class = "msg-hint", style = "margin:-6px 0 12px;",
      #  "Uploads are validated against the file handler as you add them."
      #),
      #tags$h5("Expected files"),
      div(class = "msg-hint", style = "margin:-4px 0 10px;",
          "Drop each required file below. Loaded files appear underneath."),
      slots,
      card(
        card_header(
          div(
            style = "display:flex; justify-content:space-between; align-items:center; gap:8px;",
            tags$span("Loaded files"),
            actionButton("discard_all", label = HTML("&#x1F5D1;&#xFE0F; Discard all"),
                         class = "btn btn-sm btn-outline-danger",
                         title = "Remove all loaded files from this dataset")
          )
        ),
        card_body(uiOutput("loaded_files"))
      )
      # Validation messages now live in the floating, foldable dock pinned to
      # the bottom of the window (output$floating_msgs), not inline here.
    )
  })

  # --- main layout (landing vs workspace) --------------------------------
  output$main <- renderUI({
    # Depend ONLY on rv$structure (set once per load) so metadata edits and
    # uploads -- which mutate rv$dta -- do not rebuild the whole workspace or
    # reset the active tab / file inputs. Live bits live in their own outputs.
    if (is.null(rv$structure)) {
      # Landing
      restore_available <- file.exists(session_file)
      card(
        max_height = "620px",
        card_header(tags$h3("Load a DTA specification", style = "margin:0;")),
        card_body(
          p("Drag and drop a DTA ", tags$code(".yaml"),
            " file to begin, or load the bundled example."),
          div(class = "msg-hint", style = "margin:-6px 0 8px;",
              "A full DTA (with metadata) or a standalone dataset spec is accepted; ",
              "a dataset-only file is loaded without a Metadata section."),
          div(class = "dropzone",
              fileInput("dta_file", "Drop or choose a .yaml / .yml file",
                        accept = c(".yaml", ".yml"), width = "100%")),
          div(
            style = "display:flex; gap:10px; margin-top:8px;",
            actionButton("load_example", "Load example", class = "btn btn-outline-primary"),
            if (restore_available)
              actionButton("restore_session", "Restore previous session",
                           class = "btn btn-outline-secondary")
          )
        )
      )
    } else {
      # Workspace
      names_ds <- names(rv$structure)
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          uiOutput("workspace_header"),
          uiOutput("summary_metrics"),
          uiOutput("dataset_nav_ui"),
          actionButton("check_all", "Check all datasets", class = "btn btn-primary w-100"),
          tags$hr(),
          #div(style = "height:8px;"),
          downloadButton("dl_yaml", "Export DTA YAML", class = "btn btn-outline-primary w-100"),
          #div(style = "height:6px;"),
          downloadButton("dl_docx", "Export Word", class = "btn btn-outline-primary w-100"),
          #div(style = "height:6px;"),
          downloadButton("dl_pdf", "Export PDF", class = "btn btn-outline-primary w-100"),
          uiOutput("validation_report_ui"),
          tags$hr(),
          actionButton("reset_app", "Start over", class = "btn btn-outline-danger w-100")
        ),
        {
          # A standalone dataset YAML has no metadata -> hide the Metadata tab.
          dataset_only <- isolate(rv$dataset_only)
          metadata_panel <- nav_panel(
            "Metadata",
            uiOutput("metadata_editor")
          )
          panels <- list(
            nav_panel("Datasets", uiOutput("dataset_detail")),
            if (!isTRUE(dataset_only)) metadata_panel,
            nav_panel(
              "Raw YAML",
              div(
                class = "yaml-edit-bar",
                div(class = "msg-hint",
                    HTML("Edit the document and click <b>Apply changes</b>. It is validated as YAML <i>and</i> as a full DTA / DTADataSet before it replaces the loaded document — on any error nothing changes and the reason is shown below. Uploaded files are kept for datasets whose file handlers are unchanged; a dataset's validation is cleared only if its columns or rules changed, and deleted datasets drop their uploads.")),
                div(
                  class = "yaml-edit-actions",
                  actionButton("apply_yaml", "Apply changes", class = "btn btn-sm btn-primary"),
                  actionButton("revert_yaml", "Revert", class = "btn btn-sm btn-outline-secondary")
                )
              ),
              uiOutput("yaml_validation_msg"),
              if (requireNamespace("shinyAce", quietly = TRUE)) {
                div(
                  class = "yaml-ace-wrap",
                  shinyAce::aceEditor(
                    "raw_yaml_editor",
                    value = isolate(rv$yaml_text) %||% "",
                    mode = "yaml", theme = "tomorrow_night",
                    height = "55vh", fontSize = 13,
                    showLineNumbers = TRUE, wordWrap = FALSE,
                    debounce = 100, autoComplete = "disabled"
                  )
                )
              } else {
                textAreaInput("raw_yaml_editor", label = NULL,
                              value = isolate(rv$yaml_text), width = "100%", rows = 22)
              }
            )
          )
          do.call(navset_card_tab, Filter(Negate(is.null), panels))
        }
      )
    }
  })

  # --- restore previous session ------------------------------------------
  observeEvent(input$restore_session, {
    if (!file.exists(session_file)) return()
    saved <- tryCatch(readRDS(session_file), error = function(e) NULL)
    # Prefer the saveRDS-safe dump (arrow tables collected to data.frames);
    # fall back to a legacy `dta` field for older session files.
    restored <- if (!is.null(saved$dump)) {
      tryCatch(dta_restore_session(saved$dump), error = function(e) NULL)
    } else {
      saved$dta
    }
    if (is.null(saved) || is.null(restored)) {
      showNotification("Could not restore the previous session.", type = "error")
      return()
    }
    rv$dta <- restored
    rv$yaml_text <- saved$yaml_text
    rv$structure <- saved$structure %||% build_structure(restored)
    ups <- saved$uploads %||% list()
    rv$uploads <- lapply(ups, function(recs) {
      if (length(recs) == 0) return(list())
      if (is.character(recs)) {
        lapply(recs, function(f) list(file = f, table = tools::file_path_sans_ext(basename(f))))
      } else recs
    })
    rv$status <- saved$status %||% stats::setNames(
      rep("pending", length(dta_dataset_names(restored))),
      dta_dataset_names(restored)
    )
    rv$active <- saved$active %||% (dta_dataset_names(restored)[1] %||% NULL)
    rv$dataset_only <- isTRUE(saved$dataset_only)
    rv$is_example <- isTRUE(saved$is_example)
    rv$md_token <- rv$md_token + 1
    rv$contacts_token <- rv$contacts_token + 1
    showNotification("Previous session restored.", type = "message")
  })
}

shinyApp(ui, server)
