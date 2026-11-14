# The reason a deliverable .zip/.xpt carries on its loaded row when it was kept
# closed, and the sentence the summary shows for it.
`__DTAtools_delivery_kept__` <- "kept as one file: a type: any entry takes it as a deliverable; to have its files checked, name it as the container of their entries"

#' Load a Delivery: Files, Zip Archives and SAS Transport Libraries
#'
#' Routes every file in `files` to the dataset(s) whose file entry matches its
#' name and binds it with [load_file()]. A `.zip` is opened and its files are
#' routed the same way. A SAS transport library (`.xpt`) holding several
#' datasets is split, and member AE is offered as `ae.xpt`. A file entry with a
#' `container` expects its file inside a matching container; a file that
#' arrives elsewhere is still loaded, and the call says it did not conform.
#'
#' @section Routing:
#' A file entry is tried against a file in five tiers, best first: (1) a
#' literal entry (`pattern: false`) that has no container requirement or whose
#' container the file arrived in; (2) a pattern entry whose container the file
#' arrived in; (3) a literal entry whose container the file did not arrive in;
#' (4) a pattern entry without a container requirement; (5) a pattern entry
#' whose container the file did not arrive in. Within one dataset the best
#' entry wins (a fitting entry before a free one of the same tier, then the
#' first entry). Across datasets only the datasets at the best tier take the
#' file. When that tier is a literal one, all of them take it: two datasets
#' that name the same file both get it. When it is a pattern tier and several
#' datasets are there, the file is `ambiguous`: it is not loaded, its row lists
#' the candidate datasets, and `route` chooses. A file that arrived outside the
#' container of its entry is loaded all the same and reported as not
#' conforming (`conformant` is `FALSE`): validating the data and judging the
#' packaging are separate questions. When the same item arrives twice for a
#' dataset, a copy that conforms beats copies that do not; several copies that
#' all arrived outside the container are not loaded at all.
#'
#' A `.zip` is opened (only when it is one of `files`) when an entry declares
#' it as its container, or when no literal entry takes it, and a `.xpt` is
#' split when an entry declares it as its container, or when no literal
#' `type: xpt` entry takes it and it holds more than one dataset. A `.zip` or
#' `.xpt` that a `type: any` entry takes as a deliverable, by naming it or by
#' listing its ending in `extensions`, stays one file and is neither opened nor
#' scanned unless an entry declares it as its container; it may be taken even
#' when it arrived outside that entry's container (reported as not conforming)
#' as long as no entry's container matches the archive's own name. Only an
#' entry's own `container` reference, not the document's default container,
#' declares an archive that a literal `type: any` entry names and that is not a
#' fit there (`container: none`, or another container). A single-dataset
#' `.xpt` is a file: the entry that names it reads it whole. Only the files an
#' entry takes are extracted from a zip, and only the members an entry takes
#' are split out of a library.
#'
#' With `datasets`, only those datasets take files: a file meant for another
#' dataset is `unused`, and a file that is ambiguous between a chosen dataset
#' and others goes to the chosen one when it is the only one left.
#'
#' @section Limits:
#' Four options bound one call. `DTAtools.container_max_members` (default
#' 10000) bounds the entries of one zip, the members of one library and the
#' files one call creates, extracted or split. `DTAtools.container_max_bytes`
#' (default 1e13) bounds the bytes one call writes. `DTAtools.container_max_member_bytes`
#' (default 1e13) bounds the largest single file one call extracts from a zip
#' or splits out of a library, streamed or not, and judges a gzip file that is
#' read into memory by what it inflates to. `DTAtools.container_max_ratio`
#' (default 1100) bounds a zip's declared uncompressed size against its size on
#' disk. The two byte limits are effectively off in R and are there to be
#' tightened; the Shiny app sets tighter values for its own calls.
#'
#' @section Safety:
#' The names in a zip are checked before anything is extracted: an absolute
#' path, a drive letter, a `..` segment, a symbolic link, an encrypted entry,
#' two entries that differ only in letter case, an entry count or total size
#' beyond the limits, and a name that cannot be written on some file systems
#' when an entry needs it, each refuse the archive. An entry that inflates to
#' another size than it declares is stopped near the declared size and removed.
#' In `on_error = "abort"` mode a refusal reaches the caller with its own
#' condition class.
#'
#' @section Files on disk:
#' A call writes below `exdir` (by default
#' `file.path(tempdir(), "DTAtools_delivery")`) into a directory of its own,
#' `delivery-<random>`, with one subdirectory per container,
#' `<k>-<container name>`; a file of a zip sits in a directory of its own
#' below that. Files that no dataset took are
#' deleted before the call returns, and the whole directory is deleted when the
#' call fails. Files that were bound stay until the R session ends or `exdir`
#' is deleted: a lazy dataset reads them later. A call needs free disk space
#' under `exdir` for every member it extracts, at its full inflated size,
#' streamed or not (a 60 GB member streamed from a zip needs 60 GB there).
#'
#' @section Reports:
#' Validation results name the dataset and the table. The validation report's
#' inputs table names the container a file came from (its `Container` column,
#' the chain of archives joined with " > ", empty for a loose file), also for a
#' table that was read into memory, as do the `last_delivery` table and the
#' summary message. A file that did not arrive inside the container its entry
#' requires is listed by [container_findings()] and shown in the report's rows
#' and messages.
#'
#' @param x A `DTA`, or a `DTADataSet` (a `DTADataSetTabular` or a
#'   `DTADataSetFile`).
#' @param files Character vector of paths: loose files, `.zip` archives, and
#'   `.xpt` or `.xpt.gz` SAS transport files. The same file given twice is
#'   considered once.
#' @param stream Passed to [load_file()] for every file that is bound: `"auto"`
#'   (the default, from `getOption("DTAtools.stream", "auto")`), `"always"` or
#'   `"never"`.
#' @param on_error `"abort"` (the default) stops at the first failure, leaves
#'   `x` as it was and removes every file the call extracted (an empty `exdir`
#'   it created may remain). `"skip"` binds what can be bound,
#'   lists the rest as `refused` and warns once.
#' @param exdir Directory below which a call extracts. `NULL` means
#'   `file.path(tempdir(), "DTAtools_delivery")`.
#'
#' @param route Chooses the dataset for files that are `ambiguous`: a named
#'   list (or named character vector) from a file name, as in the `name` column
#'   of `last_delivery`, to dataset ids; `character(0)` sends the file to no
#'   dataset. It only applies to files that are ambiguous. A route to a dataset
#'   the file is not ambiguous between refuses the file as a whole
#'   (`on_error = "abort"`: an error of class `dta_delivery_bad_input`). Names
#'   that match no ambiguous file are listed in the summary message.
#' @param datasets `NULL` (all datasets) or the ids of the datasets this call
#'   loads. Files meant for other datasets are `unused`, and only the files of
#'   the chosen datasets are extracted from archives.
#'
#' @return `x` (the same class as given) with every routed file bound, and the
#'   attribute `"last_delivery"`: a data frame with one row per file
#'   considered and the columns `delivered`, `container`, `member`, `name`,
#'   `dataset`, `handler`, `item`, `status` (`"loaded"`, `"opened"`,
#'   `"unused"`, `"ignored"`, `"refused"` or `"ambiguous"`), `reason`,
#'   `conformant`, `candidates` and `path`, in that order. `conformant` is
#'   `TRUE` or `FALSE` for a loaded file whose entry has a container
#'   requirement (did it arrive inside it?) and `NA` otherwise. `candidates` is
#'   a list column: the dataset ids an `ambiguous` file matches, `character(0)`
#'   for every other row. The table describes this call only and is carried
#'   along unchanged by later calls.
#'
#' @seealso [load_file()], [check()], [DTAFileXPT]
#'
#' @examples
#' spec <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
#' dta <- read_dta_from_yaml(spec)
#'
#' # A delivery zip holding the data file.
#' csv <- system.file("extdata", "clinical_data.csv", package = "DTAtools")
#' zipfile <- file.path(tempdir(), "delivery.zip")
#' zip::zip(zipfile, files = csv, mode = "cherry-pick")
#'
#' dta <- load_delivery(dta, zipfile)
#' attr(dta, "last_delivery")[, c("name", "dataset", "status")]
#' checked <- check(dta, quiet = TRUE, persist = FALSE)
#' validation_status(datasets(checked, "clinical_data"))$ok
#'
#' # The same call takes the loose file.
#' dta2 <- load_delivery(read_dta_from_yaml(spec), csv)
#' names(tables(datasets(dta2, "clinical_data")))
#' unlink(zipfile)
#' unlink(file.path(tempdir(), "DTAtools_delivery"), recursive = TRUE)
#' @export
load_delivery <- function(x, files, stream = getOption("DTAtools.stream", "auto"),
                          on_error = c("abort", "skip"), exdir = NULL, route = NULL, datasets = NULL) {
  on_error <- match.arg(on_error)
  # An object restored from an older version lacks properties its class now has.
  if (dta_is_stale(x)) x <- dta_upgrade(x)
  if (!is.null(exdir) && (!is.character(exdir) || length(exdir) != 1 || is.na(exdir) || !nzchar(exdir))) {
    # The path is a field, not part of the message: in the app it is a server path.
    cli::cli_abort("Cannot create a directory in {.arg exdir}.", class = "dta_delivery_bad_input", exdir = exdir)
  }
  if (inherits(x, "DTAtools::DTADataSet")) {
    out <- load_delivery(DTA(datasets = list(x)), files,
      stream = stream, on_error = on_error, exdir = exdir,
      route = route, datasets = datasets
    )
    ds <- out@datasets[[1]]
    attr(ds, "last_delivery") <- attr(out, "last_delivery")
    return(ds)
  }
  if (!inherits(x, "DTAtools::DTA")) {
    cli::cli_abort("{.arg x} must be a {.cls DTA} or a {.cls DTADataSet}.", class = "dta_delivery_bad_input")
  }
  dta_delivery_check_files(files)
  route <- dta_delivery_check_route(route, names(x@datasets))
  if (!is.null(datasets) && (!is.character(datasets) || length(datasets) == 0 || anyNA(datasets) || !all(datasets %in% names(x@datasets)))) {
    cli::cli_abort("{.arg datasets} must name datasets of {.arg x}.", class = "dta_delivery_bad_input")
  }
  files <- files[!duplicated(normalizePath(files, winslash = "/", mustWork = FALSE))] # the same file twice is one file
  if (is.null(exdir)) exdir <- file.path(tempdir(), "DTAtools_delivery")
  limits <- dta_container_limits()
  slots <- dta_delivery_slots(x)
  st <- new.env(parent = emptyenv())
  st$rows <- list()
  st$bind <- list()
  st$produced <- character()
  st$run_dir <- NULL
  st$k <- 0L
  st$spent <- 0
  st$made <- 0
  st$opened <- character()
  st$claims <- new.env(parent = emptyenv()) # dta_delivery_claims_cached()
  st$decl <- Filter(function(s) !is.null(s$glob), slots) # entries that declare a container
  st$route <- route
  st$datasets <- datasets
  st$route_used <- character()
  st$outside <- character() # name -> glob of misfit loaded rows
  done <- FALSE
  on.exit(if (!done && !is.null(st$run_dir)) unlink(st$run_dir, recursive = TRUE), add = TRUE)

  queue <- lapply(seq_along(files), function(i) {
    list(
      path = files[[i]], name = basename(files[[i]]), chain = character(),
      member = NA_character_, top = i, depth = 0L, split_member = FALSE
    )
  })
  while (length(queue) > 0) {
    cand <- queue[[1]]
    queue <- queue[-1]
    queue <- c(dta_delivery_visit(cand, slots, st, limits, exdir, on_error, files), queue)
  }

  bind <- dta_delivery_resolve_duplicates(st, on_error, files)
  try_bind <- function(b) {
    tryCatch(
      {
        ds <- x@datasets[[b$dataset]]
        h <- ds@files[[b$handler]]
        dta_delivery_check_inflated(b$cand$path, h, stream, limits$max_member_bytes)
        # A tabular dataset is told the table name the plan reports; a file
        # dataset keys by the path, which load_file() takes from the file.
        tabular <- inherits(ds, "DTAtools::DTADataSetTabular")
        out <- if (tabular) {
          load_file(x, dataset = b$dataset, file = b$cand$path, handler_index = b$handler, name = b$item, stream = stream)
        } else {
          load_file(x, dataset = b$dataset, file = b$cand$path, handler_index = b$handler, stream = stream)
        }
        # How it arrived: load_file() recorded "on its own"; this call knows better.
        out@datasets[[b$dataset]] <- dta_set_provenance(
          out@datasets[[b$dataset]], if (tabular) b$item else b$cand$path,
          list(file = b$cand$name, container = b$cand$chain, via = "delivery")
        )
        out
      },
      error = function(e) e
    )
  }
  for (b0 in bind) {
    # The copy that fits first; the misfit copies set aside for it (`alts`) are
    # tried in turn only when it fails to load.
    cands <- c(list(b0), b0$alts)
    loaded_at <- 0L
    for (j in seq_along(cands)) {
      b <- cands[[j]]
      res <- try_bind(b)
      if (!inherits(res, "error")) {
        loaded_at <- j
        break
      }
      if (on_error == "abort") {
        file <- b$cand$name
        container <- if (length(b$cand$chain)) paste(b$cand$chain, collapse = " > ") else NA_character_
        dataset <- b$dataset
        # encodeString(): a name with a control character prints as an escape.
        file_shown <- encodeString(file)
        container_shown <- encodeString(container)
        cli::cli_abort(
          c(
            "Could not load {.file {file_shown}} into dataset {.field {dataset}}.",
            if (!is.na(container)) c("i" = "It was taken from {.file {container_shown}}.")
          ),
          class = "dta_delivery_load_error", parent = res,
          dataset = dataset, file = file, container = container
        )
      }
      # Dataset and entry stay on the row: the file was delivered, so it is not
      # "missing" from the entry it was meant for.
      dta_delivery_row(st, b$cand, files, "refused",
        dataset = b$dataset, handler = b$handler, reason = dta_delivery_reason(res, b$cand)
      )
    }
    if (loaded_at == 0L) next
    x <- res
    reason <- if (isFALSE(b$fit)) dta_delivery_misfit_reason(b$cand, b$glob) else NA_character_
    if (isFALSE(b$fit)) st$outside[[b$cand$name]] <- b$glob
    if (isTRUE(b$kept)) {
      reason <- if (is.na(reason)) `__DTAtools_delivery_kept__` else paste0(reason, "; ", `__DTAtools_delivery_kept__`)
    }
    dta_delivery_row(st, b$cand, files, "loaded",
      dataset = b$dataset, handler = b$handler, item = b$item,
      reason = reason, conformant = if (is.null(b$glob)) NA else isTRUE(b$fit)
    )
    # The copies set aside that were not tried name the copy that was loaded.
    for (o in cands[-seq_len(loaded_at)]) {
      dta_delivery_row(st, o$cand, files, "unused",
        dataset = o$dataset, handler = o$handler,
        reason = sprintf("another copy was loaded from %s", dta_delivery_where(b))
      )
    }
  }
  plan <- dta_delivery_plan(st$rows)
  keep <- unique(plan$path[plan$status == "loaded"])
  drop <- setdiff(st$produced, keep)
  unlink(drop)

  # encodeString(): a name with a control character prints as an escape.
  delivered <- encodeString(unique(plan$delivered))
  if (!any(plan$status == "loaded")) {
    # Refusals first, then the files nothing took; system files (`ignored`)
    # stay in the plan but would only push the reasons out of the message.
    why <- plan[plan$status %in% c("refused", "ambiguous", "unused"), , drop = FALSE]
    why <- why[order(match(why$status, c("refused", "ambiguous", "unused"))), , drop = FALSE]
    lines <- if (nrow(why)) .cli_escape(paste0(encodeString(why$name), ": ", why$reason)) else character()
    if (length(lines) > 10) lines <- c(lines[1:10], paste("and", length(lines) - 10, "more"))
    names(lines) <- rep("i", length(lines))
    cli::cli_abort(
      c("Nothing in {.file {delivered}} could be loaded into the specification.", lines),
      class = "dta_delivery_nothing_loaded", plan = plan
    )
  }

  loaded <- plan[plan$status == "loaded", , drop = FALSE]
  n <- nrow(loaded)
  k <- length(unique(loaded$dataset))
  unused <- plan$name[plan$status == "unused"]
  missing <- dta_delivery_missing(slots, st$opened, plan[plan$status %in% c("loaded", "refused"), , drop = FALSE], st$datasets)
  kept <- unique(loaded$name[endsWith(ifelse(is.na(loaded$reason), "", loaded$reason), `__DTAtools_delivery_kept__`)])
  ambiguous <- unique(plan$name[plan$status == "ambiguous"])
  outside <- unique(names(st$outside))
  route_idle <- setdiff(names(st$route), st$route_used)
  cap <- function(v) if (length(v) > 10) c(v[1:10], paste("and", length(v) - 10, "more")) else v
  unused_shown <- cap(encodeString(unused))
  missing_shown <- cap(encodeString(missing))
  # Plain interpolation (values are data, never markup): the text
  # "Not inside its required container: ae.csv (sdtm_csv.zip)" without quotes.
  ambiguous_shown <- cap(encodeString(ambiguous))
  outside_shown <- cap(encodeString(unique(paste0(names(st$outside), " (", st$outside, ")"))))
  route_idle_shown <- cap(encodeString(route_idle))
  kept_shown <- encodeString(kept)
  kept_reason <- `__DTAtools_delivery_kept__`
  cli::cli_inform(
    c(
      "Loaded {n} file{?s} into {k} dataset{?s} from {.file {delivered}}.",
      if (length(missing)) c("!" = "Not in the delivery: {.file {missing_shown}}"),
      if (length(ambiguous)) c("!" = "Matches several datasets, not loaded: {ambiguous_shown}"),
      if (length(outside)) c("!" = "Not inside its required container: {outside_shown}"),
      if (length(route_idle)) c("i" = "route names files that were not ambiguous: {route_idle_shown}"),
      if (length(unused)) c("i" = "Not used: {.file {unused_shown}}"),
      if (length(kept)) c("i" = "{.file {kept_shown}}: {kept_reason}.")
    ),
    class = "dta_delivery_summary", missing = missing, unused = unused, kept = kept,
    ambiguous = ambiguous, outside = outside
  )
  refused <- plan[plan$status == "refused", , drop = FALSE]
  if (on_error == "skip" && nrow(refused) > 0) {
    lines <- .cli_escape(paste0(encodeString(refused$name), ": ", refused$reason))
    names(lines) <- rep("x", length(lines))
    nr <- nrow(refused)
    cli::cli_warn(
      c("{nr} file{?s} of the delivery could not be loaded.", lines),
      class = "dta_delivery_refused", refused = refused
    )
  }
  done <- TRUE
  attr(x, "last_delivery") <- plan
  x
}

dta_delivery_check_files <- function(files) {
  if (!is.character(files) || length(files) == 0 || anyNA(files) || any(!nzchar(files))) {
    cli::cli_abort("{.arg files} must be one or more paths to files.", class = "dta_delivery_bad_input", files = files)
  }
  missing <- files[!file.exists(files)]
  if (length(missing)) {
    cli::cli_abort(
      "{cli::qty(length(missing))}{?This file does/These files do} not exist: {.file {missing}}",
      class = "dta_delivery_bad_input", files = files
    )
  }
  dirs <- files[dir.exists(files)]
  if (length(dirs)) {
    cli::cli_abort(
      "{.file {dirs}} {?is a directory/are directories}; pass the files in it, e.g. {.code list.files(dir, recursive = TRUE, full.names = TRUE)}.",
      class = "dta_delivery_bad_input", files = files
    )
  }
  invisible(files)
}

dta_delivery_slots <- function(x) {
  out <- list()
  for (id in names(x@datasets)) {
    ds <- x@datasets[[id]]
    for (i in seq_along(ds@files)) {
      h <- ds@files[[i]]
      out[[length(out) + 1]] <- list(
        dataset = id, handler = i, h = h, type = ds@type,
        literal = !isTRUE(h@pattern),
        ref = dta_handler_container(h), # the raw reference
        glob = dta_effective_container(x, h)$filename, # where it must arrive; NULL: nowhere
        xpt = inherits(h, "DTAtools::DTAFileXPT"),
        any = inherits(h, "DTAtools::DTAFileAny"),
        ext = if (inherits(h, "DTAtools::DTAFileAny")) h@extensions
      )
    }
  }
  out
}

dta_delivery_item <- function(type, name) {
  if (identical(type, "file")) name else tools::file_path_sans_ext(dta_strip_compression_extension(name))
}


# The best slot of ONE dataset's `slots` for `cand`: lowest tier
# (1 literal fit/free, 2 pattern fit, 3 literal misfit, 4 pattern free,
# 5 pattern misfit); ties: fit before free, then entry order. NULL if none.
dta_delivery_best_slot <- function(slots, cand, kind = dta_container_kind(cand$name)) {
  best <- NULL
  for (s in slots) {
    if (!isTRUE(any(matches_filename(s$h, cand$name)))) next
    if (!dta_entry_takes_kind(s$h, kind)) next
    deliverable <- s$any && (s$literal || isTRUE(kind %in% s$ext))
    # Own-name fit only for a deliverable claim ("a file never has to arrive
    # inside itself"); a chain element fits for every claim.
    fit <- if (is.null(s$glob)) {
      NA
    } else {
      (deliverable && dta_glob_matches(s$glob, cand$name)) || dta_glob_matches(s$glob, cand$chain)
    }
    # Archive rule: a type: any slot takes a .zip/.xpt as a fit, or as a misfit
    # deliverable when no entry declares the archive as its container
    # (`cand$declared` FALSE); declared, it is packaging and never a document.
    if (s$any && !is.na(kind) && isFALSE(fit) && !(deliverable && isFALSE(cand$declared))) next
    tier <- if (s$literal) {
      if (isFALSE(fit)) 3L else 1L
    } else if (isTRUE(fit)) 2L else if (is.na(fit)) 4L else 5L
    if (is.null(best) || tier < best$tier || (tier == best$tier && isTRUE(fit) && is.na(best$fit))) {
      best <- list(slot = s, tier = tier, fit = fit, deliverable = deliverable)
    }
  }
  best
}

# The top-tier claims for `cand` (route and ambiguity are decided later).
dta_delivery_claims <- function(slots, cand, kind) {
  ds <- vapply(slots, `[[`, "", "dataset")
  by <- split(slots, factor(ds, levels = unique(ds)))
  if (!is.na(kind)) cand$declared <- dta_delivery_declares(Filter(function(s) !is.null(s$glob), slots), cand$name)
  claims <- list()
  for (id in names(by)) {
    b <- dta_delivery_best_slot(by[[id]], cand, kind)
    if (is.null(b)) next
    s <- b$slot
    claims[[length(claims) + 1]] <- list(
      dataset = id, handler = s$handler, item = dta_delivery_item(s$type, cand$name),
      literal = s$literal, xpt = s$xpt, deliverable = b$deliverable,
      tier = b$tier, fit = b$fit, glob = s$glob
    )
  }
  if (!length(claims)) {
    return(list(claims = claims))
  }
  tiers <- vapply(claims, `[[`, 1L, "tier")
  list(claims = claims[tiers == min(tiers)])
}

# Who takes the file. Ambiguity (a pattern top tier held by several datasets,
# unless routed) is judged on every kept dataset (`tied`); `datasets` applies
# last: of a tied set, the datasets in scope are the candidates, and one left
# takes the file.
dta_delivery_decide <- function(st, claims, routed = FALSE) {
  ds <- vapply(claims, `[[`, "", "dataset")
  scope <- if (is.null(st$datasets)) rep(TRUE, length(ds)) else ds %in% st$datasets
  tied <- !isTRUE(routed) && length(claims) > 1 && !claims[[1]]$tier %in% c(1L, 3L)
  if (tied && sum(scope) > 1L) {
    return(list(bind = list(), ambiguous = ds[scope], tied = ds, outside_scope = character()))
  }
  list(bind = claims[scope], ambiguous = character(), tied = if (tied) ds else character(), outside_scope = ds[!scope])
}

dta_delivery_check_route <- function(route, ids) {
  if (is.null(route)) {
    return(NULL)
  }
  if (is.character(route)) route <- as.list(route)
  ok <- is.list(route) && length(route) > 0 && !is.null(names(route)) && all(nzchar(names(route))) &&
    !anyNA(names(route)) && !anyDuplicated(names(route)) &&
    all(vapply(route, function(v) is.character(v) && !anyNA(v) && all(v %in% ids), logical(1)))
  if (!ok) {
    cli::cli_abort("{.arg route} must map file names to dataset ids of {.arg x}.", class = "dta_delivery_bad_input")
  }
  route
}

dta_delivery_and <- function(v) {
  v <- sprintf("'%s'", v)
  if (length(v) < 2) v else paste(paste(v[-length(v)], collapse = ", "), "and", v[[length(v)]])
}

dta_delivery_misfit_reason <- function(cand, glob) {
  arrived <- if (length(cand$chain)) sprintf("it arrived inside '%s'", paste(cand$chain, collapse = " > ")) else "it arrived loose"
  case <- dta_glob_matches(glob, c(cand$chain, cand$name), ignore.case = TRUE)
  sprintf(
    "loaded, but %s and the agreement requires it inside '%s'%s",
    arrived, glob, if (case) " (letter case must match)" else ""
  )
}

# Which of `names` (base names) could the slot's name or pattern take? The test
# of dta_matches_filename_base(), vectorised over the names; matches_filename()
# also applies an `extensions` restriction, so this is a superset of it.
dta_delivery_slot_hits <- function(s, names) {
  stripped <- dta_strip_compression_extension(names)
  f <- s$h@filename
  if (s$literal) {
    return(names %in% f | stripped %in% f)
  }
  hit <- logical(length(names))
  for (p in f) hit <- hit | stringr::str_detect(names, p) | stringr::str_detect(stripped, p)
  hit
}

# dta_delivery_claims() once per name, chain and kind (cached in `st`). A name
# no file entry could take (`maybe` FALSE, or no slot hit) is answered without
# calling matches_filename(), so routing stays linear in the entries.
dta_delivery_claims_cached <- function(st, slots, cand, kind, maybe = NULL) {
  key <- paste(cand$name, paste(cand$chain, collapse = "\r"), kind, sep = "\n")
  if (!is.null(st$claims[[key]])) {
    return(st$claims[[key]])
  }
  if (is.null(maybe)) maybe <- any(vapply(slots, function(s) any(dta_delivery_slot_hits(s, cand$name)), logical(1)))
  res <- if (isTRUE(maybe)) dta_delivery_claims(slots, cand, kind) else list(claims = list())
  st$claims[[key]] <- res
  res
}


dta_delivery_declares <- function(slots, name) {
  # one test per distinct glob, not per slot (a default container gives every
  # slot the same glob)
  globs <- unique(unlist(lapply(slots, `[[`, "glob")))
  any(vapply(globs, function(g) dta_glob_matches(g, name), logical(1)))
}

dta_delivery_row <- function(st, cand, files, status, reason = NA_character_, dataset = NA_character_,
                             handler = NA_integer_, item = NA_character_, conformant = NA,
                             candidates = character()) {
  st$rows[[length(st$rows) + 1]] <- list(
    delivered = basename(files[[cand$top]]),
    container = if (length(cand$chain)) paste(cand$chain, collapse = " > ") else NA_character_,
    member = cand$member, name = cand$name, dataset = dataset,
    handler = as.integer(handler), item = item, status = status, reason = reason,
    conformant = conformant, candidates = candidates,
    path = cand$path
  )
}

# The rows as the documented data frame (one column at a time: an archive of
# thousands of entries must not cost thousands of rbind() calls).
dta_delivery_plan <- function(rows) {
  col <- function(k, type) vapply(rows, function(r) r[[k]], type)
  data.frame(
    delivered = col("delivered", ""), container = col("container", ""), member = col("member", ""),
    name = col("name", ""), dataset = col("dataset", ""), handler = col("handler", 1L),
    item = col("item", ""), status = col("status", ""), reason = col("reason", ""),
    conformant = col("conformant", NA),
    candidates = I(lapply(rows, function(r) r$candidates)),
    path = col("path", ""), stringsAsFactors = FALSE
  )
}

# A condition message with the on-disk path of the file replaced by its
# delivery label, so no server path reaches a report or a browser. cli wraps a
# message at its spaces, so whitespace is collapsed first (in the message and
# in the paths), and the longest spelling goes first: a relative path is a
# suffix of the absolute one, and replacing it first would leave the working
# directory behind.
dta_delivery_reason <- function(e, cand) {
  msg <- gsub("\\s+", " ", conditionMessage(e))
  if (is.na(cand$path)) {
    return(msg)
  }
  label <- paste(c(cand$chain, cand$name), collapse = " > ")
  ps <- unique(c(
    normalizePath(cand$path, winslash = "/", mustWork = FALSE),
    normalizePath(cand$path, winslash = "\\", mustWork = FALSE),
    cand$path
  ))
  ps <- gsub("\\s+", " ", ps[order(-nchar(ps))])
  # Through a placeholder, so the base name below is not found again inside
  # the label written for a full path.
  hole <- "\001"
  for (p in ps) msg <- gsub(p, hole, msg, fixed = TRUE)
  # The readers name a file by its base name only (a server path must not
  # reach a message), so that is where the label goes as well -- as a whole
  # name, not as the tail of another one ("data.csv" holds "a.csv").
  bn <- basename(cand$path)
  if (nzchar(bn)) {
    rx <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", bn)
    msg <- gsub(paste0("(?<![\\w.-])", rx, "(?![\\w-]|\\.\\w)"), hole, msg, perl = TRUE)
  }
  gsub(hole, label, msg, fixed = TRUE)
}

# A gzip file that will be read into memory (a SAS dataset, or a delimited file
# that `stream` does not stream) is judged by what it inflates to, not by its
# size on disk. Counting stops once past the cap, and a file that cannot exceed
# it (deflate cannot pass about 1032:1) is not read at all. dta_read_xpt()
# checks its own member size, so transport files are left to it.
dta_delivery_check_inflated <- function(path, h, stream, cap) {
  if (!grepl("\\.gz$", path, ignore.case = TRUE) || file.size(path) * 1032 <= cap) {
    return(invisible())
  }
  if (inherits(h, "DTAtools::DTAFileXPT")) {
    return(invisible())
  }
  in_memory <- inherits(h, "DTAtools::DTAFileSAS7BDAT") ||
    (inherits(h, "DTAtools::DTAFileTabular") && !dta_resolve_stream_mode(stream, path))
  if (!in_memory) {
    return(invisible())
  }
  con <- gzfile(path, "rb")
  on.exit(close(con))
  n <- 0
  repeat {
    b <- readBin(con, "raw", `__DTAtools_container_chunk__`)
    if (!length(b)) {
      return(invisible())
    }
    n <- n + length(b)
    if (n > cap) dta_container_too_large(path, "DTAtools.container_max_member_bytes", n, cap)
  }
}

dta_delivery_new_dir <- function(st, exdir, label) {
  if (is.null(st$run_dir)) {
    st$run_dir <- file.path(exdir, basename(tempfile("delivery-")))
    dir.create(st$run_dir, recursive = TRUE, showWarnings = FALSE, mode = "0700")
    if (!dir.exists(st$run_dir)) {
      # The path is a field, not part of the message: in the app it is a server path.
      cli::cli_abort("Cannot create a directory in {.arg exdir}.", class = "dta_delivery_bad_input", exdir = exdir)
    }
  }
  st$k <- st$k + 1L
  # The name is cut to at most 100 bytes (whole characters), so "<k>-<name>"
  # stays within the 255-byte component limit of ext4 and APFS as well as
  # NTFS's 255 characters; <k> keeps cut names apart.
  lab <- label
  while (nchar(lab, type = "bytes") > 100) lab <- substr(lab, 1, nchar(lab) - 1)
  # Windows drops a trailing dot or space from a directory name, so the path
  # built from the cut name would no longer be the one that was created.
  lab <- sub("[. ]+$", "", lab)
  d <- file.path(st$run_dir, paste0(st$k, "-", lab))
  dir.create(d, showWarnings = FALSE, mode = "0700")
  if (!dir.exists(d)) {
    cli::cli_abort("Cannot create a directory in {.arg exdir}.", class = "dta_delivery_bad_input", exdir = exdir)
  }
  d
}

dta_delivery_charge <- function(st, path, bytes, n_files, limits) {
  if (st$spent + bytes > limits$max_bytes) {
    dta_container_too_large(path, "DTAtools.container_max_bytes", st$spent + bytes, limits$max_bytes)
  }
  if (st$made + n_files > limits$max_members) {
    dta_container_too_large(path, "DTAtools.container_max_members", st$made + n_files, limits$max_members)
  }
  st$spent <- st$spent + bytes
  st$made <- st$made + n_files
}

# Record that `cand` is to be bound into the dataset and file entry of each claim.
dta_delivery_bind_claims <- function(st, cand, claims, kept_closed = FALSE) {
  for (c in claims) {
    st$bind[[length(st$bind) + 1]] <- list(
      cand = cand, dataset = c$dataset, handler = c$handler, item = c$item,
      kept = kept_closed && isTRUE(c$deliverable), fit = c$fit, glob = c$glob
    )
  }
}

dta_delivery_visit <- function(cand, slots, st, limits, exdir, on_error, files) {
  kind <- dta_container_kind(cand$name)
  body <- function() {
    if (is.na(cand$path)) {
      # An archive entry that was not extracted: nothing in scope claims it.
      cl <- dta_delivery_claims_cached(st, slots, cand, kind)
      dec <- dta_delivery_decide(st, cl$claims)
      dta_delivery_row(st, cand, files, "unused", reason = dta_delivery_unused_reason(cand, kind, slots, dec))
      return(list())
    }
    cl <- dta_delivery_claims_cached(st, slots, cand, kind)
    declared <- dta_delivery_declares(st$decl, cand$name)
    # An archive a literal type: any entry names, and which is not itself a fit
    # there, is a deliverable: only an entry's OWN reference (not the default
    # container's glob) makes it a container.
    if (declared && any(vapply(cl$claims, function(c) c$literal && isTRUE(c$deliverable) && !isTRUE(c$fit), logical(1)))) {
      declared <- dta_delivery_declares(Filter(function(s) !is.null(s$ref) && !identical(s$ref, "none"), st$decl), cand$name)
    }
    xmem <- NULL
    # Order: opening flags from the (routed, in-scope) claims ->
    # an opened library drops its xpt claims -> ambiguity on what remains.
    settle <- function(claims, routed) {
      fl <- if (is.null(st$datasets)) claims else Filter(function(c) c$dataset %in% st$datasets, claims)
      literal <- any(vapply(fl, `[[`, TRUE, "literal"))
      # A container a `type: any` entry takes as a deliverable stays closed,
      # unless an entry declares it as its container.
      kept <- any(vapply(fl, `[[`, TRUE, "deliverable"))
      open <- FALSE
      fail <- NULL
      if (identical(kind, "zip")) {
        open <- cand$depth == 0L && (declared || !(literal || kept))
      } else if (identical(kind, "xpt") && !cand$split_member && cand$depth <= 1L) {
        lit_xpt <- any(vapply(fl, function(c) c$literal && c$xpt, TRUE))
        if (declared || !(lit_xpt || kept)) {
          # Is it a library? Only a scan can tell (once per visit).
          if (is.null(xmem)) xmem <<- tryCatch(dta_xpt_members(cand$path, limits), dta_container_error = function(e) e)
          if (inherits(xmem, "condition")) fail <- xmem else open <- declared || nrow(xmem) > 1
        }
      }
      # An opened library is never also read whole.
      if (open) claims <- Filter(function(c) !c$xpt, claims)
      list(open = open, kept = kept, fail = fail, claims = claims, dec = dta_delivery_decide(st, claims, routed))
    }
    s <- settle(cl$claims, FALSE)
    route_rows <- FALSE
    if (length(s$dec$tied) && !is.null(st$route) && cand$name %in% names(st$route)) {
      # A route settles ambiguity only, and one row per file says what became of it.
      st$route_used <- union(st$route_used, cand$name)
      r <- st$route[[cand$name]]
      bad <- setdiff(r, s$dec$tied)
      if (length(bad)) {
        # One misrouted id refuses the file as a whole: nothing is bound.
        msg <- sprintf(
          "route sends it to dataset '%s', which is not one of the datasets it is ambiguous between (%s)",
          bad[[1]], dta_delivery_and(s$dec$tied)
        )
        if (on_error == "abort") {
          cli::cli_abort(
            "{.arg route} sends {.file {encodeString(cand$name)}} to dataset {.field {bad[[1]]}}, which is not one of the datasets it is ambiguous between ({dta_delivery_and(s$dec$tied)}).",
            class = "dta_delivery_bad_input"
          )
        }
        dta_delivery_row(st, cand, files, "refused", reason = msg)
        # Nothing is bound, but the opening decision stays the un-routed one.
        if (!s$open) {
          return(list())
        }
        s$dec <- list(bind = list(), ambiguous = character(), tied = character(), outside_scope = character())
      } else {
        route_rows <- TRUE
        if (length(r)) {
          s <- settle(Filter(function(c) c$dataset %in% r, cl$claims), TRUE)
          route_rows <- FALSE
        } else {
          # Routed to no dataset: the opening decision stays what it was without
          # the route, and nothing is bound.
          dta_delivery_row(st, cand, files, "unused", reason = "route sends it to no dataset")
          s$dec <- list(bind = list(), ambiguous = character(), tied = character(), outside_scope = character())
        }
      }
    }
    if (!is.null(s$fail)) {
      # A scan failure is the file's own problem when something wants the file
      # (an entry declares it as a container or claims it, or a limit stopped the
      # scan and the specification has a `type: xpt` entry); otherwise it is the
      # reason the file was not used.
      wanted <- declared || length(cl$claims) > 0 ||
        (inherits(s$fail, "dta_container_too_large") && any(vapply(slots, `[[`, TRUE, "xpt")))
      if (!wanted) {
        dta_delivery_row(st, cand, files, "unused", reason = dta_delivery_reason(s$fail, cand))
        return(list())
      }
      # As for an unreadable .zip: a `type: any` entry that takes the file keeps
      # it as a file, so skip mode reports only that opening it failed.
      dta_delivery_bind_claims(st, cand, Filter(function(c) !c$xpt, s$dec$bind))
      rlang::cnd_signal(s$fail)
    }
    open <- s$open
    dec <- s$dec
    # Kept closed only because a deliverable claim keeps it: its loaded row and
    # the summary say so, and name the remedy.
    kept_closed <- s$kept && !declared && !open &&
      ((identical(kind, "zip") && cand$depth == 0L) || (identical(kind, "xpt") && !cand$split_member && cand$depth <= 1L))
    dta_delivery_bind_claims(st, cand, dec$bind, kept_closed)
    if (length(dec$ambiguous)) {
      amb <- cand
      amb$path <- NA_character_ # the extracted copy is deleted (red-team D9)
      dta_delivery_row(st, amb, files, "ambiguous",
        candidates = dec$ambiguous,
        reason = sprintf(
          "matches the file entries of datasets %s; pass route = list(\"%s\" = \"<dataset>\") to choose",
          dta_delivery_and(dec$ambiguous), cand$name
        )
      )
    }
    if (open) {
      st$opened <- c(st$opened, cand$name)
      new <- if (identical(kind, "zip")) {
        dta_delivery_open_zip(cand, slots, st, limits, exdir, on_error, files)
      } else {
        dta_delivery_open_xpt(cand, xmem, slots, st, limits, exdir, on_error, files)
      }
      dta_delivery_row(st, cand, files, "opened")
      return(new)
    }
    if (length(dec$bind) == 0 && !length(dec$ambiguous) && !route_rows) {
      dta_delivery_row(st, cand, files, "unused", reason = dta_delivery_unused_reason(cand, kind, slots, dec))
    }
    list()
  }
  if (on_error == "abort") {
    return(body())
  }
  # A refused container gives back what it was charged; its files are already
  # gone (dta_zip_extract() and dta_xpt_split() delete its directory on failure).
  spent0 <- st$spent
  made0 <- st$made
  tryCatch(body(), dta_container_error = function(e) {
    st$spent <- spent0
    st$made <- made0
    st$opened <- setdiff(st$opened, cand$name) # its declared files are not "missing"
    reason <- dta_delivery_reason(e, cand)
    bound <- any(vapply(st$bind, function(b) {
      identical(b$cand$path, cand$path) && identical(b$cand$chain, cand$chain)
    }, logical(1)))
    if (bound) {
      # Claimed and bound as a file in its own right; only opening it failed.
      dta_delivery_row(st, cand, files, "opened", reason = paste0("could not be opened: ", reason))
    } else {
      dta_delivery_row(st, cand, files, "refused", reason = reason)
    }
    list()
  })
}

dta_delivery_unused_reason <- function(cand, kind, slots, dec = NULL) {
  # Outranks every other reason, `unprobed` included (red-team D14).
  if (length(dec$outside_scope)) {
    return(sprintf("is for dataset %s, which this call does not load", dta_delivery_and(dec$outside_scope)))
  }
  if (identical(kind, "zip") && cand$depth >= 1L) {
    return("is a .zip inside a .zip, which is not opened")
  }
  if (!is.null(cand$problem) && !is.na(cand$problem)) {
    return(sprintf("matches no file entry of the specification, and it %s, so it is not extracted", cand$problem))
  }
  if (isTRUE(cand$unprobed)) {
    return(sprintf(
      "is a .xpt that was not opened: other .xpt files of %s are named by the specification, so a library in it is opened only when an entry declares it (container: %s)",
      utils::tail(cand$chain, 1), cand$name
    ))
  }
  if (isTRUE(cand$split_member)) {
    lits <- unlist(lapply(Filter(function(s) s$literal, slots), function(s) s$h@filename))
    hit <- lits[tolower(lits) == cand$name & lits != cand$name]
    if (length(hit)) {
      return(sprintf("member %s is offered as %s; the specification declares %s", cand$member, cand$name, hit[[1]]))
    }
  }
  "matches no file entry of the specification"
}

dta_delivery_open_zip <- function(cand, slots, st, limits, exdir, on_error, files) {
  ins <- dta_zip_inspect(cand$path, limits)
  chain <- c(cand$chain, cand$name)
  unprobed <- rep(FALSE, nrow(ins))
  mk <- function(i, path) {
    list(
      path = path, name = ins$name[[i]], chain = chain, member = ins$entry[[i]],
      top = cand$top, depth = cand$depth + 1L, split_member = FALSE,
      problem = ins$problem[[i]], unprobed = unprobed[[i]]
    )
  }
  for (i in which(ins$ignored)) dta_delivery_row(st, mk(i, NA_character_), files, "ignored", reason = "is a system file")
  keep <- which(!ins$ignored)
  nm <- ins$name[keep]
  kinds <- dta_container_kind(nm)
  # One vectorised name test per file entry: names no entry could take are
  # settled before any claim is computed, so routing stays linear in the
  # entries. A `type: xpt` entry that matches no name here may want a member of
  # a library inside (its test is exact: DTAFileXPT has no `extensions`).
  hits <- lapply(slots, dta_delivery_slot_hits, names = nm)
  maybe <- if (length(hits)) Reduce(`|`, hits) else logical(length(nm))
  is_xpt <- vapply(slots, `[[`, TRUE, "xpt")
  xpt_wanted <- any(vapply(hits[is_xpt], function(h) !any(h), logical(1)))
  # Extracted when a kept dataset is in `datasets` (ambiguous ones included:
  # a library may still open).
  scoped <- function(claims) {
    if (is.null(st$datasets)) claims else Filter(function(c) c$dataset %in% st$datasets, claims)
  }
  claimed <- vapply(seq_along(keep), function(j) {
    length(scoped(dta_delivery_claims_cached(st, slots, mk(keep[[j]], ""), kinds[[j]], maybe = maybe[[j]])$claims)) > 0
  }, logical(1))
  # An archive that already holds a claimed .xpt uses the one-file-per-dataset
  # layout: its other .xpt entries are not probed for library members, and
  # their rows say so (dta_delivery_unused_reason()).
  if (xpt_wanted && any(claimed & kinds %in% "xpt")) {
    unprobed[keep] <- kinds %in% "xpt" & !claimed
    xpt_wanted <- FALSE
  }
  # red-team perf fix: only .xpt names can be declared libraries; test them only.
  declared <- kinds %in% "xpt"
  if (any(declared)) {
    declared[declared] <- vapply(nm[declared], function(n) dta_delivery_declares(st$decl, n), logical(1), USE.NAMES = FALSE)
  }
  # An entry whose name some file systems cannot hold refuses the archive only
  # when something needs it; unneeded, it is listed as unused (and is never
  # probed as a library).
  problem <- ins$problem[keep]
  forced <- (claimed | declared) & !is.na(problem)
  if (any(forced)) dta_zip_refuse_unsafe(cand$path, ins$entry[keep][forced], problem[forced], hostile = FALSE)
  need <- claimed | declared | (kinds %in% "xpt" & xpt_wanted & is.na(problem))
  # The per-member cap applies to every entry this call extracts, a library
  # that is only probed or declared as a container included.
  big <- need & ins$size[keep] > limits$max_member_bytes
  if (any(big)) {
    if (on_error == "abort") {
      i <- keep[which(big)[[1]]]
      dta_container_too_large(cand$path, "DTAtools.container_max_member_bytes", ins$size[[i]], limits$max_member_bytes)
    }
    for (j in which(big)) {
      i <- keep[[j]]
      # The entry it was meant for, when a file entry claims it (a library that
      # was only probed has none).
      cl <- dta_delivery_claims_cached(st, slots, mk(i, ""), kinds[[j]], maybe = maybe[[j]])$claims
      dta_delivery_row(st, mk(i, NA_character_), files, "refused",
        dataset = if (length(cl)) cl[[1]]$dataset else NA_character_,
        handler = if (length(cl)) cl[[1]]$handler else NA_integer_,
        reason = sprintf(
          "is larger (%s bytes) than DTAtools.container_max_member_bytes (%s)",
          dta_format_count(ins$size[[i]]), dta_format_count(limits$max_member_bytes)
        )
      )
    }
    need <- need & !big
  }
  sel <- keep[need]
  dta_delivery_charge(st, cand$path, sum(ins$size[sel]), length(sel), limits)
  paths <- rep(NA_character_, nrow(ins))
  if (length(sel)) {
    d <- dta_delivery_new_dir(st, exdir, cand$name)
    paths[sel] <- dta_zip_extract(cand$path, ins$entry[sel], ins$size[sel], d)
    st$produced <- c(st$produced, paths[sel])
  }
  lapply(setdiff(keep, keep[big]), function(i) mk(i, paths[[i]]))
}

dta_delivery_open_xpt <- function(cand, xmem, slots, st, limits, exdir, on_error, files) {
  chain <- c(cand$chain, cand$name)
  mk <- function(r, path) {
    list(
      path = path, name = xmem$name[[r]], chain = chain, member = xmem$member[[r]],
      top = cand$top, depth = cand$depth + 1L, split_member = TRUE
    )
  }
  if (nrow(xmem) == 0) {
    return(list())
  }
  size <- xmem$end - xmem$start + 240
  want <- vapply(seq_len(nrow(xmem)), function(r) {
    cl <- dta_delivery_claims_cached(st, slots, mk(r, ""), "xpt")$claims
    if (!is.null(st$datasets)) cl <- Filter(function(c) c$dataset %in% st$datasets, cl)
    length(cl) > 0
  }, logical(1))
  big <- want & size > limits$max_member_bytes
  if (any(big)) {
    if (on_error == "abort") {
      r <- which(big)[[1]]
      dta_container_too_large(cand$path, "DTAtools.container_max_member_bytes", size[[r]], limits$max_member_bytes)
    }
    for (r in which(big)) {
      cl <- dta_delivery_claims_cached(st, slots, mk(r, ""), "xpt")$claims
      dta_delivery_row(st, mk(r, NA_character_), files, "refused",
        dataset = if (length(cl)) cl[[1]]$dataset else NA_character_,
        handler = if (length(cl)) cl[[1]]$handler else NA_integer_,
        reason = sprintf(
          "is larger (%s bytes) than DTAtools.container_max_member_bytes (%s)",
          dta_format_count(size[[r]]), dta_format_count(limits$max_member_bytes)
        )
      )
    }
    want <- want & !big
  }
  paths <- rep(NA_character_, nrow(xmem))
  if (any(want)) {
    dta_delivery_charge(st, cand$path, sum(size[want]), sum(want), limits)
    d <- dta_delivery_new_dir(st, exdir, cand$name)
    paths[want] <- dta_xpt_split(cand$path, xmem[want, , drop = FALSE], d)
    st$produced <- c(st$produced, paths[want])
  }
  lapply(which(!big), function(r) mk(r, paths[[r]]))
}

dta_delivery_where <- function(b) paste(c(b$cand$chain, b$cand$name), collapse = " > ")

# Two bindings of the same table (or file key) in one dataset.
dta_delivery_resolve_duplicates <- function(st, on_error, files) {
  bind <- st$bind
  if (!length(bind)) {
    return(bind)
  }
  key <- vapply(bind, function(b) paste(b$dataset, b$item, sep = "\r"), "")
  label <- function(b) {
    if (length(b$cand$chain)) paste0(paste(b$cand$chain, collapse = " > "), ": ", b$cand$member) else basename(b$cand$path)
  }
  # One non-misfit copy beats misfit copies; misfit-only copies
  # are all unused (no abort, no refusal).
  drop <- logical(length(bind))
  for (k in unique(key[duplicated(key)])) {
    i <- which(key == k)
    mis <- vapply(bind[i], function(b) isFALSE(b$fit), logical(1))
    if (sum(!mis) == 1L || all(mis)) {
      if (all(mis)) {
        reason <- sprintf("several copies arrived outside '%s'; none was loaded", bind[[i[[1]]]]$glob)
        for (j in i) {
          b <- bind[[j]]
          dta_delivery_row(st, b$cand, files, "unused", dataset = b$dataset, handler = b$handler, reason = reason)
          drop[[j]] <- TRUE
        }
      } else {
        # Set aside, not dropped: they are tried (and reported) by the load loop,
        # which knows whether the copy that fits loaded.
        bind[[i[!mis]]]$alts <- bind[i[mis]]
        drop[i[mis]] <- TRUE
      }
    }
  }
  bind <- bind[!drop]
  key <- key[!drop]
  clash <- key %in% key[duplicated(key)]
  if (!any(clash)) {
    return(bind)
  }
  if (on_error == "abort") {
    first <- key[clash][[1]]
    hit <- bind[key == first]
    dataset <- hit[[1]]$dataset
    item <- hit[[1]]$item
    sources <- vapply(hit, label, "")
    # encodeString(): a name with a control character prints as an escape.
    sources_shown <- encodeString(sources)
    cli::cli_abort(
      "Two files of the delivery would both be loaded as {.val {item}} into dataset {.field {dataset}}: {.file {sources_shown}}.",
      class = "dta_delivery_duplicate_name", dataset = dataset, item = item, sources = sources
    )
  }
  for (b in bind[clash]) {
    dta_delivery_row(st, b$cand, files, "refused", dataset = b$dataset, handler = b$handler, reason = sprintf(
      "is one of several files of the delivery that would be loaded as '%s' into dataset '%s'", b$item, b$dataset
    ))
  }
  bind[!clash]
}

# File entries that declare a container this call opened but received nothing.
# `loaded` holds the rows of delivered files: a refused file was delivered too.
dta_delivery_missing <- function(slots, opened, loaded, datasets = NULL) {
  out <- character()
  for (s in slots) {
    if (is.null(s$glob) || !length(opened)) next
    if (!is.null(datasets) && !s$dataset %in% datasets) next
    if (!dta_glob_matches(s$glob, opened)) next
    # An optional entry (minimum 0 files) is never missing.
    if (isTRUE(s$h@min_number_of_files[1] == 0)) next
    got <- any(loaded$dataset == s$dataset & loaded$handler == s$handler, na.rm = TRUE)
    if (!got) out <- c(out, if (s$literal) s$h@filename else (s$h@pattern_description %||% s$h@filename))
  }
  unique(out)
}
