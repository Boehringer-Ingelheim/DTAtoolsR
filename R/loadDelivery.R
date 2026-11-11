# The reason a deliverable .zip/.xpt carries on its loaded row when it was kept
# closed, and the sentence the summary shows for it.
`__DTAtools_delivery_kept__` <- "kept as one file: a type: any entry takes it as a deliverable; to have its files checked, name it as the container of their entries"

#' Load a Delivery: Files, Zip Archives and SAS Transport Libraries
#'
#' Routes every file in `files` to the dataset(s) whose file entry matches its
#' name and binds it with [load_file()]. A `.zip` is opened and its files are
#' routed the same way. A SAS transport library (`.xpt`) holding several
#' datasets is split, and member AE is offered as `ae.xpt`. A file entry with
#' a `container` only takes files from inside a matching container.
#'
#' @section Routing:
#' A literal file entry (`pattern: false`) is tried before a pattern entry, and
#' the first literal entry that matches a file claims it, else the first
#' pattern entry. A file that a literal entry of any dataset claims is not also
#' claimed by the pattern entries of other datasets. A `.zip` is opened (only
#' when it is one of `files`) when an entry declares it as its `container`, or
#' when no literal entry claims it, and a `.xpt` is split when an entry
#' declares it as its `container`, or when no literal `type: xpt` entry claims
#' it and it holds more than one dataset. A `.zip` or `.xpt` that a `type: any`
#' entry takes as a deliverable, by naming it or by listing its ending in
#' `extensions`, stays one file and is neither opened nor scanned unless an
#' entry declares it as its `container`. A single-dataset `.xpt` is a file: the
#' entry that names it reads it whole. Only the files an entry claims are
#' extracted from a zip, and only the members an entry claims are split out of
#' a library.
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
#' Validation results name the dataset and the table. The validation report
#' names no zip or library for a table that was read into memory; the
#' `last_delivery` table and the summary message do.
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
#' @return `x` (the same class as given) with every routed file bound, and the
#'   attribute `"last_delivery"`: a data frame with one row per file
#'   considered and the columns `delivered`, `container`, `member`, `name`,
#'   `dataset`, `handler`, `item`, `status` (`"loaded"`, `"opened"`,
#'   `"unused"`, `"ignored"` or `"refused"`), `reason` and `path`. The table
#'   describes this call only and is carried along unchanged by later calls.
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
                          on_error = c("abort", "skip"), exdir = NULL) {
  on_error <- match.arg(on_error)
  if (!is.null(exdir) && (!is.character(exdir) || length(exdir) != 1 || is.na(exdir) || !nzchar(exdir))) {
    # The path is a field, not part of the message: in the app it is a server path.
    cli::cli_abort("Cannot create a directory in {.arg exdir}.", class = "dta_delivery_bad_input", exdir = exdir)
  }
  if (inherits(x, "DTAtools::DTADataSet")) {
    out <- load_delivery(DTA(datasets = list(x)), files, stream = stream, on_error = on_error, exdir = exdir)
    ds <- out@datasets[[1]]
    attr(ds, "last_delivery") <- attr(out, "last_delivery")
    return(ds)
  }
  if (!inherits(x, "DTAtools::DTA")) {
    cli::cli_abort("{.arg x} must be a {.cls DTA} or a {.cls DTADataSet}.", class = "dta_delivery_bad_input")
  }
  dta_delivery_check_files(files)
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
  st$decl <- Filter(function(s) !is.null(s$container), slots) # entries that declare a container
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
  for (b in bind) {
    res <- tryCatch(
      {
        h <- x@datasets[[b$dataset]]@files[[b$handler]]
        dta_delivery_check_inflated(b$cand$path, h, stream, limits$max_member_bytes)
        load_file(x, dataset = b$dataset, file = b$cand$path, handler_index = b$handler, stream = stream)
      },
      error = function(e) e
    )
    if (inherits(res, "error")) {
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
      dta_delivery_row(st, b$cand, files, "refused", reason = dta_delivery_reason(res, b$cand))
    } else {
      x <- res
      dta_delivery_row(st, b$cand, files, "loaded",
        dataset = b$dataset, handler = b$handler, item = b$item,
        reason = if (isTRUE(b$kept)) `__DTAtools_delivery_kept__` else NA_character_
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
    why <- plan[plan$status %in% c("refused", "unused"), , drop = FALSE]
    why <- why[order(why$status != "refused"), , drop = FALSE]
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
  missing <- dta_delivery_missing(slots, st$opened, loaded)
  kept <- unique(loaded$name[loaded$reason %in% `__DTAtools_delivery_kept__`])
  cap <- function(v) if (length(v) > 10) c(v[1:10], paste("and", length(v) - 10, "more")) else v
  unused_shown <- cap(encodeString(unused))
  missing_shown <- cap(encodeString(missing))
  kept_shown <- encodeString(kept)
  kept_reason <- `__DTAtools_delivery_kept__`
  cli::cli_inform(
    c(
      "Loaded {n} file{?s} into {k} dataset{?s} from {.file {delivered}}.",
      if (length(missing)) c("!" = "Not in the delivery: {.file {missing_shown}}"),
      if (length(unused)) c("i" = "Not used: {.file {unused_shown}}"),
      if (length(kept)) c("i" = "{.file {kept_shown}}: {kept_reason}.")
    ),
    class = "dta_delivery_summary", missing = missing, unused = unused, kept = kept
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
        literal = !isTRUE(h@pattern), container = dta_handler_container(h),
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

# Which file entries take `cand`? Per dataset the first literal entry that
# matches, else the first pattern entry; then, across datasets, a literal claim
# anywhere removes pattern claims everywhere else. A claim is `deliverable`
# when a `type: any` entry takes the file as a file in its own right (it names
# it, or lists its ending in `extensions`): such a container stays closed.
dta_delivery_claims <- function(slots, cand, kind) {
  claims <- list()
  mismatch <- NULL
  for (ds in unique(vapply(slots, `[[`, "", "dataset"))) {
    ss <- Filter(function(s) identical(s$dataset, ds), slots)
    ok <- vapply(ss, function(s) {
      if (!isTRUE(any(matches_filename(s$h, cand$name)))) {
        return(FALSE)
      }
      if (!is.null(s$container) && !dta_matches_container(s$h, cand$chain)) {
        if (is.null(mismatch)) mismatch <<- list(dataset = ds, container = s$container, chain = cand$chain)
        return(FALSE)
      }
      if (identical(kind, "zip") && !s$any) {
        return(FALSE)
      }
      if (identical(kind, "xpt") && !(s$any || s$xpt)) {
        return(FALSE)
      }
      TRUE
    }, logical(1))
    if (!any(ok)) next
    lit <- which(ok & vapply(ss, `[[`, TRUE, "literal"))
    pick <- if (length(lit)) lit[[1]] else which(ok)[[1]]
    s <- ss[[pick]]
    claims[[length(claims) + 1]] <- list(
      dataset = ds, handler = s$handler, item = dta_delivery_item(s$type, cand$name),
      literal = s$literal, xpt = s$xpt,
      deliverable = s$any && (s$literal || isTRUE(kind %in% s$ext))
    )
  }
  if (any(vapply(claims, `[[`, TRUE, "literal"))) claims <- Filter(function(c) c$literal, claims)
  list(claims = claims, mismatch = mismatch)
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
  res <- if (isTRUE(maybe)) dta_delivery_claims(slots, cand, kind) else list(claims = list(), mismatch = NULL)
  st$claims[[key]] <- res
  res
}

dta_delivery_declares <- function(slots, name) {
  any(vapply(slots, function(s) !is.null(s$container) && dta_matches_container(s$h, name), logical(1)))
}

dta_delivery_row <- function(st, cand, files, status, reason = NA_character_, dataset = NA_character_,
                             handler = NA_integer_, item = NA_character_) {
  st$rows[[length(st$rows) + 1]] <- list(
    delivered = basename(files[[cand$top]]),
    container = if (length(cand$chain)) paste(cand$chain, collapse = " > ") else NA_character_,
    member = cand$member, name = cand$name, dataset = dataset,
    handler = as.integer(handler), item = item, status = status, reason = reason,
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
  for (p in ps) msg <- gsub(p, label, msg, fixed = TRUE)
  msg
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
      kept = kept_closed && isTRUE(c$deliverable)
    )
  }
}

dta_delivery_visit <- function(cand, slots, st, limits, exdir, on_error, files) {
  kind <- dta_container_kind(cand$name)
  body <- function() {
    if (is.na(cand$path)) {
      # An archive entry that was not extracted: nothing claims it.
      cl <- dta_delivery_claims_cached(st, slots, cand, kind)
      dta_delivery_row(st, cand, files, "unused", reason = dta_delivery_unused_reason(cl, cand, kind, slots))
      return(list())
    }
    cl <- dta_delivery_claims_cached(st, slots, cand, kind)
    declared <- dta_delivery_declares(st$decl, cand$name)
    literal <- any(vapply(cl$claims, `[[`, TRUE, "literal"))
    # A container a `type: any` entry takes as a deliverable stays closed,
    # unless an entry declares it as its container.
    kept <- any(vapply(cl$claims, `[[`, TRUE, "deliverable"))
    open <- FALSE
    xmem <- NULL
    if (identical(kind, "zip")) {
      open <- cand$depth == 0L && (declared || !(literal || kept))
    } else if (identical(kind, "xpt") && !cand$split_member && cand$depth <= 1L) {
      lit_xpt <- any(vapply(cl$claims, function(c) c$literal && c$xpt, TRUE))
      if (declared || !(lit_xpt || kept)) {
        # Is it a library? Only a scan can tell. A scan failure is the file's
        # own problem when something wants the file; otherwise it is reported
        # as the reason the file was not used.
        xmem <- tryCatch(dta_xpt_members(cand$path, limits), dta_container_error = function(e) e)
        if (inherits(xmem, "condition")) {
          # Something wants the file when an entry declares it as a container or
          # claims it, or when a limit stopped the scan and the specification has
          # a `type: xpt` entry (the library may be theirs).
          wanted <- declared || length(cl$claims) > 0 ||
            (inherits(xmem, "dta_container_too_large") && any(vapply(slots, `[[`, TRUE, "xpt")))
          if (!wanted) {
            dta_delivery_row(st, cand, files, "unused", reason = dta_delivery_reason(xmem, cand))
            return(list())
          }
          # As for an unreadable .zip: a `type: any` entry that takes the file
          # keeps it as a file, so that skip mode reports only that opening it
          # failed, and abort mode aborts.
          dta_delivery_bind_claims(st, cand, Filter(function(c) !c$xpt, cl$claims))
          rlang::cnd_signal(xmem)
        }
        open <- declared || nrow(xmem) > 1
      }
    }
    if (open) {
      # An opened library is never also read whole.
      cl$claims <- Filter(function(c) !c$xpt, cl$claims)
    }
    # Kept closed only because a deliverable claim keeps it: its loaded row and
    # the summary say so, and name the remedy.
    kept_closed <- kept && !declared && !open &&
      ((identical(kind, "zip") && cand$depth == 0L) || (identical(kind, "xpt") && !cand$split_member && cand$depth <= 1L))
    dta_delivery_bind_claims(st, cand, cl$claims, kept_closed)
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
    if (length(cl$claims) == 0) {
      dta_delivery_row(st, cand, files, "unused", reason = dta_delivery_unused_reason(cl, cand, kind, slots))
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

dta_delivery_unused_reason <- function(cl, cand, kind, slots) {
  if (!is.null(cl$mismatch)) {
    m <- cl$mismatch
    names <- c(m$chain, dta_strip_compression_extension(m$chain))
    case <- any(grepl(dta_container_regex(m$container), names, ignore.case = TRUE, perl = TRUE))
    return(sprintf(
      "matches the file entry of dataset '%s', which expects it inside %s%s",
      m$dataset, m$container, if (case) " (letter case must match)" else ""
    ))
  }
  if (identical(kind, "zip") && cand$depth >= 1L) {
    return("is a .zip inside a .zip, which is not opened")
  }
  if (!is.null(cand$problem) && !is.na(cand$problem)) {
    return(sprintf("matches no file entry of the specification, and its name %s, so it is not extracted", cand$problem))
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
  claimed <- vapply(seq_along(keep), function(j) {
    length(dta_delivery_claims_cached(st, slots, mk(keep[[j]], ""), kinds[[j]], maybe = maybe[[j]])$claims) > 0
  }, logical(1))
  # An archive that already holds a claimed .xpt uses the one-file-per-dataset
  # layout: its other .xpt entries are not probed for library members, and
  # their rows say so (dta_delivery_unused_reason()).
  if (xpt_wanted && any(claimed & kinds %in% "xpt")) {
    unprobed[keep] <- kinds %in% "xpt" & !claimed
    xpt_wanted <- FALSE
  }
  declared <- kinds %in% "xpt" & vapply(nm, function(n) dta_delivery_declares(st$decl, n), logical(1), USE.NAMES = FALSE)
  # An entry whose name some file systems cannot hold refuses the archive only
  # when something needs it; unneeded, it is listed as unused (and is never
  # probed as a library).
  problem <- ins$problem[keep]
  forced <- (claimed | declared) & !is.na(problem)
  if (any(forced)) dta_zip_refuse_unsafe(cand$path, ins$entry[keep][forced], problem[forced], hostile = FALSE)
  need <- claimed | declared | (kinds %in% "xpt" & xpt_wanted & is.na(problem))
  # The per-member cap applies to every entry something will bind.
  big <- claimed & ins$size[keep] > limits$max_member_bytes
  if (any(big)) {
    if (on_error == "abort") {
      i <- keep[which(big)[[1]]]
      dta_container_too_large(cand$path, "DTAtools.container_max_member_bytes", ins$size[[i]], limits$max_member_bytes)
    }
    for (j in which(big)) {
      i <- keep[[j]]
      dta_delivery_row(st, mk(i, NA_character_), files, "refused",
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
    length(dta_delivery_claims_cached(st, slots, mk(r, ""), "xpt")$claims) > 0
  }, logical(1))
  big <- want & size > limits$max_member_bytes
  if (any(big)) {
    if (on_error == "abort") {
      r <- which(big)[[1]]
      dta_container_too_large(cand$path, "DTAtools.container_max_member_bytes", size[[r]], limits$max_member_bytes)
    }
    for (r in which(big)) {
      dta_delivery_row(st, mk(r, NA_character_), files, "refused",
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

# Two bindings of the same table (or file key) in one dataset.
dta_delivery_resolve_duplicates <- function(st, on_error, files) {
  bind <- st$bind
  if (!length(bind)) {
    return(bind)
  }
  key <- vapply(bind, function(b) paste(b$dataset, b$item, sep = "\r"), "")
  clash <- key %in% key[duplicated(key)]
  if (!any(clash)) {
    return(bind)
  }
  label <- function(b) {
    if (length(b$cand$chain)) paste0(paste(b$cand$chain, collapse = " > "), ": ", b$cand$member) else basename(b$cand$path)
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
    dta_delivery_row(st, b$cand, files, "refused", reason = sprintf(
      "is one of several files of the delivery that would be loaded as '%s' into dataset '%s'", b$item, b$dataset
    ))
  }
  bind[!clash]
}

# File entries that declare a container this call opened but received nothing.
dta_delivery_missing <- function(slots, opened, loaded) {
  out <- character()
  for (s in slots) {
    if (is.null(s$container) || !length(opened)) next
    if (!dta_matches_container(s$h, opened)) next
    got <- any(loaded$dataset == s$dataset & loaded$handler == s$handler)
    if (!got) out <- c(out, if (s$literal) s$h@filename else (s$h@pattern_description %||% s$h@filename))
  }
  unique(out)
}
