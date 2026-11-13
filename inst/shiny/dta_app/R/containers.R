# -----------------------------------------------------------------------------
# Containers: pure, server-free helpers for the container manager, the Files
# editor's "Delivered inside" select and the file-name conflict guard.
#
# A document's `containers` section holds DTAContainer objects (id, filename,
# description, default); a file entry's `container` field holds a REFERENCE:
# `none`, a container file name (anything with a dot; a file name no explicit
# container carries declares an IMPLIED container) or a container id.
#
# Everything here reads the document through the exported DTAtools functions
# (container_assignments(), file_name_conflicts(), DTAContainer()) and writes it
# with ONE S7::set_props(dta, datasets = , containers = ) so the document is
# validated once, as a whole. dta_set_handler() is never used: it rebuilds whole
# handlers from form defaults, which would erase settings the editor never shows.
#
# A "key" names a container: its id when it is explicit, its file name when it
# is implied (the `container` column of container_assignments()).
# -----------------------------------------------------------------------------

# The document's explicit containers (named list by id); an object restored from
# an older version has no such property.
.ctr_list <- function(dta) {
  cs <- tryCatch(dta@containers, error = function(e) NULL)
  if (is.null(cs)) list() else cs
}

# "3 files" / "1 file".
.ctr_count <- function(n, one, many) paste(n, if (n == 1) one else many)

# TRUE for a reference that is a container file name (it contains a dot).
.ctr_is_filename <- function(ref) grepl(".", ref, fixed = TRUE)

# A container id derived from a file name: ".gz", then ".zip"/".xpt" stripped
# (any case), each run outside [A-Za-z0-9] -> "_", trimmed, "container" when
# nothing is left, "container_" before a leading digit, "none" -> "container_none",
# then "_2", "_3", ... until it is not in `taken`.
dta_container_id_from_filename <- function(filename, taken = character(0)) {
  x <- as.character(filename)[1]
  x <- sub("\\.gz$", "", x, ignore.case = TRUE)
  x <- sub("\\.(zip|xpt)$", "", x, ignore.case = TRUE)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) x <- "container"
  if (grepl("^[0-9]", x)) x <- paste0("container_", x)
  if (tolower(x) == "none") x <- "container_none"
  id <- x
  n <- 1L
  while (id %in% taken) {
    n <- n + 1L
    id <- paste0(x, "_", n)
  }
  id
}

# The options of every "Delivered inside" select, as a named character vector
# (label = name, value = what is stored in the entry). With a default container:
# "<file> (with all other files)" = "", the other explicit containers by id, the
# implied ones by file name, "No container required" = "none". Without one:
# "Loose or in any container" = "", then the same. A stored value that is none
# of these (`current`) is appended as the last option: a select must never fall
# back to "" and erase a reference.
dta_container_choices_base <- function(dta, ca = NULL) {
  cs <- .ctr_list(dta)
  ca <- ca %||% tryCatch(DTAtools::container_assignments(dta), error = function(e) NULL)
  dflt <- Filter(function(ct) isTRUE(ct@default), cs)
  dflt <- if (length(dflt)) dflt[[1]] else NULL

  out <- if (is.null(dflt)) {
    c("Loose or in any container" = "")
  } else {
    stats::setNames("", paste0(dflt@filename, " (with all other files)"))
  }
  for (ct in cs) {
    if (!identical(ct, dflt)) out <- c(out, stats::setNames(ct@id, ct@filename))
  }
  implied <- if (is.null(ca)) character(0) else unique(ca$filename[ca$implied %in% TRUE])
  for (f in implied) out <- c(out, stats::setNames(f, f))
  if (!is.null(dflt)) out <- c(out, "No container required" = "none")
  out
}

# A reference that is the file name of an explicit container is that container's
# id; anything else is returned as it is. A select showing `ref` must select
# this value.
dta_container_normalise_ref <- function(dta, ref) {
  ref <- as.character(ref %||% "")[1]
  for (ct in .ctr_list(dta)) if (identical(ct@filename, ref)) {
    return(ct@id)
  }
  ref
}

# A container file name (a glob: `*` and `?`, letter case must match) as an
# anchored regular expression.
dv_glob_rx <- function(glob) {
  x <- gsub("([.+^${}()|\\[\\]\\\\])", "\\\\\\1", glob, perl = TRUE)
  x <- gsub("*", ".*", x, fixed = TRUE)
  x <- gsub("?", ".", x, fixed = TRUE)
  paste0("^", x, "$")
}

# Which of `names` the container file name `glob` takes: as the package matches,
# also with a compression suffix (`.gz`) removed.
dv_glob_hits <- function(glob, names) {
  rx <- dv_glob_rx(glob)
  grepl(rx, names) | grepl(rx, sub("\\.gz$", "", names, ignore.case = TRUE))
}

# `ca` (container_assignments()) and `base` (dta_container_choices_base()) let a
# caller that renders many selects compute the expensive part once.
dta_container_choices <- function(dta, current = NULL, ca = NULL, base = NULL) {
  out <- base %||% dta_container_choices_base(dta, ca)
  current <- as.character(current %||% "")[1]
  if (is.na(current) || !nzchar(current)) {
    return(out)
  }
  current <- dta_container_normalise_ref(dta, current)
  if (current %in% out) {
    return(out)
  }
  cs <- .ctr_list(dta)
  # The default container named on the entry itself gets its plain file name:
  # "(with all other files)" already labels the empty choice.
  label <- if (current %in% names(cs)) {
    cs[[current]]@filename
  } else if (identical(current, "none")) {
    "No container required"
  } else {
    current
  }
  c(out, stats::setNames(current, label))
}

# The name a message shows for dataset `id`: its label, else the id itself.
.ctr_ds_label <- function(id, dta = NULL) {
  l <- tryCatch(dta_dataset_label(dta_get_dataset(dta, id)), error = function(e) "")
  if (length(l) == 1 && !is.na(l) && nzchar(l)) l else id
}

# One row per container in effect (explicit, then implied): a list with key,
# filename, description, default, implied, n_files, n_datasets and the summary
# text of the list ("12 files in 10 datasets", "All other files", ...).
dta_container_rows <- function(dta) {
  cs <- .ctr_list(dta)
  ca <- tryCatch(DTAtools::container_assignments(dta), error = function(e) NULL)
  if (is.null(ca)) {
    ca <- data.frame(
      dataset = character(0), handler = integer(0), expected = character(0),
      reference = character(0), container = character(0), filename = character(0),
      default = logical(0), implied = logical(0), stringsAsFactors = FALSE
    )
  }
  row <- function(key, filename, description, default, implied) {
    mine <- ca[!is.na(ca$container) & ca$container == key, , drop = FALSE]
    n_files <- nrow(mine)
    n_ds <- length(unique(mine$dataset))
    summary <- if (isTRUE(default)) {
      exc <- ca[!is.na(ca$reference) & ca$reference == "none", , drop = FALSE]
      paste0(
        "All other files",
        if (nrow(exc)) {
          paste0(
            "; except: ",
            paste0(exc$expected, " (", vapply(exc$dataset, .ctr_ds_label, character(1), dta = dta), ")", collapse = ", ")
          )
        }
      )
    } else if (n_files == 0) {
      "No files assigned"
    } else {
      paste(.ctr_count(n_files, "file", "files"), "in", .ctr_count(n_ds, "dataset", "datasets"))
    }
    list(
      key = key, filename = filename, description = description, default = isTRUE(default),
      implied = implied, n_files = n_files, n_datasets = n_ds, summary = summary
    )
  }
  rows <- lapply(unname(cs), function(ct) row(ct@id, ct@filename, ct@description, ct@default, FALSE))
  for (f in unique(ca$filename[ca$implied %in% TRUE])) {
    rows[[length(rows) + 1L]] <- row(f, f, NULL, FALSE, TRUE)
  }
  rows
}

# The Files editor's list: dta_handlers_overview(), except that an entry which
# opted out of the document's default container reads "No container required"
# where its column would otherwise be blank.
dv_files_overview <- function(dta, ds) {
  ov <- dta_handlers_overview(dta, ds)
  if (nrow(ov) > 0 && any(vapply(dta_container_rows(dta), function(r) isTRUE(r$default), logical(1)))) {
    ca <- DTAtools::container_assignments(dta)
    ca <- ca[ca$dataset == ds, , drop = FALSE]
    ov$container[!is.na(ca$reference) & ca$reference == "none"] <- "No container required"
  }
  ov
}

# ---- Mutations --------------------------------------------------------------

# A copy of the document's datasets with every file entry's reference replaced
# by fn(reference, dataset id, entry index) (NULL = no reference). Only a changed
# entry is rebuilt, by setting `container` on a copy of the handler; a dataset
# with no change is returned as it is.
.ctr_rewrite_refs <- function(dta, fn) {
  dss <- DTAtools::datasets(dta)
  for (id in names(dss)) {
    ds <- dss[[id]]
    hs <- dta_handlers(ds)
    changed <- FALSE
    for (i in seq_along(hs)) {
      old <- tryCatch(hs[[i]]@container, error = function(e) NULL)
      new <- fn(old, id, i)
      if (!identical(old, new)) {
        h <- hs[[i]]
        h@container <- new
        hs[[i]] <- h
        changed <- TRUE
      }
    }
    if (changed) {
      ds@files <- hs
      dss[[id]] <- ds
    }
  }
  dss
}

# Apply datasets and containers in ONE validation, as a dta_try() result.
.ctr_apply <- function(dta, datasets, containers) {
  dta_try(S7::set_props(dta, datasets = datasets, containers = containers))
}

# Save a container: `key` NULL/"" adds one, an explicit container's id edits it,
# an implied container's file name materialises it (with an id derived from the
# new file name). Returns dta_try() plus `note` (NULL, or the sentence naming the
# container that lost the default) and `key` (the container's id).
dta_container_save <- function(dta, key = NULL, filename, description, default) {
  cs <- .ctr_list(dta)
  key <- as.character(key %||% "")[1]
  filename <- trimws(as.character(filename %||% "")[1])
  description <- trimws(as.character(description %||% "")[1])
  description <- if (is.na(description) || !nzchar(description)) NULL else description
  default <- isTRUE(default)

  editing <- nzchar(key) && key %in% names(cs)
  old_fn <- if (editing) cs[[key]]@filename else if (nzchar(key)) key else NULL
  id <- if (editing) key else dta_container_id_from_filename(filename, names(cs))

  built <- dta_try(DTAtools::DTAContainer(id, filename, description, default))
  if (!isTRUE(built$ok)) {
    # The package prefixes the derived id; the user never chose one.
    built$error <- sub("^Container '[^']*': ", "", built$error)
    return(built)
  }
  clash <- Filter(function(ct) identical(ct@filename, filename) && !identical(ct@id, id), cs)
  if (length(clash)) {
    return(list(
      ok = FALSE, value = NULL, key = NULL, note = NULL,
      error = paste0("Another container already has the file name '", filename, "'.")
    ))
  }

  # Entries naming the container by its old or new file name now name its id: a
  # later rename of the file must not split them off.
  names_to_id <- unique(c(old_fn, filename))
  datasets <- .ctr_rewrite_refs(dta, function(ref, ds, i) {
    if (!is.null(ref) && ref %in% names_to_id) id else ref
  })

  note <- NULL
  if (default) {
    lost <- Filter(function(ct) isTRUE(ct@default) && !identical(ct@id, id), cs)
    for (ct in lost) {
      ct@default <- FALSE
      cs[[ct@id]] <- ct
    }
    if (length(lost)) {
      note <- paste0(
        "'", filename, "' now takes all other files; '",
        paste(vapply(lost, function(ct) ct@filename, character(1)), collapse = "', '"),
        "' no longer does."
      )
    }
  }
  cs[[id]] <- built$value

  res <- .ctr_apply(dta, datasets, cs)
  res$note <- if (isTRUE(res$ok)) note else NULL
  res$key <- if (isTRUE(res$ok)) id else NULL
  res
}

# Set the entries' references. `assignments`: data frame with `dataset`,
# `handler` (entry index) and `value` ("" = no own reference, NA = keep the stored one, "none", a
# container id or file name). A file name an explicit container carries becomes
# its id; any other value is stored as it is (an implied container stays
# implied, a value the select did not know stays untouched). Entries not listed
# keep their reference.
dta_container_assign <- function(dta, assignments) {
  cs <- .ctr_list(dta)
  ids <- vapply(cs, function(ct) ct@id, character(1))
  fns <- vapply(cs, function(ct) ct@filename, character(1))
  a <- data.frame(
    dataset = as.character(assignments$dataset),
    handler = as.integer(assignments$handler),
    value = as.character(assignments$value),
    stringsAsFactors = FALSE
  )
  have <- DTAtools::container_assignments(dta)
  unknown <- a[!paste(a$dataset, a$handler) %in% paste(have$dataset, have$handler), , drop = FALSE]
  if (nrow(unknown)) {
    return(list(
      ok = FALSE, value = NULL,
      error = paste0("Dataset '", unknown$dataset[[1]], "' has no file entry ", unknown$handler[[1]], ".")
    ))
  }
  want <- stats::setNames(a$value, paste(a$dataset, a$handler))
  datasets <- .ctr_rewrite_refs(dta, function(ref, ds, i) {
    k <- paste(ds, i)
    if (!k %in% names(want)) {
      return(ref)
    }
    v <- want[[k]]
    if (is.na(v)) {
      ref
    } else if (!nzchar(v)) {
      NULL
    } else if (v %in% fns) {
      ids[[match(v, fns)]]
    } else {
      v
    }
  })
  .ctr_apply(dta, datasets, cs)
}

# Remove a container (an explicit one by id, an implied one by file name) and
# clear every reference to it, by id and by file name.
dta_container_remove <- function(dta, key) {
  cs <- .ctr_list(dta)
  key <- as.character(key %||% "")[1]
  if (key %in% names(cs)) {
    gone <- c(key, cs[[key]]@filename)
    cs[[key]] <- NULL
  } else if (nzchar(key) && .ctr_is_filename(key)) {
    gone <- key
  } else {
    return(list(ok = FALSE, value = NULL, error = paste0("There is no container '", key, "'.")))
  }
  datasets <- .ctr_rewrite_refs(dta, function(ref, ds, i) if (!is.null(ref) && ref %in% gone) NULL else ref)
  .ctr_apply(dta, datasets, cs)
}

# ---- Reading ----------------------------------------------------------------

# The file name (glob) the entry `hi` of dataset `ds` must arrive inside, or ""
# (own reference, else the default container). Pass `ca` (container_assignments())
# when asking for many entries.
dta_effective_glob <- function(dta, ds, hi, ca = NULL) {
  ca <- ca %||% tryCatch(DTAtools::container_assignments(dta), error = function(e) NULL)
  if (is.null(ca)) {
    return("")
  }
  r <- ca[ca$dataset == ds & ca$handler == as.integer(hi), , drop = FALSE]
  if (nrow(r) == 0 || is.na(r$filename[[1]])) "" else r$filename[[1]]
}

# ---- File-name conflicts (by content, never by entry index) ------------------

# `x`: a DTA, or a data frame as file_name_conflicts() returns it.
.ctr_conflict_frame <- function(x) {
  if (is.data.frame(x)) x else DTAtools::file_name_conflicts(x)
}

.ctr_conflict_keys <- function(f) {
  if (nrow(f) == 0) {
    return(character(0))
  }
  vapply(seq_len(nrow(f)), function(i) {
    paste(
      sort(c(
        paste(f$dataset_a[[i]], f$expected_a[[i]]),
        paste(f$dataset_b[[i]], f$expected_b[[i]])
      ), method = "radix"),
      collapse = " <> "
    )
  }, character(1))
}

# One key per conflict. Entry indices shift when an entry is inserted or removed;
# the datasets and what the entries expect do not.
dta_conflict_keys <- function(x) .ctr_conflict_keys(.ctr_conflict_frame(x))

# The conflicts of `after` that `before` did not have (rows of `after`).
dta_new_conflicts <- function(before, after) {
  b <- .ctr_conflict_frame(before)
  a <- .ctr_conflict_frame(after)
  # Compared as counts: two entries with the same description make the same key
  # twice, and the extra one is a new conflict.
  ka <- .ctr_conflict_keys(a)
  tb <- table(.ctr_conflict_keys(b))
  seen <- stats::ave(seq_along(ka), ka, FUN = seq_along)
  had <- ifelse(ka %in% names(tb), as.integer(tb[ka]), 0L)
  a[seen > had, , drop = FALSE]
}

# The sentence that refuses an edit introducing the conflict(s) `rows`
# (the first one is named).
dta_conflict_refusal <- function(rows, dta = NULL) {
  paste0(
    "This would let datasets '", .ctr_ds_label(rows$dataset_a[[1]], dta), "' and '",
    .ctr_ds_label(rows$dataset_b[[1]], dta),
    "' both take a file named '", rows$example[[1]],
    "'. Make the expected names distinct (or put the two entries in containers that cannot have the same name)."
  )
}
