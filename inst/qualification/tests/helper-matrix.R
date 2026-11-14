# Helpers for the matrix-dataset cases (area MATRIX).
#
# Every fixture is written by the case that uses it, as bytes, so that the line
# ends, the BOM and the delimiter are exactly what the case says. The expected
# values live in the cases and are taken from the requirement text, never from
# a run.
#
# Every helper that creates a file takes `env` and hands it on, because
# qa_tempdir() defers the deletion of its directory to `env`; with the default
# it would delete the directory the moment the helper returned.

# Both ways a table can be held: whole in memory, and scanned in batches.
MX_STREAMS <- c("never", "always")

# Write `text` (character or raw) to a fresh temp file called `name`.
mx_path <- function(text, name = "m.tsv", env = parent.frame()) {
  p <- file.path(qa_tempdir(env), name)
  writeBin(if (is.raw(text)) text else charToRaw(text), p)
  p
}

# A matrix dataset as the YAML reader would produce it from the list a
# specification gives. `k` row-label columns (ID, DESC, EXTRA); every label
# after the first may be missing unless `required_labels`.
mx_ds <- function(corner = "blank", k = 1L, header = list(), cells = NULL,
                  rows = NULL, ext = "tsv", rules = NULL, files_extra = list(),
                  required_labels = FALSE, id = "m") {
  ids <- c("ID", "DESC", "EXTRA")
  labels <- lapply(seq_len(k), function(i) {
    list(id = ids[[i]], type = "SAS Char", nullable = if (required_labels) FALSE else i > 1L)
  })
  m <- list(
    header = c(list(corner = corner), header),
    cells = cells %||% list(type = "SAS Int", nullable = FALSE)
  )
  if (!is.null(rows)) m$rows <- rows
  spec <- list(
    id = id, type = "matrix",
    files = c(list(filename = paste0(id, ".", ext), type = ext), files_extra),
    row_labels = labels, matrix = m
  )
  if (!is.null(rules)) spec$rules <- rules
  dta_dataset_from_list(spec)
}

# Load and check one file. The table name is taken from the file, so it is
# read back rather than assumed.
mx_checked <- function(ds, path, stream, ...) {
  loaded <- load_file(ds, file = path, stream = stream)
  check(loaded, quiet = TRUE, persist = FALSE, ...)
}

# The reported messages reduced to what a requirement is about: which axis,
# which row, which column, which constraint. Message text is prose and may be
# reworded without the verdict changing.
mx_found <- function(d) {
  msgs <- as.data.frame(messages(d))
  out <- data.frame(
    source = as.character(msgs$source),
    row = as.integer(msgs$row),
    column = as.character(msgs$column),
    keyword = as.character(msgs$keyword),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$source, out$row, out$column, out$keyword, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# The same shape, for an expected value written down in the case.
mx_expect <- function(source = character(0), row = integer(0),
                      column = character(0), keyword = character(0)) {
  out <- data.frame(
    source = as.character(source), row = as.integer(row),
    column = as.character(column), keyword = as.character(keyword),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$source, out$row, out$column, out$keyword, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Run `fun(checked, stream)` once per way of holding the table.
mx_each <- function(ds, path, fun, ...) {
  for (stream in MX_STREAMS) {
    fun(mx_checked(ds, path, stream, ...), stream)
  }
  invisible(NULL)
}

# The error a load raises, or NULL when it was accepted.
mx_load_error <- function(ds, path, stream = "never") {
  tryCatch(
    {
      load_file(ds, file = path, stream = stream)
      NULL
    },
    error = function(e) e
  )
}

# The error an expression raises, or NULL.
mx_error <- function(expr) {
  tryCatch(
    {
      force(expr)
      NULL
    },
    error = function(e) e
  )
}

# Does a condition carry `class`?
mx_is <- function(cnd, class) !is.null(cnd) && inherits(cnd, class)

# The table of the (only) bound table as a data frame, named as the reader
# named its columns.
mx_table <- function(d) as.data.frame(tables(d)[[1]])

# The per-check report of the (only) table.
mx_checks <- function(d) {
  validation_errors(d, names(tables(d))[[1]])$columnspec_checks
}

# The column names of the one bound table, however it is held.
mh_names <- function(d) names(tables(d)[[1]])

# Checked and clean: valid, and nothing reported on any axis.
mh_clean <- function(d) isTRUE(validation_status(d)$ok) && nrow(messages(d)) == 0L

# The list a specification gives for a matrix dataset; named arguments replace
# a key, and NULL removes it.
mx_body <- function(...) {
  body <- list(
    id = "m", type = "matrix", files = list(filename = "m.tsv", type = "tsv"),
    row_labels = list(list(id = "ID", type = "SAS Char", nullable = FALSE)),
    matrix = list(header = list(corner = "blank"), cells = list(type = "SAS Int", nullable = FALSE))
  )
  over <- list(...)
  for (nm in names(over)) body[nm] <- list(over[[nm]])
  body
}
