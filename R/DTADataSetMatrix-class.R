#' @title DTADataSetMatrix Class
#' @description
#' A dataset whose file is a matrix: a CSV or TSV whose header row labels the
#' data columns, whose leading column(s) identify and describe each row, and
#' whose remaining cells hold one homogeneous measurement (counts, expression,
#' genotype calls). The data columns are not listed: they are described by a
#' rule (the \code{matrix} settings), so a table may have tens of thousands of
#' them.
#'
#' A \code{DTADataSetMatrix} is a \code{\link{DTADataSetTabular}}: it is loaded
#' with \code{\link{load_file}}, validated with \code{\link{check}} and reported
#' on like any tabular dataset. The columns of \code{specs} are the
#' \emph{row-label} columns, matched to the file's leading columns by position;
#' the first one is the default row key. What the header, the dimensions and
#' the cells must look like is the \code{matrix} setting.
#'
#' In a specification the row-label columns are written under
#' \code{row_labels:} (not \code{columns:}) and the settings under
#' \code{matrix:}, with \code{type: matrix}.
#'
#' @section Loading:
#' \code{load_file()} reads the header first and refuses a file whose first
#' data row does not have as many fields as the header implies (a header that
#' lacks the field above the row names, a preamble, another delimiter). The raw
#' header is kept in \code{@heads}; every \code{check()} recomputes the data
#' column names from it under the \emph{current} specification. A table that no
#' longer matches (the specification changed, or a lazily held file was edited
#' in place) is reported as that table's failure, with the instruction to load
#' it again; the other tables and datasets are still validated.
#'
#' @section Abort classes:
#' The constructor and its validator abort with a class naming the problem:
#' \code{dta_matrix_tables_refused} (a non-empty \code{tables}; use
#' \code{load_file()}), \code{dta_matrix_invalid_row_labels} (no row label),
#' \code{dta_matrix_invalid_row_key} (a key that is not a row-label id),
#' \code{dta_matrix_invalid_key_nullable} (a key column declared
#' \code{nullable: Yes}), \code{dta_matrix_invalid_corner} (a corner list whose
#' length is not the number of row labels), \code{dta_matrix_invalid_file} (a
#' file handler that is not CSV or TSV with a header),
#' \code{dta_matrix_invalid_tables} (a table that is not an Arrow Table or
#' Dataset) and \code{dta_matrix_invalid_rule} (a rule id starting with
#' \code{.matrix:}). Loading aborts with \code{dta_matrix_header_gate} (and the
#' subclasses \code{dta_matrix_header_fewer}, \code{dta_matrix_header_more},
#' \code{dta_matrix_header_ragged}, \code{dta_matrix_header_empty}),
#' \code{dta_matrix_preamble} or \code{dta_matrix_delimiter}.
#'
#' @param id Character. Identifier of the dataset.
#' @param specs A \code{DTAColumnSpecCollection} of the row-label columns (at
#'   least one). Rules may refer to these columns only.
#' @param matrix A \code{DTAMatrixSpec}, or the YAML-shaped list of the
#'   \code{matrix:} block (\code{header}, \code{rows}, \code{cells}).
#' @param files A list of \code{DTAFileCSV} or \code{DTAFileTSV} handlers with
#'   \code{has_header = TRUE}.
#' @param name Character or NULL. Human-readable name.
#' @param description Character or NULL. Long free-text description.
#' @param template_source,template_version,template_date Character or NULL.
#'   Where the specification came from.
#' @param tables Must be empty: a matrix is bound with \code{load_file()},
#'   which records the header the data columns are named from.
#' @return An object of class \code{DTADataSetMatrix}.
#' @examples
#' path <- file.path(tempdir(), "counts.tsv")
#' writeLines(
#'   c("\tS1\tS2\tS3", "geneA\t10\t0\t7", "geneB\t3\t25\t1"),
#'   path
#' )
#'
#' ds <- DTADataSetMatrix(
#'   id = "counts",
#'   specs = DTAColumnSpecCollection(
#'     columns = list(GENE = DTAColumnSpec(id = "GENE", type = "SAS Char", nullable = FALSE))
#'   ),
#'   matrix = list(
#'     header = list(corner = "blank", pattern = "^S[0-9]+$", count = list(min = 1, max = 384)),
#'     cells = list(type = "SAS Int", nullable = FALSE, range = list(min = 0))
#'   ),
#'   files = list(DTAFileTSV(filename = "counts.tsv"))
#' )
#' ds <- load_file(ds, file = path)
#' ds <- check(ds, quiet = TRUE, persist = FALSE)
#' validation_status(ds)
#' @include DTADataSetTabular-class.R DTADataSetFile-class.R DTAMatrixSpec-class.R
#' @export
DTADataSetMatrix <- S7::new_class(
  "DTADataSetMatrix",
  parent = DTADataSetTabular,
  constructor = function(
    id,
    specs,
    matrix,
    files = list(),
    name = NULL,
    description = NULL,
    template_source = NULL,
    template_version = NULL,
    template_date = NULL,
    tables = list()
  ) {
    if (missing(id)) dta_abort_missing_dataset_id()
    if (length(tables) > 0L) {
      cli_abort(
        c(
          "A {.cls DTADataSetMatrix} cannot be built with {.arg tables}.",
          "i" = "Bind a file with {.fn load_file}: it records the header the data columns are named from."
        ),
        class = "dta_matrix_tables_refused"
      )
    }
    if (missing(matrix) || is.null(matrix)) {
      cli_abort(
        "A {.cls DTADataSetMatrix} needs its {.arg matrix} settings.",
        class = "dta_matrix_field_missing"
      )
    }
    if (is.list(matrix)) {
      matrix <- dta_matrix_spec_from_list(matrix)
    }
    if (is.null(matrix@row_key) && length(specs@columns) > 0L) {
      matrix@row_key <- specs@columns[[1L]]@id
    }

    new_object(
      DTADataSetTabular(
        id = id, specs = specs, files = files, name = name,
        description = description, template_source = template_source,
        template_version = template_version, template_date = template_date
      ),
      type = "matrix",
      matrix = matrix,
      heads = list()
    )
  },
  properties = list(
    matrix = DTAMatrixSpec,
    # Raw header of every bound table, by table name: list(fields, handler).
    # Set by load_file() only; check() recomputes the data column names from it.
    heads = S7::new_property(S7::class_list, default = list())
  ),
  validator = function(self) {
    ids <- dta_matrix_label_ids(self)
    k <- length(ids)
    if (k < 1L) {
      cli_abort(
        c(
          "A matrix dataset needs at least one row-label column.",
          "i" = "Declare them under {.field row_labels}."
        ),
        class = "dta_matrix_invalid_row_labels"
      )
    }
    key <- dta_matrix_row_key(self)
    if (!all(key %in% ids)) {
      cli_abort(
        "{.field matrix.rows.key} names {.val {setdiff(key, ids)}}, which {?is/are} not a row-label id ({.val {ids}}).",
        class = "dta_matrix_invalid_row_key"
      )
    }
    nullable_keys <- key[vapply(key, function(id) isTRUE(self@specs@columns[[id]]@nullable), logical(1))]
    if (length(nullable_keys) > 0L) {
      cli_abort(
        "The row key column{?s} {.val {nullable_keys}} cannot be {.code nullable: Yes}: a row needs an identity.",
        class = "dta_matrix_invalid_key_nullable"
      )
    }
    if (self@matrix@corner_mode == "list" && length(self@matrix@corner) != k) {
      cli_abort(
        "{.field matrix.header.corner} lists {length(self@matrix@corner)} entr{?y/ies} but the dataset has {k} row-label column{?s}.",
        class = "dta_matrix_invalid_corner"
      )
    }
    for (h in self@files) {
      if (!(inherits(h, "DTAtools::DTAFileCSV") || inherits(h, "DTAtools::DTAFileTSV")) ||
        !isTRUE(h@has_header)) {
        cli_abort(
          c(
            "A matrix dataset reads CSV or TSV files with a header line.",
            "x" = "{.field {paste(h@filename, collapse = ', ')}} is not one."
          ),
          class = "dta_matrix_invalid_file"
        )
      }
    }
    for (tbl in self@tables) {
      if (!(inherits(tbl, "Table") || inherits(tbl, "Dataset"))) {
        cli_abort(
          "The tables of a matrix dataset must be an Arrow {.cls Table} or {.cls Dataset}.",
          class = "dta_matrix_invalid_tables"
        )
      }
    }
    rule_ids <- vapply(self@specs@rules, function(r) r@id, character(1))
    reserved <- rule_ids[startsWith(rule_ids, ".matrix:")]
    if (length(reserved) > 0L) {
      cli_abort(
        "Rule id{?s} {.val {reserved}} start{?s/} with {.val .matrix:}, which is reserved.",
        class = "dta_matrix_invalid_rule"
      )
    }
    NULL
  }
)

# ---- print --------------------------------------------------------------------

#' @name print
#' @export
method(print, DTADataSetMatrix) <- function(x, ...) {
  dta_print_tabular(x, "DTADataSetMatrix")
  m <- x@matrix
  corner <- if (m@corner_mode == "list") {
    paste(ifelse(is.na(m@corner), "null", m@corner), collapse = ", ")
  } else {
    m@corner_mode
  }
  key <- paste(dta_matrix_row_key(x), collapse = ", ")
  n_columns <- paste(m@n_columns, collapse = " to ")
  n_rows <- paste(m@n_rows, collapse = " to ")
  cli::cli_alert_info("Matrix:")
  cli::cli_alert("corner: {corner}; row key: {key}")
  cli::cli_alert("data columns: {n_columns}; rows: {n_rows}; cells: {dta_cell_type(m@cells)}")
  invisible(x)
}

# ---- row labels and corner ----------------------------------------------------

# The ids of the row-label columns, in file order.
dta_matrix_label_ids <- function(x) {
  unname(vapply(x@specs@columns, function(col) col@id, character(1)))
}

# The row key; the first row label when none is declared.
dta_matrix_row_key <- function(x) {
  x@matrix@row_key %||% dta_matrix_label_ids(x)[1L]
}

# Header fields above the row labels: k, or k - 1 when the corner's first entry
# is `null` (R's write.table writes no field above the row names).
dta_matrix_corner_n <- function(x) {
  k <- length(dta_matrix_label_ids(x))
  if (x@matrix@corner_mode == "list" && is.na(x@matrix@corner[[1L]])) k - 1L else k
}

# The header text each of the c corner fields must have, cleaned the way the
# header read is.
dta_matrix_corner_expected <- function(x) {
  m <- x@matrix
  expected <- switch(m@corner_mode,
    named = dta_matrix_label_ids(x),
    blank = rep("", length(dta_matrix_label_ids(x))),
    list = if (is.na(m@corner[[1L]])) m@corner[-1L] else m@corner
  )
  dta_clean_column_names(expected)
}

# The data labels of a header: everything after the corner.
dta_matrix_data_labels <- function(head_fields, x) {
  cn <- dta_matrix_corner_n(x)
  if (length(head_fields) > cn) head_fields[(cn + 1L):length(head_fields)] else character(0)
}

# ---- reading names ------------------------------------------------------------

# The names the table's columns are read under: the row-label ids, then each data
# label where it is usable (non-empty, and unique among ALL names), else
# "<column p>", p being the 1-based position of the column in the file's data
# rows. A label that collides with a generated name is replaced too, until the
# names are unique. Cell findings of a bad-label column therefore carry the same
# text its header finding names.
dta_matrix_reading_names <- function(head_fields, x) {
  ids <- dta_matrix_label_ids(x)
  labs <- dta_matrix_data_labels(head_fields, x)
  m <- length(labs)
  generated <- paste0("<column ", length(ids) + seq_len(m), ">")
  usable <- !is.na(labs) & nzchar(labs) & !(labs %in% ids) &
    !(duplicated(labs) | duplicated(labs, fromLast = TRUE))
  repeat {
    colliding <- usable & (labs %in% generated[!usable])
    if (!any(colliding)) break
    usable[colliding] <- FALSE
  }
  c(ids, ifelse(usable, labs, generated))
}

# ---- header findings ----------------------------------------------------------

#' @title Structural Findings of a Matrix Header
#' @description
#' Judges a header against the \code{matrix:} settings: the corner, every data
#' label, repeated labels and the number of data columns. The findings are in
#' the shape of the column-spec error frame; ordered by position in the header.
#' @param head_fields The header fields as read (cleaned).
#' @param x A \code{DTADataSetMatrix}.
#' @return A list: \code{findings} (data frame: row, column, keyword, message,
#'   columnspec, data) and \code{declared} (named integer: how many things each
#'   header keyword judged).
#' @keywords internal
dta_matrix_head_findings <- function(head_fields, x) {
  m <- x@matrix
  ids <- dta_matrix_label_ids(x)
  k <- length(ids)
  cn <- dta_matrix_corner_n(x)
  expected <- dta_matrix_corner_expected(x)
  labs <- dta_matrix_data_labels(head_fields, x)
  n_data <- length(labs)
  rnames <- dta_matrix_reading_names(head_fields, x)
  dnames <- rnames[-seq_len(k)]
  col_pos <- k + seq_len(n_data) # position in the data rows
  quoted <- function(v) paste0("\"", v, "\"")
  count <- function(n) format(n, big.mark = "", scientific = FALSE, trim = TRUE)

  # One column vector per field, one data frame at the end: a header of 20 000
  # failing labels must not cost a data frame (and an rbind) per finding.
  parts <- list()
  add <- function(position, column, keyword, message, columnspec, data) {
    if (length(position) == 0L) {
      return(invisible())
    }
    n <- length(position)
    parts[[length(parts) + 1L]] <<- list(
      position = as.numeric(position), column = rep_len(as.character(column), n),
      keyword = rep_len(keyword, n), message = rep_len(as.character(message), n),
      columnspec = rep_len(as.character(columnspec), n), data = rep_len(as.character(data), n)
    )
    invisible()
  }

  # corner ----------------------------------------------------------------------
  i <- seq_len(cn)
  found <- ifelse(i <= length(head_fields), head_fields[pmin(i, length(head_fields))], NA_character_)
  bad <- i[is.na(found) | found != expected[i]]
  if (length(bad) > 0L) {
    shown <- ifelse(is.na(found[bad]), "no field", quoted(found[bad]))
    add(
      bad, ids[k - cn + bad], "corner",
      sprintf("Header field %d is %s, expected %s.", bad, shown, quoted(expected[bad])),
      expected[bad], ifelse(is.na(found[bad]), "", found[bad])
    )
  }

  # data labels -------------------------------------------------------------------
  present <- which(!is.na(labs) & nzchar(labs))
  if (length(present) > 0L) {
    f <- dta_check_column_spec("<header>", as_json_schema(m@header), labs[present])
    if (!is.null(f) && nrow(f) > 0L) {
      j <- present[f$row]
      add(
        cn + j, dnames[j], "propertyNames",
        sprintf("Label %s of column %d %s", quoted(labs[j]), col_pos[j], f$message),
        f$columnspec, f$data
      )
    }
  }
  unlabelled <- setdiff(seq_len(n_data), present)
  add(
    cn + unlabelled, dnames[unlabelled], "propertyNames",
    sprintf("Column %d has no label.", col_pos[unlabelled]),
    rep_len("non-empty label", length(unlabelled)), rep_len("", length(unlabelled))
  )

  # repeated labels ---------------------------------------------------------------
  # Grouped once; `split()` on a factor with the levels given never sorts, so the
  # result does not depend on the locale.
  everything <- c(ids, labs)
  candidates <- which(!is.na(everything) & nzchar(everything))
  cand_labs <- everything[candidates]
  in_repeat <- cand_labs %in% cand_labs[duplicated(cand_labs)]
  if (any(in_repeat)) {
    rep_labs <- cand_labs[in_repeat]
    groups <- split(candidates[in_repeat], factor(rep_labs, levels = unique(rep_labs)))
    first_data <- vapply(groups, function(occ) min(occ[occ > k]), numeric(1))
    lab <- names(groups)
    # A data label equal to a row-label id is read under a generated name; the
    # finding sits on that data column, not on the (clean) row-label column.
    column <- ifelse(lab %in% ids, rnames[first_data], lab)
    message <- vapply(seq_along(groups), function(g) {
      occ <- groups[[g]]
      sprintf(
        "Header label %s appears %d times: %s.", quoted(lab[[g]]), length(occ),
        paste0("column ", occ, " (read as ", quoted(rnames[occ]), ")", collapse = ", ")
      )
    }, character(1))
    add(cn + first_data - k, column, "uniqueNames", message, "unique labels", lab)
  }

  # number of data columns --------------------------------------------------------
  bounds <- m@n_columns
  if (n_data < bounds[[1L]]) {
    add(
      Inf, NA_character_, "minProperties",
      sprintf("The header has %d data column(s); at least %s required.", n_data, count(bounds[[1L]])),
      count(bounds[[1L]]), as.character(n_data)
    )
  }
  if (n_data > bounds[[2L]]) {
    add(
      Inf, NA_character_, "maxProperties",
      sprintf("The header has %d data column(s); at most %s allowed.", n_data, count(bounds[[2L]])),
      count(bounds[[2L]]), as.character(n_data)
    )
  }

  col <- function(field, empty) {
    if (length(parts) == 0L) empty else unlist(lapply(parts, `[[`, field), use.names = FALSE)
  }
  position <- col("position", numeric(0))
  o <- order(position, seq_along(position), method = "radix")
  findings <- data.frame(
    row = rep(NA_integer_, length(o)),
    column = col("column", character(0))[o],
    keyword = col("keyword", character(0))[o],
    message = col("message", character(0))[o],
    columnspec = col("columnspec", character(0))[o],
    data = col("data", character(0))[o],
    stringsAsFactors = FALSE
  )

  list(
    findings = findings,
    declared = c(
      corner = as.integer(cn),
      propertyNames = as.integer(n_data),
      uniqueNames = as.integer(n_data),
      minProperties = 1L,
      maxProperties = as.integer(is.finite(bounds[[2L]])),
      minItems = as.integer(m@n_rows[[1L]] > 0),
      maxItems = as.integer(is.finite(m@n_rows[[2L]]))
    )
  )
}

# ---- gate ---------------------------------------------------------------------

# Refuses, before any data is read, a file whose head cannot be a matrix of this
# shape: a preamble, another delimiter, or a first data row whose field count is
# not what the header implies (h + k - c).
dta_matrix_gate <- function(head, x, path) {
  if (identical(head$hint, "preamble")) {
    cli_abort(
      c(
        "{.path {path}} starts with a {.code #} line.",
        "i" = "Preamble lines (featureCounts, GCT) are not supported: remove them from the file."
      ),
      class = "dta_matrix_preamble"
    )
  }
  if (identical(head$hint, "delimiter")) {
    cli_abort(
      c(
        "The header of {.path {path}} is one field that holds another delimiter.",
        "i" = "The file appears to be separated by something other than the declared separator."
      ),
      class = "dta_matrix_delimiter"
    )
  }
  if (head$n_header == 0L) {
    cli_abort(
      "{.path {path}} has no header line.",
      class = c("dta_matrix_header_empty", "dta_matrix_header_gate")
    )
  }
  k <- length(dta_matrix_label_ids(x))
  cn <- dta_matrix_corner_n(x)
  if (head$n_header < cn) {
    cli_abort(
      c(
        "The header of {.path {path}} has {head$n_header} field{?s}, but the corner alone needs {cn}.",
        "i" = "Declare fewer row labels, or check the separator."
      ),
      class = c("dta_matrix_header_short", "dta_matrix_header_gate")
    )
  }
  expected <- head$n_header + (k - cn)
  found <- head$n_first_row
  if (!is.na(found) && found != expected) {
    # More fields in the first data row than the header implies: the header
    # lacks the field above the row names ("fewer"). Fewer: the header has a
    # corner field the spec does not declare ("more").
    shape <- if (found == expected + 1L) {
      "fewer"
    } else if (found == expected - 1L) {
      "more"
    } else {
      "ragged"
    }
    hint <- switch(shape,
      fewer = "The header has one field fewer than the data rows. If it has no field above the row names (R {.fn write.table}), declare {.code corner: [null, ...]}.",
      more = "The header has a field above the row labels that your corner does not declare (drop the {.code null}).",
      ragged = "The first data row and the header have unrelated field counts: the file is ragged."
    )
    cli_abort(
      c(
        "The header of {.path {path}} has {head$n_header} field{?s}, so a data row should have {expected}, but the first one has {found}.",
        "i" = hint
      ),
      class = c(paste0("dta_matrix_header_", shape), "dta_matrix_header_gate"),
      expected = expected, found = found
    )
  }
  invisible(head)
}

# ---- effective specs ----------------------------------------------------------

# The generated rule that makes the row key unique.
dta_matrix_key_rule <- function(x) {
  DTARuleColUnique(id = ".matrix:row_key", columns = dta_matrix_row_key(x))
}

# The user's rules plus the generated row-key rule.
dta_matrix_rules <- function(x) {
  c(as.list(x@specs@rules), list(dta_matrix_key_rule(x)))
}

# The rules of a dataset as the reports look them up.
dta_dataset_rules <- function(x) {
  if (inherits(x, "DTAtools::DTADataSetMatrix")) dta_matrix_rules(x) else x@specs@rules
}

#' @title Effective Column Specs of a Matrix Table
#' @description
#' The row-label specs, one clone of the cell spec per data column (renamed to
#' the column's reading name) and the generated row-key rule. The clones are
#' never validated: their ids are free text (\code{S 1}, \code{a{b}}).
#' @param x A \code{DTADataSetMatrix}.
#' @param reading_names All the table's column names: the row labels, then the
#'   data columns.
#' @return A \code{DTAColumnSpecCollection}.
#' @keywords internal
dta_matrix_effective_specs <- function(x, reading_names) {
  k <- length(dta_matrix_label_ids(x))
  data_names <- reading_names[-seq_len(min(k, length(reading_names)))]
  cells <- lapply(data_names, function(nm) {
    clone <- x@matrix@cells
    S7::prop(clone, "id", check = FALSE) <- nm
    clone
  })
  names(cells) <- data_names
  specs <- x@specs
  S7::prop(specs, "columns", check = FALSE) <- c(x@specs@columns, cells)
  S7::prop(specs, "rules", check = FALSE) <- dta_matrix_rules(x)
  specs
}

# ---- hooks of check(DTADataSetTabular) ----------------------------------------

method(dta_table_specs, DTADataSetMatrix) <- function(x, table, ...) {
  dta_matrix_effective_specs(x, dta_table_column_names(table))
}

method(dta_specs_fingerprint, DTADataSetMatrix) <- function(x, ...) {
  list(as.list(x@specs), as.list(x@matrix))
}

method(dta_table_structure, DTADataSetMatrix) <- function(x, name, table, ...) {
  fail <- function(why) {
    list(failure = sprintf("Table '%s' %s", name, why), structure = NULL)
  }
  head <- x@heads[[name]]
  if (is.null(head)) {
    return(fail("has no recorded header; load it again with load_file()."))
  }
  if (!identical(dta_matrix_reading_names(head$fields, x), dta_table_column_names(table))) {
    return(fail("no longer matches its recorded header: the specification changed since load_file(); load it again."))
  }

  # A lazy table is a plan over a file that can change underneath it.
  if (inherits(table, "Dataset")) {
    sources <- dta_dataset_source_files(table)
    if (length(sources) > 0L && all(file.exists(sources))) {
      handler <- tryCatch(x@files[[head$handler]], error = function(e) NULL)
      now <- if (is.null(handler)) {
        NULL
      } else {
        tryCatch(dta_matrix_read_head(handler, sources[[1L]]), error = function(e) NULL)
      }
      if (is.null(now) || !identical(now$fields, head$fields)) {
        return(fail("has a header that differs from the one recorded: the file changed since load_file(); load it again."))
      }
    }
  }

  hf <- dta_matrix_head_findings(head$fields, x)
  list(
    failure = NULL,
    structure = list(
      findings = hf$findings,
      rows = x@matrix@n_rows,
      declared = hf$declared,
      matrix = TRUE
    )
  )
}

# ---- load_file ----------------------------------------------------------------

#' @usage load_file(x, ...)
#' @rdname load_file
#' @export
method(load_file, DTADataSetMatrix) <- function(
  x,
  file,
  handler_index = 1,
  name = dta_default_table_name(file),
  stream = getOption("DTAtools.stream", "auto")
) {
  # The guards of the tabular method, in its order: nothing is read, and no
  # property of a stale object is touched, before they have passed.
  if (dta_is_stale(x)) x <- dta_upgrade(x)
  handler_index <- dta_resolve_file_handler_index(handler_index, x@files)
  handler <- files(x, handler_index)
  file <- dta_assert_single_file_path(file, "load_file")
  if (!is.character(name) || length(name) != 1 || is.na(name) || !nzchar(name)) {
    cli::cli_abort(
      "{.fn load_file} requires {.arg name} to be a single non-missing, non-empty string."
    )
  }
  dta_assert_not_container(x, handler, file)

  head <- dta_matrix_read_head(handler, file)
  dta_matrix_gate(head, x, file)
  reading_names <- dta_matrix_reading_names(head$fields, x)

  # A lazy table is coerced per batch by check(), with the specs it has then.
  lazy <- dta_load_is_lazy(stream, file, handler)
  coerce_specs <- if (lazy) x@specs else dta_matrix_effective_specs(x, reading_names)

  x <- dta_load_tabular(
    x, file, handler, name, stream,
    column_names = reading_names, coerce_specs = coerce_specs, lazy = lazy
  )
  # Overwritten when the name is replaced: the head describes the table bound now.
  x@heads[[name]] <- list(fields = head$fields, handler = handler_index)
  x
}
