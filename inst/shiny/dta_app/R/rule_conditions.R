# =============================================================================
# Rule editor: condition clauses <-> form rows.
#
# The IF / THEN builder and the grouped-condition builder show a clause as rows
# of Column / Operator / Value. These are the pure functions between the two
# shapes; app.R only gathers the rows from the inputs and calls them.
# =============================================================================

# A named-list clause {COL: {op: val}} as editable rows, each list(col, op, val).
# "min"/"max" keys (single or combined) map to the UI "min_max" operator with a
# "min, max" value.
condition_to_form_rows <- function(cond) {
  if (is.null(cond) || length(cond) == 0) {
    return(list())
  }
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

# The text of one Value box as the R value the operator takes.
parse_form_value <- function(op, txt) {
  txt <- txt %||% ""
  if (identical(op, "empty")) {
    v <- tolower(trimws(txt))
    return(!(v %in% c("false", "no", "0", "f", "n"))) # default TRUE
  }
  if (identical(op, "pattern")) {
    return(as.character(txt))
  }
  if (op %in% c("in", "not_in")) {
    parts <- trimws(strsplit(txt, ",")[[1]])
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0) {
      return(character(0))
    }
    nums <- suppressWarnings(as.numeric(parts))
    if (all(!is.na(nums))) {
      return(nums)
    }
    return(parts)
  }
  if (!nzchar(trimws(txt))) {
    return("")
  }
  num <- suppressWarnings(as.numeric(txt))
  if (!is.na(num)) {
    return(num)
  }
  txt
}

# The positional "min, max" text of a between row as list(min =, max =); a bound
# that is blank or not a number is left out (no nzchar filter on the split, so
# ", 9" means max-only).
parse_min_max_text <- function(txt) {
  parts <- trimws(strsplit(txt %||% "", ",", fixed = TRUE)[[1]])
  mn <- if (length(parts) >= 1 && nzchar(parts[1])) suppressWarnings(as.numeric(parts[1])) else NA_real_
  mx <- if (length(parts) >= 2 && nzchar(parts[2])) suppressWarnings(as.numeric(parts[2])) else NA_real_
  out <- list()
  if (!is.na(mn)) out$min <- mn
  if (!is.na(mx)) out$max <- mx
  out
}

# Form rows (each list(col, op, val)) as a named-list clause. A row without a
# column is empty and skipped. `families` is the declared type family per column
# id; the rows are not read against it yet.
form_rows_to_condition <- function(rows, families = character(0)) {
  out <- list()
  for (row in rows) {
    col <- trimws(row$col %||% "")
    if (!nzchar(col)) next
    op <- row$op %||% "equals"
    inner <- list()
    if (identical(op, "min_max")) {
      inner <- parse_min_max_text(row$val)
      if (length(inner) == 0) next
    } else {
      inner[[op]] <- parse_form_value(op, row$val)
    }
    out[[col]] <- inner
  }
  out
}
