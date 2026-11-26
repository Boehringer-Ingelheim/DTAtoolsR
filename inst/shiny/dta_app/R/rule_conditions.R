# =============================================================================
# Rule editor: condition clauses <-> form rows.
#
# The IF / THEN builder and the grouped-condition builder show a clause as rows
# of Column / Operator / Value. These are the pure functions between the two
# shapes; app.R only gathers the rows from the inputs and calls them.
#
# The principle: opening a rule and saving it without a change keeps what it
# means. Three rules follow from it.
#   * One row per (column, operator). A column with several operators is several
#     rows, merged again on Save; an operator twice in one column is an error.
#   * An untouched row keeps the value it was opened with, as the very R value
#     (an integer stays an integer, "007" stays text). Only a row the user
#     changed is read from its text, and it is read by the declared type of its
#     column, never by whether the text looks like a number.
#   * Nothing is dropped silently. What the editor cannot show, or a value it
#     cannot read, is an error that says so.
#
# Errors carry the class dta_form_error and one of dta_form_unsupported,
# dta_form_value_error, dta_form_operator_twice.
# =============================================================================

# ---- operators ----------------------------------------------------------------

# Every operator key the engine accepts (R/evaluateRules.R dta_condition_operators())
# and the form operator that shows it. The engine's aliases are written as their
# normal form: range as min/max, equal as equals, not_equal as not_equals. The
# values must all be options of dta_condition_operators() below in utils_dta.R
# (the editor's own list): a <select> with no option for its `selected` value
# reports its first option, so an operator the list lacks would silently become
# "equals". A test compares both lists with the engine's.
form_operator_aliases <- function() {
  c(
    equals = "equals", equal = "equals",
    not_equals = "not_equals", not_equal = "not_equals",
    `in` = "in", not_in = "not_in",
    greater = "greater", less = "less",
    greater_equal = "greater_equal", less_equal = "less_equal",
    min = "min_max", max = "min_max", range = "min_max",
    pattern = "pattern", empty = "empty"
  )
}

# ---- errors -------------------------------------------------------------------

# The words before a message: which clause, column and operator it is about.
form_where <- function(col, op, clause = NULL) {
  ops <- dta_condition_operators()
  label <- names(ops)[match(op, ops)]
  if (is.na(label)) label <- op
  paste0(if (!is.null(clause)) paste0(clause, ": "), "column ", col, ", ", label, ": ")
}

# `problem` is a cli template that may name variables of the calling function
# ({.val {txt}}); `where` is interpolated as a value, never read as a template,
# because it carries a column id and a condition name the user typed.
form_value_error <- function(where, problem, .envir = parent.frame()) {
  scope <- new.env(parent = .envir)
  scope$where <- where
  cli::cli_abort(
    paste0("{where}", problem),
    class = c("dta_form_value_error", "dta_form_error"),
    call = NULL, .envir = scope
  )
}

# ---- numbers ------------------------------------------------------------------

# A decimal number as the editor reads one: an optional sign, digits with an
# optional point, an optional exponent. No decimal comma, no hex, no Inf/NaN/NA.
form_number_pattern <- "^[+-]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][+-]?[0-9]+)?$"

# The number a text that matches `form_number_pattern` spells. R's own
# as.numeric() is not correctly rounded where `long double` is no wider than
# `double` (macOS arm64) and reads some 17-digit decimals one ulp off, so the
# text is rewritten in JSON's number grammar and read by jsonlite, whose parser
# uses the C library's strtod. Overflow (`1e400`) is Inf either way.
form_parse_number <- function(text) {
  parts <- regmatches(
    text, regexec("^([+-]?)([0-9]*)[.]?([0-9]*)([eE][+-]?[0-9]+)?$", text)
  )[[1]]
  whole <- sub("^0+(?=[0-9])", "", parts[3], perl = TRUE)
  json <- paste0(
    if (parts[2] == "-") "-", if (nzchar(whole)) whole else "0",
    if (nzchar(parts[4])) paste0(".", parts[4]), parts[5]
  )
  value <- tryCatch(
    jsonlite::parse_json(paste0("[", json, "]"))[[1]],
    error = function(e) NULL
  )
  if (is.numeric(value) && length(value) == 1L) as.double(value) else as.numeric(text)
}

form_read_number <- function(txt, where) {
  text <- trimws(txt)
  value <- if (grepl(form_number_pattern, text)) form_parse_number(text) else NA_real_
  if (!is.finite(value)) {
    form_value_error(where, "{.val {txt}} is not a number; write decimals with a point.")
  }
  value
}

# The text of a range bound's box: empty for an open side (absent, NA, +-Inf).
form_bound_text <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) || !is.finite(x[1])) {
    return("")
  }
  form_number_text(x)
}

# The shortest text that reads back as exactly this number. `as.character()`
# would show 100000 as "1e+05" and 0.1 + 0.2 as "0.3", which is another number.
form_number_text <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("")
  }
  vapply(x, function(v) {
    if (is.na(v)) {
      return("")
    }
    if (is.integer(v) || !is.finite(v)) {
      return(as.character(v))
    }
    for (digits in 15:17) {
      text <- sprintf("%.*g", digits, v)
      if (form_parse_number(text) == v) {
        return(text)
      }
    }
    text
  }, character(1))
}

# ---- clause -> rows -------------------------------------------------------------

# One value as the text of a Value box. A number is shown so that it reads back
# as itself; text is shown as it is.
form_scalar_text <- function(x) {
  x <- unlist(x, use.names = FALSE)
  if (length(x) == 0 || is.na(x[1])) {
    return("")
  }
  if (is.numeric(x)) form_number_text(x[1]) else as.character(x[1])
}

# The members of an in / not_in list as one text: comma separated, a member that
# holds a comma or a double quote (or is blank, padded, or a lone ~) inside
# double quotes with the quotes doubled, a missing member as ~.
form_list_text <- function(raw) {
  members <- if (is.list(raw)) raw else as.list(raw)
  paste(vapply(members, function(m) {
    if (is.null(m) || (length(m) == 1 && is.na(m))) {
      return("~")
    }
    text <- if (is.numeric(m)) form_number_text(m) else paste(as.character(unlist(m)), collapse = " ")
    if (!nzchar(text) || grepl("[,\"]", text) || !identical(text, trimws(text)) || identical(text, "~")) {
      text <- paste0("\"", gsub("\"", "\"\"", text, fixed = TRUE), "\"")
    }
    text
  }, character(1)), collapse = ", ")
}

form_row <- function(col, op, val, value) list(col = col, op = op, val = val, value = value)

form_unsupported <- function(problem, .envir = parent.frame()) {
  cli::cli_abort(
    c(problem, i = "Edit this rule in the YAML text instead."),
    class = c("dta_form_unsupported", "dta_form_error"),
    call = NULL, .envir = .envir
  )
}

# A named-list clause {COL: {op: val}} as editable rows, each
# list(col, op, val, value): `val` is the text of the Value box, `value` the
# entries the row stands for, kept verbatim under the engine's normal-form keys.
# One row per (column, operator); min and max together are one `min_max` row, and
# `range`, `equal` and `not_equal` are shown as min_max, equals and not_equals.
# A clause the editor cannot show (an unknown operator, a column that does not
# map operators to values) is an error, never a quiet rewrite.
condition_to_form_rows <- function(cond) {
  if (is.null(cond) || length(cond) == 0) {
    return(list())
  }
  if (is.null(names(cond)) || !all(nzchar(names(cond)))) {
    form_unsupported("A condition lists conditions without naming their columns.")
  }
  aliases <- form_operator_aliases()
  rows <- list()
  for (col in names(cond)) {
    spec <- cond[[col]]
    keys <- names(spec)
    if (!is.list(spec) || length(spec) == 0 || is.null(keys) || !all(nzchar(keys))) {
      form_unsupported("Column {.field {col}} does not map operators to values.")
    }
    unknown <- setdiff(keys, names(aliases))
    if (length(unknown) > 0) {
      form_unsupported("Column {.field {col}} uses {.val {unknown[1]}}, an operator the editor does not know.")
    }
    band_shown <- FALSE
    shown_ops <- character(0)
    for (key in keys) {
      raw <- spec[[key]]
      if (key %in% c("min", "max")) {
        if (band_shown) next
        band_shown <- TRUE
        value <- spec[intersect(c("min", "max"), keys)]
        mn <- if ("min" %in% keys) form_scalar_text(spec[["min"]]) else ""
        mx <- if ("max" %in% keys) form_scalar_text(spec[["max"]]) else ""
        row <- form_row(col, "min_max", sub(",\\s*$", "", paste0(mn, ", ", mx)), value)
      } else if (identical(key, "range")) {
        bounds <- as.list(raw)
        if (length(bounds) != 2) {
          form_unsupported("Column {.field {col}} gives {.val range} {length(bounds)} value{?s}, not two.")
        }
        value <- list(min = bounds[[1]], max = bounds[[2]])
        row <- form_row(
          col, "min_max",
          paste(vapply(bounds, form_scalar_text, character(1)), collapse = ", "), value
        )
      } else {
        op <- aliases[[key]]
        text <- if (identical(op, "empty")) {
          if (isTRUE(raw)) "true" else "false"
        } else if (op %in% c("in", "not_in")) {
          form_list_text(raw)
        } else {
          members <- as.list(raw)
          paste(vapply(members, form_scalar_text, character(1)), collapse = ", ")
        }
        row <- form_row(col, op, text, stats::setNames(list(raw), op))
      }
      # Two keys the form shows as one operator (`equals` with `equal`, `range`
      # with `min`) cannot be told apart in one row, and merging them would change
      # what the rule says; the form holds one row per operator, so such a clause
      # is not opened (it could never be saved without a change).
      if (row$op %in% shown_ops) {
        twice <- keys[unname(aliases[keys]) == row$op]
        form_unsupported("Column {.field {col}} has {.val {twice}}, which the editor shows as one operator.")
      }
      shown_ops <- c(shown_ops, row$op)
      rows[[length(rows) + 1L]] <- row
    }
  }
  rows
}

# ---- text -> value ----------------------------------------------------------------

form_need_value <- function(txt, where) {
  if (!nzchar(trimws(txt))) {
    form_value_error(where, "needs a value.")
  }
}

# true / false for the `empty` operator; a blank box is the default, true.
form_read_logical <- function(txt, where) {
  v <- tolower(trimws(txt))
  if (!nzchar(v) || v %in% c("true", "yes", "t", "y", "1")) {
    return(TRUE)
  }
  if (v %in% c("false", "no", "f", "n", "0")) {
    return(FALSE)
  }
  form_value_error(where, "{.val {txt}} is not true or false.")
}

# The members of a list text: values separated by commas, optionally in double
# quotes (a comma or a doubled quote inside them is part of the value), blanks
# around an unquoted value dropped. Returns list(text, quoted) over the members
# that hold something; an unquoted blank member (a doubled or trailing comma)
# holds nothing. A quote that is never closed, text after a closing quote, and a
# quote in the middle of an unquoted value are errors, not guesses.
form_split_list <- function(txt, where) {
  chars <- strsplit(txt, "", fixed = TRUE)[[1]]
  n <- length(chars)
  blank <- function(ch) ch %in% c(" ", "\t", "\r", "\n")
  members <- character(0)
  quoted <- logical(0)
  i <- 1L
  repeat {
    while (i <= n && blank(chars[i])) i <- i + 1L
    if (i <= n && identical(chars[i], "\"")) {
      i <- i + 1L
      buffer <- character(0)
      closed <- FALSE
      while (i <= n) {
        if (identical(chars[i], "\"")) {
          if (i < n && identical(chars[i + 1L], "\"")) {
            buffer <- c(buffer, "\"")
            i <- i + 2L
            next
          }
          closed <- TRUE
          i <- i + 1L
          break
        }
        buffer <- c(buffer, chars[i])
        i <- i + 1L
      }
      if (!closed) {
        form_value_error(where, "the list {.val {txt}} has a double quote that is never closed.")
      }
      while (i <= n && blank(chars[i])) i <- i + 1L
      if (i <= n && !identical(chars[i], ",")) {
        form_value_error(where, "in the list {.val {txt}}, a value in double quotes must be followed by a comma.")
      }
      members <- c(members, paste(buffer, collapse = ""))
      quoted <- c(quoted, TRUE)
    } else {
      start <- i
      while (i <= n && !identical(chars[i], ",")) {
        if (identical(chars[i], "\"")) {
          form_value_error(
            where,
            "in the list {.val {txt}}, a double quote must begin a value; put a value that holds one in double quotes and double the quote."
          )
        }
        i <- i + 1L
      }
      members <- c(members, if (i > start) trimws(paste(chars[start:(i - 1L)], collapse = "")) else "")
      quoted <- c(quoted, FALSE)
    }
    if (i > n) break
    i <- i + 1L
  }
  keep <- quoted | nzchar(members)
  list(text = members[keep], quoted = quoted[keep])
}

# An in / not_in list: numbers for a numeric column, text otherwise; an unquoted
# ~ is a missing member, as in YAML.
form_read_list <- function(txt, numeric, where) {
  parts <- form_split_list(txt, where)
  if (length(parts$text) == 0) {
    form_value_error(where, "needs at least one value.")
  }
  missing <- !parts$quoted & parts$text == "~"
  if (numeric) {
    out <- rep(NA_real_, length(parts$text))
    out[!missing] <- vapply(parts$text[!missing], form_read_number, numeric(1), where = where, USE.NAMES = FALSE)
  } else {
    out <- parts$text
    out[missing] <- NA_character_
  }
  out
}

# A comparison bound (greater, less, ...): a number; on a date column the text
# of the date, which the engine reads as a date.
form_read_bound <- function(txt, family, where) {
  form_need_value(txt, where)
  if (identical(family, "temporal")) trimws(txt) else form_read_number(txt, where)
}

# The positional "min, max" text of a between row as list(min =, max =); a blank
# bound is absent (", 9" is max-only). At most two bounds, at least one.
parse_form_between <- function(txt, family = NA_character_, where = "") {
  txt <- txt %||% ""
  parts <- trimws(strsplit(txt, ",", fixed = TRUE)[[1]])
  if (length(parts) > 2) {
    form_value_error(where, "{.val {txt}} has more than two bounds; write \"min, max\".")
  }
  bound <- function(part) {
    if (!nzchar(part)) NULL else form_read_bound(part, family, where)
  }
  out <- list()
  if (length(parts) >= 1) out$min <- bound(parts[1])
  if (length(parts) >= 2) out$max <- bound(parts[2])
  if (length(out) == 0) {
    form_value_error(where, "needs a minimum and/or a maximum.")
  }
  out
}

# The two boxes of a range rule: list(min =, max =). A blank box is an open
# side: -Inf for the minimum, Inf for the maximum; both blank is an error. A box
# still showing the bound the rule was opened with (`orig`, list(min =, max =))
# keeps that bound as it was, so an integer stays an integer. The message text of
# "needs one" is the editor's old one.
parse_form_bounds <- function(min_txt, max_txt, orig = list()) {
  read <- function(txt, label, kept, open) {
    txt <- txt %||% ""
    if (!nzchar(trimws(txt))) {
      return(open)
    }
    if (!is.null(kept) && identical(txt, form_number_text(kept))) {
      return(kept)
    }
    form_read_number(txt, paste0(label, ": "))
  }
  if (!nzchar(trimws(min_txt %||% "")) && !nzchar(trimws(max_txt %||% ""))) {
    form_value_error("", "A range rule needs a minimum and/or maximum.")
  }
  list(
    min = read(min_txt, "Minimum", orig[["min"]], -Inf),
    max = read(max_txt, "Maximum", orig[["max"]], Inf)
  )
}

# The text of one Value box as the R value its operator takes, read by the
# declared type family of the column: numbers for a numeric column (error when
# the text is not one), text for any other or an unknown column. The comparison
# bounds are numbers on every column but a date one.
parse_form_value <- function(op, txt, family = NA_character_, where = "") {
  txt <- txt %||% ""
  numeric_col <- isTRUE(family %in% c("number", "integer"))
  switch(op,
    empty = form_read_logical(txt, where),
    pattern = {
      form_need_value(txt, where)
      txt
    },
    `in` = ,
    not_in = form_read_list(txt, numeric_col, where),
    greater = ,
    less = ,
    greater_equal = ,
    less_equal = form_read_bound(txt, family, where),
    equals = ,
    not_equals = {
      form_need_value(txt, where)
      if (numeric_col) form_read_number(txt, where) else txt
    },
    form_value_error(where, "{.val {op}} is not an operator the editor can read.")
  )
}

# ---- rows -> clause ------------------------------------------------------------------

# A row is untouched when its column, operator and text are the ones it was
# opened with. `orig` is the row condition_to_form_rows() produced for it.
form_row_untouched <- function(row, col, op) {
  orig <- row$orig
  !is.null(orig) && !is.null(orig$value) &&
    identical(col, orig$col) && identical(op, orig$op) && identical(row$val, orig$val)
}

# Form rows (each list(col, op, val), and orig for a row that was opened with the
# rule) as a named-list clause. A row without a column is empty and skipped.
# Rows of one column are merged, in the order of the rows; the same operator
# twice in one column is an error. An untouched row keeps its original value; any
# other row is read from its text by the type family of its column, `families`
# being a named vector column id -> family. `clause` ("IF", "THEN", ...) is only
# named in the messages.
form_rows_to_condition <- function(rows, families = character(0), clause = NULL) {
  out <- list()
  seen <- list()
  for (row in rows) {
    col <- trimws(row$col %||% "")
    if (!nzchar(col)) {
      # An empty row is skipped; a row with a value and no column is not.
      val <- row$val %||% ""
      if (nzchar(trimws(val))) {
        form_value_error(
          if (is.null(clause)) "" else paste0(clause, ": "),
          "a row with the value {.val {val}} has no column."
        )
      }
      next
    }
    op <- row$op %||% "equals"
    where <- form_where(col, op, clause)
    entries <- if (form_row_untouched(row, col, op)) {
      row$orig$value
    } else {
      family <- if (col %in% names(families)) families[[col]] else NA_character_
      if (identical(op, "min_max")) {
        parse_form_between(row$val, family, where)
      } else {
        stats::setNames(list(parse_form_value(op, row$val, family, where)), op)
      }
    }
    if (op %in% seen[[col]]) {
      label <- names(dta_condition_operators())[match(op, dta_condition_operators())]
      cli::cli_abort(
        if (is.null(clause)) {
          "Column {.field {col}} has the operator {.val {label}} on two rows; combine them into one row."
        } else {
          "{clause}: column {.field {col}} has the operator {.val {label}} on two rows; combine them into one row."
        },
        class = c("dta_form_operator_twice", "dta_form_error"), call = NULL
      )
    }
    seen[[col]] <- c(seen[[col]], op)
    merged <- out[[col]] %||% list()
    merged[names(entries)] <- entries
    out[[col]] <- merged
  }
  out
}
