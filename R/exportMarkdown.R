# Markdown writer primitives for the document exports.
#
# Everything here is internal and works on character vectors of *lines*. The
# contract shared by all builders:
#
# * Author text (titles, descriptions, names, values) is always passed through
#   .md_escape() -- directly or via .md_table(), .md_kv_table(), .md_list() and
#   .md_render() -- so it can neither change the structure of the document nor
#   be reinterpreted (emphasis, lists, headings, tables, raw HTML, math, links).
# * Blocks are joined with exactly one blank line (.md_compose()), which also
#   rules out the Setext trap (a "---" directly below a text line would turn
#   that line into a heading).
# * Output is deterministic: no timestamps, no locale-dependent formatting.
# * .md_write() writes UTF-8 with LF line endings on every platform, one line
#   ending at the end of the file and no trailing blanks on any line.

# Split a character vector into single lines (LF, CRLF and CR all end a line).
# NA becomes an empty line; the result has one element per line, interior
# empty lines are kept, NULL and character(0) give character(0).
.md_lines <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(character(0))
  }
  x <- as.character(unlist(x, use.names = FALSE))
  x[is.na(x)] <- ""
  x <- enc2utf8(x)
  x <- gsub("\r\n?", "\n", x)
  unlist(strsplit(paste0(x, "\n"), "\n", fixed = TRUE), use.names = FALSE)
}

# Drop empty lines from both ends of a vector of lines.
.md_trim_blank <- function(lines) {
  keep <- which(nzchar(lines))
  if (length(keep) == 0L) {
    return(character(0))
  }
  lines[keep[1L]:keep[length(keep)]]
}

# Escape one string (possibly several lines) as plain text. The escaping of
# the bulk of the special characters is delegated to .plain_text_to_md(); this
# adds what that function does not cover:
#   * "$" (math), emoji short codes (":smile:"), and a list marker that ends
#     its line ("1." or "2)" on a line of its own opens an empty list item);
#   * trailing blanks are removed, because they would be a hard line break
#     here and would be stripped by .md_write() anyway.
.md_escape_text <- function(text) {
  lines <- .md_lines(text)
  lines <- sub("[ \t]+$", "", lines)
  text <- paste(lines, collapse = "\n")
  text <- .plain_text_to_md(text, table = FALSE)
  text <- gsub("$", "\\$", text, fixed = TRUE)
  text <- gsub("(?m)^([ \t\u00a0]*[0-9]+)([.)])$", "\\1\\\\\\2", text, perl = TRUE)
  gsub(":(?=[A-Za-z0-9_+-]+:)", "\\\\:", text, perl = TRUE)
}

# Escape one string for a table cell: indentation is trimmed (no NBSP), line
# breaks become "<br>" and "|" is escaped, so the cell can neither end early
# nor add a cell.
.md_escape_cell <- function(text) {
  lines <- .md_lines(text)
  lines <- sub("[ \t]+$", "", sub("^[ \t]+", "", lines))
  lines <- .md_trim_blank(lines)
  if (length(lines) == 0L) {
    return("")
  }
  escaped <- vapply(lines, .md_escape_text, character(1), USE.NAMES = FALSE)
  paste(escaped, collapse = "<br>")
}

# Escape text so that it is shown literally: Markdown and HTML syntax, line
# starts that would open lists, quotes, headings or code, and "$" are all
# neutralised. With `table = TRUE` the result is safe inside a table cell
# (single line: breaks become "<br>", leading indentation is trimmed). NA
# becomes "", NULL gives character(0).
.md_escape <- function(x, table = FALSE) {
  if (is.null(x)) {
    return(character(0))
  }
  x <- as.character(x)
  x[is.na(x)] <- ""
  one <- if (isTRUE(table)) .md_escape_cell else .md_escape_text
  vapply(x, one, character(1), USE.NAMES = FALSE)
}

# Reverse of .md_escape() for the characters it produces (ASCII punctuation
# escapes and the three HTML entities). Used to recover the heading text that
# a renderer sees, e.g. to compute an anchor.
.md_unescape <- function(x) {
  x <- gsub("\\\\([!-/:-@\\[-`{-~])", "\\1", x, perl = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

# One table cell value as a string (NA -> "", factors and list cells flattened).
.md_cell_chr <- function(col) {
  if (is.list(col)) {
    return(vapply(col, function(v) {
      v <- unlist(v, use.names = FALSE)
      if (length(v) == 0L) "" else paste(as.character(v[!is.na(v)]), collapse = ", ")
    }, character(1)))
  }
  out <- as.character(col)
  out[is.na(out)] <- ""
  out
}

# A GFM pipe table: header row, delimiter row, one line per data row, every
# line with the same number of cells. Every cell is escaped. A table without
# rows or without columns gives character(0). `align` is NULL or a vector of
# "left" / "right" / "center" (recycled over the columns).
.md_table <- function(df, align = NULL) {
  if (!is.data.frame(df)) {
    cli::cli_abort("{.arg df} must be a data frame, not {.cls {class(df)}}.")
  }
  if (nrow(df) == 0L || ncol(df) == 0L) {
    return(character(0))
  }
  n_col <- ncol(df)
  delim <- rep("---", n_col)
  if (!is.null(align)) {
    ok <- c("left", "right", "center")
    if (!is.character(align) || !length(align) %in% c(1L, n_col) || !all(align %in% ok)) {
      cli::cli_abort(
        "{.arg align} must be {.val left}, {.val right} or {.val center}, one per column or a single value."
      )
    }
    delim <- c(left = ":---", right = "---:", center = ":---:")[rep_len(align, n_col)]
  }
  header <- .md_escape(names(df), table = TRUE)
  cells <- lapply(df, function(col) .md_escape(.md_cell_chr(col), table = TRUE))
  rows <- do.call(paste, c(unname(cells), sep = " | "))
  c(
    paste0("| ", paste(header, collapse = " | "), " |"),
    paste0("| ", paste(unname(delim), collapse = " | "), " |"),
    paste0("| ", rows, " |")
  )
}

# Normalise "pairs" to a two-column data frame (Item, Details). Accepts a
# two-column data frame, a named vector or list (names are the items), or a
# list of list(key =, value =) elements.
.md_pairs_df <- function(pairs) {
  if (is.null(pairs) || length(pairs) == 0L) {
    return(NULL)
  }
  flatten <- function(v) {
    v <- unlist(v, use.names = FALSE)
    if (length(v) == 0L) "" else paste(as.character(v[!is.na(v)]), collapse = ", ")
  }
  if (is.data.frame(pairs)) {
    if (ncol(pairs) < 2L) {
      cli::cli_abort("{.arg pairs} must have at least two columns (item, details).")
    }
    item <- .md_cell_chr(pairs[[1L]])
    details <- .md_cell_chr(pairs[[2L]])
  } else if (is.list(pairs) && is.null(names(pairs)) &&
    all(vapply(pairs, function(p) is.list(p) && all(c("key", "value") %in% names(p)), logical(1)))) {
    item <- vapply(pairs, function(p) flatten(p[["key"]]), character(1))
    details <- vapply(pairs, function(p) flatten(p[["value"]]), character(1))
  } else if (!is.null(names(pairs)) && !anyNA(names(pairs))) {
    item <- names(pairs)
    details <- vapply(unname(as.list(pairs)), flatten, character(1))
  } else {
    cli::cli_abort(c(
      "{.arg pairs} must be a data frame, a named vector/list, or a list of {.code list(key, value)}.",
      i = "Got an unnamed {.cls {class(pairs)}}."
    ))
  }
  data.frame(Item = item, Details = details, stringsAsFactors = FALSE)
}

# Two-column "Item | Details" table. No pairs -> character(0).
.md_kv_table <- function(pairs) {
  .md_table(.md_pairs_df(pairs) %||% data.frame(Item = character(0), Details = character(0)))
}

# Bullet list. Items are escaped (`escape = FALSE` takes ready-made inline
# Markdown instead). NA and blank items are dropped; line breaks inside an
# item become hard breaks with a continuation line (blank lines inside an
# item are dropped). No items -> character(0).
.md_list <- function(items, escape = TRUE) {
  items <- unlist(items, use.names = FALSE)
  if (length(items) == 0L) {
    return(character(0))
  }
  out <- list()
  for (item in as.character(items)) {
    if (is.na(item)) next
    lines <- .md_lines(item)
    lines <- sub("[ \t]+$", "", sub("^[ \t]+", "", lines))
    lines <- lines[nzchar(lines)]
    if (length(lines) == 0L) next
    if (isTRUE(escape)) {
      lines <- .md_lines(.md_escape_text(paste(lines, collapse = "\n")))
    }
    n <- length(lines)
    if (n > 1L) {
      lines[-n] <- paste0(lines[-n], "\\")
    }
    out[[length(out) + 1L]] <- c(paste0("- ", lines[1L]), if (n > 1L) paste0("  ", lines[-1L]))
  }
  as.character(unlist(out, use.names = FALSE))
}

# Join blocks (character vectors of lines) with exactly one blank line. Empty
# blocks vanish, blank lines at the edges of a block are dropped, runs of blank
# lines inside a block collapse to one, whitespace-only lines become empty.
# A line made only of "-" or "=" is separated from a text line above it, so it
# can never act as a Setext underline.
.md_compose <- function(...) {
  blocks <- list(...)
  out <- character(0)
  for (block in blocks) {
    lines <- .md_lines(block)
    lines[grepl("^[ \t]*$", lines)] <- ""
    lines <- .md_trim_blank(lines)
    if (length(lines) == 0L) next
    lines <- lines[c(TRUE, !(lines[-1L] == "" & lines[-length(lines)] == ""))]
    trap <- which(grepl("^ {0,3}(-+|=+)[ \t]*$", lines))
    trap <- trap[trap > 1L & nzchar(lines[pmax(trap - 1L, 1L)])]
    for (i in rev(trap)) {
      lines <- append(lines, "", after = i - 1L)
    }
    out <- c(out, if (length(out)) "", lines)
  }
  out
}

# GitHub-flavoured heading anchor for the plain (unescaped) heading text:
# lower case; everything except letters, marks, numbers, "_" (connector
# punctuation), "-" and the space is removed; each space becomes "-". A slug
# already in `used` gets "-1", "-2", ... appended. Returns the slug and the
# updated `used` as list(slug =, used =).
.md_slug <- function(text, used = character()) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    cli::cli_abort("{.arg text} must be a single string.")
  }
  slug <- tolower(enc2utf8(text))
  slug <- gsub("[^\\p{L}\\p{M}\\p{N}\\p{Pc} -]", "", slug, perl = TRUE)
  slug <- gsub(" ", "-", slug, fixed = TRUE)
  candidate <- slug
  i <- 0L
  while (candidate %in% used) {
    i <- i + 1L
    candidate <- paste0(slug, "-", i)
  }
  list(slug = candidate, used = c(used, candidate))
}

# A chapter of the document: `body` is a vector of lines, `children` a list of
# further .md_section() values (sub-chapters).
.md_section <- function(title, body = character(), children = list()) {
  if (!is.character(title) || length(title) != 1L || is.na(title) ||
    !nzchar(.md_clean_title(title))) {
    cli::cli_abort("{.arg title} must be a single non-empty string.")
  }
  if (!is.list(children) || !all(vapply(children, inherits, logical(1), what = "dta_md_section"))) {
    cli::cli_abort("{.arg children} must be a list of {.fn .md_section} values.")
  }
  structure(
    list(title = .md_clean_title(title), body = .md_lines(body), children = unname(children)),
    class = "dta_md_section"
  )
}

# Heading text as a renderer sees it: one line, single spaces.
.md_clean_title <- function(title) {
  trimws(gsub("[ \t\r\n]+", " ", title))
}

# TRUE if a chapter has neither text nor any non-empty sub-chapter.
.md_section_empty <- function(section) {
  !any(nzchar(trimws(section$body))) && all(vapply(section$children, .md_section_empty, logical(1)))
}

# Remove empty chapters at every level.
.md_prune <- function(sections) {
  sections <- Filter(Negate(.md_section_empty), sections)
  lapply(sections, function(s) {
    s$children <- .md_prune(s$children)
    s
  })
}

# Flatten chapters to heading records in document order, numbering them
# 1, 1.1, 1.1.1. The deepest heading is level 4 (three chapter levels).
.md_number <- function(sections, prefix = character(), depth = 1L) {
  if (length(sections) > 0L && depth > 3L) {
    cli::cli_abort("Chapters can be nested at most three levels deep (heading level 4).")
  }
  out <- list()
  for (i in seq_along(sections)) {
    s <- sections[[i]]
    number <- paste(c(prefix, i), collapse = ".")
    out[[length(out) + 1L]] <- list(
      depth = depth, number = number, title = s$title, body = s$body
    )
    out <- c(out, .md_number(s$children, c(prefix, i), depth + 1L))
  }
  out
}

# Heading texts (unescaped) of the ATX headings in `lines`.
.md_head_titles <- function(lines) {
  hits <- regmatches(lines, regexec("^ {0,3}#{1,6}[ \t]+(.*?)([ \t]+#+)?[ \t]*$", lines, perl = TRUE))
  vapply(Filter(length, hits), function(m) .md_clean_title(.md_unescape(m[2L])), character(1))
}

# Assemble the document: `head` lines, a "## Contents" list linking chapters
# of levels 1 and 2, the numbered chapters (empty ones omitted, numbers close
# the gaps), then `footer` lines. Anchors are those GitHub computes for the
# rendered headings (number included), duplicates across the whole document
# receiving "-1", "-2", ...
.md_render <- function(head = character(), sections = list(), footer = character()) {
  sections <- .md_prune(sections)
  if (length(sections) == 0L) {
    return(.md_compose(head, footer))
  }
  entries <- .md_number(sections)

  used <- character()
  for (title in c(.md_head_titles(.md_lines(head)), "Contents")) {
    used <- .md_slug(title, used)$used
  }
  for (i in seq_along(entries)) {
    slugged <- .md_slug(paste(entries[[i]]$number, entries[[i]]$title), used)
    entries[[i]]$slug <- slugged$slug
    used <- slugged$used
  }

  toc <- character(0)
  for (e in entries) {
    if (e$depth > 2L) next
    toc <- c(toc, paste0(
      strrep("  ", e$depth - 1L), "- [", e$number, " ", .md_escape(e$title), "](#", e$slug, ")"
    ))
  }

  blocks <- list(head, "## Contents", toc)
  for (e in entries) {
    blocks <- c(blocks, list(
      paste0(strrep("#", e$depth + 1L), " ", e$number, " ", .md_escape(e$title)),
      e$body
    ))
  }
  blocks <- c(blocks, list(footer))
  do.call(.md_compose, blocks)
}

# Write lines to `file` as UTF-8 with LF line endings (also on Windows: the
# connection is binary), exactly one line ending at the end of the file and no
# blanks at the end of any line. Goes through .dta_write_staged(), so an
# existing file is only replaced by a complete new one. Returns `file`
# invisibly.
.md_write <- function(lines, file) {
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    cli::cli_abort("{.arg file} must be a single file path.")
  }
  raw_lines <- enc2utf8(as.character(unlist(lines, use.names = FALSE)))
  if (!all(validUTF8(raw_lines[!is.na(raw_lines)]))) {
    cli::cli_abort("The Markdown text contains characters that cannot be written as UTF-8.")
  }
  lines <- .md_lines(raw_lines)
  lines <- sub("[ \t]+$", "", lines)
  last <- which(nzchar(lines))
  lines <- if (length(last)) lines[seq_len(last[length(last)])] else character(0)
  bytes <- if (length(lines)) {
    charToRaw(paste0(paste(lines, collapse = "\n"), "\n"))
  } else {
    raw(0)
  }
  .dta_write_staged(file, function(stage) {
    con <- base::file(stage, open = "wb")
    on.exit(close(con), add = TRUE)
    writeBin(bytes, con)
  })
}
