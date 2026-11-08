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
  # Other control characters (BEL, VT, FF, ESC, C1 controls ...) have no
  # meaning in a text document and some readers end a line on them: a space.
  x <- gsub("[\\x{01}-\\x{08}\\x{0B}\\x{0C}\\x{0E}-\\x{1F}\\x{7F}-\\x{9F}]", " ", x, perl = TRUE)
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

# Web addresses and e-mail addresses in author text. They are written as
# links (see .md_link()) instead of being escaped: a backslash escape inside
# an address would either break it ("john\_doe@...") or, where a reader turns
# bare addresses into links itself, show up in the link. A URL starts with
# http://, https://, ftp:// or www.; an e-mail address follows the address
# grammar of CommonMark autolinks, so the link is always recognised.
.MD_URL_PATTERN <- paste0(
  "(?<![A-Za-z0-9@._%+-])(?:(?:https?|ftp)://[^\\s<>\\p{Z}]+|www\\.[A-Za-z0-9][^\\s<>\\p{Z}]*)"
)
.MD_EMAIL_PATTERN <- paste0(
  "(?<![A-Za-z0-9._%+-])[A-Za-z0-9._%+-]+@",
  "[A-Za-z0-9](?:[A-Za-z0-9-]*[A-Za-z0-9])?(?:\\.[A-Za-z0-9](?:[A-Za-z0-9-]*[A-Za-z0-9])?)+",
  "(?![A-Za-z0-9-]|\\.[A-Za-z0-9])"
)

# Punctuation that ends a sentence rather than a URL: trailing ".,:;!?'\"*_~"
# and a closing ")" or "]" without its opening partner inside the URL are
# left outside the link.
.md_trim_url <- function(url) {
  repeat {
    last <- substr(url, nchar(url), nchar(url))
    count <- function(ch) lengths(regmatches(url, gregexpr(ch, url, fixed = TRUE)))
    if (last %in% c(".", ",", ":", ";", "!", "?", "'", "\"", "*", "_", "~") ||
      (last == ")" && count(")") > count("(")) ||
      (last == "]" && count("]") > count("["))) {
      url <- substr(url, 1L, nchar(url) - 1L)
    } else {
      return(url)
    }
  }
}

# Start and end (character positions) of every URL and e-mail address in one
# line, in order; a zero-row matrix when there is none.
.md_link_spans <- function(line) {
  spans <- matrix(integer(0), ncol = 2L)
  m <- gregexpr(.MD_URL_PATTERN, line, perl = TRUE)[[1]]
  if (m[[1]] > 0L) {
    for (k in seq_along(m)) {
      url <- .md_trim_url(substr(line, m[[k]], m[[k]] + attr(m, "match.length")[[k]] - 1L))
      if (grepl("^(?:(?:https?|ftp)://[^/?#]|www\\.[A-Za-z0-9])", url, perl = TRUE)) {
        spans <- rbind(spans, c(m[[k]], m[[k]] + nchar(url) - 1L))
      }
    }
  }
  m <- gregexpr(.MD_EMAIL_PATTERN, line, perl = TRUE)[[1]]
  if (m[[1]] > 0L) {
    for (k in seq_along(m)) {
      start <- m[[k]]
      end <- start + attr(m, "match.length")[[k]] - 1L
      inside <- nrow(spans) > 0L && any(spans[, 1L] <= end & spans[, 2L] >= start)
      if (!inside) spans <- rbind(spans, c(start, end))
    }
  }
  spans[order(spans[, 1L]), , drop = FALSE]
}

# One URL or e-mail address as an autolink, "<...>". An autolink takes its
# content literally -- no escapes apply in it -- so the address is written
# unchanged; only in a table cell is "|" escaped, because the table splits its
# cells before anything else is read. An address starting with "www." has no
# scheme, which an autolink needs: it is linked as "http://www..." (the target
# GitHub gives it too). Link text with escapes would not do: a reader that
# links bare addresses itself (pandoc 3.13) shows the backslashes.
.md_link <- function(token, table = FALSE) {
  if (startsWith(token, "www.")) {
    token <- paste0("http://", token)
  }
  if (isTRUE(table)) {
    token <- gsub("|", "\\|", token, fixed = TRUE)
  }
  paste0("<", token, ">")
}

# Escape one line segment of plain text (no URLs in it). `line_start` adds the
# rules for the start of a line: list markers, Setext underlines, indentation.
#   * "\" and the characters that open inline syntax are backslash-escaped:
#     ` * _ { } [ ] # ! ~ | and "$" (math);
#   * "<" and ">" become entities; "&" is escaped only where it would start
#     an entity ("&amp;", "&#169;"), so "R&D" stays as written;
#   * an emoji short code (":smile:", ":+1:", ":100:") gets its first ":"
#     escaped; a time such as "07:45:59" is no short code and stays as is.
.md_escape_plain <- function(text, line_start = TRUE) {
  text <- gsub("\\", "\\\\", text, fixed = TRUE)
  text <- gsub("&(?=[#A-Za-z0-9]+;)", "\\\\&", text, perl = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub("([`*_{}\\[\\]#!~|$])", "\\\\\\1", text, perl = TRUE)
  text <- gsub(":(?=(?:(?![0-9]+:)[A-Za-z0-9_+-]+|100|1234):)", "\\\\:", text, perl = TRUE)
  if (!isTRUE(line_start)) {
    return(text)
  }
  text <- sub("^([ \t]*)([-+])", "\\1\\\\\\2", text, perl = TRUE)
  text <- sub("^([ \t]*[0-9]+)([.)])(?=[ \t]|$)", "\\1\\\\\\2", text, perl = TRUE)
  # A line of "=" alone would underline the line above as a heading.
  text <- sub("^([ \t]*)(=+[ \t]*)$", "\\1\\\\\\2", text, perl = TRUE)
  # Visible indentation must not open an indented code block: non-breaking
  # spaces of the same width (a tab reaches the next multiple of four).
  indent <- regmatches(text, regexpr("^[ \t]+", text))
  if (length(indent) == 1L) {
    width <- 0L
    for (char in strsplit(indent, "", fixed = TRUE)[[1]]) {
      width <- if (char == "\t") (width %/% 4L + 1L) * 4L else width + 1L
    }
    text <- paste0(strrep("\u00a0", width), substring(text, nchar(indent) + 1L))
  }
  text
}

# Escape one line: its URLs and e-mail addresses become links (unless `links`
# is FALSE, e.g. for the text of a link, which cannot hold another link), the
# rest is escaped as plain text.
.md_escape_line <- function(line, table = FALSE, links = TRUE) {
  spans <- if (isTRUE(links)) .md_link_spans(line) else matrix(integer(0), ncol = 2L)
  if (nrow(spans) == 0L) {
    return(.md_escape_plain(line))
  }
  out <- character(0)
  pos <- 1L
  for (k in seq_len(nrow(spans))) {
    if (spans[k, 1L] > pos) {
      out <- c(out, .md_escape_plain(substr(line, pos, spans[k, 1L] - 1L), line_start = pos == 1L))
    }
    out <- c(out, .md_link(substr(line, spans[k, 1L], spans[k, 2L]), table = table))
    pos <- spans[k, 2L] + 1L
  }
  if (pos <= nchar(line)) {
    out <- c(out, .md_escape_plain(substr(line, pos, nchar(line)), line_start = FALSE))
  }
  paste(out, collapse = "")
}

# Escape one string (possibly several lines) as plain text, line by line.
# Trailing blanks are removed, because they would be a hard line break here
# and would be stripped by .md_write() anyway.
.md_escape_text <- function(text, table = FALSE, links = TRUE) {
  lines <- .md_lines(text)
  lines <- sub("[ \t]+$", "", lines)
  escaped <- vapply(lines, .md_escape_line, character(1), table = table, links = links, USE.NAMES = FALSE)
  paste(escaped, collapse = "\n")
}

# Escape one string for a table cell: indentation is trimmed (no NBSP), line
# breaks become "<br>" and "|" is escaped, so the cell can neither end early
# nor add a cell.
.md_escape_cell <- function(text, links = TRUE) {
  lines <- .md_lines(text)
  lines <- sub("[ \t]+$", "", sub("^[ \t]+", "", lines))
  lines <- .md_trim_blank(lines)
  if (length(lines) == 0L) {
    return("")
  }
  escaped <- vapply(lines, .md_escape_text, character(1), table = TRUE, links = links, USE.NAMES = FALSE)
  paste(escaped, collapse = "<br>")
}

# Escape text so that it is shown literally: Markdown and HTML syntax, line
# starts that would open lists, quotes, headings or code, and "$" are all
# neutralised; web and e-mail addresses become links showing the same text.
# With `table = TRUE` the result is safe inside a table cell (single line:
# breaks become "<br>", leading indentation is trimmed). `links = FALSE`
# escapes addresses like any other text (for text that is itself a link).
# NA becomes "", NULL gives character(0).
.md_escape <- function(x, table = FALSE, links = TRUE) {
  if (is.null(x)) {
    return(character(0))
  }
  x <- as.character(x)
  x[is.na(x)] <- ""
  if (isTRUE(table)) {
    vapply(x, .md_escape_cell, character(1), links = links, USE.NAMES = FALSE)
  } else {
    vapply(x, .md_escape_text, character(1), links = links, USE.NAMES = FALSE)
  }
}

# Reverse of .md_escape() for what it produces (links, ASCII punctuation
# escapes and the HTML entities). Used to recover the heading text that a
# renderer sees, e.g. to compute an anchor.
.md_unescape <- function(x) {
  x <- gsub("<((?:(?:https?|ftp)://|[A-Za-z0-9._%+-]+@)[^<>]*)>", "\\1", x, perl = TRUE)
  # Entities first, and only those .md_escape() wrote ("\\&lt;" is the
  # author's literal text "&lt;").
  x <- gsub("(?<!\\\\)&lt;", "<", x, perl = TRUE)
  x <- gsub("(?<!\\\\)&gt;", ">", x, perl = TRUE)
  gsub("\\\\([!-/:-@\\[-`{-~])", "\\1", x, perl = TRUE)
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
# "left" / "right" / "center" (recycled over the columns). `drop_empty = TRUE`
# leaves out every column whose cells are all empty (blank), header aside.
.md_table <- function(df, align = NULL, drop_empty = FALSE) {
  if (!is.data.frame(df)) {
    cli::cli_abort("{.arg df} must be a data frame, not {.cls {class(df)}}.")
  }
  if (isTRUE(drop_empty) && ncol(df) > 0L) {
    filled <- vapply(df, function(col) any(nzchar(trimws(.md_cell_chr(col)))), logical(1))
    if (!is.null(align) && length(align) == ncol(df)) align <- align[filled]
    df <- df[, filled, drop = FALSE]
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
  # Lower case by the Unicode rules, whatever the locale of the session
  # (tolower() leaves "\u00dc" alone under LC_CTYPE=C, the reader does not).
  slug <- stringr::str_to_lower(enc2utf8(text), locale = "en")
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
# of levels 1 and 2 (only when there are at least two chapters of level 1: a
# list with one entry guides nobody), the numbered chapters (empty ones
# omitted, numbers close the gaps), then `footer` lines. Anchors are those
# GitHub computes for the rendered headings (number included), in document
# order -- unnumbered headings inside a chapter's text count too -- so
# duplicates across the whole document receive "-1", "-2", ...
.md_render <- function(head = character(), sections = list(), footer = character()) {
  sections <- .md_prune(sections)
  if (length(sections) == 0L) {
    return(.md_compose(head, footer))
  }
  entries <- .md_number(sections)
  with_toc <- length(sections) >= 2L

  used <- character()
  for (title in c(.md_head_titles(.md_lines(head)), if (with_toc) "Contents")) {
    used <- .md_slug(title, used)$used
  }
  for (i in seq_along(entries)) {
    slugged <- .md_slug(paste(entries[[i]]$number, entries[[i]]$title), used)
    entries[[i]]$slug <- slugged$slug
    used <- slugged$used
    for (title in .md_head_titles(entries[[i]]$body)) {
      used <- .md_slug(title, used)$used
    }
  }

  toc <- character(0)
  for (e in entries) {
    if (!with_toc || e$depth > 2L) next
    toc <- c(toc, paste0(
      strrep("  ", e$depth - 1L), "- [", e$number, " ", .md_escape(e$title, links = FALSE), "](#", e$slug, ")"
    ))
  }

  blocks <- list(head, if (with_toc) "## Contents", toc)
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
