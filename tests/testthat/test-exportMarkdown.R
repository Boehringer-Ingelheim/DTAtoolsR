# Tests for the internal Markdown writer primitives (R/exportMarkdown.R).
#
# Expectations are written by hand from the Markdown / GFM specification and
# from the contract of each function. Where pandoc is used it acts as an
# independent reader of the produced Markdown (pandoc -f gfm); it is skipped
# when pandoc is not installed. Nothing here depends on translated text.

# ---- helpers (pandoc as an independent reader) ------------------------------

mdt_skip_without_pandoc <- function() {
  testthat::skip_if_not(nzchar(Sys.which("pandoc")), "pandoc is not installed")
}

# Run pandoc on `lines` (written as UTF-8, LF) and return the output lines.
mdt_pandoc <- function(lines, to = "json", from = "gfm") {
  inp <- tempfile(fileext = ".md")
  out <- tempfile()
  on.exit(unlink(c(inp, out), force = TRUE), add = TRUE)
  text <- paste0(paste(lines, collapse = "\n"), "\n")
  writeBin(charToRaw(enc2utf8(text)), inp)
  status <- system2(
    Sys.which("pandoc"), c("-f", from, "-t", to, "-o", out, inp),
    stdout = FALSE, stderr = FALSE
  )
  if (!identical(status, 0L)) stop("pandoc failed")
  readLines(out, encoding = "UTF-8", warn = FALSE)
}

mdt_ast <- function(lines, from = "gfm") {
  jsonlite::fromJSON(
    paste(mdt_pandoc(lines, "json", from), collapse = "\n"),
    simplifyVector = FALSE
  )$blocks
}

# Collapse whitespace the way a Markdown reader does.
mdt_norm <- function(x) {
  trimws(gsub("[ \t\n\u00a0]+", " ", x))
}

# Text of a list of inlines; NULL if anything but plain text occurs (so any
# emphasis, code, link, raw HTML, hard break ... is detected as a failure).
# `<br>` is accepted as a line break where `allow_br`, hard breaks where
# `allow_break`.
mdt_inlines <- function(inl, allow_br = FALSE, allow_break = FALSE) {
  parts <- vapply(inl, function(i) {
    switch(i$t,
      Str = i$c,
      Space = " ",
      SoftBreak = " ",
      LineBreak = if (allow_break) " " else NA_character_,
      RawInline = if (allow_br && identical(i$c[[2]], "<br>")) " " else NA_character_,
      NA_character_
    )
  }, character(1))
  if (anyNA(parts)) NULL else paste(parts, collapse = "")
}

mdt_table <- function(block, allow_br = TRUE) {
  rows <- c(block$c[[4]][[2]], block$c[[5]][[1]][[4]])
  lapply(rows, function(row) {
    vapply(row[[2]], function(cell) {
      blocks <- cell[[5]]
      if (length(blocks) == 0L) {
        return("")
      }
      if (length(blocks) != 1L || !blocks[[1]]$t %in% c("Plain", "Para")) {
        return(NA_character_)
      }
      txt <- mdt_inlines(blocks[[1]]$c, allow_br = allow_br)
      if (is.null(txt)) NA_character_ else txt
    }, character(1))
  })
}

# ---- random author text ------------------------------------------------------

# Characters that mean something in Markdown, GFM or pandoc, and line starts
# that would open a list, quote, heading, code block, table or rule.
mdt_specials <- c(
  "*", "_", "`", "|", "<", ">", "[", "]", "#", "!", "~", "=", "$", "&", "\\",
  "{", "}", "(", ")", "-", "+", ":", "^", "%", ".", "\"", "'"
)
mdt_words <- c("alpha", "Beta", "x1", "caf\u00e9", "99", "<b>", "&amp;", "\\n", "1.", "2)")
mdt_starts <- c(
  "", "", "- ", "1. ", "> ", "# ", "## ", "    ", "\t", "+ ", "* ", "2) ", "---", "===",
  "```", "~~~", "|", "<div>", "[x]: y", "  ", "- - -", "* * *", "___", "$$", ":::", "10. ",
  "1.", "- [ ] ", "<!-- ", "\u00a0"
)

mdt_line <- function() {
  n <- sample(1:5, 1)
  tokens <- sample(c(mdt_specials, mdt_words), n, replace = TRUE)
  seps <- sample(c("", " ", "  "), n, replace = TRUE)
  line <- paste0(sample(mdt_starts, 1), paste0(tokens, seps, collapse = ""), tokens[1])
  line
}

# Several lines, never an empty one (that would end the paragraph).
mdt_text <- function() {
  paste(vapply(seq_len(sample(1:4, 1)), function(i) mdt_line(), character(1)), collapse = "\n")
}

# Lines exactly as a reader sees them: indentation and trailing blanks go.
mdt_expect_text <- function(text) {
  mdt_norm(text)
}

# ---- .md_escape ---------------------------------------------------------------

test_that(".md_escape neutralises every Markdown and HTML trigger", {
  expect_identical(.md_escape("a*b_c"), "a\\*b\\_c")
  expect_identical(.md_escape("`code`"), "\\`code\\`")
  expect_identical(.md_escape("$5 and $6"), "\\$5 and \\$6")
  expect_identical(.md_escape("x | y"), "x \\| y")
  # "&" is escaped only where it would start an entity reference.
  expect_identical(.md_escape("5 < 6 & 7 > 3"), "5 &lt; 6 & 7 &gt; 3")
  expect_identical(.md_escape("R&D &amp; &#169; &x;"), "R&D \\&amp; \\&\\#169; \\&x;")
  expect_identical(.md_escape("[a](b) ![c](d)"), "\\[a\\](b) \\!\\[c\\](d)")
  # "=" means nothing inside a line.
  expect_identical(.md_escape("a~~b~~ c==d"), "a\\~\\~b\\~\\~ c==d")
  expect_identical(.md_escape("back\\slash"), "back\\\\slash")
  expect_identical(.md_escape("{x}"), "\\{x\\}")
})

test_that(".md_escape protects line starts that would change the structure", {
  expect_identical(
    .md_escape("- a\n+ b\n1. c\n2) d\n> e\n# f"),
    "\\- a\n\\+ b\n1\\. c\n2\\) d\n&gt; e\n\\# f"
  )
  # A list marker with nothing after it still opens an (empty) list item.
  expect_identical(.md_escape("1."), "1\\.")
  expect_identical(.md_escape("a\n12)"), "a\n12\\)")
  # Rules and Setext underlines.
  expect_identical(.md_escape("a\n---"), "a\n\\---")
  expect_identical(.md_escape("a\n==="), "a\n\\===")
  expect_identical(.md_escape("a\n=== x"), "a\n=== x")
  # Emoji short codes would be replaced by the emoji; a time is no short code.
  expect_identical(.md_escape(":smile: x"), "\\:smile: x")
  expect_identical(.md_escape(":+1: :100: :8ball:"), "\\:+1: \\:100: \\:8ball:")
  expect_identical(.md_escape("at 07:45:59 and 1:2:3"), "at 07:45:59 and 1:2:3")
  # Indentation is kept visible (non-breaking spaces), not turned into code.
  expect_identical(.md_escape("    code"), "\u00a0\u00a0\u00a0\u00a0code")
})

test_that(".md_escape handles empty, NA, NULL and vector input", {
  expect_identical(.md_escape(NULL), character(0))
  expect_identical(.md_escape(character(0)), character(0))
  expect_identical(.md_escape(NA_character_), "")
  expect_identical(.md_escape(""), "")
  expect_identical(.md_escape(c("a_b", NA, "c")), c("a\\_b", "", "c"))
  expect_identical(.md_escape(12.5), "12.5")
  # CRLF and CR end lines like LF does.
  expect_identical(.md_escape("a\r\n- b\r- c"), "a\n\\- b\n\\- c")
  # Trailing blanks (a hard break in Markdown) are dropped.
  expect_identical(.md_escape("a  \nb \t"), "a\nb")
})

test_that(".md_escape(table = TRUE) gives a single-line, indentation-free cell", {
  expect_identical(.md_escape("  - a\n\tb", table = TRUE), "\\- a<br>b")
  expect_identical(.md_escape("a|b", table = TRUE), "a\\|b")
  expect_identical(.md_escape("a\n\nb", table = TRUE), "a<br><br>b")
  expect_identical(.md_escape("\n\n x \n\n", table = TRUE), "x")
  expect_identical(.md_escape("   ", table = TRUE), "")
  expect_identical(.md_escape(NA, table = TRUE), "")
  expect_false(grepl("\u00a0", .md_escape("      deep", table = TRUE)))
  expect_false(grepl("\n", .md_escape("a\nb\r\nc", table = TRUE)))
})

test_that("escaped paragraphs survive pandoc as exactly the author's text", {
  mdt_skip_without_pandoc()
  withr::local_seed(20240607)
  texts <- c(
    vapply(seq_len(160), function(i) mdt_text(), character(1)),
    # hand-picked worst cases
    "- item", "1. item", "> quote", "# heading", "    code", "\ttab code", "---", "===",
    "```", "~~~", "|a|b|\n|-|-|", "a\n---", "a\n===", "<div>x</div>", "<!-- c -->",
    "[x]: example", "![img](a.png)", "*em* **strong** ~~del~~ `code`",
    "$x$ and $$y$$", "1.", "- [ ] task", "\\", "a \\\nb", "&copy; &#169; &lt;",
    "* * *", "- - -", "___", ":::", "|", "A | B", "\\*not em\\*", "<script>alert(1)</script>"
  )
  doc <- unlist(lapply(texts, function(t) c(.md_escape(t), "")))
  blocks <- mdt_ast(doc)
  expect_length(blocks, length(texts))
  for (i in seq_along(texts)) {
    expect_identical(blocks[[i]]$t, "Para", info = sprintf("text %d: %s", i, deparse(texts[i])))
    shown <- mdt_inlines(blocks[[i]]$c)
    expect_false(is.null(shown), info = sprintf("text %d: %s", i, deparse(texts[i])))
    expect_identical(mdt_norm(shown), mdt_expect_text(texts[i]),
      info = sprintf("text %d: %s", i, deparse(texts[i]))
    )
  }
})

test_that("escaped text inside a sentence keeps the surrounding structure", {
  mdt_skip_without_pandoc()
  withr::local_seed(11)
  texts <- vapply(seq_len(40), function(i) mdt_text(), character(1))
  # The same text as a heading, a bullet and a block that is followed by a rule.
  doc <- unlist(lapply(texts, function(t) {
    flat <- gsub("\n", " ", t)
    c(paste("##", .md_escape(flat)), "", .md_list(t), "", .md_escape(t), "", "---", "")
  }))
  blocks <- mdt_ast(doc)
  types <- vapply(blocks, function(b) b$t, character(1))
  expect_identical(types, rep(c("Header", "BulletList", "Para", "HorizontalRule"), length(texts)))
  for (i in seq_along(texts)) {
    header <- blocks[[4 * (i - 1) + 1]]
    expect_identical(header$c[[1]], 2L)
    expect_identical(
      mdt_norm(mdt_inlines(header$c[[3]])),
      mdt_norm(gsub("\n", " ", texts[i])),
      info = sprintf("heading %d", i)
    )
  }
})

test_that("emoji short codes stay text; autolinks keep their visible text", {
  mdt_skip_without_pandoc()
  blocks <- mdt_ast(c(.md_escape("A :smile: and :+1: and :tada:"), "", .md_escape("see www.example.org now")))
  expect_identical(mdt_inlines(blocks[[1]]$c), "A :smile: and :+1: and :tada:")
  # GFM turns bare web addresses into links; the text the reader sees is unchanged.
  texts <- unlist(rapply(blocks[[2]]$c, function(x) x, classes = "character", how = "unlist"))
  shown <- paste(texts[!texts %in% c("Str", "Space", "Link", "Plain", "Para", "Code")], collapse = "")
  expect_true(grepl("www.example.org", shown, fixed = TRUE))
})

# ---- .md_table ----------------------------------------------------------------

test_that(".md_table writes header, delimiter and rows with exact escaping", {
  df <- data.frame(
    Name = c("a|b", "x\n  y", "$5"),
    Role = c("", "r", NA),
    stringsAsFactors = FALSE
  )
  expect_identical(.md_table(df), c(
    "| Name | Role |",
    "| --- | --- |",
    "| a\\|b |  |",
    "| x<br>y | r |",
    "| \\$5 |  |"
  ))
})

test_that(".md_table honours alignment and rejects bad input", {
  df <- data.frame(A = "1", B = "2", C = "3")
  expect_identical(.md_table(df, align = c("left", "right", "center"))[2], "| :--- | ---: | :---: |")
  expect_identical(.md_table(df, align = "right")[2], "| ---: | ---: | ---: |")
  expect_error(.md_table(df, align = c("left", "right")), "left.*right.*center")
  expect_error(.md_table(df, align = "middle"), "left.*right.*center")
  expect_error(.md_table(list(1, 2)), "data frame")
})

test_that(".md_table escapes the header and flattens factor, number and list cells", {
  df <- data.frame(`A|B` = factor(c("u", "v")), N = c(1.5, NA), check.names = FALSE)
  df$L <- list(c("p", "q"), character(0))
  expect_identical(.md_table(df), c(
    "| A\\|B | N | L |",
    "| --- | --- | --- |",
    "| u | 1.5 | p, q |",
    "| v |  |  |"
  ))
})

test_that(".md_table of an empty table is nothing", {
  expect_identical(.md_table(data.frame(A = character(0))), character(0))
  expect_identical(.md_table(data.frame()), character(0))
  expect_identical(.md_table(data.frame(row.names = 1:3)), character(0))
})

test_that("tables with hostile cells keep their shape in pandoc", {
  mdt_skip_without_pandoc()
  withr::local_seed(20240608)
  tables <- lapply(seq_len(60), function(i) {
    n_col <- sample(1:4, 1)
    n_row <- sample(1:5, 1)
    cells <- matrix(
      vapply(seq_len(n_col * n_row), function(k) {
        if (runif(1) < 0.15) "" else mdt_text()
      }, character(1)),
      nrow = n_row
    )
    df <- as.data.frame(cells, stringsAsFactors = FALSE)
    names(df) <- vapply(seq_len(n_col), function(k) mdt_line(), character(1))
    df
  })
  doc <- unlist(lapply(tables, function(df) c(.md_table(df), "", "separator", "")))
  blocks <- mdt_ast(doc)
  expect_length(blocks, 2 * length(tables))
  for (i in seq_along(tables)) {
    df <- tables[[i]]
    block <- blocks[[2 * i - 1]]
    info <- sprintf("table %d (%d x %d)", i, nrow(df), ncol(df))
    expect_identical(block$t, "Table", info = info)
    expect_identical(blocks[[2 * i]]$t, "Para", info = info)
    expect_length(block$c[[3]], ncol(df)) # column specs
    rows <- mdt_table(block)
    expect_length(rows, nrow(df) + 1L)
    expect_true(all(lengths(rows) == ncol(df)), info = info)
    expected_head <- vapply(names(df), function(h) mdt_norm(h), character(1), USE.NAMES = FALSE)
    expect_identical(mdt_norm(rows[[1]]), expected_head, info = info)
    for (r in seq_len(nrow(df))) {
      expected <- vapply(df[r, ], mdt_norm, character(1), USE.NAMES = FALSE)
      expect_identical(mdt_norm(rows[[r + 1L]]), expected, info = paste(info, "row", r))
    }
  }
})

test_that("every line of a table has the same number of unescaped pipes", {
  withr::local_seed(5)
  for (i in 1:30) {
    n_col <- sample(1:5, 1)
    df <- as.data.frame(
      matrix(vapply(seq_len(n_col * 3), function(k) mdt_text(), character(1)), nrow = 3),
      stringsAsFactors = FALSE
    )
    lines <- .md_table(df)
    expect_length(lines, 5L)
    # Count pipes that are not escaped by an odd run of backslashes.
    pipes <- vapply(lines, function(l) {
      lengths(regmatches(l, gregexpr("(?<!\\\\)(\\\\\\\\)*\\|", l, perl = TRUE)))
    }, integer(1))
    expect_true(all(pipes == n_col + 1L), info = paste(lines, collapse = "\n"))
    expect_false(any(grepl("\n", lines)))
  }
})

# ---- .md_kv_table / .md_list ----------------------------------------------------

test_that(".md_kv_table accepts the three pair shapes and escapes both columns", {
  expected <- c("| Item | Details |", "| --- | --- |", "| Study ID | A_1 |", "| Note | a\\|b |")
  expect_identical(.md_kv_table(c(`Study ID` = "A_1", Note = "a|b")), sub("A_1", "A\\\\_1", expected))
  expect_identical(
    .md_kv_table(list(`Study ID` = "A_1", Note = "a|b")),
    sub("A_1", "A\\\\_1", expected)
  )
  expect_identical(
    .md_kv_table(data.frame(Item = c("Study ID", "Note"), Details = c("A_1", "a|b"))),
    sub("A_1", "A\\\\_1", expected)
  )
  expect_identical(
    .md_kv_table(list(list(key = "Study ID", value = "A_1"), list(key = "Note", value = "a|b"))),
    sub("A_1", "A\\\\_1", expected)
  )
  # Multi-valued, missing and NULL values do not break the two-column shape.
  expect_identical(
    .md_kv_table(list(K = c("x", "y"), M = NA_character_, N = NULL))[3:5],
    c("| K | x, y |", "| M |  |", "| N |  |")
  )
})

test_that(".md_kv_table without pairs is nothing and unnamed input is rejected", {
  expect_identical(.md_kv_table(NULL), character(0))
  expect_identical(.md_kv_table(list()), character(0))
  expect_identical(.md_kv_table(character(0)), character(0))
  expect_error(.md_kv_table(c("a", "b")), "named vector")
})

test_that(".md_list writes escaped bullets and hard breaks", {
  expect_identical(.md_list(c("one", "- two", "a_b")), c("- one", "- \\- two", "- a\\_b"))
  expect_identical(.md_list(c("x\ny\n\nz")), c("- x\\", "  y\\", "  z"))
  expect_identical(.md_list(c("one", NA, "", "  ", "two")), c("- one", "- two"))
  expect_identical(.md_list(list("a", "b")), c("- a", "- b"))
  expect_identical(.md_list(NULL), character(0))
  expect_identical(.md_list(character(0)), character(0))
  expect_identical(.md_list(c(NA, "")), character(0))
  expect_identical(.md_list("**bold** text", escape = FALSE), "- **bold** text")
})

test_that("list items from random text stay one item each in pandoc", {
  mdt_skip_without_pandoc()
  withr::local_seed(77)
  items <- vapply(seq_len(50), function(i) mdt_text(), character(1))
  blocks <- mdt_ast(.md_list(items))
  expect_length(blocks, 1L)
  expect_identical(blocks[[1]]$t, "BulletList")
  expect_length(blocks[[1]]$c, length(items))
  for (i in seq_along(items)) {
    item <- blocks[[1]]$c[[i]]
    expect_length(item, 1L)
    shown <- mdt_inlines(item[[1]]$c, allow_break = TRUE)
    expect_false(is.null(shown), info = deparse(items[i]))
    expect_identical(mdt_norm(shown), mdt_norm(items[i]), info = deparse(items[i]))
  }
})

# ---- .md_compose -------------------------------------------------------------

test_that(".md_compose joins blocks with exactly one blank line", {
  expect_identical(
    .md_compose("a", c("b", "c"), character(0), NULL, "d"),
    c("a", "", "b", "c", "", "d")
  )
  expect_identical(
    .md_compose(c("", "a", "", ""), c("", "", "b")),
    c("a", "", "b")
  )
  # Runs of blank lines inside a block collapse; blanks-only lines are empty.
  expect_identical(.md_compose(c("a", "", " \t", "", "b")), c("a", "", "b"))
  # Embedded line breaks are split into lines.
  expect_identical(.md_compose("a\nb", "c\r\nd"), c("a", "b", "", "c", "d"))
  expect_identical(.md_compose("a", "", "b", c("c", "", "", "d")), c("a", "", "b", "", "c", "", "d"))
})

test_that(".md_compose never leaves two blank lines in a row", {
  withr::local_seed(3)
  for (i in 1:50) {
    blocks <- lapply(1:6, function(k) {
      sample(c("x", "", " ", "-", "---", "y"), sample(0:5, 1), replace = TRUE)
    })
    out <- do.call(.md_compose, blocks)
    if (length(out)) {
      expect_true(nzchar(out[1]) && nzchar(out[length(out)]))
      expect_false(any(out[-1] == "" & out[-length(out)] == ""))
    }
  }
})

test_that(".md_compose empty input is nothing", {
  expect_identical(.md_compose(), character(0))
  expect_identical(.md_compose(NULL, character(0), ""), character(0))
})

test_that("a rule after text is never read as a Setext heading underline", {
  mdt_skip_without_pandoc()
  # The trap itself: without the blank line a heading appears.
  trap <- mdt_ast(c("Some text", "---"))
  expect_identical(trap[[1]]$t, "Header")
  # Between blocks, compose puts the blank line in.
  safe <- mdt_ast(.md_compose("Some text", c("---", "Footer line")))
  expect_identical(vapply(safe, function(b) b$t, character(1)), c("Para", "HorizontalRule", "Para"))
  # Inside one block it separates the rule from the text above.
  inside <- mdt_ast(.md_compose(c("Some text", "---", "Footer line")))
  expect_identical(vapply(inside, function(b) b$t, character(1)), c("Para", "HorizontalRule", "Para"))
  inside_eq <- mdt_ast(.md_compose(c("Some text", "===")))
  expect_false("Header" %in% vapply(inside_eq, function(b) b$t, character(1)))
  # A table delimiter row is not a rule and is left alone.
  expect_identical(.md_compose(c("| A |", "| --- |", "| x |")), c("| A |", "| --- |", "| x |"))
})

# ---- .md_slug ------------------------------------------------------------------

test_that(".md_slug follows the GitHub algorithm (hand-written expectations)", {
  slug <- function(x) .md_slug(x)$slug
  expect_identical(slug("Hello World"), "hello-world")
  expect_identical(slug("A & B / C"), "a--b--c")
  expect_identical(slug("2.1 Supplier"), "21-supplier")
  expect_identical(slug("Data-Set_1"), "data-set_1")
  expect_identical(slug("What's new? (really) [x]"), "whats-new-really-x")
  expect_identical(slug("100% done!"), "100-done")
  expect_identical(slug("C++ & C#"), "c--c")
  expect_identical(slug("Caf\u00e9 \u00dcberblick"), "caf\u00e9-\u00fcberblick")
  expect_identical(slug("Stra\u00dfe"), "stra\u00dfe")
  expect_identical(slug("\u65e5\u672c\u8a9e \u03a9"), "\u65e5\u672c\u8a9e-\u03c9")
  expect_identical(slug("\u00bd of 3\u00b2"), "\u00bd-of-3\u00b2")
  # GitHub keeps every space as one hyphen and drops a tab (pandoc collapses
  # runs of white space instead).
  expect_identical(slug("a  b"), "a--b")
  expect_identical(slug("tab\tx"), "tabx")
  expect_identical(slug(" lead"), "-lead")
  expect_identical(slug("***"), "")
})

test_that(".md_slug numbers duplicates and returns the updated list", {
  first <- .md_slug("Supplier")
  expect_identical(first, list(slug = "supplier", used = "supplier"))
  second <- .md_slug("Supplier", first$used)
  expect_identical(second, list(slug = "supplier-1", used = c("supplier", "supplier-1")))
  third <- .md_slug("supplier", second$used)
  expect_identical(third$slug, "supplier-2")
  expect_identical(third$used, c("supplier", "supplier-1", "supplier-2"))
  # A heading that is literally "Supplier-1" after the generated one gets a suffix too.
  expect_identical(.md_slug("Supplier-1", second$used)$slug, "supplier-1-1")
  # `used` is a set of anchors, other texts do not interfere.
  expect_identical(.md_slug("Other", second$used)$slug, "other")
  expect_error(.md_slug(c("a", "b")), "single string")
  expect_error(.md_slug(NA_character_), "single string")
})

test_that(".md_slug agrees with the anchors pandoc computes for gfm headings", {
  mdt_skip_without_pandoc()
  titles <- c(
    "Supplier", "Supplier", "Supplier", "A & B / C", "Caf\u00e9 \u00dcberblick",
    "Stra\u00dfe", "Data-Set_1", "What's new? (really) [x]", "2.1 Supplier", "100% done!",
    "\u65e5\u672c\u8a9e \u03a9", "C++ & C#", "\u00bd of 3\u00b2", "\"Quoted\" text", "foo (bar)",
    "1.2.3 Version", "under__score", "\u00c0\u00c9\u00ce \u0152uvre", "x\u2011y \u2013 z",
    "Receiver", "Receiver", "e.g. i.e. etc.", "Q&A: how/why?", "a.b:c;d,e", "\u20ac100 + 50%",
    "Datasets", "Data sets"
  )
  blocks <- mdt_ast(paste("##", vapply(titles, .md_escape, character(1))))
  expect_length(blocks, length(titles))
  used <- character()
  for (i in seq_along(titles)) {
    got <- .md_slug(titles[i], used)
    used <- got$used
    expect_identical(blocks[[i]]$c[[2]][[1]], got$slug, info = titles[i])
  }
})

# ---- sections, numbering, contents -----------------------------------------------

mdt_headers <- function(blocks) {
  Filter(function(b) b$t == "Header", blocks)
}

mdt_header_text <- function(h) mdt_inlines(h$c[[3]])

mdt_sample_sections <- function() {
  list(
    .md_section("Approval & Signatures", "Body one"),
    .md_section("Skipped", character(0), list(.md_section("Also empty"))),
    .md_section("Parties", character(0), list(
      .md_section("Supplier", "supplier text"),
      .md_section("Receiver"),
      .md_section("Third_party [x]", c("t"), list(
        .md_section("Detail", "deep"),
        .md_section("Nothing")
      ))
    )),
    .md_section("Datasets", c("d"), list(.md_section("Files", "f")))
  )
}

test_that(".md_render numbers chapters, drops empty ones and closes the gaps", {
  out <- .md_render(c("# Doc", "", "intro"), mdt_sample_sections(), c("---", "end"))
  headings <- grep("^#{1,6} ", out, value = TRUE)
  expect_identical(headings, c(
    "# Doc",
    "## Contents",
    "## 1 Approval & Signatures",
    "## 2 Parties",
    "### 2.1 Supplier",
    "### 2.2 Third\\_party \\[x\\]",
    "#### 2.2.1 Detail",
    "## 3 Datasets",
    "### 3.1 Files"
  ))
  expect_identical(out[1:3], c("# Doc", "", "intro"))
  expect_identical(utils::tail(out, 2), c("---", "end"))
})

test_that(".md_render lists levels 1 and 2 in Contents", {
  out <- .md_render("# Doc", mdt_sample_sections())
  start <- which(out == "## Contents")
  toc <- out[(start + 2):(which(out == "## 1 Approval & Signatures") - 2)]
  expect_identical(toc, c(
    "- [1 Approval & Signatures](#1-approval--signatures)",
    "- [2 Parties](#2-parties)",
    "  - [2.1 Supplier](#21-supplier)",
    "  - [2.2 Third\\_party \\[x\\]](#22-third_party-x)",
    "- [3 Datasets](#3-datasets)",
    "  - [3.1 Files](#31-files)"
  ))
})

test_that(".md_render without any content writes only head and footer", {
  expect_identical(.md_render(c("# Doc", "text"), list(), c("---", "f")), c("# Doc", "text", "", "---", "f"))
  expect_identical(.md_render(character(0), list(.md_section("Empty")), character(0)), character(0))
  expect_false("## Contents" %in% .md_render("# Doc", list(.md_section("Empty"))))
  expect_identical(.md_render(), character(0))
})

test_that(".md_section and .md_render reject bad structure", {
  expect_error(.md_section(""), "non-empty")
  expect_error(.md_section(c("a", "b")), "non-empty")
  expect_error(.md_section("  \n "), "non-empty")
  expect_error(.md_section("A", children = list("x")), "md_section")
  deep <- .md_section("L1", children = list(.md_section("L2", children = list(
    .md_section("L3", children = list(.md_section("L4", "x")))
  ))))
  expect_error(.md_render("# D", list(deep)), "three levels")
  ok <- .md_section("L1", children = list(.md_section("L2", children = list(.md_section("L3", "x")))))
  expect_true("#### 1.1.1 L3" %in% .md_render("# D", list(ok)))
})

test_that("headings with line breaks or spaces collapse to one line", {
  out <- .md_render("# D", list(.md_section("Two\n  words\tand   more", "x"), .md_section("B", "y")))
  expect_true("## 1 Two words and more" %in% out)
  expect_true(any(grepl("(#1-two-words-and-more)", out, fixed = TRUE)))
})

test_that("Contents links hit the anchors pandoc assigns to the rendered file", {
  mdt_skip_without_pandoc()
  hostile <- c(
    "Data & Sets / Q&A", "Caf\u00e9: \u00dcberblick", "Third_party [x] (y)", "100% $5 #1!",
    "Supplier", "Supplier", "`code` *em* ~~s~~"
  )
  sections <- c(
    list(.md_section("Intro", "i", list(.md_section("A", "a")))),
    lapply(hostile, function(t) .md_section(t, "text", list(.md_section(t, "child")))),
    list(.md_section("Eleven", "e")),
    list(.md_section("A", "second A at number 10 or later")),
    lapply(1:3, function(k) .md_section("Tail", "t"))
  )
  out <- .md_render(c("# 1 Intro", "", "lead"), sections, c("---", "end"))
  blocks <- mdt_ast(out)

  headers <- mdt_headers(blocks)
  ids <- vapply(headers, function(h) h$c[[2]][[1]], character(1))
  expect_false(anyDuplicated(ids) > 0L)

  # Every link in the Contents list leads to a heading with the same text.
  lists <- Filter(function(b) b$t == "BulletList", blocks)
  expect_length(lists, 1L)
  collect <- function(items) {
    unlist(lapply(items, function(item) {
      c(
        list(item[[1]]$c[[1]]),
        if (length(item) > 1L) collect(item[[2]]$c) else NULL
      )
    }), recursive = FALSE)
  }
  links <- collect(lists[[1]]$c)
  expect_true(length(links) >= 10L)
  heading_by_id <- setNames(lapply(headers, mdt_header_text), ids)
  for (link in links) {
    expect_identical(link$t, "Link")
    target <- sub("^#", "", link$c[[3]][[1]])
    expect_true(target %in% ids, info = target)
    expect_identical(mdt_inlines(link$c[[2]]), heading_by_id[[target]], info = target)
  }
  # Levels 1 and 2 only: nothing deeper, nothing missing.
  levels <- vapply(headers, function(h) h$c[[1]], integer(1))
  texts <- vapply(headers, mdt_header_text, character(1))
  in_toc <- vapply(links, function(l) mdt_inlines(l$c[[2]]), character(1))
  expected_toc <- texts[levels %in% c(2L, 3L) & !texts %in% "Contents" & grepl("^[0-9]", texts)]
  expect_identical(in_toc, expected_toc)
  # The heading of the head ("1 Intro") and chapter 1 share their anchor base.
  expect_true("1-intro" %in% ids && "1-intro-1" %in% ids)
})

test_that("headings in the rendered file have the numbers that were promised", {
  mdt_skip_without_pandoc()
  blocks <- mdt_ast(.md_render("# Doc", mdt_sample_sections(), c("---", "end")))
  texts <- vapply(mdt_headers(blocks), mdt_header_text, character(1))
  expect_identical(texts, c(
    "Doc", "Contents", "1 Approval & Signatures", "2 Parties", "2.1 Supplier",
    "2.2 Third_party [x]", "2.2.1 Detail", "3 Datasets", "3.1 Files"
  ))
  last <- blocks[[length(blocks)]]
  expect_identical(last$t, "Para")
  expect_identical(blocks[[length(blocks) - 1L]]$t, "HorizontalRule")
})

test_that("a heading in the head takes its anchor before the chapters", {
  two <- function(first) list(.md_section(first, "x"), .md_section("Other", "y"))
  out <- .md_render(c("# 1 Parties"), two("Parties"))
  expect_true("- [1 Parties](#1-parties-1)" %in% out)
  out2 <- .md_render(c("# Contents"), two("Parties"))
  expect_true("- [1 Parties](#1-parties)" %in% out2)
})

# ---- .md_write -----------------------------------------------------------------------

test_that(".md_write writes UTF-8, LF, one final newline and no trailing blanks", {
  f <- withr::local_tempfile(fileext = ".md")
  .md_write(c("caf\u00e9  ", "x\t", "", "", "end \u00a0", "", ""), f)
  bytes <- readBin(f, "raw", file.size(f))
  expected <- charToRaw("caf\u00c3\u00a9\nx\n\n\nend \u00c2\u00a0\n")
  expected <- c(
    charToRaw("caf"), as.raw(c(0xc3, 0xa9)), charToRaw("\nx\n\n\nend "),
    as.raw(c(0xc2, 0xa0)), charToRaw("\n")
  )
  expect_identical(bytes, expected)
})

test_that(".md_write turns CR and CRLF into LF and leaves no CR", {
  f <- withr::local_tempfile(fileext = ".md")
  .md_write(c("a\r\nb\rc", "d\r\n"), f)
  bytes <- readBin(f, "raw", file.size(f))
  expect_false(as.raw(13) %in% bytes)
  expect_identical(bytes, charToRaw("a\nb\nc\nd\n"))
})

test_that(".md_write ends with exactly one line ending", {
  f <- withr::local_tempfile(fileext = ".md")
  for (lines in list("x", c("x", ""), c("x", "", "", "  "), "x\n\n\n")) {
    .md_write(lines, f)
    expect_identical(readBin(f, "raw", file.size(f)), charToRaw("x\n"))
  }
  # Empty input is an empty file, not a stray newline.
  .md_write(character(0), f)
  expect_identical(file.size(f), 0)
  .md_write(c("", " "), f)
  expect_identical(file.size(f), 0)
})

test_that(".md_write replaces an existing file completely and leaves no staging file", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "out.md")
  writeLines(strrep("old content\n", 100), f)
  returned <- .md_write(c("new", "content"), f)
  expect_identical(returned, f)
  expect_identical(readLines(f, encoding = "UTF-8"), c("new", "content"))
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE), "out.md")
})

test_that(".md_write is deterministic across calls and locales", {
  dir <- withr::local_tempdir()
  lines <- .md_render(
    c("# Doc", "", .md_escape("Name: caf\u00e9 | $ & <b>")),
    mdt_sample_sections(), c("---", "Doc \u00b7 v1")
  )
  a <- file.path(dir, "a.md")
  b <- file.path(dir, "b.md")
  .md_write(lines, a)
  withr::with_locale(c(LC_TIME = "C"), .md_write(lines, b))
  bytes_a <- readBin(a, "raw", file.size(a))
  expect_identical(bytes_a, readBin(b, "raw", file.size(b)))
  expect_false(as.raw(13) %in% bytes_a)
  expect_identical(utils::tail(bytes_a, 1L), as.raw(10))
  text <- rawToChar(bytes_a)
  Encoding(text) <- "UTF-8"
  expect_true(validUTF8(text))
  expect_false(grepl("[ \t]\n", text))
  expect_false(grepl("\n\n\n", text, fixed = TRUE))
  expect_false(grepl("Generated", text, fixed = TRUE))
  expect_false(grepl("[0-9]{2}:[0-9]{2}:[0-9]{2}", text))
})

test_that(".md_write validates its arguments", {
  expect_error(.md_write("x", c("a", "b")), "single file path")
  expect_error(.md_write("x", NA_character_), "single file path")
  expect_error(.md_write("x", ""), "single file path")
  bad <- rawToChar(as.raw(c(0x61, 0xff, 0x62)))
  expect_error(.md_write(bad, withr::local_tempfile(fileext = ".md")), "UTF-8")
})

test_that("a whole document round-trips through the file and pandoc", {
  mdt_skip_without_pandoc()
  f <- withr::local_tempfile(fileext = ".md")
  lines <- .md_render(
    c("# Doc", "", "**Study** one"),
    list(
      .md_section("Info", c(
        .md_kv_table(c(`Study ID` = "S_1", Note = "a|b $5")),
        "", .md_list(c("one", "- two"))
      )),
      .md_section("Empty"),
      .md_section("Data", .md_table(data.frame(A = c("x", "y"), B = c("1", "2"))))
    ),
    c("---", "Doc")
  )
  .md_write(lines, f)
  back <- readLines(f, encoding = "UTF-8", warn = FALSE)
  expect_identical(back, lines)
  blocks <- mdt_ast(back)
  types <- vapply(blocks, function(b) b$t, character(1))
  expect_identical(types, c(
    "Header", "Para", "Header", "BulletList", "Header", "Table", "BulletList",
    "Header", "Table", "HorizontalRule", "Para"
  ))
  kv <- mdt_table(blocks[[6]])
  expect_identical(kv[[3]], c("Note", "a|b $5"))
  expect_identical(kv[[2]], c("Study ID", "S_1"))
})

# ---- web and e-mail addresses ------------------------------------------------

# Every pandoc on the machine that can read gfm: the addresses must survive
# both older (3.1) and newer (3.13+) readers, which treat bare addresses
# differently.
mdt_pandocs <- function() {
  found <- unique(normalizePath(
    Filter(nzchar, c(Sys.which("pandoc"), "/usr/bin/pandoc", "/opt/r45/bin/pandoc", "/usr/local/bin/pandoc")),
    mustWork = FALSE
  ))
  found[file.exists(found)]
}

mdt_ast_with <- function(pandoc, lines) {
  inp <- tempfile(fileext = ".md")
  out <- tempfile(fileext = ".json")
  on.exit(unlink(c(inp, out), force = TRUE), add = TRUE)
  writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\n"), "\n"))), inp)
  status <- system2(pandoc, c("-f", "gfm", "-t", "json", "-o", out, inp), stdout = FALSE, stderr = FALSE)
  if (!identical(status, 0L)) stop("pandoc failed")
  jsonlite::fromJSON(paste(readLines(out, encoding = "UTF-8", warn = FALSE), collapse = "\n"),
    simplifyVector = FALSE
  )$blocks
}

# Links (target and shown text) in a list of inlines, in order.
mdt_links <- function(x) {
  out <- list()
  walk <- function(node) {
    if (is.list(node) && identical(node$t, "Link")) {
      out[[length(out) + 1L]] <<- list(target = node$c[[3]][[1]], inlines = node$c[[2]])
    } else if (is.list(node)) {
      for (child in node) walk(child)
    }
  }
  walk(x)
  out
}

# The text a reader sees for a list of inlines (Str/Space, through spans and
# links).
mdt_visible <- function(inl) {
  parts <- character(0)
  walk <- function(node) {
    if (!is.list(node)) {
      return()
    }
    if (!is.null(node$t)) {
      if (node$t == "Str") parts <<- c(parts, node$c)
      if (node$t %in% c("Space", "SoftBreak")) parts <<- c(parts, " ")
      if (node$t == "Link") walk(node$c[[2]])
      if (node$t == "Span") walk(node$c[[2]])
      return()
    }
    for (child in node) walk(child)
  }
  walk(inl)
  paste(parts, collapse = "")
}

test_that("URLs and e-mail addresses become links that are not escaped", {
  expect_identical(.md_escape("Mail john_doe@ex-ample.org."), "Mail <john_doe@ex-ample.org>.")
  expect_identical(
    .md_escape("see https://example.org/my_file.csv?a=1&b=2, ok"),
    "see <https://example.org/my_file.csv?a=1&b=2>, ok"
  )
  # A closing parenthesis without its partner belongs to the sentence.
  expect_identical(.md_escape("(https://x.org/a_(b))"), "(<https://x.org/a_(b)>)")
  # www. has no scheme, which an autolink needs: linked as http://www. ...
  expect_identical(.md_escape("www.ex.org/a_b."), "<http://www.ex.org/a_b>.")
  # In a table cell "|" is escaped even inside the link, or it would split the cell.
  expect_identical(.md_escape("x|y https://a.org/p|q", table = TRUE), "x\\|y <https://a.org/p\\|q>")
  expect_identical(.md_escape("x https://a.org/p|q"), "x <https://a.org/p|q>")
  # Not an address: no scheme, no host, no valid domain.
  expect_identical(.md_escape("http:// and a@b and a@b_c.org"), "http:// and a@b and a@b\\_c.org")
  # links = FALSE escapes addresses like other text (text that is itself a link).
  expect_identical(.md_escape("a_b@x.org", links = FALSE), "a\\_b@x.org")
})

test_that("pandoc links every address to exactly the author's text (every pandoc found)", {
  pandocs <- mdt_pandocs()
  skip_if(length(pandocs) == 0L, "pandoc is not installed")
  texts <- c(
    "Mail john_doe@ex-ample.org now.",
    "Get https://example.org/my_file.csv?a=1&b=2&amp;c=*x*_y here",
    "(see http://ex.org/a_(b)_c), then ftp://files.ex.org/x~y/.",
    "Visit www.ex-ample.org/path_one/two_three.",
    "Pipes: https://a.org/p|q and first.last+tag@sub.ex.org"
  )
  targets <- list(
    "mailto:john_doe@ex-ample.org",
    "https://example.org/my_file.csv?a=1&b=2&amp;c=*x*_y",
    c("http://ex.org/a_(b)_c", "ftp://files.ex.org/x~y/"),
    "http://www.ex-ample.org/path_one/two_three",
    c("https://a.org/p|q", "mailto:first.last+tag@sub.ex.org")
  )
  for (pandoc in pandocs) {
    for (as_table in c(FALSE, TRUE)) {
      doc <- if (as_table) {
        .md_table(data.frame(Text = texts, Other = "x"))
      } else {
        unlist(lapply(texts, function(t) c(.md_escape(t), "")))
      }
      blocks <- mdt_ast_with(pandoc, doc)
      paragraphs <- if (as_table) {
        rows <- blocks[[1]]$c[[5]][[1]][[4]]
        lapply(rows, function(row) row[[2]][[1]][[5]][[1]]$c)
      } else {
        lapply(blocks, function(b) b$c)
      }
      expect_length(paragraphs, length(texts))
      for (i in seq_along(texts)) {
        info <- sprintf("%s, table = %s: %s", pandoc, as_table, texts[i])
        got <- vapply(mdt_links(paragraphs[[i]]), function(l) l$target, character(1))
        expect_identical(got, targets[[i]], info = info)
        # What the reader sees is the author's text; an address starting with
        # "www." is shown with the scheme it is linked with.
        expect_identical(mdt_visible(paragraphs[[i]]), sub("www.", "http://www.", texts[i], fixed = TRUE), info = info)
      }
    }
  }
})

# ---- escaping: minimal and readable ---------------------------------------------

test_that("entities, times and '=' lines are escaped only where they mean something", {
  mdt_skip_without_pandoc()
  texts <- c(
    "R&D & Co; &amp; &copy; &#169; &#x1F600; &nosuch; & ;", "07:45:59 and 12:00:00",
    "a\n===", "a\n= = =", "c==d and ==x==", ":smile: :+1: :100: :1234: :8ball: :45:"
  )
  blocks <- mdt_ast(unlist(lapply(texts, function(t) c(.md_escape(t), ""))))
  expect_length(blocks, length(texts))
  for (i in seq_along(texts)) {
    expect_identical(blocks[[i]]$t, "Para", info = texts[i])
    expect_identical(mdt_norm(mdt_inlines(blocks[[i]]$c)), mdt_norm(texts[i]), info = texts[i])
  }
  # Readable source: no escape where none is needed.
  expect_identical(.md_escape("07:45:59"), "07:45:59")
  expect_identical(.md_escape("Q&A"), "Q&A")
})

test_that("control characters become spaces", {
  expect_identical(.md_lines("a\ab\vc\fd\033e"), "a b c d e")
  expect_identical(.md_escape("bell\a and\u0085next"), "bell  and next")
  expect_identical(.md_lines("tab\tstays"), "tab\tstays")
})

# ---- anchors in any locale ---------------------------------------------------------

test_that(".md_slug lower-cases non-ASCII letters whatever the locale", {
  withr::local_locale(c(LC_CTYPE = "C"))
  expect_identical(.md_slug("\u00dcbersicht Labor")$slug, "\u00fcbersicht-labor")
  expect_identical(.md_slug("\u00c9TAPE \u00c0 \u03a9")$slug, "\u00e9tape-\u00e0-\u03c9")
  out <- .md_render("# D", list(.md_section("\u00dcbersicht Labor", "x"), .md_section("B", "y")))
  expect_true("- [1 \u00dcbersicht Labor](#1-\u00fcbersicht-labor)" %in% out)
})

# ---- contents threshold, unnumbered headings ------------------------------------------

test_that("Contents appears only with at least two level-1 chapters", {
  one <- .md_render("# D", list(.md_section("Only", "x", list(.md_section("A", "a"), .md_section("B", "b")))))
  expect_false("## Contents" %in% one)
  expect_identical(grep("^#", one, value = TRUE), c("# D", "## 1 Only", "### 1.1 A", "### 1.2 B"))
  two <- .md_render("# D", list(.md_section("One", "x"), .md_section("Two", "y")))
  expect_identical(two[3:6], c("## Contents", "", "- [1 One](#1-one)", "- [2 Two](#2-two)"))
})

test_that("unnumbered headings in a chapter's text take their anchors in document order", {
  sections <- list(
    .md_section("Rules", c("##### 2 Notes", "", "text")),
    .md_section("Notes", "y")
  )
  out <- .md_render("# D", sections)
  # "##### 2 Notes" comes first, so the chapter "2 Notes" after it gets "-1".
  expect_true("- [2 Notes](#2-notes-1)" %in% out)
  mdt_skip_without_pandoc()
  ids <- vapply(mdt_headers(mdt_ast(out)), function(h) h$c[[2]][[1]], character(1))
  expect_true(all(c("2-notes", "2-notes-1") %in% ids))
})

# ---- empty columns ---------------------------------------------------------------

test_that(".md_table(drop_empty = TRUE) leaves out columns without any content", {
  df <- data.frame(A = c("x", "y"), B = c("", NA), C = c(" ", "z"), D = c(NA, " \n "), stringsAsFactors = FALSE)
  expect_identical(.md_table(df, drop_empty = TRUE), c(
    "| A | C |", "| --- | --- |", "| x |  |", "| y | z |"
  ))
  # Alignment follows the columns that are kept.
  expect_identical(.md_table(df, align = c("left", "right", "center", "right"), drop_empty = TRUE)[2], "| :--- | :---: |")
  # By default every column stays; a table whose columns are all empty is nothing.
  expect_identical(.md_table(df)[1], "| A | B | C | D |")
  expect_identical(.md_table(data.frame(B = c("", NA)), drop_empty = TRUE), character(0))
})
