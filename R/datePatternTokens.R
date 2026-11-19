# ---- date/time masks inside a regular expression --------------------------------
# A regex-mode field (a file entry's `filename` with `pattern: true`, a column's
# `pattern`, a rule condition's `pattern` operator) may embed a date/time mask
# written `<<yyyyMMddHHmm>>`. It is expanded into a regex fragment only where
# the pattern is used to test a value; the stored property keeps what the
# author wrote. Outside `<<...>>` nothing changes.
#
# Inside the delimiters the letters y M d H m s are reserved: a run of one of
# them must be one of the tokens below. Their wrong-case twins Y D S h are
# refused as typos (M/m are both tokens, guarded by the case check below).
# Every other character (`-`, `:`, the ISO `T`, ...) is literal and escaped. `dd`/`d` is not calendar-aware
# (`02-30` passes) and `yyyy` is 2000-2099 by design.

dta_date_pattern_fragments <- c(
  yyyy = "20[0-9]{2}",
  yy = "[0-9]{2}",
  MMMM = "(?:January|February|March|April|May|June|July|August|September|October|November|December)",
  MMM = "(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)",
  MM = "(?:0[1-9]|1[0-2])",
  dd = "(?:0[1-9]|[12][0-9]|3[01])",
  d = "(?:[1-9]|[12][0-9]|3[01])",
  HH = "(?:[01][0-9]|2[0-3])",
  H = "(?:[0-9]|1[0-9]|2[0-3])",
  mm = "[0-5][0-9]",
  ss = "[0-5][0-9]"
)

# `x` with every `<<mask>>` replaced by its regex fragment. Vectorised; NA and
# elements without `<<` are returned unchanged, so is a non-character `x`.
# Aborts with class `dta_date_pattern_invalid` on a malformed mask.
dta_expand_date_pattern_tokens <- function(x) {
  if (!is.character(x)) {
    return(x)
  }
  has <- !is.na(x) & grepl("<<", x, fixed = TRUE)
  if (any(has)) {
    x[has] <- vapply(x[has], dta_expand_date_pattern_one, character(1), USE.NAMES = FALSE)
  }
  x
}

dta_expand_date_pattern_one <- function(s) {
  out <- character(0)
  rest <- s
  repeat {
    open <- regexpr("<<", rest, fixed = TRUE)
    if (open < 0) break
    after <- substring(rest, open + 2L)
    close <- regexpr(">>", after, fixed = TRUE)
    if (close < 0) {
      delims <- c("<<", ">>") # interpolated: a literal `<` in a cli string fails
      cli::cli_abort(
        "The date mask in {.val {s}} opens with {.code {delims[1]}} but is never closed with {.code {delims[2]}}.",
        class = "dta_date_pattern_invalid"
      )
    }
    out <- c(out, substring(rest, 1L, open - 1L), dta_date_mask_regex(substring(after, 1L, close - 1L)))
    rest <- substring(after, close + 2L)
  }
  paste(c(out, rest), collapse = "")
}

# The regex fragment of one mask (the text between `<<` and `>>`).
dta_date_mask_regex <- function(mask) {
  if (!nzchar(mask)) {
    examples <- c("<<>>", "<<yyyyMMdd>>") # interpolated: a literal `<` in a cli string fails
    cli::cli_abort(
      "A date mask {.code {examples[1]}} is empty; write the tokens inside it, e.g. {.code {examples[2]}}.",
      class = "dta_date_pattern_invalid"
    )
  }
  runs <- rle(strsplit(mask, "", fixed = TRUE)[[1]])
  reserved <- c("y", "M", "d", "H", "m", "s")
  parts <- character(0)
  tokens <- character(0) # the tokens in order, literals skipped
  # The wrong case of a letter that has only one meaningful case (`YYYY`, `DD`,
  # `SS`, `hh`) would otherwise be literal text that never matches.
  miscased <- c(Y = "y", D = "d", S = "s", h = "H")
  meaning <- c(y = "year", d = "day", s = "second", H = "hour")
  # `d`/`H` are themselves valid tokens, but `y`/`s` alone are not (only `yy`,
  # `yyyy`, `ss` are) -- suggest a run that actually works.
  suggestion <- c(Y = "yyyy", D = "d", S = "ss", h = "H")
  for (i in seq_along(runs$values)) {
    ch <- runs$values[[i]]
    len <- runs$lengths[[i]]
    if (ch %in% names(miscased)) {
      run <- strrep(ch, len)
      fix <- miscased[[ch]]
      what <- meaning[[fix]]
      hint <- suggestion[[ch]]
      cli::cli_abort(
        c(
          "The date mask {.val {mask}} contains {.val {run}}: {.code {ch}} is not a date/time token.",
          i = "Did you mean {.code {hint}} ({what})? Token letters are case-sensitive."
        ),
        class = "dta_date_pattern_invalid"
      )
    }
    if (ch %in% reserved) {
      run <- strrep(ch, len)
      if (!run %in% names(dta_date_pattern_fragments)) {
        valid <- names(dta_date_pattern_fragments)
        cli::cli_abort(
          c(
            "The date mask {.val {mask}} contains {.val {run}}, which is not a date/time token.",
            i = "Valid tokens: {.code {valid}}."
          ),
          class = "dta_date_pattern_invalid"
        )
      }
      parts <- c(parts, dta_date_pattern_fragments[[run]])
      tokens <- c(tokens, run)
    } else {
      lit <- strrep(ch, len)
      parts <- c(parts, gsub("([.\\\\|()\\[\\]{}^$*+?])", "\\\\\\1", lit, perl = TRUE))
    }
  }
  dta_date_mask_case_check(mask, tokens)
  paste(parts, collapse = "")
}

# `mm` (minutes) and `MM` (month) differ only in case and are each valid alone,
# so `<<yyyymmdd>>` would quietly mean year-minute-day. A minute token whose
# neighbouring tokens are dates only, or a month token whose neighbours are
# times only, is refused as a likely typo. Literals between tokens do not break
# adjacency (`<<yyyy-mm-dd>>` is caught too).
dta_date_mask_case_check <- function(mask, tokens) {
  date_tokens <- c("yyyy", "yy", "dd", "d")
  time_tokens <- c("HH", "H", "ss")
  month_tokens <- c("MM", "MMM", "MMMM")
  for (i in seq_along(tokens)) {
    nb <- tokens[c(i - 1L, i + 1L)[c(i > 1L, i < length(tokens))]]
    if (identical(tokens[[i]], "mm") && any(nb %in% date_tokens) && !any(nb %in% time_tokens)) {
      cli::cli_abort(
        c(
          "The date mask {.val {mask}} has {.code mm} next to a date: it looks like a mistyped {.code MM} (month).",
          i = "{.code mm} means minutes, {.code MM} means month."
        ),
        class = "dta_date_pattern_invalid"
      )
    }
    # Only bare `MM` can be confused with `mm` (same length, different case);
    # `MMM`/`MMMM` have no minute-token counterpart and are never ambiguous.
    if (identical(tokens[[i]], "MM") && any(nb %in% time_tokens) && !any(nb %in% c(date_tokens, month_tokens))) {
      run <- tokens[[i]]
      cli::cli_abort(
        c(
          "The date mask {.val {mask}} has {.code {run}} next to a time: it looks like a mistyped {.code mm} (minutes).",
          i = "{.code MM} means month, {.code mm} means minutes."
        ),
        class = "dta_date_pattern_invalid"
      )
    }
  }
  invisible(NULL)
}

# Why `x` holds a malformed date mask, or NULL. Same convention as
# dta_file_count_bounds_problem(): shared by the DTAFile validator and
# lint_dta(), so they refuse exactly the same entries.
dta_date_pattern_problem <- function(x) {
  tryCatch(
    {
      dta_expand_date_pattern_tokens(x)
      NULL
    },
    dta_date_pattern_invalid = function(e) dta_condition_text(e)
  )
}
