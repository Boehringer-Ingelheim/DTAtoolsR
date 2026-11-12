# Single source of the plain-English wording of validation rules.
#
# Every human-readable output (Word, Markdown, HTML report, template guide,
# console, Shiny) describes a rule through the functions in this file and
# nothing else. Rules are never shown as code: no YAML, no R, no code spans,
# no monospace. The wording below is the contract; the tests in
# tests/testthat/test-rulePhrasing.R are written from it by hand.
#
# The wording describes what the evaluator (R/evaluateRules.R,
# R/streamingValidation.R) does, including where that is surprising; it never
# describes what a rule was presumably meant to do.
#
# ---------------------------------------------------------------------------
# Notation
# ---------------------------------------------------------------------------
# * Names (columns, grouping columns, condition names, requirement ids) are
#   written as they are when they consist of a letter followed by letters,
#   digits, "_" or "."; any other name is wrapped in single typographic
#   quotes, U+2018 ... U+2019.
# * Text values are wrapped in double typographic quotes, U+201C ... U+201D.
#   Inside text, a line feed is shown as [line break], a tab as [tab], a
#   carriage return as [carriage return] and any other control character as
#   [character U+XXXX].
# * Numbers use the shortest decimal form that reads back to exactly the same
#   double: 15 significant digits, else 16, else 17. Between 1e-7 and 1e21
#   (by absolute value) the plain decimal form is used ("-12.25"); outside it
#   the form "m times 10 to the power of e", with words for both signs:
#   "minus 2.5 times 10 to the power of minus 9". -0 is written 0; Inf, -Inf
#   and NaN are "infinity", "minus infinity" and "not a number". Integer and
#   double storage read the same.
# * Logical values are "true" and "false"; a missing entry is "missing".
# * Lists read "a", "a or b", "a, b or c" (one of) and "a and b",
#   "a, b and c" (all of).
# * No mathematical symbols (they do not survive pdflatex); the source holds
#   only ASCII, with \u escapes for typographic quotes and bullets.
#
# ---------------------------------------------------------------------------
# Layout (.rule_lines is the only place that decides it)
# ---------------------------------------------------------------------------
# A description is a sequence of lines. A line is a statement (no label), a
# labelled line "Label: text", a label alone "Label:" that introduces a list,
# or a list item, shown as "<U+2022> text" in plain text and Word, "- text"
# in Markdown and <li> in HTML. Labels, in this order:
#   Purpose            the author's description, verbatim (first line); a
#                      line break in it stays a line break in every format
#                      (<br> in HTML)
#   (statement)        the rule itself, no label
#   Conditions         group rules: one item per named condition
#   Requirements       group rules: one item per constraint
#   Problem            why the rule cannot be evaluated or misbehaves
#   Missing values     what a missing value does
#   Non-numeric values what a present value that is not a number does
# A labelled section with one note is one line "Label: Note."; with several
# notes it is "Label:" followed by one item per note. Text after a label
# starts with a capital letter and ends with a period.
# Punctuation of list items: an item that is a fragment of the sentence that
# introduces it (IF/THEN tests, Conditions) has no final period; an item that
# is a sentence of its own (Requirements, notes) ends with a period.
# The one-line summary (.rule_summary) drops Purpose and the notes, joins
# fragment items with "; " and sentence items with " ", and ends a list of
# fragments with ".".
#
# Kind labels (.rule_kind_label): "Range", "Unique", "Conditional (IF/THEN)",
# "Grouped condition", "Unsupported rule type".
#
# ---------------------------------------------------------------------------
# Rule types
# ---------------------------------------------------------------------------
# Range (col_range):
#   "Every value in column C must be a number from a to b, inclusive."
#   min = -Inf: "... must be a number that is at most b."
#   max =  Inf: "... must be a number that is at least a."
#   both infinite: "... must be a number."
#   Missing values: "Not checked."
#   Non-numeric values: "A value that is present but is not a number
#   violates the rule."
#   More than one column: the sentence names all columns and the bounds, and
#   a Problem line says the rule cannot be evaluated (the evaluator aborts).
#
# Unique (col_unique):
#   "No two rows may have the same value in column C."
#   "No two rows may have the same combination of values in columns A and B."
#   Missing values: "Two missing values are equal; so are two "not a
#   number" (NaN) values, which are not missing." (kept short: one line of
#   the template guide's rules table)
#   (Values are compared exactly, 0 and minus 0 alike: the reading guide.)
#
# Condition (col_condition):
#   "When <IF condition form>, <THEN requirement form>."  (one column each)
#   Several IF columns: "When all of the following are true:" + list,
#   then "Then <requirement form>." or "Then all of the following must
#   hold:" + list. One IF column and several THEN columns: "When <IF>, all
#   of the following must hold:" + list. An empty IF reads "In every row".
#   Several tests on one column: "C is X, is Y and matches Z".
#
# Group condition (group_condition):
#   "Rows are grouped by the value in column G" / "by the combination of
#   values in columns G and H", "; within each group, the requirements below
#   must hold."
#   Conditions: "<name>: a row in which <IF condition form>"
#   Requirements, with S = "at least one row" (scope any) or "every row"
#   (scope all):
#     requires:  "<id>: If S of a group meets condition I, then S of that
#                group must meet condition T."
#     mutually_exclusive: "<id>: Within a group, it must not happen that S
#                meets condition L and also S meets condition R."
#     followed by "A violation is reported with the message "..."." or,
#     without a message, "A violation is reported with a message naming the
#     group and its rows."
#     A message that is not a single text (several texts, a list, a number)
#     is shown in full: "Its message is given as "one" and "two" instead of
#     a single text.", with the Problem "Requirement <id> has a message that
#     is not a single text: when a group fails it, the validation stops with
#     an error instead of reporting the violation." (the evaluator's
#     vapply(..., character(1)) aborts).
#     Any other requirement type: "<id>: This requirement has the unsupported
#     type "t"; it is never checked. Its other settings are ..." and the
#     Problem "Requirement <id> has an unsupported type and is never
#     checked." (the evaluator skips it; the rule itself still runs).
#   Missing values, first item: "A missing value in a grouping column counts
#   as a value of its own: rows missing it are grouped together."; then per
#   condition column. A condition used only as the IF of a `requires` constraint
#   (dta_group_if_only_conditions()) has the outcomes of the IF of a
#   conditional rule (notes below), because the validator keeps a row with a
#   value that is not a number in scope there; it is worded as a named group
#   condition ("A row in which C is missing does not meet condition N, unless
#   another column of the condition holds a value that is not a number and no
#   other test of the condition fails."; "A value in C that is not a number
#   does not by itself keep the row from meeting condition N."). Any other group condition
#   (also a THEN, left or right) needs a decided test, so a test that is not
#   decided never meets it:
#     test fails on missing: "A row in which C is missing does not meet
#                             condition N."
#     test holds, one column: "A row in which C is missing meets condition N."
#     test holds, several:    "If C is missing, this part of condition N
#                             counts as met."
#   Non-numeric values: "A value in C that is not a number does not meet
#   condition N."
#
# Any other rule class: "This rule has the unsupported type 'x'; its
# parameters cannot be described."
#
# The author's description is always its own "Purpose" line and never
# replaces the mechanical wording.
#
# Problem: "This rule cannot be evaluated." followed by the reasons when the
# evaluator aborts on the rule; reasons alone when it runs but a part of the
# rule never works as written. Either way .rule_describe()$valid is FALSE.
#
# ---------------------------------------------------------------------------
# Operators (condition form / requirement form / a missing value)
# ---------------------------------------------------------------------------
# operator               | IF: C ...                 | THEN: C must ...        | missing
# -----------------------+---------------------------+-------------------------+--------
# equals, equal          | is equal to v             | be equal to v           | undecided
# not_equals, not_equal  | is not equal to v         | not be equal to v       | undecided
# in                     | is one of a, b or c       | be one of a, b or c     | (1)
# not_in                 | is not one of a, b or c   | not be one of a, b or c | (2)
# greater                | is greater than v         | be greater than v       | undecided
# less                   | is less than v            | be less than v          | undecided
# greater_equal, min     | is at least v             | be at least v           | undecided
# less_equal, max        | is at most v              | be at most v            | undecided
# range, min + max       | is from a to b, inclusive | be from a to b, incl.   | undecided
# pattern                | matches the pattern "p"   | match the pattern "p"   | no
# empty: true            | is empty                  | be empty                | yes
# empty: false           | is not empty              | not be empty            | no
# anything else          | passes the unsupported test 'op' with v
#                        | must pass the unsupported test 'op' with v
#                          and a line "Problem: This rule cannot be evaluated.
#                          The test 'op' on column C is not supported."
# A test with the wrong number of values (or a column given a bare value
# instead of tests) is described the same way, with its own Problem line.
#
# min with an infinite max reads "at least", max with an infinite min "at
# most", and both infinite "a number".
# The numeric tests (greater, less, greater_equal, less_equal, min, max,
# range) read a bound given as text as a number: min: "5" reads "at least 5"
# (in a column of dates such a bound stops the validation with an error; the
# reading guide says so). A text bound that does not read as a number is
# shown as text ("at least "abc"") with a Problem; the comparison is then
# undecided for every row that holds a number: the IF is never met by a
# number, the THEN is violated by every row. In a column of dates, the bound
# is read as a date (as.Date()), and text that is not a date stops the
# validation with an error. So the Problem reads
#   a date such as "2024-01-31": "The test 'min' on column C is given
#     "2024-01-31", which is not a number: no value passes this test, unless
#     C holds dates."
#   any other text: "The test 'min' on column C is given "abc", which is not
#     a number: no value passes this test, and if C holds dates, the
#     validation stops with an error."
#   bounds given as a list (a YAML sequence of mixed types, range: [0, x]):
#     a date column reads them as numbers (days), so text never passes:
#     "...: no value passes this test, in a column of dates as well."
#
# in / not_in values:
#   A plain vector lists every entry; a missing entry reads "missing":
#   "is one of "A" or missing". It matches a missing value.
#   A list (YAML sequence) is listed without its empty (YAML ~) and missing
#   entries, which are explained in parentheses instead, because most of
#   them are not read as "missing" (see below). The evaluator
#   (dta_equality_bound()) compares a list as numbers only when the column
#   holds numbers and the flattened list is numeric, or is text of which
#   every entry that is not missing reads as a number (a text "2" counts; a
#   list of logical values only does not, since flattening keeps it logical,
#   while true next to a number flattens to 1; an empty entry is dropped);
#   otherwise it compares text, and a missing entry is the text "NA" (unless
#   it is a missing text entry, see below). An empty entry (YAML ~) is read as
#   a missing text entry.
#   Since the list is known, the wording names only what depends on the
#   column:
#     missing entry, list compares as numbers: "a missing entry, which
#                    matches a missing value if C holds numbers and
#                    otherwise only the text "NA""
#     missing entry, otherwise: "a missing entry, which matches only the
#                    text "NA", never a missing value"
#   as "one of a or b (the list also holds <entry>[, and <entry>])" or, with
#   no other entry, "one of the entries of a list that holds only <entry>".
#     missing text entry (YAML ~, .na.character, NA_character_): "a missing
#                    entry, which matches a missing value" in a column of any
#                    type (match() keeps it missing while the other entries
#                    become text); next to it, any other missing entry reads
#                    "another missing entry, which ..." as above, without
#                    ", never a missing value".
#   (1) in: yes for a plain vector with a missing entry or a list with a
#       missing text entry; "depends" for a list with another missing entry
#       that compares as numbers; otherwise no.
#   (2) not_in: the opposite of (1).
# "missing" says what the test gives for a missing value: yes, no, or
# undecided (no for a group condition and the THEN; see the IF notes).
# Several tests on one column: no if any test gives no, else "depends" if a
# test depends, else undecided if any test is undecided, else yes.
# Resulting notes ("this part of" only when the clause has several columns):
#   IF, yes:        "If C is missing, [this part of] the condition counts as
#                    met."
#   IF, no:         "If C is missing, the condition is not met and the row is
#                    not checked."
#   IF, undecided:  the same, and when another column of the condition has a
#                    numeric test: "..., unless another column of the
#                    condition holds a value that is not a number and no other
#                    test of the condition fails." (dta_condition_in_scope()).
#   IF, depends:    "If C is missing, the condition is not met and the row is
#                    not checked, unless C holds numbers; then [this part
#                    of] the condition counts as met." (not_in: the two
#                    outcomes swapped).
#   THEN, yes:      "If C is missing, [this part of] the requirement counts
#                    as met."
#   THEN, no/undecided: "If C is missing in a row that meets the condition,
#                    the rule is violated."
#   THEN, depends:  "..., the rule is violated, unless C holds numbers; then
#                    [this part of] the requirement counts as met." (not_in:
#                    swapped).
#   (Group conditions: "..., unless C holds numbers; then ...", the same.)
# A column whose tests cannot be evaluated gets no note. A THEN column that
# the IF also tests gets no missing-value note when a missing value there
# never reaches the THEN: one of its IF tests gives no, or they are
# undecided (and yes) and no other IF column has a numeric test
# (dta_condition_in_scope() keeps an undecided row only for a value of
# another numeric column that is not a number). Example: "When VISIT is equal
# to "V03", VISIT must ..." has no note on VISIT in the THEN.
# Columns with a numeric test also get a note on values that are present
# but not numbers:
#   IF:   "A value in C that is not a number does not by itself exempt the
#          row from the check."
#   THEN: "A value in C that is not a number, in a row that meets the
#          condition, violates the rule."
#
# The reading guide (.rule_reading_guide) is shown once per document and
# states what holds for every rule: per-file checking, exact text
# comparison, what reads as a number, when equality compares numbers, how
# a number is written when compared as text, dates (a number bound counts
# days, or seconds in a column of dates with a time; a text bound must be a
# date), missing and empty values, patterns, and absent columns (a grouped
# condition passes a file with no rows as long as its grouping columns are
# present: rule_check_group_condition() checks only the grouping columns
# before it returns for an empty table).

# ---------------------------------------------------------------------------
# Notation helpers
# ---------------------------------------------------------------------------

.RULE_LQ <- "\u201c"
.RULE_RQ <- "\u201d"
.RULE_LSQ <- "\u2018"
.RULE_RSQ <- "\u2019"
.RULE_BULLET <- "\u2022"

# Make control characters visible inside a value or name.
.rule_escape_text <- function(x) {
  x <- gsub("\n", "[line break]", x, fixed = TRUE)
  x <- gsub("\t", "[tab]", x, fixed = TRUE)
  x <- gsub("\r", "[carriage return]", x, fixed = TRUE)
  ctrl <- gregexpr("[\001-\037\177]", x, perl = TRUE)
  regmatches(x, ctrl) <- lapply(regmatches(x, ctrl), function(chars) {
    vapply(chars, function(ch) {
      sprintf("[character U+%04X]", utf8ToInt(ch))
    }, character(1), USE.NAMES = FALSE)
  })
  x
}

# Shortest decimal rendering of one number that reads back exactly.
.rule_number <- function(x) {
  if (length(x) != 1L || !(is.numeric(x) || is.logical(x))) {
    cli::cli_abort(
      "{.arg x} must be a single number.",
      class = "dta_rule_phrasing_error"
    )
  }
  x <- as.double(x)
  if (is.nan(x)) {
    return("not a number")
  }
  if (is.na(x)) {
    return("missing")
  }
  if (is.infinite(x)) {
    return(if (x > 0) "infinity" else "minus infinity")
  }
  if (x == 0) {
    return("0")
  }

  # sprintf() is correctly rounded everywhere, but as.numeric() is not (macOS
  # arm64 has no extended long double and misreads some decimals by one ulp),
  # so the read-back goes through jsonlite, whose parser uses the C library's
  # strtod. It yields Inf on overflow and may return an integer; `==` copes.
  sci <- NULL
  for (digits in 15:17) {
    sci <- sprintf("%.*e", digits - 1L, x)
    if (isTRUE(jsonlite::parse_json(paste0("[", sci, "]"))[[1]] == x)) {
      break
    }
  }

  negative <- startsWith(sci, "-")
  sci <- sub("^-", "", sci)
  mantissa <- sub("e.*$", "", sci)
  exponent <- as.integer(sub("^.*e", "", sci))
  significand <- gsub(".", "", mantissa, fixed = TRUE)
  significand <- sub("0+$", "", significand)
  if (!nzchar(significand)) {
    significand <- "0"
  }
  sign <- if (negative) "-" else ""

  if (abs(x) >= 1e-7 && abs(x) < 1e21) {
    n <- nchar(significand)
    if (exponent >= 0L) {
      if (n <= exponent + 1L) {
        out <- paste0(significand, strrep("0", exponent + 1L - n))
      } else {
        out <- paste0(
          substr(significand, 1L, exponent + 1L), ".",
          substr(significand, exponent + 2L, n)
        )
      }
    } else {
      out <- paste0("0.", strrep("0", -exponent - 1L), significand)
    }
    return(paste0(sign, out))
  }

  m <- if (nchar(significand) > 1L) {
    paste0(substr(significand, 1L, 1L), ".", substring(significand, 2L))
  } else {
    significand
  }
  paste0(
    if (negative) "minus " else "", m, " times 10 to the power of ",
    if (exponent < 0L) paste0("minus ", -exponent) else exponent
  )
}

# One value as it appears in a rule: number, text, logical or missing.
.rule_value <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return("missing")
  }
  if (is.list(x) && length(x) == 1L) {
    return(.rule_value(x[[1L]]))
  }
  if (is.list(x) || length(x) != 1L) {
    cli::cli_abort(
      "{.arg x} must be a single value.",
      class = "dta_rule_phrasing_error"
    )
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.na(x) && !(is.double(x) && is.nan(x))) {
    return("missing")
  }
  if (is.logical(x)) {
    return(if (isTRUE(x)) "true" else "false")
  }
  if (is.numeric(x)) {
    return(.rule_number(x))
  }
  paste0(.RULE_LQ, .rule_escape_text(as.character(x)), .RULE_RQ)
}

# Every entry of a value list, each rendered with .rule_value(). A NULL
# entry of a list (YAML ~) is a missing entry.
.rule_values <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }
  if (is.list(x)) {
    out <- lapply(x, function(el) {
      if (is.list(el)) .rule_values(el) else if (length(el) == 0L) "missing" else .rule_values(el)
    })
    return(unlist(out, use.names = FALSE))
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  vapply(seq_along(x), function(i) .rule_value(x[i]), character(1))
}

# A name: as written when it is a plain identifier, otherwise quoted.
.rule_name <- function(x) {
  x <- as.character(x)
  vapply(x, function(one) {
    if (is.na(one)) {
      return("missing")
    }
    if (grepl("^[A-Za-z][A-Za-z0-9_.]*$", one)) {
      one
    } else {
      paste0(.RULE_LSQ, .rule_escape_text(one), .RULE_RSQ)
    }
  }, character(1), USE.NAMES = FALSE)
}

.rule_join <- function(x, word) {
  x <- as.character(x)
  n <- length(x)
  if (n == 0L) {
    return("")
  }
  if (n == 1L) {
    return(x)
  }
  paste0(paste(x[-n], collapse = ", "), " ", word, " ", x[n])
}

.rule_or <- function(x) .rule_join(x, "or")

.rule_and <- function(x) .rule_join(x, "and")

# "column A" / "columns A and B"
.rule_columns <- function(cols) {
  paste0(if (length(cols) == 1L) "column " else "columns ", .rule_and(.rule_name(cols)))
}

# ---------------------------------------------------------------------------
# Operators
# ---------------------------------------------------------------------------

# A bound of a band (min/max/range); TRUE when it removes no number.
.rule_open_bound <- function(x, upper) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && is.infinite(x) &&
    ((upper && x > 0) || (!upper && x < 0))
}

# Phrase for an inclusive band from `lo` to `hi`, without the verb.
.rule_band <- function(lo, hi) {
  lo_open <- .rule_open_bound(lo, upper = FALSE)
  hi_open <- .rule_open_bound(hi, upper = TRUE)
  if (lo_open && hi_open) {
    "a number"
  } else if (lo_open) {
    paste0("at most ", .rule_value(hi))
  } else if (hi_open) {
    paste0("at least ", .rule_value(lo))
  } else {
    paste0("from ", .rule_value(lo), " to ", .rule_value(hi), ", inclusive")
  }
}

# The two bounds of a range, each keeping its own type (a list may mix a
# number and a text).
.rule_bounds <- function(value) {
  if (is.list(value) && length(value) == 2L &&
    all(vapply(value, function(v) length(unlist(v)) == 1L, logical(1)))) {
    return(list(unlist(value[[1]]), unlist(value[[2]])))
  }
  flat <- .rule_flat(value)
  list(flat[[1]], flat[[2]])
}

.rule_flat <- function(value) {
  if (is.list(value)) unlist(value, use.names = FALSE) else value
}

# A bound of a numeric test. Text that reads as a number is that number, as
# dta_numeric_operands() reads it; other text stays text and is flagged.
.rule_numeric_bound <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    parsed <- suppressWarnings(as.numeric(x))
    if (!is.na(parsed)) {
      return(list(value = parsed, bad = FALSE))
    }
    return(list(value = x, bad = TRUE))
  }
  list(value = x, bad = FALSE)
}

# Whether dta_equality_bound() turns the entries of a list into numbers when
# the column holds numbers: the list is flattened (empty entries vanish), and
# it compares as numbers when the result is numeric, or is text of which every
# entry that is not missing reads as a number. A list of logical values (with
# no number among them) stays a comparison of text.
.rule_list_reads_numeric <- function(value) {
  values <- unlist(value, use.names = FALSE)
  if (is.numeric(values)) {
    return(TRUE)
  }
  if (!is.character(values) && !is.factor(values)) {
    return(FALSE)
  }
  parsed <- suppressWarnings(as.numeric(as.character(values)))
  !any(is.na(parsed) & !is.na(values))
}

# Whether a text bound reads as a date the way a date column compares it
# (as.Date() on the text, which a date-time column accepts as well); other
# text makes that comparison stop with an error.
.rule_reads_as_date <- function(x) {
  parsed <- tryCatch(as.Date(as.character(x)), error = function(e) NA)
  length(parsed) == 1L && !is.na(parsed)
}

# The values of an in/not_in test. A plain vector lists every entry, a
# missing one as "missing". A list (YAML sequence) is listed without its empty
# (NULL) and missing entries, which are explained in parentheses (see the
# comment block at the top).
# `missing` is "yes", "no" or "depends": whether `in` holds for a missing
# value.
.rule_set_values <- function(value, col) {
  if (!is.list(value)) {
    return(list(
      text = paste0("one of ", .rule_or(.rule_values(value))),
      # NaN is not NA for %in%: only a true missing entry matches one.
      missing = if (any(is.na(value) & !(if (is.double(value)) is.nan(value) else FALSE))) "yes" else "no"
    ))
  }
  is_null <- vapply(value, function(el) length(el) == 0L, logical(1))
  is_na <- vapply(value, function(el) {
    !is.list(el) && length(el) == 1L && is.na(el) && !(is.double(el) && is.nan(el))
  }, logical(1))
  # A missing text entry (YAML .na.character) stays a true missing value when
  # match() turns the list into text, so it matches a missing value in a
  # column of any type; any other missing entry becomes the text "NA".
  # An empty entry (YAML ~) is read as a missing text entry (see
  # dta_condition_mask()).
  is_na_text <- is_null | (is_na & vapply(value, is.character, logical(1)))
  is_na_other <- is_na & !is_na_text
  shown <- .rule_values(value[!is_null & !is_na])
  bodies <- character(0)
  missing <- "no"
  if (any(is_na_text)) {
    missing <- "yes"
    bodies <- c(bodies, "a missing entry, which matches a missing value")
  }
  if (any(is_na_other)) {
    entry <- if (any(is_na_text)) "another missing entry" else "a missing entry"
    if (.rule_list_reads_numeric(value)) {
      if (missing == "no") {
        missing <- "depends"
      }
      bodies <- c(bodies, paste0(
        entry, ", which matches a missing value if ", col,
        " holds numbers and otherwise only the text ", .rule_value("NA")
      ))
    } else {
      bodies <- c(bodies, paste0(
        entry, ", which matches only the text ", .rule_value("NA"),
        if (!any(is_na_text)) ", never a missing value"
      ))
    }
  }
  text <- if (length(shown) == 0L) {
    paste0("one of the entries of a list that holds only ", paste(bodies, collapse = ", and "))
  } else if (length(bodies) > 0L) {
    paste0(
      "one of ", .rule_or(shown), " (the list also holds ",
      paste(bodies, collapse = ", and "), ")"
    )
  } else {
    paste0("one of ", .rule_or(shown))
  }
  list(text = text, missing = missing)
}

# One test on one column. Returns the condition form ("is ..."), the
# requirement form ("must be ..."), what the test gives for a missing value
# ("yes", "no", "undecided", "depends", "depends_not"), whether it compares
# numbers, a problem sentence (or NULL) when the evaluator cannot run it, and
# a defect sentence (or NULL) when it runs but the test never works as
# written.
.rule_predicate <- function(column, operator, value) {
  pred <- function(is, must, missing, numeric = FALSE, problem = NULL, defect = NULL) {
    list(
      is = is, must = must, missing = missing, numeric = numeric,
      problem = problem, defect = defect
    )
  }
  be <- function(phrase, missing, numeric = FALSE, defect = NULL) {
    pred(paste0("is ", phrase), paste0("must be ", phrase), missing, numeric, defect = defect)
  }
  be_not <- function(phrase, missing) {
    pred(paste0("is not ", phrase), paste0("must not be ", phrase), missing)
  }
  flat <- .rule_flat(value)
  n <- length(flat)
  single_ok <- n == 1L && !anyNA(flat)
  col <- .rule_name(column)

  invalid <- function(need) {
    shown <- if (length(value) == 0L) "no value" else .rule_and(.rule_values(value))
    phrase <- paste0(
      "the test ", .RULE_LSQ, operator, .RULE_RSQ, " with ", shown
    )
    pred(
      paste0("passes ", phrase), paste0("must pass ", phrase), "no",
      problem = paste0(
        "The test ", .RULE_LSQ, operator, .RULE_RSQ, " on column ", col,
        " needs ", need, "."
      )
    )
  }

  # A numeric test whose bound is text that does not read as a number: the
  # comparison is undecided for every row (see the comment block at the top).
  bad_bound <- function(bounds) {
    bad <- Filter(function(b) isTRUE(b$bad), bounds)
    if (length(bad) == 0L) {
      return(NULL)
    }
    paste0(
      "The test ", .RULE_LSQ, operator, .RULE_RSQ, " on column ", col,
      " is given ", .rule_and(vapply(bad, function(b) .rule_value(b$value), character(1))),
      ", which ", if (length(bad) == 1L) "is not a number" else "are not numbers",
      if (is.list(value)) {
        # Given as a list (a YAML sequence of mixed types), a date column
        # reads every bound as a number too, so text never passes there either.
        ": no value passes this test, in a column of dates as well."
      } else if (all(vapply(bad, function(b) .rule_reads_as_date(b$value), logical(1)))) {
        paste0(": no value passes this test, unless ", col, " holds dates.")
      } else {
        paste0(
          ": no value passes this test, and if ", col,
          " holds dates, the validation stops with an error."
        )
      }
    )
  }

  single <- c(
    "equals", "equal", "not_equals", "not_equal", "greater", "less",
    "greater_equal", "less_equal", "min", "max", "pattern"
  )
  if (operator %in% single && !single_ok) {
    return(invalid("exactly one value that is not missing"))
  }

  numeric_one <- function(phrase_of) {
    b <- .rule_numeric_bound(flat)
    be(phrase_of(b$value), "undecided", TRUE, defect = bad_bound(list(b)))
  }

  switch(operator,
    equals = ,
    equal = be(paste0("equal to ", .rule_value(flat)), "undecided"),
    not_equals = ,
    not_equal = be_not(paste0("equal to ", .rule_value(flat)), "undecided"),
    `in` = {
      # Count the members as written: unlist() drops a YAML `~`, so `in: [~]`
      # would count none, while the evaluator counts it as one member.
      if (length(value) < 1L) {
        return(invalid("one or more values"))
      }
      set <- .rule_set_values(value, col)
      be(set$text, set$missing)
    },
    not_in = {
      # Count the members as written: unlist() drops a YAML `~`, so `in: [~]`
      # would count none, while the evaluator counts it as one member.
      if (length(value) < 1L) {
        return(invalid("one or more values"))
      }
      set <- .rule_set_values(value, col)
      be_not(set$text, switch(set$missing,
        yes = "no",
        no = "yes",
        depends = "depends_not"
      ))
    },
    greater = numeric_one(function(v) paste0("greater than ", .rule_value(v))),
    less = numeric_one(function(v) paste0("less than ", .rule_value(v))),
    greater_equal = ,
    min = numeric_one(function(v) .rule_band(v, Inf)),
    less_equal = ,
    max = numeric_one(function(v) .rule_band(-Inf, v)),
    range = {
      if (n != 2L || anyNA(flat)) {
        return(invalid("exactly two values that are not missing, the lower and upper bound"))
      }
      bounds <- lapply(.rule_bounds(value), .rule_numeric_bound)
      be(
        .rule_band(bounds[[1]]$value, bounds[[2]]$value), "undecided", TRUE,
        defect = bad_bound(bounds)
      )
    },
    pattern = pred(
      paste0("matches the pattern ", .rule_value(flat)),
      paste0("must match the pattern ", .rule_value(flat)),
      "no"
    ),
    empty = {
      if (!(isTRUE(flat) || isFALSE(flat))) {
        return(invalid("the value true or false"))
      }
      if (isTRUE(flat)) be("empty", "yes") else be_not("empty", "no")
    },
    {
      shown <- if (length(value) == 0L) "no value" else .rule_and(.rule_values(value))
      phrase <- paste0(
        "the unsupported test ", .RULE_LSQ, .rule_escape_text(operator), .RULE_RSQ,
        " with ", shown
      )
      pred(
        paste0("passes ", phrase), paste0("must pass ", phrase), "no",
        problem = paste0(
          "The test ", .RULE_LSQ, .rule_escape_text(operator), .RULE_RSQ,
          " on column ", col, " is not supported."
        )
      )
    }
  )
}

# What all tests on one column give together for a missing value: they are
# combined with AND, so one "no" decides it.
.rule_missing_outcome <- function(kinds) {
  if ("no" %in% kinds) {
    return("no")
  }
  dep <- intersect(c("depends", "depends_not"), kinds)
  if (length(dep) > 0L) {
    if (all(kinds %in% c(dep[[1]], "yes"))) {
      return(dep[[1]])
    }
    # A "depends" test next to an undecided one: the row stays in scope only
    # when the depends test says yes (depends: the column holds numbers;
    # depends_not: it does not) AND, as for any undecided test, another column
    # of the condition holds a value that is not a number and no test fails
    # (dta_condition_in_scope()). Only depends and depends_not together leave
    # nothing to say but "no".
    return(if ("undecided" %in% kinds && length(dep) == 1L) paste0(dep[[1]], "_undecided") else "no")
  }
  if ("undecided" %in% kinds) {
    return("undecided")
  }
  "yes"
}

# All tests on one column, as one phrase with the column as subject.
# `form` is "if" (condition form) or "then" (requirement form).
.rule_column_clause <- function(column, spec, form) {
  col <- .rule_name(column)
  bad <- function(phrase_is, phrase_must, problem) {
    list(
      text = paste0(col, " ", if (form == "if") phrase_is else phrase_must),
      missing = "no", numeric = FALSE,
      problems = problem, defects = character(0)
    )
  }
  if (!is.list(spec)) {
    shown <- if (length(spec) == 0L) "no value" else .rule_and(.rule_values(spec))
    return(bad(
      paste0("is given the bare value ", shown, " instead of a test"),
      paste0("is given the bare value ", shown, " instead of a test"),
      paste0("Column ", col, " is given a value instead of a test.")
    ))
  }
  ops <- names(spec)
  if (length(spec) == 0L || is.null(ops) || !all(nzchar(ops))) {
    return(bad(
      "has no test", "has no test",
      paste0("Column ", col, " has no named test.")
    ))
  }

  preds <- list()
  band_done <- FALSE
  for (i in seq_along(spec)) {
    op <- ops[[i]]
    if (op %in% c("min", "max") && all(c("min", "max") %in% ops)) {
      if (band_done) {
        next
      }
      band_done <- TRUE
      lo <- spec[["min"]]
      hi <- spec[["max"]]
      p_lo <- .rule_predicate(column, "min", lo)
      p_hi <- .rule_predicate(column, "max", hi)
      if (!is.null(p_lo$problem) || !is.null(p_hi$problem) ||
        !is.null(p_lo$defect) || !is.null(p_hi$defect)) {
        preds <- c(preds, list(p_lo, p_hi))
      } else {
        preds[[length(preds) + 1L]] <- .rule_predicate(
          column, "range", list(.rule_flat(lo), .rule_flat(hi))
        )
      }
      next
    }
    preds[[length(preds) + 1L]] <- .rule_predicate(column, op, spec[[i]])
  }

  forms <- vapply(preds, function(p) if (form == "if") p$is else p$must, character(1))
  list(
    text = paste0(col, " ", .rule_and(forms)),
    missing = .rule_missing_outcome(vapply(preds, function(p) p$missing, character(1))),
    kinds = vapply(preds, function(p) p$missing, character(1)),
    numeric = any(vapply(preds, function(p) isTRUE(p$numeric), logical(1))),
    problems = unlist(lapply(preds, function(p) p$problem), use.names = FALSE),
    defects = unlist(lapply(preds, function(p) p$defect), use.names = FALSE)
  )
}

# A whole clause (column -> tests).
.rule_clause <- function(map, form) {
  if (is.null(map) || length(map) == 0L) {
    return(list())
  }
  cols <- names(map)
  if (!is.list(map) || is.null(cols) || !all(nzchar(cols))) {
    return(list(list(
      column = NA_character_,
      text = paste0("an entry without a column name (", .rule_and(.rule_values(map)), ")"),
      missing = "no", numeric = FALSE,
      problems = "An entry does not name its column.", defects = character(0)
    )))
  }
  lapply(seq_along(map), function(i) {
    out <- .rule_column_clause(cols[[i]], map[[i]], form)
    out$column <- cols[[i]]
    out
  })
}

# Whether a missing value in `column` can never reach the THEN clause,
# because the IF clause `if_parts` tests the same column and a missing value
# there takes the row out of scope: a test that gives "no" decides the
# conjunction, and an undecided one keeps the row only when another column of
# the IF compares numbers and holds a value that is not one
# (dta_condition_in_scope()).
.rule_then_missing_unreachable <- function(column, if_parts) {
  for (k in seq_along(if_parts)) {
    p <- if_parts[[k]]
    if (!identical(p$column, column) || length(p$problems) > 0L) {
      next
    }
    kinds <- p$kinds %||% p$missing
    if ("no" %in% kinds) {
      return(TRUE)
    }
    others_numeric <- any(vapply(if_parts[-k], function(o) {
      isTRUE(o$numeric) && length(o$problems) == 0L
    }, logical(1)))
    if ("undecided" %in% kinds && all(kinds %in% c("undecided", "yes")) && !others_numeric) {
      return(TRUE)
    }
  }
  FALSE
}

# Missing-value and non-number notes for one clause. For the THEN clause,
# `if_parts` is the IF clause: a THEN column whose missing value can never
# reach the THEN gets no missing-value note.
.rule_clause_notes <- function(parts, form, condition_name = NULL, if_parts = list()) {
  missing <- character(0)
  nonnum <- character(0)
  several <- length(parts) > 1L
  usable <- vapply(parts, function(p) !is.na(p$column) && length(p$problems) == 0L, logical(1))
  numeric_cols <- vapply(parts, function(p) isTRUE(p$numeric), logical(1)) & usable
  unless_list <- function(col) {
    paste0(", unless ", col, " holds numbers; then ")
  }
  for (k in seq_along(parts)) {
    p <- parts[[k]]
    # A column whose tests cannot be evaluated gets no note: the Problem line
    # already says the rule cannot run.
    if (!usable[[k]]) {
      next
    }
    col <- .rule_name(p$column)
    # A depends (or depends_not) test next to an undecided one: the exception
    # of an undecided test, which holds only while the depends test says yes.
    combo <- function(dep) {
      if (!any(numeric_cols[-k])) {
        return("")
      }
      paste0(
        ", unless ", col, if (identical(dep, "depends")) " holds numbers" else " does not hold numbers",
        ", another column of the condition holds a value that is not a number",
        " and no other test of the condition fails"
      )
    }
    if (identical(form, "group_if")) {
      # The outcomes of an IF (the validator keeps such a row in scope, see
      # dta_group_if_only_conditions()), in the words of a group condition:
      # a group rule names its conditions and checks groups, not rows.
      cond <- .rule_name(condition_name)
      met <- if (several) {
        paste0("this part of condition ", cond, " counts as met")
      } else {
        paste0("the row meets condition ", cond)
      }
      not_met <- paste0("A row in which ", col, " is missing does not meet condition ", cond)
      missing <- c(missing, switch(p$missing,
        yes = if (several) {
          paste0("If ", col, " is missing, ", met, ".")
        } else {
          paste0("A row in which ", col, " is missing meets condition ", cond, ".")
        },
        undecided = paste0(
          not_met,
          if (any(numeric_cols[-k])) {
            paste0(
              ", unless another column of the condition holds a value that",
              " is not a number and no other test of the condition fails"
            )
          },
          "."
        ),
        depends = paste0(not_met, unless_list(col), met, "."),
        depends_not = paste0(
          "If ", col, " is missing, ", met, unless_list(col), "the row does not meet condition ", cond, "."
        ),
        depends_undecided = paste0(not_met, combo("depends"), "."),
        depends_not_undecided = paste0(not_met, combo("depends_not"), "."),
        paste0(not_met, ".")
      ))
      if (p$numeric) {
        nonnum <- c(nonnum, paste0(
          "A value in ", col, " that is not a number does not by itself keep the row from meeting condition ",
          cond, "."
        ))
      }
    } else if (identical(form, "if")) {
      part <- if (several) "this part of the condition" else "the condition"
      met <- paste0(part, " counts as met")
      not_met <- "the condition is not met and the row is not checked"
      missing <- c(missing, switch(p$missing,
        yes = paste0("If ", col, " is missing, ", met, "."),
        undecided = paste0(
          "If ", col, " is missing, ", not_met,
          if (any(numeric_cols[-k])) {
            paste0(
              ", unless another column of the condition holds a value that",
              " is not a number and no other test of the condition fails"
            )
          },
          "."
        ),
        depends = paste0("If ", col, " is missing, ", not_met, unless_list(col), met, "."),
        depends_not = paste0("If ", col, " is missing, ", met, unless_list(col), not_met, "."),
        depends_undecided = paste0("If ", col, " is missing, ", not_met, combo("depends"), "."),
        depends_not_undecided = paste0("If ", col, " is missing, ", not_met, combo("depends_not"), "."),
        paste0("If ", col, " is missing, ", not_met, ".")
      ))
      if (p$numeric) {
        nonnum <- c(nonnum, paste0(
          "A value in ", col, " that is not a number does not by itself exempt the row from the check."
        ))
      }
    } else if (identical(form, "then")) {
      part <- if (several) "this part of the requirement" else "the requirement"
      met <- paste0(part, " counts as met")
      violated <- "the rule is violated"
      missing <- c(missing, if (!.rule_then_missing_unreachable(p$column, if_parts)) {
        switch(p$missing,
          yes = paste0("If ", col, " is missing, ", met, "."),
          depends = paste0(
            "If ", col, " is missing in a row that meets the condition, ", violated,
            unless_list(col), met, "."
          ),
          depends_not = paste0("If ", col, " is missing, ", met, unless_list(col), violated, "."),
          paste0("If ", col, " is missing in a row that meets the condition, ", violated, ".")
        )
      })
      if (p$numeric) {
        nonnum <- c(nonnum, paste0(
          "A value in ", col, " that is not a number, in a row that meets the condition, violates the rule."
        ))
      }
    } else {
      cond <- .rule_name(condition_name)
      met <- if (several) {
        paste0("this part of condition ", cond, " counts as met")
      } else {
        paste0("the row meets condition ", cond)
      }
      not_met <- paste0("A row in which ", col, " is missing does not meet condition ", cond)
      missing <- c(missing, switch(p$missing,
        yes = if (several) {
          paste0("If ", col, " is missing, ", met, ".")
        } else {
          paste0("A row in which ", col, " is missing meets condition ", cond, ".")
        },
        depends = paste0(not_met, unless_list(col), met, "."),
        depends_not = paste0(
          "If ", col, " is missing, ", met, unless_list(col), "the row does not meet condition ", cond, "."
        ),
        paste0(not_met, ".")
      ))
      if (p$numeric) {
        nonnum <- c(nonnum, paste0(
          "A value in ", col, " that is not a number does not meet condition ", cond, "."
        ))
      }
    }
  }
  list(missing = missing, nonnum = nonnum)
}


# ---------------------------------------------------------------------------
# Rule descriptions
# ---------------------------------------------------------------------------

.rule_section <- function(role, text = "", label = "", items = character(0)) {
  list(role = role, label = label, text = text, items = items)
}

# Notes become one labelled line, or a labelled list when there are several.
.rule_note_section <- function(role, label, notes) {
  if (length(notes) == 0L) {
    return(NULL)
  }
  if (length(notes) == 1L) {
    return(.rule_section(role, notes, label))
  }
  .rule_section(role, "", label, notes)
}

# `problems` stop the evaluator; `defects` let it run, but a part of the rule
# never works as written.
.rule_problem_section <- function(problems, defects = character(0)) {
  if (length(problems) == 0L && length(defects) == 0L) {
    return(NULL)
  }
  lead <- if (length(problems) > 0L) "This rule cannot be evaluated." else character(0)
  .rule_section(
    "problem",
    paste(c(lead, unique(problems), unique(defects)), collapse = " "),
    "Problem"
  )
}

.rule_is <- function(rule, class) {
  inherits(rule, paste0("DTAtools::", class)) || inherits(rule, class)
}

.rule_kind_label <- function(rule) {
  if (.rule_is(rule, "DTARuleGroupCondition")) {
    "Grouped condition"
  } else if (.rule_is(rule, "DTARuleColCondition")) {
    "Conditional (IF/THEN)"
  } else if (.rule_is(rule, "DTARuleColRange")) {
    "Range"
  } else if (.rule_is(rule, "DTARuleColUnique")) {
    "Unique"
  } else {
    "Unsupported rule type"
  }
}

.rule_describe_range <- function(rule) {
  cols <- rule@columns
  band <- .rule_band(rule@min, rule@max)
  band <- if (identical(band, "a number")) {
    "a number"
  } else {
    if (startsWith(band, "from ")) paste0("a number ", band) else paste0("a number that is ", band)
  }
  sections <- list()
  if (length(cols) == 1L) {
    sections[[1]] <- .rule_section(
      "statement",
      paste0("Every value in ", .rule_columns(cols), " must be ", band, ".")
    )
    sections[[2]] <- .rule_section("missing", "Not checked.", "Missing values")
    sections[[3]] <- .rule_section(
      "nonnumeric", "A value that is present but is not a number violates the rule.",
      "Non-numeric values"
    )
  } else {
    sections[[1]] <- .rule_section(
      "statement",
      paste0("Every value in ", .rule_columns(cols), " would have to be ", band, ".")
    )
    sections[[2]] <- .rule_problem_section(paste0(
      "A range rule needs exactly one column, but this one names ",
      length(cols), "."
    ))
  }
  sections
}

.rule_describe_unique <- function(rule) {
  cols <- rule@columns
  text <- if (length(cols) == 1L) {
    paste0("No two rows may have the same value in ", .rule_columns(cols), ".")
  } else {
    paste0(
      "No two rows may have the same combination of values in ",
      .rule_columns(cols), "."
    )
  }
  # dta_count_duplicates(): exact comparison (the reading guide), NA equal
  # to NA, NaN equal to NaN but not to NA.
  list(
    .rule_section("statement", text),
    .rule_section(
      "missing",
      paste0(
        "Two missing values are equal; so are two ",
        .RULE_LQ, "not a number", .RULE_RQ, " (NaN) values, which are not missing."
      ),
      "Missing values"
    )
  )
}

.rule_describe_condition <- function(rule) {
  if_parts <- .rule_clause(rule@condition, "if")
  then_parts <- .rule_clause(rule@then, "then")
  if_text <- vapply(if_parts, function(p) p$text, character(1))
  then_text <- vapply(then_parts, function(p) p$text, character(1))
  problems <- c(
    unlist(lapply(if_parts, function(p) p$problems), use.names = FALSE),
    unlist(lapply(then_parts, function(p) p$problems), use.names = FALSE)
  )
  if (!is.list(rule@condition) || !is.list(rule@then)) {
    problems <- c(problems, "A clause is not a list of columns and tests.")
  }
  defects <- c(
    unlist(lapply(if_parts, function(p) p$defects), use.names = FALSE),
    unlist(lapply(then_parts, function(p) p$defects), use.names = FALSE)
  )
  if (length(then_parts) == 0L) {
    then_text <- "nothing is required"
  }

  sections <- list()
  if (length(if_parts) <= 1L) {
    lead <- if (length(if_parts) == 0L) "In every row" else paste0("When ", if_text)
    if (length(then_text) == 1L) {
      sections[[1]] <- .rule_section("statement", paste0(lead, ", ", then_text, "."))
    } else {
      sections[[1]] <- .rule_section(
        "statement", paste0(lead, ", all of the following must hold:"),
        items = then_text
      )
    }
  } else {
    sections[[1]] <- .rule_section(
      "statement", "When all of the following are true:",
      items = if_text
    )
    if (length(then_text) == 1L) {
      sections[[2]] <- .rule_section("statement", paste0("Then ", then_text, "."))
    } else {
      sections[[2]] <- .rule_section(
        "statement", "Then all of the following must hold:",
        items = then_text
      )
    }
  }

  if_notes <- .rule_clause_notes(if_parts, "if")
  then_notes <- .rule_clause_notes(then_parts, "then", if_parts = if_parts)
  c(
    sections,
    list(
      .rule_problem_section(problems, defects),
      .rule_note_section("missing", "Missing values", c(if_notes$missing, then_notes$missing)),
      .rule_note_section("nonnumeric", "Non-numeric values", c(if_notes$nonnum, then_notes$nonnum))
    )
  )
}

.rule_scope_phrase <- function(scope) {
  scope <- scope %||% "any"
  if (identical(scope, "all")) {
    "every row"
  } else if (identical(scope, "any")) {
    "at least one row"
  } else {
    # The evaluator reads anything but "all" as "any".
    paste0(
      "at least one row (the unsupported scope ", .rule_and(.rule_values(scope)),
      " is read as at least one row)"
    )
  }
}

.rule_describe_constraint <- function(cst, index) {
  if (!is.list(cst)) {
    return(list(
      text = paste0("Requirement ", index, " is not a list of settings (", .rule_and(.rule_values(cst)), ")."),
      problem = paste0("Requirement ", index, " is not a list of settings.")
    ))
  }
  id <- .rule_name(cst[["id"]] %||% paste0("constraint_", index))
  ctype <- as.character(cst[["type"]] %||% "")
  msg <- cst[["message"]]
  defect <- NULL
  # The evaluator reports `message` as it is; anything but a single text makes
  # its vapply(..., character(1)) stop when a group fails the requirement.
  message <- if (is.null(msg)) {
    "A violation is reported with a message naming the group and its rows."
  } else if (is.character(msg) && length(msg) == 1L) {
    paste0(
      "A violation is reported with the message ",
      .rule_value(if (is.na(msg)) "NA" else msg), "."
    )
  } else {
    defect <- paste0(
      "Requirement ", id, " has a message that is not a single text: when a group",
      " fails it, the validation stops with an error instead of reporting the violation."
    )
    paste0(
      "Its message is given as ",
      if (length(msg) == 0L) "no value" else .rule_and(.rule_values(msg)),
      " instead of a single text."
    )
  }

  if (identical(ctype, "requires")) {
    text <- paste0(
      id, ": If ", .rule_scope_phrase(cst[["if_scope"]]), " of a group meets condition ",
      .rule_name(cst[["if"]] %||% ""), ", then ", .rule_scope_phrase(cst[["then_scope"]]),
      " of that group must meet condition ", .rule_name(cst[["then"]] %||% ""),
      ". ", message
    )
    return(list(text = text, problem = NULL, defect = defect))
  }
  if (identical(ctype, "mutually_exclusive")) {
    text <- paste0(
      id, ": Within a group, it must not happen that ",
      .rule_scope_phrase(cst[["left_scope"]]), " meets condition ", .rule_name(cst[["left"]] %||% ""),
      " and also ", .rule_scope_phrase(cst[["right_scope"]]), " meets condition ",
      .rule_name(cst[["right"]] %||% ""), ". ", message
    )
    return(list(text = text, problem = NULL, defect = defect))
  }

  # The evaluator skips a requirement of any other type without reporting it,
  # and never uses its message; the rule itself still runs.
  others <- cst[setdiff(names(cst), c("id", "type"))]
  settings <- if (length(others) > 0L) {
    paste0(
      " Its other settings are ",
      .rule_and(vapply(names(others), function(nm) {
        paste0(.rule_name(nm), " ", .rule_and(.rule_values(others[[nm]])))
      }, character(1))),
      "."
    )
  } else {
    ""
  }
  list(
    text = paste0(
      id, ": This requirement has the unsupported type ", .rule_value(ctype),
      "; it is never checked.", settings
    ),
    problem = NULL,
    defect = paste0("Requirement ", id, " has an unsupported type and is never checked.")
  )
}

.rule_describe_group <- function(rule) {
  group_by <- rule@group_by
  grouping <- if (length(group_by) == 1L) {
    paste0("the value in ", .rule_columns(group_by))
  } else {
    paste0("the combination of values in ", .rule_columns(group_by))
  }
  conditions <- rule@conditions %||% list()
  cond_names <- names(conditions) %||% rep("", length(conditions))
  # A malformed constraint is described as a problem further down, not here.
  if_only <- tryCatch(
    dta_group_if_only_conditions(rule@constraints %||% list()),
    error = function(e) character(0)
  )

  problems <- character(0)
  defects <- character(0)
  cond_items <- character(0)
  missing <- "A missing value in a grouping column counts as a value of its own: rows missing it are grouped together."
  nonnum <- character(0)
  for (i in seq_along(conditions)) {
    parts <- .rule_clause(conditions[[i]], "if")
    texts <- vapply(parts, function(p) p$text, character(1))
    cond_items <- c(cond_items, paste0(
      .rule_name(cond_names[[i]]), ": a row in which ",
      if (length(texts) == 0L) "nothing is tested" else .rule_and(texts)
    ))
    problems <- c(problems, unlist(lapply(parts, function(p) p$problems), use.names = FALSE))
    defects <- c(defects, unlist(lapply(parts, function(p) p$defects), use.names = FALSE))
    # An IF-only condition keeps non-number rows in scope in the validator, so
    # it gets the outcomes of a conditional rule's IF (see
    # dta_group_if_only_conditions()), worded for a named group condition.
    notes <- .rule_clause_notes(
      parts, if (cond_names[[i]] %in% if_only) "group_if" else "group", cond_names[[i]]
    )
    missing <- c(missing, notes$missing)
    nonnum <- c(nonnum, notes$nonnum)
  }

  constraints <- rule@constraints %||% list()
  req_items <- character(0)
  for (i in seq_along(constraints)) {
    d <- .rule_describe_constraint(constraints[[i]], i)
    req_items <- c(req_items, d$text)
    problems <- c(problems, d$problem)
    defects <- c(defects, d$defect)
  }

  list(
    .rule_section(
      "statement",
      paste0(
        "Rows are grouped by ", grouping,
        "; within each group, the requirements below must hold."
      )
    ),
    .rule_section("conditions", "", "Conditions", cond_items),
    .rule_section("requirements", "", "Requirements", req_items),
    .rule_problem_section(problems, defects),
    .rule_note_section("missing", "Missing values", missing),
    .rule_note_section("nonnumeric", "Non-numeric values", nonnum)
  )
}

# Structured description of a rule: id, type, kind label, whether the rule
# is free of problems (no Problem line: it can be evaluated and every part of
# it works as written), and its sections (role, label, text, items).
.rule_describe <- function(rule) {
  if (!.rule_is(rule, "DTARule")) {
    cli::cli_abort(
      "{.arg rule} must be a {.cls DTARule} object, not {.cls {class(rule)}}.",
      class = "dta_rule_phrasing_error"
    )
  }
  id <- tryCatch(rule@id, error = function(e) NULL)
  type <- tryCatch(rule@type, error = function(e) NULL)
  description <- tryCatch(rule@description, error = function(e) NULL)

  purpose <- if (is.character(description) && length(description) == 1L &&
    !is.na(description) && nzchar(description)) {
    list(.rule_section("purpose", description, "Purpose"))
  } else {
    list()
  }

  body <- if (.rule_is(rule, "DTARuleGroupCondition")) {
    .rule_describe_group(rule)
  } else if (.rule_is(rule, "DTARuleColCondition")) {
    .rule_describe_condition(rule)
  } else if (.rule_is(rule, "DTARuleColRange")) {
    .rule_describe_range(rule)
  } else if (.rule_is(rule, "DTARuleColUnique")) {
    .rule_describe_unique(rule)
  } else {
    shown <- if (is.character(type) && length(type) == 1L && !is.na(type)) {
      type
    } else {
      class(rule)[[1]]
    }
    list(.rule_section(
      "problem",
      paste0(
        "This rule has the unsupported type ", .RULE_LSQ, .rule_escape_text(shown),
        .RULE_RSQ, "; its parameters cannot be described."
      )
    ))
  }

  sections <- Filter(Negate(is.null), c(purpose, body))
  list(
    id = id,
    type = type,
    kind = .rule_kind_label(rule),
    valid = !any(vapply(sections, function(s) identical(s$role, "problem"), logical(1))),
    sections = sections
  )
}

# ---------------------------------------------------------------------------
# Layout: the one place that decides lines, labels and bullets
# ---------------------------------------------------------------------------

# One row per output line: role, label ("" for none), text ("" for a label
# that introduces a list) and bullet (TRUE for a list item).
.rule_lines <- function(rule) {
  desc <- .rule_describe(rule)
  role <- character(0)
  label <- character(0)
  text <- character(0)
  bullet <- logical(0)
  add <- function(r, l, t, b) {
    role <<- c(role, r)
    label <<- c(label, l)
    text <<- c(text, t)
    bullet <<- c(bullet, b)
  }
  for (s in desc$sections) {
    if (nzchar(s$text) || (nzchar(s$label) && length(s$items) > 0L)) {
      add(s$role, s$label, s$text, FALSE)
    }
    for (item in s$items) {
      add(s$role, "", item, TRUE)
    }
  }
  data.frame(
    role = role, label = label, text = text, bullet = bullet,
    stringsAsFactors = FALSE
  )
}

# Plain text, one line per row; list items start with a bullet.
.rule_text <- function(rule) {
  lines <- .rule_lines(rule)
  out <- ifelse(
    lines$bullet,
    paste0(.RULE_BULLET, " ", lines$text),
    ifelse(
      nzchar(lines$label),
      ifelse(nzchar(lines$text), paste0(lines$label, ": ", lines$text), paste0(lines$label, ":")),
      lines$text
    )
  )
  paste(out, collapse = "\n")
}

# One line, without purpose and notes: the statement and its lists.
.rule_summary <- function(rule) {
  lines <- .rule_lines(rule)
  keep <- !lines$role %in% c("purpose", "missing", "nonnumeric")
  lines <- lines[keep, , drop = FALSE]
  out <- character(0)
  i <- 1L
  n <- nrow(lines)
  while (i <= n) {
    if (lines$bullet[[i]]) {
      j <- i
      while (j < n && lines$bullet[[j + 1L]]) j <- j + 1L
      # Sentence items keep their own period and are joined by a space;
      # fragment items are joined by "; " and the run ends with a period.
      items <- lines$text[i:j]
      sentence <- grepl("[.]$", items)
      sep <- ifelse(sentence, " ", "; ")
      run <- paste0(paste0(items[-length(items)], sep[-length(items)], collapse = ""), items[length(items)])
      if (!grepl("[.]$", run)) {
        run <- paste0(run, ".")
      }
      out <- c(out, run)
      i <- j + 1L
      next
    }
    out <- c(out, if (nzchar(lines$label[[i]])) {
      if (nzchar(lines$text[[i]])) paste0(lines$label[[i]], ": ", lines$text[[i]]) else paste0(lines$label[[i]], ":")
    } else {
      lines$text[[i]]
    })
    i <- i + 1L
  }
  paste(out, collapse = " ")
}

# HTML <dt>/<dd> pairs for a definition list: each label starts a term,
# unlabelled statements go under "Rule"; paragraphs are <p>, lists <ul>.
.rule_html_dl <- function(rule) {
  lines <- .rule_lines(rule)
  esc <- .report_html_escape
  terms <- character(0)
  bodies <- list()
  i <- 1L
  n <- nrow(lines)
  in_rule <- FALSE
  while (i <= n) {
    if (nzchar(lines$label[[i]])) {
      terms <- c(terms, lines$label[[i]])
      bodies[[length(terms)]] <- character(0)
      in_rule <- FALSE
    } else if (!in_rule && (!lines$bullet[[i]] || length(terms) == 0L)) {
      terms <- c(terms, "Rule")
      bodies[[length(terms)]] <- character(0)
      in_rule <- TRUE
    }
    k <- length(terms)
    if (lines$bullet[[i]]) {
      j <- i
      while (j < n && lines$bullet[[j + 1L]]) j <- j + 1L
      bodies[[k]] <- c(bodies[[k]], paste0(
        "<ul>", paste0("<li>", esc(lines$text[i:j]), "</li>", collapse = ""), "</ul>"
      ))
      i <- j + 1L
      next
    }
    # Trailing white space (a final line break of a Purpose) would only add
    # an empty line.
    text <- sub("[[:space:]]+$", "", lines$text[[i]])
    if (nzchar(text)) {
      # A line break of a multi-line Purpose (line feed, carriage return or
      # both) stays a line break, as in Word and Markdown; it is inserted
      # after escaping, so it cannot be markup.
      bodies[[k]] <- c(bodies[[k]], paste0(
        "<p>", gsub("\r\n?|\n", "<br>", esc(text)), "</p>"
      ))
    }
    i <- i + 1L
  }
  paste0(
    "<dt>", esc(terms), "</dt><dd>",
    vapply(bodies, paste, character(1), collapse = ""), "</dd>",
    collapse = ""
  )
}

# Word paragraph for a flextable cell: bold labels, bullet items, one line
# per row. The concatenated text equals .rule_text().
.rule_docx_paragraph <- function(rule) {
  lines <- .rule_lines(rule)
  chunks <- list()
  for (i in seq_len(nrow(lines))) {
    if (i > 1L) {
      chunks[[length(chunks) + 1L]] <- flextable::as_chunk("\n")
    }
    label <- lines$label[[i]]
    # The author's Purpose is verbatim; see .xml_safe_text().
    text <- .xml_safe_text(lines$text[[i]])
    if (lines$bullet[[i]]) {
      chunks[[length(chunks) + 1L]] <- flextable::as_chunk(paste0(.RULE_BULLET, " ", text))
    } else if (nzchar(label)) {
      chunks[[length(chunks) + 1L]] <- flextable::as_b(paste0(label, ":"))
      if (nzchar(text)) {
        chunks[[length(chunks) + 1L]] <- flextable::as_chunk(paste0(" ", text))
      }
    } else {
      chunks[[length(chunks) + 1L]] <- flextable::as_chunk(text)
    }
  }
  do.call(flextable::as_paragraph, chunks)
}

# How to read the rule descriptions; shown once per document.
.rule_reading_guide <- function() {
  c(
    "Each rule is checked separately for every file of the dataset.",
    paste(
      "Text values are compared exactly, including upper and lower case,",
      "spaces and punctuation."
    ),
    paste(
      "A value reads as a number when it is written with a point as the decimal",
      "separator, such as 1234.5 or -0.5; a value with a decimal comma or a",
      "thousands separator, such as 1,5 or 1 000, is not a number. Spaces before",
      "and after the value are ignored, and a few other notations also read as",
      "numbers, such as 1e3 for 1000 or 0x10 for 16. Numbers are compared",
      "exactly; 0 and minus 0 count as the same."
    ),
    paste(
      "Tests such as greater than, at least or from ... to ... read the column",
      "value as a number. Equal to, not equal to, one of and not one of compare",
      "numbers as numbers when the column holds numbers, as a column declared",
      "with a number type does, and every listed value reads as a number;",
      "otherwise they compare text. A number that is compared as text is first",
      "written out by the software, sometimes in a short scientific form:",
      "0.0001 becomes 1e-04 and 100000 can become 1e+05, so it matches only",
      "text written the same way."
    ),
    paste0(
      "In a column of dates, the tests that read numbers compare dates instead:",
      " a bound written as a number counts the days since 1 January 1970 (in a",
      " column of dates with a time, the seconds since the start of 1 January",
      " 1970 in UTC), and a bound written as text is read as a date written like",
      " 2024-01-31 (in a column of dates with a time, as midnight in the time",
      " zone of the computer that runs the validation); any other text stops the",
      " validation with an error, even",
      " text that reads as a number, such as ", .RULE_LQ, "5", .RULE_RQ, "."
    ),
    paste(
      "A value is missing when the cell holds no value; for tests that read",
      "numbers, a blank cell also counts as missing. A value is empty when it is",
      "missing or consists only of ordinary spaces, tabs, carriage returns or",
      "line breaks; a non-breaking space is not empty."
    ),
    paste(
      "A pattern is a Perl-compatible regular expression, checked against the",
      "value as text, so a number may read as 1e+05 and a true or false value as",
      "TRUE or FALSE. It is case-sensitive unless the pattern itself says",
      "otherwise, and is satisfied when it matches any part of the value, unless",
      "the pattern itself is tied to the start or the end of the value."
    ),
    paste(
      "If a column that a rule needs is not present in a file, the rule cannot",
      "be evaluated and fails for that file, except that a grouped condition",
      "passes for a file with no rows as long as its grouping columns are",
      "present."
    )
  )
}
