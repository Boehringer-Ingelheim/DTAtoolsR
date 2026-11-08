# Single source of the plain-English wording of validation rules.
#
# Every human-readable output (Word, Markdown, HTML report, template guide,
# console, Shiny) describes a rule through the functions in this file and
# nothing else. Rules are never shown as code: no YAML, no R, no code spans,
# no monospace. The wording below is the contract; the tests in
# tests/testthat/test-rulePhrasing.R are written from it by hand.
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
#   (by absolute value) the plain decimal form is used; outside it the form
#   "m times 10 to the power of e". -0 is written 0; Inf, -Inf and NaN are
#   "infinity", "minus infinity" and "not a number". Integer and double
#   storage read the same.
# * Logical values are "true" and "false"; a missing entry is "missing".
# * Lists read "a", "a or b", "a, b or c" (one of) and "a and b",
#   "a, b and c" (all of).
# * No mathematical symbols (they do not survive pdflatex); the source holds
#   only ASCII, with \u escapes for typographic quotes and bullets.
#
# ---------------------------------------------------------------------------
# Rule types
# ---------------------------------------------------------------------------
# Range (col_range):
#   "Every value in column C must be a number from a to b, inclusive."
#   min = -Inf: "... must be a number that is at most b."
#   max =  Inf: "... must be a number that is at least a."
#   both infinite: "... must be a number."
#   Missing values: "Missing values are not checked."
#   Non-numeric values: "A value that is present but is not a number
#   violates the rule."
#   More than one column: the sentence names all columns and the bounds, and
#   a Problem line says the rule cannot be evaluated (the evaluator aborts).
#
# Unique (col_unique):
#   "No two rows may have the same value in column C."
#   "No two rows may have the same combination of values in columns A and B."
#   Missing values: "Two missing values count as the same value."
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
#     followed by "If not, the message is "...""  or, without a message,
#     "If not, a message naming the group and its rows is reported."
#   Missing values: a missing value in a grouping column groups with the
#   other missing values; per condition column, as in the IF column of the
#   operator table, but a test that is not decided never meets a condition.
#
# Any other rule class: "This rule has the unsupported type 'x'; its
# parameters cannot be described."
#
# The author's description is always its own "Purpose" line and never
# replaces the mechanical wording.
#
# ---------------------------------------------------------------------------
# Operators (condition form / requirement form / a missing value)
# ---------------------------------------------------------------------------
# operator               | IF: C ...                 | THEN: C must ...        | missing
# -----------------------+---------------------------+-------------------------+--------
# equals, equal          | is equal to v             | be equal to v           | no
# not_equals, not_equal  | is not equal to v         | not be equal to v       | no
# in                     | is one of a, b or c       | be one of a, b or c     | (1)
# not_in                 | is not one of a, b or c   | not be one of a, b or c | (2)
# greater                | is greater than v         | be greater than v       | no
# less                   | is less than v            | be less than v          | no
# greater_equal, min     | is at least v             | be at least v           | no
# less_equal, max        | is at most v              | be at most v            | no
# range, min + max       | is from a to b, inclusive | be from a to b, incl.   | no
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
# "missing" says whether the test holds for a missing value:
#   (1) yes only when the values are a plain vector containing a missing
#       entry; a missing entry inside a list (YAML ~) does not match.
#   (2) no when the values are a plain vector containing a missing entry,
#       otherwise yes.
# Resulting notes (from the evaluator, R/evaluateRules.R):
#   IF, test fails on missing:  "If C is missing, the condition is not met
#                                and the row is not checked."
#   IF, test holds on missing:  "If C is missing, this part of the condition
#                                counts as met."
#   THEN, test fails:           "If C is missing in a row that meets the
#                                condition, the rule is violated."
#   THEN, test holds:           "If C is missing, this part of the
#                                requirement counts as met."
# A column whose tests cannot be evaluated gets no note.
# Columns with a numeric test (greater, less, greater_equal, less_equal, min,
# max, range) also get a note on values that are present but not numbers:
#   IF:   "A value in C that is not a number does not exempt the row from
#          the check."
#   THEN: "A value in C that is not a number violates the rule."
#   group condition: "A value in C that is not a number does not meet
#          condition N."

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

  sci <- NULL
  for (digits in 15:17) {
    sci <- sprintf("%.*e", digits - 1L, x)
    if (as.numeric(sci) == x) {
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
  paste0(sign, m, " times 10 to the power of ", exponent)
}

# One value as it appears in a rule: number, text, logical or missing.
.rule_value <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return("missing")
  }
  if (is.list(x)) {
    x <- x[[1L]]
    return(.rule_value(x))
  }
  if (length(x) != 1L) {
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

# One test on one column. Returns the condition form ("is ..."), the
# requirement form ("must be ..."), whether the test holds for a missing
# value, whether it compares numbers, and a problem sentence (or NULL) when
# the evaluator cannot run it.
.rule_predicate <- function(column, operator, value) {
  be <- function(phrase, na_true, numeric = FALSE) {
    list(
      is = paste0("is ", phrase), must = paste0("must be ", phrase),
      na_true = na_true, numeric = numeric, problem = NULL
    )
  }
  be_not <- function(phrase, na_true) {
    list(
      is = paste0("is not ", phrase), must = paste0("must not be ", phrase),
      na_true = na_true, numeric = FALSE, problem = NULL
    )
  }
  flat <- .rule_flat(value)
  n <- length(flat)
  has_missing <- n > 0L && anyNA(flat) ||
    (is.list(value) && any(vapply(value, length, integer(1)) == 0L))
  single_ok <- n == 1L && !anyNA(flat)
  col <- .rule_name(column)

  invalid <- function(need) {
    shown <- if (length(value) == 0L) "no value" else .rule_and(.rule_values(value))
    phrase <- paste0(
      "the test ", .RULE_LSQ, operator, .RULE_RSQ, " with ", shown
    )
    list(
      is = paste0("passes ", phrase), must = paste0("must pass ", phrase),
      na_true = FALSE, numeric = FALSE,
      problem = paste0(
        "The test ", .RULE_LSQ, operator, .RULE_RSQ, " on column ", col,
        " needs ", need, "."
      )
    )
  }

  single <- c(
    "equals", "equal", "not_equals", "not_equal", "greater", "less",
    "greater_equal", "less_equal", "min", "max", "pattern"
  )
  if (operator %in% single && !single_ok) {
    return(invalid("exactly one value that is not missing"))
  }

  switch(operator,
    equals = ,
    equal = be(paste0("equal to ", .rule_value(flat)), FALSE),
    not_equals = ,
    not_equal = be_not(paste0("equal to ", .rule_value(flat)), FALSE),
    `in` = {
      if (n < 1L) {
        return(invalid("one or more values"))
      }
      out <- be(paste0("one of ", .rule_or(.rule_values(value))), !is.list(value) && anyNA(value))
      out$list_missing <- is.list(value) && has_missing
      out
    },
    not_in = {
      if (n < 1L) {
        return(invalid("one or more values"))
      }
      out <- be_not(
        paste0("one of ", .rule_or(.rule_values(value))),
        !(!is.list(value) && anyNA(value))
      )
      out$list_missing <- is.list(value) && has_missing
      out
    },
    greater = be(paste0("greater than ", .rule_value(flat)), FALSE, TRUE),
    less = be(paste0("less than ", .rule_value(flat)), FALSE, TRUE),
    greater_equal = ,
    min = be(.rule_band(flat, Inf), FALSE, TRUE),
    less_equal = ,
    max = be(.rule_band(-Inf, flat), FALSE, TRUE),
    range = {
      if (n != 2L || anyNA(flat)) {
        return(invalid("exactly two values that are not missing, the lower and upper bound"))
      }
      bounds <- .rule_bounds(value)
      be(.rule_band(bounds[[1]], bounds[[2]]), FALSE, TRUE)
    },
    pattern = list(
      is = paste0("matches the pattern ", .rule_value(flat)),
      must = paste0("must match the pattern ", .rule_value(flat)),
      na_true = FALSE, numeric = FALSE, problem = NULL
    ),
    empty = {
      if (!(isTRUE(flat) || isFALSE(flat))) {
        return(invalid("the value true or false"))
      }
      if (isTRUE(flat)) be("empty", TRUE) else be_not("empty", FALSE)
    },
    {
      shown <- if (length(value) == 0L) "no value" else .rule_and(.rule_values(value))
      phrase <- paste0(
        "the unsupported test ", .RULE_LSQ, .rule_escape_text(operator), .RULE_RSQ,
        " with ", shown
      )
      list(
        is = paste0("passes ", phrase), must = paste0("must pass ", phrase),
        na_true = FALSE, numeric = FALSE,
        problem = paste0(
          "The test ", .RULE_LSQ, .rule_escape_text(operator), .RULE_RSQ,
          " on column ", col, " is not supported."
        )
      )
    }
  )
}

# All tests on one column, as one phrase with the column as subject.
# `form` is "if" (condition form) or "then" (requirement form).
.rule_column_clause <- function(column, spec, form) {
  col <- .rule_name(column)
  bad <- function(phrase_is, phrase_must, problem) {
    list(
      text = paste0(col, " ", if (form == "if") phrase_is else phrase_must),
      na_true = FALSE, numeric = FALSE, list_missing = FALSE,
      problems = problem
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
      if (!is.null(p_lo$problem) || !is.null(p_hi$problem)) {
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
    na_true = all(vapply(preds, function(p) isTRUE(p$na_true), logical(1))),
    numeric = any(vapply(preds, function(p) isTRUE(p$numeric), logical(1))),
    list_missing = any(vapply(preds, function(p) isTRUE(p$list_missing), logical(1))),
    problems = unlist(lapply(preds, function(p) p$problem), use.names = FALSE)
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
      na_true = FALSE, numeric = FALSE, list_missing = FALSE,
      problems = "An entry does not name its column."
    )))
  }
  lapply(seq_along(map), function(i) {
    out <- .rule_column_clause(cols[[i]], map[[i]], form)
    out$column <- cols[[i]]
    out
  })
}

# Missing-value and non-number notes for one clause.
.rule_clause_notes <- function(parts, form, condition_name = NULL) {
  missing <- character(0)
  nonnum <- character(0)
  for (p in parts) {
    # A column whose tests cannot be evaluated gets no note: the Problem line
    # already says the rule cannot run.
    if (is.na(p$column) || length(p$problems) > 0L) {
      next
    }
    col <- .rule_name(p$column)
    list_note <- if (isTRUE(p$list_missing)) {
      " The missing entry in the list of values does not match a missing value."
    } else {
      ""
    }
    if (identical(form, "if")) {
      missing <- c(missing, paste0(if (p$na_true) {
        paste0("If ", col, " is missing, this part of the condition counts as met.")
      } else {
        paste0("If ", col, " is missing, the condition is not met and the row is not checked.")
      }, list_note))
      if (p$numeric) {
        nonnum <- c(nonnum, paste0(
          "A value in ", col, " that is not a number does not exempt the row from the check."
        ))
      }
    } else if (identical(form, "then")) {
      missing <- c(missing, paste0(if (p$na_true) {
        paste0("If ", col, " is missing, this part of the requirement counts as met.")
      } else {
        paste0("If ", col, " is missing in a row that meets the condition, the rule is violated.")
      }, list_note))
      if (p$numeric) {
        nonnum <- c(nonnum, paste0("A value in ", col, " that is not a number violates the rule."))
      }
    } else {
      cond <- .rule_name(condition_name)
      missing <- c(missing, paste0(if (p$na_true) {
        paste0("If ", col, " is missing, this part of condition ", cond, " counts as met.")
      } else {
        paste0("A row in which ", col, " is missing does not meet condition ", cond, ".")
      }, list_note))
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

.rule_problem_section <- function(problems) {
  if (length(problems) == 0L) {
    return(NULL)
  }
  .rule_section(
    "problem",
    paste(c("This rule cannot be evaluated.", unique(problems)), collapse = " "),
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
    sections[[2]] <- .rule_section("missing", "Missing values are not checked.", "Missing values")
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
  list(
    .rule_section("statement", text),
    .rule_section("missing", "Two missing values count as the same value.", "Missing values")
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
  then_notes <- .rule_clause_notes(then_parts, "then")
  c(
    sections,
    list(
      .rule_problem_section(problems),
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
      "at least one row (the unsupported scope ", .rule_value(scope),
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
  scope_problem <- NULL
  message <- if (!is.null(cst[["message"]])) {
    paste0("If not, the message is ", .rule_value(cst[["message"]]), ".")
  } else {
    "If not, a message naming the group and its rows is reported."
  }

  if (identical(ctype, "requires")) {
    text <- paste0(
      id, ": If ", .rule_scope_phrase(cst[["if_scope"]]), " of a group meets condition ",
      .rule_name(cst[["if"]] %||% ""), ", then ", .rule_scope_phrase(cst[["then_scope"]]),
      " of that group must meet condition ", .rule_name(cst[["then"]] %||% ""),
      ". ", message
    )
    return(list(text = text, problem = scope_problem))
  }
  if (identical(ctype, "mutually_exclusive")) {
    text <- paste0(
      id, ": Within a group, it must not happen that ",
      .rule_scope_phrase(cst[["left_scope"]]), " meets condition ", .rule_name(cst[["left"]] %||% ""),
      " and also ", .rule_scope_phrase(cst[["right_scope"]]), " meets condition ",
      .rule_name(cst[["right"]] %||% ""), ". ", message
    )
    return(list(text = text, problem = scope_problem))
  }

  # The evaluator skips a requirement of any other type without reporting it.
  others <- cst[setdiff(names(cst), c("id", "type", "message"))]
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
      "; it is never checked.", settings, " ", message
    ),
    problem = paste0("Requirement ", id, " has an unsupported type and is never checked.")
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

  problems <- character(0)
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
    notes <- .rule_clause_notes(parts, "group", cond_names[[i]])
    missing <- c(missing, notes$missing)
    nonnum <- c(nonnum, notes$nonnum)
  }

  constraints <- rule@constraints %||% list()
  req_items <- character(0)
  for (i in seq_along(constraints)) {
    d <- .rule_describe_constraint(constraints[[i]], i)
    req_items <- c(req_items, d$text)
    problems <- c(problems, d$problem)
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
    .rule_problem_section(problems),
    .rule_note_section("missing", "Missing values", missing),
    .rule_note_section("nonnumeric", "Non-numeric values", nonnum)
  )
}

# Structured description of a rule: id, type, kind label, whether the rule
# can be evaluated, and its sections (role, label, text, items).
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
      run <- paste(lines$text[i:j], collapse = "; ")
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

# Markdown block: optional bold id and kind, then paragraphs and real "-"
# lists. All text is escaped, so a value can never become markup.
.rule_md <- function(rule, heading = TRUE) {
  lines <- .rule_lines(rule)
  esc <- function(x) .plain_text_to_md(x)
  blocks <- character(0)
  if (isTRUE(heading)) {
    id <- tryCatch(rule@id, error = function(e) NULL)
    kind <- esc(.rule_kind_label(rule))
    blocks <- c(blocks, if (is.character(id) && length(id) == 1L && nzchar(id)) {
      paste0("**", esc(id), "** \u00b7 ", kind)
    } else {
      paste0("**", kind, "**")
    })
  }
  i <- 1L
  n <- nrow(lines)
  while (i <= n) {
    if (lines$bullet[[i]]) {
      j <- i
      while (j < n && lines$bullet[[j + 1L]]) j <- j + 1L
      blocks <- c(blocks, paste0("- ", esc(lines$text[i:j]), collapse = "\n"))
      i <- j + 1L
      next
    }
    label <- lines$label[[i]]
    text <- lines$text[[i]]
    blocks <- c(blocks, if (nzchar(label)) {
      if (nzchar(text)) paste0("**", esc(label), ":** ", esc(text)) else paste0("**", esc(label), ":**")
    } else {
      esc(text)
    })
    i <- i + 1L
  }
  paste(blocks, collapse = "\n\n")
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
    if (nzchar(lines$text[[i]])) {
      bodies[[k]] <- c(bodies[[k]], paste0("<p>", esc(lines$text[[i]]), "</p>"))
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
    text <- lines$text[[i]]
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
      "Tests such as greater than, at least or from ... to ... read the column",
      "value as a number. Equal to and one of compare numbers as numbers when the",
      "column holds numbers and the stated value reads as a number; otherwise",
      "they compare the text."
    ),
    paste(
      "A value is missing when the cell holds no value; for tests that read",
      "numbers, a blank cell also counts as missing. A value is empty when it is",
      "missing or consists only of spaces, tabs or line breaks."
    ),
    paste(
      "A pattern is a Perl-compatible regular expression. It is case-sensitive",
      "and is satisfied when it matches any part of the value, unless the",
      "pattern itself is tied to the start or the end of the value."
    ),
    paste(
      "If a column that a rule needs is not present in a file, the rule cannot",
      "be evaluated and fails for that file."
    )
  )
}
