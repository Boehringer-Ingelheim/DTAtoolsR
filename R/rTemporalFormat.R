# The date/time formats of the R backend (`type: R Date`, `R POSIXct`, `R hms`).
#
# A format is a small, locale-independent subset of the strptime directives:
# `%Y %m %d %H %M %S %OS1..%OS6 %F %T` and a closed set of literals. It is read
# by ONE left-to-right tokeniser (dta_r_format_parse()), and the regex builder,
# the day-of-month check and the renderer all work from the tokens it returns, so
# they cannot disagree about what a format means.
#
# `strptime()` and `as.Date()` are never called on user data: they are lenient
# (`2024-1-5`, trailing text and extra spaces all parse), locale-dependent for
# month names, and do not read `%OSn` at all. The check is a fixed-width regex
# plus an arithmetic days-in-month test, like the SAS side's sasTemporalText.R.

`__DTAtools_r_temporal_types__` <- c("Date", "POSIXct", "hms")

# The only characters a format may contain besides directives. No digits (they
# would be ambiguous next to a fixed-width field), no braces, quotes or
# backslashes (the text flows into a regex and into messages), no other letters.
`__DTAtools_r_format_literals__` <- c(" ", "-", "/", ".", ",", ":", ";", "_", "T", "Z")

#' @title Default Format of an R Temporal Type
#' @description
#' The format an `R Date`, `R POSIXct` or `R hms` column gets when the
#' specification names none. The constructor stores it, so a supplier reading
#' the specification sees exactly what is enforced.
#' @param type `"Date"`, `"POSIXct"` or `"hms"`.
#' @return A format string, or `NULL` for any other type.
#' @keywords internal
dta_r_default_format <- function(type) {
  if (!is.character(type) || length(type) != 1L || is.na(type)) {
    return(NULL)
  }
  switch(type,
    Date = "%Y-%m-%d",
    POSIXct = "%Y-%m-%d %H:%M:%S",
    hms = "%H:%M:%S",
    NULL
  )
}

# A piece of a format as a message shows it: a control character (a tab, a line
# break) as its escape (`\t`), every other character as typed. encodeString()
# alone would also double a printable backslash. Bytes, so that text R cannot
# scan is still shown.
dta_r_shown <- function(x) {
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  control <- grepl("[[:cntrl:]]", chars, useBytes = TRUE)
  chars[control] <- vapply(chars[control], encodeString, character(1), USE.NAMES = FALSE)
  paste(chars, collapse = "")
}

#' @title Parse an R Temporal Format
#' @description
#' The single tokeniser behind every R temporal format. Directives are `%Y`
#' (4 digits), `%m`, `%d`, `%H`, `%M`, `%S`, `%OS1` to `%OS6` (seconds with an
#' optional fraction of up to that many digits), and `%F` (= `%Y-%m-%d`) and
#' `%T` (= `%H:%M:%S`), which are expanded here. Literals are a space,
#' `- / . , : ; _` and the letters `T` and `Z`.
#'
#' Refused: any other directive (bare `%OS`, `%%`, `%b`, `%B`, `%a`, `%A`, `%p`,
#' `%I`, `%y`, `%j`, `%z`, `%Z`, `%e`, a lone trailing `%`), any other literal, a
#' repeated year, month, day, hour or minute, more than one of `%S` and `%OSn`,
#' a `.` or digit directly after `%OSn`, and a format that is not one non-empty
#' piece of text. The family is `"Date"` (year, month and day, no time part),
#' `"POSIXct"` (date plus hour and minute) or `"hms"` (hour and minute, no date
#' part); a format that is none of those is refused.
#'
#' Messages quote the offending piece as a value; they are plain strings, never
#' templates.
#' @param format A single string.
#' @return A list: `ok` (`TRUE` exactly when `problem` is `NULL`), `problem` (one
#'   plain message or `NULL`), `tokens` (a list of `list(kind, text, n)`, `kind`
#'   one of `Y m d H M S OS lit`), `fields` (the subset of `Y m d H M S` present),
#'   `frac` (the `n` of `%OSn`, else `NA_integer_`) and `family`.
#' @keywords internal
dta_r_format_parse <- function(format) {
  tok <- function(kind, text = NA_character_, n = NA_integer_) {
    list(kind = kind, text = text, n = n)
  }
  tokens <- list()
  fail <- function(problem) {
    list(
      ok = FALSE, problem = problem, tokens = tokens, fields = character(0),
      frac = NA_integer_, family = NA_character_
    )
  }

  if (!is.character(format) || length(format) != 1L || is.na(format) || !nzchar(format)) {
    return(fail("The format must be one non-empty piece of text."))
  }
  chars <- tryCatch(strsplit(format, "", fixed = TRUE)[[1]], error = function(e) NULL)
  if (is.null(chars)) {
    return(fail("The format is not valid text."))
  }

  n_chars <- length(chars)
  i <- 1L
  while (i <= n_chars) {
    ch <- chars[[i]]
    if (ch == "%") {
      nxt <- if (i < n_chars) chars[[i + 1L]] else ""
      if (!nzchar(nxt)) {
        return(fail("The format ends with a lone '%'."))
      }
      if (nxt %in% c("Y", "m", "d", "H", "M", "S")) {
        tokens[[length(tokens) + 1L]] <- tok(nxt)
        i <- i + 2L
      } else if (nxt == "F") {
        tokens <- c(tokens, list(
          tok("Y"), tok("lit", "-"), tok("m"), tok("lit", "-"), tok("d")
        ))
        i <- i + 2L
      } else if (nxt == "T") {
        tokens <- c(tokens, list(
          tok("H"), tok("lit", ":"), tok("M"), tok("lit", ":"), tok("S")
        ))
        i <- i + 2L
      } else if (nxt == "O") {
        digit <- if (i + 3L <= n_chars && identical(chars[[i + 2L]], "S")) {
          chars[[i + 3L]]
        } else {
          ""
        }
        if (!digit %in% as.character(1:6)) {
          # dta_r_shown(): a tab or other control character in the format is
          # shown as its escape, not printed raw into the message.
          return(fail(paste0(
            "The directive '", dta_r_shown(paste(chars[i:min(i + 2L, n_chars)], collapse = "")),
            "' is not supported on its own; write %OS followed by one digit from 1 to 6, for example %OS3."
          )))
        }
        tokens[[length(tokens) + 1L]] <- tok("OS", n = as.integer(digit))
        i <- i + 4L
      } else {
        return(fail(paste0(
          "The directive '%", dta_r_shown(nxt), "' is not supported; the supported directives are ",
          "%Y %m %d %H %M %S %OS1 to %OS6 %F and %T."
        )))
      }
    } else {
      last <- if (length(tokens) > 0L) tokens[[length(tokens)]]$kind else ""
      if (identical(last, "OS") && (ch == "." || grepl("^[0-9]$", ch))) {
        return(fail(paste0(
          "The character '", dta_r_shown(ch), "' cannot directly follow %OSn, because it would read as part of the fraction."
        )))
      }
      if (!ch %in% `__DTAtools_r_format_literals__`) {
        return(fail(paste0(
          "The character '", dta_r_shown(ch), "' is not allowed in a format; only a space, - / . , : ; _ ",
          "and the letters T and Z may stand for themselves."
        )))
      }
      tokens[[length(tokens) + 1L]] <- tok("lit", ch)
      i <- i + 1L
    }
  }

  kinds <- vapply(tokens, function(t) t$kind, character(1))
  for (k in c("Y", "m", "d", "H", "M")) {
    if (sum(kinds == k) > 1L) {
      return(fail(paste0("The directive '%", k, "' appears more than once.")))
    }
  }
  if (sum(kinds %in% c("S", "OS")) > 1L) {
    return(fail("Seconds appear more than once; use only one of %S and %OSn."))
  }

  fields <- intersect(
    c("Y", "m", "d", "H", "M", "S"),
    ifelse(kinds == "OS", "S", kinds)
  )
  os <- which(kinds == "OS")
  frac <- if (length(os) == 1L) tokens[[os]]$n else NA_integer_

  has_date <- all(c("Y", "m", "d") %in% fields)
  no_date <- !any(c("Y", "m", "d") %in% fields)
  has_hm <- all(c("H", "M") %in% fields)
  family <- if (has_date && !any(c("H", "M", "S") %in% fields)) {
    "Date"
  } else if (has_date && has_hm) {
    "POSIXct"
  } else if (no_date && has_hm) {
    "hms"
  } else {
    NA_character_
  }

  problem <- if (is.na(family)) {
    paste0(
      "The format does not describe a Date, POSIXct or hms: a Date needs %Y, %m and %d and no time part, ",
      "a POSIXct also needs %H and %M, and an hms needs %H and %M and no date part."
    )
  }
  list(
    ok = is.null(problem), problem = problem, tokens = tokens, fields = fields,
    frac = frac, family = family
  )
}

#' @title Problem With an R Temporal Format
#' @description
#' The reason a format cannot be used for a declared type, or `NULL` when it can:
#' the tokeniser's own refusal, or a family that differs from `type`.
#' @param format A single string.
#' @param type `"Date"`, `"POSIXct"`, `"hms"` or `NULL` (any family).
#' @return One plain message, or `NULL`.
#' @keywords internal
dta_r_format_problem <- function(format, type = NULL) {
  parsed <- dta_r_format_parse(format)
  if (!parsed$ok) {
    return(parsed$problem)
  }
  if (!is.null(type) && !identical(parsed$family, type)) {
    return(paste0(
      "The format describes a ", parsed$family, ", which does not fit the type ", type, "."
    ))
  }
  NULL
}

# The regex of one directive. Only year, month and day capture, so the calendar
# check can read them back; everything else is a non-capturing group.
dta_r_token_regex <- function(tok) {
  switch(tok$kind,
    Y = "([0-9]{4})",
    m = "(0[1-9]|1[0-2])",
    d = "(0[1-9]|[12][0-9]|3[01])",
    H = "(?:[01][0-9]|2[0-3])",
    M = "[0-5][0-9]",
    S = "[0-5][0-9]",
    OS = paste0("[0-5][0-9](?:\\.[0-9]{1,", tok$n, "})?"),
    lit = if (identical(tok$text, ".")) "\\." else tok$text
  )
}

#' @title Regular Expression of an R Temporal Format
#' @description
#' The PCRE a value must match to fit the format: fixed widths and ranges per
#' directive (`%Y` `[0-9]{4}`, `%m` 01-12, `%d` 01-31, `%H` 00-23, `%M` and `%S`
#' 00-59, `%OSn` seconds plus an optional fraction of 1 to n digits), literals
#' escaped. Matching is case-sensitive and the value is not trimmed. The
#' expression is anchored at the start and, at the end, with PCRE's absolute
#' end-of-subject anchor rather than a dollar sign, because the dollar sign also
#' matches before a final line break.
#' @param parsed The result of [dta_r_format_parse()], with `ok = TRUE`.
#' @return A list: `regex`, and `groups`, the capture-group index of each of
#'   `Y`, `m` and `d` present in the format (a named integer vector).
#' @keywords internal
dta_r_temporal_regex <- function(parsed) {
  groups <- integer(0)
  pieces <- character(length(parsed$tokens))
  for (k in seq_along(parsed$tokens)) {
    tok <- parsed$tokens[[k]]
    if (tok$kind %in% c("Y", "m", "d")) {
      groups[tok$kind] <- length(groups) + 1L
    }
    pieces[[k]] <- dta_r_token_regex(tok)
  }
  list(regex = paste0("^", paste(pieces, collapse = ""), "\\z"), groups = groups)
}

# Days in each month of a Gregorian year; February gains a day in a leap year
# (divisible by 4, except centuries that are not divisible by 400). Year 0000
# counts as leap and 0100 does not.
dta_r_days_in_month <- function(year, month) {
  leap <- year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0)
  c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[month] + (month == 2 & leap)
}

#' @title Values That Do Not Fit an R Temporal Format
#' @description
#' The positions of the values that do not fit `format`: the regex first, then,
#' for a format with year, month and day, the days of the month on the captured
#' groups (so `2023-02-29` and `2024-04-31` fail, `2024-02-29` and `0000-02-29`
#' pass). Done once per distinct value. A value with a byte outside ASCII cannot
#' match and is reported without being handed to the regex engine. `NA` is never
#' reported.
#' @param txt A character vector (anything else goes through `as.character()`).
#' @param format A valid format string.
#' @return An integer vector of indices into `txt`.
#' @keywords internal
dta_r_temporal_bad <- function(txt, format) {
  parsed <- dta_r_format_parse(format)
  if (!parsed$ok) {
    cli::cli_abort(c(
      "Cannot check values against an unusable format.",
      "x" = "{parsed$problem}"
    ))
  }
  txt <- as.character(txt)
  present <- which(!is.na(txt))
  if (length(present) == 0L) {
    return(integer(0))
  }

  built <- dta_r_temporal_regex(parsed)
  groups <- built$groups
  fits <- dta_map_unique(txt[present], function(u) {
    ok <- rep(FALSE, length(u))
    ascii <- !is.na(iconv(u, "", "ASCII"))
    if (!any(ascii)) {
      return(ok)
    }
    v <- u[ascii]
    m <- regexpr(built$regex, v, perl = TRUE)
    fit <- m != -1L
    if (length(groups) == 3L && any(fit)) {
      start <- attr(m, "capture.start")
      len <- attr(m, "capture.length")
      part <- function(key) {
        g <- groups[[key]]
        as.integer(substr(v, start[, g], start[, g] + len[, g] - 1L))
      }
      day <- part("d")
      fit <- fit & !is.na(day) & day <= dta_r_days_in_month(part("Y"), part("m"))
    }
    ok[ascii] <- fit
    ok
  })
  present[!fits]
}

#' @title Render Typed Values as R Temporal Text
#' @description
#' The text of a `Date`, `POSIXct` (rendered in UTC) or `hms`/`difftime` (seconds)
#' vector under a format, driven by the same tokens as the checker. Pure
#' arithmetic on the number, never `format()`: fields are zero-padded to their
#' fixed width, the year to four digits, and the hours of an `hms` are the real
#' hour count (past 23 they come out as text the checker then rejects). `%OSn`
#' is the seconds plus the first n digits of the fraction, which is taken from
#' the value rounded to whole microseconds and then truncated, as the SAS
#' renderer does; `format(x + 0.3, "%OS3")` prints `.299`.
#' @param x A `Date`, `POSIXct`, `hms` or `difftime` vector.
#' @param format A valid format string.
#' @return A character vector the length of `x`, `NA` where `x` is not finite.
#' @keywords internal
dta_r_render_temporal <- function(x, format) {
  parsed <- dta_r_format_parse(format)
  if (!parsed$ok) {
    cli::cli_abort(c(
      "Cannot render values with an unusable format.",
      "x" = "{parsed$problem}"
    ))
  }

  if (inherits(x, "Date")) {
    num <- as.numeric(x)
    ok <- is.finite(num)
    day <- floor(num[ok])
    sod <- rep(0, sum(ok))
  } else if (inherits(x, "POSIXct")) {
    num <- as.numeric(x)
    ok <- is.finite(num)
    secs <- round(num[ok], 6)
    day <- floor(secs / 86400)
    sod <- secs - day * 86400
  } else if (inherits(x, "difftime")) {
    if (any(c("Y", "m", "d") %in% parsed$fields)) {
      cli::cli_abort("A time of day has no date parts to render.")
    }
    # as.numeric(x, units =) ignores the units of an hms-classed vector once
    # the hms namespace is loaded, so the seconds are taken from the number and
    # the attribute, as the SAS7BDAT normaliser does (no attribute: seconds).
    units <- attr(x, "units") %||% "secs"
    num <- as.numeric(as.difftime(as.numeric(unclass(x)), units = units), units = "secs")
    ok <- is.finite(num)
    day <- rep(NA_real_, sum(ok))
    sod <- round(num[ok], 6)
  } else {
    cli::cli_abort("{.arg x} must be a Date, POSIXct, hms or difftime vector.")
  }

  out <- rep(NA_character_, length(num))
  if (!any(ok)) {
    return(out)
  }

  # SAS day numbers count from 1960-01-01, 3653 days before 1970-01-01.
  civil <- dta_sas_civil_from_days(day + 3653)
  neg <- sod < 0
  a <- abs(sod)
  whole <- floor(a)
  micro <- pmin(round((a - whole) * 1e6), 999999)
  sign <- ifelse(neg, "-", "")
  k <- sum(ok)

  parts <- lapply(parsed$tokens, function(tok) {
    switch(tok$kind,
      Y = sprintf("%04.0f", civil$year),
      m = sprintf("%02.0f", civil$month),
      d = sprintf("%02.0f", civil$day),
      H = paste0(sign, sprintf("%02.0f", whole %/% 3600)),
      M = sprintf("%02.0f", (whole %% 3600) %/% 60),
      S = sprintf("%02.0f", whole %% 60),
      OS = paste0(
        sprintf("%02.0f", whole %% 60), ".",
        substr(sprintf("%06.0f", micro), 1, tok$n)
      ),
      lit = rep(tok$text, k)
    )
  })
  out[ok] <- do.call(paste0, parts)
  out
}
