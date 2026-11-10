# The display text of a SAS date, datetime or time, as the SAS format would
# write it. Pure arithmetic on the SAS number: nothing here consults the
# locale (`format()`, `months()`, `as.POSIXlt()` all would), so the same value
# renders to the same text on every machine.

# Hard-coded for the same reason: month names must not follow the system locale.
`__DTAtools_sas_month_abb__` <- c(
  "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
  "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
)

#' @title Civil Date from a SAS Day Number
#' @description
#' The proleptic Gregorian year, month and day of a count of days since
#' 1960-01-01 (the SAS epoch), by the integer days-from-civil inverse. The shift
#' 715815 is the 719468 days between 0000-03-01 and 1970-01-01 less the 3653
#' days between 1960-01-01 and 1970-01-01.
#' @param days A whole-number numeric vector.
#' @return A list of numeric vectors `year`, `month` (1-12) and `day`.
#' @keywords internal
dta_sas_civil_from_days <- function(days) {
  z <- days + 715815
  era <- z %/% 146097
  doe <- z - era * 146097
  yoe <- (doe - doe %/% 1460 + doe %/% 36524 - doe %/% 146096) %/% 365
  doy <- doe - (365 * yoe + yoe %/% 4 - yoe %/% 100)
  mp <- (5 * doy + 2) %/% 153
  month <- ifelse(mp < 10, mp + 3, mp - 9)
  list(
    year = yoe + era * 400 + (month <= 2),
    month = month,
    day = doy - (153 * mp + 2) %/% 5 + 1
  )
}

#' @title Display Text of a SAS Date, Datetime or Time
#' @description
#' Renders SAS numbers the way the declared SAS display format writes them.
#'
#' | format | width | text |
#' | --- | --- | --- |
#' | `DATEw.` | 5-6 / 7-8 / 9-10 / 11 | `15AUG` / `15AUG25` / `15AUG2025` / `15-AUG-2025` |
#' | `DATETIMEw.` | 13-15 / 16-18 / 19-40 | `15AUG25:14:30` / `15AUG25:14:30:00` / `15AUG2025:14:30:00` |
#' | `TIMEw.d` | 5-7 / 8 and up | `14:30` / `14:30:00` (hour not padded, `.dd` appended while it fits) |
#'
#' Without a format, with a format of another kind, or with a width the table
#' has no text for, the result is ISO 8601: `YYYY-MM-DD`, `YYYY-MM-DDTHH:MM:SS`
#' or `HH:MM:SS` (a negative time gets a leading `-`). A year outside 1582-9999
#' is always ISO. The value is first rounded to the microsecond; fractions the text
#' does not show are then truncated, not rounded.
#' @param x A numeric vector: days since 1960-01-01 for `"Date"`, seconds since
#'   1960-01-01 00:00:00 for `"DateTime"`, seconds for `"Time"`.
#' @param kind `"Date"`, `"DateTime"` or `"Time"`.
#' @param format A SAS format such as `"DATE9."`, or `NULL`.
#' @return A character vector the length of `x`, trimmed, `NA` where `x` is not
#'   finite.
#' @keywords internal
dta_sas_render_temporal <- function(x, kind = c("Date", "DateTime", "Time"), format = NULL) {
  kind <- match.arg(kind)
  x <- as.numeric(x)
  out <- rep(NA_character_, length(x))
  ok <- is.finite(x)
  if (!any(ok)) {
    return(out)
  }

  parts <- if (!is.null(format) && length(format) == 1 && !is.na(format)) {
    dta_sas_format_parts(format)
  }
  if (!is.null(parts) && parts$family != tolower(kind)) {
    parts <- NULL
  }
  w <- if (is.null(parts)) NA_integer_ else parts$w
  d <- if (is.null(parts) || is.na(parts$d)) 0L else parts$d

  v <- round(x[ok], 6)
  out[ok] <- switch(kind,
    Date = dta_sas_date_text(floor(v), NULL, w),
    DateTime = {
      day <- floor(v / 86400)
      dta_sas_date_text(day, v - 86400 * day, w)
    },
    Time = dta_sas_time_text(v, w, d)
  )
  out
}

# `day` is whole days since 1960-01-01; `sod` the seconds into that day, or
# NULL for a date.
dta_sas_date_text <- function(day, sod, w) {
  cv <- dta_sas_civil_from_days(day)
  mon <- `__DTAtools_sas_month_abb__`[cv$month]
  dd <- sprintf("%02.0f", cv$day)
  yyyy <- sprintf("%04.0f", cv$year)
  yy <- sprintf("%02.0f", cv$year %% 100)
  iso <- paste0(yyyy, "-", sprintf("%02.0f", cv$month), "-", dd)

  if (is.null(sod)) {
    txt <- if (is.na(w)) {
      iso
    } else if (w >= 5 && w <= 6) {
      paste0(dd, mon)
    } else if (w >= 7 && w <= 8) {
      paste0(dd, mon, yy)
    } else if (w >= 9 && w <= 10) {
      paste0(dd, mon, yyyy)
    } else if (w == 11) {
      paste0(dd, "-", mon, "-", yyyy)
    } else {
      iso
    }
  } else {
    hh <- sprintf("%02.0f", sod %/% 3600)
    mm <- sprintf("%02.0f", (sod %% 3600) %/% 60)
    ss <- sprintf("%02.0f", floor(sod %% 60))
    iso <- paste0(iso, "T", hh, ":", mm, ":", ss)
    txt <- if (is.na(w)) {
      iso
    } else if (w >= 13 && w <= 15) {
      paste0(dd, mon, yy, ":", hh, ":", mm)
    } else if (w >= 16 && w <= 18) {
      paste0(dd, mon, yy, ":", hh, ":", mm, ":", ss)
    } else if (w >= 19 && w <= 40) {
      paste0(dd, mon, yyyy, ":", hh, ":", mm, ":", ss)
    } else {
      iso
    }
  }
  # Outside the years SAS formats can show (and the calendar it uses): ISO.
  odd <- cv$year < 1582 | cv$year > 9999
  txt[odd] <- iso[odd]
  txt
}

# `s` is seconds, possibly negative or beyond a day; `w` and `d` are the format
# width and decimals (`w` NA for ISO).
dta_sas_time_text <- function(s, w, d) {
  a <- abs(s)
  whole <- floor(a)
  micro <- pmin(round((a - whole) * 1e6), 999999)
  sign <- ifelse(s < 0, "-", "")
  h <- sprintf("%.0f", whole %/% 3600)
  mi <- sprintf("%02.0f", (whole %% 3600) %/% 60)
  se <- sprintf("%02.0f", whole %% 60)

  if (is.na(w) || w < 5) {
    return(paste0(sign, sprintf("%02.0f", whole %/% 3600), ":", mi, ":", se))
  }
  if (w < 8) {
    return(paste0(sign, h, ":", mi))
  }
  base <- paste0(sign, h, ":", mi, ":", se)
  # ".dd" is appended only for as many digits as fit in the width.
  k <- pmax(0, pmin(d, w - nchar(base) - 1))
  digits <- substr(paste0(sprintf("%06.0f", micro), strrep("0", 32)), 1, k)
  ifelse(k > 0, paste0(base, ".", digits), base)
}
