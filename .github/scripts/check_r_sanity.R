#!/usr/bin/env Rscript
# Replacement for three of precommit's trivial R hooks, run against R/:
#
#   - parsable-R: every file must parse. Kept even though roxygenise() (run
#     right after this in CI) would hit the same syntax error, because a
#     parse failure reported here names the file directly instead of
#     surfacing as a confusing pkgload/roxygen2 stack trace.
#   - no-browser-statement: forgotten `browser()` calls left in from
#     debugging.
#   - no-debug-statement: forgotten `debug()` calls (the "single-step this
#     function from now on" call, not `debugonce()`/`undebug()`, which are
#     different functions and not flagged).
#
# All three are cheap (parsing R/ takes well under a second) and have a
# strictly good cost/benefit here, so all three are kept.
#
# Implemented via getParseData() rather than a text regex so that
# `browser()`/`debug()` mentioned in a comment or a string literal is never
# flagged -- only an actual call in code is.

r_dir <- "R"
r_files <- sort(list.files(r_dir, pattern = "\\.R$", full.names = TRUE))

parse_failures <- character()
flagged_calls <- data.frame(file = character(), line = integer(), call = character())

flagged_fns <- c("browser", "debug")

for (f in r_files) {
  parsed <- tryCatch(parse(f, keep.source = TRUE), error = function(e) conditionMessage(e))
  if (is.character(parsed)) {
    parse_failures[f] <- parsed
    next
  }
  pd <- utils::getParseData(parsed)
  # A call `fn(...)` tokenises as a SYMBOL_FUNCTION_CALL for `fn` immediately
  # followed by a '(' token: this excludes `debug <- function(...)` (that's a
  # SYMBOL on the left of an assignment, not SYMBOL_FUNCTION_CALL) and
  # `x$debug()` (a different symbol, "debug" as a list element, still tagged
  # SYMBOL_FUNCTION_CALL but a legitimate method call unrelated to base::debug
  # -- accepted as a rare false positive in exchange for staying simple).
  call_rows <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% flagged_fns, ]
  if (nrow(call_rows) > 0) {
    flagged_calls <- rbind(flagged_calls, data.frame(
      file = f, line = call_rows$line1, call = call_rows$text
    ))
  }
}

ok <- TRUE

if (length(parse_failures) > 0) {
  ok <- FALSE
  cat("Files that do not parse:\n\n")
  for (f in names(parse_failures)) {
    cat(sprintf("  %s: %s\n", f, parse_failures[f]))
  }
  cat("\n")
}

if (nrow(flagged_calls) > 0) {
  ok <- FALSE
  cat("browser()/debug() calls left in R/:\n\n")
  for (i in seq_len(nrow(flagged_calls))) {
    cat(sprintf(
      "  %s:%d  %s()\n", flagged_calls$file[i], flagged_calls$line[i], flagged_calls$call[i]
    ))
  }
  cat("\nRemove these before merging.\n")
}

if (!ok) {
  quit(status = 1)
}

cat("check_r_sanity.R: OK -- R/ parses cleanly with no browser()/debug() calls.\n")
