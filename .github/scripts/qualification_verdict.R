#!/usr/bin/env Rscript
# CI step: fail the job when the qualification run's verdict is FAIL.
#
# A file rather than `Rscript -e '...'` in the workflow: a multi-line -e
# argument is cut at the first newline by Rscript on Windows, and R's unix
# front-end re-evaluates -e arguments so that a backslash is halved on the way
# in. A script file avoids both.

summaries <- list.files(
  "qualification-output",
  pattern = "^SUMMARY[.]txt$", recursive = TRUE, full.names = TRUE
)
if (length(summaries) == 0) stop("No qualification summary was produced.")
text <- readLines(summaries[[1]])
verdict <- sub("^Verdict[[:space:]]*:[[:space:]]*", "", grep("^Verdict", text, value = TRUE)[[1]])
cat("Verdict:", verdict, "\n")
if (startsWith(verdict, "FAIL")) stop("The qualification run failed.")
