#!/usr/bin/env Rscript
# CI step: fail the job unless the qualification run's verdict is a PASS.
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
# Not only FAIL: a run that executed nothing is "NOT EXECUTED", and that is not a
# success either. Every verdict that says the evidence holds begins with PASS.
if (!startsWith(verdict, "PASS")) stop("The qualification run did not pass (verdict: ", verdict, ").")
