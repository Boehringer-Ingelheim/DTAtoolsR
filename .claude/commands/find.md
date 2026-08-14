---
description: Locate code via the Explore subagent (Haiku) instead of grepping the main thread.
argument-hint: <what to locate, e.g. "where DTAFile validators are called">
---

Use the `Explore` subagent to locate the following in this package: $ARGUMENTS

Do not grep or glob from the main thread for this — that is what the subagent is
for. Spawn it, wait for its `path:line` report, then summarise the answer for me
in a few lines with clickable references.

If the question spans several independent things ("where is X used" *and* "which
files define Y"), spawn one `Explore` agent per thread of enquiry in a single
message so they run in parallel.
