suppressMessages(library(DTAtools))
source("inst/shiny/dta_app/R/utils_dta.R")

# --- build a tabular dataset with TWO tables sharing the same specs ----------
ds <- create_example_DTADataSetTabular(2)          # 1 table: tab1
tabs <- ds@tables
tabs[["tab2"]] <- tabs[["tab1"]]                   # 2nd table, same structure
ds@tables <- tabs
cat("tables:", paste(names(ds@tables), collapse = ", "), "\n")

# validate both tables -> validation_index / _store each length 2
ds <- check(ds, quiet = TRUE)
cat("after check: n_tables =", length(ds@tables),
    " n_vindex =", length(ds@validation_index),
    " n_vstore =", length(ds@validation_store), "\n")

# --- (A) OLD order: drop the table BEFORE clearing validation entries -------
old_order <- function(ds, table) {
  tabs <- ds@tables
  tabs[[table]] <- NULL
  ds@tables <- tabs                 # <-- validator sees vindex(2) > tables(1)
  vi <- ds@validation_index; vi[[table]] <- NULL; ds@validation_index <- vi
  vs <- ds@validation_store; vs[[table]] <- NULL; ds@validation_store <- vs
  ds
}
rA <- tryCatch(old_order(ds, "tab1"), error = function(e) conditionMessage(e))
cat("\n[A] OLD order removing a validated table -> ",
    if (is.character(rA)) paste("ERROR:", rA) else "ok", "\n")

# --- (B) NEW order: clear validation entries FIRST, then drop the table ------
new_order <- function(ds, table) {
  vi <- ds@validation_index; vi[[table]] <- NULL; ds@validation_index <- vi
  vs <- ds@validation_store; vs[[table]] <- NULL; ds@validation_store <- vs
  tabs <- ds@tables; tabs[[table]] <- NULL; ds@tables <- tabs
  ds
}
rB <- tryCatch(new_order(ds, "tab1"), error = function(e) conditionMessage(e))
if (is.character(rB)) {
  cat("[B] NEW order -> ERROR:", rB, "\n")
} else {
  cat("[B] NEW order removing a validated table -> ok; n_tables =",
      length(rB@tables), " n_vindex =", length(rB@validation_index),
      " n_vstore =", length(rB@validation_store), "\n")
}

# --- (C) exercise the actual app helper via a DTA ----------------------------
dta <- DTA(datasets = ds, metadata = create_example_DTAMetaData())
r <- dta_unload_table(dta, ds@name, "tab1")
cat("\n[C] dta_unload_table (current impl) REMOVE_OK:", r$ok, "\n")
if (!r$ok) cat("    ERROR:", r$error, "\n")
