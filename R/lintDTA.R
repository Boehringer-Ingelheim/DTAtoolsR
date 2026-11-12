# ---- Findings shape ----------------------------------------------------------

# One zero-row frame with the full column set, so "nothing to report" and "some
# findings" share exactly one shape all the way through rbind()/order().
dta_findings_empty <- function() {
  data.frame(
    object = character(0),
    id = character(0),
    severity = character(0),
    code = character(0),
    message = character(0),
    stringsAsFactors = FALSE
  )
}

# Accumulates findings row by row and turns them into a frame once. A closure
# rather than repeated rbind(): the collection validator runs on every
# construction and assignment, so the common case (nothing found) must cost no
# more than building five empty vectors.
dta_findings_builder <- function() {
  rows <- list()
  list(
    add = function(object, id, severity, code, message) {
      rows[[length(rows) + 1L]] <<- list(
        as.character(object), as.character(id)[1], severity, code,
        paste(as.character(message), collapse = " ")
      )
      invisible(NULL)
    },
    get = function() {
      if (length(rows) == 0L) {
        return(dta_findings_empty())
      }
      pull <- function(i) vapply(rows, `[[`, character(1), i)
      data.frame(
        object = pull(1L), id = pull(2L), severity = pull(3L),
        code = pull(4L), message = pull(5L),
        stringsAsFactors = FALSE
      )
    }
  )
}

dta_condition_text <- function(e) {
  gsub("\\s*\n\\s*", " ", conditionMessage(e))
}

# ---- Cross-object checks -----------------------------------------------------

# The declared SAS type of a column, `NA` when it declares none.
dta_column_type <- function(spec) {
  s <- spec@structure
  if (is.null(s) || !.structure_value_is_set(s@type)) {
    return(NA_character_)
  }
  as.character(s@type)
}

# Condition names a constraint list refers to that `conditions` does not define.
# Exact `[[` access: `$` would partial-match `left` onto `left_scope`.
dta_group_unknown_conditions <- function(constraints, condition_names) {
  refs <- character(0)
  if (is.list(constraints)) {
    for (cst in constraints) {
      if (!is.list(cst)) next
      for (key in c("left", "right", "if", "then")) {
        v <- cst[[key]]
        if (is.character(v) && length(v) == 1L && nzchar(v)) refs <- c(refs, v)
      }
    }
  }
  setdiff(unique(refs), as.character(condition_names))
}

dta_clauses_of <- function(rule) {
  switch(rule@type,
    col_condition = list(
      list(label = "condition", clause = rule_get_slot(rule, "condition")),
      list(label = "then", clause = rule_get_slot(rule, "then"))
    ),
    group_condition = {
      conds <- rule_get_slot(rule, "conditions")
      lapply(names(conds), function(nm) list(label = nm, clause = conds[[nm]]))
    },
    list()
  )
}

# Text form of an operand, for comparison against a codelist.
dta_operand_text <- function(v) {
  if (is.numeric(v)) {
    out <- dta_numeric_text(as.double(v))
    ifelse(is.na(out), as.character(v), out)
  } else {
    as.character(v)
  }
}

# Does any of `vals` break the column's own type, length, format or pattern?
# Reuses the evaluator behind the DTAColumnSpec validator by probing a copy of
# the spec whose `values` are the operands.
dta_operand_problems <- function(spec, vals) {
  ctype <- dta_column_type(spec)
  if (identical(ctype, "Bool")) {
    return(character(0))
  }
  problems <- character(0)
  if (!is.null(spec@structure)) {
    probe <- spec
    S7::prop(probe, "values", check = FALSE) <- if (is.numeric(vals)) vals else as.character(vals)
    S7::prop(probe, "examples", check = FALSE) <- NULL
    S7::prop(probe, "pattern", check = FALSE) <- NULL
    # The probe's operands travel as the spec's `values`; name them for what
    # they are here.
    problems <- sub("^values ", "condition value ", dta_spec_value_problems(probe))
  }
  pattern <- spec@pattern
  if (!is.null(pattern) && !is.na(pattern)) {
    txt <- dta_operand_text(vals)
    bad <- !is.na(txt) & !vapply(
      txt, function(t) isTRUE(grepl(pattern, t, perl = TRUE)), logical(1)
    )
    if (any(bad)) {
      problems <- c(problems, sprintf("'%s' does not match the pattern", txt[bad]))
    }
  }
  problems
}

#' @title Cross-Object Findings of a Column and Rule Set
#' @description
#' The single source of truth for "do these rules fit these columns": the
#' `DTAColumnSpecCollection` validator and [lint_dta()] both call it, so the two
#' cannot drift apart. Works on already-built objects, touches no data and
#' is cheap when there are no rules.
#' @param columns A list of `DTAColumnSpec`.
#' @param rules A list of `DTARule`, or `NULL`.
#' @param declared Character. Column ids to count as declared; defaults to the
#'   ids of `columns`. [lint_dta()] passes the ids of columns that failed to
#'   build, so a broken column does not also make its rules look dangling.
#' @return A data frame with columns `object` (`"column"`, `"rule"` or
#'   `"dataset"`), `id`, `severity` (`"error"` or `"warning"`), `code` and
#'   `message`; zero rows when clean.
#' @keywords internal
dta_specs_findings <- function(columns, rules, declared = NULL) {
  fb <- dta_findings_builder()
  columns <- columns %||% list()
  rules <- rules %||% list()

  col_ids <- vapply(columns, function(col) col@id[1], character(1), USE.NAMES = FALSE)
  declared <- unique(c(col_ids, declared))

  dup_cols <- unique(col_ids[duplicated(col_ids)])
  for (id in dup_cols) {
    fb$add(
      "column", id, "error", "column_id_duplicate",
      sprintf("Column id '%s' is declared more than once.", id)
    )
  }

  # Declared type per column, computed once: dta_column_type() is not free and
  # the rule loop below would otherwise repeat it per operator.
  col_types <- vapply(columns, dta_column_type, character(1), USE.NAMES = FALSE)

  # Char `$w.` narrower than the declared length: values would be truncated on
  # display although they pass the length check.
  for (k in seq_along(columns)) {
    col <- columns[[k]]
    s <- col@structure
    if (is.null(s) || !identical(col_types[[k]], "Char") ||
      is.null(s@length) || !.structure_value_is_set(s@format)) {
      next
    }
    w <- suppressWarnings(as.numeric(sub("^\\$([0-9]+)\\.$", "\\1", trimws(s@format))))
    if (length(w) == 1L && !is.na(w) && is.numeric(s@length) && w < s@length[1]) {
      fb$add(
        "column", col@id, "warning", "format_narrower_than_length",
        sprintf("Column '%s': format '%s' is narrower than its length %s.", col@id, s@format, s@length[1])
      )
    }
  }

  if (length(rules) == 0L) {
    return(fb$get())
  }

  by_id <- columns
  names(by_id) <- col_ids
  type_by_id <- col_types
  names(type_by_id) <- col_ids
  rule_ids <- vapply(rules, function(r) r@id[1], character(1), USE.NAMES = FALSE)
  for (id in unique(rule_ids[duplicated(rule_ids)])) {
    fb$add(
      "rule", id, "error", "rule_id_duplicate",
      sprintf("Rule id '%s' is used by more than one rule.", id)
    )
  }

  if (length(declared) == 0L) {
    fb$add(
      "dataset", NA_character_, "warning", "rules_without_columns",
      "Rules are defined but no column is declared, so they cannot be checked against columns."
    )
  }

  numeric_types <- c("Num", "Int")
  # Type of a declared column, NA when untyped or not declared (or broken).
  type_of <- function(col_name) {
    unname(type_by_id[col_name])
  }
  non_numeric <- function(col_name) {
    t <- type_of(col_name)
    !is.na(t) && !t %in% numeric_types
  }
  # The engine reads Date/Time/DateTime as text (as_r_type() maps them to
  # "character") and runs a numeric operator through a strict number
  # conversion, so every comparison on such a column is a false failure. A
  # numeric operator is therefore wrong on Char and on the three SAS date types.
  text_typed <- function(col_name) {
    type_of(col_name) %in% c("Char", "Date", "Time", "DateTime")
  }
  probe_cache <- new.env(parent = emptyenv())

  for (rule in rules) {
    rid <- rule@id[1]
    cols <- tryCatch(dta_rule_all_columns(rule), error = function(e) NULL)
    if (is.null(cols)) {
      fb$add(
        "rule", rid, "warning", "rule_not_checkable",
        sprintf("Rule '%s' has type '%s', which cannot be checked against columns.", rid, rule@type)
      )
      next
    }

    if (length(declared) > 0L) {
      missing_cols <- setdiff(cols, declared)
      if (length(missing_cols) > 0L) {
        fb$add(
          "rule", rid, "error", "rule_column_undeclared",
          sprintf("Rule '%s' uses undeclared column(s): %s.", rid, paste(missing_cols, collapse = ", "))
        )
      }
    }

    if (identical(rule@type, "col_range")) {
      bad <- Filter(non_numeric, as.character(cols))
      if (length(bad) > 0L) {
        fb$add(
          "rule", rid, "error", "rule_range_non_numeric",
          sprintf("Range rule '%s' is on non-numeric column(s): %s.", rid, paste(bad, collapse = ", "))
        )
      }
    }

    if (identical(rule@type, "group_condition")) {
      unknown <- dta_group_unknown_conditions(
        rule_get_slot(rule, "constraints"), names(rule_get_slot(rule, "conditions"))
      )
      if (length(unknown) > 0L) {
        fb$add(
          "rule", rid, "error", "group_constraint_unknown_condition",
          sprintf("Rule '%s' has a constraint on undefined condition(s): %s.", rid, paste(unknown, collapse = ", "))
        )
      }
    }

    for (cl in dta_clauses_of(rule)) {
      norm <- tryCatch(
        dta_normalize_conditions(cl$clause, arg = cl$label),
        error = function(e) e
      )
      if (inherits(norm, "error")) {
        fb$add(
          "rule", rid, "error", "rule_condition_operator_invalid",
          sprintf("Rule '%s', %s: %s", rid, cl$label, dta_condition_text(norm))
        )
        next
      }
      for (col_name in names(norm)) {
        ops <- norm[[col_name]]
        if (!is.list(ops) || length(ops) == 0L || is.null(names(ops)) || !all(nzchar(names(ops)))) {
          fb$add(
            "rule", rid, "error", "rule_condition_operator_invalid",
            sprintf("Rule '%s', %s: column '%s' must map operators to values.", rid, cl$label, col_name)
          )
          next
        }
        for (op in names(ops)) {
          where <- sprintf("Rule '%s', %s: column '%s', operator '%s'", rid, cl$label, col_name, op)
          if (!op %in% dta_condition_operators()) {
            fb$add(
              "rule", rid, "error", "rule_condition_operator_invalid",
              sprintf("%s is not a known operator.", where)
            )
            next
          }
          vals <- unlist(ops[[op]], use.names = FALSE)
          vals <- vals[!is.na(vals)]

          if (op %in% dta_numeric_condition_operators()) {
            if (text_typed(col_name)) {
              is_char <- identical(type_of(col_name), "Char")
              # A date column is text only in file reads; an in-memory Date-class
              # table compares correctly, so that case warns instead of
              # refusing a spec the engine can run.
              fb$add(
                "rule", rid, if (is_char) "error" else "warning", "rule_condition_numeric_on_text",
                sprintf(
                  "%s compares numbers, but the column is %s, which is read as text%s.",
                  where, type_of(col_name),
                  if (is_char) "" else ": file reads hold dates as text and a date is not a number, so compare with a quoted ISO value, and only on Date-class data"
                )
              )
            } else if (length(vals) > 0L &&
              any(is.na(suppressWarnings(as.numeric(as.character(vals)))))) {
              fb$add(
                "rule", rid, "error", "rule_condition_numeric_on_text",
                sprintf("%s needs a numeric bound.", where)
              )
            }
            next
          }

          if (!op %in% c("equals", "equal", "in", "not_equals", "not_equal", "not_in") ||
            length(vals) == 0L) {
            next
          }
          # YAML reads an unquoted Y/N/yes/no/on/off as a logical, which can
          # never equal the text a non-Bool column holds. Reported for the
          # negated operators too: `not_equals: N` then excludes nothing.
          if (is.logical(vals) && !is.na(type_of(col_name)) && type_of(col_name) != "Bool") {
            fb$add(
              "rule", rid, "error", "rule_condition_value_unfit",
              sprintf(
                "%s uses a logical value, but the column is %s. YAML reads an unquoted Y or N as TRUE or FALSE; quote it, e.g. equals: 'Y'.",
                where, type_of(col_name)
              )
            )
            next
          }
          # A negated comparison against a value outside the codelist is always
          # true: pointless but harmless, so it is not reported.
          positive <- op %in% c("equals", "equal", "in")
          if (!positive) next
          spec <- by_id[[col_name]]
          if (is.null(spec)) next

          if (!is.null(spec@values)) {
            if (!isTRUE(type_of(col_name) %in% numeric_types)) {
              outside <- !dta_operand_text(vals) %in% dta_operand_text(spec@values)
            } else {
              outside <- !suppressWarnings(as.numeric(as.character(vals))) %in%
                suppressWarnings(as.numeric(as.character(spec@values)))
            }
            if (any(outside)) {
              fb$add(
                "rule", rid, "error",
                "rule_condition_value_outside_values",
                sprintf(
                  "%s uses value(s) %s that the column's values list does not allow.", where,
                  paste(sprintf("'%s'", dta_operand_text(vals[outside])), collapse = ", ")
                )
              )
            }
          } else {
            # Many rules repeat the same operand on the same column; the probe
            # is the expensive part, so each distinct one runs once.
            key <- paste(c(col_name, class(vals)[1], vals), collapse = "\r")
            if (is.null(probe_cache[[key]])) {
              probe_cache[[key]] <- list(dta_operand_problems(spec, vals))
            }
            problems <- probe_cache[[key]][[1]]
            # A number against a text column: YAML has already dropped any
            # leading zero or trailing zero (`01` is 1, `1.10` is 1.1), so the
            # value no longer matches the text the data holds.
            if (is.numeric(vals) && identical(type_of(col_name), "Char")) {
              fb$add(
                "rule", rid, "warning", "rule_condition_value_unfit",
                sprintf("%s: a number was written where the column holds text; YAML drops leading and trailing zeros, so quote the value.", where)
              )
            }
            if (length(problems) > 0L) {
              fb$add(
                "rule", rid, "error", "rule_condition_value_unfit",
                sprintf("%s uses a value the column can never hold: %s.", where, paste(problems, collapse = "; "))
              )
            }
          }
        }
      }
    }
  }

  fb$get()
}

# ---- lint_dta ---------------------------------------------------------------

#' @title Lint a DTA Specification
#' @description
#' Reports every problem of a DTA document in one pass, instead of stopping at
#' the first constructor error. Columns and rules are built one by one so a
#' broken one becomes a finding, and the pieces that did build are then checked
#' against each other (rules naming undeclared columns, a range on a text
#' column, an `equals` value outside the column's `values`, ...). The same
#' cross-object checks are enforced by the `DTAColumnSpecCollection` class
#' itself, so an inconsistent specification can never exist as an object.
#'
#' @section Codes:
#' Errors: `yaml_unreadable`, `metadata_invalid`, `dataset_invalid`,
#' `dataset_id_missing`, `dataset_id_duplicate`, `column_spec_invalid`,
#' `column_id_duplicate`, `rule_invalid`, `rule_id_duplicate`,
#' `rule_column_undeclared`, `rule_range_non_numeric`,
#' `rule_condition_operator_invalid`, `rule_condition_numeric_on_text`,
#' `rule_condition_value_unfit`, `rule_condition_value_outside_values`,
#' `group_constraint_unknown_condition`, `file_container_invalid`,
#' `file_field_unknown`, `file_container_misplaced`, `file_count_bound_invalid`.
#'
#' Warnings: `rules_without_columns`, `rule_not_checkable`,
#' `format_narrower_than_length`, `metadata_import_issue`, `version_unquoted`
#' (an unquoted numeric `metadata.version` in a YAML file, which YAML reads
#' as a number: `1.10` becomes `1.1`).
#'
#' @param x One of: a path to a DTA YAML file; a parsed list (a DTA document,
#'   one dataset, or a list with `columns` and/or `rules`); a `DTA`,
#'   `DTADataSet` or `DTAColumnSpecCollection` object.
#' @param strict Logical. When `TRUE`, abort with class `dta_lint_error` if
#'   any finding has severity `error`; the findings are attached as
#'   `$findings` of the condition. Warnings never abort.
#' @return A data frame with columns `dataset`, `object`, `id`, `severity`,
#'   `code` and `message`, sorted; zero rows when the specification is clean.
#'   `dataset` is `""` for findings about the document or an anonymous
#'   specification.
#' @examples
#' file <- system.file("extdata", "clinical_dta.yaml", package = "DTAtools")
#' lint_dta(file)
#'
#' lint_dta(list(
#'   columns = list(list(id = "SEX", type = "SAS Char")),
#'   rules = list(list(id = "r1", type = "col_range", columns = "AGE", range = c(0, 1)))
#' ))
#' @export
lint_dta <- function(x, strict = FALSE) {
  findings <- if (is.character(x) && length(x) == 1L) {
    dta_lint_path(x)
  } else if (is.list(x) && !isS4(x) && !inherits(x, "S7_object")) {
    dta_lint_list(x)
  } else if (inherits(x, "S7_object")) {
    dta_lint_object(x)
  } else {
    cli::cli_abort(
      "{.arg x} must be a file path, a list, or a DTA, dataset or column spec collection."
    )
  }

  if (nrow(findings) > 0L) {
    o <- order(findings$dataset, findings$object, findings$id, findings$code,
      findings$message,
      method = "radix"
    )
    findings <- findings[o, , drop = FALSE]
    rownames(findings) <- NULL
  }

  if (isTRUE(strict)) {
    dta_lint_enforce(findings, warn = FALSE)
  }
  findings
}

dta_lint_with_dataset <- function(findings, dataset) {
  cbind(
    dataset = rep(as.character(dataset), nrow(findings)),
    findings,
    stringsAsFactors = FALSE
  )
}

dta_lint_empty <- function() {
  dta_lint_with_dataset(dta_findings_empty(), character(0))
}

dta_lint_row <- function(dataset, object, id, severity, code, message) {
  fb <- dta_findings_builder()
  fb$add(object, id, severity, code, message)
  dta_lint_with_dataset(fb$get(), dataset)
}

dta_lint_path <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("YAML file does not exist: {.file {path}}")
  }
  raw <- tryCatch(yaml::read_yaml(path), error = function(e) e)
  if (inherits(raw, "error")) {
    text <- dta_condition_text(raw)
    return(dta_lint_row(
      "", "document", NA_character_, "error", "yaml_unreadable",
      paste0(
        "The file is not valid YAML: ", text,
        if (grepl("alias", text, fixed = TRUE)) {
          " A value that starts with * must be quoted, e.g. container: \"*_SDTM.zip\"."
        }
      )
    ))
  }
  if (!is.list(raw)) {
    return(dta_lint_row(
      "", "document", NA_character_, "error", "yaml_unreadable",
      "The file does not hold a YAML mapping."
    ))
  }
  out <- dta_lint_list(raw)
  version <- raw[["metadata"]][["version"]]
  if (is.list(raw[["metadata"]]) && !is.null(version) && !is.character(version)) {
    out <- rbind(out, dta_lint_row(
      "", "metadata", "version", "warning", "version_unquoted",
      sprintf(
        "metadata.version is not quoted, so YAML read it as the number %s; quote it (e.g. \"1.10\").",
        format(version[[1]])
      )
    ))
  }
  out
}

dta_lint_entry_is_dataset <- function(e) {
  is.list(e) && any(c("id", "name", "type") %in% names(e))
}

dta_lint_list <- function(x) {
  nms <- names(x)
  if (any(c("datasets", "metadata") %in% nms)) {
    return(dta_lint_document(x))
  }
  if (dta_lint_entry_is_dataset(x)) {
    return(dta_lint_dataset_list(x, "#1"))
  }
  if (any(c("columns", "rules") %in% nms)) {
    return(dta_lint_specs_lists(x[["columns"]], x[["rules"]], dataset = ""))
  }
  # An unnamed list of datasets, as read_dataset_from_yaml() accepts.
  if (length(x) > 0L && is.null(nms) && dta_lint_entry_is_dataset(x[[1]])) {
    return(dta_lint_document(list(datasets = x)))
  }
  # Anything else is read as a document that holds no datasets (the shape
  # dta_from_list() always tolerated, with its own warnings), not as an error.
  dta_lint_document(x)
}

dta_lint_document <- function(x) {
  out <- dta_lint_empty()

  md <- x[["metadata"]]
  if (!is.null(md)) {
    out <- rbind(out, dta_lint_metadata_list(md))
  }

  datasets <- x[["datasets"]]
  if (length(datasets) == 0L) {
    return(out)
  }
  if (dta_lint_entry_is_dataset(datasets)) {
    datasets <- list(datasets)
  } else if (!all(vapply(datasets, dta_lint_entry_is_dataset, logical(1)))) {
    return(rbind(out, dta_lint_row(
      "", "document", NA_character_, "error", "dataset_invalid",
      "The 'datasets' section must be a list of datasets."
    )))
  }

  ids <- vapply(datasets, function(d) {
    id <- d[["id"]]
    if (is.null(id) || length(id) != 1L || !nzchar(as.character(id))) NA_character_ else as.character(id)
  }, character(1))
  for (dup in unique(ids[!is.na(ids) & duplicated(ids)])) {
    out <- rbind(out, dta_lint_row(
      dup, "dataset", dup, "error", "dataset_id_duplicate",
      sprintf("Dataset id '%s' is used by more than one dataset.", dup)
    ))
  }
  for (i in seq_along(datasets)) {
    label <- if (is.na(ids[[i]])) paste0("#", i) else ids[[i]]
    out <- rbind(out, dta_lint_dataset_list(datasets[[i]], label))
  }
  out
}

dta_lint_metadata_list <- function(md) {
  if (!is.list(md)) {
    return(dta_lint_row(
      "", "metadata", NA_character_, "error", "metadata_invalid",
      "The 'metadata' section must be a mapping."
    ))
  }
  # Same preconditions as dta_from_list(), in the same words.
  if (is.null(md[["title"]])) {
    return(dta_lint_row(
      "", "metadata", NA_character_, "error", "metadata_invalid",
      "Metadata section must contain 'title' field"
    ))
  }
  if (is.null(md[["version"]])) {
    return(dta_lint_row(
      "", "metadata", NA_character_, "error", "metadata_invalid",
      "Metadata section must contain 'version' field"
    ))
  }
  # An unknown key makes R print the whole offending argument into its
  # "unused argument" error, which is exponential for a YAML alias bomb, so
  # unknown keys are reported by name here, before anything is expanded.
  unknown <- setdiff(names(md), names(formals(DTAMetaData)))
  if (length(unknown) > 0L) {
    return(dta_lint_row(
      "", "metadata", NA_character_, "error", "metadata_invalid",
      sprintf("Unknown metadata field(s): %s.", paste(sQuote(unknown, FALSE), collapse = ", "))
    ))
  }
  built <- tryCatch(do.call(DTAMetaData, md), error = function(e) e)
  if (inherits(built, "error")) {
    return(dta_lint_row(
      "", "metadata", NA_character_, "error", "metadata_invalid",
      dta_condition_text(built)
    ))
  }
  dta_lint_metadata_object(built)
}

dta_lint_metadata_object <- function(md) {
  errs <- tryCatch(metadata_import_errors(md), error = function(e) NULL)
  if (is.null(errs) || nrow(errs) == 0L) {
    return(dta_lint_empty())
  }
  msgs <- dta_metadata_import_error_messages(errs)
  fields <- as.character(errs$column)
  out <- dta_lint_empty()
  for (i in seq_along(msgs)) {
    out <- rbind(out, dta_lint_row(
      "", "metadata", fields[[i]], "warning", "metadata_import_issue", msgs[[i]]
    ))
  }
  out
}

dta_lint_dataset_list <- function(d, label) {
  out <- dta_lint_empty()
  id <- d[["id"]]
  if (is.null(id) || length(id) != 1L || !nzchar(as.character(id))) {
    out <- rbind(out, dta_lint_row(
      label, "dataset", NA_character_, "error", "dataset_id_missing",
      "A dataset needs an id, its identifier; name is only the human-readable label."
    ))
  }
  type <- d[["type"]]
  if (is.null(type)) {
    out <- rbind(out, dta_lint_row(
      label, "dataset", label, "error", "dataset_invalid",
      sprintf("Dataset '%s' must contain a type.", label)
    ))
  } else if (!type %in% `__DTAtools_supported_dataset_types__`) {
    out <- rbind(out, dta_lint_row(
      label, "dataset", label, "error", "dataset_invalid",
      sprintf("Dataset type '%s' is not supported.", paste(type, collapse = ","))
    ))
    return(out)
  }
  if (!is.null(type)) {
    if (!is.null(d[["container"]])) {
      out <- rbind(out, dta_lint_row(
        label, "dataset", label, "error", "file_container_misplaced",
        "'container' belongs to a file entry: write it under 'files:'."
      ))
    }
    out <- rbind(out, dta_lint_files_list(d[["files"]], label))
    if (!identical(type, "tabular")) {
      return(out)
    }
  }
  rbind(out, dta_lint_specs_lists(d[["columns"]], d[["rules"]], label))
}

# The file entries of a dataset, judged without building them: a key the
# entry's constructor does not take, and a `container` that is not a single
# .zip/.xpt file name. `files` is normalised as dta_file_handlers_from_list()
# does (fully named = one entry, otherwise a list of entries).
dta_lint_files_list <- function(files, dataset) {
  out <- dta_lint_empty()
  if (is.null(files) || length(files) == 0L || !is.list(files)) {
    return(out)
  }
  nms <- names(files)
  if (!is.null(nms) && all(nzchar(nms))) {
    files <- list(files)
  }
  constructors <- list(
    csv = DTAFileCSV, tsv = DTAFileTSV, any = DTAFileAny,
    sas7bdat = DTAFileSAS7BDAT, xpt = DTAFileXPT
  )
  for (i in seq_along(files)) {
    entry <- files[[i]]
    if (!is.list(entry)) {
      next
    }
    id <- if (is.null(entry[["filename"]])) {
      paste0("#", i)
    } else {
      paste(unlist(entry[["filename"]]), collapse = ", ")
    }
    type <- entry[["type"]]
    if (is.character(type) && length(type) == 1L && type %in% names(constructors)) {
      unknown <- setdiff(names(entry), c("type", names(formals(constructors[[type]]))))
      if (length(unknown) > 0L) {
        out <- rbind(out, dta_lint_row(
          dataset, "file", id, "error", "file_field_unknown",
          sprintf("Unknown file field(s): %s.", paste(sQuote(unknown, FALSE), collapse = ", "))
        ))
      }
    }
    if (!is.null(entry[["container"]])) {
      problem <- dta_container_value_problem(entry[["container"]])
      if (!is.null(problem)) {
        out <- rbind(out, dta_lint_row(
          dataset, "file", id, "error", "file_container_invalid", problem
        ))
      }
    }
    # The bounds the entry would be refused for when built (the DTAFile
    # validator runs the same test), reported here instead of as an abort.
    nf <- entry[["number_of_files"]]
    lo <- entry[["min_number_of_files"]]
    hi <- entry[["max_number_of_files"]]
    # The constructor defaults an entry that declares no count to exactly 1.
    if (is.null(nf) && is.null(lo) && is.null(hi)) nf <- 1
    problem <- dta_file_count_bounds_problem(lo, hi, number = nf, pattern = isTRUE(entry[["pattern"]]))
    if (!is.null(problem)) {
      out <- rbind(out, dta_lint_row(
        dataset, "file", id, "error", "file_count_bound_invalid", problem
      ))
    }
  }
  out
}

# Builds every column and rule from its raw list, one by one, then runs the
# cross-object checks on whatever built.
dta_lint_specs_lists <- function(columns, rules, dataset) {
  fb <- dta_findings_builder()
  ok_cols <- list()
  ok_rules <- list()
  declared <- character(0)

  if (length(columns) > 0L && !is.list(columns)) {
    fb$add("dataset", dataset, "error", "column_spec_invalid", "'columns' must be a list.")
    columns <- list()
  }
  for (i in seq_along(columns)) {
    raw <- columns[[i]]
    cid <- if (is.list(raw) && !is.null(raw[["id"]])) as.character(raw[["id"]])[1] else paste0("#", i)
    if (is.list(raw) && !is.null(raw[["id"]])) declared <- c(declared, cid)
    built <- tryCatch(
      {
        if (!is.list(raw)) cli::cli_abort("A column must be a mapping.")
        do.call(DTAColumnSpec, raw)
      },
      error = function(e) e
    )
    if (inherits(built, "error")) {
      fb$add("column", cid, "error", "column_spec_invalid", dta_condition_text(built))
    } else {
      ok_cols[[length(ok_cols) + 1L]] <- built
    }
  }

  if (length(rules) > 0L && !is.list(rules)) {
    fb$add("dataset", dataset, "error", "rule_invalid", "'rules' must be a list.")
    rules <- list()
  }
  for (i in seq_along(rules)) {
    raw <- rules[[i]]
    rid <- if (is.list(raw) && !is.null(raw[["id"]])) as.character(raw[["id"]])[1] else paste0("#", i)
    built <- tryCatch(
      {
        if (!is.list(raw)) cli::cli_abort("A rule must be a mapping.")
        do.call(DTARuleFactory, raw)
      },
      error = function(e) e
    )
    if (inherits(built, "error")) {
      unknown <- if (is.list(raw) && identical(raw[["type"]], "group_condition")) {
        dta_group_unknown_conditions(raw[["constraints"]], names(raw[["conditions"]]))
      } else {
        character(0)
      }
      if (length(unknown) > 0L) {
        fb$add(
          "rule", rid, "error", "group_constraint_unknown_condition",
          sprintf("Rule '%s' has a constraint on undefined condition(s): %s.", rid, paste(unknown, collapse = ", "))
        )
      } else {
        fb$add("rule", rid, "error", "rule_invalid", dta_condition_text(built))
      }
    } else {
      ok_rules[[length(ok_rules) + 1L]] <- built
    }
  }

  own <- fb$get()
  cross <- dta_specs_findings(ok_cols, ok_rules, declared = declared)
  dta_lint_with_dataset(rbind(own, cross), dataset)
}

dta_lint_object <- function(x) {
  if (inherits(x, "DTAtools::DTA")) {
    out <- dta_lint_empty()
    if (!is.null(x@metadata)) {
      out <- rbind(out, dta_lint_metadata_object(x@metadata))
    }
    datasets <- x@datasets
    ids <- vapply(datasets, function(d) as.character(d@id)[1], character(1), USE.NAMES = FALSE)
    for (dup in unique(ids[duplicated(ids)])) {
      out <- rbind(out, dta_lint_row(
        dup, "dataset", dup, "error", "dataset_id_duplicate",
        sprintf("Dataset id '%s' is used by more than one dataset.", dup)
      ))
    }
    for (d in datasets) {
      out <- rbind(out, dta_lint_object(d))
    }
    return(out)
  }
  if (inherits(x, "DTAtools::DTADataSetTabular")) {
    return(dta_lint_with_dataset(
      dta_specs_findings(x@specs@columns, x@specs@rules), x@id
    ))
  }
  if (inherits(x, "DTAtools::DTADataSet")) {
    return(dta_lint_empty())
  }
  if (inherits(x, "DTAtools::DTAColumnSpecCollection")) {
    return(dta_lint_with_dataset(dta_specs_findings(x@columns, x@rules), ""))
  }
  cli::cli_abort(
    "{.arg x} must be a file path, a list, or a DTA, dataset or column spec collection."
  )
}

# Turns a findings frame into the entry-point behaviour: errors abort with
# every finding (class `dta_lint_error`, frame in `$findings`), warnings are
# passed through with cli_warn() when `warn` is TRUE.
dta_lint_enforce <- function(findings, warn = TRUE) {
  if (!is.data.frame(findings) || nrow(findings) == 0L) {
    return(invisible(findings))
  }
  describe <- function(rows) {
    where <- ifelse(nzchar(rows$dataset), paste0("[", rows$dataset, "] "), "")
    .cli_escape(paste0(where, "(", rows$code, ") ", rows$message))
  }
  errs <- findings[findings$severity == "error", , drop = FALSE]
  if (nrow(errs) > 0L) {
    msgs <- describe(errs)
    names(msgs) <- rep("x", length(msgs))
    cli::cli_abort(
      c(sprintf("The DTA specification has %d error(s).", nrow(errs)), msgs),
      class = "dta_lint_error",
      findings = findings
    )
  }
  if (isTRUE(warn)) {
    warns <- findings[findings$severity == "warning", , drop = FALSE]
    if (nrow(warns) > 0L) {
      msgs <- describe(warns)
      names(msgs) <- rep("!", length(msgs))
      cli::cli_warn(
        c(sprintf("The DTA specification has %d warning(s).", nrow(warns)), msgs),
        class = "dta_lint_warning"
      )
    }
  }
  invisible(findings)
}
