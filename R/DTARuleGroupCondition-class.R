#' @title DTARuleGroupCondition Class
#' @description
#' A grouped cross-row rule that evaluates named conditions within each group
#' and applies constraint logic between those conditions.
#'
#' @import S7
#' @importFrom cli cli_abort
#' @export
#'
#' @param id Character. A unique identifier for the rule.
#' @param description Character or NULL. Free-text description of the rule.
#' @param group_by Character vector. Columns used to define the groups.
#' @param conditions Named list of named conditions. Each entry maps a
#'   condition name to a column-condition mapping in the same shape used by
#'   `DTARuleColCondition`.
#' @param constraints List of constraint specifications. Supported constraint
#'   `type` values are `mutually_exclusive` (alias: `not_both`) and
#'   `requires` (alias: `implies`).
#' @return An object of class `DTARuleGroupCondition`.
#'
#' @examples
#' DTAtools::DTARuleGroupCondition(
#'   id = "sample_visit_status_logic",
#'   group_by = c("SUBJIDN", "GFREFID", "VISIT"),
#'   conditions = list(
#'     c1_failed = list(GFREASND = list(empty = FALSE)),
#'     c2_reported = list(GFREASND = list(empty = TRUE), GFORRES = list(empty = FALSE)),
#'     c3_not_done = list(GFSTAT = list(equals = "NOT DONE"))
#'   ),
#'   constraints = list(
#'     list(type = "mutually_exclusive", left = "c1_failed", right = "c2_reported"),
#'     list(type = "requires", `if` = "c1_failed", then = "c3_not_done")
#'   )
#' )
#' @include DTARule-class.R
DTARuleGroupCondition <- S7::new_class(
  "DTARuleGroupCondition",
  parent = DTARule,
  constructor = function(
    id,
    description = NULL,
    group_by = NULL,
    conditions = NULL,
    constraints = NULL
  ) {
    if (is.null(group_by) || length(group_by) < 1) {
      cli_abort(c(
        "Invalid {.arg group_by} in group_condition rule {.val {id}}.",
        x = "No grouping columns were provided.",
        i = "Provide one or more existing table columns, e.g. {.val c('SUBJECT_ID', 'VISIT')}."
      ))
    }
    if (is.list(group_by)) {
      group_by <- unlist(group_by)
    }
    if (!is.character(group_by) || any(!nzchar(group_by))) {
      cli_abort(c(
        "Invalid {.arg group_by} in group_condition rule {.val {id}}.",
        x = "Each grouping entry must be a non-empty character column name.",
        i = "Example: {.val c('SUBJECT_ID', 'VISIT')}."
      ))
    }

    if (is.null(conditions) || !is.list(conditions) || length(conditions) < 1) {
      cli_abort(c(
        "Invalid {.arg conditions} in group_condition rule {.val {id}}.",
        x = "No named conditions were provided.",
        i = "Expected shape: {.val list(c1 = list(COL = list(equals = 'X')))}."
      ))
    }

    condition_names <- names(conditions)
    if (is.null(condition_names) || any(!nzchar(condition_names))) {
      cli_abort(c(
        "Invalid {.arg conditions} in group_condition rule {.val {id}}.",
        x = "Every condition needs a non-empty name.",
        i = "Example names: {.val c('c_failed', 'c_reported')}."
      ))
    }
    if (anyDuplicated(condition_names)) {
      dupes <- unique(condition_names[duplicated(condition_names)])
      cli_abort(c(
        "Invalid {.arg conditions} in group_condition rule {.val {id}}.",
        x = "Condition names must be unique.",
        i = "Duplicate name{?s}: {.val {dupes}}."
      ))
    }

    normalized_conditions <- lapply(
      seq_along(conditions),
      function(i) {
        dta_normalize_conditions(conditions[[i]], arg = condition_names[[i]])
      }
    )
    names(normalized_conditions) <- condition_names

    normalized_constraints <- dta_normalize_group_constraints(constraints, condition_names)

    new_object(
      DTAtools::DTARule(
        id = id,
        type = "check_group_condition",
        description = description
      ),
      group_by = as.character(group_by),
      conditions = normalized_conditions,
      constraints = normalized_constraints
    )
  },
  properties = list(
    id = class_character,
    type = class_character,
    group_by = class_character,
    conditions = class_list_or_null,
    constraints = class_list_or_null
  ),
  validator = function(self) {
    if (!is.null(self@id) && (length(self@id) != 1 || !is.character(self@id) || any(grepl("\\s", self@id)))) {
      cli_abort("@id cannot have whitespaces and needs to be defined.")
    }
    if (!self@type %in% c("check_group_condition", "group_condition")) {
      cli_abort("'type' must be 'check_group_condition' or 'group_condition'.")
    }
    if (is.null(self@group_by) || length(self@group_by) < 1 || any(!nzchar(self@group_by))) {
      cli_abort("'group_by' must be a non-empty character vector of columns.")
    }
    if (is.null(self@conditions) || !is.list(self@conditions) || length(self@conditions) < 1) {
      cli_abort("'conditions' must be a non-empty named list.")
    }
    if (is.null(names(self@conditions)) || any(!nzchar(names(self@conditions)))) {
      cli_abort("Each group condition must have a non-empty name.")
    }
    if (anyDuplicated(names(self@conditions))) {
      cli_abort("Condition names in 'conditions' must be unique.")
    }
    if (is.null(self@constraints) || !is.list(self@constraints) || length(self@constraints) < 1) {
      cli_abort("'constraints' must be a non-empty list.")
    }
  }
)

#' @keywords internal
dta_normalize_group_constraints <- function(constraints, condition_names) {
  if (is.null(constraints) || !is.list(constraints) || length(constraints) < 1) {
    cli_abort(c(
      "Invalid {.arg constraints} for group_condition rule.",
      x = "No constraint definitions were provided.",
      i = "Provide at least one constraint of type {.val mutually_exclusive} or {.val requires}."
    ))
  }

  normalize_scope <- function(scope, arg_name) {
    scope <- scope %||% "any"
    if (!scope %in% c("any", "all")) {
      cli_abort(c(
        "Invalid scope value in group_condition constraint.",
        x = "Field {.field {arg_name}} has value {.val {scope}}.",
        i = "Allowed values are {.val any} or {.val all}."
      ))
    }
    scope
  }

  out <- lapply(seq_along(constraints), function(i) {
    cst <- constraints[[i]]
    if (!is.list(cst) || length(cst) == 0) {
      cli_abort(c(
        "Invalid constraint at index {.val {i}} in group_condition rule.",
        x = "Each constraint must be a non-empty list.",
        i = "Expected fields depend on type: left/right for mutually_exclusive, if/then for requires."
      ))
    }

    ctype <- cst$type %||% ""
    ctype <- switch(as.character(ctype),
      not_both = "mutually_exclusive",
      implies = "requires",
      as.character(ctype)
    )

    cid <- cst$id %||% paste0("constraint_", i)
    msg <- cst$message %||% NULL

    if (identical(ctype, "mutually_exclusive")) {
      left <- cst$left %||% cst[["if"]]
      right <- cst$right %||% cst[["then"]]
      if (!is.character(left) || length(left) != 1 || !nzchar(left)) {
        cli_abort(c(
          "Invalid constraint {.val {cid}} (index {.val {i}}).",
          x = "Type {.val mutually_exclusive} requires a non-empty {.field left} condition name."
        ))
      }
      if (!is.character(right) || length(right) != 1 || !nzchar(right)) {
        cli_abort(c(
          "Invalid constraint {.val {cid}} (index {.val {i}}).",
          x = "Type {.val mutually_exclusive} requires a non-empty {.field right} condition name."
        ))
      }
      unknown <- setdiff(c(left, right), condition_names)
      if (length(unknown) > 0) {
        cli_abort(c(
          "Constraint {.val {cid}} references unknown condition name{?s}.",
          x = "Unknown: {.val {unknown}}.",
          i = "Defined condition name{?s}: {.val {condition_names}}."
        ))
      }
      return(list(
        id = cid,
        type = ctype,
        left = left,
        right = right,
        left_scope = normalize_scope(cst$left_scope, "left_scope"),
        right_scope = normalize_scope(cst$right_scope, "right_scope"),
        message = msg
      ))
    }

    if (identical(ctype, "requires")) {
      if_name <- cst[["if"]] %||% cst$left
      then_name <- cst[["then"]] %||% cst$right
      if (!is.character(if_name) || length(if_name) != 1 || !nzchar(if_name)) {
        cli_abort(c(
          "Invalid constraint {.val {cid}} (index {.val {i}}).",
          x = "Type {.val requires} requires a non-empty {.field if} condition name."
        ))
      }
      if (!is.character(then_name) || length(then_name) != 1 || !nzchar(then_name)) {
        cli_abort(c(
          "Invalid constraint {.val {cid}} (index {.val {i}}).",
          x = "Type {.val requires} requires a non-empty {.field then} condition name."
        ))
      }
      unknown <- setdiff(c(if_name, then_name), condition_names)
      if (length(unknown) > 0) {
        cli_abort(c(
          "Constraint {.val {cid}} references unknown condition name{?s}.",
          x = "Unknown: {.val {unknown}}.",
          i = "Defined condition name{?s}: {.val {condition_names}}."
        ))
      }
      return(list(
        id = cid,
        type = ctype,
        `if` = if_name,
        `then` = then_name,
        if_scope = normalize_scope(cst$if_scope, "if_scope"),
        then_scope = normalize_scope(cst$then_scope, "then_scope"),
        message = msg
      ))
    }

    cli_abort(c(
      "Constraint {.val {cid}} uses unsupported type {.val {ctype}}.",
      x = "Supported types are {.val mutually_exclusive} and {.val requires}.",
      i = "Alias values are accepted: {.val not_both} -> {.val mutually_exclusive}, {.val implies} -> {.val requires}."
    ))
  })

  ids <- vapply(out, function(x) x$id, character(1))
  if (anyDuplicated(ids)) {
    dupes <- unique(ids[duplicated(ids)])
    cli_abort(c(
      "Constraint ids in group_condition must be unique.",
      x = "Duplicate id{?s}: {.val {dupes}}."
    ))
  }

  out
}

#' @title print
#' @description
#' Print overview for DTARuleGroupCondition.
#' @param x An object of class DTARuleGroupCondition
#' @name print
#' @export
method(print, DTARuleGroupCondition) <- function(x, ...) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli::cli_text("<{.emph DTARuleGroupCondition}> : {.field {x@id}}")
  if (!is.null(x@description)) cli::cli_text("{x@description}")
  cli::cli_alert_info("group_by: {paste(x@group_by, collapse = ', ')}")
  cli::cli_alert_info("conditions: {length(x@conditions)}")
  cli::cli_alert_info("constraints: {length(x@constraints)}")
}

# check() method for DTARuleGroupCondition; documented at ?check (generic).
method(check, DTARuleGroupCondition) <- function(x, tab) {
  if (!inherits(tab, "Table")) {
    cli::cli_abort("The 'tab' argument must be an arrow Table.")
  }
}

#' @title as.list for DTARuleGroupCondition
#' @description
#' Convert a DTARuleGroupCondition object to a list.
#' @param x An object of class DTARuleGroupCondition
#' @param ... Additional arguments (not used).
#' @return A named list containing the properties of the rule.
#' @export
#' @name as.list
method(as.list, DTARuleGroupCondition) <- function(x, ...) {
  list(
    id = x@id,
    type = x@type,
    description = x@description,
    group_by = x@group_by,
    conditions = x@conditions,
    constraints = x@constraints
  )
}
