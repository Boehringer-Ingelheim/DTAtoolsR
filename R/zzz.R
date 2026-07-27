utils::globalVariables(c(
  "Format", "nullable", "description", "id",
  "instancePath", "keyword", "schema", "data", "."
))

.onLoad <- function(...) {
  S7::methods_register()
}

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL
