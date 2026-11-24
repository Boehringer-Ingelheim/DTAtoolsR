utils::globalVariables(c(
  "Format", "nullable", "description", "id",
  "instancePath", "keyword", "columnspec", "data", ".", ".n",
  "Name", "Role"
))

.onLoad <- function(...) {
  S7::S7_on_load()
}

.onUnload <- function(...) {
  S7::S7_on_unload()
}

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

# Last file in Collate, after every `method<-()`: drops the placeholders S7 1.0.0
# leaves in the namespace for the base generics (`names`, `print`, `labels`, ...).
S7::S7_on_build()
