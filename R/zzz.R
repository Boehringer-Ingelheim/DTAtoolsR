utils::globalVariables(c("Format", "nullable", "description", "id"))

.onLoad <- function(...) {
  S7::methods_register()
}
