.onLoad <- function(libname = find.package("CRIMpp"), pkgname = "CRIMpp"){
  if (getRversion() >= "2.15.1")
    utils::globalVariables(
      # magrittr pipe
      "."
    )
}
