.onLoad <- function(libname, pkgname) {
    if (!require("methods"))
       stop("Unable to load 'methods' package")
}


.onAttach <- function(libname, pkgname) {
    if ("Biobase" %in% loadedNamespaces())
      Biobase::addVigs2WinMenu("graph")
}


.onUnload <- function( libpath ) {
  library.dynam.unload("graph", libpath )
}
