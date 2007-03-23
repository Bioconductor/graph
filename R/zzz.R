.onLoad <- function(libname, pkgname) {
    if (!require("methods"))
       stop("Unable to load 'methods' package")
}


.onAttach <- function(libname, pkgname) {
    if (.Platform$OS.type == "windows")
      suppressWarnings(require("Biobase")) && addVigs2WinMenu("graph")
}


.onUnload <- function( libpath ) {
  library.dynam.unload("graph", libpath )
}
