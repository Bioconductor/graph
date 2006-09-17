.onLoad <- function(libname, pkgname) {
    if (!require("methods"))
       stop("Unable to load ", sQuote("methods"), " package")
    .initGContents()


}


.onAttach <- function(libname, pkgname) {
    suppressWarnings(require("Biobase")) && addVigs2WinMenu("graph")
}


.onUnload <- function( libpath ) {
  library.dynam.unload( "graph", libpath )
}
