.onLoad <- function(libname, pkgname) {
    if (!require("methods"))
       stop("Unable to load 'methods' package")
}

.onUnload <- function( libpath ) {
  library.dynam.unload("graph", libpath )
}
