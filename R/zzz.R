
## unexported environment for persistent stuff like par settings
## specific to the graph package

.GraphEnv <- new.env(parent = emptyenv())

assign("par",
       list(col = "red", lwd = 1, lty = 1),
       env = .GraphEnv)



.onLoad <- function(libname, pkgname) {
    if (!require("methods"))
       stop("Unable to load 'methods' package")
}

.onUnload <- function( libpath ) {
  library.dynam.unload("graph", libpath )
}
