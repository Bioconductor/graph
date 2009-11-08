
## unexported environment for persistent stuff like par settings
## specific to the graph package

.GraphEnv <- new.env(parent = emptyenv())
.GraphEnv$par <- list()
graph.par(.default.graph.pars())


.onLoad <- function(libname, pkgname) {
    if (!require("methods"))
       stop("Unable to load 'methods' package")
}

.onUnload <- function( libpath ) {
  library.dynam.unload("BioC_graph", libpath )
}



