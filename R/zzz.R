## unexported environment for persistent stuff like par settings
## specific to the graph package

.GraphEnv <- new.env(parent = emptyenv())
.GraphEnv$par <- list()
graph.par(.default.graph.pars())

.onUnload <- function( libpath ) {
  library.dynam.unload("BioC_graph", libpath )
}

