## unexported environment for persistent stuff like par settings
## specific to the graph package

.GraphEnv <- new.env(parent = emptyenv())
.GraphEnv$par <- list()
graph.par(.default.graph.pars())

.onUnload <- function( libpath ) {
  library.dynam.unload("BioC_graph", libpath )
}

## utilities

qrequire <-
    function(package, ..., quietly=TRUE, character.only=TRUE)
{
    suppressWarnings({
        require(package, ..., quietly=quietly,
                character.only=character.only)
    }) || stop("package required but not installed: ", sQuote(package),
               call.=FALSE)
}
    

pasteq <- function(..., collapse=", ")
    paste(sQuote(...), collapse=collapse)
