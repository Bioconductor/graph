### helpers

.node_rename_check <- function(g, new_nodes) {
    if (length(value) != length(nodes(g)))
      stop("need as many names as there are nodes", call.=FALSE)
    if (any(duplicated(new_nodes)))
      stop("node names must be unique", call.=FALSE)
    if (any(is.na(new_nodes)))
      stop("node names cannot be NA", call.=FALSE)
}

### graph

## FIXME: add methods at this level to reuse attribute handling


### graphNEL

setMethod("nodes", "graphNEL", function(object) object@nodes)

setReplaceMethod("nodes", c("graphNEL", "character"),
                 function(object, value) {
                     if(length(value) != length(object@nodes))
                       stop("need as many names as there are nodes")
                     if(any(duplicated(value)))
                       stop("node names must be unique")
                     object@nodes <- value
                     names(object@edgeL) <- value
                     object})

### graphAM

setMethod("nodes", signature("graphAM"),
          function(object) {
              ## initialize guarantees colnames
              colnames(object@adjMat)
          })

setReplaceMethod("nodes", signature("graphAM", "character"),
                 function(object, value) {
                     if(length(value) != ncol(object@adjMat))
                       stop("need as many names as there are nodes")
                     if(any(duplicated(value)))
                       stop("node names must be unique")
                     colnames(object@adjMat) <- value
                     object
                 })

### clusterGraph

setMethod("nodes", "clusterGraph", function(object)
          as.character(unlist(object@clusters)))

setReplaceMethod("nodes", c("clusterGraph", "character"),
                 function(object, value) {
                     clens = sapply(object@clusters, length)
                     if(length(value) != sum(clens))
                       stop("need as many names as there are nodes")
                     if(any(duplicated(value)))
                       stop("node names must be unique")
                     nc = length(clens)
                     ni = rep(1:nc, clens)
                     newc = split(value, ni)
                     names(newc) = names(object@clusters)
                     object@clusters = newc
                     object})

### distGraph

setMethod("nodes", "distGraph", function(object)
          attr(object@Dist, "Labels" ))
