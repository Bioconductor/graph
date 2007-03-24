### helpers

.node_rename_check <- function(g, new_nodes) {
    if (length(new_nodes) != numNodes(g))
      stop("need as many names as there are nodes", call.=FALSE)
    if (any(duplicated(new_nodes)))
      stop("node names must be unique", call.=FALSE)
    if (any(is.na(new_nodes)))
      stop("node names cannot be NA", call.=FALSE)
}

.rename_node_attributes <- function(g, new_nodes) {
    ## FIXME: should be done in place?
    ## FIXME: we are doing the verification twice :-(
    old <- nodes(g)
    idx <- match(names(g@nodeData), old, 0)
    names(g@nodeData) <- new_nodes[idx]
    g
}

### graph

## FIXME: add methods at this level to reuse attribute handling


### graphNEL

setMethod("nodes", "graphNEL", function(object) object@nodes)

setReplaceMethod("nodes", c("graphNEL", "character"),
                 function(object, value) {
                     .node_rename_check(object, value)
                     object@nodes <- value
                     names(object@edgeL) <- value
                     .rename_node_attributes(object, value)
                 })

### graphAM

setMethod("nodes", signature("graphAM"),
          function(object) {
              ## initialize guarantees colnames
              colnames(object@adjMat)
          })

setReplaceMethod("nodes", signature("graphAM", "character"),
                 function(object, value) {
                     .node_rename_check(object, value)
                     colnames(object@adjMat) <- value
                     object
                 })

### clusterGraph

setMethod("nodes", "clusterGraph", function(object)
          as.character(unlist(object@clusters)))

setReplaceMethod("nodes", c("clusterGraph", "character"),
                 function(object, value) {
                     .node_rename_check(object, value)
                     clens = sapply(object@clusters, length)
                     nc = length(clens)
                     ni = rep(1:nc, clens)
                     newc = split(value, ni)
                     names(newc) = names(object@clusters)
                     object@clusters = newc
                     object
                 })

### distGraph

setMethod("nodes", "distGraph", function(object)
          attr(object@Dist, "Labels" ))
