### helpers

.node_rename_check <- function(g, new_nodes) {
    checkValidNodeName(new_nodes)
    if (length(new_nodes) != numNodes(g))
      stop("need as many names as there are nodes", call.=FALSE)
    if (any(duplicated(new_nodes)))
      stop("node names must be unique", call.=FALSE)
}

.rename_node_attributes <- function(g, new_nodes) {
    ## FIXME: should be done in place?
    ## FIXME: we are doing the verification twice :-(
    old <- nodes(g)
    idx <- match(names(g@nodeData), old, 0)
    names(g@nodeData) <- new_nodes[idx]
    g
}

.get_edgeData_indicies <- function(g) {
    ee <- .getAllEdges(g)
    if (length(ee$from) && length(ee$to)) {
        kk <- .makeEdgeKeys(ee$from, ee$to)
        match(names(g@edgeData), kk)
    } else {
        integer(0)
    }
}

.rename_edge_attributes <- function(g, whEdges) {
    ee <- .getAllEdges(g)
    if (length(ee$from) && length(ee$to)) {
        kk <- .makeEdgeKeys(ee$from, ee$to)
        names(g@edgeData) <- kk[whEdges]
    }
    g
}

### graph

## A template method for node<- on graph objects
##
## Subclasses should define a renameNodes method only.
## This way, validation of node names and handling
## of node and edge attributes can be shared.
##
setReplaceMethod("nodes", c("graph", "character"),
                 function(object, value) {
                     .node_rename_check(object, value)
                     whEdges <- .get_edgeData_indicies(object)
                     object <- .rename_node_attributes(object, value)
                     ## the template method for different
                     ## graph representations
                     object <- renameNodes(object, value)
                     ##

                     if (length(whEdges))
                       .rename_edge_attributes(object, whEdges)
                     else
                       object
                 })

### graphNEL

setMethod("nodes", "graphNEL", function(object) object@nodes)

setMethod("renameNodes", "graphNEL", function(g, value) {
    g@nodes <- value
    names(g@edgeL) <- value
    g
})

### graphAM2
setMethod("nodes", "graphAM2", function(object) object@nodes)

### graphAM

setMethod("nodes", signature("graphAM"),
          function(object) {
              ## initialize guarantees colnames
              colnames(object@adjMat)
          })

setMethod("renameNodes", "graphAM", function(g, value) {
    colnames(g@adjMat) <- value
    g
})

### clusterGraph

setMethod("nodes", "clusterGraph", function(object)
          as.character(unlist(object@clusters)))

setMethod("renameNodes", "clusterGraph", function(g, value) {
    clens = sapply(g@clusters, length)
    nc = length(clens)
    ni = rep(1:nc, clens)
    newc = split(value, ni)
    names(newc) = names(g@clusters)
    g@clusters = newc
    g
})

### distGraph

setMethod("nodes", "distGraph", function(object)
          attr(object@Dist, "Labels" ))
