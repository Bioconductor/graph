## bit array adjacency matrix representation of a graph

setMethod("initialize", signature("graphAM2"),
          function(.Object, nodes, bitVect, edgemode) {
              .Object@graphData$edgemode <- edgemode
              .Object@nodeData <- new("attrData")
              .Object@edgeData <- new("attrData")
              .Object@nodes <- nodes
              .Object@bitVect <- bitVect
              .Object
          })

.undirectEdges <- function(from, to)
{
    fromIsFirst <- from < to
    toIsFirst <- !fromIsFirst
    tmpFrom <- c(from[fromIsFirst], to[toIsFirst])
    tmpTo <- c(to[fromIsFirst], from[toIsFirst])
    from <- tmpFrom
    to <- tmpTo
    list(from=from, to=to)
}

GraphAM2 <- function(from, to, nodes=NULL, weights=NULL, edgemode="undirected")
{
    nodes <- sort(unique(c(nodes, from, to)))
    n_nodes <- length(nodes)
    if (edgemode == "undirected") {
        ## normalize edges so that edges have nodes in lexical order
        tmp <- .undirectEdges(from, to)
        from <- tmp[["from"]]
        to <- tmp[["to"]]
    }
    ## map 'from', 'to' from character to integer indicies
    from_i <- match(from, nodes)
    to_i <- match(to, nodes)

    bitVect <- makebits(n_nodes * n_nodes, bitdim = c(n_nodes, n_nodes))
    bitVect <- setBitCell(bitVect, from_i, to_i, rep(1L, length(from_i)))
    g <- new("graphAM2", nodes=nodes, bitVect=bitVect,
             edgemode=edgemode)
    g
}

getEdgeList2 <- function(g) {
    nodeNames <- g@nodes
    numNodes <- length(nodeNames)
    eList <- structure(vector(mode="list", length=numNodes),
                       names = nodeNames)
    isMiss <- logical(numNodes)
    bv <- g@bitVect
    for (i in seq_len(numNodes)) {
        aRow <- getRow(bv, i)
        val <- nodeNames[aRow]
        if (length(val))
            eList[[i]] <- nodeNames[aRow]
        else
            isMiss[i] <- TRUE
    }
    eList[!isMiss]
}


setMethod("edges", signature("graphAM2", "missing"),
          function(object) {
              ans <- getEdgeList2(object)
              if (isDirected(object)) {
                  ans
              } else {
                  lens <- sapply(ans, length)
                  nms <- rep(names(ans), lens)
                  revAns <- split(nms, unlist(ans))
                  for (n in names(revAns)) {
                      ans[[n]] <- sort(c(ans[[n]], revAns[[n]]))
                  }
              }
              ans
          })


setMethod("edges", signature("graphAM2", "character"),
          function(object, which) {
              ## TODO: refactor to optimize
              edges(object)[which]
          })


setMethod("numNodes", signature("graphAM2"),
          function(object) length(object@nodes))


setMethod("numEdges", signature(object="graphAM2"),
          function(object) {
              .Call(graph_bitarray_sum, object@bitVect)
          })

setMethod("isAdjacent",
          signature(object="graphAM2", from="character", to="character"),
          function(object, from, to) {
              ## FIXME: should use a convention for
              ## undirected case
              if (!isDirected(object)) {
                  ## normalize edges so that edges have nodes in lexical order
                  tmp <- .undirectEdges(from, to)
                  from <- tmp[["from"]]
                  to <- tmp[["to"]]
              }
              nodeNames <- object@nodes
              from_i <- match(from, nodeNames)
              to_i <- match(to, nodeNames)
              ## FIXME: should check for NA
              getBitCell(object@bitVect, from_i, to_i)
              ## FIXME: should return value be named with edge labels?
          })


## getIndices <- function(nodes, from, to) {
##     ## Return indices into the adjMat for nodes from and to.
##     i <- match(from, nodes, nomatch=0)
##     if (i == 0)
##       stop("Unknown node", sQuote(from), "specified in from")
##     j <- match(to, nodes, nomatch=0)
##     if (j == 0)
##       stop("Unknown node", sQuote(to), "specified in to")
##     list(from=i, to=j)
## }


setMethod("addNode",
          signature(node="character", object="graphAM2", edges="missing"),
          function(node, object) {
              stop("operation not supported")
          })


setMethod("addEdge",
          signature(from="character", to="character", graph="graphAM2",
                    weights="missing"),
          function(from, to, graph) {
              stop("operation not supported")
          })


setMethod("clearNode",
          signature(node="character", object="graphAM2"),
          function(node, object) {
              stop("operation not supported")
              ## idx <- getNodeIndex(nodes(object), node)
              ## zeroVect <- rep(0, ncol(object@adjMat))
              ## ## clear edges from node to other
              ## object@adjMat[idx, ] <- zeroVect
              ## ## clear edges from other to node
              ## object@adjMat[, idx] <- zeroVect

              ## TODO: clear edge attributes


              object
          })


## TODO: implement a clearEdgeAttributes method

setMethod("removeNode",
          signature(node="character", object="graphAM2"),
          function(node, object) {
              stop("operation not supported")
              ## idx <- getNodeIndex(nodes(object), node)
              ## object@adjMat <- object@adjMat[-idx, -idx]

              ## ## TODO: clear edge attributes

              ## object
          })


## getNodeIndex <- function(nodeNames, node) {
##     idx <- match(node, nodeNames, nomatch=NA)
##     if (any(is.na(idx)))
##       stop("Unknown node", sQuote(node))
##     idx
## }

## coordToIndex <- function(x, y, nrow) (y * nrow) - (nrow - x)

setMethod("removeEdge",
          signature(from="character", to="character", graph="graphAM2"),
          function(from, to, graph) {
              stop("operation not supported")
              ## fromIdx <- getNodeIndex(nodes(graph), from)
              ## toIdx <- getNodeIndex(nodes(graph), to)
              ## rowCnt <- nrow(graph@adjMat)
              ## graph@adjMat[coordToIndex(fromIdx, toIdx, rowCnt)] <- 0
              ## if (!isDirected(graph))
              ##   graph@adjMat[coordToIndex(toIdx, fromIdx, rowCnt)] <- 0
              ## graph
          })

## Yuck, I'd rather not support such shenanigans
## ## This signature looks strange, but to get in edges for all nodes
## ## it makes sense to be able to write inEdges(g)
## setMethod("inEdges", signature(node="graphAM2", object="missing"),
##           function(node, object) {
##               allNodes <- nodes(node)
##               return(inEdges(allNodes, node))
##           })

## ## But we still want inEdges(object=g) to work
## setMethod("inEdges", signature(node="missing", object="graphAM2"),
##           function(node, object) {
##               allNodes <- nodes(object)
##               return(inEdges(allNodes, object))
##           })


setMethod("inEdges", signature(node="character", object="graphAM2"),
          function(node, object) {
              allNodes <- nodes(object)
              unknownNodes <- !(node %in% allNodes)
              if (any(unknownNodes))
                stop("Unknown nodes:\n", paste(unknownNodes, collapse=", "))
              ## cols of adjacency give us in edges
              bv <- object@bitVect

              ans <- structure(vector("list", length(node)),
                               names = node)
              node_i <- match(node, allNodes)
              k <- 1L
              for (i in node_i) {
                  ans[[k]] <- allNodes[getColumn(bv, i)]
                  k <- k + 1L
              }
              ans
          })

edge_set_intersect <- function(g1, g2)
{
    ## TODO: make this vectorized to take ... or a list of graph
    ## objects.  Probably have to forget about a generic as dispatch
    ## will be difficult.
    nodeNames <- nodes(g1)
    stopifnot(all(nodeNames == nodes(g2)))
    bv1 <- g1@bitVect
    ## TODO: could we use structure here to avoid a copy?
    keepAttrs <- attributes(bv1)
    sharedVect <- bv1 & g2@bitVect
    attributes(sharedVect) <- keepAttrs
    new("graphAM2", nodes = nodeNames, bitVect = sharedVect,
        edgemode = edgemode(g1))
}

edge_set_union <- function(g1, g2)
{
    ## TODO: make this vectorized to take ... or a list of graph
    ## objects.  Probably have to forget about a generic as dispatch
    ## will be difficult.
    nodeNames <- nodes(g1)
    stopifnot(all(nodeNames == nodes(g2)))
    bv1 <- g1@bitVect
    ## TODO: could we use structure here to avoid a copy?
    keepAttrs <- attributes(bv1)
    sharedVect <- bv1 | g2@bitVect
    attributes(sharedVect) <- keepAttrs
    new("graphAM2", nodes = nodeNames, bitVect = sharedVect,
        edgemode = edgemode(g1))
}
