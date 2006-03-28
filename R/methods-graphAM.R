## adjacency matrix representation of a graph

isValidAdjMat <- function(adjMat, mode="undirected") {
    ## Determine if adjacency matrix adjMat is valid Element adjMat[i, j] == 1
    ## if the graph contains an edge FROM node i TO node j.  If mode is
    ## "undirected", then adjMat should be symmetrix.    
    if (! nrow(adjMat) == ncol(adjMat))
      stop("adjacency matrix must be square")
    if (mode == "undirected")
      if (any(adjMat != t(adjMat))) ## XXX: this could be slow
        stop("adjacency matrix must be symmetric for undirected graphs")
    if (any(adjMat < 0))
      stop("negative values not allowed in adjacency matrix")
    if (is.null(dimnames(adjMat))) {
        nNames <- NULL
    } else {
        ## take first non-null dimname
        nonNullIndices <- which(!is.null(dimnames(adjMat)))
        nNames <- dimnames(adjMat)[[nonNullIndices[1]]]
        if (any(duplicated(nNames)))
          stop("node names must be distinct")
        if (length(nonNullIndices) == 2) {
            ## verify rownames match colnames
            if (any(rownames(adjMat) != colnames(adjMat)))
              stop("row and column names must match")
        }
    }
    return(nNames)
}


isValidNodeList <- function(nList, nNames) {
    if (!is.list(nList) || is.null(names(nList)))
      stop("nodes must be specified as a named list")
    if (!setequal(names(nList), nNames))
      stop("names of node list must match graph node names")
    return(TRUE)
}


initEdgeSet <- function(self, values) {
    if (!is.list(values) || length(values) != 1 || is.null(names(values)))
      stop("values must be a named list with one element")
    self@edgeData <- new("attrData", defaults=values)
    valName <- names(values)[1]
    eSpec <- graph:::.getAllEdges(self)
    from <- eSpec$from
    to <- eSpec$to
    v <- t(self@adjMat)
    v <- v[v != 0] ## this unrolls the matrix in the right way
    edgeData(self, from=from, to=to, attr=valName) <- v
    ## FIXME: consider storing matrix as logical
    self
}


setMethod("initialize", signature("graphAM"),
          function(.Object, adjMat, edgemode="undirected", values) {
              nNames <- graph:::isValidAdjMat(adjMat, edgemode)
              if (is.null(nNames))
                nNames <- paste("n", 1:ncol(adjMat), sep="")
              colnames(adjMat) <- nNames
              rownames(adjMat) <- NULL
              .Object@adjMat <- adjMat
              edgemode(.Object) <- edgemode
              if (!missing(values))
                .Object <- graph:::initEdgeSet(.Object, values)
              else
                .Object@edgeData <- new("attrData")
              .Object@nodeData <- new("attrData")
              .Object
          })


getEdgeList <- function(adjMat, nodeNames) {
    numNodes <- length(nodeNames)
    eList <- vector(mode="list", length=numNodes)
    for (i in seq(length=numNodes)) {
        aRow <- adjMat[i, ]
        result <- names(base::which(aRow != 0))
        if (is.null(result))
          result <- character(0)
        eList[[i]] <- result
    }
    names(eList) <- nodeNames
    eList
}


setMethod("edges", signature("graphAM", "missing"),
          function(object) {
              getEdgeList(object@adjMat, nodes(object))
          })


setMethod("edges", signature("graphAM", "character"),
          function(object, which) {
              idx <- base::which(colnames(object@adjMat) %in% which)
              getEdgeList(object@adjMat[idx, ], nodes(object)[idx])
          })


setMethod("nodes", signature("graphAM"),
          function(object) {
              ## initialize gaurantees colnames
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


setMethod("numNodes", signature("graphAM"),
          function(object) length(nodes(object)))


setMethod("numEdges", signature(object="graphAM"),
          function(object) {
              nE <- sum(object@adjMat != 0)
              if (!isDirected(object)) {
                  selfLoops <- sum(diag(object@adjMat) != 0)
                  nE <- selfLoops + (nE - selfLoops)/2
              }
              nE
          })


setMethod("isAdjacent",
          signature(object="graphAM", from="character", to="character"),
          function(object, from, to) {
              eSpec <- graph:::.normalizeEdges(from, to)
              from <- eSpec$from
              to <- eSpec$to
              fromIdx <- match(from, nodes(object), nomatch=0)
              toIdx <- match(to, nodes(object), nomatch=0)
              if (any(fromIdx == 0))
                stop("Unknown nodes in from: ",
                     paste(from[fromIdx == 0], collapse=", "))
              if (any(toIdx == 0))
                stop("Unknown nodes in to: ",
                     paste(to[toIdx == 0], collapse=", "))
              result <- logical(length(fromIdx))
              for (i in 1:length(fromIdx))
                  result[i] <- object@adjMat[fromIdx[i], toIdx[i]] != 0
              result
          })


extendAdjMat <- function(adjMat, nodes) {
    nms <- c(colnames(adjMat), nodes)
    curCols <- ncol(adjMat)
    newCols <- matrix(0, nrow=curCols, ncol=length(nodes))
    adjMat <- cbind(adjMat, newCols)
    newRows <- matrix(0, nrow=length(nodes), ncol=ncol(adjMat))
    adjMat <- rbind(adjMat, newRows)
    colnames(adjMat) <- nms
    adjMat
}


getIndices <- function(nodes, from, to) {
    ## Return indices into the adjMat for nodes from and to.
    i <- match(from, nodes, nomatch=0)
    if (i == 0)
      stop("Unknown node", sQuote(from), "specified in from")
    j <- match(to, nodes, nomatch=0)
    if (j == 0)
      stop("Unknown node", sQuote(to), "specified in to")
    list(from=i, to=j)
}
    


setMethod("addNode",
          signature(node="character", object="graphAM", edges="missing"),
          function(node, object) {
              already <- node %in% nodes(object)
              if(any(already))
                stop(paste(sQuote(node[already]), collapse=", "),
                           " are already nodes in the graph")
              object@adjMat <- extendAdjMat(object@adjMat, node)
              object
          })


setMethod("addEdge",
          signature(from="character", to="character", graph="graphAM",
                    weights="missing"),
          function(from, to, graph) {
              if (isAdjacent(graph, from, to))
                stop("edge from ", sQuote(from), " to ", sQuote(to),
                     "already exists")
              idx <- getIndices(nodes(graph), from, to)
              graph@adjMat[idx$from, idx$to] <- 1
              if (! isDirected(graph))
                graph@adjMat[idx$to, idx$from] <- 1
              graph
          })
              
              
setMethod("clearNode",
          signature(node="character", object="graphAM"),
          function(node, object) {
              idx <- getNodeIndex(nodes(object), node)
              zeroVect <- rep(0, ncol(object@adjMat))
              ## clear edges from node to other
              object@adjMat[idx, ] <- zeroVect
              ## clear edges from other to node
              object@adjMat[, idx] <- zeroVect

              ## TODO: clear edge attributes


              object
          })


## TODO: implement a clearEdgeAttributes method

setMethod("removeNode",
          signature(node="character", object="graphAM"),
          function(node, object) {
              idx <- getNodeIndex(nodes(object), node)
              object@adjMat <- object@adjMat[-idx, -idx]

              ## TODO: clear edge attributes
              
              object
          })


getNodeIndex <- function(nodeNames, node) {
    idx <- match(node, nodeNames, nomatch=NA)
    if (is.na(idx))
      stop("Unknown node", sQuote(node))
    idx
}


setMethod("removeEdge",
          signature(from="character", to="character", graph="graphAM"),
          function(from, to, graph) {
              fromIdx <- getNodeIndex(nodes(graph), from)
              toIdx <- getNodeIndex(nodes(graph), to)
              graph@adjMat[fromIdx, toIdx] <- 0
              if (!isDirected(graph))
                graph@adjMat[toIdx, fromIdx] <- 0
              graph
          })


## This signature looks strange, but to get in edges for all nodes
## it makes sense to be able to write inEdges(g)
setMethod("inEdges", signature(node="graphAM", object="missing"),
          function(object, node) {
              allNodes <- nodes(node)
              return(inEdges(allNodes, node))
          })

## But we still want inEdges(object=g) to work
setMethod("inEdges", signature(node="missing", object="graphAM"),
          function(object, node) {
              allNodes <- nodes(object)
              return(inEdges(allNodes, object))
          })


setMethod("inEdges", signature(node="character", object="graphAM"),
          function(node, object) {
              allNodes <- nodes(object)
              unknownNodes <- !(node %in% allNodes)
              if (any(unknownNodes))
                stop("Unknown nodes:\n", paste(unknownNodes, collapse=", "))
              ## cols of adjMat tells us in edges
              ans <- apply(object@adjMat[, node], 2, function(col) {
                  allNodes[as.logical(col)]
                  })
              return(ans)
          })


setAs(from="graphAM", to="graphNEL",
      function(from, to) {
          n <- nodes(from)
          edgeList <- lapply(edges(from), function(e) list(edges=match(e, n)))
          gnel <- new("graphNEL", nodes=n, edgeL=edgeList,
                      edgemode=edgemode(from))
          ## copy edge and node attributes
          gnel@edgeData <- from@edgeData
          gnel@nodeData <- from@nodeData
          gnel
      })
