## adjacency matrix representation of a graph

isValidAdjMat <- function(adjMat, mode="undirected") {
    ## Determine if adjacency matrix adjMat is valid Element adjMat[i, j] == 1
    ## if the graph contains an edge FROM node i TO node j.  If mode is
    ## "undirected", then adjMat should be symmetrix.
    if (length(adjMat) == 0L) return(character(0L))
    if (! nrow(adjMat) == ncol(adjMat))
      stop("adjacency matrix must be square")
    if (mode == "undirected")
      if (any(adjMat != t(adjMat))) ## XXX: this could be slow
        stop("adjacency matrix must be symmetric for undirected graphs")
    if (any(adjMat < 0))
      stop("adjacency matrix must not have negative values")
    if (is.null(dimnames(adjMat))) {
        nNames <- NULL
    } else {
        ## take first non-null dimname
        nonNullIndices <- which(sapply(dimnames(adjMat),
                                       function(x) !is.null(x)))
        nNames <- dimnames(adjMat)[[nonNullIndices[1]]]
        if (any(duplicated(nNames)))
          stop("node names must be distinct")
        checkValidNodeName(nNames)
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
    ## Put matrix elements into @edgeData using attr name from 'values'.
    if (!is.list(values) || length(values) != 1 || is.null(names(values)))
      stop("'values' must be a named list with one element")
    self@edgeData <- new("attrData", defaults=values)
    valName <- names(values)[1]
    eSpec <- .getAllEdges(self)
    from <- eSpec$from
    to <- eSpec$to
    if (length(from) > 0L && length(to) > 0L) {
        v <- t(self@adjMat)
        v <- v[v != 0] ## this unrolls the matrix in the right way
        edgeData(self, from=from, to=to, attr=valName) <- v
        ## FIXME: consider storing matrix as logical
    }
    self
}


setMethod("initialize", signature("graphAM"),
          function(.Object, adjMat, edgemode="undirected", values) {
              nNames <- isValidAdjMat(adjMat, edgemode)
              if (is.null(nNames))
                  nNames <- paste0("n", seq_len(ncol(adjMat)))
              .Object@graphData$edgemode <- edgemode
              .Object@nodeData <- new("attrData")
              colnames(adjMat) <- nNames
              rownames(adjMat) <- NULL
              .Object@adjMat <- adjMat

              if (!missing(values))
                .Object <- initEdgeSet(.Object, values)
              else
                .Object@edgeData <- new("attrData")

              ## Matrix values have been stored in @edgeData,
              ## so now we normalize to 0/1
              adjMat <- .Object@adjMat
              adjMat[adjMat != 0L] <- 1L
              .Object@adjMat <- adjMat

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


setMethod("edges", signature("graphAM"),
          function(object, which) {
              adjMat <- object@adjMat
              if (length(adjMat) == 0L) return(list())
              nodes <- nodes(object)
              if (!missing(which)) {
                  if (!is.character(which))
                      stop("'which' must be missing or a character vector")
                  idx <- base::which(colnames(adjMat) %in% which)
                  adjMat <- adjMat[idx, ]
                  nodes <- nodes[idx]
              }
              getEdgeList(adjMat, nodes)
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
              eSpec <- .normalizeEdges(from, to)
              from <- eSpec$from
              to <- eSpec$to
              fromIdx <- match(from, nodes(object), nomatch=0)
              toIdx <- match(to, nodes(object), nomatch=0)
              if (any(fromIdx == 0))
                stop("'from' unknown node(s): ",
                     pasteq(from[fromIdx == 0]))
              if (any(toIdx == 0))
                stop("'to' unknown nodes: ", pasteq(to[toIdx == 0]))
              result <- logical(length(fromIdx))
              for (i in seq_along(fromIdx))
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
    if (any(bad <- (i == 0)))
      stop("'from' unknown node(s): ", sQuote(from[bad]))
    j <- match(to, nodes, nomatch=0)
    if (any(bad <- (j == 0)))
      stop("'to' unknown node(s): ", sQuote(to[bad]))
    list(from=i, to=j)
}



setMethod("addNode",
          signature(node="character", object="graphAM", edges="missing"),
          function(node, object) {
              already <- node %in% nodes(object)
              if(any(already))
                stop("node(s) already exist: ", pasteq(node[already]))
              checkValidNodeName(node)
              object@adjMat <- extendAdjMat(object@adjMat, node)
              object
          })


setMethod("addEdge",
          signature(from="character", to="character", graph="graphAM",
                    weights="missing"),
          function(from, to, graph) {
              if (any(bad <- isAdjacent(graph, from, to)))
                  stop("edge(s) already exist: ",
                       pasteq(paste(from[bad], to[bad], sep="|")))
              idx <- getIndices(nodes(graph), from, to)
              idx <- cbind(idx$from, idx$to)
              graph@adjMat[idx] <- 1L
              if (! isDirected(graph))
                graph@adjMat[idx[ , c(2L, 1L)]] <- 1L
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
    if (any(is.na(idx)))
        stop("unknown node(s): ", pasteq(node[is.na(idx)]))
    idx
}

coordToIndex <- function(x, y, nrow) (y * nrow) - (nrow - x)

setMethod("removeEdge",
          signature(from="character", to="character", graph="graphAM"),
          function(from, to, graph) {
              fromIdx <- getNodeIndex(nodes(graph), from)
              toIdx <- getNodeIndex(nodes(graph), to)
              rowCnt <- nrow(graph@adjMat)
              graph@adjMat[coordToIndex(fromIdx, toIdx, rowCnt)] <- 0
              if (!isDirected(graph))
                graph@adjMat[coordToIndex(toIdx, fromIdx, rowCnt)] <- 0
              graph
          })


## This signature looks strange, but to get in edges for all nodes
## it makes sense to be able to write inEdges(g)
setMethod("inEdges", signature(node="graphAM", object="missing"),
          function(node, object) {
              allNodes <- nodes(node)
              return(inEdges(allNodes, node))
          })

## But we still want inEdges(object=g) to work
setMethod("inEdges", signature(node="missing", object="graphAM"),
          function(node, object) {
              allNodes <- nodes(object)
              return(inEdges(allNodes, object))
          })


setMethod("inEdges", signature(node="character", object="graphAM"),
          function(node, object) {
              allNodes <- nodes(object)
              unknownNodes <- !(node %in% allNodes)
              if (any(unknownNodes))
                stop("unknown nodes: ",
                     pasteq(node[unknownNodes]))
              ## cols of adjMat tells us in edges
              adjMat <- object@adjMat
              ans <- list()
              for (n in node) {
                  ans[[n]] <- allNodes[as.logical(adjMat[, n])]
              }
              ans
          })


setAs(from="graphAM", to="matrix",
      function(from) {
          am <- from@adjMat
          if (length(am) == 0L) return(am)
          if ("weight" %in% names(edgeDataDefaults(from))) {
              tm <- t(am)
              tm[tm != 0] <- unlist(edgeData(from, attr="weight"))
              m <- t(tm)
          } else {
              m <- am
          }
	  rownames(m) <- colnames(m)
	  m
      })
## ^^ the reverse is in ./mat2graph.R

setAs(from="graphAM", to="graphNEL",
      function(from) {
	  gnel <- graphNEL(nodes=nodes(from), edgeL=edges(from),
		      edgemode=edgemode(from))
	  ## copy edge and node attributes:
	  gnel@edgeData <- from@edgeData
	  gnel@nodeData <- from@nodeData
	  gnel
      })


## This is also used in mat2graph.R :
NEL2mat <- function(g) {
    theNodes <- nodes(g)
    numNodes <- length(theNodes)
    mat <- matrix(0:0, nrow=numNodes, ncol=numNodes)
    rownames(mat) <- colnames(mat) <- theNodes
    theEdges <- edges(g)
    wts <- edgeWeights(g)
    use.wts <- any(unlist(wts) != 1)
    for (n in theNodes) {
	e <- theEdges[[n]]
	if (length(e))
	    mat[n, e] <- if(use.wts) wts[[n]] else 1L
    }
    mat
}

setAs(from="graphNEL", to="graphAM",
      function(from) {
          theNodes <- nodes(from)
          numNodes <- length(theNodes)
          mat <- matrix(0, nrow=numNodes, ncol=numNodes,
                        dimnames=list(theNodes, theNodes))
          theEdges <- edges(from)
          for (n in theNodes) {
              e <- theEdges[[n]]
              if (length(e))
                mat[n, e] <- 1
          }
          ## XXX: it would be safer to pass mat here, but since we cannot
          ##      yet pass in the edgeData and nodeData, we benefit greatly
          ##      by avoiding the copying of large matrices.
          gam <- graphAM(matrix(0), edgemode=edgemode(from))
          ## one of the things that initialize will do is remove row
          ## names, so that only one copy of node names are stored
          rownames(mat) <- NULL
          ## copy edge and node attributes
          gam@edgeData <- from@edgeData
          gam@nodeData <- from@nodeData
          gam@adjMat <- mat
          gam
      })
