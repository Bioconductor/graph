## Incidence matrix representation of a graph

validGraphIM <- function(object) {
    inciMatDims <- dim(object@inciMat)
    if (inciMatDims[1] != inciMatDims[2])
      return("incidence matrix must be square")
    cn <- colnames(object@inciMat)
    rn <- rownames(object@inciMat)
    if (!is.null(cn) || !is.null(rn))
      if (!all(cn == rn))
        return("incidence matrix row and column names must match")
    return(TRUE)
}


.isValidInciMat <- function(inciMat, mode="undirected") {
    if (! nrow(inciMat) == ncol(inciMat))
      stop("incidence matrix must be square")
    if (mode == "undirected")
      if (any(inciMat != t(inciMat))) ## XXX: this could be slow
        stop("incidence matrix must be symmetric for undirected graphs")
    if (any(inciMat < 0))
      stop("negative values not allowed in incidence matrix")
    if (is.null(dimnames(inciMat))) {
        nNames <- NULL
    } else {
        ## take first non-null dimname
        nonNullIndices <- which(!is.null(dimnames(inciMat)))
        nNames <- dimnames(inciMat)[[nonNullIndices[1]]]
        if (any(duplicated(nNames)))
          stop("node names must be distinct")
        if (length(nonNullIndices) == 2) {
            ## verify rownames match colnames
            if (any(rownames(inciMat) != colnames(inciMat)))
              stop("row and column names must match")
        }
    }
    return(nNames)
}


.isValidNodeList <- function(nList, nNames) {
    if (!is.list(nList) || is.null(names(nList)))
      stop("nodes must be specified as a named list")
    if (!setequal(names(nList), nNames))
      stop("names of node list must match graph node names")
    return(TRUE)
}


.initEdgeSet <- function(self, values) {
    if (!is.list(values) || length(values) != 1 || is.null(names(values)))
      stop("values must be a named list with one element")
    self@edgeData <- new("attrData", defaults=values)
    nodeNames <- nodes(self)
    defaultValue <- values[[1]]
    valName <- names(values)[1]
    for (j in 1:ncol(self@inciMat)) {
        ## work column-wise for efficiency
        haveW <- which((self@inciMat[, j] != defaultValue)
                       & (self@inciMat[, j] != 0))
        if (length(haveW) > 0) {
            toNode <- nodeNames[j]
            for (fromIdx in haveW) {
                fromNode <- nodeNames[fromIdx]
                v <- self@inciMat[fromIdx, j]
                edgeData(self, fromNode, toNode, attr=valName) <- v
            }
        }
    }
    self
}


setMethod("initialize", signature("graphIM"),
          function(.Object, inciMat, edgemode="undirected", values) {
              nNames <- graph:::.isValidInciMat(inciMat, edgemode)
              if (is.null(nNames))
                nNames <- paste("n", 1:ncol(inciMat), sep="")
              colnames(inciMat) <- nNames
              rownames(inciMat) <- NULL
              .Object@inciMat <- inciMat
              edgemode(.Object) <- edgemode
              if (!missing(values))
                .Object <- graph:::.initEdgeSet(.Object, values)
              else
                .Object@edgeData <- new("attrData")
              .Object@nodeData <- new("attrData")
              .Object
          })


.getEdgeList <- function(inciMat, nodeNames) {
    eList <- apply(inciMat, 1, function(aRow) {
        result <- names(base::which(aRow != 0))
        if (is.null(result))
          character(0)
        else
          result
    })
    names(eList) <- nodeNames
    eList
}


setMethod("edges", signature("graphIM", "missing"),
          function(object) {
              .getEdgeList(object@inciMat, nodes(object))
          })


setMethod("edges", signature("graphIM", "character"),
          function(object, which) {
              idx <- base::which(colnames(object@inciMat) %in% which)
              .getEdgeList(object@inciMat[idx, ], nodes(object)[idx])
          })


setMethod("nodes", signature("graphIM"),
          function(object) {
              ## initialize gaurantees colnames
              colnames(object@inciMat)
          })


setReplaceMethod("nodes", signature("graphIM", "character"),
                 function(object, value) {
                     if(length(value) != ncol(object@inciMat))
                       stop("need as many names as there are nodes")
                     if(any(duplicated(value)))
                       stop("node names must be unique")
                     colnames(object@inciMat) <- value
                     object
                 })


setMethod("numNodes", signature("graphIM"),
          function(object) length(nodes(object)))


setMethod("numEdges", signature("graphIM"),
          function(graph) {
              nE <- sum(graph@inciMat != 0)
              if (!isDirected(graph))
                nE <- nE / 2
              nE
          })


setMethod("isAdjacent",
          signature(object="graphIM", from="character", to="character"),
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
                  result[i] <- object@inciMat[fromIdx[i], toIdx[i]] != 0
              result
          })


.extendInciMat <- function(inciMat, nodes) {
    nms <- c(colnames(inciMat), nodes)
    curCols <- ncol(inciMat)
    newCols <- matrix(0, nrow=curCols, ncol=length(nodes))
    inciMat <- cbind(inciMat, newCols)
    newRows <- matrix(0, nrow=length(nodes), ncol=ncol(inciMat))
    inciMat <- rbind(inciMat, newRows)
    colnames(inciMat) <- nms
    inciMat
}


.getIndices <- function(nodes, from, to) {
    ## Return indices into the inciMat for nodes from and to.
    i <- match(from, nodes, nomatch=0)
    if (i == 0)
      stop("Unknown node", sQuote(from), "specified in from")
    j <- match(to, nodes, nomatch=0)
    if (j == 0)
      stop("Unknown node", sQuote(to), "specified in to")
    list(from=i, to=j)
}
    


setMethod("addNodes",
          signature(object="graphIM", nodes="character"),
          function(object, nodes) {
              ## TODO: allow adding node objects and possibly edges
              ## TODO: Can the verification code be shared?  Perhaps put it
              ##       in the generic?
              already <- nodes %in% nodes(object)
              if(any(already))
                stop(paste(sQuote(nodes[already]), collapse=", "),
                           " are already nodes in the graph")
              object@inciMat <- .extendInciMat(object@inciMat, nodes)
              ## need to clone edgeSet
              object
          })


setMethod("addEdge",
          signature(from="character", to="character", graph="graphIM",
                    weights="missing"),
          function(from, to, graph) {
              if (isAdjacent(graph, from, to))
                stop("edge from ", sQuote(from), " to ", sQuote(to),
                     "already exists")
              idx <- .getIndices(nodes(graph), from, to)
              graph@inciMat[idx$from, idx$to] <- 1
              if (! isDirected(graph))
                graph@inciMat[idx$to, idx$from] <- 1
              graph
          })
              
              
setMethod("clearNode",
          signature(node="character", object="graphIM"),
          function(node, object) {
              idx <- .getNodeIndex(nodes(object), node)
              zeroVect <- rep(0, ncol(object@inciMat))
              ## clear edges from node to other
              object@inciMat[idx, ] <- zeroVect
              ## clear edges from other to node
              object@inciMat[, idx] <- zeroVect

              ## TODO: clear edge attributes


              object
          })


## TODO: implement a clearEdgeAttributes method

setMethod("removeNode",
          signature(node="character", object="graphIM"),
          function(node, object) {
              idx <- .getNodeIndex(nodes(object), node)
              object@inciMat <- object@inciMat[-idx, -idx]

              ## TODO: clear edge attributes
              
              object
          })


.getNodeIndex <- function(nodeNames, node) {
    idx <- match(node, nodeNames, nomatch=NA)
    if (is.na(idx))
      stop("Unknown node", sQuote(node))
    idx
}


setMethod("removeEdge",
          signature(from="character", to="character", graph="graphIM"),
          function(from, to, graph) {
              fromIdx <- .getNodeIndex(nodes(graph), from)
              toIdx <- .getNodeIndex(nodes(graph), to)
              graph@inciMat[fromIdx, toIdx] <- 0
              if (!isDirected(graph))
                graph@inciMat[toIdx, fromIdx] <- 0
              graph
          })



              


