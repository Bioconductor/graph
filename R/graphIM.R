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


.isValidInciMat <- function(inciMat) {
    if (! nrow(inciMat) == ncol(inciMat))
      stop("incidence matrix must be square")
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


setMethod("initialize", signature("graphIM"),
          function(.Object, inciMat, nodes=NULL) {
              nNames <- .isValidInciMat(inciMat)
              if (is.null(nNames))
                nNames <- paste("n", 1:ncol(inciMat), sep="")
              colnames(inciMat) <- nNames
              rownames(inciMat) <- NULL
              .Object@inciMat <- inciMat
              if (!is.null(nodes)) {
                  .isValidNodeList(nodes, nNames)
                   .Object@nodes <- new.env(hash=TRUE, parent=NULL)
                  for (n in nNames) {
                      .Object@nodes[[n]] <- nodes[[n]]
                  }
              }
              .Object
          })


.getEdgeList <- function(inciMat, nodeNames) {
    eList <- apply(inciMat, 1, function(aRow) names(base::which(aRow != 0)))
    names(eList) <- nodeNames
    eList
}


setMethod("edges", signature("graphIM", "missing"),
          function(object) {
              .getEdgeList(object@inciMat, nodeNames(object))
          })


setMethod("edges", signature("graphIM", "character"),
          function(object, which) {
              idx <- base::which(colnames(object@inciMat) %in% which)
              .getEdgeList(object@inciMat[idx, ], nodeNames(object)[idx])
          })


setMethod("nodes", signature("graphIM"),
          function(object, which) {
              if (missing(which))
                return(as.list(object@nodes))
              else
                return(mget(which, object@nodes))
          })


setMethod("nodeNames", signature("graphIM"),
          function(object) {
              ## initialize gaurantees colnames
              colnames(object@inciMat)
          })


setReplaceMethod("nodeNames", signature("graphIM", "character"),
                 function(object, value) {
                     if(length(value) != ncol(object@inciMat))
                       stop("need as many names as there are nodes")
                     if(any(duplicated(value)))
                       stop("node names must be unique")
                     colnames(object@inciMat) <- value
                     object
                 })

