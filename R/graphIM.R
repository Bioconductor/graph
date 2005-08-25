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



setMethod("edges", signature("graphIM", "missing"),
          function(object) {
              apply(object@inciMat, 1, function(aRow)
                    names(base::which(aRow != 0)))
          })


setMethod("edges", signature("graphIM", "character"),
          function(object, which) {
              idx <- base::which(colnames(object@inciMat) %in% which)
              apply(object@inciMat[idx, ], 1, function(aRow)
                    names(base::which(aRow != 0)))
          })


setMethod("nodes", signature("graphIM"),
          function(object) {
              rn <- rownames(object@inciMat)
              if (is.null(rn))
                nodes <- as.character(1:nrow(object@inciMat))
              else
                nodes <- rn
              nodes
          })


setReplaceMethod("nodes", signature("graphIM", "character"),
                 function(object, value) {
                     if(length(value) != nrow(object@inciMat))
                       stop("need as many names as there are nodes")
                     if(any(duplicated(value)))
                       stop("node names must be unique")
                     rownames(object@inciMat) <- value
                     ## don't really need both...
                     colnames(object@inciMat) <- value
                     object})

