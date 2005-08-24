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

setClass("graphIM", contains="graph",
         representation(inciMat="matrix"),
         validity=function(object) validGraphIM(object))


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

