setMethod("initialize", signature("edgeSet"),
          function(.Object, attrList) {
              .Object@data <- list()
              ## hard code the key separator
              .Object@keySep <- "|"
              if (missing(attrList))
                attrList <- list()
              .Object@attrList <- attrList
              .Object
          })

              
.makeKey <- function(es, from, to) {
    paste(from, to, sep=es@keySep)
}


.parseKey <- function(es, key) {
    flds <- split(key, es@keySep)
    list(from=flds[1], to=flds[2])
}


.addDefaultAttrs <- function(propList, defaults) {
    missingAttrs <- ! (names(defaults) %in%  names(propList))
    propList <- c(propList, defaults[missingAttrs])
    propList
}


.verifyAttrListNames <- function(propList, defaults) {
    if (any(! names(propList)  %in% names(defaults))) {
        nms <- names(propList)
        badNms <- nms[! nms %in% names(defaults)]
        stop("The following attribute names",
             "were not found in the edgeSet attributes:",
             paste(badNms, collapse=", "))
    } else {
        TRUE
    }
}


setMethod("edgeProps",
          signature(object="edgeSet", from="character", to="character"),
          function(object, from, to) {
              ## TODO: make edgeProps access vectorized, for now, force single edges
              if (length(from) != length(to))
                stop("length of from and to must be equal")
##               if (length(from) != 1 || length(to) != 1)
##                 stop("from and to must specify single nodes")
              eNames <- .makeKey(object, from, to)
              attrs <- object@data[eNames]
              attrs <- lapply(attrs, .addDefaultAttrs, object@attrList)
              names(attrs) <- eNames
              attrs
          })


setReplaceMethod("edgeProps",
                 signature(object="edgeSet", from="character", to="character",
                           value="list"),
                 function(object, from, to, value) {
                     .verifyAttrListNames(value, object@attrList)
                     eName <- .makeKey(object, from, to)
                     object@data[[eName]] <- value
                     object
                 })
              




          
