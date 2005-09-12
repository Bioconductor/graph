setMethod("initialize", signature("nodeSet"),
          function(.Object, attrList) {
              .Object@data <- list()
              if (missing(attrList))
                attrList <- list()
              .Object@attrList <- attrList
              .Object
          })


.addDefaultAttrs <- function(propList, defaults) {
    missingAttrs <- ! (names(defaults) %in%  names(propList))
    propList <- c(propList, defaults[missingAttrs])
    propList
}


.verifyAttrListNames <- function(propList, defaults) {
    if (any(! names(propList)  %in% names(defaults))) {
        nms <- names(propList)
        badNms <- nms[! nms %in% names(defaults)]
        stop("The following attribute names ",
             "were not found in the nodeSet attributes: ",
             paste(badNms, collapse=", "))
    } else {
        TRUE
    }
}


setMethod("nodeProps",
          signature(object="nodeSet", n="character"),
          function(object, n) {
              attrs <- object@data[n]
              attrs <- lapply(attrs, .addDefaultAttrs, object@attrList)
              names(attrs) <- n
              attrs
          })


setMethod("nodeProps",
          signature(object="nodeSet", n="missing"),
          function(object, n) {
              attrs <- lapply(object@data, .addDefaultAttrs, object@attrList)
              names(attrs) <- names(object@data)
              attrs
          })


## setReplaceMethod("nodeProps",
##                  signature(object="nodeSet", value="list"),
##                  function(object, value) {
##                      if (is.null(names(value)))
##                          stop("must specify nodeProps with a named list")
##                      nodeNames <- names(value)
##                      nil <- lapply(value, .verifyAttrListNames, object@attrList)
##                      for (i in 1:length(nodeNames)) {
##                          nodeName <- nodeNames[i]
##                          val <- value[[i]]
##                          object@data[[nodeName]] <- val
##                      }
##                      object
##                  })
              

setReplaceMethod("nodeProps",
                 signature(object="nodeSet", n="character", value="list"),
                 function(object, n, value) {
                     if (is.null(names(value)))
                         stop("must specify nodeProps with a named list")
                     .verifyAttrListNames(value, object@attrList)
                     for (nodeName in n) {
                         object@data[[nodeName]] <- value
                     }
                     object
                 })
              




          
