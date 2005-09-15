setMethod("initialize", signature("attrData"),
          function(.Object, attrList) {
              .Object@data <- list()
              if (missing(attrList))
                attrList <- list()
              else {
                  if (is.null(names(attrList)) || any(is.na(names(attrList))))
                    stop("attrList must have names for all elements")
              }
              .Object@attrList <- attrList
              .Object
          })


.addDefaultAttrs <- function(attrList, defaults) {
    if (is.null(attrList))
      return(defaults)
    defaults[names(attrList)] <- attrList
    defaults
}


.verifyAttrListNames <- function(attrList, defaults) {
    if (any(! names(attrList)  %in% names(defaults))) {
        nms <- names(attrList)
        badNms <- nms[! nms %in% names(defaults)]
        stop("The following attribute names ",
             "were not found in the attrData attributes: ",
             paste(badNms, collapse=", "))
    } else {
        TRUE
    }
}


.checkAttrLength <- function(attrName) {
    if (length(attrName) != 1)
      stop("attr argument must specify a single attribute name.")
}


.verifyAttrName <- function(attrName, knownNames) {
    .checkAttrLength(attrName)
    if (! attrName %in% knownNames)
      stop("unknown attribute name: ", sQuote(attrName))
    TRUE
}
    

setMethod("attrDefaults", signature(self="attrData", attr="missing"),
          function(self, attr) {
              self@attrList
          })


setMethod("attrDefaults", signature(self="attrData", attr="character"),
          function(self, attr) {
              .verifyAttrName(attr, names(self@attrList))
              self@attrList[[attr]]
          })


setReplaceMethod("attrDefaults", signature(self="attrData", attr="character",
                                           value="ANY"),
                 function(self, attr, value) {
                     .checkAttrLength(attr)
                     self@attrList[[attr]] <- value
                     self
                 })


setReplaceMethod("attrDefaults", signature(self="attrData", attr="missing",
                                           value="list"),
                 function(self, attr, value) {
                     if (is.null(names(value)))
                       stop("attribute list must have names")
                     self@attrList <- value
                     self
                 })


setMethod("attrDataItem", signature(self="attrData", x="character",
                                    attr="missing"),
          function(self, x, attr) {
              itemData <- self@data[x]
              ## unknown items will have name NA and value NULL
              names(itemData) <- x 
              itemData <- lapply(itemData, .addDefaultAttrs, self@attrList)
              itemData
          })


setMethod("attrDataItem", signature(self="attrData", x="character",
                                    attr="character"),
          function(self, x, attr) {
              .verifyAttrName(attr, names(self@attrList))
              itemData <- lapply(self@data[x], function(alist) {
                  val <- alist[[attr]]
                  if (is.null(val))
                    self@attrList[[attr]]
                  else
                    val
              })
              ## unknown items will have name NA and value NULL
              names(itemData) <- x 
              itemData
          })


setReplaceMethod("attrDataItem",
                 signature(self="attrData", x="character", attr="character",
                           value="ANY"),
                 function(self, x, attr, value) {
                     .verifyAttrName(attr, names(self@attrList))
                     for (item in x) {
                         alist <- self@data[[item]]
                         if (is.null(alist))
                           self@data[[item]] <- list(attr=value)
                         else
                           self@data[[item]][[attr]] <- value
                     }
                     self
          })



## setMethod("props",
##           signature(object="attrData", p="character"),
##           function(object, p) {
##               attrs <- object@data[p]
##               attrs <- lapply(attrs, .addDefaultAttrs, object@attrList)
##               names(attrs) <- p
##               attrs
##           })


## setMethod("props",
##           signature(object="nodeSet", p="missing"),
##           function(object, p) {
##               attrs <- lapply(object@data, .addDefaultAttrs, object@attrList)
##               names(attrs) <- names(object@data)
##               attrs
##           })


## setReplaceMethod("props",
##                  signature(object="attrData", p="character", value="list"),
##                  function(object, p, value) {
##                      if (is.null(names(value)))
##                          stop("Attributes must be specified with a named list")
##                      .verifyAttrListNames(value, object@attrList)
##                      for (propName in p) {
##                          object@data[[propName]] <- value
##                      }
##                      object
##                  })
              




          
