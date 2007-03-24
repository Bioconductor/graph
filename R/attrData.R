setMethod("initialize", signature("attrData"),
          function(.Object, defaults) {
              .Object@data <- list()
              if (missing(defaults))
                defaults <- list()
              else {
                  if (is.null(names(defaults)) || any(is.na(names(defaults))))
                    stop("defaults must have names for all elements")
              }
              .Object@defaults <- defaults
              .Object
          })


.addDefaultAttrs <- function(attrData, defaults) {
    if (is.null(attrData))
      return(defaults)
    defaults[names(attrData)] <- attrData
    defaults
}


.verifyAttrListNames <- function(attrData, defaults) {
    if (any(! names(attrData)  %in% names(defaults))) {
        nms <- names(attrData)
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
    graph:::.checkAttrLength(attrName)
    if (! attrName %in% knownNames)
      stop("unknown attribute name: ", sQuote(attrName))
    TRUE
}
    

setMethod("attrDefaults", signature(self="attrData", attr="missing"),
          function(self, attr) {
              self@defaults
          })


setMethod("attrDefaults", signature(self="attrData", attr="character"),
          function(self, attr) {
              graph:::.verifyAttrName(attr, names(self@defaults))
              self@defaults[[attr]]
          })


setReplaceMethod("attrDefaults", signature(self="attrData", attr="character",
                                           value="ANY"),
                 function(self, attr, value) {
                     graph:::.checkAttrLength(attr)
                     self@defaults[[attr]] <- value
                     self
                 })


setReplaceMethod("attrDefaults", signature(self="attrData", attr="missing",
                                           value="list"),
                 function(self, attr, value) {
                     if (is.null(names(value)))
                       stop("attribute list must have names")
                     self@defaults <- value
                     self
                 })


setMethod("attrDataItem", signature(self="attrData", x="character",
                                    attr="missing"),
          function(self, x, attr) {
              itemData <- self@data[x]
              ## unknown items will have name NA and value NULL
              names(itemData) <- x 
              itemData <- lapply(itemData, .addDefaultAttrs, self@defaults)
              itemData
          })


setMethod("attrDataItem", signature(self="attrData", x="character",
                                    attr="character"),
          function(self, x, attr) {
              graph:::.verifyAttrName(attr, names(self@defaults))
              .Call("graph_attrData_lookup", self, x, attr, PACKAGE="graph")
          })


setReplaceMethod("attrDataItem",
                 signature(self="attrData", x="character", attr="character",
                           value="ANY"),
                 function(self, x, attr, value) {
                     graph:::.verifyAttrName(attr, names(self@defaults))
                     if (length(value) > 1 && length(value) != length(x))
                       stop("invalid args: value must be length one or ",
                            "have the same length as x")
                     self@data <- .Call("graph_sublist_assign",
                                        self@data, x, attr, value,
                                        PACKAGE="graph")
                     self
          })


setReplaceMethod("removeAttrDataItem",
                 signature(self="attrData", x="character", value="NULL"),
                 function(self, x, value) {
                     idx <- match(x, names(self@data))
                     idx <- idx[!is.na(idx)]
                     if (length(idx))
                       self@data <- self@data[-idx]
                     self
                 })


setMethod("names", "attrData",
          function(x) {
              names(x@data)
          })


setReplaceMethod("names", signature(x="attrData", value="character"),
                 function(x, value) {
                     if (length(x@data) != length(value))
                       stop("'value' argument length doesn't match data")
                     if (any(duplicated(value)))
                       stop("'value' argument must specify unique names")
                     if (any(is.na(value)))
                       stop("'value' argument cannot contain NAs")
                     names(x@data) <- value
                     x
                 })
