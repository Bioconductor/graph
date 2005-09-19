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
              itemData <- lapply(self@data[x], function(alist) {
                  val <- alist[[attr]]
                  if (is.null(val))
                    self@defaults[[attr]]
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
                     graph:::.verifyAttrName(attr, names(self@defaults))
                     wasAsIs <- FALSE
                     if (is(value, "AsIs")) {
                         wasAsIs <- TRUE
                         ## TODO: consider removing AsIs class here
                         ## this isn't trivial if we started w/ built-in
                         ## class :-(
                         classVect <- class(value)
                         if (length(classVect) > 1)
                           class(value) <- classVect[2:length(classVect)]
                         else
                             class(value) <- NULL
                     }
                     assignWholeItem <- FALSE
                     if (length(value) == 1 || length(value) != length(x)
                         || wasAsIs)
                       assignWholeItem <- TRUE
                     for (i in 1:length(x)) {
                         item <- x[i]
                         alist <- self@data[[item]]
                         if (is.null(alist))
                           self@data[[item]] <- list()
                         if (assignWholeItem)
                           self@data[[item]][[attr]] <- value
                         else
                           self@data[[item]][[attr]] <- value[[i]]
                     }
                     self
          })              




          
