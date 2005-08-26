setMethod("initialize", signature("edgeSet"),
          function(.Object, attrList) {
              .Object@data <- new.env(hash=TRUE, parent=NULL)
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


setMethod("addEdge2", signature(container="edgeSet", from="character", to="character"),
          function(container, from, to, attrList) {
              eName <- .makeKey(container, from, to)
              if (exists(eName, container@data))
                stop("Cannot add edge because it already exists")
              if (missing(attrList))
                attrList <- list()
              ## XXX: rely on data slot being an environment
              ##      and hence a reference.
              container@data[[eName]] <- attrList
              invisible(container)
          })


.addDefaultAttrs <- function(propList, defaults) {
    missingAttrs <- ! (names(defaults) %in%  names(propList))
    propList <- c(propList, defaults[missingAttrs])
    propList
}


setMethod("getEdge", signature(container="edgeSet", from="character",
                               to="character"),
          function(container, from, to) {
              if (length(from) != 1 || length(to) != 1)
                stop("from and to must specify single nodes")
              eName <- .makeKey(container, from, to)
              attrs <- container@data[[eName]]
              attrs <- .addDefaultAttrs(attrs, container@attrList)
              attrs
          })


setMethod("getEdges", signature(container="edgeSet", from="character",
                                to="character"),
          function(container, from, to) {
              eNames <- .makeKey(container, from, to)
              eAttrs <- mget(eNames, container@data)
              eAttrs <- lapply(eAttrs, .addDefaultAttrs, container@attrList)
              eAttrs
          })


setMethod("removeEdges", signature(container="edgeSet", from="character",
                                   to="character"),
          function(container, from, to) {
              eNames <- .makeKey(container, from, to)
              remove(list=eNames, envir=container@data)
          })




          
