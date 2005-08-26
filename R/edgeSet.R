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

              
.makeKey <- function(from, to, es) {
    paste(from, to, sep=es@keySep)
}


.parseKey <- function(key, es) {
    flds <- split(key, es@keySep)
    list(from=flds[1], to=flds[2])
}


setMethod("addEdge2", signature(from="character", to="character",
                                object="edgeSet"),
          function(from, to, object, attrList) {
              ## should this be makeKey(object, from, to) ???
              eName <- .makeKey(from, to, object)
              if (exists(eName, object@data))
                stop("Cannot add edge because it already exists")
              if (missing(attrList))
                attrList <- list()
              ## XXX: rely on data slot being an environment
              ##      and hence a reference.
              object@data[[eName]] <- attrList
              invisible(object)
          })


setMethod("getEdge", signature(from="character", to="character",
                               object="edgeSet"),
          function(from, to, object) {
              if (length(from) != 1 || length(to) != 1)
                stop("from and to must specify single nodes")
              eName <- .makeKey(from, to, object)
              object@data[[eName]]
          })


setMethod("getEdges", signature(from="character", to="character",
                               object="edgeSet"),
          function(from, to, object) {
              eNames <- .makeKey(from, to, object)
              mget(eNames, object@data)
          })


setMethod("removeEdges", signature(from="character", to="character",
                                   object="edgeSet"),
          function(from, to, object) {
              eNames <- .makeKey(from, to, object)
              remove(list=eNames, envir=object@data)
          })




          
