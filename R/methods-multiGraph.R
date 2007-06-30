##some methods for the multigraph class

setMethod("show", signature("multiGraph"),
          function(object) {
              numNodes<- numNodes(object)
               cat("A", class(object), "with \n")
              cat("Number of nodes =", numNodes, "\n")
              cat("Number of edge lists =", length(object@edgeL), "\n")
          })

setMethod(nodes, signature(object="multiGraph"),
          function(object)  object@nodes)

setMethod(numNodes, signature(object="multiGraph"),
          function(object) length(nodes(object)))


setMethod("edges", signature("multiGraph", "missing"),
          function(object) {
              nV = nodes(object)
              lapply(object@edgeL, function(x) edges(x, nV, nV))
          })

setMethod("edges", signature("multiGraph", "character"),
          function(object, which) {
              nV = nodes(object)
              lapply(object@edgeL, function(x) edges(x, which, nV))
          })

#####edgeSet methods here for now as well

##this is a bit dangerous as these are not really the nodes of the
##graph - we don't enforce having all nodes in the adj matrix
setMethod("isDirected", "edgeSet",
	  function(object) object@edgemode == "directed")

setMethod("edgemode", "edgeSet", function(object) object@edgemode)

setMethod("show", signature("edgeSet"),
          function(object) {
              numEdge<-numEdges(object)
              cat("A", class(object), "with", object@edgemode, "edges\n")
              cat("Number of Edges =", numEdge, "\n")
          })


###edgeSetAM methods

setMethod(nodes, signature(object="edgeSetAM"),
          function(object)  rownames(object@adjMat))

#setMethod("edges", signature("edgeSetAM", "missing"),
#          function(object) {
#              getEdgeList(object@adjMat, nodes(object))
#          })

setMethod("edges", signature("edgeSetAM", "character"),
          function(object, which, nodes) {
              stopifnot( is.character(nodes) )
              idx <- base::which(colnames(object@adjMat) %in% which)
              getEdgeList(object@adjMat[idx, ], nodes[idx])
          })

setMethod("numEdges", signature(object="edgeSetAM"),
          function(object) {
              nE <- sum(object@adjMat != 0)
              if (!isDirected(object)) {
                  selfLoops <- sum(diag(object@adjMat) != 0)
                  nE <- selfLoops + (nE - selfLoops)/2
              }
              nE
          })

          
##edgeSetNEL methods here
##and here we are a bit scuppered by the way we represent the edge
##lists - we need to have the node set around

#setMethod("edges", c("edgeSetNEL", "missing"), function(object, which) {
#    gNodes <- object@nodes
#    lapply(object@edgeL, function(x) gNodes[x$edges])})

setMethod("edges", signature("edgeSetNEL", "character"),
          function(object, which, nodes) {
              stopifnot( is.character(nodes) )
              lapply(object@edgeL[which], function(x) nodes[x$edges])})

