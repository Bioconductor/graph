##some methods for the multigraph class

setMethod("show", signature("multiGraph"),
          function(object) {
              numNodes<- numNodes(object)
               cat("A", class(object), "with \n")
              cat("Number of nodes =", numNodes, "\n")
              cat("Number of edge lists =", length(object@edgeL), "\n")
          })

setMethod("isDirected", "multiGraph",
	  function(object)
          sapply(object@edgeL, isDirected))


setMethod("nodes", signature(object="multiGraph"),
          function(object)  object@nodes)

setMethod("numNodes", signature(object="multiGraph"),
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

numEHelper = function(gEdges, directed) {
    if (length(gEdges) == 0)
        return(length(gEdges))
    numEdges <- length(unlist(gEdges, use.names=FALSE))
    if (directed) {
        numSelfLoops <- sum(mapply(function(e, n) sum(n == e),
                                   gEdges, names(gEdges)))
        numEdges <- numSelfLoops + (numEdges - numSelfLoops) / 2
    }
    numEdges
}
    
setMethod("numEdges", signature(object="multiGraph"),
          function(object) {
              gEdges <- edges(object)
              dir <- isDirected(object)
              ans <- rep(NA, length(dir))
              for(i in 1:length(dir))
                  ans[i] = numEHelper(gEdges[[i]], dir[i])
              ans
          })

##we need a validity checking method: ensure that nodes are the same
##in all edgeSets - which is hard as the edgeSets don't always seem to
##know

#####edgeSet methods here

##this is a bit dangerous as these are not really the nodes of the
##graph - we don't enforce having all nodes in the adj matrix

setMethod("isDirected", "edgeSet",
	  function(object) edgemode(object) == "directed")

setMethod("edgemode", "edgeSet", function(object) edgemode(object))

setMethod("show", signature("edgeSet"),
          function(object) {
              numEdge<-numEdges(object)
              cat("A", class(object), "with", edgemode(object), "edges\n")
              cat("Number of Edges =", numEdge, "\n")
          })


###edgeSetAM methods

setMethod("nodes", signature(object="edgeSetAM"),
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



