##Copyright 2002 Robert Gentleman, all rights reserved

##a class structure for graphs
##the class structure is based on GXL



##for undirected graphs the toEdges and fromEdges lists are the same

setMethod("initialize",
          signature(.Object = "gNode"),
          function (.Object, label="", fromEdges=list(),
                    toEdges=list(), edgeOrder=list(),
                    nodeType="", property=list(), nodeID=getuuid(), ...)
      {
          .Object@label <- label
          .Object@fromEdges <- fromEdges
          .Object@toEdges <- toEdges
          .Object@edgeOrder <- edgeOrder
          .Object@nodeType <- nodeType
          .Object@nodeID <- nodeID
          .Object@property <- asGraphProperty(property)
          .Object
      })


##I think we need to separate directed from the type of the edge
##if directed=FALSE then the bNode and eNode are just ends, not
##beginning and ending nodes

setMethod("initialize",
          signature(.Object = "gEdge"),
          function (.Object, edgeType="", directed=FALSE,
                    bNode, eNode, property=1.0, edgeID=getuuid(), ...)
      {
          property <- asGraphProperty(property, hasWeight=TRUE)
          if (is(bNode, "gNode"))
              bNode <- nodeID(bNode)
          if (is(eNode, "gNode"))
              eNode <- nodeID(eNode)
          .Object@edgeType <- edgeType
          .Object@directed <- directed
          .Object@bNode <- bNode
          .Object@eNode <- eNode
          .Object@property <- property
          .Object@edgeID <- edgeID
          .Object
      })

##setup the accessor functions

setMethod("edgeID", "gEdge", function(object)
          object@edgeID)

setMethod("eNode", "gEdge", function(object)
          object@eNode)

setMethod("bNode", "gEdge", function(object)
          object@bNode)


setMethod("toEdges", "gNode", function(object) object@toEdges)

setReplaceMethod("toEdges", "gNode", function(object, value) {
    object@toEdges <- value
    object})

setMethod("fromEdges", "gNode", function(object) object@fromEdges)

setReplaceMethod("fromEdges", "gNode", function(object, value) {
    object@fromEdges <- value
    object})


setMethod("label", "gNode", function(object) object@label)


setMethod("edgeOrder", "gNode", function(object) object@edgeOrder)



setMethod("nodeID", "gNode", function(object) object@nodeID)

setMethod("nodeType", "gNode", function(object) object@nodeType)


#### hashtables -- very primitive start


setMethod("initialize", "hashtable", function(.Object) {
    .Object@hashtable=new.env(hash=TRUE)
    .Object})



setMethod("hash", signature("ANY", "ANY", "hashtable"),
          function(key, value, htable) {
              if(!is.character(key) )
                  key <- as.character(key)
              assign(key, value, env= htable@hashtable)
          })


#### define a general graph structure here

setMethod("initialize", "generalGraph", function(.Object,
                                                 nodes=NULL, edges=NULL) {
    .Object@nodes = new("hashtable")
    .Object@edges = new("hashtable")
    for(node in nodes )
        hash(nodeID(node), node,.Object@nodes )
    for(edge in edges )
        hash(edgeID(edge), edge,.Object@edges)
    .Object})

##coercion to generalGraph -- this will not be efficient
setAs("graphNEL", "generalGraph", def=function(from) {
    nodes <- nodes(from)
    edges <- edges(from)
    eWts <- edgeWeights(from)
    nNodes <- length(nodes)
    nodeIDS <- as.list(rep(NA, nNodes))
    names(nodeIDS) <- nodes
    for(i in seq(along=nodes) ) nodeIDS[[i]] <- newID()
    nEdges <- numEdges(from)
    nodeObj <- vector("list", length=nNodes)
    edgeObj <- vector("list", length=nEdges)
    edgeCt <- 1
    tlist <- vector("list", length=nNodes) ##to edges
    flist <- vector("list", length=nNodes) ##from edges
    for( i in seq(along=nodes) ) {
        tlist[[i]] <- vector("list", 0)
        flist[[i]] <-  vector("list", 0)
    }
    if( edgemode(from) != "directed" )
        DIRECTED <- FALSE
    else
        DIRECTED <- TRUE

    for(j in seq(along=nodes) ) {
        eD <- edges[[j]]
        eW <- eWts[[j]]
        ##for undirected graphs we drop 1/2 the edges
        if( !DIRECTED ) {
            whE <- match(eD, nodes)
            eD <- eD[whE>j]
            eW <- eW[whE>j]
            whE <- whE[whE>j]
        }
        ct2 <- 1
        for( e1 in eD ) {
            eID <- newID();
            edgeObj[[edgeCt]] <-
                new("gEdge", edgeID=eID, weight=eW[ct2], bNode =
                    nodeIDS[[j]], eNode = nodeIDS[[whE[ct2]]])
            flist[[j]] <- c(flist[[j]], eID)
            tlist[[whE[ct2]]] <- c(tlist[[whE[ct2]]], eID)
            ct2 <- ct2+1
            edgeCt <- edgeCt+1
        }
    }
    for(i in seq(along=nodes) )
        nodeObj[[i]] <- new("gNode", label=nodes[j], fromEdges=flist[[i]],
                            toEdges=tlist[[i]], nodeID=nodeIDS[[i]])

    new("generalGraph", nodes=nodeObj, edges=edgeObj)
})
