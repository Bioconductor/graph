##Copyright 2002 Robert Gentleman, all rights reserved

##a class structure for graphs
##the class structure is based on GXL

    setClass("graphID")
    ##some functions to allow nodes and edges to have identifiers
    ##there are lots of problems with the integer version --
##    if( require(Ruuid, quietly=TRUE) ) {
##        setIs( "Ruuid", "graphID")
##        .haveUUID <- TRUE
##    }  else{
        setClassUnion("graphID", "integer")
        .haveUUID <- FALSE
##        warning("without Ruuid you may have problems with node or edge IDS")
##    }

    ##here we set up some global variables (that should be internal to
    ##and will be once we have namespaces
    assign("idenv", new.env(hash=TRUE))
    assign("idval", 1, env=idenv)

    if( .haveUUID ) {
        assign("startids", function(x) NULL)
        assign("newID", getuuid)
##        assign("nullgraphID", new("Ruuid"), pos="package:graph")
    } else {
        assign("startids", function(x) assign("idval", x, env=idenv))
        assign("newID", function() {
            val <- get("idval", env=idenv)
            assign("idval", val+1, env=idenv)
            return(val)
        })
        assign("nullgraphID", as.integer(-1))
        }

    ##for undirected graphs the toEdges and fromEdges lists are the same
    setClass("gNode",
           representation(nodeID="graphID",
                          nodeType="character",
                          toEdges="list",
                          fromEdges="list",
                          edgeOrder="numeric",
                          label="character" ),
           prototype = list(nodeID=as.integer(1), nodetype="unknown",
             edgeOrder=0, label=""))
    ##I think we need to separate directed from the type of the edge
    ##if directed=FALSE then the bNode and eNode are just ends, not
    ##beginning and ending nodes
    setClass("gEdge",
          representation(edgeID="graphID",
                         edgeType="character",
                         weight="numeric",
                         directed="logical",
                         bNode="graphID",    ##begin - if directed
                         eNode="graphID"),   ##end   - if directed
          prototype = list(edgeID=nullgraphID, edgeType="unknown",
          directed=FALSE, bNode=nullgraphID, eNode=nullgraphID, weight=1))

    setClass("simpleEdge",
             representation(edgeType="character",
                            weight="numeric",
                            directed="logical",
                            bNode="character",    ##begin - if directed
                         eNode="character"),   ##end   - if directed
          prototype = list(edgeType="unknown",
          directed=FALSE, bNode="", eNode="", weight=1))
    ##setup the accessor functions

    if (is.null(getGeneric("edgeID")) && !exists("edgeID",
                                                 mode="function") )
        setGeneric("edgeID", function(object)
                   standardGeneric("edgeID"))
    setMethod("edgeID", "gEdge", function(object)
              object@edgeID)

    if (is.null(getGeneric("eNode")))
        setGeneric("eNode", function(object)
                   standardGeneric("eNode"))
    setMethod("eNode", "gEdge", function(object)
              object@eNode)
    if (is.null(getGeneric("bNode")))
        setGeneric("bNode", function(object)
                   standardGeneric("bNode"))
    setMethod("bNode", "gEdge", function(object)
              object@bNode)

   setGeneric("toEdges", function(object) standardGeneric("toEdges"))

   setMethod("toEdges", "gNode", function(object) object@toEdges)

   setGeneric("toEdges<-",
               function(object, value) standardGeneric("toEdges<-"))
   setReplaceMethod("toEdges", "gNode", function(object, value) {
      object@toEdges <- value
      object})

    setGeneric("fromEdges",
               function(object) standardGeneric("fromEdges"))
   setMethod("fromEdges", "gNode", function(object) object@fromEdges)

    setGeneric("fromEdges<-",
               function(object, value) standardGeneric("fromEdges<-"))
     setReplaceMethod("fromEdges", "gNode", function(object, value) {
      object@fromEdges <- value
      object})

      setGeneric("label", function(object) standardGeneric("label"))
   setMethod("label", "gNode", function(object) object@label)

     setGeneric("edgeOrder", function(object) standardGeneric("edgeOrder"))
   setMethod("edgeOrder", "gNode", function(object) object@edgeOrder)

    setGeneric("nodeID", function(object) standardGeneric("nodeID"))

   setMethod("nodeID", "gNode", function(object) object@nodeID)

    setGeneric("nodeType", function(object) standardGeneric("nodeType"))

   setMethod("nodeType", "gNode", function(object) object@nodeType)


#### hashtables -- very primitive start

    setClass("hashtable", representation(hashtable="environment"))
    setMethod("initialize", "hashtable", function(.Object) {
        .Object@hashtable=new.env(hash=TRUE)
        .Object})

   if( !exists("hash", mode="function") )
       setGeneric("hash", function(key, value, htable) standardGeneric("hash"))

    setMethod("hash", signature("ANY", "ANY", "hashtable"),
              function(key, value, htable) {
                  if(!is.character(key) )
                      key <- as.character(key)
                  assign(key, value, env= htable@hashtable)
              })

      if( !isGeneric("contents") && !exists("contents", mode="function") )
       setGeneric("contents", function(object)
                  standardGeneric("contents"))

    setMethod("contents", "hashtable",
              function(object) ls(env=object@hashtable))


#### define a general graph structure here
    setClass("generalGraph", representation(nodes="hashtable",
                                            edges="hashtable"),
                                            contains="graph")

    setMethod("initialize", "generalGraph", function(.Object,
             nodes=NULL, edges=NULL) {
        .Object@nodes = new("hashtable")
        .Object@edges = new("hashtable")
        browser()
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

# as(gR, "generalGraph")

