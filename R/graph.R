
validGraph<-function(object)
{
  if (class(object) == "graphNEL")
  {
    objEdges<-edges(object)
    objNodes<-nodes(object)

    if (any(is.na(objNodes)))
      stop("NA element in nodes.")
    if (any(is.na(unlist(objEdges,use.names=F))))
      stop("NA element in edges.")
    if (length(objNodes)!=length(objEdges))
      stop("Nodes and edges must have the same length.")
    if (!all(objNodes==names(objEdges)))
      stop("Edges don't have the same names as the nodes.")
    if (length(unlist(objEdges,use.names=F))%%2 != 0)
      stop("Each edge must be duplicated.")
  }
  return(TRUE)
}

#graph<-function(newnodes,newedges)
#{
#  new("graphNEL",nodes=as(newnodes,"vector"),
#         edgeL=as(newedges,"list"))
#}

.initGraph <- function(where)
{
  ## we define a virtual graph class -- to hold generic functions
  setClass("graph", representation(edgemode="character",
                                   "VIRTUAL"), where=where)

  if( !exists("edgemode", mode="function") )
      setGeneric("edgemode", function(object)
                 standardGeneric("edgemode"), where=where)
  setMethod("edgemode", "graph", function(object) object@edgemode,
            where=where)


  ## a node-edge-list graph
  ##the edgeL is a list, with edges, weights etc

  setClass("graphNEL",representation(nodes="vector",edgeL="list"),
            contains="graph",
           where=where)

  setMethod("initialize", "graphNEL", function(.Object, nodes=character(0),
       edgeL = vector("list",length=0), edgemode="undirected") {
       if( !missing(edgeL) && length(edgeL) > 1 )
         if( is.character(edgeL[[1]]) )
             edgeL <- lapply(edgeL, function(x) match(x, nodes))
       .Object@nodes <- nodes
       .Object@edgeL <- edgeL
       .Object@edgemode <- edgemode
       .Object},
       where=where)



  if (!isGeneric("nodes"))
  {
    if (is.function("nodes"))
      fun <- nodes
    else
      fun <- function(object) standardGeneric("nodes")
    setGeneric("nodes", fun, where=where)
  }

  setMethod("nodes", "graphNEL", function(object) object@nodes,
            where=where)

  setGeneric("nodes<-", function(object, value)
             standardGeneric("nodes<-"), where=where)
  setReplaceMethod("nodes", c("graphNEL", "character"),
             function(object, value) {
                 if(length(value) != length(object@nodes))
                     stop("need as many names as there are nodes")
                 if(any(duplicated(value)))
                     stop("node names must be unique")
                 object@nodes <- value
                 object}, where=where)

  if (!exists("edges", mode="function"))
      setGeneric("edges", function(object, which)
                 standardGeneric("edges"), where=where)

  ##the graphNEL representation stores edges as indexes into the
  ##node label vector
  setMethod("edges", c("graphNEL", "missing"), function(object, which) {
      gNodes <- object@nodes
      lapply(object@edgeL, function(x) gNodes[x$edges])},
            where=where)

  setMethod("edges", c("graphNEL", "character"),
            function(object, which) {
                gNodes <- nodes(object)
                lapply(object@edgeL[which], function(x) gNodes[x$edges])},
            where=where)

  if (is.null(getGeneric("uniqueEdges")))
      setGeneric("uniqueEdges", function(object)
                 standardGeneric("uniqueEdges"), where=where)

  ##FIXME: seem to have broken this
  setMethod("uniqueEdges", "graphNEL", function(object) {
      browser()
      edges <- edges(object)
      weights <- edgeWeights(object)
      nodes <- nodes(object)
      done <- character()
      outEdges <- list()

      newEdgeObj <- function(x, ID) {
          ## Helper function for the lapply below
          ## Creates a new gEdge object given specified
          ## information.
          eNodeVal <- as.integer(match(x,nodes))
          out <- new("gEdge", edgeID=newID(),
                     bNode=ID,
                     eNode=eNodeVal,
                     weight=as.numeric(curWeights[as.character(eNodeVal)])
                     )
          out
      }

      for (i in seq(along=nodes)) {
          curEdges <- edges[[i]]
          curWeights <- weights[[i]]
          newEdges <- curEdges[which(!(curEdges %in% done))]
          done <- c(done,nodes[i])
          outEdges <- c(outEdges,lapply(newEdges, newEdgeObj, ))
      }
      outEdges
  }, where=where)



  ##we are going to need some notion of degree
  if( !isGeneric("degree") )
    setGeneric("degree", function(object, Nodes) standardGeneric("degree"),
        where=where)

  setMethod("degree", signature(object="graph", Nodes="missing"),
      function(object)  {
          ns <- nodes(object)
          nl <- edges(object)
          deg <- sapply(nl, length)
          names(deg) <- ns
          deg
     }, where=where)

  setMethod("degree", "graph",  function(object, Nodes) {
       nl <- edges(object)
       nl <- nl[Nodes]

       deg <- sapply(nl, length)
       names(deg) <- Nodes
       deg
   }, where=where )

  setGeneric("edgeWeights", function(object, index)
      standardGeneric("edgeWeights"), where=where)

  setMethod("edgeWeights", "graphNEL", function(object, index) {
           if( missing(index) )
           	tlist <- object@edgeL
           else
                tlist <- object@edgeL[index]
           wts <- lapply(tlist, function(x) {
               wts <- x$weights
               if(is.null(wts))
                 wts <- rep(1, length(x$edges))
               wts})
            wts}, where=where)


##RG: some methods for adjcency and accessibility
  setGeneric("adj", function(object, index)
	standardGeneric("adj"), where = where)

  ## do we want the indices or the nodes?
  setMethod("adj", "graphNEL", function(object, index) {
        initI <- as.character(index)
        nd <- nodes(object)
        if( is.character(index) )
          index <- match(index, nd)
        if( is.na(index) || index < 0 || index > length(nd) )
          stop(paste("selected vertex", initI, "is not in the graph"))
	edges(object)[index]}, where = where )

  setGeneric("acc", function(object, index)
	standardGeneric("acc"), where=where)


##  setMethod("acc", "graph", function(object, index) {
##       visit <- function(ind) {
##          marked[ind] <<- TRUE
##          alist <- adj(object, ind)[[1]]
##          for( EDGE in alist) {
##            if( !marked[EDGE] ) {
##               visit(EDGE)
##               rval <<- c(EDGE, rval)
##            }
##          }
##        }
##        marked <- rep(FALSE, numNodes(object))
##        rval <- vector(length=0)
##        names(marked) <- nodes(object)
##        visit(index)
##        return(rval)
##   }, where = where)

##an iterative method ! yuck

  setMethod("acc", "graph", function(object, index) {
       nN <- numNodes(object)
       nNames<- nodes(object)
       marked<-rep(0, nN)
       distv <- rep(0, nN)
       names(distv) <- nNames
       distx <- 1
       names(marked) <- nNames
       nmkd <- 0
       marked[index] <- 1
       done <- FALSE
       while( !done ) {
         minds <- nNames[marked==1]
         for( node in minds) {
           avec <- adj(object, node)[[1]]
           avec <- avec[marked[avec]==0] #don't mark any already marked
           marked[avec] <- 1
           distv[avec] <- distx
         }
         marked[minds] <- 2
         distx <- distx+1
         newmk <- sum(marked==1)
         if( newmk == 0 )
           done <- TRUE
       }
       marked[index] <- 0 ##not the node itself
       return(distv[marked==2])
    }, where=where)

  setGeneric("dfs", function(object) standardGeneric("dfs"),
     where=where)

  setMethod("dfs", "graph", function(object) {
        visit <- function(ind) {
          #now <<- now+1
          marked[ind] <<- now
          alist <- adj(object, ind)[[1]]
          for( EDGE in alist)
            if( marked[EDGE]==0 )
               visit(EDGE)
         }
         now <- 0
         marked <- rep(0, numNodes(object))
         names(marked) <- nodes(object)
         for(i in 1:numNodes(object))
           if( marked[i]==0 ) {
                now <- now+1
                visit(i)
           }
         return(marked)
    }, where=where)

  setGeneric("edgeL", function(graph, index) standardGeneric("edgeL"),
     where=where)

  setMethod("edgeL", "graphNEL", function(graph, index) {
        if( missing(index) )
            graph@edgeL
        else
           graph@edgeL[index]}, where=where)

  setGeneric("subGraph", function(snodes, graph) standardGeneric("subGraph"),
    where=where)

  ##the map from the labels to the integers, 1:n must be used
  ## you must renumber the edges of the subGraph
  ##from 1 to length(snodes)
  ## it is important to get the subsets of the edgeList items right
  setMethod("subGraph", c("character", "graphNEL"), function(snodes, graph) {
      nn<-nodes(graph)
      numN <- length(nn)
      if( is.character(snodes) ) {
         ma <- match(snodes, nn)
         if( any(is.na(ma)) )
             stop("error the nodes in the graph do not match the subnodes")
      }
      else {
         ma <- snodes
         if( any( snodes < 0 ) || any( snodes > numN ) )
            stop("subnodes must be between 1 and the number of nodes in G")
      }
      ##subset the edgeList elements here
      ed <- edgeL(graph)[ma]
      newed <- lapply(ed, function(x) { good <- x$edges %in% ma
                 lapply(x, function(y) y[good]) })

      ##map from the old indices - 1:nn to the new 1:length(snodes)
      t1 <- rep(NA, numN)
      t1[ma] <- 1:length(snodes)
      newed <- lapply(newed,
                  function(x) {ed <- t1[x$edges] ##new names
                               x$edges <- ed
                               lapply(x, function(y) {names(y) <- ed; y})})
      ##finally return
      new("graphNEL", nodes=nn[ma], edgeL=newed) },
            where=where)


  ###some other tools
   setGeneric("intersection", function(x, y) standardGeneric("intersection"),
     where=where)

   setMethod("intersection", c("graph", "graph"), function(x,y) {
       if( edgemode(x) != edgemode(y) )
           stop("both graphs must have the same edgemode")
       xN <- nodes(x)
       yN <- nodes(y)
       bN <- intersect(xN,yN)
       lb <- length(bN)
       if(lb != length(xN) || lb != length(yN))
           stop("graphs must have the same node set")
       yOrd <- match(xN, yN)
       xE <- edges(x)
       yE <- edges(y)[yOrd]
       rval <- vector("list", length=length(xE))
       for(i in 1:length(xE) ) {
           ans <- intersect(xE[[i]], yE[[i]])
           if( length(ans) > 0 )
               rval[[i]] <- list(edges=match(ans, xN),
                                 weights=rep(1, length(ans)))
           else
               rval[[i]] <- list(edges=numeric(0), weights=numeric(0))
       }
       names(rval) <- xN
       new("graphNEL", nodes=bN, edgeL=rval, edgemode=edgemode(x))
   }, where=where)

  setGeneric("union", function(x, y) standardGeneric("union"),
             where=where)

  setMethod("union", c("graph", "graph"), function(x, y) {
      ex <- edgemode(x); ey <- edgemode(y);
      if( ex == ey )
          outmode <- ex
      else
          stop("cannot handle different edgemodes, yet")

      xN <- sort(nodes(x))
      yN <- sort(nodes(y))
      if( any(xN != yN) )
          stop("graphs must have the same nodes")
      xE <- edges(x)
      yE <- edges(y)
      rval <- vector("list", length=length(xE))
      names(rval) <- names(xE)
      for(i in names(xE) ) {
          ans <- unique(c(xE[[i]], yE[[i]]))
          if( length(ans) > 0 )
              rval[[i]] <- list(edges = match(ans, xN), weights=rep(1,
                                                        length(ans) ))
          else
              rval[[i]] <- list(edges=numeric(0), weights=numeric(0))
      }
      names(rval) <- xN
      new("graphNEL", nodes=xN, edgeL=rval, edgemode=outmode)
  }, where=where)

   setGeneric("complement", function(x) standardGeneric("complement"),
              where=where)

   setMethod("complement", c("graph"), function(x) {
       xN <- nodes(x)
       xE <- edges(x)
       rval <- vector("list", length=length(xE))
       names(rval) <- names(xE)
       for( i in names(xE) ) {
           ans <-xN[ !(xN %in% c(i, xE[[i]])) ]
           lena <- length(ans)
           if( lena > 0 )
               rval[[i]] <- list(edges=match(ans, xN),
                                 weights=rep(1, lena))
           else
               rval[[i]] <- list(edges=numeric(0), weights=numeric(0))
       }
       new("graphNEL", nodes=xN, edgeL=rval, edgemode=edgemode(x))
   }, where=where)

  ##connected components
  setGeneric("connComp", function(object) standardGeneric("connComp"))

  setMethod("connComp", "graph", function(object) {
    NL <- nodes(object)
    marked <- rep(0, length(NL))
    names(marked) <- NL
    done <- FALSE
    rval <- vector("list", 1)
    cnode <- 1
    index <- 1
    nused <- numeric(0)
    while( !done ) {
	curracc <- acc(object, cnode)
        rval[[index]] <- curracc
        nused <- c(nused, cnode)
        index <- index + 1
        if( length(curracc) > 0 )
            marked[names(curracc)] <- 1
        marked[cnode] <- 1
        cnode <- match(0, marked)
        if( is.na(cnode) )
          done <- TRUE
    }
    nmR <- NL[nused]
    nc <- length(rval)
    rL <- vector("list", length=nc)
    for(i in 1:nc) rL[[i]]<-c(nmR[[i]], names(rval[[i]]))
    return(rL)
 }, where=where)

  setGeneric("isConnected", function(object, ...)
             standardGeneric("isConnected"), where=where)
  setMethod("isConnected", "graph", function(object, ...) {
      if (length(connComp(object)) == 1)
          TRUE
      else
          FALSE
  }, where=where)

  setGeneric("numNodes", function(object) standardGeneric("numNodes"),
     where=where)

  setMethod("numNodes", "graph", function(object) length(nodes(object)),
    where=where)

  setMethod("numNodes", "graphNEL", function(object) length(object@nodes),
     where=where)

##print methods
  setMethod("show", "graphNEL",
  function(object)
   {
     numNull<-numNoEdges(object)
     numNodes<- numNodes(object)
     numEdge<-numEdges(object)
##     totalEdgesPossible<-numNodes*(numNodes-1)/2
##     nodeMostEdges<-mostEdges(object)
##     aveEdge<-aveNumEdges(object)
     cat("A graph with ", object@edgemode, " edges\n")
     cat("Number of Nodes = ",numNodes,"\n",sep="")
     cat("Number of Edges = ",numEdge,"\n",sep="")
##     cat("Number of Nodes with no edges = ",numNull,"\n",sep="")
##     cat("If the graph was completely linked, the number of edges would be ",format(totalEdgesPossible,big.mark=","),".\n",sep="")
##     cat("The node with the most edges is ",nodeMostEdges$id," with ", nodeMostEdges$maxLen, " edges.","\n",sep="")
##     cat("The average number of edges is ",format(aveEdge,digits=3),".\n",sep="")
   },
  where=where
  )
}

#.First.lib <- function(libname, pkgname, where) {
#    require(methods)
#    require(Biobase)
#    where <- match(paste("package:", pkgname, sep=""), search())
#    .initGraph(where)
#    cacheMetaData(as.environment(where))
#}













