
validGraph<-function(object, quietly=FALSE)
{
  if (class(object) == "graphNEL") {
      objEdges<-edges(object)
      objNodes<-nodes(object)
      bad <- FALSE
      if (any(is.na(objNodes))) {
          if (!quietly ) cat("NA element in nodes.\n")
          bad <- TRUE
      }
      if (any(is.na(unlist(objEdges,use.names=FALSE)))) {
          if( !quietly )
              cat ("NA element in edges.\n")
          bad <- TRUE
      }
      ##don't think we want to force this one
#      if (length(objNodes)!=length(objEdges)) {
#          if( !quietly )
#              cat("Nodes and edges must have the same length.\n")
#          bad <- TRUE
#      }
      if (!all( names(objEdges) %in% objNodes )) {
           if( !quietly )
              cat("Edges don't have the same names as the nodes.\n")
           bad <- TRUE
       }
      if (any(duplicated(objNodes))) {
          if( !quietly )
              cat("Node names may not be duplicated\n")
          bad <- TRUE
      }
      ##check for reciprocity in undirected graphs
      ##paste to->from and from->to if any are not duplicated then
      ##the edge is not reciprocal. Note we are not going to handle
      ##multiedges well.
      if( object@edgemode == "undirected") {
          eds <- lapply(object@edgeL, function(x) x$edges)
          v1 <- sapply(eds, length)
          v2 <- sum(v1)
          tM <- paste(rep(1:length(v1), v1), unlist(eds), sep=" -> " )
          tM2 <- paste(unlist(eds), rep(1:length(v1), v1), sep=" -> " )
          tM3 <- c(tM, tM2)
          vv <- duplicated(tM3)
          badn <- which(vv == FALSE)
          badn <- badn[badn>v2]
          if( length(badn)>0 ) {
              if( !quietly ) {
                  from <- badn-v2
                  cat("The graph is undirected and\n")
                  cat("the following edges are not reciprocated\n")
                  cat(tM3[from], sep="\n")
                  cat("\n")
              }
              return(FALSE)
          }
      }
  }
  return(!bad)
}

#graph<-function(newnodes,newedges)
#{
#  new("graphNEL",nodes=as(newnodes,"vector"),
#         edgeL=as(newedges,"list"))
#}

  ## we define a virtual graph class -- to hold generic functions
  setClass("graph", representation(edgemode="character",
                                   "VIRTUAL"))

  if( !exists("edgemode", mode="function") )
      setGeneric("edgemode", function(object)
                 standardGeneric("edgemode"))
  setMethod("edgemode", "graph", function(object) object@edgemode)


  setGeneric("edgemode<-", function(object, value)
             standardGeneric("edgemode<-"))

  setReplaceMethod("edgemode", c("graph", "character"),
             function(object, value) {
                 if(length(value) != 1)
                     stop("edgemode is the wrong length")
                 if( !(value %in% c("directed", "undirected")) )
                     stop(paste("supplied mode is", value,
                                "it must be either directed or undirected"))
                 object@edgemode <- value
                 object
             })

  ## a node-edge-list graph
  ##the edgeL is a list, with edges, weights etc

  setClass("graphNEL",representation(nodes="vector",edgeL="list"),
            contains="graph")

  setMethod("initialize", "graphNEL", function(.Object, nodes=character(0),
       edgeL = vector("list",length=0), edgemode) {
       if( missing(edgemode) )
           edgemode <- "undirected"
       if( !missing(edgeL) && length(edgeL) > 1 )
         if( is.character(edgeL[[1]]) )
             edgeL <- lapply(edgeL, function(x) match(x, nodes))
       if( missing(edgeL) )
           edgeL = vector("list",length=0)
       if( missing(nodes) )
           nodes = character(0)

       .Object@nodes <- nodes
       .Object@edgeL <- edgeL
       .Object@edgemode <- edgemode
       .Object})

  setGeneric("nodes", function(object) standardGeneric("nodes"))
  setMethod("nodes", "graphNEL", function(object) object@nodes)

  setGeneric("nodes<-", function(object, value)
             standardGeneric("nodes<-"))
  setReplaceMethod("nodes", c("graphNEL", "character"),
             function(object, value) {
                 if(length(value) != length(object@nodes))
                     stop("need as many names as there are nodes")
                 if(any(duplicated(value)))
                     stop("node names must be unique")
                 object@nodes <- value
                 names(object@edgeL) <- value
                 object})

  if (!exists("edges", mode="function"))
      setGeneric("edges", function(object, which)
                 standardGeneric("edges"))

  ##the graphNEL representation stores edges as indexes into the
  ##node label vector
  setMethod("edges", c("graphNEL", "missing"), function(object, which) {
      gNodes <- object@nodes
      lapply(object@edgeL, function(x) gNodes[x$edges])})

  setMethod("edges", c("graphNEL", "character"),
            function(object, which) {
                gNodes <- nodes(object)
                lapply(object@edgeL[which], function(x) gNodes[x$edges])})


  ##we are going to need some notion of degree
  if( !isGeneric("degree") )
    setGeneric("degree", function(object, Nodes) standardGeneric("degree"))

  ##handle directed graphs by a list inDegree and outDegree
  setMethod("degree", signature(object="graph", Nodes="missing"),
      function(object)  {
          ns <- nodes(object)
          nl <- edges(object)
          deg <- sapply(nl, length)
          names(deg) <- ns
          if( object@edgemode == "undirected" )
              return(deg)
          else if( object@edgemode == "directed" ) {
              b1<- unlist(nl)
              b2<- table(b1)
              inDegree <- rep(0, length(ns))
              names(inDegree) <- ns
              inDegree[names(b2)]<-b2
              return(list(inDegree=inDegree, outDegree=deg))
          }
          stop(paste("edgemode", object@edgemode, "is not valid"))
     })

  setMethod("degree", "graph",  function(object, Nodes) {
       nl <- edges(object)
       nls <- nl[Nodes]

       deg <- sapply(nls, length)
       names(deg) <- Nodes
       if( object@edgemode == "undirected" )
           return(deg)
       else if( object@edgemode == "directed" ) {
           b1 <- unlist(nl)
           b2 <- table(b1)[Nodes]
           inDegree <- rep(0, length(nls))
           names(inDegree) <- Nodes
           inDegree[names(b2)] <- b2
           return(list(inDegree=inDegree, outDegree=deg))
       }
       stop(paste("edgemode", object@edgemode, "is not valid"))
     })

  setGeneric("edgeWeights", function(object, index)
      standardGeneric("edgeWeights"))

  setMethod("edgeWeights", "graphNEL", function(object, index) {
           if( missing(index) )
           	tlist <- object@edgeL
           else
                tlist <- object@edgeL[index]
           wts <- lapply(tlist, function(x) {
               wts <- x$weights
               if(is.null(wts)) {
                   wts <- rep(1, length(x$edges))
                   names(wts) <- x$edges
               }
               ## Always make sure that the weight vector
               ## has names attached
               if (is.null(names(wts)))
                   names(wts) <- x$edges

               wts})
            wts})


##RG: some methods for adjcency and accessibility
  setGeneric("adj", function(object, index)
	standardGeneric("adj"))

  ## do we want the indices or the nodes?
  setMethod("adj", "graphNEL", function(object, index) {
        initI <- as.character(index)
        nd <- nodes(object)
        if( is.character(index) )
          index <- match(index, nd)
        if( is.na(index) || index < 0 || index > length(nd) )
          stop(paste("selected vertex", initI, "is not in the graph"))
	edges(object)[index]})

  setGeneric("acc", function(object, index)
	standardGeneric("acc"))


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
    })

  setGeneric("dfs", function(object) standardGeneric("dfs"))

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
    })

  setGeneric("edgeL", function(graph, index) standardGeneric("edgeL"))

  setMethod("edgeL", "graphNEL", function(graph, index) {
        if( missing(index) )
            graph@edgeL
        else
           graph@edgeL[index]})

  setGeneric("subGraph", function(snodes, graph) standardGeneric("subGraph"))

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
      new("graphNEL", nodes=nn[ma], edgeL=newed) })


  ###some other tools
   setGeneric("intersection", function(x, y) standardGeneric("intersection"))

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
   })

  setGeneric("join", function(x, y) standardGeneric("join"))

  setMethod("join", c("graph", "graph"), function(x, y) {
      ex <- edgemode(x); ey <- edgemode(y);
      if( ex == ey )
          outmode <- ex
      else
          stop("cannot handle different edgemodes, yet")

      nX <- nodes(x)
      numXnodes <- length(nX)
      nY <- nodes(y)
      ## Combine the two sets of nodes, removing any duplications
      newNodes <- unique(c(nX, nY))

      eLX <- edgeL(x)
      eLY <- edgeL(y)

      newEdgeL <- eLX

      ## Can't just cat the edgeL's together like this
      ## as the node #s have all changed.
      if (length(eLY) > 0) {
          eLYnames <- names(eLY)
          for (i in 1:length(eLY)) {
              newEntry <- eLY[i]
              ## !! first need to adjust the targets on the edges
              newEdges <- newEntry[[1]]$edges
              if (length(newEdges) > 0) {
                  for (j in 1:length(newEdges)) {
                      curTo <- nY[newEdges[j]]
                      newTo <- match(curTo,newNodes)
                      if (is.na(newTo))
                          stop("Error reassigning duplicated nodes")
                      newEdges[j] <- newTo
                  }
              }
              newEntry[[1]]$edges <- newEdges

              ## now need to attach it to the list.  If this
              ## is a duplicated node, combine it with the
              ## original, otherwise add it ot the list
              if (length(newEdgeL) == 0)
                  newEdgeL <- newEntry
              else if (eLYnames[i] %in% nX) {
                  entry <- which(names(newEdgeL) == eLYnames[i])
                  if (length(entry) > 1)
                      stop("Duplicated node names in original graph")
                  curEntry <- newEdgeL[[entry]]
                  curEntry$edges <- c(curEntry$edges, newEntry[[1]]$edges)
                  curEntry$weights <- c(curEntry$weights,
                                        newEntry[[1]]$weights)
                  if (!is.null(curEntry))
                      newEdgeL[[entry]] <- curEntry
              }
              else
                  newEdgeL <- c(newEdgeL,newEntry)
          }
      }

      new("graphNEL", nodes=newNodes, edgeL=newEdgeL, edgemode=ex)
  })

  setGeneric("union", function(x, y) standardGeneric("union"))

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
      names(rval) <- xN
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
  })

   setGeneric("complement", function(x) standardGeneric("complement"))

   setMethod("complement", c("graph"), function(x) {
       if( edgemode(x) != "undirected" )
           stop(paste("can't handle edgemode:", x@edgemode, "yet"))

       xN <- nodes(x)
       xE <- edges(x)
       rval <- vector("list", length=length(xE))
       names(rval) <- xN
       for( i in xN ) {
           ans <-xN[ !(xN %in% c(i, xE[[i]])) ]
           lena <- length(ans)
           if( lena > 0 )
               rval[[i]] <- list(edges=match(ans, xN),
                                 weights=rep(1, lena))
           else
               rval[[i]] <- list(edges=numeric(0), weights=numeric(0))
       }
       new("graphNEL", nodes=xN, edgeL=rval, edgemode=edgemode(x))
   })

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
 })

  setGeneric("isConnected", function(object, ...)
             standardGeneric("isConnected"))
  setMethod("isConnected", "graph", function(object, ...) {
      if (length(connComp(object)) == 1)
          TRUE
      else
          FALSE
  })

  setGeneric("numNodes", function(object) standardGeneric("numNodes"))

  setMethod("numNodes", "graph", function(object) length(nodes(object)))

  setMethod("numNodes", "graphNEL", function(object) length(object@nodes))

  setGeneric("addNode", function(node, object)
    standardGeneric("addNode"))

  setMethod("addNode", c("character", "graphNEL"),
        function(node, object) {
            gN = nodes(object)
            already <- match(node, gN)
            if( any(!is.na(already)) )
                stop(paste(node[already], "is already a node"))
            ##add them on the end so we don't renumber
            gN = c(gN, node)
            edgeL <-  object@edgeL
            nNode <- length(node)
            nEd <- vector("list", length=nNode)
            names(nEd) <- node
            for(i in 1:nNode)
                nEd[[i]] <- list(edges=numeric(0), weights=numeric(0))
            edgeL <- c(edgeL, nEd)
            new("graphNEL", gN, edgeL, edgemode(object))
        })

  setGeneric("removeNode", function(node, object)
      standardGeneric("removeNode"))

  setMethod("removeNode", c("character", "graphNEL"),
       function(node, object) {
           ##first clear the node -- does the checking too
           nG <- clearNode(node, object)
           nN <- nodes(nG)
           wh <- match(node, nN)
           gN <- nN[-wh]
           nE <- nG@edgeL[-wh]
           ##now renumber the nodes as stored in the edgelist
           nE2 <- lapply(nE, function(el) {
               oldN <- nN[el$edges]
               el$edges <- match(oldN, gN)
               el
           })
           new("graphNEL", gN, nE2, edgemode(object))
       })

  setGeneric("clearNode", function(node, object)
  standardGeneric("clearNode"))

  setMethod("clearNode", c("character", "graphNEL"),
       function(node, object) {
           gN <- nodes(object)
           whN <- match(node, gN)
           if(any(is.na(whN)) )
               stop(paste(whN[is.na(whN)], "is not a node in the graph"))
            gE <- .dropEdges(whN, object@edgeL)
           gE[[whN]] = list(edges=numeric(0), weights=numeric(0))
           new("graphNEL", gN, gE, edgemode(object))
       })



  ##FIXME: vectorize
  setGeneric("removeEdge", function(from, to, graph)
  standardGeneric("removeEdge"))

  setMethod("removeEdge", c("character", "character", "graphNEL"),
            function(from, to, graph) {
                gN <- nodes(graph)
                wh <- match(c(from, to), gN)
                if( any(is.na(wh)) )
                    stop(paste(wh[is.na[wh]], "is not a node"))
                nE <- edges(graph, from)
                whD <- match(to, nE[[1]])
                if( is.na(whD) )
                    stop(paste("no edge from", from, "to", to))
                nEd <- graph@edgeL
                nEd[[from]]$edges <- nEd[[from]]$edges[-whD]
                nEd[[from]]$weights <- nEd[[from]]$weights[-whD]
                ##now if undirected we need to remove the other one
                if( edgemode(graph) == "undirected" ) {
                    nE <- edges(graph, to)
                    whD <- match(from, nE[[1]])
                    if( is.na(whD) )
                        stop("problem with graph edges")
                    nEd[[to]]$edges <- nEd[[to]]$edges[-whD]
                    nEd[[to]]$weights <- nEd[[to]]$weights[-whD]
                }
                new("graphNEL", gN, nEd, edgemode(graph))
            })


    setGeneric("addEdge", function(from, to, graph, weights)
        standardGeneric("addEdge"))


     ##FIXME: do  we care if the edge already exists?
     setMethod("addEdge", c("character", "character", "graphNEL", "numeric"),
         function(from, to, graph, weights) {
             gN <- nodes(graph)
             whF <- match(from, gN)
             if( any(is.na(whF)) )
                 stop(paste(from[is.na(whF)], "is not a node"))
             whT <- match(to, gN)
             if( any(is.na(whT)) )
                 stop(paste(to[is.na(whT)], "is not a node"))
             ##roll out the shorter one
             lenT <- length(to)
             lenF <- length(from)
             if( lenT > lenF ) {
                 from <- rep(from, lenT)
                 whF <- rep(whF, lenT)
             }
             if( lenF > lenT ) {
                 whT <- rep(whT, lenF)
                 to <- rep(to, lenF)
             }
             ##now the same
             lenN <- max(lenT, lenF)
             weights <- rep(weights, lenN)
             eL <- graph@edgeL
             for(i in 1:lenN) {
                 old <- eL[[from[i]]]
                 old$edges <- c(old$edges, whT[i])
                 old$weights <- c(old$weights, weights[i])
                 eL[[from[i]]] <- old
             }
             ##if undirected, then we need to go in both directions
             if( edgemode(graph) == "undirected")
                 for(i in 1:lenN) {
                     old <- eL[[to[i]]]
                     old$edges <- c(old$edges, whF[i])
                     old$weights <- c(old$weights, weights[i])
                     eL[[to[i]]] <- old
                 }

             new("graphNEL", gN, eL, edgemode(graph))
         })

   setGeneric("combineNodes", function(nodes, graph, newName)
         standardGeneric("combineNodes"))

  setMethod("combineNodes", c("character", "graphNEL", "character"),
        function(nodes, graph, newName) {
            if( length(newName) > 1 )
                stop("must have a single name")
            gN <- nodes(graph)
            whN <- match(nodes, gN)
            if( any(is.na(whN)) )
                stop(paste(from[is.na(whN)], "is not a node"))
            eL <- graph@edgeL
            outE <- eL[nodes]
            if( length(nodes) == 1 ) {
                warning("nothing to collapse")
                return(graph)
            }
            ##if undirected then we know everything
            if( edgemode(graph) == "directed" )
                inE <- inEdges(nodes, graph)
            else
                inE <- NULL
            g2 <- removeNode(nodes, graph)
            g2 <- addNode(newName, g2)
            nC <- length(nodes)
            oE <- NULL; oW <- NULL;
            ##seems very inefficient
            for(i in 1:nC) {
                oE <- c(oE, outE[[nodes[i]]]$edges)
                oW <- c(oW, outE[[nodes[i]]]$weights)
            }
            oE <- gN[oE]
            oEd <- match(nodes, oE)
            oEd <- oEd[!is.na(oEd)]
            if( length(oEd) > 0 ) {
                oE <- oE[-oEd]
                oW <- oW[-oEd]
            }
            g2 <- addEdge(newName, oE, g2, oW)
            ##if directed we need to fix up the in edges
            if( !is.null(inE) ) {
                nC <- length(inE)
                oE <- NULL; oW <- NULL
                nmE <- names(inE)
                for(i in 1:nC) {
                    oE <- c(oE, inE[[i]])
                    for( j in inE[[i]] )
                        oW <- c(oW, .edgeWeight(j, nmE[i], graph))
                }
                g2 <- addEdge(oE, newName, g2, oW)
            }
            g2})


    ##inEdges returns a list of all nodes with edges
    ##to the nodes in "node"
    setGeneric("inEdges", function(node, object)
       standardGeneric("inEdges"),)

    setMethod("inEdges", c("missing", "graphNEL"),
              function(node, object)
                  inEdges(nodes(object), object))

    setMethod("inEdges", c("character", "graphNEL"),
         function(node, object) {
             gN <- nodes(object)
             whN <- match(node, gN)
             if( any(is.na(whN)) )
                 stop(paste(from[is.na[whN]], "is not a node"))
             nN <- length(node)
             rval <- vector("list", length=nN)
             names(rval) <- node
             eL <- object@edgeL
             for(i in 1:nN) {
                 whOnes <- sapply(eL,
                   function(x) {
                       if( whN[i] %in% x$edges)
                           return(TRUE)
                       FALSE})

                 rval[[i]] <- gN[whOnes]
             }
             rval})


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
   })

   .dropEdges <- function(x,z) {
       ans =  lapply(z,function(ww) {
           bad <- match(x, ww$edges)
           bad <- bad[!is.na(bad)]
           if(length(bad) > 0 )
               ans = list(edges= ww$edges[-bad],
                  weights = ww$weights[-bad])
           else
               ans = ww
       })
       ans}


    .edgeWeight <- function(from, to, graph)
    {
        gN <- nodes(graph)
        wF <- match(from, gN)
        if( is.na(wF) )
            stop(paste(from, "is not a node"))
        wT <- match(to, gN)
        if( is.na(wT) )
            stop(paste(to, "is not a node"))
        eL <- graph@edgeL[[from]]
        mt <- match(wT, eL$edges)
        if(is.na(mt) )
            stop(paste("no edge from", from, "to", to))
        eL$weights[mt]
    }









