setMethod("isDirected", signature("graph"),
          function(object) {
              if (object@edgemode == "directed")
                TRUE
              else
                FALSE
          })


setMethod("edgemode", "graph", function(object) object@edgemode)


setMethod("numEdges", signature(object="graph"),
          function(object) {
              gEdges <- edges(object)
              if (length(gEdges) == 0)
                return(length(gEdges))
              numEdges <- length(unlist(gEdges, use.names=FALSE))
              if (!isDirected(object)) {
                  numSelfLoops <- sum(mapply(function(e, n) sum(n == e),
                                             gEdges, names(gEdges)))
                  numEdges <- numSelfLoops + (numEdges - numSelfLoops) / 2
              }
              numEdges
          })


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

setMethod("isAdjacent",signature(object="graph", from="character",
                                 to="character"),
          function(object, from, to) {
              if (length(from) != length(to))
                stop("from and to must have the same length")
              fromEdges <- edges(object)[from]
              ans <- vector(mode="logical", length=length(to))
              for (i in 1:length(to)) {
                  ans[i] <- to[i] %in% fromEdges[[i]]
              }
              ans
          })


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

  setMethod("acc", c("graph", "character"), function(object, index) {
       nN <- numNodes(object)
       nNames<- nodes(object)
       nIndex <- length(index)
       whN <- match(index, nNames)
       if( any(is.na(whN)) )
           stop("unmatched node provided")

       rval <- vector("list", length=nIndex)
       names(rval) <- nNames[whN]
       for( i in 1:nIndex) {
           marked<-rep(0, nN)
           distv <- rep(0, nN)
           names(distv) <- nNames
           distx <- 1
           names(marked) <- nNames
           nmkd <- 0
           marked[index[i]] <- 1
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
           marked[index[i]] <- 0 ##not the node itself
           rval[[i]] <- distv[marked==2]
       }
       return(rval)
    })


setMethod("edgeWeights", signature(object="graph", index="character"),
          function(object, index) {
              ew <- tryCatch(edgeData(object, from=index, attr="weight"),
                             error=function(e) {
                                 edgeDataDefaults(object, "weight") <- 1
                                 edgeData(object, from=index, attr="weight")
                             })
              gEdges <- edges(object)[index]
              edgeCounts <- sapply(gEdges, length)
              nn <- rep(index, edgeCounts)
              names(ew) <- unlist(gEdges)
              ans = split(unlist(ew), nn)
              ans <- c(ans, lapply(gEdges[edgeCounts == 0], as.numeric))
              ans
          })


setMethod("edgeWeights", signature(object="graph", index="numeric"),
          function(object, index) {
              index <- nodes(object)[index]
              edgeWeights(object, index)
          })


setMethod("edgeWeights", signature(object="graph", index="missing"),
          function(object, index) {
              index <- nodes(object)
              edgeWeights(object, index)
          })
##               ew <- tryCatch(edgeData(object, attr="weight"),
##                              error=function(e) {
##                                  edgeDataDefaults(object, "weight") <- 1
##                                  edgeData(object, attr="weight")
##                              })
##               edgeCounts <- sapply(edges(object), length)
##               nn <- rep(nodes(object), edgeCounts)
##               names(ew) <- unlist(edges(object))
##               ans = split(unlist(ew), nn)
##               ans <- c(ans, lapply(edges(g)[edgeCounts == 0], as.numeric))
##               ans
##           })


setMethod("DFS", c("graph", "character", "ANY"), function(object, node,
    checkConn=FALSE) {
    nNames <- nodes(object)
    marked <- rep(NA, length(nNames))
    names(marked) <- nNames
    m1 <- match(node, nNames)
    if( is.na(m1) )
        stop(paste("node:", node, "is not in the graph"))

    ##this could be expensive
    if (checkConn) {
        c1 <- connComp(object)
        if(length(c1) != 1)
            stop("graph is not connected")
    }
    marked[m1] <- 0
    ##repeat until all are marked - marked has no NA's
    counter <- 1
    while( any(is.na(marked)) ) {
        fE <- boundary(nNames[!is.na(marked)], object)
        fE <- fE[sapply(fE, length) > 0]
        wh <- marked[names(fE)]
        v1 <- sort(wh)
        newN <- fE[[names(v1)[v1==max(v1)]]]
        marked[newN[1]] <- counter
        counter <- counter+1
    }
    return(marked)
})




## a faster implementation of "intersection"
## not yet fully tested...
## wh 15.1.2005

setMethod("intersection3", c("graph", "graph"), function(x,y) {
  if( edgemode(x) != edgemode(y) )
    stop("both graphs must have the same edgemode")
  xN <- nodes(x)
  yN <- nodes(y)

  bN <- intersect(xN, yN)
  nn <- length(bN)

  if(nn==0)
    return(new("graphNEL", nodes=character(0),
               edgeL=vector("list", 0), edgemode=edgemode(x)))

  if(!all(xN %in% bN))
    x  <- subGraph(bN, x)
  if(!all(yN %in% bN))
    y  <- subGraph(bN, y)
  ## stopifnot(identical(nodes(x), bN), identical(nodes(y), bN))

  xE <- edgeMatrix(x)
  yE <- edgeMatrix(y)
  ## stopifnot(nrow(xE)==2, ncol(xE)==numEdges(x),
  ##           nrow(yE)==2, ncol(yE)==numEdges(y))

  ft2i <- function(ft) { ft[1,]-1 + (ft[2,]-1)*nn }
  i2ft <- function(i)  {  1 + rbind(i%%nn, i%/%nn) }
  ## stopifnot(all(xE == i2ft(ft2i(xE))),
  ##           all(yE == i2ft(ft2i(yE))))

  ## intersect
  bft <- i2ft(intersect(ft2i(xE),  ft2i(yE)))

  ## replace integers by node names (characters)
  bft <- rbind(bN[bft[1,]],
               bN[bft[2,]])

  ftM2graphNEL(bft, V=bN, edgemode=edgemode(x))
} )


### yet another implementation of "intersection", in C

setMethod("intersection2", c("graph", "graph"), function(x,y) {
        if (edgemode(x) != edgemode(y) )
           stop("both graphs must have the same edgemode")

        if (edgemode(x) == "undirected")
           edgeM <- 0
        else
           edgeM <- 1

        .Call("graphIntersection", nodes(x), nodes(y),
              edges(x), edges(y), edgeM, PACKAGE="graph")

})

   setMethod("intersection", c("graph", "graph"), function(x,y) {
       if( edgemode(x) != edgemode(y) )
           stop("both graphs must have the same edgemode")
       xN <- nodes(x)
       yN <- nodes(y)
       bN <- intersect(xN, yN)
       if( length(bN) == 0 )
           return(new("graphNEL", nodes=character(0),
                      edgeL=vector("list", 0), edgemode=edgemode(x)))
       ##lb <- length(bN)
       ##if(lb != length(xN) || lb != length(yN))
       ##    stop("graphs must have the same node set")
       xE <- edges(x, bN)
       xE = lapply(xE, function(x) {
           x[x %in% bN]})
       yE <- edges(y, bN)
       yE = lapply(yE, function(x) {
           x[x %in% bN]})
       rval <- vector("list", length=length(xE))
       for(i in 1:length(xE) ) {
           ans <- intersect(xE[[i]], yE[[i]])
           rval[[i]] <- list(edges=match(ans, bN),
                                 weights=rep(1, length(ans)))
       }
       names(rval) <- bN
       new("graphNEL", nodes=bN, edgeL=rval, edgemode=edgemode(x))
   })

setMethod("join", c("graph", "graph"), function(x, y) {
    ex <- edgemode(x); ey <- edgemode(y)
    if(ex == ey)
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
                ##should be user adjustable -
                ##for now just remove extras
                dups = duplicated(curEntry$edges)
                if(any(dups) ) {
                    curEntry$edges = curEntry$edges[!dups]
                    curEntry$weights = curEntry$weights[!dups]
                }
                if (!is.null(curEntry))
                    newEdgeL[[entry]] <- curEntry
            }
            else
                newEdgeL <- c(newEdgeL,newEntry)
        }
    }

    ## Some graphs have edgeL's that are missing the original
    ## node from the edgeL.  When we collated the edgeL above,
    ## those nodes will be missing - so need to make sure that
    ## all nodes are present, as the new("graphNEL") call below
    ## will check to make sure that length(nodes) == length(edgeL)
    for (missNode in newNodes[! newNodes %in% names(newEdgeL)]) {
        newEdgeL[[length(newEdgeL) + 1]] <- list(edges=numeric(),
                                                 weights=numeric())
        names(newEdgeL)[length(newEdgeL)] <- missNode
    }

    new("graphNEL", nodes=newNodes, edgeL=newEdgeL, edgemode=ex)
})



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
        rval[[i]] <-
            if( length(ans) > 0 )
                list(edges = match(ans, xN),
                     weights= rep(1, length(ans)))
            else
                list(edges=numeric(0), weights=numeric(0))
    }
    names(rval) <- xN
    new("graphNEL", nodes=xN, edgeL=rval, edgemode=outmode)
})



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


setMethod("connComp", "graph", function(object) {
    ##if directed we do weak connectivity
    ##by transforming to the underlying undirected graph
    if( edgemode(object) == "directed")
        object = ugraph(object)
    NL <- nodes(object)
    marked <- rep(0, length(NL))
    names(marked) <- NL
    done <- FALSE
    rval <- vector("list", 1)
    cnode <- 1
    index <- 1
    nused <- numeric(0)
    while( !done ) {
	curracc <- acc(object, NL[cnode])[[1]]
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

setMethod("isConnected", "graph",
          function(object, ...) (length(connComp(object)) == 1))

setMethod("numNodes", "graph", function(object) length(nodes(object)))


setMethod("show", signature("graph"),
          function(object) {
              numNodes<- numNodes(object)
              numEdge<-numEdges(object)
              cat("A", class(object), "graph with", object@edgemode, "edges\n")
              cat("Number of Nodes =", numNodes, "\n")
              cat("Number of Edges =", numEdge, "\n")
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


##take a sparse matrix - csr and put it into a graph
sparseM2Graph <- function(sM, nodeNames) {
    require("SparseM") || stop("need SparseM for this operation")
    nN <- dim(sM)[1]
    if( nN != dim(sM)[2] )
        stop("only square matrices can be transformed, now")
    if( length(nodeNames) != nN )
        stop("wrong number of node names supplied")
    if( !is.character(nodeNames) )
        stop("wrong type of node names supplied")
    dd <- diff(sM@ia)
    e1 <- rep(1:nN, dd)
    eL <- split(sM@ja, e1)
    eW <- split(sM@ra, e1)

    edL <- vector("list", length=nN)
    names(edL) <- 1:nN
    for(i in as.character(1:nN) ){
        ##need this because otherwise partial matching is done
        if( i %in% names(eL) )
            edL[[i]] <- list(edges=eL[[i]], weights=eW[[i]])
    }
    names(edL) <- nodeNames
    new("graphNEL", nodes=nodeNames, edgeL=edL)
}

##translate a graph to a SparseMatrix:
##ra - the values; these will be 1's for now
##ja - the column indices
##ia the row offsets (
graph2SparseM <- function(g, useweights=FALSE) {
    require("SparseM") || stop("need SparseM for this operation")
    if (! is(g, "graphNEL"))
       stop("coercion only works for graphNEL class")
    nr = nc = numNodes(g)
    e1 = g@edgeL
    e2 = lapply(e1, function(x) x$edges)

    eL = listLen(e2)
    if( useweights )
        ra = unlist(lapply(e1, function(x) x$weights))
    else
        ra = rep(1, sum(eL))
    ja = as.integer(unlist(e2))
    ia = as.integer(cumsum(c(1, eL)))
    new("matrix.csr", ra=ra, ja=ja, ia=ia, dimension=c(nr, nc))
}

##--------------------------
## edge names
##--------------------------

setMethod("edgeNames",
  signature="graph",
  definition=function(object, recipEdges=c("combined", "distinct")) {
    recipEdges <- match.arg(recipEdges)

#     to <- edges(object)
#     from <- names(to)
#     edgeNames <- as.vector(unlist(mapply(function(x,y) {
#         if (length(x) > 0)
#             paste(y,x,sep="~")
#         else
#             NULL}, to, from)))
#     if (recipEdges == "combined") {
#         revNames <-  unlist(mapply(function(x,y) {
#             if (length(x) > 0)
#                 paste(x,y,sep="~")
#             else
#                 NULL}, to, from))
#         handled <- character()
#         remove <- numeric()
#         for (i in 1:length(edgeNames)) {
#             if (! revNames[i] %in% handled)
#                 handled <- c(handled, edgeNames[i])
#             else
#                 remove <- c(remove, i)
#         }
#         if (length(remove) > 0)
#             edgeNames <- edgeNames[-remove]
#     }
#     edgeNames

    ## convert names to integers ("standard node labeling")
    to   <- lapply(edges(object), match, nodes(object))
    from <- match(names(to), nodes(object))

    if(any(is.na(unlist(to)))||any(is.na(from)))
      stop("Edge names do not match node names.")

    ## from-to matrix
    ft <- matrix(c(rep(from, listLen(to)), to=unlist(to)), ncol=2)

    if (recipEdges == "combined") {
      ## revert those edges for which 'from' > 'to'
      revert <- ft[, 1] > ft[, 2]
      ft2 <- ft
      ft2[revert,] <- ft2[revert, c(2, 1)]
      ft <- ft[!duplicated.array(ft2, MARGIN=1),, drop=FALSE]
    }

    return(paste(nodes(object)[ft[, 1]], nodes(object)[ft[, 2]], sep="~"))
  },
  valueClass="character")

##--------------------------
## clustering coefficient
##--------------------------

setMethod("clusteringCoefficient",
  signature=signature(object="graph"),
  definition=function(object, selfLoops=FALSE) {
  if(edgemode(object)!="undirected")
    return(NULL)

  ## Convert names to integers ("standard node labeling")
  ## This is here for efficiency - the matching code in the for-loop
  ## below would also work for the characters (names).
  to   <- lapply(edges(object), match, nodes(object))
  from <- match(names(to), nodes(object))

  if(any(is.na(unlist(to)))||any(is.na(from)))
    stop("Edge names do not match node names.")

  if(!selfLoops) {
    ufrom <- rep(from, listLen(to))
    uto   <- unlist(to)
    if(any(ufrom==uto))
      stop("Graph must not contain self-loops.")
    totEdges <- function(i) i*(i-1)
  } else {
    totEdges <- function(i) i*i
  }

  clustCoef <- rep(as.numeric(NA), numNodes(object))
  names(clustCoef) <- nodes(object)
  for (i in which(listLen(to)>0)) {
    ## to[[i]] are all the nodes reached from i.
    ## to[ to[[i]] ] are all second-degree neihbours
    nb <- sapply(to[ to[[i]] ], function(x) sum(!is.na(match(x, to[[i]]))))
    clustCoef[from[i]] <- sum(nb) / totEdges(length(nb))
  }
  return(clustCoef)
  },
  valueClass="numeric")


## ---------------------------------------------------------------------
## node data access
## ---------------------------------------------------------------------
setMethod("nodeDataDefaults", signature(self="graph", attr="missing"),
          function(self, attr) attrDefaults(self@nodeData))


setMethod("nodeDataDefaults", signature(self="graph", attr="character"),
          function(self, attr) attrDefaults(self@nodeData, attr))


setReplaceMethod("nodeDataDefaults", signature(self="graph", attr="missing",
                                               value="list"),
                 function(self, attr, value) {
                     attrDefaults(self@nodeData) <- value
                     self
                 })


setReplaceMethod("nodeDataDefaults", signature(self="graph", attr="character",
                                               value="ANY"),
                 function(self, attr, value) {
                     attrDefaults(self@nodeData, attr) <- value
                     self
                 })


.verifyNodes <- function(n, nodes) {
    unknownNodes <- n[! n %in% nodes]
    if (length(unknownNodes) > 0)
      stop("Unknown nodes: ",
           paste(unknownNodes, collapse=", "))
    TRUE
}


setMethod("nodeData",
          signature(self="graph", n="character", attr="character"),
          function(self, n, attr) {
              graph:::.verifyNodes(n, nodes(self))
              attrDataItem(self@nodeData, x=n, attr=attr)
          })


setReplaceMethod("nodeData",
                 signature(self="graph", n="character", attr="character",
                           value="ANY"),
                 function(self, n, attr, value) {
                     graph:::.verifyNodes(n, nodes(self))
                     attrDataItem(self@nodeData, x=n, attr=attr) <- value
                     self
          })


setMethod("nodeData",
          signature(self="graph", n="character", attr="missing"),
          function(self, n, attr) {
              graph:::.verifyNodes(n, nodes(self))
              attrDataItem(self@nodeData, x=n)
          })


setMethod("nodeData",
          signature(self="graph", n="missing", attr="character"),
          function(self, n, attr) {
              attrDataItem(self@nodeData, x=nodes(self), attr=attr)
          })


setReplaceMethod("nodeData",
          signature(self="graph", n="missing", attr="character", value="ANY"),
          function(self, n, attr, value) {
              attrDataItem(self@nodeData, x=nodes(self), attr=attr) <- value
              self
          })


setMethod("nodeData",
          signature(self="graph", n="missing", attr="missing"),
          function(self, n, attr) {
              attrDataItem(self@nodeData, x=nodes(self))
          })
## ---------------------------------------------------------------------


## ---------------------------------------------------------------------
## edge data access
## ---------------------------------------------------------------------
setMethod("edgeDataDefaults", signature(self="graph", attr="missing"),
          function(self, attr) attrDefaults(self@edgeData))


setMethod("edgeDataDefaults", signature(self="graph", attr="character"),
          function(self, attr) attrDefaults(self@edgeData, attr))


setReplaceMethod("edgeDataDefaults", signature(self="graph", attr="missing",
                                               value="list"),
                 function(self, attr, value) {
                     attrDefaults(self@edgeData) <- value
                     self
                 })


setReplaceMethod("edgeDataDefaults", signature(self="graph", attr="missing",
                                               value="list"),
                 function(self, attr, value) {
                     attrDefaults(self@edgeData) <- value
                     self
                 })


setReplaceMethod("edgeDataDefaults", signature(self="graph", attr="character",
                                               value="ANY"),
                 function(self, attr, value) {
                     attrDefaults(self@edgeData, attr) <- value
                     self
                 })


.normalizeEdges <- function(from, to) {
    lenFr <- length(from)
    lenTo <- length(to)
    if (lenFr > lenTo) {
        if (lenTo != 1)
          stop("'to' must be length 1 or ", lenFr, " for this call.")
        to <- rep(to, lenFr)
    } else if (lenFr < lenTo) {
        if (lenFr != 1)
          stop("'from' must be length 1 or ", lenTo, " for this call.")
        from <- rep(from, lenTo)
    }
    list(from=from, to=to)
}


.verifyEdges <- function(graph, from, to) {
    stopifnot(length(from) == length(to))
    adjList <- isAdjacent(graph, from, to)
    if (any(!adjList)) {
        badFr <- from[!adjList]
        badTo <- to[!adjList]
        res <- paste(badFr, badTo, sep="|", collapse=", ")
        stop("Edges not found: ", res)
    }
    TRUE
}


.makeEdgeKeys <- function(from, to) {
    EDGE_KEY_SEP <- "|"
    stopifnot(length(from) == length(to))
    paste(from, to, sep=EDGE_KEY_SEP)
}


.getEdgeKeys <- function(graph, from, to) {
    eSpec <- graph:::.normalizeEdges(from, to)
    from <- eSpec$from
    to <- eSpec$to
    graph:::.verifyEdges(graph, from, to)
    edgeKeys <- graph:::.makeEdgeKeys(from, to)
    edgeKeys
}


setMethod("edgeData", signature(self="graph", from="character", to="character",
                                attr="character"),
          function(self, from, to, attr) {
              edgeKeys <- graph:::.getEdgeKeys(self, from, to)
              attrDataItem(self@edgeData, x=edgeKeys, attr=attr)
          })


setMethod("edgeData", signature(self="graph", from="character", to="missing",
                                attr="character"),
          function(self, from, to, attr) {
              graph:::.verifyNodes(from, nodes(self))
              gEdges <- edges(self)[from]
              lens <- sapply(gEdges, length)
              if (any(lens == 0))
                warning("No edges from nodes: ",
                        paste(from[lens == 0], collapse=", "))
              fEdges <- rep(from, lens)
              tEdges <- unlist(edges(self)[from])
              edgeKeys <- graph:::.getEdgeKeys(self, fEdges, tEdges)
              attrDataItem(self@edgeData, x=edgeKeys, attr=attr)
          })


setMethod("edgeData", signature(self="graph", from="missing", to="character",
                                attr="character"),
          function(self, from, to, attr) {
              from <- unlist(edges(self)[to])
              edgeKeys <- graph:::.getEdgeKeys(self, from, to)
              attrDataItem(self@edgeData, x=edgeKeys, attr=attr)
          })


setReplaceMethod("edgeData",
                 signature(self="graph", from="character", to="character",
                           attr="character", value="ANY"),
                 function(self, from, to, attr, value) {
                     edgeKeys <- graph:::.getEdgeKeys(self, from, to)
                     attrDataItem(self@edgeData, x=edgeKeys, attr=attr) <- value
                     self
                 })


setReplaceMethod("edgeData",
                 signature(self="graph", from="character", to="missing",
                           attr="character", value="ANY"),
                 function(self, from, to, attr, value) {
                     graph:::.verifyNodes(from, nodes(self))
                     gEdges <- edges(self)[from]
                     lens <- sapply(gEdges, length)
                     if (any(lens == 0))
                       warning("No edges from nodes: ",
                               paste(from[lens == 0], collapse=", "))
                     fEdges <- rep(from, lens)
                     tEdges <- unlist(edges(self)[from])
                     edgeKeys <- graph:::.getEdgeKeys(self, fEdges, tEdges)
                     attrDataItem(self@edgeData, x=edgeKeys, attr=attr) <- value
                     self
                 })


setReplaceMethod("edgeData",
                 signature(self="graph", from="missing", to="character",
                           attr="character", value="ANY"),
                 function(self, from, to, attr, value) {
                     from <- unlist(edges(self)[to])
                     edgeKeys <- graph:::.getEdgeKeys(self, from, to)
                     attrDataItem(self@edgeData, x=edgeKeys, attr=attr) <- value
                     self
                 })


.getAllEdges <- function(graph) {
    e1 <- edges(graph)
    n1 <- nodes(graph)
    n1 <- rep(n1, sapply(e1, length))
    list(from=n1, to=unlist(e1))
}


setMethod("edgeData", signature(self="graph", from="missing", to="missing",
                                attr="character"),
          function(self, from, to, attr) {
              eSpec <- graph:::.getAllEdges(self)
              from <- eSpec$from
              to <- eSpec$to
              edgeKeys <- graph:::.getEdgeKeys(self, from, to)
              attrDataItem(self@edgeData, x=edgeKeys, attr=attr)
          })


setMethod("edgeData", signature(self="graph", from="character", to="character",
                                attr="missing"),
          function(self, from, to, attr) {
              edgeKeys <- graph:::.getEdgeKeys(self, from, to)
              attrDataItem(self@edgeData, x=edgeKeys)
          })


setMethod("edgeData", signature(self="graph", from="missing", to="missing",
                                attr="missing"),
          function(self, from, to, attr) {
              eSpec <- graph:::.getAllEdges(self)
              from <- eSpec$from
              to <- eSpec$to
              edgeKeys <- graph:::.getEdgeKeys(self, from, to)
              attrDataItem(self@edgeData, x=edgeKeys)
          })
## ---------------------------------------------------------------------
