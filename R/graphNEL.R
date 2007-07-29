validGraph<-function(object, quietly=FALSE) {
    ## FIXME: we are doing if/else branching here on
    ## specific subclasses of graph.  We should make this a generic
    ## so we can organize checking.  Also, graphAM is not 
    ## currently being checked in any way.
    bad = FALSE
    if (is(object, "graphNEL")) {
        objEdges<-edges(object)
        objNodes<-nodes(object)
        if (any(is.na(objNodes))) {
            if (!quietly ) cat("NA element in nodes.\n")
            bad <- TRUE
        }
        if(length(objEdges)>0)
          if(any(is.na(unlist(objEdges,use.names=FALSE)))) {
              if(!quietly)
                cat("NA element in edges.\n")
              bad <- TRUE
          }

        ##don't think we want to force this one
        ##      if (length(objNodes)!=length(objEdges)) {
        ##          if( !quietly )
        ##              cat("Nodes and edges must have the same length.\n")
        ##          bad <- TRUE
        ##      }
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
        if(object@edgemode == "undirected" && length(objEdges)>0 ) {
            fr <- rep(names(objEdges), sapply(objEdges, length))
            to <- unlist(objEdges)
            frto <- paste(fr, to, sep=EDGE_KEY_SEP)
            tofr <- paste(to, fr, sep=EDGE_KEY_SEP)
            badEdges <- setdiff(tofr, frto)
            if (length(badEdges) > 0) {
                if (!quietly) {
                    cat("The graph is undirected and the following edges",
                        "are not reciprocated:\n",
                        paste(badEdges, collapse=", "), "\n\n")
                }
                bad <- TRUE
            }
        }
    }
    else if( is(object, "distGraph") ) {
        if( is(object@Dist, "dist") )
          return(TRUE)
        else
          return(FALSE)
    }
    return(!bad)
}

setMethod("initialize", "graphNEL",
     function(.Object, ...) {
        .Object <- callNextMethod()
        validObject(.Object)
        return(.Object)
     })


graphNEL_init_edges_nested <- function(nodes, edgeL) {
    if(length(nodes) != length(edgeL) )
      stop("nodes and edges must align")
    nameE <- names(edgeL)
    if( !is.null(nameE) && !all( nameE %in% nodes) )
      stop("names of nodes and edges must agree")
    if( !is.null(nameE) )
      edgeL <- edgeL[nodes]
    edgeL <- lapply(edgeL, function(x) {
        if (is.character(x$edges))
          x$edges <- match(x$edges, nodes)
        if (is.null(x) || is.null(x$edges))
          x <- list(edges=numeric(0))
        x
    })
    edgeL
}


graphNEL_init_edgeL_weights <- function(gnel) {
    defaultWeight <- 1
    edgeDataDefaults(gnel, attr="weight") <- defaultWeight
    edgeL <- gnel@edgeL
    wts <- unlist(lapply(edgeL, function(x) {
        w <- x$weights
        if (is.null(w) || length(w) == 0)
          return(rep(defaultWeight, length(x$edges)))
        w
    }))
    if (!is.numeric(wts))
      stop("weights in edgeL must be numeric")
    
    eSpec <- graph:::.getAllEdges(gnel)
    from <- eSpec$from
    to <- eSpec$to
    edgeData(gnel, from=from, to=to, attr="weight") <- wts
    ## remove weights, since now stored in the edgeData
    edgeL <- lapply(edgeL, function(x) x["edges"])
    gnel@edgeL <- edgeL
    gnel
}


graphNEL_init_edges <- function(nodes, edges) {
    nameE <- names(edges)
    if (is.null(nameE) || !all(nameE %in% nodes))
      stop("invalid arg: edges must have names corresponding to nodes")
    edgeL <- lapply(edges, function(x) {
        if (is.list(x))
          stop("invalid arg ", sQuote("edgeL"), "\n",
               "expecting a list of character or list of lists")
        list(edges=match(x, nodes))
    })
    edgeL
}


setMethod("initialize", "graphNEL",
          ## FIXME: what about edge weights?
          function(.Object, nodes=character(0), edgeL, edgemode) {
              if (length(nodes))
                checkValidNodeName(nodes)
              if( missing(edgemode) )
                edgemode <- "undirected"
              doWeights <- FALSE
              if (missing(edgeL) || (!is.null(edgeL) && length(edgeL) == 0)) {
                  edgeL <- vector(mode="list", length=length(nodes))
                  names(edgeL) <- nodes
              } else {
                  ## which list structure was used?
                  edgeParser <- graphNEL_init_edges
                  firstVal <- edgeL[[1]]
                  if (is.null(firstVal))
                    stop("invalid arg ", sQuote("edgeL"), "\n", 
                         "elements must be character or list, got NULL")
                  if (length(edgeL) > 0 && is.list(edgeL[[1]])) {
                      edgeParser <- graphNEL_init_edges_nested
                      doWeights <- TRUE
                  }
                  edgeL <- edgeParser(nodes, edgeL)
              }
              .Object@nodes <- nodes
              .Object@edgeL <- edgeL
              .Object@edgemode <- edgemode
              validObject(.Object)
              if (doWeights)
                .Object <- graphNEL_init_edgeL_weights(.Object)
              return(.Object)
          })


##the graphNEL representation stores edges as indexes into the
##node label vector
setMethod("edges", c("graphNEL", "missing"), function(object, which) {
    gNodes <- object@nodes
    lapply(object@edgeL, function(x) gNodes[x$edges])})


setMethod("edges", c("graphNEL", "character"),
          function(object, which) {
              gNodes <- nodes(object)
              lapply(object@edgeL[which], function(x) gNodes[x$edges])})


setMethod("adj", "graphNEL", function(object, index) {
    initI <- as.character(index)
    nd <- nodes(object)
    if( is.character(index) )
      index <- match(index, nd)
    if( is.na(index) || index < 0 || index > length(nd) )
      stop(paste("selected vertex", initI, "is not in the graph"))
    edges(object)[index]})


setMethod("edgeL", "graphNEL", function(graph, index) {
    if( missing(index) )
      graph@edgeL
    else
      graph@edgeL[index]})


setMethod("subGraph", signature(snodes="character", graph="graphNEL"),
          function(snodes, graph) {
              origNodes <- nodes(graph)
              snodesIdx <- match(snodes, origNodes)
              if (any(is.na(snodesIdx))) {
                  bad <- snodes[which(is.na(snodesIdx))]
                  stop("invalid arg: snodes contains nodes not in the ",
                       "graph:\n", paste(bad, collapse=", "))
              }
              killedNodes <- origNodes[-snodesIdx]
              newEdges <- lapply(edges(graph)[snodes],
                                 function(x) {
                                     whD <- match(killedNodes, x, nomatch=0)
                                     if (any(whD))
                                       x[-whD]
                                     else
                                       x
                                 })
              ans <- new("graphNEL", nodes=snodes, edgeL=newEdges,
                         edgemode=edgemode(graph))
              ## FIXME: need to clean the attributes, right now we are passing
              ##        too much.
              nodeIdx <- match(snodes, names(graph@nodeData), 0)
              ans@nodeData@defaults <- graph@nodeData@defaults
              ans@nodeData@data <- graph@nodeData@data[nodeIdx]
              ee <- .getAllEdges(ans)
              if (length(ee$from) && length(ee$to)) {
                  kk <- .makeEdgeKeys(ee$from, ee$to)
                  whkk <- match(kk, names(graph@edgeData), 0)
                  ans@edgeData@defaults <- graph@edgeData@defaults
                  ans@edgeData@data <- graph@edgeData@data[whkk]
              }
              ans
          })


setMethod("numNodes", "graphNEL", function(object) length(object@nodes))


setMethod("addNode", signature(node="character", object="graphNEL",
                               edges="missing"),
          function(node, object, edges) {
              gN = nodes(object)
              already <- match(node, gN)
              if( any(!is.na(already)) )
                stop(paste(gN[already], collapse=", "), " is already a node")
              checkValidNodeName(node)
              ## add them on the end so we don't renumber
              gN = c(gN, node)
              edgeL <-  object@edgeL
              nEd <- vector("list", length=length(node))
              names(nEd) <- node
              for(i in seq(along=nEd))
                nEd[[i]] <- list(edges=numeric(0))
              edgeL <- c(edgeL, nEd)
              object@nodes <- gN
              object@edgeL <- edgeL
              object
          })


##they need to supply a list of edges, one for each element of node
##it might be better to do this by first adding the nodes then
##calling addEdges on that graph
setMethod("addNode", signature(node="character", object="graphNEL",
                               edges="list"),
          function(node, object, edges) {
              ## first add the nodes, it does the checking too
              object <- addNode(node, object)
              ## now add the edges:
              if (!all(names(edges) == node))
                stop("edges must be named and in the same order as nodes")
              doWeights <- FALSE
              newEdges <- lapply(edges, function(x) {
                  if (is.character(x))
                    x
                  else if (is.numeric(x)) {
                      doWeights <<- TRUE  ## set flag in function scope
                      if (length(x) == 0)
                        enms <- character(0)
                      else
                        enms <- names(x)
                      if (is.null(enms))
                        stop("invalid arg ", sQuote("edges"), "\n",
                             "elements must be character or have names ",
                             "corresponding to nodes")
                      enms
                  } else {
                      stop("invalid arg ", sQuote("edges"), "\n",
                           "expecting character or numeric list elements.")
                  }
              })

              for (i in seq(along=newEdges)) {
                  if (length(newEdges[[i]]) == 0)
                    next
                  if (doWeights)
                   object <- addEdge(from=node[i], to=newEdges[[i]], object,
                                     weights=edges[[i]])
                  else
                    object <- addEdge(from=node[i], to=newEdges[[i]], object)
              }
              object
          })


setMethod("removeNode", c("character", "graphNEL"),
          function(node, object) {
              ##first clear the node -- does the checking too
              object <- clearNode(node, object)
              nN <- nodes(object)
              wh <- match(node, nN)
              gN <- nN[-wh]
              nE <- object@edgeL[-wh]
              ## Now renumber the nodes as stored in the edgelist
              nE2 <- lapply(nE, function(el) {
                  oldN <- nN[el$edges]
                  el$edges <- match(oldN, gN)
                  el
              })
              object@nodes <- gN
              object@edgeL <- nE2
              object
          })


setMethod("clearNode", c("character", "graphNEL"), function(node, object) {
    gN <- nodes(object)
    whN <- match(node, gN)
    if(any(is.na(whN)) )
      stop(paste(whN[is.na(whN)], "is not a node in the graph"))
    ## clear node attributes
    object <- clearNodeData(object, node)
    object <- .dropEdges(object, whN)
    object@edgeL[whN] <- list(list(edges=numeric(0)))
    object
})


edgeKiller <- function(edgeL, from, whichKill) {
    for (i in seq(along=from)) {
        toKill <- whichKill[[i]]
        toKill <- toKill[!is.na(toKill)]
        if (length(toKill) == 0)
          stop("no edge from ", from[i], " to remove")
        edgeL[[from[i]]]$edges <- edgeL[[from[i]]]$edges[-toKill]
    }
    edgeL
}


setMethod("removeEdge",
          signature(from="character", to="character", graph="graphNEL"),
          function(from, to, graph) {
              gN <- nodes(graph)
              wh <- match(c(from, to), gN)
              if( any(is.na(wh)) )
                stop(paste(wh[is.na[wh]], "is not a node"))
              if (length(to) == 1)
                to <- rep(to, length(from))
              if (length(from) == 1)
                from <- rep(from, length(to))
              if (!isDirected(graph)) { 
                  fromOrig <- from
                  from <- c(fromOrig, to)
                  to <- c(to, fromOrig)
                  remove(fromOrig)
              }
              graph <- clearEdgeData(graph, from, to)
              remEL <- split(to, from)
              fromU <- names(remEL)
              nE <- edges(graph, fromU)
              whD <- mapply(function(x, y) match(x, y), remEL, nE,
                            SIMPLIFY=FALSE)
              graph@edgeL <- edgeKiller(graph@edgeL, fromU, whD)
              graph
          })


setMethod("addEdge", signature=signature(from="character", to="character",
                       graph="graphNEL", weights="numeric"),
          function(from, to, graph, weights) {
              graph <- addEdge(from, to, graph)
              if (!("weight" %in% names(edgeDataDefaults(graph))))
                edgeDataDefaults(graph, attr="weight") <- 1:1
              edgeData(graph, from=from, to=to, attr="weight") <- weights
              graph
          })


setMethod("addEdge", signature=signature(from="character", to="character",
                       graph="graphNEL", weights="missing"),
          function(from, to, graph) {
              preEdges <- isAdjacent(graph, from, to)
              if (any(preEdges)) {
                  preFr <- from[preEdges]
                  preTo <- to[preEdges]
                  preEdges <- paste(preFr, preTo, sep=EDGE_KEY_SEP)
                  warning("The following edges already exist and ",
                          "will be replaced:\n",
                          paste(preEdges, collapse=", "))
              }
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
              eL <- graph@edgeL
              for(i in 1:lenN) {
                  old <- eL[[from[i]]]
                  ## remove duplicate edges
                  old$edges <- unique(c(old$edges, whT[i]))
                  eL[[from[i]]] <- old
              }
              ##if undirected, then we need to go in both directions
              if( edgemode(graph) == "undirected")
                for(i in 1:lenN) {
                    old <- eL[[to[i]]]
                    ## remove duplicate edges
                    old$edges <- unique(c(old$edges, whF[i]))
                    eL[[to[i]]] <- old
                }
              graph@edgeL <- eL
              ##FIXME: should we call validObject here?
              graph
          })
 

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

              ##there might be no edges to add
              if( length(oE) > 0 ) {
                  if (is.null(oW) )
                    oW = rep(1, length(oE))
                  g2 <- addEdge(newName, oE, g2, oW)
              }
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
                  oEd = match(nodes, oE)
                  oEd = oEd[!is.na(oEd)]
                  if(length(oEd) > 0 ) {
                      oE <- oE[-oEd]
                      oW <- oW[-oEd]
                  }
                  if(is.null(oW)) oW=rep(1, length(oE))
                  g2 <- addEdge(oE, newName, g2, oW)
              }
              g2
          })


##inEdges returns a list of all nodes with edges
##to the nodes in "node"
setMethod("inEdges", c("missing", "graphNEL"),
          function(node, object)
          inEdges(nodes(object), object))


##seems more sensible - if there is only one arg
setMethod("inEdges", c("graphNEL", "missing"),
          function(node, object)
          inEdges(nodes(node), node))


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


.dropEdges <- function(self, x) {
    ## Remove all edges in graphNEL self to node with
    ## index x.  Also remove all edges from node with index x.
    ## Return the modified graphNEL.
    ## Removing edges also removes the associated attributes.
    oldEdgeL <- self@edgeL
    newEdgeL <- vector(mode="list", length=length(oldEdgeL))
    names(newEdgeL) <- names(oldEdgeL)
    nds <- nodes(self)
    for (i in seq(along=nds)) {
        toList <- oldEdgeL[[i]]$edges
        if (i %in% x) {
            to <- nds[toList]
            if (length(to))
              self <- clearEdgeData(self, from=nds[i], to=to)
            toList <- list(edges=numeric(0))
        } else {
            bad <- match(x, toList)
            bad <- bad[!is.na(bad)]
            if (length(bad)) {
                self <- clearEdgeData(self, from=nds[i], to=nds[toList[bad]])
                toList <- list(edges=toList[-bad])
            } else {
                toList <- list(edges=toList)
            }
        }
        newEdgeL[[nds[i]]] <- (toList)
    }
    self@edgeL <- newEdgeL
    self
}

##a leaf is an element of the graph with in edges and no out
## edges - the edgeL list in a directed graphNEL list the out
##edges

inOutCounts <- function(object) {
   if(!(edgemode(object)) == "directed") stop("only for directed graphs")
   numOut=sapply(object@edgeL, function(x) length(x$edges))
   inEdges = nodes(object)[unlist(sapply(object@edgeL, function(x)
                x$edges))]
   numIn = table(inEdges)
   return(list(numOut = numOut, numIn = numIn))
 }

leaves <- function(object) {
    io = inOutCounts(object)
    noIn = setdiff(nodes(object), names(io$numIn))
    pL = io$numOut[noIn]
    return(names(pL)[pL==0])
}
 
 inE <- function(object) {
   if(!(edgemode(object)) == "directed") stop("only for directed graphs")
   inEdges = nodes(object)[unlist(sapply(object@edgeL, function(x)
                x$edges))]
   numE = sapply(object@edgeL, function(x) length(x$edges))
   froms = rep(names(object@edgeL), numE)
   split(froms, inEdges)
 }

