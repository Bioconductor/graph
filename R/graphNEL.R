validGraph<-function(object, quietly=FALSE) {
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
            eds <- lapply(objEdges, function(x) x$edges)
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

setMethod("initialize", "graphNEL",
          ## FIXME: what about edge weights?
          function(.Object, nodes=character(0), edgeL, edgemode) {
              if( missing(edgemode) )
                edgemode <- "undirected"
              if( missing(edgeL) ) {
                  edgeL <- vector(mode="list", length=length(nodes))
                  names(edgeL) <- nodes
              } else {
                  if(length(nodes) != length(edgeL) )
                    stop("nodes and edges must align")
                  nameE <- names(edgeL)
                  if( !is.null(nameE) && !all( nameE %in% nodes) )
                    stop("names of nodes and edges must agree")
                  if( !is.null(nameE) )
                    edgeL <- edgeL[nodes]
                  ## (wh:) the following expression replaces one that was a bug
                  edgeL <- lapply(edgeL, function(x) {
                      if(is.character(x$edges))
                        x$edges <- match(x$edges, nodes)
                      return(x)
                  }) ## end lapply
              } ## end else-if

              .Object@nodes <- nodes
              .Object@edgeL <- edgeL
              .Object@edgemode <- edgemode

              validObject(.Object)
              return(.Object)
          })


setMethod("nodes", "graphNEL", function(object) object@nodes)


setReplaceMethod("nodes", c("graphNEL", "character"),
                 function(object, value) {
                     if(length(value) != length(object@nodes))
                       stop("need as many names as there are nodes")
                     if(any(duplicated(value)))
                       stop("node names must be unique")
                     object@nodes <- value
                     names(object@edgeL) <- value
                     object})


##the graphNEL representation stores edges as indexes into the
##node label vector
setMethod("edges", c("graphNEL", "missing"), function(object, which) {
    gNodes <- object@nodes
    lapply(object@edgeL, function(x) gNodes[x$edges])})


setMethod("edges", c("graphNEL", "character"),
          function(object, which) {
              gNodes <- nodes(object)
              lapply(object@edgeL[which], function(x) gNodes[x$edges])})


setMethod("edgeWeights", "graphNEL", function(object, index) {
    gN = nodes(object)
    if( !missing(index) ) {
        if(is.numeric(index) ) {
            if( any(index <= 0) )
              stop("only positive indices allowed")
            if( any(index > length(gN)) )
              stop("index too large")
        }
        if(is.character(index) ) {
            wh = match(index, gN)
            if( any(is.na(wh)) )
              stop("node name is incorrect")
        }
    }
    if( missing(index) )
      tlist <- object@edgeL
    else
      tlist <- object@edgeL[index]
    wts <- lapply(tlist, function(x) {
        wts <- x$weights
        if(is.null(wts)) {
            wts <- rep(1, length(x$edges))
            names(wts) <- gN[x$edges]
        }
        ## Always make sure that the weight vector
        ## has names attached
        if (is.null(names(wts)))
          names(wts) <- gN[x$edges]

        wts})
    wts})


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
                                 if( length(ed) > 0 )
                                   lapply(x, function(y) {names(y) <- ed; y})})
    ##finally return
    new("graphNEL", nodes=nn[ma], edgeL=newed, edgemode=edgemode(graph)) })


setMethod("numNodes", "graphNEL", function(object) length(object@nodes))


setMethod("addNode", c("character", "graphNEL", "missing"),
          function(node, object, edges) {
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
              for(i in seq(along=nEd))
                nEd[[i]] <- list(edges=numeric(0), weights=numeric(0))
              edgeL <- c(edgeL, nEd)
              new("graphNEL", gN, edgeL, edgemode(object))
          })


##they need to supply a list of edges, one for each element of node
##it might be better to do this by first adding the nodes then
##calling addEdges on that graph
setMethod("addNode", c("character", "graphNEL", "list"),
          function(node, object, edges) {
              gN = nodes(object)
              nNode = length(gN)
              if( !all(names(edges) == node) )
                stop("edges must be named and in the same order as nodes")
              already <- match(node, gN)
              if( any(!is.na(already)) )
                stop(paste(node[already], "is already a node"))
              ##add them on the end so we don't renumber
              gN = c(gN, node)
              edgeL <-  object@edgeL
              nAdd <- length(node)
              nEd <- vector("list", length=nAdd)
              names(nEd) <- node
              for(i in 1:nAdd) {
                  ed <- edges[[i]]
                  if( is.character(ed) ) {
                      whE = match(ed, gN)
                      wts = rep(1, length(whE))
                  } else if( is.numeric(ed) ) {
                      whE = match(names(edges), gN)
                      wts = ed
                  } else
                  stop("bad type for edgelist")
                  if( any(is.na(whE)) )
                    stop("supplied edges not in the graph")
                  names(wts) = whE
                  nEd[[i]] <- list(edges=whE, weights=wts)
              }
              edgeL <- c(edgeL, nEd)
              ##for undirected graphs we need to put the edges on
              ##the other side
              if( edgemode(object) == "undirected")
                for(i in 1:nAdd) {
                    ed <- edges[[i]]
                    if( is.character(ed) ) {
                        whE = match(ed, gN)
                        wts = rep(1, length(whE))
                    } else if( is.numeric(ed) ) {
                        whE = match(names(edges), gN)
                        wts = ed
                    } else
                    stop("bad type for edgelist")
                    for(j in 1:length(whE) ) {
                        idx = whE[j]
                        nL <- NULL
                        nL$edges <- c(edgeL[[idx]]$edges, nNode+i)
                        nL$weights <- c(edgeL[[idx]]$weights, wts[j])
                        names(nL$weights) <-
                          c(names(edgeL[[idx]]$weights), nNode+i)
                        edgeL[[idx]] <- nL
                    }
                }
              new("graphNEL", gN, edgeL, edgemode(object))
          })


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


setMethod("clearNode", c("character", "graphNEL"), function(node, object) {
    gN <- nodes(object)
    whN <- match(node, gN)
    if(any(is.na(whN)) )
      stop(paste(whN[is.na(whN)], "is not a node in the graph"))
    gE <- .dropEdges(whN, object@edgeL)
    gE[[whN]] = list(edges=numeric(0), weights=numeric(0))
    new("graphNEL", gN, gE, edgemode(object))
})


##FIXME: vectorize
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
              g2})


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

