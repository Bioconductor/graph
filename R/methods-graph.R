## String used as the separator to name edges in a graph.
EDGE_KEY_SEP <- "|"
EDGEMODE_DEPR_MSG <- "The edgemode slot is deprecated.\nUse 'updateGraph' to update this graph object."
EDGEMODE_DEFUNCT_MSG <- "The edgemode slot no longer exists.\nUse 'updateGraph' to update this graph object."


checkValidNodeName <- function(node) {
    if (!is.character(node))
      stop("node names must be character, got: ", sQuote(class(node)))
    ## Node names must have nchar(n) > 0, not be NA,
    ## and not contain the EDGE_KEY_SEP character.
    if (any(nchar(node) == 0))
      stop("invalid node names: empty string not allowed")
    if (any(is.na(node)))
      stop("invalid node names: missing value NA not allowed")
    bad <- grep(EDGE_KEY_SEP, node, fixed=TRUE)
    if (length(bad))
      stop("node name(s) contain edge separator ",
           sQuote(EDGE_KEY_SEP), ": ", pasteq(node[bad]))
    TRUE
}

setMethod("isDirected", "graph",
	  function(object){
            edgemode(object) == "directed"
          })
          


## Look through all serialized object within a folder, check if they are of
## class graph and update if necessary. This is not recursive, so lists of
## graphs or graphs within slots of objects will not be updated.
updateFolder <- function(path="."){
    files <- dir(path, pattern="\\.rda$")
    for(f in files){
        env <- new.env()
        load(f, envir=env)
        objects <- ls(env)
        needSave <- FALSE
        for(i in objects){
            if(is(get(i, env), "graph") && !isUpToDate(get(i, env))){
                assign(i, updateGraph(get(i, env)), envir=env)
                cat("Updated graph object", i, "\n")
                needSave <- TRUE
            }
        }
        if(needSave)
            save(list=objects, file=file.path(path,f), envir=env)
    } 
}




## Get the "real" slots of an object (slotNames gets the slots from
## the object definition)
getObjectSlots <- function(object) {
    if(!is.object(object) || isVirtualClass(class(object)))
      return(NULL)
    value <- attributes(object)
    value$class <- NULL
    if(is(object, "vector")) {
        .Data <- as.vector(object)
        attr(.Data, "class") <- NULL
        attrNames <- c('comment', 'dim', 'dimnames', 'names',
                       'row.names', 'tsp')
        for (nm in names(value)[names(value) %in% attrNames])
          attr(.Data, nm) <- value[[nm]]
        value <- value[!names(value) %in% attrNames]
        value$.Data <- .Data
    }
    value
}


## (FH 11/7/07) If the graph object is not up to data give a
## deprecated warning and try to find something useful,
## else use the edgemode item of the graphData list
setMethod("edgemode", "graph", function(object)
      {
          if(!isUpToDate(object)){
              ## first check in graphData then in edgemode slot
              if(!"graphData" %in% names(getObjectSlots(object))){
                  .Defunct(msg=EDGEMODE_DEFUNCT_MSG)
              }else{
                  em <- object@graphData$edgemode
                  if (is.null(em) && hasEdgemode(object))
                      em <- object@edgemode
                  if(is.null(em))
                      stop("'graph' object is corrupted")
              }
          }else
          em <- object@graphData$edgemode
          return(em)
      })


## (FH 11/7/07) Changed this to update the object in case it is outdated
## (edgemode now lives as a list item in graphData) 
setReplaceMethod("edgemode", c("graph", "character"),
    function(object, value)
{
    if(length(value) != 1L)
        stop("'edgemode' must be length 1")
    if(!(value %in% c("directed", "undirected")) )
        stop("'edgemode' must be 'directed' or 'undirected', was ",
             sQuote(value))
    if(hasEdgemode(object)){
        warning("'edgemode' slot is deprecated; ",
                "this graph object has been updated to ",
                "a new version", call.=FALSE)
        object <- updateGraph(object)
    }
    object@graphData$edgemode <- value
    edgeRenderInfo(object) <- list(arrowhead=NULL, arrowtail=NULL)
    object
})


## Check if graph object is up to date
isUpToDate <- function(object, error=FALSE)
{
  if(!is(object, "graph"))
    stop("'object' must inherit from class 'graph'")
  availSlots <- getObjectSlots(object)
  availSlotNames <- names(availSlots)
  definedSlotNames <- slotNames(object)
  valid <- setequal(availSlotNames, definedSlotNames) &&
                    length(object@graphData$edgemode)
  if(error && !valid)
    .Defunct(msg=EDGEMODE_DEFUNCT_MSG)
  return(valid)
}

hasEdgemode <- function(object)
{
   if(!is(object, "graph"))
     stop("'object' must inherit from class 'graph'")
   sn <- names(getObjectSlots(object))
   return("edgemode" %in% sn)
}


## Update an old graph instance
setMethod("updateGraph", "graph", function(object)
      {
          availSlots <- getObjectSlots(object)
          availSlotNames <- names(availSlots)
          definedSlotNames <- slotNames(object)
          if(isUpToDate(object)){
              message("This graph object seems to be up to date")
              newObject <- object
          }else{
              commonSlots <- intersect(definedSlotNames, availSlotNames)
              missingSlots <- setdiff(definedSlotNames, availSlotNames)
              if("graphData" %in% missingSlots &&
                 !"edgemode" %in% availSlotNames)
                stop("'object' is corrupted, don't know how to update.")
              newObject <- new(class(object))
              for(s in commonSlots)
                  slot(newObject, s) <- availSlots[[s]]
              edgemode(newObject) <- suppressWarnings(edgemode(object))
        }
          return(newObject)
      })
              
              
    
   

setMethod("numEdges", signature(object="graph"),
          function(object) {
              gEdges <- edges(object)
              if (length(gEdges) == 0)
                return(0)
              numEdges <- length(unlist(gEdges, use.names=FALSE))
              if (!isDirected(object)) {
                  numSelfLoops <- sum(mapply(function(e, n) sum(n == e),
                                             gEdges, names(gEdges)))
                  numEdges <- numSelfLoops + (numEdges - numSelfLoops) / 2
              }
              numEdges
          })

  ## a node-edge-list graph
  ##the edgeL is a list, with edges, weights etc

setMethod("isAdjacent",signature(object="graph", from="character",
                                 to="character"),
          function(object, from, to) {
              eSpec <- .normalizeEdges(from, to)
              from <- eSpec$from
              to <- eSpec$to
              fromIdx <- match(from, nodes(object), nomatch=0)
              toIdx <- match(to, nodes(object), nomatch=0)
              if (any(fromIdx == 0))
                stop("unknown nodes in 'from': ",
                     pasteq(from[fromIdx == 0]))
              if (any(toIdx == 0))
                stop("unknown nodes in 'to': ",
                     pasteq(to[toIdx == 0]))
              fromEdges <- edges(object)[from]
              .Call(graph_is_adjacent, fromEdges, to)
          })


  setMethod("degree", "graph",  function(object, Nodes) {
       nl <- edges(object)
       if (missing(Nodes))
           Nodes <- nodes(object)
       nls <- nl[Nodes]

       deg <- listLen(nls)
       names(deg) <- Nodes
       if (!isDirected(object)) {
           nself = sapply(1:length(nl),
             function(i) sum(names(nl)[i] == nl[[i]]))
           names(nself) = names(nl)
           return(deg + sum(nself))
           }
       else {
           b1 <- unlist(nl)
           b2 <- table(b1)
           nonZeroNodes <- Nodes[Nodes %in% names(b2)]
           inDegree <- structure(integer(length(Nodes)), names=Nodes)
           inDegree[nonZeroNodes] <- as.integer(b2[nonZeroNodes])
           return(list(inDegree=inDegree, outDegree=deg))
       }
  })


setMethod("leaves", "graph",
          function(object, degree.dir) {
              deg <- degree(object)
              leaf_degree <- 1L
              if (isDirected(object)) {
                  if (missing(degree.dir))
                    stop("'degree.dir' must be specified for a directed graph")
                  degree.dir <- switch(match.arg(degree.dir, c("in", "out")),
                                       "in"="inDegree",
                                       "out"="outDegree")
                  deg <- deg[[degree.dir]]
                  leaf_degree <- 0L
              }
              wh <- deg == leaf_degree
              names(deg)[wh]
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
       for( i in seq_len(nIndex)) {
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
          function(object, index, attr, default, type.checker)
          {
              ## Check extra args
              if (!is.character(attr) || length(attr) != 1)
                stop("'attr' must be character(1)")
              if (!is.null(type.checker) && !is.function(type.checker))
                stop("'type.checker' be a function or NULL")
              
              if (! attr %in% names(edgeDataDefaults(object))) {
                  ## No existing 'weight' edge attr, uses default
                  edgeDataDefaults(object, attr) <- default
              }
              ew <- edgeData(object, from=index, attr=attr)
              if (!length(ew))
                return(lapply(edges(object), function(x)
                              vector(mode=mode(default), length=0))[index])
              gEdges <- edges(object)[index]
              edgeCounts <- sapply(gEdges, length)
              nn <- rep(index, edgeCounts)
              names(ew) <- unlist(gEdges, use.names=FALSE)
              ew <- unlist(ew)
              if (!is.null(type.checker) && !isTRUE(type.checker(ew)))
                stop("edge weight type.checker(ew) not TRUE\n",
                     "typeof(ew): ", typeof(ew))

##               for (el in ew) {
##                   ## XXX: if el is an S4 instance, it will match "list"
##                   if (!isTRUE(type.checker(el)))
##                     stop("invalid type of edge weight.\n",
##                          "type.checker(el) not TRUE\n",
##                          "typeof(el): ", typeof(el))
##               }

##               if (!is.null(type.checker)) {
##                   dMode <- storage.mode(default)
##                   tryCatch(storage.mode(ew) <- dMode,
##                            warning=function(w) {
##                                wMsg <- conditionMessage(w)
##                                msg <- paste("unable to type.checker edge weight to",
##                                             dMode, "\n", wMsg)
##                                if (grep("NA", msg))
##                                  stop(msg)
##                                else
##                                  warning(wMsg)
##                            })
              ans = split(unlist(ew), nn)
              ans <- c(ans, lapply(gEdges[edgeCounts == 0], as.numeric))
              ans[index] ## split does sorting, we want orig order
          })


setMethod("edgeWeights", signature(object="graph", index="numeric"),
          function(object, index, attr, default, type.checker)
          {
              index <- nodes(object)[index]
              edgeWeights(object, index, attr=attr, default=default,
                          type.checker=type.checker)
          })


setMethod("edgeWeights", signature(object="graph", index="missing"),
          function(object, index, attr, default, type.checker)
          {
              index <- nodes(object)
              edgeWeights(object, index, attr=attr, default=default,
                          type.checker=type.checker)
          })


setMethod("DFS", c("graph", "character", "ANY"), function(object, node,
    checkConn=TRUE) {
    nNames <- nodes(object)
    marked <- rep(NA, length(nNames))
    names(marked) <- nNames
    m1 <- match(node, nNames)
    if( is.na(m1) )
        stop("node not in graph: ", sQuote(node))

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



### yet another implementation of "intersection", in C

setMethod("intersection2", c("graph", "graph"), function(x,y) {
        if (edgemode(x) != edgemode(y) )
           stop("both graphs must have the same edgemode")

        if (edgemode(x) == "undirected")
           edgeM <- 0
        else
           edgeM <- 1

        .Call(graph_intersection, nodes(x), nodes(y), edges(x),
              edges(y), edgeM)

})

   setMethod("intersection", c("graph", "graph"), function(x,y) {
       if( edgemode(x) != edgemode(y) )
           stop("both graphs must have the same edgemode")
       xN <- nodes(x)
       yN <- nodes(y)
       bN <- intersect(xN, yN)
       if( length(bN) == 0 )
           return(graphNEL(nodes=character(0),
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
       for(i in seq_along(xE) ) {
           ans <- intersect(xE[[i]], yE[[i]])
           rval[[i]] <- list(edges=match(ans, bN),
                                 weights=rep(1, length(ans)))
       }
       names(rval) <- bN
       graphNEL(nodes=bN, edgeL=rval, edgemode=edgemode(x))
   })

setMethod("join", c("graph", "graph"), function(x, y) {
    ex <- edgemode(x); ey <- edgemode(y)
    if(ex == ey)
        outmode <- ex
    else
        stop("cannot handle different edgemodes")

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
        for (i in seq_along(eLY)) {
            newEntry <- eLY[i]
            ## !! first need to adjust the targets on the edges
            newEdges <- newEntry[[1]]$edges
            if (length(newEdges) > 0) {
                for (j in seq_along(newEdges)) {
                    curTo <- nY[newEdges[j]]
                    newTo <- match(curTo,newNodes)
                    if (is.na(newTo))
                        stop("error reassigning duplicated nodes")
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
                    stop("duplicated node names in original graph")
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
    ## all nodes are present, as the graphNEL() call below
    ## will check to make sure that length(nodes) == length(edgeL)
    for (missNode in newNodes[! newNodes %in% names(newEdgeL)]) {
        newEdgeL[[length(newEdgeL) + 1]] <- list(edges=numeric(),
                                                 weights=numeric())
        names(newEdgeL)[length(newEdgeL)] <- missNode
    }

    graphNEL(nodes=newNodes, edgeL=newEdgeL, edgemode=ex)
})



setMethod("union", c("graph", "graph"), function(x, y, ...) {
    ex <- edgemode(x); ey <- edgemode(y);
    if( ex == ey )
        outmode <- ex
    else
        stop("cannot handle different edgemodes")

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
    graphNEL(nodes=xN, edgeL=rval, edgemode=outmode)
})



setMethod("complement", c("graph"), function(x) {
    if( edgemode(x) != "undirected" )
        stop("'edgemode' not supported: ", sQuote(edgemode(x)))

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
    graphNEL(nodes=xN, edgeL=rval, edgemode=edgemode(x))
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
    for(i in seq_len(nc)) rL[[i]]<-c(nmR[[i]], names(rval[[i]]))
    return(rL)
})

setMethod("isConnected", "graph",
          function(object, ...) (length(connComp(object)) == 1))

setMethod("numNodes", "graph", function(object) length(nodes(object)))


setMethod("show", signature("graph"),
          function(object) {
              isUpToDate(object, error=TRUE)
              numNodes<- numNodes(object)
              numEdge<-numEdges(object)
              cat("A", class(object), "graph with", edgemode(object), "edges\n")
              cat("Number of Nodes =", numNodes, "\n")
              cat("Number of Edges =", numEdge, "\n")
          })


.edgeWeight <- function(from, to, graph)
{
    gN <- nodes(graph)
    wF <- match(from, gN)
    if( is.na(wF) )
        stop("not a node: ", sQuote(from))
    wT <- match(to, gN)
    if( is.na(wT) )
        stop("not a node: ", sQuote(to))
    eL <- graph@edgeL[[from]]
    mt <- match(wT, eL$edges)
    if(is.na(mt) )
        stop("no edge from ", sQuote(from), " to ", sQuote(to))
    eL$weights[mt]
}


##take a sparse matrix - csr and put it into a graph
sparseM2Graph <- function(sM, nodeNames, edgemode=c("directed", "undirected"))
{
    ## FIXME: this needs to become a method
    qrequire("SparseM")
    edgemode <- match.arg(edgemode)
    nN <- dim(sM)[1]
    if( nN != dim(sM)[2] )
        stop("only square matrices can be transformed")
    if( length(nodeNames) != nN )
        stop("wrong number of node names supplied")
    if( !is.character(nodeNames) )
        stop("wrong type of node names supplied")
    dd <- diff(sM@ia)
    e1 <- rep(seq_len(nN), dd)
    eL <- split(sM@ja, e1)
    eW <- split(sM@ra, e1)

    edL <- vector("list", length=nN)
    names(edL) <- seq_len(nN)
    for(i in as.character(seq_len(nN)) ){
        ##need this because otherwise partial matching is done
        if( i %in% names(eL) )
            edL[[i]] <- list(edges=eL[[i]], weights=eW[[i]])
        else
          edL[[i]] <- list(edges=numeric(0))
    }
    names(edL) <- nodeNames
    graphNEL(nodes=nodeNames, edgeL=edL, edgemode=edgemode)
}

##translate a graph to a SparseMatrix:
##ra - the values; these will be 1's for now
##ja - the column indices
##ia the row offsets (
graph2SparseM <- function(g, useweights=FALSE) {
    ## FIXME: this needs to become a method
    qrequire("SparseM")
    if (! is(g, "graphNEL"))
       stop("coercion only works for graphNEL class")
    nr = nc = numNodes(g)
    e1 = g@edgeL
    e2 = lapply(e1, function(x) x$edges)

    eL = listLen(e2)
    if (useweights && ("weight" %in% names(edgeDataDefaults(g))))
      ra <- unlist(edgeData(g, attr="weight"))
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

    ## convert names to integers ("standard node labeling")
    to   <- lapply(edges(object), match, nodes(object))
    from <- match(names(to), nodes(object))

    if(any(is.na(unlist(to)))||any(is.na(from)))
      stop("edge names do not match node names")

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
    stop("edge names do not match node names")

  if(!selfLoops) {
    ufrom <- rep(from, listLen(to))
    uto   <- unlist(to)
    if(any(ufrom==uto))
      stop("graph must not contain self-loops")
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
      stop("unknown nodes: ", pasteq(unknownNodes))
    TRUE
}


setMethod("nodeData",
          signature(self="graph", n="character", attr="character"),
          function(self, n, attr) {
              .verifyNodes(n, nodes(self))
              attrDataItem(self@nodeData, x=n, attr=attr)
          })


setReplaceMethod("nodeData",
                 signature(self="graph", n="character", attr="character",
                           value="ANY"),
                 function(self, n, attr, value) {
                     .verifyNodes(n, nodes(self))
                     attrDataItem(self@nodeData, x=n, attr=attr) <- value
                     self
          })


setMethod("nodeData",
          signature(self="graph", n="character", attr="missing"),
          function(self, n, attr) {
              .verifyNodes(n, nodes(self))
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
          stop("'to' must be length 1 or ", lenFr)
        to <- rep(to, lenFr)
    } else if (lenFr < lenTo) {
        if (lenFr != 1)
          stop("'from' must be length 1 or ", lenTo)
        from <- rep(from, lenTo)
    }
    list(from=from, to=to)
}


.verifyEdges <- function(graph, from, to) {
    stopifnot(length(from) == length(to))
    if (length(from) == 0L) return(TRUE) # no edges
    adjList <- isAdjacent(graph, from, to)
    if (any(!adjList)) {
        badFr <- from[!adjList]
        badTo <- to[!adjList]
        res <- paste(badFr, badTo, sep=EDGE_KEY_SEP, collapse=", ")
        stop("edges not found: ", sQuote(res))
    }
    TRUE
}


.makeEdgeKeys <- function(from, to) {
    stopifnot(length(from) == length(to))
    paste(from, to, sep=EDGE_KEY_SEP)
}


.getEdgeKeys <- function(graph, from, to) {
    eSpec <- .normalizeEdges(from, to)
    from <- eSpec$from
    to <- eSpec$to
    .verifyEdges(graph, from, to)
    edgeKeys <- .makeEdgeKeys(from, to)
    edgeKeys
}


setMethod("edgeData", signature(self="graph", from="character", to="character",
                                attr="character"),
          function(self, from, to, attr) {
              edgeKeys <- .getEdgeKeys(self, from, to)
              attrDataItem(self@edgeData, x=edgeKeys, attr=attr)
          })


setMethod("edgeData", signature(self="graph", from="character", to="missing",
                                attr="character"),
          function(self, from, to, attr) {
              .verifyNodes(from, nodes(self))
              gEdges <- edges(self)[from]
              lens <- sapply(gEdges, length)
              fEdges <- rep(from, lens)
              if (!length(fEdges))
                return(list())
              tEdges <- unlist(gEdges)
              edgeKeys <- .getEdgeKeys(self, fEdges, tEdges)
              attrDataItem(self@edgeData, x=edgeKeys, attr=attr)
          })


setMethod("edgeData", signature(self="graph", from="missing", to="character",
                                attr="character"),
          function(self, from, to, attr) {
              eDat <- edges(self)
              inE <- inEdges(to, self)
              to <- rep(to, sapply(inE, length))
              from <- unlist(inE)
              ## from <- names(eDat)[sapply(eDat, function(x) to %in% x)]
              edgeKeys <- .getEdgeKeys(self, from, to)
              attrDataItem(self@edgeData, x=edgeKeys, attr=attr)
          })


setReplaceMethod("edgeData",
                 signature(self="graph", from="character", to="character",
                           attr="character", value="ANY"),
                 function(self, from, to, attr, value) {
                     edgeKeys <- .getEdgeKeys(self, from, to)
                     attrDataItem(self@edgeData, x=edgeKeys, attr=attr) <- value
                     if (!isDirected(self)) {
                         edgeKeys <- .getEdgeKeys(self, to, from)
                         attrDataItem(self@edgeData, x=edgeKeys,
                                      attr=attr) <- value
                     }
                     self
                 })


setReplaceMethod("edgeData",
                 signature(self="graph", from="character", to="missing",
                           attr="character", value="ANY"),
                 function(self, from, to, attr, value) {
                     .verifyNodes(from, nodes(self))
                     gEdges <- edges(self)[from]
                     lens <- sapply(gEdges, length)
                     if (any(lens == 0))
                       warning("no edges from nodes: ",
                               pasteq(from[lens == 0]))
                     fEdges <- rep(from, lens)
                     tEdges <- unlist(edges(self)[from])
                     edgeKeys <- .getEdgeKeys(self, fEdges, tEdges)
                     attrDataItem(self@edgeData, x=edgeKeys, attr=attr) <- value
                     if (!isDirected(self)) {
                         edgeKeys <- .getEdgeKeys(self, tEdges, fEdges)
                         attrDataItem(self@edgeData, x=edgeKeys,
                                      attr=attr) <- value
                     }
                     self
                 })


setReplaceMethod("edgeData",
                 signature(self="graph", from="missing", to="character",
                           attr="character", value="ANY"),
                 function(self, from, to, attr, value) {
                     eDat <- edges(self)
                     from <- names(eDat)[sapply(eDat, function(x) to[1] %in% x)]
                     edgeKeys <- .getEdgeKeys(self, from, to)
                     attrDataItem(self@edgeData, x=edgeKeys, attr=attr) <- value
                     if (!isDirected(self)) {
                         edgeKeys <- .getEdgeKeys(self, to, from)
                         attrDataItem(self@edgeData, x=edgeKeys,
                                      attr=attr) <- value
                     }
                     self
                 })


.getAllEdges <- function(graph) {
    e1 <- edges(graph)
    if (length(e1) == 0L) {
        list(from=character(0), to=character(0))
    } else {
        n1 <- nodes(graph)
        n1 <- rep(n1, sapply(e1, length))
        list(from=n1, to=unlist(e1))
    }
}


setMethod("edgeData", signature(self="graph", from="missing", to="missing",
                                attr="character"),
          function(self, from, to, attr) {
              eSpec <- .getAllEdges(self)
              from <- eSpec$from
              to <- eSpec$to
              edgeKeys <- .getEdgeKeys(self, from, to)
              attrDataItem(self@edgeData, x=edgeKeys, attr=attr)
          })


setMethod("edgeData", signature(self="graph", from="character", to="character",
                                attr="missing"),
          function(self, from, to, attr) {
              edgeKeys <- .getEdgeKeys(self, from, to)
              attrDataItem(self@edgeData, x=edgeKeys)
          })


setMethod("edgeData", signature(self="graph", from="missing", to="missing",
                                attr="missing"),
          function(self, from, to, attr) {
              eSpec <- .getAllEdges(self)
              from <- eSpec$from
              to <- eSpec$to
              edgeKeys <- .getEdgeKeys(self, from, to)
              attrDataItem(self@edgeData, x=edgeKeys)
          })

## still needed for Rgraphviz' plot() [well, as long as edgeL() is still there]
setMethod("edgeL", "graph",
	  function(graph, index) callGeneric(as(graph, "graphNEL")))

setMethod("plot", c("graph", "ANY"),
    function(x, y, ...)
{
    qrequire("Rgraphviz")
    method <- getMethod("plot", c("graph", "ANY"),
                        getNamespace("Rgraphviz"))
    method(x, y, ...)
})

clearEdgeData <- function(self, from, to) {
    ##FIXME: make me a method
    edgeKeys <- .getEdgeKeys(self, from, to)
    removeAttrDataItem(self@edgeData, x=edgeKeys) <- NULL
    self
}


clearNodeData <- function(self, n) {
    ##FIXME: make me a method
    removeAttrDataItem(self@nodeData, x=n) <- NULL
    self
}


