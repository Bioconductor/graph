## bit array adjacency matrix representation of a graph

setMethod("initialize", signature("graphBAM"),
        function(.Object, nodes,edgeSet) {
            .Object@graphData$edgemode <- if(is(edgeSet, "UEdgeSet")) 
                                            "undirected" 
                                          else "directed"
            .Object@nodeData <- new("attrData")
            .Object@edgeData <- new("attrData")
            .Object@nodes <- nodes
            .Object@edgeSet <- edgeSet
            .Object
        })

GraphBAM <- function(from, to, nodes=NULL, weights=NULL, edgemode="undirected") {
    if(is.null(nodes)){
        nodes <- sort(unique(c(from,to)))
    } else {
        snodes <- c(from,to)
        snodesIdx <- match(snodes, nodes)
        if (any(is.na(snodesIdx))) {
            stop("invalid arg: from or to contains nodes not in the ",
                    "nodes argument\n")
        }
    }
    weight = if(is.null(weights)) rep(1L, length(from)) else weights
    ft <- data.frame(from, to, weight)
    is_directed  <-  if(edgemode == "directed") TRUE else FALSE
    edge_sets <- .makeMDEdgeSet(es_name = 1, es = ft, is_directed = is_directed, nodes)
    g <- new("graphBAM", nodes = nodes, edgeSet = edge_sets)
}

setMethod("numEdges", signature = signature(object = "graphBAM"),
        function(object) {
            numEdges(object@edgeSet)
        })

setMethod("show", signature("graphBAM"),
        function(object) {
            numNodes<- numNodes(object)
            cat("A", class(object), "with", edgemode(object), "edges \n")
            cat("Number of nodes =", numNodes, "\n")
            cat("edge_count =", numEdges(object),"\n")
        })

.undirectEdges <- function(from, to)
{
    fromIsFirst <- from < to
    toIsFirst <- !fromIsFirst
    tmpFrom <- c(from[fromIsFirst], to[toIsFirst])
    tmpTo <- c(to[fromIsFirst], from[toIsFirst])
    from <- tmpFrom
    to <- tmpTo
    list(from=from, to=to)
}

getEdgeList2 <- function(g) {
    nodeNames <- g@nodes
    numNodes <- length(nodeNames)
    eList <- structure(vector(mode="list", length=numNodes),
            names = nodeNames)
    isMiss <- logical(numNodes)
    bv <- g@edgeSet@bit_vector
    for (i in seq_len(numNodes)) {
        aRow <- getRow(bv, i)
        val <- nodeNames[aRow]
        if (length(val))
            eList[[i]] <- nodeNames[aRow]
        else{
            isMiss[i] <- TRUE
            eList[[i]] <- character(0)
        }
    }
    eList
}

setMethod("edges", signature("graphBAM", "missing"),
        function(object) {
            ans <- getEdgeList2(object)
            if (edgemode(object)=="directed"){
                ans
            } else {
                lens <- sapply(ans, length)
                nms <- rep(names(ans), lens)
                revAns <- split(nms, unlist(ans))
                for (n in names(revAns)) {
                    ans[[n]] <- sort(c(ans[[n]], revAns[[n]]))
                }
            }
            ans
        })


setMethod("edges", signature("graphBAM", "character"),
        function(object, which) {
            ## TODO: refactor to optimize
            eg <- edges(object)
            eg[which]
        })

getWeightList2 <- function(g){
    nodeNames <- g@nodes
    numNodes <- length(nodeNames)
    w <- g@edgeSet@weights
    eList <- structure(vector(mode="list", length=numNodes),
            names = nodeNames)
    isMiss <- logical(numNodes)
    bv <- g@edgeSet@bit_vector
    ft <- .Call(graph_bitarray_rowColPos, g@edgeSet@bit_vector,
            numNodes)
    if(!isDirected(g)){
        df <- cbind(from=ft[,"to"], to = ft[,"from"])
        ft <- rbind(ft,df)
        w <- c(w,w)
    }
    fromNode <- nodeNames[ft[ , "from"]]
    toNode <- nodeNames[ft[ , "to"]]
    wList <- split(w, fromNode)
    wNameList <- split(toNode, fromNode)
    wList <- mapply(function(wVals, wNames) {
        names(wVals) <- wNames
        wVals
    }, wList, wNameList)
    haveNoEdge <- setdiff(nodeNames, names(wList))
    names(haveNoEdge) <- haveNoEdge
    haveNoEdge <- lapply(haveNoEdge, function(x) numeric(0))
    c(wList, haveNoEdge)[nodeNames]
}

setMethod("edgeWeights", signature(object="graphBAM", index="character"),
          function(object, index, attr, default, type.checker)
          {
              if (!is.character(attr) || length(attr) != 1)
                stop(sQuote("attr"),
                     " must be a character vector of length one.")
              if (!is.null(type.checker) && !is.function(type.checker))
                stop(sQuote("type.checker"), " must be a function or NULL.")
             
               lst <- getWeightList2(object)
               lst[index]
          })


setMethod("edgeWeights", signature(object="graphBAM", index="numeric"),
          function(object, index, attr, default, type.checker)
          {
              index <- nodes(object)[index]
              edgeWeights(object, index, attr=attr, default=default,
                          type.checker=type.checker)
          })


setMethod("edgeWeights", signature(object="graphBAM", index="missing"),
          function(object, index, attr, default, type.checker)
          {
              index <- nodes(object)
              edgeWeights(object, index, attr=attr, default=default,
                          type.checker=type.checker)
          })


setMethod("edgeData", signature(self="graphBAM", from="missing", to="missing",
                  attr="missing"),
         function(self, from, to, attr) {
              nodeNames <- self@nodes
              numNodes <- length(nodeNames)
              bv <- self@edgeSet@bit_vector
              ft <- .Call(graph_bitarray_rowColPos, bv, numNodes)
              w <- self@edgeSet@weights
              if(!isDirected(self)){
                    df <- cbind(from=ft[,"to"], to = ft[,"from"])
                    ft <- rbind(ft,df)
                    w <- c(w,w)
              }
              ## FIXME Also, possible to
              ## reuse code from MultiGraph eweights function?
              nodeLbl <- paste( nodeNames[ft[,"from"]], nodeNames[ft[, "to"]],
                      sep ="|")
              names(w) <- nodeLbl
              lapply(w, function(x) list(weight = as.numeric(x)))
          })

setMethod("edgeData", signature(self="graphBAM", from="character", to="missing",
                attr="character"),
        function(self, from, to, attr) {
            if(attr !="weight")
                stop("Operation not supported")
            else{
                nodeNames <- self@nodes
                indx <- which(nodeNames ==from)
                numNodes <- length(nodeNames)
                bv <- self@edgeSet@bit_vector
                w <- self@edgeSet@weights
                ft <- .Call(graph_bitarray_rowColPos, self@edgeSet@bit_vector,
                        numNodes)
                if(!isDirected(self)){
                    df <- cbind(from=ft[,"to"], to = ft[,"from"])
                    ft <- rbind(ft,df)
                    w <- c(w,w)
                }
                ft <- data.frame(ft,w)
                ft <- ft[ ft[,"from"] %in% indx,]
                nodeLbl <- paste( nodeNames[ft[,"from"]], nodeNames[ft[, "to"]],
                        sep ="|")
                w <- ft[,"w"][1:length(nodeLbl)]
                names(w) <- nodeLbl
                lapply(w, function(x) list(weight = as.numeric(x)))

            }
        })

setMethod("edgeData", signature(self="graphBAM", from="character", to="character",
                attr="character"),
        function(self, from, to, attr) {
            if(attr !="weight")
                stop("Operation not supported")
            else{
                nodeNames <- self@nodes
                numNodes <- length(nodeNames)
                bv <- self@edgeSet@bit_vector
                w <- self@edgeSet@weights
                ft <- .Call(graph_bitarray_rowColPos, self@edgeSet@bit_vector,
                        numNodes)
                if(!isDirected(self)){
                    df <- cbind(from=ft[,"to"], to = ft[,"from"])
                    ft <- rbind(ft,df)
                    w <- c(w,w)
                }
                ft <- data.frame(ft,w)
                ft <- ft[ft[,"from"] %in% which(nodeNames == from),]
                ft <- ft[ft[,"to"] %in% which(nodeNames == to),] 
                nodeLbl <- paste( nodeNames[ft[,"from"]], nodeNames[ft[, "to"]],
                        sep ="|")
                w <- ft[,"w"][1:length(nodeLbl)]
                names(w) <- nodeLbl
                lapply(w, function(x) list(weight = as.numeric(x)))
            }
        })


setMethod("numNodes", signature("graphBAM"),
        function(object) length(object@nodes))


setMethod("isAdjacent",
        signature(object="graphBAM", from="character", to="character"),
        function(object, from, to) {
            
            nodeNames <- object@nodes
            snodes <- c(from,to)
            snodesIdx <- match(snodes, nodeNames)
            if (any(is.na(snodesIdx))) {
               stop("invalid arg: from or to contains nodes not in the ",
                        "graphBAM object:\n")
            }
            ## FIXME: should use a convention for
            ## undirected case
            if (!isDirected(object)) {
                ## normalize edges so that edges have nodes in lexical order
                tmp <- .undirectEdges(from, to)
                from <- tmp[["from"]]
                to <- tmp[["to"]]
            }
            from_i <- match(from, nodeNames)
            to_i <- match(to, nodeNames)
            ## FIXME: should check for NA
            getBitCell(object@edgeSet@bit_vector, from_i, to_i)
            ## FIXME: should return value be named with edge labels?
        })

setMethod("subGraph", signature(snodes="character", graph="graphBAM"),
        function(snodes, graph) {
            origNodes <- nodes(graph)
            snodes <- sort(snodes)
            snodesIdx <- match(snodes, origNodes)
            if (any(is.na(snodesIdx))) {
                bad <- snodes[which(is.na(snodesIdx))]
                stop("invalid arg: snodes contains nodes not in the ",
                        "MultiGraph:\n", paste(bad, collapse=", "))
            }
            res <- .Call("graph_bitarray_subGraph", graph@edgeSet@bit_vector, snodesIdx)
            graph@edgeSet@bit_vector <- res$bitVec
            graph@edgeSet@weights <- graph@edgeSet@weights[res$setPos]
            if(length(graph@edgeSet@edge_attrs))
                graph@edgeSet@edge_attrs <- graph@edgeSet@edge_attrs[res$setPos]
            graph@nodes <- snodes
            graph
        })

setMethod("edgeMatrix", "graphBAM",
        function(object, duplicates=FALSE) {
            bitvec <- object@edgeSet@bit_vector
            nds <- nodes(object)
            df <- .Call(graph_bitarray_rowColPos, bitvec, length(nds) )
            t(df)
        })



## getIndices <- function(nodes, from, to) {
##     ## Return indices into the adjMat for nodes from and to.
##     i <- match(from, nodes, nomatch=0)
##     if (i == 0)
##       stop("Unknown node", sQuote(from), "specified in from")
##     j <- match(to, nodes, nomatch=0)
##     if (j == 0)
##       stop("Unknown node", sQuote(to), "specified in to")
##     list(from=i, to=j)
## }


setMethod("addNode",
        signature(node="character", object="graphBAM", edges="missing"),
        function(node, object) {
            stop("operation not supported")
        })


setMethod("addEdge",
        signature(from="character", to="character", graph="graphBAM",
                weights="missing"),
        function(from, to, graph) {
            stop("operation not supported")
        })


setMethod("clearNode",
        signature(node="character", object="graphBAM"),
        function(node, object) {
            stop("operation not supported")
            ## idx <- getNodeIndex(nodes(object), node)
            ## zeroVect <- rep(0, ncol(object@adjMat))
            ## ## clear edges from node to other
            ## object@adjMat[idx, ] <- zeroVect
            ## ## clear edges from other to node
            ## object@adjMat[, idx] <- zeroVect

            ## TODO: clear edge attributes


            object
        })


## TODO: implement a clearEdgeAttributes method

setMethod("removeNode",
        signature(node="character", object="graphBAM"),
        function(node, object) {
            stop("operation not supported")
            ## idx <- getNodeIndex(nodes(object), node)
            ## object@adjMat <- object@adjMat[-idx, -idx]

            ## ## TODO: clear edge attributes

            ## object
        })


## getNodeIndex <- function(nodeNames, node) {
##     idx <- match(node, nodeNames, nomatch=NA)
##     if (any(is.na(idx)))
##       stop("Unknown node", sQuote(node))
##     idx
## }

## coordToIndex <- function(x, y, nrow) (y * nrow) - (nrow - x)

setMethod("removeEdge",
        signature(from="character", to="character", graph="graphBAM"),
        function(from, to, graph) {
            stop("operation not supported")
            ## fromIdx <- getNodeIndex(nodes(graph), from)
            ## toIdx <- getNodeIndex(nodes(graph), to)
            ## rowCnt <- nrow(graph@adjMat)
            ## graph@adjMat[coordToIndex(fromIdx, toIdx, rowCnt)] <- 0
            ## if (!isDirected(graph))
            ##   graph@adjMat[coordToIndex(toIdx, fromIdx, rowCnt)] <- 0
            ## graph
        })

## Yuck, I'd rather not support such shenanigans
## ## This signature looks strange, but to get in edges for all nodes
## ## it makes sense to be able to write inEdges(g)
## setMethod("inEdges", signature(node="graphBAM", object="missing"),
##           function(node, object) {
##               allNodes <- nodes(node)
##               return(inEdges(allNodes, node))
##           })

## ## But we still want inEdges(object=g) to work
## setMethod("inEdges", signature(node="missing", object="graphBAM"),
##           function(node, object) {
##               allNodes <- nodes(object)
##               return(inEdges(allNodes, object))
##           })


setMethod("inEdges", signature(node="character", object="graphBAM"),
        function(node, object) {
            allNodes <- nodes(object)
            unknownNodes <- !(node %in% allNodes)
            if (any(unknownNodes))
                stop("Unknown nodes:\n", paste(unknownNodes, collapse=", "))
            ## cols of adjacency give us in edges
            bv <- object@edgeSet@bit_vector

            ans <- structure(vector("list", length(node)),
                    names = node)
            node_i <- match(node, allNodes)
            k <- 1L
            for (i in node_i) {
                ans[[k]] <- allNodes[getColumn(bv, i)]
                k <- k + 1L
            }
            ans
        })
#
#edge_set_intersect <- function(g1, g2)
#{
#    ## TODO: make this vectorized to take ... or a list of graph
#    ## objects.  Probably have to forget about a generic as dispatch
#    ## will be difficult.
#    nodeNames <- nodes(g1)
#    stopifnot(all(nodeNames == nodes(g2)))
#    all_directed <- all(isDirected(g1))
#    klass <- if (all_directed) "DiEdgeSet" else "UEdgeSet"
#    bv <- g1@edgeSet@bit_vector
#    keepAttrs <- attributes(bv)
#    bv <- bv & g2@edgeSet@bit_vector
#    attributes(bv) <- keepAttrs
#    n_edges <- attr(bv, "nbitset") <- .Call(graph_bitarray_sum, bv)
#    if (n_edges > 0) {
#        new_edge_set <- list(new(klass, bit_vector = bv,
#                                  weights = rep(1L, n_edges),
#                                  edge_attrs = list()))
#        names(new_edge_set) <- paste(names(edge_sets), collapse = "_")
#    } else {
#        new_edge_set <- list()
#    }
#    new("graphBAM", edgeSet = new_edge_set, nodes = nodes(g1))
#}
#
#edge_set_union <- function(g1, g2)
#{
#    ## TODO: make this vectorized to take ... or a list of graph
#    ## objects.  Probably have to forget about a generic as dispatch
#    ## will be difficult.
#    nodeNames <- nodes(g1)
#    stopifnot(all(nodeNames == nodes(g2)))
#    bv1 <- g1@edgeSet@bit_vector
#    all_directed <- all(isDirected(g1))
#    klass <- if (all_directed) "DiEdgeSet" else "UEdgeSet"
#    bv <- g1@edgeSet@bit_vector
#    keepAttrs <- attributes(bv)
#    sharedVect <- bv1 | g2@edgeSet@bit_vector
#    attributes(sharedVect) <- keepAttrs
#    n_edges <- attr(bv, "nbitset") <- .Call(graph_bitarray_sum, bv)
#    if (n_edges > 0) {
#        new_edge_set <- list(new(klass, bit_vector = bv,
#                                  weights = rep(1L, n_edges),
#                                  edge_attrs = list()))
#        names(new_edge_set) <- paste(names(edge_sets), collapse = "_")
#    } else {
#        new_edge_set <- list()
#    }
#    new("graphBAM", edgeSet = new_edge_set, nodes = nodes(g1))
#}
#
#







