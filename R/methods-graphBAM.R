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

graphBAM <- function(df, nodes = NULL, edgemode = "undirected") {
    cl <- colnames(df) %in%  c("from","to", "weight")
    if(!all(cl)){
        stop(c( c("from", "to", "weight")[!cl],
                    " is not in the column names of df \n"))
    }
    nodes <- sort(unique(c(as.character(df$from), as.character(df$to), nodes)))
    is_directed <- edgemode == "directed"
    edge_sets <- .makeMDEdgeSet(es_name = 1, es = df, is_directed = is_directed, nodes)
    new("graphBAM", nodes = nodes, edgeSet = edge_sets)
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

graphBAMExtractFromTo <- function(object) {

    diEdgeSetToDataFrame(object@edgeSet,nodes(object))
}

setAs(from="graphBAM", to="matrix",
      function(from) {
          edgeSetToMatrix(nodes(from), from@edgeSet, isDirected(from))
      })

setAs(from="graphBAM", to="graphAM",
        function(from) {
    am <- new("graphAM", adjMat = as(from, "matrix"),
          edgemode = edgemode(from), values = list(weight=1))
    am@nodeData <- from@nodeData
    am
    })

setAs(from="graphBAM", to="graphNEL",
        function(from) {
            am <- as(from, "graphAM")
            as(am, "graphNEL") 
        })

graphToBAM <- function(object) {
    ew <- edgeWeights(object)
    df_empty <- data.frame(from = character(0), to = character(0),
                           weight = numeric(0))
    df_list <- lapply(names(ew), function(x){
                tmp <- ew[[x]]
                if ((nw <- length(tmp)) > 0) {
                    data.frame(from = rep(x, nw), to = names(tmp),
                               weight = as.numeric(tmp))
                } else df_empty
            })
     df <- do.call(rbind, df_list)
     is_directed  <-  edgemode(object) == "directed"
     nn <- nodes(object)
     edge_sets <- .makeMDEdgeSet(es_name = 1, es = df,
                                 is_directed = is_directed, nn)
     bam <-  new("graphBAM", nodes = nn, edgeSet = edge_sets)
     bam@edgeData <- object@edgeData
     bam@nodeData <- object@nodeData
     bam
}

setAs(from="graphNEL", to="graphBAM",
        function(from) {
            #graphToBAM(from)
            am <- as(from, "graphAM")
            as(am,"graphBAM")
        })

setAs(from="graphAM", to="graphBAM",
        function(from) {
            ##FIXME for undirected case...
            graphToBAM(from)
        })

setMethod("ugraph", "graphBAM",
          function(graph) {
              edgemode(graph) <- "undirected"
              graph@edgeSet <- ugraph(graph@edgeSet)
              graph
          })


setMethod("addEdge",
          signature=c("character", "character", "graphBAM", "numeric"),
          function(from, to, graph, weights) {
              nn <- nodes(graph)
              report_bad <- function(w) {
                  bad <- w[!(w %in% nn)]
                  stop("unknown nodes in '", deparse(substitute(w)),
                       "': ",
                       paste(bad, collapse=", "))
              }
              if (!all(from %in% nn)) report_bad(from)
              if (!all(to %in% nn)) report_bad(to)
              df <- graphBAMExtractFromTo(graph)
              if (length(from) != length(to))
                  if (length(from) != 1 && length(to) != 1)
                      stop("'from' and 'to' lengths do not conform")
              df2 <- data.frame(from=from, to=to, weight=weights,
                                stringsAsFactors = FALSE)
              graphBAM(rbind(df, df2), edgemode=edgemode(graph))
          })

setMethod("addEdge",
          signature=c("character", "character", "graphBAM", "missing"),
          function(from, to, graph, weights) {
              w <- rep(1L, max(length(from), length(to)))
              addEdge(from, to, graph, w)
          })

setMethod("addNode",
        signature(node="character", object="graphBAM", edges="missing"),
        function(node, object) {
            df <- graphBAMExtractFromTo(object)
            graphBAM(df, nodes = node, edgemode = edgemode(object))
        })
