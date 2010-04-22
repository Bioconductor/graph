## bit array adjacency matrix representation of a graph

setMethod("initialize", signature("graphBAM"),
        function(.Object, nodes,edgeSet) {
            .Object@graphData$edgemode <-
                if (isDirected(edgeSet)) "directed" else "undirected"
            .Object@renderInfo@edges <- list(arrowhead=NULL, arrowtail=NULL)
            .Object@nodeData <- new("attrData")
            .Object@edgeData <- new("attrData")
            .Object@nodes <- nodes
            .Object@edgeSet <- edgeSet
            .Object
        })

graphBAM <- function(df, nodes = NULL, edgemode = "undirected",
                     ignore_dup_edges = FALSE) {
    cl <- colnames(df) %in%  c("from","to", "weight")
    if(!all(cl)){
        stop(c( c("from", "to", "weight")[!cl],
                    " is not in the column names of df \n"))
    }
    nodes <- sort(unique(c(as.character(df$from), as.character(df$to), nodes)))
    is_directed <- edgemode == "directed"
    edge_sets <- .makeMDEdgeSet(es_name = 1, es = df,
                                is_directed = is_directed, nodes,
                                ignore_dup_edges = ignore_dup_edges)
    new("graphBAM", nodes = nodes, edgeSet = edge_sets)
}

setMethod("numEdges", signature = signature(object = "graphBAM"),
        function(object) {
            numEdges(object@edgeSet)
        })

.undirectEdges <- function(from, to)
{
    fromIsFirst <- from <= to
    toIsFirst <- !fromIsFirst
    tmpFrom <- c(from[fromIsFirst], to[toIsFirst])
    tmpTo <- c(to[fromIsFirst], from[toIsFirst])
    from <- tmpFrom
    to <- tmpTo
    list(from=from, to=to)
}

.edges_gbam <- function(object, which, direction="out")
{
    nn <- nodes(object)
    if (numEdges(object) == 0L) {
        names(nn) <- nn
        c0 <- character(0L)
        return(lapply(nn, function(x) c0))
    }
    ft <- .Call(graph_bitarray_rowColPos, object@edgeSet@bit_vector)
    ft[] <- nn[ft]
    eL <- singles <- NULL
    if (isDirected(object)) {
        if (direction == "in")
            ft[ , c("from", "to")] <- ft[ , c("to", "from")]
        eL <- split(ft[ , "to"], ft[ , "from"])
        singles <- nn[!(nn %in% ft[ , "from"])]
    } else {
        eL <- lapply(split(ft, ft[ , c("to", "from")]), unique)
        singles <- nn[!(nn %in% ft)]
    }
    if (length(singles) > 0) {
        names(singles) <- singles
        c0 <- character(0L)
        empties <- lapply(singles, function(x) c0)
        eL <- c(eL, empties)
    }
    eL[order(names(eL))]
}

setMethod("inEdges", signature("character", "graphBAM"),
          function(node, object) {
              .edges_gbam(object, direction = "in")[node]
          })

setMethod("edges", signature("graphBAM", "missing"), .edges_gbam)

setMethod("edges", signature("graphBAM", "character"),
        function(object, which) {
            ## TODO: refactor to optimize
            .edges_gbam(object)[which]
        })

setMethod("adj", c("graphBAM", "character"),
          function(object, index) edges(object, index))

getWeightList2 <- function(g){
    nodeNames <- g@nodes
    numNodes <- length(nodeNames)
    w <- g@edgeSet@weights
    ft <- .Call(graph_bitarray_rowColPos, g@edgeSet@bit_vector)
    if(!isDirected(g)){
        ft <- rbind(ft, ft[ , c(2L, 1L)])
        w <- c(w,w)
    }
    ft[] <- nodeNames[ft]
    wList <- split(w, ft[ , 1L])
    wNameList <- split(ft[ , 2L], ft[ , 1L])
    wList <- mapply(function(wVals, wNames) {
        names(wVals) <- wNames
        a <- wVals[order(wNames)]
        if (!isDirected(g)) a[!duplicated(names(a))] else a
    }, wList, wNameList)
    haveNoEdge <- setdiff(nodeNames, names(wList))
    names(haveNoEdge) <- haveNoEdge
    n0 <- numeric(0)
    haveNoEdge <- lapply(haveNoEdge, function(x) n0)
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
               getWeightList2(object)[index]
          })


setMethod("edgeWeights", signature(object="graphBAM", index="numeric"),
          function(object, index, attr, default, type.checker)
          {
              edgeWeights(object, nodes(object)[index], attr=attr,
                          default=default, type.checker=type.checker)
          })


setMethod("edgeWeights", signature(object="graphBAM", index="missing"),
          function(object, index, attr, default, type.checker)
          {
              edgeWeights(object, nodes(object), attr=attr, default=default,
                          type.checker=type.checker)
          })


setMethod("edgeData", signature(self="graphBAM", from="missing", to="missing",
                  attr="missing"),
         function(self, from, to, attr) {
              nodeNames <- self@nodes
              numNodes <- length(nodeNames)
              bv <- self@edgeSet@bit_vector
              ft <- .Call(graph_bitarray_rowColPos, bv)
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
                ft <- .Call(graph_bitarray_rowColPos, self@edgeSet@bit_vector)
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
                ft <- .Call(graph_bitarray_rowColPos, self@edgeSet@bit_vector)
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

.align_from_to <- function(from, to, nodeNames)
{
    from_len <- length(from)
    to_len <- length(to)
    req_nn <- unique(c(from, to))
    if (!all(okidx <- req_nn %in% nodeNames))
        stop("unknown nodes: ", paste(req_nn[!okidx], collapse = ", "))
    if (from_len != to_len) {
        if (from_len == 1L)
            from <- rep(from, to_len)
        else if (to_len == 1L)
            to <- rep(to, from_len)
        else
            stop("invalid lengths of 'from' and 'to'")
    }
    cbind(from=from, to=to)
}

.set_weights <- function(g, from, to, value)
{
    nodeNames <- g@nodes
    req_ft <- .align_from_to(from, to, nodeNames)
    ## remove dups
    req_ft <- req_ft[!duplicated(req_ft), , drop = FALSE]
    if (length(value) == 1L)
        value <- rep(value, nrow(req_ft))
    else if (length(value) != nrow(req_ft))
        stop("number of edges and weight values must align")
    ft <- .Call(graph_bitarray_rowColPos, g@edgeSet@bit_vector)
    if (!isDirected(g)) {
        ## normalize from/to
        tmp <- .mg_undirectEdges(req_ft[ , 1], req_ft[, 2], value)
        req_ft <- cbind(tmp[["from"]], tmp[["to"]])
        value <- tmp[["weight"]]
    }
    ## convert node names to index
    req_i <- structure(match(req_ft, nodeNames), dim = dim(req_ft))
    tmp <- rbind(req_i, ft)
    idx <- duplicated(tmp)[seq(nrow(req_i) + 1L, nrow(tmp))]
    g@edgeSet@weights[idx] <- value
    g
}

setReplaceMethod("edgeData",
                 signature(self="graphBAM",
                           from="character", to="character",
                           attr="character", value="numeric"),
                 function(self, from, to, attr, value) {
                     if (attr != "weight")
                         stop("operation only supported for attr = \"weight\"")
                     .set_weights(self, from, to, value)
                 })

setMethod("numNodes", signature("graphBAM"),
        function(object) length(object@nodes))


setMethod("isAdjacent",
        signature(object="graphBAM", from="character", to="character"),
        function(object, from, to) {
            nodeNames <- object@nodes
            req_ft <- .align_from_to(from, to, nodeNames)
            if (!isDirected(object)) {
                ## normalize edges so that edges have nodes in lexical order
                tmp <- .undirectEdges(req_ft[ , "from"], req_ft[ , "to"])
                req_ft[ , "from"] <- tmp[["from"]]
                req_ft[ , "to"] <- tmp[["to"]]
            }
            from_i <- match(req_ft[ , "from"], nodeNames)
            to_i <- match(req_ft[ , "to"], nodeNames)
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
            df <- .Call(graph_bitarray_rowColPos, bitvec)
            t(df)
        })

setMethod("clearNode",
        signature(node="character", object="graphBAM"),
        function(node, object) {
            stop("operation not supported")
        })

setMethod("removeNode",
        signature(node="character", object="graphBAM"),
        function(node, object) {
            stop("operation not supported")
        })

setMethod("extractFromTo", "graphBAM",
          function(g) {
              diEdgeSetToDataFrame(g@edgeSet, nodes(g))
          })

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
     nn <- nodes(object)
     bam <- graphBAM(df, nodes = nn, edgemode = edgemode(object),
                     ignore_dup_edges = TRUE)
     bam@renderInfo <- object@renderInfo
     bam@graphData <- object@graphData
     ## FIXME: graphBAM doesn't really handle edge attributes in the same way
     ## we can copy data over so it can be copied back, but it won't really
     ## be accessible in the new graphBAM object.
     bam@edgeData <- object@edgeData
     bam@nodeData <- object@nodeData
     bam
}

setAs(from="graphNEL", to="graphBAM",
        function(from) as(as(from, "graphAM"), "graphBAM"))

setAs(from="graphAM", to="graphBAM",
        function(from) graphToBAM(from))

setMethod("ugraph", "graphBAM",
          function(graph) {
              graph@graphData$edgemode <- "undirected"
              graph@renderInfo <- new("renderInfo")
              graph@edgeSet <- ugraph(graph@edgeSet)
              graph
          })

setMethod("addEdge",
          signature=c("character", "character", "graphBAM", "numeric"),
          function(from, to, graph, weights) {
              nn <- nodes(graph)
              req_ft <- .align_from_to(from, to, nn)
              df <- extractFromTo(graph)
              df2 <- data.frame(from=req_ft[ , 1], to=req_ft[ , 2],
                                weight=weights, stringsAsFactors = FALSE)
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
            df <- extractFromTo(object)
            graphBAM(df, nodes = node, edgemode = edgemode(object))
        })

setReplaceMethod("edgemode", c("graphBAM", "character"),
                 function(object, value) {
                     if (length(value) != 1L) stop("edgemode must be length one")
                     if (edgemode(object) == value) return(object)
                     switch(value,
                            "directed" = {
                                ## add reciprocal edges
                                df <- extractFromTo(object)
                                es <- rbind(df,
                                            data.frame(from=df$to, to=df$from,
                                                       weight=df$weight,
                                                       stringsAsFactors=FALSE))
                                object <- graphBAM(es, nodes = nodes(object),
                                                   edgemode = value)
                            },
                            "undirected" = {
                                object <- ugraph(object)
                            },
                            stop(paste("supplied mode is", value,
                                       "it must be either directed or undirected")))
                     object
                 })

.remEdge <- function(from, to, graph)
{
    nn <- nodes(graph)
    req_ft <- .align_from_to(from, to, nn)
    ft <- .Call(graph_bitarray_rowColPos, graph@edgeSet@bit_vector)
    all_f <- nn[ft[ , 1]]
    all_t <- nn[ft[ , 2]]
    wh <- !((all_f %in% req_ft[ , 1]) & (all_t %in% req_ft[ , 2]))
    new_weights <- graph@edgeSet@weights[wh]
    graph@edgeSet@bit_vector <- setBitCell(graph@edgeSet@bit_vector,
                                           match(from, nn),
                                           match(to, nn),
                                           rep(0L, nrow(req_ft)))
    graph@edgeSet@weights <- new_weights
    graph
}

setMethod("removeEdge", c("character", "character", "graphBAM"),
          function(from, to, graph) .remEdge(from, to, graph))


setReplaceMethod("nodes", c("graphBAM", "character"),
                 function(object, value) {
                     stop("operation not supported")
                 })

setMethod("intersection", c("graphBAM", "graphBAM"),
        function(x, y) {
    nn <- intersect(nodes(x), nodes(y))
    nnLen <- length(nn)
    dr1 <- isDirected(x)
    dr2 <- isDirected(y)
    theMode <- if (dr1 && dr2) "directed" else "undirected"
    c0 <- character(0)
    df <- data.frame(from = c0, to = c0, weight = numeric(0))
    ans <- graphBAM(df, edgemode = theMode)
    if (nnLen == 0) return(ans)
    sg1 <- if (nnLen == numNodes(x)) x else subGraph(nn, x)
    sg2 <- if (nnLen == numNodes(y)) y else subGraph(nn, y)
    if (!(dr1 && dr2)) {
        if (dr1) sg1 <- ugraph(sg1) else sg2 <- ugraph(sg2)
    }
    bv <- sg1@edgeSet@bit_vector & sg2@edgeSet@bit_vector
    attributes(bv) <- attributes(sg1@edgeSet@bit_vector)
    attr(bv, "nbitset") <- ns <- .Call(graph_bitarray_sum, bv)
    ans@edgeSet@edge_attrs <- list()
    ans@edgeSet@bit_vector <- bv
    ans@edgeSet@weights <- rep(1L, ns)
    ans@nodes <- nn
    ans
})

setMethod("union", c("graphBAM", "graphBAM"), function(x, y) {
    dr1 <- isDirected(x)
    dr2 <- isDirected(y)
    theMode <- if (dr1 && dr2) "directed" else "undirected"
    theNodes <- unique(c(nodes(x), nodes(y)))
    df <- rbind(extractFromTo(x), extractFromTo(y))
    df[["weight"]] <- 1L
    graphBAM(df, nodes = theNodes, edgemode = theMode, ignore_dup_edges = TRUE)
})

