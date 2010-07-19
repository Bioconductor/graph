MultiGraph <- function(edgeSets, nodes = NULL, directed = TRUE, 
        ignore_dup_edges = FALSE)
{
    .mg_validate_edgeSet(edgeSets)
    nodeNames <- .mg_node_names(edgeSets, nodes)
    n_nodes <- length(nodeNames)
    edge_sets <- makeMDEdgeSets(edgeSets, directed, nodeNames,
            ignore_dup_edges = ignore_dup_edges)
    new("MultiGraph", edge_sets = edge_sets, nodes = nodeNames)
}

makeMDEdgeSets <- function(edgeSets, directed, nodes, ignore_dup_edges = FALSE)
{
    directed <- if (length(directed) == 1L)
        rep(directed, length(edgeSets))
    else if (length(directed) != length(edgeSets))
        stop("'directed' must align with 'edgeSets' or have length one",
             call. = FALSE)
    else
        directed

    ans <- vector(mode = "list", length = length(edgeSets))
    nms <- names(edgeSets)
    names(ans) <- nms
    for (i in seq_along(edgeSets)) {
        ans[[i]] <- .makeMDEdgeSet(nms[[i]], edgeSets[[i]], directed[[i]], nodes,
                ignore_dup_edges = ignore_dup_edges) 
    }
    ans
}

.makeMDEdgeSet <- function(es_name, es, is_directed, nodes,
                           ignore_dup_edges = FALSE)
{ 
    if (!all(c("from", "to", "weight") %in% names(es)))
        stop("edgeSets data.frames must have columns: from, to, weight",
             call. = FALSE)
    n_nodes <- length(nodes)
    bitVect <- makebits(n_nodes * n_nodes, bitdim = c(n_nodes, n_nodes))
    weights <- numeric(0L)
    if (nrow(es) > 0L) {
        from <- as.character(es[["from"]])
        to <- as.character(es[["to"]])
        weights <- es[["weight"]]
        if (!is_directed) {
            tmp <- .mg_undirectEdges(from, to, weights)
            from <- tmp[["from"]]
            to <- tmp[["to"]]
            weights <- tmp[["weight"]]
        }
        ## map 'from', 'to' from character to integer indicies
        from_i <- match(from, nodes)
        to_i <- match(to, nodes)
        edge_order <- order(to_i, from_i)
        weights <- weights[edge_order]
        if (!is.numeric(weights))
            stop("'weight' column must be numeric", call. = FALSE)
        if (ignore_dup_edges) {
            ## NB: we only consider nodes for duplication, ignoring
            ## weight value.
            ft <- cbind(from_i, to_i)[edge_order, ]
            tmp <- paste(ft[,"from_i"],ft[,"to_i"], sep="_")
            want <- !duplicated(tmp)
            from_i <- ft[want, 1]
            to_i <- ft[want, 2]
            weights <- weights[want]
        } else {
            from_i <- from_i[edge_order]
            to_i <- to_i[edge_order]
        }
        ## TODO: should not have to pass vector of 1s for each edge in
        ## setBitCell.
        bitVect <- setBitCell(bitVect, from_i, to_i, rep(1L, length(from_i)))
        edge_count <- nbitset(bitVect)
        if (length(from_i) != edge_count)
            .report_duplicate_edges(es_name, from, to, is_directed)
    }
    klass <- if (is_directed) "DiEdgeSet" else "UEdgeSet"
    ## FIXME: need to handle extra edge attributes.  These will need to
    ## come in as a separate argument as a list of attribute lists that
    ## align with from/to
    new(klass, bit_vector = bitVect, weights = weights,
        edge_attrs = list())
}

.report_duplicate_edges <- function(name, from, to, directed)
{
    df <- cbind(from, to)
    sep <- if (directed) "=>" else "="
    if (any(dups <- duplicated(df))) {
        stop("duplicate edges specified in edge set ", name, ": ",
             paste(from[dups], to[dups], sep = sep, collapse = ", "),
             call. = FALSE)
    }
}

.mg_undirectEdges <- function(from, to, weight)
{
    fromIsFirst <- from < to
    toIsFirst <- !fromIsFirst
    tmpFrom <- c(from[fromIsFirst], to[toIsFirst])
    tmpTo <- c(to[fromIsFirst], from[toIsFirst])
    tmpW <- c(weight[fromIsFirst], weight[toIsFirst])
    list(from = tmpFrom, to = tmpTo, weight = tmpW)
}

.mg_validate_node_names <- function(nodeNames)
{
    if (!all(valid <- .mg_valid_node_names(nodeNames))) {
        stop(sum(!valid), " invalid node names: ",
             paste("'", head(nodeNames[!valid], 10L), "'", sep="",
                   collapse=", "), call. = FALSE)
    }
}

.mg_node_names <- function(edgeSets, nodes)
{
    ## XXX: we sort the node names and are thus subject
    ## to locale variation
    ftSets <- lapply(edgeSets,
                     function(ft) {
                         cbind(from = as.character(ft[["from"]]),
                               to = as.character(ft[["to"]]))
                     })
    nodeNames <- unique(c(unlist(ftSets, use.names = FALSE), nodes))
    if (is.null(nodeNames) || length(nodeNames) == 0L)
        nodeNames <- character(0)
    else {
        nodeNames <- sort(nodeNames, na.last = FALSE)
        .mg_validate_node_names(nodeNames)
    }
    nodeNames
}

.mg_validate_edgeSet <- function(edgeSets)
{
    if (!is.list(edgeSets))
        stop("'edgeSets' must be a named list or empty list", call. = FALSE)
    if (length(edgeSets) > 0L) {
        nms <- names(edgeSets)
        if (is.null(nms))
            stop("'edgeSets' must be a named list", call. = FALSE)
        if (!all(nzchar(nms)) || any(is.na(nms)) || any(duplicated(nms)))
            stop("names(edgeSets) is invalid", call. = FALSE)
    }
}

MultiDiGraph <- function(edgeSets, nodes = NULL)
{
    ## Nodes are stored in sorted order.  The sparse Edge index vector
    ## is stored in sorted index order.  Since R is column-major, this
    ## means that when translated to x, y coordinates, the ordering is
    ## sorted by column and then row.

    ## edgeSets is a list of from/to matrices
    ftSets <- lapply(edgeSets,
                       function(ft) {
                           cbind(from = as.character(ft[[1L]]),
                                 to = as.character(ft[[2L]]))
                       })
    nodeNames <-
      sort(unique(c(unlist(ftSets, use.names = FALSE), nodes)),
           na.last = FALSE)
    if (!all(valid <- .mg_valid_node_names(nodeNames))) {
        stop("invalid node names: ",
             paste("'", head(nodeNames[!valid], 10L), "'", sep="",
                   collapse=", "))
    }
    n_nodes <- length(nodeNames)
    if (n_nodes > 2^15) n_nodes <- as.double(n_nodes)

    n_edgeSets <- length(edgeSets)
    es_names <- names(edgeSets)
    edgeAttrs <- structure(vector("list", n_edgeSets),
                           names = es_names)
    ## FIXME: should we enforce having a "weight" column?
    for (i in seq_len(n_edgeSets)) {
        ft <- ftSets[[i]]
        from_i <- match(ft[, 1L], nodeNames)
        to_i <- match(ft[, 2L], nodeNames)

        sparseAM <- .coordToIndex(from_i, to_i, n_nodes)
        edgeIdxOrder <- order(sparseAM)
        esi <- edgeSets[[i]]
        edgeAttrs[[i]] <-
            structure(data.frame(mdg_edge_index = sparseAM[edgeIdxOrder],
                                 esi[edgeIdxOrder, c(-1L, -2L)],
                                 row.names = NULL),
                      names = c("mdg_edge_index", names(esi)[-(1:2)]))
    }

    new("MultiDiGraph", nodes = nodeNames, edgeAttrs = edgeAttrs)
}

.mg_valid_node_names <- function(names)
{
    !(is.na(names) | (sapply(names, nchar) == 0) |
      (regexpr("\\||\n|\t", names) > 0))
}

setMethod("numNodes", signature = signature(object = "MultiGraph"),
          function(object) {
              length(object@nodes)
          })

setMethod("numEdges", signature = signature(object = "MultiGraph"),
          function(object) {
              ## TODO: would it make more sense to just
              ## return the length of @weights?
              sapply(object@edge_sets, function(es) {
                  nbitset(es@bit_vector)
              })
          })

setMethod("nodes", signature = signature(object = "MultiGraph"),
          function(object, ...) {
              object@nodes
          })

setMethod("isDirected", signature = signature(object = "MultiGraph"),
          function(object) sapply(object@edge_sets, isDirected))

setMethod("isDirected", signature = signature(object = "DiEdgeSet"),
          function(object) TRUE)

setMethod("isDirected", signature = signature(object = "UEdgeSet"),
          function(object) FALSE)

setMethod("ugraph", signature = signature(graph = "MultiGraph"),
          function(graph) {
              graph@edge_sets <- lapply(graph@edge_sets, ugraph)
              graph
          })

## FIXME: should ugraph on an undirected graph also drop
## attributes to keep things consistent?
setMethod("ugraph", "UEdgeSet",
          function(graph) {
              new("UEdgeSet", bit_vector = graph@bit_vector,
                  weights = rep(1L, length(graph@weights)),
                  edge_attrs = list())
          })

setMethod("ugraph", "DiEdgeSet",
          function(graph) {
              ## XXX: edge weights => 1, edge attributes dropped
              bit_vector <- .Call(graph_bitarray_undirect, graph@bit_vector)
              new("UEdgeSet", bit_vector = bit_vector,
                  weights = rep(1L, nbitset(bit_vector)),
                  edge_attrs = list())
          })

setMethod("show",  signature = signature(object = "MultiGraph"),
          function(object) {
              cat(class(object),
                  sprintf("with %d nodes and %d edge sets\n",
                          numNodes(object), length(object@edge_sets)))
              if (length(object@edge_sets)) {
                  edgeCounts <- numEdges(object)
                  df <- data.frame(edge_set = names(edgeCounts),
                                   directed = sapply(object@edge_sets, isDirected),
                                   edge_count = edgeCounts,
                                   stringsAsFactors = FALSE,
                                   row.names = NULL)
                  print(head(df, 10L), row.names = FALSE)
              }
              invisible(NULL)
          })

eweights <- function(object, names.sep = NULL)
{
    if (is.null(names.sep)) {
        lapply(object@edge_sets, function(es) {
            es@weights
        })
    } else {
        sep <- names.sep[1]
        nn <- nodes(object)
        n_nodes <- length(object@nodes)
        lapply(object@edge_sets, function(es) {
            w <- es@weights
            ft <- .Call(graph_bitarray_rowColPos, es@bit_vector)
            names(w)  <- paste(nn[ft[, "from"]], nn[ft[, "to"]], sep = names.sep)
            w
        })
    }
}

edgeMatrices <- function(object)
{
    n_nodes <- length(object@nodes)
    lapply(object@edgeAttrs, function(attrs) {
        matrix(t(.indexToCoord(attrs[[1L]], n_nodes)),
               nrow = 2L,
               dimnames = list(c("from", "to"), NULL))
    })
}

fromToList <- function(object)
{
    nodeNames <- object@nodes
    n_nodes <- length(nodeNames)
    lapply(object@edgeAttrs, function(attrs) {
        coord <- .indexToCoord(attrs[[1L]], n_nodes)
        coord[] <- nodeNames[coord]
        ft <- structure(data.frame(from = coord[ , 1L], to = coord[ , 2L],
                         attrs[-1L], stringsAsFactors = FALSE),
                        names = c("from", "to", names(attrs)[-1L]))
        ft
    })
}

subsetEdgeSets <- function(object, expr)
{
    new("MultiGraph", nodes = object@nodes,
        edgeAttrs = object@edgeAttrs[expr])
}

extractGraph <- function(object, which)
{
    if (length(which) != 1L)
        stop("'which' must be length one")
    edgeAttr <- object@edgeAttrs[[which]]
    nodeNames <- nodes(object)
    ftmat <- .indexToCoord(edgeAttr[[1L]], length(nodeNames))
    ftmat[] <- nodeNames[ftmat]
    ftM2graphNEL(ftmat, W = edgeAttr[[2L]], V = nodeNames,
                 edgemode = "directed")
}

## Generate a random from/to table
##
## Returns a list with two elements:
##   $node: character vector of node labels
##   $ft: a data.frame with columns 'from', 'to', and 'weight'
##
randFromTo <- function(numNodes, numEdges, weightFun = function(N) rep(1L, N),
                       directed = TRUE)
{
    if (numNodes > 2^15) numNodes <- as.double(numNodes)
    maxEdges <- numNodes * numNodes
    nodeNames <- sprintf("%010d", seq_len(numNodes))
    ## use double to allow for large numNodes
    numToGen <- max(numEdges * 4, numNodes)
    idx <- unique(ceiling(runif(numToGen, min = 0, max = maxEdges)))
    stopifnot(length(idx) >= numEdges)
    idx <- idx[seq_len(numEdges)]
    to_i <- ((idx - 1L) %/% numNodes) + 1L
    from_i <- ((idx - 1L) %% numNodes) + 1L
    from <- nodeNames[from_i]
    to <- nodeNames[to_i]
    w <- weightFun(length(from))
    list(nodes = nodeNames,
         ft = data.frame(from = from, to = to, weight = w,
                         stringsAsFactors = FALSE))
}

oneWeights <- function(x) rep(1L, nrow(x))

edgeSetIntersect0 <- function(g)
{
    edge_sets <- g@edge_sets
    n_sets <- length(edge_sets)
    if (n_sets < 2L) return(g)

    all_directed <- all(isDirected(g))
    if (!all_directed) edge_sets <- lapply(edge_sets, ugraph)
    klass <- if (all_directed) "DiEdgeSet" else "UEdgeSet"

    bv <- edge_sets[[1L]]@bit_vector
    keepAttrs <- attributes(bv)
    for (i in seq.int(2L, n_sets)) {
        bv <- bv & edge_sets[[i]]@bit_vector
    }
    attributes(bv) <- keepAttrs
    n_edges <- attr(bv, "nbitset") <- .Call(graph_bitarray_sum, bv)
    if (n_edges > 0) {
        new_edge_sets <- list(new(klass, bit_vector = bv,
                                  weights = rep(1L, n_edges),
                                  edge_attrs = list()))
        names(new_edge_sets) <- paste(names(edge_sets), collapse = "_")
    } else {
        new_edge_sets <- list()
    }
    new("MultiGraph", edge_sets = new_edge_sets, nodes = nodes(g))
}

edgeUnion <- function(object, weightFun = NULL)
{
    edgeAttrs <- object@edgeAttrs
    uAttrs <- unlist(lapply(edgeAttrs, function(x) x[[1L]]))
    ## to deal with weights properly I guess we will need
    ## to compute the intersection.
    dups <- duplicated(uAttrs)
    uAttrs <- sort(uAttrs[!dups])
    newWeights <- if (is.null(weightFun)) {
        rep(1L, length(uAttrs))
    } else {
        wList <- lapply(edgeAttrs, function(ea) {
            idx <- match(uAttrs, ea[[1L]])
            ea[[2L]][idx]
        })
        wMat <- do.call(cbind, wList)
        weightFun(wMat)
    }
    object@edgeAttrs <- list(data.frame(mdg_edge_index = uAttrs,
                                        weight = newWeights,
                                        stringsAsFactors = FALSE))
    object
}

## TODO: should you be allowed to rename edge sets?
## or at least name unnamed edge sets?

subsetEdgeSets <- function(object, edgeSets) {
    if (!all(nzchar(edgeSets)) || any(is.na(edgeSets)) ||
        !(all(edgeSets %in% names(object@edge_sets))))
        stop("edgeSet specified is invalid")
    if(any(dups <- duplicated(edgeSets)))
        stop("duplicate edges specified in edge set ", edgeSets[dups])
    object@edge_sets<- object@edge_sets[edgeSets]
    object
}

diEdgeSetToDataFrame <- function(edgeSets,nodes) {
    bitvec <- edgeSets@bit_vector
    df <- .Call(graph_bitarray_rowColPos, bitvec)
    data.frame(from = nodes[df[, "from"]], to = nodes[df[, "to"]],
               weight = edgeSets@weights)
}

.extractFromTo_mg <- function(g) {
    nn <- nodes(g)
    lapply(g@edge_sets, function(x) diEdgeSetToDataFrame(x, nn))
}
setMethod("extractFromTo", "MultiGraph", .extractFromTo_mg)

.mgDegree <- function(object) {
    nn <- nodes(object)
    len <- length(nn)
    idx_str <- as.character(seq_len(len))
    lapply(object@edge_sets, function(edgeSet) {
        bitvec <- edgeSet@bit_vector
        df <- .Call(graph_bitarray_rowColPos, bitvec)

        tbl <- structure(table(df[, "from"]), class=NULL)
        indx <- idx_str %in% names(tbl)
        from <- to <- structure(rep(0, len), names=nn)
        from[indx] <- tbl

        tbl <- structure(table(df[, "to"]), class=NULL)
        indx <- idx_str %in% names(tbl)
        to[indx] <- tbl

        if (isDirected(edgeSet)) {
            list(inDegree = to, outDegree = from)
        } else {
            degree = from + to
        }
    })
}

setMethod("subGraph", signature(snodes="character", graph="MultiGraph"),
         function(snodes, graph) {
    origNodes <- nodes(graph)
    snodes <- sort(snodes)
    snodesIdx <- match(snodes, origNodes)
    if (any(is.na(snodesIdx))) {
        bad <- snodes[which(is.na(snodesIdx))]
        stop("invalid arg: snodes contains nodes not in the ",
              "MultiGraph:\n", paste(bad, collapse=", "))
    }
    graph@edge_sets <-  lapply(graph@edge_sets, function(x){
                res <- .Call("graph_bitarray_subGraph", x@bit_vector, snodesIdx)
                x@bit_vector <- res$bitVec
                x@weights <- x@weights[res$setPos]
                if(length(x@edge_attrs)){
                    #  x@edge_attrs <- x@edge_attrs[res$setPos]
                    x@edge_attrs <- lapply(x@edge_attrs, 
                        function(y) {
                            y[res$setPos]
                        })
                }
                x
            })
    graph@nodeData <- lapply(graph@nodeData, function(x) {
                         x[snodesIdx]
                      })
    graph@nodes <- snodes
    graph
})

extractGraphAM <- function(g, edgeSets) {
    nds <- nodes(g)
    drct <- isDirected(g)
    ## FIXME: validate that edgeSets is a valid subset as is
    ## done in subsetEdgeSets
    esets <- if (missing(edgeSets)) g@edge_sets else g@edge_sets[edgeSets]
    nms <- names(esets)
    names(nms) <- nms
    lapply(nms, function(x) {
        mat <- edgeSetToMatrix(nds, esets[[x]], drct[[x]])
        new("graphAM", adjMat=mat,
            edgemode = if(drct[[x]]) "directed" else "undirected",
            values= list(weight=1))
    })
}

edgeSetToMatrix <- function(nds, edgeSet, directed)
{
    .Call(graph_bitarray_edgeSetToMatrix,
          nds, edgeSet@bit_vector,
          as.numeric(edgeSet@weights), as.logical(directed))
}

edgeIntersect <- function(e1, e2) {

    dr1 <- isDirected(e1)
    dr2 <- isDirected(e2)
    if (! (dr1 && dr2)) {
        if(dr1) e1 <- ugraph(e1)
        else    e2 <- ugraph(e2)
    }
    klass <- if (dr1 && dr2) "DiEdgeSet" else "UEdgeSet"
    bv <- e1@bit_vector & e2@bit_vector
    attributes(bv) <- attributes(e1@bit_vector)
    attr(bv, "nbitset") <- ns <- .Call(graph_bitarray_sum, bv)
    new_edge_set <- new(klass, bit_vector = bv,
                                  weights = rep(1L, ns),
                                  edge_attrs = list())
    new_edge_set
}
  
setMethod("intersection", c("MultiGraph", "MultiGraph"),
        function(x, y) {

    nn <- intersect(nodes(x), nodes(y))
    nnLen <- length(nn)

    nmsX <- names(x@edge_sets)
    nmsY <- names(y@edge_sets)
    eg <- intersect(nmsX, nmsY)
    x@edge_sets <- x@edge_sets[eg]
    y@edge_sets <- y@edge_sets[eg]

    sgx <- if (nnLen == numNodes(x)) x else subGraph(nn, x)
    sgy <- if (nnLen == numNodes(y)) y else subGraph(nn, y)
  
    new_edge_sets <- lapply(eg, function(k) {
                        edgeIntersect(sgx@edge_sets[[k]], sgy@edge_sets[[k]])         
                     })
    names(new_edge_sets) <- eg    
    new("MultiGraph", edge_sets = new_edge_sets, nodes = nn)
})

setMethod("union", c("MultiGraph", "MultiGraph"), function(x, y) {
    
    theNodes <- unique(c(nodes(x), nodes(y)))
    dfx <- .extractFromTo_mg(x)
    dfy <- .extractFromTo_mg(y)
    theEdgeSets <- unique(c(names(dfx), names(dfy)))
    dr1 <- isDirected(x)
    dr2 <- isDirected(y)
    
    ft <- lapply(theEdgeSets, function(k) {
        df <- rbind(dfx[[k]], dfy[[k]])
        if(nrow(df) > 0)
            df[["weight"]] <- 1L
        df
    })
    names(ft) <- theEdgeSets
    theMode <- structure(rep(FALSE,length(theEdgeSets)), names = theEdgeSets)
    cmn <- intersect(names(dr1), names(dr2))
    rest <- theEdgeSets[!theEdgeSets %in% cmn]
    theMode[names(which(dr1[names(dr1) %in% rest]))] <- TRUE
    theMode[names(which(dr2[names(dr2) %in% rest]))] <- TRUE
    theMode[cmn] <- dr1[cmn] & dr2[cmn]
    MultiGraph(edgeSets = ft, nodes = theNodes, directed = theMode,
            ignore_dup_edges = TRUE)
})

extractGraphBAM <- function(g, edgeSets) {
    nn <- nodes(g)
    esets <- if (missing(edgeSets)) g@edge_sets else g@edge_sets[edgeSets]
    df_empty <- data.frame(from = character(0), to = character(0),
            weight = numeric(0))
    lapply(esets, function(x) {
                edgeMode <- if(isDirected(x)) "directed" else "undirected"
                bam <- graphBAM(df_empty, nodes = nn, edgemode = edgeMode,
                        ignore_dup_edges = TRUE)
                bam@edgeSet <- x
                bam
            })
}


## Degree of a multigraph
setMethod("degree", signature(object = "MultiGraph", Nodes = "missing"),
      function(object){
          .mgDegree(object)
})

## Node data accces methods 
setMethod("nodeData",
        signature(self = "MultiGraph", n = "character", attr = "character"),
        function(self, n, attr) {
            if(length(attr) != 1L)
                stop("attr argument must specify a single attribute name")
            if( ! (attr %in% names(self@nodeData)))
                stop(paste("attribute", attr," is not present in self"))

            nds <- nodes(self)
            .verifyNodes(n, nds)
            idx <- match(n, nds)
            as.list(structure(self@nodeData[[attr]][idx], names = n))
        })

setMethod("nodeData",
        signature(self = "MultiGraph", n = "missing", attr = "character"),
        function(self, n, attr) {
            if(length(attr) != 1L)
                stop("attr argument must specify a single attribute name")
            if( ! (attr %in% names(self@nodeData)))
                stop(paste("attribute", attr," is not present in self"))
            nds <- nodes(self)
            as.list(structure(self@nodeData[[attr]], names = nds))
        })

setMethod("nodeData",
        signature(self = "MultiGraph", n = "character", attr = "missing"),
        function(self, n, attr) {

            nds <- nodes(self)
            .verifyNodes(n ,nds)
            idx <- match(n, nds)
            if(!any(idx))
                stop("Specified node is not in self")
            lapply(self@nodeData, function(x) {
                        structure(x[idx], names = n)
                    })
        })

setMethod("nodeData",
        signature(self = "MultiGraph", n = "missing", attr = "missing"),
        function(self, n, attr) {
           nds <- nodes(self)
           lapply(self@nodeData, function(x) {
                    structure(x, names = nds)
                   })
        })

## Node data replacement methods 

setReplaceMethod("nodeData",
        signature(self = "MultiGraph", n="character", attr="character", value="ANY"),
        function(self, n, attr, value) {
            if(length(attr) != 1L)
                stop("attr argument must specify a single attribute name")
            if(is.vector(value)){
                len <- length(value)
            } else {
                len <- 1
                value <- list(value)
            }
            if(len!=1L && len != length(n)) {
                stop("value must be of length one or have the same length as n")
            }

            nms <- names(self@nodeData)
            nds <- nodes(self)
            .verifyNodes(n, nds)
            idx <- match(n, nds)
            if(!( attr %in% nms))
                self@nodeData[[attr]] <- rep(NA, length(nds))
            self@nodeData[[attr]][idx] <- value
            self 
        })

setReplaceMethod("nodeData",
        signature(self = "MultiGraph", n="missing", attr="character", value="ANY"),
        function(self, n, attr, value) {
            if(length(attr) != 1L)
                stop("attr argument must specify a single attribute name")
            
            lenNode <- length(nodes(self))
            if(is.vector(value)){
                lenVal <- length(value)
            } else {
                lenVal <- 1
                value <- list(value)
            }
            if(lenVal !=1L && lenVal != lenNode) {
                stop("value must be of length one or have the same length
                            as number of nodes of self")
            }
            idx <- seq_len(lenNode)
            self@nodeData[[attr]][idx] <- value
            self
        })

## edgeData access methods for MultiGraph 
.verifyMgEdgeSet <- function(mg, e) {
    if(!e %in% names(mg@edge_sets))
        stop( paste("edgeSet", e, "not found in self", sep = " "))
    if(numEdges(mg)[e] == 0)
        stop( paste("edgeSet", e, "does not have any connected edges", sep = " "))
}

.verifyMgEdges <- function(mg, e, from, to) {
    stopifnot(length(from) == length(to))
    if (length(from) == 0L) 
        stop("Edges specified not found in edgeSet:", e)
    adjList <- .mgIsAdj(mg, e, from, to)
    if (any(!adjList)) {
        badFr <- from[!adjList]
        badTo <- to[!adjList]
        res <- paste(badFr, badTo, sep = "|", collapse = ", ")
        stop("Edges not found: ", res)
    }
    TRUE
}
setMethod("mgEdgeData",
        signature(self = "MultiGraph", edgeSet = "character", from = "missing", 
                to = "missing", attr = "character"),
        function(self, edgeSet, from , to, attr) {
            .verifyMgEdgeSet(self,edgeSet)
            eg <- .edges_mg(self, edgeSet)             
            .mgGetAttrs( self, edgeSet, from = names(eg), attr)
        })

setMethod("mgEdgeData",
        signature(self = "MultiGraph", edgeSet = "character", from = "character", 
                to = "missing", attr = "character"),
        function(self, edgeSet, from , to, attr) {
            .verifyMgEdgeSet(self,edgeSet)
            .mgGetAttrs( self, edgeSet, from, attr)
        })

setMethod("mgEdgeData",
        signature(self = "MultiGraph", edgeSet = "character", from = "missing", 
                to = "character", attr = "character"),
        function(self, edgeSet, from , to, attr) {
            .verifyMgEdgeSet(self,edgeSet)
            nodeNames <- self@nodes
            numNodes <- length(nodeNames)
            bv <- self@edge_sets[[edgeSet]]@bit_vector
            if(attr == "weight")
                val <- self@edge_sets[[edgeSet]]@weights
            else 
                val <- self@edge_sets[[edgeSet]]@edge_attrs[[attr]]

            ft <- .Call(graph_bitarray_rowColPos, self@edge_sets[[edgeSet]]@bit_vector)
            if(!isDirected(self@edge_sets[[edgeSet]])) {
                df <- cbind(from=ft[,"to"], to = ft[,"from"])
                ft <- rbind(ft,df)
                val <- c(val,val)
            }
            indx <- seq_len(length(val))
            ft <- data.frame(ft, indx, stringsAsFactors = FALSE)
            ft <- ft[ft[,"to"] %in% which(nodeNames %in% to),]  ## was ==
            nodeLbl <- paste( nodeNames[ft[,"from"]], nodeNames[ft[, "to"]],
                    sep ="|")
            val <- val[ft[,"indx"]][1:length(nodeLbl)]
            names(val) <- nodeLbl
            as.list(val)
        })

setMethod("mgEdgeData",
        signature(self = "MultiGraph", edgeSet = "character", from = "character", 
                to = "character", attr = "character"),
        function(self, edgeSet, from , to, attr) {
            .verifyMgEdgeSet(self,edgeSet)
            nodeNames <- self@nodes
            req_ft <- .align_from_to(from, to, nodeNames)
             if(nrow(req_ft) > 0)
                 .verifyMgEdges(self, edgeSet, req_ft[,"from"], req_ft[,"to"])
             else 
                 stop(paste("Edges specified could not be found in edgeSet", edgeSet, sep = " "))

            numNodes <- length(nodeNames)
            bv <- self@edge_sets[[edgeSet]]@bit_vector
            if(attr == "weight")
               val <- self@edge_sets[[edgeSet]]@weights
            else
               val <- self@edge_sets[[edgeSet]]@edge_attrs[[attr]]

            ft <- .Call(graph_bitarray_rowColPos, self@edge_sets[[edgeSet]]@bit_vector)
            if(!isDirected(self@edge_sets[[edgeSet]])) {
                df <- cbind(from=ft[,"to"], to = ft[,"from"])
                ft <- rbind(ft,df)
                val <- c(val, val)
            }
            indx <- seq_len(length(val))
            ft <- data.frame(ft, indx, stringsAsFactors = FALSE)
            df_list <- lapply( seq_len( nrow(req_ft)), function(x) {
                        fIndx <- which(nodeNames %in% req_ft[,"from"][x])
                        tIndx <- which(nodeNames %in% req_ft[,"to"][x])
                        ft[ ft[, "from"] == fIndx & ft[, "to"] == tIndx,]
                    })
            ft <- do.call(rbind, df_list)

            nodeLbl <- paste( nodeNames[ft[,"from"]], nodeNames[ft[, "to"]],
                    sep ="|")
            val <- val[ft[,"indx"]][1:length(nodeLbl)]
            names(val) <- nodeLbl
            as.list(val)
        })

setReplaceMethod("mgEdgeData",
        signature(self = "MultiGraph", edgeSet = "character", from = "character", 
                to = "character", attr = "character", value = "ANY"),
        function(self, edgeSet, from, to, attr, value) { 
            .verifyMgEdgeSet(self,edgeSet)
            if(length(edgeSet) != 1L)
                stop("edgeSet has to be of length 1")
            .mgSetAttrs(self, edgeSet, from, to, attr, value) 
        })


setReplaceMethod("mgEdgeData",
        signature(self="MultiGraph", edgeSet = "character", from = "character",
                to = "missing", attr="character", value = "ANY"),
        function(self, edgeSet, from, to, attr, value) {
            .verifyMgEdgeSet(self,edgeSet)
            eg <- .edges_mg(self, edgeSet)[from]
            to <- unlist(eg, use.names = FALSE)
            len <- as.numeric(sapply(eg, length))
            from <- rep(names(eg),len)
            .mgSetAttrs(self, edgeSet, from, to, attr, value)
        })


setReplaceMethod("mgEdgeData",
                 signature(self="MultiGraph", edgeSet = "character",
                           from="missing", to="character",
                           attr="character", value="ANY"),
                 function(self,edgeSet, from, to, attr, value) {
                     .verifyMgEdgeSet(self,edgeSet)
                     eg <- .edges_mg(self, edgeSet, direction = "in")
                     eg <- eg[order(names(eg))][to]
                     from  <- unlist(eg, use.names = FALSE) 
                     len <- as.numeric(sapply(eg, length))
                     to <- rep(names(eg), len)
                     .mgSetAttrs(self, edgeSet, from, to, attr, value)
                 })

setReplaceMethod("mgEdgeData",
                 signature(self="MultiGraph", edgeSet = "character",
                         from="missing", to="missing",
                         attr="character", value="ANY"),
                 function(self, edgeSet, from, to, attr, value) {
                     .verifyMgEdgeSet(self, edgeSet)
                     nn <- nodes(self)
                     df <- diEdgeSetToDataFrame(self@edge_sets[[edgeSet]], nn)
                     from <- nn[df[,"from"]]
                     to <- nn[df[,"to"]]
                     .mgSetAttrs(self, edgeSet, from, to, attr, value)
                 })

.edges_mg <- function(object, e, direction="out")
{
    nn <- nodes(object)
    if (numEdges(object)[e] == 0L) {
        names(nn) <- nn
        c0 <- character(0L)
        return(lapply(nn, function(x) c0))
    }
    ft <- .Call(graph_bitarray_rowColPos, object@edge_sets[[e]]@bit_vector)
    ft[] <- nn[ft]
    eL <- singles <- NULL
    if (isDirected(object@edge_sets[[e]])) {
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

.mgIsAdj <- function(object, e, from, to) {
    eSpec <- graph:::.normalizeEdges(from, to)
    from <- eSpec$from
    to <- eSpec$to
    fromIdx <- match(from, nodes(object), nomatch=0)
    toIdx <- match(to, nodes(object), nomatch=0)
    if (any(fromIdx == 0))
        stop("Unknown nodes in from: ",
            paste(from[fromIdx == 0], collapse=", "))
    if (any(toIdx == 0))
        stop("Unknown nodes in to: ",
            paste(to[toIdx == 0], collapse=", "))
    fromEdges <- .edges_mg(object, e)[from]
    .Call("graph_is_adjacent", fromEdges, to,
            PACKAGE="BioC_graph")
}

.mgSetAttrs <- function(mg, e, from, to, attr, value)
{   
    nodeNames <- mg@nodes
    req_ft <- .align_from_to(from, to, nodeNames)

    ## remove dups
    req_ft <- req_ft[!duplicated(req_ft), , drop = FALSE]
    if(nrow(req_ft) > 0)
        .verifyMgEdges(mg, e, req_ft[,"from"], req_ft[,"to"])
    else 
       stop(paste("Edges specified could not be found in edgeSet", e, sep = " "))

    if (length(value) == 1L) {
        value <- if(is.atomic(value)) rep(value, nrow(req_ft)) else  rep(list(value), nrow(req_ft))
    } else if (length(value) != nrow(req_ft))
    stop("number of edges and attribute values must align")
    ft <- .Call(graph_bitarray_rowColPos, mg@edge_sets[[e]]@bit_vector)
    if (!isDirected(mg@edge_sets[[e]])) {
        ## normalize from/to
        tmp <- .mg_undirectEdges(req_ft[ , 1], req_ft[, 2], value)
        req_ft <- cbind(tmp[["from"]], tmp[["to"]])
        value <- tmp[["weight"]]
    }
    ## convert node names to index
    req_i <- structure(match(req_ft, nodeNames), dim = dim(req_ft))
    value <- value[order(req_i[,1], req_i[,2])]
    tmp <- rbind(req_i, ft)
    idx <- duplicated(tmp)[seq(nrow(req_i) + 1L, nrow(tmp))]
    if(attr == "weight") {
         mg@edge_sets[[e]]@weights[idx] <- value
    } else {
         if(!(attr %in% names(mg@edge_sets[[e]]@edge_attrs)))
             mg@edge_sets[[e]]@edge_attrs[[attr]] <- 
                    rep(NA, attr(mg@edge_sets[[e]]@bit_vector, "nbitset"))
         mg@edge_sets[[e]]@edge_attrs[[attr]][idx] <- 
                if(is.atomic(value)) as.list(value) else list(value)
    }
    mg
}

.mgGetAttrs <- function(self, edge, from, attr) {
    
    nodeNames <- self@nodes
    indx <- which(nodeNames %in% from)
    numNodes <- length(nodeNames)
    bv <- self@edge_sets[[edge]]@bit_vector
    if(attr == "weight") {
        val <- self@edge_sets[[edge]]@weights
    } else { 
        if(attr %in% names(self@edge_sets[[edge]]@edge_attrs)) {
            val <- self@edge_sets[[edge]]@edge_attrs[[attr]]
        } else {
            stop(paste("Unknown attribute name: ", attr, sep="")) 
        }
    }
    ft <- .Call(graph_bitarray_rowColPos, bv)
    if(!isDirected(self@edge_sets[[edge]])) {
        df <- cbind(from=ft[,"to"], to = ft[,"from"])
        ft <- rbind(ft,df)
        val <- c(val,val)
    }
    tmp <- seq_len(length(val))
    ft <- data.frame(ft, tmp, stringsAsFactors = FALSE )
    ft <- ft[ ft[,"from"] %in% indx,]
    if(nrow(ft) >0 )
        .verifyMgEdges(self, edge, nodeNames[ft[,"from"]], nodeNames[ft[,"to"]])
    else 
        stop(paste("Edges specified in \"from\" could not be found in edgeSet", edge, sep = " "))

    nodeLbl <- paste( nodeNames[ft[,"from"]], nodeNames[ft[, "to"]],
            sep ="|")
    val <- val[ft[,"tmp"]][1:length(nodeLbl)]
    names(val) <- nodeLbl
    as.list(val)
}


