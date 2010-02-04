MultiGraph <- function(edgeSets, nodes = NULL, directed = TRUE)
{
    nodeNames <- .mg_node_names(edgeSets, nodes)
    n_nodes <- length(nodeNames)
    .mg_validate_edgeSet_names(edgeSets)
    edge_sets <- makeMDEdgeSets(edgeSets, directed, nodeNames)
    new("MultiGraph", edge_sets = edge_sets, nodes = nodeNames)
}

makeMDEdgeSets <- function(edgeSets, directed, nodes)
{
    directed <- if (length(directed) == 1L)
        rep(directed, length(edgeSets))
    else if (length(directed) != length(edgeSets))
        stop("'directed' must align with 'edgeSets' or have length one",
             call. = FALSE)
    else
        directed

    ans <- vector(mode = "list", length = length(edgeSets))
    for (i in seq_along(edgeSets)) {
        ans[[i]] <- .makeMDEdgeSet(edgeSets[[i]], directed[[i]], nodes)
    }
    names(ans) <- names(edgeSets)
    ans
}

.makeMDEdgeSet <- function(es, is_directed, nodes)
{
    if (!all(c("from", "to", "weight") %in% names(es)))
        stop("data.frame must have columns: from, to, weight",
             call. = FALSE)
    n_nodes <- length(nodes)
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
    bitVect <- makebits(n_nodes * n_nodes, bitdim = c(n_nodes, n_nodes))
    ## TODO: should not have to pass vector of 1s for each edge in
    ## setBitCell.
    bitVect <- setBitCell(bitVect, from_i[edge_order], to_i[edge_order],
                          rep(1L, length(from_i)))
    klass <- if (is_directed) "DiEdgeSet" else "UEdgeSet"
    ## FIXME: need to handle extra edge attributes.  These will need to
    ## come in as a separate argument as a list of attribute lists that
    ## align with from/to
    new(klass, bit_vector = bitVect, weights = weights, edge_attrs = list())
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
    nodeNames <-
        sort(unique(c(unlist(ftSets, use.names = FALSE), nodes)),
             na.last = FALSE)
    .mg_validate_node_names(nodeNames)
    nodeNames
}

.mg_validate_edgeSet_names <- function(edgeSets)
{
    nms <- names(edgeSets)
    if (is.null(nms))
        stop("'edgeSets' must be a named list", call. = FALSE)
    if (!all(nzchar(nms)) || any(is.na(nms)))
        stop("'edgeSets' has invalid names", call. = FALSE)
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
                  .Call(graph_bitarray_sum, es@bit_vector)
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
                  weights = rep(1L, .Call(graph_bitarray_sum, bit_vector)),
                  edge_attrs = list())
          })

setMethod("show",  signature = signature(object = "MultiGraph"),
          function(object) {
              cat(class(object),
                  sprintf("with %d nodes and %d edge sets\n",
                          numNodes(object), length(object@edge_sets)))
              edgeCounts <- numEdges(object)
              df <- data.frame(edge_set = names(edgeCounts),
                               directed = sapply(object@edge_sets, isDirected),
                               edge_count = edgeCounts,
                               stringsAsFactors = FALSE,
                               row.names = NULL)
              print(head(df, 10L), row.names = FALSE)
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
            edge_indices <- .Call(graph_bitarray_edge_indices, es@bit_vector)
            coord <- .indexToCoord(edge_indices, n_nodes)
            names(w) <- paste(nn[coord[,1L]], nn[coord[,2L]], sep = sep)
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

edgeIntersect <- function(object, weightFun = oneWeights)
{
    ## assume we have a weight attr and it is the first column
    ## after the edgeIndex ([[2]])
    ## Drop non-weight attrs
    nodeNames <- nodes(object)
    edgeCounts <- numEdges(object)
    minIdx <- which.min(edgeCounts)
    edgeAttrs <- object@edgeAttrs
    ei1 <- edgeAttrs[[minIdx]][[1L]]
    for (ea in edgeAttrs[-minIdx]) {
        ei1 <- intersect(ei1, ea[[1L]])
    }
    edgeAttrs2 <- lapply(edgeAttrs, function(edgeAttr) {
        keep <- edgeAttr[[1L]] %in% ei1
        edgeAttr[keep, 1:2]
    })
    n_sets <- length(edgeAttrs2)
    if (n_sets > 0L) {
        firstW <- edgeAttrs2[[1L]][[2L]]
        weightMat <- matrix(vector(typeof(firstW), length(firstW) * n_sets),
                            ncol = n_sets)
        for (i in seq_len(n_sets)) {
            weightMat[ , i] <- edgeAttrs2[[i]][[2L]]
        }
        weights <- weightFun(weightMat)
    } else {
        weights <- integer(0L)
    }

    lapply(edgeAttrs2, function(x) x[[2L]])
    object@edgeAttrs <- list(data.frame(mdg_edge_index = edgeAttrs2[[1L]][[1L]],
                                        weight = weights))
    object
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
