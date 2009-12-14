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
    if (!all(valid <- .mdg_valid_node_names(nodeNames))) {
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

.mdg_valid_node_names <- function(names)
{
    !(is.na(names) | (sapply(names, nchar) == 0) |
      (regexpr("\\||\n|\t", names) > 0))
}

setMethod("numNodes", signature = signature(object = "MultiDiGraph"),
          function(object) {
              length(object@nodes)
          })

setMethod("numEdges", signature = signature(object = "MultiDiGraph"),
          function(object) {
              sapply(object@edgeAttrs, function(edgeData) nrow(edgeData))
          })

setMethod("nodes", signature = signature(object = "MultiDiGraph"),
          function(object, ...) {
              object@nodes
          })

setMethod("show",  signature = signature(object = "MultiDiGraph"),
          function(object) {
              cat(class(object),
                  sprintf("with %d nodes and %d edge sets\n",
                          numNodes(object), length(object@edgeAttrs)))
              edgeCounts <- numEdges(object)
              if (is.null(names(edgeCounts)))
                  names(edgeCounts) <- seq_len(length(edgeCounts))
              edgeCounts <- head(edgeCounts, 10L)
              cat(sprintf("edge set %s: %d edges\n",
                          names(edgeCounts), edgeCounts), sep = "")
              invisible(NULL)
          })

eweights <- function(object, names.sep = NULL)
{
    if (is.null(names.sep)) {
        lapply(object@edgeAttrs, function(attrs) attrs[[2L]])
    } else {
        sep <- names.sep[1]
        nn <- nodes(object)
        n_nodes <- length(object@nodes)
        lapply(object@edgeAttrs, function(attrs) {
            w <- attrs[[2L]]
            coord <- .indexToCoord(attrs[[1L]], n_nodes)
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
    new("MultiDiGraph", nodes = object@nodes,
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
randFromTo <- function(numNodes, numEdges, weightFun = function(N) rep(1L, N))
{
    if (numNodes > 2^15) numNodes <- as.double(numNodes)
    maxEdges <- numNodes * numNodes
    nodeNames <- sprintf("%010d", seq_len(numNodes))
    ## use double to allow for large numNodes
    diagIdx <- 1L + 0L:(numNodes - 1L) * (numNodes + 1L)
    numToGen <- max(numEdges * 4, numNodes)
    idx <- unique(ceiling(runif(numToGen, min = 0, max = maxEdges)))
    idx <- idx[!(idx %in% diagIdx)]
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
