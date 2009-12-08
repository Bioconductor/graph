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
    am_size <- n_nodes * n_nodes
    am_dim <- c(n_nodes, n_nodes)

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
        ea_names <- names(esi)[c(-1L, -2L)]
        edgeAttr <-
          data.frame(mdg_edge_index = sparseAM[edgeIdxOrder],
                     esi[edgeIdxOrder, c(-1L, -2L)], row.names = NULL)
        names(edgeAttr) <- c("mdg_edge_index", ea_names)
        edgeAttrs[[i]] <- edgeAttr
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
    numNodes <- as.integer(numNodes)
    numEdges <- as.integer(numEdges)
    maxEdges <- numNodes * numNodes

    nodeNames <- paste("n", seq_len(numNodes), sep="")

    diagIdx <- 1L + 0L:(numNodes - 1L) * (numNodes + 1L)
    idx <- sample(seq_len(maxEdges)[-diagIdx], numEdges)
    to_i <- ((idx - 1L) %/% numNodes) + 1L
    from_i <- ((idx - 1L) %% numNodes) + 1L
    from <- nodeNames[from_i]
    to <- nodeNames[to_i]
    w <- weightFun(length(from))
    list(nodes = nodeNames,
         ft = data.frame(from = from, to = to, weight = w,
                         stringsAsFactors = FALSE))
}

oneWeights <- function(...) rep(1L, length(list(...)[[1L]]))

sumWeights <- function(...)
{
    rowSums(do.call(cbind, list(...)))
}

avgWeights <- function(...)
{
    rowMeans(do.call(cbind, list(...)))
}

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
    newWeights <- do.call(weightFun, lapply(edgeAttrs2, function(x) x[[2L]]))
    object@edgeAttrs <- list(data.frame(mdg_edge_index = edgeAttrs2[[1L]][[1L]],
                                        weight = newWeights))
    object
}
## TODO: should you be allowed to rename edge sets?
## or at least name unnamed edge sets?
