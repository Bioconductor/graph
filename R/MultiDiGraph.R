MultiDiGraph <- function(edgeSets, nodes = NULL)
{
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
    bitVectors <- structure(vector("list", n_edgeSets),
                            names = es_names)
    edgeAttrs <- structure(vector("list", n_edgeSets),
                           names = es_names)
    for (i in seq_len(n_edgeSets)) {
        ft <- ftSets[[i]]
        from_i <- match(ft[, 1L], nodeNames)
        to_i <- match(ft[, 2L], nodeNames)

        bitVect <- makebits(am_size, bitdim = am_dim)
        bitVectors[[i]] <-
          setBitCell(bitVect, from_i, to_i, rep(1L, length(from_i)))

        edgeAttr <-
          data.frame(mdg_from_i = from_i, mdg_to_i = to_i,
                     edgeSets[[i]][-c(1:2)], row.names = NULL)
        edgeAttrs[[i]] <- edgeAttr[order(edgeAttr[[1L]], edgeAttr[[2L]]), ]
    }

    new("MultiDiGraph", nodes = nodeNames, bitVectors = bitVectors,
        edgeAttrs = edgeAttrs)
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
              sapply(object@bitVectors, function(bitVect) {
                            .Call(graph_bitarray_sum, bitVect)
                        })
          })

setMethod("nodes", signature = signature(object = "MultiDiGraph"),
          function(object, ...) {
              object@nodes
          })

setMethod("show",  signature = signature(object = "MultiDiGraph"),
          function(object) {
              cat(class(object),
                  sprintf("with %d nodes and %d edge sets\n",
                          numNodes(object), length(object@bitVectors)))
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
        lapply(object@edgeAttrs, function(attrs) attrs[[3L]])
    } else {
        sep <- names.sep[1]
        nn <- nodes(object)
        lapply(object@edgeAttrs, function(attrs) {
            w <- attrs[[3L]]
            names(w) <- paste(nn[attrs[[1L]]], nn[attrs[[2L]]], sep = sep)
            w
        })
    }
}

edgeMatrices <- function(object)
{
    lapply(object@edgeAttrs, function(attrs) {
        rbind(from=attrs[[1L]], to=attrs[[2L]])
    })
}

extractGraph <- function(object, which)
{
    if (length(which) != 1L)
        stop("'which' must be length one")
    edgeAttr <- object@edgeAttrs[[which]]
    ftmat <- do.call(cbind, edgeAttr[1:2])
    nodeNames <- nodes(object)
    ftmat[] <- nodeNames[ftmat]
    ftM2graphNEL(ftmat, W = edgeAttr[[3L]], V = nodeNames,
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
