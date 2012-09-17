MultiGraph <- function(edgeSets, nodes = NULL, directed = TRUE, 
        ignore_dup_edges = FALSE)
{
    .mg_validate_edgeSet(edgeSets)
    nodeNames <- .mg_node_names(edgeSets, nodes)
    n_nodes <- length(nodeNames)
    edge_sets <- makeMDEdgeSets(edgeSets, directed, nodeNames,
            ignore_dup_edges = ignore_dup_edges)
    mg <- new("MultiGraph", edge_sets = edge_sets, nodes = nodeNames)
    
    mg@edge_defaults <- sapply(names(edge_sets), function(x) {
            list("weight" = 1L)
        }, simplify = FALSE)
    mg
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
        stop("'edgeSets' must have columns 'from', 'to', 'weight'",
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
        stop("duplicate edges in edge set ", sQuote(name), ": ",
             pasteq(paste(from[dups], to[dups], sep = sep)),
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
        stop("invalid node names: ",
             pasteq(head(nodeNames[!valid], 10L)), call. = FALSE)
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
        stop("node names invalid: ",
             pasteq(head(nodeNames[!valid], 10L)))
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
# 
# edgeMatrices <- function(object)
# {
#     n_nodes <- length(object@nodes)
#     lapply(object@edgeAttrs, function(attrs) {
#         matrix(t(.indexToCoord(attrs[[1L]], n_nodes)),
#                nrow = 2L,
#                dimnames = list(c("from", "to"), NULL))
#     })
# }
# 
# fromToList <- function(object)
# {
#     nodeNames <- object@nodes
#     n_nodes <- length(nodeNames)
#     lapply(object@edgeAttrs, function(attrs) {
#         coord <- .indexToCoord(attrs[[1L]], n_nodes)
#         coord[] <- nodeNames[coord]
#         ft <- structure(data.frame(from = coord[ , 1L], to = coord[ , 2L],
#                          attrs[-1L], stringsAsFactors = FALSE),
#                         names = c("from", "to", names(attrs)[-1L]))
#         ft
#     })
# }
# 
# 
# extractGraph <- function(object, which)
# {
#     if (length(which) != 1L)
#         stop("'which' must be length one")
#     edgeAttr <- object@edgeAttrs[[which]]
#     nodeNames <- nodes(object)
#     ftmat <- .indexToCoord(edgeAttr[[1L]], length(nodeNames))
#     ftmat[] <- nodeNames[ftmat]
#     ftM2graphNEL(ftmat, W = edgeAttr[[2L]], V = nodeNames,
#                  edgemode = "directed")
# }
# 
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

edgeSetIntersect0 <- function(g, edgeFun = NULL)
{
    edge_sets <- g@edge_sets
    n_sets <- length(edge_sets)
    if (n_sets < 2L) return(g)
 
    nms <- names(edge_sets)
    nName <- paste(nms, collapse = "_")
    funList <- structure(list(edgeFun), names = nName)
    directed <- isDirected(g)
    klass <- if (all(directed)) "DiEdgeSet" else "UEdgeSet"
   
    if(!( all(directed) || all(!directed))) {
        stop("all edges must either be directed or undirected")
    }
    
    g1 <- subsetEdgeSets(g,nms[1])
    names(g1@edge_sets) <- names(g1@edge_defaults) <- nName
    if(length(g1@userAttrPos@edgePos))  names(g1@userAttrPos@edgePos) <- nName
    for ( i in seq.int(2L, n_sets)) {
        g2 <- subsetEdgeSets(g, nms[i])
        names(g2@edge_sets) <- names(g2@edge_defaults) <- nName
        if(length(g2@userAttrPos@edgePos))  names(g2@userAttrPos@edgePos) <- nName
        g1 <- graphIntersect(g1, g2, edgeFun = funList)
    }
    n_edges <- attr(g1@edge_sets[[1L]], "nbitset") <- 
        .Call(graph_bitarray_sum, g1@edge_sets[[1L]]@bit_vector )
    if(n_edges >0) {
        return(g1)
    } else {
        new_edge_sets <- list()
        return(new("MultiGraph", edge_sets = new_edge_sets, nodes = nodes(g1)))
    }
}

edgeSetUnion0 <- function(g, edgeFun = NULL)
{
    edge_sets <- g@edge_sets
    n_sets <- length(edge_sets)
    if (n_sets < 2L) return(g)
    nms <- names(edge_sets)
    nName <- paste(names(edge_sets), collapse = "_")
    funList <- structure(list(edgeFun), names = nms[1])
    directed <- isDirected(g)
    klass <- if (all(directed)) "DiEdgeSet" else "UEdgeSet"
    if(!( all(directed) || all(!directed))) {
        stop("all edges must either be directed or undirected")
    }

    g1 <- subsetEdgeSets(g,nms[1])
    names(g1@edge_sets) <- nName
    for ( i in seq.int(2L, n_sets)) {
        g2 <- subsetEdgeSets(g, names(g@edge_sets)[i])
        names(g2@edge_sets) <- nName
        g1 <- graphUnion(g1, g2, funList)
    }

    n_edges <- attr(g1@edge_sets[[1L]], "nbitset") <- .Call(graph_bitarray_sum,
                                                            g1@edge_sets[[1L]]@bit_vector )
    if(n_edges >0) {
        return(g1)
    } else {
        new_edge_sets <- list()
        return(new("MultiGraph", edge_sets = new_edge_sets, nodes = nodes(g1)))
    }
}

## TODO: should you be allowed to rename edge sets?
## or at least name unnamed edge sets?

subsetEdgeSets <- function(object, edgeSets) {
    if (!all(nzchar(edgeSets)) || any(is.na(edgeSets)) ||
        !(all(edgeSets %in% names(object@edge_sets))))
        stop("'edgeSet' is invalid")
    if(any(dups <- duplicated(edgeSets)))
        stop("duplicate edges specified in edge set ", edgeSets[dups])
    object@edge_sets<- object@edge_sets[edgeSets]
    nms <- names(object@edge_defaults)
    mtch <- edgeSets[edgeSets %in% nms]
    object@edge_defaults <- if (length(mtch)) object@edge_defaults[mtch]
                            else  list()
    nms <- names( object@userAttrPos@edgePos)
    mtch <- edgeSets[edgeSets %in% nms]
    object@userAttrPos@edgePos <- if (length(mtch))object@userAttrPos@edgePos[mtch]
                            else list()
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

setMethod("subGraph", 
          signature(snodes="character", graph="MultiGraph"),
          function(snodes, graph) {
              origNodes <- nodes(graph)
              snodes <- sort(snodes)
              snodesIdx <- match(snodes, origNodes)
              if (any(is.na(snodesIdx))) {
                  bad <- snodes[which(is.na(snodesIdx))]
                  stop("'snodes' contains nodes not in MultiGraph: ",
                       pasteq(bad))
              }
              nms <- names(graph@nodeData@defaults)
              len <- length(snodes)
              tmp <- makebits(len)
              for(i in nms){
                  indx <-  bitToLogical(graph@userAttrPos@nodePos[[i]])[snodesIdx]
                  graph@nodeData@data[[i]] <-  .getNodeAttrVec(graph, i)[snodesIdx][indx]
                  graph@userAttrPos@nodePos[[i]] <- setbitv(tmp, which(indx), rep(1L, length(which(indx))))
              }
              graph@nodes <- snodes
              edgNms <- names(graph@edge_sets)
              for(i in edgNms) {
                  res <- .Call(graph_bitarray_subGraph, graph@edge_sets[[i]]@bit_vector, snodesIdx)
                  graph@edge_sets[[i]]@bit_vector <- res$bitVec
                  graph@edge_sets[[i]]@weights <- graph@edge_sets[[i]]@weights[res$setPos]
                  attrNms <- names(graph@edge_sets[[i]]@edge_attrs)
                  for (j in attrNms) {
                      attrBit <- graph@userAttrPos@edgePos[[i]][[j]]
                      res <- .Call(graph_bitarray_subGraph, attrBit, snodesIdx)
                      graph@edge_sets[[i]]@edge_attrs[[j]] <-  graph@edge_sets[[i]]@edge_attrs[[j]][res$setPos]
                      graph@userAttrPos@edgePos[[i]][[j]] <- res$bitVec
                  }
              }
              graph
          })

extractGraphAM <- function(g, edgeSets) {
    if(missing(edgeSets))
        edgeSets <- names(g@edge_sets)
    if (!all(nzchar(edgeSets)) || any(is.na(edgeSets)) ||
        !(all(edgeSets %in% names(g@edge_sets))))
        stop("'edgeSet' is invalid")

    nds <- nodes(g)
    drct <- isDirected(g)
    esets <- if (missing(edgeSets)) g@edge_sets else g@edge_sets[edgeSets]
    nms <- names(esets)
    names(nms) <- nms
    lapply(nms, function(x) {
        mat <- edgeSetToMatrix(nds, esets[[x]], drct[[x]])
        bam <- graphAM(adjMat=mat,
            edgemode = if(drct[[x]]) "directed" else "undirected",
            values= list(weight = esets[[x]]@weights))
    })
}

edgeSetToMatrix <- function(nds, edgeSet, directed)
{
    .Call(graph_bitarray_edgeSetToMatrix,
          nds, edgeSet@bit_vector,
          as.numeric(edgeSet@weights), as.logical(directed))
}

setMethod("graphIntersect", 
          c("MultiGraph", "MultiGraph"),
          function(x, y, nodeFun, edgeFun, ...){
              nn <- intersect(nodes(x), nodes(y))
              nnLen <- length(nn)
              nmsX <- names(x@edge_sets)
              nmsY <- names(y@edge_sets)
              eg <- intersect(nmsX, nmsY)
              dr1 <- isDirected(x)[eg]
              dr2 <- isDirected(y)[eg]
              if(!all(dr1 == dr2))
                  stop("edgeSets 'x' and 'y' must have same edgemode")
              theMode <- dr1 & dr2
              if (nnLen == 0){
                  ft <- data.frame(from = character(0), to = character(0), weight = numeric(0))
                  es <- structure(rep(list(ft), length(eg)), names = eg)
                  mg <- MultiGraph(es, directed = theMode)
                  return(mg)
              }

              x <- subsetEdgeSets(x, eg)
              y <- subsetEdgeSets(y, eg)
              sgx <- if (nnLen == numNodes(x)) x else subGraph(nn, x)
              sgy <- if (nnLen == numNodes(y)) y else subGraph(nn, y)

              if(missing(edgeFun))
                  edgeFun <- structure( rep(list(NULL),length(eg)), names = eg)
              if(missing(nodeFun))
                  nodeFun <- NULL
              new_edge_sets <- structure(lapply(eg, function(k) {
                             .getEdgeIntersect(sgx@edge_sets[[k]], sgy@edge_sets[[k]])
                            }), names = eg)
              mg <- .getMGIntersect(sgx, sgy, new_edge_sets, edgeFun) 
              mg@nodeData@defaults <- .retNodeIntersectDefaults(sgx, sgy)
              mg@userAttrPos@nodePos <- .getIntNodeUserAttrPos(sgx, sgy)
              mg@nodeData@data <- .nodeIntersect(sgx, sgy, mg, nodeFun)
              mg 
          })

.getMGIntersect <- function(g1, g2, edge_set, edgeFun) {
    nn <- intersect(nodes(g1), nodes(g2))
    ans <- new("MultiGraph", edge_sets = edge_set, nodes = nn)
    eg <- intersect(names(g1@edge_sets), names(g2@edge_sets))
    for( i in eg) {
        e1Attr <- names(g1@edge_sets[[i]]@edge_attrs)
        e2Attr <- names(g2@edge_sets[[i]]@edge_attrs) 
        commonAttr <- intersect(e1Attr, e2Attr)
        funList <- edgeFun[[i]]
        if(!is.null(funList)) {
            fIndx <- names(funList) %in% c(commonAttr, "weight")
            if(!all(fIndx))
                stop("attributes in 'funList' not in edge attributes: ",
                     pasteq(names(funList)[fIndx]))
        }
        attrType <- .Call(graph_bitarray_Intersect_Attrs, edge_set[[i]]@bit_vector,
                          g1@edge_sets[[i]]@bit_vector, g2@edge_sets[[i]]@bit_vector)
        if(length(attrType$from) >0) {
            ans@edge_sets[[i]]@weights <- .getIntersectAttrs("weight", attrType, g1@edge_sets[[i]]@weights,
                                                           g2@edge_sets[[i]]@weights, funList)
        }
        bt <- (g1@edge_sets[[i]]@bit_vector) & (g2@edge_sets[[i]]@bit_vector)
        attributes(bt) <- attributes(g1@edge_sets[[i]]@bit_vector)
        ns <- .Call(graph_bitarray_sum, bt)
        attr(bt, "nbitset") <- ns

        for(j in commonAttr) {
            ans@userAttrPos@edgePos[[i]][[j]] <- bt
            xAttr <- .retMGAttrVec(g1, i, j)
            yAttr <- .retMGAttrVec(g2, i, j)
            if(length(attrType$from) >0) {
                ans@edge_sets[[i]]@edge_attrs[[j]] <- .getIntersectAttrs(j, attrType, xAttr, yAttr, funList)
            }
        }
        ans@edge_defaults[[i]]<- 
                .retEdgeIntersectDefaults(g1@edge_defaults[[i]], g2@edge_defaults[[i]])

    }
    ans
}

.scaleMG <- function(g, theNodes) {
    if (all(nodes(g) %in% theNodes) && length(nodes(g)) == length(theNodes))
        return(g)
    else {

        es <- names(g@edge_sets)
        for (i in es) {
            g@edge_sets[[i]] <- .scaleEdgeSet(g, i, theNodes)

            nms <- names(g@userAttrPos@edgePos[[i]])
            for(j in nms) {
                g@userAttrPos@edgePos[[i]][[j]] <-
                    .scaleUserAttrPos(g@userAttrPos@edgePos[[i]][[j]], theNodes)
            }
        }
    }
    nms <- names(g@nodeData@defaults)
    for(i in nms){
        origNds  <- nodes(g)[(bitToLogical(g@userAttrPos@nodePos[[i]]))]
        ndsLen <- length(theNodes)
        indx <- match(origNds, theNodes)
        bt <- makebits(ndsLen)
        bt <- setbitv(bt, indx, rep(1L, length(indx)))
        g@userAttrPos@nodePos[[i]] <- bt
        g@nodeData@data[[i]] <- g@nodeData@data[[i]]
    }
    g@nodes <- theNodes
    g
}

.scaleUserAttrPos <- function(edgePos, nds) {
    ft <- .Call(graph_bitarray_rowColPos, edgePos)
    ndsLen <- length(nds)
    posBit <- .createZeroBitPos(ndsLen)
    setBitCell(posBit, ft[,"from"], ft[,"to"], rep(1L, nrow(ft)))
}

.scaleEdgeSet <- function(g, es, nds) {
    df <- diEdgeSetToDataFrame(g@edge_sets[[es]], nodes(g))
    edge_sets <- .makeMDEdgeSet(es_name = 1, es = df,
                                        is_directed = isDirected(g@edge_sets[[es]]), nds,
                                        ignore_dup_edges = FALSE)
    
    edge_sets@edge_attrs <- g@edge_sets[[es]]@edge_attrs
    edge_sets
}

.retMGEdgeUnionUserAttrPos <- function(x, y) {
    nmsX <- names(x@edge_sets)
    nmsY <- names(y@edge_sets)
    cmnEdges <- intersect(nmsX, nmsY)
    allEdges <-  union(nmsX, nmsY)
    snglEdges <- allEdges[! allEdges %in% cmnEdges]
    
    ans <- structure(vector(mode = "list", length(allEdges)), names = allEdges)

    for(i in cmnEdges) {
        xAttr <- names(x@edge_sets[[i]]@edge_attrs)
        yAttr <- names(y@edge_sets[[i]]@edge_attrs)
        unionAttrs <- unique(union(xAttr, yAttr))
        commonAttrs <- intersect(xAttr,yAttr)
        singleAttrs <- unionAttrs[!unionAttrs %in% commonAttrs]
        ans[[i]] <-  structure(vector(mode = "list", length(unionAttrs)), names =
                               unionAttrs)
        
        bt <- (x@edge_sets[[i]]@bit_vector) | (y@edge_sets[[i]]@bit_vector)
        attributes(bt) <- attributes(x@edge_sets[[i]]@bit_vector)
        ns <- .Call(graph_bitarray_sum, bt)
        attr(bt, "nbitset") <- ns
        for (j in commonAttrs) {
           ans[[i]][[j]] <- bt        
        }

        for(j in singleAttrs) {
            if(j %in% names(x@edge_sets[[i]]))
                ans[[i]][[j]] <- x@userAttrPos@edgePos[[i]][[j]]
            else
                ans[[i]][[j]] <- y@userAttrPos@edgePos[[i]][[j]]
        }
    }

    for(i in snglEdges) {
        if(i %in% names(x@edge_sets))
            ans[[i]] <- x@userAttrPos@edgePos[[i]]
        else
            ans[[i]] <- y@userAttrPos@edgePos[[i]]
    }
    ans
}

.getUnionEdgeSet <- function(g1, g2, edgeFun) {
    eg <- intersect(names(g1@edge_sets),names(g2@edge_sets))
    theNodes <- unique(c(nodes(g1), nodes(g2)))
    edge_sets <- structure(lapply(eg, function(i) {
        e1Attr <- names(g1@edge_sets[[i]]@edge_attrs)
        e2Attr <- names(g2@edge_sets[[i]]@edge_attrs) 
        unionAttr <- unique(union(e1Attr, e2Attr))
        if(!is.null(edgeFun[[i]])) {
            fIndx <-  names(edgeFun[[i]]) %in% c(unionAttr, "weight")
            if(!all(fIndx))
                stop("attributes in 'edgeFun' not in edge attributes: ",
                     pasteq(names(edgeFun[[i]])[fIndx]))
        }

        bv <- g1@edge_sets[[i]]@bit_vector | g2@edge_sets[[i]]@bit_vector
        attributes(bv) <- attributes(g1@edge_sets[[i]]@bit_vector)
        attr(bv, "nbitset") <- ns <- .Call(graph_bitarray_sum, bv)
        dr1 <- isDirected(g1@edge_sets[[i]])
        dr2 <- isDirected(g2@edge_sets[[i]])
        theMode <- if (dr1 && dr2) "directed" else "undirected"

        c0 <- character(0)
        df <- data.frame(from = c0, to = c0, weight = numeric(0))
        edge_set <- .makeMDEdgeSet(es_name = 1, es =df,
                               is_directed = (theMode == "directed"),
                               nodes = theNodes, ignore_dup_edges = FALSE)
        edge_set@bit_vector <- bv
         
        cmnBit <- g1@edge_sets[[i]]@bit_vector & g2@edge_sets[[i]]@bit_vector
        attributes(cmnBit) <- attributes(g1@edge_sets[[i]]@bit_vector)
        attr(cmnBit, "nbitset") <- .Call(graph_bitarray_sum, cmnBit)

        fromOneBit <- g1@edge_sets[[i]]@bit_vector & (!cmnBit)
        attributes(fromOneBit) <- attributes(g1@edge_sets[[i]]@bit_vector)
        attr(fromOneBit, "nbitset") <- .Call(graph_bitarray_sum, fromOneBit)
        
        fromTwoBit <- g2@edge_sets[[i]]@bit_vector & (!cmnBit)
        attributes(fromTwoBit) <- attributes(g2@edge_sets[[i]]@bit_vector)
        attr(fromTwoBit, "nbitset") <- .Call(graph_bitarray_sum, fromTwoBit)

        attrType <- .Call(graph_bitarray_Union_Attrs, bv, cmnBit, fromOneBit,
                          fromTwoBit)
        if(length(attrType$from) >0) {
            edge_set@weights <- as.numeric(.getMGUnionWeights(attrType,
                                                                      g1, g2, i, edgeFun[[i]]))
        }
        if(!is.null(unionAttr)) {              
            for(j in unionAttr) {
                edge_set@edge_attrs[[j]] <-  .getMGUnionAttrs(j, attrType, g1,
                                                              g2, i, edgeFun[[i]])
            }
        }
        edge_set
      }), names = eg)
      edge_sets
}

.getMGUnionWeights <- function(attrType, g1, g2, es, funList) {
    len <- length(attrType$from)
    attr1 <- vector(len, mode = "numeric")
    attr1[1:len] <- NA
    ## from x
    k <- (as.numeric(attrType$from) ==1)
    attr1[k]  <- g1@edge_sets[[es]]@weights[attrType$indx1[k]] 
    ## from y 
    k <- (as.numeric(attrType$from) == 2)
    attr1[k]  <- g2@edge_sets[[es]]@weights[attrType$indx2[k]]
    ## resolve union
    k <- (as.numeric(attrType$from) ==0)
    if(any(k)) {
        val1 <- g1@edge_sets[[es]]@weights[attrType$indx1[k]]
        val2 <- g2@edge_sets[[es]]@weights[attrType$indx2[k]]

        if(!is.null(funList) && ("weight" %in% names(funList))) {
            attr1[k] <-  sapply(seq_len(sum(k)), function(p) {
                    return(funList[["weight"]](val1[[p]], val2[[p]]))
                })
        } else if(is.vector(val1) && is.vector(val2)) {
            eqInd <- sapply(seq_len(length(val1)), function(x){
                     identical(val1[x], val2[x])
                 })
            pt <-  which(eqInd)
            lp <- length(which(k))
            tmp <- vector(lp, mode ="numeric")
            tmp[1:lp] <- NA
            tmp[pt] <-  val1[pt]
            attr1[k]  <- tmp
        } 
    } 
    attr1
}


 
.getMGUnionAttrs <- function(att, attrType, x , y, es, funList  ) {
    len <- length(attrType$from)
    indx <- as.numeric(attrType$from)
    if(att %in% names(x@edge_sets[[es]]@edge_attrs))
        mds <- mode(x@edge_sets[[es]]@edge_attrs[[att]])
    else if(att %in% names(y@edge_sets[[es]]@edge_attrs))
        mds <- mode(y@edge_sets[[es]]@edge_attrs[[att]])

    attr1 <- vector(len , mode = mds)
    attr1[1:len] <- NA
    ## from x
    k <- (as.numeric(attrType$from) ==1)
    if(att  %in% names(x@edge_sets[[es]]@edge_attrs)) {
        xAttr <- .retMGAttrVec(x, es, att)
        attr1[k]  <- xAttr[ attrType$indx1[k]] 
    }
    ## from y 
    k <- (as.numeric(attrType$from) == 2)
    if(att  %in% names(y@edge_sets[[es]]@edge_attrs)) {
        yAttr <- .retMGAttrVec(y, es, att)
        attr1[k]  <- yAttr[ attrType$indx2[k]]
    }
    ## resolve union
    k <- (as.numeric(attrType$from) ==0)
    if(any(k)) {
        if(att %in% names(x@edge_sets[[es]]@edge_attrs))
            val1 <- xAttr[ attrType$indx1[k]]
        else 
            val1  <- yAttr[ attrType$indx2[k]]

        if(att %in% names(y@edge_sets[[es]]@edge_attrs))
            val2 <- yAttr[ attrType$indx2[k]]
        else 
            val2  <- xAttr[ attrType$indx1[k]]
 
        if(!is.null(funList) && (att %in% names(funList))) {
            attr1[k] <- sapply(seq_len(sum(k)), function(p) {
                    return(funList[[att]](val1[[p]], val2[[p]]))
                })
        } else if (is.vector(val1) && is.vector(val2)) {
            eqInd <- sapply(seq_len(length(val1)), function(x){
                        identical(val1[x], val2[x])
                        })
            pt <-  which(eqInd)
            lp <- sum(k)
            tmp <- vector(lp, mode = mds)
            tmp[1:lp] <- NA
            tmp[pt] <-  val1[pt]
            attr1[k]  <- tmp
        } 
    }
    attr1
}



setMethod("graphUnion", c("MultiGraph", "MultiGraph"),
          function(x, y, nodeFun, edgeFun, ...){
              dr1 <- isDirected(x)
              dr2 <- isDirected(y)
              nmsX <- names(x@edge_sets)
              nmsY <- names(y@edge_sets)
              eg <- intersect(nmsX, nmsY)
              if(missing(edgeFun))
                  edgeFun <- structure( rep(list(NULL),length(eg)), names = eg)
              if(missing(nodeFun))
                  nodeFun <- NULL

              if(!all(dr1[eg] == dr2[eg]))
                  stop("edgeSets 'x' and 'y' must have same edgemode")
              theNodes <- unique(c(nodes(x), nodes(y)))
              nnLen <- length(theNodes)
              mgx <- .scaleMG(x, theNodes)
              mgy <- .scaleMG(y, theNodes)
              xeg <- nmsX[ !nmsX %in% eg]
              yeg <- nmsY[ !nmsY %in% eg]
              new_edge_sets <- .getUnionEdgeSet(mgx, mgy, edgeFun)
              x_edge_sets <- x@edge_sets[xeg]
              y_edge_sets <- y@edge_sets[yeg]

              mg <- new("MultiGraph", edge_sets = c(new_edge_sets, x_edge_sets, 
                                                    y_edge_sets), nodes = theNodes)
              mg@userAttrPos@edgePos <- .retMGEdgeUnionUserAttrPos(mgx, mgy)
              mg@nodeData@defaults <- .retNodeUnionDefaults(mgx, mgy)
              mg@nodeData@data <- .nodeUnion(x, y, nodeFun)
              mg@userAttrPos@nodePos <- .getUnionNodeUserAttrPos(mg, mgx, mgy)
              mg@edge_defaults <- .retMGEdgeUnionDefaults(mgx, mgy)
              mg 
          })


.retMGEdgeUnionDefaults <- function(g1, g2) {
    eg <- intersect(names(g1@edge_sets), names(g2@edge_sets))
    structure(lapply(eg, function(i) {
           cmnAttrs <- intersect(names(g1@edge_defaults[[i]]), 
                                           names(g2@edge_defaults[[i]]))
    unqAttrs <- unique(c(names(g1@edge_defaults[[i]]), 
                         names(g2@edge_defaults[[i]])))
    singleAttrs <- unqAttrs[!( unqAttrs %in% cmnAttrs)]
    cmn <- structure(lapply(cmnAttrs, function(x) {
                            if(identical(g1@edge_defaults[[i]][[x]], 
                                         g2@edge_defaults[[i]][[x]])) g1@edge_defaults[[i]][[x]] else NA
                         }), names = cmnAttrs)

    sng <- structure(lapply(singleAttrs, function(x) {
                            if(x %in% names(g1@edge_defaults[[i]]))
                                g1@edge_defaults[[i]][[x]] 
                            else 
                                g2@edge_defaults[[i]][[x]] 
                         }), names = singleAttrs)
    c(cmn, sng)
                                              }), 
    names = eg)
  }
 
extractGraphBAM <- function(g, edgeSets) {
    if(missing(edgeSets))
        edgeSets <- names(g@edge_sets)
    if (!all(nzchar(edgeSets)) || any(is.na(edgeSets)) ||
        !(all(edgeSets %in% names(g@edge_sets))))
        stop("edgeSet is invalid")
    nn <- nodes(g)
    esets <-  g@edge_sets[edgeSets]
    df_empty <- data.frame(from = character(0), to = character(0),
            weight = numeric(0))
    structure(lapply(names(esets), function(x) {
                edgeMode <- if(isDirected(esets[[x]])) "directed" else "undirected"
                bam <- graphBAM(df_empty, nodes = nn, edgemode = edgeMode,
                        ignore_dup_edges = TRUE)
                bam@edgeSet <- esets[[x]]
                bam@nodeData@data <- g@nodeData@data
                bam@nodeData@defaults <- g@nodeData@defaults
                bam@userAttrPos@nodePos <- g@userAttrPos@nodePos
                bam@edgeData@defaults <- g@edge_defaults[[x]]
                if(length(g@userAttrPos@edgePos))
                    bam@userAttrPos@edgePos <- g@userAttrPos@edgePos[[x]]
                bam
            }), names = names(esets))
}


## Degree of a multigraph
setMethod("degree", signature(object = "MultiGraph", Nodes = "missing"),
      function(object){
          .mgDegree(object)
})

## Node data accces methods 

.nodeDataRetrieve <- function(self, n, attr) {
    if (length(attr) != 1L)
        stop("'attr' argument must specify a single attribute name")
    if ( ! (attr %in% names(self@nodeData@defaults)))
        stop("attribute not present: ", sQuote(attr))
    nds <- nodes(self)
    .verifyNodes(n, nds)
    idx <- match(n, nds)
    names(idx) <- 1:length(idx)
    idx <- sort(idx) 
    n <- n[as.numeric(names(idx))]
    names(idx) <- NULL
    bt <- self@userAttrPos@nodePos[[attr]]
    ord <- .getNodeAttrPos(bt, idx)
    res <- vector(length(idx), mode = mode(self@nodeData@defaults[[attr]]))
    res[1:length(idx)] <- self@nodeData@defaults[[attr]]
    if (length(ord$leftPos))
        res[ord$leftPos] <- self@nodeData@data[[attr]][ord$rightPos]
    as.list(structure(res, names = n))
}

setMethod("nodeData",
        signature(self = "MultiGraph", n = "character", attr = "character"),
        function(self, n, attr) {
            .nodeDataRetrieve(self, n, attr)
        })

setMethod("nodeData",
        signature(self = "MultiGraph", n = "missing", attr = "character"),
        function(self, n, attr) {
            if(length(attr) != 1L)
                stop("'attr' must specify a single attribute")
            if( ! (attr %in% names(self@nodeData@defaults)))
                stop("attribute not present: ", sQuote(attr))
            nds <- nodes(self)
            nodeData(self, n= nds, attr= attr)
        })

setMethod("nodeData",
        signature(self = "MultiGraph", n = "character", attr = "missing"),
        function(self, n, attr) {
            nds <- nodes(self)
            .verifyNodes(n ,nds)
            nms <- names(self@nodeData@defaults)
            structure(lapply(nms, function(x) {
                     nodeData(self, n, x)
                }), names = nms)
        })

setMethod("nodeData",
        signature(self = "MultiGraph", n = "missing", attr = "missing"),
        function(self, n, attr) {
            nds <- nodes(self)
            nms <- names(self@nodeData@defaults)
            structure(lapply(nms, function(x) {
                        nodeData(self, nds, x)
                    }), names = nms)
        })

## Node data replacement methods 

.nodeDataReplaceNodeGiven <- function(self, n, attr, value) {
    .verifyAttrName(attr, names(self@nodeData@defaults))
    if(length(attr) != 1L)
        stop("attr argument must specify a single attribute")
    if(is.vector(value)){
        len <- length(value)
    } else {
        len <- 1
        value <- list(value)
    }
    if(len!=1L && len != length(n)) {
        stop("value must be of length one or have the same length as n")
    }
    nms <- names(self@nodeData@data)
    nds <- nodes(self)
    .verifyNodes(n, nds)
    if(len==1L && len !=length(n))
        value <- rep(value, length(n))
    idx <- match(n, nds)
    names(idx) <- 1:length(idx)
    idx <- sort(idx) 
    value <- value[as.numeric(names(idx))]
    names(idx) <- NULL       
    bt <- self@userAttrPos@nodePos[[attr]] 
    bt <- .Call(graph_bitarray_set, bt, as.integer(idx), 
        as.integer(rep(1L, length(idx))))
    ns <- attr(bt, "nbitset")
    self@userAttrPos@nodePos[[attr]] <- bt
    ord <- .getNodeAttrOrder(bt, idx)
    newAttr <- vector(ns, mode = mode(value))
    newAttr[attr(ord$newVal, "newPos")] <- value[ord$newVal]
    newAttr[attr(ord$origVal, "origPos")] <- self@nodeData@data[[attr]][ord$origVal]
    self@nodeData@data[[attr]] <- newAttr
    self 
}

.nodeDataReplaceNodeMissing <- function (self, attr, value) {
    .verifyAttrName(attr, names(self@nodeData@defaults))
    if(length(attr) != 1L)
        stop("attr argument must specify a single attribute")
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
        nodeData(self, n = nodes(self), attr) <- value
        self
}

setReplaceMethod("nodeData",
        signature(self = "MultiGraph", n="character", attr="character", value="ANY"),
        function(self, n, attr, value) {
            .nodeDataReplaceNodeGiven(self, n, attr, value)            
        })

setReplaceMethod("nodeData",
        signature(self = "MultiGraph", n="missing", attr="character", value="ANY"),
        function(self, n, attr, value) {
            .nodeDataReplaceNodeMissing(self, attr, value)
        })

.verifyMgEdgeSetNames <- function(mg, e) {
    if(!e %in% names(mg@edge_defaults))
        stop("edgeSet not found: ", sQuote(e))
    if(numEdges(mg)[e] == 0)
        stop("edgeSet does not have any connected edges: ", sQuote(e))
}

.verifyMgEdges <- function(mg, e, from, to) {
    stopifnot(length(from) == length(to))
    if (length(from) == 0L) 
        stop("edges not in edgeSet: ", sQuote(e))
    adjList <- .mgIsAdj(mg, e, from, to)
    if (any(!adjList)) {
        badFr <- from[!adjList]
        badTo <- to[!adjList]
        res <- paste(badFr, badTo, sep = "|", collapse = ", ")
        stop("edges not found: ", sQuote(res))
    }
    TRUE
}

.verifyMGAttrs <- function(mg, e, attr) {
    if( !(attr %in% names(mg@edge_defaults[[e]])))
        stop("attr ", sQuote(attr), " not in edgeSet ", sQuote(e))
}

setMethod("mgEdgeDataDefaults", 
        signature(self ="MultiGraph", edgeSet="character", attr="character"),
        function(self, edgeSet, attr) {
            self@edge_defaults[[edgeSet]][[attr]]
        })

setMethod("mgEdgeDataDefaults", 
          signature(self ="MultiGraph", edgeSet="character", attr="missing"),
          function(self, edgeSet, attr) {
              self@edge_defaults[[edgeSet]]
          })

setReplaceMethod("mgEdgeDataDefaults", 
                 signature(self="MultiGraph", edgeSet = "character", attr="missing",
                           value="list"),
                 function(self, edgeSet, attr, value) {
                     self@edge_defaults[[edgeSet]] <- value  
                     wt <- self@edge_defaults[[edgeSet]][["weights"]]
                     if(!is.numeric(wt) && !is.null(wt))
                         stop("weights attribute has to be of type numeric")
                     ndsLen <- length(nodes(self))
                     nms <- names(value)
                     posBit <- .createZeroBitPos(ndsLen)
                     for(i in 1: length(value)){
                         if(!(nms[i] %in% names(self@userAttrPos@edgePos[[edgeSet]]))) {
                             if(nms[i] != "weight"){
                                 self@userAttrPos@edgePos[[edgeSet]][[nms[i]]] <- posBit
                             }
                         }
                     }
                     self
                 })


setReplaceMethod("mgEdgeDataDefaults", 
                 signature(self="MultiGraph", edgeSet = "character",
                           attr="character", value="ANY"),
                 function(self, edgeSet, attr, value) {
                     self@edge_defaults[[edgeSet]][[attr]] <- value
                     if(attr == "weight") {
                         wt <- self@edge_defaults[[edgeSet]][["weights"]]
                         if(!is.numeric(wt) && !is.null(wt))
                             stop("weights attribute has to be of type numeric")
                     } else{
                         ndsLen <- length(nodes(self))
                         posBit <- .createZeroBitPos(ndsLen)
                         if(!(attr %in% names(self@userAttrPos@edgePos[[edgeSet]]))) {
                             self@userAttrPos@edgePos[[edgeSet]][[attr]] <- posBit
                         }
                     }
                     self
                 })

## MultiGraph edgeData methods 
setMethod("mgEdgeData",
          signature(self = "MultiGraph", edgeSet = "character", from = "character", 
                    to = "character", attr = "character"),
          function(self, edgeSet, from , to, attr) {
              nodeNames <- self@nodes
              .verifyMgEdgeSetNames(self, edgeSet)
              req_ft <- .align_from_to(from, to, nodeNames)
              if(nrow(req_ft) > 0)
                  .verifyMgEdges(self, edgeSet, req_ft[,"from"], req_ft[,"to"])
              .verifyMGAttrs(self, edgeSet, attr)
              numNodes <- length(nodeNames)
              bv <- self@edge_sets[[edgeSet]]@bit_vector
              val <- .retMGAttrVec(self, edgeSet, attr)
              ft <- .Call(graph_bitarray_rowColPos, self@edge_sets[[edgeSet]]@bit_vector)
              if(!isDirected(self@edge_sets[[edgeSet]])) {
                  df <- cbind(from=ft[,"to"], to = ft[,"from"])
                  ft <- rbind(ft,df)
                  val <- c(val, val)
              }
              ft <- data.frame(ft)
              ft <- ft[with(ft, order(to,from)),]
              req_i <- structure(match(req_ft, nodeNames), dim = dim(req_ft))
              colnames(req_i) <- c("from", "to")
              tmp <- rbind(req_i, ft)
              pst <- paste(tmp[,"from"],tmp[,"to"], sep = "_")
              idx <- duplicated(pst)[seq(nrow(req_i) +1 , nrow(tmp))]
              ord <- order(req_i[,2], req_i[,1])
              req_i <- req_i[ord,,drop =FALSE]
              val <- structure(val[idx], 
                        names = paste(nodeNames[req_i[,1]],nodeNames[req_i[,2]], sep = "|"))
              as.list(val)
          })

setMethod("mgEdgeData",
          signature(self = "MultiGraph", edgeSet = "character", from = "character", 
                    to = "missing", attr = "character"),
          function(self, edgeSet, from , to, attr) {
              .verifyMgEdgeSetNames(self,edgeSet)
              .mgGetAttrs( self, edgeSet, from, attr)
          })


setMethod("mgEdgeData",
          signature(self = "MultiGraph", edgeSet = "character", from = "missing", 
                    to = "character", attr = "character"),
          function(self, edgeSet, from , to, attr) {
              .verifyMgEdgeSetNames(self,edgeSet)
              nodeNames <- self@nodes
              numNodes <- length(nodeNames)
              bv <- self@edge_sets[[edgeSet]]@bit_vector
              val <- .retMGAttrVec(self, edgeSet, attr)
              ft <- .Call(graph_bitarray_rowColPos, self@edge_sets[[edgeSet]]@bit_vector)
              if(!isDirected(self@edge_sets[[edgeSet]])) {
                  df <- cbind(from=ft[,"to"], to = ft[,"from"])
                  ft <- rbind(ft,df)
                  val <- c(val,val)
              }
              tmp <- seq_len(length(val))
              ft <- data.frame(ft, tmp, stringsAsFactors = FALSE)
              ft <- ft[ft[,"to"] %in% which(nodeNames %in% to),]  
              if(nrow(ft) == 0)
                  stop("edges in \"to\" not found in \"self\"")
              .verifyMgEdges(self, edgeSet, nodeNames[ft[,"from"]], nodeNames[ft[,"to"]])
              nodeLbl <- paste( nodeNames[ft[,"from"]], nodeNames[ft[, "to"]], sep ="|")
              val <- val[ft[,"tmp"]][1:length(nodeLbl)]
              names(val) <- nodeLbl
              as.list(val)
             })

setMethod("mgEdgeData",
          signature(self = "MultiGraph", edgeSet = "character", from = "missing", 
                    to = "missing", attr = "character"),
          function(self, edgeSet, from , to, attr) {
              .verifyMgEdgeSetNames(self,edgeSet)
              eg <- .edges_mg(self, edgeSet)             
              .mgGetAttrs( self, edgeSet, from = names(eg), attr)
          })
## MultiGraph edgeData replacement methods 
setReplaceMethod("mgEdgeData",
                 signature(self = "MultiGraph", edgeSet = "character", from = "character", 
                           to = "character", attr = "character", value = "ANY"),
                 function(self, edgeSet, from, to, attr, value) { 
                     .verifyMgEdgeSetNames(self,edgeSet)
                     .verifyAttrName(attr, names(self@edge_defaults[[edgeSet]]))
                     lenFrom <- length(from)
                     lenTo <- length(to)
                     if (lenFrom != lenTo) {
                         if(lenFrom ==1)
                             from <- rep(from, lenTo)
                         else if (lenTo == 1)
                             to <- rep(to , lenFrom)
                         else
                             stop("arguments 'from', 'to' differ in length")
                     }
                     if(length(edgeSet) != 1L)
                         stop("edgeSet has to be of length 1")
                     .mgSetAttrs(self, edgeSet, from, to, attr, value) 
                 })


setReplaceMethod("mgEdgeData",
                 signature(self="MultiGraph", edgeSet = "character", 
                           from = "character", to = "missing", attr="character",
                           value = "ANY"),
                 function(self, edgeSet, from, to, attr, value) {
                     .verifyAttrName(attr, names(self@edge_defaults[[edgeSet]]))
                     .verifyMgEdgeSetNames(self,edgeSet)
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
                     .verifyMgEdgeSetNames(self,edgeSet)
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
                     .verifyMgEdgeSetNames(self, edgeSet)
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
    fromEdges <- .edges_mg(object, e)[from]
    .Call(graph_is_adjacent, fromEdges, to)
}


.mgSetAttrs <- function(mg, e, from, to, attr, value)
{  
    nodeNames <- mg@nodes
    req_ft <- .align_from_to(from, to, nodeNames)

    ## remove dups
    indx <- duplicated(paste(req_ft[,"from"], req_ft[,"to"], sep ="_"))
    req_ft <- req_ft[!indx, ,drop = FALSE]
    if(nrow(req_ft) > 0)
        .verifyMgEdges(mg, e, req_ft[,"from"], req_ft[,"to"])
    else 
       stop("edges specified could not be found in edgeSet ", sQuote(e))

    if(is.vector(value)) {
        len  <- length(value)
    }else{
        len <- 1
        value <- list(value)
    }
    if(len == 1L)
        value <- rep(value, nrow(req_ft))

    if(length(value) != nrow(req_ft))
         stop("number of edges and attribute values must be the same")

    ft <- .Call(graph_bitarray_rowColPos, mg@edge_sets[[e]]@bit_vector)
    if (!isDirected(mg@edge_sets[[e]])) {
        ## normalize from/to
        valIndx <- seq_len(length(value))
        tmp <- .mg_undirectEdges(req_ft[ , 1], req_ft[, 2], valIndx)
        req_ft <- cbind(tmp[["from"]], tmp[["to"]])
        value <- value[tmp[["weight"]]]
    }
    ## convert node names to index
    req_i <- structure(match(req_ft, nodeNames), dim = dim(req_ft))
    colnames(req_i) <- c("from", "to")
    req_i <- data.frame(req_i)
    idx <- order(req_i[,2], req_i[,1])
    req_i <- req_i[idx, ]
    value <- value[idx]
    if(attr == "weight") {
        attrBit <- mg@edge_sets[[e]]@bit_vector
        if(nrow(req_i)) {
            ord <- .Call(graph_bitarray_getEdgeAttrOrder,  attrBit, 
                         as.integer(req_i[,"from"]), as.integer(req_i[,"to"]))
            mg@edge_sets[[e]]@bit_vector <- setBitCell(attrBit, req_i[,"from"], req_i[,"to"], 
                                                              rep(1L, nrow(req_i)))
            nt <- attr(mg@edge_set[[e]]@bit_vector, "nbitset")
        } else {
            nt <- attr(attrBit, "nbitset")
            ord <- list(newLeftPos = integer(0), newRightPos = integer(0), 
                        origLeftPos = seq_len(nt), origRightPos = seq_len(nt))
        }
        newAttr <- vector(nt, mode = mode(value))
        newAttr[ord$origLeftPos] <- mg@edgeSet@weights[ord$origRightPos]
        newAttr[ord$newLeftPos] <- value[ord$newRightPos]
        mg@edge_sets[[e]]@weights <- newAttr
    } else {
        attrBit <- mg@userAttrPos@edgePos[[e]][[attr]]
        if(nrow(req_i)) {
            ord <- .Call(graph_bitarray_getEdgeAttrOrder, attrBit, 
                         as.integer(req_i[,"from"]), as.integer(req_i[,"to"]))
            mg@userAttrPos@edgePos[[e]][[attr]] <- setBitCell(attrBit, req_i[,"from"], req_i[,"to"], 
                                                                      rep(1L, nrow(req_i)))
            nt <- attr(mg@userAttrPos@edgePos[[e]][[attr]], "nbitset")
        } else {
            nt <- attr(attrBit, "nbitset")
            ord <- list(newLeftPos = integer(0), newRightPos = integer(0), 
                        origLeftPos = seq_len(nt), origRightPos = seq_len(nt))
        }
        newAttr <- vector(nt, mode = mode(value))
        newAttr[ord$origLeftPos] <- 
                     mg@edge_sets[[e]]@edge_attrs[[attr]][ord$origRightPos]
        newAttr[ord$newLeftPos] <- value[ord$newRightPos]
        mg@edge_sets[[e]]@edge_attrs[[attr]] <- newAttr
    }
    mg
}

.retMGAttrVec <- function(g, e, attr) {
    if(attr !="weight") {
        k1 <- g@edge_sets[[e]]@bit_vector
        k2<- g@userAttrPos@edgePos[[e]][[attr]]
        tmp <- attributes(k1)
        res <- k1& (!k2)
        attributes(res) <- tmp
        ns <- .Call(graph_bitarray_sum, res)
        attr(res, "nbitset") <- ns
        ft <- data.frame(.Call(graph_bitarray_rowColPos,res))
        dflt <- g@edge_defaults[[e]][[attr]]
        attrBit <- g@userAttrPos@edgePos[[e]][[attr]]
        ft <- ft[with(ft, order(to, from)),]
        if(nrow(ft)) {
            ord <- .Call(graph_bitarray_getEdgeAttrOrder,  attrBit, 
                     as.integer(ft[,"from"]), as.integer(ft[,"to"]))
            attrBit <- setBitCell(attrBit, ft[,"from"], ft[,"to"], 
                                      rep(1L, nrow(ft)))
            nt <- attr(attrBit, "nbitset")
        } else {
            nt <- attr(attrBit, "nbitset")
            ord <- list(newLeftPos = integer(0), newRightPos = integer(0), 
                        origLeftPos = seq_len(nt), origRightPos = seq_len(nt))
        }
        newAttr <- vector(nt, mode = mode(dflt))
        if(!is.null(g@edge_sets[[e]]@edge_attrs[[attr]])) {
            newAttr[ord$origLeftPos] <- g@edge_sets[[e]]@edge_attrs[[attr]][ord$origRightPos]
            newAttr[ord$newLeftPos] <- if(mode(dflt)=="list") rep(list(dflt), length(ord$newLeftPos)) else  dflt
        }else{
            newAttr[1:nt] <- if(mode(dflt)=="list") rep(list(dflt), nt) else  dflt
        }
    }else{
        newAttr <- g@edge_sets[[e]]@weights
    }   
    newAttr
}

.mgGetAttrs <- function(self, edge, from, attr) {
    nodeNames <- self@nodes
    indx <- which(nodeNames %in% from)
    numNodes <- length(nodeNames)
    bv <- self@edge_sets[[edge]]@bit_vector
    .verifyMGAttrs(self, edge, attr)
    val <- .retMGAttrVec(self, edge, attr)
    ft <- .Call(graph_bitarray_rowColPos, bv)
    if(!isDirected(self@edge_sets[[edge]])) {
        df <- cbind(from=ft[,"to"], to = ft[,"from"])
        ft <- rbind(ft,df)
        val <- c(val,val)
    }
    tmp <- seq_len(length(val))
    ft <- data.frame(ft, tmp, stringsAsFactors = FALSE )
    ft <- ft[ ft[,"from"] %in% indx,]
    if(nrow(ft) == 0)
        stop("edges specified in \"from\" not found in edgeSet ",
             sQuote(edge))
    nodeLbl <- paste( nodeNames[ft[,"from"]], nodeNames[ft[, "to"]],
            sep ="|")
    val <- val[ft[,"tmp"]][1:length(nodeLbl)]
    names(val) <- nodeLbl
    as.list(val)
 }


       
setMethod("nodeDataDefaults", 
        signature(self="MultiGraph", attr="missing"),
        function(self, attr){
            attrDefaults(self@nodeData)
        })

setMethod("nodeDataDefaults", 
        signature(self="MultiGraph", attr="character"),
        function(self, attr){
            attrDefaults(self@nodeData, attr)
        })

setReplaceMethod("nodeDataDefaults", 
        signature(self="MultiGraph", attr="missing", value="list"),
        function(self, attr, value) {
            attrDefaults(self@nodeData) <- value
            nmsNds <- names(self@nodeData@defaults)
            nmsAttr <- names(self@userAttrPos@nodePos)
            ndsLbls <- nmsNds[ !(nmsNds %in% nmsAttr)]
            if(length(ndsLbls)) {
                ndsLen <- length(nodes(self))
                bt <- makebits(ndsLen)
            }
            for(i in ndsLbls) {
                self@userAttrPos@nodePos[[i]] <- bt
            }
            self
        })

setReplaceMethod("nodeDataDefaults", 
    signature(self="MultiGraph", attr="character", value="ANY"),
    function(self, attr, value) {
        attrDefaults(self@nodeData, attr) <- value
        ndsLen <- length(nodes(self))
        if(! (attr %in% names(self@userAttrPos@nodePos))){
            bt <- makebits(ndsLen)
            self@userAttrPos@nodePos[[attr]] <- bt
        }
        self
    })


setMethod("edgeSets", signature("MultiGraph"), function(object, ...) {
        names(object@edge_sets)
    })


setMethod("edges", signature("MultiGraph", "character"),
          function(object, which, edgeSet) {
              if(missing(edgeSet))
                  edgeSet <- edgeSets(object)
              if (!all(edgeSet %in% edgeSets(object)))
                  stop("edgeSet specified not found in MultiGraph")
              if (!all(which %in% nodes(object)))
                  stop("nodes specified not found in MultiGraph")
              eg <- extractGraphBAM(object, edgeSet)
              if(length(eg) == 1)
                  edges(eg[[edgeSet]], which)
              else 
                  lapply(eg, edges, which)            
          })

setMethod("edges", signature("MultiGraph", "missing"),
          function(object, edgeSet) {
              if (missing(edgeSet))
                  edgeSet <- edgeSets(object)
              if (!all(edgeSet %in% edgeSets(object)))
                  stop("edgeSet specified not found in MultiGraph")

              eg <- extractGraphBAM(object, edgeSet)
              if(length(eg) == 1)
                  edges(eg[[edgeSet]])
              else 
                  lapply(eg, edges)    
          })

setMethod("edgeNames", signature("MultiGraph"),
          function(object, edgeSet) {
              if(missing(edgeSet))
                  edgeSet <- edgeSets(object)
              if (!all(edgeSet %in% edgeSets(object)))
                  stop("edgeSet specified not found in MultiGraph")
              eg <- extractGraphBAM(object, edgeSet)
              if(length(eg) == 1)
                  edgeNames(eg[[edgeSet]])
              else 
                  lapply(eg, edgeNames)    
          })
            

