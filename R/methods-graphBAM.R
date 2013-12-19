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
                     ignore_dup_edges = FALSE) 
{
    .required <- c("from", "to", "weight")
    cl <- .required %in% names(df)
    if (!all(cl)) {
        stop("required 'names(df)' not present: ",
             pasteq(.required[!cl]))
    }
    if (any(duplicated(nodes)))
        stop(sQuote(nodes), " must be unique") 
    edge_nodes <- unique(c(as.character(df$from), as.character(df$to)))
    if (!all(edge_nodes %in% nodes))
        nodes <- sort(c(edge_nodes, nodes))
    else if (is.null(nodes))
        nodes <- edge_nodes
    is_directed <- edgemode == "directed"
    edge_sets <- .makeMDEdgeSet(es_name = 1, es = df,
                                is_directed = is_directed, nodes,
                                ignore_dup_edges = ignore_dup_edges)
    g <- new("graphBAM", nodes = nodes, edgeSet = edge_sets)
    g@edgeData@defaults[["weight"]] <- 1L
    g
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
    }, wList, wNameList, SIMPLIFY=FALSE)
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
                stop("'attr' must be character(1)")
              if (!is.null(type.checker) && !is.function(type.checker))
                stop("'type.checker' must be a function or NULL")
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

.eAttrsFun <- function(self, from, attr) {
    nodeNames <- self@nodes
    indx <- which(nodeNames %in% from)
    numNodes <- length(nodeNames)
    bv <- self@edgeSet@bit_vector
    .verifyBAMAttrs(self, attr)
    val <- .retAttrVec(self, attr)
    ft <- .Call(graph_bitarray_rowColPos, bv)
    if(!isDirected(self)){
        df <- cbind(from=ft[,"to"], to = ft[,"from"])
        ft <- rbind(ft,df)
        val <- c(val,val)
    }
    tmp <- seq_len(length(val))   # indices into val
    ft <- data.frame(ft, tmp, stringsAsFactors = FALSE )
    ft <- ft[ ft[,"from"] %in% indx,]
    if(nrow(ft) == 0)
         stop("edges specified in 'from' not found in 'self'")

    nodeLbl <- paste( nodeNames[ft[,"from"]], nodeNames[ft[, "to"]],
            sep ="|")
    val <- val[ft[,"tmp"]][seq_along(nodeLbl)]
    names(val) <- nodeLbl
    val
}

## graphBAM edgeData methods 


setMethod("edgeData", 
          signature(self="graphBAM", from="character", to= "missing", attr="character"),
          function(self, from, to, attr){
              as.list(.eAttrsFun(self, from, attr))       
          })

setMethod("edgeData",
          signature(self="graphBAM", from="character", to="character", attr="character"),
          function(self, from, to, attr) {
              edgeData.from <- edgeData(self, attr=attr, from=from)
              unrecognized.nodes <- setdiff(to, nodes(self))
              if(length(unrecognized.nodes) > 0) {
                  msg <- sprintf("nodes not in graph: %s",
                                 paste(sQuote(unrecognized.nodes), collapse=", "))
                  stop(msg)
                  }
              edgeNames <- names(edgeData.from)
              toStarts <- regexpr("|", edgeNames, fixed=TRUE) + 1L
              actual.to.nodes <- substring(edgeNames, toStarts, nchar(edgeNames))
              edgeData.from[actual.to.nodes %in% to]
          })

setMethod("edgeData", 
          signature(self="graphBAM", from="missing", to= "character", attr="character"),
          function(self, from, to, attr){
              nodeNames <- self@nodes
              numNodes <- length(nodeNames)
              bv <- self@edgeSet@bit_vector
              .verifyBAMAttrs(self, attr)
              val <- .retAttrVec(self, attr)
              ft <- .Call(graph_bitarray_rowColPos, self@edgeSet@bit_vector)
              if(!isDirected(self)){
                  df <- cbind(from=ft[,"to"], to = ft[,"from"])
                  ft <- rbind(ft,df)
                  val <- c(val,val)
              }
              tmp <- seq_len(length(val))
              ft <- data.frame(ft, tmp, stringsAsFactors = FALSE)
              ft <- ft[ft[,"to"] %in% which(nodeNames %in% to),]  
              if(nrow(ft) == 0)
                  stop("edges specified in 'to' not found in 'self'")
              .verifyEdges(self, nodeNames[ft[,"from"]], nodeNames[ft[,"to"]])
              nodeLbl <- paste( nodeNames[ft[,"from"]], nodeNames[ft[, "to"]],
                               sep ="|")
              val <- val[ft[,"tmp"]][seq_along(nodeLbl)]
              names(val) <- nodeLbl
              as.list(val)
          })

setMethod("edgeData", 
          signature(self="graphBAM", from="missing", to="missing", attr="missing"),
          function(self, from, to, attr) {
              nodeNames <- self@nodes
              numNodes <- length(nodeNames)
              attr <- "weight"
              bv <- self@edgeSet@bit_vector
              ft <- .Call(graph_bitarray_rowColPos, bv)
              w <- .retAttrVec(self, attr)
              if(!isDirected(self)){
                  df <- cbind(from=ft[,"to"], to = ft[,"from"])
                  ft <- rbind(ft,df)
                  w <- c(w,w)
              }
              nodeLbl <- paste( nodeNames[ft[,"from"]], nodeNames[ft[, "to"]],
                               sep ="|")
              names(w) <- nodeLbl
              lapply(w, function(x) list(weight = as.numeric(x)))
          })


setMethod("edgeData", 
          signature(self="graphBAM", from="missing", to= "missing", attr="character"),
          function(self, from, to, attr){
              as.list(.eAttrsFun(self, from = names(edges(self)), attr= attr))       
          })

.retAttrVec <- function(g, attr) {
    if(attr !="weight") {
        k1 <- g@edgeSet@bit_vector
        k2<- g@userAttrPos@edgePos[[attr]]
        tmp <- attributes(k1)
        res <- k1& (!k2)
        attributes(res) <- tmp
        ns <- .Call(graph_bitarray_sum, res)
        attr(res, "nbitset") <- ns
        ft <- data.frame(.Call(graph_bitarray_rowColPos, res))
        dflt <- g@edgeData@defaults[[attr]]
        attrBit <- g@userAttrPos@edgePos[[attr]]
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
        if(!is.null(g@edgeSet@edge_attrs[[attr]])) {
            newAttr[ord$origLeftPos] <- g@edgeSet@edge_attrs[[attr]][ord$origRightPos]
            newAttr[ord$newLeftPos] <- if(mode(dflt)=="list") rep(list(dflt), length(ord$newLeftPos)) else  dflt
        }else{
            newAttr[seq_len(nt)] <-
                if(mode(dflt)=="list") rep(list(dflt), nt) else  dflt
        }
    }else{
        newAttr <- g@edgeSet@weights
    }   
    newAttr
}

.align_from_to <- function(from, to, nodeNames)
{
    from_len <- length(from)
    to_len <- length(to)
    req_nn <- unique(c(from, to))
    if (!all(okidx <- req_nn %in% nodeNames))
        stop("unknown nodes: ", pasteq(req_nn[!okidx]))
    if (from_len != to_len) {
        if (from_len == 1L)
            from <- rep(from, to_len)
        else if (to_len == 1L)
            to <- rep(to, from_len)
        else
            stop("invalid lengths of 'from' and 'to'")
    }
    df <- cbind(from=from, to=to)
}

.verifyBAMAttrs <- function(bam, attr) {
    if( !(attr %in% names(bam@edgeData@defaults)))
        stop("'attr' not found: ", sQuote(attr))
}

.set_attrs <- function(g, from, to, attr, value)
{  
    nodeNames <- g@nodes
    req_ft <- .align_from_to(from, to, nodeNames)
    ## remove dups
    indx <- duplicated(paste(req_ft[,"from"], req_ft[,"to"], sep ="_"))
    req_ft <- req_ft[!indx, ,drop = FALSE]
    if(nrow(req_ft) > 0 )
        .verifyEdges(g, req_ft[,1], req_ft[,2])
    else
        stop("edges specified could not be found in \"self\"")
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
    
    ft <- .Call(graph_bitarray_rowColPos, g@edgeSet@bit_vector)

    if (!isDirected(g)) {
        ## normalize from/to
        valIndx <- seq_len(length(value))
        tmp <- .mg_undirectEdges(req_ft[ , 1], req_ft[, 2], valIndx)
        req_ft <- cbind("from"= tmp[["from"]],"to" = tmp[["to"]])
        value <- value[tmp[["weight"]]]
    }

     req_i <- structure(match(req_ft, nodeNames), dim = dim(req_ft))
     colnames(req_i) <- c("from", "to")
     req_i <- data.frame(req_i)
     idx <- order(req_i[,2], req_i[,1])
     req_i <- req_i[idx, ]
     value <- value[idx]
    if(attr == "weight") {
        attrBit <- g@edgeSet@bit_vector
        if(nrow(req_i)) { 
            ord <- .Call(graph_bitarray_getEdgeAttrOrder,  attrBit, 
                         as.integer(req_i[,"from"]), as.integer(req_i[,"to"]))
            g@edgeSet@bit_vector <- setBitCell(attrBit, req_i[,"from"], req_i[,"to"], 
                                                       rep(1L, nrow(req_i)))
            nt <- attr(g@edgeSet@bit_vector, "nbitset")
        } else {
            nt <- attr(attrBit, "nbitset")
            ord <- list(newLeftPos = integer(0), newRightPos = integer(0), 
                        origLeftPos = seq_len(nt), origRightPos = seq_len(nt))
        }

        newAttr <- vector(nt, mode = mode(value))
        newAttr[ord$origLeftPos] <- g@edgeSet@weights[ord$origRightPos]
        newAttr[ord$newLeftPos] <- value[ord$newRightPos]
        g@edgeSet@weights <- newAttr
    }else {
        attrBit <- g@userAttrPos@edgePos[[attr]]
        if(nrow(req_i)) { 
            ord <- .Call(graph_bitarray_getEdgeAttrOrder, attrBit, 
                         as.integer(req_i[,"from"]), as.integer(req_i[,"to"]))
            g@userAttrPos@edgePos[[attr]] <- setBitCell(attrBit, req_i[,"from"], req_i[,"to"], 
                                                                rep(1L, nrow(req_i)))
            nt <- attr(g@userAttrPos@edgePos[[attr]], "nbitset")
        } else {
            nt <- attr(attrBit, "nbitset")
            ord <- list(newLeftPos = integer(0), newRightPos = integer(0), 
                        origLeftPos = seq_len(nt), origRightPos = seq_len(nt))
        }
        newAttr <- vector(nt, mode = mode(value))
        newAttr[ord$origLeftPos] <- 
                     g@edgeSet@edge_attrs[[attr]][ord$origRightPos]
        newAttr[ord$newLeftPos] <- value[ord$newRightPos]
        g@edgeSet@edge_attrs[[attr]] <- newAttr
    }
    g
}

## graphBAM edgeData replacement methods 
setReplaceMethod("edgeData",
                 signature(self="graphBAM",
                           from="character", to="character",
                           attr="character", value="ANY"),
                 function(self, from, to, attr, value) {
                     .verifyAttrName(attr, names(self@edgeData@defaults))
                     lenFrom <- length(from)
                     lenTo <- length(to)
                     if(lenFrom != lenTo) {
                         if(lenFrom ==1)
                             from <- rep(from, lenTo)
                         else if (lenTo == 1)
                            to <- rep(to , lenFrom)
                         else
                            stop("'from', 'to' differ in length")
                     }
                     .verifyEdges(self, from, to)
                     .set_attrs(self, from, to, attr, value)

                 })

setReplaceMethod("edgeData",
                 signature(self="graphBAM",
                           from="character", to="missing",
                           attr="character", value="ANY"),
                 function(self, from, to, attr, value) {
                      .verifyAttrName(attr, names(self@edgeData@defaults))
                      eg <- edges(self,from)
                      to <- unlist(eg, use.names = FALSE)
                      len <- as.numeric(sapply(eg, length))
                      from <- rep(names(eg),len) 
                     .verifyEdges(self, from, to) 
                     .set_attrs(self, from, to, attr, value)
                 })

setReplaceMethod("edgeData",
                 signature(self="graphBAM",
                           from="missing", to="character",
                           attr="character", value="ANY"),
                 function(self, from, to, attr, value) {
                     .verifyAttrName(attr, names(self@edgeData@defaults))
                     eg <- inEdges(to, self)
                     eg <- eg[order(names(eg))]
                     from  <- unlist(eg, use.names = FALSE) 
                     len <- as.numeric(sapply(eg, length))
                     to <- rep(names(eg), len)
                     .verifyEdges(self, from, to)
                     .set_attrs(self, from, to, attr, value)
                 })
## graphBAM edgeDataDefaults replacement methods
setReplaceMethod("edgeDataDefaults", signature(self="graphBAM", attr="missing",
                     value="list"),
                 function(self, attr, value) {
                     attrDefaults(self@edgeData) <- value
                     wt <- self@edgeData@defaults[["weights"]]
                     if(!is.numeric(wt) && !is.null(wt))
                         stop("'weights' attribute must be numeric()")
                     ndsLen <- length(nodes(self))
                     nms <- names(value)
                     for(i in seq_along(value)) {
                         if(!(nms[i] %in% names(self@userAttrPos@edgePos))) {
                             if(nms[i] != "weight"){
                                 posBit <- .createZeroBitPos(ndsLen)
                                 self@userAttrPos@edgePos[[nms[i]]] <- posBit
                             }
                         }
                     }
                     self
                 })

            
setReplaceMethod("edgeDataDefaults", signature(self="graphBAM", attr="character",
                                               value="ANY"),
                 function(self, attr, value) {
                     attrDefaults(self@edgeData, attr) <- value
                     if(attr == "weight") {
                        wt <- self@edgeData@defaults[["weights"]]
                        if(!is.numeric(wt) && !is.null(wt))
                             stop("'weights' attribute must be numeric()")
                     } else{
                        ndsLen <- length(nodes(self))
                        if(!(attr %in% names(self@userAttrPos@edgePos))) {
                            posBit <- .createZeroBitPos(ndsLen)
                            self@userAttrPos@edgePos[[attr]] <- posBit
                        }
                     }
                     self
                 })

.createZeroBitPos <- function(ndsLen) {
    makebits(ndsLen * ndsLen, bitdim = c(ndsLen, ndsLen))
}

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
            .Call(graph_bitarray_getBitCell, object@edgeSet@bit_vector, from_i, to_i)
        })

setMethod("subGraph", signature(snodes="character", graph="graphBAM"),
        function(snodes, graph){ 
            origNodes <- nodes(graph)
            snodes <- sort(snodes)
            snodesIdx <- match(snodes, origNodes)
            if (any(is.na(snodesIdx))) {
                bad <- snodes[which(is.na(snodesIdx))]
                stop("'snodes' not in MultiGraph: ", pasteq(bad))
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
            
            res <- .Call(graph_bitarray_subGraph, graph@edgeSet@bit_vector, snodesIdx)
            graph@edgeSet@bit_vector <- res$bitVec
            graph@edgeSet@weights <- graph@edgeSet@weights[res$setPos]
            nms <- names(graph@edgeSet@edge_attrs)
            for(i in nms) {
                attrBit <- graph@userAttrPos@edgePos[[i]]
                res <- .Call(graph_bitarray_subGraph, attrBit, snodesIdx)
                graph@edgeSet@edge_attrs[[i]] <-  graph@edgeSet@edge_attrs[[i]][res$setPos]
                graph@userAttrPos@edgePos[[i]] <- res$bitVec
            }
            graph
        })

setMethod("edgeMatrix", "graphBAM",
        function(object, duplicates=FALSE) {
            bitvec <- object@edgeSet@bit_vector
            nds <- nodes(object)
            df <- .Call(graph_bitarray_rowColPos, bitvec)
            if (duplicates)
                df <- rbind(df, cbind(as.vector(df[, "to"]),
                            as.vector(df[, "from"])))
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
            nn <- nodes(object)
            if(!all(node %in% nn))
                stop("'node' not all in 'object'")
            df <- extractFromTo(object)
            indx <- ! ( (as.character(df[,"from"])  %in% node) |
            (as.character(df[,"to"])  %in% node))
            nIndx <- !(nn %in% node)
            bam <- graphBAM(df[indx,], nodes = nn[nIndx] , 
                    edgemode = edgemode(object))
            bam@edgeSet@weights <- object@edgeSet@weights[indx]
            bam@edgeData@defaults <- object@edgeData@defaults
            bam@nodeData@defaults <- object@nodeData@defaults
            nms <- names(object@edgeSet@edge_attrs)
            snodesIdx <- which(nIndx)
            for(i in nms) {
                attrBit <- object@userAttrPos@edgePos[[i]]
                res <- .Call(graph_bitarray_subGraph, attrBit, snodesIdx)
                bam@edgeSet@edge_attrs[[i]] <-  object@edgeSet@edge_attrs[[i]][res$setPos]
                bam@userAttrPos@edgePos[[i]] <- res$bitVec
            }
            nms <- names(object@nodeData@defaults)
            len <- length(snodesIdx)
            tmp <- makebits(len)
            for(i in nms){
                leftPos <- which(bitToLogical(object@userAttrPos@nodePos[[i]])[snodesIdx])
                rightPos <- which(bitToLogical(object@userAttrPos@nodePos[[i]]))[snodesIdx]
                bt <- setbitv(tmp, leftPos, rep(1L, length(leftPos)))
                # k <- .getNodeAttrValues(bt, indx)
                bam@userAttrPos@nodePos[[i]] <- bt
                bam@nodeData@data[[i]] <- object@nodeData@data[[i]][rightPos]
            }
            bam
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
    am <- graphAM(adjMat = as(from, "matrix"),
          edgemode = edgemode(from), values = list(weight=1))
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
     ## FIXME: graphBAM doesn't really handle edge attributes in the same way
     ## we can copy data over so it can be copied back, but it won't really
     ## be accessible in the new graphBAM object.
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
              req_i <- structure(match(req_ft, nn), dim = dim(req_ft))
              if(nrow(req_i)) {
                  ord <- .Call(graph_bitarray_getEdgeAttrOrder,
                               graph@edgeSet@bit_vector,
                               as.integer(req_i[,1]), as.integer(req_i[,2]))
                  graph@edgeSet@bit_vector <-
                      setBitCell(graph@edgeSet@bit_vector, req_i[,1], req_i[,2], 
                                         rep(1L, nrow(req_i)))
                  nt <- attr(graph@edgeSet@bit_vector, "nbitset")
              } else {
                 nt <- attr(graph@edgeSet@bit_vector, "nbitset")
                 ord <- list(newLeftPos = integer(0), newRightPos = integer(0), 
                          origLeftPos = seq_len(nt), origRightPos = seq_len(nt))
              }
              newAttr <- vector(nt, mode = "numeric")
              newAttr[ord$origLeftPos] <- graph@edgeSet@weights[ord$origRightPos]
              newAttr[ord$newLeftPos] <- weights[ord$newRightPos]
              graph@edgeSet@weights <- newAttr
              graph@edgeData@defaults <- graph@edgeData@defaults
              graph@edgeSet@edge_attrs <- graph@edgeSet@edge_attrs
              graph@userAttrPos@edgePos <- graph@userAttrPos@edgePos 
             
              graph@nodeData@defaults <- graph@nodeData@defaults
              graph@nodeData@data <- graph@nodeData@data
              graph@userAttrPos@nodePos <- graph@userAttrPos@nodePos
              graph
          
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

            nds <- sort(unique(c(nodes(object), node)))
            ndsLen <- length(nds)
            df <- extractFromTo(object)
            g <- graphBAM(df, nodes = nds, edgemode = edgemode(object))
            nms <- names(object@nodeData@defaults)
            
            for(i in nms){
                origNds  <- nodes(object)[(bitToLogical(object@userAttrPos@nodePos[[i]]))]
                indx <- match(origNds, nodes(g))
                bt <- makebits(ndsLen)
                bt <- setbitv(bt, indx, rep(1L, length(indx)))
                g@userAttrPos@nodePos[[i]] <- bt
                g@nodeData@data[[i]] <- object@nodeData@data[[i]]
            }
            g@nodeData@defaults <- object@nodeData@defaults
            g@edgeSet@edge_attrs <- object@edgeSet@edge_attrs
            g@edgeData@defaults <- object@edgeData@defaults
            nms <- names(object@userAttrPos@edgePos)
            for(i in nms){
                ft <- .Call(graph_bitarray_rowColPos,object@userAttrPos@edgePos[[i]])
                posBit <- .createZeroBitPos(ndsLen)
                from.nodes.new <- match(nodes(object)[ft[, 'from']], nodes(g))
                to.nodes.new   <- match(nodes(object)[ft[, 'to']],   nodes(g))
                g@userAttrPos@edgePos[[i]] <- setBitCell(posBit,
                                                         from.nodes.new,
                                                         to.nodes.new,
                                                         rep(1L, nrow(ft)))
            } 
            g
        })

setReplaceMethod("edgemode", c("graphBAM", "character"),
    function(object, value)
{
    if (length(value) != 1L) stop("'edgemode' must be length one")
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
           stop("'edgemode' must be 'directed' or 'undirected', was ",
                sQuote(value)))
    object
})

.remEdge <- function(from, to, graph)
{   
   
    lenFrom <- length(from)
    lenTo <- length(to)
    if(lenFrom != lenTo) {
        if(lenFrom ==1)
            from <- rep(from, lenTo)
        else if (lenTo == 1)
            to <- rep(to , lenFrom)
        else
            stop("arguments 'from', 'to' differ in length")

    }
    .verifyEdges(graph, from, to)
    nn <- nodes(graph)
    req_ft <- .align_from_to(from, to, nn)
    req_from <-  match(req_ft[,"from"], nn)
    req_to <- match(req_ft[,"to"], nn)
    nms <- names(graph@edgeSet@edge_attrs)
    if(!is.null(nms)) {
        for(i in nms){
            graph@userAttrPos@edgePos[[i]] <-  
            setBitCell(graph@userAttrPos@edgePos[[i]], req_from, req_to,
                rep(0L, nrow(req_ft)))
            if(length(req_from)) {
                ord <- .Call(graph_bitarray_getEdgeAttrOrder, graph@userAttrPos@edgePos[[i]],
                             as.integer(req_from), as.integer(req_to))
            } else {

                nt <- attr(graph@userAttrPos@edgePos[[i]], "nbitset")
                ord <- list(newLeftPos = integer(0), newRightPos = integer(0), 
                        origLeftPos = seq_len(nt), origRightPos = seq_len(nt))
            }
            graph@edgeSet@edge_attrs[[i]] <- graph@edgeSet@edge_attrs[[i]][ord$origLeftPos]
        }
    }
    graph@edgeSet@bit_vector <- setBitCell(graph@edgeSet@bit_vector,
                                           match(from, nn),
                                           match(to, nn),
                                           rep(0L, nrow(req_ft)))
    ord <- .Call(graph_bitarray_getEdgeAttrOrder,  graph@edgeSet@bit_vector , 
            as.integer(req_from), as.integer(req_to))
    graph@edgeSet@weights <- graph@edgeSet@weights[ord$origLeftPos]
    graph
}

setMethod("removeEdge", c("character", "character", "graphBAM"),
          function(from, to, graph) .remEdge(from, to, graph))


setMethod("removeEdgesByWeight", c("graphBAM"),
      function(graph, lessThan, greaterThan ){
          if(missing(lessThan) && missing(greaterThan))
              stop("specify 'lessThan' or 'greaterThan'")
          if(!missing(lessThan) && !missing(greaterThan)){
              indx <- ( graph@edgeSet@weights >= lessThan & 
                      graph@edgeSet@weights <= greaterThan)
          } else if(missing(lessThan)){
              indx <- graph@edgeSet@weights <= greaterThan
          }else if(missing(greaterThan)){
              indx <- graph@edgeSet@weights >= lessThan
          }
          nn <- nodes(graph)
          bt <- .Call(graph_bitarray_removeEdges, graph@edgeSet@bit_vector, indx)
          tempBit <-  graph@edgeSet@bit_vector & (bt)
          attributes(tempBit) <- attributes(graph@edgeSet@bit_vector)
          attr(tempBit, "nbitset") <- .Call(graph_bitarray_sum, tempBit)
          ft <- .Call(graph_bitarray_rowColPos, tempBit)

          tp <- graph@edgeSet@bit_vector & (!bt)
          attributes(tp) <- attributes(graph@edgeSet@bit_vector)
          attr(tp, "nbitset") <- .Call(graph_bitarray_sum, tp)
          ft2 <-  .Call(graph_bitarray_rowColPos, tp)

          graph@edgeSet@weights <- graph@edgeSet@weights[indx]
          graph@edgeSet@bit_vector <- bt
          nms <- names(graph@userAttrPos@edgePos)
          if(!is.null(nms)){
              for(i in nms) {
                  graph@userAttrPos@edgePos[[i]] <- 
                  setBitCell(graph@userAttrPos@edgePos[[i]], 
                      ft2[,"from"], ft2[,"to"], rep(0L, nrow(ft2)))
                  if(nrow(ft2)) {
                  ord <- .Call(graph_bitarray_getEdgeAttrOrder,
                      graph@userAttrPos@edgePos[[i]], as.integer(ft2[,"from"]),
                      as.integer(ft2[, "to"]))
                  } else {
                      nt <- attr(graph@userAttrPos@edgePos[[i]], "nbitset")
                      ord <- list(newLeftPos = integer(0), newRightPos = integer(0), 
                                  origLeftPos = seq_len(nt), origRightPos = seq_len(nt))
                  }
                  graph@edgeSet@edge_attrs[[i]] <- 
                    graph@edgeSet@edge_attrs[[i]][ord$origLeftPos]
              }
          }
          graph
      })

setReplaceMethod("nodes", c("graphBAM", "character"),
                 function(object, value) {
                     stop("operation not supported")
                 })

.getUnionWeights <- function(attrType, g1, g2, funList) {
    len <- length(attrType$from)
    attr1 <- vector(len, mode = "numeric")
    attr1[seq_len(len)] <- NA
    ## from x
    k <- (as.numeric(attrType$from) ==1)
    attr1[k]  <- g1@edgeSet@weights[attrType$indx1[k]] 
    ## from y 
    k <- (as.numeric(attrType$from) == 2)
    attr1[k]  <- g2@edgeSet@weights[attrType$indx2[k]]
    ## resolve union
    k <- (as.numeric(attrType$from) ==0)
    if(any(k)) {
        val1 <- g1@edgeSet@weights[attrType$indx1[k]]
        val2 <- g2@edgeSet@weights[attrType$indx2[k]]

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
            tmp[seq_len(lp)] <- NA
            tmp[pt] <-  val1[pt]
            attr1[k]  <- tmp
        } 
    } 
    attr1
}

.getNodeAttrVec <- function(g, att) {
  unlist(nodeData(g, attr = att), use.names = FALSE)
}

    
.getUnionAttrs <- function(att, attrType, x , y, funList  ) {
    len <- length(attrType$from)
    indx <- as.numeric(attrType$from)
    if(att %in% names(x@edgeSet@edge_attrs))
        mds <- mode(x@edgeSet@edge_attrs)
    else if(att %in% names(y@edgeSet@edge_attrs))
        mds <- mode(y@edgeSet@edge_attrs)

    attr1 <- vector(len , mode = mds)
    attr1[seq_len(len)] <- NA
    ## from x
    k <- (as.numeric(attrType$from) ==1)
    if(att  %in% names(x@edgeSet@edge_attrs)) {
        xAttr <- .retAttrVec(x, att)
        attr1[k]  <- xAttr[ attrType$indx1[k]] 
    }
    ## from y 
    k <- (as.numeric(attrType$from) == 2)
    if(att  %in% names(y@edgeSet@edge_attrs)) {
        yAttr <- .retAttrVec(y, att)
        attr1[k]  <- yAttr[ attrType$indx2[k]]
    }
    ## resolve union
    k <- (as.numeric(attrType$from) ==0)
    if(any(k)) {
        if(att %in% names(x@edgeSet@edge_attrs))
            val1 <- xAttr[ attrType$indx1[k]]
        else 
            val1  <- yAttr[ attrType$indx2[k]]

        if(att %in% names(y@edgeSet@edge_attrs))
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
            tmp[seq_len(lp)] <- NA
            tmp[pt] <-  val1[pt]
            attr1[k]  <- tmp
        } 
    }
    attr1
}

.getEdgeIntersect <- function(e1, e2) {
    dr1 <- isDirected(e1)
    dr2 <- isDirected(e1)
    if(dr1 != dr2)
        stop("edges should both be directed or undirected")
    theMode <- if (dr1 && dr2) "directed" else "undirected"
    e1Attr <- names(e1@edge_attrs)
    e2Attr <- names(e2@edge_attrs) 
    commonAttr <- intersect(e1Attr, e2Attr)
    bv <- e1@bit_vector & e2@bit_vector
    attributes(bv) <- attributes(e1@bit_vector)
    attr(bv, "nbitset") <- ns <- .Call(graph_bitarray_sum, bv)
    c0 <- character(0)
    df <- data.frame(from = c0, to = c0, weight = numeric(0), stringsAsFactors = FALSE)
    edge_set <- .makeMDEdgeSet(es_name = 1, es = df,
                               is_directed = (theMode == "directed"), nodes = c0,
                               ignore_dup_edges = FALSE)
    edge_set@bit_vector <- bv
    edge_set
}


.getBAMIntersect <- function(g1, g2, edge_set, funList) {
    nn <- intersect(nodes(g1), nodes(g2))
    ans <- new("graphBAM", edgeSet= edge_set, nodes =nn)
    e1Attr <- names(g1@edgeSet@edge_attrs)
    e2Attr <- names(g2@edgeSet@edge_attrs) 
    commonAttr <- intersect(e1Attr, e2Attr)

    attrType <- .Call(graph_bitarray_Intersect_Attrs, edge_set@bit_vector,
                      g1@edgeSet@bit_vector, g2@edgeSet@bit_vector)
    if(!is.null(funList)) {
        fIndx <- names(funList) %in% c(commonAttr, "weight")
        if(!all(fIndx))
            stop("attributes in 'funList' not in edge attributes: ",
                 pasteq(names(funList)[fIndx]))
    }

    if(length(attrType$from) >0) {
        edge_set@weights <- .getIntersectAttrs("weight", attrType, g1@edgeSet@weights,
                                                       g2@edgeSet@weights, funList)
    }
    for(i in commonAttr) {
        ans@userAttrPos@edgePos[[i]] <- edge_set@bit_vector
        xAttr <- .retAttrVec(g1, i)
        yAttr <- .retAttrVec(g2, i)
        if(length(attrType$from) >0) {
            edge_set@edge_attrs[[i]] <- .getIntersectAttrs(i, attrType, xAttr, yAttr, funList)
        }
    }
    ans@edgeSet <- edge_set
    ans
}

.bamIntersect <- function(g1, g2, funList) {

    e1 <- g1@edgeSet
    e2 <- g2@edgeSet
    dr1 <- isDirected(e1)
    dr2 <- isDirected(e1)
    if(dr1 != dr2)
        stop("edges should both be directed or undirected")
    theMode <- if (dr1 && dr2) "directed" else "undirected"
    e1Attr <- names(e1@edge_attrs)
    e2Attr <- names(e2@edge_attrs) 
    commonAttr <- intersect(e1Attr, e2Attr)
    if(!is.null(funList)) {
        fIndx <- names(funList) %in% c(commonAttr, "weight")
        if(!all(fIndx))
            stop("attributes in 'funList' not in edge attributes: ",
                 pasteq(names(funList)[fIndx]))
    }
    bv <- e1@bit_vector & e2@bit_vector
    attributes(bv) <- attributes(e1@bit_vector)
    attr(bv, "nbitset") <- ns <- .Call(graph_bitarray_sum, bv)
    c0 <- character(0)
    df <- data.frame(from = c0, to = c0, weight = numeric(0), stringsAsFactors = FALSE)
    edge_set <- .makeMDEdgeSet(es_name = 1, es = df,
                                is_directed = (theMode == "directed"), nodes = c0,
                                ignore_dup_edges = FALSE)
    edge_set@bit_vector <- bv
   
    
    nn <- intersect(nodes(g1), nodes(g2))
    ans <- new("graphBAM", edgeSet= edge_set, nodes =nn)
    attrType <- .Call(graph_bitarray_Intersect_Attrs, edge_set@bit_vector,
        e1@bit_vector, e2@bit_vector)
    
    if(length(attrType$from) >0) {
        edge_set@weights <- .getIntersectAttrs("weight", attrType, e1@weights,
            e2@weights, funList)
    }
    for(i in commonAttr) {
        ans@userAttrPos@edgePos[[i]] <- edge_set@bit_vector
        xAttr <- .retAttrVec(g1, i)
        yAttr <- .retAttrVec(g2, i)
        if(length(attrType$from) >0) {
           edge_set@edge_attrs[[i]] <- .getIntersectAttrs(i, attrType, xAttr, yAttr, funList)
        }
   }
   ans@edgeSet <- edge_set
   ans
}

.getUnionNodeUserAttrPos <- function(g, sg1, sg2) {
    cmnAttrs <- intersect(names(sg1@nodeData@defaults), names(sg2@nodeData@defaults))
    unqAttrs <- unique(c(names(sg1@nodeData@defaults), names(sg2@nodeData@defaults)))
    singleAttrs <- unqAttrs[!( unqAttrs %in% cmnAttrs)]

    n1 <- structure(lapply(cmnAttrs, function(i){
                len <- length(g@nodeData@data[[i]])
                bt <- makebits(len)
                bt <- setbitv(bt, seq_len(len), rep(1L, len))
            }),names = cmnAttrs) 

    n2 <- structure(lapply(singleAttrs, function(i){
                if(i %in% names(sg1@userAttrPos@nodePos))
                    return(sg1@userAttrPos@nodePos[[i]])
                else if(i %in% names(sg2@userAttrPos@nodePos))
                    return(sg2@userAttrPos@nodePos[[i]])
            }), names=  singleAttrs)
    c(n1, n2)
}

.getIntNodeUserAttrPos <- function(sg1 , sg2) {
    nattr <- intersect(names(sg1@nodeData@defaults), names(sg2@nodeData@defaults))
    structure(lapply(nattr, function(i) {
        bv <- sg1@userAttrPos@nodePos[[i]] & sg2@userAttrPos@nodePos[[i]]
        attributes(bv) <- attributes(sg1@userAttrPos@nodePos[[i]])
        attr(bv, "nbitset") <- ns <- .Call(graph_bitarray_sum, bv)
        bv
    }), names = nattr)
}

.getIntersectAttrs <- function(att, attrType, xAtt , yAtt, funList  )  {
    len <- length(attrType$from)
    from1 <-  attrType$indx1
    from2 <-  attrType$indx2
    k <- (as.numeric(attrType$from) ==0)
    attr1 <- vector(sum(k), mode =mode(xAtt))
    attr1[seq_len(sum(k))] <- NA
    val1 <- xAtt[attrType$indx1[k]]
    val2 <- yAtt[attrType$indx2[k]]

    if(!is.null(funList) && (att %in% names(funList))) {
        attr1[k] <- sapply(seq_len(sum(k)), function(p) {
                    return(funList[[att]](val1[[p]], val2[[p]]))
                })
    } else if (is.vector(val1) && is.vector(val2)) {
         eqInd <- sapply(seq_len(length(val1)), function(x){
                     identical(val1[x], val2[x])
                 })
         pt <-  which(eqInd)
         attr1[pt] <-  val1[pt]
    } 
    attr1
}

setMethod("graphIntersect", c("graphBAM", "graphBAM"),
           function(x, y, nodeFun, edgeFun, ...){
    nn <- intersect(nodes(x), nodes(y))
    nnLen <- length(nn)
    if(nnLen ==0) {
        dr1 <- isDirected(x)
        dr2 <- isDirected(y)
        if(dr1 != dr2)
            stop("'x' and 'y' should both be directed or undirected")
        theMode <- if (dr1) "directed" else "undirected"
        c0 <- character(0)
        df <- data.frame(from = c0, to = c0, weight = numeric(0), 
                stringsAsFactors = FALSE)
        ans <- graphBAM(df, edgemode = theMode)
        return(ans)
    }
    sg1 <- if (nnLen == numNodes(x)) x else subGraph(nn, x)
    sg2 <- if (nnLen == numNodes(y)) y else subGraph(nn, y)
    if(missing(nodeFun))
        nodeFun <- NULL
    if(missing(edgeFun))
        edgeFun <- NULL
    
    edge_set <- .getEdgeIntersect(sg1@edgeSet, sg2@edgeSet)
    ans <- .getBAMIntersect(sg1, sg2, edge_set, edgeFun)
    ans@edgeData@defaults <- 
        .retEdgeIntersectDefaults(sg1@edgeData@defaults, sg2@edgeData@defaults)
    ans@nodeData@defaults <- .retNodeIntersectDefaults(sg1, sg2)
    ans@userAttrPos@nodePos <- .getIntNodeUserAttrPos(sg1, sg2)
    ans@nodeData@data <- .nodeIntersect(sg1, sg2, ans, nodeFun)
    ans
})

.getIntersectEdgeUserAttrPos <- function(edge_set) {
    nms <- names(edge_set@edge_attrs)
    structure(lapply(nms, function(){
        edge_set@bit_vector
    }), names = nms)
    
}

.retNodeUnionDefaults <- function(sg1, sg2) {
    cmnAttrs <- intersect(names(sg1@nodeData@defaults), names(sg2@nodeData@defaults))
    unqAttrs <- unique(c(names(sg1@nodeData@defaults), names(sg2@nodeData@defaults)))
    singleAttrs <- unqAttrs[!( unqAttrs %in% cmnAttrs)]
    cmn <- structure(lapply(cmnAttrs, function(x) {
                if(identical(sg1@nodeData@defaults[[x]], sg2@nodeData@defaults[[x]])) sg1@nodeData@defaults[[x]] else NA
            }), names = cmnAttrs)
    sng <- structure(lapply(singleAttrs, function(x) {
               if(x %in% names(sg1@nodeData@defaults))
                    sg1@nodeData@defaults[[x]] 
               else 
                    sg2@nodeData@defaults[[x]] 
            }), names = singleAttrs)
    c(cmn, sng)

}

.retNodeIntersectDefaults <- function(sg1, sg2) {
    cmnAttrs <- intersect(names(sg1@nodeData@defaults), names(sg2@nodeData@defaults))
    structure(lapply(cmnAttrs, function(x) {
                if(identical(sg1@nodeData@defaults[[x]], sg2@nodeData@defaults[[x]])) sg1@nodeData@defaults[[x]] else NA
            }), names = cmnAttrs)

}


.retEdgeIntersectDefaults <- function(d1, d2) {
    cmnAttrs <- intersect(names(d1), names(d2))
    structure(lapply(cmnAttrs, function(x) {
                     if(identical(d1[[x]], d2[[x]])) d1[[x]] else NA
            }), names = cmnAttrs)

}



.retEdgeUnionDefaults <- function(sg1, sg2) {
    cmnAttrs <- intersect(names(sg1@edgeData@defaults), names(sg2@edgeData@defaults))
    unqAttrs <- unique(c(names(sg1@edgeData@defaults), names(sg2@edgeData@defaults)))
    singleAttrs <- unqAttrs[!( unqAttrs %in% cmnAttrs)]
    cmn <- structure(lapply(cmnAttrs, function(x) {
                if(identical(sg1@edgeData@defaults[[x]], sg2@edgeData@defaults[[x]])) sg1@edgeData@defaults[[x]] else NA
            }), names = cmnAttrs)
    
    sng <- structure(lapply(singleAttrs, function(x) {
               if(x %in% names(sg1@edgeData@defaults))
                    sg1@edgeData@defaults[[x]] 
               else 
                    sg2@edgeData@defaults[[x]] 
            }), names = singleAttrs)
    c(cmn, sng)
}

.nodeIntersect <- function(sg1, sg2, ans, funList){
    cmn <- intersect(names(sg1@nodeData@defaults), names(sg2@nodeData@defaults))
    nattr <- structure(lapply(cmn, function(x) {
                 attr1 <- .getNodeAttrVec(sg1, x)
                 attr2 <- .getNodeAttrVec(sg2, x)
                 len <- length(attr1)
                 if(!is.null(funList) && (x %in% names(funList))) {
                    res <- sapply(seq_len(len), function(p) {
                             return(funList[[x]](attr1[[p]], attr2[[p]]))
                           })
                 } else if (is.vector(attr1) && is.vector(attr2)) {
                    indx <- which(sapply(seq_len(len), function(p){
                                identical(attr1[p], attr2[p])
                           }))
                    res <- rep(NA, len)
                    res[indx] <- attr1[indx]
                }
                res
                res[bitToLogical(ans@userAttrPos@nodePos[[x]])]
             }), names = cmn)
     nattr
}

.scaleGraphBAM <- function(g, theNodes, edgemode) {
    if(all(nodes(g) %in% theNodes) && length(nodes(g)) == length(theNodes))
        return(g)
    else {
    ndsLen <- length(theNodes)
    df <- extractFromTo(g)
    bam <- graphBAM(df, nodes = theNodes, edgemode= edgemode, 
                     ignore_dup_edges = FALSE)
    bam@edgeSet@weights <- g@edgeSet@weights
    bam@edgeSet@edge_attrs <- g@edgeSet@edge_attrs
    bam@edgeData@defaults <- g@edgeData@defaults
    nms <- names(g@nodeData@defaults)
    for(i in nms){
        origNds  <- nodes(g)[(bitToLogical(g@userAttrPos@nodePos[[i]]))]
        indx <- match(origNds, theNodes)
        bt <- makebits(ndsLen)
        bt <- setbitv(bt, indx, rep(1L, length(indx)))
        bam@userAttrPos@nodePos[[i]] <- bt
        bam@nodeData@data[[i]] <- g@nodeData@data[[i]]
    }
    bam@nodeData@defaults <- g@nodeData@defaults
    nms <- names(g@userAttrPos@edgePos)
    for(i in nms) {
        ft <- .Call(graph_bitarray_rowColPos, g@userAttrPos@edgePos[[i]])
        posBit <- .createZeroBitPos(ndsLen)
        bam@userAttrPos@edgePos[[i]] <- setBitCell(posBit, ft[,"from"], ft[,"to"], rep(1L, nrow(ft)))
    }
    return(bam)
   }
}



.bamUnion <- function(g1, g2, theNodes, nodeFun, edgeFun) { 
    dr1 <- isDirected(g1)
    dr2 <- isDirected(g2)
    if(dr1 != dr2)
        stop("edges should both be directed or undirected")
    theMode <- if (dr1 && dr2) "directed" else "undirected"
    e1Attr <- names(g1@edgeSet@edge_attrs)
    e2Attr <- names(g2@edgeSet@edge_attrs) 
    unionAttr <- unique(union(e1Attr, e2Attr))
    if(!is.null(edgeFun)) {
        fIndx <-  names(edgeFun) %in% c(unionAttr, "weight")
        if(!all(fIndx))
            stop("attributes in 'edgeFun' not in edge attributes: ",
                 pasteq(names(edgeFun)[fIndx]))
    }
    bam1 <- .scaleGraphBAM(g1, theNodes, theMode)
    bam2 <- .scaleGraphBAM(g2, theNodes, theMode)
    bv <- bam1@edgeSet@bit_vector | bam2@edgeSet@bit_vector
    attributes(bv) <- attributes(bam1@edgeSet@bit_vector)
    attr(bv, "nbitset") <- ns <- .Call(graph_bitarray_sum, bv)
    
    c0 <- character(0)
    df <- data.frame(from = c0, to = c0, weight = numeric(0))
    edge_set <- .makeMDEdgeSet(es_name = 1, es =df,
                   is_directed = (theMode == "directed"),
                  nodes = theNodes, ignore_dup_edges = FALSE)
    edge_set@bit_vector <- bv
    ans <- new("graphBAM", edgeSet= edge_set, nodes =theNodes)

    cmnBit <- bam1@edgeSet@bit_vector & bam2@edgeSet@bit_vector
    attributes(cmnBit) <- attributes(bam1@edgeSet@bit_vector)
    attr(cmnBit, "nbitset") <- .Call(graph_bitarray_sum, cmnBit)

    fromOneBit <- bam1@edgeSet@bit_vector & (!cmnBit)
    attributes(fromOneBit) <- attributes(bam1@edgeSet@bit_vector)
    attr(fromOneBit, "nbitset") <- .Call(graph_bitarray_sum, fromOneBit)

    fromTwoBit <- bam2@edgeSet@bit_vector & (!cmnBit)
    attributes(fromTwoBit) <- attributes(bam2@edgeSet@bit_vector)
    attr(fromTwoBit, "nbitset") <- .Call(graph_bitarray_sum, fromTwoBit)
    attrType <- .Call(graph_bitarray_Union_Attrs, bv, cmnBit, fromOneBit,
                       fromTwoBit)
    if(length(attrType$from) >0) {
          edge_set@weights <- as.numeric(.getUnionWeights(attrType, bam1, bam2, edgeFun))
    }
    if(!is.null(unionAttr)) {              
        for(i in unionAttr) {
            ans@userAttrPos@edgePos[[i]] <- edge_set@bit_vector
            edge_set@edge_attrs[[i]] <-  .getUnionAttrs(i, attrType, bam1, bam2, edgeFun)
        }
    }
    ans@edgeSet <- edge_set
    ans@nodeData@data <- .nodeUnion(g1, g2, nodeFun)
    ans@userAttrPos@nodePos <- .getUnionNodeUserAttrPos(ans,bam1, bam2)
    ans
  }


setMethod("graphUnion", c("graphBAM", "graphBAM"), 
        function(x, y, nodeFun, edgeFun, ...) {
   theNodes <- unique(c(nodes(x), nodes(y)))
   nnLen <- length(theNodes)
   if(nnLen ==0) {
       dr1 <- isDirected(x)
       dr2 <- isDirected(y)
       if(dr1 != dr2)
           stop("'x' and 'y' should both be directed or undirected")
       theMode <- if (dr1) "directed" else "undirected"
       c0 <- character(0)
       df <- data.frame(from = c0, to = c0, weight = numeric(0), 
                stringsAsFactors = FALSE)
       ans <- graphBAM(df, edgemode = theMode)
       return(ans)
   }
   if(missing(nodeFun))
       nodeFun <- NULL
   if(missing(edgeFun))
       edgeFun <- NULL
   ans <- .bamUnion(x, y, theNodes, nodeFun, edgeFun)
   ans@nodeData@defaults <- .retNodeUnionDefaults(x, y)
   ans@edgeData@defaults <- .retEdgeUnionDefaults(x, y)
   ans 
})

.nodeUnion <- function(g1, g2, funList) {
    xAttr <-  names(g1@nodeData@defaults)
    yAttr <-  names(g2@nodeData@defaults)
    if(!is.null(funList)) {
        fIndx <-  names(funList) %in% c(xAttr, yAttr)
        if(!all(fIndx))
            stop("attributes in 'nodeFun' not in node attributes: ",
                 pasteq(names(funList)[fIndx]))
    }
    ndX <- nodes(g1)
    ndY <- nodes(g2)
    ndAns <- unique(c(ndX, ndY))
    unionAttrs <- union(xAttr, yAttr)
    commonAttrs <- intersect(xAttr,yAttr)
    singleAttrs <- unionAttrs[!unionAttrs %in% commonAttrs]
    cmnNds <- intersect(ndX, ndY)
    fxNds <-  ndX[!ndX %in% cmnNds]
    fyNds <-  ndY[!ndY %in% cmnNds]
    ### deal with single attrs
    n1 <- sapply(singleAttrs, function(k){
               if(k %in% xAttr){
                    att <- .getNodeAttrVec(g1, k)
               } else if(k %in% yAttr){
                    att <- .getNodeAttrVec(g2, k)
               }
               list(att)
           })
    
    n2 <- sapply(commonAttrs, function(k) {
               att <- rep(NA, length(ndAns))
               ##from X
               indx <- match(fxNds, ndAns)
               attr1 <- .getNodeAttrVec(g1, k)
               attr2 <- .getNodeAttrVec(g2, k)
               att[indx] <- attr1[match(fxNds,ndX)]
               ##from Y
               indx <- match(fyNds, ndAns)
               att[indx] <- attr2[match(fyNds,ndY)]
               
               if(!is.null(funList) && (k %in% names(funList))) {
                    tmp <- sapply(cmnNds, function(p){
                            dX <- match(p, ndX)
                            dY <- match(p, ndY)
                            funList[[k]](attr1[[dX]], attr2[[dY]])
                        }, USE.NAMES = FALSE)
                    indx <- match(cmnNds, ndAns)
                    att[indx] <- tmp
                } else if (is.vector(attr1) && is.vector(attr2)){
                    tmp <- sapply(cmnNds, function(p){
                            dX <- match(p, ndX)
                            dY <- match(p, ndY)
                            identical(attr1[[dX]], attr2[[dY]])
                        }, USE.NAMES = FALSE)
                    indx <- match(cmnNds[tmp], ndAns)
                    att[indx] <- attr1[match(cmnNds[tmp], ndX)]
               }
               list(att)
            })
     c(n1,n2)
 }

setMethod("nodeDataDefaults", 
        signature(self="graphBAM", attr="missing"),
        function(self, attr){
            attrDefaults(self@nodeData)
        })

setMethod("nodeDataDefaults", 
        signature(self="graphBAM", attr="character"),
        function(self, attr){
            attrDefaults(self@nodeData, attr)
        })

setReplaceMethod("nodeDataDefaults", 
        signature(self="graphBAM", attr="missing", value="list"),
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
    signature(self="graphBAM", attr="character", value="ANY"),
    function(self, attr, value) {
        attrDefaults(self@nodeData, attr) <- value
        ndsLen <- length(nodes(self))
        if(! (attr %in% names(self@userAttrPos@nodePos))){
            bt <- makebits(ndsLen)
            self@userAttrPos@nodePos[[attr]] <- bt
        }
        self
    })

## Node data replacement methods 

.getNodeAttrOrder <- function(x, indx) {
    len <- attr(x, "bitlen")
    nset <- attr(x, "nbitset")
    if (is.null(len)) len <- length(x) * 8L
    k <- p <- 1
    setIndx =  1
    attrIndx = 1 
    k2<- k3 <- k1 <- 1
    nval = vector(length(indx), mode ="integer")
    npos <- nval
    fpos = vector(max(c(nset, length(indx))), mode ="integer")
    fval = vector(max(c(nset, length(indx))), mode ="integer")
    mm <- length(indx)
    for( i in seq_len(len)) {
        bt <- testbit(x, i)
        intIndx <- indx[attrIndx]
        if(bt) {
            if(intIndx == i) {
                nval[k2] = attrIndx 
                npos[k2] = k3 
                k2 <- k2 + 1 
                k3 <- k3 + 1
                if(attrIndx < mm)
                    attrIndx = attrIndx +1 
            }
            if(intIndx != i){
                  fpos[k1] <- setIndx 
                  fval[k1] <- k1 
                  k1 <- k1+1
                  k3 <- k3+ 1
                }
            setIndx <- setIndx +1
         }
    }     
    length(fpos) <- k1-1
    length(fval) <- k1-1 
    attributes(fval) <- list("origPos" = fpos)
    attributes(nval) <- list("newPos" = npos)
    list( origVal = fval, newVal = nval)
}

setReplaceMethod("nodeData",
        signature(self = "graphBAM", n="character", attr="character", value="ANY"),
        function(self, n, attr, value) {
            .nodeDataReplaceNodeGiven(self, n ,attr, value)
        })

setReplaceMethod("nodeData",
        signature(self = "graphBAM", n="missing", attr="character", value="ANY"),
        function(self, n, attr, value) {
            .nodeDataReplaceNodeMissing(self, attr, value)
        })

 .getNodeAttrPos <- function(x, indx) {
    bt <- bitToLogical(x)
    leftPos <- which(bt[indx])
    cs <- cumsum(bt) 
    k <- indx[bt[indx]]
    rightPos <- cs[k ]
    list(leftPos= leftPos, rightPos = rightPos)
}
## Node data accces methods 

setMethod("nodeData",
        signature(self = "graphBAM", n = "character", attr = "character"),
        function(self, n, attr) {
            .nodeDataRetrieve(self, n, attr)   
        })

setMethod("nodeData",
        signature(self = "graphBAM", n = "missing", attr = "character"),
        function(self, n, attr) {
            if(length(attr) != 1L)
                stop("'attr' must specify a single attribute name")
            nds <- nodes(self)
            nodeData(self, n= nds, attr= attr)
        })

setMethod("nodeData",
        signature(self = "graphBAM", n = "character", attr = "missing"),
        function(self, n, attr) {
            nds <- nodes(self)
            .verifyNodes(n ,nds)
            nms <- names(self@nodeData@defaults)
            structure(lapply(nms, function(x) {
                     nodeData(self, n, x)
                }), names = nms)
        })

setMethod("nodeData",
        signature(self = "graphBAM", n = "missing", attr = "missing"),
        function(self, n, attr) {
           nds <- nodes(self)
           nms <- names(self@nodeData@defaults)
           structure(lapply(nms, function(x) {
                      nodeData(self, nds, x)
                }), names = nms)

        })

