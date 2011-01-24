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
    ft <- .Call("graph_bitarray_rowColPos", g@edgeSet@bit_vector)
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

.eAttrsFun <- function(self, from, attr) {
    nodeNames <- self@nodes
    indx <- which(nodeNames %in% from)
    numNodes <- length(nodeNames)
    bv <- self@edgeSet@bit_vector
    .verifyBAMAttrs(self, attr)
    val <- graph:::.retAttrVec(self, attr)
    ft <- .Call(graph:::graph_bitarray_rowColPos, bv)
    if(!isDirected(self)){
        df <- cbind(from=ft[,"to"], to = ft[,"from"])
        ft <- rbind(ft,df)
        val <- c(val,val)
    }
    tmp <- seq_len(length(val))
    ft <- data.frame(ft, tmp, stringsAsFactors = FALSE )
    ft <- ft[ ft[,"from"] %in% indx,]
    if(nrow(ft) == 0)
         stop("Edges specified in \"from\" not found in \"self\"")

    nodeLbl <- paste( nodeNames[ft[,"from"]], nodeNames[ft[, "to"]],
            sep ="|")
    val <- val[ft[,"tmp"]][1:length(nodeLbl)]
    names(val) <- nodeLbl
    val
}

setMethod("edgeData", signature(self="graphBAM", from="missing", to="missing",
              attr="missing"),
      function(self, from, to, attr) {
              nodeNames <- self@nodes
              numNodes <- length(nodeNames)
              attr <- "weight"
              bv <- self@edgeSet@bit_vector
              ft <- .Call(graph:::graph_bitarray_rowColPos, bv)
              w <- graph:::.retAttrVec(self, attr)
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


setMethod("edgeData", signature(self="graphBAM", from="missing", to= "missing",
                attr="character"),
        function(self, from, to, attr){
                as.list(.eAttrsFun(self, from = names(edges(self)), attr= attr))       
        })

setMethod("edgeData", signature(self="graphBAM", from="character", to= "missing",
                attr="character"),
        function(self, from, to, attr){
            as.list(.eAttrsFun(self, from, attr))       
        })

setMethod("edgeData", signature(self="graphBAM", from="missing", to= "character",
                attr="character"),
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
                    stop("Edges specified in \"to\" not found in \"self\"")
                .verifyEdges(self, nodeNames[ft[,"from"]], nodeNames[ft[,"to"]])
                nodeLbl <- paste( nodeNames[ft[,"from"]], nodeNames[ft[, "to"]],
                        sep ="|")
                val <- val[ft[,"tmp"]][1:length(nodeLbl)]
                names(val) <- nodeLbl
                as.list(val)
        })

setMethod("edgeData", signature(self="graphBAM", from="character", to="character",
                attr="character"),
        function(self, from, to, attr) {
            nodeNames <- self@nodes
            req_ft <- graph:::.align_from_to(from, to, nodeNames)
           .verifyEdges(self, req_ft[,"from"],req_ft[,"to"])
            numNodes <- length(nodeNames)
            bv <- self@edgeSet@bit_vector
            .verifyBAMAttrs(self, attr)
            val <- graph:::.retAttrVec(self, attr)
            ft <- .Call(graph_bitarray_rowColPos, self@edgeSet@bit_vector)
            if(!isDirected(self)){
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
            val <- structure(val[idx], names = paste(
                            nodeNames[req_i[,1]],nodeNames[req_i[,2]],
                            sep = "|"))
            as.list(val)
        })

.retAttrVec <- function(g, attr) {
    if(attr !="weight") {
        k1 <- g@edgeSet@bit_vector
        k2<- g@userAttrPos@edgePos[[attr]]
        tmp <- attributes(k1)
        res <- k1& (!k2)
        attributes(res) <- tmp
        ns <- .Call("graph_bitarray_sum", res)
        attr(res, "nbitset") <- ns
        ft <- data.frame(.Call("graph_bitarray_rowColPos",res))
        dflt <- g@edgeData@defaults[[attr]]
        attrBit <- g@userAttrPos@edgePos[[attr]]
        ft <- ft[with(ft, order(to, from)),]
        ## maybe ft needs reordering
        ord <- .Call("graph_bitarray_getEdgeAttrOrder",  attrBit, 
            as.integer(ft[,"from"]), as.integer(ft[,"to"]))
        attrBit <- graph:::setBitCell(attrBit, ft[,"from"], ft[,"to"], 
            rep(1L, nrow(ft)))
        nt <- attr(attrBit, "nbitset")
        newAttr <- vector(nt, mode = mode(dflt))
        if(!is.null(g@edgeSet@edge_attrs[[attr]])) {
            newAttr[ord$origLeftPos] <- g@edgeSet@edge_attrs[[attr]][ord$origRightPos]
            newAttr[ord$newLeftPos] <- if(mode(dflt)=="list") rep(list(dflt), length(ord$newLeftPos)) else  dflt
        }else{
            newAttr[1:nt] <- if(mode(dflt)=="list") rep(list(dflt), nt) else  dflt
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
        stop("unknown nodes: ", paste(req_nn[!okidx], collapse = ", "))
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
        stop(paste("attr", attr, "could not be found in \"self\"", sep =" "))
}

.set_attrs <- function(g, from, to, attr, value)
{  
    nodeNames <- g@nodes
    req_ft <- graph:::.align_from_to(from, to, nodeNames)
    ## remove dups
    indx <- duplicated(paste(req_ft[,"from"], req_ft[,"to"], sep ="_"))
    req_ft <- req_ft[!indx, ,drop = FALSE]
    if(nrow(req_ft) > 0 )
        .verifyEdges(g, req_ft[,1], req_ft[,2])
    else
        stop("Edges specified could not be found in \"self\"")
    if(is.vector(value)) {
        len  <- length(value)
    }else{
        len <- 1
        value <- list(value)
    }
    if(len == 1L)
        value <- rep(value, nrow(req_ft))

    if(length(value) != nrow(req_ft))
         stop("Number of edges and attribute values must be the same")
    
    ft <- .Call(graph_bitarray_rowColPos, g@edgeSet@bit_vector)

    if (!isDirected(g)) {
        ## normalize from/to
        valIndx <- seq_len(length(value))
        tmp <- .mg_undirectEdges(req_ft[ , 1], req_ft[, 2], valIndx)
        req_ft <- cbind("from"= tmp[["from"]],"to" = tmp[["to"]])
        value <- value[tmp[["weight"]]]
    }
    # ft <- data.frame(ft)
    # ft <- ft[with(ft, order(to,from)),]

     req_i <- structure(match(req_ft, nodeNames), dim = dim(req_ft))
     colnames(req_i) <- c("from", "to")
    # value <- value[order(req_i[,2], req_i[,1])]
    # tmp <- rbind(req_i, ft)
    # pst <- paste(tmp[,"from"],tmp[,"to"], sep = "_")
    # idx <- duplicated(pst)[seq(nrow(req_i) +1 , nrow(tmp))]
     # req_i <- structure(match(req_ft, nodeNames), dim = dim(req_ft))
     req_i <- data.frame(req_i)
     idx <- order(req_i[,2], req_i[,1])
     req_i <- req_i[idx, ]
     value <- value[idx]
     # tmpIndx <- with(req_i, order(to, from))
     #req_i <- req_i[tmpIndx,]
     #colnames(req_i) <- c("from", "to")
     #  value <- value[indx]
     #value <- value[order(req_i[,2], req_i[,1])]
    if(attr == "weight") {
        attrBit <- g@edgeSet@bit_vector
        ord <- .Call("graph_bitarray_getEdgeAttrOrder",  attrBit, 
                    as.integer(req_i[,"from"]), as.integer(req_i[,"to"]))
        g@edgeSet@bit_vector <- graph:::setBitCell(attrBit, req_i[,"from"], req_i[,"to"], 
                                rep(1L, nrow(req_i)))
        nt <- attr(g@edgeSet@bit_vector, "nbitset")
        newAttr <- vector(nt, mode = mode(value))
        newAttr[ord$origLeftPos] <- g@edgeSet@weights[ord$origRightPos]
        newAttr[ord$newLeftPos] <- value[ord$newRightPos]
        g@edgeSet@weights <- newAttr
    }else {
        attrBit <- g@userAttrPos@edgePos[[attr]]
        ord <- .Call("graph_bitarray_getEdgeAttrOrder", attrBit, 
                    as.integer(req_i[,"from"]), as.integer(req_i[,"to"]))
        g@userAttrPos@edgePos[[attr]] <- graph:::setBitCell(attrBit, req_i[,"from"], req_i[,"to"], 
                                rep(1L, nrow(req_i)))
        nt <- attr(g@userAttrPos@edgePos[[attr]], "nbitset")
        newAttr <- vector(nt, mode = mode(value))
        newAttr[ord$origLeftPos] <- 
                     g@edgeSet@edge_attrs[[attr]][ord$origRightPos]
        newAttr[ord$newLeftPos] <- value[ord$newRightPos]
        g@edgeSet@edge_attrs[[attr]] <- newAttr
    }
    g
}


setReplaceMethod("edgeData",
                 signature(self="graphBAM",
                           from="character", to="character",
                           attr="character", value="ANY"),
                 function(self, from, to, attr, value) {
                     lenFrom <- length(from)
                     lenTo <- length(to)
                     if(lenFrom != lenTo) {
                         if(lenFrom ==1)
                             from <- rep(from, lenTo)
                         else if (lenTo == 1)
                            to <- rep(to , lenFrom)
                         else
                            stop("Arguments from,to differ in length")
                        
                     }
                     .verifyEdges(self, from, to)
                     .set_attrs(self, from, to, attr, value)

                 })

setReplaceMethod("edgeData",
                 signature(self="graphBAM",
                           from="character", to="missing",
                           attr="character", value="ANY"),
                 function(self, from, to, attr, value) {
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
                     eg <- inEdges(to, self)
                     eg <- eg[order(names(eg))]
                     from  <- unlist(eg, use.names = FALSE) 
                     len <- as.numeric(sapply(eg, length))
                     to <- rep(names(eg), len)
                     .verifyEdges(self, from, to)
                     .set_attrs(self, from, to, attr, value)
                 })

setReplaceMethod("edgeDataDefaults", signature(self="graphBAM", attr="missing",
                     value="list"),
                 function(self, attr, value) {
                     attrDefaults(self@edgeData) <- value
                     wt <- self@edgeData@defaults[["weights"]]
                     if(!is.numeric(wt) && !is.null(wt))
                         stop("weights attribute has to be of type numeric")
                     ndsLen <- length(nodes(self))
                     nms <- names(value)
                     for(i in 1: length(value)){
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
                             stop("weights attribute has to be of type numeric")
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
    graph:::makebits(ndsLen * ndsLen, bitdim = c(ndsLen, ndsLen))
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
            .Call("graph_bitarray_getBitCell", object@edgeSet@bit_vector, from_i, to_i)
        })

setMethod("subGraph", signature(snodes="character", graph="graphBAM"),
        function(snodes, graph){ 
            origNodes <- nodes(graph)
            snodes <- sort(snodes)
            snodesIdx <- match(snodes, origNodes)
            if (any(is.na(snodesIdx))) {
                bad <- snodes[which(is.na(snodesIdx))]
                stop("invalid arg: snodes contains nodes not in the ",
                        "MultiGraph:\n", paste(bad, collapse=", "))
            }
            nms <- names(graph@nodeData@defaults)
            len <- length(snodes)
            tmp <- graph:::makebits(len)
            for(i in nms){
                indx <-  graph:::bitToLogical(graph@userAttrPos@nodePos[[i]])[snodesIdx]
                graph@nodeData@data[[i]] <-  .getNodeAttrVec(graph, i)[snodesIdx][indx]
                graph@userAttrPos@nodePos[[i]] <- graph:::setbitv(tmp, which(indx), rep(1L, length(which(indx))))
            }
            graph@nodes <- snodes
            
            res <- .Call("graph_bitarray_subGraph", graph@edgeSet@bit_vector, snodesIdx)
            graph@edgeSet@bit_vector <- res$bitVec
            graph@edgeSet@weights <- graph@edgeSet@weights[res$setPos]
            nms <- names(graph@edgeSet@edge_attrs)
            for(i in nms) {
                attrBit <- graph@userAttrPos@edgePos[[i]]
                res <- .Call("graph_bitarray_subGraph", attrBit, snodesIdx)
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
                stop("nodes specified do not exist in object")
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
                res <- .Call("graph_bitarray_subGraph", attrBit, snodesIdx)
                bam@edgeSet@edge_attrs[[i]] <-  object@edgeSet@edge_attrs[[i]][res$setPos]
                bam@userAttrPos@edgePos[[i]] <- res$bitVec
            }
            nms <- names(object@nodeData@defaults)
            len <- length(snodesIdx)
            tmp <- graph:::makebits(len)
            for(i in nms){
                leftPos <- which(graph:::bitToLogical(object@userAttrPos@nodePos[[i]])[snodesIdx])
                rightPos <- which(graph:::bitToLogical(object@userAttrPos@nodePos[[i]]))[snodesIdx]
                bt <- graph:::setbitv(tmp, leftPos, rep(1L, length(leftPos)))
                # k <- graph:::.getNodeAttrValues(bt, indx)
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
    am <- new("graphAM", adjMat = as(from, "matrix"),
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
              req_ft <- graph:::.align_from_to(from, to, nn)
              req_i <- structure(match(req_ft, nn), dim = dim(req_ft))
              ord <- .Call("graph_bitarray_getEdgeAttrOrder",
                  graph@edgeSet@bit_vector,
                as.integer(req_i[,1]), as.integer(req_i[,2]))
               graph@edgeSet@bit_vector <-
               graph:::setBitCell(graph@edgeSet@bit_vector, req_i[,1], req_i[,2], 
                                rep(1L, nrow(req_i)))
               nt <- attr(graph@edgeSet@bit_vector, "nbitset")
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


      #     
      #setMethod("addEdge",
      #          signature=c("character", "character", "graphBAM", "numeric"),
      #          function(from, to, graph, weights) {
      #              nn <- nodes(graph)
      #              req_ft <- .align_from_to(from, to, nn)
      #              df <- extractFromTo(graph)
      #              df2 <- data.frame(from=req_ft[ , 1], to=req_ft[ , 2],
      #                                weight=weights, stringsAsFactors = FALSE)
      #              g <- graphBAM(rbind(df, df2),nodes=nn, edgemode=edgemode(graph))
      #              g@edgeData@defaults <- graph@edgeData@defaults
      #              g@edgeSet@edge_attrs <- graph@edgeSet@edge_attrs
      #              g@userAttrPos@edgePos <- graph@userAttrPos@edgePos 
      #             
      #              g@nodeData@defaults <- graph@nodeData@defaults
      #              g@nodeData@data <- graph@nodeData@data
      #              g@userAttrPos@nodePos <- graph@userAttrPos@nodePos
      #              g@renderInfo <- graph@renderInfo
      #              g
      #          })
      #
setMethod("addEdge",
          signature=c("character", "character", "graphBAM", "missing"),
          function(from, to, graph, weights) {
              w <- rep(1L, max(length(from), length(to)))
              addEdge(from, to, graph, w)
          })

setMethod("addNode",
        signature(node="character", object="graphBAM", edges="missing"),
        function(node, object) {

            nds <- unique(c(nodes(object), node))
            ndsLen <- length(nds)
            df <- extractFromTo(object)
            g <- graphBAM(df, nodes = nds, edgemode = edgemode(object))
            nms <- names(object@nodeData@defaults)
            
            for(i in nms){
                origNds  <- nodes(object)[(graph:::bitToLogical(object@userAttrPos@nodePos[[i]]))]
                indx <- match(origNds, nodes(g))
                bt <- graph:::makebits(ndsLen)
                bt <- graph:::setbitv(bt, indx, rep(1L, length(indx)))
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
                g@userAttrPos@edgePos[[i]] <- setBitCell(posBit, ft[,"from"], ft[ ,"to"],
                            rep(1L, nrow(ft)))             
            } 
            g
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
   
    lenFrom <- length(from)
    lenTo <- length(to)
    if(lenFrom != lenTo) {
        if(lenFrom ==1)
            from <- rep(from, lenTo)
        else if (lenTo == 1)
            to <- rep(to , lenFrom)
        else
            stop("Arguments from,to differ in length")

    }
    graph:::.verifyEdges(graph, from, to)
    nn <- nodes(graph)
    req_ft <- graph:::.align_from_to(from, to, nn)
    req_from <-  match(req_ft[,"from"], nn)
    req_to <- match(req_ft[,"to"], nn)
    nms <- names(graph@edgeSet@edge_attrs)
    if(!is.null(nms)) {
        for(i in nms){
            graph@userAttrPos@edgePos[[i]] <-  
            graph:::setBitCell(graph@userAttrPos@edgePos[[i]], req_from, req_to,
                rep(0L, nrow(req_ft)))
            ord <- .Call("graph_bitarray_getEdgeAttrOrder",graph@userAttrPos@edgePos[[i]],
                as.integer(req_from), as.integer(req_to))
            graph@edgeSet@edge_attrs[[i]] <- graph@edgeSet@edge_attrs[[i]][ord$origLeftPos]
        }
    }
    graph@edgeSet@bit_vector <- graph:::setBitCell(graph@edgeSet@bit_vector,
                                           match(from, nn),
                                           match(to, nn),
                                           rep(0L, nrow(req_ft)))
    ord <- .Call("graph_bitarray_getEdgeAttrOrder",  graph@edgeSet@bit_vector , 
            as.integer(req_from), as.integer(req_to))
    graph@edgeSet@weights <- graph@edgeSet@weights[ord$origLeftPos]
    graph
}

setMethod("removeEdge", c("character", "character", "graphBAM"),
          function(from, to, graph) .remEdge(from, to, graph))


setMethod("removeEdgesByWeight", c("graphBAM"),
      function(graph, lessThan, greaterThan ){
          if(missing(lessThan) && missing(greaterThan))
              stop("For the edges to be removed, please specify a \"lessThan\" 
                  and \"greaterThan\" threshold for the weight attribute")
          if(!missing(lessThan) && !missing(greaterThan)){
              indx <- ( graph@edgeSet@weights >= lessThan & 
                      graph@edgeSet@weights <= greaterThan)
          } else if(missing(lessThan)){
              indx <- graph@edgeSet@weights <= greaterThan
          }else if(missing(greaterThan)){
              indx <- graph@edgeSet@weights >= lessThan
          }
          nn <- nodes(graph)
          bt <- .Call("graph_bitarray_removeEdges", graph@edgeSet@bit_vector, indx)
          tempBit <-  graph@edgeSet@bit_vector & (bt)
          attributes(tempBit) <- attributes(graph@edgeSet@bit_vector)
          attr(tempBit, "nbitset") <- .Call("graph_bitarray_sum", tempBit)
          ft <- .Call("graph_bitarray_rowColPos", tempBit)

          tp <- graph@edgeSet@bit_vector & (!bt)
          attributes(tp) <- attributes(graph@edgeSet@bit_vector)
          attr(tp, "nbitset") <- .Call("graph_bitarray_sum", tp)
          ft2 <-  .Call("graph_bitarray_rowColPos", tp)

          graph@edgeSet@weights <- graph@edgeSet@weights[indx]
          graph@edgeSet@bit_vector <- bt
          nms <- names(graph@userAttrPos@edgePos)
          if(!is.null(nms)){
              for(i in nms) {
                  graph@userAttrPos@edgePos[[i]] <- 
                  graph:::setBitCell(graph@userAttrPos@edgePos[[i]], 
                      ft2[,"from"], ft2[,"to"], rep(0L, nrow(ft2)))

                  ord <- .Call("graph_bitarray_getEdgeAttrOrder",
                      graph@userAttrPos@edgePos[[i]], as.integer(ft2[,"from"]),
                      as.integer(ft2[, "to"]))
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
             # 
             #.getIntersectWeights <- function(attrType, x, y, funList) {
             #    len <- length(attrType)
             #    indx <- as.numeric(attrType)
             #    from1 <- attr(attrType, "indx1")
             #    from2 <- attr(attrType, "indx2")
             #    attr1 <- rep(NA, len)
             #    k <- indx ==0
             #    val1 <- x@weights[from1[k]]
             #    val2 <- y@weights[from2[k]]
             #
             #    if(!is.null(funList) && ("weight" %in% names(funList))) {
             #        attr1[k] <-  sapply(seq_len(sum(k)), function(p) {
             #                    return(funList[["weight"]](val1[[p]], val2[[p]]))
             #                })
             #    } else if(is.atomic(val1) && is.atomic(val2)) {
             #         pt <-  which(val1 == val2)
             #         tmp <- rep(NA, length(which(k)))
             #         tmp[pt] <-  val1[pt]
             #         attr1[k]  <- tmp
             #    } 
             #    attr1
             #}
             #
.getIntersectAttrs <- function(att, attrType, x , y, funList  ) {
    len <- length(attrType)
    from1 <- attr(attrType, "indx1")
    from2 <- attr(attrType, "indx2")
    indx <- as.numeric(attrType)
    attr1 <- rep(NA, len)
    ## resolve union
    k <- indx ==0
    val1 <- x@edge_attrs[[att]][from1[k]]
    val2 <- y@edge_attrs[[att]][from2[k]]
  
    if(!is.null(funList) && (att %in% names(funList))) {
        attr1[k] <- sapply(seq_len(sum(k)), function(p) {
                    return(funList[[att]](val1[[p]], val2[[p]]))
                })
    } else if (is.vector(val1) && is.vector(val2)) {
        eqInd <- sapply(seq_len(length(val1)), function(x){
                     identical(val1[x], val2[x])
                 })

         pt <-  which(eqInd)
         tmp <- rep(NA, sum(k))
         tmp[pt] <-  val1[pt]
         attr1[k]  <- tmp
    } 
    attr1
}

#
#.getUnionWeights <- function(attrType, x, y, funList) {
#    len <- length(attrType)
#    indx <- as.numeric(attrType)
#    from1 <- attr(attrType, "indx1")
#    from2 <- attr(attrType, "indx2")
#    attr1 <- rep(NA, len)
#    ## from x
#    k <- indx ==1
#    attr1[k]  <- x@weights[from1[k]] 
#    ## from y 
#    k <- indx == 2
#    attr1[k]  <- y@weights[from2[k]]
#    ## resolve union
#    k <- indx ==0 
#    if(any(k)) {
#        val1 <- x@weights[from1[k]]
#        val2 <- y@weights[from2[k]]
#
#        if(!is.null(funList) && ("weight" %in% names(funList))) {
#            attr1[k] <-  sapply(seq_len(sum(k)), function(p) {
#                    return(funList[["weight"]](val1[[p]], val2[[p]]))
#                })
#        } else if(is.vector(val1) && is.vector(val2)) {
#            eqInd <- sapply(seq_len(length(val1)), function(x){
#                     identical(val1[x], val2[x])
#                 })
#            pt <-  which(eqInd)
#            tmp <- rep(NA, length(which(k)))
#            tmp[pt] <-  val1[pt]
#            attr1[k]  <- tmp
#        } 
#    } 
#    attr1
#}
#

.getAttrVec <- function(g, att){
    nms <- names(g@edgeData@defaults)
    bv <- g@edgeSet@bit_vector & (!g@userAttrPos@edgePos[[att]])
    attributes(bv) <- attributes(g@edgeSet@bit_vector)
    attr(bv, "nbitset") <- .Call("graph_bitarray_sum", bv)
    ft <- .Call("graph_bitarray_rowColPos", bv)
    attrBit <- g@userAttrPos@edgePos[[att]]
    ord <- .Call("graph_bitarray_getEdgeAttrOrder",  attrBit, 
                    as.integer(ft[,"from"]), as.integer(ft[,"to"]))
    nt <- attr(g@edgeSet@bit_vector, "nbitset")
    dflt <- g@edgeData@defaults[[att]]
    newAttr <- vector(nt, mode = mode(dflt))
    newAttr[ord$origLeftPos] <-  g@edgeSet@edge_attrs[[att]][ord$origRightPos]
    dflt <- g@edgeData@defaults[[att]]
    newAttr[ord$newLeftPos] <- if(mode(dflt)=="list") rep(list(dflt), nt) else  dflt
    newAttr
}

.getNodeAttrVec <- function(g, att) {
  unlist(nodeData(g, attr = att), use.names = FALSE)
}

.getUnionAttrs <- function(att, attrType, x , y, funList  ) {
    b <- x@edgeSet@bit_vector | x@edgeSet@bit_vector
     
    nms <- names(g@edgeData@defaults)
    bv <- v & (!g@userAttrPos@edgePos[[att]])
    attributes(bv) <- attributes(g@edgeSet@bit_vector)
    attr(bv, "nbitset") <- .Call(graph_bitarray_sum, bv)
    ft <- .Call("graph_bitarray_rowColPos", bv)

    attrBit <- g@userAttrPos@edgePos[[att]]
    ord <- .Call("graph_bitarray_getEdgeAttrOrder",  bv, 
                    as.integer(ft[,"from"]), as.integer(ft[,"to"]))

    nt <- attr(g@userAttrPos@edgePos[[attr]], "nbitset")
    newAttr <- vector(nt, mode = mode(g@edgeData@defaults))
    newAttr[attr(ord$newVal, "newPos")] <- value[ord$newVal]
    newAttr[attr(ord$origVal,"origPos")] <- g@edgeSet@edge_attrs[[attr]][ord$origVal]
    newAttr
    attrBit <- g@userAttrPos@edgePos[[att]]
        g@userAttrPos@edgePos[[attr]] <- setBitCell(attrBit, req_i[,"from"], req_i[,"to"], 
                                rep(1L, nrow(req_i)))
        ord <- .Call("graph_bitarray_getEdgeAttrOrder",  g@userAttrPos@edgePos[[attr]], 
                    as.integer(req_i[,"from"]), as.integer(req_i[,"to"]))
        nt <- attr(g@userAttrPos@edgePos[[attr]], "nbitset")
        newAttr <- vector(nt, mode = mode(value))
        newAttr[attr(ord$newVal, "newPos")] <- value[ord$newVal]
        newAttr[attr(ord$origVal,"origPos")] <- g@edgeSet@edge_attrs[[attr]][ord$origVal]
        g@edgeSet@edge_attrs[[attr]] <- newAttr

    #        bam@userAttrPos@edgePos[[i]] <- setBitCell(g@userAttrPos@edgePos[[i]], 
    #                                  ft[,"from"], ft[ ,"to"], rep(1L, nrow(ft))) 
    #    }
    #

    
    
    len <- length(attrType)
    from1 <- attr(attrType, "indx1")
    from2 <- attr(attrType, "indx2")

    indx <- as.numeric(attrType)
    attr1 <- rep(NA, len)
    if(att != "weight"){
        xattr <- x@edgeSet@edge_attrs[[att]]
        yattr <- y@edgeSet@edge_attrs[[att]]
    } else {
        xattr <- x@edgeSet@weights
        yattr <- y@edgeSet@weights
    
    }
    ## from the x
    k <- indx ==1
    if(att  %in% c("weight",names(x@edgeSet@edge_attrs)))
        attr1[k]  <- xattr[from1[k]] 
    ## from y 
    k <- indx == 2
    if(att  %in% c("weight", names(y@edgeSet@edge_attrs)))
        attr1[k]  <- yattr[from2[k]]
    ## resolve union
    k <- indx ==0
    if(any(k)) {
        if(att %in% c("weight", names(x@edgeSet@edge_attrs)))
            val1 <- xattr[from1[k]]
        else 
            val1  <- yattr[from2[k]]

        if(att %in% c("weight", names(y@edgeSet@edge_attrs)))
            val2 <- yattr[from2[k]]
        else 
            val2  <- xattr[from1[k]]
 
        if(!is.null(funList) && (att %in% c("weight", names(funList)))) {
            attr1[k] <- sapply(seq_len(sum(k)), function(p) {
                    return(funList[[att]](val1[[p]], val2[[p]]))
                })
        } else if (is.vector(val1) && is.vector(val2)) {
            eqInd <- sapply(seq_len(length(val1)), function(x){
                        identical(val1[x], val2[x])
                        })
            pt <-  which(eqInd)
            tmp <- rep(NA, sum(k))
            tmp[pt] <-  val1[pt]
            attr1[k]  <- tmp
        } 
    }
    attr1
}

.bamIntersect <- function(g1, g2, funList) {

    e1 <- g1@edgeSet
    e2 <- g2@edgeSet
    dr1 <- isDirected(e1)
    dr2 <- isDirected(e1)
    if(dr1 != dr2)
        stop("Edges should both be directed or undirected")
    theMode <- if (dr1 && dr2) "directed" else "undirected"
    e1Attr <- names(e1@edge_attrs)
    e2Attr <- names(e2@edge_attrs) 
    commonAttr <- intersect(e1Attr, e2Attr)
    if(!is.null(funList)) {
        fIndx <- names(funList) %in% c(commonAttr, "weight")
        if(!all(fIndx))
            stop( paste("Attributes", names(funList)[fIndx], "defined by 
                               \"funList\", were not found in the edge 
                                  attributes", sep = " "))
    }
    bv <- e1@bit_vector & e2@bit_vector
    attributes(bv) <- attributes(e1@bit_vector)
    attr(bv, "nbitset") <- ns <- .Call(graph_bitarray_sum, bv)
    c0 <- character(0)
    df <- data.frame(from = c0, to = c0, weight = numeric(0), stringsAsFactors = FALSE)
    edge_set <- .makeMDEdgeSet(es_name = 1, es = df,
                                is_directed = (theMode == "directed"), nodes = c0,
                                ignore_dup_edges = FALSE)
    nn <- intersect(nodes(g1), nodes(g2))
    ans <- new("graphBAM", edgeSet= edge_set, nodes =nn)
    edge_set@bit_vector <- bv
    attrType <- .Call("graph_bitarray_Intersect_Attrs", edge_set@bit_vector,
        e1@bit_vector, e2@bit_vector)
    
    if(length(attrType) >0) {
        edge_set@weights <- .getIntersectAttrs("weight", attrType, e1@weights,
            e2@weights, funList)
    }
    for(i in commonAttr) {
        bv <- g1@userAttrPos@edgePos[[i]] &  g2@userAttrPos@edgePos[[i]]
        attributes(bv) <- attributes(e1@bit_vector)
        attr(bv, "nbitset") <- ns <- .Call(graph_bitarray_sum, bv)
        ans@userAttrPos@edgePos[[i]] <- bv
        xAttr <- graph:::.getAttrVec(g1, i)
        yAttr <- graph:::.getAttrVec(g2, i)
        edge_set@edge_attrs[[i]] <- .getIntersectAttrs(i, attrType, xAttr, yAttr, funList)
    }
   ans@edgeSet <- edge_set
   ans
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
    len <- length(attrType)
    from1 <- attr(attrType, "indx1")
    from2 <- attr(attrType, "indx2")
    indx <- as.numeric(attrType)
    k <- indx ==0
    attr1 <- vector(sum(k), mode =mode(xAtt))
    attr1[1:sum(k)] <- NA
    val1 <- xAtt[from1[k]]
    val2 <- yAtt[from2[k]]

    #   if(att != "weight"){
    #        attr1 <- vector(sum(k), mode =mode(x@edge_attrs[[att]]))
    #        attr1[1:sum(k)] <- NA
    #        val1 <- x@edge_attrs[[att]][from1[k]]
    #        val2 <- y@edge_attrs[[att]][from2[k]]
    #    }else {
    #        attr1 <- vector(sum(k), mode = "numeric")
    #        attr1[1:sum(k)] <- NA
    #        val1 <- x@weights[from1[k]]
    #        val2 <- y@weights[from2[k]]
    #    }
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
            stop("x and y should both be directed or undirected")
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
    ans <- .bamIntersect(sg1, sg2, edgeFun)
    ans@nodeData@defaults <- .retNodeIntersectDefaults(sg1, sg2)
    ans@edgeData@defaults <- .retEdgeIntersectDefaults(sg1, sg2)
    ans@userAttrPos@nodePos <- .getIntNodeUserAttrPos(sg1, sg2)
    ans@nodeData@data <- .nodeIntersect(sg1, sg2, ans, nodeFun)
    ans
})

.retNodeIntersectDefaults <- function(sg1, sg2) {
    ndsAttrs <- intersect(names(sg1@nodeData@defaults), names(sg1@nodeData@defaults))
    structure(lapply(ndsAttrs, function(x) {
                if(identical(sg1@nodeData@defaults[[x]], sg2@nodeData@defaults[[x]])) sg1@nodeData@defaults[[x]] else NA
            }), names = ndsAttrs)
}

.retEdgeIntersectDefaults <- function(sg1, sg2) {
    edgeAttrs <- intersect(names(sg1@edgeData@defaults), names(sg2@edgeData@defaults))
    structure(lapply(edgeAttrs, function(x) {
                if(identical(sg1@edgeData@defaults[[x]], sg2@edgeData@defaults[[x]])) sg1@edgeData@defaults[[x]] else NA
            }), names = edgeAttrs)
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
                res[graph:::bitToLogical(ans@userAttrPos@nodePos[[x]])]
             }), names = cmn)
     nattr
}

.scaleGraphBAM <- function(g, theNodes, edgemode) {
    ndsLen <- length(theNodes)
    df <- extractFromTo(g)
    bam <- graphBAM(df, nodes = theNodes, edgemode= edgemode, 
                     ignore_dup_edges = FALSE)
    bam@edgeSet@weights <- g@edgeSet@weights
    
    #wbt <- bam@edgeSet@bit_vector
    #    nms <- names(g@edgeData@defaults)
    #    for(i in nms) {
    #        bv <- wbt & (!g@userAttrPos@edgePos[[i]])
    #        attributes(bv) <- attributes(bam1@edgeSet@bit_vector)
    #        attr(bv, "nbitset") <- .Call(graph_bitarray_sum, bv)
    #        bam@userAttrPos@edgePos[[i]] <- setBitCell(g@userAttrPos@edgePos[[i]], 
    #                                  ft[,"from"], ft[ ,"to"], rep(1L, nrow(ft))) 
    #    }
    #

    bam@edgeSet@edge_attrs <- g@edgeSet@edge_attrs
    bam@edgeData@defaults <- g@edgeData@defaults
    
    nms <- names(g@nodeData@defaults)
    for(i in nms){
        origNds  <- theNodes[(graph:::bitToLogical(g@userAttrPos@nodePos[[i]]))]
        indx <- match(theNodes, origNds)
        bt <- graph:::makebits(ndsLen)
        bt <- graph:::setbitv(bt, indx, rep(1L, length(indx)))
        bam@userAttrPos@nodePos[[i]] <- bt
        bam@nodeData@data[[i]] <- g@nodeData@data[[i]]
    }
    bam@nodeData@defaults <- g@nodeData@defaults
    nms <- names(g@userAttrPos@edgePos)
    for(i in nms) {
        ft <- .Call("graph_bitarray_rowColPos", g@userAttrPos@edgePos[[i]])
        posBit <- graph:::.createZeroBitPos(ndsLen)
        bam@userAttrPos@edgePos[[i]] <- setBitCell(posBit, ft[,"from"], ft[,"to"], rep(1L, nrow(ft)))
    }
    bam
}

.bamUnion <- function(g1, g2, theNodes, funList) { 
    dr1 <- isDirected(g1)
    dr2 <- isDirected(g2)
    if(dr1 != dr2)
        stop("Edges should both be directed or undirected")
    theMode <- if (dr1 && dr2) "directed" else "undirected"
    e1Attr <- names(g1@edgeSet@edge_attrs)
    e2Attr <- names(g2@edgeSet@edge_attrs) 
    unionAttr <- unique(union(e1Attr, e2Attr))
    if(!is.null(funList)) {
        fIndx <-  names(funList) %in% c(unionAttr, "weight")
        if(!all(fIndx))
            stop( paste("Attributes", names(funList)[fIndx], "defined by 
                        \"funList\", were not found in the edge 
                        attributes", sep = " "))
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
    attrType <- .Call("graph_bitarray_Union_Attrs", bv, cmnBit, fromOneBit,
                       fromTwoBit)
    nms <- c( "weight",unionAttr)


    
    for(i in nms) {
    
        if( i == "weight"){
            bt1 <- bam1@edgeSet@bit_vector
            bt2 <- bam2@edgeSet@bit_vector
        } else {
            bt1 <- bam1@userAttrPos@edgePos[[i]]
            bt2 <- bam2@userAttrPos@edgePos[[i]]
        }
        sumBit <- bt1 | bt2
        attributes(sumBit) <- attributes(bt1)
        attr(sumBit, "nbitset") <- .Call(graph_bitarray_sum, sumBit)
        
        cmnBit <- bt1 & bt2
        attributes(cmnBit) <- attributes(bt1)
        attr(cmnBit, "nbitset") <- .Call(graph_bitarray_sum, cmnBit)

        fromOneBit <- bt1 & (!cmnBit)
        attributes(fromOneBit) <- attributes(bt1)
        attr(fromOneBit, "nbitset") <- .Call(graph_bitarray_sum, fromOneBit)

        fromTwoBit <- bt2 & (!cmnBit)
        attributes(fromTwoBit) <- attributes(bt2)
        attr(fromTwoBit, "nbitset") <- .Call(graph_bitarray_sum, fromTwoBit)
        attrType <- .Call("graph_bitarray_Union_Attrs", sumBit, cmnBit, fromOneBit,
                           fromTwoBit)
        if(i == "weight")
            ans@edgeSet@weights <- .getUnionAttrs(i, attrType, bam1, bam2, funList)
        else {
            ans@edgeSet@edge_attrs[[i]] <- .getUnionAttrs(i, attrType, bam1, bam2, funList)
            ans@userAttrPos@edgePos[[i]] <- sumBit
        }
    }
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
           stop("x and y should both be directed or undirected")
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
   if( !identical(x@edgeData@defaults, y@edgeData@defaults))
        stop("Default attribute values must be the same for the union of graphs")
   ans <- .bamUnion(x, y, theNodes, edgeFun)
   ans@edgeData@defaults <- x@edgeData@defaults
   ans@nodeData@data <- .nodeUnion(x@nodeData@data, y@nodeData@data,
                           nodes(x), nodes(y), nodes(ans), nodeFun)  
   ans 
})
#
#.nodeUnion <- function(attr1, attr2, ndX, ndY, ndAns, funList) {
#    xAttr <-  names(attr1)
#    yAttr <-  names(attr2)
#    unionAttrs <- union(xAttr, yAttr)
#    commonAttrs <- intersect(xAttr,yAttr)
#    singleAttrs <- unionAttrs[!unionAttrs %in% commonAttrs]
#      
#    cmnNds <- intersect(ndX, ndY)
#    fxNds <-  ndX[!ndX %in% cmnNds]
#    fyNds <-  ndY[!ndY %in% cmnNds]
#
#    ### deal with single attrs
#    n1 <- sapply(singleAttrs, function(k){
#               if(k %in% xAttr){
#                    indx <- match(ndX, ndAns)
#                    att <- rep(NA, length(ndAns))
#                    att[indx] <- attr1[[k]]
#               } else if(k %in% yAttr){
#                    indx <- match(ndY, ndAns)
#                    att <- rep(NA, length(ndAns))
#                    att[indx] <- attr2[[k]]
#               }
#               list(att)
#           })
#    
#    n2 <- sapply(commonAttrs, function(k) {
#               att <- rep(NA, length(ndAns))
#               ##from X
#               indx <- match(fxNds, ndAns)
#               att[indx] <- attr1[[k]][match(fxNds,ndX)]
#               ##from Y
#               indx <- match(fyNds, ndAns)
#               att[indx] <- attr2[[k]][match(fyNds,ndY)]
#               
#               if(!is.null(funList) && (k %in% names(funList))) {
#                    tmp <- sapply(cmnNds, function(p){
#                            dX <- match(p, ndX)
#                            dY <- match(p, ndY)
#                            funList[[k]](attr1[[k]][[dX]], attr2[[k]][[dY]])
#                        }, USE.NAMES = FALSE)
#                    indx <- match(cmnNds, ndAns)
#                    att[indx] <- tmp
#                } else if (is.vector(attr1[[k]]) && is.vector(attr2[[k]])){
#                    tmp <- sapply(cmnNds, function(p){
#                            dX <- match(p, ndX)
#                            dY <- match(p, ndY)
#                            identical(attr1[[k]][[dX]], attr2[[k]][[dY]])
#                        }, USE.NAMES = FALSE)
#                    indx <- match(cmnNds[tmp], ndAns)
#                    att[indx] <- attr1[[k]][match(cmnNds[tmp], ndX)]
#               }
#               list(att)
#            })
#     c(n1,n2)
#}

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

setReplaceMethod("nodeData",
        signature(self = "graphBAM", n="character", attr="character", value="ANY"),
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
            bt <- .Call("graph_bitarray_set", bt, as.integer(idx), 
                 as.integer(rep(1L, length(idx))))
            ns <- attr(bt, "nbitset")
            self@userAttrPos@nodePos[[attr]] <- bt
            ord <- .getNodeAttrOrder(bt, idx)
            newAttr <- vector(ns, mode = mode(value))
            newAttr[attr(ord$newVal, "newPos")] <- value[ord$newVal]
            newAttr[attr(ord$origVal, "origPos")] <- self@nodeData@data[[attr]][ord$origVal]
            self@nodeData@data[[attr]] <- newAttr
            self 
        })

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
        signature(self = "graphBAM", n="missing", attr="character", value="ANY"),
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
            nodeData(self, n = nodes(self), attr) <- value
            self
        })
    #
    #.getNodeAttrValues <- function(x, nds) {
    #    bt <- graph:::bitToLogical(x)
    #    cs <- cumsum(bt)
    #    idx <- nds[bt[nds]]
    #    list(nodePos= which(nds %in% idx), attrPos = cs[idx]) 
    #}
    #

.getNodeAttrPos <- function(x, indx) {
    bt <- graph:::bitToLogical(x)
    leftPos <- which(bt[indx])
    cs <- cumsum(bt) 
    rightPos <- cs[which(bt)]
    list(leftPos= leftPos, rightPos = rightPos)
}

## Node data accces methods 
setMethod("nodeData",
        signature(self = "graphBAM", n = "character", attr = "character"),
        function(self, n, attr) {
            if(length(attr) != 1L)
                stop("attr argument must specify a single attribute name")
            if( ! (attr %in% names(self@nodeData@data)))
                stop(paste("attribute", attr," is not present in self"))
            nds <- nodes(self)
            .verifyNodes(n, nds)
            idx <- match(n, nds)
            names(idx) <- 1:length(idx)
            idx <- sort(idx) 
            n <- n[as.numeric(names(idx))]
            names(idx) <- NULL
            bt <- self@userAttrPos@nodePos[[attr]]
            ord <- .getNodeAttrPos(bt, idx)
            #   leftPos <- which(graph:::bitToLogical(bt)[idx])
            #  rightPos <- which(graph:::bitToLogical(bt))[idx]
            res <- vector(length(idx), mode = mode(self@nodeData@defaults[[attr]]))
            res[1:length(idx)] <- self@nodeData@defaults[[attr]]
            if(length(ord$leftPos))
                res[ord$leftPos] <- self@nodeData@data[[attr]][ord$rightPos]
            as.list(structure(res, names = n))
        })

setMethod("nodeData",
        signature(self = "graphBAM", n = "missing", attr = "character"),
        function(self, n, attr) {
            if(length(attr) != 1L)
                stop("attr argument must specify a single attribute name")
            if( ! (attr %in% names(self@nodeData@data)))
                stop(paste("attribute", attr," is not present in self"))
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

