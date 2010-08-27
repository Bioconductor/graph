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

.eAttrsFun <- function(self, from, attr) {
    
    nodeNames <- self@nodes
    indx <- which(nodeNames %in% from)
    numNodes <- length(nodeNames)
    bv <- self@edgeSet@bit_vector
    if(attr == "weight")
        val <- self@edgeSet@weights
    else{ 
        if(attr %in% names(self@edgeSet@edge_attrs))
            val <- self@edgeSet@edge_attrs[[attr]]
        else
            stop(paste("Unknown attribute name: ", attr, sep="")) 
    }
    ft <- .Call(graph_bitarray_rowColPos, bv)
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
                if(attr == "weight") {
                    val <- self@edgeSet@weights
                } else { 
                    if(attr %in% names(self@edgeSet@edge_attrs))
                        val <- self@edgeSet@edge_attrs[[attr]]
                    else
                        stop(paste("Unknown attribute name: ", attr, sep="")) 
                }
                ft <- .Call(graph_bitarray_rowColPos, self@edgeSet@bit_vector)
                if(!isDirected(self)){
                    df <- cbind(from=ft[,"to"], to = ft[,"from"])
                    ft <- rbind(ft,df)
                    val <- c(val,val)
                }
                tmp <- seq_len(length(val))
                ft <- data.frame(ft, tmp, stringsAsFactors = FALSE)
                ft <- ft[ft[,"to"] %in% which(nodeNames %in% to),]  ## was ==
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
            req_ft <- .align_from_to(from, to, nodeNames)
           .verifyEdges(self, req_ft[,"from"],req_ft[,"to"])
            numNodes <- length(nodeNames)
            bv <- self@edgeSet@bit_vector
            if(attr == "weight") {
               val <- self@edgeSet@weights
            } else{
               if(attr %in% names(self@edgeSet@edge_attrs))
                     val <- self@edgeSet@edge_attrs[[attr]]
               else
                    stop(paste("Unknown attribute name: ", attr, sep="")) 
            }
 
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
    if( !(attr %in% c(names(bam@edgeSet@edge_attrs), "weight")))
        stop(paste("attr", attr, "could not be found in \"self\"", sep =" "))
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
    ft <- data.frame(ft)
    ft <- ft[with(ft, order(to,from)),]
    req_i <- structure(match(req_ft, nodeNames), dim = dim(req_ft))
    colnames(req_i) <- c("from", "to")
    value <- value[order(req_i[,2], req_i[,1])]
    tmp <- rbind(req_i, ft)
    pst <- paste(tmp[,"from"],tmp[,"to"], sep = "_")
    idx <- duplicated(pst)[seq(nrow(req_i) +1 , nrow(tmp))]
    
    if(attr == "weight") {
         g@edgeSet@weights[idx] <- value
    } else {
         if(!(attr %in% names(g@edgeSet@edge_attrs))){
             nn <- attr(g@edgeSet@bit_vector,"nbitset")
             g@edgeSet@edge_attrs[[attr]][1:nn] <- NA
         }
         g@edgeSet@edge_attrs[[attr]][idx] <- value
    }
    g
}

setReplaceMethod("edgeData",
                 signature(self="graphBAM",
                           from="character", to="character",
                           attr="character", value="ANY"),
                 function(self, from, to, attr, value) {
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
                     .set_attrs(self, from, to, attr, value)
                 })


setReplaceMethod("edgeData",
                 signature(self="graphBAM", from="missing", to="missing",
                           attr="character", value="ANY"),
                 function(self, from, to, attr, value) {
                    nset <- attr(self@edgeSet@bit_vector,"nbitset")
                    
                    if(is.vector(value)){
                       len <- length(value)
                    
                    } else {
                        len <- 1
                        value <- list(value)
                    }
                    if(len != 1L)
                        stop("value should be of length 1")
                    value <- rep(value, nset)
                    
                    if(attr == "weight"){
                        self@edgeSet@weights <- value
                    } else{ 
                        self@edgeSet@edge_attrs[[attr]] <- value
                    }
                    self
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
            .Call("graph_bitarray_getBitCell", object@edgeSet@bit_vector, from_i, to_i)
            ##getBitCell(object@edgeSet@bit_vector, from_i, to_i)
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
            if(length(graph@edgeSet@edge_attrs)) {
                graph@edgeSet@edge_attrs <- lapply(graph@edgeSet@edge_attrs, 
                        function(x) {
                            x[res$setPos]
                        })
            }
            if(length(graph@nodeData@data)){
                graph@nodeData@data <- lapply(graph@nodeData@data, function(x) {
                        x[snodesIdx]
                        })
            }
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
              g <- graphBAM(rbind(df, df2), edgemode=edgemode(graph))
              indx <- .Call("graph_bitarray_getEdgeAttrPos", graph@edgeSet@bit_vector,
                             g@edgeSet@bit_vector)
              len <- attr(g@edgeSet@bit_vector, "nbitset")
              g@edgeSet@edge_attrs <- lapply(graph@edgeSet@edge_attrs, function(x){
                       val <- rep(NA, len)
                       val[indx] <- x
                       val
                      })
              g@nodeData@data <- graph@nodeData@data
              g@renderInfo <- graph@renderInfo
              g
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
            nds <- unique(c(nodes(object), node))
            df <- extractFromTo(object)
            g <- graphBAM(df, nodes = nds, edgemode = edgemode(object))
            indx <- match(nodes(object), nodes(g))
            g@nodeData@data <- lapply(object@nodeData@data, function(x) {
                      val <- rep(NA, length(nodes(g)))
                      val[indx] <- x
                      val
                    })
            g@edgeSet@edge_attrs <- object@edgeSet@edge_attrs
            g@renderInfo <- object@renderInfo
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
    nn <- nodes(graph)
    req_ft <- .align_from_to(from, to, nn)
    ft <- .Call(graph_bitarray_rowColPos, graph@edgeSet@bit_vector)
    all_f <- nn[ft[ , 1]]
    all_t <- nn[ft[ , 2]]
    wh <- !((all_f %in% req_ft[ , 1]) & (all_t %in% req_ft[ , 2]))
    new_weights <- graph@edgeSet@weights[wh]
    graph@edgeSet@edge_attrs <- lapply( graph@edgeSet@edge_attrs, function(x) {
                x[wh]   
            })
    graph@edgeSet@bit_vector <- setBitCell(graph@edgeSet@bit_vector,
                                           match(from, nn),
                                           match(to, nn),
                                           rep(0L, nrow(req_ft)))
    graph@edgeSet@weights <- new_weights
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
          graph@edgeSet@weights <- graph@edgeSet@weights[indx]
          graph@edgeSet@edge_attrs <- lapply( graph@edgeSet@edge_attrs, 
                  function(x) {
                      x[indx]   
                  })
          graph@edgeSet@bit_vector <- .Call(graph_bitarray_removeEdges, 
                                            graph@edgeSet@bit_vector, indx)
          graph
      })

setReplaceMethod("nodes", c("graphBAM", "character"),
                 function(object, value) {
                     stop("operation not supported")
                 })
 
.getIntersectWeights <- function(attrType, x, y, funList) {
    len <- length(attrType)
    indx <- as.numeric(attrType)
    from1 <- attr(attrType, "indx1")
    from2 <- attr(attrType, "indx2")
    attr1 <- rep(NA, len)
    k <- indx ==0
    val1 <- x@weights[from1[k]]
    val2 <- y@weights[from2[k]]

    if(!is.null(funList) && ("weight" %in% names(funList))) {
        attr1[k] <-  sapply(seq_len(sum(k)), function(p) {
                    return(funList[["weight"]](val1[[p]], val2[[p]]))
                })
    } else if(is.atomic(val1) && is.atomic(val2)) {
         pt <-  which(val1 == val2)
         tmp <- rep(NA, length(which(k)))
         tmp[pt] <-  val1[pt]
         attr1[k]  <- tmp
    } 
    attr1
}

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


.getUnionWeights <- function(attrType, x, y, funList) {
    len <- length(attrType)
    indx <- as.numeric(attrType)
    from1 <- attr(attrType, "indx1")
    from2 <- attr(attrType, "indx2")
    attr1 <- rep(NA, len)
    ## from x
    k <- indx ==1
    attr1[k]  <- x@weights[from1[k]] 
    ## from y 
    k <- indx == 2
    attr1[k]  <- y@weights[from2[k]]
    ## resolve union
    k <- indx ==0 
    if(any(k)) {
        val1 <- x@weights[from1[k]]
        val2 <- y@weights[from2[k]]

        if(!is.null(funList) && ("weight" %in% names(funList))) {
            attr1[k] <-  sapply(seq_len(sum(k)), function(p) {
                    return(funList[["weight"]](val1[[p]], val2[[p]]))
                })
        } else if(is.vector(val1) && is.vector(val2)) {
            eqInd <- sapply(seq_len(length(val1)), function(x){
                     identical(val1[x], val2[x])
                 })
            pt <-  which(eqInd)
            tmp <- rep(NA, length(which(k)))
            tmp[pt] <-  val1[pt]
            attr1[k]  <- tmp
        } 
    } 
    attr1
}

.getUnionAttrs <- function(att, attrType, x , y, funList  ) {
    len <- length(attrType)
    from1 <- attr(attrType, "indx1")
    from2 <- attr(attrType, "indx2")

    indx <- as.numeric(attrType)
       attr1 <- rep(NA, len)
    ## from the x
    k <- indx ==1
    if(att  %in% names(x@edge_attrs))
        attr1[k]  <- x@edge_attrs[[att]][from1[k]] 
    ## from y 
    k <- indx == 2
    if(att  %in% names(y@edge_attrs))
        attr1[k]  <- y@edge_attrs[[att]][from2[k]]
    ## resolve union
    k <- indx ==0
    if(any(k)) {
        if(att %in% names(x@edge_attrs))
            val1 <- x@edge_attrs[[att]][from1[k]]
        else 
            val1  <- y@edge_attrs[[att]][from2[k]]

        if(att %in% names(y@edge_attrs))
            val2 <- y@edge_attrs[[att]][from2[k]]
        else 
            val2  <- x@edge_attrs[[att]][from1[k]]
 
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
    edge_set <- .edgeIntersect(sg1@edgeSet, sg2@edgeSet, edgeFun)
    ans <- new("graphBAM", edgeSet= edge_set, nodes =nn)
    ans@nodeData@data <- .nodeIntersect(sg1@nodeData@data, sg2@nodeData@data, nodeFun)
    ans
})

.nodeIntersect <- function(attr1, attr2, funList){
    cmn <- intersect(names(attr1), names(attr2))
    nattr <- structure(lapply(cmn, function(x) {
                 len <- length(attr1[[x]])
                 if(!is.null(funList) && (x %in% names(funList))) {
                    res <- sapply(seq_len(len), function(p) {
                             return(funList[[x]](attr1[[x]][[p]], attr2[[x]][[p]]))
                           })
                 } else if (is.vector(attr1[[x]]) && is.vector(attr2[[x]])) {
                    indx <- which(sapply(seq_len(len), function(p){
                                identical(attr1[[x]][p], attr2[[x]][p])
                           }))
                    res <- rep(NA, len)
                    res[indx] <- attr1[[x]][indx]
                }
                res
             }), names = cmn)
     nattr
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
   # df1 <- extractFromTo(x)
   # df2 <- extractFromTo(y)
   df1 <- diEdgeSetToDataFrame(x@edgeSet,nodes(x))
   df2 <- diEdgeSetToDataFrame(y@edgeSet,nodes(y))
   edge_set <- .edgeUnion(x@edgeSet, y@edgeSet, df1, df2, theNodes, edgeFun)
   ans <- new("graphBAM", edgeSet= edge_set, nodes =theNodes)
   ans@nodeData@data <- .nodeUnion(x@nodeData@data, y@nodeData@data,
                           nodes(x), nodes(y), nodes(ans), nodeFun)  
   ans 
})

.nodeUnion <- function(attr1, attr2, ndX, ndY, ndAns, funList) {
    xAttr <-  names(attr1)
    yAttr <-  names(attr2)
    unionAttrs <- union(xAttr, yAttr)
    commonAttrs <- intersect(xAttr,yAttr)
    singleAttrs <- unionAttrs[!unionAttrs %in% commonAttrs]
      
    cmnNds <- intersect(ndX, ndY)
    fxNds <-  ndX[!ndX %in% cmnNds]
    fyNds <-  ndY[!ndY %in% cmnNds]

    ### deal with single attrs
    n1 <- sapply(singleAttrs, function(k){
               if(k %in% xAttr){
                    indx <- match(ndX, ndAns)
                    att <- rep(NA, length(ndAns))
                    att[indx] <- attr1[[k]]
               } else if(k %in% yAttr){
                    indx <- match(ndY, ndAns)
                    att <- rep(NA, length(ndAns))
                    att[indx] <- attr2[[k]]
               }
               list(att)
           })
    
    n2 <- sapply(commonAttrs, function(k) {
               att <- rep(NA, length(ndAns))
               ##from X
               indx <- match(fxNds, ndAns)
               att[indx] <- attr1[[k]][match(fxNds,ndX)]
               ##from Y
               indx <- match(fyNds, ndAns)
               att[indx] <- attr2[[k]][match(fyNds,ndY)]
               
               if(!is.null(funList) && (k %in% names(funList))) {
                    tmp <- sapply(cmnNds, function(p){
                            dX <- match(p, ndX)
                            dY <- match(p, ndY)
                            funList[[k]](attr1[[k]][[dX]], attr2[[k]][[dY]])
                        }, USE.NAMES = FALSE)
                    indx <- match(cmnNds, ndAns)
                    att[indx] <- tmp
                } else if (is.vector(attr1[[k]]) && is.vector(attr2[[k]])){
                    tmp <- sapply(cmnNds, function(p){
                            dX <- match(p, ndX)
                            dY <- match(p, ndY)
                            identical(attr1[[k]][[dX]], attr2[[k]][[dY]])
                        }, USE.NAMES = FALSE)
                    indx <- match(cmnNds[tmp], ndAns)
                    att[indx] <- attr1[[k]][match(cmnNds[tmp], ndX)]
               }
               list(att)
            })
     c(n1,n2)
 }

setMethod("nodeDataDefaults", 
        signature(self="graphBAM", attr="missing"),
        function(self, attr){
            stop("Method not supported for graphBAM objects")

        })

setMethod("nodeDataDefaults", 
        signature(self="graphBAM", attr="character"),
        function(self, attr){
            stop("Method not supported for graphBAM objects")
        })

setReplaceMethod("nodeDataDefaults", 
        signature(self="graphBAM", attr="missing", value="ANY"),
        function(self, attr, value) {
            stop("Method not supported for graphBAM objects")
        })

setReplaceMethod("nodeDataDefaults", 
        signature(self="graphBAM", attr="character", value="ANY"),
        function(self, attr, value) {
            stop("Method not supported for graphBAM objects")
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
            idx <- match(n, nds)
            if(!( attr %in% nms))
                self@nodeData@data[[attr]] <- rep( NA, length(nds))
            self@nodeData@data[[attr]][idx] <- value
            self 
        })

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
            self@nodeData@data[[attr]][idx] <-  value
            self
        })


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
            idx <- nds %in% n
            as.list(structure(self@nodeData@data[[attr]][idx], names = n))
        })

setMethod("nodeData",
        signature(self = "graphBAM", n = "missing", attr = "character"),
        function(self, n, attr) {
            if(length(attr) != 1L)
                stop("attr argument must specify a single attribute name")
            if( ! (attr %in% names(self@nodeData@data)))
                stop(paste("attribute", attr," is not present in self"))


            nds <- nodes(self)
            as.list(structure(self@nodeData@data[[attr]], names = nds))
        })

setMethod("nodeData",
        signature(self = "graphBAM", n = "character", attr = "missing"),
        function(self, n, attr) {

            nds <- nodes(self)
            .verifyNodes(n ,nds)
            idx <- nds %in% n
            if(!any(idx))
                stop("Specified node is not in self")
            lapply(self@nodeData@data, function(x) {
                        structure(x[idx], names = n)
                    })
        })

setMethod("nodeData",
        signature(self = "graphBAM", n = "missing", attr = "missing"),
        function(self, n, attr) {
           nds <- nodes(self)
           lapply(self@nodeData@data, function(x) {
                    structure(x, names = nds)
                   })
        })




