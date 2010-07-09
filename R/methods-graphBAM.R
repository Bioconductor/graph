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
    if (length(value) == 1L) {
        value <- if(is.atomic(value)) {
                     rep(value, nrow(req_ft))
                 } else{ 
                    rep(list(value), nrow(req_ft))
                }
    } else if(length(value) != nrow(req_ft))
        stop("number of edges and attribute values must align")
    ft <- .Call(graph_bitarray_rowColPos, g@edgeSet@bit_vector)
    if (!isDirected(g)) {
        ## normalize from/to
        tmp <- .mg_undirectEdges(req_ft[ , 1], req_ft[, 2], value)
        req_ft <- cbind("from"= tmp[["from"]],"to" = tmp[["to"]])
        value <- tmp[["weight"]]
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
                    len = length(value)
                    if (len == 1L) {
                        value <- if(is.atomic(value)) {
                                    rep(value, nset)
                                 }else{ 
                                    rep(list(value), nset)
                                 }
                    } else {
                        stop("value should be of length 1")
                    }
                    
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
    val1 <- x@edgeSet@weights[from1[k]]
    val2 <- y@edgeSet@weights[from2[k]]

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
    val1 <- x@edgeSet@edge_attrs[[att]][from1[k]]
    val2 <- y@edgeSet@edge_attrs[[att]][from2[k]]
 
    if(!is.null(funList) && (att %in% names(funList))) {
        attr1[k] <- sapply(seq_len(sum(k)), function(p) {
                    return(funList[[att]](val1[[p]], val2[[p]]))
                })
    } else if (is.atomic(val1) && is.atomic(val2)) {
         pt <-  which(val1 == val2)
         tmp <- rep(NA, sum(k))
         tmp[pt] <-  val1[pt]
         attr1[k]  <- tmp
    } else {
         stop( paste("Please specify a function for handling the union of two
                         objects of attribute", att, sep =" "))
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
    attr1[k]  <- x@edgeSet@weights[from1[k]] 
    ## from y 
    k <- indx == 2
    attr1[k]  <- y@edgeSet@weights[from2[k]]
    ## resolve union
    k <- indx ==0
    val1 <- x@edgeSet@weights[from1[k]]
    val2 <- y@edgeSet@weights[from2[k]]

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

.getUnionAttrs <- function(att, attrType, x , y, funList  ) {
    len <- length(attrType)
    from1 <- attr(attrType, "indx1")
    from2 <- attr(attrType, "indx2")

    indx <- as.numeric(attrType)
       attr1 <- rep(NA, len)
    ## from the x
    k <- indx ==1
    if(att  %in% names(x@edgeSet@edge_attrs))
        attr1[k]  <- x@edgeSet@edge_attrs[[att]][from1[k]] 
    ## from y 
    k <- indx == 2
    if(att  %in% names(y@edgeSet@edge_attrs))
        attr1[k]  <- y@edgeSet@edge_attrs[[att]][from2[k]]
    ## resolve union
    k <- indx ==0
    if(att %in% names(x@edgeSet@edge_attrs))
       val1 <- x@edgeSet@edge_attrs[[att]][from1[k]]
    else val1  <- y@edgeSet@edge_attrs[[att]][from2[k]]

    if(att %in% names(y@edgeSet@edge_attrs))
       val2 <- y@edgeSet@edge_attrs[[att]][from2[k]]
    else val2  <- x@edgeSet@edge_attrs[[att]][from1[k]]
 
    if(!is.null(funList) && (att %in% names(funList))) {
        attr1[k] <- sapply(seq_len(sum(k)), function(p) {
                    return(funList[[att]](val1[[p]], val2[[p]]))
                })
    } else if (is.atomic(val1) && is.atomic(val2)) {
         pt <-  which(val1 == val2)
         tmp <- rep(NA, sum(k))
         tmp[pt] <-  val1[pt]
         attr1[k]  <- tmp
    } else {
         stop( paste("Please specify a function for handling the union of two
                         objects of attribute", att, sep =" "))
    }
    attr1
}

setMethod("graphIntersect", c("graphBAM", "graphBAM"),
           function(x, y, ..., funList){
           dr1 <- isDirected(x)
           dr2 <- isDirected(y)
           if(dr1 != dr2)
               stop("x and y should both be directed or undirected")
           theMode <- if (dr1 && dr2) "directed" else "undirected"

           xAttr <- names(x@edgeSet@edge_attrs)
           yAttr <- names(y@edgeSet@edge_attrs) 
           commonAttr <- intersect(xAttr, yAttr)
           if(!missing(funList)) {
               fIndx <- names(funList) %in% c(commonAttr, "weight")
               if(!all(fIndx))
                   stop( paste("Attributes", names(funList)[fIndx], "defined by 
                               \"funList\", were not found in the edge 
                                  attributes", sep = " "))
                          
            } else {
               funList <- NULL
            }
            nn <- intersect(nodes(x), nodes(y))
            nnLen <- length(nn)
                        c0 <- character(0)
            df <- data.frame(from = c0, to = c0, weight = numeric(0), 
                    stringsAsFactors = FALSE)
            ans <- graphBAM(df, edgemode = theMode)
            if (nnLen == 0) return(ans)
            sg1 <- if (nnLen == numNodes(x)) x else subGraph(nn, x)
            sg2 <- if (nnLen == numNodes(y)) y else subGraph(nn, y)
            bv <- sg1@edgeSet@bit_vector & sg2@edgeSet@bit_vector
            attributes(bv) <- attributes(sg1@edgeSet@bit_vector)
            attr(bv, "nbitset") <- ns <- .Call(graph_bitarray_sum, bv)
            ans@edgeSet@edge_attrs <- list()
            ans@edgeSet@bit_vector <- bv
            ans@edgeSet@weights <- rep(1L, ns)
            ans@nodes <- nn
            
            fromOneBit <- sg1@edgeSet@bit_vector 
            attributes(fromOneBit) <- attributes(sg1@edgeSet@bit_vector)
            attr(fromOneBit, "nbitset") <- .Call(graph_bitarray_sum, fromOneBit)

            fromTwoBit <- sg2@edgeSet@bit_vector 
            attributes(fromTwoBit) <- attributes(sg2@edgeSet@bit_vector)
            attr(fromTwoBit, "nbitset") <- .Call(graph_bitarray_sum, fromTwoBit)

            attrType <- .Call("graph_bitarray_Intersect_Attrs", bv,
                    fromOneBit, fromTwoBit)
     
            ans@edgeSet@edge_attrs <- structure( lapply(commonAttr, function(k) {
                                     .getIntersectAttrs(k, attrType, sg1, sg2, funList)
                                       }), names = commonAttr)
    
            ans@edgeSet@weights <- as.numeric(.getIntersectWeights(attrType, sg1, 
                                              sg2, funList))
            ans
})
 
setMethod("graphUnion", c("graphBAM", "graphBAM"), 
        function(x, y, ..., funList) {
    dr1 <- isDirected(x)
    dr2 <- isDirected(y)
    if(dr1 != dr2)
        stop("x and y should both be directed or undirected")

    xAttr <- names(x@edgeSet@edge_attrs)
    yAttr <- names(y@edgeSet@edge_attrs) 
    unionAttr <- unique(union(xAttr, yAttr))
    if(!missing(funList)) {
        fIndx <-  names(funList) %in% c(unionAttr, "weight")
        if(!all(fIndx))
            stop( paste("Attributes", names(funList)[fIndx], "defined by 
                        \"funList\", were not found in the edge 
                        attributes", sep = " "))
    }else{
        funList <- NULL
    }
    
    theMode <- if (dr1 && dr2) "directed" else "undirected"
    theNodes <- unique(c(nodes(x), nodes(y)))
    df1 <- extractFromTo(x)
    bam1 <- graphBAM(df1, nodes = theNodes, edgemode = theMode)
    df2 <- extractFromTo(y)
    bam2 <- graphBAM(df2, nodes = theNodes, edgemode = theMode)
    c0 <- character(0)
    df <- data.frame(from = c0, to = c0, weight = numeric(0))
    ans <- graphBAM(df, nodes = theNodes, edgemode = theMode)
    bv <- bam1@edgeSet@bit_vector | bam2@edgeSet@bit_vector
    attributes(bv) <- attributes(bam1@edgeSet@bit_vector)
    attr(bv, "nbitset") <- ns <- .Call(graph_bitarray_sum, bv)
    ans@edgeSet@bit_vector <- bv
    ans@edgeSet@weights <- rep(1L, ns)
    
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
    ans@edgeSet@edge_attrs <- structure( lapply(unionAttr, function(k) {
                                     .getUnionAttrs(k, attrType, x, y, funList)
                                       }), names = unionAttr)
    ans@edgeSet@weights <- as.numeric(.getUnionWeights(attrType, x, y, funList))
    ans
})
