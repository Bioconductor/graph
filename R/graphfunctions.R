################################################################
# function:
# boundary takes two parameters:
#   graph is the original graph from which the subgraph will be created
#   subgraph either the subgraph or the nodes of the subgraph
# boundary returns a list of length equal to the number of nodes in the
#   subgraph. Each element is a list of the nodes in graph
#
# created by: Elizabeth Whalen
# last updated: Feb 15, 2003, RG
################################################################

boundary<-function(subgraph, graph)
{
  if ( !is(graph, "graph") )
    stop("The second parameter must be an object of type graph.")

  if( is(subgraph, "graph") )
      snodes <- nodes(subgraph)
  else if( is.character(subgraph) )
      snodes <- subgraph
  else
      stop("wrong type of first argument")

  if( any( !(snodes %in% nodes(graph)) ) )
      stop("the supplied subgraph is not a subgraph of the supplied graph")

  subE <- edges(graph)[snodes]

  lapply(subE, function(x) x[!(x %in% snodes)] )
}


##check to see if any edges are duplicated, as we often don't have
##good ways to deal with that
duplicatedEdges <- function(graph) {
    if( !is(graph, "graphNEL") )
        stop("only graphNEL supported for now")

    for(e in graph@edgeL)
        if( any(duplicated(e$edges)) )
            return(TRUE)
    return(FALSE)
}

##ugraph: take a directed graph and return the underlying undirected graph
ugraph <- function(graph)
{
    if( edgemode(graph) == "undirected")
        return(graph)
    if( !is(graph, "graphNEL") )
        stop("only graphNEL supported for now")

    if( duplicatedEdges(graph) )
        stop("there are duplicated edges, cannot handle multigraphs")

    eL <- graph@edgeL
    nN <- nodes(graph)
    ##just in case they are not in the same order!
    eL <- eL[nN]
    for( i in 1:length(eL) ) {
        cNode <- nN[i]
        e <- eL[[i]]
        if( length(e$edges) > 0 ) {
            wh <- nN[e$edges]
            for(j in 1:length(wh) ) {
                eX <- eL[[wh[j]]]$edges
                ##the current node is i so check for it
                if( i %in% eX)
                    next
                eL[[wh[j]]]$edges <- c(i, eX)
                eL[[wh[j]]]$weights <- c(e$weights[j],
                                         eL[[wh[j]]]$weights)
            }
        }
    }
    edgemode(graph) <- "undirected"
    graph@edgeL <- eL
    return(graph)
}


 setGeneric("edgeMatrix",
            function(object, duplicates=FALSE) standardGeneric("edgeMatrix"))

 setMethod("edgeMatrix", c("graphNEL", "ANY"),
           function(object, duplicates) {
                   ## Return a 2 row numeric matrix (from, to, weight)
               ed <- object@edgeL
               ##reorder to the same order as nodes
               ed <- ed[nodes(object)]
               nN <- length(ed)
               eds<-lapply(ed, function(x) x$edges)
               if( require("Biobase") )
                   elem <- listLen(eds)
               else
                   elem <- sapply(ed, length)
               from <- rep(1:nN, elem)
               to <- unlist(eds)
               ans <- rbind(from, to)
               ##we duplicate edges in undirected graphNEL
               ##so here we remove them
               if( edgemode(object) == "undirected"  && !duplicates) {
                   swap <- from>to
                   ans[1,swap]<-to[swap]
                   ans[2,swap]<-from[swap]
                   t1 <- paste(ans[1,], ans[2,], sep="+")
                   ans <- ans[ ,!duplicated(t1), drop=FALSE]
               }
               ans
           })


  setMethod("edgeMatrix", c("clusterGraph", "ANY"),
            function(object, duplicates) {
                cls<-object@clusters
                nd <- nodes(object)
                ans <- numeric(0)
                for(cl in cls) {
                    idx <- match(cl, nd)
                    nn <- length(idx)
                    v1 <- rep(idx[-nn], (nn-1):1)
                    v2 <- numeric(0)
                    for( i in 2:nn)
                        v2 <- c(v2, i:nn)
                    v2 <- idx[v2]
                    ta <- rbind(v1, v2)
                    if( is.matrix(ans) )
                        ans <- cbind(ans, rbind(v1, v2))
                    else
                        ans <- rbind(v1, v2)
                }
                dimnames(ans) <- list(c("from", "to"), NULL)
                ans
            })

  setMethod("edgeMatrix", c("distGraph", "ANY"),
            function(object, duplicates) {
               ## Return a 2 row numeric matrix (from, to, weight)
               ed <- edges(object)
               ##reorder to the same order as nodes
               NODES <- nodes(object)
               ed <- ed[NODES]
               nN <- length(ed)
               if( require("Biobase") )
                   elem <- listLen(ed)
               else
                   elem <- sapply(ed, length)
               from <- rep(1:nN, elem)
               to <- match(unlist(ed), NODES)
               ans <- rbind(from, to)
               ##we duplicate edges in undirected graphNEL
               ##so here we remove them
               ##FIXME: see graphNEL for a speedup of this part
               if( edgemode(object) == "undirected"  && !duplicates) {
                   t1 <- apply(ans, 2, function(x) {paste(sort(x),
                                                           collapse="+")})
                   ans <- ans[ ,!duplicated(t1), drop=FALSE]
               }
               ans
           })



##it seems to me that we might want the edge weights for
##a given edgeMatrix and that that would be much better done
##in the edgeMatrix function
##we are presuming that eM has integer offsets in it
##eWV <- function(g, eM, sep=ifelse(edgemode(g)=="directed", "->",
##                       "--"))
##{
##    unE <- unique(eM[1,])
##    edL <- g@edgeL
##    eE <- lapply(edL, function(x) x$edges)
##    eW <- lapply(edL, function(x) {
##        ans = x$weights
##        ed = length(x$edges)
##        if( is.null(ans) && ed > 0 )
##            ans = rep(1, ed)
##        ans})
##
##    nr <- listLen(eE)
##    ##now we can subset -
##    eMn <- paste(rep((1:length(nr))[unE],nr[unE]), unlist(eE[unE]), sep=sep)
##    eWv <- unlist(eW[unE])
##    dE <- paste(eM[1,], eM[2,], sep=sep)
##    wh<-match(dE, eMn)
##    if(any(is.na(wh)) )
##        stop("edges in supplied edgematrix not found")
##    ans <-eWv[wh]
##    names(ans) <- eMn[wh]
##    ans
##}

#eWV <- function(g, eM, sep=ifelse(edgemode(g)=="directed", "->",
#                       "--"))
#{
#    edL <- g@edgeL
#    ##fix up the edgeweights so we really find them
#    eW <- lapply(edL, function(x) {
#        ans = x$weights
#        ed = length(x$edges)
#        if( is.null(ans) && ed > 0 )
#            ans = rep(1, ed)
#        if( length(ans) > 0 )
#            names(ans) = x$edges
#        ans})
#
#    a1 <- apply(eM, 2,
#                function(x) eW[[x[1]]][as.character(x[2])])
#    names(a1) <- paste(eM[1,], eM[2,], sep=sep)
#    return(a1)
#}


eWV <- function (g, eM, sep = ifelse(edgemode(g) == "directed", "->", 
    "--"), useNNames = FALSE) 
{
#
# returns vector of weights.  default has names equal to node
# indices, but useNNames can be set to put node names as names
# of corresponding weights
#
    edL <- g@edgeL
    eW <- lapply(edL, function(x) {
        ans = x$weights
        ed = length(x$edges)
        if (is.null(ans) && ed > 0) 
            ans = rep(1, ed)
        if (length(ans) > 0) 
            names(ans) = x$edges
        ans
    })
    a1 <- apply(eM, 2, function(x) eW[[x[1]]][as.character(x[2])])
    if (useNNames) 
        names(a1) <- paste(nodes(g)[eM[1, ]], nodes(g)[eM[2, 
            ]], sep = sep)
    else names(a1) <- paste(eM[1, ], eM[2, ], sep = sep)
    return(a1)
}


pathWeights <- function (g, p, eM = NULL) 
{
#
# a path is a vector of names of adjacent nodes
# we form the vector of steps through the path
# (pairs of adjacent nodes) and attach the weights
# for each step.  no checking is done to verify
# that the path p exists in g
#
    if (length(p) < 2) 
        stop("a path must have length > 1")
    if (is.null(eM)) 
        eM <- edgeMatrix(g)
    wv <- eWV(g, eM, useNNames = TRUE)
    sep <- ifelse(edgemode(g) == "undirected", "--", "->")
    pcomps <- cbind(p[-length(p)], p[-1])
    if (edgemode(g) == "undirected") pcomps <- rbind(pcomps, pcomps[,c(2,1)]) # don't know node order in wv labels
    inds <- apply(pcomps, 1, function(x) paste(x[1], x[2], sep = sep))
    tmp <- wv[inds]
    tmp[!is.na(tmp)]
}
