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
    stop("The first parameter must be an object of type graph.")

  if( is(subgraph, "graph") )
      snodes <- nodes(subgraph)
  else
      snodes <- subgraph

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
