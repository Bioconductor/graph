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



