################################################################
# function:
# boundary takes three parameters:
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

################################################################
# function:
# makeGraph takes four parameters:
#   object is the graph object that will be created
#   linkBasis is a character string that tells how the nodes will be linked
#   nodeType is a character string that tells what the node types are
#    (ex. affy ids)
#   lib is the library that contains the environments that will be used
#    to create the graph object
#
# makeGraph calls another function based on the link type
# and this second function will create a new graph object based
# on the node type.
#
# notes:
# Currently, the only linkBasis available is pmid.
#
# created by: Elizabeth Whalen
# last updated: July 12, 2002
################################################################

makeGraph<-function(object,linkBasis,nodeType,lib)
 {
   linkBasis<-match.arg(linkBasis,c("pmid"))
   switch(linkBasis,
          pmid=makePubmedGraph(object,nodeType,lib))
 }


