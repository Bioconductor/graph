################################################################
# function:
# graphBoundary takes three parameters:
#   origgraph is the original graph from which the subgraph will be created
#   subnodes is the vector of all the nodes in the subgraph
#   boundary is a boolean - if false a subgraph is created, and if true
#    a boundary to the subgraph is created
# graphBoundary creates a new subgraph (of type class graph).
# The subgraph, which is a subset of the original graph,
# will be created based on the nodes.
# If boundary is true, then only elements that are connected to
# the nodes, but are not part of the subgraph will be returned.
#
# notes: This function can make a subgraph or a boundary to a
# subgraph.
#
# created by: Elizabeth Whalen
# last updated: August 1, 2002
################################################################

graphBoundary<-function(origgraph, subnodes, boundary)
{
  if (class(origgraph) != "graph")
    stop("The first parameter must be an object of type graph.")
  if (length(nodes(origgraph))==0)
    stop("The first parameter must be an initialized graph object.")
  if (!is.vector(subnodes))
    stop("The second parameter must be a vector of nodes.")

  orignodes<-nodes(origgraph)
  origedges<-edges(origgraph)

  #only use nodes that are in the original graph
  validnodes <- subnodes[subnodes %in% orignodes]

  #nodes that were not in the original graph
  invalidnodes <- subnodes[!subnodes %in% orignodes]

  subgraph<-new("graph")
  subgraph@nodes<-validnodes
  subedges<-vector(mode="list",length=length(validnodes))
  names(subedges)<-validnodes

  for (i in 1:length(validnodes))
  {
    curorigEdge<-origedges[[validnodes[i]]]
    if (!is.null(curorigEdge) && length(curorigEdge) > 0 )
    {
      curorigEdge<-unlist(curorigEdge,use.names=FALSE)
      #take only elements that are in the subgraph's node vector
      if (boundary==FALSE)
        cursubEdge<- curorigEdge[curorigEdge %in% validnodes]
      else
        cursubEdge<- curorigEdge[!curorigEdge %in% validnodes]
      if (length(cursubEdge) > 0)
        subedges[[validnodes[i]]]<-c(cursubEdge)
      #else leave edge list as NULL
    }
  }
  subgraph@edges<-subedges
  if (!boundary && !validGraph(subgraph))
    stop("A valid subgraph was not created.")
  subgraph
}

################################################################
# function:
# subGraph takes two parameters:
#   origgraph is the original graph from which the subgraph will be created
#   subnodes is the vector of all the nodes in the subgraph
# subGraph creates a new subgraph (of type class graph).
# The subgraph, which is a subset of the original graph,
# will be created based on the nodes.
#
# created by: Elizabeth Whalen
# last updated: August 1, 2002
################################################################

subGraph<-function(origgraph,subnodes)
  graphBoundary(origgraph,subnodes,FALSE)


################################################################
# function:
# boundary takes two parameters:
#   origgraph is the original graph from which the subgraph will be created
#   subnodegraph can be either the vector of all the nodes in the subgraph
#     or it can be the subgraph object
# boundary creates a boundary to the subgraph that would be
# made with the subnodes from the origgraph.  Only elements that
# are connected to the nodes in the original graph, but are not
# part of the subgraph will be returned.
#
# notes: Instead of returning a graph object, this function returns
# a list.  The names of the list will be the nodes connected
# to the subgraph in the original graph and the elements of the list
# will be the nodes in the subgraph that they are connected to.
# This reversing of the edge list will allow a user to tell if a node
# is connected to more than one element in the subgraph.
#
# created by: Elizabeth Whalen
# last updated: August 2, 2002
################################################################

boundary<-function(origgraph, subnodegraph)
{
  #if had a subgraph parameter then would need to get the nodes of the
  #subgraph and use those as the subnodes

  if (is(subnodegraph, "graph"))
    subnodes<-nodes(subnodegraph)
  else
    subnodes<-subnodegraph

  boundaryGraph<-graphBoundary(origgraph, subnodes, TRUE)

  #now need to get the edge list and reverse the order of the list
  boundaryEdge<-edges(boundaryGraph)

  #get the vector of the unique values in the egde list
  boundaryVec<-uniqueVec(boundaryEdge,TRUE)

  boundaryList<-revList(boundaryVec,boundaryEdge)

  boundaryList
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


