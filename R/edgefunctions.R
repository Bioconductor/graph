################################################################
# function:
# aveNumEdges takes one parameter:
#   objgraph is the graph object
# aveNumEdges counts the number of edges in the graph and divides
# that by the number of nodes in the graph to give the
# average number of edges.  A double representing the average
# number of edges will be returned.
#
# created by: Elizabeth Whalen
# last updated: July 22, 2002
################################################################

aveNumEdges<-function(objgraph)
{
  numbNodes<-length(nodes(objgraph))
  numbEdges<-numEdges(objgraph)

  aveNumEdges<-numbEdges/numbNodes
  aveNumEdges
}

################################################################
# function:
# numEdges takes one parameter:
#   objgraph is the graph object
# numEdges counts the number of edges in the graph.  First, it
# sums up all the length of the edge list and this sum must be
# an even number because each edge is repeated twice.  To calculate
# the number of edges, the sum is divided by two.  An integer
# representing the number of edges will be returned.
#
# notes: The number of edges is divided by two because each edge
# is repeated twice, for most of our ways of representing undirected
# graphs
#
# created by: Elizabeth Whalen
# last updated: July 22, 2002
################################################################

numEdges <- function(graph)
{
  numbEdges <- length(unlist(edges(graph),use.names=FALSE))

##  if (numbEdges %% 2 != 0)
##    stop("The graph is not valid because of the number of edges.")
  if( graph@edgemode == "undirected" )
      numbEdges<-numbEdges/2
  numbEdges
}

################################################################
# function:
# calcProb takes two parameters:
#   origgraph is the original graph from which the subgraph was made
#   subgraph is the subgraph made from the original graph
# calcProb calculates the probability of having the number of edges
# found in the subgraph given that it was made from origgraph.
# The hypergeometric distribution is used to calculate the
# probability (using the pdf).
#
# created by: Elizabeth Whalen
# last updated: July 22, 2002
################################################################

calcProb <- function(origgraph,subgraph)
{
  origNumNodes<-length(nodes(origgraph))
  subNumNodes<-length(nodes(subgraph))

  origNumEdges<-numEdges(origgraph)
  subNumEdges<-numEdges(subgraph)

  dyads <- (origNumNodes * (origNumNodes - 1) / 2) - origNumEdges
  sampledyads <- subNumNodes * (subNumNodes - 1) / 2

  prob<-dhyper(subNumEdges,origNumEdges,dyads,sampledyads)

  prob
}

################################################################
# function:
# calcSumProb takes two parameters:
#   origgraph is the original graph from which the subgraph was made
#   subgraph is the subgraph made from the original graph
# calcSumProb calculates the probability of having greater than or equal
# to the number of edges found in the subgraph given that it was made
# from origgraph.
# The hypergeometric distribution is used to calculate the summed
# probability (using the cdf).
#
# notes: This calculates the upper tail of the hypergeometric
# distribution.
#
# created by: Elizabeth Whalen
# last updated: July 22, 2002
################################################################

calcSumProb <- function(origgraph,subgraph)
{
  origNumNodes<-length(nodes(origgraph)) #g
  subNumNodes<-length(nodes(subgraph))   #gs

  origNumEdges<-numEdges(origgraph)      #L
  subNumEdges<-numEdges(subgraph)        #Ls

  dyads <- (origNumNodes * (origNumNodes - 1) / 2) - origNumEdges
  sampledyads <- subNumNodes * (subNumNodes - 1) / 2

  prob<-phyper(subNumEdges,origNumEdges,dyads,sampledyads,lower.tail=FALSE)

  prob
}

################################################################
# function:
# mostEdges takes one parameter:
#   objGraph is the graph object
# mostEdges finds the node that has the most edges in the graph.
# The index of the node, the node id (ex. affy id, locus
# link id, ...), and the number of edges for that node is returned
# in a list.
#
# created by: Elizabeth Whalen
# last updated: August 2, 2002
################################################################

mostEdges<-function(objGraph)
{
  oEdges<-edges(objGraph)
  oNodes<-nodes(objGraph)
  maxLen<-0
  index<-0
  for (i in 1: length(oEdges))
  {
    if (!is.null(oEdges[[i]]) && length(oEdges[[i]]) > maxLen)
    {
      maxLen <- length(oEdges[[i]])
      index <- i
    }
  }
  id<-oNodes[index]
  indexLen<-list(index=index,id=id,maxLen=maxLen)
  indexLen
}

################################################################
# function:
# numNoEdges takes one parameter:
#   objGraph is the graph object
# numNoEdges calculates the number of nodes that have an edge list
# of NULL (i.e. no edges) and returns an integer representing
# the number of NULL edge lists in the graph.
#
# created by: Elizabeth Whalen
# last updated: July 22, 2002
################################################################

numNoEdges<-function(objGraph)
{
 els <- sapply(edges(objGraph), length)
 sum(els==0)
}


