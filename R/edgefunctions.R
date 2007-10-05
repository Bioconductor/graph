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
  numEdges(objgraph)/length(nodes(objgraph))


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

calcProb <- function(subgraph, origgraph)
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
#   g   is the original graph from which the subgraph was made
#   sg  is the subgraph made from the original graph
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

calcSumProb <- function(sg, g)
{
  origNumNodes<-length(nodes(g)) #g
  subNumNodes<-length(nodes(sg))   #gs

  origNumEdges<-numEdges(g)      #L
  subNumEdges<-numEdges(sg)        #Ls

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
  lens <- sapply(oEdges, length)
  mx <- max(lens)
  return(names(oEdges)[match(mx, lens)])
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



##########################################################
##RG/2003
##########################################################

##listEdges: list all edges for every pair of nodes in the graph
##so if there N nodes then there are N choose 2 possible entries
##dropNULL=T/F says whether to drop those pairs with no edges


listEdges <- function(object, dropNULL=TRUE)
{
    if( !is(object, "graphNEL") )
        stop("listEdges only works for graphNEL objects")
    Nd <- nodes(object)
    Nn <- length(Nd)
    EL <- object@edgeL
    eList <- NULL
    for(i in 1:Nn) {
        Node <- Nd[i]
        ELi <- EL[[i]]
        for( j in seq(along=ELi$edges) ) {
            toN <- Nd[ELi$edges[j]]
            btwn <- paste(sort(c(Node, toN)), collapse=":")
            newN <-new("simpleEdge",
                    bNode=Node, eNode=toN,
                    weight=if( is.null(ELi$weights[j])) 1 else ELi$weights[j] ,
                    directed = edgemode(object)=="directed",
                    edgeType=if(is.null(ELi$type[j])) "" else
                       ELi$type[j])
            if( is.null(eList[[btwn]]) )
                eList[[btwn]] <- list(newN)
            else
                eList[[btwn]] <- list(eList[[btwn]], newN )
        }
    }
    return(eList)
}


listLen <- function(list)
    .Call("listLen", list, PACKAGE="graph")


