\name{boundary}
\alias{boundary}

\title{ Makes a Boundary to a SubGraph }
\description{
 \code{boundary} creates a new boundary that is of type list.  Only elements 
 that are connected to the subgraph nodes in the original graph, but are 
 not part of the subgraph, will be returned.
}
\usage{
boundary(origgraph, subnodegraph)
}

\arguments{
  \item{origgraph}{ the original graph from which the boundary will 
                    be created }
  \item{subnodegraph}{ can either be the vector of all the nodes in the 
                    subgraph or the actual subgraph object }
}
\details{
  Instead of returning a graph object, this function returns
  a list.  The names of the list will be the nodes in the original graph 
  that are connected to the subgraph, but are not actually part of the 
  subgraph, and the elements of the list will be the nodes in the subgraph 
  that they are connected to.  This reversing of the edge list will allow 
  a user to tell if a node is connected to more than one element in the 
  subgraph.
}
\value{
  A list that is a boundary to the subgraph will be returned.
}

\author{ Elizabeth Whalen }

\seealso{\code{\link{makeGraph}}},
         \code{\link{subGraph}}, \code{\link{graphBoundary}}
 }

\examples{

}
\keyword{ manip }







