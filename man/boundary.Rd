\name{boundary}
\alias{boundary}

\title{ Returns the Boundary between a Graph and a SubGraph }
\description{
 The boundary of a subgraph is the set of nodes in the original graph
 that have edges to nodes in the subgraph. The function \code{boundary}
 computes the boundary and returns it as a list whose length is the same
 length as the number of nodes in the subgraph.
}
\usage{
boundary(subgraph, graph)
}

\arguments{
  \item{graph}{ the original graph from which the boundary will 
                    be created }
  \item{subgraph}{ can either be the vector of the node labels or the
    subgraph itself.}
}
\details{
  This function returns a named list. The elements of the list
  correspond to the nodes in the subgraph. The elements are lists of the
  names of the nodes in the graph which share an edge with the
  respective node in the subgraph.

  Element "a", say of the list corresponds to node "a" and the value in
  this list is a vector of node labels. Those nodes are in the graph
  (but not the subgraph) and share an edge with node "a" in the subgraph.

}
\value{
  A list that is a boundary of the subgraph will be returned.
}

\author{ Elizabeth Whalen and R. Gentleman }

\seealso{\code{\link{makeGraph}}},
         \code{\link{subGraph}}, \code{\link{graph}}
 }

\examples{

}
\keyword{ manip }







