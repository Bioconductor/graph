\name{subGraph}
\alias{subGraph}
\alias{origgraph}
\alias{subnodes}

\title{ Makes a SubGraph from an existing Graph }
\description{
 subGraph creates a new subgraph (of type class graph).  The subgraph, 
 which is a subset of the original graph, will be created based on the 
 subnodes parameter. 
}
\usage{
subGraph(graph, snodes)
}

\arguments{
  \item{graph}{ the original graph from which the subgraph will 
                    be created }
  \item{snodes}{ the vector of all the nodes in the subgraph }
}
\details{
  The \code{snodes} must be nodes in the original graph.  Any nodes passed
  in as a parameter that are not part of the original graph will be 
  removed before making the subgraph.
}
\value{
  A graph that is a subset of the original graph is returned.
}

\author{ Elizabeth Whalen }

\seealso{\code{\link{makeGraph}},
         \code{\link{graphBoundary}}, \code{\link{boundary}} }

\examples{

}
\keyword{ manip }







