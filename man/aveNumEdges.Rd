\name{aveNumEdges}
\alias{aveNumEdges}
\title{ Calculate the average number of edges in a graph }
\description{
  aveNumEdges divides the number of edges in the graph
  by the number of nodes to give the average number of edges. 
}
\usage{
aveNumEdges(objgraph)
}
\arguments{
  \item{objgraph}{ the graph object }
}
\value{
  A double representing the average number of edges will be returned.
}

\author{ Elizabeth Whalen }

\seealso{ \code{\link{numEdges}}, \code{\link{mostEdges}}, 
   \code{\link{numNoEdges}} 
}

\examples{
 set.seed(124)
 g1 <- randomGraph(1:10, letters[7:12], p=.6)
 aveNumEdges(g1)

}
\keyword{ manip }



