\name{numNoEdges}
\alias{numNoEdges}
\title{ Calculate the number of nodes that have an edge list of NULL }
\description{
  \code{numNoEdges} calculates the number of nodes that have an edge list
 of NULL (i.e. no edges). 
}
\usage{
numNoEdges(objGraph)
}

\arguments{
  \item{objGraph}{ the graph object }
}

\value{
  An integer representing the number of NULL edge lists in the graph.
}

\author{ Elizabeth Whalen }

\seealso{ \code{\link{numEdges}}, \code{\link{aveNumEdges}}, 
   \code{\link{mostEdges}} 
}

\examples{
  set.seed(999)
  g1 <- randomEGraph(letters, .01)
  numNoEdges(g1)
}
\keyword{ manip }
