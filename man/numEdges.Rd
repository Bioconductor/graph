\name{numEdges}
\alias{numEdges}
\title{ Calculate the number of edges in a graph }
\description{
  \code{numEdges} counts the number of edges in the graph object.  
}
\usage{
numEdges(graph)
}

\arguments{
  \item{graph}{ the graph }
}

\value{
  An integer representing the number of edges will be returned.
}

\author{ Elizabeth Whalen }

\seealso{ \code{\link{aveNumEdges}}, \code{\link{mostEdges}}, 
   \code{\link{numNoEdges}} 
}

\examples{
 set.seed(333)
 g1 <- randomEGraph(letters[13:26], .2)
  numEdges(g1)
}
\keyword{ manip }

