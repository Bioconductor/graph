\name{mostEdges}
\alias{mostEdges}

\title{ Find the node in a graph with the greatest number of edges }
\description{
  \code{mostEdges} finds the node that has the most edges in the graph.
 This is the node with the highest degree.
}
\usage{
mostEdges(objGraph)
}

\arguments{
  \item{objGraph}{ the graph object }
}
\value{
  \item{index}{the index of the node with the most edges}
  \item{id}{the node value with the most edges; may be affy id, locus
                link id, or genename depending on the node type}
  \item{maxLen}{the number of edges for that node}

}

\author{ Elizabeth Whalen }

\seealso{ \code{\link{numEdges}}, \code{\link{aveNumEdges}},
  \code{\link{numNoEdges}}
}

\examples{
  set.seed(123)
  g1 <- randomGraph(11:30, letters[20:26], p=.4)
  mostEdges(g1)
}
\keyword{ manip }