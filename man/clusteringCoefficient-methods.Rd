\name{clusteringCoefficient-methods}
\docType{methods}
\alias{clusteringCoefficient}
\alias{clusteringCoefficient,graph-method}

\title{Clustering coefficient of a graph}

\description{
This generic function takes an object that inherits from the \code{graph} 
class. The graph needs to have \code{edgemode=="undirected"}. If it has
\code{edgemode=="directed"}, the function will return NULL.
}
\usage{
\S4method{clusteringCoefficient}{graph}(object)
}
\arguments{
\item{object}{An instance of the appropriate graph class.}
}

\value{A named numeric vector with the clustering coefficients
  for each node. For nodes with 2 or more edges,
  the values are between 0 and 1.
  For nodes that have no edges, the function returns the value NA.
  For nodes that have exactly one edge, the function returns NaN.
}

\examples{
set.seed(123)
g1 <- randomGraph(letters[1:10], 1:4, p=.3)
clusteringCoefficient(g1)
}

\keyword{methods}
