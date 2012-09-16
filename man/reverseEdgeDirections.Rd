\name{reverseEdgeDirections}
\alias{reverseEdgeDirections}

\title{Reverse the edges of a directed graph}
\description{
  Return a new directed graph instance with each edge oriented in the
  opposite direction relative to the corresponding edge in the input
  graph.
}
\usage{
reverseEdgeDirections(g)
}
\arguments{
  \item{g}{A \code{graph} subclass that can be coerced to \code{graphAM}}
}
\details{
  WARNING: this doesn't handle edge attributes properly.  It is a
  preliminary implementation and subject to change.
}
\value{
  A \code{graphNEL} instance
}
\author{S. Falcon}
\examples{
g <- graphNEL(nodes=c("a", "b", "c"),
         edgeL=list(a=c("b", "c"), b=character(0), c=character(0)),
         edgemode="directed")

stopifnot(isAdjacent(g, "a", "b"))
stopifnot(!isAdjacent(g, "b", "a"))

grev <- reverseEdgeDirections(g)
stopifnot(!isAdjacent(grev, "a", "b"))
stopifnot(isAdjacent(grev, "b", "a"))

}

\keyword{manip}

