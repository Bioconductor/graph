\name{removeEdge}
\alias{removeEdge}
\title{ removeEdge }
\description{
  A function to remove the specified edges from a graph.
}
\usage{
removeEdge(from, to, graph)
}
\arguments{
  \item{from}{from edge labels }
  \item{to}{ to edge labels}
  \item{graph}{a \code{graph} object }
}
\details{
  The edges in the graph from nodes in the \code{from} set to the
  \code{to} set are removed and a new graph object returned.
  Both \code{from} and \code{to} can be vectors, not necessarily of the
  same length (in which case standard rules for replicating the shorter
  one apply).

  If the graph is \code{undirected} then the undirected edge is removed.
  
}
\value{
  A new instance of a graph with the same class as \code{graph} is
  returned. Any of the listed edges in \code{graph} have been removed.
}
\author{R. Gentleman}

\seealso{\code{\link{addNode}},\code{\link{addEdge}},\code{\link{removeNode}},}

\examples{
  V <- LETTERS[1:4]
  edL1 <- vector("list", length=4)
  names(edL1) <- V
  for(i in 1:4)
    edL1[[i]] <- list(edges=c(2,1,4,3)[i], weights=sqrt(i))
  gR <- new("graphNEL", nodes=V, edgeL=edL1)

  gX <- removeEdge("A", "B", gR)
}
\keyword{manip}
