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
  A new graph instance is returned with the edges specified by
  corresponding elements of the \code{from} and \code{to} vectors
  removed.  If \code{from} and \code{to} are not the same length, one of
  them should have length one.  All edges to be removed must exist in
  \code{graph}.
}
\value{
  A new instance of a graph with the same class as \code{graph} is
  returned with the specified edges removed.
}
\author{R. Gentleman}

\seealso{\code{\link{addNode}},\code{\link{addEdge}},\code{\link{removeNode}}}

\examples{
  V <- LETTERS[1:4]
  edL1 <- vector("list", length=4)
  names(edL1) <- V
  for(i in 1:4)
    edL1[[i]] <- list(edges=c(2,1,4,3)[i], weights=sqrt(i))
  gR <- graphNEL(nodes=V, edgeL=edL1)

  gX <- removeEdge("A", "B", gR)

  set.seed(123)
  g <- randomEGraph(V=letters[1:5],edges=5)
  g2 <- removeEdge(from=c("a","b"), to=c("d","c"), g)
}
\keyword{manip}
