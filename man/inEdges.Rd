\name{inEdges}
\alias{inEdges}
\alias{inEdges,graphNEL,missing-method}
\title{ inEdges }
\description{
  Computes and returns a list of all the edges in to the specified set
  of nodes.
}
\usage{
inEdges(node, object)
}
\arguments{
  \item{node}{a set of nodes }
  \item{object}{a \code{graph} object }
}
\details{
  All in edges for the set of nodes are computed. For an undirected graph
  this is also all the out edges.
}
\value{
  A list with one element for each node, the values are the vector of
  node labels for those nodes with edges to the specified node.
}
\author{R. Gentleman}

\seealso{\code{\link{removeNode}}, \code{\link{clearNode}}}

\examples{

  V <- LETTERS[1:4]
  edL3 <- vector("list", length=4)
  for(i in 1:4)
    edL3[[i]] <- list(edges=(i\%\%4)+1, weights=i)
  names(edL3) <- V
  gR3 <- new("graphNEL", nodes=V, edgeL=edL3, "directed")
  inEdges(c("A", "B"), gR3)

}
\keyword{ manip}
