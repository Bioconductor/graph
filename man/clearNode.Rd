\name{clearNode}
\alias{clearNode}
\title{ clearNode }
\description{
  This function removes all edges to or from the specified node in the graph.
}
\usage{
clearNode(node, object)
}
\arguments{
  \item{node}{a node }
  \item{object}{a \code{graph} }
}
\details{
  All edges to and from \code{node} are removed. \code{node} can be a
  vector. 
}
\value{
  A new instance of the graph with all edges to and from the specified
  node(s) removed.
}
\author{R. Gentleman}

\seealso{\code{\link{removeNode}}, \code{\link{removeEdge}}}

  \examples{
 V <- LETTERS[1:4]
 edL3 <- vector("list", length=4)
 for(i in 1:4)
   edL3[[i]] <- list(edges=(i\%\%4)+1, weights=i)
 names(edL3) <- V
 gR3 <- new("graphNEL", nodes=V, edgeL=edL3, "directed")
 g4 <- clearNode("A", gR3)

}
\keyword{manip }
