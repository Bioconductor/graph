\name{addNode}
\alias{addNode}
\title{ addNode }
\description{
  Add a node to a graph.
}
\usage{
addNode(node, object)
}
\arguments{
  \item{node}{a character string, or node object }
  \item{object}{a \code{graph} }
}
\details{
  The supplied node is added to the set of nodes of the \code{object}.
}
\value{
  A new graph of the same class as \code{object} with the supplied node
  added to the set of nodes.
}
\author{R. Gentleman }
\seealso{\code{\link{removeNode}}, \code{\link{removeEdge}},
  \code{\link{addEdge}}}

  \examples{

  V <- LETTERS[1:4]
  edL1 <- vector("list", length=4)
  names(edL1) <- V
  for(i in 1:4)
     edL1[[i]] <- list(edges=c(2,1,4,3)[i], weights=sqrt(i))
  gR <- new("graphNEL", nodes=V, edgeL=edL1)
  gX <- addNode("X", gR)
}
\keyword{ manip }
