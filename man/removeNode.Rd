\name{removeNode}
\alias{removeNode}
\title{ removeNode }
\description{
  A function to remove a node from a graph. All edges to and from the
  node are also removed.
}
\usage{
removeNode(node, object)
}
\arguments{
  \item{node}{The label of the node to be removed. }
  \item{object}{The graph to remove the node from. }
}
\details{
  The specified node is removed from the graph as are all edges to and
  from that node. A new instance of the same class as \code{object} with
  the specified node(s) is returned.

  Note, node can be a vector of labels, in which case all nodes are
  removed.

  This is similar to \code{\link{subGraph}}.
}
\value{
  A new instance of a graph of the same class as \code{object} but with
  all specified nodes removed.
  
}
\author{R. Gentleman}
\seealso{\code{\link{removeEdge}}, \code{\link{addEdge}},
  \code{\link{addNode}},\code{\link{subGraph}} }

\examples{
  V <- LETTERS[1:4]
  edL2 <- vector("list", length=4)
  names(edL2) <- V
  for(i in 1:4)
    edL2[[i]] <- list(edges=c(2,1,2,1)[i], weights=sqrt(i))
  gR2 <- new("graphNEL", nodes=V, edgeL=edL2, edgemode="directed")
  gX <- removeNode("C", gR2)

}
\keyword{manip}
