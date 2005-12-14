\name{inEdges}
\alias{inEdges}
\title{Generic Method inEdges}
\description{
  Returns a list of all incoming edges for the specified nodes.
}
\usage{
inEdges(node, object)
}
\arguments{
  \item{node}{character vector of node names}
  \item{object}{a \code{graph} object}
}
\details{
  If no \code{node} argument is specified, \code{inEdges} returns the
  incoming edges for all nodes in the graph.

  For an undirected graph, \code{inEdges} returns all edges for the
  specified nodes.
}
\value{
  A list with length matching the length of \code{node}.  If \code{node}
  was missing, a list containing an element for each node in the graph.

  Each list element contains a character vector of node names giving the
  nodes that have outgoing edges to the node given by the name of the
  list element.
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
