\name{combineNodes}
\alias{combineNodes}
\title{ combineNodes  }
\description{
  A function to combine, or collapse, a specified set of nodes in a graph.
}
\usage{
combineNodes(nodes, graph, newName)
}
\arguments{
  \item{nodes}{A set of nodes that are to be collapsed. }
  \item{graph}{The graph containing the nodes }
  \item{newName}{The name for the new, collapsed node. }
}
\details{
  The nodes specified are reduced to a single new node with label given
  by \code{newName}. The in and out edges of the set of nodes are all
  made into in and out edges for the new node.
}
\value{
  An new instance of a graph of the same class as \code{graph} is
  returned. This new graph has the specified nodes reduced to a single
  node. 
}
\author{R. Gentleman}

\seealso{\code{\link{inEdges}}, \code{\link{addNode}}}

\examples{
  V <- LETTERS[1:4]
  edL1 <- vector("list", length=4)
  names(edL1) <- V
  for(i in 1:4)
    edL1[[i]] <- list(edges=c(2,1,4,3)[i], weights=sqrt(i))
  gR <- new("graphNEL", nodes=V, edgeL=edL1)
  gR <- addNode("M", gR)
  gR <- addEdge("M", "A", gR, 1)
  gR <- addEdge("B", "D", gR, 1)
  gX <- combineNodes(c("B","D"), gR, "X")
}
\keyword{ manip}
