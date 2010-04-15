\name{addNode}
\alias{addNode}
\alias{addNode,character,graphNEL,missing-method}
\alias{addNode,character,graphNEL,list-method}
\alias{addNode,character,distGraph,list-method}
\alias{addNode,character,graphBAM,missing-method}
\title{ addNode }
\description{
  Add one or more nodes to a graph.
}
\usage{
addNode(node, object, edges)
}
\arguments{
  \item{node}{A character vector of node names. }
  \item{object}{A \code{graph} }
  \item{edges}{A named list of edges.} 
}
\details{
  The supplied \code{node}s are added to the set of nodes of the
  \code{object}.

  If \code{edges} are provided then their must be the
  same number as there are \code{node}s and the must be in the same
  order. The elements of the \code{edges} list are vectors. They can be
  character vectors of node labels for nodes in \code{object} and if so
  then they are added with unit weights. If the vector is numeric then
  it must be named (with labels corresponding to nodes in the
  \code{object}) and the values are taken to be the edge weights.
  
  When the \code{object} is a \code{distGraph} then the \code{edges} must 
  be supplied and they must contain appropriate distances for all nodes 
  both those in \code{object} and those supplied.
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

set.seed(123)
g1 <- randomGraph(letters[1:10], 1:4, p=.3)
g2 <- addNode("z", g1, edges=list(c("a", "h", "g")))

}
\keyword{ manip }
