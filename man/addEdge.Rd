\name{addEdge}
\alias{addEdge}
\title{ addEdge }
\description{
  A function to add an edge to a graph.
}
\usage{
addEdge(from, to, graph, weights)
}
\arguments{
  \item{from}{The node the edge starts at }
  \item{to}{The node the edge goes to. }
  \item{graph}{The graph that the edge is being added to. }
  \item{weights}{A vector of weights, one for each edge. }
}
\details{
  Both \code{from} and \code{to} can be vectors. They need not be the
  same length (if not the standard rules for replicating the shorter one
  are used). Edges are added to the graph between the supplied nodes.

  The \code{weights} are given for each edge.

  The implementation is a bit too oriented towards the \code{graphNEL}
  class and will likely change in the next release to accomodate more
  general graph classes.

  If the graph is undirected then the edge is bidirectional (and only
  needs to be added once). For directed graphs the edge is directional.
}
\value{
  A new instance of a graph object with the same class as \code{graph}
  but with the indicated edges added.
}
\author{R. Gentleman}
\seealso{\code{\link{addNode}},\code{\link{removeEdge}},
    \code{\link{removeNode}} }

  \examples{
V <- LETTERS[1:4]
edL2 <- vector("list", length=4)
names(edL2) <- V
for(i in 1:4)
  edL2[[i]] <- list(edges=c(2,1,2,1)[i], weights=sqrt(i))
gR2 <- new("graphNEL", nodes=V, edgeL=edL2, edgemode="directed")

gX <- addEdge("A", "C", gR2, 1)

gR3 <- randomEGraph(letters[10:14], .4)
gY <- addEdge("n", "l", gR3, 1)

}
\keyword{ manip }
