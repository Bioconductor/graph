\name{randomEGraph}
\alias{randomEGraph}
\title{ Random Edge Graph  }
\description{
  A function to create random graphs according to a random edge model.
  The user supplies the set of nodes for the graph as \code{V} and
  either a probability, \code{p}, that is used for each edge or the
  number of edges, \code{edges} they want to have in the resulting graph.
}
\usage{
randomEGraph(V, p, edges)
}
\arguments{
  \item{V}{The nodes for the graph. }
  \item{p}{ The probability of an edge being selected. }
  \item{edges}{ The number of edges wanted. }
}
\details{
  The user must specify the set of nodes and either a probability for
  edge selection or the number of edges wanted, but not both.
   Let \code{nV} denote the
  number of nodes. There are \code{choose(nV, 2)} edges in the complete
  graph. If \code{p} is specified then a biased coin (probability of
  heads being \code{p}) is tossed for each edge and if it is heads that
  edge is selected. If \code{edges} is specified then that many edges
  are sampled without replacement from the set of possible edges.
}
\value{
  An object of class \code{\link{graphNEL-class}} that contains the nodes and
  edges. 
}
\author{R. Gentleman }

\seealso{\code{\link{randomGraph}}}

\examples{
  set.seed(123)
  V <- letters[14:22]
  g1 <- randomEGraph(V, .2)

  g2 <- randomEGraph(V, edges=30)
}
\keyword{ manip }
