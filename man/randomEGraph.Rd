\name{randomEGraph}
\alias{randomEGraph}
\title{ Random Edge Graph  }
\description{
  A function to create random graphs according to a random edge model.
  The user supplies the set of nodes for the graph as \code{V} and a
  probability, \code{p}, for an edge existing.
}
\usage{
randomEGraph(V, p)
}
\arguments{
  \item{V}{The nodes for the graph. }
  \item{p}{ The probability of an edge being selected. }
}
\details{
   Let \code{nV} denote the
  number of nodes. There are \code{choose(nV, 2)} edges in the complete
  graph. Conceptually. for each of these a biased coin (probability of
  heads being \code{p}) is tossed and if it is heads that edge is
  selected. 
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

}
\keyword{ manip }
