\name{calcSumProb}
\alias{calcSumProb}

\title{ Calculate the probability that a subgraph has an unusual number
  of edges.}
\description{
  For any graph a set of nodes can be used to obtain an induced subgraph
  (see \code{\link{subGraph}}). An interesting question is whether that
  subgraph has an unusually large number of edges. This function
  computes the probability that a \emph{random} subgraph with the same
  number of nodes has more edges than the number observed in the
  presented subgraph. The appropriate probability distribution is
  the hypergeometric.
}
\usage{
calcSumProb(sg, g)
}

\arguments{
  \item{sg}{ subgraph made from the original graph }
  \item{g}{ original graph object from which the subgraph was made }
}

\value{
  The probability of having greater than or equal to the subgraph's 
  number of edges is returned.
}
\details{
The computation is based on the following argument. In the original
graph there are \eqn{n} nodes and hence \eqn{N=n*(n-1)/2} edges in the
complete graph. If we consider these \eqn{N} nodes to be of two types,
corresponding to those that are either in our graph, \code{g}, or not in
it. Then we think of the subgraph which has say \eqn{m} nodes and
\eqn{M=m*(m-1)/2} possible edges as representing \eqn{M} draws from an
urn containing \eqn{N} balls of which some are white (those in \code{g})
and some are black. We count the number of edges in the subgraph and use
a Hypergeomtric distribution to ask whether our subgraph is particularly
dense. 
  }
\author{ Elizabeth Whalen }

\seealso{ \code{\link{calcProb}} }

\examples{
  set.seed(123)
  V <- letters[14:22]
  g1 <- randomEGraph(V, .2)

  sg1 <- subGraph(letters[c(15,17,20,21,22)], g1)
  calcSumProb(sg1, g1)
}
\keyword{ manip }
