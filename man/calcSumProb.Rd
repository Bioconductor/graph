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
calcSumProb(origgraph, subgraph)
}

\arguments{
  \item{origgraph}{ original graph object from which the subgraph was made }
  \item{subgraph}{ subgraph made from the original graph }
}

\value{
  The probability of having greater than or equal to the subgraph's 
  number of edges is returned.
}

\author{ Elizabeth Whalen }

\seealso{ \code{\link{calcProb}} }

\examples{
#  data(pmedu95aAffy,package=graph)
#  Anodes <- nodes(pmedu95aAffy)
#  subAgraph <- subGraph(pmedu95aAffy,Anodes[1:10])
#  calcSumProb(pmedu95aAffy,subAgraph)
}
\keyword{ manip }
