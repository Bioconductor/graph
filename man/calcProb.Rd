\name{calcProb}
\alias{calcProb}

\title{ Calculate the hypergeometric probability of the subgraph's number of 
edges.
}
\description{
  \code{calcProb} calculates the probability of having the number of edges
  found in the subgraph given that it was made from \code{origgraph}.
  The hypergeometric distribution is used to calculate the 
  probability (using the pdf).
}
\usage{
calcProb(origgraph, subgraph)
}

\arguments{
  \item{origgraph}{ original graph object from which the subgraph was made }
  \item{subgraph}{ subgraph made from the original graph }
}

\value{
  The probability of the subgraph's number of edges is returned.
}

\author{ Elizabeth Whalen }

\seealso{ \code{\link{calcSumProb}} 
}

\examples{
 #none right now
}
\keyword{ manip }
