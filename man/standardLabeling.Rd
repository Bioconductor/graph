\name{Standard labeling of edges with integers}
\alias{ftM2int}
\alias{int2ftM}
\title{Standard labeling of edges with integers}
\description{
Functions to convert between from-to representation and standard
labeling of the edges for undirected graphs with no self-loops.
}
\usage{
ftM2int(ft)
int2ftM(i)
}
\arguments{
  \item{i}{Numeric vector.}
  \item{ft}{Numeric nx2 or 2xn matrix.}
}
\details{A standard 1-based node labeling of a graph G=(V,E)
  is a one-to-one mapping between the integers from 1 to |V|
  and the nodes in V. A standard 1-based edge
  labeling of an undirected graph G=(V,E) with no self-loops
  is \emph{the} one-to-one mapping between the integers from 1
  to |V| choose 2 = |V|*(|V|-1)/2 such that the edge labeled 1 is
  between nodes 2 and 1, the edge labeled 2 is between
  nodes 3 and 1, the edge labeled 3 is between nodes 3 and 2,
  and so on. 
}

\value{
For \code{ftM2int}, a numeric vector of length n. 
For \code{int2ftM}, a \code{length(i) x 2} matrix.
}

\author{Wolfgang Huber}

\examples{
 nNodes <- 200
 nEdges <- choose(nNodes, 2)
 i <- 1:nEdges
 ft <- int2ftM(i)
 ft[1:6,]
 stopifnot(all(ft[,1]>ft[,2])) ## always from higher to lower
 stopifnot(!any(duplicated(paste(ft[,1], ft[,2]))))
 stopifnot(ft[nEdges, 1]==nNodes, ft[nEdges, 2]==nNodes-1) 

 j <- ftM2int(ft)
 stopifnot(all(i==j))
}

\keyword{graphs}
