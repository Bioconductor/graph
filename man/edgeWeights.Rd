\name{edgeWeights}
\alias{edgeWeights}
\title{Retrieve the edge weights for specified indices. }
\description{
  Obtains the weights for all edges from the nodes specified by
  \code{index}. 
}
\usage{
edgeWeights(object, index)
}
\arguments{
  \item{object}{A graph, any object that inherits from the \code{graph}
    class.} 
  \item{index}{If supplied either a numeric vector or a vector of node names. }
}
\details{
  If \code{index} is suppled then edge weights from these nodes to all
  adjacent nodes are found and returned. If \code{index} is not supplied
  then the edge weights for all nodes are returned.
}
\value{
  A list of named edge weight vectors. The names are index offsets into
  the set of nodes. 
}
\author{R. Gentleman }

\seealso{\code{\link{nodes}}, \code{\link{edges}}}

\examples{
  V <- LETTERS[1:4]
  edL2 <- vector("list", length=4)
  names(edL2) <- V
  for(i in 1:4)
    edL2[[i]] <- list(edges=c(2,1,2,1)[i], weights=sqrt(i))
  gR2 <- new("graphNEL", nodes=V, edgeL=edL2)
  edgeWeights(gR2, "C")
  edgeWeights(gR2)

}
\keyword{manip}
