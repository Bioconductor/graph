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
  \item{index}{If supplied a character or numeric vector of node names
    or indices. }
}
\details{
  If \code{index} is suppled then edge weights from these nodes to all
  adjacent nodes are found and returned. If \code{index} is not supplied
  then the edge weights for all nodes are returned.

  The \code{edgeData} method is the standard way to access edge
  attribute information for a \code{graph} instance.  \code{edgeWeights}
  provides a convenience wrapper around \code{edgeData}.  If an edge
  attribute with name \code{"weight"} is present, it will be used.

  If the attribute values associated with \code{"weight"} are not
  vectors, an error will be triggered.  Although any R object can be
  specified as the attribute of an edge, the structure of the return
  list of \code{edgeWeights} only works with vector objects.
  
  If no edge attribute with name \code{"weight"} has been explicitly
  defined, a default edge weight of 1 will be temporarily assigned; a
  graph with no edge attributes defined will have edge weights of 1
  according to \code{edgeWeights}.
}
\value{
  A named list of named edge weight vectors. The names on the list the
  names of the nodes specified by \code{index}, or all nodes if
  \code{index} was not provided.  The names on the weight vectors are
  node names to identify the edge to which the weight belongs.
}
\author{R. Gentleman }

\seealso{\code{\link{nodes}}, \code{\link{edges}}}

\examples{
  V <- LETTERS[1:4]
  edL2 <- vector("list", length=4)
  names(edL2) <- V
  for(i in 1:4)
    edL2[[i]] <- list(edges=c(2,1,2,1)[i], weights=sqrt(i))
  gR2 <- new("graphNEL", nodes=V, edgeL=edL2, edgemode="directed")
  edgeWeights(gR2, "C")
  edgeWeights(gR2)
  edgeData(gR2, attr="weight")
  edgeData(gR2, from="C", attr="weight")
}
\keyword{manip}
