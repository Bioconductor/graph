\name{edgeWeights}
\alias{edgeWeights}
\title{Retrieve the edge weights of a graph}
\description{
  A generic function that returns the edge weights of a graph.  If
  \code{index} is specified, only the weights for the edges from the
  specified nodes are returned.  The user can control which edge
  attribute is interpreted as the weight, see the Details section.
}
\usage{
edgeWeights(object, index, ..., attr = "weight", default = 1, type.checker = is.numeric)
}
\arguments{
  \item{object}{A graph, any object that inherits from the \code{graph}
    class.} 
  \item{index}{If supplied, a character or numeric vector of node names
    or indices.}
  \item{...}{Unused.}
  \item{attr}{The name of the edge attribute to use as a weight.  You
    can view the list of defined edge attributes and their default values
    using \code{edgeDataDefaults}.  The default attribute name is
    \code{"weight"}, see the Details section.}
  \item{default}{The value to use if \code{object} has no edge attribute
    named by the value of \code{attr}.  The default is the value 1
    (double).}
  \item{type.checker}{A function that will be used to check that the
    edge weights are of the correct type.  This function should return
    TRUE if the input vector is of the right type and FALSE otherwise.
    The default is to check for numeric edge weights using
    \code{is.numeric}.  If no type checking is desired, specify
    \code{NULL}.}
}
\details{
  If \code{index} is suppled, then edge weights from these nodes to all
  adjacent nodes are returned. If \code{index} is not supplied, then the
  edge weights for all nodes are returned.  The value for nodes without
  any outgoing edges will be a zero-length vector of the appropriate
  mode.

  The \code{edgeWeights} method is a convenience wrapper around
  \code{edgeData}, the general-purpose way to access edge attribute
  information for a \code{graph} instance.  In general, edge attributes
  can be arbitary R objects.  However, for \code{edgeWeights} to make
  sense, the values must be vectors of length not more than one.

  By default, \code{edgeWeights} looks for an edge attribute with name
  \code{"weight"} and, if found, uses these values to construct the edge
  weight list.  You can make use of attributes stored under a different
  name by providing a value for the \code{attr} argument.  For example,
  if \code{object} is a graph instance with an edge attribute named
  \code{"WTS"}, then the call \code{edgeWeights(object, attr="WTS")}
  will attempt to use those values.

  The function specified by \code{type.checker} will be given a vector
  of edge weights; if the return value is not \code{TRUE}, then an error
  will be signaled indicating that the edge weights in the graph are not
  of the expected type.  Type checking is skipped if \code{type.checker}
  is \code{NULL}.

  If the graph instance does not have an edge attribute with name given
  by the value of the \code{attr} argument, \code{default} will be used
  as the weight for all edges.  Note that if there is an attribute named
  by \code{attr}, then its default value will be used for edges not
  specifically customized.  See \code{edgeData} and
  \code{edgeDataDefaults} for more information.

  Because of their position after the \code{...}, no partial matching is
  performed for the arguments \code{attr}, \code{default}, and
  \code{type.checker}.  
  
}
\value{
  A named list of named edge weight vectors. The names on the list are
  the names of the nodes specified by \code{index}, or all nodes if
  \code{index} was not provided.  The names on the weight vectors are
  node names to identify the edge to which the weight belongs.
}
\author{R. Gentleman and S. Falcon}

\seealso{
  \code{\link{nodes}}
  \code{\link{edges}}
  \code{\link{edgeData}}
  \code{\link{edgeDataDefaults}}
  \code{\link{is.numeric}}
  \code{\link{is.integer}}
  \code{\link{is.character}}
}

\examples{
  V <- LETTERS[1:4]
  edL2 <- vector("list", length=4)
  names(edL2) <- V
  for(i in 1:4)
    edL2[[i]] <- list(edges=c(2,1,2,1)[i], weights=sqrt(i))
  gR2 <- new("graphNEL", nodes=V, edgeL=edL2, edgemode="directed")
  edgeWeights(gR2, "C")
  edgeWeights(gR2)
  edgeWeights(gR2, attr="foo", default=5)
  edgeData(gR2, attr="weight")
  edgeData(gR2, from="C", attr="weight")
}
\keyword{manip}
