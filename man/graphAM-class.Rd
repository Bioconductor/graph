\name{graphAM-class}
\docType{class}
\alias{graphAM-class}
\alias{graphAM}
\alias{addEdge,character,character,graphAM,missing-method}
\alias{addNode,character,graphAM,missing-method}
\alias{clearNode,character,graphAM-method}
\alias{coerce,graphAM,graphNEL-method}
\alias{coerce,graphAM,matrix-method}
\alias{coerce,matrix,graphAM-method}
\alias{coerce,graphAM,graphBAM-method}
\alias{edges,graphAM-method}
\alias{initialize,graphAM-method}
\alias{inEdges,missing,graphAM-method}
\alias{inEdges,character,graphAM-method}
\alias{inEdges,graphAM,missing-method}
\alias{isAdjacent,graphAM,character,character-method}
\alias{nodes<-,graphAM,character-method}
\alias{nodes,graphAM-method}
\alias{numEdges,graphAM-method}
\alias{numNodes,graphAM-method}
\alias{removeEdge,character,character,graphAM-method}
\alias{removeNode,character,graphAM-method}

\title{Class "graphAM"}

\description{
  A graph class where node and edge information is represented as an
  adjacency matrix.  The adjacency matrix is square and element
  \code{adjMat[i, j]} is one if there is an edge from node i to
  node j and zero otherwise.
}

\details{
  The non-zero matrix values can be used to initialize an edge
  attribute.  If this is desired, use the \code{values} argument in the
  call to \code{new} and provide a list with a single named element.
  The name determines the attributes and the value provides the default
  value for that attribute.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{graphAM(adjMat,
  edgemode, values)}.
}
\section{Slots}{
  \describe{
    \item{\code{adjMat}:}{An adjacency \code{"matrix"} describing the
    graph structure.  The \code{\link{colnames}} of the matrix will be used as
    node names for the graph if present.}
    \item{\code{edgeData}:}{Storage for edge attributes.}
    \item{\code{nodeData}:}{Storage for node attributes.}
  }
}
\section{Extends}{
Class \code{"graph"}, directly.
}

\section{Constructor}{
  \code{graphAM(adjMat=matrix(integer(), 0, 0), edgemode='undirected', values=NA)}
  creates a graphAM instance.
  \describe{
    \item{adjMat}{An \code{integer} matrix specifying which nodes have
      edges between them.}
    \item{edgemode}{Either "directed" or "undirected".}
    \item{values}{A named list of length 1, used (rather obscurely) to
      specify that non-zero adjMat values initialize an edge attribute.
     The name of the single element in that list becomes the name of that
     attribute, with the specified default value.
     This default value is, however, never used:  the specified edge
  attribute always has the value contained in the adjacency matrix,
  which is traditionally 1, but can be any positive number.}
    }
}

\section{Methods}{
  \describe{
    \item{addEdge}{\code{signature(from = "character", to = "character", graph = "graphAM", weights = "missing")}: ... }
    \item{addNode}{\code{signature(object = "graphAM", nodes = "character")}: ... }
    \item{clearNode}{\code{signature(node = "character", object = "graphAM")}: ... }
    \item{coerce}{\code{signature(from = "graphAM", to = "graphNEL")}: ... }
    \item{coerce}{\code{signature(from = "graphAM", to = "graphBAM")}: ... }
    \item{coerce}{\code{signature(from = "graphAM", to = "matrix")}: In
      converting to a \code{matrix}, if an edge attribute named
      \code{"weight"} is defined, the non-zero elements of the matrix
      will contain the corresponding attribute value.  For more flexible
      matrix conversion, see \code{toMatrix}.}
    \item{coerce}{\code{signature(from = "matrix", to = "graphAM")}:
      This coerce method exists for symmetry.  In most cases, creating a
      new \code{graphAM} instance using \code{new} gives one more
      control over the resulting graph.}
    \item{edges}{\code{signature(object = "graphAM")}: ... }
    \item{initialize}{\code{signature(.Object = "graphAM")}: ... }
    \item{inEdges}{\code{signature(node = "character", object =
        "graphNEL")}: Return the incoming edges for the specified
      nodes.  See \code{\link{inEdges}}.}
    \item{isAdjacent}{\code{signature(object = "graphAM", from = "character", to = "character")}: ... }
    \item{nodes<-}{\code{signature(object = "graphAM", value = "character")}: ... }
    \item{nodes}{\code{signature(object = "graphAM")}: ... }
    \item{numEdges}{\code{signature(graph = "graphAM")}: ... }
    \item{numNodes}{\code{signature(object = "graphAM")}: ... }
    \item{removeEdge}{\code{signature(from = "character", to = "character", graph = "graphAM")}: ... }
    \item{removeNode}{\code{signature(node = "character", object = "graphAM")}: ... }
  }
}
\author{Seth Falcon}

\seealso{
  \code{\link{graph-class}}, \code{\link{graphNEL-class}}
}
\examples{
mat <- rbind(c(0, 0, 1, 1),
             c(0, 0, 1, 1),
             c(1, 1, 0, 1),
             c(1, 1, 1, 0))
rownames(mat) <- colnames(mat) <- letters[1:4]
g1 <- graphAM(adjMat=mat)
stopifnot(identical(mat, as(g1, "matrix")), validObject(g1))

## now with weights:
mat[1,3] <- mat[3,1] <- 10
gw <- graphAM(adjMat=mat, values=list(weight=1))

## consistency check:
stopifnot(identical(mat, as(gw, "matrix")),
          validObject(gw),
          identical(gw, as(as(gw, "graphNEL"), "graphAM")))
}
\keyword{graphs}
\keyword{classes}
