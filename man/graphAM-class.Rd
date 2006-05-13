\name{graphAM-class}
\docType{class}
\alias{graphAM-class}
\alias{addEdge,character,character,graphAM,missing-method}
\alias{addNode,character,graphAM,missing-method}
\alias{clearNode,character,graphAM-method}
\alias{coerce,graphAM,graphNEL-method}
\alias{coerce,graphAM,matrix-method}
\alias{coerce,matrix,graphAM-method}
\alias{edges,graphAM,missing-method}
\alias{edges,graphAM,character-method}
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
  \code{adjMat[i, j]} is non-zero if there is an edge from node i to
  node j.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("graphAM", adjMat, edgemode, values)}.
}
\section{Slots}{
  \describe{
    \item{\code{adjMat}:}{An adjacency \code{"matrix"} describing the
    graph structure.  The \code{\link{colnames}} of the matrix will be used as
    node names for the graph if present.}
    \item{\code{edgemode}:}{A \code{"character"} vector specifying
      whether the graph is "directed" or "undirected".}
    \item{\code{edgeData}:}{Storage for edge attributes.}
    \item{\code{nodeData}:}{Storage for node attributes.}
  }
}
\section{Extends}{
Class \code{"graph"}, directly.
}
\section{Methods}{
  \describe{
    \item{addEdge}{\code{signature(from = "character", to = "character", graph = "graphAM", weights = "missing")}: ... }
    \item{addNode}{\code{signature(object = "graphAM", nodes = "character")}: ... }
    \item{clearNode}{\code{signature(node = "character", object = "graphAM")}: ... }
    \item{coerce}{\code{signature(from = "graphAM", to = "graphNEL")}: ... }
    \item{coerce}{\code{signature(from = "graphAM", to = "matrix")}: ... }
    \item{coerce}{\code{signature(from = "matrix", to = "graphAM")}: ... }
    \item{edges}{\code{signature(object = "graphAM", which = "missing")}: ... }
    \item{edges}{\code{signature(object = "graphAM", which = "character")}: ... }
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
g1 <- new("graphAM", adjMat=mat)
stopifnot(identical(mat, as(g1, "matrix")), validObject(g1))

## now with weights:
mat[1,3] <- mat[3,1] <- 10
gw <- new("graphAM", adjMat=mat)

## consistency check :
stopifnot(identical(mat, as(gw, "matrix")), validObject(gw),
## FIXME  identical(gw, as(as(gw, "graphNEL"), "graphAM")),
          TRUE)
}
\keyword{graphs}
\keyword{classes}
