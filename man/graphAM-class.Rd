\name{graphAM-class}
\docType{class}
\alias{graphAM-class}
\alias{addEdge,character,character,graphAM,missing-method}
\alias{addNodes,graphAM,character-method}
\alias{clearNode,character,graphAM-method}
\alias{edges,graphAM,missing-method}
\alias{edges,graphAM,character-method}
\alias{initialize,graphAM-method}
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
    graph structure.  The \code{colnames} of the matrix will be used as
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
    \item{addNodes}{\code{signature(object = "graphAM", nodes = "character")}: ... }
    \item{clearNode}{\code{signature(node = "character", object = "graphAM")}: ... }
    \item{edges}{\code{signature(object = "graphAM", which = "missing")}: ... }
    \item{edges}{\code{signature(object = "graphAM", which = "character")}: ... }
    \item{initialize}{\code{signature(.Object = "graphAM")}: ... }
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
    mat <- matrix(c(0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0),
                  byrow=TRUE, ncol=4)
    rownames(mat) <- letters[1:4]
    colnames(mat) <- letters[1:4]
    g1 <- new("graphAM", adjMat=mat)
}

\keyword{classes}
