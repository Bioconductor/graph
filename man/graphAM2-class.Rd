\name{graphAM2-class}
\Rdversion{1.1}
\docType{class}
\alias{graphAM2-class}
\alias{addEdge,character,character,graphAM2,missing-method}
\alias{addNode,character,graphAM2,missing-method}
\alias{clearNode,character,graphAM2-method}
\alias{edges,graphAM2,missing-method}
\alias{edges,graphAM2,character-method}
\alias{inEdges,character,graphAM2-method}
\alias{initialize,graphAM2-method}
\alias{isAdjacent,graphAM2,character,character-method}
\alias{nodes,graphAM2-method}
\alias{numEdges,graphAM2-method}
\alias{numNodes,graphAM2-method}
\alias{removeEdge,character,character,graphAM2-method}
\alias{removeNode,character,graphAM2-method}
\alias{GraphAM2}

\title{EXPERIMENTAL class "graphAM2"}
\description{
  An adjacency matrix representation backed by a bit array.
  The bit array is stored as a raw vector.  This is EXPERIMENTAL and
  subject to change.
}

\usage{
GraphAM2(from, to, nodes=NULL, weights=NULL, edgemode="undirected")
}
  %     Create a \code{graphAM2} instance given an edge list specified as
  %     character vectors from and to.  The nodes argument can be used to
  %     add nodes with degree zero.  The weights argument should have the
  %     same length as from and to.
  %     }

\arguments{
  \item{from}{A character vector of node labels corresponding to source
    nodes of edges in the graph.  This must have the same length as
    \code{to}.}
  \item{to}{A character vector of node labels corresponding to
  destination nodes of the edges in the graph.}
  \item{nodes}{A character vector of node labels.  Use this to add
  degree zero nodes to the graph.  If \code{NULL}, the set of nodes
  found in \code{from} and \code{to} will be used.}
  \item{weights}{A vector of the same length as \code{from} and
  \code{to} providing an edge weight for the associated edge.}
  \item{edgemode}{A string, one of "directed" or "undirected".}
}
\section{Constructors}{
  \code{GraphAM2}
}
\section{Extends}{
Class \code{"\linkS4class{graph}"}, directly.
}
\section{Methods}{
  \describe{
    
    \item{addEdge}{\code{signature(from = "character", to = "character", graph = "graphAM2", weights = "missing")}: ... }
    \item{addNode}{\code{signature(node = "character", object = "graphAM2", edges = "missing")}: ... }
    \item{clearNode}{\code{signature(node = "character", object = "graphAM2")}: ... }
    \item{edges}{\code{signature(object = "graphAM2", which = "missing")}: ... }
    \item{edges}{\code{signature(object = "graphAM2", which = "character")}: ... }
    \item{inEdges}{\code{signature(node = "character", object = "graphAM2")}: ... }
    \item{initialize}{\code{signature(.Object = "graphAM2")}: ... }
    \item{isAdjacent}{\code{signature(object = "graphAM2", from = "character", to = "character")}: ... }
    \item{nodes}{\code{signature(object = "graphAM2")}: ... }
    \item{numEdges}{\code{signature(object = "graphAM2")}: ... }
    \item{numNodes}{\code{signature(object = "graphAM2")}: ... }
    \item{removeEdge}{\code{signature(from = "character", to = "character", graph = "graphAM2")}: ... }
    \item{removeNode}{\code{signature(node = "character", object = "graphAM2")}: ... }
	 }
}

\author{S. Falcon}




\examples{
    f <- c("a", "a", "b", "c", "d")
    t <- c("b", "c", "c", "d", "a")
    g <- GraphAM2(f, t)
    gd <- GraphAM2(f, t, edgemode = "directed")
    g
    gd
}
\keyword{classes}
