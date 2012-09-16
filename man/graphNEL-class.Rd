\name{graphNEL-class}
\docType{class}
\alias{graphNEL-class}
\alias{graphNEL}
\alias{coerce}
\alias{edgeL}
\alias{edges}
\alias{edges}
\alias{initialize}
\alias{nodes<-}
\alias{nodes}
\alias{addEdge,character,character,graphNEL,numeric-method}
\alias{addEdge,character,character,graphNEL,missing-method}
\alias{addNode,character,graphNEL-method}
\alias{adj,graphNEL,ANY-method}
\alias{clearNode,character,graphNEL-method}
\alias{coerce,graphNEL,generalGraph-method}
\alias{coerce,graphNEL,graphAM-method}
\alias{coerce,graphNEL,graphBAM-method}
\alias{edgeL,graphNEL-method}
\alias{edges,graphNEL,missing-method}
\alias{edges,graphNEL,character-method}
\alias{edgeWeights,graphNEL-method}
\alias{inEdges,graphNEL,missing-method}
\alias{inEdges,missing,graphNEL-method}
\alias{inEdges,character,graphNEL-method}
\alias{initialize,graphNEL-method}
\alias{nodes<-,graphNEL,character-method}
\alias{nodes,graphNEL-method}
\alias{numNodes,graphNEL-method}
\alias{removeEdge,character,character,graphNEL-method}
\alias{removeNode,character,graphNEL-method}
\alias{toGXL,graphNEL-method}
\title{Class "graphNEL"}

\description{
  This is a class of graphs that are represented in terms of nodes and
  an edge list. This is a suitable representation for a graph with a
  large number of nodes and relatively few edges.
}

\section{Slots}{
  \describe{
    \item{\code{nodes}:}{Object of class \code{"vector"}.}

    \item{\code{edgeL}:}{Object of class \code{"list"}. The \code{edgeL}
      must be the same length as \code{nodes}. The elements of this
      vector correspond to the same element in \code{nodes}. The
      elements are themselves lists. If the node has any edges then this
      list will have an element named \code{edges}.  This will
      eventually change.  Since edge weights are now stored in the
      edge attributes construct, we do not need the extra level of
      list.
    }
  }
}

\section{Extends}{
Class \code{"graph"}, directly.
}

\section{Constructor}{
  \code{graphNEL(nodes=character(), edgeL=list(), edgemode='undirected')}
  creates a graphNEL instance.

  \describe{
  \item{nodes}{A character vector of node labels.}
  \item{edgeL}{A named list either in the format returned by the
    \code{edges} method or a list of lists where each inner list has
    an element named \code{edges} and optionally an element named
    \code{weights}.  If \code{weights} is present, it must be the same
    length as the \code{edges} element.}
  \item{edgemode}{Either "directed" or "undirected".}

  }
}

\section{Methods}{
  \describe{
    \item{adj}{\code{signature(object = "graphNEL")}: A method for
      finding nodes adjacent to the suplied node.}
    \item{edgeL}{\code{signature(graph = "graphNEL")}: A method for
      obtaining the edge list.}
    \item{edgeWeights}{\code{signature(object = "graphNEL")}: A method
      for obtaining the edge weights. }
    \item{edges}{\code{signature(object = "graphNEL")}: A method for
      obtaining the edges.}
    \item{inEdges}{\code{signature(node = "character", object =
        "graphNEL")}: Return the incoming edges for the specified
        nodes.  See \code{\link{inEdges}}.}
    \item{nodes}{\code{signature(object = "graphNEL")}: A method for
      obtaining the nodes. }
    \item{numNodes}{\code{signature(object = "graphNEL")}:A method for
      determining how many nodes are in the graph. }
    \item{subGraph}{\code{signature(snodes="character", graph =
	"graphNEL")}:A method for
      obtaining the induced subgraph based on the set of supplied nodes
      and the supplied graph.}
    \item{plot}{Please see the help page for \code{plot.graphNEL} in the
      \code{Rgraphviz} package}
    \item{graph2graphviz}{\code{signature(object = "graphNEL")}: A method
      that will convert a \code{graphNEL} object into a matrix suitable
      for interaction with \code{Rgraphviz}.  Not intended to be called
      directly.  This function will insure that no NA's (or other
      undesired values) are in the graph, or created by coersion.}
    \item{nodes<-}{\code{signature(object="graphNEL",
	value="character")}: A method for replacing the nodes in a graph
      object. It checks to be sure the values are the right length and
      unique. }
    \item{coerce}{\code{signature(from = "graphNEL", to = "graphAM")}:
      Called via \code{as}, the method converts to an adjacency matrix
      representation.  See \code{\link{graphAM-class}}. }
    \item{coerce}{\code{signature(from = "graphNEL", to = "graphBAM")}:
      Called via \code{as}, the method converts to an bit array
      representation.  See \code{\link{graphBAM-class}}. }
  }
}

\details{
   The \code{graphNEL} class provides a very general structure for
   representing graphs. It will be reasonably efficient for lists with
   relatively more nodes than edges.  Although this representation can
   support multi-edges, such support is not implemented and instances
   of \code{graphNEL} are assumed to be simple graphs with at most one
   edge between any pair of nodes.

   The \code{edgeL} is a named \code{list} of the same length as the
   node vector. The names are the names of the nodes. Each element of
   \code{edgeL} is itself a list. Each element of this (sub)list is a
   vector (all must be the same length) and each element represents an
   edge to another node. The sublist named \code{edges} holds index
   values into the node vector. And each such entry represents an edge
   from the node which has the same name as the component of
   \code{edgeL} to the node with index provided. Another component that
   is often used is named \code{weights}. It represents edge weights.
   The user can specify any other edge attributes (such as types
   etc). They are responsible for any special handling that
   these might require.

   For an \code{undirected} instance all edges are reciprocated (there
   is an edge from A to B and from B to A).

   Note that the reason for using indices to represent the \code{to} end
   of a node is so that we can easily support permutation of the node
   labels as a way to generate randomizations of the graph.
 }
\author{R. Gentleman}

\seealso{\code{\link{graphAM-class}}, \code{\link{distGraph-class}},
  \code{\link{clusterGraph-class}}
}

\examples{
   set.seed(123)
   V <- LETTERS[1:4]
   edL <- vector("list", length=4)
   names(edL) <- V
   for(i in 1:4)
      edL[[i]] <- list(edges=5-i, weights=runif(1))
   gR <- graphNEL(nodes=V, edgeL=edL)
   edges(gR)
   edgeWeights(gR)
}
\keyword{classes}
