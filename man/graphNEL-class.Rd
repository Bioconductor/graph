\name{graphNEL-class}
\docType{class}
\alias{graphNEL-class}
\alias{graphNEL}
\alias{subGraph}
\alias{edgeL}
\alias{edgeWeights}
\alias{graph2graphviz}
\title{Class "graphNEL"}
\description{
  This is a class of graphs that are represented in terms of nodes and
  an edge list. This is a suitable representation for a graph with a
  large number of nodes and relatively few edges.}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("graphNEL",...)}.
}
\section{Slots}{
  \describe{
    \item{\code{nodes}:}{Object of class \code{"vector"}.}
    \item{\code{edgeL}:}{Object of class \code{"list"}. The \code{edgeL}
      must be the same length as \code{nodes}. The elements of this
      vector correspond to the same element in \code{nodes}. The
      elements are themselves lists. If the node has any edges then this
      list will have an element named \code{edges}. If edge weights are
      used then there must be an element named \code{weights} and it
      must be the same length as the \code{edges} element.
    }
  }
}
\section{Extends}{
Class \code{"graph"}, directly.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "graphNEL")}:A print method for
    the graph.}
    \item{adj}{\code{signature(object = "graphNEL")}: A method for
      finding nodes adjacent to the suplied node.}
    \item{edgeL}{\code{signature(graph = "graphNEL")}: A mehtod for
      obtaining the edge list.}
    \item{edgeWeights}{\code{signature(object = "graphNEL")}: A method
      for obtaining the edge weights. }
    \item{edges}{\code{signature(object = "graphNEL")}: A method for
      obtaining the edges.}
    \item{nodes}{\code{signature(object = "graphNEL")}: A method for
      obtaining the nodes. }
    \item{numNodes}{\code{signature(object = "graphNEL")}:A method for
      determining how many nodes are in the graph. }
    \item{subGraph}{\code{signature(subgraph="character", graph =
	"graphNEL")}:A method for
      obtaining the induced subgraph based on the set of supplied nodes
      and the supplied graph.}
    \item{plot}{\code{signature(x="graphNEL",y,...,nodeLabels,
	centerNode, nodeCols=par("bg"), textCols=par("fg"))}:A method for
      plotting a \code{graphNEL} object.  Requires the package
      \code{Rgraphviz} to be loaded.  Users can specify a layout engine with
      the \code{y} parameter, being "dot", "neato" or "twopi", with
      "dot" being the default.  Standard plotting parameters can be
      passed in via the \code{...} argument.  Labels for the nodes
      default to the names of the nodes, but can be customized using the
      \code{nodeLabels} parameter.  The \code{nodeCols} and
      \code{textCols} will specify colors to be used for the nodes and
      label text respectively - values can either be a single color or a
      vector of colors (which will be cycled).  A \code{centerNode} can
      be specified (primarily for the "twopi" layout method) using this
      argument.}
    \item{graph2graphviz}{\code{signature(object = "graphNEL")}: A method
      that will convert a \code{graphNEL} object into a matrix suitable
      for interaction with \code{Rgraphviz}.  Not intended to be called
      directly.  This function will insure that no NA's (or other
      undesired values) are in the graph, or created by coersion.}
    \item{nodes<-}{\code{signature(object="graphNEL",
	value="character")}: A method for replacing the nodes in a graph
      object. It checks to be sure the values are the right length and
      unique. }
  }
}


\author{R. Gentleman}

\seealso{\code{\link{distGraph-class}},
  \code{\link{clusterGraph-class}} } 

\examples{
   set.seed(123)
   V <- LETTERS[1:4]
   edL <- vector("list", length=4)
   names(edL) <- V
   toE <- LETTERS[4:1]
   for(i in 1:4) 
      edL[[i]] <- list(edges=5-i, weights=runif(1))
   gR <- new("graphNEL", nodes=V, edgeL=edL)
   edges(gR)
   edgeWeights(gR)

}
\keyword{classes}
