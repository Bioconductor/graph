\name{graphH-class}
\docType{class}
\alias{graphH-class}
\alias{getNodes}
\alias{idstring}
\alias{degree,graphH,missing-method}
\alias{degree,graphH,character-method}
\alias{edges,graphH,missing-method}
\alias{edges,graphH,character-method}
\alias{getNodes,graphH,missing-method}
\alias{getNodes,graphH,character-method}
\alias{idstring,graphH-method}
\alias{initialize,graphH-method}
\alias{nodes,graphH-method}
\alias{nodes<-,graphH,character-method}

\title{Class "graphH"}
\description{
  This is a class of graphs that are represented in terms of nodes, a
  list of in-edge identifiers and a list of out-edge identifiers for the
  nodes. and a hashtable for the edges. This is a suitable
  representation for a graph with a large number of nodes and relatively
  few edges.}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("graphH", graphID, nodes, edges, edgemode, ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{graphID}:}{Object of class \code{"Ruuid"}. This is an
      unique identifier for the graph. Modifying the graph results in a
      modification of the graph ID.}
    \item{\code{nodes}:}{Object of class \code{"list"} with each element
      an object of class \code{"gNode"}. The name of each element is its
      node ID.}
    \item{\code{label2nodeID}:}{Object of class \code{"character"}, this
    is used internally to map between the node labels and the node IDs.}
    \item{\code{edges}:}{Object of class \code{"environment"}, this is
      used as hashtable to store the edges (with edge IDs as keys).}
    \item{\code{edgemode}:}{Indicates whether edges are
      \code{"directed"} or \code{"undirected"}}
  }
}
\section{Extends}{
Class \code{"graph"}, directly.
}
\section{Methods}{
  \describe{
    \item{degree}{\code{signature(object = "graphH", Nodes = "missing")}:
      find the \code{degree} of the graph nodes (number of coincident edges).}
    \item{degree}{\code{signature(object = "graphH", Nodes = "character")}:
      find the \code{degree} of the specified graph nodes.}
    \item{edges}{\code{signature(object = "graphNEL", which="missing")}:
      A method for obtaining the out edges of the nodes as list, each
      element of the list representing the out edges for a particular
      node. Each such element is a character vector of end node labels
      of the out edges for the corresponding begin nodes.}
    \item{edges}{\code{signature(object = "graphNEL", which="character")}:
      A method for obtaining the out edges of the specified nodes.}
    \item{getNodes}{\code{signature(x = "graphH", which = "missing")}: A
      method for obtaining all the nodes of a graph as a list of
      \code{"gNode"} objects.}
    \item{getNodes}{\code{signature(x = "graphH", which = "character")}: A
      method for obtaining the specified nodes of a graph as a list of
      \code{"gNode"} objects.}
    \item{idstring}{\code{signature(x = "graphH")}: A method that returns
      the graph identifier as a character string.}
    \item{initialize}{\code{signature(.Object = "graphH")}: Initializer
      for \code{"graphH"} objects.}
    \item{nodes}{\code{signature(object = "graphH")}: A method for
      obtaining the node labels of a graph. }
    \item{nodes<-}{\code{signature(object="graphH",
	value="character")}: A method for replacing the node labels in a graph
      object. It checks to be sure the values are the right length and
      unique. }
  }
}
\author{Saikat DebRoy}
\seealso{\code{\link{graphNEL-class}}}
\examples{
v <- lapply(letters[1:5], function(l) new("gNode", label=l))
e <- vector("list", 2)
e[[1]] <- new("gEdge", bNode=v[[1]], eNode=v[[2]])
e[[2]] <- new("gEdge", bNode=v[[2]], eNode=v[[3]])
g <- new("graphH", nodes=v, edges=e)
nodes(g)
edges(g)
}
\keyword{classes}
