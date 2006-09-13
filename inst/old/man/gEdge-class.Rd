\name{gEdge-class}
\docType{class}
\alias{gEdge-class}
\alias{edgeID}
\alias{bNode}
\alias{eNode}
\alias{edgeID,gEdge-method}
\alias{bNode,gEdge-method}
\alias{eNode,gEdge-method}
\alias{idstring,gEdge-method}
\alias{initialize,gEdge-method}
\title{Class "gEdge" }
\description{A very generic class for describing the edges of a graph.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gEdge", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{edgeID}:}{Object of class \code{"Ruuid"} a unique
           identifier for the edge. }
    \item{\code{edgeType}:}{Object of class \code{"character"} the type of
         edge. }
    \item{\code{bNode}:}{Object of class \code{"Ruuid"} identifier of beginning
      node for the edge. }
    \item{\code{eNode}:}{Object of class \code{"integer"} identifier of
      ending node for the edge. }
    \item{\code{property}:}{Object of class \code{"list"}, holds
      properties associated with the edge. It always contains at least
      one element of type numeric named "weight"}
  }
}
\section{Extends}{
Class \code{"propertyHolder"}, directly.
}
\section{Methods}{
  \describe{
    \item{bNode}{\code{signature(object = "gEdge")}: A method that
      returns the identifier for the begining node of the edge.}
    \item{eNode}{\code{signature(object = "gEdge")}: A method that
      returns the identifier for the ending node of the edge.}
    \item{edgeID}{\code{signature(object = "gEdge")}: A method that
      returns the edge identifier. }
    \item{idstring}{\code{signature(x = "gEdge")}: A method that returns
      the edge identifier as a character string.}
    \item{initialize}{\code{signature(.Object = "gEdge", \ldots)}:
      initializer for \code{"gEdge"} class.
    }
  }
}
\author{R. Gentleman and Saikat DebRoy}
\note{Based on GXL's definition of edges.}
\seealso{ \code{\link{gNode-class}}}

\examples{
nodeA <- new("gNode", label="A")
nodeB <- new("gNode", label="A")
edgeAB <- new("gEdge", bNode=nodeA, eNode=nodeB)
idstring(edgeAB)
bNode(edgeAB) == nodeID(nodeA)
eNode(edgeAB) == nodeID(nodeB)
fromEdges(nodeA) <- fromEdges(nodeB) <- list(edgeAB)
toEdges(nodeA) <- toEdges(nodeB) <- list(edgeAB)
fromEdges(nodeA)
toEdges(nodeA)
fromEdges(nodeB)
toEdges(nodeB)
}
\keyword{classes}
