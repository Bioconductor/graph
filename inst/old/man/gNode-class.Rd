\name{gNode-class}
\docType{class}
\alias{gNode-class}
\alias{edgeOrder}
\alias{edgeOrder,gNode-method}
\alias{fromEdges}
\alias{fromEdges,gNode-method}
\alias{fromEdges<-}
\alias{fromEdges<-,gNode-method}
\alias{toEdges}
\alias{toEdges,gNode-method}
\alias{toEdges<-}
\alias{toEdges<-,gNode-method}
\alias{label}
\alias{label,gNode-method}
\alias{nodeID}
\alias{nodeID,gNode-method}
\alias{nodeType}
\alias{nodeType,gNode-method}
\alias{initialize,gNode-method}
\alias{idstring,gNode-method}
\title{Class "gNode"}
\description{This is an implemention of a very general graph node class.
It is patterned after the descriptions in GXL. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gNode", ...)}.
These are nodes of a graph and can have any number of attributes.
}
\section{Slots}{
  \describe{
    \item{\code{label}:}{Object of class \code{"character"} A label of
      the node. Should be unique within the graph. }
    \item{\code{fromEdges}:}{Object of class \code{"list"} A vector
      of the edge identifiers corresponding to the edges starting at
      that node. }
    \item{\code{toEdges}:}{Object of class \code{"list"} A vector of
      the edge identifiers corresponding to the edges pointing to that node. }
    \item{\code{edgeOrder}:}{Object of class \code{"list"} A vector
      of edge identifiers indicating the order in which the edges in the slot
      \code{"fromEdges"} would be traversed (may also be used for
      rendering). If this is of length zero, then the \code{"fromEdges"}
      slot is used for the order.}
    \item{\code{nodeType}:}{Object of class \code{"character"} the type
      of node. Could be a factor. }
    \item{\code{nodeID}:}{Object of class \code{"Ruuid"} a unique
      identifier.} 
    \item{\code{property}:}{Object of class \code{"list"}, holds
      properties associated with the graph node.}
  }
}
\section{Extends}{
Class \code{"propertyHolder"}, directly.
}
\section{Methods}{
  \describe{
    \item{edgeOrder}{\code{signature(object = "gNode")}: A method that
      returns the \code{edgeOrder} for the named node.}
    \item{fromEdges}{\code{signature(object = "gNode")}: A method that
      returns the \code{fromEdges}. }
    \item{fromEdges<-}{\code{signature(object = "gNode")}: A method that
    assigns the \code{fromEdges}}
    \item{label}{\code{signature(object = "gNode")}:A method that
      returns the node label. }
    \item{nodeID}{\code{signature(object = "gNode")}: A method that
      returns the node identifier. }
    \item{nodeType}{\code{signature(object = "gNode")}: A method that
      returns the node type. }
    \item{toEdges}{\code{signature(object = "gNode")}: A method that
      returns the \code{toEdges}. }
    \item{toEdges<-}{\code{signature(object = "gNode")}: A method that
      assigns the \code{toEdges}. }
    \item{idstring}{\code{signature(x = "gNode")}: A method that returns
      the node identifier as a character string.}
    \item{initialize}{\code{signature(.Object = "gNode", \ldots)}:
      initializer for \code{"gNode"} class.
    }
  }
}
\author{R. Gentleman and Saikat DebRoy}

\seealso{\code{\link{distGraph-class}},
 \code{\link{graphNEL-class}}}

\examples{
node <- new("gNode", label="A")
label(node)
idstring(node)
}
\keyword{classes}
