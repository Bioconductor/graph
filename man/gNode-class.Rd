\name{gNode-class}
\docType{class}
\alias{gNode-class}
\alias{edgeOrder}
\alias{fromEdges}
\alias{toEdges}
\alias{fromEdges<-}
\alias{label}
\alias{nodeID}
\alias{nodeType}
\alias{toEdges<-}
\alias{edgeOrder,gNode-method}
\alias{fromEdges,gNode-method}
\alias{toEdges,gNode-method}
\alias{fromEdges<-,gNode-method}
\alias{label,gNode-method}
\alias{nodeID,gNode-method}
\alias{nodeType,gNode-method}
\alias{toEdges<-,gNode-method}
\title{Class "gNode"}
\description{This is an implemention of a very general graph node class.
It is patterned after the descriptions in GXL. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gNode", ...)}.
These are nodes of a graph and can have any number of attributes.
}
\section{Slots}{
  \describe{
    \item{\code{nodeID}:}{Object of class \code{"integer"} a unique
      integer identifier.} 
    \item{\code{nodeType}:}{Object of class \code{"character"} the type
      of node. Could be a factor. }
    \item{\code{toEdges}:}{Object of class \code{"integer"} A vector of
      the edge identifiers corresponding to the edges pointing to that node. }
    \item{\code{fromEdges}:}{Object of class \code{"integer"} A vector
      of the edge identifiers corresponding to the edges starting at
      that node. }
    \item{\code{edgeOrder}:}{Object of class \code{"integer"} A vector
      of edge identifiers indicating the order in which they would be
      traversed (may also be used for rendering). }
    \item{\code{label}:}{Object of class \code{"character"} A label of
      the node. Should be unique. }
  }
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
  }
}
\author{R. Gentleman }

\seealso{\code{\link{distGraph-class}},
 \code{\link{graphNEL-class}}}

\examples{
  ##none yet
}
\keyword{classes}
