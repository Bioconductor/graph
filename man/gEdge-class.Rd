\name{gEdge-class}
\docType{class}
\alias{gEdge-class}
\title{Class "gEdge" }
\description{A very generic class for describing the edges of a graph.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gEdge", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{edgeID}:}{Object of class \code{"integer"} a unique integer 
           identifier for the edge. }
    \item{\code{edgeType}:}{Object of class \code{"character"} the type of 
         edge. }
    \item{\code{weight}:}{Object of class \code{"numeric"} an edge weight. }
    \item{\code{bNode}:}{Object of class \code{"integer"} a beginning
      node for the edge. }
    \item{\code{eNode}:}{Object of class \code{"integer"} an ending node for
          the edge. }
  }
}

\section{Methods}{
No methods defined with class "gEdge" in the signature.
}
\author{R. Gentleman}
\note{Based on GXL's definition of edges.}
\seealso{ \code{\link{gNode-class}}}

\examples{
##---- Should be DIRECTLY executable !! ----
}
\keyword{classes}
