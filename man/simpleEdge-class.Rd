\name{simpleEdge-class}
\docType{class}
\alias{simpleEdge-class}

\title{Class "simpleEdge".}
\description{A simple class for representing edges in graphs.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("simpleEdge", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{edgeType}:}{Object of class \code{"character"}; the type
    of the edge.}
    \item{\code{weight}:}{Object of class \code{"numeric"}; the edge weight.}
    \item{\code{directed}:}{Object of class \code{"logical"}; is the
      edge directed. }
    \item{\code{bNode}:}{Object of class \code{"character"}; the
      beginning node. }
    \item{\code{eNode}:}{Object of class \code{"character"}; the ending node. }
  }
}
\section{Methods}{
No methods defined with class "simpleEdge" in the signature.
}
\author{R. Gentleman}
\note{All slots are length one vectors (this is not currently checked
  for). If the edge is not directed there is no real meaning to the
  concepts of beginning node or ending node and these should not be
  interpreted as such. }

\seealso{
 See also\code{\link{gEdge-class}}}
\examples{
  new("simpleEdge", bNode="A", eNode="D")
}
\keyword{classes}
