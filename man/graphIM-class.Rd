\name{graphIM-class}
\docType{class}
\alias{graphIM-class}
\alias{edges,graphIM,missing-method}
\alias{edges,graphIM,character-method}


\title{Class "graphIM"}
\description{
  A graph class where node and edge information is represented as an
  incidence matrix.
  }
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("graphIM", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{inciMat}:}{An incidence \code{"matrix"} giving the graph
    structure.  The matrix must be square.  Dimnames should either be
    \code{NULL} or they should match.}
    \item{\code{edgemode}:}{A \code{"character"} vector either
    "directed" or "undirected".}
  }
}
\section{Extends}{
Class \code{"graph"}, directly.
}
\section{Methods}{
  \describe{
    \item{edges}{\code{signature(object = "graphIM", which = "missing")}: ... }
    \item{edges}{\code{signature(object = "graphIM", which = "character")}: ... }
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
    g1 <- new("graphIM", inciMat=mat)
}
\keyword{classes}
