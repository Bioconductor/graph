\name{distGraph-class}
\docType{class}
\alias{distGraph-class}
\alias{threshold}
\alias{Dist}
\alias{[.dist}
\alias{distGraph-class}
\alias{adj,distGraph,ANY-method}
\alias{Dist,distGraph-method}
\alias{edges,distGraph,missing-method}
\alias{edges,distGraph,character-method}
\alias{edgeL,distGraph-method}
\alias{nodes,distGraph-method}
\alias{numNodes,distGraph-method}
\alias{show,distGraph-method}
\alias{threshold,distGraph-method}
\alias{edgeWeights,distGraph-method}
\alias{edgeWeights,distGraph,ANY-method}
\alias{initialize,distGraph-method}

\title{Class "distGraph"}
\description{ A class definition for graphs that are based on distances.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("distGraph", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{Dist}:}{Object of class \code{"dist"} that forms the
    basis for the edge weights used in the \code{distGraph}.}
  }
}
\section{Extends}{
Class \code{"graph"}, directly.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "distGraph")}: a print method}
    \item{Dist}{\code{signature(object = "distGraph")}: return the dist
      object. }
    \item{adj}{\code{signature(object = "distGraph")}: find the nodes
      adjacent to the supplied node. }
    \item{nodes}{\code{signature(object = "distGraph")}: return the
      nodes in the graph. }
    \item{numNodes}{\code{signature(object = "distGraph")}: return the
      number of nodes. }
    \item{threshold}{\code{signature(object = "distGraph", k, value)}: set all
      distances that are larger than the supplied threshold, \code{k}, to the 
      supplied value. The default is value is zero (and so is appropriate for
      similarities, rather than distances). }
    \item{initialize}{\code{signature(object = "distGraph")}:
        initialize a \code{distGraph} instance. }

    \item{edgeWeights}{Return a list of edge weights in a list format
      similar to the \code{edges} method.}

    \item{edgeL}{\code{signature(graph = "distGraph")}: A method for
      obtaining the edge list.}
  }
}
\references{Shamir's paper and Butte et al}
\author{R. Gentleman }

\seealso{\code{\link{graph-class}}, \code{\link{clusterGraph-class}}}

\examples{
   set.seed(123)
   x <- rnorm(26)
   names(x) <- letters
   library(stats)
   d1 <- dist(x)
   g1 <- new("distGraph", Dist=d1)
}
\keyword{classes}
