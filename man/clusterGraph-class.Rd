\name{clusterGraph-class}
\docType{class}
\alias{clusterGraph-class}
\alias{clusterGraph-class}
\alias{adj,clusterGraph-method}
\alias{connComp,clusterGraph-method}
\alias{edges,clusterGraph,missing-method}
\alias{edges,clusterGraph,character-method}
\alias{edgeWeights,clusterGraph-method}
\alias{nodes,clusterGraph-method}
\alias{numNodes,clusterGraph-method}
\alias{show,clusterGraph-method}
\title{Class "clusterGraph" }
\description{  A cluster graph is a special sort of graph for clustered
  data. Each cluster forms a completely connected subgraph. Three are no
  edges between clusters.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("clusterGraph", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{clusters}:}{Object of class \code{"list"} a list of the
      labels of the elements, one element of the list for each cluster. }
  }
}
\section{Extends}{
Class \code{"graph"}, directly.
}
\section{Methods}{
  \describe{
    \item{connComp}{\code{signature(object = "clusterGraph")}: find the
      connected components; simply the clusters in this case. }
    \item{acc}{\code{signature(object = "clusterGraph")}: find the
      accessible nodes from the supplied node. }
    \item{adj}{\code{signature(object = "clusterGraph")}: find the
      adjacent nodes to the supplied node. }
    \item{nodes}{\code{signature(object = "clusterGraph")}: return the
      nodes. }
    \item{numNodes}{\code{signature(object = "clusterGraph")}: return
      the number of nodes. }
  }
}
\author{R. Gentleman}

\seealso{ \code{\link{graph-class}}, \code{\link{distGraph-class}} }

\examples{
  cG1 <- new("clusterGraph", clusters=list(a=c(1,2,3), b=c(4,5,6)))
  cG1
  acc(cG1, c("1", "2"))
}
\keyword{classes}
