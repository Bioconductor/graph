\name{leaves}
\alias{leaves}
\alias{leaves,graph-method}

\title{Find the leaves of a graph}
\description{
  A leaf of an undirected graph is a node with degree equal to one.  A
  leaf of a directed graph is defined with respect to in-degree or
  out-degree.  The leaves of a directed graph with respect to in-degree
  (out-degree) are those nodes with in-degree (out-degree) equal to
  zero.
}
\usage{
leaves(object, degree.dir)
}

\arguments{
  \item{object}{A \code{graph} object}
  
  \item{degree.dir}{One of \code{"in"} or \code{"out"}.  This argument
    is ignored when \code{object} is undirected and required otherwise.
    When \code{degree.dir="in"} (\code{degree.dir="out"}), nodes have no
    in coming (out going) edges will be returned. }
}

\value{
  A character vector giving the node labels of the leaves.
}
\author{Seth Falcon}
\examples{
data(graphExamples)
graphExamples[[1]]
leaves(graphExamples[[1]])

data(apopGraph)
leaves(apopGraph, "in")
leaves(apopGraph, "out")

}


