\name{boundary}
\alias{boundary}

\title{ Returns the Boundary between a Graph and a SubGraph }
\description{
 The boundary of a subgraph is the set of nodes in the original graph
 that have edges to nodes in the subgraph. The function \code{boundary}
 computes the boundary and returns it as a list whose length is the same
 length as the number of nodes in the subgraph.
}
\usage{
boundary(subgraph, graph)
}

\arguments{
  \item{graph}{ the original graph from which the boundary will 
                    be created }
  \item{subgraph}{ can either be the vector of the node labels or the
    subgraph itself.}
}
\details{
  The \emph{boundary} of a \emph{subgraph} is the set of nodes in the
  graph which have an edge that connects them to the specified subgraph
  but which are themselves not elements of the subgraph.

  For convenience users can specify the subgraph as either a graph or a
  vector of node labels.
}
\value{
  This function returns a named list of length equal to the number of
  nodes in \code{subgraph}. The elements of the list
  correspond to the nodes in the \code{subgraph}. The elements are lists
  of the nodes in \code{graph} which share an edge with the
  respective node in \code{subgraph}.

}

\author{ Elizabeth Whalen and R. Gentleman }

\seealso{ \code{\link{subGraph}}, \code{\link{graph-class}}
 }

\examples{
  set.seed(123)
  g1 <- randomGraph(letters[1:10], 1:4, p=.3)
  ##both should be "a"
  boundary(c("g", "i"), g1)
}
\keyword{ manip }

