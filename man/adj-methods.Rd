\name{adj-methods}
\docType{methods}
\alias{adj}
\title{ Methods for finding the adjacency list for selected nodes. } 
\alias{adj-methods}
\description{
 This generic function takes an object that inherits from the \code{graph}
class and a node in that graph and returns a vector containing information
about all other nodes that are adjacent to the given node.
This means that they are joined to the given node by an edge.
The accessibility list, \code{\link{acc}} is the list of all nodes that can
be reached from a specified node.
}
\value{
  The methods return vector of nodes that are adjacent to the 
 specified node.
}

\section{Methods}{\describe{
\item{object = graph}{An object that inherits from glass \code{graph}}
\item{index}{An index (could be multiple) }
}}
\seealso{\code{\link{acc-methods}}}
\keyword{methods}
