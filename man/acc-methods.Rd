\name{acc-methods}
\docType{methods}
\alias{acc-methods}
\alias{acc,graph-method}
\alias{acc,clusterGraph-method}
\alias{acc}
\alias{acc,graph,character-method}
\alias{acc,clusterGraph,character-method}
\title{Methods for Accessibility Lists}

\description{
 This generic function takes an object that inherits from the \code{graph} 
class and a node in that graph and returns a vector containing information
about all other nodes that are accessible from the given node. The
methods are vectorized so that \code{index} can be a vector.
}
\usage{
\S4method{acc}{graph,character}(object, index)
\S4method{acc}{clusterGraph,character}(object, index)

}
\arguments{
  \item{object}{An instance of the appropriate graph class.}
  \item{index}{A character vector specifying the nodes for which
    accessibilty information is wanted.}
}

\value{
  The methods should return a named list of integer vectors. The
  \code{names} of the list correspond to the names of the supplied
  nodes. For each element of the list the returned vector is named. The
  names of the vector elements correspond to the nodes that are
  accessible from the given node. The values in the vector indicate how
  many edges are between the given node and the node in the return vector.
}
\section{Methods}{\describe{
    \item{object = graph}{An object of class graph.}
    \item{object = clusterGraph}{An instance of the \code{clusterGraph}
      class.} 
\item{index}{A \code{character} vector of indices corresponding to nodes in the
  graph.}  }
}

\examples{
 set.seed(123)
 gR3 <- randomGraph(LETTERS[1:10], M<-1:2, p=.5)
 acc(gR3, "A")
 acc(gR3, c("B", "D"))
}

\keyword{methods}
