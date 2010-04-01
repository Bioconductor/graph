\name{subGraph}
\alias{subGraph}
\alias{subGraph,character,graphNEL-method}
\alias{subGraph,character,clusterGraph-method}
\alias{subGraph,character,distGraph-method}
\alias{subGraph,character,MultiGraph-method}
\alias{subGraph,character,graphBAM-method}
\title{Create a Subgraph  }
\description{
  Given a set of nodes and a graph this function creates
  and returns subgraph with only the supplied nodes and any edges
  between them.
}
\usage{
subGraph(snodes, graph)
}
\arguments{
  \item{snodes}{A \code{character} vector of node labels. }
  \item{graph}{A graph object, it must inherit from the \code{graph} class. }
}
\details{
  The returned subgraph is a copy of the graph. Implementations for
  Implementations for \code{graphNEL}, \code{distGraph} and
  \code{clusterGraph}. 
}
\value{
  A graph of the same class as the \code{graph} argument but with only
  the supplied nodes.
}
\author{R. Gentleman }

\seealso{ \code{\link{nodes}},\code{\link{edges}} }

\examples{
        set.seed(123)
        x <- rnorm(26)
        names(x) <- letters
        library(stats)
        d1 <- dist(x)
        g1 <- new("distGraph", Dist=d1)
        subGraph(letters[1:5], g1)

}
\keyword{manip}
