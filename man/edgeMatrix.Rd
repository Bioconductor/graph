\name{edgeMatrix}
\alias{edgeMatrix}
\alias{edgeMatrix,graphNEL-method}
\alias{edgeMatrix,clusterGraph-method}
\title{Compute an Edge Matrix for a Graph  }
\description{
 For our purposes an \emph{edge matrix} is a matrix with two rows and as
 many columns as there are edges. The entries in the first row are the
 index of the node the edge is \emph{from}, those in the second row indicate
 the node the edge is \emph{to}.

 If the graph is \dQuote{undirected} then the \code{duplicates} option
 can be used to indicate whether reciprocal edges are wanted. The
 default is to leave them out. In this case the notions of \emph{from}
 and \emph{to} are not relevant.
}
\usage{
edgeMatrix(object, duplicates=FALSE)
}
\arguments{
  \item{object}{An object that inherits from \code{graph}. }
  \item{duplicates}{Whether or not duplicate edges should be produced
    for \dQuote{undirected} graphs. }
}
\details{
  Implementations for both \code{graphNEL} and \code{clusterGraph} are
  available. 
}
\value{
  A matrix with two rows, \emph{from} and \emph{to}, and as many columns
  as there are edges. Entries indicate the index in the node vector that
  corresponds to the appropriate end of the edge.
 }

\author{R. Gentleman }

\seealso{ \code{\link{edges}} }

\examples{
  set.seed(123)
  g1 <- randomGraph(letters[1:10], 1:4, p=.3)
  edgeMatrix(g1)
  g2 <- new("clusterGraph", clusters=list(a=c(1,2,3), b=c(4,5,6)))
  edgeMatrix(g2)
}
\keyword{manip }
