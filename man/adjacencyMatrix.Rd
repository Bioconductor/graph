\name{adjacencyMatrix}
\alias{adjacencyMatrix}
\alias{adjacencyMatrix,graphBAM-method}
\title{Compute an Adjacency Matrix for a graphBAM object  }
\description{
  Though unwieldy for large matrices, a full adjacency matrix
  can be useful for debugging and export.

  If the graph is \dQuote{undirected} then recicprocal edges
  are explicit in the matrix.

}
\usage{
adjacencyMatrix(object)
}
\arguments{
  \item{object}{A \code{graphBAM} object. }
}
\details{
  Thus far only implemented for \code{graphBAM} objects.
}
\value{
  \code{adjacencyMatrix} returns an n x n matrix, where n is
  the number of nodes in the graph, ordered in the same manner as
  seen in the \code{nodes} method.  All cells in the matrix are \code{0}
  except where edges are found.
 }

\author{P. Shannon}

\seealso{ \code{\link{edgeMatrix}} }

\examples{
  from   <- c("a", "a", "a", "x", "x", "c")
  to     <- c("b", "c", "x", "y", "c", "a")
  weight <- c(3.4, 2.6, 1.7, 5.3, 1.6, 7.9)
  df <- data.frame(from, to, weight)
  g1 <- graphBAM(df, edgemode = "directed")
  adjacencyMatrix(g1)
}
\keyword{manip }
