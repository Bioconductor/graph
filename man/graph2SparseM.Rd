\name{graph2SparseM}
\alias{graph2SparseM}
\alias{sparseM2Graph}
\title{Coercion methods between graphs and sparse matrices  }
\description{
  These functions provide coercions between objects that inherit from
  the \code{graph} class to sparse matrices from the \code{SparseM}
  package. 
}
\usage{
graph2SparseM(g, useweights=FALSE)
sparseM2Graph(sM, nodeNames) 

}
\arguments{
  \item{g}{An instance of the \code{graph} class. }
  \item{useweights}{A logical value indicating whether to use the edge
    weights in the graph as values in the sparse matrix.}
  \item{sM}{A sparse matrix.}
  \item{nodeNames}{A \code{character} vector of the node names.}
}
\details{
  A very simple coercion from one representation to another.

  Currently it is presumed that the matrix is square. For other graph
  formats, such as bipartite graphs, some improvements will be needed;
  patches are welcome.
  
}
\value{
  \code{graph2SparseM} takes as input an instance of the \code{graph}
  class and returns a sparse matrix while \code{sparseM2Graph} takes a
  sparse matrix as input and returns an in instance of the graph class.
}

\author{R. Gentleman }

\seealso{\code{\link{graph}}}

\examples{
  set.seed(123)
  g1 <- randomGraph(letters[1:10], 1:4, p=.3)
  s1 <- graph2SparseM(g1)

  sparseM2Graph(s1, letters[1:10])

}
\keyword{manip}
