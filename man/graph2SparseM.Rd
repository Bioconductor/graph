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
sparseM2Graph(sM, nodeNames, edgemode=c("directed", "undirected")) 

}
\arguments{
  \item{g}{An instance of the \code{graph} class. }
  \item{useweights}{A logical value indicating whether to use the edge
    weights in the graph as values in the sparse matrix.}
  \item{sM}{A sparse matrix.}
  \item{nodeNames}{A \code{character} vector of the node names.}
  \item{edgemode}{Specifies whether the graph to be created should have
    directed (default) or undirected edges.  If undirected, the input
    matrix \code{sM} must be symmetric.}
}
\details{
  A very simple coercion from one representation to another.

  Currently it is presumed that the matrix is square. For other graph
  formats, such as bipartite graphs, some improvements will be needed;
  patches are welcome.
  
}
\value{
  \code{graph2SparseM} takes as input an instance of a subclass of the
  \code{graph} class and returns a sparse matrix.

  \code{sparseM2Graph} takes a sparse matrix as input and returns an
  instance of the \code{graphNEL} class.  By default, the
  \code{graphNEL} returned will have directed edges.
}

\author{R. Gentleman }

\seealso{
  \code{\link{graph-class}},
  \code{\link{graphNEL-class}}, and for
  other conversions, \code{\link{aM2bpG}} and \code{\link{ftM2adjM}}
}

\examples{
  set.seed(123)
  g1 <- randomGraph(letters[1:10], 1:4, p=.3)
  s1 <- graph2SparseM(g1, useweights=TRUE)
  g2 <- sparseM2Graph(s1, letters[1:10], edgemode="undirected")
  ## consistency check
  stopifnot(all.equal(g1, g2))
}
\keyword{manip}
