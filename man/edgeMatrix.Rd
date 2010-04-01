\name{edgeMatrix}
\alias{edgeMatrix}
\alias{eWV}
\alias{pathWeights}
\alias{edgeMatrix,graphNEL-method}
\alias{edgeMatrix,graphAM-method}
\alias{edgeMatrix,clusterGraph-method}
\alias{edgeMatrix,distGraph-method}
\alias{edgeMatrix,graphBAM-method}
\title{Compute an Edge Matrix or weight vector for a Graph  }
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
eWV(g, eM, sep = ifelse(edgemode(g) == "directed", "->",
                 "--"), useNNames=FALSE)
pathWeights(g, p, eM=NULL)
}
\arguments{
  \item{object}{An object that inherits from \code{graph}. }
  \item{g}{An object that inherits from \code{graph}. }
  \item{duplicates}{Whether or not duplicate edges should be produced
    for \dQuote{undirected} graphs. }
  \item{eM}{An edge matrix}
  \item{sep}{a character string to concatenate node labels in
    the edge label}
  \item{useNNames}{a logical; if TRUE, node names are used in
    the edge label; if FALSE, node indices are used}
  \item{p}{a vector of node names constituting a path in graph \code{g}}
  \item{\dots}{arguments passed to \code{edgeMatrix}.}
}
\details{
  Implementations for \code{graphNEL}, \code{clusterGraph} and
  \code{distGraph} are available. 
}
\value{
  \code{edgeMatrix} returns a matrix with two rows, \emph{from} and \emph{to}, and as many columns
  as there are edges. Entries indicate the index in the node vector that
  corresponds to the appropriate end of the edge.

  \code{eWV} uses the edge matrix to create an annotated vector of 
  edge weights.

  \code{pathWeights} returns an annotated vector of edge weights
  for a specified path in a graph.
 }

\note{A path through an undirected graph may have several
representations as a named vector of edges.  Thus in the
example, when the weights for path b-a-i are requested,
the result is the pair of weights for edges a--b and a--i,
as these are the edge labels computed for graph g1.}

\author{R. Gentleman }

\seealso{ \code{\link{edges}} }

\examples{
  set.seed(123)
  g1 <- randomGraph(letters[1:10], 1:4, p=.3)
  edgeMatrix(g1)
  g2 <- new("clusterGraph", clusters=list(a=c(1,2,3), b=c(4,5,6)))
  em2 <- edgeMatrix(g2)
  eWV(g1, edgeMatrix(g1))
  eWV(g1, edgeMatrix(g1), useNNames=TRUE)
  pathWeights(g1, c("b", "a", "i"))
}
\keyword{manip }
