\name{Coercions between matrix and graph representations}
\alias{aM2bpG}
\alias{ftM2adjM}
\alias{ftM2graphNEL}

\alias{coerce,matrix,graphNEL-method}
\alias{coerce,graphNEL,matrix-method}

\title{Coercions between matrix and graph representations}
\description{
A collection of functions and methods to convert various forms of
matrices into graph objects.
}
\usage{
aM2bpG(aM)
ftM2adjM(ft, W=NULL, V=NULL, edgemode="directed")
ftM2graphNEL(ft, W=NULL, V=NULL, edgemode="directed")
\S4method{coerce}{graphNEL,matrix}(g,m)
\S4method{coerce}{matrix,graphNEL}(m,g)
}
\arguments{
  \item{ft}{A 2xn or nx2 matrix containing the \code{from/to}
    representation of graph edges.}
  \item{W}{An optional vector of edge weights.}
  \item{V}{An optional vector of node names.}
  \item{aM}{An affiliation matrix for a bipartite graph.}
  \item{edgemode}{Character. Specifies if the resulting graph is
    to be directed or undirected.}
  \item{g}{Object of class graphNEL.}
  \item{m}{Matrix.}
}
\details{

In the functions \code{ftM2adjM} and \code{ftM2graphNEL}, a
\code{from/to} matrix \code{ft} is converted into an \code{adjacency}
matrix or a \code{graphNEL} object respectively.  If \code{ft} is a
\code{2xn} matrix, then the first row represents the \code{from} nodes
and the second row the \code{to} nodes.  If \code{ft} is an \code{nx2}
matrix, then the first column represents the \code{from} nodes and the
second column the \code{to} nodes.  If it is 2x2, then the
\code{from/to} nodes are taken to be represenred by the columns.

This representation does not allow for unconnected nodes except with the
\code{V} argument (see below).
The \code{edgemode} parameter can be used to specify if the desired output
is a directed or undirected graph. 

The same edge must not occur twice in the \code{from/to} matrix.
If \code{edgemode} is \code{undirected}, the edge \code{(u,v)} and
\code{(v,u)} must only be specified once.

\code{W} is an optional vector of edge weights.  The order of the edge
weights in the vector should correspond to the order of the edges
recorded in \code{ft}.  If it is not specified, edge weights of 1 are
assigned by default.

\code{V} is an optional vector of node names.  All elements of \code{ft}
must be contained in \code{V}, but not all names in \code{V} need to be
contained in \code{ft}.  If \code{V} is not specified, it is set to all
nodes represented in \code{ft}.  Specifying \code{V} is most useful for
creating a graph that includes nodes with degree 0.

\code{aM} is an affiliation matrix as frequently used in social networks
analysis.  The rows of \code{aM} represent actors, and the columns
represent events.  An entry of "1" in the ith row and jth column
represents affiliation of the ith actor with the jth event.  Weighted
entries may also be used.  \code{aM2bpG} returns a \code{graphNEL} object with
nodes consisting of the set of actors and events, and directed (possibly
weighted) edges from the actors to their corresponding events.  If
plotted using \code{Rgraphviz} and the \code{dot} layout, the bipartite structure of
the graph returned by \code{aM2bpG} should be evident. 

An \code{adjacency} matrix can be coerced into a \code{graphNEL} using
the \code{as} method.  If the matrix is a symmetric matrix, then the
resulting graph will be \code{undirected}, otherwise it will be
\code{directed}.
}

\value{
For \code{ftM2graphNEL} and \code{aM2bpG}, an object of class
\code{graphNEL}.
For \code{ftM2adjM}, a matrix (the adjacency matrix representation).
}

\author{Denise Scholtens, Wolfgang Huber}

\examples{

## From-To matrix

From <- c("A","A","C","C")
To   <- c("B","C","B","D")
L <- cbind(From,To)

W  <- 1:4
M1 <- ftM2adjM(L, W, edgemode="directed")
M2 <- ftM2adjM(L, W, edgemode="undirected")
stopifnot(all(M1+t(M1)==M2))

G1 <- ftM2graphNEL(L, W, edgemode="directed")
G2 <- ftM2graphNEL(L, W, edgemode="undirected")

## Adjacency matrix

From <- matrix(runif(100), nrow=10, ncol=10)
From <- (From+t(From)) > pi/4
rownames(From) <- colnames(From) <- LETTERS[1:10]

To <- as(From,"graphNEL")
Back <- as(To,"matrix")

stopifnot(all(From == Back))


}

\keyword{graphs }
