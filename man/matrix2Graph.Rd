\name{Matrics to Graph coersions}
\alias{aM2bpG}
\alias{ftM2adjM}
\alias{coerce,matrix,graphNEL-method}

\title{Coercing matrices to graphs}
\description{
A collection of functions and methods to convert various forms of
matrices into graph objects.
}
\usage{
aM2bpG(aM)
ftM2adjM(ft, W=NULL, V=NULL, edgemode="directed")
}
\arguments{
  \item{ft}{A matrix with two columns containing the from/to
    representation of graph edges.}
  \item{W}{An optional vector of edge weights.}
  \item{V}{An optional vector of node names.}
  \item{aM}{An affiliation matrix for a bipartite graph.}
  \item{edgemode}{Specifies if the resulting graph is to be directed or undirected}
}
\details{

In the fuction \code{ftM2adjM}, a \code{from/to} matrix is converted
into an \code{adjacency} matrix (which can then be coerced directly into
a \code{\link{graphNEL}} with \code{as}.  Ths first column of the
\code{ft} represents the \code{from} nodes and the second column
represents \code{to} nodes.  This representation does not allow for
unconnected nodes except with the \code{V} argument (see below).  The
\code{edgemode} parameter can be used to specify if the desired output
is a directed or undirected graph. 
  
Also in \code{ftM2adjM}, \code{W} is an optional vector of edge weights.
The order of the edge weights in the vector should correspond to the
order of the edges recorded in \code{L}.  If it is not specified, edge
weights of 1 are assigned by default.  The \code{V} argument is an
optional vector of node names.  All nodes in \code{ft} must be contained
in \code{V}, but not all nodes in \code{V} must be contained in
\code{ft}.  If \code{V} is not specified, it is set to all nodes
represented in \code{ft} or \code{M}.  Specifying \code{V} is most useful
for creating a graph that includes nodes with degree 0. 

\code{aM} is an affiliation matrix as frequently used in social networks
analysis.  The rows of \code{aM} represent actors, and the columns
represent events.  An entry of "1" in the ith row and jth column
represents affiliation of the ith actor with the jth event.  Weighted
entries may also be used.  \code{aM2bpG} returns a \code{graphNEL} object with
nodes consisting of the set of actors and events, and directed (possibly
weighted) edges from the actors to their corresponding events.  If
plotted using \code{Rgraphviz} and the \code{dot} layout, the bipartite structure of
the graph returned by \code{aM2bpG} should be evident. 

An \code{adjacency} matrix can be directly coerced into a
\code{graphNEL} using the \code{as} method.  If the matrix is a
symmetric matrix, then the resulting graph will be \code{undirected},
otherwise it will be \code{directed}.

}
\value{
  For \code{aM2bpG}, an object of class \code{graphNEL}.  For
  \code{ftM2adjM}, an adjacency matrix representation of the coerced graph.
}

\author{Denise Scholtens}

\examples{
From <- c("A","A","C","C")
To <- c("B","C","A","D")
L <- cbind(From,To)

W <- 1:4
M1 <- ftM2adjM(L,W, edgemode="undirected")
M2 <- ftM2adjM(L)

G1 <- as(M1, "graphNEL")
G2 <- as(M2, "graphNEL")
}

\keyword{graphs }
