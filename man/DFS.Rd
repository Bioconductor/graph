\name{DFS}
\alias{DFS}
\alias{DFS,graph,character-method}
\title{Depth First Search  }
\description{
  This function implements algorithm 4.2.1 of Gross and Yellen. The
  input is a \code{graph} and a \code{node} to start from. It returns a
  standard vertex labeling of \code{graph}. This is a vector with
  elements corresponding to the nodes of \code{graph} and with values
  that correspond to point in the depth first search the node is
  visited.
}
\usage{
DFS(object, node, checkConn=TRUE)
}
\arguments{
  \item{object}{An instance of the \code{graph} class. }
  \item{node}{A \code{character} indicating the starting node. }
  \item{checkConn}{A \code{logical} indicating whether the connectivity
    of the graph should be checked. }
}
\details{
  This function implements algorithm 4.2.1 of Gross and Yellen. Specific
  details are given there.

  It requires that the graph be connected. By default, this is checked,
  but since the checking can be expensive it is optional.

  A faster and mostly likely better implementation of depth first
  searching is given by \code{\link[RBGL:bfs]{dfs}} in the \pkg{RBGL}
  package.  
}
\value{
  A vector with names given by the nodes of \code{graph} whose values
  are \code{0} to one less than the number of nodes. These indices
  indicate the point at which the node will be visited.
}
\references{\emph{Graph Theory and its Applications}, J. Gross and J. Yellen. }
\author{R. Gentleman }

\seealso{\code{\link{boundary}}}

\examples{
  RNGkind("Mersenne-Twister")
  set.seed(123)
  g1 <- randomGraph(letters[1:10], 1:4, p=.3)
  RNGkind()
  DFS(g1, "a")
}
\keyword{manip}
