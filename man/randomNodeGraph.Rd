\name{randomNodeGraph}
\alias{randomNodeGraph}
\title{Generate Random Graph with Specified Degree Distribution}
\description{
  \code{randomNodeGraph} generates a random graph with the specified
  degree distribution.  Self-loops are allowed.  The resultant graph is
  directed (but can always be coerced to be undirected).
}
\usage{
randomNodeGraph(nodeDegree)
}
\arguments{
  \item{nodeDegree}{A named integer vector specifying the node degrees. }
}
\details{
  The input vector must be named, the names are taken to be the names of
  the nodes. The sum must be even (there is a theorem that says we
  require that to construct a graph). Self-loops are allowed, although
  patches to the code that make this a switchable parameter would be
  welcome.
}
\value{
  An instance of the \code{graphNEL} class. The graph is directed.
}
\references{Random Graphs as Models of Networks, M. E. J. Newman. }
\author{R. Gentleman}

\seealso{\code{\link{randomGraph}}, \code{\link{randomEGraph}}
}

\examples{
set.seed(123)
c1 <- c(a = 1, b = 1, c = 2, d = 4)

(g1 <- randomNodeGraph(c1))
stopifnot(validObject(g1))
}
\keyword{graphs}
\keyword{manip}
