\name{duplicatedEdges}
\alias{duplicatedEdges}
\title{duplicatedEdges  }
\description{
  A multigraph is a graph where edges between nodes can be represented
  several times. For some algorithms this causes
  problems. \code{duplicatedEdges} tests an instance of the
  \code{graphNEL} class to see if it has duplicated edges and returns
  \code{TRUE} if it does and \code{FALSE} otherwise.
}
\usage{
duplicatedEdges(graph)
}
\arguments{
  \item{graph}{An instance of the class \code{graphNEL} }
}
\details{
  It would be nice to handle other types of graphs.
}
\value{
  A logical, either \code{TRUE} if the graph has duplicated edges or
  \code{FALSE} it not.
}

\author{R. Gentleman }

\seealso{ \code{\link{connComp}}, \code{\link{ugraph}} }

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
}
\keyword{manip }
