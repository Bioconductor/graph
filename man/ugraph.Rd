\name{ugraph}
\alias{ugraph}
\title{Underlying Graph}
\description{
 For a \emph{directed} graph the underlying graph is the graph that is
 constructed where all edge orientation is ignored. This function
 carries out such a transformation on \code{graphNEL} instances.
}
\usage{
ugraph(graph)
}
\arguments{
  \item{graph}{An instance of \code{graphNEL}. }
}

\details{
   If \code{graph} is already \emph{undirected} then it is simply
   returned.

   If \code{graph} is a multi-graph (has multiple edges) and error is
   thrown as it is unclear how to compute the underlying graph in that
   context.
 }
 

\value{
  An instance of \code{graphNEL} with the same nodes as the input but
  which is \code{undirected}.
}
\references{Graph Theory and its Applications, J. Gross and J. Yellen. }
\author{R. Gentleman }

\seealso{\code{\link{connComp}}}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
}
\keyword{manip}
