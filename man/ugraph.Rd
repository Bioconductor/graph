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

   If \code{graph} is a multi-graph (has multiple edges) an error is
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
V <- letters[1:4]
edL2 <- vector("list", length=4)
names(edL2) <- V
for(i in 1:4)
  edL2[[i]] <- list(edges=c(2,1,2,1)[i], weights=sqrt(i))
gR2 <- new("graphNEL", nodes=V, edgeL=edL2, edgemode="directed")

ugraph(gR2)

}
\keyword{manip}
