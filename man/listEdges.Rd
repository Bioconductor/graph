\name{listEdges}
\alias{listEdges}
\title{List the Edges of a Graph  }
\description{
  A list where each element contains all edges between two nodes,
  regardless of orientation. The list has names which are node pairs,
  in lexicographic order, and elements all edges between those nodes.
}
\usage{
listEdges(object, dropNULL=TRUE)
}
\arguments{
  \item{object}{An instance of the \code{\link{graphNEL-class}} class. }
  \item{dropNULL}{Should those node pairs with no edges be dropped from
    the returned list. }
}
\details{
  The function is currently only implemented for graphs of the
  \code{\link{graphNEL-class}}. The edges in the returned list are
  instances of the \code{\link{simpleEdge-class}}.
}
\value{
  A named list of \code{\link{simpleEdge-class}} objects. 
}
\author{R. Gentleman}

\seealso{\code{\link{simpleEdge-class}}, \code{\link{edges}}}

\examples{
        set.seed(123)
        V <- LETTERS[1:4]
        edL <- vector("list", length=4)
        names(edL) <- V
        toE <- LETTERS[4:1]
        for(i in 1:4) 
           edL[[i]] <- list(edges=5-i, weights=runif(1))
        gR <- new("graphNEL", nodes=V, edgeL=edL)
        listEdges(gR)
}
\keyword{manip}
