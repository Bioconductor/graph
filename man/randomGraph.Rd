\name{randomGraph}
\alias{randomGraph}
\title{ Random Graph }
\description{
 This function generates a random graph according to a model that
 involves a latent variable.  The construction is to randomly assign
 members of the set \code{M} to the nodes, \code{V}. An edge is assigned
 between two elements of \code{V} when they both have the same element
 of \code{M} assigned to them. An object of class \code{graphNEL} is
 returned.
}
\usage{
randomGraph(V, M, p, weights=TRUE)
}
\arguments{
  \item{V}{The nodes of the graph. }
  \item{M}{A set of values used to generate the graph. }
  \item{p}{A value between 0 and 1 that indicates the probability of
    selecting an element of \code{M}}
  \item{weights}{A logical indicating whether to use the number of
    shared elements of \code{M} as weights. }
}
\details{
  The model is quite simple. To generate a graph, \code{G}, the user
  supplies the list of nodes, \code{V} and a set of values \code{M}
  which will be used to create the graph. For each node in \code{V} a
  logical vector with length equal to the length of \code{M} is
  generated. The probability of a \code{TRUE} at any position is
  determined by \code{p}. Once valus from \code{M} have been assigned to
  each node in \code{V} the result is processed into a graph. This is
  done by creating an edge between any two elements of \code{V} that
  share an element of \code{M} (as chosen by the selection process).

  The sizes of \code{V} and \code{M} and the values of \code{p}
  determine how dense the graph will be.
}
\value{
An object of class \code{\link{graphNEL-class}} is returned.
}
\author{R. Gentleman }

\seealso{\code{\link{randomEGraph}}, \code{\link{randomNodeGraph}}
}

\examples{
 set.seed(123)
 V <- letters[1:10]
 M <- 1:4
 g1 <- randomGraph(V, M, 0.2)
 numEdges(g1) # 16, in this case
 edgeNames(g1)# "<from> ~ <to>"  since undirected
}
\keyword{graphs}
