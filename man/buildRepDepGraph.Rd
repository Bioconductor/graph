\name{buildRepDepGraph}
\alias{buildRepDepGraph}
\alias{pkgInstOrder}
\title{Functionality to manage repository dependency graphs}
\description{
  These functions can be used to represent and manipulate dependency
  graphs for a specified package repository.
}
\usage{
buildRepDepGraph(repository, depLevel = c("Depends", "Suggests"))
pkgInstOrder(pkg, repGraph)
}
\arguments{
  \item{repository}{A URL to a \code{CRAN} style repository}
  \item{depLevel}{One of \code{Depends} or \code{Suggests}, detailing
    the level of dependencies to search.  The \code{Suggests} value
    includes everything in \code{Depends}.}
  \item{pkg}{The package to get the installation order for}
  \item{repGraph}{A \code{graph} object representing a repository, as
    from \code{buildRepDepGraph}}
}
\value{
  For \code{buildRepDepGraph}, a graph representing the dependency
  structure of the specified repository, where an edge from node
  \code{A} to node \code{B} represents a dependency on \code{B} by \code{A}.

  For \code{pkgInstOrder}, a vector is returned, listing the appropriate
  order one would take to install all of the necessary packages to
  install the specified package.  That is, it makes sure that at every
  step, any package being installed does not depend on one that has not
  yet been installed.  This order can then be used with functions such
  as \code{install.packages}.
}
\author{Jeff Gentry}
\examples{
   repos <- getOption("repositories")()["BIOC"] ## Get BIOC repos

   if("FIXME"=="Jeff, we can't assume that we're always online - wh")
     buildRepDepGraph(repos)
}
\keyword{graphs}
