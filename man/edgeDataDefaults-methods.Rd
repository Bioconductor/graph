\name{edgeDataDefaults-methods}
\docType{methods}
\alias{edgeDataDefaults-methods}
\alias{edgeDataDefaults<--methods}
\alias{edgeDataDefaults}
\alias{edgeDataDefaults<-}

\title{Get and set default attributes for the edges of a graph}

\usage{
edgeDataDefaults(self, attr)
edgeDataDefaults(self, attr) <- value
}
\arguments{
  \item{self}{A \code{\link{graph-class}} instance}
  \item{attr}{A \code{character} vector of length one giving the name of
    the attribute}
  \item{value}{An R class to use as the default value for the specified attribute}
\description{
  Set default values for attributes associated with the edges of a graph.
}
\keyword{methods}

