\name{edgeData-methods}
\docType{methods}
\alias{edgeData-methods}
\alias{edgeData<--methods}
\alias{edgeData}
\alias{edgeData<-}

\title{Get and set attributes for the edges of a graph object}

\usage{
edgeData(self, from, to, attr)
edgeData(self, from, to, attr) <- value
}
\arguments{
  \item{self}{A \code{graph-class} instance}
  \item{from}{A \code{character} vector of node names}
  \item{to}{A \code{character} vector of node names}
  \item{attr}{A \code{character} vector of length one specifying the
  name of a node attribute}
  \item{value}{An R object to store as the attribute value}
}

\description{
  Attributes of the edges of a graph can be accessed using
  \code{edgeData}.  The attributes must be defined using
  \code{\link{edgeDataDefaults}}.  You can ommit the \code{from} or
  \code{to} argument to retrieve attribute values for all edges to
  (respectively, from) a given node.
}
\keyword{methods}

