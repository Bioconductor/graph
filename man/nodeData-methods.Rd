\name{nodeData-methods}
\docType{methods}
\alias{nodeData-methods}
\alias{nodeData<--methods}
\alias{nodeData}
\alias{nodeData<-}

\title{Get and set attributes for the nodes of a graph object}
\description{
  Attributes of the nodes of a graph can be accessed using
  \code{nodeData}.  The attributes must be defined using
  \code{\link{nodeDataDefaults}}.  You can ommit the \code{n} argument
  to retrieve attributes for all nodes in the graph.  You can ommit the
  \code{attr} argument to retrieve all attributes.
}

\usage{
nodeData(self, n, attr)
nodeData(self, n, attr) <- value
}
\arguments{
  \item{self}{A \code{graph-class} instance}
  \item{n}{A \code{character} vector of node names}
  \item{attr}{A \code{character} vector of length one specifying the
    name of a node attribute}
  \item{value}{An R object to store as the attribute value}
}

\keyword{methods}

