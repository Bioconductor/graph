\name{isAdjacent-methods}
\docType{methods}
\alias{isAdjacent-methods}
\alias{isAdjacent}

\title{Determine if nodes share an edge in a graph}

\usage{
isAdjacent(object, from, to, ...)
}
\arguments{
  \item{object}{An instance of a subclass of \code{\link{graph-class}}.}
  \item{from}{A \code{character} vector of nodes in the graph.}
  \item{to}{A \code{character} vector of nodes in the graph}
  \item{...}{May be used by methods called on subclasses of \code{graph}}
}

\description{
  For a given subclass of \code{graph-class}, returns \code{TRUE} if the
  graph contains an edge from node specified by \code{from} to the node
  specified by \code{to}.

  The appropriate logical vector will be returned as long as \code{from}
  and \code{to} have the same length and contain nodes in the graph
  object specified by \code{object}.
}
\keyword{methods}

