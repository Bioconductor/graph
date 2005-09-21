\name{isAdjacent-methods}
\docType{methods}
\alias{isAdjacent-methods}
\alias{isAdjacent}
\alias{isAdjacent,graph,character,character-method}
\alias{isAdjacent,graphIM,character,character-method}

\title{Determine if nodes share an edge in a graph}

\usage{
isAdjacent(object, from, to)
}
\arguments{
  \item{object}{An instance of a subclass of \code{\link{class-graph}}.}
  \item{from}{A \code{character} of nodes in the graph.}
  \item{to}{A \code{character} vector of nodes in the graph}
}

\description{
  For a given subclass of \code{class-graph}, returns \code{TRUE} if the
  graph contains an edge from node specified by \code{from} to the node
  specified by \code{to}.

  The appropriate logical vector will be returned as long as \code{from}
  and \code{to} have the same length and contain nodes in the graph
  object specified by \code{object}.
}
% \section{Methods}{
%   \describe{
    
%     \item{object = "graph", from = "character", to = "character"}{ describe this method here }
    
%     \item{object = "graphIM", from = "character", to = "character"}{ describe this method here }
%   }
% }
\keyword{methods}

