\name{toDotWithRI}
\alias{toDotWithRI}
\title{Translate a graph to "dot" including rendering information}
\usage{
toDotWithRI(graph, graph_name = NULL, subGraphList = list(),
  isStrict = TRUE)
}
\arguments{
  \item{graph}{An object of graph \code{graph}}

  \item{graph_name}{The name of the graph}

  \item{subGraphList}{A list of objects of class \code{graph}. Each of
    these is used as a subgraph in the graph. The names in the list are
    taken as the names of the subgraph. If a graph is to be seen as a
    cluster (i.e. with a box around it), its name has to start with
    \code{cluster}.}

  \item{isStrict}{Should the graph be strict}
}
\value{
  A character vector with the graph in dot format
}
\description{
  The function takes a graph object and translates it into the dot
  format. All rendering information is written verbatim into the dot
  graph as well
}

\details{
  Given a graph object, it is translated into the \code{dot} language so
  that it can be rendered using the \code{graphviz} software. In
  addition to plotting the graph itself, all the rendering information
  is being used as well.

  \code{graphRenderInfo} attributes are written as an attribute list
  after the \code{graph} statement in dot.

  \code{nodeRendenInfo} attributes are written as attribute lists after
  each node. If an attribute is constant across all node, a global node
  attribute is written instead of many individual ones.##' Newlines ##'
  in attributes do not lead to newlines in labels. In \code{label},
  \code{headlabel} and \code{taillabel}, in order to get a newline,
  right justification or left justification, the two character sequences
  \code{\\n}, \code{\\r} and \code{\\l} have to be written (i.e. in
  order to create this in R, the backslash has to be escaped in a
  string, that is has to be written as a double-backslash).

  \code{edgeRenderInfo} attributes as written as attribute lists after
  each edge, unless an attribute is constant, then it is written as a
  global edge attribute.

  In general, all attribute values are being wrapped in double-quotes,
  unless the attibute value start with a \code{<} and ends with a
  \code{>}. In this case it is taken as html content and not wrapped in
  double quotes (nor are contained newlines escaped).

  The resulting graph in dot format is returned as a character vector.
}

\author{
  Holger Hoefling \email{hhoeflin@gmail.com}
}