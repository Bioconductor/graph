\name{graph-class}
\docType{class}
\alias{graph-class}
\alias{complement}
\alias{connComp}
\alias{degree}
\alias{degree}
\alias{edgemode}
\alias{intersection}
\alias{isConnected}
\alias{join}
\alias{numNodes}
\alias{union}
\alias{acc,graph-method}
\alias{complement,graph-method}
\alias{connComp,graph-method}
\alias{degree,graph,missing-method}
\alias{degree,graph,ANY-method}
\alias{dfs,graph-method}
\alias{edgemode,graph-method}
\alias{intersection,graph,graph-method}
\alias{isConnected,graph-method}
\alias{join,graph,graph-method}
\alias{numNodes,graph-method}
\alias{union,graph,graph-method}
\title{Class "graph" }
\description{A virtual class that all graph classes should extend. }
\section{Objects from the Class}
{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
    \item{\code{edgemode}:}{Indicates whether edges are
      \code{"directed"} or \code{"undirected"}}
  }
}

\section{Methods}{
  \describe{
    \item{connComp}{\code{signature(object = "graph")}: find the
      connected components of a graph.}
    \item{isConnected}{\code{signature(object = "graph")}: A boolean
      that details if a graph is fully connected or not.}
    \item{acc}{\code{signature(object = "graph")}: find all nodes
      accessible from the specified node. }
    \item{degree}{\code{signature(object = "graph", Nodes = "missing")}:
    find the \code{degree} of a node (number of coincident edges).}
    \item{degree}{\code{signature(object = "graph", Nodes = "ANY")}: as
      above.  }
    \item{dfs}{\code{signature(object = "graph")}: execute a depth first
    search on a graph starting with the specified node.}
    \item{intersection}{\code{signature(x = "graph", y = "graph")}: compute the
      intersection of the two supplied graphs. They must have identical
      nodes. Currently this returns an object of class
      \code{graphNEL}. With edge weights of 1 for any matching edge.}
    \item{join}{\code{signature(x = "graph", y = "graph")}: returns the
      joining of two graphs.  Nodes which are shared by both graphs will
      have their edges merged.}
    \item{union}{\code{signature(x = "graph", y = "graph")}: compute the
      union of the two supplied graphs. They must have identical
      nodes. Currently this returns an object of class \code{graphNEL}.}
    \item{complement}{\code{signature(x = "graph")}: compute the
      complement of the supplied graph. The complement is defined with
      respect to the complete graph on the nodes in \code{x}.
      Currently this returns an object of class \code{graphNEL}.}
    \item{numNodes}{\code{signature(object = "graph")}: compute the
      number of nodes in a graph. }
    \item{edges}{\code{signature(object="graph", which="character")}:
      return the edges indicated by \code{which}. \code{which} can be
      missing in which case all edges are returned or it can be a
      character vector with the node labels indicating the nodes whose
      edge lists are wanted.}
    \item{nodes<-}{A generic function that allows different
      implementations of the \code{graph} class to reset the node labels}
    \item{plot}{Please see the help page for the \code{plot.graph}
      method in the \code{Rgraphviz} package}
  }
}
\references{ Graph Theory and its Applications, J. Gross and J. Yellen. }
\author{R. Gentleman and E. Whalen. }

\seealso{\code{\link{graphNEL-class}}, \code{\link{distGraph-class}} }

\examples{
  set.seed(123)
  g1 <- randomGraph(letters[1:10], 1:4, p=.3)
  edges(g1)
  edges(g1, "a")

}
\keyword{classes}
