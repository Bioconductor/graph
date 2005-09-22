\name{graph-class}
\docType{class}
\alias{graph-class}

\alias{complement}
\alias{complement,graph-method}

\alias{connComp}
\alias{connComp,graph-method}

\alias{degree}
\alias{degree,graph,missing-method}
\alias{degree,graph,ANY-method}

\alias{dfs,graph-method}

\alias{edgeDataDefaults<-,graph,missing,list-method}
\alias{edgeDataDefaults<-,graph,character-method}
\alias{edgeDataDefaults,graph,missing-method}
\alias{edgeDataDefaults,graph,character-method}
\alias{edgeData<-,graph,character,character,character-method}
\alias{edgeData<-,graph,character,missing,character-method}
\alias{edgeData<-,graph,missing,character,character-method}
\alias{edgeData,graph,character,character,character-method}
\alias{edgeData,graph,character,character,missing-method}
\alias{edgeData,graph,character,missing,character-method}
\alias{edgeData,graph,missing,character,character-method}
\alias{edgeData,graph,missing,missing,character-method}
\alias{edgeData,graph,missing,missing,missing-method}

\alias{edgemode}
\alias{edgemode<-}
\alias{edgemode,graph-method}
\alias{edgemode<-,graph,character-method}

\alias{edgeNames}
\alias{edgeNames,graph-method}

%% wh 16.1.05: I think intersection2 and intersection3 can go once we have an
%% implementation that everybody is happy with
\alias{intersection}
\alias{intersection,graph,graph-method}
\alias{intersection2} 
\alias{intersection2,graph,graph-method}
\alias{intersection3}
\alias{intersection3,graph,graph-method}

\alias{isAdjacent,graph,character,character-method}

\alias{isConnected}
\alias{isConnected,graph-method}

\alias{isDirected}
\alias{isDirected,graph-method}

\alias{join}
\alias{join,graph,graph-method}

\alias{nodeDataDefaults<-,graph,missing,list-method}
\alias{nodeDataDefaults<-,graph,character-method}
\alias{nodeDataDefaults,graph,missing-method}
\alias{nodeDataDefaults,graph,character-method}
\alias{nodeData<-,graph,character,character-method}
\alias{nodeData<-,graph,missing,character-method}
\alias{nodeData,graph,character,character-method}
\alias{nodeData,graph,character,missing-method}
\alias{nodeData,graph,missing,character-method}
\alias{nodeData,graph,missing,missing-method}

\alias{numNodes}
\alias{numNodes,graph-method}

\alias{show}
\alias{show,graph-method}

\alias{union}
\alias{union,graph,graph-method}

\title{Class "graph" }
\description{A virtual class that all graph classes should extend. }
\section{Objects from the Class}{
A virtual Class: No objects may be created from it.
}
\section{Slots}{
  \describe{
    \item{\code{edgemode}:}{Indicates whether edges are
      \code{"directed"} or \code{"undirected"}}
  }
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "graph")}:A print method for
      the graph.}
    \item{acc}{\code{signature(object = "graph")}: find all nodes
      accessible from the specified node. }

        \item{complement}{\code{signature(x = "graph")}: compute the
      complement of the supplied graph. The complement is defined with
      respect to the complete graph on the nodes in \code{x}.
      Currently this returns an object of class \code{graphNEL}.}

    \item{connComp}{\code{signature(object = "graph")}: find the
      connected components of a graph.}


    \item{degree}{\code{signature(object = "graph", Nodes = "missing")}:
    find the \code{degree} of a node (number of coincident edges).}

  \item{degree}{\code{signature(object = "graph", Nodes = "ANY")}: as
      above.  }

    \item{dfs}{\code{signature(object = "graph")}: execute a depth first
    search on a graph starting with the specified node.}

      \item{edges}{\code{signature(object="graph", which="character")}:
      return the edges indicated by \code{which}. \code{which} can be
      missing in which case all edges are returned or it can be a
      character vector with the node labels indicating the nodes whose
      edge lists are wanted.}

    \item{edgeDataDefaults}{Get and set default attributes for the edges
      in the graph.}
    
    \item{edgeData}{Get and set attributes for edges in the graph}

    \item{edgemode}{\code{signature(object="graph")}: return the
      \code{edgemode} for the graph. Currently this can be either
      \code{directed} or \code{undirected}.}
    \item{edgemode<-}{\code{signature(object="graph",
	value="character")}: set the \code{edgemode}
      for the graph. Currently this can be either
      \code{directed} or \code{undirected}.}

    \item{intersection}{\code{signature(x = "graph", y = "graph")}: compute the
      intersection of the two supplied graphs. They must have identical
      nodes. Currently this returns an object of class
      \code{graphNEL}. With edge weights of 1 for any matching edge.}

    \item{isAdjacent}{\code{signature(from="character",
    to="character")}: Determine if edges exists between nodes.}
    
    \item{isConnected}{\code{signature(object = "graph")}: A boolean
      that details if a graph is fully connected or not.}

    \item{isDirected}{Return \code{TRUE} if the graph object has
    directed edges and \code{FALSE} otherwise.}

    \item{join}{\code{signature(x = "graph", y = "graph")}: returns the
      joining of two graphs.  Nodes which are shared by both graphs will
      have their edges merged.}

    \item{nodes<-}{A generic function that allows different
      implementations of the \code{graph} class to reset the node
      labels}

    \item{nodeDataDefaults}{Get/set default attributes for nodes in the
      graph.}

    \item{nodeData}{Get/set attributes for nodes in the graph.}

    \item{numNodes}{\code{signature(object = "graph")}: compute the
      number of nodes in a graph. }

    \item{plot}{Please see the help page for the \code{plot.graph}
      method in the \code{Rgraphviz} package}

    \item{union}{\code{signature(x = "graph", y = "graph")}: compute the
      union of the two supplied graphs. They must have identical
      nodes. Currently this returns an object of class \code{graphNEL}.}

    \item{edgeNames}{\code{signature(object = "graph")}: Returns a
      vector of the edge names for this graph, using the format
      \code{tail\~head}, where \code{head} is the name of the tail node
      and \code{head} is the name of the head node.}

    }
  }
}

\details{
  \code{degree} returns either a named vector (names correspond to the
  nodes in the graph) containing the degree for undirected graphs or a
  list with two components, \code{inDegree} and \code{outDegree} for
  directed graphs.

  \code{connComp} returns a list of the connected components. Each
  element of this list contains the labels of all nodes in that
  component.

  For a \emph{directed graph} or \emph{digraph} the \dfn{underlying
    graph} is the graph that results from removing all direction from
  the edges. This can be achieved using the function \code{\link{ugraph}}.

  A \dfn{weakly connected} component of a \emph{digraph} is one that is
  a connected component of the underlying graph. This is the default for
  \code{connComp}. A \emph{digraph} is  \dfn{strongly connected} if
  every two vertices are mutually reachable. A \dfn{strongly connected}
  component of a \emph{digraph}, \strong{D}, is a maximal \emph{strongly
  connected} subdigraph of \strong{D}. See the \pkg{RBGL} package for an
  implementation of Trajan's algorithm to find \emph{strongly
  connected} components (\code{\link[RBGL]{strongComp}}).

  In the \pkg{graph} implementation of \code{connComp} \emph{weak
    connectivity} is used. If the argument to \code{connComp} is a
  directed graph then \code{\link{ugraph}} is called to create the
  underlying undirected graph and that is used to compute connected
  components. Users who want different behavior are encouraged to use
  \pkg{RBGL}.
}

\references{ Graph Theory and its Applications, J. Gross and J. Yellen. }
\author{R. Gentleman and E. Whalen. }

\seealso{\code{\link{graphNEL-class}}, \code{\link{distGraph-class}} }

\examples{
  set.seed(123)
  g1 <- randomGraph(letters[1:10], 1:4, p=.3)
  edges(g1)
  edges(g1, "a")

  ## edgeNames(g1)
}
\keyword{classes}
