\name{MultiGraph-class}
\Rdversion{1.1}
\docType{class}
\alias{MultiGraph-class}
\alias{nodes,MultiGraph-method}
\alias{numEdges,MultiGraph-method}
\alias{numNodes,MultiGraph-method}
\alias{show,MultiGraph-method}
\alias{MultiGraph}
\alias{eweights}
% these are not supposed to be publicly available
% but aliased here to quiet R CMD check
\alias{isDirected,DiEdgeSet-method}
\alias{isDirected,UEdgeSet-method}

\title{EXPERIMENTAL class "MultiGraph"}
\description{

  The MultiGraph class represents a single node set and a set of edge
  sets.  Each edge set is either directed or undirected.  We can think
  of an edge in a MultiGraph as a 4-tuple (from-node, to-node,
  edge-type, weight), where the edge-type field in the tuple identifies
  the edge set, the weight is a numeric value, and the order of the
  nodes only matters in the case of a directed edge set.  Unlike some of
  the graph representations, self-loops are allowed (from-node ==
  to-node).

  There is support for arbitrary edge attributes which is primarily
  useful for rendering plots of MultiGraphs.  These attributes are
  stored separately from the edge weights to facilitate efficient edge
  weight computation.

}

\usage{
MultiGraph(edgeSets, nodes = NULL, directed = TRUE)
eweights(object, names.sep = NULL)
}

\arguments{
  \item{edgeSets}{
    
    A named list of \code{data.frame} objects each representing an edge
    set of the multigraph.  Each \code{data.frame} must have three
    columns: "from", "to", and "weight".  Columns "from" and "to" can be
    either factors or character vectors.  The "weight" column must be
    numeric.

  }
  \item{nodes}{
    
    A character vector of node labels.  Nodes with zero degree can be
    included in a graph by specifying the node labels in \code{nodes}.
    The node set of the resulting multigraph is the union of the node
    labels found in \code{edgeSets} and \code{nodes}.

  }

  \item{directed}{

    A logical vector indicating whether the edge sets specified in
    \code{edgeSets} represent directed edges.  If this argument has
    length one, the value applies to all edge sets in \code{edgeSets}.
    Otherwise, this argument must have the same length as
    \code{edgeSets}, values are aligned by position.

  }

  \item{object}{

    A \code{MultiGraph} instance
    
  }

  \item{names.sep}{
    
    The string to use as a separator between from and to node labels.
    If \code{NULL} no names will be attached to the returned vector.
    
  }

}
\section{Constructors}{
  \code{MultiGraph}
}
\section{Methods}{
  \describe{
    \item{nodes}{\code{signature(object = "MultiGraph")}: ... }
    \item{numEdges}{\code{signature(object = "MultiGraph")}: ... }
    \item{numNodes}{\code{signature(object = "MultiGraph")}: ... }
    \item{show}{\code{signature(object = "MultiGraph")}: ... }
    \item{eweights}{Return edge weights (actually first edge attribute)}
  }
}

\author{S. Falcon}

\examples{
    ft1 <- data.frame(from=c("a", "a", "a", "b", "b"),
                        to=c("b", "c", "d", "a", "d"),
                      weight=c(1, 3.1, 5.4, 1, 2.2))

    ft2 <- data.frame(from=c("a", "a", "a", "x", "x", "c"),
                        to=c("b", "c", "x", "y", "c", "a"),
                      weight=c(3.4, 2.6, 1, 1, 1, 7.9))

    esets <- list(es1=ft1, es2=ft2)

    g <- MultiGraph(esets)

    nodes(g)
    numEdges(g)

    eweights(g)
    eweights(g, names.sep = "=>")
}
\keyword{classes}
