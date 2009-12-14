\name{MultiDiGraph-class}
\Rdversion{1.1}
\docType{class}
\alias{MultiDiGraph-class}
\alias{nodes,MultiDiGraph-method}
\alias{numEdges,MultiDiGraph-method}
\alias{numNodes,MultiDiGraph-method}
\alias{show,MultiDiGraph-method}
\alias{MultiDiGraph}

\alias{eweights}
\alias{edgeMatrices}
\alias{extractGraph}
\alias{edgeIntersect}
\alias{edgeUnion}

\title{EXPERIMENTAL class "MultiDiGraph"}
\description{
  A class for representing directed multigraphs.  The term multigraph is
  used ambiguously.  Here we mean a graph with zero or more directed
  edges between a pair of nodes and no self-edges.  Conceptually, this
  is implemented as a shared node set and a list of edge sets.

  Most operations on a \code{MultiDiGraph} will return a list with one
  element for each edge set (e.g. \code{numEdges}).
}

\usage{
MultiDiGraph(edgeSets, nodes = NULL)
eweights(object, names.sep = NULL)
edgeMatrices(object)
extractGraph(object, which)
edgeIntersect(object, weightFun = oneWeights)
edgeUnion(object, weightFun = NULL)
}

\arguments{
  \item{edgeSets}{
    A list of \code{data.frame}s.  Each \code{data.frame} must have at
    least two columns, "from" and "to", that represent the graph
    structure as a from/to edge list.  Additional columns are treated as
    edge attributes and the first such edge attribute column is treated
    specially as edge weights.
  }
  \item{nodes}{
    A character vector of node labels.  Nodes with zero degree can be
    included in a graph by specifying the node labels in \code{nodes}.
    The node set of the resulting directed multigraph is the union of
    the node labels found in \code{edgeSets} and \code{nodes}.}

  \item{object}{
    A \code{MultiDiGraph} instance
  }

  \item{which}{
    For \code{extractGraph}, an integer or character vector of length
    one used to select an edge set in  \code{MultiDiGraph}.
  }

  \item{names.sep}{
    The string to use as a separator between from and to node labels.
    If \code{NULL} no names will be attached to the returned vector.
  }

  \item{weightFun}{
    A function that will be used to combine edge weights for multigraph
    set operations.  The function should take a single argument, a
    matrix of edge weights.  Each column corresponds to edge weights
    from a single edge set (order of edge sets is preserved).  In the
    case of \code{edgeUnion} the matrix will contain \code{NA} values
    for edges not in a given edge set.  The return value should be a
    vector with length equal to the number of rows of its argument.
  }
}
\section{Constructors}{
  \code{MultiDiGraph}
}
\section{Methods}{
  \describe{
    \item{nodes}{\code{signature(object = "MultiDiGraph")}: ... }
    \item{numEdges}{\code{signature(object = "MultiDiGraph")}: ... }
    \item{numNodes}{\code{signature(object = "MultiDiGraph")}: ... }
    \item{show}{\code{signature(object = "MultiDiGraph")}: ... }
    \item{eweights}{Return edge weights (actually first edge attribute)}
    \item{edgeMatrices}{Return a list of edge matrices for each edge set}
    \item{extractGraph}{Return a \code{graphNEL} corresponding to the
      specified edge set.}

    \item{edgeIntersect}{Return a new \code{MultiDiGraph} with a single
      edge set containing the intersection of the original edge sets.
      By default the intersection edges will have a weight of one.  Edge
      weight combining can be customized by specifying a
      \code{weightFun} function.
    }

    \item{edgeUnion}{Return a new \code{MultiDiGraph} with a single edge
      set containing the union of the original edge sets.}
  }
}

\author{S. Falcon}




\examples{
    ft1 <- data.frame(from=c("a", "a", "a", "b", "b"),
                      to=c("b", "c", "d", "a", "d"),
                      weights=c(1, 3.1, 5.4, 1, 2.2))

    ft2 <- data.frame(from=c("a", "a", "a", "x", "x", "c"),
                      to=c("b", "c", "x", "y", "c", "a"),
                      weights=c(3.4, 2.6, 1, 1, 1, 7.9))

    esets <- list(ft1, ft2)

    g <- MultiDiGraph(esets)

    nodes(g)
    numEdges(g)

    eweights(g)
    eweights(g, names.sep = "=>")

    edgeMatrices(g)


    gnel <- extractGraph(g, which = 2L)
    gnel
    nodes(gnel)

}
\keyword{classes}
