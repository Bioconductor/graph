\name{multiGraph-class}
\docType{class}
\alias{multiGraph-class}

\alias{edgeSet-class}
\alias{edgeSetNEL-class}
\alias{edgeSetAM-class}

\alias{show,multiGraph-method}
\alias{show,edgeSet-method}

\alias{isDirected,multiGraph-method}
\alias{isDirected,edgeSet-method}

\alias{edgemode,edgeSet-method}

\alias{nodes,multiGraph-method}
\alias{nodes,edgeSetAM-method}
\alias{numNodes,multiGraph-method}

\alias{edges,multiGraph,missing-method}
\alias{edges,multiGraph,character-method}
\alias{edges,edgeSetAM,character-method}
\alias{edges,edgeSetNEL,character-method}

\alias{numEdges,edgeSetAM-method}
\alias{numEdges,multiGraph-method}


\title{Class "multiGraph" }
\description{A collection of classes to model multigraphs. These
include the multiGraph class as well as classes to contain edge sets. }
\section{Objects from the Class}{
Objects can be created from the \code{multiGraph} class, the
\code{edgeSet} class is virtual, and particular variants should be used.
}
\section{Slots}{
  These slots are for the multiGraph class.
  \describe{
    \item{nodes}{The names of the nodes.}
    \item{edgeL}{A list of edge lists.}
    \item{nodeData}{An instance of the \code{attrData} class.}
    \item{graphData}{A list.}
  }
  These slots are for the \code{edgeSet} class, or one of its
  sublcasses.
  \describe{
  \item{edgeData}{An instance of the \code{attrData} class.}
  \item{edgemode}{A character vector, one of directed, or undirected.}
  \item{edgeL}{A list of the edges (graphNEL)}
  \item{adjMat}{An adjacency martix (graphAM)}
  }
}

\section{Methods}{
  \describe{
    \item{show}{Print a multigraph.}
    \item{isDirected}{A vector indicating which of the edgeSets is
      directed.}
    \item{nodes}{Retrieve the node names}
    \item{numNodes}{Return the number of nodes}
    \item{edges}{Return either all edges, or a subset of them, depending
      on the arguments supplied.}
    \item{numEdges}{Return a vector with the number of edges, for each
      edge set.}
  }
}
\keyword{classes}


