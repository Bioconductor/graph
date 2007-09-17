\name{multiGraph-class}
\docType{class}
\alias{multiGraph-class}

\alias{edgeSet-class}
\alias{edgeSetNEL-class}
\alias{edgeSetAM-class}

\title{Class "multGraph" }
\description{A collection of classes to model multigraphs. These
include the multiGraph class as well as classes to contain edge sets. }
\section{Objects from the Class}{
Objects can be created from the \code{multiGraph} class, the
\code{edgeSet} class is virtual, and particular variants should be used.
}
\section{Slots}{
  \describe{
    \item{nodes}{The names of the nodes.}
    \item{edgeL}{A list of edge lists.}
    \item{nodeData}{An instance of the \code{attrData} class.}
  }
}

