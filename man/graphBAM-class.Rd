\name{graphBAM-class}
\Rdversion{1.1}
\docType{class}
\alias{graphBAM-class}
\alias{addEdge,character,character,graphBAM,missing-method}
\alias{addEdge,character,character,graphBAM,numeric-method}
\alias{edgemode<-,graphBAM,character-method}
\alias{clearNode,character,graphBAM-method}
\alias{edges,graphBAM,missing-method}
\alias{edges,graphBAM,character-method}
\alias{inEdges,character,graphBAM-method}
\alias{initialize,graphBAM-method}
\alias{isAdjacent,graphBAM,character,character-method}
\alias{nodes,graphBAM-method}
\alias{show,graphBAM-method}
\alias{numEdges,graphBAM-method}
\alias{numNodes,graphBAM-method}
\alias{removeEdge,character,character,graphBAM-method}
\alias{removeNode,character,graphBAM-method}
\alias{edgeWeights,graphBAM-method}
\alias{edgeWeights,graphBAM,character-method}
\alias{edgeWeights,graphBAM,numeric-method}
\alias{edgeWeights,graphBAM,missing-method}
\alias{edgeData,graphBAM,character,missing,character-method}
\alias{edgeData,graphBAM,missing,missing,missing-method}
\alias{edgeData,graphBAM,character,character,character-method}
\alias{edgeData<-,graphBAM,character,character,character,numeric-method}
\alias{graphBAM}
\alias{coerce,graphBAM,graphAM-method}
\alias{coerce,graphBAM,graphNEL-method}
\alias{coerce,graphBAM,matrix-method}
\alias{graphBAMExtractFromTo}
\alias{ugraph,graphBAM-method}
\alias{adj,graphBAM,character-method}

\title{EXPERIMENTAL class "graphBAM"}
\description{
  The graphBAM class represents an adjacency matrix as a bit array.
  The bit array is stored as a raw vector.  This is EXPERIMENTAL and
  subject to change.
}

\usage{
graphBAM(df, nodes=NULL, edgemode="undirected", ignore_dup_edges = FALSE)
}
  %     Create a \code{graphBAM} instance given an edge list specified as
  %     character vectors from and to.  The nodes argument can be used to
  %     add nodes with degree zero.  The weights argument should have the
  %     same length as from and to.
  %     }

\arguments{
 \item{df}{A \code{data.frame} object with three columns: "from", "to" and 
     "weight". Columns "from" and "to" can be either factors or character vectors. 
     The "weight" column must be  numeric.
    }
 \item{nodes}{A character vector of node labels.  Use this to add
  degree zero nodes to the graph.  If \code{NULL}, the set of nodes
  found in \code{from} and \code{to} will be used.}

\item{edgemode}{A string, one of "directed" or "undirected".}

\item{ignore_dup_edges}{If \code{FALSE} (default), specifying duplicate
  edges in the input is an error.  When set to \code{TRUE} duplicate
  edges are ignored.  Edge weight values are ignored when determining
  duplicates.  This is most useful for graph import and conversion.}
}
\section{Constructors}{
  \code{GraphBAM}
}
\section{Extends}{
Class \code{"\linkS4class{graph}"}, directly.
}
\section{Methods}{
  \describe{
    
    \item{addEdge}{\code{signature(from = "character", to = "character", graph = "graphBAM", weights = "missing")}: ... }
    \item{addNode}{\code{signature(node = "character", object = "graphBAM", edges = "missing")}: ... }
    \item{clearNode}{\code{signature(node = "character", object = "graphBAM")}: ... }
    \item{edges}{\code{signature(object = "graphBAM", which = "missing")}: ... }
    \item{edges}{\code{signature(object = "graphBAM", which = "character")}: ... }
    \item{inEdges}{\code{signature(node = "character", object = "graphBAM")}: ... }
    \item{initialize}{\code{signature(.Object = "graphBAM")}: ... }
    \item{isAdjacent}{\code{signature(object = "graphBAM", from = "character", to = "character")}: ... }
    \item{nodes}{\code{signature(object = "graphBAM")} Returns the nodes of the graphBAM object. }
    \item{numEdges}{\code{signature(object = "graphBAM")} Returns an integer vector of edge counts. }
    \item{numNodes}{\code{signature(object = "graphBAM")} Returns the number of nodes in the graphBAM object }
    \item{removeEdge}{\code{signature(from = "character", to = "character", graph = "graphBAM")}: ... }
    \item{removeNode}{\code{signature(node = "character", object = "graphBAM")}: ... }
    \item{edgeData}{\code{signature(self = "graphBAM", from = "character", 
            to="character", attr = "character")}}
    \item{edgeData}{\code{signature(self = "graphBAM", from = "character", 
            to="missing", attr = "character")}}
    \item{edgeData}{\code{signature(self = "graphBAM", from = "missing", 
            to="missing", attr = "missing")}}
    \item{edgeWeights}{\code{signature(object = "graphBAM", index = "character")} Returns the 
     edge weights from the indexed nodes to all adjacent nodes }
    \item{edgeWeights}{\code{signature(object = "graphBAM", index = "missing")} Returns the edge weights of the 
     graphBAM object}
    \item{edgeWeights}{\code{signature(object = "graphBAM", index = "numeric")} Returns the 
    edge weights from the indexed nodes to all adjacent nodes }
    \item{coerce}{\code{signature(from = "graphBAM", to = "graphAM")}: ... }
    \item{coerce}{\code{signature(from = "graphBAM", to = "graphNEL")}: ... }
    \item{coerce}{\code{signature(from = "graphBAM", to = "matrix")}: ... }
    \item{show}{\code{signature(object = "graphBAM")}} 
    \item{graphBAMExtractFromTo}{ Returns a data frame with column names from, to 
        and weight corresponding to the connected nodes in the graphBAM object.}
    \item{graphBAMToMatrix}{Returns a matrix with the weights corresponding the 
      connected nodes in the graphBAM object}
    \item{edgemode<-}{
      Set the edgemode for the graph ("directed" or "undirected").  If
    the specified edgemode is the same, the object is returned without
    changes.  Otherwise, a directed graph is converted to an undirected
    graph via \code{ugraph} and an undirected graph is returned such
    that each edge is interpreted as two edges, one in each direction.
  }
  \item{ugraph}{Return an undirected version of the current graph.
    Conceptually, the arrows of a graph's directed edges are removed.}
  }
}

\author{S. Falcon, N. Gopalakrishnan}




\examples{
    f <- c("a", "a", "b", "c", "d")
    t <- c("b", "c", "c", "d", "a")
    weight <- rep(1,length(f))
    df <- data.frame(from=f, to=t, weight= weight)
    g <- graphBAM(df)
    gd <- graphBAM(df, edgemode = "directed")
    g
    gd
    nd <- nodes(g)
    w1 <- edgeWeights(g)
    w2 <- edgeWeights(g,"a")
    w3 <- edgeWeights(g,1)
    d1 <- edges(g)
    d2 <- edges(g,c("a", "b"))
    e1 <- edgeData(g)   
    e2 <- edgeData(g, "a", "c",attr="weight")
    em <- edgeMatrix(g) 
    id <- isDirected(g)
    sg <- subGraph(c("a","c","d"), g)
    ft <- graphBAMExtractFromTo(g)
    am <- as(g,"graphAM")
    nl <- as(g,"graphNEL")
    mt <- as(g,"matrix")
}
\keyword{classes}
