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
\alias{edgeData,graphBAM,character,character,character-method}
\alias{edgeData,graphBAM,character,missing,character-method}
\alias{edgeData,graphBAM,missing,character,character-method}
\alias{edgeData,graphBAM,missing,missing,missing-method}
\alias{edgeData,graphBAM,missing,missing,character-method}
\alias{edgeData<-,graphBAM,character,character,character-method}
\alias{edgeData<-,graphBAM,missing,character,character-method}
\alias{edgeData<-,graphBAM,character,missing,character-method}
\alias{edgeData<-,graphBAM,missing,missing,character-method}
\alias{edgeData<-,graphBAM,character,character,character,ANY-method}
\alias{edgeData<-,graphBAM,missing,character,character,ANY-method}
\alias{edgeData<-,graphBAM,character,missing,character,ANY-method}
\alias{edgeData<-,graphBAM,missing,missing,character,ANY-method}
\alias{graphBAM}
\alias{coerce,graphBAM,graphAM-method}
\alias{coerce,graphBAM,graphNEL-method}
\alias{coerce,graphBAM,matrix-method}
\alias{extractFromTo,graphBAM-method}
\alias{ugraph,graphBAM-method}
\alias{adj,graphBAM,character-method}
\alias{graphIntersect}
\alias{graphIntersect,graphBAM,graphBAM-method}
\alias{graphUnion}
\alias{graphUnion,graphBAM,graphBAM-method}
\alias{nodes<-,graphBAM,character-method}
\alias{removeEdgesByWeight}
\alias{removeEdgesByWeight,graphBAM-method}
\title{EXPERIMENTAL class "graphBAM"}
\description{
  The graphBAM class represents a graph as an adjacency matrix.  The
  adjacency matrix is stored as a bit array using a \code{raw} vector to
  reduce the memory footprint and speed operations like
  \code{graphIntersection}.  This class is EXPERIMENTAL and its API is
  subject to change.
}

\usage{
graphBAM(df, nodes=NULL, edgemode="undirected", ignore_dup_edges = FALSE)
}

\arguments{
  \item{df}{

    A \code{data.frame} with three columns: "from", "to" and
    "weight". Columns "from" and "to" can be either factors or character
    vectors.  Each row of \code{df} describes an edge in the resulting
    graph.  The "weight" column must be numeric.
    
  }
  
  \item{nodes}{

    A character vector of node labels.  Use this to add degree zero
    nodes to the graph.  If \code{NULL}, the set of nodes found in
    \code{from} and \code{to} will be used.
    
  }

  \item{edgemode}{
    A string, one of "directed" or "undirected".
  }

  \item{ignore_dup_edges}{

    If \code{FALSE} (default), specifying duplicate edges in the input
    is an error.  When set to \code{TRUE} duplicate edges are ignored.
    Edge weight values are ignored when determining duplicates.  This is
    most useful for graph import and conversion.
    
  }
}
\section{Constructors}{
  The \code{GraphBAM} function is used to create new \code{graphBAM}
  instances.  Edges are specified in a \code{data.frame}.  For
  undirected graphs, reciprical edges should not be includes unless
  \code{ignoe_dup_edges} is \code{TRUE}.
}
\section{Extends}{
Class \code{"\linkS4class{graph}"}, directly.
}

\section{Methods for graphBAM objects}{

  \describe{
    
    \item{\code{addEdge(from, to, graph, weights)}}{
      
      Return a new \code{graphBAM} object with the specified edge(s)
      added.  The \code{from} and \code{to} arguments must either be the
      same length or one of them must be of length one.  Each time an
      edge is added, the entire graph is copied.  For the purpose of
      building a graph it will often be more efficient to build up the
      list of edges and call \code{GraphBAM}.

    }
  
    \item{\code{addNode(node, object)}}{
      
      Return a new \code{graphBAM} object with the specified node(s)
      added.
      
    }

    \item{\code{clearNode(node, object)}}{
      
      This operation is not currently supported.
      
    }
    
    \item{\code{edges(object, which)}}{

      Returns an adjacency list representation of the graph.  The list
      will have an entry for each node with a vector of adjacent node
      labels or \code{character(0)}.  For undirected graphs,
      \code{edges} returns the reciprocal edges.  The optional
      argument \code{which} can be a character vector of node labels.
      When present, only entries for the specified nodes will be
      returned.
      
    }
    
    \item{\code{inEdges(node, object)}}{
      (Not yet supported)
      Similar to the \code{edges} function, but the adjacency list maps
      nodes that have an edge to the given node instead of from the
      given node.
    }
    
    \item{\code{isAdjacent(object, from, to)}}{
      Returns a logical vector indicating whether there is an edge
      corresponding to the elements in \code{from} and \code{to}.  These
      vectors must have the same length, unless one has length one.
    }
    
    \item{\code{nodes(object)}}{
      Return the node labels for the graph
    }
    
    \item{\code{numEdges(object)}}{
      Returns the number of edges in the graph.
    }
    
    \item{\code{numNodes(object)}}{
      Returns the number of nodes in the graph
    }
    
    \item{\code{removeEdge(from, to, graph)}}{

      Return a new \code{graphBAM} object with the specified edges
      removed.  The \code{from} and \code{to} arguments must be
      the same length unless one of them has length one.
      
    }
    
    \item{\code{removeNode(node, object)}}{
      Operation not supported.
    }
    
    \item{\code{edgeData(self, from, to, attr)}}{
      Access edge attributes.  See help for \code{edgeData}.
    }
    
    \item{\code{edgeWeights(object, index)}}{
      Return the edge weights for the graph in adjacency list format.
      The optional argument \code{index} specified a character vector of
      nodes.  In this case, only the weights for the specified nodes
      will be returned.
    }
    
    \item{\code{extractFromTo(g)}}{

      Returns a data frame with column names "from", "to", and "weight"
      corresponding to the connected nodes in the graphBAM object.
      
    }

    \item{\code{graphIntersect(x, y, funList)}}{

      When given two \code{graphBAM} objects, \code{graphIntersect}
      returns a new \code{graphBAM} containing the nodes and edges in
      common between the two graphs.  The intersection is computed by
      first finding the intersection of the node sets, obtaining the
      induced subgraphs, and finding the intersection of the resulting
      edge sets. Node/Edge attributes that are equal are carried over to
      the result. Non equal edge/node attributes will result in the 
      corresponding attribute being set to NA. The user has the option 
      of providing a named list of functions for resolving conflicting
      edge attributes.
    }
  
    \item{\code{edgemode(object) <- value}}{
      
      Set the edgemode for the graph ("directed" or "undirected").  If
      the specified edgemode is the same, the object is returned without
      changes.  Otherwise, a directed graph is converted to an
      undirected graph via \code{ugraph} and an undirected graph is
      returned such that each edge is interpreted as two edges, one in
      each direction.
      
    }

    \item{\code{ugraph(graph)}}{

      Return an undirected version of the current graph.  Conceptually,
      the arrows of a graph's directed edges are removed.

    }

    \item{\code{nodes(object) <- value}}{

      Replacement of a \code{graphBAM} object's node labels is currently
      not supported.  An error is raised if this method is called.
      
    }
    
  }
}

\section{Coercion}{

  \code{graphBAM} objects can be coerced to \code{graphAM},
  \code{graphNEL}, and \code{matrix} instances via \code{as(g, CLASS)}.

}

\author{N. Gopalakrishnan, S. Falcon}

\examples{
    f <- c("a", "a", "b", "c", "d")
    t <- c("b", "c", "c", "d", "a")
    weight <- c(2.3, 2.3, 4.3, 1.0, 3.0)
    df <- data.frame(from=f, to=t, weight= weight)
    g <- graphBAM(df)
    gd <- graphBAM(df, edgemode = "directed")
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
    ft <- extractFromTo(g)
    am <- as(g,"graphAM")
    nl <- as(g,"graphNEL")
    mt <- as(g,"matrix")
    k  <- graphIntersect(g,g)
    k <- graphUnion(g,g)
    e <- removeEdgesByWeight(g,lessThan= 3.0)
    g
    gd
}
\keyword{classes}
