\name{MultiGraph-class}
\Rdversion{1.1}
\docType{class}
\alias{MultiGraph-class}
\alias{nodes,MultiGraph-method}
\alias{numEdges,MultiGraph-method}
\alias{numEdges,MGEdgeSet-method}
\alias{numNodes,MultiGraph-method}
\alias{show,MultiGraph-method}
\alias{MultiGraph}
\alias{eweights}
\alias{edgeSetIntersect0}
\alias{edgeSetUnion0}
\alias{subsetEdgeSets}
\alias{extractFromTo}
\alias{extractFromTo,MultiGraph-method}
\alias{extractGraphAM}
\alias{extractGraphBAM}
\alias{isDirected,MultiGraph-method}
\alias{ugraph,MultiGraph-method}
% these are not supposed to be publicly available
% but aliased here to quiet R CMD check
\alias{isDirected,DiEdgeSet-method}
\alias{isDirected,UEdgeSet-method}
\alias{ugraph,DiEdgeSet-method}
\alias{ugraph,UEdgeSet-method}
\alias{graphIntersect,MultiGraph,MultiGraph-method}
\alias{graphUnion,MultiGraph,MultiGraph-method}
\alias{nodeData,MultiGraph,character,character-method}
\alias{nodeData,MultiGraph,character,missing-method}
\alias{nodeData,MultiGraph,missing,character-method}
\alias{nodeData,MultiGraph,missing,missing-method}
\alias{nodeData<-,MultiGraph,character,character-method}
\alias{nodeData<-,MultiGraph,missing,character-method}
\alias{mgEdgeDataDefaults}
\alias{mgEdgeDataDefaults<-}
\alias{mgEdgeDataDefaults,MultiGraph,character,character-method}
\alias{mgEdgeDataDefaults,MultiGraph,character,missing-method}
\alias{mgEdgeDataDefaults<-,MultiGraph,character,character,ANY-method}
\alias{mgEdgeDataDefaults<-,MultiGraph,character,missing,list-method}
\alias{nodeDataDefaults,MultiGraph,character-method}
\alias{nodeDataDefaults,MultiGraph,missing-method}
\alias{nodeDataDefaults<-,MultiGraph,character,ANY-method}
\alias{nodeDataDefaults<-,MultiGraph,missing,list-method}
\alias{mgEdgeData}
\alias{mgEdgeData<-}
\alias{mgEdgeData,MultiGraph,character,character,character,character-method}
\alias{mgEdgeData,MultiGraph,character,character,missing,character-method}
\alias{mgEdgeData,MultiGraph,character,missing,character,character-method}
\alias{mgEdgeData,MultiGraph,character,missing,missing,character-method}
\alias{mgEdgeData<-,MultiGraph,character,character,character,character-method}
\alias{mgEdgeData<-,MultiGraph,character,character,missing,character-method}
\alias{mgEdgeData<-,MultiGraph,character,missing,character,character-method}
\alias{mgEdgeData<-,MultiGraph,character,missing,missing,character-method}
\alias{edges,MultiGraph-method}
\alias{edgeNames,MultiGraph-method}
\alias{edgeSets}
\alias{edgeSets,MultiGraph-method}
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
MultiGraph(edgeSets, nodes = NULL, directed = TRUE, ignore_dup_edges = FALSE)
eweights(object, names.sep = NULL)
edgeSetIntersect0(g, edgeFun = NULL)
edgeSetIntersect0(g, edgeFun = NULL)
extractGraphAM(g, edgeSets)
extractGraphBAM(g, edgeSets)
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

  \item{g}{
    A \code{MultiGraph} instance
  }

  \item{names.sep}{
    
    The string to use as a separator between from and to node labels.
    If \code{NULL} no names will be attached to the returned vector.
    
  }

  \item{ignore_dup_edges}{

    If \code{FALSE} (default), specifying duplicate edges in the input
    is an error.  When set to \code{TRUE} duplicate edges are ignored.
    Edge weight values are ignored when determining duplicates.  This is
    most useful for graph import and conversion.
  }
  \item{edgeFun}{
    A user specified named list of functions to resolve edge attributes in a union
    or intersection operation
  } 

}
\section{Constructors}{
  \code{MultiGraph}
}
\section{Methods}{
  \describe{

    \item{nodes}{Return the nodes of the multigraph.}

    \item{numEdges}{Return an integer vector named by edge set
      containing edge counts for each edge set.}

    \item{numNodes}{Return the number of nodes in the multigraph.}

    \item{eweights}{Return a list named by edge set; each element is a
      numeric vector of edge weights for the corresponding edge set.}

    \item{isDirected}{Return a logical vector named by the edge sets in
      \code{object} with a \code{TRUE} indicating a directed edge set
      and \code{FALSE} for undirected.}

    \item{edges}{Returns a list named by edge set; for the edges in the 
        MultiGraph}

    \item{edgeNames}{Returns a list named by the edge set; for the names
        of the edges in the MultiGraph}

    \item{extractFromTo}{Return a list named by the edge sets; each
      element is a data frame with column names from, to and weight
	  corresponding to the connected nodes in the edge set.}

    \item{subsetEdgeSets}{Return a new \code{MultiGraph} object 
	  representing the subset of edge sets from the original 
      \code{MultiGraph}.}

    \item{extractGraphAM}{Return a named \code{list} of \code{graphAM}
      objects corresponding to the edge sets from the original 
      \code{MultiGraph}.}
  
    \item{extractGraphBAM}{Return a named \code{list} of \code{graphBAM}
      objects corresponding to the edge sets from the original 
      \code{MultiGraph}.}

    \item{ugraph}{Return a new \code{MultiGraph} object in which all
      edge sets have been converted to undirected edge sets.  This
      operation sets all edge weights to one and drops other edge
      attributes.}

    \item{edgeSetIntersect0}{Return a new \code{MultiGraph} object
    representing the intersection of edges across all edge sets within
    \code{g}.  The return value will have a single edge set if the edge
    sets in \code{g} are disjoint.  Otherwise, there will be a single
    edge set containing the shared edges.  The node set is preserved.
    Edge weights and edge attributes are transferred over to the output if they
    have the same value, else user has the option of providing a function
    to resolve the conflict.}

    \item{edgeSetUnion0}{Return a new \code{MultiGraph} object
    representing the union of edges across all edge sets within
    \code{g}.  The node set is preserved.  Edge weights and edge attributes are
    transferred over to the output if they have the same value, else user has
    the option of providing a function to resolve the conflict.}

    \item{\code{graphIntersect(x, y, nodeFun, edgeFun)}}{
    When given two \code{MultiGraph} objects, \code{graphIntersect}
    returns a new \code{MultiGraph} containing the nodes and edges in
    common between the two graphs.  The intersection is computed by
    first finding the intersection of the node sets, obtaining the
    induced subgraphs, and finding the intersection of the resulting
    edge sets. The corresponding named edgeSets in \code{x} and \code{y} should
    both be either directed or undirected.Node/Edge attributes that are equal
    are carried over to the result. Non equal edge/node attributes will result 
    in the corresponding attribute being set to NA. The user has the option 
    of providing a named list(names of edgeSets) of list (names of edge attributes)
    of edge functions correspoding to the names of the edge attributes for 
    resolving conflicting edge attributes (\code{edgeFun}). For resolving any 
    of the conflicting node attributes, the user has the option of providing a
    named \code{list} of functions corresponding to the node attribute names (\code{nodeFun}). 
    }

    \item{\code{graphUnion(x, y, nodeFun, edgeFun)}}{
    When given two \code{MultiGraph} objects, \code{graphUnion}
    returns a new \code{MultiGraph} containing the union of nodes and 
    edges between the two graphs. The corresponding pairs of named edgeSets
    in  \code{x} and \code{y} should both be either directed or undirected. 
    Non equal edge/node attributes will result in the corresponding attribute 
    being set to NA. The user has the option of providing a named list(names of
    edgeSets) of list (names of edge attributes) of edge functions correspoding
    to the names of the edge attributes for resolving conflicting edge 
    attributes (\code{edgeFun}). For resolving any of the conflicting node attributes, the user 
    has the option of providing a named \code{list} of functions corresponding 
    to the node attribute names (\code{nodeFun}). 


    }
    \item{ \code{edgeSets(object, ...)}}{
    Returns the names of the  edgeSets in the  MultiGraph
    \code{object} as a character vector.
    } 
    

    \item{show}{Prints a short summary of a MultiGraph object}
  }
}

\author{S. Falcon, Gopalakrishnan N}

\examples{
    ft1 <- data.frame(from=c("a", "a", "a", "b", "b"),
                        to=c("b", "c", "d", "a", "d"),
                      weight=c(1, 3.1, 5.4, 1, 2.2),
                      stringsAsFactors = TRUE)

    ft2 <- data.frame(from=c("a", "a", "a", "x", "x", "c"),
                        to=c("b", "c", "x", "y", "c", "a"),
                      weight=c(3.4, 2.6, 1, 1, 1, 7.9),
                      stringsAsFactors = TRUE)

    esets <- list(es1=ft1, es2=ft2)

    g <- MultiGraph(esets)

    nodes(g)
    numEdges(g)
    eweights(g)
    eweights(g, names.sep = "=>")
    isDirected(g)
    edges(g, edgeSet ="es1")
    edges(g, "a", "es1")
    edgeNames(g, "es2")
    edgeSets(g)
    ug <- ugraph(g)
    isDirected(ug)
    numEdges(ug)
    edgeSetIntersect0(g)
    subsetEdgeSets(g, "es1")
    extractFromTo(g)
    extractGraphAM(g)
    extractGraphAM(g, "es1")
    extractGraphBAM(g, "es1")
    graphIntersect(g, g)
    graphUnion(g,g)
    mgEdgeDataDefaults(g, "es1", attr = "color" ) <- "white"
    mgEdgeData(g, "es1", from = "a", to = c("b", "c"), attr = "color") <- "red"
    mgEdgeData(g, "es1", from = "a", to = c("b", "c"), attr = "color") 
    nodeDataDefaults(g, attr ="shape") <- "circle"
    nodeData(g, n = c("a", "b", "c"), attr = "shape") <- "triangle"
    nodeData(g, n = c("a", "b", "x", "y"), attr = "shape") 
}
\keyword{classes}
