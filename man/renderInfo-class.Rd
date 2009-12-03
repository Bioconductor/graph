\name{renderInfo-class}
\docType{class}
\alias{renderInfo-class}
\alias{edgeRenderInfo}
\alias{edgeRenderInfo<-}
\alias{nodeRenderInfo}
\alias{nodeRenderInfo<-}
\alias{parRenderInfo}
\alias{parRenderInfo<-}
\alias{graphRenderInfo}
\alias{graphRenderInfo<-}

\title{Class "renderInfo"}
\description{A container class to manage graph rendering attributes.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("renderInfo")}
  or by using the initializer \code{.renderInfoPrototype}.
}
\section{Slots}{
  
  \describe{
    
    \item{\code{pars}:}{List of default rendering attributes with two
      items \code{nodes} and \code{edges}. When not set further down the
      parameter hierarchy, these defaults will be used for all
      nodes/edges in the graph. }
    
    \item{\code{nodes}:}{Named list of attributes specific to nodes. }
    
    \item{\code{edges}:}{Named list of attributes specific to edges. }

    \item{\code{graph}:}{Named list of graph-wide attributes. }
    
  }
  
  Each item of \code{nodes} and \code{edges} can take arbitrary
  vectors, the only restriction is that they have to be of either
  length 1 or length equal to the number of nodes or edges,
  respectively.
  
  \code{pars} and \code{graph} can take arbitrary skalars, the latter
  for both edges and nodes. }

\section{Methods}{
  
  The following are functions rather than methods and build the API to
  control the graphical output of a graph when it is plotted using
  \code{\link[Rgraphviz]{renderGraph}}.

  \describe{
 
    \item{parRenderInfo, parRenderInfo<-}{getter and setter for
      items of slot \code{pars}}
    
    \item{nodeRenderInfo, nodeRenderInfo<-}{getter and setter for
      items of slot \code{nodes}}
    
    \item{edgeRenderInfo, edgeRenderInfo<-}{getter and setter for
      items of slot \code{edges}}
    
    \item{graphRenderInfo, graphRenderInfo<-}{getter and setter for
      items of slot \code{graph}}
    
  }
    
  The getters all take two arguments: \code{g} is a graph object and
  \code{name} is a character giving the name of one of the item in the
  respective slot. When \code{name} is missing this will give you the
  whole list.
  
  The setters are a bit more complex: \code{nodeRenderInfo<-} and
  \code{edgeRenderInfo<-} can take

  \describe{
  \item{named list of named vectors}{where the names have to match the node
    or edge names. Items in the vector that don't match a valid edge or
    node name will be silently ignored. For undirected edges the order
    of head nodes and tail nodes in edge names is ignored,
    i.e. \code{a~b} is equivalent to code{b~a}}
  \item{named list of skalars}{which will set all the attribute for all
    edges or nodes in the graph}
  \code{parRenderInfo<-}{will only take a list with items
    \code{nodes}, \code{edges} and \code{graph}. The content of these
    list items can be arbitrary named vectors.}
  \code{parRenderInfo<-}{takes an arbitrary list}
  }

  Available rendering parameters for nodes are:
  
  \describe{
  \item{col:}{ the color of the line drawn as node border. Defaults to
    \code{black}.}
  
  \item{lty:}{ the type of the line drawn as node border. Defaults to
    \code{solid}. Valid values are the same as for the R's base
    graphic parameter \code{lty}.}
  
  \item{lwd:}{ the width of the line drawn as node border. Defaults to
    \code{1}. Note that the underlying low level plotting functions do
    not support vectorized \code{lwd} values. Instead, only the first
    item of the vector will be used. }
  
  \item{fill:}{ the color used to fill a node. Defaults to
    \code{transparent}.}
  
  \item{textCol:}{ the font color used for the node labels. Defaults
    to \code{black}.}
  
  \item{fontsize: }{the font size for the node labels in
    points. Defaults to \code{14}. Note that the fontsize will be
    automatically adjusted to make sure that all labels fit their
    respective nodes. You may want to increase the node size by
    supplying the appropriate layout parameters to \code{Graphviz}
    in order to allow for larger fontsizes.}
  
  \item{cex:}{ Expansion factor to further control the fontsize. As
    default, this parameter is not set, in which case the fontsize
    will be clipped to the node size. This mainly exists to for
    consistency with the base graphic parameters and to override the
    clipping of fontsize to nodesize.}
  }
  
  Available rendering parameters for edges are:
  
  \describe{
  \item{col:}{ the color of the edge line. Defaults to \code{black}.}
  
  \item{lty:}{ the type of the edge line. Defaults to
    \code{solid}. Valid values are the same as for the R's base
    graphic parameter \code{lty}.}
  
  \item{lwd:}{ the width of the edge line. Defaults to \code{1}.}
  
  \item{textCol:}{ the font color used for the edge labels. Defaults
    to \code{black}.}
  
  \item{fontsize:}{ the font size for the edge labels in
    points. Defaults to \code{14}.}
  
  \item{cex:}{ Expansion factor to further control the fontsize. This
    mainly exists to be consistent with the base graphic parameters.}
  }
}

\author{Deepayan Sarkar, Florian Hahne}

\examples{
g <- randomGraph(letters[1:4], 1:3, p=0.8)
nodeRenderInfo(g) <- list(fill=c("a"="red", "b"="green"))
edgeRenderInfo(g) <- list(lwd=3)
edgeRenderInfo(g) <- list(lty=3, col="red")
parRenderInfo(g) <- list(edges=list(lwd=2, lty="dashed"),
nodes=list(col="gray", fill="gray"))
nodeRenderInfo(g)
edgeRenderInfo(g, "lwd")
edgeRenderInfo(g, c("lwd", "col"))
parRenderInfo(g)
}

\keyword{classes}
