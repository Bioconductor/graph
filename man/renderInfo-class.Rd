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
    \item{\code{nodes}:}{Named list of attributes specific to nodes.}
    \item{\code{edges}:}{Named list of attributes specific to edges.}
    \item{\code{pars}:}{List of default rendering attributes with two items
      \code{nodes} and \code{edges}}
    Each item of \code{nodes} and \code{edges} can take arbitrary
    vectors, the only restriction is that they have to be of either
    length 1 or length equal to the number of nodes or edges,
    respectively.

    \code{pars} can take arbitrary skalars for both edges and nodes.
  }
}
\section{Methods}{
  The following are functions rather than methods and build the API to
  control the graphical output of a graph when it is plotted using
  \code{\link[Rgraphviz]{renderGraph}}. Please see
  \code{\link{graphRendering}} for more details.
  \describe{
    \item{nodeRenderInfo, nodeRenderInfo<-}{getter and setter for
      items of slot \code{nodes}}
    \item{edgeRenderInfo, edgeRenderInfo<-}{getter and setter for
      items of slot \code{edges}}
    \item{graphRenderInfo, graphRenderInfo<-}{getter and setter for
      items of slot \code{graph}}
    \item{parRenderInfo, parRenderInfo<-}{getter and setter for
      items of slot \code{pars}}
    The getters all take two arguments: \code{g} is a graph object and
    \code{name} is a character giving the name of one of the item in the
    respective slot. When \code{name} is missing this will give you the
    whole list.

    The setters are a bit more complex: \code{nodeRenderInfo<-} and
    \code{edgeRenderInfo<-} can take
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
