\name{integrinMediatedCellAdhesion}
\alias{IMCA}
\alias{IMCAAttrs}
\alias{IMCAGraph}
\alias{integrinMediatedCellAdhesion}
\docType{data}
\title{KEGG Integrin Mediated Cell Adhesion graph}
\description{
  A graph representing the integrin-mediated cell adhesion pathway from
  KEGG, as well as a list of attributes for use in plotting the
  graph with \code{Rgraphviz}.  
}
\usage{
data(integrinMediatedCellAdhesion)
}
\details{
  The \code{integrinMediatedCellAdhesion} data set contains two objects:
  
  The first is \code{IMCAGraph}, which is an object of class
  \code{graph-NEL} and represents the hsa04510 graph from \code{KEGG}.


  The second is \code{IMCAAttrs}, which is a list of four elements.  The
  first element, \code{defAttrs} corresponds to the \code{attrs}
  arguments of \code{\link{agopen}} and \code{\link{plot.graph}}.  The
  second element is \code{nodeAttrs} which corresponds to the
  \code{nodeAttrs} argument in the same two functions from
  \code{Rgraphviz}.  The third element, \code{subGList} corresponds to
  the \code{subGList} argument in those functions.  Lastly, the fourth
  element, \code{LocusLink} provides a named list where the names are
  the nodes and the values are vectors of LocusLink ID values which
  correspond to those nodes.
  
  The values from \code{defAttrs}, \code{nodeAttrs} and \code{subGList}
  in the \code{IMCAAttrs} list are part of an ongoing attempt by
  Bioconductor to provide the set of options to most accurately recreate
  the actual visual image of the pathway from the KEGG site using
  \code{Rgraphviz}.  Users may try out their own combination of
  attributes and settings for their own needs, but these represent our
  own efforts at as closely recreating the image as possible.
}
\source{
\url{http://www.genome.ad.jp/kegg/pathway/hsa/hsa04510.html}
}
\examples{
data(integrinMediatedCellAdhesion)
if (require("Rgraphviz") & interactive())
  plot(IMCAGraph, attrs=IMCAAttrs$defAttrs,
       nodeAttrs=IMCAAttrs$nodeAttrs, subGList=IMCAAttrs$subGList)
}
\keyword{datasets}
