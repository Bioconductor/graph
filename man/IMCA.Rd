\name{integrinMediatedCellAdhesion}
\alias{IMCA}
\alias{IMCAAttrs}
\alias{IMCAGraph}
\alias{IMCALocusLink}
\docType{data}
\title{KEGG Integrin Mediated Cell Adhesion graph}
\description{
  A graph representing the integrin-mediated cell adhesion pathway from
  KEGG, as well as a data.frame of attributes for use in plotting the
  graph with \code{Rgraphviz} and a list to compare the nodes with their
  respective LocusLink IDs.  
}
\usage{
data(integrinMediatedCellAdhesion)
}
\details{
  The \code{integrinMediatedCellAdhesion} data set contains three objects:
  
  The first is \code{IMCAGraph}, which is an object of class
  \code{graph-NEL} and represents the hsa04510 graph from \code{KEGG}.

  The second is \code{IMCAAttrs}, which is a data.frame with two columns,
  and a row for every node in \code{IMCAGraph}.  The first column lists
  what color the node is represented with on the \code{KEGG} site.  The
  second column lists the type of the node - either \code{genesym} or
  \code{text}.  Most nodes are of type \code{genesym} as they represent
  genes, but some of the nodes in the \code{KEGG} graph were not genes
  and thus those nodes are of type \code{text}.

  The third, \code{IMCALocusLink} is a named list where the names
  correspond to the node names in \code{IMCAGraph}.  The values of the
  list are the LocusLink IDs that correspond to that node in the KEGG graph.
}
\source{
\url{http://www.genome.ad.jp/kegg/pathway/hsa/hsa04510.html}
}
\examples{
data(integrinMediatedCellAdhesion)
if (require("Rgraphviz") & interactive())
  plot(IMCAGraph)
}
\keyword{datasets}
