\name{apoptosisGraph}
\alias{apopGraph}
\alias{apopAttrs}
\alias{apopLocusLink}
\docType{data}
\title{KEGG apoptosis pathway graph}
\description{
  A graph representing the apoptosis pathway from
  KEGG, as well as a data.frame of attributes for use in plotting the
  graph with \code{Rgraphviz} and a list to compare the nodes with their
  respective LocusLink IDs.  
}
\usage{
data(apopGraph)
}
\details{
  The \code{apopGraph} data set contains three objects:
  
  The first is \code{apopGraph}, which is an object of class
  \code{graph-NEL} and represents the hsa04210 graph from \code{KEGG}.

  The second is \code{apopAttrs}, which is a data.frame with two columns,
  and a row for every node in \code{apopGraph}.  The first column lists
  what color the node is represented with on the \code{KEGG} site.  The
  second column lists the type of the node - either \code{genesym} or
  \code{text}.  Most nodes are of type \code{genesym} as they represent
  genes, but some of the nodes in the \code{KEGG} graph were not genes
  and thus those nodes are of type \code{text}.

  The third, \code{apopLocusLink} is a named list where the names
  correspond to the node names in \code{apopGraph}.  The values of the
  list are the LocusLink IDs that correspond to that node in the KEGG graph.
}
\source{
\url{http://www.genome.ad.jp/kegg/pathway/hsa/hsa04210.html}
}
\examples{
data(apopGraph)
if (require("Rgraphviz") & interactive())
  plot(apopGraph)
}
\keyword{datasets}
