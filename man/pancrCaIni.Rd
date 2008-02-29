\name{pancrCaIni}
\alias{pancrCaIni}
\non_function{}
\title{ A graph encoding parts of the pancreatic
cancer initiation pathway }
\description{ A graph encoding parts of the pancreatic
cancer initiation pathway }
\usage{data(pancrCaIni)}
\format{
  The format is:
Formal class 'graphNEL' [package "graph"] with edgemode "directed".
}
\source{
The KEGG pancreatic cancer pathway was visually inspected on 17 Sept 2007
and a subgraph was isolated.  The HUGO names for
each symbol in the KEGG visualization were obtained and checked for
existance on hgu95av2.  Some abbreviated terms for processes are also included
as nodes.
}
%\references{
%   
%}
\examples{
data(pancrCaIni)
if (require(Rgraphviz)) {
   nat = rep(FALSE, length(nodes(pancrCaIni)))
   names(nat) = nodes(pancrCaIni)
   plot(pancrCaIni, nodeAttrs=list(fixedsize=nat))
   }
}
\keyword{models}
