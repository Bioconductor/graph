\name{MAPKsig}
\alias{MAPKsig}
\non_function{}
\title{ A graph encoding parts of the MAPK signaling pathway }
\description{ A graph encoding parts of the MAPK signaling pathway }
\usage{data(MAPKsig)}
\format{
  The format is:
Formal class 'graphNEL' [package "graph"] with edgemode "directed".
}
\source{
The KEGG pancreatic cancer pathway was visually inspected on 17 Sept 2007, and the
subgraph associated with MAPK signaling was isolated.  The HUGO names for
each symbol in the KEGG visualization were obtained and checked for
existance on hgu95av2.  Some abbreviated terms for processes are also included
as nodes.
}
%\references{
%   
%}
\examples{
data(MAPKsig)
if (require(Rgraphviz)) {
   nat = rep(FALSE, length(nodes(MAPKsig)))
   names(nat) = nodes(MAPKsig)
   plot(MAPKsig, nodeAttrs=list(fixedsize=nat))
   }
}
\keyword{models}
