\name{toDotR-methods}
\docType{methods}
\alias{toDotR}
\alias{toDotR,graphNEL,character,list,list-method}
\alias{toDotR,graphNEL,character,missing,missing-method}
\alias{toDotR,graphNEL,character,missing,list-method}
\alias{toDotR,graphNEL,missing,missing,missing-method}
\alias{toDotR,graphNEL,missing,missing,list-method}
\alias{toDotR,graphNEL,missing,character,missing-method}
\alias{toDotR,graphNEL,missing,list,list-method}
\alias{toDotR,graphNEL,missing,list,missing-method}
%\S4method{toDotR,graphNEL,character,list,list-method}
%\S4method{toDotR,graphNEL,character,missing,missing-method}
%\S4method{toDotR,graphNEL,character,missing,list-method}
%\S4method{toDotR,graphNEL,missing,missing,missing-method}
%\S4method{toDotR,graphNEL,missing,missing,list-method}
%\S4method{toDotR,graphNEL,missing,character,missing-method}
%\S4method{toDotR,graphNEL,missing,list,list-method}
%\S4method{toDotR,graphNEL,missing,list,missing-method}
\title{ Methods for Function toDotR, using R to generate a dot serialization} 

\description{There are two basic methods of generating
dot (\url{http://www.graphviz.org}) language serializations
of R \code{\link[graph]{graph-class}} structures.  First,
using the \code{\link[Rgraphviz:toDot-methods]{toDot}} methods of the 
Rgraphviz package, the native graphviz agraph-associated methods can be
employed to create the dot serialization.
Second, with the methods described here, R functions can
be used to perform the serialization directly from
the graph data structure, without Rgraphviz.}

\section{Methods}{\describe{

\item{G = "graphNEL", outDotFile = "character", renderList = "list", optList = "list"}{ create dot language descriptionof graph }

\item{G = "graphNEL", outDotFile = "character", renderList = "missing", optList = "missing"}{ create dot language descriptionof graph }

\item{G = "graphNEL", outDotFile = "character", renderList = "missing", optList = "list"}{ create dot language descriptionof graph }

\item{G = "graphNEL", outDotFile = "missing", renderList = "missing", optList = "missing"}{ create dot language descriptionof graph }

\item{G = "graphNEL", outDotFile = "missing", renderList = "missing", optList = "list"}{ create dot language descriptionof graph }

\item{G = "graphNEL", outDotFile = "missing", renderList = "character", optList = "missing"}{ create dot language descriptionof graph }

\item{G = "graphNEL", outDotFile = "missing", renderList = "list", optList = "list"}{ create dot language descriptionof graph }

\item{G = "graphNEL", outDotFile = "missing", renderList = "list", optList = "missing"}{ create dot language descriptionof graph }

\item{G = "compoundGraph", outDotFile = "character", renderList = "list", optList = "missing"}{ create dot language descriptionof graph }

\item{G = "compoundGraph", outDotFile = "character", renderList = "list", optList = "list"}{ create dot language descriptionof graph }

\item{G = "compoundGraph", outDotFile = "missing", renderList = "list", optList = "missing"}{ create dot language descriptionof graph }
}}
\seealso{\code{\link[Rgraphviz]{toDot-methods}}}
\examples{
example(randomGraph)
tmp <- tempfile()
toDotR( g1, tmp )
readLines(tmp)
unlink(tmp)
}
\keyword{methods}
