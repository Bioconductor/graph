\name{fromGXL-methods}
\docType{methods}
\title{  Methods for GXL manipulations in package graph }
\alias{fromGXL-methods}
\alias{toGXL-methods}
\alias{GXL}
\alias{dumpGXL-methods}
\alias{dumpGXL}
\alias{fromGXL}     
\alias{gxlTreeNEL}
\alias{NELhandler}  
\alias{toGXL}      
\alias{validateGXL}
\alias{connection-class}
\alias{file-class}

\description{GXL \url{www.gupro.de/GXL} is "an XML sublanguage designed
to be a standard exchange format for graphs".  This document describes
tools in the graph package for importing GXL data to R and for
writing graph data out as GXL.}
\value{
\item{fromGXL}{currently returns a graphNEL when possible.  This
function is based on xmlEventParse with handlers defined
in the function NELhandler.  The dump()
element of this handler should emit information on
all children of nodes and edges; the asGraphNEL()
element will return a graphNEL object with weights
if child <attr> with name attribute "weights" is
present for each edge element.}
\item{toGXL}{for an
input of class "graphNEL", returns an object of class c("XMLInternalDOM", "XMLOutputStream");
see the example for how to convert this to a text stream encoding XML}
\item{dumpGXL}{returns an R list with all the node, edge, and named attribute
information specified in the GXL stream}  
\item{validateGXL}{returns
silently (invisibly returns the parsed tree) for a DTD-compliant stream, or is otherwise very noisy}
}
\section{Methods}{
\describe{
\item{fromGXL}{ con = connection: returns a graphNEL based on a parsing of
the GXL stream on the connection }
\item{dumpGXL}{ con = connection: returns an R list based on a parsing of
the GXL stream on the connection }
\item{validateGXL}{ con = connection: checks the GXL stream against its DTD}
\item{toGXL}{ object = graphNEL: creates an XMLInternalDOM representing the graph in GXL}
}
}
\author{Vince Carey <stvjc@channing.harvard.edu>}
\note{At present, toGXL does not return a validating GXL stream
because XML package does not properly handle the dtd and namespaces
arguments to xmlTree.  This is being repaired.  To fix
the stream, add
\code{
<!DOCTYPE gxl SYSTEM "http://www.gupro.de/GXL/gxl-1.0.1.dtd">
}
as second record in the output.

Some structures in a graphNEL and some tags in GXL may not be handled
at this time.

Note added 18 Jul 2003: It has been observed that graphviz dot2gxl generates
GXL with graph attribute edgeids=FALSE.  fromGXL will fail to process
the edges correctly.  You can manually add arbitrary edgeids to the dot2gxl
output edges and fromGXL will succeed.  Perhaps fromGXL can be made less
dependent on edge id attributes.
}
\examples{
sf <- file(system.file("GXL/simpleExample.gxl", package="graph"))
show(fromGXL(sf))
print(dumpGXL(sf))
validateGXL(sf)
bad <- file(system.file("GXL/c2.gxl", package="graph"))
# here's how you can check if the GXL is well-formed
#
# try( validateGXL(bad) )
#
gR <- new("graphNEL", nodes=letters[1:4], edgeL=list(
 a=list(edges=4), b=list(edges=3), c=list(edges=c(2,1)), d=list(edges=1)),
 edgemode="directed")
#
# following requires that you are using XML bound with recent libxml2
#
if (libxmlVersion()$major>=2 && libxmlVersion()$minor>=4)
  cat(saveXML(toGXL(gR)$value()))
wtd <- file(system.file("GXL/kmstEx.gxl", package="graph"))
wtdg <- fromGXL(wtd)
print(edgeWeights(wtdg))
}
\keyword{methods}
\keyword{ models }
