\name{fromGXL-methods}
\docType{methods}
\title{  Methods for GXL manipulations in package graph }
\alias{fromGXL-methods}
\alias{dumpGXL-methods}
\section{Methods}{
fromGXL,connection-method
\describe{
%\alias{fromGXL,connection-method}
\item{con = connection}{ returns a graphNEL based on a parsing of
the GXL stream on the connection }
}
dumpGXL,connection-method
\describe{
%\alias{dumpGXL,connection-method}
\item{con = connection}{ returns an R list based on a parsing of
the GXL stream on the connection }
}
}
\examples{
sf <- file(system.file("GXL/simpleExample.gxl", package="graph"))
show(fromGXL(sf))
}
\keyword{methods}
\keyword{ models }
