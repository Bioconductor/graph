\name{acc-methods}
\docType{methods}
\title{Methods for Accessibility Lists}
\alias{acc-methods}
\alias{acc}
\description{
 This generic function takes an object that inherits from the \code{graph} 
class and a node in that graph and returns a vector containing information
about all other nodes that are accessible from the given node.
}
\value{
  The methods should return an integer vector. The \code{names} of this
vector are the nodes that are accessible from the given node.
The values in the vector indicate how many edges between the given node
and the node in the return vector.
}
\section{Methods}{\describe{
\item{object = graph}{An object of class graph. }
\item{index}{An index corresponding to a node in the graph. }
}}
\keyword{methods}
