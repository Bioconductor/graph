\name{validGraph}
\alias{validGraph}
\title{ Test whether graph object is valid }
\description{
  validGraph is a validating function for a graph object.
}
\usage{
validGraph(object)
}

\arguments{
  \item{object}{ a graph object to be tested }
}

\value{
 If the graph object is valid, TRUE is returned; else a warning 
 is returned that lists what is wrong with the graph object.
}

\author{ Elizabeth Whalen }

\seealso{\code{\link{graph}} }

\examples{
  
  testGraph<-new("graphNEL")
  testGraph@nodes<-c("node1","node2","node3")
  testGraph@edgeL<-list(node1=c("node2"),node2=c("node1","node3"),
                        node3=c("node2"))
  validGraph(testGraph)

}
\keyword{ manip}
