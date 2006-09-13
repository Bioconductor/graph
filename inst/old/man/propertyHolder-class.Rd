\name{propertyHolder-class}
\docType{class}
\alias{propertyHolder-class}
\alias{property}
\alias{property<-}
\alias{property,propertyHolder,character-method}
\alias{property<-,propertyHolder,character-method}

\title{Class "propertyHolder"}
\description{
  This is a virtual class used as a base class for graph edge and node
  classes. This is used to hold named properties associated with objects
  from its super classes.}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
    \item{\code{property}:}{Object of class \code{"list"}, a named list
      of properties.}
  }
}
\section{Methods}{
  \describe{
    \item{property}{\code{signature(x = "propertyHolder", prop =
	"character")}: A method for accessing the property named in
      \code{prop}.}
    \item{property<-}{\code{signature(x = "propertyHolder", prop =
	"character")}: A method for setting the property named in
      \code{prop}.}
  }
}
\author{Saikat DebRoy}
\seealso{\code{\link{gNode-class}} and \code{\link{gEdge-class}} are
  super classes of \code{"propertyHolder"}. The later always holds a
  numeric property named "weight".
}
\examples{
nodeA <- new("gNode", label="A")
nodeB <- new("gNode", label="A")
edgeAB <- new("gEdge", bNode=nodeA, eNode=nodeB)
property(edgeAB, "weight") # default weight is 1.0
property(edgeAB, "weight") <- 2.0
property(edgeAB, "weight")
}
\keyword{classes}
