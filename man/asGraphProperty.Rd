\name{asGraphProperty}
\alias{asGraphProperty}
\alias{asGraphProperty-methods}
\alias{asGraphProperty,list,missing-method}
\alias{asGraphProperty,numeric,missing-method}
\alias{asGraphProperty,list,logical-method}
\alias{asGraphProperty,numeric,logical-method}
\title{Generic for converting input to graph property list}
\description{
  This is a generic that returns converts its input to a value suitable
  for the \code{property} slot of the \code{\link{propertyHolder-class}}
  virtual class.
}
\usage{
asGraphProperty(x, hasWeight)
}
\arguments{
  \item{x}{}
  \item{hasWeight}{\code{TRUE} or \code{FALSE} indicating that the
    property list must have an element named "weight"}
}
\details{
  If \code{x} is a list, then all of its elements must be named. If it
  is a numeric vector, it is converted to a list (using
  \code{as.list}). If it is originally numeric, is of length one and has
  no names then it is converted to a list with a single element named
  "weight".
}
\value{
  A named list.
}
\author{Saikat DebRoy}
\seealso{\code{\link{propertyHolder-class}}}
\examples{
asGraphProperty(2.0, hasWeight=TRUE)
asGraphProperty(c(a=1, b=2))
}
\keyword{methods}
