\name{hashtable}
\alias{hashtable}
\title{Create a hashtable}
\description{
  This function returns a new hashtable object. At present we use
  \code{environment} objects as hashtables.
}
\usage{
hashtable()
}
\value{
  An object of mode \code{environment}.
}
\author{Saikat DebRoy}
\examples{
h <- hashtable()
h[['a']] <- 1
h[['a']]
}
\keyword{environment}
