\name{graph.par}
\alias{graph.par}
\alias{graph.par.get}
\title{Graphical parameters and other settings }
\description{
  Functions providing an interface to persistent graphical parameters
  and other settings used in the package. 
}
  
\usage{
graph.par(...)
graph.par.get(name)
}

\arguments{
  \item{\dots}{ either character strings naming parameters whose values
    are to be retrieved, or named arguments giving values that are to be
    set. }
  \item{name}{ character string, giving a valid parameter name. }
}

\details{
  \code{graph.par} works sort of like \code{\link{par}}, but the details
  are yet to be decided.

  \code{graph.par.get(name)} is equivalent to \code{graph.par(name)[[1]]}
}

\value{

  In query mode, when no parameters are being set, \code{graph.par}
  returns a list containing the current values of the requested
  parameters.  When called with no arguments, it returns a list with all
  parameters.  When a parameter is set, the return value is a list
  containing previous values of these parameters.

}

\seealso{
  \code{\link{par}}
}

\author{ Deepayan Sarkar, \email{deepayan.sarkar@r-project.org} }
\keyword{utilities}
