\name{generalGraph-class}
\docType{class}
\alias{generalGraph-class}
\alias{coerce,generalGraph-method}
\alias{initialize,generalGraph-method}
\title{Class "generalGraph" ~~~ }
\description{  ~~ A concise (1-5 lines) description of what the class is.  ~~}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("generalGraph", nodes, edges)}.
}
\section{Slots}{
  \describe{
    \item{\code{nodes}:}{Object of class \code{"hashtable"} }
    \item{\code{edges}:}{Object of class \code{"hashtable"} }
    \item{\code{edgemode}:}{Object of class \code{"character", from class "graph"} }
  }
}
\section{Extends}{
Class \code{"graph"}, directly.
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "graphNEL", to = "generalGraph")}: ... }
    \item{initialize}{\code{signature(.Object = "generalGraph")}: ... }
  }
}
\author{R. Gentleman }


\seealso{  }

\examples{
##---- Should be DIRECTLY executable !! ----
}
\keyword{classes}
