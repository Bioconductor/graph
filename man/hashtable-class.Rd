\name{hashtable-class}
\docType{class}
\alias{hashtable-class}
\alias{hash}
\alias{contents}
\alias{hash,ANY,ANY,hashtable-method}
\alias{contents,hashtable-method}
\alias{initialize,hashtable-method}
\title{Class "hashtable"}
\description{ A preliminary implementation of a hash table class}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("hashtable", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{hashtable}:}{Object of class \code{"environment"}. }
  }
}

\section{Methods}{
  \describe{
    \item{contents}{\code{signature(object = "hashtable")}: ... }
    \item{hash}{\code{signature(key = "ANY", value = "ANY", htable = "hashtable")}: ... }
    \item{initialize}{\code{signature(.Object = "hashtable")}: ... }
  }
}
\author{R.Gentleman }


\seealso{  }

\examples{
##---- Should be DIRECTLY executable !! ----
}
\keyword{classes}
