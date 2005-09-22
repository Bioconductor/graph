\name{attrDefaults-methods}
\docType{methods}
\alias{attrDefaults-methods}
\alias{attrDefaults<--methods}
\alias{attrDefaults}
\alias{attrDefaults<-}

\title{Get and set the default attributes of an attrData object}

\usage{
attrDefaults(self, attr)
attrDefaults(self, attr) <- value
}
\arguments{
  \item{self}{A \code{\link{attrData-class}} instance}
  \item{attr}{A \code{character} vector of length 1 giving the name of
    an attribute.  Can be missing.}
  \item{value}{An R object that will be used as the default value of the
  specified attribute, or a named list of attribute name/default value
  pairs if \code{attr} is missing.}
}
\description{
  The \code{attrDefaults} method provides access to a
  \code{\link{attrData-class}} object's default attribute list.  The
  default attribute list of a \code{attrData-class} object defines what
  attributes can be customized for individual data elements by
  defining attribute names and default values.
}
\keyword{methods}

