\name{attrDataItem-methods}
\docType{methods}
\alias{attrDataItem-methods}
\alias{attrDataItem<--methods}
\alias{attrDataItem}
\alias{attrDataItem<-}

\title{Get and set attributes values for items in an attrData object}

\usage{
attrDataItem(self, x, attr)
attrDataItem(self, x, attr) <- value
}
\arguments{
  \item{self}{A \code{\link{attrData-class}} instance}
  
  \item{x}{A \code{character} vector of item names}
  
  \item{attr}{A \code{character} vector of length 1 giving the attribute
  name to get/set.  Note that the attribute name must have already been
  defined for the \code{attrData} object via
  \code{\link{attrDefaults}}.  If missing, return a list of all
  attributes for the specified nodes.}

  \item{value}{An R object to set as the attribute value for the
  specified items.  If the object has length one or does not have a
  length method defined, it will be assigned to all items in \code{x}.
  If the length of \code{value} is the same as \code{x}, the
  corresponding elements will be assigned.  We will add an argument to
  indicate that the \code{value} is to be taken as-is for those cases
  where the lengths are the same coincidentally.}
}

\description{
 The \code{attrDataItem} method provides get/set access to items stored
 in a \code{\link{attrData-class}} object.
}
\keyword{methods}

