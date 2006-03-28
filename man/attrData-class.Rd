\name{attrData-class}
\docType{class}
\alias{attrData-class}
\alias{attrDataItem<-,attrData,character,character-method}
\alias{attrDataItem,attrData,character,missing-method}
\alias{attrDataItem,attrData,character,character-method}
\alias{attrDefaults<-,attrData,character,ANY-method}
\alias{attrDefaults<-,attrData,missing,list-method}
\alias{attrDefaults,attrData,missing-method}
\alias{attrDefaults,attrData,character-method}
\alias{initialize,attrData-method}
\alias{removeAttrDataItem<-}
\alias{removeAttrDataItem<-,attrData,character,NULL-method}

\title{Class "attrData"}
\description{A container class to manage generic attributes.  Supports
  named attributes with default values with methods for vectorized access.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("attrData", defaults)}.
The \code{defaults} argument should be a named list containing the
initial attribute names and default values.   
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Where custom attribute data is stored}
    \item{\code{defaults}:}{A named list of known attribute names and
      defualt values.}
  }
}
\section{Methods}{
  \describe{
    \item{attrDataItem<-}{\code{signature(self = "attrData", x = "character", attr = "character")}: ... }
    \item{attrDataItem}{\code{signature(self = "attrData", x = "character", attr = "missing")}: ... }
    \item{attrDataItem}{\code{signature(self = "attrData", x = "character", attr = "character")}: ... }
    \item{attrDefaults<-}{\code{signature(self = "attrData", attr = "character", value = "ANY")}: ... }
    \item{attrDefaults<-}{\code{signature(self = "attrData", attr = "missing", value = "list")}: ... }
    \item{attrDefaults}{\code{signature(self = "attrData", attr = "missing")}: ... }
    \item{attrDefaults}{\code{signature(self = "attrData", attr = "character")}: ... }
    \item{initialize}{\code{signature(.Object = "attrData")}: ... }
    \item{removeAttrDataItem}{\code{signature(self="attrData",
        x="character", value="NULL")}: Remove the data associated with
        the key specified by \code{x}.}
  }
}
\author{Seth Falcon}

\examples{
defaultProps <- list(weight=1, color="blue", friends=c("Bob", "Alice"))
adat <- new("attrData", defaults=defaultProps)

## Get all defaults
attrDefaults(adat)

## Or get only a specific attribute
attrDefaults(adat, attr="color")

## Update default weight
attrDefaults(adat, attr="weight") <- 500

## Add new attribute
attrDefaults(adat, attr="length") <- 0

## Asking for the attributes of an element you haven't customized
## returns the defaults
attrDataItem(adat, x=c("n1", "n2"), attr="length")

## You can customize values
attrDataItem(adat, x=c("n1", "n2"), attr="length") <- 5
}
\keyword{classes}
