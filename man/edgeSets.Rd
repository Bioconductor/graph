\name{edgeSets}
\alias{esetsFemale}
\alias{esetsMale}
\docType{data}
\title{MultiGraph edgeSet data}
\description{

C57BL/6J and C3H/HeJ mouse strains exhibit different cardiovascular and 
metabolic phenotypes on the  hyperlipidemic apolipoprotein E (Apoe) null background.
The interaction data for the genes from adipose, brain, liver and muscle tissue 
samples from male and female mice are included as a list of \code{data.frame}s.
Each \code{data.frame} contains information for the \code{from-gene}, \code{to-gene}
and the strength of interaction (\code{weight}) for each of the tissues studied.

}
\usage{
    data(esetsFemale)
    data(esetsMale)
}
\source{

Sage Commons Repository
   \url{http://sagebase.org/commons/dataset1.php#UCLA1}
 }
\examples{
data(esetsFemale)
data(esetsMale)

}
\keyword{datasets}

