\name{biocRepos}
\alias{biocRepos}
\docType{data}
\title{A graph representing the Bioconductor package repository}
\description{
  This graph is a rendition of the Bioconductor package repository and
  represents the dependency graph of that repository.  An edge between
  two package denotes a dependency on the 'to' package by the 'from'
  package.  
}
\usage{data(biocRepos)}
\format{
  The format is:
 list()
 - attr(*, "nodes")= chr [1:76] "AnnBuilder" "Biobase" "XML" "reposTools" ...
 - attr(*, "edgeL")=List of 76
  ..$ AnnBuilder          :List of 2
  .. ..$ edges  : num [1:2] 2 3
  .. ..$ weights: num [1:2] 1 1
  ..$ Biobase             :List of 2
  .. ..$ edges  : num 4
  .. ..$ weights: num 1
  ..$ XML                 :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ reposTools          :List of 2
  .. ..$ edges  : num 50
  .. ..$ weights: num 1
  ..$ DynDoc              :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ MAGEML              :List of 2
  .. ..$ edges  : num [1:6] 7 8 9 10 2 3
  .. ..$ weights: num [1:6] 1 1 1 1 1 1
  ..$ marrayClasses       :List of 2
  .. ..$ edges  : num [1:2] 50 2
  .. ..$ weights: num [1:2] 1 1
  ..$ marrayInput         :List of 2
  .. ..$ edges  : num [1:4] 7 50 9 10
  .. ..$ weights: num [1:4] 1 1 1 1
  ..$ tcltk               :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ tkWidgets           :List of 2
  .. ..$ edges  : num [1:2] 75 5
  .. ..$ weights: num [1:2] 1 1
  ..$ MeasurementError.cor:List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ RBGL                :List of 2
  .. ..$ edges  : num 13
  .. ..$ weights: num 1
  ..$ graph               :List of 2
  .. ..$ edges  : num [1:2] 52 3
  .. ..$ weights: num [1:2] 1 1
  ..$ ROC                 :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ RdbiPgSQL           :List of 2
  .. ..$ edges  : num 16
  .. ..$ weights: num 1
  ..$ Rdbi                :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ Ruuid               :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ SAGElyzer           :List of 2
  .. ..$ edges  : num [1:4] 1 10 19 20
  .. ..$ weights: num [1:4] 1 1 1 1
  ..$ genefilter          :List of 2
  .. ..$ edges  : num [1:3] 2 47 31
  .. ..$ weights: num [1:3] 1 1 1
  ..$ annotate            :List of 2
  .. ..$ edges  : num [1:4] 2 3 4 31
  .. ..$ weights: num [1:4] 1 1 1 1
  ..$ SNPtools            :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ affyPLM             :List of 2
  .. ..$ edges  : num [1:2] 23 24
  .. ..$ weights: num [1:2] 1 1
  ..$ affy                :List of 2
  .. ..$ edges  : num [1:5] 2 25 26 4 10
  .. ..$ weights: num [1:5] 1 1 1 1 1
  ..$ affydata            :List of 2
  .. ..$ edges  : num 23
  .. ..$ weights: num 1
  ..$ eda                 :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ modreg              :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ affycomp            :List of 2
  .. ..$ edges  : num [1:2] 26 2
  .. ..$ weights: num [1:2] 1 1
  ..$ annaffy             :List of 2
  .. ..$ edges  : num [1:4] 2 9 29 30
  .. ..$ weights: num [1:4] 1 1 1 1
  ..$ GO                  :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ KEGG                :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ ctest               :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ ctc                 :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ daMA                :List of 2
  .. ..$ edges  : num 34
  .. ..$ weights: num 1
  ..$ MASS                :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ edd                 :List of 2
  .. ..$ edges  : num [1:6] 36 2 37 38 39 31
  .. ..$ weights: num [1:6] 1 1 1 1 1 1
  ..$ golubEsets          :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ class               :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ nnet                :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ xtable              :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ externalVector      :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ factDesign          :List of 2
  .. ..$ edges  : num [1:5] 23 20 2 42 43
  .. ..$ weights: num [1:5] 1 1 1 1 1
  ..$ mva                 :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ multtest            :List of 2
  .. ..$ edges  : num 31
  .. ..$ weights: num 1
  ..$ gcrma               :List of 2
  .. ..$ edges  : num [1:5] 23 24 34 45 46
  .. ..$ weights: num [1:5] 1 1 1 1 1
  ..$ hgu95av2probe       :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ matchprobes         :List of 2
  .. ..$ edges  : num [1:4] 2 23 64 45
  .. ..$ weights: num [1:4] 1 1 1 1
  ..$ survival            :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ geneplotter         :List of 2
  .. ..$ edges  : num [1:3] 20 2 31
  .. ..$ weights: num [1:3] 1 1 1
  ..$ globaltest          :List of 2
  .. ..$ edges  : num 50
  .. ..$ weights: num 1
  ..$ methods             :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ gpls                :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ cluster             :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ hexbin              :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ limma               :List of 2
  .. ..$ edges  : num [1:7] 50 26 34 55 56 57 58
  .. ..$ weights: num [1:7] 1 1 1 1 1 1 1
  ..$ splines             :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ nls                 :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ nlme                :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ sma                 :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ makecdfenv          :List of 2
  .. ..$ edges  : num [1:2] 2 23
  .. ..$ weights: num [1:2] 1 1
  ..$ marrayNorm          :List of 2
  .. ..$ edges  : num [1:5] 7 8 50 26 61
  .. ..$ weights: num [1:5] 1 1 1 1 1
  ..$ stepfun             :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ marrayPlots         :List of 2
  .. ..$ edges  : num [1:5] 7 50 26 8 60
  .. ..$ weights: num [1:5] 1 1 1 1 1
  ..$ marrayTools         :List of 2
  .. ..$ edges  : num [1:5] 10 2 60 20 19
  .. ..$ weights: num [1:5] 1 1 1 1 1
  ..$ hgu95av2cdf         :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ ontoTools           :List of 2
  .. ..$ edges  : num [1:9] 50 13 66 67 68 69 70 29 2
  .. ..$ weights: num [1:9] 1 1 1 1 1 1 1 1 1
  ..$ SparseM             :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ combinat            :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ Rgraphviz           :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ humanLLMappings     :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ Iyer517             :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ pamr                :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ siggenes            :List of 2
  .. ..$ edges  : num [1:2] 26 55
  .. ..$ weights: num [1:2] 1 1
  ..$ splicegear          :List of 2
  .. ..$ edges  : num [1:4] 2 3 20 74
  .. ..$ weights: num [1:4] 1 1 1 1
  ..$ grid                :List of 2
  .. ..$ edges  : num(0) 
  .. ..$ weights: num(0) 
  ..$ widgetTools         :List of 2
  .. ..$ edges  : num 9
  .. ..$ weights: num 1
  ..$ vsn                 :List of 2
  .. ..$ edges  : num [1:4] 2 23 24 60
  .. ..$ weights: num [1:4] 1 1 1 1
 - attr(*, "edgemode")= chr "directed"
 - attr(*, "class")= atomic [1:1] graphNEL
  ..- attr(*, "package")= chr "graph"
}
\source{
  This graph was generated by the function
  \code{\link{buildPkgDepGraph}} in the \code{reposTools} package.
}
\examples{
data(biocRepos)
}
\keyword{datasets}
