##copyright 2002 R. Gentleman, all rights reserved

## a simple implementation of the notions of cluster graphs
## and clustering using distance measures

## for the adjacency matrix view of undirected graphs the
## storage of the graph in lower triangular form is appealing

##  The lower triangle of the distance matrix stored by columns in a
##     single vector.  The vector has the attributes `"Size"', `"Diag"',
##     `"Upper"', `"Labels"' and `"class"' equal to `"dist"'.

## the lower triangle stored by columns
## for i < j <= n the dissimilarity between row i and j is
## x[n*(i-1)-i*(i-1)/2+j-i]
## size is the number of obs
## labels are their labels, Diag and Upper indicate how to print
##

##a function for Martin -- if it ever gets good enough.

##FIXME: should look at other matrix packages; this is just subsetting
##from a lower (or upper) triangular matrix representation -- which
##should be easier...but maybe it needs to be done in C

 "[.dist" <- function(x, i, j, drop=TRUE) {
    lend <- attr(x, "Size")
    if( missing(i) )
      if( missing(j) )
         return(x)
      else { ## return columns
         i <- 1:lend
      }
    if( missing(j) ) {
       j <- 1:lend
    }
    ## we have both -- return a matrix
    leni <- length(i)
    lenj <- length(j)
    outl <- leni*lenj
    iuse <- rep(i, length.out=outl)
    juse <- rep(j, rep(leni, lenj), length.out=outl)

    igtj <- iuse > juse
    newi <- ifelse(!igtj, iuse, juse)
    newj <- ifelse(igtj, iuse, juse)
    lend <- attr(x, "Size")
    subs <- lend*(newi-1)-newi*(newi-1)/2+newj-newi
    zeros <- newi==newj
    rval <- rep(0, length(subs))
    subs <- subs[!zeros]
    sdata <- unclass(x)[subs]
    rval[!zeros]<-sdata
    labels <- attr(x, "Labels")
    if( drop && (lenj == 1 || leni==1) ) {
	out<-rval
        if( leni == 1 )
          names(out) <- labels[j]
        else
          names(out) <- labels[i]
    }
    else {
        out <- matrix(rval, nc=lenj, nr=leni)
        dimnames(out) <- list(labels[i], labels[j])
   }
   out
 }

##rely on .initGraph having been called first to set up the
##classes and generics

.initDistGraph <- function(where)
{

 setClass("distGraph",
     representation( Dist = "dist"), contains="graph", where=where)

 setMethod("nodes", "distGraph", function(object)
      attr(object@Dist, "Labels" ), where=where)

 setGeneric("Dist", function(object) standardGeneric("Dist"), where=where)

 setMethod("Dist", "distGraph", function(object)
    object@Dist, where=where)

  setMethod("show", "distGraph", function(object) {
    cat("distGraph with ", attr(object@Dist, "Size"),
   " nodes \n", sep="")}, where=where)

  setGeneric("threshold", function(object, k)
  standardGeneric("threshold"), where=where)

  setMethod("threshold", "distGraph", function(object, k) {
        nd <- object@Dist
        nd[nd > k ] <- 0
        new("distGraph", Dist=nd)
     }, where=where)


  setMethod("numNodes", "distGraph", function(object)
     attr(Dist(object), "Size"), where=where)

  setMethod("adj", "distGraph", function(object, index) {
        nodenames<- nodes(object)
        if( is.character(index) )
          index <- match(index, nodenames)
        if( !is.numeric(index) )
	   stop("index not recognized")

        adjM <- object@Dist[index,]
        if( is.matrix(adjM) )
           adjL <- split(adjM, row(adjM))
        else
           adjL <- list(adjM)

        for(i in 1:length(adjL) )
          adjL[[i]] <- names(adjL[[i]])[adjL[[i]] > 0 ]
        return(adjL) }, where=where)


  ##acc is graph generic --
  ##setGeneric("acc", function(object, index)
  ##	standardGeneric("acc"))


}


.initClustGraph <- function(where=where) {

 if( !isClass("graph") )
   stop("cannot initialize clusterGraph without graph")

 setClass("clusterGraph",
     representation( clusters = "list"), contains="graph", where=where)

 setMethod("nodes", "clusterGraph", function(object)
    unlist(object@clusters), where=where)

 setMethod("numNodes", "clusterGraph", function(object)
    sum(sapply(object@clusters, length)), where=where)

 setMethod("adj", "clusterGraph", function(object, index)
    for(cl in object@clusters) if( index %in% cl ) cl,
    where=where)

 ##for cluster graphs, acc and adj are the same
 setMethod("acc", "clusterGraph", function(object, index)
      for(cl in object@clusters) if( index %in% cl ) cl,
      where=where)

 setMethod("connComp", "clusterGraph", function(object)
      object@clusters, where=where)
}
