##Copyright R. Gentleman, 2002, all rights reserved

##code for generating random graphs

##two different methods of generating random graphs on a given set of
##nodes
##M1: randomly generate edges from the choose(|V|, 2) possible edges
##M2: given V and a set of shared attributes M, select for each node
##    of V a subset of M. Then compute edges according to whether
##    nodes share common elements of M


## sample nodes from M with prob p

randomGraph <- function(V, M, p, weights = TRUE)
{
    if( any(duplicated(V)) )
       stop("node names must be unique")

    lenM <- length(M)
    lenV <- length(V)
    nSel <- lapply(V, function(x)
                 M[sample(c(TRUE,FALSE), lenM, TRUE, prob=c(p, 1-p))])
    lens <- sapply(nSel, length)
    objs <- unlist(nSel)
    wh <- rep(1:lenV, lens)
    rval <- vector("list",lenV)
    names(rval) <- V
    tmp <- split(wh, objs)
    for( vec in tmp )
      for(i in vec)
       for(j in vec)
          if( i != j ) {
           rval[[i]] <- c(rval[[i]], j)
        }
    rval <- lapply(rval, function(x) {if(is.null(x) ) return(NULL)
                                      cts <- tabulate(x, nbins=lenV)
                                      names(cts) <- 1:lenV
                                      cts <- cts[cts>0]
                         return(list(edges=as.numeric(names(cts)),
                                                  weights=cts))})
    if( !weights )
      rval <- lapply(rval, function(x) {x$weights <- NULL; x})
    new("graphNEL", nodes = V, edgeL=rval)
}




##generate edges at random according to some probability

randomEGraph <- function(V, p)
{
  numN <- length(V)
  numE <- choose(numN, 2)
  rval <- vector("list", length=numN)
  names(rval) <- V
  for( i in 1:(numN-1) ) {
    wh <- sample(c(TRUE, FALSE), numN-i, TRUE, p=c(p, 1-p))
    rval[[i]] <- list(edges = ((i+1):numN)[wh])
  }
  for( i in 1:(numN-1) )
    for( k in rval[[i]]$edges )
      if( k > i ) rval[[k]]$edges <- c(rval[[k]]$edges, i)
  new("graphNEL", nodes = V, edgeL = rval)
}



