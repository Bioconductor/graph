##Copyright R. Gentleman, 2002, all rights reserved

##code for generating random graphs

##two different methods of generating random graphs on a given set of
##nodes
##M1: randomly generate edges from the choose(|V|, 2) possible edges
##M2: given V and a set of shared attributes M, select for each node
##    of V a subset of M. Then compute edges according to whether
##    nodes share common elements of M


##M2: sample "properties" from M with prob p

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
              pos <- match(j, rval[[i]]$edges)
              if(is.na(pos) ) {
                  rval[[i]]$edges <- c(rval[[i]]$edges, j)
                  ln <-length(rval[[i]]$edges)
                  rval[[i]]$weights <- c(rval[[i]]$weights, 1)
                  names(rval[[i]]$weights)[ln] <- j
              }
              else
                  rval[[i]]$weights[[pos]] <- rval[[i]]$weights[[pos]]+1
          }
      new("graphNEL", nodes = V, edgeL=rval)
}

##  gg<-randomGraph(letters[1:10], 1:4, .3)




##generate edges at random according to some probability
##there are choose(numN, 2) possible edges; to make life simple,
##(but less efficient), we first make up a matrix of all possible node edge
##combinations, then select entries and finally make the graph

randomEGraph <- function(V, p, edges)
{
  numN <- length(V)
  numE <- choose(numN, 2)
  if( !missing(p) && (length(p) !=1 || 0 > p || p > 1 ))
      stop("bad value for p")
  if( !missing(edges) && (length(edges) != 1 || edges < 0 || edges > numE))
      stop("bad value for edges")
  inds <- 1:numN
  fromN <- rep(inds[-numN], (numN-1):1)
  s<- numeric(0)
  for(i in 2:numN) s<-c(s, i:numN)
  ## tmat is a 2 column matrix, the first column is the from node, the second
  ## the to node
  tmat <- cbind(fromN, s)
  if( !missing(p) )
      wh <- sample(c(TRUE, FALSE), numE, TRUE, p=c(p,1-p))
  else if( !missing(edges) )
      wh <- sample(1:numE, edges, FALSE)
  tmat <- tmat[wh,]
  numE <- ifelse( is.logical(wh), sum(wh), length(wh))
  rval <- vector("list", length=numN)
  for( i in 1:numE ) {
      ##first put in from -> to
      rval[[tmat[i,1]]]$edges <- c(rval[[tmat[i,1]]]$edges, tmat[i,2])
      ln <- length(rval[[tmat[i,1]]]$edges)
      rval[[tmat[i,1]]]$weights <- c(rval[[tmat[i,1]]]$weights, 1)
      names(rval[[tmat[i,1]]]$weights)[ln] <- tmat[i,2]
      ##since undirected, put in to -> from
      rval[[tmat[i,2]]]$edges <- c(rval[[tmat[i,2]]]$edges, tmat[i,1])
      ln <- length(rval[[tmat[i,2]]]$edges)
      rval[[tmat[i,2]]]$weights <- c(rval[[tmat[i,2]]]$weights, 1)
      names(rval[[tmat[i,2]]]$weights)[ln] <- tmat[i,1]
  }
  names(rval) <- V
  new("graphNEL", nodes = V, edgeL = rval)
}

## g2 <- randomEGraph(letters[1:10], .2)

randomNodeGraph <- function(nodeDegree)
{
    if( any(nodeDegree < 0 ) )
        stop("only positive degrees allowed")

    numEdge <- sum(nodeDegree)
    if( numEdge %% 2 != 0 )
        stop("sum of degrees must be even")

    wh <- sample(numEdge)
    Nodes <- rep(names(nodeDegree), nodeDegree)
    if( is.null(Nodes) )
        stop("nodes must be named")

    from <- Nodes[wh[1:(numEdge/2)]]
    to <- Nodes[wh[(1+numEdge/2):numEdge]]
    edL <- split(to, from)
    eN <- names(nodeDegree)
    outL <- lapply(edL, function(x) list(edges=match(x, eN),
                                         weights=rep(1, length(x))))

    oL <- vector("list", length=length(eN))
    names(oL) <- eN
    oL[names(outL)] <- outL
    g <- new("graphNEL", nodes=names(oL), edgeL=oL,
             edgemode="directed")
    g
}


