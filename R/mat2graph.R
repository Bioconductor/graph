aM2bpG<-function(aM){

	if(is.null(rownames(aM))) stop("aM must have row names.")
	if(is.null(colnames(aM))) stop("aM must have column names.")

	V <- c(rownames(aM),colnames(aM))

	tmat<-which(aM>0,arr.ind=TRUE)
	tmat[,2] <- tmat[,2] + dim(aM)[1]
	numN<-length(V)
	numE<-dim(tmat)[1]

	rval <- vector("list", length = numN)
    	for (i in 1:numE) {
        	rval[[tmat[i, 1]]]$edges <- c(rval[[tmat[i, 1]]]$edges,
            	tmat[i, 2])
        	ln <- length(rval[[tmat[i, 1]]]$edges)
        	rval[[tmat[i, 1]]]$weights <- c(rval[[tmat[i, 1]]]$weights,
            	aM[tmat[i,1],(tmat[i,2]-dim(aM)[1])])
        	names(rval[[tmat[i, 1]]]$weights)[ln] <- tmat[i, 2]
	}
    	names(rval) <- V
    	g1<-new("graphNEL", nodes = V, edgeL = rval)
	g1@edgemode<-"directed"
	return(g1)
}

## WH 23 June 2004
ftM2adjM <- function(ft, W=NULL, V=NULL, edgemode="directed")
  .ftM2other(ft, W, V, edgemode, "adjM")

ftM2graphNEL <- function(ft, W=NULL, V=NULL, edgemode="directed")
  .ftM2other(ft, W, V, edgemode, "graphNEL")

.ftM2other <- function(ft, W, V, edgemode, targetclass) {
   ## ft should be a 2xn or nx2 matrix. If nx2, transpose
   if(!is.matrix(ft) || !any(dim(ft)==2))
     stop("'ft' must be a 2xn or nx2 matrix")
     
   if(ncol(ft)!=2)
     ft <- t(ft)
   numE <- nrow(ft)

   ## deal with W
   if(is.null(W)) W <- rep(1,numE)
   if(!length(W)==numE)
     stop("Length of 'W' must equal number of edges in 'ft'.")

   ## deal with edgemode
   if(!edgemode %in% c("undirected", "directed"))
     stop("'edgemode' most be either 'directed' or 'undirected'")
   if(edgemode=="undirected") {
     ft <- rbind(ft, ft[,2:1])
     W  <- c(W,W)
   }
   
   ## deal with V
   if(is.null(V)) V <- names(table(ft))
   ift <- cbind(match(ft[,1], V), match(ft[,2], V))
   if(any(is.na(ift)))
     stop("Node names in 'ft' must be contained in 'V'.")
   numN <- length(V)

   ind <- ift[,1]+(ift[,2]-1)*numN
   if(any(duplicated(ind)))
     stop("Please do not specify the same edge multiple times")

   switch(targetclass,
    adjM = {
      mat <-matrix(0, ncol=numN, nrow=numN, dimnames=list(V,V))
      mat[ind] <- W
      mat
    },
    graphNEL = {
      toN <- split(ift[,2], ft[,1]) ## 1st column=from, 2nd column=to 
      eW  <- split(W, ft[,1])
      edgeL <- lapply(V, function(nm) list(edges=toN[[nm]], weights=eW[[nm]]))
      names(edgeL) <- V
      new("graphNEL", nodes=V, edgeL=edgeL, edgemode=edgemode)
    },
    stop(paste("Unknown targetclass '", targetclass, "'", sep=""))
  ) ## end switch        
}


setAs("matrix", "graphNEL", function(from) {
    if(is.null(rownames(from))) stop("from must have row names")
    if(is.null(colnames(from))) stop("from must have column names")
    if(!identical(rownames(from),colnames(from)))
        stop("Row and column names of from must be the same.")

    V <- rownames(from)

    tmat<-which(from>0,arr.ind=TRUE)
    numN<-length(V)
    numE<-dim(tmat)[1]

    rval <- vector("list", length = numN)
    for (i in 1:numE) {
        rval[[tmat[i, 1]]]$edges <- c(rval[[tmat[i, 1]]]$edges,
                                      tmat[i, 2])
        ln <- length(rval[[tmat[i, 1]]]$edges)
        rval[[tmat[i, 1]]]$weights <- c(rval[[tmat[i, 1]]]$weights,
                                        from[tmat[i,1],tmat[i,2]])
        names(rval[[tmat[i, 1]]]$weights)[ln] <- tmat[i, 2]
    }
    names(rval) <- V
    g1<-new("graphNEL", nodes = V, edgeL = rval)

    if (all(from == t(from)))
        g1@edgemode<- "undirected"
    else
        g1@edgemode <- "directed"
    g1
})

setAs("graphNEL","matrix",function(from) {
  nr = nc = numNodes(from)
  e1 = from@edgeL
  m <- matrix(unlist(lapply(e1,function(x) {
    y <- rep(0,nc)
    y[x$edges] = if(is.null(x$weights)) rep(1,length(x$edges)) else x$weights
    y
  })),nr=nr,nc=nc,byrow=TRUE,dimnames=list(from@nodes,from@nodes))
  #Update missing reflexive edges when the graph is undirected
  if(edgemode(from) == "undirected")
    m + (m == 0)*t(m)
  else
    m
})

