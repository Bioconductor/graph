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
    	new("graphNEL", nodes = V, edgeL = rval, edgemode="directed")
}

## WH 23 June 2004, Ladir CH
ftM2adjM <- function(ft, W=NULL, V=NULL, edgemode="directed")
  .ftM2other(ft, W, V, edgemode, "adjM")

ftM2graphNEL <- function(ft, W=NULL, V=NULL, edgemode="directed")
  .ftM2other(ft, W, V, edgemode, "graphNEL")

.ftM2other <- function(ft, W, V, edgemode, targetclass) {
   ## ft: nx2 matrix.
   if(!(is.matrix(ft) && ncol(ft)==2))
     stop("'ft' must be an nx2 matrix.")
   numE <- nrow(ft)

   ## deal with W
   if(is.null(W)) W <- rep(1,numE)
   if(!length(W)==numE)
     stop("Length of 'W' must equal number of edges in 'ft'.")

   ## deal with edgemode
   if(!edgemode %in% c("undirected", "directed"))
     stop("'edgemode' most be either 'directed' or 'undirected'")
   if(edgemode == "undirected") {
     ## reflect each pair -- but *not* the self-edges!
     differ <- ft[,1] != ft[,2]
     ft <- rbind(ft, ft[differ, 2:1])
     W  <-     c( W,  W[differ])
   }

   ## deal with V
   cft <- as.character(ft)
   if(is.null(V))
     V <- unique(cft)
   ift <- array(match(cft, V), dim=dim(ft))
   if(any(is.na(ift)))
     stop("Node names in 'ft' must be contained in 'V'.")
   numN <- length(V)

   ind <- ift[,1]+(ift[,2]-1)*numN
   if(any(duplicated(ind)))
     stop("Please do not specify the same edge multiple times")

   switch(targetclass,
    adjM = {
      mat <- matrix(0, ncol=numN, nrow=numN, dimnames=list(V,V))
      mat[ind] <- W
      mat
    },
    graphNEL = {
      ## ift[,2] are the indices of the to-nodes in V
      ## ft[,1] are the names of the from-nodes
      ## toN is a named list, whose names are the levels of ft[,1] and whose elements are the indices of to-nodes in V
      ##   names(toN) is a subset of V, but not identical: the nodes with no outgoing edges are not in names(toN)
      ## Beware of partial matching! (This lead to a bug in earlier versions of this function, where edges were
      ##   invented if there were nodes with no outgoing edges whose name partially matched the name of other 
      ##   nodes with outgoing edges.
      toN <- split(ift[,2], ft[,1]) 
      eW  <- split(W,       ft[,1])
      edgeL <- lapply(V, function(x) list(edges=NULL, weights=NULL))
      names(edgeL) <- V
      
      mt = match(names(toN), V)
      for(k in seq(along=mt))
        edgeL[[mt[k]]] <- list(edges=toN[[k]], weights=eW[[k]])
      
      new("graphNEL", nodes=V, edgeL=edgeL, edgemode=edgemode)
    },
    stop(paste("Unknown targetclass '", targetclass, "'", sep=""))
  ) ## end switch
}


setAs("matrix", "graphAM", function(from) {
  if(!identical(ncol(from), nrow(from)))
    stop("'ncol(from)' and 'nrow(from)' must be the same.")
    
  if(is.null(rownames(from))) {
    rownames(from) = if(is.null(colnames(from))) {
      paste(seq_len(nrow(from)))
    } else {
      colnames(from)
    }
  }
  
  if(is.null(colnames(from))) {
    colnames(from) = if(is.null(rownames(from))) {
      paste(seq_len(ncol(from)))
    } else {
      rownames(from)
    }
  }

  if(!identical(rownames(from),colnames(from)))
    stop("'rownames(from)' and 'colnames(from)' must be identical.")
  
  if(!is.numeric(from)) 
    storage.mode(from) = "integer"

  emode <- if (all(from == t(from))) "undirected" else "directed"
  defaultWeight <- vector(mode = typeof(from), length = 1L)
  defaultWeight[1L] <- 1L
  new("graphAM", from, edgemode=emode, values=list(weight=defaultWeight))
})


setAs("matrix", "graphNEL",
      function(from) as(as(from, "graphAM"), "graphNEL"))


setAs("graphNEL", "matrix", function(from) {
    as(as(from, "graphAM"), "matrix")
})


