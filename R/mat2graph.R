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


ftM2adjM <- function(ft, W=NULL, V=NULL, edgemode="directed") {
    if(!is.null(V) && sum(!names(table(ft)) %in% V)>0)
        stop("Node names in ft must be contained in V.")

	if(is.null(V)) V <- names(table(ft))

    numN <- length(V)
    numE <- dim(ft)[1]

    if(is.null(W)) W <- rep(1,numE)
    if(!length(W)==numE)
        stop("Length of W must equal number of rows of ft.")


    mat<-matrix(rep(0,numN*numN),nrow=numN)
    rownames(mat) <- V
    colnames(mat) <- V

    for (e in 1:numE) {
        temp1<-ft[e,1]
        temp2<-ft[e,2]
            mat[temp1,temp2]<-W[e]
        if (edgemode == "undirected")
            mat[temp2,temp1]<-W[e]
        }
    mat
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

