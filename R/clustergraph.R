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



 setMethod("initialize", "distGraph", function(.Object, Dist) {
       if( is.null( attr(Dist, "Labels") ) )
	   attr(Dist, "Labels") = as.character(1:attr(Dist, "Size"))
       else
         checkValidNodeName(attr(Dist, "Labels"))
       .Object@Dist = Dist
       .Object
 })

 setMethod("nodes", "distGraph", function(object)
      attr(object@Dist, "Labels" ))

 

 setMethod("Dist", "distGraph", function(object)
    object@Dist)

  setMethod("show", "distGraph", function(object) {
    cat("distGraph with ", attr(object@Dist, "Size"),
   " nodes \n", sep="")})

 
  setMethod("threshold", "distGraph", function(object, k, value=0) {
        nd <- object@Dist
        nd[nd > k ] <- value 
        new("distGraph", Dist=nd)
     })

 setMethod("addNode", c("character", "distGraph", "list"),
        function(node, object, edges) {
            gN = nodes(object)
            nNode = length(gN)
            nAdd = length(node)
            numE = sapply(edges, length)
            if( any(numE != nNode+nAdd) )
                stop("must supply all internode distances")
            newN <- c(gN, node)
            nmE <- names(edges)
            if( length(nmE) != nAdd || any(!(names(edges) %in% node)))
               stop("edges must be named")

            edges <- edges[node]
            lapply(edges, function(x) if(any( !(names(x) %in%
                                                newN) ) )
                   stop("bad edge specification"))
            ordE <- lapply(edges, function(x) x[newN])
            subM <- sapply(ordE, function(x) x) ##should be a matrix!
            oldD <- as.matrix(Dist(object))
            f1 <- cbind(oldD, subM[1:nNode,])
            f2 <- rbind(f1, t(subM))
            rv <- new("distGraph", Dist = as.dist(f2))
        })


  setMethod("numNodes", "distGraph", function(object)
     attr(Dist(object), "Size"))

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
        return(adjL) })

   setMethod("edges", c("distGraph", "missing"), function(object, which) {
       nN <- nodes(object)
       eL <- lapply(nN, function(x) adj(object, x)[[1]])
       names(eL) <- nN
       return(eL) })

   setMethod("edges", c("distGraph", "character"), function(object, which) {
       nN <- nodes(object)
       wh<-match(which, nN)
       if( any(is.na(wh)) )
           stop("not all nodes are in the supplied graph")
       eL <- lapply(which, function(x) adj(object, x)[[1]])
       names(eL) <- which
       eL})

 ## FIXME: update for new attribute storage
 setMethod("edgeWeights", "distGraph",
           function(object, index, attr, default, type.checker) {
               edg <- edges(object)
               if( !missing(index) )
                   edg <- edg[index]
               NODES <- nodes(object)
               edLocs <- match(names(edg), NODES)
               edE <- lapply(edg, function(x) match(x, NODES))
               for( i in seq(along=edLocs) )
                   edE[[i]] <- object@Dist[edLocs[i], edE[[i]]]
               names(edE) <- names(edg)
               edE
           })

  setMethod("subGraph", c("character", "distGraph"),
            function(snodes, graph) {
                gN <- nodes(graph)
                whN <- match(snodes, gN)
                if( any(is.na(whN) ) )
                    stop("supplied nodes not all in graph")
                nD <- as.matrix(Dist(graph))[whN, whN]
                new("distGraph", Dist=as.dist(nD))
            })

setMethod("edgeL", "distGraph", function(graph, index) {
    edges <- edges(graph)
    edgeL <- mapply(function(x, y, nodes) {
        out <- list(edges=match(x, nodes), weights=y)
    }, edges, edgeWeights(graph), MoreArgs=list(nodes=nodes(graph)),
                    SIMPLIFY=FALSE)
    names(edgeL) <- names(edges)

    if (! missing(index))
        edgeL <- edgeL[[index]]

    edgeL
})

####################################
##clusterGraph code here
####################################


 setMethod("nodes", "clusterGraph", function(object)
    as.character(unlist(object@clusters)))

 setReplaceMethod("nodes", c("clusterGraph", "character"),
             function(object, value) {
                 clens = sapply(object@clusters, length)
                 if(length(value) != sum(clens))
                     stop("need as many names as there are nodes")
                 if(any(duplicated(value)))
                     stop("node names must be unique")
                 nc = length(clens)
                 ni = rep(1:nc, clens)
                 newc = split(value, ni)
                 names(newc) = names(object@clusters)
                 object@clusters = newc
                 object})

 setMethod("edges", c("clusterGraph", "missing"), function(object, which) {
     edges<-list()
     for(clust in object@clusters) {
         cc <- as.character(clust)
         for(i in seq(along=cc) )
             edges[[cc[i]]] <- cc[-i]
     }
     edges})

 setMethod("edges", c("clusterGraph", "character"), function(object, which) {
     nN <- nodes(object)
     wh <- match(which, nN)
     if( any(is.na(wh)) )
         stop("not all nodes are in the supplied graph")
     edges<-list()
     for(clust in object@clusters) {
         cc <- intersect(as.character(clust), which)
         for(i in seq(along=cc) )
           edges[[cc[i]]] <- cc[-i]
     }
     edges[which]})

setMethod("edgeL", "clusterGraph", function(graph, index) {
    clusters <- connComp(graph)
    nodes <- nodes(graph)
    edgeL <- list()

    cur <- 1
    for (i in seq(along=clusters)) {
        curClust <- clusters[[i]]
        for (j in seq(along = curClust)) {
            edgeL[[cur]] <- list(edges=match(curClust[-j], nodes))
            cur <- cur + 1
        }
    }
    names(edgeL) <- nodes

    if (! missing(index))
        edgeL <- edgeL[[index]]

    edgeL
})


##FIXME: this should be done from distances, but for now...)
##eg, if a distance matrix was supplied we could use that to define
##edge weights -- as that seems appropriate

 ## FIXME: update for new attr storage
 setMethod("edgeWeights", "clusterGraph",
           function(object, index, attr, default, type.checker) {
     edg <- edges(object)
     if( !missing(index) )
         edg <- edg[index]

     ans <- lapply(edg, function(x) { ans <- rep(1, length(x));
                                      names(ans) <- x; ans})
     ans})


 setMethod("subGraph", c("character", "clusterGraph"),
           function(snodes, graph) {
               cList <- graph@clusters
               cL <- lapply(cList, function(x) intersect(x, snodes))
               graph@clusters <- cL
               graph})

 setMethod("numNodes", "clusterGraph", function(object)
    sum(sapply(object@clusters, length)))

 setMethod("adj", "clusterGraph", function(object, index) {
     nIndex <- length(index)
     if( any(is.na(match(index, nodes(object)))) )
         stop("invalid node label supplied")
     rval <- vector("list", length=nIndex)
     names(rval) <- index
     for(i in 1:nIndex) {
         for(cl in object@clusters)
             if( index[i] %in% cl )
                 rval[[i]] <- cl
     }
     return(rval)})


 ##for cluster graphs, acc and adj are the same
 setMethod("acc", c("clusterGraph", "character"),
           function(object, index) {
               nIndex <- length(index)
               if( any(is.na(match(index, nodes(object)))) )
                   stop("invalid node label supplied")
               rval <- vector("list", length=nIndex)
               names(rval) <- index
               for(i in 1:nIndex) {
                   for(cl in object@clusters)
                       if( index[i] %in% cl )
                           rval[[i]] <- cl
                   }
               return(rval)})

 setMethod("connComp", "clusterGraph", function(object)
      object@clusters)

  setMethod("show", "clusterGraph",
  function(object)
   {
     numNull<-numNoEdges(object)
     numNodes<- numNodes(object)
     numEdge<-numEdges(object)
     cat("A graph with ", object@edgemode, " edges\n")
     cat("Number of Nodes = ",numNodes,"\n",sep="")
     cat("Number of Edges = ",numEdge,"\n",sep="")
   })

setAs(from="clusterGraph", to="matrix",
      function(from, to) {
          theNodes <- nodes(from)       # will be grouped by cluster!
          numNodes <- length(theNodes)
          m <- matrix(0, nrow=numNodes, ncol=numNodes,
                      dimnames=list(theNodes, theNodes))
          for (clust in from@clusters) {
              idx <- match(clust, theNodes)
              m[idx, idx] <- 1
          }
          diag(m) <- 0                 # eliminate self-loops
          m
      })
