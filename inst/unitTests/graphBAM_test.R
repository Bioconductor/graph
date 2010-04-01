## library("graph")
set.seed(0x12a9b)

randBAMGraph <- function(numNodes = 10 , numEdges = 10)
{
    df <-  graph:::randFromTo(numNodes, numEdges)
    wts = seq_len(numNodes)
    g <- GraphBAM(df$ft$from, df$ft$to, df$nodes, edgemode="directed", weights = wts)
    g
}

make_smallBAM <- function() {
    from = c("a", "a", "a", "x", "x", "c")
    to   = c("b", "c", "x", "y", "c", "a")
    weight=c(3.4, 2.6, 1.7, 5.3, 1.6, 7.9)
    g1 <- GraphBAM(from, to, edgemode = "directed", weights = weight)
    g1
}

create_bigBAM <- function()
{
    r1 <- randFromTo(100, 100)
    g1 <- GraphBAM(r1$from, r1$to, r1$nodes, edgemode="directed", weights=seq_len(100))
    g1
}

test_create_graphBAMSmall <- function() {

    from = c("a", "d", "d", "b")
    to = c("b", "a", "d", "c")
    weights= c(1.5, 3.1, 5.4, 1)
    nodes = c("a","b","c","d") 
    g1 <- GraphBAM(from, to, edgemode = "directed", weights = weights)
    g2 <- GraphBAM(from, to, edgemode = "undirected")
    
    checkEquals(4L, numEdges(g1))
    checkEquals(isDirected(g1), TRUE)
    checkEquals(isAdjacent(g1, c("a", "d", "b"), c("b", "d", "c") ), c(TRUE,TRUE,TRUE))
    checkEquals(names(edges(g1)), c("a", "b", "c", "d"))
    k <- edges(g1)
    checkEquals(list(k$a, k$b, k$c, k$d), list("b", "c", character(0), c("a", "d")))
    w <- edgeWeights(g1)
    checkEquals(names(w), c("a", "b", "c", "d"))
    checkEquals(list(w$a, w$b, w$c, w$d), list(structure(1.5, names="b"),
            structure(1, names="c"), character(0), structure(c(3.1, 5.4), 
            names= c("a", "d"))))
    checkEquals(4L, numNodes(g2))
    checkEquals(4L, numEdges(g2))
    checkEquals(isDirected(g2), FALSE)
    checkEquals(isAdjacent(g1, c("a","d","b"), c("b","d","c") ), c(TRUE,TRUE,TRUE))
    
}

test_BAMNodes <- function() {
    from = c("a", "a", "a", "x", "x", "c")
    to   = c("b", "c", "x", "y", "c", "a")
    weight=c(3.4, 2.6, 1.7, 5.3, 1.6, 7.9)
    g1 <- GraphBAM(from, to, edgemode = "directed", weights = weight)
    nds <- nodes(g1)
    checkIdentical(all(nds %in% unique(c(from,to))),TRUE)
    checkIdentical(isDirected(g1),TRUE)
}


checkBAMSubGraph <- function(g, subG) {
    nds <- nodes(g)
    subNodes <- nodes(subG)
    w1 <- g@edgeSet@weights
    ft1 <- .Call(graph:::graph_bitarray_rowColPos, g@edgeSet@bit_vector, length(nds))
    origFromTo <- data.frame(from=nds[ft1[,"from"]], to = nds[ft1[,"to"]], weights = w1)
  
    w2 <- subG@edgeSet@weights
    ft2 <- .Call(graph:::graph_bitarray_rowColPos, subG@edgeSet@bit_vector, length(subNodes))
    subFromTo <- data.frame(from = subNodes[ft2[,"from"]], to = subNodes[ft2[,"to"]], weights = w2)

    indx <- (origFromTo$from %in% subNodes) &
    (origFromTo$to %in% subNodes)
    want <- origFromTo[(origFromTo$from %in% subNodes) & (origFromTo$to %in% subNodes),]

    checkEquals(as.character(want$from), as.character(subFromTo$from))
    checkIdentical(as.character(want$to), as.character(subFromTo$to))
    checkEquals(g@edgeSet@weights[indx], subG@edgeSet@weights)
}

test_BAMSubGraph_Small <- function() { 
    g1 <- make_smallBAM()
    sg <- subGraph(c("a","x", "y"), g1)
    checkIdentical(isDirected(sg), TRUE)
    checkIdentical(nodes(sg), c("a", "x", "y"))
    checkBAMSubGraph(g1,sg)    
}


test_BAMSubGraph_Large  <- function() {
    g1 <- randBAMGraph(100,100)
    sn <- sample(nodes(g1), 55)
    sg <- subGraph( sn, g1)
    checkIdentical(isDirected(sg), TRUE)
    checkBAMSubGraph(g1,sg) 
}


test_BAM_edgeWeights <- function() {
    g1 <- make_smallBAM()
    ew1 <- edgeWeights(g1)
    checkEquals(names(ew1), c("a", "b", "c", "x", "y"))
    checkEquals(list(ew1$a, ew1$b, ew1$c, ew1$x, ew1$y), 
            list(structure( c(3.4, 2.6, 1.7), names = c("b","c","x")),
            character(0), structure(c(7.9), names = "a"),
            structure(c(1.6, 5.3), names= c("c", "y")), character(0)))

    ew2 <- edgeWeights(g1,c("a","b")) ##index = char
    checkEquals(names(ew2), c("a","b"))
    checkEquals(list(ew2$a, ew2$b), list(structure( c(3.4, 2.6, 1.7),
                            names = c("b","c","x")), character(0)))
     
    ew2 <- edgeWeights(g1, 1:2) ##index = numeric
    checkEquals(names(ew2), c("a","b"))
    checkEquals(list(ew2$a, ew2$b), list(structure( c(3.4, 2.6, 1.7),
                            names = c("b","c","x")), character(0)))
}


test_BAM_edges <- function() {
    g1 <- make_smallBAM()
    ew1 <- edges(g1)
    checkEquals(names(ew1), c("a", "b", "c", "x", "y"))
    checkEquals(list(ew1$a, ew1$b, ew1$c, ew1$x, ew1$y), 
            list( c("b","c","x"), character(0), "a", c("c", "y"), character(0)))

    ew2 <- edges(g1, c("c", "b")) 
    checkEquals(names(ew2), c("c","b"))
    checkEquals(list(ew2$c, ew2$b), list("a", character(0)))
}


test_BAM_edgeMatrix <- function() {
      g1 <- make_smallBAM()
      em <- edgeMatrix(g1)
      checkEquals(em[1,], c(3, 1, 1, 4, 1, 4))
      checkEquals(em[2,], c(1, 2, 3, 3, 4, 5))
}

test_BAMSmall_edgeData <- function(){
      g1 <- make_smallBAM()
      eg <- edgeData(g1)
      tmp <- paste(c("c", "a", "a", "x", "a", "x"), c("a","b","c","c","x","y"),sep="|")
      checkEquals(names(eg), tmp)
      vals <- sapply( names(eg),function(k){
               eg[[k]]$weight               
              })
      checkEquals(names(vals), tmp)
      checkEquals( as.numeric(vals),c(7.9, 3.4, 2.6, 1.6, 1.7, 5.3))

      eg <- edgeData(g1, "a", attr="weight")
      tmp <- paste("c", "a", sep = "|")
      checkEquals(names(eg), tmp)
      vals <- sapply( names(eg),function(k){
               eg[[k]]$weight               
              })
      checkEquals(names(vals), tmp)
      checkEquals( as.numeric(vals),7.9)

      checkException(eg <- edgeData(g1, "a", attr="weightsss"))
     
      eg <- edgeData(g1, "a", "b", attr="weight")
      tmp <- paste("a", "b", sep = "|")
      checkEquals(names(eg), tmp)
      vals <- sapply( names(eg),function(k){
               eg[[k]]$weight               
              })
      checkEquals(names(vals), tmp)
      checkEquals( as.numeric(vals),3.4)
 }




