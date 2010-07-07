## library("graph")
set.seed(0x12a9b)

randBAMGraph <- function(numNodes = 10 , numEdges = 10)
{
    df <-  graph:::randFromTo(numNodes, numEdges)
    df$ft$weight = seq_len(numNodes)
    g <- graphBAM(df$ft, nodes = df$nodes, edgemode = "directed")
    g
}

make_smallBAM <- function() {
    from = c("a", "a", "a", "x", "x", "c")
    to   = c("b", "c", "x", "y", "c", "a")
    weight=c(3.4, 2.6, 1.7, 5.3, 1.6, 7.9)
    df <- data.frame(from, to, weight)
    g1 <- graphBAM(df, edgemode = "directed")
    g1
}

make_unDirectedBAM <- function() {

    from = c("a", "a", "a", "x", "x", "c")
    to   = c("b", "c", "x", "y", "c", "d")
    weight=c(3.4, 2.6, 1.7, 5.3, 1.6, 7.9)
    df <- data.frame(from, to, weight)
    g1 <- graphBAM(df, edgemode = "undirected")
    g1

}

create_bigBAM <- function()
{
    r1 <- randFromTo(100, 100)
    r1$ft$weight <- seq_len(100)
    g1 <- graphBAM(r1$ft, r1$nodes, edgemode="directed")
    g1
}

test_create_graphBAMSmall <- function() {

    from = c("a", "d", "d", "b")
    to = c("b", "a", "d", "c")
    weight= c(1.5, 3.1, 5.4, 1)
    nodes = c("a","b","c","d")
    df <- data.frame(from, to, weight)
    g1 <- graphBAM(df, nodes, edgemode = "directed")
    g2 <- graphBAM(df, nodes, edgemode = "undirected")

    checkEquals(4L, numEdges(g1))
    checkEquals(isDirected(g1), TRUE)
    checkEquals(isAdjacent(g1, c("a", "d", "b"), c("b", "d", "c") ), c(TRUE,TRUE,TRUE))
    checkEquals(names(edges(g1)), c("a", "b", "c", "d"))
    k <- edges(g1)
    checkEquals(list(k$a, k$b, k$c, k$d), list("b", "c", character(0), c("a", "d")))
    w <- edgeWeights(g1)
    checkEquals(names(w), c("a", "b", "c", "d"))
    checkEquals(list(w$a, w$b, w$c, w$d), list(structure(1.5, names="b"),
            structure(1, names="c"), numeric(0), structure(c(3.1, 5.4),
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
    df <- data.frame(from, to, weight)
    g1 <- graphBAM(df, edgemode = "directed")
    nds <- nodes(g1)
    checkIdentical(all(nds %in% unique(c(from,to))),TRUE)
    checkIdentical(isDirected(g1),TRUE)
}


checkBAMSubGraph <- function(g, subG) {
    nds <- nodes(g)
    subNodes <- nodes(subG)
    w1 <- g@edgeSet@weights
    ft1 <- .Call(graph:::graph_bitarray_rowColPos, g@edgeSet@bit_vector)
    origFromTo <- data.frame(from=nds[ft1[,"from"]], to = nds[ft1[,"to"]], weights = w1)

    w2 <- subG@edgeSet@weights
    ft2 <- .Call(graph:::graph_bitarray_rowColPos, subG@edgeSet@bit_vector)
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
            numeric(0), structure(c(7.9), names = "a"),
            structure(c(1.6, 5.3), names= c("c", "y")), numeric(0)))

    ew2 <- edgeWeights(g1,c("a","b")) ##index = char
    checkEquals(names(ew2), c("a","b"))
    checkEquals(list(ew2$a, ew2$b), list(structure( c(3.4, 2.6, 1.7),
                            names = c("b","c","x")), numeric(0)))

    ew2 <- edgeWeights(g1, 1:2) ##index = numeric
    checkEquals(names(ew2), c("a","b"))
    checkEquals(list(ew2$a, ew2$b), list(structure( c(3.4, 2.6, 1.7),
                            names = c("b","c","x")), numeric(0)))
}

test_BAM_edgeWeights_undirected <- function()
{
      from = c("a", "d", "d", "b", "a")
        to = c("b", "a", "d", "c", "c")
    weight = c(1.5, 2.1, 3.4, 4.1, 5.6)
    df <- data.frame(from, to, weight)
    gu <- graphBAM(df, nodes="e", edgemode = "undirected")
    want <- list(a=c(b=1.5, c=5.6, d=2.1),
                 b=c(a=1.5, c=4.1),
                 c=c(a=5.6, b=4.1),
                 d=c(a=2.1, d=3.4),
                 e=numeric(0))
   checkEquals(want, edgeWeights(gu))

   checkEquals(want[c("c", "a")], edgeWeights(gu, c("c", "a")))
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

test_BAM_adj <- function() {
    g1 <- make_smallBAM()
    ew <- adj(g1, c("c", "b"))
    checkEquals(names(ew), c("c","b"))
    checkEquals(list(ew$c, ew$b), list("a", character(0)))
}

test_BAM_edgeMatrix <- function() {
      g1 <- make_smallBAM()
      em <- edgeMatrix(g1)
      checkEquals(em[1,], c(3, 1, 1, 4, 1, 4))
      checkEquals(em[2,], c(1, 2, 3, 3, 4, 5))
}

test_BAM_removeEdge_unknown_nodes <- function()
{
    g1 <- make_smallBAM()
    checkException(removeEdge("a", "q", g1))
    checkException(removeEdge("q", "a", g1))
    checkException(removeEdge("a", c("q", "aa", "tt"), g1))
    checkException(removeEdge(c("a", "q", "tt", "aa"),
                              c("a", "q", "aa", "tt"), g1))
}

test_BAM_removeEdge <- function()
{
    g1 <- make_smallBAM()
    ## removing nothing does nothing
    c0 <- character(0)
    checkEquals(edges(g1), edges(removeEdge(c0, c0, g1)))
    ## there is no y => a edge
    checkEquals(edges(g1), edges(removeEdge("y", "a", g1)))

    g2 <- removeEdge("c", "a", g1)
    checkEquals(list(c=character(0)), edges(g2, "c"))
    em <- edgeMatrix(g2)
    checkEquals(em[1,], c(1, 1, 4, 1, 4))
    checkEquals(em[2,], c(2, 3, 3, 4, 5))

    g3 <- removeEdge("a", c("b", "x"), g1)
    checkEquals(list(a="c"), edges(g3, "a"))
    checkEquals(edges(g1)[-1], edges(g3)[-1])

    g4 <- removeEdge(c("a", "x"), "c", g1)
    checkEquals(list(a=c("b", "x")), edges(g4, "a"))
    checkEquals(list(x="y"), edges(g4, "x"))
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
      tmp <- paste( c("a", "a", "a"), c("b", "c", "x"), sep = "|")
      checkEquals(names(eg), tmp)
      vals <- sapply( names(eg),function(k){
               eg[[k]] 
              })
      checkEquals(names(vals), tmp)
      checkEquals( as.numeric(vals), c(3.4, 2.6, 1.7))

      checkException(eg <- edgeData(g1, "a", attr="weightsss"))

      eg <- edgeData(g1, "a", "b", attr="weight")
      tmp <- paste("a", "b", sep = "|")
      checkEquals(names(eg), tmp)
      vals <- sapply( names(eg),function(k){
               eg[[k]]
              })
      checkEquals(names(vals), tmp)
      checkEquals( as.numeric(vals),3.4)
 }

test_BAM_extractFromToUndirected <- function() {
    g1 <- make_unDirectedBAM()
    ft <- extractFromTo(g1)
    checkEquals(as.character(ft$from), c("a", "a", "c", "a", "c", "x"))
    checkEquals(as.character(ft$to), c("b", "c", "d", "x", "x", "y"))
    checkEquals(ft$weight, c(3.4, 2.6, 7.9, 1.7, 1.6, 5.3))
}

test_BAM_extractFromToDirected <- function() {
    g1 <- make_smallBAM()
    ft <- extractFromTo(g1)
    checkEquals(as.character(ft$from), c("c", "a", "a", "x", "a", "x"))
    checkEquals(as.character(ft$to), c("a", "b", "c", "c", "x", "y"))
    checkEquals(ft$weight, c(7.9, 3.4, 2.6, 1.6, 1.7, 5.3))
}

test_BAM_bamToMatrix_UnDirected <- function() {
    g1 <- make_unDirectedBAM()
    mat <- as(g1, "matrix")
    checkEquals(isSymmetric(mat), TRUE)
    checkEquals(mat[upper.tri(mat)],
          c(3.4, 2.6, 0.0, 0.0, 0.0, 7.9, 1.7, 0.0,
                  1.6, 0.0, 0.0, 0.0, 0.0, 0.0, 5.3))
    checkEquals(rownames(mat),colnames(mat))
    checkEquals(rownames(mat), c("a", "b", "c", "d", "x", "y"))
}

test_BAM_bamToMatrix_Directed <- function() {
    g1 <- make_smallBAM()
    mat <- as(g1, "matrix")
    checkEquals(as.numeric(mat), c(0.0, 0.0, 7.9, 0.0,
                    0.0, 3.4, 0.0, 0.0, 0.0, 0.0, 2.6, 0.0,
                    0.0, 1.6, 0.0, 1.7, 0.0, 0.0, 0.0,0.0,
                    0.0, 0.0, 0.0, 5.3, 0.0))
    checkEquals(rownames(mat),colnames(mat))
    checkEquals(rownames(mat), c("a","b", "c", "x","y"))
}

test_BAM_bamTographAM_unDirected <- function() {
    g1 <- make_unDirectedBAM()
    am <- as(g1,"graphAM")
    checkEquals(nodes(g1), nodes(am))
    checkEquals(edgemode(g1), edgemode(am))
    checkEquals(edges(g1), edges(am))
    w1 <- edgeWeights(g1)
    w2 <- edgeWeights(am)
    checkEquals(names(w1), names(w2))
    checkEquals( w1$a, w2$a)
    checkEquals( w1$b, w2$b)
    checkEquals( sort(w1$c), sort(w2$c))
    checkEquals( w1$d, w2$d)
    checkEquals( sort(w1$x), sort(w2$x))
    checkEquals( w1$y, w2$y)
}

test_BAM_bamTographAM_Directed <- function() {
    g1 <- make_smallBAM()
    am <- as(g1,"graphAM")
    checkEquals(nodes(g1), nodes(am))
    checkEquals(edgemode(g1), edgemode(am))
    checkEquals(edges(g1), edges(am))
    w1 <- edgeWeights(g1)
    w2 <- edgeWeights(am)
    checkEquals(names(w1), names(w2))
    checkEquals( w1$a, w2$a)
    checkEquals( w1$b, w2$b)
    checkEquals( sort(w1$c), sort(w2$c))
    checkEquals( w1$d, w2$d)
    checkEquals( sort(w1$x), sort(w2$x))
    checkEquals( w1$y, w2$y)
}

test_BAM_bamTographNEL_UnDirected <- function() {
    g1 <- make_unDirectedBAM()
    nel <- as(g1,"graphNEL")
    checkEquals(nodes(g1), nodes(nel))
    checkEquals(edgemode(g1), edgemode(nel))
    checkEquals(edges(g1), edges(nel))
    w1 <- edgeWeights(g1)
    w2 <- edgeWeights(nel)
    checkEquals(names(w1), names(w2))
    checkEquals( w1$a, w2$a)
    checkEquals( w1$b, w2$b)
    checkEquals( sort(w1$c), sort(w2$c))
    checkEquals( w1$d, w2$d)
    checkEquals( sort(w1$x), sort(w2$x))
    checkEquals( w1$y, w2$y)
}


test_BAM_bamTographNEL_Directed <- function() {
    g1 <- make_smallBAM()
    nel <- as(g1,"graphNEL")
    checkEquals(nodes(g1), nodes(nel))
    checkEquals(edgemode(g1), edgemode(nel))
    checkEquals(edges(g1), edges(nel))
    w1 <- edgeWeights(g1)
    w2 <- edgeWeights(nel)
    checkEquals(names(w1), names(w2))
    checkEquals( w1$a, w2$a)
    checkEquals( w1$b, w2$b)
    checkEquals( sort(w1$c), sort(w2$c))
    checkEquals( w1$d, w2$d)
    checkEquals( sort(w1$x), sort(w2$x))
    checkEquals( w1$y, w2$y)
}

create_GraphNEL_Directed <- function() {
     set.seed(123)
     V <- letters[1:4]
     edL <- vector("list", length=4)
     names(edL) <- V
     edL[["a"]] <- list(edges=c(3, 4), weights=c(.13, .14))
     edL[["b"]] <- list(edges=c(3), weights=.23)
     edL[["c"]] <- list(edges=numeric(0), weights=numeric(0))
     edL[["d"]] <- list(edges=c(2, 3), weights=c(.42, .43))
     gR <- new("graphNEL", nodes = V, edgeL = edL, edgemode = "directed" )
     gR
}

create_GraphNEL_UnDirected <- function() {
     set.seed(123)
     V <- letters[1:4]
     edL <- vector("list", length=4)
     names(edL) <- V
     edL[["a"]] <- list(edges=c(2, 3), weights=c(.13, .14))
     edL[["b"]] <- list(edges=c(1), weights=.13)
     edL[["c"]] <- list(edges=c(1), weights=0.14)
     edL[["d"]] <- list(edges= numeric(0), weights=numeric(0))
     gR <- new("graphNEL", nodes = V, edgeL = edL, edgemode = "undirected" )
     gR
}

test_graphNEL_Directed_To_graphBAM <-function() {
    nel <- create_GraphNEL_Directed()
    bam <- as(nel, "graphBAM")
    checkEquals(nodes(nel), nodes(bam))
    checkEquals(edgemode(nel), edgemode(bam))
    checkEquals(edges(nel), edges(bam))
    w1 <- edgeWeights(nel)
    w2 <- edgeWeights(bam)
    checkEquals(w1,w2)
}

test_graphNEL_Directed_To_graphBAM <- function() {
    nel <- create_GraphNEL_Directed()
    bam <- as(nel, "graphBAM")
    checkEquals(nodes(nel), nodes(bam))
    checkEquals(edgemode(nel), edgemode(bam))
    checkEquals(edges(nel), edges(bam))
    w1 <- edgeWeights(nel)
    w2 <- edgeWeights(bam)
    checkEquals(w1,w2)
}

test_graphNEL_UnDirected_To_graphBAM <- function()  {
   nel <- create_GraphNEL_UnDirected()
   bam <- as(nel, "graphBAM")
   checkEquals(nodes(nel), nodes(bam))
   checkEquals(edgemode(nel), edgemode(bam))
   checkEquals(edges(nel), edges(bam))
   w1 <- edgeWeights(nel)
   w2 <- edgeWeights(bam)
   checkEquals(w1,w2)
}

test_graphAM_Directed_To_graphBAM <- function() {
    nel <- create_GraphNEL_Directed()
    am <- as(nel, "graphAM")
    bam <- as(am, "graphBAM")
    checkEquals(nodes(am), nodes(bam))
    checkEquals(edgemode(am), edgemode(bam))
    checkEquals(edges(am), edges(bam))
    w1 <- edgeWeights(am)
    w2 <- edgeWeights(bam)
    checkEquals(w1,w2)
}

test_graphAM_UnDirected_To_graphBAM<- function() {
   nel <- create_GraphNEL_UnDirected()
   am <- as(nel, "graphAM")
   bam <- as(am, "graphBAM")
   checkEquals(nodes(am), nodes(bam))
   checkEquals(edgemode(am), edgemode(bam))
   checkEquals(edges(am), edges(bam))
   w1 <- edgeWeights(am)
   w2 <- edgeWeights(bam)
   checkEquals(w1, w2)
}

test_BAM_set_edge_weights <- function()
{
    getw <- function(x) unlist(edgeWeights(x))

    g <- make_smallBAM()
    weight0 <- unlist(edgeWeights(g))
    edgeData(g, "c", "a", attr="weight") <- 123.0
    want <- weight0
    want["c.a"] <- 123.0
    checkEquals(want, getw(g))

    g <- make_smallBAM()
    edgeData(g, "a", c("b", "c", "x"), attr="weight") <- c(10, 11, 12)
    want <- weight0
    want[c("a.b", "a.c", "a.x")] <- c(10, 11, 12)
    checkEquals(want, getw(g))
}

test_BAM_Intersect_UnDirected <- function() {
    ## nodes a b c d x y
    from = c("a", "b", "d", "d")
    to   = c("b", "c", "x", "y")
    weight=c(1.2, 2.4, 3.2, 5.4)
    df <- data.frame(from, to, weight)
    g1 <- graphBAM(df, edgemode = "undirected")

    ## nodes a b c d x y z
    from = c("a", "b", "b", "d", "d")
    to   = c("b", "c", "d", "c", "x")
    weight=c(3.2, 1.2, 2.1, 3.2, 3.5)
    df <- data.frame(from, to, weight)
    g2 <- graphBAM(df, nodes = c("a","b","c", "d", "x", "y", "z"),
            edgemode = "undirected")

    g <- graphIntersect(g1,g2)
    checkEquals(intersect(nodes(g1), nodes(g2)), nodes(g))
    checkEquals(FALSE, isDirected(g))
    eg <- edgeData(g)
    vals <- sapply( names(eg),function(k){
               eg[[k]]$weight
              })
    tmp <- paste(c("a", "b", "d", "b", "c", "x"), c("b", "c", "x", "a", "b", "d"), sep= "|")
    checkEquals(tmp, names(vals))
    checkEquals(as.numeric(rep(NA, 6)), as.numeric(vals))
}


test_BAM_Intersect_Directed <- function() {
    ## nodes a b c d x y
    from = c("a", "b", "d", "d")
    to   = c("b", "c", "x", "y")
    weight=c(1.2, 2.4, 3.2, 5.4)
    df <- data.frame(from, to, weight)
    g1 <- graphBAM(df, edgemode = "directed")

    ## nodes a b c d x y z
    from = c("a", "b", "b", "d", "d")
    to   = c("b", "c", "d", "c", "x")
    weight=c(1.2, 1.2, 2.1, 3.2, 3.5)
    df <- data.frame(from, to, weight)
    g2 <- graphBAM(df, nodes = c("a","b","c", "d", "x", "y", "z"),
            edgemode = "directed")

    g <- graphIntersect(g1,g2)
    checkEquals(intersect(nodes(g1), nodes(g2)), nodes(g))
    checkEquals(TRUE, isDirected(g))
    eg <- edgeData(g)
    vals <- sapply( names(eg),function(k){
               eg[[k]]$weight
              })

    tmp <- paste(c("a", "b", "d"), c("b", "c", "x"), sep= "|")
    checkEquals(tmp, names(vals))
    checkEquals(c(1.2, NA, NA), as.numeric(vals))

}

test_BAM_Intersect_UnDirected2 <- function() {
    ## nodes a b d x y
    from = c("a", "d", "d")
    to   = c("b", "x", "y")
    weight=c(1.2, 3.2, 5.4)
    df <- data.frame(from, to, weight)
    g1 <- graphBAM(df, edgemode = "undirected")

    ## nodes a b c d x y z
    from = c("a", "b", "b", "d", "d")
    to   = c("b", "c", "d", "c", "x")
    weight=c(3.2, 1.2, 2.1, 5.2, 3.2)
    df <- data.frame(from, to, weight)
    g2 <- graphBAM(df, nodes = c("a","b","c", "d", "x", "y", "z"),
            edgemode = "undirected")

    g <- graphIntersect(g1,g2)
    checkEquals(intersect(nodes(g1), nodes(g2)), nodes(g))
    checkEquals(FALSE, isDirected(g))
    eg <- edgeData(g)
    vals <- sapply( names(eg),function(k){
               eg[[k]]$weight
              })
    tmp <- paste(c("a", "d", "b", "x"), c("b", "x", "a", "d"), sep= "|")
    checkEquals(tmp, names(vals))
    checkEquals(rep(c(NA,3.2),2), as.numeric(vals))
}

test_BAM_Intersect_EmptyEdges <- function() {

    from = c("a", "d", "d")
    to   = c("b", "x", "y")
    weight=c(1.2, 3.2, 5.4)
    df <- data.frame(from, to, weight)
    g1 <- graphBAM(df, edgemode = "directed")

    from = c("h", "i", "j")
    to   = c("b", "x", "y")
    weight=c(1.2, 3.2, 5.4)
    df <- data.frame(from, to, weight)
    g2 <- graphBAM(df, edgemode = "directed")

    g <- graphIntersect(g1,g2)
    checkEquals(nodes(g), intersect(nodes(g1), nodes(g2)))
    checkEquals(isDirected(g), TRUE)
    eg <- edgeWeights(g)
    checkEquals(c("b", "x", "y"), names(eg))
    checkEquals(list(numeric(0), numeric(0), numeric(0)),list(eg$b, eg$x, eg$y))
}

test_BAM_Intersect_EmptyNodes <- function() {

    from = c("a", "d", "d")
    to   = c("b", "x", "y")
    weight=c(1.2, 3.2, 5.4)
    df <- data.frame(from, to, weight)
    g1 <- graphBAM(df, edgemode = "unirected")

    from = c("h", "i", "j")
    to   = c("s", "h", "l")
    weight=c(1.2, 3.2, 5.4)
    df <- data.frame(from, to, weight)
    g2 <- graphBAM(df, edgemode = "undirected")

    g <- graphIntersect(g1,g2)
    checkEquals(intersect(nodes(g1), nodes(g2)), nodes(g))
    checkEquals(FALSE, isDirected(g))
    eg <- edgeWeights(g)
    checkEquals(list(), eg)
}

test_BAM_isAdjacent <- function()
{
     from = c("a", "d", "d", "b", "a")
     to = c("b", "a", "d", "c", "c")
     weight= c(1.5, 2.1, 3.4, 4.1, 5.6)
     df <- data.frame(from, to, weight)
     gd <- graphBAM(df, nodes="e", edgemode = "directed")

     ## single edges
     for (i in seq_len(nrow(df))) {
         checkEquals(TRUE, isAdjacent(gd, from[i], to[i]))
     }

     ## vectorized
     checkEquals(c(FALSE, TRUE, TRUE, FALSE, FALSE),
                 isAdjacent(gd, "a", letters[1:5]))

     checkEquals(c(FALSE, FALSE, FALSE, TRUE, FALSE),
                 isAdjacent(gd, letters[1:5], "a"))
}

test_BAM_Union_UnDirected <- function() {
    ## nodes a b c d x y
    from = c("a", "b", "d", "d")
    to   = c("b", "c", "x", "y")
    weight=c(1.2, 2.4, 3.5, 5.4)
    df <- data.frame(from, to, weight)
    g1 <- graphBAM(df, edgemode = "undirected")

    ## nodes a b c d x y z 
    from = c("a", "b", "b", "d", "d")
    to   = c("b", "c", "d", "c", "x")
    weight=c(3.2, 1.2, 2.1, 3.2, 3.5)
    df <- data.frame(from, to, weight)
    g2 <- graphBAM(df, nodes = c("a","b","c", "d", "x", "y", "z"), 
            edgemode = "undirected")

    g <- graphUnion(g1,g2)
    checkEquals(union(nodes(g1), nodes(g2)), nodes(g))
    checkEquals(FALSE, isDirected(g))
    df <- extractFromTo(g)
    tmp <- data.frame(from = c("a", "b", "b", "c", "d", "d"),
                      to = c("b", "c", "d", "d", "x", "y"),
                      weight = c( NA, NA, 2.1, 3.2, 3.5, 5.4))
    checkEquals(tmp, df)
}


test_BAM_Union_Directed <- function() {
    ## nodes a b c d x y
    from = c("a", "b", "d", "d")
    to   = c("b", "c", "x", "y")
    weight=c(1.2, 2.4, 3.5, 5.4)
    df <- data.frame(from, to, weight)
    g1 <- graphBAM(df, edgemode = "directed")

    ## nodes a b c d x y z 
    from = c("a", "b", "b", "d", "d")
    to   = c("b", "c", "d", "c", "x")
    weight=c(1.2, 1.2, 2.1, 3.2, 3.5)
    df <- data.frame(from, to, weight)
    g2 <- graphBAM(df, nodes = c("a","b","c", "d", "x", "y", "z"), 
            edgemode = "directed")

    g <- graphUnion(g1,g2)
    checkEquals(union(nodes(g1), nodes(g2)), nodes(g))
    checkEquals(TRUE, isDirected(g))

    df <- extractFromTo(g)
    tmp <- data.frame(from = c("a", "b", "d", "b", "d", "d"),
                        to = c("b", "c", "c", "d", "x", "y"),
                    weight = c( 1.2, NA, 3.2, 2.1, 3.5, 5.4))
    checkEquals(tmp, df)

}

test_BAM_Union_Mixed <- function() {
    ## nodes a b d x y
    from = c("a", "d", "d")
    to   = c("b", "x", "y")
    weight=c(1.2, 3.2, 5.4)
    df <- data.frame(from, to, weight)
    g1 <- graphBAM(df, edgemode = "undirected")

    ## nodes a b c d x y z 
    from = c("a", "b", "b", "d", "d")
    to   = c("b", "c", "d", "c", "x")
    weight=c(3.2, 1.2, 2.1, 3.2, 3.5)
    df <- data.frame(from, to, weight)
    g2 <- graphBAM(df, nodes = c("a","b","c", "d", "x", "y", "z"), 
            edgemode = "directed")

    checkException(g <- graphUnion(g1,g2))
}

test_BAM_inEdges <- function()
{
      from = c("a", "d", "d", "b", "a")
        to = c("b", "a", "d", "c", "c")
    weight = c(1.5, 2.1, 3.4, 4.1, 5.6)
      df <- data.frame(from, to, weight)
      ## directed
      gd <- graphBAM(df, nodes="e", edgemode = "directed")

      want <- list(a="d",
                   b="a",
                   c=c("a", "b"),
                   d="d",
                   e=character(0))
      checkEquals(want, inEdges(nodes(gd), gd))

      ## undirected
      gu <- graphBAM(df, nodes="e", edgemode = "undirected")
      checkEquals(edges(gu), inEdges(nodes(gu), gu))
}


test_BAM_directed_attrs <- function() {

    from = c("a", "a", "a", "x", "x", "c")
    to   = c("b", "c", "x", "y", "c", "a")
    weight = c(2, 1, 3, 4, 5, 6)
    df <- data.frame(from, to, weight)
    bam <- graphBAM(df, edgemode = "directed")
    
    checkException(edgeData(bam,from="a", attr="code"))
    edgeData(bam, attr = "weight") <- 1
    edgeData(bam, attr = "code") <- "plain"

    res <- unlist(edgeData(bam,from="a", attr="code"))
    nmres <- paste(c("a","a","a"), c ("b", "c", "x"), sep="|")  
    checkEquals(names(res), nmres)
    checkEquals(as.character(res), c("plain", "plain", "plain"))

    edgeData(bam,from = "a", to = "x", attr= "code") <- "red"
    res <- unlist(edgeData(bam, from = "a", attr = "code"))
    checkEquals(names(res), nmres)
    checkEquals(as.character(res), c("plain", "plain", "red"))

    edgeData(bam,to = "c", attr= "code") <- "yellow"
    res <- unlist(edgeData(bam, to= "c", attr = "code"))
    nmres <- paste(c("a", "x"), c("c", "c"), sep = "|")
    checkEquals(names(res), nmres)
    checkEquals(as.character(res), c("yellow", "yellow"))
}

test_BAM_undirected_attrs <- function() {

    from = c("a", "a", "a", "x", "x")
    to   = c("b", "c", "x", "y", "c")
    weight = c(2, 1, 3, 4, 5)
    df <- data.frame(from, to, weight)
    bam <- graphBAM(df, edgemode = "undirected")
    checkException(edgeData(bam,from="a", attr="code"))
    edgeData(bam, attr = "weight")  <- 1
    edgeData(bam, attr = "code") <- "plain"

    res <- unlist(edgeData(bam,from="a", attr="code"))
    nmres <- paste(c("a","a","a"), c ("b", "c", "x"), sep="|")  
    checkEquals(names(res), nmres)
    checkEquals(as.character(res), c("plain", "plain", "plain"))

    edgeData(bam,from = "a", to = "x", attr= "code") <- "red"
    res <- unlist(edgeData(bam, from = "a", attr = "code"))
    checkEquals(names(res), nmres)
    checkEquals(as.character(res), c("plain", "plain", "red"))

    edgeData(bam,to = "c", attr= "code") <- "yellow"
    res <- unlist(edgeData(bam, to= "c", attr = "code"))
    nmres <- paste(c("a", "x"), c("c", "c"), sep = "|")
    checkEquals(names(res), nmres)
    checkEquals(as.character(res), c("yellow", "yellow"))
}


test_graphBAM_detailed_Attribute_Intersection <- function() {

## nodes a b c d x y
from = c("a", "b", "d", "d")
to   = c("b", "c", "y", "x")
weight=c(1.2, 2.4, 5.4, 3.2)
df <- data.frame(from, to, weight)
g1 <- graphBAM(df, edgemode = "directed")
edgeData(g1, from = from, to = to ,attr = "weight")  <- c(1.2, 2.4, 5.4, 3.2)

edgeData(g1, from = from, to = to ,attr = "color") <-  c("red", "blue", NA, "green")
edgeData(g1, from = from, to = to , attr = "type") <-  c("high", "low", "high", NA)
## nodes a b c d x y z
from = c("a", "b", "b", "d", "d")
to   = c("b", "c", "d", "c", "x")
weight=c(1.2, 4.2, 5.6, 2.1, 3.2)
df <- data.frame(from, to, weight)
g2 <- graphBAM(df, nodes = c("a","b","c", "d", "x", "y", "z"),
        edgemode = "directed")
edgeData(g2, from = from, to = to,  attr = "color") <- c("red", "blue", NA, "red",
                                                         "yellow")
g <- graphIntersect(g1, g2)
df <- extractFromTo(g)
tmp <- data.frame( from = c("a", "b", "d"), 
                     to = c("b", "c", "x"), 
                 weight = c(1.2, NA, 3.2))

checkEquals(tmp, df)

attColor <- edgeData(g, attr = "color")
nms <- paste(c("a", "b", "d"),  c("b", "c", "x"), sep = "|")
target <- structure( c("red", "blue", NA), names = nms)
checkEquals(target, unlist(attColor))

checkException(edgeData(g, attr = "type"))

weightFun <- function(x, y) {
   return(x +y )
}

colorFun <- function(x,y) {
    if(x=="red" && y == "red")
        return("white")
    else
        return("black")
}

g <- graphIntersect(g1, g2, funList = list(weight = weightFun, color = colorFun))

attWeight <- edgeData(g, attr = "weight")
nms <- paste(c("a", "b", "d"),  c("b", "c", "x"), sep = "|")
target <- structure( c( 2.4, 6.6, 6.4), names = nms)
checkEquals(target, unlist(attWeight))

attColor <- edgeData(g, attr = "color")
nms <- paste(c("a", "b", "d"),  c("b", "c", "x"), sep = "|")
target <- structure( c( 2.4, 6.6, 6.4), names = nms)
checkEquals(target, unlist(attWeight))

}


test_graphBAM_detailed_Attribute_Union <- function() {

## nodes a b c d x y
from = c("a", "b", "d", "d")
to   = c("b", "c", "y", "x")
weight=c(1.2, 2.4, 5.4, 3.2)
df <- data.frame(from, to, weight)
g1 <- graphBAM(df, edgemode = "directed")
edgeData(g1, from = from, to = to ,attr = "weight")  <- c(1.2, 2.4, 5.4, 3.2)

edgeData(g1, from = from, to = to ,attr = "color") <-  c("red", "blue", NA, "green")
edgeData(g1, from = from, to = to , attr = "type") <-  c("high", "low", "high", NA)
## nodes a b c d x y z
from = c("a", "b", "b", "d", "d")
to   = c("b", "c", "d", "c", "x")
weight=c(1.2, 4.2, 5.6, 2.1, 3.2)
df <- data.frame(from, to, weight)
g2 <- graphBAM(df, nodes = c("a","b","c", "d", "x", "y", "z"),
        edgemode = "directed")
edgeData(g2, from = from, to = to,  attr = "color") <- c("red", "blue", NA, "red",
                                                         "yellow")
g <- graphUnion(g1, g2)
df <- extractFromTo(g)
tmp <- data.frame( from = c("a", "b", "d", "b", "d", "d"), 
                     to = c("b", "c", "c", "d", "x", "y"), 
                 weight = c(1.2, NA, 2.1, 5.6, 3.2, 5.4))
checkEquals(tmp, df)

attColor <- edgeData(g, attr = "color")
nms <- paste(c("a", "b", "d", "b", "d", "d"),  c("b", "c", "c", "d", "x", "y"), sep = "|")
target <- structure( c("red", "blue", "red", NA, NA, NA), names = nms)
checkEquals(target, unlist(attColor))

attType <- edgeData(g, attr = "type")
nms <- paste(c("a", "b", "d", "b", "d", "d"),  c("b", "c", "c", "d", "x", "y"), sep = "|")
target <- structure( c("high", "low", NA, NA, NA, "high"), names = nms)
checkEquals(target, unlist(attType))


weightFun <- function(x, y) {
   return(x + y )
}

colorFun <- function(x,y) {
    if(x=="red" || y == "red")
        return("white")
    else
        return("black")
}

g <- graphUnion(g1, g2, funList = list(weight = weightFun, color = colorFun))

attWeight <- edgeData(g, attr = "weight")
nms <- paste(c("a", "b", "d", "b", "d", "d"),  c("b", "c", "c", "d", "x", "y"), sep = "|")
target <- structure( c( 2.4, 6.6, 2.1, 5.6, 6.4, 5.4), names = nms)
checkEquals(target, unlist(attWeight))

attColor <- edgeData(g, attr = "color")
nms <- paste(c("a", "b", "d", "b", "d", "d"),  c("b", "c", "c", "d", "x", "y"), sep = "|")
target <- structure(c( "white", "black", "red", NA, "black", NA), names = nms)
checkEquals( target, unlist(attColor))

attType <- edgeData(g, attr = "type")
nms <- paste(c("a", "b", "d", "b", "d", "d"),  c("b", "c", "c", "d", "x", "y"), sep = "|")
target <- structure( c("high", "low", NA, NA, NA, "high"), names = nms)
checkEquals(target, unlist(attType))


attType <- edgeData(g, attr = "type")
nms <- paste(c("a", "b", "d", "b", "d", "d"),  c("b", "c", "c", "d", "x", "y"), sep = "|")
target <- structure( c("high", "low", NA, NA, NA, "high"), names = nms)
checkEquals(target, unlist(attType))

}

