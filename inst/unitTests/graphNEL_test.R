simpleGraphNEL <- function() {
     set.seed(123)
     V <- letters[1:4]
     edL <- vector("list", length=4)
     names(edL) <- V
     edL[["a"]] <- list(edges=c(3, 4), weights=runif(2))
     edL[["b"]] <- list(edges=c(3, 4), weights=runif(2))
     edL[["c"]] <- list(edges=c(1, 2, 4), weights=runif(3))
     edL[["d"]] <- list(edges=c(1, 2, 3), weights=runif(3))
     gR <- new("graphNEL", nodes=V, edgeL=edL)
     gR
 }


simpleDirectedGraphNEL <- function() {
    set.seed(123)
     V <- letters[1:4]
     edL <- vector("list", length=4)
     names(edL) <- V
     edL[["a"]] <- list(edges=c(3, 4), weights=runif(2))
     edL[["b"]] <- list(edges=c(3), weights=runif(2))
     edL[["c"]] <- list(edges=numeric(0), weights=numeric(0))
     edL[["d"]] <- list(edges=c(2, 3), weights=runif(3))
     gR <- new("graphNEL", nodes=V, edgeL=edL, edgemode="directed")
     gR
 }


testIsAdjacent <- function() {
    g1 <- simpleGraphNEL()

    checkEquals(FALSE, isAdjacent(g1, "a", "b"))
    checkEquals(TRUE, isAdjacent(g1, "a", "c"))

    expect <- c(FALSE, TRUE, TRUE)
    got <- isAdjacent(g1, c("a", "a", "a"), c("b", "c", "d"))
    checkEquals(expect, got)
}


testNumEdges <- function() {
    mat <- matrix(c(1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0), ncol=4)
    rownames(mat) <- letters[1:4]
    colnames(mat) <- letters[1:4]
    g <- as(mat, "graphNEL")
    checkEquals(4, numEdges(g))
}


testInEdges <- function() {
    g <- simpleDirectedGraphNEL()
    expectedInEdges <- list(a=character(0), b="d", c=c("a", "b", "d"),
                            d="a")
    checkEquals(expectedInEdges, inEdges(g))
    n <- c("a", "d")
    checkEquals(expectedInEdges[n], inEdges(n, g))
}


testEmptyGraph <- function() {
    g <- new("graphNEL")
    checkEquals(0, numEdges(g))
    checkEquals(0, numNodes(g))
}


testEdgesConstructor <- function() {
    g <- simpleGraphNEL()
    g2 <- new("graphNEL", nodes=nodes(g),
              edges=edges(g))
    checkEquals(nodes(g), nodes(g2))
    checkEquals(edges(g), edges(g2))
}


testNotBothEdgesAndEdgeL <- function() {
    ## can't specify both edges and edgeL
    g <- simpleGraphNEL()
    myCheckException(new("graphNEL", nodes=nodes(g),
                         edges=edges(g),
                         edgeL=g@edgeL))
}
