simpleInciMat <- function() {
    mat <- matrix(c(0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0),
                  byrow=TRUE, ncol=4)
    rownames(mat) <- letters[1:4]
    colnames(mat) <- letters[1:4]
    mat
}


testInvalidNonSquare <- function() {
    mat <- cbind(c(0, 0, 1), c(1, 1, 1))
    myCheckException(new("graphIM", inciMat=mat))
}


testEdges <- function() {
    mat <- simpleInciMat() 
    g1 <- new("graphIM", inciMat=mat)
    got <- edges(g1)
    expect <- list(a=c("c", "d"), b=c("c", "d"), c=c("a", "b", "d"),
                   d=c("a", "b", "c"))
    checkEquals(expect, got)

    got <- edges(g1, c("a", "d"))
    expect <- expect[c("a", "d")]
    checkEquals(expect, got)
}

testEdgesSubset <- function() {
    mat <- simpleInciMat() 
    g1 <- new("graphIM", inciMat=mat)
    got <- edges(g1)
    expect <- list(a=c("c", "d"), d=c("a", "b", "c"))
    got <- edges(g1, c("a", "d"))
    checkEquals(expect, got)
}
    

testNodes <- function() {
    mat <- simpleInciMat()
    g1 <- new("graphIM", inciMat=mat)
    got <- nodes(g1)
    expect <- letters[1:4]
    checkEquals(expect, got)
}


testNodesReplace <- function() {
    mat <- simpleInciMat()
    g1 <- new("graphIM", inciMat=mat)
    nodes(g1) <- LETTERS[1:4]
    expect <- LETTERS[1:4]
    checkEquals(expect, nodes(g1))
}


testEdgeWeights <- function() {
    ## default weight is 1
    mat <- simpleInciMat()
    g1 <- new("graphIM", inciMat=mat)
    expect <- list(a=c(c=1, d=1), b=c(c=1, d=1), c=c(a=1, b=1, d=1),
                   d=c(a=1, b=1, c=1))
    checkEquals(expect, edgeWeights(g1))
}
    
    
