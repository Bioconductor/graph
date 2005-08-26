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


testConstructWithNodeObjects <- function() {
    mat <- simpleInciMat()
    nodeObjects <- lapply(1:4, function(x) {
        value <- x
        type <- "NodeObject"
        list(value=value, type=type)})
    names(nodeObjects) <- colnames(mat)
    g1 <- new("graphIM", inciMat=mat, nodes=nodeObjects)
    gotNodes <- nodes(g1)
    checkEquals(nodeObjects, gotNodes)
}


testConstructBadNodeObjects <- function() {
    mat <- simpleInciMat()
    nodeObjects <- lapply(1:4, function(x) {
        value <- x
        type <- "NodeObject"
        list(value=value, type=type)})
    ## use wrong names
    names(nodeObjects) <- LETTERS[1:4]
    myCheckException(new("graphIM", inciMat=mat, nodes=nodeObjects))
    ## use too few node objects
    names(nodeObjects) <- colnames(mat)
    tooFew <- nodeObjects[1:2]
    myCheckException(new("graphIM", inciMat=mat, nodes=tooFew))
    ## fail to specify names
    names(nodeObjects) <- NULL
    myCheckException(new("graphIM", inciMat=mat, nodes=nodeObjects))
}


testNodesSubset <- function() {
    mat <- simpleInciMat()
    nodeObjects <- lapply(1:4, function(x) {
        value <- x
        type <- "NodeObject"
        list(value=value, type=type)})
    names(nodeObjects) <- colnames(mat)
    g1 <- new("graphIM", inciMat=mat, nodes=nodeObjects)
    checkEquals(nodeObjects[c("a", "c")], nodes(g1, c("a", "c")))
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
    

testNodeNames <- function() {
    mat <- simpleInciMat()
    g1 <- new("graphIM", inciMat=mat)
    got <- nodeNames(g1)
    expect <- letters[1:4]
    checkEquals(expect, got)
}


testNodeNamesReplace <- function() {
    mat <- simpleInciMat()
    g1 <- new("graphIM", inciMat=mat)
    nodeNames(g1) <- LETTERS[1:4]
    expect <- LETTERS[1:4]
    checkEquals(expect, nodeNames(g1))
}


testNumNodes <- function() {
    mat <- simpleInciMat() 
    g1 <- new("graphIM", inciMat=mat)
    checkEquals(nrow(mat), numNodes(g1))
}


testNumEdges <- function() {
    mat <- simpleInciMat() 
    g1 <- new("graphIM", inciMat=mat)
    checkEquals(5, numEdges(g1))

    edgemode(g1) <- "directed"
    checkEquals(10, numEdges(g1))
}


## testSubgraph <- function() {
##     mat <- simpleInciMat() 
##     g1 <- new("graphIM", inciMat=mat)
##     g2 <- subgraph(c("a", "b", "c"), ffff)

##                }
    
## testEdgeWeights <- function() {
##     ## default weight is 1
##     mat <- simpleInciMat()
##     g1 <- new("graphIM", inciMat=mat)
##     expect <- list(a=c(c=1, d=1), b=c(c=1, d=1), c=c(a=1, b=1, d=1),
##                    d=c(a=1, b=1, c=1))
##     checkEquals(expect, edgeWeights(g1))
## }
    
    
