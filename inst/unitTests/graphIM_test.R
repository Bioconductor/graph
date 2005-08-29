simpleInciMat <- function() {
    ## Here's a simple graph for testing
    ##    a           b
    ##    |\         /|
    ##    | \___c___/ |
    ##    |     |     |
    ##    \     |     /
    ##     \____d____/
    ##
    ##
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
    g1 <- new("graphIM", inciMat=mat, nodeSet=nodeObjects)
    gotNodes <- nodeSet(g1)
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
    myCheckException(new("graphIM", inciMat=mat, nodeSet=nodeObjects))
    ## use too few node objects
    names(nodeObjects) <- colnames(mat)
    tooFew <- nodeObjects[1:2]
    myCheckException(new("graphIM", inciMat=mat, nodeSet=tooFew))
    ## fail to specify names
    names(nodeObjects) <- NULL
    myCheckException(new("graphIM", inciMat=mat, nodeSet=nodeObjects))
}


testNodesSubset <- function() {
    mat <- simpleInciMat()
    nodeObjects <- lapply(1:4, function(x) {
        value <- x
        type <- "NodeObject"
        list(value=value, type=type)})
    names(nodeObjects) <- colnames(mat)
    g1 <- new("graphIM", inciMat=mat, nodeSet=nodeObjects)
    checkEquals(nodeObjects[c("a", "c")], nodeSet(g1)[c("a", "c")])
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
    got <- nodes(g1)
    expect <- letters[1:4]
    checkEquals(expect, got)
}


testNodeNamesReplace <- function() {
    mat <- simpleInciMat()
    g1 <- new("graphIM", inciMat=mat)
    nodes(g1) <- LETTERS[1:4]
    expect <- LETTERS[1:4]
    checkEquals(expect, nodes(g1))
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
    
    
## Tests for edge attributes

testEdgeSetAttributes <- function() {
    mat <- simpleInciMat()
    g1 <- new("graphIM", inciMat=mat)

    ## If no attributes have been defined, empty list or NULL?
    checkEquals(list(), edgeSetAttributes(g1))

    myEdgeAttributes <- list(weight=1, color="blue")
    ## ideally, this is a set-once property because otherwise
    ## we'll run into consistency issues for edges that have customized
    ## attributes
    edgeSetAttributes(g1) <- myEdgeAttributes
    checkEquals(myEdgeAttributes, edgeSetAttributes(g1))
    ## So assigning into it again is an error
    myCheckException(edgeSetAttributes(g1) <- myEdgeAttributes)
}


testEdgeSetAttr <- function() {
    mat <- simpleInciMat()
    g1 <- new("graphIM", inciMat=mat)

    myCheckException(edgeSetAttr(g1, "noSuchAttr"))
    
    ## You can add attributes via edgeSetAttr and redefine default values
    edgeSetAttr(g1, "weight") <- 1
    checkEquals(1, edgeSetAttr(g1, "weight"))
    ## Note: the names is edgeSet Attr, not edge SetAttr, is this too confusing?

    ## redefine
    val <- list(a=1, b=2)
    edgeSetAttr(g1, "weight") <- val
    checkEquals(val, edgeSetAttr(g1, "weight"))

    ## add
    edgeSetAttr(g1, "color") <- "blue"

    expect <- list(weight=val, color="blue")
    checkEquals(expect, edgeSetAttributes(g1))
}


testEdgeAttributes <- function() {
    mat <- simpleInciMat()
    g1 <- new("graphIM", inciMat=mat)

    ## If nothing defined, empty list for now
    checkEquals(list(), edgeAttributes(g1, from="a", to="d"))
    
    myEdgeAttributes <- list(weight=1, color="blue")
    edgeSetAttributes(g1) <- myEdgeAttributes

    ## pickup default values
    checkEquals(myEdgeAttributes, edgeAttributes(g1, from="a", to="d"))

    ## disallow assigning names not in edgeSetAttributes
    badEdgeAttributes <- list(weight=400, style="modern", type="fruit")
    myCheckException(edgeAttributes(g1, "a", "d") <- badEdgeAttributes)

    ## No such edge
    myCheckException(edgeAttributes(g1, "a", "b"))
}
