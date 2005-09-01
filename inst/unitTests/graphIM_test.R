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


simpleDirectedGraph <- function() {
    ## Here's a simple graph for testing
    ##    a           b
    ##    |\         /^
    ##    | \__>c<__/ |
    ##    |     ^     |
    ##    \     |     /
    ##     \___>d____/
    ##
    ##
    mat <- matrix(c(0, 0, 1, 1,
                    0, 0, 1, 0,
                    0, 0, 0, 0,
                    0, 1, 1, 0),
                  byrow=TRUE, ncol=4)
    rownames(mat) <- letters[1:4]
    colnames(mat) <- letters[1:4]
    mat
    new("graphIM", inciMat=mat, edgemode="directed")
}
    


testInvalidNonSquare <- function() {
    mat <- cbind(c(0, 0, 1), c(1, 1, 1))
    myCheckException(new("graphIM", inciMat=mat))
}


testInvalidNegativeValues <- function() {
    mat <- matrix(c(0, 1, -4, -1), ncol=2)
    myCheckException(new("graphIM", inciMat=mat))
}


testInvalidNonSymmetric <- function() {
    mat <- matrix(c(0, 1, 1,
                    0, 0, 1,
                    0, 0, 0), ncol=3, byrow=TRUE)
    colnames(mat) <- letters[1:3]
    myCheckException(new("graphIM", inciMat=mat))
    myCheckException(new("graphIM", inciMat=mat, edgemode="undirected"))
    g1 <- new("graphIM", inciMat=mat, edgemode="directed")
}


testValuesToAttr <- function() {
    mat <- matrix(c(0, 0, 1, 2,
                    0, 0, 3, 0,
                    0, 0, 0, 0,
                    0, 4, 5, 0),
                  byrow=TRUE, ncol=4)
    rownames(mat) <- letters[1:4]
    colnames(mat) <- letters[1:4]
    mat
    g1 <- new("graphIM", inciMat=mat, edgemode="directed",
              values=list(weight=1))
    checkEquals(4, edgeAttributes(g1, "d", "b")[[1]]$weight)
    checkEquals(3, edgeAttributes(g1, "b", "c")[[1]]$weight)
    checkEquals(2, edgeAttributes(g1, "a", "d")[[1]]$weight)
    checkEquals(1, edgeAttributes(g1, "a", "c")[[1]]$weight)

    myCheckException(new("graphIM", inciMat=mat, edgemode="directed",
                         values=list(weight=1, not=2)))
    myCheckException(new("graphIM", inciMat=mat, edgemode="directed",
                         values=list("must", "name")))
    myCheckException(new("graphIM", inciMat=mat, edgemode="directed",
                         values="weight"))

    g1 <- new("graphIM", inciMat=mat, edgemode="directed",
              values=list(type=4))
    checkEquals(4, edgeAttributes(g1, "d", "b")[[1]]$type)
    checkEquals(3, edgeAttributes(g1, "b", "c")[[1]]$type)
    checkEquals(2, edgeAttributes(g1, "a", "d")[[1]]$type)
    checkEquals(1, edgeAttributes(g1, "a", "c")[[1]]$type)
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


testEdgesDirected <- function() {
    g1 <- simpleDirectedGraph()
    expect <- list(a=c("c", "d"), b="c", c=character(0),
                   d=c("b", "c"))
    checkEquals(expect, edges(g1))
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


testIsAdjacent <- function() {
    mat <- simpleInciMat()
    g1 <- new("graphIM", inciMat=mat)

    checkEquals(TRUE, isAdjacent(g1, "a", "c"))
    checkEquals(TRUE, isAdjacent(g1, "c", "a"))
    checkEquals(FALSE, isAdjacent(g1, "a", "b"))
    checkEquals(FALSE, isAdjacent(g1, "b", "a"))
    myCheckException(isAdjacent(g1, "z", "a"))
    myCheckException(isAdjacent(g1, "a", "z"))
}


testIsAdjacentVectorized <- function() {
    mat <- simpleInciMat()
    g1 <- new("graphIM", inciMat=mat)

    fr <- c("a", "c", "a", "b")
    to <- c("c", "a", "b", "a")
    expect <- c(TRUE, TRUE, FALSE, FALSE)
    checkEquals(expect, isAdjacent(g1, fr, to))
    checkEquals(expect, isAdjacent(g1, to, fr))
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
    ## TODO: make edgeSetAttributes a set-once property
    ## ideally, this is a set-once property because otherwise
    ## we'll run into consistency issues for edges that have customized
    ## attributes
    edgeSetAttributes(g1) <- myEdgeAttributes
    checkEquals(myEdgeAttributes, edgeSetAttributes(g1))
    ## So assigning into it again is an error
    ##myCheckException(edgeSetAttributes(g1) <- myEdgeAttributes)
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
    checkEquals(list(), edgeAttributes(g1, from="a", to="d")[[1]])

    ## Exception if node not found
    myCheckException(edgeAttributes(g1, from="a", to="nosuchnode"))
    myCheckException(edgeAttributes(g1, from="nosuchnode", to="a"))

    ## Exception if edge not found
    myCheckException(edgeAttributes(g1, from="a", to="b"))


    ## pickup default values
    myEdgeAttributes <- list(weight=1, color="blue")
    edgeSetAttributes(g1) <- myEdgeAttributes
    checkEquals(myEdgeAttributes, edgeAttributes(g1, from="a", to="d")[[1]])

    ## disallow assigning names not in edgeSetAttributes
    badEdgeAttributes <- list(weight=400, style="modern", type="fruit")
    myCheckException(edgeAttributes(g1, "a", "d") <- badEdgeAttributes)

    ## Customize existing edges
    edgeAttributes(g1, "a", "d") <- list(weight=800)
    checkEquals(800, edgeAttributes(g1, "a", "d")[[1]]$weight)
    checkEquals("blue", edgeAttributes(g1, "a", "d")[[1]]$color)
}


testEdgeAttributesVectorized <- function() {
    g1 <- simpleDirectedGraph()
    myEdgeAttributes <- list(weight=1, color="blue")
    edgeSetAttributes(g1) <- myEdgeAttributes

    eAttrs <- edgeAttributes(g1, from="a")
    checkEquals(TRUE, setequal(c("a|c", "a|d"), names(eAttrs)))

    ## test with to="missing"
    myCheckException(edgeAttributes(g1, from="c"))
    checkEquals("b|c", names(edgeAttributes(g1, from="b")))

    ## test with from="missing"
    myCheckException(edgeAttributes(g1, to="a"))
    checkEquals("d|b", names(edgeAttributes(g1, to="b")))
    expect <- paste(c("a", "b", "d"), "c", sep="|")
    checkEquals(expect, names(edgeAttributes(g1, to="c")))

    fr <- c("a", "a", "d", "b", "d")
    to <- c("d", "c", "b", "c", "c")
    eAttrs <- edgeAttributes(g1, from=fr, to=to)
    expectNames <- paste(fr, to, sep="|")
    checkEquals(expectNames, names(eAttrs))

    
}


testAddNodes <- function() {
    mat <- simpleInciMat()
    g1 <- new("graphIM", inciMat=mat)

    newNodes <- c("r", "s", "a", "b")
    checkException(addNodes(g1, newNodes))

    newNodes <- c("r", "s")
    expect <- c(nodes(g1), newNodes)
    g1 <- addNodes(g1, newNodes)
    checkEquals(expect, nodes(g1))
}


testAddEdge <- function() {
    ## I would like different order of the args in the generic, but not sure it is
    ## worth it to change... but would seem more consistent.
    mat <- simpleInciMat()
    g1 <- new("graphIM", inciMat=mat)
    g1 <- addNodes(g1, "e")
    checkEquals(FALSE, isAdjacent(g1, "b", "e"))
    g1 <- addEdge(graph=g1, from="b", to="e")
    checkEquals(TRUE, isAdjacent(g1, "b", "e"))
}


testClearNode <- function() {
    mat <- simpleInciMat()
    g1 <- new("graphIM", inciMat=mat)
    edgeSetAttr(g1, "weight") <- 1
    edgeAttributes(g1, "a", "c") <- list(weight=400)
    
    checkEquals(TRUE, isAdjacent(g1, "a", "c"))
    checkEquals(TRUE, isAdjacent(g1, "a", "d"))
    checkEquals(400, edgeAttributes(g1, "a", "c")[[1]]$weight)
    g1 <- clearNode("a", g1)
    checkEquals(FALSE, isAdjacent(g1, "a", "c"))
    checkEquals(FALSE, isAdjacent(g1, "a", "d"))
    checkException(edgeAttributes(g1, "a", "c"))
}


testRemoveNode <- function() {
    mat <- simpleInciMat()
    g1 <- new("graphIM", inciMat=mat)
    origNodes <- nodes(g1)

    checkEquals(TRUE, "b" %in% origNodes)
    g1 <- removeNode("b", g1)
    checkEquals(FALSE, "b" %in% nodes(g1))
}


testRemoveEdge <- function() {
    mat <- simpleInciMat()
    g1 <- new("graphIM", inciMat=mat)

    checkEquals(TRUE, isAdjacent(g1, "a", "c"))
    g1 <- removeEdge("a", "c", g1)
    checkEquals(FALSE, isAdjacent(g1, "a", "c"))
}


testGraphIMCloning <- function() {
    mat <- simpleInciMat()
    g1 <- new("graphIM", inciMat=mat)
    origNodes <- nodes(g1)

    g2 <- g1

    ## modify g1
    g1 <- addNodes(g1, "z")
    ## TODO: since initialize creates a default edgeSetAttributes list, this method is useless...
    ##edgeSetAttributes(g1) <- list(weight=2, color="green")
    edgeSetAttr(g1, "weight") <- 2
    edgeSetAttr(g1, "color") <- "green"
    ## g2 should not have changed
    checkEquals(list(), edgeSetAttributes(g2))
    checkEquals(origNodes, nodes(g2))
}

