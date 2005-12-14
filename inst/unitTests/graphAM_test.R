simpleAdjMat <- function() {
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
    new("graphAM", adjMat=mat, edgemode="directed")
}
    


testInvalidNonSquare <- function() {
    mat <- cbind(c(0, 0, 1), c(1, 1, 1))
    myCheckException(new("graphAM", adjMat=mat))
}


testInvalidNegativeValues <- function() {
    mat <- matrix(c(0, 1, -4, -1), ncol=2)
    myCheckException(new("graphAM", adjMat=mat))
}


testInvalidNonSymmetric <- function() {
    mat <- matrix(c(0, 1, 1,
                    0, 0, 1,
                    0, 0, 0), ncol=3, byrow=TRUE)
    colnames(mat) <- letters[1:3]
    myCheckException(new("graphAM", adjMat=mat))
    myCheckException(new("graphAM", adjMat=mat, edgemode="undirected"))
    g1 <- new("graphAM", adjMat=mat, edgemode="directed")
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
    g1 <- new("graphAM", adjMat=mat, edgemode="directed",
              values=list(weight=1))
    checkEquals(4, edgeData(g1, "d", "b", attr="weight")[[1]])
    checkEquals(3, edgeData(g1, "b", "c", attr="weight")[[1]])
    checkEquals(2, edgeData(g1, "a", "d", attr="weight")[[1]])
    checkEquals(1, edgeData(g1, "a", "c", attr="weight")[[1]])

    myCheckException(new("graphAM", adjMat=mat, edgemode="directed",
                         values=list(weight=1, not=2)))
    myCheckException(new("graphAM", adjMat=mat, edgemode="directed",
                         values=list("must", "name")))
    myCheckException(new("graphAM", adjMat=mat, edgemode="directed",
                         values="weight"))

    g1 <- new("graphAM", adjMat=mat, edgemode="directed",
              values=list(type=4))
    checkEquals(4, edgeData(g1, "d", "b", attr="type")[[1]])
    checkEquals(3, edgeData(g1, "b", "c", attr="type")[[1]])
    checkEquals(2, edgeData(g1, "a", "d", attr="type")[[1]])
    checkEquals(1, edgeData(g1, "a", "c", attr="type")[[1]])
}


testEdges <- function() {
    mat <- simpleAdjMat() 
    g1 <- new("graphAM", adjMat=mat)
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
    mat <- simpleAdjMat() 
    g1 <- new("graphAM", adjMat=mat)
    got <- edges(g1)
    expect <- list(a=c("c", "d"), d=c("a", "b", "c"))
    got <- edges(g1, c("a", "d"))
    checkEquals(expect, got)
}
    

testNodeNames <- function() {
    mat <- simpleAdjMat()
    g1 <- new("graphAM", adjMat=mat)
    got <- nodes(g1)
    expect <- letters[1:4]
    checkEquals(expect, got)
}


testNodeNamesReplace <- function() {
    mat <- simpleAdjMat()
    g1 <- new("graphAM", adjMat=mat)
    nodes(g1) <- LETTERS[1:4]
    expect <- LETTERS[1:4]
    checkEquals(expect, nodes(g1))
}


testNumNodes <- function() {
    mat <- simpleAdjMat() 
    g1 <- new("graphAM", adjMat=mat)
    checkEquals(nrow(mat), numNodes(g1))
}


testNumEdges <- function() {
    mat <- simpleAdjMat() 
    g1 <- new("graphAM", adjMat=mat)
    checkEquals(5, numEdges(g1))

    edgemode(g1) <- "directed"
    checkEquals(10, numEdges(g1))
}


testNumEdgesWithSelfLoop <- function() {
    mat <- matrix(c(1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0), ncol=4)
    g1 <- new("graphAM", adjMat=mat)
    checkEquals(4, numEdges(g1))
}

testIsAdjacent <- function() {
    mat <- simpleAdjMat()
    g1 <- new("graphAM", adjMat=mat)

    checkEquals(TRUE, isAdjacent(g1, "a", "c"))
    checkEquals(TRUE, isAdjacent(g1, "c", "a"))
    checkEquals(FALSE, isAdjacent(g1, "a", "b"))
    checkEquals(FALSE, isAdjacent(g1, "b", "a"))
    myCheckException(isAdjacent(g1, "z", "a"))
    myCheckException(isAdjacent(g1, "a", "z"))
}


testIsAdjacentVectorized <- function() {
    mat <- simpleAdjMat()
    g1 <- new("graphAM", adjMat=mat)

    fr <- c("a", "c", "a", "b")
    to <- c("c", "a", "b", "a")
    expect <- c(TRUE, TRUE, FALSE, FALSE)
    checkEquals(expect, isAdjacent(g1, fr, to))
    checkEquals(expect, isAdjacent(g1, to, fr))
}


## testSubgraph <- function() {
##     mat <- simpleAdjMat() 
##     g1 <- new("graphAM", adjMat=mat)
##     g2 <- subgraph(c("a", "b", "c"), ffff)

##                }
    
## testEdgeWeights <- function() {
##     ## default weight is 1
##     mat <- simpleAdjMat()
##     g1 <- new("graphAM", adjMat=mat)
##     expect <- list(a=c(c=1, d=1), b=c(c=1, d=1), c=c(a=1, b=1, d=1),
##                    d=c(a=1, b=1, c=1))
##     checkEquals(expect, edgeWeights(g1))
## }
    
    
testAddNode <- function() {
    mat <- simpleAdjMat()
    g1 <- new("graphAM", adjMat=mat)

    newNodes <- c("r", "s", "a", "b")
    checkException(addNode(newNodes, g1))

    newNodes <- c("r", "s")
    expect <- c(nodes(g1), newNodes)
    g1 <- addNode(newNodes, g1)
    checkEquals(expect, nodes(g1))
}


testAddEdge <- function() {
    ## I would like different order of the args in the generic, but not sure it is
    ## worth it to change... but would seem more consistent.
    mat <- simpleAdjMat()
    g1 <- new("graphAM", adjMat=mat)
    g1 <- addNode("e", g1)
    checkEquals(FALSE, isAdjacent(g1, "b", "e"))
    g1 <- addEdge(graph=g1, from="b", to="e")
    checkEquals(TRUE, isAdjacent(g1, "b", "e"))
}


testClearNode <- function() {
    mat <- simpleAdjMat()
    g1 <- new("graphAM", adjMat=mat)
    edgeDataDefaults(g1, attr="weight") <- 1
    edgeData(g1, "a", "c", attr="weight") <- 400
    
    checkEquals(TRUE, isAdjacent(g1, "a", "c"))
    checkEquals(TRUE, isAdjacent(g1, "a", "d"))
    checkEquals(400, edgeData(g1, "a", "c", attr="weight")[[1]])
    g1 <- clearNode("a", g1)
    checkEquals(FALSE, isAdjacent(g1, "a", "c"))
    checkEquals(FALSE, isAdjacent(g1, "a", "d"))
    checkException(edgeData(g1, "a", "c", attr="weight"))
}


testRemoveNode <- function() {
    mat <- simpleAdjMat()
    g1 <- new("graphAM", adjMat=mat)
    origNodes <- nodes(g1)

    checkEquals(TRUE, "b" %in% origNodes)
    g1 <- removeNode("b", g1)
    checkEquals(FALSE, "b" %in% nodes(g1))
}


testRemoveEdge <- function() {
    mat <- simpleAdjMat()
    g1 <- new("graphAM", adjMat=mat)

    checkEquals(TRUE, isAdjacent(g1, "a", "c"))
    g1 <- removeEdge("a", "c", g1)
    checkEquals(FALSE, isAdjacent(g1, "a", "c"))
}


testGraphIMCloning <- function() {
    mat <- simpleAdjMat()
    g1 <- new("graphAM", adjMat=mat)
    origNodes <- nodes(g1)

    g2 <- g1

    ## modify g1
    g1 <- addNode("NEW", g1)
    edgeDataDefaults(g1, "weight") <- 2
    edgeDataDefaults(g1, "color") <- "green"
    ## g2 should not have changed
    checkEquals(list(), edgeDataDefaults(g2))
    checkEquals(origNodes, nodes(g2))
}


testUndirectedAsGraphNEL <- function() {
    mat <- simpleAdjMat()
    g1 <- new("graphAM", adjMat=mat)
    gNel <- as(g1, "graphNEL")
    checkEquals(edges(g1), edges(gNel))
    checkEquals(nodes(g1), nodes(gNel))
    checkEquals(edgemode(g1), edgemode(gNel))
    checkEquals(edgeDataDefaults(g1), edgeDataDefaults(gNel))
    checkEquals(nodeDataDefaults(g1), nodeDataDefaults(gNel))
}


testDirectedAsGraphNEL <- function() {
    g1 <- simpleDirectedGraph()
    gNel <- as(g1, "graphNEL")
    checkEquals(edges(g1), edges(gNel))
    checkEquals(nodes(g1), nodes(gNel))
    checkEquals(edgemode(g1), edgemode(gNel))
    checkEquals(edgeDataDefaults(g1), edgeDataDefaults(gNel))
    checkEquals(nodeDataDefaults(g1), nodeDataDefaults(gNel))
}


testInEdges <- function() {
    g1 <- simpleDirectedGraph()
    expected <- list(a=character(0), b="d", c=c("a", "b", "d"), d="a")
    checkEquals(expected, inEdges(g1))
}
