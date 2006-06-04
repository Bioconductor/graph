simpleGraphNEL <- function() {
     V <- letters[1:4]
     edL <- vector("list", length=4)
     names(edL) <- V
     edL[["a"]] <- list(edges=c(3, 4), weights=c(.13, .14))
     edL[["b"]] <- list(edges=c(3, 4), weights=c(.23, .24))
     edL[["c"]] <- list(edges=c(1, 2, 4), weights=c(.13, .23, .34))
     edL[["d"]] <- list(edges=c(1, 2, 3), weights=c(.14, .24, .34))
     gR <- new("graphNEL", nodes=V, edgeL=edL)
     gR
 }


simpleDirectedGraphNEL <- function() {
    set.seed(123)
     V <- letters[1:4]
     edL <- vector("list", length=4)
     names(edL) <- V
     edL[["a"]] <- list(edges=c(3, 4), weights=c(.13, .14))
     edL[["b"]] <- list(edges=c(3), weights=.23)
     edL[["c"]] <- list(edges=numeric(0), weights=numeric(0))
     edL[["d"]] <- list(edges=c(2, 3), weights=c(.42, .43))
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


testCreateGraphNoEdges <- function() {
    g <- new("graphNEL", nodes=c("a", "b"))
    checkEquals(0, numEdges(g))
    checkEquals(2, numNodes(g))

    g <- new("graphNEL", nodes=c("a", "b"), edgeL=list())
    checkEquals(0, numEdges(g))
    checkEquals(2, numNodes(g))
    
    checkEquals(2, length(edges(g)))
    checkEquals(nodes(g), names(edges(g)))
    checkEquals(0, sum(sapply(edges(g), length)))
}


testConstructor <- function() {
    g <- simpleGraphNEL()
    g2 <- new("graphNEL", nodes=nodes(g), edgeL=edges(g))
    checkEquals(nodes(g), nodes(g2))
    checkEquals(edges(g), edges(g2))

    ## We also support the more complicated list structure for describing graph
    ## edges.
    g2 <- new("graphNEL", nodes=nodes(g), edgeL=g@edgeL)
    checkEquals(nodes(g), nodes(g2))
    checkEquals(edges(g), edges(g2))
}


testNullHandlingInEdgeL <- function() {
    g <- simpleDirectedGraphNEL()
    eL <- g@edgeL
    eL <- c(eL[c("a", "b", "c")], list(d=NULL))
    g2 <- new("graphNEL", nodes(g), eL, "directed")
    checkTrue(all(sapply(g2@edgeL, function(x) !is.null(x))))
}


testCaptureWeightsWithEdgeLUndirected <- function() {
    g <- simpleGraphNEL()
    expect <- as.list(c(.13, .14))
    names(expect) <- c("a|c", "a|d")
    checkEquals(expect, edgeData(g, from="a", attr="weight"))
}


testCaptureWeightsWithEdgeLDirected <- function() {
    g <- simpleDirectedGraphNEL()
    expect <- as.list(c(.13, .14))
    names(expect) <- c("a|c", "a|d")
    checkEquals(expect, edgeData(g, from="a", attr="weight"))
}


testAddNode <- function() {
    g1 <- simpleGraphNEL()
    newNodes <- c("r", "s", "a", "b")
    checkException(addNode(newNodes, g1), silent=TRUE)

    newNodes <- c("r", "s")
    expect <- c(nodes(g1), newNodes)
    g1 <- addNode(newNodes, g1)
    checkEquals(expect, nodes(g1))
}


testAddNodeWithEdges <- function() {
    g1 <- simpleGraphNEL()
    newNodes <- c("r", "s", "t")
    newEdges <- list(r=c("a", "s"), s="b", t=character(0))
    g2 <- addNode(newNodes, g1, newEdges)

    checkEquals(c(nodes(g1), newNodes), nodes(g2))
    expect <- list(r=c("a", "s"))
    checkEquals(expect, edges(g2)["r"])
    expectEdges <- edges(g1)
    expectEdges[["a"]] <- c(expectEdges[["a"]], "r")
    expectEdges[["b"]] <- c(expectEdges[["b"]], "s")
    expectEdges[["r"]] <- c("a", "s")
    expectEdges[["s"]] <- c("r", "b")
    expectEdges[["t"]] <- character(0)
    checkEquals(expectEdges, edges(g2))
}


testAddNodeWithEdgesAndWeights <- function() {
    g1 <- simpleGraphNEL()
    newNodes <- c("r", "s", "t")
    newEdges <- list(r=c(a=11, s=22), s=c(b=33), t=numeric(0))
    g2 <- addNode(newNodes, g1, newEdges)

    checkEquals(c(nodes(g1), newNodes), nodes(g2))
    expect <- list(r=c("a", "s"))
    checkEquals(expect, edges(g2)["r"])
    expectEdges <- edges(g1)
    expectEdges[["a"]] <- c(expectEdges[["a"]], "r")
    expectEdges[["b"]] <- c(expectEdges[["b"]], "s")
    expectEdges[["r"]] <- c("a", "s")
    expectEdges[["s"]] <- c("r", "b")
    expectEdges[["t"]] <- character(0)
    checkEquals(expectEdges, edges(g2))
}


testSubGraphNoEdges <- function() {
    g1 <- simpleGraphNEL()
    g1 <- removeEdge("a", c("c", "d"), g1)
    g2 <- subGraph("a", g1) ## g2 has no edges
    checkEquals(0, numEdges(g2))
    checkEquals(1, numNodes(g2))
}


testSubGraphNoEdgesDirected <- function() {
    g1 <- simpleDirectedGraphNEL()
    g1 <- removeEdge("a", c("c", "d"), g1)
    g2 <- subGraph("a", g1) ## g2 has no edges
    checkEquals(0, numEdges(g2))
    checkEquals(1, numNodes(g2))
 }


testRemoveEdge <- function() {
    g1 <- simpleDirectedGraphNEL()
    f <- c("a", "a")
    t <- c("c", "d")
    g2 <- removeEdge(from=f, to=t, g1)
    checkEquals(3, numEdges(g2))
    checkTrue(!length(edges(g2)[["a"]]))
}


test_eWV <- function() {
    V <- LETTERS[1:4]
    gR <- new("graphNEL", nodes=V)
    gX <- addEdge("A", "C", gR, 0.2) 

    ans <- eWV(gX, edgeMatrix(gX), useNNames = TRUE) 
    checkEquals(c("A--C"=0.2), ans)
}


testEdgeWeightsNoEdges <- function() {
    g <- new("graphNEL", nodes=letters[1:6])
    expect <- lapply(edges(g), as.numeric)
    checkEquals(expect, edgeWeights(g))
}


testRemoveNode1 <- function() {
    ## using the example from the removeNode help page
    V <- LETTERS[1:4]
    edL2 <- vector("list", length=4)
    names(edL2) <- V
    for(i in 1:4)
      edL2[[i]] <- list(edges=c(2,1,2,1)[i],
                        weights=sqrt(i))
    gR2 <- new("graphNEL", nodes=V, edgeL=edL2, edgemode="directed")

    gX <- removeNode("C", gR2)
    checkEquals(c("A", "B", "D"), nodes(gX))

    gY <- removeNode(c("A","D"), gX)
    checkEquals("B", nodes(gY))

    gZ <- removeNode(c("A","C","D"), gR2)
    checkEquals("B", nodes(gZ))

    ## XXX: using direct slot access to verify that edge attributes
    ##      have been completely removed.
    checkTrue(length(gZ@edgeData@data) == 0)
}


testRemoveNode2 <- function() {
    g <- simpleDirectedGraphNEL()
    nds <- nodes(g)
    for (n in nds) {
        g2 <- removeNode(n, g)
        checkEquals(nds[nds != n], nodes(g2))
    }
}
