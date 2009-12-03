## library("graph")
randFromTo <- function(numNodes, numEdges)
{
    nodeNames <- paste("n", seq_len(numNodes), sep="")
    maxEdges <- numNodes^2
    n1 <- sample(nodeNames, maxEdges, replace=TRUE)
    n2 <- sample(nodeNames, maxEdges, replace=TRUE)
    notSelfLoop <- n1 != n2
    n1 <- n1[notSelfLoop]
    n2 <- n2[notSelfLoop]
    idx <- seq_len(numEdges)
    list(nodes = nodeNames, from = n1[idx], to = n2[idx])
}

test_create_small <- function()
{
    f <- c("a", "a", "b", "c", "d")
    t <- c("b", "c", "c", "d", "a")
    g <- GraphAM2(f, t)
    gd <- GraphAM2(f, t, edgemode = "directed")

    checkEquals(4L, numNodes(g))
    checkEquals(4L, numNodes(gd))

    checkEquals(5L, numEdges(g))
    checkEquals(5L, numEdges(gd))
    ## TODO more tests
    edges(g)
    isAdjacent(g, c("a", "a", "b"), c("d", "c", "c"))
}

test_create_big <- function()
{
    r1 <- randFromTo(100, 100)
    r2 <- randFromTo(100, 100)
    g1 <- GraphAM2(r1$from, r1$to, r1$nodes, edgemode="directed")
    g2 <- GraphAM2(r2$from, r2$to, r1$nodes, edgemode="directed")
}
