library("graph")

data(graphExamples)
data(apopGraph)

test_degree_undirected <- function() {
    g <- graphExamples[[1]]
    want <- as.integer(c(5, 5, 1, 5, 5, 5, 0, 6, 0, 0))
    names(want) <- nodes(g)
    checkEquals(want, degree(g))

    gam <- as(g, "graphAM")
    checkEquals(want, degree(g))
}

test_degree_directed <- function() {
    want_in <- c(TRF1=1L, "NF-kB"=4L, CASP2=2L, Daxx=0L)
    want_out <- c(TRF1=1L, "NF-kB"=1L, CASP2=0L, Daxx=1L)

    got <- degree(apopGraph, c("TRF1", "NF-kB", "CASP2", "Daxx"))
    checkEquals(want_in, got[["inDegree"]])
    checkEquals(want_out, got[["outDegree"]])
}

test_handshaking = function() {
    ge1 = graphExamples[[1]]
    checkEquals(sum(degree(ge1)), 2*ncol(edgeMatrix(ge1)))  # handshaking
}

test_degree_self = function() {
    ge1 = graphExamples[[1]] # 16 edges
    checkEquals(ncol(edgeMatrix(ge1)), 16)
    ge2 = addEdge("j", "j", ge1)
    checkEquals(ncol(edgeMatrix(ge2)), 17)
    checkEquals(sum(degree(ge2)), 34)
}
