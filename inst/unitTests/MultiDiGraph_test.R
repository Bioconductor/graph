make_basic_MultiDiGraph <- function()
{
    ft1 <- data.frame(from=c("a", "a", "a", "b", "b"),
                      to=c("b", "c", "d", "a", "d"),
                      weights=c(1, 3.1, 5.4, 1, 2.2))

    ft2 <- data.frame(from=c("a", "a", "a", "x", "x", "c"),
                      to=c("b", "c", "x", "y", "c", "a"),
                      weights=c(3.4, 2.6, 1, 1, 1, 7.9))

    esets <- list(ft1, ft2)

    g <- MultiDiGraph(esets)
    list(esets=esets, g=g)
}

sort_esets <- function(esets)
{
    lapply(esets, function(ft) {
        ft <- ft[order(ft$from, ft$to), ]
    })
}

test_create_infer_nodes <- function()
{
    basic <- make_basic_MultiDiGraph()
    esets <- basic$esets
    g <- basic$g
    checkEquals(6L, numNodes(g))
    checkEquals(c(5L, 6L), numEdges(g))
}

## test constructor with nodes arg given (singleton case)

test_bad_nodes_in_create <- function()
{
    ft1 <- matrix(c("a", "b",
                    "a", as.character(NA),
                    "b", "d"),
                  byrow = TRUE,
                  ncol = 2)

    ft2 <- matrix(c("a", "b",
                    "a\n", "c",
                    "", "x",
                    "x", "y",
                    "x", "z|a",
                    "c", "a\t"),
                  byrow = TRUE,
                  ncol = 2)

    esets <- list(as.data.frame(ft1), as.data.frame(ft2))

    checkException(MultiDiGraph(esets))
}

test_edgeWeights_create <- function()
{
    basic <- make_basic_MultiDiGraph()
    esets <- basic$esets
    g <- basic$g

    checkEquals(6L, numNodes(g))
    checkEquals(c(5L, 6L), numEdges(g))
    checkIdentical(list(esets[[1L]][order(esets[[1L]][,1L],
                                          esets[[1L]][,2L]), "weights"],
                        esets[[2L]][order(esets[[2L]][,1L],
                                          esets[[2L]][,2L]), "weights"]),
                   eweights(g))
}

test_edgeWeights_edge_names <- function()
{
    basic <- make_basic_MultiDiGraph()
    esets <- basic$esets
    g <- basic$g

    wv <- eweights(g, names.sep = "\t")
    esets <- sort_esets(esets)

    want <- paste(esets[[1]]$from, esets[[1]]$to, sep = "\t")
    checkEquals(want, names(wv[[1]]))
}


test_edgeMatrices <- function()
{
    ft1 <- data.frame(from=c("a", "a", "a", "b", "b"),
                        to=c("b", "c", "d", "a", "d"),
                      weights=c(1, 3.1, 5.4, 1, 2.2))

    ft2 <- data.frame(from=c("a", "a", "a", "x", "x", "c"),
                        to=c("b", "c", "x", "y", "c", "a"),
                      weights=c(3.4, 2.6, 1, 1, 1, 7.9))

    esets <- list(ft1, ft2)

    g <- MultiDiGraph(esets)

    ems <- edgeMatrices(g)
    checkEquals(2L, length(ems))
    checkEquals(c(2L, 5L), dim(ems[[1L]]))
    checkEquals(c(2L, 6L), dim(ems[[2L]]))
    checkTrue(typeof(ems[[1L]]) == "integer")
    checkTrue(typeof(ems[[2L]]) == "integer")

    nn <- nodes(g)
    ft1 <- ft1[order(ft1$from, ft1$to), ]
    ft2 <- ft2[order(ft2$from, ft2$to), ]

    gotft1 <- data.frame(from = nn[ems[[1L]][1L, ]],
                         to = nn[ems[[1L]][2L, ]])
    checkEquals(ft1$from, gotft1$from)
    checkEquals(ft1$to, gotft1$to)

    gotft2 <- data.frame(from = nn[ems[[2L]][1L, ]],
                         to = nn[ems[[2L]][2L, ]])
    checkEquals(ft2$from, gotft2$from)
    checkEquals(ft2$to, gotft2$to)
}

test_extractGraph <- function()
{
    g <- make_basic_MultiDiGraph()$g
    g1 <- extractGraph(g, which = 1L)
    checkEquals("graphNEL", as.character(class(g1)))
    checkEquals(nodes(g), nodes(g1))
    checkEquals(5L, numEdges(g1))
}


## write tests for named edge sets

## intersection
## union

## refining the bit array stuff
