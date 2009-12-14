set.seed(0x12a9b)

make_basic_MultiDiGraph <- function()
{
    ft1 <- data.frame(from=c("a", "a", "a", "b", "b"),
                      to=c("b", "c", "d", "a", "d"),
                      weight=c(1, 3.1, 5.4, 1, 2.2))

    ft2 <- data.frame(from=c("a", "a", "a", "x", "x", "c"),
                      to=c("b", "c", "x", "y", "c", "a"),
                      weight=c(3.4, 2.6, 1, 1, 1, 7.9))

    esets <- list(ft1, ft2)

    g <- MultiDiGraph(esets)
    list(esets=esets, g=g)
}

randMultiDiGraph <- function(numNodes, numEdges)
{
    ftlist <- lapply(numEdges, function(ne) {
        graph:::randFromTo(numNodes, ne)
    })
    nn <- ftlist[[1L]]$nodes
    edgeSets <- lapply(ftlist, function(x) x[["ft"]])
    MultiDiGraph(edgeSets, nodes = nn)
}

sort_esets <- function(esets)
{
    ## sorting is based on column major ordering
    lapply(esets, function(ft) {
        ft <- ft[order(ft$to, ft$from), ]
    })
}

make_unique_ft <- function(ftdata)
{
    ## ftdata is a list with components $nodes and $ft
    ## $ft is a data.frame with columns 'from', 'to', and 'weight'
    ft <- ftdata[["ft"]]
    el <- paste(ft[["from"]], ft[["to"]], sep = "\t")
    dups <- duplicated(el)
    ftdata[["ft"]] <- ft[!dups, ]
    ftdata
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

    esets <- sort_esets(esets)
    checkIdentical(list(esets[[1L]][, "weight"],
                        esets[[2L]][, "weight"]),
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
                      weight=c(1, 3.1, 5.4, 1, 2.2))

    ft2 <- data.frame(from=c("a", "a", "a", "x", "x", "c"),
                        to=c("b", "c", "x", "y", "c", "a"),
                      weight=c(3.4, 2.6, 1, 1, 1, 7.9))

    esets <- list(ft1, ft2)

    g <- MultiDiGraph(esets)

    ems <- edgeMatrices(g)
    checkEquals(2L, length(ems))
    checkEquals(c(2L, 5L), dim(ems[[1L]]))
    checkEquals(c(2L, 6L), dim(ems[[2L]]))
    checkTrue(typeof(ems[[1L]]) == "integer")
    checkTrue(typeof(ems[[2L]]) == "integer")

    nn <- nodes(g)
    esets <- sort_esets(esets)
    ft1 <- esets[[1L]]
    ft2 <- esets[[2L]]

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

test_extractGraph_large <- function()
{
    eCounts <- c(e1=5, e2=10, e3=25, e4=75)
    eNames <- names(eCounts)
    g <- randMultiDiGraph(10, eCounts)
    for (i in seq_len(length(eCounts))) {
        gnel <- extractGraph(g, i)
        checkEquals(eCounts[[i]], numEdges(gnel))
        checkEquals(nodes(g), nodes(gnel))
        ## also verify extraction by name
        checkEquals(eCounts[[i]], numEdges(extractGraph(g, eNames[i])))
    }
}

test_edgeIntersect <- function()
{
    ftdata1 <- graph:::randFromTo(10, 5)
    ftdata2 <- graph:::randFromTo(10, 50)
    ftdata3 <- graph:::randFromTo(10, 50)

    ftdata2$ft <- rbind(ftdata2$ft, ftdata1$ft)
    ftdata3$ft <- rbind(ftdata3$ft, ftdata1$ft)

    ftdata2 <- make_unique_ft(ftdata2)
    ftdata3 <- make_unique_ft(ftdata3)

    edgeSets <- lapply(list(ftdata1, ftdata2, ftdata3),
                       function(x) x[["ft"]])
    nn <- ftdata1[["nodes"]]
    g <- MultiDiGraph(edgeSets, nn)

    wSum <- function(...)
    {
        rowSums(do.call(cbind, list(...)))
    }
    g2 <- edgeIntersect(g, weightFun = wSum)
    checkEquals(1L, length(numEdges(g2)))
    checkEquals(5L, numEdges(g2)[[1L]])
    checkTrue(all(eweights(g2)[[1L]] == 3L))
    checkEquals(edgeMatrices(g)[[1L]], edgeMatrices(g2)[[1L]])
}

test_fromToMatrices <- function()
{
    basic <- make_basic_MultiDiGraph()
    got <- graph:::fromToMatrices(basic[["g"]])
    esets <- sort_esets(basic[["esets"]])
    checkEquals(c("from", "to", "weight"), names(got[[1L]]))
    checkEquals(c("from", "to", "weight"), names(got[[2L]]))
    for (i in 1:2) {
        for (cn in c("from", "to", "weight")) {
            checkEquals(esets[[i]][[cn]], got[[i]][[cn]])
        }
    }
}

## write tests for named edge sets

## intersection
## union

## refining the bit array stuff
